-module(space).

-behaviour(websocket_client_handler).

%% Parsing implemented according to:
%% https://github.com/futurice/space-tyckiting/blob/master/server/DETAILS.md

-export([start_link/0,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,
         mk_action/3,
         mk_actions/2
        ]).

-include("include/space.hrl").

start_link() ->
    {ok, HostAddress} = application:get_env(host_address),
    websocket_client:start_link(HostAddress, ?MODULE, []).

init([], _ConnState) ->
    random:seed(),
    {ok, TeamName} = application:get_env(team_name),
    JoinReq = jiffy:encode({[{type, join}, {teamName, TeamName}]}),
    io:format("Lets join: ~p~n", [JoinReq]),
    websocket_client:cast(self(), {text, JoinReq}),
    {ok, state_uninitialized}.

websocket_handle({text, Msg}, _ConnState, State0) ->
    JsonMsg  = jiffy:decode(Msg),
    case handle_request(JsonMsg, State0) of
        dont_reply ->
            {ok, State0};
        {Response, State1} ->
            EncodedResponse = jiffy:encode(Response),
            {reply, {text, EncodedResponse}, State1}
    end.

websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang msg received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p with reason ~p ~n",
              [State, Reason]),
    {close, <<>>, bye}.

handle_request({L}, State) ->
    case v(<<"type">>, L) of
        <<"connected">> ->
            TeamId = v(<<"teamId">>, L),
            Config = v(<<"config">>, L),
            InitialConfig = parse_config(Config),
            io:format("Yay, connected to server! teamid ~p config ~p ~n",
                      [TeamId, InitialConfig]),
            dont_reply;
        <<"start">> ->
            Team         = v(<<"you">>, L),
            Teams        = v(<<"otherTeams">>, L),
            Config       = v(<<"config">>, L),
            ParsedConfig = parse_config(Config),
            ParsedTeam   = parse_team(Team),
            ParsedTeams  = lists:map(fun parse_team/1, Teams),
            {ok, AI}     = application:get_env(current_ai),
            apply(AI, give_moves, [0, ParsedConfig, ParsedTeam,
                                   ParsedTeams, [], State]);
        <<"end">> ->
            Team       = v(<<"you">>, L),
            WinnerTeam = v(<<"winnerTeamId">>, L),
            #team{team_id = TeamId} = parse_team(Team),
            case WinnerTeam == TeamId of
                true ->
                    io:format("Congrats, you won!~n");
                false ->
                    io:format("Oh noez, you lost!~n")
            end,
            dont_reply;
        <<"events">> ->
            RoundId      = v(<<"roundId">>, L),
            Team         = v(<<"you">>, L),
            Teams        = v(<<"otherTeams">>, L),
            Events       = v(<<"events">>, L),
            Config       = v(<<"config">>, L),
            ParsedConfig = parse_config(Config),
            ParsedTeam   = parse_team(Team),
            ParsedTeams  = lists:map(fun parse_team/1, Teams),
            ParsedEvents = lists:map(fun parse_event/1, Events),
            {ok, AI} = application:get_env(current_ai),
            apply(AI, give_moves, [RoundId, ParsedConfig, ParsedTeam,
                                   ParsedTeams, ParsedEvents, State]);
        _ ->
            io:format("Unsupported request from server: ~p~n", [{L}]),
            dont_reply
    end;

handle_request(X, _State) ->
    io:format("Unsupported request from server: ~p~n", [X]),
    dont_reply.

mk_action(Type, BotId, {X, Y}) ->
    {[{type, Type},
      {botId, BotId},
      {pos, {[{x, X}, {y, Y}]}}]}.

mk_actions(RoundId, BotActions) ->
    {[{type, actions},
      {roundId, RoundId},
      {actions, BotActions}
     ]}.

parse_config({L}) ->
    #config{bots         = v(<<"bots">>, L),
            field_radius = v(<<"fieldRadius">>, L),
            move         = v(<<"move">>, L),
            start_hp     = v(<<"startHp">>, L),
            cannon       = v(<<"cannon">>, L),
            radar        = v(<<"radar">>, L),
            see          = v(<<"see">>, L),
            max_count    = v(<<"maxCount">>, L),
            asteroids    = v(<<"asteroids">>, L),
            loop_time    = v(<<"loopTime">>, L),
            no_wait      = v(<<"noWait">>, L)}.

parse_team({L}) ->
    Bots = v(<<"bots">>, L),
    #team{name    = v(<<"name">>, L),
          team_id = v(<<"teamId">>, L),
          bots    = lists:map(fun parse_bot/1, Bots)}.

parse_bot({L}) ->
    case v(<<"pos">>, L) of
        %% Opponent bots can be given without pos and hp
        false ->
            #bot{bot_id  = v(<<"botId">>, L),
                 name    = v(<<"name">>, L),
                 team_id = v(<<"teamId">>, L),
                 alive   = v(<<"alive">>, L)};
        {PosL} ->
            X = v(<<"x">>, PosL),
            Y = v(<<"y">>, PosL),
            #bot{bot_id  = v(<<"botId">>, L),
                 name    = v(<<"name">>, L),
                 team_id = v(<<"teamId">>, L),
                 alive   = v(<<"alive">>, L),
                 pos     = {X,Y},
                 hp      = v(<<"hp">>, L)}
    end.

parse_event({L}) ->
    parse_event(v(<<"event">>, L), L).

parse_event(<<"hit">>, L) ->
    {hit, v(<<"botId">>, L), v(<<"source">>, L)};

parse_event(<<"die">>, L) ->
    {die, v(<<"botId">>, L)};

parse_event(<<"see">>, L) ->
    {Pos} = v(<<"pos">>, L),
    {see, v(<<"source">>, L), v(<<"botId">>, L),
     {v(<<"x">>, Pos), v(<<"y">>, Pos)}
    };

parse_event(<<"radarEcho">>, L) ->
    {Pos} = v(<<"pos">>, L),
    {radar_echo, {v(<<"x">>, Pos), v(<<"y">>, Pos)}};

parse_event(<<"seeAsteroid">>, L) ->
    {Pos} = v(<<"pos">>, L),
    {see_asteroid, {v(<<"x">>, Pos), v(<<"y">>, Pos)}};

parse_event(<<"detected">>, L) ->
    {detected, v(<<"botId">>, L)};

parse_event(<<"damaged">>, L) ->
    {damaged, v(<<"botId">>, L), v(<<"damage">>, L)};

parse_event(<<"move">>, L) ->
    {Pos} = v(<<"pos">>, L),
    {move, v(<<"botId">>, L), {v(<<"x">>, Pos), v(<<"y">>, Pos)}};

parse_event(<<"noaction">>, L) ->
    {noaction, v(<<"botId">>, L)}.

v(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false    -> false;
        {_, Val} -> Val
    end.
