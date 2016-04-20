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

handle_request({[{<<"type">>, <<"connected">>},
                 {<<"teamId">>, TeamId},
                 Config]}, _State) ->
    InitialConfig = parse_config(Config),
    io:format("Yay, connected to server! teamid ~p config ~p ~n",
              [TeamId, InitialConfig]),
    %% No need to reply:
    dont_reply;

handle_request({[{<<"type">>, <<"start">>},
                 {<<"you">>, Team},
                 Config,
                 {<<"otherTeams">>, Teams}]}, State) ->
    ParsedConfig = parse_config(Config),
    ParsedTeam   = parse_team(Team),
    ParsedTeams  = lists:map(fun parse_team/1, Teams),
    {ok, AI} = application:get_env(current_ai),
    apply(AI, give_moves, [0, ParsedConfig, ParsedTeam,
                           ParsedTeams, [], State]);

handle_request({[{<<"type">>, <<"end">>},
                 {<<"you">>, Team},
                 {<<"winnerTeamId">>, WinnerTeam}]}, _State) ->
    #team{team_id = TeamId } = parse_team(Team),
    case WinnerTeam == TeamId of
        true ->
            io:format("Congrats, you won!~n");
        false ->
            io:format("Oh noez, you lost!~n")
    end,
    dont_reply;

handle_request({[{<<"type">>, <<"events">>},
                 {<<"roundId">>, RoundId},
                 Config,
                 {<<"you">>, Team},
                 {<<"otherTeams">>, Teams},
                 {<<"events">>, Events}]}, State) ->
    ParsedConfig = parse_config(Config),
    ParsedTeam   = parse_team(Team),
    ParsedTeams  = lists:map(fun parse_team/1, Teams),
    ParsedEvents = lists:map(fun parse_event/1, Events),
    {ok, AI} = application:get_env(current_ai),
    apply(AI, give_moves, [RoundId, ParsedConfig, ParsedTeam,
                           ParsedTeams, ParsedEvents, State]);

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

parse_config({<<"config">>,
              {[{<<"bots">>, Bots},
                {<<"fieldRadius">>, FieldRadius},
                {<<"move">>, Move},
                {<<"startHp">>, StartHP},
                {<<"cannon">>, Cannon},
                {<<"radar">>, Radar},
                {<<"see">>, See},
                {<<"maxCount">>, MaxCount},
                {<<"asteroids">>, Asteroids},
                {<<"loopTime">>, LoopTime},
                {<<"noWait">>, NoWait}]}}) ->
    #config{bots = Bots,
            field_radius = FieldRadius,
            move = Move,
            start_hp = StartHP,
            cannon = Cannon,
            radar = Radar,
            see = See,
            max_count = MaxCount,
            asteroids = Asteroids,
            loop_time = LoopTime,
            no_wait = NoWait}.

parse_team({[{<<"name">>, Name},
             {<<"teamId">>, TeamId},
             {<<"bots">>, Bots}]}) ->
    #team{name = Name,
          team_id = TeamId,
          bots = lists:map(fun parse_bot/1, Bots)}.

parse_bot({[{<<"botId">>, Id},
            {<<"name">>, Name},
            {<<"teamId">>, TeamId},
            {<<"alive">>, Alive},
            {<<"pos">>, {[{<<"x">>, X}, {<<"y">>, Y}]}},
            {<<"hp">>, HP}]}) ->
    #bot{bot_id = Id,
         name = Name,
         team_id = TeamId,
         alive = Alive,
         pos = {X,Y},
         hp = HP};
%% Opponent bots can be given without pos and hp
parse_bot({[{<<"botId">>, Id},
            {<<"name">>, Name},
            {<<"teamId">>, TeamId},
            {<<"alive">>, Alive}]}) ->
    #bot{bot_id = Id,
         name = Name,
         team_id = TeamId,
         alive = Alive}.

parse_event({[{<<"event">>, <<"hit">>},
              {<<"source">>, Source},
              {<<"botId">>, BotId}]}) ->
    {hit, BotId, Source};

parse_event({[{<<"event">>, <<"die">>},
              {<<"botId">>, BotId}]}) ->
    {die, BotId};

parse_event({[{<<"event">>, <<"see">>},
              {<<"source">>, Source},
              {<<"botId">>, BotId},
              {<<"pos">>, {[{<<"x">>, X}, {<<"y">>, Y}]}}]}) ->
    {see, Source, BotId, {X, Y}};

parse_event({[{<<"event">>, <<"radarEcho">>},
              {<<"pos">>, {[{<<"x">>, X}, {<<"y">>, Y}]}}]}) ->
    {radar_echo, {X, Y}};

parse_event({[{<<"event">>, <<"detected">>},
              {<<"botId">>, BotId}]}) ->
    {detected, BotId};

parse_event({[{<<"event">>, <<"damaged">>},
              {<<"botId">>, BotId},
              {<<"damage">>, Damage}]}) ->
    {damaged, BotId, Damage};

parse_event({[{<<"event">>, <<"move">>},
              {<<"botId">>, BotId},
              {<<"pos">>, {[{<<"x">>, X}, {<<"y">>, Y}]}}]}) ->
    {move, BotId, {X, Y}};

parse_event({[{<<"event">>, <<"noaction">>},
              {<<"botId">>, BotId}]}) ->
    {noaction, BotId}.
