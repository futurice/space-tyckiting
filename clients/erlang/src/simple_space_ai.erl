-module(simple_space_ai).

-export([give_moves/6]).

-include("include/space.hrl").

%% To be changed by AI implementer:
-record(state, {some_data :: string()}).

give_moves(RoundId, Config, Team, OtherTeams, Events, state_uninitialized) ->
    State = #state{some_data = "Initial state!"},
    give_moves(RoundId, Config, Team, OtherTeams, Events, State);

give_moves(RoundId,
           #config{bots         = _ConfigBots,
                   field_radius = _ConfigFieldRadius,
                   move         = _ConfigMove,
                   start_hp     = _ConfigStartHp,
                   cannon       = _ConfigCannon,
                   radar        = _ConfigRadar,
                   see          = _ConfigSee,
                   max_count    = _ConfigMaxCound,
                   asteroids    = _ConfigAsteroids,
                   loop_time    = _ConfigLoopTime,
                   no_wait      = _ConfigNoWait} = _Config,
           #team{name    = _MyTeamName,
                 team_id = _MyTeamId,
                 bots    = MyBots} = _MyTeam,
           _OtherTeams,
           Events,
           #state{some_data = SomeData} = State) ->

    MyBotIds = [MB#bot.bot_id || MB <- MyBots],

    %% Current AI state to keep information
    %% not given by the server (my own memory)
    io:format("Current state: ~p~n", [SomeData]),

    %% Some modifications to the local state, information
    %% that is not provided by the server.
    NewState = State#state{some_data = "Our grand plan is to win!"},

    %% All hostile bots seen:
    RadarSeen    = [Pos || {radar_echo, Pos} <- Events],
    AdjacentSeen = [Pos || {see, _Source, BotId, Pos} <- Events,
                           not lists:member(BotId, MyBotIds)],
    Seen = RadarSeen ++ AdjacentSeen,

    %% Other supported events:
    %% Hits    = [{radar_echo, BotId, Source} ||
    %%               {radar_echo, BotId, Source} <- Events],
    %% Dies    = [{die, BotId} || {die, BotId} <- Events],
    %% Detects = [{detected, BotId} || {detected, BotId} <- Events],
    %% Damages = [{damaged, BotId, Damage}
    %%               || {damaged, BotId, Damage} <- Events],
    %% Moves   = [{move, BotId, {X, Y}} || {move, BotId, {X, Y}} <- Events],
    %% NoActs  = [{noaction, BotId} || {noaction, BotId} <- Events],

    %% TODO: Implement your logic and return, right now,
    %%       all bots are scanning and cannoning
    MyCleverAI = fun(#bot{bot_id  = BotId,
                          name    = _MyBotName,
                          team_id = _MyBotTeaMId,
                          hp      = _MyBotHP,
                          alive   = _IsMyBotAlive,
                          pos     = {MyBotX, MyBotY}}) ->
                     case Seen of
                         [] ->
                             %% When no one is seen, either radar or move
                             case random:uniform(2) of
                                 1 ->
                                     RX = -11 + random:uniform(25),
                                     RY = -11 + random:uniform(25),
                                     space:mk_action(<<"radar">>,
                                                     BotId,
                                                     {RX, RY});
                                 2 ->
                                     BotDeltaX = random:uniform(3) - 2,
                                     BotDeltaY = random:uniform(3) - 2,
                                     space:mk_action(<<"move">>,
                                                     BotId,
                                                     {MyBotX + BotDeltaX,
                                                      MyBotY + BotDeltaY})
                             end;
                         [{AttackX, AttackY}|_] ->
                             space:mk_action(<<"cannon">>,
                                             BotId,
                                             {AttackX, AttackY})
                     end
                 end,
    BotActions = lists:map(MyCleverAI, MyBots),
    {space:mk_actions(RoundId, BotActions), NewState}.
