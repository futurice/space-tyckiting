-record(config, {bots         :: integer(),
                 field_radius :: integer(),
                 move         :: integer(),
                 start_hp     :: integer(),
                 cannon       :: integer(),
                 radar        :: integer(),
                 see          :: integer(),
                 max_count    :: integer(),
                 asteroids    :: integer(),
                 loop_time    :: integer(),
                 no_wait      :: boolean()}).

-record(bot, {bot_id  :: integer(),
              name    :: string(),
              team_id :: integer(),
              hp      :: integer(),
              alive   :: boolean(),
              pos     :: {integer(), integer()}}).

-record(team, {name    :: string(),
               team_id :: integer(),
               bots    :: [#bot{}]}).
