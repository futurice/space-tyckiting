{application, space_application,
 [{description,  "Space tyckiting client"},
  {vsn,          "0.1"},
  {id,           "space"},
  {modules,      [space,
                  space_application,
                  simple_space_ai]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {space_application, []}},
  {env, [{current_ai,   simple_space_ai},
         {host_address, "ws://localhost:3000"},
         {team_name,    <<"my_tyckiting">>}]
  }]}.
