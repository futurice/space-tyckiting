# Space tyckiting erlang client

##Download dependencies and build
Download [Erlang/OTP] (https://www.erlang-solutions.com/resources/download.html)
, version R16B03 or above. Download [rebar] (https://github.com/rebar/rebar).
This project is not working with rebar2 or rebar3 right now, since the JSON
encoding/decoding library `jiffy` seems not to compile C sources with those.
When dependencies are installed:
####make build

##Run application
Have server up and running (maybe at ws://localhost:3000, configurable
in ebin/space_application.app).
####make run

##Your AI code
The specific AI code that controls the space ships is located in
an `AI module`. An example of such an AI module
is given in `src/simple_space_ai.erl`. It has a function named
`give_moves/5` that receives all known information about the
current game state, and is expected to return actions for each
bot of your team.

##Configurable AI module
To enable quickly switching between AIs, there is an env variable
in ebin/space_application.app named `current_ai` that refers to
an erlang module. Change this env variable to another AI module
to "route" calls there.

##Erlang learning resources
* [Learn you some Erlang] (http://learnyousomeerlang.com)
* http://www.erlang.org/course
* https://pragprog.com/book/jaerlang2/programming-erlang
