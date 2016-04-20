get-deps:
	mkdir -p deps
	rebar get-deps

build: get-deps
	rebar compile

run:
	erl -pa ebin/ -pa deps/*/ebin -eval "application:start(space_application)"

clean:
	rebar clean
