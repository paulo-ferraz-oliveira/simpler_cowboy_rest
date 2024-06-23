-module(simpler_cowboy_rest_example_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    ok = simpler_cowboy_rest_example:start(),
    {ok, self()}.

stop(_State) ->
    ok.
