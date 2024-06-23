-module(simpler_cowboy_rest_example_shared).

-export([service_available/2]).
-export([is_authorized/2]).

-elvis([{elvis_style, no_debug_call, disable}]).

service_available(Req, State) ->
    io:format("Service is available! (output from shared)~n"),
    {true, Req, State}.

is_authorized(Req, State) ->
    io:format("You're authorized! (output from shared)~n"),
    {true, Req, State}.
