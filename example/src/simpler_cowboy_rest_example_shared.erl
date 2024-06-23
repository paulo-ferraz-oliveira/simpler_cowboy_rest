-module(simpler_cowboy_rest_example_shared).

-export([service_available/2]).
-export([is_authorized/2]).

service_available(Req, State) ->
    logger:info("Service is available! (output from shared)"),
    {true, Req, State}.

is_authorized(Req, State) ->
    logger:info("You're authorized! (output from shared)"),
    {true, Req, State}.
