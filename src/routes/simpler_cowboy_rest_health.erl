-module(simpler_cowboy_rest_health).
-behaviour(simpler_cowboy_rest).

-export([get/2]).

get(Req, State) ->
    {"I'm Ok, thanks", Req, State}.
