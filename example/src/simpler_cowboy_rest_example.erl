-module(simpler_cowboy_rest_example).

-export([start/0]).

start() ->
    ok = start_db(),
    ok = start_cowboy().

%
% Private.

start_cowboy() ->
    TransOpts = [
        {port, 8080}
    ],

    Routes = [
        {"/health", #{
            methods => [<<"GET">>],
            are_dispatched_to => simpler_cowboy_rest_example_health
        }},
        {"/kv[/[:k]]", #{
            methods => [<<"PUT">>, <<"GET">>, <<"POST">>, <<"DELETE">>],
            are_dispatched_to => simpler_cowboy_rest_example_kv
        }}
    ],

    {ok, _} = simpler_cowboy_rest:start(Routes, TransOpts),
    ok.

start_db() ->
    _ = ets:new(simpler_cowboy_rest_example, [named_table, public]),
    ok.
