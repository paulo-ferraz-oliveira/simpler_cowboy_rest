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
            % , with_constraints => [] % optional
            % Other state options go here, too
        }},
        {"/kv[/[:k]]", #{
            methods => [<<"PUT">>, <<"GET">>, <<"POST">>, <<"DELETE">>],
            are_dispatched_to => simpler_cowboy_rest_example_kv,
            with_constraints => [{k, restrict_k}]
            % Other state options go here, too
        }}
    ],

    {ok, _} = simpler_cowboy_rest:start(clear, Routes, TransOpts, #{}),
    ok.

start_db() ->
    _ = ets:new(simpler_cowboy_rest_example, [named_table, public]),
    ok.
