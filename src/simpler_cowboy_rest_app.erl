-module(simpler_cowboy_rest_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, StartArgs) ->
    {ok, _} = start_cowboy(_TransOpts = StartArgs),
    ok = start_db(),
    {ok, self()}.

stop(_State) ->
    ok.

start_cowboy(TransOpts) ->
    Routes = [
        {"/health", simpler_cowboy_rest, #{
            methods => [<<"GET">>],
            are_dispatched_to => simpler_cowboy_rest_health
        }},
        {"/kv[/[:k]]", simpler_cowboy_rest, #{
            methods => [<<"PUT">>, <<"GET">>, <<"POST">>, <<"DELETE">>],
            are_dispatched_to => simpler_cowboy_rest_kv
        }}
    ],
    DispatchRules = cowboy_router:compile([{'_', Routes}]),
    ProtoOpts = #{
        env => #{
            dispatch => DispatchRules
        }
    },
    cowboy:start_clear(simpler_cowboy_rest, TransOpts, ProtoOpts).

start_db() ->
    _ = ets:new(simpler_cowboy_rest, [named_table, public]),
    ok.
