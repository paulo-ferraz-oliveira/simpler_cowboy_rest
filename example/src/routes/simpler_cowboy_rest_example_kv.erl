-module(simpler_cowboy_rest_example_kv).
-behaviour(simpler_cowboy_rest).

-export([resource_exists/2]).
-export([allow_missing_post/2]).

-export([put/2]).
-export([get/2]).
-export([post/2]).
-export([delete/2]).

resource_exists(Req, State) ->
    {[] =/= ets:lookup(simpler_cowboy_rest_example, k(Req)), Req, State}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

put(Req, State) ->
    {ets:insert_new(simpler_cowboy_rest_example, {k(Req), body(Req)}), Req, State}.

get(Req, State) ->
    {ets_lookup(k(Req)), Req, State}.

post(Req, State) ->
    {ets:insert(simpler_cowboy_rest_example, {k(Req), body(Req)}), Req, State}.

delete(Req, State) ->
    {ets:delete(simpler_cowboy_rest_example, k(Req)), Req, State}.

% internal

k(Req) ->
    cowboy_req:binding(k, Req).

body(Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    Body.

ets_lookup(K) ->
    [{_, V}] = ets:lookup(simpler_cowboy_rest_example, K),
    #{<<"result">> => V}.
