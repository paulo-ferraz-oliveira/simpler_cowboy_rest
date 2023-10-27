# `simpler_cowboy_rest`

A simpler (and opinionated) `cowboy_rest` behaviour.

Find below the Gist of it, and explore the project to see how it's used.

## The behaviour

### Context

[`cowboy_rest`](https://github.com/ninenines/cowboy/blob/master/src/cowboy_rest.erl),
a behaviour presented by [`cowboy`](https://github.com/ninenines/cowboy), allows you
to define handlers for HTTP routes and verbs.

### Motivation

Since it's possible many of your handlers will implement similar behaviour-implementing
callbacks, I present a generic way to have a "higher-level" behaviour without
the need declaring generic calls that all your callbacks have to call, or using
solutions like [inaka/mixer](https://github.com/inaka/mixer), which are based
on parse transforms.

I also sprinkle some opinions, like having verbs visible in the route metadata,
considering `application/json` the default data exchange "protocol", and naming
verbs as methods, like `put`, and `delete` (I even "get rid" of `delete_resource/2`).

### Details/opinions/caveats

"`% per route extensions`", below, assumes each of your handlers handles unique
verbs (e.g. no two GET for similar routes implemented in the same module).

Functions exported with `/3` (first argument being `default`) is where you'll define/maintain
generic callback definitions. To add a generic callback, expose a `/2` -equivalent.

Because we're using `erlang:function_exported` to check if a function is exported,
not calling the function directly we use `:module_info` to force code load,
otherwise the first call to each route would "fail".

### How to

Declare your dispatch handlers as:

```erlang
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
```

(check [`simpler_cowboy_rest_app.erl`](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/master/src/simpler_cowboy_rest_app.erl) for more)

Declare your new generic behavior as:

```erlang
-module(simpler_cowboy_rest).
-behaviour(cowboy_rest).

% cowboy_rest

-export([init/2]).
-export([service_available/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2]).

% default fallbacks

-export([init/3]).
-export([service_available/3]).
-export([allowed_methods/3]).
-export([is_authorized/3]).
-export([forbidden/3]).
-export([content_types_provided/3]).
-export([resource_exists/3]).
-export([allow_missing_post/3]).
-export([content_types_accepted/3]).
-export([delete/3]).

% per route extensions

-export([put/2]).
-export([get/2]).
-export([post/2]).
-export([delete/2]).

% spec'ed as per `cowboy_rest`'s current definition (2.10.0)

-callback init(Req, State) -> {Result, Req, State} when
    Result :: cowboy_rest.

-callback service_available(Req, State) -> {Result, Req, State} when
    Result :: boolean() | stop.

-callback allowed_methods(Req, State) -> {Result, Req, State} when
    Result :: [binary()] | stop.

-callback is_authorized(Req, State) -> {Result, Req, State} when
    Result :: true | {false, AuthHeader :: iodata()} | stop.

-callback forbidden(Req, State) -> {Result, Req, State} when
    Result :: boolean() | stop.

-callback content_types_provided(Req, State) -> {Result, Req, State} when
    Result :: [{binary() | ParsedMime, ProvideCallback :: atom()}] | stop,
    ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
    Params :: [{Key :: binary(), Value :: binary()}].

-callback resource_exists(Req, State) -> {Result, Req, State} when
    Result :: boolean() | stop.

-callback allow_missing_post(Req, State) -> {Result, Req, State} when
    Result :: boolean() | stop.

-callback content_types_accepted(Req, State) -> {Result, Req, State} when
    Result :: [{binary() | ParsedMime, AcceptCallback :: atom()}] | stop,
    ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
    Params :: [{Key :: binary(), Value :: binary()}].

-callback delete_resource(Req, State) -> {Result, Req, State} when
    Result :: boolean() | stop.

-callback put(Req, State) -> {Result, Req, State} when
    Result :: true | {true, URI :: iodata()} | false | stop.

-callback get(Req, State) -> {Result, Req, State} when
    Result :: cowboy_req:resp_body() | stop.

-callback post(Req, State) -> {Result, Req, State} when
    Result :: true | {true, URI :: iodata()} | false | stop.

-callback delete(Req, State) -> {Result, Req, State} when
    Result :: boolean() | stop.

-optional_callbacks([init/2]).
-optional_callbacks([service_available/2]).
-optional_callbacks([allowed_methods/2]).
-optional_callbacks([is_authorized/2]).
-optional_callbacks([forbidden/2]).
-optional_callbacks([content_types_provided/2]).
-optional_callbacks([resource_exists/2]).
-optional_callbacks([allow_missing_post/2]).
-optional_callbacks([content_types_accepted/2]).
-optional_callbacks([delete_resource/2]).

-optional_callbacks([put/2]).
-optional_callbacks([get/2]).
-optional_callbacks([post/2]).
-optional_callbacks([delete/2]).

-define(M, (maps:get(are_dispatched_to, State))).
-define(F, ?FUNCTION_NAME).
-define(A, ?FUNCTION_ARITY).
-define(MFA, {?M, ?F, ?A}).

% cowboy_rest (and default fallbacks)

init(default, Req, State) ->
    ?M:module_info(), % Force module to be loaded
    {cowboy_rest, Req, State}.

init(Req, State) ->
    dispatch(?MFA, Req, State).

service_available(default, Req, State) ->
    {true, Req, State}.

service_available(Req, State) ->
    dispatch(?MFA, Req, State).

allowed_methods(default, Req, #{methods := Methods} = State) ->
    {Methods, Req, State}.

allowed_methods(Req, State) ->
    dispatch(?MFA, Req, State).

is_authorized(default, Req, State) ->
    {true, Req, State}.

is_authorized(Req, State) ->
    dispatch(?MFA, Req, State).

forbidden(default, Req, State) ->
    {false, Req, State}.

forbidden(Req, State) ->
    dispatch(?MFA, Req, State).

content_types_provided(default, Req0, State0) ->
    % It's kinda odd that cowboy:
    % - calls this function for e.g. a POST request
    % - while at the same time specifying it with a ProvideCallback
    {Methods, Req, State} = allowed_methods(Req0, State0),
    case lists:member(cowboy_req:method(Req), Methods) of
        true ->
            % If defined, we're assuming JSON is the common interface "protocol"
            {[{{<<"application">>, <<"json">>, '*'}, get}], Req, State};
        false ->
            {[{{<<"text">>, <<"html">>, '*'}, to_html}], Req, State}
    end.

content_types_provided(Req, State) ->
    dispatch(?MFA, Req, State).

resource_exists(default, Req, State) ->
    {true, Req, State}.

resource_exists(Req, State) ->
    dispatch(?MFA, Req, State).

allow_missing_post(default, Req, State) ->
    {true, Req, State}.

allow_missing_post(Req, State) ->
    dispatch(?MFA, Req, State).

content_types_accepted(default, Req, State) ->
    Method =
        case cowboy_req:method(Req) of
            <<"POST">> -> post;
            <<"PUT">> -> put
        end,
    % If defined, we're assuming JSON is the common interface "protocol"
    {[{{<<"application">>, <<"json">>, '*'}, Method}], Req, State}.

content_types_accepted(Req, State) ->
    dispatch(?MFA, Req, State).

delete(default, Req, State) -> % Exception to the rule: we want to rename this!
    {false, Req, State}.

delete_resource(Req, State) ->
    dispatch({?M, delete, ?A}, Req, State).

% per route extensions

post(Req, State) ->
    ?M:?F(Req, State).

get(Req, State) ->
    ?M:?F(Req, State).

put(Req, State) ->
    ?M:?F(Req, State).

delete(Req, State) ->
    ?M:?F(Req, State).

% internal

dispatch({M, F, A}, Req, State) ->
    case erlang:function_exported(M, F, A) of
        true ->
            M:F(Req, State);
        false ->
            ?MODULE:F(default, Req, State)
    end.
```

(check [simpler_cowboy_rest.erl](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/master/src/simpler_cowboy_rest.erl))

## The project

### License

License information can be found inside [LICENSE](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/main/LICENSE).

### Running tests

Start the application in a `rebar3`/Erlang shell, with `rebar3 shell`.

Run tests with `shelltest example.test` inside folder `test`. You'll need to have
[shelltestrunner](https://github.com/simonmichael/shelltestrunner) installed.

Example output:

```console
➜  tests git:(main) shelltest example.test
:example.test:1: [OK]
:example.test:2: [OK]
:example.test:3: [OK]
:example.test:4: [OK]
:example.test:5: [OK]
:example.test:6: [OK]
:example.test:7: [OK]
:example.test:8: [OK]
:example.test:9: [OK]

         Test Cases  Total
 Passed  9           9
 Failed  0           0
 Total   9           9
```
