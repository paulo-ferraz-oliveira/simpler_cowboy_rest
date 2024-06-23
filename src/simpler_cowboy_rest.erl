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

% dev API

-export([start/2]).

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
    % Force modules to be loaded
    ?M:module_info(),
    case shared_impl() of
        undefined ->
            ok;
        SharedImpl ->
            SharedImpl:module_info()
    end,
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

% Exception to the rule: we want to rename this!
delete(default, Req, State) ->
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

% dev API

start(Routes0, TransOpts) ->
    Routes = expand_to_cowboy(Routes0),
    DispatchRules = cowboy_router:compile([{'_', Routes}]),
    ProtoOpts = #{
        env => #{
            dispatch => DispatchRules
        }
    },
    cowboy:start_clear(simpler_cowboy_rest, TransOpts, ProtoOpts).

% internal

dispatch({M, F, A}, Req, State) ->
    case erlang:function_exported(M, F, A) of
        true ->
            M:F(Req, State);
        false ->
            SharedImpl = application:get_env(simpler_cowboy_rest, shared_impl, undefined),
            dispatch_shared(SharedImpl, F, A, Req, State)
    end.

dispatch_shared(undefined = _SharedImpl, F, _A, Req, State) ->
    ?MODULE:F(default, Req, State);
dispatch_shared(SharedImpl, F, A, Req, State) ->
    case erlang:function_exported(SharedImpl, F, A) of
        true ->
            SharedImpl:F(Req, State);
        false ->
            ?MODULE:F(default, Req, State)
    end.

expand_to_cowboy(Routes) ->
    lists:foldl(
        fun({Path, Opts} = _Route, AccIn) ->
            AccIn ++ [{Path, simpler_cowboy_rest, Opts}]
        end,
        [],
        Routes
    ).

shared_impl() ->
    application:get_env(simpler_cowboy_rest, shared_impl, undefined).
