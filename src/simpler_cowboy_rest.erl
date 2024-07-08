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
-ignore_xref([init/3]).
-export([service_available/3]).
-ignore_xref([service_available/3]).
-export([allowed_methods/3]).
-ignore_xref([allowed_methods/3]).
-export([is_authorized/3]).
-ignore_xref([is_authorized/3]).
-export([forbidden/3]).
-ignore_xref([forbidden/3]).
-export([content_types_provided/3]).
-ignore_xref([content_types_provided/3]).
-export([resource_exists/3]).
-ignore_xref([resource_exists/3]).
-export([allow_missing_post/3]).
-ignore_xref([allow_missing_post/3]).
-export([content_types_accepted/3]).
-ignore_xref([content_types_accepted/3]).
-export([delete/3]).
-ignore_xref([delete/3]).

% per route extensions

-export([put/2]).
-ignore_xref([put/2]).
-export([get/2]).
-ignore_xref([get/2]).
-export([post/2]).
-ignore_xref([post/2]).
-export([delete/2]).
-ignore_xref([delete/2]).

% dev API

-export([start/4]).
-ignore_xref([start/4]).
-export([stop/0]).
-ignore_xref([stop/0]).

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
    Result :: json:encode_value() | stop.

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
    {Result0, Req, State} = ?M:?F(Req, State),
    Result = iolist_to_binary(json:encode(Result0)),
    {Result, Req, State}.

put(Req, State) ->
    ?M:?F(Req, State).

delete(Req, State) ->
    ?M:?F(Req, State).

% dev API

start(ClearOrTls, Routes0, TransOpts, ProtoOpts0) ->
    Routes = expand_to_cowboy(Routes0),
    DispatchRules = cowboy_router:compile([{'_', Routes}]),
    Env = maps:get(env, ProtoOpts0, #{}),
    ProtoOpts = maps:merge(ProtoOpts0, #{
        env => maps:merge(Env, #{
            dispatch => DispatchRules
        })
    }),
    cowboy_start(ClearOrTls, TransOpts, ProtoOpts).

stop() ->
    cowboy:stop_listener(simpler_cowboy_rest).

cowboy_start(clear = _ClearOrTls, TransOpts, ProtoOpts) ->
    cowboy:start_clear(simpler_cowboy_rest, TransOpts, ProtoOpts);
cowboy_start(tls = _ClearOrTls, TransOpts, ProtoOpts) ->
    cowboy:start_tls(simpler_cowboy_rest, TransOpts, ProtoOpts).

% internal

dispatch({M, F, A}, Req, State) ->
    case erlang:function_exported(M, F, A) of
        true ->
            M:F(Req, State);
        false ->
            dispatch_shared(shared_impl(), F, A, Req, State)
    end.

dispatch_shared(undefined = _SharedImpl, F, A, Req, State) ->
    logger_set_process_metadata(?MODULE, F, A),
    ?MODULE:F(default, Req, State);
dispatch_shared(SharedImpl, F, A, Req, State) ->
    case erlang:function_exported(SharedImpl, F, A) of
        true ->
            logger_set_process_metadata(SharedImpl, F, A),
            SharedImpl:F(Req, State);
        false ->
            logger_set_process_metadata(?MODULE, F, A),
            ?MODULE:F(default, Req, State)
    end.

expand_to_cowboy(Routes) ->
    lists:foldl(
        fun({Path, Opts0} = _Route, AccIn) ->
            BaseConstraints = maps:get(with_constraints, Opts0, []),
            Mod = maps:get(are_dispatched_to, Opts0),
            Constraints = massage_constraints([], BaseConstraints, Mod),
            Opts = maps:remove(with_constraints, Opts0),
            AccIn ++ [{Path, Constraints, simpler_cowboy_rest, Opts}]
        end,
        [],
        Routes
    ).

massage_constraints(Acc, [], _Mod) ->
    Acc;
massage_constraints(Acc, [{K, Constraints} | Rest], Mod) ->
    CowboyConstraints = to_cowboy_constraints(Constraints, Mod),
    massage_constraints(Acc ++ [{K, CowboyConstraints}], Rest, Mod).

to_cowboy_constraints(Constraints, Mod) when is_atom(Constraints) ->
    to_cowboy_constraints([Constraints], Mod);
to_cowboy_constraints(Constraints, Mod) ->
    lists:foldl(
        fun(Constraint, Acc) ->
            Acc ++ [massaged_constraint(Constraint, Mod)]
        end,
        [],
        Constraints
    ).

massaged_constraint(Constraint, Mod) when is_atom(Constraint) ->
    fun Mod:Constraint/2;
massaged_constraint(Constraint, _Mod) ->
    Constraint.

logger_set_process_metadata(M, F, A) ->
    logger:set_process_metadata(#{simpler_cowboy_rest_dispatched_to => {M, F, A}}).

shared_impl() ->
    application:get_env(simpler_cowboy_rest, shared_impl, undefined).
