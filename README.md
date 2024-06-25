# `simpler_cowboy_rest` [![CI][ci-img]][ci]

[ci]: https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/actions
[ci-img]: https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/actions/workflows/ci.yml/badge.svg

A simpler (and opinionated) `cowboy_rest` application.

## The behaviour

### Context

[`cowboy_rest`](https://github.com/ninenines/cowboy/blob/master/src/cowboy_rest.erl),
a behaviour presented by [`cowboy`](https://github.com/ninenines/cowboy), allows you
to define handlers for HTTP routes and verbs.

### Motivation

Since it's possible many of your handlers will implement similar behaviour-implementing
callbacks, this application implements a generic way to have a "higher-level" behaviour without
the need for declaring generic calls that all your callbacks have to call, or using
solutions like [inaka/mixer](https://github.com/inaka/mixer), which are based
on parse transforms. This is available via configuration key (module) `shared_impl`,
where you'll implement a `simpler_cowboy_rest` behaviour to deal with shared implementation details.
(the [example](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/tree/main/example)
makes simple use of this concept).

It also sprinkles some opinions, like having verbs visible in the route metadata,
considering `application/json` the default data exchange "protocol", and naming
verbs as methods, like `put`, and `delete` (it even "gets rid" of `delete_resource/2`).

Mind you that because of the JSON "constraint", we expect not stuff like
`cowboy_req:resp_body()` but a `json:encode_value()` since we're going to encode inside the
application.

### Details/opinions/caveats

"`% per route extensions`" assumes each of your handlers handles unique
verbs (e.g. no two GET for similar routes implemented in the same module).

Functions exported with `/3` (first argument being `default`) is where you'll define/maintain
generic callback definitions. To add a generic callback, expose a `/2` -equivalent.

Because we're using `erlang:function_exported` to check if a function is exported,
not calling the function directly we use `:module_info` to force code load,
otherwise the first call to each route would "fail".

### How to

Declare your dispatch handlers (and start the server) with something like the following

```erlang
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

    {ok, _} = simpler_cowboy_rest:start(Routes, TransOpts).
```

#### Logger metadata

The application saves logger metadata key `simpler_cowboy_rest_dispatched_to` as an MFA
tuple that can be recovered for tracing your logging entries.

## The project

### Changelog

A complete changelog can be found under [CHANGELOG.md](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/main/CHANGELOG.md).

### Code of Conduct

This project's code of conduct is made explicit in [CODE_OF_CONDUCT.md](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/main/CODE_OF_CONDUCT.md).

### Contributing

First of all, thank you for contributing with your time and patience.

If you want to request a new feature make sure to
[open an issue](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/issues) so we can
discuss it first.

Bug reports and questions are also welcome, but do check you're using the latest version of the
plugin - if you found a bug - and/or search the issue database - if you have a question, since it
might have already been answered before.

Contributions will be subject to the MIT License.
You will retain the copyright.

For more information check out [CONTRIBUTING.md](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/main/CONTRIBUTING.md).

### License

License information can be found inside [LICENSE.md](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/main/LICENSE.md).

### Security

This project's security policy is made explicit in [SECURITY.md](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest/blob/main/SECURITY.md).

### Running tests

Start the example application, in folder `example`, in a `rebar3`/Erlang shell, with
`rebar3 shell`.

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
