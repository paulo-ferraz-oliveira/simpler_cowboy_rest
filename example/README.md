# `simpler_cowboy_rest_example`

This is an example project (`rebar3`-based) for showcasing
[simpler_cowboy_rest](https://github.com/paulo-ferraz-oliveira/simpler_cowboy_rest)'s
capabilities.

To run it, you need to create a release: e.g. `rebar3 release`.

Then start it: e.g.
`_build/default/rel/simpler_cowboy_rest_example/bin/simpler_cowboy_rest_example daemon`.

If no errors were show, the application should now be served the application in `localhost:8080`.
Check `http://localhost:8080/health`, for example.
