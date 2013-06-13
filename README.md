# esputnik

Send alerts into Sputnik

## Setup

```
$ make
```

## Tests

```
$ make test
```

## Erlang API

``` erlang
-type team_name() :: iolist().
-type message() :: iolist().
-type alert_type() :: alert|resolve.
-type request_id() :: iolist().
-type alert_error() :: internal_error|{code, pos_integer()}.
-type sputnik_path() :: iolist().
-opaque connection().
-type sputnik_server() :: sputnik_path()|connection().
-type alert_output() :: {ok, request_id(), connection()}|
                         {error, alert_error(), connection()}.
                         
esputnik_api:alert(sputnik_server(), sputnik_message()) -> alert_output().
esputnik_api:alert(sputnik_server(), alert_type(), team_name(), message()) -> alert_output().
esputnik_api:alert(sputnik_server(), alert_type(), team_name(), message(), alert_opts()) -> alert_output().
```

You can also create a server and cast messages to it.

``` erlang
esputnik:start_link(esputnik_api:sputnik_path()) -> {ok, pid()}.
```

If you prefer you can get the child_specs to use

``` erlang
esputnik:child_spec(esputnik_api:sputnik_path()) -> supervisor:child_spec().
```

To send messages once you'we spawned a server

``` erlang
esputnik:alert(alert_type(), team_name(), message()) -> ok.
esputnik:alert(esputnik_api:alert_type(), esputnik_api:team_name(), esputnik_api:message(), esputnik_api:alert_opts()) -> ok.
```
