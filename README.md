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

## Configuration

You need to specify a `sputnik_server`

``` erlang
[{esputnik, [{sputnik_server, iolist()}]}].
```

This configuration variable is used in the `esputnik` stateful `gen_server`.

## Erlang API

## Types

``` erlang
-type team_name() :: iolist().
-type message() :: iolist().
-type alert_type() :: alert|resolve.
-type request_id() :: iolist().
-type alert_error() :: internal_error|{code, pos_integer()}.
-opaque connection().
-type sputnik_path() :: iolist().
-type sputnik_server() :: sputnik_path()|connection().
-type alert_output() :: {ok, request_id(), connection()}|
                         {error, alert_error(), connection()}.
-type alert_opt() :: {request_id, iolist()}|{message_id, iolist()}|
                          {priority, priority()}.
```

## Server API

``` erlang
esputnik:alert(alert_type(), team_name(), message()) -> ok.
esputnik:alert(esputnik_api:alert_type(), esputnik_api:team_name(), esputnik_api:message(), esputnik_api:alert_opts()) -> ok.
```

## API module

``` erlang
esputnik_api:alert(sputnik_server(), sputnik_message()) -> alert_output().
esputnik_api:alert(sputnik_server(), alert_type(), team_name(), message()) -> alert_output().
esputnik_api:alert(sputnik_server(), alert_type(), team_name(), message(), alert_opts()) -> alert_output().
```
