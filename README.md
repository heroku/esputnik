# esputnik

Send alerts to a Sputnik server

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
[{esputnik, [{sputnik_api_url, iolist()},
             {connect_timeout, pos_integer()}, % ms, default 2000
             {recv_timeout, pos_integer()} % ms, default 2000
            ]}].
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
-type sputnik_api_url() :: iolist().
-type sputnik_server() :: sputnik_api_url()|connection().
-type alert_output() :: {ok, request_id(), connection()}|
                         {error, alert_error(), connection()}.
-type alert_opt() :: {request_id, iolist()}|{message_id, iolist()}|
                          {priority, priority()}.
```

## Server API

``` erlang
esputnik:alert(alert_type(), team_name(), message()) -> ok.
esputnik:alert(esputnik_api:alert_type(), esputnik_api:team_name(), esputnik_api:message(), esputnik_api:alert_opts()) -> ok.
change_api_url(esputnik_api:sputnik_api_url()) -> {changed, esputnik_api:sputnik_api_url(),
                                                   esputnik_api:sputnik_api_url()}.
```

## API module

``` erlang
to_sputnik_message(alert_type(), team_name(), message(), alert_opts()) -> {ok, sputnik_message()}.
send_alert(sputnik_server(), sputnik_message(), sputnik_connect_opts()) -> alert_output().
send_alert(sputnik_server(), sputnik_message()) -> alert_output().
```
