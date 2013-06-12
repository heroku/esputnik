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

### Configuration

``` erlang
-type sputnik_api_endpoint() :: binary().

[{esputnik, [{sputnik_api_endpoint, sputnik_api_endpoint()}]}].
```

### Sending a sputnik alert

``` erlang
-type team_name() :: iolist().
-type message() :: iolist().
-type alert_type() :: alert|resolve.
-type request_id() :: iolist().
-type alert_error() :: internal_error|{code, pos_integer()}.
-spec alert(alert_type(), team_name(), message()) ->
                   {ok, request_id()}|{error, alert_error()}.
```

You can specify some options as such

``` erlang
-type alert_opt() :: {request_id, iolist()}|{message_id, iolist()}|
                          {priority, priority()}.
-type alert_opts() :: [alert_opt()].
-spec alert(alert_type(), team_name(), message(), alert_opts()) ->
                   {ok, request_id()}|{error, alert_error()}.
```
