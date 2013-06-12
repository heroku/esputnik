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

### Sending a sputnik alert

``` erlang
-type team_name() :: iolist().
-type message() :: iolist().
-type alert_type() :: alert|resolve.
-type request_id() :: iolist().
-type alert_error() :: internal_error|{code, pos_integer()}.
-spec alert(team_name(), message(), alert_type()) ->
                   {ok, request_id()}|{error, alert_error()}.
```

You can specify some options as such

``` erlang
-type alert_opt() :: {request_id, iolist()}|{message_id, iolist()}|
                          {priority, priority()}.
-type alert_opts() :: [alert_opt()].
-spec alert(team_name(), message(), alert_type(), alert_opts()) ->
                   {ok, request_id()}|{error, alert_error()}.
```
