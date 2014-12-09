# esputnik

Send alerts to a Sputnik server

[![Build Status](https://magnum.travis-ci.com/heroku/esputnik.png?token=xVUSrt9RJn9ZjQwspdLg&branch=master)](https://magnum.travis-ci.com/heroku/esputnik)

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
[{esputnik, [{sputnik_api_url, iodata()},
             {connect_timeout, pos_integer()}, % ms, default 2000
             {recv_timeout, pos_integer()}, % ms, default 2000
             {throttle_time, integer()} % ms, default 1000
            ]}].
```

This configuration variable is used in the `esputnik` stateful `gen_server`.

## Erlang API

## Types

``` erlang
-type team_name() :: iodata().
-type message() :: iodata().
-type alert_type() :: alert|resolve.
-type request_id() :: iodata().
-type alert_error() :: internal_error|{code, pos_integer()}.
-opaque connection().
-type sputnik_api_url() :: iodata().
-type sputnik_server() :: sputnik_api_url()|connection().
-type alert_output() :: {ok, request_id(), connection()}|
                         {error, alert_error(), connection()}.
-type alert_opt() :: {request_id, iodata()}|{message_id, iodata()}|
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
