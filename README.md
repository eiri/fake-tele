# Fake tele
_A fake telemetry MQTT client_

## Description and motivation

This is an erlang app that creates a network of MQTT clients reporting randomly generated telemetry to a broker. The clients can randomly appear, quit, crash or re-join.

The propose of the app is to simulate traffic for testing of a MQTT broker.

## Build

```
$ brew install rebar3
$ rebar3 compile
```

## Run

```
$ rebar3 shell --apps fmqttc
===> Verifying dependencies...
===> Compiling fmqttc
Erlang/OTP 22 [erts-10.7.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]

Eshell V10.7.1  (abort with ^G)
1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/docs/releases)
===> Booted sasl
2020-04-11T21:08:02.198797-03:00 notice: fmqttc_manager is up
===> Booted fmqttc
2020-04-11T21:08:02.220964-03:00 notice: fmqttc_worker (1) child is up

```

## License

[MIT](https://github.com/eiri/fake-tele/blob/master/LICENSE)
