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

1> l(fmqttc).
{module,fmqttc}

2> {ok, Pid} = fmqttc:start_child(a).
2020-04-11T19:39:44.974147-03:00 notice: fmqttc_client (a) child is up
{ok,<0.154.0>}

3> fmqttc:stop_child(Pid).
2020-04-11T19:40:01.760324-03:00 notice: fmqttc_client child is down
ok
```

## License

[MIT](https://github.com/eiri/fake-tele/blob/master/LICENSE)
