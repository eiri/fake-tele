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
2020-04-11T22:18:10.053794-03:00 notice: fmqttc_manager is up
2020-04-11T22:18:10.097484-03:00 notice: fmqttc_manager starting 7 clients
===> Booted sasl
===> Booted fmqttc
2020-04-11T22:18:10.098731-03:00 notice: fmqttc_client a6b0349dbb281059 is up
2020-04-11T22:18:10.098867-03:00 notice: a6b0349dbb281059 temp 0.28°
2020-04-11T22:18:10.099170-03:00 notice: fmqttc_client a0212d5e5c29e49d is up
2020-04-11T22:18:10.099333-03:00 notice: a0212d5e5c29e49d temp -1.07°
2020-04-11T22:18:10.099418-03:00 notice: fmqttc_client 51f069ea0fc42790 is up
2020-04-11T22:18:10.099549-03:00 notice: 51f069ea0fc42790 temp 0.24°
2020-04-11T22:18:10.099759-03:00 notice: fmqttc_client 915dc066b6a77140 is up
2020-04-11T22:18:10.099916-03:00 notice: 915dc066b6a77140 temp -1.64°
2020-04-11T22:18:10.100106-03:00 notice: fmqttc_client a11d8a4bbeb6ab40 is up
2020-04-11T22:18:10.100254-03:00 notice: a11d8a4bbeb6ab40 temp 1.40°
2020-04-11T22:18:10.100345-03:00 notice: fmqttc_client d52c727bf64d8189 is up
2020-04-11T22:18:10.100443-03:00 notice: d52c727bf64d8189 temp 1.83°
2020-04-11T22:18:10.100671-03:00 notice: fmqttc_client f1fd7e37bf0d7198 is up
2020-04-11T22:18:10.100800-03:00 notice: f1fd7e37bf0d7198 temp 0.97°
2020-04-11T22:18:11.099483-03:00 notice: a6b0349dbb281059 temp 2.57°
2020-04-11T22:18:11.100181-03:00 notice: 51f069ea0fc42790 temp -1.19°
2020-04-11T22:18:11.100366-03:00 notice: 915dc066b6a77140 temp -2.40°
2020-04-11T22:18:11.100181-03:00 notice: a0212d5e5c29e49d temp -0.29°
2020-04-11T22:18:11.101211-03:00 notice: a11d8a4bbeb6ab40 temp 2.09°
2020-04-11T22:18:11.101211-03:00 notice: d52c727bf64d8189 temp 1.65°
2020-04-11T22:18:11.101370-03:00 notice: f1fd7e37bf0d7198 temp -0.94°
2020-04-11T22:18:12.100230-03:00 notice: a6b0349dbb281059 temp 0.59°
2020-04-11T22:18:12.101294-03:00 notice: 915dc066b6a77140 temp -2.75°
2020-04-11T22:18:12.101295-03:00 notice: 51f069ea0fc42790 temp -0.45°
2020-04-11T22:18:12.101503-03:00 notice: a0212d5e5c29e49d temp -0.56°
2020-04-11T22:18:12.102352-03:00 notice: f1fd7e37bf0d7198 temp 1.30°
2020-04-11T22:18:12.102352-03:00 notice: a11d8a4bbeb6ab40 temp 4.08°
2020-04-11T22:18:12.102523-03:00 notice: d52c727bf64d8189 temp 2.27°

```

## License

[MIT](https://github.com/eiri/fake-tele/blob/master/LICENSE)
