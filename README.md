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

### Start server

_This assumes that docker is available, up and running_

```
$ make server-start
docker run --name mosquitto --rm -d -p 1883:1883 -p 9001:9001 -v mosquitto/mosquitto.conf:/mosquitto/config/mosquitto.conf eclipse-mosquitto:1.6
b0921b11f3457c4948702c86014e5834b399c2208a5f017fc33a1c1e29c6c4b1

$ make server-logs
docker logs -f mosquitto
2020-04-12T16:20:38: mosquitto version 1.6.9 starting
2020-04-12T16:20:38: Config loaded from /mosquitto/config/mosquitto.conf.
2020-04-12T16:20:38: Opening ipv4 listen socket on port 1883.
2020-04-12T16:20:38: Opening ipv6 listen socket on port 1883.
```

### Start client

```
$ make run
rebar3 shell --apps=fmqttc
===> Verifying dependencies...
===> Compiling fmqttc
Erlang/OTP 22 [erts-10.7.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]

Eshell V10.7.1  (abort with ^G)
1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/docs/releases)
2020-04-12T13:20:49.255756-03:00 notice: fmqttc_manager is up
2020-04-12T13:20:49.305957-03:00 notice: fmqttc_manager starting 5 clients
===> Booted sasl
===> Booted fmqttc
2020-04-12T13:20:49.307639-03:00 notice: fmqttc_client 5f5d38498dcf98df is up
2020-04-12T13:20:49.308084-03:00 notice: fmqttc_client 353fdc0f419d0c03 is up
2020-04-12T13:20:49.308493-03:00 notice: fmqttc_client 3e7b1fbcd6ab5bc is up
2020-04-12T13:20:49.308834-03:00 notice: fmqttc_client 37ea3a43b96935a is up
2020-04-12T13:20:49.309132-03:00 notice: fmqttc_client 8b95e906ff489774 is up
make: *** [run] Interrupt: 2
```

### Check server logs, stop server

```
2020-04-12T16:20:49: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:20:49: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:20:49: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:20:49: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:20:49: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:20:49: New client connected from 172.17.0.1 as 5f5d38498dcf98df (p2, c1, k60).
2020-04-12T16:20:49: New client connected from 172.17.0.1 as 8b95e906ff489774 (p2, c1, k60).
2020-04-12T16:20:49: New client connected from 172.17.0.1 as 3e7b1fbcd6ab5bc (p2, c1, k60).
2020-04-12T16:20:49: New client connected from 172.17.0.1 as 37ea3a43b96935a (p2, c1, k60).
2020-04-12T16:20:49: New client connected from 172.17.0.1 as 353fdc0f419d0c03 (p2, c1, k60).
2020-04-12T16:20:52: Socket error on client 3e7b1fbcd6ab5bc, disconnecting.
2020-04-12T16:20:52: Socket error on client 37ea3a43b96935a, disconnecting.
2020-04-12T16:20:52: Socket error on client 5f5d38498dcf98df, disconnecting.
2020-04-12T16:20:52: Socket error on client 353fdc0f419d0c03, disconnecting.
2020-04-12T16:20:52: Socket error on client 8b95e906ff489774, disconnecting.
c^Cmake: *** [server-logs] Interrupt: 2

$ make server-stop
docker stop mosquitto
mosquitto
```

## License

[MIT](https://github.com/eiri/fake-tele/blob/master/LICENSE)
