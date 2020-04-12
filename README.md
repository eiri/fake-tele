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
2020-04-12T13:55:49.953012-03:00 notice: fmqttc_manager is up
2020-04-12T13:55:50.005459-03:00 notice: fmqttc_manager starting 11 clients
2020-04-12T13:55:50.006918-03:00 notice: fmqttc_monitor is up
===> Booted sasl
===> Booted emqtt
===> Booted fmqttc
2020-04-12T13:55:50.009021-03:00 notice: fmqttc_client 41933fed1cbefab is up
2020-04-12T13:55:50.009457-03:00 notice: fmqttc_client 3ae45a92b82599a1 is up
2020-04-12T13:55:50.009742-03:00 notice: fmqttc_client 2b12ba459cf72eb7 is up
2020-04-12T13:55:50.010036-03:00 notice: fmqttc_client be3ca650420788a7 is up
2020-04-12T13:55:50.010603-03:00 notice: fmqttc_client 77c5db62a50b4a34 is up
2020-04-12T13:55:50.010958-03:00 notice: fmqttc_client e44b23202c7947fb is up
2020-04-12T13:55:50.011258-03:00 notice: fmqttc_client cf8b0ad72f7d0a83 is up
2020-04-12T13:55:50.011582-03:00 notice: fmqttc_client 1dcb623a5626e377 is up
2020-04-12T13:55:50.011847-03:00 notice: fmqttc_client bede5ea8d6d6beb4 is up
2020-04-12T13:55:50.012097-03:00 notice: fmqttc_client a17195820e81351b is up
2020-04-12T13:55:50.012359-03:00 notice: fmqttc_client 2bf92906da1ff0bc is up
2020-04-12T13:55:50.145188-03:00 notice: valve/2 temp -1.14°
2020-04-12T13:55:50.145393-03:00 notice: valve/8 temp -1.02°
2020-04-12T13:55:50.145541-03:00 notice: valve/11 temp 1.78°
2020-04-12T13:55:50.939233-03:00 notice: valve/7 temp -3.35°
2020-04-12T13:55:50.942606-03:00 notice: valve/6 temp -1.73°
2020-04-12T13:55:50.965943-03:00 notice: valve/9 temp 0.50°
2020-04-12T13:55:51.014674-03:00 notice: valve/3 temp 0.28°
2020-04-12T13:55:51.038658-03:00 notice: valve/10 temp 1.15°
2020-04-12T13:55:51.048845-03:00 notice: valve/1 temp -1.02°
2020-04-12T13:55:51.114618-03:00 notice: valve/4 temp 2.49°
2020-04-12T13:55:51.162162-03:00 notice: valve/11 temp 1.86°
2020-04-12T13:55:51.178096-03:00 notice: valve/8 temp -0.92°
2020-04-12T13:55:51.289377-03:00 notice: valve/2 temp -1.20°
2020-04-12T13:55:51.293706-03:00 notice: valve/5 temp -0.96°
2020-04-12T13:55:51.776663-03:00 notice: valve/7 temp -1.91°
2020-04-12T13:55:51.784030-03:00 notice: valve/6 temp -2.63°
2020-04-12T13:55:51.846197-03:00 notice: valve/9 temp 1.35°
make: *** [run] Interrupt: 2
```

### Check server logs, stop server

```
20-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New connection from 172.17.0.1 on port 1883.
2020-04-12T16:55:50: New client connected from 172.17.0.1 as bede5ea8d6d6beb4 (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as monitor (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as a17195820e81351b (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as 1dcb623a5626e377 (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as cf8b0ad72f7d0a83 (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as 2b12ba459cf72eb7 (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as 3ae45a92b82599a1 (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as 41933fed1cbefab (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as be3ca650420788a7 (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as e44b23202c7947fb (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as 2bf92906da1ff0bc (p2, c1, k60).
2020-04-12T16:55:50: New client connected from 172.17.0.1 as 77c5db62a50b4a34 (p2, c1, k60).
2020-04-12T16:55:51: Socket error on client bede5ea8d6d6beb4, disconnecting.
2020-04-12T16:55:51: Socket error on client 2bf92906da1ff0bc, disconnecting.
2020-04-12T16:55:51: Socket error on client e44b23202c7947fb, disconnecting.
2020-04-12T16:55:51: Socket error on client a17195820e81351b, disconnecting.
2020-04-12T16:55:51: Socket error on client cf8b0ad72f7d0a83, disconnecting.
2020-04-12T16:55:51: Socket error on client be3ca650420788a7, disconnecting.
2020-04-12T16:55:51: Socket error on client 1dcb623a5626e377, disconnecting.
2020-04-12T16:55:51: Socket error on client 2b12ba459cf72eb7, disconnecting.
2020-04-12T16:55:51: Socket error on client 41933fed1cbefab, disconnecting.
2020-04-12T16:55:51: Socket error on client monitor, disconnecting.
2020-04-12T16:55:51: Socket error on client 77c5db62a50b4a34, disconnecting.
2020-04-12T16:55:51: Socket error on client 3ae45a92b82599a1, disconnecting.
^Cmake: *** [server-logs] Interrupt: 2

$ make server-stop
docker stop mosquitto
mosquitto
```

## License

[MIT](https://github.com/eiri/fake-tele/blob/master/LICENSE)
