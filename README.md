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
make server-start
docker-compose up -d
Creating network "fake-tele_default" with the default driver
Creating mosquitto ... done
Creating influxdb  ... done
Creating grafana   ... done

$ make server-logs
docker-compose logs -f mosquitto
Attaching to mosquitto
mosquitto    | 2020-04-13T00:19:23: mosquitto version 1.6.9 starting
mosquitto    | 2020-04-13T00:19:23: Config loaded from /mosquitto/config/mosquitto.conf.
mosquitto    | 2020-04-13T00:19:23: Opening ipv4 listen socket on port 1883.
mosquitto    | 2020-04-13T00:19:23: Opening ipv6 listen socket on port 1883.
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
2020-04-12T21:20:24.675651-03:00 notice: fmqttc_manager is up
2020-04-12T21:20:24.728720-03:00 notice: fmqttc_manager starting 1 clients
2020-04-12T21:20:24.730597-03:00 notice: fmqttc_monitor is up
===> Booted sasl
===> Booted emqtt
===> Booted fmqttc
2020-04-12T21:20:24.732063-03:00 notice: fmqttc_client 71f7b4c56d0c1bda is up
2020-04-12T21:20:25.886894-03:00 notice: valve/1 temp -0.85°
2020-04-12T21:20:26.979647-03:00 notice: valve/1 temp -0.88°
2020-04-12T21:20:28.073851-03:00 notice: valve/1 temp -0.97°
2020-04-12T21:20:29.166302-03:00 notice: valve/1 temp -1.45°
2020-04-12T21:20:30.259340-03:00 notice: valve/1 temp -1.92°
make: *** [run] Interrupt: 2
```

### Check server logs, stop server

```
mosquitto    | 2020-04-13T00:20:24: New connection from 172.20.0.1 on port 1883.
mosquitto    | 2020-04-13T00:20:24: New connection from 172.20.0.1 on port 1883.
mosquitto    | 2020-04-13T00:20:24: New client connected from 172.20.0.1 as monitor (p2, c1, k60).
mosquitto    | 2020-04-13T00:20:24: New client connected from 172.20.0.1 as 71f7b4c56d0c1bda (p2, c1, k60).
mosquitto    | 2020-04-13T00:20:30: Socket error on client monitor, disconnecting.
mosquitto    | 2020-04-13T00:20:30: Socket error on client 71f7b4c56d0c1bda, disconnecting.
^CERROR: Aborting.
make: *** [server-logs] Error 1

$ make server-stop
docker-compose down
Stopping grafana   ... done
Stopping mosquitto ... done
Stopping influxdb  ... done
Removing grafana   ... done
Removing mosquitto ... done
Removing influxdb  ... done
Removing network fake-tele_default
```

## License

[MIT](https://github.com/eiri/fake-tele/blob/master/LICENSE)
