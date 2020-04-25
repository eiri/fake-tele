# Fake tele
_A fake telemetry MQTT client_

## Description and motivation

This is an erlang app that creates a network of MQTT clients reporting randomly generated telemetry to a broker. The clients can randomly appear, quit, crash or re-join.

The propose of the app is to simulate traffic for testing of a MQTT broker.

![Grafana dashboard](grafana.png?raw=true "Temperature graph")

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
docker-compose logs -f mosquitto
Attaching to mosquitto
mosquitto    | 2020-04-25T18:19:15: mosquitto version 1.6.9 starting`
mosquitto    | 2020-04-25T18:19:15: Config loaded from /mosquitto/config/mosquitto.conf.
mosquitto    | 2020-04-25T18:19:15: Opening ipv4 listen socket on port 1883.
mosquitto    | 2020-04-25T18:19:15: Opening ipv6 listen socket on port 1883.
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
2020-04-25T15:20:09.381384-03:00 notice: fmqttc_manager is up
2020-04-25T15:20:09.427119-03:00 notice: fmqttc_manager starting 12 clients
2020-04-25T15:20:09.430304-03:00 notice: fmqttc_monitor is up
===> Booted sasl
===> Booted cowlib
===> Booted gun
===> Booted emqtt
===> Booted fmqttc
2020-04-25T15:20:09.431887-03:00 notice: fmqttc_client ffee974fe94dfe32 is up
2020-04-25T15:20:09.432349-03:00 notice: fmqttc_client 389f9e8905e21811 is up
2020-04-25T15:20:09.432988-03:00 notice: fmqttc_client 8f2e5995c7f6843e is up
2020-04-25T15:20:09.433275-03:00 notice: fmqttc_client 6996cdde9a35523b is up
2020-04-25T15:20:09.433651-03:00 notice: fmqttc_client 6a0002e73bc5cdad is up
2020-04-25T15:20:09.433999-03:00 notice: fmqttc_client 5ae086e473d5b6ab is up
2020-04-25T15:20:09.434335-03:00 notice: fmqttc_client 68af7a2b746e102e is up
2020-04-25T15:20:09.434658-03:00 notice: fmqttc_client 33999f0612c1635 is up
2020-04-25T15:20:09.434971-03:00 notice: fmqttc_client 4ccada6a999b51b4 is up
2020-04-25T15:20:09.435618-03:00 notice: fmqttc_client 82a72eb96067cb1e is up
2020-04-25T15:20:09.435880-03:00 notice: fmqttc_client f4824ef378ca5477 is up
2020-04-25T15:20:09.436103-03:00 notice: fmqttc_client 24f9af266961eddf is up
2020-04-25T15:20:09.533224-03:00 notice: fmqttc_monitor drop db fake-tele
2020-04-25T15:20:09.590110-03:00 notice: fmqttc_monitor create db fake-tele
2020-04-25T15:20:09.590314-03:00 notice: fmqttc_monitor subscribe to valve/#
2020-04-25T15:20:10.426776-03:00 notice: valve/1 temp 2.77°
2020-04-25T15:20:10.427194-03:00 notice: fmqttc_monitor store point device,topic=valve/1,type=valve,num=1 temperature=2.773579 1587838810426
2020-04-25T15:20:10.529602-03:00 notice: valve/11 temp 0.59°
2020-04-25T15:20:10.529970-03:00 notice: fmqttc_monitor store point device,topic=valve/11,type=valve,num=11 temperature=0.591063 1587838810529
2020-04-25T15:20:10.571613-03:00 notice: valve/10 temp -0.84°
2020-04-25T15:20:10.572106-03:00 notice: fmqttc_monitor store point device,topic=valve/10,type=valve,num=10 temperature=-0.836942 1587838810571
```

### Check server logs

```
mosquitto    | 2020-04-25T18:20:09: New connection from 172.19.0.1 on port 1883.
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 4ccada6a999b51b4 (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as ffee974fe94dfe32 (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 68af7a2b746e102e (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as f4824ef378ca5477 (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 5ae086e473d5b6ab (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 8f2e5995c7f6843e (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 389f9e8905e21811 (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 6a0002e73bc5cdad (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 24f9af266961eddf (p2, c1, k60).
mosquitto    | 2020-04-25T18:20:09: New client connected from 172.19.0.1 as 82a72eb96067cb1e (p2, c1, k60).
```

### Create grafana dashboard

Open http://localhost:3000. Login as `admin:admin`. Add data source `InfluxDB` with name "InfluxDB", `url` http://influxdb:8086 and database "fake-tele".

Go to "Home" (in page header), choose sub-menu "Import dashboard" and upload or paste json from `infra/grafana-dashboard.json`.

Go to created dashboard "MQTT".

### Stop servers

```
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
