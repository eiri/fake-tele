# Fake tele
_A fake telemetry MQTT client_

![test](https://github.com/eiri/fake-tele/workflows/test/badge.svg)

## Description and motivation

This is an erlang app that creates a network of MQTT clients reporting randomly generated telemetry to a broker.

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
rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling fmqttc
Erlang/OTP 22 [erts-10.7.2.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.7.2.3  (abort with ^G)
1> manager <0.212.0> --- status: up
manager <0.212.0> --- count: 12, msg: starting clients, op: start
monitor <0.214.0> --- status: up
===> Booted sasl
===> Booted cowlib
===> Booted gun
===> Booted emqtt
===> Booted fmqttc
client <0.216.0> --- name: <<"8a934c827237d3b4">>, status: up
client <0.217.0> --- name: <<"48c6dc18dc99cba6">>, status: up
client <0.218.0> --- name: <<"933aba898dea2ff3">>, status: up
client <0.219.0> --- name: <<"33e0e341b9a259c">>, status: up
client <0.220.0> --- name: <<"c0cdd9efe89868a4">>, status: up
client <0.221.0> --- name: <<"760eeafc915f44ba">>, status: up
client <0.222.0> --- name: <<"7f27def4b2a2507c">>, status: up
client <0.223.0> --- name: <<"8e84bb80f8f8aed5">>, status: up
client <0.224.0> --- name: <<"99fe1f420a6cc671">>, status: up
client <0.225.0> --- name: <<"8d708964589d0174">>, status: up
client <0.226.0> --- name: <<"adccc78740de8393">>, status: up
client <0.227.0> --- name: <<"7124b7dd8b8014aa">>, status: up
monitor <0.214.0> --- db_name: fake-tele, op: drop_db
client <0.221.0> --- op: publish, temperature: -2.9434974645733645, topic: <<"valve/6">>
client <0.216.0> --- op: publish, temperature: -1.072523188204584, topic: <<"valve/1">>
client <0.222.0> --- op: publish, temperature: -1.5642711496985728, topic: <<"valve/7">>
client <0.220.0> --- op: publish, temperature: 2.4016237197555386, topic: <<"valve/5">>
client <0.226.0> --- op: publish, temperature: -7.0537612024331535, topic: <<"valve/11">>
client <0.224.0> --- op: publish, temperature: -2.3359506488698725, topic: <<"valve/9">>
client <0.218.0> --- op: publish, temperature: 1.685536958010378, topic: <<"valve/3">>
client <0.219.0> --- op: publish, temperature: -0.3899430400831154, topic: <<"valve/4">>
client <0.225.0> --- op: publish, temperature: -0.23322251275841133, topic: <<"valve/10">>
client <0.223.0> --- op: publish, temperature: -4.533438180553141, topic: <<"valve/8">>
client <0.227.0> --- op: publish, temperature: 0.15495664336423534, topic: <<"valve/12">>
monitor <0.214.0> --- db_name: fake-tele, op: create_db
monitor <0.214.0> --- op: subscribe, topic: <<"valve/#">>
client <0.221.0> --- op: publish, temperature: -3.2812143280975694, topic: <<"valve/6">>
monitor <0.214.0> --- op: received, temperature: -3.28°, topic: <<"valve/6">>
monitor <0.214.0> --- op: store, point: <<"device,topic=valve/6,type=valve,num=6 temperature=-3.281214 1600690670319">>
client <0.227.0> --- op: publish, temperature: 1.3545571741741156, topic: <<"valve/12">>
monitor <0.214.0> --- op: received, temperature: 1.35°, topic: <<"valve/12">>
monitor <0.214.0> --- op: store, point: <<"device,topic=valve/12,type=valve,num=12 temperature=1.354557 1600690670336">>
client <0.226.0> --- op: publish, temperature: -7.510741794056561, topic: <<"valve/11">>
monitor <0.214.0> --- op: received, temperature: -7.51°, topic: <<"valve/11">>
monitor <0.214.0> --- op: store, point: <<"device,topic=valve/11,type=valve,num=11 temperature=-7.510742 1600690670366">>
client <0.222.0> --- op: publish, temperature: -3.6949636784426114, topic: <<"valve/7">>
monitor <0.214.0> --- op: received, temperature: -3.69°, topic: <<"valve/7">>
monitor <0.214.0> --- op: store, point: <<"device,topic=valve/7,type=valve,num=7 temperature=-3.694964 1600690670408">>
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
