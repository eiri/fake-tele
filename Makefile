.DEFAULT_GOAL := all

PROJECT := fmqttc

.PHONY: all
all: build

.PHONY: format
format:
	rebar3 fmt

.PHONY: build
build: format
	rebar3 compile

.PHONY: run
run:
	rebar3 shell

.PHONY: server-start
server-start:
	docker-compose up -d

.PHONY: server-logs
server-logs:
	docker-compose logs -f mosquitto

.PHONY: server-stop
server-stop:
	docker-compose down

.PHONY: query-influx
query-influx:
	curl -q -H 'Accept: application/csv' -G 'http://localhost:8086/query?db=fake-tele' --data-urlencode 'q=SELECT * FROM device GROUP BY topic ORDER BY time DESC LIMIT 10'
