.DEFAULT_GOAL := all

PROJECT := fmqttc

.PHONY: all
all: build

.PHONY: build
build:
	rebar3 compile

.PHONY: run
run:
	rebar3 shell --apps=$(PROJECT)

.PHONY: server-start
server-start:
	docker run --name mosquitto --rm -d -p 1883:1883 -p 9001:9001 -v $(PWD)/mosquitto/mosquitto.conf:/mosquitto/config/mosquitto.conf eclipse-mosquitto:1.6

.PHONY: server-logs
server-logs:
	docker logs -f mosquitto

.PHONY: server-stop
server-stop:
	docker stop mosquitto
