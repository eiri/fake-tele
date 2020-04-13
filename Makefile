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
	docker-compose up -d

.PHONY: server-logs
server-logs:
	docker-compose logs -f mosquitto

.PHONY: server-stop
server-stop:
	docker-compose down
