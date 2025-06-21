.DEFAULT_GOAL := help
SHELL := /bin/bash

MOCHI := go run ../../cmd/mochi/main.go
SERVER := go run ../../cmd/cloud-server/main.go
PORT ?= 7777

.PHONY: test server help

## Run the in-memory cloud server
server:
	$(SERVER) -addr :$(PORT)

## Run Mochi tests against the local server
test:
	$(SERVER) -addr :$(PORT) & \
	pid=$$!; \
	sleep 0.5; \
	$(MOCHI) test ./cloud_test.mochi; \
	kill $$pid

## Show available commands
help:
	@echo "Commands:";
	@grep -E '^[a-zA-Z_-]+:.*?##' $(MAKEFILE_LIST) | awk 'BEGIN {FS=":"}; {printf "  %-10s %s\n", $$1, $$2}'
