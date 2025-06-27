.PHONY: run test ensure build

BIN := ../../bin/mochi-cosmo

ensure:
	go run ./tools.go

run:
	go run -tags "cosmo libcosmo" ./main.go

test:
	go test -tags "cosmo libcosmo"

build: ensure
	mkdir -p $(dir $(BIN))
	go build -tags "cosmo libcosmo" -o $(BIN) ../../cmd/mochi-cosmo
