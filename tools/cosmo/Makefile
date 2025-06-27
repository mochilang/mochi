.PHONY: run test ensure build

BIN := ../../bin/mochi-cosmo

ensure:
	go run ./tools.go

run:
       go run -tags "cosmo" ./main.go

test:
       go test -tags "cosmo"

build: ensure
       mkdir -p $(dir $(BIN))
       go build -tags "cosmo" -o $(BIN) ../../cmd/mochi-cosmo
