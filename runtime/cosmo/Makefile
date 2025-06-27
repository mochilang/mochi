COSMO_TOOLS := ../../tools/cosmo
BIN := ../../bin/mochi-cosmo

.PHONY: all build clean linux windows macos

all: build

build:
go build -tags cosmo -o $(BIN) ../../cmd/mochi-cosmo
@echo "Built $(BIN)"

linux:
GOOS=linux GOARCH=amd64 go build -tags cosmo -o $(BIN)-linux ../../cmd/mochi-cosmo

windows:
GOOS=windows GOARCH=amd64 go build -tags cosmo -o $(BIN)-win.exe ../../cmd/mochi-cosmo

macos:
GOOS=darwin GOARCH=amd64 go build -tags cosmo -o $(BIN)-macos ../../cmd/mochi-cosmo

clean:
rm -f $(BIN) $(BIN)-linux $(BIN)-win.exe $(BIN)-macos
