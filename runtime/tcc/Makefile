TCC_TOOLS := ../../tools/tcc
LIB := $(TCC_TOOLS)/tcc/libtcc.a
BIN := ../../bin/mochi-tcc

.PHONY: all build libtcc clean linux windows

all: build

libtcc:
$(MAKE) -C $(TCC_TOOLS) libtcc

build: libtcc
go build -tags 'tcc libtcc' -o $(BIN) ../../cmd/mochi-tcc
@echo "Built $(BIN)"

linux: libtcc
GOOS=linux GOARCH=amd64 go build -tags 'tcc libtcc' -o $(BIN)-linux ../../cmd/mochi-tcc

windows: libtcc
GOOS=windows GOARCH=amd64 go build -tags 'tcc libtcc' -o $(BIN)-win.exe ../../cmd/mochi-tcc

clean:
rm -f $(BIN) $(BIN)-linux $(BIN)-win.exe
