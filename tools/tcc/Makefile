TCC_DIR := tools/tcc/tools/tcc/tcc
LIB := $(TCC_DIR)/libtcc.a
BIN := mochi-tcc

.PHONY: libtcc run test clean ensure mochi-tcc

ensure:
	go run ./tools.go

$(LIB): ensure
	cd $(TCC_DIR) && ./configure --enable-static && make libtcc.a

libtcc: $(LIB)

run: $(LIB)
	go run -tags 'tcc libtcc' ./main.go

test: $(LIB)
	go test -tags 'tcc libtcc'

mochi-tcc: $(LIB)
	go build -tags 'tcc libtcc' -o $(BIN) ../../cmd/mochi-tcc
	@echo "Built $(BIN)"

clean:
	rm -f $(LIB) $(BIN)
