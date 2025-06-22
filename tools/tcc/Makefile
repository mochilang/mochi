TCC_DIR := tcc
LIB := $(TCC_DIR)/libtcc.a

.PHONY: libtcc run test clean ensure

ensure:
	go run ./tools.go

libtcc: ensure
	cd $(TCC_DIR) && ./configure --disable-static --enable-static && make libtcc.a

run: $(LIB)
	go run -tags 'tcc libtcc' ./main.go

test: $(LIB)
	go test -tags 'tcc libtcc'

clean:
	rm -f $(LIB)
