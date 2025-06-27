COSMOCC := cosmocc

.PHONY: run test clean ensure

ensure:
	go run ./tools.go

run: ensure
	go run -tags cosmo ./main.go

test: ensure
	go test -tags cosmo

clean:
	rm -f bin/cosmocc
