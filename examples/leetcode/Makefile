MOCHI_BIN := $(CURDIR)/bin/mochi

.PHONY: mochi run test clean

mochi:
	@mkdir -p bin
	@if [ ! -f $(MOCHI_BIN) ]; then \
		echo "Downloading Mochi binary..."; \
		curl -L https://github.com/mochilang/mochi/releases/latest/download/mochi -o $(MOCHI_BIN); \
		chmod +x $(MOCHI_BIN); \
	else \
		echo "Using $(MOCHI_BIN)"; \
	fi

run: mochi
	@if [ -z "$(ID)" ]; then \
		echo "Usage: make run ID=<problem number>"; \
		exit 1; \
	fi
	@$(MOCHI_BIN) run $(ID)/*.mochi

test: mochi
	@$(MOCHI_BIN) test ./...

clean:
	@rm -rf bin
