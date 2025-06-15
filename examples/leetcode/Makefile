MOCHI_BIN := $(CURDIR)/bin/mochi
MOCHI_ROOT := $(abspath $(CURDIR)/../..)

# Detect OS/ARCH for downloading the correct release asset
OS  := $(shell uname -s)
ARCH := $(shell uname -m)

# Normalize values used in Mochi release names
ifeq ($(OS),Darwin)
OS := Darwin
else
OS := Linux
endif

ifeq ($(ARCH),x86_64)
ARCH := x86_64
else ifeq ($(ARCH),aarch64)
ARCH := arm64
else ifeq ($(ARCH),arm64)
ARCH := arm64
endif

MOCHI_TAR := mochi_$(OS)_$(ARCH).tar.gz
MOCHI_URL := https://github.com/mochilang/mochi/releases/latest/download/$(MOCHI_TAR)

.PHONY: mochi run test clean

mochi:
	@mkdir -p bin
	@if [ ! -f $(MOCHI_BIN) ]; then \
	        echo "Building Mochi from source..."; \
	        cd $(MOCHI_ROOT) && go build -o $(CURDIR)/bin/mochi ./cmd/mochi; \
	fi
	@if [ ! -f $(MOCHI_BIN) ]; then \
	        echo "Downloading Mochi binary for $(OS)/$(ARCH)..."; \
	        curl -L $(MOCHI_URL) -o bin/$(MOCHI_TAR); \
	        tar -xzf bin/$(MOCHI_TAR) -C bin; \
	        rm -f bin/$(MOCHI_TAR); \
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
	@find . -name '*.mochi' -print0 | xargs -0 -n1 $(MOCHI_BIN) test

clean:
	@rm -rf bin
