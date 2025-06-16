.DEFAULT_GOAL := help
SHELL := /bin/bash

MOCHI_BIN := $(CURDIR)/bin/mochi
MOCHI_ROOT := $(abspath $(CURDIR)/../..)

# OS/ARCH detection
OS  := $(shell uname -s)
ARCH := $(shell uname -m)

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

.PHONY: mochi run run-go test compile clean help

mochi: ## Build or download the Mochi binary
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

run: mochi ## Run a .mochi file. Usage: make run ID=<problem number>
	@if [ -z "$(ID)" ]; then \
		echo "❌ Usage: make run ID=<problem number>"; \
		exit 1; \
	fi
	@$(MOCHI_BIN) run $(ID)/*.mochi

run-go: mochi ## Compile and run Go output. Usage: make run-go ID=<problem number>
	@if [ -z "$(ID)" ]; then \
		echo "❌ Usage: make run-go ID=<problem number>"; \
		exit 1; \
	fi
	@dir=$(ID); \
	file=$$(ls "$$dir"/*.mochi 2>/dev/null | head -n 1); \
	if [ -z "$$file" ]; then \
		echo "❌ No .mochi file found in '$$dir'"; \
		exit 1; \
	fi; \
	base=$$(basename "$$file" .mochi); \
	outdir=../leetcode-out/$$dir; \
	out=$$outdir/$$base.go.out; \
	mkdir -p "$$outdir"; \
	echo "🔧 Compiling $$file to $$out..."; \
	if ! $(MOCHI_BIN) build "$$file" -o "$$out" --target go; then \
		echo "❌ Compilation failed."; \
		exit 1; \
	fi; \
	echo "🚀 Running Go program..."; \
	tmp=$$(mktemp /tmp/mochi-$$base-XXXX.go); \
	cp "$$out" "$$tmp"; \
	go run "$$tmp"; \
	rm "$$tmp"

test: mochi ## Run all tests in *.mochi files
	@find . -name '*.mochi' -print0 | xargs -0 -n1 $(MOCHI_BIN) test

compile: mochi ## Compile all .mochi files to Go and save to ../leetcode-out
	@mkdir -p ../leetcode-out
	@for f in $$(find $(shell seq -s " " 1 400) -name '*.mochi'); do \
		start=$$(date +%s%N); \
		dir=$$(dirname "$$f"); \
		rel=$${dir#./}; \
		mkdir -p ../leetcode-out/"$$rel"; \
		base=$$(basename "$$f" .mochi); \
		out=../leetcode-out/"$$rel"/"$$base.go.out"; \
		if ./bin/mochi build "$$f" -o "$$out" --target go > /dev/null 2>&1; then \
			end=$$(date +%s%N); \
			dur_ms=$$(( (end - start)/1000000 )); \
			printf "✅ Compiled \033[1m%s\033[0m → %s in %d ms\n" "$$f" "$$out" "$$dur_ms"; \
		else \
			echo "❌ Failed to compile: $$f" >&2; \
		fi; \
	done




clean: ## Remove Mochi binaries and build outputs
	@rm -rf bin
	@echo "🧹 Cleaned bin/"

help: ## Show help message
	@echo ""
	@echo "📦 \033[1mMochi LeetCode Makefile\033[0m"
	@echo "Usage: make [target] [ID=n] (optional)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'
