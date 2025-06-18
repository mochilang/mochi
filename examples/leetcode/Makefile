.DEFAULT_GOAL := help
SHELL := /bin/bash

MOCHI_BIN := $(CURDIR)/bin/mochi
MOCHI_ROOT := $(abspath $(CURDIR)/../..)

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

.PHONY: mochi run run-go run-cpp run-cs test compile build-one build-all clean help

# ────────────────────────────────────────────────────────────────
# 💡 Shared shell function for compiling one .mochi file
define build_one_lang
	start=$$(date +%s%N); \
	dir=$$(dirname "$1"); \
	base=$$(basename "$1" .mochi); \
	out_dir=../leetcode-out/$2/$$dir; \
	out_file=$$out_dir/$$base.$2; \
	mkdir -p "$$out_dir"; \
	success=true; \
	if $(MOCHI_BIN) build "$1" -o "$$out_file" --target $2 > /dev/null 2>&1; then \
               case "$2" in \
                        go) go run "$$out_file" > /dev/null 2>&1 || success=false ;; \
                        py) python3 "$$out_file" > /dev/null 2>&1 || success=false ;; \
                        ts) deno run --allow-all "$$out_file" > /dev/null 2>&1 || success=false ;; \
                        cpp) \
                                bin="$$out_dir/$$base"; \
                                g++ "$$out_file" -std=c++17 -o "$$bin" > /dev/null 2>&1 && "$$bin" > /dev/null 2>&1 || success=false ;; \
                        cs) \
                                proj="$$out_dir/app"; \
                                mkdir -p "$$proj"; \
                                echo '<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>' > "$$proj/app.csproj"; \
                                mv "$$out_file" "$$proj/Program.cs"; \
                                dotnet run --project "$$proj" > /dev/null 2>&1 || success=false ;; \
               esac; \
	else \
		echo "❌ Compile failed: $1 → $2" >&2; \
		success=false; \
	fi; \
	end=$$(date +%s%N); \
	dur_ms=$$(( (end - start)/1000000 )); \
	if $$success; then \
		printf "✅ \033[1m%s\033[0m → $2 compiled & ran in %d ms\n" "$1" "$$dur_ms"; \
	else \
		printf "❌ \033[1m%s\033[0m → $2 failed\n" "$1"; \
	fi
endef

# ────────────────────────────────────────────────────────────────
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

run: mochi ## Run a .mochi file. Usage: make run ID=<problem>
	@if [ -z "$(ID)" ]; then echo "❌ Usage: make run ID=<problem>"; exit 1; fi
	@$(MOCHI_BIN) run $(ID)/*.mochi

run-go: mochi ## Compile and run Go version of problem. Usage: make run-go ID=<problem>
	@if [ -z "$(ID)" ]; then echo "❌ Usage: make run-go ID=<problem>"; exit 1; fi
	@file=$$(ls $(ID)/*.mochi | head -n 1); \
	base=$$(basename "$$file" .mochi); \
	out=../leetcode-out/$(ID)/$$base.go; \
	mkdir -p ../leetcode-out/$(ID); \
	echo "🔧 Compiling $$file to $$out..."; \
	if ! $(MOCHI_BIN) build "$$file" -o "$$out" --target go; then \
	echo "❌ Compilation failed."; exit 1; fi; \
	go run "$$out"

run-cpp: mochi ## Compile and run C++ version of problem. Usage: make run-cpp ID=<problem>
	@if [ -z "$(ID)" ]; then echo "❌ Usage: make run-cpp ID=<problem>"; exit 1; fi
	@file=$$(ls $(ID)/*.mochi | head -n 1); \
	base=$$(basename "$$file" .mochi); \
	out_dir=../leetcode-out/cpp/$(ID); \
	out=$$out_dir/$$base.cpp; \
	mkdir -p "$$out_dir"; \
	echo "🔧 Compiling $$file to $$out..."; \
	if ! $(MOCHI_BIN) build "$$file" -o "$$out" --target cpp; then \
	echo "❌ Compilation failed."; exit 1; fi; \
        g++ "$$out" -std=c++17 -o "$$out_dir/$$base"; \
        "$$out_dir/$$base"

run-cs: mochi ## Compile and run C# version of problem. Usage: make run-cs ID=<problem>
       @if [ -z "$(ID)" ]; then echo "❌ Usage: make run-cs ID=<problem>"; exit 1; fi
       @file=$$(ls $(ID)/*.mochi | head -n 1); \
       base=$$(basename "$$file" .mochi); \
       out_dir=../leetcode-out/cs/$(ID); \
       out=$$out_dir/Program.cs; \
       proj=$$out_dir/app; \
       mkdir -p "$$proj"; \
       echo "🔧 Compiling $$file to $$out..."; \
       if ! $(MOCHI_BIN) build "$$file" -o "$$out" --target cs; then \
       echo "❌ Compilation failed."; exit 1; fi; \
       echo '<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>' > "$$proj/app.csproj"; \
       mv "$$out" "$$proj/Program.cs"; \
       dotnet run --project "$$proj"


build-one: mochi ## Compile one .mochi to a language. Usage: make build-one ID=123 LANG=go|py|ts|cpp|cs
       @if [ -z "$(ID)" ] || [ -z "$(LANG)" ]; then \
       echo "❌ Usage: make build-one ID=123 LANG=go|py|ts|cpp|cs"; exit 1; fi
       @file=$$(ls $(ID)/*.mochi | head -n 1); \
       $(call build_one_lang,$$file,$(LANG))

build-all: mochi ## Compile all problems from FROM to TO in parallel. Usage: make build-all [FROM=1] [TO=400] [JOBS=6]
	@FROM=$${FROM:-1}; TO=$${TO:-400}; JOBS=$${JOBS:-6}; \
	pids=(); \
	sem() { \
		while [ "$${#pids[@]}" -ge $$JOBS ]; do \
			wait -n "${pids[@]}" 2>/dev/null || break; \
			new_pids=(); \
			for pid in "$${pids[@]}"; do \
				if kill -0 $$pid 2>/dev/null; then new_pids+=($$pid); fi; \
			done; \
			pids=($${new_pids[@]}); \
		done; \
	}; \
	for i in $$(seq $$FROM $$TO); do \
		if [ -d "$$i" ]; then \
			for file in $$(find "$$i" -name '*.mochi' 2>/dev/null); do \
for lang in go py ts cpp cs; do \
					( $(call build_one_lang,$$file,$$lang) ) & \
					pids+=($$!); \
					sem; \
				done; \
			done; \
		fi; \
	done; \
	for pid in "$${pids[@]}"; do wait $$pid; done



test: mochi ## Run all tests
	@find . -name '*.mochi' -print0 | xargs -0 -n1 $(MOCHI_BIN) test

compile: build-all ## (Alias) Compile all problems

clean: ## Remove binaries and build outputs
	@rm -rf bin ../leetcode-out
	@echo "🧹 Cleaned bin/ and ../leetcode-out/"

help: ## Show help message
	@echo ""
	@echo "📦 \033[1mMochi LeetCode Makefile\033[0m"
	@echo "Usage: make [target] [VAR=val]"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
