.DEFAULT_GOAL := help
SHELL := /bin/bash

MOCHI_ROOT := $(abspath ../..)
RUNNER := $(MOCHI_ROOT)/cmd/leetcode-runner

.PHONY: run build range test clean help run-ex

run: ## Run a problem. Usage: make run ID=<n>
@if [ -z "$(ID)" ]; then echo "❌ Usage: make run ID=<n>"; exit 1; fi
@go run $(RUNNER) run $(ID)

run-ex: ## Build and run problem in Elixir. Usage: make run-ex ID=<n>
@if [ -z "$(ID)" ]; then echo "❌ Usage: make run-ex ID=<n>"; exit 1; fi
@go run $(RUNNER) build --id $(ID) --lang ex --run

build: ## Build and run one problem in language. Usage: make build ID=<n> LANG=go
@if [ -z "$(ID)" ]; then echo "❌ Usage: make build ID=<n> LANG=<lang>"; exit 1; fi
@go run $(RUNNER) build --id $(ID) $(if $(LANG),--lang $(LANG)) --run

range: ## Build problems in range. Usage: make range FROM=1 TO=100 LANG=go
@go run $(RUNNER) build --from $(FROM) --to $(TO) $(if $(LANG),--lang $(LANG)) --run

test: ## Run tests for all problems
@go run $(RUNNER) test

clean:
@rm -rf ../leetcode-out
@echo "🧹 Cleaned build outputs"

help:
@echo ""
@echo "📦 \033[1mMochi LeetCode Makefile\033[0m"
@echo "Usage: make [target] [VAR=val]"
@echo ""
@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-10s\033[0m %s\n", $$1, $$2}'

