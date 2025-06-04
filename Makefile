# Default target
.DEFAULT_GOAL := help
.PHONY: bench

# Project metadata
APP_NAME := mochi
RUN_NAME := mochi-run
BIN_DIR  := $(HOME)/bin
VERSION  := $(shell cat VERSION)
GIT_COMMIT := $(shell git rev-parse --short HEAD)
BUILD_TIME := $(shell date -u +"%Y-%m-%dT%H:%M:%SZ")

# Source paths
MAIN_SRC := cmd/mochi/main.go
RUN_SRC  := cmd/mochi-run/main.go

# Build flags
GO       := go
GOFLAGS  := -trimpath -mod=readonly
LDFLAGS := "-s -w -X 'main.version=$(VERSION)' -X 'main.gitCommit=$(GIT_COMMIT)' -X 'main.buildTime=$(BUILD_TIME)'"

# --------------------------
# Build Targets
# --------------------------

build: build-mochi build-run ## Build all binaries

build-mochi:
	@mkdir -p $(BIN_DIR)
	@echo "🔧 Building $(APP_NAME)..."
	@$(GO) build $(GOFLAGS) -ldflags=$(LDFLAGS) -o $(BIN_DIR)/$(APP_NAME) $(MAIN_SRC)
	@echo "✅ Built: $(BIN_DIR)/$(APP_NAME) (v$(VERSION))"

build-run:
	@mkdir -p $(BIN_DIR)
	@echo "🔧 Building $(RUN_NAME)..."
	@$(GO) build $(GOFLAGS) -ldflags=$(LDFLAGS) -o $(BIN_DIR)/$(RUN_NAME) $(RUN_SRC)
	@echo "✅ Built: $(BIN_DIR)/$(RUN_NAME) (v$(VERSION))"

# --------------------------
# Testing and Golden Update
# --------------------------

test: ## Run tests. Usage: make test STAGE=parser/types/interpreter
ifdef STAGE
	@echo "🧪 Running tests for stage: $(STAGE)"
	@$(GO) test ./$(STAGE) --vet=off -v
else
	@echo "🧪 Running full test suite"
	@$(GO) test ./... --vet=off -v
endif

update-golden: ## Update golden files. Usage: make update-golden STAGE=...
ifdef STAGE
	@echo "✏️  Updating golden files for stage: $(STAGE)"
	@$(GO) test ./$(STAGE) -update --vet=off
else
	@echo "✏️  Updating all golden files"
	@$(GO) test ./... -update --vet=off
endif

bench: build-mochi ## Run Mochi benchmarks
	@echo "🏃 Running benchmarks..."
	@PATH="$(BIN_DIR):$$PATH" $(GO) run ./cmd/mochi-bench
# --------------------------
# Maintenance
# --------------------------

fmt: ## Format source files
	@echo "🧹 Formatting source..."
	@$(GO) fmt ./...

lint: ## Run static analysis
	@echo "🔎 Running static analysis..."
	@if command -v golangci-lint > /dev/null 2>&1; then \
		echo "📦 Using golangci-lint"; \
		golangci-lint run ./... --timeout=5m; \
	else \
		echo "⚠️  golangci-lint not found, falling back to go vet"; \
		$(GO) vet ./...; \
	fi

update: ## Update Go module dependencies
	@echo "⬆️  Updating dependencies..."
	@$(GO) get -u ./...
	@$(GO) mod tidy

clean: ## Clean built binaries
	@echo "🧽 Cleaning binaries..."
	@rm -f $(BIN_DIR)/$(APP_NAME) $(BIN_DIR)/$(RUN_NAME)
	@echo "🗑️  Removed binaries"

release-src: ## Sync source code to GitHub repo
	@rsync -avh --delete \
		--exclude=".git" \
		--exclude=".DS_Store" \
		--exclude="*/.DS_Store" \
		--exclude="dist/" \
		./ $(HOME)/github/mochilang/mochi/

pull-src: ## Pull latest source code from GitHub repo
	@rsync -avh --delete \
		--exclude=".git" \
		--exclude=".DS_Store" \
		--exclude="*/.DS_Store" \
		--exclude="dist/" \
		$(HOME)/github/mochilang/mochi/ ./

release: ## Release new version. Usage: make release VERSION=X.Y.Z
ifndef VERSION
	$(error ❌ VERSION not set. Usage: make release VERSION=X.Y.Z)
endif
	@echo "✏️  Preparing Mochi v$(VERSION)..."
	@echo "$(VERSION)" > VERSION
	@git add VERSION
	@git commit -m "release: prepare v$(VERSION)" || echo "⚠️  Nothing to commit"
	@git tag -f v$(VERSION)
	@git push origin v$(VERSION)

	@echo "🚀 Running GoReleaser (full release)..."
	@GITHUB_TOKEN=$${GITHUB_TOKEN} goreleaser release --clean
	@echo "✅ Release complete: v$(VERSION)"

snapshot: ## Dry-run snapshot build (no publish)
	@echo "🧪 Running GoReleaser snapshot..."
	@goreleaser release --snapshot --clean
	@echo "✅ Snapshot build complete"

help: ## Show help message
	@echo ""
	@echo "📦 Mochi Makefile"
	@echo "--------------------------"
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
