.DEFAULT_GOAL := help

WASM := mochi.wasm

.PHONY: build wasm_exec.js serve clean test help

build: $(WASM) wasm_exec.js ## Build WebAssembly binary and copy wasm_exec.js

$(WASM): main.go
	@echo "🔧 Building WebAssembly binary..."
	tinygo build -o $(WASM) -target wasm ./
	@echo "✅ Generated $(WASM)"

wasm_exec.js:
	@echo "🔧 Copying wasm_exec.js..."
	cp $(shell go env GOROOT)/misc/wasm/wasm_exec.js .
	@echo "✅ Copied wasm_exec.js"

serve: build ## Serve playground at http://localhost:8080
	@echo "🌐 Starting local server on port 8080..."
	python3 -m http.server 8080

clean: ## Remove generated files
	@rm -f $(WASM) wasm_exec.js
	@echo "🧹 Cleaned build artifacts"

test: ## Run playground tests with the slow tag
	go test -tags slow ./...

help: ## Show this help message
	@echo ""
	@echo "Mochi Playground Makefile"
	@echo "-------------------------"
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\\033[36m%-15s\\033[0m %s\\n", $$1, $$2}'
