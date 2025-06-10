.PHONY: all

all: mochi.wasm

mochi.wasm: main.go
	@echo "🔧 Building WebAssembly binary..."
	tinygo build -o mochi.wasm -target wasm ./
	@echo "✅ Generated mochi.wasm"
