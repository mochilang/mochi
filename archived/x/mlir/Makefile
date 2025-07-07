MLIR?=hello.mlir
LL?=hello.ll
BIN?=hello

all: $(BIN)

$(MLIR): ../../examples/v0.1/hello.mochi
	mochi build --target mlir $< -o $(MLIR)

$(LL): $(MLIR)
	mlir-translate-19 --mlir-to-llvmir $(MLIR) > $(LL)

$(BIN): $(LL)
	clang-19 $(LL) -o $(BIN)

run: $(BIN)
	./$(BIN)

clean:
	rm -f $(MLIR) $(LL) $(BIN)

.PHONY: all run clean
