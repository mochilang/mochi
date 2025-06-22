.PHONY: all clean

BIN := go4

all: $(BIN)

$(BIN): go4.go
@echo "🔧 Building $(BIN)..."
go build -o $(BIN) go4.go
@echo "✅ Built $(BIN)"

clean:
rm -f $(BIN)
