package main

import (
	"encoding/json"
	"io"
	"os"

	"mochi/tools/any2mochi"
)

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	prog := any2mochi.ParseDartToAST(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(prog); err != nil {
		panic(err)
	}
}
