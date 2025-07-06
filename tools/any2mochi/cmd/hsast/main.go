package main

import (
	"encoding/json"
	"io"
	"os"

	"mochi/tools/any2mochi"
)

func main() {
	data, _ := io.ReadAll(os.Stdin)
	items := any2mochi.ParseHsAST(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(items)
}
