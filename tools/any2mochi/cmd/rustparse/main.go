package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"

	"mochi/tools/any2mochi"
)

func main() {
	flag.Parse()
	var data []byte
	var err error
	if flag.NArg() == 1 {
		data, err = os.ReadFile(flag.Arg(0))
	} else {
		data, err = io.ReadAll(os.Stdin)
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	ast, err := any2mochi.ParseRustAST(string(data))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(ast)
}
