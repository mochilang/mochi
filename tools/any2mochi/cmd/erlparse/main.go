package main

import (
	"encoding/json"
	"io"
	"os"

	"mochi/tools/any2mochi"
)

func main() {
	var data []byte
	var err error
	if len(os.Args) > 1 {
		data, err = os.ReadFile(os.Args[1])
	} else {
		data, err = io.ReadAll(os.Stdin)
	}
	if err != nil {
		panic(err)
	}
	funcs := any2mochi.ParseErlangFuncs(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(funcs); err != nil {
		panic(err)
	}
}
