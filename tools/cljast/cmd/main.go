package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"

	"mochi/tools/cljast"
)

func main() {
	var data []byte
	var err error
	if len(os.Args) > 1 && os.Args[1] != "-" {
		data, err = os.ReadFile(os.Args[1])
	} else {
		data, err = io.ReadAll(os.Stdin)
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	ast := cljast.Parse(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(ast); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
