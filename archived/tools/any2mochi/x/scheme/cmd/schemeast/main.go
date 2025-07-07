package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"

	"mochi/archived/tools/any2mochi/x/scheme"
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
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	items, err := scheme.ParseItems(string(data))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	enc := json.NewEncoder(os.Stdout)
	if err := enc.Encode(items); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
