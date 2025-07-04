package main

import (
	"fmt"
	"io"
	"os"

	"mochi/tools/sqlite2duck"
)

func usage() {
	fmt.Fprintln(os.Stderr, "usage: sqlite2duck <file.sql>|-")
	os.Exit(1)
}

func main() {
	if len(os.Args) != 2 {
		usage()
	}
	var data []byte
	var err error
	if os.Args[1] == "-" {
		data, err = io.ReadAll(os.Stdin)
	} else {
		data, err = os.ReadFile(os.Args[1])
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	out := sqlite2duck.Convert(string(data))
	fmt.Print(out)
}
