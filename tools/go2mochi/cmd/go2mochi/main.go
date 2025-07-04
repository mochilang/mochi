package main

import (
	"flag"
	"fmt"
	"os"

	"mochi/tools/go2mochi"
)

func main() {
	flag.Parse()
	if flag.NArg() != 1 {
		fmt.Fprintln(os.Stderr, "usage: go2mochi <file.go>")
		os.Exit(1)
	}
	code, err := go2mochi.Convert(flag.Arg(0))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	os.Stdout.Write(code)
}
