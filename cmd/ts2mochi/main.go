package main

import (
	"flag"
	"fmt"
	"os"

	"mochi/tools/ts2mochi"
)

func main() {
	flag.Parse()
	if flag.NArg() < 1 {
		fmt.Fprintln(os.Stderr, "usage: ts2mochi <file.ts>")
		os.Exit(1)
	}
	out, err := ts2mochi.ConvertFile(flag.Arg(0))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	os.Stdout.Write(out)
}
