package main

import (
	"flag"
	"fmt"
	"os"

	"mochi/tools/ts2mochi"
)

var jsonOut = flag.String("json", "", "write parsed AST to file")

func main() {
	flag.Parse()
	if flag.NArg() < 1 {
		fmt.Fprintln(os.Stderr, "usage: ts2mochi [--json ast.json] <file.ts>")
		os.Exit(1)
	}
	code, ast, err := ts2mochi.ConvertFileWithJSON(flag.Arg(0))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if *jsonOut != "" {
		if err := os.WriteFile(*jsonOut, ast, 0644); err != nil {
			fmt.Fprintln(os.Stderr, "write json:", err)
		}
	}
	os.Stdout.Write(code)
}
