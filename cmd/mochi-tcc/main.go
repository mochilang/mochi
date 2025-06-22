//go:build tcc && libtcc

package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	ccode "mochi/compile/c"
	"mochi/parser"
	"mochi/tools/tcc"
	"mochi/types"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: mochi-tcc <file.mochi> [output]")
		os.Exit(1)
	}
	input := os.Args[1]
	out := ""
	if len(os.Args) > 2 {
		out = os.Args[2]
	} else {
		out = strings.TrimSuffix(filepath.Base(input), filepath.Ext(input))
	}

	prog, err := parser.Parse(input)
	if err != nil {
		fmt.Fprintln(os.Stderr, "parse:", err)
		os.Exit(1)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		for _, e := range errs {
			fmt.Fprintln(os.Stderr, e)
		}
		os.Exit(1)
	}

	code, err := ccode.New(env).Compile(prog)
	if err != nil {
		fmt.Fprintln(os.Stderr, "compile:", err)
		os.Exit(1)
	}

	if err := tcc.CompileToFile(string(code), out); err != nil {
		fmt.Fprintln(os.Stderr, "tcc:", err)
		os.Exit(1)
	}
}
