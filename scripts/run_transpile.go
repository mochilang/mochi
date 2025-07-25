//go:build slow

package main

import (
	"fmt"
	"mochi/parser"
	hs "mochi/transpiler/x/hs"
	"mochi/types"
	"os"
)

func main() {
	if len(os.Args) != 3 {
		fmt.Fprintf(os.Stderr, "usage: run_transpile <src> <dst>\n")
		os.Exit(1)
	}
	prog, err := parser.Parse(os.Args[1])
	if err != nil {
		panic(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		fmt.Fprintln(os.Stderr, errs[0])
		panic(errs[0])
	}
	hprog, err := hs.Transpile(prog, env)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		panic(err)
	}
	code := hs.Emit(hprog, false)
	os.WriteFile(os.Args[2], code, 0644)
}
