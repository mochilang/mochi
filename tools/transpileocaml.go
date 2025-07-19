//go:build transpileocaml

package main

import (
	"fmt"
	"os"

	"mochi/parser"
	ocaml "mochi/transpiler/x/ocaml"
	"mochi/types"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: transpileocaml <file.mochi>")
		os.Exit(1)
	}
	filename := os.Args[1]
	prog, err := parser.Parse(filename)
	if err != nil {
		panic(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		panic(errs[0])
	}
	ast, err := ocaml.Transpile(prog, env)
	if err != nil {
		panic(err)
	}
	os.Stdout.Write(ast.Emit())
}
