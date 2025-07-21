package main

import (
	"fmt"
	"os"

	"mochi/parser"
	ocaml "mochi/transpiler/x/ocaml"
	"mochi/types"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Fprintf(os.Stderr, "usage: genocaml <input.mochi> <output.ml>\n")
		os.Exit(1)
	}
	src := os.Args[1]
	out := os.Args[2]
	prog, err := parser.Parse(src)
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
	code := ast.Emit()
	if err := os.WriteFile(out, code, 0644); err != nil {
		panic(err)
	}
}
