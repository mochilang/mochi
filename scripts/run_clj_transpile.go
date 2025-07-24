package main

import (
	"fmt"
	"os"

	"mochi/parser"
	cljt "mochi/transpiler/x/clj"
	"mochi/types"
)

func main() {
	if len(os.Args) != 3 {
		fmt.Fprintf(os.Stderr, "usage: run_clj_transpile <src> <dst>\n")
		os.Exit(1)
	}
	prog, err := parser.Parse(os.Args[1])
	if err != nil {
		panic(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		panic(errs[0])
	}
	ast, err := cljt.Transpile(prog, env)
	if err != nil {
		panic(err)
	}
	code := cljt.Format(cljt.EmitString(ast))
	os.WriteFile(os.Args[2], code, 0644)
}
