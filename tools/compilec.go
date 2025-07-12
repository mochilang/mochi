//go:build slow

package main

import (
	"fmt"
	"os"

	ccode "mochi/compiler/x/c"
	"mochi/parser"
	"mochi/types"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: compilec <file.mochi>")
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
	code, err := ccode.New(env).Compile(prog)
	if err != nil {
		panic(err)
	}
	os.Stdout.Write(code)
}
