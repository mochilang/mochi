package main

import (
	"fmt"
	"os"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: mochi-vm <file.mochi>")
		os.Exit(1)
	}
	file := os.Args[1]
	prog, err := parser.Parse(file)
	if err != nil {
		fmt.Fprintln(os.Stderr, "parse:", err)
		os.Exit(1)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		fmt.Fprintln(os.Stderr, "type error:", errs[0])
		os.Exit(1)
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		fmt.Fprintln(os.Stderr, "compile:", err)
		os.Exit(1)
	}
	m := vm.New(p, os.Stdout)
	if err := m.Run(); err != nil {
		fmt.Fprintln(os.Stderr, "run:", err)
		os.Exit(1)
	}
}
