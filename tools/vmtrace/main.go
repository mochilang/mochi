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
		fmt.Fprintln(os.Stderr, "usage: vmtrace <file.mochi>")
		os.Exit(1)
	}

	for _, path := range os.Args[1:] {
		prog, err := parser.Parse(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", path, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintf(os.Stderr, "%s: type error: %v\n", path, errs[0])
			continue
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", path, err)
			continue
		}
		steps, err := vm.Trace(p)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: runtime error: %v\n", path, err)
			continue
		}
		for _, s := range steps {
			fmt.Printf("%s:%d\t%-12s %v\n", s.Func, s.PC, s.Instr.Op, s.Regs)
		}
	}
}
