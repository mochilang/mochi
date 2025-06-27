package main

import (
	"fmt"
	"os"

	"mochi/parser"
	vm "mochi/runtime/vm"
	"mochi/types"
)

func usage() {
	fmt.Fprintf(os.Stderr, "usage: regopt <file.mochi>\n")
	os.Exit(1)
}

func funcName(p *vm.Program, idx int) string {
	if idx < 0 || idx >= len(p.Funcs) {
		return fmt.Sprintf("%d", idx)
	}
	name := p.Funcs[idx].Name
	if name == "" {
		if idx == 0 {
			return "main"
		}
		return fmt.Sprintf("fn%d", idx)
	}
	return name
}

func main() {
	if len(os.Args) != 2 {
		usage()
	}
	file := os.Args[1]
	prog, err := parser.Parse(file)
	if err != nil {
		fmt.Fprintln(os.Stderr, "parse error:", err)
		os.Exit(1)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		fmt.Fprintln(os.Stderr, "type error:", errs[0])
		os.Exit(1)
	}
	p, errc := vm.Compile(prog, env)
	if errc != nil {
		fmt.Fprintln(os.Stderr, "compile error:", errc)
		os.Exit(1)
	}
	for idx, fn := range p.Funcs {
		name := funcName(p, idx)
		fmt.Printf("Function %s (regs=%d)\n", name, fn.NumRegs)
		fmt.Print(vm.VisualizeUsage(&fn))
		fmt.Println()
	}
}
