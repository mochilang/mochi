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
		fmt.Fprintln(os.Stderr, "usage: vmregs <file.mochi>")
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
		fmt.Printf("== %s ==\n", path)
		for i := range p.Funcs {
			fn := &p.Funcs[i]
			usage := vm.RegUsage(fn)
			life := vm.RegLifetimes(fn)
			graph := vm.InterferenceGraph(life)
			fmt.Printf("-- function %s (regs=%d) --\n", p.Funcs[i].Name, fn.NumRegs)
			fmt.Println("usage:", usage)
			fmt.Println("lifetimes:", life)
			fmt.Println("interference:")
			for r, row := range graph {
				fmt.Printf("r%d:", r)
				for _, edge := range row {
					if edge {
						fmt.Print(" #")
					} else {
						fmt.Print(" .")
					}
				}
				fmt.Println()
			}
			fmt.Println("diagram:")
			fmt.Print(vm.VisualizeRegUsage(fn))
		}
	}
}
