package main

import (
	"fmt"
	"os"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

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
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: vmreport <file.mochi>")
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
		src, err := os.ReadFile(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", path, err)
			continue
		}
		fmt.Printf("== %s ==\n", path)
		fmt.Print(p.Disassemble(string(src)))

		for i := range p.Funcs {
			fn := &p.Funcs[i]
			analysis := vm.Infer(fn)
			fmt.Printf("\n-- function %s (regs=%d) --\n", funcName(p, i), fn.NumRegs)
			for pc, ins := range fn.Code {
				tags := analysis.In[pc]
				var tagStr string
				if tags != nil {
					tagStr = fmt.Sprint(tags)
				}
				dead := ""
				if pc < len(analysis.Dead) && analysis.Dead[pc] {
					dead = "dead"
				}
				fmt.Printf("%4d: %-12s %v %s\n", pc, ins.Op, tagStr, dead)
			}
		}
	}
}
