package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: runvm FILE")
		os.Exit(1)
	}
	src := os.Args[1]
	if dir := filepath.Dir(src); dir != "." {
		if err := os.Chdir(dir); err != nil {
			fmt.Fprintln(os.Stderr, "chdir error:", err)
			os.Exit(1)
		}
		src = filepath.Base(src)
	}
	prog, err := parser.Parse(src)
	if err != nil {
		fmt.Fprintln(os.Stderr, "parse error:", err)
		os.Exit(1)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		fmt.Fprintln(os.Stderr, "type error:", errs[0])
		os.Exit(1)
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		fmt.Fprintln(os.Stderr, "compile error:", err)
		os.Exit(1)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		fmt.Fprintln(os.Stderr, "run error:", err)
		os.Exit(1)
	}
	fmt.Print(strings.TrimSpace(out.String()))
}
