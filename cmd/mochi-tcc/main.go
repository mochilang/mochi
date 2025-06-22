package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	ccode "mochi/compile/c"
	"mochi/parser"
	"mochi/types"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: mochi-tcc <file.mochi> [output]")
		os.Exit(1)
	}
	input := os.Args[1]
	out := ""
	if len(os.Args) > 2 {
		out = os.Args[2]
	} else {
		out = strings.TrimSuffix(filepath.Base(input), filepath.Ext(input))
	}

	prog, err := parser.Parse(input)
	if err != nil {
		fmt.Fprintln(os.Stderr, "parse:", err)
		os.Exit(1)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		for _, e := range errs {
			fmt.Fprintln(os.Stderr, e)
		}
		os.Exit(1)
	}

	code, err := ccode.New(env).Compile(prog)
	if err != nil {
		fmt.Fprintln(os.Stderr, "compile:", err)
		os.Exit(1)
	}

	tmp := filepath.Join(os.TempDir(), out+".c")
	if err := os.WriteFile(tmp, code, 0644); err != nil {
		fmt.Fprintln(os.Stderr, "write temp:", err)
		os.Exit(1)
	}

	tcc := os.Getenv("TCC")
	if tcc == "" {
		tcc = "tcc"
	}
	cmd := exec.Command(tcc, tmp, "-o", out)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintln(os.Stderr, "tcc:", err)
		os.Exit(1)
	}
}
