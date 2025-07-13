//go:build ignore

package main

import (
	"bytes"
	"fmt"
	schemecode "mochi/compiler/x/scheme"
	"mochi/parser"
	"mochi/types"
	"os"
	"os/exec"
	"path/filepath"
)

func main() {
	root := "."
	schemePath, err := schemecode.EnsureScheme()
	if err != nil {
		fmt.Fprintf(os.Stderr, "chibi-scheme missing: %v\n", err)
		os.Exit(1)
	}
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
		codePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", q+".scm")
		outPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scheme", q+".out")
		prog, err := parser.Parse(src)
		if err != nil {
			fmt.Fprintf(os.Stderr, "parse %s: %v\n", q, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintf(os.Stderr, "type %s: %v\n", q, errs[0])
			continue
		}
		code, err := schemecode.New(env).Compile(prog)
		if err != nil {
			fmt.Fprintf(os.Stderr, "compile %s: %v\n", q, err)
			continue
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintf(os.Stderr, "write %s: %v\n", codePath, err)
			continue
		}
		cmd := exec.Command(schemePath, "-m", "chibi", codePath)
		cmd.Dir = root
		var out bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &out
		cmd.Run()
		os.WriteFile(outPath, bytes.TrimSpace(out.Bytes()), 0o644)
	}
}
