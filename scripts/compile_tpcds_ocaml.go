//go:build archive

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	ocaml "mochi/compiler/x/ocaml"
	"mochi/parser"
	"mochi/types"
)

func main() {
	root, _ := os.Getwd()
	for {
		if _, err := os.Stat(filepath.Join(root, "go.mod")); err == nil {
			break
		}
		parent := filepath.Dir(root)
		if parent == root {
			panic("go.mod not found")
		}
		root = parent
	}

	queries := []int{}
	if env := os.Getenv("QUERIES"); env != "" {
		for _, part := range strings.Split(env, ",") {
			if n, err := strconv.Atoi(strings.TrimSpace(part)); err == nil {
				queries = append(queries, n)
			}
		}
	} else {
		for i := 1; i <= 99; i++ {
			queries = append(queries, i)
		}
	}

	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	outDir := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ocaml")
	_ = os.MkdirAll(outDir, 0o755)
	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		prog, err := parser.Parse(src)
		if err != nil {
			fmt.Fprintln(os.Stderr, "parse", q, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintln(os.Stderr, "type", q, errs[0])
			continue
		}
		code, err := ocaml.New(env).Compile(prog, "")
		if err != nil {
			fmt.Fprintln(os.Stderr, "compile", q, err)
			continue
		}
		outPath := filepath.Join(outDir, q+".ml")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		// run compiled code to capture output
		tmp := filepath.Join(os.TempDir(), q+".ml")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		exe := filepath.Join(os.TempDir(), q)
		if out, err := exec.Command("ocamlc", tmp, "-o", exe).CombinedOutput(); err != nil {
			fmt.Fprintf(os.Stderr, "ocamlc %s: %v\n%s\n", q, err, out)
			continue
		}
		out, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			fmt.Fprintf(os.Stderr, "run %s: %v\n%s\n", q, err, out)
		}
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
