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

	ccode "mochi/compiler/x/c"
	"mochi/parser"
	"mochi/types"
)

func main() {
	cc, err := ccode.EnsureCC()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	outDir := filepath.Join("tests", "dataset", "tpc-h", "compiler", "c")
	_ = os.MkdirAll(outDir, 0o755)

	queries := []int{}
	if env := os.Getenv("QUERIES"); env != "" {
		for _, part := range strings.Split(env, ",") {
			if n, err := strconv.Atoi(strings.TrimSpace(part)); err == nil {
				queries = append(queries, n)
			}
		}
	} else {
		for i := 1; i <= 22; i++ {
			queries = append(queries, i)
		}
	}

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join("tests", "dataset", "tpc-h", q+".mochi")
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
		code, err := ccode.New(env).Compile(prog)
		if err != nil {
			fmt.Fprintln(os.Stderr, "compile", q, err)
			os.WriteFile(filepath.Join(outDir, q+".error"), []byte(err.Error()+"\n"), 0o644)
			continue
		}
		code = ccode.FormatC(code)
		codePath := filepath.Join(outDir, q+".c")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".c")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		bin := strings.TrimSuffix(tmp, ".c")
		if out, err := exec.Command(cc, tmp, "-o", bin).CombinedOutput(); err != nil {
			os.WriteFile(filepath.Join(outDir, q+".error"), out, 0o644)
			fmt.Fprintf(os.Stderr, "cc %s: %v\n%s\n", q, err, out)
			continue
		}
		out, err := exec.Command(bin).CombinedOutput()
		if err != nil {
			os.WriteFile(filepath.Join(outDir, q+".error"), out, 0o644)
			fmt.Fprintf(os.Stderr, "run %s: %v\n%s\n", q, err, out)
			continue
		}
		os.Remove(filepath.Join(outDir, q+".error"))
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
