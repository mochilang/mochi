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

	pascode "mochi/compiler/x/pascal"
	"mochi/parser"
	"mochi/types"
)

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	fpc, err := pascode.EnsureFPC()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	outDir := filepath.Join("tests", "dataset", "tpc-ds", "compiler", "pas")
	_ = os.MkdirAll(outDir, 0o755)

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

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join("tests", "dataset", "tpc-ds", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(filepath.Join(outDir, q+".error"), []byte(err.Error()), 0o644)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(filepath.Join(outDir, q+".error"), []byte(errs[0].Error()), 0o644)
			continue
		}
		code, err := pascode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(filepath.Join(outDir, q+".error"), []byte(err.Error()), 0o644)
			continue
		}
		codePath := filepath.Join(outDir, q+".pas.out")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".pas")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		bin := filepath.Join(os.TempDir(), q)
		if out, err := exec.Command(fpc, tmp, "-o"+bin).CombinedOutput(); err != nil {
			os.WriteFile(filepath.Join(outDir, q+".error"), out, 0o644)
			fmt.Fprintf(os.Stderr, "fpc %s: %v\n", q, err)
			continue
		}
		out, err := exec.Command(bin).CombinedOutput()
		if err != nil {
			os.WriteFile(filepath.Join(outDir, q+".error"), out, 0o644)
			fmt.Fprintf(os.Stderr, "run %s: %v\n", q, err)
			continue
		}
		os.Remove(filepath.Join(outDir, q+".error"))
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
