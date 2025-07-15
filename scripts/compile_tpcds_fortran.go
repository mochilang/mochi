package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	ftncode "mochi/compiler/x/fortran"
	"mochi/parser"
	"mochi/types"
)

func writeError(dir, name string, msg string) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0o644)
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	gfortran, err := ftncode.EnsureFortran()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

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

	outDir := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "fortran")
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
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		prog, err := parser.Parse(src)
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("parse error: %v", err))
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			writeError(outDir, q, fmt.Sprintf("type error: %v", errs[0]))
			continue
		}
		code, err := ftncode.New(env).Compile(prog)
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("compile error: %v", err))
			continue
		}
		codePath := filepath.Join(outDir, q+".f90.out")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".f90")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		exe := strings.TrimSuffix(tmp, ".f90")
		if out, err := exec.Command(gfortran, tmp, "-static", "-o", exe).CombinedOutput(); err != nil {
			writeError(outDir, q, fmt.Sprintf("gfortran: %v\n%s", err, out))
			continue
		}
		cmd := exec.Command(exe)
		if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(in)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("run: %v\n%s", err, out))
			continue
		}
		_ = os.Remove(filepath.Join(outDir, q+".error"))
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
