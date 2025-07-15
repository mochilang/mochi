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

	ftncode "mochi/compiler/x/fortran"
	"mochi/parser"
	"mochi/types"
)

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	outDir := filepath.Join("tests", "dataset", "tpc-ds", "compiler", "fortran")
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

	gfortran, err := ftncode.EnsureFortran()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join("tests", "dataset", "tpc-ds", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		errPath := filepath.Join(outDir, q+".error")
		codeOut := filepath.Join(outDir, q+".f90")
		outPath := filepath.Join(outDir, q+".out")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(codeOut)
			os.Remove(outPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			os.Remove(codeOut)
			os.Remove(outPath)
			continue
		}
		code, err := ftncode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(codeOut)
			os.Remove(outPath)
			continue
		}
		if err := os.WriteFile(codeOut, code, 0o644); err != nil {
			os.WriteFile(errPath, []byte("write code: "+err.Error()), 0o644)
			os.Remove(outPath)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".f90")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			os.WriteFile(errPath, []byte("tmp write: "+err.Error()), 0o644)
			os.Remove(outPath)
			continue
		}
		exe := filepath.Join(os.TempDir(), q)
		if out, err := exec.Command(gfortran, tmp, "-static", "-o", exe).CombinedOutput(); err != nil {
			msg := fmt.Sprintf("gfortran %s: %v\n%s\n", q, err, out)
			os.WriteFile(errPath, []byte(msg), 0o644)
			os.Remove(outPath)
			continue
		}
		cmd := exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			msg := fmt.Sprintf("run %s: %v\n%s\n", q, err, out)
			os.WriteFile(errPath, []byte(msg), 0o644)
			os.Remove(outPath)
			continue
		}
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(outPath, cleaned, 0o644); err != nil {
			os.WriteFile(errPath, []byte("write out: "+err.Error()), 0o644)
			os.Remove(outPath)
			continue
		}
		os.Remove(errPath)
	}
}
