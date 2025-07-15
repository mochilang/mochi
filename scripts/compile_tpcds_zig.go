//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	zigcode "mochi/compiler/x/zig"
	"mochi/parser"
	"mochi/types"
)

func writeError(dir, name string, msg string) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0o644)
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	outDir := filepath.Join("tests", "dataset", "tpc-ds", "compiler", "zig")
	_ = os.MkdirAll(outDir, 0o755)

	var queries []int
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

	zigc, err := zigcode.EnsureZig()
	if err != nil {
		fmt.Fprintln(os.Stderr, "zig not installed:", err)
		return
	}

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join("tests", "dataset", "tpc-ds", q+".mochi")
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
		code, err := zigcode.New(env).Compile(prog)
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("compile error: %v", err))
			continue
		}
		codeOut := filepath.Join(outDir, q+".zig.out")
		if err := os.WriteFile(codeOut, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".zig")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		exe := filepath.Join(os.TempDir(), q)
		buildOut, err := exec.Command(zigc, "build-exe", tmp, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput()
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("zig build: %v\n%s", err, buildOut))
			continue
		}
		runOut, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("run: %v\n%s", err, runOut))
			continue
		}
		cleaned := append(bytes.TrimSpace(runOut), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
		os.Remove(filepath.Join(outDir, q+".error"))
	}
}
