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

	rustcode "mochi/compiler/x/rust"
	"mochi/parser"
	"mochi/types"
)

func writeError(dir, name string, msg string) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0o644)
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

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

	outDir := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rust")
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
		code, err := rustcode.New(env).Compile(prog)
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("compile error: %v", err))
			continue
		}
		codeOut := filepath.Join(outDir, q+".rs.out")
		if err := os.WriteFile(codeOut, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".rs")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		bin := filepath.Join(os.TempDir(), q)
		if out, err := exec.Command("rustc", tmp, "-O", "-o", bin).CombinedOutput(); err != nil {
			writeError(outDir, q, fmt.Sprintf("rustc: %v\n%s", err, out))
			continue
		}
		cmd := exec.Command(bin)
		var outBuf bytes.Buffer
		cmd.Stdout = &outBuf
		cmd.Stderr = &outBuf
		if err := cmd.Run(); err != nil {
			writeError(outDir, q, fmt.Sprintf("run: %v\n%s", err, outBuf.String()))
			continue
		}
		cleaned := append(bytes.TrimSpace(outBuf.Bytes()), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
		os.Remove(filepath.Join(outDir, q+".error"))
	}
}
