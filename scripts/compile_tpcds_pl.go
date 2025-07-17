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

	pl "mochi/compiler/x/pl"
	"mochi/parser"
	"mochi/types"
)

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	return dir
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	root := repoRoot()
	outDir := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "pl")
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

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		errPath := filepath.Join(outDir, q+".error")
		codePath := filepath.Join(outDir, q+".pl")
		outPath := filepath.Join(outDir, q+".out")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			os.Remove(codePath)
			os.Remove(outPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			os.Remove(codePath)
			os.Remove(outPath)
			continue
		}
		code, err := pl.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte("compile: "+err.Error()), 0o644)
			os.Remove(codePath)
			os.Remove(outPath)
			continue
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".pl")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		cmd := exec.Command("swipl", "-q", "-s", tmp, "-t", "halt")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			os.Remove(outPath)
			continue
		}
		os.Remove(errPath)
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(outPath, cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
