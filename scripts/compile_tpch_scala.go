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

	scalacode "mochi/compiler/x/scala"
	"mochi/parser"
	"mochi/types"
)

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

	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "scala")
	_ = os.MkdirAll(outDir, 0o755)

	queries := []int{}
	if env := os.Getenv("QUERIES"); env != "" {
		for _, part := range strings.Split(env, ",") {
			if n, err := strconv.Atoi(strings.TrimSpace(part)); err == nil {
				queries = append(queries, n)
			}
		}
	} else {
		for i := 1; i <= 8; i++ {
			queries = append(queries, i)
		}
	}

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
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
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			fmt.Fprintln(os.Stderr, "compile", q, err)
			continue
		}
		codePath := filepath.Join(outDir, q+".scala")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmpDir := os.TempDir()
		tmpFile := filepath.Join(tmpDir, q+".scala")
		if err := os.WriteFile(tmpFile, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		errPath := filepath.Join(outDir, q+".error")
		outPath := filepath.Join(outDir, q+".out")
		if out, err := exec.Command("scalac", "-d", tmpDir, tmpFile).CombinedOutput(); err != nil {
			os.WriteFile(errPath, out, 0o644)
			os.Remove(outPath)
			fmt.Fprintf(os.Stderr, "scalac %s: %v\n", q, err)
			continue
		}
		run := exec.Command("scala", "-classpath", tmpDir, q)
		var buf bytes.Buffer
		run.Stdout = &buf
		run.Stderr = &buf
		if err := run.Run(); err != nil {
			os.WriteFile(errPath, buf.Bytes(), 0o644)
			os.Remove(outPath)
			fmt.Fprintf(os.Stderr, "run %s: %v\n", q, err)
			continue
		}
		cleaned := append(bytes.TrimSpace(buf.Bytes()), '\n')
		if err := os.WriteFile(outPath, cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
			continue
		}
		os.Remove(errPath)
	}
}
