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

	erlang "mochi/compiler/x/erlang"
	"mochi/parser"
	"mochi/types"
)

func findRoot() string {
	dir, _ := os.Getwd()
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			panic("go.mod not found")
		}
		dir = parent
	}
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	root := findRoot()
	outDir := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "erlang")
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
		codePath := filepath.Join(outDir, q+".erl")
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
		code, err := erlang.New(src).Compile(prog)
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
		tmp := filepath.Join(os.TempDir(), q+".erl")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		outBytes, err := exec.Command("escript", tmp).CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), outBytes...), 0o644)
			os.Remove(outPath)
			continue
		}
		lines := bytes.Split(outBytes, []byte("\n"))
		var buf bytes.Buffer
		for _, line := range lines {
			if bytes.HasPrefix(line, []byte("/tmp/")) {
				continue
			}
			if len(line) > 0 {
				buf.Write(line)
				buf.WriteByte('\n')
			}
		}
		cleaned := buf.Bytes()
		if len(cleaned) == 0 {
			cleaned = []byte{'\n'}
		}
		if err := os.WriteFile(outPath, cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
		os.Remove(errPath)
	}
}
