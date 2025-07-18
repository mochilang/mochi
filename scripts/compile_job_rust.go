//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	rustcode "mochi/compiler/x/rust"
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

	outDir := filepath.Join(root, "tests", "dataset", "job", "compiler", "rust")
	_ = os.MkdirAll(outDir, 0o755)

	var queries []int
	for i := 1; i <= 5; i++ {
		queries = append(queries, i)
	}
	for i := 10; i <= 20; i++ {
		queries = append(queries, i)
	}

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		prog, err := parser.Parse(src)
		if err != nil {
			panic(err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			panic(errs[0])
		}
		code, err := rustcode.New(env).Compile(prog)
		if err != nil {
			panic(err)
		}
		outPath := filepath.Join(outDir, q+".rust")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			panic(err)
		}
		tmp := filepath.Join(os.TempDir(), q+".rs")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			panic(err)
		}
		bin := filepath.Join(os.TempDir(), q)
		if out, err := exec.Command("rustc", tmp, "-O", "-o", bin).CombinedOutput(); err != nil {
			fmt.Fprintf(os.Stderr, "warning: rustc %s: %v\n%s", q, err, out)
			continue
		}
		cmd := exec.Command(bin)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		var outBuf bytes.Buffer
		cmd.Stdout = &outBuf
		if err := cmd.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "warning: run %s: %v\n", q, err)
			continue
		}
		cleaned := append(bytes.TrimSpace(outBuf.Bytes()), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			panic(err)
		}
	}
}
