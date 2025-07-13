//go:build ignore

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	excode "mochi/compiler/x/ex"
	"mochi/parser"
	"mochi/types"
)

func main() {
	// Ensure deterministic timestamps in generated headers.
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

	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ex")
	_ = os.MkdirAll(outDir, 0o755)

	for i := 1; i <= 22; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			panic(err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			panic(errs[0])
		}
		code, err := excode.New(env).Compile(prog)
		if err != nil {
			panic(err)
		}
		codePath := filepath.Join(outDir, base+".ex")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			panic(err)
		}

		tmpdir := os.TempDir()
		file := filepath.Join(tmpdir, base+".exs")
		if err := os.WriteFile(file, code, 0o644); err != nil {
			panic(err)
		}
		cmd := exec.Command("elixir", file)
		cmd.Dir = root
		var outBuf bytes.Buffer
		cmd.Stdout = &outBuf
		cmd.Stderr = nil
		if err := cmd.Run(); err != nil {
			panic(fmt.Errorf("run %s: %v", base, err))
		}
		outPath := filepath.Join(outDir, base+".out")
		if err := os.WriteFile(outPath, bytes.TrimSpace(outBuf.Bytes()), 0o644); err != nil {
			panic(err)
		}
	}
}
