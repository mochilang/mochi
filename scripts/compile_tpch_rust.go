//go:build ignore

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

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

	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rust")
	_ = os.MkdirAll(outDir, 0o755)

	queries := []int{1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 13, 14, 15, 16, 18}
	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
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
		outPath := filepath.Join(outDir, q+".rs")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			panic(err)
		}
		tmp := filepath.Join(os.TempDir(), q+".rs")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			panic(err)
		}
		bin := filepath.Join(os.TempDir(), q)
		if out, err := exec.Command("rustc", tmp, "-O", "-o", bin).CombinedOutput(); err != nil {
			panic(fmt.Errorf("rustc %s: %v\n%s", q, err, out))
		}
		cmd := exec.Command(bin)
		var outBuf bytes.Buffer
		cmd.Stdout = &outBuf
		if err := cmd.Run(); err != nil {
			panic(fmt.Errorf("run %s: %v", q, err))
		}
		cleaned := append(bytes.TrimSpace(outBuf.Bytes()), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			panic(err)
		}
	}
}
