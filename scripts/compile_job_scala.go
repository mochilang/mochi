//go:build ignore

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

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

	outDir := filepath.Join(root, "tests", "dataset", "job", "compiler", "scala")
	_ = os.MkdirAll(outDir, 0o755)

	for i := 11; i <= 20; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			panic(err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			panic(errs[0])
		}
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			panic(err)
		}
		scalaPath := filepath.Join(outDir, q+".scala")
		if err := os.WriteFile(scalaPath, code, 0o644); err != nil {
			panic(err)
		}
		tmpDir := os.TempDir()
		tmpFile := filepath.Join(tmpDir, q+".scala")
		if err := os.WriteFile(tmpFile, code, 0o644); err != nil {
			panic(err)
		}
		cmd := exec.Command("scalac", tmpFile)
		cmd.Dir = tmpDir
		if out, err := cmd.CombinedOutput(); err != nil {
			panic(fmt.Errorf("scalac %s: %v\n%s", q, err, out))
		}
		run := exec.Command("scala", q)
		run.Dir = tmpDir
		var buf bytes.Buffer
		run.Stdout = &buf
		run.Stderr = &buf
		if err := run.Run(); err != nil {
			// capture output even if the program exits with failure (e.g., failed assertions)
			fmt.Fprintf(os.Stderr, "warning: run %s: %v\n", q, err)
		}
		cleaned := append(bytes.TrimSpace(buf.Bytes()), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			panic(err)
		}
	}
}
