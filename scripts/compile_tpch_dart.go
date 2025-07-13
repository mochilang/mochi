//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	dartcode "mochi/compiler/x/dart"
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

	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "dart")
	_ = os.MkdirAll(outDir, 0o755)

	for i := 1; i <= 22; i++ {
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
		code, err := dartcode.New(env).Compile(prog)
		if err != nil {
			panic(err)
		}
		outPath := filepath.Join(outDir, q+".dart")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			panic(err)
		}
		tmp := filepath.Join(os.TempDir(), q+".dart")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			panic(err)
		}
		if dartExe, err := exec.LookPath("dart"); err == nil {
			cmd := exec.Command(dartExe, tmp)
			out, err := cmd.CombinedOutput()
			if err != nil {
				panic(fmt.Errorf("run %s: %v\n%s", q, err, out))
			}
			cleaned := append(bytes.TrimSpace(out), '\n')
			if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
				panic(err)
			}
		}
		fmt.Println("generated", outPath)
	}
}
