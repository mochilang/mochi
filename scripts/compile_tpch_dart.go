//go:build slow

package main

import (
	"fmt"
	dartcode "mochi/compiler/x/dart"
	"mochi/parser"
	"mochi/types"
	"os"
	"path/filepath"
)

func main() {
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
	q := "q1"
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
	if err := os.WriteFile(outPath, code, 0644); err != nil {
		panic(err)
	}
	fmt.Println("generated", outPath)
}
