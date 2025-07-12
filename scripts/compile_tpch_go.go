package main

import (
	"mochi/compiler/x/go"
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
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		panic(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		panic(errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		panic(err)
	}
	outPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", "q1.go.out")
	os.WriteFile(outPath, code, 0644)
}
