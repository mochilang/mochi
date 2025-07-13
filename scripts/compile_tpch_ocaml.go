//go:build ignore

package main

import (
	"fmt"
	"os"
	"path/filepath"

	ocaml "mochi/compiler/x/ocaml"
	"mochi/parser"
	"mochi/types"
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
	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "ocaml")
	_ = os.MkdirAll(outDir, 0o755)
	for i := 1; i <= 5; i++ {
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
		code, err := ocaml.New(env).Compile(prog, src)
		if err != nil {
			panic(err)
		}
		outPath := filepath.Join(outDir, q+".ml")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			panic(err)
		}
	}
}
