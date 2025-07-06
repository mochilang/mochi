package main

import (
	"fmt"
	"os"
	"path/filepath"

	pycode "mochi/compile/py"
	"mochi/parser"
	"mochi/types"
)

func main() {
	files, err := filepath.Glob("tests/compiler/py/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}
	for _, src := range files {
		prog, err := parser.Parse(src)
		if err != nil {
			fmt.Fprintln(os.Stderr, "parse", src, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintln(os.Stderr, "type", src, errs[0])
			continue
		}
		code, err := pycode.New(env).Compile(prog)
		if err != nil {
			fmt.Fprintln(os.Stderr, "compile", src, err)
			continue
		}
		outPath := src[:len(src)-len(".mochi")] + ".py.out"
		if err := os.WriteFile(outPath, code, 0644); err != nil {
			fmt.Fprintln(os.Stderr, "write", outPath, err)
		} else {
			fmt.Println("updated", outPath)
		}
	}
}
