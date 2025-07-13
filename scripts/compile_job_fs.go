//go:build archive

package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"

	fscode "mochi/compiler/x/fs"
	"mochi/parser"
	"mochi/types"
)

func main() {
	outDir := filepath.Join("tests", "dataset", "job", "compiler", "fs")
	_ = os.RemoveAll(outDir)
	_ = os.MkdirAll(outDir, 0o755)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join("tests", "dataset", "job", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			panic(err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			panic(errs[0])
		}
		code, err := fscode.CompileFile(src)
		if err != nil {
			panic(err)
		}
		// Strip header (first line starting with //)
		if idx := bytes.IndexByte(code, '\n'); idx != -1 && bytes.HasPrefix(code, []byte("//")) {
			code = code[idx+1:]
		}
		outPath := filepath.Join(outDir, q+".fs")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			panic(err)
		}
		// Copy expected output from generic dataset outputs
		srcOut := filepath.Join("tests", "dataset", "job", "out", q+".out")
		dstOut := filepath.Join(outDir, q+".out")
		if f, err := os.Open(srcOut); err == nil {
			defer f.Close()
			data, err := io.ReadAll(f)
			if err != nil {
				panic(err)
			}
			if err := os.WriteFile(dstOut, data, 0o644); err != nil {
				panic(err)
			}
		}
	}
}
