//go:build ignore

package main

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	cobol "mochi/compiler/x/cobol"
	"mochi/parser"
	"mochi/types"
)

func main() {
	pattern := filepath.Join("tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		panic(err)
	}
	outDir := filepath.Join("tests", "machine", "x", "cobol")
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		prog, err := parser.Parse(src)
		if err != nil {
			ioutil.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(filepath.Join(outDir, name+".cob"))
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			ioutil.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(filepath.Join(outDir, name+".cob"))
			continue
		}
		code, err := cobol.New(env).Compile(prog)
		if err != nil {
			ioutil.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(filepath.Join(outDir, name+".cob"))
			continue
		}
		ioutil.WriteFile(filepath.Join(outDir, name+".cob"), code, 0644)
		// remove stale error file on success
		os.Remove(errPath)
	}
}
