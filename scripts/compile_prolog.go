//go:build ignore

package main

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	pl "mochi/compiler/x/pl"
	"mochi/parser"
)

func main() {
	pattern := filepath.Join("tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		panic(err)
	}
	outDir := filepath.Join("tests", "machine", "x", "pl")
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		prog, err := parser.Parse(src)
		if err != nil {
			ioutil.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(filepath.Join(outDir, name+".pl"))
			continue
		}
		code, err := pl.New().Compile(prog)
		if err != nil {
			ioutil.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(filepath.Join(outDir, name+".pl"))
			continue
		}
		ioutil.WriteFile(filepath.Join(outDir, name+".pl"), code, 0644)
		os.Remove(errPath)
	}
}
