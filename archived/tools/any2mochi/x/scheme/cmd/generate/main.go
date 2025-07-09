//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	scheme "mochi/archived/tools/any2mochi/x/scheme"
	"mochi/parser"
	vm "mochi/runtime/vm"
	"mochi/types"
)

func main() {
	files, _ := filepath.Glob("tests/compiler/scheme/*.scm.out")
	outDir := "tests/any2mochi/scheme"
	os.MkdirAll(outDir, 0755)
	var errs []string
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".scm.out")
		outPath := filepath.Join(outDir, name+".mochi")
		errPath := filepath.Join(outDir, name+".error")
		data, err := scheme.ConvertFile(src)
		if err == nil {
			prog, pErr := parser.ParseString(string(data))
			if pErr != nil {
				err = fmt.Errorf("parse error: %w", pErr)
			} else {
				env := types.NewEnv(nil)
				if tErrs := types.Check(prog, env); len(tErrs) > 0 {
					err = fmt.Errorf("type error: %v", tErrs[0])
				} else if p2, vErr := vm.CompileWithSource(prog, env, string(data)); vErr != nil {
					err = fmt.Errorf("vm compile error: %w", vErr)
				} else {
					var buf bytes.Buffer
					m := vm.New(p2, &buf)
					if rErr := m.Run(); rErr != nil {
						if ve, ok := rErr.(*vm.VMError); ok {
							err = fmt.Errorf("vm run error:\n%s", ve.Format(p2))
						} else {
							err = fmt.Errorf("vm run error: %v", rErr)
						}
					}
				}
			}
		}
		if err != nil {
			os.WriteFile(outPath, nil, 0644)
			os.WriteFile(errPath, []byte(err.Error()+"\n"), 0644)
			errs = append(errs, fmt.Sprintf("%s: %v", name, err))
		} else {
			os.WriteFile(outPath, data, 0644)
			os.Remove(errPath)
		}
	}
	f, _ := os.Create(filepath.Join(outDir, "ERRORS.md"))
	defer f.Close()
	f.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		f.WriteString("None\n")
	} else {
		for _, e := range errs {
			f.WriteString("- " + e + "\n")
		}
	}
}
