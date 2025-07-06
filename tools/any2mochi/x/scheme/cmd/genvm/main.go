//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	schemecode "mochi/compile/x/scheme"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	scheme "mochi/tools/any2mochi/x/scheme"
	"mochi/types"
)

func main() {
	root, _ := filepath.Abs(".")
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		panic(err)
	}
	if len(files) == 0 {
		panic("no files")
	}
	status := make(map[string]string)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errMsg := ""
		prog, err := parser.Parse(src)
		if err != nil {
			errMsg = fmt.Sprintf("parse error: %v", err)
		} else {
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				errMsg = fmt.Sprintf("type error: %v", errs[0])
			} else if schemeSrc, err := schemecode.New(env).Compile(prog); err != nil {
				errMsg = fmt.Sprintf("compile error: %v", err)
			} else {
				dir, _ := os.MkdirTemp("", "schemeround")
				schemeFile := filepath.Join(dir, name+".scm")
				if wErr := os.WriteFile(schemeFile, schemeSrc, 0644); wErr != nil {
					errMsg = fmt.Sprintf("write error: %v", wErr)
				} else if mochiSrc, err := scheme.ConvertFile(schemeFile); err != nil {
					errMsg = fmt.Sprintf("convert error: %v", err)
				} else if prog2, err := parser.ParseString(string(mochiSrc)); err != nil {
					errMsg = fmt.Sprintf("parse2 error: %v", err)
				} else {
					env2 := types.NewEnv(nil)
					if errs := types.Check(prog2, env2); len(errs) > 0 {
						errMsg = fmt.Sprintf("type2 error: %v", errs[0])
					} else if p2, err := vm.CompileWithSource(prog2, env2, string(mochiSrc)); err != nil {
						errMsg = fmt.Sprintf("vm compile error: %v", err)
					} else {
						var buf bytes.Buffer
						m := vm.New(p2, &buf)
						if err := m.Run(); err != nil {
							if ve, ok := err.(*vm.VMError); ok {
								errMsg = fmt.Sprintf("vm run error:\n%s", ve.Format(p2))
							} else {
								errMsg = fmt.Sprintf("vm run error: %v", err)
							}
						}
					}
				}
			}
		}
		status[name] = errMsg
	}
	outDir := filepath.Join(root, "tests", "any2mochi", "scheme_vm")
	any2mochi.WriteStatusMarkdown(outDir, status)
}
