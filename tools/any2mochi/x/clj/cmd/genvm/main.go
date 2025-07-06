package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	cljcode "mochi/compile/x/clj"
	"mochi/parser"
	"mochi/runtime/vm"
	cljconv "mochi/tools/any2mochi/x/clj"
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
			} else if cljSrc, err := cljcode.New(env).Compile(prog); err != nil {
				errMsg = fmt.Sprintf("compile error: %v", err)
			} else {
				dir, _ := os.MkdirTemp("", "cljrt")
				cljFile := filepath.Join(dir, name+".clj")
				if wErr := os.WriteFile(cljFile, cljSrc, 0644); wErr != nil {
					errMsg = fmt.Sprintf("write error: %v", wErr)
				} else if mochiSrc, err := cljconv.ConvertFile(cljFile); err != nil {
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
	outDir := filepath.Join(root, "tests", "any2mochi", "clj")
	writeStatusMarkdown(outDir, status)
}

func writeStatusMarkdown(dir string, status map[string]string) {
	os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf strings.Builder
	buf.WriteString("# Errors\n\n")
	if len(status) == 0 {
		buf.WriteString("None\n")
	} else {
		names := make([]string, 0, len(status))
		for n := range status {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			if msg := status[n]; msg != "" {
				buf.WriteString("- " + n + ": " + msg + "\n")
			} else {
				buf.WriteString("- " + n + ": ok\n")
			}
		}
	}
	os.WriteFile(path, []byte(buf.String()), 0644)
}
