//go:build slow

package dart

import (
	"bytes"
	"fmt"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	dartcode "mochi/compile/x/dart"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func TestRoundtripVMValid(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	dir := filepath.Join(root, "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	var results []string
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		status := "ok"
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				status = fmt.Sprintf("parse error: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				status = fmt.Sprintf("type error: %v", errs[0])
				return
			}
			code, err := dartcode.New(env).Compile(prog)
			if err != nil {
				status = fmt.Sprintf("compile dart error: %v", err)
				return
			}
			mochiCode, err := Convert(string(code))
			if err != nil {
				status = fmt.Sprintf("convert error: %v", err)
				return
			}
			prog2, err := parser.ParseString(string(mochiCode))
			if err != nil {
				status = fmt.Sprintf("parse back error: %v", err)
				return
			}
			env2 := types.NewEnv(nil)
			if errs := types.Check(prog2, env2); len(errs) > 0 {
				status = fmt.Sprintf("type back error: %v", errs[0])
				return
			}
			p, err := vm.CompileWithSource(prog2, env2, string(mochiCode))
			if err != nil {
				status = fmt.Sprintf("vm compile error: %v", err)
				return
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					status = fmt.Sprintf("vm run error:\n%s", ve.Format(p))
				} else {
					status = fmt.Sprintf("vm run error: %v", err)
				}
				return
			}
		})
		results = append(results, fmt.Sprintf("%s: %s", name, status))
	}
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests", "any2mochi", "dart"), results)
}
