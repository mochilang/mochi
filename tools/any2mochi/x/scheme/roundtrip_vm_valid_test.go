//go:build slow

package scheme

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	schemecode "mochi/archived/x/scheme"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

// TestSchemeRoundtripVMValid compiles each valid VM test program to Scheme,
// converts the generated Scheme code back to Mochi, then runs it with the VM.
func TestSchemeRoundtripVMValid(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	status := make(map[string]string)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				status[name] = fmt.Sprintf("parse error: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				status[name] = fmt.Sprintf("type error: %v", errs[0])
				return
			}
			schemeSrc, err := schemecode.New(env).Compile(prog)
			if err != nil {
				status[name] = fmt.Sprintf("compile error: %v", err)
				return
			}
			dir := t.TempDir()
			schemeFile := filepath.Join(dir, name+".scm")
			if err := os.WriteFile(schemeFile, schemeSrc, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			mochiSrc, err := ConvertFile(schemeFile)
			if err != nil {
				status[name] = fmt.Sprintf("convert error: %v", err)
				return
			}
			prog2, err := parser.ParseString(string(mochiSrc))
			if err != nil {
				status[name] = fmt.Sprintf("parse2 error: %v", err)
				return
			}
			env2 := types.NewEnv(nil)
			if errs := types.Check(prog2, env2); len(errs) > 0 {
				status[name] = fmt.Sprintf("type2 error: %v", errs[0])
				return
			}
			p2, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
			if err != nil {
				status[name] = fmt.Sprintf("vm compile error: %v", err)
				return
			}
			var buf bytes.Buffer
			m := vm.New(p2, &buf)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					status[name] = fmt.Sprintf("vm run error:\n%s", ve.Format(p2))
				} else {
					status[name] = fmt.Sprintf("vm run error: %v", err)
				}
				return
			}
			status[name] = ""
		})
	}
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests", "any2mochi", "scheme_vm"), status)
}
