//go:build slow

package cobol

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	cobolcode "mochi/compile/x/cobol"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func TestCobolRoundtripVMValid(t *testing.T) {
	os.Setenv("MOCHI_SKIP_COBFMT", "1")
	defer os.Unsetenv("MOCHI_SKIP_COBFMT")

	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests/vm/valid", "*.mochi")
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
		var errMsg string
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				errMsg = fmt.Sprintf("%s: parse error: %v", name, err)
				t.Log(errMsg)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				errMsg = fmt.Sprintf("%s: type error: %v", name, errs[0])
				t.Log(errMsg)
				return
			}
			var code []byte
			func() {
				defer func() {
					if r := recover(); r != nil {
						errMsg = fmt.Sprintf("%s: compile panic: %v", name, r)
					}
				}()
				var err error
				code, err = cobolcode.New(env).Compile(prog)
				if err != nil && errMsg == "" {
					errMsg = fmt.Sprintf("%s: compile error: %v", name, err)
				}
			}()
			if errMsg != "" {
				t.Log(errMsg)
				return
			}
			mochiSrc, err := Convert(string(code))
			if err != nil {
				errMsg = fmt.Sprintf("%s: convert error: %v", name, err)
				t.Log(errMsg)
				return
			}
			prog2, err := parser.ParseString(string(mochiSrc))
			if err != nil {
				errMsg = fmt.Sprintf("%s: parse2 error: %v", name, err)
				t.Log(errMsg)
				return
			}
			env2 := types.NewEnv(nil)
			if errs := types.Check(prog2, env2); len(errs) > 0 {
				errMsg = fmt.Sprintf("%s: type2 error: %v", name, errs[0])
				t.Log(errMsg)
				return
			}
			p, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
			if err != nil {
				errMsg = fmt.Sprintf("%s: vm compile error: %v", name, err)
				t.Log(errMsg)
				return
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if rErr := m.Run(); rErr != nil {
				if ve, ok := rErr.(*vm.VMError); ok {
					errMsg = fmt.Sprintf("%s: vm run error:\n%s", name, ve.Format(p))
				} else {
					errMsg = fmt.Sprintf("%s: vm run error: %v", name, rErr)
				}
				t.Log(errMsg)
				return
			}
		})
		status[name] = errMsg
	}

	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/cobol_vm"), status)
}
