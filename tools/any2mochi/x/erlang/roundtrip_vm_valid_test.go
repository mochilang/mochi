//go:build slow

package erlang

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	erlcode "mochi/compile/x/erlang"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func TestErlangRoundtripVMValid(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	tmp := t.TempDir()
	fake := filepath.Join(tmp, "erlfmt")
	if err := os.WriteFile(fake, []byte("#!/bin/sh\ncat"), 0755); err == nil {
		oldPath := os.Getenv("PATH")
		os.Setenv("PATH", tmp+":"+oldPath)
		t.Cleanup(func() { os.Setenv("PATH", oldPath) })
	}
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests/vm/valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	var results []string
	for _, src := range files {
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			prog, err := parser.ParseString(string(data))
			if err != nil {
				results = append(results, fmt.Sprintf("%s: parse error: %v", name, err))
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				results = append(results, fmt.Sprintf("%s: type error: %v", name, errs[0]))
				return
			}
			code, err := erlcode.New(env).Compile(prog)
			if err != nil {
				results = append(results, fmt.Sprintf("%s: compile error: %v", name, err))
				return
			}
			mochiSrc, err := Convert(string(code))
			if err != nil {
				results = append(results, fmt.Sprintf("%s: convert error: %v", name, err))
				return
			}
			prog2, err := parser.ParseString(string(mochiSrc))
			if err != nil {
				results = append(results, fmt.Sprintf("%s: parse2 error: %v", name, err))
				return
			}
			env2 := types.NewEnv(nil)
			if errs := types.Check(prog2, env2); len(errs) > 0 {
				results = append(results, fmt.Sprintf("%s: type2 error: %v", name, errs[0]))
				return
			}
			p2, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
			if err != nil {
				results = append(results, fmt.Sprintf("%s: vm compile error: %v", name, err))
				return
			}
			m := vm.New(p2, io.Discard)
			if err := m.Run(); err != nil {
				results = append(results, fmt.Sprintf("%s: vm run error: %v", name, err))
				return
			}
			results = append(results, fmt.Sprintf("%s: ok", name))
		})
	}
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/erl"), results)
}
