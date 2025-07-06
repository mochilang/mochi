//go:build slow

package ex

import (
	"bytes"
	"fmt"
	"path/filepath"
	"testing"

	excode "mochi/compile/x/ex"
	"mochi/parser"
	"mochi/runtime/vm"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

// TestVMRoundTrip compiles VM golden programs to Elixir, converts back to Mochi
// and executes them with the runtime VM. Any failures are written to
// tests/any2mochi/ex/ERRORS.md.
func TestVMRoundTrip(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests/vm/valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	var errs []string
	for _, src := range files {
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			if e := roundTrip(src); e != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", name, e))
				t.Log(e)
			}
		})
	}

	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/ex"), errs)
}

func roundTrip(path string) error {
	prog, err := parser.Parse(path)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if ts := types.Check(prog, env); len(ts) > 0 {
		return fmt.Errorf("type error: %v", ts[0])
	}
	code, err := excode.New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}
	mochiSrc, err := ConvertParsed(string(code))
	if err != nil {
		return fmt.Errorf("convert error: %w", err)
	}
	prog2, err := parser.ParseString(string(mochiSrc))
	if err != nil {
		return fmt.Errorf("parse2 error: %w", err)
	}
	env2 := types.NewEnv(nil)
	if ts := types.Check(prog2, env2); len(ts) > 0 {
		return fmt.Errorf("type2 error: %v", ts[0])
	}
	p, err := vm.Compile(prog2, env2)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	m := vm.New(p, &bytes.Buffer{})
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	return nil
}
