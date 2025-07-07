//go:build slow

package hs

import (
	"fmt"
	"path/filepath"
	"testing"

	hscode "mochi/archived/x/hs"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	"mochi/types"
)

func compileMochiToHS(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := hscode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestHS_VM_RoundTrip(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToHS,
		ConvertFile,
		"hs",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/hs_vm"), status)
}
