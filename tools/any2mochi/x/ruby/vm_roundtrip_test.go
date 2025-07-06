//go:build slow

package ruby

import (
	"fmt"
	"path/filepath"
	"testing"

	rbcode "mochi/compile/x/rb"
	"mochi/parser"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func compileMochiToRB(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestRuby_VM_RoundTrip(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToRB,
		ConvertFile,
		"rb",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/ruby_vm"), status)
}
