//go:build slow

package prolog

import (
	"fmt"
	"path/filepath"
	"testing"

	plcode "mochi/archived/x/pl"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	"mochi/types"
)

// compileMochiToProlog compiles a Mochi file to Prolog source.
func compileMochiToProlog(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := plcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestPrologRoundTripVM(t *testing.T) {
	if err := plcode.EnsureSWIPL(); err != nil {
		t.Skipf("swipl not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToProlog,
		ConvertFile,
		"pl",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/pl_vm"), status)
}
