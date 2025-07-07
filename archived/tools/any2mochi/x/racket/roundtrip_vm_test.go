//go:build slow

package racket

import (
	"fmt"
	rktcode "mochi/archived/x/rkt"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	"mochi/types"
	"path/filepath"
	"testing"
)

func compileMochiToRacket(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rktcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestRacketRoundTripVM(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToRacket,
		ConvertFile,
		"rkt",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/rkt_vm"), status)
}
