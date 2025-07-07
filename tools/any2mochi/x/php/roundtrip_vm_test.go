//go:build slow

package php

import (
	"fmt"
	"path/filepath"
	"testing"

	phpcode "mochi/archived/x/php"
	"mochi/parser"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func compileMochiToPHP(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := phpcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestPHP_VM_RoundTrip(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToPHP,
		ConvertFile,
		"php",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/php_vm"), status)
}
