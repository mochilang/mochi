//go:build archived && slow

package excode_test

import (
	"fmt"
	"path/filepath"
	"testing"

	excode "mochi/archived/x/ex"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	exconv "mochi/archived/tools/any2mochi/x/ex"
	"mochi/types"
)

// compileMochiToEx compiles a Mochi source file to Elixir code.
func compileMochiToEx(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := excode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestVMValid_Roundtrip(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToEx,
		exconv.ConvertFileParsed,
		"ex",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "compile/x/ex"), status)
}
