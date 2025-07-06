//go:build slow

package ex

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	excode "mochi/compile/x/ex"
	"mochi/parser"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

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

func TestEx_VM_RoundTrip(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToEx,
		ConvertFileParsed,
		"ex",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/ex_vm"), status)
}
