//go:build slow

package rust

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	rscode "mochi/compile/x/rust"
	"mochi/parser"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func compileMochiToRust(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rscode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestRust_VM_RoundTrip(t *testing.T) {
	if err := rscode.Ensure(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}
	if _, err := exec.LookPath("rust-analyzer"); err != nil {
		t.Skip("rust-analyzer not installed")
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToRust,
		ConvertFile,
		"rust",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/rust_vm"), status)
}
