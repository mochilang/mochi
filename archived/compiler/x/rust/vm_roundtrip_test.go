//go:build archived && slow

package rscode_test

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	rscode "mochi/archived/x/rust"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	rconv "mochi/archived/tools/any2mochi/x/rust"
	"mochi/types"
)

// compileMochiToRust compiles a Mochi file to Rust source code.
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

func TestVMValid_Roundtrip(t *testing.T) {
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
		rconv.ConvertFile,
		"rust",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "compile/x/rust"), status)
}
