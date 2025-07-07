//go:build slow

package kt

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	ktcode "mochi/archived/x/kt"
	"mochi/parser"
	any2mochi "mochi/tools/any2mochi"
	"mochi/types"
)

func compileMochiToKt(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := ktcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestKt_VM_RoundTrip(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Log("kotlinc not installed; using fallback parser")
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToKt,
		ConvertFile,
		"kt",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/kt"), status)
}
