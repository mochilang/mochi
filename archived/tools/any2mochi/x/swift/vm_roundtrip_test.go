//go:build slow

package swift

import (
	"fmt"
	"path/filepath"
	"testing"

	swiftcode "mochi/archived/x/swift"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	"mochi/types"
)

func compileMochiToSwift(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := swiftcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestSwift_VM_RoundTrip(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToSwift,
		ConvertFile,
		"swift",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/swift_vm"), status)
}
