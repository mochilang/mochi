//go:build archived && slow

package cppcode_test

import (
	"fmt"
	"path/filepath"
	"testing"

	cppcode "mochi/archived/x/cpp"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	cppconv "mochi/archived/tools/any2mochi/x/cpp"
	"mochi/types"
)

// compileMochiToCPP compiles a Mochi source file to C++ code.
func compileMochiToCPP(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := cppcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestCPP_VM_Roundtrip(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToCPP,
		cppconv.ConvertFile,
		"cpp",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "compile/x/cpp"), status)
}
