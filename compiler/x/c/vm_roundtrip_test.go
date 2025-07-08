//go:build archived && slow

package ccode_test

import (
	"fmt"
	"path/filepath"
	"testing"

	ccode "mochi/archived/x/c"
	"mochi/parser"
	any2mochi "mochi/archived/tools/any2mochi"
	cconv "mochi/archived/tools/any2mochi/x/c"
	"mochi/types"
)

// compileMochiToC compiles a Mochi source file to C code.
func compileMochiToC(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := ccode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestVMValid_Roundtrip(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToC,
		cconv.ConvertFile,
		"c",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "compile/x/c"), status)
}
