//go:build archive && slow

package ocaml

import (
	"fmt"
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
	mlcode "mochi/archived/x/ocaml"
	"mochi/parser"
	"mochi/types"
)

func compileMochiToOCaml(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := mlcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestOCaml_VM_RoundTrip(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToOCaml,
		ConvertFile,
		"ocaml",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/ocaml_vm"), status)
}
