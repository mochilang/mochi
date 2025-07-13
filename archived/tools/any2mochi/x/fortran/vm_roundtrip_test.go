//go:build archive && slow

package fortran

import (
	"fmt"
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
	ftncode "mochi/archived/x/fortran"
	"mochi/parser"
	"mochi/types"
)

// compileMochiToFortran compiles Mochi source into Fortran code.
func compileMochiToFortran(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	code, err := ftncode.New().Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	return code, nil
}

func TestFortran_VM_RoundTrip(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunCompileConvertRunStatus(
		t,
		filepath.Join(root, "tests/vm/valid"),
		"*.mochi",
		compileMochiToFortran,
		ConvertFile,
		"fortran",
	)
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/fortran_vm"), status)
}
