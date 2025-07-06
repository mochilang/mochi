//go:build slow

package fortran

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertFortran_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/fortran"), "*.f90.out", ConvertFile, "fortran", ".mochi", ".error")
}
