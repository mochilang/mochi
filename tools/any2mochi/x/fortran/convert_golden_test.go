//go:build slow

package fortran

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertFortran_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/fortran"), "*.f90.out", ConvertFile, "fortran", ".mochi", ".error")
}
