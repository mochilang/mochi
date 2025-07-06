//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertFortran_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/fortran"), "*.f90.out", ConvertFortranFile, "fortran", ".mochi", ".error")
}
