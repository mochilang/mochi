//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	ft "mochi/tools/any2mochi/x/fortran"
)

func TestConvertFortran_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/fortran"), "*.f90.out", ft.ConvertFile, "fortran", ".mochi", ".error")
}
