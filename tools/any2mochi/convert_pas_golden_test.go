//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertPas_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/pas"), "*.pas.out", ConvertPasFile, "pas", ".mochi", ".error")
}
