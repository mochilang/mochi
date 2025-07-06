//go:build slow

package pas

import (
	"path/filepath"
	"testing"
)

func TestConvertPas_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/pas"), "*.pas.out", ConvertFile, "pas", ".mochi", ".error")
}
