//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertScheme_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", ConvertSchemeFile, "scheme", ".mochi", ".error")
}

func TestConvertSchemeCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", ConvertSchemeFile, "scheme", ".mochi", ".error")
}
