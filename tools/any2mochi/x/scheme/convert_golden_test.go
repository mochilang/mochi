//go:build slow

package scheme

import (
	"path/filepath"
	"testing"

	scheme "mochi/tools/any2mochi/x/scheme"
)

func TestConvertScheme_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", scheme.ConvertFile, "scheme", ".mochi", ".error")
}

func TestConvertSchemeCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/scheme"), "*.scm.out", scheme.ConvertFile, "scheme", ".mochi", ".error")
}
