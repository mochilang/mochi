//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertRb_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertRbFile, "rb", ".mochi", ".error")
}

func TestConvertRbCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/rb"), "*.rb.out", ConvertRbFile, "rb", ".mochi", ".error")
}
