//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertRust_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/rust"), "*.rs.out", ConvertRustFile, "rust", ".mochi", ".error")
}
