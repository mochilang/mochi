//go:build slow

package rust

import (
	"path/filepath"
	"testing"

	rust "mochi/tools/any2mochi/x/rust"
)

func TestConvertRust_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/rust"), "*.rs.out", rust.ConvertFile, "rust", ".mochi", ".error")
}
