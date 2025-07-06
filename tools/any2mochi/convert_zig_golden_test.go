//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertZig_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/zig"), "*.zig.out", ConvertZigFile, "zig", ".mochi", ".error")
}
