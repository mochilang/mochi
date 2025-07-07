//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	zig "mochi/tools/any2mochi/x/zig"
)

func TestConvertZig_Golden(t *testing.T) {
	root := findRepoRoot(t)
	errs := runConvertRunGolden(t, filepath.Join(root, "tests/compiler/zig"), "*.zig.out", zig.ConvertFile, "zig", ".mochi", ".error")
	writeErrorsMarkdown(filepath.Join(root, "tests/any2mochi/zig"), errs)
}
