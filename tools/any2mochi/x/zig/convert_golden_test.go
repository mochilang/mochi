//go:build slow

package zig

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertZig_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/zig"), "*.zig.out", ConvertFile, "zig", ".mochi", ".error")
}
