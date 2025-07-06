//go:build slow

package zig_test

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
	zig "mochi/tools/any2mochi/x/zig"
)

func TestConvertZig_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/zig"), "*.zig.out", zig.ConvertFile, "zig", ".mochi", ".error")
}
