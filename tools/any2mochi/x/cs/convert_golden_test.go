//go:build slow

package cs

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertCs_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/cs"), "*.cs.out", ConvertFile, "cs", ".mochi", ".error")
}
