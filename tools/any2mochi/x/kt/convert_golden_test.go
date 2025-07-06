//go:build slow

package kt

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertKt_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/kt"), "*.kt.out", ConvertFile, "kt", ".mochi", ".error")
}
