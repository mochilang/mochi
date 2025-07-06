//go:build slow

package clj

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertClj_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/clj"), "*.clj.out", ConvertFile, "clj", ".mochi", ".error")
}
