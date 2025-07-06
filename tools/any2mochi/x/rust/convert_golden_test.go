//go:build slow

package rust

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertRust_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/rust"), "*.rs.out", ConvertFile, "rust", ".mochi", ".error")
}
