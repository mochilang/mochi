//go:build slow

package cpp

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertCpp_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/cpp"), "*.cpp.out", ConvertFile, "cpp", ".mochi", ".error")
}
