//go:build slow

package dart

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertDart_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/dart"), "*.dart.out", ConvertFile, "dart", ".mochi", ".error")
}
