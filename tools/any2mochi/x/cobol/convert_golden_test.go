//go:build slow

package cobol

import (
	"mochi/tools/any2mochi/testutil"
	"path/filepath"
	"testing"
)

func TestConvertCobol_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/cobol"), "*.cob.out", ConvertFile, "cobol", ".mochi", ".error")
}
