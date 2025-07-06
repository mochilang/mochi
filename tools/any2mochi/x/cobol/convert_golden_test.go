//go:build slow

package cobol

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertCobol_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/cobol"), "*.cob.out", ConvertFile, "cobol", ".mochi", ".error")
}
