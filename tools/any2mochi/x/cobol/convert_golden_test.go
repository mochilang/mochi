//go:build slow

package cobol

import (
	"path/filepath"
	"testing"

	cobol "mochi/tools/any2mochi/x/cobol"
)

func TestConvertCobol_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/cobol"), "*.cob.out", cobol.ConvertFile, "cobol", ".mochi", ".error")
}
