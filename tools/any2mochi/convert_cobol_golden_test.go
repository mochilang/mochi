//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertCobol_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/cobol"), "*.cob.out", ConvertCobolFile, "cobol", ".mochi", ".error")
}
