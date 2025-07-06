//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	cs "mochi/tools/any2mochi/x/cs"
)

func TestConvertCs_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/cs"), "*.cs.out", cs.ConvertFile, "cs", ".mochi", ".error")
}
