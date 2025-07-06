//go:build slow

package dart

import (
	"path/filepath"
	"testing"

	dart "mochi/tools/any2mochi/x/dart"
)

func TestConvertDart_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/dart"), "*.dart.out", dart.ConvertFile, "dart", ".mochi", ".error")
}
