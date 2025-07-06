//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertRkt_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/rkt"), "*.rkt.out", ConvertRktFile, "rkt", ".mochi", ".error")
}
