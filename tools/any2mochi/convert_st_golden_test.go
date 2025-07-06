//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertSt_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/st"), "*.st.out", ConvertStFile, "st", ".mochi", ".error")
}
