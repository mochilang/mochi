//go:build slow

package st

import (
	"path/filepath"
	"testing"

	st "mochi/tools/any2mochi/x/st"
)

func TestConvertSt_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/st"), "*.st.out", st.ConvertFile, "st", ".mochi", ".error")
}
