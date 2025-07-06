//go:build slow

package st

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/tools/any2mochi"
)

func TestConvertSt_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/st"), "*.st.out", ConvertFile, "st", ".mochi", ".error")
}
