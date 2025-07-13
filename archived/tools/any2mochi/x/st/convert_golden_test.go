//go:build archive && slow

package st

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
)

func TestConvertSt_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/st"), "*.st.out", ConvertFile, "st", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/st"), errs)
}
