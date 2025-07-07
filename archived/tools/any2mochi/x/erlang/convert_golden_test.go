//go:build slow

package erlang

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
)

func TestConvertErl_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/erl"), "*.erl.out", ConvertFile, "erl", ".mochi", ".error")
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/erlang"), errs)
}
