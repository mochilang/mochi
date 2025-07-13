//go:build slow

package dart

import (
	"path/filepath"
	"testing"

	any2mochi "mochi/archived/tools/any2mochi"
)

func TestConvertDart_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	status := any2mochi.RunConvertRunStatus(t, filepath.Join(root, "tests/compiler/dart"), "*.dart", ConvertFile, "dart", ".mochi", ".error")
	any2mochi.WriteStatusMarkdown(filepath.Join(root, "tests/any2mochi/dart"), status)
}
