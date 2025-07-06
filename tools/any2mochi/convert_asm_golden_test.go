//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertAsm_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/asm"), "*.s.out", ConvertAsmFile, "asm", ".mochi", ".error")
}
