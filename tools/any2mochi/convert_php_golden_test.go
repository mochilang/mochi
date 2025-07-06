//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertPhp_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/php"), "*.php.out", ConvertPhpFile, "php", ".mochi", ".error")
}
