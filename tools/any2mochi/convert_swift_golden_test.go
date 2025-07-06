//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

func TestConvertSwift_Golden(t *testing.T) {
	root := findRepoRoot(t)
	runConvertGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", ConvertSwiftFile, "swift", ".mochi", ".error")
}

func TestConvertSwiftCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)

	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", ConvertSwiftFile, "swift", ".mochi", ".error")
}
