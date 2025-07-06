//go:build slow

package swift

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	swiftcode "mochi/compile/x/swift"
	swift "mochi/tools/any2mochi/x/swift"
)

func TestConvertSwift_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = swiftcode.EnsureSwift()
	runConvertGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", swift.ConvertFile, "swift", ".mochi", ".error")
}

func TestConvertSwiftCompile_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = gocode.EnsureGopls()
	_ = swiftcode.EnsureSwift()
	runConvertCompileGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", swift.ConvertFile, "swift", ".mochi", ".error")
}
