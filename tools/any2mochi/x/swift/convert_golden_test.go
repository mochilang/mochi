//go:build slow

package swift

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	swiftcode "mochi/compile/x/swift"
	any2mochi "mochi/tools/any2mochi"
)

func TestConvertSwift_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	_ = swiftcode.EnsureSwift()
	any2mochi.RunConvertGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", ConvertFile, "swift", ".mochi", ".error")
}

func TestConvertSwiftCompile_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	_ = gocode.EnsureGopls()
	_ = swiftcode.EnsureSwift()
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", ConvertFile, "swift", ".mochi", ".error")
}
