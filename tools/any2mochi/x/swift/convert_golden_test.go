//go:build slow

package swift

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	swiftcode "mochi/compile/x/swift"
	"mochi/tools/any2mochi/testutil"
)

func TestConvertSwift_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	_ = swiftcode.EnsureSwift()
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", ConvertFile, "swift", ".mochi", ".error")
}

func TestConvertSwiftCompile_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	_ = gocode.EnsureGopls()
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/swift"), "*.swift.out", ConvertFile, "swift", ".mochi", ".error")
}
