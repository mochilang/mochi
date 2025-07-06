//go:build slow

package ts

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	tscode "mochi/compile/ts"
	"mochi/tools/any2mochi/testutil"
)

func TestConvert_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	_ = tscode.EnsureTSLanguageServer()
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
	testutil.RunConvertGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
}

func TestConvertCompile_Golden(t *testing.T) {
	root := testutil.FindRepoRoot(t)
	_ = gocode.EnsureGopls()
	_ = tscode.EnsureTSLanguageServer()
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
	testutil.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
}
