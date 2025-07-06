//go:build slow

package ts

import (
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	tscode "mochi/compile/ts"
	any2mochi "mochi/tools/any2mochi"
)

func TestConvert_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	_ = tscode.EnsureTSLanguageServer()
	errs := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
	errs2 := any2mochi.RunConvertRunGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
	errs = append(errs, errs2...)
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "tests/any2mochi/ts"), errs)
}

func TestConvertCompile_Golden(t *testing.T) {
	root := any2mochi.FindRepoRoot(t)
	_ = gocode.EnsureGopls()
	_ = tscode.EnsureTSLanguageServer()
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
	any2mochi.RunConvertCompileGolden(t, filepath.Join(root, "tests/compiler/ts_simple"), "*.ts.out", ConvertFile, "ts", ".mochi", ".error")
}
