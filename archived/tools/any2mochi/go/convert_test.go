//go:build slow

package golang

import (
	"testing"

	gocode "mochi/archived/go"
)

func TestConvert(t *testing.T) {
	_ = gocode.EnsureGopls()
	requireBinary(t, "gopls")
	src := "package foo\nfunc Pair() (int, int) { return 1, 2 }"
	out, err := Convert(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun Pair(): (int, int) {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
