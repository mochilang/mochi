//go:build slow

package golang

import (
	"testing"

	gocode "mochi/compile/go"
)

func TestConvert(t *testing.T) {
	_ = gocode.EnsureGopls()
	requireBinary(t, "gopls")
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	out, err := Convert(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun Add(x: int, y: int): int {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
