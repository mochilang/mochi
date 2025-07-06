//go:build slow

package any2mochi

import (
	"testing"

	gocode "mochi/compile/go"
	goconv "mochi/tools/any2mochi/go"
)

func TestConvertGo(t *testing.T) {
	_ = gocode.EnsureGopls()
	requireBinary(t, "gopls")
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	out, err := goconv.ConvertGo(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun Add() {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
