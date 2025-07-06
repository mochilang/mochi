//go:build slow

package golang

import (
	"testing"

	gocode "mochi/compile/go"
)

func TestConvertGo(t *testing.T) {
	_ = gocode.EnsureGopls()
	requireBinary(t, "gopls")
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	out, err := ConvertGo(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun Add() {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
