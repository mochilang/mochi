//go:build slow

package any2mochi

import "testing"

func TestConvertGo(t *testing.T) {
	src := "package foo\nfunc Add(x int, y int) int { return x + y }"
	out, err := ConvertGo(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun Add() {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
