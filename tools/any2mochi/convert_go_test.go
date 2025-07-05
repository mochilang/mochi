//go:build slow

package any2mochi

import (
	"testing"
)

func TestConvertGo(t *testing.T) {
	src := "package main\nfunc Add(x int, y int) int { return x + y }\nfunc main(){}"
	out, err := ConvertGo(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun Add(x: int, y: int): int {\n  return x + y\n}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
