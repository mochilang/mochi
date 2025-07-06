//go:build slow

package any2mochi

import "testing"

func TestConvertPython(t *testing.T) {
	src := "def add(x, y):\n    return x + y"
	out, err := ConvertPython(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	expected := "fun add(x, y) {}\n"
	if string(out) != expected {
		t.Fatalf("unexpected output: %s", out)
	}
}
