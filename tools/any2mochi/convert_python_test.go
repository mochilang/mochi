//go:build slow

package any2mochi

import (
	"testing"

	pycode "mochi/compile/py"
)

func TestConvertPython(t *testing.T) {
	_ = pycode.EnsurePyright()
	requireBinary(t, Servers["python"].Command)
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
