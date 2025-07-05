//go:build slow

package any2mochi

import (
	"testing"

	pycode "mochi/compile/py"
)

func TestConvertPython(t *testing.T) {
	_ = pycode.EnsurePyright()
	requireBinary(t, "python3")
	src := "def add(x, y):\n    return x + y"
	out, err := ConvertPython(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	want := "fun add(x,y): any {\n  return x + y\n}\n\n"
	if string(out) != want {
		t.Fatalf("unexpected output: %s", out)
	}
}
