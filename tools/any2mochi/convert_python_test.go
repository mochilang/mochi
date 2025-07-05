//go:build slow

package any2mochi

import (
	"testing"

	pycode "mochi/compile/py"
)

func TestConvertPython(t *testing.T) {
	_ = pycode.EnsurePyright()
	requireBinary(t, "pyright-langserver")
	src := "def add(x, y):\n    return x + y"
	out, err := ConvertPython(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun add() {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
