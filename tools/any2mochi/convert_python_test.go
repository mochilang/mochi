//go:build slow

package any2mochi

import (
	"testing"

	pycode "mochi/compile/py"
)

func TestConvertPython(t *testing.T) {
	_ = pycode.EnsurePyright()
	requireBinary(t, "python3")
	src := "def sum_even(n):\n    s = 0\n    while n > 0:\n        if n % 2 == 0:\n            s = s + n\n        n = n - 1\n    return s"
	out, err := ConvertPython(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	want := "fun sum_even(n): any {\n  let s = 0\n  while n > 0 {\n    if n % 2 == 0 {\n      let s = s + n\n    }\n    let n = n - 1\n  }\n  return s\n}\n\n"
	if string(out) != want {
		t.Fatalf("unexpected output: %s", out)
	}
}
