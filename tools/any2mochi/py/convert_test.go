//go:build slow

package py

import (
	"os/exec"
	"testing"

	pycode "mochi/archived/py"
)

func requireBinary(t *testing.T, name string) {
	t.Helper()
	if _, err := exec.LookPath(name); err != nil {
		t.Skipf("%s not found", name)
	}
}

func TestConvert(t *testing.T) {
	_ = pycode.EnsurePyright()
	requireBinary(t, Servers["python"].Command)
	src := "def add(x, y):\n    return x + y"
	out, err := Convert(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	expected := "fun add(x, y) {}\n"
	if string(out) != expected {
		t.Fatalf("unexpected output: %s", out)
	}
}
