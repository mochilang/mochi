//go:build slow

package any2mochi

import "testing"

func TestConvertPython_Golden(t *testing.T) {
	t.Skip("py2mochi translator required for full test suite")
}

func TestConvertPythonCompile_Golden(t *testing.T) {
	t.Skip("py2mochi translator required for full test suite")
}
