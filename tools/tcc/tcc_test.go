//go:build tcc && libtcc

package tcc

import "testing"

func TestCompileAndRun(t *testing.T) {
	res, err := CompileAndRun(`
int square(int x) { return x * x; }
`)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	if res != 25 {
		t.Fatalf("unexpected result: %d", res)
	}
}
