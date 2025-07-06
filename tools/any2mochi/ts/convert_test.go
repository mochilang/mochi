//go:build slow

package ts

import (
	"os/exec"
	"testing"

	tscode "mochi/compile/ts"
	parent "mochi/tools/any2mochi"
)

func requireBinary(t *testing.T, name string) {
	t.Helper()
	if _, err := exec.LookPath(name); err != nil {
		t.Skipf("%s not found", name)
	}
}

func TestConvert(t *testing.T) {
	_ = tscode.EnsureTSLanguageServer()
	requireBinary(t, "typescript-language-server")
	src := "export function add(x: number, y: number): number { return x + y }"
	out, err := Convert(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun add(x: int, y: int): int {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
