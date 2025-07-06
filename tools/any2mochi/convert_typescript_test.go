//go:build slow

package any2mochi

import "testing"

func TestConvertTypeScript(t *testing.T) {
	requireBinary(t, "typescript-language-server")
	src := "export function add(x: number, y: number): number { return x + y }"
	out, err := ConvertTypeScript(src)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	if string(out) != "fun add(x: int, y: int): int {}\n" {
		t.Fatalf("unexpected output: %s", out)
	}
}
