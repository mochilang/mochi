package stackvm

import (
	"bytes"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
)

func TestStackVM_TwoSum(t *testing.T) {
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	p, err := Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	var out bytes.Buffer
	vm := New(p, &out)
	if err := vm.Run(); err != nil {
		t.Fatalf("run error: %v", err)
	}
	got := strings.TrimSpace(out.String())
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}
