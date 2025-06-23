package vm

import (
	"bytes"
	"testing"
)

func TestVM_AddAndPrint(t *testing.T) {
	code := []Instr{
		{Op: OpLoadConst, A: 0, Value: 1},
		{Op: OpLoadConst, A: 1, Value: 2},
		{Op: OpAdd, A: 2, B: 0, C: 1},
		{Op: OpPrint, A: 2},
		{Op: OpReturn},
	}
	vm := New(code, 3)
	var buf bytes.Buffer
	vm.SetWriter(&buf)
	if err := vm.Run(); err != nil {
		t.Fatalf("vm run failed: %v", err)
	}
	if buf.String() != "3\n" {
		t.Fatalf("unexpected output: %q", buf.String())
	}
}
