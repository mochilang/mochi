package jit

import "testing"

func TestCompileSimple(t *testing.T) {
	expr := BinOp{
		Op:    "+",
		Left:  IntLit{Val: 3},
		Right: BinOp{Op: "*", Left: IntLit{Val: 4}, Right: IntLit{Val: 5}},
	}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}
