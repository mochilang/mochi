package jit

import (
	"bytes"
	"testing"
)

func TestCompileSimple(t *testing.T) {
	expr := BinOp{
		Op:    "&&",
		Left:  BinOp{Op: "==", Left: IntLit{Val: 3}, Right: IntLit{Val: 3}},
		Right: BinOp{Op: ">", Left: IntLit{Val: 5}, Right: IntLit{Val: 2}},
	}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}

func TestCompileFloat(t *testing.T) {
	expr := FBinOp{Op: "+", Left: FloatLit{Val: 1.5}, Right: FloatLit{Val: 2.25}}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}

func TestCompileMixedFloat(t *testing.T) {
	expr := FBinOp{Op: "*", Left: IntLit{Val: 3}, Right: FloatLit{Val: 2.5}}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}

func TestCompileFloatNeg(t *testing.T) {
	expr := FUnOp{Op: "-", Expr: FloatLit{Val: 3.14}}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}

func TestCompileCast(t *testing.T) {
	expr := Cast{Expr: IntLit{Val: 5}, To: "float"}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}

	expr2 := Cast{Expr: FloatLit{Val: 2.5}, To: "int"}
	asm2 := New()
	expr2.compile(asm2)
	asm2.Ret()
	if len(asm2.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}

func TestCompileIfExpr(t *testing.T) {
	expr := IfExpr{
		Cond: BinOp{Op: "==", Left: IntLit{Val: 1}, Right: IntLit{Val: 1}},
		Then: IntLit{Val: 42},
		Else: IntLit{Val: 0},
	}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}

func TestCompileIfFloat(t *testing.T) {
	expr := IfExpr{
		Cond: BoolLit{Val: true},
		Then: FloatLit{Val: 1.5},
		Else: FloatLit{Val: 2.5},
	}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
}

func TestShortCircuitJumps(t *testing.T) {
	expr := BinOp{Op: "&&", Left: BoolLit{Val: false}, Right: BoolLit{Val: true}}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	code := asm.Code()
	if !bytes.Contains(code, []byte{0x0F, 0x84}) {
		t.Fatalf("expected jz instruction in && compilation")
	}

	expr2 := BinOp{Op: "||", Left: BoolLit{Val: true}, Right: BoolLit{Val: false}}
	asm2 := New()
	expr2.compile(asm2)
	asm2.Ret()
	code2 := asm2.Code()
	if !bytes.Contains(code2, []byte{0x0F, 0x85}) {
		t.Fatalf("expected jnz instruction in || compilation")
	}
}

func TestCompileInList(t *testing.T) {
	expr := BinOp{Op: "in", Left: IntLit{Val: 2}, Right: ListLit{Elems: []int64{1, 2, 3}}}
	fn, err := Compile(expr)
	if err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	if fn() != 1 {
		t.Fatalf("expected 1 from membership test")
	}

	expr2 := BinOp{Op: "in", Left: IntLit{Val: 4}, Right: ListLit{Elems: []int64{1, 2, 3}}}
	fn2, err := Compile(expr2)
	if err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	if fn2() != 0 {
		t.Fatalf("expected 0 for missing element")
	}
}

func TestCompileLen(t *testing.T) {
	expr := LenExpr{Expr: ListLit{Elems: []int64{1, 2, 3}}}
	asm := New()
	expr.compile(asm)
	asm.Ret()
	if len(asm.Code()) == 0 {
		t.Fatalf("expected code to be generated")
	}
	fn, err := Compile(expr)
	if err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	if fn() != 3 {
		t.Fatalf("expected len 3 got %d", fn())
	}
}

func TestCompileLenString(t *testing.T) {
	expr := LenExpr{Expr: StrLit{Val: "héllo"}}
	fn, err := Compile(expr)
	if err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	if fn() != 5 {
		t.Fatalf("expected len 5 got %d", fn())
	}
}

func TestCompileStringEquality(t *testing.T) {
	expr := BinOp{Op: "==", Left: StrLit{Val: "foo"}, Right: StrLit{Val: "foo"}}
	fn, err := Compile(expr)
	if err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	if fn() != 1 {
		t.Fatalf("expected equality to be true")
	}
}

func TestCompileInString(t *testing.T) {
	expr := BinOp{Op: "in", Left: StrLit{Val: "e"}, Right: StrLit{Val: "hello"}}
	fn, err := Compile(expr)
	if err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	if fn() != 1 {
		t.Fatalf("expected membership true")
	}

	expr2 := BinOp{Op: "in", Left: StrLit{Val: "x"}, Right: StrLit{Val: "hello"}}
	fn2, err := Compile(expr2)
	if err != nil {
		t.Fatalf("compile failed: %v", err)
	}
	if fn2() != 0 {
		t.Fatalf("expected membership false")
	}
}
