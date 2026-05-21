package cgen

import (
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// TestEmitConstReturn covers the load-bearing happy path: a function
// that produces an i64 constant and returns it. The Phase 4.0 gate
// "binary runs and prints the same stdout as `mochi run`" depends on
// this lowering being byte-exact.
func TestEmitConstReturn(t *testing.T) {
	fn := &ir.Function{Name: "answer", Result: ir.TypeI64}
	bid := fn.AddBlock()
	c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 42})
	blk := fn.Block(bid)
	blk.Values = []uint32{c}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: c}

	src, err := Emit(&Program{Funcs: []*ir.Function{fn}, Main: "answer"})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	s := string(src)
	mustContain(t, s, "#include <stdint.h>")
	mustContain(t, s, "static int64_t answer(void)")
	mustContain(t, s, "v0 = (int64_t)42LL;")
	mustContain(t, s, "return v0;")
	mustContain(t, s, "int main(void)")
	mustContain(t, s, "printf(\"%lld\\n\", (long long)_r);")
}

// TestEmitFibIter walks the shared fib_iter fixture (the same IR the
// Go emitter uses) so a regression in phi lowering, RPO walk, or
// terminator emission surfaces against a non-trivial shape. The body
// must contain four blocks, three labels, both branches of the loop,
// and the back-edge phi assignments.
func TestEmitFibIter(t *testing.T) {
	src, err := Emit(&Program{Funcs: []*ir.Function{ir.FixtureFibIter()}})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	s := string(src)
	mustContain(t, s, "static int64_t fib_iter(int64_t v0)")
	// Phi assignment on the back-edge from body to head: a <- b, b <- t.
	if !strings.Contains(s, "v4 = ") || !strings.Contains(s, "v5 = ") {
		t.Errorf("expected phi back-edge writes in body terminator; got:\n%s", s)
	}
	// All three non-entry block labels present.
	mustContain(t, s, "L1:")
	mustContain(t, s, "L2:")
	mustContain(t, s, "L3:")
}

// TestEmitImmOps walks the i64 immediate-form ops, which carry the
// constant in Value.Const rather than as a separate OpConst.
func TestEmitImmOps(t *testing.T) {
	fn := &ir.Function{Name: "imm", Result: ir.TypeI64}
	nID := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
	fn.Params = []uint32{nID}
	bid := fn.AddBlock()
	add := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpAddI64Imm, Args: []uint32{nID}, Const: 7})
	mul := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpMulI64Imm, Args: []uint32{add}, Const: 3})
	blk := fn.Block(bid)
	blk.Values = []uint32{add, mul}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: mul}

	src, err := Emit(&Program{Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	s := string(src)
	mustContain(t, s, "v1 = v0 + (int64_t)7LL;")
	mustContain(t, s, "v2 = v1 * (int64_t)3LL;")
}

// TestEmitCompare covers the six i64 comparison ops; the result is
// the canonical 0/1 int the bool ABI expects (see cType TypeBool).
func TestEmitCompare(t *testing.T) {
	for _, c := range []struct {
		op   ir.OpCode
		want string
	}{
		{ir.OpCmpEqI64, "v0 == v1"},
		{ir.OpCmpNeI64, "v0 != v1"},
		{ir.OpCmpLtI64, "v0 < v1"},
		{ir.OpCmpLeI64, "v0 <= v1"},
		{ir.OpCmpGtI64, "v0 > v1"},
		{ir.OpCmpGeI64, "v0 >= v1"},
	} {
		fn := &ir.Function{Name: "cmp", Result: ir.TypeBool}
		a := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
		b := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
		fn.Params = []uint32{a, b}
		bid := fn.AddBlock()
		r := fn.AddValue(ir.Value{Type: ir.TypeBool, Op: c.op, Args: []uint32{a, b}})
		blk := fn.Block(bid)
		blk.Values = []uint32{r}
		blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: r}
		src, err := Emit(&Program{Funcs: []*ir.Function{fn}})
		if err != nil {
			t.Fatalf("%s: Emit: %v", c.op, err)
		}
		if !strings.Contains(string(src), c.want) {
			t.Errorf("%s: missing %q in:\n%s", c.op, c.want, src)
		}
	}
}

// TestEmitF64Const covers the bit-cast lowering for f64 constants.
// The IR carries f64 consts as raw bit patterns; the emitter must
// not write them as decimal doubles (the round-trip would lose bits
// the SSA value was guaranteeing).
func TestEmitF64Const(t *testing.T) {
	fn := &ir.Function{Name: "pi", Result: ir.TypeF64}
	bid := fn.AddBlock()
	// 3.14159265358979323846 as raw bits.
	c := fn.AddValue(ir.Value{Type: ir.TypeF64, Op: ir.OpConst, Const: 0x400921FB54442D18})
	blk := fn.Block(bid)
	blk.Values = []uint32{c}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: c}
	src, err := Emit(&Program{Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	mustContain(t, string(src), "union { uint64_t u; double d; }")
}

// TestEmitMainF64 exercises the Main-result branch for f64 (printf
// %.17g) so the gate "matches `mochi run` stdout" has a defined
// shape for non-integer return values.
func TestEmitMainF64(t *testing.T) {
	fn := &ir.Function{Name: "ratio", Result: ir.TypeF64}
	bid := fn.AddBlock()
	c := fn.AddValue(ir.Value{Type: ir.TypeF64, Op: ir.OpConst, Const: 0x3FF0000000000000})
	blk := fn.Block(bid)
	blk.Values = []uint32{c}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: c}
	src, err := Emit(&Program{Funcs: []*ir.Function{fn}, Main: "ratio"})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	mustContain(t, string(src), "printf(\"%.17g\\n\", _r);")
}

// TestEmitUnsupportedOp ensures the emitter rejects Phase 4.1+ ops
// with ErrUnsupportedOp rather than silently emitting garbled C. The
// driver downstream treats this as a clean "AOT cannot lower this
// program" signal.
func TestEmitUnsupportedOp(t *testing.T) {
	fn := &ir.Function{Name: "bad", Result: ir.TypeI64}
	bid := fn.AddBlock()
	c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 0})
	// OpLenStr is out of Phase 4.0 scope.
	s := fn.AddValue(ir.Value{Type: ir.TypeStr, Op: ir.OpParam})
	fn.Params = []uint32{s}
	ln := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpLenStr, Args: []uint32{s}})
	blk := fn.Block(bid)
	blk.Values = []uint32{c, ln}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: ln}
	_, err := Emit(&Program{Funcs: []*ir.Function{fn}})
	if err == nil {
		t.Fatalf("expected ErrUnsupportedOp, got nil")
	}
	// Errors may wrap; substring match is enough.
	if !strings.Contains(err.Error(), "unsupported") {
		t.Errorf("expected unsupported-op error, got %v", err)
	}
}

// TestEmitMainNotFound covers the Main-name guard.
func TestEmitMainNotFound(t *testing.T) {
	fn := &ir.Function{Name: "foo", Result: ir.TypeI64}
	bid := fn.AddBlock()
	c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 0})
	blk := fn.Block(bid)
	blk.Values = []uint32{c}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: c}
	if _, err := Emit(&Program{Funcs: []*ir.Function{fn}, Main: "missing"}); err == nil {
		t.Errorf("expected error when Main names a missing function")
	}
}

// TestEmitConstI64Min covers the LLONG_MIN edge: writing it as a
// positive literal is undefined in C99 because the lexer parses the
// abs(value) before applying unary minus. The emitter must spell it
// as `-LLONG_MAX - 1`.
func TestEmitConstI64Min(t *testing.T) {
	fn := &ir.Function{Name: "min", Result: ir.TypeI64}
	bid := fn.AddBlock()
	c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: -1 << 63})
	blk := fn.Block(bid)
	blk.Values = []uint32{c}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: c}
	src, err := Emit(&Program{Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	mustContain(t, string(src), "-9223372036854775807LL - 1")
}

func mustContain(t *testing.T, s, sub string) {
	t.Helper()
	if !strings.Contains(s, sub) {
		t.Errorf("expected to contain %q\n--- src ---\n%s", sub, s)
	}
}
