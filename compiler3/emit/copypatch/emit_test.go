package copypatch

import (
	"errors"
	"testing"

	"mochi/compiler3/ir"
)

// TestEmitterSupportedMatchesArch ensures Supported() and NewEmitter()
// agree on the host's stencil-table availability. A drift would let
// the runtime path-select copypatch on a host the emitter cannot
// compile for.
func TestEmitterSupportedMatchesArch(t *testing.T) {
	_, err := NewEmitter()
	if Supported() && err != nil {
		t.Errorf("Supported() true but NewEmitter() err = %v", err)
	}
	if !Supported() && err == nil {
		t.Errorf("Supported() false but NewEmitter() succeeded")
	}
}

// TestCompileConstReturn covers the simplest end-to-end path: a
// Function whose only block holds an OpConst followed by TermReturn.
// The emitted buffer must be the OpConst stencil's bytes followed by
// the ret stencil's bytes, with the imm64 patched in. The assertion
// is arch-portable: the expected length and reloc offset come from
// the host's own stencil table (amd64 places imm64 at offset 2 of a
// 10-byte stencil; arm64 places it at offset 8 of a 16-byte stencil).
func TestCompileConstReturn(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	const k int64 = 123456789
	fn := buildConstReturn(k)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, relocs, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	tab := archStencils()
	opConst := tab[ir.OpConst]
	ret := tab[ir.OpInvalid]
	if got, want := len(code), len(opConst.Bytes)+len(ret.Bytes); got != want {
		t.Errorf("len(code) = %d, want %d (opConst=%d + ret=%d)",
			got, want, len(opConst.Bytes), len(ret.Bytes))
	}
	if len(relocs) != 1 {
		t.Fatalf("len(relocs) = %d, want 1", len(relocs))
	}
	wantOffset := opConst.Relocs[0].Offset
	if relocs[0].Offset != wantOffset || relocs[0].Kind != RelocImm64 {
		t.Errorf("reloc = %+v, want offset=%d kind=imm64", relocs[0], wantOffset)
	}
	if int64(relocs[0].Addend) != k {
		t.Errorf("reloc addend = %d, want %d", relocs[0].Addend, k)
	}
}

// TestCompileAddChain covers the multi-op path: Const + Const + Add +
// Return. The emitter walks the block's Values, picks a stencil per
// op, and rebases each stencil's relocs to the function-buffer offset.
// Arch-portable: expected sizes and reloc offsets are computed from
// the host's stencil table.
func TestCompileAddChain(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	const a, b int64 = 100, 23
	fn := buildConstAddReturn(a, b)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, relocs, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	tab := archStencils()
	opConst := tab[ir.OpConst]
	opAdd := tab[ir.OpAddI64]
	ret := tab[ir.OpInvalid]
	wantLen := 2*len(opConst.Bytes) + len(opAdd.Bytes) + len(ret.Bytes)
	if got := len(code); got != wantLen {
		t.Errorf("len(code) = %d, want %d", got, wantLen)
	}
	if got, want := len(relocs), 2; got != want {
		t.Errorf("len(relocs) = %d, want %d", got, want)
	}
	wantOff0 := opConst.Relocs[0].Offset
	wantOff1 := uint32(len(opConst.Bytes)) + opConst.Relocs[0].Offset
	if relocs[0].Offset != wantOff0 {
		t.Errorf("relocs[0].Offset = %d, want %d", relocs[0].Offset, wantOff0)
	}
	if relocs[1].Offset != wantOff1 {
		t.Errorf("relocs[1].Offset = %d, want %d", relocs[1].Offset, wantOff1)
	}
	if int64(relocs[0].Addend) != a || int64(relocs[1].Addend) != b {
		t.Errorf("addends = (%d, %d), want (%d, %d)",
			relocs[0].Addend, relocs[1].Addend, a, b)
	}
}

// TestCompileRejectsPhi checks that OpPhi triggers ErrNoStencil so
// the caller falls back to vm3. Phase 2.3's cross-op register
// allocator unlocks phi support.
func TestCompileRejectsPhi(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	fn := &ir.Function{Name: "phi", Result: ir.TypeI64}
	bid := fn.AddBlock()
	v := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpPhi})
	fn.Block(bid).Values = append(fn.Block(bid).Values, v)
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: v}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile(phi) err = %v, want ErrNoStencil", err)
	}
}

// TestCompileRejectsDiv covers the OpDivI64 fallback. Division needs
// an overflow slow-path (Phase 2.5); until then it must fall back.
// On arm64 the placeholder table also lacks OpDivI64; the rejection
// path is identical on both archs.
func TestCompileRejectsDiv(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	fn := buildBinaryOp(ir.OpDivI64, 10, 2)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile(div) err = %v, want ErrNoStencil", err)
	}
}

// TestCompileNilFunction checks the nil-Function guard. The runtime
// path must not segfault on a misuse.
func TestCompileNilFunction(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	if _, _, err := e.Compile(nil); err == nil {
		t.Errorf("Compile(nil) = nil, want non-nil error")
	}
}

// TestCompileEmptyFunction checks the no-block guard. An IR Function
// with zero Blocks is a malformed input; the emitter rejects it as
// ErrNoStencil so the caller falls back rather than emitting an empty
// buffer the runtime would jump into.
func TestCompileEmptyFunction(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	fn := &ir.Function{Name: "empty", Result: ir.TypeI64}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile(empty) err = %v, want ErrNoStencil", err)
	}
}

// TestCompileBadTerminator checks the TermInvalid fallback. The IR
// validator should already have caught this, but the emitter must
// refuse defensively so the runtime never lands on an unterminated
// buffer.
func TestCompileBadTerminator(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	fn := &ir.Function{Name: "bad_term", Result: ir.TypeI64}
	bid := fn.AddBlock()
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermInvalid}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile(bad_term) err = %v, want ErrNoStencil", err)
	}
}

// TestCompileRejectsBranchOnUnsupportedArch ensures the emitter
// refuses to lower a TermJump or TermBranch on a GOARCH whose
// arch-specific stencil file reports `archSupportsBranches() ==
// false`. The amd64 file sets it true; the arm64 placeholder sets it
// false until Phase 2.1 ports the rel26 / cbz encodings. The test is
// a no-op skip on archs where archSupportsBranches() is true (the
// existing branch tests cover those paths).
func TestCompileRejectsBranchOnUnsupportedArch(t *testing.T) {
	if !Supported() {
		t.Skip("host has no stencil table")
	}
	if archSupportsBranches() {
		t.Skip("host supports inter-block branches; reject path is unreachable")
	}
	fn := &ir.Function{Name: "jump", Result: ir.TypeI64}
	b0 := fn.AddBlock()
	b1 := fn.AddBlock()
	v := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 1})
	fn.Block(b0).Values = append(fn.Block(b0).Values, v)
	fn.Block(b0).Term = ir.Terminator{Kind: ir.TermJump, Target: b1}
	fn.Block(b1).Term = ir.Terminator{Kind: ir.TermReturn, Value: v}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile(jump) err = %v, want ErrNoStencil", err)
	}
}

// TestIsImmediateOp enumerates the *Imm opcodes the appendStencil
// addend-patch path covers, plus a couple of negatives. A regression
// would silently drop the Value.Const literal into a zero Addend.
func TestIsImmediateOp(t *testing.T) {
	yes := []ir.OpCode{
		ir.OpAddI64Imm, ir.OpSubI64Imm, ir.OpMulI64Imm,
		ir.OpDivI64Imm, ir.OpModI64Imm,
		ir.OpCmpEqI64Imm, ir.OpCmpNeI64Imm,
		ir.OpCmpLtI64Imm, ir.OpCmpLeI64Imm,
		ir.OpCmpGtI64Imm, ir.OpCmpGeI64Imm,
	}
	for _, op := range yes {
		if !isImmediateOp(op) {
			t.Errorf("isImmediateOp(%s) = false, want true", op)
		}
	}
	no := []ir.OpCode{ir.OpConst, ir.OpAddI64, ir.OpCmpEqI64, ir.OpParam}
	for _, op := range no {
		if isImmediateOp(op) {
			t.Errorf("isImmediateOp(%s) = true, want false", op)
		}
	}
}

// buildConstReturn constructs an IR Function with a single block
// holding one OpConst whose Value.Const is k, terminated by
// TermReturn. Used by TestCompileConstReturn and TestCacheInstall.
func buildConstReturn(k int64) *ir.Function {
	fn := &ir.Function{
		Name:   "const_return",
		Result: ir.TypeI64,
	}
	bid := fn.AddBlock()
	v := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: k})
	fn.Block(bid).Values = append(fn.Block(bid).Values, v)
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: v}
	return fn
}

// buildConstAddReturn constructs an IR Function representing
//
//	r := a + b
//	return r
//
// where a and b are i64 literals. Used by TestCompileAddChain.
func buildConstAddReturn(a, b int64) *ir.Function {
	fn := &ir.Function{
		Name:   "const_add_return",
		Result: ir.TypeI64,
	}
	bid := fn.AddBlock()
	va := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: a})
	vb := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: b})
	vr := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpAddI64, Args: []uint32{va, vb}})
	fn.Block(bid).Values = append(fn.Block(bid).Values, va, vb, vr)
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: vr}
	return fn
}

// buildBinaryOp constructs a single-block Function that evaluates
// op(a, b) where a, b are i64 literals and op is one of the i64
// binary opcodes the host's stencil set covers (or doesn't, for
// rejection tests).
func buildBinaryOp(op ir.OpCode, a, b int64) *ir.Function {
	fn := &ir.Function{Name: op.String(), Result: ir.TypeI64}
	bid := fn.AddBlock()
	va := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: a})
	vb := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: b})
	vr := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: op, Args: []uint32{va, vb}})
	fn.Block(bid).Values = append(fn.Block(bid).Values, va, vb, vr)
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: vr}
	return fn
}

// buildImmOp constructs a single-block Function that evaluates
// op(a, imm) where a is an i64 literal feeding the left operand and
// imm is the *Imm right operand carried in Value.Const.
func buildImmOp(op ir.OpCode, a, imm int64) *ir.Function {
	fn := &ir.Function{Name: op.String(), Result: ir.TypeI64}
	bid := fn.AddBlock()
	va := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: a})
	vr := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: op, Args: []uint32{va}, Const: imm})
	fn.Block(bid).Values = append(fn.Block(bid).Values, va, vr)
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: vr}
	return fn
}
