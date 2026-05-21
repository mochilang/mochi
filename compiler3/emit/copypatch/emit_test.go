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
// the ret stencil's bytes, with the imm64 patched in.
func TestCompileConstReturn(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	fn := buildConstReturn(123456789)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, relocs, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	// OpConst stencil = 10 bytes; ret stencil = 1 byte. Total 11.
	if got, want := len(code), 11; got != want {
		t.Errorf("len(code) = %d, want %d", got, want)
	}
	// One reloc, at offset 2 (the mov rax, imm64 patch site),
	// kind = imm64, addend = 123456789.
	if len(relocs) != 1 {
		t.Fatalf("len(relocs) = %d, want 1", len(relocs))
	}
	if relocs[0].Offset != 2 || relocs[0].Kind != RelocImm64 {
		t.Errorf("reloc = %+v, want offset=2 kind=imm64", relocs[0])
	}
	if relocs[0].Addend != 123456789 {
		t.Errorf("reloc addend = %d, want 123456789", relocs[0].Addend)
	}
}

// TestCompileAddChain covers the multi-op path: Const + Add + Return.
// The emitter walks the block's Values, picks a stencil per op, and
// rebases each stencil's relocs to the function-buffer offset.
func TestCompileAddChain(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	fn := buildConstAddReturn(100, 23)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, relocs, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	// Two OpConst (10 bytes each) + OpAddI64 (3 bytes) + ret (1) = 24.
	if got, want := len(code), 24; got != want {
		t.Errorf("len(code) = %d, want %d", got, want)
	}
	// Two relocs, one per OpConst, each at offset (idx * 10) + 2.
	if got, want := len(relocs), 2; got != want {
		t.Errorf("len(relocs) = %d, want %d", got, want)
	}
	if relocs[0].Offset != 2 {
		t.Errorf("relocs[0].Offset = %d, want 2", relocs[0].Offset)
	}
	if relocs[1].Offset != 12 {
		t.Errorf("relocs[1].Offset = %d, want 12", relocs[1].Offset)
	}
	if relocs[0].Addend != 100 || relocs[1].Addend != 23 {
		t.Errorf("addends = (%d, %d), want (100, 23)", relocs[0].Addend, relocs[1].Addend)
	}
}

// TestCompileNoStencil checks the fallback path: when the IR
// references an opcode the stencil table does not cover, Compile
// returns ErrNoStencil and the caller is expected to fall back to
// vm3 interpretation.
func TestCompileNoStencil(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	fn := &ir.Function{
		Name:   "unsupported",
		Result: ir.TypeI64,
	}
	bid := fn.AddBlock()
	// OpMulI64 has no stencil in the Phase 1 table.
	v := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpMulI64})
	fn.Block(bid).Values = append(fn.Block(bid).Values, v)
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: v}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile err = %v, want ErrNoStencil", err)
	}
}

// TestCompileNilFunction checks the nil-Function guard. The runtime
// path must not segfault on a misuse.
func TestCompileNilFunction(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	if _, _, err := e.Compile(nil); err == nil {
		t.Errorf("Compile(nil) = nil, want non-nil error")
	}
}

// TestCompileMultiBlock checks the Phase 1 one-block restriction. A
// Function with two blocks must reject as ErrNoStencil so the
// fallback path engages cleanly.
func TestCompileMultiBlock(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	fn := &ir.Function{Name: "twoblocks", Result: ir.TypeI64}
	fn.AddBlock()
	fn.AddBlock()
	fn.Blocks[0].Term = ir.Terminator{Kind: ir.TermJump, Target: 1}
	fn.Blocks[1].Term = ir.Terminator{Kind: ir.TermReturn}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile multi-block err = %v, want ErrNoStencil", err)
	}
}

// TestCompileMissingTerminator covers the TermReturn-only check. A
// function without TermReturn is rejected; Phase 1.4 lifts this
// restriction once branch stencils land.
func TestCompileMissingTerminator(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	fn := &ir.Function{Name: "nojump", Result: ir.TypeI64}
	bid := fn.AddBlock()
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermJump, Target: 0}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	_, _, err = e.Compile(fn)
	if !errors.Is(err, ErrNoStencil) {
		t.Errorf("Compile missing-Return err = %v, want ErrNoStencil", err)
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
