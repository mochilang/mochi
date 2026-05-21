//go:build arm64

package copypatch

import (
	"testing"

	"mochi/compiler3/ir"
)

// TestCompileConstReturnARM64 asserts the exact aarch64 byte layout
// for the OpConst + ret path on the Phase 1.1 placeholder table:
//
//	40 00 00 58    ldr x0, [pc, #8]
//	03 00 00 14    b   #12              (skip the literal pool)
//	8 bytes        literal pool entry   (RelocImm64 patch site)
//	C0 03 5F D6    ret
//
// Total = 20 bytes, with the imm64 reloc at offset 8.
//
// The portable `TestCompileConstReturn` in emit_test.go checks the same
// semantics via the host stencil table; this test pins the arm64-
// specific encoding so a regression in stencils_arm64.go fails loudly.
func TestCompileConstReturnARM64(t *testing.T) {
	// k fits in int32 so the truncated `Addend = int32(v.Const)` patch
	// path (shared with amd64 OpConst) round-trips cleanly. The wider
	// imm64 path lands in Phase 1.1's stencilgen pipeline once
	// SymImmI64 supersedes the SymOpRetTarget placeholder.
	const k int64 = 0x11223344
	fn := buildConstReturn(k)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, relocs, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	if got, want := len(code), 20; got != want {
		t.Fatalf("len(code) = %d, want %d", got, want)
	}
	// ldr x0, [pc, #8]  = 0x58000040 LE = 40 00 00 58.
	want := []byte{0x40, 0x00, 0x00, 0x58}
	for i, b := range want {
		if code[i] != b {
			t.Errorf("ldr[%d] = %#02x, want %#02x", i, code[i], b)
		}
	}
	// b #12  = 0x14000003 LE = 03 00 00 14.
	want = []byte{0x03, 0x00, 0x00, 0x14}
	for i, b := range want {
		if code[4+i] != b {
			t.Errorf("b[%d] = %#02x, want %#02x", i, code[4+i], b)
		}
	}
	// Literal pool slot at [8:16] is zero (Compile leaves it unpatched
	// until the cache.Install pass applies the reloc).
	for i := 8; i < 16; i++ {
		if code[i] != 0 {
			t.Errorf("literal[%d] = %#02x, want 0", i-8, code[i])
		}
	}
	// ret = 0xD65F03C0 LE = C0 03 5F D6.
	want = []byte{0xC0, 0x03, 0x5F, 0xD6}
	for i, b := range want {
		if code[16+i] != b {
			t.Errorf("ret[%d] = %#02x, want %#02x", i, code[16+i], b)
		}
	}
	// Reloc: imm64 at offset 8, addend carries the low 32 bits of k
	// (the int32 Addend field's truncation matches the amd64 OpConst
	// path; cache.Install's SymImmI64 lookup widens the patch back to
	// 64 bits in Phase 1.1's stencilgen pipeline).
	if len(relocs) != 1 {
		t.Fatalf("len(relocs) = %d, want 1", len(relocs))
	}
	if relocs[0].Offset != 8 || relocs[0].Kind != RelocImm64 {
		t.Errorf("reloc = %+v, want offset=8 kind=imm64", relocs[0])
	}
	if int64(relocs[0].Addend) != k {
		t.Errorf("reloc addend = %d, want %d", relocs[0].Addend, k)
	}
}

// TestCompileAddChainARM64 asserts the exact aarch64 byte layout for
// the two-constant + add + ret path on the Phase 1.1 placeholder:
//
//	OpConst (16 bytes) + OpConst (16 bytes) + add x0,x0,x1 (4 bytes)
//	+ ret (4 bytes) = 40 bytes total.
//
// add x0, x0, x1 = 0x8B010000 LE = 00 00 01 8B.
func TestCompileAddChainARM64(t *testing.T) {
	fn := buildConstAddReturn(100, 23)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, _, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	if got, want := len(code), 40; got != want {
		t.Fatalf("len(code) = %d, want %d", got, want)
	}
	// add x0, x0, x1 at offset 32.
	want := []byte{0x00, 0x00, 0x01, 0x8B}
	for i, b := range want {
		if code[32+i] != b {
			t.Errorf("add[%d] = %#02x, want %#02x", i, code[32+i], b)
		}
	}
	// ret at offset 36.
	want = []byte{0xC0, 0x03, 0x5F, 0xD6}
	for i, b := range want {
		if code[36+i] != b {
			t.Errorf("ret[%d] = %#02x, want %#02x", i, code[36+i], b)
		}
	}
}

// TestARM64RejectsSubMulNeg confirms that the Phase 1.1 placeholder
// table covers only the three opcodes (OpConst, OpAddI64, OpInvalid
// for ret). Every other arithmetic opcode the amd64 table supports
// must still report `ErrNoStencil` on arm64 until Phase 2.1 widens the
// table. A regression that silently added a wrong-ISA stencil would
// produce a buffer the runtime would jump into and trap on; this test
// guards against that.
func TestARM64RejectsSubMulNeg(t *testing.T) {
	cases := []ir.OpCode{
		ir.OpSubI64, ir.OpMulI64, ir.OpNegI64,
		ir.OpAddI64Imm, ir.OpSubI64Imm, ir.OpMulI64Imm,
		ir.OpCmpEqI64, ir.OpCmpNeI64, ir.OpCmpLtI64,
		ir.OpCmpLeI64, ir.OpCmpGtI64, ir.OpCmpGeI64,
		ir.OpCmpEqI64Imm, ir.OpCmpNeI64Imm, ir.OpCmpLtI64Imm,
		ir.OpCmpLeI64Imm, ir.OpCmpGtI64Imm, ir.OpCmpGeI64Imm,
	}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	for _, op := range cases {
		t.Run(op.String(), func(t *testing.T) {
			var fn *ir.Function
			if op == ir.OpNegI64 {
				fn = &ir.Function{Name: op.String(), Result: ir.TypeI64}
				bid := fn.AddBlock()
				va := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 5})
				vr := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: op, Args: []uint32{va}})
				fn.Block(bid).Values = append(fn.Block(bid).Values, va, vr)
				fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: vr}
			} else if isImmediateOp(op) {
				fn = buildImmOp(op, 5, 3)
			} else {
				fn = buildBinaryOp(op, 5, 3)
			}
			_, _, err := e.Compile(fn)
			if err == nil {
				t.Fatalf("Compile(%s) = nil, want ErrNoStencil", op)
			}
		})
	}
}
