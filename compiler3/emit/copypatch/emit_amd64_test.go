//go:build amd64

package copypatch

import (
	"encoding/binary"
	"testing"

	"mochi/compiler3/ir"
)

// TestCompileBinaryArith covers OpSubI64 and OpMulI64 on amd64. Each
// variant is exercised end-to-end (two-constant feed + arith op + ret)
// and the byte sequence at the arith-op offset is asserted against the
// hand-written stencil table. This test is amd64-specific because it
// asserts on x86_64 ModR/M and REX prefix bytes; the arm64 placeholder
// keeps the same shape behind `TestCompileAddChain` in emit_test.go.
func TestCompileBinaryArith(t *testing.T) {
	cases := []struct {
		name   string
		op     ir.OpCode
		bytes  []byte
		opSize int
	}{
		{"sub", ir.OpSubI64, []byte{0x48, 0x29, 0xF8}, 3},
		{"mul", ir.OpMulI64, []byte{0x48, 0x0F, 0xAF, 0xC7}, 4},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			fn := buildBinaryOp(c.op, 11, 22)
			e, err := NewEmitter()
			if err != nil {
				t.Fatalf("NewEmitter: %v", err)
			}
			code, _, err := e.Compile(fn)
			if err != nil {
				t.Fatalf("Compile: %v", err)
			}
			// Two OpConst (10 bytes each) + arith + ret.
			wantLen := 20 + c.opSize + 1
			if got := len(code); got != wantLen {
				t.Fatalf("len(code) = %d, want %d", got, wantLen)
			}
			arith := code[20 : 20+c.opSize]
			for i, b := range c.bytes {
				if arith[i] != b {
					t.Errorf("arith[%d] = %#02x, want %#02x", i, arith[i], b)
				}
			}
		})
	}
}

// TestCompileNegI64 walks the unary OpNegI64 lowering on amd64. One
// OpConst feeds the neg, the result is returned. neg rax = 48 F7 D8.
// Arm64's analog (`neg x0, x0` = `CB 00 03 E0`) lands in Phase 2.1.
func TestCompileNegI64(t *testing.T) {
	fn := &ir.Function{Name: "neg", Result: ir.TypeI64}
	bid := fn.AddBlock()
	va := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 7})
	vr := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpNegI64, Args: []uint32{va}})
	fn.Block(bid).Values = append(fn.Block(bid).Values, va, vr)
	fn.Block(bid).Term = ir.Terminator{Kind: ir.TermReturn, Value: vr}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, _, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	// OpConst (10) + neg (3) + ret (1) = 14.
	if got, want := len(code), 14; got != want {
		t.Fatalf("len(code) = %d, want %d", got, want)
	}
	want := []byte{0x48, 0xF7, 0xD8}
	for i, b := range want {
		if code[10+i] != b {
			t.Errorf("code[%d] = %#02x, want %#02x", 10+i, code[10+i], b)
		}
	}
}

// TestCompileImmOps walks the three i64 arith-Imm stencils on amd64
// and checks the imm32 Addend lands in the reloc. The IR uses one
// OpConst feeding the *Imm op (whose Value.Const carries the
// immediate) and returns the result. Arm64's *Imm encoding (12-bit
// imm in `add x0, x0, #imm` plus the multi-instruction path for the
// general case) lands in Phase 2.1.
func TestCompileImmOps(t *testing.T) {
	cases := []struct {
		name   string
		op     ir.OpCode
		imm    int64
		opSize int
		// relocOffsetInOp names where in the *Imm stencil the imm32 sits.
		relocOffsetInOp uint32
	}{
		{"add_imm", ir.OpAddI64Imm, 99, 6, 2},
		{"sub_imm", ir.OpSubI64Imm, -7, 6, 2},
		{"mul_imm", ir.OpMulI64Imm, 3, 7, 3},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			fn := buildImmOp(c.op, 5, c.imm)
			e, err := NewEmitter()
			if err != nil {
				t.Fatalf("NewEmitter: %v", err)
			}
			code, relocs, err := e.Compile(fn)
			if err != nil {
				t.Fatalf("Compile: %v", err)
			}
			// OpConst (10) + *Imm (c.opSize) + ret (1).
			if got, want := len(code), 11+c.opSize; got != want {
				t.Fatalf("len(code) = %d, want %d", got, want)
			}
			// Two relocs: OpConst RelocImm64 at offset 2, *Imm RelocImm32
			// at offset 10 + relocOffsetInOp.
			if got, want := len(relocs), 2; got != want {
				t.Fatalf("len(relocs) = %d, want %d", got, want)
			}
			want := uint32(10) + c.relocOffsetInOp
			if relocs[1].Offset != want {
				t.Errorf("imm reloc offset = %d, want %d", relocs[1].Offset, want)
			}
			if relocs[1].Kind != RelocImm32 {
				t.Errorf("imm reloc kind = %s, want imm32", relocs[1].Kind)
			}
			if int64(relocs[1].Addend) != c.imm {
				t.Errorf("imm addend = %d, want %d", relocs[1].Addend, c.imm)
			}
		})
	}
}

// TestCompileCompare exercises the six i64 comparison lowerings on
// amd64. Each must emit cmp rax, rdi (48 39 F8) + setCC al (0F XX C0)
// + movzx rax, al (48 0F B6 C0), with the setCC opcode varying per
// relation. Arm64's analog (`cmp x0, x1` + `cset x0, COND`) lands in
// Phase 2.1.
func TestCompileCompare(t *testing.T) {
	cases := []struct {
		name      string
		op        ir.OpCode
		setOpcode byte
	}{
		{"eq", ir.OpCmpEqI64, 0x94},
		{"ne", ir.OpCmpNeI64, 0x95},
		{"lt", ir.OpCmpLtI64, 0x9C},
		{"le", ir.OpCmpLeI64, 0x9E},
		{"gt", ir.OpCmpGtI64, 0x9F},
		{"ge", ir.OpCmpGeI64, 0x9D},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			fn := buildBinaryOp(c.op, 1, 2)
			fn.Values[2].Type = ir.TypeBool
			e, err := NewEmitter()
			if err != nil {
				t.Fatalf("NewEmitter: %v", err)
			}
			code, _, err := e.Compile(fn)
			if err != nil {
				t.Fatalf("Compile: %v", err)
			}
			// Two OpConst (20) + cmp+setCC+movzx (10) + ret (1) = 31.
			if got, want := len(code), 31; got != want {
				t.Fatalf("len(code) = %d, want %d", got, want)
			}
			// cmp prefix at offset 20.
			if code[20] != 0x48 || code[21] != 0x39 || code[22] != 0xF8 {
				t.Errorf("cmp prefix = % x, want 48 39 F8", code[20:23])
			}
			// setCC opcode at offset 23+1.
			if code[23] != 0x0F || code[24] != c.setOpcode || code[25] != 0xC0 {
				t.Errorf("setCC = % x, want 0F %02x C0", code[23:26], c.setOpcode)
			}
			// movzx tail.
			if code[26] != 0x48 || code[27] != 0x0F || code[28] != 0xB6 || code[29] != 0xC0 {
				t.Errorf("movzx tail = % x, want 48 0F B6 C0", code[26:30])
			}
		})
	}
}

// TestCompileCompareImm exercises the six i64-imm comparison lowerings
// on amd64. The cmp prefix is 48 3D + imm32, the setCC opcode varies
// per relation, and the imm32 Addend is set from Value.Const.
func TestCompileCompareImm(t *testing.T) {
	cases := []struct {
		name      string
		op        ir.OpCode
		setOpcode byte
	}{
		{"eq", ir.OpCmpEqI64Imm, 0x94},
		{"ne", ir.OpCmpNeI64Imm, 0x95},
		{"lt", ir.OpCmpLtI64Imm, 0x9C},
		{"le", ir.OpCmpLeI64Imm, 0x9E},
		{"gt", ir.OpCmpGtI64Imm, 0x9F},
		{"ge", ir.OpCmpGeI64Imm, 0x9D},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			fn := buildImmOp(c.op, 42, 7)
			fn.Values[1].Type = ir.TypeBool
			e, err := NewEmitter()
			if err != nil {
				t.Fatalf("NewEmitter: %v", err)
			}
			code, relocs, err := e.Compile(fn)
			if err != nil {
				t.Fatalf("Compile: %v", err)
			}
			// OpConst (10) + cmp+setCC+movzx (13) + ret (1) = 24.
			if got, want := len(code), 24; got != want {
				t.Fatalf("len(code) = %d, want %d", got, want)
			}
			// cmp prefix at offset 10.
			if code[10] != 0x48 || code[11] != 0x3D {
				t.Errorf("cmp prefix = % x, want 48 3D", code[10:12])
			}
			// setCC opcode at offset 16+1.
			if code[16] != 0x0F || code[17] != c.setOpcode || code[18] != 0xC0 {
				t.Errorf("setCC = % x, want 0F %02x C0", code[16:19], c.setOpcode)
			}
			// Two relocs: OpConst RelocImm64 at 2, *Imm RelocImm32 at 12.
			if got, want := len(relocs), 2; got != want {
				t.Fatalf("len(relocs) = %d, want %d", got, want)
			}
			if relocs[1].Offset != 12 || relocs[1].Kind != RelocImm32 {
				t.Errorf("imm reloc = %+v, want offset=12 kind=imm32", relocs[1])
			}
			if relocs[1].Addend != 7 {
				t.Errorf("imm addend = %d, want 7", relocs[1].Addend)
			}
		})
	}
}

// TestCompileMultiBlockJump checks the multi-block TermJump path on
// amd64. Two blocks: block 0 holds Const + TermJump to block 1; block
// 1 holds the ret. The jump's rel32 must point from the end of the
// rel32 field to the start of block 1. Arm64's TermJump (b rel26)
// lands in Phase 2.1; until then `archSupportsBranches()` is false on
// arm64 and `TestCompileRejectsBranchOnUnsupportedArch` covers the
// reject path.
func TestCompileMultiBlockJump(t *testing.T) {
	fn := &ir.Function{Name: "jump", Result: ir.TypeI64}
	b0 := fn.AddBlock()
	b1 := fn.AddBlock()
	v := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 9})
	fn.Block(b0).Values = append(fn.Block(b0).Values, v)
	fn.Block(b0).Term = ir.Terminator{Kind: ir.TermJump, Target: b1}
	fn.Block(b1).Term = ir.Terminator{Kind: ir.TermReturn, Value: v}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, _, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	// OpConst (10) + jmp E9+rel32 (5) + ret (1) = 16.
	if got, want := len(code), 16; got != want {
		t.Fatalf("len(code) = %d, want %d", got, want)
	}
	if code[10] != 0xE9 {
		t.Errorf("expected E9 at offset 10, got %#02x", code[10])
	}
	// rel32 = block1 start (15) - end of rel32 field (15) = 0.
	disp := int32(binary.LittleEndian.Uint32(code[11:15]))
	if disp != 0 {
		t.Errorf("jump disp = %d, want 0", disp)
	}
	if code[15] != 0xC3 {
		t.Errorf("expected ret (C3) at offset 15, got %#02x", code[15])
	}
}

// TestCompileMultiBlockBranch checks the TermBranch lowering on amd64.
// Three blocks: block 0 produces a bool, branches to block 1 (true) or
// block 2 (false). The emitter must lay down test rax,rax + jne IfTrue
// + jmp IfFalse, with both rel32 displacements pointing at the right
// block starts. Arm64's TermBranch (cbnz rel19 + b rel26) lands in
// Phase 2.1.
func TestCompileMultiBlockBranch(t *testing.T) {
	fn := &ir.Function{Name: "branch", Result: ir.TypeI64}
	b0 := fn.AddBlock()
	b1 := fn.AddBlock()
	b2 := fn.AddBlock()
	vcond := fn.AddValue(ir.Value{Type: ir.TypeBool, Op: ir.OpConst, Const: 1})
	vone := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 1})
	vtwo := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 2})
	fn.Block(b0).Values = append(fn.Block(b0).Values, vcond)
	fn.Block(b0).Term = ir.Terminator{Kind: ir.TermBranch, Value: vcond, IfTrue: b1, IfFalse: b2}
	fn.Block(b1).Values = append(fn.Block(b1).Values, vone)
	fn.Block(b1).Term = ir.Terminator{Kind: ir.TermReturn, Value: vone}
	fn.Block(b2).Values = append(fn.Block(b2).Values, vtwo)
	fn.Block(b2).Term = ir.Terminator{Kind: ir.TermReturn, Value: vtwo}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, _, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	// b0: OpConst (10) + test (3) + jne (6) + jmp (5) = 24.
	// b1: OpConst (10) + ret (1) = 11. Starts at 24.
	// b2: OpConst (10) + ret (1) = 11. Starts at 35.
	// Total 46.
	if got, want := len(code), 46; got != want {
		t.Fatalf("len(code) = %d, want %d", got, want)
	}
	if code[10] != 0x48 || code[11] != 0x85 || code[12] != 0xC0 {
		t.Errorf("test rax,rax bytes = % x, want 48 85 C0", code[10:13])
	}
	if code[13] != 0x0F || code[14] != 0x85 {
		t.Errorf("jne prefix = % x, want 0F 85", code[13:15])
	}
	// jne rel32: site=15, end=19, target=24 → disp=5.
	if disp := int32(binary.LittleEndian.Uint32(code[15:19])); disp != 5 {
		t.Errorf("jne disp = %d, want 5", disp)
	}
	if code[19] != 0xE9 {
		t.Errorf("jmp opcode = %#02x, want E9", code[19])
	}
	// jmp rel32: site=20, end=24, target=35 → disp=11.
	if disp := int32(binary.LittleEndian.Uint32(code[20:24])); disp != 11 {
		t.Errorf("jmp disp = %d, want 11", disp)
	}
}

// TestCompileConstReturnAMD64 asserts the exact amd64 byte layout for
// the OpConst+ret path: mov rax, imm64 (10 bytes, imm at offset 2)
// followed by ret (1 byte, C3). The portable
// `TestCompileConstReturn` in emit_test.go checks the same semantics
// via the host stencil table; this test pins the amd64-specific
// encoding so a regression in stencils_amd64.go fails loudly.
func TestCompileConstReturnAMD64(t *testing.T) {
	fn := buildConstReturn(0x1122334455667788)
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	code, _, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	if got, want := len(code), 11; got != want {
		t.Fatalf("len(code) = %d, want %d", got, want)
	}
	if code[0] != 0x48 || code[1] != 0xB8 {
		t.Errorf("mov rax prefix = % x, want 48 B8", code[0:2])
	}
	if code[10] != 0xC3 {
		t.Errorf("ret byte = %#02x, want C3", code[10])
	}
}
