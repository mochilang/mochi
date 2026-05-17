package opt

import (
	"testing"

	"mochi/compiler2/ir"
)

// buildAddOneCallerCallee constructs a tiny module:
//
//	main(x): r = addOne(x); ret r
//	addOne(x): ret x + 1
//
// Used to exercise the simple single-block leaf-inline path.
func buildAddOneCallerCallee() *ir.Module {
	const addOneIdx = 1

	bMain := ir.NewBuilder("main", []ir.Type{ir.TI64}, ir.TI64)
	mx := bMain.Param(0)
	r := bMain.Call(addOneIdx, ir.TI64, mx)
	bMain.Ret(r)

	bAdd := ir.NewBuilder("addOne", []ir.Type{ir.TI64}, ir.TI64)
	ax := bAdd.Param(0)
	one := bAdd.ConstI64(1)
	sum := bAdd.AddI64(ax, one)
	bAdd.Ret(sum)

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bAdd.Function()}, Main: 0}
}

func TestLeafInlineSingleBlock(t *testing.T) {
	m := buildAddOneCallerCallee()
	if n := LeafInline(m); n != 1 {
		t.Fatalf("LeafInline count = %d, want 1", n)
	}
	main := m.Funcs[0]
	if err := ir.Verify(m, main); err != nil {
		t.Fatalf("Verify caller after inline: %v", err)
	}
	for _, ins := range main.Values {
		if ins.Op == ir.OpCall {
			t.Fatalf("call instruction survived inline: %+v", ins)
		}
	}
}

// buildBranchyCalleeCaller builds a more realistic multi-block leaf:
//
//	main(x): r = sign(x); ret r
//	sign(x): if x < 0 -> ret -1; else if x == 0 -> ret 0; else ret 1
//
// Three Ret blocks merge into a continuation phi after inlining.
func buildBranchyCalleeCaller() *ir.Module {
	const signIdx = 1

	bMain := ir.NewBuilder("main", []ir.Type{ir.TI64}, ir.TI64)
	mx := bMain.Param(0)
	r := bMain.Call(signIdx, ir.TI64, mx)
	bMain.Ret(r)

	bSign := ir.NewBuilder("sign", []ir.Type{ir.TI64}, ir.TI64)
	sx := bSign.Param(0)
	neg := bSign.NewBlock()
	chkZero := bSign.NewBlock()
	zero := bSign.NewBlock()
	pos := bSign.NewBlock()
	bSign.CondBr(bSign.LessI64(sx, bSign.ConstI64(0)), neg, chkZero)
	bSign.SwitchTo(neg)
	negOne := bSign.ConstI64(-1)
	bSign.Ret(negOne)
	bSign.SwitchTo(chkZero)
	bSign.CondBr(bSign.EqualI64(sx, bSign.ConstI64(0)), zero, pos)
	bSign.SwitchTo(zero)
	bSign.Ret(bSign.ConstI64(0))
	bSign.SwitchTo(pos)
	bSign.Ret(bSign.ConstI64(1))

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bSign.Function()}, Main: 0}
}

func TestLeafInlineMultiBlock(t *testing.T) {
	m := buildBranchyCalleeCaller()
	if n := LeafInline(m); n != 1 {
		t.Fatalf("LeafInline count = %d, want 1", n)
	}
	main := m.Funcs[0]
	if err := ir.Verify(m, main); err != nil {
		t.Fatalf("Verify caller after inline: %v", err)
	}
	for _, ins := range main.Values {
		if ins.Op == ir.OpCall {
			t.Fatalf("call instruction survived inline: %+v", ins)
		}
	}
	// Expect a phi at the continuation block head merging three values.
	foundPhi := false
	for _, ins := range main.Values {
		if ins.Op == ir.OpPhi {
			if len(ins.Args) != 3 || len(ins.AuxBlocks) != 3 {
				t.Fatalf("phi: %d args / %d preds, want 3/3", len(ins.Args), len(ins.AuxBlocks))
			}
			foundPhi = true
		}
	}
	if !foundPhi {
		t.Fatalf("expected a phi at the continuation block")
	}
}

// TestLeafInlineSkipsNonLeaf checks that we don't inline a function
// that itself contains a call.
func TestLeafInlineSkipsNonLeaf(t *testing.T) {
	const middleIdx = 1
	const leafIdx = 2

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	one := bMain.ConstI64(1)
	r := bMain.Call(middleIdx, ir.TI64, one)
	bMain.Ret(r)

	bMid := ir.NewBuilder("middle", []ir.Type{ir.TI64}, ir.TI64)
	mx := bMid.Param(0)
	r2 := bMid.Call(leafIdx, ir.TI64, mx)
	bMid.Ret(r2)

	bLeaf := ir.NewBuilder("leaf", []ir.Type{ir.TI64}, ir.TI64)
	lx := bLeaf.Param(0)
	one2 := bLeaf.ConstI64(2)
	sum := bLeaf.AddI64(lx, one2)
	bLeaf.Ret(sum)

	m := &ir.Module{Funcs: []*ir.Function{bMain.Function(), bMid.Function(), bLeaf.Function()}, Main: 0}
	// main calls middle (not a leaf); middle calls leaf (a leaf). Only
	// the latter site should inline.
	if n := LeafInline(m); n != 1 {
		t.Fatalf("LeafInline count = %d, want 1 (only middle->leaf)", n)
	}
	// main's call to middle stays.
	gotMainCall := false
	for _, ins := range m.Funcs[0].Values {
		if ins.Op == ir.OpCall && ins.Aux == middleIdx {
			gotMainCall = true
		}
	}
	if !gotMainCall {
		t.Fatalf("main's call to middle was inlined unexpectedly")
	}
	// middle no longer calls leaf.
	for _, ins := range m.Funcs[1].Values {
		if ins.Op == ir.OpCall {
			t.Fatalf("middle still has a call: %+v", ins)
		}
	}
}

func TestLeafInlineUnitReturn(t *testing.T) {
	// Caller calls a unit-returning leaf in the middle of a block;
	// the leaf has a single Ret. After inlining we should have no
	// phi and no call.
	const leafIdx = 1
	bMain := ir.NewBuilder("main", []ir.Type{ir.TI64}, ir.TI64)
	mx := bMain.Param(0)
	bMain.Call(leafIdx, ir.TUnit, mx)
	bMain.Ret(mx)

	bLeaf := ir.NewBuilder("noop", []ir.Type{ir.TI64}, ir.TUnit)
	_ = bLeaf.Param(0)
	bLeaf.Ret(-1)

	m := &ir.Module{Funcs: []*ir.Function{bMain.Function(), bLeaf.Function()}, Main: 0}
	if n := LeafInline(m); n != 1 {
		t.Fatalf("LeafInline count = %d, want 1", n)
	}
	main := m.Funcs[0]
	if err := ir.Verify(m, main); err != nil {
		t.Fatalf("Verify: %v", err)
	}
	for _, ins := range main.Values {
		if ins.Op == ir.OpCall {
			t.Fatalf("call survived: %+v", ins)
		}
		if ins.Op == ir.OpPhi {
			t.Fatalf("unexpected phi: %+v", ins)
		}
	}
}
