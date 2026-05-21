package verify

import (
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// ruleEFixture builds a tiny function with one TypeList parameter and
// a single Dispatch op against it, returning fn, the param ID, and the
// dispatch op ID. Tests then tag the param with a RefMode and assert
// rule E accepts or rejects.
//
// The function shape:
//
//	func(l: list) -> i64 {
//	    op := <dispatch>(l, ...)
//	    return <i64 result>
//	}
//
// dispatchOp is the Op to apply. extra args are added in order after
// the list handle. resultType controls the function's Result.
func ruleEFixture(t *testing.T, dispatchOp ir.OpCode, extras []ir.Value, resultType ir.Type) (*ir.Function, uint32, uint32) {
	t.Helper()
	fn := &ir.Function{Name: "fx", Result: resultType}
	pn := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	args := []uint32{pn}
	allValues := []uint32{pn}
	for i := range extras {
		id := fn.AddValue(extras[i])
		args = append(args, id)
		allValues = append(allValues, id)
	}
	disp := fn.AddValue(ir.Value{Type: resultTypeOf(dispatchOp), Op: dispatchOp, Args: args})
	allValues = append(allValues, disp)
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = allValues
	switch resultType {
	case ir.TypeUnit:
		blk.Term = ir.Terminator{Kind: ir.TermReturn}
	default:
		// Need a value of resultType for the return. If the dispatch
		// itself produces resultType, return it; otherwise add a const.
		if fn.Values[disp].Type == resultType {
			blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: disp}
		} else {
			k := fn.AddValue(ir.Value{Type: resultType, Op: ir.OpConst})
			blk.Values = append(blk.Values, k)
			blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: k}
		}
	}
	return fn, pn, disp
}

// resultTypeOf returns the IR-declared result Type of an Op. Mirrors
// the contract table; kept inline here so the test does not reach into
// the ir package's private opContract symbol.
func resultTypeOf(o ir.OpCode) ir.Type {
	switch o {
	case ir.OpListLenI64:
		return ir.TypeI64
	case ir.OpListGetI64:
		return ir.TypeI64
	case ir.OpListPushI64, ir.OpListSetI64, ir.OpListSetF64:
		return ir.TypeUnit
	case ir.OpListGetF64:
		return ir.TypeF64
	}
	return ir.TypeInvalid
}

// TestRuleEDefaultModeAccepts asserts that a function with no RefModes
// passes rule E trivially. This is the load-bearing property: existing
// default-mode programs see zero new verifier overhead.
func TestRuleEDefaultModeAccepts(t *testing.T) {
	fn, _, _ := ruleEFixture(t, ir.OpListLenI64, nil, ir.TypeI64)
	if err := Function(fn); err != nil {
		t.Fatalf("default-mode list-len function failed verification: %v", err)
	}
}

// TestRuleEBorrowAcceptsRead asserts that a read Dispatch (OpListLenI64)
// against a borrow-tagged value is accepted.
func TestRuleEBorrowAcceptsRead(t *testing.T) {
	fn, pn, _ := ruleEFixture(t, ir.OpListLenI64, nil, ir.TypeI64)
	fn.SetRefMode(pn, ir.RefModeBorrow)
	if err := Function(fn); err != nil {
		t.Fatalf("borrow+read should verify: %v", err)
	}
}

// TestRuleEBorrowRejectsWrite asserts that a write Dispatch
// (OpListPushI64) against a borrow-tagged value is rejected with a
// rule E citation. This is the core borrow guarantee.
func TestRuleEBorrowRejectsWrite(t *testing.T) {
	fn, pn, _ := ruleEFixture(t, ir.OpListPushI64,
		[]ir.Value{{Type: ir.TypeI64, Op: ir.OpConst, Const: 7}}, ir.TypeUnit)
	fn.SetRefMode(pn, ir.RefModeBorrow)
	err := Function(fn)
	if err == nil {
		t.Fatal("borrow+write should fail; got nil")
	}
	if !strings.Contains(err.Error(), "rule E") {
		t.Errorf("error should cite rule E; got %v", err)
	}
	if !strings.Contains(err.Error(), "borrow-tagged") {
		t.Errorf("error should mention borrow-tagged; got %v", err)
	}
}

// TestRuleEBorrowRejectsSet checks the OpListSetI64 branch as well, so
// the rejection is not specific to OpListPushI64.
func TestRuleEBorrowRejectsSet(t *testing.T) {
	fn, pn, _ := ruleEFixture(t, ir.OpListSetI64,
		[]ir.Value{
			{Type: ir.TypeI64, Op: ir.OpConst, Const: 0},
			{Type: ir.TypeI64, Op: ir.OpConst, Const: 42},
		}, ir.TypeUnit)
	fn.SetRefMode(pn, ir.RefModeBorrow)
	if err := Function(fn); err == nil || !strings.Contains(err.Error(), "rule E") {
		t.Fatalf("expected rule E rejection on borrow+OpListSetI64; got %v", err)
	}
}

// TestRuleEInoutAcceptsRead asserts inout permits reads.
func TestRuleEInoutAcceptsRead(t *testing.T) {
	fn, pn, _ := ruleEFixture(t, ir.OpListLenI64, nil, ir.TypeI64)
	fn.SetRefMode(pn, ir.RefModeInout)
	if err := Function(fn); err != nil {
		t.Errorf("inout+read should verify: %v", err)
	}
}

// TestRuleEInoutAcceptsWrite asserts inout permits writes (the
// exclusivity obligation is deferred to the frontend per §6.9).
func TestRuleEInoutAcceptsWrite(t *testing.T) {
	fn, pn, _ := ruleEFixture(t, ir.OpListPushI64,
		[]ir.Value{{Type: ir.TypeI64, Op: ir.OpConst, Const: 1}}, ir.TypeUnit)
	fn.SetRefMode(pn, ir.RefModeInout)
	if err := Function(fn); err != nil {
		t.Errorf("inout+write should verify: %v", err)
	}
}

// TestRuleEConsumeAcceptsSingleUse asserts a consume binding may
// participate in exactly one Dispatch.
func TestRuleEConsumeAcceptsSingleUse(t *testing.T) {
	fn, pn, _ := ruleEFixture(t, ir.OpListLenI64, nil, ir.TypeI64)
	fn.SetRefMode(pn, ir.RefModeConsume)
	if err := Function(fn); err != nil {
		t.Errorf("consume+single-use should verify: %v", err)
	}
}

// TestRuleEConsumeRejectsDoubleUse builds an IR where a consume-tagged
// list is used by two Dispatch ops in the same block. Rule E must
// reject the second use.
func TestRuleEConsumeRejectsDoubleUse(t *testing.T) {
	fn := &ir.Function{Name: "consume2", Result: ir.TypeI64}
	pn := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	len1 := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{pn}})
	len2 := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{pn}})
	sum := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpAddI64, Args: []uint32{len1, len2}})
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = []uint32{pn, len1, len2, sum}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: sum}
	fn.SetRefMode(pn, ir.RefModeConsume)
	err := Function(fn)
	if err == nil {
		t.Fatal("consume+two-uses should fail; got nil")
	}
	if !strings.Contains(err.Error(), "rule E") {
		t.Errorf("error should cite rule E; got %v", err)
	}
	if !strings.Contains(err.Error(), "consume-tagged") {
		t.Errorf("error should mention consume-tagged; got %v", err)
	}
}

// TestRuleEWeakRejectsAnyDispatch asserts a weak binding cannot appear
// as Args[0] of any Dispatch (read or write). The frontend must wire
// try_deref before dispatching against a weak handle.
func TestRuleEWeakRejectsAnyDispatch(t *testing.T) {
	for _, op := range []ir.OpCode{ir.OpListLenI64, ir.OpListGetI64, ir.OpListPushI64} {
		t.Run(op.String(), func(t *testing.T) {
			var extras []ir.Value
			var rt ir.Type
			switch op {
			case ir.OpListLenI64:
				rt = ir.TypeI64
			case ir.OpListGetI64:
				extras = []ir.Value{{Type: ir.TypeI64, Op: ir.OpConst}}
				rt = ir.TypeI64
			case ir.OpListPushI64:
				extras = []ir.Value{{Type: ir.TypeI64, Op: ir.OpConst, Const: 1}}
				rt = ir.TypeUnit
			}
			fn, pn, _ := ruleEFixture(t, op, extras, rt)
			fn.SetRefMode(pn, ir.RefModeWeak)
			err := Function(fn)
			if err == nil {
				t.Fatalf("weak+%s should fail; got nil", op)
			}
			if !strings.Contains(err.Error(), "rule E") {
				t.Errorf("error should cite rule E; got %v", err)
			}
			if !strings.Contains(err.Error(), "weak-tagged") {
				t.Errorf("error should mention weak-tagged; got %v", err)
			}
		})
	}
}

// TestRuleENonDispatchUseIgnored asserts that a non-Dispatch reference
// (e.g., OpCall passing the borrowed handle along) is not policed by
// rule E directly. The callee's own rule E walks downstream uses.
func TestRuleENonDispatchUseIgnored(t *testing.T) {
	fn := &ir.Function{Name: "callthrough", Result: ir.TypeI64}
	pn := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	// OpCall passing pn along. KindCall, not KindDispatch.
	call := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpCall, Args: []uint32{pn}})
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = []uint32{pn, call}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: call}
	fn.SetRefMode(pn, ir.RefModeBorrow)
	if err := Function(fn); err != nil {
		t.Errorf("borrow + non-dispatch use should verify: %v", err)
	}
}

// TestSetRefModePanicsOnRewrite asserts the SetRefMode helper panics
// when a Value's mode is already set to a different mode. This catches
// frontend bugs at the call site rather than letting them silently
// last-write-wins.
func TestSetRefModePanicsOnRewrite(t *testing.T) {
	fn := &ir.Function{Name: "rewrite"}
	id := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpParam})
	fn.SetRefMode(id, ir.RefModeBorrow)
	defer func() {
		if r := recover(); r == nil {
			t.Error("SetRefMode should panic when rewriting an existing mode")
		}
	}()
	fn.SetRefMode(id, ir.RefModeConsume)
}

// TestSetRefModeIdempotent asserts setting the same mode twice is a
// no-op (no panic, no double-entry in the map).
func TestSetRefModeIdempotent(t *testing.T) {
	fn := &ir.Function{Name: "idem"}
	id := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpParam})
	fn.SetRefMode(id, ir.RefModeBorrow)
	fn.SetRefMode(id, ir.RefModeBorrow)
	if got := fn.RefModeOf(id); got != ir.RefModeBorrow {
		t.Errorf("after double-SetRefMode(borrow), got %s", got)
	}
	if len(fn.RefModes) != 1 {
		t.Errorf("RefModes has %d entries; want 1", len(fn.RefModes))
	}
}

// TestSetRefModeNoneRemoves asserts setting RefModeNone removes the
// tag entirely (returns the Value to default-mode behavior).
func TestSetRefModeNoneRemoves(t *testing.T) {
	fn := &ir.Function{Name: "remove"}
	id := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpParam})
	fn.SetRefMode(id, ir.RefModeBorrow)
	fn.SetRefMode(id, ir.RefModeNone)
	if got := fn.RefModeOf(id); got != ir.RefModeNone {
		t.Errorf("after SetRefMode(none), got %s", got)
	}
	if _, ok := fn.RefModes[id]; ok {
		t.Error("RefModes still contains the entry after SetRefMode(none)")
	}
}

// TestRefModeOfNilMap asserts the accessor returns RefModeNone for a
// Function whose RefModes is nil (the default case).
func TestRefModeOfNilMap(t *testing.T) {
	fn := &ir.Function{Name: "nilmap"}
	id := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpParam})
	if got := fn.RefModeOf(id); got != ir.RefModeNone {
		t.Errorf("nil RefModes should return RefModeNone; got %s", got)
	}
}

// TestRefModeStringerCoversAllValues asserts every defined RefMode has
// a String() entry. If a new mode is added without extending String(),
// this test fails.
func TestRefModeStringerCoversAllValues(t *testing.T) {
	cases := []struct {
		m    ir.RefMode
		want string
	}{
		{ir.RefModeNone, "none"},
		{ir.RefModeConsume, "consume"},
		{ir.RefModeBorrow, "borrow"},
		{ir.RefModeInout, "inout"},
		{ir.RefModeWeak, "weak"},
	}
	for _, c := range cases {
		if got := c.m.String(); got != c.want {
			t.Errorf("RefMode(%d).String() = %q, want %q", c.m, got, c.want)
		}
	}
}

// TestOpIsMutatingClassification spot-checks the dispatch op
// classification used by rule E. Adding a new Dispatch op without
// classifying it triggers the init-time mustClassifyAllDispatch
// panic; this test mirrors that invariant in a friendlier surface.
func TestOpIsMutatingClassification(t *testing.T) {
	cases := []struct {
		op   ir.OpCode
		want bool
	}{
		{ir.OpListLenI64, false},
		{ir.OpListGetI64, false},
		{ir.OpListGetF64, false},
		{ir.OpMapGetI64I64, false},
		{ir.OpF64ArrayLenI64, false},
		{ir.OpF64ArrayGetF64, false},
		{ir.OpLenStr, false},
		{ir.OpListPushI64, true},
		{ir.OpListSetI64, true},
		{ir.OpListSetF64, true},
		{ir.OpMapSetI64I64, true},
		{ir.OpF64ArrayPushF64, true},
		{ir.OpF64ArraySetF64, true},
	}
	for _, c := range cases {
		if got := opIsMutating(c.op); got != c.want {
			t.Errorf("opIsMutating(%s) = %v, want %v", c.op, got, c.want)
		}
	}
}

// TestMustClassifyAllDispatchCoversAllDispatchOps asserts every
// KindDispatch op in the IR appears in either readDispatchOps or
// writeDispatchOps. The init-time check panics on a coverage gap;
// running the same loop as a test surfaces a friendlier message.
func TestMustClassifyAllDispatchCoversAllDispatchOps(t *testing.T) {
	read := make(map[ir.OpCode]bool)
	for _, o := range readDispatchOps {
		read[o] = true
	}
	write := make(map[ir.OpCode]bool)
	for _, o := range writeDispatchOps {
		write[o] = true
	}
	const lastOpCode = ir.OpCallGo
	for o := ir.OpInvalid + 1; o <= lastOpCode; o++ {
		if kindOf(o) != KindDispatch {
			continue
		}
		if !read[o] && !write[o] {
			t.Errorf("Dispatch op %s is unclassified for rule E", o)
		}
		if read[o] && write[o] {
			t.Errorf("Dispatch op %s is in both read and write lists", o)
		}
	}
}
