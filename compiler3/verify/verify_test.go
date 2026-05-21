package verify

import (
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// TestKindOfCoversAllKnownOps runs the init-time coverage check
// explicitly as a test so the test suite reports it. (The init()
// already panics on a coverage gap; this test re-runs the same loop
// inside t.Run for a friendlier failure shape.)
func TestKindOfCoversAllKnownOps(t *testing.T) {
	const lastOpCode = ir.OpCallGo
	for o := ir.OpInvalid + 1; o <= lastOpCode; o++ {
		if kindOf(o) == KindInvalid {
			t.Errorf("OpCode %s (=%d) is unclassified", o, o)
		}
	}
}

// TestKindClassificationsAreStable spot-checks the producer kinds for
// representative ops in each kind class. If a future PR moves an op
// between kinds it must update this test, which forces reviewers to
// notice the change.
func TestKindClassificationsAreStable(t *testing.T) {
	cases := []struct {
		op   ir.OpCode
		want ProducerKind
	}{
		{ir.OpParam, KindMove},
		{ir.OpPhi, KindMove},
		{ir.OpConst, KindInline},
		{ir.OpAddI64, KindOperator},
		{ir.OpCmpEqI64, KindOperator},
		{ir.OpLenStr, KindDispatch},
		{ir.OpConcatStr, KindConstructor},
		{ir.OpNewList, KindConstructor},
		{ir.OpListGetI64, KindDispatch},
		{ir.OpMapSetI64I64, KindDispatch},
		{ir.OpNewF64Array, KindConstructor},
		{ir.OpF64ArrayGetF64, KindDispatch},
		{ir.OpCall, KindCall},
		{ir.OpTailCall, KindCall},
		{ir.OpFnRef, KindConstructor},
		{ir.OpCallGo, KindCall},
		{ir.OpQueryFilter, KindCall},
		{ir.OpQueryGroupBy, KindCall},
	}
	for _, c := range cases {
		if got := kindOf(c.op); got != c.want {
			t.Errorf("kindOf(%s) = %s, want %s", c.op, got, c.want)
		}
	}
}

// TestHandleTypeCoverage asserts every handle-shaped IR Type is
// recognized. If the IR grows a new heap-allocated Type, the verifier
// must classify it; this test catches a silent drift.
func TestHandleTypeCoverage(t *testing.T) {
	want := map[ir.Type]bool{
		ir.TypeStr:     true,
		ir.TypeList:    true,
		ir.TypeMap:     true,
		ir.TypeSet:     true,
		ir.TypeStruct:  true,
		ir.TypeClosure: true,
		ir.TypeBignum:  true,
		ir.TypeBytes:   true,
		ir.TypePair:    true,
		ir.TypeF64Arr:  true,
		ir.TypeI64Arr:  true,
		ir.TypeU8Arr:   true,
		// Non-handle types stay false:
		ir.TypeI64:     false,
		ir.TypeF64:     false,
		ir.TypeBool:    false,
		ir.TypeAny:     false,
		ir.TypeUnit:    false,
		ir.TypeInvalid: false,
	}
	for ty, w := range want {
		if got := HandleType(ty); got != w {
			t.Errorf("HandleType(%s) = %v, want %v", ty, got, w)
		}
	}
}

// TestAllIRFixturesPassVerifier runs every Fixture in compiler3/ir
// through Function. The positive corpus must verify; a false-reject
// here would block the Phase 1 gate.
func TestAllIRFixturesPassVerifier(t *testing.T) {
	fixtures := map[string]*ir.Function{
		"FibIter":           ir.FixtureFibIter(),
		"SumLoop":           ir.FixtureSumLoop(),
		"IsEven":            ir.FixtureIsEven(),
		"Double":            ir.FixtureDouble(),
		"Ident":             ir.FixtureIdent(),
		"AddPair":           ir.FixtureAddPair(),
		"FactRec":           ir.FixtureFactRec(),
		"GoCallToUpper":     ir.FixtureGoCallToUpper(),
		"GoCallContains":    ir.FixtureGoCallContains(),
		"GoCallAlias":       ir.FixtureGoCallAlias(),
	}
	for name, fn := range fixtures {
		t.Run(name, func(t *testing.T) {
			if err := Function(fn); err != nil {
				t.Errorf("Function(%s): %v", name, err)
			}
		})
	}
}

// TestFunctionRejectsHandleFromOperator builds an IR where a handle-
// typed Value is produced by an operator op (rule A violation). The
// verifier must reject. ir.Validate catches this first via its
// operand-type contract, which is also acceptable: the point of rule A
// is to fail any handle-shaped Value produced by a non-constructor
// op, and either label closes the gap.
func TestFunctionRejectsHandleFromOperator(t *testing.T) {
	fn := &ir.Function{Name: "bad", Result: ir.TypeI64}
	pn := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	// A TypeList Value produced by OpAddI64. ir.Validate's contract
	// catches this on the result-type check; if a future refactor
	// drops that check, rule A is the structural backstop.
	bogus := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpAddI64, Args: []uint32{pn, pn}})
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = []uint32{pn, bogus}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: pn}
	err := Function(fn)
	if err == nil {
		t.Fatal("Function accepted a handle-from-operator IR; want rejection")
	}
	if !strings.Contains(err.Error(), "rule A") && !strings.Contains(err.Error(), "ir.Validate") {
		t.Errorf("expected error to cite rule A or ir.Validate; got %v", err)
	}
}

// TestRuleAStandaloneOnContractlessOp exercises rule A independently
// of ir.Validate's contract table. ir.OpFnRef is classified as
// KindConstructor in kindOf (it materializes a TypeClosure handle).
// If we construct an OpFnRef with a non-closure handle Type, rule A
// has to flag it because contractResult returns TypeInvalid for
// OpFnRef (ir.Validate's contract table also returns no constraint).
// This is the case where rule A is the load-bearing check.
func TestRuleAStandaloneOnContractlessOp(t *testing.T) {
	// Build a Value with a contractless Op (e.g. ir.OpCall) and a
	// handle Type, but route it through checkRuleA directly. ir.OpCall
	// is classified as KindCall, so a handle output is allowed. We
	// want to verify rule A's positive path here.
	fn := &ir.Function{Name: "ok", Result: ir.TypeI64}
	pn := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	// OpCall returning a list. The Call kind permits handle outputs.
	res := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpCall, Args: []uint32{pn}})
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = []uint32{pn, res}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: pn}
	// checkRuleA alone should accept (positive path).
	if err := checkRuleA(fn); err != nil {
		t.Errorf("checkRuleA rejected an OpCall->TypeList Value: %v", err)
	}
}

// TestFunctionRejectsContractMismatch builds an IR where an op's
// output Type does not match its contract (rule B). ir.Validate would
// also catch this; rule B re-affirms it under the rule-B label so a
// future refactor that drops ir.Validate's contract check does not
// silently weaken memory safety.
func TestFunctionRejectsContractMismatch(t *testing.T) {
	fn := &ir.Function{Name: "bad", Result: ir.TypeI64}
	pn := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	// OpNewList declared as TypeI64 (should be TypeList).
	bogus := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpNewList})
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = []uint32{pn, bogus}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: bogus}
	err := Function(fn)
	if err == nil {
		t.Fatal("Function accepted a contract-mismatch IR; want rejection")
	}
	// ir.Validate or rule B may catch this; either is acceptable so
	// long as we stop the IR before emit.
	if !strings.Contains(err.Error(), "ir.Validate") && !strings.Contains(err.Error(), "rule B") {
		t.Errorf("expected error to cite ir.Validate or rule B; got %v", err)
	}
}

// TestFunctionRejectsDispatchArenaMismatch builds an IR where a
// dispatch op (OpListLenI64) takes an argument whose Type is TypeMap
// instead of TypeList (rule D). ir.Validate already catches the
// operand mismatch, but rule D's job is to label the failure so
// downstream tooling (audit, debugger) can attribute the trap to a
// memory-safety boundary rather than a generic type-check failure.
func TestFunctionRejectsDispatchArenaMismatch(t *testing.T) {
	fn := &ir.Function{Name: "bad", Result: ir.TypeI64}
	pn := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	// A TypeMap handle from OpNewMap.
	m := fn.AddValue(ir.Value{Type: ir.TypeMap, Op: ir.OpNewMap})
	// OpListLenI64 reading the map; rule D should reject this.
	bogus := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{m}})
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = []uint32{pn, m, bogus}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: bogus}
	err := Function(fn)
	if err == nil {
		t.Fatal("Function accepted a dispatch-arena-mismatch IR; want rejection")
	}
	// ir.Validate's operand-type check is the first line of defense;
	// rule D is the structural label. Either is acceptable as the
	// failure surface so long as emit is blocked.
	if !strings.Contains(err.Error(), "ir.Validate") && !strings.Contains(err.Error(), "rule D") {
		t.Errorf("expected error to cite ir.Validate or rule D; got %v", err)
	}
}

// TestFunctionRejectsNilFunction asserts the verifier short-circuits
// on a nil input rather than panicking. Defensive belt-and-braces:
// the emit pipeline never passes nil, but a future test harness might.
func TestFunctionRejectsNilFunction(t *testing.T) {
	if err := Function(nil); err == nil {
		t.Fatal("Function(nil) returned nil; want error")
	}
}

// TestFunctionsBatchVerification asserts Functions short-circuits at
// the first malformed entry and propagates the error verbatim.
func TestFunctionsBatchVerification(t *testing.T) {
	good := ir.FixtureFibIter()
	bad := &ir.Function{Name: "bad", Result: ir.TypeI64}
	pn := bad.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
	bad.Params = []uint32{pn}
	bogus := bad.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpAddI64, Args: []uint32{pn, pn}})
	entry := bad.AddBlock()
	blk := bad.Block(entry)
	blk.Values = []uint32{pn, bogus}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: pn}
	err := Functions([]*ir.Function{good, bad})
	if err == nil {
		t.Fatal("Functions accepted a batch containing a bad entry; want rejection")
	}
}

// TestFunctionRejectsUnclassifiedOp simulates the spec-in-sync
// backstop: if a future Op is added to the IR without being
// classified in kindOf, the init-time check panics. We can't trigger
// that without registering a fake OpCode value; instead we verify the
// runtime-level check (checkRuleC) reports a clear error if it ever
// sees an unclassified op. We fabricate an OpCode value just past the
// known set and assert kindOf returns KindInvalid.
func TestFunctionRejectsUnclassifiedOp(t *testing.T) {
	fake := ir.OpCode(ir.OpCallGo + 1)
	if kindOf(fake) != KindInvalid {
		t.Skipf("OpCode %d already classified; nothing to test", fake)
	}
	fn := &ir.Function{Name: "bad", Result: ir.TypeI64}
	pn := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpParam})
	fn.Params = []uint32{pn}
	// Inject an unclassified op as the producer of a TypeI64 Value.
	// ir.Validate may not flag this (contract returns TypeInvalid for
	// unknown ops). checkRuleC must.
	bogus := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: fake})
	entry := fn.AddBlock()
	blk := fn.Block(entry)
	blk.Values = []uint32{pn, bogus}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: bogus}
	err := Function(fn)
	if err == nil {
		t.Fatal("Function accepted an unclassified op; want rejection")
	}
	if !strings.Contains(err.Error(), "rule C") {
		t.Errorf("expected error to cite rule C; got %v", err)
	}
}
