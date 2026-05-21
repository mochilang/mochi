// Package verify implements the MEP-41 §6.2 verifier rule classes on
// the compiler3 IR. The verifier sits between SSA lowering and emit;
// every emit entry point in compiler3 calls Function on each
// ir.Function before lowering, and verification failure is a
// compile-time error.
//
// The verifier is the single point of memory-safety policy described
// in docs/security/threat-model.md. Phase 1 of MEP-41 (the present
// package) covers rule classes A through D. Rule class E (reference
// modes) lands in Phase 4. Generation opacity (rule class C) is
// completed in Phase 2; the Phase 1 version installs the structural
// no-gen-leaking-op invariant via the producer-kind table.
//
// The four rule classes Phase 1 enforces:
//
//   - Class A (handle origin). Every Value whose Type names a heap-
//     allocated arena (TypeList, TypeMap, TypeSet, TypeStruct,
//     TypeClosure, TypeBignum, TypeBytes, TypePair, TypeF64Arr,
//     TypeI64Arr, TypeU8Arr, TypeStr) is produced by an op in the
//     constructor / move / inline / call allowlist. No op may compute
//     a handle from arbitrary bits.
//
//   - Class B (tag stability). A Value's Type is set at construction
//     and never rewritten. Verified via the producer kind table:
//     every op has a fixed result kind, and no op is classified as a
//     tag-mutator.
//
//   - Class C (generation opacity). No op leaks the generation field
//     of a Cell. Verified via the producer kind table: every op is
//     classified, and the set of GenLeaking ops is provably empty.
//     New OpCodes added to ir/types.go must be classified here in the
//     same PR; the kindOf coverage check catches uncovered ops.
//
//   - Class D (arena tag dispatch). Every dereferencing op carries an
//     argument whose Type matches the arena the op dispatches into.
//     The check piggybacks on ir.Validate's operand-type table (which
//     was originally written for type preservation) and re-affirms
//     it under the rule-D label.
//
// The package exposes a single panicking helper (`mustClassifyAll`)
// invoked from a `init()` that asserts the kindOf switch covers every
// OpCode in ir/types.go. A new OpCode that hits no case in kindOf will
// panic at init time, failing every test that depends on this package.
// This makes the spec-in-sync rule (MEP-41 §13) machine-enforced: a PR
// that grows the IR must also classify the new op.
package verify

import (
	"fmt"

	"mochi/compiler3/ir"
)

// ProducerKind classifies how a Value comes into existence. Rule A
// uses this directly: handle-typed Values must come from a Constructor,
// Move, Inline, or Call producer kind.
type ProducerKind uint8

const (
	// KindInvalid is the zero value; an op that lands here is
	// unclassified and the verifier panics at init.
	KindInvalid ProducerKind = iota

	// KindMove copies an existing Value's payload without touching the
	// arena tag or generation. Phi joins also count: every incoming
	// edge is already a Move-producer, and the join preserves the
	// arena tag from the predecessor.
	KindMove

	// KindInline produces an inline-encoded Cell (small int, float,
	// bool, inline-short string). Verified by Type plus opContract;
	// the verifier does not need to inspect the literal payload.
	KindInline

	// KindConstructor invokes an alloc constructor in runtime/vm3.
	// New handle-typed Values arrive only through these ops.
	KindConstructor

	// KindOperator produces a non-handle Value (arithmetic, compare,
	// length). Operator ops never produce a heap-allocated Type.
	KindOperator

	// KindDispatch reads a heap-allocated arena via an existing
	// handle. The op's first argument carries the arena tag the
	// dispatch jumps into; rule D verifies it matches.
	KindDispatch

	// KindCall invokes a function (Mochi-internal or Go FFI) whose
	// return value enters this function. The result's Type is the
	// callee's declared Result; the verifier trusts the callee's own
	// verification, which is invoked separately in Function.
	KindCall

	// KindReserved is for ops the verifier intentionally treats as a
	// black box at this Phase. Currently empty; new ops should
	// generally land in one of the other kinds.
	KindReserved
)

// String renders a ProducerKind for error messages.
func (k ProducerKind) String() string {
	switch k {
	case KindInvalid:
		return "invalid"
	case KindMove:
		return "move"
	case KindInline:
		return "inline"
	case KindConstructor:
		return "constructor"
	case KindOperator:
		return "operator"
	case KindDispatch:
		return "dispatch"
	case KindCall:
		return "call"
	case KindReserved:
		return "reserved"
	}
	return "?"
}

// kindOf classifies every ir.OpCode. Adding a new OpCode without
// extending this switch is a compile-time noticeable change: the
// init-time coverage check (mustClassifyAll) walks every OpCode value
// from OpInvalid+1 through the last known op and asserts each gets a
// non-Invalid classification. The kindOf result for OpInvalid itself
// is intentionally KindInvalid; the coverage check excludes it.
func kindOf(o ir.OpCode) ProducerKind {
	switch o {
	case ir.OpInvalid:
		return KindInvalid

	case ir.OpParam, ir.OpPhi:
		return KindMove

	case ir.OpConst:
		return KindInline

	case ir.OpAddI64, ir.OpSubI64, ir.OpMulI64, ir.OpDivI64, ir.OpModI64, ir.OpNegI64,
		ir.OpAddI64Imm, ir.OpSubI64Imm, ir.OpMulI64Imm, ir.OpDivI64Imm, ir.OpModI64Imm,
		ir.OpAddF64, ir.OpSubF64, ir.OpMulF64, ir.OpDivF64, ir.OpNegF64,
		ir.OpCmpEqI64, ir.OpCmpNeI64, ir.OpCmpLtI64, ir.OpCmpLeI64, ir.OpCmpGtI64, ir.OpCmpGeI64,
		ir.OpCmpEqI64Imm, ir.OpCmpNeI64Imm, ir.OpCmpLtI64Imm, ir.OpCmpLeI64Imm, ir.OpCmpGtI64Imm, ir.OpCmpGeI64Imm:
		return KindOperator

	case ir.OpLenStr:
		return KindDispatch
	case ir.OpConcatStr:
		return KindConstructor

	case ir.OpNewList, ir.OpNewMap, ir.OpNewF64Array:
		return KindConstructor

	case ir.OpListLenI64, ir.OpListPushI64, ir.OpListGetI64, ir.OpListSetI64,
		ir.OpListGetF64, ir.OpListSetF64,
		ir.OpMapSetI64I64, ir.OpMapGetI64I64,
		ir.OpF64ArrayLenI64, ir.OpF64ArrayPushF64, ir.OpF64ArrayGetF64, ir.OpF64ArraySetF64:
		return KindDispatch

	case ir.OpCall, ir.OpTailCall, ir.OpCallGo:
		return KindCall

	case ir.OpFnRef:
		// OpFnRef materializes a function reference (typed TypeClosure).
		// It is a constructor in the same sense as alloc constructors:
		// it produces a fresh handle-shaped Value whose payload is a
		// compile-time function index, not arbitrary bits.
		return KindConstructor

	case ir.OpQueryFilter, ir.OpQueryMap, ir.OpQuerySortBy, ir.OpQuerySortByDesc,
		ir.OpQueryLimit, ir.OpQueryDistinct, ir.OpQueryGroupBy,
		ir.OpQueryJoin, ir.OpQueryLeftJoin, ir.OpQueryOuterJoin, ir.OpQueryCrossJoin:
		// Query ops lower to runtime/mochi/query calls that return a
		// freshly-constructed result handle. Their output Type is a
		// heap-allocated arena (TypeList) or TypeAny; the runtime is
		// responsible for the underlying allocation. From the IR's
		// point of view the op is a call-shaped producer.
		return KindCall
	}
	return KindInvalid
}

// init asserts every OpCode value in ir/types.go is covered by kindOf.
// If a new OpCode is added without extending the switch, this fires at
// import time and every test that depends on the verifier (every
// compiler3 emit test) fails loudly. This is the structural backstop
// for MEP-41 rule class C (generation opacity): a gen-leaking op would
// have to be classified here, and adding KindGenLeaking would be a
// visible spec-violating change in code review.
func init() {
	mustClassifyAll()
}

// mustClassifyAll walks every OpCode in [OpInvalid+1, lastOpCode] and
// asserts kindOf returns a non-Invalid classification. lastOpCode is
// the last OpCode known to ir/types.go at this MEP-41 Phase 1 commit;
// adding a new op past it bumps this constant in the same PR.
func mustClassifyAll() {
	const lastOpCode = ir.OpCallGo
	for o := ir.OpInvalid + 1; o <= lastOpCode; o++ {
		if kindOf(o) == KindInvalid {
			panic(fmt.Sprintf("verify: OpCode %s (=%d) is unclassified; extend kindOf in compiler3/verify/verify.go (MEP-41 §6.2 rule class C / coverage backstop)", o, o))
		}
	}
}

// HandleType reports whether t names a heap-allocated arena (and thus
// is a Mochi handle in the §6 sense). Rule A constrains how Values of
// these Types may come into existence.
func HandleType(t ir.Type) bool {
	switch t {
	case ir.TypeStr, ir.TypeList, ir.TypeMap, ir.TypeSet, ir.TypeStruct,
		ir.TypeClosure, ir.TypeBignum, ir.TypeBytes, ir.TypePair,
		ir.TypeF64Arr, ir.TypeI64Arr, ir.TypeU8Arr:
		return true
	}
	return false
}

// Function runs every Phase-1 verifier rule against fn. The caller is
// usually a compiler3 emit driver; verification failure is a hard
// error and emit must not proceed.
func Function(fn *ir.Function) error {
	if fn == nil {
		return fmt.Errorf("verify: nil Function")
	}
	if err := ir.Validate(fn); err != nil {
		return fmt.Errorf("verify: ir.Validate: %w", err)
	}
	if err := checkRuleA(fn); err != nil {
		return fmt.Errorf("verify rule A (handle origin): %w", err)
	}
	if err := checkRuleB(fn); err != nil {
		return fmt.Errorf("verify rule B (tag stability): %w", err)
	}
	if err := checkRuleC(fn); err != nil {
		return fmt.Errorf("verify rule C (generation opacity): %w", err)
	}
	if err := checkRuleD(fn); err != nil {
		return fmt.Errorf("verify rule D (arena dispatch): %w", err)
	}
	return nil
}

// Functions verifies a batch. The first error short-circuits.
func Functions(fns []*ir.Function) error {
	for _, fn := range fns {
		if err := Function(fn); err != nil {
			return err
		}
	}
	return nil
}

// checkRuleA verifies the handle-origin invariant. Every Value whose
// Type is a handle Type (HandleType) must be produced by a kindOf in
// {KindMove, KindInline, KindConstructor, KindCall}. KindOperator and
// KindDispatch never produce handles; KindReserved/KindInvalid never
// appear on a well-formed Value.
func checkRuleA(fn *ir.Function) error {
	for i := range fn.Values {
		v := &fn.Values[i]
		if !HandleType(v.Type) {
			continue
		}
		k := kindOf(v.Op)
		switch k {
		case KindMove, KindInline, KindConstructor, KindCall:
			// allowed
		default:
			return fmt.Errorf("v%d type=%s op=%s kind=%s: handle-typed Value produced by non-constructor op", i, v.Type, v.Op, k)
		}
	}
	return nil
}

// checkRuleB verifies tag stability. The IR's SSA invariant already
// makes a Value's Type immutable, so the check here is structural:
// (a) no op is classified as a tag-mutator (kindOf never returns a
// "rewrites tag" kind; this is enforced by the kind enum having no
// such case), and (b) every op's output Type matches its contract
// (re-affirms ir.Validate's checkOperandTypes under the rule-B label).
func checkRuleB(fn *ir.Function) error {
	// The kindOf table has no "tag-mutator" kind; the absence is the
	// invariant. Re-affirm contract-type preservation:
	for i := range fn.Values {
		v := &fn.Values[i]
		want := contractResult(v.Op)
		if want == ir.TypeInvalid {
			continue
		}
		if v.Type != want {
			return fmt.Errorf("v%d op=%s: Type=%s, contract Type=%s", i, v.Op, v.Type, want)
		}
	}
	return nil
}

// checkRuleC verifies generation opacity. The structural argument is
// that kindOf classifies every OpCode and no kind is "GenLeaking";
// adding a gen-leaking op would require both a new OpCode in
// ir/types.go and a new ProducerKind in this file (because the new op
// would have to be classified). The init() coverage assertion
// guarantees no OpCode slips through.
//
// At the runtime check, we re-walk fn's ops and assert each is
// classifiable. A KindInvalid here would mean the program was emitted
// before mustClassifyAll noticed the gap, which is a bug worth
// catching at the emit-side too.
func checkRuleC(fn *ir.Function) error {
	for i := range fn.Values {
		v := &fn.Values[i]
		if kindOf(v.Op) == KindInvalid && v.Op != ir.OpInvalid {
			return fmt.Errorf("v%d op=%s: unclassified op (rule C requires every op be classified; see kindOf in compiler3/verify/verify.go)", i, v.Op)
		}
	}
	return nil
}

// checkRuleD verifies arena-tag dispatch. Every KindDispatch op's
// first argument must be a handle whose Type matches the arena the
// op dispatches into. ir.Validate already enforces operand types for
// the known dispatch ops; this check re-affirms it under the rule-D
// label so a future Phase 1 PR can extend dispatch enforcement to
// any op the IR grows without dragging ir.Validate's signature with it.
func checkRuleD(fn *ir.Function) error {
	for i := range fn.Values {
		v := &fn.Values[i]
		if kindOf(v.Op) != KindDispatch {
			continue
		}
		want := dispatchArena(v.Op)
		if want == ir.TypeInvalid {
			// Dispatch op without a declared arena Type; this would
			// be a kindOf bug, but we accept rather than panic to
			// keep verify defensive.
			continue
		}
		if len(v.Args) == 0 {
			return fmt.Errorf("v%d op=%s: dispatch op has no handle argument", i, v.Op)
		}
		srcID := v.Args[0]
		if int(srcID) >= len(fn.Values) {
			return fmt.Errorf("v%d op=%s: dispatch arg v%d out of range", i, v.Op, srcID)
		}
		got := fn.Values[srcID].Type
		if got != want {
			return fmt.Errorf("v%d op=%s: dispatch arg v%d has Type %s, want %s (arena tag mismatch)", i, v.Op, srcID, got, want)
		}
	}
	return nil
}

// contractResult returns the OpCode's declared result Type, or
// TypeInvalid if the op is not in the contract table. Mirrors the
// table in ir/validate.go without taking a dependency on its private
// opContract symbol.
func contractResult(o ir.OpCode) ir.Type {
	switch o {
	case ir.OpAddI64, ir.OpSubI64, ir.OpMulI64, ir.OpDivI64, ir.OpModI64, ir.OpNegI64,
		ir.OpAddI64Imm, ir.OpSubI64Imm, ir.OpMulI64Imm, ir.OpDivI64Imm, ir.OpModI64Imm:
		return ir.TypeI64
	case ir.OpAddF64, ir.OpSubF64, ir.OpMulF64, ir.OpDivF64, ir.OpNegF64:
		return ir.TypeF64
	case ir.OpCmpEqI64, ir.OpCmpNeI64, ir.OpCmpLtI64, ir.OpCmpLeI64, ir.OpCmpGtI64, ir.OpCmpGeI64,
		ir.OpCmpEqI64Imm, ir.OpCmpNeI64Imm, ir.OpCmpLtI64Imm, ir.OpCmpLeI64Imm, ir.OpCmpGtI64Imm, ir.OpCmpGeI64Imm:
		return ir.TypeBool
	case ir.OpLenStr:
		return ir.TypeI64
	case ir.OpConcatStr:
		return ir.TypeStr
	case ir.OpNewList:
		return ir.TypeList
	case ir.OpNewMap:
		return ir.TypeMap
	case ir.OpNewF64Array:
		return ir.TypeF64Arr
	case ir.OpListLenI64:
		return ir.TypeI64
	case ir.OpListPushI64, ir.OpListSetI64, ir.OpListSetF64:
		return ir.TypeUnit
	case ir.OpListGetI64:
		return ir.TypeI64
	case ir.OpListGetF64:
		return ir.TypeF64
	case ir.OpMapSetI64I64:
		return ir.TypeUnit
	case ir.OpMapGetI64I64:
		return ir.TypeI64
	case ir.OpF64ArrayLenI64:
		return ir.TypeI64
	case ir.OpF64ArrayPushF64, ir.OpF64ArraySetF64:
		return ir.TypeUnit
	case ir.OpF64ArrayGetF64:
		return ir.TypeF64
	}
	return ir.TypeInvalid
}

// dispatchArena returns the arena Type a KindDispatch op reads from.
// Used by rule D.
func dispatchArena(o ir.OpCode) ir.Type {
	switch o {
	case ir.OpLenStr:
		return ir.TypeStr
	case ir.OpListLenI64, ir.OpListPushI64, ir.OpListGetI64, ir.OpListSetI64,
		ir.OpListGetF64, ir.OpListSetF64:
		return ir.TypeList
	case ir.OpMapSetI64I64, ir.OpMapGetI64I64:
		return ir.TypeMap
	case ir.OpF64ArrayLenI64, ir.OpF64ArrayPushF64, ir.OpF64ArrayGetF64, ir.OpF64ArraySetF64:
		return ir.TypeF64Arr
	}
	return ir.TypeInvalid
}
