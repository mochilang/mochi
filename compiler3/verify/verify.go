// Package verify implements the MEP-41 §6.2 verifier rule classes on
// the compiler3 IR. The verifier sits between SSA lowering and emit;
// every emit entry point in compiler3 calls Function on each
// ir.Function before lowering, and verification failure is a
// compile-time error.
//
// The verifier is the single point of memory-safety policy described
// in docs/security/threat-model.md. Phase 1 of MEP-41 covers rule
// classes A through D. Generation opacity (rule class C) is completed
// in Phase 2; the Phase 1 version installs the structural
// no-gen-leaking-op invariant via the producer-kind table. Phase 4
// adds rule class E (reference-mode discipline) on top.
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
//   - Class E (reference-mode discipline). The optional borrow / consume
//     / inout / weak annotations from MEP-41 §6.9 carry verifier
//     obligations: borrow values may not appear as the first argument
//     to a mutating Dispatch op, consume values may appear in at most
//     one Dispatch use, weak values may never appear as a Dispatch arg
//     (the surface language must wire `try_deref`, MEP-16 §6.6, to
//     turn a weak handle into a checked RefModeNone temporary). inout
//     values carry an exclusivity obligation that the present checker
//     records but defers to the frontend until the surface language is
//     in tree. Default-mode Values are unaffected (their Function has
//     nil RefModes).
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
		ir.OpCmpEqI64Imm, ir.OpCmpNeI64Imm, ir.OpCmpLtI64Imm, ir.OpCmpLeI64Imm, ir.OpCmpGtI64Imm, ir.OpCmpGeI64Imm,
		ir.OpAndI64, ir.OpOrI64, ir.OpXorI64, ir.OpShlI64, ir.OpShrI64, ir.OpNotI64,
		ir.OpCmpEqF64, ir.OpCmpNeF64, ir.OpCmpLtF64, ir.OpCmpLeF64, ir.OpCmpGtF64, ir.OpCmpGeF64,
		ir.OpCmpEqStr, ir.OpCmpNeStr,
		ir.OpCmpEqBool, ir.OpCmpNeBool,
		ir.OpAndBool, ir.OpOrBool,
		ir.OpNotBool,
		ir.OpI64ToF64, ir.OpF64ToI64,
		ir.OpSqrtF64,
		ir.OpNow,
		ir.OpJsonI64Object:
		return KindOperator

	case ir.OpLenStr:
		return KindDispatch
	case ir.OpConcatStr, ir.OpI64ToStr, ir.OpF64ToStr, ir.OpBoolToStr, ir.OpListI64ToStr, ir.OpF64ArrayToStr, ir.OpStrArrToStr:
		return KindConstructor

	case ir.OpNewList, ir.OpNewMap, ir.OpNewF64Array, ir.OpNewStrArr, ir.OpNewMapStrI64, ir.OpNewListAny,
		ir.OpNewListList,
		ir.OpListConcatI64, ir.OpF64ArrayConcat,
		ir.OpListAnyGetAny,
		ir.OpStrArrGetStr,
		ir.OpStrArrSlice,
		ir.OpListListGet,
		ir.OpListListToStr:
		// OpListAnyGetAny returns a handle (TypeListAny) borrowed from
		// an existing tree node. OpStrArrGetStr returns a handle
		// (TypeStr) borrowed from a `const char**` slot. Rule A
		// requires handle-typed Values to originate from a
		// Constructor / Move / Inline / Call kind, so the get-ops are
		// classified Constructor (same rationale as OpFnRef: produces
		// a fresh-looking handle whose payload is a derived pointer,
		// not arbitrary bits).
		return KindConstructor

	case ir.OpListLenI64, ir.OpListPushI64, ir.OpListGetI64, ir.OpListSetI64,
		ir.OpListGetF64, ir.OpListSetF64,
		ir.OpMapSetI64I64, ir.OpMapGetI64I64,
		ir.OpMapSetStrI64, ir.OpMapGetStrI64, ir.OpMapLenStrI64,
		ir.OpF64ArrayLenI64, ir.OpF64ArrayPushF64, ir.OpF64ArrayGetF64, ir.OpF64ArraySetF64,
		ir.OpStrArrLen, ir.OpStrArrPushStr, ir.OpStrArrSetStr,
		ir.OpListAnyLen, ir.OpListAnyPushAny,
		ir.OpListListPush, ir.OpListListLen:
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
	mustClassifyAllDispatch()
}

// mustClassifyAll walks every OpCode in [OpInvalid+1, lastOpCode] and
// asserts kindOf returns a non-Invalid classification. lastOpCode is
// the last OpCode known to ir/types.go at this MEP-41 Phase 1 commit;
// adding a new op past it bumps this constant in the same PR.
func mustClassifyAll() {
	const lastOpCode = ir.OpJsonI64Object
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
		ir.TypeF64Arr, ir.TypeI64Arr, ir.TypeU8Arr, ir.TypeListAny,
		ir.TypeMapStrI64, ir.TypeListList:
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
	if err := checkRuleE(fn); err != nil {
		return fmt.Errorf("verify rule E (reference modes): %w", err)
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
		ir.OpCmpEqI64Imm, ir.OpCmpNeI64Imm, ir.OpCmpLtI64Imm, ir.OpCmpLeI64Imm, ir.OpCmpGtI64Imm, ir.OpCmpGeI64Imm,
		ir.OpCmpEqStr, ir.OpCmpNeStr,
		ir.OpCmpEqBool, ir.OpCmpNeBool,
		ir.OpAndBool, ir.OpOrBool:
		return ir.TypeBool
	case ir.OpLenStr:
		return ir.TypeI64
	case ir.OpConcatStr, ir.OpI64ToStr, ir.OpF64ToStr, ir.OpBoolToStr:
		return ir.TypeStr
	case ir.OpNewList:
		return ir.TypeList
	case ir.OpNewMap:
		return ir.TypeMap
	case ir.OpNewF64Array:
		return ir.TypeF64Arr
	case ir.OpNewStrArr:
		return ir.TypeStrArr
	case ir.OpStrArrLen:
		return ir.TypeI64
	case ir.OpStrArrPushStr, ir.OpStrArrSetStr:
		return ir.TypeUnit
	case ir.OpStrArrGetStr:
		return ir.TypeStr
	case ir.OpStrArrSlice:
		return ir.TypeStrArr
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
	case ir.OpNewMapStrI64:
		return ir.TypeMapStrI64
	case ir.OpMapSetStrI64:
		return ir.TypeUnit
	case ir.OpMapGetStrI64, ir.OpMapLenStrI64:
		return ir.TypeI64
	case ir.OpF64ArrayLenI64:
		return ir.TypeI64
	case ir.OpF64ArrayPushF64, ir.OpF64ArraySetF64:
		return ir.TypeUnit
	case ir.OpF64ArrayGetF64:
		return ir.TypeF64
	case ir.OpListConcatI64:
		return ir.TypeList
	case ir.OpF64ArrayConcat:
		return ir.TypeF64Arr
	case ir.OpNow:
		return ir.TypeI64
	case ir.OpNewListAny:
		return ir.TypeListAny
	case ir.OpListAnyLen:
		return ir.TypeI64
	case ir.OpListAnyPushAny:
		return ir.TypeUnit
	case ir.OpListAnyGetAny:
		return ir.TypeListAny
	case ir.OpNewListList:
		return ir.TypeListList
	case ir.OpListListPush:
		return ir.TypeUnit
	case ir.OpListListGet:
		return ir.TypeList
	case ir.OpListListLen:
		return ir.TypeI64
	case ir.OpListListToStr:
		return ir.TypeStr
	case ir.OpJsonI64Object:
		return ir.TypeUnit
	}
	return ir.TypeInvalid
}

// opIsMutating reports whether a KindDispatch op is a write to the
// arena it dispatches into. Rule E uses this classification to refuse
// mutating ops on borrow-tagged values.
//
// Read Dispatch ops: OpLenStr, OpListLenI64, OpListGetI64, OpListGetF64,
// OpMapGetI64I64, OpF64ArrayLenI64, OpF64ArrayGetF64.
//
// Write Dispatch ops: OpListPushI64, OpListSetI64, OpListSetF64,
// OpMapSetI64I64, OpF64ArrayPushF64, OpF64ArraySetF64.
//
// Adding a new Dispatch op to ir/types.go without classifying it here
// would silently default to "non-mutating" for rule E purposes. The
// init() coverage check below catches this by asserting every
// KindDispatch op appears in either readDispatchOps or writeDispatchOps.
func opIsMutating(o ir.OpCode) bool {
	switch o {
	case ir.OpListPushI64, ir.OpListSetI64, ir.OpListSetF64,
		ir.OpMapSetI64I64,
		ir.OpMapSetStrI64,
		ir.OpF64ArrayPushF64, ir.OpF64ArraySetF64,
		ir.OpStrArrPushStr, ir.OpStrArrSetStr,
		ir.OpListAnyPushAny,
		ir.OpListListPush:
		return true
	}
	return false
}

// readDispatchOps lists every KindDispatch op that does not mutate its
// arena. Kept as data (rather than a switch) so the coverage check can
// enumerate both halves of the dispatch op set.
var readDispatchOps = []ir.OpCode{
	ir.OpLenStr,
	ir.OpListLenI64,
	ir.OpListGetI64,
	ir.OpListGetF64,
	ir.OpMapGetI64I64,
	ir.OpMapGetStrI64,
	ir.OpMapLenStrI64,
	ir.OpF64ArrayLenI64,
	ir.OpF64ArrayGetF64,
	ir.OpStrArrLen,
	ir.OpListAnyLen,
	ir.OpListListLen,
}

// writeDispatchOps lists every KindDispatch op that mutates its arena.
var writeDispatchOps = []ir.OpCode{
	ir.OpListPushI64,
	ir.OpListSetI64,
	ir.OpListSetF64,
	ir.OpMapSetI64I64,
	ir.OpMapSetStrI64,
	ir.OpF64ArrayPushF64,
	ir.OpF64ArraySetF64,
	ir.OpStrArrPushStr,
	ir.OpStrArrSetStr,
	ir.OpListAnyPushAny,
	ir.OpListListPush,
}

// checkRuleE verifies the §6.9 reference-mode obligations. Default-mode
// functions (nil RefModes) pass trivially; the rest of the checker only
// runs when at least one Value carries a non-default mode.
//
// The checker walks every KindDispatch Value (the only IR consumers of
// a handle), looks up the mode of the dispatch's handle argument
// (Args[0]), and applies the per-mode obligation:
//
//   - RefModeNone: no obligation. Falls through.
//
//   - RefModeBorrow: every Dispatch arg targeting a borrowed Value must
//     be a read op. A write op is a rule violation. Borrow values may
//     be Dispatch arg multiple times (read-only sharing is the point).
//
//   - RefModeInout: read and write ops are both permitted. The
//     exclusivity obligation (no other live alias) is a surface-language
//     property that the frontend must enforce; rule E records the mode
//     so a future SSA-level alias analysis can be wired in.
//
//   - RefModeConsume: at most one Dispatch op may consume the binding.
//     Reading a consumed Value twice is a rule violation. The intent is
//     to enable `gc.kill`-style deterministic free after the consume
//     point.
//
//   - RefModeWeak: no Dispatch op may take a weak Value as Args[0].
//     The surface language must wire `try_deref` (MEP-16 §6.6) to
//     materialize an Option-typed RefModeNone temporary.
//
// Move-shaped ops (OpPhi, OpParam) do not count as Dispatch uses; the
// SSA join itself preserves the mode obligation downstream.
func checkRuleE(fn *ir.Function) error {
	if len(fn.RefModes) == 0 {
		return nil
	}
	consumeUses := make(map[uint32]int, len(fn.RefModes))
	for i := range fn.Values {
		v := &fn.Values[i]
		if kindOf(v.Op) != KindDispatch {
			continue
		}
		if len(v.Args) == 0 {
			continue
		}
		srcID := v.Args[0]
		if int(srcID) >= len(fn.Values) {
			return fmt.Errorf("v%d op=%s: dispatch arg v%d out of range", i, v.Op, srcID)
		}
		mode := fn.RefModeOf(srcID)
		switch mode {
		case ir.RefModeNone, ir.RefModeInout:
			// inout permits any dispatch; exclusivity is the frontend's
			// responsibility until the surface language lands.
		case ir.RefModeBorrow:
			if opIsMutating(v.Op) {
				return fmt.Errorf("v%d op=%s: mutating dispatch on borrow-tagged v%d (rule E §6.9; demote the source to RefModeInout or copy before mutation)", i, v.Op, srcID)
			}
		case ir.RefModeConsume:
			consumeUses[srcID]++
			if consumeUses[srcID] > 1 {
				return fmt.Errorf("v%d op=%s: consume-tagged v%d used %d times (rule E §6.9; at most one dispatch use per consume binding)", i, v.Op, srcID, consumeUses[srcID])
			}
		case ir.RefModeWeak:
			return fmt.Errorf("v%d op=%s: dispatch on weak-tagged v%d (rule E §6.9; route through try_deref to materialize an Option-typed temporary before the dispatch)", i, v.Op, srcID)
		default:
			return fmt.Errorf("v%d op=%s: dispatch arg v%d carries unknown RefMode=%d", i, v.Op, srcID, mode)
		}
	}
	return nil
}

// mustClassifyAllDispatch asserts every KindDispatch op is listed in
// either readDispatchOps or writeDispatchOps. A new Dispatch op added
// to ir/types.go without classifying it here would silently default to
// non-mutating in checkRuleE, which could mask a borrow-mode violation.
// Running this check at init time keeps the spec-in-sync rule (MEP-41
// §13) machine-enforced for rule E in the same way mustClassifyAll does
// for rule C.
func mustClassifyAllDispatch() {
	seen := make(map[ir.OpCode]bool)
	for _, o := range readDispatchOps {
		if opIsMutating(o) {
			panic(fmt.Sprintf("verify: %s listed in readDispatchOps but opIsMutating reports true", o))
		}
		seen[o] = true
	}
	for _, o := range writeDispatchOps {
		if !opIsMutating(o) {
			panic(fmt.Sprintf("verify: %s listed in writeDispatchOps but opIsMutating reports false", o))
		}
		if seen[o] {
			panic(fmt.Sprintf("verify: %s listed in both readDispatchOps and writeDispatchOps", o))
		}
		seen[o] = true
	}
	const lastOpCode = ir.OpCallGo
	for o := ir.OpInvalid + 1; o <= lastOpCode; o++ {
		if kindOf(o) != KindDispatch {
			continue
		}
		if !seen[o] {
			panic(fmt.Sprintf("verify: Dispatch OpCode %s (=%d) is not classified for rule E; add it to readDispatchOps or writeDispatchOps in compiler3/verify/verify.go", o, o))
		}
	}
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
	case ir.OpMapSetStrI64, ir.OpMapGetStrI64, ir.OpMapLenStrI64:
		return ir.TypeMapStrI64
	case ir.OpF64ArrayLenI64, ir.OpF64ArrayPushF64, ir.OpF64ArrayGetF64, ir.OpF64ArraySetF64:
		return ir.TypeF64Arr
	case ir.OpStrArrLen, ir.OpStrArrPushStr, ir.OpStrArrGetStr, ir.OpStrArrSetStr:
		return ir.TypeStrArr
	case ir.OpListAnyLen, ir.OpListAnyPushAny:
		return ir.TypeListAny
	case ir.OpListListPush, ir.OpListListLen:
		return ir.TypeListList
	}
	return ir.TypeInvalid
}
