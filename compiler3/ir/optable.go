// MEP-42 Phase 4.2.30: data-driven op registry.
//
// Before this phase, adding a new IR op required hand-touching ~10
// places across 5 files: the iota declaration and String() case in
// ir/types.go, the opContract switch in ir/validate.go, the kindOf
// and contractResult switches in verify/verify.go, the
// readDispatchOps / writeDispatchOps slices in verify/verify.go (for
// Dispatch ops), the emit case in emit/c/emit.go, the emit case in
// emit/go/emit.go, plus the genuinely op-specific frontend lowering
// hook. The same metadata (Result type, Arg types) appeared in two
// independent switches (opContract and contractResult); drift was
// silent.
//
// This file collapses everything except the op-specific emit logic
// and the frontend lowering into a single declarative table:
// `opTable` lists one OpInfo entry per registered op, and the
// downstream consumers (OpCode.String, opContract, verify.kindOf,
// verify.contractResult, verify.readDispatchOps,
// verify.writeDispatchOps) read from it. Adding a new op now means
// adding one OpInfo literal, plus the emit cases that are genuinely
// op-specific. The init-time consistency check in this file rejects
// double-registration; the analogous check in verify rejects ops
// whose Kind contradicts their dispatch classification.
//
// Migration is incremental: ops registered here override the legacy
// switches in validate.go and verify/verify.go; ops not yet
// registered fall through to those switches unchanged. New ops
// should always go through this file, even when an analogous old
// op still lives in a switch.
package ir

// OpKind classifies how a Value comes into existence. Mirror of
// verify.ProducerKind, lifted to ir so the optable can carry the
// classification alongside the Result/Args contract. verify.kindOf
// maps these onto its public ProducerKind constants.
type OpKind uint8

const (
	// KindUnclassified is the zero value. Ops with this Kind are
	// either unregistered (the downstream verifier reads them from
	// its legacy switch) or registered with an unset Kind field
	// (a registry mistake the init-time check catches).
	KindUnclassified OpKind = iota

	// KindMove copies an existing Value without touching the arena
	// tag or generation. Phi joins and OpParam land here.
	KindMove

	// KindInline produces an inline-encoded payload (small int,
	// float, bool, inline-short string). OpConst is the only member.
	KindInline

	// KindConstructor invokes an alloc constructor that returns a
	// fresh handle. Rule A: handle-typed Values must come from
	// Constructor, Move, Inline, or Call.
	KindConstructor

	// KindOperator produces a non-handle Value (arithmetic, compare,
	// length). Result type is never a HandleType.
	KindOperator

	// KindDispatch reads (or mutates) a heap-allocated arena via an
	// existing handle. The op's first argument carries the arena
	// tag the dispatch jumps into.
	KindDispatch

	// KindCall invokes a function (Mochi-internal or Go FFI). The
	// callee is verified separately.
	KindCall

	// KindReserved is for ops the verifier intentionally treats as
	// a black box at this Phase.
	KindReserved
)

// OpInfo declares everything the IR / validator / verifier need to
// know about an op aside from its emit shape and frontend hook.
// Name backs OpCode.String(). Result + Args + NumArgs back
// opContract (and verify.contractResult). Kind drives verify.kindOf.
// Mutates is consulted only when Kind == KindDispatch; it routes
// the op into readDispatchOps (false) or writeDispatchOps (true).
type OpInfo struct {
	// Code is the OpCode this entry describes; required.
	Code OpCode
	// Name is the short debug name returned by OpCode.String().
	Name string
	// Result is the Type of the produced Value. Use TypeUnit for
	// statement-shaped ops. TypeInvalid (the zero value) signals
	// no constraint (use sparingly; the verifier won't check the
	// result type).
	Result Type
	// Args lists the expected operand Types in source-position order.
	// Positions past NumArgs must be TypeInvalid.
	Args [3]Type
	// NumArgs is the count of declared argument positions. Distinct
	// from a Type filter because some ops accept variadic Args
	// (NumArgs=0 with the operand list interpreted per-op).
	NumArgs int
	// Kind drives verify.kindOf. KindUnclassified is rejected at
	// init.
	Kind OpKind
	// Mutates is consulted only when Kind == KindDispatch; true
	// places the op in writeDispatchOps, false in readDispatchOps.
	// Ignored for other Kinds.
	Mutates bool
}

// opTable is the declarative registry. Entries here are the source
// of truth; the legacy switches in validate.go and verify/verify.go
// fall through to it. To add a new op:
//
//  1. Declare the OpCode constant in types.go.
//  2. Append an OpInfo literal here.
//  3. Add the genuinely op-specific emit case in emit/c and emit/go
//     (and frontend/lower.go for source-language plumbing).
//
// Three steps, not ten.
var opTable = []OpInfo{
	// String surface (MEP-42 Phase 4.2.x). Migrated here as the
	// proof of the registry mechanism in Phase 4.2.30.
	{Code: OpLenStr, Name: "len.str", Result: TypeI64, Args: [3]Type{TypeStr}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
	{Code: OpConcatStr, Name: "concat.str", Result: TypeStr, Args: [3]Type{TypeStr, TypeStr}, NumArgs: 2, Kind: KindConstructor},
	{Code: OpCmpEqStr, Name: "cmp.eq.str", Result: TypeBool, Args: [3]Type{TypeStr, TypeStr}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpNeStr, Name: "cmp.ne.str", Result: TypeBool, Args: [3]Type{TypeStr, TypeStr}, NumArgs: 2, Kind: KindOperator},
	{Code: OpI64ToStr, Name: "i64.to.str", Result: TypeStr, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindConstructor},
	{Code: OpF64ToStr, Name: "f64.to.str", Result: TypeStr, Args: [3]Type{TypeF64}, NumArgs: 1, Kind: KindConstructor},
	{Code: OpBoolToStr, Name: "bool.to.str", Result: TypeStr, Args: [3]Type{TypeBool}, NumArgs: 1, Kind: KindConstructor},
	{Code: OpStrCharAt, Name: "str.charat", Result: TypeStr, Args: [3]Type{TypeStr, TypeI64}, NumArgs: 2, Kind: KindConstructor},
	{Code: OpStrIn, Name: "str.in", Result: TypeBool, Args: [3]Type{TypeStr, TypeStr}, NumArgs: 2, Kind: KindDispatch, Mutates: false},
	{Code: OpStrRuneLen, Name: "str.rune.len", Result: TypeI64, Args: [3]Type{TypeStr}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
}

// opTableIndex maps OpCode to its index in opTable, or -1 if the op
// is unregistered. Built once at init() from opTable.
var opTableIndex [256]int

func init() {
	for i := range opTableIndex {
		opTableIndex[i] = -1
	}
	for i, info := range opTable {
		if info.Code == OpInvalid {
			panic("ir: opTable entry has OpCode == OpInvalid")
		}
		if info.Kind == KindUnclassified {
			panic("ir: opTable entry for " + info.Name + " has KindUnclassified; set Kind explicitly")
		}
		idx := int(info.Code)
		if idx >= len(opTableIndex) {
			panic("ir: OpCode value exceeds opTableIndex; bump the array size")
		}
		if opTableIndex[idx] != -1 {
			panic("ir: opTable double registration for " + info.Name)
		}
		opTableIndex[idx] = i
	}
}

// OpInfoOf returns the registry entry for o, or (zero, false) if o
// is not registered. Downstream consumers (validate.opContract,
// verify.kindOf, etc.) check ok first and fall back to their legacy
// switches when the op is not yet migrated.
func OpInfoOf(o OpCode) (OpInfo, bool) {
	idx := opTableIndex[uint8(o)]
	if idx < 0 {
		return OpInfo{}, false
	}
	return opTable[idx], true
}

// ReadDispatchOps returns the OpCodes of every registered op whose
// Kind == KindDispatch and Mutates == false. verify reads this to
// build its rule E classification; non-registered Dispatch ops still
// come from verify.readDispatchOps directly.
func ReadDispatchOps() []OpCode {
	var out []OpCode
	for _, info := range opTable {
		if info.Kind == KindDispatch && !info.Mutates {
			out = append(out, info.Code)
		}
	}
	return out
}

// WriteDispatchOps is the Mutates==true counterpart of ReadDispatchOps.
func WriteDispatchOps() []OpCode {
	var out []OpCode
	for _, info := range opTable {
		if info.Kind == KindDispatch && info.Mutates {
			out = append(out, info.Code)
		}
	}
	return out
}
