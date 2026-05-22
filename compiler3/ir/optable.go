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

	// Scalar arithmetic, comparison, bitwise, conversion, and math
	// surface (MEP-42 Phase 4.2.31). Migrated from the legacy switches
	// in ir/types.go (String), ir/validate.go (opContract), and
	// verify/verify.go (kindOf, contractResult) to the registry as the
	// next batch after the string surface (Phase 4.2.30).
	//
	// All entries are KindOperator: the result Type is never a handle
	// Type, and the op does not dispatch into a heap-allocated arena.
	{Code: OpAddI64, Name: "add.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpSubI64, Name: "sub.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpMulI64, Name: "mul.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpDivI64, Name: "div.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpModI64, Name: "mod.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpNegI64, Name: "neg.i64", Result: TypeI64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},

	{Code: OpAddI64Imm, Name: "addk.i64", Result: TypeI64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpSubI64Imm, Name: "subk.i64", Result: TypeI64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpMulI64Imm, Name: "mulk.i64", Result: TypeI64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpDivI64Imm, Name: "divk.i64", Result: TypeI64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpModI64Imm, Name: "modk.i64", Result: TypeI64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},

	{Code: OpAddF64, Name: "add.f64", Result: TypeF64, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpSubF64, Name: "sub.f64", Result: TypeF64, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpMulF64, Name: "mul.f64", Result: TypeF64, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpDivF64, Name: "div.f64", Result: TypeF64, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpNegF64, Name: "neg.f64", Result: TypeF64, Args: [3]Type{TypeF64}, NumArgs: 1, Kind: KindOperator},

	{Code: OpCmpEqI64, Name: "cmp.eq.i64", Result: TypeBool, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpNeI64, Name: "cmp.ne.i64", Result: TypeBool, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpLtI64, Name: "cmp.lt.i64", Result: TypeBool, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpLeI64, Name: "cmp.le.i64", Result: TypeBool, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpGtI64, Name: "cmp.gt.i64", Result: TypeBool, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpGeI64, Name: "cmp.ge.i64", Result: TypeBool, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},

	{Code: OpCmpEqI64Imm, Name: "cmp.eq.i64.imm", Result: TypeBool, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpCmpNeI64Imm, Name: "cmp.ne.i64.imm", Result: TypeBool, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpCmpLtI64Imm, Name: "cmp.lt.i64.imm", Result: TypeBool, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpCmpLeI64Imm, Name: "cmp.le.i64.imm", Result: TypeBool, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpCmpGtI64Imm, Name: "cmp.gt.i64.imm", Result: TypeBool, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpCmpGeI64Imm, Name: "cmp.ge.i64.imm", Result: TypeBool, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},

	{Code: OpCmpEqF64, Name: "cmp.eq.f64", Result: TypeBool, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpNeF64, Name: "cmp.ne.f64", Result: TypeBool, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpLtF64, Name: "cmp.lt.f64", Result: TypeBool, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpLeF64, Name: "cmp.le.f64", Result: TypeBool, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpGtF64, Name: "cmp.gt.f64", Result: TypeBool, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpGeF64, Name: "cmp.ge.f64", Result: TypeBool, Args: [3]Type{TypeF64, TypeF64}, NumArgs: 2, Kind: KindOperator},

	{Code: OpCmpEqBool, Name: "cmp.eq.bool", Result: TypeBool, Args: [3]Type{TypeBool, TypeBool}, NumArgs: 2, Kind: KindOperator},
	{Code: OpCmpNeBool, Name: "cmp.ne.bool", Result: TypeBool, Args: [3]Type{TypeBool, TypeBool}, NumArgs: 2, Kind: KindOperator},
	{Code: OpAndBool, Name: "and.bool", Result: TypeBool, Args: [3]Type{TypeBool, TypeBool}, NumArgs: 2, Kind: KindOperator},
	{Code: OpOrBool, Name: "or.bool", Result: TypeBool, Args: [3]Type{TypeBool, TypeBool}, NumArgs: 2, Kind: KindOperator},
	{Code: OpNotBool, Name: "not.bool", Result: TypeBool, Args: [3]Type{TypeBool}, NumArgs: 1, Kind: KindOperator},

	{Code: OpAndI64, Name: "and.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpOrI64, Name: "or.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpXorI64, Name: "xor.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpShlI64, Name: "shl.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpShrI64, Name: "shr.i64", Result: TypeI64, Args: [3]Type{TypeI64, TypeI64}, NumArgs: 2, Kind: KindOperator},
	{Code: OpNotI64, Name: "not.i64", Result: TypeI64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},

	{Code: OpI64ToF64, Name: "i64.to.f64", Result: TypeF64, Args: [3]Type{TypeI64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpF64ToI64, Name: "f64.to.i64", Result: TypeI64, Args: [3]Type{TypeF64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpSqrtF64, Name: "sqrt.f64", Result: TypeF64, Args: [3]Type{TypeF64}, NumArgs: 1, Kind: KindOperator},
	{Code: OpNow, Name: "now", Result: TypeI64, Args: [3]Type{}, NumArgs: 0, Kind: KindOperator},

	// Heap-allocating surface (MEP-42 Phase 4.2.32). Every op here
	// either produces a handle Type (Constructor) or operates on an
	// existing handle (Dispatch). The Dispatch reads / writes split
	// drives rule E classification (Mutates flag).
	//
	// List(I64) family.
	{Code: OpNewList, Name: "newlist", Result: TypeList, Args: [3]Type{}, NumArgs: 0, Kind: KindConstructor},
	{Code: OpListLenI64, Name: "list.len.i64", Result: TypeI64, Args: [3]Type{TypeList}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
	{Code: OpListPushI64, Name: "list.push.i64", Result: TypeUnit, Args: [3]Type{TypeList, TypeI64}, NumArgs: 2, Kind: KindDispatch, Mutates: true},
	{Code: OpListGetI64, Name: "list.get.i64", Result: TypeI64, Args: [3]Type{TypeList, TypeI64}, NumArgs: 2, Kind: KindDispatch, Mutates: false},
	{Code: OpListSetI64, Name: "list.set.i64", Result: TypeUnit, Args: [3]Type{TypeList, TypeI64, TypeI64}, NumArgs: 3, Kind: KindDispatch, Mutates: true},
	{Code: OpListGetF64, Name: "list.get.f64", Result: TypeF64, Args: [3]Type{TypeList, TypeI64}, NumArgs: 2, Kind: KindDispatch, Mutates: false},
	{Code: OpListSetF64, Name: "list.set.f64", Result: TypeUnit, Args: [3]Type{TypeList, TypeI64, TypeF64}, NumArgs: 3, Kind: KindDispatch, Mutates: true},
	{Code: OpListConcatI64, Name: "list.concat.i64", Result: TypeList, Args: [3]Type{TypeList, TypeList}, NumArgs: 2, Kind: KindConstructor},
	{Code: OpListI64ToStr, Name: "list.i64.tostr", Result: TypeStr, Args: [3]Type{TypeList}, NumArgs: 1, Kind: KindConstructor},

	// Map(I64,I64) family.
	{Code: OpNewMap, Name: "newmap", Result: TypeMap, Args: [3]Type{}, NumArgs: 0, Kind: KindConstructor},
	{Code: OpMapSetI64I64, Name: "map.set.i64.i64", Result: TypeUnit, Args: [3]Type{TypeMap, TypeI64, TypeI64}, NumArgs: 3, Kind: KindDispatch, Mutates: true},
	{Code: OpMapGetI64I64, Name: "map.get.i64.i64", Result: TypeI64, Args: [3]Type{TypeMap, TypeI64}, NumArgs: 2, Kind: KindDispatch, Mutates: false},

	// F64 array family.
	{Code: OpNewF64Array, Name: "newf64array", Result: TypeF64Arr, Args: [3]Type{}, NumArgs: 0, Kind: KindConstructor},
	{Code: OpF64ArrayLenI64, Name: "f64arr.len.i64", Result: TypeI64, Args: [3]Type{TypeF64Arr}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
	{Code: OpF64ArrayPushF64, Name: "f64arr.push.f64", Result: TypeUnit, Args: [3]Type{TypeF64Arr, TypeF64}, NumArgs: 2, Kind: KindDispatch, Mutates: true},
	{Code: OpF64ArrayGetF64, Name: "f64arr.get.f64", Result: TypeF64, Args: [3]Type{TypeF64Arr, TypeI64}, NumArgs: 2, Kind: KindDispatch, Mutates: false},
	{Code: OpF64ArraySetF64, Name: "f64arr.set.f64", Result: TypeUnit, Args: [3]Type{TypeF64Arr, TypeI64, TypeF64}, NumArgs: 3, Kind: KindDispatch, Mutates: true},
	{Code: OpF64ArrayConcat, Name: "f64arr.concat", Result: TypeF64Arr, Args: [3]Type{TypeF64Arr, TypeF64Arr}, NumArgs: 2, Kind: KindConstructor},
	{Code: OpF64ArrayToStr, Name: "f64array.tostr", Result: TypeStr, Args: [3]Type{TypeF64Arr}, NumArgs: 1, Kind: KindConstructor},

	// String array family. OpStrArrGetStr is Constructor: it returns a
	// handle (TypeStr) borrowed from a const char** slot; rule A requires
	// handle-typed Values to originate from a Constructor / Move /
	// Inline / Call kind.
	{Code: OpNewStrArr, Name: "newstrarr", Result: TypeStrArr, Args: [3]Type{}, NumArgs: 0, Kind: KindConstructor},
	{Code: OpStrArrLen, Name: "strarr.len", Result: TypeI64, Args: [3]Type{TypeStrArr}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
	{Code: OpStrArrPushStr, Name: "strarr.push.str", Result: TypeUnit, Args: [3]Type{TypeStrArr, TypeStr}, NumArgs: 2, Kind: KindDispatch, Mutates: true},
	{Code: OpStrArrGetStr, Name: "strarr.get.str", Result: TypeStr, Args: [3]Type{TypeStrArr, TypeI64}, NumArgs: 2, Kind: KindConstructor},
	{Code: OpStrArrSetStr, Name: "strarr.set.str", Result: TypeUnit, Args: [3]Type{TypeStrArr, TypeI64, TypeStr}, NumArgs: 3, Kind: KindDispatch, Mutates: true},
	{Code: OpStrArrSlice, Name: "strarr.slice", Result: TypeStrArr, Args: [3]Type{TypeStrArr, TypeI64, TypeI64}, NumArgs: 3, Kind: KindConstructor},
	{Code: OpStrArrToStr, Name: "strarr.tostr", Result: TypeStr, Args: [3]Type{TypeStrArr}, NumArgs: 1, Kind: KindConstructor},

	// Map(Str,I64) family. OpMapStrI64SortedKeys is Constructor: it
	// allocates a TypeStrArr handle.
	{Code: OpNewMapStrI64, Name: "newmapstri64", Result: TypeMapStrI64, Args: [3]Type{}, NumArgs: 0, Kind: KindConstructor},
	{Code: OpMapSetStrI64, Name: "map.set.str.i64", Result: TypeUnit, Args: [3]Type{TypeMapStrI64, TypeStr, TypeI64}, NumArgs: 3, Kind: KindDispatch, Mutates: true},
	{Code: OpMapGetStrI64, Name: "map.get.str.i64", Result: TypeI64, Args: [3]Type{TypeMapStrI64, TypeStr}, NumArgs: 2, Kind: KindDispatch, Mutates: false},
	{Code: OpMapLenStrI64, Name: "map.len.str.i64", Result: TypeI64, Args: [3]Type{TypeMapStrI64}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
	{Code: OpMapStrI64SortedKeys, Name: "map.str.i64.sortedkeys", Result: TypeStrArr, Args: [3]Type{TypeMapStrI64}, NumArgs: 1, Kind: KindConstructor},

	// ListList family. OpListListGet returns a TypeList handle; it is a
	// Constructor for rule A purposes (same rationale as OpStrArrGetStr).
	{Code: OpNewListList, Name: "newlistlist", Result: TypeListList, Args: [3]Type{}, NumArgs: 0, Kind: KindConstructor},
	{Code: OpListListPush, Name: "listlist.push", Result: TypeUnit, Args: [3]Type{TypeListList, TypeList}, NumArgs: 2, Kind: KindDispatch, Mutates: true},
	{Code: OpListListGet, Name: "listlist.get", Result: TypeList, Args: [3]Type{TypeListList, TypeI64}, NumArgs: 2, Kind: KindConstructor},
	{Code: OpListListLen, Name: "listlist.len", Result: TypeI64, Args: [3]Type{TypeListList}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
	{Code: OpListListToStr, Name: "listlist.tostr", Result: TypeStr, Args: [3]Type{TypeListList}, NumArgs: 1, Kind: KindConstructor},

	// ListAny family. OpListAnyGetAny returns a TypeListAny handle;
	// Constructor for the same handle-origin reason.
	{Code: OpNewListAny, Name: "newlistany", Result: TypeListAny, Args: [3]Type{}, NumArgs: 0, Kind: KindConstructor},
	{Code: OpListAnyLen, Name: "listany.len", Result: TypeI64, Args: [3]Type{TypeListAny}, NumArgs: 1, Kind: KindDispatch, Mutates: false},
	{Code: OpListAnyPushAny, Name: "listany.push", Result: TypeUnit, Args: [3]Type{TypeListAny, TypeListAny}, NumArgs: 2, Kind: KindDispatch, Mutates: true},
	{Code: OpListAnyGetAny, Name: "listany.get", Result: TypeListAny, Args: [3]Type{TypeListAny, TypeI64}, NumArgs: 2, Kind: KindConstructor},
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
