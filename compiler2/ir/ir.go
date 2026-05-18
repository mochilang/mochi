// Package ir is the typed SSA IR produced by compiler2's front end and
// consumed by the optimization, register allocation, and emit packages.
//
// Contract: MEP-21 v2. Values are typed. Blocks end with one of the
// terminator ops (OpRet, OpBr, OpCondBr). Phi nodes appear only at the
// head of a block.
package ir

// Type is the IR-level type. The set is intentionally small for step 4
// and grows with the typed-op table in MEP-21 v2 (float, string, list,
// map, struct, closure).
type Type uint8

const (
	TUnit Type = iota
	TI64
	TF64
	TBool
	TPtr
	TStr
	TList
	TMap
	// Typed array kinds (MEP-37 §3.3). Each is a separate IR type so
	// the verifier can pick the matching element-typed Get/Set op.
	TF64Array
	TI64Array
	TU8Array
	// Pair (MEP-37 §3.4). Packed two-element immutable tuple. Cell.Obj
	// holds a *vmPair allocated through the per-VM arena; the runtime's
	// monotonic-per-VM lifetime means the pointer never dangles.
	TPair
	// Byte view (MEP-38 §3.1). Cell.Obj holds a *vmBytes carrying
	// (buf, off, n, owns); writable iff produced by OpBytesNew or
	// OpStdinReadAll, read-only via OpBytesSlice / OpBytesFromU8Array
	// / OpBytesFromStr.
	TBytes
	// Bignum (MEP-39 §4.2.3). Arbitrary-precision signed integers
	// backed by *big.Int through Cell.Obj. Distinct from TI64 so the
	// builder dispatches OpAddBigInt vs OpAddI64 without operand
	// inspection at runtime; the verifier rejects mixed-width arithmetic.
	TBigInt
)

func (t Type) String() string {
	switch t {
	case TUnit:
		return "unit"
	case TI64:
		return "i64"
	case TF64:
		return "f64"
	case TBool:
		return "bool"
	case TPtr:
		return "ptr"
	case TStr:
		return "str"
	case TList:
		return "list"
	case TMap:
		return "map"
	case TF64Array:
		return "f64array"
	case TI64Array:
		return "i64array"
	case TU8Array:
		return "u8array"
	case TPair:
		return "pair"
	case TBytes:
		return "bytes"
	case TBigInt:
		return "bigint"
	}
	return "?"
}

// ValueID identifies an SSA value within a Function.
type ValueID int32

// BlockID identifies a basic block within a Function.
type BlockID int32

// Op enumerates IR opcodes. Order is significant only as a stable
// debugging aid.
type Op uint8

const (
	OpInvalid Op = iota

	// Pure: produce a typed value, no side effects.
	OpParam     // Aux = parameter index
	OpConstI64  // Aux = constant value
	OpConstBool // Aux = 0 or 1
	OpAddI64
	OpSubI64
	OpMulI64
	OpDivI64
	OpModI64
	OpLessI64
	OpLessEqI64
	OpEqualI64

	// String subsystem (MEP-24 §2). OpConstStr's Aux is the index into
	// the function-local string constant pool the builder maintains; the
	// emitter merges those into Function.StrConsts. OpIndexStr may trap
	// on OOB so it is not treated as pure by DCE.
	OpConstStr  // Aux = idx into Function.Strings
	OpConcatStr // Args[0] ++ Args[1]
	OpLenStr    // len(Args[0])
	OpIndexStr  // Args[0][Args[1]]
	OpEqualStr  // Args[0] == Args[1]
	OpHashStr   // hash(Args[0])

	// List subsystem (MEP-24 §3). Lists are reference-typed; the
	// allocating ops produce a fresh TList value. ListGet may trap on
	// OOB (not pure); ListSet/ListPush mutate and are never pure.
	OpNewList  // Aux = capacity hint
	OpListLen  // len(Args[0])
	OpListGet  // Args[0][Args[1]]
	OpListSet  // Args[0][Args[1]] = Args[2]; result type TUnit
	OpListPush // Args[0].push(Args[1]); result type TUnit
	// OpListAppend models Mochi's functional append: result is a new
	// TList equal to Args[0] with Args[1] pushed; Args[0] is unchanged.
	// Without copy-elision the implementation allocates a fresh backing
	// array. The emitter consults the regalloc last-use map: when
	// Args[0] is at last use, it sets the MEP-36 Phase 3c last-use bit
	// on operand B and the runtime mutates the source in place, returning
	// the same pointer. This is the IR's anchor for the spec's "fluent
	// chain captures in-place" claim ([§3.5](#35-compile-time-last-use-bit)).
	OpListAppend

	// Map subsystem (MEP-24 §4). Maps are reference-typed; OpNewMap
	// produces a fresh TMap value. OpMapGet returns the value type (or
	// TUnit/null sentinel when callers do not care); OpMapSet/OpMapDel
	// mutate and have TUnit result.
	OpNewMap // (no args)
	OpMapLen // len(Args[0])
	OpMapGet // Args[0][Args[1]]; missing key -> null cell
	OpMapHas // bool(present(Args[0], Args[1]))
	OpMapSet // Args[0][Args[1]] = Args[2]; TUnit
	OpMapDel // delete Args[0][Args[1]]; TUnit

	// Float arithmetic subsystem (MEP-37 §3.2). OpConstF64 carries the
	// float bit-pattern in Aux (read back via math.Float64frombits in
	// emit so the constant pool stores a single CFloat cell).
	OpConstF64
	OpAddF64
	OpSubF64
	OpMulF64
	OpDivF64
	OpNegF64
	OpAbsF64
	OpSqrtF64
	OpLessF64
	OpLessEqF64
	OpEqualF64
	// OpFmaF64 computes Args[0]*Args[1] + Args[2] with a single rounding
	// (math.FMA in the runtime). Lowered by emit when a mul has exactly
	// one use and that use is the matching add; the builder also exposes
	// a direct constructor for IR producers that already know the
	// pattern (Mandelbrot's iteration).
	OpFmaF64
	// Numeric conversions; see vm2/ops.go for the truncation contract.
	OpI64ToF64
	OpF64ToI64

	// Typed array subsystem (MEP-37 §3.3). The allocator opcode takes a
	// length argument; element ops take an array and an int index. The
	// verifier checks that Get/Set are dispatched against the matching
	// array kind so a TF64Array.Set with an i64 value is a build-time
	// error.
	OpNewF64Array // Args[0] = length
	OpF64ArrLen   // len(Args[0])
	OpF64ArrGet   // Args[0][Args[1]] -> TF64
	OpF64ArrSet   // Args[0][Args[1]] = Args[2]; TUnit
	OpNewI64Array
	OpI64ArrLen
	OpI64ArrGet
	OpI64ArrSet
	OpNewU8Array
	OpU8ArrLen
	OpU8ArrGet // result type TI64 (byte widened to int)
	OpU8ArrSet // Args[2] is a TI64 value; runtime truncates to byte

	// Bulk byte-array super-ops (MEP-39 §6.5 iter 7, §6.7 iter 2). Each
	// collapses a per-byte / per-iter interpreter loop into one dispatch
	// that runs the loop natively in the runtime.
	OpU8FillACGT             // Args[0]=dst (TU8Array), Args[1]=n (TI64); dst[i] = "ACGT"[i&3] for i in [0,n); TUnit
	OpU8ReverseComplementDNA // Args[0]=src, Args[1]=dst, Args[2]=n; dst[n-1-i] = compDNA(src[i]) for i in [0,n); TUnit
	OpU8SumI64               // Args[0]=arr, Args[1]=n; returns sum(arr[0:n]) as TI64
	OpKNucleotideRun         // Args[0]=counts (TI64Array length>=20), Args[1]=n; bakes in canonical LCG (seed=42, *3877+29573 %139968) and HOMO_SAPIENS cumprob cascade; TUnit

	// Pair subsystem (MEP-37 §3.4). Packed two-element tuples backed by
	// the per-VM vmPair arena. Construction is one OpNewPair; reads are
	// pure (no traps), so DCE may drop them; the constructor is treated
	// as opaque because the arena allocator is observable.
	OpNewPair // Args[0]=fst, Args[1]=snd -> TPair
	OpPairFst // Args[0] -> Cell at the .a slot
	OpPairSnd // Args[0] -> Cell at the .b slot

	// Byte-view subsystem (MEP-38 §3.1). The view type is TBytes; ops
	// constructed against any other type are a verifier error. OpBytesSet
	// is only legal at runtime against an owning view (§3.1.4); the
	// IR-level verifier admits it on any TBytes, the runtime traps.
	OpBytesNew         // Args[0] = length (TI64) -> TBytes
	OpBytesLen         // len(Args[0])
	OpBytesGet         // Args[0][Args[1]] -> TI64
	OpBytesSet         // Args[0][Args[1]] = Args[2]; TUnit
	OpBytesSlice       // Args[0][Args[1] : Args[1]+Args[2]] -> TBytes
	OpBytesEqual       // Args[0] == Args[1] -> TBool
	OpBytesHash        // hash(Args[0]) -> TI64
	OpBytesFromU8Array // Args[0] -> TBytes view (read-only)
	OpBytesFromStr     // Args[0] -> TBytes view (read-only)
	OpStdoutWriteBytes // write Args[0]; TUnit
	OpStdinReadAll     // (no args) -> TBytes

	// Bignum subsystem (MEP-39 §4.2.3). Constants live in
	// Function.BigInts (decimal strings, deduplicated); OpConstBigInt's
	// Aux is the index into that pool. Emit parses the string into a
	// *big.Int once and pre-boxes the resulting Cell into Program.Consts
	// so OpConstBigInt lowers to an OpLoadConstI in vm2 (no new
	// const-load opcode). Arithmetic ops produce a fresh TBigInt (no
	// in-place mutation today). OpI64ToBigInt widens an i64 loop
	// counter; OpBigIntToStr formats in base-10 for print/io.
	OpConstBigInt  // Aux = idx into Function.BigInts
	OpAddBigInt    // Args[0] + Args[1] -> TBigInt
	OpSubBigInt    // Args[0] - Args[1] -> TBigInt
	OpMulBigInt    // Args[0] * Args[1] -> TBigInt
	OpDivBigInt    // Args[0] / Args[1] -> TBigInt; div by zero traps
	OpModBigInt    // Args[0] % Args[1] -> TBigInt; mod by zero traps
	OpLessBigInt   // Args[0] < Args[1] -> TBool
	OpEqualBigInt  // Args[0] == Args[1] -> TBool
	OpI64ToBigInt  // Args[0] (TI64) -> TBigInt
	OpBigIntToI64  // Args[0] (TBigInt) -> TI64 (low 64 bits; pidigits' digit candidate)
	OpBigIntToStr  // Args[0] (TBigInt) -> TStr

	// Call: Aux = function index, Args = arg values. May have effects;
	// optimizers must treat as opaque.
	OpCall

	// Terminators: must appear once and only at end of block.
	OpRet      // Args[0] = value (or none for TUnit)
	OpBr       // AuxBlocks[0] = target
	OpCondBr   // Args[0] = cond, AuxBlocks[0] = then, AuxBlocks[1] = else
	OpTailCall // tail call. Aux = function index, Args = arg values; transfers control without growing the frame stack.

	// Phi: must appear at head of block. Args[i] pairs with AuxBlocks[i].
	OpPhi
)

// Inst is a single SSA instruction. Args/AuxBlocks are op-specific.
type Inst struct {
	Op        Op
	Type      Type
	Args      []ValueID
	AuxBlocks []BlockID
	Aux       int64
}

// IsTerminator reports whether the op ends a block.
func (op Op) IsTerminator() bool {
	switch op {
	case OpRet, OpBr, OpCondBr, OpTailCall:
		return true
	}
	return false
}

// Block is a basic block in SSA form.
type Block struct {
	ID    BlockID
	Insts []ValueID // value IDs in execution order; last one is the terminator
}

// Function is a compiled function in IR form.
type Function struct {
	Name       string
	Params     []Type
	RetType    Type
	Blocks     []*Block
	Entry      BlockID
	Values     []Inst // indexed by ValueID
	ValueBlock []BlockID
	// Strings is the per-function string pool referenced by OpConstStr.
	// Aux on an OpConstStr is an index into this slice. The pool is
	// deduplicated by the builder; emit forwards it to vm2.Function.StrConsts.
	Strings []string
	// BigInts is the per-function bignum constant pool referenced by
	// OpConstBigInt. Each entry is a base-10 decimal string (so the IR
	// stays trivially serializable). Emit parses each entry into a
	// *big.Int once, boxes it via vm2.CBigInt, and stores the resulting
	// Cell in Program.Consts; OpConstBigInt lowers to OpLoadConstI with
	// that Consts index. The pool is deduplicated by the builder.
	BigInts []string
}

// NumValues returns the count of SSA values defined in the function.
func (f *Function) NumValues() int { return len(f.Values) }

// Module is a collection of functions referenced by index.
type Module struct {
	Funcs []*Function
	Main  int
}
