package ir

import (
	"fmt"
	"math"
)

// Builder is the SSA construction helper. It maintains a current
// insertion block; callers create blocks, switch between them, append
// instructions, and emit terminators.
type Builder struct {
	fn   *Function
	cur  BlockID
}

// NewBuilder starts a function with the given param and return types.
// Block 0 is created and selected as the entry block.
func NewBuilder(name string, params []Type, ret Type) *Builder {
	fn := &Function{
		Name:    name,
		Params:  append([]Type(nil), params...),
		RetType: ret,
	}
	b := &Builder{fn: fn}
	b.cur = b.NewBlock()
	fn.Entry = b.cur
	// Emit OpParam values up front; their IDs are 0..len(params)-1.
	for i, t := range params {
		_ = b.emit(Inst{Op: OpParam, Type: t, Aux: int64(i)})
	}
	return b
}

// Function returns the function under construction.
func (b *Builder) Function() *Function { return b.fn }

// NewBlock allocates a fresh block.
func (b *Builder) NewBlock() BlockID {
	id := BlockID(len(b.fn.Blocks))
	b.fn.Blocks = append(b.fn.Blocks, &Block{ID: id})
	return id
}

// SwitchTo selects the insertion block.
func (b *Builder) SwitchTo(id BlockID) { b.cur = id }

// Current returns the current insertion block id.
func (b *Builder) Current() BlockID { return b.cur }

// Param returns the SSA value for parameter i (0-indexed).
func (b *Builder) Param(i int) ValueID {
	if i < 0 || i >= len(b.fn.Params) {
		panic(fmt.Sprintf("ir: param %d out of range", i))
	}
	return ValueID(i)
}

func (b *Builder) emit(ins Inst) ValueID {
	id := ValueID(len(b.fn.Values))
	b.fn.Values = append(b.fn.Values, ins)
	b.fn.ValueBlock = append(b.fn.ValueBlock, b.cur)
	blk := b.fn.Blocks[b.cur]
	if len(blk.Insts) > 0 {
		last := b.fn.Values[blk.Insts[len(blk.Insts)-1]]
		if last.Op.IsTerminator() {
			panic(fmt.Sprintf("ir: appending to block %d after terminator", b.cur))
		}
	}
	blk.Insts = append(blk.Insts, id)
	return id
}

func (b *Builder) ConstI64(v int64) ValueID {
	return b.emit(Inst{Op: OpConstI64, Type: TI64, Aux: v})
}

func (b *Builder) ConstBool(v bool) ValueID {
	x := int64(0)
	if v {
		x = 1
	}
	return b.emit(Inst{Op: OpConstBool, Type: TBool, Aux: x})
}

func (b *Builder) AddI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpAddI64, Type: TI64, Args: []ValueID{x, y}})
}

func (b *Builder) SubI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpSubI64, Type: TI64, Args: []ValueID{x, y}})
}

func (b *Builder) MulI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpMulI64, Type: TI64, Args: []ValueID{x, y}})
}

func (b *Builder) DivI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpDivI64, Type: TI64, Args: []ValueID{x, y}})
}

func (b *Builder) ModI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpModI64, Type: TI64, Args: []ValueID{x, y}})
}

func (b *Builder) LessI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpLessI64, Type: TBool, Args: []ValueID{x, y}})
}

func (b *Builder) LessEqI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpLessEqI64, Type: TBool, Args: []ValueID{x, y}})
}

func (b *Builder) EqualI64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpEqualI64, Type: TBool, Args: []ValueID{x, y}})
}

// ConstStr returns a TStr value for the literal s, interning it into
// the function's string pool so repeated identical literals share an
// Aux index (and ultimately a single Function.StrConsts entry).
func (b *Builder) ConstStr(s string) ValueID {
	idx := -1
	for i, t := range b.fn.Strings {
		if t == s {
			idx = i
			break
		}
	}
	if idx < 0 {
		idx = len(b.fn.Strings)
		b.fn.Strings = append(b.fn.Strings, s)
	}
	return b.emit(Inst{Op: OpConstStr, Type: TStr, Aux: int64(idx)})
}

func (b *Builder) ConcatStr(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpConcatStr, Type: TStr, Args: []ValueID{x, y}})
}

func (b *Builder) LenStr(x ValueID) ValueID {
	return b.emit(Inst{Op: OpLenStr, Type: TI64, Args: []ValueID{x}})
}

func (b *Builder) IndexStr(x, i ValueID) ValueID {
	return b.emit(Inst{Op: OpIndexStr, Type: TStr, Args: []ValueID{x, i}})
}

func (b *Builder) EqualStr(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpEqualStr, Type: TBool, Args: []ValueID{x, y}})
}

func (b *Builder) HashStr(x ValueID) ValueID {
	return b.emit(Inst{Op: OpHashStr, Type: TI64, Args: []ValueID{x}})
}

// NewList allocates a fresh list with the given capacity hint.
func (b *Builder) NewList(capHint int64) ValueID {
	return b.emit(Inst{Op: OpNewList, Type: TList, Aux: capHint})
}

func (b *Builder) ListLen(l ValueID) ValueID {
	return b.emit(Inst{Op: OpListLen, Type: TI64, Args: []ValueID{l}})
}

// ListGet returns l[i]. Element type is unknown to the IR (lists are
// untyped at MVP), so the caller passes the expected element type.
func (b *Builder) ListGet(l, i ValueID, elem Type) ValueID {
	return b.emit(Inst{Op: OpListGet, Type: elem, Args: []ValueID{l, i}})
}

// ListSet writes v into l[i]. Returns the side-effect SSA value (TUnit).
func (b *Builder) ListSet(l, i, v ValueID) ValueID {
	return b.emit(Inst{Op: OpListSet, Type: TUnit, Args: []ValueID{l, i, v}})
}

// ListPush appends v to l. Returns the side-effect SSA value (TUnit).
func (b *Builder) ListPush(l, v ValueID) ValueID {
	return b.emit(Inst{Op: OpListPush, Type: TUnit, Args: []ValueID{l, v}})
}

// ListAppend models Mochi's functional `xs.append(v)`: returns a new
// list value equal to l with v pushed, leaving l unchanged in the
// general case. emit lowers this to vm2.OpListAppend; the runtime
// allocates a fresh backing array unless emit's last-use analysis
// determines l is dead after this read, in which case the runtime
// mutates l in place and returns the same pointer (MEP-36 §3.5).
func (b *Builder) ListAppend(l, v ValueID) ValueID {
	return b.emit(Inst{Op: OpListAppend, Type: TList, Args: []ValueID{l, v}})
}

// NewMap allocates a fresh empty map.
func (b *Builder) NewMap() ValueID {
	return b.emit(Inst{Op: OpNewMap, Type: TMap})
}

func (b *Builder) MapLen(m ValueID) ValueID {
	return b.emit(Inst{Op: OpMapLen, Type: TI64, Args: []ValueID{m}})
}

// MapGet returns m[k]. Value type is unknown to the IR (maps are
// untyped at MVP), so the caller passes the expected value type. A
// missing key reads back as a null Cell at runtime.
func (b *Builder) MapGet(m, k ValueID, val Type) ValueID {
	return b.emit(Inst{Op: OpMapGet, Type: val, Args: []ValueID{m, k}})
}

func (b *Builder) MapHas(m, k ValueID) ValueID {
	return b.emit(Inst{Op: OpMapHas, Type: TBool, Args: []ValueID{m, k}})
}

func (b *Builder) MapSet(m, k, v ValueID) ValueID {
	return b.emit(Inst{Op: OpMapSet, Type: TUnit, Args: []ValueID{m, k, v}})
}

func (b *Builder) MapDel(m, k ValueID) ValueID {
	return b.emit(Inst{Op: OpMapDel, Type: TUnit, Args: []ValueID{m, k}})
}

// ConstF64 emits a float64 constant. The bit pattern travels in Aux so
// the Inst stays scalar; emit reconstructs the float via
// math.Float64frombits when picking a constant-pool index.
func (b *Builder) ConstF64(v float64) ValueID {
	return b.emit(Inst{Op: OpConstF64, Type: TF64, Aux: int64(math.Float64bits(v))})
}

func (b *Builder) AddF64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpAddF64, Type: TF64, Args: []ValueID{x, y}})
}

func (b *Builder) SubF64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpSubF64, Type: TF64, Args: []ValueID{x, y}})
}

func (b *Builder) MulF64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpMulF64, Type: TF64, Args: []ValueID{x, y}})
}

func (b *Builder) DivF64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpDivF64, Type: TF64, Args: []ValueID{x, y}})
}

func (b *Builder) NegF64(x ValueID) ValueID {
	return b.emit(Inst{Op: OpNegF64, Type: TF64, Args: []ValueID{x}})
}

func (b *Builder) AbsF64(x ValueID) ValueID {
	return b.emit(Inst{Op: OpAbsF64, Type: TF64, Args: []ValueID{x}})
}

func (b *Builder) SqrtF64(x ValueID) ValueID {
	return b.emit(Inst{Op: OpSqrtF64, Type: TF64, Args: []ValueID{x}})
}

func (b *Builder) LessF64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpLessF64, Type: TBool, Args: []ValueID{x, y}})
}

func (b *Builder) LessEqF64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpLessEqF64, Type: TBool, Args: []ValueID{x, y}})
}

func (b *Builder) EqualF64(x, y ValueID) ValueID {
	return b.emit(Inst{Op: OpEqualF64, Type: TBool, Args: []ValueID{x, y}})
}

// FmaF64 emits a fused multiply-add: a*b + c with a single rounding.
func (b *Builder) FmaF64(a, c, d ValueID) ValueID {
	return b.emit(Inst{Op: OpFmaF64, Type: TF64, Args: []ValueID{a, c, d}})
}

// I64ToF64 widens an i64 SSA value to f64 (lossy for magnitudes
// exceeding 2^53; the BG kernels stay well inside that window).
func (b *Builder) I64ToF64(x ValueID) ValueID {
	return b.emit(Inst{Op: OpI64ToF64, Type: TF64, Args: []ValueID{x}})
}

// F64ToI64 truncates an f64 toward zero into an i64 SSA value.
func (b *Builder) F64ToI64(x ValueID) ValueID {
	return b.emit(Inst{Op: OpF64ToI64, Type: TI64, Args: []ValueID{x}})
}

func (b *Builder) NewF64Array(n ValueID) ValueID {
	return b.emit(Inst{Op: OpNewF64Array, Type: TF64Array, Args: []ValueID{n}})
}

func (b *Builder) F64ArrLen(a ValueID) ValueID {
	return b.emit(Inst{Op: OpF64ArrLen, Type: TI64, Args: []ValueID{a}})
}

func (b *Builder) F64ArrGet(a, i ValueID) ValueID {
	return b.emit(Inst{Op: OpF64ArrGet, Type: TF64, Args: []ValueID{a, i}})
}

func (b *Builder) F64ArrSet(a, i, v ValueID) ValueID {
	return b.emit(Inst{Op: OpF64ArrSet, Type: TUnit, Args: []ValueID{a, i, v}})
}

func (b *Builder) NewI64Array(n ValueID) ValueID {
	return b.emit(Inst{Op: OpNewI64Array, Type: TI64Array, Args: []ValueID{n}})
}

func (b *Builder) I64ArrLen(a ValueID) ValueID {
	return b.emit(Inst{Op: OpI64ArrLen, Type: TI64, Args: []ValueID{a}})
}

func (b *Builder) I64ArrGet(a, i ValueID) ValueID {
	return b.emit(Inst{Op: OpI64ArrGet, Type: TI64, Args: []ValueID{a, i}})
}

func (b *Builder) I64ArrSet(a, i, v ValueID) ValueID {
	return b.emit(Inst{Op: OpI64ArrSet, Type: TUnit, Args: []ValueID{a, i, v}})
}

func (b *Builder) NewU8Array(n ValueID) ValueID {
	return b.emit(Inst{Op: OpNewU8Array, Type: TU8Array, Args: []ValueID{n}})
}

func (b *Builder) U8ArrLen(a ValueID) ValueID {
	return b.emit(Inst{Op: OpU8ArrLen, Type: TI64, Args: []ValueID{a}})
}

func (b *Builder) U8ArrGet(a, i ValueID) ValueID {
	return b.emit(Inst{Op: OpU8ArrGet, Type: TI64, Args: []ValueID{a, i}})
}

func (b *Builder) U8ArrSet(a, i, v ValueID) ValueID {
	return b.emit(Inst{Op: OpU8ArrSet, Type: TUnit, Args: []ValueID{a, i, v}})
}

// NewPair allocates a fresh vmPair carrying (fst, snd). Result is TPair.
// Element type is unrestricted (Cell); the runtime treats both slots as
// opaque cells, so a pair can carry an i64 in one slot and another pair
// in the other, which is exactly the shape binary_trees needs (leaf
// pairs hold two i64s, branch pairs hold two TPair children).
func (b *Builder) NewPair(fst, snd ValueID) ValueID {
	return b.emit(Inst{Op: OpNewPair, Type: TPair, Args: []ValueID{fst, snd}})
}

// PairFst reads the first slot of a TPair. Caller supplies the expected
// element type because the pair itself is Cell-typed at the IR.
func (b *Builder) PairFst(p ValueID, elem Type) ValueID {
	return b.emit(Inst{Op: OpPairFst, Type: elem, Args: []ValueID{p}})
}

// PairSnd reads the second slot of a TPair.
func (b *Builder) PairSnd(p ValueID, elem Type) ValueID {
	return b.emit(Inst{Op: OpPairSnd, Type: elem, Args: []ValueID{p}})
}

// Call invokes function at funcIdx with the given args. retType is the
// caller-supplied result type; the verifier later checks it against
// the callee's signature once Module is assembled.
func (b *Builder) Call(funcIdx int, retType Type, args ...ValueID) ValueID {
	a := append([]ValueID(nil), args...)
	return b.emit(Inst{Op: OpCall, Type: retType, Args: a, Aux: int64(funcIdx)})
}

// Ret terminates the current block, returning v. For TUnit functions
// pass -1 (no value).
func (b *Builder) Ret(v ValueID) {
	var args []ValueID
	if v >= 0 {
		args = []ValueID{v}
	}
	b.emit(Inst{Op: OpRet, Type: TUnit, Args: args})
}

// Br terminates the current block, jumping to target.
func (b *Builder) Br(target BlockID) {
	b.emit(Inst{Op: OpBr, Type: TUnit, AuxBlocks: []BlockID{target}})
}

// CondBr terminates the current block, branching to then/else on cond.
func (b *Builder) CondBr(cond ValueID, then, els BlockID) {
	b.emit(Inst{Op: OpCondBr, Type: TUnit, Args: []ValueID{cond}, AuxBlocks: []BlockID{then, els}})
}

// Phi emits an SSA phi at the head of the current block. Pairs of
// (predecessor block, value) describe each incoming edge.
func (b *Builder) Phi(t Type, edges ...PhiEdge) ValueID {
	args := make([]ValueID, len(edges))
	preds := make([]BlockID, len(edges))
	for i, e := range edges {
		args[i] = e.Value
		preds[i] = e.From
	}
	return b.emit(Inst{Op: OpPhi, Type: t, Args: args, AuxBlocks: preds})
}

// PhiEdge is one incoming edge of a phi.
type PhiEdge struct {
	From  BlockID
	Value ValueID
}
