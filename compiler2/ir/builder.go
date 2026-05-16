package ir

import "fmt"

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
