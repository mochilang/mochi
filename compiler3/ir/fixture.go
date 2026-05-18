package ir

// Hand-built IR fixtures, one per corpus kernel. Phase 4.2 (opt
// passes), Phase 4.3 (regalloc), and Phase 4.4 (emit) consume these
// as their golden inputs. Each fixture must Validate() cleanly.
//
// Naming: FixtureFooBar returns the IR for compiler3/corpus.FooBar.
// The IR shape matches the SSA form a future Phase 4.5 typed AST
// frontend would emit (modulo the temp names).

// FixtureFibIter builds the SSA IR for the iterative Fibonacci
// kernel. Mochi source:
//
//	fun fib(n) {
//	  var a = 0
//	  var b = 1
//	  for i in range(n) {
//	    var t = a + b
//	    a = b
//	    b = t
//	  }
//	  return a
//	}
//
// SSA blocks: entry, loop_head (phi join), loop_body, exit. The loop
// induction variable and the (a, b) pair join at loop_head via phis.
func FixtureFibIter() *Function {
	fn := &Function{Name: "fib_iter", Result: TypeI64}

	// Param: n (i64).
	nID := fn.AddValue(Value{Type: TypeI64, Op: OpParam})
	fn.Params = []uint32{nID}

	// Allocate all blocks up front so block pointers obtained via
	// fn.Block remain stable through the rest of the build.
	entryID := fn.AddBlock()
	headID := fn.AddBlock()
	bodyID := fn.AddBlock()
	exitID := fn.AddBlock()

	a0 := fn.AddValue(Value{Type: TypeI64, Op: OpConst, Const: 0})
	b0 := fn.AddValue(Value{Type: TypeI64, Op: OpConst, Const: 1})
	i0 := fn.AddValue(Value{Type: TypeI64, Op: OpConst, Const: 0})
	entry := fn.Block(entryID)
	entry.Values = []uint32{a0, b0, i0}
	entry.Term = Terminator{Kind: TermJump, Target: headID}
	entry.Succs = []uint32{headID}

	// Phis at loop head: (a, b, i) join entry and loop_body.
	// Placeholder srcID 0 on the back-edge slot; patched once the
	// loop_body values exist.
	aPhi := fn.AddValue(Value{Type: TypeI64, Op: OpPhi, Args: []uint32{entryID, a0, bodyID, 0}})
	bPhi := fn.AddValue(Value{Type: TypeI64, Op: OpPhi, Args: []uint32{entryID, b0, bodyID, 0}})
	iPhi := fn.AddValue(Value{Type: TypeI64, Op: OpPhi, Args: []uint32{entryID, i0, bodyID, 0}})
	cond := fn.AddValue(Value{Type: TypeBool, Op: OpCmpGeI64, Args: []uint32{iPhi, nID}})
	head := fn.Block(headID)
	head.Preds = []uint32{entryID, bodyID}
	head.Values = []uint32{aPhi, bPhi, iPhi, cond}
	head.Term = Terminator{Kind: TermBranch, Value: cond, IfTrue: exitID, IfFalse: bodyID}
	head.Succs = []uint32{exitID, bodyID}

	// Loop body: t = a + b; a' = b; b' = t; i' = i + 1.
	// Mochi assigns a := b, b := t. In SSA these are renames, not
	// new values, so we feed b back as aPhi's new incoming and t as
	// bPhi's new incoming. No separate Value needed.
	t := fn.AddValue(Value{Type: TypeI64, Op: OpAddI64, Args: []uint32{aPhi, bPhi}})
	iNext := fn.AddValue(Value{Type: TypeI64, Op: OpAddI64Imm, Args: []uint32{iPhi}, Const: 1})
	body := fn.Block(bodyID)
	body.Values = []uint32{t, iNext}
	body.Preds = []uint32{headID}
	body.Term = Terminator{Kind: TermJump, Target: headID}
	body.Succs = []uint32{headID}

	// Patch phi back-edge sources now that body values exist.
	fn.Values[aPhi].Args[3] = bPhi  // a' = b
	fn.Values[bPhi].Args[3] = t     // b' = t
	fn.Values[iPhi].Args[3] = iNext // i' = i+1

	exit := fn.Block(exitID)
	exit.Preds = []uint32{headID}
	exit.Term = Terminator{Kind: TermReturn, Value: aPhi}
	return fn
}

// FixtureSumLoop builds the SSA IR for the sum_loop kernel: sum of
// [0, n). Source:
//
//	fun sum_loop(n) {
//	  var s = 0
//	  for i in range(n) {
//	    s = s + i
//	  }
//	  return s
//	}
func FixtureSumLoop() *Function {
	fn := &Function{Name: "sum_loop", Result: TypeI64}
	nID := fn.AddValue(Value{Type: TypeI64, Op: OpParam})
	fn.Params = []uint32{nID}

	entryID := fn.AddBlock()
	headID := fn.AddBlock()
	bodyID := fn.AddBlock()
	exitID := fn.AddBlock()

	s0 := fn.AddValue(Value{Type: TypeI64, Op: OpConst, Const: 0})
	i0 := fn.AddValue(Value{Type: TypeI64, Op: OpConst, Const: 0})
	entry := fn.Block(entryID)
	entry.Values = []uint32{s0, i0}
	entry.Term = Terminator{Kind: TermJump, Target: headID}
	entry.Succs = []uint32{headID}

	sPhi := fn.AddValue(Value{Type: TypeI64, Op: OpPhi, Args: []uint32{entryID, s0, bodyID, 0}})
	iPhi := fn.AddValue(Value{Type: TypeI64, Op: OpPhi, Args: []uint32{entryID, i0, bodyID, 0}})
	cond := fn.AddValue(Value{Type: TypeBool, Op: OpCmpGeI64, Args: []uint32{iPhi, nID}})
	head := fn.Block(headID)
	head.Preds = []uint32{entryID, bodyID}
	head.Values = []uint32{sPhi, iPhi, cond}
	head.Term = Terminator{Kind: TermBranch, Value: cond, IfTrue: exitID, IfFalse: bodyID}
	head.Succs = []uint32{exitID, bodyID}

	sNext := fn.AddValue(Value{Type: TypeI64, Op: OpAddI64, Args: []uint32{sPhi, iPhi}})
	iNext := fn.AddValue(Value{Type: TypeI64, Op: OpAddI64Imm, Args: []uint32{iPhi}, Const: 1})
	body := fn.Block(bodyID)
	body.Values = []uint32{sNext, iNext}
	body.Preds = []uint32{headID}
	body.Term = Terminator{Kind: TermJump, Target: headID}
	body.Succs = []uint32{headID}

	fn.Values[sPhi].Args[3] = sNext
	fn.Values[iPhi].Args[3] = iNext

	exit := fn.Block(exitID)
	exit.Preds = []uint32{headID}
	exit.Term = Terminator{Kind: TermReturn, Value: sPhi}
	return fn
}

// FixtureFactRec builds the SSA IR for the recursive factorial
// kernel. Source:
//
//	fun fact_rec(n) {
//	  if n <= 1 return 1
//	  return n * fact_rec(n - 1)
//	}
//
// Value.Const on the OpCall carries the callee index 0 (the function
// calls itself). The validator's opContract is silent on OpCall;
// emit and regalloc handle it via Args layout.
func FixtureFactRec() *Function {
	fn := &Function{Name: "fact_rec", Result: TypeI64}
	nID := fn.AddValue(Value{Type: TypeI64, Op: OpParam})
	fn.Params = []uint32{nID}

	entryID := fn.AddBlock()
	baseID := fn.AddBlock()
	recID := fn.AddBlock()

	cond := fn.AddValue(Value{Type: TypeBool, Op: OpCmpLeI64Imm, Args: []uint32{nID}, Const: 1})
	entry := fn.Block(entryID)
	entry.Values = []uint32{cond}
	entry.Term = Terminator{Kind: TermBranch, Value: cond, IfTrue: baseID, IfFalse: recID}
	entry.Succs = []uint32{baseID, recID}

	one := fn.AddValue(Value{Type: TypeI64, Op: OpConst, Const: 1})
	base := fn.Block(baseID)
	base.Values = []uint32{one}
	base.Preds = []uint32{entryID}
	base.Term = Terminator{Kind: TermReturn, Value: one}

	nm1 := fn.AddValue(Value{Type: TypeI64, Op: OpSubI64Imm, Args: []uint32{nID}, Const: 1})
	callRet := fn.AddValue(Value{Type: TypeI64, Op: OpCall, Args: []uint32{nm1}, Const: 0})
	mul := fn.AddValue(Value{Type: TypeI64, Op: OpMulI64, Args: []uint32{nID, callRet}})
	rec := fn.Block(recID)
	rec.Values = []uint32{nm1, callRet, mul}
	rec.Preds = []uint32{entryID}
	rec.Term = Terminator{Kind: TermReturn, Value: mul}

	return fn
}
