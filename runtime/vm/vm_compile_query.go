package vm

import (
	"fmt"
	"mochi/parser"
	"mochi/types"

	"github.com/alecthomas/participle/v2/lexer"
)

// compileQuery compiles a simple query expression supporting FROM clauses,
// optional WHERE filtering and SELECT projection. Only cross joins are
// handled and results are accumulated into a list.
func (fc *funcCompiler) compileQuery(q *parser.QueryExpr) int {
	// Detect simple aggregate SELECT like `sum(x)` without GROUP BY.
	var aggOp Op
	var aggPos lexer.Position
	if q.Group == nil {
		if op, arg, pos, ok := aggregateCall(q.Select); ok {
			aggOp = op
			aggPos = pos
			origSel := q.Select
			q.Select = arg
			defer func() { q.Select = origSel }()
		}
	}

	dst := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: dst, Val: Value{Tag: ValueList, List: []Value{}}})

	if q.Where != nil {
		if b, ok := fc.constBool(q.Where); ok && !b {
			if aggOp != 0 {
				out := fc.newReg()
				fc.emit(aggPos, Instr{Op: aggOp, A: out, B: dst})
				return out
			}
			return dst
		}
	}
	if q.Group != nil {
		if len(q.Froms) == 0 && len(q.Joins) == 0 {
			fc.compileGroupQuery(q, dst)
		} else {
			fc.compileGroupQueryAny(q, dst)
		}
	} else {
		switch {
		case len(q.Joins) == 1 && len(q.Froms) == 0:
			fc.compileJoinQuery(q, dst)
		case len(q.Joins) == 0:
			lvl := whereEvalLevel(q)
			fc.compileQueryFrom(q, dst, 0, lvl)
		default:
			fc.compileQueryFull(q, dst, 0)
		}
	}
	if q.Sort != nil {
		sorted := fc.newReg()
		fc.emit(q.Sort.Pos, Instr{Op: OpSort, A: sorted, B: dst})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: sorted})
	}
	if q.Skip != nil {
		start := fc.compileExpr(q.Skip)
		nul := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: nul, Val: Value{Tag: ValueNull}})
		out := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpSlice, A: out, B: dst, C: start, D: nul})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: out})
	}
	if q.Take != nil {
		zero := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: zero, Val: Value{Tag: ValueInt, Int: 0}})
		end := fc.compileExpr(q.Take)
		out := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpSlice, A: out, B: dst, C: zero, D: end})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: out})
	}
	if aggOp != 0 {
		out := fc.newReg()
		fc.emit(aggPos, Instr{Op: aggOp, A: out, B: dst})
		dst = out
	}
	return dst
}

func (fc *funcCompiler) compileQueryFull(q *parser.QueryExpr, dst int, level int) {
	var name string
	var src *parser.Expr
	if level == 0 {
		name = q.Var
		src = q.Source
		fc.preloadFieldConsts(q.Where)
		fc.preloadFieldConsts(q.Select)
		fc.preloadFieldConsts(q.Sort)
	} else {
		from := q.Froms[level-1]
		name = from.Var
		src = from.Src
	}

	srcReg := fc.compileExpr(src)
	listReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lenReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLen, A: lenReg, B: listReg})
	idxReg := fc.newReg()
	zero := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(src.Pos, Instr{Op: OpMove, A: idxReg, B: zero})
	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lenReg})
	jmp := len(fc.fn.Code)
	fc.emit(src.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[name]
	if !ok {
		varReg = fc.newReg()
		fc.vars[name] = varReg
	}
	fc.emit(src.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	if level < len(q.Froms) {
		fc.compileQueryFull(q, dst, level+1)
	} else {
		fc.compileJoins(q, dst, 0)
	}

	one := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(src.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(src.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
}

func (fc *funcCompiler) compileJoins(q *parser.QueryExpr, dst int, idx int) {
	if idx >= len(q.Joins) {
		appendVal := func() {
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				vtmp := fc.compileExpr(q.Select)
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: vtmp})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			} else {
				vtmp := fc.compileExpr(q.Select)
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: vtmp})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			}
		}
		if q.Where != nil {
			cond := fc.compileExpr(q.Where)
			skip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			appendVal()
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			appendVal()
		}
		return
	}

	join := q.Joins[idx]
	joinType := "inner"
	if join.Side != nil {
		joinType = *join.Side
	}

	if rl, ok := fc.constListLen(join.Src); ok && rl == 0 {
		switch joinType {
		case "inner", "right":
			return
		case "left", "outer":
			rvar, ok := fc.vars[join.Var]
			if !ok {
				rvar = fc.newReg()
				fc.vars[join.Var] = rvar
			}
			nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
			fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
			fc.compileJoins(q, dst, idx+1)
			return
		}
	}

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)
	ri := fc.newReg()
	zero := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(join.Pos, Instr{Op: OpMove, A: ri, B: zero})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if joinType == "left" || joinType == "outer" {
		matched := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileJoins(q, dst, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileJoins(q, dst, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAdd, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end

		check := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		fc.compileJoins(q, dst, idx+1)
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)
	} else {
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.compileJoins(q, dst, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.compileJoins(q, dst, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAdd, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end
	}
}

func (fc *funcCompiler) compileJoinQuery(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]
	joinType := "inner"
	if join.Side != nil {
		joinType = *join.Side
	}

	if rl, ok := fc.constListLen(join.Src); ok && rl == 0 {
		switch joinType {
		case "inner", "right":
			empty := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: empty})
			return
		case "left", "outer":
			fc.compileEmptyRightOuterJoin(q, dst)
			return
		}
	}

	if ll, ok := fc.constListLen(q.Source); ok && ll == 0 {
		switch joinType {
		case "inner", "left":
			empty := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: empty})
			return
		case "right", "outer":
			fc.compileEmptyLeftOuterJoin(q, dst)
			return
		}
	}

	if joinType == "inner" {
		if rl, ok := fc.constListLen(join.Src); ok && rl == 1 {
			fc.compileSingleRowRightJoin(q, dst)
			return
		}
		if ll, ok := fc.constListLen(q.Source); ok && ll == 1 {
			fc.compileSingleRowLeftJoin(q, dst)
			return
		}
	}

	if joinType == "left" {
		if rl, ok := fc.constListLen(join.Src); ok && rl == 1 {
			fc.compileSingleRowRightOuterJoin(q, dst)
			return
		}
	}

	if joinType == "right" {
		if ll, ok := fc.constListLen(q.Source); ok && ll == 1 {
			fc.compileSingleRowLeftOuterJoin(q, dst)
			return
		}
	}

	if joinType == "outer" {
		if rl, ok := fc.constListLen(join.Src); ok && rl == 1 {
			if ll, ok2 := fc.constListLen(q.Source); ok2 && ll == 1 {
				fc.compileSingleRowFullOuterJoin(q, dst)
			} else {
				fc.compileSingleRowRightOuterJoin(q, dst)
			}
			return
		}
		if ll, ok := fc.constListLen(q.Source); ok && ll == 1 {
			fc.compileSingleRowLeftOuterJoin(q, dst)
			return
		}
	}

	if joinType == "inner" {
		if _, _, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			if !fc.smallConstJoin(q.Source, join.Src) {
				// Temporarily fall back to the nested loop join
				// implementation to avoid issues with the hash
				// join path on complex queries like TPC-H.
				// fc.compileHashJoin(q, dst, lk, rk)
				// return
			}
		}
	}

	if joinType == "left" {
		if lk, rk, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			fc.compileHashLeftJoin(q, dst, lk, rk)
			return
		}
	}

	if joinType == "right" {
		if lk, rk, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			fc.compileHashRightJoin(q, dst, lk, rk)
			return
		} else {
			fc.compileJoinQueryRight(q, dst)
			return
		}
	}

	if joinType == "outer" {
		if lk, rk, ok := eqJoinKeys(join.On, q.Var, join.Var); ok {
			fc.compileHashOuterJoin(q, dst, lk, rk)
			return
		} else {
			fc.compileJoinQueryRight(q, dst)
			return
		}
	}

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	// helper to append selected value
	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	// inner join results
	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend

	one2 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one2})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	// left unmatched rows
	if joinType == "left" || joinType == "outer" {
		li2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li2, Val: Value{Tag: ValueInt, Int: 0}})
		lstart2 := len(fc.fn.Code)
		lcond2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond2, B: li2, C: llen})
		ljmp2 := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond2})
		lelem2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem2, B: llist, C: li2})
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem2})

		matched := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		ri2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri2, Val: Value{Tag: ValueInt, Int: 0}})
		rstart2 := len(fc.fn.Code)
		rcond2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond2, B: ri2, C: rlen})
		rjmp2 := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond2})
		relem2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem2, B: rlist, C: ri2})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem2})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		}
		one3 := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri2, B: ri2, C: one3})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart2})
		rend2 := len(fc.fn.Code)
		fc.fn.Code[rjmp2].B = rend2

		check := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
		fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)

		one4 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li2, B: li2, C: one4})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart2})
		lend2 := len(fc.fn.Code)
		fc.fn.Code[ljmp2].B = lend2
	}

	if joinType == "right" || joinType == "outer" {
		ri3 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri3, Val: Value{Tag: ValueInt, Int: 0}})
		rstart3 := len(fc.fn.Code)
		rcond3 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond3, B: ri3, C: rlen})
		rjmp3 := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond3})
		relem3 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem3, B: rlist, C: ri3})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem3})

		matched := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		li3 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li3, Val: Value{Tag: ValueInt, Int: 0}})
		lstart3 := len(fc.fn.Code)
		lcond3 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond3, B: li3, C: llen})
		ljmp3 := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond3})
		lelem3 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem3, B: llist, C: li3})
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem3})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		}
		one5 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li3, B: li3, C: one5})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart3})
		lend3 := len(fc.fn.Code)
		fc.fn.Code[ljmp3].B = lend3

		check := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)

		one6 := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri3, B: ri3, C: one6})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart3})
		rend3 := len(fc.fn.Code)
		fc.fn.Code[rjmp3].B = rend3
	}
}

// compileHashJoin performs an inner join using a hash map when the ON clause
// is a simple equality between left and right expressions. Only inner joins are
// handled by this helper.
func (fc *funcCompiler) compileHashJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereLeft := ok && wa == q.Var
	whereRight := ok && wa == join.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	if ll, ok1 := fc.constListLen(q.Source); ok1 {
		if rl, ok2 := fc.constListLen(join.Src); ok2 {
			if rl <= ll {
				fc.compileHashJoinSide(q, dst, leftKey, rightKey, true, llist, llen, rlist, rlen, whereLeft, whereRight)
			} else {
				fc.compileHashJoinSide(q, dst, leftKey, rightKey, false, llist, llen, rlist, rlen, whereLeft, whereRight)
			}
			return
		}
	}

	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	emptyLeft := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpEqualInt, A: emptyLeft, B: llen, C: zero})
	jmpEmptyLeft := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: emptyLeft})
	emptyRight := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpEqualInt, A: emptyRight, B: rlen, C: zero})
	jmpEmptyRight := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: emptyRight})

	cond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessEq, A: cond, B: rlen, C: llen})
	jmpLeft := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: cond})

	// hash right side when it is smaller
	fc.compileHashJoinSide(q, dst, leftKey, rightKey, true, llist, llen, rlist, rlen, whereLeft, whereRight)
	jumpEnd := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJump})
	fc.fn.Code[jmpLeft].B = len(fc.fn.Code)
	fc.fn.Code[jmpEmptyLeft].B = jumpEnd
	fc.fn.Code[jmpEmptyRight].B = jumpEnd

	// hash left side when it is smaller
	fc.compileHashJoinSide(q, dst, leftKey, rightKey, false, llist, llen, rlist, rlen, whereLeft, whereRight)

	fc.fn.Code[jumpEnd].B = len(fc.fn.Code)
}

func (fc *funcCompiler) compileHashJoinSide(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr, hashRight bool, llist, llen, rlist, rlen int, whereLeft, whereRight bool) {
	join := q.Joins[0]
	if hashRight {
		rmap := fc.newReg()
		cap := 0
		if n, ok := fc.constListLen(join.Src); ok {
			cap = n
		}
		fc.emit(join.Pos, Instr{Op: OpMakeMap, A: rmap, B: cap})

		ri := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
		rstart := len(fc.fn.Code)
		rcond := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
		rjmp := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
		relem := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
		rvar, ok := fc.vars[join.Var]
		if !ok {
			rvar = fc.newReg()
			fc.vars[join.Var] = rvar
		}
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
		var wskip int
		if whereRight {
			w := fc.compileExpr(q.Where)
			wskip = len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		}
		key := fc.compileExpr(rightKey)
		list := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		has := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
		skip := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has})
		newList := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
		fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: newList})
		fc.fn.Code[skip].B = len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
		tmp := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: relem})
		fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: tmp})
		if whereRight {
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		}
		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		rend := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = rend

		appendSelect := func() {
			val := fc.compileExpr(q.Select)
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			} else {
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: val})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			}
		}

		li := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
		lstart := len(fc.fn.Code)
		lcond := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
		ljmp := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
		lelem := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
		lvar, ok := fc.vars[q.Var]
		if !ok {
			lvar = fc.newReg()
			fc.vars[q.Var] = lvar
		}
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
		lkey := fc.compileExpr(leftKey)
		matches := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: matches, B: rmap, C: lkey})
		nil2 := fc.constReg(q.Pos, Value{Tag: ValueNull})
		has2 := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
		skipMatches := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: has2})
		mlen := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLen, A: mlen, B: matches})
		mi := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
		mstart := len(fc.fn.Code)
		mcond := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
		mjmp := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
		melem := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
		fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: melem})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		oneM := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
		fc.emit(q.Pos, Instr{Op: OpJump, A: mstart})
		mend := len(fc.fn.Code)
		fc.fn.Code[mjmp].B = mend
		fc.fn.Code[skipMatches].B = len(fc.fn.Code)

		oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
		lend := len(fc.fn.Code)
		fc.fn.Code[ljmp].B = lend
	} else {
		lmap := fc.newReg()
		cap := 0
		if n, ok := fc.constListLen(q.Source); ok {
			cap = n
		}
		fc.emit(q.Pos, Instr{Op: OpMakeMap, A: lmap, B: cap})

		li := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
		lstart := len(fc.fn.Code)
		lcond := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
		ljmp := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
		lelem := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
		lvar, ok := fc.vars[q.Var]
		if !ok {
			lvar = fc.newReg()
			fc.vars[q.Var] = lvar
		}
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
		var wskip int
		if whereLeft {
			w := fc.compileExpr(q.Where)
			wskip = len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		}
		key := fc.compileExpr(leftKey)
		list := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
		nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
		has := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
		skip := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has})
		newList := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
		fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: newList})
		fc.fn.Code[skip].B = len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
		tmp := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: lelem})
		fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: tmp})
		if whereLeft {
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		}
		one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
		fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
		lend := len(fc.fn.Code)
		fc.fn.Code[ljmp].B = lend

		appendSelect := func() {
			val := fc.compileExpr(q.Select)
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			} else {
				tmp2 := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: val})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
			}
		}

		ri := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
		rstart := len(fc.fn.Code)
		rcond := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
		rjmp := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
		relem := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
		rvar, ok := fc.vars[join.Var]
		if !ok {
			rvar = fc.newReg()
			fc.vars[join.Var] = rvar
		}
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
		rkey := fc.compileExpr(rightKey)
		matches := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: matches, B: lmap, C: rkey})
		nil2 := fc.constReg(join.Pos, Value{Tag: ValueNull})
		has2 := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
		skipMatches := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: has2})
		mlen := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLen, A: mlen, B: matches})
		mi := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
		mstart := len(fc.fn.Code)
		mcond := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
		mjmp := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
		melem := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
		fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: melem})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		oneM := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
		fc.emit(join.Pos, Instr{Op: OpJump, A: mstart})
		mend := len(fc.fn.Code)
		fc.fn.Code[mjmp].B = mend
		fc.fn.Code[skipMatches].B = len(fc.fn.Code)

		oneR := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: oneR})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		rend := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = rend
	}
}

// compileHashLeftJoin performs a left join using a hash map when the ON clause
// is a simple equality between left and right expressions.
func (fc *funcCompiler) compileHashLeftJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereRight := ok && wa == join.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	rmap := fc.newReg()
	rcap := 0
	if n, ok := fc.constListLen(join.Src); ok {
		rcap = n
	}
	fc.emit(join.Pos, Instr{Op: OpMakeMap, A: rmap, B: rcap})

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	var wskip int
	if whereRight {
		w := fc.compileExpr(q.Where)
		wskip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
	}
	key := fc.compileExpr(rightKey)
	list := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
	has := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
	skip := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has})
	newList := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: newList})
	fc.fn.Code[skip].B = len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	tmp := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: relem})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: tmp})
	if whereRight {
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	}
	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
	lkey := fc.compileExpr(leftKey)
	matches := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: matches, B: rmap, C: lkey})
	nil2 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	has2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
	skipMatches := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: has2})
	mlen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: mlen, B: matches})
	mi := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
	mstart := len(fc.fn.Code)
	mcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
	mjmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
	melem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: melem})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	oneM := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
	fc.emit(q.Pos, Instr{Op: OpJump, A: mstart})
	mend := len(fc.fn.Code)
	fc.fn.Code[mjmp].B = mend
	fc.fn.Code[skipMatches].B = len(fc.fn.Code)

	skipAdd := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has2})
	nilreg3 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: nilreg3})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip2 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip2].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend
}

// compileHashRightJoin performs a right join using a hash map when the ON clause
// is a simple equality between left and right expressions.
func (fc *funcCompiler) compileHashRightJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereLeft := ok && wa == q.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	lmap := fc.newReg()
	lcap := 0
	if n, ok := fc.constListLen(q.Source); ok {
		lcap = n
	}
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: lmap, B: lcap})

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
	var wskip int
	if whereLeft {
		w := fc.compileExpr(q.Where)
		wskip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
	}
	key := fc.compileExpr(leftKey)
	list := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
	nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
	has := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
	skip := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has})
	newList := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: newList})
	fc.fn.Code[skip].B = len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpIndex, A: list, B: lmap, C: key})
	tmp := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: lelem})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: lmap, B: key, C: tmp})
	if whereLeft {
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	}
	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp2 := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
		} else {
			tmp2 := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp2, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp2})
		}
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	rkey := fc.compileExpr(rightKey)
	matches := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: matches, B: lmap, C: rkey})
	nil2 := fc.constReg(join.Pos, Value{Tag: ValueNull})
	has2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
	skipMatches := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: has2})
	mlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: mlen, B: matches})
	mi := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
	mstart := len(fc.fn.Code)
	mcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
	mjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
	melem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: melem})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	oneM := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
	fc.emit(join.Pos, Instr{Op: OpJump, A: mstart})
	mend := len(fc.fn.Code)
	fc.fn.Code[mjmp].B = mend
	fc.fn.Code[skipMatches].B = len(fc.fn.Code)

	skipAdd := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has2})
	nilreg3 := fc.constReg(join.Pos, Value{Tag: ValueNull})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilreg3})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip2 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip2].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneR := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: oneR})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend
}

func (fc *funcCompiler) compileJoinQueryRight(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})

	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	matched := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})

	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	check := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
	skipAdd := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
	nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilreg})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneR := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: oneR})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	end := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = end
}

// compileHashOuterJoin performs a full outer join using a hash map when the ON clause
// is a simple equality between left and right expressions.
func (fc *funcCompiler) compileHashOuterJoin(q *parser.QueryExpr, dst int, leftKey, rightKey *parser.Expr) {
	join := q.Joins[0]

	wa, ok := whereAlias(q.Where)
	whereRight := ok && wa == join.Var

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	rmap := fc.newReg()
	rcap := 0
	if n, ok := fc.constListLen(join.Src); ok {
		rcap = n
	}
	fc.emit(join.Pos, Instr{Op: OpMakeMap, A: rmap, B: rcap})

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	var wskip int
	if whereRight {
		w := fc.compileExpr(q.Where)
		wskip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
	}
	key := fc.compileExpr(rightKey)
	list := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
	has := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: has, B: list, C: nilreg})
	skip := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: has})
	newList := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMakeList, A: newList, B: 0, C: 0})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: newList})
	fc.fn.Code[skip].B = len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpIndex, A: list, B: rmap, C: key})
	tmp := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpAppend, A: tmp, B: list, C: relem})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: rmap, B: key, C: tmp})
	if whereRight {
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	}
	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend

	matched := fc.newReg()
	mcap := 0
	if n, ok := fc.constListLen(join.Src); ok {
		mcap = n
	}
	fc.emit(join.Pos, Instr{Op: OpMakeMap, A: matched, B: mcap})
	trueReg := fc.constReg(join.Pos, Value{Tag: ValueBool, Bool: true})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
	lkey := fc.compileExpr(leftKey)
	matches := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: matches, B: rmap, C: lkey})
	nil2 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	has2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpNotEqual, A: has2, B: matches, C: nil2})
	skipMatches := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: has2})
	fc.emit(join.Pos, Instr{Op: OpSetIndex, A: matched, B: lkey, C: trueReg})
	mlen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: mlen, B: matches})
	mi := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: mi, Val: Value{Tag: ValueInt, Int: 0}})
	mstart := len(fc.fn.Code)
	mcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: mcond, B: mi, C: mlen})
	mjmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: mcond})
	melem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: melem, B: matches, C: mi})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: melem})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	oneM := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: mi, B: mi, C: oneM})
	fc.emit(q.Pos, Instr{Op: OpJump, A: mstart})
	mend := len(fc.fn.Code)
	fc.fn.Code[mjmp].B = mend
	fc.fn.Code[skipMatches].B = len(fc.fn.Code)

	skipAdd := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfTrue, A: has2})
	nilreg3 := fc.constReg(q.Pos, Value{Tag: ValueNull})
	fc.emit(q.Pos, Instr{Op: OpMove, A: rvar, B: nilreg3})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip2 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip2].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd].B = len(fc.fn.Code)

	oneL := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: oneL})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend

	ri2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri2, Val: Value{Tag: ValueInt, Int: 0}})
	rstart2 := len(fc.fn.Code)
	rcond2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond2, B: ri2, C: rlen})
	rjmp2 := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond2})
	relem2 := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem2, B: rlist, C: ri2})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem2})
	rkey2 := fc.compileExpr(rightKey)
	hit := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: hit, B: matched, C: rkey2})
	nil4 := fc.constReg(join.Pos, Value{Tag: ValueNull})
	hasHit := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpNotEqual, A: hasHit, B: hit, C: nil4})
	skipAdd2 := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: hasHit})
	nilL := fc.constReg(join.Pos, Value{Tag: ValueNull})
	fc.emit(join.Pos, Instr{Op: OpMove, A: lvar, B: nilL})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip3 := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip3].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	fc.fn.Code[skipAdd2].B = len(fc.fn.Code)

	oneR2 := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri2, B: ri2, C: oneR2})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart2})
	rend2 := len(fc.fn.Code)
	fc.fn.Code[rjmp2].B = rend2
}

// compileSingleRowRightJoin handles inner joins when the right side has exactly
// one element. The right row is evaluated once and reused while scanning the
// left side.
func (fc *funcCompiler) compileSingleRowRightJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	zero := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 0})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: zero})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	end := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = end
}

// compileSingleRowLeftJoin handles inner joins when the left side has exactly
// one element. The left row is evaluated once and reused while scanning the
// right side.
func (fc *funcCompiler) compileSingleRowLeftJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: zero})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			kreg := fc.newReg()
			vreg := fc.newReg()
			key := fc.compileExpr(q.Sort)
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	end := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = end
}

// compileSingleRowRightOuterJoin handles left joins when the right side has exactly
// one element. The right row is evaluated once and reused while scanning the
// left side.
func (fc *funcCompiler) compileSingleRowRightOuterJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	zero := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 0})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: zero})
	rrow := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMove, A: rrow, B: relem})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			key := fc.compileExpr(q.Sort)
			kreg := fc.newReg()
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			vreg := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})

	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: rrow})
	if join.On != nil {
		cond := fc.compileExpr(join.On)
		match := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		jumpEnd := len(fc.fn.Code)
		fc.emit(q.Pos, Instr{Op: OpJump})
		fc.fn.Code[match].B = len(fc.fn.Code)
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[jumpEnd].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend
}

// compileSingleRowLeftOuterJoin handles right joins when the left side has exactly
// one element. The left row is evaluated once and reused while scanning the
// right side.
func (fc *funcCompiler) compileSingleRowLeftOuterJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: zero})
	lrow := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMove, A: lrow, B: lelem})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			key := fc.compileExpr(q.Sort)
			kreg := fc.newReg()
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			vreg := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lrow})

	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		jumpEnd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJump})
		fc.fn.Code[skip].B = len(fc.fn.Code)
		nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lrow})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip2 := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip2].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		fc.fn.Code[jumpEnd].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}

	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend
}

// compileSingleRowFullOuterJoin handles outer joins when both sides have exactly
// one element each. The join condition is evaluated once to decide whether to
// produce a matched row or two unmatched rows.
func (fc *funcCompiler) compileSingleRowFullOuterJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(join.On)
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: zero})
	lrow := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMove, A: lrow, B: lelem})

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: zero})
	rrow := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpMove, A: rrow, B: relem})

	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			key := fc.compileExpr(q.Sort)
			kreg := fc.newReg()
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			vreg := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lrow})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: rrow})
	if join.On != nil {
		cond := fc.compileExpr(join.On)
		skip := len(fc.fn.Code)
		fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
		jumpEnd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJump})
		fc.fn.Code[skip].B = len(fc.fn.Code)

		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lrow})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}

		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: rrow})
		fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: nilreg})
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip2 := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip2].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}

		fc.fn.Code[jumpEnd].B = len(fc.fn.Code)
	} else {
		if q.Where != nil {
			w := fc.compileExpr(q.Where)
			wskip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
			appendSelect()
			fc.fn.Code[wskip].B = len(fc.fn.Code)
		} else {
			appendSelect()
		}
	}
}

// compileEmptyRightOuterJoin handles left or outer joins when the right side is empty.
func (fc *funcCompiler) compileEmptyRightOuterJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	leftReg := fc.compileExpr(q.Source)
	llist := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: llist, B: leftReg})
	llen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: llen, B: llist})

	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			key := fc.compileExpr(q.Sort)
			kreg := fc.newReg()
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			vreg := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	li := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: li, Val: Value{Tag: ValueInt, Int: 0}})
	lstart := len(fc.fn.Code)
	lcond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: lcond, B: li, C: llen})
	ljmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: lcond})
	lelem := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: lelem, B: llist, C: li})
	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: lelem})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: li, B: li, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: lstart})
	lend := len(fc.fn.Code)
	fc.fn.Code[ljmp].B = lend
}

// compileEmptyLeftOuterJoin handles right or outer joins when the left side is empty.
func (fc *funcCompiler) compileEmptyLeftOuterJoin(q *parser.QueryExpr, dst int) {
	join := q.Joins[0]

	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})

	lvar, ok := fc.vars[q.Var]
	if !ok {
		lvar = fc.newReg()
		fc.vars[q.Var] = lvar
	}
	nilreg := fc.constReg(q.Pos, Value{Tag: ValueNull})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}

	appendSelect := func() {
		val := fc.compileExpr(q.Select)
		if q.Sort != nil {
			key := fc.compileExpr(q.Sort)
			kreg := fc.newReg()
			fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
			vreg := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
			pair := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		} else {
			tmp := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
			fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
		}
	}

	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})
	fc.emit(q.Pos, Instr{Op: OpMove, A: lvar, B: nilreg})
	if q.Where != nil {
		w := fc.compileExpr(q.Where)
		wskip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: w})
		appendSelect()
		fc.fn.Code[wskip].B = len(fc.fn.Code)
	} else {
		appendSelect()
	}
	one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
	fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
	rend := len(fc.fn.Code)
	fc.fn.Code[rjmp].B = rend
}

// compileGroupQuery handles simple queries with a single FROM clause and GROUP BY.
func (fc *funcCompiler) compileGroupQuery(q *parser.QueryExpr, dst int) {
	for _, g := range q.Group.Exprs {
		fc.preloadFieldConsts(g)
	}
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Group.Having)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)
	prevGroup := fc.groupVar
	fc.groupVar = q.Group.Name
	defer func() { fc.groupVar = prevGroup }()
	srcReg := fc.compileExpr(q.Source)
	listReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lenReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: lenReg, B: listReg})
	idxReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpConst, A: idxReg, Val: Value{Tag: ValueInt, Int: 0}})

	groupsMap := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: groupsMap, B: 0})
	groupsList := fc.newReg()
	emptyList := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
	fc.emit(q.Pos, Instr{Op: OpMove, A: groupsList, B: emptyList})

	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lenReg})
	jmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[q.Var]
	if !ok {
		varReg = fc.newReg()
		fc.vars[q.Var] = varReg
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	if q.Where != nil {
		cond := fc.compileExpr(q.Where)
		skip := len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
		fc.compileGroupAccum(q, elemReg, varReg, groupsMap, groupsList)
		fc.fn.Code[skip].B = len(fc.fn.Code)
	} else {
		fc.compileGroupAccum(q, elemReg, varReg, groupsMap, groupsList)
	}

	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end

	// iterate groups and produce final results
	gi := fc.newReg()
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(q.Pos, Instr{Op: OpMove, A: gi, B: zero})
	glen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: glen, B: groupsList})
	loop2 := len(fc.fn.Code)
	cond2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: cond2, B: gi, C: glen})
	jmp2 := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: cond2})

	grp := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: grp, B: groupsList, C: gi})
	gvar, ok := fc.vars[q.Group.Name]
	if !ok {
		gvar = fc.newReg()
		fc.vars[q.Group.Name] = gvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: gvar, B: grp})

	var skip int
	if q.Group.Having != nil {
		cond := fc.compileExpr(q.Group.Having)
		skip = len(fc.fn.Code)
		fc.emit(q.Group.Having.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	}

	val := fc.compileExpr(q.Select)
	if q.Sort != nil {
		kreg := fc.newReg()
		vreg := fc.newReg()
		key := fc.compileExpr(q.Sort)
		fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
		fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
		pair := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
		tmpOut := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmpOut, B: dst, C: pair})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmpOut})
	} else {
		tmpOut := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmpOut, B: dst, C: val})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmpOut})
	}

	one2 := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: gi, B: gi, C: one2})
	fc.emit(q.Pos, Instr{Op: OpJump, A: loop2})
	end2 := len(fc.fn.Code)
	fc.fn.Code[jmp2].B = end2
	if q.Group.Having != nil {
		fc.fn.Code[skip].B = end2
	}
}

func (fc *funcCompiler) compileGroupAccum(q *parser.QueryExpr, elemReg, varReg, gmap, glist int) {
	exprs := q.Group.Exprs
	regs := make([]int, len(exprs))
	for i, e := range exprs {
		regs[i] = fc.compileExpr(e)
	}
	key := regs[0]
	var fieldNames []string
	if len(exprs) > 1 {
		fieldNames = make([]string, len(exprs))
		for i, e := range exprs {
			if name := extractFieldName(e); name != "" {
				fieldNames[i] = name
			} else {
				fieldNames[i] = fmt.Sprintf("k%d", i+1)
			}
		}
		pairs := make([]int, len(exprs)*2)
		for i, name := range fieldNames {
			k := fc.constReg(exprs[i].Pos, Value{Tag: ValueStr, Str: name})
			kr := fc.newReg()
			fc.emit(exprs[i].Pos, Instr{Op: OpMove, A: kr, B: k})
			vr := fc.newReg()
			fc.emit(exprs[i].Pos, Instr{Op: OpMove, A: vr, B: regs[i]})
			pairs[i*2] = kr
			pairs[i*2+1] = vr
		}
		key = fc.newReg()
		start := pairs[0]
		fc.emit(q.Pos, Instr{Op: OpMakeMap, A: key, B: len(exprs), C: start})
	}
	keyStr := fc.newReg()
	fc.emit(q.Group.Pos, Instr{Op: OpStr, A: keyStr, B: key})
	exists := fc.newReg()
	fc.emit(q.Group.Pos, Instr{Op: OpIn, A: exists, B: keyStr, C: gmap})
	jump := len(fc.fn.Code)
	fc.emit(q.Group.Pos, Instr{Op: OpJumpIfTrue, A: exists})

	items := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
	k1 := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "__group__"})
	v1 := fc.constReg(q.Pos, Value{Tag: ValueBool, Bool: true})
	k2 := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "key"})
	v2 := fc.newReg()
	fc.emit(q.Group.Pos, Instr{Op: OpMove, A: v2, B: key})
	k3 := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "items"})
	v3 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMove, A: v3, B: items})
	kcnt := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "count"})
	vcnt := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	pairsGrp := []int{k1, v1, k2, v2, k3, v3, kcnt, vcnt}
	if len(fieldNames) > 0 {
		for i, name := range fieldNames {
			k := fc.freshConst(exprs[i].Pos, Value{Tag: ValueStr, Str: name})
			v := fc.newReg()
			fc.emit(exprs[i].Pos, Instr{Op: OpMove, A: v, B: regs[i]})
			pairsGrp = append(pairsGrp, k, v)
		}
	}
	contig := make([]int, len(pairsGrp))
	for i, r := range pairsGrp {
		nr := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: nr, B: r})
		contig[i] = nr
	}
	grp := fc.newReg()
	startGrp := contig[0]
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: grp, B: len(contig) / 2, C: startGrp})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: gmap, B: keyStr, C: grp})
	tmpList := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAppend, A: tmpList, B: glist, C: grp})
	fc.emit(q.Pos, Instr{Op: OpMove, A: glist, B: tmpList})

	end := len(fc.fn.Code)
	fc.fn.Code[jump].B = end

	itemsKey := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "items"})
	grp2 := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: grp2, B: gmap, C: keyStr})
	cur := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: cur, B: grp2, C: itemsKey})
	newList := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAppend, A: newList, B: cur, C: elemReg})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: grp2, B: itemsKey, C: newList})
	countKey := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: "count"})
	curCnt := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: curCnt, B: grp2, C: countKey})
	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	newCnt := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: newCnt, B: curCnt, C: one})
	fc.emit(q.Pos, Instr{Op: OpSetIndex, A: grp2, B: countKey, C: newCnt})
}

// compileGroupQueryAny handles GROUP BY queries that may include additional FROM
// clauses or JOINs. It builds row objects containing all bound variables which
// are accumulated into groups.
func (fc *funcCompiler) compileGroupQueryAny(q *parser.QueryExpr, dst int) {
	for _, g := range q.Group.Exprs {
		fc.preloadFieldConsts(g)
	}
	fc.preloadFieldConsts(q.Where)
	fc.preloadFieldConsts(q.Group.Having)
	fc.preloadFieldConsts(q.Select)
	fc.preloadFieldConsts(q.Sort)
	prevGroup := fc.groupVar
	fc.groupVar = q.Group.Name
	defer func() { fc.groupVar = prevGroup }()
	groupsMap := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: groupsMap, B: 0})
	groupsList := fc.newReg()
	emptyList := fc.constReg(q.Pos, Value{Tag: ValueList, List: []Value{}})
	fc.emit(q.Pos, Instr{Op: OpMove, A: groupsList, B: emptyList})

	fc.compileGroupFromAny(q, groupsMap, groupsList, 0)

	// iterate groups and produce final results
	gi := fc.newReg()
	zero := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(q.Pos, Instr{Op: OpMove, A: gi, B: zero})
	glen := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLen, A: glen, B: groupsList})
	loop := len(fc.fn.Code)
	cond := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpLessInt, A: cond, B: gi, C: glen})
	jmp := len(fc.fn.Code)
	fc.emit(q.Pos, Instr{Op: OpJumpIfFalse, A: cond})

	grp := fc.newReg()
	fc.emit(q.Pos, Instr{Op: OpIndex, A: grp, B: groupsList, C: gi})
	gvar, ok := fc.vars[q.Group.Name]
	if !ok {
		gvar = fc.newReg()
		fc.vars[q.Group.Name] = gvar
	}
	fc.emit(q.Pos, Instr{Op: OpMove, A: gvar, B: grp})

	var skip int
	if q.Group.Having != nil {
		cond := fc.compileExpr(q.Group.Having)
		skip = len(fc.fn.Code)
		fc.emit(q.Group.Having.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	}

	val := fc.compileExpr(q.Select)
	if q.Sort != nil {
		kreg := fc.newReg()
		vreg := fc.newReg()
		key := fc.compileExpr(q.Sort)
		fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
		fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
		pair := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
		tmp := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
	} else {
		tmp := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
		fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
	}

	one := fc.constReg(q.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(q.Pos, Instr{Op: OpAddInt, A: gi, B: gi, C: one})
	fc.emit(q.Pos, Instr{Op: OpJump, A: loop})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
	if q.Group.Having != nil {
		fc.fn.Code[skip].B = end
	}
}

func (fc *funcCompiler) compileGroupFromAny(q *parser.QueryExpr, gmap, glist int, level int) {
	var name string
	var src *parser.Expr
	if level == 0 {
		name = q.Var
		src = q.Source
	} else {
		from := q.Froms[level-1]
		name = from.Var
		src = from.Src
	}

	srcReg := fc.compileExpr(src)
	listReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lenReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLen, A: lenReg, B: listReg})
	idxReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpConst, A: idxReg, Val: Value{Tag: ValueInt, Int: 0}})
	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lenReg})
	jmp := len(fc.fn.Code)
	fc.emit(src.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[name]
	if !ok {
		varReg = fc.newReg()
		fc.vars[name] = varReg
	}
	fc.emit(src.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	if level < len(q.Froms) {
		fc.pushScope()
		fc.compileGroupFromAny(q, gmap, glist, level+1)
		fc.popScope()
	} else {
		fc.pushScope()
		fc.compileGroupJoinAny(q, gmap, glist, 0)
		fc.popScope()
	}

	one := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(src.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(src.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
}

func (fc *funcCompiler) compileGroupJoinAny(q *parser.QueryExpr, gmap, glist int, idx int) {
	if idx >= len(q.Joins) {
		doAccum := func() {
			row := fc.buildRowMap(q)
			vreg, ok := fc.vars[q.Var]
			if !ok {
				vreg = fc.newReg()
				fc.vars[q.Var] = vreg
			}
			fc.compileGroupAccum(q, row, vreg, gmap, glist)
		}
		if q.Where != nil {
			cond := fc.compileExpr(q.Where)
			skip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			doAccum()
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			doAccum()
		}
		return
	}

	join := q.Joins[idx]
	joinType := "inner"
	if join.Side != nil {
		joinType = *join.Side
	}

	rightReg := fc.compileExpr(join.Src)
	rlist := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIterPrep, A: rlist, B: rightReg})
	rlen := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLen, A: rlen, B: rlist})
	ri := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpConst, A: ri, Val: Value{Tag: ValueInt, Int: 0}})
	rstart := len(fc.fn.Code)
	rcond := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpLessInt, A: rcond, B: ri, C: rlen})
	rjmp := len(fc.fn.Code)
	fc.emit(join.Pos, Instr{Op: OpJumpIfFalse, A: rcond})
	relem := fc.newReg()
	fc.emit(join.Pos, Instr{Op: OpIndex, A: relem, B: rlist, C: ri})
	rvar, ok := fc.vars[join.Var]
	if !ok {
		rvar = fc.newReg()
		fc.vars[join.Var] = rvar
	}
	fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: relem})

	if joinType == "left" || joinType == "outer" {
		matched := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: false}})
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.emit(join.Pos, Instr{Op: OpConst, A: matched, Val: Value{Tag: ValueBool, Bool: true}})
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end

		check := fc.newReg()
		fc.emit(join.Pos, Instr{Op: OpMove, A: check, B: matched})
		skipAdd := len(fc.fn.Code)
		fc.emit(join.Pos, Instr{Op: OpJumpIfTrue, A: check})
		nilreg := fc.constReg(join.Pos, Value{Tag: ValueNull})
		fc.emit(join.Pos, Instr{Op: OpMove, A: rvar, B: nilreg})
		fc.compileGroupJoinAny(q, gmap, glist, idx+1)
		fc.fn.Code[skipAdd].B = len(fc.fn.Code)
	} else {
		if join.On != nil {
			cond := fc.compileExpr(join.On)
			skip := len(fc.fn.Code)
			fc.emit(join.On.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
			fc.fn.Code[skip].B = len(fc.fn.Code)
		} else {
			fc.compileGroupJoinAny(q, gmap, glist, idx+1)
		}

		one := fc.constReg(join.Pos, Value{Tag: ValueInt, Int: 1})
		fc.emit(join.Pos, Instr{Op: OpAddInt, A: ri, B: ri, C: one})
		fc.emit(join.Pos, Instr{Op: OpJump, A: rstart})
		end := len(fc.fn.Code)
		fc.fn.Code[rjmp].B = end
	}
}

func (fc *funcCompiler) buildRowMap(q *parser.QueryExpr) int {
	names := []string{q.Var}
	regs := []int{fc.vars[q.Var]}
	for _, f := range q.Froms {
		names = append(names, f.Var)
		regs = append(regs, fc.vars[f.Var])
	}
	for _, j := range q.Joins {
		names = append(names, j.Var)
		regs = append(regs, fc.vars[j.Var])
	}

	pairs := []int{}
	addPair := func(k, v int) {
		pairs = append(pairs, k, v)
	}
	var addStructFields func(reg int, st types.StructType)
	addStructFields = func(reg int, st types.StructType) {
		for _, f := range st.Fields {
			fk := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: f.Name})
			fv := fc.newReg()
			fc.emit(q.Pos, Instr{Op: OpIndex, A: fv, B: reg, C: fk})
			addPair(fk, fv)
			if ft, ok := f.Type.(types.StructType); ok {
				addStructFields(fv, ft)
			}
		}
	}
	for i, n := range names {
		k := fc.constReg(q.Pos, Value{Tag: ValueStr, Str: n})
		v := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: v, B: regs[i]})
		addPair(k, v)

		if typ, err := fc.comp.env.GetVar(n); err == nil {
			if st, ok := typ.(types.StructType); ok {
				addStructFields(regs[i], st)
			}
		}
	}
	contig := make([]int, len(pairs))
	for i, r := range pairs {
		nr := fc.newReg()
		fc.emit(q.Pos, Instr{Op: OpMove, A: nr, B: r})
		contig[i] = nr
	}
	row := fc.newReg()
	start := 0
	if len(contig) > 0 {
		start = contig[0]
	}
	fc.emit(q.Pos, Instr{Op: OpMakeMap, A: row, B: len(pairs) / 2, C: start})
	return row
}

// compileQueryFrom recursively emits nested loops for each FROM clause.
func (fc *funcCompiler) compileQueryFrom(q *parser.QueryExpr, dst int, level int, whereLevel int) {
	var name string
	var src *parser.Expr
	if level == 0 {
		name = q.Var
		src = q.Source
		if q.Where != nil {
			if b, ok := fc.constBool(q.Where); ok && !b {
				fc.emit(q.Pos, Instr{Op: OpConst, A: dst, Val: Value{Tag: ValueList, List: []Value{}}})
				return
			}
		}
	} else {
		from := q.Froms[level-1]
		name = from.Var
		src = from.Src
	}

	if l, ok := fc.constListLen(src); ok && l == 0 {
		return
	}

	if level == 0 {
		fc.preloadFieldConsts(q.Where)
		fc.preloadFieldConsts(q.Select)
		fc.preloadFieldConsts(q.Sort)
	}

	srcReg := fc.compileExpr(src)
	listReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIterPrep, A: listReg, B: srcReg})
	lengthReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLen, A: lengthReg, B: listReg})
	idxReg := fc.newReg()
	zero := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 0})
	fc.emit(src.Pos, Instr{Op: OpMove, A: idxReg, B: zero})
	loopStart := len(fc.fn.Code)
	condReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpLessInt, A: condReg, B: idxReg, C: lengthReg})
	jmp := len(fc.fn.Code)
	fc.emit(src.Pos, Instr{Op: OpJumpIfFalse, A: condReg})

	elemReg := fc.newReg()
	fc.emit(src.Pos, Instr{Op: OpIndex, A: elemReg, B: listReg, C: idxReg})
	varReg, ok := fc.vars[name]
	if !ok {
		varReg = fc.newReg()
		fc.vars[name] = varReg
	}
	fc.emit(src.Pos, Instr{Op: OpMove, A: varReg, B: elemReg})

	skip := -1
	outerCheck := level == whereLevel && level < len(q.Froms) && q.Where != nil
	if outerCheck {
		cond := fc.compileExpr(q.Where)
		skip = len(fc.fn.Code)
		fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
	}

	if level < len(q.Froms) {
		fc.pushScope()
		fc.compileQueryFrom(q, dst, level+1, whereLevel)
		fc.popScope()
	} else {
		fc.pushScope()
		appendVal := func() {
			val := fc.compileExpr(q.Select)
			if q.Sort != nil {
				kreg := fc.newReg()
				vreg := fc.newReg()
				key := fc.compileExpr(q.Sort)
				fc.emit(q.Sort.Pos, Instr{Op: OpMove, A: kreg, B: key})
				fc.emit(q.Pos, Instr{Op: OpMove, A: vreg, B: val})
				pair := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpMakeList, A: pair, B: 2, C: kreg})
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: pair})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			} else {
				tmp := fc.newReg()
				fc.emit(q.Pos, Instr{Op: OpAppend, A: tmp, B: dst, C: val})
				fc.emit(q.Pos, Instr{Op: OpMove, A: dst, B: tmp})
			}
		}
		if q.Where != nil && level == whereLevel {
			cond := fc.compileExpr(q.Where)
			innerSkip := len(fc.fn.Code)
			fc.emit(q.Where.Pos, Instr{Op: OpJumpIfFalse, A: cond})
			appendVal()
			fc.fn.Code[innerSkip].B = len(fc.fn.Code)
		} else {
			appendVal()
		}
		fc.popScope()
	}

	if skip != -1 {
		fc.fn.Code[skip].B = len(fc.fn.Code)
	}

	one := fc.constReg(src.Pos, Value{Tag: ValueInt, Int: 1})
	fc.emit(src.Pos, Instr{Op: OpAddInt, A: idxReg, B: idxReg, C: one})
	fc.emit(src.Pos, Instr{Op: OpJump, A: loopStart})
	end := len(fc.fn.Code)
	fc.fn.Code[jmp].B = end
}
