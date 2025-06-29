package stackvm

import (
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

type compiler struct {
	prog  *parser.Program
	funcs []Function
	fnIdx map[string]int
}

func Compile(p *parser.Program) (*Program, error) {
	c := &compiler{prog: p, fnIdx: map[string]int{}}
	// Pre-assign function indices
	c.funcs = append(c.funcs, Function{Name: "main"})
	for _, st := range p.Statements {
		if st.Fun != nil {
			idx := len(c.funcs)
			c.fnIdx[st.Fun.Name] = idx
			c.funcs = append(c.funcs, Function{Name: st.Fun.Name})
		}
	}
	// Compile functions
	for _, st := range p.Statements {
		if st.Fun != nil {
			idx := c.fnIdx[st.Fun.Name]
			fn := c.compileFun(st.Fun)
			c.funcs[idx] = fn
		}
	}
	main := c.compileMain(p)
	c.funcs[0] = main
	return &Program{Funcs: c.funcs}, nil
}

type funcCompiler struct {
	c      *compiler
	fn     Function
	vars   map[string]int
	env    *types.Env
	consts map[string]Value
}

func (c *compiler) compileMain(p *parser.Program) Function {
	fc := &funcCompiler{c: c, vars: map[string]int{}, env: types.NewEnv(nil), consts: map[string]Value{}}
	for _, st := range p.Statements {
		if st.Fun == nil {
			fc.compileStmt(st)
		}
	}
	fc.emit(OpReturn, 0, 0, Value{})
	return fc.fn
}

func (c *compiler) compileFun(fn *parser.FunStmt) Function {
	fc := &funcCompiler{c: c, vars: map[string]int{}, env: types.NewEnv(nil), consts: map[string]Value{}}
	// params
	for i, p := range fn.Params {
		fc.vars[p.Name] = i
	}
	fc.fn.NumVars = len(fn.Params)
	for _, st := range fn.Body {
		fc.compileStmt(st)
	}
	fc.emit(OpReturn, 0, 0, Value{})
	fc.fn.Name = fn.Name
	return fc.fn
}

func (fc *funcCompiler) emit(op Op, a, b int, val Value) {
	fc.fn.Code = append(fc.fn.Code, Instr{Op: op, A: a, B: b, Val: val})
}
func (fc *funcCompiler) varIndex(name string) int {
	idx, ok := fc.vars[name]
	if !ok {
		idx = fc.fn.NumVars
		fc.fn.NumVars++
		fc.vars[name] = idx
	}
	return idx
}

func (fc *funcCompiler) compileStmt(s *parser.Statement) {
	switch {
	case s.Let != nil:
		idx := fc.varIndex(s.Let.Name)
		if s.Let.Value != nil {
			if v, ok := fc.constExpr(s.Let.Value); ok {
				fc.env.SetValue(s.Let.Name, valueToAny(v), false)
				fc.consts[s.Let.Name] = v
				fc.emit(OpPushConst, 0, 0, v)
			} else {
				fc.compileExpr(s.Let.Value)
			}
			fc.emit(OpStore, idx, 0, Value{})
		}
	case s.Return != nil:
		fc.compileExpr(s.Return.Value)
		fc.emit(OpReturn, 1, 0, Value{})
	case s.For != nil:
		fc.compileFor(s.For)
	case s.If != nil:
		fc.compileIf(s.If)
	case s.Test != nil:
		for _, st := range s.Test.Body {
			fc.compileStmt(st)
		}
	case s.Expect != nil:
		fc.compileExpr(s.Expect.Value)
		fc.emit(OpExpect, 0, 0, Value{})
	case s.Expr != nil:
		fc.compileExpr(s.Expr.Expr)
		fc.emit(OpPop, 0, 0, Value{})
	}
}

func (fc *funcCompiler) compileIf(s *parser.IfStmt) {
	fc.compileExpr(s.Cond)
	jmpFalseIdx := len(fc.fn.Code)
	fc.emit(OpJumpIfFalse, 0, 0, Value{})
	for _, st := range s.Then {
		fc.compileStmt(st)
	}
	jmpEndIdx := len(fc.fn.Code)
	fc.emit(OpJump, 0, 0, Value{})
	fc.fn.Code[jmpFalseIdx].A = len(fc.fn.Code)
	for _, st := range s.Else {
		fc.compileStmt(st)
	}
	fc.fn.Code[jmpEndIdx].A = len(fc.fn.Code)
}

func (fc *funcCompiler) compileFor(f *parser.ForStmt) {
	idx := fc.varIndex(f.Name)
	// start
	if f.Source != nil {
		fc.compileExpr(f.Source)
	} else {
		fc.emit(OpPushConst, 0, 0, Value{Tag: ValueInt, Int: 0})
	}
	fc.emit(OpStore, idx, 0, Value{})
	loopStart := len(fc.fn.Code)
	// condition
	fc.emit(OpLoad, idx, 0, Value{})
	fc.compileExpr(f.RangeEnd)
	fc.emit(OpLess, 0, 0, Value{})
	jmpIdx := len(fc.fn.Code)
	fc.emit(OpJumpIfFalse, 0, 0, Value{}) // placeholder
	for _, st := range f.Body {
		fc.compileStmt(st)
	}
	// i = i + 1
	fc.emit(OpLoad, idx, 0, Value{})
	fc.emit(OpPushConst, 0, 0, Value{Tag: ValueInt, Int: 1})
	fc.emit(OpAdd, 0, 0, Value{})
	fc.emit(OpStore, idx, 0, Value{})
	fc.emit(OpJump, loopStart, 0, Value{})
	fc.fn.Code[jmpIdx].A = len(fc.fn.Code)
}

func (fc *funcCompiler) constExpr(e *parser.Expr) (Value, bool) {
	if e == nil || containsIOCall(e) {
		return Value{}, false
	}
	modRoot, _ := mod.FindRoot(".")
	interp := interpreter.New(&parser.Program{}, fc.env.Copy(), modRoot)
	val, err := interp.EvalExpr(e)
	if err != nil {
		return Value{}, false
	}
	return anyToValue(val), true
}

func (fc *funcCompiler) constQuery(q *parser.QueryExpr) (Value, bool) {
	if q == nil {
		return Value{}, false
	}
	modRoot, _ := mod.FindRoot(".")
	interp := interpreter.New(&parser.Program{}, fc.env.Copy(), modRoot)
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Query: q}}}}}
	val, err := interp.EvalExpr(expr)
	if err != nil {
		return Value{}, false
	}
	return anyToValue(val), true
}

func (fc *funcCompiler) compileExpr(e *parser.Expr) {
	if v, ok := fc.constExpr(e); ok {
		fc.emit(OpPushConst, 0, 0, v)
		return
	}
	fc.compileBinary(e.Binary)
}

func (fc *funcCompiler) compileBinary(b *parser.BinaryExpr) {
	fc.compileUnary(b.Left)
	for _, op := range b.Right {
		fc.compileExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}})
		switch op.Op {
		case "+":
			fc.emit(OpAdd, 0, 0, Value{})
		case "-":
			fc.emit(OpSub, 0, 0, Value{})
		case "==":
			fc.emit(OpEqual, 0, 0, Value{})
		case "<":
			fc.emit(OpLess, 0, 0, Value{})
		case "<=":
			fc.emit(OpLessEq, 0, 0, Value{})
		}
	}
}

func (fc *funcCompiler) compileUnary(u *parser.Unary) {
	fc.compilePostfix(u.Value)
	for _, op := range u.Ops {
		switch op {
		case "-":
			fc.emit(OpNeg, 0, 0, Value{})
		case "!":
			// not implemented
		}
	}
}
func (fc *funcCompiler) compilePostfix(p *parser.PostfixExpr) {
	fc.compilePrimary(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Start != nil {
				fc.compileExpr(op.Index.Start)
			} else {
				fc.emit(OpPushConst, 0, 0, Value{Tag: ValueInt, Int: 0})
			}
			fc.emit(OpIndex, 0, 0, Value{})
		}
	}
}

func (fc *funcCompiler) compilePrimary(p *parser.Primary) {
	if p.Call != nil {
		for _, arg := range p.Call.Args {
			fc.compileExpr(arg)
		}
		name := p.Call.Func
		if idx, ok := fc.c.fnIdx[name]; ok {
			fc.emit(OpCall, idx, len(p.Call.Args), Value{})
		} else if name == "print" {
			fc.emit(OpCall, -1, len(p.Call.Args), Value{})
		} else if name == "len" {
			fc.emit(OpCall, -2, len(p.Call.Args), Value{})
		} else if name == "json" {
			fc.emit(OpCall, -3, len(p.Call.Args), Value{})
		} else if name == "min" {
			fc.emit(OpCall, -4, len(p.Call.Args), Value{})
		}
	} else if p.Lit != nil {
		switch {
		case p.Lit.Int != nil:
			fc.emit(OpPushConst, 0, 0, Value{Tag: ValueInt, Int: *p.Lit.Int})
		case p.Lit.Str != nil:
			fc.emit(OpPushConst, 0, 0, Value{Tag: ValueStr, Str: *p.Lit.Str})
		case p.Lit.Bool != nil:
			fc.emit(OpPushConst, 0, 0, Value{Tag: ValueBool, Bool: bool(*p.Lit.Bool)})
		case p.Lit.Null:
			fc.emit(OpPushConst, 0, 0, Value{Tag: ValueNull})
		}
	} else if p.List != nil {
		for _, e := range p.List.Elems {
			fc.compileExpr(e)
		}
		fc.emit(OpMakeList, len(p.List.Elems), 0, Value{})
	} else if p.Map != nil {
		for _, it := range p.Map.Items {
			if name, ok := identName(it.Key); ok {
				fc.emit(OpPushConst, 0, 0, Value{Tag: ValueStr, Str: name})
			} else {
				fc.compileExpr(it.Key)
			}
			fc.compileExpr(it.Value)
		}
		fc.emit(OpMakeMap, len(p.Map.Items), 0, Value{})
	} else if p.Query != nil {
		if v, ok := fc.constQuery(p.Query); ok {
			fc.emit(OpPushConst, 0, 0, v)
		}
	} else if p.Selector != nil && len(p.Selector.Tail) == 0 {
		idx := fc.varIndex(p.Selector.Root)
		fc.emit(OpLoad, idx, 0, Value{})
	}
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func containsIOCall(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	return hasCall(e.Binary)
}

func hasCall(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if hasCallUnary(b.Left) {
		return true
	}
	for _, op := range b.Right {
		if containsIOCall(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}) {
			return true
		}
	}
	return false
}

func hasCallUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return hasCallPostfix(u.Value)
}

func hasCallPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if hasCallPrimary(p.Target) {
		return true
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if containsIOCall(op.Index.Start) || containsIOCall(op.Index.End) {
				return true
			}
		}
	}
	return false
}

func hasCallPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Call != nil {
		if p.Call.Func == "print" || p.Call.Func == "json" || p.Call.Func == "input" {
			return true
		}
		for _, a := range p.Call.Args {
			if containsIOCall(a) {
				return true
			}
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			if containsIOCall(e) {
				return true
			}
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			if containsIOCall(it.Key) || containsIOCall(it.Value) {
				return true
			}
		}
	}
	if p.Group != nil {
		if containsIOCall(p.Group) {
			return true
		}
	}
	if p.Query != nil {
		if containsIOCall(p.Query.Source) {
			return true
		}
		for _, f := range p.Query.Froms {
			if containsIOCall(f.Src) {
				return true
			}
		}
		for _, j := range p.Query.Joins {
			if containsIOCall(j.Src) || containsIOCall(j.On) {
				return true
			}
		}
		if containsIOCall(p.Query.Where) || containsIOCall(p.Query.Sort) || containsIOCall(p.Query.Skip) || containsIOCall(p.Query.Take) || containsIOCall(p.Query.Select) {
			return true
		}
	}
	return false
}
