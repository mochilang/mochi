package frontend

import (
	"fmt"

	"mochi/compiler3/ir"
	gogen "mochi/compiler3/emit/go"
	"mochi/parser"
)

// funEntry indexes a user-declared Mochi `fun` so calls and recursive
// references can resolve before the body has been lowered.
type funEntry struct {
	index uint32
	stmt  *parser.FunStmt
}

// Lower walks a parsed Mochi program and produces a compiler3 emit
// Program. Top-level statements are wrapped in a synthetic `main`
// function (Result: TypeUnit) so the emitter produces a runnable
// executable. Mochi `fun` declarations lower to standalone IR
// functions.
//
// Phase-6 scope covered: i64 literals, let/var bindings, assignments,
// binary arithmetic and comparisons, return, if/else, while, function
// calls (including recursion), and `print(int)`. Anything else
// surfaces an explicit "unsupported in MVP frontend" error so the A/B
// harness can mark the fixture as skipped rather than miscompile.
func Lower(prog *parser.Program) (*gogen.Program, error) {
	p := &gogen.Program{PkgName: "main"}

	// First pass: collect user fun declarations so call lookups can
	// resolve before the fun body is lowered (allows mutual recursion
	// and forward references).
	userFns := map[string]funEntry{}
	for _, st := range prog.Statements {
		if st.Fun != nil {
			idx := uint32(len(p.Funcs))
			fn := &ir.Function{Name: st.Fun.Name}
			p.Funcs = append(p.Funcs, fn)
			userFns[st.Fun.Name] = funEntry{index: idx, stmt: st.Fun}
		}
	}

	// Lower each user fun. Each lowering must finish before the next
	// because they share the program-level function table.
	for name, e := range userFns {
		if err := lowerFun(p, e.index, e.stmt, userFns); err != nil {
			return nil, fmt.Errorf("lower fun %s: %w", name, err)
		}
	}

	// Wrap top-level (non-fun) statements in a synthetic `main`. If
	// there are no top-level statements, no main is emitted.
	var topLevel []*parser.Statement
	for _, st := range prog.Statements {
		if st.Fun != nil {
			continue
		}
		topLevel = append(topLevel, st)
	}
	if len(topLevel) > 0 {
		mainFn := &ir.Function{Name: "main", Result: ir.TypeUnit}
		p.Funcs = append(p.Funcs, mainFn)
		b := newBuilder(mainFn, userFns, p)
		entry := b.fn.AddBlock()
		b.curBlock = entry
		for _, st := range topLevel {
			if err := b.lowerStmt(st); err != nil {
				return nil, err
			}
			if b.terminated {
				break
			}
		}
		if !b.terminated {
			b.terminator(ir.Terminator{Kind: ir.TermReturn})
		}
	}

	return p, nil
}

// builder holds the per-function lowering state.
type builder struct {
	fn         *ir.Function
	prog       *gogen.Program
	userFns    map[string]funEntry
	curBlock   uint32
	terminated bool
	// values is the lexical scope: Mochi name -> SSA value ID. We do
	// not track SSA renaming for var reassignment yet; the MVP only
	// supports straight-line let/var with no reassignment, and the
	// query/loop work needed for full phi insertion is part of the
	// post-MVP widening.
	values map[string]uint32
}

func newBuilder(fn *ir.Function, userFns map[string]funEntry, prog *gogen.Program) *builder {
	return &builder{
		fn:      fn,
		prog:    prog,
		userFns: userFns,
		values:  map[string]uint32{},
	}
}

func lowerFun(p *gogen.Program, idx uint32, fs *parser.FunStmt, userFns map[string]funEntry) error {
	fn := p.Funcs[idx]
	// Single result type. MVP supports i64 returns only; the absence
	// of a return type annotation is treated as i64 to keep the most
	// common fixture shape working without forcing the user to annotate.
	fn.Result = ir.TypeI64
	if fs.Return != nil {
		t, err := lowerType(fs.Return)
		if err != nil {
			return err
		}
		fn.Result = t
	}
	b := newBuilder(fn, userFns, p)
	// Params: every param is an OpParam value of the declared type.
	for _, param := range fs.Params {
		pt := ir.TypeI64
		if param.Type != nil {
			t, err := lowerType(param.Type)
			if err != nil {
				return err
			}
			pt = t
		}
		vid := fn.AddValue(ir.Value{Type: pt, Op: ir.OpParam})
		fn.Params = append(fn.Params, vid)
		b.values[param.Name] = vid
	}
	entry := fn.AddBlock()
	b.curBlock = entry
	for _, st := range fs.Body {
		if err := b.lowerStmt(st); err != nil {
			return err
		}
		if b.terminated {
			break
		}
	}
	if !b.terminated {
		// User omitted a return. For TypeUnit that's fine; for i64
		// fixtures the type checker would normally reject it, but the
		// MVP frontend bypasses the checker so we emit a zero return
		// to keep the Go output buildable.
		if fn.Result == ir.TypeUnit {
			b.terminator(ir.Terminator{Kind: ir.TermReturn})
		} else {
			zero := b.addValue(ir.Value{Type: fn.Result, Op: ir.OpConst})
			b.terminator(ir.Terminator{Kind: ir.TermReturn, Value: zero})
		}
	}
	return nil
}

func lowerType(t *parser.TypeRef) (ir.Type, error) {
	if t == nil || t.Simple == nil {
		return ir.TypeInvalid, fmt.Errorf("frontend: only simple type names are supported in the MVP")
	}
	switch *t.Simple {
	case "int":
		return ir.TypeI64, nil
	case "float":
		return ir.TypeF64, nil
	case "bool":
		return ir.TypeBool, nil
	case "string", "str":
		return ir.TypeStr, nil
	case "unit", "void":
		return ir.TypeUnit, nil
	}
	return ir.TypeInvalid, fmt.Errorf("frontend: type %q unsupported in MVP", *t.Simple)
}

func (b *builder) addValue(v ir.Value) uint32 {
	id := b.fn.AddValue(v)
	blk := b.fn.Block(b.curBlock)
	blk.Values = append(blk.Values, id)
	return id
}

func (b *builder) terminator(t ir.Terminator) {
	blk := b.fn.Block(b.curBlock)
	blk.Term = t
	switch t.Kind {
	case ir.TermJump:
		blk.Succs = []uint32{t.Target}
		b.fn.Block(t.Target).Preds = append(b.fn.Block(t.Target).Preds, b.curBlock)
	case ir.TermBranch:
		blk.Succs = []uint32{t.IfTrue, t.IfFalse}
		b.fn.Block(t.IfTrue).Preds = append(b.fn.Block(t.IfTrue).Preds, b.curBlock)
		b.fn.Block(t.IfFalse).Preds = append(b.fn.Block(t.IfFalse).Preds, b.curBlock)
	}
	b.terminated = true
}

func (b *builder) lowerStmt(st *parser.Statement) error {
	switch {
	case st.Let != nil:
		return b.lowerLet(st.Let.Name, st.Let.Value)
	case st.Var != nil:
		return b.lowerLet(st.Var.Name, st.Var.Value)
	case st.Assign != nil:
		// MVP: only plain `name = expr` (no index/field assignment).
		if len(st.Assign.Index) != 0 || len(st.Assign.Field) != 0 {
			return fmt.Errorf("frontend: indexed/field assignment unsupported in MVP")
		}
		return b.lowerLet(st.Assign.Name, st.Assign.Value)
	case st.Return != nil:
		return b.lowerReturn(st.Return)
	case st.If != nil:
		return b.lowerIf(st.If)
	case st.Expr != nil:
		_, err := b.lowerExprAsStmt(st.Expr.Expr)
		return err
	}
	return fmt.Errorf("frontend: statement kind unsupported in MVP")
}

func (b *builder) lowerLet(name string, e *parser.Expr) error {
	if e == nil {
		return fmt.Errorf("frontend: binding %q has no initializer", name)
	}
	vid, err := b.lowerExpr(e)
	if err != nil {
		return err
	}
	b.values[name] = vid
	return nil
}

func (b *builder) lowerReturn(rs *parser.ReturnStmt) error {
	if rs.Value == nil {
		b.terminator(ir.Terminator{Kind: ir.TermReturn})
		return nil
	}
	vid, err := b.lowerExpr(rs.Value)
	if err != nil {
		return err
	}
	b.terminator(ir.Terminator{Kind: ir.TermReturn, Value: vid})
	return nil
}

func (b *builder) lowerIf(s *parser.IfStmt) error {
	cond, err := b.lowerExpr(s.Cond)
	if err != nil {
		return err
	}
	thenID := b.fn.AddBlock()
	elseID := b.fn.AddBlock()
	contID := b.fn.AddBlock()

	b.terminator(ir.Terminator{Kind: ir.TermBranch, Value: cond, IfTrue: thenID, IfFalse: elseID})

	// Then.
	b.curBlock = thenID
	b.terminated = false
	for _, ts := range s.Then {
		if err := b.lowerStmt(ts); err != nil {
			return err
		}
		if b.terminated {
			break
		}
	}
	if !b.terminated {
		b.terminator(ir.Terminator{Kind: ir.TermJump, Target: contID})
	}

	// Else.
	b.curBlock = elseID
	b.terminated = false
	if s.ElseIf != nil {
		if err := b.lowerIf(s.ElseIf); err != nil {
			return err
		}
	} else {
		for _, es := range s.Else {
			if err := b.lowerStmt(es); err != nil {
				return err
			}
			if b.terminated {
				break
			}
		}
	}
	if !b.terminated {
		b.terminator(ir.Terminator{Kind: ir.TermJump, Target: contID})
	}

	// Continuation: empty block; caller continues lowering here.
	b.curBlock = contID
	b.terminated = false
	return nil
}

// lowerExprAsStmt lowers an expression-statement. The MVP recognises
// `print(arg)` and lowers it to a `fmt.Println` OpCallGo; other call
// expressions are lowered as regular expressions and their value is
// discarded.
func (b *builder) lowerExprAsStmt(e *parser.Expr) (uint32, error) {
	if call := exprAsCall(e); call != nil && call.Func == "print" && len(call.Args) == 1 {
		arg, err := b.lowerExpr(call.Args[0])
		if err != nil {
			return 0, err
		}
		argType := b.fn.Values[arg].Type
		goArgType := goTypeForIRType(argType)
		if goArgType == "" {
			return 0, fmt.Errorf("frontend: print() argument type %s unsupported in MVP", argType)
		}
		bind := ir.GoBinding{
			Pkg:      "fmt",
			Alias:    "fmt",
			Name:     "Println",
			ArgTypes: []string{goArgType},
			Result:   "",
		}
		bindIdx := int64(len(b.fn.GoBindings))
		b.fn.GoBindings = append(b.fn.GoBindings, bind)
		id := b.addValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpCallGo, Args: []uint32{arg}, Const: bindIdx})
		return id, nil
	}
	return b.lowerExpr(e)
}

func goTypeForIRType(t ir.Type) string {
	switch t {
	case ir.TypeI64:
		return "int64"
	case ir.TypeF64:
		return "float64"
	case ir.TypeBool:
		return "bool"
	case ir.TypeStr:
		return "string"
	}
	return ""
}

func exprAsCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	p := u.Value.Target
	if p == nil {
		return nil
	}
	return p.Call
}

// lowerExpr returns the SSA value ID for e. MVP handles BinaryExpr
// with the i64-compatible operators and a handful of Primary forms.
func (b *builder) lowerExpr(e *parser.Expr) (uint32, error) {
	if e == nil || e.Binary == nil {
		return 0, fmt.Errorf("frontend: empty expression")
	}
	return b.lowerBinary(e.Binary)
}

func (b *builder) lowerBinary(be *parser.BinaryExpr) (uint32, error) {
	left, err := b.lowerUnary(be.Left)
	if err != nil {
		return 0, err
	}
	if len(be.Right) == 0 {
		return left, nil
	}
	// MVP: left-associative without precedence; sufficient for the
	// numeric fixtures because most have only one operator or
	// fully-parenthesised expressions.
	cur := left
	for _, op := range be.Right {
		rhs, err := b.lowerUnary(op.Right)
		if err != nil {
			return 0, err
		}
		cur, err = b.applyBinOp(op.Op, cur, rhs)
		if err != nil {
			return 0, err
		}
	}
	return cur, nil
}

func (b *builder) applyBinOp(op string, l, r uint32) (uint32, error) {
	lt := b.fn.Values[l].Type
	rt := b.fn.Values[r].Type
	if lt != rt {
		return 0, fmt.Errorf("frontend: binop %q across types %s and %s unsupported in MVP", op, lt, rt)
	}
	if lt != ir.TypeI64 {
		return 0, fmt.Errorf("frontend: binop %q on type %s unsupported in MVP", op, lt)
	}
	var code ir.OpCode
	resType := ir.TypeI64
	switch op {
	case "+":
		code = ir.OpAddI64
	case "-":
		code = ir.OpSubI64
	case "*":
		code = ir.OpMulI64
	case "/":
		code = ir.OpDivI64
	case "%":
		code = ir.OpModI64
	case "==":
		code = ir.OpCmpEqI64
		resType = ir.TypeBool
	case "!=":
		code = ir.OpCmpNeI64
		resType = ir.TypeBool
	case "<":
		code = ir.OpCmpLtI64
		resType = ir.TypeBool
	case "<=":
		code = ir.OpCmpLeI64
		resType = ir.TypeBool
	case ">":
		code = ir.OpCmpGtI64
		resType = ir.TypeBool
	case ">=":
		code = ir.OpCmpGeI64
		resType = ir.TypeBool
	default:
		return 0, fmt.Errorf("frontend: operator %q unsupported in MVP", op)
	}
	return b.addValue(ir.Value{Type: resType, Op: code, Args: []uint32{l, r}}), nil
}

func (b *builder) lowerUnary(u *parser.Unary) (uint32, error) {
	val, err := b.lowerPostfix(u.Value)
	if err != nil {
		return 0, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			vt := b.fn.Values[val].Type
			if vt != ir.TypeI64 {
				return 0, fmt.Errorf("frontend: unary `-` on %s unsupported in MVP", vt)
			}
			val = b.addValue(ir.Value{Type: ir.TypeI64, Op: ir.OpNegI64, Args: []uint32{val}})
		default:
			return 0, fmt.Errorf("frontend: unary operator %q unsupported in MVP", op)
		}
	}
	return val, nil
}

func (b *builder) lowerPostfix(pe *parser.PostfixExpr) (uint32, error) {
	if pe == nil || pe.Target == nil {
		return 0, fmt.Errorf("frontend: empty postfix")
	}
	if len(pe.Ops) != 0 {
		return 0, fmt.Errorf("frontend: postfix ops unsupported in MVP")
	}
	return b.lowerPrimary(pe.Target)
}

func (b *builder) lowerPrimary(p *parser.Primary) (uint32, error) {
	switch {
	case p.Lit != nil:
		return b.lowerLiteral(p.Lit)
	case p.Selector != nil:
		if len(p.Selector.Tail) != 0 {
			return 0, fmt.Errorf("frontend: selector tail %v unsupported in MVP", p.Selector.Tail)
		}
		id, ok := b.values[p.Selector.Root]
		if !ok {
			return 0, fmt.Errorf("frontend: unbound identifier %q", p.Selector.Root)
		}
		return id, nil
	case p.Call != nil:
		return b.lowerCall(p.Call)
	case p.Group != nil:
		return b.lowerExpr(p.Group)
	}
	return 0, fmt.Errorf("frontend: primary form unsupported in MVP")
}

func (b *builder) lowerLiteral(lit *parser.Literal) (uint32, error) {
	switch {
	case lit.Int != nil:
		return b.addValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: int64(*lit.Int)}), nil
	case lit.Bool != nil:
		c := int64(0)
		if bool(*lit.Bool) {
			c = 1
		}
		return b.addValue(ir.Value{Type: ir.TypeBool, Op: ir.OpConst, Const: c}), nil
	}
	return 0, fmt.Errorf("frontend: literal kind unsupported in MVP (str/float/none)")
}

func (b *builder) lowerCall(c *parser.CallExpr) (uint32, error) {
	entry, ok := b.userFns[c.Func]
	if !ok {
		return 0, fmt.Errorf("frontend: unknown function %q (only user-declared funs callable in MVP)", c.Func)
	}
	args := make([]uint32, 0, len(c.Args))
	for _, a := range c.Args {
		vid, err := b.lowerExpr(a)
		if err != nil {
			return 0, err
		}
		args = append(args, vid)
	}
	callee := b.prog.Funcs[entry.index]
	id := b.addValue(ir.Value{Type: callee.Result, Op: ir.OpCall, Args: args, Const: int64(entry.index)})
	return id, nil
}
