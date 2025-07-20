//go:build slow

package scheme

import (
	"bytes"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

var currentEnv *types.Env
var breakStack []Symbol
var continueStack []Symbol
var gensymCounter int

func pushLoop(breakSym, contSym Symbol) {
	breakStack = append(breakStack, breakSym)
	continueStack = append(continueStack, contSym)
}

func popLoop() {
	if len(breakStack) > 0 {
		breakStack = breakStack[:len(breakStack)-1]
	}
	if len(continueStack) > 0 {
		continueStack = continueStack[:len(continueStack)-1]
	}
}

func currentBreak() Symbol    { return breakStack[len(breakStack)-1] }
func currentContinue() Symbol { return continueStack[len(continueStack)-1] }

func gensym(prefix string) Symbol {
	gensymCounter++
	return Symbol(fmt.Sprintf("%s%d", prefix, gensymCounter))
}

// Node represents a Scheme AST node.
type Node interface{ Emit(io.Writer) }

type Symbol string

func (s Symbol) Emit(w io.Writer) { io.WriteString(w, string(s)) }

type StringLit string

func (s StringLit) Emit(w io.Writer) { fmt.Fprintf(w, "%q", string(s)) }

type IntLit int

func (i IntLit) Emit(w io.Writer) { fmt.Fprintf(w, "%d", int(i)) }

type BoolLit bool

func (b BoolLit) Emit(w io.Writer) {
	if bool(b) {
		io.WriteString(w, "\"true\"")
	} else {
		io.WriteString(w, "\"false\"")
	}
}

type List struct{ Elems []Node }

func (l *List) Emit(w io.Writer) {
	io.WriteString(w, "(")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, " ")
		}
		if e != nil {
			e.Emit(w)
		}
	}
	io.WriteString(w, ")")
}

type Program struct{ Forms []Node }

func (p *Program) Emit(w io.Writer) {
	for i, f := range p.Forms {
		if f != nil {
			f.Emit(w)
		}
		if i < len(p.Forms)-1 {
			io.WriteString(w, "\n")
		}
	}
}

func EmitString(p *Program) []byte {
	var buf bytes.Buffer
	if p != nil {
		p.Emit(&buf)
	}
	return buf.Bytes()
}

func Format(src []byte) []byte {
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	var buf bytes.Buffer
	indent := 0
	for i := 0; i < len(src); i++ {
		ch := src[i]
		switch ch {
		case '(':
			buf.WriteByte(ch)
			indent++
		case ')':
			buf.WriteByte(ch)
			if i+1 < len(src) && src[i+1] != '\n' {
				buf.WriteByte('\n')
				buf.WriteString(strings.Repeat("  ", indent-1))
			}
			indent--
		case '\n':
			buf.WriteByte('\n')
			if i+1 < len(src) && src[i+1] != ')' {
				buf.WriteString(strings.Repeat("  ", indent))
			}
		default:
			buf.WriteByte(ch)
		}
	}
	if buf.Len() > 0 && buf.Bytes()[buf.Len()-1] != '\n' {
		buf.WriteByte('\n')
	}
	return append(header(), buf.Bytes()...)
}

func header() []byte {
	ts := time.Now().UTC()
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	return []byte(fmt.Sprintf(";; Generated on %s\n(import (srfi 1) (srfi 69) (chibi string))\n", ts.In(loc).Format("2006-01-02 15:04 -0700")))
}

func voidSym() Node { return &List{Elems: []Node{Symbol("quote"), Symbol("nil")}} }

func typedDefault(t *parser.TypeRef) Node {
	if t == nil || t.Simple == nil {
		return voidSym()
	}
	switch *t.Simple {
	case "int":
		return IntLit(0)
	case "bool":
		return BoolLit(false)
	case "string":
		return StringLit("")
	case "map":
		return &List{Elems: []Node{Symbol("make-hash-table")}}
	default:
		return voidSym()
	}
}

func convertStmts(stmts []*parser.Statement) ([]Node, error) {
	var forms []Node
	for _, st := range stmts {
		f, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if f != nil {
			if lst, ok := f.(*List); ok && len(lst.Elems) > 0 && lst.Elems[0] == Symbol("begin") {
				forms = append(forms, lst.Elems[1:]...)
			} else {
				forms = append(forms, f)
			}
		}
	}
	return forms, nil
}

func convertIfStmt(is *parser.IfStmt) (Node, error) {
	cond, err := convertParserExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	thenForms, err := convertStmts(is.Then)
	if err != nil {
		return nil, err
	}
	thenNode := &List{Elems: append([]Node{Symbol("begin")}, thenForms...)}
	var elseNode Node = voidSym()
	if is.ElseIf != nil {
		elseNode, err = convertIfStmt(is.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if len(is.Else) > 0 {
		elseForms, err := convertStmts(is.Else)
		if err != nil {
			return nil, err
		}
		elseNode = &List{Elems: append([]Node{Symbol("begin")}, elseForms...)}
	}
	return &List{Elems: []Node{Symbol("if"), cond, thenNode, elseNode}}, nil
}

func convertWhileStmt(ws *parser.WhileStmt) (Node, error) {
	cond, err := convertParserExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	loopSym := gensym("loop")
	breakSym := gensym("break")
	pushLoop(breakSym, loopSym)
	body, err := convertStmts(ws.Body)
	if err != nil {
		popLoop()
		return nil, err
	}
	loopCall := &List{Elems: []Node{loopSym}}
	body = append(body, loopCall)
	bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
	popLoop()
	return &List{Elems: []Node{
		Symbol("let/ec"), breakSym,
		&List{Elems: []Node{
			Symbol("let"), loopSym, &List{Elems: []Node{}},
			&List{Elems: []Node{Symbol("if"), cond, bodyNode, voidSym()}}}},
	}}, nil
}

func convertForStmt(fs *parser.ForStmt) (Node, error) {
	loopSym := gensym("loop")
	pushLoop(Symbol(""), loopSym)
	if fs.RangeEnd != nil {
		start, err := convertParserExpr(fs.Source)
		if err != nil {
			popLoop()
			return nil, err
		}
		end, err := convertParserExpr(fs.RangeEnd)
		if err != nil {
			popLoop()
			return nil, err
		}
		body, err := convertStmts(fs.Body)
		if err != nil {
			popLoop()
			return nil, err
		}
		inc := &List{Elems: []Node{Symbol("+"), Symbol(fs.Name), IntLit(1)}}
		loopCall := &List{Elems: []Node{loopSym, inc}}
		body = append(body, loopCall)
		bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
		cond := &List{Elems: []Node{Symbol("<"), Symbol(fs.Name), end}}
		popLoop()
		return &List{Elems: []Node{
			Symbol("let"), loopSym,
			&List{Elems: []Node{&List{Elems: []Node{Symbol(fs.Name), start}}}},
			&List{Elems: []Node{Symbol("if"), cond, bodyNode, voidSym()}}}}, nil
	}
	iter, err := convertParserExpr(fs.Source)
	if err != nil {
		popLoop()
		return nil, err
	}
	if _, ok := types.ExprType(fs.Source, currentEnv).(types.MapType); ok {
		iter = &List{Elems: []Node{Symbol("hash-table-keys"), iter}}
	}
	elemSym := Symbol(fs.Name)
	body, err := convertStmts(fs.Body)
	if err != nil {
		popLoop()
		return nil, err
	}
	bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
	popLoop()
	return &List{Elems: []Node{
		Symbol("for-each"),
		&List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{elemSym}}, bodyNode}},
		iter,
	}}, nil
}

func convertStmt(st *parser.Statement) (Node, error) {
	switch {
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" && len(st.Expr.Expr.Binary.Right) == 0 {
			args := make([]Node, len(call.Args))
			for i, a := range call.Args {
				n, err := convertParserExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = n
			}
			forms := []Node{Symbol("begin")}
			for i, a := range args {
				forms = append(forms, &List{Elems: []Node{Symbol("display"), a}})
				if i < len(args)-1 {
					forms = append(forms, &List{Elems: []Node{Symbol("display"), StringLit(" ")}})
				}
			}
			forms = append(forms, &List{Elems: []Node{Symbol("newline")}})
			return &List{Elems: forms}, nil
		}
		return nil, fmt.Errorf("unsupported expression statement")
	case st.Let != nil:
		name := st.Let.Name
		var val Node
		if st.Let.Value != nil {
			var err error
			val, err = convertParserExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil {
			val = typedDefault(st.Let.Type)
		} else {
			val = voidSym()
		}
		return &List{Elems: []Node{Symbol("define"), Symbol(name), val}}, nil
	case st.Var != nil:
		name := st.Var.Name
		var val Node
		if st.Var.Value != nil {
			var err error
			val, err = convertParserExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil {
			val = typedDefault(st.Var.Type)
		} else {
			val = voidSym()
		}
		return &List{Elems: []Node{Symbol("define"), Symbol(name), val}}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		val, err := convertParserExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &List{Elems: []Node{Symbol("set!"), Symbol(st.Assign.Name), val}}, nil
	case st.Assign != nil && len(st.Assign.Index) > 0 && len(st.Assign.Field) == 0:
		var target Node = Symbol(st.Assign.Name)
		for i := 0; i < len(st.Assign.Index)-1; i++ {
			idx := st.Assign.Index[i]
			var err error
			target, err = convertIndex(target, &parser.Primary{Selector: &parser.SelectorExpr{Root: st.Assign.Name}}, idx)
			if err != nil {
				return nil, err
			}
		}
		last := st.Assign.Index[len(st.Assign.Index)-1]
		idxNode, err := convertParserExpr(last.Start)
		if err != nil {
			return nil, err
		}
		val, err := convertParserExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		typ, _ := currentEnv.GetVar(st.Assign.Name)
		if _, ok := typ.(types.MapType); ok {
			return &List{Elems: []Node{Symbol("hash-table-set!"), target, idxNode, val}}, nil
		}
		return &List{Elems: []Node{Symbol("list-set!"), target, idxNode, val}}, nil
	case st.Fun != nil:
		params := []Node{}
		for _, p := range st.Fun.Params {
			params = append(params, Symbol(p.Name))
		}
		bodyForms, err := convertStmts(st.Fun.Body)
		if err != nil {
			return nil, err
		}
		var body Node
		if len(bodyForms) == 1 {
			body = bodyForms[0]
		} else {
			body = &List{Elems: append([]Node{Symbol("begin")}, bodyForms...)}
		}
		return &List{Elems: []Node{
			Symbol("define"),
			&List{Elems: append([]Node{Symbol(st.Fun.Name)}, params...)},
			body,
		}}, nil
	case st.Return != nil:
		if st.Return.Value == nil {
			return voidSym(), nil
		}
		return convertParserExpr(st.Return.Value)
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.Type != nil:
		return nil, nil
	case st.Break != nil:
		if len(breakStack) == 0 {
			return nil, fmt.Errorf("break outside loop")
		}
		return &List{Elems: []Node{currentBreak()}}, nil
	case st.Continue != nil:
		if len(continueStack) == 0 {
			return nil, fmt.Errorf("continue outside loop")
		}
		return &List{Elems: []Node{currentContinue()}}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

// Transpile converts a Mochi AST into a minimal Scheme AST supporting
// print statements with string literals.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	currentEnv = env
	p := &Program{}
	for _, st := range prog.Statements {
		form, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if form != nil {
			if lst, ok := form.(*List); ok && len(lst.Elems) > 0 && lst.Elems[0] == Symbol("begin") {
				p.Forms = append(p.Forms, lst.Elems[1:]...)
			} else {
				p.Forms = append(p.Forms, form)
			}
		}
	}
	return p, nil
}

func convertParserExpr(e *parser.Expr) (Node, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := convertParserUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	exprs := []Node{left}
	ops := []string{}
	for _, op := range e.Binary.Right {
		right, err := convertParserPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		for len(ops) > 0 && precedence(ops[len(ops)-1]) >= precedence(op.Op) {
			r := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			l := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			o := ops[len(ops)-1]
			ops = ops[:len(ops)-1]
			exprs = append(exprs, makeBinary(o, l, r))
		}
		ops = append(ops, op.Op)
		exprs = append(exprs, right)
	}
	for len(ops) > 0 {
		r := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		l := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		o := ops[len(ops)-1]
		ops = ops[:len(ops)-1]
		exprs = append(exprs, makeBinary(o, l, r))
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("expr reduce error")
	}
	return exprs[0], nil
}

func convertParserUnary(u *parser.Unary) (Node, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := convertParserPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			expr = &List{Elems: []Node{Symbol("-"), expr}}
		case "!":
			expr = &List{Elems: []Node{Symbol("not"), expr}}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertParserPostfix(pf *parser.PostfixExpr) (Node, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	var node Node
	var err error
	// handle selector like `x.contains()` parsed as part of target
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		rootPrim := &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
		node, err = convertParserPrimary(rootPrim)
		if err != nil {
			return nil, err
		}
		call := pf.Ops[0].Call
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		arg, err := convertParserExpr(call.Args[0])
		if err != nil {
			return nil, err
		}
		node = &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string-contains"), node, arg}}, BoolLit(true), BoolLit(false)}}
		pf = &parser.PostfixExpr{Target: rootPrim, Ops: pf.Ops[1:]}
	} else {
		node, err = convertParserPrimary(pf.Target)
		if err != nil {
			return nil, err
		}
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Call != nil:
			var err error
			node, err = convertCall(node, op.Call)
			if err != nil {
				return nil, err
			}
		case op.Index != nil:
			var err error
			var orig *parser.Primary
			if i == 0 {
				orig = pf.Target
			}
			node, err = convertIndex(node, orig, op.Index)
			if err != nil {
				return nil, err
			}
		case op.Cast != nil:
			t := op.Cast.Type
			if t.Simple != nil && *t.Simple == "int" {
				node = &List{Elems: []Node{Symbol("string->number"), node}}
			} else if t.Simple != nil && *t.Simple == "string" {
				node = &List{Elems: []Node{Symbol("number->string"), node}}
			} // ignore other casts
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertParserExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			if i == 0 && types.IsStringPrimary(pf.Target, currentEnv) {
				node = &List{Elems: []Node{Symbol("string-contains"), node, arg}}
			} else {
				node = &List{Elems: []Node{
					Symbol("if"),
					&List{Elems: []Node{Symbol("string?"), node}},
					&List{Elems: []Node{Symbol("string-contains"), node, arg}},
					&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), arg, node}}, BoolLit(true), BoolLit(false)}},
				}}
			}
			i++
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return node, nil
}

func convertParserPrimary(p *parser.Primary) (Node, error) {
	switch {
	case p.Lit != nil && p.Lit.Int != nil:
		return IntLit(int(*p.Lit.Int)), nil
	case p.Lit != nil && p.Lit.Str != nil:
		return StringLit(*p.Lit.Str), nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return BoolLit(bool(*p.Lit.Bool)), nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if currentEnv != nil {
			if _, err := currentEnv.GetVar(p.Selector.Root); err == nil {
				return Symbol(p.Selector.Root), nil
			}
		}
		return StringLit(p.Selector.Root), nil
	case p.Selector != nil:
		var node Node = Symbol(p.Selector.Root)
		for _, f := range p.Selector.Tail {
			node = &List{Elems: []Node{Symbol("hash-table-ref"), node, StringLit(f)}}
		}
		return node, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Group != nil:
		return convertParserExpr(p.Group)
	case p.List != nil:
		elems := []Node{}
		for _, e := range p.List.Elems {
			n, err := convertParserExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, n)
		}
		return &List{Elems: append([]Node{Symbol("list")}, elems...)}, nil
	case p.Map != nil:
		pairs := []Node{Symbol("list")}
		for _, it := range p.Map.Items {
			var k Node
			if s, ok := types.SimpleStringKey(it.Key); ok {
				k = StringLit(s)
			} else {
				var err error
				k, err = convertParserExpr(it.Key)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertParserExpr(it.Value)
			if err != nil {
				return nil, err
			}
			pair := &List{Elems: []Node{Symbol("cons"), k, v}}
			pairs = append(pairs, pair)
		}
		return &List{Elems: []Node{Symbol("alist->hash-table"), &List{Elems: pairs}}}, nil
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr)
	case p.Call != nil:
		return convertCall(Symbol(p.Call.Func), &parser.CallOp{Args: p.Call.Args})
	}
	return nil, fmt.Errorf("unsupported primary")
}

func convertIfExpr(ie *parser.IfExpr) (Node, error) {
	cond, err := convertParserExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenNode, err := convertParserExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseNode Node = voidSym()
	if ie.ElseIf != nil {
		elseNode, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseNode, err = convertParserExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	}
	return &List{Elems: []Node{Symbol("if"), cond, thenNode, elseNode}}, nil
}

func convertFunExpr(fe *parser.FunExpr) (Node, error) {
	params := []Node{}
	for _, p := range fe.Params {
		params = append(params, Symbol(p.Name))
	}
	var body Node
	if fe.ExprBody != nil {
		b, err := convertParserExpr(fe.ExprBody)
		if err != nil {
			return nil, err
		}
		body = b
	} else {
		stmts, err := convertStmts(fe.BlockBody)
		if err != nil {
			return nil, err
		}
		if len(stmts) == 1 {
			body = stmts[0]
		} else {
			body = &List{Elems: append([]Node{Symbol("begin")}, stmts...)}
		}
	}
	return &List{Elems: []Node{Symbol("lambda"), &List{Elems: params}, body}}, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Node, error) {
	prevEnv := currentEnv
	env := types.NewEnv(currentEnv)

	srcType := types.ExprType(q.Source, currentEnv)
	var elemType types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemType = lt.Elem
	} else if gt, ok := srcType.(types.GroupType); ok {
		elemType = gt.Elem
	}
	env.SetVar(q.Var, elemType, true)
	for _, f := range q.Froms {
		ft := types.ExprType(f.Src, currentEnv)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		env.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := types.ExprType(j.Src, currentEnv)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		env.SetVar(j.Var, je, true)
	}
	currentEnv = env

	loops := []struct {
		name   string
		source Node
	}{}
	src, err := convertParserExpr(q.Source)
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	loops = append(loops, struct {
		name   string
		source Node
	}{q.Var, src})
	for _, j := range q.Joins {
		src, err := convertParserExpr(j.Src)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		loops = append(loops, struct {
			name   string
			source Node
		}{j.Var, src})
	}
	for _, f := range q.Froms {
		src, err := convertParserExpr(f.Src)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		loops = append(loops, struct {
			name   string
			source Node
		}{f.Var, src})
	}
	sel, err := convertParserExpr(q.Select)
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	var cond Node
	if q.Where != nil {
		cond, err = convertParserExpr(q.Where)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
	}
	for _, j := range q.Joins {
		jc, err := convertParserExpr(j.On)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = makeBinary("&&", cond, jc)
		}
	}
	resSym := gensym("res")
	appendNode := Node(&List{Elems: []Node{
		Symbol("set!"), resSym,
		&List{Elems: []Node{
			Symbol("append"), resSym,
			&List{Elems: []Node{Symbol("list"), sel}},
		}},
	}})
	if cond != nil {
		appendNode = &List{Elems: []Node{Symbol("if"), cond, appendNode, voidSym()}}
	}
	body := appendNode
	for i := len(loops) - 1; i >= 0; i-- {
		l := loops[i]
		body = &List{Elems: []Node{
			Symbol("for-each"),
			&List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol(l.name)}}, body}},
			l.source,
		}}
	}
	currentEnv = prevEnv
	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{&List{Elems: []Node{resSym, &List{Elems: []Node{Symbol("list")}}}}}},
		&List{Elems: []Node{Symbol("begin"), body, resSym}},
	}}, nil
}

func makeBinary(op string, left, right Node) Node {
	isStr := func(n Node) bool {
		switch n.(type) {
		case StringLit:
			return true
		}
		return false
	}
	switch op {
	case "+":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string-append"), left, right}}
		}
		return &List{Elems: []Node{Symbol("+"), left, right}}
	case "-", "*":
		return &List{Elems: []Node{Symbol(op), left, right}}
	case "/":
		div := &List{Elems: []Node{Symbol("/"), left, right}}
		return &List{Elems: []Node{Symbol("exact->inexact"), div}}
	case "%":
		return &List{Elems: []Node{Symbol("modulo"), left, right}}
	case "<":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string<?"), left, right}}
		}
		return &List{Elems: []Node{Symbol("<"), left, right}}
	case "<=":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string<=?"), left, right}}
		}
		return &List{Elems: []Node{Symbol("<="), left, right}}
	case ">":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string>?"), left, right}}
		}
		return &List{Elems: []Node{Symbol(">"), left, right}}
	case ">=":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string>=?"), left, right}}
		}
		return &List{Elems: []Node{Symbol(">="), left, right}}
	case "==":
		return &List{Elems: []Node{Symbol("="), left, right}}
	case "!=":
		return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("="), left, right}}}}
	case "&&":
		return &List{Elems: []Node{Symbol("and"), left, right}}
	case "||":
		return &List{Elems: []Node{Symbol("or"), left, right}}
	case "in":
		return &List{Elems: []Node{
			Symbol("cond"),
			&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), right}}, &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string-contains"), right, left}}, StringLit("true"), StringLit("false")}}}},
			&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), right}}, &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("hash-table-exists?"), right, left}}, StringLit("true"), StringLit("false")}}}},
			&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), left, right}}, StringLit("true"), StringLit("false")}}}},
		}}
	default:
		return &List{Elems: []Node{Symbol(op), left, right}}
	}
}

func precedence(op string) int {
	switch op {
	case "||":
		return 1
	case "&&":
		return 2
	case "==", "!=", "<", "<=", ">", ">=", "in":
		return 3
	case "+", "-":
		return 4
	case "*", "/", "%":
		return 5
	default:
		return 0
	}
}

func convertCall(target Node, call *parser.CallOp) (Node, error) {
	sym, ok := target.(Symbol)
	if !ok {
		return nil, fmt.Errorf("unsupported call")
	}
	args := make([]Node, len(call.Args))
	for i, a := range call.Args {
		n, err := convertParserExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = n
	}
	switch sym {
	case "len", "count":
		if len(args) != 1 {
			return nil, fmt.Errorf("len expects 1 arg")
		}
		switch types.ExprType(call.Args[0], currentEnv).(type) {
		case types.StringType:
			return &List{Elems: []Node{Symbol("string-length"), args[0]}}, nil
		case types.ListType:
			return &List{Elems: []Node{Symbol("length"), args[0]}}, nil
		case types.MapType:
			return &List{Elems: []Node{Symbol("hash-table-size"), args[0]}}, nil
		default:
			return &List{Elems: []Node{
				Symbol("cond"),
				&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), args[0]}}, &List{Elems: []Node{Symbol("string-length"), args[0]}}}},
				&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), args[0]}}, &List{Elems: []Node{Symbol("hash-table-size"), args[0]}}}},
				&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("length"), args[0]}}}},
			}}, nil
		}
	case "append":
		if len(args) != 2 {
			return nil, fmt.Errorf("append expects 2 args")
		}
		if _, ok := types.ExprType(call.Args[0], currentEnv).(types.StringType); ok {
			return &List{Elems: []Node{Symbol("string-append"), args[0], args[1]}}, nil
		}
		return &List{Elems: []Node{Symbol("append"), args[0], &List{Elems: []Node{Symbol("list"), args[1]}}}}, nil
	case "sum":
		if len(args) != 1 {
			return nil, fmt.Errorf("sum expects 1 arg")
		}
		return &List{Elems: []Node{Symbol("apply"), Symbol("+"), args[0]}}, nil
	case "avg":
		if len(args) != 1 {
			return nil, fmt.Errorf("avg expects 1 arg")
		}
		xs := Symbol("xs")
		div := &List{Elems: []Node{Symbol("/"),
			&List{Elems: []Node{Symbol("apply"), Symbol("+"), xs}},
			&List{Elems: []Node{Symbol("length"), xs}},
		}}
		return &List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{&List{Elems: []Node{xs, args[0]}}}},
			&List{Elems: []Node{Symbol("exact->inexact"), div}},
		}}, nil
	case "str":
		if len(args) != 1 {
			return nil, fmt.Errorf("str expects 1 arg")
		}
		return &List{Elems: []Node{Symbol("number->string"), args[0]}}, nil
	case "min", "max":
		if len(args) != 1 {
			return nil, fmt.Errorf("%s expects 1 arg", sym)
		}
		return &List{Elems: []Node{Symbol("apply"), Symbol(string(sym)), args[0]}}, nil
	case "substring":
		if len(args) != 3 {
			return nil, fmt.Errorf("substring expects 3 args")
		}
		return &List{Elems: []Node{Symbol("substring"), args[0], args[1], args[2]}}, nil
	default:
		elems := make([]Node, 0, len(args)+1)
		elems = append(elems, Symbol(sym))
		elems = append(elems, args...)
		return &List{Elems: elems}, nil
	}
}

func convertIndex(target Node, orig *parser.Primary, idx *parser.IndexOp) (Node, error) {
	if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
		var start Node = IntLit(0)
		if idx.Start != nil {
			n, err := convertParserExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			start = n
		}
		var end Node
		if idx.End != nil {
			n, err := convertParserExpr(idx.End)
			if err != nil {
				return nil, err
			}
			end = n
		} else {
			end = &List{Elems: []Node{Symbol("length"), target}}
		}
		lenNode := &List{Elems: []Node{Symbol("-"), end, start}}
		if orig != nil {
			switch types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: orig}}}}, currentEnv).(type) {
			case types.StringType:
				return &List{Elems: []Node{Symbol("substring"), target, start, end}}, nil
			case types.ListType:
				return &List{Elems: []Node{Symbol("take"), &List{Elems: []Node{Symbol("drop"), target, start}}, lenNode}}, nil
			}
		}
		return &List{Elems: []Node{
			Symbol("if"),
			&List{Elems: []Node{Symbol("string?"), target}},
			&List{Elems: []Node{Symbol("substring"), target, start, end}},
			&List{Elems: []Node{Symbol("take"), &List{Elems: []Node{Symbol("drop"), target, start}}, lenNode}},
		}}, nil
	}
	if idx.Start == nil {
		return nil, fmt.Errorf("unsupported index")
	}
	in, err := convertParserExpr(idx.Start)
	if err != nil {
		return nil, err
	}
	if orig != nil {
		switch types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: orig}}}}, currentEnv).(type) {
		case types.StringType:
			return &List{Elems: []Node{Symbol("string-ref"), target, in}}, nil
		case types.ListType:
			return &List{Elems: []Node{Symbol("list-ref"), target, in}}, nil
		case types.MapType:
			return &List{Elems: []Node{Symbol("hash-table-ref"), target, in}}, nil
		}
	}
	return &List{Elems: []Node{
		Symbol("cond"),
		&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), target}}, &List{Elems: []Node{Symbol("string-ref"), target, in}}}},
		&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), target}}, &List{Elems: []Node{Symbol("hash-table-ref"), target, in}}}},
		&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("list-ref"), target, in}}}},
	}}, nil
}
