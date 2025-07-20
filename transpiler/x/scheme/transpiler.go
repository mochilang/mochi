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
		io.WriteString(w, "#t")
	} else {
		io.WriteString(w, "#f")
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
	return append(header(), src...)
}

func header() []byte {
	ts := time.Now().UTC()
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	return []byte(fmt.Sprintf(";; Generated on %s\n(import (srfi 1))\n", ts.In(loc).Format("2006-01-02 15:04 -0700")))
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
			forms = append(forms, f)
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
	body, err := convertStmts(ws.Body)
	if err != nil {
		return nil, err
	}
	loopCall := &List{Elems: []Node{Symbol("loop")}}
	body = append(body, loopCall)
	bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
	return &List{Elems: []Node{
		Symbol("let"), Symbol("loop"), &List{Elems: []Node{}},
		&List{Elems: []Node{Symbol("if"), cond, bodyNode, voidSym()}},
	}}, nil
}

func convertForStmt(fs *parser.ForStmt) (Node, error) {
	if fs.RangeEnd != nil {
		start, err := convertParserExpr(fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertParserExpr(fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmts(fs.Body)
		if err != nil {
			return nil, err
		}
		inc := &List{Elems: []Node{Symbol("+"), Symbol(fs.Name), IntLit(1)}}
		loopCall := &List{Elems: []Node{Symbol("loop"), inc}}
		body = append(body, loopCall)
		bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
		cond := &List{Elems: []Node{Symbol("<"), Symbol(fs.Name), end}}
		return &List{Elems: []Node{
			Symbol("let"), Symbol("loop"),
			&List{Elems: []Node{&List{Elems: []Node{Symbol(fs.Name), start}}}},
			&List{Elems: []Node{Symbol("if"), cond, bodyNode, voidSym()}},
		}}, nil
	}
	iter, err := convertParserExpr(fs.Source)
	if err != nil {
		return nil, err
	}
	body, err := convertStmts(fs.Body)
	if err != nil {
		return nil, err
	}
	bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
	lambda := &List{Elems: []Node{
		Symbol("lambda"), &List{Elems: []Node{Symbol(fs.Name)}}, bodyNode,
	}}
	return &List{Elems: []Node{Symbol("for-each"), lambda, iter}}, nil
}

func convertStmt(st *parser.Statement) (Node, error) {
	switch {
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" && len(call.Args) == 1 && len(st.Expr.Expr.Binary.Right) == 0 {
			argExpr := call.Args[0]
			arg, err := convertParserExpr(argExpr)
			if err != nil {
				return nil, err
			}
			return &List{Elems: []Node{Symbol("begin"), &List{Elems: []Node{Symbol("display"), arg}}, &List{Elems: []Node{Symbol("newline")}}}}, nil
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
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

// Transpile converts a Mochi AST into a minimal Scheme AST supporting
// print statements with string literals.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env
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
	node, err := convertParserPrimary(pf.Target)
	if err != nil {
		return nil, err
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
			node, err = convertIndex(node, op.Index)
			if err != nil {
				return nil, err
			}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertParserExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			node = &List{Elems: []Node{
				Symbol("if"),
				&List{Elems: []Node{Symbol("string?"), node}},
				&List{Elems: []Node{Symbol("string-contains"), node, arg}},
				&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), arg, node}}, BoolLit(true), BoolLit(false)}},
			}}
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
		return Symbol(p.Selector.Root), nil
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
	case "-", "*", "/":
		return &List{Elems: []Node{Symbol(op), left, right}}
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
			Symbol("if"),
			&List{Elems: []Node{Symbol("string?"), right}},
			&List{Elems: []Node{Symbol("string-contains"), right, left}},
			&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), left, right}}, BoolLit(true), BoolLit(false)}},
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
		return &List{Elems: []Node{
			Symbol("if"),
			&List{Elems: []Node{Symbol("string?"), args[0]}},
			&List{Elems: []Node{Symbol("string-length"), args[0]}},
			&List{Elems: []Node{Symbol("length"), args[0]}},
		}}, nil
	case "append":
		if len(args) != 2 {
			return nil, fmt.Errorf("append expects 2 args")
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
		return &List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{&List{Elems: []Node{xs, args[0]}}}},
			&List{Elems: []Node{Symbol("/"),
				&List{Elems: []Node{Symbol("apply"), Symbol("+"), xs}},
				&List{Elems: []Node{Symbol("length"), xs}},
			}},
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
		return nil, fmt.Errorf("unsupported call")
	}
}

func convertIndex(target Node, idx *parser.IndexOp) (Node, error) {
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
	return &List{Elems: []Node{
		Symbol("if"),
		&List{Elems: []Node{Symbol("string?"), target}},
		&List{Elems: []Node{Symbol("string-ref"), target, in}},
		&List{Elems: []Node{Symbol("list-ref"), target, in}},
	}}, nil
}
