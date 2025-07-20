//go:build slow

package cljt

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"

	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

// --- Simple Clojure AST ---

// Node represents any Clojure AST node that can be emitted as code.
type Node interface {
	Emit(io.Writer)
}

// Symbol represents a Clojure symbol.
type Symbol string

func (s Symbol) Emit(w io.Writer) {
	io.WriteString(w, string(s))
}

// StringLit represents a quoted string literal.
type StringLit string

func (s StringLit) Emit(w io.Writer) {
	fmt.Fprintf(w, "%q", string(s))
}

// IntLit represents an integer literal.
type IntLit int64

func (i IntLit) Emit(w io.Writer) {
	io.WriteString(w, strconv.FormatInt(int64(i), 10))
}

// FloatLit represents a floating point literal.
type FloatLit float64

func (f FloatLit) Emit(w io.Writer) {
	io.WriteString(w, strconv.FormatFloat(float64(f), 'f', -1, 64))
}

// List represents a Clojure list form: (elem1 elem2 ...)
type List struct {
	Elems []Node
}

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

// Vector represents a Clojure vector: [elem1 elem2 ...]
type Vector struct {
	Elems []Node
}

func (v *Vector) Emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range v.Elems {
		if i > 0 {
			io.WriteString(w, " ")
		}
		if e != nil {
			e.Emit(w)
		}
	}
	io.WriteString(w, "]")
}

// Set represents a Clojure set: #{elem1 elem2 ...}
type Set struct {
	Elems []Node
}

func (s *Set) Emit(w io.Writer) {
	io.WriteString(w, "#{")
	for i, e := range s.Elems {
		if i > 0 {
			io.WriteString(w, " ")
		}
		if e != nil {
			e.Emit(w)
		}
	}
	io.WriteString(w, "}")
}

// Defn represents a function definition.
type Defn struct {
	Name   string
	Params []Node
	Body   []Node
}

func (d *Defn) Emit(w io.Writer) {
	iw, ok := w.(*indentWriter)
	if !ok {
		iw = &indentWriter{w: w}
	}
	io.WriteString(iw, "(defn ")
	io.WriteString(iw, d.Name)
	io.WriteString(iw, " ")
	(&Vector{Elems: d.Params}).Emit(iw)
	io.WriteString(iw, "\n")
	iw.indent += 2
	for i, n := range d.Body {
		iw.writeIndent()
		if n != nil {
			n.Emit(iw)
		} else {
			io.WriteString(iw, "nil")
		}
		if i < len(d.Body)-1 {
			io.WriteString(iw, "\n")
		}
	}
	iw.indent -= 2
	io.WriteString(iw, ")")
}

type indentWriter struct {
	w      io.Writer
	indent int
}

func (iw *indentWriter) Write(p []byte) (int, error) { return iw.w.Write(p) }

func (iw *indentWriter) writeIndent() {
	io.WriteString(iw.w, strings.Repeat(" ", iw.indent))
}

// Program is a sequence of top-level forms.
type Program struct {
	Forms []Node
}

func (p *Program) Emit(w io.Writer) {
	iw := &indentWriter{w: w}
	for i, f := range p.Forms {
		if f == nil {
			continue
		}
		f.Emit(iw)
		if i < len(p.Forms)-1 {
			io.WriteString(iw, "\n\n")
		}
	}
}

// EmitString returns the program source as a byte slice.
func EmitString(p *Program) []byte {
	var buf bytes.Buffer
	if p != nil {
		p.Emit(&buf)
	}
	return buf.Bytes()
}

// Format returns the Clojure source with a generated header and trailing newline.
func Format(src []byte) []byte {
	src = bytes.TrimRight(src, "\n")
	if len(src) > 0 {
		src = append(src, '\n')
	}
	return append(meta.Header(";;"), src...)
}

// --- Transpiler ---

var transpileEnv *types.Env
var currentSeqVar string

// Transpile converts a Mochi program into a Clojure AST. The implementation
// is intentionally minimal and currently only supports very small programs used
// by a subset of tests.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	if prog == nil {
		return nil, fmt.Errorf("nil program")
	}

	transpileEnv = env
	defer func() { transpileEnv = nil }()

	pr := &Program{}

	// emit (ns main)
	pr.Forms = append(pr.Forms, &List{Elems: []Node{Symbol("ns"), Symbol("main")}})

	body := []Node{}
	for _, st := range prog.Statements {
		if st.Let != nil || st.Var != nil {
			n, err := transpileStmt(st)
			if err != nil {
				return nil, err
			}
			pr.Forms = append(pr.Forms, n)
			continue
		}
		n, err := transpileStmt(st)
		if err != nil {
			return nil, err
		}
		if n != nil {
			body = append(body, n)
		}
	}

	pr.Forms = append(pr.Forms, &Defn{Name: "-main", Params: nil, Body: body})

	// invoke main
	pr.Forms = append(pr.Forms, &List{Elems: []Node{Symbol("-main")}})
	return pr, nil
}

func transpileStmt(s *parser.Statement) (Node, error) {
	switch {
	case s.Expr != nil:
		return transpileExpr(s.Expr.Expr)
	case s.If != nil:
		return transpileIfStmt(s.If)
	case s.Let != nil:
		var v Node
		var err error
		if s.Let.Value != nil {
			v, err = transpileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
		} else {
			if s.Let.Type != nil {
				v = defaultValue(s.Let.Type)
			} else {
				v = Symbol("nil")
			}
		}
		return &List{Elems: []Node{Symbol("def"), Symbol(s.Let.Name), v}}, nil
	case s.Var != nil:
		var v Node
		var err error
		if s.Var.Value != nil {
			v, err = transpileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		} else {
			if s.Var.Type != nil {
				v = defaultValue(s.Var.Type)
			} else {
				v = Symbol("nil")
			}
		}
		return &List{Elems: []Node{Symbol("def"), Symbol(s.Var.Name), v}}, nil
	case s.Assign != nil:
		v, err := transpileExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(s.Assign.Index) == 1 && s.Assign.Index[0].Colon == nil && s.Assign.Index[0].Colon2 == nil && s.Assign.Index[0].End == nil && s.Assign.Index[0].Step == nil {
			idx, err := transpileExpr(s.Assign.Index[0].Start)
			if err != nil {
				return nil, err
			}
			assign := &List{Elems: []Node{Symbol("assoc"), Symbol(s.Assign.Name), idx, v}}
			return &List{Elems: []Node{Symbol("def"), Symbol(s.Assign.Name), assign}}, nil
		}
		return &List{Elems: []Node{Symbol("def"), Symbol(s.Assign.Name), v}}, nil
	case s.Fun != nil:
		return transpileFunStmt(s.Fun)
	case s.Return != nil:
		return transpileReturnStmt(s.Return)
	case s.While != nil:
		return transpileWhileStmt(s.While)
	case s.For != nil:
		return transpileForStmt(s.For)
	case s.Break != nil:
		if currentSeqVar != "" {
			return &List{Elems: []Node{Symbol("recur"), Symbol("nil")}}, nil
		}
		return nil, fmt.Errorf("break outside loop")
	case s.Continue != nil:
		if currentSeqVar != "" {
			restSeq := &List{Elems: []Node{Symbol("rest"), Symbol(currentSeqVar)}}
			return &List{Elems: []Node{Symbol("recur"), restSeq}}, nil
		}
		return nil, fmt.Errorf("continue outside loop")
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func transpileFunStmt(f *parser.FunStmt) (Node, error) {
	params := []Node{}
	for _, p := range f.Params {
		params = append(params, Symbol(p.Name))
	}
	body := []Node{}
	for _, st := range f.Body {
		n, err := transpileStmt(st)
		if err != nil {
			return nil, err
		}
		if n != nil {
			body = append(body, n)
		}
	}
	return &Defn{Name: f.Name, Params: params, Body: body}, nil
}

func transpileReturnStmt(r *parser.ReturnStmt) (Node, error) {
	if r.Value == nil {
		return Symbol("nil"), nil
	}
	return transpileExpr(r.Value)
}

var binOp = map[string]string{
	"+":  "+",
	"-":  "-",
	"*":  "*",
	"/":  "/",
	"%":  "mod",
	"==": "=",
	"!=": "not=",
	"<":  "<",
	"<=": "<=",
	">":  ">",
	">=": ">=",
	"&&": "and",
	"||": "or",
	"in": "in",
}

func isStringNode(n Node) bool {
	switch t := n.(type) {
	case StringLit:
		return true
	case *List:
		if len(t.Elems) > 0 {
			if sym, ok := t.Elems[0].(Symbol); ok && sym == "str" {
				return true
			}
		}
	case Symbol:
		if transpileEnv != nil {
			if typ, err := transpileEnv.GetVar(string(t)); err == nil {
				if _, ok := typ.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func isMapNode(n Node) bool {
	if l, ok := n.(*List); ok && len(l.Elems) > 0 {
		if sym, ok := l.Elems[0].(Symbol); ok && sym == "hash-map" {
			return true
		}
	}
	if s, ok := n.(Symbol); ok && transpileEnv != nil {
		if typ, err := transpileEnv.GetVar(string(s)); err == nil {
			if _, ok := typ.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func isNumberNode(n Node) bool {
	switch t := n.(type) {
	case IntLit:
		return true
	case FloatLit:
		return true
	case Symbol:
		if transpileEnv != nil {
			if typ, err := transpileEnv.GetVar(string(t)); err == nil {
				switch typ.(type) {
				case types.IntType, types.FloatType:
					return true
				}
			}
		}
	}
	return false
}

func isZeroNode(n Node) bool {
	switch v := n.(type) {
	case IntLit:
		return v == 0
	case FloatLit:
		return v == 0.0
	}
	return false
}

func transpileExpr(e *parser.Expr) (Node, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	if e.Binary == nil {
		return nil, fmt.Errorf("unsupported expr")
	}
	left, err := transpileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	n := left
	for _, op := range e.Binary.Right {
		right, err := transpilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		if op.Op == "in" {
			if isStringNode(right) {
				n = &List{Elems: []Node{Symbol("clojure.string/includes?"), right, n}}
			} else if isMapNode(right) {
				n = &List{Elems: []Node{Symbol("contains?"), right, n}}
			} else {
				set := &Set{Elems: []Node{n}}
				some := &List{Elems: []Node{Symbol("some"), set, right}}
				n = &List{Elems: []Node{Symbol("boolean"), some}}
			}
			continue
		}
		if (op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=") && (isStringNode(n) || isStringNode(right)) {
			cmp := &List{Elems: []Node{Symbol("compare"), n, right}}
			switch op.Op {
			case "<":
				n = &List{Elems: []Node{Symbol("neg?"), cmp}}
			case "<=":
				n = &List{Elems: []Node{Symbol("<="), cmp, IntLit(0)}}
			case ">":
				n = &List{Elems: []Node{Symbol("pos?"), cmp}}
			case ">=":
				n = &List{Elems: []Node{Symbol(">="), cmp, IntLit(0)}}
			}
			continue
		}
		sym, ok := binOp[op.Op]
		if !ok {
			return nil, fmt.Errorf("binary op not supported")
		}
		if sym == "+" && (isStringNode(n) || isStringNode(right)) {
			n = &List{Elems: []Node{Symbol("str"), n, right}}
		} else {
			n = &List{Elems: []Node{Symbol(sym), n, right}}
		}
	}
	return n, nil
}

func transpileUnary(u *parser.Unary) (Node, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	n, err := transpilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			n = &List{Elems: []Node{Symbol("-"), n}}
		case "!":
			n = &List{Elems: []Node{Symbol("not"), n}}
		default:
			return nil, fmt.Errorf("unary op not supported")
		}
	}
	return n, nil
}

func transpilePostfix(p *parser.PostfixExpr) (Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}

	ops := []*parser.PostfixOp{}
	target := p.Target
	if target.Selector != nil && len(target.Selector.Tail) > 0 {
		for _, name := range target.Selector.Tail {
			ops = append(ops, &parser.PostfixOp{Field: &parser.FieldOp{Name: name}})
		}
		target = &parser.Primary{Selector: &parser.SelectorExpr{Root: target.Selector.Root}}
	}
	ops = append(ops, p.Ops...)

	n, err := transpilePrimary(target)
	if err != nil {
		return nil, err
	}

	for i := 0; i < len(ops); i++ {
		op := ops[i]
		switch {
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
				var start Node = IntLit(0)
				var end Node
				var err error
				if idx.Start != nil {
					start, err = transpileExpr(idx.Start)
					if err != nil {
						return nil, err
					}
				}
				if idx.End != nil {
					end, err = transpileExpr(idx.End)
					if err != nil {
						return nil, err
					}
				} else {
					end = &List{Elems: []Node{Symbol("count"), n}}
				}
				if isStringNode(n) {
					n = &List{Elems: []Node{Symbol("subs"), n, start, end}}
				} else {
					n = &List{Elems: []Node{Symbol("subvec"), n, start, end}}
				}
				continue
			}
			i, err := transpileExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			if isMapNode(n) {
				n = &List{Elems: []Node{Symbol("get"), n, i}}
			} else {
				n = &List{Elems: []Node{Symbol("nth"), n, i}}
			}
		case op.Field != nil:
			if i+1 < len(ops) && ops[i+1].Call != nil && op.Field.Name == "contains" {
				call := ops[i+1].Call
				if len(call.Args) != 1 {
					return nil, fmt.Errorf("contains expects 1 arg")
				}
				arg, err := transpileExpr(call.Args[0])
				if err != nil {
					return nil, err
				}
				n = &List{Elems: []Node{Symbol("clojure.string/includes?"), n, arg}}
				i++
				continue
			}
			return nil, fmt.Errorf("field access not supported")
		case op.Call != nil:
			args := []Node{}
			for _, a := range op.Call.Args {
				ae, err := transpileExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ae)
			}
			n = &List{Elems: append([]Node{n}, args...)}
		case op.Cast != nil:
			var err error
			n, err = castNode(n, op.Cast.Type)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("postfix ops not supported")
		}
	}
	return n, nil
}

func transpileBlock(stmts []*parser.Statement) (Node, error) {
	if len(stmts) == 0 {
		return Symbol("nil"), nil
	}
	if len(stmts) == 1 {
		return transpileStmt(stmts[0])
	}
	elems := []Node{Symbol("do")}
	for _, st := range stmts {
		n, err := transpileStmt(st)
		if err != nil {
			return nil, err
		}
		if n != nil {
			elems = append(elems, n)
		}
	}
	return &List{Elems: elems}, nil
}

func hasLoopCtrl(stmts []*parser.Statement) bool {
	for _, st := range stmts {
		switch {
		case st.Break != nil, st.Continue != nil:
			return true
		case st.If != nil:
			if hasLoopCtrl(st.If.Then) || (st.If.ElseIf != nil && hasLoopCtrl(st.If.ElseIf.Then)) || (len(st.If.Else) > 0 && hasLoopCtrl(st.If.Else)) {
				return true
			}
		}
	}
	return false
}
func transpileIfStmt(s *parser.IfStmt) (Node, error) {
	cond, err := transpileExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	thenNode, err := transpileBlock(s.Then)
	if err != nil {
		return nil, err
	}
	var elseNode Node
	if s.ElseIf != nil {
		elseNode, err = transpileIfStmt(s.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if len(s.Else) > 0 {
		elseNode, err = transpileBlock(s.Else)
		if err != nil {
			return nil, err
		}
	}
	if elseNode == nil {
		return &List{Elems: []Node{Symbol("when"), cond, thenNode}}, nil
	}
	return &List{Elems: []Node{Symbol("if"), cond, thenNode, elseNode}}, nil
}

func transpileIfExpr(e *parser.IfExpr) (Node, error) {
	cond, err := transpileExpr(e.Cond)
	if err != nil {
		return nil, err
	}
	thenNode, err := transpileExpr(e.Then)
	if err != nil {
		return nil, err
	}
	var elseNode Node
	if e.ElseIf != nil {
		elseNode, err = transpileIfExpr(e.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if e.Else != nil {
		elseNode, err = transpileExpr(e.Else)
		if err != nil {
			return nil, err
		}
	}
	if elseNode == nil {
		return &List{Elems: []Node{Symbol("when"), cond, thenNode}}, nil
	}
	return &List{Elems: []Node{Symbol("if"), cond, thenNode, elseNode}}, nil
}

func transpilePrimary(p *parser.Primary) (Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil primary")
	}
	switch {
	case p.Call != nil:
		return transpileCall(p.Call)
	case p.Lit != nil:
		return transpileLiteral(p.Lit)
	case p.If != nil:
		return transpileIfExpr(p.If)
	case p.List != nil:
		elems := []Node{}
		for _, e := range p.List.Elems {
			n, err := transpileExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, n)
		}
		return &Vector{Elems: elems}, nil
	case p.Map != nil:
		elems := []Node{Symbol("hash-map")}
		for _, it := range p.Map.Items {
			k, err := transpileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := transpileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			elems = append(elems, k, v)
		}
		return &List{Elems: elems}, nil
	case p.FunExpr != nil:
		params := []Node{}
		for _, pm := range p.FunExpr.Params {
			params = append(params, Symbol(pm.Name))
		}
		var body Node
		var err error
		if p.FunExpr.ExprBody != nil {
			body, err = transpileExpr(p.FunExpr.ExprBody)
		} else {
			body, err = transpileBlock(p.FunExpr.BlockBody)
		}
		if err != nil {
			return nil, err
		}
		return &List{Elems: []Node{Symbol("fn"), &Vector{Elems: params}, body}}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return Symbol(p.Selector.Root), nil
	case p.Group != nil:
		return transpileExpr(p.Group)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func transpileCall(c *parser.CallExpr) (Node, error) {
	elems := []Node{}
	switch c.Func {
	case "print":
		elems = append(elems, Symbol("println"))
	case "len":
		elems = append(elems, Symbol("count"))
	case "count":
		elems = append(elems, Symbol("count"))
	case "min":
		elems = append(elems, Symbol("apply"), Symbol("min"))
	case "max":
		elems = append(elems, Symbol("apply"), Symbol("max"))
	case "substring":
		elems = append(elems, Symbol("subs"))
	case "append":
		elems = append(elems, Symbol("conj"))
	case "sum":
		elems = append(elems, Symbol("reduce"), Symbol("+"), IntLit(0))
	case "avg":
		if len(c.Args) != 1 {
			return nil, fmt.Errorf("avg expects 1 arg")
		}
		coll, err := transpileExpr(c.Args[0])
		if err != nil {
			return nil, err
		}
		sum := &List{Elems: []Node{Symbol("reduce"), Symbol("+"), IntLit(0), coll}}
		cnt := &List{Elems: []Node{Symbol("count"), coll}}
		avg := &List{Elems: []Node{Symbol("double"), &List{Elems: []Node{Symbol("/"), sum, cnt}}}}
		return avg, nil
	case "values":
		elems = append(elems, Symbol("vals"))
	default:
		elems = append(elems, Symbol(c.Func))
	}
	for _, arg := range c.Args {
		a, err := transpileExpr(arg)
		if err != nil {
			return nil, err
		}
		elems = append(elems, a)
	}
	return &List{Elems: elems}, nil
}

func transpileLiteral(l *parser.Literal) (Node, error) {
	switch {
	case l.Str != nil:
		return StringLit(*l.Str), nil
	case l.Int != nil:
		return IntLit(*l.Int), nil
	case l.Float != nil:
		return FloatLit(*l.Float), nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return Symbol("true"), nil
		}
		return Symbol("false"), nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func defaultValue(t *parser.TypeRef) Node {
	if t == nil || t.Simple == nil {
		return Symbol("nil")
	}
	switch *t.Simple {
	case "int":
		return IntLit(0)
	case "float":
		return FloatLit(0.0)
	case "bool":
		return Symbol("false")
	case "string":
		return StringLit("")
	case "list":
		return &Vector{}
	case "map":
		return &List{Elems: []Node{Symbol("hash-map")}}
	default:
		return Symbol("nil")
	}
}

func castNode(n Node, t *parser.TypeRef) (Node, error) {
	if t == nil || t.Simple == nil {
		return n, nil
	}
	switch *t.Simple {
	case "int":
		if isStringNode(n) {
			return &List{Elems: []Node{Symbol("Integer/parseInt"), n}}, nil
		}
		return &List{Elems: []Node{Symbol("int"), n}}, nil
	case "float":
		if isStringNode(n) {
			return &List{Elems: []Node{Symbol("Double/parseDouble"), n}}, nil
		}
		return &List{Elems: []Node{Symbol("double"), n}}, nil
	case "string":
		return &List{Elems: []Node{Symbol("str"), n}}, nil
	default:
		return nil, fmt.Errorf("cast to %s not supported", *t.Simple)
	}
}

func blockForms(stmts []*parser.Statement) ([]Node, error) {
	forms := []Node{}
	for _, st := range stmts {
		n, err := transpileStmt(st)
		if err != nil {
			return nil, err
		}
		if n != nil {
			forms = append(forms, n)
		}
	}
	return forms, nil
}

func transpileWhileStmt(w *parser.WhileStmt) (Node, error) {
	cond, err := transpileExpr(w.Cond)
	if err != nil {
		return nil, err
	}
	body, err := blockForms(w.Body)
	if err != nil {
		return nil, err
	}
	var bodyNode Node
	if len(body) == 1 {
		bodyNode = body[0]
	} else {
		bodyNode = &List{Elems: append([]Node{Symbol("do")}, body...)}
	}
	return &List{Elems: []Node{Symbol("while"), cond, bodyNode}}, nil
}

func transpileForStmt(f *parser.ForStmt) (Node, error) {
	useCtrl := hasLoopCtrl(f.Body)
	var prevSeq string
	if useCtrl {
		prevSeq = currentSeqVar
		currentSeqVar = f.Name + "_seq"
	}
	bodyNodes := []Node{}
	for _, st := range f.Body {
		n, err := transpileStmt(st)
		if err != nil {
			if useCtrl {
				currentSeqVar = prevSeq
			}
			return nil, err
		}
		if n != nil {
			bodyNodes = append(bodyNodes, n)
		}
	}
	if useCtrl {
		currentSeqVar = prevSeq
	}
	var bodyNode Node
	if len(bodyNodes) == 1 {
		bodyNode = bodyNodes[0]
	} else {
		bodyNode = &List{Elems: append([]Node{Symbol("do")}, bodyNodes...)}
	}
	var seq Node
	useDotimes := false
	if f.RangeEnd != nil {
		start, err := transpileExpr(f.Source)
		if err != nil {
			return nil, err
		}
		end, err := transpileExpr(f.RangeEnd)
		if err != nil {
			return nil, err
		}
		if isZeroNode(start) {
			useDotimes = true
			seq = end
		} else {
			seq = &List{Elems: []Node{Symbol("range"), start, end}}
		}
	} else {
		iter, err := transpileExpr(f.Source)
		if err != nil {
			return nil, err
		}
		seq = iter
	}
	if !useCtrl {
		binding := &Vector{Elems: []Node{Symbol(f.Name), seq}}
		if useDotimes {
			return &List{Elems: []Node{Symbol("dotimes"), binding, bodyNode}}, nil
		}
		return &List{Elems: []Node{Symbol("doseq"), binding, bodyNode}}, nil
	}

	seqSym := f.Name + "_seq"
	binding := &Vector{Elems: []Node{Symbol(seqSym), seq}}
	iterBinding := &Vector{Elems: []Node{Symbol(f.Name), &List{Elems: []Node{Symbol("first"), Symbol(seqSym)}}}}
	nextSeq := &List{Elems: []Node{Symbol("rest"), Symbol(seqSym)}}
	recurExpr := &List{Elems: []Node{Symbol("recur"), nextSeq}}
	letForm := &List{Elems: []Node{Symbol("let"), iterBinding, bodyNode, recurExpr}}
	loopBody := &List{Elems: []Node{Symbol("when"), &List{Elems: []Node{Symbol("seq"), Symbol(seqSym)}}, letForm}}
	return &List{Elems: []Node{Symbol("loop"), binding, loopBody}}, nil
}
