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

// Keyword represents a Clojure keyword.
type Keyword string

func (k Keyword) Emit(w io.Writer) {
	io.WriteString(w, ":"+string(k))
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

// Map represents a Clojure map: {:k v ...}
type Map struct {
	Pairs [][2]Node
}

func (m *Map) Emit(w io.Writer) {
	io.WriteString(w, "{")
	for i, kv := range m.Pairs {
		if i > 0 {
			io.WriteString(w, " ")
		}
		if kv[0] != nil {
			kv[0].Emit(w)
			io.WriteString(w, " ")
		}
		if kv[1] != nil {
			kv[1].Emit(w)
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

func inferStructLiteral(e *parser.Expr, env *types.Env) (types.StructType, bool) {
	if env == nil || e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return types.StructType{}, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return types.StructType{}, false
	}
	p := u.Value.Target
	if ll := p.List; ll != nil {
		if st, ok := types.InferStructFromList(ll, env); ok {
			return st, true
		}
	}
	if ml := p.Map; ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, env); ok {
			return st, true
		}
	}
	return types.StructType{}, false
}

func addRecordDef(p *Program, name string, fields []string) {
	elems := []Node{Symbol("defrecord"), Symbol(name), &Vector{}}
	vec := elems[2].(*Vector)
	for _, f := range fields {
		vec.Elems = append(vec.Elems, Symbol(f))
	}
	form := &List{Elems: elems}
	// insert before other forms after ns/require
	if len(p.Forms) > 2 {
		p.Forms = append(p.Forms[:2], append([]Node{form}, p.Forms[2:]...)...)
	} else {
		p.Forms = append(p.Forms, form)
	}
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
var groupVars map[string]bool
var structCount int
var currentProgram *Program
var funDepth int
var funParamsStack [][]string
var nestedFunArgs map[string][]string

// Transpile converts a Mochi program into a Clojure AST. The implementation
// is intentionally minimal and currently only supports very small programs used
// by a subset of tests.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	if prog == nil {
		return nil, fmt.Errorf("nil program")
	}

	transpileEnv = env
	groupVars = make(map[string]bool)
	structCount = 0
	funParamsStack = nil
	nestedFunArgs = make(map[string][]string)
	pr := &Program{}
	currentProgram = pr
	defer func() {
		transpileEnv = nil
		groupVars = nil
		currentProgram = nil
		nestedFunArgs = nil
		funParamsStack = nil
	}()

	// emit (ns main)
	pr.Forms = append(pr.Forms, &List{Elems: []Node{Symbol("ns"), Symbol("main")}})
	pr.Forms = append(pr.Forms, &List{Elems: []Node{Symbol("require"), Symbol("'clojure.set")}})

	body := []Node{}
	for _, st := range prog.Statements {
		if st.Type != nil || st.ExternType != nil {
			// type declarations have no runtime representation
			continue
		}
		if st.Let != nil || st.Var != nil || st.Fun != nil {
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
			if st, ok := inferStructLiteral(s.Let.Value, transpileEnv); ok {
				structCount++
				name := fmt.Sprintf("Anon%d", structCount)
				if currentProgram != nil {
					addRecordDef(currentProgram, name, st.Order)
				}
				st.Name = name
				if transpileEnv != nil {
					transpileEnv.SetStruct(name, st)
				}
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
			if st, ok := inferStructLiteral(s.Var.Value, transpileEnv); ok {
				structCount++
				name := fmt.Sprintf("Anon%d", structCount)
				if currentProgram != nil {
					addRecordDef(currentProgram, name, st.Order)
				}
				st.Name = name
				if transpileEnv != nil {
					transpileEnv.SetStruct(name, st)
				}
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
		if len(s.Assign.Index) > 0 {
			simple := true
			idxs := []Node{}
			for _, idx := range s.Assign.Index {
				if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
					simple = false
					break
				}
				en, err := transpileExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				idxs = append(idxs, en)
			}
			if simple {
				var assign Node
				if len(idxs) == 1 {
					assign = &List{Elems: []Node{Symbol("assoc"), Symbol(s.Assign.Name), idxs[0], v}}
				} else {
					assign = &List{Elems: []Node{Symbol("assoc-in"), Symbol(s.Assign.Name), &Vector{Elems: idxs}, v}}
				}
				return &List{Elems: []Node{Symbol("def"), Symbol(s.Assign.Name), assign}}, nil
			}
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
	funDepth++
	defer func() { funDepth--; funParamsStack = funParamsStack[:len(funParamsStack)-1] }()

	params := []Node{}
	names := []string{}
	for _, p := range f.Params {
		params = append(params, Symbol(p.Name))
		names = append(names, p.Name)
	}
	funParamsStack = append(funParamsStack, names)
	body := []Node{}
	for i := 0; i < len(f.Body); i++ {
		st := f.Body[i]
		// Pattern: if ... return X; return Y
		if i+1 == len(f.Body)-1 && st.If != nil && f.Body[i+1].Return != nil {
			if len(st.If.Then) == 1 && st.If.Then[0].Return != nil && st.If.Else == nil && st.If.ElseIf == nil {
				cond, err := transpileExpr(st.If.Cond)
				if err != nil {
					return nil, err
				}
				thenExpr, err := transpileExpr(st.If.Then[0].Return.Value)
				if err != nil {
					return nil, err
				}
				elseExpr, err := transpileExpr(f.Body[i+1].Return.Value)
				if err != nil {
					return nil, err
				}
				body = append(body, &List{Elems: []Node{Symbol("if"), cond, thenExpr, elseExpr}})
				break
			}
		}
		n, err := transpileStmt(st)
		if err != nil {
			return nil, err
		}
		if n != nil {
			body = append(body, n)
		}
	}
	defn := &Defn{Name: f.Name, Params: params, Body: body}
	if funDepth > 1 && currentProgram != nil {
		captured := []string{}
		for i := 0; i < len(funParamsStack)-1; i++ {
			captured = append(captured, funParamsStack[i]...)
		}
		newParams := []Node{}
		for _, n := range captured {
			newParams = append(newParams, Symbol(n))
		}
		defn.Params = append(newParams, defn.Params...)
		nestedFunArgs[f.Name] = captured
		currentProgram.Forms = append(currentProgram.Forms, defn)
		return nil, nil
	}
	return defn, nil
}

func transpileReturnStmt(r *parser.ReturnStmt) (Node, error) {
	if r.Value == nil {
		return Symbol("nil"), nil
	}
	return transpileExpr(r.Value)
}

var binOp = map[string]string{
	"+":         "+",
	"-":         "-",
	"*":         "*",
	"/":         "/",
	"%":         "mod",
	"==":        "=",
	"!=":        "not=",
	"<":         "<",
	"<=":        "<=",
	">":         ">",
	">=":        ">=",
	"&&":        "and",
	"||":        "or",
	"in":        "in",
	"union":     "union",
	"union_all": "union_all",
	"except":    "except",
	"intersect": "intersect",
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
	switch t := n.(type) {
	case *Map:
		return true
	case *List:
		if len(t.Elems) > 0 {
			if sym, ok := t.Elems[0].(Symbol); ok && sym == "hash-map" {
				return true
			}
		}
	case Symbol:
		if transpileEnv != nil {
			if typ, err := transpileEnv.GetVar(string(t)); err == nil {
				if _, ok := typ.(types.MapType); ok {
					return true
				}
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

func isAggCall(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return ""
	}
	name := u.Value.Target.Call.Func
	switch name {
	case "sum", "avg", "count":
		if len(u.Value.Target.Call.Args) == 1 {
			return name
		}
	}
	return ""
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
		switch sym {
		case "+":
			if isStringNode(n) || isStringNode(right) {
				n = &List{Elems: []Node{Symbol("str"), n, right}}
			} else {
				n = &List{Elems: []Node{Symbol("+"), n, right}}
			}
		case "union":
			setFn := func(x Node) Node { return &List{Elems: []Node{Symbol("set"), x}} }
			u := &List{Elems: []Node{Symbol("clojure.set/union"), setFn(n), setFn(right)}}
			n = &List{Elems: []Node{Symbol("vec"), u}}
		case "union_all":
			n = &List{Elems: []Node{Symbol("vec"), &List{Elems: []Node{Symbol("concat"), n, right}}}}
		case "except":
			diff := &List{Elems: []Node{Symbol("clojure.set/difference"), &List{Elems: []Node{Symbol("set"), n}}, &List{Elems: []Node{Symbol("set"), right}}}}
			n = &List{Elems: []Node{Symbol("vec"), diff}}
		case "intersect":
			inter := &List{Elems: []Node{Symbol("clojure.set/intersection"), &List{Elems: []Node{Symbol("set"), n}}, &List{Elems: []Node{Symbol("set"), right}}}}
			n = &List{Elems: []Node{Symbol("vec"), inter}}
		default:
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
			if isMapNode(n) || isStringNode(i) {
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
			// access map fields by keyword as function
			key := Keyword(op.Field.Name)
			n = &List{Elems: []Node{key, n}}
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
		pairs := [][2]Node{}
		for _, it := range p.Map.Items {
			k, err := transpileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			if sym, ok := k.(Symbol); ok {
				k = Keyword(sym)
			}
			v, err := transpileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			pairs = append(pairs, [2]Node{k, v})
		}
		return &Map{Pairs: pairs}, nil
	case p.Match != nil:
		return transpileMatchExpr(p.Match)
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
	case p.Query != nil:
		return transpileQueryExpr(p.Query)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return Symbol(p.Selector.Root), nil
	case p.Selector != nil && len(p.Selector.Tail) > 0:
		pf := &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Selector.Root}}}
		for _, t := range p.Selector.Tail {
			pf.Ops = append(pf.Ops, &parser.PostfixOp{Field: &parser.FieldOp{Name: t}})
		}
		return transpilePostfix(pf)
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
	case "json":
		if len(c.Args) != 1 {
			return nil, fmt.Errorf("json expects 1 arg")
		}
		arg, err := transpileExpr(c.Args[0])
		if err != nil {
			return nil, err
		}
		pairFn := &List{Elems: []Node{
			Symbol("fn"),
			&Vector{Elems: []Node{&Vector{Elems: []Node{Symbol("k"), Symbol("v")}}}},
			&List{Elems: []Node{Symbol("str"), StringLit("\""), &List{Elems: []Node{Symbol("name"), Symbol("k")}}, StringLit("\":"), Symbol("v")}},
		}}
		joinPairs := &List{Elems: []Node{
			Symbol("clojure.string/join"),
			StringLit(","),
			&List{Elems: []Node{Symbol("map"), pairFn, Symbol("m")}},
		}}
		body := &List{Elems: []Node{Symbol("str"), StringLit("{"), joinPairs, StringLit("}")}}
		lambda := &List{Elems: []Node{Symbol("fn"), &Vector{Elems: []Node{Symbol("m")}}, body}}
		call := &List{Elems: []Node{lambda, arg}}
		return &List{Elems: []Node{Symbol("println"), call}}, nil
	case "exists":
		if len(c.Args) != 1 {
			return nil, fmt.Errorf("exists expects 1 arg")
		}
		arg, err := transpileExpr(c.Args[0])
		if err != nil {
			return nil, err
		}
		cnt := &List{Elems: []Node{Symbol("count"), arg}}
		return &List{Elems: []Node{Symbol(">"), cnt, IntLit(0)}}, nil
	case "values":
		elems = append(elems, Symbol("vals"))
	default:
		elems = append(elems, Symbol(c.Func))
		if extra, ok := nestedFunArgs[c.Func]; ok {
			for _, v := range extra {
				elems = append(elems, Symbol(v))
			}
		}
	}
	for _, arg := range c.Args {
		a, err := transpileExpr(arg)
		if err != nil {
			return nil, err
		}
		elems = append(elems, a)
	}
	if sym, ok := elems[0].(Symbol); ok && transpileEnv != nil {
		if typ, err := transpileEnv.GetVar(string(sym)); err == nil {
			callArity := len(elems) - 1
			if ft, ok := typ.(types.FuncType); ok && !ft.Variadic && len(ft.Params) > callArity {
				elems = append([]Node{Symbol("partial"), sym}, elems[1:]...)
				return &List{Elems: elems}, nil
			}
		}
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

func transpileMatchExpr(m *parser.MatchExpr) (Node, error) {
	target, err := transpileExpr(m.Target)
	if err != nil {
		return nil, err
	}
	elems := []Node{Symbol("cond")}
	for _, c := range m.Cases {
		pat, err := transpileExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if sym, ok := pat.(Symbol); ok && string(sym) == "_" {
			elems = append(elems, Symbol("true"))
			res, err := transpileExpr(c.Result)
			if err != nil {
				return nil, err
			}
			elems = append(elems, res)
			continue
		}
		eq := &List{Elems: []Node{Symbol("="), target, pat}}
		res, err := transpileExpr(c.Result)
		if err != nil {
			return nil, err
		}
		elems = append(elems, eq, res)
	}
	return &List{Elems: elems}, nil
}

func transpileQueryExpr(q *parser.QueryExpr) (Node, error) {
	if q == nil {
		return nil, fmt.Errorf("nil query")
	}
	if q.Distinct {
		return nil, fmt.Errorf("unsupported query features")
	}

	// handle a single right join without additional clauses by swapping
	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil {
		j := q.Joins[0]
		if j.Side != nil && *j.Side == "right" && j.On != nil {
			leftSrc, err := transpileExpr(q.Source)
			if err != nil {
				return nil, err
			}
			if name, ok := identName(q.Source); ok && groupVars != nil && groupVars[name] {
				leftSrc = &List{Elems: []Node{Keyword("items"), Symbol(name)}}
			}
			rightSrc, err := transpileExpr(j.Src)
			if err != nil {
				return nil, err
			}
			if name, ok := identName(j.Src); ok && groupVars != nil && groupVars[name] {
				rightSrc = &List{Elems: []Node{Keyword("items"), Symbol(name)}}
			}
			onExpr, err := transpileExpr(j.On)
			if err != nil {
				return nil, err
			}
			tmp := q.Var + "_tmp"
			filterFn := &List{Elems: []Node{Symbol("fn"), &Vector{Elems: []Node{Symbol(q.Var)}}, onExpr}}
			filtered := &List{Elems: []Node{Symbol("filter"), filterFn, leftSrc}}
			joinSeq := &List{Elems: []Node{Symbol("let"), &Vector{Elems: []Node{Symbol(tmp), filtered}},
				&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("seq"), Symbol(tmp)}}, Symbol(tmp), &Vector{Elems: []Node{Symbol("nil")}}}}}}

			bindings := []Node{Symbol(j.Var), rightSrc, Symbol(q.Var), joinSeq}
			conds := []Node{}
			if q.Where != nil {
				ce, err := transpileExpr(q.Where)
				if err != nil {
					return nil, err
				}
				conds = append(conds, ce)
			}
			sel, err := transpileExpr(q.Select)
			if err != nil {
				return nil, err
			}
			vecElems := bindings
			if len(conds) == 1 {
				vecElems = append(vecElems, Keyword("when"), conds[0])
			} else if len(conds) > 1 {
				andForm := []Node{Symbol("and")}
				andForm = append(andForm, conds...)
				vecElems = append(vecElems, Keyword("when"), &List{Elems: andForm})
			}
			return &List{Elems: []Node{Symbol("for"), &Vector{Elems: vecElems}, sel}}, nil
		}
	}

	bindings := []Node{}
	src, err := transpileExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if name, ok := identName(q.Source); ok && groupVars != nil && groupVars[name] {
		src = &List{Elems: []Node{Keyword("items"), Symbol(name)}}
	}
	if q.Sort != nil {
		key, err := transpileExpr(q.Sort)
		if err != nil {
			return nil, err
		}
		fn := &List{Elems: []Node{Symbol("fn"), &Vector{Elems: []Node{Symbol(q.Var)}}, key}}
		src = &List{Elems: []Node{Symbol("sort-by"), fn, src}}
	}
	if q.Skip != nil {
		sk, err := transpileExpr(q.Skip)
		if err != nil {
			return nil, err
		}
		src = &List{Elems: []Node{Symbol("drop"), sk, src}}
	}
	if q.Take != nil {
		tk, err := transpileExpr(q.Take)
		if err != nil {
			return nil, err
		}
		src = &List{Elems: []Node{Symbol("take"), tk, src}}
	}
	bindings = append(bindings, Symbol(q.Var), src)

	conds := []Node{}

	for _, f := range q.Froms {
		fe, err := transpileExpr(f.Src)
		if err != nil {
			return nil, err
		}
		bindings = append(bindings, Symbol(f.Var), fe)
	}

	for _, j := range q.Joins {
		je, err := transpileExpr(j.Src)
		if err != nil {
			return nil, err
		}
		if j.Side != nil && *j.Side == "left" && j.On != nil {
			onExpr, err := transpileExpr(j.On)
			if err != nil {
				return nil, err
			}
			tmp := j.Var + "_tmp"
			filterFn := &List{Elems: []Node{Symbol("fn"), &Vector{Elems: []Node{Symbol(j.Var)}}, onExpr}}
			filtered := &List{Elems: []Node{Symbol("filter"), filterFn, je}}
			joinSeq := &List{Elems: []Node{Symbol("let"), &Vector{Elems: []Node{Symbol(tmp), filtered}},
				&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("seq"), Symbol(tmp)}}, Symbol(tmp), &Vector{Elems: []Node{Symbol("nil")}}}}}}
			bindings = append(bindings, Symbol(j.Var), joinSeq)
		} else if j.Side != nil {
			return nil, fmt.Errorf("unsupported join type")
		} else {
			bindings = append(bindings, Symbol(j.Var), je)
			if j.On != nil {
				onExpr, err := transpileExpr(j.On)
				if err != nil {
					return nil, err
				}
				conds = append(conds, onExpr)
			}
		}
	}

	if q.Where != nil {
		ce, err := transpileExpr(q.Where)
		if err != nil {
			return nil, err
		}
		conds = append(conds, ce)
	}

	if q.Group == nil {
		selCall := isAggCall(q.Select)
		vecElems := bindings
		if len(conds) == 1 {
			vecElems = append(vecElems, Keyword("when"), conds[0])
		} else if len(conds) > 1 {
			andForm := []Node{Symbol("and")}
			andForm = append(andForm, conds...)
			vecElems = append(vecElems, Keyword("when"), &List{Elems: andForm})
		}

		if selCall != "" {
			arg, err := transpileExpr(q.Select.Binary.Left.Value.Target.Call.Args[0])
			if err != nil {
				return nil, err
			}
			comp := &List{Elems: []Node{Symbol("for"), &Vector{Elems: vecElems}, arg}}
			switch selCall {
			case "sum":
				return &List{Elems: []Node{Symbol("reduce"), Symbol("+"), IntLit(0), comp}}, nil
			case "avg":
				sum := &List{Elems: []Node{Symbol("reduce"), Symbol("+"), IntLit(0), comp}}
				cnt := &List{Elems: []Node{Symbol("count"), comp}}
				return &List{Elems: []Node{Symbol("double"), &List{Elems: []Node{Symbol("/"), sum, cnt}}}}, nil
			case "count":
				return &List{Elems: []Node{Symbol("count"), comp}}, nil
			}
		}

		sel, err := transpileExpr(q.Select)
		if err != nil {
			return nil, err
		}
		forForm := &List{Elems: []Node{Symbol("for"), &Vector{Elems: vecElems}, sel}}
		return forForm, nil
	}

	if len(q.Group.Exprs) == 0 {
		return nil, fmt.Errorf("missing group key")
	}
	keyExpr, err := transpileExpr(q.Group.Exprs[0])
	if err != nil {
		return nil, err
	}

	names := []string{q.Var}
	for _, f := range q.Froms {
		names = append(names, f.Var)
	}
	for _, j := range q.Joins {
		names = append(names, j.Var)
	}

	var item Node
	if len(names) == 1 {
		item = Symbol(q.Var)
	} else {
		pairs := make([][2]Node, 0, len(names))
		for _, n := range names {
			pairs = append(pairs, [2]Node{Keyword(n), Symbol(n)})
		}
		item = &Map{Pairs: pairs}
	}

	vecElems := bindings
	if len(conds) == 1 {
		vecElems = append(vecElems, Keyword("when"), conds[0])
	} else if len(conds) > 1 {
		andForm := []Node{Symbol("and")}
		andForm = append(andForm, conds...)
		vecElems = append(vecElems, Keyword("when"), &List{Elems: andForm})
	}
	vecElems = append(vecElems, Keyword("let"), &Vector{Elems: []Node{Symbol("k"), keyExpr}})

	rowMap := &Map{Pairs: [][2]Node{{Keyword("key"), Symbol("k")}, {Keyword("item"), item}}}
	rowsFor := &List{Elems: []Node{Symbol("for"), &Vector{Elems: vecElems}, rowMap}}
	groupMap := &List{Elems: []Node{Symbol("group-by"), Keyword("key"), rowsFor}}

	gPairs := [][2]Node{{Keyword("key"), Symbol("k")}, {Keyword("items"), &List{Elems: []Node{Symbol("map"), Keyword("item"), Symbol("rows")}}}}

	vec2 := []Node{&Vector{Elems: []Node{Symbol("k"), Symbol("rows")}}, groupMap, Keyword("let"), &Vector{Elems: []Node{Symbol(q.Group.Name), &Map{Pairs: gPairs}}}}
	if groupVars != nil {
		groupVars[q.Group.Name] = true
		defer delete(groupVars, q.Group.Name)
	}
	if q.Group.Having != nil {
		hav, err := transpileExpr(q.Group.Having)
		if err != nil {
			return nil, err
		}
		vec2 = append(vec2, Keyword("when"), hav)
	}
	sel, err := transpileExpr(q.Select)
	if err != nil {
		return nil, err
	}
	return &List{Elems: []Node{Symbol("for"), &Vector{Elems: vec2}, sel}}, nil
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value.Target
	if p == nil || p.Selector == nil || len(p.Selector.Tail) > 0 {
		return "", false
	}
	return p.Selector.Root, true
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
		return &Map{}
	default:
		return Symbol("nil")
	}
}

func castNode(n Node, t *parser.TypeRef) (Node, error) {
	if t == nil || t.Simple == nil {
		return n, nil
	}
	if transpileEnv != nil {
		if _, ok := transpileEnv.GetStruct(*t.Simple); ok {
			if m, ok2 := n.(*Map); ok2 {
				for i, kv := range m.Pairs {
					switch k := kv[0].(type) {
					case StringLit:
						m.Pairs[i][0] = Keyword(string(k))
					}
				}
			}
			// structs have the same runtime representation as maps
			return n, nil
		}
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

	condElems := []Node{}
	other := []Node{}
	for _, n := range bodyNodes {
		if c, b, ok := condRecur(n); ok {
			condElems = append(condElems, c, b)
		} else {
			other = append(other, n)
		}
	}

	elseBody := append([]Node{}, other...)
	elseBody = append(elseBody, &List{Elems: []Node{Symbol("recur"), nextSeq}})
	var elseNode Node
	if len(elseBody) == 1 {
		elseNode = elseBody[0]
	} else {
		elseNode = &List{Elems: append([]Node{Symbol("do")}, elseBody...)}
	}
	condElems = append(condElems, Keyword("else"), elseNode)

	condForm := &List{Elems: append([]Node{Symbol("cond")}, condElems...)}
	letForm := &List{Elems: []Node{Symbol("let"), iterBinding, condForm}}
	loopBody := &List{Elems: []Node{Symbol("when"), &List{Elems: []Node{Symbol("seq"), Symbol(seqSym)}}, letForm}}
	return &List{Elems: []Node{Symbol("loop"), binding, loopBody}}, nil
}

func condRecur(n Node) (cond Node, recur Node, ok bool) {
	l, ok := n.(*List)
	if !ok || len(l.Elems) != 3 {
		return nil, nil, false
	}
	sym, ok := l.Elems[0].(Symbol)
	if !ok || sym != "when" {
		return nil, nil, false
	}
	r, ok := l.Elems[2].(*List)
	if !ok || len(r.Elems) != 2 {
		return nil, nil, false
	}
	rsym, ok := r.Elems[0].(Symbol)
	if !ok || rsym != "recur" {
		return nil, nil, false
	}
	return l.Elems[1], r, true
}
