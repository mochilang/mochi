//go:build slow

package erl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

// Program is a minimal Erlang module consisting of sequential statements.
// Program is a minimal Erlang module consisting of sequential statements and
// optional function declarations.
type Program struct {
	Funs  []*FuncDecl
	Stmts []Stmt
}

// context tracks variable aliases to emulate mutable variables.
type context struct {
	alias   map[string]string
	orig    map[string]string
	counter map[string]int
}

func newContext() *context {
	return &context{alias: map[string]string{}, orig: map[string]string{}, counter: map[string]int{}}
}

func (c *context) newAlias(name string) string {
	c.counter[name]++
	alias := sanitize(name)
	if c.counter[name] > 1 {
		alias = fmt.Sprintf("%s%d", alias, c.counter[name])
	}
	c.alias[name] = alias
	c.orig[alias] = name
	return alias
}

func (c *context) current(name string) string {
	if a, ok := c.alias[name]; ok {
		return a
	}
	return c.newAlias(name)
}

func (c *context) original(alias string) string {
	if o, ok := c.orig[alias]; ok {
		return o
	}
	return alias
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// ContainsExpr represents s.contains(sub).
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

// SliceExpr represents s[i:j] for strings and lists.
type SliceExpr struct {
	Target   Expr
	Start    Expr
	End      Expr
	Kind     string
	IsString bool
}

// SubstringExpr represents substring(s, i, j).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

// PrintStmt represents a call to print/1.
type PrintStmt struct{ Expr Expr }

// ReturnStmt represents returning a value from a function.
type ReturnStmt struct{ Expr Expr }

// FuncDecl is a simple function declaration with no parameters.
type FuncDecl struct {
	Name   string
	Body   []Stmt
	Return Expr
}

// LetStmt represents a variable binding.
type LetStmt struct {
	Name string
	Expr Expr
}

// CallExpr represents a function call.
type CallExpr struct {
	Func string
	Args []Expr
}

// BinaryExpr is a binary operation.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// UnaryExpr is a prefix unary operation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

// ListLit represents a list literal.
type ListLit struct{ Elems []Expr }

// IfExpr is a conditional expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

// NameRef refers to a variable.
type NameRef struct {
	Name     string
	IsString bool
}

// MapLit represents a map literal.
type MapLit struct{ Items []MapItem }

// MapItem is a key/value pair within a map literal.
type MapItem struct {
	Key   Expr
	Value Expr
}

// IndexExpr represents indexing into a list, map or string.
type IndexExpr struct {
	Target   Expr
	Index    Expr
	Kind     string // "list", "map" or "string"
	IsString bool
}

type IntLit struct{ Value int64 }

type BoolLit struct{ Value bool }

type StringLit struct{ Value string }

// AtomLit represents a simple atom like 'nil'.
type AtomLit struct{ Name string }

// IfStmt represents a simple if statement with optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (p *PrintStmt) emit(w io.Writer) {
	format := "~p~n"
	if isStringExpr(p.Expr) {
		format = "~s~n"
	}
	fmt.Fprintf(w, "io:format(\"%s\", [", format)
	p.Expr.emit(w)
	io.WriteString(w, "])")
}

func (r *ReturnStmt) emit(w io.Writer) {
	if r.Expr != nil {
		r.Expr.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
}

func (fd *FuncDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "%s() ->\n", fd.Name)
	for _, st := range fd.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		io.WriteString(w, ",\n")
	}
	io.WriteString(w, "    ")
	if fd.Return != nil {
		fd.Return.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
	io.WriteString(w, ".\n\n")
}

func isStringExpr(e Expr) bool {
	switch v := e.(type) {
	case *StringLit:
		return true
	case *CallExpr:
		return v.Func == "str"
	case *BinaryExpr:
		if v.Op == "++" || v.Op == "+" {
			return isStringExpr(v.Left) || isStringExpr(v.Right)
		}
		return false
	case *IfExpr:
		return isStringExpr(v.Then) && isStringExpr(v.Else)
	case *NameRef:
		return v.IsString
	case *IndexExpr:
		return v.IsString
	default:
		return false
	}
}

func isMapExpr(e Expr, env *types.Env, ctx *context) bool {
	switch v := e.(type) {
	case *MapLit:
		return true
	case *NameRef:
		if env != nil {
			name := v.Name
			if ctx != nil {
				name = ctx.original(v.Name)
			}
			if t, err := env.GetVar(name); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	}
	return false
}

func mapValueIsString(e Expr, env *types.Env, ctx *context) bool {
	if nr, ok := e.(*NameRef); ok {
		name := nr.Name
		if ctx != nil {
			name = ctx.original(nr.Name)
		}
		if t, err := env.GetVar(name); err == nil {
			if mt, ok := t.(types.MapType); ok {
				if _, ok := mt.Value.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if ml, ok := e.(*MapLit); ok && len(ml.Items) > 0 {
		return isStringExpr(ml.Items[0].Value)
	}
	return false
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", l.Name)
	l.Expr.emit(w)
}

func (c *CallExpr) emit(w io.Writer) {
	switch c.Func {
	case "append":
		// append(list, elem)
		io.WriteString(w, "lists:append(")
		if len(c.Args) == 2 {
			c.Args[0].emit(w)
			io.WriteString(w, ", [")
			c.Args[1].emit(w)
			io.WriteString(w, "])")
		} else {
			io.WriteString(w, ")")
		}
		return
	case "avg":
		if len(c.Args) == 1 {
			io.WriteString(w, "(lists:sum(")
			c.Args[0].emit(w)
			io.WriteString(w, ") / length(")
			c.Args[0].emit(w)
			io.WriteString(w, "))")
		} else {
			io.WriteString(w, "0")
		}
		return
	case "count":
		io.WriteString(w, "length(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "len":
		io.WriteString(w, "length(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
		return
	case "str":
		io.WriteString(w, "integer_to_list(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
		return
	case "sum":
		io.WriteString(w, "lists:sum(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "min":
		io.WriteString(w, "lists:min(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "max":
		io.WriteString(w, "lists:max(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	case "values":
		io.WriteString(w, "maps:values(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	name := c.Func
	io.WriteString(w, name)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func (b *BinaryExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	op := mapOp(b.Op)
	// use string concatenation operator when needed
	if b.Op == "+" {
		if _, ok := b.Left.(*StringLit); ok {
			op = "++"
		}
		if _, ok := b.Right.(*StringLit); ok {
			op = "++"
		}
	}
	b.Left.emit(w)
	io.WriteString(w, " "+op+" ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" {
		io.WriteString(w, "not ")
	} else {
		io.WriteString(w, u.Op)
	}
	u.Expr.emit(w)
}

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "#{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		it.Key.emit(w)
		io.WriteString(w, " => ")
		it.Value.emit(w)
	}
	io.WriteString(w, "}")
}

func (i *IndexExpr) emit(w io.Writer) {
	switch i.Kind {
	case "map":
		io.WriteString(w, "maps:get(")
		i.Index.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ")")
	case "string":
		io.WriteString(w, "string:substr(")
		i.Target.emit(w)
		io.WriteString(w, ", ")
		i.Index.emit(w)
		io.WriteString(w, " + 1, 1)")
	default: // list
		io.WriteString(w, "lists:nth(")
		i.Index.emit(w)
		io.WriteString(w, " + 1, ")
		i.Target.emit(w)
		io.WriteString(w, ")")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	switch s.Kind {
	case "string":
		io.WriteString(w, "string:substr(")
		s.Target.emit(w)
		io.WriteString(w, ", ")
		if s.Start != nil {
			s.Start.emit(w)
			io.WriteString(w, " + 1")
		} else {
			io.WriteString(w, "1")
		}
		io.WriteString(w, ", ")
		if s.End != nil {
			io.WriteString(w, "(")
			s.End.emit(w)
			io.WriteString(w, " - ")
			if s.Start != nil {
				s.Start.emit(w)
			} else {
				io.WriteString(w, "0")
			}
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "byte_size(")
			s.Target.emit(w)
			io.WriteString(w, ")")
			if s.Start != nil {
				io.WriteString(w, " - ")
				s.Start.emit(w)
			}
		}
		io.WriteString(w, ")")
	default:
		io.WriteString(w, "lists:sublist(")
		s.Target.emit(w)
		io.WriteString(w, ", ")
		if s.Start != nil {
			s.Start.emit(w)
			io.WriteString(w, " + 1")
		} else {
			io.WriteString(w, "1")
		}
		io.WriteString(w, ", ")
		if s.End != nil {
			io.WriteString(w, "(")
			s.End.emit(w)
			io.WriteString(w, " - ")
			if s.Start != nil {
				s.Start.emit(w)
			} else {
				io.WriteString(w, "0")
			}
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "length(")
			s.Target.emit(w)
			io.WriteString(w, ")")
			if s.Start != nil {
				io.WriteString(w, " - ")
				s.Start.emit(w)
			}
		}
		io.WriteString(w, ")")
	}
}

func (c *ContainsExpr) emit(w io.Writer) {
	io.WriteString(w, "(string:str(")
	c.Str.emit(w)
	io.WriteString(w, ", ")
	c.Sub.emit(w)
	io.WriteString(w, ") =/= 0)")
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "string:substr(")
	s.Str.emit(w)
	io.WriteString(w, ", ")
	s.Start.emit(w)
	io.WriteString(w, " + 1, (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, "))")
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(case ")
	i.Cond.emit(w)
	io.WriteString(w, " of\n    true -> ")
	i.Then.emit(w)
	io.WriteString(w, ";\n    _ -> ")
	i.Else.emit(w)
	io.WriteString(w, "\nend)")
}

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (a *AtomLit) emit(w io.Writer) { io.WriteString(w, a.Name) }

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "case ")
	i.Cond.emit(w)
	io.WriteString(w, " of\n        true -> ")
	for idx, st := range i.Then {
		if idx > 0 {
			io.WriteString(w, ",\n            ")
		}
		st.emit(w)
	}
	if len(i.Else) > 0 {
		io.WriteString(w, ";\n        _ -> ")
		for idx, st := range i.Else {
			if idx > 0 {
				io.WriteString(w, ",\n            ")
			}
			st.emit(w)
		}
	}
	io.WriteString(w, "\n    end")
}

func mapOp(op string) string {
	switch op {
	case "&&":
		return "andalso"
	case "||":
		return "orelse"
	case "!=":
		return "/="
	case "<=":
		return "=<"
	case "/":
		return "div"
	case "%":
		return "rem"
	default:
		return op
	}
}

func sanitize(name string) string {
	if name == "" {
		return "V"
	}
	return strings.ToUpper(name[:1]) + name[1:]
}

// Transpile converts a subset of Mochi to an Erlang AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	ctx := newContext()
	p := &Program{}
	for _, st := range prog.Statements {
		if st.Fun != nil {
			fd, err := convertFunStmt(st.Fun, env, ctx)
			if err != nil {
				return nil, err
			}
			p.Funs = append(p.Funs, fd)
			continue
		}
		stmts, err := convertStmt(st, env, ctx)
		if err != nil {
			return nil, err
		}
		p.Stmts = append(p.Stmts, stmts...)
	}
	return p, nil
}

func convertStmt(st *parser.Statement, env *types.Env, ctx *context) ([]Stmt, error) {
	switch {
	case st.Return != nil:
		if st.Return.Value != nil {
			val, err := convertExpr(st.Return.Value, env, ctx)
			if err != nil {
				return nil, err
			}
			return []Stmt{&ReturnStmt{Expr: val}}, nil
		}
		return []Stmt{&ReturnStmt{}}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value, env, ctx)
			if err != nil {
				return nil, err
			}
		} else {
			e = &AtomLit{Name: "nil"}
		}
		alias := ctx.newAlias(st.Let.Name)
		return []Stmt{&LetStmt{Name: alias, Expr: e}}, nil
	case st.Var != nil:
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value, env, ctx)
			if err != nil {
				return nil, err
			}
		} else {
			e = &AtomLit{Name: "nil"}
		}
		alias := ctx.newAlias(st.Var.Name)
		return []Stmt{&LetStmt{Name: alias, Expr: e}}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		val, err := convertExpr(st.Assign.Value, env, ctx)
		if err != nil {
			return nil, err
		}
		alias := ctx.newAlias(st.Assign.Name)
		return []Stmt{&LetStmt{Name: alias, Expr: val}}, nil
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr, env, ctx)
		if err != nil {
			return nil, err
		}
		if c, ok := e.(*CallExpr); ok && c.Func == "print" && len(c.Args) == 1 {
			arg := c.Args[0]
			return []Stmt{&PrintStmt{Expr: arg}}, nil
		}
		return nil, fmt.Errorf("unsupported expression")
	case st.If != nil:
		s, err := convertIfStmt(st.If, env, ctx)
		if err != nil {
			return nil, err
		}
		return []Stmt{s}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIfStmt(n *parser.IfStmt, env *types.Env, ctx *context) (*IfStmt, error) {
	cond, err := convertExpr(n.Cond, env, ctx)
	if err != nil {
		return nil, err
	}
	thenStmts := []Stmt{}
	for _, st := range n.Then {
		cs, err := convertStmt(st, env, ctx)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, cs...)
	}
	var elseStmts []Stmt
	if n.ElseIf != nil {
		es, err := convertIfStmt(n.ElseIf, env, ctx)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{es}
	} else if len(n.Else) > 0 {
		for _, st := range n.Else {
			cs, err := convertStmt(st, env, ctx)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, cs...)
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertFunStmt(fn *parser.FunStmt, env *types.Env, ctx *context) (*FuncDecl, error) {
	child := types.NewEnv(env)
	fctx := newContext()
	var stmts []Stmt
	var ret Expr
	for i, st := range fn.Body {
		if r := st.Return; r != nil && i == len(fn.Body)-1 {
			if r.Value != nil {
				var err error
				ret, err = convertExpr(r.Value, child, fctx)
				if err != nil {
					return nil, err
				}
			}
			continue
		}
		ss, err := convertStmt(st, child, fctx)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, ss...)
	}
	if ret == nil {
		ret = &AtomLit{Name: "nil"}
	}
	return &FuncDecl{Name: fn.Name, Body: stmts, Return: ret}, nil
}

func convertExpr(e *parser.Expr, env *types.Env, ctx *context) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary, env, ctx)
}

func convertBinary(b *parser.BinaryExpr, env *types.Env, ctx *context) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	left, err := convertUnary(b.Left, env, ctx)
	if err != nil {
		return nil, err
	}
	ops := make([]string, len(b.Right))
	exprs := []Expr{left}
	for i, op := range b.Right {
		r, err := convertPostfix(op.Right, env, ctx)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, r)
		ops[i] = op.Op
	}
	// handle membership operator early
	for i := 0; i < len(ops); i++ {
		if ops[i] == "in" {
			l := exprs[i]
			r := exprs[i+1]
			if isStringExpr(r) {
				cmp := &CallExpr{Func: "string:str", Args: []Expr{r, l}}
				exprs[i] = &BinaryExpr{Left: cmp, Op: "!=", Right: &IntLit{Value: 0}}
			} else if isMapExpr(r, env, ctx) {
				exprs[i] = &CallExpr{Func: "maps:is_key", Args: []Expr{l, r}}
			} else {
				exprs[i] = &CallExpr{Func: "lists:member", Args: []Expr{l, r}}
			}
			exprs = append(exprs[:i+1], exprs[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
			i--
		}
	}
	levels := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!="}, {"&&"}, {"||"}}
	contains := func(list []string, v string) bool {
		for _, s := range list {
			if s == v {
				return true
			}
		}
		return false
	}
	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
				l := exprs[i]
				r := exprs[i+1]
				exprs[i] = &BinaryExpr{Left: l, Op: ops[i], Right: r}
				exprs = append(exprs[:i+1], exprs[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	return exprs[0], nil
}

func convertUnary(u *parser.Unary, env *types.Env, ctx *context) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value, env, ctx)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env, ctx *context) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		base, err := convertPrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}, env, ctx)
		if err != nil {
			return nil, err
		}
		if len(pf.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		arg, err := convertExpr(pf.Ops[0].Call.Args[0], env, ctx)
		if err != nil {
			return nil, err
		}
		return &ContainsExpr{Str: base, Sub: arg}, nil
	}
	expr, err := convertPrimary(pf.Target, env, ctx)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Cast != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int":
			expr = &CallExpr{Func: "list_to_integer", Args: []Expr{expr}}
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(op.Index.Start, env, ctx)
			if err != nil {
				return nil, err
			}
			kind := "list"
			isStr := false
			if isMapExpr(expr, env, ctx) {
				kind = "map"
				if mapValueIsString(expr, env, ctx) {
					isStr = true
				}
			} else if isStringExpr(expr) {
				kind = "string"
				isStr = true
			}
			expr = &IndexExpr{Target: expr, Index: idx, Kind: kind, IsString: isStr}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Step == nil && op.Index.Colon2 == nil:
			var startExpr, endExpr Expr
			var err error
			if op.Index.Start != nil {
				startExpr, err = convertExpr(op.Index.Start, env, ctx)
				if err != nil {
					return nil, err
				}
			}
			if op.Index.End != nil {
				endExpr, err = convertExpr(op.Index.End, env, ctx)
				if err != nil {
					return nil, err
				}
			}
			kind := "list"
			isStr := false
			if isStringExpr(expr) {
				kind = "string"
				isStr = true
			}
			expr = &SliceExpr{Target: expr, Start: startExpr, End: endExpr, Kind: kind, IsString: isStr}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertExpr(call.Args[0], env, ctx)
			if err != nil {
				return nil, err
			}
			expr = &ContainsExpr{Str: expr, Sub: arg}
			i++
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary, env *types.Env, ctx *context) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		nr := &NameRef{Name: ctx.current(p.Selector.Root)}
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				nr.IsString = true
			}
		}
		return nr, nil
	case p.Call != nil:
		if p.Call.Func == "substring" && len(p.Call.Args) == 3 {
			strExpr, err := convertExpr(p.Call.Args[0], env, ctx)
			if err != nil {
				return nil, err
			}
			startExpr, err := convertExpr(p.Call.Args[1], env, ctx)
			if err != nil {
				return nil, err
			}
			endExpr, err := convertExpr(p.Call.Args[2], env, ctx)
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Str: strExpr, Start: startExpr, End: endExpr}, nil
		}
		ce := &CallExpr{Func: p.Call.Func}
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a, env, ctx)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ae)
		}
		if ce.Func == "len" && len(ce.Args) == 1 {
			if isMapExpr(ce.Args[0], env, ctx) {
				ce.Func = "maps:size"
			} else {
				ce.Func = "length"
			}
		} else if ce.Func == "values" && len(ce.Args) == 1 {
			ce.Func = "maps:values"
		}
		return ce, nil
	case p.Group != nil:
		return convertExpr(p.Group, env, ctx)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ae, err := convertExpr(e, env, ctx)
			if err != nil {
				return nil, err
			}
			elems[i] = ae
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(it.Key, env, ctx)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value, env, ctx)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.If != nil:
		return convertIf(p.If, env, ctx)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertIf(ifx *parser.IfExpr, env *types.Env, ctx *context) (Expr, error) {
	cond, err := convertExpr(ifx.Cond, env, ctx)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ifx.Then, env, ctx)
	if err != nil {
		return nil, err
	}
	elseExpr := Expr(&BoolLit{Value: false})
	if ifx.Else != nil {
		elseExpr, err = convertExpr(ifx.Else, env, ctx)
		if err != nil {
			return nil, err
		}
	} else if ifx.ElseIf != nil {
		elseExpr, err = convertIf(ifx.ElseIf, env, ctx)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: int64(*l.Int)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Null:
		return &AtomLit{Name: "nil"}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

// Emit renders Erlang source for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	ts := time.Now().UTC()
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	hash := ""
	if out, err := exec.Command("git", "rev-parse", "--short", "HEAD").Output(); err == nil {
		hash = strings.TrimSpace(string(out))
	}
	buf.WriteString("#!/usr/bin/env escript\n")
	buf.WriteString("-module(main).\n")
	exports := []string{"main/1"}
	for _, f := range p.Funs {
		exports = append(exports, fmt.Sprintf("%s/0", f.Name))
	}
	buf.WriteString("-export([" + strings.Join(exports, ", ") + "]).\n\n")
	buf.WriteString(fmt.Sprintf("%% Generated by Mochi transpiler v%s (%s) on %s\n\n", version(), hash, ts.Format("2006-01-02 15:04 MST")))
	for _, f := range p.Funs {
		f.emit(&buf)
	}
	buf.WriteString("main(_) ->\n")
	for i, s := range p.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		if i < len(p.Stmts)-1 {
			buf.WriteString(",\n")
		} else {
			buf.WriteString(".\n")
		}
	}
	return buf.Bytes()
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(b))
}
