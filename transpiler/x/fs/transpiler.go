//go:build slow

package fstrans

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

// Program represents a simple sequence of statements.
type Program struct {
	Stmts []Stmt
}

// varTypes holds the inferred type for each variable defined during
// transpilation. It is reset for every call to Transpile.
var varTypes map[string]string

func copyMap(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// LambdaExpr represents an inline function expression.
type LambdaExpr struct {
	Params []string
	Expr   Expr
	Body   []Stmt
}

func (l *LambdaExpr) emit(w io.Writer) {
	io.WriteString(w, "fun")
	for _, p := range l.Params {
		io.WriteString(w, " ")
		io.WriteString(w, p)
	}
	io.WriteString(w, " -> ")
	if l.Expr != nil {
		if needsParen(l.Expr) {
			io.WriteString(w, "(")
			l.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			l.Expr.emit(w)
		}
		return
	}
	if len(l.Body) == 0 {
		io.WriteString(w, "()")
		return
	}
	w.Write([]byte{'\n'})
	for i, st := range l.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		if i < len(l.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type FunDef struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FunDef) emit(w io.Writer) {
	io.WriteString(w, "let rec ")
	io.WriteString(w, f.Name)
	for _, p := range f.Params {
		io.WriteString(w, " ")
		io.WriteString(w, p)
	}
	io.WriteString(w, " =\n")
	for i, st := range f.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		if i < len(f.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	if r.Expr != nil {
		r.Expr.emit(w)
	}
}

// ListLit represents an F# list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		e.emit(w)
		if i < len(l.Elems)-1 {
			io.WriteString(w, "; ")
		}
	}
	io.WriteString(w, "]")
}

// AppendExpr represents append(list, elem).
type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	io.WriteString(w, " @ [")
	a.Elem.emit(w)
	io.WriteString(w, "]")
}

// SubstringExpr represents substring(str, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Str.emit(w)
	io.WriteString(w, ".Substring(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
	io.WriteString(w, ")")
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then\n")
	for idx, st := range i.Then {
		st.emit(w)
		if idx < len(i.Then)-1 {
			w.Write([]byte{'\n'})
		}
	}
	if len(i.Else) > 0 {
		io.WriteString(w, "\nelse\n")
		for idx, st := range i.Else {
			st.emit(w)
			if idx < len(i.Else)-1 {
				w.Write([]byte{'\n'})
			}
		}
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type AssignStmt struct {
	Name string
	Expr Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, s.Name)
	io.WriteString(w, " <- ")
	s.Expr.emit(w)
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while ")
	wst.Cond.emit(w)
	io.WriteString(w, " do\n")
	for i, st := range wst.Body {
		st.emit(w)
		if i < len(wst.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type ForStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fst *ForStmt) emit(w io.Writer) {
	io.WriteString(w, "for ")
	io.WriteString(w, fst.Name)
	io.WriteString(w, " in ")
	if fst.End != nil {
		fst.Start.emit(w)
		io.WriteString(w, " .. (")
		(&BinaryExpr{Left: fst.End, Op: "-", Right: &IntLit{Value: 1}}).emit(w)
		io.WriteString(w, ")")
	} else {
		fst.Start.emit(w)
	}
	io.WriteString(w, " do\n")
	for i, st := range fst.Body {
		st.emit(w)
		if i < len(fst.Body)-1 {
			w.Write([]byte{'\n'})
		}
	}
}

type LetStmt struct {
	Name    string
	Mutable bool
	Type    string
	Expr    Expr
}

func (s *LetStmt) emit(w io.Writer) {
	io.WriteString(w, "let ")
	if s.Mutable {
		io.WriteString(w, "mutable ")
	}
	io.WriteString(w, s.Name)
	if s.Type != "" {
		io.WriteString(w, ": ")
		io.WriteString(w, s.Type)
	}
	io.WriteString(w, " = ")
	if s.Expr != nil {
		s.Expr.emit(w)
	} else {
		io.WriteString(w, "0")
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	if u.Op != "-" {
		io.WriteString(w, " ")
	}
	if needsParen(u.Expr) {
		io.WriteString(w, "(")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	} else {
		u.Expr.emit(w)
	}
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "in" {
		rtyp := inferType(b.Right)
		if rtyp == "string" {
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
			io.WriteString(w, ".Contains(")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, ")")
		} else if rtyp == "list" {
			io.WriteString(w, "List.contains ")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, " ")
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
		} else {
			io.WriteString(w, "Seq.contains ")
			if needsParen(b.Left) {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			io.WriteString(w, " ")
			if needsParen(b.Right) {
				io.WriteString(w, "(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
		}
		return
	}
	if needsParen(b.Left) {
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emit(w)
	}
	io.WriteString(w, " ")
	io.WriteString(w, mapOp(b.Op))
	io.WriteString(w, " ")
	if needsParen(b.Right) {
		io.WriteString(w, "(")
		b.Right.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emit(w)
	}
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

type IdentExpr struct{ Name string }

func (i *IdentExpr) emit(w io.Writer) { io.WriteString(w, i.Name) }

type UnitLit struct{}

func (u *UnitLit) emit(w io.Writer) { io.WriteString(w, "()") }

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then ")
	if needsParen(i.Then) {
		io.WriteString(w, "(")
		i.Then.emit(w)
		io.WriteString(w, ")")
	} else {
		i.Then.emit(w)
	}
	io.WriteString(w, " else ")
	if needsParen(i.Else) {
		io.WriteString(w, "(")
		i.Else.emit(w)
		io.WriteString(w, ")")
	} else {
		i.Else.emit(w)
	}
}

func mapOp(op string) string {
	switch op {
	case "==":
		return "="
	case "!=":
		return "<>"
	default:
		return op
	}
}

func mapMethod(name string) string {
	switch name {
	case "contains":
		return "Contains"
	default:
		return name
	}
}

func precedence(op string) int {
	switch op {
	case "||":
		return 1
	case "&&":
		return 2
	case "==", "!=", "<", "<=", ">", ">=":
		return 3
	case "in":
		return 3
	case "+", "-":
		return 4
	case "*", "/", "%":
		return 5
	default:
		return 0
	}
}

func needsParen(e Expr) bool {
	switch e.(type) {
	case *BinaryExpr, *UnaryExpr, *IfExpr, *AppendExpr, *SubstringExpr, *CallExpr, *IndexExpr, *LambdaExpr, *FieldExpr, *MethodCallExpr, *SliceExpr, *CastExpr:
		return true
	default:
		return false
	}
}

func inferType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		return "int"
	case *StringLit:
		return "string"
	case *BoolLit:
		return "bool"
	case *ListLit:
		return "list"
	case *IdentExpr:
		if t, ok := varTypes[v.Name]; ok {
			return t
		}
		return ""
	case *UnaryExpr:
		if v.Op == "not" {
			return "bool"
		}
		return inferType(v.Expr)
	case *BinaryExpr:
		switch v.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return "bool"
		case "+", "-", "*", "/", "%":
			lt := inferType(v.Left)
			rt := inferType(v.Right)
			if lt == rt {
				return lt
			}
		}
	case *AppendExpr:
		return "list"
	case *SubstringExpr:
		return "string"
	case *SliceExpr:
		return inferType(v.Target)
	case *CallExpr:
		switch v.Func {
		case "string":
			return "string"
		case "Seq.length", "List.length", "String.length":
			return "int"
		case "Seq.sum", "List.sum":
			return "int"
		case "Seq.averageBy float", "List.averageBy float":
			return "float"
		}
	case *MethodCallExpr:
		switch v.Name {
		case "contains", "Contains":
			return "bool"
		}
	case *IfExpr:
		t := inferType(v.Then)
		e2 := inferType(v.Else)
		if t == e2 {
			return t
		}
	}
	return ""
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	for _, a := range c.Args {
		io.WriteString(w, " ")
		if needsParen(a) {
			io.WriteString(w, "(")
			a.emit(w)
			io.WriteString(w, ")")
		} else {
			a.emit(w)
		}
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, ".[")
	i.Index.emit(w)
	io.WriteString(w, "]")
}

// FieldExpr represents a field selection like obj.field.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, f.Name)
}

// MethodCallExpr represents a method invocation target.method(args).
type MethodCallExpr struct {
	Target Expr
	Name   string
	Args   []Expr
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, mapMethod(m.Name))
	io.WriteString(w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// SliceExpr represents slicing start:end on strings.
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

func (s *SliceExpr) emit(w io.Writer) {
	if inferType(s.Target) == "list" {
		s.Target.emit(w)
		io.WriteString(w, ".[")
		s.Start.emit(w)
		io.WriteString(w, "..(")
		(&BinaryExpr{Left: s.End, Op: "-", Right: &IntLit{Value: 1}}).emit(w)
		io.WriteString(w, ")]")
		return
	}
	s.Target.emit(w)
	io.WriteString(w, ".Substring(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
	io.WriteString(w, ")")
}

// CastExpr represents expr as type.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "int ")
		if needsParen(c.Expr) {
			io.WriteString(w, "(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			c.Expr.emit(w)
		}
	default:
		c.Expr.emit(w)
	}
}

// Emit generates formatted F# code from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	for i, st := range prog.Stmts {
		st.emit(&buf)
		if i < len(prog.Stmts)-1 {
			buf.WriteByte('\n')
		}
	}
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		buf.WriteByte('\n')
	}
	return buf.Bytes()
}

func header() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	return fmt.Sprintf("// Generated %s\nopen System\n\n", ts.Format("2006-01-02 15:04 MST"))
}

// Transpile converts a Mochi program to a simple F# AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env
	varTypes = map[string]string{}
	p := &Program{}
	for _, st := range prog.Statements {
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		p.Stmts = append(p.Stmts, conv)
	}
	return p, nil
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		}
		declared := ""
		if st.Let.Type != nil && st.Let.Type.Simple != nil {
			declared = *st.Let.Type.Simple
		} else {
			declared = inferType(e)
		}
		varTypes[st.Let.Name] = declared
		typ := declared
		if typ == "list" {
			typ = ""
		}
		return &LetStmt{Name: st.Let.Name, Expr: e, Type: typ}, nil
	case st.Var != nil:
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		}
		declared := ""
		if st.Var.Type != nil && st.Var.Type.Simple != nil {
			declared = *st.Var.Type.Simple
		} else {
			declared = inferType(e)
		}
		varTypes[st.Var.Name] = declared
		typ := declared
		if typ == "list" {
			typ = ""
		}
		return &LetStmt{Name: st.Var.Name, Expr: e, Type: typ, Mutable: true}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		varTypes[st.Assign.Name] = inferType(e)
		return &AssignStmt{Name: st.Assign.Name, Expr: e}, nil
	case st.Return != nil:
		var e Expr
		if st.Return.Value != nil {
			var err error
			e, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: e}, nil
	case st.Fun != nil:
		save := varTypes
		varTypes = copyMap(varTypes)
		for _, p := range st.Fun.Params {
			if p.Type != nil && p.Type.Simple != nil {
				varTypes[p.Name] = *p.Type.Simple
			}
		}
		body := make([]Stmt, len(st.Fun.Body))
		for i, s := range st.Fun.Body {
			cs, err := convertStmt(s)
			if err != nil {
				varTypes = save
				return nil, err
			}
			body[i] = cs
		}
		params := make([]string, len(st.Fun.Params))
		for i, p := range st.Fun.Params {
			params[i] = p.Name
		}
		varTypes = save
		return &FunDef{Name: st.Fun.Name, Params: params, Body: body}, nil
	case st.While != nil:
		cond, err := convertExpr(st.While.Cond)
		if err != nil {
			return nil, err
		}
		body := make([]Stmt, len(st.While.Body))
		for i, s := range st.While.Body {
			cs, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			body[i] = cs
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		start, err := convertExpr(st.For.Source)
		if err != nil {
			return nil, err
		}
		var end Expr
		if st.For.RangeEnd != nil {
			end, err = convertExpr(st.For.RangeEnd)
			if err != nil {
				return nil, err
			}
		}
		body := make([]Stmt, len(st.For.Body))
		for i, s := range st.For.Body {
			cs, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			body[i] = cs
		}
		return &ForStmt{Name: st.For.Name, Start: start, End: end, Body: body}, nil
	case st.If != nil:
		return convertIfStmt(st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := convertUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	exprs := []Expr{left}
	ops := []string{}
	for _, op := range e.Binary.Right {
		right, err := convertPostfix(op.Right)
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
			exprs = append(exprs, &BinaryExpr{Left: l, Op: o, Right: r})
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
		exprs = append(exprs, &BinaryExpr{Left: l, Op: o, Right: r})
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("expr reduce error")
	}
	return exprs[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			expr = &UnaryExpr{Op: "-", Expr: expr}
		case "!":
			expr = &UnaryExpr{Op: "not", Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil && op.Index.End == nil && op.Index.Step == nil && op.Index.Start != nil:
			idx, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Index != nil && op.Index.Colon != nil && op.Index.End != nil && op.Index.Step == nil && op.Index.Colon2 == nil:
			start, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(op.Index.End)
			if err != nil {
				return nil, err
			}
			expr = &SliceExpr{Target: expr, Start: start, End: end}
		case op.Field != nil:
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ae, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = ae
			}
			if fe, ok := expr.(*FieldExpr); ok {
				expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
			} else {
				if id, ok := expr.(*IdentExpr); ok {
					expr = &CallExpr{Func: id.Name, Args: args}
				} else {
					return nil, fmt.Errorf("unsupported postfix")
				}
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			expr = &CastExpr{Expr: expr, Type: *op.Cast.Type.Simple}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 1 {
				switch inferType(args[0]) {
				case "bool":
					return &CallExpr{Func: "printfn \"%b\"", Args: []Expr{args[0]}}, nil
				case "int":
					return &CallExpr{Func: "printfn \"%d\"", Args: []Expr{args[0]}}, nil
				case "list":
					mapped := &CallExpr{Func: "List.map string", Args: []Expr{args[0]}}
					concat := &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: " "}, mapped}}
					wrapped := &BinaryExpr{Left: &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: concat}, Op: "+", Right: &StringLit{Value: "]"}}
					return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{wrapped}}, nil
				default:
					arg := &CallExpr{Func: "string", Args: []Expr{args[0]}}
					return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{arg}}, nil
				}
			}
			elems := make([]Expr, len(args))
			for i, a := range args {
				switch inferType(a) {
				case "bool":
					elems[i] = &CallExpr{Func: "sprintf \"%b\"", Args: []Expr{a}}
				case "int":
					elems[i] = &CallExpr{Func: "sprintf \"%d\"", Args: []Expr{a}}
				default:
					elems[i] = &CallExpr{Func: "string", Args: []Expr{a}}
				}
			}
			list := &ListLit{Elems: elems}
			concat := &CallExpr{Func: "String.concat", Args: []Expr{&StringLit{Value: " "}, list}}
			return &CallExpr{Func: "printfn \"%s\"", Args: []Expr{concat}}, nil
		case "len":
			fn := "Seq.length"
			if len(args) == 1 {
				switch inferType(args[0]) {
				case "list":
					fn = "List.length"
				case "string":
					fn = "String.length"
				}
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "str":
			return &CallExpr{Func: "string", Args: args}, nil
		case "sum":
			fn := "Seq.sum"
			if len(args) == 1 && inferType(args[0]) == "list" {
				fn = "List.sum"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "avg":
			fn := "Seq.averageBy float"
			if len(args) == 1 && inferType(args[0]) == "list" {
				fn = "List.averageBy float"
			}
			return &CallExpr{Func: fn, Args: args}, nil
		case "append":
			if len(args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		case "substring":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		default:
			return &CallExpr{Func: p.Call.Func, Args: args}, nil
		}
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.FunExpr != nil:
		save := varTypes
		varTypes = copyMap(varTypes)
		params := make([]string, len(p.FunExpr.Params))
		for i, par := range p.FunExpr.Params {
			params[i] = par.Name
			if par.Type != nil && par.Type.Simple != nil {
				varTypes[par.Name] = *par.Type.Simple
			}
		}
		if p.FunExpr.ExprBody != nil {
			body, err := convertExpr(p.FunExpr.ExprBody)
			varTypes = save
			if err != nil {
				return nil, err
			}
			return &LambdaExpr{Params: params, Expr: body}, nil
		}
		stmts := make([]Stmt, len(p.FunExpr.BlockBody))
		for i, s := range p.FunExpr.BlockBody {
			cs, err := convertStmt(s)
			if err != nil {
				varTypes = save
				return nil, err
			}
			stmts[i] = cs
		}
		varTypes = save
		return &LambdaExpr{Params: params, Body: stmts}, nil
	case p.Selector != nil:
		expr := Expr(&IdentExpr{Name: p.Selector.Root})
		for _, name := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: name}
		}
		return expr, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func convertIfExpr(in *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(in.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(in.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if in.ElseIf != nil {
		elseExpr, err = convertIfExpr(in.ElseIf)
	} else if in.Else != nil {
		elseExpr, err = convertExpr(in.Else)
	} else {
		elseExpr = &UnitLit{}
	}
	if err != nil {
		return nil, err
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertIfStmt(in *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(in.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts := make([]Stmt, len(in.Then))
	for i, s := range in.Then {
		cs, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		thenStmts[i] = cs
	}
	var elseStmts []Stmt
	if in.ElseIf != nil {
		es, err := convertIfStmt(in.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{es}
	} else if len(in.Else) > 0 {
		elseStmts = make([]Stmt, len(in.Else))
		for i, s := range in.Else {
			cs, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			elseStmts[i] = cs
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}
