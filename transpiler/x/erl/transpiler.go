package erl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

// Program is a minimal Erlang module consisting of sequential statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// PrintStmt represents a call to print/1.
type PrintStmt struct{ Expr Expr }

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

type IntLit struct{ Value int64 }

type BoolLit struct{ Value bool }

type StringLit struct{ Value string }

func (p *PrintStmt) emit(w io.Writer) {
	if isStringExpr(p.Expr) {
		io.WriteString(w, "io:format(\"~s~n\", [")
	} else {
		io.WriteString(w, "io:format(\"~p~n\", [")
	}
	p.Expr.emit(w)
	io.WriteString(w, "])")
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
	default:
		return false
	}
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", sanitize(l.Name))
	l.Expr.emit(w)
}

func (c *CallExpr) emit(w io.Writer) {
	name := c.Func
	switch c.Func {
	case "len":
		name = "length"
	case "str":
		name = "integer_to_list"
	}
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

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(if ")
	i.Cond.emit(w)
	io.WriteString(w, " -> ")
	i.Then.emit(w)
	io.WriteString(w, "; true -> ")
	i.Else.emit(w)
	io.WriteString(w, " end)")
}

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, sanitize(n.Name)) }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

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
	_ = env
	p := &Program{}
	strVars := map[string]bool{}
	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			e, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			if isStringExpr(e) {
				strVars[st.Let.Name] = true
			}
			p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Expr: e})
		case st.Expr != nil:
			e, err := convertExpr(st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			if c, ok := e.(*CallExpr); ok && c.Func == "print" && len(c.Args) == 1 {
				arg := c.Args[0]
				if n, ok := arg.(*NameRef); ok {
					if strVars[n.Name] {
						n.IsString = true
					}
				}
				p.Stmts = append(p.Stmts, &PrintStmt{Expr: arg})
			} else {
				return nil, fmt.Errorf("unsupported expression")
			}
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return p, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	left, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	ops := make([]string, len(b.Right))
	exprs := []Expr{left}
	for i, op := range b.Right {
		r, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, r)
		ops[i] = op.Op
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

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		switch {
		case op.Cast != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int":
			expr = &CallExpr{Func: "list_to_integer", Args: []Expr{expr}}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &NameRef{Name: p.Selector.Root}, nil
	case p.Call != nil:
		ce := &CallExpr{Func: p.Call.Func}
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ae)
		}
		return ce, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ae, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ae
		}
		return &ListLit{Elems: elems}, nil
	case p.If != nil:
		return convertIf(p.If)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertIf(ifx *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ifx.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ifx.Then)
	if err != nil {
		return nil, err
	}
	elseExpr := Expr(&BoolLit{Value: false})
	if ifx.Else != nil {
		elseExpr, err = convertExpr(ifx.Else)
		if err != nil {
			return nil, err
		}
	} else if ifx.ElseIf != nil {
		elseExpr, err = convertIf(ifx.ElseIf)
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
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

// Emit renders Erlang source for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString("#!/usr/bin/env escript\n")
	buf.Write(header())
	buf.WriteString("-module(main).\n")
	buf.WriteString("-export([main/1]).\n\n")
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

func header() []byte {
	t := time.Now().UTC().Format(time.RFC3339)
	return []byte(fmt.Sprintf("%% Generated by Mochi transpiler v%s on %s\n", version(), t))
}
