//go:build slow

package rkt

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type PrintStmt struct{ Expr Expr }

func (p *PrintStmt) emit(w io.Writer) {
	io.WriteString(w, "(displayln ")
	p.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type LetStmt struct {
	Name string
	Expr Expr
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(define %s ", l.Name)
	l.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s ", a.Name)
	a.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "(if ")
	i.Cond.emit(w)
	io.WriteString(w, " (begin\n")
	for _, s := range i.Then {
		s.emit(w)
	}
	io.WriteString(w, ")")
	if len(i.Else) > 0 {
		io.WriteString(w, " (begin\n")
		for _, s := range i.Else {
			s.emit(w)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")\n")
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "(let loop ()\n  (when ")
	wst.Cond.emit(w)
	io.WriteString(w, "\n")
	for _, st := range wst.Body {
		io.WriteString(w, "    ")
		st.emit(w)
	}
	io.WriteString(w, "    (loop)))\n")
}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func convertIfExpr(n *parser.IfExpr, env *types.Env) (Expr, error) {
	if n == nil || n.Else == nil && n.ElseIf == nil {
		return nil, fmt.Errorf("unsupported if expression")
	}
	cond, err := convertExpr(n.Cond, env)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(n.Then, env)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if n.ElseIf != nil {
		elseExpr, err = convertIfExpr(n.ElseIf, env)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr, err = convertExpr(n.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(if ")
	i.Cond.emit(w)
	io.WriteString(w, " ")
	i.Then.emit(w)
	io.WriteString(w, " ")
	i.Else.emit(w)
	io.WriteString(w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	switch u.Op {
	case "-":
		io.WriteString(w, "(- ")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	case "!":
		io.WriteString(w, "(not ")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		fmt.Fprintf(w, "(%s ", u.Op)
		u.Expr.emit(w)
		io.WriteString(w, ")")
	}
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) { io.WriteString(w, n.Name) }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "(list")
	for _, e := range l.Elems {
		io.WriteString(w, " ")
		e.emit(w)
	}
	io.WriteString(w, ")")
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	io.WriteString(w, c.Func)
	for _, a := range c.Args {
		io.WriteString(w, " ")
		a.emit(w)
	}
	io.WriteString(w, ")")
}

type LenExpr struct{ Arg Expr }

func (l *LenExpr) emit(w io.Writer) {
	io.WriteString(w, "(if (string? ")
	l.Arg.emit(w)
	io.WriteString(w, ") (string-length ")
	l.Arg.emit(w)
	io.WriteString(w, ") (length ")
	l.Arg.emit(w)
	io.WriteString(w, "))")
}

type AvgExpr struct{ Arg Expr }

func (a *AvgExpr) emit(w io.Writer) {
	io.WriteString(w, "(if (null? ")
	a.Arg.emit(w)
	io.WriteString(w, ") 0 (exact->inexact (/ (apply + ")
	a.Arg.emit(w)
	io.WriteString(w, ") (length ")
	a.Arg.emit(w)
	io.WriteString(w, "))))")
}

type BinaryExpr struct {
	Op          string
	Left, Right Expr
}

type BoolToInt struct{ Expr Expr }

func (b *BoolToInt) emit(w io.Writer) {
	io.WriteString(w, "(if ")
	b.Expr.emit(w)
	io.WriteString(w, " 1 0)")
}

func (b *BinaryExpr) emit(w io.Writer) {
	switch b.Op {
	case "!=":
		io.WriteString(w, "(not (= ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, "))")
	case "==":
		io.WriteString(w, "(= ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	default:
		fmt.Fprintf(w, "(%s ", b.Op)
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	}
}

func repoRoot() string {
	dir, _ := os.Getwd()
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
		return "unknown"
	}
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "unknown"
	}
	return strings.TrimSpace(string(b))
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04:05 MST")
	return fmt.Sprintf(";; Generated by Mochi %s on %s\n#lang racket\n(define (_div a b) (if (and (integer? a) (integer? b)) (quotient a b) (/ a b)))\n", version(), ts)
}

func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		s.emit(w)
	}
	return nil
}

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	r := &Program{}
	for _, st := range prog.Statements {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		if s != nil {
			r.Stmts = append(r.Stmts, s)
		}
	}
	_ = env
	return r, nil
}

func convertStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Let != nil && st.Let.Value != nil:
		e, err := convertExpr(st.Let.Value, env)
		if err != nil {
			return nil, err
		}
		return &LetStmt{Name: st.Let.Name, Expr: e}, nil
	case st.Var != nil && st.Var.Value != nil:
		e, err := convertExpr(st.Var.Value, env)
		if err != nil {
			return nil, err
		}
		return &LetStmt{Name: st.Var.Name, Expr: e}, nil
	case st.Assign != nil:
		e, err := convertExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: e}, nil
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" && len(call.Args) == 1 {
			e, err := convertExpr(call.Args[0], env)
			if err != nil {
				return nil, err
			}
			if isBoolExpr(call.Args[0]) {
				e = &BoolToInt{Expr: e}
			}
			return &PrintStmt{Expr: e}, nil
		}
		return nil, fmt.Errorf("unsupported expression statement")
	case st.If != nil:
		return convertIfStmt(st.If, env)
	case st.While != nil:
		return convertWhileStmt(st.While, env)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertStatements(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		out = append(out, s)
	}
	return out, nil
}

func convertIfStmt(n *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(n.Cond, env)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStatements(n.Then, env)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	switch {
	case n.ElseIf != nil:
		s, err := convertIfStmt(n.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	case len(n.Else) > 0:
		elseStmts, err = convertStatements(n.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(n *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(n.Cond, env)
	if err != nil {
		return nil, err
	}
	body, err := convertStatements(n.Body, env)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary, env)
}

func precedence(op string) int {
	switch op {
	case "or":
		return 1
	case "and":
		return 2
	case "==", "!=", "<", "<=", ">", ">=":
		return 3
	case "+", "-", "string-append":
		return 4
	case "*", "/", "modulo":
		return 5
	default:
		return 0
	}
}

func convertBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	left, err := convertUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	exprs := []Expr{left}
	ops := []string{}
	for _, part := range b.Right {
		right, err := convertPostfix(part.Right, env)
		if err != nil {
			return nil, err
		}
		op := part.Op
		if op == "+" && (types.IsStringUnary(b.Left, env) || types.IsStringPostfix(part.Right, env)) {
			op = "string-append"
		}
		if op == "&&" {
			op = "and"
		}
		if op == "||" {
			op = "or"
		}
		if op == "/" {
			op = "_div"
		}
		if op == "<" || op == "<=" || op == ">" || op == ">=" {
			if types.IsStringUnary(b.Left, env) || types.IsStringPostfix(part.Right, env) {
				switch op {
				case "<":
					op = "string<?"
				case "<=":
					op = "string<=?"
				case ">":
					op = "string>?"
				case ">=":
					op = "string>=?"
				}
			}
		}
		if op == "%" {
			op = "modulo"
		}
		for len(ops) > 0 && precedence(ops[len(ops)-1]) >= precedence(op) {
			r := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			l := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			o := ops[len(ops)-1]
			ops = ops[:len(ops)-1]
			exprs = append(exprs, &BinaryExpr{Op: o, Left: l, Right: r})
		}
		ops = append(ops, op)
		exprs = append(exprs, right)
	}
	for len(ops) > 0 {
		r := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		l := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		o := ops[len(ops)-1]
		ops = ops[:len(ops)-1]
		exprs = append(exprs, &BinaryExpr{Op: o, Left: l, Right: r})
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("expr reduce error")
	}
	return exprs[0], nil
}

func convertUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			expr = &UnaryExpr{Op: op, Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if len(pf.Ops) > 0 {
		return nil, fmt.Errorf("postfix ops not supported")
	}
	return convertPrimary(pf.Target, env)
}

func convertPrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.List != nil:
		return convertList(p.List, env)
	case p.Call != nil:
		return convertCall(p.Call, env)
	case p.If != nil:
		return convertIfExpr(p.If, env)
	case p.Group != nil:
		return convertExpr(p.Group, env)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &Name{Name: p.Selector.Root}, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	if l.Int != nil {
		return &IntLit{Value: int(*l.Int)}, nil
	}
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, nil
	}
	return nil, fmt.Errorf("unsupported literal")
}

func convertList(l *parser.ListLiteral, env *types.Env) (Expr, error) {
	var elems []Expr
	for _, e := range l.Elems {
		ce, err := convertExpr(e, env)
		if err != nil {
			return nil, err
		}
		elems = append(elems, ce)
	}
	return &ListLit{Elems: elems}, nil
}

func convertCall(c *parser.CallExpr, env *types.Env) (Expr, error) {
	var args []Expr
	for _, a := range c.Args {
		ae, err := convertExpr(a, env)
		if err != nil {
			return nil, err
		}
		args = append(args, ae)
	}
	switch c.Func {
	case "len":
		if len(args) != 1 {
			return nil, fmt.Errorf("len expects 1 arg")
		}
		return &LenExpr{Arg: args[0]}, nil
	case "append":
		if len(args) == 2 {
			return &CallExpr{Func: "append", Args: []Expr{args[0], &CallExpr{Func: "list", Args: []Expr{args[1]}}}}, nil
		}
	case "avg":
		if len(args) == 1 {
			return &AvgExpr{Arg: args[0]}, nil
		}
	}
	return nil, fmt.Errorf("unsupported call")
}

func isBoolExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) == 0 {
		p := e.Binary.Left.Value.Target
		return p != nil && p.Lit != nil && p.Lit.Bool != nil
	}
	last := e.Binary.Right[len(e.Binary.Right)-1].Op
	switch last {
	case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
		return true
	default:
		return false
	}
}
