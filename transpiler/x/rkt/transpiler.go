//go:build slow

package rkt

import (
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

type ListAssignStmt struct {
	Name  string
	Index Expr
	Expr  Expr
}

func (l *ListAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s (list-set %s ", l.Name, l.Name)
	l.Index.emit(w)
	io.WriteString(w, " ")
	l.Expr.emit(w)
	io.WriteString(w, "))\n")
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

type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s (for/list ([item %s])\n  ", u.Target, u.Target)
	if u.Cond != nil {
		io.WriteString(w, "(begin\n    (when ")
		u.Cond.emit(w)
		io.WriteString(w, "\n")
		for i, f := range u.Fields {
			io.WriteString(w, "      (set! item (hash-set item \"")
			io.WriteString(w, f)
			io.WriteString(w, "\" ")
			u.Values[i].emit(w)
			io.WriteString(w, "))\n")
		}
		io.WriteString(w, "    )\n    item)))\n")
	} else {
		io.WriteString(w, "(begin\n")
		for i, f := range u.Fields {
			io.WriteString(w, "    (set! item (hash-set item \"")
			io.WriteString(w, f)
			io.WriteString(w, "\" ")
			u.Values[i].emit(w)
			io.WriteString(w, "))\n")
		}
		io.WriteString(w, "    item)))\n")
	}
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	r.Expr.emit(w)
	io.WriteString(w, "\n")
}

type FunDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FunDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "(define (%s", f.Name)
	for _, p := range f.Params {
		fmt.Fprintf(w, " %s", p)
	}
	io.WriteString(w, ")\n")
	for _, st := range f.Body {
		st.emit(w)
	}
	io.WriteString(w, ")\n")
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

type IndexExpr struct {
	Target   Expr
	Index    Expr
	IsString bool
	IsMap    bool
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.IsString {
		io.WriteString(w, "(string-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, ")")
	} else if i.IsMap {
		io.WriteString(w, "(hash-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "(list-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, ")")
	}
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

type SumExpr struct{ Arg Expr }

func (s *SumExpr) emit(w io.Writer) {
	io.WriteString(w, "(apply + ")
	s.Arg.emit(w)
	io.WriteString(w, ")")
}

type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	io.WriteString(w, "(format \"~a\" ")
	s.Arg.emit(w)
	io.WriteString(w, ")")
}

type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "(inexact->exact (string->number ")
		c.Value.emit(w)
		io.WriteString(w, "))")
	case "float":
		io.WriteString(w, "(exact->inexact ")
		c.Value.emit(w)
		io.WriteString(w, ")")
	case "string":
		io.WriteString(w, "(format \"~a\" ")
		c.Value.emit(w)
		io.WriteString(w, ")")
	default:
		c.Value.emit(w)
	}
}

func isSimpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func substituteFields(e Expr, varName string, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Name:
		if fields[ex.Name] {
			return &IndexExpr{Target: &Name{Name: varName}, Index: &StringLit{Value: ex.Name}, IsMap: true}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFields(ex.Left, varName, fields)
		ex.Right = substituteFields(ex.Right, varName, fields)
		return ex
	case *UnaryExpr:
		ex.Expr = substituteFields(ex.Expr, varName, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		return ex
	case *IndexExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		ex.Index = substituteFields(ex.Index, varName, fields)
		return ex
	case *IfExpr:
		ex.Cond = substituteFields(ex.Cond, varName, fields)
		ex.Then = substituteFields(ex.Then, varName, fields)
		ex.Else = substituteFields(ex.Else, varName, fields)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFields(ex.Elems[i], varName, fields)
		}
		return ex
	case *LenExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *AvgExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *SumExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *StrExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *CastExpr:
		ex.Value = substituteFields(ex.Value, varName, fields)
		return ex
	default:
		return ex
	}
}

type BinaryExpr struct {
	Op          string
	Left, Right Expr
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

func gitTime() string {
	root := repoRoot()
	if root == "" {
		return time.Now().Format("2006-01-02 15:04 -0700")
	}
	out, err := exec.Command("git", "-C", root, "log", "-1", "--format=%cI").Output()
	if err != nil {
		return time.Now().Format("2006-01-02 15:04 -0700")
	}
	t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out)))
	if err != nil {
		return time.Now().Format("2006-01-02 15:04 -0700")
	}
	if loc, err := time.LoadLocation("Asia/Bangkok"); err == nil {
		t = t.In(loc)
	}
	return t.Format("2006-01-02 15:04 -0700")
}

func header() string {
	return fmt.Sprintf(";; Generated by Mochi %s on %s\n#lang racket\n(require racket/list racket/string)\n\n", version(), gitTime())
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
		if len(st.Assign.Index) == 1 {
			idxExpr, err := convertExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			return &ListAssignStmt{Name: st.Assign.Name, Index: idxExpr, Expr: e}, nil
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: e}, nil
	case st.Return != nil:
		var e Expr
		var err error
		if st.Return.Value != nil {
			e, err = convertExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		} else {
			e = &Name{Name: "void"}
		}
		return &ReturnStmt{Expr: e}, nil
	case st.Fun != nil:
		return convertFunStmt(st.Fun, env)
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" && len(call.Args) == 1 {
			e, err := convertExpr(call.Args[0], env)
			if err != nil {
				return nil, err
			}
			return &PrintStmt{Expr: e}, nil
		}
		return nil, fmt.Errorf("unsupported expression statement")
	case st.Update != nil:
		up, err := convertUpdateStmt(st.Update, env)
		if err != nil {
			return nil, err
		}
		return up, nil
	case st.Type != nil:
		return nil, nil
	case st.Test != nil:
		// ignore test blocks
		return nil, nil
	case st.Expect != nil:
		// ignore expects
		return nil, nil
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

func convertFunStmt(fn *parser.FunStmt, env *types.Env) (Stmt, error) {
	child := types.NewEnv(env)
	var params []string
	for _, p := range fn.Params {
		params = append(params, p.Name)
		child.SetVar(p.Name, types.AnyType{}, true)
	}
	body, err := convertStatements(fn.Body, child)
	if err != nil {
		return nil, err
	}
	return &FunDecl{Name: fn.Name, Params: params, Body: body}, nil
}

func convertUpdateStmt(u *parser.UpdateStmt, env *types.Env) (Stmt, error) {
	t, err := env.GetVar(u.Target)
	if err != nil {
		return nil, err
	}
	lt, ok := t.(types.ListType)
	if !ok {
		return nil, fmt.Errorf("update target not list")
	}
	st, ok := lt.Elem.(types.StructType)
	if !ok {
		return nil, fmt.Errorf("update element not struct")
	}
	child := types.NewEnv(env)
	fields := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fields[name] = true
	}
	var names []string
	var values []Expr
	for _, it := range u.Set.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(it.Value, child)
		if err != nil {
			return nil, err
		}
		val = substituteFields(val, "item", fields)
		names = append(names, key)
		values = append(values, val)
	}
	var cond Expr
	if u.Where != nil {
		cond, err = convertExpr(u.Where, child)
		if err != nil {
			return nil, err
		}
		cond = substituteFields(cond, "item", fields)
	}
	return &UpdateStmt{Target: u.Target, Fields: names, Values: values, Cond: cond}, nil
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
			lt := types.TypeOfUnary(b.Left, env)
			rt := types.TypeOfPostfix(part.Right, env)
			if (types.IsIntType(lt) || types.IsInt64Type(lt)) && (types.IsIntType(rt) || types.IsInt64Type(rt)) {
				op = "quotient"
			} else {
				op = "/"
			}
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
	expr, err := convertPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	ops := pf.Ops
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
		for _, t := range pf.Target.Selector.Tail {
			ops = append([]*parser.PostfixOp{{Field: &parser.FieldOp{Name: t}}}, ops...)
		}
	}
	pendingField := ""
	for _, op := range ops {
		switch {
		case op.Field != nil:
			pendingField = op.Field.Name
		case op.Call != nil:
			if pendingField != "" {
				switch pendingField {
				case "contains":
					if len(op.Call.Args) != 1 {
						return nil, fmt.Errorf("contains expects 1 arg")
					}
					arg, err := convertExpr(op.Call.Args[0], env)
					if err != nil {
						return nil, err
					}
					expr = &CallExpr{Func: "string-contains?", Args: []Expr{expr, arg}}
				default:
					return nil, fmt.Errorf("unsupported method %s", pendingField)
				}
				pendingField = ""
			} else if n, ok := expr.(*Name); ok {
				expr, err = convertCall(&parser.CallExpr{Func: n.Name, Args: op.Call.Args}, env)
				if err != nil {
					return nil, err
				}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Index != nil && op.Index.Colon == nil:
			idx, err := convertExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			isStr := types.IsStringPrimary(pf.Target, env)
			isMap := types.IsMapPrimary(pf.Target, env)
			expr = &IndexExpr{Target: expr, Index: idx, IsString: isStr, IsMap: isMap}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.List != nil:
		return convertList(p.List, env)
	case p.Struct != nil:
		return convertStruct(p.Struct, env)
	case p.Call != nil:
		return convertCall(p.Call, env)
	case p.If != nil:
		return convertIfExpr(p.If, env)
	case p.Group != nil:
		return convertExpr(p.Group, env)
	case p.Selector != nil:
		return &Name{Name: p.Selector.Root}, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	if l.Int != nil {
		return &IntLit{Value: int(*l.Int)}, nil
	}
	if l.Bool != nil {
		if *l.Bool {
			return &Name{Name: "#t"}, nil
		}
		return &Name{Name: "#f"}, nil
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

func convertStruct(s *parser.StructLiteral, env *types.Env) (Expr, error) {
	var args []Expr
	for _, f := range s.Fields {
		val, err := convertExpr(f.Value, env)
		if err != nil {
			return nil, err
		}
		args = append(args, &StringLit{Value: f.Name})
		args = append(args, val)
	}
	return &CallExpr{Func: "hash", Args: args}, nil
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
	case "sum":
		if len(args) == 1 {
			return &SumExpr{Arg: args[0]}, nil
		}
	case "count":
		if len(args) == 1 {
			return &LenExpr{Arg: args[0]}, nil
		}
	case "str":
		if len(args) == 1 {
			return &StrExpr{Arg: args[0]}, nil
		}
	default:
		return &CallExpr{Func: c.Func, Args: args}, nil
	}
	return nil, fmt.Errorf("unsupported call")
}
