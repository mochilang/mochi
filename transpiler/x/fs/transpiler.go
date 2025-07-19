//go:build slow

package fstrans

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

// Program represents a simple sequence of statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

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

func precedence(op string) int {
	switch op {
	case "||":
		return 1
	case "&&":
		return 2
	case "==", "!=", "<", "<=", ">", ">=":
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
	case *BinaryExpr, *UnaryExpr, *IfExpr, *AppendExpr, *SubstringExpr, *CallExpr:
		return true
	default:
		return false
	}
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
	ver := readVersion()
	ts := time.Now().Format("2006-01-02 15:04:05 MST")
	return fmt.Sprintf("// Mochi %s - generated %s\nopen System\n\n"+
		"let print (x: obj) =\n"+
		"    match x with\n"+
		"    | :? bool as b -> printfn \"%%d\" (if b then 1 else 0)\n"+
		"    | :? float as f -> printfn \"%%.1f\" f\n"+
		"    | :? string as s -> printfn \"%%s\" s\n"+
		"    | :? System.Collections.IEnumerable as e ->\n"+
		"        e |> Seq.cast<obj> |> Seq.map string |> String.concat \" \" |> printfn \"%%s\"\n"+
		"    | _ -> printfn \"%%O\" x\n\n", ver, ts)
}

func readVersion() string {
	_, file, _, _ := runtime.Caller(0)
	dir := filepath.Dir(file)
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			b, err := os.ReadFile(filepath.Join(dir, "VERSION"))
			if err != nil {
				return "unknown"
			}
			return strings.TrimSpace(string(b))
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	return "unknown"
}

// Transpile converts a Mochi program to a simple F# AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env
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
		typ := ""
		if st.Let.Type != nil && st.Let.Type.Simple != nil {
			typ = *st.Let.Type.Simple
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
		typ := ""
		if st.Var.Type != nil && st.Var.Type.Simple != nil {
			typ = *st.Var.Type.Simple
		}
		return &LetStmt{Name: st.Var.Name, Expr: e, Type: typ, Mutable: true}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: e}, nil
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
	if pf == nil || len(pf.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return convertPrimary(pf.Target)
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
			return &CallExpr{Func: "print", Args: args}, nil
		case "len":
			return &CallExpr{Func: "Seq.length", Args: args}, nil
		case "str":
			return &CallExpr{Func: "string", Args: args}, nil
		case "sum":
			return &CallExpr{Func: "Seq.sum", Args: args}, nil
		case "avg":
			return &CallExpr{Func: "Seq.averageBy float", Args: args}, nil
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
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &IdentExpr{Name: p.Selector.Root}, nil
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
