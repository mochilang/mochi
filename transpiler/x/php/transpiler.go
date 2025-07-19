//go:build slow

package php

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// --- Simple PHP AST ---

type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

// IfStmt represents a conditional statement with optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if (")
	s.Cond.emit(w)
	io.WriteString(w, ") {\n")
	for _, st := range s.Then {
		st.emit(w)
		io.WriteString(w, ";\n")
	}
	io.WriteString(w, "}")
	if len(s.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range s.Else {
			st.emit(w)
			io.WriteString(w, ";\n")
		}
		io.WriteString(w, "}")
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "$%s = ", s.Name)
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "null")
	}
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "$%s = ", s.Name)
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "null")
	}
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "$%s = ", s.Name)
	s.Value.emit(w)
}

type Expr interface{ emit(io.Writer) }

type CallExpr struct {
	Func string
	Args []Expr
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// GroupExpr represents a parenthesized sub-expression.
type GroupExpr struct{ X Expr }

// IntDivExpr represents integer division of Left / Right.
type IntDivExpr struct {
	Left  Expr
	Right Expr
}

type Var struct{ Name string }

type IntLit struct{ Value int }

type BoolLit struct{ Value bool }

type StringLit struct{ Value string }

type UnaryExpr struct {
	Op string
	X  Expr
}

type ListLit struct{ Elems []Expr }

// SubstringExpr represents substring(str, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

// CondExpr represents a conditional expression using PHP's ternary operator.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	c.Cond.emit(w)
	io.WriteString(w, " ? ")
	c.Then.emit(w)
	io.WriteString(w, " : ")
	c.Else.emit(w)
	io.WriteString(w, ")")
}

func (c *CallExpr) emit(w io.Writer) {
	if c.Func == "echo" {
		fmt.Fprint(w, "echo ")
		for i, a := range c.Args {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			a.emit(w)
		}
		fmt.Fprint(w, ", PHP_EOL")
		return
	}
	fmt.Fprint(w, c.Func)
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		_, leftStr := b.Left.(*StringLit)
		_, rightStr := b.Right.(*StringLit)
		if leftStr || rightStr {
			b.Left.emit(w)
			fmt.Fprint(w, " . ")
			b.Right.emit(w)
		} else {
			b.Left.emit(w)
			fmt.Fprint(w, " + ")
			b.Right.emit(w)
		}
		return
	}
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
}

func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.X.emit(w)
	io.WriteString(w, ")")
}

func (d *IntDivExpr) emit(w io.Writer) {
	io.WriteString(w, "intdiv(")
	d.Left.emit(w)
	io.WriteString(w, ", ")
	d.Right.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	u.X.emit(w)
}

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "]")
}

func (s *SubstringExpr) emit(w io.Writer) {
	fmt.Fprint(w, "substr(")
	s.Str.emit(w)
	fmt.Fprint(w, ", ")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
	fmt.Fprint(w, ")")
}

func (v *Var) emit(w io.Writer) { fmt.Fprintf(w, "$%s", v.Name) }

func (i *IntLit) emit(w io.Writer) { fmt.Fprint(w, i.Value) }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		fmt.Fprint(w, "true")
	} else {
		fmt.Fprint(w, "false")
	}
}

// Emit writes formatted PHP source to w.
func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, "<?php\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		s.emit(w)
		if _, ok := s.(*IfStmt); ok {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(w, "?>")
	return err
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

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("// Generated by Mochi transpiler v%s on %s\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

// Transpile converts a Mochi program into our PHP AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	p := &Program{}
	for _, st := range prog.Statements {
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		p.Stmts = append(p.Stmts, conv)
	}
	_ = env
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
	operands := []Expr{}
	ops := []string{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, p := range b.Right {
		r, err := convertPostfix(p.Right)
		if err != nil {
			return nil, err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, r)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	apply := func(left Expr, op string, right Expr) Expr {
		if op == "/" {
			return &IntDivExpr{Left: left, Right: right}
		}
		return &BinaryExpr{Left: left, Op: op, Right: right}
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					expr := apply(operands[i], ops[i], operands[i+1])
					operands[i] = expr
					operands = append(operands[:i+1], operands[i+2:]...)
					ops = append(ops[:i], ops[i+1:]...)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	x, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op != "-" {
			return nil, fmt.Errorf("unary op %s not supported", op)
		}
		x = &UnaryExpr{Op: "-", X: x}
	}
	return x, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if len(pf.Ops) > 0 {
		return nil, fmt.Errorf("postfix ops not supported")
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
		name := p.Call.Func
		if name == "print" {
			name = "echo"
		} else if name == "len" {
			if len(args) == 1 {
				if _, ok := args[0].(*ListLit); ok {
					name = "count"
				} else {
					name = "strlen"
				}
			}
		} else if name == "substring" {
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		} else if name == "sum" {
			if len(args) != 1 {
				return nil, fmt.Errorf("sum expects 1 arg")
			}
			return &CallExpr{Func: "array_sum", Args: args}, nil
		} else if name == "avg" {
			if len(args) != 1 {
				return nil, fmt.Errorf("avg expects 1 arg")
			}
			frac := &BinaryExpr{Left: &CallExpr{Func: "array_sum", Args: args}, Op: "/", Right: &CallExpr{Func: "count", Args: args}}
			nfArgs := []Expr{frac, &IntLit{Value: 1}, &StringLit{Value: "."}, &StringLit{Value: ""}}
			return &CallExpr{Func: "number_format", Args: nfArgs}, nil
		} else if name == "str" {
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects 1 arg")
			}
			return &CallExpr{Func: "strval", Args: args}, nil
		} else if name == "min" || name == "max" {
			if len(args) != 1 {
				return nil, fmt.Errorf("%s expects 1 arg", name)
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil:
		return convertLiteral(p.Lit)
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
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 {
			return nil, fmt.Errorf("selector tail not supported")
		}
		return &Var{Name: p.Selector.Root}, nil
	case p.Group != nil:
		ex, err := convertExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{X: ex}, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Int != nil:
		return &IntLit{Value: int(*l.Int)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	}
	return nil, fmt.Errorf("unsupported literal")
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
		var val Expr
		if st.Let.Value != nil {
			v, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			val = v
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil {
			switch *st.Let.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "bool":
				val = &BoolLit{Value: false}
			case "string":
				val = &StringLit{Value: ""}
			}
		}
		return &LetStmt{Name: st.Let.Name, Value: val}, nil
	case st.Var != nil:
		var val Expr
		if st.Var.Value != nil {
			v, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			val = v
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil {
			switch *st.Var.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "bool":
				val = &BoolLit{Value: false}
			case "string":
				val = &StringLit{Value: ""}
			}
		}
		return &VarStmt{Name: st.Var.Name, Value: val}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("complex assignment not supported")
		}
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Value: e}, nil
	case st.If != nil:
		return convertIfStmt(st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIfStmt(ifst *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(ifst.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts := make([]Stmt, len(ifst.Then))
	for i, s := range ifst.Then {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		thenStmts[i] = st
	}
	var elseStmts []Stmt
	if ifst.ElseIf != nil {
		st, err := convertIfStmt(ifst.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if len(ifst.Else) > 0 {
		elseStmts = make([]Stmt, len(ifst.Else))
		for i, s := range ifst.Else {
			st, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			elseStmts[i] = st
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &BoolLit{Value: false}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

// Convert the PHP AST to a generic ast.Node for debugging.
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let_stmt", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		child := &ast.Node{Kind: "null"}
		if st.Value != nil {
			child = exprNode(st.Value)
		}
		return &ast.Node{Kind: "var_stmt", Value: st.Name, Children: []*ast.Node{child}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign_stmt", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if_stmt", Children: []*ast.Node{exprNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, s2 := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, s2 := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	if e == nil {
		return &ast.Node{Kind: "null"}
	}
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.X)}}
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	case *Var:
		return &ast.Node{Kind: "var", Value: ex.Name}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: ex.Value}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.X)}}
	case *IntDivExpr:
		return &ast.Node{Kind: "intdiv", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the AST in Lisp-like form to stdout.
func Print(p *Program) { toNode(p).Print("") }
