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

type Stmt interface{ emit(io.Writer) error }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) error { s.Expr.emit(w); return nil }

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) error {
	fmt.Fprintf(w, "$%s = ", s.Name)
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "null")
	}
	return nil
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) error {
	fmt.Fprintf(w, "$%s = ", s.Name)
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "null")
	}
	return nil
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) error {
	fmt.Fprintf(w, "$%s = ", s.Name)
	s.Value.emit(w)
	return nil
}

type PrintStmt struct{ Args []Expr }

func (s *PrintStmt) emit(w io.Writer) error {
	for i, a := range s.Args {
		fmt.Fprint(w, "$tmp = ")
		a.emit(w)
		if _, err := io.WriteString(w, ";\n"); err != nil {
			return err
		}
		fmt.Fprint(w, "if (is_array($tmp)) { echo implode(' ', array_map('strval', $tmp)); } else if (is_bool($tmp)) { echo $tmp ? '1' : '0'; } else { echo strval($tmp); }")
		if i < len(s.Args)-1 {
			fmt.Fprint(w, "; echo ' '")
		}
		if _, err := io.WriteString(w, "\n"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "echo PHP_EOL")
	return err
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

type Var struct{ Name string }

type IntLit struct{ Value int }

type BoolLit struct{ Value bool }

type StringLit struct{ Value string }

type UnaryExpr struct {
	Op string
	X  Expr
}

type ListLit struct{ Elems []Expr }

func (c *CallExpr) emit(w io.Writer) {
	switch c.Func {
	case "len":
		fmt.Fprint(w, "((is_array(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ")) ? count(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ") : strlen(strval(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ")))")
	case "append":
		fmt.Fprint(w, "array_merge(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ", [")
		c.Args[1].emit(w)
		fmt.Fprint(w, "])")
	case "count":
		fmt.Fprint(w, "count(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ")")
	case "sum":
		fmt.Fprint(w, "array_sum(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ")")
	case "avg":
		io.WriteString(w, "(count(")
		c.Args[0].emit(w)
		io.WriteString(w, ") ? sprintf(\"%.1f\", array_sum(")
		c.Args[0].emit(w)
		io.WriteString(w, ") / count(")
		c.Args[0].emit(w)
		io.WriteString(w, ")) : \"0.0\")")
	case "str":
		fmt.Fprint(w, "strval(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ")")
	default:
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
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		if _, ok := b.Left.(*StringLit); ok {
			b.Left.emit(w)
			fmt.Fprint(w, " . ")
			b.Right.emit(w)
			return
		}
		if _, ok := b.Right.(*StringLit); ok {
			b.Left.emit(w)
			fmt.Fprint(w, " . ")
			b.Right.emit(w)
			return
		}
	}
	if b.Op == "/" {
		fmt.Fprint(w, "intdiv(")
		b.Left.emit(w)
		fmt.Fprint(w, ", ")
		b.Right.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
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
		if err := s.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ";\n"); err != nil {
			return err
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
		if st.Expr != nil {
			e, err := convertExpr(st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			if c, ok := e.(*CallExpr); ok && c.Func == "print" {
				p.Stmts = append(p.Stmts, &PrintStmt{Args: c.Args})
			} else {
				p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
			}
		} else if st.Let != nil {
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
			p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Value: val})
		} else if st.Var != nil {
			var val Expr
			if st.Var.Value != nil {
				v, err := convertExpr(st.Var.Value)
				if err != nil {
					return nil, err
				}
				val = v
			}
			p.Stmts = append(p.Stmts, &VarStmt{Name: st.Var.Name, Value: val})
		} else if st.Assign != nil {
			if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
				return nil, fmt.Errorf("complex assignment not supported")
			}
			e, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Value: e})
		} else {
			return nil, fmt.Errorf("unsupported statement")
		}
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
	left, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	if len(b.Right) == 0 {
		return left, nil
	}
	if len(b.Right) != 1 {
		return nil, fmt.Errorf("binary op not supported")
	}
	right, err := convertPostfix(b.Right[0].Right)
	if err != nil {
		return nil, err
	}
	return &BinaryExpr{Left: left, Op: b.Right[0].Op, Right: right}, nil
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
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 {
			return nil, fmt.Errorf("selector tail not supported")
		}
		return &Var{Name: p.Selector.Root}, nil
	case p.Group != nil:
		return convertExpr(p.Group)
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
	case *PrintStmt:
		n := &ast.Node{Kind: "print"}
		for _, a := range st.Args {
			n.Children = append(n.Children, exprNode(a))
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
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the AST in Lisp-like form to stdout.
func Print(p *Program) { toNode(p).Print("") }
