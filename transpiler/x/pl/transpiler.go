//go:build slow

package pl

import (
	"fmt"
	"io"
	"strconv"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Program represents a simple Prolog program.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer, int) }

type PrintStmt struct{ Expr Expr }
type LetStmt struct {
	Name string
	Expr Expr
}

type compileEnv struct{ vars map[string]int }

func newCompileEnv() *compileEnv { return &compileEnv{vars: make(map[string]int)} }

func (e *compileEnv) fresh(name string) string {
	v, ok := e.vars[name]
	if !ok {
		v = -1
	}
	v++
	e.vars[name] = v
	return varName(name, v)
}

func (e *compileEnv) current(name string) string {
	v, ok := e.vars[name]
	if !ok {
		return varName(name, 0)
	}
	return varName(name, v)
}

func varName(name string, v int) string {
	if v == 0 {
		return cap(name)
	}
	return fmt.Sprintf("%s%d", cap(name), v)
}

func (p *PrintStmt) emit(w io.Writer, idx int) {
	if be, ok := p.Expr.(*BinaryExpr); ok {
		if isBoolOp(be.Op) {
			io.WriteString(w, "    (")
			be.emit(w)
			io.WriteString(w, " -> write(true) ; write(false)), nl")
			return
		}
		if isArithOp(be.Op) {
			fmt.Fprintf(w, "    R%d is ", idx)
			be.emit(w)
			fmt.Fprintf(w, ", write(R%d), nl", idx)
			return
		}
	}
	io.WriteString(w, "    write(")
	p.Expr.emit(w)
	io.WriteString(w, "), nl")
}

func (l *LetStmt) emit(w io.Writer, _ int) {
	fmt.Fprintf(w, "    %s is ", l.Name)
	l.Expr.emit(w)
}

type Expr interface{ emit(io.Writer) }
type IntLit struct{ Value int }
type BoolLit struct{ Value bool }
type StringLit struct{ Value string }
type Var struct{ Name string }
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}
type GroupExpr struct{ Expr Expr }
type CastExpr struct {
	Expr Expr
	Type string
}

func (i *IntLit) emit(w io.Writer)    { fmt.Fprintf(w, "%d", i.Value) }
func (b *BoolLit) emit(w io.Writer)   { fmt.Fprintf(w, "%v", b.Value) }
func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "'%s'", escape(s.Value)) }
func (v *Var) emit(w io.Writer)       { io.WriteString(w, v.Name) }
func (b *BinaryExpr) emit(w io.Writer) {
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
}
func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.Expr.emit(w)
	io.WriteString(w, ")")
}
func (c *CastExpr) emit(w io.Writer) {
	if c.Type == "int" {
		if s, ok := c.Expr.(*StringLit); ok {
			n, err := strconv.Atoi(s.Value)
			if err == nil {
				fmt.Fprintf(w, "%d", n)
				return
			}
		}
	}
	c.Expr.emit(w)
}

func escape(s string) string {
	s = strings.ReplaceAll(s, "'", "''")
	return s
}

func cap(name string) string {
	if name == "" {
		return ""
	}
	return strings.ToUpper(name[:1]) + name[1:]
}

func isBoolOp(op string) bool {
	switch op {
	case "=:=", "=\\=", "<", "<=", ">", ">=":
		return true
	}
	return false
}

func isArithOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "mod":
		return true
	}
	return false
}

// Transpile converts a Mochi program to a Prolog AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env
	ce := newCompileEnv()
	p := &Program{}
	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			var expr Expr
			if st.Let.Value != nil {
				var err error
				expr, err = toExpr(st.Let.Value, ce)
				if err != nil {
					return nil, err
				}
			} else {
				expr = &IntLit{Value: 0}
			}
			name := ce.fresh(st.Let.Name)
			p.Stmts = append(p.Stmts, &LetStmt{Name: name, Expr: expr})
		case st.Var != nil:
			var expr Expr
			if st.Var.Value != nil {
				var err error
				expr, err = toExpr(st.Var.Value, ce)
				if err != nil {
					return nil, err
				}
			} else {
				expr = &IntLit{Value: 0}
			}
			name := ce.fresh(st.Var.Name)
			p.Stmts = append(p.Stmts, &LetStmt{Name: name, Expr: expr})
		case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
			expr, err := toExpr(st.Assign.Value, ce)
			if err != nil {
				return nil, err
			}
			name := ce.fresh(st.Assign.Name)
			p.Stmts = append(p.Stmts, &LetStmt{Name: name, Expr: expr})
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil || call.Func != "print" || len(call.Args) != 1 {
				return nil, fmt.Errorf("unsupported expression")
			}
			arg, err := toExpr(call.Args[0], ce)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &PrintStmt{Expr: arg})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return p, nil
}

// Emit writes the Prolog source for the given program.
func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	io.WriteString(w, ":- initialization(main).\n\n")
	io.WriteString(w, "main :-\n")
	for i, st := range p.Stmts {
		st.emit(w, i)
		if i < len(p.Stmts)-1 {
			io.WriteString(w, ",\n")
		} else {
			io.WriteString(w, ".\n")
		}
	}
	return nil
}

func header() string {
	t := time.Now().UTC().Format("2006-01-02 15:04:05 MST")
	return fmt.Sprintf("%s %s\n:- style_check(-singleton).\n", "% Generated by Mochi transpiler", t)
}

// Print converts the Program to ast.Node and prints it.
func Print(p *Program) {
	toNode(p).Print("")
}

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *PrintStmt:
		return &ast.Node{Kind: "print", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func toExpr(e *parser.Expr, env *compileEnv) (Expr, error) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return toBinary(e.Binary, env)
}

func toBinary(b *parser.BinaryExpr, env *compileEnv) (Expr, error) {
	left, err := toUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	for _, r := range b.Right {
		right, err := toPostfix(r.Right, env)
		if err != nil {
			return nil, err
		}
		op := r.Op
		var opStr string
		switch op {
		case "+", "-", "*", "/":
			opStr = op
		case "%":
			opStr = "mod"
		case "==":
			opStr = "=:="
		case "!=":
			opStr = "=\\="
		case "<", "<=", ">", ">=":
			opStr = op
		default:
			return nil, fmt.Errorf("unsupported op")
		}
		left = &BinaryExpr{Left: left, Op: opStr, Right: right}
	}
	return left, nil
}

func toUnary(u *parser.Unary, env *compileEnv) (Expr, error) {
	expr, err := toPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			if lit, ok := expr.(*IntLit); ok {
				expr = &IntLit{Value: -lit.Value}
			} else {
				expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
			}
		default:
			return nil, fmt.Errorf("unsupported unary")
		}
	}
	return expr, nil
}

func toPostfix(pf *parser.PostfixExpr, env *compileEnv) (Expr, error) {
	expr, err := toPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		switch {
		case op.Cast != nil:
			if op.Cast.Type == nil || op.Cast.Type.Simple == nil {
				return nil, fmt.Errorf("unsupported cast")
			}
			expr = &CastExpr{Expr: expr, Type: *op.Cast.Type.Simple}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func toPrimary(p *parser.Primary, env *compileEnv) (Expr, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Bool != nil {
			return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
		}
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 {
			return nil, fmt.Errorf("unsupported selector")
		}
		return &Var{Name: env.current(p.Selector.Root)}, nil
	case p.Group != nil:
		expr, err := toExpr(p.Group, env)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: expr}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: ex.Value}
	case *StringLit:
		return &ast.Node{Kind: "str", Value: ex.Value}
	case *Var:
		return &ast.Node{Kind: "var", Value: ex.Name}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Expr)}}
	default:
		return &ast.Node{Kind: "expr"}
	}
}
