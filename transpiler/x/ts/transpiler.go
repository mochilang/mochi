//go:build slow

package tstranspiler

import (
	"bytes"
	"fmt"
	"io"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"

	meta "mochi/transpiler/meta"
)

// Simple TypeScript AST nodes used by the transpiler.

type Program struct {
	Stmts []Stmt
}

type Stmt interface {
	emit(io.Writer)
}

type Expr interface {
	emit(io.Writer)
}

type ExprStmt struct {
	Expr Expr
}

// VarDecl represents a variable declaration like `let x = expr`.
type VarDecl struct {
	Name string
	Expr Expr
}

// AssignStmt represents an assignment like `x = expr`.
type AssignStmt struct {
	Name string
	Expr Expr
}

type CallExpr struct {
	Func string
	Args []Expr
}

type StringLit struct {
	Value string
}

// NumberLit is a numeric literal.
type NumberLit struct {
	Value string
}

// BoolLit is a boolean literal.
type BoolLit struct {
	Value bool
}

// NameRef refers to a variable.
type NameRef struct {
	Name string
}

// BinaryExpr represents a binary operation such as 1 + 2.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// UnaryExpr represents a prefix unary operation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (s *ExprStmt) emit(w io.Writer) {
	if s == nil {
		return
	}
	s.Expr.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte('(')
	} else {
		io.WriteString(w, "(")
	}
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(')')
	} else {
		io.WriteString(w, ")")
	}
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }

func (b *BinaryExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

func (v *VarDecl) emit(w io.Writer) {
	io.WriteString(w, "let ")
	io.WriteString(w, v.Name)
	if v.Expr != nil {
		io.WriteString(w, " = ")
		v.Expr.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	a.Expr.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

// Emit converts the AST back into TypeScript source code.
func Emit(p *Program) []byte {
	var b bytes.Buffer
	b.Write(meta.Header("//"))
	for i, s := range p.Stmts {
		if i > 0 {
			b.WriteByte('\n')
		}
		s.emit(&b)
		b.WriteByte('\n')
	}
	return b.Bytes()
}

// Transpile converts a Mochi program into a TypeScript AST. Only a very
// small subset of the language is supported: programs consisting of a single
// call to the builtin `print` with a string literal argument.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env // env not used by this minimal implementation
	tsProg := &Program{}

	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			e, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			tsProg.Stmts = append(tsProg.Stmts, &VarDecl{Name: st.Let.Name, Expr: e})
		case st.Var != nil:
			e, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			tsProg.Stmts = append(tsProg.Stmts, &VarDecl{Name: st.Var.Name, Expr: e})
		case st.Assign != nil:
			e, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			tsProg.Stmts = append(tsProg.Stmts, &AssignStmt{Name: st.Assign.Name, Expr: e})
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil || call.Func != "print" {
				return nil, fmt.Errorf("unsupported expression")
			}
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("print expects one argument")
			}
			arg, err := convertExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			tsProg.Stmts = append(tsProg.Stmts, &ExprStmt{Expr: &CallExpr{Func: "console.log", Args: []Expr{arg}}})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return tsProg, nil
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
	expr := left
	for _, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		expr = &BinaryExpr{Left: expr, Op: op.Op, Right: right}
	}
	return expr, nil
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

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if len(p.Ops) > 0 {
		return nil, fmt.Errorf("postfix ops not supported")
	}
	return convertPrimary(p.Target)
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
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		return &NumberLit{Value: fmt.Sprintf("%g", *l.Float)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

// print converts the given TypeScript AST into a generic ast.Node tree and
// writes it to w. It is useful for debugging and tests.
func print(p *Program, w io.Writer) {
	if p == nil {
		return
	}
	node := progToNode(p)
	io.WriteString(w, node.String())
}

func progToNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtToNode(st))
	}
	return n
}

func stmtToNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprToNode(st.Expr)}}
	case *VarDecl:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprToNode(st.Expr)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprToNode(st.Expr)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprToNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *NumberLit:
		return &ast.Node{Kind: "number", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprToNode(ex.Left), exprToNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprToNode(ex.Expr)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
