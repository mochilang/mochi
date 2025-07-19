package rs

import (
	"bytes"
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

// Program represents a Rust program consisting of a list of statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

// Expr represents an expression node.
type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement consisting of a single expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type NumberLit struct{ Value string }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

type NameRef struct{ Name string }

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }

type VarDecl struct {
	Name    string
	Expr    Expr
	Type    string
	Mutable bool
}

func (v *VarDecl) emit(w io.Writer) {
	io.WriteString(w, "let ")
	if v.Mutable {
		io.WriteString(w, "mut ")
	}
	io.WriteString(w, v.Name)
	if v.Type != "" {
		io.WriteString(w, ": ")
		io.WriteString(w, v.Type)
	}
	if v.Expr != nil {
		io.WriteString(w, " = ")
		v.Expr.emit(w)
	} else if v.Type != "" {
		io.WriteString(w, " = ")
		io.WriteString(w, defaultValueForType(v.Type))
	}
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	a.Expr.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

// --- Transpiler ---

// Transpile converts a Mochi AST to a simplified Rust AST. Only a very small
// subset of Mochi is supported which is sufficient for tests.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	prog := &Program{}
	for _, stmt := range p.Statements {
		switch {
		case stmt.Expr != nil:
			e, err := compileExpr(stmt.Expr.Expr)
			if err != nil {
				return nil, err
			}
			prog.Stmts = append(prog.Stmts, &ExprStmt{Expr: e})
		case stmt.Let != nil:
			var e Expr
			var err error
			if stmt.Let.Value != nil {
				e, err = compileExpr(stmt.Let.Value)
				if err != nil {
					return nil, err
				}
			}
			typ := ""
			if stmt.Let.Type != nil && stmt.Let.Type.Simple != nil {
				typ = rustType(*stmt.Let.Type.Simple)
			}
			prog.Stmts = append(prog.Stmts, &VarDecl{Name: stmt.Let.Name, Expr: e, Type: typ})
		case stmt.Var != nil:
			var e Expr
			var err error
			if stmt.Var.Value != nil {
				e, err = compileExpr(stmt.Var.Value)
				if err != nil {
					return nil, err
				}
			}
			typ := ""
			if stmt.Var.Type != nil && stmt.Var.Type.Simple != nil {
				typ = rustType(*stmt.Var.Type.Simple)
			}
			prog.Stmts = append(prog.Stmts, &VarDecl{Name: stmt.Var.Name, Expr: e, Type: typ, Mutable: true})
		case stmt.Assign != nil:
			e, err := compileExpr(stmt.Assign.Value)
			if err != nil {
				return nil, err
			}
			prog.Stmts = append(prog.Stmts, &AssignStmt{Name: stmt.Assign.Name, Expr: e})
		case stmt.Test == nil && stmt.Import == nil && stmt.Type == nil:
			return nil, fmt.Errorf("unsupported statement at %d:%d", stmt.Pos.Line, stmt.Pos.Column)
		}
	}
	_ = env // reserved for future use
	return prog, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	expr := left
	for _, op := range e.Binary.Right {
		right, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		expr = &BinaryExpr{Left: expr, Op: op.Op, Right: right}
	}
	return expr, nil
}

func compileUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := compilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
	}
	return expr, nil
}

func compilePostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil || len(p.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return compilePrimary(p.Target)
}

func compilePrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		if name == "print" {
			name = "println!"
			if len(args) == 1 {
				if _, ok := args[0].(*StringLit); !ok {
					args = append([]Expr{&StringLit{Value: "{}"}}, args...)
				}
			}
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil:
		return compileLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &NameRef{Name: p.Selector.Root}, nil
	case p.Group != nil:
		return compileExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Int != nil:
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		return &NumberLit{Value: fmt.Sprintf("%g", *l.Float)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func rustType(t string) string {
	switch t {
	case "int":
		return "i64"
	case "float":
		return "f64"
	case "bool":
		return "bool"
	case "string":
		return "String"
	}
	return "i64"
}

func defaultValueForType(t string) string {
	switch t {
	case "i64":
		return "0"
	case "f64":
		return "0.0"
	case "bool":
		return "false"
	case "String":
		return "String::new()"
	}
	return "Default::default()"
}

// Emit generates formatted Rust source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	buf.WriteString("fn main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteString(";\n")
	}
	buf.WriteString("}\n")
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		b = append(b, '\n')
	}
	return buf.Bytes()
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
		return "dev"
	}
	if b, err := os.ReadFile(filepath.Join(root, "VERSION")); err == nil {
		return strings.TrimSpace(string(b))
	}
	return "dev"
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04:05 MST")
	return fmt.Sprintf("// Generated by Mochi transpiler v%s on %s\n", version(), ts)
}

// Print converts prog to ast.Node form and prints it.
func Print(prog *Program) {
	toNode(prog).Print("")
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
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *VarDecl:
		n := &ast.Node{Kind: "let", Value: st.Name}
		if st.Expr != nil {
			n.Children = []*ast.Node{exprNode(st.Expr)}
		}
		return n
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *NumberLit:
		return &ast.Node{Kind: "number", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
