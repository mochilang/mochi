//go:build slow

package dartt

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

// --- Simple Dart AST ---

// Program represents a sequence of statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) error }

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (w *WhileStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "while ("); err != nil {
		return err
	}
	if err := w.Cond.emit(out); err != nil {
		return err
	}
	if _, err := io.WriteString(out, ") {\n"); err != nil {
		return err
	}
	for _, st := range w.Body {
		if _, err := io.WriteString(out, "  "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		if _, ok := st.(*IfStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*WhileStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(out, "}")
	return err
}

// IfStmt represents a conditional statement with an optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "if ("); err != nil {
		return err
	}
	if err := s.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range s.Then {
		if _, err := io.WriteString(w, "    "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ";\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  }"); err != nil {
		return err
	}
	if len(s.Else) > 0 {
		if _, err := io.WriteString(w, " else {\n"); err != nil {
			return err
		}
		for _, st := range s.Else {
			if _, err := io.WriteString(w, "    "); err != nil {
				return err
			}
			if err := st.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "  }"); err != nil {
			return err
		}
	}
	return nil
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "var "+s.Name); err != nil {
		return err
	}
	if s.Value != nil {
		if _, err := io.WriteString(w, " = "); err != nil {
			return err
		}
		return s.Value.emit(w)
	}
	return nil
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, s.Name+" = "); err != nil {
		return err
	}
	return s.Value.emit(w)
}

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "var "+s.Name+" = "); err != nil {
		return err
	}
	return s.Value.emit(w)
}

// ReturnStmt represents a `return` statement.
type ReturnStmt struct {
	Value Expr
}

func (s *ReturnStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "return"); err != nil {
		return err
	}
	if s.Value != nil {
		if _, err := io.WriteString(w, " "); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
	}
	return nil
}

// FuncDecl represents a function definition.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FuncDecl) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "dynamic "+f.Name+"("); err != nil {
		return err
	}
	for i, p := range f.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *FuncDecl:
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) error { return s.Expr.emit(w) }

type Expr interface{ emit(io.Writer) error }

type UnaryExpr struct {
	Op string
	X  Expr
}

func (u *UnaryExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, u.Op); err != nil {
		return err
	}
	return u.X.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := b.Left.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " "+b.Op+" "); err != nil {
		return err
	}
	if err := b.Right.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// CondExpr represents a conditional expression like `cond ? a : b`.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) error {
	if err := c.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " ? "); err != nil {
		return err
	}
	if err := c.Then.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " : "); err != nil {
		return err
	}
	return c.Else.emit(w)
}

type CallExpr struct {
	Func Expr
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) error {
	if err := c.Func.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, a := range c.Args {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := a.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) error { _, err := io.WriteString(w, n.Name); return err }

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) error { _, err := fmt.Fprintf(w, "%q", s.Value); return err }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) error { _, err := fmt.Fprintf(w, "%d", i.Value); return err }

// ListLit represents a list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	for i, e := range l.Elems {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := e.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

// LambdaExpr represents an inline function expression.
type LambdaExpr struct {
	Params []string
	Body   Expr
}

func (l *LambdaExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, p := range l.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") => "); err != nil {
		return err
	}
	if err := l.Body.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// LenExpr represents the `len` builtin.
type LenExpr struct{ X Expr }

func (l *LenExpr) emit(w io.Writer) error {
	if err := l.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length")
	return err
}

func emitExpr(w io.Writer, e Expr) error { return e.emit(w) }

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

// Emit writes Dart source for p to w.
func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for _, st := range p.Stmts {
		if fd, ok := st.(*FuncDecl); ok {
			if err := fd.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "void main() {\n"); err != nil {
		return err
	}
	for _, st := range p.Stmts {
		if _, ok := st.(*FuncDecl); ok {
			continue
		}
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if _, ok := st.(*IfStmt); ok {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*WhileStmt); ok {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(w, "}\n")
	return err
}

// Transpile converts a Mochi program into a simple Dart AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	p := &Program{}
	for _, st := range prog.Statements {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		p.Stmts = append(p.Stmts, s)
	}
	_ = env
	return p, nil
}

func convertIfStmt(i *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(i.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		elseStmts, err = convertStmtList(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(wst *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(wst.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(wst.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		out = append(out, s)
	}
	return out, nil
}

func convertStmtInternal(st *parser.Statement) (Stmt, error) {
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
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil && *st.Let.Type.Simple == "int" {
			e = &IntLit{Value: 0}
		} else {
			return nil, fmt.Errorf("let missing value not supported")
		}
		return &LetStmt{Name: st.Let.Name, Value: e}, nil
	case st.Var != nil:
		var e Expr
		if st.Var.Value != nil {
			var err error
			e, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "int" {
			e = &IntLit{Value: 0}
		}
		return &VarStmt{Name: st.Var.Name, Value: e}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("complex assignment not supported")
		}
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Value: e}, nil
	case st.Return != nil:
		var e Expr
		if st.Return.Value != nil {
			var err error
			e, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: e}, nil
	case st.Fun != nil:
		body, err := convertStmtList(st.Fun.Body)
		if err != nil {
			return nil, err
		}
		var params []string
		for _, p := range st.Fun.Params {
			params = append(params, p.Name)
		}
		return &FuncDecl{Name: st.Fun.Name, Params: params, Body: body}, nil
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.If != nil:
		return convertIfStmt(st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
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
	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]string, len(b.Right))
	for i, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		ops[i] = op.Op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				expr := &BinaryExpr{Left: left, Op: ops[i], Right: right}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	ex, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op != "-" {
			return nil, fmt.Errorf("unary op %s not supported", op)
		}
		ex = &UnaryExpr{Op: op, X: ex}
	}
	return ex, nil
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
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &LenExpr{X: arg}, nil
		}
		ce := &CallExpr{Func: &Name{p.Call.Func}}
		for _, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			if p.Call.Func == "print" && len(p.Call.Args) == 1 && isBoolExpr(a) {
				ex = &CondExpr{Cond: ex, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
			}
			ce.Args = append(ce.Args, ex)
		}
		return ce, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.List != nil:
		var elems []Expr
		for _, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, nil
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Body: body}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &Name{Name: p.Selector.Root}, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported expression")
}

func isBoolExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) == 0 {
		return false
	}
	for _, op := range e.Binary.Right {
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=":
			return true
		}
	}
	return false
}

// --- AST -> generic node (for debugging) ---
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtNode(st))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		node := &ast.Node{Kind: "var", Value: st.Name}
		if st.Value != nil {
			node.Children = []*ast.Node{exprNode(st.Value)}
		}
		return node
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = []*ast.Node{exprNode(st.Value)}
		}
		return n
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, c := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(c))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, c := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(c))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(ex.Func))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.X)}}
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Body))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the AST in Lisp-like form to stdout.
func Print(p *Program) { toNode(p).Print("") }
