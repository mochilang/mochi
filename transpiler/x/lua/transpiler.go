//go:build slow

package lua

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

// Program represents a simple Lua program consisting of a sequence of
// statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }
type AssignStmt struct {
	Name  string
	Value Expr
}
type FunStmt struct {
	Name   string
	Params []string
	Body   []Stmt
}
type ReturnStmt struct{ Value Expr }

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

type BreakStmt struct{}

type ContinueStmt struct{}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

type CallExpr struct {
	Func string
	Args []Expr
}

type StringLit struct{ Value string }
type IntLit struct{ Value int }
type BoolLit struct{ Value bool }
type Ident struct{ Name string }
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

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

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	if a.Value == nil {
		io.WriteString(w, "nil")
	} else {
		a.Value.emit(w)
	}
}

func (f *FunStmt) emit(w io.Writer) {
	io.WriteString(w, "function ")
	io.WriteString(w, f.Name)
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ")\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return")
	if r.Value != nil {
		io.WriteString(w, " ")
		r.Value.emit(w)
	}
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if ")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, " then\n")
	for _, st := range i.Then {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if len(i.Else) > 0 {
		io.WriteString(w, "else\n")
		for _, st := range i.Else {
			st.emit(w)
			io.WriteString(w, "\n")
		}
	}
	io.WriteString(w, "end")
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while ")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	io.WriteString(w, " do\n")
	for _, st := range wst.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "for ")
	io.WriteString(w, fr.Name)
	io.WriteString(w, " = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, ", ")
	if fr.End != nil {
		fr.End.emit(w)
		io.WriteString(w, " - 1")
	}
	io.WriteString(w, " do\n")
	for _, st := range fr.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}

func (fi *ForInStmt) emit(w io.Writer) {
	io.WriteString(w, "for _, ")
	io.WriteString(w, fi.Name)
	io.WriteString(w, " in pairs(")
	if fi.Iterable != nil {
		fi.Iterable.emit(w)
	}
	io.WriteString(w, ") do\n")
	for _, st := range fi.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break") }

func (c *ContinueStmt) emit(w io.Writer) { io.WriteString(w, "-- continue") }

func (ie *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "((")
	ie.Cond.emit(w)
	io.WriteString(w, ") and (")
	ie.Then.emit(w)
	io.WriteString(w, ") or (")
	ie.Else.emit(w)
	io.WriteString(w, "))")
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }
func (i *IntLit) emit(w io.Writer)    { fmt.Fprintf(w, "%d", i.Value) }
func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}
func (id *Ident) emit(w io.Writer) { io.WriteString(w, id.Name) }

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *BinaryExpr:
		if ex.Op == ".." || ex.Op == "+" {
			return isStringExpr(ex.Left) || isStringExpr(ex.Right)
		}
	}
	return false
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "in" {
		io.WriteString(w, "contains(")
		b.Right.emit(w)
		io.WriteString(w, ", ")
		b.Left.emit(w)
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	op := b.Op
	if op == "!=" {
		op = "~="
	}
	if op == "+" && (isStringExpr(b.Left) || isStringExpr(b.Right)) {
		op = ".."
	} else if op == "&&" {
		op = "and"
	} else if op == "||" {
		op = "or"
	} else if op == "/" {
		op = "//"
	}
	io.WriteString(w, op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
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
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("-- Generated by Mochi v%s on %s\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

func builtins() string {
	return `
function len(x)
  if type(x) == "string" or type(x) == "table" then
    return #x
  end
  return 0
end

function str(x)
  return tostring(x)
end

function substring(s, i, j)
  return string.sub(s, i + 1, j)
end

function contains(c, v)
  if type(c) == "string" then
    return string.find(c, v, 1, true) ~= nil
  elseif type(c) == "table" then
    for _, x in ipairs(c) do
      if x == v then return true end
    end
  end
  return false
end

function mochi_print(v)
  if type(v) == "boolean" then
    if v then
      print(1)
    else
      print(0)
    end
  else
    print(v)
  end
end
`
}

// Emit converts the AST back into Lua source code with a standard header.
func Emit(p *Program) []byte {
	var b bytes.Buffer
	b.WriteString(header())
	b.WriteString(builtins())
	for i, st := range p.Stmts {
		if i > 0 {
			b.WriteByte('\n')
		}
		st.emit(&b)
		b.WriteByte('\n')
	}
	return b.Bytes()
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
	exprs := []Expr{left}
	ops := []string{}
	for _, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, right)
		ops = append(ops, op.Op)
	}
	expr, err := buildPrecedence(exprs, ops)
	if err != nil {
		return nil, err
	}
	return expr, nil
}

func buildPrecedence(exprs []Expr, ops []string) (Expr, error) {
	// handle *, / first
	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 2
		case "+", "-":
			return 1
		default:
			return 0
		}
	}
	for {
		idx := -1
		for i, op := range ops {
			if prec(op) == 2 {
				idx = i
				break
			}
		}
		if idx == -1 {
			break
		}
		expr := &BinaryExpr{Left: exprs[idx], Op: ops[idx], Right: exprs[idx+1]}
		exprs = append(exprs[:idx], append([]Expr{expr}, exprs[idx+2:]...)...)
		ops = append(ops[:idx], ops[idx+1:]...)
	}
	for len(ops) > 0 {
		expr := &BinaryExpr{Left: exprs[0], Op: ops[0], Right: exprs[1]}
		exprs = append([]Expr{expr}, exprs[2:]...)
		ops = ops[1:]
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("invalid expression")
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
		if op != "-" {
			return nil, fmt.Errorf("unsupported unary operator")
		}
		expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
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
	case p.Call != nil:
		name := p.Call.Func
		if name == "print" {
			name = "mochi_print"
		}
		ce := &CallExpr{Func: name}
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ae)
		}
		return ce, nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return &Ident{Name: p.Selector.Root}, nil
		}
		return nil, fmt.Errorf("unsupported selector")
	case p.Group != nil:
		return convertExpr(p.Group)
	case p.If != nil:
		return convertIfExpr(p.If)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: int(*l.Int)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
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
		elseExpr = &Ident{Name: "nil"}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertIfStmt(is *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, st := range is.Then {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, s)
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		s, err := convertIfStmt(is.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(is.Else) > 0 {
		for _, st := range is.Else {
			s, err := convertStmt(st)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, s)
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertReturnStmt(rs *parser.ReturnStmt) (Stmt, error) {
	var expr Expr
	var err error
	if rs.Value != nil {
		expr, err = convertExpr(rs.Value)
		if err != nil {
			return nil, err
		}
	}
	return &ReturnStmt{Value: expr}, nil
}

func convertWhileStmt(ws *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range ws.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fs *parser.ForStmt) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, st := range fs.Body {
			s, err := convertStmt(st)
			if err != nil {
				return nil, err
			}
			body = append(body, s)
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fs.Source)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range fs.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &ForInStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertFunStmt(fs *parser.FunStmt) (Stmt, error) {
	f := &FunStmt{Name: fs.Name}
	for _, p := range fs.Params {
		f.Params = append(f.Params, p.Name)
	}
	for _, st := range fs.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		f.Body = append(f.Body, s)
	}
	return f, nil
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Expr != nil:
		expr, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: expr}, nil
	case st.Let != nil:
		var expr Expr
		var err error
		if st.Let.Value != nil {
			expr, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil && *st.Let.Type.Simple == "int" {
			expr = &IntLit{Value: 0}
		}
		return &AssignStmt{Name: st.Let.Name, Value: expr}, nil
	case st.Var != nil:
		var expr Expr
		var err error
		if st.Var.Value != nil {
			expr, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "int" {
			expr = &IntLit{Value: 0}
		}
		return &AssignStmt{Name: st.Var.Name, Value: expr}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("unsupported assignment")
		}
		expr, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Value: expr}, nil
	case st.Fun != nil:
		return convertFunStmt(st.Fun)
	case st.Return != nil:
		return convertReturnStmt(st.Return)
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

// Transpile converts a Mochi AST into a simple Lua AST supporting variable
// declarations, assignments, `if` statements and expressions, and calls like
// `print`. Expressions handle unary negation, arithmetic, comparison and basic
// boolean operators.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env
	lp := &Program{}
	for _, st := range prog.Statements {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		lp.Stmts = append(lp.Stmts, s)
	}
	return lp, nil
}

// Print renders a tree representation of the Lua AST to stdout. It is
// useful for debugging and tests.
func Print(p *Program) {
	toNode(p).Print("")
}

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
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *AssignStmt:
		var child *ast.Node
		if st.Value != nil {
			child = exprNode(st.Value)
		}
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{child}}
	case *FunStmt:
		n := &ast.Node{Kind: "fun", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params)
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ReturnStmt:
		var child *ast.Node
		if st.Value != nil {
			child = exprNode(st.Value)
		}
		return &ast.Node{Kind: "return", Children: []*ast.Node{child}}
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
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
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		n.Children = append(n.Children, exprNode(st.Cond))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		{
			n := &ast.Node{Kind: "for_range", Value: st.Name}
			n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
			body := &ast.Node{Kind: "body"}
			for _, s2 := range st.Body {
				body.Children = append(body.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, body)
			return n
		}
	case *ForInStmt:
		{
			n := &ast.Node{Kind: "for_in", Value: st.Name}
			n.Children = append(n.Children, exprNode(st.Iterable))
			body := &ast.Node{Kind: "body"}
			for _, s2 := range st.Body {
				body.Children = append(body.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, body)
			return n
		}
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	if e == nil {
		return &ast.Node{Kind: "nil"}
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
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprintf("%d", ex.Value)}
	case *Ident:
		return &ast.Node{Kind: "ident", Value: ex.Name}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *IfExpr:
		n := &ast.Node{Kind: "cond"}
		n.Children = append(n.Children, exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
