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

// IfStmt represents a conditional statement with an optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents a numeric range for-loop like
// `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForInStmt represents iteration over an iterable expression.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
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

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range i.Else {
			st.emit(w)
			io.WriteString(w, "\n")
		}
		io.WriteString(w, "}")
	}
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while (")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (f *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "for (let ")
	io.WriteString(w, f.Name)
	io.WriteString(w, " = ")
	if f.Start != nil {
		f.Start.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Name)
	io.WriteString(w, " < ")
	if f.End != nil {
		f.End.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Name)
	io.WriteString(w, "++) {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (f *ForInStmt) emit(w io.Writer) {
	io.WriteString(w, "for (const ")
	io.WriteString(w, f.Name)
	io.WriteString(w, " of ")
	if f.Iterable != nil {
		f.Iterable.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
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
		stmt, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		tsProg.Stmts = append(tsProg.Stmts, stmt)
	}
	return tsProg, nil
}

func convertStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Let != nil:
		e, err := convertExpr(s.Let.Value)
		if err != nil {
			return nil, err
		}
		return &VarDecl{Name: s.Let.Name, Expr: e}, nil
	case s.Var != nil:
		e, err := convertExpr(s.Var.Value)
		if err != nil {
			return nil, err
		}
		return &VarDecl{Name: s.Var.Name, Expr: e}, nil
	case s.Assign != nil:
		e, err := convertExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: s.Assign.Name, Expr: e}, nil
	case s.Expr != nil:
		call := s.Expr.Expr.Binary.Left.Value.Target.Call
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
		return &ExprStmt{Expr: &CallExpr{Func: "console.log", Args: []Expr{arg}}}, nil
	case s.If != nil:
		return convertIfStmt(s.If)
	case s.While != nil:
		return convertWhileStmt(s.While)
	case s.For != nil:
		return convertForStmt(s.For)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
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

func convertWhileStmt(w *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(w.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(w.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(f *parser.ForStmt) (Stmt, error) {
	if f.RangeEnd != nil {
		start, err := convertExpr(f.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(f.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmtList(f.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: f.Name, Start: start, End: end, Body: body}, nil
	}
	iterable, err := convertExpr(f.Source)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	return &ForInStmt{Name: f.Name, Iterable: iterable, Body: body}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		out = append(out, st)
	}
	return out, nil
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
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprToNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, c := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, c := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtToNode(c))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprToNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for-range", Value: st.Name, Children: []*ast.Node{exprToNode(st.Start), exprToNode(st.End)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForInStmt:
		n := &ast.Node{Kind: "for-in", Value: st.Name, Children: []*ast.Node{exprToNode(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
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
