//go:build slow

package ex

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

// Program represents a sequence of Elixir statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer, int) }

// VarRef references a variable name or dotted selector.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

// LetStmt binds a variable optionally to a value.
type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, s.Name)
	io.WriteString(w, " = ")
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
}

// AssignStmt reassigns a variable.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, s.Name)
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement consisting solely of an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	s.Expr.emit(w)
}

// ReturnStmt returns from a function optionally with a value.
type ReturnStmt struct{ Value Expr }

func (r *ReturnStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	if r.Value != nil {
		r.Value.emit(w)
	}
}

// IfStmt is a simple if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "if ")
	s.Cond.emit(w)
	io.WriteString(w, " do\n")
	for _, st := range s.Then {
		st.emit(w, indent+1)
		io.WriteString(w, "\n")
	}
	if len(s.Else) > 0 {
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "else\n")
		for _, st := range s.Else {
			st.emit(w, indent+1)
			io.WriteString(w, "\n")
		}
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end")
}

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "while ")
	wst.Cond.emit(w)
	io.WriteString(w, " do\n")
	for _, st := range wst.Body {
		st.emit(w, indent+1)
		io.WriteString(w, "\n")
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end")
}

// ForStmt represents a basic for loop over a collection or range.
type ForStmt struct {
	Name   string
	Start  Expr
	End    Expr // optional, when non-nil compile as range
	Source Expr // used when End is nil
	Body   []Stmt
}

func (fs *ForStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "for ")
	io.WriteString(w, fs.Name)
	io.WriteString(w, " <- ")
	if fs.End != nil {
		io.WriteString(w, "(")
		fs.Start.emit(w)
		io.WriteString(w, "..(")
		fs.End.emit(w)
		io.WriteString(w, " - 1)")
		io.WriteString(w, ")")
	} else {
		fs.Source.emit(w)
	}
	io.WriteString(w, " do\n")
	for _, st := range fs.Body {
		st.emit(w, indent+1)
		io.WriteString(w, "\n")
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end")
}

// FuncDecl defines a simple function.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (fn *FuncDecl) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "def ")
	io.WriteString(w, fn.Name)
	io.WriteString(w, "(")
	for i, p := range fn.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") do\n")
	for _, st := range fn.Body {
		st.emit(w, indent+1)
		io.WriteString(w, "\n")
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end")
}

// CondExpr represents a conditional expression.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	c.Cond.emit(w)
	io.WriteString(w, ", do: ")
	c.Then.emit(w)
	if c.Else != nil {
		io.WriteString(w, ", else: ")
		c.Else.emit(w)
	}
}

// BinaryExpr represents a binary operation such as 1 + 2.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
	MapIn bool
}

func (b *BinaryExpr) emit(w io.Writer) {
	isInt := func(e Expr) bool {
		if n, ok := e.(*NumberLit); ok {
			return !strings.Contains(n.Value, ".")
		}
		return false
	}
	isString := func(e Expr) bool {
		_, ok := e.(*StringLit)
		return ok
	}
	if b.Op == "/" && isInt(b.Left) && isInt(b.Right) {
		io.WriteString(w, "div(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "%" {
		io.WriteString(w, "rem(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "+" && (isString(b.Left) || isString(b.Right)) {
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, " <> ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "in" && b.MapIn {
		io.WriteString(w, "Map.has_key?(")
		b.Right.emit(w)
		io.WriteString(w, ", ")
		b.Left.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "in" && (isString(b.Left) || isString(b.Right)) {
		io.WriteString(w, "String.contains?(")
		b.Right.emit(w)
		io.WriteString(w, ", ")
		b.Left.emit(w)
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

// UnaryExpr represents a prefix unary operation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

// CallExpr represents a function call.
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

// IndexExpr represents indexing into a list or string.
type IndexExpr struct {
	Target   Expr
	Index    Expr
	IsString bool
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.IsString {
		io.WriteString(w, "String.at(")
	} else {
		io.WriteString(w, "Enum.at(")
	}
	i.Target.emit(w)
	io.WriteString(w, ", ")
	i.Index.emit(w)
	io.WriteString(w, ")")
}

// StringLit is a quoted string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

// NumberLit is a numeric literal.
type NumberLit struct{ Value string }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

// BoolLit is a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

// ListLit is a list literal like [1,2,3].
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

// MapLit represents a map literal like %{key => value}.
type MapLit struct{ Items []MapItem }

type MapItem struct {
	Key   Expr
	Value Expr
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "%{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		it.Key.emit(w)
		io.WriteString(w, " => ")
		it.Value.emit(w)
	}
	io.WriteString(w, "}")
}

// CastExpr represents a simple cast like expr as int.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "String.to_integer(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		c.Expr.emit(w)
	}
}

// Emit generates Elixir source from the AST.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	hasFunc := false
	for _, st := range p.Stmts {
		if _, ok := st.(*FuncDecl); ok {
			hasFunc = true
			break
		}
	}
	if hasFunc {
		buf.WriteString("defmodule Main do\n")
		var main []Stmt
		for _, st := range p.Stmts {
			if _, ok := st.(*FuncDecl); ok {
				st.emit(&buf, 1)
				buf.WriteString("\n")
			} else {
				main = append(main, st)
			}
		}
		buf.WriteString("  def main() do\n")
		for _, st := range main {
			st.emit(&buf, 2)
			buf.WriteString("\n")
		}
		buf.WriteString("  end\nend\n")
		buf.WriteString("Main.main()\n")
	} else {
		for _, st := range p.Stmts {
			st.emit(&buf, 0)
			buf.WriteString("\n")
		}
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into an Elixir AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	res := &Program{}
	for _, st := range prog.Statements {
		stmt, err := compileStmt(st, env)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			res.Stmts = append(res.Stmts, stmt)
		}
	}
	_ = env
	return res, nil
}

func compileStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := compileExpr(st.Expr.Expr, env)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var val Expr
		if st.Let.Value != nil {
			var err error
			val, err = compileExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil && *st.Let.Type.Simple == "int" {
			val = &NumberLit{Value: "0"}
		}
		return &LetStmt{Name: st.Let.Name, Value: val}, nil
	case st.Var != nil:
		var val Expr
		if st.Var.Value != nil {
			var err error
			val, err = compileExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "int" {
			val = &NumberLit{Value: "0"}
		}
		return &LetStmt{Name: st.Var.Name, Value: val}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: st.Assign.Name, Value: val}, nil
		}
		if len(st.Assign.Index) == 1 && len(st.Assign.Field) == 0 {
			idx, err := compileExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			t, _ := env.GetVar(st.Assign.Name)
			var call *CallExpr
			switch t.(type) {
			case types.ListType:
				call = &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, val}}
			case types.MapType:
				call = &CallExpr{Func: "Map.put", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, val}}
			default:
				return nil, fmt.Errorf("unsupported indexed assignment at %d:%d", st.Pos.Line, st.Pos.Column)
			}
			return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
		}
		if len(st.Assign.Index) == 2 && len(st.Assign.Field) == 0 {
			idx0, err := compileExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			idx1, err := compileExpr(st.Assign.Index[1].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			t, _ := env.GetVar(st.Assign.Name)
			outer, ok := t.(types.ListType)
			if !ok {
				return nil, fmt.Errorf("unsupported indexed assignment at %d:%d", st.Pos.Line, st.Pos.Column)
			}
			if _, ok := outer.Elem.(types.ListType); !ok {
				return nil, fmt.Errorf("unsupported indexed assignment at %d:%d", st.Pos.Line, st.Pos.Column)
			}
			inner := &CallExpr{Func: "Enum.at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0}}
			innerUpdate := &CallExpr{Func: "List.replace_at", Args: []Expr{inner, idx1, val}}
			call := &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0, innerUpdate}}
			return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
		}
		return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
	case st.If != nil:
		return compileIfStmt(st.If, env)
	case st.While != nil:
		return compileWhileStmt(st.While, env)
	case st.For != nil:
		return compileForStmt(st.For, env)
	case st.Return != nil:
		var val Expr
		if st.Return.Value != nil {
			var err error
			val, err = compileExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: val}, nil
	case st.Fun != nil:
		body := make([]Stmt, 0, len(st.Fun.Body))
		for _, b := range st.Fun.Body {
			bs, err := compileStmt(b, env)
			if err != nil {
				return nil, err
			}
			if bs != nil {
				body = append(body, bs)
			}
		}
		params := make([]string, len(st.Fun.Params))
		for i, p := range st.Fun.Params {
			params[i] = p.Name
		}
		return &FuncDecl{Name: st.Fun.Name, Params: params, Body: body}, nil
	default:
		if st.Test == nil && st.Import == nil && st.Type == nil {
			return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
		}
	}
	return nil, nil
}

func compileIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := compileExpr(is.Cond, env)
	if err != nil {
		return nil, err
	}
	thenStmts := make([]Stmt, 0, len(is.Then))
	for _, s := range is.Then {
		st, err := compileStmt(s, env)
		if err != nil {
			return nil, err
		}
		if st != nil {
			thenStmts = append(thenStmts, st)
		}
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		elseStmt, err := compileIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{elseStmt}
	} else if len(is.Else) > 0 {
		for _, s := range is.Else {
			st, err := compileStmt(s, env)
			if err != nil {
				return nil, err
			}
			if st != nil {
				elseStmts = append(elseStmts, st)
			}
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func compileWhileStmt(ws *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := compileExpr(ws.Cond, env)
	if err != nil {
		return nil, err
	}
	body := make([]Stmt, 0, len(ws.Body))
	for _, s := range ws.Body {
		st, err := compileStmt(s, env)
		if err != nil {
			return nil, err
		}
		if st != nil {
			body = append(body, st)
		}
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func compileForStmt(fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	start, err := compileExpr(fs.Source, env)
	if err != nil {
		return nil, err
	}
	var end Expr
	if fs.RangeEnd != nil {
		end, err = compileExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
	}
	body := make([]Stmt, 0, len(fs.Body))
	for _, s := range fs.Body {
		st, err := compileStmt(s, env)
		if err != nil {
			return nil, err
		}
		if st != nil {
			body = append(body, st)
		}
	}
	src := start
	if fs.RangeEnd == nil {
		if _, ok := types.TypeOfExprBasic(fs.Source, env).(types.MapType); ok {
			src = &CallExpr{Func: "Map.keys", Args: []Expr{start}}
		}
	}
	if end != nil {
		src = nil
	}
	res := &ForStmt{Name: fs.Name, Start: start, End: end, Source: src, Body: body}
	return res, nil
}

func compileExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return compileBinary(e.Binary, env)
}

func compileIfExpr(ie *parser.IfExpr, env *types.Env) (Expr, error) {
	cond, err := compileExpr(ie.Cond, env)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(ie.Then, env)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = compileIfExpr(ie.ElseIf, env)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = compileExpr(ie.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func compileUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := compilePostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = &UnaryExpr{Op: u.Ops[i], Expr: expr}
	}
	return expr, nil
}

func compileBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	left, err := compileUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	operands := []Expr{left}
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		expr, err := compilePostfix(op.Right, env)
		if err != nil {
			return nil, err
		}
		ops[i] = op
		operands = append(operands, expr)
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
			if contains(level, ops[i].Op) {
				bin := &BinaryExpr{Left: operands[i], Op: ops[i].Op, Right: operands[i+1]}
				if ops[i].Op == "in" {
					if _, ok := types.TypeOfPostfix(ops[i].Right, env).(types.MapType); ok {
						bin.MapIn = true
					}
				}
				operands[i] = bin
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return nil, fmt.Errorf("invalid expression")
	}
	return operands[0], nil
}

func compilePostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		arg, err := compileExpr(pf.Ops[0].Call.Args[0], env)
		if err != nil {
			return nil, err
		}
		base, err := compilePrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}, env)
		if err != nil {
			return nil, err
		}
		expr := &CallExpr{Func: "String.contains?", Args: []Expr{base, arg}}
		return expr, nil
	}
	expr, err := compilePrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	typ := types.TypeOfPrimary(pf.Target, env)
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				expr = &CastExpr{Expr: expr, Type: *op.Cast.Type.Simple}
			} else {
				return nil, fmt.Errorf("unsupported cast")
			}
		} else if op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil {
			idx, err := compileExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			switch tt := typ.(type) {
			case types.StringType:
				expr = &IndexExpr{Target: expr, Index: idx, IsString: true}
				typ = types.StringType{}
			case types.ListType:
				expr = &IndexExpr{Target: expr, Index: idx, IsString: false}
				typ = tt.Elem
			case types.MapType:
				expr = &CallExpr{Func: "Map.get", Args: []Expr{expr, idx}}
				typ = tt.Value
			default:
				expr = &IndexExpr{Target: expr, Index: idx}
				typ = types.AnyType{}
			}
		} else if op.Index != nil && (op.Index.Colon != nil || op.Index.Colon2 != nil) {
			var start Expr = &NumberLit{Value: "0"}
			if op.Index.Start != nil {
				s, err := compileExpr(op.Index.Start, env)
				if err != nil {
					return nil, err
				}
				start = s
			}
			if op.Index.End == nil {
				return nil, fmt.Errorf("unsupported slice without end")
			}
			end, err := compileExpr(op.Index.End, env)
			if err != nil {
				return nil, err
			}
			diff := &BinaryExpr{Left: end, Op: "-", Right: start}
			switch tt := typ.(type) {
			case types.StringType:
				expr = &CallExpr{Func: "String.slice", Args: []Expr{expr, start, diff}}
				typ = types.StringType{}
			case types.ListType:
				expr = &CallExpr{Func: "Enum.slice", Args: []Expr{expr, start, diff}}
				typ = tt.Elem
			default:
				expr = &CallExpr{Func: "Enum.slice", Args: []Expr{expr, start, diff}}
				typ = types.AnyType{}
			}
		} else if op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("unsupported contains call")
			}
			arg, err := compileExpr(call.Args[0], env)
			if err != nil {
				return nil, err
			}
			expr = &CallExpr{Func: "String.contains?", Args: []Expr{expr, arg}}
			typ = types.BoolType{}
			i++
		} else {
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func compilePrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a, env)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		switch name {
		case "print":
			name = "IO.puts"
		case "count":
			name = "Enum.count"
		case "len":
			name = "length"
			if len(args) == 1 {
				t := types.TypeOfExprBasic(p.Call.Args[0], env)
				if _, ok := t.(types.StringType); ok {
					name = "String.length"
				} else if _, ok := t.(types.MapType); ok {
					name = "map_size"
				}
			}
		case "sum":
			name = "Enum.sum"
		case "min":
			name = "Enum.min"
		case "max":
			name = "Enum.max"
		case "avg":
			if len(args) == 1 {
				sumCall := &CallExpr{Func: "Enum.sum", Args: []Expr{args[0]}}
				countCall := &CallExpr{Func: "Enum.count", Args: []Expr{args[0]}}
				return &BinaryExpr{Left: sumCall, Op: "/", Right: countCall}, nil
			}
		case "str":
			name = "to_string"
		case "append":
			if len(args) == 2 {
				list := args[0]
				elemList := &ListLit{Elems: []Expr{args[1]}}
				return &CallExpr{Func: "Enum.concat", Args: []Expr{list, elemList}}, nil
			}
		case "values":
			if len(args) == 1 {
				inner := &CallExpr{Func: "Map.values", Args: []Expr{args[0]}}
				sort := &CallExpr{Func: "Enum.sort", Args: []Expr{inner}}
				return &CallExpr{Func: "Enum.join", Args: []Expr{sort, &StringLit{Value: " "}}}, nil
			}
		case "substring":
			if len(args) == 3 {
				diff := &BinaryExpr{Left: args[2], Op: "-", Right: args[1]}
				return &CallExpr{Func: "String.slice", Args: []Expr{args[0], args[1], diff}}, nil
			}
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil:
		return compileLiteral(p.Lit)
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return &VarRef{Name: name}, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, el := range p.List.Elems {
			ex, err := compileExpr(el, env)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := compileExpr(it.Key, env)
			if err != nil {
				return nil, err
			}
			v, err := compileExpr(it.Value, env)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.If != nil:
		return compileIfExpr(p.If, env)
	case p.Group != nil:
		return compileExpr(p.Group, env)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileLiteral(l *parser.Literal) (Expr, error) {
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
	return fmt.Sprintf("# Generated by Mochi transpiler v%s on %s\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

// Print converts the custom AST to an ast.Node and prints it.
func Print(p *Program) {
	n := toNodeProg(p)
	fmt.Print(n.String())
}

func toNodeProg(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, toNodeStmt(s))
	}
	return n
}

func toNodeStmt(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{toNodeExpr(st.Expr)}}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = []*ast.Node{toNodeExpr(st.Value)}
		}
		return n
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, toNodeStmt(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		thenN := &ast.Node{Kind: "then"}
		for _, t := range st.Then {
			thenN.Children = append(thenN.Children, toNodeStmt(t))
		}
		n.Children = append(n.Children, thenN)
		if len(st.Else) > 0 {
			elseN := &ast.Node{Kind: "else"}
			for _, e := range st.Else {
				elseN.Children = append(elseN.Children, toNodeStmt(e))
			}
			n.Children = append(n.Children, elseN)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForStmt:
		n := &ast.Node{Kind: "for", Value: st.Name}
		if st.End != nil {
			rng := &ast.Node{Kind: "range", Children: []*ast.Node{toNodeExpr(st.Start), toNodeExpr(st.End)}}
			n.Children = append(n.Children, rng)
		} else {
			n.Children = append(n.Children, toNodeExpr(st.Source))
		}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func toNodeExpr(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, toNodeExpr(a))
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
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, toNodeExpr(el))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			item := &ast.Node{Kind: "item"}
			item.Children = append(item.Children, toNodeExpr(it.Key))
			item.Children = append(item.Children, toNodeExpr(it.Value))
			n.Children = append(n.Children, item)
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{toNodeExpr(ex.Target), toNodeExpr(ex.Index)}}
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{toNodeExpr(ex.Cond), toNodeExpr(ex.Then), toNodeExpr(ex.Else)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
