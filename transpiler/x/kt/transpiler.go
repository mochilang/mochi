//go:build slow

package kt

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Program represents a simple Kotlin program consisting of statements executed in main.
// Program contains top level functions and statements executed in `main`.
type Program struct {
	Funcs []*FuncDef
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

// ReturnStmt is a return statement inside a function.
type ReturnStmt struct{ Value Expr }

func (s *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return")
	if s.Value != nil {
		io.WriteString(w, " ")
		s.Value.emit(w)
	}
}

// FuncDef represents a top level function definition.
type FuncDef struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FuncDef) emit(w io.Writer) {
	io.WriteString(w, "fun "+f.Name+"(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p+": Any")
	}
	io.WriteString(w, "): Any {\n")
	for _, s := range f.Body {
		io.WriteString(w, "    ")
		s.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
	io.WriteString(w, "\n")
}

type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement that evaluates an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

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

// StringLit represents a quoted string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IntLit struct{ Value int64 }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

// IndexExpr represents a[i].
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (ix *IndexExpr) emit(w io.Writer) {
	ix.Target.emit(w)
	io.WriteString(w, "[")
	ix.Index.emit(w)
	io.WriteString(w, "]")
}

// MapLit represents a Kotlin map literal.
type MapLit struct{ Items []MapItem }

type MapItem struct {
	Key   Expr
	Value Expr
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "mapOf(")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		it.Key.emit(w)
		io.WriteString(w, " to ")
		it.Value.emit(w)
	}
	io.WriteString(w, ")")
}

// ContainsExpr represents s.contains(sub).
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

func (c *ContainsExpr) emit(w io.Writer) {
	c.Str.emit(w)
	io.WriteString(w, ".contains(")
	c.Sub.emit(w)
	io.WriteString(w, ")")
}

type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " "+b.Op+" ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) { // 'let' is immutable
	io.WriteString(w, "val "+s.Name+" = ")
	s.Value.emit(w)
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) {
	io.WriteString(w, "var "+s.Name+" = ")
	s.Value.emit(w)
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, s.Name+" = ")
	s.Value.emit(w)
}

// IfStmt represents a simple if/else conditional.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		io.WriteString(w, "    ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range i.Else {
			io.WriteString(w, "    ")
			st.emit(w)
			io.WriteString(w, "\n")
		}
		io.WriteString(w, "}")
	}
}

// WhileStmt represents a basic while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while (")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range ws.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

// ForRangeStmt represents a numeric for-loop like `for i in a..b {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "for ("+fr.Name+" in ")
	if fr.Start != nil {
		fr.Start.emit(w)
	}
	io.WriteString(w, " until ")
	if fr.End != nil {
		fr.End.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range fr.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

// ForEachStmt iterates over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (fe *ForEachStmt) emit(w io.Writer) {
	io.WriteString(w, "for ("+fe.Name+" in ")
	if fe.Iterable != nil {
		fe.Iterable.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range fe.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break") }

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue") }

// IfExpr is a conditional expression using Kotlin's `if`.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (ie *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if (")
	ie.Cond.emit(w)
	io.WriteString(w, ") ")
	ie.Then.emit(w)
	if ie.Else != nil {
		io.WriteString(w, " else ")
		ie.Else.emit(w)
	}
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "listOf(")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, ")")
}

type CountExpr struct{ Value Expr }

func (c *CountExpr) emit(w io.Writer) {
	c.Value.emit(w)
	io.WriteString(w, ".size")
}

type SumExpr struct{ Value Expr }

func (s *SumExpr) emit(w io.Writer) {
	s.Value.emit(w)
	io.WriteString(w, ".sum()")
}

type AvgExpr struct{ Value Expr }

func (a *AvgExpr) emit(w io.Writer) {
	a.Value.emit(w)
	io.WriteString(w, ".average()")
}

type LenExpr struct {
	Value    Expr
	IsString bool
}

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	if l.IsString {
		io.WriteString(w, ".length")
	} else {
		io.WriteString(w, ".size")
	}
}

type StrExpr struct{ Value Expr }

func (s *StrExpr) emit(w io.Writer) {
	s.Value.emit(w)
	io.WriteString(w, ".toString()")
}

type NotExpr struct{ Value Expr }

func (n *NotExpr) emit(w io.Writer) {
	io.WriteString(w, "!")
	n.Value.emit(w)
}

type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	io.WriteString(w, " + listOf(")
	a.Elem.emit(w)
	io.WriteString(w, ")")
}

type MinExpr struct{ Value Expr }

func (m *MinExpr) emit(w io.Writer) {
	m.Value.emit(w)
	io.WriteString(w, ".min()")
}

type MaxExpr struct{ Value Expr }

func (m *MaxExpr) emit(w io.Writer) {
	m.Value.emit(w)
	io.WriteString(w, ".max()")
}

type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	v.Map.emit(w)
	io.WriteString(w, ".values")
}

type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Value.emit(w)
	io.WriteString(w, ".substring(")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	s.End.emit(w)
	io.WriteString(w, ")")
}

// Transpile converts a Mochi program to a simple Kotlin AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	p := &Program{}
	for _, st := range prog.Statements {
		switch {
		case st.Expr != nil:
			e, err := convertExpr(env, st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
		case st.Let != nil:
			var val Expr
			if st.Let.Value != nil {
				var err error
				val, err = convertExpr(env, st.Let.Value)
				if err != nil {
					return nil, err
				}
			} else {
				val = &IntLit{Value: 0}
			}
			p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Value: val})
		case st.Var != nil:
			var val Expr
			if st.Var.Value != nil {
				var err error
				val, err = convertExpr(env, st.Var.Value)
				if err != nil {
					return nil, err
				}
			} else {
				val = &IntLit{Value: 0}
			}
			p.Stmts = append(p.Stmts, &VarStmt{Name: st.Var.Name, Value: val})
		case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
			e, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Value: e})
		case st.Return != nil:
			var val Expr
			if st.Return.Value != nil {
				var err error
				val, err = convertExpr(env, st.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			p.Stmts = append(p.Stmts, &ReturnStmt{Value: val})
		case st.Fun != nil:
			body, err := convertStmts(env, st.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p0 := range st.Fun.Params {
				params = append(params, p0.Name)
			}
			p.Funcs = append(p.Funcs, &FuncDef{Name: st.Fun.Name, Params: params, Body: body})
		case st.If != nil:
			stmt, err := convertIfStmt(env, st.If)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.While != nil:
			stmt, err := convertWhileStmt(env, st.While)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.For != nil:
			stmt, err := convertForStmt(env, st.For)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.Break != nil:
			p.Stmts = append(p.Stmts, &BreakStmt{})
		case st.Continue != nil:
			p.Stmts = append(p.Stmts, &ContinueStmt{})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return p, nil
}

func convertStmts(env *types.Env, list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Expr != nil:
			e, err := convertExpr(env, s.Expr.Expr)
			if err != nil {
				return nil, err
			}
			out = append(out, &ExprStmt{Expr: e})
		case s.Let != nil:
			v, err := convertExpr(env, s.Let.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &LetStmt{Name: s.Let.Name, Value: v})
		case s.Var != nil:
			v, err := convertExpr(env, s.Var.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &VarStmt{Name: s.Var.Name, Value: v})
		case s.Assign != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0:
			v, err := convertExpr(env, s.Assign.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssignStmt{Name: s.Assign.Name, Value: v})
		case s.Return != nil:
			var v Expr
			if s.Return.Value != nil {
				var err error
				v, err = convertExpr(env, s.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			out = append(out, &ReturnStmt{Value: v})
		case s.If != nil:
			st, err := convertIfStmt(env, s.If)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.While != nil:
			st, err := convertWhileStmt(env, s.While)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.For != nil:
			st, err := convertForStmt(env, s.For)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.Break != nil:
			out = append(out, &BreakStmt{})
		case s.Continue != nil:
			out = append(out, &ContinueStmt{})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func convertIfStmt(env *types.Env, is *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(env, is.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmts(env, is.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		stmt, err := convertIfStmt(env, is.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{stmt}
	} else if len(is.Else) > 0 {
		elseStmts, err = convertStmts(env, is.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(env *types.Env, ws *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(env, ws.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmts(env, ws.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(env *types.Env, fs *parser.ForStmt) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(env, fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(env, fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmts(env, fs.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(env, fs.Source)
	if err != nil {
		return nil, err
	}
	body, err := convertStmts(env, fs.Body)
	if err != nil {
		return nil, err
	}
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertIfExpr(env *types.Env, ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(env, ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(env, ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(env, ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(env, ie.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertExpr(env *types.Env, e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	first, err := convertUnary(env, e.Binary.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := []string{}
	for _, part := range e.Binary.Right {
		r, err := convertPostfix(env, part.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, r)
		ops = append(ops, part.Op)
	}
	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}}
	contains := func(list []string, s string) bool {
		for _, v := range list {
			if v == s {
				return true
			}
		}
		return false
	}
	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				operands[i] = &BinaryExpr{Left: l, Op: ops[i], Right: r}
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

func convertUnary(env *types.Env, u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	ex, err := convertPostfix(env, u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			ex = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: ex}
		case "!":
			ex = &NotExpr{Value: ex}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return ex, nil
}

func convertPostfix(env *types.Env, p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "contains" && len(p.Ops) == 1 && p.Ops[0].Call != nil {
		if len(p.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		base, err := convertPrimary(env, &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Target.Selector.Root}})
		if err != nil {
			return nil, err
		}
		arg, err := convertExpr(env, p.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		return &ContainsExpr{Str: base, Sub: arg}, nil
	}
	expr, err := convertPrimary(env, p.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(env, op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil:
			call := p.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertExpr(env, call.Args[0])
			if err != nil {
				return nil, err
			}
			expr = &ContainsExpr{Str: expr, Sub: arg}
			i++ // skip call op
		case op.Field != nil:
			return nil, fmt.Errorf("unsupported field access")
		case op.Call != nil:
			return nil, fmt.Errorf("unsupported call")
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(env *types.Env, p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		switch p.Call.Func {
		case "count", "sum", "avg", "len", "str":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("%s expects 1 arg", p.Call.Func)
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			switch p.Call.Func {
			case "count":
				return &CountExpr{Value: arg}, nil
			case "sum":
				return &SumExpr{Value: arg}, nil
			case "avg":
				return &AvgExpr{Value: arg}, nil
			case "len":
				isStr := types.IsStringExpr(p.Call.Args[0], env)
				return &LenExpr{Value: arg, IsString: isStr}, nil
			case "str":
				return &StrExpr{Value: arg}, nil
			}
			return nil, fmt.Errorf("unsupported builtin")
		case "append":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			list, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			elem, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &AppendExpr{List: list, Elem: elem}, nil
		case "min":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("min expects 1 arg")
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MinExpr{Value: arg}, nil
		case "max":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("max expects 1 arg")
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MaxExpr{Value: arg}, nil
		case "values":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("values expects 1 arg")
			}
			m, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &ValuesExpr{Map: m}, nil
		case "substring":
			if len(p.Call.Args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			str, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			start, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(env, p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Value: str, Start: start, End: end}, nil
		default:
			args := make([]Expr, len(p.Call.Args))
			for i, a := range p.Call.Args {
				ex, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			name := p.Call.Func
			if name == "print" {
				name = "println"
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
	case p.If != nil:
		return convertIfExpr(env, p.If)
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int64(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(env, e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(env, it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Group != nil:
		return convertExpr(env, p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

// Emit returns formatted Kotlin source code for prog.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	ver := readVersion()
	ts := time.Now().Format("2006-01-02 15:04:05 MST")
	fmt.Fprintf(&buf, "// Mochi %s - generated %s\n", ver, ts)
	for _, f := range prog.Funcs {
		f.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("fun main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("}\n")
	return buf.Bytes()
}

func readVersion() string {
	_, file, _, _ := runtime.Caller(0)
	dir := filepath.Dir(file)
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			v, err := os.ReadFile(filepath.Join(dir, "VERSION"))
			if err != nil {
				return "unknown"
			}
			return strings.TrimSpace(string(v))
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "unknown"
}

// Print writes a Lisp-like representation of the Kotlin AST to stdout.
func Print(p *Program) {
	toNodeProg(p).Print("")
}

func toNodeProg(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, f := range p.Funcs {
		n.Children = append(n.Children, toNodeStmt(f))
	}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, toNodeStmt(s))
	}
	return n
}

func toNodeStmt(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{toNodeExpr(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *VarStmt:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, t := range st.Then {
			thenNode.Children = append(thenNode.Children, toNodeStmt(t))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, e := range st.Else {
				elseNode.Children = append(elseNode.Children, toNodeStmt(e))
			}
			n.Children = append(n.Children, elseNode)
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
	case *ForRangeStmt:
		n := &ast.Node{Kind: "forrange", Value: st.Name}
		n.Children = append(n.Children, toNodeExpr(st.Start), toNodeExpr(st.End))
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForEachStmt:
		n := &ast.Node{Kind: "foreach", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = append(n.Children, toNodeExpr(st.Value))
		}
		return n
	case *FuncDef:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, b := range st.Body {
			n.Children = append(n.Children, toNodeStmt(b))
		}
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
	case *CountExpr:
		return &ast.Node{Kind: "count", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *StrExpr:
		return &ast.Node{Kind: "str", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *NotExpr:
		return &ast.Node{Kind: "not", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *VarRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: ex.Value}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, toNodeExpr(el))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			n.Children = append(n.Children, &ast.Node{Kind: "entry", Children: []*ast.Node{toNodeExpr(it.Key), toNodeExpr(it.Value)}})
		}
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{toNodeExpr(ex.Target), toNodeExpr(ex.Index)}}
	case *ContainsExpr:
		return &ast.Node{Kind: "contains", Children: []*ast.Node{toNodeExpr(ex.Str), toNodeExpr(ex.Sub)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{toNodeExpr(ex.List), toNodeExpr(ex.Elem)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{toNodeExpr(ex.Map)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{toNodeExpr(ex.Value), toNodeExpr(ex.Start), toNodeExpr(ex.End)}}
	case *IfExpr:
		n := &ast.Node{Kind: "ifexpr"}
		n.Children = append(n.Children, toNodeExpr(ex.Cond), toNodeExpr(ex.Then))
		if ex.Else != nil {
			n.Children = append(n.Children, toNodeExpr(ex.Else))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
