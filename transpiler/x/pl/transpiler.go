//go:build slow

package pl

import (
	"fmt"
	"io"
	"strconv"
	"strings"

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
	switch e := p.Expr.(type) {
	case *BinaryExpr:
		be := e
		if isBoolOp(be.Op) {
			io.WriteString(w, "    (")
			be.emit(w)
			io.WriteString(w, " -> writeln(true) ; writeln(false))")
			return
		}
		if isArithOp(be.Op) && !(be.Op == "+" && (isStringLit(be.Left) || isStringLit(be.Right))) {
			fmt.Fprintf(w, "    R%d is ", idx)
			be.emit(w)
			fmt.Fprintf(w, ", writeln(R%d)", idx)
			return
		}
	case *LenExpr:
		if _, ok := e.Value.(*StringLit); ok {
			fmt.Fprintf(w, "    string_length(")
		} else {
			fmt.Fprintf(w, "    length(")
		}
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *StrExpr:
		io.WriteString(w, "    writeln(")
		e.emit(w)
		io.WriteString(w, ")")
		return
	case *CountExpr:
		fmt.Fprintf(w, "    length(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *SumExpr:
		fmt.Fprintf(w, "    sum_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *AvgExpr:
		fmt.Fprintf(w, "    sum_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", S%d), length(", idx)
		e.Value.emit(w)
		fmt.Fprintf(w, ", L%d), R%d is S%d / L%d, writeln(R%d)", idx, idx, idx, idx, idx)
		return
	case *MinExpr:
		fmt.Fprintf(w, "    min_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *MaxExpr:
		fmt.Fprintf(w, "    max_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *AppendExpr:
		fmt.Fprintf(w, "    append(")
		e.List.emit(w)
		io.WriteString(w, ", [")
		e.Elem.emit(w)
		fmt.Fprintf(w, "], R%d), writeln(R%d)", idx, idx)
		return
	case *IndexExpr:
		fmt.Fprintf(w, "    nth0(")
		e.Index.emit(w)
		io.WriteString(w, ", ")
		e.Target.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *SubstringExpr:
		fmt.Fprintf(w, "    L%d is ", idx)
		e.End.emit(w)
		io.WriteString(w, " - ")
		e.Start.emit(w)
		fmt.Fprintf(w, ", sub_string(")
		e.Str.emit(w)
		io.WriteString(w, ", ")
		e.Start.emit(w)
		fmt.Fprintf(w, ", L%d, _, R%d), writeln(R%d)", idx, idx, idx)
		return
	}
	io.WriteString(w, "    writeln(")
	p.Expr.emit(w)
	io.WriteString(w, ")")
}

func (l *LetStmt) emit(w io.Writer, _ int) {
	if _, ok := l.Expr.(*ListLit); ok {
		fmt.Fprintf(w, "    %s = ", l.Name)
	} else {
		fmt.Fprintf(w, "    %s is ", l.Name)
	}
	l.Expr.emit(w)
}

type Expr interface{ emit(io.Writer) }
type IntLit struct{ Value int }
type BoolLit struct{ Value bool }
type StringLit struct{ Value string }
type Var struct{ Name string }
type ListLit struct{ Elems []Expr }
type LenExpr struct{ Value Expr }
type StrExpr struct{ Value Expr }
type CountExpr struct{ Value Expr }
type SumExpr struct{ Value Expr }
type AvgExpr struct{ Value Expr }
type MinExpr struct{ Value Expr }
type MaxExpr struct{ Value Expr }
type AppendExpr struct {
	List Expr
	Elem Expr
}
type IndexExpr struct {
	Target Expr
	Index  Expr
}
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}
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
	if b.Op == "+" {
		if l, ok := b.Left.(*StringLit); ok {
			if r, ok2 := b.Right.(*StringLit); ok2 {
				fmt.Fprintf(w, "'%s'", escape(l.Value+r.Value))
				return
			}
		}
	}
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

func (l *LenExpr) emit(w io.Writer) {
	io.WriteString(w, "len(")
	l.Value.emit(w)
	io.WriteString(w, ")")
}

func (s *StrExpr) emit(w io.Writer) {
	if lit, ok := s.Value.(*IntLit); ok {
		fmt.Fprintf(w, "'%d'", lit.Value)
		return
	}
	s.Value.emit(w)
}

func (c *CountExpr) emit(w io.Writer) {
	io.WriteString(w, "length(")
	c.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (s *SumExpr) emit(w io.Writer) {
	io.WriteString(w, "sum_list(")
	s.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (a *AvgExpr) emit(w io.Writer) {
	io.WriteString(w, "sum_list(")
	a.Value.emit(w)
	io.WriteString(w, ", S), length(")
	a.Value.emit(w)
	io.WriteString(w, ", L), R is S / L")
}

func (m *MinExpr) emit(w io.Writer) {
	io.WriteString(w, "min_list(")
	m.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (m *MaxExpr) emit(w io.Writer) {
	io.WriteString(w, "max_list(")
	m.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "append(")
	a.List.emit(w)
	io.WriteString(w, ", [")
	a.Elem.emit(w)
	io.WriteString(w, "], R)")
}

func (i *IndexExpr) emit(w io.Writer) {
	io.WriteString(w, "nth0(")
	i.Index.emit(w)
	io.WriteString(w, ", ")
	i.Target.emit(w)
	io.WriteString(w, ", R)")
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "(Len is ")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ", sub_string(")
	s.Str.emit(w)
	io.WriteString(w, ", ")
	s.Start.emit(w)
	io.WriteString(w, ", Len, _, R))")
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
	case "=:=", "=\\=", "<", "<=", ">", ">=", "@<", "@=<", "@>", "@>=", ",", ";":
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

func isStringLit(e Expr) bool {
	_, ok := e.(*StringLit)
	return ok
}

func intValue(e Expr) (int, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, true
	case *GroupExpr:
		return intValue(v.Expr)
	}
	return 0, false
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
			if isStringLit(left) || isStringLit(right) {
				switch op {
				case "<":
					opStr = "@<"
				case "<=":
					opStr = "@=<"
				case ">":
					opStr = "@>"
				case ">=":
					opStr = "@>="
				}
			} else {
				opStr = op
			}
		case "&&":
			opStr = ","
		case "||":
			opStr = ";"
		default:
			return nil, fmt.Errorf("unsupported op")
		}
		if lb, lok := left.(*BoolLit); lok {
			if rb, rok := right.(*BoolLit); rok {
				switch opStr {
				case ",":
					left = &BoolLit{Value: lb.Value && rb.Value}
					continue
				case ";":
					left = &BoolLit{Value: lb.Value || rb.Value}
					continue
				}
			}
		} else if lv, lok := intValue(left); lok {
			if rv, rok := intValue(right); rok {
				switch opStr {
				case "+":
					left = &IntLit{Value: lv + rv}
					continue
				case "-":
					left = &IntLit{Value: lv - rv}
					continue
				case "*":
					left = &IntLit{Value: lv * rv}
					continue
				case "mod":
					if rv != 0 {
						left = &IntLit{Value: lv % rv}
						continue
					}
				}
			}
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
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.End != nil || op.Index.Step != nil {
				return nil, fmt.Errorf("unsupported slice")
			}
			idx, err := toExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			if list, ok := expr.(*ListLit); ok {
				if lit, ok2 := idx.(*IntLit); ok2 {
					if lit.Value >= 0 && lit.Value < len(list.Elems) {
						expr = list.Elems[lit.Value]
						continue
					}
				}
			}
			expr = &IndexExpr{Target: expr, Index: idx}
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
	case p.Call != nil:
		switch p.Call.Func {
		case "len", "str", "count", "sum", "avg", "min", "max":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("unsupported call")
			}
			arg, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			switch p.Call.Func {
			case "len":
				return &LenExpr{Value: arg}, nil
			case "str":
				return &StrExpr{Value: arg}, nil
			case "count":
				return &CountExpr{Value: arg}, nil
			case "sum":
				return &SumExpr{Value: arg}, nil
			case "avg":
				return &AvgExpr{Value: arg}, nil
			case "min":
				return &MinExpr{Value: arg}, nil
			case "max":
				return &MaxExpr{Value: arg}, nil
			}
		case "substring":
			if len(p.Call.Args) != 3 {
				return nil, fmt.Errorf("unsupported call")
			}
			strArg, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			startArg, err := toExpr(p.Call.Args[1], env)
			if err != nil {
				return nil, err
			}
			endArg, err := toExpr(p.Call.Args[2], env)
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Str: strArg, Start: startArg, End: endArg}, nil
		case "append":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("unsupported call")
			}
			listArg, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			elemArg, err := toExpr(p.Call.Args[1], env)
			if err != nil {
				return nil, err
			}
			return &AppendExpr{List: listArg, Elem: elemArg}, nil
		default:
			return nil, fmt.Errorf("unsupported call")
		}
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := toExpr(e, env)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
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
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Value)}}
	case *StrExpr:
		return &ast.Node{Kind: "strcall", Children: []*ast.Node{exprNode(ex.Value)}}
	case *CountExpr:
		return &ast.Node{Kind: "count", Children: []*ast.Node{exprNode(ex.Value)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.Value)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{exprNode(ex.Value)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Elem)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Expr)}}
	default:
		return &ast.Node{Kind: "expr"}
	}
}
