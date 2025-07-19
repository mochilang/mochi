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
	switch e := p.Expr.(type) {
	case *BinaryExpr:
		be := e
		if isBoolOp(be.Op) {
			io.WriteString(w, "    (")
			be.emit(w)
			io.WriteString(w, " -> write(true) ; write(false)), nl")
			return
		}
		if isArithOp(be.Op) && !(be.Op == "+" && (isStringLit(be.Left) || isStringLit(be.Right))) {
			fmt.Fprintf(w, "    R%d is ", idx)
			be.emit(w)
			fmt.Fprintf(w, ", write(R%d), nl", idx)
			return
		}
	case *LenExpr:
		switch v := e.Value.(type) {
		case *SetOpExpr:
			name := v.Op
			if name == "union_all" {
				name = "append"
			}
			fmt.Fprintf(w, "    %s(", name)
			v.Left.emit(w)
			io.WriteString(w, ", ")
			v.Right.emit(w)
			fmt.Fprintf(w, ", T%d), length(T%d, R%d), write(R%d), nl", idx, idx, idx, idx)
			return
		case *IndexExpr:
			fmt.Fprintf(w, "    get_item(")
			v.Container.emit(w)
			io.WriteString(w, ", ")
			v.Index.emit(w)
			fmt.Fprintf(w, ", T%d), length(T%d, R%d), write(R%d), nl", idx, idx, idx, idx)
			return
		}
		if _, ok := e.Value.(*StringLit); ok {
			fmt.Fprintf(w, "    string_length(")
		} else {
			fmt.Fprintf(w, "    length(")
		}
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	case *StrExpr:
		io.WriteString(w, "    write(")
		e.emit(w)
		io.WriteString(w, "), nl")
		return
	case *CountExpr:
		fmt.Fprintf(w, "    length(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	case *SumExpr:
		fmt.Fprintf(w, "    sum_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	case *AvgExpr:
		fmt.Fprintf(w, "    sum_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", S%d), length(", idx)
		e.Value.emit(w)
		fmt.Fprintf(w, ", L%d), R%d is S%d / L%d, write(R%d), nl", idx, idx, idx, idx, idx)
		return
	case *MinExpr:
		fmt.Fprintf(w, "    min_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	case *MaxExpr:
		fmt.Fprintf(w, "    max_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	case *AppendExpr:
		fmt.Fprintf(w, "    append(")
		e.List.emit(w)
		io.WriteString(w, ", [")
		e.Elem.emit(w)
		fmt.Fprintf(w, "], R%d), write(R%d), nl", idx, idx)
		return
	case *SubstringExpr:
		fmt.Fprintf(w, "    substring(")
		e.Str.emit(w)
		io.WriteString(w, ", ")
		e.Start.emit(w)
		io.WriteString(w, ", ")
		e.End.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	case *IndexExpr:
		fmt.Fprintf(w, "    get_item(")
		e.Container.emit(w)
		io.WriteString(w, ", ")
		e.Index.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	case *SetOpExpr:
		name := e.Op
		if name == "union_all" {
			name = "append"
		}
		fmt.Fprintf(w, "    %s(", name)
		e.Left.emit(w)
		io.WriteString(w, ", ")
		e.Right.emit(w)
		fmt.Fprintf(w, ", R%d), write(R%d), nl", idx, idx)
		return
	}
	io.WriteString(w, "    write(")
	p.Expr.emit(w)
	io.WriteString(w, "), nl")
}

func (l *LetStmt) emit(w io.Writer, _ int) {
	switch e := l.Expr.(type) {
	case *IndexExpr:
		fmt.Fprintf(w, "    get_item(")
		e.Container.emit(w)
		io.WriteString(w, ", ")
		e.Index.emit(w)
		fmt.Fprintf(w, ", %s)", l.Name)
	case *SetOpExpr:
		name := e.Op
		if name == "union_all" {
			name = "append"
		}
		fmt.Fprintf(w, "    %s(", name)
		e.Left.emit(w)
		io.WriteString(w, ", ")
		e.Right.emit(w)
		fmt.Fprintf(w, ", %s)", l.Name)
	default:
		if be, ok := l.Expr.(*BinaryExpr); ok && isArithOp(be.Op) && !(be.Op == "+" && (isStringLit(be.Left) || isStringLit(be.Right))) {
			fmt.Fprintf(w, "    %s is ", l.Name)
			l.Expr.emit(w)
		} else {
			fmt.Fprintf(w, "    %s = ", l.Name)
			l.Expr.emit(w)
		}
	}
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
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}
type IndexExpr struct {
	Container Expr
	Index     Expr
}
type SetOpExpr struct {
	Left  Expr
	Op    string
	Right Expr
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
func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "\"%s\"", escape(s.Value)) }
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

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "substring(")
	s.Str.emit(w)
	io.WriteString(w, ", ")
	s.Start.emit(w)
	io.WriteString(w, ", ")
	s.End.emit(w)
	io.WriteString(w, ", R)")
}

func (i *IndexExpr) emit(w io.Writer) {
	io.WriteString(w, "get_item(")
	i.Container.emit(w)
	io.WriteString(w, ", ")
	i.Index.emit(w)
	io.WriteString(w, ", R)")
}

func (s *SetOpExpr) emit(w io.Writer) {
	name := s.Op
	if name == "union_all" {
		name = "append"
	}
	io.WriteString(w, name)
	io.WriteString(w, "(")
	s.Left.emit(w)
	io.WriteString(w, ", ")
	s.Right.emit(w)
	io.WriteString(w, ", R)")
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
	case "=:=", "=\\=", "<", "<=", ">", ">=", "@<", "@=<", "@>", "@>=":
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
	return fmt.Sprintf(`%s %s
:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).
union(A, B, R) :- append(A, B, C), list_to_set(C, R).
except([], _, []).
except([H|T], B, R) :- memberchk(H, B), !, except(T, B, R).
except([H|T], B, [H|R]) :- except(T, B, R).
intersect(A, B, R) :- intersect(A, B, [], R).
intersect([], _, Acc, R) :- reverse(Acc, R).
intersect([H|T], B, Acc, R) :- memberchk(H, B), \+ memberchk(H, Acc), !, intersect(T, B, [H|Acc], R).
intersect([_|T], B, Acc, R) :- intersect(T, B, Acc, R).
substring(S,I,J,R) :- Len is J - I, sub_string(S, I, Len, _, R).
`, "% Generated by Mochi transpiler", t)
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
		case "union":
			if r.All {
				left = &SetOpExpr{Left: left, Op: "union_all", Right: right}
				continue
			}
			left = &SetOpExpr{Left: left, Op: "union", Right: right}
			continue
		case "except":
			left = &SetOpExpr{Left: left, Op: "except", Right: right}
			continue
		case "intersect":
			left = &SetOpExpr{Left: left, Op: "intersect", Right: right}
			continue
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
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			if op.Index.Start == nil {
				return nil, fmt.Errorf("unsupported index")
			}
			idx, err := toExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Container: expr, Index: idx}
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
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Container), exprNode(ex.Index)}}
	case *SetOpExpr:
		return &ast.Node{Kind: "setop", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Expr)}}
	default:
		return &ast.Node{Kind: "expr"}
	}
}
