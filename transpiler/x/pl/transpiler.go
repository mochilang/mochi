//go:build slow

package pl

import (
	"encoding/json"
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Program represents a simple Prolog program.
type Program struct {
	Funcs []*Function
	Stmts []Stmt
}

type Function struct {
	Name   string
	Params []string
	Body   []Stmt
	Return Expr
}

func (f *Function) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, cap(p))
	}
	if len(f.Params) > 0 {
		io.WriteString(w, ", ")
	}
	io.WriteString(w, "R) :-\n")
	for i, st := range f.Body {
		st.emit(w, i)
		io.WriteString(w, ",\n")
	}
	io.WriteString(w, "    R = ")
	f.Return.emit(w)
	io.WriteString(w, ".\n\n")
}

type Stmt interface{ emit(io.Writer, int) }

type PrintStmt struct{ Expr Expr }
type MultiPrintStmt struct{ Exprs []Expr }
type LetStmt struct {
	Name string
	Expr Expr
}

// IndexAssignStmt updates a list element at runtime.
type IndexAssignStmt struct {
	Name   string
	Target string
	Index  Expr
	Value  Expr
}
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type compileEnv struct {
	vars   map[string]int
	consts map[string]Expr
	funcs  map[string]Expr
}

func newCompileEnv(funcs map[string]Expr) *compileEnv {
	return &compileEnv{vars: make(map[string]int), consts: make(map[string]Expr), funcs: funcs}
}

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

func (e *compileEnv) setConst(name string, ex Expr) {
	switch ex.(type) {
	case *IntLit, *FloatLit, *BoolLit, *StringLit, *ListLit, *MapLit:
		e.consts[name] = ex
	default:
		delete(e.consts, name)
	}
}

func (e *compileEnv) constExpr(name string) Expr {
	return e.consts[name]
}

func varName(name string, v int) string {
	if v == 0 {
		return cap(name)
	}
	return fmt.Sprintf("%s%d", cap(name), v)
}

func collectConstFuncs(p *parser.Program) map[string]Expr {
	funcs := make(map[string]Expr)
	ce := newCompileEnv(nil)
	for _, st := range p.Statements {
		if st.Fun == nil || len(st.Fun.Params) > 0 || st.Fun.Return == nil {
			continue
		}
		body := st.Fun.Body
		if len(body) == 0 {
			continue
		}
		ret := body[len(body)-1].Return
		if ret == nil || ret.Value == nil {
			continue
		}
		expr, err := toExpr(ret.Value, ce)
		if err == nil {
			funcs[st.Fun.Name] = expr
		}
	}
	return funcs
}

func (p *PrintStmt) emit(w io.Writer, idx int) {
	switch e := p.Expr.(type) {
	case *BinaryExpr:
		be := e
		if be.Op == "=" || be.Op == "\\=" {
			if se, ok := be.Left.(*SliceExpr); ok && se.IsString {
				fmt.Fprintf(w, "    L%d is ", idx)
				se.End.emit(w)
				io.WriteString(w, " - ")
				se.Start.emit(w)
				fmt.Fprintf(w, ", sub_string(")
				se.Target.emit(w)
				io.WriteString(w, ", ")
				se.Start.emit(w)
				fmt.Fprintf(w, ", L%d, _, T%d), ", idx, idx)
				if be.Op == "=" {
					fmt.Fprintf(w, "((T%d = ", idx)
				} else {
					fmt.Fprintf(w, "((T%d \\= ", idx)
				}
				be.Right.emit(w)
				io.WriteString(w, ") -> writeln(true) ; writeln(false))")
				return
			}
		}
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
			e.Value.emit(w)
			fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
			return
		}
		if _, ok := e.Value.(*MapLit); ok {
			fmt.Fprintf(w, "    dict_pairs(")
			e.Value.emit(w)
			fmt.Fprintf(w, ", _, P%d), length(P%d, R%d), writeln(R%d)", idx, idx, idx, idx)
			return
		}
		fmt.Fprintf(w, "    length(")
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
	case *ListLit:
		for i, el := range e.Elems {
			io.WriteString(w, "    writeln(")
			el.emit(w)
			io.WriteString(w, ")")
			if i < len(e.Elems)-1 {
				io.WriteString(w, ",\n")
			}
		}
		return
	case *CallExpr:
		fmt.Fprintf(w, "    %s(", e.Name)
		for i, a := range e.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *IndexExpr:
		if e.IsString {
			io.WriteString(w, "    sub_string(")
			e.Target.emit(w)
			io.WriteString(w, ", ")
			e.Index.emit(w)
			fmt.Fprintf(w, ", 1, _, R%d), writeln(R%d)", idx, idx)
		} else {
			fmt.Fprintf(w, "    nth0(")
			e.Index.emit(w)
			io.WriteString(w, ", ")
			e.Target.emit(w)
			fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		}
		return
	case *SliceExpr:
		if e.IsString {
			fmt.Fprintf(w, "    L%d is ", idx)
			e.End.emit(w)
			io.WriteString(w, " - ")
			e.Start.emit(w)
			fmt.Fprintf(w, ", sub_string(")
			e.Target.emit(w)
			io.WriteString(w, ", ")
			e.Start.emit(w)
			fmt.Fprintf(w, ", L%d, _, R%d), writeln(R%d)", idx, idx, idx)
		} else {
			io.WriteString(w, "    writeln(slice_not_supported)")
		}
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
	case *InExpr:
		io.WriteString(w, "    (")
		e.emit(w)
		io.WriteString(w, " -> writeln(true) ; writeln(false))")
		return
	case *IfExpr:
		io.WriteString(w, "    (")
		e.Cond.emit(w)
		io.WriteString(w, " -> ")
		switch t := e.Then.(type) {
		case *StringLit:
			fmt.Fprintf(w, "writeln('%s')", escape(t.Value))
		default:
			io.WriteString(w, "writeln(")
			e.Then.emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, " ; ")
		switch t := e.Else.(type) {
		case *StringLit:
			fmt.Fprintf(w, "writeln('%s')", escape(t.Value))
		default:
			io.WriteString(w, "writeln(")
			e.Else.emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, "    writeln(")
	p.Expr.emit(w)
	io.WriteString(w, ")")
}

func (p *MultiPrintStmt) emit(w io.Writer, idx int) {
	for i, ex := range p.Exprs {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if i == len(p.Exprs)-1 {
			io.WriteString(w, "writeln(")
			ex.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "write(")
			ex.emit(w)
			io.WriteString(w, "), write(' ')")
		}
	}
}

func (l *LetStmt) emit(w io.Writer, _ int) {
	if ie, ok := l.Expr.(*IfExpr); ok {
		io.WriteString(w, "    ")
		emitIfToVar(w, l.Name, ie)
		return
	}
	if ce, ok := l.Expr.(*CallExpr); ok {
		fmt.Fprintf(w, "    %s(", ce.Name)
		for i, a := range ce.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		fmt.Fprintf(w, ", %s)", l.Name)
		return
	}
	if needsIs(l.Expr) {
		fmt.Fprintf(w, "    %s is ", l.Name)
	} else {
		fmt.Fprintf(w, "    %s = ", l.Name)
	}
	l.Expr.emit(w)
}

func (s *IndexAssignStmt) emit(w io.Writer, idx int) {
	tmp := fmt.Sprintf("T%d", idx)
	fmt.Fprintf(w, "    nth0(")
	s.Index.emit(w)
	fmt.Fprintf(w, ", %s, _, %s),\n    nth0(", s.Target, tmp)
	s.Index.emit(w)
	fmt.Fprintf(w, ", %s, ", s.Name)
	s.Value.emit(w)
	fmt.Fprintf(w, ", %s)", tmp)
}

func emitIfToVar(w io.Writer, name string, ie *IfExpr) {
	io.WriteString(w, "(")
	ie.Cond.emit(w)
	io.WriteString(w, " -> ")
	if t, ok := ie.Then.(*IfExpr); ok {
		emitIfToVar(w, name, t)
	} else {
		fmt.Fprintf(w, "%s = ", name)
		ie.Then.emit(w)
	}
	io.WriteString(w, " ; ")
	if e, ok := ie.Else.(*IfExpr); ok {
		emitIfToVar(w, name, e)
	} else {
		fmt.Fprintf(w, "%s = ", name)
		ie.Else.emit(w)
	}
	io.WriteString(w, ")")
}

func (i *IfStmt) emit(w io.Writer, idx int) {
	io.WriteString(w, "    (")
	i.Cond.emit(w)
	io.WriteString(w, " ->\n")
	for j, st := range i.Then {
		st.emit(w, idx+j)
		if j < len(i.Then)-1 {
			io.WriteString(w, ",\n")
		}
	}
	if len(i.Else) > 0 {
		io.WriteString(w, " ;\n")
		for j, st := range i.Else {
			st.emit(w, idx+j)
			if j < len(i.Else)-1 {
				io.WriteString(w, ",\n")
			}
		}
	}
	io.WriteString(w, ")")
}

type Expr interface{ emit(io.Writer) }
type IntLit struct{ Value int }
type FloatLit struct{ Value float64 }
type BoolLit struct{ Value bool }
type StringLit struct{ Value string }
type Var struct{ Name string }
type UnaryNot struct{ Expr Expr }
type ListLit struct{ Elems []Expr }
type MapItem struct {
	Key   string
	Value Expr
}
type MapLit struct{ Items []MapItem }
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
	Target   Expr
	Index    Expr
	IsString bool
	IsMap    bool
}
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}
type CallExpr struct {
	Name string
	Args []Expr
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
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}
type SliceExpr struct {
	Target   Expr
	Start    Expr
	End      Expr
	IsString bool
}

type InExpr struct {
	Elem     Expr
	Target   Expr
	IsString bool
	IsMap    bool
}

func (i *IntLit) emit(w io.Writer)    { fmt.Fprintf(w, "%d", i.Value) }
func (f *FloatLit) emit(w io.Writer)  { io.WriteString(w, strconv.FormatFloat(f.Value, 'f', -1, 64)) }
func (b *BoolLit) emit(w io.Writer)   { fmt.Fprintf(w, "%v", b.Value) }
func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "\"%s\"", escape(s.Value)) }
func (v *Var) emit(w io.Writer)       { io.WriteString(w, v.Name) }
func (u *UnaryNot) emit(w io.Writer) {
	io.WriteString(w, "\\+(")
	u.Expr.emit(w)
	io.WriteString(w, ")")
}
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

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, it.Key)
		io.WriteString(w, ": ")
		it.Value.emit(w)
	}
	io.WriteString(w, "}")
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
	if i.IsMap {
		io.WriteString(w, "get_dict(")
		i.Index.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", R)")
	} else if i.IsString {
		io.WriteString(w, "sub_string(")
		i.Target.emit(w)
		io.WriteString(w, ", ")
		i.Index.emit(w)
		io.WriteString(w, ", 1, _, R)")
	} else {
		io.WriteString(w, "nth0(")
		i.Index.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", R)")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	if s.IsString {
		io.WriteString(w, "(Len is ")
		s.End.emit(w)
		io.WriteString(w, " - ")
		s.Start.emit(w)
		io.WriteString(w, ", sub_string(")
		s.Target.emit(w)
		io.WriteString(w, ", ")
		s.Start.emit(w)
		io.WriteString(w, ", Len, _, R))")
	} else {
		io.WriteString(w, "slice_not_supported")
	}
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	i.Cond.emit(w)
	io.WriteString(w, " -> ")
	i.Then.emit(w)
	io.WriteString(w, " ; ")
	i.Else.emit(w)
	io.WriteString(w, ")")
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

func (i *InExpr) emit(w io.Writer) {
	if i.IsMap {
		io.WriteString(w, "get_dict(")
		i.Elem.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", _)")
	} else if i.IsString {
		io.WriteString(w, "sub_string(")
		i.Target.emit(w)
		io.WriteString(w, ", _, _, _, ")
		i.Elem.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "member(")
		i.Elem.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ")")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Name)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
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

func uncap(name string) string {
	if name == "" {
		return ""
	}
	return strings.ToLower(name[:1]) + name[1:]
}

func isBoolOp(op string) bool {
	switch op {
	case "=:=", "=\\=", "=", "\\=", "<", "<=", ">", ">=", "@<", "@=<", "@>", "@>=", ",", ";", "in":
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

func needsIs(e Expr) bool {
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *BinaryExpr:
		return isArithOp(ex.Op) || needsIs(ex.Left) || needsIs(ex.Right)
	case *GroupExpr:
		return needsIs(ex.Expr)
	case *CastExpr:
		return ex.Type == "int"
	case *LenExpr:
		return true
	case *UnaryNot:
		return false
	default:
		return false
	}
}

func isStringLit(e Expr) bool {
	_, ok := e.(*StringLit)
	return ok
}

func isStringLike(e Expr, env *compileEnv) bool {
	if isStringLit(e) {
		return true
	}
	if v, ok := e.(*Var); ok {
		if c, ok2 := env.constExpr(v.Name).(*StringLit); ok2 && c != nil {
			return true
		}
	}
	if _, ok := e.(*StrExpr); ok {
		return true
	}
	if _, ok := e.(*SubstringExpr); ok {
		return true
	}
	if _, ok := e.(*SliceExpr); ok {
		return true
	}
	return false
}

func isMapLike(e Expr, env *compileEnv) bool {
	if _, ok := e.(*MapLit); ok {
		return true
	}
	if v, ok := e.(*Var); ok {
		if _, ok2 := env.constExpr(v.Name).(*MapLit); ok2 {
			return true
		}
	}
	return false
}

func intValue(e Expr) (int, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, true
	case *FloatLit:
		return int(v.Value), true
	case *GroupExpr:
		return intValue(v.Expr)
	}
	return 0, false
}

func cloneList(l *ListLit) *ListLit {
	elems := make([]Expr, len(l.Elems))
	copy(elems, l.Elems)
	return &ListLit{Elems: elems}
}

func updateNestedList(l *ListLit, idx1, idx2 int, val Expr) (*ListLit, bool) {
	if idx1 < 0 || idx1 >= len(l.Elems) {
		return nil, false
	}
	inner, ok := l.Elems[idx1].(*ListLit)
	if !ok {
		return nil, false
	}
	if idx2 < 0 || idx2 >= len(inner.Elems) {
		return nil, false
	}
	newInner := cloneList(inner)
	newInner.Elems[idx2] = val
	newOuter := cloneList(l)
	newOuter.Elems[idx1] = newInner
	return newOuter, true
}

func nextSimpleAssign(sts []*parser.Statement, idx int, name string) bool {
	if idx+1 >= len(sts) {
		return false
	}
	n := sts[idx+1]
	return n.Assign != nil && n.Assign.Name == name && len(n.Assign.Index) == 0 && len(n.Assign.Field) == 0
}

// Transpile converts a Mochi program to a Prolog AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env
	constFuncs := collectConstFuncs(prog)
	ce := newCompileEnv(constFuncs)
	p := &Program{}
	for i, st := range prog.Statements {
		switch {
		case st.Fun != nil:
			if len(st.Fun.Params) == 0 && st.Fun.Return != nil {
				// used for constant folding only
				continue
			}
			fn, err := compileFunction(st.Fun, ce)
			if err != nil {
				return nil, err
			}
			p.Funcs = append(p.Funcs, fn)
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
			ce.setConst(name, expr)
			p.Stmts = append(p.Stmts, &LetStmt{Name: name, Expr: expr})
		case st.Var != nil:
			if nextSimpleAssign(prog.Statements, i, st.Var.Name) {
				continue
			}
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
			ce.setConst(name, expr)
			p.Stmts = append(p.Stmts, &LetStmt{Name: name, Expr: expr})
		case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
			expr, err := toExpr(st.Assign.Value, ce)
			if err != nil {
				return nil, err
			}
			name := ce.fresh(st.Assign.Name)
			ce.setConst(name, expr)
			p.Stmts = append(p.Stmts, &LetStmt{Name: name, Expr: expr})
		case st.Assign != nil && len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil && st.Assign.Index[0].End == nil && st.Assign.Index[0].Step == nil:
			idx, err := toExpr(st.Assign.Index[0].Start, ce)
			if err != nil {
				return nil, err
			}
			val, err := toExpr(st.Assign.Value, ce)
			if err != nil {
				return nil, err
			}
			target := ce.current(st.Assign.Name)
			name := ce.fresh(st.Assign.Name)
			ce.setConst(name, nil)
			p.Stmts = append(p.Stmts, &IndexAssignStmt{Name: name, Target: target, Index: idx, Value: val})
		case st.If != nil:
			cond, err := toExpr(st.If.Cond, ce)
			if err != nil {
				return nil, err
			}
			if st.If.ElseIf != nil {
				return nil, fmt.Errorf("unsupported elseif")
			}
			thenStmts, err := compileStmts(st.If.Then, ce)
			if err != nil {
				return nil, err
			}
			var elseStmts []Stmt
			if st.If.Else != nil {
				elseStmts, err = compileStmts(st.If.Else, ce)
				if err != nil {
					return nil, err
				}
			}
			p.Stmts = append(p.Stmts, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		case st.For != nil:
			loopStmts, err := compileStmts([]*parser.Statement{st}, ce)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, loopStmts...)
		case st.While != nil:
			loopStmts, err := compileStmts([]*parser.Statement{st}, ce)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, loopStmts...)
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil || call.Func != "print" || len(call.Args) != 1 {
				return nil, fmt.Errorf("unsupported expression")
			}
			arg, err := toExpr(call.Args[0], ce)
			if err != nil {
				return nil, err
			}
			if v, ok := arg.(*Var); ok {
				if c := ce.constExpr(v.Name); c != nil {
					arg = c
				}
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
	for _, fn := range p.Funcs {
		fn.emit(w)
	}
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

func compileStmts(sts []*parser.Statement, env *compileEnv) ([]Stmt, error) {
	var out []Stmt
	for i, s := range sts {
		switch {
		case s.Expr != nil:
			call := s.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil || (call.Func != "print" && call.Func != "json") {
				return nil, fmt.Errorf("unsupported expression")
			}
			args := make([]Expr, len(call.Args))
			allConst := true
			var sb strings.Builder
			for i, a := range call.Args {
				ex, err := toExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[i] = ex
				if allConst {
					switch v := ex.(type) {
					case *StringLit:
						sb.WriteString(v.Value)
					case *IntLit:
						sb.WriteString(strconv.Itoa(v.Value))
					case *FloatLit:
						sb.WriteString(strconv.FormatFloat(v.Value, 'f', -1, 64))
					case *BoolLit:
						if v.Value {
							sb.WriteString("true")
						} else {
							sb.WriteString("false")
						}
					default:
						allConst = false
					}
				}
				if i < len(call.Args)-1 && allConst {
					sb.WriteString(" ")
				}
			}
			if call.Func == "json" {
				if len(args) != 1 {
					return nil, fmt.Errorf("unsupported expression")
				}
				arg := args[0]
				if v, ok := arg.(*Var); ok {
					if c := env.constExpr(v.Name); c != nil {
						arg = c
					}
				}
				js, err := jsonString(arg)
				if err != nil {
					return nil, err
				}
				out = append(out, &PrintStmt{Expr: &StringLit{Value: js}})
			} else {
				var stmt Stmt
				if len(args) == 1 {
					arg := args[0]
					if v, ok := arg.(*Var); ok {
						if c := env.constExpr(v.Name); c != nil {
							arg = c
						}
					}
					stmt = &PrintStmt{Expr: arg}
					out = append(out, stmt)
				} else if allConst {
					out = append(out, &PrintStmt{Expr: &StringLit{Value: sb.String()}})
				} else {
					out = append(out, &MultiPrintStmt{Exprs: args})
				}
			}
		case s.Let != nil:
			var expr Expr
			if s.Let.Value != nil {
				var err error
				expr, err = toExpr(s.Let.Value, env)
				if err != nil {
					return nil, err
				}
			} else {
				expr = &IntLit{Value: 0}
			}
			name := env.fresh(s.Let.Name)
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
		case s.Var != nil:
			if nextSimpleAssign(sts, i, s.Var.Name) {
				continue
			}
			var expr Expr
			if s.Var.Value != nil {
				var err error
				expr, err = toExpr(s.Var.Value, env)
				if err != nil {
					return nil, err
				}
			} else {
				expr = &IntLit{Value: 0}
			}
			name := env.fresh(s.Var.Name)
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
		case s.Assign != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0:
			expr, err := toExpr(s.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			name := env.fresh(s.Assign.Name)
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
		case s.Assign != nil && len(s.Assign.Index) == 2 &&
			s.Assign.Index[0].Colon == nil && s.Assign.Index[0].End == nil && s.Assign.Index[0].Step == nil &&
			s.Assign.Index[1].Colon == nil && s.Assign.Index[1].End == nil && s.Assign.Index[1].Step == nil:
			idx1Expr, err := toExpr(s.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			idx2Expr, err := toExpr(s.Assign.Index[1].Start, env)
			if err != nil {
				return nil, err
			}
			idx1, ok1 := intValue(idx1Expr)
			idx2, ok2 := intValue(idx2Expr)
			list, ok3 := env.constExpr(env.current(s.Assign.Name)).(*ListLit)
			if ok1 && ok2 && ok3 {
				val, err := toExpr(s.Assign.Value, env)
				if err != nil {
					return nil, err
				}
				updated, ok := updateNestedList(list, idx1, idx2, val)
				if ok {
					name := env.fresh(s.Assign.Name)
					env.setConst(name, updated)
					out = append(out, &LetStmt{Name: name, Expr: updated})
					// handled
					break
				}
			}
			return nil, fmt.Errorf("unsupported statement")

		case s.Assign != nil && len(s.Assign.Index) == 1 && s.Assign.Index[0].Colon == nil && s.Assign.Index[0].End == nil && s.Assign.Index[0].Step == nil:
			idx, err := toExpr(s.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := toExpr(s.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			target := env.current(s.Assign.Name)
			name := env.fresh(s.Assign.Name)
			env.setConst(name, nil)
			out = append(out, &IndexAssignStmt{Name: name, Target: target, Index: idx, Value: val})
		case s.If != nil:
			cond, err := toExpr(s.If.Cond, env)
			if err != nil {
				return nil, err
			}
			if s.If.ElseIf != nil {
				return nil, fmt.Errorf("unsupported elseif")
			}
			thenStmts, err := compileStmts(s.If.Then, env)
			if err != nil {
				return nil, err
			}
			var elseStmts []Stmt
			if s.If.Else != nil {
				elseStmts, err = compileStmts(s.If.Else, env)
				if err != nil {
					return nil, err
				}
			}
			out = append(out, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		case s.Return != nil:
			if s.Return.Value == nil {
				return nil, fmt.Errorf("unsupported return")
			}
			expr, err := toExpr(s.Return.Value, env)
			if err != nil {
				return nil, err
			}
			name := env.fresh("return")
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
		case s.For != nil:
			if s.For.RangeEnd != nil {
				start, err := toExpr(s.For.Source, env)
				if err != nil {
					return nil, err
				}
				end, err := toExpr(s.For.RangeEnd, env)
				if err != nil {
					return nil, err
				}
				sv, ok1 := intValue(start)
				ev, ok2 := intValue(end)
				if !ok1 || !ok2 {
					return nil, fmt.Errorf("unsupported for-loop")
				}
				for i := sv; i < ev; i++ {
					vname := env.fresh(s.For.Name)
					iv := &IntLit{Value: i}
					env.setConst(vname, iv)
					out = append(out, &LetStmt{Name: vname, Expr: iv})
					body, err := compileStmts(s.For.Body, env)
					if err != nil {
						return nil, err
					}
					out = append(out, body...)
				}
			} else {
				src, err := toExpr(s.For.Source, env)
				if err != nil {
					return nil, err
				}
				var list *ListLit
				var m *MapLit
				switch ex := src.(type) {
				case *ListLit:
					list = ex
				case *MapLit:
					m = ex
				case *Var:
					if c, ok := env.constExpr(ex.Name).(*ListLit); ok {
						list = c
					}
					if c, ok := env.constExpr(ex.Name).(*MapLit); ok {
						m = c
					}
				}
				if list == nil && m == nil {
					return nil, fmt.Errorf("unsupported for-loop")
				}
				if m != nil {
					for _, it := range m.Items {
						vname := env.fresh(s.For.Name)
						lit := &StringLit{Value: it.Key}
						env.setConst(vname, lit)
						out = append(out, &LetStmt{Name: vname, Expr: lit})
						body, err := compileStmts(s.For.Body, env)
						if err != nil {
							return nil, err
						}
						out = append(out, body...)
					}
				} else {
					for _, elem := range list.Elems {
						vname := env.fresh(s.For.Name)
						env.setConst(vname, elem)
						out = append(out, &LetStmt{Name: vname, Expr: elem})
						body, err := compileStmts(s.For.Body, env)
						if err != nil {
							return nil, err
						}
						out = append(out, body...)
					}
				}
			}
		case s.While != nil:
			for {
				condExpr, err := toExpr(s.While.Cond, env)
				if err != nil {
					return nil, err
				}
				b, ok := condExpr.(*BoolLit)
				if !ok {
					return nil, fmt.Errorf("unsupported while")
				}
				if !b.Value {
					break
				}
				body, err := compileStmts(s.While.Body, env)
				if err != nil {
					return nil, err
				}
				out = append(out, body...)
			}
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *PrintStmt:
		return &ast.Node{Kind: "print", Children: []*ast.Node{exprNode(st.Expr)}}
	case *MultiPrintStmt:
		n := &ast.Node{Kind: "printmany"}
		for _, e := range st.Exprs {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		for _, t := range st.Then {
			n.Children = append(n.Children, stmtNode(t))
		}
		for _, e := range st.Else {
			n.Children = append(n.Children, stmtNode(e))
		}
		return n
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
			if isStringLike(left, env) || isStringLike(right, env) {
				opStr = "="
			} else {
				opStr = "=:="
			}
		case "!=":
			if isStringLike(left, env) || isStringLike(right, env) {
				opStr = "\\="
			} else {
				opStr = "=\\="
			}
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
		case "in":
			opStr = "in"
		default:
			return nil, fmt.Errorf("unsupported op")
		}
		if li, lok := left.(*IntLit); lok {
			if ri, rok := right.(*IntLit); rok {
				switch opStr {
				case "+":
					left = &IntLit{Value: li.Value + ri.Value}
					continue
				case "-":
					left = &IntLit{Value: li.Value - ri.Value}
					continue
				case "*":
					left = &IntLit{Value: li.Value * ri.Value}
					continue
				case "mod":
					left = &IntLit{Value: li.Value % ri.Value}
					continue
				case "/":
					left = &FloatLit{Value: float64(li.Value) / float64(ri.Value)}
					continue
				case "=:=", "=":
					left = &BoolLit{Value: li.Value == ri.Value}
					continue
				case "=\\=", "\\=":
					left = &BoolLit{Value: li.Value != ri.Value}
					continue
				case "<":
					left = &BoolLit{Value: li.Value < ri.Value}
					continue
				case "<=":
					left = &BoolLit{Value: li.Value <= ri.Value}
					continue
				case ">":
					left = &BoolLit{Value: li.Value > ri.Value}
					continue
				case ">=":
					left = &BoolLit{Value: li.Value >= ri.Value}
					continue
				}
			}
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
		}
		if opStr == "in" {
			if list, ok := right.(*ListLit); ok {
				if lit, ok2 := left.(*IntLit); ok2 {
					found := false
					for _, e := range list.Elems {
						if li, ok3 := e.(*IntLit); ok3 && li.Value == lit.Value {
							found = true
							break
						}
					}
					left = &BoolLit{Value: found}
					continue
				}
			}
			if mp, ok := right.(*MapLit); ok {
				if s, ok2 := left.(*StringLit); ok2 {
					found := false
					for _, it := range mp.Items {
						if it.Key == s.Value {
							found = true
							break
						}
					}
					left = &BoolLit{Value: found}
					continue
				}
			}
			left = &InExpr{Elem: left, Target: right, IsString: isStringLike(right, env), IsMap: isMapLike(right, env)}
		} else {
			left = &BinaryExpr{Left: left, Op: opStr, Right: right}
		}
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
		case "!":
			if b, ok := expr.(*BoolLit); ok {
				expr = &BoolLit{Value: !b.Value}
			} else {
				expr = &UnaryNot{Expr: expr}
			}
		default:
			return nil, fmt.Errorf("unsupported unary")
		}
	}
	return expr, nil
}

func toPostfix(pf *parser.PostfixExpr, env *compileEnv) (Expr, error) {
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		if len(pf.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("unsupported call")
		}
		arg, err := toExpr(pf.Ops[0].Call.Args[0], env)
		if err != nil {
			return nil, err
		}
		target := &Var{Name: env.current(pf.Target.Selector.Root)}
		return &InExpr{Elem: arg, Target: target, IsString: true}, nil
	}

	expr, err := toPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Step != nil {
				return nil, fmt.Errorf("unsupported slice")
			}
			idxExpr, err := toExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			if op.Index.Colon != nil || op.Index.End != nil {
				endExpr, err := toExpr(op.Index.End, env)
				if err != nil {
					return nil, err
				}
				isStr := isStringLike(expr, env)
				if list, ok := expr.(*ListLit); ok && !isStr {
					if s, ok1 := idxExpr.(*IntLit); ok1 {
						if e, ok2 := endExpr.(*IntLit); ok2 {
							if s.Value >= 0 && e.Value <= len(list.Elems) {
								slice := make([]Expr, e.Value-s.Value)
								copy(slice, list.Elems[s.Value:e.Value])
								expr = &ListLit{Elems: slice}
								continue
							}
						}
					}
				}
				expr = &SliceExpr{Target: expr, Start: idxExpr, End: endExpr, IsString: isStr}
				continue
			}
			if list, ok := expr.(*ListLit); ok {
				if lit, ok2 := idxExpr.(*IntLit); ok2 {
					if lit.Value >= 0 && lit.Value < len(list.Elems) {
						expr = list.Elems[lit.Value]
						continue
					}
				}
			}
			if mp, ok := expr.(*MapLit); ok {
				switch k := idxExpr.(type) {
				case *StringLit:
					found := false
					var val Expr
					for _, it := range mp.Items {
						if it.Key == k.Value {
							found = true
							val = it.Value
							break
						}
					}
					if found {
						expr = val
						continue
					}
				case *IntLit:
					s := strconv.Itoa(k.Value)
					for _, it := range mp.Items {
						if it.Key == s {
							expr = it.Value
							continue
						}
					}
				}
			}
			expr = &IndexExpr{Target: expr, Index: idxExpr, IsString: isStringLike(expr, env), IsMap: isMapLike(expr, env)}
		case op.Cast != nil:
			if op.Cast.Type == nil || op.Cast.Type.Simple == nil {
				return nil, fmt.Errorf("unsupported cast")
			}
			expr = &CastExpr{Expr: expr, Type: *op.Cast.Type.Simple}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ex, err := toExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			if v, ok := expr.(*Var); ok {
				return &CallExpr{Name: v.Name, Args: args}, nil
			}
			return nil, fmt.Errorf("unsupported call")
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
		if p.Lit.Float != nil {
			return &FloatLit{Value: *p.Lit.Float}, nil
		}
		if p.Lit.Bool != nil {
			return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
		}
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return &Var{Name: env.current(p.Selector.Root)}, nil
		}
		if len(p.Selector.Tail) == 1 {
			if c, ok := env.constExpr(env.current(p.Selector.Root)).(*MapLit); ok {
				for _, it := range c.Items {
					if it.Key == p.Selector.Tail[0] {
						return it.Value, nil
					}
				}
			}
			return nil, fmt.Errorf("unsupported selector")
		}
		return nil, fmt.Errorf("unsupported selector")
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
				switch a := arg.(type) {
				case *StringLit:
					return &IntLit{Value: len(a.Value)}, nil
				case *ListLit:
					return &IntLit{Value: len(a.Elems)}, nil
				case *MapLit:
					return &IntLit{Value: len(a.Items)}, nil
				case *Var:
					if c, ok := env.constExpr(a.Name).(*StringLit); ok {
						return &IntLit{Value: len(c.Value)}, nil
					}
					if l, ok := env.constExpr(a.Name).(*ListLit); ok {
						return &IntLit{Value: len(l.Elems)}, nil
					}
					if m, ok := env.constExpr(a.Name).(*MapLit); ok {
						return &IntLit{Value: len(m.Items)}, nil
					}
				}
				if isStringLike(arg, env) {
					return &CallExpr{Name: "string_length", Args: []Expr{arg}}, nil
				}
				return &LenExpr{Value: arg}, nil
			case "str":
				return &StrExpr{Value: arg}, nil
			case "count":
				if l, ok := constList(arg, env); ok {
					return &IntLit{Value: len(l.Elems)}, nil
				}
				return &CountExpr{Value: arg}, nil
			case "sum":
				if l, ok := constList(arg, env); ok {
					sum := 0
					for _, e := range l.Elems {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in sum")
						}
						sum += iv
					}
					return &IntLit{Value: sum}, nil
				}
				return &SumExpr{Value: arg}, nil
			case "avg":
				if l, ok := constList(arg, env); ok && len(l.Elems) > 0 {
					total := 0
					for _, e := range l.Elems {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in avg")
						}
						total += iv
					}
					return &FloatLit{Value: float64(total) / float64(len(l.Elems))}, nil
				}
				return &AvgExpr{Value: arg}, nil
			case "min":
				if l, ok := constList(arg, env); ok && len(l.Elems) > 0 {
					minV, ok := intValue(l.Elems[0])
					if !ok {
						return nil, fmt.Errorf("non-int in min")
					}
					for _, e := range l.Elems[1:] {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in min")
						}
						if iv < minV {
							minV = iv
						}
					}
					return &IntLit{Value: minV}, nil
				}
				return &MinExpr{Value: arg}, nil
			case "max":
				if l, ok := constList(arg, env); ok && len(l.Elems) > 0 {
					maxV, ok := intValue(l.Elems[0])
					if !ok {
						return nil, fmt.Errorf("non-int in max")
					}
					for _, e := range l.Elems[1:] {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in max")
						}
						if iv > maxV {
							maxV = iv
						}
					}
					return &IntLit{Value: maxV}, nil
				}
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
			if len(p.Call.Args) == 0 {
				if ex, ok := env.funcs[p.Call.Func]; ok {
					return ex, nil
				}
			}
			args := make([]Expr, len(p.Call.Args))
			for i, a := range p.Call.Args {
				ex, err := toExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			return &CallExpr{Name: p.Call.Func, Args: args}, nil
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
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			key, err := toExpr(it.Key, env)
			if err != nil {
				return nil, err
			}
			var keyStr string
			switch k := key.(type) {
			case *StringLit:
				keyStr = k.Value
			case *IntLit:
				keyStr = strconv.Itoa(k.Value)
			case *Var:
				// Treat bare identifiers as string keys if they are not variables
				if env.constExpr(k.Name) == nil {
					keyStr = uncap(k.Name)
				} else {
					keyStr = k.Name
				}
			default:
				return nil, fmt.Errorf("unsupported map key")
			}
			val, err := toExpr(it.Value, env)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: keyStr, Value: val}
		}
		return &MapLit{Items: items}, nil
	case p.Query != nil:
		return evalQueryExpr(p.Query, env)
	case p.Group != nil:
		expr, err := toExpr(p.Group, env)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: expr}, nil
	case p.If != nil:
		return toIfExpr(p.If, env)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *FloatLit:
		return &ast.Node{Kind: "float", Value: ex.Value}
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
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			n.Children = append(n.Children, &ast.Node{Kind: "entry", Value: it.Key, Children: []*ast.Node{exprNode(it.Value)}})
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
	case *IfExpr:
		return &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	case *SliceExpr:
		return &ast.Node{Kind: "slice", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Start), exprNode(ex.End)}}
	default:
		return &ast.Node{Kind: "expr"}
	}
}

func compileFunction(fn *parser.FunStmt, env *compileEnv) (*Function, error) {
	fenv := newCompileEnv(env.funcs)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
		fenv.vars[p.Name] = 0
	}
	fenv.vars["return"] = 0
	body, err := compileStmts(fn.Body, fenv)
	if err != nil {
		return nil, err
	}
	ret := fenv.current("return")
	return &Function{Name: fn.Name, Params: params, Body: body, Return: &Var{Name: ret}}, nil
}

func toIfExpr(ifp *parser.IfExpr, env *compileEnv) (Expr, error) {
	cond, err := toExpr(ifp.Cond, env)
	if err != nil {
		return nil, err
	}
	thenExpr, err := toExpr(ifp.Then, env)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ifp.ElseIf != nil {
		elseExpr, err = toIfExpr(ifp.ElseIf, env)
		if err != nil {
			return nil, err
		}
	} else if ifp.Else != nil {
		elseExpr, err = toExpr(ifp.Else, env)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &IntLit{Value: 0}
	}
	if b, ok := cond.(*BoolLit); ok {
		if b.Value {
			return thenExpr, nil
		}
		return elseExpr, nil
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func constList(e Expr, env *compileEnv) (*ListLit, bool) {
	switch v := e.(type) {
	case *ListLit:
		return v, true
	case *Var:
		if c, ok := env.constExpr(v.Name).(*ListLit); ok {
			return c, true
		}
		if g, ok := env.constExpr(v.Name).(*MapLit); ok {
			for _, it := range g.Items {
				if it.Key == "items" {
					if l, ok2 := it.Value.(*ListLit); ok2 {
						return l, true
					}
					break
				}
			}
		}
	}
	return nil, false
}

func evalQueryExpr(q *parser.QueryExpr, env *compileEnv) (Expr, error) {
	if q.Group != nil {
		src, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		list, ok := constList(src, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		groups := make(map[string]*ListLit)
		order := []string{}
		env.vars[q.Var] = 0
		for _, item := range list.Elems {
			env.consts[env.current(q.Var)] = item
			keyEx, err := toExpr(q.Group.Exprs[0], env)
			if err != nil {
				return nil, err
			}
			keyStr, ok := keyEx.(*StringLit)
			if !ok {
				return nil, fmt.Errorf("non-string key")
			}
			gl, ok := groups[keyStr.Value]
			if !ok {
				gl = &ListLit{}
				groups[keyStr.Value] = gl
				order = append(order, keyStr.Value)
			}
			gl.Elems = append(gl.Elems, item)
		}
		type result struct {
			sortKey string
			val     Expr
		}
		results := []result{}
		for _, k := range order {
			keyLit := &StringLit{Value: k}
			g := &MapLit{Items: []MapItem{{Key: "key", Value: keyLit}, {Key: "items", Value: groups[k]}}}
			env.vars[q.Group.Name] = 0
			env.consts[env.current(q.Group.Name)] = g
			if q.Group.Having != nil {
				hv, err := toExpr(q.Group.Having, env)
				if err != nil {
					return nil, err
				}
				hb, ok := hv.(*BoolLit)
				if !ok || !hb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			if q.Group.Having != nil {
				hv, err := toExpr(q.Group.Having, env)
				if err != nil {
					return nil, err
				}
				hb, ok := hv.(*BoolLit)
				if !ok || !hb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			if q.Where != nil {
				wc, err := toExpr(q.Where, env)
				if err != nil {
					return nil, err
				}
				wb, ok := wc.(*BoolLit)
				if !ok || !wb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			val, err := toExpr(q.Select, env)
			if err != nil {
				return nil, err
			}
			sortVal := k
			if q.Sort != nil {
				sv, err := toExpr(q.Sort, env)
				if err != nil {
					return nil, err
				}
				sl, ok := sv.(*StringLit)
				if !ok {
					return nil, fmt.Errorf("non-string sort key")
				}
				sortVal = sl.Value
			}
			delete(env.consts, env.current(q.Group.Name))
			delete(env.vars, q.Group.Name)
			results = append(results, result{sortKey: sortVal, val: val})
		}
		if q.Sort != nil {
			sort.Slice(results, func(i, j int) bool { return results[i].sortKey < results[j].sortKey })
		}
		outElems := make([]Expr, len(results))
		for i, r := range results {
			outElems[i] = r.val
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		return &ListLit{Elems: outElems}, nil
	}
	if len(q.Froms) > 0 && len(q.Joins) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		src, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		first, ok := constList(src, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		type clause struct {
			name string
			list *ListLit
		}
		clauses := []clause{{q.Var, first}}
		for _, f := range q.Froms {
			ex, err := toExpr(f.Src, env)
			if err != nil {
				return nil, err
			}
			l, ok := constList(ex, env)
			if !ok {
				return nil, fmt.Errorf("unsupported query source")
			}
			clauses = append(clauses, clause{f.Var, l})
		}
		out := &ListLit{}
		var iter func(int) error
		iter = func(i int) error {
			if i == len(clauses) {
				if q.Where != nil {
					cond, err := toExpr(q.Where, env)
					if err != nil {
						return err
					}
					b, ok := cond.(*BoolLit)
					if !ok || !b.Value {
						return nil
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return err
				}
				out.Elems = append(out.Elems, val)
				return nil
			}
			cl := clauses[i]
			env.vars[cl.name] = 0
			for _, it := range cl.list.Elems {
				env.consts[env.current(cl.name)] = it
				if err := iter(i + 1); err != nil {
					return err
				}
			}
			delete(env.consts, env.current(cl.name))
			delete(env.vars, cl.name)
			return nil
		}
		if err := iter(0); err != nil {
			return nil, err
		}
		return out, nil
	}

	if len(q.Joins) == 1 && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		j := q.Joins[0]
		leftSrc, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		left, ok := constList(leftSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		rightSrc, err := toExpr(j.Src, env)
		if err != nil {
			return nil, err
		}
		right, ok := constList(rightSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported join source")
		}
		out := &ListLit{}
		env.vars[q.Var] = 0
		env.vars[j.Var] = 0
		for _, l := range left.Elems {
			env.consts[env.current(q.Var)] = l
			for _, r := range right.Elems {
				env.consts[env.current(j.Var)] = r
				cond, err := toExpr(j.On, env)
				if err != nil {
					return nil, err
				}
				b, ok := cond.(*BoolLit)
				if !ok || !b.Value {
					continue
				}
				if q.Where != nil {
					wc, err := toExpr(q.Where, env)
					if err != nil {
						return nil, err
					}
					wb, ok := wc.(*BoolLit)
					if !ok || !wb.Value {
						continue
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return nil, err
				}
				out.Elems = append(out.Elems, val)
			}
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		delete(env.consts, env.current(j.Var))
		delete(env.vars, j.Var)
		return out, nil
	}

	if q.Group != nil && len(q.Joins) == 1 && len(q.Froms) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		j := q.Joins[0]
		leftSrc, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		left, ok := constList(leftSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		rightSrc, err := toExpr(j.Src, env)
		if err != nil {
			return nil, err
		}
		right, ok := constList(rightSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported join source")
		}
		groups := map[string]*ListLit{}
		order := []string{}
		env.vars[q.Var] = 0
		env.vars[j.Var] = 0
		for _, l := range left.Elems {
			env.consts[env.current(q.Var)] = l
			for _, r := range right.Elems {
				env.consts[env.current(j.Var)] = r
				cond, err := toExpr(j.On, env)
				if err != nil {
					return nil, err
				}
				b, ok := cond.(*BoolLit)
				if !ok || !b.Value {
					continue
				}
				keyEx, err := toExpr(q.Group.Exprs[0], env)
				if err != nil {
					return nil, err
				}
				keyLit, ok := keyEx.(*StringLit)
				if !ok {
					return nil, fmt.Errorf("non-string key")
				}
				gl, ok := groups[keyLit.Value]
				if !ok {
					gl = &ListLit{}
					groups[keyLit.Value] = gl
					order = append(order, keyLit.Value)
				}
				item := &MapLit{Items: []MapItem{{Key: q.Var, Value: l}, {Key: j.Var, Value: r}}}
				gl.Elems = append(gl.Elems, item)
			}
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		delete(env.consts, env.current(j.Var))
		delete(env.vars, j.Var)
		results := []Expr{}
		for _, k := range order {
			gMap := &MapLit{Items: []MapItem{{Key: "key", Value: &StringLit{Value: k}}, {Key: "items", Value: groups[k]}}}
			env.vars[q.Group.Name] = 0
			env.consts[env.current(q.Group.Name)] = gMap
			if q.Group.Having != nil {
				hv, err := toExpr(q.Group.Having, env)
				if err != nil {
					return nil, err
				}
				hb, ok := hv.(*BoolLit)
				if !ok || !hb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			if q.Where != nil {
				wc, err := toExpr(q.Where, env)
				if err != nil {
					return nil, err
				}
				wb, ok := wc.(*BoolLit)
				if !ok || !wb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			val, err := toExpr(q.Select, env)
			if err != nil {
				return nil, err
			}
			results = append(results, val)
			delete(env.consts, env.current(q.Group.Name))
			delete(env.vars, q.Group.Name)
		}
		return &ListLit{Elems: results}, nil
	}

	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, fmt.Errorf("unsupported query features")
	}

	src, err := toExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	list, ok := constList(src, env)
	if !ok {
		return nil, fmt.Errorf("unsupported query source")
	}
	out := &ListLit{}
	env.vars[q.Var] = 0
	for _, item := range list.Elems {
		env.consts[env.current(q.Var)] = item
		if q.Where != nil {
			cond, err := toExpr(q.Where, env)
			if err != nil {
				return nil, err
			}
			b, ok := cond.(*BoolLit)
			if !ok || !b.Value {
				continue
			}
		}
		val, err := toExpr(q.Select, env)
		if err != nil {
			return nil, err
		}
		out.Elems = append(out.Elems, val)
	}
	delete(env.consts, env.current(q.Var))
	delete(env.vars, q.Var)
	return out, nil
}

func jsonString(e Expr) (string, error) {
	v, err := toInterface(e)
	if err != nil {
		return "", err
	}
	b, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		return "", err
	}
	return string(b), nil
}

func toInterface(e Expr) (interface{}, error) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, nil
	case *FloatLit:
		return v.Value, nil
	case *BoolLit:
		return v.Value, nil
	case *StringLit:
		return v.Value, nil
	case *ListLit:
		arr := make([]interface{}, len(v.Elems))
		for i, el := range v.Elems {
			val, err := toInterface(el)
			if err != nil {
				return nil, err
			}
			arr[i] = val
		}
		return arr, nil
	case *MapLit:
		m := make(map[string]interface{})
		for _, it := range v.Items {
			val, err := toInterface(it.Value)
			if err != nil {
				return nil, err
			}
			m[it.Key] = val
		}
		return m, nil
	default:
		return nil, fmt.Errorf("unsupported json expr")
	}
}
