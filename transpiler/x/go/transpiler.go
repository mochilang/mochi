//go:build slow

package gotranspiler

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"strings"

	"mochi/ast"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

// Program represents a Go program consisting of a sequence of statements.
type Program struct {
	Stmts      []Stmt
	UseStrings bool
	UseStrconv bool
	UsePrint   bool
	UseSort    bool
	UseMath    bool
}

var (
	usesStrings bool
	usesStrconv bool
	usesPrint   bool
	usesSort    bool
	usesMath    bool
)

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

// PrintStmt prints a value using Go's fmt package with Mochi semantics.
// Arguments may include helper expressions that format values as strings.
type PrintStmt struct{ Args []Expr }

func (p *PrintStmt) emit(w io.Writer) {
	// special handling for values() builtin to match expected output
	if len(p.Args) == 1 {
		if ve, ok := p.Args[0].(*ValuesExpr); ok {
			usesStrings = true
			io.WriteString(w, "fmt.Println(strings.Join(func() []string { list := ")
			ve.emit(w)
			io.WriteString(w, "; out := make([]string, len(list)); for i, v := range list { out[i] = fmt.Sprint(v) }; return out }(), \" \"))")
			return
		}
	}
	io.WriteString(w, "fmt.Println(")
	for i, e := range p.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, ")")
}

type VarDecl struct {
	Name  string
	Type  string
	Value Expr
}

func (v *VarDecl) emit(w io.Writer) {
	switch {
	case v.Value != nil && v.Type != "":
		fmt.Fprintf(w, "var %s %s = ", v.Name, v.Type)
		v.Value.emit(w)
	case v.Value != nil:
		fmt.Fprintf(w, "%s := ", v.Name)
		v.Value.emit(w)
	case v.Type != "":
		fmt.Fprintf(w, "var %s %s", v.Name, v.Type)
	default:
		fmt.Fprintf(w, "var %s", v.Name)
	}
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", a.Name)
	a.Value.emit(w)
}

// IfStmt represents a simple if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break") }

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue") }

type ReturnStmt struct{ Value Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Value != nil {
		fmt.Fprint(w, " ")
		r.Value.emit(w)
	}
}

type ParamDecl struct {
	Name string
	Type string
}

type FuncDecl struct {
	Name   string
	Params []ParamDecl
	Return string
	Body   []Stmt
}

func (fd *FuncDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "func %s(", fd.Name)
	for i, p := range fd.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s %s", p.Name, p.Type)
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if fd.Return != "" {
		fmt.Fprintf(w, " %s", fd.Return)
	}
	fmt.Fprint(w, " {\n")
	for _, st := range fd.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

type FuncLit struct {
	Params []ParamDecl
	Return string
	Body   []Stmt
}

func (fl *FuncLit) emit(w io.Writer) {
	fmt.Fprint(w, "func(")
	for i, p := range fl.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s %s", p.Name, p.Type)
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if fl.Return != "" {
		fmt.Fprintf(w, " %s", fl.Return)
	}
	fmt.Fprint(w, " {\n")
	for _, st := range fl.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (ie *IfExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() any {")
	fmt.Fprint(w, "if ")
	ie.Cond.emit(w)
	fmt.Fprint(w, " { return ")
	ie.Then.emit(w)
	fmt.Fprint(w, " }")
	if ie.Else != nil {
		fmt.Fprint(w, " else { return ")
		ie.Else.emit(w)
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, " }()")
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if ")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	fmt.Fprint(w, " {\n")
	for _, s := range i.Then {
		fmt.Fprint(w, "    ")
		s.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, s := range i.Else {
			fmt.Fprint(w, "    ")
			s.emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "}")
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	fmt.Fprint(w, c.Func)
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) { fmt.Fprintf(w, "%v", f.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprintf(w, "%t", b.Value) }

// NotExpr represents a boolean negation.
type NotExpr struct{ Expr Expr }

func (n *NotExpr) emit(w io.Writer) {
	fmt.Fprint(w, "!")
	n.Expr.emit(w)
}

// MapLit represents a map literal.
type MapLit struct {
	KeyType   string
	ValueType string
	Keys      []Expr
	Values    []Expr
}

func (m *MapLit) emit(w io.Writer) {
	fmt.Fprintf(w, "map[%s]%s{", m.KeyType, m.ValueType)
	for i := range m.Keys {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		m.Keys[i].emit(w)
		fmt.Fprint(w, ": ")
		m.Values[i].emit(w)
	}
	fmt.Fprint(w, "}")
}

// IndexExpr represents `X[i]`.
type IndexExpr struct {
	X     Expr
	Index Expr
}

func (ix *IndexExpr) emit(w io.Writer) {
	ix.X.emit(w)
	fmt.Fprint(w, "[")
	if ix.Index != nil {
		ix.Index.emit(w)
	}
	fmt.Fprint(w, "]")
}

// SliceExpr represents `X[i:j]`.
type SliceExpr struct {
	X     Expr
	Start Expr
	End   Expr
}

func (sx *SliceExpr) emit(w io.Writer) {
	sx.X.emit(w)
	fmt.Fprint(w, "[")
	if sx.Start != nil {
		sx.Start.emit(w)
	}
	fmt.Fprint(w, ":")
	if sx.End != nil {
		sx.End.emit(w)
	}
	fmt.Fprint(w, "]")
}

// RuneSliceExpr represents `[]rune(expr)`.
type RuneSliceExpr struct{ Expr Expr }

func (rs *RuneSliceExpr) emit(w io.Writer) {
	fmt.Fprint(w, "[]rune(")
	rs.Expr.emit(w)
	fmt.Fprint(w, ")")
}

// WhileStmt represents a basic while loop using Go's for syntax.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "for ")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	}
	fmt.Fprint(w, " {\n")
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

// ForRangeStmt represents a numeric for-loop like `for i in a..b {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for %s := ", fr.Name)
	if fr.Start != nil {
		fr.Start.emit(w)
	}
	fmt.Fprintf(w, "; %s < ", fr.Name)
	if fr.End != nil {
		fr.End.emit(w)
	}
	fmt.Fprintf(w, "; %s++ {\n", fr.Name)
	for _, st := range fr.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

// ForEachStmt represents iteration over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	IsMap    bool
}

func (fe *ForEachStmt) emit(w io.Writer) {
	if fe.IsMap {
		fmt.Fprintf(w, "for %s := range ", fe.Name)
	} else {
		fmt.Fprintf(w, "for _, %s := range ", fe.Name)
	}
	if fe.Iterable != nil {
		fe.Iterable.emit(w)
	}
	fmt.Fprint(w, " {\n")
	for _, st := range fe.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

// IndexAssignStmt represents assignment to a list element.
type IndexAssignStmt struct {
	Name  string
	Index Expr
	Value Expr
}

func (ias *IndexAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s[", ias.Name)
	ias.Index.emit(w)
	fmt.Fprint(w, "] = ")
	ias.Value.emit(w)
}

type ListLit struct {
	ElemType string
	Elems    []Expr
}

func (l *ListLit) emit(w io.Writer) {
	if l.ElemType == "" {
		l.ElemType = "any"
	}
	fmt.Fprintf(w, "[]%s{", l.ElemType)
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

type AvgExpr struct{ List Expr }

func (a *AvgExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() float64 { sum := 0; for _, n := range ")
	a.List.emit(w)
	fmt.Fprint(w, " { sum += n }; return float64(sum) / float64(len(")
	a.List.emit(w)
	fmt.Fprint(w, ") ) }()")
}

type SumExpr struct{ List Expr }

func (s *SumExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() int { s := 0; for _, n := range ")
	s.List.emit(w)
	fmt.Fprint(w, " { s += n }; return s }()")
}

type MinExpr struct{ List Expr }

func (m *MinExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() int { if len(")
	m.List.emit(w)
	fmt.Fprint(w, ") == 0 { return 0 }; m := ")
	m.List.emit(w)
	fmt.Fprint(w, "[0]; for _, n := range ")
	m.List.emit(w)
	fmt.Fprint(w, "[1:] { if n < m { m = n } }; return m }()")
}

type MaxExpr struct{ List Expr }

func (m *MaxExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() int { if len(")
	m.List.emit(w)
	fmt.Fprint(w, ") == 0 { return 0 }; m := ")
	m.List.emit(w)
	fmt.Fprint(w, "[0]; for _, n := range ")
	m.List.emit(w)
	fmt.Fprint(w, "[1:] { if n > m { m = n } }; return m }()")
}

// ValuesExpr collects all values from a map.
type ValuesExpr struct {
	Map       Expr
	ValueType string
}

func (v *ValuesExpr) emit(w io.Writer) {
	usesSort = true
	fmt.Fprintf(w, "func() []%s { res := make([]%s, 0, len(", v.ValueType, v.ValueType)
	v.Map.emit(w)
	fmt.Fprint(w, ")) ; for _, val := range ")
	v.Map.emit(w)
	fmt.Fprint(w, " { res = append(res, val) } ; ")
	switch v.ValueType {
	case "int":
		fmt.Fprint(w, "sort.Ints(res); ")
	case "string":
		fmt.Fprint(w, "sort.Strings(res); ")
	default:
		fmt.Fprint(w, "sort.Slice(res, func(i,j int) bool { return fmt.Sprint(res[i]) < fmt.Sprint(res[j]) }); ")
	}
	fmt.Fprint(w, "return res }()")
}

// ListStringExpr converts a list to a string with Mochi style formatting.
type ListStringExpr struct{ List Expr }

func (ls *ListStringExpr) emit(w io.Writer) {
	io.WriteString(w, "func() string { return fmt.Sprint(")
	ls.List.emit(w)
	io.WriteString(w, ") }()")
}

// FloatStringExpr formats a float with a trailing decimal.
type FloatStringExpr struct{ Value Expr }

func (fs *FloatStringExpr) emit(w io.Writer) {
	io.WriteString(w, "func() string { f := ")
	fs.Value.emit(w)
	io.WriteString(w, "; if f == float64(int(f)) { return fmt.Sprint(int(f)) }; return fmt.Sprint(f) }()")
}

type ContainsExpr struct {
	Collection Expr
	Value      Expr
	Kind       string // list, map, or string
	ElemType   string
}

func (c *ContainsExpr) emit(w io.Writer) {
	switch c.Kind {
	case "string":
		usesStrings = true
		fmt.Fprint(w, "strings.Contains(")
		c.Collection.emit(w)
		fmt.Fprint(w, ", ")
		c.Value.emit(w)
		fmt.Fprint(w, ")")
	case "map":
		fmt.Fprint(w, "func() bool { _, ok := ")
		c.Collection.emit(w)
		fmt.Fprint(w, "[")
		c.Value.emit(w)
		fmt.Fprint(w, "]; return ok }()")
	default: // list
		fmt.Fprint(w, "func() bool { for _, v := range ")
		c.Collection.emit(w)
		fmt.Fprint(w, " { if v == ")
		c.Value.emit(w)
		fmt.Fprint(w, " { return true } } ; return false }()")
	}
}

type UnionExpr struct {
	Left, Right Expr
	ElemType    string
}

func (u *UnionExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { m := map[%s]struct{}{}; res := []%s{}; for _, v := range ", u.ElemType, u.ElemType, u.ElemType)
	u.Left.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; !ok { m[v] = struct{}{}; res = append(res, v) } } for _, v := range ")
	u.Right.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; !ok { m[v] = struct{}{}; res = append(res, v) } } return res }()")
}

type UnionAllExpr struct {
	Left, Right Expr
	ElemType    string
}

func (u *UnionAllExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { res := make([]%s, len(", u.ElemType, u.ElemType)
	u.Left.emit(w)
	fmt.Fprint(w, "); copy(res, ")
	u.Left.emit(w)
	fmt.Fprint(w, "); res = append(res, ")
	u.Right.emit(w)
	fmt.Fprint(w, "...); return res }()")
}

type ExceptExpr struct {
	Left, Right Expr
	ElemType    string
}

func (e *ExceptExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { m := map[%s]struct{}{}; ", e.ElemType, e.ElemType)
	fmt.Fprint(w, "for _, v := range ")
	e.Right.emit(w)
	fmt.Fprint(w, " { m[v] = struct{}{} } res := []")
	fmt.Fprint(w, e.ElemType)
	fmt.Fprint(w, "{}; for _, v := range ")
	e.Left.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; !ok { res = append(res, v) } } return res }()")
}

type IntersectExpr struct {
	Left, Right Expr
	ElemType    string
}

func (i *IntersectExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { m := map[%s]struct{}{}; for _, v := range ", i.ElemType, i.ElemType)
	i.Left.emit(w)
	fmt.Fprint(w, " { m[v] = struct{}{} } res := []")
	fmt.Fprint(w, i.ElemType)
	fmt.Fprint(w, "{}; for _, v := range ")
	i.Right.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; ok { res = append(res, v) } } return res }()")
}

type AtoiExpr struct{ Expr Expr }

func (a *AtoiExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() int { n, _ := strconv.Atoi(")
	a.Expr.emit(w)
	fmt.Fprint(w, "); return n }()")
}

// Transpile converts a Mochi program to a minimal Go AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	usesStrings = false
	usesStrconv = false
	usesPrint = false
	usesSort = false
	gp := &Program{}
	for _, stmt := range p.Statements {
		s, err := compileStmt(stmt, env)
		if err != nil {
			return nil, err
		}
		if s != nil {
			gp.Stmts = append(gp.Stmts, s)
		}
	}
	_ = env // reserved for future use
	gp.UseStrings = usesStrings
	gp.UseStrconv = usesStrconv
	gp.UsePrint = usesPrint
	gp.UseSort = usesSort
	gp.UseMath = usesMath
	return gp, nil
}

func compileExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	if e.Binary != nil {
		return compileBinary(e.Binary, env)
	}
	return nil, fmt.Errorf("unsupported expression")
}

func compileStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Expr != nil:
		if call := extractCall(st.Expr.Expr); call != nil && call.Func == "print" {
			args := make([]Expr, len(call.Args))
			for i, a := range call.Args {
				ex, err := compileExpr(a, env)
				if err != nil {
					return nil, err
				}
				if _, ok := ex.(*ValuesExpr); ok {
					usesStrings = true
					usesSort = true
				} else {
					t := types.TypeOfExpr(a, env)
					switch t.(type) {
					case types.ListType:
						ex = &ListStringExpr{List: ex}
					case types.FloatType:
						ex = &FloatStringExpr{Value: ex}
					}
				}
				args[i] = ex
			}
			usesPrint = true
			return &PrintStmt{Args: args}, nil
		}
		e, err := compileExpr(st.Expr.Expr, env)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var typ string
		if st.Let.Type != nil {
			typ = toGoType(st.Let.Type)
		} else if t, err := env.GetVar(st.Let.Name); err == nil {
			typ = toGoTypeFromType(t)
		}
		if st.Let.Value != nil {
			e, err := compileExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
			if ml, ok := e.(*MapLit); ok && ml.KeyType == "any" {
				if t, err := env.GetVar(st.Let.Name); err == nil {
					if mt, ok2 := t.(types.MapType); ok2 {
						ml.KeyType = toGoTypeFromType(mt.Key)
						ml.ValueType = toGoTypeFromType(mt.Value)
					}
				}
			}
			if typ == "" {
				t := types.TypeOfExpr(st.Let.Value, env)
				if types.IsAnyType(t) {
					t = types.TypeOfExprBasic(st.Let.Value, env)
				}
				typ = toGoTypeFromType(t)
				if _, ok := t.(types.FuncType); ok {
					typ = ""
				}
			}
			return &VarDecl{Name: st.Let.Name, Type: typ, Value: e}, nil
		}
		return &VarDecl{Name: st.Let.Name, Type: typ}, nil
	case st.Var != nil:
		var typ string
		if st.Var.Type != nil {
			typ = toGoType(st.Var.Type)
		} else if t, err := env.GetVar(st.Var.Name); err == nil {
			typ = toGoTypeFromType(t)
		}
		if st.Var.Value != nil {
			e, err := compileExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
			if ml, ok := e.(*MapLit); ok && ml.KeyType == "any" {
				if t, err := env.GetVar(st.Var.Name); err == nil {
					if mt, ok2 := t.(types.MapType); ok2 {
						ml.KeyType = toGoTypeFromType(mt.Key)
						ml.ValueType = toGoTypeFromType(mt.Value)
					}
				}
			}
			if typ == "" {
				t := types.TypeOfExpr(st.Var.Value, env)
				if types.IsAnyType(t) {
					t = types.TypeOfExprBasic(st.Var.Value, env)
				}
				typ = toGoTypeFromType(t)
				if _, ok := t.(types.FuncType); ok {
					typ = ""
				}
			}
			return &VarDecl{Name: st.Var.Name, Type: typ, Value: e}, nil
		}
		return &VarDecl{Name: st.Var.Name, Type: typ}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil && st.Assign.Index[0].Colon2 == nil && len(st.Assign.Field) == 0 {
			idx, err := compileExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Name: st.Assign.Name, Index: idx, Value: val}, nil
		}
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			e, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: st.Assign.Name, Value: e}, nil
		}
		return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
	case st.If != nil:
		return compileIfStmt(st.If, env)
	case st.While != nil:
		return compileWhileStmt(st.While, env)
	case st.For != nil:
		return compileForStmt(st.For, env)
	case st.Return != nil:
		return compileReturnStmt(st.Return, env)
	case st.ExternVar != nil:
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.Fun != nil:
		return compileFunStmt(st.Fun, env)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		if st.Test == nil && st.Import == nil && st.Type == nil {
			return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
		}
	}
	return nil, nil
}

func compileStmts(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := compileStmt(s, env)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func extractCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return nil
	}
	return u.Value.Target.Call
}

func compileIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := compileExpr(is.Cond, env)
	if err != nil {
		return nil, err
	}
	thenStmts, err := compileStmts(is.Then, env)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		elseStmt, err := compileIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{elseStmt}
	} else if len(is.Else) > 0 {
		elseStmts, err = compileStmts(is.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
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
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func compileMatchExpr(me *parser.MatchExpr, env *types.Env) (Expr, error) {
	target, err := compileExpr(me.Target, env)
	if err != nil {
		return nil, err
	}
	var expr Expr = &VarRef{Name: "nil"}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := compileExpr(c.Result, env)
		if err != nil {
			return nil, err
		}
		pat, err := compileExpr(c.Pattern, env)
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*VarRef); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		expr = &IfExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func compileWhileStmt(ws *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := compileExpr(ws.Cond, env)
	if err != nil {
		return nil, err
	}
	body, err := compileStmts(ws.Body, env)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func compileForStmt(fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := compileExpr(fs.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := compileExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		body, err := compileStmts(fs.Body, env)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := compileExpr(fs.Source, env)
	if err != nil {
		return nil, err
	}
	body, err := compileStmts(fs.Body, env)
	if err != nil {
		return nil, err
	}
	t := types.TypeOfExpr(fs.Source, env)
	_, isMap := t.(types.MapType)
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body, IsMap: isMap}, nil
}

func compileReturnStmt(rs *parser.ReturnStmt, env *types.Env) (Stmt, error) {
	if rs == nil {
		return &ReturnStmt{}, nil
	}
	if rs.Value == nil {
		return &ReturnStmt{}, nil
	}
	val, err := compileExpr(rs.Value, env)
	if err != nil {
		return nil, err
	}
	return &ReturnStmt{Value: val}, nil
}

func compileFunStmt(fn *parser.FunStmt, env *types.Env) (Stmt, error) {
	child := types.NewEnv(env)
	body, err := compileStmts(fn.Body, child)
	if err != nil {
		return nil, err
	}
	params := make([]ParamDecl, len(fn.Params))
	for i, p := range fn.Params {
		typ := toGoType(p.Type)
		if typ == "" {
			if t, err := child.GetVar(p.Name); err == nil {
				typ = toGoTypeFromType(t)
			}
		}
		params[i] = ParamDecl{Name: p.Name, Type: typ}
	}
	ret := toGoType(fn.Return)
	if ret == "" {
		if t, err := child.GetVar(fn.Name); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				ret = toGoTypeFromType(ft.Return)
			}
		}
	}
	return &FuncDecl{Name: fn.Name, Params: params, Return: ret, Body: body}, nil
}

func compileFunExpr(fn *parser.FunExpr, env *types.Env) (Expr, error) {
	child := types.NewEnv(env)
	var body []*parser.Statement
	var stmts []Stmt
	var err error
	if fn.BlockBody != nil {
		body = fn.BlockBody
		stmts, err = compileStmts(body, child)
		if err != nil {
			return nil, err
		}
	} else if fn.ExprBody != nil {
		ex, err := compileExpr(fn.ExprBody, child)
		if err != nil {
			return nil, err
		}
		stmts = []Stmt{&ReturnStmt{Value: ex}}
	}
	params := make([]ParamDecl, len(fn.Params))
	for i, p := range fn.Params {
		typ := toGoType(p.Type)
		if typ == "" {
			if t, err := child.GetVar(p.Name); err == nil {
				typ = toGoTypeFromType(t)
			}
		}
		params[i] = ParamDecl{Name: p.Name, Type: typ}
	}
	ret := toGoType(fn.Return)
	return &FuncLit{Params: params, Return: ret, Body: stmts}, nil
}

func compileBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	first, err := compileUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	firstType := types.TypeOfUnary(b.Left, env)
	operands := []Expr{first}
	typesList := []types.Type{firstType}
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		expr, err := compilePostfix(op.Right, env)
		if err != nil {
			return nil, err
		}
		ops[i] = op
		operands = append(operands, expr)
		typesList = append(typesList, types.TypeOfPostfix(op.Right, env))
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
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
			opName := ops[i].Op
			if opName == "union" && ops[i].All {
				opName = "union_all"
			}
			if contains(level, opName) {
				left := operands[i]
				right := operands[i+1]
				var newExpr Expr
				switch opName {
				case "in":
					ctype := typesList[i+1]
					var kind, et string
					switch ct := ctype.(type) {
					case types.StringType:
						kind = "string"
					case types.MapType:
						kind = "map"
						et = toGoTypeFromType(ct.Key)
					case types.ListType:
						kind = "list"
						et = toGoTypeFromType(ct.Elem)
					default:
						kind = "list"
					}
					if kind == "string" {
						usesStrings = true
					}
					newExpr = &ContainsExpr{Collection: right, Value: left, Kind: kind, ElemType: et}
				case "union":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &UnionExpr{Left: left, Right: right, ElemType: et}
				case "union_all":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &UnionAllExpr{Left: left, Right: right, ElemType: et}
				case "except":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &ExceptExpr{Left: left, Right: right, ElemType: et}
				case "intersect":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &IntersectExpr{Left: left, Right: right, ElemType: et}
				default:
					newExpr = &BinaryExpr{Left: left, Op: ops[i].Op, Right: right}
				}
				operands[i] = newExpr
				operands = append(operands[:i+1], operands[i+2:]...)
				typesList[i] = typesList[i+1]
				typesList = append(typesList[:i+1], typesList[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	return operands[0], nil
}

func compileUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := compilePostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
		case "!":
			expr = &NotExpr{Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func compilePostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := compilePrimary(pf.Target, env)
	if err != nil {
		// allow selector with tail handled here
		if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
			expr = &VarRef{Name: pf.Target.Selector.Root}
		} else {
			return nil, err
		}
	}
	tail := []string{}
	if pf.Target != nil && pf.Target.Selector != nil {
		tail = pf.Target.Selector.Tail
	}
	// handle simple selector access or call (e.g. pkg.Func or pkg.Const)
	if v, ok := expr.(*VarRef); ok && len(tail) == 1 {
		sel := tail[0]
		if v.Name == "math" {
			usesMath = true
			if len(sel) > 0 {
				sel = strings.ToUpper(sel[:1]) + sel[1:]
			}
		}
		name := v.Name + "." + sel
		if len(pf.Ops) == 0 {
			return &VarRef{Name: name}, nil
		}
		if len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
			args := make([]Expr, len(pf.Ops[0].Call.Args))
			for i, a := range pf.Ops[0].Call.Args {
				ex, err := compileExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			if v.Name == "math" {
				usesMath = true
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
	}
	// if tail has one element and first op is CallOp => method call
	if len(tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		method := tail[0]
		args := make([]Expr, len(pf.Ops[0].Call.Args))
		for i, a := range pf.Ops[0].Call.Args {
			ex, err := compileExpr(a, env)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		switch method {
		case "contains":
			rec := pf.Target
			if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
				rec = &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
			}
			mtype := types.TypeOfPrimary(rec, env)
			var kind, et string
			switch mt := mtype.(type) {
			case types.StringType:
				kind = "string"
			case types.MapType:
				kind = "map"
				et = toGoTypeFromType(mt.Key)
			case types.ListType:
				kind = "list"
				et = toGoTypeFromType(mt.Elem)
			default:
				kind = "list"
			}
			if kind == "string" {
				usesStrings = true
			}
			return &ContainsExpr{Collection: expr, Value: args[0], Kind: kind, ElemType: et}, nil
		default:
			return nil, fmt.Errorf("unsupported method %s", method)
		}
	}
	t := types.TypeOfPrimaryBasic(pf.Target, env)
	for _, op := range pf.Ops {
		if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil && idx.Colon2 == nil {
				if idx.Start == nil {
					return nil, fmt.Errorf("unsupported index")
				}
				iex, err := compileExpr(idx.Start, env)
				if err != nil {
					return nil, err
				}
				switch tt := t.(type) {
				case types.StringType:
					expr = &CallExpr{Func: "string", Args: []Expr{&IndexExpr{X: &RuneSliceExpr{Expr: expr}, Index: iex}}}
					t = types.StringType{}
				case types.ListType:
					expr = &IndexExpr{X: expr, Index: iex}
					t = tt.Elem
				case types.MapType:
					expr = &IndexExpr{X: expr, Index: iex}
					t = tt.Value
				default:
					expr = &IndexExpr{X: expr, Index: iex}
					t = types.AnyType{}
				}
			} else {
				var start, end Expr
				if idx.Start != nil {
					start, err = compileExpr(idx.Start, env)
					if err != nil {
						return nil, err
					}
				}
				if idx.End != nil {
					end, err = compileExpr(idx.End, env)
					if err != nil {
						return nil, err
					}
				}
				switch tt := t.(type) {
				case types.StringType:
					expr = &CallExpr{Func: "string", Args: []Expr{&SliceExpr{X: &RuneSliceExpr{Expr: expr}, Start: start, End: end}}}
					t = types.StringType{}
				case types.ListType:
					expr = &SliceExpr{X: expr, Start: start, End: end}
					t = types.ListType{Elem: tt.Elem}
				default:
					expr = &SliceExpr{X: expr, Start: start, End: end}
					t = types.AnyType{}
				}
			}
		} else if op.Call != nil {
			return nil, fmt.Errorf("unsupported call")
		} else if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int" {
				usesStrconv = true
				expr = &AtoiExpr{Expr: expr}
			} else {
				return nil, fmt.Errorf("unsupported postfix")
			}
		} else if op.Field != nil {
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
		case "avg":
			return &AvgExpr{List: args[0]}, nil
		case "count":
			name = "len"
		case "str":
			name = "fmt.Sprint"
		case "sum":
			return &SumExpr{List: args[0]}, nil
		case "min":
			return &MinExpr{List: args[0]}, nil
		case "max":
			return &MaxExpr{List: args[0]}, nil
		case "values":
			mt, _ := types.TypeOfExpr(p.Call.Args[0], env).(types.MapType)
			return &ValuesExpr{Map: args[0], ValueType: toGoTypeFromType(mt.Value)}, nil
		case "substring":
			return &CallExpr{Func: "string", Args: []Expr{&SliceExpr{X: &RuneSliceExpr{Expr: args[0]}, Start: args[1], End: args[2]}}}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := compileExpr(e, env)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		lt, _ := types.TypeOfPrimary(p, env).(types.ListType)
		elemType := toGoTypeFromType(lt.Elem)
		return &ListLit{ElemType: elemType, Elems: elems}, nil
	case p.Map != nil:
		keys := make([]Expr, len(p.Map.Items))
		vals := make([]Expr, len(p.Map.Items))
		for i, it := range p.Map.Items {
			ke, err := compileExpr(it.Key, env)
			if err != nil {
				return nil, err
			}
			ve, err := compileExpr(it.Value, env)
			if err != nil {
				return nil, err
			}
			keys[i] = ke
			vals[i] = ve
		}
		mt, _ := types.TypeOfPrimaryBasic(p, env).(types.MapType)
		return &MapLit{KeyType: toGoTypeFromType(mt.Key), ValueType: toGoTypeFromType(mt.Value), Keys: keys, Values: vals}, nil
	case p.Struct != nil:
		keys := make([]Expr, len(p.Struct.Fields))
		vals := make([]Expr, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			ve, err := compileExpr(f.Value, env)
			if err != nil {
				return nil, err
			}
			keys[i] = &StringLit{Value: f.Name}
			vals[i] = ve
		}
		return &MapLit{KeyType: "string", ValueType: "any", Keys: keys, Values: vals}, nil
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Float != nil {
			return &FloatLit{Value: *p.Lit.Float}, nil
		}
		if p.Lit.Bool != nil {
			return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
	case p.If != nil:
		return compileIfExpr(p.If, env)
	case p.Group != nil:
		return compileExpr(p.Group, env)
	case p.Match != nil:
		return compileMatchExpr(p.Match, env)
	case p.FunExpr != nil:
		return compileFunExpr(p.FunExpr, env)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func toGoType(t *parser.TypeRef) string {
	if t == nil || t.Simple == nil {
		return ""
	}
	switch *t.Simple {
	case "int":
		return "int"
	case "string":
		return "string"
	case "bool":
		return "bool"
	}
	return "any"
}

func toGoTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "int"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.BigRatType, types.FloatType:
		return "float64"
	case types.ListType:
		return "[]" + toGoTypeFromType(tt.Elem)
	case types.MapType:
		return fmt.Sprintf("map[%s]%s", toGoTypeFromType(tt.Key), toGoTypeFromType(tt.Value))
	case types.StructType:
		return "map[string]any"
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = toGoTypeFromType(p)
		}
		ret := toGoTypeFromType(tt.Return)
		if ret != "" && ret != "any" {
			return fmt.Sprintf("func(%s) %s", strings.Join(params, ", "), ret)
		}
		return fmt.Sprintf("func(%s)", strings.Join(params, ", "))
	}
	return "any"
}

func isBoolExpr(e *parser.Expr) bool { return isBoolBinary(e.Binary) }

func isBoolBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return isBoolUnary(b.Left)
	}
	for _, op := range b.Right {
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return true
		}
	}
	return isBoolUnary(b.Left)
}

func isBoolUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	for _, op := range u.Ops {
		if op == "!" {
			return true
		}
	}
	return isBoolPostfix(u.Value)
}

func isBoolPostfix(pf *parser.PostfixExpr) bool {
	if pf == nil || len(pf.Ops) > 0 {
		return false
	}
	return isBoolPrimary(pf.Target)
}

func isBoolPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Bool != nil:
		return true
	case p.Group != nil:
		return isBoolExpr(p.Group)
	default:
		return false
	}
}

func isListExpr(e *parser.Expr) bool { return isListBinary(e.Binary) }

func isListBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return isListUnary(b.Left)
	}
	for _, op := range b.Right {
		switch op.Op {
		case "union", "union_all", "except", "intersect":
			return true
		}
	}
	return isListUnary(b.Left)
}

func isListUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isListPostfix(u.Value)
}

func isListPostfix(pf *parser.PostfixExpr) bool {
	if pf == nil {
		return false
	}
	if len(pf.Ops) > 0 {
		for _, op := range pf.Ops {
			if op.Index != nil && (op.Index.Colon != nil || op.Index.Colon2 != nil) {
				return true
			}
		}
	}
	return isListPrimary(pf.Target)
}

func isListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.List != nil:
		return true
	case p.Call != nil:
		switch p.Call.Func {
		case "append", "union", "union_all", "except", "intersect", "slice":
			return true
		}
	}
	return false
}

// Emit formats the Go AST back into source code.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("//go:build ignore\n\n")
	buf.Write(meta.Header("//"))
	buf.WriteString("package main\n\n")
	buf.WriteString("import (\n    \"fmt\"\n")
	if prog.UseStrings {
		buf.WriteString("    \"strings\"\n")
	}
	if prog.UseStrconv {
		buf.WriteString("    \"strconv\"\n")
	}
	if prog.UseMath {
		buf.WriteString("    \"math\"\n")
	}
	if prog.UseSort {
		buf.WriteString("    \"sort\"\n")
	}
	buf.WriteString(")\n\n")

	// no runtime helper functions needed
	for _, s := range prog.Stmts {
		if _, ok := s.(*FuncDecl); ok {
			s.emit(&buf)
			buf.WriteString("\n\n")
		}
	}
	buf.WriteString("func main() {\n")
	for _, s := range prog.Stmts {
		if _, ok := s.(*FuncDecl); ok {
			continue
		}
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("}\n")
	out, err := format.Source(buf.Bytes())
	if err == nil {
		return out
	}
	return buf.Bytes()
}

// print converts prog to an ast.Node and prints it.
func print(prog *Program) {
	node := toNodeProg(prog)
	fmt.Print(node.String())
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
	case *PrintStmt:
		n := &ast.Node{Kind: "print"}
		for _, a := range st.Args {
			n.Children = append(n.Children, toNodeExpr(a))
		}
		return n
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{toNodeExpr(st.Expr)}}
	case *VarDecl:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		then := &ast.Node{Kind: "then"}
		for _, t := range st.Then {
			then.Children = append(then.Children, toNodeStmt(t))
		}
		n.Children = append(n.Children, then)
		if len(st.Else) > 0 {
			els := &ast.Node{Kind: "else"}
			for _, e := range st.Else {
				els.Children = append(els.Children, toNodeStmt(e))
			}
			n.Children = append(n.Children, els)
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
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p.Name})
		}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, params, body)
		return n
	case *IndexAssignStmt:
		return &ast.Node{Kind: "indexassign", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Index), toNodeExpr(st.Value)}}
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
	case *IntLit:
		return &ast.Node{Kind: "int"}
	case *BoolLit:
		return &ast.Node{Kind: "bool"}
	case *VarRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, toNodeExpr(e))
		}
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{toNodeExpr(ex.X), toNodeExpr(ex.Index)}}
	case *SliceExpr:
		n := &ast.Node{Kind: "slice"}
		n.Children = append(n.Children, toNodeExpr(ex.X))
		if ex.Start != nil {
			n.Children = append(n.Children, toNodeExpr(ex.Start))
		}
		if ex.End != nil {
			n.Children = append(n.Children, toNodeExpr(ex.End))
		}
		return n
	case *RuneSliceExpr:
		return &ast.Node{Kind: "runes", Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{toNodeExpr(ex.Map)}}
	case *ListStringExpr:
		return &ast.Node{Kind: "liststr", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *FloatStringExpr:
		return &ast.Node{Kind: "floatstr", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *ContainsExpr:
		return &ast.Node{Kind: "contains", Children: []*ast.Node{toNodeExpr(ex.Collection), toNodeExpr(ex.Value)}}
	case *UnionExpr:
		return &ast.Node{Kind: "union", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *UnionAllExpr:
		return &ast.Node{Kind: "unionall", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *ExceptExpr:
		return &ast.Node{Kind: "except", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *IntersectExpr:
		return &ast.Node{Kind: "intersect", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *AtoiExpr:
		return &ast.Node{Kind: "atoi", Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *IfExpr:
		n := &ast.Node{Kind: "ifexpr"}
		n.Children = append(n.Children, toNodeExpr(ex.Cond), toNodeExpr(ex.Then))
		if ex.Else != nil {
			n.Children = append(n.Children, toNodeExpr(ex.Else))
		}
		return n
	case *NotExpr:
		return &ast.Node{Kind: "not", Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *FuncLit:
		n := &ast.Node{Kind: "funclit"}
		params := &ast.Node{Kind: "params"}
		for _, p := range ex.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p.Name})
		}
		body := &ast.Node{Kind: "body"}
		for _, b := range ex.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, params, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
