//go:build slow

package scalat

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"mochi/ast"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

var typeDecls []*TypeDeclStmt

// Program represents a simple Scala program consisting of statements in main.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type Param struct {
	Name string
	Type string
}

type FunStmt struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
}

type ReturnStmt struct{ Value Expr }

// WhileStmt represents `while cond { ... }` loops.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents `for i in a..b { ... }` loops.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForEachStmt represents `for x in list { ... }` loops.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break") }

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue") }

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

type FunExpr struct {
	Params []Param
	Expr   Expr
}

type MatchCase struct {
	Pattern Expr
	Result  Expr
}

type MatchExpr struct {
	Target Expr
	Cases  []MatchCase
}

func (m *MatchExpr) emit(w io.Writer) {
	m.Target.emit(w)
	fmt.Fprint(w, " match {")
	for _, c := range m.Cases {
		fmt.Fprint(w, " case ")
		c.Pattern.emit(w)
		fmt.Fprint(w, " => ")
		c.Result.emit(w)
	}
	fmt.Fprint(w, " }")
}

type LetStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "val %s", s.Name)
	if s.Type != "" {
		fmt.Fprintf(w, ": %s", s.Type)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

type VarStmt struct {
	Name  string
	Type  string
	Value Expr
}

// TypeDeclStmt represents a case class declaration.
type TypeDeclStmt struct {
	Name   string
	Fields []Param
}

func (t *TypeDeclStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "case class %s(", t.Name)
	for i, f := range t.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := f.Type
		if typ == "" {
			typ = "Any"
		}
		fmt.Fprintf(w, "%s: %s", f.Name, typ)
	}
	fmt.Fprint(w, ")")
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s", s.Name)
	if s.Type != "" {
		fmt.Fprintf(w, ": %s", s.Type)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

// AssignStmt represents `target = value` assignments.
type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	s.Target.emit(w)
	fmt.Fprint(w, " = ")
	s.Value.emit(w)
}

func (f *FunStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "def %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if f.Return != "" {
		fmt.Fprintf(w, ": %s", f.Return)
	}
	fmt.Fprint(w, " = {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Value != nil {
		fmt.Fprint(w, " ")
		r.Value.emit(w)
	}
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while (")
	ws.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (%s <- ", fr.Name)
	fr.Start.emit(w)
	fmt.Fprint(w, " until ")
	fr.End.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range fr.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (fe *ForEachStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (%s <- ", fe.Name)
	fe.Iterable.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range fe.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	i.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range i.Then {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, st := range i.Else {
			fmt.Fprint(w, "    ")
			st.emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "  }")
	}
}

func (ie *IfExpr) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	ie.Cond.emit(w)
	fmt.Fprint(w, ") ")
	ie.Then.emit(w)
	fmt.Fprint(w, " else ")
	ie.Else.emit(w)
}

func (f *FunExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	fmt.Fprint(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			fmt.Fprintf(w, "%s: Any", p.Name)
		}
	}
	fmt.Fprint(w, ") => ")
	if f.Expr != nil {
		f.Expr.emit(w)
	}
	fmt.Fprint(w, ")")
}

// CallExpr represents calling a function expression with arguments.
type CallExpr struct {
	Fn   Expr
	Args []Expr
}

// LenExpr represents len(x) which becomes x.length in Scala.
type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	fmt.Fprint(w, ".size")
}

func (c *CallExpr) emit(w io.Writer) {
	c.Fn.emit(w)
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

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprintf(w, "%t", b.Value) }

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) { fmt.Fprintf(w, "%g", f.Value) }

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) { fmt.Fprint(w, n.Name) }

// ListLit represents a mutable list using ArrayBuffer.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "ArrayBuffer(")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, ")")
}

// MapEntry represents a key/value pair inside a map literal.
type MapEntry struct {
	Key   Expr
	Value Expr
}

// MapLit represents a simple map literal using Scala's Map.
type MapLit struct{ Items []MapEntry }

func (m *MapLit) emit(w io.Writer) {
	fmt.Fprint(w, "Map(")
	for i, it := range m.Items {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		it.Key.emit(w)
		fmt.Fprint(w, " -> ")
		it.Value.emit(w)
	}
	fmt.Fprint(w, ")")
}

// StructLit represents constructing a case class value.
type StructLit struct {
	Name   string
	Fields []Expr
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", s.Name)
	for i, f := range s.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		f.emit(w)
	}
	fmt.Fprint(w, ")")
}

// AppendExpr represents append(list, elem) as `list :+ elem`.
type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	fmt.Fprint(w, " :+ ")
	a.Elem.emit(w)
}

// IndexExpr represents x[i] which becomes x(i) in Scala.
type IndexExpr struct {
	Value Expr
	Index Expr
}

func (idx *IndexExpr) emit(w io.Writer) {
	idx.Value.emit(w)
	fmt.Fprint(w, "(")
	idx.Index.emit(w)
	fmt.Fprint(w, ")")
}

// SliceExpr represents x[a:b] which becomes x.slice(a, b).
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Value.emit(w)
	fmt.Fprint(w, ".slice(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// CastExpr represents value as type conversions like `"123" as int`.
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	c.Value.emit(w)
	switch c.Type {
	case "int":
		fmt.Fprint(w, ".toInt")
	case "float":
		fmt.Fprint(w, ".toDouble")
	case "string":
		fmt.Fprint(w, ".toString")
	}
}

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Receiver Expr
	Name     string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Receiver.emit(w)
	fmt.Fprintf(w, ".%s", f.Name)
}

// SubstringExpr represents substring(s, i, j) which becomes s.substring(i, j).
type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Value.emit(w)
	fmt.Fprint(w, ".substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// UnaryExpr represents prefix unary operations like !x.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	u.Expr.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
}

type queryFrom struct {
	Var string
	Src Expr
}

type queryJoin struct {
	Var  string
	Src  Expr
	Cond Expr
}

// GroupByExpr represents a query with grouping support.
type GroupByExpr struct {
	Var      string
	Source   Expr
	Key      Expr
	Name     string
	Where    Expr
	Select   Expr
	Having   Expr
	Sort     Expr
	SortType string
	Froms    []queryFrom
	Joins    []queryJoin
	ElemType string
}

type QueryExpr struct {
	Var      string
	Src      Expr
	Froms    []queryFrom
	Where    Expr
	Sort     Expr
	SortType string
	Skip     Expr
	Take     Expr
	Select   Expr
	Distinct bool
	ElemType string
}

// RightJoinExpr represents a simple right join query of two sources.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
	ElemType string
}

// LeftJoinExpr represents a simple left join query of two sources.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
	ElemType string
}

func (q *QueryExpr) emit(w io.Writer) {
	et := q.ElemType
	if et == "" {
		et = "Any"
	}
	if q.Sort == nil && !q.Distinct && q.Skip == nil && q.Take == nil && len(q.Froms) == 0 {
		fmt.Fprintf(w, "(for (%s <- ", q.Var)
		q.Src.emit(w)
		fmt.Fprint(w, ")")
		if q.Where != nil {
			fmt.Fprint(w, " if (")
			q.Where.emit(w)
			fmt.Fprint(w, ")")
		}
		fmt.Fprint(w, " yield ")
		q.Select.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	if q.Sort != nil {
		st := q.SortType
		if st == "" {
			st = "Any"
		}
		fmt.Fprintf(w, "({ var _tmp = ArrayBuffer[(%s,%s)]() ; for (%s <- ", st, et, q.Var)
	} else {
		fmt.Fprintf(w, "({ var _res = ArrayBuffer[%s]() ; for (%s <- ", et, q.Var)
	}
	q.Src.emit(w)
	fmt.Fprint(w, ") {")
	for _, f := range q.Froms {
		fmt.Fprintf(w, " for (%s <- ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, ") {")
	}
	if q.Where != nil {
		fmt.Fprint(w, " if (")
		q.Where.emit(w)
		fmt.Fprint(w, ") {")
	}
	if q.Sort != nil {
		fmt.Fprint(w, " _tmp.append((")
		q.Sort.emit(w)
		fmt.Fprint(w, ", ")
		q.Select.emit(w)
		fmt.Fprint(w, "))")
	} else {
		fmt.Fprint(w, " _res.append(")
		q.Select.emit(w)
		fmt.Fprint(w, ")")
	}
	if q.Where != nil {
		fmt.Fprint(w, " }")
	}
	for range q.Froms {
		fmt.Fprint(w, " }")
	}
	if q.Sort != nil {
		fmt.Fprint(w, " }; var _res = _tmp.sortBy(_._1).map(_._2)")
	} else {
		fmt.Fprint(w, " }")
	}
	if q.Distinct {
		fmt.Fprint(w, ".distinct")
	}
	if q.Skip != nil {
		fmt.Fprint(w, ".drop(")
		q.Skip.emit(w)
		fmt.Fprint(w, ")")
	}
	if q.Take != nil {
		fmt.Fprint(w, ".take(")
		q.Take.emit(w)
		fmt.Fprint(w, ")")
	}
	fmt.Fprint(w, "; _res })")
}

func (r *RightJoinExpr) emit(w io.Writer) {
	et := r.ElemType
	if et == "" {
		et = "Any"
	}
	fmt.Fprintf(w, "({ var _res = ArrayBuffer[%s]() ; for (%s <- ", et, r.RightVar)
	r.RightSrc.emit(w)
	fmt.Fprintf(w, ") { var matched = false ; for (%s <- ", r.LeftVar)
	r.LeftSrc.emit(w)
	fmt.Fprint(w, ") { if (")
	r.Cond.emit(w)
	fmt.Fprint(w, ") { matched = true ; _res.append(")
	r.Select.emit(w)
	fmt.Fprint(w, ") } } if (!matched) { var ")
	fmt.Fprint(w, r.LeftVar)
	fmt.Fprint(w, " = null ; _res.append(")
	r.Select.emit(w)
	fmt.Fprint(w, ") }\n _res })")
}

func (l *LeftJoinExpr) emit(w io.Writer) {
	et := l.ElemType
	if et == "" {
		et = "Any"
	}
	fmt.Fprintf(w, "({ var _res = ArrayBuffer[%s]() ; for (%s <- ", et, l.LeftVar)
	l.LeftSrc.emit(w)
	fmt.Fprintf(w, ") { var matched = false ; for (%s <- ", l.RightVar)
	l.RightSrc.emit(w)
	fmt.Fprint(w, ") { if (")
	l.Cond.emit(w)
	fmt.Fprint(w, ") { matched = true ; _res.append(")
	l.Select.emit(w)
	fmt.Fprint(w, ") } }; if (!matched) { var ")
	fmt.Fprint(w, l.RightVar)
	fmt.Fprint(w, " = null ; _res.append(")
	l.Select.emit(w)
	fmt.Fprint(w, ") }\n _res })")
}

func (g *GroupByExpr) emit(w io.Writer) {
	elem := g.ElemType
	if elem == "" {
		elem = "Any"
	}
	fmt.Fprintf(w, "({ var _tmp = ArrayBuffer[(Any, Map[String,Any])]() ; for (%s <- ", g.Var)
	g.Source.emit(w)
	fmt.Fprint(w, ") {")
	for _, f := range g.Froms {
		fmt.Fprintf(w, " for (%s <- ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, ") {")
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, " for (%s <- ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, ") { if (")
		j.Cond.emit(w)
		fmt.Fprint(w, ") {")
	}
	if g.Where != nil {
		fmt.Fprint(w, " if (")
		g.Where.emit(w)
		fmt.Fprint(w, ") {")
	}
	fmt.Fprint(w, " _tmp.append((")
	g.Key.emit(w)
	fmt.Fprint(w, ", Map(")
	fmt.Fprintf(w, "\"%s\" -> %s", g.Var, g.Var)
	for _, f := range g.Froms {
		fmt.Fprintf(w, ", \"%s\" -> %s", f.Var, f.Var)
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, ", \"%s\" -> %s", j.Var, j.Var)
	}
	fmt.Fprint(w, ")))")
	if g.Where != nil {
		fmt.Fprint(w, " }")
	}
	for range g.Joins {
		fmt.Fprint(w, " }}")
	}
	for range g.Froms {
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, " }; ArrayBuffer(_tmp.groupBy(_._1).map{ case (k, arr) => Map(\"key\" -> k, \"items\" -> ArrayBuffer(arr.map(_._2).toSeq: _*)) }.toSeq: _*)")
	if g.Having != nil {
		fmt.Fprint(w, ".filter(")
		fmt.Fprintf(w, "%s => ", g.Name)
		g.Having.emit(w)
		fmt.Fprint(w, ")")
	}
	if g.Sort != nil {
		fmt.Fprint(w, ".toSeq.sortBy(")
		fmt.Fprintf(w, "%s => ", g.Name)
		g.Sort.emit(w)
		fmt.Fprint(w, ").map(_._2)")
	}
	fmt.Fprint(w, ".map(")
	fmt.Fprintf(w, "%s => ", g.Name)
	g.Select.emit(w)
	fmt.Fprint(w, ") })")
}

// Emit generates formatted Scala source for the given program.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.Write(meta.Header("//"))
	buf.WriteString("import scala.collection.mutable.{ArrayBuffer, Map}\n")
	buf.WriteString("object Main {\n")

	for _, st := range p.Stmts {
		if fn, ok := st.(*FunStmt); ok {
			buf.WriteString("  ")
			fn.emit(&buf)
			buf.WriteByte('\n')
			buf.WriteByte('\n')
		}
	}

	buf.WriteString("  def main(args: Array[String]): Unit = {\n")
	for _, st := range p.Stmts {
		if _, ok := st.(*FunStmt); ok {
			continue
		}
		buf.WriteString("    ")
		st.emit(&buf)
		buf.WriteByte('\n')
	}
	buf.WriteString("  }\n")
	buf.WriteString("}\n")
	return formatScala(buf.Bytes())
}

// Transpile converts a Mochi AST into our simple Scala AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	sc := &Program{}
	typeDecls = nil
	for _, st := range prog.Statements {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		sc.Stmts = append(sc.Stmts, s)
	}
	if len(typeDecls) > 0 {
		stmts := make([]Stmt, 0, len(typeDecls)+len(sc.Stmts))
		for _, d := range typeDecls {
			stmts = append(stmts, d)
		}
		sc.Stmts = append(stmts, sc.Stmts...)
	}
	return sc, nil
}

func convertStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr, env)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		}
		typ := toScalaType(st.Let.Type)
		if typ == "" {
			typ = inferTypeWithEnv(e, env)
		}
		if e == nil && typ != "" {
			e = defaultExpr(typ)
		}
		if env != nil {
			var t types.Type = types.AnyType{}
			if ll, ok := e.(*ListLit); ok && len(ll.Elems) > 0 {
				if sl, ok2 := ll.Elems[0].(*StructLit); ok2 {
					t = types.ListType{Elem: types.StructType{Name: sl.Name}}
				}
			} else if sl, ok := e.(*StructLit); ok {
				t = types.StructType{Name: sl.Name}
			} else if q, ok := e.(*QueryExpr); ok {
				if q.ElemType != "" {
					t = types.ListType{Elem: types.StructType{Name: q.ElemType}}
				}
			} else if st.Let.Value != nil {
				t = types.ExprType(st.Let.Value, env)
			}
			env.SetVar(st.Let.Name, t, false)
		}
		return &LetStmt{Name: st.Let.Name, Type: typ, Value: e}, nil
	case st.Var != nil:
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		}
		typ := toScalaType(st.Var.Type)
		if typ == "" {
			typ = inferTypeWithEnv(e, env)
		}
		if e == nil && typ != "" {
			e = defaultExpr(typ)
		}
		if env != nil {
			var t types.Type = types.AnyType{}
			if ll, ok := e.(*ListLit); ok && len(ll.Elems) > 0 {
				if sl, ok2 := ll.Elems[0].(*StructLit); ok2 {
					t = types.ListType{Elem: types.StructType{Name: sl.Name}}
				}
			} else if sl, ok := e.(*StructLit); ok {
				t = types.StructType{Name: sl.Name}
			} else if q, ok := e.(*QueryExpr); ok {
				if q.ElemType != "" {
					t = types.ListType{Elem: types.StructType{Name: q.ElemType}}
				}
			} else if st.Var.Value != nil {
				t = types.ExprType(st.Var.Value, env)
			}
			env.SetVar(st.Var.Name, t, true)
		}
		return &VarStmt{Name: st.Var.Name, Type: typ, Value: e}, nil
	case st.Type != nil:
		td := &TypeDeclStmt{Name: st.Type.Name}
		for _, m := range st.Type.Members {
			if m.Field != nil {
				td.Fields = append(td.Fields, Param{Name: m.Field.Name, Type: toScalaType(m.Field.Type)})
			}
		}
		return td, nil
	case st.Assign != nil:
		target := Expr(&Name{Name: st.Assign.Name})
		if len(st.Assign.Index) > 0 {
			var err error
			target, err = applyIndexOps(target, st.Assign.Index)
			if err != nil {
				return nil, err
			}
		}
		for _, f := range st.Assign.Field {
			target = &FieldExpr{Receiver: target, Name: f.Name}
		}
		e, err := convertExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Target: target, Value: e}, nil
	case st.Fun != nil:
		return convertFunStmt(st.Fun, env)
	case st.Return != nil:
		return convertReturnStmt(st.Return, env)
	case st.While != nil:
		return convertWhileStmt(st.While, env)
	case st.For != nil:
		return convertForStmt(st.For, env)
	case st.If != nil:
		return convertIfStmt(st.If, env)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return convertBinary(e.Binary, env)
}

func convertBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	operands := []Expr{}
	operators := []string{}

	left, err := convertUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	operands = append(operands, left)

	for _, part := range b.Right {
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		operators = append(operators, op)
		right, err := convertPostfix(part.Right, env)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
	}

	apply := func(i int) {
		op := operators[i]
		left := operands[i]
		right := operands[i+1]
		var ex Expr
		switch op {
		case "in":
			ex = &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "contains"}, Args: []Expr{left}}
		case "union":
			ex = &FieldExpr{Receiver: &BinaryExpr{Left: left, Op: "++", Right: right}, Name: "distinct"}
		case "union_all":
			ex = &BinaryExpr{Left: left, Op: "++", Right: right}
		case "except":
			fn := &FunExpr{Params: []Param{{Name: "x"}}, Expr: &UnaryExpr{Op: "!", Expr: &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "contains"}, Args: []Expr{&Name{Name: "x"}}}}}
			ex = &CallExpr{Fn: &FieldExpr{Receiver: left, Name: "filter"}, Args: []Expr{fn}}
		case "intersect":
			fn := &FunExpr{Params: []Param{{Name: "x"}}, Expr: &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "contains"}, Args: []Expr{&Name{Name: "x"}}}}
			ex = &CallExpr{Fn: &FieldExpr{Receiver: left, Name: "filter"}, Args: []Expr{fn}}
		default:
			ex = &BinaryExpr{Left: left, Op: op, Right: right}
		}
		operands[i] = ex
		operands = append(operands[:i+1], operands[i+2:]...)
		operators = append(operators[:i], operators[i+1:]...)
	}

	for _, level := range [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	} {
		for i := 0; i < len(operators); {
			if contains(level, operators[i]) {
				apply(i)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unsupported binary expression")
	}
	return operands[0], nil
}

func contains(list []string, op string) bool {
	for _, s := range list {
		if s == op {
			return true
		}
	}
	return false
}

func applyIndexOps(base Expr, ops []*parser.IndexOp) (Expr, error) {
	var err error
	for _, op := range ops {
		if op.Colon != nil || op.Colon2 != nil || op.End != nil || op.Step != nil {
			return nil, fmt.Errorf("unsupported assign")
		}
		if op.Start == nil {
			return nil, fmt.Errorf("nil index")
		}
		var idx Expr
		idx, err = convertExpr(op.Start, nil)
		if err != nil {
			return nil, err
		}
		base = &IndexExpr{Value: base, Index: idx}
	}
	return base, nil
}

func convertUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	expr, err := convertPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
		case "!":
			expr = &UnaryExpr{Op: "!", Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary")
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := convertPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Field != nil:
			if n, ok := expr.(*Name); ok && env != nil {
				if typ, err := env.GetVar(n.Name); err == nil {
					switch typ.(type) {
					case types.GroupType, types.MapType:
						expr = &IndexExpr{Value: expr, Index: &StringLit{Value: op.Field.Name}}
						continue
					}
				}
			}
			if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
				call := pf.Ops[i+1].Call
				args := make([]Expr, len(call.Args))
				for j, a := range call.Args {
					ex, err := convertExpr(a, env)
					if err != nil {
						return nil, err
					}
					args[j] = ex
				}
				expr = &CallExpr{Fn: &FieldExpr{Receiver: expr, Name: op.Field.Name}, Args: args}
				i++
			} else {
				expr = &FieldExpr{Receiver: expr, Name: op.Field.Name}
			}
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil || idx.Colon2 != nil {
				if idx.Start == nil || idx.End == nil {
					return nil, fmt.Errorf("unsupported slice")
				}
				start, err := convertExpr(idx.Start, env)
				if err != nil {
					return nil, err
				}
				end, err := convertExpr(idx.End, env)
				if err != nil {
					return nil, err
				}
				expr = &SliceExpr{Value: expr, Start: start, End: end}
			} else {
				start, err := convertExpr(idx.Start, env)
				if err != nil {
					return nil, err
				}
				expr = &IndexExpr{Value: expr, Index: start}
			}
		case op.Call != nil:
			call := op.Call
			args := make([]Expr, len(call.Args))
			for j, a := range call.Args {
				ex, err := convertExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			expr = &CallExpr{Fn: expr, Args: args}
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
			} else {
				return nil, fmt.Errorf("unsupported cast")
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Call != nil:
		return convertCall(p.Call, env)
	case p.Selector != nil:
		expr := Expr(&Name{Name: p.Selector.Root})
		if env != nil {
			if typ, err := env.GetVar(p.Selector.Root); err == nil {
				if _, ok := typ.(types.GroupType); ok && len(p.Selector.Tail) > 0 {
					expr = &IndexExpr{Value: expr, Index: &StringLit{Value: p.Selector.Tail[0]}}
					for _, f := range p.Selector.Tail[1:] {
						expr = &FieldExpr{Receiver: expr, Name: f}
					}
					return expr, nil
				}
			}
		}
		for _, f := range p.Selector.Tail {
			expr = &FieldExpr{Receiver: expr, Name: f}
		}
		return expr, nil
	case p.List != nil:
		if env != nil {
			if st, ok := types.InferStructFromList(p.List, env); ok {
				name := types.UniqueStructName("Item", env, nil)
				st.Name = name
				env.SetStruct(name, st)
				fields := make([]Param, len(st.Order))
				for i, n := range st.Order {
					fields[i] = Param{Name: n, Type: toScalaTypeFromType(st.Fields[n])}
				}
				typeDecls = append(typeDecls, &TypeDeclStmt{Name: name, Fields: fields})
				elems := make([]Expr, len(p.List.Elems))
				for i, el := range p.List.Elems {
					ml := el.Binary.Left.Value.Target.Map
					sl := &parser.StructLiteral{Name: name}
					for _, fn := range st.Order {
						for _, it := range ml.Items {
							if key, _ := types.SimpleStringKey(it.Key); key == fn {
								sl.Fields = append(sl.Fields, &parser.StructLitField{Name: fn, Value: it.Value})
								break
							}
						}
					}
					ex, err := convertStructLiteral(sl, env)
					if err != nil {
						return nil, err
					}
					elems[i] = ex
				}
				return &ListLit{Elems: elems}, nil
			}
		}
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(e, env)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		entries := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k Expr
			if s, ok := types.SimpleStringKey(it.Key); ok {
				k = &StringLit{Value: s}
			} else {
				var err error
				k, err = convertExpr(it.Key, env)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertExpr(it.Value, env)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: k, Value: v}
		}
		return &MapLit{Items: entries}, nil
	case p.Query != nil:
		if rj, err := convertRightJoinQuery(p.Query, env); err == nil {
			return rj, nil
		}
		if lj, err := convertLeftJoinQuery(p.Query, env); err == nil {
			return lj, nil
		}
		if ij, err := convertInnerJoinQuery(p.Query, env); err == nil {
			return ij, nil
		}
		return convertQueryExpr(p.Query, env)
	case p.Struct != nil:
		return convertStructLiteral(p.Struct, env)
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr)
	case p.Group != nil:
		return convertExpr(p.Group, env)
	case p.Match != nil:
		return convertMatchExpr(p.Match, env)
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertCall(c *parser.CallExpr, env *types.Env) (Expr, error) {
	args := make([]Expr, len(c.Args))
	for i, a := range c.Args {
		ex, err := convertExpr(a, env)
		if err != nil {
			return nil, err
		}
		args[i] = ex
	}
	name := c.Func
	switch name {
	case "len", "count":
		if len(args) == 1 {
			if n, ok := args[0].(*Name); ok && env != nil {
				if typ, err := env.GetVar(n.Name); err == nil {
					if _, ok := typ.(types.GroupType); ok {
						v := &IndexExpr{Value: args[0], Index: &StringLit{Value: "items"}}
						return &LenExpr{Value: v}, nil
					}
				}
			}
			return &LenExpr{Value: args[0]}, nil
		}
	case "print":
		if len(args) == 1 {
			return &CallExpr{Fn: &Name{Name: "println"}, Args: args}, nil
		}
		list := &CallExpr{Fn: &Name{Name: "List"}, Args: args}
		join := &CallExpr{Fn: &FieldExpr{Receiver: list, Name: "mkString"}, Args: []Expr{&StringLit{Value: " "}}}
		return &CallExpr{Fn: &Name{Name: "println"}, Args: []Expr{join}}, nil
	case "str":
		if len(args) == 1 {
			name = "String.valueOf"
		}
	case "append":
		if len(args) == 2 {
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		}
	case "substring":
		if len(args) == 3 {
			return &SubstringExpr{Value: args[0], Start: args[1], End: args[2]}, nil
		}
	case "sum":
		if len(args) == 1 {
			return &FieldExpr{Receiver: args[0], Name: "sum"}, nil
		}
	case "avg":
		if len(args) == 1 {
			sum := &FieldExpr{Receiver: args[0], Name: "sum"}
			ln := &LenExpr{Value: args[0]}
			return &BinaryExpr{Left: sum, Op: "/", Right: ln}, nil
		}
	case "min":
		if len(args) == 1 {
			return &FieldExpr{Receiver: args[0], Name: "min"}, nil
		}
	case "max":
		if len(args) == 1 {
			return &FieldExpr{Receiver: args[0], Name: "max"}, nil
		}
	case "values":
		if len(args) == 1 {
			valCall := &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "values"}}
			return &CallExpr{Fn: &FieldExpr{Receiver: valCall, Name: "toList"}}, nil
		}
	case "exists":
		if len(c.Args) == 1 {
			if q := ExtractQueryExpr(c.Args[0]); q != nil {
				qe, err := convertQueryExpr(q, env)
				if err != nil {
					return nil, err
				}
				return &FieldExpr{Receiver: qe, Name: "nonEmpty"}, nil
			}
			return &BinaryExpr{Left: &LenExpr{Value: args[0]}, Op: ">", Right: &IntLit{Value: 0}}, nil
		}
	}

	if env != nil {
		if fn, ok := env.GetFunc(name); ok {
			if len(args) < len(fn.Params) {
				missing := fn.Params[len(args):]
				params := make([]Param, len(missing))
				for i, p := range missing {
					params[i] = Param{Name: p.Name, Type: toScalaType(p.Type)}
				}
				call := &CallExpr{Fn: &Name{Name: name}, Args: args}
				return &FunExpr{Params: params, Expr: call}, nil
			}
		}
	}

	return &CallExpr{Fn: &Name{Name: name}, Args: args}, nil
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, nil
	}
	if l.Int != nil {
		return &IntLit{Value: int(*l.Int)}, nil
	}
	if l.Float != nil {
		return &FloatLit{Value: *l.Float}, nil
	}
	if l.Bool != nil {
		return &BoolLit{Value: bool(*l.Bool)}, nil
	}
	return nil, fmt.Errorf("unsupported literal")
}

func convertFunExpr(fe *parser.FunExpr) (Expr, error) {
	f := &FunExpr{}
	for _, p := range fe.Params {
		f.Params = append(f.Params, Param{Name: p.Name, Type: toScalaType(p.Type)})
	}
	if fe.ExprBody != nil {
		expr, err := convertExpr(fe.ExprBody, nil)
		if err != nil {
			return nil, err
		}
		f.Expr = expr
	} else {
		return nil, fmt.Errorf("unsupported fun expr")
	}
	return f, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond, nil)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then, nil)
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
		elseExpr, err = convertExpr(ie.Else, nil)
		if err != nil {
			return nil, err
		}
	}
	if elseExpr == nil {
		elseExpr = &IntLit{Value: 0}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertStructLiteral(sl *parser.StructLiteral, env *types.Env) (Expr, error) {
	if env == nil {
		return nil, fmt.Errorf("no env for struct literal")
	}
	st, ok := env.GetStruct(sl.Name)
	if !ok {
		return nil, fmt.Errorf("unknown struct %s", sl.Name)
	}
	args := make([]Expr, len(st.Order))
	for i, name := range st.Order {
		var exprNode *parser.Expr
		for _, f := range sl.Fields {
			if f.Name == name {
				exprNode = f.Value
				break
			}
		}
		if exprNode == nil {
			return nil, fmt.Errorf("missing field %s", name)
		}
		ex, err := convertExpr(exprNode, env)
		if err != nil {
			return nil, err
		}
		args[i] = ex
	}
	return &StructLit{Name: sl.Name, Fields: args}, nil
}

func ExtractQueryExpr(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Query
}

func convertRightJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src, env)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(j.On, child)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel, ElemType: elemTypeStr}, nil
}

func convertLeftJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src, env)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(j.On, child)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel, ElemType: elemTypeStr}, nil
}

func convertInnerJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) == 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
	}
	src, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	srcType := types.ExprType(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemT = lt.Elem
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elemT, true)

	froms := make([]queryFrom, 0, len(q.Joins))
	var where Expr
	for _, j := range q.Joins {
		joinSrc, err := convertExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		froms = append(froms, queryFrom{Var: j.Var, Src: joinSrc})
		cond, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	if ml := mapLiteral(q.Select); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			name := types.UniqueStructName("QueryItem", env, nil)
			st.Name = name
			env.SetStruct(name, st)
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: toScalaTypeFromType(st.Fields[n])}
			}
			typeDecls = append(typeDecls, &TypeDeclStmt{Name: name, Fields: fields})
			sl := &parser.StructLiteral{Name: name}
			for _, n := range st.Order {
				for _, it := range ml.Items {
					if key, _ := types.SimpleStringKey(it.Key); key == n {
						sl.Fields = append(sl.Fields, &parser.StructLitField{Name: n, Value: it.Value})
						break
					}
				}
			}
			q.Select = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Struct: sl}}}}}
		}
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Where: where, Select: sel, Distinct: q.Distinct, ElemType: elemTypeStr}, nil
}

func convertGroupQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group == nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	t := types.ExprType(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := t.(types.ListType); ok {
		elemT = lt.Elem
	} else if gt, ok := t.(types.GroupType); ok {
		elemT = gt.Elem
	}
	child.SetVar(q.Var, elemT, true)
	froms := make([]queryFrom, 0, len(q.Froms))
	joins := make([]queryJoin, 0, len(q.Joins))
	var where Expr
	for _, f := range q.Froms {
		fe, err := convertExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		ft := types.ExprType(f.Src, child)
		var felem types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			felem = lt.Elem
		}
		child.SetVar(f.Var, felem, true)
		froms = append(froms, queryFrom{Var: f.Var, Src: fe})
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
		je, err := convertExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		cond, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		joins = append(joins, queryJoin{Var: j.Var, Src: je, Cond: cond})
	}
	if q.Where != nil {
		cond, err := convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	if len(q.Joins) > 0 || len(q.Froms) > 0 {
		elemT = types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	}
	key, err := convertExpr(q.Group.Exprs[0], child)
	if err != nil {
		return nil, err
	}
	genv := types.NewEnv(child)
	genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: elemT}, true)
	sel, err := convertExpr(q.Select, genv)
	if err != nil {
		return nil, err
	}
	var having Expr
	var sortExpr Expr
	var sortType string
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having, genv)
		if err != nil {
			return nil, err
		}
	}
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		sortType = inferType(sortExpr)
		if sortType == "" {
			sortType = toScalaTypeFromType(types.ExprType(q.Sort, genv))
		}
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, genv))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &GroupByExpr{Var: q.Var, Source: src, Key: key, Name: q.Group.Name, Where: where, Select: sel, Having: having, Sort: sortExpr, SortType: sortType, Froms: froms, Joins: joins, ElemType: elemTypeStr}, nil
}

func convertQueryExpr(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group != nil {
		return convertGroupQuery(q, env)
	}
	if len(q.Joins) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	srcType := types.ExprType(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemT = lt.Elem
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elemT, true)
	froms := make([]queryFrom, 0, len(q.Froms)+len(q.Joins))
	var where Expr
	for _, f := range q.Froms {
		fe, err := convertExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		ft := types.ExprType(f.Src, child)
		var felem types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			felem = lt.Elem
		}
		child.SetVar(f.Var, felem, true)
		froms = append(froms, queryFrom{Var: f.Var, Src: fe})
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
		je, err := convertExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		froms = append(froms, queryFrom{Var: j.Var, Src: je})
		cond, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	var sort Expr
	var sortType string
	var skipExpr Expr
	var takeExpr Expr
	if q.Where != nil {
		cond, err := convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	if q.Sort != nil {
		sort, err = convertExpr(q.Sort, child)
		if err != nil {
			return nil, err
		}
		sortType = inferType(sort)
		if sortType == "" {
			sortType = toScalaTypeFromType(types.ExprType(q.Sort, child))
		}
	}
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, env)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, env)
		if err != nil {
			return nil, err
		}
	}
	if ml := mapLiteral(q.Select); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			name := types.UniqueStructName("QueryItem", env, nil)
			st.Name = name
			env.SetStruct(name, st)
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: toScalaTypeFromType(st.Fields[n])}
			}
			typeDecls = append(typeDecls, &TypeDeclStmt{Name: name, Fields: fields})
			sl := &parser.StructLiteral{Name: name}
			for _, n := range st.Order {
				for _, it := range ml.Items {
					if key, _ := types.SimpleStringKey(it.Key); key == n {
						sl.Fields = append(sl.Fields, &parser.StructLitField{Name: n, Value: it.Value})
						break
					}
				}
			}
			q.Select = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Struct: sl}}}}}
		}
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Where: where, Sort: sort, SortType: sortType, Skip: skipExpr, Take: takeExpr, Select: sel, Distinct: q.Distinct, ElemType: elemTypeStr}, nil
}

func mapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil
	}
	return p.Target.Map
}

func convertMatchExpr(me *parser.MatchExpr, env *types.Env) (Expr, error) {
	target, err := convertExpr(me.Target, env)
	if err != nil {
		return nil, err
	}
	m := &MatchExpr{Target: target}
	for _, c := range me.Cases {
		pat, err := convertExpr(c.Pattern, env)
		if err != nil {
			return nil, err
		}
		res, err := convertExpr(c.Result, env)
		if err != nil {
			return nil, err
		}
		m.Cases = append(m.Cases, MatchCase{Pattern: pat, Result: res})
	}
	return m, nil
}

func convertFunStmt(fs *parser.FunStmt, env *types.Env) (Stmt, error) {
	fn := &FunStmt{Name: fs.Name}
	for _, p := range fs.Params {
		fn.Params = append(fn.Params, Param{Name: p.Name, Type: toScalaType(p.Type)})
	}
	fn.Return = toScalaType(fs.Return)
	for _, st := range fs.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		fn.Body = append(fn.Body, s)
	}
	return fn, nil
}

func convertReturnStmt(rs *parser.ReturnStmt, env *types.Env) (Stmt, error) {
	var expr Expr
	var err error
	if rs.Value != nil {
		expr, err = convertExpr(rs.Value, env)
		if err != nil {
			return nil, err
		}
	}
	return &ReturnStmt{Value: expr}, nil
}

func convertWhileStmt(ws *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(ws.Cond, env)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range ws.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(fs.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, st := range fs.Body {
			s, err := convertStmt(st, env)
			if err != nil {
				return nil, err
			}
			body = append(body, s)
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fs.Source, env)
	if err != nil {
		return nil, err
	}
	if n, ok := iter.(*Name); ok && env != nil {
		if typ, err := env.GetVar(n.Name); err == nil {
			if _, ok := typ.(types.MapType); ok {
				iter = &FieldExpr{Receiver: iter, Name: "keys"}
			}
		}
	}
	var body []Stmt
	for _, st := range fs.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(is.Cond, env)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, st := range is.Then {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, s)
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		s, err := convertIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(is.Else) > 0 {
		for _, st := range is.Else {
			s, err := convertStmt(st, env)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, s)
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func toScalaType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "string":
			return "String"
		case "bool":
			return "Boolean"
		case "float":
			return "Double"
		}
		return "Any"
	}
	if t.Fun != nil {
		parts := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			typ := toScalaType(p)
			if typ == "" {
				typ = "Any"
			}
			parts[i] = typ
		}
		ret := toScalaType(t.Fun.Return)
		if ret == "" {
			ret = "Unit"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	}
	return "Any"
}

func toScalaTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	case types.FloatType:
		return "Double"
	case types.ListType:
		return fmt.Sprintf("ArrayBuffer[%s]", toScalaTypeFromType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Map[%s,%s]", toScalaTypeFromType(tt.Key), toScalaTypeFromType(tt.Value))
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
		return "Any"
	}
	return "Any"
}

// inferType attempts a best-effort static type deduction for the expression.
func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "Int"
	case *StringLit:
		return "String"
	case *BoolLit:
		return "Boolean"
	case *FloatLit:
		return "Double"
	case *ListLit:
		if len(ex.Elems) == 0 {
			return "ArrayBuffer[Any]"
		}
		t := inferType(ex.Elems[0])
		if t == "" {
			return "ArrayBuffer[Any]"
		}
		for _, e := range ex.Elems[1:] {
			if inferType(e) != t {
				return "ArrayBuffer[Any]"
			}
		}
		return fmt.Sprintf("ArrayBuffer[%s]", t)
	case *MapLit:
		if len(ex.Items) == 0 {
			return "Map[Any,Any]"
		}
		kt := inferType(ex.Items[0].Key)
		if kt == "" {
			kt = "Any"
		}
		vt := inferType(ex.Items[0].Value)
		if vt == "" {
			vt = "Any"
		}
		for _, it := range ex.Items[1:] {
			if inferType(it.Key) != kt {
				kt = "Any"
			}
			if inferType(it.Value) != vt {
				vt = "Any"
			}
		}
		return fmt.Sprintf("Map[%s,%s]", kt, vt)
	case *StructLit:
		return ex.Name
	case *LenExpr:
		return "Int"
	case *AppendExpr:
		if t := inferType(ex.List); strings.HasPrefix(t, "ArrayBuffer[") {
			return t
		}
		return "ArrayBuffer[Any]"
	case *IndexExpr:
		t := inferType(ex.Value)
		if strings.HasPrefix(t, "ArrayBuffer[") {
			inner := strings.TrimSuffix(strings.TrimPrefix(t, "ArrayBuffer["), "]")
			if inner != "" {
				return inner
			}
			return "Any"
		}
		if strings.HasPrefix(t, "Map[") {
			parts := strings.TrimSuffix(strings.TrimPrefix(t, "Map["), "]")
			kv := strings.SplitN(parts, ",", 2)
			if len(kv) == 2 {
				valType := strings.TrimSpace(kv[1])
				if valType != "" {
					return valType
				}
			}
		}
		return "Any"
	case *SubstringExpr:
		return "String"
	case *BinaryExpr:
		switch ex.Op {
		case "+", "-", "*", "/", "%":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "Double" || rt == "Double" {
				return "Double"
			}
			return "Int"
		case "==", "!=", ">", "<", ">=", "<=":
			return "Boolean"
		}
	case *CastExpr:
		return toScalaType(&parser.TypeRef{Simple: &ex.Type})
	case *FunExpr:
		parts := make([]string, len(ex.Params))
		for i, p := range ex.Params {
			if p.Type != "" {
				parts[i] = p.Type
			} else {
				parts[i] = "Any"
			}
		}
		ret := inferType(ex.Expr)
		if ret == "" {
			ret = "Any"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	case *IfExpr:
		t1 := inferType(ex.Then)
		t2 := inferType(ex.Else)
		if t1 == t2 {
			return t1
		}
	case *QueryExpr:
		if ex.ElemType != "" {
			return fmt.Sprintf("ArrayBuffer[%s]", ex.ElemType)
		}
		return "ArrayBuffer[Any]"
	case *RightJoinExpr:
		if ex.ElemType != "" {
			return fmt.Sprintf("ArrayBuffer[%s]", ex.ElemType)
		}
		return "ArrayBuffer[Any]"
	case *GroupByExpr:
		if ex.ElemType != "" {
			return fmt.Sprintf("ArrayBuffer[%s]", ex.ElemType)
		}
		return "ArrayBuffer[Any]"
	default:
		_ = ex
	}
	return ""
}

func inferTypeWithEnv(e Expr, env *types.Env) string {
	if t := inferType(e); t != "" {
		return t
	}
	if n, ok := e.(*Name); ok && env != nil {
		if typ, err := env.GetVar(n.Name); err == nil {
			return toScalaTypeFromType(typ)
		}
	}
	return ""
}

// defaultExpr returns a zero value expression for the given Scala type.
func defaultExpr(typ string) Expr {
	switch {
	case typ == "Int" || typ == "Long" || typ == "Double" || typ == "Float":
		if typ == "Double" || typ == "Float" {
			return &FloatLit{Value: 0}
		}
		return &IntLit{Value: 0}
	case typ == "String":
		return &StringLit{Value: ""}
	case typ == "Boolean":
		return &BoolLit{Value: false}
	case strings.HasPrefix(typ, "ArrayBuffer"):
		return &CallExpr{Fn: &Name{Name: "ArrayBuffer"}, Args: nil}
	case strings.HasPrefix(typ, "Map"):
		return &CallExpr{Fn: &Name{Name: "Map"}, Args: nil}
	default:
		return nil
	}
}

// Print converts the Scala AST to ast.Node and prints it.
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
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *ReturnStmt:
		return &ast.Node{Kind: "return", Children: []*ast.Node{exprNode(st.Value)}}
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
		n := &ast.Node{Kind: "forrange", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForEachStmt:
		n := &ast.Node{Kind: "foreach", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Iterable))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *AssignStmt:
		n := &ast.Node{Kind: "assign"}
		n.Children = append(n.Children, exprNode(st.Target), exprNode(st.Value))
		return n
	case *FunStmt:
		n := &ast.Node{Kind: "fun", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, params, body)
		return n
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
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(ex.Fn))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: fmt.Sprint(ex.Value)}
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Value)}}
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
			pair := &ast.Node{Kind: "pair"}
			pair.Children = append(pair.Children, exprNode(it.Key), exprNode(it.Value))
			n.Children = append(n.Children, pair)
		}
		return n
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Value), exprNode(ex.Start), exprNode(ex.End)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Value)}}
	case *IfExpr:
		n := &ast.Node{Kind: "ifexpr"}
		n.Children = append(n.Children, exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else))
		return n
	case *FunExpr:
		n := &ast.Node{Kind: "funexpr"}
		params := &ast.Node{Kind: "params"}
		for _, p := range ex.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params, exprNode(ex.Expr))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func formatScala(src []byte) []byte {
	lines := strings.Split(string(src), "\n")
	var out []string
	indent := 0
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "}") {
			if indent > 0 {
				indent--
			}
		}
		out = append(out, strings.Repeat("  ", indent)+trimmed)
		if strings.HasSuffix(trimmed, "{") {
			indent++
		}
	}
	return []byte(strings.Join(out, "\n"))
}
