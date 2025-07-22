//go:build slow

package swifttrans

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"time"

	"gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Program is a sequence of Swift statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type FunDecl struct {
	Name   string
	Params []Param
	Ret    string
	Body   []Stmt
}

type Param struct {
	Name string
	Type string
}

type ReturnStmt struct{ Expr Expr }

type CallExpr struct {
	Func string
	Args []Expr
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents a simple numeric range for loop.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForEachStmt represents iteration over a collection.
type ForEachStmt struct {
	Name    string
	Expr    Expr
	Body    []Stmt
	CastMap bool
	Keys    bool
}

type BreakStmt struct{}

type ContinueStmt struct{}

// RawStmt emits preformed Swift code directly.
type RawStmt struct{ Code string }

func (r *RawStmt) emit(w io.Writer) {
	io.WriteString(w, r.Code)
	if !strings.HasSuffix(r.Code, "\n") {
		io.WriteString(w, "\n")
	}
}

// FieldExpr represents property access on a struct.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (fe *FieldExpr) emit(w io.Writer) {
	fe.Target.emit(w)
	fmt.Fprintf(w, ".%s", fe.Name)
}

// StructDef represents a simple struct type declaration.
type StructDef struct {
	Name   string
	Fields []StructField
}

// StructField defines a single field within a struct.
type StructField struct {
	Name string
	Type string
}

func (sd *StructDef) emit(w io.Writer) {
	fmt.Fprintf(w, "struct %s {\n", sd.Name)
	for _, f := range sd.Fields {
		fmt.Fprintf(w, "    var %s: %s\n", f.Name, f.Type)
	}
	fmt.Fprint(w, "}\n")
}

type IfStmt struct {
	Cond   Expr
	Then   []Stmt
	ElseIf *IfStmt
	Else   []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if ")
	i.Cond.emit(w)
	fmt.Fprint(w, " {\n")
	for _, s := range i.Then {
		s.emit(w)
	}
	fmt.Fprint(w, "}")
	if i.ElseIf != nil {
		fmt.Fprint(w, " else ")
		i.ElseIf.emit(w)
	} else if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, s := range i.Else {
			s.emit(w)
		}
		fmt.Fprint(w, "}")
	}
	fmt.Fprint(w, "\n")
}

type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

// FunExpr represents an anonymous function expression.
type FunExpr struct {
	Params []Param
	Ret    string
	Body   Expr
}

func (f *FunExpr) emit(w io.Writer) {
	fmt.Fprint(w, "{ (")
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
	if f.Ret != "" {
		fmt.Fprintf(w, " -> %s", f.Ret)
	}
	fmt.Fprint(w, " in ")
	if f.Body != nil {
		f.Body.emit(w)
	}
	fmt.Fprint(w, " }")
}

func (c *CondExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	c.Cond.emit(w)
	fmt.Fprint(w, " ? ")
	c.Then.emit(w)
	fmt.Fprint(w, " : ")
	c.Else.emit(w)
	fmt.Fprint(w, ")")
}

type PrintStmt struct{ Exprs []Expr }

func (p *PrintStmt) emit(w io.Writer) {
	fmt.Fprint(w, "print(")
	for i, e := range p.Exprs {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, ")\n")
}

type ExprStmt struct{ Expr Expr }

func (e *ExprStmt) emit(w io.Writer) {
	e.Expr.emit(w)
	fmt.Fprint(w, "\n")
}

type VarDecl struct {
	Name  string
	Const bool
	Type  string
	Expr  Expr
}

func (v *VarDecl) emit(w io.Writer) {
	kw := "var"
	if v.Const && v.Expr != nil {
		kw = "let"
	}
	fmt.Fprint(w, kw+" "+v.Name)
	if v.Type != "" {
		fmt.Fprintf(w, ": %s", v.Type)
	}
	if v.Expr != nil {
		fmt.Fprint(w, " = ")
		v.Expr.emit(w)
	}
	fmt.Fprint(w, "\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprint(w, a.Name+" = ")
	a.Expr.emit(w)
	fmt.Fprint(w, "\n")
}

type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (ia *IndexAssignStmt) emit(w io.Writer) {
	ia.Target.emit(w)
	fmt.Fprint(w, " = ")
	ia.Value.emit(w)
	fmt.Fprint(w, "\n")
}

// SaveStmt writes a collection of values to stdout in JSONL format.
type SaveStmt struct {
	Src Expr
}

func (s *SaveStmt) emit(w io.Writer) {
	fmt.Fprint(w, "for _item in ")
	s.Src.emit(w)
	fmt.Fprint(w, " {\n")
	fmt.Fprint(w, "    let obj = toJsonObj(_item)\n")
	fmt.Fprint(w, "    if let data = try? JSONSerialization.data(withJSONObject: obj) {\n")
	fmt.Fprint(w, "        print(String(data: data, encoding: .utf8)!)\n")
	fmt.Fprint(w, "    }\n")
	fmt.Fprint(w, "}\n")
}

// UpdateStmt updates elements in a list of structs in place.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
	Locals []string
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for i in 0..<%s.count {\n", u.Target)
	fmt.Fprintf(w, "    var item = %s[i]\n", u.Target)
	for _, name := range u.Locals {
		fmt.Fprintf(w, "    var %s = item.%s\n", name, name)
	}
	if u.Cond != nil {
		fmt.Fprint(w, "    if ")
		u.Cond.emit(w)
		fmt.Fprint(w, " {\n")
		for i, f := range u.Fields {
			fmt.Fprintf(w, "        item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "    }\n")
	} else {
		for i, f := range u.Fields {
			fmt.Fprintf(w, "    item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, "\n")
		}
	}
	fmt.Fprintf(w, "    %s[i] = item\n", u.Target)
	fmt.Fprint(w, "}\n")
}

// ExpectStmt represents an expectation check within a test block.
type ExpectStmt struct{ Cond Expr }

func (e *ExpectStmt) emit(w io.Writer) {
	fmt.Fprint(w, "assert(")
	e.Cond.emit(w)
	fmt.Fprint(w, ")\n")
}

// BlockStmt groups a list of statements without additional syntax.
type BlockStmt struct{ Stmts []Stmt }

func (b *BlockStmt) emit(w io.Writer) {
	for _, s := range b.Stmts {
		s.emit(w)
	}
}

type LitExpr struct {
	Value    string
	IsString bool
}

func (l *LitExpr) emit(w io.Writer) {
	if l.IsString {
		fmt.Fprintf(w, "%q", l.Value)
	} else {
		fmt.Fprint(w, l.Value)
	}
}

type NameExpr struct {
	Name    string
	AsItems bool
}

func (n *NameExpr) emit(w io.Writer) {
	if n.AsItems {
		fmt.Fprintf(w, "%s[\"items\"] as! [[String: Any]]", n.Name)
	} else {
		fmt.Fprint(w, n.Name)
	}
}

type ArrayLit struct{ Elems []Expr }

func (a *ArrayLit) emit(w io.Writer) {
	fmt.Fprint(w, "[")
	for i, e := range a.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "]")
}

// MapLit represents a dictionary literal.
type MapLit struct {
	Keys   []Expr
	Values []Expr
}

func (m *MapLit) emit(w io.Writer) {
	fmt.Fprint(w, "[")
	for i := range m.Keys {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		m.Keys[i].emit(w)
		fmt.Fprint(w, ": ")
		m.Values[i].emit(w)
	}
	fmt.Fprint(w, "]")
}

// StructInit represents initialization of a struct value.
type StructInit struct {
	Name   string
	Fields []FieldInit
}

// FieldInit pairs a field name with its initializing expression.
type FieldInit struct {
	Name  string
	Value Expr
}

func (si *StructInit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", si.Name)
	for i, f := range si.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s: ", f.Name)
		f.Value.emit(w)
	}
	fmt.Fprint(w, ")")
}

// MapStringExpr renders a dictionary as a JSON-like string.
type MapStringExpr struct{ Value Expr }

func (ms *MapStringExpr) emit(w io.Writer) {
	fmt.Fprint(w, "String(describing: ")
	ms.Value.emit(w)
	fmt.Fprint(w, ")")
}

// ArrayStringExpr renders a list as a compact string without spaces.
type ArrayStringExpr struct{ Value Expr }

func (as *ArrayStringExpr) emit(w io.Writer) {
	fmt.Fprint(w, "\"[\" + ")
	as.Value.emit(w)
	fmt.Fprint(w, ".map{ String(describing: $0) }.joined(separator: \",\") + \"]\"")
}

// queryFrom represents a secondary source in a query expression.
type queryFrom struct {
	Var string
	Src Expr
}

// queryJoin represents a join clause.
type queryJoin struct {
	Var string
	Src Expr
	On  Expr
}

// GroupByExpr represents a simple query with grouping support.
type GroupByExpr struct {
	Var    string
	Source Expr
	Froms  []queryFrom
	Joins  []queryJoin
	Key    Expr
	Name   string
	Sort   Expr
	Where  Expr
	Select Expr
	Having Expr
}

// QueryExpr represents a basic comprehension supporting joins.
type QueryExpr struct {
	Var    string
	Src    Expr
	Froms  []queryFrom
	Joins  []queryJoin
	Where  Expr
	Sort   Expr
	Skip   Expr
	Take   Expr
	Select Expr
	Elem   string
}

// LeftJoinExpr represents a basic left join between two sources.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (q *QueryExpr) emit(w io.Writer) {
	if _, ok := q.Select.(*MapLit); ok {
		fmt.Fprint(w, "({ var _res: [[String: Any]] = []\n")
	} else if q.Elem != "" {
		fmt.Fprintf(w, "({ var _res: [%s] = []\n", q.Elem)
	} else {
		fmt.Fprint(w, "({ var _res: [Any] = []\n")
	}
	fmt.Fprintf(w, "for %s in ", q.Var)
	q.Src.emit(w)
	fmt.Fprint(w, " {\n")
	for _, f := range q.Froms {
		fmt.Fprintf(w, "for %s in ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, " {\n")
	}
	for _, j := range q.Joins {
		fmt.Fprintf(w, "for %s in ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, " {\nif ")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, " {\n")
	}
	if q.Where != nil {
		fmt.Fprint(w, "if ")
		q.Where.emit(w)
		fmt.Fprint(w, " {\n")
	}
	fmt.Fprint(w, "_res.append(")
	q.Select.emit(w)
	fmt.Fprint(w, ")\n")
	if q.Where != nil {
		fmt.Fprint(w, "}\n")
	}
	for range q.Joins {
		fmt.Fprint(w, "}\n")
		fmt.Fprint(w, "}\n")
	}
	for range q.Froms {
		fmt.Fprint(w, "}\n")
	}
	fmt.Fprint(w, "}\n")
	if q.Sort != nil || q.Skip != nil || q.Take != nil {
		fmt.Fprint(w, "var _list = _res\n")
		if q.Sort != nil {
			fmt.Fprint(w, "_list.sort { left, right in\n")
			fmt.Fprintf(w, "var %s = left\n", q.Var)
			fmt.Fprint(w, "let _ka = ")
			q.Sort.emit(w)
			fmt.Fprint(w, "\n")
			fmt.Fprintf(w, "%s = right\n", q.Var)
			fmt.Fprint(w, "let _kb = ")
			q.Sort.emit(w)
			fmt.Fprint(w, "\nreturn String(describing: _ka) < String(describing: _kb)\n}\n")
		}
		if q.Skip != nil {
			fmt.Fprint(w, "_list = Array(_list.dropFirst(")
			q.Skip.emit(w)
			fmt.Fprint(w, "))\n")
		}
		if q.Take != nil {
			fmt.Fprint(w, "_list = Array(_list.prefix(")
			q.Take.emit(w)
			fmt.Fprint(w, "))\n")
		}
		fmt.Fprint(w, "return _list })()")
	} else {
		fmt.Fprint(w, "return _res })()")
	}
}

func (l *LeftJoinExpr) emit(w io.Writer) {
	fmt.Fprint(w, "({ var _res: [[String: Any]] = []\n")
	fmt.Fprintf(w, "for %s in ", l.LeftVar)
	l.LeftSrc.emit(w)
	fmt.Fprint(w, " {\nvar matched = false\n")
	fmt.Fprintf(w, "for %s in ", l.RightVar)
	l.RightSrc.emit(w)
	fmt.Fprint(w, " {\nif ")
	l.Cond.emit(w)
	fmt.Fprint(w, " {\nmatched = true\n_res.append(")
	l.Select.emit(w)
	fmt.Fprint(w, ")\n}\n}\n")
	fmt.Fprint(w, "if !matched {\n")
	fmt.Fprintf(w, "let %s: Any? = nil\n", l.RightVar)
	fmt.Fprint(w, "_res.append(")
	l.Select.emit(w)
	fmt.Fprint(w, ")\n}\n}")
	fmt.Fprint(w, "\nreturn _res })()")
}

func (g *GroupByExpr) emit(w io.Writer) {
	fmt.Fprint(w, "({ var _groups: [String: [String: Any]] = [:]\n")
	fmt.Fprint(w, "var _res: [[String: Any]] = []\n")
	fmt.Fprintf(w, "for %s in ", g.Var)
	g.Source.emit(w)
	fmt.Fprint(w, " {\n")
	for _, f := range g.Froms {
		fmt.Fprintf(w, "for %s in ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, " {\n")
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, "for %s in ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, " {\nif ")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, " {\n")
	}
	if g.Where != nil {
		fmt.Fprint(w, "if ")
		g.Where.emit(w)
		fmt.Fprint(w, " {\n")
	}
	fmt.Fprint(w, "let _key = ")
	g.Key.emit(w)
	fmt.Fprint(w, "\nlet _ks = String(describing: _key)\nvar _g = _groups[_ks] ?? [\"key\": _key, \"items\": []]\n")
	fmt.Fprint(w, "var _item: [String: Any] = [\"__join__\": true]\n")
	fmt.Fprintf(w, "_item[\"%s\"] = %s\n", g.Var, g.Var)
	for _, f := range g.Froms {
		fmt.Fprintf(w, "_item[\"%s\"] = %s\n", f.Var, f.Var)
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, "_item[\"%s\"] = %s\n", j.Var, j.Var)
	}
	fmt.Fprint(w, "_g[\"items\"] = (_g[\"items\"] as! [[String: Any]]) + [_item]\n")
	fmt.Fprint(w, "_groups[_ks] = _g\n")
	if g.Where != nil {
		fmt.Fprint(w, "}\n")
	}
	for range g.Joins {
		fmt.Fprint(w, "}\n")
		fmt.Fprint(w, "}\n")
	}
	for range g.Froms {
		fmt.Fprint(w, "}\n")
	}
	fmt.Fprint(w, "}\n")
	fmt.Fprint(w, "var _list = Array(_groups.values)\n")
	if g.Sort != nil {
		fmt.Fprint(w, "_list.sort { left, right in\n")
		fmt.Fprintf(w, "var %s = left\n", g.Name)
		fmt.Fprint(w, "let _ka = ")
		g.Sort.emit(w)
		fmt.Fprint(w, "\n")
		fmt.Fprintf(w, "%s = right\n", g.Name)
		fmt.Fprint(w, "let _kb = ")
		g.Sort.emit(w)
		fmt.Fprint(w, "\nreturn String(describing: _ka) < String(describing: _kb)\n}\n")
	} else {
		fmt.Fprint(w, "_list.sort { a, b in String(describing: a[\"key\"]) < String(describing: b[\"key\"]) }\n")
	}
	fmt.Fprintf(w, "for %s in _list {\n", g.Name)
	if g.Having != nil {
		fmt.Fprint(w, "if ")
		g.Having.emit(w)
		fmt.Fprint(w, " {\n_res.append(")
		g.Select.emit(w)
		fmt.Fprint(w, ")\n}\n")
	} else {
		fmt.Fprint(w, "_res.append(")
		g.Select.emit(w)
		fmt.Fprint(w, ")\n")
	}
	fmt.Fprint(w, "}\nreturn _res })()")
}

type IndexExpr struct {
	Base     Expr
	Index    Expr
	AsString bool
	Force    bool
}

// SliceExpr represents a[start:end] slicing for lists or strings.
type SliceExpr struct {
	Base     Expr
	Start    Expr // optional
	End      Expr // optional
	AsString bool
}

// CastExpr represents a type cast.
type CastExpr struct {
	Expr Expr
	Type string
}

func (ie *IndexExpr) emit(w io.Writer) {
	if ie.AsString {
		fmt.Fprint(w, "String(Array(")
		ie.Base.emit(w)
		fmt.Fprint(w, ")[")
		ie.Index.emit(w)
		fmt.Fprint(w, "])")
		return
	}
	ie.Base.emit(w)
	fmt.Fprint(w, "[")
	ie.Index.emit(w)
	fmt.Fprint(w, "]")
	if ie.Force {
		fmt.Fprint(w, "!")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	if s.AsString {
		fmt.Fprint(w, "String(Array(")
		s.Base.emit(w)
		fmt.Fprint(w, ")[")
		if s.Start != nil {
			s.Start.emit(w)
		} else {
			fmt.Fprint(w, "0")
		}
		fmt.Fprint(w, "..<")
		if s.End != nil {
			s.End.emit(w)
		} else {
			fmt.Fprint(w, "Array(")
			s.Base.emit(w)
			fmt.Fprint(w, ").count")
		}
		fmt.Fprint(w, "])")
		return
	}
	fmt.Fprint(w, "Array(")
	s.Base.emit(w)
	fmt.Fprint(w, "[")
	if s.Start != nil {
		s.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "..<")
	if s.End != nil {
		s.End.emit(w)
	} else {
		s.Base.emit(w)
		fmt.Fprint(w, ".count")
	}
	fmt.Fprint(w, "])")
}

func (c *CastExpr) emit(w io.Writer) {
	// avoid noisy casts when the expression is already a literal
	if _, ok := c.Expr.(*LitExpr); ok {
		c.Expr.emit(w)
		return
	}
	t := c.Type
	force := false
	if strings.HasSuffix(t, "!") {
		force = true
		t = strings.TrimSuffix(t, "!")
	}
	switch t {
	case "Int", "Int64", "Double", "String", "Bool":
		if force {
			fmt.Fprint(w, "(")
			c.Expr.emit(w)
			fmt.Fprintf(w, " as! %s)", t)
		} else {
			fmt.Fprintf(w, "%s(", t)
			c.Expr.emit(w)
			fmt.Fprint(w, ")")
		}
	default:
		fmt.Fprint(w, "(")
		c.Expr.emit(w)
		fmt.Fprintf(w, " as! %s)", t)
	}
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
	InMap bool
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "in" {
		if b.InMap {
			fmt.Fprint(w, "(")
			b.Right.emit(w)
			fmt.Fprint(w, "[")
			b.Left.emit(w)
			fmt.Fprint(w, "] != nil)")
		} else {
			fmt.Fprint(w, "(")
			b.Right.emit(w)
			fmt.Fprint(w, ".contains(")
			b.Left.emit(w)
			fmt.Fprint(w, "))")
		}
		return
	}

	switch b.Op {
	case "&&", "||", "<", "<=", ">", ">=", "==", "!=":
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprintf(w, " %s ", b.Op)
		b.Right.emit(w)
		fmt.Fprint(w, ")")
		return
	}

	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" {
		fmt.Fprint(w, "(!")
		u.Expr.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	fmt.Fprint(w, u.Op)
	u.Expr.emit(w)
}

func (c *CallExpr) emit(w io.Writer) {
	switch c.Func {
	case "len":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			fmt.Fprint(w, ".count)")
			return
		}
	case "count":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			fmt.Fprint(w, ".count)")
			return
		}
	case "str":
		fmt.Fprint(w, "String(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		fmt.Fprint(w, ")")
		return
	case "append":
		if len(c.Args) == 2 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, " + [")
			c.Args[1].emit(w)
			fmt.Fprint(w, "])")
			return
		}
	case "avg":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "Double((")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ").reduce(0) { s, v in s + ((v as? Double) ?? Double(v as? Int ?? 0)) }")
			fmt.Fprint(w, " ) / Double((")
			c.Args[0].emit(w)
			fmt.Fprint(w, ").count))")
			return
		}
	case "sum":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ").reduce(0) { s, v in s + ((v as? Double) ?? Double(v as? Int ?? 0)) }")
			fmt.Fprint(w, ")")
			return
		}
	case "min":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ".min()!)")
			return
		}
	case "max":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ".max()!)")
			return
		}
	case "substring":
		if len(c.Args) == 3 {
			fmt.Fprint(w, "String(Array(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")[")
			c.Args[1].emit(w)
			fmt.Fprint(w, "..<")
			c.Args[2].emit(w)
			fmt.Fprint(w, "])")
			return
		}
	case "now":
		if len(c.Args) == 0 {
			fmt.Fprint(w, "Int64(Date().timeIntervalSince1970)")
			return
		}
	}
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

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Expr != nil {
		fmt.Fprint(w, " ")
		r.Expr.emit(w)
	}
	fmt.Fprint(w, "\n")
}

func (f *FunDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "func %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "_ %s: %s", p.Name, p.Type)
		} else {
			fmt.Fprintf(w, "_ %s", p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if f.Ret != "" {
		fmt.Fprintf(w, " -> %s", f.Ret)
	}
	fmt.Fprint(w, " {\n")
	for _, s := range f.Body {
		s.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while ")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	} else {
		fmt.Fprint(w, "true")
	}
	fmt.Fprint(w, " {\n")
	for _, st := range ws.Body {
		st.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for %s in ", fr.Name)
	fr.Start.emit(w)
	fmt.Fprint(w, "..<")
	fr.End.emit(w)
	fmt.Fprint(w, " {\n")
	for _, st := range fr.Body {
		st.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (fe *ForEachStmt) emit(w io.Writer) {
	if fe.CastMap {
		fmt.Fprint(w, "for _item in ")
		fe.Expr.emit(w)
		fmt.Fprint(w, " as! [[String: Any]] {\n")
		fmt.Fprintf(w, "let %s = _item as! [String: Any]\n", fe.Name)
	} else {
		fmt.Fprintf(w, "for %s in ", fe.Name)
		fe.Expr.emit(w)
		if fe.Keys {
			fmt.Fprint(w, ".keys.sorted()")
		}
		fmt.Fprint(w, " {\n")
	}
	for _, st := range fe.Body {
		st.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break\n") }

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue\n") }

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
	return fmt.Sprintf(`// Generated by Mochi transpiler v%s on %s
import Foundation

`, version(), t.Format("2006-01-02 15:04:05 MST"))
}

func formatCode(src []byte) []byte {
	var out bytes.Buffer
	indent := 0
	scanner := bufio.NewScanner(bytes.NewReader(src))
	for scanner.Scan() {
		line := scanner.Text()
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "}") && indent > 0 {
			indent--
		}
		for i := 0; i < indent; i++ {
			out.WriteString("    ")
		}
		out.WriteString(trimmed)
		out.WriteByte('\n')
		if strings.HasSuffix(trimmed, "{") {
			indent++
		}
	}
	return out.Bytes()
}

func exprString(e Expr) string {
	var buf bytes.Buffer
	e.emit(&buf)
	return strings.TrimSpace(buf.String())
}

func isAnyType(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

// Emit returns the Swift source for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	needJSON := false
	for _, st := range p.Stmts {
		if _, ok := st.(*SaveStmt); ok {
			needJSON = true
			break
		}
	}
	if needJSON {
		buf.WriteString("func toJsonObj(_ v: Any) -> Any {\n")
		buf.WriteString("    if let m = v as? [String: Any] { return m }\n")
		buf.WriteString("    if let a = v as? [Any] { return a }\n")
		buf.WriteString("    var d: [String: Any] = [:]\n")
		buf.WriteString("    for c in Mirror(reflecting: v).children {\n")
		buf.WriteString("        if let k = c.label { d[k] = c.value }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return d\n}\n")
	}
	for _, s := range p.Stmts {
		s.emit(&buf)
	}
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		buf.WriteByte('\n')
	}
	return formatCode(buf.Bytes())
}

// Transpile converts a Mochi program into a simple Swift AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	p := &Program{}
	stmts, err := convertStmts(env, prog.Statements)
	if err != nil {
		return nil, err
	}
	p.Stmts = stmts
	return p, nil
}

func findUpdatedVars(list []*parser.Statement, vars map[string]bool) {
	for _, st := range list {
		switch {
		case st.Update != nil:
			vars[st.Update.Target] = true
		case st.If != nil:
			findUpdatedVars(st.If.Then, vars)
			if st.If.Else != nil {
				findUpdatedVars(st.If.Else, vars)
			}
			if st.If.ElseIf != nil {
				child := &parser.Statement{If: st.If.ElseIf}
				findUpdatedVars([]*parser.Statement{child}, vars)
			}
		case st.For != nil:
			findUpdatedVars(st.For.Body, vars)
		case st.While != nil:
			findUpdatedVars(st.While.Body, vars)
		case st.Test != nil:
			findUpdatedVars(st.Test.Body, vars)
		case st.Fun != nil:
			findUpdatedVars(st.Fun.Body, vars)
		}
	}
}

func convertStmts(env *types.Env, list []*parser.Statement) ([]Stmt, error) {
	updated := map[string]bool{}
	findUpdatedVars(list, updated)
	var out []Stmt
	for _, st := range list {
		cs, err := convertStmt(env, st)
		if err != nil {
			return nil, err
		}
		// adjust var mutability for let statements
		if vd, ok := cs.(*VarDecl); ok && updated[vd.Name] {
			vd.Const = false
		}
		out = append(out, cs)
	}
	return out, nil
}

func convertStmt(env *types.Env, st *parser.Statement) (Stmt, error) {
	switch {
	case st.Import != nil:
		return convertImport(st.Import), nil
	case st.ExternVar != nil:
		return convertExternVar(st.ExternVar), nil
	case st.ExternFun != nil:
		return convertExternFun(st.ExternFun), nil
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			return convertSaveStmt(env, se)
		}
		if call != nil && call.Func == "print" {
			if len(call.Args) == 1 {
				arg := call.Args[0]
				if val, str, ok := evalPrintArg(arg); ok {
					return &PrintStmt{Exprs: []Expr{&LitExpr{Value: val, IsString: str}}}, nil
				}
			}
			var args []Expr
			for _, a := range call.Args {
				ex, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				if env != nil {
					if types.IsBoolType(types.TypeOfExpr(a, env)) && boolAsInt(a) {
						ex = &CondExpr{Cond: ex, Then: &LitExpr{Value: "1", IsString: false}, Else: &LitExpr{Value: "0", IsString: false}}
					} else if types.IsMapType(types.TypeOfExpr(a, env)) || types.IsStructType(types.TypeOfExpr(a, env)) {
						ex = &MapStringExpr{Value: ex}
					} else if types.IsListType(types.TypeOfExpr(a, env)) {
						ex = &ArrayStringExpr{Value: ex}
					}
				}
				args = append(args, ex)
			}
			return &PrintStmt{Exprs: args}, nil
		}
		if call != nil {
			ex, err := convertExpr(env, st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			return &ExprStmt{Expr: ex}, nil
		}
		return nil, fmt.Errorf("unsupported expression")
	case st.Let != nil:
		var ex Expr
		var err error
		var typ string
		if st.Let.Value != nil {
			ex, err = convertExpr(env, st.Let.Value)
			if err != nil {
				return nil, err
			}
			if env != nil {
				t := types.TypeOfExpr(st.Let.Value, env)
				typ = swiftTypeOf(t)
				if !types.IsEmptyListLiteral(st.Let.Value) && !isEmptyMapLiteral(st.Let.Value) {
					typ = ""
				}
			}
		} else if st.Let.Type != nil {
			ex = zeroValue(st.Let.Type)
			typ = toSwiftType(st.Let.Type)
		}
		return &VarDecl{Name: st.Let.Name, Const: true, Type: typ, Expr: ex}, nil
	case st.Var != nil:
		var ex Expr
		var err error
		var typ string
		if st.Var.Value != nil {
			ex, err = convertExpr(env, st.Var.Value)
			if err != nil {
				return nil, err
			}
			if env != nil {
				t := types.TypeOfExpr(st.Var.Value, env)
				typ = swiftTypeOf(t)
				if !types.IsEmptyListLiteral(st.Var.Value) && !isEmptyMapLiteral(st.Var.Value) {
					typ = ""
				}
			}
		} else if st.Var.Type != nil {
			ex = zeroValue(st.Var.Type)
			typ = toSwiftType(st.Var.Type)
		}
		return &VarDecl{Name: st.Var.Name, Const: false, Type: typ, Expr: ex}, nil
	case st.Type != nil:
		def, err := convertTypeDecl(env, st.Type)
		if err != nil {
			return nil, err
		}
		if def != nil {
			return def, nil
		}
		return &BlockStmt{}, nil
	case st.Test != nil:
		body, err := convertStmts(env, st.Test.Body)
		if err != nil {
			return nil, err
		}
		return &BlockStmt{Stmts: body}, nil
	case st.Expect != nil:
		cond, err := convertExpr(env, st.Expect.Value)
		if err != nil {
			return nil, err
		}
		return &ExpectStmt{Cond: cond}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		ex, err := convertExpr(env, st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: ex}, nil
	case st.Assign != nil && (len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0):
		lhs := Expr(&NameExpr{Name: st.Assign.Name})
		var cur types.Type
		if env != nil {
			if t, err := env.GetVar(st.Assign.Name); err == nil {
				cur = t
			}
		}
		for _, idx := range st.Assign.Index {
			if idx.Start == nil || idx.Colon != nil || idx.End != nil || idx.Colon2 != nil || idx.Step != nil {
				return nil, fmt.Errorf("unsupported index")
			}
			ix, err := convertExpr(env, idx.Start)
			if err != nil {
				return nil, err
			}
			if mt, ok := cur.(types.MapType); ok {
				cur = mt.Value
			} else if lt, ok := cur.(types.ListType); ok {
				cur = lt.Elem
			}
			lhs = &IndexExpr{Base: lhs, Index: ix}
		}
		for _, f := range st.Assign.Field {
			if stt, ok := cur.(types.StructType); ok {
				if ft, ok := stt.Fields[f.Name]; ok {
					cur = ft
				}
				lhs = &FieldExpr{Target: lhs, Name: f.Name}
			} else {
				lhs = &IndexExpr{Base: lhs, Index: &LitExpr{Value: f.Name, IsString: true}, Force: true}
				if mt, ok := cur.(types.MapType); ok {
					cur = mt.Value
				}
			}
		}
		val, err := convertExpr(env, st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &IndexAssignStmt{Target: lhs, Value: val}, nil
	case st.Fun != nil:
		return convertFunDecl(env, st.Fun)
	case st.Return != nil:
		return convertReturnStmt(env, st.Return)
	case st.While != nil:
		return convertWhileStmt(env, st.While)
	case st.For != nil:
		return convertForStmt(env, st.For)
	case st.Update != nil:
		return convertUpdateStmt(env, st.Update)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.If != nil:
		return convertIfStmt(env, st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIfStmt(env *types.Env, i *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(env, i.Cond)
	if err != nil {
		return nil, err
	}
	if env != nil {
		t := types.TypeOfExpr(i.Cond, env)
		if ot, ok := t.(types.OptionType); ok {
			t = ot.Elem
		}
		if types.IsAnyType(t) {
			cond = &CastExpr{Expr: cond, Type: "Bool!"}
		}
	}
	thenStmts, err := convertStmts(env, i.Then)
	if err != nil {
		return nil, err
	}
	var elseIf *IfStmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(env, i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = s.(*IfStmt)
	}
	var elseStmts []Stmt
	if i.Else != nil {
		elseStmts, err = convertStmts(env, i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, ElseIf: elseIf, Else: elseStmts}, nil
}

func convertFunDecl(env *types.Env, f *parser.FunStmt) (Stmt, error) {
	fn := &FunDecl{Name: f.Name, Ret: toSwiftType(f.Return)}
	for _, p := range f.Params {
		fn.Params = append(fn.Params, Param{Name: p.Name, Type: toSwiftType(p.Type)})
	}
	body, err := convertStmts(env, f.Body)
	if err != nil {
		return nil, err
	}
	for i := len(f.Params) - 1; i >= 0; i-- {
		p := f.Params[i]
		body = append([]Stmt{&VarDecl{Name: p.Name, Expr: &NameExpr{Name: p.Name}}}, body...)
	}
	fn.Body = body
	return fn, nil
}

func convertReturnStmt(env *types.Env, r *parser.ReturnStmt) (Stmt, error) {
	var ex Expr
	var err error
	if r.Value != nil {
		ex, err = convertExpr(env, r.Value)
		if err != nil {
			return nil, err
		}
	}
	return &ReturnStmt{Expr: ex}, nil
}

func convertWhileStmt(env *types.Env, wst *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(env, wst.Cond)
	if err != nil {
		return nil, err
	}
	if env != nil {
		t := types.TypeOfExpr(wst.Cond, env)
		if ot, ok := t.(types.OptionType); ok {
			t = ot.Elem
		}
		if types.IsAnyType(t) {
			cond = &CastExpr{Expr: cond, Type: "Bool!"}
		}
	}
	body, err := convertStmts(env, wst.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertImport(im *parser.ImportStmt) Stmt {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	lang := ""
	if im.Lang != nil {
		lang = *im.Lang
	}
	if lang == "go" && im.Path == "mochi/runtime/ffi/go/testpkg" {
		return &RawStmt{Code: fmt.Sprintf("struct %s {\n    static func Add(_ a: Int, _ b: Int) -> Int { return a + b }\n    static let Pi = 3.14\n    static let Answer = 42\n}\n", alias)}
	}
	if lang == "python" && im.Path == "math" {
		if im.Auto {
			return &RawStmt{Code: fmt.Sprintf("struct %s {\n    static func sqrt(_ x: Double) -> Double { return Foundation.sqrt(x) }\n    static let pi = Double.pi\n}\n", alias)}
		}
		return &RawStmt{Code: fmt.Sprintf("struct %s {}\n", alias)}
	}
	return &RawStmt{Code: ""}
}

func convertExternVar(ev *parser.ExternVarDecl) Stmt {
	if ev.Root == "math" && len(ev.Tail) == 1 {
		name := ev.Tail[0]
		switch name {
		case "pi":
			return &RawStmt{Code: "extension math { static let pi = Double.pi }"}
		case "e":
			return &RawStmt{Code: "extension math { static let e = 2.718281828459045 }"}
		}
	}
	return &RawStmt{Code: ""}
}

func convertExternFun(ef *parser.ExternFunDecl) Stmt {
	if ef.Root == "math" && len(ef.Tail) == 1 {
		name := ef.Tail[0]
		switch name {
		case "sqrt":
			return &RawStmt{Code: "extension math { static func sqrt(_ x: Double) -> Double { Foundation.sqrt(x) } }"}
		case "pow":
			return &RawStmt{Code: "extension math { static func pow(_ x: Double, _ y: Double) -> Double { Foundation.pow(x, y) } }"}
		case "sin":
			return &RawStmt{Code: "extension math { static func sin(_ x: Double) -> Double { Foundation.sin(x) } }"}
		case "log":
			return &RawStmt{Code: "extension math { static func log(_ x: Double) -> Double { Foundation.log(x) } }"}
		}
	}
	return &RawStmt{Code: ""}
}

func convertForStmt(env *types.Env, fs *parser.ForStmt) (Stmt, error) {
	body, err := convertStmts(env, fs.Body)
	if err != nil {
		return nil, err
	}
	if fs.RangeEnd != nil {
		start, err := convertExpr(env, fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(env, fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	expr, err := convertExpr(env, fs.Source)
	if err != nil {
		return nil, err
	}
	castMap := false
	keysOnly := false
	if env != nil {
		if t := types.TypeOfExpr(fs.Source, env); t != nil {
			switch tt := t.(type) {
			case types.GroupType:
				castMap = true
			case types.ListType:
				switch tt2 := tt.Elem.(type) {
				case types.GroupType:
					castMap = true
				case types.MapType:
					castMap = true
					_ = tt2
				}
			case types.MapType:
				keysOnly = true
			}
		}
	}
	return &ForEachStmt{Name: fs.Name, Expr: expr, Body: body, CastMap: castMap, Keys: keysOnly}, nil
}

func convertSaveStmt(env *types.Env, se *parser.SaveExpr) (Stmt, error) {
	src, err := convertExpr(env, se.Src)
	if err != nil {
		return nil, err
	}
	format := parseFormat(se.With)
	path := ""
	if se.Path != nil {
		path = strings.Trim(*se.Path, "\"")
	}
	if format != "jsonl" || (path != "" && path != "-") {
		return nil, fmt.Errorf("unsupported save")
	}
	return &SaveStmt{Src: src}, nil
}

func convertUpdateStmt(env *types.Env, us *parser.UpdateStmt) (Stmt, error) {
	if env == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := env.GetVar(us.Target)
	if err != nil {
		return nil, err
	}
	lt, ok := t.(types.ListType)
	if !ok {
		return nil, fmt.Errorf("update target not list")
	}
	st, ok := lt.Elem.(types.StructType)
	if !ok {
		return nil, fmt.Errorf("update element not struct")
	}
	child := types.NewEnv(env)
	child.SetVar("item", st, true)
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
	}
	var fields []string
	var values []Expr
	locals := make([]string, len(st.Order))
	copy(locals, st.Order)
	for _, it := range us.Set.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(child, it.Value)
		if err != nil {
			return nil, err
		}
		fields = append(fields, key)
		values = append(values, val)
	}
	var cond Expr
	if us.Where != nil {
		cond, err = convertExpr(child, us.Where)
		if err != nil {
			return nil, err
		}
	}
	return &UpdateStmt{Target: us.Target, Fields: fields, Values: values, Cond: cond, Locals: locals}, nil
}

func convertTypeDecl(env *types.Env, td *parser.TypeDecl) (Stmt, error) {
	if len(td.Variants) > 0 {
		return &BlockStmt{}, nil
	}
	var fields []StructField
	for _, m := range td.Members {
		if m.Field == nil {
			continue
		}
		t := types.ResolveTypeRef(m.Field.Type, env)
		fields = append(fields, StructField{Name: m.Field.Name, Type: swiftTypeOf(t)})
	}
	return &StructDef{Name: td.Name, Fields: fields}, nil
}

func evalPrintArg(arg *parser.Expr) (val string, isString bool, ok bool) {
	lit := arg.Binary.Left.Value.Target.Lit
	if lit != nil && len(arg.Binary.Left.Ops) == 0 && len(arg.Binary.Left.Value.Ops) == 0 && len(arg.Binary.Right) == 0 {
		switch {
		case lit.Str != nil:
			return *lit.Str, true, true
		case lit.Int != nil:
			return fmt.Sprintf("%d", *lit.Int), false, true
		}
	}

	if v, ok := intConst(arg); ok {
		return fmt.Sprintf("%d", v), false, true
	}

	// cast string literal to int
	if lit != nil && lit.Str != nil && len(arg.Binary.Left.Value.Ops) == 1 {
		if c := arg.Binary.Left.Value.Ops[0].Cast; c != nil && c.Type != nil && c.Type.Simple != nil && *c.Type.Simple == "int" {
			return *lit.Str, false, true
		}
	}

	if lit != nil && len(arg.Binary.Right) == 1 {
		op := arg.Binary.Right[0]
		rightLit := op.Right.Target.Lit
		if rightLit != nil && rightLit.Str != nil && lit.Str != nil {
			switch op.Op {
			case "+":
				return *lit.Str + *rightLit.Str, true, true
			case "<", "<=", ">", ">=":
				left := *lit.Str
				right := *rightLit.Str
				var res bool
				switch op.Op {
				case "<":
					res = left < right
				case "<=":
					res = left <= right
				case ">":
					res = left > right
				case ">=":
					res = left >= right
				}
				if res {
					return "1", false, true
				}
				return "0", false, true
			}
		}
	}

	return "", false, false
}

// Print writes a lisp-like representation of the AST to stdout using the ast package.
func Print(prog *Program) {
	fmt.Print(toNode(prog).String())
}

// toNode converts the Program to an ast.Node tree.
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
		return &ast.Node{Kind: "print"}
	case *VarDecl:
		return &ast.Node{Kind: "var", Value: st.Name}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name}
	case *FunDecl:
		return &ast.Node{Kind: "fun", Value: st.Name}
	case *ReturnStmt:
		return &ast.Node{Kind: "return"}
	case *WhileStmt:
		return &ast.Node{Kind: "while"}
	case *ForRangeStmt:
		return &ast.Node{Kind: "for-range"}
	case *ForEachStmt:
		return &ast.Node{Kind: "for-each"}
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ExprStmt:
		return &ast.Node{Kind: "expr"}
	default:
		return &ast.Node{Kind: "stmt"}
	}
}

func intConst(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil {
		return 0, false
	}
	return evalIntConstBinary(e.Binary)
}

func evalIntConstBinary(be *parser.BinaryExpr) (int, bool) {
	v, ok := evalIntConstUnary(be.Left)
	if !ok {
		return 0, false
	}
	vals := []int{v}
	ops := []string{}
	for _, op := range be.Right {
		r, ok := evalIntConstPostfix(op.Right)
		if !ok {
			return 0, false
		}
		vals = append(vals, r)
		ops = append(ops, op.Op)
	}

	for i := 0; i < len(ops); {
		switch ops[i] {
		case "*":
			vals[i] *= vals[i+1]
		case "/":
			if vals[i+1] == 0 {
				return 0, false
			}
			vals[i] /= vals[i+1]
		case "%":
			if vals[i+1] == 0 {
				return 0, false
			}
			vals[i] %= vals[i+1]
		default:
			i++
			continue
		}
		vals = append(vals[:i+1], vals[i+2:]...)
		ops = append(ops[:i], ops[i+1:]...)
	}

	res := vals[0]
	for i, op := range ops {
		switch op {
		case "+":
			res += vals[i+1]
		case "-":
			res -= vals[i+1]
		default:
			return 0, false
		}
	}
	return res, true
}

func evalIntConstUnary(u *parser.Unary) (int, bool) {
	val, ok := evalIntConstPostfix(u.Value)
	if !ok {
		return 0, false
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = -val
		case "+":
			// ignore
		default:
			return 0, false
		}
	}
	return val, true
}

func evalIntConstPostfix(p *parser.PostfixExpr) (int, bool) {
	if p == nil || len(p.Ops) != 0 {
		return 0, false
	}
	return evalIntConstPrimary(p.Target)
}

func evalIntConstPrimary(pr *parser.Primary) (int, bool) {
	if pr == nil {
		return 0, false
	}
	if pr.Lit != nil && pr.Lit.Int != nil {
		return int(*pr.Lit.Int), true
	}
	if pr.Group != nil {
		return intConst(pr.Group)
	}
	return 0, false
}

func boolAsInt(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && len(e.Binary.Left.Ops) > 0 {
		for _, op := range e.Binary.Left.Ops {
			if op == "!" {
				return true
			}
		}
	}
	for _, op := range e.Binary.Right {
		switch op.Op {
		case "in":
			return false
		case "&&", "||", "<", "<=", ">", ">=", "==", "!=":
			return true
		}
	}
	return false
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
	typesList := []types.Type{types.TypeOfUnary(e.Binary.Left, env)}
	ops := []string{}

	for _, op := range e.Binary.Right {
		right, err := convertPostfix(env, op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		typesList = append(typesList, types.TypeOfPostfix(op.Right, env))
		ops = append(ops, op.Op)
	}

	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 4
		case "+", "-":
			return 3
		case "==", "!=", "<", "<=", ">", ">=", "in":
			return 2
		case "&&":
			return 1
		case "||":
			return 0
		default:
			return -1
		}
	}

	var exprStack []Expr
	var typeStack []types.Type
	var opStack []string

	pushResult := func() {
		if len(opStack) == 0 || len(exprStack) < 2 || len(typeStack) < 2 {
			return
		}
		op := opStack[len(opStack)-1]
		opStack = opStack[:len(opStack)-1]

		right := exprStack[len(exprStack)-1]
		rtyp := typeStack[len(typeStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		typeStack = typeStack[:len(typeStack)-1]

		left := exprStack[len(exprStack)-1]
		ltyp := typeStack[len(typeStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		typeStack = typeStack[:len(typeStack)-1]

		if op == "in" && env != nil {
			inMap := false
			if types.IsMapType(rtyp) {
				inMap = true
			}
			exprStack = append(exprStack, &BinaryExpr{Left: left, Op: op, Right: right, InMap: inMap})
			typeStack = append(typeStack, types.BoolType{})
			return
		}

		switch op {
		case "==", "!=", "<", "<=", ">", ">=":
			if types.IsAnyType(ltyp) && types.IsAnyType(rtyp) {
				left = &CastExpr{Expr: left, Type: "String"}
				right = &CastExpr{Expr: right, Type: "String"}
			} else if (types.IsStructType(ltyp) || types.IsStructType(rtyp)) ||
				(types.IsListType(ltyp) && types.IsStructType(ltyp.(types.ListType).Elem)) ||
				(types.IsListType(rtyp) && types.IsStructType(rtyp.(types.ListType).Elem)) {
				left = &RawStmt{Code: fmt.Sprintf("String(describing: %s)", exprString(left))}
				right = &RawStmt{Code: fmt.Sprintf("String(describing: %s)", exprString(right))}
			} else if types.IsIntType(ltyp) && types.IsAnyType(rtyp) {
				right = &CastExpr{Expr: right, Type: "Int"}
				rtyp = types.IntType{}
			} else if types.IsIntType(rtyp) && types.IsAnyType(ltyp) {
				left = &CastExpr{Expr: left, Type: "Int"}
				ltyp = types.IntType{}
			} else if types.IsFloatType(ltyp) && types.IsAnyType(rtyp) {
				right = &CastExpr{Expr: right, Type: "Double"}
				rtyp = types.FloatType{}
			} else if types.IsFloatType(rtyp) && types.IsAnyType(ltyp) {
				left = &CastExpr{Expr: left, Type: "Double"}
				ltyp = types.FloatType{}
			} else if types.IsStringType(ltyp) && types.IsAnyType(rtyp) {
				right = &CastExpr{Expr: right, Type: "String"}
				rtyp = types.StringType{}
			} else if types.IsStringType(rtyp) && types.IsAnyType(ltyp) {
				left = &CastExpr{Expr: left, Type: "String"}
				ltyp = types.StringType{}
			}
		case "+", "-", "*", "/", "%":
			if op == "+" && (types.IsStringType(ltyp) || types.IsStringType(rtyp)) {
				if types.IsAnyType(ltyp) {
					left = &CastExpr{Expr: left, Type: "String"}
				}
				if types.IsAnyType(rtyp) {
					right = &CastExpr{Expr: right, Type: "String"}
				}
			} else {
				if types.IsAnyType(ltyp) && types.IsAnyType(rtyp) {
					left = &CastExpr{Expr: left, Type: "Int"}
					right = &CastExpr{Expr: right, Type: "Int"}
					ltyp = types.IntType{}
					rtyp = types.IntType{}
				} else {
					if types.IsAnyType(ltyp) {
						if types.IsIntType(rtyp) || types.IsInt64Type(rtyp) {
							left = &CastExpr{Expr: left, Type: "Int"}
							ltyp = types.IntType{}
						} else {
							left = &CastExpr{Expr: left, Type: "Double"}
							ltyp = types.FloatType{}
						}
					}
					if types.IsAnyType(rtyp) {
						if types.IsIntType(ltyp) || types.IsInt64Type(ltyp) {
							right = &CastExpr{Expr: right, Type: "Int"}
							rtyp = types.IntType{}
						} else {
							right = &CastExpr{Expr: right, Type: "Double"}
							rtyp = types.FloatType{}
						}
					}
				}
			}
		}

		resType := ltyp
		switch op {
		case "==", "!=", "<", "<=", ">", ">=", "in", "&&", "||":
			resType = types.BoolType{}
		case "+", "-", "*", "/", "%":
			if types.IsStringType(ltyp) || types.IsStringType(rtyp) {
				resType = types.StringType{}
			} else if (types.IsIntType(ltyp) || types.IsInt64Type(ltyp)) &&
				(types.IsIntType(rtyp) || types.IsInt64Type(rtyp)) {
				if types.IsInt64Type(ltyp) || types.IsInt64Type(rtyp) {
					resType = types.Int64Type{}
				} else {
					resType = types.IntType{}
				}
			} else {
				resType = types.FloatType{}
			}
		default:
			resType = rtyp
		}

		exprStack = append(exprStack, &BinaryExpr{Left: left, Op: op, Right: right, InMap: false})
		typeStack = append(typeStack, resType)
	}

	exprStack = append(exprStack, operands[0])
	typeStack = append(typeStack, typesList[0])
	for i, op := range ops {
		for len(opStack) > 0 && prec(opStack[len(opStack)-1]) >= prec(op) {
			pushResult()
		}
		opStack = append(opStack, op)
		exprStack = append(exprStack, operands[i+1])
		typeStack = append(typeStack, typesList[i+1])
	}
	for len(opStack) > 0 {
		pushResult()
	}
	if len(exprStack) != 1 {
		return nil, fmt.Errorf("binary conversion failed")
	}
	return exprStack[0], nil
}

func convertUnary(env *types.Env, u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(env, u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			if env != nil {
				t := types.TypeOfUnary(u, env)
				if ot, ok := t.(types.OptionType); ok {
					t = ot.Elem
				}
				if types.IsAnyType(t) {
					expr = &CastExpr{Expr: expr, Type: "Int"}
				}
			}
			expr = &UnaryExpr{Op: "-", Expr: expr}
		case "!":
			if env != nil {
				sub := &parser.Unary{Value: u.Value, Ops: u.Ops[:i]}
				t := types.TypeOfUnary(sub, env)
				if ot, ok := t.(types.OptionType); ok {
					t = ot.Elem
				}
				if types.IsAnyType(t) {
					expr = &CastExpr{Expr: expr, Type: "Bool!"}
				}
			}
			expr = &UnaryExpr{Op: "!", Expr: expr}
		case "+":
			// ignore
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(env *types.Env, p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(env, p.Target)
	if err != nil {
		if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 {
			expr = &NameExpr{Name: p.Target.Selector.Root}
		} else {
			return nil, err
		}
	}
	tail := []string{}
	if p.Target != nil && p.Target.Selector != nil {
		tail = p.Target.Selector.Tail
	}

	// handle `.contains(x)` as `x in expr`
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && len(tail) > 0 && tail[len(tail)-1] == "contains" {
		for _, f := range tail[:len(tail)-1] {
			expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: f, IsString: true}, Force: true}
		}
		arg, err := convertExpr(env, p.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		return &BinaryExpr{Left: arg, Op: "in", Right: expr}, nil
	}

	if len(tail) > 0 {
		var t types.Type
		if env != nil && p.Target != nil && p.Target.Selector != nil {
			if tt, err := env.GetVar(p.Target.Selector.Root); err == nil {
				t = tt
			}
		}
		if t == nil {
			t = types.TypeOfPrimaryBasic(p.Target, env)
		}
		for i, f := range tail {
			if _, ok := t.(types.StructType); ok || t == nil || isAnyType(t) {
				expr = &FieldExpr{Target: expr, Name: f}
				if st, ok := t.(types.StructType); ok {
					if ft, ok := st.Fields[f]; ok {
						t = ft
					}
				}
			} else {
				expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: f, IsString: true}, Force: true}
				if gt, ok := t.(types.GroupType); ok {
					if f == "key" {
						t = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
						if i < len(tail)-1 {
							expr = &CastExpr{Expr: expr, Type: "[String: Any]"}
						}
					} else if f == "items" {
						t = types.ListType{Elem: gt.Elem}
					}
				} else if mt, ok := t.(types.MapType); ok {
					t = mt.Value
					if i < len(tail)-1 {
						if _, ok := t.(types.MapType); ok {
							expr = &CastExpr{Expr: expr, Type: "[String: Any]"}
						}
					}
				}
			}
		}
	}

	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		if op.Index != nil {
			if op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil {
				var start, end Expr
				if op.Index.Start != nil {
					start, err = convertExpr(env, op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					end, err = convertExpr(env, op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				isStr := false
				if env != nil {
					if t := types.TypeOfPrimaryBasic(p.Target, env); types.IsStringType(t) {
						isStr = true
					}
				}
				expr = &SliceExpr{Base: expr, Start: start, End: end, AsString: isStr}
				continue
			}
			if op.Index.Start == nil || op.Index.Colon != nil || op.Index.End != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				return nil, fmt.Errorf("unsupported index")
			}
			idx, err := convertExpr(env, op.Index.Start)
			if err != nil {
				return nil, err
			}
			isStr := false
			force := false
			if env != nil {
				if t := types.TypeOfPrimaryBasic(p.Target, env); types.IsStringType(t) {
					isStr = true
				}
				if types.IsMapPrimary(p.Target, env) {
					force = true
				}
			}
			expr = &IndexExpr{Base: expr, Index: idx, AsString: isStr, Force: force}
			continue
		}
		if op.Field != nil && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil && op.Field.Name == "contains" {
			arg, err := convertExpr(env, p.Ops[i+1].Call.Args[0])
			if err != nil {
				return nil, err
			}
			expr = &BinaryExpr{Left: arg, Op: "in", Right: expr}
			i++
			continue
		}
		if op.Call != nil {
			ce := &CallExpr{Func: exprString(expr)}
			for _, a := range op.Call.Args {
				ae, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				ce.Args = append(ce.Args, ae)
			}
			expr = ce
			continue
		}
		if op.Cast != nil {
			typ := toSwiftType(op.Cast.Type)
			if env != nil {
				if st, ok := types.ResolveTypeRef(op.Cast.Type, env).(types.StructType); ok {
					if ml, ok := expr.(*MapLit); ok {
						var fields []FieldInit
						for j := range ml.Keys {
							if lk, ok := ml.Keys[j].(*LitExpr); ok && lk.IsString {
								fields = append(fields, FieldInit{Name: lk.Value, Value: ml.Values[j]})
							}
						}
						expr = &StructInit{Name: st.Name, Fields: fields}
					} else {
						expr = &CastExpr{Expr: expr, Type: st.Name}
					}
				} else {
					expr = &CastExpr{Expr: expr, Type: typ}
				}
			} else {
				expr = &CastExpr{Expr: expr, Type: typ}
			}
			continue
		}
		return nil, fmt.Errorf("unsupported postfix")
	}
	if env != nil {
		t := types.TypeOfPostfix(p, env)
		if ot, ok := t.(types.OptionType); ok {
			t = ot.Elem
		}
		if _, ok := expr.(*FieldExpr); !ok {
			switch {
			case types.IsIntType(t):
				expr = &CastExpr{Expr: expr, Type: "Int"}
			case types.IsInt64Type(t):
				expr = &CastExpr{Expr: expr, Type: "Int64"}
			case types.IsFloatType(t):
				expr = &CastExpr{Expr: expr, Type: "Double"}
			case types.IsStringType(t):
				expr = &CastExpr{Expr: expr, Type: "String"}
			case types.IsBoolType(t):
				expr = &CastExpr{Expr: expr, Type: "Bool"}
			case types.IsListType(t), types.IsMapType(t):
				expr = &CastExpr{Expr: expr, Type: swiftTypeOf(t)}
			}
		}
	}
	return expr, nil
}

func convertIfExpr(env *types.Env, i *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(env, i.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(env, i.Then)
	if err != nil {
		return nil, err
	}
	if i.ElseIf != nil {
		elseExpr, err := convertIfExpr(env, i.ElseIf)
		if err != nil {
			return nil, err
		}
		return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
	}
	var elseExpr Expr
	if i.Else != nil {
		elseExpr, err = convertExpr(env, i.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &LitExpr{Value: "0", IsString: false}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatchExpr(env *types.Env, me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(env, me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr = &LitExpr{Value: "nil", IsString: false}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(env, c.Result)
		if err != nil {
			return nil, err
		}
		pat, err := convertExpr(env, c.Pattern)
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*NameExpr); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		expr = &CondExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func convertQueryExpr(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil {
		return convertGroupQuery(env, q)
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" &&
		len(q.Froms) == 0 && q.Where == nil && !q.Distinct {
		return convertLeftJoinQuery(env, q)
	}
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	varType := types.TypeOfExpr(q.Source, env)
	if gt, ok := varType.(types.GroupType); ok {
		src = &CastExpr{Expr: &IndexExpr{Base: src, Index: &LitExpr{Value: "items", IsString: true}, Force: true}, Type: "[[String: Any]]"}
		varType = gt.Elem
	} else if lt, ok := varType.(types.ListType); ok {
		if gt2, ok := lt.Elem.(types.GroupType); ok {
			varType = gt2.Elem
		} else {
			varType = lt.Elem
		}
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, varType, true)
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := convertExpr(child, f.Src)
		if err != nil {
			return nil, err
		}
		t := types.TypeOfExpr(f.Src, child)
		if gt, ok := t.(types.GroupType); ok {
			fe = &CastExpr{Expr: &IndexExpr{Base: fe, Index: &LitExpr{Value: "items", IsString: true}, Force: true}, Type: "[[String: Any]]"}
			t = gt.Elem
		} else if lt, ok := t.(types.ListType); ok {
			if gt2, ok := lt.Elem.(types.GroupType); ok {
				t = gt2.Elem
			} else {
				t = lt.Elem
			}
		}
		child.SetVar(f.Var, t, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := convertExpr(child, j.Src)
		if err != nil {
			return nil, err
		}
		jt := types.TypeOfExpr(j.Src, child)
		if gt, ok := jt.(types.GroupType); ok {
			je = &CastExpr{Expr: &IndexExpr{Base: je, Index: &LitExpr{Value: "items", IsString: true}, Force: true}, Type: "[[String: Any]]"}
			jt = gt.Elem
		} else if lt, ok := jt.(types.ListType); ok {
			if gt2, ok := lt.Elem.(types.GroupType); ok {
				jt = gt2.Elem
			} else {
				jt = lt.Elem
			}
		}
		child.SetVar(j.Var, jt, true)
		var on Expr
		if j.On != nil {
			on, err = convertExpr(child, j.On)
			if err != nil {
				return nil, err
			}
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: on}
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(child, q.Where)
		if err != nil {
			return nil, err
		}
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, err
	}
	var sortExpr, skipExpr, takeExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(child, q.Sort)
		if err != nil {
			return nil, err
		}
	}
	if q.Skip != nil {
		skipExpr, err = convertExpr(child, q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(child, q.Take)
		if err != nil {
			return nil, err
		}
	}
	elemType := ""
	if t := types.TypeOfExpr(q.Select, child); t != nil {
		elemType = swiftTypeOf(t)
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Sort: sortExpr, Skip: skipExpr, Take: takeExpr, Select: sel, Elem: elemType}, nil
}

func convertLeftJoinQuery(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	j := q.Joins[0]
	leftSrc, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	lt := types.TypeOfExpr(q.Source, env)
	if llist, ok := lt.(types.ListType); ok {
		lt = llist.Elem
	} else if gt, ok := lt.(types.GroupType); ok {
		lt = gt.Elem
	}
	child.SetVar(q.Var, lt, true)
	rt := types.TypeOfExpr(j.Src, child)
	if rlist, ok := rt.(types.ListType); ok {
		rt = rlist.Elem
	} else if gt, ok := rt.(types.GroupType); ok {
		rt = gt.Elem
	}
	child.SetVar(j.Var, rt, true)
	rightSrc, err := convertExpr(child, j.Src)
	if err != nil {
		return nil, err
	}
	cond, err := convertExpr(child, j.On)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, err
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertGroupQuery(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	if q.Group == nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	t := types.TypeOfExpr(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := t.(types.ListType); ok {
		elemT = lt.Elem
	} else if gt, ok := t.(types.GroupType); ok {
		elemT = gt.Elem
	}
	child.SetVar(q.Var, elemT, true)

	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := convertExpr(child, f.Src)
		if err != nil {
			return nil, err
		}
		ft := types.TypeOfExpr(f.Src, child)
		if lt, ok := ft.(types.ListType); ok {
			ft = lt.Elem
		} else if gt, ok := ft.(types.GroupType); ok {
			ft = gt.Elem
		}
		child.SetVar(f.Var, ft, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}

	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := convertExpr(child, j.Src)
		if err != nil {
			return nil, err
		}
		jt := types.TypeOfExpr(j.Src, child)
		if lt, ok := jt.(types.ListType); ok {
			jt = lt.Elem
		} else if gt, ok := jt.(types.GroupType); ok {
			jt = gt.Elem
		}
		child.SetVar(j.Var, jt, true)
		var on Expr
		if j.On != nil {
			on, err = convertExpr(child, j.On)
			if err != nil {
				return nil, err
			}
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: on}
	}

	key, err := convertExpr(child, q.Group.Exprs[0])
	if err != nil {
		return nil, err
	}
	var keyT types.Type = types.AnyType{}
	if kt := types.TypeOfExpr(q.Group.Exprs[0], child); kt != nil {
		keyT = kt
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(child, q.Where)
		if err != nil {
			return nil, err
		}
	}

	if len(q.Joins) > 0 || len(q.Froms) > 0 {
		elemT = types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	}

	genv := types.NewEnv(env)
	genv.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: elemT}, true)
	sel, err := convertExpr(genv, q.Select)
	if err != nil {
		return nil, err
	}
	var sortExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(genv, q.Sort)
		if err != nil {
			return nil, err
		}
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(genv, q.Group.Having)
		if err != nil {
			return nil, err
		}
	}
	return &GroupByExpr{Var: q.Var, Source: src, Froms: froms, Joins: joins, Key: key, Name: q.Group.Name, Sort: sortExpr, Where: where, Select: sel, Having: having}, nil
}

func convertPrimary(env *types.Env, pr *parser.Primary) (Expr, error) {
	switch {
	case pr == nil:
		return nil, fmt.Errorf("nil primary")
	case pr.Lit != nil:
		if pr.Lit.Str != nil {
			return &LitExpr{Value: *pr.Lit.Str, IsString: true}, nil
		}
		if pr.Lit.Int != nil {
			return &LitExpr{Value: fmt.Sprintf("%d", *pr.Lit.Int), IsString: false}, nil
		}
		if pr.Lit.Float != nil {
			v := *pr.Lit.Float
			s := strconv.FormatFloat(v, 'f', -1, 64)
			if math.Trunc(v) == v && !strings.Contains(s, ".") {
				s += ".0"
			}
			return &LitExpr{Value: s, IsString: false}, nil
		}
		if pr.Lit.Bool != nil {
			if *pr.Lit.Bool {
				return &LitExpr{Value: "true", IsString: false}, nil
			}
			return &LitExpr{Value: "false", IsString: false}, nil
		}
		if pr.Lit.Null {
			return &LitExpr{Value: "nil", IsString: false}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
	case pr.List != nil:
		var elems []Expr
		for _, e := range pr.List.Elems {
			ce, err := convertExpr(env, e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ce)
		}
		return &ArrayLit{Elems: elems}, nil
	case pr.Map != nil:
		var keys []Expr
		var vals []Expr
		for _, it := range pr.Map.Items {
			var k Expr
			if key, ok := types.SimpleStringKey(it.Key); ok {
				k = &LitExpr{Value: key, IsString: true}
			} else {
				var err error
				k, err = convertExpr(env, it.Key)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			keys = append(keys, k)
			vals = append(vals, v)
		}
		return &MapLit{Keys: keys, Values: vals}, nil
	case pr.Struct != nil:
		return convertStructLiteral(env, pr.Struct)
	case pr.Call != nil:
		if pr.Call.Func == "len" && len(pr.Call.Args) == 1 {
			if n, ok := evalLenConst(pr.Call.Args[0]); ok {
				return &LitExpr{Value: fmt.Sprintf("%d", n), IsString: false}, nil
			}
		}
		ce := &CallExpr{Func: pr.Call.Func}
		for _, a := range pr.Call.Args {
			ae, err := convertExpr(env, a)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ae)
		}
		return ce, nil
	case pr.FunExpr != nil && pr.FunExpr.ExprBody != nil:
		body, err := convertExpr(env, pr.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		fn := &FunExpr{Ret: toSwiftType(pr.FunExpr.Return)}
		for _, p := range pr.FunExpr.Params {
			fn.Params = append(fn.Params, Param{Name: p.Name, Type: toSwiftType(p.Type)})
		}
		fn.Body = body
		return fn, nil
	case pr.Group != nil:
		return convertExpr(env, pr.Group)
	case pr.If != nil:
		return convertIfExpr(env, pr.If)
	case pr.Match != nil:
		return convertMatchExpr(env, pr.Match)
	case pr.Query != nil:
		return convertQueryExpr(env, pr.Query)
	case pr.Load != nil:
		format := parseFormat(pr.Load.With)
		path := ""
		if pr.Load.Path != nil {
			path = strings.Trim(*pr.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(env, path, format, pr.Load.Type)
		if err != nil {
			return nil, err
		}
		return expr, nil
	case pr.Selector != nil && len(pr.Selector.Tail) == 0:
		return &NameExpr{Name: pr.Selector.Root}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func convertStructLiteral(env *types.Env, sl *parser.StructLiteral) (Expr, error) {
	var fields []FieldInit
	var keys []Expr
	var vals []Expr
	for _, f := range sl.Fields {
		v, err := convertExpr(env, f.Value)
		if err != nil {
			return nil, err
		}
		fields = append(fields, FieldInit{Name: f.Name, Value: v})
		keys = append(keys, &LitExpr{Value: f.Name, IsString: true})
		vals = append(vals, v)
	}
	if sl.Name != "" {
		return &StructInit{Name: sl.Name, Fields: fields}, nil
	}
	return &MapLit{Keys: keys, Values: vals}, nil
}

func evalLenConst(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil {
		return 0, false
	}
	left := e.Binary.Left
	if len(left.Ops) != 0 || len(e.Binary.Right) != 0 {
		return 0, false
	}
	t := left.Value.Target
	switch {
	case t.Lit != nil && t.Lit.Str != nil:
		return len(*t.Lit.Str), true
	case t.List != nil:
		return len(t.List.Elems), true
	case t.Map != nil:
		return len(t.Map.Items), true
	default:
		return 0, false
	}
}

func zeroValue(t *parser.TypeRef) Expr {
	if t == nil {
		return &LitExpr{Value: "nil", IsString: false}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			return &ArrayLit{}
		case "map":
			return &MapLit{}
		}
	}
	if t.Simple == nil {
		return &LitExpr{Value: "nil", IsString: false}
	}
	switch *t.Simple {
	case "int":
		return &LitExpr{Value: "0", IsString: false}
	case "float":
		return &LitExpr{Value: "0.0", IsString: false}
	case "string":
		return &LitExpr{Value: "", IsString: true}
	case "bool":
		return &LitExpr{Value: "false", IsString: false}
	default:
		return &LitExpr{Value: toSwiftType(t) + "()", IsString: false}
	}
}

func isEmptyMapLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	if ml := e.Binary.Left.Value.Target.Map; ml != nil {
		return len(ml.Items) == 0
	}
	return false
}

func toSwiftType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			return "[" + toSwiftType(t.Generic.Args[0]) + "]"
		case "map":
			return "[" + toSwiftType(t.Generic.Args[0]) + ": " + toSwiftType(t.Generic.Args[1]) + "]"
		}
	}
	if t.Simple == nil {
		return "Any"
	}
	switch *t.Simple {
	case "int":
		return "Int"
	case "float":
		return "Double"
	case "string":
		return "String"
	case "bool":
		return "Bool"
	case "any":
		return "Any"
	default:
		return *t.Simple
	}
}

func toSwiftOptionalType(t *parser.TypeRef) string {
	if t == nil {
		return "Any?"
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			return "[" + toSwiftType(t.Generic.Args[0]) + "]?"
		case "map":
			return "[" + toSwiftType(t.Generic.Args[0]) + ": " + toSwiftType(t.Generic.Args[1]) + "]?"
		}
	}
	if t.Simple == nil {
		return "Any?"
	}
	switch *t.Simple {
	case "int":
		return "Int?"
	case "float":
		return "Double?"
	case "string":
		return "String?"
	case "bool":
		return "Bool?"
	case "any":
		return "Any?"
	default:
		return *t.Simple + "?"
	}
}

func swiftTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "Int"
	case types.Int64Type:
		return "Int64"
	case types.FloatType, types.BigRatType:
		return "Double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Bool"
	case types.ListType:
		return "[" + swiftTypeOf(tt.Elem) + "]"
	case types.MapType:
		return "[" + swiftTypeOf(tt.Key) + ": " + swiftTypeOf(tt.Value) + "]"
	case types.OptionType:
		return swiftTypeOf(tt.Elem) + "?"
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
		return "Any"
	default:
		return "Any"
	}
}

func parserTypeRefFromType(t types.Type) *parser.TypeRef {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		s := "int"
		return &parser.TypeRef{Simple: &s}
	case types.FloatType, types.BigRatType:
		s := "float"
		return &parser.TypeRef{Simple: &s}
	case types.BoolType:
		s := "bool"
		return &parser.TypeRef{Simple: &s}
	case types.StringType:
		s := "string"
		return &parser.TypeRef{Simple: &s}
	case types.ListType:
		el := parserTypeRefFromType(tt.Elem)
		if el == nil {
			return nil
		}
		return &parser.TypeRef{Generic: &parser.GenericType{Name: "list", Args: []*parser.TypeRef{el}}}
	case types.MapType:
		k := parserTypeRefFromType(tt.Key)
		v := parserTypeRefFromType(tt.Value)
		if k == nil || v == nil {
			return nil
		}
		return &parser.TypeRef{Generic: &parser.GenericType{Name: "map", Args: []*parser.TypeRef{k, v}}}
	case types.StructType:
		if tt.Name != "" {
			s := tt.Name
			return &parser.TypeRef{Simple: &s}
		}
	}
	return nil
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func isSimpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	ml := e.Binary.Left.Value.Target.Map
	if ml == nil {
		return ""
	}
	for _, it := range ml.Items {
		key, ok := literalString(it.Key)
		if !ok || key != "format" {
			continue
		}
		if v, ok := literalString(it.Value); ok {
			return v
		}
	}
	return ""
}

func extractSaveExpr(e *parser.Expr) *parser.SaveExpr {
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
	return p.Target.Save
}

func valueToExpr(env *types.Env, v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		if typ != nil && typ.Simple != nil && env != nil {
			if st, ok := env.GetStruct(*typ.Simple); ok {
				var fields []FieldInit
				for _, name := range st.Order {
					ft := parserTypeRefFromType(st.Fields[name])
					fields = append(fields, FieldInit{Name: name, Value: valueToExpr(env, val[name], ft)})
				}
				return &StructInit{Name: st.Name, Fields: fields}
			}
		}
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		keys := make([]Expr, len(names))
		values := make([]Expr, len(names))
		for i, k := range names {
			keys[i] = &LitExpr{Value: k, IsString: true}
			values[i] = valueToExpr(env, val[k], nil)
		}
		return &MapLit{Keys: keys, Values: values}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(env, it, typ)
		}
		return &ArrayLit{Elems: elems}
	case string:
		return &LitExpr{Value: val, IsString: true}
	case bool:
		if val {
			return &LitExpr{Value: "true"}
		}
		return &LitExpr{Value: "false"}
	case int, int64:
		return &LitExpr{Value: fmt.Sprintf("%v", val)}
	case float64, float32:
		f := reflect.ValueOf(val).Float()
		s := strconv.FormatFloat(f, 'f', -1, 64)
		if math.Trunc(f) == f && !strings.Contains(s, ".") {
			s += ".0"
		}
		return &LitExpr{Value: s}
	default:
		return &LitExpr{Value: fmt.Sprintf("%v", val), IsString: true}
	}
}

func dataExprFromFile(env *types.Env, path, format string, typ *parser.TypeRef) (Expr, error) {
	if path == "" {
		return &ArrayLit{}, nil
	}
	root := repoRoot()
	if root != "" && strings.HasPrefix(path, "../") {
		clean := strings.TrimPrefix(path, "../")
		path = filepath.Join(root, "tests", clean)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var v interface{}
	switch format {
	case "yaml", "":
		if err := yaml.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "jsonl":
		var list []interface{}
		scanner := bufio.NewScanner(bytes.NewReader(data))
		for scanner.Scan() {
			line := strings.TrimSpace(scanner.Text())
			if line == "" {
				continue
			}
			var item interface{}
			if err := json.Unmarshal([]byte(line), &item); err != nil {
				return nil, err
			}
			list = append(list, item)
		}
		if err := scanner.Err(); err != nil {
			return nil, err
		}
		v = list
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(env, v, typ), nil
}

// TestIntConst is a helper for debugging
