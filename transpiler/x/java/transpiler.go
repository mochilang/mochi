//go:build slow

package javatr

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"mochi/parser"
	"mochi/types"
)

var varTypes map[string]string
var funcRet map[string]string
var extraDecls []Stmt
var structCount int
var topEnv *types.Env
var groupItems map[string]string

func javaType(t string) string {
	switch t {
	case "int":
		return "int"
	case "bool":
		return "boolean"
	case "boolean":
		return "boolean"
	case "string":
		return "String"
	case "void":
		return "void"
	case "int[]":
		return "int[]"
	case "string[]":
		return "String[]"
	case "bool[]":
		return "boolean[]"
	case "map":
		return "java.util.Map"
	case "fn":
		return "java.util.function.IntUnaryOperator"
	default:
		if t == "" {
			return ""
		}
		return t
	}
}

func javaBoxType(t string) string {
	switch t {
	case "int":
		return "Integer"
	case "bool", "boolean":
		return "Boolean"
	case "string", "String":
		return "String"
	default:
		return "Object"
	}
}

func fieldTypeFromVar(target Expr, name string) (string, bool) {
	v, ok := target.(*VarExpr)
	if !ok || topEnv == nil {
		return "", false
	}
	tname, ok := varTypes[v.Name]
	if !ok {
		return "", false
	}
	base := strings.TrimSuffix(tname, "[]")
	if st, ok := topEnv.GetStruct(base); ok {
		if ft, ok2 := st.Fields[name]; ok2 {
			return toJavaTypeFromType(ft), true
		}
	}
	return "", false
}

func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "int"
	case *BoolLit:
		return "boolean"
	case *StringLit:
		return "string"
	case *SubstringExpr:
		return "string"
	case *IndexExpr:
		if isStringExpr(ex.Target) {
			return "string"
		}
	case *SliceExpr:
		if isStringExpr(ex.Value) {
			return "string"
		}
	case *ListLit:
		if ex.ElemType != "" {
			return ex.ElemType + "[]"
		}
		if len(ex.Elems) > 0 {
			t := inferType(ex.Elems[0])
			if t != "" {
				if strings.HasSuffix(t, "[]") {
					return t
				}
				switch t {
				case "string":
					return "string[]"
				case "boolean":
					return "bool[]"
				default:
					return t + "[]"
				}
			}
		}
		return "int[]"
	case *StructLit:
		if ex.Name != "" {
			return ex.Name
		}
	case *FieldExpr:
		if t, ok := fieldTypeFromVar(ex.Target, ex.Name); ok {
			return t
		}
	case *MapLit:
		return "map"
	case *LambdaExpr:
		return "fn"
	case *UnaryExpr:
		if ex.Op == "!" {
			return "boolean"
		}
		return inferType(ex.Value)
	case *BinaryExpr:
		switch ex.Op {
		case "+":
			if isStringExpr(ex.Left) || isStringExpr(ex.Right) {
				return "String"
			}
			return "int"
		case "-", "*", "/", "%":
			return "int"
		case "==", "!=", "<", "<=", ">", ">=":
			if isStringExpr(ex.Left) || isStringExpr(ex.Right) {
				return "boolean"
			}
			return "int"
		case "&&", "||":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "int" || rt == "int" {
				return "int"
			}
			return "boolean"
		case "in":
			return "boolean"
		}
	case *TernaryExpr:
		t := inferType(ex.Then)
		if t == "" {
			t = inferType(ex.Else)
		}
		return t
	case *LenExpr:
		return "int"
	case *AvgExpr:
		return "Object"
	case *CallExpr:
		switch ex.Func {
		case "String.valueOf", "substring":
			return "String"
		case "Integer.parseInt":
			return "int"
		case "System.out.println":
			return "void"
		default:
			if t, ok := funcRet[ex.Func]; ok {
				return t
			}
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "contains":
			return "boolean"
		case "applyAsInt":
			return "int"
		}
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok {
			return t
		}
	}
	return ""
}

func inferReturnType(body []Stmt) string {
	if len(body) == 0 {
		return "void"
	}
	if ret, ok := body[len(body)-1].(*ReturnStmt); ok {
		if ret.Expr == nil {
			return "void"
		}
		t := inferType(ret.Expr)
		if t == "" {
			return "void"
		}
		return t
	}
	return "void"
}

// --- Simple Java AST ---

type Program struct {
	Funcs []*Function
	Stmts []Stmt
}

type Param struct {
	Name string
	Type string
}

// TypeDeclStmt declares a simple struct type.
type TypeDeclStmt struct {
	Name   string
	Fields []Param
}

// queryFrom represents a single 'from' clause in a query.
type queryFrom struct {
	Var string
	Src Expr
}

// QueryExpr represents a simplified query comprehension.
type queryJoin struct {
	Var  string
	Src  Expr
	On   Expr
	Side string
}

type queryGroup struct {
	Key       Expr
	Name      string
	Having    Expr
	ItemType  string
	GroupType string
	Fields    []string
}

type QueryExpr struct {
	Var      string
	Src      Expr
	Froms    []queryFrom
	Joins    []queryJoin
	Group    *queryGroup
	Where    Expr
	Select   Expr
	ElemType string
}

// StructLit represents a struct literal.
type StructLit struct {
	Name   string
	Fields []Expr
	Names  []string
}

type Function struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"return")
	if r.Expr != nil {
		fmt.Fprint(w, " ")
		r.Expr.emit(w)
	}
	fmt.Fprint(w, ";\n")
}

type Stmt interface{ emit(io.Writer, string) }

type Expr interface{ emit(io.Writer) }

func (t *TypeDeclStmt) emit(w io.Writer, indent string) {
	fmt.Fprintf(w, indent+"static class %s {\n", t.Name)
	for _, f := range t.Fields {
		typ := javaType(f.Type)
		if typ == "" {
			typ = f.Type
		}
		fmt.Fprintf(w, indent+"    %s %s;\n", typ, f.Name)
	}
	fmt.Fprintf(w, indent+"    %s(", t.Name)
	for i, f := range t.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := javaType(f.Type)
		if typ == "" {
			typ = f.Type
		}
		fmt.Fprintf(w, "%s %s", typ, f.Name)
	}
	fmt.Fprint(w, ") {\n")
	for _, f := range t.Fields {
		fmt.Fprintf(w, indent+"        this.%s = %s;\n", f.Name, f.Name)
	}
	fmt.Fprint(w, indent+"    }\n")
	fmt.Fprint(w, indent+"}\n")
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "new %s(", s.Name)
	for i, f := range s.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		f.emit(w)
	}
	fmt.Fprint(w, ")")
}

func (q *QueryExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "new java.util.ArrayList<%s>() {{", q.ElemType)
	if q.Group != nil {
		fmt.Fprintf(w, " java.util.LinkedHashMap<String,%s> _groups = new java.util.LinkedHashMap<>();", q.Group.GroupType)
	}
	fmt.Fprintf(w, " for (var %s : ", q.Var)
	if v, ok := q.Src.(*VarExpr); ok {
		if _, ok2 := groupItems[varTypes[v.Name]]; ok2 {
			fmt.Fprint(w, v.Name+".items")
		} else {
			q.Src.emit(w)
		}
	} else {
		q.Src.emit(w)
	}
	fmt.Fprint(w, ") {")
	for _, f := range q.Froms {
		fmt.Fprintf(w, " for (var %s : ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, ") {")
	}
	for _, j := range q.Joins {
		fmt.Fprintf(w, " for (var %s : ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, ") {")
		fmt.Fprint(w, " if (")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, ") {")
	}
	if q.Where != nil {
		fmt.Fprint(w, " if (")
		q.Where.emit(w)
		fmt.Fprint(w, ") {")
	}
	if q.Group != nil {
		fmt.Fprint(w, " var _k = ")
		q.Group.Key.emit(w)
		fmt.Fprint(w, "; String _ks = String.valueOf(_k);")
		fmt.Fprintf(w, " %s g = _groups.get(_ks);", q.Group.GroupType)
		fmt.Fprintf(w, " if (g == null) { g = new %s(_k, new java.util.ArrayList<>()); _groups.put(_ks, g); }", q.Group.GroupType)
		if len(q.Group.Fields) == 1 && !strings.HasPrefix(q.Group.ItemType, "Item") {
			fmt.Fprint(w, " g.items.add(")
			fmt.Fprint(w, q.Group.Fields[0])
			fmt.Fprint(w, ");")
		} else {
			fmt.Fprintf(w, " g.items.add(new %s(", q.Group.ItemType)
			for i, fld := range q.Group.Fields {
				if i > 0 {
					fmt.Fprint(w, ", ")
				}
				fmt.Fprint(w, fld)
			}
			fmt.Fprint(w, "));")
		}
	} else {
		fmt.Fprint(w, " add(")
		q.Select.emit(w)
		fmt.Fprint(w, ");")
	}
	if q.Where != nil {
		fmt.Fprint(w, " }")
	}
	for range q.Joins {
		fmt.Fprint(w, " }")
		fmt.Fprint(w, " }")
	}
	for range q.Froms {
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, " }")
	if q.Group != nil {
		fmt.Fprintf(w, " for (var %s : _groups.values()) {", q.Group.Name)
		if q.Group.Having != nil {
			fmt.Fprint(w, " if (")
			q.Group.Having.emit(w)
			fmt.Fprint(w, ") {")
		}
		fmt.Fprint(w, " add(")
		q.Select.emit(w)
		fmt.Fprint(w, ");")
		if q.Group.Having != nil {
			fmt.Fprint(w, " }")
		}
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, "}}")
}

// LambdaExpr represents a simple lambda expression with a single return value.
type LambdaExpr struct {
	Params []Param
	Body   []Stmt
	Return string
}

func (l *LambdaExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	for i, p := range l.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := javaType(p.Type)
		if typ == "" {
			typ = "java.util.Map"
		}
		fmt.Fprintf(w, "%s %s", typ, p.Name)
	}
	fmt.Fprint(w, ") -> ")
	if len(l.Body) == 1 {
		if rs, ok := l.Body[0].(*ReturnStmt); ok {
			if rs.Expr != nil {
				rs.Expr.emit(w)
				return
			}
		}
		if es, ok := l.Body[0].(*ExprStmt); ok {
			es.Expr.emit(w)
			return
		}
	}
	fmt.Fprint(w, "{\n")
	for _, st := range l.Body {
		st.emit(w, "    ")
	}
	fmt.Fprint(w, "}")
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"if (")
	s.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range s.Then {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}")
	if len(s.Else) > 0 {
		if len(s.Else) == 1 {
			if ei, ok := s.Else[0].(*IfStmt); ok {
				fmt.Fprint(w, " else ")
				ei.emit(w, indent)
				return
			}
		}
		fmt.Fprint(w, " else {\n")
		for _, st := range s.Else {
			st.emit(w, indent+"    ")
		}
		fmt.Fprint(w, indent+"}\n")
	} else {
		fmt.Fprint(w, "\n")
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	s.Expr.emit(w)
	fmt.Fprint(w, ";\n")
}

type LetStmt struct {
	Name string
	Type string
	Expr Expr
}

func (s *LetStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	if indent == "    " {
		fmt.Fprint(w, "static ")
	}
	typ := s.Type
	if typ == "" && s.Expr != nil {
		typ = inferType(s.Expr)
	}
	if typ == "" {
		typ = "java.util.Map"
	}
	fmt.Fprint(w, javaType(typ)+" "+s.Name)
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		s.Expr.emit(w)
	}
	fmt.Fprint(w, ";\n")
}

type VarStmt struct {
	Name string
	Type string
	Expr Expr
}

func (s *VarStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent)
	if indent == "    " {
		fmt.Fprint(w, "static ")
	}
	typ := s.Type
	if typ == "" && s.Expr != nil {
		typ = inferType(s.Expr)
	}
	if typ == "" {
		typ = "java.util.Map"
	}
	fmt.Fprint(w, javaType(typ)+" "+s.Name)
	if s.Expr != nil {
		fmt.Fprint(w, " = ")
		s.Expr.emit(w)
	}
	fmt.Fprint(w, ";\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (s *AssignStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+s.Name+" = ")
	s.Expr.emit(w)
	fmt.Fprint(w, ";\n")
}

// IndexAssignStmt represents assignments like a[0] = x or m["k"] = v.
type IndexAssignStmt struct {
	Target  Expr
	Indices []Expr
	Expr    Expr
}

func (s *IndexAssignStmt) emit(w io.Writer, indent string) {
	if len(s.Indices) == 1 && isMapExpr(s.Target) {
		s.Target.emit(w)
		fmt.Fprint(w, ".put(")
		s.Indices[0].emit(w)
		fmt.Fprint(w, ", ")
		s.Expr.emit(w)
		fmt.Fprint(w, ");\n")
		return
	}
	s.Target.emit(w)
	for _, idx := range s.Indices {
		fmt.Fprint(w, "[")
		idx.emit(w)
		fmt.Fprint(w, "]")
	}
	fmt.Fprint(w, " = ")
	s.Expr.emit(w)
	fmt.Fprint(w, ";\n")
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"while (")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"for (int "+fr.Name+" = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, fr.Name+" < ")
	fr.End.emit(w)
	fmt.Fprint(w, "; ")
	fmt.Fprint(w, fr.Name+"++")
	fmt.Fprint(w, ") {\n")
	for _, st := range fr.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

// ForEachStmt represents `for x in list {}` loops.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	IsMap    bool
}

func (fe *ForEachStmt) emit(w io.Writer, indent string) {
	fmt.Fprint(w, indent+"for (var "+fe.Name+" : ")
	fe.Iterable.emit(w)
	if fe.IsMap {
		fmt.Fprint(w, ".keySet()")
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range fe.Body {
		st.emit(w, indent+"    ")
	}
	fmt.Fprint(w, indent+"}\n")
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer, indent string) { fmt.Fprint(w, indent+"break;\n") }

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indent string) { fmt.Fprint(w, indent+"continue;\n") }

// ListLit represents a list literal.
type ListLit struct {
	ElemType string
	Elems    []Expr
}

func (l *ListLit) emit(w io.Writer) {
	arrType := l.ElemType
	if arrType == "" {
		arrType = "int"
		if len(l.Elems) > 0 {
			switch inferType(l.Elems[0]) {
			case "string":
				arrType = "String"
			case "boolean":
				arrType = "boolean"
			}
		}
		arrType = javaType(arrType)
	}
	fmt.Fprintf(w, "new %s[]{", arrType)
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

// MapLit represents a simple map literal.
type MapLit struct {
	Keys   []Expr
	Values []Expr
}

func (m *MapLit) emit(w io.Writer) {
	valType := "Object"
	if len(m.Values) > 0 {
		t := inferType(m.Values[0])
		same := true
		for _, v := range m.Values[1:] {
			if inferType(v) != t {
				same = false
				break
			}
		}
		if same {
			valType = javaBoxType(t)
		}
	}
	fmt.Fprintf(w, "new java.util.LinkedHashMap<String, %s>() {{", valType)
	for i := range m.Keys {
		fmt.Fprint(w, " put(")
		m.Keys[i].emit(w)
		fmt.Fprint(w, ", ")
		m.Values[i].emit(w)
		fmt.Fprint(w, ");")
	}
	fmt.Fprint(w, " }}")
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if isStringExpr(b.Left) && isStringExpr(b.Right) {
		switch b.Op {
		case "==", "!=":
			if b.Op == "!=" {
				fmt.Fprint(w, "!")
			}
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprint(w, ".equals(")
			b.Right.emit(w)
			fmt.Fprint(w, "))")
			return
		case "<", "<=", ">", ">=":
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprint(w, ".compareTo(")
			b.Right.emit(w)
			fmt.Fprint(w, ") ")
			switch b.Op {
			case "<":
				fmt.Fprint(w, "< 0")
			case "<=":
				fmt.Fprint(w, "<= 0")
			case ">":
				fmt.Fprint(w, "> 0")
			case ">=":
				fmt.Fprint(w, ">= 0")
			}
			fmt.Fprint(w, ")")
			return
		}
	}
	b.Left.emit(w)
	fmt.Fprint(w, " "+b.Op+" ")
	b.Right.emit(w)
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprint(w, i.Value) }

type VarExpr struct{ Name string }

func (v *VarExpr) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	if isMapExpr(f.Target) {
		fmt.Fprint(w, "((Integer) (")
		f.Target.emit(w)
		fmt.Fprint(w, ".get(")
		(&StringLit{Value: f.Name}).emit(w)
		fmt.Fprint(w, ")))")
		return
	}
	f.Target.emit(w)
	fmt.Fprint(w, "."+f.Name)
}

type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Value.emit(w)
	switch {
	case isGroupExpr(l.Value):
		fmt.Fprint(w, ".items.size()")
	case isStringExpr(l.Value):
		fmt.Fprint(w, ".length()")
	case isMapExpr(l.Value):
		fmt.Fprint(w, ".size()")
	default:
		fmt.Fprint(w, ".length")
	}
}

// AvgExpr represents averaging a list of numbers.
type AvgExpr struct{ Value Expr }

func (a *AvgExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(((")
	a.Value.emit(w)
	fmt.Fprint(w, ".stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) % 1 == 0) ? (Object)(int)(")
	a.Value.emit(w)
	fmt.Fprint(w, ".stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)) : (Object)(")
	a.Value.emit(w)
	fmt.Fprint(w, ".stream().mapToDouble(v -> ((Number)v).doubleValue()).average().orElse(0)))")
}

type UnaryExpr struct {
	Op    string
	Value Expr
}

type GroupExpr struct{ Expr Expr }

func (g *GroupExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	g.Expr.emit(w)
	fmt.Fprint(w, ")")
}

type TernaryExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (t *TernaryExpr) emit(w io.Writer) {
	t.Cond.emit(w)
	fmt.Fprint(w, " ? ")
	t.Then.emit(w)
	fmt.Fprint(w, " : ")
	t.Else.emit(w)
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprint(w, b.Value) }

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	if _, ok := u.Value.(*BinaryExpr); ok {
		fmt.Fprint(w, "(")
		u.Value.emit(w)
		fmt.Fprint(w, ")")
	} else {
		u.Value.emit(w)
	}
}

type CallExpr struct {
	Func string
	Args []Expr
}

// MethodCallExpr represents target.method(args...)
type MethodCallExpr struct {
	Target Expr
	Name   string
	Args   []Expr
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	fmt.Fprint(w, "."+m.Name+"(")
	for i, a := range m.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
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

type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Str.emit(w)
	fmt.Fprint(w, ".substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// IndexExpr represents s[i]. For strings it emits charAt.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (ix *IndexExpr) emit(w io.Writer) {
	if isStringExpr(ix.Target) {
		ix.Target.emit(w)
		fmt.Fprint(w, ".charAt(")
		ix.Index.emit(w)
		fmt.Fprint(w, ")")
	} else if isMapExpr(ix.Target) {
		ix.Target.emit(w)
		fmt.Fprint(w, ".get(")
		ix.Index.emit(w)
		fmt.Fprint(w, ")")
	} else {
		ix.Target.emit(w)
		fmt.Fprint(w, "[")
		ix.Index.emit(w)
		fmt.Fprint(w, "]")
	}
}

// SliceExpr represents s[a:b]. Only strings are currently supported.
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (sli *SliceExpr) emit(w io.Writer) {
	switch {
	case isStringExpr(sli.Value):
		sli.Value.emit(w)
		fmt.Fprint(w, ".substring(")
		sli.Start.emit(w)
		fmt.Fprint(w, ", ")
		sli.End.emit(w)
		fmt.Fprint(w, ")")
	case isArrayExpr(sli.Value):
		fmt.Fprint(w, "java.util.Arrays.copyOfRange(")
		sli.Value.emit(w)
		fmt.Fprint(w, ", ")
		sli.Start.emit(w)
		fmt.Fprint(w, ", ")
		sli.End.emit(w)
		fmt.Fprint(w, ")")
	default:
		sli.Value.emit(w)
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok && (t == "string" || t == "String") {
			return true
		}
	case *CallExpr:
		if ex.Func == "String.valueOf" {
			return true
		}
	case *SubstringExpr:
		return true
	case *IndexExpr:
		if isStringExpr(ex.Target) {
			return true
		}
	case *SliceExpr:
		if isStringExpr(ex.Value) {
			return true
		}
	}
	return false
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok && t == "map" {
			return true
		}
	}
	return false
}

func isArrayExpr(e Expr) bool {
	switch ex := e.(type) {
	case *ListLit:
		return true
	case *SliceExpr:
		if !isStringExpr(ex.Value) {
			return true
		}
	case *VarExpr:
		if t, ok := varTypes[ex.Name]; ok && strings.HasSuffix(t, "[]") {
			return true
		}
	}
	return false
}

func isGroupExpr(e Expr) bool {
	if v, ok := e.(*VarExpr); ok {
		if _, ok2 := groupItems[varTypes[v.Name]]; ok2 {
			return true
		}
	}
	return false
}

func isNumericBool(e Expr) bool {
	switch ex := e.(type) {
	case *BinaryExpr:
		switch ex.Op {
		case "&&", "||", "==", "!=", "<", "<=", ">", ">=":
			return true
		}
	case *UnaryExpr:
		if ex.Op == "!" {
			return isNumericBool(ex.Value)
		}
	}
	return false
}

// Transpile converts a Mochi AST into a simple Java AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	var prog Program
	varTypes = map[string]string{}
	funcRet = map[string]string{}
	extraDecls = nil
	structCount = 0
	topEnv = env
	groupItems = map[string]string{}
	for _, s := range p.Statements {
		if s.Fun != nil {
			body, err := compileStmts(s.Fun.Body)
			if err != nil {
				return nil, err
			}
			var params []Param
			for _, p := range s.Fun.Params {
				params = append(params, Param{Name: p.Name, Type: typeRefString(p.Type)})
			}
			ret := typeRefString(s.Fun.Return)
			if ret == "" {
				ret = inferReturnType(body)
			}
			for _, p := range params {
				if javaType(p.Type) == "" {
					stmt := &AssignStmt{Name: p.Name, Expr: &CallExpr{Func: "new java.util.LinkedHashMap", Args: []Expr{&VarExpr{Name: p.Name}}}}
					body = append([]Stmt{stmt}, body...)
				}
			}
			funcRet[s.Fun.Name] = ret
			prog.Funcs = append(prog.Funcs, &Function{Name: s.Fun.Name, Params: params, Return: ret, Body: body})
			continue
		}
		st, err := compileStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			prog.Stmts = append(prog.Stmts, st)
		}
		if len(extraDecls) > 0 {
			prog.Stmts = append(prog.Stmts, extraDecls...)
			extraDecls = nil
		}
	}
	_ = env // reserved
	return &prog, nil
}

func compileStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		e, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Let != nil:
		if s.Let.Value != nil {
			e, err := compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
			t := typeRefString(s.Let.Type)
			if t == "" {
				switch ex := e.(type) {
				case *QueryExpr:
					t = fmt.Sprintf("java.util.List<%s>", ex.ElemType)
				case *ListLit:
					if ex.ElemType != "" {
						t = ex.ElemType + "[]"
					}
				}
			}
			if t == "" && topEnv != nil {
				t = toJavaTypeFromType(types.ExprType(s.Let.Value, topEnv))
			}
			if t == "" {
				t = inferType(e)
			}
			if t != "" {
				varTypes[s.Let.Name] = t
			}
			return &LetStmt{Name: s.Let.Name, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Let.Type)
		if t == "" && topEnv != nil {
			if v, err := topEnv.GetVar(s.Let.Name); err == nil {
				t = toJavaTypeFromType(v)
			}
		}
		if t != "" {
			varTypes[s.Let.Name] = t
		}
		return &LetStmt{Name: s.Let.Name, Type: t}, nil
	case s.Var != nil:
		if s.Var.Value != nil {
			e, err := compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
			t := typeRefString(s.Var.Type)
			if t == "" {
				switch ex := e.(type) {
				case *QueryExpr:
					t = fmt.Sprintf("java.util.List<%s>", ex.ElemType)
				case *ListLit:
					if ex.ElemType != "" {
						t = ex.ElemType + "[]"
					}
				}
			}
			if t == "" && topEnv != nil {
				t = toJavaTypeFromType(types.ExprType(s.Var.Value, topEnv))
			}
			if t == "" {
				t = inferType(e)
			}
			if t != "" {
				varTypes[s.Var.Name] = t
			}
			return &VarStmt{Name: s.Var.Name, Type: t, Expr: e}, nil
		}
		t := typeRefString(s.Var.Type)
		if t == "" && topEnv != nil {
			if v, err := topEnv.GetVar(s.Var.Name); err == nil {
				t = toJavaTypeFromType(v)
			}
		}
		if t != "" {
			varTypes[s.Var.Name] = t
		}
		return &VarStmt{Name: s.Var.Name, Type: t}, nil
	case s.Fun != nil:
		expr, err := compileFunExpr(&parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body})
		if err != nil {
			return nil, err
		}
		varTypes[s.Fun.Name] = "fn"
		return &VarStmt{Name: s.Fun.Name, Type: "fn", Expr: expr}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			e, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if _, ok := varTypes[s.Assign.Name]; !ok {
				if t := inferType(e); t != "" {
					varTypes[s.Assign.Name] = t
				}
			}
			return &AssignStmt{Name: s.Assign.Name, Expr: e}, nil
		}
		if len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0 {
			indices := make([]Expr, len(s.Assign.Index))
			for i, idx := range s.Assign.Index {
				if idx.Start == nil || idx.Colon != nil {
					return nil, fmt.Errorf("unsupported index")
				}
				ex, err := compileExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				indices[i] = ex
			}
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			base := Expr(&VarExpr{Name: s.Assign.Name})
			return &IndexAssignStmt{Target: base, Indices: indices, Expr: val}, nil
		}
		if len(s.Assign.Field) > 0 && len(s.Assign.Index) == 0 {
			base := Expr(&VarExpr{Name: s.Assign.Name})
			for i := 0; i < len(s.Assign.Field)-1; i++ {
				base = &IndexExpr{Target: base, Index: &StringLit{Value: s.Assign.Field[i].Name}}
			}
			key := &StringLit{Value: s.Assign.Field[len(s.Assign.Field)-1].Name}
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Target: base, Indices: []Expr{key}, Expr: val}, nil
		}
	case s.If != nil:
		cond, err := compileExpr(s.If.Cond)
		if err != nil {
			return nil, err
		}
		var thenStmts []Stmt
		for _, b := range s.If.Then {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				thenStmts = append(thenStmts, st)
			}
		}
		var elseStmts []Stmt
		if s.If.ElseIf != nil {
			st, err := compileStmt(&parser.Statement{If: s.If.ElseIf})
			if err != nil {
				return nil, err
			}
			if st != nil {
				elseStmts = append(elseStmts, st)
			}
		} else {
			for _, b := range s.If.Else {
				st, err := compileStmt(b)
				if err != nil {
					return nil, err
				}
				if st != nil {
					elseStmts = append(elseStmts, st)
				}
			}
		}
		return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
	case s.Return != nil:
		var e Expr
		var err error
		if s.Return.Value != nil {
			e, err = compileExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Expr: e}, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.While.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.For != nil && s.For.RangeEnd != nil:
		start, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		end, err := compileExpr(s.For.RangeEnd)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &ForRangeStmt{Name: s.For.Name, Start: start, End: end, Body: body}, nil
	case s.For != nil:
		iter, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		isMap := false
		switch it := iter.(type) {
		case *MapLit:
			isMap = true
		case *VarExpr:
			if t, ok := varTypes[it.Name]; ok && t == "map" {
				isMap = true
			}
		}
		return &ForEachStmt{Name: s.For.Name, Iterable: iter, Body: body, IsMap: isMap}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Test == nil && s.Import == nil && s.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil, nil
}

func compileStmts(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := compileStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	expr := left
	for _, op := range e.Binary.Right {
		r, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			expr = &BinaryExpr{Left: expr, Op: op.Op, Right: r}
		case "in":
			if isStringExpr(r) {
				expr = &MethodCallExpr{Target: r, Name: "contains", Args: []Expr{expr}}
			} else if isArrayExpr(r) {
				arr := &CallExpr{Func: "java.util.Arrays.asList", Args: []Expr{r}}
				expr = &MethodCallExpr{Target: arr, Name: "contains", Args: []Expr{expr}}
			} else if isMapExpr(r) {
				expr = &MethodCallExpr{Target: r, Name: "containsKey", Args: []Expr{expr}}
			} else {
				return nil, fmt.Errorf("unsupported binary op: %s", op.Op)
			}
		default:
			return nil, fmt.Errorf("unsupported binary op: %s", op.Op)
		}
	}
	return expr, nil
}

func compileUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := compilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-", "!":
			expr = &UnaryExpr{Op: u.Ops[i], Value: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op: %s", u.Ops[i])
		}
	}
	return expr, nil
}

func compilePostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := compilePrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			if op.Index.Start == nil {
				return nil, fmt.Errorf("unsupported index")
			}
			idx, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			if op.Index.Start == nil || op.Index.End == nil {
				return nil, fmt.Errorf("unsupported slice")
			}
			start, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			end, err := compileExpr(op.Index.End)
			if err != nil {
				return nil, err
			}
			expr = &SliceExpr{Value: expr, Start: start, End: end}
		case op.Field != nil:
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for j, a := range op.Call.Args {
				ex, err := compileExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			if fe, ok := expr.(*FieldExpr); ok {
				expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
			} else if v, ok := expr.(*VarExpr); ok {
				if t, ok := varTypes[v.Name]; ok && t == "fn" {
					expr = &MethodCallExpr{Target: expr, Name: "applyAsInt", Args: args}
				} else {
					expr = &CallExpr{Func: v.Name, Args: args}
				}
			} else if _, ok := expr.(*LambdaExpr); ok {
				expr = &MethodCallExpr{Target: expr, Name: "applyAsInt", Args: args}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			switch *op.Cast.Type.Simple {
			case "int":
				expr = &CallExpr{Func: "Integer.parseInt", Args: []Expr{expr}}
			default:
				// ignore other casts
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func compilePrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		if t, ok := varTypes[name]; ok && t == "fn" {
			return &MethodCallExpr{Target: &VarExpr{Name: name}, Name: "applyAsInt", Args: args}, nil
		}
		if name == "print" {
			name = "System.out.println"
			for i, a := range args {
				if isNumericBool(a) {
					args[i] = &TernaryExpr{Cond: a, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
				} else if isArrayExpr(a) {
					args[i] = &CallExpr{Func: "java.util.Arrays.toString", Args: []Expr{a}}
				}
			}
			if len(args) > 1 {
				expr := args[0]
				for i := 1; i < len(args); i++ {
					expr = &BinaryExpr{Left: &BinaryExpr{Left: expr, Op: "+", Right: &StringLit{Value: " "}}, Op: "+", Right: args[i]}
				}
				args = []Expr{expr}
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
		if name == "len" && len(args) == 1 {
			return &LenExpr{Value: args[0]}, nil
		}
		if name == "count" && len(args) == 1 {
			return &LenExpr{Value: args[0]}, nil
		}
		if name == "avg" && len(args) == 1 {
			return &AvgExpr{Value: args[0]}, nil
		}
		if name == "str" && len(args) == 1 {
			return &CallExpr{Func: "String.valueOf", Args: args}, nil
		}
		if name == "substring" && len(args) == 3 {
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Selector != nil:
		expr := Expr(&VarExpr{Name: p.Selector.Root})
		for _, name := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: name}
		}
		return expr, nil
	case p.Group != nil:
		e, err := compileExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: e}, nil
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.List != nil:
		if st, ok := inferStructFromList(p.List); ok {
			structCount++
			name := fmt.Sprintf("Data%d", structCount)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fields := make([]Param, len(st.Order))
			elems := make([]Expr, len(p.List.Elems))
			for i, fn := range st.Order {
				fields[i] = Param{Name: fn, Type: toJavaTypeFromType(st.Fields[fn])}
			}
			for i, e := range p.List.Elems {
				ml := e.Binary.Left.Value.Target.Map
				vals := make([]Expr, len(st.Order))
				for j, it := range ml.Items {
					v, err := compileExpr(it.Value)
					if err != nil {
						return nil, err
					}
					vals[j] = v
				}
				elems[i] = &StructLit{Name: name, Fields: vals, Names: st.Order}
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fields})
			return &ListLit{ElemType: name, Elems: elems}, nil
		}
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := compileExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		ml := p.Map
		if st, ok := types.InferStructFromMapEnv(ml, topEnv); ok {
			structCount++
			name := fmt.Sprintf("Data%d", structCount)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fields := make([]Param, len(st.Order))
			vals := make([]Expr, len(st.Order))
			for i, it := range ml.Items {
				v, err := compileExpr(it.Value)
				if err != nil {
					return nil, err
				}
				vals[i] = v
				fields[i] = Param{Name: st.Order[i], Type: toJavaTypeFromType(st.Fields[st.Order[i]])}
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fields})
			return &StructLit{Name: name, Fields: vals, Names: st.Order}, nil
		}
		keys := make([]Expr, len(ml.Items))
		vals := make([]Expr, len(ml.Items))
		for i, it := range ml.Items {
			ke, err := compileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			ve, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			keys[i] = ke
			vals[i] = ve
		}
		return &MapLit{Keys: keys, Values: vals}, nil
	case p.Struct != nil:
		names := make([]string, len(p.Struct.Fields))
		vals := make([]Expr, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			names[i] = f.Name
			v, err := compileExpr(f.Value)
			if err != nil {
				return nil, err
			}
			vals[i] = v
		}
		return &StructLit{Name: p.Struct.Name, Fields: vals, Names: names}, nil
	case p.FunExpr != nil:
		return compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return compileMatchExpr(p.Match)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = compileIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = compileExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &BoolLit{Value: false}
	}
	return &TernaryExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func compileFunExpr(fn *parser.FunExpr) (Expr, error) {
	params := make([]Param, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = Param{Name: p.Name, Type: typeRefString(p.Type)}
	}
	var body []Stmt
	if fn.ExprBody != nil {
		ex, err := compileExpr(fn.ExprBody)
		if err != nil {
			return nil, err
		}
		body = []Stmt{&ReturnStmt{Expr: ex}}
	} else {
		var err error
		body, err = compileStmts(fn.BlockBody)
		if err != nil {
			return nil, err
		}
	}
	for _, p := range params {
		if javaType(p.Type) == "" {
			stmt := &AssignStmt{Name: p.Name, Expr: &CallExpr{Func: "new java.util.LinkedHashMap", Args: []Expr{&VarExpr{Name: p.Name}}}}
			body = append([]Stmt{stmt}, body...)
		}
	}
	ret := typeRefString(fn.Return)
	if ret == "" {
		ret = inferReturnType(body)
	}
	return &LambdaExpr{Params: params, Body: body, Return: ret}, nil
}

func compileMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := compileExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := compileExpr(c.Result)
		if err != nil {
			return nil, err
		}
		pat, err := compileExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if v, ok := pat.(*VarExpr); ok && v.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		if expr == nil {
			expr = res
		}
		expr = &TernaryExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func compileQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, fmt.Errorf("unsupported query features")
	}
	src, err := compileExpr(q.Source)
	if err != nil {
		return nil, err
	}
	var elemType string
	if v, ok := src.(*VarExpr); ok {
		if it, ok2 := groupItems[varTypes[v.Name]]; ok2 {
			src = &FieldExpr{Target: src, Name: "items"}
			elemType = it
		}
	}
	if topEnv != nil {
		if lt, ok := types.ExprType(q.Source, topEnv).(types.ListType); ok {
			elemType = toJavaTypeFromType(lt.Elem)
		}
	}
	if elemType == "" {
		switch s := src.(type) {
		case *VarExpr:
			if vt, ok := varTypes[s.Name]; ok {
				elemType = strings.TrimSuffix(vt, "[]")
			}
		case *ListLit:
			elemType = s.ElemType
		}
	}
	if elemType == "" {
		elemType = "java.util.Map"
	}
	varTypes[q.Var] = elemType
	if topEnv != nil {
		if st, ok := topEnv.GetStruct(elemType); ok {
			topEnv.SetVar(q.Var, st, false)
		}
	}
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := compileExpr(f.Src)
		if err != nil {
			return nil, err
		}
		varTypes[f.Var] = "java.util.Map"
		if topEnv != nil {
			if lt, ok := types.ExprType(f.Src, topEnv).(types.ListType); ok {
				varTypes[f.Var] = toJavaTypeFromType(lt.Elem)
				if st, ok := topEnv.GetStruct(varTypes[f.Var]); ok {
					topEnv.SetVar(f.Var, st, false)
				}
			}
		}
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := compileExpr(j.Src)
		if err != nil {
			return nil, err
		}
		jt := "java.util.Map"
		if topEnv != nil {
			if lt, ok := types.ExprType(j.Src, topEnv).(types.ListType); ok {
				jt = toJavaTypeFromType(lt.Elem)
				if st, ok := topEnv.GetStruct(jt); ok {
					topEnv.SetVar(j.Var, st, false)
				}
			}
		}
		varTypes[j.Var] = jt
		onExpr, err := compileExpr(j.On)
		if err != nil {
			return nil, err
		}
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: onExpr, Side: side}
	}
	var where Expr
	if q.Where != nil {
		where, err = compileExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}

	var group *queryGroup
	if q.Group != nil && len(q.Group.Exprs) > 0 {
		keyExpr, err := compileExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		keyType := "java.util.Map"
		if topEnv != nil {
			keyType = toJavaTypeFromType(types.ExprType(q.Group.Exprs[0], topEnv))
		}
		if keyType == "" {
			keyType = inferType(keyExpr)
		}
		if keyType == "" {
			keyType = "Object"
		}
		itemFields := append([]string{q.Var}, make([]string, 0, len(q.Froms)+len(q.Joins))...)
		for _, f := range q.Froms {
			itemFields = append(itemFields, f.Var)
		}
		for _, j := range q.Joins {
			itemFields = append(itemFields, j.Var)
		}
		var itemName string
		if len(itemFields) == 1 {
			itemName = varTypes[itemFields[0]]
		} else {
			itemName = fmt.Sprintf("Item%d", structCount+1)
			itemDecl := make([]Param, len(itemFields))
			for i, n := range itemFields {
				itemDecl[i] = Param{Name: n, Type: varTypes[n]}
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: itemName, Fields: itemDecl})
		}

		groupName := fmt.Sprintf("Group%d", structCount+1)
		structCount++
		gfields := []Param{
			{Name: "key", Type: keyType},
			{Name: "items", Type: fmt.Sprintf("java.util.List<%s>", itemName)},
		}
		extraDecls = append(extraDecls, &TypeDeclStmt{Name: groupName, Fields: gfields})
		groupItems[groupName] = itemName
		var having Expr
		if q.Group.Having != nil {
			having, err = compileExpr(q.Group.Having)
			if err != nil {
				return nil, err
			}
		}
		varTypes[q.Group.Name] = groupName
		group = &queryGroup{Key: keyExpr, Name: q.Group.Name, Having: having, ItemType: itemName, GroupType: groupName, Fields: itemFields}
	}

	idx := len(extraDecls)
	sel, err := compileExpr(q.Select)
	if err != nil {
		return nil, err
	}
	if ml := mapLiteral(q.Select); ml != nil {
		extraDecls = extraDecls[:idx]
		if st, ok := types.InferStructFromMapEnv(ml, topEnv); ok {
			structCount++
			name := fmt.Sprintf("Result%d", structCount)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fieldsDecl := make([]Param, len(st.Order))
			vals := make([]Expr, len(st.Order))
			for i, it := range ml.Items {
				v, err := compileExpr(it.Value)
				if err != nil {
					return nil, err
				}
				tname := ""
				if fe, ok := v.(*FieldExpr); ok {
					if ft, ok2 := fieldTypeFromVar(fe.Target, fe.Name); ok2 {
						tname = ft
					}
				}
				if tname == "" {
					tname = inferType(v)
				}
				if tname == "" {
					tname = "Object"
				}
				fieldsDecl[i] = Param{Name: st.Order[i], Type: tname}
				vals[i] = v
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
			sel = &StructLit{Name: name, Fields: vals, Names: st.Order}
			elemType = name
		}
	}
	tsel := javaBoxType(inferType(sel))
	if tsel != "" && tsel != "Object" {
		elemType = tsel
	} else if elemType == "" {
		elemType = "Object"
	}

	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Group: group, Where: where, Select: sel, ElemType: elemType}, nil
}

// Emit generates formatted Java source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("public class Main {\n")
	// emit type declarations and global variables first
	for _, st := range prog.Stmts {
		switch st.(type) {
		case *TypeDeclStmt:
			st.emit(&buf, "    ")
			buf.WriteByte('\n')
		case *LetStmt, *VarStmt:
			st.emit(&buf, "    ")
		}
	}
	if len(prog.Stmts) > 0 {
		buf.WriteByte('\n')
	}
	for i, fn := range prog.Funcs {
		ret := javaType(fn.Return)
		if ret == "" {
			ret = "void"
		}
		buf.WriteString("    static " + ret + " " + fn.Name + "(")
		for i, p := range fn.Params {
			if i > 0 {
				buf.WriteString(", ")
			}
			typ := javaType(p.Type)
			if typ == "" {
				typ = "java.util.Map"
			}
			buf.WriteString(typ + " " + p.Name)
		}
		buf.WriteString(") {\n")
		for _, s := range fn.Body {
			s.emit(&buf, "        ")
		}
		buf.WriteString("    }")
		buf.WriteByte('\n')
		if i < len(prog.Funcs)-1 {
			buf.WriteByte('\n')
		}
	}
	buf.WriteString("    public static void main(String[] args) {\n")
	for _, st := range prog.Stmts {
		switch st.(type) {
		case *LetStmt, *VarStmt, *TypeDeclStmt:
			// already emitted as globals or declarations
		default:
			st.emit(&buf, "        ")
		}
	}
	buf.WriteString("    }\n")
	buf.WriteString("}\n")
	return formatJava(buf.Bytes())
}

func formatJava(src []byte) []byte {
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

func typeRefString(tr *parser.TypeRef) string {
	if tr == nil {
		return ""
	}
	if tr.Simple != nil {
		return *tr.Simple
	}
	if tr.Generic != nil {
		return tr.Generic.Name
	}
	if tr.Fun != nil {
		return "fn"
	}
	return ""
}

func toJavaTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.BoolType:
		return "boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		et := toJavaTypeFromType(tt.Elem)
		if et == "" {
			et = "Object"
		}
		return et + "[]"
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
	}
	return ""
}

func mapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil
	}
	v := u.Value
	if len(v.Ops) != 0 || v.Target == nil {
		return nil
	}
	return v.Target.Map
}

func inferStructFromList(ll *parser.ListLiteral) (st types.StructType, ok bool) {
	if ll == nil || len(ll.Elems) == 0 {
		return types.StructType{}, false
	}
	first := ll.Elems[0]
	if first.Binary == nil || len(first.Binary.Right) != 0 {
		return types.StructType{}, false
	}
	fm := first.Binary.Left.Value.Target.Map
	if fm == nil {
		return types.StructType{}, false
	}
	fields := map[string]types.Type{}
	order := make([]string, len(fm.Items))
	for i, it := range fm.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		order[i] = key
		fields[key] = types.ExprType(it.Value, topEnv)
	}
	for _, el := range ll.Elems[1:] {
		if el.Binary == nil || len(el.Binary.Right) != 0 {
			return types.StructType{}, false
		}
		ml := el.Binary.Left.Value.Target.Map
		if ml == nil || len(ml.Items) != len(order) {
			return types.StructType{}, false
		}
		for i, it := range ml.Items {
			key, ok := types.SimpleStringKey(it.Key)
			if !ok || key != order[i] {
				return types.StructType{}, false
			}
			t := types.ExprType(it.Value, topEnv)
			if !types.EqualTypes(fields[key], t) {
				return types.StructType{}, false
			}
		}
	}
	return types.StructType{Fields: fields, Order: order}, true
}
