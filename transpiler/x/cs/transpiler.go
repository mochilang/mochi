//go:build slow

package cstranspiler

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// --- C# AST ---

type Program struct {
	Globals []*Global
	Funcs   []*Function
	Stmts   []Stmt
}

type Global struct {
	Name  string
	Value Expr
}

var stringVars map[string]bool
var mapVars map[string]bool
var varTypes map[string]string
var usesDict bool
var usesLinq bool
var usesJson bool

type Stmt interface{ emit(io.Writer) }

func isBlockStmt(s Stmt) bool {
	switch s.(type) {
	case *ForRangeStmt, *ForInStmt, *WhileStmt, *IfStmt:
		return true
	}
	return false
}

// LetStmt represents a variable declaration.
type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) {
	if t, ok := varTypes[s.Name]; ok && t != "" {
		fmt.Fprintf(w, "%s %s = ", t, s.Name)
	} else {
		fmt.Fprintf(w, "var %s = ", s.Name)
	}
	s.Value.emit(w)
}

// VarStmt represents a mutable variable declaration.
type VarStmt struct {
	Name  string
	Value Expr // optional
}

func (s *VarStmt) emit(w io.Writer) {
	if t, ok := varTypes[s.Name]; ok && t != "" {
		fmt.Fprintf(w, "%s %s", t, s.Name)
	} else {
		fmt.Fprintf(w, "var %s", s.Name)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

// AssignStmt represents simple assignment to a variable.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", s.Name)
	s.Value.emit(w)
}

// AssignIndexStmt represents assignment to an indexed element like xs[i] = v.
type AssignIndexStmt struct {
	Target Expr
	Index  Expr
	Value  Expr
}

func (s *AssignIndexStmt) emit(w io.Writer) {
	s.Target.emit(w)
	fmt.Fprint(w, "[")
	s.Index.emit(w)
	fmt.Fprint(w, "] = ")
	s.Value.emit(w)
}

// ReturnStmt represents a return statement within a function.
type ReturnStmt struct {
	Value Expr // optional
}

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Value != nil {
		fmt.Fprint(w, " ")
		r.Value.emit(w)
	}
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break") }

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue") }

// ForRangeStmt represents a numeric range for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (var %s = ", f.Var)
	if f.Start != nil {
		f.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; ")
	fmt.Fprintf(w, "%s < ", f.Var)
	if f.End != nil {
		f.End.emit(w)
	}
	fmt.Fprint(w, "; ")
	fmt.Fprintf(w, "%s++) {\n", f.Var)
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		if isBlockStmt(st) {
			fmt.Fprint(w, "\n")
		} else {
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprint(w, "}")
}

// ForInStmt represents iteration over elements of an iterable.
type ForInStmt struct {
	Var      string
	Iterable Expr
	Body     []Stmt
}

func (f *ForInStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "foreach (var %s in ", f.Var)
	if isMapExpr(f.Iterable) {
		f.Iterable.emit(w)
		fmt.Fprint(w, ".Keys")
	} else {
		f.Iterable.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		if isBlockStmt(st) {
			fmt.Fprint(w, "\n")
		} else {
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprint(w, "}")
}

// Function represents a simple function declaration.
type Function struct {
	Name       string
	Params     []string
	ParamTypes []string
	ReturnType string
	Body       []Stmt
}

func (f *Function) emit(w io.Writer) {
	ret := f.ReturnType
	if ret == "" {
		ret = "void"
	}
	fmt.Fprintf(w, "static %s %s(", ret, f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := "int"
		if len(f.ParamTypes) > i && f.ParamTypes[i] != "" {
			typ = f.ParamTypes[i]
		}
		fmt.Fprintf(w, "%s %s", typ, p)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}\n")
}

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while (")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		if isBlockStmt(st) {
			fmt.Fprint(w, "\n")
		} else {
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprint(w, "}")
}

// IfStmt represents a conditional statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range i.Then {
		fmt.Fprint(w, "    ")
		st.emit(w)
		if isBlockStmt(st) {
			fmt.Fprint(w, "\n")
		} else {
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprint(w, "}")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, st := range i.Else {
			fmt.Fprint(w, "    ")
			st.emit(w)
			if isBlockStmt(st) {
				fmt.Fprint(w, "\n")
			} else {
				fmt.Fprint(w, ";\n")
			}
		}
		fmt.Fprint(w, "}")
	}
}

// IfExpr is a ternary conditional expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (ie *IfExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	ie.Cond.emit(w)
	fmt.Fprint(w, " ? ")
	ie.Then.emit(w)
	fmt.Fprint(w, " : ")
	ie.Else.emit(w)
	fmt.Fprint(w, ")")
}

type Expr interface{ emit(io.Writer) }

// VarRef references a variable by name.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

// BinaryExpr represents a binary operation like addition or comparison.
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	switch b.Op {
	case "union":
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprint(w, ".Concat(")
		b.Right.emit(w)
		fmt.Fprint(w, ").Distinct().ToArray())")
	case "union_all":
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprint(w, ".Concat(")
		b.Right.emit(w)
		fmt.Fprint(w, ").ToArray())")
	case "except":
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprint(w, ".Except(")
		b.Right.emit(w)
		fmt.Fprint(w, ").ToArray())")
	case "intersect":
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprint(w, ".Intersect(")
		b.Right.emit(w)
		fmt.Fprint(w, ").ToArray())")
	default:
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprintf(w, " %s ", b.Op)
		b.Right.emit(w)
		fmt.Fprint(w, ")")
	}
}

// BoolOpExpr represents boolean && and || operations with integer semantics.
type BoolOpExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b *BoolOpExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

// UnaryExpr represents a unary prefix operation.
type UnaryExpr struct {
	Op  string
	Val Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	u.Val.emit(w)
}

type NotExpr struct{ Val Expr }

func (n *NotExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(!")
	n.Val.emit(w)
	fmt.Fprint(w, ")")
}

// CmpExpr emits comparison result as 1 or 0.
type CmpExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (c *CmpExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	if (c.Op == "<" || c.Op == "<=" || c.Op == ">" || c.Op == ">=") &&
		(isStringExpr(c.Left) || isStringExpr(c.Right)) {
		fmt.Fprint(w, "string.Compare(")
		c.Left.emit(w)
		fmt.Fprint(w, ", ")
		c.Right.emit(w)
		fmt.Fprint(w, ")")
		switch c.Op {
		case "<":
			fmt.Fprint(w, " < 0")
		case "<=":
			fmt.Fprint(w, " <= 0")
		case ">":
			fmt.Fprint(w, " > 0")
		case ">=":
			fmt.Fprint(w, " >= 0")
		}
		fmt.Fprint(w, ")")
		return
	}
	c.Left.emit(w)
	fmt.Fprintf(w, " %s ", c.Op)
	c.Right.emit(w)
	fmt.Fprint(w, ")")
}

type InExpr struct {
	Item       Expr
	Collection Expr
}

func (ie *InExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	if isStringExpr(ie.Collection) {
		ie.Collection.emit(w)
		fmt.Fprint(w, ".Contains(")
		ie.Item.emit(w)
		fmt.Fprint(w, ")")
	} else if isMapExpr(ie.Collection) {
		ie.Collection.emit(w)
		fmt.Fprint(w, ".ContainsKey(")
		ie.Item.emit(w)
		fmt.Fprint(w, ")")
	} else {
		fmt.Fprint(w, "Array.IndexOf(")
		ie.Collection.emit(w)
		fmt.Fprint(w, ", ")
		ie.Item.emit(w)
		fmt.Fprint(w, ") >= 0")
	}
	fmt.Fprint(w, ")")
}

// ExprStmt represents a statement consisting solely of an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

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

// MethodCallExpr represents target.Method(args...).
type MethodCallExpr struct {
	Target Expr
	Name   string
	Args   []Expr
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	fmt.Fprintf(w, ".%s(", m.Name)
	for i, a := range m.Args {
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

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		fmt.Fprint(w, "true")
	} else {
		fmt.Fprint(w, "false")
	}
}

// IndexExpr represents xs[i].
type IndexExpr struct {
	Target Expr
	Index  Expr
}

type FunLit struct {
	Params     []string
	ParamTypes []string
	ReturnType string
	Body       []Stmt
	ExprBody   Expr
}

func (f *FunLit) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if len(f.ParamTypes) > i && f.ParamTypes[i] != "" {
			fmt.Fprintf(w, "%s %s", f.ParamTypes[i], p)
		} else {
			fmt.Fprint(w, p)
		}
	}
	fmt.Fprint(w, ") => ")
	if f.ExprBody != nil {
		f.ExprBody.emit(w)
	} else {
		fmt.Fprint(w, "{")
		for i, st := range f.Body {
			if i > 0 {
				fmt.Fprint(w, " ")
			}
			st.emit(w)
			fmt.Fprint(w, ";")
		}
		fmt.Fprint(w, "}")
	}
}

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (f *FieldExpr) emit(w io.Writer) {
	if isMapExpr(f.Target) {
		fmt.Fprint(w, "((dynamic)(")
		f.Target.emit(w)
		fmt.Fprintf(w, "[\"%s\"]))", f.Name)
	} else {
		f.Target.emit(w)
		fmt.Fprintf(w, ".%s", f.Name)
	}
}

// SliceExpr represents xs[a:b] or s[a:b] for lists and strings.
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SliceExpr) emit(w io.Writer) {
	if isStringExpr(s.Value) {
		s.Value.emit(w)
		fmt.Fprint(w, ".Substring(")
		s.Start.emit(w)
		fmt.Fprint(w, ", ")
		fmt.Fprint(w, "(")
		s.End.emit(w)
		fmt.Fprint(w, " - ")
		s.Start.emit(w)
		fmt.Fprint(w, "))")
	} else {
		s.Value.emit(w)
		fmt.Fprint(w, ".Skip(")
		s.Start.emit(w)
		fmt.Fprint(w, ").Take(")
		fmt.Fprint(w, "(")
		s.End.emit(w)
		fmt.Fprint(w, " - ")
		s.Start.emit(w)
		fmt.Fprint(w, ")).ToArray()")
	}
}

// ContainsExpr represents s.contains(x)
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

func (c *ContainsExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	c.Str.emit(w)
	fmt.Fprint(w, ".Contains(")
	c.Sub.emit(w)
	fmt.Fprint(w, ") ? 1 : 0)")
}

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *StrExpr:
		return true
	case *SliceExpr:
		return isStringExpr(ex.Value)
	case *SubstringExpr:
		return true
	case *VarRef:
		return stringVars[ex.Name]
	case *CallExpr:
		if ex.Func == "Convert.ToString" {
			return true
		}
	}
	return false
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *VarRef:
		return mapVars[ex.Name]
	}
	return false
}

func csType(t *parser.TypeRef) string {
	if t == nil {
		return "object"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "string":
			return "string"
		case "bool":
			return "bool"
		}
		return "object"
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return fmt.Sprintf("%s[]", csType(t.Generic.Args[0]))
			}
			return "object[]"
		case "map":
			if len(t.Generic.Args) == 2 {
				k := csType(t.Generic.Args[0])
				v := csType(t.Generic.Args[1])
				return fmt.Sprintf("Dictionary<%s, %s>", k, v)
			}
			return "Dictionary<object, object>"
		}
	}
	if t.Fun != nil {
		var parts []string
		for _, p := range t.Fun.Params {
			parts = append(parts, csType(p))
		}
		parts = append(parts, csType(t.Fun.Return))
		return fmt.Sprintf("Func<%s>", strings.Join(parts, ", "))
	}
	return "object"
}

func typeOfExpr(e Expr) string {
	switch ex := e.(type) {
	case *StringLit:
		return "string"
	case *IntLit:
		return "int"
	case *BoolLit:
		return "bool"
	case *BoolOpExpr:
		return "bool"
	case *CmpExpr:
		return "bool"
	case *NotExpr:
		return "bool"
	case *InExpr:
		return "bool"
	case *BinaryExpr:
		lt := typeOfExpr(ex.Left)
		rt := typeOfExpr(ex.Right)
		if lt == rt {
			return lt
		}
		if lt == "double" || rt == "double" {
			return "double"
		}
		if lt == "string" || rt == "string" {
			return "string"
		}
		return "int"
	case *IfExpr:
		t := typeOfExpr(ex.Then)
		e := typeOfExpr(ex.Else)
		if t == e {
			return t
		}
		if t == "" {
			return e
		}
		if e == "" {
			return t
		}
		return ""
	case *AvgExpr:
		return "double"
	case *SumExpr:
		return "int"
	case *MinExpr, *MaxExpr:
		return "int"
	case *StrExpr:
		return "string"
	case *SubstringExpr:
		return "string"
	case *ContainsExpr:
		return "int"
	case *ExistsExpr:
		return "bool"
	case *ValuesExpr:
		return valueType(ex.Map)
	case *IndexExpr:
		t := typeOfExpr(ex.Target)
		if strings.HasSuffix(t, "[]") {
			return strings.TrimSuffix(t, "[]")
		}
		if strings.HasPrefix(t, "Dictionary<") {
			parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			arr := strings.Split(parts, ",")
			if len(arr) == 2 {
				return strings.TrimSpace(arr[1])
			}
		}
		return ""
	case *FieldExpr:
		t := typeOfExpr(ex.Target)
		if strings.HasPrefix(t, "Dictionary<") {
			parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			arr := strings.Split(parts, ",")
			if len(arr) == 2 {
				return strings.TrimSpace(arr[1])
			}
		}
		return ""
	case *ListLit:
		return listType(ex)
	case *MapLit:
		k, v := mapTypes(ex)
		return fmt.Sprintf("Dictionary<%s, %s>", k, v)
	case *FunLit:
		return fmt.Sprintf("Func<%s>", strings.Join(append(append([]string{}, ex.ParamTypes...), ex.ReturnType), ", "))
	case *VarRef:
		if t, ok := varTypes[ex.Name]; ok {
			return t
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "ToArray":
			t := typeOfExpr(ex.Target)
			if strings.HasSuffix(t, "[]") {
				return t
			}
			if t != "" {
				return t
			}
			return "object[]"
		case "Select", "Where":
			return typeOfExpr(ex.Target)
		}
	}
	return ""
}

func mapTypes(m *MapLit) (string, string) {
	keyType := ""
	valType := ""
	for i, it := range m.Items {
		kt := typeOfExpr(it.Key)
		vt := typeOfExpr(it.Value)
		if i == 0 {
			keyType = kt
			valType = vt
		} else {
			if keyType != kt {
				keyType = ""
			}
			if valType != vt {
				valType = ""
			}
		}
	}
	if keyType == "" {
		keyType = "object"
	}
	if valType == "" {
		valType = "object"
	}
	return keyType, valType
}

func listType(l *ListLit) string {
	elemType := ""
	for i, e := range l.Elems {
		t := typeOfExpr(e)
		if i == 0 {
			elemType = t
		} else if elemType != t {
			elemType = ""
		}
	}
	if elemType == "" {
		elemType = "object"
	}
	return fmt.Sprintf("%s[]", elemType)
}

func (ix *IndexExpr) emit(w io.Writer) {
	ix.Target.emit(w)
	fmt.Fprint(w, "[")
	ix.Index.emit(w)
	fmt.Fprint(w, "]")
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "new[]{")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

type MapItem struct {
	Key   Expr
	Value Expr
}

type MapLit struct{ Items []MapItem }

func (m *MapLit) emit(w io.Writer) {
	k, v := mapTypes(m)
	if k != "object" || v != "object" {
		usesDict = true
	}
	fmt.Fprintf(w, "new Dictionary<%s, %s>{", k, v)
	for i, it := range m.Items {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprint(w, "{")
		it.Key.emit(w)
		fmt.Fprint(w, ", ")
		it.Value.emit(w)
		fmt.Fprint(w, "}")
	}
	fmt.Fprint(w, "}")
}

type CountExpr struct{ Arg Expr }

func (c *CountExpr) emit(w io.Writer) {
	c.Arg.emit(w)
	if isMapExpr(c.Arg) {
		fmt.Fprint(w, ".Count")
	} else {
		fmt.Fprint(w, ".Length")
	}
}

type AvgExpr struct{ Arg Expr }

func (a *AvgExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	a.Arg.emit(w)
	fmt.Fprint(w, ".Average()).ToString(\"0.0\")")
}

type LenExpr struct{ Arg Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Arg.emit(w)
	if isMapExpr(l.Arg) {
		fmt.Fprint(w, ".Count")
	} else {
		fmt.Fprint(w, ".Length")
	}
}

type SumExpr struct{ Arg Expr }

func (s *SumExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	s.Arg.emit(w)
	fmt.Fprint(w, ".Sum())")
}

type AppendExpr struct {
	List Expr
	Item Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	a.List.emit(w)
	fmt.Fprint(w, ".Append(")
	a.Item.emit(w)
	fmt.Fprint(w, ").ToArray())")
}

type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	if isStringExpr(s.Arg) {
		s.Arg.emit(w)
	} else {
		s.Arg.emit(w)
		fmt.Fprint(w, ".ToString()")
	}
}

type MinExpr struct{ Arg Expr }

func (m *MinExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	m.Arg.emit(w)
	fmt.Fprint(w, ".Min())")
}

type MaxExpr struct{ Arg Expr }

func (m *MaxExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	m.Arg.emit(w)
	fmt.Fprint(w, ".Max())")
}

type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Str.emit(w)
	fmt.Fprint(w, ".Substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", (")
	s.End.emit(w)
	fmt.Fprint(w, " - ")
	s.Start.emit(w)
	fmt.Fprint(w, "))")
}

type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	v.Map.emit(w)
	fmt.Fprint(w, ".Values.ToList()")
}

func valueType(e Expr) string {
	t := typeOfExpr(e)
	if strings.HasPrefix(t, "Dictionary<") {
		parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
		arr := strings.Split(parts, ",")
		if len(arr) == 2 {
			return strings.TrimSpace(arr[1]) + "[]"
		}
	}
	return "object[]"
}

type ExistsExpr struct{ Arg Expr }

func (e *ExistsExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	e.Arg.emit(w)
	fmt.Fprint(w, ".Any())")
}

// Transpile converts a Mochi AST to a simple C# AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	prog := &Program{}
	stringVars = make(map[string]bool)
	mapVars = make(map[string]bool)
	varTypes = make(map[string]string)
	usesDict = false
	usesLinq = false
	usesJson = false
	for _, st := range p.Statements {
		s, err := compileStmt(prog, st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			prog.Stmts = append(prog.Stmts, s)
		}
	}
	_ = env // env reserved for future use
	return prog, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	operands := []Expr{}
	ops := []string{}

	first, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, p := range e.Binary.Right {
		r, err := compilePostfix(p.Right)
		if err != nil {
			return nil, err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, r)
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

	apply := func(left Expr, op string, right Expr) Expr {
		switch op {
		case "==", "!=", "<", "<=", ">", ">=":
			return &CmpExpr{Op: op, Left: left, Right: right}
		case "&&", "||":
			return &BoolOpExpr{Op: op, Left: left, Right: right}
		case "in":
			return &InExpr{Item: left, Collection: right}
		case "union", "union_all", "except", "intersect":
			usesLinq = true
			fallthrough
		default:
			return &BinaryExpr{Op: op, Left: left, Right: right}
		}
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					expr := apply(operands[i], ops[i], operands[i+1])
					operands[i] = expr
					operands = append(operands[:i+1], operands[i+2:]...)
					ops = append(ops[:i], ops[i+1:]...)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
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
		if u.Ops[i] == "!" {
			expr = &NotExpr{Val: expr}
		} else {
			expr = &UnaryExpr{Op: u.Ops[i], Val: expr}
		}
	}
	return expr, nil
}

func compilePostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := compilePrimary(p.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
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
			if !isStringExpr(expr) {
				usesLinq = true
			}
			expr = &SliceExpr{Value: expr, Start: start, End: end}
		case op.Field != nil:
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
		case op.Call != nil:
			if fe, ok := expr.(*FieldExpr); ok && fe.Name == "contains" {
				if len(op.Call.Args) != 1 {
					return nil, fmt.Errorf("unsupported method call")
				}
				arg, err := compileExpr(op.Call.Args[0])
				if err != nil {
					return nil, err
				}
				expr = &ContainsExpr{Str: fe.Target, Sub: arg}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			switch *op.Cast.Type.Simple {
			case "int":
				expr = &CallExpr{Func: "Convert.ToInt32", Args: []Expr{expr}}
			default:
				// other casts are treated as no-ops
			}
		case op.Cast != nil && op.Cast.Type != nil:
			// ignore casts to complex types (e.g. structs)
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func compileStmt(prog *Program, s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		e, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Let != nil:
		var val Expr
		var err error
		if s.Let.Value != nil {
			val, err = compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Let.Type != nil && s.Let.Type.Simple != nil {
			switch *s.Let.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "string":
				val = &StringLit{Value: ""}
				stringVars[s.Let.Name] = true
			case "bool":
				val = &BoolLit{Value: false}
			default:
				return nil, fmt.Errorf("unsupported let type")
			}
			varTypes[s.Let.Name] = csType(s.Let.Type)
		} else {
			return nil, fmt.Errorf("unsupported let")
		}
		if isStringExpr(val) {
			stringVars[s.Let.Name] = true
		}
		if isMapExpr(val) {
			mapVars[s.Let.Name] = true
		}
		if t := typeOfExpr(val); t != "" {
			varTypes[s.Let.Name] = t
		}
		if prog != nil {
			prog.Globals = append(prog.Globals, &Global{Name: s.Let.Name, Value: val})
			return nil, nil
		}
		return &LetStmt{Name: s.Let.Name, Value: val}, nil
	case s.Var != nil:
		var val Expr
		var err error
		if s.Var.Value != nil {
			val, err = compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Var.Type != nil && s.Var.Type.Simple != nil {
			switch *s.Var.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "string":
				val = &StringLit{Value: ""}
				stringVars[s.Var.Name] = true
			case "bool":
				val = &BoolLit{Value: false}
			default:
				return nil, fmt.Errorf("unsupported var type")
			}
			varTypes[s.Var.Name] = csType(s.Var.Type)
		}
		if isStringExpr(val) {
			stringVars[s.Var.Name] = true
		}
		if isMapExpr(val) {
			mapVars[s.Var.Name] = true
		}
		if t := typeOfExpr(val); t != "" {
			varTypes[s.Var.Name] = t
		}
		if prog != nil {
			prog.Globals = append(prog.Globals, &Global{Name: s.Var.Name, Value: val})
			return nil, nil
		}
		return &VarStmt{Name: s.Var.Name, Value: val}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if isStringExpr(val) {
				stringVars[s.Assign.Name] = true
			}
			if isMapExpr(val) {
				mapVars[s.Assign.Name] = true
			}
			if t := typeOfExpr(val); t != "" {
				varTypes[s.Assign.Name] = t
			}
			return &AssignStmt{Name: s.Assign.Name, Value: val}, nil
		}
		if len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0 {
			var target Expr = &VarRef{Name: s.Assign.Name}
			for i := 0; i < len(s.Assign.Index)-1; i++ {
				idx, err := compileExpr(s.Assign.Index[i].Start)
				if err != nil {
					return nil, err
				}
				target = &IndexExpr{Target: target, Index: idx}
			}
			idx, err := compileExpr(s.Assign.Index[len(s.Assign.Index)-1].Start)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			return &AssignIndexStmt{Target: target, Index: idx, Value: val}, nil
		}
		return nil, fmt.Errorf("unsupported assignment")
	case s.Fun != nil:
		params := make([]string, len(s.Fun.Params))
		ptypes := make([]string, len(s.Fun.Params))
		for i, p := range s.Fun.Params {
			params[i] = p.Name
			ptypes[i] = csType(p.Type)
		}
		var body []Stmt
		for _, b := range s.Fun.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		prog.Funcs = append(prog.Funcs, &Function{Name: s.Fun.Name, Params: params, ParamTypes: ptypes, ReturnType: csType(s.Fun.Return), Body: body})
		return nil, nil
	case s.Return != nil:
		var val Expr
		if s.Return.Value != nil {
			var err error
			val, err = compileExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: val}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.For != nil:
		if s.For.RangeEnd != nil {
			varTypes[s.For.Name] = "int"
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
				st, err := compileStmt(prog, b)
				if err != nil {
					return nil, err
				}
				if st != nil {
					body = append(body, st)
				}
			}
			return &ForRangeStmt{Var: s.For.Name, Start: start, End: end, Body: body}, nil
		}
		iterable, err := compileExpr(s.For.Source)
		if err != nil {
			return nil, err
		}
		if t := typeOfExpr(iterable); t != "" {
			if strings.HasPrefix(t, "Dictionary<") {
				parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
				key := strings.Split(parts, ",")[0]
				varTypes[s.For.Name] = strings.TrimSpace(key)
			} else if strings.HasSuffix(t, "[]") {
				varTypes[s.For.Name] = strings.TrimSuffix(t, "[]")
			}
		}
		var body []Stmt
		for _, b := range s.For.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &ForInStmt{Var: s.For.Name, Iterable: iterable, Body: body}, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.While.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.If != nil:
		return compileIfStmt(prog, s.If)
	case s.Test == nil && s.Import == nil && s.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil, nil
}

func compileIfStmt(prog *Program, i *parser.IfStmt) (Stmt, error) {
	cond, err := compileExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, st := range i.Then {
		s, err := compileStmt(prog, st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			thenStmts = append(thenStmts, s)
		}
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := compileIfStmt(prog, i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		for _, st := range i.Else {
			s, err := compileStmt(prog, st)
			if err != nil {
				return nil, err
			}
			if s != nil {
				elseStmts = append(elseStmts, s)
			}
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
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
		switch name {
		case "print":
			name = "Console.WriteLine"
			if len(args) == 1 {
				return &CallExpr{Func: name, Args: args}, nil
			}
			list := &ListLit{Elems: args}
			join := &CallExpr{Func: "string.Join", Args: []Expr{&StringLit{Value: " "}, list}}
			trimmed := &MethodCallExpr{Target: join, Name: "TrimEnd"}
			return &CallExpr{Func: name, Args: []Expr{trimmed}}, nil
		case "append":
			if len(args) == 2 {
				usesLinq = true
				return &AppendExpr{List: args[0], Item: args[1]}, nil
			}
		case "exists":
			if len(args) == 1 {
				usesLinq = true
				return &ExistsExpr{Arg: args[0]}, nil
			}
		case "count":
			if len(args) == 1 {
				return &CountExpr{Arg: args[0]}, nil
			}
		case "avg":
			if len(args) == 1 {
				usesLinq = true
				return &AvgExpr{Arg: args[0]}, nil
			}
		case "len":
			if len(args) == 1 {
				return &LenExpr{Arg: args[0]}, nil
			}
		case "sum":
			if len(args) == 1 {
				usesLinq = true
				return &SumExpr{Arg: args[0]}, nil
			}
		case "str":
			if len(args) == 1 {
				return &StrExpr{Arg: args[0]}, nil
			}
		case "min":
			if len(args) == 1 {
				usesLinq = true
				return &MinExpr{Arg: args[0]}, nil
			}
		case "max":
			if len(args) == 1 {
				usesLinq = true
				return &MaxExpr{Arg: args[0]}, nil
			}
		case "substring":
			if len(args) == 3 {
				return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
			}
		case "values":
			if len(args) == 1 {
				usesDict = true
				usesLinq = true
				return &ValuesExpr{Map: args[0]}, nil
			}
		case "json":
			if len(args) == 1 {
				usesJson = true
				inner := &CallExpr{Func: "JsonSerializer.Serialize", Args: []Expr{args[0]}}
				return &CallExpr{Func: "Console.WriteLine", Args: []Expr{inner}}, nil
			}
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.List != nil:
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
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := compileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			if vr, ok := k.(*VarRef); ok {
				k = &StringLit{Value: vr.Name}
			}
			v, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		usesDict = true
		return &MapLit{Items: items}, nil
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.Selector != nil:
		expr := Expr(&VarRef{Name: p.Selector.Root})
		for _, t := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: t}
		}
		return expr, nil
	case p.Group != nil:
		return compileExpr(p.Group)
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.FunExpr != nil:
		return compileFunExpr(p.FunExpr)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileFunExpr(f *parser.FunExpr) (Expr, error) {
	params := make([]string, len(f.Params))
	ptypes := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.Name
		ptypes[i] = csType(p.Type)
	}
	var body []Stmt
	var expr Expr
	var err error
	if f.ExprBody != nil {
		expr, err = compileExpr(f.ExprBody)
		if err != nil {
			return nil, err
		}
	}
	for _, st := range f.BlockBody {
		s, err2 := compileStmt(nil, st)
		if err2 != nil {
			return nil, err2
		}
		if s != nil {
			body = append(body, s)
		}
	}
	return &FunLit{Params: params, ParamTypes: ptypes, ReturnType: csType(f.Return), Body: body, ExprBody: expr}, nil
}

func compileIfExpr(i *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(i.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if i.ElseIf != nil {
		elseExpr, err = compileIfExpr(i.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if i.Else != nil {
		elseExpr, err = compileExpr(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func compileQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}

	src, err := compileExpr(q.Source)
	if err != nil {
		return nil, err
	}

	t := typeOfExpr(src)
	elemT := "object"
	if strings.HasSuffix(t, "[]") {
		elemT = strings.TrimSuffix(t, "[]")
	} else if strings.HasPrefix(t, "Dictionary<") {
		parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
		arr := strings.Split(parts, ",")
		if len(arr) == 2 {
			elemT = strings.TrimSpace(arr[1])
		}
	}

	saved := varTypes[q.Var]
	savedMap := mapVars[q.Var]
	varTypes[q.Var] = elemT
	if strings.HasPrefix(elemT, "Dictionary<") {
		mapVars[q.Var] = true
	}
	var cond Expr
	if q.Where != nil {
		cond, err = compileExpr(q.Where)
		if err != nil {
			varTypes[q.Var] = saved
			mapVars[q.Var] = savedMap
			return nil, err
		}
	}
	sel, err := compileExpr(q.Select)
	if err != nil {
		varTypes[q.Var] = saved
		return nil, err
	}
	varTypes[q.Var] = saved
	usesLinq = true

	expr := src
	if cond != nil {
		expr = &MethodCallExpr{Target: expr, Name: "Where", Args: []Expr{&FunLit{Params: []string{q.Var}, ExprBody: cond}}}
	}
	expr = &MethodCallExpr{Target: expr, Name: "Select", Args: []Expr{&FunLit{Params: []string{q.Var}, ExprBody: sel}}}
	expr = &MethodCallExpr{Target: expr, Name: "ToArray"}
	return expr, nil
}

// Emit generates formatted C# source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("using System;\n")
	if usesDict {
		buf.WriteString("using System.Collections.Generic;\n")
	}
	if usesLinq {
		buf.WriteString("using System.Linq;\n")
	}
	if usesJson {
		buf.WriteString("using System.Text.Json;\n")
	}
	buf.WriteString("\n")
	buf.WriteString("class Program {\n")
	for _, g := range prog.Globals {
		buf.WriteString("\tstatic ")
		if t, ok := varTypes[g.Name]; ok && t != "" {
			fmt.Fprintf(&buf, "%s %s = ", t, g.Name)
		} else {
			fmt.Fprintf(&buf, "var %s = ", g.Name)
		}
		g.Value.emit(&buf)
		buf.WriteString(";\n")
	}
	for _, fn := range prog.Funcs {
		buf.WriteString("\t")
		fn.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("\tstatic void Main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("\t\t")
		s.emit(&buf)
		if isBlockStmt(s) {
			buf.WriteString("\n")
		} else {
			buf.WriteString(";\n")
		}
	}
	buf.WriteString("\t}\n")
	buf.WriteString("}\n")
	return formatCS(buf.Bytes())
}

// formatCS performs very basic formatting and prepends a standard header.
func formatCS(src []byte) []byte {
	header := "// Generated by Mochi\n"
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	lines := bytes.Split(src, []byte("\n"))
	for i, ln := range lines {
		lines[i] = bytes.TrimRight(ln, " ")
	}
	out := bytes.Join(lines, []byte("\n"))
	if len(out) > 0 && out[len(out)-1] != '\n' {
		out = append(out, '\n')
	}
	return append([]byte(header), out...)
}

// print converts the custom AST to an ast.Node and prints it.
