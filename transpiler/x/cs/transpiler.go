//go:build slow

package cstranspiler

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode"

	yaml "gopkg.in/yaml.v3"
	"mochi/parser"
	"mochi/types"
)

var version string
var currentProg *Program

func init() {
	_, file, _, _ := runtime.Caller(0)
	root := filepath.Join(filepath.Dir(file), "../../..")
	if b, err := os.ReadFile(filepath.Join(root, "VERSION")); err == nil {
		version = strings.TrimSpace(string(b))
	} else {
		version = "unknown"
	}
}

// --- C# AST ---

type Program struct {
	Imports []string
	Structs []StructDecl
	Globals []*Global
	Funcs   []*Function
	Stmts   []Stmt
}

type Global struct {
	Name  string
	Value Expr
}

type StructDecl struct {
	Name   string
	Fields []StructField
}

type StructField struct {
	Name string
	Type string
}

var stringVars map[string]bool
var mapVars map[string]bool
var varTypes map[string]string
var structTypes map[string]types.StructType
var funRets map[string]string
var funParams map[string][]string
var usesDict bool
var usesLinq bool
var usesJson bool
var usesNow bool
var usesInput bool
var globalDecls map[string]*Global
var mutatedVars map[string]bool
var blockDepth int
var importAliases map[string]string
var varAliases map[string]string
var aliasCounter int
var usesFmt bool

// reserved lists C# reserved keywords that cannot be used as identifiers.
var reserved = map[string]bool{
	"abstract": true, "as": true, "base": true, "bool": true, "break": true,
	"byte": true, "case": true, "catch": true, "char": true, "checked": true,
	"class": true, "const": true, "continue": true, "decimal": true,
	"default": true, "delegate": true, "do": true, "double": true,
	"else": true, "enum": true, "event": true, "explicit": true,
	"extern": true, "false": true, "finally": true, "fixed": true,
	"float": true, "for": true, "foreach": true, "goto": true, "if": true,
	"implicit": true, "in": true, "int": true, "interface": true,
	"internal": true, "is": true, "lock": true, "long": true,
	"namespace": true, "new": true, "null": true, "object": true,
	"operator": true, "out": true, "override": true, "params": true,
	"private": true, "protected": true, "public": true, "readonly": true,
	"ref": true, "return": true, "sbyte": true, "sealed": true,
	"short": true, "sizeof": true, "stackalloc": true, "static": true,
	"string": true, "struct": true, "switch": true, "this": true,
	"throw": true, "true": true, "try": true, "typeof": true, "uint": true,
	"ulong": true, "unchecked": true, "unsafe": true, "ushort": true,
	"using": true, "virtual": true, "void": true, "volatile": true,
	"while": true,
}

func safeName(n string) string {
	if reserved[n] {
		return "_" + n
	}
	return n
}

func validIdent(n string) bool {
	for i, r := range n {
		if i == 0 {
			if !(unicode.IsLetter(r) || r == '_') {
				return false
			}
		} else {
			if !(unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_') {
				return false
			}
		}
	}
	return true
}

func gitTimestamp() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			return t.Format("2006-01-02 15:04 MST")
		}
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	return time.Now().In(loc).Format("2006-01-02 15:04 MST")
}

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
	name := safeName(s.Name)
	t := ""
	if s.Value != nil {
		if v := typeOfExpr(s.Value); v != "" && v != "object" {
			t = v
		}
	}
	if t == "" {
		if vt, ok := varTypes[s.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
			t = vt
		}
	}
	if t != "" && t != "object" {
		fmt.Fprintf(w, "%s %s = ", t, name)
	} else {
		fmt.Fprintf(w, "var %s = ", name)
	}
	s.Value.emit(w)
}

// VarStmt represents a mutable variable declaration.
type VarStmt struct {
	Name  string
	Value Expr // optional
}

func (s *VarStmt) emit(w io.Writer) {
	name := safeName(s.Name)
	t := ""
	if s.Value != nil {
		if v := typeOfExpr(s.Value); v != "" && v != "object" {
			t = v
		}
	}
	if t == "" {
		if vt, ok := varTypes[s.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
			t = vt
		}
	}
	if t != "" && t != "object" {
		fmt.Fprintf(w, "%s %s", t, name)
	} else {
		fmt.Fprintf(w, "var %s", name)
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
	fmt.Fprintf(w, "%s = ", safeName(s.Name))
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

// AssignFieldStmt represents obj.field assignment.
type AssignFieldStmt struct {
	Target Expr
	Name   string
	Value  Expr
}

func (a *AssignFieldStmt) emit(w io.Writer) {
	a.Target.emit(w)
	fmt.Fprintf(w, ".%s = ", a.Name)
	a.Value.emit(w)
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

// UpdateStmt updates fields of items in a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (int i = 0; i < %s.Length; i++) {\n", u.Target)
	fmt.Fprintf(w, "    var item = %s[i];\n", u.Target)
	if u.Cond != nil {
		fmt.Fprint(w, "    if (")
		u.Cond.emit(w)
		fmt.Fprint(w, ") {\n")
		for i, f := range u.Fields {
			fmt.Fprintf(w, "        item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, ";\n")
		}
		fmt.Fprint(w, "    }\n")
	} else {
		for i, f := range u.Fields {
			fmt.Fprintf(w, "    item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprintf(w, "    %s[i] = item;\n", u.Target)
	fmt.Fprint(w, "}\n")
}

// SaveStmt saves a list of maps to stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		fmt.Fprint(w, "foreach (var _row in ")
		s.Src.emit(w)
		fmt.Fprint(w, ") {\n")
		elemType := strings.TrimSuffix(typeOfExpr(s.Src), "[]")
		if st, ok := structTypes[elemType]; ok {
			names := append([]string{}, st.Order...)
			sort.Strings(names)
			fmt.Fprint(w, "    var _tmp = new SortedDictionary<string, object>{")
			for i, f := range names {
				if i > 0 {
					fmt.Fprint(w, ", ")
				}
				fmt.Fprintf(w, "{\"%s\", _row.%s}", f, f)
			}
			fmt.Fprint(w, "};\n")
		} else {
			fmt.Fprint(w, "    var _tmp = _row.OrderBy(kv => kv.Key).ToDictionary(kv => kv.Key, kv => kv.Value);\n")
		}
		fmt.Fprint(w, "    Console.WriteLine(JsonSerializer.Serialize(_tmp));\n")
		fmt.Fprint(w, "}")
		return
	}
	fmt.Fprint(w, "// unsupported save")
}

// ForRangeStmt represents a numeric range for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(w io.Writer) {
	v := safeName(f.Var)
	fmt.Fprintf(w, "for (var %s = ", v)
	if f.Start != nil {
		f.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "; ")
	fmt.Fprintf(w, "%s < ", v)
	if f.End != nil {
		f.End.emit(w)
	}
	fmt.Fprint(w, "; ")
	fmt.Fprintf(w, "%s++) {\n", v)
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
	v := safeName(f.Var)
	fmt.Fprintf(w, "foreach (var %s in ", v)
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
	saved := make(map[string]string)
	fmt.Fprintf(w, "static %s %s(", ret, safeName(f.Name))
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := "int"
		if len(f.ParamTypes) > i && f.ParamTypes[i] != "" {
			typ = f.ParamTypes[i]
		}
		fmt.Fprintf(w, "%s %s", typ, safeName(p))
		saved[p] = varTypes[p]
		varTypes[p] = typ
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}\n")
	for name, t := range saved {
		if t == "" {
			delete(varTypes, name)
		} else {
			varTypes[name] = t
		}
	}
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

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, safeName(v.Name)) }

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
		if b.Op == "+" && isListExpr(b.Left) && isListExpr(b.Right) {
			usesLinq = true
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprint(w, ".Concat(")
			b.Right.emit(w)
			fmt.Fprint(w, ").ToArray())")
		} else {
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprintf(w, " %s ", b.Op)
			b.Right.emit(w)
			fmt.Fprint(w, ")")
		}
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
	} else if isMapExpr(ie.Collection) || strings.HasPrefix(typeOfExpr(ie.Collection), "Dictionary<") {
		ie.Collection.emit(w)
		fmt.Fprint(w, ".ContainsKey(")
		ie.Item.emit(w)
		fmt.Fprint(w, ")")
	} else if _, ok := ie.Collection.(*VarRef); ok {
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
	if vr, ok := m.Target.(*VarRef); ok {
		if alias, ok2 := importAliases[vr.Name]; ok2 && alias == "math" {
			fmt.Fprint(w, "Math.")
			fmt.Fprintf(w, "%s(", strings.Title(m.Name))
			for i, a := range m.Args {
				if i > 0 {
					fmt.Fprint(w, ", ")
				}
				a.emit(w)
			}
			fmt.Fprint(w, ")")
			return
		}
	}
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

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) { fmt.Fprintf(w, "%g", f.Value) }

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
	if vr, ok := f.Target.(*VarRef); ok {
		if alias, ok2 := importAliases[vr.Name]; ok2 && alias == "math" {
			name := strings.Title(f.Name)
			if f.Name == "pi" {
				name = "PI"
			}
			fmt.Fprintf(w, "Math.%s", name)
			return
		}
	}
	if isMapExpr(f.Target) {
		fmt.Fprint(w, "((dynamic)(")
		f.Target.emit(w)
		fmt.Fprintf(w, "[\"%s\"]))", f.Name)
	} else {
		f.Target.emit(w)
		fmt.Fprintf(w, ".%s", f.Name)
	}
}

// RawExpr holds arbitrary C# code produced by the transpiler.
type RawExpr struct {
	Code string
	Type string
}

func (r *RawExpr) emit(w io.Writer) { io.WriteString(w, r.Code) }

func exprString(e Expr) string {
	var buf bytes.Buffer
	e.emit(&buf)
	return buf.String()
}

func exprUsesVar(e Expr, name string) bool {
	s := exprString(e)
	return strings.Contains(s, name)
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
	c.Str.emit(w)
	fmt.Fprint(w, ".Contains(")
	c.Sub.emit(w)
	fmt.Fprint(w, ")")
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
	case *IndexExpr:
		return typeOfExpr(ex) == "string"
	case *VarRef:
		return stringVars[ex.Name]
	case *CallExpr:
		if ex.Func == "Convert.ToString" {
			return true
		}
	case *RawExpr:
		return ex.Type == "string"
	case *FmtExpr:
		return true
	case *FmtTopExpr:
		return true
	}
	return false
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *IndexExpr:
		if isMapExpr(ex.Target) {
			return true
		}
	case *VarRef:
		if t, ok := varTypes[ex.Name]; ok {
			if strings.HasSuffix(t, "[]") {
				return false
			}
			if strings.HasPrefix(t, "Dictionary<") {
				return true
			}
			return false
		}
		return false
	default:
		t := typeOfExpr(e)
		if strings.HasSuffix(t, "[]") {
			return false
		}
		if strings.HasPrefix(t, "Dictionary<") {
			return true
		}
	}
	return false
}

func isListExpr(e Expr) bool {
	switch ex := e.(type) {
	case *ListLit:
		return true
	case *SliceExpr:
		return true
	case *AppendExpr:
		return true
	case *VarRef:
		if t, ok := varTypes[ex.Name]; ok {
			return strings.HasSuffix(t, "[]")
		}
	case *RawExpr:
		return strings.HasSuffix(ex.Type, "[]")
	}
	return strings.HasSuffix(typeOfExpr(e), "[]")
}

func isStructExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StructLit:
		return true
	case *VarRef:
		if t, ok := varTypes[ex.Name]; ok {
			_, ok2 := structTypes[t]
			return ok2
		}
	}
	return false
}

// usesVar reports whether expression e references variable name.
func usesVar(e Expr, name string) bool {
	switch ex := e.(type) {
	case *VarRef:
		return ex.Name == name
	case *FieldExpr:
		return usesVar(ex.Target, name)
	case *BinaryExpr:
		return usesVar(ex.Left, name) || usesVar(ex.Right, name)
	case *BoolOpExpr:
		return usesVar(ex.Left, name) || usesVar(ex.Right, name)
	case *CmpExpr:
		return usesVar(ex.Left, name) || usesVar(ex.Right, name)
	case *CallExpr:
		for _, a := range ex.Args {
			if usesVar(a, name) {
				return true
			}
		}
		return false
	case *MethodCallExpr:
		if usesVar(ex.Target, name) {
			return true
		}
		for _, a := range ex.Args {
			if usesVar(a, name) {
				return true
			}
		}
		return false
	case *UnaryExpr:
		return usesVar(ex.Val, name)
	case *NotExpr:
		return usesVar(ex.Val, name)
	case *IndexExpr:
		return usesVar(ex.Target, name) || usesVar(ex.Index, name)
	case *SliceExpr:
		if usesVar(ex.Value, name) {
			return true
		}
		if ex.Start != nil && usesVar(ex.Start, name) {
			return true
		}
		if ex.End != nil && usesVar(ex.End, name) {
			return true
		}
		return false
	}
	return false
}

func isBoolExpr(e Expr) bool {
	return typeOfExpr(e) == "bool"
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
		if st, ok := structTypes[*t.Simple]; ok {
			return st.Name
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

func csTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return fmt.Sprintf("%s[]", csTypeFromType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Dictionary<%s, %s>", csTypeFromType(tt.Key), csTypeFromType(tt.Value))
	case types.OptionType:
		return csTypeFromType(tt.Elem)
	case types.StructType:
		return tt.Name
	default:
		return "object"
	}
}

func structMapType(st types.StructType) string {
	valType := ""
	for _, name := range st.Order {
		t := csTypeFromType(st.Fields[name])
		if valType == "" {
			valType = t
		} else if valType != t {
			valType = "object"
			break
		}
	}
	if valType == "" {
		valType = "object"
	}
	return fmt.Sprintf("Dictionary<string, %s>", valType)
}

func toStructName(name string) string {
	if strings.HasSuffix(name, "s") && len(name) > 1 {
		name = name[:len(name)-1]
	}
	return strings.Title(name)
}

func simpleType(t string) types.Type {
	switch t {
	case "int":
		return types.IntType{}
	case "double":
		return types.FloatType{}
	case "string":
		return types.StringType{}
	case "bool":
		return types.BoolType{}
	default:
		return types.AnyType{}
	}
}

func inferStructMap(varName string, prog *Program, m *MapLit) (Expr, bool) {
	return m, false
}

func inferStructList(varName string, prog *Program, l *ListLit) (Expr, bool) {
	if len(l.Elems) == 0 {
		return l, false
	}
	first, ok := l.Elems[0].(*MapLit)
	if !ok {
		return l, false
	}
	keys := make([]string, len(first.Items))
	typesMap := make(map[string]string)
	for i, it := range first.Items {
		k, ok := it.Key.(*StringLit)
		if !ok {
			return l, false
		}
		keys[i] = k.Value
		typesMap[k.Value] = typeOfExpr(it.Value)
	}
	for _, e := range l.Elems[1:] {
		m, ok := e.(*MapLit)
		if !ok || len(m.Items) != len(keys) {
			return l, false
		}
		for i, it := range m.Items {
			k, ok := it.Key.(*StringLit)
			if !ok || k.Value != keys[i] {
				return l, false
			}
			t := typeOfExpr(it.Value)
			if typesMap[keys[i]] != t {
				typesMap[keys[i]] = "object"
			}
		}
	}
	sname := toStructName(varName)
	if _, exists := structTypes[sname]; exists {
		return l, false
	}
	fields := make([]StructField, len(keys))
	typeFields := make(map[string]types.Type)
	for i, k := range keys {
		fields[i] = StructField{Name: k, Type: typesMap[k]}
		typeFields[k] = simpleType(typesMap[k])
	}
	if prog != nil {
		prog.Structs = append(prog.Structs, StructDecl{Name: sname, Fields: fields})
	}
	structTypes[sname] = types.StructType{Name: sname, Fields: typeFields, Order: keys}
	varTypes[varName] = fmt.Sprintf("%s[]", sname)
	delete(mapVars, varName)
	newElems := make([]Expr, len(l.Elems))
	for i, e := range l.Elems {
		m := e.(*MapLit)
		vals := make([]StructFieldValue, len(keys))
		for j, k := range keys {
			val := m.Items[j].Value
			vals[j] = StructFieldValue{Name: k, Value: val}
		}
		newElems[i] = &StructLit{Name: sname, Fields: vals}
	}
	return &ListLit{Elems: newElems}, true
}

func typeOfExpr(e Expr) string {
	switch ex := e.(type) {
	case *StringLit:
		return "string"
	case *IntLit:
		return "int"
	case *FloatLit:
		return "double"
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
		t := typeOfExpr(ex.Arg)
		if strings.HasSuffix(t, "[]") {
			t = strings.TrimSuffix(t, "[]")
		}
		if t == "double" {
			return "double"
		}
		return "int"
	case *CountExpr:
		return "int"
	case *MinExpr:
		t := typeOfExpr(ex.Arg)
		if strings.HasSuffix(t, "[]") {
			t = strings.TrimSuffix(t, "[]")
		}
		if t == "double" {
			return "double"
		}
		return "int"
	case *MaxExpr:
		t := typeOfExpr(ex.Arg)
		if strings.HasSuffix(t, "[]") {
			t = strings.TrimSuffix(t, "[]")
		}
		if t == "double" {
			return "double"
		}
		return "int"
	case *StrExpr:
		return "string"
	case *SubstringExpr:
		return "string"
	case *ContainsExpr:
		return "bool"
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
		if st, ok := structTypes[t]; ok {
			if ft, ok2 := st.Fields[ex.Name]; ok2 {
				return csTypeFromType(ft)
			}
		}
		if strings.HasPrefix(t, "Dictionary<") {
			parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			arr := strings.Split(parts, ",")
			if len(arr) == 2 {
				return strings.TrimSpace(arr[1])
			}
		}
		return ""
	case *SliceExpr:
		return typeOfExpr(ex.Value)
	case *ListLit:
		return listType(ex)
	case *MapLit:
		k, v := mapTypes(ex)
		return fmt.Sprintf("Dictionary<%s, %s>", k, v)
	case *StructLit:
		return ex.Name
	case *AppendExpr:
		t := typeOfExpr(ex.List)
		if strings.HasSuffix(t, "[]") {
			return t
		}
		return ""
	case *FunLit:
		return fmt.Sprintf("Func<%s>", strings.Join(append(append([]string{}, ex.ParamTypes...), ex.ReturnType), ", "))
	case *VarRef:
		if t, ok := varTypes[ex.Name]; ok {
			return t
		}
	case *CallExpr:
		if ret, ok := funRets[ex.Func]; ok {
			return ret
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
		default:
			if vr, ok := ex.Target.(*VarRef); ok {
				if alias, ok2 := importAliases[vr.Name]; ok2 && alias == "math" {
					return "double"
				}
			}
		}
	case *RawExpr:
		return ex.Type
	case *FmtExpr:
		return "string"
	case *FmtTopExpr:
		return "string"
	}
	return ""
}

func mapTypes(m *MapLit) (string, string) {
	keyType := m.KeyType
	valType := m.ValType
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
	if len(l.Elems) == 0 {
		if l.ElemType != "" {
			return l.ElemType
		}
		return "object[]"
	}
	elemType := ""
	for i, e := range l.Elems {
		t := typeOfExpr(e)
		if t == "" {
			switch v := e.(type) {
			case *IntLit:
				t = "int"
			case *FloatLit:
				t = "double"
			case *VarRef:
				if vt, ok := varTypes[v.Name]; ok {
					t = vt
				}
			}
		}
		if i == 0 {
			elemType = t
		} else if elemType != t {
			elemType = ""
		}
	}
	if elemType == "" {
		allInt := true
		for _, e := range l.Elems {
			switch v := e.(type) {
			case *IntLit:
			case *VarRef:
				if t, ok := varTypes[v.Name]; !ok || t != "int" {
					allInt = false
				}
			default:
				allInt = false
			}
			if !allInt {
				break
			}
		}
		if allInt {
			elemType = "int"
		} else {
			elemType = "object"
		}
	}
	return fmt.Sprintf("%s[]", elemType)
}

func (ix *IndexExpr) emit(w io.Writer) {
	t := typeOfExpr(ix)
	if (t == "object" || t == "") && isMapExpr(ix.Target) {
		fmt.Fprint(w, "((dynamic)")
		ix.Target.emit(w)
		fmt.Fprint(w, ")[")
		ix.Index.emit(w)
		fmt.Fprint(w, "]")
		return
	}
	ix.Target.emit(w)
	fmt.Fprint(w, "[")
	ix.Index.emit(w)
	fmt.Fprint(w, "]")
}

type ListLit struct {
	Elems    []Expr
	ElemType string // optional element type when list is empty
}

func (l *ListLit) emit(w io.Writer) {
	t := l.ElemType
	if t == "" {
		t = listType(l)
	}
	fmt.Fprintf(w, "new %s{", t)
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

type MapLit struct {
	Items   []MapItem
	KeyType string
	ValType string
}

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

type StructFieldValue struct {
	Name  string
	Value Expr
}

type StructLit struct {
	Name   string
	Fields []StructFieldValue
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "new %s{", s.Name)
	for i, f := range s.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s = ", f.Name)
		f.Value.emit(w)
	}
	fmt.Fprint(w, "}")
}

type CountExpr struct{ Arg Expr }

func (c *CountExpr) emit(w io.Writer) {
	typ := typeOfExpr(c.Arg)
	if st, ok := structTypes[typ]; ok {
		if _, ok := st.Fields["items"]; ok {
			c.Arg.emit(w)
			fmt.Fprint(w, ".items.Length")
			return
		}
	}
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
	fmt.Fprint(w, ".Average())")
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

// FmtExpr wraps another expression with the _fmt helper.
type FmtExpr struct{ Value Expr }

func (f *FmtExpr) emit(w io.Writer) {
	fmt.Fprint(w, "_fmt(")
	f.Value.emit(w)
	fmt.Fprint(w, ")")
}

// FmtTopExpr calls the _fmtTop helper on the value.
type FmtTopExpr struct{ Value Expr }

func (f *FmtTopExpr) emit(w io.Writer) {
	fmt.Fprint(w, "_fmtTop(")
	f.Value.emit(w)
	fmt.Fprint(w, ")")
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
	if mc, ok := e.Arg.(*MethodCallExpr); ok && mc.Name == "ToArray" {
		fmt.Fprint(w, "(")
		mc.Target.emit(w)
		fmt.Fprint(w, ".Any())")
		return
	}
	fmt.Fprint(w, "(")
	e.Arg.emit(w)
	fmt.Fprint(w, ".Any())")
}

// Transpile converts a Mochi AST to a simple C# AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	prog := &Program{}
	currentProg = prog
	defer func() { currentProg = nil }()
	stringVars = make(map[string]bool)
	mapVars = make(map[string]bool)
	varTypes = make(map[string]string)
	structTypes = env.Structs()
	funRets = make(map[string]string)
	funParams = make(map[string][]string)
	usesDict = false
	usesLinq = false
	usesJson = false
	usesNow = false
	usesInput = false
	usesFmt = false
	globalDecls = make(map[string]*Global)
	mutatedVars = make(map[string]bool)
	importAliases = make(map[string]string)
	varAliases = make(map[string]string)
	aliasCounter = 0
	for _, st := range p.Statements {
		s, err := compileStmt(prog, st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			prog.Stmts = append(prog.Stmts, s)
		}
	}
	for name, st := range structTypes {
		found := false
		for _, decl := range prog.Structs {
			if decl.Name == name {
				found = true
				break
			}
		}
		if !found && len(st.Order) > 0 {
			fields := make([]StructField, len(st.Order))
			for i, n := range st.Order {
				fields[i] = StructField{Name: n, Type: csTypeFromType(st.Fields[n])}
			}
			prog.Structs = append(prog.Structs, StructDecl{Name: name, Fields: fields})
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
			if typeOfExpr(expr) != "bool" {
				expr = &CallExpr{Func: "Convert.ToBoolean", Args: []Expr{expr}}
			}
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
			if lit, ok := idx.(*StringLit); ok && isStructExpr(expr) {
				if t := typeOfExpr(expr); t != "" {
					if st, ok2 := structTypes[t]; ok2 {
						if _, ok3 := st.Fields[lit.Value]; ok3 {
							expr = &FieldExpr{Target: expr, Name: lit.Value}
							continue
						}
					}
				}
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			var start Expr
			var end Expr
			var err error
			if op.Index.Start != nil {
				start, err = compileExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
			} else {
				start = &IntLit{Value: 0}
			}
			if op.Index.End != nil {
				end, err = compileExpr(op.Index.End)
				if err != nil {
					return nil, err
				}
			} else {
				end = &LenExpr{Arg: expr}
			}
			if isStringExpr(expr) {
				expr = &SubstringExpr{Str: expr, Start: start, End: end}
			} else {
				usesLinq = true
				expr = &SliceExpr{Value: expr, Start: start, End: end}
			}
		case op.Field != nil:
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ex, err := compileExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			if fe, ok := expr.(*FieldExpr); ok {
				if fe.Name == "contains" {
					if len(args) != 1 {
						return nil, fmt.Errorf("unsupported method call")
					}
					expr = &ContainsExpr{Str: fe.Target, Sub: args[0]}
				} else {
					expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
				}
			} else if vr, ok := expr.(*VarRef); ok {
				expr = &CallExpr{Func: vr.Name, Args: args}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			switch *op.Cast.Type.Simple {
			case "int":
				expr = &CallExpr{Func: "Convert.ToInt32", Args: []Expr{expr}}
			case "float":
				expr = &CallExpr{Func: "Convert.ToDouble", Args: []Expr{expr}}
			default:
				// other casts are treated as no-ops
			}
		case op.Cast != nil && op.Cast.Type != nil:
			if op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "list" && len(op.Cast.Type.Generic.Args) == 1 {
				typ := fmt.Sprintf("%s[]", csType(op.Cast.Type.Generic.Args[0]))
				expr = &RawExpr{Code: fmt.Sprintf("(%s)"+exprString(expr), typ), Type: typ}
			} else if op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "map" && len(op.Cast.Type.Generic.Args) == 2 {
				kt := csType(op.Cast.Type.Generic.Args[0])
				vt := csType(op.Cast.Type.Generic.Args[1])
				typ := fmt.Sprintf("Dictionary<%s, %s>", kt, vt)
				expr = &RawExpr{Code: fmt.Sprintf("(%s)"+exprString(expr), typ), Type: typ}
			} // ignore other casts
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func compileStmt(prog *Program, s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		if se := extractSaveExpr(s.Expr.Expr); se != nil {
			src, err := compileExpr(se.Src)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			usesJson = true
			usesLinq = true
			usesDict = true
			return &SaveStmt{Src: src, Path: path, Format: format}, nil
		}
		e, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Let != nil:
		if _, ok := varTypes[s.Let.Name]; ok {
			if s.Let.Value == nil {
				return nil, fmt.Errorf("redeclare without value")
			}
			val, err := compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: s.Let.Name, Value: val}, nil
		}
		var val Expr
		var err error
		if s.Let.Value != nil {
			val, err = compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
			if s.Let.Type != nil {
				varTypes[s.Let.Name] = csType(s.Let.Type)
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
		if list, ok := val.(*ListLit); ok {
			if res, changed := inferStructList(s.Let.Name, prog, list); changed {
				val = res
			}
			if len(list.Elems) == 0 && s.Let.Type != nil && s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "list" && len(s.Let.Type.Generic.Args) == 1 {
				list.ElemType = fmt.Sprintf("%s[]", csType(s.Let.Type.Generic.Args[0]))
			}
		}
		if mp, ok := val.(*MapLit); ok {
			if len(mp.Items) == 0 && s.Let.Type != nil && s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "map" && len(s.Let.Type.Generic.Args) == 2 {
				mp.KeyType = csType(s.Let.Type.Generic.Args[0])
				mp.ValType = csType(s.Let.Type.Generic.Args[1])
			} else if len(mp.Items) > 2 && false {
				if res, changed := inferStructMap(s.Let.Name, prog, mp); changed {
					val = res
				}
			}
		}
		if isStringExpr(val) {
			stringVars[s.Let.Name] = true
		}
		if isMapExpr(val) {
			mapVars[s.Let.Name] = true
		}
		if t := typeOfExpr(val); t != "" {
			if s.Let.Type == nil {
				varTypes[s.Let.Name] = t
			}
		}
		alias := s.Let.Name
		if _, ok := varAliases[s.Let.Name]; ok {
			alias = fmt.Sprintf("%s_%d", s.Let.Name, aliasCounter)
			aliasCounter++
			varAliases[s.Let.Name] = alias
		} else {
			varAliases[s.Let.Name] = alias
		}
		if t, ok := varTypes[s.Let.Name]; ok {
			varTypes[alias] = t
			if alias != s.Let.Name {
				delete(varTypes, s.Let.Name)
			}
		}
		if prog != nil && blockDepth == 0 {
			for m := range mutatedVars {
				if exprUsesVar(val, m) {
					return &LetStmt{Name: alias, Value: val}, nil
				}
			}
			g := &Global{Name: alias, Value: val}
			prog.Globals = append(prog.Globals, g)
			globalDecls[s.Let.Name] = g
			return nil, nil
		}
		return &LetStmt{Name: alias, Value: val}, nil
	case s.Var != nil:
		if _, ok := varTypes[s.Var.Name]; ok {
			if s.Var.Value == nil {
				return nil, fmt.Errorf("redeclare without value")
			}
			val, err := compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
			name := s.Var.Name
			if alias, ok := varAliases[name]; ok {
				name = alias
			}
			return &AssignStmt{Name: name, Value: val}, nil
		}
		var val Expr
		var err error
		if s.Var.Value != nil {
			val, err = compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
			if s.Var.Type != nil {
				varTypes[s.Var.Name] = csType(s.Var.Type)
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
		if list, ok := val.(*ListLit); ok {
			if len(list.Elems) == 0 {
				if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "list" && len(s.Var.Type.Generic.Args) == 1 {
					list.ElemType = fmt.Sprintf("%s[]", csType(s.Var.Type.Generic.Args[0]))
				} else {
					list.ElemType = "object[]"
					varTypes[s.Var.Name] = "object[]"
				}
			}
			_ = list // do not convert mutable vars to structs
		}
		if mp, ok := val.(*MapLit); ok {
			if len(mp.Items) == 0 && s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "map" && len(s.Var.Type.Generic.Args) == 2 {
				mp.KeyType = csType(s.Var.Type.Generic.Args[0])
				mp.ValType = csType(s.Var.Type.Generic.Args[1])
			}
			_ = mp // keep as dictionary for mutable vars
		}
		if isStringExpr(val) {
			stringVars[s.Var.Name] = true
		}
		if isMapExpr(val) {
			mapVars[s.Var.Name] = true
		}
		if t := typeOfExpr(val); t != "" {
			if s.Var.Type == nil {
				varTypes[s.Var.Name] = t
			}
		}
		alias := s.Var.Name
		if _, ok := varAliases[s.Var.Name]; ok {
			alias = fmt.Sprintf("%s_%d", s.Var.Name, aliasCounter)
			aliasCounter++
			varAliases[s.Var.Name] = alias
		} else {
			varAliases[s.Var.Name] = alias
		}
		if t, ok := varTypes[s.Var.Name]; ok {
			varTypes[alias] = t
			if alias != s.Var.Name {
				delete(varTypes, s.Var.Name)
			}
		}
		if prog != nil && blockDepth == 0 {
			g := &Global{Name: alias, Value: val}
			prog.Globals = append(prog.Globals, g)
			globalDecls[s.Var.Name] = g
			return nil, nil
		}
		return &VarStmt{Name: alias, Value: val}, nil
	case s.Type != nil:
		if prog != nil {
			if st, ok := structTypes[s.Type.Name]; ok {
				fields := make([]StructField, len(st.Order))
				for i, n := range st.Order {
					fields[i] = StructField{Name: n, Type: csTypeFromType(st.Fields[n])}
				}
				prog.Structs = append(prog.Structs, StructDecl{Name: s.Type.Name, Fields: fields})
			}
		}
		return nil, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if app, ok := val.(*AppendExpr); ok {
				if vt, ok2 := varTypes[s.Assign.Name]; ok2 {
					item := app.Item
					if mp, ok3 := item.(*MapLit); ok3 {
						if len(mp.Items) > 2 {
							if res, changed := inferStructMap(s.Assign.Name, prog, mp); changed {
								item = res
							}
						}
					}
					t := typeOfExpr(item)
					if t != "" {
						if vt == "" || vt == "object[]" {
							varTypes[s.Assign.Name] = t + "[]"
						} else if strings.HasSuffix(vt, "[]") {
							cur := strings.TrimSuffix(vt, "[]")
							if cur != t {
								if cur == "int" && t == "double" {
									varTypes[s.Assign.Name] = "double[]"
								} else {
									varTypes[s.Assign.Name] = "object[]"
								}
							}
						}
						if g, ok3 := globalDecls[s.Assign.Name]; ok3 {
							g.Value = &RawExpr{Code: fmt.Sprintf("new %s{}", varTypes[s.Assign.Name])}
						}
					}
					val = &AppendExpr{List: app.List, Item: item}
				}
			}
			if isStringExpr(val) {
				stringVars[s.Assign.Name] = true
			}
			if isMapExpr(val) {
				mapVars[s.Assign.Name] = true
				if mp, ok := val.(*MapLit); ok {
					if len(mp.Items) > 2 {
						if res, changed := inferStructMap(s.Assign.Name, prog, mp); changed {
							val = res
						}
					}
				}
			}
			if t := typeOfExpr(val); t != "" {
				varTypes[s.Assign.Name] = t
			}
			mutatedVars[s.Assign.Name] = true
			name := s.Assign.Name
			if alias, ok := varAliases[name]; ok {
				name = alias
			}
			return &AssignStmt{Name: name, Value: val}, nil
		}
		if len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0 {
			name := s.Assign.Name
			if alias, ok := varAliases[name]; ok {
				name = alias
			}
			var target Expr = &VarRef{Name: name}
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
		if len(s.Assign.Field) > 0 && len(s.Assign.Index) == 0 {
			name := s.Assign.Name
			if alias, ok := varAliases[name]; ok {
				name = alias
			}
			var target Expr = &VarRef{Name: name}
			for i := 0; i < len(s.Assign.Field)-1; i++ {
				target = &FieldExpr{Target: target, Name: s.Assign.Field[i].Name}
			}
			fieldName := s.Assign.Field[len(s.Assign.Field)-1].Name
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if isMapExpr(target) {
				idx := &StringLit{Value: fieldName}
				return &AssignIndexStmt{Target: target, Index: idx, Value: val}, nil
			}
			return &AssignFieldStmt{Target: target, Name: fieldName, Value: val}, nil
		}
		return nil, fmt.Errorf("unsupported assignment")
	case s.Fun != nil:
		params := make([]string, len(s.Fun.Params))
		ptypes := make([]string, len(s.Fun.Params))
		// save global variable maps so local declarations don't leak
		savedAll := cloneStringMap(varTypes)
		savedMap := cloneBoolMap(mapVars)
		savedStrAll := cloneBoolMap(stringVars)
		savedMut := cloneBoolMap(mutatedVars)
		saved := make(map[string]string)
		savedStr := make(map[string]bool)
		for i, p := range s.Fun.Params {
			params[i] = p.Name
			ptypes[i] = csType(p.Type)
			saved[p.Name] = varTypes[p.Name]
			varTypes[p.Name] = ptypes[i]
			savedStr[p.Name] = stringVars[p.Name]
			if ptypes[i] == "string" {
				stringVars[p.Name] = true
			} else {
				delete(stringVars, p.Name)
			}
		}
		retType := csType(s.Fun.Return)
		var body []Stmt
		if prog != nil && blockDepth > 0 {
			prog = nil
		}
		blockDepth++
		for _, b := range s.Fun.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		blockDepth--
		for name, typ := range saved {
			if typ == "" {
				delete(varTypes, name)
			} else {
				varTypes[name] = typ
			}
		}
		for name, val := range savedStr {
			if !val {
				delete(stringVars, name)
			} else {
				stringVars[name] = val
			}
		}
		// restore global maps
		varTypes = savedAll
		mapVars = savedMap
		stringVars = savedStrAll
		mutatedVars = savedMut
		if prog != nil && blockDepth == 0 {
			varTypes[s.Fun.Name] = fmt.Sprintf("fn/%d", len(ptypes))
			funRets[s.Fun.Name] = retType
			funParams[s.Fun.Name] = append([]string{}, ptypes...)
		}
		if s.Fun.Return == nil {
			retType = ""
		}
		if prog != nil {
			prog.Funcs = append(prog.Funcs, &Function{Name: s.Fun.Name, Params: params, ParamTypes: ptypes, ReturnType: retType, Body: body})
			return nil, nil
		}
		lit := &FunLit{Params: params, ParamTypes: ptypes, ReturnType: retType, Body: body}
		return &LetStmt{Name: s.Fun.Name, Value: lit}, nil
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
			savedVar := varTypes[s.For.Name]
			savedAlias := varAliases[s.For.Name]
			varTypes[s.For.Name] = "int"
			alias := fmt.Sprintf("%s_%d", s.For.Name, aliasCounter)
			aliasCounter++
			varAliases[s.For.Name] = alias
			start, err := compileExpr(s.For.Source)
			if err != nil {
				return nil, err
			}
			end, err := compileExpr(s.For.RangeEnd)
			if err != nil {
				return nil, err
			}
			savedVars := cloneStringMap(varTypes)
			savedMap := cloneBoolMap(mapVars)
			savedStr := cloneBoolMap(stringVars)
			savedMut := cloneBoolMap(mutatedVars)
			var body []Stmt
			blockDepth++
			for _, b := range s.For.Body {
				st, err := compileStmt(prog, b)
				if err != nil {
					return nil, err
				}
				if st != nil {
					body = append(body, st)
				}
			}
			blockDepth--
			varTypes = savedVars
			mapVars = savedMap
			stringVars = savedStr
			mutatedVars = savedMut
			stmt := &ForRangeStmt{Var: alias, Start: start, End: end, Body: body}
			if savedVar == "" {
				delete(varTypes, s.For.Name)
			} else {
				varTypes[s.For.Name] = savedVar
			}
			if savedAlias == "" {
				delete(varAliases, s.For.Name)
			} else {
				varAliases[s.For.Name] = savedAlias
			}
			return stmt, nil
		}
		savedVar := varTypes[s.For.Name]
		savedAlias := varAliases[s.For.Name]
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
		alias := fmt.Sprintf("%s_%d", s.For.Name, aliasCounter)
		aliasCounter++
		varAliases[s.For.Name] = alias
		savedVars := cloneStringMap(varTypes)
		savedMap := cloneBoolMap(mapVars)
		savedStr := cloneBoolMap(stringVars)
		savedMut := cloneBoolMap(mutatedVars)
		var body []Stmt
		blockDepth++
		for _, b := range s.For.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		blockDepth--
		varTypes = savedVars
		mapVars = savedMap
		stringVars = savedStr
		mutatedVars = savedMut
		stmt := &ForInStmt{Var: alias, Iterable: iterable, Body: body}
		if savedVar == "" {
			delete(varTypes, s.For.Name)
		} else {
			varTypes[s.For.Name] = savedVar
		}
		if savedAlias == "" {
			delete(varAliases, s.For.Name)
		} else {
			varAliases[s.For.Name] = savedAlias
		}
		return stmt, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		savedVars := cloneStringMap(varTypes)
		savedMap := cloneBoolMap(mapVars)
		savedStr := cloneBoolMap(stringVars)
		savedMut := cloneBoolMap(mutatedVars)
		var body []Stmt
		blockDepth++
		for _, b := range s.While.Body {
			st, err := compileStmt(prog, b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		blockDepth--
		varTypes = savedVars
		mapVars = savedMap
		stringVars = savedStr
		mutatedVars = savedMut
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.If != nil:
		return compileIfStmt(prog, s.If)
	case s.Update != nil:
		return compileUpdateStmt(s.Update)
	case s.Import != nil:
		if prog != nil && s.Import.Lang != nil && *s.Import.Lang == "python" {
			if s.Import.Path == "\"math\"" || s.Import.Path == "math" {
				prog.Imports = append(prog.Imports, fmt.Sprintf("using %s = System.Math;", s.Import.As))
				importAliases[s.Import.As] = "math"
				return nil, nil
			}
		} else if prog != nil && s.Import.Auto && s.Import.Lang != nil && *s.Import.Lang == "go" {
			path := strings.Trim(s.Import.Path, "\"")
			if path == "net" {
				if _, ok := importAliases[s.Import.As]; !ok {
					stub := "using System.Net;\n" +
						"using System.Linq;\n" +
						"static class net {\n" +
						"    public static object[] LookupHost(string host) {\n" +
						"        try {\n" +
						"            var addrs = Dns.GetHostAddresses(host).Select(a => a.ToString()).ToArray();\n" +
						"            return new object[]{addrs, null};\n" +
						"        } catch (Exception ex) {\n" +
						"            return new object[]{null, ex.Message};\n" +
						"        }\n" +
						"    }\n" +
						"}\n"
					prog.Imports = append(prog.Imports, stub)
					usesLinq = true
				}
				importAliases[s.Import.As] = "net"
			} else if path == "mochi/runtime/ffi/go/testpkg" {
				if _, ok := importAliases[s.Import.As]; !ok {
					stub := "using System.Security.Cryptography;\n" +
						"using System.Text;\n" +
						"static class testpkg {\n" +
						"    public static string FifteenPuzzleExample() => \"Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd\";\n" +
						"    public static string MD5Hex(string s) {\n" +
						"        using var md5 = System.Security.Cryptography.MD5.Create();\n" +
						"        var bytes = System.Text.Encoding.ASCII.GetBytes(s);\n" +
						"        var hash = md5.ComputeHash(bytes);\n" +
						"        return string.Concat(hash.Select(b => b.ToString(\"x2\")));\n" +
						"    }\n" +
						"}\n"
					prog.Imports = append(prog.Imports, stub)
					usesLinq = true
				}
				importAliases[s.Import.As] = "testpkg"
			}
			varTypes[s.Import.As] = "dynamic"
			return nil, nil
		}
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		return nil, nil
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
	savedVars := cloneStringMap(varTypes)
	blockDepth++
	for _, st := range i.Then {
		s, err := compileStmt(prog, st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			thenStmts = append(thenStmts, s)
		}
	}
	blockDepth--
	varTypes = savedVars
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := compileIfStmt(prog, i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		savedVars := cloneStringMap(varTypes)
		blockDepth++
		for _, st := range i.Else {
			s, err := compileStmt(prog, st)
			if err != nil {
				return nil, err
			}
			if s != nil {
				elseStmts = append(elseStmts, s)
			}
		}
		blockDepth--
		varTypes = savedVars
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func compileUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	fields := make([]string, len(u.Set.Items))
	values := make([]Expr, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, ok := simpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := compileExpr(it.Value)
		if err != nil {
			return nil, err
		}
		fields[i] = key
		values[i] = val
	}
	var cond Expr
	if u.Where != nil {
		var err error
		cond, err = compileExpr(u.Where)
		if err != nil {
			return nil, err
		}
	}
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
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
		if name == "now" && len(args) == 0 {
			usesNow = true
			return &RawExpr{Code: "_now()", Type: "int"}, nil
		}
		if sig, ok := varTypes[name]; ok && strings.HasPrefix(sig, "fn/") {
			n, err := strconv.Atoi(strings.TrimPrefix(sig, "fn/"))
			if err == nil && len(args) < n {
				missing := n - len(args)
				params := make([]string, missing)
				ptypes := make([]string, missing)
				for i := 0; i < missing; i++ {
					params[i] = fmt.Sprintf("p%d", i)
				}
				callArgs := make([]Expr, 0, n)
				callArgs = append(callArgs, args...)
				for i := 0; i < missing; i++ {
					callArgs = append(callArgs, &VarRef{Name: params[i]})
				}
				body := &CallExpr{Func: name, Args: callArgs}
				if types, ok := funParams[name]; ok {
					copy(ptypes, types[len(args):])
				}
				ret := "object"
				if r, ok := funRets[name]; ok {
					ret = r
				}
				return &FunLit{Params: params, ParamTypes: ptypes, ReturnType: ret, ExprBody: body}, nil
			}
		}
		switch name {
		case "print":
			name = "Console.WriteLine"
			if len(args) == 1 {
				arg := args[0]
				if isBoolExpr(arg) {
					inner := &IfExpr{Cond: arg, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
					return &CallExpr{Func: name, Args: []Expr{inner}}, nil
				}
				if _, ok := arg.(*MapLit); ok {
					usesJson = true
					inner := &CallExpr{Func: "JsonSerializer.Serialize", Args: []Expr{arg}}
					return &CallExpr{Func: name, Args: []Expr{inner}}, nil
				}
				if _, ok := arg.(*StructLit); ok {
					usesJson = true
					inner := &CallExpr{Func: "JsonSerializer.Serialize", Args: []Expr{arg}}
					return &CallExpr{Func: name, Args: []Expr{inner}}, nil
				}
				usesFmt = true
				return &CallExpr{Func: name, Args: []Expr{&FmtTopExpr{Value: arg}}}, nil
			}
			elems := make([]Expr, len(args))
			for i, a := range args {
				elems[i] = &FmtTopExpr{Value: a}
			}
			list := &ListLit{Elems: elems}
			join := &CallExpr{Func: "string.Join", Args: []Expr{&StringLit{Value: " "}, list}}
			usesFmt = true
			return &CallExpr{Func: name, Args: []Expr{join}}, nil
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
		case "abs":
			if len(args) == 1 {
				return &CallExpr{Func: "Math.Abs", Args: []Expr{args[0]}}, nil
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
		case "int":
			if len(args) == 1 {
				return &CallExpr{Func: "Convert.ToInt32", Args: []Expr{args[0]}}, nil
			}
		case "float":
			if len(args) == 1 {
				return &CallExpr{Func: "Convert.ToDouble", Args: []Expr{args[0]}}, nil
			}
		case "upper":
			if len(args) == 1 {
				return &MethodCallExpr{Target: args[0], Name: "ToUpper", Args: nil}, nil
			}
		case "lower":
			if len(args) == 1 {
				return &MethodCallExpr{Target: args[0], Name: "ToLower", Args: nil}, nil
			}
		case "input":
			if len(args) == 0 {
				usesInput = true
				return &RawExpr{Code: "_input()", Type: "string"}, nil
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
	case p.Lit != nil && p.Lit.Float != nil:
		return &FloatLit{Value: *p.Lit.Float}, nil
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
		lit := &ListLit{Elems: elems}
		lit.ElemType = listType(lit)
		return lit, nil
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
	case p.Struct != nil:
		st, ok := structTypes[p.Struct.Name]
		if !ok {
			return nil, fmt.Errorf("unknown struct %s", p.Struct.Name)
		}
		fields := make([]StructFieldValue, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			val, err := compileExpr(f.Value)
			if err != nil {
				return nil, err
			}
			fields[i] = StructFieldValue{Name: f.Name, Value: val}
		}
		return &StructLit{Name: st.Name, Fields: fields}, nil
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		if format == "" {
			format = "yaml"
		}
		if format != "yaml" && format != "jsonl" {
			return nil, fmt.Errorf("unsupported load format")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type)
		if err != nil {
			return nil, err
		}
		return expr, nil
	case p.Match != nil:
		return compileMatchExpr(p.Match)
	case p.Selector != nil:
		name := p.Selector.Root
		if alias, ok := varAliases[name]; ok {
			name = alias
		}
		if name == "nil" && len(p.Selector.Tail) == 0 {
			return &RawExpr{Code: "null", Type: "object"}, nil
		}
		expr := Expr(&VarRef{Name: name})
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

func compileMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := compileExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var body bytes.Buffer
	body.WriteString("var __t = ")
	target.emit(&body)
	body.WriteString("; ")
	retType := "object"
	for i, c := range me.Cases {
		name, vars, ok := structPattern(c.Pattern)
		if !ok {
			return nil, fmt.Errorf("unsupported match pattern")
		}
		saved := make(map[string]string)
		st := structTypes[name]
		for j, v := range vars {
			saved[v] = varTypes[v]
			varTypes[v] = csTypeFromType(st.Fields[st.Order[j]])
		}
		res, err := compileExpr(c.Result)
		for _, v := range vars {
			if saved[v] == "" {
				delete(varTypes, v)
			} else {
				varTypes[v] = saved[v]
			}
		}
		if err != nil {
			return nil, err
		}
		if i > 0 {
			body.WriteString(" else ")
		}
		tmp := fmt.Sprintf("_p%d", i)
		body.WriteString("if (__t is " + name + " " + tmp + ") { ")
		for j, v := range vars {
			field := st.Order[j]
			body.WriteString(fmt.Sprintf("var %s = %s.%s; ", safeName(v), tmp, field))
		}
		if retType == "object" {
			t := typeOfExpr(res)
			if t != "" {
				retType = t
			}
		}
		var rbuf bytes.Buffer
		res.emit(&rbuf)
		body.WriteString("return " + rbuf.String() + "; }")
	}
	body.WriteString(" return default(" + retType + "); ")
	code := fmt.Sprintf("((Func<%s>)(() => { %s}))()", retType, body.String())
	return &RawExpr{Code: code, Type: retType}, nil
}

func compileQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}

	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Where == nil &&
		q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		j := q.Joins[0]
		if j.Side != nil && *j.Side == "outer" {
			leftSrc, err := compileExpr(q.Source)
			if err != nil {
				return nil, err
			}
			rightSrc, err := compileExpr(j.Src)
			if err != nil {
				return nil, err
			}
			leftType := strings.TrimSuffix(typeOfExpr(leftSrc), "[]")
			rightType := strings.TrimSuffix(typeOfExpr(rightSrc), "[]")
			sv1, sv2 := varTypes[q.Var], varTypes[j.Var]
			sm1, sm2 := mapVars[q.Var], mapVars[j.Var]
			varTypes[q.Var] = leftType
			varTypes[j.Var] = rightType
			if strings.HasPrefix(leftType, "Dictionary<") {
				mapVars[q.Var] = true
			}
			if strings.HasPrefix(rightType, "Dictionary<") {
				mapVars[j.Var] = true
			}
			cond, err := compileExpr(j.On)
			if err != nil {
				varTypes[q.Var] = sv1
				varTypes[j.Var] = sv2
				mapVars[q.Var] = sm1
				mapVars[j.Var] = sm2
				return nil, err
			}
			sel, err := compileExpr(q.Select)
			if err != nil {
				varTypes[q.Var] = sv1
				varTypes[j.Var] = sv2
				mapVars[q.Var] = sm1
				mapVars[j.Var] = sm2
				return nil, err
			}
			elem := typeOfExpr(sel)
			code := fmt.Sprintf("(() => { var _res = new List<%s>(); ", elem)
			code += fmt.Sprintf("foreach (var %s in %s) { bool _matched = false; ", q.Var, exprString(leftSrc))
			code += fmt.Sprintf("foreach (var %s in %s) { if (!(%s)) continue; _matched = true; _res.Add(%s); } ", j.Var, exprString(rightSrc), exprString(cond), exprString(sel))
			code += fmt.Sprintf("if (!_matched) { var %s = default(%s); _res.Add(%s); } } ", j.Var, rightType, exprString(sel))
			code += fmt.Sprintf("foreach (var %s in %s) { bool _exists = false; ", j.Var, exprString(rightSrc))
			code += fmt.Sprintf("foreach (var %s in %s) { if (%s) { _exists = true; break; } } ", q.Var, exprString(leftSrc), exprString(cond))
			code += fmt.Sprintf("if (!_exists) { var %s = default(%s); _res.Add(%s); } } return _res.ToArray(); })()", q.Var, leftType, exprString(sel))
			usesDict = true
			varTypes[q.Var] = sv1
			varTypes[j.Var] = sv2
			mapVars[q.Var] = sm1
			mapVars[j.Var] = sm2
			return &RawExpr{Code: code, Type: fmt.Sprintf("%s[]", elem)}, nil
		}
	}

	src, err := compileExpr(q.Source)
	if err != nil {
		return nil, err
	}

	srcExpr := exprString(src)
	srcType := typeOfExpr(src)
	elemType := strings.TrimSuffix(srcType, "[]")
	if st, ok := structTypes[srcType]; ok {
		if it, ok2 := st.Fields["items"]; ok2 {
			srcExpr += ".items"
			if lt, ok3 := it.(types.ListType); ok3 {
				elemType = csTypeFromType(lt.Elem)
			} else {
				elemType = strings.TrimSuffix(csTypeFromType(it), "[]")
			}
		}
	}

	builder := &strings.Builder{}
	curVar := q.Var
	fmt.Fprintf(builder, "from %s in %s", curVar, srcExpr)

	savedVar := varTypes[curVar]
	savedMap := mapVars[curVar]
	varTypes[curVar] = elemType
	if strings.HasPrefix(elemType, "Dictionary<") {
		mapVars[curVar] = true
	}

	for _, f := range q.Froms {
		s, err := compileExpr(f.Src)
		if err != nil {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, err
		}
		fmt.Fprintf(builder, " from %s in %s", f.Var, exprString(s))
		vt := strings.TrimSuffix(typeOfExpr(s), "[]")
		if vt != "" {
			varTypes[f.Var] = vt
			if strings.HasPrefix(vt, "Dictionary<") {
				mapVars[f.Var] = true
			}
		}
	}

	for idx, j := range q.Joins {
		js, err := compileExpr(j.Src)
		if err != nil {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, err
		}
		cond, err := compileExpr(j.On)
		if err != nil {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, err
		}
		cmp, ok := cond.(*CmpExpr)
		if !ok || cmp.Op != "==" {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, fmt.Errorf("unsupported join condition")
		}
		leftExpr := cmp.Left
		rightExpr := cmp.Right
		if usesVar(leftExpr, j.Var) && !usesVar(rightExpr, j.Var) {
			leftExpr, rightExpr = rightExpr, leftExpr
		}
		left := exprString(leftExpr)
		right := exprString(rightExpr)
		if j.Side == nil {
			fmt.Fprintf(builder, " join %s in %s on %s equals %s", j.Var, exprString(js), left, right)
		} else if *j.Side == "left" {
			tmp := j.Var + "Tmp"
			fmt.Fprintf(builder, " join %s in %s on %s equals %s into %s", j.Var, exprString(js), left, right, tmp)
			fmt.Fprintf(builder, " from %s in %s.DefaultIfEmpty()", j.Var, tmp)
		} else if *j.Side == "right" {
			if idx == 0 && len(q.Froms) == 0 {
				// rewrite as left join starting from the right-hand sequence
				builder.Reset()
				fmt.Fprintf(builder, "from %s in %s", j.Var, exprString(js))
				tmp := curVar + "Tmp"
				left := exprString(cmp.Right)
				right := exprString(cmp.Left)
				fmt.Fprintf(builder, " join %s in %s on %s equals %s into %s", curVar, srcExpr, left, right, tmp)
				fmt.Fprintf(builder, " from %s in %s.DefaultIfEmpty()", curVar, tmp)
				varTypes[j.Var] = strings.TrimSuffix(typeOfExpr(js), "[]")
				if strings.HasPrefix(varTypes[j.Var], "Dictionary<") {
					mapVars[j.Var] = true
				}
				curVar = j.Var
			} else {
				tmp := j.Var + "Tmp"
				fmt.Fprintf(builder, " join %s in %s on %s equals %s into %s", curVar, exprString(js), right, left, tmp)
				fmt.Fprintf(builder, " from %s in %s.DefaultIfEmpty()", curVar, tmp)
				curVar = j.Var
			}
		} else {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, fmt.Errorf("unsupported join side")
		}
		varTypes[j.Var] = strings.TrimSuffix(typeOfExpr(js), "[]")
		if strings.HasPrefix(varTypes[j.Var], "Dictionary<") {
			mapVars[j.Var] = true
		}
	}

	if q.Where != nil {
		cond, err := compileExpr(q.Where)
		if err != nil {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, err
		}
		fmt.Fprintf(builder, " where %s", exprString(cond))
	}

	if q.Group != nil {
		if len(q.Group.Exprs) != 1 {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, fmt.Errorf("unsupported group")
		}
		keyExpr, err := compileExpr(q.Group.Exprs[0])
		if err != nil {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, err
		}
		if mp, ok := keyExpr.(*MapLit); ok {
			if res, changed := inferStructMap(q.Group.Name+"Key", currentProg, mp); changed {
				keyExpr = res
			}
		}
		elemType := varTypes[curVar]
		groupExprStr := curVar
		if len(q.Joins) > 0 {
			jStruct := toStructName(q.Group.Name + "Row")
			names := []string{curVar}
			for _, j := range q.Joins {
				names = append(names, j.Var)
			}
			fields := make([]StructFieldValue, len(names))
			typeFields := make(map[string]types.Type)
			sf := make([]StructField, len(names))
			for i, n := range names {
				fields[i] = StructFieldValue{Name: n, Value: &VarRef{Name: n}}
				if t, ok := varTypes[n]; ok {
					typeFields[n] = simpleType(t)
					sf[i] = StructField{Name: n, Type: t}
				} else {
					typeFields[n] = types.AnyType{}
					sf[i] = StructField{Name: n, Type: "object"}
				}
			}
			structTypes[jStruct] = types.StructType{Name: jStruct, Fields: typeFields, Order: names}
			if currentProg != nil {
				currentProg.Structs = append(currentProg.Structs, StructDecl{Name: jStruct, Fields: sf})
			}
			joinLit := &StructLit{Name: jStruct, Fields: fields}
			groupExprStr = exprString(joinLit)
			elemType = jStruct
		}
		tmp := q.Group.Name + "Tmp"
		fmt.Fprintf(builder, " group %s by %s into %s", groupExprStr, exprString(keyExpr), tmp)
		gStruct := toStructName(q.Group.Name) + "Group"
		keyT := simpleType(typeOfExpr(keyExpr))
		if sl, ok := keyExpr.(*StructLit); ok {
			if st, ok2 := structTypes[sl.Name]; ok2 {
				keyT = st
			}
		}
		structTypes[gStruct] = types.StructType{Name: gStruct, Fields: map[string]types.Type{
			"key":   keyT,
			"items": types.ListType{Elem: simpleType(elemType)},
		}, Order: []string{"key", "items"}}
		if currentProg != nil {
			itemType := elemType
			if strings.HasSuffix(itemType, "[]") {
				itemType = strings.TrimSuffix(itemType, "[]")
			}
			fields := []StructField{
				{Name: "key", Type: typeOfExpr(keyExpr)},
				{Name: "items", Type: itemType + "[]"},
			}
			currentProg.Structs = append(currentProg.Structs, StructDecl{Name: gStruct, Fields: fields})
		}
		fmt.Fprintf(builder, " let %s = new %s{ key = %s.Key, items = %s.ToArray() }", q.Group.Name, gStruct, tmp, tmp)
		varTypes[q.Group.Name] = gStruct
		if q.Group.Having != nil {
			cond, err := compileExpr(q.Group.Having)
			if err != nil {
				varTypes[curVar] = savedVar
				mapVars[curVar] = savedMap
				return nil, err
			}
			fmt.Fprintf(builder, " where %s", exprString(cond))
		}
		// after grouping, use new variable
		savedVar = varTypes[q.Group.Name]
		curVar = q.Group.Name
	}

	if q.Sort != nil {
		sortExpr, err := compileExpr(q.Sort)
		if err != nil {
			varTypes[curVar] = savedVar
			mapVars[curVar] = savedMap
			return nil, err
		}
		if mp, ok := sortExpr.(*MapLit); ok {
			elems := make([]string, len(mp.Items))
			for i, it := range mp.Items {
				elems[i] = exprString(it.Value)
			}
			sortExpr = &RawExpr{Code: fmt.Sprintf("Tuple.Create(%s)", strings.Join(elems, ", "))}
		}
		desc := false
		if ue, ok := sortExpr.(*UnaryExpr); ok && ue.Op == "-" {
			sortExpr = ue.Val
			desc = true
		}
		fmt.Fprintf(builder, " orderby %s", exprString(sortExpr))
		if desc {
			fmt.Fprintf(builder, " descending")
		}
	}

	sel, err := compileExpr(q.Select)
	if err != nil {
		varTypes[curVar] = savedVar
		mapVars[curVar] = savedMap
		return nil, err
	}
	if mp, ok := sel.(*MapLit); ok {
		if res, changed := inferStructMap(curVar+"Result", currentProg, mp); changed {
			sel = res
		}
	}

	fmt.Fprintf(builder, " select %s", exprString(sel))

	query := builder.String()

	switch agg := sel.(type) {
	case *SumExpr:
		arg := exprString(agg.Arg)
		base := strings.TrimSuffix(query, " select "+exprString(sel)) + " select " + arg
		if arg == curVar {
			query = fmt.Sprintf("(%s).Sum()", base)
		} else {
			query = fmt.Sprintf("(%s).Sum(%s => %s)", base, curVar, arg)
		}
		usesLinq = true
		typ := "double"
		if typeOfExpr(agg.Arg) == "int" {
			typ = "int"
		}
		varTypes[curVar] = savedVar
		mapVars[curVar] = savedMap
		return &RawExpr{Code: query, Type: typ}, nil
	case *CountExpr:
		base := strings.TrimSuffix(query, " select "+exprString(sel))
		query = fmt.Sprintf("(%s).Count()", base)
		usesLinq = true
		varTypes[curVar] = savedVar
		mapVars[curVar] = savedMap
		return &RawExpr{Code: query, Type: "int"}, nil
	}

	if q.Skip != nil {
		s, err := compileExpr(q.Skip)
		if err != nil {
			return nil, err
		}
		query = fmt.Sprintf("(%s).Skip(%s)", query, exprString(s))
	}
	if q.Take != nil {
		t, err := compileExpr(q.Take)
		if err != nil {
			return nil, err
		}
		query = fmt.Sprintf("(%s).Take(%s)", query, exprString(t))
	}

	query = fmt.Sprintf("(%s).ToArray()", query)

	usesLinq = true
	elemType = typeOfExpr(sel)
	varTypes[curVar] = savedVar
	mapVars[curVar] = savedMap
	return &RawExpr{Code: query, Type: fmt.Sprintf("%s[]", elemType)}, nil
}

// Emit generates formatted C# source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	ts := gitTimestamp()
	fmt.Fprintf(&buf, "// Generated by Mochi %s on %s\n", strings.TrimSpace(version), ts)
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
	if usesInput {
		buf.WriteString("using System.IO;\n")
	}
	if usesFmt {
		buf.WriteString("using System.Collections;\n")
		if !usesDict {
			buf.WriteString("using System.Collections.Generic;\n")
		}
	}
	for _, imp := range prog.Imports {
		buf.WriteString(imp)
		if !strings.HasSuffix(imp, "\n") {
			buf.WriteString("\n")
		}
	}
	buf.WriteString("\n")

	for _, st := range prog.Structs {
		fmt.Fprintf(&buf, "struct %s {\n", st.Name)
		for _, f := range st.Fields {
			fmt.Fprintf(&buf, "    public %s %s;\n", f.Type, f.Name)
		}
		fmt.Fprintf(&buf, "    public override string ToString() => $\"%s {{", st.Name)
		for i, f := range st.Fields {
			if i > 0 {
				buf.WriteString(", ")
			}
			fmt.Fprintf(&buf, "%s = ", f.Name)
			if f.Type == "string" {
				fmt.Fprintf(&buf, "\\\"{%s}\\\"", f.Name)
			} else if f.Type == "double" {
				fmt.Fprintf(&buf, "{%s.ToString(\"0.0\")}", f.Name)
			} else {
				fmt.Fprintf(&buf, "{%s}", f.Name)
			}
		}
		buf.WriteString("}}\";\n")
		buf.WriteString("}\n")
	}

	buf.WriteString("class Program {\n")
	if usesNow {
		buf.WriteString("\tstatic bool seededNow = false;\n")
		buf.WriteString("\tstatic long nowSeed = 0;\n")
		buf.WriteString("\tstatic int _now() {\n")
		buf.WriteString("\t\tif (!seededNow) {\n")
		buf.WriteString("\t\t\tvar s = Environment.GetEnvironmentVariable(\"MOCHI_NOW_SEED\");\n")
		buf.WriteString("\t\t\tif (long.TryParse(s, out var v)) {\n")
		buf.WriteString("\t\t\t\tnowSeed = v;\n")
		buf.WriteString("\t\t\t\tseededNow = true;\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\tif (seededNow) {\n")
		buf.WriteString("\t\t\tnowSeed = (nowSeed*1664525 + 1013904223) % 2147483647;\n")
		buf.WriteString("\t\t\treturn (int)nowSeed;\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\treturn (int)(DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() % int.MaxValue);\n")
		buf.WriteString("\t}\n")
	}
	if usesInput {
		buf.WriteString("\tstatic string[] inputLines;\n")
		buf.WriteString("\tstatic int inputIndex = 0;\n")
		buf.WriteString("\tstatic string _input() {\n")
		buf.WriteString("\t\tif (inputLines == null) {\n")
		buf.WriteString("\t\t\tvar path = Environment.GetEnvironmentVariable(\"MOCHI_INPUT_FILE\");\n")
		buf.WriteString("\t\t\tif (!string.IsNullOrEmpty(path) && File.Exists(path)) {\n")
		buf.WriteString("\t\t\t\tinputLines = File.ReadAllLines(path);\n")
		buf.WriteString("\t\t\t} else {\n")
		buf.WriteString("\t\t\t\tinputLines = new string[]{};\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\tif (inputIndex < inputLines.Length) {\n")
		buf.WriteString("\t\t\treturn inputLines[inputIndex++];\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\treturn Console.ReadLine();\n")
		buf.WriteString("\t}\n")
	}
	if usesFmt {
		buf.WriteString(`static string _fmt(object v) {
if (v is Array a) {
var parts = new List<string>();
foreach (var x in a) parts.Add(_fmt(x));
return "[" + string.Join(" ", parts) + "]";
}
if (v is System.Collections.IDictionary d) {
var keys = new List<string>();
foreach (var k in d.Keys) keys.Add(k.ToString());
keys.Sort();
var parts = new List<string>();
foreach (var k in keys) parts.Add(k + ":" + _fmt(d[k]));
return "map[" + string.Join(" ", parts) + "]";
}
if (v is System.Collections.IEnumerable e && !(v is string)) {
var parts = new List<string>();
foreach (var x in e) parts.Add(_fmt(x));
return string.Join(" ", parts);
}
if (v is bool b) return b ? "1" : "0";
return Convert.ToString(v);
}
`)
		buf.WriteString(`static string _fmtTop(object v) {
if (v is Array a && a.Length > 0 && a.GetValue(0) is Array) {
    var parts = new List<string>();
    foreach (var x in a) parts.Add(_fmt(x));
    return string.Join(" ", parts);
}
return _fmt(v);
}
`)
	}
	for _, g := range prog.Globals {
		buf.WriteString("\tstatic ")
		t := typeOfExpr(g.Value)
		if vt, ok := varTypes[g.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
			if t == "" || t == "object" {
				t = vt
			}
		}
		if t == "" || t == "object" {
			fmt.Fprintf(&buf, "dynamic %s = ", g.Name)
		} else {
			fmt.Fprintf(&buf, "%s %s = ", t, g.Name)
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
	s := strings.ReplaceAll(string(src), "\t", "    ")
	lines := strings.Split(s, "\n")
	indent := 0
	for i, ln := range lines {
		trimmed := strings.TrimSpace(ln)
		if strings.HasPrefix(trimmed, "}") {
			if indent > 0 {
				indent--
			}
		}
		if trimmed != "" {
			lines[i] = strings.Repeat("    ", indent) + strings.TrimRight(trimmed, " ")
		} else {
			lines[i] = ""
		}
		if strings.HasSuffix(trimmed, "{") {
			indent++
		}
	}
	out := strings.Join(lines, "\n")
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}

func fieldNames(fs []StructField) []string {
	names := make([]string, len(fs))
	for i, f := range fs {
		names[i] = f.Name
	}
	return names
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

func simpleIdent(e *parser.Expr) (string, bool) {
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

// structPattern checks if expression e is a constructor call of a known struct
// and returns the struct name and argument identifiers.
func structPattern(e *parser.Expr) (string, []string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	st, ok := structTypes[call.Func]
	if !ok || len(call.Args) != len(st.Order) {
		return "", nil, false
	}
	vars := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, ok := simpleIdent(a)
		if !ok {
			return "", nil, false
		}
		vars[i] = v
	}
	return call.Func, vars, true
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return ""
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return ""
	}
	for _, it := range p.Target.Map.Items {
		key, ok := simpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func repoRoot() string {
	dir, _ := os.Getwd()
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

func dataExprFromFile(path, format string, typ *parser.TypeRef) (Expr, error) {
	if path == "" {
		return &ListLit{}, nil
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
	case "yaml":
		if err := yaml.Unmarshal(data, &v); err != nil {
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
		return nil, fmt.Errorf("unsupported format")
	}
	return valueToExpr(v, typ), nil
}

func valueToExpr(v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		if typ != nil && typ.Simple != nil {
			if st, ok := structTypes[*typ.Simple]; ok {
				fields := make([]StructFieldValue, len(st.Order))
				for i, k := range st.Order {
					ft := parserTypeRefFromType(st.Fields[k])
					fields[i] = StructFieldValue{Name: k, Value: valueToExpr(val[k], ft)}
				}
				return &StructLit{Name: st.Name, Fields: fields}
			}
		}
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		items := make([]MapItem, len(names))
		for i, k := range names {
			items[i] = MapItem{Key: &StringLit{Value: k}, Value: valueToExpr(val[k], nil)}
		}
		return &MapLit{Items: items}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it, typ)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case float64:
		if typ != nil && typ.Simple != nil && *typ.Simple == "int" {
			return &IntLit{Value: int(val)}
		}
		return &FloatLit{Value: val}
	case int, int64:
		return &IntLit{Value: int(reflect.ValueOf(val).Int())}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func parserTypeRefFromType(t types.Type) *parser.TypeRef {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		s := "int"
		return &parser.TypeRef{Simple: &s}
	case types.FloatType:
		s := "double"
		return &parser.TypeRef{Simple: &s}
	case types.BoolType:
		s := "bool"
		return &parser.TypeRef{Simple: &s}
	case types.StringType:
		s := "string"
		return &parser.TypeRef{Simple: &s}
	case types.StructType:
		if tt.Name != "" {
			s := tt.Name
			return &parser.TypeRef{Simple: &s}
		}
	}
	return nil
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

// print converts the custom AST to an ast.Node and prints it.

func cloneStringMap(src map[string]string) map[string]string {
	dst := make(map[string]string)
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func cloneBoolMap(src map[string]bool) map[string]bool {
	dst := make(map[string]bool)
	for k, v := range src {
		dst[k] = v
	}
	return dst
}
