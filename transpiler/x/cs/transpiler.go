//go:build slow

package cstranspiler

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"

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
var globalDecls map[string]*Global
var mutatedVars map[string]bool
var blockDepth int

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
	if len(m.Items) == 0 {
		return m, false
	}
	keys := make([]string, len(m.Items))
	typesMap := make(map[string]string)
	vals := make([]StructFieldValue, len(m.Items))
	for i, it := range m.Items {
		k, ok := it.Key.(*StringLit)
		if !ok {
			return m, false
		}
		keys[i] = k.Value
		typ := typeOfExpr(it.Value)
		if se, ok := it.Value.(*SumExpr); ok {
			t := typeOfExpr(se.Arg)
			if strings.HasSuffix(t, "[]") {
				t = strings.TrimSuffix(t, "[]")
			}
			if t == "double" {
				typ = "double"
			}
		}
		typesMap[k.Value] = typ
		vals[i] = StructFieldValue{Name: k.Value, Value: it.Value}
	}

	sname := toStructName(varName)
	if _, exists := structTypes[sname]; exists {
		return m, false
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
	varTypes[varName] = sname
	delete(mapVars, varName)
	return &StructLit{Name: sname, Fields: vals}, true
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
		return "double"
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
		}
	case *RawExpr:
		return ex.Type
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
	t := listType(l)
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
	if strings.HasSuffix(typeOfExpr(s.Arg), "[]") {
		fmt.Fprint(w, "(")
		s.Arg.emit(w)
		fmt.Fprint(w, ".Sum())")
	} else {
		s.Arg.emit(w)
	}
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
	globalDecls = make(map[string]*Global)
	mutatedVars = make(map[string]bool)
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
		if list, ok := val.(*ListLit); ok {
			if res, changed := inferStructList(s.Let.Name, prog, list); changed {
				val = res
			}
		}
		if mp, ok := val.(*MapLit); ok {
			if len(mp.Items) > 2 {
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
			varTypes[s.Let.Name] = t
		}
		if prog != nil && blockDepth == 0 {
			for m := range mutatedVars {
				if exprUsesVar(val, m) {
					return &LetStmt{Name: s.Let.Name, Value: val}, nil
				}
			}
			g := &Global{Name: s.Let.Name, Value: val}
			prog.Globals = append(prog.Globals, g)
			globalDecls[s.Let.Name] = g
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
		if list, ok := val.(*ListLit); ok {
			_ = list // do not convert mutable vars to structs
		}
		if mp, ok := val.(*MapLit); ok {
			_ = mp // keep as dictionary for mutable vars
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
		if prog != nil && blockDepth == 0 {
			g := &Global{Name: s.Var.Name, Value: val}
			prog.Globals = append(prog.Globals, g)
			globalDecls[s.Var.Name] = g
			return nil, nil
		}
		return &VarStmt{Name: s.Var.Name, Value: val}, nil
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
				if vt, ok2 := varTypes[s.Assign.Name]; ok2 && (vt == "" || vt == "object[]") {
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
						varTypes[s.Assign.Name] = t + "[]"
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
		if len(s.Assign.Field) > 0 && len(s.Assign.Index) == 0 {
			var target Expr = &VarRef{Name: s.Assign.Name}
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
		for i, p := range s.Fun.Params {
			params[i] = p.Name
			ptypes[i] = csType(p.Type)
		}
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
		retType := csType(s.Fun.Return)
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
		return &ForInStmt{Var: s.For.Name, Iterable: iterable, Body: body}, nil
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
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
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := compileIfStmt(prog, i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
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
					return &CallExpr{Func: name, Args: []Expr{arg}}, nil
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
				if strings.HasSuffix(typeOfExpr(arg), "[]") {
					join := &CallExpr{Func: "string.Join", Args: []Expr{&StringLit{Value: ", "}, arg}}
					wrapped := &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: &BinaryExpr{Left: join, Op: "+", Right: &StringLit{Value: "]"}}}
					return &CallExpr{Func: name, Args: []Expr{wrapped}}, nil
				}
				if typeOfExpr(arg) == "double" {
					formatted := &MethodCallExpr{Target: arg, Name: "ToString", Args: []Expr{&StringLit{Value: "0.0"}}}
					return &CallExpr{Func: name, Args: []Expr{formatted}}, nil
				}
				return &CallExpr{Func: name, Args: []Expr{arg}}, nil
			}
			elems := make([]Expr, len(args))
			for i, a := range args {
				elems[i] = a
			}
			list := &ListLit{Elems: elems}
			join := &CallExpr{Func: "string.Join", Args: []Expr{&StringLit{Value: ", "}, list}}
			wrapped := &BinaryExpr{Left: &StringLit{Value: "["}, Op: "+", Right: &BinaryExpr{Left: join, Op: "+", Right: &StringLit{Value: "]"}}}
			return &CallExpr{Func: name, Args: []Expr{wrapped}}, nil
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
	case p.Match != nil:
		return compileMatchExpr(p.Match)
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
		patExpr, err := compileExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if i == len(me.Cases)-1 {
			expr = res
			if vr, ok := patExpr.(*VarRef); ok && vr.Name == "_" {
				continue
			}
		}
		if vr, ok := patExpr.(*VarRef); ok && vr.Name == "_" {
			expr = res
			continue
		}
		cond := &CmpExpr{Op: "==", Left: target, Right: patExpr}
		expr = &IfExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
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
		base := strings.TrimSuffix(query, " select "+exprString(sel))
		if arg == curVar {
			query = fmt.Sprintf("(%s).Sum()", base)
		} else {
			query = fmt.Sprintf("(%s).Sum(%s => %s)", base, curVar, arg)
		}
		usesLinq = true
		typ := "double"
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
	buf.WriteString("\n")

	for _, st := range prog.Structs {
		fmt.Fprintf(&buf, "struct %s {\n", st.Name)
		for _, f := range st.Fields {
			fmt.Fprintf(&buf, "    public %s %s;\n", f.Type, f.Name)
		}
		buf.WriteString("    public override string ToString() => $\"{{")
		for i, f := range st.Fields {
			if i > 0 {
				buf.WriteString(", ")
			}
			fmt.Fprintf(&buf, "'%s': ", f.Name)
			if f.Type == "string" {
				fmt.Fprintf(&buf, "'{%s}'", f.Name)
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
	for _, g := range prog.Globals {
		buf.WriteString("\tstatic ")
		t := typeOfExpr(g.Value)
		if t == "" {
			if vt, ok := varTypes[g.Name]; ok && vt != "" {
				t = vt
			}
		}
		if t != "" && t != "object" {
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
	s := strings.ReplaceAll(string(src), "\t", "    ")
	lines := strings.Split(s, "\n")
	for i, ln := range lines {
		lines[i] = strings.TrimRight(ln, " ")
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
