//go:build slow

package cstranspiler

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"regexp"
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
	Imports   []string
	Structs   []StructDecl
	Globals   []*Global
	Funcs     []*Function
	Stmts     []Stmt
	BenchMain bool
}

var benchMain bool

// SetBenchMain configures whether the generated program's main body
// is wrapped in a benchmark block when emitting code. When enabled the
// program prints a JSON object with duration and memory usage on exit.
func SetBenchMain(v bool) { benchMain = v }

type Global struct {
	Name  string
	Value Expr
}

type StructDecl struct {
	Name    string
	Fields  []StructField
	Methods []*Function
}

type StructField struct {
	Name string
	Type string
}

var stringVars map[string]bool
var mapVars map[string]bool
var varTypes map[string]string
var finalVarTypes map[string]string
var structTypes map[string]types.StructType
var funRets map[string]string
var funParams map[string][]string
var userFuncs map[string]bool
var usesDict bool
var usesLinq bool
var usesJson bool
var usesNow bool
var usesSHA256 bool
var usesInput bool
var usesMem bool
var usesBigInt bool
var usesBigRat bool
var usesFetch bool
var fetchStructs map[string]bool
var currentAssign string
var currentReturnType string
var currentReturnVoid bool
var globalDecls map[string]*Global
var mutatedVars map[string]bool
var blockDepth int
var importAliases map[string]string
var varAliases map[string]string
var aliasCounter int
var usesFmt bool
var usesRepeat bool
var usesSubstr bool
var usesSlice bool
var usesLen bool
var usesMod bool
var usesAtoi bool
var usesIdx bool
var envTypes map[string]types.Type

func isNullExpr(e Expr) bool {
	if r, ok := e.(*RawExpr); ok {
		return strings.TrimSpace(r.Code) == "null"
	}
	return false
}

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
	case *ForRangeStmt, *ForInStmt, *WhileStmt, *IfStmt, *BenchStmt:
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
	if vt, ok := varTypes[s.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
		t = vt
	} else if vt, ok := finalVarTypes[s.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
		t = vt
	}
	if fl, ok := s.Value.(*FunLit); ok {
		if t == "" || t == "object" {
			t = typeOfExpr(fl)
		}
		if t == "" {
			t = "Action"
		}
		fmt.Fprintf(w, "%s %s = null;\n", t, name)
		fmt.Fprintf(w, "%s = ", name)
		fl.emit(w)
		return
	}
	if t != "" && t != "object" {
		fmt.Fprintf(w, "%s %s = ", t, name)
	} else if t == "object" {
		fmt.Fprintf(w, "object %s = ", name)
	} else if isNullExpr(s.Value) {
		fmt.Fprintf(w, "object %s = ", name)
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
	if vt, ok := varTypes[s.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
		t = vt
	} else if vt, ok := finalVarTypes[s.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
		t = vt
	}
	if t != "" && t != "object" {
		fmt.Fprintf(w, "%s %s", t, name)
	} else if t == "object" {
		fmt.Fprintf(w, "object %s", name)
	} else if isNullExpr(s.Value) {
		fmt.Fprintf(w, "object %s", name)
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
	ltype := finalVarTypes[s.Name]
	rtype := typeOfExpr(s.Value)
	if ltype != "" && rtype == "object" && ltype != "object" {
		fmt.Fprintf(w, "(%s)(", ltype)
		s.Value.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	s.Value.emit(w)
}

// AssignIndexStmt represents assignment to an indexed element like xs[i] = v.
type AssignIndexStmt struct {
	Target Expr
	Index  Expr
	Value  Expr
}

func (s *AssignIndexStmt) emit(w io.Writer) {
	if lit, ok := s.Index.(*StringLit); ok {
		if t := typeOfExpr(s.Target); t != "" {
			if st, ok := structTypes[t]; ok {
				if _, ok := st.Fields[lit.Value]; ok {
					s.Target.emit(w)
					fmt.Fprintf(w, ".%s = ", safeName(lit.Value))
					s.Value.emit(w)
					return
				}
			}
		}
	}
	targetType := typeOfExpr(s.Target)
	idxType := typeOfExpr(s.Index)
	s.Target.emit(w)
	fmt.Fprint(w, "[")
	if strings.HasSuffix(targetType, "[]") && idxType != "int" {
		fmt.Fprint(w, "(int)(")
		s.Index.emit(w)
		fmt.Fprint(w, ")")
	} else {
		s.Index.emit(w)
	}
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
	fmt.Fprintf(w, ".%s = ", safeName(a.Name))
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

// BenchStmt represents a benchmark block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(w io.Writer) {
	fmt.Fprint(w, "{\n")
	fmt.Fprint(w, "    var __memStart = _mem();\n")
	fmt.Fprint(w, "    var __start = _now();\n")
	for _, st := range b.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		if isBlockStmt(st) {
			fmt.Fprint(w, "\n")
		} else {
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprint(w, "    var __end = _now();\n")
	fmt.Fprint(w, "    var __memEnd = _mem();\n")
	fmt.Fprint(w, "    var __dur = (__end - __start);\n")
	fmt.Fprint(w, "    if (__dur <= 0) __dur = 1;\n")
	fmt.Fprint(w, "    var __memDiff = __memEnd - __memStart;\n")
	fmt.Fprint(w, "    if (__memDiff <= 0) __memDiff = __memEnd;\n")
	fmt.Fprintf(w, "    Console.WriteLine(\"{\\\"name\\\":\\\"%s\\\",\\\"duration_us\\\":\" + __dur + \",\\\"memory_bytes\\\":\" + __memDiff + \"}\");\n", b.Name)
	fmt.Fprint(w, "}")
}

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
	if typeOfExpr(f.Iterable) == "string" {
		idx := fmt.Sprintf("_i%d", aliasCounter)
		aliasCounter++
		fmt.Fprintf(w, "for (var %s = 0; %s < ", idx, idx)
		f.Iterable.emit(w)
		fmt.Fprintf(w, ".Length; %s++) {\n", idx)
		fmt.Fprintf(w, "    var %s = ", v)
		f.Iterable.emit(w)
		fmt.Fprintf(w, ".Substring((int)(%s), 1);\n", idx)
	} else {
		elem := "var"
		t := typeOfExpr(f.Iterable)
		if strings.HasSuffix(t, "[]") {
			elem = strings.TrimSuffix(t, "[]")
		} else if isMapExpr(f.Iterable) && strings.HasPrefix(t, "Dictionary<") {
			inside := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			parts := strings.Split(inside, ",")
			if len(parts) == 2 {
				elem = strings.TrimSpace(parts[0])
			}
		}
		fmt.Fprintf(w, "foreach (%s %s in ", elem, v)
		if isMapExpr(f.Iterable) {
			f.Iterable.emit(w)
			fmt.Fprint(w, ".Keys")
		} else {
			f.Iterable.emit(w)
		}
		fmt.Fprint(w, ") {\n")
	}
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
	Receiver   string
}

func (f *Function) emit(w io.Writer) {
	ret := f.ReturnType
	if ret == "" {
		ret = "void"
	}
	saved := make(map[string]string)
	if f.Receiver != "" {
		fmt.Fprintf(w, "public %s %s(", ret, safeName(f.Name))
	} else {
		fmt.Fprintf(w, "public static %s %s(", ret, safeName(f.Name))
	}
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
	needRet := ret != "void"
	if len(f.Body) > 0 {
		if _, ok := f.Body[len(f.Body)-1].(*ReturnStmt); ok {
			needRet = false
		}
	}
	if needRet {
		fmt.Fprintf(w, "    return default(%s);\n", ret)
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
	if len(i.Else) == 1 {
		if nested, ok := i.Else[0].(*IfStmt); ok {
			fmt.Fprint(w, " else ")
			nested.emit(w)
			return
		}
	}
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
		if b.Op == "+" && (isListExpr(b.Left) && isListExpr(b.Right) ||
			(strings.HasSuffix(typeOfExpr(b.Left), "[]") && strings.HasSuffix(typeOfExpr(b.Right), "[]"))) {
			usesLinq = true
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprint(w, ".Concat(")
			b.Right.emit(w)
			fmt.Fprint(w, ").ToArray())")
		} else {
			lt := typeOfExpr(b.Left)
			rt := typeOfExpr(b.Right)
			if b.Op == "%" {
				usesMod = true
				lt := typeOfExpr(b.Left)
				rt := typeOfExpr(b.Right)
				if lt == "double" || rt == "double" {
					fmt.Fprint(w, "(")
					b.Left.emit(w)
					fmt.Fprint(w, " % ")
					b.Right.emit(w)
					fmt.Fprint(w, ")")
				} else {
					fmt.Fprint(w, "_mod(")
					b.Left.emit(w)
					fmt.Fprint(w, ", ")
					b.Right.emit(w)
					fmt.Fprint(w, ")")
				}
				return
			}
			if b.Op == "+" && lt == "string" && strings.HasSuffix(rt, "[]") {
				fmt.Fprint(w, "(")
				b.Left.emit(w)
				fmt.Fprint(w, " + string.Concat(")
				b.Right.emit(w)
				fmt.Fprint(w, "))")
				return
			}
			if b.Op == "+" && rt == "string" && strings.HasSuffix(lt, "[]") {
				fmt.Fprint(w, "(")
				fmt.Fprint(w, "string.Concat(")
				b.Left.emit(w)
				fmt.Fprint(w, ") + ")
				b.Right.emit(w)
				fmt.Fprint(w, ")")
				return
			}
			if lt == "BigRat" || rt == "BigRat" {
				usesBigRat = true
				switch b.Op {
				case "+":
					fmt.Fprint(w, "_add(")
					b.Left.emit(w)
					fmt.Fprint(w, ", ")
					b.Right.emit(w)
					fmt.Fprint(w, ")")
				case "-":
					fmt.Fprint(w, "_sub(")
					b.Left.emit(w)
					fmt.Fprint(w, ", ")
					b.Right.emit(w)
					fmt.Fprint(w, ")")
				case "*":
					fmt.Fprint(w, "_mul(")
					b.Left.emit(w)
					fmt.Fprint(w, ", ")
					b.Right.emit(w)
					fmt.Fprint(w, ")")
				case "/":
					fmt.Fprint(w, "_div(")
					b.Left.emit(w)
					fmt.Fprint(w, ", ")
					b.Right.emit(w)
					fmt.Fprint(w, ")")
				default:
					fmt.Fprint(w, "0")
				}
				return
			}
			useDyn := lt == "object" || lt == "" || rt == "object" || rt == ""
			fmt.Fprint(w, "(")
			if useDyn {
				fmt.Fprint(w, "((dynamic)(")
				b.Left.emit(w)
				fmt.Fprint(w, "))")
			} else {
				b.Left.emit(w)
			}
			fmt.Fprintf(w, " %s ", b.Op)
			if useDyn {
				fmt.Fprint(w, "((dynamic)(")
				b.Right.emit(w)
				fmt.Fprint(w, "))")
			} else {
				b.Right.emit(w)
			}
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
	t := typeOfExpr(u.Val)
	if u.Op == "-" && t == "BigRat" {
		usesBigRat = true
		fmt.Fprint(w, "_neg(")
		u.Val.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	if (u.Op == "-" || u.Op == "+") && (t == "" || t == "object") {
		fmt.Fprintf(w, "%s((dynamic)(", u.Op)
		u.Val.emit(w)
		fmt.Fprint(w, "))")
		return
	}
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
	if (c.Op == "==" || c.Op == "!=") && (isListExpr(c.Left) || isListExpr(c.Right)) {
		elem := strings.TrimSuffix(typeOfExpr(c.Left), "[]")
		if elem == "" || elem == "object" {
			elem = strings.TrimSuffix(typeOfExpr(c.Right), "[]")
		}
		if elem == "" || elem == "object" {
			elem = "object"
		}
		if r, ok := c.Right.(*ListLit); ok && len(r.Elems) == 0 && (r.ElemType == "" || r.ElemType == "object[]") {
			r.ElemType = elem + "[]"
		}
		if l, ok := c.Left.(*ListLit); ok && len(l.Elems) == 0 && (l.ElemType == "" || l.ElemType == "object[]") {
			l.ElemType = elem + "[]"
		}
		if c.Op == "!=" {
			fmt.Fprint(w, "!")
		}
		fmt.Fprintf(w, "Enumerable.SequenceEqual<%s>(", elem)
		c.Left.emit(w)
		fmt.Fprint(w, ", ")
		c.Right.emit(w)
		fmt.Fprint(w, ")")
		fmt.Fprint(w, ")")
		return
	}
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
	if (c.Op == "<" || c.Op == "<=" || c.Op == ">" || c.Op == ">=") &&
		(isDynamicExpr(c.Left) || isDynamicExpr(c.Right)) {
		lt := typeOfExpr(c.Left)
		rt := typeOfExpr(c.Right)
		numeric := func(t string) bool { return t == "int" || t == "long" || t == "double" }
		if numeric(lt) || numeric(rt) {
			fmt.Fprint(w, "Convert.ToDouble(")
			c.Left.emit(w)
			fmt.Fprintf(w, ") %s Convert.ToDouble(", c.Op)
			c.Right.emit(w)
			fmt.Fprint(w, ")")
			fmt.Fprint(w, ")")
			return
		}
		fmt.Fprint(w, "string.Compare(Convert.ToString(")
		c.Left.emit(w)
		fmt.Fprint(w, "), Convert.ToString(")
		c.Right.emit(w)
		fmt.Fprint(w, "))")
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
	if (c.Op == "==" || c.Op == "!=") && ((isStringExpr(c.Left) && isBoolExpr(c.Right)) || (isBoolExpr(c.Left) && isStringExpr(c.Right))) {
		if isBoolExpr(c.Left) {
			fmt.Fprint(w, "Convert.ToString(")
			c.Left.emit(w)
			fmt.Fprint(w, ")")
		} else {
			c.Left.emit(w)
		}
		fmt.Fprintf(w, " %s ", c.Op)
		if isBoolExpr(c.Right) {
			fmt.Fprint(w, "Convert.ToString(")
			c.Right.emit(w)
			fmt.Fprint(w, ")")
		} else {
			c.Right.emit(w)
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
	} else if fe, ok := ie.Collection.(*FieldExpr); ok {
		t := typeOfExpr(fe.Target)
		if st, ok2 := structTypes[t]; ok2 {
			if _, ok3 := st.Fields[fe.Name].(types.MapType); ok3 {
				ie.Collection.emit(w)
				fmt.Fprint(w, ".ContainsKey(")
				ie.Item.emit(w)
				fmt.Fprint(w, ")")
				return
			}
		}
		t = typeOfExpr(ie.Collection)
		if strings.HasSuffix(t, "[]") {
			fmt.Fprint(w, "Array.IndexOf(")
			ie.Collection.emit(w)
			fmt.Fprint(w, ", ")
			ie.Item.emit(w)
			fmt.Fprint(w, ") >= 0")
		} else {
			ie.Collection.emit(w)
			fmt.Fprint(w, ".ContainsKey(")
			ie.Item.emit(w)
			fmt.Fprint(w, ")")
		}
	} else if _, ok := ie.Collection.(*VarRef); ok {
		t := typeOfExpr(ie.Collection)
		if strings.HasSuffix(t, "[]") {
			fmt.Fprint(w, "Array.IndexOf(")
			ie.Collection.emit(w)
			fmt.Fprint(w, ", ")
			ie.Item.emit(w)
			fmt.Fprint(w, ") >= 0")
		} else {
			ie.Collection.emit(w)
			fmt.Fprint(w, ".ContainsKey(")
			ie.Item.emit(w)
			fmt.Fprint(w, ")")
		}
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

func (s *ExprStmt) emit(w io.Writer) {
	if r, ok := s.Expr.(*RawExpr); ok && strings.TrimSpace(r.Code) == "null" {
		return
	}
	s.Expr.emit(w)
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	if c.Func == "keys" && len(c.Args) == 1 {
		c.Args[0].emit(w)
		fmt.Fprint(w, ".Keys")
		return
	}
	// Special case conversions to int for BigInteger arguments
	if (c.Func == "Convert.ToInt32" || c.Func == "Convert.ToInt64") && len(c.Args) == 1 &&
		typeOfExpr(c.Args[0]) == "BigInteger" {
		fmt.Fprint(w, "(int)(")
		c.Args[0].emit(w)
		fmt.Fprint(w, ")")
		return
	}
	name := c.Func
	if userFuncs != nil && userFuncs[name] {
		name = "Program." + name
	}
	fmt.Fprint(w, name)
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

// MapGetExpr represents map.get(key, default).
type MapGetExpr struct {
	Target  Expr
	Key     Expr
	Default Expr
}

func (m *MapGetExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	m.Target.emit(w)
	fmt.Fprint(w, ".ContainsKey(")
	m.Key.emit(w)
	fmt.Fprint(w, ") ? ")
	m.Target.emit(w)
	fmt.Fprint(w, "[")
	m.Key.emit(w)
	fmt.Fprint(w, "] : ")
	m.Default.emit(w)
	fmt.Fprint(w, ")")
}

func (m *MethodCallExpr) emit(w io.Writer) {
	if m.Name == "keys" && len(m.Args) == 0 && (isMapExpr(m.Target) || strings.HasPrefix(typeOfExpr(m.Target), "Dictionary<")) {
		m.Target.emit(w)
		fmt.Fprint(w, ".Keys")
		return
	}
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

type IntLit struct{ Value int64 }

func (i *IntLit) emit(w io.Writer) {
	if i.Value > math.MaxInt32 || i.Value < math.MinInt32 {
		fmt.Fprintf(w, "%dL", i.Value)
	} else {
		fmt.Fprintf(w, "%d", i.Value)
	}
}

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

func (f *FloatLit) emit(w io.Writer) {
	s := strconv.FormatFloat(f.Value, 'g', -1, 64)
	if strings.ContainsAny(s, "eE") {
		s = strings.ReplaceAll(s, "e+", "e")
		s = strings.ReplaceAll(s, "E+", "E")
	} else if !strings.Contains(s, ".") {
		s += ".0"
	}
	fmt.Fprint(w, s)
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
		t := typeOfExpr(f.Target)
		if t == "object" || t == "" {
			fmt.Fprint(w, "((dynamic)(")
			f.Target.emit(w)
			fmt.Fprintf(w, ")).%s", safeName(f.Name))
		} else {
			f.Target.emit(w)
			fmt.Fprintf(w, ".%s", safeName(f.Name))
		}
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
		t := typeOfExpr(s.Value)
		elem := "object"
		if strings.HasSuffix(t, "[]") {
			elem = strings.TrimSuffix(t, "[]")
		}
		fmt.Fprintf(w, "_slice<%s>(", elem)
		s.Value.emit(w)
		fmt.Fprint(w, ", ")
		s.Start.emit(w)
		fmt.Fprint(w, ", ")
		s.End.emit(w)
		fmt.Fprint(w, ")")
	}
}

// ContainsExpr represents s.contains(x)
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

func (c *ContainsExpr) emit(w io.Writer) {
	if isMapExpr(c.Str) {
		c.Str.emit(w)
		fmt.Fprint(w, ".ContainsKey(")
		c.Sub.emit(w)
		fmt.Fprint(w, ")")
		return
	}
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
		if stringVars[ex.Name] {
			return true
		}
		if t, ok := varTypes[ex.Name]; ok && t == "string" {
			return true
		}
		if t, ok := finalVarTypes[ex.Name]; ok && t == "string" {
			return true
		}
	case *CallExpr:
		if ex.Func == "Convert.ToString" {
			return true
		}
		if ret, ok := funRets[ex.Func]; ok && ret == "string" {
			return true
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "ToLower", "ToUpper", "Trim", "TrimStart", "TrimEnd", "Replace", "Substring":
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

func isDynamicExpr(e Expr) bool {
	t := typeOfExpr(e)
	if t == "" || t == "object" {
		return true
	}
	if vr, ok := e.(*VarRef); ok {
		if vt, ok2 := varTypes[vr.Name]; ok2 && (vt == "" || vt == "object") {
			return true
		}
		if vt, ok2 := finalVarTypes[vr.Name]; ok2 && (vt == "" || vt == "object") {
			return true
		}
	}
	return false
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *IndexExpr:
		t := typeOfExpr(e)
		if strings.HasPrefix(t, "Dictionary<") {
			return true
		}
	case *FieldExpr:
		if isMapExpr(ex.Target) {
			return true
		}
		t := typeOfExpr(e)
		return strings.HasPrefix(t, "Dictionary<")
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
		if t, ok := finalVarTypes[ex.Name]; ok {
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
		if t, ok := finalVarTypes[ex.Name]; ok {
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
		if t, ok := finalVarTypes[ex.Name]; ok {
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

func isUnit(t *parser.TypeRef) bool {
	if t == nil {
		return true
	}
	if t.Simple != nil && *t.Simple == "unit" {
		return true
	}
	if t.Simple == nil && t.Generic == nil && t.Fun == nil {
		return true
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
			return "long"
		case "string":
			return "string"
		case "bool":
			return "bool"
		case "float":
			return "double"
		case "bigint":
			usesBigInt = true
			return "BigInteger"
		case "bigrat":
			usesBigRat = true
			usesBigInt = true
			return "BigRat"
		case "void":
			return "void"
		}
		if st, ok := structTypes[*t.Simple]; ok {
			return st.Name
		}
		if typ, ok := envTypes[*t.Simple]; ok {
			return csTypeFromType(typ)
		}
		return "object"
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				if a := t.Generic.Args[0]; a.Fun != nil && isUnit(a.Fun.Return) {
					return "Func<object>[]"
				}
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
		if t.Fun.Return != nil {
			parts = append(parts, csType(t.Fun.Return))
			return fmt.Sprintf("Func<%s>", strings.Join(parts, ", "))
		}
		if len(parts) == 0 {
			return "Action"
		}
		return fmt.Sprintf("Action<%s>", strings.Join(parts, ", "))
	}
	return "object"
}

func csTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "long"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.BigIntType:
		usesBigInt = true
		return "BigInteger"
	case types.BigRatType:
		usesBigRat = true
		usesBigInt = true
		return "BigRat"
	case types.ListType:
		switch ft := tt.Elem.(type) {
		case types.FuncType:
			if ft.Return == nil && len(ft.Params) == 0 {
				return "Func<object>[]"
			}
		case *types.FuncType:
			if ft.Return == nil && len(ft.Params) == 0 {
				return "Func<object>[]"
			}
		}
		return fmt.Sprintf("%s[]", csTypeFromType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Dictionary<%s, %s>", csTypeFromType(tt.Key), csTypeFromType(tt.Value))
	case types.OptionType:
		return csTypeFromType(tt.Elem)
	case types.StructType:
		return tt.Name
	case types.FuncType:
		parts := make([]string, 0, len(tt.Params))
		for _, p := range tt.Params {
			parts = append(parts, csTypeFromType(p))
		}
		if tt.Return != nil {
			parts = append(parts, csTypeFromType(tt.Return))
			return fmt.Sprintf("Func<%s>", strings.Join(parts, ", "))
		}
		if len(parts) == 0 {
			return "Func<object>"
		}
		return fmt.Sprintf("Action<%s>", strings.Join(parts, ", "))
	case *types.FuncType:
		parts := make([]string, 0, len(tt.Params))
		for _, p := range tt.Params {
			parts = append(parts, csTypeFromType(p))
		}
		if tt.Return != nil {
			parts = append(parts, csTypeFromType(tt.Return))
			return fmt.Sprintf("Func<%s>", strings.Join(parts, ", "))
		}
		if len(parts) == 0 {
			return "Func<object>"
		}
		return fmt.Sprintf("Action<%s>", strings.Join(parts, ", "))
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

func zeroExprForType(t *parser.TypeRef) Expr {
	if t == nil {
		return &RawExpr{Code: "null"}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int", "bigint":
			return &IntLit{Value: 0}
		case "string":
			return &StringLit{Value: ""}
		case "bool":
			return &BoolLit{Value: false}
		case "float":
			return &RawExpr{Code: "0.0"}
		default:
			if _, ok := structTypes[*t.Simple]; ok {
				return &RawExpr{Code: fmt.Sprintf("new %s()", csType(t))}
			}
		}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			elem := "object[]"
			if len(t.Generic.Args) == 1 {
				elem = fmt.Sprintf("%s[]", csType(t.Generic.Args[0]))
			}
			return &ListLit{ElemType: elem}
		case "map":
			kt := "object"
			vt := "object"
			if len(t.Generic.Args) == 2 {
				kt = csType(t.Generic.Args[0])
				vt = csType(t.Generic.Args[1])
			}
			return &MapLit{KeyType: kt, ValType: vt}
		}
	}
	return &RawExpr{Code: "null"}
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
		return "long"
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
	case *UnaryExpr:
		return typeOfExpr(ex.Val)
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
		if lt == "long" || rt == "long" {
			return "long"
		}
		if lt == "string" || rt == "string" {
			return "string"
		}
		return "long"
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
		if t == "long" {
			return "long"
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
	case *LenExpr:
		return "long"
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
		if t == "string" {
			return "string"
		}
		if strings.HasSuffix(t, "[]") {
			return strings.TrimSuffix(t, "[]")
		}
		if strings.HasPrefix(t, "Dictionary<") {
			parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			arr := splitTop(parts)
			if len(arr) == 2 {
				return arr[1]
			}
		}
		if lit, ok := ex.Index.(*StringLit); ok {
			if st, ok := structTypes[t]; ok {
				if ft, ok := st.Fields[lit.Value]; ok {
					return csTypeFromType(ft)
				}
			}
		}
		return ""
	case *FieldExpr:
		t := typeOfExpr(ex.Target)
		if (t == "string" || strings.HasSuffix(t, "[]")) && ex.Name == "Length" {
			return "int"
		}
		if st, ok := structTypes[t]; ok {
			if ft, ok2 := st.Fields[ex.Name]; ok2 {
				return csTypeFromType(ft)
			}
		}
		if strings.HasPrefix(t, "Dictionary<") {
			parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			arr := splitTop(parts)
			if len(arr) == 2 {
				return arr[1]
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
		if ex.ReturnType != "" {
			return fmt.Sprintf("Func<%s>", strings.Join(append(append([]string{}, ex.ParamTypes...), ex.ReturnType), ", "))
		}
		if len(ex.ParamTypes) == 0 {
			return "Action"
		}
		return fmt.Sprintf("Action<%s>", strings.Join(ex.ParamTypes, ", "))
	case *VarRef:
		if t, ok := varTypes[ex.Name]; ok {
			return t
		}
		if t, ok := finalVarTypes[ex.Name]; ok {
			return t
		}
	case *CallExpr:
		if ex.Func == "keys" && len(ex.Args) == 1 {
			t := typeOfExpr(ex.Args[0])
			if strings.HasPrefix(t, "Dictionary<") {
				parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
				key := strings.Split(parts, ",")[0]
				return fmt.Sprintf("%s[]", strings.TrimSpace(key))
			}
			return "object[]"
		}
		if ret, ok := funRets[ex.Func]; ok {
			return ret
		}
	case *MethodCallExpr:
		switch ex.Name {
		case "ToLower", "ToUpper", "Trim", "TrimStart", "TrimEnd", "Replace", "Substring":
			return "string"
		case "ToArray":
			t := typeOfExpr(ex.Target)
			if strings.HasSuffix(t, "[]") {
				return t
			}
			if t != "" {
				return t
			}
			return "object[]"
		case "keys":
			t := typeOfExpr(ex.Target)
			if strings.HasPrefix(t, "Dictionary<") {
				parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
				key := strings.Split(parts, ",")[0]
				return fmt.Sprintf("%s[]", strings.TrimSpace(key))
			}
			return "object[]"
		case "Split":
			return "string[]"
		case "Select", "Where":
			return typeOfExpr(ex.Target)
		default:
			if vr, ok := ex.Target.(*VarRef); ok {
				if alias, ok2 := importAliases[vr.Name]; ok2 && alias == "math" {
					return "double"
				}
			}
		}
	case *MapGetExpr:
		t := typeOfExpr(ex.Target)
		if strings.HasPrefix(t, "Dictionary<") {
			parts := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			arr := strings.Split(parts, ",")
			if len(arr) == 2 {
				return strings.TrimSpace(arr[1])
			}
		}
		return "object"
	case *RawExpr:
		return ex.Type
	case *FmtExpr:
		return "string"
	case *FmtTopExpr:
		return "string"
	}
	return ""
}

func splitTop(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<':
			depth++
		case '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	parts = append(parts, strings.TrimSpace(s[start:]))
	return parts
}

func mapTypes(m *MapLit) (string, string) {
	if m.KeyType != "" && m.ValType != "" {
		return m.KeyType, m.ValType
	}
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
	for _, e := range l.Elems {
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
		if t == "object[]" || t == "object" || t == "" {
			if elemType == "" {
				continue
			}
			// ignore empty/unknown elements once type determined
			continue
		}
		if elemType == "" {
			elemType = t
		} else if elemType != t {
			elemType = ""
			break
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
	targetType := typeOfExpr(ix.Target)
	idxType := typeOfExpr(ix.Index)
	if lit, ok := ix.Index.(*StringLit); ok {
		if st, ok := structTypes[targetType]; ok {
			if _, ok := st.Fields[lit.Value]; ok {
				ix.Target.emit(w)
				fmt.Fprintf(w, ".%s", safeName(lit.Value))
				return
			}
		}
	}
	if strings.HasPrefix(targetType, "Dictionary<") && !strings.HasSuffix(targetType, "[]") {
		parts := strings.TrimPrefix(strings.TrimSuffix(targetType, ">"), "Dictionary<")
		arr := splitTop(parts)
		valType := "object"
		if len(arr) == 2 {
			valType = strings.TrimSpace(arr[1])
		}
		fmt.Fprint(w, "(")
		ix.Target.emit(w)
		fmt.Fprint(w, ".ContainsKey(")
		ix.Index.emit(w)
		fmt.Fprint(w, ") ? ")
		ix.Target.emit(w)
		fmt.Fprint(w, "[")
		ix.Index.emit(w)
		fmt.Fprint(w, "] : ")
		switch valType {
		case "long":
			fmt.Fprint(w, "0")
		case "double":
			fmt.Fprint(w, "0.0")
		case "bool":
			fmt.Fprint(w, "false")
		case "string":
			fmt.Fprint(w, "\"\"")
		default:
			fmt.Fprint(w, "null")
		}
		fmt.Fprint(w, ")")
		return
	}
	if t == "object" || t == "" {
		fmt.Fprint(w, "((dynamic)")
		ix.Target.emit(w)
		fmt.Fprint(w, ")[")
		ix.Index.emit(w)
		fmt.Fprint(w, "]")
		return
	}
	if strings.HasSuffix(targetType, "[]") {
		usesIdx = true
		fmt.Fprint(w, "_idx(")
		ix.Target.emit(w)
		fmt.Fprint(w, ", ")
		ix.Index.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	if targetType == "string" {
		ix.Target.emit(w)
		fmt.Fprint(w, ".Substring(")
		if idxType != "int" {
			fmt.Fprint(w, "(int)(")
			ix.Index.emit(w)
			fmt.Fprint(w, ")")
		} else {
			ix.Index.emit(w)
		}
		fmt.Fprint(w, ", 1)")
		return
	}
	ix.Target.emit(w)
	fmt.Fprint(w, "[")
	if idxType != "int" {
		fmt.Fprint(w, "(int)(")
		ix.Index.emit(w)
		fmt.Fprint(w, ")")
	} else {
		ix.Index.emit(w)
	}
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
		fmt.Fprintf(w, "%s = ", safeName(f.Name))
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
	t := typeOfExpr(l.Arg)
	name := ""
	if vr, ok := l.Arg.(*VarRef); ok {
		name = vr.Name
	}
	if mapVars[name] {
		l.Arg.emit(w)
		fmt.Fprint(w, ".Count")
		return
	}
	if strings.HasPrefix(t, "Dictionary<") && !strings.HasSuffix(t, "[]") || isMapExpr(l.Arg) {
		l.Arg.emit(w)
		fmt.Fprint(w, ".Count")
	} else if t == "" || t == "object" {
		fmt.Fprint(w, "_len(")
		l.Arg.emit(w)
		fmt.Fprint(w, ")")
	} else {
		l.Arg.emit(w)
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
	usesLinq = true
	t := typeOfExpr(a.List)
	if t == "" || strings.HasSuffix(t, "object[]") {
		if vr, ok := a.List.(*VarRef); ok {
			if ft, ok2 := finalVarTypes[vr.Name]; ok2 && ft != "" && !strings.HasSuffix(ft, "object[]") {
				t = ft
			}
		}
	}
	elem := "object"
	if strings.HasSuffix(t, "[]") {
		elem = strings.TrimSuffix(t, "[]")
	} else if et := typeOfExpr(a.Item); et != "" && et != "object" {
		elem = et
	}
	fmt.Fprintf(w, "(Enumerable.ToArray(Enumerable.Append<%s>(", elem)
	a.List.emit(w)
	fmt.Fprint(w, ", ")
	tItem := typeOfExpr(a.Item)
	if (tItem == "object" || tItem == "object[]") && elem != "object" {
		if lit, ok := a.Item.(*ListLit); ok && len(lit.Elems) == 0 {
			lit.ElemType = elem
			a.Item.emit(w)
		} else {
			fmt.Fprintf(w, "(%s)", elem)
			a.Item.emit(w)
		}
	} else {
		a.Item.emit(w)
	}
	fmt.Fprint(w, ")))")
}

type ConcatExpr struct {
	A Expr
	B Expr
}

func (c *ConcatExpr) emit(w io.Writer) {
	usesLinq = true
	fmt.Fprint(w, "(Enumerable.ToArray(Enumerable.Concat(")
	c.A.emit(w)
	fmt.Fprint(w, ", ")
	c.B.emit(w)
	fmt.Fprint(w, ")))")
}

type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	if isStringExpr(s.Arg) {
		s.Arg.emit(w)
	} else {
		fmt.Fprint(w, "_fmtStr(")
		s.Arg.emit(w)
		fmt.Fprint(w, ")")
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
	fmt.Fprint(w, "Program._fmtTop(")
	f.Value.emit(w)
	fmt.Fprint(w, ")")
}

// PanicExpr represents a call to panic that throws an exception.
type PanicExpr struct{ Arg Expr }

func (p *PanicExpr) emit(w io.Writer) {
	fmt.Fprint(w, "throw new Exception(")
	p.Arg.emit(w)
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
	fmt.Fprint(w, "_substr(")
	s.Str.emit(w)
	fmt.Fprint(w, ", ")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
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
	fmt.Fprint(w, " is System.Collections.IEnumerable && !(")
	e.Arg.emit(w)
	fmt.Fprint(w, " is string))")
}

// Transpile converts a Mochi AST to a simple C# AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	prog := &Program{BenchMain: benchMain}
	currentProg = prog
	defer func() { currentProg = nil }()
	stringVars = make(map[string]bool)
	mapVars = make(map[string]bool)
	varTypes = make(map[string]string)
	finalVarTypes = make(map[string]string)
	structTypes = env.Structs()
	funRets = make(map[string]string)
	funParams = make(map[string][]string)
	userFuncs = make(map[string]bool)
	usesDict = false
	usesLinq = false
	usesJson = false
	usesNow = false
	usesInput = false
	usesMem = false
	usesFmt = false
	usesRepeat = false
	usesSubstr = false
	usesSlice = false
	usesLen = false
	usesMod = false
	usesBigInt = false
	usesBigRat = false
	usesFetch = false
	fetchStructs = make(map[string]bool)
	usesIdx = true
	currentAssign = ""
	globalDecls = make(map[string]*Global)
	mutatedVars = make(map[string]bool)
	importAliases = make(map[string]string)
	varAliases = make(map[string]string)
	aliasCounter = 0
	envTypes = env.Types()
	prog.Globals = append(prog.Globals, &Global{Name: "__name__", Value: &StringLit{Value: "__main__"}})
	for name, typ := range env.Types() {
		if ft, ok := typ.(types.FuncType); ok {
			params := make([]string, len(ft.Params))
			for i, p := range ft.Params {
				params[i] = csTypeFromType(p)
			}
			funParams[name] = params
			funRets[name] = csTypeFromType(ft.Return)
		}
	}
	funRets["_bigrat"] = "BigRat"
	funRets["_add"] = "BigRat"
	funRets["_sub"] = "BigRat"
	funRets["_mul"] = "BigRat"
	funRets["_div"] = "BigRat"
	funRets["_num"] = "BigInteger"
	funRets["_denom"] = "BigInteger"
	funRets["Convert.ToInt64"] = "long"
	funRets["_atoi"] = "long"
	funRets["_fetch"] = "object"
	funRets["Convert.ToDouble"] = "double"
	funRets["Convert.ToString"] = "string"
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
		if !found {
			fields := make([]StructField, len(st.Order))
			for i, n := range st.Order {
				fields[i] = StructField{Name: n, Type: csTypeFromType(st.Fields[n])}
			}
			var methods []*Function
			if len(st.Methods) > 0 {
				keys := make([]string, 0, len(st.Methods))
				for m := range st.Methods {
					keys = append(keys, m)
				}
				sort.Strings(keys)
				for _, m := range keys {
					fn, err := compileStructMethod(name, st.Methods[m])
					if err != nil {
						return nil, err
					}
					methods = append(methods, fn)
				}
			}
			prog.Structs = append(prog.Structs, StructDecl{Name: name, Fields: fields, Methods: methods})
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
			if op == "==" || op == "!=" {
				if isListExpr(left) || isListExpr(right) {
					usesLinq = true
					if l, ok := left.(*ListLit); ok && len(l.Elems) == 0 && l.ElemType == "" {
						l.ElemType = typeOfExpr(right)
					}
					if r, ok := right.(*ListLit); ok && len(r.Elems) == 0 && r.ElemType == "" {
						r.ElemType = typeOfExpr(left)
					}
				} else if (isStringExpr(left) && isBoolExpr(right)) || (isBoolExpr(left) && isStringExpr(right)) {
					if bl, ok := left.(*BoolLit); ok {
						left = &StringLit{Value: map[bool]string{true: "true", false: "false"}[bl.Value]}
					}
					if br, ok := right.(*BoolLit); ok {
						right = &StringLit{Value: map[bool]string{true: "true", false: "false"}[br.Value]}
					}
				}
			}
			return &CmpExpr{Op: op, Left: left, Right: right}
		case "&&", "||":
			return &BoolOpExpr{Op: op, Left: left, Right: right}
		case "in":
			return &InExpr{Item: left, Collection: right}
		case "union", "union_all", "except", "intersect":
			usesLinq = true
			fallthrough
		default:
			if op == "%" {
				usesMod = true
			}
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
				usesSubstr = true
				expr = &SubstringExpr{Str: expr, Start: start, End: end}
			} else {
				usesSlice = true
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
				if vr, ok2 := fe.Target.(*VarRef); ok2 && vr.Name == "Object" && fe.Name == "keys" {
					if len(args) != 1 {
						return nil, fmt.Errorf("unsupported method call")
					}
					usesLinq = true
					code := fmt.Sprintf("((System.Collections.IDictionary)%s).Keys.Cast<string>()", exprString(args[0]))
					expr = &RawExpr{Code: code, Type: "string[]"}
				} else if vr, ok2 := fe.Target.(*VarRef); ok2 && vr.Name == "stdout" && fe.Name == "write" && len(args) == 1 {
					expr = &CallExpr{Func: "Console.Write", Args: args}
				} else if fe.Name == "contains" {
					if len(args) != 1 {
						return nil, fmt.Errorf("unsupported method call")
					}
					expr = &ContainsExpr{Str: fe.Target, Sub: args[0]}
				} else if fe.Name == "get" {
					if len(args) != 2 {
						return nil, fmt.Errorf("unsupported method call")
					}
					expr = &MapGetExpr{Target: fe.Target, Key: args[0], Default: args[1]}
				} else if fe.Name == "padStart" {
					if len(args) == 2 {
						var ch Expr
						if lit, ok := args[1].(*StringLit); ok && len(lit.Value) > 0 {
							ch = &RawExpr{Code: fmt.Sprintf("'%c'", []rune(lit.Value)[0]), Type: "char"}
						} else {
							ch = &CallExpr{Func: "Convert.ToChar", Args: []Expr{args[1]}}
						}
						tgt := fe.Target
						if typeOfExpr(tgt) != "string" {
							tgt = &CallExpr{Func: "Convert.ToString", Args: []Expr{tgt}}
						}
						expr = &MethodCallExpr{Target: tgt, Name: "PadLeft", Args: []Expr{args[0], ch}}
					} else if len(args) == 1 {
						ch := &RawExpr{Code: "' '", Type: "char"}
						tgt := fe.Target
						if typeOfExpr(tgt) != "string" {
							tgt = &CallExpr{Func: "Convert.ToString", Args: []Expr{tgt}}
						}
						expr = &MethodCallExpr{Target: tgt, Name: "PadLeft", Args: []Expr{args[0], ch}}
					} else {
						return nil, fmt.Errorf("unsupported method call")
					}
				} else if fe.Name == "join" {
					if len(args) == 1 {
						expr = &CallExpr{Func: "string.Join", Args: []Expr{args[0], fe.Target}}
					} else {
						return nil, fmt.Errorf("unsupported method call")
					}
				} else {
					expr = &MethodCallExpr{Target: fe.Target, Name: fe.Name, Args: args}
				}
			} else if vr, ok := expr.(*VarRef); ok {
				if vr.Name == "stdout" && len(args) == 1 {
					expr = &CallExpr{Func: "Console.Write", Args: args}
				} else {
					expr = &CallExpr{Func: vr.Name, Args: args}
				}
			} else if _, ok := expr.(*IndexExpr); ok {
				var sb strings.Builder
				sb.WriteString(exprString(expr))
				sb.WriteString("(")
				for i, a := range args {
					if i > 0 {
						sb.WriteString(", ")
					}
					sb.WriteString(exprString(a))
				}
				sb.WriteString(")")
				expr = &RawExpr{Code: sb.String()}
			} else if _, ok := expr.(*CallExpr); ok {
				var sb strings.Builder
				sb.WriteString(exprString(expr))
				sb.WriteString("(")
				for i, a := range args {
					if i > 0 {
						sb.WriteString(", ")
					}
					sb.WriteString(exprString(a))
				}
				sb.WriteString(")")
				expr = &RawExpr{Code: sb.String()}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			switch *op.Cast.Type.Simple {
			case "int":
				if texpr := typeOfExpr(expr); texpr == "" || texpr == "object" || texpr == "string" {
					expr = &RawExpr{Code: fmt.Sprintf("long.Parse(%s)", exprString(expr)), Type: "long"}
				} else {
					expr = &RawExpr{Code: fmt.Sprintf("(long)(%s)", exprString(expr)), Type: "long"}
				}
			case "float":
				expr = &CallExpr{Func: "Convert.ToDouble", Args: []Expr{expr}}
			case "bigint":
				usesBigInt = true
				expr = &RawExpr{Code: fmt.Sprintf("new BigInteger(%s)", exprString(expr)), Type: "BigInteger"}
			case "bigrat":
				usesBigRat = true
				usesBigInt = true
				expr = &RawExpr{Code: fmt.Sprintf("_bigrat(%s)", exprString(expr)), Type: "BigRat"}
			default:
				// other casts are treated as no-ops
			}
		case op.Cast != nil && op.Cast.Type != nil:
			if op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "list" && len(op.Cast.Type.Generic.Args) == 1 {
				typ := fmt.Sprintf("%s[]", csType(op.Cast.Type.Generic.Args[0]))
				if lit, ok := expr.(*ListLit); ok && len(lit.Elems) == 0 {
					lit.ElemType = typ
					expr = lit
				} else {
					expr = &RawExpr{Code: fmt.Sprintf("(%s as %s) ?? new %s{}", exprString(expr), typ, typ), Type: typ}
				}
			} else if op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "map" && len(op.Cast.Type.Generic.Args) == 2 {
				kt := csType(op.Cast.Type.Generic.Args[0])
				vt := csType(op.Cast.Type.Generic.Args[1])
				typ := fmt.Sprintf("Dictionary<%s, %s>", kt, vt)
				expr = &RawExpr{Code: fmt.Sprintf("(%s as %s) ?? new %s{}", exprString(expr), typ, typ), Type: typ}
			} else if op.Cast.Type.Simple != nil {
				switch *op.Cast.Type.Simple {
				case "int":
					texpr := typeOfExpr(expr)
					if texpr == "" || texpr == "object" || texpr == "string" {
						expr = &RawExpr{Code: fmt.Sprintf("long.Parse(%s)", exprString(expr)), Type: "long"}
					}
				}
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
		if vt, ok := varTypes[s.Let.Name]; ok && !strings.HasPrefix(vt, "fn/") {
			if s.Let.Value == nil {
				return nil, fmt.Errorf("redeclare without value")
			}
			saved := currentAssign
			currentAssign = s.Let.Name
			val, err := compileExpr(s.Let.Value)
			currentAssign = saved
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: s.Let.Name, Value: val}, nil
		}
		var val Expr
		var err error
		if s.Let.Value != nil {
			if s.Let.Type != nil {
				varTypes[s.Let.Name] = csType(s.Let.Type)
			}
			saved := currentAssign
			currentAssign = s.Let.Name
			val, err = compileExpr(s.Let.Value)
			currentAssign = saved
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
			case "bigint":
				val = &IntLit{Value: 0}
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
			if s.Let.Type != nil && s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "list" && len(s.Let.Type.Generic.Args) == 1 {
				list.ElemType = fmt.Sprintf("%s[]", csType(s.Let.Type.Generic.Args[0]))
				varTypes[s.Let.Name] = list.ElemType
			} else if len(list.Elems) == 0 {
				list.ElemType = "object[]"
				varTypes[s.Let.Name] = "object[]"
			}
		}
		if mp, ok := val.(*MapLit); ok {
			if s.Let.Type != nil && s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "map" && len(s.Let.Type.Generic.Args) == 2 {
				mp.KeyType = csType(s.Let.Type.Generic.Args[0])
				mp.ValType = csType(s.Let.Type.Generic.Args[1])
				for _, it := range mp.Items {
					if l, ok := it.Value.(*ListLit); ok && len(l.Elems) == 0 {
						l.ElemType = mp.ValType
					} else if m2, ok := it.Value.(*MapLit); ok && len(m2.Items) == 0 {
						if strings.HasPrefix(mp.ValType, "Dictionary<") {
							inside := strings.TrimPrefix(strings.TrimSuffix(mp.ValType, ">"), "Dictionary<")
							parts := strings.Split(inside, ",")
							if len(parts) == 2 {
								m2.KeyType = strings.TrimSpace(parts[0])
								m2.ValType = strings.TrimSpace(parts[1])
							}
						}
					}
				}
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
		alias := fmt.Sprintf("%s_%d", s.Let.Name, aliasCounter)
		aliasCounter++
		varAliases[s.Let.Name] = alias
		if t, ok := varTypes[s.Let.Name]; ok {
			varTypes[alias] = t
			if alias != s.Let.Name {
				delete(varTypes, s.Let.Name)
			}
			finalVarTypes[alias] = t
		}
		if prog != nil && blockDepth == 0 && len(prog.Stmts) == 0 {
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
		if vt, ok := varTypes[s.Var.Name]; ok && !strings.HasPrefix(vt, "fn/") {
			if s.Var.Value == nil {
				return nil, fmt.Errorf("redeclare without value")
			}
			saved := currentAssign
			currentAssign = s.Var.Name
			val, err := compileExpr(s.Var.Value)
			currentAssign = saved
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
			if s.Var.Type != nil {
				varTypes[s.Var.Name] = csType(s.Var.Type)
			}
			saved := currentAssign
			currentAssign = s.Var.Name
			val, err = compileExpr(s.Var.Value)
			currentAssign = saved
			if err != nil {
				return nil, err
			}
		} else if s.Var.Type != nil {
			val = zeroExprForType(s.Var.Type)
			if isStringExpr(val) {
				stringVars[s.Var.Name] = true
			}
			varTypes[s.Var.Name] = csType(s.Var.Type)
		}
		if list, ok := val.(*ListLit); ok {
			if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "list" && len(s.Var.Type.Generic.Args) == 1 {
				list.ElemType = fmt.Sprintf("%s[]", csType(s.Var.Type.Generic.Args[0]))
				if mt := s.Var.Type.Generic.Args[0]; mt.Generic != nil && mt.Generic.Name == "map" && len(mt.Generic.Args) == 2 {
					kt := csType(mt.Generic.Args[0])
					vt := csType(mt.Generic.Args[1])
					for _, e := range list.Elems {
						if mp, ok2 := e.(*MapLit); ok2 {
							mp.KeyType = kt
							mp.ValType = vt
						}
					}
				}
				varTypes[s.Var.Name] = list.ElemType
			} else if len(list.Elems) == 0 {
				if list.ElemType == "" {
					list.ElemType = "object[]"
				}
				varTypes[s.Var.Name] = list.ElemType
			}
			_ = list // do not convert mutable vars to structs
		}
		if mp, ok := val.(*MapLit); ok {
			if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "map" && len(s.Var.Type.Generic.Args) == 2 {
				mp.KeyType = csType(s.Var.Type.Generic.Args[0])
				mp.ValType = csType(s.Var.Type.Generic.Args[1])
				for _, it := range mp.Items {
					if l, ok := it.Value.(*ListLit); ok && len(l.Elems) == 0 {
						l.ElemType = mp.ValType
					} else if m2, ok := it.Value.(*MapLit); ok && len(m2.Items) == 0 {
						if strings.HasPrefix(mp.ValType, "Dictionary<") {
							inside := strings.TrimPrefix(strings.TrimSuffix(mp.ValType, ">"), "Dictionary<")
							parts := strings.Split(inside, ",")
							if len(parts) == 2 {
								m2.KeyType = strings.TrimSpace(parts[0])
								m2.ValType = strings.TrimSpace(parts[1])
							}
						}
					}
				}
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
		alias := fmt.Sprintf("%s_%d", s.Var.Name, aliasCounter)
		aliasCounter++
		varAliases[s.Var.Name] = alias
		if t, ok := varTypes[s.Var.Name]; ok {
			varTypes[alias] = t
			if alias != s.Var.Name {
				delete(varTypes, s.Var.Name)
			}
			finalVarTypes[alias] = t
		}
		if mapVars[s.Var.Name] {
			mapVars[alias] = true
			if alias != s.Var.Name {
				delete(mapVars, s.Var.Name)
			}
		}
		if prog != nil && blockDepth == 0 && len(prog.Stmts) == 0 {
			for m := range mutatedVars {
				if exprUsesVar(val, m) {
					return &VarStmt{Name: alias, Value: val}, nil
				}
			}
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
				var methods []*Function
				if len(st.Methods) > 0 {
					keys := make([]string, 0, len(st.Methods))
					for name := range st.Methods {
						keys = append(keys, name)
					}
					sort.Strings(keys)
					for _, name := range keys {
						fn, err := compileStructMethod(s.Type.Name, st.Methods[name])
						if err != nil {
							return nil, err
						}
						methods = append(methods, fn)
					}
				}
				prog.Structs = append(prog.Structs, StructDecl{Name: s.Type.Name, Fields: fields, Methods: methods})
			}
		}
		return nil, nil
	case s.Assign != nil:
		val, err := compileExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		// Construct a PostfixExpr representing the assignment target
		tgt := &parser.PostfixExpr{
			Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: s.Assign.Name}},
		}
		for _, idx := range s.Assign.Index {
			tgt.Ops = append(tgt.Ops, &parser.PostfixOp{Index: idx})
		}
		for _, fld := range s.Assign.Field {
			tgt.Ops = append(tgt.Ops, &parser.PostfixOp{Field: fld})
		}
		targetExpr, err := compilePostfix(tgt)
		if err != nil {
			return nil, err
		}
		switch t := targetExpr.(type) {
		case *VarRef:
			name := t.Name
			if alias, ok := varAliases[name]; ok {
				name = alias
			}
			if isStringExpr(val) {
				stringVars[name] = true
			}
			if isMapExpr(val) {
				mapVars[name] = true
			}
			prev := ""
			if tt, ok := finalVarTypes[name]; ok {
				prev = tt
			} else if tt, ok := varTypes[name]; ok {
				prev = tt
			}
			if tt := typeOfExpr(val); tt != "" && tt != "object" {
				varTypes[name] = tt
				finalVarTypes[name] = tt
			}
			if l, ok := val.(*ListLit); ok && len(l.Elems) == 0 {
				if prev != "" && strings.HasSuffix(prev, "[]") {
					l.ElemType = prev
					varTypes[name] = prev
					finalVarTypes[name] = prev
				}
			} else if mp, ok := val.(*MapLit); ok && len(mp.Items) == 0 {
				if prev != "" && strings.HasPrefix(prev, "Dictionary<") {
					inside := strings.TrimPrefix(strings.TrimSuffix(prev, ">"), "Dictionary<")
					parts := strings.Split(inside, ",")
					if len(parts) == 2 {
						mp.KeyType = strings.TrimSpace(parts[0])
						mp.ValType = strings.TrimSpace(parts[1])
						varTypes[name] = prev
						finalVarTypes[name] = prev
					}
				}
			}
			mutatedVars[name] = true
			return &AssignStmt{Name: name, Value: val}, nil
		case *IndexExpr:
			if l, ok := val.(*ListLit); ok && len(l.Elems) == 0 {
				if tt := typeOfExpr(t.Target); strings.HasPrefix(tt, "Dictionary<") {
					inside := strings.TrimPrefix(strings.TrimSuffix(tt, ">"), "Dictionary<")
					parts := strings.Split(inside, ",")
					if len(parts) == 2 {
						l.ElemType = strings.TrimSpace(parts[1])
					}
				}
			}
			return &AssignIndexStmt{Target: t.Target, Index: t.Index, Value: val}, nil
		case *FieldExpr:
			// When assigning an empty map or list literal to a struct field,
			// propagate the field's type so we emit a properly typed zero value
			// instead of Dictionary<object, object> or object[].
			if l, ok := val.(*ListLit); ok && len(l.Elems) == 0 {
				if ft := typeOfExpr(t); strings.HasSuffix(ft, "[]") {
					l.ElemType = ft
				}
			} else if mp, ok := val.(*MapLit); ok && len(mp.Items) == 0 {
				if ft := typeOfExpr(t); strings.HasPrefix(ft, "Dictionary<") {
					inside := strings.TrimPrefix(strings.TrimSuffix(ft, ">"), "Dictionary<")
					parts := strings.Split(inside, ",")
					if len(parts) == 2 {
						mp.KeyType = strings.TrimSpace(parts[0])
						mp.ValType = strings.TrimSpace(parts[1])
					}
				}
			}
			return &AssignFieldStmt{Target: t.Target, Name: t.Name, Value: val}, nil
		default:
			return nil, fmt.Errorf("unsupported assignment target")
		}
	case s.Fun != nil:
		params := make([]string, len(s.Fun.Params))
		ptypes := make([]string, len(s.Fun.Params))
		savedAliases := cloneStringMap(varAliases)
		funAlias := safeName(s.Fun.Name)
		varAliases[s.Fun.Name] = funAlias
		savedAliases[s.Fun.Name] = funAlias
		savedRet := currentReturnType
		savedVoid := currentReturnVoid
		defer func() {
			varAliases = savedAliases
			currentReturnType = savedRet
			currentReturnVoid = savedVoid
		}()
		// save global variable maps so local declarations don't leak
		savedAll := cloneStringMap(varTypes)
		savedMap := cloneBoolMap(mapVars)
		savedStrAll := cloneBoolMap(stringVars)
		savedMut := cloneBoolMap(mutatedVars)
		saved := make(map[string]string)
		savedStr := make(map[string]bool)
		for i, p := range s.Fun.Params {
			alias := fmt.Sprintf("%s_%d", p.Name, aliasCounter)
			aliasCounter++
			params[i] = alias
			ptypes[i] = csType(p.Type)
			saved[p.Name] = varTypes[p.Name]
			varTypes[p.Name] = ptypes[i]
			varAliases[p.Name] = alias
			varTypes[alias] = ptypes[i]
			savedStr[p.Name] = stringVars[p.Name]
			if ptypes[i] == "string" {
				stringVars[p.Name] = true
			} else {
				delete(stringVars, p.Name)
			}
		}
		retType := csType(s.Fun.Return)
		currentReturnType = ""
		if retType != "void" && retType != "" {
			currentReturnType = retType
		}
		currentReturnVoid = s.Fun.Return == nil || retType == "void"
		// record return and parameter types for local functions so callers can infer them
		funRets[s.Fun.Name] = currentReturnType
		funParams[s.Fun.Name] = append([]string{}, ptypes...)
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
		for _, st := range body {
			if r, ok := st.(*ReturnStmt); ok && r.Value != nil {
				vt := typeOfExpr(r.Value)
				if strings.HasPrefix(vt, "Func<") {
					if strings.HasPrefix(retType, "Func<object") || strings.HasPrefix(retType, "Action") || retType == "" {
						retType = vt
						currentReturnType = vt
						funRets[s.Fun.Name] = retType
					}
				}
			}
		}
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
		for name, typ := range varTypes {
			if typ != "" {
				finalVarTypes[name] = typ
			}
		}
		// restore global maps
		varTypes = savedAll
		mapVars = savedMap
		stringVars = savedStrAll
		mutatedVars = savedMut
		if prog != nil && blockDepth == 0 {
			varTypes[s.Fun.Name] = fmt.Sprintf("fn/%d", len(ptypes))
			if currentReturnType != "" {
				funRets[s.Fun.Name] = currentReturnType
			} else {
				funRets[s.Fun.Name] = retType
			}
			userFuncs[s.Fun.Name] = true
		}
		if currentReturnType != "" {
			retType = currentReturnType
		}
		if s.Fun.Return == nil || retType == "void" {
			retType = ""
		}
		var res Stmt
		if prog != nil {
			if s.Fun.Name == "exp" && len(params) == 1 && retType == "double" {
				body = []Stmt{&ReturnStmt{Value: &RawExpr{Code: fmt.Sprintf("Math.Exp(%s)", safeName(params[0]))}}}
			}
			prog.Funcs = append(prog.Funcs, &Function{Name: s.Fun.Name, Params: params, ParamTypes: ptypes, ReturnType: retType, Body: body})
		} else {
			lit := &FunLit{Params: params, ParamTypes: ptypes, ReturnType: retType, Body: body}
			typ := typeOfExpr(lit)
			varTypes[s.Fun.Name] = typ
			finalVarTypes[s.Fun.Name] = typ
			res = &LetStmt{Name: s.Fun.Name, Value: lit}
		}
		if res == nil {
			return nil, nil
		}
		return res, nil
	case s.Return != nil:
		var val Expr
		if s.Return.Value != nil {
			var err error
			val, err = compileExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
			if ml, ok := val.(*MapLit); ok {
				if st, ok := structTypes[currentReturnType]; ok {
					fields := make([]StructFieldValue, len(ml.Items))
					for i, it := range ml.Items {
						if k, ok := it.Key.(*StringLit); ok {
							fields[i] = StructFieldValue{Name: k.Value, Value: it.Value}
						}
					}
					val = &StructLit{Name: st.Name, Fields: fields}
				}
			}
			if ml, ok := val.(*MapLit); ok && currentReturnType != "" && strings.HasPrefix(currentReturnType, "Dictionary<") {
				parts := strings.TrimPrefix(strings.TrimSuffix(currentReturnType, ">"), "Dictionary<")
				arr := strings.Split(parts, ",")
				if len(arr) == 2 {
					if ml.KeyType == "" {
						ml.KeyType = strings.TrimSpace(arr[0])
					}
					if ml.ValType == "" {
						ml.ValType = strings.TrimSpace(arr[1])
					}
				}
			}
			if l, ok := val.(*ListLit); ok && len(l.Elems) == 0 && strings.HasSuffix(currentReturnType, "[]") {
				if l.ElemType == "" || l.ElemType == "object[]" {
					l.ElemType = currentReturnType
				}
			}
			if l, ok := val.(*ListLit); ok && len(l.Elems) > 0 && strings.HasSuffix(currentReturnType, "[]") {
				elemCt := strings.TrimSuffix(currentReturnType, "[]")
				if l.ElemType == "" || l.ElemType == "object[]" {
					l.ElemType = currentReturnType
				}
				for _, e := range l.Elems {
					if ll, ok2 := e.(*ListLit); ok2 && len(ll.Elems) == 0 {
						if ll.ElemType == "" || ll.ElemType == "object[]" || ll.ElemType == "object" {
							ll.ElemType = elemCt
						}
					}
				}
			}
			if ct := currentReturnType; ct != "" {
				vt := typeOfExpr(val)
				if strings.HasPrefix(ct, "Action") && strings.HasPrefix(vt, "Func<") {
					currentReturnType = vt
				} else if vt != ct && vt != "" {
					if vt == "object[]" && strings.HasSuffix(ct, "[]") {
						elem := strings.TrimSuffix(ct, "[]")
						if elem == "Action" {
							if l, ok := val.(*ListLit); ok {
								for i, e := range l.Elems {
									if vr, ok2 := e.(*VarRef); ok2 {
										call := &CallExpr{Func: vr.Name, Args: nil}
										cast := &RawExpr{Code: fmt.Sprintf("(object)(%s)", exprString(call)), Type: "object"}
										l.Elems[i] = &FunLit{ExprBody: cast, ReturnType: "object"}
									}
								}
								ct = "Func<object>[]"
								currentReturnType = ct
								l.ElemType = ct
								vt = ct
							}
						} else if elem == "Func<object>" {
							if l, ok := val.(*ListLit); ok {
								for i, e := range l.Elems {
									if vr, ok2 := e.(*VarRef); ok2 {
										call := &CallExpr{Func: vr.Name, Args: nil}
										cast := &RawExpr{Code: fmt.Sprintf("(object)(%s)", exprString(call)), Type: "object"}
										l.Elems[i] = &FunLit{ExprBody: cast, ReturnType: "object"}
									}
								}
								l.ElemType = ct
								vt = ct
							}
						}
						if vt != ct {
							usesLinq = true
							val = &RawExpr{Code: fmt.Sprintf("%s.Cast<%s>().ToArray()", exprString(val), elem), Type: ct}
						}
					} else {
						code := exprString(val)
						if strings.HasPrefix(code, "(") {
							code = "(" + code + ")"
						}
						val = &RawExpr{Code: fmt.Sprintf("(%s)%s", ct, code), Type: ct}
					}
				}
			}
		}
		if val == nil {
			if !currentReturnVoid {
				val = &RawExpr{Code: fmt.Sprintf("default(%s)", currentReturnType), Type: currentReturnType}
			}
		} else if currentReturnVoid {
			if rv, ok := val.(*RawExpr); ok && rv.Code == "null" {
				val = nil
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
			varTypes[s.For.Name] = "long"
			alias := fmt.Sprintf("%s_%d", s.For.Name, aliasCounter)
			aliasCounter++
			varAliases[s.For.Name] = alias
			if t, ok := varTypes[s.For.Name]; ok {
				varTypes[alias] = t
				finalVarTypes[alias] = t
				if stringVars[s.For.Name] {
					stringVars[alias] = true
				}
				if mapVars[s.For.Name] {
					mapVars[alias] = true
				}
			}
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
		if t, ok := varTypes[s.For.Name]; ok {
			varTypes[alias] = t
			finalVarTypes[alias] = t
			if stringVars[s.For.Name] {
				stringVars[alias] = true
			}
			if mapVars[s.For.Name] {
				mapVars[alias] = true
			}
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
		stmt := &ForInStmt{Var: alias, Iterable: iterable, Body: body}
		if t := typeOfExpr(iterable); strings.HasSuffix(t, "[]") {
			varTypes[alias] = strings.TrimSuffix(t, "[]")
		} else if isMapExpr(iterable) && strings.HasPrefix(t, "Dictionary<") {
			inside := strings.TrimPrefix(strings.TrimSuffix(t, ">"), "Dictionary<")
			parts := strings.Split(inside, ",")
			if len(parts) == 2 {
				varTypes[alias] = strings.TrimSpace(parts[0])
			}
		}
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
	case s.Bench != nil:
		savedVars := cloneStringMap(varTypes)
		savedMap := cloneBoolMap(mapVars)
		savedStr := cloneBoolMap(stringVars)
		savedMut := cloneBoolMap(mutatedVars)
		var body []Stmt
		blockDepth++
		for _, b := range s.Bench.Body {
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
		usesNow = true
		usesMem = true
		return &BenchStmt{Name: s.Bench.Name, Body: body}, nil
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
			} else if s.Import.Path == "\"subprocess\"" || s.Import.Path == "subprocess" {
				if _, ok := importAliases[s.Import.As]; !ok {
					stub := "using System.Diagnostics;\n" +
						fmt.Sprintf("static class %s {\n", s.Import.As) +
						"    public static string getoutput(string cmd) {\n" +
						"        var psi = new ProcessStartInfo(\"/bin/sh\", \"-c \" + cmd) { RedirectStandardOutput = true };\n" +
						"        using var p = Process.Start(psi);\n" +
						"        var output = p.StandardOutput.ReadToEnd();\n" +
						"        p.WaitForExit();\n" +
						"        return output.TrimEnd('\\n');\n" +
						"    }\n" +
						"}\n"
					prog.Imports = append(prog.Imports, stub)
				}
				importAliases[s.Import.As] = "subprocess"
				varTypes[s.Import.As] = "dynamic"
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
			} else if path == "os" || path == "mochi/runtime/ffi/go/os" {
				if _, ok := importAliases[s.Import.As]; !ok {
					stub := "using System;\n" +
						"using System.Linq;\n" +
						"using System.Collections;\n" +
						"static class os {\n" +
						"    public static string Getenv(string k) => Environment.GetEnvironmentVariable(k);\n" +
						"    public static string[] Environ() => Environment.GetEnvironmentVariables().Cast<DictionaryEntry>().Select(e => $\"{e.Key}={e.Value}\").ToArray();\n" +
						"}\n"
					prog.Imports = append(prog.Imports, stub)
				}
				importAliases[s.Import.As] = "os"
			} else if path == "mochi/runtime/ffi/go/testpkg" {
				if _, ok := importAliases[s.Import.As]; !ok {
					stub := "using System.Security.Cryptography;\n" +
						"using System.Text;\n" +
						"using System.Numerics;\n" +
						"using System.Collections.Generic;\n" +
						"static class testpkg {\n" +
						"    public class ECDSAResult {\n" +
						"        public string D { get; set; }\n" +
						"        public string X { get; set; }\n" +
						"        public string Y { get; set; }\n" +
						"        public string Hash { get; set; }\n" +
						"        public string R { get; set; }\n" +
						"        public string S { get; set; }\n" +
						"        public bool Valid { get; set; }\n" +
						"    }\n" +
						"    public static string FifteenPuzzleExample() => \"Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd\";\n" +
						"    public static string MD5Hex(string s) {\n" +
						"        using var md5 = System.Security.Cryptography.MD5.Create();\n" +
						"        var bytes = System.Text.Encoding.ASCII.GetBytes(s);\n" +
						"        var hash = md5.ComputeHash(bytes);\n" +
						"        return string.Concat(hash.Select(b => b.ToString(\"x2\")));\n" +
						"    }\n" +
						"    public static ECDSAResult ECDSAExample() {\n" +
						"        return new ECDSAResult{\n" +
						"            D = \"1234567890\",\n" +
						"            X = \"43162711582587979080031819627904423023685561091192625653251495188141318209988\",\n" +
						"            Y = \"86807430002474105664458509423764867536342689150582922106807036347047552480521\",\n" +
						"            Hash = \"0xe6f9ed0d\",\n" +
						"            R = \"43162711582587979080031819627904423023685561091192625653251495188141318209988\",\n" +
						"            S = \"94150071556658883365738746782965214584303361499725266605620843043083873122499\",\n" +
						"            Valid = true\n" +
						"        };\n" +
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
		if alias, ok := varAliases[name]; ok {
			name = alias
		}
		if types, ok := funParams[name]; ok {
			for i, arg := range args {
				if list, ok2 := arg.(*ListLit); ok2 && len(list.Elems) == 0 && (list.ElemType == "" || list.ElemType == "object[]") {
					if i < len(types) && strings.HasSuffix(types[i], "[]") {
						list.ElemType = types[i]
					}
				} else if mp, ok2 := arg.(*MapLit); ok2 && len(mp.Items) == 0 && i < len(types) && strings.HasPrefix(types[i], "Dictionary<") {
					parts := strings.TrimPrefix(strings.TrimSuffix(types[i], ">"), "Dictionary<")
					arr := strings.Split(parts, ",")
					if len(arr) == 2 {
						if mp.KeyType == "" {
							mp.KeyType = strings.TrimSpace(arr[0])
						}
						if mp.ValType == "" {
							mp.ValType = strings.TrimSpace(arr[1])
						}
					}
				} else if i < len(types) && typeOfExpr(arg) == "object[]" && strings.HasSuffix(types[i], "[]") {
					elem := strings.TrimSuffix(types[i], "[]")
					conv := ""
					switch elem {
					case "long":
						conv = fmt.Sprintf("Enumerable.ToArray(%s.Select(x => Convert.ToInt64(x)))", exprString(arg))
					case "double":
						conv = fmt.Sprintf("Enumerable.ToArray(%s.Select(x => Convert.ToDouble(x)))", exprString(arg))
					case "object":
						conv = exprString(arg)
					default:
						conv = fmt.Sprintf("Enumerable.ToArray(%s.Cast<%s>())", exprString(arg), elem)
					}
					args[i] = &RawExpr{Code: conv, Type: types[i]}
				} else if i < len(types) && typeOfExpr(arg) == "object" && strings.HasSuffix(types[i], "[]") {
					args[i] = &RawExpr{Code: fmt.Sprintf("(%s)%s", types[i], exprString(arg)), Type: types[i]}
				} else if i < len(types) && typeOfExpr(arg) == "object" {
					switch types[i] {
					case "long":
						args[i] = &RawExpr{Code: fmt.Sprintf("Convert.ToInt64(%s)", exprString(arg)), Type: "long"}
					case "double":
						args[i] = &RawExpr{Code: fmt.Sprintf("Convert.ToDouble(%s)", exprString(arg)), Type: "double"}
					case "string":
						args[i] = &RawExpr{Code: fmt.Sprintf("Convert.ToString(%s)", exprString(arg)), Type: "string"}
					default:
						args[i] = &RawExpr{Code: fmt.Sprintf("(%s)%s", types[i], exprString(arg)), Type: types[i]}
					}
				}
			}
		}
		if name == "now" && len(args) == 0 {
			usesNow = true
			return &RawExpr{Code: "_now()", Type: "long"}, nil
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
		if userFuncs[name] {
			return &CallExpr{Func: name, Args: args}, nil
		}
		switch name {
		case "print":
			name = "Console.WriteLine"
			if len(args) == 1 {
				arg := args[0]
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
		case "concat":
			if len(args) == 2 {
				usesLinq = true
				return &ConcatExpr{A: args[0], B: args[1]}, nil
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
		case "floor":
			if len(args) == 1 {
				return &CallExpr{Func: "Math.Floor", Args: []Expr{args[0]}}, nil
			}
		case "len":
			if len(args) == 1 {
				usesLen = true
				return &LenExpr{Arg: args[0]}, nil
			}
		case "sum":
			if len(args) == 1 {
				usesLinq = true
				return &SumExpr{Arg: args[0]}, nil
			}
		case "str":
			if len(args) == 1 {
				usesFmt = true
				return &StrExpr{Arg: args[0]}, nil
			}
		case "int":
			if len(args) == 1 {
				usesAtoi = true
				return &CallExpr{Func: "_atoi", Args: []Expr{args[0]}}, nil
			}
		case "float":
			if len(args) == 1 {
				return &CallExpr{Func: "Convert.ToDouble", Args: []Expr{args[0]}}, nil
			}
		case "to_float":
			if len(args) == 1 {
				return &CallExpr{Func: "Convert.ToDouble", Args: []Expr{args[0]}}, nil
			}
		case "panic":
			if len(args) == 1 {
				return &PanicExpr{Arg: args[0]}, nil
			}
		case "error":
			if len(args) == 1 {
				return &PanicExpr{Arg: args[0]}, nil
			}
		case "upper":
			if len(args) == 1 {
				return &MethodCallExpr{Target: args[0], Name: "ToUpper", Args: nil}, nil
			}
		case "lower":
			if len(args) == 1 {
				return &MethodCallExpr{Target: args[0], Name: "ToLower", Args: nil}, nil
			}
		case "contains":
			if len(args) == 2 {
				return &ContainsExpr{Str: args[0], Sub: args[1]}, nil
			}
		case "indexOf":
			if len(args) == 2 {
				return &MethodCallExpr{Target: args[0], Name: "IndexOf", Args: []Expr{args[1]}}, nil
			}
		case "padStart":
			if len(args) == 3 {
				var ch Expr
				if lit, ok := args[2].(*StringLit); ok && len(lit.Value) > 0 {
					ch = &RawExpr{Code: fmt.Sprintf("'%c'", []rune(lit.Value)[0]), Type: "char"}
				} else {
					ch = &CallExpr{Func: "Convert.ToChar", Args: []Expr{args[2]}}
				}
				tgt := args[0]
				if typeOfExpr(tgt) != "string" {
					tgt = &CallExpr{Func: "Convert.ToString", Args: []Expr{tgt}}
				}
				return &MethodCallExpr{Target: tgt, Name: "PadLeft", Args: []Expr{args[1], ch}}, nil
			} else if len(args) == 2 {
				ch := &RawExpr{Code: "' '", Type: "char"}
				tgt := args[0]
				if typeOfExpr(tgt) != "string" {
					tgt = &CallExpr{Func: "Convert.ToString", Args: []Expr{tgt}}
				}
				return &MethodCallExpr{Target: tgt, Name: "PadLeft", Args: []Expr{args[1], ch}}, nil
			}
		case "repeat":
			if len(args) == 2 {
				usesRepeat = true
				return &CallExpr{Func: "repeat", Args: args}, nil
			}
		case "num":
			if len(args) == 1 {
				usesBigRat = true
				usesBigInt = true
				return &CallExpr{Func: "_num", Args: []Expr{args[0]}}, nil
			}
		case "denom":
			if len(args) == 1 {
				usesBigRat = true
				usesBigInt = true
				return &CallExpr{Func: "_denom", Args: []Expr{args[0]}}, nil
			}
		case "split":
			if len(args) == 2 {
				return &MethodCallExpr{Target: args[0], Name: "Split", Args: []Expr{args[1]}}, nil
			}
		case "Object.keys":
			if len(args) == 1 {
				usesLinq = true
				code := fmt.Sprintf("((System.Collections.IDictionary)%s).Keys.Cast<string>()", exprString(args[0]))
				return &RawExpr{Code: code, Type: "string[]"}, nil
			}
		case "pow":
			if len(args) == 2 {
				code := fmt.Sprintf("(long)Math.Pow(%s, %s)", exprString(args[0]), exprString(args[1]))
				return &RawExpr{Code: code, Type: "long"}, nil
			}
		case "parseIntStr":
			switch len(args) {
			case 1:
				return &CallExpr{Func: "Convert.ToInt64", Args: []Expr{args[0]}}, nil
			case 2:
				return &CallExpr{Func: "Convert.ToInt64", Args: []Expr{args[0], args[1]}}, nil
			}
		case "toi":
			if len(args) == 1 {
				return &CallExpr{Func: "Convert.ToInt64", Args: []Expr{args[0]}}, nil
			}
		case "sha256":
			if len(args) == 1 {
				usesSHA256 = true
				return &CallExpr{Func: "_sha256", Args: args}, nil
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
		case "slice":
			if len(args) == 3 {
				usesSlice = true
				return &SliceExpr{Value: args[0], Start: args[1], End: args[2]}, nil
			}
		case "substring":
			if len(args) == 3 {
				return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
			}
		case "substr":
			if len(args) == 3 {
				return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
			}
		case "values":
			if len(args) == 1 {
				usesDict = true
				usesLinq = true
				return &ValuesExpr{Map: args[0]}, nil
			}
		case "first":
			if len(args) == 1 {
				t := typeOfExpr(args[0])
				if strings.HasSuffix(t, "[]") {
					elem := strings.TrimSuffix(t, "[]")
					return &RawExpr{Code: fmt.Sprintf("%s[0]", exprString(args[0])), Type: elem}, nil
				}
				usesLinq = true
				return &CallExpr{Func: "Enumerable.First", Args: args}, nil
			}
		case "json":
			if len(args) == 1 {
				usesJson = true
				inner := &CallExpr{Func: "JsonSerializer.Serialize", Args: []Expr{args[0]}}
				return &CallExpr{Func: "Console.WriteLine", Args: []Expr{inner}}, nil
			}
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Fetch != nil:
		urlExpr, err := compileExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		usesJson = true
		usesFetch = true
		if currentAssign != "" {
			if t, ok := varTypes[currentAssign]; ok && t != "" {
				fetchStructs[t] = true
				funRets["_fetch_"+t] = t
				if p.Fetch.With != nil {
					withExpr, err := compileExpr(p.Fetch.With)
					if err != nil {
						return nil, err
					}
					return &CallExpr{Func: "_fetch_" + t, Args: []Expr{urlExpr, withExpr}}, nil
				}
				return &CallExpr{Func: "_fetch_" + t, Args: []Expr{urlExpr}}, nil
			}
		}
		if p.Fetch.With != nil {
			withExpr, err := compileExpr(p.Fetch.With)
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr, withExpr}}, nil
		}
		return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int64(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Float != nil:
		return &FloatLit{Value: *p.Lit.Float}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Lit != nil && p.Lit.Null:
		return &RawExpr{Code: "null", Type: "object"}, nil
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
		if strings.HasSuffix(lit.ElemType, "[]") {
			inner := strings.TrimSuffix(lit.ElemType, "[]")
			for _, e := range elems {
				if l2, ok := e.(*ListLit); ok && len(l2.Elems) == 0 && (l2.ElemType == "" || l2.ElemType == "object[]") {
					l2.ElemType = inner
				}
			}
		}
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
			if ml, ok := val.(*MapLit); ok && len(ml.Items) == 0 {
				if ft, ok2 := st.Fields[f.Name]; ok2 {
					if mt, ok3 := ft.(types.MapType); ok3 {
						ml.KeyType = csTypeFromType(mt.Key)
						ml.ValType = csTypeFromType(mt.Value)
					}
				}
			}
			if list, ok := val.(*ListLit); ok && len(list.Elems) == 0 {
				if ft, ok2 := st.Fields[f.Name]; ok2 {
					if lt, ok3 := ft.(types.ListType); ok3 {
						list.ElemType = fmt.Sprintf("%s[]", csTypeFromType(lt.Elem))
					}
				}
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
		if len(p.Selector.Tail) == 0 {
			if st, ok := structTypes[name]; ok && len(st.Order) == 0 {
				return &StructLit{Name: st.Name, Fields: nil}, nil
			}
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
	savedRet := currentReturnType
	savedVoid := currentReturnVoid
	currentReturnType = csType(f.Return)
	currentReturnVoid = f.Return == nil
	defer func() { currentReturnType = savedRet; currentReturnVoid = savedVoid }()
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
		if lit, err := compileExpr(c.Pattern); err == nil {
			switch lit.(type) {
			case *IntLit, *StringLit, *BoolLit:
				res, err := compileExpr(c.Result)
				if err != nil {
					return nil, err
				}
				t := typeOfExpr(res)
				if t != "" {
					if retType == "object" {
						retType = t
					} else if retType != t {
						retType = "object"
					}
				}
				if i > 0 {
					body.WriteString(" else ")
				}
				body.WriteString("if (__t == ")
				var lbuf bytes.Buffer
				lit.emit(&lbuf)
				body.WriteString(lbuf.String())
				body.WriteString(") { ")
				var rbuf bytes.Buffer
				res.emit(&rbuf)
				body.WriteString("return " + rbuf.String() + "; }")
				continue
			}
		}

		if name, ok := simpleIdent(c.Pattern); ok {
			if st, ok2 := structTypes[name]; ok2 && len(st.Order) == 0 {
				res, err := compileExpr(c.Result)
				if err != nil {
					return nil, err
				}
				t := typeOfExpr(res)
				if i > 0 {
					body.WriteString(" else ")
				}
				body.WriteString("if (__t is " + name + ") { ")
				if t != "" {
					if retType == "object" {
						retType = t
					} else if retType != t {
						retType = "object"
					}
				}
				var rbuf bytes.Buffer
				res.emit(&rbuf)
				body.WriteString("return " + rbuf.String() + "; }")
				continue
			}
		}

		name, vars, ok := structPattern(c.Pattern)
		if !ok {
			return nil, fmt.Errorf("unsupported match pattern")
		}
		for j, v := range vars {
			if v == "_" {
				vars[j] = fmt.Sprintf("_unused_%d_%d", i, j)
			}
		}
		saved := make(map[string]string)
		st := structTypes[name]
		for j, v := range vars {
			saved[v] = varTypes[v]
			varTypes[v] = csTypeFromType(st.Fields[st.Order[j]])
		}
		res, err := compileExpr(c.Result)
		t := typeOfExpr(res)
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
		if t != "" {
			if retType == "object" {
				retType = t
			} else if retType != t {
				retType = "object"
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
	if prog.BenchMain {
		usesNow = true
		usesMem = true
		usesJson = true
		usesDict = true
		prog.Stmts = []Stmt{&BenchStmt{Name: "main", Body: prog.Stmts}}
	}

	var buf bytes.Buffer
	ts := gitTimestamp()
	fmt.Fprintf(&buf, "// Generated by Mochi %s on %s\n", strings.TrimSpace(version), ts)
	buf.WriteString("using System;\n")
	if usesDict {
		buf.WriteString("using System.Collections.Generic;\n")
	}
	// Always include LINQ for array operations.
	buf.WriteString("using System.Linq;\n")
	if usesJson {
		buf.WriteString("using System.Text.Json;\n")
	}
	if usesFetch {
		buf.WriteString("using System.Net.Http;\n")
	}
	if usesBigInt {
		buf.WriteString("using System.Numerics;\n")
	}
	if usesSHA256 {
		buf.WriteString("using System.Security.Cryptography;\n")
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
		fmt.Fprintf(&buf, "class %s {\n", st.Name)
		for _, f := range st.Fields {
			fmt.Fprintf(&buf, "    public %s %s;\n", f.Type, safeName(f.Name))
		}
		for _, m := range st.Methods {
			buf.WriteString("    ")
			m.emit(&buf)
			buf.WriteString("\n")
		}
		fmt.Fprintf(&buf, "    public override string ToString() => $\"%s {{", st.Name)
		for i, f := range st.Fields {
			if i > 0 {
				buf.WriteString(", ")
			}
			fmt.Fprintf(&buf, "%s = ", safeName(f.Name))
			if f.Type == "string" {
				fmt.Fprintf(&buf, "\\\"{%s}\\\"", safeName(f.Name))
			} else if f.Type == "double" {
				fmt.Fprintf(&buf, "{%s.ToString(\"0.0\")}", safeName(f.Name))
			} else {
				fmt.Fprintf(&buf, "{%s}", safeName(f.Name))
			}
		}
		buf.WriteString("}}\";\n")
		buf.WriteString("}\n")
	}

	buf.WriteString("#pragma warning disable CS0162\n")
	buf.WriteString("class Program {\n")
	if usesNow {
		buf.WriteString("\tstatic bool seededNow = false;\n")
		buf.WriteString("\tstatic long nowSeed = 0;\n")
		buf.WriteString("\tstatic long _now() {\n")
		buf.WriteString("\t\tif (!seededNow) {\n")
		buf.WriteString("\t\t\tvar s = Environment.GetEnvironmentVariable(\"MOCHI_NOW_SEED\");\n")
		buf.WriteString("\t\t\tif (long.TryParse(s, out var v)) {\n")
		buf.WriteString("\t\t\t\tnowSeed = v;\n")
		buf.WriteString("\t\t\t\tseededNow = true;\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\tif (seededNow) {\n")
		buf.WriteString("\t\t\tnowSeed = unchecked(nowSeed * 1664525 + 1013904223);\n")
		buf.WriteString("\t\t\tnowSeed %= 9223372036854775783L;\n")
		buf.WriteString("\t\t\tif (nowSeed < 0) nowSeed += 9223372036854775783L;\n")
		buf.WriteString("\t\t\treturn nowSeed;\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\treturn DateTime.UtcNow.Ticks / 100;\n")
		buf.WriteString("\t}\n")
	}
	if usesMem {
		buf.WriteString("\tstatic long _mem() {\n")
		buf.WriteString("\t\treturn GC.GetTotalAllocatedBytes(true);\n")
		buf.WriteString("\t}\n")
	}
	if usesLen {
		buf.WriteString("\tstatic long _len(object v) {\n")
		buf.WriteString("\t\tif (v is Array a) return a.Length;\n")
		buf.WriteString("\t\tif (v is string s) return s.Length;\n")
		buf.WriteString("\t\tif (v is System.Collections.ICollection c) return c.Count;\n")
		buf.WriteString("\t\treturn Convert.ToString(v).Length;\n")
		buf.WriteString("\t}\n")
	}
	if usesIdx {
		buf.WriteString("\tstatic T _idx<T>(T[] arr, long i) {\n")
		buf.WriteString("\t\tif (arr == null) return default(T);\n")
		buf.WriteString("\t\tif (i < 0) i += arr.Length;\n")
		buf.WriteString("\t\tif (i < 0 || i >= arr.Length) return default(T);\n")
		buf.WriteString("\t\treturn arr[(int)i];\n")
		buf.WriteString("\t}\n")
	}
	if usesSlice {
		buf.WriteString("\tstatic T[] _slice<T>(T[] arr, long start, long end) {\n")
		buf.WriteString("\t\tif (start < 0) start = 0;\n")
		buf.WriteString("\t\tif (end < 0) end = 0;\n")
		buf.WriteString("\t\tif (start > arr.Length) start = arr.Length;\n")
		buf.WriteString("\t\tif (end > arr.Length) end = arr.Length;\n")
		buf.WriteString("\t\tif (start > end) start = end;\n")
		buf.WriteString("\t\tvar len = end - start;\n")
		buf.WriteString("\t\tvar res = new T[len];\n")
		buf.WriteString("\t\tArray.Copy(arr, (int)start, res, 0, (int)len);\n")
		buf.WriteString("\t\treturn res;\n")
		buf.WriteString("\t}\n")
	}
	if usesSHA256 {
		buf.WriteString("\tstatic long[] _sha256(long[] bs) {\n")
		buf.WriteString("\t\tusing var sha = System.Security.Cryptography.SHA256.Create();\n")
		buf.WriteString("\t\tvar bytes = new byte[bs.Length];\n")
		buf.WriteString("\t\tfor (int i = 0; i < bs.Length; i++) bytes[i] = (byte)bs[i];\n")
		buf.WriteString("\t\tvar hash = sha.ComputeHash(bytes);\n")
		buf.WriteString("\t\tvar res = new long[hash.Length];\n")
		buf.WriteString("\t\tfor (int i = 0; i < hash.Length; i++) res[i] = hash[i];\n")
		buf.WriteString("\t\treturn res;\n")
		buf.WriteString("\t}\n")
		buf.WriteString("\tstatic long[] _sha256(string s) {\n")
		buf.WriteString("\t\tusing var sha = System.Security.Cryptography.SHA256.Create();\n")
		buf.WriteString("\t\tvar bytes = System.Text.Encoding.UTF8.GetBytes(s);\n")
		buf.WriteString("\t\tvar hash = sha.ComputeHash(bytes);\n")
		buf.WriteString("\t\tvar res = new long[hash.Length];\n")
		buf.WriteString("\t\tfor (int i = 0; i < hash.Length; i++) res[i] = hash[i];\n")
		buf.WriteString("\t\treturn res;\n")
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
		buf.WriteString("\t\tvar line = Console.ReadLine();\n")
		buf.WriteString("\t\treturn line == null ? \"\" : line;\n")
		buf.WriteString("\t}\n")
	}
	if usesAtoi {
		buf.WriteString("\tstatic long _atoi(object v) {\n")
		buf.WriteString("\t\tif (v == null) return 0;\n")
		buf.WriteString("\t\tif (v is long l) return l;\n")
		buf.WriteString("\t\tif (v is int i) return i;\n")
		buf.WriteString("\t\tif (v is double d) return (long)d;\n")
		buf.WriteString("\t\tif (v is bool b) return b ? 1L : 0L;\n")
		buf.WriteString("\t\tif (v is string s) {\n")
		buf.WriteString("\t\t\tif (long.TryParse(s, out var n)) return n;\n")
		buf.WriteString("\t\t\tif (double.TryParse(s, out var f)) return (long)f;\n")
		buf.WriteString("\t\t\treturn 0;\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\ttry { return Convert.ToInt64(v); } catch { return 0; }\n")
		buf.WriteString("\t}\n")
	}
	if usesBigRat {
		buf.WriteString("\tpublic class BigRat {\n")
		buf.WriteString("\t\tpublic BigInteger num;\n")
		buf.WriteString("\t\tpublic BigInteger den;\n")
		buf.WriteString("\t\tpublic BigRat(BigInteger n, BigInteger d) {\n")
		buf.WriteString("\t\t\tif (d.Sign < 0) { n = BigInteger.Negate(n); d = BigInteger.Negate(d); }\n")
		buf.WriteString("\t\t\tvar g = BigInteger.GreatestCommonDivisor(n, d);\n")
		buf.WriteString("\t\t\tnum = n / g; den = d / g;\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\tpublic override string ToString() => den.Equals(BigInteger.One) ? num.ToString() : num.ToString()+\"/\"+den.ToString();\n")
		buf.WriteString("\t}\n")
		buf.WriteString("\tstatic BigInteger _toBigInt(object x) {\n")
		buf.WriteString("\t\tif (x is BigInteger bi) return bi;\n")
		buf.WriteString("\t\tif (x is BigRat br) return br.num;\n")
		buf.WriteString("\t\tif (x is int ii) return new BigInteger(ii);\n")
		buf.WriteString("\t\tif (x is long ll) return new BigInteger(ll);\n")
		buf.WriteString("\t\tif (x is double dd) return new BigInteger((long)dd);\n")
		buf.WriteString("\t\tif (x is string ss) return BigInteger.Parse(ss);\n")
		buf.WriteString("\t\treturn BigInteger.Zero;\n")
		buf.WriteString("\t}\n")
		buf.WriteString("\tstatic BigRat _bigrat(object n, object d = null) {\n")
		buf.WriteString("\t\tif (n is BigRat br && d == null) return br;\n")
		buf.WriteString("\t\tvar nn = _toBigInt(n);\n")
		buf.WriteString("\t\tvar dd = d == null ? BigInteger.One : _toBigInt(d);\n")
		buf.WriteString("\t\treturn new BigRat(nn, dd);\n")
		buf.WriteString("\t}\n")
		buf.WriteString("\tstatic BigRat _add(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.den + y.num*x.den, x.den*y.den); }\n")
		buf.WriteString("\tstatic BigRat _sub(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.den - y.num*x.den, x.den*y.den); }\n")
		buf.WriteString("\tstatic BigRat _mul(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.num, x.den*y.den); }\n")
		buf.WriteString("\tstatic BigRat _div(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.den, x.den*y.num); }\n")
		buf.WriteString("\tstatic BigRat _neg(object a) { var x=_bigrat(a, null); return new BigRat(BigInteger.Negate(x.num), x.den); }\n")
		buf.WriteString("\tstatic BigInteger _num(object x) { return x is BigRat br ? br.num : _toBigInt(x); }\n")
		buf.WriteString("\tstatic BigInteger _denom(object x) { return x is BigRat br ? br.den : BigInteger.One; }\n")
	}
	if usesRepeat {
		buf.WriteString("\tstatic string repeat(string s, long n) {\n")
		buf.WriteString("\t\treturn string.Concat(Enumerable.Repeat(s, (int)n));\n")
		buf.WriteString("\t}\n")
	}
	if usesMod {
		buf.WriteString("\tstatic long _mod(long a, long b) {\n")
		buf.WriteString("\t\tif (b == 0) return 0;\n")
		buf.WriteString("\t\tvar r = a % b;\n")
		buf.WriteString("\t\tif ((r < 0 && b > 0) || (r > 0 && b < 0)) r += b;\n")
		buf.WriteString("\t\treturn r;\n")
		buf.WriteString("\t}\n")
	}
	if usesFetch {
		buf.WriteString("\tstatic object _fetch(string url) {\n")
		buf.WriteString("\t\tif (url.StartsWith(\"http://\") || url.StartsWith(\"https://\")) {\n")
		buf.WriteString("\t\t\tusing var client = new HttpClient();\n")
		buf.WriteString("\t\t\tvar resp = client.GetAsync(url).Result;\n")
		buf.WriteString("\t\t\tusing var stream = resp.Content.ReadAsStream();\n")
		buf.WriteString("\t\t\tvar opts = new JsonSerializerOptions{ IncludeFields = true };\n")
		buf.WriteString("\t\t\treturn JsonSerializer.Deserialize<object>(stream, opts);\n")
		buf.WriteString("\t\t} else {\n")
		buf.WriteString("\t\t\tvar path = url;\n")
		buf.WriteString("\t\t\tif (!System.IO.Path.IsPathRooted(path)) {\n")
		buf.WriteString("\t\t\t\tvar root = Environment.GetEnvironmentVariable(\"MOCHI_ROOT\");\n")
		buf.WriteString("\t\t\t\tif (!string.IsNullOrEmpty(root)) {\n")
		buf.WriteString("\t\t\t\t\tvar combined = System.IO.Path.Combine(root, path.Replace('/', System.IO.Path.DirectorySeparatorChar));\n")
		buf.WriteString("\t\t\t\t\tif (System.IO.File.Exists(combined)) path = combined;\n")
		buf.WriteString("\t\t\t\t}\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\t\tusing var stream = System.IO.File.OpenRead(path);\n")
		buf.WriteString("\t\t\tvar opts = new JsonSerializerOptions{ IncludeFields = true };\n")
		buf.WriteString("\t\t\treturn JsonSerializer.Deserialize<object>(stream, opts);\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t}\n")
		names := make([]string, 0, len(fetchStructs))
		for n := range fetchStructs {
			names = append(names, n)
		}
		sort.Strings(names)
		buf.WriteString("\tstatic string _fetch(string url, object optsObj) {\n")
		buf.WriteString("\t\tif (!(optsObj is System.Collections.IDictionary o)) return Convert.ToString(_fetch(url));\n")
		buf.WriteString("\t\tvar tmp = new Dictionary<string, object>();\n")
		buf.WriteString("\t\tforeach (System.Collections.DictionaryEntry kv in o) {\n")
		buf.WriteString("\t\t\tvar k = kv.Key.ToString();\n")
		buf.WriteString("\t\t\tvar i = k.IndexOf('_');\n")
		buf.WriteString("\t\t\tif (i >= 0) k = k.Substring(0, i);\n")
		buf.WriteString("\t\t\ttmp[k] = kv.Value;\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\tvar method = tmp.ContainsKey(\"method\") ? Convert.ToString(tmp[\"method\"]) : \"GET\";\n")
		buf.WriteString("\t\tDictionary<string, string>? headers = null;\n")
		buf.WriteString("\t\tif (tmp.ContainsKey(\"headers\")) {\n")
		buf.WriteString("\t\t\theaders = new Dictionary<string, string>();\n")
		buf.WriteString("\t\t\tif (tmp[\"headers\"] is System.Collections.IDictionary hd) {\n")
		buf.WriteString("\t\t\t\tforeach (System.Collections.DictionaryEntry kv in hd) {\n")
		buf.WriteString("\t\t\t\t\theaders[kv.Key.ToString()] = kv.Value.ToString();\n")
		buf.WriteString("\t\t\t\t}\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\tobject bodyObj = null;\n")
		buf.WriteString("\t\tif (tmp.ContainsKey(\"body\")) bodyObj = tmp[\"body\"];\n")
		buf.WriteString("\t\tif (url.StartsWith(\"http://\") || url.StartsWith(\"https://\")) {\n")
		buf.WriteString("\t\t\tusing var client = new HttpClient();\n")
		buf.WriteString("\t\t\tif (method == \"POST\") {\n")
		buf.WriteString("\t\t\t\tvar jsonBody = bodyObj == null ? \"\" : JsonSerializer.Serialize(bodyObj);\n")
		buf.WriteString("\t\t\t\tvar content = new StringContent(jsonBody);\n")
		buf.WriteString("\t\t\t\tif (headers != null) foreach (var kv in headers) content.Headers.TryAddWithoutValidation(kv.Key, kv.Value);\n")
		buf.WriteString("\t\t\t\tvar respPost = client.PostAsync(url, content).Result;\n")
		buf.WriteString("\t\t\t\treturn respPost.Content.ReadAsStringAsync().Result;\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\t\tif (headers != null) foreach (var kv in headers) client.DefaultRequestHeaders.TryAddWithoutValidation(kv.Key, kv.Value);\n")
		buf.WriteString("\t\t\tvar resp = client.GetAsync(url).Result;\n")
		buf.WriteString("\t\t\treturn resp.Content.ReadAsStringAsync().Result;\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\treturn Convert.ToString(_fetch(url));\n")
		buf.WriteString("\t}\n")
		for _, n := range names {
			buf.WriteString("\tstatic " + n + " _fetch_" + n + "(string url) {\n")
			buf.WriteString("\t\tif (url.StartsWith(\"http://\") || url.StartsWith(\"https://\")) {\n")
			buf.WriteString("\t\t\tusing var client = new HttpClient();\n")
			buf.WriteString("\t\t\tvar resp = client.GetAsync(url).Result;\n")
			buf.WriteString("\t\t\tusing var stream = resp.Content.ReadAsStream();\n")
			buf.WriteString("\t\t\tvar opts = new JsonSerializerOptions{ IncludeFields = true };\n")
			buf.WriteString("\t\t\tvar res = JsonSerializer.Deserialize<" + n + ">(stream, opts);\n")
			buf.WriteString("\t\t\treturn res == null ? new " + n + "() : res;\n")
			buf.WriteString("\t\t} else {\n")
			buf.WriteString("\t\t\tvar path = url;\n")
			buf.WriteString("\t\t\tif (!System.IO.Path.IsPathRooted(path)) {\n")
			buf.WriteString("\t\t\t\tvar root = Environment.GetEnvironmentVariable(\"MOCHI_ROOT\");\n")
			buf.WriteString("\t\t\t\tif (!string.IsNullOrEmpty(root)) {\n")
			buf.WriteString("\t\t\t\t\tvar combined = System.IO.Path.Combine(root, path.Replace('/', System.IO.Path.DirectorySeparatorChar));\n")
			buf.WriteString("\t\t\t\t\tif (System.IO.File.Exists(combined)) path = combined;\n")
			buf.WriteString("\t\t\t\t}\n")
			buf.WriteString("\t\t\t}\n")
			buf.WriteString("\t\t\tusing var stream = System.IO.File.OpenRead(path);\n")
			buf.WriteString("\t\t\tvar opts = new JsonSerializerOptions{ IncludeFields = true };\n")
			buf.WriteString("\t\t\tvar res = JsonSerializer.Deserialize<" + n + ">(stream, opts);\n")
			buf.WriteString("\t\t\treturn res == null ? new " + n + "() : res;\n")
			buf.WriteString("\t\t}\n")
			buf.WriteString("\t}\n")
			buf.WriteString("\tstatic " + n + " _fetch_" + n + "(string url, object optsObj) {\n")
			buf.WriteString("\t\tvar str = _fetch(url, optsObj);\n")
			buf.WriteString("\t\tvar opts = new JsonSerializerOptions{ IncludeFields = true };\n")
			buf.WriteString("\t\tvar res = JsonSerializer.Deserialize<" + n + ">(str, opts);\n")
			buf.WriteString("\t\treturn res == null ? new " + n + "() : res;\n")
			buf.WriteString("\t}\n")
		}
	}
	buf.WriteString("\tstatic string _substr(string s, long start, long end) {\n")
	buf.WriteString("\t\tif (start < 0) start = 0;\n")
	buf.WriteString("\t\tif (end < 0) end = 0;\n")
	buf.WriteString("\t\tif (start > s.Length) start = s.Length;\n")
	buf.WriteString("\t\tif (end > s.Length) end = s.Length;\n")
	buf.WriteString("\t\tif (start > end) start = end;\n")
	buf.WriteString("\t\treturn s.Substring((int)start, (int)(end - start));\n")
	buf.WriteString("\t}\n")
	if usesFmt {
		buf.WriteString(`static string _fmt(object v) {
if (v is Array a) {
var parts = new List<string>();
foreach (var x in a) parts.Add(_fmt(x));
return "[" + string.Join(", ", parts) + "]";
}
if (v is System.Collections.IDictionary d) {
var keys = new List<string>();
foreach (var k in d.Keys) keys.Add(k.ToString());
keys.Sort();
var parts = new List<string>();
foreach (var k in keys) parts.Add(k + ":" + _fmt(d[k]));
return "map[" + string.Join(", ", parts) + "]";
}
if (v is System.Collections.IEnumerable e && !(v is string)) {
var parts = new List<string>();
foreach (var x in e) parts.Add(_fmt(x));
return string.Join(", ", parts);
}
if (v is string s) return "\"" + s.Replace("\"", "\\\"") + "\"";
if (v is bool b) return b ? "true" : "false";
return Convert.ToString(v);
}
`)
		buf.WriteString(`static string _fmtStr(object v) {
if (v is Array a) {
var parts = new List<string>();
foreach (var x in a) parts.Add(_fmtStr(x));
return "[" + string.Join(" ", parts) + "]";
}
if (v is System.Collections.IDictionary d) {
var keys = new List<string>();
foreach (var k in d.Keys) keys.Add(k.ToString());
keys.Sort();
var parts = new List<string>();
foreach (var k in keys) parts.Add(k + ":" + _fmtStr(d[k]));
return "map[" + string.Join(" ", parts) + "]";
}
if (v is System.Collections.IEnumerable e && !(v is string)) {
var parts = new List<string>();
foreach (var x in e) parts.Add(_fmtStr(x));
return string.Join(" ", parts);
}
if (v is string s) return "\"" + s.Replace("\"", "\\\"") + "\"";
if (v is bool b) return b ? "true" : "false";
return Convert.ToString(v);
}
`)
		buf.WriteString(`public static string _fmtTop(object v) {
if (v is Array a && a.Length > 0 && a.GetValue(0) is Array) {
    var parts = new List<string>();
    foreach (var x in a) parts.Add(_fmt(x));
    return string.Join(" ", parts);
}
if (v is string s) return s;
return _fmt(v);
}
`)
	}
	for _, g := range prog.Globals {
		buf.WriteString("\tstatic ")
		t := typeOfExpr(g.Value)
		if vt, ok := finalVarTypes[g.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
			t = vt
		} else if vt, ok := varTypes[g.Name]; ok && vt != "" && !strings.HasPrefix(vt, "fn/") {
			if t == "" || t == "object" || t != vt {
				t = vt
			}
		}
		name := safeName(g.Name)
		if t == "" || t == "object" {
			fmt.Fprintf(&buf, "dynamic %s = ", name)
		} else {
			fmt.Fprintf(&buf, "%s %s = ", t, name)
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
	buf.WriteString("\t\t_ = __name__;\n")
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
	// fix casting of inline lambda expressions
	re := regexp.MustCompile(`\(object\)\(object ([A-Za-z0-9_]+)\) => {`)
	s = re.ReplaceAllString(s, "(object)((object $1) => {")
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
			return &IntLit{Value: int64(val)}
		}
		return &FloatLit{Value: val}
	case int, int64:
		return &IntLit{Value: reflect.ValueOf(val).Int()}
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

func compileStructMethod(structName string, m types.Method) (*Function, error) {
	params := make([]string, len(m.Decl.Params))
	ptypes := make([]string, len(m.Decl.Params))
	for i, p := range m.Decl.Params {
		params[i] = p.Name
		ptypes[i] = csType(p.Type)
	}
	savedVT := cloneStringMap(varTypes)
	savedStr := cloneBoolMap(stringVars)
	for fname, ft := range structTypes[structName].Fields {
		t := csTypeFromType(ft)
		varTypes[fname] = t
		if t == "string" {
			stringVars[fname] = true
		}
	}
	for i, p := range params {
		varTypes[p] = ptypes[i]
		if ptypes[i] == "string" {
			stringVars[p] = true
		}
	}
	var body []Stmt
	for _, st := range m.Decl.Body {
		s, err := compileStmt(nil, st)
		if err != nil {
			varTypes = savedVT
			stringVars = savedStr
			return nil, err
		}
		if s != nil {
			body = append(body, s)
		}
	}
	varTypes = savedVT
	stringVars = savedStr
	retType := csType(m.Decl.Return)
	if m.Decl.Return == nil {
		retType = ""
	}
	return &Function{Name: m.Decl.Name, Params: params, ParamTypes: ptypes, ReturnType: retType, Body: body, Receiver: structName}, nil
}
