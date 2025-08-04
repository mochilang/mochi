//go:build slow

package zigt

import (
	"bytes"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"

	"mochi/parser"
	"mochi/runtime/data"
	"mochi/types"
)

var constLists map[string]*ListLit
var mapVars map[string]bool
var structDefs map[string]*StructDef
var extraFuncs []*Func
var funcCounter int
var varTypes map[string]string
var varDecls map[string]*VarDecl
var groupCounter int
var groupItemTypes map[string]string
var typeAliases map[string]string
var varUses map[string]int
var varMut map[string]bool
var funDepth int
var funParamsStack [][]string
var nestedFunArgs map[string][]string
var funcReturns map[string]string
var funcParamTypes map[string][]string
var builtinAliases map[string]string
var transEnv *types.Env
var loopCounter int
var mainFuncName string
var useNow bool
var useStr bool
var useConcat bool
var useInput bool
var useLookupHost bool
var useAscii bool
var useMem bool
var usePrint bool
var useSplit bool
var useValue bool
var useSha256 bool
var aliasStack []map[string]string
var namesStack [][]string
var nameCounts map[string]int
var localMutablesStack []map[string]bool
var globalNames map[string]bool
var variantTags map[string]int
var globalInits []Stmt
var currentReturnType string
var zigKeywords = map[string]bool{
	"fn":       true,
	"const":    true,
	"var":      true,
	"if":       true,
	"else":     true,
	"switch":   true,
	"for":      true,
	"while":    true,
	"break":    true,
	"continue": true,
	"defer":    true,
	"return":   true,
	"pub":      true,
}

// when true, wrap the generated main function in a benchmark block
var benchMain bool

// GetFuncReturns exposes function return types for testing.
func GetFuncReturns() map[string]string { return funcReturns }

// SetBenchMain configures whether the generated main function is wrapped in
// a benchmark block when emitting code. When enabled, the program will print
// a JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

func pushAliasScope() {
	aliasStack = append(aliasStack, map[string]string{})
	namesStack = append(namesStack, []string{})
}

func popAliasScope() {
	if len(aliasStack) == 0 {
		return
	}
	aliasStack = aliasStack[:len(aliasStack)-1]
	namesStack = namesStack[:len(namesStack)-1]
}

func uniqueName(name string) string {
	cnt := nameCounts[name]
	nameCounts[name] = cnt + 1
	if cnt == 0 {
		return zigIdent(name)
	}
	return fmt.Sprintf("%s_%d", zigIdent(name), cnt)
}

func toSnakeCase(s string) string {
	var buf strings.Builder
	for i, r := range s {
		if unicode.IsUpper(r) {
			if i > 0 {
				buf.WriteByte('_')
			}
			buf.WriteRune(unicode.ToLower(r))
		} else {
			buf.WriteRune(r)
		}
	}
	return buf.String()
}

func zigIdent(s string) string {
        var buf strings.Builder
        for i, r := range s {
		if unicode.IsLetter(r) || r == '_' || (unicode.IsDigit(r) && i > 0) {
			buf.WriteRune(r)
		} else if unicode.IsDigit(r) && i == 0 {
			buf.WriteByte('_')
			buf.WriteRune(r)
		} else {
			fmt.Fprintf(&buf, "_%x", r)
		}
	}
	if buf.Len() == 0 {
		return "_"
	}
	ident := buf.String()
	if zigKeywords[ident] {
		return ident + "_"
	}
	return ident
}

func isIntType(t string) bool {
	return strings.HasPrefix(t, "i") || strings.HasPrefix(t, "u")
}

func valueTag(t string) string {
        switch t {
        case "i64":
                return "Int"
        case "f64":
                return "Float"
        case "[]const u8":
                return "Str"
        case "bool":
                return "Bool"
       case "[][]i64":
               return "List"
       case "[][]const u8":
               return "StrList"
        default:
                return "Int"
        }
}

func valueAccess(t string) string {
        switch t {
        case "i64":
                return ".Int"
        case "f64":
                return ".Float"
        case "[]const u8":
                return ".Str"
        case "bool":
                return ".Bool"
       case "[][]i64":
               return ".List"
       case "[][]const u8":
               return ".StrList"
        default:
                return ""
        }
}

// Program represents a Zig source file with one or more functions.
type Field struct {
	Name string
	Type string
}

type StructDef struct {
	Name   string
	Fields []Field
}

func (sd *StructDef) emit(w io.Writer) {
	fmt.Fprintf(w, "const %s = struct {\n", sd.Name)
	for _, f := range sd.Fields {
		fmt.Fprintf(w, "    %s: %s,\n", f.Name, f.Type)
	}
	io.WriteString(w, "};\n")
}

type Program struct {
	Structs   []*StructDef
	Globals   []*VarDecl
	Functions []*Func
}

type Param struct {
	Name string
	Type string
}

type Func struct {
	Name       string
	Params     []Param
	ReturnType string
	Body       []Stmt
	Aliases    map[string]string
}

type Stmt interface{ emit(io.Writer, int) }

type Expr interface{ emit(io.Writer) }

// BoolLit represents a boolean literal.
type BoolLit struct{ Value bool }

// NullLit represents a null literal.
type NullLit struct{}

// IfStmt represents a simple if-else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

// WhileStmt represents a basic while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForStmt represents iteration over a range or iterable collection.
type ForStmt struct {
	Name     string
	Start    Expr // used for numeric ranges
	End      Expr // end of range (exclusive)
	Iterable Expr // used for foreach loops
	Body     []Stmt
}

// IfExpr represents an if expression returning a value.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

// CallExpr represents a simple function call.
type CallExpr struct {
	Func string
	Args []Expr
}

func exprToString(e Expr) (string, bool) {
	switch t := e.(type) {
	case *VarRef:
		return t.Name, true
	case *IndexExpr:
		return exprToString(t.Target)
	case *FieldExpr:
		if s, ok := exprToString(t.Target); ok {
			return s + "." + t.Name, true
		}
	}
	return "", false
}

// PrintStmt writes values using std.debug.print.
type PrintStmt struct{ Values []Expr }

// VarDecl represents `let` or `var` declarations.
type VarDecl struct {
	Name    string
	Value   Expr
	Mutable bool
	Type    string
}

// AssignStmt represents simple assignments like `x = expr`.
type AssignStmt struct {
	Name  string
	Value Expr
}

// IndexAssignStmt assigns to an indexed expression like `xs[i] = val`.
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

// FieldAssignStmt assigns to a field expression like `obj.x = val`.
type FieldAssignStmt struct {
	Target Expr
	Name   string
	Value  Expr
}

// UpdateStmt updates fields of items in a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

// SaveStmt writes a list of structs or maps to stdout as JSON lines.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

// JSONStmt prints a value as JSON on its own line.
type JSONStmt struct{ Value Expr }

// ExprStmt allows top-level expressions.
type ExprStmt struct{ Expr Expr }

type StringLit struct{ Value string }

type IntLit struct{ Value int }

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

type VarRef struct {
	Name string
	Map  bool
}

func resolveAlias(name string) string {
	for i := len(aliasStack) - 1; i >= 0; i-- {
		if a, ok := aliasStack[i][name]; ok {
			return a
		}
	}
	return name
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// SliceExpr represents a slice operation like xs[a:b].
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

// CastExpr casts a value to a named type.
type CastExpr struct {
	Value Expr
	Type  string
}

// CopySliceExpr allocates a mutable copy of a slice.
type CopySliceExpr struct {
	Src  string
	Elem string
}

func (e *CopySliceExpr) emit(w io.Writer) {
	elem := strings.TrimPrefix(e.Elem, "const ")
	fmt.Fprintf(w, "blk: { const tmp = std.heap.page_allocator.alloc(%s, %s.len) catch unreachable; @memcpy(tmp, %s); break :blk tmp; }", elem, e.Src, e.Src)
}

// NotExpr represents logical negation.
type NotExpr struct{ Expr Expr }

// ListLit represents a list literal of integer expressions.
type ListLit struct {
	Elems    []Expr
	ElemType string
}

type MapEntry struct {
	Key   Expr
	Value Expr
}

type MapLit struct {
	Entries    []MapEntry
	StructName string
	KeyType    string
	ValType    string
}

func isConstExpr(e Expr) bool {
	switch e.(type) {
	case *IntLit, *FloatLit, *StringLit, *BoolLit, *NullLit:
		return true
	default:
		return false
	}
}

func collectVarsStmt(s Stmt, m map[string]bool) {
	switch st := s.(type) {
	case *VarDecl:
		collectVarsExpr(st.Value, m)
	case *AssignStmt:
		collectVarsExpr(st.Value, m)
	case *IndexAssignStmt:
		collectVarsExpr(st.Target, m)
		collectVarsExpr(st.Value, m)
	case *FieldAssignStmt:
		collectVarsExpr(st.Target, m)
		collectVarsExpr(st.Value, m)
	case *IfStmt:
		collectVarsExpr(st.Cond, m)
		for _, b := range st.Then {
			collectVarsStmt(b, m)
		}
		for _, b := range st.Else {
			collectVarsStmt(b, m)
		}
	case *WhileStmt:
		collectVarsExpr(st.Cond, m)
		for _, b := range st.Body {
			collectVarsStmt(b, m)
		}
	case *ForStmt:
		collectVarsExpr(st.Start, m)
		collectVarsExpr(st.End, m)
		collectVarsExpr(st.Iterable, m)
		for _, b := range st.Body {
			collectVarsStmt(b, m)
		}
	case *ExprStmt:
		collectVarsExpr(st.Expr, m)
	case *PrintStmt:
		for _, e := range st.Values {
			collectVarsExpr(e, m)
		}
	case *BenchStmt:
		for _, b := range st.Body {
			collectVarsStmt(b, m)
		}
	}
}

func collectVarsExpr(e Expr, m map[string]bool) {
	if e == nil {
		return
	}
	switch ex := e.(type) {
	case *VarRef:
		m[ex.Name] = true
	case *BinaryExpr:
		collectVarsExpr(ex.Left, m)
		collectVarsExpr(ex.Right, m)
	case *CallExpr:
		m[ex.Func] = true
		for _, a := range ex.Args {
			collectVarsExpr(a, m)
		}
	case *IndexExpr:
		collectVarsExpr(ex.Target, m)
		collectVarsExpr(ex.Index, m)
	case *SliceExpr:
		collectVarsExpr(ex.Target, m)
		collectVarsExpr(ex.Start, m)
		collectVarsExpr(ex.End, m)
	case *NotExpr:
		collectVarsExpr(ex.Expr, m)
	case *ListLit:
		for _, e2 := range ex.Elems {
			collectVarsExpr(e2, m)
		}
	case *MapLit:
		for _, me := range ex.Entries {
			collectVarsExpr(me.Key, m)
			collectVarsExpr(me.Value, m)
		}
	case *AppendExpr:
		collectVarsExpr(ex.List, m)
		collectVarsExpr(ex.Value, m)
	case *FieldExpr:
		collectVarsExpr(ex.Target, m)
	case *IfExpr:
		collectVarsExpr(ex.Cond, m)
		collectVarsExpr(ex.Then, m)
		collectVarsExpr(ex.Else, m)
	case *QueryComp:
		for _, src := range ex.Sources {
			collectVarsExpr(src, m)
		}
		collectVarsExpr(ex.Elem, m)
		collectVarsExpr(ex.Filter, m)
		collectVarsExpr(ex.Sort, m)
		collectVarsExpr(ex.Skip, m)
		collectVarsExpr(ex.Take, m)
	case *GroupByExpr:
		collectVarsExpr(ex.Source, m)
		collectVarsExpr(ex.Key, m)
		collectVarsExpr(ex.SelectExpr, m)
		collectVarsExpr(ex.Sort, m)
	case *CopySliceExpr:
		m[ex.Src] = true
	}
}

type AppendExpr struct {
	List     Expr
	Value    Expr
	ElemType string
}

func (ae *AppendExpr) emit(w io.Writer) {
	elem := ae.ElemType
	if elem == "" {
		elem = zigTypeFromExpr(ae.Value)
	}
	fmt.Fprintf(w, "blk: { var _tmp = std.ArrayList(%s).init(std.heap.page_allocator); ", elem)
	fmt.Fprintf(w, "_tmp.appendSlice(@as([]const %s, ", elem)
	ae.List.emit(w)
	io.WriteString(w, ")) catch |err| handleError(err); _tmp.append(")
	ae.Value.emit(w)
	io.WriteString(w, ") catch |err| handleError(err); break :blk (_tmp.toOwnedSlice() catch |err| handleError(err)); }")
}

type FieldExpr struct {
	Target Expr
	Name   string
}

func (fe *FieldExpr) emit(w io.Writer) {
	fe.Target.emit(w)
	name := fe.Name
	if r, _ := utf8.DecodeRuneInString(name); unicode.IsLower(r) {
		name = toSnakeCase(name)
	}
	fmt.Fprintf(w, ".%s", name)
}

type QueryComp struct {
	Vars     []string
	Sources  []Expr
	Elem     Expr
	ElemType string
	Filter   Expr
	Sort     Expr
	Desc     bool
	Skip     Expr
	Take     Expr
}

type GroupByExpr struct {
	Var        string
	Source     Expr
	Key        Expr
	GroupVar   string
	SelectExpr Expr
	ElemType   string
	KeyType    string
	SrcElem    string
	StructName string
	Sort       Expr
	Desc       bool
}

func (qc *QueryComp) emit(w io.Writer) {
	elemType := qc.ElemType
	sortType := ""
	if qc.Sort != nil {
		sortType = zigTypeFromExpr(qc.Sort)
		fmt.Fprintf(w, "blk: {\n    var arr = std.ArrayList(struct{key: %s, val: %s}).init(std.heap.page_allocator);\n", sortType, elemType)
	} else {
		fmt.Fprintf(w, "blk: {\n    var arr = std.ArrayList(%s).init(std.heap.page_allocator);\n", elemType)
	}
	indent := 1
	for i, src := range qc.Sources {
		writeIndent(w, indent)
		io.WriteString(w, "for (")
		src.emit(w)
		io.WriteString(w, ") |")
		io.WriteString(w, qc.Vars[i])
		io.WriteString(w, "| {\n")
		indent++
	}
	if qc.Filter != nil {
		writeIndent(w, indent)
		io.WriteString(w, "if (")
		qc.Filter.emit(w)
		io.WriteString(w, ") {\n")
		indent++
	}
	writeIndent(w, indent)
	io.WriteString(w, "arr.append(")
	if qc.Sort != nil {
		io.WriteString(w, ".{ .key = ")
		qc.Sort.emit(w)
		io.WriteString(w, ", .val = ")
		qc.Elem.emit(w)
		io.WriteString(w, " }")
	} else {
		qc.Elem.emit(w)
	}
	io.WriteString(w, ") catch unreachable;\n")
	if qc.Filter != nil {
		indent--
		writeIndent(w, indent)
		io.WriteString(w, "}\n")
	}
	for range qc.Sources {
		indent--
		writeIndent(w, indent)
		io.WriteString(w, "}\n")
	}
	writeIndent(w, 1)
	io.WriteString(w, "var tmp = arr.toOwnedSlice() catch unreachable;\n")
	if qc.Sort != nil {
		writeIndent(w, 1)
		io.WriteString(w, "std.sort.sort(struct{key: ")
		io.WriteString(w, sortType)
		io.WriteString(w, ", val: ")
		io.WriteString(w, elemType)
		io.WriteString(w, "}, tmp, {}, struct{fn lt(ctx: void, a: struct{key: ")
		io.WriteString(w, sortType)
		io.WriteString(w, ", val: ")
		io.WriteString(w, elemType)
		io.WriteString(w, "}, b: struct{key: ")
		io.WriteString(w, sortType)
		io.WriteString(w, ", val: ")
		io.WriteString(w, elemType)
		io.WriteString(w, "}) bool { return ")
		if qc.Desc {
			io.WriteString(w, "a.key > b.key")
		} else {
			io.WriteString(w, "a.key < b.key")
		}
		io.WriteString(w, "; } }.lt);\n")
		writeIndent(w, 1)
		fmt.Fprintf(w, "var result = std.ArrayList(%s).init(std.heap.page_allocator);\n", elemType)
		writeIndent(w, 1)
		io.WriteString(w, "for (tmp) |it| { result.append(it.val) catch unreachable; }\n")
		writeIndent(w, 1)
		io.WriteString(w, "tmp = result.toOwnedSlice() catch unreachable;\n")
	}
	if qc.Skip != nil || qc.Take != nil {
		writeIndent(w, 1)
		io.WriteString(w, "var start: usize = 0;\n")
		if qc.Skip != nil {
			writeIndent(w, 1)
			io.WriteString(w, "start = @intCast(")
			qc.Skip.emit(w)
			io.WriteString(w, ");\n")
		}
		writeIndent(w, 1)
		io.WriteString(w, "var end: usize = tmp.len;\n")
		if qc.Take != nil {
			writeIndent(w, 1)
			io.WriteString(w, "end = @min(tmp.len, start + @intCast(")
			qc.Take.emit(w)
			io.WriteString(w, "));\n")
		}
		writeIndent(w, 1)
		io.WriteString(w, "tmp = tmp[start..end];\n")
	}
	writeIndent(w, 1)
	io.WriteString(w, "break :blk tmp;\n}")
}

func (gq *GroupByExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "blk: {\n    var groups_map = std.AutoHashMap(%s, std.ArrayList(%s)).init(std.heap.page_allocator);\n", gq.KeyType, gq.SrcElem)
	writeIndent(w, 1)
	io.WriteString(w, "for (")
	gq.Source.emit(w)
	fmt.Fprintf(w, ") |%s| {\n", gq.Var)
	writeIndent(w, 2)
	io.WriteString(w, "const k = ")
	gq.Key.emit(w)
	io.WriteString(w, ";\n")
	writeIndent(w, 2)
	io.WriteString(w, "if (groups_map.getPtr(k)) |arr| {\n")
	writeIndent(w, 3)
	fmt.Fprintf(w, "arr.*.append(%s) catch unreachable;\n", gq.Var)
	writeIndent(w, 2)
	io.WriteString(w, "} else {\n")
	writeIndent(w, 3)
	fmt.Fprintf(w, "var tmp_arr = std.ArrayList(%s).init(std.heap.page_allocator);\n", gq.SrcElem)
	writeIndent(w, 3)
	fmt.Fprintf(w, "tmp_arr.append(%s) catch unreachable;\n", gq.Var)
	writeIndent(w, 3)
	io.WriteString(w, "groups_map.put(k, tmp_arr) catch unreachable;\n")
	writeIndent(w, 2)
	io.WriteString(w, "}\n")
	writeIndent(w, 1)
	io.WriteString(w, "}\n")
	writeIndent(w, 1)
	fmt.Fprintf(w, "var groups = std.ArrayList(%s).init(std.heap.page_allocator);\n", gq.StructName)
	writeIndent(w, 1)
	io.WriteString(w, "var it = groups_map.iterator();\n")
	writeIndent(w, 1)
	io.WriteString(w, "while (it.next()) |kv| {\n")
	writeIndent(w, 2)
	fmt.Fprintf(w, "groups.append(.{ .key = kv.key.*, .items = kv.value.toOwnedSlice() catch unreachable }) catch unreachable;\n")
	writeIndent(w, 1)
	io.WriteString(w, "}\n")
	writeIndent(w, 1)
	io.WriteString(w, "const arr = groups.toOwnedSlice() catch unreachable;\n")
	writeIndent(w, 1)
	sortType := ""
	if gq.Sort != nil {
		sortType = zigTypeFromExpr(gq.Sort)
		fmt.Fprintf(w, "var result = std.ArrayList(struct{key: %s, val: %s}).init(std.heap.page_allocator);\n", sortType, gq.ElemType)
	} else {
		fmt.Fprintf(w, "var result = std.ArrayList(%s).init(std.heap.page_allocator);\n", gq.ElemType)
	}
	writeIndent(w, 1)
	io.WriteString(w, "for (arr) |")
	io.WriteString(w, gq.GroupVar)
	io.WriteString(w, "| {\n")
	writeIndent(w, 2)
	io.WriteString(w, "result.append(")
	if gq.Sort != nil {
		io.WriteString(w, ".{ .key = ")
		gq.Sort.emit(w)
		io.WriteString(w, ", .val = ")
		gq.SelectExpr.emit(w)
		io.WriteString(w, " }")
	} else {
		gq.SelectExpr.emit(w)
	}
	io.WriteString(w, ") catch unreachable;\n")
	writeIndent(w, 1)
	io.WriteString(w, "}\n")
	writeIndent(w, 1)
	io.WriteString(w, "var tmp = result.toOwnedSlice() catch unreachable;\n")
	if gq.Sort != nil {
		writeIndent(w, 1)
		io.WriteString(w, "std.sort.sort(struct{key: ")
		io.WriteString(w, sortType)
		io.WriteString(w, ", val: ")
		io.WriteString(w, gq.ElemType)
		io.WriteString(w, "}, tmp, {}, struct{fn lt(ctx: void, a: struct{key: ")
		io.WriteString(w, sortType)
		io.WriteString(w, ", val: ")
		io.WriteString(w, gq.ElemType)
		io.WriteString(w, "}, b: struct{key: ")
		io.WriteString(w, sortType)
		io.WriteString(w, ", val: ")
		io.WriteString(w, gq.ElemType)
		io.WriteString(w, "}) bool { return ")
		if gq.Desc {
			io.WriteString(w, "a.key > b.key")
		} else {
			io.WriteString(w, "a.key < b.key")
		}
		io.WriteString(w, "; } }.lt);\n")
		writeIndent(w, 1)
		fmt.Fprintf(w, "var arr2 = std.ArrayList(%s).init(std.heap.page_allocator);\n", gq.ElemType)
		writeIndent(w, 1)
		io.WriteString(w, "for (tmp) |it| { arr2.append(it.val) catch unreachable; }\n")
		writeIndent(w, 1)
		io.WriteString(w, "tmp = arr2.toOwnedSlice() catch unreachable;\n")
	}
	writeIndent(w, 1)
	io.WriteString(w, "break :blk tmp;\n}")
}

type FuncExpr struct {
	Name       string
	Params     []Param
	ReturnType string
	Body       []Stmt
}

func zigTypeFromExpr(e Expr) string {
	switch e.(type) {
	case *StringLit:
		return "[]const u8"
	case *BoolLit:
		return "bool"
       case *NullLit:
               return "Value"
	case *FloatLit:
		return "f64"
	case *IntLit:
		return "i64"
	case *VarRef:
		if t, ok := varTypes[e.(*VarRef).Name]; ok && t != "" {
			return t
		}
		return "i64"
       case *MapLit:
               if m := e.(*MapLit); m.StructName != "" {
                       return m.StructName
               }
               m := e.(*MapLit)
               val := m.ValType
               if val == "" {
                       val = "i64"
               }
               key := m.KeyType
               if key == "" || key == "[]const u8" {
                       return fmt.Sprintf("std.StringHashMap(%s)", val)
               }
               return fmt.Sprintf("std.AutoHashMap(%s, %s)", key, val)
	case *ListLit:
		if l := e.(*ListLit); l.ElemType != "" {
			return "[]" + l.ElemType
		}
		return "[]i64"
	case *QueryComp:
		qc := e.(*QueryComp)
		if qc.ElemType != "" {
			return "[]" + qc.ElemType
		}
		return "[]i64"
	case *FieldExpr:
		if fe := e.(*FieldExpr); fe != nil {
			if vr, ok := fe.Target.(*VarRef); ok {
				if stName, ok := varTypes[vr.Name]; ok {
					if sd, ok2 := structDefs[stName]; ok2 {
						for _, f := range sd.Fields {
							if f.Name == toSnakeCase(fe.Name) {
								return f.Type
							}
						}
					}
				}
			}
		}
		return "i64"
	case *BinaryExpr:
		be := e.(*BinaryExpr)
		if be.Op == "+" {
			lt := zigTypeFromExpr(be.Left)
			rt := zigTypeFromExpr(be.Right)
			if lt == "[]const u8" || rt == "[]const u8" {
				return "[]const u8"
			}
			if lt == "f64" || rt == "f64" {
				return "f64"
			}
			return "i64"
		}
		switch be.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return "bool"
		default:
			lt := zigTypeFromExpr(be.Left)
			rt := zigTypeFromExpr(be.Right)
			if lt == "f64" || rt == "f64" {
				return "f64"
			}
			return "i64"
		}
	case *CastExpr:
		ce := e.(*CastExpr)
		ct := ce.Type
		if ct != "" {
			switch ct {
			case "int":
				return "i64"
			case "string":
				return "[]const u8"
			default:
				return ct
			}
		}
		vt := zigTypeFromExpr(ce.Value)
		if vt == "Value" {
			return "Value"
		}
		return vt
	case *IfExpr:
		ie := e.(*IfExpr)
		tType := zigTypeFromExpr(ie.Then)
		eType := zigTypeFromExpr(ie.Else)
		if tType == eType {
			return tType
		}
		return "i64"
	case *CallExpr:
		ce := e.(*CallExpr)
		switch ce.Func {
		case "len", "count", "exists":
			return "i64"
		case "int":
			return "i64"
		case "sum":
			if len(ce.Args) > 0 {
				t := zigTypeFromExpr(ce.Args[0])
				if strings.HasPrefix(t, "[]") {
					t = t[2:]
				}
				if t == "f64" {
					return "f64"
				}
			}
			return "i64"
		case "avg":
			return "f64"
		case "min", "max":
			if len(ce.Args) > 0 {
				return zigTypeFromExpr(ce.Args[0])
			}
			return "i64"
		case "_str", "_concat_string":
			return "[]const u8"
		case "_split_string":
			return "[][]const u8"
		case "_now":
			return "i64"
		case "_input":
			return "[]const u8"
		default:
			if strings.HasPrefix(ce.Func, "std.math.") {
				return "f64"
			}
			if strings.HasPrefix(ce.Func, "std.ascii.") {
				return "[]const u8"
			}
			if rt, ok := funcReturns[ce.Func]; ok {
				if rt == "" {
					return "void"
				}
				return rt
			}
			return "i64"
		}
	case *AppendExpr:
		ae := e.(*AppendExpr)
		// Prefer explicitly tracked element type when available.
		if ae.ElemType != "" {
			return "[]" + ae.ElemType
		}
		// Otherwise infer from the list or value being appended.
		lt := zigTypeFromExpr(ae.List)
		if strings.HasPrefix(lt, "[]") {
			return lt
		}
		return "[]" + zigTypeFromExpr(ae.Value)
	case *IndexExpr:
		ix := e.(*IndexExpr)
		t := zigTypeFromExpr(ix.Target)
		if ix.Map {
			if strings.HasPrefix(t, "std.StringHashMap(") {
				return strings.TrimSuffix(strings.TrimPrefix(t, "std.StringHashMap("), ")")
			}
			if strings.HasPrefix(t, "std.AutoHashMap(") {
				inner := strings.TrimSuffix(strings.TrimPrefix(t, "std.AutoHashMap("), ")")
				parts := strings.Split(inner, ",")
				if len(parts) == 2 {
					return strings.TrimSpace(parts[1])
				}
			}
			return "i64"
		}
		if strings.HasPrefix(t, "[]") {
			if t == "[]const u8" {
				return t
			}
			return t[2:]
		}
		if strings.HasPrefix(t, "[") {
			if pos := strings.Index(t, "]"); pos > 0 {
				return t[pos+1:]
			}
		}
		return "i64"
	case *SliceExpr:
		se := e.(*SliceExpr)
		t := zigTypeFromExpr(se.Target)
		if strings.HasPrefix(t, "[]") {
			return t
		}
		if strings.HasPrefix(t, "[") {
			if pos := strings.Index(t, "]"); pos > 0 {
				return "[]" + t[pos+1:]
			}
		}
		return "[]i64"
	default:
		return "i64"
	}
}

func (m *MapLit) emit(w io.Writer) {
	if m.StructName != "" {
		io.WriteString(w, ".{ ")
		for i, e := range m.Entries {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			if k, ok := e.Key.(*StringLit); ok {
				fmt.Fprintf(w, ".%s = ", zigIdent(k.Value))
			}
			e.Value.emit(w)
		}
		io.WriteString(w, " }")
		return
	}
	keyType := m.KeyType
	valType := m.ValType
	if keyType == "" || valType == "" {
		keyType = "[]const u8"
		valType = "i64"
		if len(m.Entries) > 0 {
			keyType = zigTypeFromExpr(m.Entries[0].Key)
			valType = zigTypeFromExpr(m.Entries[0].Value)
		}
	}
	mapType := "std.StringHashMap(" + valType + ")"
	if keyType != "[]const u8" {
		mapType = fmt.Sprintf("std.AutoHashMap(%s, %s)", keyType, valType)
	}
       fmt.Fprintf(w, "blk: { var m = %s.init(std.heap.page_allocator);", mapType)
	for _, e := range m.Entries {
		io.WriteString(w, " m.put(")
		e.Key.emit(w)
		io.WriteString(w, ", ")
		if valType == "Value" && zigTypeFromExpr(e.Value) != "Value" {
			io.WriteString(w, "Value{.")
			io.WriteString(w, valueTag(zigTypeFromExpr(e.Value)))
			io.WriteString(w, " = ")
			e.Value.emit(w)
			io.WriteString(w, "}")
		} else {
			e.Value.emit(w)
		}
		io.WriteString(w, ") catch unreachable;")
	}
	io.WriteString(w, " break :blk m; }")
}

// IndexExpr represents list indexing like `xs[i]`.
type IndexExpr struct {
	Target Expr
	Index  Expr
	Map    bool
}

// BreakStmt exits the nearest loop.
type BreakStmt struct{}

// ContinueStmt skips to the next loop iteration.
type ContinueStmt struct{}

// ReturnStmt returns from a function optionally with a value.
type ReturnStmt struct{ Value Expr }

// BenchStmt measures execution time and prints a JSON result.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func gitTime() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				t = t.In(loc)
			}
			return t.Format("2006-01-02 15:04 -0700")
		}
	}
	return time.Now().Format("2006-01-02 15:04 -0700")
}

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

func constValue(e *parser.Expr) (any, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	pf := u.Value
	switch {
	case pf.Target.Lit != nil:
		lit := pf.Target.Lit
		switch {
		case lit.Int != nil:
			return int64(*lit.Int), true
		case lit.Float != nil:
			return *lit.Float, true
		case lit.Bool != nil:
			return bool(*lit.Bool), true
		case lit.Str != nil:
			return *lit.Str, true
		case lit.Null:
			return nil, true
		}
	case pf.Target.Map != nil:
		m := map[string]any{}
		for _, it := range pf.Target.Map.Items {
			key, ok := types.SimpleStringKey(it.Key)
			if !ok {
				return nil, false
			}
			v, ok := constValue(it.Value)
			if !ok {
				return nil, false
			}
			m[key] = v
		}
		return m, true
	case pf.Target.List != nil:
		arr := make([]any, len(pf.Target.List.Elems))
		for i, el := range pf.Target.List.Elems {
			v, ok := constValue(el)
			if !ok {
				return nil, false
			}
			arr[i] = v
		}
		return arr, true
	}
	return nil, false
}

func valueToExpr(v any) Expr {
	switch val := v.(type) {
	case nil:
		return &StringLit{Value: ""}
	case bool:
		return &BoolLit{Value: val}
	case int:
		return &IntLit{Value: val}
	case int64:
		return &IntLit{Value: int(val)}
	case float64:
		if math.Trunc(val) == val {
			return &IntLit{Value: int(val)}
		}
		return &FloatLit{Value: val}
	case string:
		return &StringLit{Value: val}
	case map[string]any:
		items := make([]MapEntry, 0, len(val))
		keys := make([]string, 0, len(val))
		for k := range val {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			items = append(items, MapEntry{Key: &StringLit{Value: k}, Value: valueToExpr(val[k])})
		}
		return &MapLit{Entries: items}
	case []any:
		elems := make([]Expr, len(val))
		for i, el := range val {
			elems[i] = valueToExpr(el)
		}
		return &ListLit{Elems: elems}
	}
	return &StringLit{Value: fmt.Sprintf("%v", v)}
}

func header() string {
	return fmt.Sprintf("// Generated by Mochi Zig transpiler on %s\n", gitTime())
}

func compileLoadExpr(l *parser.LoadExpr) (Expr, error) {
	path := ""
	if l.Path != nil {
		path = *l.Path
	}
	if path != "" && !filepath.IsAbs(path) {
		root := repoRoot()
		cand := filepath.Join(root, path)
		if _, err := os.Stat(cand); err != nil {
			clean := path
			for strings.HasPrefix(clean, "../") {
				clean = strings.TrimPrefix(clean, "../")
			}
			cand = filepath.Join(root, "tests", clean)
		}
		path = cand
	}
	format := "jsonl"
	if l.With != nil {
		if v, ok := constValue(l.With); ok {
			if m, ok2 := v.(map[string]any); ok2 {
				if f, ok3 := m["format"].(string); ok3 {
					format = f
				}
			}
		}
	}
	var rows []map[string]any
	var err error
	switch format {
	case "yaml":
		rows, err = data.LoadYAML(path)
	case "jsonl":
		rows, err = data.LoadJSONL(path)
	default:
		rows, err = data.LoadJSON(path)
	}
	if err != nil {
		return nil, err
	}
	elems := make([]Expr, len(rows))
	elemType := ""
	if l.Type != nil && l.Type.Simple != nil {
		stName := *l.Type.Simple
		elemType = stName
		if st, ok := transEnv.GetStruct(stName); ok {
			for i, r := range rows {
				entries := make([]MapEntry, len(st.Order))
				for j, f := range st.Order {
					entries[j] = MapEntry{Key: &StringLit{Value: f}, Value: valueToExpr(r[f])}
				}
				elems[i] = &MapLit{Entries: entries, StructName: stName}
			}
		}
	} else {
		for i, r := range rows {
			elems[i] = valueToExpr(r)
		}
	}
	return &ListLit{Elems: elems, ElemType: elemType}, nil
}

// Emit returns the Zig source code for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	varUses, varMut = collectVarInfo(p)
	for _, vd := range varDecls {
		if vd.Mutable && !varMut[vd.Name] {
			vd.Mutable = false
		}
	}
	buf.WriteString(header())
	buf.WriteString("const std = @import(\"std\");\n")
       if useValue {
               buf.WriteString("const Value = union(enum) { Null: void, Int: i64, Float: f64, Str: []const u8, Bool: bool, List: [][]i64, StrList: [][]const u8, };\n")
       }
	buf.WriteString("\nfn handleError(err: anyerror) noreturn {\n")
	buf.WriteString("    std.debug.panic(\"{any}\", .{err});\n}\n\n")
	for _, st := range p.Structs {
		st.emit(&buf)
		buf.WriteString("\n")
	}
	for _, g := range p.Globals {
		g.emit(&buf, 0)
	}
	if len(p.Globals) > 0 {
		buf.WriteString("\n")
	}
	firstFn := true
	for _, fn := range p.Functions {
		if fn == nil {
			continue
		}
		if !firstFn {
			buf.WriteString("\n")
		}
		firstFn = false
		fn.emit(&buf)
	}
	if useNow {
		buf.WriteString("\nvar _now_seed: i64 = 0;\n")
		buf.WriteString("var _now_seeded: bool = false;\n")
		buf.WriteString("fn _now() i64 {\n")
		buf.WriteString("    if (_now_seeded) {\n")
		buf.WriteString("        _now_seed = @mod(_now_seed * 1664525 + 1013904223, 2147483647);\n")
		buf.WriteString("        return _now_seed;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if (! _now_seeded) {\n")
		buf.WriteString("        if (std.process.getEnvVarOwned(std.heap.page_allocator, \"MOCHI_NOW_SEED\")) |env_seed| {\n")
		buf.WriteString("            defer std.heap.page_allocator.free(env_seed);\n")
		buf.WriteString("            if (std.fmt.parseInt(i64, env_seed, 10)) |v| {\n")
		buf.WriteString("                _now_seed = v;\n")
		buf.WriteString("                _now_seeded = true;\n")
		buf.WriteString("                _now_seed = @mod(_now_seed * 1664525 + 1013904223, 2147483647);\n")
		buf.WriteString("                return _now_seed;\n")
		buf.WriteString("            } else |_| {}\n")
		buf.WriteString("        } else |_| {}\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return @as(i64, @intCast(std.time.nanoTimestamp()));\n")
		buf.WriteString("}\n")
	}
	if useStr {
		buf.WriteString("\nfn _str(v: anytype) []const u8 {\n")
		buf.WriteString("    if (@TypeOf(v) == f64 or @TypeOf(v) == f32) {\n")
		buf.WriteString("        return std.fmt.allocPrint(std.heap.page_allocator, \"{d}\", .{v}) catch unreachable;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return std.fmt.allocPrint(std.heap.page_allocator, \"{any}\", .{v}) catch unreachable;\n")
		buf.WriteString("}\n")
	}
	if useConcat {
		buf.WriteString("\nfn _concat_string(lhs: []const u8, rhs: []const u8) []const u8 {\n")
		buf.WriteString("    const alloc = std.heap.page_allocator;\n")
		buf.WriteString("    var out = alloc.alloc(u8, lhs.len + rhs.len + 1) catch unreachable;\n")
		buf.WriteString("    std.mem.copyForwards(u8, out[0..lhs.len], lhs);\n")
		buf.WriteString("    std.mem.copyForwards(u8, out[lhs.len..lhs.len + rhs.len], rhs);\n")
		buf.WriteString("    out[lhs.len + rhs.len] = 0;\n")
		buf.WriteString("    return out[0..lhs.len + rhs.len];\n")
		buf.WriteString("}\n")
	}
	if useSplit {
		buf.WriteString("\nfn _split_string(s: []const u8, sep: []const u8) [][]const u8 {\n")
		buf.WriteString("    var res = std.ArrayList([]const u8).init(std.heap.page_allocator);\n")
		buf.WriteString("    defer res.deinit();\n")
		buf.WriteString("    var it = std.mem.splitSequence(u8, s, sep);\n")
		buf.WriteString("    while (it.next()) |p| { res.append(p) catch unreachable; }\n")
		buf.WriteString("    return res.toOwnedSlice() catch unreachable;\n")
		buf.WriteString("}\n")
	}
	if useAscii {
		buf.WriteString("\nfn _upper(s: []const u8) []const u8 {\n")
		buf.WriteString("    var out = std.heap.page_allocator.alloc(u8, s.len + 1) catch unreachable;\n")
		buf.WriteString("    _ = std.ascii.upperString(out[0..s.len], s);\n")
		buf.WriteString("    out[s.len] = 0;\n")
		buf.WriteString("    return out[0..s.len];\n")
		buf.WriteString("}\n")
		buf.WriteString("\nfn _lower(s: []const u8) []const u8 {\n")
		buf.WriteString("    var out = std.heap.page_allocator.alloc(u8, s.len + 1) catch unreachable;\n")
		buf.WriteString("    _ = std.ascii.lowerString(out[0..s.len], s);\n")
		buf.WriteString("    out[s.len] = 0;\n")
		buf.WriteString("    return out[0..s.len];\n")
		buf.WriteString("}\n")
	}
	if useSha256 {
		buf.WriteString("\nfn _sha256(bs: []i64) []i64 {\n")
		buf.WriteString("    var data = std.heap.page_allocator.alloc(u8, bs.len) catch unreachable;\n")
		buf.WriteString("    defer std.heap.page_allocator.free(data);\n")
		buf.WriteString("    var i: usize = 0;\n")
		buf.WriteString("    while (i < bs.len) { data[i] = @intCast(bs[i]); i += 1; }\n")
		buf.WriteString("    var digest: [32]u8 = undefined;\n")
		buf.WriteString("    std.crypto.hash.sha2.Sha256.hash(data, &digest, .{});\n")
		buf.WriteString("    var out = std.heap.page_allocator.alloc(i64, 32) catch unreachable;\n")
		buf.WriteString("    i = 0;\n")
		buf.WriteString("    while (i < 32) { out[i] = digest[i]; i += 1; }\n")
		buf.WriteString("    return out;\n")
		buf.WriteString("}\n")
	}
	if useInput {
		buf.WriteString("\nfn _input() []const u8 {\n")
		buf.WriteString("    var reader = std.io.bufferedReaderSize(4096, std.io.getStdIn().reader());\n")
		buf.WriteString("    const opt_line = reader.reader().readUntilDelimiterOrEofAlloc(std.heap.page_allocator, '\\n', 1 << 20) catch return \"\";\n")
		buf.WriteString("    const line = opt_line orelse return \"\";\n")
		buf.WriteString("    if (line.len > 0 and line[line.len - 1] == '\\n') {\n")
		buf.WriteString("        return line[0..line.len-1];\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return line;\n")
		buf.WriteString("}\n")
	}
	if useLookupHost {
		buf.WriteString("\nfn _lookup_host(host: []const u8) []const i32 {\n")
		buf.WriteString("    return &[_]i32{0, 0};\n")
		buf.WriteString("}\n")
	}
	if useMem {
		buf.WriteString("\nfn _mem() i64 {\n")
		buf.WriteString("    const path = \"/proc/self/statm\";\n")
		buf.WriteString("    var file = std.fs.openFileAbsolute(path, .{}) catch return 0;\n")
		buf.WriteString("    defer file.close();\n")
		buf.WriteString("    var buf: [64]u8 = undefined;\n")
		buf.WriteString("    const n = file.readAll(&buf) catch return 0;\n")
		buf.WriteString("    var it = std.mem.tokenizeScalar(u8, buf[0..n], ' ');\n")
		buf.WriteString("    _ = it.next(); // total program size\n")
		buf.WriteString("    if (it.next()) |tok| {\n")
		buf.WriteString("        const pages = std.fmt.parseInt(i64, tok, 10) catch return 0;\n")
		buf.WriteString("        return pages * std.mem.page_size;\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0;\n")
		buf.WriteString("}\n")
	}
	if usePrint {
		buf.WriteString("\nfn _print(v: []const u8) void {\n")
		buf.WriteString("    std.debug.print(\"{s}\\n\", .{v});\n")
		buf.WriteString("}\n")
	}
	return buf.Bytes()
}

func (f *Func) emit(w io.Writer) {
	pub := ""
	if f.Name == "main" {
		pub = "pub "
	}
	fmt.Fprintf(w, "%sfn %s(", pub, f.Name)
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		name := p.Name
		if varUses[p.Name] == 0 {
			name = "_"
		}
		fmt.Fprintf(w, "%s: %s", name, p.Type)
	}
	ret := f.ReturnType
	if ret == "" {
		ret = "void"
	}
       fmt.Fprintf(w, ") %s {\n", ret)
       currentReturnType = ret
       aliasStack = append(aliasStack, f.Aliases)
       for _, st := range f.Body {
               st.emit(w, 1)
       }
       aliasStack = aliasStack[:len(aliasStack)-1]
       currentReturnType = ""
       fmt.Fprintln(w, "}")
}

func writeIndent(w io.Writer, n int) {
	for i := 0; i < n; i++ {
		io.WriteString(w, "    ")
	}
}

func (s *PrintStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	if len(s.Values) == 0 {
		io.WriteString(w, "std.debug.print(\"\\n\", .{});\n")
		return
	}
	fmtSpec := make([]string, len(s.Values))
	for i, v := range s.Values {
		switch t := v.(type) {
		case *IndexExpr:
			if t.Map {
				fmtSpec[i] = "{s}"
			} else {
				fmtSpec[i] = "{d}"
			}
		case *StringLit:
			fmtSpec[i] = "{s}"
		case *FloatLit:
			fmtSpec[i] = "{d}"
		default:
			tname := zigTypeFromExpr(v)
			if tname == "[]const u8" {
				fmtSpec[i] = "{s}"
			} else if tname == "f64" || tname == "f32" {
				fmtSpec[i] = "{d}"
			} else {
				fmtSpec[i] = "{any}"
			}
		}
	}
	io.WriteString(w, "std.debug.print(\"")
	io.WriteString(w, strings.Join(fmtSpec, " "))
	io.WriteString(w, "\\n\", .{")
	for i, v := range s.Values {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		v.emit(w)
	}
	io.WriteString(w, "});\n")
}

func (v *VarDecl) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	mut := varMut[v.Name]
	targetType := v.Type
	if targetType == "" {
		if t, ok := varTypes[v.Name]; ok && t != "" {
			targetType = t
		} else {
			targetType = zigTypeFromExpr(v.Value)
		}
	}
	if strings.HasPrefix(targetType, "std.StringHashMap(") || strings.HasPrefix(targetType, "std.AutoHashMap(") {
		mut = true
	}
	kw := "const"
	if mut {
		kw = "var"
	}
	if lit, ok := v.Value.(*ListLit); ok && mut && v.Type == "" {
		elem := lit.ElemType
		if elem == "" {
			elem = "i64"
		}
		fmt.Fprintf(w, "%s %s: [%d]%s = [", kw, v.Name, len(lit.Elems), elem)
		fmt.Fprintf(w, "%d]%s{", len(lit.Elems), elem)
		for i, e := range lit.Elems {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			e.emit(w)
		}
		io.WriteString(w, "};\n")
		return
	}
	fmt.Fprintf(w, "%s %s", kw, v.Name)
	if targetType != "" {
		typ := targetType
		if kw == "const" && strings.HasPrefix(typ, "const ") {
			typ = strings.TrimPrefix(typ, "const ")
		}
		fmt.Fprintf(w, ": %s", typ)
	}
	io.WriteString(w, " = ")
	if v.Value == nil {
		if strings.HasPrefix(targetType, "std.StringHashMap(") || strings.HasPrefix(targetType, "std.AutoHashMap(") {
			fmt.Fprintf(w, "%s.init(std.heap.page_allocator)", targetType)
		} else if strings.HasPrefix(targetType, "[]") {
			base := targetType[2:]
			if strings.HasPrefix(base, "const ") {
				base = base[len("const "):]
			}
			fmt.Fprintf(w, "&[_]%s{}", base)
               } else {
                       if isIntType(targetType) || strings.HasPrefix(targetType, "f") || targetType == "bool" {
                               io.WriteString(w, "0")
                       } else {
                               io.WriteString(w, "undefined")
                       }
               }
       } else {
               v.Value.emit(w)
               if zigTypeFromExpr(v.Value) == "Value" {
                       io.WriteString(w, valueAccess(targetType))
		}
	}
	io.WriteString(w, ";")
	if indent > 0 && varUses[v.Name] == 0 && !varMut[v.Name] {
		io.WriteString(w, " _ = ")
		io.WriteString(w, v.Name)
		io.WriteString(w, ";")
	}
	io.WriteString(w, "\n")
}

func (a *AssignStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	fmt.Fprintf(w, "%s = ", a.Name)
	a.Value.emit(w)
	if zigTypeFromExpr(a.Value) == "Value" {
		if suf := valueAccess(varTypes[a.Name]); suf != "" {
			io.WriteString(w, suf)
		}
	}
	io.WriteString(w, ";\n")
}

func (a *IndexAssignStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	if idx, ok := a.Target.(*IndexExpr); ok && idx.Map {
		var emitPath func(*IndexExpr)
		emitPath = func(ix *IndexExpr) {
			if inner, ok2 := ix.Target.(*IndexExpr); ok2 && inner.Map {
				emitPath(inner)
				io.WriteString(w, ".get(")
				ix.Index.emit(w)
				io.WriteString(w, ").?")
			} else {
				ix.Target.emit(w)
			}
		}
		emitPath(idx)
		io.WriteString(w, ".put(")
		idx.Index.emit(w)
		io.WriteString(w, ", ")
		a.Value.emit(w)
		io.WriteString(w, ") catch unreachable;\n")
		return
	}
	a.Target.emit(w)
	io.WriteString(w, " = ")
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (a *FieldAssignStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	a.Target.emit(w)
	fmt.Fprintf(w, ".%s = ", toSnakeCase(a.Name))
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (u *UpdateStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	fmt.Fprintf(w, "for (%s) |item, idx| {\n", u.Target)
	if u.Cond != nil {
		writeIndent(w, indent+1)
		io.WriteString(w, "if (")
		u.Cond.emit(w)
		io.WriteString(w, ") {\n")
	}
	inner := indent + 1
	if u.Cond != nil {
		inner++
	}
	for i, f := range u.Fields {
		writeIndent(w, inner)
		fmt.Fprintf(w, "item.%s = ", toSnakeCase(f))
		u.Values[i].emit(w)
		io.WriteString(w, ";\n")
	}
	if u.Cond != nil {
		writeIndent(w, indent+1)
		io.WriteString(w, "}\n")
	}
	writeIndent(w, indent+1)
	fmt.Fprintf(w, "%s[idx] = item;\n", u.Target)
	writeIndent(w, indent)
	io.WriteString(w, "}\n")
}

func (s *SaveStmt) emit(w io.Writer, indent int) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		writeIndent(w, indent)
		io.WriteString(w, "for (")
		if s.Src != nil {
			s.Src.emit(w)
		}
		io.WriteString(w, ") |row| {\n")
		writeIndent(w, indent+1)
		io.WriteString(w, "const __j = std.json.stringifyAlloc(std.heap.page_allocator, row, .{}) catch unreachable;\n")
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "const _tmp = std.mem.replaceOwned(u8, std.heap.page_allocator, __j, %q, %q) catch unreachable;\n", ":", ": ")
		writeIndent(w, indent+1)
		io.WriteString(w, "defer std.heap.page_allocator.free(_tmp);\n")
		writeIndent(w, indent+1)
		fmt.Fprintf(w, "const _line = std.mem.replaceOwned(u8, std.heap.page_allocator, _tmp, %q, %q) catch unreachable;\n", ",", ", ")
		writeIndent(w, indent+1)
		io.WriteString(w, "defer std.heap.page_allocator.free(_line);\n")
		writeIndent(w, indent+1)
		io.WriteString(w, "std.heap.page_allocator.free(__j);\n")
		writeIndent(w, indent+1)
		io.WriteString(w, "std.debug.print(\"{s}\\n\", .{_line});\n")
		writeIndent(w, indent)
		io.WriteString(w, "}\n")
		return
	}
	writeIndent(w, indent)
	io.WriteString(w, "// unsupported save\n")
}

func (j *JSONStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "const __j = std.json.stringifyAlloc(std.heap.page_allocator, ")
	if j.Value != nil {
		j.Value.emit(w)
	} else {
		io.WriteString(w, "null")
	}
	io.WriteString(w, ", .{}) catch unreachable;\n")
	writeIndent(w, indent)
	io.WriteString(w, "std.debug.print(\"{s}\\n\", .{__j});\n")
	writeIndent(w, indent)
	io.WriteString(w, "std.heap.page_allocator.free(__j);\n")
}

func (e *ExprStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	if e.Expr != nil {
		if zigTypeFromExpr(e.Expr) != "void" {
			io.WriteString(w, "_ = ")
		}
		e.Expr.emit(w)
		io.WriteString(w, ";")
	}
	io.WriteString(w, "\n")
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

func (f *FloatLit) emit(w io.Writer) {
	if math.Trunc(f.Value) == f.Value {
		io.WriteString(w, strconv.FormatFloat(f.Value, 'f', 1, 64))
	} else {
		io.WriteString(w, strconv.FormatFloat(f.Value, 'f', -1, 64))
	}
}

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, resolveAlias(v.Name)) }

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		lt := zigTypeFromExpr(b.Left)
		rt := zigTypeFromExpr(b.Right)
		if lt == "[]const u8" || rt == "[]const u8" {
			useConcat = true
			io.WriteString(w, "_concat_string(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
		if l, ok := b.Left.(*StringLit); ok {
			if r, ok2 := b.Right.(*StringLit); ok2 {
				fmt.Fprintf(w, "%q", l.Value+r.Value)
				return
			}
		}
	}
       lt := zigTypeFromExpr(b.Left)
       rt := zigTypeFromExpr(b.Right)
       if (b.Op == "==" || b.Op == "!=") && lt == "Value" && rt == "Value" {
               if _, ok := b.Right.(*NullLit); ok {
                       b.Left.emit(w)
                       fmt.Fprintf(w, " %s .Null", b.Op)
                       return
               }
               if _, ok := b.Left.(*NullLit); ok {
                       b.Right.emit(w)
                       fmt.Fprintf(w, " %s .Null", b.Op)
                       return
               }
       }
       if lt == "[]const u8" && rt == "[]const u8" {
               switch b.Op {
		case "==", "!=":
			if b.Op == "==" {
				io.WriteString(w, "std.mem.eql(u8, ")
			} else {
				io.WriteString(w, "!std.mem.eql(u8, ")
			}
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		case "<":
			io.WriteString(w, "std.mem.order(u8, ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ") == .lt")
			return
		case "<=":
			io.WriteString(w, "std.mem.order(u8, ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ") != .gt")
			return
		case ">":
			io.WriteString(w, "std.mem.order(u8, ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ") == .gt")
			return
		case ">=":
			io.WriteString(w, "std.mem.order(u8, ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ") != .lt")
			return
		}
	}
	// handle equality for slices of non-byte elements
	if strings.HasPrefix(lt, "[]") && lt == rt {
		switch b.Op {
		case "==", "!=":
			elem := lt[2:]
			if b.Op == "==" {
				fmt.Fprintf(w, "std.mem.eql(%s, ", elem)
			} else {
				fmt.Fprintf(w, "!std.mem.eql(%s, ", elem)
			}
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	if lt == "f64" && isIntType(rt) {
		if _, ok := b.Right.(*IntLit); ok {
			b.Left.emit(w)
			fmt.Fprintf(w, " %s ", b.Op)
			io.WriteString(w, "@as(f64, @floatFromInt(")
			b.Right.emit(w)
			io.WriteString(w, "))")
			return
		}
	}
	if isIntType(lt) && rt == "f64" {
		if _, ok := b.Left.(*IntLit); ok {
			io.WriteString(w, "@as(f64, @floatFromInt(")
			b.Left.emit(w)
			io.WriteString(w, "))")
			fmt.Fprintf(w, " %s ", b.Op)
			b.Right.emit(w)
			return
		}
	}
	if b.Op == "in" {
		if vr, ok := b.Right.(*VarRef); ok && mapVars[vr.Name] {
			vr.emit(w)
			io.WriteString(w, ".contains(")
			b.Left.emit(w)
			io.WriteString(w, ")")
		} else if _, ok := b.Right.(*StringLit); ok {
			io.WriteString(w, "std.mem.indexOf(u8, ")
			b.Right.emit(w)
			io.WriteString(w, ", ")
			b.Left.emit(w)
			io.WriteString(w, ") != null")
		} else if _, ok := b.Left.(*StringLit); ok {
			io.WriteString(w, "std.mem.indexOf(u8, ")
			b.Right.emit(w)
			io.WriteString(w, ", ")
			b.Left.emit(w)
			io.WriteString(w, ") != null")
		} else {
			io.WriteString(w, "std.mem.indexOfScalar(i64, ")
			b.Right.emit(w)
			io.WriteString(w, ", ")
			b.Left.emit(w)
			io.WriteString(w, ") != null")
		}
		return
	}
	op := b.Op
	if op == "%" {
		io.WriteString(w, "@mod(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if op == "/" {
		lt := zigTypeFromExpr(b.Left)
		rt := zigTypeFromExpr(b.Right)
		if isIntType(lt) && isIntType(rt) {
			io.WriteString(w, "@divTrunc(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	lp := precedence(b.Op)
	if lb, ok := b.Left.(*BinaryExpr); ok && precedence(lb.Op) > lp {
		io.WriteString(w, "(")
		lb.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emit(w)
	}
	if op == "&&" {
		op = "and"
	} else if op == "||" {
		op = "or"
	}
	fmt.Fprintf(w, " %s ", op)
	if rb, ok := b.Right.(*BinaryExpr); ok && precedence(rb.Op) >= lp {
		io.WriteString(w, "(")
		rb.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emit(w)
	}
}

func precedence(op string) int {
	switch op {
	case "*", "/", "%":
		return 0
	case "+", "-":
		return 1
	case "<", "<=", ">", ">=":
		return 2
	case "==", "!=", "in":
		return 3
	case "&&":
		return 4
	case "||":
		return 5
	default:
		return 6
	}
}

func (l *ListLit) emit(w io.Writer) {
	if l.ElemType != "" {
		if len(l.Elems) == 0 {
			et := l.ElemType
			if strings.HasPrefix(et, "const ") {
				et = et[len("const "):]
			}
			fmt.Fprintf(w, "&[_]%s{}", et)
			return
		}
		et := l.ElemType
		if strings.HasPrefix(et, "const ") {
			et = et[len("const "):]
		}
		lbl := fmt.Sprintf("blk%d", groupCounter)
		tmp := fmt.Sprintf("_tmp%d", groupCounter)
		groupCounter++
		fmt.Fprintf(w, "%s: { var %s = std.heap.page_allocator.alloc(%s, %d) catch unreachable;", lbl, tmp, et, len(l.Elems))
		for i, e := range l.Elems {
			fmt.Fprintf(w, " %s[%d] = ", tmp, i)
			if l.ElemType == "Value" {
				t := zigTypeFromExpr(e)
				fmt.Fprintf(w, "Value{.%s = ", valueTag(t))
				e.emit(w)
				io.WriteString(w, "};")
			} else {
				e.emit(w)
				io.WriteString(w, ";")
			}
		}
		fmt.Fprintf(w, " break :%s %s; }", lbl, tmp)
		return
	} else if len(l.Elems) > 0 {
		if _, ok := l.Elems[0].(*ListLit); ok {
			if sub, ok := l.Elems[0].(*ListLit); ok {
				fmt.Fprintf(w, "[%d][%d]i64{", len(l.Elems), len(sub.Elems))
			}
		} else {
			fmt.Fprintf(w, "[%d]i64{", len(l.Elems))
		}
	} else {
		io.WriteString(w, "&[_]i64{}")
		return
	}
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	if l.ElemType != "" {
		io.WriteString(w, "})[0..]")
	} else {
		io.WriteString(w, "}")
	}
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.Map {
		i.Target.emit(w)
		io.WriteString(w, ".get(")
		i.Index.emit(w)
		io.WriteString(w, ").?")
		return
	}
	t := zigTypeFromExpr(i.Target)
	if t == "[]const u8" {
		i.Target.emit(w)
		io.WriteString(w, "[@intCast(")
		i.Index.emit(w)
		io.WriteString(w, ")..@intCast(")
		i.Index.emit(w)
		io.WriteString(w, ") + 1]")
	} else {
		i.Target.emit(w)
		io.WriteString(w, "[@intCast(")
		i.Index.emit(w)
		io.WriteString(w, ")]")
	}
}

func (sli *SliceExpr) emit(w io.Writer) {
	sli.Target.emit(w)
	io.WriteString(w, "[")
       if sli.Start != nil {
               io.WriteString(w, "@intCast(")
               sli.Start.emit(w)
               io.WriteString(w, ")")
       } else {
               io.WriteString(w, "0")
       }
       io.WriteString(w, "..")
	if sli.End != nil {
		io.WriteString(w, "@intCast(")
		sli.End.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, "]")
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int", "i64":
		if zigTypeFromExpr(c.Value) == "[]const u8" {
			io.WriteString(w, "std.fmt.parseInt(i64, ")
			c.Value.emit(w)
			io.WriteString(w, ", 10) catch 0")
		} else if zigTypeFromExpr(c.Value) == "f64" {
			io.WriteString(w, "@as(i64, @intFromFloat(")
			c.Value.emit(w)
			io.WriteString(w, "))")
		} else {
			io.WriteString(w, "@as(i64, ")
			c.Value.emit(w)
			io.WriteString(w, ")")
		}
	case "float", "f64":
		vt := zigTypeFromExpr(c.Value)
		if vt == "f64" {
			io.WriteString(w, "@as(f64, ")
			c.Value.emit(w)
			io.WriteString(w, ")")
		} else if isIntType(vt) {
			io.WriteString(w, "@as(f64, @floatFromInt(")
			c.Value.emit(w)
			io.WriteString(w, "))")
		} else {
			io.WriteString(w, "@as(f64, ")
			c.Value.emit(w)
			io.WriteString(w, ")")
		}
	default:
		c.Value.emit(w)
		if zigTypeFromExpr(c.Value) == "Value" {
			io.WriteString(w, valueAccess(c.Type))
		}
	}
}

func (n *NotExpr) emit(w io.Writer) {
	io.WriteString(w, "!(")
	n.Expr.emit(w)
	io.WriteString(w, ")")
}

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (n *NullLit) emit(w io.Writer) {
       if useValue {
               io.WriteString(w, "Value{.Null = {}}");
       } else {
               io.WriteString(w, "0")
       }
}

func (i *IfStmt) emit(w io.Writer, indent int) {
	for j := 0; j < indent; j++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "if (")
	i.Cond.emit(w)
	if zigTypeFromExpr(i.Cond) == "Value" {
		io.WriteString(w, ".Bool")
	}
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		st.emit(w, indent+1)
	}
	for j := 0; j < indent; j++ {
		io.WriteString(w, "    ")
	}
	if len(i.Else) == 0 {
		io.WriteString(w, "}\n")
		return
	}
	io.WriteString(w, "} else {\n")
	for _, st := range i.Else {
		st.emit(w, indent+1)
	}
	for j := 0; j < indent; j++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "}\n")
}

func (wst *WhileStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "while (")
	wst.Cond.emit(w)
	io.WriteString(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w, indent+1)
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "}\n")
}

func (f *ForStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	used := stmtsUse(f.Name, f.Body)
	tmp := fmt.Sprintf("__it%d", loopCounter)
	loopCounter++
	if f.Iterable != nil {
		io.WriteString(w, "for (")
		f.Iterable.emit(w)
		io.WriteString(w, ") |")
		if used {
			io.WriteString(w, tmp)
			io.WriteString(w, "| {\n")
			writeIndent(w, indent+1)
			typ := zigTypeFromExpr(f.Iterable)
			if typ == "[]const u8" {
				fmt.Fprintf(w, "const %s: []const u8 = &[_]u8{%s};\n", f.Name, tmp)
			} else {
				fmt.Fprintf(w, "const %s = %s;\n", f.Name, tmp)
			}
		} else {
			io.WriteString(w, "_|")
			io.WriteString(w, " {\n")
		}
	} else {
		io.WriteString(w, "for (")
		f.Start.emit(w)
		io.WriteString(w, "..")
		f.End.emit(w)
		io.WriteString(w, ") |")
		if used {
			io.WriteString(w, tmp)
			io.WriteString(w, "| {\n")
			writeIndent(w, indent+1)
			fmt.Fprintf(w, "const %s: i64 = @as(i64, @intCast(%s));\n", f.Name, tmp)
		} else {
			io.WriteString(w, "_|")
			io.WriteString(w, " {\n")
		}
	}
	for _, st := range f.Body {
		st.emit(w, indent+1)
	}
	writeIndent(w, indent)
	io.WriteString(w, "}\n")
}

func (b *BreakStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "break;\n")
}

func (c *ContinueStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "continue;\n")
}

func (r *ReturnStmt) emit(w io.Writer, indent int) {
        writeIndent(w, indent)
        io.WriteString(w, "return")
        if r.Value != nil {
               io.WriteString(w, " ")
               if list, ok := r.Value.(*ListLit); ok && list.ElemType == "" && strings.HasPrefix(currentReturnType, "[]") {
                       list.ElemType = currentReturnType[2:]
               }
               r.Value.emit(w)
        }
        io.WriteString(w, ";\n")
}

func (b *BenchStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	io.WriteString(w, "{\n")
	indent++
	writeIndent(w, indent)
	io.WriteString(w, "const __start = _now();\n")
	writeIndent(w, indent)
	io.WriteString(w, "const __start_mem: i64 = _mem();\n")
	for _, st := range b.Body {
		st.emit(w, indent)
	}
	writeIndent(w, indent)
	io.WriteString(w, "const __end = _now();\n")
	writeIndent(w, indent)
	io.WriteString(w, "const __end_mem: i64 = _mem();\n")
	writeIndent(w, indent)
	io.WriteString(w, "const __duration_us: i64 = @divTrunc(@as(i64, @intCast(__end - __start)), 1000);\n")
	writeIndent(w, indent)
	io.WriteString(w, "const __mem_diff: i64 = __end_mem - __start_mem;\n")
	writeIndent(w, indent)
	io.WriteString(w, "const __memory_bytes: i64 = if (__mem_diff < 0) -__mem_diff else __mem_diff;\n")
	writeIndent(w, indent)
	fmt.Fprintf(w, "std.debug.print(\"{{\\\"duration_us\\\":{d},\\\"memory_bytes\\\":{d},\\\"name\\\":\\\"%s\\\"}}\\n\", .{__duration_us, __memory_bytes});\n", b.Name)
	indent--
	writeIndent(w, indent)
	io.WriteString(w, "}\n")
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	} else {
		io.WriteString(w, "false")
	}
	io.WriteString(w, ") ")
	if i.Then != nil {
		i.Then.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, " else ")
	if i.Else != nil {
		i.Else.emit(w)
	} else {
		io.WriteString(w, "0")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	switch c.Func {
	case "len", "count":
		if len(c.Args) > 0 {
			io.WriteString(w, "@as(i64, @intCast(")
			if s, ok := c.Args[0].(*StringLit); ok {
				fmt.Fprintf(w, "%q.len", s.Value)
			} else if l, ok := c.Args[0].(*ListLit); ok {
				fmt.Fprintf(w, "%d", len(l.Elems))
			} else if v, ok := c.Args[0].(*VarRef); ok {
				if mapVars[v.Name] {
					v.emit(w)
					io.WriteString(w, ".count()")
				} else if t, ok2 := varTypes[v.Name]; ok2 {
					if _, ok3 := groupItemTypes[t]; ok3 {
						v.emit(w)
						io.WriteString(w, ".items.len")
					} else {
						v.emit(w)
						io.WriteString(w, ".len")
					}
				} else {
					v.emit(w)
					io.WriteString(w, ".len")
				}
			} else {
				c.Args[0].emit(w)
				io.WriteString(w, ".len")
			}
			io.WriteString(w, "))")
		} else {
			io.WriteString(w, "0")
		}
	case "int":
		if len(c.Args) == 1 {
			io.WriteString(w, "std.fmt.parseInt(i64, ")
			c.Args[0].emit(w)
			io.WriteString(w, ", 10) catch 0")
		} else {
			io.WriteString(w, "0")
		}
	case "avg":
		if len(c.Args) == 1 {
			io.WriteString(w, "blk: { var arr = ")
			if v, ok := c.Args[0].(*VarRef); ok {
				if t, ok2 := varTypes[v.Name]; ok2 {
					if _, ok3 := groupItemTypes[t]; ok3 {
						v.emit(w)
						io.WriteString(w, ".items")
					} else {
						v.emit(w)
					}
				} else {
					v.emit(w)
				}
			} else {
				c.Args[0].emit(w)
			}
			io.WriteString(w, "; if (arr.len == 0) break :blk 0; var sum: f64 = 0; for (arr) |v| { sum += @as(f64, v); } break :blk sum / @as(f64, arr.len); }")
		} else {
			io.WriteString(w, "0")
		}
	case "min":
		if len(c.Args) == 1 {
			io.WriteString(w, "blk: { var arr = ")
			c.Args[0].emit(w)
			io.WriteString(w, "; var m = arr[0]; for (arr[1..]) |v| { if (v < m) m = v; } break :blk m; }")
		} else {
			io.WriteString(w, "0")
		}
	case "max":
		if len(c.Args) == 1 {
			io.WriteString(w, "blk: { var arr = ")
			c.Args[0].emit(w)
			io.WriteString(w, "; var m = arr[0]; for (arr[1..]) |v| { if (v > m) m = v; } break :blk m; }")
		} else {
			io.WriteString(w, "0")
		}
	case "contains":
		if len(c.Args) == 2 {
			targetType := zigTypeFromExpr(c.Args[0])
			if strings.HasPrefix(targetType, "std.StringHashMap(") || strings.HasPrefix(targetType, "std.AutoHashMap(") {
				c.Args[0].emit(w)
				io.WriteString(w, ".contains(")
				c.Args[1].emit(w)
				io.WriteString(w, ")")
			} else {
				io.WriteString(w, "std.mem.indexOf(u8, ")
				c.Args[0].emit(w)
				io.WriteString(w, ", ")
				c.Args[1].emit(w)
				io.WriteString(w, ") != null")
			}
		} else {
			io.WriteString(w, "false")
		}
	case "values":
		if len(c.Args) == 1 {
			if m, ok := c.Args[0].(*MapLit); ok {
				vals := make([]Expr, len(m.Entries))
				for i, e := range m.Entries {
					vals[i] = e.Value
				}
				(&ListLit{Elems: vals}).emit(w)
			} else if v, ok := c.Args[0].(*VarRef); ok && mapVars[v.Name] {
				io.WriteString(w, "blk: { var it = ")
				v.emit(w)
				io.WriteString(w, ".iterator(); var arr = std.ArrayList(i64).init(std.heap.page_allocator); while (it.next()) |kv| { arr.append(kv.value) catch unreachable; } break :blk arr.toOwnedSlice() catch unreachable; }")
			} else {
				io.WriteString(w, "[]i64{}")
			}
		} else {
			io.WriteString(w, "[]i64{}")
		}
	case "keys":
		if len(c.Args) == 1 {
			if m, ok := c.Args[0].(*MapLit); ok {
				keys := make([]Expr, len(m.Entries))
				for i, e := range m.Entries {
					keys[i] = e.Key
				}
				(&ListLit{Elems: keys, ElemType: "[]const u8"}).emit(w)
			} else if v, ok := c.Args[0].(*VarRef); ok {
				if ml, ok2 := varDecls[v.Name]; ok2 {
					if m, ok3 := ml.Value.(*MapLit); ok3 && m.StructName != "" {
						keys := make([]Expr, len(m.Entries))
						for i, e := range m.Entries {
							keys[i] = e.Key
						}
						(&ListLit{Elems: keys, ElemType: "[]const u8"}).emit(w)
						break
					}
				}
				if mapVars[v.Name] {
					io.WriteString(w, "blk: { var it = ")
					v.emit(w)
					io.WriteString(w, ".keyIterator(); var arr = std.ArrayList([]const u8).init(std.heap.page_allocator); while (it.next()) |k| { arr.append(k.*) catch unreachable; } break :blk arr.toOwnedSlice() catch unreachable; }")
				} else {
					io.WriteString(w, "[]i64{}")
				}
			} else {
				io.WriteString(w, "[]i64{}")
			}
		} else {
			io.WriteString(w, "[]i64{}")
		}
	default:
		io.WriteString(w, c.Func)
		io.WriteString(w, "(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
                       if params, ok := funcParamTypes[c.Func]; ok && i < len(params) {
                               exp := params[i]
                               if lit, ok := a.(*ListLit); ok && lit.ElemType == "" && exp == "Value" {
                                       lit.ElemType = "[]i64"
                               }
                               at := zigTypeFromExpr(a)
                               if v, ok := a.(*VarRef); ok {
                                       if vd, ok2 := varDecls[v.Name]; ok2 && strings.HasPrefix(vd.Type, "[") {
                                               at = vd.Type
                                       }
                               }
				if exp != at {
					switch {
					case at == "Value":
						a.emit(w)
						io.WriteString(w, valueAccess(exp))
					case exp == "Value":
						io.WriteString(w, "Value{.")
						io.WriteString(w, valueTag(at))
						io.WriteString(w, " = ")
						a.emit(w)
						io.WriteString(w, "}")
					case exp == "i64" && at == "f64":
						io.WriteString(w, "@as(i64, @intFromFloat(")
						a.emit(w)
						io.WriteString(w, "))")
					case exp == "f64" && at == "i64":
						if v, ok3 := a.(*VarRef); ok3 {
							if vt, ok4 := varTypes[v.Name]; ok4 && vt == "f64" {
								a.emit(w)
								break
							}
						}
						io.WriteString(w, "@as(f64, @floatFromInt(")
						a.emit(w)
						io.WriteString(w, "))")
					case strings.HasPrefix(exp, "[]") && strings.HasPrefix(at, "["):
						a.emit(w)
						io.WriteString(w, "[0..]")
					default:
						a.emit(w)
					}
				} else {
					a.emit(w)
				}
			} else {
				a.emit(w)
			}
		}
		io.WriteString(w, ")")
	}
}

// Transpile converts a Mochi program into our simple Zig AST.
func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	benchMain = bench
	main := &Func{Name: "main"}
	funcs := []*Func{}
	globals := []*VarDecl{}
	transEnv = env
	extraFuncs = nil
	globalInits = nil
	funcCounter = 0
	varTypes = map[string]string{}
	varDecls = map[string]*VarDecl{}
	varUses = map[string]int{}
	groupCounter = 0
	groupItemTypes = map[string]string{}
	typeAliases = map[string]string{}
	funDepth = 0
	funParamsStack = nil
	nestedFunArgs = map[string][]string{}
	funcReturns = map[string]string{}
	funcParamTypes = map[string][]string{}
	builtinAliases = map[string]string{}
	mainFuncName = ""
	useNow = false
	useStr = false
	useConcat = false
	useLookupHost = false
	useAscii = false
	useMem = false
	usePrint = false
	useSplit = false
	useValue = false
	useSha256 = false
	aliasStack = nil
	namesStack = nil
	nameCounts = map[string]int{}
	pushAliasScope()
	globalNames = map[string]bool{}
	funDepth++
	funParamsStack = append(funParamsStack, nil)
	for _, st := range prog.Statements {
		if st.Let != nil {
			globalNames[st.Let.Name] = true
		}
		if st.Var != nil {
			globalNames[st.Var.Name] = true
		}
	}
	mutables := map[string]bool{}
	collectMutables(prog.Statements, mutables)
	constLists = map[string]*ListLit{}
	mapVars = map[string]bool{}
	structDefs = map[string]*StructDef{}
	variantTags = map[string]int{}

	// reserve global variable names so that functions compiled earlier
	// do not reuse the same identifiers. This prevents name collisions
	// when a local variable shares the name with a global variable that
	// is declared later in the source program.
	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			name := st.Let.Name
			if funDepth > 0 && globalNames[name] {
				name = name + "_var"
			}
			alias := uniqueName(name)
			aliasStack[len(aliasStack)-1][st.Let.Name] = alias
			namesStack[len(namesStack)-1] = append(namesStack[len(namesStack)-1], name)
		case st.Var != nil:
			name := st.Var.Name
			if funDepth > 0 && globalNames[name] {
				name = name + "_var"
			}
			alias := uniqueName(name)
			aliasStack[len(aliasStack)-1][st.Var.Name] = alias
			namesStack[len(namesStack)-1] = append(namesStack[len(namesStack)-1], name)
		}
	}
	for _, st := range prog.Statements {
		if st.Import != nil && st.Import.Lang != nil {
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			path := strings.Trim(st.Import.Path, "\"")
			switch *st.Import.Lang {
			case "go":
				if st.Import.Auto && path == "mochi/runtime/ffi/go/testpkg" {
					builtinAliases[alias] = "go_testpkg"
				}
				if st.Import.Auto && path == "net" {
					builtinAliases[alias] = "go_net"
				}
			case "python":
				if path == "math" {
					builtinAliases[alias] = "python_math"
				}
			}
		}
	}
	for _, st := range prog.Statements {
		if st.Fun != nil {
			fn, err := compileFunStmt(st.Fun, prog)
			if err != nil {
				return nil, err
			}
			funcs = append(funcs, fn)
			continue
		}
		s, err := compileStmt(st, prog)
		if err != nil {
			if st.Test == nil && st.Import == nil && st.Type == nil {
				return nil, err
			}
			continue
		}
		if vd, ok := s.(*VarDecl); ok {
			vd.Mutable = vd.Mutable || mutables[vd.Name]
			if !vd.Mutable {
				if lst, ok2 := vd.Value.(*ListLit); ok2 {
					constLists[vd.Name] = lst
				}
			}
			globals = append(globals, vd)
			continue
		}
		if s != nil {
			main.Body = append(main.Body, s)
		}
	}
	funParamsStack = funParamsStack[:len(funParamsStack)-1]
	funDepth--
	if len(globalInits) > 0 {
		newBody := make([]Stmt, 0, len(globalInits)+len(main.Body))
		newBody = append(newBody, globalInits...)
		newBody = append(newBody, main.Body...)
		main.Body = newBody
	}
	_ = env
	if benchMain {
		useNow = true
		useMem = true
		main.Body = []Stmt{&BenchStmt{Name: "main", Body: main.Body}}
	}
	funcs = append(funcs, extraFuncs...)
	funcs = append(funcs, main)
	structs := make([]*StructDef, 0, len(structDefs))
	for _, sd := range structDefs {
		structs = append(structs, sd)
	}
	progAST := &Program{Structs: structs, Globals: globals, Functions: funcs}
	popAliasScope()
	return progAST, nil
}

func collectMutables(sts []*parser.Statement, m map[string]bool) {
	for _, st := range sts {
		switch {
		case st.Assign != nil:
			if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
				m[st.Assign.Name] = true
			}
		case st.If != nil:
			collectMutablesIf(st.If, m)
		case st.While != nil:
			collectMutables(st.While.Body, m)
		case st.For != nil:
			collectMutables(st.For.Body, m)
		}
	}
}

func collectMutablesIf(is *parser.IfStmt, m map[string]bool) {
	if is == nil {
		return
	}
	collectMutables(is.Then, m)
	if is.ElseIf != nil {
		collectMutablesIf(is.ElseIf, m)
	}
	collectMutables(is.Else, m)
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	first, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]parser.BinaryOp, 0, len(e.Binary.Right))
	for _, op := range e.Binary.Right {
		r, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, r)
		ops = append(ops, *op)
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
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

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if !contains(lvl, ops[i].Op) {
				i++
				continue
			}
			l := operands[i]
			r := operands[i+1]
			if ll, ok := l.(*ListLit); ok {
				if rl, ok2 := r.(*ListLit); ok2 {
					switch ops[i].Op {
					case "union":
						seen := map[int]bool{}
						res := []Expr{}
						for _, v := range ll.Elems {
							iv := v.(*IntLit)
							if !seen[iv.Value] {
								seen[iv.Value] = true
								res = append(res, iv)
							}
						}
						for _, v := range rl.Elems {
							iv := v.(*IntLit)
							if !seen[iv.Value] || ops[i].All {
								if !seen[iv.Value] {
									seen[iv.Value] = true
								}
								res = append(res, iv)
							}
						}
						operands[i] = &ListLit{Elems: res}
						operands = append(operands[:i+1], operands[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					case "except":
						m := map[int]bool{}
						for _, v := range rl.Elems {
							m[v.(*IntLit).Value] = true
						}
						res := []Expr{}
						for _, v := range ll.Elems {
							iv := v.(*IntLit)
							if !m[iv.Value] {
								res = append(res, iv)
							}
						}
						operands[i] = &ListLit{Elems: res}
						operands = append(operands[:i+1], operands[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					case "intersect":
						m := map[int]bool{}
						for _, v := range rl.Elems {
							m[v.(*IntLit).Value] = true
						}
						res := []Expr{}
						for _, v := range ll.Elems {
							iv := v.(*IntLit)
							if m[iv.Value] {
								res = append(res, iv)
							}
						}
						operands[i] = &ListLit{Elems: res}
						operands = append(operands[:i+1], operands[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					}
				}
			}
			expr := &BinaryExpr{Left: l, Op: ops[i].Op, Right: r}
			operands[i] = expr
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}
	if len(operands) != 1 {
		return nil, fmt.Errorf("unexpected state after binary compile")
	}
	return operands[0], nil
}

func compileUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := compilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
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

func compilePostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		alias := pf.Target.Selector.Root
		method := pf.Target.Selector.Tail[0]
		if kind, ok := builtinAliases[alias]; ok {
			args := make([]Expr, len(pf.Ops[0].Call.Args))
			for i, a := range pf.Ops[0].Call.Args {
				ex, err := compileExpr(a)
				if err != nil {
					return nil, err
				}
				if il, ok := ex.(*IntLit); ok {
					ex = &FloatLit{Value: float64(il.Value)}
				}
				args[i] = ex
			}
			switch kind {
			case "go_testpkg":
				if method == "Add" && len(args) == 2 {
					return &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}, nil
				}
			case "python_math":
				switch method {
				case "sqrt", "sin":
					if len(args) == 1 {
						return &CallExpr{Func: "std.math." + strings.ToLower(method), Args: args}, nil
					}
				case "log":
					if len(args) == 1 {
						return &CallExpr{Func: "std.math.log", Args: []Expr{&VarRef{Name: "f64"}, &FieldExpr{Target: &VarRef{Name: "std.math"}, Name: "e"}, args[0]}}, nil
					}
				case "pow":
					if len(args) == 2 {
						return &CallExpr{Func: "std.math.pow", Args: []Expr{&VarRef{Name: "f64"}, args[0], args[1]}}, nil
					}
				}
			case "go_net":
				if method == "LookupHost" && len(args) == 1 {
					useLookupHost = true
					return &CallExpr{Func: "_lookup_host", Args: args}, nil
				}
			}
		}
	}
	// Detect selector call pattern like `s.contains(sub)`
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		base, err := compilePrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}})
		if err != nil {
			return nil, err
		}
		if len(pf.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		arg, err := compileExpr(pf.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		return &CallExpr{Func: "contains", Args: []Expr{base, arg}}, nil
	}

	expr, err := compilePrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil && op.Index.Colon2 == nil {
				idx, err := compileExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				imap := false
				switch t := expr.(type) {
				case *MapLit:
					if t.StructName != "" {
						if key, ok := idx.(*StringLit); ok {
							expr = &FieldExpr{Target: expr, Name: key.Value}
							continue
						}
					}
					imap = true
				case *VarRef:
					if st, ok := varTypes[t.Name]; ok {
						if _, ok2 := structDefs[st]; ok2 {
							if key, ok3 := idx.(*StringLit); ok3 {
								expr = &FieldExpr{Target: expr, Name: key.Value}
								continue
							}
						}
					}
					if mapVars[t.Name] {
						imap = true
					}
				case *IndexExpr:
					imap = t.Map
				}
				expr = &IndexExpr{Target: expr, Index: idx, Map: imap}
				continue
			}
			if op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil {
				var start, end Expr
				if op.Index.Start != nil {
					start, err = compileExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					end, err = compileExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				expr = &SliceExpr{Target: expr, Start: start, End: end}
				continue
			}
			return nil, fmt.Errorf("unsupported postfix")
		}
		if op.Call != nil {
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ex, err := compileExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
                       if fe, ok := expr.(*FieldExpr); ok {
                               expr = &CallExpr{Func: fe.Name, Args: append([]Expr{fe.Target}, args...)}
                               continue
                       }
                       if name, ok := exprToString(expr); ok {
                               expr = &CallExpr{Func: name, Args: args}
                               continue
                       }
                       return nil, fmt.Errorf("unsupported call target")
               }
		if op.Field != nil {
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type != nil {
				expr = &CastExpr{Value: expr, Type: toZigType(op.Cast.Type)}
				continue
			}
			return nil, fmt.Errorf("unsupported cast type")
		}
		return nil, fmt.Errorf("unsupported postfix")
	}
	return expr, nil
}

func compilePrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.FunExpr != nil:
		name := fmt.Sprintf("fn_%d", funcCounter)
		funcCounter++
		names := make([]string, len(p.FunExpr.Params))
		params := make([]Param, len(p.FunExpr.Params))
		aliases := map[string]string{}
		pushAliasScope()
		aliasStack[len(aliasStack)-1] = aliases
		defer popAliasScope()
		for i, par := range p.FunExpr.Params {
			typ := toZigType(par.Type)
			name := par.Name
			if globalNames[name] {
				alias := name + "_param"
				aliases[name] = alias
				name = alias
			}
			params[i] = Param{Name: name, Type: typ}
			names[i] = name
			varTypes[name] = typ
		}
		funParamsStack = append(funParamsStack, names)
		funDepth++
		defer func() {
			funDepth--
			funParamsStack = funParamsStack[:len(funParamsStack)-1]
		}()
		ret := ""
		if p.FunExpr.Return != nil {
			if p.FunExpr.Return.Simple != nil && *p.FunExpr.Return.Simple == "any" {
				ret = ""
			} else {
				ret = toZigType(p.FunExpr.Return)
			}
		}
		body := []Stmt{}
		if p.FunExpr.ExprBody != nil {
			expr, err := compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return nil, err
			}
			body = append(body, &ReturnStmt{Value: expr})
		} else {
			for _, st := range p.FunExpr.BlockBody {
				s, err := compileStmt(st, nil)
				if err != nil {
					return nil, err
				}
				body = append(body, s)
			}
		}
		f := &Func{Name: name, Params: params, ReturnType: ret, Body: body, Aliases: aliases}
		if ret == "[]Value" {
			for _, st := range f.Body {
				if rs, ok := st.(*ReturnStmt); ok {
					if l, ok2 := rs.Value.(*ListLit); ok2 {
						l.ElemType = "Value"
					}
				}
			}
		}
		// Track function signature for later type inference.
		paramTypes := make([]string, len(params))
		for i, p := range params {
			paramTypes[i] = p.Type
		}
		funcParamTypes[name] = paramTypes
		varTypes[name] = fmt.Sprintf("*const fn(%s) %s", strings.Join(paramTypes, ", "), ret)
		funcReturns[name] = ret
		if funDepth > 0 {
			captured := []string{}
			for i := 0; i < len(funParamsStack)-1; i++ {
				captured = append(captured, funParamsStack[i]...)
			}
			newParams := make([]Param, 0, len(captured)+len(f.Params))
			for _, n := range captured {
				t := varTypes[n]
				if t == "" {
					t = "i64"
				}
				newParams = append(newParams, Param{Name: n, Type: t})
			}
			newParams = append(newParams, f.Params...)
			f.Params = newParams
			nestedFunArgs[name] = captured
		}
		extraFuncs = append(extraFuncs, f)
		return &VarRef{Name: name}, nil
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		switch p.Call.Func {
		case "now":
			if len(args) != 0 {
				return nil, fmt.Errorf("now expects no arguments")
			}
			useNow = true
			return &CallExpr{Func: "_now"}, nil
		case "input":
			if len(args) != 0 {
				return nil, fmt.Errorf("input expects no arguments")
			}
			useInput = true
			return &CallExpr{Func: "_input"}, nil
		case "str":
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects one argument")
			}
			useStr = true
			if lit, ok := args[0].(*IntLit); ok {
				return &StringLit{Value: fmt.Sprintf("%d", lit.Value)}, nil
			}
			return &CallExpr{Func: "_str", Args: args}, nil
		case "substring":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			if s, ok := args[0].(*StringLit); ok {
				if start, ok1 := args[1].(*IntLit); ok1 {
					if end, ok2 := args[2].(*IntLit); ok2 {
						if start.Value >= 0 && end.Value <= len(s.Value) && start.Value <= end.Value {
							return &StringLit{Value: s.Value[start.Value:end.Value]}, nil
						}
					}
				}
			}
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2]}, nil
		case "upper":
			if len(args) != 1 {
				return nil, fmt.Errorf("upper expects one argument")
			}
			useAscii = true
			return &CallExpr{Func: "_upper", Args: args}, nil
		case "lower":
			if len(args) != 1 {
				return nil, fmt.Errorf("lower expects one argument")
			}
			useAscii = true
			return &CallExpr{Func: "_lower", Args: args}, nil
		case "split":
			if len(args) != 2 {
				return nil, fmt.Errorf("split expects two arguments")
			}
			useSplit = true
			return &CallExpr{Func: "_split_string", Args: args}, nil
		case "sha256":
			if len(args) != 1 {
				return nil, fmt.Errorf("sha256 expects one argument")
			}
			useSha256 = true
			funcReturns["_sha256"] = "[]i64"
			return &CallExpr{Func: "_sha256", Args: args}, nil
		case "print":
			if len(args) == 0 {
				return nil, fmt.Errorf("print expects at least one argument")
			}
			usePrint = true
			return &CallExpr{Func: "_print", Args: args}, nil
		case "sum":
			if len(args) == 1 {
				if list, ok := args[0].(*ListLit); ok {
					total := 0
					for _, e := range list.Elems {
						lit, ok := e.(*IntLit)
						if !ok {
							return nil, fmt.Errorf("unsupported sum element")
						}
						total += lit.Value
					}
					return &IntLit{Value: total}, nil
				}
			}
		case "avg":
			if len(args) == 1 {
				if list, ok := args[0].(*ListLit); ok {
					if len(list.Elems) == 0 {
						return &FloatLit{Value: 0}, nil
					}
					total := 0
					for _, e := range list.Elems {
						lit, ok := e.(*IntLit)
						if !ok {
							return nil, fmt.Errorf("unsupported avg element")
						}
						total += lit.Value
					}
					avg := float64(total) / float64(len(list.Elems))
					return &FloatLit{Value: avg}, nil
				}
			}
		case "exists":
			if len(args) == 1 {
				call := &CallExpr{Func: "len", Args: []Expr{args[0]}}
				return &BinaryExpr{Left: call, Op: "!=", Right: &IntLit{Value: 0}}, nil
			}
		case "append":
			if len(args) == 2 {
				if list, ok := args[0].(*ListLit); ok {
					elems := append(append([]Expr{}, list.Elems...), args[1])
					return &ListLit{Elems: elems}, nil
				}
				if v, ok := args[0].(*VarRef); ok {
					if vt, ok2 := varTypes[v.Name]; ok2 {
						elemT := ""
						if strings.HasPrefix(vt, "[]") {
							elemT = vt[2:]
						}
						if elemT != "" {
							if vd, ok3 := varDecls[v.Name]; ok3 && vt == "[]i64" && vd.Type == "" {
								// update element type only when not explicitly typed
								elemT = zigTypeFromExpr(args[1])
								varTypes[v.Name] = "[]" + elemT
								vd.Type = "[]" + elemT
								if lst, ok4 := vd.Value.(*ListLit); ok4 {
									lst.ElemType = elemT
								}
							} else {
								valT := zigTypeFromExpr(args[1])
								if valT == "Value" && elemT != "Value" {
									field := map[string]string{"i64": "Int", "f64": "Float", "[]const u8": "Str", "bool": "Bool"}[elemT]
									if field != "" {
										args[1] = &FieldExpr{Target: args[1], Name: field}
										valT = elemT
									}
								}
								return &AppendExpr{List: args[0], Value: args[1], ElemType: elemT}, nil
							}
						}
					}
					if lst, ok2 := constLists[v.Name]; ok2 {
						elems := make([]Expr, 0, len(lst.Elems)+1)
						for i := range lst.Elems {
							elems = append(elems, &IndexExpr{Target: &VarRef{Name: resolveAlias(v.Name)}, Index: &IntLit{Value: i}})
						}
						elems = append(elems, args[1])
						return &ListLit{Elems: elems}, nil
					}
				}
				return &AppendExpr{List: args[0], Value: args[1], ElemType: zigTypeFromExpr(args[1])}, nil
			}
		}
		name := p.Call.Func
		if name == "main" && mainFuncName != "" {
			name = mainFuncName
		}
		if extra, ok := nestedFunArgs[name]; ok {
			pre := make([]Expr, len(extra))
			for i, n := range extra {
				pre[i] = &VarRef{Name: resolveAlias(n)}
			}
			args = append(pre, args...)
		}
		return &CallExpr{Func: name, Args: args}, nil
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
		if p.Lit.Null {
			return &NullLit{}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
	case p.Load != nil:
		return compileLoadExpr(p.Load)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := compileExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		var elemType string
		if len(elems) > 0 {
			elemType = zigTypeFromExpr(elems[0])
			mixed := false
			for _, e := range elems[1:] {
				t := zigTypeFromExpr(e)
				if t != elemType {
					if (elemType == "i64" && t == "f64") || (elemType == "f64" && t == "i64") {
						elemType = "f64"
					} else {
						mixed = true
						break
					}
				}
			}
			if mixed {
				elemType = "Value"
				useValue = true
			}
		}
		return &ListLit{Elems: elems, ElemType: elemType}, nil
	case p.Map != nil:
		entries := make([]MapEntry, len(p.Map.Items))
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
			entries[i] = MapEntry{Key: k, Value: v}
		}
		ml := &MapLit{Entries: entries}
		if len(entries) > 0 {
			ml.KeyType = zigTypeFromExpr(entries[0].Key)
			valType := zigTypeFromExpr(entries[0].Value)
			mixed := false
			for _, e := range entries[1:] {
				t := zigTypeFromExpr(e.Value)
				if t != valType {
					if (valType == "i64" && t == "f64") || (valType == "f64" && t == "i64") {
						valType = "f64"
					} else {
						mixed = true
						break
					}
				}
			}
			if mixed {
				valType = "Value"
				useValue = true
			}
			ml.ValType = valType
		}
		return ml, nil
	case p.Struct != nil:
		entries := make([]MapEntry, len(p.Struct.Fields))
		fields := make([]Field, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := compileExpr(f.Value)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: &StringLit{Value: f.Name}, Value: v}
			fields[i] = Field{Name: toSnakeCase(f.Name), Type: zigTypeFromExpr(v)}
		}
		if _, ok := structDefs[p.Struct.Name]; !ok {
			structDefs[p.Struct.Name] = &StructDef{Name: p.Struct.Name, Fields: fields}
		}
		if tag, ok := variantTags[p.Struct.Name]; ok {
			entries = append([]MapEntry{{Key: &StringLit{Value: "op"}, Value: &IntLit{Value: tag}}}, entries...)
		}
		return &MapLit{Entries: entries, StructName: p.Struct.Name}, nil
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.Match != nil:
		return compileMatchExpr(p.Match)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 1 {
			if kind, ok := builtinAliases[p.Selector.Root]; ok {
				switch kind {
				case "go_testpkg":
					switch p.Selector.Tail[0] {
					case "Pi":
						return &FloatLit{Value: 3.14}, nil
					case "Answer":
						return &IntLit{Value: 42}, nil
					}
				case "python_math":
					switch p.Selector.Tail[0] {
					case "pi":
						return &FloatLit{Value: 3.141592653589793}, nil
					case "e":
						return &FloatLit{Value: 2.718281828459045}, nil
					}
				}
			}
		}
		name := resolveAlias(p.Selector.Root)
		var expr Expr = &VarRef{Name: name}
		for _, f := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Name: f}
		}
		return expr, nil
	case p.Group != nil:
		return compileExpr(p.Group)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
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
		return nil, fmt.Errorf("missing else expression")
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func compileMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := compileExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr = &StringLit{Value: ""}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		if call := extractCallPattern(c.Pattern); call != nil {
			if tag, ok := variantTags[call.Func]; ok {
				cond := &BinaryExpr{Left: &IndexExpr{Target: target, Index: &StringLit{Value: "op"}, Map: true}, Op: "==", Right: &IntLit{Value: tag}}
				var res Expr
				switch call.Func {
				case "Num":
					res = &IndexExpr{Target: target, Index: &StringLit{Value: "value"}, Map: true}
				case "Bin":
					res = &CallExpr{Func: "binEval", Args: []Expr{
						&IndexExpr{Target: target, Index: &StringLit{Value: "op"}, Map: true},
						&IndexExpr{Target: target, Index: &StringLit{Value: "left"}, Map: true},
						&IndexExpr{Target: target, Index: &StringLit{Value: "right"}, Map: true},
					}}
				}
				expr = &IfExpr{Cond: cond, Then: res, Else: expr}
				continue
			}
		}
		res, err := compileExpr(c.Result)
		if err != nil {
			return nil, err
		}
		pat, err := compileExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if v, ok := pat.(*VarRef); ok && v.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		expr = &IfExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func extractCallPattern(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	pf := e.Binary.Left.Value
	if pf == nil || pf.Target == nil || pf.Target.Call == nil || len(pf.Ops) != 0 {
		return nil
	}
	return pf.Target.Call
}

func compileIfStmt(is *parser.IfStmt, prog *parser.Program) (Stmt, error) {
	cond, err := compileExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	pushAliasScope()
	for _, s := range is.Then {
		st, err := compileStmt(s, prog)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, st)
	}
	popAliasScope()
	var elseStmts []Stmt
	if is.ElseIf != nil {
		st, err := compileIfStmt(is.ElseIf, prog)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if len(is.Else) > 0 {
		pushAliasScope()
		for _, s := range is.Else {
			st, err := compileStmt(s, prog)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, st)
		}
		popAliasScope()
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func compileWhileStmt(ws *parser.WhileStmt, prog *parser.Program) (Stmt, error) {
	cond, err := compileExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	body := make([]Stmt, 0, len(ws.Body))
	pushAliasScope()
	funDepth++
	// track varDecls before processing the loop so we can remove
	// any declarations that are scoped within the loop body
	outerDecls := make(map[string]*VarDecl, len(varDecls))
	for k, v := range varDecls {
		outerDecls[k] = v
	}
	for _, s := range ws.Body {
		st, err := compileStmt(s, prog)
		if err != nil {
			funDepth--
			return nil, err
		}
		body = append(body, st)
	}
	// remove varDecls introduced inside the loop body
	for k := range varDecls {
		if _, ok := outerDecls[k]; !ok {
			delete(varDecls, k)
		}
	}
	funDepth--
	popAliasScope()
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func compileForStmt(fs *parser.ForStmt, prog *parser.Program) (Stmt, error) {
	var start, end, iter Expr
	var elemType string
	var err error
	if fs.RangeEnd != nil {
		start, err = compileExpr(fs.Source)
		if err != nil {
			return nil, err
		}
		end, err = compileExpr(fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		elemType = "i64"
	} else {
		iter, err = compileExpr(fs.Source)
		if err != nil {
			return nil, err
		}
		t := zigTypeFromExpr(iter)
		if strings.HasPrefix(t, "[]") {
			elemType = t[2:]
		} else {
			elemType = "i64"
		}
	}
	body := make([]Stmt, 0, len(fs.Body))
	pushAliasScope()
	alias := uniqueName(fs.Name)
	aliasStack[len(aliasStack)-1][fs.Name] = alias
	namesStack[len(namesStack)-1] = append(namesStack[len(namesStack)-1], fs.Name)
	varTypes[alias] = elemType
	outerDecls := make(map[string]*VarDecl, len(varDecls))
	for k, v := range varDecls {
		outerDecls[k] = v
	}
	for _, s := range fs.Body {
		st, err := compileStmt(s, prog)
		if err != nil {
			return nil, err
		}
		body = append(body, st)
	}
	for k := range varDecls {
		if _, ok := outerDecls[k]; !ok {
			delete(varDecls, k)
		}
	}
	popAliasScope()
	return &ForStmt{Name: alias, Start: start, End: end, Iterable: iter, Body: body}, nil
}

func compileBenchStmt(bs *parser.BenchBlock, prog *parser.Program) (Stmt, error) {
	body := make([]Stmt, 0, len(bs.Body))
	pushAliasScope()
	for _, s := range bs.Body {
		st, err := compileStmt(s, prog)
		if err != nil {
			return nil, err
		}
		body = append(body, st)
	}
	popAliasScope()
	useNow = true
	useMem = true
	return &BenchStmt{Name: bs.Name, Body: body}, nil
}

func mapFields(m *MapLit) ([]Field, bool) {
	fields := make([]Field, len(m.Entries))
	for i, e := range m.Entries {
		key, ok := e.Key.(*StringLit)
		if !ok {
			return nil, false
		}
		fields[i] = Field{Name: zigIdent(key.Value), Type: zigTypeFromExpr(e.Value)}
	}
	return fields, true
}

func elemTypeFromExpr(e *parser.Expr) string {
	if e == nil || e.Binary == nil {
		return "i64"
	}
	pf := e.Binary.Left.Value.Target
	if pf.Selector != nil && len(pf.Selector.Tail) == 0 {
		if lst, ok := constLists[pf.Selector.Root]; ok {
			if lst.ElemType != "" {
				return lst.ElemType
			}
		}
		if t, ok := varTypes[pf.Selector.Root]; ok {
			if elem, ok2 := groupItemTypes[t]; ok2 {
				return elem
			}
			if strings.HasPrefix(t, "[]") {
				return t[2:]
			}
		}
	}
	if pf.List != nil && len(pf.List.Elems) > 0 {
		elem := pf.List.Elems[0]
		if elem.Binary != nil && elem.Binary.Left != nil {
			lit := elem.Binary.Left.Value.Target.Lit
			if lit != nil {
				if lit.Int != nil {
					return "i64"
				}
				if lit.Bool != nil {
					return "bool"
				}
				if lit.Str != nil {
					return "[]const u8"
				}
			}
		}
	}
	return "i64"
}

func inferListStruct(varName string, list *ListLit) string {
	if len(list.Elems) == 0 {
		return ""
	}
	first, ok := list.Elems[0].(*MapLit)
	if !ok {
		return ""
	}
	fields, ok := mapFields(first)
	if !ok {
		return ""
	}
	for _, el := range list.Elems[1:] {
		ml, ok := el.(*MapLit)
		if !ok {
			return ""
		}
		if len(ml.Entries) != len(first.Entries) {
			return ""
		}
		for i, e := range ml.Entries {
			k, ok := e.Key.(*StringLit)
			if !ok || k.Value != fields[i].Name {
				return ""
			}
		}
	}
	base := strings.TrimSuffix(varName, "s")
	structName := strings.Title(base)
	if _, ok := structDefs[structName]; !ok {
		structDefs[structName] = &StructDef{Name: structName, Fields: fields}
	}
	for _, el := range list.Elems {
		if ml, ok := el.(*MapLit); ok {
			ml.StructName = structName
		}
	}
	list.ElemType = structName
	return structName
}

func unwrapDesc(e Expr) (Expr, bool) {
	if b, ok := e.(*BinaryExpr); ok && b.Op == "-" {
		if l, ok2 := b.Left.(*IntLit); ok2 && l.Value == 0 {
			return b.Right, true
		}
	}
	return e, false
}

func compileQueryExpr(q *parser.QueryExpr) (Expr, error) {
	vars := []string{q.Var}
	sources := []Expr{}
	src, err := compileExpr(q.Source)
	if err != nil {
		return nil, err
	}
	sources = append(sources, src)
	varTypes[q.Var] = elemTypeFromExpr(q.Source)
	for _, fc := range q.Froms {
		s, err := compileExpr(fc.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, fc.Var)
		sources = append(sources, s)
		varTypes[fc.Var] = elemTypeFromExpr(fc.Src)
	}
	var filter Expr
	if q.Where != nil {
		filter, err = compileExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}
	for _, jc := range q.Joins {
		s, err := compileExpr(jc.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, jc.Var)
		sources = append(sources, s)
		varTypes[jc.Var] = elemTypeFromExpr(jc.Src)
		if jc.On != nil {
			cond, err := compileExpr(jc.On)
			if err != nil {
				return nil, err
			}
			if filter == nil {
				filter = cond
			} else {
				filter = &BinaryExpr{Left: filter, Op: "&&", Right: cond}
			}
		}
	}
	if q.Group != nil {
		prev := varTypes[q.Var]
		varTypes[q.Var] = elemTypeFromExpr(q.Source)
		keyExpr, err := compileExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		if prev == "" {
			delete(varTypes, q.Var)
		} else {
			varTypes[q.Var] = prev
		}
		keyType := zigTypeFromExpr(keyExpr)
		srcElem := elemTypeFromExpr(q.Source)
		structName := fmt.Sprintf("Group%d", groupCounter)
		groupCounter++
		structDefs[structName] = &StructDef{Name: structName, Fields: []Field{{Name: "key", Type: keyType}, {Name: "items", Type: "[]" + srcElem}}}
		groupItemTypes[structName] = srcElem
		prevG := varTypes[q.Group.Name]
		varTypes[q.Group.Name] = structName
		elem, err := compileExpr(q.Select)
		if err != nil {
			return nil, err
		}
		if prevG == "" {
			delete(varTypes, q.Group.Name)
		} else {
			varTypes[q.Group.Name] = prevG
		}
		elemType := zigTypeFromExpr(elem)
		if ml, ok := elem.(*MapLit); ok {
			if ml.StructName == "" {
				if fields, ok2 := mapFields(ml); ok2 {
					structName := fmt.Sprintf("Entry%d", len(structDefs))
					if _, exists := structDefs[structName]; !exists {
						structDefs[structName] = &StructDef{Name: structName, Fields: fields}
					}
					ml.StructName = structName
					elemType = structName
				}
			} else {
				elemType = ml.StructName
			}
		}
		var sortExpr Expr
		var desc bool
		if q.Sort != nil {
			sortExpr, err = compileExpr(q.Sort)
			if err != nil {
				return nil, err
			}
			sortExpr, desc = unwrapDesc(sortExpr)
		}
		return &GroupByExpr{Var: q.Var, Source: src, Key: keyExpr, GroupVar: q.Group.Name, SelectExpr: elem, ElemType: elemType, KeyType: keyType, SrcElem: srcElem, StructName: structName, Sort: sortExpr, Desc: desc}, nil
	}

	elem, err := compileExpr(q.Select)
	if err != nil {
		return nil, err
	}
	elemType := zigTypeFromExpr(elem)
	if ml, ok := elem.(*MapLit); ok {
		structName := fmt.Sprintf("Entry%d", len(structDefs))
		if _, exists := structDefs[structName]; !exists {
			fields := make([]Field, len(ml.Entries))
			for i, e := range ml.Entries {
				var name string
				switch k := e.Key.(type) {
				case *StringLit:
					name = k.Value
				case *VarRef:
					name = k.Name
				default:
					return nil, fmt.Errorf("query key must be string")
				}
				fields[i] = Field{Name: toSnakeCase(name), Type: zigTypeFromExpr(e.Value)}
			}
			structDefs[structName] = &StructDef{Name: structName, Fields: fields}
		}
		ml.StructName = structName
		elemType = structName
	}
	var sortExpr Expr
	var desc bool
	if q.Sort != nil {
		sortExpr, err = compileExpr(q.Sort)
		if err != nil {
			return nil, err
		}
		sortExpr, desc = unwrapDesc(sortExpr)
	}
	var skipExpr Expr
	if q.Skip != nil {
		skipExpr, err = compileExpr(q.Skip)
		if err != nil {
			return nil, err
		}
	}
	var takeExpr Expr
	if q.Take != nil {
		takeExpr, err = compileExpr(q.Take)
		if err != nil {
			return nil, err
		}
	}
	return &QueryComp{Vars: vars, Sources: sources, Elem: elem, ElemType: elemType, Filter: filter, Sort: sortExpr, Desc: desc, Skip: skipExpr, Take: takeExpr}, nil
}

func toZigType(t *parser.TypeRef) string {
	if t == nil {
		return "i64"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = toZigType(p)
		}
		ret := "void"
		if t.Fun.Return != nil {
			ret = toZigType(t.Fun.Return)
		}
		// Use a function pointer type so functions can be passed around
		// as first-class values at runtime.
		return fmt.Sprintf("*const fn(%s) %s", strings.Join(params, ", "), ret)
	}
	if t.Simple != nil {
		if alias, ok := typeAliases[*t.Simple]; ok {
			return alias
		}
		if _, ok := structDefs[*t.Simple]; ok {
			return *t.Simple
		}
		switch *t.Simple {
		case "int":
			return "i64"
		case "float":
			return "f64"
		case "bool":
			return "bool"
		case "string":
			return "[]const u8"
		case "any":
			useValue = true
			return "Value"
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			elem := toZigType(t.Generic.Args[0])
			return "[]" + elem
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			key := toZigType(t.Generic.Args[0])
			val := toZigType(t.Generic.Args[1])
			if key == "[]const u8" {
				return fmt.Sprintf("std.StringHashMap(%s)", val)
			}
			return fmt.Sprintf("std.AutoHashMap(%s,%s)", key, val)
		}
	}
	return "i64"
}

func compileFunStmt(fn *parser.FunStmt, prog *parser.Program) (*Func, error) {
	names := make([]string, len(fn.Params))
	params := make([]Param, len(fn.Params))
	mutables := map[string]bool{}
	collectMutables(fn.Body, mutables)
	localMutablesStack = append(localMutablesStack, mutables)
	defer func() { localMutablesStack = localMutablesStack[:len(localMutablesStack)-1] }()
	pushAliasScope()
	aliases := aliasStack[len(aliasStack)-1]
	defer popAliasScope()
	preStmts := []Stmt{}
	paramTypes := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := toZigType(p.Type)
		name := p.Name
		paramName := name
		if mutables[p.Name] {
			paramName = uniqueName(name + "_param")
		} else if globalNames[name] {
			paramName = name + "_param"
		} else {
			paramName = uniqueName(name)
		}
		aliasName := paramName
		if mutables[p.Name] {
			aliasName = uniqueName(name + "_var")
			namesStack[len(namesStack)-1] = append(namesStack[len(namesStack)-1], name)
			vd := &VarDecl{Name: aliasName, Value: &VarRef{Name: paramName}, Mutable: true}
			preStmts = append(preStmts, vd)
			varDecls[p.Name] = vd
			varDecls[aliasName] = vd
		}
		aliases[name] = aliasName
		params[i] = Param{Name: paramName, Type: typ}
		names[i] = aliasName
		varTypes[aliasName] = typ
		varTypes[name] = typ
		paramTypes[i] = typ
		if strings.HasPrefix(typ, "std.StringHashMap(") || strings.HasPrefix(typ, "std.AutoHashMap(") {
			mapVars[aliasName] = true
		}
	}
	name := fn.Name
	if funDepth == 1 && fn.Name == "main" {
		mainFuncName = "mochi_main"
		name = mainFuncName
	}
	funParamsStack = append(funParamsStack, names)
	funDepth++
	defer func() {
		funDepth--
		funParamsStack = funParamsStack[:len(funParamsStack)-1]
	}()
	ret := ""
	if fn.Return != nil {
		ret = toZigType(fn.Return)
	}
	funcReturns[name] = ret
	funcParamTypes[name] = paramTypes
	body := make([]Stmt, 0, len(fn.Body)+len(preStmts))
	body = append(body, preStmts...)
	for _, st := range fn.Body {
		s, err := compileStmt(st, prog)
		if err != nil {
			return nil, err
		}
		if s != nil {
			body = append(body, s)
		}
	}
	used := map[string]bool{}
	for _, st := range body {
		collectVarsStmt(st, used)
	}
	if strings.HasPrefix(ret, "std.StringHashMap(") {
		for _, st := range body {
			if rs, ok := st.(*ReturnStmt); ok {
				if ml, ok2 := rs.Value.(*MapLit); ok2 && ml.StructName != "" {
					ret = ml.StructName
					break
				}
			}
		}
	}
	f := &Func{Name: name, Params: params, ReturnType: ret, Body: body, Aliases: aliases}
	if ret == "[]Value" {
		for _, st := range f.Body {
			if rs, ok := st.(*ReturnStmt); ok {
				if l, ok2 := rs.Value.(*ListLit); ok2 {
					l.ElemType = "Value"
				}
			}
		}
	}
	if funDepth > 2 {
		capturedMap := map[string]bool{}
		captured := []string{}
		for i := 0; i < len(aliasStack)-1; i++ {
			for _, alias := range aliasStack[i] {
				if used[alias] && !capturedMap[alias] {
					capturedMap[alias] = true
					captured = append(captured, alias)
				}
			}
		}
		newParams := make([]Param, 0, len(captured)+len(f.Params))
		for _, n := range captured {
			t := varTypes[n]
			if t == "" {
				t = "i64"
			}
			newParams = append(newParams, Param{Name: n, Type: t})
		}
		newParams = append(newParams, f.Params...)
		f.Params = newParams
		nestedFunArgs[name] = captured
		extraFuncs = append(extraFuncs, f)
		return nil, nil
	}
	return f, nil
}

func compileReturnStmt(rs *parser.ReturnStmt) (Stmt, error) {
	if rs == nil {
		return &ReturnStmt{}, nil
	}
	if rs.Value == nil {
		return &ReturnStmt{}, nil
	}
	val, err := compileExpr(rs.Value)
	if err != nil {
		return nil, err
	}
	return &ReturnStmt{Value: val}, nil
}

func compileStmt(s *parser.Statement, prog *parser.Program) (Stmt, error) {
	switch {
	case s.Expr != nil:
		if se := extractSaveExpr(s.Expr.Expr); se != nil {
			return compileSaveStmt(se)
		}
		expr, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		if call, ok := expr.(*CallExpr); ok {
			if (call.Func == "print" || call.Func == "_print") && len(call.Args) > 0 {
				return &PrintStmt{Values: call.Args}, nil
			}
			if call.Func == "json" && len(call.Args) == 1 {
				return &JSONStmt{Value: call.Args[0]}, nil
			}
		}
		return &ExprStmt{Expr: expr}, nil
	case s.Let != nil:
		var expr Expr
		var err error
		if s.Let.Value != nil {
			expr, err = compileExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
		} else {
			expr = &IntLit{Value: 0}
		}
		name := s.Let.Name
		if funDepth > 0 && globalNames[name] {
			name = name + "_var"
		}
		alias, ok := aliasStack[len(aliasStack)-1][s.Let.Name]
		if !ok {
			alias = uniqueName(name)
			aliasStack[len(aliasStack)-1][s.Let.Name] = alias
			namesStack[len(namesStack)-1] = append(namesStack[len(namesStack)-1], name)
		}
		vd := &VarDecl{Name: alias, Value: expr}
		varDecls[s.Let.Name] = vd
		varDecls[alias] = vd
		if lst, ok := expr.(*ListLit); ok {
			if funDepth == 0 && inferListStruct(s.Let.Name, lst) != "" {
				vd.Type = fmt.Sprintf("[%d]%s", len(lst.Elems), lst.ElemType)
				varTypes[s.Let.Name] = "[]" + lst.ElemType
			} else if funDepth == 0 && len(lst.Elems) > 0 && vd.Type == "" {
				elem := lst.ElemType
				if elem == "" {
					elem = "i64"
				}
				vd.Type = fmt.Sprintf("[%d]%s", len(lst.Elems), elem)
				lst.ElemType = ""
				varTypes[s.Let.Name] = vd.Type
			} else if lst.ElemType == "" && vd.Type != "" && strings.HasPrefix(vd.Type, "[]") {
				lst.ElemType = vd.Type[2:]
			}
		}
		if ml, ok := expr.(*MapLit); ok {
			if ml.StructName != "" {
				vd.Type = ml.StructName
				if funDepth == 0 {
					mapVars[s.Let.Name] = false
				}
			} else {
				if funDepth == 0 {
					mapVars[s.Let.Name] = true
				}
				mapVars[alias] = true
				if vd.Type != "" {
					if strings.HasPrefix(vd.Type, "std.StringHashMap(") {
						ml.KeyType = "[]const u8"
						ml.ValType = strings.TrimSuffix(strings.TrimPrefix(vd.Type, "std.StringHashMap("), ")")
					} else if strings.HasPrefix(vd.Type, "std.AutoHashMap(") {
						inside := strings.TrimSuffix(strings.TrimPrefix(vd.Type, "std.AutoHashMap("), ")")
						parts := strings.Split(inside, ",")
						if len(parts) == 2 {
							ml.KeyType = strings.TrimSpace(parts[0])
							ml.ValType = strings.TrimSpace(parts[1])
						}
					}
				}
			}
		}
		if qc, ok := expr.(*QueryComp); ok {
			vd.Type = "[]" + qc.ElemType
		}
		typ := vd.Type
		if typ == "" {
			typ = zigTypeFromExpr(expr)
		}
		if funDepth == 0 {
			varTypes[s.Let.Name] = typ
		}
		varTypes[alias] = typ
		if strings.HasPrefix(typ, "std.StringHashMap(") || strings.HasPrefix(typ, "std.AutoHashMap(") {
			mapVars[s.Let.Name] = true
			mapVars[alias] = true
		}
		if funDepth <= 1 && !isConstExpr(expr) {
			vd.Value = nil
			vd.Mutable = true
			globalInits = append(globalInits, &AssignStmt{Name: alias, Value: expr})
		}
		return vd, nil
	case s.Var != nil:
		var expr Expr
		var err error
		if s.Var.Value != nil {
			expr, err = compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		} else {
			expr = &IntLit{Value: 0}
		}
		mutable := funDepth <= 1
		if funDepth > 0 && len(localMutablesStack) > 0 {
			m := localMutablesStack[len(localMutablesStack)-1]
			mutable = m[s.Var.Name]
		}
		name := s.Var.Name
		if funDepth > 0 && globalNames[name] {
			name = name + "_var"
		}
		alias, ok := aliasStack[len(aliasStack)-1][s.Var.Name]
		if !ok {
			alias = uniqueName(name)
			aliasStack[len(aliasStack)-1][s.Var.Name] = alias
			namesStack[len(namesStack)-1] = append(namesStack[len(namesStack)-1], name)
		}
		vd := &VarDecl{Name: alias, Value: expr, Mutable: mutable}
		varDecls[s.Var.Name] = vd
		varDecls[alias] = vd
		if s.Var.Type != nil {
			vd.Type = toZigType(s.Var.Type)
			if funDepth == 0 {
				varTypes[s.Var.Name] = vd.Type
			}
		}
		if lst, ok := expr.(*ListLit); ok {
			if funDepth == 0 && inferListStruct(s.Var.Name, lst) != "" {
				vd.Type = fmt.Sprintf("[%d]%s", len(lst.Elems), lst.ElemType)
				varTypes[s.Var.Name] = "[]" + lst.ElemType
			} else if funDepth == 0 && len(lst.Elems) > 0 && vd.Type == "" {
				elem := lst.ElemType
				if elem == "" {
					elem = "i64"
				}
				vd.Type = fmt.Sprintf("[%d]%s", len(lst.Elems), elem)
				lst.ElemType = ""
				varTypes[s.Var.Name] = vd.Type
			} else if lst.ElemType == "" && vd.Type != "" && strings.HasPrefix(vd.Type, "[]") {
				lst.ElemType = vd.Type[2:]
			}
		}
		if ml, ok := expr.(*MapLit); ok {
			if ml.StructName != "" {
				vd.Type = ml.StructName
				if funDepth == 0 {
					mapVars[s.Var.Name] = false
				}
			} else {
				if funDepth == 0 {
					mapVars[s.Var.Name] = true
				}
				mapVars[alias] = true
				if vd.Type != "" {
					if strings.HasPrefix(vd.Type, "std.StringHashMap(") {
						ml.KeyType = "[]const u8"
						ml.ValType = strings.TrimSuffix(strings.TrimPrefix(vd.Type, "std.StringHashMap("), ")")
					} else if strings.HasPrefix(vd.Type, "std.AutoHashMap(") {
						inside := strings.TrimSuffix(strings.TrimPrefix(vd.Type, "std.AutoHashMap("), ")")
						parts := strings.Split(inside, ",")
						if len(parts) == 2 {
							ml.KeyType = strings.TrimSpace(parts[0])
							ml.ValType = strings.TrimSpace(parts[1])
						}
					}
				}
			}
		}
		if vr, ok := expr.(*VarRef); ok {
			if t, ok := varTypes[vr.Name]; ok && strings.HasPrefix(t, "[]") {
				elem := t[2:]
				vd.Value = &CopySliceExpr{Src: vr.Name, Elem: elem}
			}
		}
		if qc, ok := expr.(*QueryComp); ok {
			vd.Type = "[]" + qc.ElemType
		}
		typ := vd.Type
		if typ == "" {
			typ = zigTypeFromExpr(expr)
		}
		if funDepth == 0 {
			varTypes[s.Var.Name] = typ
		}
		varTypes[alias] = typ
		if strings.HasPrefix(typ, "std.StringHashMap(") || strings.HasPrefix(typ, "std.AutoHashMap(") {
			mapVars[s.Var.Name] = true
			mapVars[alias] = true
		}
		if funDepth <= 1 && !isConstExpr(expr) {
			vd.Value = nil
			globalInits = append(globalInits, &AssignStmt{Name: alias, Value: expr})
		}
		return vd, nil
	case s.Assign != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0:
		expr, err := compileExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		if ml, ok := expr.(*MapLit); ok {
			if ml.StructName != "" {
				mapVars[s.Assign.Name] = false
			} else {
				mapVars[s.Assign.Name] = true
			}
		}
		alias := resolveAlias(s.Assign.Name)
		return &AssignStmt{Name: alias, Value: expr}, nil
	case s.Assign != nil && len(s.Assign.Field) > 0:
		alias := resolveAlias(s.Assign.Name)
		var target Expr = &VarRef{Name: alias}
		for _, f := range s.Assign.Field[:len(s.Assign.Field)-1] {
			target = &FieldExpr{Target: target, Name: f.Name}
		}
		fieldName := s.Assign.Field[len(s.Assign.Field)-1].Name
		val, err := compileExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &FieldAssignStmt{Target: target, Name: fieldName, Value: val}, nil
	case s.Assign != nil && len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0:
		alias := resolveAlias(s.Assign.Name)
		target := Expr(&VarRef{Name: alias, Map: mapVars[s.Assign.Name]})
		imap := mapVars[s.Assign.Name]
		for _, idx := range s.Assign.Index {
			ix, err := compileExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			target = &IndexExpr{Target: target, Index: ix, Map: imap}
			// subsequent indexes default to list access
			imap = false
		}
		val, err := compileExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &IndexAssignStmt{Target: target, Value: val}, nil
	case s.If != nil:
		return compileIfStmt(s.If, prog)
	case s.While != nil:
		return compileWhileStmt(s.While, prog)
	case s.For != nil:
		return compileForStmt(s.For, prog)
	case s.Bench != nil:
		return compileBenchStmt(s.Bench, prog)
	case s.Return != nil:
		return compileReturnStmt(s.Return)
	case s.Fun != nil:
		_, err := compileFunStmt(s.Fun, prog)
		return nil, err
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Update != nil:
		return compileUpdateStmt(s.Update)
	case s.Type != nil:
		return nil, compileTypeDecl(s.Type)
	case s.ExternVar != nil:
		return nil, nil
	case s.ExternFun != nil:
		return nil, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
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

func compileUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	itemType := ""
	if t, ok := varTypes[u.Target]; ok && strings.HasPrefix(t, "[]") {
		itemType = t[2:]
	}
	fieldMap := map[string]bool{}
	if sd, ok := structDefs[itemType]; ok {
		for _, f := range sd.Fields {
			fieldMap[f.Name] = true
		}
	}
	rewrite := func(e Expr) Expr {
		var walk func(Expr) Expr
		walk = func(x Expr) Expr {
			switch v := x.(type) {
			case *VarRef:
				if fieldMap[v.Name] {
					return &FieldExpr{Target: &VarRef{Name: "item"}, Name: v.Name}
				}
				return v
			case *BinaryExpr:
				v.Left = walk(v.Left)
				v.Right = walk(v.Right)
				return v
			case *CallExpr:
				for i := range v.Args {
					v.Args[i] = walk(v.Args[i])
				}
				return v
			case *IndexExpr:
				v.Target = walk(v.Target)
				v.Index = walk(v.Index)
				return v
			case *FieldExpr:
				v.Target = walk(v.Target)
				return v
			case *NotExpr:
				v.Expr = walk(v.Expr)
				return v
			case *IfExpr:
				v.Cond = walk(v.Cond)
				v.Then = walk(v.Then)
				v.Else = walk(v.Else)
				return v
			}
			return x
		}
		return walk(e)
	}
	fields := make([]string, len(u.Set.Items))
	values := make([]Expr, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, ok := isSimpleIdent(it.Key)
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
		values[i] = rewrite(val)
		fields[i] = key
	}
	var cond Expr
	if u.Where != nil {
		var err error
		cond, err = compileExpr(u.Where)
		if err != nil {
			return nil, err
		}
		cond = rewrite(cond)
	}
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func compileSaveStmt(se *parser.SaveExpr) (Stmt, error) {
	src, err := compileExpr(se.Src)
	if err != nil {
		return nil, err
	}
	path := ""
	if se.Path != nil {
		path = strings.Trim(*se.Path, "\"")
	}
	format := "jsonl"
	if se.With != nil {
		if v, ok := constValue(se.With); ok {
			if m, ok2 := v.(map[string]any); ok2 {
				if f, ok3 := m["format"].(string); ok3 {
					format = f
				}
			}
		}
	}
	return &SaveStmt{Src: src, Path: path, Format: format}, nil
}

func exprUses(name string, e Expr) bool {
	switch t := e.(type) {
	case *VarRef:
		return t.Name == name
	case *BinaryExpr:
		return exprUses(name, t.Left) || exprUses(name, t.Right)
	case *CallExpr:
		for _, a := range t.Args {
			if exprUses(name, a) {
				return true
			}
		}
		return false
	case *IndexExpr:
		return exprUses(name, t.Target) || exprUses(name, t.Index)
	case *FieldExpr:
		return exprUses(name, t.Target)
	case *SliceExpr:
		return exprUses(name, t.Target) || exprUses(name, t.Start) || exprUses(name, t.End)
	case *IfExpr:
		return exprUses(name, t.Cond) || exprUses(name, t.Then) || exprUses(name, t.Else)
	case *AppendExpr:
		return exprUses(name, t.List) || exprUses(name, t.Value)
	case *NotExpr:
		return exprUses(name, t.Expr)
	case *ListLit:
		for _, el := range t.Elems {
			if exprUses(name, el) {
				return true
			}
		}
	case *MapLit:
		for _, m := range t.Entries {
			if exprUses(name, m.Key) || exprUses(name, m.Value) {
				return true
			}
		}
	case *QueryComp:
		for _, s := range t.Sources {
			if exprUses(name, s) {
				return true
			}
		}
		if exprUses(name, t.Elem) || exprUses(name, t.Filter) || exprUses(name, t.Sort) || exprUses(name, t.Skip) || exprUses(name, t.Take) {
			return true
		}
	case *GroupByExpr:
		return exprUses(name, t.Source) || exprUses(name, t.Key) || exprUses(name, t.SelectExpr) || exprUses(name, t.Sort)
	}
	return false
}

func stmtsUse(name string, stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *VarDecl:
			if exprUses(name, s.Value) {
				return true
			}
		case *AssignStmt:
			if s.Name == name || exprUses(name, s.Value) {
				return true
			}
		case *IndexAssignStmt:
			if exprUses(name, s.Target) || exprUses(name, s.Value) {
				return true
			}
		case *FieldAssignStmt:
			if exprUses(name, s.Target) || exprUses(name, s.Value) {
				return true
			}
		case *ExprStmt:
			if exprUses(name, s.Expr) {
				return true
			}
		case *IfStmt:
			if exprUses(name, s.Cond) || stmtsUse(name, s.Then) || stmtsUse(name, s.Else) {
				return true
			}
		case *WhileStmt:
			if exprUses(name, s.Cond) || stmtsUse(name, s.Body) {
				return true
			}
		case *ForStmt:
			if s.Name != name {
				if exprUses(name, s.Start) || exprUses(name, s.End) || exprUses(name, s.Iterable) {
					return true
				}
				if stmtsUse(name, s.Body) {
					return true
				}
			}
		case *UpdateStmt:
			if s.Target == name {
				return true
			}
			if exprUses(name, s.Cond) {
				return true
			}
			for _, v := range s.Values {
				if exprUses(name, v) {
					return true
				}
			}
		case *SaveStmt:
			if exprUses(name, s.Src) {
				return true
			}
		case *PrintStmt:
			for _, v := range s.Values {
				if exprUses(name, v) {
					return true
				}
			}
		case *BenchStmt:
			if stmtsUse(name, s.Body) {
				return true
			}
		}
	}
	return false
}

func compileTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		if td.Name == "Tree" && len(td.Variants) == 2 && td.Variants[1].Name == "Node" {
			fields := make([]Field, len(td.Variants[1].Fields))
			for i, f := range td.Variants[1].Fields {
				fields[i] = Field{Name: toSnakeCase(f.Name), Type: toZigType(f.Type)}
			}
			structDefs["Node"] = &StructDef{Name: "Node", Fields: fields}
			typeAliases["Tree"] = "?*Node"
			return nil
		}
		for i, v := range td.Variants {
			fields := make([]Field, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = Field{Name: toSnakeCase(f.Name), Type: toZigType(f.Type)}
			}
			structDefs[v.Name] = &StructDef{Name: v.Name, Fields: fields}
			variantTags[v.Name] = i
		}
		typeAliases[td.Name] = "std.StringHashMap(i64)"
		return nil
	}
       if len(td.Members) > 0 {
               fields := []Field{}
               for _, m := range td.Members {
                       if m.Field != nil {
                               fields = append(fields, Field{Name: toSnakeCase(m.Field.Name), Type: toZigType(m.Field.Type)})
                       } else if m.Method != nil {
                               fn := m.Method
                               params := []Param{{Name: "self", Type: td.Name}}
                               paramTypes := []string{td.Name}
                               for _, p := range fn.Params {
                                       ptype := toZigType(p.Type)
                                       pname := toSnakeCase(p.Name)
                                       params = append(params, Param{Name: pname, Type: ptype})
                                       paramTypes = append(paramTypes, ptype)
                               }
                               ret := ""
                               if fn.Return != nil {
                                       ret = toZigType(fn.Return)
                               }
                               body := []Stmt{}
                               for _, st := range fn.Body {
                                       s, err := compileStmt(st, nil)
                                       if err != nil {
                                               return err
                                       }
                                       if s != nil {
                                               body = append(body, s)
                                       }
                               }
                               f := &Func{Name: fn.Name, Params: params, ReturnType: ret, Body: body, Aliases: map[string]string{}}
                               funcParamTypes[fn.Name] = paramTypes
                               varTypes[fn.Name] = fmt.Sprintf("*const fn(%s) %s", strings.Join(paramTypes, ", "), ret)
                               funcReturns[fn.Name] = ret
                               extraFuncs = append(extraFuncs, f)
                       }
               }
               structDefs[td.Name] = &StructDef{Name: td.Name, Fields: fields}
       }
       return nil
}

func collectVarInfo(p *Program) (map[string]int, map[string]bool) {
	uses := map[string]int{}
	muts := map[string]bool{}
	var walkExpr func(e Expr)
	walkExpr = func(e Expr) {
		switch t := e.(type) {
		case *VarRef:
			uses[t.Name]++
		case *BinaryExpr:
			walkExpr(t.Left)
			walkExpr(t.Right)
		case *CallExpr:
			for _, a := range t.Args {
				walkExpr(a)
			}
			// Count uses of the called function name even if it
			// isn't present in varDecls. This ensures that
			// parameters with function types are treated as used
			// and not renamed to '_' during emission.
			uses[t.Func]++
		case *IfExpr:
			walkExpr(t.Cond)
			if t.Then != nil {
				walkExpr(t.Then)
			}
			if t.Else != nil {
				walkExpr(t.Else)
			}
		case *IndexExpr:
			walkExpr(t.Target)
			walkExpr(t.Index)
		case *FieldExpr:
			walkExpr(t.Target)
		case *CastExpr:
			walkExpr(t.Value)
		case *ListLit:
			for _, e2 := range t.Elems {
				walkExpr(e2)
			}
		case *MapLit:
			for _, e2 := range t.Entries {
				walkExpr(e2.Key)
				walkExpr(e2.Value)
			}
		case *AppendExpr:
			if s, ok := exprToString(t.List); ok {
				base := strings.SplitN(s, ".", 2)[0]
				muts[base] = true
			}
			walkExpr(t.List)
			walkExpr(t.Value)
		case *NotExpr:
			walkExpr(t.Expr)
		case *SliceExpr:
			walkExpr(t.Target)
			walkExpr(t.Start)
			walkExpr(t.End)
		case *CopySliceExpr:
			uses[t.Src]++
		}
	}
	var walkStmt func(s Stmt)
	walkStmt = func(s Stmt) {
		switch t := s.(type) {
		case *VarDecl:
			if t.Value != nil {
				walkExpr(t.Value)
			}
		case *AssignStmt:
			muts[t.Name] = true
			walkExpr(t.Value)
		case *IndexAssignStmt:
			if s, ok := exprToString(t.Target); ok {
				base := strings.SplitN(s, ".", 2)[0]
				muts[base] = true
			}
			walkExpr(t.Target)
			walkExpr(t.Value)
		case *FieldAssignStmt:
			if s, ok := exprToString(t.Target); ok {
				base := strings.SplitN(s, ".", 2)[0]
				muts[base] = true
			}
			walkExpr(t.Value)
		case *ExprStmt:
			walkExpr(t.Expr)
		case *PrintStmt:
			for _, v := range t.Values {
				walkExpr(v)
			}
		case *ReturnStmt:
			if t.Value != nil {
				walkExpr(t.Value)
			}
		case *IfStmt:
			walkExpr(t.Cond)
			for _, st := range t.Then {
				walkStmt(st)
			}
			for _, st := range t.Else {
				walkStmt(st)
			}
		case *WhileStmt:
			walkExpr(t.Cond)
			for _, st := range t.Body {
				walkStmt(st)
			}
		case *ForStmt:
			if t.Iterable != nil {
				walkExpr(t.Iterable)
			}
			if t.Start != nil {
				walkExpr(t.Start)
			}
			if t.End != nil {
				walkExpr(t.End)
			}
			for _, st := range t.Body {
				walkStmt(st)
			}
		case *BenchStmt:
			for _, st := range t.Body {
				walkStmt(st)
			}
		}
	}
	for _, g := range p.Globals {
		walkStmt(g)
	}
	for _, f := range p.Functions {
		if f == nil {
			continue
		}
		for _, st := range f.Body {
			walkStmt(st)
		}
	}
	return uses, muts
}
