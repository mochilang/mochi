//go:build slow

package zigt

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"
	"time"
	"unicode"

	"mochi/parser"
	"mochi/types"
)

var constLists map[string]*ListLit
var mapVars map[string]bool
var structDefs map[string]*StructDef
var extraFuncs []*Func
var funcCounter int
var varTypes map[string]string

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
}

type Stmt interface{ emit(io.Writer, int) }

type Expr interface{ emit(io.Writer) }

// BoolLit represents a boolean literal.
type BoolLit struct{ Value bool }

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

// PrintStmt writes values using std.io.getStdOut().writer().print.
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
}

type FieldExpr struct {
	Target Expr
	Name   string
}

func (fe *FieldExpr) emit(w io.Writer) {
	fe.Target.emit(w)
	fmt.Fprintf(w, ".%s", toSnakeCase(fe.Name))
}

type QueryComp struct {
	Vars     []string
	Sources  []Expr
	Elem     Expr
	ElemType string
	Filter   Expr
}

func (qc *QueryComp) emit(w io.Writer) {
	fmt.Fprintf(w, "blk: {\n    var arr = std.ArrayList(%s).init(std.heap.page_allocator);\n", qc.ElemType)
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
	qc.Elem.emit(w)
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
	io.WriteString(w, "const tmp = arr.toOwnedSlice() catch unreachable;\n")
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
	case *FloatLit:
		return "f64"
	case *VarRef:
		if t, ok := varTypes[e.(*VarRef).Name]; ok && t != "" {
			return t
		}
		return "i64"
	case *MapLit:
		if m := e.(*MapLit); m.StructName != "" {
			return m.StructName
		}
		return "std.StringHashMap(i64)"
	case *ListLit:
		if l := e.(*ListLit); l.ElemType != "" {
			return "[]" + l.ElemType
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
		switch e.(*BinaryExpr).Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return "bool"
		default:
			return "i64"
		}
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
				fmt.Fprintf(w, ".%s = ", toSnakeCase(k.Value))
			}
			e.Value.emit(w)
		}
		io.WriteString(w, " }")
		return
	}
	keyType := "[]const u8"
	valType := "i64"
	if len(m.Entries) > 0 {
		keyType = zigTypeFromExpr(m.Entries[0].Key)
		valType = zigTypeFromExpr(m.Entries[0].Value)
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
		e.Value.emit(w)
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

func header() string {
	ts := time.Now().Format("2006-01-02 15:04 -0700")
	return fmt.Sprintf("// Generated by Mochi Zig transpiler on %s\n", ts)
}

// Emit returns the Zig source code for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	buf.WriteString("const std = @import(\"std\");\n\n")
	for _, st := range p.Structs {
		st.emit(&buf)
		buf.WriteString("\n")
	}
	for i, fn := range p.Functions {
		if i > 0 {
			buf.WriteString("\n")
		}
		fn.emit(&buf)
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
		fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
	}
	ret := f.ReturnType
	if ret == "" {
		ret = "void"
	}
	fmt.Fprintf(w, ") %s {\n", ret)
	for _, st := range f.Body {
		st.emit(w, 1)
	}
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
		io.WriteString(w, "try std.io.getStdOut().writer().print(\"\\n\", .{});\n")
		return
	}
	fmtSpec := make([]string, len(s.Values))
	for i, v := range s.Values {
		switch t := v.(type) {
		case *IndexExpr:
			if t.Map {
				fmtSpec[i] = "{s}"
			} else {
				fmtSpec[i] = "{c}"
			}
		case *StringLit:
			fmtSpec[i] = "{s}"
		default:
			fmtSpec[i] = "{any}"
		}
	}
	io.WriteString(w, "try std.io.getStdOut().writer().print(\"")
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
	kw := "const"
	if v.Mutable {
		kw = "var"
	}
	fmt.Fprintf(w, "%s %s", kw, v.Name)
	if v.Type != "" {
		fmt.Fprintf(w, ": %s", v.Type)
	} else if v.Mutable {
		if _, ok := v.Value.(*IntLit); ok {
			io.WriteString(w, ": i64")
		}
	}
	io.WriteString(w, " = ")
	if v.Value == nil {
		io.WriteString(w, "0")
	} else {
		v.Value.emit(w)
	}
	io.WriteString(w, ";\n")
}

func (a *AssignStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	fmt.Fprintf(w, "%s = ", a.Name)
	a.Value.emit(w)
	io.WriteString(w, ";\n")
}

func (a *IndexAssignStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	if idx, ok := a.Target.(*IndexExpr); ok && idx.Map {
		emitMapPut(w, idx, a.Value)
		io.WriteString(w, "\n")
	} else {
		a.Target.emit(w)
		io.WriteString(w, " = ")
		a.Value.emit(w)
		io.WriteString(w, ";\n")
	}
}

func (e *ExprStmt) emit(w io.Writer, indent int) {
	writeIndent(w, indent)
	if e.Expr != nil {
		e.Expr.emit(w)
	}
	io.WriteString(w, "\n")
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

func (f *FloatLit) emit(w io.Writer) {
	io.WriteString(w, strconv.FormatFloat(f.Value, 'f', 1, 64))
}

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		if l, ok := b.Left.(*StringLit); ok {
			if r, ok2 := b.Right.(*StringLit); ok2 {
				fmt.Fprintf(w, "%q", l.Value+r.Value)
				return
			}
		}
	}
	if _, ok := b.Left.(*StringLit); ok {
		if _, ok2 := b.Right.(*StringLit); ok2 {
			switch b.Op {
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
	}
	if b.Op == "in" {
		if _, ok := b.Right.(*StringLit); ok {
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
		} else if vr, ok := b.Right.(*VarRef); ok && mapVars[vr.Name] {
			vr.emit(w)
			io.WriteString(w, ".contains(")
			b.Left.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "std.mem.indexOfScalar(i64, ")
			b.Right.emit(w)
			io.WriteString(w, ", ")
			b.Left.emit(w)
			io.WriteString(w, ") != null")
		}
		return
	}
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
}

func (l *ListLit) emit(w io.Writer) {
	if l.ElemType != "" {
		fmt.Fprintf(w, "[_]%s{", l.ElemType)
	} else if len(l.Elems) > 0 {
		if _, ok := l.Elems[0].(*ListLit); ok {
			if sub, ok := l.Elems[0].(*ListLit); ok {
				fmt.Fprintf(w, "[_][%d]i64{", len(sub.Elems))
			}
		} else {
			io.WriteString(w, "[_]i64{")
		}
	} else {
		io.WriteString(w, "[_]i64{")
	}
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "}")
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.Map {
		i.Target.emit(w)
		io.WriteString(w, ".get(")
		i.Index.emit(w)
		io.WriteString(w, ").?")
	} else {
		i.Target.emit(w)
		io.WriteString(w, "[")
		i.Index.emit(w)
		io.WriteString(w, "]")
	}
}

func emitMapPut(w io.Writer, idx *IndexExpr, val Expr) {
	if inner, ok := idx.Target.(*IndexExpr); ok && inner.Map {
		emitMapGet(w, inner)
		io.WriteString(w, ".put(")
		idx.Index.emit(w)
		io.WriteString(w, ", ")
		val.emit(w)
		io.WriteString(w, ") catch unreachable;")
		return
	}
	idx.Target.emit(w)
	io.WriteString(w, ".put(")
	idx.Index.emit(w)
	io.WriteString(w, ", ")
	val.emit(w)
	io.WriteString(w, ") catch unreachable;")
}

func emitMapGet(w io.Writer, idx *IndexExpr) {
	if inner, ok := idx.Target.(*IndexExpr); ok && inner.Map {
		emitMapGet(w, inner)
		io.WriteString(w, ".get(")
		idx.Index.emit(w)
		io.WriteString(w, ").?")
		return
	}
	idx.Target.emit(w)
	io.WriteString(w, ".get(")
	idx.Index.emit(w)
	io.WriteString(w, ").?")
}

func (sli *SliceExpr) emit(w io.Writer) {
	sli.Target.emit(w)
	io.WriteString(w, "[")
	if sli.Start != nil {
		sli.Start.emit(w)
	}
	io.WriteString(w, "..")
	if sli.End != nil {
		sli.End.emit(w)
	}
	io.WriteString(w, "]")
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "std.fmt.parseInt(i64, ")
		c.Value.emit(w)
		io.WriteString(w, ", 10) catch 0")
	default:
		c.Value.emit(w)
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

func (i *IfStmt) emit(w io.Writer, indent int) {
	for j := 0; j < indent; j++ {
		io.WriteString(w, "    ")
	}
	io.WriteString(w, "if (")
	i.Cond.emit(w)
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
	if f.Iterable != nil {
		io.WriteString(w, "for (")
		f.Iterable.emit(w)
		io.WriteString(w, ") |")
		io.WriteString(w, f.Name)
		io.WriteString(w, "| {\n")
	} else {
		io.WriteString(w, "for (")
		f.Start.emit(w)
		io.WriteString(w, "..")
		f.End.emit(w)
		io.WriteString(w, ") |")
		io.WriteString(w, f.Name)
		io.WriteString(w, "| {\n")
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
		r.Value.emit(w)
	}
	io.WriteString(w, ";\n")
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if (")
	i.Cond.emit(w)
	io.WriteString(w, ") ")
	i.Then.emit(w)
	io.WriteString(w, " else ")
	i.Else.emit(w)
}

func (c *CallExpr) emit(w io.Writer) {
	switch c.Func {
	case "len", "count":
		if len(c.Args) > 0 {
			if s, ok := c.Args[0].(*StringLit); ok {
				fmt.Fprintf(w, "%q.len", s.Value)
			} else if l, ok := c.Args[0].(*ListLit); ok {
				fmt.Fprintf(w, "%d", len(l.Elems))
			} else if v, ok := c.Args[0].(*VarRef); ok && mapVars[v.Name] {
				v.emit(w)
				io.WriteString(w, ".count()")
			} else {
				io.WriteString(w, "std.mem.len(")
				c.Args[0].emit(w)
				io.WriteString(w, ")")
			}
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
			io.WriteString(w, "std.mem.indexOf(u8, ")
			c.Args[0].emit(w)
			io.WriteString(w, ", ")
			c.Args[1].emit(w)
			io.WriteString(w, ") != null")
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
				io.WriteString(w, ".iterator(); var arr = std.ArrayList(i64).init(std.heap.page_allocator); while (it.next()) |kv| { arr.append(kv.value) catch unreachable; } break :blk arr.toOwnedSlice(); }")
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
			a.emit(w)
		}
		io.WriteString(w, ")")
	}
}

// Transpile converts a Mochi program into our simple Zig AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	main := &Func{Name: "main"}
	funcs := []*Func{}
	extraFuncs = nil
	funcCounter = 0
	varTypes = map[string]string{}
	mutables := map[string]bool{}
	collectMutables(prog.Statements, mutables)
	constLists = map[string]*ListLit{}
	mapVars = map[string]bool{}
	structDefs = map[string]*StructDef{}
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
			vd.Mutable = mutables[vd.Name]
			if !vd.Mutable {
				if lst, ok2 := vd.Value.(*ListLit); ok2 {
					constLists[vd.Name] = lst
				}
			}
		}
		main.Body = append(main.Body, s)
	}
	_ = env
	funcs = append(funcs, extraFuncs...)
	funcs = append(funcs, main)
	structs := make([]*StructDef, 0, len(structDefs))
	for _, sd := range structDefs {
		structs = append(structs, sd)
	}
	return &Program{Structs: structs, Functions: funcs}, nil
}

func collectMutables(sts []*parser.Statement, m map[string]bool) {
	for _, st := range sts {
		switch {
		case st.Assign != nil:
			m[st.Assign.Name] = true
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
		if llist, ok := expr.(*ListLit); ok {
			if rlist, ok2 := r.(*ListLit); ok2 {
				switch op.Op {
				case "union":
					seen := map[int]bool{}
					res := []Expr{}
					for _, v := range llist.Elems {
						iv := v.(*IntLit)
						if !seen[iv.Value] {
							seen[iv.Value] = true
							res = append(res, iv)
						}
					}
					for _, v := range rlist.Elems {
						iv := v.(*IntLit)
						if !seen[iv.Value] || op.All {
							if !seen[iv.Value] {
								seen[iv.Value] = true
							}
							res = append(res, iv)
						}
					}
					expr = &ListLit{Elems: res}
					continue
				case "except":
					m := map[int]bool{}
					for _, v := range rlist.Elems {
						m[v.(*IntLit).Value] = true
					}
					res := []Expr{}
					for _, v := range llist.Elems {
						iv := v.(*IntLit)
						if !m[iv.Value] {
							res = append(res, iv)
						}
					}
					expr = &ListLit{Elems: res}
					continue
				case "intersect":
					m := map[int]bool{}
					for _, v := range rlist.Elems {
						m[v.(*IntLit).Value] = true
					}
					res := []Expr{}
					for _, v := range llist.Elems {
						iv := v.(*IntLit)
						if m[iv.Value] {
							res = append(res, iv)
						}
					}
					expr = &ListLit{Elems: res}
					continue
				}
			}
		}
		expr = &BinaryExpr{Left: expr, Op: op.Op, Right: r}
	}
	return expr, nil
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
					imap = true
				case *VarRef:
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
		if op.Field != nil {
			expr = &FieldExpr{Target: expr, Name: op.Field.Name}
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
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
		params := make([]Param, len(p.FunExpr.Params))
		for i, par := range p.FunExpr.Params {
			params[i] = Param{Name: par.Name, Type: toZigType(par.Type)}
		}
		ret := toZigType(p.FunExpr.Return)
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
		extraFuncs = append(extraFuncs, &Func{Name: name, Params: params, ReturnType: ret, Body: body})
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
		case "str":
			if len(args) == 1 {
				if lit, ok := args[0].(*IntLit); ok {
					return &StringLit{Value: fmt.Sprintf("%d", lit.Value)}, nil
				}
			}
		case "substring":
			if len(args) == 3 {
				if s, ok := args[0].(*StringLit); ok {
					if start, ok1 := args[1].(*IntLit); ok1 {
						if end, ok2 := args[2].(*IntLit); ok2 {
							if start.Value >= 0 && end.Value <= len(s.Value) && start.Value <= end.Value {
								return &StringLit{Value: s.Value[start.Value:end.Value]}, nil
							}
						}
					}
				}
			}
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
					if lst, ok2 := constLists[v.Name]; ok2 {
						elems := make([]Expr, 0, len(lst.Elems)+1)
						for i := range lst.Elems {
							elems = append(elems, &IndexExpr{Target: &VarRef{Name: v.Name}, Index: &IntLit{Value: i}})
						}
						elems = append(elems, args[1])
						return &ListLit{Elems: elems}, nil
					}
				}
			}
		}
		return &CallExpr{Func: p.Call.Func, Args: args}, nil
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Bool != nil {
			return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
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
			for _, e := range elems[1:] {
				if zigTypeFromExpr(e) != elemType {
					elemType = ""
					break
				}
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
			// treat identifiers as string keys for maps
			if vr, ok := k.(*VarRef); ok {
				k = &StringLit{Value: vr.Name}
			}
			v, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: k, Value: v}
		}
		return &MapLit{Entries: entries}, nil
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.Match != nil:
		return compileMatchExpr(p.Match)
	case p.Selector != nil:
		var expr Expr = &VarRef{Name: p.Selector.Root}
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

func compileIfStmt(is *parser.IfStmt, prog *parser.Program) (Stmt, error) {
	cond, err := compileExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, s := range is.Then {
		st, err := compileStmt(s, prog)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, st)
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		st, err := compileIfStmt(is.ElseIf, prog)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if len(is.Else) > 0 {
		for _, s := range is.Else {
			st, err := compileStmt(s, prog)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, st)
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func compileWhileStmt(ws *parser.WhileStmt, prog *parser.Program) (Stmt, error) {
	cond, err := compileExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	body := make([]Stmt, 0, len(ws.Body))
	for _, s := range ws.Body {
		st, err := compileStmt(s, prog)
		if err != nil {
			return nil, err
		}
		body = append(body, st)
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func compileForStmt(fs *parser.ForStmt, prog *parser.Program) (Stmt, error) {
	var start, end, iter Expr
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
	} else {
		iter, err = compileExpr(fs.Source)
		if err != nil {
			return nil, err
		}
	}
	body := make([]Stmt, 0, len(fs.Body))
	for _, s := range fs.Body {
		st, err := compileStmt(s, prog)
		if err != nil {
			return nil, err
		}
		body = append(body, st)
	}
	return &ForStmt{Name: fs.Name, Start: start, End: end, Iterable: iter, Body: body}, nil
}

func mapFields(m *MapLit) ([]Field, bool) {
	fields := make([]Field, len(m.Entries))
	for i, e := range m.Entries {
		key, ok := e.Key.(*StringLit)
		if !ok {
			return nil, false
		}
		fields[i] = Field{Name: toSnakeCase(key.Value), Type: zigTypeFromExpr(e.Value)}
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

func compileQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, fmt.Errorf("unsupported query features")
	}
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
	elem, err := compileExpr(q.Select)
	if err != nil {
		return nil, err
	}
	elemType := zigTypeFromExpr(elem)
	if ml, ok := elem.(*MapLit); ok {
		structName := "Entry"
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
	return &QueryComp{Vars: vars, Sources: sources, Elem: elem, ElemType: elemType, Filter: filter}, nil
}

func toZigType(t *parser.TypeRef) string {
	if t == nil {
		return "i64"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i64"
		case "bool":
			return "bool"
		case "string":
			return "[]const u8"
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		elem := toZigType(t.Generic.Args[0])
		return "[]" + elem
	}
	return "i64"
}

func compileFunStmt(fn *parser.FunStmt, prog *parser.Program) (*Func, error) {
	params := make([]Param, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = Param{Name: p.Name, Type: toZigType(p.Type)}
	}
	ret := toZigType(fn.Return)
	body := make([]Stmt, 0, len(fn.Body))
	for _, st := range fn.Body {
		s, err := compileStmt(st, prog)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &Func{Name: fn.Name, Params: params, ReturnType: ret, Body: body}, nil
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
		call := s.Expr.Expr.Binary.Left.Value.Target.Call
		if call == nil || call.Func != "print" || len(call.Args) == 0 {
			return nil, fmt.Errorf("unsupported expression")
		}
		args := make([]Expr, len(call.Args))
		for i, a := range call.Args {
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		return &PrintStmt{Values: args}, nil
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
		vd := &VarDecl{Name: s.Let.Name, Value: expr}
		if lst, ok := expr.(*ListLit); ok {
			if inferListStruct(s.Let.Name, lst) != "" {
				vd.Type = "[_]" + lst.ElemType
			}
		}
		if ml, ok := expr.(*MapLit); ok {
			mapVars[s.Let.Name] = true
			if ml.StructName != "" {
				vd.Type = ml.StructName
			}
		}
		if qc, ok := expr.(*QueryComp); ok {
			vd.Type = "[]" + qc.ElemType
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
		vd := &VarDecl{Name: s.Var.Name, Value: expr, Mutable: true}
		if lst, ok := expr.(*ListLit); ok {
			if inferListStruct(s.Var.Name, lst) != "" {
				vd.Type = "[_]" + lst.ElemType
			}
		}
		if ml, ok := expr.(*MapLit); ok {
			mapVars[s.Var.Name] = true
			if ml.StructName != "" {
				vd.Type = ml.StructName
			}
		}
		if qc, ok := expr.(*QueryComp); ok {
			vd.Type = "[]" + qc.ElemType
		}
		return vd, nil
	case s.Assign != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0:
		expr, err := compileExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		if _, ok := expr.(*MapLit); ok {
			mapVars[s.Assign.Name] = true
		}
		return &AssignStmt{Name: s.Assign.Name, Value: expr}, nil
	case s.Assign != nil && len(s.Assign.Index) > 0 && len(s.Assign.Field) == 0:
		target := Expr(&VarRef{Name: s.Assign.Name, Map: mapVars[s.Assign.Name]})
		for _, idx := range s.Assign.Index {
			ix, err := compileExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			target = &IndexExpr{Target: target, Index: ix, Map: true}
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
	case s.Return != nil:
		return compileReturnStmt(s.Return)
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}
