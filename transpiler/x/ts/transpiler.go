//go:build slow

package tstranspiler

import (
	"bytes"
	"fmt"
	"io"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"unicode"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"

	meta "mochi/transpiler/meta"
)

// Simple TypeScript AST nodes used by the transpiler.

type Program struct {
	Stmts []Stmt
}

// TranspileEnv is used for type inference during conversion.
var transpileEnv *types.Env
var generatedTypes map[string]bool
var prelude []Stmt
var pythonMathAliases map[string]bool
var useNow bool
var useInput bool
var useKeys bool
var useNumDenom bool
var useSHA256 bool
var useRepeat bool
var useConcat bool
var useParseIntStr bool
var useBench bool
var useStdout bool
var useHas bool
var useFetch bool
var useLookupHost bool
var useStr bool
var useLen bool
var usePanic bool
var usePyName bool
var funcDepth int
var returnOutsideFunc bool
var paramStack []map[string]bool
var asyncFuncs map[string]bool

const maxSafeMul = 94906265 // sqrt(2^53 - 1)

var reserved = map[string]bool{
	"break": true, "case": true, "catch": true, "class": true, "const": true,
	"continue": true, "debugger": true, "default": true, "delete": true,
	"do": true, "else": true, "enum": true, "export": true, "extends": true,
	"false": true, "finally": true, "for": true, "function": true, "if": true,
	"import": true, "in": true, "instanceof": true, "new": true, "null": true,
	"return": true, "super": true, "switch": true, "this": true, "throw": true,
	"true": true, "try": true, "typeof": true, "var": true, "void": true,
	"while": true, "with": true, "as": true, "implements": true,
	"interface": true, "let": true, "package": true, "private": true,
	"protected": true, "public": true, "static": true, "yield": true,
	"any": true, "boolean": true, "constructor": true, "declare": true,
	"get": true, "module": true, "require": true, "number": true, "set": true,
	"string": true, "symbol": true, "type": true, "from": true, "of": true,
}

func safeName(name string) string {
	name = strings.TrimSpace(name)
	if reserved[name] {
		return "_" + name
	}
	return name
}

type Stmt interface {
	emit(io.Writer)
}

// BreakStmt represents a break statement inside loops.
type BreakStmt struct{}

// ContinueStmt represents a continue statement inside loops.
type ContinueStmt struct{}

// ReturnStmt represents returning from a function.
type ReturnStmt struct {
	Value Expr
}

// AwaitExpr represents awaiting a promise.
type AwaitExpr struct {
	X Expr
}

func (a *AwaitExpr) emit(w io.Writer) {
	io.WriteString(w, "await ")
	a.X.emit(w)
}

// FuncDecl represents a function definition.
type FuncDecl struct {
	Name       string
	Params     []string
	ParamTypes []string
	ReturnType string
	Body       []Stmt
	Async      bool
}

// TypeAlias represents `type Name = { ... }` declarations.
type TypeAlias struct {
	Name string
	Type string
}

// InterfaceDecl represents `interface Name { ... }` declarations.
type InterfaceDecl struct {
	Name   string
	Fields []string
}

func (i *InterfaceDecl) emit(w io.Writer) {
	io.WriteString(w, "export interface ")
	io.WriteString(w, i.Name)
	io.WriteString(w, " { ")
	io.WriteString(w, strings.Join(i.Fields, "; "))
	io.WriteString(w, " }")
}

func (t *TypeAlias) emit(w io.Writer) {
	io.WriteString(w, "export type ")
	io.WriteString(w, t.Name)
	io.WriteString(w, " = ")
	io.WriteString(w, t.Type)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

// IfStmt represents a conditional statement with an optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents a numeric range for-loop like
// `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForInStmt represents iteration over an iterable expression.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	Keys     bool // if true, iterate over keys using `in` instead of values
}

// BenchStmt represents a benchmark block that measures execution time
// of its body and prints a JSON summary similar to the VM output.
type BenchStmt struct {
	Name  string
	Body  []Stmt
	Async bool
}

// IIFEStmt wraps a series of statements in an immediately invoked
// function expression. It is used when top-level statements need to
// contain return statements which are not valid in the global scope of
// TypeScript.
type IIFEStmt struct {
	Body  []Stmt
	Async bool
}

type Expr interface {
	emit(io.Writer)
}

type ExprStmt struct {
	Expr Expr
}

// VarDecl represents a variable declaration like `let x = expr`.
// VarDecl represents a variable declaration like `let x = expr`. If Const is
// true the variable is emitted as `const`, otherwise `let`.
type VarDecl struct {
	Name  string
	Expr  Expr
	Type  string
	Const bool
}

// AssignStmt represents an assignment like `x = expr`.
type AssignStmt struct {
	Name string
	Expr Expr
}

type CallExpr struct {
	Func string
	Args []Expr
}

type StringLit struct {
	Value string
}

// NumberLit is a numeric literal.
type NumberLit struct {
	Value string
}

// BoolLit is a boolean literal.
type BoolLit struct {
	Value bool
}

// formatFloat keeps a trailing `.0` for whole numbers so the generated code
// more closely matches Mochi's print output for floats.
func formatFloat(f float64) string {
	if math.Trunc(f) == f {
		return fmt.Sprintf("%.1f", f)
	}
	return fmt.Sprintf("%g", f)
}

// NullLit is the `null` literal.
type NullLit struct{}

// NameRef refers to a variable.
type NameRef struct {
	Name string
}

// BinaryExpr represents a binary operation such as 1 + 2.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// UnaryExpr represents a prefix unary operation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

// ListLit represents a list/array literal.
type ListLit struct {
	Elems []Expr
}

// LenExpr represents the builtin len() call.
type LenExpr struct {
	Value Expr
}

// AppendExpr represents appending an element to a list.
type AppendExpr struct {
	List Expr
	Elem Expr
}

// SpreadExpr represents spreading an iterable into function arguments.
type SpreadExpr struct{ Expr Expr }

// AvgExpr represents averaging a list of numbers.
type AvgExpr struct{ Value Expr }

// SumExpr represents summing a list of numbers.
type SumExpr struct{ Value Expr }

// MinExpr returns the minimum value of a list.
type MinExpr struct{ Value Expr }

// MaxExpr returns the maximum value of a list.
type MaxExpr struct{ Value Expr }

// IntDivExpr performs integer division of two expressions.
type IntDivExpr struct {
	Left, Right Expr
	Big         bool
}

// NowExpr expands to a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

func (n *NowExpr) emit(w io.Writer) { io.WriteString(w, "_now()") }

// ValuesExpr returns Object.values(o).
type ValuesExpr struct{ Value Expr }

// SubstringExpr represents substring(s, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

type UnionExpr struct{ Left, Right Expr }
type UnionAllExpr struct{ Left, Right Expr }
type ExceptExpr struct{ Left, Right Expr }
type IntersectExpr struct{ Left, Right Expr }

type FormatListExpr struct {
	Value       Expr
	FloatFields []string
	Compact     bool
}

// BoolStringExpr renders a boolean value as "True" or "False" for output.
type BoolStringExpr struct{ Value Expr }

// PrintExpr represents a call to the builtin print function. The arguments are
// joined with a space and trailing whitespace is trimmed to avoid mismatches
// with the VM output.
type PrintExpr struct{ Args []Expr }

// MapLit represents a map/object literal.
type MapLit struct {
	Entries []MapEntry
}

// MapEntry is a key/value pair inside a MapLit.
type MapEntry struct {
	Key   Expr
	Value Expr
}

// IndexExpr represents `target[index]` access.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

// SliceExpr represents a[start:end] slicing.
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

// MethodCallExpr represents target.method(args...).
type MethodCallExpr struct {
	Target Expr
	Method string
	Args   []Expr
}

// MapKeysExpr represents extraction of map keys as a number array.
type MapKeysExpr struct {
	Target Expr
}

// FunExpr represents an anonymous function expression.
type FunExpr struct {
	Params []string
	Body   []Stmt
	Expr   Expr
}

// InvokeExpr represents calling a function expression.
type InvokeExpr struct {
	Callee Expr
	Args   []Expr
}

// IfExpr represents a ternary conditional expression `cond ? a : b`.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

// QueryLoop represents one iteration variable in a query expression.
type QueryLoop struct {
	Name   string
	Source Expr
}

// JoinSpec represents a join clause inside a query.
type JoinSpec struct {
	Name   string
	Source Expr
	Side   string // "", "left", "right", "outer"
	On     Expr
}

// QueryExprJS represents a simplified query comprehension.
type QueryExprJS struct {
	Loops    []QueryLoop
	Joins    []JoinSpec
	Where    Expr
	Sort     Expr
	Skip     Expr
	Take     Expr
	Select   Expr
	ElemType string
}

// GroupQueryExpr represents a simple query with grouping support.
type GroupQueryExpr struct {
	Var      string
	Source   Expr
	Key      Expr
	Row      Expr
	GroupVar string
	Cond     Expr
	Select   Expr
	Having   Expr
	Sort     Expr
	ElemType string
}

// GroupJoinQueryExpr handles queries with joins and grouping.
type GroupJoinQueryExpr struct {
	Loops    []QueryLoop
	Joins    []JoinSpec
	Where    Expr
	Key      Expr
	Row      Expr
	GroupVar string
	Select   Expr
	Having   Expr
	Sort     Expr
	ElemType string
}

// AggQueryExpr represents simple aggregation queries like
// `from x in xs where ... select sum(x)`.
type AggQueryExpr struct {
	Var    string
	Source Expr
	Where  Expr
	Op     string
}

// IndexAssignStmt assigns to an indexed expression like x[i] = v.
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

// SaveStmt saves a list of maps to a file or stdout in a simple JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

// UpdateStmt updates fields of items in a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

// StructUpdateExpr creates a copy of a struct with one field changed.
type StructUpdateExpr struct {
	Target Expr
	Field  string
	Value  Expr
}

// RawExpr emits raw TypeScript code verbatim.
type RawExpr struct{ Code string }

// RawStmt emits raw TypeScript statement(s) verbatim.
type RawStmt struct{ Code string }

func (s *ExprStmt) emit(w io.Writer) {
	if s == nil {
		return
	}
	s.Expr.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, safeName(c.Func))
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte('(')
	} else {
		io.WriteString(w, "(")
	}
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(')')
	} else {
		io.WriteString(w, ")")
	}
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (n *NullLit) emit(w io.Writer) { io.WriteString(w, "null") }

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, safeName(n.Name)) }

func (b *BinaryExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	if u.Op == "typeof" || strings.HasSuffix(u.Op, " ") {
		// operators like "typeof" require a trailing space before the operand
		if !strings.HasSuffix(u.Op, " ") {
			io.WriteString(w, " ")
		}
	}
	u.Expr.emit(w)
}

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

func (l *LenExpr) emit(w io.Writer) {
	if l.Value == nil {
		io.WriteString(w, "0")
		return
	}
	io.WriteString(w, "_len(")
	l.Value.emit(w)
	io.WriteString(w, ")")
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "[")
	io.WriteString(w, "...")
	if a.List != nil {
		a.List.emit(w)
	}
	io.WriteString(w, ", ")
	if a.Elem != nil {
		a.Elem.emit(w)
	}
	io.WriteString(w, "]")
}

func (s *SpreadExpr) emit(w io.Writer) {
	io.WriteString(w, "...")
	if s.Expr != nil {
		s.Expr.emit(w)
	}
}

func (e *AvgExpr) emit(w io.Writer) {
	io.WriteString(w, "(() => { const arr = ")
	if e.Value != nil {
		e.Value.emit(w)
	}
	io.WriteString(w, "; return (arr.reduce((a, b) => a + b, 0.0) / arr.length).toFixed(1); })()")
}

func (e *SumExpr) emit(w io.Writer) {
	if e.Value != nil {
		e.Value.emit(w)
		io.WriteString(w, ".reduce((a, b) => a + b, 0.0)")
	} else {
		io.WriteString(w, "0.0")
	}
}

func (e *MinExpr) emit(w io.Writer) {
	io.WriteString(w, "(() => { const _arr = ")
	if e.Value != nil {
		e.Value.emit(w)
	}
	io.WriteString(w, "; return _arr.length === 0 ? 0 : Math.min(..._arr); })()")
}

func (e *MaxExpr) emit(w io.Writer) {
	io.WriteString(w, "(() => { const _arr = ")
	if e.Value != nil {
		e.Value.emit(w)
	}
	io.WriteString(w, "; return _arr.length === 0 ? 0 : Math.max(..._arr); })()")
}

func (e *IntDivExpr) emit(w io.Writer) {
	if e.Big {
		if e.Left != nil {
			e.Left.emit(w)
		}
		io.WriteString(w, " / ")
		if e.Right != nil {
			e.Right.emit(w)
		}
	} else {
		io.WriteString(w, "Math.trunc(")
		if e.Left != nil {
			e.Left.emit(w)
		}
		io.WriteString(w, " / ")
		if e.Right != nil {
			e.Right.emit(w)
		}
		io.WriteString(w, ")")
	}
}

func (e *ValuesExpr) emit(w io.Writer) {
	io.WriteString(w, "Object.values(")
	if e.Value != nil {
		e.Value.emit(w)
	}
	io.WriteString(w, ")")
}

func (u *UnionExpr) emit(w io.Writer) {
	io.WriteString(w, "Array.from(new Set([...")
	if u.Left != nil {
		u.Left.emit(w)
	}
	io.WriteString(w, ", ...")
	if u.Right != nil {
		u.Right.emit(w)
	}
	io.WriteString(w, "]))")
}

func (u *UnionAllExpr) emit(w io.Writer) {
	io.WriteString(w, "[")
	io.WriteString(w, "...")
	if u.Left != nil {
		u.Left.emit(w)
	}
	io.WriteString(w, ", ...")
	if u.Right != nil {
		u.Right.emit(w)
	}
	io.WriteString(w, "]")
}

func (e *ExceptExpr) emit(w io.Writer) {
	if e.Left != nil {
		e.Left.emit(w)
	}
	io.WriteString(w, ".filter(x => !")
	if e.Right != nil {
		e.Right.emit(w)
	}
	io.WriteString(w, ".includes(x))")
}

func (e *IntersectExpr) emit(w io.Writer) {
	if e.Left != nil {
		e.Left.emit(w)
	}
	io.WriteString(w, ".filter(x => ")
	if e.Right != nil {
		e.Right.emit(w)
	}
	io.WriteString(w, ".includes(x))")
}

func (f *FormatListExpr) emit(w io.Writer) {
	io.WriteString(w, "\"[\" + (")
	if f.Value != nil {
		f.Value.emit(w)
	} else {
		io.WriteString(w, "[]")
	}
	io.WriteString(w, `).join(' ') + "]"`)
}

func (p *PrintExpr) emit(w io.Writer) {
	args := p.Args
	newline := true
	if len(args) > 0 {
		if b, ok := args[len(args)-1].(*BoolLit); ok {
			newline = b.Value
			args = args[:len(args)-1]
		}
	}
	if !newline {
		io.WriteString(w, "_stdout_write(")
	} else {
		io.WriteString(w, "console.log(")
	}
	if len(args) == 0 {
		io.WriteString(w, "\"\"")
	}
	for i, a := range args {
		if i > 0 {
			io.WriteString(w, " + \" \" + ")
		}
		if a != nil {
			useStr = true
			io.WriteString(w, "_str(")
			a.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "\"\"")
		}
	}
	io.WriteString(w, ")")
}

func (b *BoolStringExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	if b.Value != nil {
		b.Value.emit(w)
	}
	io.WriteString(w, " ? \"True\" : \"False\")")
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	if s.Str != nil {
		s.Str.emit(w)
	}
	io.WriteString(w, ").substring(")
	if s.Start != nil {
		s.Start.emit(w)
	}
	io.WriteString(w, ", ")
	if s.End != nil {
		s.End.emit(w)
	}
	io.WriteString(w, ")")
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "{")
	for i, e := range m.Entries {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		switch k := e.Key.(type) {
		case *StringLit:
			if v, ok := e.Value.(*NameRef); ok && v.Name == k.Value {
				io.WriteString(w, k.Value)
				continue
			}
			fmt.Fprintf(w, "%q: ", k.Value)
			e.Value.emit(w)
		case *NameRef:
			if v, ok := e.Value.(*NameRef); ok && v.Name == k.Name {
				io.WriteString(w, k.Name)
				continue
			}
			io.WriteString(w, k.Name)
			io.WriteString(w, ": ")
			e.Value.emit(w)
		default:
			io.WriteString(w, "[")
			e.Key.emit(w)
			io.WriteString(w, "]: ")
			e.Value.emit(w)
		}
	}
	io.WriteString(w, "}")
}

func (i *IndexExpr) emit(w io.Writer) {
	if s, ok := i.Index.(*StringLit); ok && isIdent(s.Value) {
		i.Target.emit(w)
		io.WriteString(w, ".")
		io.WriteString(w, s.Value)
		return
	}
	i.Target.emit(w)
	io.WriteString(w, "[")
	if i.Index != nil {
		io.WriteString(w, "(")
		i.Index.emit(w)
		io.WriteString(w, ") < 0 ? ")
		i.Target.emit(w)
		io.WriteString(w, ".length + (")
		i.Index.emit(w)
		io.WriteString(w, ") : ")
		i.Index.emit(w)
	}
	io.WriteString(w, "]")
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, ".slice(")
	if s.Start != nil {
		s.Start.emit(w)
	} else if s.End != nil {
		io.WriteString(w, "0")
	}
	if s.End != nil {
		io.WriteString(w, ", ")
		s.End.emit(w)
	}
	io.WriteString(w, ")")
}

func (m *MethodCallExpr) emit(w io.Writer) {
	if m.Method == "keys" && len(m.Args) == 0 {
		io.WriteString(w, "Object.keys(")
		if m.Target != nil {
			m.Target.emit(w)
		}
		io.WriteString(w, ").map(k => +k)")
		return
	}
	if m.Method == "get" && len(m.Args) >= 1 {
		io.WriteString(w, "(")
		if m.Target != nil {
			m.Target.emit(w)
		}
		io.WriteString(w, "[")
		m.Args[0].emit(w)
		io.WriteString(w, "]")
		if len(m.Args) == 2 {
			io.WriteString(w, " ?? ")
			m.Args[1].emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	m.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, safeName(m.Method))
	io.WriteString(w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func (m *MapKeysExpr) emit(w io.Writer) {
	io.WriteString(w, "Object.keys(")
	if m.Target != nil {
		m.Target.emit(w)
	}
	io.WriteString(w, ").map(k => +k)")
}

func (f *FunExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") => ")
	if f.Expr != nil {
		f.Expr.emit(w)
	} else {
		io.WriteString(w, "{\n")
		for _, st := range f.Body {
			st.emit(w)
			io.WriteString(w, "\n")
		}
		io.WriteString(w, "}")
	}
}

func (i *InvokeExpr) emit(w io.Writer) {
	i.Callee.emit(w)
	io.WriteString(w, "(")
	for j, a := range i.Args {
		if j > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func (e *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	if e.Cond != nil {
		e.Cond.emit(w)
	}
	io.WriteString(w, " ? ")
	if e.Then != nil {
		e.Then.emit(w)
	} else {
		io.WriteString(w, "null")
	}
	io.WriteString(w, " : ")
	if e.Else != nil {
		e.Else.emit(w)
	} else {
		io.WriteString(w, "null")
	}
	io.WriteString(w, ")")
}

func (q *QueryExprJS) emit(w io.Writer) {
	iw := &indentWriter{w: w, indent: "  "}
	io.WriteString(iw, "(() => {\n")
	simple := len(q.Loops) == 1 && len(q.Joins) > 0 && q.Sort == nil && q.Skip == nil && q.Take == nil
	if simple {
		for _, j := range q.Joins {
			if j.Side != "" {
				simple = false
				break
			}
		}
	}
	if simple {
		io.WriteString(iw, "  const result")
		if q.ElemType != "" {
			io.WriteString(iw, ": ")
			io.WriteString(iw, q.ElemType)
			io.WriteString(iw, "[]")
		}
		io.WriteString(iw, " = []\n")
		io.WriteString(iw, "  for (const ")
		io.WriteString(iw, q.Loops[0].Name)
		io.WriteString(iw, " of ")
		q.Loops[0].Source.emit(iw)
		io.WriteString(iw, ") {\n")
		var emitJoin func(int, int)
		emitJoin = func(idx, level int) {
			if idx >= len(q.Joins) {
				if q.Where != nil {
					io.WriteString(iw, strings.Repeat(iw.indent, level))
					io.WriteString(iw, "if (!(")
					q.Where.emit(iw)
					io.WriteString(iw, ")) continue\n")
				}
				io.WriteString(iw, strings.Repeat(iw.indent, level))
				io.WriteString(iw, "result.push(")
				q.Select.emit(iw)
				io.WriteString(iw, ")\n")
				return
			}
			j := q.Joins[idx]
			io.WriteString(iw, strings.Repeat(iw.indent, level))
			io.WriteString(iw, "for (const ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, " of ")
			j.Source.emit(iw)
			io.WriteString(iw, ") {\n")
			io.WriteString(iw, strings.Repeat(iw.indent, level+1))
			io.WriteString(iw, "if (!(")
			j.On.emit(iw)
			io.WriteString(iw, ")) continue\n")
			emitJoin(idx+1, level+1)
			io.WriteString(iw, strings.Repeat(iw.indent, level))
			io.WriteString(iw, "}\n")
		}
		emitJoin(0, 1)
		io.WriteString(iw, "  }\n  return result\n")
		io.WriteString(iw, "})()")
		return
	}

	leftSimple := len(q.Loops) == 1 && len(q.Joins) == 1 && q.Joins[0].Side == "left" && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil
	if leftSimple {
		j := q.Joins[0]
		io.WriteString(iw, "  const result")
		if q.ElemType != "" {
			io.WriteString(iw, ": ")
			io.WriteString(iw, q.ElemType)
			io.WriteString(iw, "[]")
		}
		io.WriteString(iw, " = []\n")
		io.WriteString(iw, "  for (const ")
		io.WriteString(iw, q.Loops[0].Name)
		io.WriteString(iw, " of ")
		q.Loops[0].Source.emit(iw)
		io.WriteString(iw, ") {\n")
		io.WriteString(iw, "    let matched = false\n")
		io.WriteString(iw, "    for (const ")
		io.WriteString(iw, j.Name)
		io.WriteString(iw, " of ")
		j.Source.emit(iw)
		io.WriteString(iw, ") {\n")
		io.WriteString(iw, "      if (!(")
		j.On.emit(iw)
		io.WriteString(iw, ")) continue\n")
		io.WriteString(iw, "      matched = true\n")
		io.WriteString(iw, "      result.push(")
		q.Select.emit(iw)
		io.WriteString(iw, ")\n")
		io.WriteString(iw, "    }\n")
		io.WriteString(iw, "    if (!matched) result.push(")
		emitReplaceName(iw, q.Select, j.Name, &NullLit{})
		io.WriteString(iw, ")\n")
		io.WriteString(iw, "  }\n  return result\n")
		io.WriteString(iw, "})()")
		return
	}
	if len(q.Joins) == 0 && len(q.Loops) <= 1 {
		io.WriteString(iw, "  const result")
		if q.ElemType != "" {
			io.WriteString(iw, ": ")
			io.WriteString(iw, q.ElemType)
			io.WriteString(iw, "[]")
		}
		io.WriteString(iw, " = []\n")
		var emitLoops func(int, int)
		emitLoops = func(idx, level int) {
			if idx >= len(q.Loops) {
				if q.Where != nil {
					io.WriteString(iw, strings.Repeat(iw.indent, level))
					io.WriteString(iw, "if (")
					q.Where.emit(iw)
					io.WriteString(iw, ") {\n")
					level++
				}
				io.WriteString(iw, strings.Repeat(iw.indent, level))
				if q.Sort != nil {
					io.WriteString(iw, "result.push({k: ")
					q.Sort.emit(iw)
					io.WriteString(iw, ", v: ")
					q.Select.emit(iw)
					io.WriteString(iw, "})\n")
				} else {
					io.WriteString(iw, "result.push(")
					q.Select.emit(iw)
					io.WriteString(iw, ")\n")
				}
				if q.Where != nil {
					level--
					io.WriteString(iw, strings.Repeat(iw.indent, level))
					io.WriteString(iw, "}\n")
				}
				return
			}
			loop := q.Loops[idx]
			io.WriteString(iw, strings.Repeat(iw.indent, level))
			io.WriteString(iw, "for (const ")
			io.WriteString(iw, loop.Name)
			io.WriteString(iw, " of ")
			loop.Source.emit(iw)
			io.WriteString(iw, ") {\n")
			emitLoops(idx+1, level+1)
			io.WriteString(iw, strings.Repeat(iw.indent, level))
			io.WriteString(iw, "}\n")
		}
		emitLoops(0, 1)
		if q.Sort != nil {
			io.WriteString(iw, "  result.sort((a, b) => ")
			emitSortComparator(iw, q.Sort)
			io.WriteString(iw, ")\n")
			io.WriteString(iw, "  const out = result.map(r => r.v)\n")
		} else {
			io.WriteString(iw, "  const out = result\n")
		}
		if q.Skip != nil || q.Take != nil {
			io.WriteString(iw, "  return out.slice(")
			if q.Skip != nil {
				q.Skip.emit(iw)
			} else {
				io.WriteString(iw, "0")
			}
			if q.Take != nil {
				io.WriteString(iw, ", ")
				if q.Skip != nil {
					io.WriteString(iw, "(")
					q.Skip.emit(iw)
					io.WriteString(iw, " + ")
					q.Take.emit(iw)
					io.WriteString(iw, ")")
				} else {
					q.Take.emit(iw)
				}
			}
			io.WriteString(iw, ")\n")
		} else {
			io.WriteString(iw, "  return out\n")
		}
		io.WriteString(iw, "})()")
		return
	}

	// cross joins for additional loops
	io.WriteString(iw, "  let _items = ")
	if len(q.Loops) > 0 {
		q.Loops[0].Source.emit(iw)
	} else {
		io.WriteString(iw, "[]")
	}
	io.WriteString(iw, ".map(v => [v])\n")

	names := []string{}
	if len(q.Loops) > 0 {
		names = append(names, q.Loops[0].Name)
	}
	for i := 1; i < len(q.Loops); i++ {
		loop := q.Loops[i]
		io.WriteString(iw, "  { const _next = []\n")
		io.WriteString(iw, "    for (const it of _items) {\n")
		io.WriteString(iw, "      for (const ")
		io.WriteString(iw, loop.Name)
		io.WriteString(iw, " of ")
		loop.Source.emit(iw)
		io.WriteString(iw, ") { _next.push([...it, ")
		io.WriteString(iw, loop.Name)
		io.WriteString(iw, "]) }\n")
		io.WriteString(iw, "    }\n")
		io.WriteString(iw, "    _items = _next }\n")
		names = append(names, loop.Name)
	}

	for _, j := range q.Joins {
		io.WriteString(iw, "  { const _joined = []\n")
		io.WriteString(iw, "    const _arr = ")
		j.Source.emit(iw)
		io.WriteString(iw, "\n")
		if j.Side == "right" {
			io.WriteString(iw, "    for (let _ri=0; _ri < _arr.length; _ri++) {\n")
			io.WriteString(iw, "      const ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, " = _arr[_ri];\n")
			io.WriteString(iw, "      let _m = false;\n")
			io.WriteString(iw, "      for (const _left of _items) {\n")
			if len(names) > 0 {
				io.WriteString(iw, "        const [")
				io.WriteString(iw, strings.Join(names, ", "))
				io.WriteString(iw, "] = _left;\n")
			}
			if j.On != nil {
				io.WriteString(iw, "        if (!(")
				j.On.emit(iw)
				io.WriteString(iw, ")) continue;\n")
			}
			io.WriteString(iw, "        _m = true; _joined.push([..._left, ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, "]) }\n")
			io.WriteString(iw, "      if (!_m) { const _undef = Array(_items[0]?.length || 0).fill(null); _joined.push([..._undef, ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, "]) }\n")
			io.WriteString(iw, "    }\n")
		} else {
			if j.Side == "right" || j.Side == "outer" {
				io.WriteString(iw, "    const _matched = new Array(_arr.length).fill(false)\n")
			}
			io.WriteString(iw, "    for (const _left of _items) {\n")
			if len(names) > 0 {
				io.WriteString(iw, "      const [")
				io.WriteString(iw, strings.Join(names, ", "))
				io.WriteString(iw, "] = _left;\n")
			}
			io.WriteString(iw, "      let _m = false;\n")
			io.WriteString(iw, "      for (let _ri=0; _ri < _arr.length; _ri++) {\n")
			io.WriteString(iw, "        const ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, " = _arr[_ri];\n")
			if j.On != nil {
				io.WriteString(iw, "        if (!(")
				j.On.emit(iw)
				io.WriteString(iw, ")) continue;\n")
			}
			io.WriteString(iw, "        _m = true;")
			if j.Side == "right" || j.Side == "outer" {
				io.WriteString(iw, " _matched[_ri] = true;")
			}
			io.WriteString(iw, " _joined.push([..._left, ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, "]) }\n")

			if j.Side == "left" || j.Side == "outer" {
				io.WriteString(iw, "      if (!_m) _joined.push([..._left, null])\n")
			}
			io.WriteString(iw, "    }\n")
			if j.Side == "right" || j.Side == "outer" {
				io.WriteString(iw, "    for (let _ri=0; _ri < _arr.length; _ri++) { if (!_matched[_ri]) {\n")
				io.WriteString(iw, "      const _undef = Array(_items[0]?.length || 0).fill(null);\n")
				io.WriteString(iw, "      _joined.push([..._undef, _arr[_ri]]) } }\n")
			}
		}
		io.WriteString(iw, "    _items = _joined;\n")
		io.WriteString(iw, "  }\n")
		names = append(names, j.Name)
	}

	if q.Where != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || len(names) > 0 {
		io.WriteString(iw, "  let _rows = _items\n")
	} else {
		io.WriteString(iw, "  let _rows = _items\n")
	}

	if q.Where != nil {
		io.WriteString(iw, "  _rows = _rows.filter(r => { const [")
		io.WriteString(iw, strings.Join(names, ", "))
		io.WriteString(iw, "] = r; return ")
		q.Where.emit(iw)
		io.WriteString(iw, " })\n")
	}

	if q.Sort != nil {
		io.WriteString(iw, "  { const _pairs = _rows.map(r => { const [")
		io.WriteString(iw, strings.Join(names, ", "))
		io.WriteString(iw, "] = r; return {item: r, key: ")
		q.Sort.emit(iw)
		io.WriteString(iw, "} });\n")
		io.WriteString(iw, "    _pairs.sort((a,b)=>{const ak=a.key;const bk=b.key;if(ak<bk)return -1;if(ak>bk)return 1;const sak=JSON.stringify(ak);const sbk=JSON.stringify(bk);return sak<sbk?-1:sak>sbk?1:0});\n")
		io.WriteString(iw, "    _rows = _pairs.map(p=>p.item); }\n")
	}

	if q.Skip != nil {
		io.WriteString(iw, "  { const n = ")
		q.Skip.emit(iw)
		io.WriteString(iw, "; _rows = n < _rows.length ? _rows.slice(n) : []; }\n")
	}

	if q.Take != nil {
		io.WriteString(iw, "  { const n = ")
		q.Take.emit(iw)
		io.WriteString(iw, "; if (n < _rows.length) _rows = _rows.slice(0, n); }\n")
	}

	io.WriteString(iw, "  const result")
	if q.ElemType != "" {
		io.WriteString(iw, ": ")
		io.WriteString(iw, q.ElemType)
		io.WriteString(iw, "[]")
	}
	io.WriteString(iw, " = []\n")
	io.WriteString(iw, "  for (const r of _rows) { const [")
	io.WriteString(iw, strings.Join(names, ", "))
	io.WriteString(iw, "] = r; result.push(")
	q.Select.emit(iw)
	io.WriteString(iw, ") }\n")
	io.WriteString(iw, "  return result\n")
	io.WriteString(iw, "})()")
}

func (gq *GroupQueryExpr) emit(w io.Writer) {
	iw := &indentWriter{w: w, indent: "  "}
	io.WriteString(iw, "(() => {\n")
	io.WriteString(iw, "  const groups = new Map<string, {key: any; items: any[]}>()\n")
	io.WriteString(iw, "  for (const ")
	io.WriteString(iw, gq.Var)
	io.WriteString(iw, " of ")
	gq.Source.emit(iw)
	io.WriteString(iw, ") {\n")
	if gq.Cond != nil {
		io.WriteString(iw, "    if (!(")
		gq.Cond.emit(iw)
		io.WriteString(iw, ")) continue\n")
	}
	io.WriteString(iw, "    const k = ")
	gq.Key.emit(iw)
	io.WriteString(iw, "\n    const ks = JSON.stringify(k)\n    let g = groups.get(ks)\n    if (!g) { g = {key: k, items: []}; groups.set(ks, g) }\n    g.items.push(")
	gq.Row.emit(iw)
	io.WriteString(iw, ")\n  }\n")
	io.WriteString(iw, "  let ordered = Array.from(groups.values())\n")
	io.WriteString(iw, "  const result")
	if gq.ElemType != "" {
		io.WriteString(iw, ": ")
		io.WriteString(iw, gq.ElemType)
		io.WriteString(iw, "[]")
	}
	io.WriteString(iw, " = []\n")
	if gq.Sort != nil {
		io.WriteString(iw, "  const pairs = ordered.map(grp => { const ")
		io.WriteString(iw, gq.GroupVar)
		io.WriteString(iw, " = grp; return {g: ")
		io.WriteString(iw, gq.GroupVar)
		io.WriteString(iw, ", key: ")
		gq.Sort.emit(iw)
		io.WriteString(iw, "} })\n")
		io.WriteString(iw, "  pairs.sort((a,b)=>{const ak=a.key;const bk=b.key;if(ak<bk)return -1;if(ak>bk)return 1;const sak=JSON.stringify(ak);const sbk=JSON.stringify(bk);return sak<sbk?-1:sak>sbk?1:0})\n")
		io.WriteString(iw, "  ordered = pairs.map(p => p.g)\n")
	}
	io.WriteString(iw, "  for (const ")
	io.WriteString(iw, gq.GroupVar)
	io.WriteString(iw, " of ordered) {\n")
	if gq.Having != nil {
		io.WriteString(iw, "    if (")
		gq.Having.emit(iw)
		io.WriteString(iw, ") {\n      result.push(")
		gq.Select.emit(iw)
		io.WriteString(iw, ")\n    }\n")
	} else {
		io.WriteString(iw, "    result.push(")
		gq.Select.emit(iw)
		io.WriteString(iw, ")\n")
	}
	io.WriteString(iw, "  }\n  return result\n")
	io.WriteString(iw, "})()")
}

func (gq *GroupJoinQueryExpr) emit(w io.Writer) {
	iw := &indentWriter{w: w, indent: "  "}
	io.WriteString(iw, "(() => {\n")
	io.WriteString(iw, "  const groups = new Map<string, {key: any; items: any[]}>()\n")
	io.WriteString(iw, "  let rows = ")
	if len(gq.Loops) > 0 {
		gq.Loops[0].Source.emit(iw)
	} else {
		io.WriteString(iw, "[]")
	}
	io.WriteString(iw, ".map(v => [v])\n")
	names := []string{}
	if len(gq.Loops) > 0 {
		names = append(names, gq.Loops[0].Name)
	}
	for i := 1; i < len(gq.Loops); i++ {
		loop := gq.Loops[i]
		io.WriteString(iw, "  { const next = []\n")
		io.WriteString(iw, "    for (const it of rows) {\n")
		io.WriteString(iw, "      for (const ")
		io.WriteString(iw, loop.Name)
		io.WriteString(iw, " of ")
		loop.Source.emit(iw)
		io.WriteString(iw, ") { next.push([...it, ")
		io.WriteString(iw, loop.Name)
		io.WriteString(iw, "]) }\n")
		io.WriteString(iw, "    }\n")
		io.WriteString(iw, "    rows = next }\n")
		names = append(names, loop.Name)
	}
	for _, j := range gq.Joins {
		io.WriteString(iw, "  { const joined = []\n")
		io.WriteString(iw, "    const arr = ")
		j.Source.emit(iw)
		io.WriteString(iw, "\n")
		if j.Side == "right" {
			io.WriteString(iw, "    for (let ri=0; ri < arr.length; ri++) {\n")
			io.WriteString(iw, "      const ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, " = arr[ri];\n")
			io.WriteString(iw, "      let m = false;\n")
			io.WriteString(iw, "      for (const left of rows) {\n")
			if len(names) > 0 {
				io.WriteString(iw, "        const [")
				io.WriteString(iw, strings.Join(names, ", "))
				io.WriteString(iw, "] = left;\n")
			}
			if j.On != nil {
				io.WriteString(iw, "        if (!(")
				j.On.emit(iw)
				io.WriteString(iw, ")) continue;\n")
			}
			io.WriteString(iw, "        m = true; joined.push([...left, ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, "]) }\n")
			io.WriteString(iw, "      if (!m) { const undef = Array(rows[0]?.length || 0).fill(null); joined.push([...undef, ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, "]) }\n")
			io.WriteString(iw, "    }\n")
		} else {
			if j.Side == "right" || j.Side == "outer" {
				io.WriteString(iw, "    const matched = new Array(arr.length).fill(false)\n")
			}
			io.WriteString(iw, "    for (const left of rows) {\n")
			if len(names) > 0 {
				io.WriteString(iw, "      const [")
				io.WriteString(iw, strings.Join(names, ", "))
				io.WriteString(iw, "] = left;\n")
			}
			io.WriteString(iw, "      let m = false;\n")
			io.WriteString(iw, "      for (let ri=0; ri < arr.length; ri++) {\n")
			io.WriteString(iw, "        const ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, " = arr[ri];\n")
			if j.On != nil {
				io.WriteString(iw, "        if (!(")
				j.On.emit(iw)
				io.WriteString(iw, ")) continue;\n")
			}
			io.WriteString(iw, "        m = true;")
			if j.Side == "right" || j.Side == "outer" {
				io.WriteString(iw, " matched[ri] = true;")
			}
			io.WriteString(iw, " joined.push([...left, ")
			io.WriteString(iw, j.Name)
			io.WriteString(iw, "]) }\n")
			if j.Side == "left" || j.Side == "outer" {
				io.WriteString(iw, "      if (!m) joined.push([...left, null])\n")
			}
			io.WriteString(iw, "    }\n")
			if j.Side == "right" || j.Side == "outer" {
				io.WriteString(iw, "    for (let ri=0; ri < arr.length; ri++) { if (!matched[ri]) {\n")
				io.WriteString(iw, "      const undef = Array(rows[0]?.length || 0).fill(null);\n")
				io.WriteString(iw, "      joined.push([...undef, arr[ri]]) } }\n")
			}
		}
		io.WriteString(iw, "    rows = joined;\n")
		io.WriteString(iw, "  }\n")
		names = append(names, j.Name)
	}
	io.WriteString(iw, "  for (const it of rows) {\n")
	if len(names) > 0 {
		io.WriteString(iw, "    const [")
		io.WriteString(iw, strings.Join(names, ", "))
		io.WriteString(iw, "] = it;\n")
	}
	if gq.Where != nil {
		io.WriteString(iw, "    if (!(")
		gq.Where.emit(iw)
		io.WriteString(iw, ")) continue\n")
	}
	io.WriteString(iw, "    const k = ")
	gq.Key.emit(iw)
	io.WriteString(iw, "\n    const ks = JSON.stringify(k)\n    let g = groups.get(ks)\n    if (!g) { g = {key: k, items: []}; groups.set(ks, g) }\n    g.items.push(")
	gq.Row.emit(iw)
	io.WriteString(iw, ")\n  }\n")
	io.WriteString(iw, "  let ordered = Array.from(groups.values())\n")
	io.WriteString(iw, "  const result")
	if gq.ElemType != "" {
		io.WriteString(iw, ": ")
		io.WriteString(iw, gq.ElemType)
		io.WriteString(iw, "[]")
	}
	io.WriteString(iw, " = []\n")
	if gq.Sort != nil {
		io.WriteString(iw, "  const pairs = ordered.map(grp => { const ")
		io.WriteString(iw, gq.GroupVar)
		io.WriteString(iw, " = grp; return {g: ")
		io.WriteString(iw, gq.GroupVar)
		io.WriteString(iw, ", key: ")
		gq.Sort.emit(iw)
		io.WriteString(iw, "} })\n")
		io.WriteString(iw, "  pairs.sort((a,b)=>{const ak=a.key;const bk=b.key;if(ak<bk)return -1;if(ak>bk)return 1;const sak=JSON.stringify(ak);const sbk=JSON.stringify(bk);return sak<sbk?-1:sak>sbk?1:0})\n")
		io.WriteString(iw, "  ordered = pairs.map(p => p.g)\n")
	}
	io.WriteString(iw, "  for (const ")
	io.WriteString(iw, gq.GroupVar)
	io.WriteString(iw, " of ordered) {\n")
	if gq.Having != nil {
		io.WriteString(iw, "    if (")
		gq.Having.emit(iw)
		io.WriteString(iw, ") {\n      result.push(")
		gq.Select.emit(iw)
		io.WriteString(iw, ")\n    }\n")
	} else {
		io.WriteString(iw, "    result.push(")
		gq.Select.emit(iw)
		io.WriteString(iw, ")\n")
	}
	io.WriteString(iw, "  }\n  return result\n")
	io.WriteString(iw, "})()")
}

func (a *AggQueryExpr) emit(w io.Writer) {
	iw := &indentWriter{w: w, indent: "  "}
	io.WriteString(iw, "(() => {\n")
	init := "0"
	switch a.Op {
	case "min":
		init = "Infinity"
	case "max":
		init = "-Infinity"
	}
	io.WriteString(iw, "  let acc = "+init+"\n")
	if a.Op == "avg" {
		io.WriteString(iw, "  let count = 0\n")
	}
	io.WriteString(iw, "  for (const ")
	io.WriteString(iw, a.Var)
	io.WriteString(iw, " of ")
	a.Source.emit(iw)
	io.WriteString(iw, ") {\n")
	if a.Where != nil {
		io.WriteString(iw, "    if (")
		a.Where.emit(iw)
		io.WriteString(iw, ") {\n")
	}
	switch a.Op {
	case "sum", "avg":
		io.WriteString(iw, "    acc += ")
		io.WriteString(iw, a.Var)
		io.WriteString(iw, "\n")
		if a.Op == "avg" {
			io.WriteString(iw, "    count++\n")
		}
	case "min":
		io.WriteString(iw, "    if (")
		io.WriteString(iw, a.Var)
		io.WriteString(iw, " < acc) acc = ")
		io.WriteString(iw, a.Var)
		io.WriteString(iw, "\n")
	case "max":
		io.WriteString(iw, "    if (")
		io.WriteString(iw, a.Var)
		io.WriteString(iw, " > acc) acc = ")
		io.WriteString(iw, a.Var)
		io.WriteString(iw, "\n")
	}
	if a.Where != nil {
		io.WriteString(iw, "    }\n")
	}
	io.WriteString(iw, "  }\n")
	io.WriteString(iw, "  return ")
	switch a.Op {
	case "avg":
		io.WriteString(iw, "count === 0 ? 0 : acc / count")
	case "min":
		io.WriteString(iw, "acc === Infinity ? 0 : acc")
	case "max":
		io.WriteString(iw, "acc === -Infinity ? 0 : acc")
	default:
		io.WriteString(iw, "acc")
	}
	io.WriteString(iw, "\n")
	io.WriteString(iw, "})()")
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, " = ")
	s.Value.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "for (const _row of ")
		if s.Src != nil {
			s.Src.emit(w)
		}
		io.WriteString(w, ") {\n")
		io.WriteString(w, "  const _keys = Object.keys(_row).sort()\n")
		io.WriteString(w, "  const _tmp:any = {}\n")
		io.WriteString(w, "  for (const k of _keys) _tmp[k] = _row[k]\n")
		io.WriteString(w, "  console.log(JSON.stringify(_tmp))\n")
		io.WriteString(w, "}")
		return
	}
	io.WriteString(w, "// unsupported save")
}

func (b *BenchStmt) emit(w io.Writer) {
	// hoist variable declarations so other top-level functions can access them
	for _, st := range b.Body {
		if v, ok := st.(*VarDecl); ok {
			fmt.Fprintf(w, "let %s", v.Name)
			if v.Type != "" {
				fmt.Fprintf(w, ": %s", v.Type)
			}
			io.WriteString(w, "\n")
		}
	}
	if b.Async {
		io.WriteString(w, "(async () => {\n")
	} else {
		io.WriteString(w, "(() => {\n")
	}
	io.WriteString(w, "  globalThis.gc?.()\n")
	io.WriteString(w, "  const _startMem = _mem()\n")
	io.WriteString(w, "  const _start = _now()\n")
	iw := &indentWriter{w: w, indent: "  "}
	for _, st := range b.Body {
		if v, ok := st.(*VarDecl); ok {
			fmt.Fprintf(iw, "%s = ", v.Name)
			v.Expr.emit(iw)
			io.WriteString(iw, "\n")
			continue
		}
		emitStmt(iw, st, 1)
	}
	io.WriteString(w, "  const _end = _now()\n")
	io.WriteString(w, "  const _duration = _end - _start\n")
	io.WriteString(w, "  const _duration_us = Math.trunc(_duration / 1000)\n")
	io.WriteString(w, "  globalThis.gc?.()\n")
	io.WriteString(w, "  const _endMem = _mem()\n")
	io.WriteString(w, "  const _memory_bytes = Math.max(0, _endMem - _startMem)\n")
	fmt.Fprintf(w, "  console.log(JSON.stringify({\n    \"duration_us\": _duration_us,\n    \"memory_bytes\": _memory_bytes,\n    \"name\": %q\n  }, null, \"  \"))\n", b.Name)
	io.WriteString(w, "})();\n")
}

func (i *IIFEStmt) emit(w io.Writer) {
	if i.Async && useFetch {
		io.WriteString(w, "(async () => {\n")
	} else {
		io.WriteString(w, "(() => {\n")
	}
	iw := &indentWriter{w: w, indent: "  "}
	for _, st := range i.Body {
		emitStmt(iw, st, 1)
	}
	io.WriteString(w, "})();")
}

func (u *UpdateStmt) emit(w io.Writer) {
	io.WriteString(w, "for (let i = 0; i < ")
	io.WriteString(w, u.Target)
	io.WriteString(w, ".length; i++) {\n")
	io.WriteString(w, "  let item = ")
	io.WriteString(w, u.Target)
	io.WriteString(w, "[i]\n")
	if u.Cond != nil {
		io.WriteString(w, "  if (")
		u.Cond.emit(w)
		io.WriteString(w, ") {\n")
	}
	pad := "  "
	if u.Cond != nil {
		pad = "    "
	}
	for i, f := range u.Fields {
		io.WriteString(w, pad)
		fmt.Fprintf(w, "item[%q] = ", f)
		u.Values[i].emit(w)
		io.WriteString(w, ";\n")
	}
	if u.Cond != nil {
		io.WriteString(w, "  }\n")
	}
	io.WriteString(w, "  ")
	io.WriteString(w, u.Target)
	io.WriteString(w, "[i] = item\n")
	io.WriteString(w, "}\n")
}

func (u *StructUpdateExpr) emit(w io.Writer) {
	io.WriteString(w, "({ ...")
	if u.Target != nil {
		u.Target.emit(w)
	}
	io.WriteString(w, ", ")
	fmt.Fprintf(w, "%q: ", u.Field)
	if u.Value != nil {
		u.Value.emit(w)
	}
	io.WriteString(w, " })")
}

func (r *RawExpr) emit(w io.Writer) { io.WriteString(w, r.Code) }

func (r *RawStmt) emit(w io.Writer) { io.WriteString(w, r.Code) }

func (v *VarDecl) emit(w io.Writer) {
	if v.Const {
		io.WriteString(w, "const ")
	} else {
		io.WriteString(w, "let ")
	}
	io.WriteString(w, safeName(v.Name))
	if v.Type != "" && v.Type != "any" {
		io.WriteString(w, ": ")
		io.WriteString(w, v.Type)
	}
	if v.Expr != nil {
		io.WriteString(w, " = ")
		v.Expr.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (a *AssignStmt) emit(w io.Writer) {
	if ap, ok := a.Expr.(*AppendExpr); ok {
		if nr, ok2 := ap.List.(*NameRef); ok2 && nr.Name == a.Name {
			io.WriteString(w, safeName(a.Name))
			io.WriteString(w, ".push(")
			if ap.Elem != nil {
				ap.Elem.emit(w)
			}
			io.WriteString(w, ")")
			if b, ok := w.(interface{ WriteByte(byte) error }); ok {
				b.WriteByte(';')
			} else {
				io.WriteString(w, ";")
			}
			return
		}
	}
	io.WriteString(w, safeName(a.Name))
	io.WriteString(w, " = ")
	a.Expr.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range i.Else {
			st.emit(w)
			io.WriteString(w, "\n")
		}
		io.WriteString(w, "}")
	}
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while (")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range wst.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (f *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "for (let ")
	io.WriteString(w, safeName(f.Name))
	io.WriteString(w, " = ")
	if f.Start != nil {
		f.Start.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, safeName(f.Name))
	io.WriteString(w, " < ")
	if f.End != nil {
		f.End.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, safeName(f.Name))
	io.WriteString(w, "++) {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (f *ForInStmt) emit(w io.Writer) {
	io.WriteString(w, "for (const ")
	io.WriteString(w, safeName(f.Name))
	if f.Keys {
		io.WriteString(w, " in ")
	} else {
		io.WriteString(w, " of ")
	}
	if f.Iterable != nil {
		f.Iterable.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break;") }

func (c *ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue;") }

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return")
	if r.Value != nil {
		io.WriteString(w, " ")
		r.Value.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (f *FuncDecl) emit(w io.Writer) {
	if f.Async {
		io.WriteString(w, "async function ")
	} else {
		io.WriteString(w, "function ")
	}
	io.WriteString(w, safeName(f.Name))
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, safeName(p))
		if i < len(f.ParamTypes) && f.ParamTypes[i] != "" && f.ParamTypes[i] != "any" {
			io.WriteString(w, ": ")
			io.WriteString(w, f.ParamTypes[i])
		}
	}
	io.WriteString(w, ")")
	if f.ReturnType != "" && f.ReturnType != "any" {
		io.WriteString(w, ": ")
		io.WriteString(w, f.ReturnType)
	}
	io.WriteString(w, " {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

// Emit converts the AST back into TypeScript source code.
type indentWriter struct {
	w      io.Writer
	indent string
}

func (iw *indentWriter) Write(p []byte) (int, error) {
	return iw.w.Write(p)
}

func (iw *indentWriter) WriteByte(c byte) error {
	if wb, ok := iw.w.(interface{ WriteByte(byte) error }); ok {
		return wb.WriteByte(c)
	}
	_, err := iw.w.Write([]byte{c})
	return err
}

func Emit(p *Program) []byte {
	var b bytes.Buffer
	if os.Getenv("MOCHI_NO_HEADER") == "" {
		b.Write(meta.Header("//"))
		b.WriteByte('\n')
	}
	iw := &indentWriter{w: &b, indent: "  "}
	for _, s := range p.Stmts {
		emitStmt(iw, s, 0)
	}
	code := b.Bytes()
	re := regexp.MustCompile(`= ([0-9]+);\n\s+e([0-9]+);`)
	code = re.ReplaceAll(code, []byte(`= ${1}e${2};`))
	return code
}

func emitStmt(w *indentWriter, s Stmt, level int) {
	pad := strings.Repeat(w.indent, level)
	switch st := s.(type) {
	case *RawStmt:
		io.WriteString(w, pad)
		st.emit(w)
		if !strings.HasSuffix(st.Code, "\n") {
			io.WriteString(w, "\n")
		}
	case *ExprStmt:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *VarDecl:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *TypeAlias:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *InterfaceDecl:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *AssignStmt:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *IndexAssignStmt:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *ReturnStmt:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *BreakStmt:
		io.WriteString(w, pad)
		io.WriteString(w, "break\n")
	case *ContinueStmt:
		io.WriteString(w, pad)
		io.WriteString(w, "continue\n")
	case *FuncDecl:
		io.WriteString(w, pad)
		if st.Async {
			io.WriteString(w, "async function ")
		} else {
			io.WriteString(w, "function ")
		}
		io.WriteString(w, st.Name)
		io.WriteString(w, "(")
		for i, p := range st.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, safeName(p))
			if i < len(st.ParamTypes) && st.ParamTypes[i] != "" && st.ParamTypes[i] != "any" {
				io.WriteString(w, ": ")
				io.WriteString(w, st.ParamTypes[i])
			}
		}
		io.WriteString(w, ")")
		if st.ReturnType != "" && st.ReturnType != "any" {
			io.WriteString(w, ": ")
			io.WriteString(w, st.ReturnType)
		}
		io.WriteString(w, " {\n")
		for _, bs := range st.Body {
			emitStmt(w, bs, level+1)
		}
		io.WriteString(w, pad)
		io.WriteString(w, "}\n")
	case *IfStmt:
		io.WriteString(w, pad)
		io.WriteString(w, "if (")
		if st.Cond != nil {
			st.Cond.emit(w)
		}
		io.WriteString(w, ") {\n")
		for _, bs := range st.Then {
			emitStmt(w, bs, level+1)
		}
		io.WriteString(w, pad)
		io.WriteString(w, "}")
		if len(st.Else) > 0 {
			io.WriteString(w, " else {\n")
			for _, bs := range st.Else {
				emitStmt(w, bs, level+1)
			}
			io.WriteString(w, pad)
			io.WriteString(w, "}")
		}
		io.WriteString(w, "\n")
	case *WhileStmt:
		io.WriteString(w, pad)
		io.WriteString(w, "while (")
		if st.Cond != nil {
			st.Cond.emit(w)
		}
		io.WriteString(w, ") {\n")
		for _, bs := range st.Body {
			emitStmt(w, bs, level+1)
		}
		io.WriteString(w, pad)
		io.WriteString(w, "}\n")
	case *ForRangeStmt:
		io.WriteString(w, pad)
		io.WriteString(w, "for (let ")
		io.WriteString(w, safeName(st.Name))
		io.WriteString(w, " = ")
		if st.Start != nil {
			st.Start.emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, "; ")
		io.WriteString(w, safeName(st.Name))
		io.WriteString(w, " < ")
		if st.End != nil {
			st.End.emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, "; ")
		io.WriteString(w, safeName(st.Name))
		io.WriteString(w, "++) {\n")
		for _, bs := range st.Body {
			emitStmt(w, bs, level+1)
		}
		io.WriteString(w, pad)
		io.WriteString(w, "}\n")
	case *ForInStmt:
		io.WriteString(w, pad)
		io.WriteString(w, "for (const ")
		io.WriteString(w, safeName(st.Name))
		if st.Keys {
			io.WriteString(w, " in ")
		} else {
			io.WriteString(w, " of ")
		}
		if st.Iterable != nil {
			st.Iterable.emit(w)
		}
		io.WriteString(w, ") {\n")
		for _, bs := range st.Body {
			emitStmt(w, bs, level+1)
		}
		io.WriteString(w, pad)
		io.WriteString(w, "}\n")
	case *BenchStmt:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	case *SaveStmt:
		io.WriteString(w, pad)
		st.emit(w)
		io.WriteString(w, "\n")
	}
}

// Transpile converts a Mochi program into a TypeScript AST. The resulting
// code aims to be idiomatic and readable TypeScript without depending on
// runtime helper libraries. Most Mochi features are supported including
// joins and grouping in query expressions.
func Transpile(prog *parser.Program, env *types.Env, benchMain bool) (*Program, error) {
	transpileEnv = env
	generatedTypes = map[string]bool{}
	prelude = nil
	pythonMathAliases = map[string]bool{}
	useNow = false
	useInput = false
	useKeys = false
	useNumDenom = false
	useSHA256 = false
	useRepeat = false
	useConcat = false
	useParseIntStr = false
	useStdout = false
	useHas = false
	useFetch = false
	useLookupHost = false
	useStr = false
	useLen = false
	usePanic = false
	usePyName = false
	funcDepth = 0
	returnOutsideFunc = false
	asyncFuncs = map[string]bool{}
	defer func() {
		transpileEnv = nil
		generatedTypes = nil
		prelude = nil
		pythonMathAliases = nil
		useNow = false
		useInput = false
		useKeys = false
		useNumDenom = false
		useSHA256 = false
		useRepeat = false
		useConcat = false
		useParseIntStr = false
		useStdout = false
		useHas = false
		useFetch = false
		useLookupHost = false
		useStr = false
		useLen = false
		usePanic = false
		usePyName = false
		funcDepth = 0
		returnOutsideFunc = false
		asyncFuncs = nil
	}()
	tsProg := &Program{}
	fixScientificNotation(prog)
	mainDefined := false
	mainCalled := false

	for _, st := range prog.Statements {
		if st.Fun != nil && st.Fun.Name == "main" {
			mainDefined = true
		}
		if st.Expr != nil && isTopLevelMainCall(st.Expr.Expr) {
			mainCalled = true
		}
	}

	for _, st := range prog.Statements {
		stmt, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			tsProg.Stmts = append(tsProg.Stmts, stmt)
		}
	}

	optimizeFillLoops(tsProg)

	if !benchMain && useFetch {
		for _, st := range tsProg.Stmts {
			if fd, ok := st.(*FuncDecl); ok && fd.Name == "main" {
				fd.Async = true
			}
		}
		for i, st := range tsProg.Stmts {
			if es, ok := st.(*ExprStmt); ok {
				if call, ok2 := es.Expr.(*CallExpr); ok2 && call.Func == "main" && len(call.Args) == 0 {
					tsProg.Stmts[i] = &ExprStmt{Expr: &AwaitExpr{X: call}}
				}
			}
		}
	}

	if benchMain {
		// keep type declarations and function declarations at top-level
		// so that exported interfaces or type aliases remain valid and
		// functions are in scope for other declarations within the bench
		// block.
		var mainStmts []Stmt
		for _, st := range tsProg.Stmts {
			switch st.(type) {
			case *InterfaceDecl, *TypeAlias, *FuncDecl:
				prelude = append(prelude, st)
				if fd, ok := st.(*FuncDecl); ok && fd.Name == "main" && useFetch {
					fd.Async = true
				}
			case *VarDecl:
				v := st.(*VarDecl)
				if simpleExpr(v.Expr) {
					prelude = append(prelude, st)
				} else {
					// keep declarations with non-trivial
					// initializers inside the benchmarked
					// block to preserve execution order
					mainStmts = append(mainStmts, st)
				}
			default:
				mainStmts = append(mainStmts, st)
			}
		}
		if mainDefined && !mainCalled {
			if useFetch {
				mainStmts = append(mainStmts, &ExprStmt{Expr: &AwaitExpr{X: &CallExpr{Func: "main"}}})
			} else {
				mainStmts = append(mainStmts, &ExprStmt{Expr: &CallExpr{Func: "main"}})
			}
		}
		tsProg.Stmts = []Stmt{&BenchStmt{Name: "main", Body: mainStmts, Async: useFetch}}
		useBench = true
		useNow = true
	}
	if useNow {
		seed := os.Getenv("MOCHI_NOW_SEED")
		if seed != "" {
			prelude = append(prelude, &RawStmt{Code: fmt.Sprintf(`var _nowSeed = %s;
var _nowSeeded = true;
function _now(): number {
  _nowSeed = (_nowSeed * 1664525 + 1013904223) %% 2147483647;
  return _nowSeed;
}`, seed)})
		} else {
			prelude = append(prelude, &RawStmt{Code: `var _nowSeed = 0;
var _nowSeeded = false;
{
  let s = "";
  if (typeof Deno !== "undefined") {
    try {
      s = Deno.env.get("MOCHI_NOW_SEED") ?? "";
    } catch (_e) {
      s = "";
    }
  } else if (typeof process !== "undefined") {
    s = process.env.MOCHI_NOW_SEED || "";
  }
  if (s) {
    const v = parseInt(s, 10);
    if (!isNaN(v)) {
      _nowSeed = v;
      _nowSeeded = true;
    }
  } else {
    _nowSeed = 1;
    _nowSeeded = true;
  }
}
function _now(): number {
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
    return _nowSeed;
  }
  if (typeof Deno !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  if (typeof performance !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  return Date.now() * 1000;
}`})
		}
	}
	if useBench {
		prelude = append(prelude, &RawStmt{Code: `function _mem(): number {
  if (typeof Deno !== 'undefined') {
    return (Deno.memoryUsage?.().heapUsed ?? 0);
  }
  if (typeof process !== 'undefined') {
    return process.memoryUsage().heapUsed;
  }
  return 0;
}`})
	}
	if useInput {
		prelude = append(prelude, &RawStmt{Code: `let _inputData: string[] | null = null;
function _input(): string {
  if (_inputData === null) {
    let data: string;
    if (typeof Deno !== 'undefined') {
      const dec = new TextDecoder();
      const chunks: string[] = [];
      const buf = new Uint8Array(1024);
      for (;;) {
        const n = Deno.stdin.readSync(buf);
        if (n === null) break;
        chunks.push(dec.decode(buf.subarray(0, n)));
        if (n < buf.length) break;
      }
      data = chunks.join('');
    } else {
      const fs = require('fs');
      data = fs.readFileSync(0, 'utf8');
    }
    _inputData = data.split(/\r?\n/);
  }
  const v = _inputData.shift();
  return v === undefined ? '' : v;
}`})
	}
	if useKeys {
		prelude = append(prelude, &RawStmt{Code: `function _keys(obj: any): any[] {
  return Object.keys(obj);
}`})
	}
	if useNumDenom {
		prelude = append(prelude, &RawStmt{Code: `function num(x: number): number { return x; }
function denom(_x: number): number { return 1; }`})
	}
	if useSHA256 {
		prelude = append(prelude, &RawStmt{Code: `import { createHash } from 'node:crypto';
function sha256(bs: number[]): number[] {
  const hash = createHash('sha256');
  hash.update(new Uint8Array(bs));
  return Array.from(hash.digest());
}`})
	}
	if useRepeat {
		prelude = append(prelude, &RawStmt{Code: `function repeat(s: string, n: number): string { return s.repeat(Math.trunc(n)); }`})
	}
	if useConcat {
		prelude = append(prelude, &RawStmt{Code: `function concat(a: any[], b: any[]): any[] { return a.concat(b); }`})
	}
	if useParseIntStr {
		prelude = append(prelude, &RawStmt{Code: `function parseIntStr(s: string, base: number): number { return parseInt(s, Math.trunc(base)); }`})
	}
	if useLen {
		prelude = append(prelude, &RawStmt{Code: `function _len(x: any): number { return Array.isArray(x) || typeof x === 'string' ? x.length : Object.keys(x ?? {}).length; }`})
	}
	if useStr {
		prelude = append(prelude, &RawStmt{Code: `function _str(x: any): string {
  if (typeof x === 'number') {
    if (Object.is(x, -0)) return '-0';
    if (x === Infinity) return '+Inf';
    if (x === -Infinity) return '-Inf';
    if (Number.isNaN(x)) return 'NaN';
  }
  return String(x);
}`})
	}
	if usePanic {
		prelude = append(prelude, &RawStmt{Code: `function _panic(msg: any): never { throw new Error(String(msg)); }`})
	}
	if usePyName {
		prelude = append(prelude, &RawStmt{Code: `const __name__ = "__main__";`})
	}
	if useHas {
		prelude = append(prelude, &RawStmt{Code: `function _has(obj: any, key: any): boolean {
  return Object.prototype.hasOwnProperty.call(obj, String(key));
}`})
	}
	if useStdout {
		prelude = append(prelude, &RawStmt{Code: `function _stdout_write(s: string) {
  if (typeof Deno !== 'undefined') {
    Deno.stdout.writeSync(new TextEncoder().encode(s));
  } else if (typeof process !== 'undefined') {
    process.stdout.write(s);
  } else {
    console.log(s);
  }
}`})
	}
	if useLookupHost {
		prelude = append(prelude, &RawStmt{Code: `async function _lookupHost(name: string): Promise<any[]> {
  try {
    if (typeof Deno !== 'undefined' && 'resolveDns' in Deno) {
      return [await Deno.resolveDns(name, 'A'), null];
    }
    const dns = require('dns').promises;
    return [await dns.resolve4(name), null];
  } catch (e) {
    return [[], e];
  }
}`})
	}
	if useFetch {
		prelude = append(prelude, &RawStmt{Code: `async function _fetch(url: string, opts?: any): Promise<any> {
  const init: RequestInit = { method: opts?.method ?? 'GET' };
  if (opts?.headers) init.headers = opts.headers;
  if (opts && 'body' in opts) init.body = JSON.stringify(opts.body);
  if (opts?.query) {
    const qs = new URLSearchParams();
    for (const [k, v] of Object.entries(opts.query)) qs.set(k, String(v));
    const sep = url.includes('?') ? '&' : '?';
    url = url + sep + qs.toString();
  }
  if (!/^https?:/.test(url)) {
    const root = new URL('../../../../..', import.meta.url).pathname;
    const path = url.startsWith('/') ? url : root + url;
    const text = await Deno.readTextFile(path);
    try { return JSON.parse(text); } catch { return text; }
  }
  const resp = await fetch(url, init);
  const text = await resp.text();
  try { return JSON.parse(text); } catch { return text; }
}`})
	}
	if len(prelude) > 0 {
		tsProg.Stmts = append(prelude, tsProg.Stmts...)
	}
	if returnOutsideFunc && !benchMain {
		var body []Stmt
		var decls []Stmt
		for _, st := range tsProg.Stmts {
			switch st.(type) {
			case *InterfaceDecl, *TypeAlias, *FuncDecl, *VarDecl:
				decls = append(decls, st)
			default:
				body = append(body, st)
			}
		}
		tsProg.Stmts = append(decls, &IIFEStmt{Body: body, Async: useFetch})
	}
	return tsProg, nil
}

func convertStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Test != nil:
		// Ignore test blocks
		return nil, nil
	case s.Let != nil:
		var e Expr
		var err error
		if s.Let.Value != nil {
			e, err = convertExpr(s.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Let.Type != nil {
			e = zeroValue(s.Let.Type, transpileEnv)
		}
		// `let` bindings in Mochi may be reassigned, so emit a normal
		// `let` declaration rather than `const` to match the VM
		// semantics.
		mutable := true
		var t types.Type
		var typErr error
		if transpileEnv != nil {
			t, typErr = transpileEnv.GetVar(s.Let.Name)
		}
		if typErr != nil && s.Let.Value != nil {
			t = types.CheckExprType(s.Let.Value, transpileEnv)
		}
		if it, ok := inferLiteralType(s.Let.Value, transpileEnv); ok {
			t = it
		} else if s.Let.Type != nil {
			t = types.ResolveTypeRef(s.Let.Type, transpileEnv)
		}
		switch tt := t.(type) {
		case types.StructType:
			name := ensureNamedStruct(tt, s.Let.Name)
			tt.Name = name
			t = tt
		case types.ListType:
			if st, ok := tt.Elem.(types.StructType); ok {
				name := ensureNamedStruct(st, strings.TrimSuffix(s.Let.Name, "s"))
				st.Name = name
				tt.Elem = st
				t = tt
			}
		}
		typeStr := tsType(t)
		if transpileEnv != nil {
			transpileEnv.SetVar(s.Let.Name, t, false)
		}
		if q, ok := e.(*QueryExprJS); ok && q.ElemType != "" {
			typeStr = q.ElemType + "[]"
		}
		return &VarDecl{Name: s.Let.Name, Expr: e, Const: !mutable, Type: typeStr}, nil
	case s.Var != nil:
		var e Expr
		var err error
		if s.Var.Value != nil {
			e, err = convertExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if s.Var.Type != nil {
			e = zeroValue(s.Var.Type, transpileEnv)
		}
		// `var` bindings are mutable by definition. We still look up
		// the type from the environment for improved annotations but
		// skip the mutable check to avoid accidentally emitting
		// `const` when scope analysis fails for locals.
		mutable := true
		var t types.Type
		if s.Var.Value != nil {
			t = types.CheckExprType(s.Var.Value, transpileEnv)
		} else if transpileEnv != nil {
			var typErr error
			t, typErr = transpileEnv.GetVar(s.Var.Name)
			if typErr != nil {
				t = nil
			}
		}
		if it, ok := inferLiteralType(s.Var.Value, transpileEnv); ok {
			t = it
		} else if s.Var.Type != nil {
			t = types.ResolveTypeRef(s.Var.Type, transpileEnv)
		}
		switch tt := t.(type) {
		case types.StructType:
			name := ensureNamedStruct(tt, s.Var.Name)
			tt.Name = name
			t = tt
		case types.ListType:
			if st, ok := tt.Elem.(types.StructType); ok {
				name := ensureNamedStruct(st, strings.TrimSuffix(s.Var.Name, "s"))
				st.Name = name
				tt.Elem = st
				t = tt
			}
		}
		typeStr := tsType(t)
		if transpileEnv != nil {
			// If the name matches a function parameter, we cannot re-declare it.
			if len(paramStack) > 0 && paramStack[len(paramStack)-1][s.Var.Name] {
				transpileEnv.SetVarDeep(s.Var.Name, t, true)
				if q, ok := e.(*QueryExprJS); ok && q.ElemType != "" {
					typeStr = q.ElemType + "[]"
				}
				return &AssignStmt{Name: s.Var.Name, Expr: e}, nil
			}
			transpileEnv.SetVar(s.Var.Name, t, true)
			if len(paramStack) > 0 {
				paramStack[len(paramStack)-1][s.Var.Name] = true
			}
		}
		if q, ok := e.(*QueryExprJS); ok && q.ElemType != "" {
			typeStr = q.ElemType + "[]"
		}
		return &VarDecl{Name: s.Var.Name, Expr: e, Const: !mutable, Type: typeStr}, nil
	case s.Assign != nil:
		val, err := convertExpr(s.Assign.Value)
		if err != nil {
			return nil, err
		}
		target := Expr(&NameRef{Name: s.Assign.Name})
		if len(s.Assign.Index) > 0 {
			target, err = applyIndexOps(target, s.Assign.Index)
			if err != nil {
				return nil, err
			}
		}
		for _, f := range s.Assign.Field {
			target = &IndexExpr{Target: target, Index: &StringLit{Value: f.Name}}
		}
		if len(s.Assign.Index) > 0 || len(s.Assign.Field) > 0 {
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		return &AssignStmt{Name: s.Assign.Name, Expr: val}, nil
	case s.Type != nil:
		alias, err := convertTypeDecl(s.Type)
		if err != nil {
			return nil, err
		}
		return alias, nil
	case s.ExternVar != nil:
		return nil, nil
	case s.ExternFun != nil:
		return nil, nil
	case s.ExternObject != nil:
		return nil, nil
	case s.ExternType != nil:
		return nil, nil
	case s.Import != nil:
		alias := s.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(s.Import.Path)
		}
		if s.Import.Lang != nil && *s.Import.Lang == "python" {
			path := strings.Trim(s.Import.Path, "\"")
			switch path {
			case "math":
				if pythonMathAliases != nil {
					pythonMathAliases[alias] = true
				}
				return &VarDecl{Name: alias, Expr: &NameRef{Name: "Math"}, Const: true}, nil
			case "subprocess":
				expr := &RawExpr{Code: `{ getoutput: (cmd: string): string => 'Would run: ' + cmd }`}
				return &VarDecl{Name: alias, Expr: expr, Const: true}, nil
			}
		}
		if s.Import.Lang != nil && *s.Import.Lang == "go" {
			path := strings.Trim(s.Import.Path, "\"")
			switch path {
			case "mochi/runtime/ffi/go/testpkg":
				useSHA256 = true
				expr := &RawExpr{Code: `{ Add: (a:number,b:number)=>a+b,
  Pi: 3.14,
  Answer: 42,
  FifteenPuzzleExample: ()=>'Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd',
  MD5Hex: (s:string)=>{ const h=createHash('md5'); h.update(s); return h.digest('hex'); },
  ECDSAExample: () => ({
    D: '1234567890',
    X: '43162711582587979080031819627904423023685561091192625653251495188141318209988',
    Y: '86807430002474105664458509423764867536342689150582922106807036347047552480521',
    Hash: '0xe6f9ed0d',
    R: '43162711582587979080031819627904423023685561091192625653251495188141318209988',
    S: '94150071556658883365738746782965214584303361499725266605620843043083873122499',
    Valid: true
  })
}`}
				return &VarDecl{Name: alias, Expr: expr, Const: true}, nil
			case "strings":
				expr := &RawExpr{Code: "{ ToUpper: (s:string)=>s.toUpperCase(), TrimSpace: (s:string)=>s.trim() }"}
				return &VarDecl{Name: alias, Expr: expr, Const: true}, nil
			case "net":
				expr := &RawExpr{Code: "{ LookupHost: (host:string)=>_lookupHost(host) }"}
				useLookupHost = true
				useFetch = true
				return &VarDecl{Name: alias, Expr: expr, Const: true}, nil
			case "os":
				expr := &RawExpr{Code: `{
  Getenv: (name: string): string => {
    if (typeof Deno !== 'undefined') {
      return Deno.env.get(name) ?? "";
    }
    if (typeof process !== 'undefined') {
      return process.env[name] || "";
    }
    return "";
  },
  Environ: (): string[] => {
    if (typeof Deno !== 'undefined') {
      try {
        return Object.entries(Deno.env.toObject()).map(([k, v]) => k + "=" + v);
      } catch (_e) {
        return [];
      }
    }
    if (typeof process !== 'undefined') {
      return Object.entries(process.env).map(([k, v]) => k + "=" + v);
    }
    return [];
  }
}`}
				return &VarDecl{Name: alias, Expr: expr, Const: true}, nil
			}
		}
		return nil, nil
	case s.Expr != nil:
		if se := extractSaveExpr(s.Expr.Expr); se != nil {
			src, err := convertExpr(se.Src)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			return &SaveStmt{Src: src, Path: path, Format: format}, nil
		}
		e, err := convertExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Return != nil:
		var e Expr
		if s.Return.Value != nil {
			var err error
			e, err = convertExpr(s.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		if funcDepth == 0 {
			returnOutsideFunc = true
		}
		return &ReturnStmt{Value: e}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Fun != nil:
		child := types.NewEnv(transpileEnv)
		var params []string
		var typesArr []string
		paramMap := map[string]bool{}
		for _, p := range s.Fun.Params {
			params = append(params, p.Name)
			var pt types.Type
			if p.Type != nil {
				pt = types.ResolveTypeRef(p.Type, transpileEnv)
				typesArr = append(typesArr, tsType(pt))
			} else {
				pt = types.AnyType{}
				typesArr = append(typesArr, "")
			}
			child.SetVar(p.Name, pt, true)
			paramMap[p.Name] = true
		}
		prev := transpileEnv
		transpileEnv = child
		paramStack = append(paramStack, paramMap)
		funcDepth++
		body, err := convertStmtList(s.Fun.Body)
		funcDepth--
		transpileEnv = prev
		paramStack = paramStack[:len(paramStack)-1]
		if err != nil {
			return nil, err
		}
		var retType string
		if s.Fun.Return != nil {
			retType = tsType(types.ResolveTypeRef(s.Fun.Return, transpileEnv))
		}
		name := safeName(s.Fun.Name)
		fd := &FuncDecl{Name: name, Params: params, ParamTypes: typesArr, ReturnType: retType, Body: body, Async: useFetch}
		if fd.Async {
			asyncFuncs[name] = true
		}
		return fd, nil
	case s.If != nil:
		return convertIfStmt(s.If, transpileEnv)
	case s.While != nil:
		return convertWhileStmt(s.While, transpileEnv)
	case s.For != nil:
		return convertForStmt(s.For, transpileEnv)
	case s.Bench != nil:
		body, err := convertStmtList(s.Bench.Body)
		if err != nil {
			return nil, err
		}
		useNow = true
		useBench = true
		return &BenchStmt{Name: s.Bench.Name, Body: body, Async: useFetch}, nil
	case s.Update != nil:
		up, err := convertUpdate(s.Update, transpileEnv)
		if err != nil {
			return nil, err
		}
		return up, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIfStmt(i *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(i.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(i.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		elseStmts, err = convertStmtList(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(w *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(w.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(w.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(f *parser.ForStmt, env *types.Env) (Stmt, error) {
	if f.RangeEnd != nil {
		start, err := convertExpr(f.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(f.RangeEnd)
		if err != nil {
			return nil, err
		}
		if env != nil {
			env.SetVar(f.Name, types.IntType{}, true)
		}
		body, err := convertStmtList(f.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: f.Name, Start: start, End: end, Body: body}, nil
	}
	iterable, err := convertExpr(f.Source)
	if err != nil {
		return nil, err
	}
	var elemType types.Type
	keys := false
	if env != nil {
		switch t := types.ExprType(f.Source, env).(type) {
		case types.MapType:
			keys = true
			elemType = t.Key
		case types.StructType:
			// Treat structured maps like objects and iterate over keys
			keys = true
			elemType = types.StringType{}
		case types.ListType:
			elemType = t.Elem
		case types.GroupType:
			elemType = t.Elem
		case types.StringType:
			elemType = types.StringType{}
		}
		if elemType != nil {
			env.SetVar(f.Name, elemType, true)
		}
	}
	body, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	return &ForInStmt{Name: f.Name, Iterable: iterable, Body: body, Keys: keys}, nil
}

func convertUpdate(u *parser.UpdateStmt, env *types.Env) (*UpdateStmt, error) {
	if env == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := env.GetVar(u.Target)
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
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
	}
	prev := transpileEnv
	transpileEnv = child
	var fields []string
	var values []Expr
	for _, item := range u.Set.Items {
		key, ok := isSimpleIdent(item.Key)
		if !ok {
			key, ok = literalString(item.Key)
			if !ok {
				transpileEnv = prev
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(item.Value)
		if err != nil {
			transpileEnv = prev
			return nil, err
		}
		val = substituteFields(val, "item", fieldSet, nil)
		fields = append(fields, key)
		values = append(values, val)
	}
	var cond Expr
	if u.Where != nil {
		cond, err = convertExpr(u.Where)
		if err != nil {
			transpileEnv = prev
			return nil, err
		}
		cond = substituteFields(cond, "item", fieldSet, nil)
	}
	transpileEnv = prev
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func convertTypeDecl(td *parser.TypeDecl) (Stmt, error) {
	if td.Alias != nil {
		typ := tsType(types.ResolveTypeRef(td.Alias, transpileEnv))
		return &TypeAlias{Name: td.Name, Type: typ}, nil
	}
	if len(td.Variants) > 0 {
		parts := make([]string, len(td.Variants))
		for i, v := range td.Variants {
			fields := []string{"tag: \"" + v.Name + "\""}
			for _, f := range v.Fields {
				ft := types.ResolveTypeRef(f.Type, transpileEnv)
				fields = append(fields, fmt.Sprintf("%s: %s", tsKey(f.Name), tsType(ft)))
			}
			parts[i] = "{ " + strings.Join(fields, "; ") + " }"
		}
		typ := strings.Join(parts, " | ")
		return &TypeAlias{Name: td.Name, Type: typ}, nil
	}
	var fields []string
	fieldSet := map[string]bool{}
	methodSet := map[string]bool{}
	for _, m := range td.Members {
		if m.Field != nil {
			ft := types.ResolveTypeRef(m.Field.Type, transpileEnv)
			fields = append(fields, fmt.Sprintf("%s: %s", tsKey(m.Field.Name), tsType(ft)))
			fieldSet[m.Field.Name] = true
		}
		if m.Method != nil {
			methodSet[m.Method.Name] = true
		}
	}
	for _, m := range td.Members {
		if m.Method != nil {
			params := []string{"self"}
			paramTypes := []string{td.Name}
			child := types.NewEnv(transpileEnv)
			st, _ := transpileEnv.GetStruct(td.Name)
			child.SetVar("self", st, true)
			for _, p := range m.Method.Params {
				params = append(params, p.Name)
				var pt types.Type = types.AnyType{}
				if p.Type != nil {
					pt = types.ResolveTypeRef(p.Type, transpileEnv)
					paramTypes = append(paramTypes, tsType(pt))
				} else {
					paramTypes = append(paramTypes, "")
				}
				child.SetVar(p.Name, pt, true)
			}
			prev := transpileEnv
			transpileEnv = child
			funcDepth++
			body, err := convertStmtList(m.Method.Body)
			funcDepth--
			transpileEnv = prev
			if err != nil {
				return nil, err
			}
			for i := range body {
				body[i] = substituteStmtFields(body[i], "self", fieldSet, methodSet)
			}
			var retType string
			if m.Method.Return != nil {
				retType = tsType(types.ResolveTypeRef(m.Method.Return, transpileEnv))
			}
			prelude = append(prelude, &FuncDecl{Name: td.Name + "_" + m.Method.Name, Params: params, ParamTypes: paramTypes, ReturnType: retType, Body: body, Async: useFetch})
		}
	}
	return &InterfaceDecl{Name: td.Name, Fields: fields}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	paramStack = append(paramStack, map[string]bool{})
	defer func() { paramStack = paramStack[:len(paramStack)-1] }()
	var out []Stmt
	for _, s := range list {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	loops := []QueryLoop{{Name: q.Var}}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if nr, ok := src.(*NameRef); ok && transpileEnv != nil {
		if t, err := transpileEnv.GetVar(nr.Name); err == nil {
			if _, ok2 := t.(types.GroupType); ok2 {
				src = &IndexExpr{Target: src, Index: &StringLit{Value: "items"}}
			}
		}
	}
	loops[0].Source = src
	for _, f := range q.Froms {
		src, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		loops = append(loops, QueryLoop{Name: f.Var, Source: src})
	}
	joins := make([]JoinSpec, 0, len(q.Joins))
	for _, j := range q.Joins {
		src, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		on, err := convertExpr(j.On)
		if err != nil {
			return nil, err
		}
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		joins = append(joins, JoinSpec{Name: j.Var, Source: src, Side: side, On: on})
	}

	// build a child environment with loop and join variables for type inference
	prevEnv := transpileEnv
	child := types.NewEnv(transpileEnv)
	elemT := types.ExprType(q.Source, transpileEnv)
	if lt, ok := elemT.(types.ListType); ok {
		elemT = lt.Elem
	} else if gt, ok := elemT.(types.GroupType); ok {
		elemT = gt.Elem
	}
	child.SetVar(q.Var, elemT, true)
	for i, f := range q.Froms {
		t := types.ExprType(f.Src, transpileEnv)
		if lt, ok := t.(types.ListType); ok {
			t = lt.Elem
		} else if gt, ok := t.(types.GroupType); ok {
			t = gt.Elem
		}
		if i+1 < len(loops) {
			child.SetVar(f.Var, t, true)
		}
	}
	for _, j := range q.Joins {
		t := types.ExprType(j.Src, transpileEnv)
		if lt, ok := t.(types.ListType); ok {
			t = lt.Elem
		} else if gt, ok := t.(types.GroupType); ok {
			t = gt.Elem
		}
		child.SetVar(j.Var, t, true)
	}
	transpileEnv = child
	defer func() { transpileEnv = prevEnv }()
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}
	var sort Expr
	if q.Sort != nil && q.Group == nil {
		sort, err = convertExpr(q.Sort)
		if err != nil {
			return nil, err
		}
	}
	var skip Expr
	if q.Skip != nil {
		skip, err = convertExpr(q.Skip)
		if err != nil {
			return nil, err
		}
	}
	var take Expr
	if q.Take != nil {
		take, err = convertExpr(q.Take)
		if err != nil {
			return nil, err
		}
	}
	selEnv := transpileEnv
	sel, err := convertExpr(q.Select)
	if err != nil {
		return nil, err
	}
	elemType := ""
	if ml := mapLiteral(q.Select); ml != nil && selEnv != nil {
		if st, ok := types.InferStructFromMapEnv(ml, selEnv); ok {
			name := ensureNamedStruct(st, "Result")
			st.Name = name
			elemType = name
		}
	}

	if q.Group != nil && len(q.Group.Exprs) == 1 && (len(q.Froms) != 0 || len(q.Joins) != 0 || q.Skip != nil || q.Take != nil) {
		key, err := convertExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		names := []string{q.Var}
		for _, f := range q.Froms {
			names = append(names, f.Var)
		}
		for _, j := range q.Joins {
			names = append(names, j.Var)
		}
		entries := make([]MapEntry, len(names))
		for i, n := range names {
			entries[i] = MapEntry{Key: &StringLit{Value: n}, Value: &NameRef{Name: n}}
		}
		row := &MapLit{Entries: entries}
		prev := transpileEnv
		child := types.NewEnv(transpileEnv)
		child.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
		transpileEnv = child
		if q.Sort != nil {
			sort, err = convertExpr(q.Sort)
			if err != nil {
				transpileEnv = prev
				return nil, err
			}
		}
		sel, err = convertExpr(q.Select)
		if err != nil {
			transpileEnv = prev
			return nil, err
		}
		var having Expr
		if q.Group.Having != nil {
			having, err = convertExpr(q.Group.Having)
			if err != nil {
				transpileEnv = prev
				return nil, err
			}
		}
		elemType = ""
		if ml := mapLiteral(q.Select); ml != nil {
			if st, ok := types.InferStructFromMapEnv(ml, transpileEnv); ok {
				name := ensureNamedStruct(st, "Result")
				st.Name = name
				elemType = name
			}
		}
		transpileEnv = prev
		return &GroupJoinQueryExpr{Loops: loops, Joins: joins, Where: where, Key: key, Row: row, GroupVar: q.Group.Name, Select: sel, Having: having, Sort: sort, ElemType: elemType}, nil
	}

	if q.Group != nil && len(q.Group.Exprs) == 1 && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Skip == nil && q.Take == nil {
		key, err := convertExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		prev := transpileEnv
		elemT := types.ExprType(q.Source, transpileEnv)
		if lt, ok := elemT.(types.ListType); ok {
			elemT = lt.Elem
		}
		child := types.NewEnv(transpileEnv)
		child.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: elemT}, true)
		transpileEnv = child
		if q.Sort != nil {
			sort, err = convertExpr(q.Sort)
			if err != nil {
				transpileEnv = prev
				return nil, err
			}
		}
		sel, err = convertExpr(q.Select)
		if err != nil {
			transpileEnv = prev
			return nil, err
		}
		var having Expr
		if q.Group.Having != nil {
			having, err = convertExpr(q.Group.Having)
			if err != nil {
				transpileEnv = prev
				return nil, err
			}
		}
		elemType = ""
		if ml := mapLiteral(q.Select); ml != nil {
			if st, ok := types.InferStructFromMapEnv(ml, transpileEnv); ok {
				name := ensureNamedStruct(st, "Result")
				st.Name = name
				elemType = name
			}
		}
		transpileEnv = prev
		return &GroupQueryExpr{Var: q.Var, Source: src, Key: key, Row: &NameRef{Name: q.Var}, GroupVar: q.Group.Name, Cond: where, Select: sel, Having: having, Sort: sort, ElemType: elemType}, nil
	}
	if q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && len(loops) == 1 {
		if s, ok := sel.(*SumExpr); ok {
			if n, ok2 := s.Value.(*NameRef); ok2 && n.Name == q.Var {
				return &AggQueryExpr{Var: loops[0].Name, Source: loops[0].Source, Where: where, Op: "sum"}, nil
			}
		} else if a, ok := sel.(*AvgExpr); ok {
			if n, ok2 := a.Value.(*NameRef); ok2 && n.Name == q.Var {
				return &AggQueryExpr{Var: loops[0].Name, Source: loops[0].Source, Where: where, Op: "avg"}, nil
			}
		} else if m, ok := sel.(*MinExpr); ok {
			if n, ok2 := m.Value.(*NameRef); ok2 && n.Name == q.Var {
				return &AggQueryExpr{Var: loops[0].Name, Source: loops[0].Source, Where: where, Op: "min"}, nil
			}
		} else if m, ok := sel.(*MaxExpr); ok {
			if n, ok2 := m.Value.(*NameRef); ok2 && n.Name == q.Var {
				return &AggQueryExpr{Var: loops[0].Name, Source: loops[0].Source, Where: where, Op: "max"}, nil
			}
		}
	}
	return &QueryExprJS{Loops: loops, Joins: joins, Where: where, Sort: sort, Skip: skip, Take: take, Select: sel, ElemType: elemType}, nil
}

func applyIndexOps(base Expr, ops []*parser.IndexOp) (Expr, error) {
	var err error
	for _, op := range ops {
		if op.Colon != nil {
			if op.Colon2 != nil || op.Step != nil {
				return nil, fmt.Errorf("slice step not supported")
			}
			var start, end Expr
			if op.Start != nil {
				start, err = convertExpr(op.Start)
				if err != nil {
					return nil, err
				}
			}
			if op.End != nil {
				end, err = convertExpr(op.End)
				if err != nil {
					return nil, err
				}
			}
			base = &SliceExpr{Target: base, Start: start, End: end}
		} else {
			if op.Colon2 != nil || op.End != nil || op.Step != nil {
				return nil, fmt.Errorf("slice not supported")
			}
			if op.Start == nil {
				return nil, fmt.Errorf("nil index")
			}
			var idx Expr
			idx, err = convertExpr(op.Start)
			if err != nil {
				return nil, err
			}
			base = &IndexExpr{Target: base, Index: idx}
		}
	}
	return base, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func postfixExprType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}
	if transpileEnv == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, transpileEnv)
}

func isMapExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if sel := p.Target.Selector; sel != nil && transpileEnv != nil {
		if t, err := transpileEnv.GetVar(sel.Root); err == nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
	}
	if transpileEnv != nil {
		t := types.CheckExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}, transpileEnv)
		if _, ok := t.(types.MapType); ok {
			return true
		}
	}
	return false
}

func isMapIntKey(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil || transpileEnv == nil {
		return false
	}
	t := types.CheckExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}, transpileEnv)
	if mt, ok := t.(types.MapType); ok {
		return isIntType(mt.Key)
	}
	return false
}

func isStructExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil || transpileEnv == nil {
		return false
	}
	t := types.CheckExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}, transpileEnv)
	_, ok := t.(types.StructType)
	return ok
}

func isIntType(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return true
	}
	return false
}

func isIntLitExpr(e Expr) bool {
	if n, ok := e.(*NumberLit); ok {
		return !strings.Contains(n.Value, ".")
	}
	return false
}

func isFloatLitExpr(e Expr) bool {
	if n, ok := e.(*NumberLit); ok {
		return strings.Contains(n.Value, ".")
	}
	return false
}

func isVarIntExpr(e Expr) bool {
	if nr, ok := e.(*NameRef); ok && transpileEnv != nil {
		if t, err := transpileEnv.GetVar(nr.Name); err == nil {
			return isIntType(t)
		}
	}
	return false
}

func isBigIntExpr(e Expr) bool {
	switch v := e.(type) {
	case *CallExpr:
		return v.Func == "BigInt"
	case *NumberLit:
		return strings.HasSuffix(v.Value, "n")
	case *IntDivExpr:
		return v.Big
	}
	return false
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	operands := []Expr{}
	ops := []string{}
	opnodes := []*parser.BinaryOp{}
	typesArr := []types.Type{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	if transpileEnv != nil {
		t := types.CheckExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}, transpileEnv)
		typesArr = append(typesArr, t)
	} else {
		typesArr = append(typesArr, types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}, nil))
	}
	for _, r := range b.Right {
		o, err := convertPostfix(r.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, o)
		ops = append(ops, r.Op)
		opnodes = append(opnodes, r)
		if transpileEnv != nil {
			typesArr = append(typesArr, types.CheckExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: r.Right}}}, transpileEnv))
		} else {
			typesArr = append(typesArr, types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: r.Right}}}, nil))
		}
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "except", "intersect"},
	}

	apply := func(i int) {
		if isBigIntExpr(operands[i]) || isBigIntExpr(operands[i+1]) {
			if !isBigIntExpr(operands[i]) {
				operands[i] = &CallExpr{Func: "BigInt", Args: []Expr{operands[i]}}
			}
			if !isBigIntExpr(operands[i+1]) {
				operands[i+1] = &CallExpr{Func: "BigInt", Args: []Expr{operands[i+1]}}
			}
			typesArr[i] = types.BigIntType{}
			typesArr[i+1] = types.BigIntType{}
		}
		switch ops[i] {
		case "+":
			leftList := isListType(typesArr[i])
			rightList := isListType(typesArr[i+1])
			if !leftList {
				if nr, ok := operands[i].(*NameRef); ok && transpileEnv != nil {
					if t, err := transpileEnv.GetVar(nr.Name); err == nil {
						leftList = isListType(t)
					}
				}
			}
			if !rightList {
				if nr, ok := operands[i+1].(*NameRef); ok && transpileEnv != nil {
					if t, err := transpileEnv.GetVar(nr.Name); err == nil {
						rightList = isListType(t)
					}
				} else if _, ok := operands[i+1].(*ListLit); ok {
					rightList = true
				}
			}
			if leftList && rightList {
				if oLit, ok := operands[i+1].(*ListLit); ok && len(oLit.Elems) == 1 {
					operands[i] = &AppendExpr{List: operands[i], Elem: oLit.Elems[0]}
				} else {
					operands[i] = &UnionAllExpr{Left: operands[i], Right: operands[i+1]}
				}
				break
			}
			if isBigIntType(typesArr[i]) || isBigIntType(typesArr[i+1]) {
				if !isBigIntType(typesArr[i]) {
					operands[i] = &CallExpr{Func: "BigInt", Args: []Expr{operands[i]}}
					typesArr[i] = types.BigIntType{}
				}
				if !isBigIntType(typesArr[i+1]) {
					operands[i+1] = &CallExpr{Func: "BigInt", Args: []Expr{operands[i+1]}}
					typesArr[i+1] = types.BigIntType{}
				}
				operands[i] = &BinaryExpr{Left: operands[i], Op: ops[i], Right: operands[i+1]}
				typesArr[i] = types.BigIntType{}
			} else {
				operands[i] = &BinaryExpr{Left: operands[i], Op: ops[i], Right: operands[i+1]}
				if isIntType(typesArr[i]) && isIntType(typesArr[i+1]) {
					typesArr[i] = types.IntType{}
				} else {
					typesArr[i] = types.FloatType{}
				}
			}
			break
		case "in":
			isMap := false
			if typ := postfixExprType(opnodes[i].Right); typ != nil {
				switch typ.(type) {
				case types.MapType, types.StructType:
					isMap = true
				}
			}
			if !isMap && transpileEnv != nil {
				if id := opnodes[i].Right.Target.Selector; id != nil {
					if t, err := transpileEnv.GetVar(id.Root); err == nil {
						if _, ok := t.(types.MapType); ok {
							isMap = true
						}
					}
				}
			}
			if !isMap && isMapExpr(opnodes[i].Right) {
				isMap = true
			}
			if isMap {
				operands[i] = &BinaryExpr{Left: operands[i], Op: "in", Right: operands[i+1]}
			} else {
				operands[i] = &MethodCallExpr{Target: operands[i+1], Method: "includes", Args: []Expr{operands[i]}}
			}
		case "union":
			if opnodes[i].All {
				operands[i] = &UnionAllExpr{Left: operands[i], Right: operands[i+1]}
			} else {
				operands[i] = &UnionExpr{Left: operands[i], Right: operands[i+1]}
			}
		case "except":
			operands[i] = &ExceptExpr{Left: operands[i], Right: operands[i+1]}
		case "intersect":
			operands[i] = &IntersectExpr{Left: operands[i], Right: operands[i+1]}
		default:
			if isBigIntType(typesArr[i]) || isBigIntType(typesArr[i+1]) {
				if !isBigIntType(typesArr[i]) {
					operands[i] = &CallExpr{Func: "BigInt", Args: []Expr{operands[i]}}
					typesArr[i] = types.BigIntType{}
				}
				if !isBigIntType(typesArr[i+1]) {
					operands[i+1] = &CallExpr{Func: "BigInt", Args: []Expr{operands[i+1]}}
					typesArr[i+1] = types.BigIntType{}
				}
			} else if ops[i] == "%" && isIntType(typesArr[i]) && isIntType(typesArr[i+1]) && containsLargeIntMul(operands[i]) {
				operands[i] = toBigIntExpr(operands[i])
				operands[i+1] = toBigIntExpr(operands[i+1])
				expr := &BinaryExpr{Left: operands[i], Op: "%", Right: operands[i+1]}
				operands[i] = &CallExpr{Func: "Number", Args: []Expr{expr}}
				typesArr[i] = types.IntType{}
			} else if ops[i] == "/" && (isIntType(typesArr[i]) && isIntType(typesArr[i+1]) || (isVarIntExpr(operands[i]) && isIntLitExpr(operands[i+1])) || (isIntLitExpr(operands[i]) && isIntLitExpr(operands[i+1]))) && !(isFloatLitExpr(operands[i]) || isFloatLitExpr(operands[i+1])) {
				if isBigIntType(typesArr[i]) || isBigIntType(typesArr[i+1]) {
					operands[i] = &IntDivExpr{Left: operands[i], Right: operands[i+1], Big: true}
					typesArr[i] = types.BigIntType{}
				} else {
					operands[i] = &IntDivExpr{Left: operands[i], Right: operands[i+1]}
					typesArr[i] = types.IntType{}
				}
			} else {
				operands[i] = &BinaryExpr{Left: operands[i], Op: ops[i], Right: operands[i+1]}
				switch ops[i] {
				case "+", "-", "*", "%":
					if isIntType(typesArr[i]) && isIntType(typesArr[i+1]) {
						typesArr[i] = types.IntType{}
					} else {
						typesArr[i] = types.FloatType{}
					}
				case "/":
					typesArr[i] = types.FloatType{}
				case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
					typesArr[i] = types.BoolType{}
				default:
					typesArr[i] = typesArr[i]
				}
			}
		}
		operands = append(operands[:i+1], operands[i+2:]...)
		ops = append(ops[:i], ops[i+1:]...)
		opnodes = append(opnodes[:i], opnodes[i+1:]...)
		typesArr = append(typesArr[:i+1], typesArr[i+2:]...)
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range lvl {
				if ops[i] == t {
					apply(i)
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

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
	}
	return expr, nil
}

func convertPostfix(p *parser.PostfixExpr) (expr Expr, err error) {
	if p == nil || p.Target == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	defer func() {
		if r := recover(); r != nil {
			expr = nil
			err = fmt.Errorf("convertPostfix panic")
		}
	}()
	expr, err = convertPrimary(p.Target)
	if err != nil {
		return nil, err
	}
	partial := &parser.PostfixExpr{Target: p.Target}
	curType := postfixExprType(partial)
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Field != nil && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil:
			if st, ok := curType.(types.StructType); ok {
				if _, ok2 := st.Methods[op.Field.Name]; ok2 {
					callOp := p.Ops[i+1].Call
					args := make([]Expr, len(callOp.Args)+1)
					args[0] = expr
					for j, a := range callOp.Args {
						ae, err := convertExpr(a)
						if err != nil {
							return nil, err
						}
						args[j+1] = ae
					}
					fname := st.Name + "_" + op.Field.Name
					expr = &CallExpr{Func: fname, Args: args}
					partial.Ops = append(partial.Ops, op, p.Ops[i+1])
					curType = postfixExprType(partial)
					i++
					continue
				}
			}
			if nr, ok := expr.(*NameRef); ok && nr.Name == "stdout" && op.Field.Name == "write" {
				callOp := p.Ops[i+1].Call
				args := make([]Expr, len(callOp.Args))
				for j, a := range callOp.Args {
					ae, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args[j] = ae
				}
				expr = &CallExpr{Func: "_stdout_write", Args: args}
				useStdout = true
				partial.Ops = append(partial.Ops, op, p.Ops[i+1])
				curType = postfixExprType(partial)
				i++
				continue
			}
			// treat as property access on non-struct value
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}}
			partial.Ops = append(partial.Ops, op)
			curType = postfixExprType(partial)
			continue
		case op.Index != nil:
			if op.Index.Colon != nil {
				if op.Index.Colon2 != nil || op.Index.Step != nil {
					return nil, fmt.Errorf("slice step not supported")
				}
				var start, end Expr
				if op.Index.Start != nil {
					start, err = convertExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					end, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				expr = &SliceExpr{Target: expr, Start: start, End: end}
			} else {
				if op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
					return nil, fmt.Errorf("postfix slice not supported")
				}
				if op.Index.Start == nil {
					return nil, fmt.Errorf("nil index")
				}
				idxExpr := op.Index.Start
				idx, err := convertExpr(idxExpr)
				if err != nil {
					return nil, err
				}
				if keyStr, isStr := literalString(idxExpr); !isStr {
					if isMapIntKey(partial) || (!isMapExpr(partial) && !isStructExpr(partial)) {
						if be, ok := idx.(*BinaryExpr); ok && be.Op == "*" && isMapIntKey(partial) {
							be.Left = &CallExpr{Func: "Math.trunc", Args: []Expr{be.Left}}
							idx = be
						} else {
							idx = &CallExpr{Func: "Math.trunc", Args: []Expr{idx}}
						}
					}
				} else {
					_ = keyStr // avoid unused variable warnings
				}
				expr = &IndexExpr{Target: expr, Index: idx}
			}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ae, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = ae
			}
			if idx, ok := expr.(*IndexExpr); ok {
				if lit, ok2 := idx.Index.(*StringLit); ok2 {
					switch {
					case lit.Value == "contains" && len(args) == 1:
						expr = &MethodCallExpr{Target: idx.Target, Method: "includes", Args: args}
					case lit.Value == "keys" && len(args) == 0:
						expr = &MapKeysExpr{Target: idx.Target}
					case lit.Value == "get":
						expr = &MethodCallExpr{Target: idx.Target, Method: "get", Args: args}
					case lit.Value == "padStart" && len(args) == 2:
						expr = &MethodCallExpr{Target: &CallExpr{Func: "String", Args: []Expr{idx.Target}}, Method: "padStart", Args: args}
					case lit.Value == "write":
						expr = &CallExpr{Func: "_stdout_write", Args: args}
						useStdout = true
					case lit.Value == "LookupHost" && len(args) == 1:
						if nr, ok3 := idx.Target.(*NameRef); ok3 && nr.Name == "net" {
							useLookupHost = true
							useFetch = true
							expr = &AwaitExpr{X: &CallExpr{Func: "_lookupHost", Args: args}}
						} else {
							expr = &InvokeExpr{Callee: expr, Args: args}
						}
					default:
						expr = &InvokeExpr{Callee: expr, Args: args}
					}
				} else {
					expr = &InvokeExpr{Callee: expr, Args: args}
				}
			} else {
				expr = &InvokeExpr{Callee: expr, Args: args}
			}
			partial.Ops = append(partial.Ops, op)
			curType = postfixExprType(partial)
		case op.Field != nil:
			if nr, ok := expr.(*NameRef); ok && pythonMathAliases != nil && pythonMathAliases[nr.Name] {
				up := strings.ToUpper(op.Field.Name)
				expr = &IndexExpr{Target: expr, Index: &StringLit{Value: up}}
			} else {
				expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}}
			}
			partial.Ops = append(partial.Ops, op)
			curType = postfixExprType(partial)
		case op.Cast != nil:
			if op.Cast.Type != nil && transpileEnv != nil {
				t := types.ResolveTypeRef(op.Cast.Type, transpileEnv)
				switch t.(type) {
				case types.IntType, types.Int64Type:
					if curType != nil && isBigIntType(curType) {
						expr = &CallExpr{Func: "Number", Args: []Expr{expr}}
						break
					}
					switch e := expr.(type) {
					case *SliceExpr, *MethodCallExpr, *SubstringExpr:
						useParseIntStr = true
						expr = &CallExpr{Func: "parseIntStr", Args: []Expr{expr, &NumberLit{Value: "10"}}}
					case *IndexExpr:
						if ct := curType; ct != nil {
							if _, ok := ct.(types.StringType); ok {
								useParseIntStr = true
								expr = &CallExpr{Func: "parseIntStr", Args: []Expr{e, &NumberLit{Value: "10"}}}
							} else {
								expr = &CallExpr{Func: "Math.trunc", Args: []Expr{e}}
							}
						} else {
							expr = &CallExpr{Func: "Math.trunc", Args: []Expr{e}}
						}
					default:
						expr = &CallExpr{Func: "Math.trunc", Args: []Expr{expr}}
					}
				case types.BigIntType:
					switch e := expr.(type) {
					case *NumberLit:
						if !strings.HasSuffix(e.Value, "n") {
							e.Value = strings.TrimSuffix(e.Value, ".0") + "n"
						}
						expr = e
					default:
						expr = &CallExpr{Func: "BigInt", Args: []Expr{expr}}
					}
				case types.BigRatType:
					expr = &CallExpr{Func: "Number", Args: []Expr{expr}}
				default:
					// other casts are no-ops in JavaScript
				}
			}
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Struct != nil:
		entries := make([]MapEntry, 0, len(p.Struct.Fields)+1)
		if transpileEnv != nil {
			if ut, ok := transpileEnv.FindUnionByVariant(p.Struct.Name); ok {
				entries = append(entries, MapEntry{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: p.Struct.Name}})
				st := ut.Variants[p.Struct.Name]
				for i, f := range p.Struct.Fields {
					val, err := convertExpr(f.Value)
					if err != nil {
						return nil, err
					}
					entries = append(entries, MapEntry{Key: &StringLit{Value: st.Order[i]}, Value: val})
				}
				return &MapLit{Entries: entries}, nil
			}
		}
		for _, f := range p.Struct.Fields {
			val, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			entries = append(entries, MapEntry{Key: &StringLit{Value: f.Name}, Value: val})
		}
		if transpileEnv != nil {
			if st, ok := transpileEnv.GetStruct(p.Struct.Name); ok {
				for name := range st.Methods {
					code := fmt.Sprintf("function(...args){ return %s(this, ...args); }", st.Name+"_"+name)
					entries = append(entries, MapEntry{Key: &StringLit{Value: name}, Value: &RawExpr{Code: code}})
				}
			}
		}
		return &MapLit{Entries: entries}, nil
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 && p.Selector.Root == "nil" {
			return &NullLit{}, nil
		}
		if transpileEnv != nil && len(p.Selector.Tail) == 0 {
			if u, ok := transpileEnv.FindUnionByVariant(p.Selector.Root); ok {
				v := u.Variants[p.Selector.Root]
				if len(v.Order) == 0 {
					return &MapLit{Entries: []MapEntry{
						{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: p.Selector.Root}},
					}}, nil
				}
			}
		}
		expr := selectorToExpr(p.Selector)
		return expr, nil
	case p.Call != nil:
		if transpileEnv != nil {
			if u, ok := transpileEnv.FindUnionByVariant(p.Call.Func); ok {
				v := u.Variants[p.Call.Func]
				if len(v.Order) == len(p.Call.Args) {
					entries := []MapEntry{{Key: &StringLit{Value: "tag"}, Value: &StringLit{Value: p.Call.Func}}}
					for i, name := range v.Order {
						ae, err := convertExpr(p.Call.Args[i])
						if err != nil {
							return nil, err
						}
						entries = append(entries, MapEntry{Key: &StringLit{Value: name}, Value: ae})
					}
					return &MapLit{Entries: entries}, nil
				}
			}
		}
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ae
		}
		switch p.Call.Func {
		case "print":
			if transpileEnv != nil {
				for i, a := range args {
					if isListType(types.ExprType(p.Call.Args[i], transpileEnv)) {
						args[i] = &FormatListExpr{Value: a}
					}
				}
			}
			if len(args) > 0 {
				if b, ok := args[len(args)-1].(*BoolLit); ok && !b.Value {
					useStdout = true
				}
			}
			useStr = true
			return &PrintExpr{Args: args}, nil
		case "len":
			if len(args) != 1 {
				return nil, fmt.Errorf("len expects one argument")
			}
			if transpileEnv != nil {
				if _, ok := types.ExprType(p.Call.Args[0], transpileEnv).(types.GroupType); ok {
					args[0] = &IndexExpr{Target: args[0], Index: &StringLit{Value: "items"}}
				}
			}
			useLen = true
			return &LenExpr{Value: args[0]}, nil
		case "append":
			if len(args) != 2 {
				return nil, fmt.Errorf("append expects two arguments")
			}
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		case "avg":
			if len(args) != 1 {
				return nil, fmt.Errorf("avg expects one argument")
			}
			return &AvgExpr{Value: args[0]}, nil
		case "count":
			if len(args) != 1 {
				return nil, fmt.Errorf("count expects one argument")
			}
			if transpileEnv != nil {
				if _, ok := types.ExprType(p.Call.Args[0], transpileEnv).(types.GroupType); ok {
					args[0] = &IndexExpr{Target: args[0], Index: &StringLit{Value: "items"}}
				}
			}
			useLen = true
			return &LenExpr{Value: args[0]}, nil
		case "sum":
			if len(args) != 1 {
				return nil, fmt.Errorf("sum expects one argument")
			}
			return &SumExpr{Value: args[0]}, nil
		case "min":
			if len(args) != 1 {
				return nil, fmt.Errorf("min expects one argument")
			}
			return &MinExpr{Value: args[0]}, nil
		case "max":
			if len(args) != 1 {
				return nil, fmt.Errorf("max expects one argument")
			}
			return &MaxExpr{Value: args[0]}, nil
		case "exists":
			if len(args) != 1 {
				return nil, fmt.Errorf("exists expects one argument")
			}
			return &BinaryExpr{Left: &LenExpr{Value: args[0]}, Op: ">", Right: &NumberLit{Value: "0"}}, nil
		case "values":
			if len(args) != 1 {
				return nil, fmt.Errorf("values expects one argument")
			}
			return &ValuesExpr{Value: args[0]}, nil
		case "now":
			if len(args) != 0 {
				return nil, fmt.Errorf("now expects no arguments")
			}
			useNow = true
			return &NowExpr{}, nil
		case "input":
			if len(args) != 0 {
				return nil, fmt.Errorf("input expects no arguments")
			}
			useInput = true
			return &CallExpr{Func: "_input"}, nil
		case "keys":
			if len(args) != 1 {
				return nil, fmt.Errorf("keys expects one argument")
			}
			useKeys = true
			return &CallExpr{Func: "_keys", Args: args}, nil
		case "json":
			if len(args) != 1 {
				return nil, fmt.Errorf("json expects one argument")
			}
			pretty := &CallExpr{Func: "JSON.stringify", Args: []Expr{args[0], &NullLit{}, &NumberLit{Value: "2"}}}
			return &CallExpr{Func: "console.log", Args: []Expr{pretty}}, nil
		case "str":
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects one argument")
			}
			useStr = true
			return &CallExpr{Func: "_str", Args: args}, nil
		case "int":
			if len(args) != 1 {
				return nil, fmt.Errorf("int expects one argument")
			}
			// Math.trunc mirrors Mochi's int() which drops the
			// fractional part without rounding.
			return &CallExpr{Func: "Math.trunc", Args: args}, nil
		case "float":
			if len(args) != 1 {
				return nil, fmt.Errorf("float expects one argument")
			}
			return &CallExpr{Func: "Number", Args: args}, nil
		case "split":
			if transpileEnv != nil {
				if _, ok := transpileEnv.GetFunc("split"); ok {
					return &CallExpr{Func: "split", Args: args}, nil
				}
			}
			if len(args) != 2 {
				return nil, fmt.Errorf("split expects two arguments")
			}
			return &MethodCallExpr{Target: args[0], Method: "split", Args: []Expr{args[1]}}, nil
		case "parseIntStr":
			if transpileEnv != nil {
				if _, ok := transpileEnv.GetFunc("parseIntStr"); ok {
					return &CallExpr{Func: "parseIntStr", Args: args}, nil
				}
			}
			switch len(args) {
			case 1:
				useParseIntStr = true
				return &CallExpr{Func: "parseIntStr", Args: []Expr{args[0], &NumberLit{Value: "10"}}}, nil
			case 2:
				useParseIntStr = true
				return &CallExpr{Func: "parseIntStr", Args: []Expr{args[0], args[1]}}, nil
			default:
				return nil, fmt.Errorf("parseIntStr expects one or two arguments")
			}
		case "substring":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects three arguments")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		case "indexOf":
			if len(args) < 2 || len(args) > 3 {
				return nil, fmt.Errorf("indexOf expects two or three arguments")
			}
			mc := &MethodCallExpr{Target: args[0], Method: "indexOf"}
			mc.Args = append(mc.Args, args[1])
			if len(args) == 3 {
				mc.Args = append(mc.Args, args[2])
			}
			return mc, nil
		case "repeat":
			if transpileEnv != nil {
				if _, ok := transpileEnv.GetFunc("repeat"); ok {
					return &CallExpr{Func: "repeat", Args: args}, nil
				}
			}
			if len(args) != 2 {
				return nil, fmt.Errorf("repeat expects two arguments")
			}
			useRepeat = true
			return &CallExpr{Func: "repeat", Args: []Expr{args[0], args[1]}}, nil
		case "concat":
			if transpileEnv != nil {
				if _, ok := transpileEnv.GetFunc("concat"); ok {
					return &CallExpr{Func: "concat", Args: args}, nil
				}
			}
			if len(args) != 2 {
				return nil, fmt.Errorf("concat expects two arguments")
			}
			useConcat = true
			return &CallExpr{Func: "concat", Args: []Expr{args[0], args[1]}}, nil
		case "pow":
			if len(args) != 2 {
				return nil, fmt.Errorf("pow expects two arguments")
			}
			if transpileEnv != nil {
				if _, ok := transpileEnv.GetFunc("pow"); ok {
					return &CallExpr{Func: "pow", Args: args}, nil
				}
			}
			return &CallExpr{Func: "Math.pow", Args: args}, nil
		case "substr":
			if len(args) != 3 {
				return nil, fmt.Errorf("substr expects three arguments")
			}
			return &MethodCallExpr{Target: args[0], Method: "substring", Args: []Expr{args[1], args[2]}}, nil
		case "slice":
			if len(args) != 3 {
				return nil, fmt.Errorf("slice expects three arguments")
			}
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2]}, nil
		case "upper":
			if len(args) != 1 {
				return nil, fmt.Errorf("upper expects one argument")
			}
			return &MethodCallExpr{Target: args[0], Method: "toUpperCase", Args: nil}, nil
		case "lower":
			if len(args) != 1 {
				return nil, fmt.Errorf("lower expects one argument")
			}
			return &MethodCallExpr{Target: args[0], Method: "toLowerCase", Args: nil}, nil
		case "contains":
			if transpileEnv != nil {
				if _, ok := transpileEnv.GetFunc("contains"); ok {
					return &CallExpr{Func: "contains", Args: args}, nil
				}
			}
			if len(args) != 2 {
				return nil, fmt.Errorf("contains expects two arguments")
			}
			if transpileEnv != nil {
				if _, ok := types.ExprType(p.Call.Args[0], transpileEnv).(types.MapType); ok {
					useHas = true
					return &CallExpr{Func: "_has", Args: []Expr{args[0], args[1]}}, nil
				}
			}
			return &MethodCallExpr{Target: args[0], Method: "includes", Args: []Expr{args[1]}}, nil
		case "padStart":
			if len(args) != 3 {
				return nil, fmt.Errorf("padStart expects three arguments")
			}
			target := &CallExpr{Func: "String", Args: []Expr{args[0]}}
			return &MethodCallExpr{Target: target, Method: "padStart", Args: []Expr{args[1], args[2]}}, nil
		case "sha256":
			if len(args) != 1 {
				return nil, fmt.Errorf("sha256 expects one argument")
			}
			useSHA256 = true
			return &CallExpr{Func: "sha256", Args: args}, nil
		case "num":
			if len(args) != 1 {
				return nil, fmt.Errorf("num expects one argument")
			}
			useNumDenom = true
			return &CallExpr{Func: "num", Args: args}, nil
		case "denom":
			if len(args) != 1 {
				return nil, fmt.Errorf("denom expects one argument")
			}
			useNumDenom = true
			return &CallExpr{Func: "denom", Args: args}, nil
		case "first":
			if len(args) != 1 {
				return nil, fmt.Errorf("first expects one argument")
			}
			return &IndexExpr{Target: args[0], Index: &NumberLit{Value: "0"}}, nil
		case "error":
			if len(args) != 1 {
				return nil, fmt.Errorf("error expects one argument")
			}
			usePanic = true
			return &CallExpr{Func: "_panic", Args: args}, nil
		case "panic":
			if _, ok := transpileEnv.GetFunc("panic"); ok {
				// user-defined panic function; treat like normal call
				return &CallExpr{Func: "panic", Args: args}, nil
			}
			if len(args) != 1 {
				return nil, fmt.Errorf("panic expects one argument")
			}
			usePanic = true
			return &CallExpr{Func: "_panic", Args: args}, nil
		default:
			if fn, ok := transpileEnv.GetFunc(p.Call.Func); ok {
				if len(args) < len(fn.Params) {
					missing := fn.Params[len(args):]
					paramNames := make([]string, len(missing))
					callArgs := append([]Expr{}, args...)
					for i, pa := range missing {
						paramNames[i] = pa.Name
						callArgs = append(callArgs, &NameRef{Name: pa.Name})
					}
					return &FunExpr{
						Params: paramNames,
						Expr:   &CallExpr{Func: p.Call.Func, Args: callArgs},
					}, nil
				}
			}
			if asyncFuncs[p.Call.Func] {
				return &AwaitExpr{X: &CallExpr{Func: p.Call.Func, Args: args}}, nil
			}
			return &CallExpr{Func: p.Call.Func, Args: args}, nil
		}
	case p.If != nil:
		cond, err := convertExpr(p.If.Cond)
		if err != nil {
			return nil, err
		}
		thenExpr, err := convertExpr(p.If.Then)
		if err != nil {
			return nil, err
		}
		var elseExpr Expr
		if p.If.ElseIf != nil {
			elseExpr, err = convertPrimary(&parser.Primary{If: p.If.ElseIf})
			if err != nil {
				return nil, err
			}
		} else if p.If.Else != nil {
			elseExpr, err = convertExpr(p.If.Else)
			if err != nil {
				return nil, err
			}
		}
		return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		entries := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: k, Value: v}
		}
		// Avoid generating unused type declarations for simple map
		// literals. Struct interfaces are emitted when a variable or
		// query explicitly requires one, so map literals on their own
		// can remain anonymous objects.
		return &MapLit{Entries: entries}, nil
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		useFetch = true
		if p.Fetch.With != nil {
			withExpr, err := convertExpr(p.Fetch.With)
			if err != nil {
				return nil, err
			}
			return &AwaitExpr{X: &CallExpr{Func: "_fetch", Args: []Expr{urlExpr, withExpr}}}, nil
		}
		return &AwaitExpr{X: &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}}, nil
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		pathExpr := fmt.Sprintf("%q", path)
		clean := path
		for strings.HasPrefix(clean, "../") {
			clean = strings.TrimPrefix(clean, "../")
		}
		if path != "" && strings.HasPrefix(path, "../") {
			pathExpr = fmt.Sprintf("new URL(\"../../../%s\", import.meta.url).pathname", clean)
		} else if path != "" && !strings.HasPrefix(path, "/") {
			pathExpr = fmt.Sprintf("new URL('../../../../..', import.meta.url).pathname + %q", path)
		}
		switch format {
		case "json":
			return &RawExpr{Code: fmt.Sprintf("JSON.parse(Deno.readTextFileSync(%s))", pathExpr)}, nil
		case "jsonl":
			code := fmt.Sprintf("Deno.readTextFileSync(%s).trim().split(/\\r?\\n/).map(l=>JSON.parse(l))", pathExpr)
			return &RawExpr{Code: code}, nil
		case "yaml":
			code := fmt.Sprintf(`(() => {const _t=Deno.readTextFileSync(%s).trim().split(/\r?\n/);const _o:any[]=[];let c:any={};for(let line of _t){if(line.startsWith('- ')){if(Object.keys(c).length)_o.push(c);c={};line=line.slice(2);}else if(line.startsWith('  ')){line=line.slice(2);}if(!line)continue;const [k,v]=line.split(':');const val=v.trim();c[k.trim()]=/^\d+$/.test(val)?+val:val;}if(Object.keys(c).length)_o.push(c);return _o;})()`, pathExpr)
			return &RawExpr{Code: code}, nil
		default:
			return nil, fmt.Errorf("unsupported load format")
		}
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.FunExpr != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		if p.FunExpr.ExprBody != nil {
			expr, err := convertExpr(p.FunExpr.ExprBody)
			if err != nil {
				return nil, err
			}
			return &FunExpr{Params: params, Expr: expr}, nil
		}
		body, err := convertStmtList(p.FunExpr.BlockBody)
		if err != nil {
			return nil, err
		}
		return &FunExpr{Params: params, Body: body}, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr = &NameRef{Name: "undefined"}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		cond, fields, err := patternCond(c.Pattern, target)
		if err != nil {
			return nil, err
		}
		res = replaceFields(res, target, fields)
		expr = &IfExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		if lt := literalType(l); lt != nil {
			if _, ok := lt.(types.BigIntType); ok {
				const maxSafe = int64(1<<53 - 1)
				v := int64(*l.Int)
				if v <= maxSafe && v >= -maxSafe {
					return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
				}
				return &NumberLit{Value: fmt.Sprintf("%dn", *l.Int)}, nil
			}
		}
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		return &NumberLit{Value: formatFloat(*l.Float)}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Null:
		return &NullLit{}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

// zeroValue returns a default expression for the given type reference. Only a
// few primitive types are recognized; other types result in no initializer.
func zeroValue(t *parser.TypeRef, env *types.Env) Expr {
	if t == nil {
		return nil
	}
	if env == nil {
		env = transpileEnv
	}
	typ := types.ResolveTypeRef(t, env)
	switch typ.(type) {
	case types.IntType, types.FloatType:
		return &NumberLit{Value: "0"}
	case types.BigIntType:
		return &NumberLit{Value: "0n"}
	case types.BoolType:
		return &BoolLit{Value: false}
	case types.StringType:
		return &StringLit{Value: ""}
	case types.ListType:
		return &ListLit{}
	case types.MapType:
		return &MapLit{}
	case types.OptionType:
		return &NullLit{}
	case types.StructType:
		return &MapLit{}
	default:
		return nil
	}
}

// simpleExpr reports whether e is a literal expression without side effects.
// This is used to decide if a variable declaration can be safely hoisted out
// of the benchmark body.
func simpleExpr(e Expr) bool {
	switch v := e.(type) {
	case nil:
		return true
	case *NumberLit, *StringLit, *BoolLit, *NullLit:
		return true
	case *ListLit:
		for _, el := range v.Elems {
			if !simpleExpr(el) {
				return false
			}
		}
		return true
	case *MapLit:
		for _, ent := range v.Entries {
			if !simpleExpr(ent.Key) || !simpleExpr(ent.Value) {
				return false
			}
		}
		return true
	case *NameRef:
		return false
	case *BinaryExpr:
		if v.Op == "+" || v.Op == "-" || v.Op == "*" || v.Op == "/" || v.Op == "%" {
			return simpleExpr(v.Left) && simpleExpr(v.Right)
		}
		return false
	default:
		return false
	}
}

// inferLiteralType attempts to derive a more precise type from an initializer
// expression without relying solely on the type checker. It is used to produce
// typed declarations for list and map literals in untyped code.
func inferLiteralType(e *parser.Expr, env *types.Env) (types.Type, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if p.Target == nil {
		return nil, false
	}
	if ll := p.Target.List; ll != nil {
		if st, ok := types.InferStructFromList(ll, env); ok {
			return types.ListType{Elem: st}, true
		}
	}
	if ml := p.Target.Map; ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, env); ok {
			return st, true
		}
		if mt, ok := types.InferSimpleMap(ml, env); ok {
			return mt, true
		}
	}
	return nil, false
}

func literalType(l *parser.Literal) types.Type {
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: l}}}}}
	if transpileEnv == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, transpileEnv)
}

func tsType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.BigRatType, types.BigIntType:
		return "number"
	case types.BoolType:
		return "boolean"
	case types.StringType:
		return "string"
	case types.ListType:
		return tsType(tt.Elem) + "[]"
	case types.MapType:
		return "Record<" + tsType(tt.Key) + ", " + tsType(tt.Value) + ">"
	case types.OptionType:
		return tsType(tt.Elem) + " | null"
	case types.StructType:
		if tt.Name != "" {
			if transpileEnv != nil {
				if ut, ok := transpileEnv.FindUnionByVariant(tt.Name); ok {
					if ut.Name != "" {
						return ut.Name
					}
				}
			}
			return tt.Name
		}
		parts := make([]string, len(tt.Order))
		for i, name := range tt.Order {
			parts[i] = tsKey(name) + ": " + tsType(tt.Fields[name])
		}
		return "{ " + strings.Join(parts, "; ") + " }"
	case types.UnionType:
		if tt.Name != "" {
			return tt.Name
		}
		parts := make([]string, 0, len(tt.Variants))
		for name, st := range tt.Variants {
			fields := []string{"tag: \"" + name + "\""}
			for _, f := range st.Order {
				fields = append(fields, tsKey(f)+": "+tsType(st.Fields[f]))
			}
			parts = append(parts, "{ "+strings.Join(fields, "; ")+" }")
		}
		sort.Strings(parts)
		return strings.Join(parts, " | ")
	default:
		return "any"
	}
}

func ensureNamedStruct(st types.StructType, hint string) string {
	if st.Name != "" {
		return st.Name
	}
	name := types.UniqueStructName(strings.Title(hint), transpileEnv, nil)
	if generatedTypes == nil {
		generatedTypes = map[string]bool{}
	}
	if !generatedTypes[name] {
		fields := make([]string, len(st.Order))
		for i, f := range st.Order {
			fields[i] = fmt.Sprintf("%s: %s", tsKey(f), tsType(st.Fields[f]))
		}
		prelude = append(prelude, &InterfaceDecl{Name: name, Fields: fields})
		generatedTypes[name] = true
	}
	return name
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

func optimizeFillLoops(p *Program) {
	var out []Stmt
	for i := 0; i < len(p.Stmts); i++ {
		if w, ok := p.Stmts[i].(*WhileStmt); ok {
			if repl := rewriteFillLoop(w); repl != nil {
				out = append(out, repl...)
				continue
			}
		}
		out = append(out, p.Stmts[i])
	}
	p.Stmts = out
}

func rewriteFillLoop(w *WhileStmt) []Stmt {
	be, ok := w.Cond.(*BinaryExpr)
	if !ok || be.Op != "<=" {
		return nil
	}
	iter, ok := be.Left.(*NameRef)
	if !ok {
		return nil
	}
	if len(w.Body) == 0 {
		return nil
	}
	incr, ok := w.Body[len(w.Body)-1].(*AssignStmt)
	if !ok || incr.Name != iter.Name {
		return nil
	}
	bin, ok := incr.Expr.(*BinaryExpr)
	if !ok || bin.Op != "+" {
		return nil
	}
	ln, ok := bin.Left.(*NameRef)
	if !ok || ln.Name != iter.Name {
		return nil
	}
	rn, ok := bin.Right.(*NumberLit)
	if !ok || rn.Value != "1" {
		return nil
	}
	arrays := []string{}
	values := []Expr{}
	for _, st := range w.Body[:len(w.Body)-1] {
		as, ok := st.(*AssignStmt)
		if !ok {
			return nil
		}
		ce, ok := as.Expr.(*AppendExpr)
		if !ok {
			return nil
		}
		nr, ok := ce.List.(*NameRef)
		if !ok || nr.Name != as.Name {
			return nil
		}
		arrays = append(arrays, as.Name)
		values = append(values, ce.Elem)
	}
	if len(arrays) == 0 {
		return nil
	}
	length := emitExprString(be.Right) + " + 1"
	var stmts []Stmt
	for i, arr := range arrays {
		t := typedArrayType(arr)
		if t == "" {
			return nil
		}
		val := emitExprString(values[i])
		code := fmt.Sprintf("%s = new %s(%s).fill(%s);", safeName(arr), t, length, val)
		stmts = append(stmts, &RawStmt{Code: code})
	}
	return stmts
}

func typedArrayType(name string) string {
	if transpileEnv == nil {
		return ""
	}
	t, err := transpileEnv.GetVar(name)
	if err != nil {
		return ""
	}
	lt, ok := t.(types.ListType)
	if !ok {
		return ""
	}
	switch lt.Elem.(type) {
	case types.IntType, types.Int64Type:
		return "Int32Array"
	case types.FloatType, types.BigIntType, types.BigRatType:
		return "Float64Array"
	default:
		return ""
	}
}

func emitExprString(e Expr) string {
	var buf bytes.Buffer
	if e != nil {
		e.emit(&buf)
	}
	return buf.String()
}

func isNumericBool(e Expr) bool {
	switch ex := e.(type) {
	case *BinaryExpr:
		switch ex.Op {
		case "in":
			return true
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return true
		}
		return isNumericBool(ex.Left) || isNumericBool(ex.Right)
	case *UnaryExpr:
		if ex.Op == "!" {
			if be, ok := ex.Expr.(*BinaryExpr); ok && be.Op == "in" {
				return false
			}
			if mc, ok := ex.Expr.(*MethodCallExpr); ok && mc.Method == "includes" {
				return false
			}
			return true
		}
		return isNumericBool(ex.Expr)
	case *MethodCallExpr:
		if ex.Method == "includes" {
			return true
		}
		return false
	default:
		return false
	}
}

func isListType(t types.Type) bool {
	switch tt := t.(type) {
	case types.ListType:
		return true
	case types.OptionType:
		return isListType(tt.Elem)
	default:
		return false
	}
}

func isBoolType(t types.Type) bool {
	switch tt := t.(type) {
	case types.BoolType:
		return true
	case types.OptionType:
		return isBoolType(tt.Elem)
	default:
		return false
	}
}

func isBoolExpr(e *parser.Expr) bool {
	if transpileEnv == nil {
		return false
	}
	return isBoolType(types.CheckExprType(e, transpileEnv))
}

func isBigIntType(t types.Type) bool {
	switch tt := t.(type) {
	case types.OptionType:
		return isBigIntType(tt.Elem)
	default:
		return false
	}
}

func containsLargeIntMul(e Expr) bool {
	switch v := e.(type) {
	case *BinaryExpr:
		if v.Op == "*" {
			if isLargeIntOperand(v.Left) && (isVarIntExpr(v.Right) || isLargeIntOperand(v.Right)) {
				return true
			}
			if isLargeIntOperand(v.Right) && (isVarIntExpr(v.Left) || isLargeIntOperand(v.Left)) {
				return true
			}
		}
		return containsLargeIntMul(v.Left) || containsLargeIntMul(v.Right)
	case *IntDivExpr:
		return containsLargeIntMul(v.Left) || containsLargeIntMul(v.Right)
	}
	return false
}

func isLargeIntOperand(e Expr) bool {
	if n, ok := e.(*NumberLit); ok {
		val, err := strconv.ParseInt(strings.TrimSuffix(n.Value, "n"), 10, 64)
		if err == nil {
			if val > maxSafeMul || val < -maxSafeMul {
				return true
			}
		}
	}
	return false
}

func toBigIntExpr(e Expr) Expr {
	switch v := e.(type) {
	case *NumberLit:
		if !strings.HasSuffix(v.Value, "n") {
			return &NumberLit{Value: v.Value + "n"}
		}
		return v
	case *BinaryExpr:
		return &BinaryExpr{Left: toBigIntExpr(v.Left), Op: v.Op, Right: toBigIntExpr(v.Right)}
	case *IntDivExpr:
		return &IntDivExpr{Left: toBigIntExpr(v.Left), Right: toBigIntExpr(v.Right), Big: true}
	case *NameRef:
		return &CallExpr{Func: "BigInt", Args: []Expr{v}}
	default:
		return &CallExpr{Func: "BigInt", Args: []Expr{e}}
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

func isTopLevelMainCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Right != nil {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return false
	}
	call := u.Value.Target.Call
	return call != nil && call.Func == "main" && len(call.Args) == 0
}

func isIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
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

func tsKey(name string) string {
	if isIdent(name) {
		return name
	}
	return strconv.Quote(name)
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
		key, ok := isSimpleIdent(it.Key)
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

func mapStringFields(m *MapLit) ([]string, bool) {
	if m == nil {
		return nil, false
	}
	fields := make([]string, 0, len(m.Entries))
	for _, ent := range m.Entries {
		switch k := ent.Key.(type) {
		case *StringLit:
			fields = append(fields, k.Value)
		case *NameRef:
			fields = append(fields, k.Name)
		default:
			return nil, false
		}
	}
	return fields, true
}

func emitSortComparator(w io.Writer, sortExpr Expr) {
	switch s := sortExpr.(type) {
	case *IndexExpr:
		if key, ok := s.Index.(*StringLit); ok && isIdent(key.Value) {
			fmt.Fprintf(w, "a.k.%s < b.k.%s ? -1 : a.k.%s > b.k.%s ? 1 : 0", key.Value, key.Value, key.Value, key.Value)
			return
		}
	case *NameRef:
		io.WriteString(w, "a.k < b.k ? -1 : a.k > b.k ? 1 : 0")
		return
	case *MapLit:
		if fields, ok := mapStringFields(s); ok {
			for _, f := range fields {
				fmt.Fprintf(w, "a.k.%s < b.k.%s ? -1 : a.k.%s > b.k.%s ? 1 : ", f, f, f, f)
			}
			io.WriteString(w, "0")
			return
		}
	}
	io.WriteString(w, "a.k < b.k ? -1 : a.k > b.k ? 1 : 0")
}

func substituteFields(e Expr, varName string, fields map[string]bool, methods map[string]bool) Expr {
	switch ex := e.(type) {
	case *NameRef:
		if fields[ex.Name] {
			return &IndexExpr{Target: &NameRef{Name: varName}, Index: &StringLit{Value: ex.Name}}
		}
		if methods[ex.Name] {
			return &MethodCallExpr{Target: &NameRef{Name: varName}, Method: ex.Name}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFields(ex.Left, varName, fields, methods)
		ex.Right = substituteFields(ex.Right, varName, fields, methods)
		return ex
	case *UnaryExpr:
		ex.Expr = substituteFields(ex.Expr, varName, fields, methods)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields, methods)
		}
		if methods[ex.Func] {
			return &MethodCallExpr{Target: &NameRef{Name: varName}, Method: ex.Func, Args: ex.Args}
		}
		return ex
	case *MethodCallExpr:
		ex.Target = substituteFields(ex.Target, varName, fields, methods)
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields, methods)
		}
		return ex
	case *IndexExpr:
		ex.Target = substituteFields(ex.Target, varName, fields, methods)
		ex.Index = substituteFields(ex.Index, varName, fields, methods)
		return ex
	case *SliceExpr:
		ex.Target = substituteFields(ex.Target, varName, fields, methods)
		if ex.Start != nil {
			ex.Start = substituteFields(ex.Start, varName, fields, methods)
		}
		if ex.End != nil {
			ex.End = substituteFields(ex.End, varName, fields, methods)
		}
		return ex
	case *IfExpr:
		ex.Cond = substituteFields(ex.Cond, varName, fields, methods)
		ex.Then = substituteFields(ex.Then, varName, fields, methods)
		ex.Else = substituteFields(ex.Else, varName, fields, methods)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFields(ex.Elems[i], varName, fields, methods)
		}
		return ex
	case *MapLit:
		for i := range ex.Entries {
			ex.Entries[i].Key = substituteFields(ex.Entries[i].Key, varName, fields, methods)
			ex.Entries[i].Value = substituteFields(ex.Entries[i].Value, varName, fields, methods)
		}
		return ex
	default:
		return ex
	}
}

func replaceFields(e Expr, target Expr, fields map[string]string) Expr {
	switch ex := e.(type) {
	case *NameRef:
		if field, ok := fields[ex.Name]; ok {
			return &IndexExpr{Target: target, Index: &StringLit{Value: field}}
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceFields(ex.Left, target, fields)
		ex.Right = replaceFields(ex.Right, target, fields)
		return ex
	case *UnaryExpr:
		ex.Expr = replaceFields(ex.Expr, target, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = replaceFields(ex.Args[i], target, fields)
		}
		return ex
	case *MethodCallExpr:
		ex.Target = replaceFields(ex.Target, target, fields)
		for i := range ex.Args {
			ex.Args[i] = replaceFields(ex.Args[i], target, fields)
		}
		return ex
	case *IndexExpr:
		ex.Target = replaceFields(ex.Target, target, fields)
		ex.Index = replaceFields(ex.Index, target, fields)
		return ex
	case *SliceExpr:
		ex.Target = replaceFields(ex.Target, target, fields)
		if ex.Start != nil {
			ex.Start = replaceFields(ex.Start, target, fields)
		}
		if ex.End != nil {
			ex.End = replaceFields(ex.End, target, fields)
		}
		return ex
	case *IfExpr:
		ex.Cond = replaceFields(ex.Cond, target, fields)
		ex.Then = replaceFields(ex.Then, target, fields)
		ex.Else = replaceFields(ex.Else, target, fields)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceFields(ex.Elems[i], target, fields)
		}
		return ex
	case *MapLit:
		for i := range ex.Entries {
			ex.Entries[i].Key = replaceFields(ex.Entries[i].Key, target, fields)
			ex.Entries[i].Value = replaceFields(ex.Entries[i].Value, target, fields)
		}
		return ex
	default:
		return ex
	}
}

func substituteStmtFields(s Stmt, varName string, fields, methods map[string]bool) Stmt {
	switch st := s.(type) {
	case *ExprStmt:
		st.Expr = substituteFields(st.Expr, varName, fields, methods)
		return st
	case *ReturnStmt:
		if st.Value != nil {
			st.Value = substituteFields(st.Value, varName, fields, methods)
		}
		return st
	case *AssignStmt:
		st.Expr = substituteFields(st.Expr, varName, fields, methods)
		return st
	case *VarDecl:
		if st.Expr != nil {
			st.Expr = substituteFields(st.Expr, varName, fields, methods)
		}
		return st
	default:
		return st
	}
}

func emitReplaceName(w io.Writer, e Expr, name string, repl Expr) {
	switch ex := e.(type) {
	case *NameRef:
		if ex.Name == name {
			repl.emit(w)
		} else {
			ex.emit(w)
		}
	case *IndexExpr:
		emitReplaceName(w, ex.Target, name, repl)
		io.WriteString(w, "[")
		if ex.Index != nil {
			emitReplaceName(w, ex.Index, name, repl)
		}
		io.WriteString(w, "]")
	case *MapLit:
		io.WriteString(w, "{")
		for i, ent := range ex.Entries {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			switch k := ent.Key.(type) {
			case *StringLit:
				if v, ok := ent.Value.(*NameRef); ok && v.Name == k.Value {
					if k.Value == name {
						io.WriteString(w, k.Value)
						continue
					}
				}
				fmt.Fprintf(w, "%q: ", k.Value)
				emitReplaceName(w, ent.Value, name, repl)
			case *NameRef:
				if v, ok := ent.Value.(*NameRef); ok && v.Name == k.Name {
					if k.Name == name {
						io.WriteString(w, k.Name)
						continue
					}
				}
				io.WriteString(w, k.Name)
				io.WriteString(w, ": ")
				emitReplaceName(w, ent.Value, name, repl)
			default:
				io.WriteString(w, "[")
				emitReplaceName(w, ent.Key, name, repl)
				io.WriteString(w, "]: ")
				emitReplaceName(w, ent.Value, name, repl)
			}
		}
		io.WriteString(w, "}")
	case *BinaryExpr:
		emitReplaceName(w, ex.Left, name, repl)
		io.WriteString(w, " "+ex.Op+" ")
		emitReplaceName(w, ex.Right, name, repl)
	case *UnaryExpr:
		io.WriteString(w, ex.Op)
		emitReplaceName(w, ex.Expr, name, repl)
	case *ListLit:
		io.WriteString(w, "[")
		for i, el := range ex.Elems {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			emitReplaceName(w, el, name, repl)
		}
		io.WriteString(w, "]")
	default:
		ex.emit(w)
	}
}

func patternCond(pat *parser.Expr, target Expr) (Expr, map[string]string, error) {
	if pat == nil {
		return nil, nil, fmt.Errorf("nil pattern")
	}
	if call, ok := callPattern(pat); ok && transpileEnv != nil {
		if ut, ok := transpileEnv.FindUnionByVariant(call.Func); ok {
			st := ut.Variants[call.Func]
			cond := &BinaryExpr{Left: &IndexExpr{Target: target, Index: &StringLit{Value: "tag"}}, Op: "===", Right: &StringLit{Value: call.Func}}
			fields := map[string]string{}
			for i, arg := range call.Args {
				fieldName := st.Order[i]
				if name, ok := identName(arg); ok {
					fields[name] = fieldName
				} else {
					val, err := convertExpr(arg)
					if err != nil {
						return nil, nil, err
					}
					part := &BinaryExpr{Left: &IndexExpr{Target: target, Index: &StringLit{Value: fieldName}}, Op: "===", Right: val}
					cond = &BinaryExpr{Left: cond, Op: "&&", Right: part}
				}
			}
			return cond, fields, nil
		}
	}
	if name, ok := identName(pat); ok {
		if name == "_" {
			return &BoolLit{Value: true}, nil, nil
		}
		if transpileEnv != nil {
			if _, ok := transpileEnv.FindUnionByVariant(name); ok {
				cond := &BinaryExpr{Left: &IndexExpr{Target: target, Index: &StringLit{Value: "tag"}}, Op: "===", Right: &StringLit{Value: name}}
				return cond, nil, nil
			}
		}
	}
	expr, err := convertExpr(pat)
	if err != nil {
		return nil, nil, err
	}
	return &BinaryExpr{Left: target, Op: "===", Right: expr}, nil, nil
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

// selectorToExpr converts a selector expression like a.b.c into nested index
// expressions so the transpiler can treat it uniformly with dynamic property
// access. It never returns nil.
func selectorToExpr(sel *parser.SelectorExpr) Expr {
	expr := Expr(&NameRef{Name: sel.Root})
	if sel.Root == "__name__" {
		usePyName = true
	}
	for _, part := range sel.Tail {
		if pythonMathAliases != nil {
			if nr, ok := expr.(*NameRef); ok && pythonMathAliases[nr.Name] {
				if part == "pi" || part == "e" {
					part = strings.ToUpper(part)
				}
			}
		}
		expr = &IndexExpr{Target: expr, Index: &StringLit{Value: part}}
	}
	return expr
}

// print converts the given TypeScript AST into a generic ast.Node tree and
// writes it to w. It is useful for debugging and tests.
func print(p *Program, w io.Writer) {
	if p == nil {
		return
	}
	node := progToNode(p)
	io.WriteString(w, node.String())
}

func progToNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtToNode(st))
	}
	return n
}

func stmtToNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprToNode(st.Expr)}}
	case *VarDecl:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprToNode(st.Expr)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprToNode(st.Expr)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "idx-assign", Children: []*ast.Node{exprToNode(st.Target), exprToNode(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprToNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, c := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, c := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtToNode(c))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprToNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for-range", Value: st.Name, Children: []*ast.Node{exprToNode(st.Start), exprToNode(st.End)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForInStmt:
		value := st.Name
		if st.Keys {
			value += "-keys"
		}
		n := &ast.Node{Kind: "for-in", Value: value, Children: []*ast.Node{exprToNode(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, c := range st.Body {
			body.Children = append(body.Children, stmtToNode(c))
		}
		n.Children = append(n.Children, body)
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ReturnStmt:
		child := &ast.Node{Kind: "return"}
		if st.Value != nil {
			child.Children = []*ast.Node{exprToNode(st.Value)}
		}
		return child
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for i, p := range st.Params {
			val := p
			if i < len(st.ParamTypes) && st.ParamTypes[i] != "" {
				val += ":" + st.ParamTypes[i]
			}
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: val})
		}
		if st.ReturnType != "" {
			n.Children = append(n.Children, &ast.Node{Kind: "returns", Value: st.ReturnType})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtToNode(b))
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprToNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *NumberLit:
		return &ast.Node{Kind: "number", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *NullLit:
		return &ast.Node{Kind: "null"}
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprToNode(ex.Left), exprToNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprToNode(ex.Expr)}}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprToNode(e))
		}
		return n
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprToNode(ex.List), exprToNode(ex.Elem)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *UnionExpr:
		return &ast.Node{Kind: "union", Children: []*ast.Node{exprToNode(ex.Left), exprToNode(ex.Right)}}
	case *UnionAllExpr:
		return &ast.Node{Kind: "unionall", Children: []*ast.Node{exprToNode(ex.Left), exprToNode(ex.Right)}}
	case *ExceptExpr:
		return &ast.Node{Kind: "except", Children: []*ast.Node{exprToNode(ex.Left), exprToNode(ex.Right)}}
	case *IntersectExpr:
		return &ast.Node{Kind: "intersect", Children: []*ast.Node{exprToNode(ex.Left), exprToNode(ex.Right)}}
	case *FormatListExpr:
		return &ast.Node{Kind: "fmtlist", Children: []*ast.Node{exprToNode(ex.Value)}}
	case *PrintExpr:
		n := &ast.Node{Kind: "print"}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprToNode(ex.Str), exprToNode(ex.Start), exprToNode(ex.End)}}
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, e := range ex.Entries {
			n.Children = append(n.Children, &ast.Node{Kind: "entry", Children: []*ast.Node{exprToNode(e.Key), exprToNode(e.Value)}})
		}
		return n
	case *StructUpdateExpr:
		n := &ast.Node{Kind: "struct-update", Value: ex.Field}
		n.Children = append(n.Children, exprToNode(ex.Target))
		n.Children = append(n.Children, exprToNode(ex.Value))
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprToNode(ex.Target), exprToNode(ex.Index)}}
	case *SliceExpr:
		return &ast.Node{Kind: "slice", Children: []*ast.Node{exprToNode(ex.Target), exprToNode(ex.Start), exprToNode(ex.End)}}
	case *FunExpr:
		n := &ast.Node{Kind: "funexpr"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		if ex.Expr != nil {
			n.Children = append(n.Children, exprToNode(ex.Expr))
		} else {
			body := &ast.Node{Kind: "body"}
			for _, st := range ex.Body {
				body.Children = append(body.Children, stmtToNode(st))
			}
			n.Children = append(n.Children, body)
		}
		return n
	case *InvokeExpr:
		n := &ast.Node{Kind: "invoke"}
		n.Children = append(n.Children, exprToNode(ex.Callee))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *IfExpr:
		n := &ast.Node{Kind: "ifexpr"}
		n.Children = append(n.Children, exprToNode(ex.Cond))
		n.Children = append(n.Children, exprToNode(ex.Then))
		n.Children = append(n.Children, exprToNode(ex.Else))
		return n
	case *MethodCallExpr:
		n := &ast.Node{Kind: "method", Value: ex.Method}
		n.Children = append(n.Children, exprToNode(ex.Target))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *QueryExprJS:
		n := &ast.Node{Kind: "query"}
		for _, l := range ex.Loops {
			loopNode := &ast.Node{Kind: "for", Value: l.Name, Children: []*ast.Node{exprToNode(l.Source)}}
			n.Children = append(n.Children, loopNode)
		}
		if ex.Where != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprToNode(ex.Where)}})
		}
		if ex.Sort != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "sort", Children: []*ast.Node{exprToNode(ex.Sort)}})
		}
		if ex.Skip != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "skip", Children: []*ast.Node{exprToNode(ex.Skip)}})
		}
		if ex.Take != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "take", Children: []*ast.Node{exprToNode(ex.Take)}})
		}
		n.Children = append(n.Children, &ast.Node{Kind: "select", Children: []*ast.Node{exprToNode(ex.Select)}})
		return n
	case *AggQueryExpr:
		n := &ast.Node{Kind: "agg", Value: ex.Op}
		n.Children = append(n.Children, &ast.Node{Kind: "var", Value: ex.Var})
		n.Children = append(n.Children, exprToNode(ex.Source))
		if ex.Where != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprToNode(ex.Where)}})
		}
		return n
	case *GroupQueryExpr:
		n := &ast.Node{Kind: "group-query"}
		n.Children = append(n.Children, &ast.Node{Kind: "var", Value: ex.Var})
		n.Children = append(n.Children, exprToNode(ex.Source))
		n.Children = append(n.Children, &ast.Node{Kind: "key", Children: []*ast.Node{exprToNode(ex.Key)}})
		if ex.Having != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "having", Children: []*ast.Node{exprToNode(ex.Having)}})
		}
		n.Children = append(n.Children, &ast.Node{Kind: "select", Children: []*ast.Node{exprToNode(ex.Select)}})
		return n
	case *GroupJoinQueryExpr:
		n := &ast.Node{Kind: "group-query"}
		for _, l := range ex.Loops {
			n.Children = append(n.Children, &ast.Node{Kind: "for", Value: l.Name, Children: []*ast.Node{exprToNode(l.Source)}})
		}
		for _, j := range ex.Joins {
			jc := &ast.Node{Kind: "join", Value: j.Name, Children: []*ast.Node{exprToNode(j.Source), exprToNode(j.On)}}
			if j.Side != "" {
				jc.Value = j.Side + " " + j.Name
			}
			n.Children = append(n.Children, jc)
		}
		if ex.Where != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprToNode(ex.Where)}})
		}
		n.Children = append(n.Children, &ast.Node{Kind: "key", Children: []*ast.Node{exprToNode(ex.Key)}})
		if ex.Having != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "having", Children: []*ast.Node{exprToNode(ex.Having)}})
		}
		n.Children = append(n.Children, &ast.Node{Kind: "select", Children: []*ast.Node{exprToNode(ex.Select)}})
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// fixScientificNotation merges parsed statements that represent scientific
// notation like `1.0e18` which the Mochi parser currently tokenizes as a
// number followed by an identifier such as `e18`. This pass adjusts the
// preceding numeric literal and removes the dangling identifier statement so
// the transpiled code sees the correct value.
func fixScientificNotation(prog *parser.Program) {
	if prog == nil {
		return
	}
	prog.Statements = fixSciSlice(prog.Statements)
}

func fixSciSlice(stmts []*parser.Statement) []*parser.Statement {
	if len(stmts) == 0 {
		return stmts
	}
	var out []*parser.Statement
	for i := 0; i < len(stmts); i++ {
		st := stmts[i]
		// Merge var/let/assign followed by eNN expression
		if i+1 < len(stmts) && stmts[i+1].Expr != nil {
			if exp, ok := parseENotation(stmts[i+1].Expr.Expr); ok {
				adjusted := false
				if st.Var != nil {
					adjusted = applyENotation(st.Var.Value, exp)
				} else if st.Let != nil {
					adjusted = applyENotation(st.Let.Value, exp)
				} else if st.Assign != nil {
					adjusted = applyENotation(st.Assign.Value, exp)
				}
				if adjusted {
					i++ // skip exponent statement
				}
			}
		}
		// Recurse into statement bodies
		if st.Fun != nil {
			st.Fun.Body = fixSciSlice(st.Fun.Body)
		}
		if st.If != nil {
			st.If.Then = fixSciSlice(st.If.Then)
			if st.If.ElseIf != nil {
				st.If.ElseIf.Then = fixSciSlice(st.If.ElseIf.Then)
				if len(st.If.ElseIf.Else) > 0 {
					st.If.ElseIf.Else = fixSciSlice(st.If.ElseIf.Else)
				}
			}
			if len(st.If.Else) > 0 {
				st.If.Else = fixSciSlice(st.If.Else)
			}
		}
		if st.While != nil {
			st.While.Body = fixSciSlice(st.While.Body)
		}
		if st.For != nil {
			st.For.Body = fixSciSlice(st.For.Body)
		}
		if st.Bench != nil {
			st.Bench.Body = fixSciSlice(st.Bench.Body)
		}
		if st.Test != nil {
			st.Test.Body = fixSciSlice(st.Test.Body)
		}
		out = append(out, st)
	}
	return out
}

// parseENotation checks if the expression is an identifier of the form eNN
// and returns the numeric exponent when true.
func parseENotation(expr *parser.Expr) (int, bool) {
	if expr == nil || expr.Binary == nil || len(expr.Binary.Right) != 0 {
		return 0, false
	}
	u := expr.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil {
		return 0, false
	}
	sel := u.Value.Target.Selector
	if sel == nil || len(sel.Tail) != 0 {
		return 0, false
	}
	name := sel.Root
	if strings.HasPrefix(name, "e") {
		if n, err := strconv.Atoi(name[1:]); err == nil {
			return n, true
		}
	}
	return 0, false
}

// applyENotation multiplies the numeric literal inside expr by 10^exp. It
// returns true if the expression was modified.
func applyENotation(expr *parser.Expr, exp int) bool {
	if expr == nil || expr.Binary == nil || len(expr.Binary.Right) != 0 {
		return false
	}
	u := expr.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	sign := 1.0
	if len(u.Ops) == 1 {
		if u.Ops[0] == "-" {
			sign = -1.0
		} else if u.Ops[0] != "+" {
			return false
		}
		u.Ops = nil
	} else if len(u.Ops) > 1 {
		return false
	}
	lit := u.Value.Target.Lit
	if lit == nil {
		return false
	}
	var val float64
	if lit.Float != nil {
		val = *lit.Float
	} else if lit.Int != nil {
		val = float64(*lit.Int)
		lit.Int = nil
	} else {
		return false
	}
	val = sign * val * math.Pow(10, float64(exp))
	lit.Float = &val
	return true
}
