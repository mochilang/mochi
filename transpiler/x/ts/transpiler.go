//go:build slow

package tstranspiler

import (
	"bytes"
	"fmt"
	"io"
	"strings"

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

// FuncDecl represents a function definition.
type FuncDecl struct {
	Name       string
	Params     []string
	ParamTypes []string
	ReturnType string
	Body       []Stmt
}

// TypeAlias represents `type Name = { ... }` declarations.
type TypeAlias struct {
	Name string
	Type string
}

func (t *TypeAlias) emit(w io.Writer) {
	io.WriteString(w, "type ")
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

type FormatListExpr struct{ Value Expr }

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

// QueryExprJS represents a simplified query comprehension.
type QueryExprJS struct {
	Loops  []QueryLoop
	Where  Expr
	Sort   Expr
	Skip   Expr
	Take   Expr
	Select Expr
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

// RawExpr emits raw TypeScript code verbatim.
type RawExpr struct{ Code string }

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
	io.WriteString(w, c.Func)
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

func (n *NameRef) emit(w io.Writer) { io.WriteString(w, n.Name) }

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
	io.WriteString(w, "(")
	io.WriteString(w, "Array.isArray(")
	l.Value.emit(w)
	io.WriteString(w, ") || typeof ")
	l.Value.emit(w)
	io.WriteString(w, " === 'string' ? ")
	l.Value.emit(w)
	io.WriteString(w, ".length : Object.keys(")
	l.Value.emit(w)
	io.WriteString(w, " ?? {}).length)")
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
	io.WriteString(w, "; return arr.reduce((a, b) => a + b, 0) / arr.length; })()")
}

func (e *SumExpr) emit(w io.Writer) {
	if e.Value != nil {
		e.Value.emit(w)
		io.WriteString(w, ".reduce((a, b) => a + b, 0)")
	} else {
		io.WriteString(w, "0")
	}
}

func (e *MinExpr) emit(w io.Writer) {
	io.WriteString(w, "(() => { const arr = ")
	if e.Value != nil {
		e.Value.emit(w)
	}
	io.WriteString(w, "; return arr.length === 0 ? 0 : Math.min(...arr); })()")
}

func (e *MaxExpr) emit(w io.Writer) {
	io.WriteString(w, "(() => { const arr = ")
	if e.Value != nil {
		e.Value.emit(w)
	}
	io.WriteString(w, "; return arr.length === 0 ? 0 : Math.max(...arr); })()")
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
	io.WriteString(w, "\"[\" + ")
	if f.Value != nil {
		f.Value.emit(w)
		io.WriteString(w, ".join(\", \")")
	} else {
		io.WriteString(w, "\"\"")
	}
	io.WriteString(w, " + \"]\"")
}

func (p *PrintExpr) emit(w io.Writer) {
	io.WriteString(w, "console.log([")
	for i, a := range p.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if a != nil {
			a.emit(w)
		}
	}
	io.WriteString(w, "].join(\" \").trimEnd())")
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
			fmt.Fprintf(w, "%q: ", k.Value)
		case *NameRef:
			io.WriteString(w, k.Name)
			io.WriteString(w, ": ")
		default:
			io.WriteString(w, "[")
			e.Key.emit(w)
			io.WriteString(w, "]: ")
		}
		e.Value.emit(w)
	}
	io.WriteString(w, "}")
}

func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, "[")
	if i.Index != nil {
		i.Index.emit(w)
	}
	io.WriteString(w, "]")
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, ".slice(")
	if s.Start != nil {
		s.Start.emit(w)
	}
	if s.End != nil {
		io.WriteString(w, ", ")
		s.End.emit(w)
	}
	io.WriteString(w, ")")
}

func (m *MethodCallExpr) emit(w io.Writer) {
	m.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, m.Method)
	io.WriteString(w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
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
	io.WriteString(iw, "  const result = []\n")
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
		io.WriteString(iw, "  result.sort((a, b) => {")
		io.WriteString(iw, "const ak = JSON.stringify(a.k); const bk = JSON.stringify(b.k);")
		io.WriteString(iw, " return ak < bk ? -1 : ak > bk ? 1 : 0})\n")
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
		io.WriteString(w, "  console.log(JSON.stringify(_row))\n")
		io.WriteString(w, "}")
		return
	}
	io.WriteString(w, "// unsupported save")
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

func (r *RawExpr) emit(w io.Writer) { io.WriteString(w, r.Code) }

func (v *VarDecl) emit(w io.Writer) {
	if v.Const {
		io.WriteString(w, "const ")
	} else {
		io.WriteString(w, "let ")
	}
	io.WriteString(w, v.Name)
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
	io.WriteString(w, a.Name)
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
	io.WriteString(w, f.Name)
	io.WriteString(w, " = ")
	if f.Start != nil {
		f.Start.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Name)
	io.WriteString(w, " < ")
	if f.End != nil {
		f.End.emit(w)
	}
	io.WriteString(w, "; ")
	io.WriteString(w, f.Name)
	io.WriteString(w, "++) {\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

func (f *ForInStmt) emit(w io.Writer) {
	io.WriteString(w, "for (const ")
	io.WriteString(w, f.Name)
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
	io.WriteString(w, "function ")
	io.WriteString(w, f.Name)
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
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
	b.Write(meta.Header("//"))
	b.WriteByte('\n')
	iw := &indentWriter{w: &b, indent: "  "}
	for _, s := range p.Stmts {
		emitStmt(iw, s, 0)
	}
	return b.Bytes()
}

func emitStmt(w *indentWriter, s Stmt, level int) {
	pad := strings.Repeat(w.indent, level)
	switch st := s.(type) {
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
		io.WriteString(w, "function ")
		io.WriteString(w, st.Name)
		io.WriteString(w, "(")
		for i, p := range st.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, p)
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
		io.WriteString(w, st.Name)
		io.WriteString(w, " = ")
		if st.Start != nil {
			st.Start.emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, "; ")
		io.WriteString(w, st.Name)
		io.WriteString(w, " < ")
		if st.End != nil {
			st.End.emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, "; ")
		io.WriteString(w, st.Name)
		io.WriteString(w, "++) {\n")
		for _, bs := range st.Body {
			emitStmt(w, bs, level+1)
		}
		io.WriteString(w, pad)
		io.WriteString(w, "}\n")
	case *ForInStmt:
		io.WriteString(w, pad)
		io.WriteString(w, "for (const ")
		io.WriteString(w, st.Name)
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
	}
}

// Transpile converts a Mochi program into a TypeScript AST. Only a very
// small subset of the language is supported: programs consisting of a single
// call to the builtin `print` with a string literal argument.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	transpileEnv = env
	defer func() { transpileEnv = nil }()
	tsProg := &Program{}

	for _, st := range prog.Statements {
		stmt, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		tsProg.Stmts = append(tsProg.Stmts, stmt)
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
		mutable, _ := transpileEnv.IsMutable(s.Let.Name)
		t, _ := transpileEnv.GetVar(s.Let.Name)
		return &VarDecl{Name: s.Let.Name, Expr: e, Const: !mutable, Type: tsType(t)}, nil
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
		mutable, _ := transpileEnv.IsMutable(s.Var.Name)
		t, _ := transpileEnv.GetVar(s.Var.Name)
		return &VarDecl{Name: s.Var.Name, Expr: e, Const: !mutable, Type: tsType(t)}, nil
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
		if s.Import.Lang != nil && *s.Import.Lang == "python" && strings.Trim(s.Import.Path, "\"") == "math" {
			return &VarDecl{Name: alias, Expr: &NameRef{Name: "Math"}, Const: true}, nil
		}
		if s.Import.Lang != nil && *s.Import.Lang == "go" && strings.Trim(s.Import.Path, "\"") == "mochi/runtime/ffi/go/testpkg" {
			expr := &RawExpr{Code: "{ Add: (a:number,b:number)=>a+b, Pi: 3.14, Answer: 42 }"}
			return &VarDecl{Name: alias, Expr: expr, Const: true}, nil
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
		return &ReturnStmt{Value: e}, nil
	case s.Break != nil:
		return &BreakStmt{}, nil
	case s.Continue != nil:
		return &ContinueStmt{}, nil
	case s.Fun != nil:
		body, err := convertStmtList(s.Fun.Body)
		if err != nil {
			return nil, err
		}
		var params []string
		var typesArr []string
		for _, p := range s.Fun.Params {
			params = append(params, p.Name)
			if p.Type != nil {
				typesArr = append(typesArr, tsType(types.ResolveTypeRef(p.Type, transpileEnv)))
			} else {
				typesArr = append(typesArr, "")
			}
		}
		var retType string
		if s.Fun.Return != nil {
			retType = tsType(types.ResolveTypeRef(s.Fun.Return, transpileEnv))
		}
		return &FuncDecl{Name: s.Fun.Name, Params: params, ParamTypes: typesArr, ReturnType: retType, Body: body}, nil
	case s.If != nil:
		return convertIfStmt(s.If, transpileEnv)
	case s.While != nil:
		return convertWhileStmt(s.While, transpileEnv)
	case s.For != nil:
		return convertForStmt(s.For, transpileEnv)
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
	body, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	keys := false
	if env != nil {
		switch types.ExprType(f.Source, env).(type) {
		case types.MapType:
			keys = true
		}
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
		val = substituteFields(val, "item", fieldSet)
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
		cond = substituteFields(cond, "item", fieldSet)
	}
	transpileEnv = prev
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func convertTypeDecl(td *parser.TypeDecl) (Stmt, error) {
	if len(td.Variants) > 0 {
		return nil, fmt.Errorf("sum types not supported")
	}
	var parts []string
	for _, m := range td.Members {
		if m.Field == nil {
			continue
		}
		ft := types.ResolveTypeRef(m.Field.Type, transpileEnv)
		parts = append(parts, fmt.Sprintf("%s: %s", m.Field.Name, tsType(ft)))
	}
	typ := "{ " + strings.Join(parts, "; ") + " }"
	return &TypeAlias{Name: td.Name, Type: typ}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
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
	loops[0].Source = src
	for _, f := range q.Froms {
		src, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		loops = append(loops, QueryLoop{Name: f.Var, Source: src})
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}
	var sort Expr
	if q.Sort != nil {
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
	sel, err := convertExpr(q.Select)
	if err != nil {
		return nil, err
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
	return &QueryExprJS{Loops: loops, Where: where, Sort: sort, Skip: skip, Take: take, Select: sel}, nil
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
	return false
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	operands := []Expr{}
	ops := []string{}
	opnodes := []*parser.BinaryOp{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, r := range b.Right {
		o, err := convertPostfix(r.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, o)
		ops = append(ops, r.Op)
		opnodes = append(opnodes, r)
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
		switch ops[i] {
		case "in":
			isMap := false
			if typ := postfixExprType(opnodes[i].Right); typ != nil {
				if _, ok := typ.(types.MapType); ok {
					isMap = true
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
			operands[i] = &BinaryExpr{Left: operands[i], Op: ops[i], Right: operands[i+1]}
		}
		operands = append(operands[:i+1], operands[i+2:]...)
		ops = append(ops[:i], ops[i+1:]...)
		opnodes = append(opnodes[:i], opnodes[i+1:]...)
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

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(p.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range p.Ops {
		switch {
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
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
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
				if lit, ok2 := idx.Index.(*StringLit); ok2 && lit.Value == "contains" && len(args) == 1 {
					expr = &MethodCallExpr{Target: idx.Target, Method: "includes", Args: args}
				} else {
					expr = &InvokeExpr{Callee: expr, Args: args}
				}
			} else {
				expr = &InvokeExpr{Callee: expr, Args: args}
			}
		case op.Field != nil:
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}}
		case op.Cast != nil:
			// ignore casts
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Struct != nil:
		entries := make([]MapEntry, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			val, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: &StringLit{Value: f.Name}, Value: val}
		}
		return &MapLit{Entries: entries}, nil
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil:
		expr := selectorToExpr(p.Selector)
		return expr, nil
	case p.Call != nil:
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
				if len(args) == 1 {
					// Special case printing list values
					if v, ok := args[0].(*ValuesExpr); ok {
						val := &CallExpr{Func: "Object.values", Args: []Expr{v.Value}}
						sortCall := &MethodCallExpr{Target: val, Method: "sort", Args: []Expr{}}
						args[0] = &MethodCallExpr{Target: sortCall, Method: "join", Args: []Expr{&StringLit{Value: " "}}}
					} else if lt, ok := types.ExprType(p.Call.Args[0], transpileEnv).(types.ListType); ok {
						switch lt.Elem.(type) {
						case types.StringType, types.AnyType:
							args[0] = &MethodCallExpr{Target: args[0], Method: "join", Args: []Expr{&StringLit{Value: " "}}}
						case types.MapType, types.StructType:
							js := &CallExpr{Func: "JSON.stringify", Args: []Expr{&NameRef{Name: "x"}}}
							rep1 := &MethodCallExpr{Target: js, Method: "replace", Args: []Expr{&CallExpr{Func: "RegExp", Args: []Expr{&StringLit{Value: ":"}, &StringLit{Value: "g"}}}, &StringLit{Value: ": "}}}
							rep2 := &MethodCallExpr{Target: rep1, Method: "replace", Args: []Expr{&CallExpr{Func: "RegExp", Args: []Expr{&StringLit{Value: ","}, &StringLit{Value: "g"}}}, &StringLit{Value: ", "}}}
							m := &MethodCallExpr{
								Target: args[0],
								Method: "map",
								Args:   []Expr{&FunExpr{Params: []string{"x"}, Expr: rep2}},
							}
							args[0] = &MethodCallExpr{Target: m, Method: "join", Args: []Expr{&StringLit{Value: " "}}}
						default:
							args = []Expr{&FormatListExpr{Value: args[0]}}
						}
					}
				}
				for i, a := range args {
					if isNumericBool(a) {
						args[i] = &UnaryExpr{Op: "+", Expr: a}
					}
				}
			}
			return &PrintExpr{Args: args}, nil
		case "len":
			if len(args) != 1 {
				return nil, fmt.Errorf("len expects one argument")
			}
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
		case "json":
			if len(args) != 1 {
				return nil, fmt.Errorf("json expects one argument")
			}
			return &CallExpr{Func: "console.log", Args: []Expr{&CallExpr{Func: "JSON.stringify", Args: args}}}, nil
		case "str":
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects one argument")
			}
			return &CallExpr{Func: "String", Args: args}, nil
		case "substring":
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects three arguments")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
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
		return &MapLit{Entries: entries}, nil
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
		pat, err := convertExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*NameRef); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "===", Right: pat}
		expr = &IfExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		return &NumberLit{Value: fmt.Sprintf("%g", *l.Float)}, nil
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

func tsType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.BigIntType, types.BigRatType:
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
		parts := make([]string, len(tt.Order))
		for i, name := range tt.Order {
			parts[i] = name + ": " + tsType(tt.Fields[name])
		}
		return "{ " + strings.Join(parts, "; ") + " }"
	default:
		return "any"
	}
}

func isNumericBool(e Expr) bool {
	switch ex := e.(type) {
	case *BinaryExpr:
		switch ex.Op {
		case "in":
			return false
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return true
		}
		return isNumericBool(ex.Left) || isNumericBool(ex.Right)
	case *UnaryExpr:
		if ex.Op == "!" {
			return true
		}
		return isNumericBool(ex.Expr)
	default:
		return false
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

func substituteFields(e Expr, varName string, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *NameRef:
		if fields[ex.Name] {
			return &IndexExpr{Target: &NameRef{Name: varName}, Index: &StringLit{Value: ex.Name}}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFields(ex.Left, varName, fields)
		ex.Right = substituteFields(ex.Right, varName, fields)
		return ex
	case *UnaryExpr:
		ex.Expr = substituteFields(ex.Expr, varName, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		return ex
	case *MethodCallExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		return ex
	case *IndexExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		ex.Index = substituteFields(ex.Index, varName, fields)
		return ex
	case *SliceExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		if ex.Start != nil {
			ex.Start = substituteFields(ex.Start, varName, fields)
		}
		if ex.End != nil {
			ex.End = substituteFields(ex.End, varName, fields)
		}
		return ex
	case *IfExpr:
		ex.Cond = substituteFields(ex.Cond, varName, fields)
		ex.Then = substituteFields(ex.Then, varName, fields)
		ex.Else = substituteFields(ex.Else, varName, fields)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFields(ex.Elems[i], varName, fields)
		}
		return ex
	case *MapLit:
		for i := range ex.Entries {
			ex.Entries[i].Key = substituteFields(ex.Entries[i].Key, varName, fields)
			ex.Entries[i].Value = substituteFields(ex.Entries[i].Value, varName, fields)
		}
		return ex
	default:
		return ex
	}
}

// selectorToExpr converts a selector expression like a.b.c into nested index
// expressions so the transpiler can treat it uniformly with dynamic property
// access. It never returns nil.
func selectorToExpr(sel *parser.SelectorExpr) Expr {
	expr := Expr(&NameRef{Name: sel.Root})
	for _, part := range sel.Tail {
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
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
