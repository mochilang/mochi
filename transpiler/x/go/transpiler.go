//go:build slow

package gotranspiler

import (
	"bytes"
	"encoding/json"
	"fmt"
	"go/format"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"unicode"

	yaml "gopkg.in/yaml.v3"
	"mochi/ast"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

// Program represents a Go program consisting of a sequence of statements.
type Program struct {
	Stmts        []Stmt
	Imports      map[string]string
	UseStrings   bool
	UseStrconv   bool
	UsePrint     bool
	UseSort      bool
	UseJSON      bool
	UseTime      bool
	UseInput     bool
	UseSubstr    bool
	UseFloatConv bool
	UseBigInt    bool
}

var (
	usesStrings    bool
	usesStrconv    bool
	usesPrint      bool
	usesSort       bool
	usesJSON       bool
	usesTime       bool
	usesInput      bool
	usesSubstr     bool
	usesFloatConv  bool
	usesBigInt     bool
	topEnv         *types.Env
	extraDecls     []Stmt
	structCount    int
	imports        map[string]string
	currentRetType string
	mainFuncName   string
	fieldTypeGuess map[string]string
)

func toPascalCase(s string) string {
	parts := strings.Split(s, "_")
	for i, p := range parts {
		if len(p) == 0 {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
}

func structNameFromVar(name string) string {
	if strings.HasSuffix(name, "ies") && len(name) > 3 {
		name = name[:len(name)-3] + "y"
	} else if strings.HasSuffix(name, "s") && len(name) > 1 {
		name = name[:len(name)-1]
	}
	return toPascalCase(name)
}

var commonInitialisms = []string{"ID", "URL", "HTTP", "JSON", "XML", "SQL", "UID", "UUID"}

func toGoFieldName(name string) string {
	n := toPascalCase(name)
	for _, init := range commonInitialisms {
		lower := strings.ToLower(init)
		n = strings.ReplaceAll(n, strings.Title(lower), init)
	}
	return n
}

func isIdentifier(s string) bool {
	for i, r := range s {
		if i == 0 {
			if !unicode.IsLetter(r) && r != '_' {
				return false
			}
		} else {
			if !unicode.IsLetter(r) && !unicode.IsDigit(r) && r != '_' {
				return false
			}
		}
	}
	return len(s) > 0
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type StmtList struct{ List []Stmt }

func (sl *StmtList) emit(w io.Writer) {
	for i, s := range sl.List {
		if i > 0 {
			io.WriteString(w, "\n")
		}
		s.emit(w)
	}
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

// PrintStmt prints a value using Go's fmt package with Mochi semantics.
// Arguments may include helper expressions that format values as strings.
type PrintStmt struct{ Args []Expr }

func (p *PrintStmt) emit(w io.Writer) {
	io.WriteString(w, "fmt.Println(")
	for i, e := range p.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, ")")
}

type VarDecl struct {
	Name   string
	Type   string
	Value  Expr
	Global bool
}

func (v *VarDecl) emit(w io.Writer) {
	switch {
	case v.Value != nil && v.Type != "":
		fmt.Fprintf(w, "var %s %s = ", v.Name, v.Type)
		v.Value.emit(w)
	case v.Value != nil:
		if v.Global {
			fmt.Fprintf(w, "var %s = ", v.Name)
		} else {
			fmt.Fprintf(w, "%s := ", v.Name)
		}
		v.Value.emit(w)
	case v.Type != "":
		fmt.Fprintf(w, "var %s %s", v.Name, v.Type)
	default:
		fmt.Fprintf(w, "var %s", v.Name)
	}
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", a.Name)
	a.Value.emit(w)
}

// SetStmt assigns to an arbitrary indexed or field-select expression.
type SetStmt struct {
	Target Expr
	Value  Expr
}

func (s *SetStmt) emit(w io.Writer) {
	s.Target.emit(w)
	fmt.Fprint(w, " = ")
	s.Value.emit(w)
}

// IfStmt represents a simple if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break") }

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue") }

type ReturnStmt struct{ Value Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Value != nil {
		fmt.Fprint(w, " ")
		r.Value.emit(w)
	}
}

type ParamDecl struct {
	Name string
	Type string
}

// TypeDeclStmt declares a simple struct type.
type TypeDeclStmt struct {
	Name   string
	Fields []ParamDecl
}

func (t *TypeDeclStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "type %s struct {\n", t.Name)
	for _, f := range t.Fields {
		fmt.Fprintf(w, "    %s %s `json:%q`\n", toGoFieldName(f.Name), f.Type, f.Name)
	}
	fmt.Fprint(w, "}")
}

// UnionDeclStmt declares a simple union type as a Go interface and its variants.
type UnionDeclStmt struct {
	Name     string
	Variants []UnionVariant
}

type UnionVariant struct {
	Name   string
	Fields []ParamDecl
}

func (u *UnionDeclStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "type %s interface { is%s() }\n", u.Name, u.Name)
	for _, v := range u.Variants {
		fmt.Fprintf(w, "type %s struct {\n", v.Name)
		for _, f := range v.Fields {
			fmt.Fprintf(w, "    %s %s `json:%q`\n", toGoFieldName(f.Name), f.Type, f.Name)
		}
		fmt.Fprint(w, "}\n")
		fmt.Fprintf(w, "func (%s) is%s() {}\n", v.Name, u.Name)
	}
}

type FuncDecl struct {
	Name   string
	Params []ParamDecl
	Return string
	Body   []Stmt
}

func (fd *FuncDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "func %s(", fd.Name)
	for i, p := range fd.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s %s", p.Name, p.Type)
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if fd.Return != "" {
		fmt.Fprintf(w, " %s", fd.Return)
	}
	fmt.Fprint(w, " {\n")
	for _, st := range fd.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

type FuncLit struct {
	Params []ParamDecl
	Return string
	Body   []Stmt
}

func (fl *FuncLit) emit(w io.Writer) {
	fmt.Fprint(w, "func(")
	for i, p := range fl.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s %s", p.Name, p.Type)
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if fl.Return != "" {
		fmt.Fprintf(w, " %s", fl.Return)
	}
	fmt.Fprint(w, " {\n")
	for _, st := range fl.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
	Type string
}

func (ie *IfExpr) emit(w io.Writer) {
	ret := "any"
	if ie.Type != "" {
		ret = ie.Type
	}
	fmt.Fprintf(w, "func() %s {", ret)
	fmt.Fprint(w, "if ")
	ie.Cond.emit(w)
	fmt.Fprint(w, " { return ")
	ie.Then.emit(w)
	fmt.Fprint(w, " }")
	if ie.Else != nil {
		fmt.Fprint(w, " else { return ")
		ie.Else.emit(w)
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, " }()")
}

// UnionMatchExpr represents a type switch over a union.
type UnionMatchExpr struct {
	Target Expr
	Cases  []UnionMatchCase
	Type   string
}

type UnionMatchCase struct {
	Variant  string
	Fields   []string
	Bindings []string
	Body     Expr
}

func (m *UnionMatchExpr) emit(w io.Writer) {
	ret := "any"
	if m.Type != "" {
		ret = m.Type
	}
	fmt.Fprintf(w, "func() %s { switch uv := ", ret)
	m.Target.emit(w)
	fmt.Fprint(w, ".(type) {")
	for _, c := range m.Cases {
		fmt.Fprintf(w, "case %s:", c.Variant)
		for i, b := range c.Bindings {
			if b != "" && i < len(c.Fields) {
				if b == "_" {
					fmt.Fprintf(w, " _ = uv.%s;", c.Fields[i])
				} else {
					fmt.Fprintf(w, " %s := uv.%s;", b, c.Fields[i])
				}
			}
		}
		fmt.Fprint(w, " return ")
		c.Body.emit(w)
		fmt.Fprint(w, ";")
	}
	fmt.Fprintf(w, "default: var z %s; return z } }()", ret)
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if ")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	fmt.Fprint(w, " {\n")
	for _, s := range i.Then {
		fmt.Fprint(w, "    ")
		s.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, s := range i.Else {
			fmt.Fprint(w, "    ")
			s.emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "}")
	}
}

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

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) {
	if f.Value == float64(int(f.Value)) {
		fmt.Fprintf(w, "%.1f", f.Value)
	} else {
		fmt.Fprintf(w, "%g", f.Value)
	}
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprintf(w, "%t", b.Value) }

// NullLit represents the `null` literal which maps to Go's nil.
type NullLit struct{}

func (n *NullLit) emit(w io.Writer) { io.WriteString(w, "nil") }

// NotExpr represents a boolean negation.
type NotExpr struct{ Expr Expr }

func (n *NotExpr) emit(w io.Writer) {
	fmt.Fprint(w, "!")
	n.Expr.emit(w)
}

// MapLit represents a map literal.
type MapLit struct {
	KeyType   string
	ValueType string
	Keys      []Expr
	Values    []Expr
}

func updateMapLitTypes(ml *MapLit, t types.Type) {
	switch mt := t.(type) {
	case types.MapType:
		ml.KeyType = toGoTypeFromType(mt.Key)
		ml.ValueType = toGoTypeFromType(mt.Value)
		for _, v := range ml.Values {
			if inner, ok := v.(*MapLit); ok {
				updateMapLitTypes(inner, mt.Value)
			}
		}
	case types.StructType:
		ml.KeyType = "string"
		ml.ValueType = "any"
		for i, v := range ml.Values {
			if inner, ok := v.(*MapLit); ok {
				if ft, ok2 := mt.Fields[ml.Keys[i].(*StringLit).Value]; ok2 {
					updateMapLitTypes(inner, ft)
				}
			}
		}
	}
}

func (m *MapLit) emit(w io.Writer) {
	key := m.KeyType
	if key == "" {
		key = "string"
	}
	val := m.ValueType
	if val == "" {
		val = "any"
	}
	fmt.Fprintf(w, "map[%s]%s{", key, val)
	for i := range m.Keys {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		m.Keys[i].emit(w)
		fmt.Fprint(w, ": ")
		m.Values[i].emit(w)
	}
	fmt.Fprint(w, "}")
}

// StructLit represents a struct literal.
type StructLit struct {
	Name   string
	Fields []Expr
	Names  []string
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s{\n", s.Name)
	for i, f := range s.Fields {
		fmt.Fprintf(w, "    %s: ", toGoFieldName(s.Names[i]))
		f.emit(w)
		fmt.Fprint(w, ",\n")
	}
	fmt.Fprint(w, "}")
}

func (s *StructLit) emitBare(w io.Writer) {
	fmt.Fprint(w, "{\n")
	for i, f := range s.Fields {
		fmt.Fprintf(w, "    %s: ", toGoFieldName(s.Names[i]))
		f.emit(w)
		fmt.Fprint(w, ",\n")
	}
	fmt.Fprint(w, "}")
}

// IndexExpr represents `X[i]`.
type IndexExpr struct {
	X     Expr
	Index Expr
}

func (ix *IndexExpr) emit(w io.Writer) {
	ix.X.emit(w)
	fmt.Fprint(w, "[")
	if ix.Index != nil {
		ix.Index.emit(w)
	}
	fmt.Fprint(w, "]")
}

// FieldExpr represents `X.f`.
type FieldExpr struct {
	X    Expr
	Name string
}

func (fe *FieldExpr) emit(w io.Writer) {
	fe.X.emit(w)
	fmt.Fprintf(w, ".%s", toGoFieldName(fe.Name))
}

// SliceExpr represents `X[i:j]`.
type SliceExpr struct {
	X     Expr
	Start Expr
	End   Expr
}

func (sx *SliceExpr) emit(w io.Writer) {
	sx.X.emit(w)
	fmt.Fprint(w, "[")
	if sx.Start != nil {
		sx.Start.emit(w)
	}
	fmt.Fprint(w, ":")
	if sx.End != nil {
		sx.End.emit(w)
	}
	fmt.Fprint(w, "]")
}

// RuneSliceExpr represents `[]rune(expr)`.
type RuneSliceExpr struct{ Expr Expr }

func (rs *RuneSliceExpr) emit(w io.Writer) {
	fmt.Fprint(w, "[]rune(")
	rs.Expr.emit(w)
	fmt.Fprint(w, ")")
}

// WhileStmt represents a basic while loop using Go's for syntax.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "for ")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	}
	fmt.Fprint(w, " {\n")
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

// ForRangeStmt represents a numeric for-loop like `for i in a..b {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for %s := ", fr.Name)
	if fr.Start != nil {
		fr.Start.emit(w)
	}
	fmt.Fprintf(w, "; %s < ", fr.Name)
	if fr.End != nil {
		fr.End.emit(w)
	}
	fmt.Fprintf(w, "; %s++ {\n", fr.Name)
	for _, st := range fr.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

// ForEachStmt represents iteration over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	IsMap    bool
	KeyType  string
}

func (fe *ForEachStmt) emit(w io.Writer) {
	if fe.IsMap {
		usesSort = true
		fmt.Fprintf(w, "for _, %s := range func() []%s { keys := make([]%s, 0, len(", fe.Name, fe.KeyType, fe.KeyType)
		if fe.Iterable != nil {
			fe.Iterable.emit(w)
		}
		fmt.Fprint(w, "))\n")
		fmt.Fprint(w, "        for k := range ")
		if fe.Iterable != nil {
			fe.Iterable.emit(w)
		}
		fmt.Fprint(w, " { keys = append(keys, k) }\n")
		fmt.Fprint(w, "        sort.Slice(keys, func(i, j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) })\n")
		fmt.Fprint(w, "        return keys }()")
	} else {
		fmt.Fprintf(w, "for _, %s := range ", fe.Name)
		if fe.Iterable != nil {
			fe.Iterable.emit(w)
		}
	}
	fmt.Fprint(w, " {\n")
	for _, st := range fe.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

// IndexAssignStmt represents assignment to a list element.
type IndexAssignStmt struct {
	Name  string
	Index Expr
	Value Expr
}

func (ias *IndexAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s[", ias.Name)
	ias.Index.emit(w)
	fmt.Fprint(w, "] = ")
	ias.Value.emit(w)
}

// UpdateStmt updates fields of items in a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for i, item := range %s {\n", u.Target)
	if u.Cond != nil {
		fmt.Fprint(w, "    if ")
		u.Cond.emit(w)
		fmt.Fprint(w, " {\n")
	}
	indent := "    "
	if u.Cond != nil {
		indent = "        "
	}
	for i, f := range u.Fields {
		fmt.Fprintf(w, "%sitem.%s = ", indent, toGoFieldName(f))
		u.Values[i].emit(w)
		fmt.Fprint(w, "\n")
	}
	if u.Cond != nil {
		fmt.Fprint(w, "    }\n")
	}
	fmt.Fprintf(w, "    %s[i] = item\n", u.Target)
	fmt.Fprint(w, "}\n")
}

// SaveStmt writes a list of maps or structs to stdout as JSON lines.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		fmt.Fprint(w, "for _, _row := range ")
		if s.Src != nil {
			s.Src.emit(w)
		}
		fmt.Fprint(w, ` {
    b, _ := json.Marshal(_row)
    line := string(b)
    line = strings.ReplaceAll(line, ":", ": ")
    line = strings.ReplaceAll(line, ",", ", ")
    fmt.Println(line)
}
`)
		return
	}
	fmt.Fprint(w, "// unsupported save")
}

type ListLit struct {
	ElemType string
	Elems    []Expr
	Pretty   bool
}

func (l *ListLit) emit(w io.Writer) {
	if l.ElemType == "" {
		l.ElemType = "any"
	}
	fmt.Fprintf(w, "[]%s{", l.ElemType)
	if l.Pretty && len(l.Elems) > 0 {
		fmt.Fprint(w, "\n")
	}
	for i, e := range l.Elems {
		if l.Pretty {
			io.WriteString(w, "    ")
		} else if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if sl, ok := e.(*StructLit); ok && l.Pretty && sl.Name == l.ElemType {
			sl.emitBare(w)
		} else {
			e.emit(w)
		}
		if l.Pretty {
			fmt.Fprint(w, ",\n")
		}
	}
	if l.Pretty && len(l.Elems) > 0 {
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "}")
}

type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

// BigBinaryExpr represents arithmetic on big integers.
type BigBinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BigBinaryExpr) emit(w io.Writer) {
	switch b.Op {
	case "+":
		io.WriteString(w, "new(big.Int).Add(")
	case "-":
		io.WriteString(w, "new(big.Int).Sub(")
	case "*":
		io.WriteString(w, "new(big.Int).Mul(")
	case "/":
		io.WriteString(w, "new(big.Int).Div(")
	case "%":
		io.WriteString(w, "new(big.Int).Mod(")
	default:
		io.WriteString(w, "new(big.Int)")
		return
	}
	b.Left.emit(w)
	io.WriteString(w, ", ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

// BigCmpExpr compares two big integers with the given operator.
type BigCmpExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BigCmpExpr) emit(w io.Writer) {
	io.WriteString(w, "func() bool { return ")
	b.Left.emit(w)
	io.WriteString(w, ".Cmp(")
	b.Right.emit(w)
	io.WriteString(w, ") ")
	switch b.Op {
	case "<":
		io.WriteString(w, "< 0")
	case "<=":
		io.WriteString(w, "<= 0")
	case ">":
		io.WriteString(w, "> 0")
	case ">=":
		io.WriteString(w, ">= 0")
	case "==":
		io.WriteString(w, "== 0")
	case "!=":
		io.WriteString(w, "!= 0")
	}
	io.WriteString(w, " }()")
}

// BigIntToIntExpr converts a *big.Int to int via Int64().
type BigIntToIntExpr struct{ Value Expr }

func (b *BigIntToIntExpr) emit(w io.Writer) {
	io.WriteString(w, "int(")
	b.Value.emit(w)
	io.WriteString(w, ".Int64())")
}

type AvgExpr struct{ List Expr }

func (a *AvgExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() float64 { sum := 0; for _, n := range ")
	a.List.emit(w)
	fmt.Fprint(w, " { sum += n }; return float64(sum) / float64(len(")
	a.List.emit(w)
	fmt.Fprint(w, ") ) }()")
}

type SumExpr struct {
	List    Expr
	IsFloat bool
}

func (s *SumExpr) emit(w io.Writer) {
	if s.IsFloat {
		fmt.Fprint(w, "func() float64 { s := 0.0; for _, n := range ")
		s.List.emit(w)
		fmt.Fprint(w, " { s += n }; return s }()")
	} else {
		fmt.Fprint(w, "func() int { s := 0; for _, n := range ")
		s.List.emit(w)
		fmt.Fprint(w, " { s += n }; return s }()")
	}
}

type MinExpr struct{ List Expr }

func (m *MinExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() int { if len(")
	m.List.emit(w)
	fmt.Fprint(w, ") == 0 { return 0 }; m := ")
	m.List.emit(w)
	fmt.Fprint(w, "[0]; for _, n := range ")
	m.List.emit(w)
	fmt.Fprint(w, "[1:] { if n < m { m = n } }; return m }()")
}

type MaxExpr struct{ List Expr }

func (m *MaxExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() int { if len(")
	m.List.emit(w)
	fmt.Fprint(w, ") == 0 { return 0 }; m := ")
	m.List.emit(w)
	fmt.Fprint(w, "[0]; for _, n := range ")
	m.List.emit(w)
	fmt.Fprint(w, "[1:] { if n > m { m = n } }; return m }()")
}

// ValuesExpr collects all values from a map.
type ValuesExpr struct {
	Map       Expr
	ValueType string
}

// KeysExpr collects all keys from a map.
type KeysExpr struct {
	Map     Expr
	KeyType string
}

func (v *ValuesExpr) emit(w io.Writer) {
	usesSort = true
	fmt.Fprintf(w, "func() []%s { res := make([]%s, 0, len(", v.ValueType, v.ValueType)
	v.Map.emit(w)
	fmt.Fprint(w, ")) ; for _, val := range ")
	v.Map.emit(w)
	fmt.Fprint(w, " { res = append(res, val) } ; ")
	switch v.ValueType {
	case "int":
		fmt.Fprint(w, "sort.Ints(res); ")
	case "string":
		fmt.Fprint(w, "sort.Strings(res); ")
	default:
		fmt.Fprint(w, "sort.Slice(res, func(i,j int) bool { return fmt.Sprint(res[i]) < fmt.Sprint(res[j]) }); ")
	}
	fmt.Fprint(w, "return res }()")
}

func (k *KeysExpr) emit(w io.Writer) {
	usesSort = true
	fmt.Fprintf(w, "func() []%s { keys := make([]%s, 0, len(", k.KeyType, k.KeyType)
	k.Map.emit(w)
	fmt.Fprint(w, ")) ; for kx := range ")
	k.Map.emit(w)
	fmt.Fprint(w, " { keys = append(keys, kx) } ; ")
	switch k.KeyType {
	case "int":
		fmt.Fprint(w, "sort.Ints(keys); ")
	case "string":
		fmt.Fprint(w, "sort.Strings(keys); ")
	default:
		fmt.Fprint(w, "sort.Slice(keys, func(i,j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) }); ")
	}
	fmt.Fprint(w, "return keys }()")
}

// ListStringExpr converts a list to a string with Mochi style formatting.
type ListStringExpr struct{ List Expr }

func (ls *ListStringExpr) emit(w io.Writer) {
	usesJSON = true
	usesStrings = true
	io.WriteString(w, "func() string { b, _ := json.Marshal(")
	ls.List.emit(w)
	io.WriteString(w, `); s := string(b); s = strings.ReplaceAll(s, ":", ": "); s = strings.ReplaceAll(s, ",", ", "); s = strings.ReplaceAll(s, "}, {", "},{"); s = strings.ReplaceAll(s, "\"", "'"); return s }()`)
}

// StringJoinExpr joins a list of strings with spaces.
type StringJoinExpr struct{ List Expr }

func (sj *StringJoinExpr) emit(w io.Writer) {
	usesStrings = true
	io.WriteString(w, "strings.Join(")
	sj.List.emit(w)
	io.WriteString(w, ", \" \"")
	io.WriteString(w, ")")
}

// StructJSONExpr renders a struct as a JSON-like string.
type StructJSONExpr struct{ Value Expr }

func (se *StructJSONExpr) emit(w io.Writer) {
	usesJSON = true
	usesStrings = true
	io.WriteString(w, "func() string { b, _ := json.Marshal(")
	se.Value.emit(w)
	io.WriteString(w, `); s := string(b); s = strings.ReplaceAll(s, ":", ": "); s = strings.ReplaceAll(s, ",", ", "); s = strings.ReplaceAll(s, "}, {", "},{"); return s }()`)
}

// FloatStringExpr formats a float with a trailing decimal.
type FloatStringExpr struct{ Value Expr }

func (fs *FloatStringExpr) emit(w io.Writer) {
	io.WriteString(w, "func() string { f := float64(")
	fs.Value.emit(w)
	io.WriteString(w, "); if f == float64(int(f)) { return fmt.Sprintf(\"%.1f\", f) }; return fmt.Sprint(f) }()")
}

// NowExpr expands to a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

func (n *NowExpr) emit(w io.Writer) {
	io.WriteString(w, "_now()")
}

// InputExpr reads a line from standard input.
type InputExpr struct{}

func (i *InputExpr) emit(w io.Writer) {
	io.WriteString(w, "_input()")
}

// JsonExpr prints a value as pretty JSON.
type JsonExpr struct{ Value Expr }

func (je *JsonExpr) emit(w io.Writer) {
	usesJSON = true
	io.WriteString(w, "func() { b, _ := json.MarshalIndent(")
	je.Value.emit(w)
	io.WriteString(w, ", \"\", \"  \"); fmt.Println(string(b)) }()")
}

// BoolIntExpr converts a boolean to an integer 1 or 0.
type BoolIntExpr struct{ Expr Expr }

func (bi *BoolIntExpr) emit(w io.Writer) {
	io.WriteString(w, "func() int { if ")
	bi.Expr.emit(w)
	io.WriteString(w, " { return 1 }; return 0 }()")
}

// LookupHostExpr wraps net.LookupHost call and returns []any with address list and error.
type LookupHostExpr struct{ Arg Expr }

func (lh *LookupHostExpr) emit(w io.Writer) {
	io.WriteString(w, "func() []any { a, b := net.LookupHost(")
	lh.Arg.emit(w)
	io.WriteString(w, "); return []any{a, b} }()")
}

// IntCastExpr converts a value to int using Go's int() conversion.
type IntCastExpr struct{ Expr Expr }

func (ic *IntCastExpr) emit(w io.Writer) {
	io.WriteString(w, "int(")
	ic.Expr.emit(w)
	io.WriteString(w, ")")
}

// ExistsExpr represents the exists() builtin result to preserve boolean output.
type ExistsExpr struct{ Expr Expr }

func (e *ExistsExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	e.Expr.emit(w)
	io.WriteString(w, ")")
}

type ContainsExpr struct {
	Collection Expr
	Value      Expr
	Kind       string // list, map, or string
	ElemType   string
}

func (c *ContainsExpr) emit(w io.Writer) {
	switch c.Kind {
	case "string":
		usesStrings = true
		fmt.Fprint(w, "strings.Contains(")
		c.Collection.emit(w)
		fmt.Fprint(w, ", ")
		c.Value.emit(w)
		fmt.Fprint(w, ")")
	case "map":
		fmt.Fprint(w, "func() bool { _, ok := ")
		c.Collection.emit(w)
		fmt.Fprint(w, "[")
		c.Value.emit(w)
		fmt.Fprint(w, "]; return ok }()")
	default: // list
		fmt.Fprint(w, "func() bool { for _, v := range ")
		c.Collection.emit(w)
		fmt.Fprint(w, " { if v == ")
		c.Value.emit(w)
		fmt.Fprint(w, " { return true } } ; return false }()")
	}
}

type UnionExpr struct {
	Left, Right Expr
	ElemType    string
}

func (u *UnionExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { m := map[%s]struct{}{}; res := []%s{}; for _, v := range ", u.ElemType, u.ElemType, u.ElemType)
	u.Left.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; !ok { m[v] = struct{}{}; res = append(res, v) } }; for _, v := range ")
	u.Right.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; !ok { m[v] = struct{}{}; res = append(res, v) } }; return res }()")
}

type UnionAllExpr struct {
	Left, Right Expr
	ElemType    string
}

func (u *UnionAllExpr) emit(w io.Writer) {
	et := u.ElemType
	if et == "" {
		et = "any"
	}
	fmt.Fprintf(w, "func() []%s { res := make([]%s, len(", et, et)
	u.Left.emit(w)
	fmt.Fprint(w, ")); copy(res, ")
	u.Left.emit(w)
	fmt.Fprint(w, "); res = append(res, ")
	u.Right.emit(w)
	fmt.Fprint(w, "...); return res }()")
}

type ExceptExpr struct {
	Left, Right Expr
	ElemType    string
}

func (e *ExceptExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { m := map[%s]struct{}{}; ", e.ElemType, e.ElemType)
	fmt.Fprint(w, "for _, v := range ")
	e.Right.emit(w)
	fmt.Fprint(w, " { m[v] = struct{}{} }; res := []")
	fmt.Fprint(w, e.ElemType)
	fmt.Fprint(w, "{}; for _, v := range ")
	e.Left.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; !ok { res = append(res, v) } }; return res }()")
}

type IntersectExpr struct {
	Left, Right Expr
	ElemType    string
}

func (i *IntersectExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { m := map[%s]struct{}{}; for _, v := range ", i.ElemType, i.ElemType)
	i.Left.emit(w)
	fmt.Fprint(w, " { m[v] = struct{}{} }; res := []")
	fmt.Fprint(w, i.ElemType)
	fmt.Fprint(w, "{}; for _, v := range ")
	i.Right.emit(w)
	fmt.Fprint(w, " { if _, ok := m[v]; ok { res = append(res, v) } }; return res }()")
}

type queryFrom struct {
	Var string
	Src Expr
}

type queryJoin struct {
	Var  string
	Src  Expr
	On   Expr
	Side string // "", "left", "right", "outer"
}

type QueryExpr struct {
	Var      string
	Src      Expr
	Froms    []queryFrom
	Joins    []queryJoin
	Where    Expr
	Sort     Expr
	SortType string
	Skip     Expr
	Take     Expr
	Select   Expr
	ElemType string
}

// GroupQueryExpr represents a simple `group by` query without joins or sorting.
type GroupQueryExpr struct {
	Var       string
	Src       Expr
	Key       Expr
	GroupVar  string
	Cond      Expr
	Select    Expr
	Having    Expr
	ElemType  string
	GroupType string
	ItemType  string
	KeyType   string
	Sort      Expr
	SortType  string
}

// GroupJoinQueryExpr represents a `group by` query that may include joins.
type GroupJoinQueryExpr struct {
	Var        string
	Src        Expr
	Froms      []queryFrom
	Joins      []queryJoin
	Where      Expr
	Key        Expr
	GroupVar   string
	Select     Expr
	Having     Expr
	ElemType   string
	ItemType   string
	KeyType    string
	Sort       Expr
	SortType   string
	Vars       []string
	SimpleItem bool
}

// OuterJoinExpr represents a simple full outer join between two sources.
type OuterJoinExpr struct {
	LeftVar   string
	LeftSrc   Expr
	RightVar  string
	RightSrc  Expr
	Cond      Expr
	Select    Expr
	ElemType  string
	LeftType  string
	RightType string
}

func (q *QueryExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s { ", q.ElemType)
	if q.Sort != nil {
		fmt.Fprintf(w, "type pair struct { Key %s; Val %s }; pairs := []pair{}; ", q.SortType, q.ElemType)
	} else {
		fmt.Fprintf(w, "res := []%s{}; ", q.ElemType)
	}
	if len(q.Joins) == 1 && q.Joins[0].Side == "right" && len(q.Froms) == 0 {
		j := q.Joins[0]
		fmt.Fprintf(w, "for _, %s := range ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, " {")
		fmt.Fprintf(w, " for _, %s := range ", q.Var)
		q.Src.emit(w)
		fmt.Fprint(w, " {")
		fmt.Fprint(w, " if ")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, " {")
	} else {
		fmt.Fprintf(w, "for _, %s := range ", q.Var)
		q.Src.emit(w)
		fmt.Fprint(w, " {")
		for _, f := range q.Froms {
			fmt.Fprintf(w, " for _, %s := range ", f.Var)
			f.Src.emit(w)
			fmt.Fprint(w, " {")
		}
		for _, j := range q.Joins {
			fmt.Fprintf(w, " for _, %s := range ", j.Var)
			j.Src.emit(w)
			fmt.Fprint(w, " {")
			fmt.Fprint(w, " if ")
			if j.On != nil {
				j.On.emit(w)
			} else {
				fmt.Fprint(w, "true")
			}
			fmt.Fprint(w, " {")
		}
	}
	if q.Where != nil {
		fmt.Fprint(w, " if ")
		q.Where.emit(w)
		fmt.Fprint(w, " {")
	}
	if q.Sort != nil {
		fmt.Fprint(w, " pairs = append(pairs, pair{")
		q.Sort.emit(w)
		fmt.Fprint(w, ", ")
		q.Select.emit(w)
		fmt.Fprint(w, "})")
	} else {
		fmt.Fprint(w, " res = append(res, ")
		q.Select.emit(w)
		fmt.Fprint(w, ")")
	}
	if q.Where != nil {
		fmt.Fprint(w, " }")
	}
	if len(q.Joins) == 1 && q.Joins[0].Side == "right" && len(q.Froms) == 0 {
		fmt.Fprint(w, " }") // if
		fmt.Fprint(w, " }") // inner for
		fmt.Fprint(w, " }") // outer for
	} else {
		for range q.Joins {
			fmt.Fprint(w, " }")
			fmt.Fprint(w, " }")
		}
		for range q.Froms {
			fmt.Fprint(w, " }")
		}
		fmt.Fprint(w, " }")
	}
	if q.Sort != nil {
		cmp := "pairs[i].Key < pairs[j].Key"
		if q.SortType == "any" {
			cmp = "fmt.Sprint(pairs[i].Key) < fmt.Sprint(pairs[j].Key)"
		}
		fmt.Fprintf(w, " ; sort.Slice(pairs, func(i,j int) bool { return %s })", cmp)
		fmt.Fprintf(w, "; res := make([]%s, len(pairs)); for i, p := range pairs { res[i] = p.Val }", q.ElemType)
	}
	if q.Skip != nil {
		fmt.Fprint(w, "; if ")
		q.Skip.emit(w)
		fmt.Fprint(w, " < len(res) { res = res[")
		q.Skip.emit(w)
		fmt.Fprint(w, ":] } else { res = []")
		fmt.Fprint(w, q.ElemType)
		fmt.Fprint(w, "{} }")
	}
	if q.Take != nil {
		fmt.Fprint(w, "; if ")
		q.Take.emit(w)
		fmt.Fprint(w, " < len(res) { res = res[:")
		q.Take.emit(w)
		fmt.Fprint(w, "] }")
	}
	fmt.Fprint(w, "; return res }()")
}

func (g *GroupQueryExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s {\n", g.ElemType)
	fmt.Fprintf(w, "groups := map[string]%s{}\n", g.GroupType)
	fmt.Fprint(w, "order := []string{}\n")
	fmt.Fprintf(w, "for _, %s := range ", g.Var)
	g.Src.emit(w)
	fmt.Fprint(w, " {\n")
	if g.Cond != nil {
		fmt.Fprint(w, "if ")
		g.Cond.emit(w)
		fmt.Fprint(w, " {\n")
	}
	fmt.Fprint(w, "k := fmt.Sprint(")
	g.Key.emit(w)
	fmt.Fprint(w, ")\n")
	fmt.Fprintf(w, "grp, ok := groups[k]\nif !ok {\n    grp = %s{Key: ", g.GroupType)
	g.Key.emit(w)
	fmt.Fprintf(w, ", Items: []%s{}}\n    groups[k] = grp\n    order = append(order, k)\n}\n", g.ItemType)
	fmt.Fprintf(w, "grp.Items = append(grp.Items, %s)\ngroups[k] = grp\n", g.Var)
	if g.Cond != nil {
		fmt.Fprint(w, "}\n")
	}
	fmt.Fprint(w, "}\n")
	if g.Sort != nil {
		fmt.Fprintf(w, "type pair struct { Key %s; Val %s }\n", g.SortType, g.ElemType)
		fmt.Fprint(w, "pairs := []pair{}\n")
	} else {
		fmt.Fprintf(w, "res := []%s{}\n", g.ElemType)
	}
	fmt.Fprint(w, "for _, k := range order {\n")
	fmt.Fprintf(w, "%s := groups[k]\n", g.GroupVar)
	if g.Having != nil {
		fmt.Fprint(w, "if ")
		g.Having.emit(w)
		fmt.Fprint(w, " {\n    ")
		if g.Sort != nil {
			fmt.Fprint(w, "pairs = append(pairs, pair{")
			g.Sort.emit(w)
			fmt.Fprint(w, ", ")
			g.Select.emit(w)
			fmt.Fprint(w, "})\n")
		} else {
			fmt.Fprint(w, "res = append(res, ")
			g.Select.emit(w)
			fmt.Fprint(w, ")\n")
		}
		fmt.Fprint(w, "}\n")
	} else {
		if g.Sort != nil {
			fmt.Fprint(w, "    pairs = append(pairs, pair{")
			g.Sort.emit(w)
			fmt.Fprint(w, ", ")
			g.Select.emit(w)
			fmt.Fprint(w, "})\n")
		} else {
			fmt.Fprint(w, "    res = append(res, ")
			g.Select.emit(w)
			fmt.Fprint(w, ")\n")
		}
	}
	fmt.Fprint(w, "}\n")
	if g.Sort != nil {
		cmp := "pairs[i].Key < pairs[j].Key"
		if g.SortType == "any" {
			cmp = "fmt.Sprint(pairs[i].Key) < fmt.Sprint(pairs[j].Key)"
		}
		fmt.Fprintf(w, "sort.Slice(pairs, func(i,j int) bool { return %s })\n", cmp)
		fmt.Fprintf(w, "res := make([]%s, len(pairs))\n", g.ElemType)
		fmt.Fprint(w, "for i, p := range pairs { res[i] = p.Val }\n")
	}
	fmt.Fprint(w, "return res }()")
}

func (g *GroupJoinQueryExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s {\n", g.ElemType)
	fmt.Fprintf(w, "groups := map[string]struct{Key %s; Items []%s}{}\n", g.KeyType, g.ItemType)
	fmt.Fprint(w, "order := []string{}\n")
	fmt.Fprintf(w, "for _, %s := range ", g.Var)
	g.Src.emit(w)
	fmt.Fprint(w, " {")
	if len(g.Joins) == 1 && g.Joins[0].Side == "left" && len(g.Froms) == 0 {
		fmt.Fprint(w, "\n    matched := false\n")
	}
	for _, f := range g.Froms {
		fmt.Fprintf(w, " for _, %s := range ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, " {")
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, " for _, %s := range ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, " {")
		fmt.Fprint(w, " if ")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, " {")
		if j.Side != "" && j.Side == "left" {
			fmt.Fprint(w, "matched = true; ")
		}
	}
	if g.Where != nil {
		fmt.Fprint(w, " if ")
		g.Where.emit(w)
		fmt.Fprint(w, " {")
	}
	fmt.Fprint(w, " k := fmt.Sprint(")
	g.Key.emit(w)
	fmt.Fprint(w, ")\n")
	fmt.Fprintf(w, "grp, ok := groups[k]\nif !ok {\n    grp = struct{Key %s; Items []%s}{Key: ", g.KeyType, g.ItemType)
	g.Key.emit(w)
	fmt.Fprintf(w, ", Items: []%s{}}\n    groups[k] = grp\n    order = append(order, k)\n}\n", g.ItemType)
	if g.SimpleItem {
		fmt.Fprintf(w, "grp.Items = append(grp.Items, %s)\n", g.Vars[0])
		fmt.Fprint(w, "groups[k] = grp")
	} else {
		fmt.Fprintf(w, "grp.Items = append(grp.Items, %s{", g.ItemType)
		for i, v := range g.Vars {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			fmt.Fprintf(w, "%s: %s", toGoFieldName(v), v)
		}
		fmt.Fprint(w, "})\n")
		fmt.Fprint(w, "groups[k] = grp")
	}
	if len(g.Joins) == 1 && g.Joins[0].Side == "left" && len(g.Froms) == 0 {
		fmt.Fprintf(w, "\n    if !matched { grp.Items = append(grp.Items, %s{}) }", g.ItemType)
	}
	if g.Where != nil {
		fmt.Fprint(w, " }")
	}
	for range g.Joins {
		fmt.Fprint(w, " }")
		fmt.Fprint(w, " }")
	}
	for range g.Froms {
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, " }\n")
	if g.Sort != nil {
		fmt.Fprintf(w, "type pair struct { Key %s; Val %s }\n", g.SortType, g.ElemType)
		fmt.Fprint(w, "pairs := []pair{}\n")
	} else {
		fmt.Fprintf(w, "res := []%s{}\n", g.ElemType)
	}
	fmt.Fprint(w, "for _, k := range order {\n")
	fmt.Fprintf(w, "%s := groups[k]\n", g.GroupVar)
	if g.Having != nil {
		fmt.Fprint(w, "if ")
		g.Having.emit(w)
		fmt.Fprint(w, " {\n    ")
		if g.Sort != nil {
			fmt.Fprint(w, "pairs = append(pairs, pair{")
			g.Sort.emit(w)
			fmt.Fprint(w, ", ")
			g.Select.emit(w)
			fmt.Fprint(w, "})\n")
		} else {
			fmt.Fprint(w, "res = append(res, ")
			g.Select.emit(w)
			fmt.Fprint(w, ")\n")
		}
		fmt.Fprint(w, "}\n")
	} else {
		if g.Sort != nil {
			fmt.Fprint(w, "    pairs = append(pairs, pair{")
			g.Sort.emit(w)
			fmt.Fprint(w, ", ")
			g.Select.emit(w)
			fmt.Fprint(w, "})\n")
		} else {
			fmt.Fprint(w, "    res = append(res, ")
			g.Select.emit(w)
			fmt.Fprint(w, ")\n")
		}
	}
	fmt.Fprint(w, "}\n")
	if g.Sort != nil {
		cmp := "pairs[i].Key < pairs[j].Key"
		if g.SortType == "any" {
			cmp = "fmt.Sprint(pairs[i].Key) < fmt.Sprint(pairs[j].Key)"
		}
		fmt.Fprintf(w, "sort.Slice(pairs, func(i,j int) bool { return %s })\n", cmp)
		fmt.Fprintf(w, "res := make([]%s, len(pairs))\n", g.ElemType)
		fmt.Fprint(w, "for i, p := range pairs { res[i] = p.Val }\n")
	}
	fmt.Fprint(w, "return res }()")
}

func (o *OuterJoinExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "func() []%s {\n", o.ElemType)
	fmt.Fprintf(w, "res := []%s{}\n", o.ElemType)
	fmt.Fprintf(w, "for _, %sVal := range ", o.LeftVar)
	o.LeftSrc.emit(w)
	fmt.Fprint(w, " {\n")
	fmt.Fprintf(w, "    %s := &%sVal\n", o.LeftVar, o.LeftVar)
	fmt.Fprint(w, "    matched := false\n")
	fmt.Fprintf(w, "    for _, %sVal := range ", o.RightVar)
	o.RightSrc.emit(w)
	fmt.Fprint(w, " {\n")
	fmt.Fprintf(w, "        %s := &%sVal\n", o.RightVar, o.RightVar)
	fmt.Fprint(w, "        if ")
	if o.Cond != nil {
		o.Cond.emit(w)
	} else {
		fmt.Fprint(w, "true")
	}
	fmt.Fprint(w, " {\n            matched = true\n            res = append(res, ")
	o.Select.emit(w)
	fmt.Fprint(w, ")\n        }\n    }\n")
	fmt.Fprintf(w, "    if !matched {\n        var %s %s = nil\n        res = append(res, ", o.RightVar, o.RightType)
	o.Select.emit(w)
	fmt.Fprint(w, ")\n    }\n")
	fmt.Fprint(w, "}\n")
	fmt.Fprintf(w, "for _, %sVal := range ", o.RightVar)
	o.RightSrc.emit(w)
	fmt.Fprint(w, " {\n")
	fmt.Fprintf(w, "    %s := &%sVal\n", o.RightVar, o.RightVar)
	fmt.Fprint(w, "    exists := false\n")
	fmt.Fprintf(w, "    for _, %sVal2 := range ", o.LeftVar)
	o.LeftSrc.emit(w)
	fmt.Fprint(w, " {\n")
	fmt.Fprintf(w, "        %s := &%sVal2\n", o.LeftVar, o.LeftVar)
	fmt.Fprint(w, "        if ")
	if o.Cond != nil {
		o.Cond.emit(w)
	} else {
		fmt.Fprint(w, "true")
	}
	fmt.Fprint(w, " {\n            exists = true\n            break\n        }\n    }\n")
	fmt.Fprintf(w, "    if !exists {\n        var %s %s = nil\n        res = append(res, ", o.LeftVar, o.LeftType)
	o.Select.emit(w)
	fmt.Fprint(w, ")\n    }\n")
	fmt.Fprint(w, "}\nreturn res }()")
}

type AtoiExpr struct{ Expr Expr }

func (a *AtoiExpr) emit(w io.Writer) {
	fmt.Fprint(w, "func() int { n, _ := strconv.Atoi(")
	a.Expr.emit(w)
	fmt.Fprint(w, "); return n }()")
}

// AssertExpr performs a type assertion on the wrapped expression.
type AssertExpr struct {
	Expr Expr
	Type string
}

func (a *AssertExpr) emit(w io.Writer) {
	a.Expr.emit(w)
	fmt.Fprintf(w, ".(%s)", a.Type)
}

// Transpile converts a Mochi program to a minimal Go AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	usesStrings = false
	usesStrconv = false
	usesPrint = false
	usesSort = false
	usesJSON = false
	usesTime = false
	usesInput = false
	usesSubstr = false
	usesFloatConv = false
	usesBigInt = false
	topEnv = env
	extraDecls = nil
	structCount = 0
	mainFuncName = ""
	fieldTypeGuess = map[string]string{}
	imports = map[string]string{}
	gp := &Program{}
	for _, stmt := range p.Statements {
		s, err := compileStmt(stmt, env)
		if err != nil {
			return nil, err
		}
		if s != nil {
			gp.Stmts = append(gp.Stmts, s)
		}
		if len(extraDecls) > 0 {
			gp.Stmts = append(gp.Stmts, extraDecls...)
			extraDecls = nil
		}
	}
	_ = env // reserved for future use
	gp.UseStrings = usesStrings
	gp.UseStrconv = usesStrconv
	gp.UsePrint = usesPrint
	gp.UseSort = usesSort
	gp.UseJSON = usesJSON
	gp.UseTime = usesTime
	gp.UseInput = usesInput
	gp.UseSubstr = usesSubstr
	gp.UseFloatConv = usesFloatConv
	gp.UseBigInt = usesBigInt
	gp.Imports = imports
	return gp, nil
}

func compileExpr(e *parser.Expr, env *types.Env, base string) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	if e.Binary != nil {
		return compileBinary(e.Binary, env, base)
	}
	return nil, fmt.Errorf("unsupported expression")
}

func compileStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Expr != nil:
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			src, err := compileExpr(se.Src, env, "")
			if err != nil {
				return nil, err
			}
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			format := parseFormat(se.With)
			usesPrint = true
			usesJSON = true
			usesStrings = true
			return &SaveStmt{Src: src, Path: path, Format: format}, nil
		}
		if call := extractCall(st.Expr.Expr); call != nil && call.Func == "print" {
			args := make([]Expr, len(call.Args))
			needStrings := false
			for i, a := range call.Args {
				ex, err := compileExpr(a, env, "")
				if err != nil {
					return nil, err
				}
				t := types.TypeOfExpr(a, env)
				switch tt := t.(type) {
				case types.ListType:
					needStrings = true
					if _, ok2 := tt.Elem.(types.StringType); ok2 {
						ex = &StringJoinExpr{List: ex}
					} else {
						usesJSON = true
						ex = &ListStringExpr{List: ex}
					}
				case types.StructType:
					needStrings = true
					usesJSON = true
					ex = &StructJSONExpr{Value: ex}
				case types.FloatType:
					ex = &FloatStringExpr{Value: ex}
				case types.BoolType:
					switch ex.(type) {
					case *ContainsExpr, *ExistsExpr, *VarRef, *BoolLit:
						// keep boolean text
					default:
						ex = &BoolIntExpr{Expr: ex}
					}
				}
				args[i] = ex
			}
			usesPrint = true
			if needStrings {
				usesStrings = true
			}
			return &PrintStmt{Args: args}, nil
		}
		e, err := compileExpr(st.Expr.Expr, env, "")
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var typ string
		var declaredType types.Type
		if st.Let.Type != nil {
			typ = toGoType(st.Let.Type, env)
			declaredType = types.ResolveTypeRef(st.Let.Type, env)
		} else if env == topEnv {
			if t, err := env.GetVar(st.Let.Name); err == nil {
				if _, ok := t.(types.FuncType); !ok {
					typ = toGoTypeFromType(t)
					declaredType = t
				}
			}
		}
		if st.Let.Value != nil {
			e, err := compileExpr(st.Let.Value, env, st.Let.Name)
			if err != nil {
				return nil, err
			}
			if ml, ok := e.(*MapLit); ok {
				if t, err := env.GetVar(st.Let.Name); err == nil {
					updateMapLitTypes(ml, t)
				} else if declaredType != nil {
					updateMapLitTypes(ml, declaredType)
				}
			}
			var valType types.Type
			if typ == "" {
				valType = types.TypeOfExpr(st.Let.Value, env)
				if types.IsAnyType(valType) {
					valType = types.TypeOfExprBasic(st.Let.Value, env)
				}
				typ = toGoTypeFromType(valType)
				if _, ok := valType.(types.FuncType); ok {
					typ = ""
				}
			} else {
				valType = types.TypeOfExpr(st.Let.Value, env)
			}
			if ll, ok := e.(*ListLit); ok {
				if st.Let.Type != nil && st.Let.Type.Generic != nil && st.Let.Type.Generic.Name == "list" && len(st.Let.Type.Generic.Args) == 1 {
					ll.ElemType = toGoType(st.Let.Type.Generic.Args[0], env)
					typ = "[]" + ll.ElemType
				} else if ll.ElemType != "" && ll.ElemType != "any" {
					if st.Let.Type == nil {
						typ = "[]" + ll.ElemType
					}
				}
			}
			if qe, ok := e.(*QueryExpr); ok && qe.ElemType != "" {
				typ = "[]" + qe.ElemType
				if stype, ok := topEnv.GetStruct(qe.ElemType); ok {
					if env == topEnv {
						env.SetVarDeep(st.Let.Name, types.ListType{Elem: stype}, false)
					} else {
						env.SetVar(st.Let.Name, types.ListType{Elem: stype}, false)
					}
				}
			}
			if gq, ok := e.(*GroupQueryExpr); ok && gq.ElemType != "" {
				typ = "[]" + gq.ElemType
				if stype, ok := topEnv.GetStruct(gq.ElemType); ok {
					if env == topEnv {
						env.SetVarDeep(st.Let.Name, types.ListType{Elem: stype}, false)
					} else {
						env.SetVar(st.Let.Name, types.ListType{Elem: stype}, false)
					}
				}
			}
			if gj, ok := e.(*GroupJoinQueryExpr); ok && gj.ElemType != "" {
				typ = "[]" + gj.ElemType
				if stype, ok := topEnv.GetStruct(gj.ElemType); ok {
					if env == topEnv {
						env.SetVarDeep(st.Let.Name, types.ListType{Elem: stype}, false)
					} else {
						env.SetVar(st.Let.Name, types.ListType{Elem: stype}, false)
					}
				}
			}
			if oj, ok := e.(*OuterJoinExpr); ok && oj.ElemType != "" {
				typ = "[]" + oj.ElemType
				if stype, ok := topEnv.GetStruct(oj.ElemType); ok {
					if env == topEnv {
						env.SetVarDeep(st.Let.Name, types.ListType{Elem: stype}, false)
					} else {
						env.SetVar(st.Let.Name, types.ListType{Elem: stype}, false)
					}
				}
			}
			if ml, ok := e.(*MapLit); ok {
				if _, ok := valType.(types.StructType); ok && typ == "" {
					if mla := mapLiteralExpr(st.Let.Value); mla != nil {
						invalid := false
						var vt types.Type
						for i, it := range mla.Items {
							key, ok2 := types.SimpleStringKey(it.Key)
							if !ok2 || !isIdentifier(key) {
								invalid = true
							}
							t := types.ExprType(it.Value, env)
							if i == 0 {
								vt = t
							} else if !types.EqualTypes(vt, t) {
								vt = types.AnyType{}
							}
						}
						if invalid {
							if vt == nil {
								vt = types.AnyType{}
							}
							valType = types.MapType{Key: types.StringType{}, Value: vt}
							typ = toGoTypeFromType(valType)
							updateMapLitTypes(ml, valType)
						}
					}
				}
			}
			if typ != "" && typ != "any" && types.IsAnyType(valType) {
				e = &AssertExpr{Expr: e, Type: typ}
			} else if ae, ok := e.(*AssertExpr); ok && typ != "" && ae.Type == typ {
				e = ae.Expr
			}
			if typ != "" && typ != "any" && types.IsAnyType(valType) {
				e = &AssertExpr{Expr: e, Type: typ}
			} else if typ == "*big.Int" {
				e = ensureBigIntExpr(e, valType)
			}
			if declaredType != nil {
				if env == topEnv {
					env.SetVarDeep(st.Let.Name, declaredType, false)
				} else {
					env.SetVar(st.Let.Name, declaredType, false)
				}
			} else if valType != nil {
				if env == topEnv {
					env.SetVarDeep(st.Let.Name, valType, false)
				} else {
					env.SetVar(st.Let.Name, valType, false)
				}
			}
			global := env == topEnv
			if global {
				switch e.(type) {
				case *QueryExpr, *GroupQueryExpr, *GroupJoinQueryExpr, *OuterJoinExpr:
					global = false
				}
			}
			vd := &VarDecl{Name: st.Let.Name, Type: typ, Value: e, Global: global}
			if vd.Global && vd.Value != nil {
				extraDecls = append(extraDecls, &AssignStmt{Name: vd.Name, Value: vd.Value})
				vd.Value = nil
			}
			if env != topEnv {
				return &StmtList{List: []Stmt{vd, &AssignStmt{Name: "_", Value: &VarRef{Name: st.Let.Name}}}}, nil
			}
			return vd, nil
		}
		return &VarDecl{Name: st.Let.Name, Type: typ, Global: env == topEnv}, nil
	case st.Var != nil:
		var typ string
		var declaredType types.Type
		if st.Var.Type != nil {
			typ = toGoType(st.Var.Type, env)
			declaredType = types.ResolveTypeRef(st.Var.Type, env)
		} else if env == topEnv {
			if t, err := env.GetVar(st.Var.Name); err == nil {
				if _, ok := t.(types.FuncType); !ok {
					typ = toGoTypeFromType(t)
					declaredType = t
				}
			}
		}
		if st.Var.Value != nil {
			e, err := compileExpr(st.Var.Value, env, st.Var.Name)
			if err != nil {
				return nil, err
			}
			if ml, ok := e.(*MapLit); ok {
				if t, err := env.GetVar(st.Var.Name); err == nil {
					updateMapLitTypes(ml, t)
				} else if declaredType != nil {
					updateMapLitTypes(ml, declaredType)
				}
			}
			var valType types.Type
			if typ == "" {
				valType = types.TypeOfExpr(st.Var.Value, env)
				if types.IsAnyType(valType) {
					valType = types.TypeOfExprBasic(st.Var.Value, env)
				}
				typ = toGoTypeFromType(valType)
				if _, ok := valType.(types.FuncType); ok {
					typ = ""
				}
			} else {
				valType = types.TypeOfExpr(st.Var.Value, env)
			}
			if ll, ok := e.(*ListLit); ok {
				if st.Var.Type != nil && st.Var.Type.Generic != nil && st.Var.Type.Generic.Name == "list" && len(st.Var.Type.Generic.Args) == 1 {
					ll.ElemType = toGoType(st.Var.Type.Generic.Args[0], env)
					typ = "[]" + ll.ElemType
				} else if ll.ElemType != "" && ll.ElemType != "any" {
					if st.Var.Type == nil {
						typ = "[]" + ll.ElemType
					}
				}
			}
			if qe, ok := e.(*QueryExpr); ok && qe.ElemType != "" {
				typ = "[]" + qe.ElemType
				if stype, ok := topEnv.GetStruct(qe.ElemType); ok {
					if env == topEnv {
						env.SetVarDeep(st.Var.Name, types.ListType{Elem: stype}, true)
					} else {
						env.SetVar(st.Var.Name, types.ListType{Elem: stype}, true)
					}
				}
			}
			if gj, ok := e.(*GroupJoinQueryExpr); ok && gj.ElemType != "" {
				typ = "[]" + gj.ElemType
				if stype, ok := topEnv.GetStruct(gj.ElemType); ok {
					if env == topEnv {
						env.SetVarDeep(st.Var.Name, types.ListType{Elem: stype}, true)
					} else {
						env.SetVar(st.Var.Name, types.ListType{Elem: stype}, true)
					}
				}
			}
			if oj, ok := e.(*OuterJoinExpr); ok && oj.ElemType != "" {
				typ = "[]" + oj.ElemType
				if stype, ok := topEnv.GetStruct(oj.ElemType); ok {
					if env == topEnv {
						env.SetVarDeep(st.Var.Name, types.ListType{Elem: stype}, true)
					} else {
						env.SetVar(st.Var.Name, types.ListType{Elem: stype}, true)
					}
				}
			}
			if typ == "*big.Int" {
				e = ensureBigIntExpr(e, valType)
			}
			if declaredType != nil {
				if env == topEnv {
					env.SetVarDeep(st.Var.Name, declaredType, true)
				} else {
					env.SetVar(st.Var.Name, declaredType, true)
				}
			} else if valType != nil {
				if env == topEnv {
					env.SetVarDeep(st.Var.Name, valType, true)
				} else {
					env.SetVar(st.Var.Name, valType, true)
				}
			}
			global := env == topEnv
			if global {
				switch e.(type) {
				case *QueryExpr, *GroupQueryExpr, *GroupJoinQueryExpr, *OuterJoinExpr:
					global = false
				}
			}
			vd := &VarDecl{Name: st.Var.Name, Type: typ, Value: e, Global: global}
			if vd.Global && vd.Value != nil {
				extraDecls = append(extraDecls, &AssignStmt{Name: vd.Name, Value: vd.Value})
				vd.Value = nil
			}
			if env != topEnv {
				return &StmtList{List: []Stmt{vd, &AssignStmt{Name: "_", Value: &VarRef{Name: st.Var.Name}}}}, nil
			}
			return vd, nil
		}
		return &VarDecl{Name: st.Var.Name, Type: typ, Global: env == topEnv}, nil
	case st.Type != nil:
		if len(st.Type.Variants) > 0 {
			if _, ok := env.GetUnion(st.Type.Name); ok {
				variants := make([]UnionVariant, 0, len(st.Type.Variants))
				for _, v := range st.Type.Variants {
					stype, ok := env.GetStruct(v.Name)
					if !ok {
						continue
					}
					fields := make([]ParamDecl, len(stype.Order))
					for i, n := range stype.Order {
						fields[i] = ParamDecl{Name: n, Type: toGoTypeFromType(stype.Fields[n])}
					}
					variants = append(variants, UnionVariant{Name: v.Name, Fields: fields})
				}
				return &UnionDeclStmt{Name: st.Type.Name, Variants: variants}, nil
			}
		} else if stype, ok := env.GetStruct(st.Type.Name); ok {
			fields := make([]ParamDecl, len(stype.Order))
			for i, n := range stype.Order {
				fields[i] = ParamDecl{Name: n, Type: toGoTypeFromType(stype.Fields[n])}
			}
			return &TypeDeclStmt{Name: st.Type.Name, Fields: fields}, nil
		}
	case st.Assign != nil:
		if len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil && st.Assign.Index[0].Colon2 == nil && len(st.Assign.Field) == 0 {
			idx, err := compileExpr(st.Assign.Index[0].Start, env, "")
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(st.Assign.Value, env, "")
			if err != nil {
				return nil, err
			}
			// attempt to refine list element type on assignment
			if vt, err := env.GetVar(st.Assign.Name); err == nil {
				if lt, ok := vt.(types.ListType); ok {
					elemT := types.TypeOfExpr(st.Assign.Value, env)
					if types.IsAnyType(elemT) {
						elemT = types.TypeOfExprBasic(st.Assign.Value, env)
					}
					if types.IsAnyType(lt.Elem) && !types.IsAnyType(elemT) {
						env.SetVarDeep(st.Assign.Name, types.ListType{Elem: elemT}, true)
					}
				}
			}
			return &IndexAssignStmt{Name: st.Assign.Name, Index: idx, Value: val}, nil
		}
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			e, err := compileExpr(st.Assign.Value, env, st.Assign.Name)
			if err != nil {
				return nil, err
			}
			if ml, ok := e.(*MapLit); ok {
				if t, err := env.GetVar(st.Assign.Name); err == nil {
					updateMapLitTypes(ml, t)
				}
			}
			if ll, ok := e.(*ListLit); ok {
				if vt, err := env.GetVar(st.Assign.Name); err == nil {
					if lt, ok2 := vt.(types.ListType); ok2 {
						if ll.ElemType == "" || ll.ElemType == "any" {
							ll.ElemType = toGoTypeFromType(lt.Elem)
						}
					}
				}
			}
			return &AssignStmt{Name: st.Assign.Name, Value: e}, nil
		}
		// build postfix expression for complex target
		pf := &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: st.Assign.Name}}}
		for _, idx := range st.Assign.Index {
			pf.Ops = append(pf.Ops, &parser.PostfixOp{Index: idx})
		}
		for _, f := range st.Assign.Field {
			pf.Ops = append(pf.Ops, &parser.PostfixOp{Field: f})
		}
		target, err := compilePostfix(pf, env, "")
		if err != nil {
			return nil, err
		}
		if as, ok := target.(*AssertExpr); ok {
			target = as.Expr
		}
		val, err := compileExpr(st.Assign.Value, env, "")
		if err != nil {
			return nil, err
		}
		return &SetStmt{Target: target, Value: val}, nil
	case st.If != nil:
		return compileIfStmt(st.If, env)
	case st.While != nil:
		return compileWhileStmt(st.While, env)
	case st.For != nil:
		return compileForStmt(st.For, env)
	case st.Update != nil:
		return compileUpdateStmt(st.Update, env)
	case st.Return != nil:
		return compileReturnStmt(st.Return, env)
	case st.Fun != nil:
		return compileFunStmt(st.Fun, env)
	case st.Import != nil:
		if st.Import.Lang != nil {
			lang := *st.Import.Lang
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			switch lang {
			case "go":
				if st.Import.Auto && imports != nil {
					imports[alias] = strings.Trim(st.Import.Path, "\"")
				}
			case "python":
				if st.Import.Path == "math" {
					if imports != nil {
						imports[alias] = "math"
					}
					if st.Import.Auto && env != nil {
						env.SetVar(alias+".pi", types.FloatType{}, false)
						env.SetVar(alias+".e", types.FloatType{}, false)
						env.SetVar(alias+".sqrt", types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}, Pure: true}, false)
						env.SetVar(alias+".pow", types.FuncType{Params: []types.Type{types.FloatType{}, types.FloatType{}}, Return: types.FloatType{}, Pure: true}, false)
					}
				}
			}
		}
		return nil, nil
	case st.ExternVar != nil:
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	default:
		if st.Test == nil && st.Import == nil && st.Type == nil {
			return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
		}
	}
	return nil, nil
}

func compileStmts(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		st, err := compileStmt(s, env)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func extractCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	u := e.Binary.Left
	if u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return nil
	}
	return u.Value.Target.Call
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

func mapLiteralExpr(e *parser.Expr) *parser.MapLiteral {
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
	ml := mapLiteralExpr(e)
	if ml == nil {
		return ""
	}
	for _, it := range ml.Items {
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

func valueToExpr(v interface{}, typ *parser.TypeRef, env *types.Env) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		fields := make([]Expr, len(names))
		for i, k := range names {
			fields[i] = valueToExpr(val[k], nil, env)
		}
		if typ != nil && typ.Simple != nil {
			return &StructLit{Name: *typ.Simple, Fields: fields, Names: names}
		}
		keys := make([]Expr, len(names))
		for i, k := range names {
			keys[i] = &StringLit{Value: k}
		}
		return &MapLit{Keys: keys, Values: fields}
	case []interface{}:
		elems := make([]Expr, len(val))
		var et *parser.TypeRef
		if typ != nil {
			if typ.Generic != nil && typ.Generic.Name == "list" && len(typ.Generic.Args) == 1 {
				et = typ.Generic.Args[0]
			} else {
				et = typ
			}
		}
		for i, it := range val {
			elems[i] = valueToExpr(it, et, env)
		}
		elemType := ""
		if et != nil && et.Simple != nil {
			elemType = *et.Simple
		}
		return &ListLit{ElemType: elemType, Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case float64:
		if val == float64(int(val)) {
			return &IntLit{Value: int(val)}
		}
		return &FloatLit{Value: val}
	case int, int64:
		return &IntLit{Value: int(reflect.ValueOf(val).Int())}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func dataExprFromFile(path, format string, typ *parser.TypeRef, env *types.Env) (Expr, error) {
	if path == "" {
		return &ListLit{}, nil
	}
	root := meta.RepoRoot()
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
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "jsonl":
		var arr []interface{}
		for _, line := range bytes.Split(data, []byte{'\n'}) {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var item interface{}
			if err := json.Unmarshal(line, &item); err == nil {
				arr = append(arr, item)
			}
		}
		v = arr
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	expr := valueToExpr(v, typ, env)
	if format == "jsonl" {
		if ll, ok := expr.(*ListLit); ok {
			ll.Pretty = true
		}
	}
	return expr, nil
}

func compileIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	condExpr, err := compileExpr(is.Cond, env, "")
	if err != nil {
		return nil, err
	}
	cond := boolExprFor(condExpr, types.ExprType(is.Cond, env))
	thenStmts, err := compileStmts(is.Then, env)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		elseStmt, err := compileIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{elseStmt}
	} else if len(is.Else) > 0 {
		elseStmts, err = compileStmts(is.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func compileIfExpr(ie *parser.IfExpr, env *types.Env) (Expr, error) {
	cond, err := compileExpr(ie.Cond, env, "")
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(ie.Then, env, "")
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = compileIfExpr(ie.ElseIf, env)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = compileExpr(ie.Else, env, "")
		if err != nil {
			return nil, err
		}
	}
	typ := toGoTypeFromType(types.IfExprType(ie, env))
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr, Type: typ}, nil
}

func compileMatchExpr(me *parser.MatchExpr, env *types.Env) (Expr, error) {
	target, err := compileExpr(me.Target, env, "")
	if err != nil {
		return nil, err
	}
	typ := "any"
	if len(me.Cases) > 0 {
		t0 := types.ExprType(me.Cases[0].Result, env)
		typ = toGoTypeFromType(t0)
		for _, c := range me.Cases[1:] {
			if !types.EqualTypes(t0, types.ExprType(c.Result, env)) {
				typ = "any"
				break
			}
		}
	}
	if (typ == "any" || typ == "") && currentRetType != "" {
		typ = currentRetType
	}
	// Detect union match by looking for variant patterns.
	if len(me.Cases) > 0 {
		var firstVar string
		if name, ok := identName(me.Cases[0].Pattern); ok {
			firstVar = name
		} else if call, ok := callPattern(me.Cases[0].Pattern); ok {
			firstVar = call.Func
		}
		if firstVar != "" {
			if _, ok2 := env.FindUnionByVariant(firstVar); ok2 {
				cases := make([]UnionMatchCase, len(me.Cases))
				for i, c := range me.Cases {
					var variant string
					var bindings []string
					var fields []string
					caseEnv := env
					if n, ok := identName(c.Pattern); ok {
						variant = n
					} else if call, ok := callPattern(c.Pattern); ok {
						variant = call.Func
						st, _ := env.GetStruct(variant)
						bindings = make([]string, len(call.Args))
						fields = make([]string, len(call.Args))
						child := types.NewEnv(env)
						for j, a := range call.Args {
							if j < len(st.Order) {
								fields[j] = toGoFieldName(st.Order[j])
								if nm, ok := identName(a); ok {
									bindings[j] = nm
									child.SetVar(nm, st.Fields[st.Order[j]], true)
								}
							}
						}
						caseEnv = child
					} else {
						variant = "_"
					}
					body, err := compileExpr(c.Result, caseEnv, "")
					if err != nil {
						return nil, err
					}
					cases[i] = UnionMatchCase{Variant: variant, Fields: fields, Bindings: bindings, Body: body}
				}
				return &UnionMatchExpr{Target: target, Cases: cases, Type: typ}, nil
			}
		}
	}

	var expr Expr = zeroValueExpr(typ)
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := compileExpr(c.Result, env, "")
		if err != nil {
			return nil, err
		}
		pat, err := compileExpr(c.Pattern, env, "")
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*VarRef); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		expr = &IfExpr{Cond: cond, Then: res, Else: expr, Type: ""}
	}

	var setType func(Expr)
	setType = func(e Expr) {
		if ife, ok := e.(*IfExpr); ok {
			ife.Type = typ
			if ife.Else != nil {
				setType(ife.Else)
			}
		}
	}
	setType(expr)
	return expr, nil
}

func compileQueryExpr(q *parser.QueryExpr, env *types.Env, base string) (Expr, error) {
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "outer" && q.Group == nil && len(q.Froms) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		return compileOuterJoinQuery(q, env, base)
	}
	if q.Group != nil && len(q.Group.Exprs) == 1 && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		src, err := compileExpr(q.Source, env, "")
		if err != nil {
			return nil, err
		}
		srcT := types.ExprType(q.Source, env)
		var elemT types.Type
		switch t := srcT.(type) {
		case types.ListType:
			elemT = t.Elem
		case types.GroupType:
			elemT = t.Elem
			src = &FieldExpr{X: src, Name: "Items"}
		default:
			elemT = types.AnyType{}
		}
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemT, true)
		keyExpr, keyType, err := compileGroupKey(q.Group.Exprs[0], child, base)
		if err != nil {
			return nil, err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: elemT}, true)
		sel, err := compileExpr(q.Select, genv, "")
		if err != nil {
			return nil, err
		}
		var having Expr
		if q.Group.Having != nil {
			having, err = compileExpr(q.Group.Having, genv, "")
			if err != nil {
				return nil, err
			}
		}
		itemType := toGoTypeFromType(elemT)
		if itemType == "" {
			itemType = "any"
		}
		et := ""
		if ml := mapLiteral(q.Select); ml != nil {
			if st, ok := types.InferStructFromMapEnv(ml, genv); ok {
				structCount++
				baseName := fmt.Sprintf("Result%d", structCount)
				if base != "" {
					baseName = structNameFromVar(base)
				}
				name := types.UniqueStructName(baseName, topEnv, nil)
				st.Name = name
				if topEnv != nil {
					topEnv.SetStruct(name, st)
				}
				fieldsDecl := make([]ParamDecl, len(st.Order))
				vals := make([]Expr, len(st.Order))
				for i, it := range ml.Items {
					fieldsDecl[i] = ParamDecl{Name: st.Order[i], Type: toGoTypeFromType(st.Fields[st.Order[i]])}
					v, err := compileExpr(it.Value, genv, "")
					if err != nil {
						return nil, err
					}
					vals[i] = v
				}
				extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
				sel = &StructLit{Name: name, Fields: vals, Names: st.Order}
				et = name
			}
		} else if isVarRef(q.Select, q.Group.Name) {
			st := types.StructType{
				Fields: map[string]types.Type{
					"key":   keyType,
					"items": types.ListType{Elem: elemT},
				},
				Order: []string{"key", "items"},
			}
			structCount++
			baseName := "Group"
			if base != "" {
				baseName = structNameFromVar(base) + "Group"
			}
			name := types.UniqueStructName(baseName, topEnv, nil)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: []ParamDecl{
				{Name: "key", Type: toGoTypeFromType(keyType)},
				{Name: "items", Type: "[]" + itemType},
			}})
			et = name
		}
		if et == "" {
			et = toGoTypeFromType(types.ExprType(q.Select, genv))
		}
		if et == "" {
			et = "any"
		}
		keyGoType := toGoTypeFromType(keyType)
		if keyGoType == "" {
			keyGoType = "any"
		}
		var sortExpr Expr
		var sortType string
		if q.Sort != nil {
			sortExpr, err = compileExpr(q.Sort, genv, "")
			if err != nil {
				return nil, err
			}
			st := types.ExprType(q.Sort, genv)
			sortType = toGoTypeFromType(st)
			if sortType == "" || !isBasicOrderedType(st) {
				sortType = "any"
			}
			usesSort = true
		}
		usesPrint = true
		groupType := fmt.Sprintf("struct{Key %s; Items []%s}", keyGoType, itemType)
		if isVarRef(q.Select, q.Group.Name) {
			groupType = et
		}
		return &GroupQueryExpr{Var: q.Var, Src: src, Key: keyExpr, GroupVar: q.Group.Name, Cond: nil, Select: sel, Having: having, ElemType: et, GroupType: groupType, ItemType: itemType, KeyType: keyGoType, Sort: sortExpr, SortType: sortType}, nil
	}
	if q.Group != nil {
		src, err := compileExpr(q.Source, env, "")
		if err != nil {
			return nil, err
		}
		srcT := types.ExprType(q.Source, env)
		var elemT types.Type
		switch t := srcT.(type) {
		case types.ListType:
			elemT = t.Elem
		case types.GroupType:
			elemT = t.Elem
			src = &FieldExpr{X: src, Name: "Items"}
		default:
			elemT = types.AnyType{}
		}
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemT, true)
		froms := make([]queryFrom, len(q.Froms))
		varNames := []string{q.Var}
		for i, f := range q.Froms {
			fe, err := compileExpr(f.Src, child, "")
			if err != nil {
				return nil, err
			}
			ft := types.ExprType(f.Src, child)
			var felem types.Type
			switch t := ft.(type) {
			case types.ListType:
				felem = t.Elem
			case types.GroupType:
				felem = t.Elem
				fe = &FieldExpr{X: fe, Name: "Items"}
			default:
				felem = types.AnyType{}
			}
			child.SetVar(f.Var, felem, true)
			froms[i] = queryFrom{Var: f.Var, Src: fe}
			varNames = append(varNames, f.Var)
		}
		joins := make([]queryJoin, len(q.Joins))
		for i, j := range q.Joins {
			je, err := compileExpr(j.Src, child, "")
			if err != nil {
				return nil, err
			}
			jt := types.ExprType(j.Src, child)
			var jelem types.Type
			switch t := jt.(type) {
			case types.ListType:
				jelem = t.Elem
			case types.GroupType:
				jelem = t.Elem
				je = &FieldExpr{X: je, Name: "Items"}
			default:
				jelem = types.AnyType{}
			}
			child.SetVar(j.Var, jelem, true)
			onExpr, err := compileExpr(j.On, child, "")
			if err != nil {
				return nil, err
			}
			side := ""
			if j.Side != nil {
				side = *j.Side
			}
			joins[i] = queryJoin{Var: j.Var, Src: je, On: onExpr, Side: side}
			varNames = append(varNames, j.Var)
		}
		keyExpr, keyType, err := compileGroupKey(q.Group.Exprs[0], child, base)
		if err != nil {
			return nil, err
		}
		var where Expr
		if q.Where != nil {
			where, err = compileExpr(q.Where, child, "")
			if err != nil {
				return nil, err
			}
			where = boolExprFor(where, types.ExprType(q.Where, child))
		}
		genv := types.NewEnv(child)
		var itemName string
		if len(varNames) == 1 && len(q.Froms) == 0 && len(q.Joins) == 0 {
			itemName = toGoTypeFromType(elemT)
			if itemName == "" {
				itemName = "any"
			}
			genv.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: elemT}, true)
		} else {
			fieldTypes := make(map[string]types.Type, len(varNames))
			fieldsDecl := make([]ParamDecl, len(varNames))
			for i, v := range varNames {
				if t, err := child.GetVar(v); err == nil {
					fieldTypes[v] = t
					fieldsDecl[i] = ParamDecl{Name: v, Type: toGoTypeFromType(t)}
				} else {
					fieldTypes[v] = types.AnyType{}
					fieldsDecl[i] = ParamDecl{Name: v, Type: "any"}
				}
			}
			structCount++
			itemName = fmt.Sprintf("GroupItem%d", structCount)
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: itemName, Fields: fieldsDecl})
			genv.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: types.StructType{Name: itemName, Fields: fieldTypes, Order: varNames}}, true)
		}
		sel, err := compileExpr(q.Select, genv, "")
		if err != nil {
			return nil, err
		}
		var having Expr
		if q.Group.Having != nil {
			having, err = compileExpr(q.Group.Having, genv, "")
			if err != nil {
				return nil, err
			}
		}
		et := ""
		if ml := mapLiteral(q.Select); ml != nil {
			if st, ok := types.InferStructFromMapEnv(ml, genv); ok {
				structCount++
				baseName := fmt.Sprintf("Result%d", structCount)
				if base != "" {
					baseName = structNameFromVar(base)
				}
				name := types.UniqueStructName(baseName, topEnv, nil)
				st.Name = name
				if topEnv != nil {
					topEnv.SetStruct(name, st)
				}
				fieldsDecl := make([]ParamDecl, len(st.Order))
				vals := make([]Expr, len(st.Order))
				for i, it := range ml.Items {
					fieldsDecl[i] = ParamDecl{Name: st.Order[i], Type: toGoTypeFromType(st.Fields[st.Order[i]])}
					v, err := compileExpr(it.Value, genv, "")
					if err != nil {
						return nil, err
					}
					vals[i] = v
				}
				extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
				sel = &StructLit{Name: name, Fields: vals, Names: st.Order}
				et = name
			}
		}
		if et == "" {
			et = toGoTypeFromType(types.ExprType(q.Select, genv))
		}
		if et == "" {
			et = "any"
		}
		usesPrint = true
		keyGoType := toGoTypeFromType(keyType)
		if keyGoType == "" {
			keyGoType = "any"
		}
		var sortExpr Expr
		var sortType string
		if q.Sort != nil {
			sortExpr, err = compileExpr(q.Sort, genv, "")
			if err != nil {
				return nil, err
			}
			st := types.ExprType(q.Sort, genv)
			sortType = toGoTypeFromType(st)
			if sortType == "" || !isBasicOrderedType(st) {
				sortType = "any"
			}
			usesSort = true
		}
		return &GroupJoinQueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Key: keyExpr, GroupVar: q.Group.Name, Select: sel, Having: having, ElemType: et, ItemType: itemName, KeyType: keyGoType, Sort: sortExpr, SortType: sortType, Vars: varNames, SimpleItem: len(varNames) == 1 && len(q.Froms) == 0 && len(q.Joins) == 0}, nil
	}
	src, err := compileExpr(q.Source, env, "")
	if err != nil {
		return nil, err
	}
	srcT := types.ExprType(q.Source, env)
	var elemT types.Type
	switch t := srcT.(type) {
	case types.ListType:
		elemT = t.Elem
	case types.GroupType:
		elemT = t.Elem
		src = &FieldExpr{X: src, Name: "Items"}
	default:
		elemT = types.AnyType{}
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elemT, true)
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := compileExpr(f.Src, child, "")
		if err != nil {
			return nil, err
		}
		ft := types.ExprType(f.Src, child)
		var felem types.Type
		switch t := ft.(type) {
		case types.ListType:
			felem = t.Elem
		case types.GroupType:
			felem = t.Elem
			fe = &FieldExpr{X: fe, Name: "Items"}
		default:
			felem = types.AnyType{}
		}
		child.SetVar(f.Var, felem, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}

	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := compileExpr(j.Src, child, "")
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type
		switch t := jt.(type) {
		case types.ListType:
			jelem = t.Elem
		case types.GroupType:
			jelem = t.Elem
			je = &FieldExpr{X: je, Name: "Items"}
		default:
			jelem = types.AnyType{}
		}
		child.SetVar(j.Var, jelem, true)
		onExpr, err := compileExpr(j.On, child, "")
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
		where, err = compileExpr(q.Where, child, "")
		if err != nil {
			return nil, err
		}
		where = boolExprFor(where, types.ExprType(q.Where, child))
	}
	sel, err := compileExpr(q.Select, child, "")
	if err != nil {
		return nil, err
	}

	// detect simple aggregate: select sum(var)
	if s, ok := sel.(*SumExpr); ok && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		if vr, ok2 := s.List.(*VarRef); ok2 && vr.Name == q.Var {
			baseType := toGoTypeFromType(elemT)
			qexpr := &QueryExpr{Var: q.Var, Src: src, Froms: nil, Joins: nil, Where: where, Sort: nil, SortType: "", Skip: nil, Take: nil, Select: &VarRef{Name: q.Var}, ElemType: baseType}
			return &SumExpr{List: qexpr}, nil
		}
	}
	et := ""
	if ml := mapLiteral(q.Select); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			structCount++
			baseName := fmt.Sprintf("Result%d", structCount)
			if base != "" {
				baseName = structNameFromVar(base)
			}
			name := types.UniqueStructName(baseName, topEnv, nil)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fieldsDecl := make([]ParamDecl, len(st.Order))
			vals := make([]Expr, len(st.Order))
			for i, it := range ml.Items {
				fieldsDecl[i] = ParamDecl{Name: st.Order[i], Type: toGoTypeFromType(st.Fields[st.Order[i]])}
				v, err := compileExpr(it.Value, child, "")
				if err != nil {
					return nil, err
				}
				vals[i] = v
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
			sel = &StructLit{Name: name, Fields: vals, Names: st.Order}
			et = name
		}
	}
	if et == "" {
		et = toGoTypeFromType(types.ExprType(q.Select, child))
	}
	if et == "" {
		et = "any"
	}

	var sortExpr Expr
	var sortType string
	if q.Sort != nil {
		sortExpr, err = compileExpr(q.Sort, child, "")
		if err != nil {
			return nil, err
		}
		st := types.ExprType(q.Sort, child)
		sortType = toGoTypeFromType(st)
		if sortType == "" || !isBasicOrderedType(st) {
			sortType = "any"
		}
		usesSort = true
	}
	var skipExpr Expr
	if q.Skip != nil {
		skipExpr, err = compileExpr(q.Skip, env, "")
		if err != nil {
			return nil, err
		}
	}
	var takeExpr Expr
	if q.Take != nil {
		takeExpr, err = compileExpr(q.Take, env, "")
		if err != nil {
			return nil, err
		}
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Sort: sortExpr, SortType: sortType, Skip: skipExpr, Take: takeExpr, Select: sel, ElemType: et}, nil
}

func compileOuterJoinQuery(q *parser.QueryExpr, env *types.Env, base string) (Expr, error) {
	j := q.Joins[0]
	leftSrc, err := compileExpr(q.Source, env, "")
	if err != nil {
		return nil, err
	}
	leftT := types.ExprType(q.Source, env)
	var leftElem types.Type
	switch t := leftT.(type) {
	case types.ListType:
		leftElem = t.Elem
	case types.GroupType:
		leftElem = t.Elem
		leftSrc = &FieldExpr{X: leftSrc, Name: "Items"}
	default:
		leftElem = types.AnyType{}
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, leftElem, true)
	rightSrc, err := compileExpr(j.Src, child, "")
	if err != nil {
		return nil, err
	}
	rightT := types.ExprType(j.Src, child)
	var rightElem types.Type
	switch t := rightT.(type) {
	case types.ListType:
		rightElem = t.Elem
	case types.GroupType:
		rightElem = t.Elem
		rightSrc = &FieldExpr{X: rightSrc, Name: "Items"}
	default:
		rightElem = types.AnyType{}
	}
	ptrEnv := types.NewEnv(env)
	ptrEnv.SetVar(q.Var, types.OptionType{Elem: leftElem}, true)
	ptrEnv.SetVar(j.Var, types.OptionType{Elem: rightElem}, true)
	condEnv := types.NewEnv(env)
	condEnv.SetVar(q.Var, leftElem, true)
	condEnv.SetVar(j.Var, rightElem, true)
	cond, err := compileExpr(j.On, condEnv, "")
	if err != nil {
		return nil, err
	}
	baseName := "Result"
	if base != "" {
		baseName = structNameFromVar(base)
	}
	name := types.UniqueStructName(baseName, topEnv, nil)
	stype := types.StructType{
		Name: name,
		Fields: map[string]types.Type{
			"order":    types.OptionType{Elem: leftElem},
			"customer": types.OptionType{Elem: rightElem},
		},
		Order: []string{"order", "customer"},
	}
	if topEnv != nil {
		topEnv.SetStruct(name, stype)
	}
	fieldsDecl := []ParamDecl{{Name: "order", Type: toGoTypeFromType(stype.Fields["order"])}, {Name: "customer", Type: toGoTypeFromType(stype.Fields["customer"])}}
	extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
	sel := &StructLit{Name: name, Fields: []Expr{&VarRef{Name: q.Var}, &VarRef{Name: j.Var}}, Names: []string{"order", "customer"}}
	et := name
	leftType := toGoTypeFromType(types.OptionType{Elem: leftElem})
	rightType := toGoTypeFromType(types.OptionType{Elem: rightElem})
	return &OuterJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel, ElemType: et, LeftType: leftType, RightType: rightType}, nil
}

func zeroValueExpr(goType string) Expr {
	switch {
	case goType == "int":
		return &IntLit{Value: 0}
	case goType == "float64":
		return &CallExpr{Func: "float64", Args: []Expr{&IntLit{Value: 0}}}
	case goType == "bool":
		return &BoolLit{Value: false}
	case goType == "string":
		return &StringLit{Value: ""}
	case strings.HasPrefix(goType, "[]"):
		return &VarRef{Name: "nil"}
	case strings.HasPrefix(goType, "*"):
		if goType == "*big.Int" {
			usesBigInt = true
			return &CallExpr{Func: "big.NewInt", Args: []Expr{&IntLit{Value: 0}}}
		}
		return &VarRef{Name: "nil"}
	default:
		if _, ok := topEnv.GetStruct(goType); ok {
			return &StructLit{Name: goType}
		}
		return &VarRef{Name: "nil"}
	}
}

func isBigIntType(t types.Type) bool {
	_, ok := t.(types.BigIntType)
	return ok
}

func ensureBigIntExpr(e Expr, t types.Type) Expr {
	if isBigIntType(t) {
		return e
	}
	usesBigInt = true
	return &CallExpr{Func: "big.NewInt", Args: []Expr{&CallExpr{Func: "int64", Args: []Expr{e}}}}
}

func boolExprFor(e Expr, t types.Type) Expr {
	if _, ok := t.(types.BoolType); ok {
		return e
	}
	switch ex := e.(type) {
	case *AssertExpr:
		if ex.Type == "bool" {
			return e
		}
	case *BinaryExpr:
		switch ex.Op {
		case "&&", "||", "==", "!=", "<", "<=", ">", ">=":
			return e
		}
	case *NotExpr:
		return e
	}
	switch t.(type) {
	case types.OptionType:
		return &BinaryExpr{Left: e, Op: "!=", Right: &VarRef{Name: "nil"}}
	case types.IntType, types.Int64Type,
		types.FloatType, types.BigRatType:
		return &BinaryExpr{Left: e, Op: "!=", Right: &IntLit{Value: 0}}
	case types.BigIntType:
		usesBigInt = true
		zero := &CallExpr{Func: "big.NewInt", Args: []Expr{&IntLit{Value: 0}}}
		return &BigCmpExpr{Left: e, Op: "!=", Right: zero}
	case types.StringType:
		return &BinaryExpr{Left: e, Op: "!=", Right: &StringLit{Value: ""}}
	case types.ListType, types.MapType:
		return &BinaryExpr{Left: &CallExpr{Func: "len", Args: []Expr{e}}, Op: "!=", Right: &IntLit{Value: 0}}
	default:
		// assume dynamic value should be treated as boolean
		return &AssertExpr{Expr: e, Type: "bool"}
	}
}

func compileWhileStmt(ws *parser.WhileStmt, env *types.Env) (Stmt, error) {
	condExpr, err := compileExpr(ws.Cond, env, "")
	if err != nil {
		return nil, err
	}
	cond := boolExprFor(condExpr, types.ExprType(ws.Cond, env))
	child := types.NewEnv(env)
	body, err := compileStmts(ws.Body, child)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func compileForStmt(fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := compileExpr(fs.Source, env, "")
		if err != nil {
			return nil, err
		}
		end, err := compileExpr(fs.RangeEnd, env, "")
		if err != nil {
			return nil, err
		}
		child := types.NewEnv(env)
		child.SetVar(fs.Name, types.IntType{}, true)
		body, err := compileStmts(fs.Body, child)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := compileExpr(fs.Source, env, "")
	if err != nil {
		return nil, err
	}
	t := types.TypeOfExpr(fs.Source, env)
	if types.IsAnyType(t) {
		t = types.TypeOfExprBasic(fs.Source, env)
	}
	child := types.NewEnv(env)
	var isMap bool
	var keyType string
	switch tt := t.(type) {
	case types.MapType:
		isMap = true
		child.SetVar(fs.Name, tt.Key, true)
		keyType = toGoTypeFromType(tt.Key)
		usesSort = true
	case types.ListType:
		child.SetVar(fs.Name, tt.Elem, true)
	default:
		child.SetVar(fs.Name, types.AnyType{}, true)
	}
	body, err := compileStmts(fs.Body, child)
	if err != nil {
		return nil, err
	}
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body, IsMap: isMap, KeyType: keyType}, nil
}

func compileUpdateStmt(u *parser.UpdateStmt, env *types.Env) (Stmt, error) {
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
	for n, ft := range st.Fields {
		child.SetVar(n, ft, true)
		fieldSet[n] = true
	}
	fields := make([]string, len(u.Set.Items))
	values := make([]Expr, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return nil, fmt.Errorf("unsupported update key")
		}
		val, err := compileExpr(it.Value, child, "")
		if err != nil {
			return nil, err
		}
		values[i] = substituteFieldVars(val, fieldSet)
		fields[i] = key
	}
	var cond Expr
	if u.Where != nil {
		c, err := compileExpr(u.Where, child, "")
		if err != nil {
			return nil, err
		}
		cond = substituteFieldVars(c, fieldSet)
		cond = boolExprFor(cond, types.ExprType(u.Where, child))
	}
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func compileReturnStmt(rs *parser.ReturnStmt, env *types.Env) (Stmt, error) {
	if rs == nil {
		return &ReturnStmt{}, nil
	}
	if rs.Value == nil {
		return &ReturnStmt{}, nil
	}
	val, err := compileExpr(rs.Value, env, "")
	if err != nil {
		return nil, err
	}
	ret := currentRetType
	if ret != "" && ret != "any" {
		exprType := toGoTypeFromType(types.ExprType(rs.Value, env))
		if exprType == "" {
			if um, ok := val.(*UnionMatchExpr); ok && um.Type != "" {
				exprType = um.Type
			} else {
				exprType = "any"
			}
		}
		if ll, ok := val.(*ListLit); ok && strings.HasPrefix(ret, "[]") {
			if ll.ElemType == "" || ll.ElemType == "any" {
				ll.ElemType = ret[2:]
				exprType = ret
			}
		}
		if ml, ok := val.(*MapLit); ok && strings.HasPrefix(ret, "map[") {
			if mt, ok2 := toTypeFromGoType(ret).(types.MapType); ok2 {
				ml.KeyType = toGoTypeFromType(mt.Key)
				ml.ValueType = toGoTypeFromType(mt.Value)
				exprType = ret
			}
		}
		if exprType != ret {
			if ae, ok := val.(*AssertExpr); !(ok && ae.Type == ret) {
				val = &AssertExpr{Expr: val, Type: ret}
			}
		}
	}
	return &ReturnStmt{Value: val}, nil
}

func compileFunStmt(fn *parser.FunStmt, env *types.Env) (Stmt, error) {
	child := types.NewEnv(env)
	// Ensure the outer environment knows about this function's type so that
	// subsequent statements (like a return) can lookup the correct type.
	if _, err := env.GetVar(fn.Name); err != nil {
		paramTypes := make([]types.Type, len(fn.Params))
		for i, p := range fn.Params {
			if p.Type != nil {
				paramTypes[i] = types.ResolveTypeRef(p.Type, env)
			} else {
				paramTypes[i] = types.AnyType{}
			}
		}
		var retT types.Type = types.AnyType{}
		if fn.Return != nil {
			retT = types.ResolveTypeRef(fn.Return, env)
		}
		env.SetVar(fn.Name, types.FuncType{Params: paramTypes, Return: retT}, false)
	}
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), true)
		} else if t, err := env.GetVar(p.Name); err == nil {
			child.SetVar(p.Name, t, true)
		} else {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
	}
	prevRet := currentRetType
	retHint := toGoType(fn.Return, env)
	currentRetType = retHint
	body, err := compileStmts(fn.Body, child)
	currentRetType = prevRet
	if err != nil {
		return nil, err
	}
	params := make([]ParamDecl, len(fn.Params))
	for i, p := range fn.Params {
		typ := toGoType(p.Type, env)
		if typ == "" {
			if t, err := child.GetVar(p.Name); err == nil {
				typ = toGoTypeFromType(t)
			}
		}
		params[i] = ParamDecl{Name: p.Name, Type: typ}
	}
	ret := toGoType(fn.Return, env)
	if ret == "" {
		if t, err := child.GetVar(fn.Name); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				ret = toGoTypeFromType(ft.Return)
			}
		}
	}
	if env != topEnv {
		lit := &FuncLit{Params: params, Return: ret, Body: body}
		return &VarDecl{Name: fn.Name, Value: lit}, nil
	}
	name := fn.Name
	if name == "main" {
		mainFuncName = "mochiMain"
		name = mainFuncName
	}
	return &FuncDecl{Name: name, Params: params, Return: ret, Body: body}, nil
}

func compileFunExpr(fn *parser.FunExpr, env *types.Env) (Expr, error) {
	child := types.NewEnv(env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), true)
		} else if t, err := env.GetVar(p.Name); err == nil {
			child.SetVar(p.Name, t, true)
		} else {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
	}
	prevRet := currentRetType
	currentRetType = toGoType(fn.Return, env)
	var body []*parser.Statement
	var stmts []Stmt
	var err error
	if fn.BlockBody != nil {
		body = fn.BlockBody
		stmts, err = compileStmts(body, child)
		if err != nil {
			return nil, err
		}
	} else if fn.ExprBody != nil {
		ex, err := compileExpr(fn.ExprBody, child, "")
		if err != nil {
			return nil, err
		}
		stmts = []Stmt{&ReturnStmt{Value: ex}}
	}
	params := make([]ParamDecl, len(fn.Params))
	for i, p := range fn.Params {
		typ := toGoType(p.Type, env)
		if typ == "" {
			if t, err := child.GetVar(p.Name); err == nil {
				typ = toGoTypeFromType(t)
			}
		}
		params[i] = ParamDecl{Name: p.Name, Type: typ}
	}
	ret := toGoType(fn.Return, env)
	currentRetType = prevRet
	return &FuncLit{Params: params, Return: ret, Body: stmts}, nil
}

func compileBinary(b *parser.BinaryExpr, env *types.Env, base string) (Expr, error) {
	first, err := compileUnary(b.Left, env, base)
	if err != nil {
		return nil, err
	}
	firstType := types.TypeOfUnary(b.Left, env)
	operands := []Expr{first}
	typesList := []types.Type{firstType}
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		expr, err := compilePostfix(op.Right, env, base)
		if err != nil {
			return nil, err
		}
		ops[i] = op
		operands = append(operands, expr)
		typesList = append(typesList, types.TypeOfPostfix(op.Right, env))
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

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			opName := ops[i].Op
			if opName == "union" && ops[i].All {
				opName = "union_all"
			}
			if contains(level, opName) {
				left := operands[i]
				right := operands[i+1]
				var newExpr Expr
				switch opName {
				case "in":
					ctype := typesList[i+1]
					var kind, et string
					switch ct := ctype.(type) {
					case types.StringType:
						kind = "string"
					case types.MapType:
						kind = "map"
						et = toGoTypeFromType(ct.Key)
					case types.ListType:
						kind = "list"
						et = toGoTypeFromType(ct.Elem)
					default:
						kind = "list"
					}
					if kind == "string" {
						usesStrings = true
					}
					newExpr = &ContainsExpr{Collection: right, Value: left, Kind: kind, ElemType: et}
				case "union":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &UnionExpr{Left: left, Right: right, ElemType: et}
				case "union_all":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &UnionAllExpr{Left: left, Right: right, ElemType: et}
				case "except":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &ExceptExpr{Left: left, Right: right, ElemType: et}
				case "intersect":
					et := "any"
					if lt, ok := typesList[i].(types.ListType); ok {
						et = toGoTypeFromType(lt.Elem)
					}
					newExpr = &IntersectExpr{Left: left, Right: right, ElemType: et}
				default:
					if isBigIntType(typesList[i]) || isBigIntType(typesList[i+1]) {
						usesBigInt = true
						left = ensureBigIntExpr(left, typesList[i])
						right = ensureBigIntExpr(right, typesList[i+1])
						switch ops[i].Op {
						case "+", "-", "*", "/", "%":
							newExpr = &BigBinaryExpr{Left: left, Op: ops[i].Op, Right: right}
						case "<", "<=", ">", ">=", "==", "!=":
							newExpr = &BigCmpExpr{Left: left, Op: ops[i].Op, Right: right}
						}
					}
					// list concatenation with +
					if ops[i].Op == "+" {
						if lt, ok := typesList[i].(types.ListType); ok {
							et := toGoTypeFromType(lt.Elem)
							newExpr = &UnionAllExpr{Left: left, Right: right, ElemType: et}
						}
					}
					// auto convert unknown types to float64 for arithmetic
					if newExpr == nil && (ops[i].Op == "+" || ops[i].Op == "-" || ops[i].Op == "*" || ops[i].Op == "/") {
						if _, ok := typesList[i].(types.AnyType); ok {
							left = &CallExpr{Func: "_toFloat", Args: []Expr{left}}
							usesFloatConv = true
						}
						if _, ok := typesList[i+1].(types.AnyType); ok {
							right = &CallExpr{Func: "_toFloat", Args: []Expr{right}}
							usesFloatConv = true
						}
						if _, ok := typesList[i].(types.FloatType); ok {
							if _, ok2 := typesList[i+1].(types.IntType); ok2 {
								right = &CallExpr{Func: "float64", Args: []Expr{right}}
							}
						}
						if _, ok := typesList[i+1].(types.FloatType); ok {
							if _, ok2 := typesList[i].(types.IntType); ok2 {
								left = &CallExpr{Func: "float64", Args: []Expr{left}}
							}
						}
					}
					if newExpr == nil {
						newExpr = &BinaryExpr{Left: left, Op: ops[i].Op, Right: right}
					}
				}
				operands[i] = newExpr
				operands = append(operands[:i+1], operands[i+2:]...)
				typesList[i] = typesList[i+1]
				typesList = append(typesList[:i+1], typesList[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	return operands[0], nil
}

func compileUnary(u *parser.Unary, env *types.Env, base string) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := compilePostfix(u.Value, env, base)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			if isBigIntType(types.TypeOfPostfix(u.Value, env)) {
				usesBigInt = true
				zero := &CallExpr{Func: "big.NewInt", Args: []Expr{&IntLit{Value: 0}}}
				expr = ensureBigIntExpr(expr, types.TypeOfPostfix(u.Value, env))
				expr = &BigBinaryExpr{Left: zero, Op: "-", Right: expr}
			} else {
				expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
			}
		case "!":
			if types.IsAnyType(types.TypeOfPostfix(u.Value, env)) {
				expr = &NotExpr{Expr: &AssertExpr{Expr: expr, Type: "bool"}}
			} else {
				expr = &NotExpr{Expr: expr}
			}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func compilePostfix(pf *parser.PostfixExpr, env *types.Env, base string) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := compilePrimary(pf.Target, env, base)
	if err != nil {
		// allow selector with tail handled here
		if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
			expr = &VarRef{Name: pf.Target.Selector.Root}
		} else {
			return nil, err
		}
	}
	// handle selector tail
	tail := []string{}
	if pf.Target != nil && pf.Target.Selector != nil {
		tail = pf.Target.Selector.Tail
	}
	var t types.Type
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
		if v, err := env.GetVar(pf.Target.Selector.Root); err == nil {
			t = v
		} else {
			t = types.TypeOfPrimaryBasic(pf.Target, env)
		}
	} else {
		t = types.TypeOfPrimaryBasic(pf.Target, env)
	}
	if len(tail) > 0 && (len(pf.Ops) == 0 || pf.Ops[0].Call == nil) {
		for _, f := range tail {
			switch tt := t.(type) {
			case types.MapType:
				expr = &IndexExpr{X: expr, Index: &StringLit{Value: f}}
				t = tt.Value
			case types.StructType:
				expr = &FieldExpr{X: expr, Name: toGoFieldName(f)}
				if ft, ok := tt.Fields[f]; ok {
					t = ft
				} else {
					t = types.AnyType{}
				}
			case types.OptionType:
				expr = &FieldExpr{X: expr, Name: toGoFieldName(f)}
				t = tt.Elem
			case types.GroupType:
				if f == "key" {
					expr = &FieldExpr{X: expr, Name: "Key"}
					t = tt.Key
				} else if f == "items" {
					expr = &FieldExpr{X: expr, Name: "Items"}
					t = types.ListType{Elem: tt.Elem}
				} else {
					expr = &FieldExpr{X: expr, Name: toGoFieldName(f)}
					t = types.AnyType{}
				}
			default:
				if types.IsAnyType(t) {
					expr = &IndexExpr{X: &AssertExpr{Expr: expr, Type: "map[string]any"}, Index: &StringLit{Value: f}}
				} else {
					expr = &FieldExpr{X: expr, Name: toGoFieldName(f)}
				}
				t = types.AnyType{}
			}
		}
	} else {
		t = types.TypeOfPrimaryBasic(pf.Target, env)
	}
	// if tail has one element and first op is CallOp => method call
	if len(tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		method := tail[0]
		args := make([]Expr, len(pf.Ops[0].Call.Args))
		for i, a := range pf.Ops[0].Call.Args {
			ex, err := compileExpr(a, env, "")
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		if _, ok := imports[pf.Target.Selector.Root]; ok {
			full := pf.Target.Selector.Root + "." + toGoFieldName(method)
			if full == "net.LookupHost" && len(args) == 1 {
				return &LookupHostExpr{Arg: args[0]}, nil
			}
			return &CallExpr{Func: full, Args: args}, nil
		}
		switch method {
		case "contains":
			rec := pf.Target
			if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
				rec = &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
			}
			mtype := types.TypeOfPrimary(rec, env)
			var kind, et string
			switch mt := mtype.(type) {
			case types.StringType:
				kind = "string"
			case types.MapType:
				kind = "map"
				et = toGoTypeFromType(mt.Key)
			case types.ListType:
				kind = "list"
				et = toGoTypeFromType(mt.Elem)
			default:
				kind = "list"
			}
			if kind == "string" {
				usesStrings = true
			}
			return &ContainsExpr{Collection: expr, Value: args[0], Kind: kind, ElemType: et}, nil
		default:
			return nil, fmt.Errorf("unsupported method %s", method)
		}
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil && idx.Colon2 == nil {
				if idx.Start == nil {
					return nil, fmt.Errorf("unsupported index")
				}
				iex, err := compileExpr(idx.Start, env, "")
				if err != nil {
					return nil, err
				}
				switch tt := t.(type) {
				case types.StringType:
					expr = &CallExpr{Func: "string", Args: []Expr{&IndexExpr{X: &RuneSliceExpr{Expr: expr}, Index: iex}}}
					t = types.StringType{}
				case types.ListType:
					expr = &IndexExpr{X: expr, Index: iex}
					t = tt.Elem
				case types.MapType:
					expr = &IndexExpr{X: expr, Index: iex}
					t = tt.Value
					if _, ok := tt.Value.(types.AnyType); ok {
						key := ""
						if sl, ok2 := iex.(*StringLit); ok2 {
							key = sl.Value
						} else if s, ok2 := literalString(idx.Start); ok2 {
							key = s
						}
						if gt, ok3 := fieldTypeGuess[key]; key != "" && ok3 && gt != "" && gt != "any" {
							if ae, ok := expr.(*AssertExpr); !(ok && ae.Type == gt) {
								expr = &AssertExpr{Expr: expr, Type: gt}
							}
							switch gt {
							case "map[string]int":
								t = types.MapType{Key: types.StringType{}, Value: types.IntType{}}
							case "map[string]any":
								t = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
							default:
								t = toTypeFromGoType(gt)
							}
						}
					}
				default:
					if types.IsAnyType(t) {
						if _, ok2 := types.TypeOfExpr(idx.Start, env).(types.IntType); ok2 {
							expr = &IndexExpr{X: &AssertExpr{Expr: expr, Type: "[]any"}, Index: iex}
						} else {
							expr = &IndexExpr{X: &AssertExpr{Expr: expr, Type: "map[string]any"}, Index: iex}
						}
					} else {
						expr = &IndexExpr{X: expr, Index: iex}
					}
					if types.IsAnyType(t) {
						key := ""
						if sl, ok2 := iex.(*StringLit); ok2 {
							key = sl.Value
						} else if s, ok2 := literalString(idx.Start); ok2 {
							key = s
						}
						if gt, ok3 := fieldTypeGuess[key]; key != "" && ok3 && gt != "" && gt != "any" {
							if ae, ok := expr.(*AssertExpr); !(ok && ae.Type == gt) {
								expr = &AssertExpr{Expr: expr, Type: gt}
							}
							switch gt {
							case "map[string]int":
								t = types.MapType{Key: types.StringType{}, Value: types.IntType{}}
							case "map[string]any":
								t = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
							default:
								t = toTypeFromGoType(gt)
							}
						} else {
							t = types.AnyType{}
						}
					} else {
						t = types.AnyType{}
					}
				}
			} else {
				var start, end Expr
				if idx.Start != nil {
					start, err = compileExpr(idx.Start, env, "")
					if err != nil {
						return nil, err
					}
				}
				if idx.End != nil {
					end, err = compileExpr(idx.End, env, "")
					if err != nil {
						return nil, err
					}
				}
				switch tt := t.(type) {
				case types.StringType:
					expr = &CallExpr{Func: "string", Args: []Expr{&SliceExpr{X: &RuneSliceExpr{Expr: expr}, Start: start, End: end}}}
					t = types.StringType{}
				case types.ListType:
					expr = &SliceExpr{X: expr, Start: start, End: end}
					t = types.ListType{Elem: tt.Elem}
				default:
					expr = &SliceExpr{X: expr, Start: start, End: end}
					t = types.AnyType{}
				}
			}
		} else if op.Call != nil {
			if vr, ok := expr.(*VarRef); ok {
				if _, ok2 := imports[vr.Name]; ok2 {
					args := make([]Expr, len(op.Call.Args))
					for j, a := range op.Call.Args {
						ex, err := compileExpr(a, env, "")
						if err != nil {
							return nil, err
						}
						args[j] = ex
					}
					return &CallExpr{Func: vr.Name, Args: args}, nil
				}
			}
			return nil, fmt.Errorf("unsupported call")
		} else if op.Cast != nil {
			if op.Cast.Type == nil {
				return nil, fmt.Errorf("unsupported postfix")
			}
			if op.Cast.Type.Simple != nil {
				name := *op.Cast.Type.Simple
				if name == "int" {
					switch t.(type) {
					case types.StringType:
						usesStrconv = true
						expr = &AtoiExpr{Expr: expr}
					case types.IntType, types.Int64Type:
						// no-op
					case types.BigIntType:
						usesBigInt = true
						expr = &BigIntToIntExpr{Value: expr}
					default:
						expr = &IntCastExpr{Expr: expr}
					}
					t = types.IntType{}
				} else if name == "bigint" {
					usesBigInt = true
					switch t.(type) {
					case types.IntType, types.Int64Type:
						expr = &CallExpr{Func: "big.NewInt", Args: []Expr{&CallExpr{Func: "int64", Args: []Expr{expr}}}}
					case types.BigIntType:
						// no-op
					default:
						return nil, fmt.Errorf("unsupported postfix")
					}
					t = types.BigIntType{}
				} else if name == "float" {
					expr = &CallExpr{Func: "float64", Args: []Expr{expr}}
				} else if st, ok := env.GetStruct(name); ok {
					if ml, ok := expr.(*MapLit); ok {
						fields := make([]Expr, len(st.Order))
						for i, fn := range st.Order {
							fields[i] = zeroValueExpr(toGoTypeFromType(st.Fields[fn]))
							for j, k := range ml.Keys {
								if sl, ok := k.(*StringLit); ok && sl.Value == fn {
									fields[i] = ml.Values[j]
									break
								}
							}
						}
						expr = &StructLit{Name: name, Fields: fields, Names: st.Order}
					} else {
						expr = &AssertExpr{Expr: expr, Type: name}
					}
				} else {
					return nil, fmt.Errorf("unsupported postfix")
				}
			} else if op.Cast.Type.Struct != nil {
				return nil, fmt.Errorf("unsupported postfix")
			} else if op.Cast.Type.Generic != nil {
				g := op.Cast.Type.Generic
				if g.Name == "list" && len(g.Args) == 1 {
					typ := "[]" + toGoType(g.Args[0], env)
					if types.IsAnyType(t) {
						if ae, ok := expr.(*AssertExpr); !(ok && ae.Type == typ) {
							expr = &AssertExpr{Expr: expr, Type: typ}
						}
					}
					t = types.ListType{Elem: types.ResolveTypeRef(g.Args[0], env)}
				} else if g.Name == "map" && len(g.Args) == 2 {
					typ := fmt.Sprintf("map[%s]%s", toGoType(g.Args[0], env), toGoType(g.Args[1], env))
					if types.IsAnyType(t) {
						if ae, ok := expr.(*AssertExpr); !(ok && ae.Type == typ) {
							expr = &AssertExpr{Expr: expr, Type: typ}
						}
					}
					t = types.MapType{Key: types.ResolveTypeRef(g.Args[0], env), Value: types.ResolveTypeRef(g.Args[1], env)}
				} else {
					return nil, fmt.Errorf("unsupported postfix")
				}
			} else {
				return nil, fmt.Errorf("unsupported postfix")
			}
		} else if op.Field != nil {
			if pf.Target != nil && pf.Target.Selector != nil {
				if _, ok := imports[pf.Target.Selector.Root]; ok && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
					args := make([]Expr, len(pf.Ops[i+1].Call.Args))
					for j, a := range pf.Ops[i+1].Call.Args {
						ex, err := compileExpr(a, env, "")
						if err != nil {
							return nil, err
						}
						args[j] = ex
					}
					return &CallExpr{Func: pf.Target.Selector.Root + "." + toGoFieldName(op.Field.Name), Args: args}, nil
				}
			}
			switch tt := t.(type) {
			case types.MapType:
				expr = &IndexExpr{X: expr, Index: &StringLit{Value: op.Field.Name}}
				t = tt.Value
			case types.StructType:
				expr = &FieldExpr{X: expr, Name: op.Field.Name}
				if ft, ok := tt.Fields[op.Field.Name]; ok {
					t = ft
				} else {
					t = types.AnyType{}
				}
			case types.OptionType:
				expr = &FieldExpr{X: expr, Name: op.Field.Name}
				t = tt.Elem
			default:
				expr = &FieldExpr{X: expr, Name: op.Field.Name}
				t = types.AnyType{}
			}
		}
	}
	return expr, nil
}

func compilePrimary(p *parser.Primary, env *types.Env, base string) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a, env, "")
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		if name == "main" && mainFuncName != "" {
			name = mainFuncName
		}
		switch name {
		case "avg":
			return &AvgExpr{List: args[0]}, nil
		case "count":
			if _, ok := types.TypeOfExpr(p.Call.Args[0], env).(types.GroupType); ok {
				return &CallExpr{Func: "len", Args: []Expr{&FieldExpr{X: args[0], Name: "Items"}}}, nil
			}
			name = "len"
		case "str":
			name = "fmt.Sprint"
		case "int":
			argType := types.TypeOfExpr(p.Call.Args[0], env)
			if _, ok := argType.(types.StringType); ok {
				usesStrconv = true
				return &AtoiExpr{Expr: args[0]}, nil
			}
			if _, ok := argType.(types.BigIntType); ok {
				usesBigInt = true
				return &BigIntToIntExpr{Value: args[0]}, nil
			}
			if types.IsAnyType(argType) {
				args[0] = &AssertExpr{Expr: args[0], Type: "int"}
			}
			return &CallExpr{Func: "int", Args: []Expr{args[0]}}, nil
		case "float":
			return &CallExpr{Func: "float64", Args: []Expr{args[0]}}, nil
		case "abs":
			if imports != nil {
				imports["math"] = "math"
			}
			return &CallExpr{Func: "math.Abs", Args: []Expr{args[0]}}, nil
		case "sum":
			isFloat := false
			switch a := args[0].(type) {
			case *ListLit:
				if a.ElemType == "float64" {
					isFloat = true
				}
			case *QueryExpr:
				if a.ElemType == "float64" {
					isFloat = true
				}
			case *GroupQueryExpr:
				if a.ElemType == "float64" {
					isFloat = true
				}
			case *GroupJoinQueryExpr:
				if a.ElemType == "float64" {
					isFloat = true
				}
			}
			return &SumExpr{List: args[0], IsFloat: isFloat}, nil
		case "min":
			return &MinExpr{List: args[0]}, nil
		case "max":
			return &MaxExpr{List: args[0]}, nil
		case "keys":
			mt, _ := types.TypeOfExpr(p.Call.Args[0], env).(types.MapType)
			usesSort = true
			return &KeysExpr{Map: args[0], KeyType: toGoTypeFromType(mt.Key)}, nil
		case "values":
			mt, _ := types.TypeOfExpr(p.Call.Args[0], env).(types.MapType)
			usesSort = true
			return &ValuesExpr{Map: args[0], ValueType: toGoTypeFromType(mt.Value)}, nil
		case "exists":
			bexpr := &BinaryExpr{Left: &CallExpr{Func: "len", Args: []Expr{args[0]}}, Op: ">", Right: &IntLit{Value: 0}}
			return &ExistsExpr{Expr: bexpr}, nil
		case "substring":
			usesSubstr = true
			return &CallExpr{Func: "_substr", Args: []Expr{args[0], args[1], args[2]}}, nil
		case "lower":
			usesStrings = true
			return &CallExpr{Func: "strings.ToLower", Args: []Expr{args[0]}}, nil
		case "upper":
			usesStrings = true
			return &CallExpr{Func: "strings.ToUpper", Args: []Expr{args[0]}}, nil
		case "now":
			usesTime = true
			return &NowExpr{}, nil
		case "input":
			usesInput = true
			return &InputExpr{}, nil
		case "json":
			usesJSON = true
			return &JsonExpr{Value: args[0]}, nil
		case "net.LookupHost":
			return &LookupHostExpr{Arg: args[0]}, nil
		}
		if t, err := env.GetVar(name); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if len(args) < len(ft.Params) {
					missing := ft.Params[len(args):]
					var fn *parser.FunStmt
					if f, ok := env.GetFunc(name); ok {
						fn = f
					}
					params := make([]ParamDecl, len(missing))
					callArgs := make([]Expr, 0, len(ft.Params))
					callArgs = append(callArgs, args...)
					for i, mt := range missing {
						pname := fmt.Sprintf("p%d", i)
						if fn != nil && i+len(args) < len(fn.Params) {
							pname = fn.Params[i+len(args)].Name
						}
						params[i] = ParamDecl{Name: pname, Type: toGoTypeFromType(mt)}
						callArgs = append(callArgs, &VarRef{Name: pname})
					}
					body := []Stmt{&ReturnStmt{Value: &CallExpr{Func: name, Args: callArgs}}}
					return &FuncLit{Params: params, Return: toGoTypeFromType(ft.Return), Body: body}, nil
				}
				if len(args) >= len(ft.Params) {
					for i, mt := range ft.Params {
						at := types.TypeOfExpr(p.Call.Args[i], env)
						if types.IsAnyType(at) {
							gt := toGoTypeFromType(mt)
							if gt != "" && gt != "any" {
								args[i] = &AssertExpr{Expr: args[i], Type: gt}
							}
						}
					}
				}
			}
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.List != nil:
		if st, ok := types.InferStructFromList(p.List, env); ok {
			structCount++
			baseName := fmt.Sprintf("Data%d", structCount)
			if base != "" {
				baseName = structNameFromVar(base)
			}
			name := types.UniqueStructName(baseName, topEnv, nil)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fieldsDecl := make([]ParamDecl, len(st.Order))
			for i, fn := range st.Order {
				fieldsDecl[i] = ParamDecl{Name: fn, Type: toGoTypeFromType(st.Fields[fn])}
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
			if base != "" {
				env.SetVarDeep(base, types.ListType{Elem: types.StructType{Name: name, Fields: st.Fields, Order: st.Order}}, true)
			}
			elems := make([]Expr, len(p.List.Elems))
			for i, e := range p.List.Elems {
				ml := e.Binary.Left.Value.Target.Map
				vals := make([]Expr, len(st.Order))
				for j, it := range ml.Items {
					ve, err := compileExpr(it.Value, env, "")
					if err != nil {
						return nil, err
					}
					vals[j] = ve
				}
				elems[i] = &StructLit{Name: name, Fields: vals, Names: st.Order}
			}
			return &ListLit{ElemType: name, Elems: elems}, nil
		}
		elems := make([]Expr, len(p.List.Elems))
		elemType := "any"
		if len(p.List.Elems) > 0 {
			var base types.Type
			same := true
			for i, e := range p.List.Elems {
				ex, err := compileExpr(e, env, "")
				if err != nil {
					return nil, err
				}
				if ml, ok := ex.(*MapLit); ok {
					updateMapLitTypes(ml, types.ExprType(e, env))
				}
				elems[i] = ex
				et := types.ExprType(e, env)
				if base == nil {
					if !types.ContainsAny(et) {
						base = et
					}
				} else if !types.EqualTypes(base, et) && !types.ContainsAny(et) {
					same = false
				}
			}
			if same && base != nil {
				elemType = toGoTypeFromType(base)
				if lt, ok := base.(types.ListType); ok {
					for i, e := range elems {
						if ll, ok2 := e.(*ListLit); ok2 && ll.ElemType == "any" && len(ll.Elems) == 0 {
							ll.ElemType = toGoTypeFromType(lt.Elem)
							elems[i] = ll
						}
					}
				}
			}
		}
		return &ListLit{ElemType: elemType, Elems: elems}, nil
	case p.Map != nil:
		keys := make([]Expr, len(p.Map.Items))
		vals := make([]Expr, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var ke Expr
			if k, ok := types.SimpleStringKey(it.Key); ok {
				ke = &StringLit{Value: k}
				if _, exists := fieldTypeGuess[k]; !exists {
					vtype := types.ExprType(it.Value, env)
					if ml := mapLiteralExpr(it.Value); ml != nil {
						if sm, ok := types.InferSimpleMap(ml, env); ok {
							vtype = sm
						}
					}
					fieldTypeGuess[k] = toGoTypeFromType(vtype)
				}
			} else {
				var err error
				ke, err = compileExpr(it.Key, env, "")
				if err != nil {
					return nil, err
				}
			}
			ve, err := compileExpr(it.Value, env, "")
			if err != nil {
				return nil, err
			}
			keys[i] = ke
			vals[i] = ve
		}
		mt, _ := types.TypeOfPrimaryBasic(p, env).(types.MapType)
		if sm, ok := types.InferSimpleMap(p.Map, env); ok {
			mt = sm
		}
		return &MapLit{KeyType: toGoTypeFromType(mt.Key), ValueType: toGoTypeFromType(mt.Value), Keys: keys, Values: vals}, nil
	case p.Struct != nil:
		st, ok := env.GetStruct(p.Struct.Name)
		if !ok {
			return nil, fmt.Errorf("unknown struct %s", p.Struct.Name)
		}
		names := make([]string, len(st.Order))
		fields := make([]Expr, len(st.Order))
		for i, name := range st.Order {
			names[i] = name
			var exprNode *parser.Expr
			for _, f := range p.Struct.Fields {
				if f.Name == name {
					exprNode = f.Value
					break
				}
			}
			if exprNode == nil {
				return nil, fmt.Errorf("missing field %s", name)
			}
			ex, err := compileExpr(exprNode, env, "")
			if err != nil {
				return nil, err
			}
			if ml, ok := ex.(*MapLit); ok {
				if stype, ok2 := env.GetStruct(p.Struct.Name); ok2 {
					if ft, ok3 := stype.Fields[name]; ok3 {
						updateMapLitTypes(ml, ft)
					}
				}
			}
			fields[i] = ex
		}
		return &StructLit{Name: p.Struct.Name, Fields: fields, Names: names}, nil
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
	case p.If != nil:
		return compileIfExpr(p.If, env)
	case p.Group != nil:
		return compileExpr(p.Group, env, "")
	case p.Match != nil:
		return compileMatchExpr(p.Match, env)
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type, env)
		if err == nil {
			return expr, nil
		}
		return nil, err
	case p.Query != nil:
		return compileQueryExpr(p.Query, env, base)
	case p.FunExpr != nil:
		return compileFunExpr(p.FunExpr, env)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if _, ok := env.FindUnionByVariant(p.Selector.Root); ok {
			return &StructLit{Name: p.Selector.Root}, nil
		}
		return &VarRef{Name: p.Selector.Root}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func toGoType(t *parser.TypeRef, env *types.Env) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "bigint":
			usesBigInt = true
			return "*big.Int"
		case "float":
			return "float64"
		case "string":
			return "string"
		case "bool":
			return "bool"
		default:
			if env != nil {
				if _, ok := env.GetStruct(*t.Simple); ok {
					return *t.Simple
				}
			}
		}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return "[]" + toGoType(t.Generic.Args[0], env)
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return fmt.Sprintf("map[%s]%s", toGoType(t.Generic.Args[0], env), toGoType(t.Generic.Args[1], env))
			}
		case "option":
			if len(t.Generic.Args) == 1 {
				return "*" + toGoType(t.Generic.Args[0], env)
			}
		}
	}
	if t.Struct != nil {
		return "map[string]any"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = toGoType(p, env)
		}
		ret := toGoType(t.Fun.Return, env)
		if ret == "" {
			ret = "any"
		}
		if ret != "any" {
			return fmt.Sprintf("func(%s) %s", strings.Join(params, ", "), ret)
		}
		return fmt.Sprintf("func(%s) any", strings.Join(params, ", "))
	}
	return "any"
}

func toGoTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.BigIntType:
		usesBigInt = true
		return "*big.Int"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.BigRatType, types.FloatType:
		return "float64"
	case types.ListType:
		elem := toGoTypeFromType(tt.Elem)
		if elem == "" {
			elem = "any"
		}
		return "[]" + elem
	case types.MapType:
		key := toGoTypeFromType(tt.Key)
		if key == "" {
			key = "any"
		}
		val := toGoTypeFromType(tt.Value)
		if val == "" {
			val = "any"
		}
		return fmt.Sprintf("map[%s]%s", key, val)
	case types.GroupType:
		key := toGoTypeFromType(tt.Key)
		if key == "" {
			key = "any"
		}
		elem := toGoTypeFromType(tt.Elem)
		if elem == "" {
			elem = "any"
		}
		return fmt.Sprintf("struct{Key %s; Items []%s}", key, elem)
	case types.StructType:
		return tt.Name
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = toGoTypeFromType(p)
		}
		ret := toGoTypeFromType(tt.Return)
		if ret == "" {
			if _, ok := tt.Return.(types.VoidType); ok {
				return fmt.Sprintf("func(%s)", strings.Join(params, ", "))
			}
			ret = "any"
		}
		return fmt.Sprintf("func(%s) %s", strings.Join(params, ", "), ret)
	case types.OptionType:
		return "*" + toGoTypeFromType(tt.Elem)
	case types.AnyType, types.VoidType:
		return ""
	}
	return "any"
}

func toTypeFromGoType(s string) types.Type {
	switch {
	case s == "int":
		return types.IntType{}
	case s == "float64":
		return types.FloatType{}
	case s == "string":
		return types.StringType{}
	case s == "bool":
		return types.BoolType{}
	case s == "*big.Int":
		return types.BigIntType{}
	case strings.HasPrefix(s, "[]"):
		return types.ListType{Elem: toTypeFromGoType(s[2:])}
	case strings.HasPrefix(s, "map[string]"):
		return types.MapType{Key: types.StringType{}, Value: toTypeFromGoType(s[len("map[string]"):])}
	case strings.HasPrefix(s, "*"):
		return types.OptionType{Elem: toTypeFromGoType(s[1:])}
	default:
		if st, ok := topEnv.GetStruct(s); ok {
			return st
		}
		return types.AnyType{}
	}
}

func isBasicOrderedType(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType,
		types.FloatType, types.BigRatType,
		types.StringType, types.BoolType:
		return true
	}
	return false
}

func isBoolExpr(e *parser.Expr) bool { return isBoolBinary(e.Binary) }

func isBoolBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return isBoolUnary(b.Left)
	}
	for _, op := range b.Right {
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return true
		}
	}
	return isBoolUnary(b.Left)
}

func isBoolUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	for _, op := range u.Ops {
		if op == "!" {
			return true
		}
	}
	return isBoolPostfix(u.Value)
}

func isBoolPostfix(pf *parser.PostfixExpr) bool {
	if pf == nil || len(pf.Ops) > 0 {
		return false
	}
	return isBoolPrimary(pf.Target)
}

func isBoolPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Bool != nil:
		return true
	case p.Group != nil:
		return isBoolExpr(p.Group)
	default:
		return false
	}
}

func isListExpr(e *parser.Expr) bool { return isListBinary(e.Binary) }

func isListBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return isListUnary(b.Left)
	}
	for _, op := range b.Right {
		switch op.Op {
		case "union", "union_all", "except", "intersect":
			return true
		}
	}
	return isListUnary(b.Left)
}

func isListUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isListPostfix(u.Value)
}

func isListPostfix(pf *parser.PostfixExpr) bool {
	if pf == nil {
		return false
	}
	if len(pf.Ops) > 0 {
		for _, op := range pf.Ops {
			if op.Index != nil && (op.Index.Colon != nil || op.Index.Colon2 != nil) {
				return true
			}
		}
	}
	return isListPrimary(pf.Target)
}

func isListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.List != nil:
		return true
	case p.Call != nil:
		switch p.Call.Func {
		case "append", "union", "union_all", "except", "intersect", "slice":
			return true
		}
	}
	return false
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

// Emit formats the Go AST back into source code.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("//go:build ignore\n\n")
	buf.Write(meta.Header("//"))
	buf.WriteString("package main\n\n")
	buf.WriteString("import (\n    \"fmt\"\n")
	if prog.Imports != nil {
		aliases := make([]string, 0, len(prog.Imports))
		for a := range prog.Imports {
			aliases = append(aliases, a)
		}
		sort.Strings(aliases)
		for _, a := range aliases {
			fmt.Fprintf(&buf, "    %s \"%s\"\n", a, prog.Imports[a])
		}
	}
	if prog.UseStrings {
		buf.WriteString("    \"strings\"\n")
	}
	if prog.UseStrconv {
		buf.WriteString("    \"strconv\"\n")
	}
	if prog.UseTime {
		buf.WriteString("    \"time\"\n")
		buf.WriteString("    \"os\"\n")
		buf.WriteString("    \"strconv\"\n")
	}
	if prog.UseJSON {
		buf.WriteString("    \"encoding/json\"\n")
	}
	if prog.UseSort {
		buf.WriteString("    \"sort\"\n")
	}
	if prog.UseBigInt {
		buf.WriteString("    \"math/big\"\n")
	}
	if prog.UseInput {
		buf.WriteString("    \"bufio\"\n")
		if !prog.UseTime {
			buf.WriteString("    \"os\"\n")
		}
	}
	buf.WriteString(")\n\n")

	if prog.UseTime {
		buf.WriteString("var seededNow bool\n")
		buf.WriteString("var nowSeed int64\n")
		buf.WriteString("func init() {\n")
		buf.WriteString("    if s := os.Getenv(\"MOCHI_NOW_SEED\"); s != \"\" {\n")
		buf.WriteString("        if v, err := strconv.ParseInt(s, 10, 64); err == nil {\n")
		buf.WriteString("            nowSeed = v\n")
		buf.WriteString("            seededNow = true\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("}\n")
		buf.WriteString("func _now() int {\n")
		buf.WriteString("    if seededNow {\n")
		buf.WriteString("        nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647\n")
		buf.WriteString("        return int(nowSeed)\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return int(time.Now().UnixNano())\n")
		buf.WriteString("}\n\n")
	}

	if prog.UseInput {
		buf.WriteString("var _scanner = bufio.NewScanner(os.Stdin)\n")
		buf.WriteString("func _input() string {\n")
		buf.WriteString("    if !_scanner.Scan() { return \"\" }\n")
		buf.WriteString("    return _scanner.Text()\n")
		buf.WriteString("}\n\n")
	}
	if prog.UseSubstr {
		buf.WriteString("func _substr(s string, start, end int) string {\n")
		buf.WriteString("    r := []rune(s)\n")
		buf.WriteString("    if start < 0 { start = 0 }\n")
		buf.WriteString("    if end > len(r) { end = len(r) }\n")
		buf.WriteString("    if start > len(r) { start = len(r) }\n")
		buf.WriteString("    if end < start { end = start }\n")
		buf.WriteString("    return string(r[start:end])\n")
		buf.WriteString("}\n\n")
	}
	if prog.UseFloatConv {
		buf.WriteString("func _toFloat(v any) float64 {\n")
		buf.WriteString("    switch t := v.(type) {\n")
		buf.WriteString("    case int: return float64(t)\n")
		buf.WriteString("    case int64: return float64(t)\n")
		buf.WriteString("    case float64: return t\n")
		buf.WriteString("    default: return 0\n")
		buf.WriteString("    }\n")
		buf.WriteString("}\n\n")
	}

	// no runtime helper functions needed
	for _, s := range prog.Stmts {
		switch st := s.(type) {
		case *TypeDeclStmt:
			st.emit(&buf)
			buf.WriteString("\n\n")
		case *UnionDeclStmt:
			st.emit(&buf)
			buf.WriteString("\n\n")
		case *FuncDecl:
			st.emit(&buf)
			buf.WriteString("\n\n")
		case *VarDecl:
			if st.Global {
				st.emit(&buf)
				buf.WriteString("\n\n")
			}
		}
	}
	buf.WriteString("func main() {\n")
	for _, s := range prog.Stmts {
		if vd, ok := s.(*VarDecl); ok && vd.Global {
			continue
		}
		if _, ok := s.(*FuncDecl); ok {
			continue
		}
		if _, ok := s.(*TypeDeclStmt); ok {
			continue
		}
		if _, ok := s.(*UnionDeclStmt); ok {
			continue
		}
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("}\n")
	out, err := format.Source(buf.Bytes())
	if err == nil {
		return out
	}
	return buf.Bytes()
}

// print converts prog to an ast.Node and prints it.
func print(prog *Program) {
	node := toNodeProg(prog)
	fmt.Print(node.String())
}

func toNodeProg(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, toNodeStmt(s))
	}
	return n
}

func toNodeStmt(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *PrintStmt:
		n := &ast.Node{Kind: "print"}
		for _, a := range st.Args {
			n.Children = append(n.Children, toNodeExpr(a))
		}
		return n
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{toNodeExpr(st.Expr)}}
	case *VarDecl:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		then := &ast.Node{Kind: "then"}
		for _, t := range st.Then {
			then.Children = append(then.Children, toNodeStmt(t))
		}
		n.Children = append(n.Children, then)
		if len(st.Else) > 0 {
			els := &ast.Node{Kind: "else"}
			for _, e := range st.Else {
				els.Children = append(els.Children, toNodeStmt(e))
			}
			n.Children = append(n.Children, els)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "forrange", Value: st.Name}
		n.Children = append(n.Children, toNodeExpr(st.Start), toNodeExpr(st.End))
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForEachStmt:
		n := &ast.Node{Kind: "foreach", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = append(n.Children, toNodeExpr(st.Value))
		}
		return n
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p.Name})
		}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, params, body)
		return n
	case *IndexAssignStmt:
		return &ast.Node{Kind: "indexassign", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Index), toNodeExpr(st.Value)}}
	case *UpdateStmt:
		n := &ast.Node{Kind: "update", Value: st.Target}
		for i, f := range st.Fields {
			n.Children = append(n.Children, &ast.Node{Kind: "field", Value: f, Children: []*ast.Node{toNodeExpr(st.Values[i])}})
		}
		if st.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{toNodeExpr(st.Cond)}})
		}
		return n
	case *SaveStmt:
		n := &ast.Node{Kind: "save"}
		if st.Src != nil {
			n.Children = append(n.Children, toNodeExpr(st.Src))
		}
		return n
	case *SetStmt:
		return &ast.Node{Kind: "set", Children: []*ast.Node{toNodeExpr(st.Target), toNodeExpr(st.Value)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func toNodeExpr(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, toNodeExpr(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int"}
	case *FloatLit:
		return &ast.Node{Kind: "float"}
	case *BoolLit:
		return &ast.Node{Kind: "bool"}
	case *VarRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, toNodeExpr(e))
		}
		return n
	case *StructLit:
		n := &ast.Node{Kind: "struct", Value: ex.Name}
		for i, f := range ex.Fields {
			field := &ast.Node{Kind: ex.Names[i], Children: []*ast.Node{toNodeExpr(f)}}
			n.Children = append(n.Children, field)
		}
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{toNodeExpr(ex.X), toNodeExpr(ex.Index)}}
	case *FieldExpr:
		n := &ast.Node{Kind: "field", Value: ex.Name}
		n.Children = append(n.Children, toNodeExpr(ex.X))
		return n
	case *SliceExpr:
		n := &ast.Node{Kind: "slice"}
		n.Children = append(n.Children, toNodeExpr(ex.X))
		if ex.Start != nil {
			n.Children = append(n.Children, toNodeExpr(ex.Start))
		}
		if ex.End != nil {
			n.Children = append(n.Children, toNodeExpr(ex.End))
		}
		return n
	case *RuneSliceExpr:
		return &ast.Node{Kind: "runes", Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *KeysExpr:
		return &ast.Node{Kind: "keys", Children: []*ast.Node{toNodeExpr(ex.Map)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{toNodeExpr(ex.Map)}}
	case *ListStringExpr:
		return &ast.Node{Kind: "liststr", Children: []*ast.Node{toNodeExpr(ex.List)}}
	case *FloatStringExpr:
		return &ast.Node{Kind: "floatstr", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *JsonExpr:
		return &ast.Node{Kind: "json", Children: []*ast.Node{toNodeExpr(ex.Value)}}
	case *ContainsExpr:
		return &ast.Node{Kind: "contains", Children: []*ast.Node{toNodeExpr(ex.Collection), toNodeExpr(ex.Value)}}
	case *ExistsExpr:
		return toNodeExpr(ex.Expr)
	case *UnionExpr:
		return &ast.Node{Kind: "union", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *UnionAllExpr:
		return &ast.Node{Kind: "unionall", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *ExceptExpr:
		return &ast.Node{Kind: "except", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *IntersectExpr:
		return &ast.Node{Kind: "intersect", Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *AtoiExpr:
		return &ast.Node{Kind: "atoi", Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *QueryExpr:
		n := &ast.Node{Kind: "query"}
		n.Children = append(n.Children, toNodeExpr(ex.Src))
		for _, f := range ex.Froms {
			fn := &ast.Node{Kind: "from", Value: f.Var, Children: []*ast.Node{toNodeExpr(f.Src)}}
			n.Children = append(n.Children, fn)
		}
		for _, j := range ex.Joins {
			jn := &ast.Node{Kind: "join", Value: j.Var}
			jn.Children = append(jn.Children, toNodeExpr(j.Src))
			if j.On != nil {
				jn.Children = append(jn.Children, &ast.Node{Kind: "on", Children: []*ast.Node{toNodeExpr(j.On)}})
			}
			if j.Side != "" {
				jn.Children = append(jn.Children, &ast.Node{Kind: j.Side})
			}
			n.Children = append(n.Children, jn)
		}
		if ex.Where != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{toNodeExpr(ex.Where)}})
		}
		n.Children = append(n.Children, &ast.Node{Kind: "select", Children: []*ast.Node{toNodeExpr(ex.Select)}})
		return n
	case *GroupQueryExpr:
		n := &ast.Node{Kind: "group_by"}
		n.Children = append(n.Children, toNodeExpr(ex.Src), toNodeExpr(ex.Key), &ast.Node{Kind: "name", Value: ex.GroupVar}, &ast.Node{Kind: "select", Children: []*ast.Node{toNodeExpr(ex.Select)}})
		if ex.Having != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "having", Children: []*ast.Node{toNodeExpr(ex.Having)}})
		}
		return n
	case *GroupJoinQueryExpr:
		n := &ast.Node{Kind: "group_join"}
		n.Children = append(n.Children, toNodeExpr(ex.Src))
		for _, f := range ex.Froms {
			n.Children = append(n.Children, &ast.Node{Kind: "from", Value: f.Var, Children: []*ast.Node{toNodeExpr(f.Src)}})
		}
		for _, j := range ex.Joins {
			jn := &ast.Node{Kind: "join", Value: j.Var}
			jn.Children = append(jn.Children, toNodeExpr(j.Src))
			if j.On != nil {
				jn.Children = append(jn.Children, &ast.Node{Kind: "on", Children: []*ast.Node{toNodeExpr(j.On)}})
			}
			if j.Side != "" {
				jn.Children = append(jn.Children, &ast.Node{Kind: j.Side})
			}
			n.Children = append(n.Children, jn)
		}
		if ex.Where != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{toNodeExpr(ex.Where)}})
		}
		n.Children = append(n.Children, toNodeExpr(ex.Key), &ast.Node{Kind: "name", Value: ex.GroupVar}, &ast.Node{Kind: "select", Children: []*ast.Node{toNodeExpr(ex.Select)}})
		if ex.Having != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "having", Children: []*ast.Node{toNodeExpr(ex.Having)}})
		}
		return n
	case *OuterJoinExpr:
		n := &ast.Node{Kind: "outer_join"}
		n.Children = append(n.Children, toNodeExpr(ex.LeftSrc), toNodeExpr(ex.RightSrc))
		if ex.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "on", Children: []*ast.Node{toNodeExpr(ex.Cond)}})
		}
		n.Children = append(n.Children, &ast.Node{Kind: "select", Children: []*ast.Node{toNodeExpr(ex.Select)}})
		return n
	case *AssertExpr:
		n := &ast.Node{Kind: "assert", Value: ex.Type}
		n.Children = append(n.Children, toNodeExpr(ex.Expr))
		return n
	case *IfExpr:
		n := &ast.Node{Kind: "ifexpr"}
		n.Children = append(n.Children, toNodeExpr(ex.Cond), toNodeExpr(ex.Then))
		if ex.Else != nil {
			n.Children = append(n.Children, toNodeExpr(ex.Else))
		}
		return n
	case *NotExpr:
		return &ast.Node{Kind: "not", Children: []*ast.Node{toNodeExpr(ex.Expr)}}
	case *FuncLit:
		n := &ast.Node{Kind: "funclit"}
		params := &ast.Node{Kind: "params"}
		for _, p := range ex.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p.Name})
		}
		body := &ast.Node{Kind: "body"}
		for _, b := range ex.Body {
			body.Children = append(body.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, params, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
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

func isVarRef(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	v := u.Value
	if len(v.Ops) != 0 || v.Target == nil {
		return false
	}
	sel := v.Target.Selector
	return sel != nil && sel.Root == name && len(sel.Tail) == 0
}

func substituteFieldVars(e Expr, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *VarRef:
		if fields[ex.Name] {
			return &FieldExpr{X: &VarRef{Name: "item"}, Name: ex.Name}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFieldVars(ex.Left, fields)
		ex.Right = substituteFieldVars(ex.Right, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = substituteFieldVars(ex.Args[i], fields)
		}
		return ex
	case *FieldExpr:
		ex.X = substituteFieldVars(ex.X, fields)
		return ex
	case *IndexExpr:
		ex.X = substituteFieldVars(ex.X, fields)
		ex.Index = substituteFieldVars(ex.Index, fields)
		return ex
	case *NotExpr:
		ex.Expr = substituteFieldVars(ex.Expr, fields)
		return ex
	case *IfExpr:
		ex.Cond = substituteFieldVars(ex.Cond, fields)
		ex.Then = substituteFieldVars(ex.Then, fields)
		if ex.Else != nil {
			ex.Else = substituteFieldVars(ex.Else, fields)
		}
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFieldVars(ex.Elems[i], fields)
		}
		return ex
	case *SliceExpr:
		ex.X = substituteFieldVars(ex.X, fields)
		if ex.Start != nil {
			ex.Start = substituteFieldVars(ex.Start, fields)
		}
		if ex.End != nil {
			ex.End = substituteFieldVars(ex.End, fields)
		}
		return ex
	case *UnionExpr:
		ex.Left = substituteFieldVars(ex.Left, fields)
		ex.Right = substituteFieldVars(ex.Right, fields)
		return ex
	case *UnionAllExpr:
		ex.Left = substituteFieldVars(ex.Left, fields)
		ex.Right = substituteFieldVars(ex.Right, fields)
		return ex
	case *ExceptExpr:
		ex.Left = substituteFieldVars(ex.Left, fields)
		ex.Right = substituteFieldVars(ex.Right, fields)
		return ex
	case *IntersectExpr:
		ex.Left = substituteFieldVars(ex.Left, fields)
		ex.Right = substituteFieldVars(ex.Right, fields)
		return ex
	case *AtoiExpr:
		ex.Expr = substituteFieldVars(ex.Expr, fields)
		return ex
	case *AssertExpr:
		ex.Expr = substituteFieldVars(ex.Expr, fields)
		return ex
	default:
		return ex
	}
}

func compileGroupKey(e *parser.Expr, env *types.Env, base string) (Expr, types.Type, error) {
	keyExpr, err := compileExpr(e, env, base)
	if err != nil {
		return nil, types.AnyType{}, err
	}
	keyType := types.ExprType(e, env)
	if ml := mapLiteral(e); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, env); ok {
			structCount++
			baseName := fmt.Sprintf("Key%d", structCount)
			if base != "" {
				baseName = structNameFromVar(base) + "Key"
			}
			name := types.UniqueStructName(baseName, topEnv, nil)
			st.Name = name
			if topEnv != nil {
				topEnv.SetStruct(name, st)
			}
			fieldsDecl := make([]ParamDecl, len(st.Order))
			vals := make([]Expr, len(st.Order))
			for i, it := range ml.Items {
				fieldsDecl[i] = ParamDecl{Name: st.Order[i], Type: toGoTypeFromType(st.Fields[st.Order[i]])}
				v, err := compileExpr(it.Value, env, "")
				if err != nil {
					return nil, types.AnyType{}, err
				}
				vals[i] = v
			}
			extraDecls = append(extraDecls, &TypeDeclStmt{Name: name, Fields: fieldsDecl})
			keyExpr = &StructLit{Name: name, Fields: vals, Names: st.Order}
			keyType = st
		}
	}
	return keyExpr, keyType, nil
}
