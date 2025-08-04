//go:build slow

package swifttrans

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"time"

	"gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

var usesNow bool
var usesLookupHost bool
var usesNum bool
var usesInt bool
var usesKeys bool
var usesMem bool
var usesBigInt bool
var usesPad bool
var usesRepeat bool
var usesRat bool
var usesSHA256 bool
var usesAppend bool
var usesLen bool
var usesSplit bool
var usesFetch bool
var fetchStructs map[string]bool
var funcMutParams map[string][]bool
var funcInOutParams map[string][]bool
var currentUpdated map[string]bool
var currentAnyUpdated map[string]bool
var classStructs map[string]bool
var nonCodableStructs map[string]bool

var swiftKeywords = map[string]struct{}{
	"associatedtype": {}, "class": {}, "deinit": {}, "enum": {}, "extension": {},
	"func": {}, "import": {}, "init": {}, "inout": {}, "let": {}, "operator": {},
	"protocol": {}, "subscript": {}, "typealias": {}, "var": {}, "break": {},
	"case": {}, "continue": {}, "default": {}, "defer": {}, "do": {}, "else": {},
	"fallthrough": {}, "for": {}, "guard": {}, "if": {}, "in": {}, "repeat": {},
	"return": {}, "switch": {}, "where": {}, "while": {}, "is": {},
}

func esc(name string) string {
	if _, ok := swiftKeywords[name]; ok {
		return "`" + name + "`"
	}
	return name
}

// Program is a sequence of Swift statements.
type Program struct {
	Stmts         []Stmt
	UseNow        bool
	UseLookupHost bool
	UseNum        bool
	UseInt        bool
	UseKeys       bool
	UseMem        bool
	UseBigInt     bool
	UsePad        bool
	UseRepeat     bool
	UseRat        bool
	UseSHA256     bool
	UseAppend     bool
	UseLen        bool
	UseSplit      bool
	UseFetch      bool
	FetchStructs  map[string]bool
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type FunDecl struct {
	Name   string
	Params []Param
	Ret    string
	Body   []Stmt
}

type Param struct {
	Name     string
	Type     string
	InOut    bool
	Escaping bool
}

type ReturnStmt struct{ Expr Expr }

type CallExpr struct {
	Func string
	Args []Expr
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents a simple numeric range for loop.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForEachStmt represents iteration over a collection.
type ForEachStmt struct {
	Name    string
	Expr    Expr
	Body    []Stmt
	CastMap bool
	Keys    bool
}

type BreakStmt struct{}

type ContinueStmt struct{}

// RawStmt emits preformed Swift code directly.
type RawStmt struct{ Code string }

func (r *RawStmt) emit(w io.Writer) {
	io.WriteString(w, r.Code)
	if !strings.HasSuffix(r.Code, "\n") {
		io.WriteString(w, "\n")
	}
}

// FieldExpr represents property access on a struct.
type FieldExpr struct {
	Target Expr
	Name   string
}

func (fe *FieldExpr) emit(w io.Writer) {
	fe.Target.emit(w)
	fmt.Fprintf(w, ".%s", fe.Name)
}

// StructDef represents a simple struct type declaration.
type StructDef struct {
	Name    string
	Fields  []StructField
	Class   bool
	Codable bool
}

// StructField defines a single field within a struct.
type StructField struct {
	Name string
	Type string
}

// defaultValueForType returns a Swift literal representing the zero value for the
// provided type string. This is used to generate parameterless initializers for
// struct types so that structs can be instantiated without specifying every
// field explicitly.
func defaultValueForType(t string) string {
	if strings.Contains(t, "->") {
		// Function type: generate a closure returning the zero value of the return type.
		parts := strings.SplitN(t, "->", 2)
		params := strings.TrimSpace(parts[0])
		ret := strings.TrimSpace(parts[1])
		params = strings.TrimPrefix(params, "(")
		params = strings.TrimSuffix(params, ")")
		var args []string
		if strings.TrimSpace(params) != "" {
			ps := strings.Split(params, ",")
			for i, p := range ps {
				p = strings.TrimSpace(p)
				args = append(args, fmt.Sprintf("_ arg%d: %s", i, p))
			}
		}
		return fmt.Sprintf("{ (%s) -> %s in %s }", strings.Join(args, ", "), ret, defaultValueForType(ret))
	}
	switch t {
	case "Int", "Int64", "Int32", "UInt", "UInt64", "UInt32", "Double", "Float":
		return "0"
	case "Bool":
		return "false"
	case "String":
		return "\"\""
	default:
		if strings.HasPrefix(t, "[") {
			if strings.Contains(t, ":") {
				return "[:]"
			}
			return "[]"
		}
		return t + "()"
	}
}

func (sd *StructDef) emit(w io.Writer) {
	kind := "struct"
	if sd.Class {
		kind = "class"
	}
	codable := ""
	if sd.Codable {
		codable = ": Codable"
	}
	fmt.Fprintf(w, "%s %s%s {\n", kind, sd.Name, codable)
	for _, f := range sd.Fields {
		fmt.Fprintf(w, "    var %s: %s\n", f.Name, f.Type)
	}
	// emit a default initializer so structs can be instantiated without parameters
	fmt.Fprint(w, "    init() {\n")
	for _, f := range sd.Fields {
		fmt.Fprintf(w, "        self.%s = %s\n", f.Name, defaultValueForType(f.Type))
	}
	fmt.Fprint(w, "    }\n")
	if len(sd.Fields) > 0 {
		fmt.Fprint(w, "    init(")
		for i, f := range sd.Fields {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			esc := ""
			if strings.Contains(f.Type, "->") {
				esc = "@escaping "
			}
			fmt.Fprintf(w, "%s: %s%s", f.Name, esc, f.Type)
		}
		fmt.Fprint(w, ") {\n")
		for _, f := range sd.Fields {
			fmt.Fprintf(w, "        self.%s = %s\n", f.Name, f.Name)
		}
		fmt.Fprint(w, "    }\n")
	}
	fmt.Fprint(w, "}\n")
}

// UnionDef represents a simple enum type declaration for a union.
type UnionDef struct {
	Name     string
	Variants []UnionVariant
}

// UnionVariant defines a single case within a union enum.
type UnionVariant struct {
	Name   string
	Fields []StructField
}

func (ud *UnionDef) emit(w io.Writer) {
	fmt.Fprintf(w, "indirect enum %s {\n", ud.Name)
	for _, v := range ud.Variants {
		fmt.Fprintf(w, "    case %s(", v.Name)
		for i, f := range v.Fields {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			fmt.Fprintf(w, "%s: %s", f.Name, f.Type)
		}
		fmt.Fprint(w, ")\n")
	}
	fmt.Fprint(w, "}\n")
}

type IfStmt struct {
	Cond   Expr
	Then   []Stmt
	ElseIf *IfStmt
	Else   []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if ")
	i.Cond.emit(w)
	fmt.Fprint(w, " {\n")
	for _, s := range i.Then {
		s.emit(w)
	}
	fmt.Fprint(w, "}")
	if i.ElseIf != nil {
		fmt.Fprint(w, " else ")
		i.ElseIf.emit(w)
	} else if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, s := range i.Else {
			s.emit(w)
		}
		fmt.Fprint(w, "}")
	}
	fmt.Fprint(w, "\n")
}

type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

// FunExpr represents an anonymous function expression.
type FunExpr struct {
	Params []Param
	Ret    string
	Body   Expr
}

// FunBlock represents an anonymous function with statement body.
type FunBlock struct {
	Params []Param
	Ret    string
	Body   []Stmt
}

func (f *FunBlock) emit(w io.Writer) {
	fmt.Fprint(w, "{ (")
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			if p.Escaping {
				fmt.Fprintf(w, "%s: @escaping %s", p.Name, p.Type)
			} else {
				fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
			}
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if f.Ret != "" {
		fmt.Fprintf(w, " -> %s", f.Ret)
	}
	fmt.Fprint(w, " in\n")
	for _, s := range f.Body {
		s.emit(w)
	}
	fmt.Fprint(w, "}")
}

// UnionMatchExpr represents a switch over a union enum value.
type UnionMatchExpr struct {
	Target Expr
	Cases  []UnionMatchCase
	Type   string
}

type UnionMatchCase struct {
	Variant  string
	Bindings []string
	Body     Expr
}

func (f *FunExpr) emit(w io.Writer) {
	fmt.Fprint(w, "{ (")
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			if p.Escaping {
				fmt.Fprintf(w, "%s: @escaping %s", p.Name, p.Type)
			} else {
				fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
			}
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if f.Ret != "" {
		fmt.Fprintf(w, " -> %s", f.Ret)
	}
	fmt.Fprint(w, " in ")
	if f.Body != nil {
		f.Body.emit(w)
	}
	fmt.Fprint(w, " }")
}

func (m *UnionMatchExpr) emit(w io.Writer) {
	ret := "Any"
	if m.Type != "" {
		ret = m.Type
	}
	fmt.Fprintf(w, "{ () -> %s in\n", ret)
	fmt.Fprint(w, "switch ")
	m.Target.emit(w)
	fmt.Fprint(w, " {\n")
	for _, c := range m.Cases {
		fmt.Fprintf(w, "case let .%s(", c.Variant)
		for i, b := range c.Bindings {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			if b != "" {
				fmt.Fprint(w, b)
			} else {
				fmt.Fprint(w, "_")
			}
		}
		fmt.Fprint(w, "):\n    return ")
		c.Body.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprintf(w, "default:\n    var z: %s\n    return z\n}\n}()", ret)
}

func (c *CondExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	c.Cond.emit(w)
	fmt.Fprint(w, " ? ")
	c.Then.emit(w)
	fmt.Fprint(w, " : ")
	c.Else.emit(w)
	fmt.Fprint(w, ")")
}

type PrintStmt struct{ Exprs []Expr }

func (p *PrintStmt) emit(w io.Writer) {
	fmt.Fprint(w, "print(")
	for i, e := range p.Exprs {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprint(w, "_p(")
		e.emit(w)
		fmt.Fprint(w, ")")
	}
	fmt.Fprint(w, ")\n")
}

type ExprStmt struct{ Expr Expr }

func (e *ExprStmt) emit(w io.Writer) {
	io.WriteString(w, "_ = ")
	e.Expr.emit(w)
	fmt.Fprint(w, "\n")
}

type EmptyStmt struct{}

func (EmptyStmt) emit(w io.Writer) {}

type VarDecl struct {
	Name  string
	Const bool
	Type  string
	Expr  Expr
}

func (v *VarDecl) emit(w io.Writer) {
	kw := "var"
	if v.Const && v.Expr != nil {
		kw = "let"
	}
	fmt.Fprint(w, kw+" "+esc(v.Name))
	typ := v.Type
	if typ == "" {
		switch e := v.Expr.(type) {
		case *MapLit:
			if len(e.Keys) == 0 {
				typ = "[String: Any]"
			}
		case *ArrayLit:
			if len(e.Elems) == 0 {
				typ = "[Any]"
			}
		}
	}
	if typ != "" {
		fmt.Fprintf(w, ": %s", typ)
	}
	if v.Expr != nil {
		fmt.Fprint(w, " = ")
		if typ == "BigInt" {
			fmt.Fprint(w, "BigInt(")
			v.Expr.emit(w)
			fmt.Fprint(w, ")")
		} else {
			v.Expr.emit(w)
		}
	}
	fmt.Fprint(w, "\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprint(w, esc(a.Name)+" = ")
	a.Expr.emit(w)
	fmt.Fprint(w, "\n")
}

type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (ia *IndexAssignStmt) emit(w io.Writer) {
	ia.Target.emit(w)
	fmt.Fprint(w, " = ")
	ia.Value.emit(w)
	fmt.Fprint(w, "\n")
}

// SaveStmt writes a collection of values to stdout in JSONL format.
type SaveStmt struct {
	Src Expr
}

func (s *SaveStmt) emit(w io.Writer) {
	fmt.Fprint(w, "for _item in ")
	s.Src.emit(w)
	fmt.Fprint(w, " {\n")
	fmt.Fprint(w, "    let obj = toJsonObj(_item)\n")
	fmt.Fprint(w, "    if let data = try? JSONSerialization.data(withJSONObject: obj) {\n")
	fmt.Fprint(w, "        print(String(data: data, encoding: .utf8)!)\n")
	fmt.Fprint(w, "    }\n")
	fmt.Fprint(w, "}\n")
}

// UpdateStmt updates elements in a list of structs in place.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
	Locals []string
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for i in 0..<%s.count {\n", u.Target)
	fmt.Fprintf(w, "    var item = %s[i]\n", u.Target)
	for _, name := range u.Locals {
		fmt.Fprintf(w, "    var %s = item.%s\n", name, name)
	}
	if u.Cond != nil {
		fmt.Fprint(w, "    if ")
		u.Cond.emit(w)
		fmt.Fprint(w, " {\n")
		for i, f := range u.Fields {
			fmt.Fprintf(w, "        item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "    }\n")
	} else {
		for i, f := range u.Fields {
			fmt.Fprintf(w, "    item.%s = ", f)
			u.Values[i].emit(w)
			fmt.Fprint(w, "\n")
		}
	}
	fmt.Fprintf(w, "    %s[i] = item\n", u.Target)
	fmt.Fprint(w, "}\n")
}

// ExpectStmt represents an expectation check within a test block.
type ExpectStmt struct{ Cond Expr }

func (e *ExpectStmt) emit(w io.Writer) {
	fmt.Fprint(w, "assert(")
	e.Cond.emit(w)
	fmt.Fprint(w, ")\n")
}

// BlockStmt groups a list of statements without additional syntax.
type BlockStmt struct{ Stmts []Stmt }

func (b *BlockStmt) emit(w io.Writer) {
	for _, s := range b.Stmts {
		s.emit(w)
	}
}

// BenchStmt represents a benchmarking block measuring runtime duration
// and memory usage of the enclosed statements. Memory usage is approximated
// by reading `/proc/self/status` on Linux systems.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(w io.Writer) {
	fmt.Fprint(w, "do {\n")
	fmt.Fprint(w, "    let _benchMemStart = _mem()\n")
	fmt.Fprint(w, "    let _benchStart = _now()\n")
	for _, st := range b.Body {
		st.emit(w)
	}
	fmt.Fprint(w, "    let _benchEnd = _now()\n")
	fmt.Fprint(w, "    let _benchMemEnd = _mem()\n")
	fmt.Fprintf(w, "    print(\"{\\n  \\\"duration_us\\\": \\((_benchEnd - _benchStart) / 1000),\\n  \\\"memory_bytes\\\": \\(_benchMemEnd - _benchMemStart),\\n  \\\"name\\\": \\\"%s\\\"\\n}\")\n", b.Name)
	fmt.Fprint(w, "}\n")
}

type LitExpr struct {
	Value    string
	IsString bool
}

func (l *LitExpr) emit(w io.Writer) {
	if l.IsString {
		io.WriteString(w, swiftQuote(l.Value))
	} else {
		fmt.Fprint(w, l.Value)
	}
}

type NameExpr struct {
	Name    string
	AsItems bool
}

func (n *NameExpr) emit(w io.Writer) {
	if n.AsItems {
		fmt.Fprintf(w, "%s[\"items\"] as! [[String: Any]]", esc(n.Name))
	} else {
		fmt.Fprint(w, esc(n.Name))
	}
}

func swiftQuote(s string) string {
	var buf bytes.Buffer
	buf.WriteByte('"')
	for _, r := range s {
		switch r {
		case '\n':
			buf.WriteString("\\n")
		case '\r':
			buf.WriteString("\\r")
		case '\t':
			buf.WriteString("\\t")
		case '\\':
			buf.WriteString("\\\\")
		case '"':
			buf.WriteString("\\\"")
		default:
			if r < 0x20 || r == 0x7f {
				fmt.Fprintf(&buf, "\\u{%04X}", r)
			} else {
				buf.WriteRune(r)
			}
		}
	}
	buf.WriteByte('"')
	return buf.String()
}

type ArrayLit struct{ Elems []Expr }

func (a *ArrayLit) emit(w io.Writer) {
	fmt.Fprint(w, "[")
	for i, e := range a.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "]")
}

// MapLit represents a dictionary literal.
type MapLit struct {
	Keys   []Expr
	Values []Expr
}

func (m *MapLit) emit(w io.Writer) {
	if len(m.Keys) == 0 {
		fmt.Fprint(w, "[:]")
		return
	}
	fmt.Fprint(w, "[")
	for i := range m.Keys {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		m.Keys[i].emit(w)
		fmt.Fprint(w, ": ")
		if al, ok := m.Values[i].(*ArrayLit); ok && len(al.Elems) == 0 {
			fmt.Fprint(w, "[] as [Any]")
		} else {
			m.Values[i].emit(w)
		}
	}
	fmt.Fprint(w, "]")
}

// MapGetExpr represents m.get(k, default) -> m[k] ?? default
type MapGetExpr struct {
	Map     Expr
	Key     Expr
	Default Expr
}

func (mg *MapGetExpr) emit(w io.Writer) {
	mg.Map.emit(w)
	fmt.Fprint(w, "[")
	mg.Key.emit(w)
	fmt.Fprint(w, "] ?? ")
	mg.Default.emit(w)
}

// StructInit represents initialization of a struct value.
type StructInit struct {
	Name   string
	Fields []FieldInit
}

// FieldInit pairs a field name with its initializing expression.
type FieldInit struct {
	Name  string
	Value Expr
}

// UnionInit represents initialization of a union case.
type UnionInit struct {
	Union   string
	Variant string
	Fields  []FieldInit
}

func (si *StructInit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", si.Name)
	for i, f := range si.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s: ", f.Name)
		f.Value.emit(w)
	}
	fmt.Fprint(w, ")")
}

func (ui *UnionInit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s.%s(", ui.Union, ui.Variant)
	for i, f := range ui.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s: ", f.Name)
		f.Value.emit(w)
	}
	fmt.Fprint(w, ")")
}

// MapStringExpr renders a dictionary as a JSON-like string.
type MapStringExpr struct{ Value Expr }

func (ms *MapStringExpr) emit(w io.Writer) {
	fmt.Fprint(w, "String(describing: ")
	ms.Value.emit(w)
	fmt.Fprint(w, ")")
}

// ArrayStringExpr renders a list as a compact string without spaces.
type ArrayStringExpr struct{ Value Expr }

func (as *ArrayStringExpr) emit(w io.Writer) {
	fmt.Fprint(w, "\"[\" + ")
	as.Value.emit(w)
	fmt.Fprint(w, ".map{ String(describing: $0) }.joined(separator: \",\") + \"]\"")
}

// LenExpr renders a length expression, optionally casting to String first.
type LenExpr struct {
	Value    Expr
	AsString bool
}

func (le *LenExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	if le.AsString {
		fmt.Fprint(w, "String(describing: ")
		le.Value.emit(w)
		fmt.Fprint(w, ").count")
	} else {
		fmt.Fprint(w, "(")
		le.Value.emit(w)
		fmt.Fprint(w, ").count")
	}
	fmt.Fprint(w, ")")
}

// queryFrom represents a secondary source in a query expression.
type queryFrom struct {
	Var string
	Src Expr
}

// queryJoin represents a join clause.
type queryJoin struct {
	Var string
	Src Expr
	On  Expr
}

// GroupByExpr represents a simple query with grouping support.
type GroupByExpr struct {
	Var    string
	Source Expr
	Froms  []queryFrom
	Joins  []queryJoin
	Key    Expr
	Name   string
	Sort   Expr
	Where  Expr
	Select Expr
	Having Expr
}

// QueryExpr represents a basic comprehension supporting joins.
type QueryExpr struct {
	Var    string
	Src    Expr
	Froms  []queryFrom
	Joins  []queryJoin
	Where  Expr
	Sort   Expr
	Skip   Expr
	Take   Expr
	Select Expr
	Elem   string
}

// LeftJoinExpr represents a basic left join between two sources.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (q *QueryExpr) emit(w io.Writer) {
	if _, ok := q.Select.(*MapLit); ok {
		fmt.Fprint(w, "({ var _res: [[String: Any]] = []\n")
	} else if q.Elem != "" {
		fmt.Fprintf(w, "({ var _res: [%s] = []\n", q.Elem)
	} else {
		fmt.Fprint(w, "({ var _res: [Any] = []\n")
	}
	fmt.Fprintf(w, "for %s in ", esc(q.Var))
	q.Src.emit(w)
	fmt.Fprint(w, " {\n")
	for _, f := range q.Froms {
		fmt.Fprintf(w, "for %s in ", esc(f.Var))
		f.Src.emit(w)
		fmt.Fprint(w, " {\n")
	}
	for _, j := range q.Joins {
		fmt.Fprintf(w, "for %s in ", esc(j.Var))
		j.Src.emit(w)
		fmt.Fprint(w, " {\nif ")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, " {\n")
	}
	if q.Where != nil {
		fmt.Fprint(w, "if ")
		q.Where.emit(w)
		fmt.Fprint(w, " {\n")
	}
	fmt.Fprint(w, "_res.append(")
	q.Select.emit(w)
	fmt.Fprint(w, ")\n")
	if q.Where != nil {
		fmt.Fprint(w, "}\n")
	}
	for range q.Joins {
		fmt.Fprint(w, "}\n")
		fmt.Fprint(w, "}\n")
	}
	for range q.Froms {
		fmt.Fprint(w, "}\n")
	}
	fmt.Fprint(w, "}\n")
	if q.Sort != nil || q.Skip != nil || q.Take != nil {
		fmt.Fprint(w, "var _list = _res\n")
		if q.Sort != nil {
			fmt.Fprint(w, "_list.sort { left, right in\n")
			fmt.Fprintf(w, "var %s = left\n", esc(q.Var))
			fmt.Fprint(w, "let _ka = ")
			q.Sort.emit(w)
			fmt.Fprint(w, "\n")
			fmt.Fprintf(w, "%s = right\n", esc(q.Var))
			fmt.Fprint(w, "let _kb = ")
			q.Sort.emit(w)
			fmt.Fprint(w, "\nreturn String(describing: _ka) < String(describing: _kb)\n}\n")
		}
		if q.Skip != nil {
			fmt.Fprint(w, "_list = Array(_list.dropFirst(")
			q.Skip.emit(w)
			fmt.Fprint(w, "))\n")
		}
		if q.Take != nil {
			fmt.Fprint(w, "_list = Array(_list.prefix(")
			q.Take.emit(w)
			fmt.Fprint(w, "))\n")
		}
		fmt.Fprint(w, "return _list })()")
	} else {
		fmt.Fprint(w, "return _res })()")
	}
}

func (l *LeftJoinExpr) emit(w io.Writer) {
	fmt.Fprint(w, "({ var _res: [[String: Any]] = []\n")
	fmt.Fprintf(w, "for %s in ", esc(l.LeftVar))
	l.LeftSrc.emit(w)
	fmt.Fprint(w, " {\nvar matched = false\n")
	fmt.Fprintf(w, "for %s in ", esc(l.RightVar))
	l.RightSrc.emit(w)
	fmt.Fprint(w, " {\nif ")
	l.Cond.emit(w)
	fmt.Fprint(w, " {\nmatched = true\n_res.append(")
	l.Select.emit(w)
	fmt.Fprint(w, ")\n}\n}\n")
	fmt.Fprint(w, "if !matched {\n")
	fmt.Fprintf(w, "let %s: Any? = nil\n", esc(l.RightVar))
	fmt.Fprint(w, "_res.append(")
	l.Select.emit(w)
	fmt.Fprint(w, ")\n}\n}")
	fmt.Fprint(w, "\nreturn _res })()")
}

func (g *GroupByExpr) emit(w io.Writer) {
	fmt.Fprint(w, "({ var _groups: [String: [String: Any]] = [:]\n")
	fmt.Fprint(w, "var _res: [[String: Any]] = []\n")
	fmt.Fprintf(w, "for %s in ", esc(g.Var))
	g.Source.emit(w)
	fmt.Fprint(w, " {\n")
	for _, f := range g.Froms {
		fmt.Fprintf(w, "for %s in ", esc(f.Var))
		f.Src.emit(w)
		fmt.Fprint(w, " {\n")
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, "for %s in ", esc(j.Var))
		j.Src.emit(w)
		fmt.Fprint(w, " {\nif ")
		if j.On != nil {
			j.On.emit(w)
		} else {
			fmt.Fprint(w, "true")
		}
		fmt.Fprint(w, " {\n")
	}
	if g.Where != nil {
		fmt.Fprint(w, "if ")
		g.Where.emit(w)
		fmt.Fprint(w, " {\n")
	}
	fmt.Fprint(w, "let _key = ")
	g.Key.emit(w)
	fmt.Fprint(w, "\nlet _ks = String(describing: _key)\nvar _g = _groups[_ks] ?? [\"key\": _key, \"items\": []]\n")
	fmt.Fprint(w, "var _item: [String: Any] = [\"__join__\": true]\n")
	fmt.Fprintf(w, "_item[\"%s\"] = %s\n", g.Var, g.Var)
	for _, f := range g.Froms {
		fmt.Fprintf(w, "_item[\"%s\"] = %s\n", f.Var, f.Var)
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, "_item[\"%s\"] = %s\n", j.Var, j.Var)
	}
	fmt.Fprint(w, "_g[\"items\"] = (_g[\"items\"] as! [[String: Any]]) + [_item]\n")
	fmt.Fprint(w, "_groups[_ks] = _g\n")
	if g.Where != nil {
		fmt.Fprint(w, "}\n")
	}
	for range g.Joins {
		fmt.Fprint(w, "}\n")
		fmt.Fprint(w, "}\n")
	}
	for range g.Froms {
		fmt.Fprint(w, "}\n")
	}
	fmt.Fprint(w, "}\n")
	fmt.Fprint(w, "var _list = Array(_groups.values)\n")
	if g.Sort != nil {
		fmt.Fprint(w, "_list.sort { left, right in\n")
		fmt.Fprintf(w, "var %s = left\n", esc(g.Name))
		fmt.Fprint(w, "let _ka = ")
		g.Sort.emit(w)
		fmt.Fprint(w, "\n")
		fmt.Fprintf(w, "%s = right\n", esc(g.Name))
		fmt.Fprint(w, "let _kb = ")
		g.Sort.emit(w)
		fmt.Fprint(w, "\nreturn String(describing: _ka) < String(describing: _kb)\n}\n")
	} else {
		fmt.Fprint(w, "_list.sort { a, b in String(describing: a[\"key\"]) < String(describing: b[\"key\"]) }\n")
	}
	fmt.Fprintf(w, "for %s in _list {\n", g.Name)
	if g.Having != nil {
		fmt.Fprint(w, "if ")
		g.Having.emit(w)
		fmt.Fprint(w, " {\n_res.append(")
		g.Select.emit(w)
		fmt.Fprint(w, ")\n}\n")
	} else {
		fmt.Fprint(w, "_res.append(")
		g.Select.emit(w)
		fmt.Fprint(w, ")\n")
	}
	fmt.Fprint(w, "}\nreturn _res })()")
}

type IndexExpr struct {
	Base      Expr
	Index     Expr
	AsString  bool
	Force     bool
	KeyString bool
	KeyAny    bool
}

// SliceExpr represents a[start:end] slicing for lists or strings.
type SliceExpr struct {
	Base     Expr
	Start    Expr // optional
	End      Expr // optional
	AsString bool
}

// CastExpr represents a type cast.
type CastExpr struct {
	Expr       Expr
	Type       string
	FromString bool
}

func (ie *IndexExpr) emit(w io.Writer) {
	if ie.AsString {
		fmt.Fprint(w, "String(Array(")
		ie.Base.emit(w)
		fmt.Fprint(w, ")[")
		ie.Index.emit(w)
		fmt.Fprint(w, "])")
		return
	}
	ie.Base.emit(w)
	fmt.Fprint(w, "[")
	if ie.KeyString {
		fmt.Fprint(w, "String(")
		ie.Index.emit(w)
		fmt.Fprint(w, ")")
	} else if ie.KeyAny {
		fmt.Fprint(w, "AnyHashable(")
		ie.Index.emit(w)
		fmt.Fprint(w, ")")
	} else {
		ie.Index.emit(w)
	}
	fmt.Fprint(w, "]")
	if ie.Force {
		fmt.Fprint(w, "!")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	if s.AsString {
		fmt.Fprint(w, "String(Array(")
		s.Base.emit(w)
		fmt.Fprint(w, ")[")
		if s.Start != nil {
			s.Start.emit(w)
		} else {
			fmt.Fprint(w, "0")
		}
		fmt.Fprint(w, "..<")
		if s.End != nil {
			s.End.emit(w)
		} else {
			fmt.Fprint(w, "Array(")
			s.Base.emit(w)
			fmt.Fprint(w, ").count")
		}
		fmt.Fprint(w, "])")
		return
	}
	fmt.Fprint(w, "Array(")
	s.Base.emit(w)
	fmt.Fprint(w, "[")
	if s.Start != nil {
		s.Start.emit(w)
	} else {
		fmt.Fprint(w, "0")
	}
	fmt.Fprint(w, "..<")
	if s.End != nil {
		s.End.emit(w)
	} else {
		s.Base.emit(w)
		fmt.Fprint(w, ".count")
	}
	fmt.Fprint(w, "])")
}

func (c *CastExpr) emit(w io.Writer) {
	// avoid noisy casts when the expression is already a literal
	if lit, ok := c.Expr.(*LitExpr); ok {
		typ := strings.TrimSuffix(c.Type, "!")
		if typ != "BigInt" && typ != "Any" && typ != "Any?" {
			lit.emit(w)
			return
		}
	}
	typ := strings.TrimSuffix(c.Type, "!")
	if typ == "Bool" {
		if _, ok := c.Expr.(*BinaryExpr); ok {
			c.Expr.emit(w)
			return
		}
		if _, ok := c.Expr.(*CallExpr); ok {
			c.Expr.emit(w)
			return
		}
	}
	if inner, ok := c.Expr.(*CastExpr); ok {
		t1 := strings.TrimSuffix(c.Type, "!")
		t2 := strings.TrimSuffix(inner.Type, "!")
		if t1 == t2 {
			inner.emit(w)
			return
		}
	}
	t := c.Type
	force := false
	if strings.HasSuffix(t, "!") {
		force = true
		t = strings.TrimSuffix(t, "!")
	}
	if t == "Any" || t == "Any?" {
		c.Expr.emit(w)
		return
	}
	switch t {
	case "Int", "Int64", "Double", "String", "Bool", "BigInt":
		if t == "Int" {
			if inner, ok := c.Expr.(*CastExpr); ok && (inner.Type == "Any" || strings.Contains(inner.Type, "Any")) {
				fmt.Fprint(w, "(")
				inner.Expr.emit(w)
				fmt.Fprint(w, " as! Int)")
				return
			}
			if force {
				// Use Int(...) constructor for most expressions to prevent
				// runtime crashes when the underlying value is a Double.
				// Using `as! Int` here can trigger an illegal instruction
				// if the value isn't already an Int. Int(...) performs a
				// safe numeric conversion instead.
				if _, ok := c.Expr.(*IndexExpr); ok {
					fmt.Fprint(w, "(")
					c.Expr.emit(w)
					fmt.Fprint(w, " as? Int ?? 0)")
				} else if c.FromString {
					fmt.Fprint(w, "Int(")
					c.Expr.emit(w)
					fmt.Fprint(w, ")!")
				} else {
					fmt.Fprint(w, "Int(")
					c.Expr.emit(w)
					fmt.Fprint(w, ")")
				}
				return
			}
			// When casting indexed values from dynamic containers to Int,
			// convert using optional cast with default.
			if _, ok := c.Expr.(*IndexExpr); ok {
				fmt.Fprint(w, "(")
				c.Expr.emit(w)
				fmt.Fprint(w, " as? Int ?? 0)")
				return
			}
			if _, ok := c.Expr.(*FieldExpr); ok {
				fmt.Fprint(w, "(")
				c.Expr.emit(w)
				fmt.Fprint(w, " as? Int ?? 0)")
				return
			}
		}
		if ce, ok := c.Expr.(*CallExpr); ok && t == "String" && ce.Func == "str" {
			c.Expr.emit(w)
			return
		}
		if t == "Double" {
			fmt.Fprint(w, "Double(")
			c.Expr.emit(w)
			fmt.Fprint(w, ")")
		} else if t == "BigInt" {
			fmt.Fprint(w, "BigInt(")
			c.Expr.emit(w)
			fmt.Fprint(w, ")")
		} else if force {
			fmt.Fprint(w, "(")
			c.Expr.emit(w)
			fmt.Fprintf(w, " as! %s)", t)
		} else if t == "String" {
			fmt.Fprint(w, "String(describing: ")
			c.Expr.emit(w)
			fmt.Fprint(w, ")")
		} else {
			fmt.Fprintf(w, "%s(", t)
			c.Expr.emit(w)
			fmt.Fprint(w, ")")
		}
	default:
		fmt.Fprint(w, "(")
		c.Expr.emit(w)
		fmt.Fprintf(w, " as! %s)", t)
	}
}

type BinaryExpr struct {
	Left     Expr
	Op       string
	Right    Expr
	InMap    bool
	FloatMod bool
	IntOp    bool
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "in" {
		if b.InMap {
			fmt.Fprint(w, "(")
			b.Right.emit(w)
			fmt.Fprint(w, "[")
			b.Left.emit(w)
			fmt.Fprint(w, "] != nil)")
		} else {
			fmt.Fprint(w, "(")
			b.Right.emit(w)
			fmt.Fprint(w, ".contains(")
			b.Left.emit(w)
			fmt.Fprint(w, "))")
		}
		return
	}

	switch b.Op {
	case "&&", "||", "<", "<=", ">", ">=", "==", "!=":
		fmt.Fprint(w, "(")
		b.Left.emit(w)
		fmt.Fprintf(w, " %s ", b.Op)
		b.Right.emit(w)
		fmt.Fprint(w, ")")
		return
	case "%":
		if b.FloatMod {
			fmt.Fprint(w, "(")
			b.Left.emit(w)
			fmt.Fprint(w, ".truncatingRemainder(dividingBy: ")
			b.Right.emit(w)
			fmt.Fprint(w, "))")
			return
		}
	}

	fmt.Fprint(w, "(")
	b.Left.emit(w)
	var op string
	switch b.Op {
	case "+":
		if b.IntOp {
			op = "&+"
		} else {
			op = "+"
		}
	case "-":
		if b.IntOp {
			op = "&-"
		} else {
			op = "-"
		}
	case "*":
		if b.IntOp {
			op = "&*"
		} else {
			op = "*"
		}
	case "union":
		op = "+"
	default:
		op = b.Op
	}
	fmt.Fprintf(w, " %s ", op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" {
		if ie, ok := u.Expr.(*IndexExpr); ok && !ie.Force {
			fmt.Fprint(w, "!(")
			ie.emit(w)
			fmt.Fprint(w, " ?? false)")
		} else if ce, ok := u.Expr.(*CastExpr); ok {
			if ie, ok2 := ce.Expr.(*IndexExpr); ok2 && !ie.Force {
				fmt.Fprint(w, "!(")
				ie.emit(w)
				fmt.Fprint(w, " ?? false)")
			} else {
				fmt.Fprint(w, "(!")
				u.Expr.emit(w)
				fmt.Fprint(w, ")")
			}
		} else {
			fmt.Fprint(w, "(!")
			u.Expr.emit(w)
			fmt.Fprint(w, ")")
		}
		return
	}
	fmt.Fprint(w, u.Op)
	u.Expr.emit(w)
}

func (c *CallExpr) emit(w io.Writer) {
	var addrable func(Expr) bool
	addrable = func(e Expr) bool {
		switch v := e.(type) {
		case *NameExpr:
			return true
		case *IndexExpr:
			return addrable(v.Base)
		case *FieldExpr:
			return addrable(v.Target)
		case *CastExpr:
			return addrable(v.Expr)
		default:
			return false
		}
	}

	var emitNoCast func(Expr)
	emitNoCast = func(e Expr) {
		if ce, ok := e.(*CastExpr); ok {
			emitNoCast(ce.Expr)
			return
		}
		e.emit(w)
	}

	switch c.Func {
	case "len":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			fmt.Fprint(w, ".count)")
			return
		}
	case "count":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			fmt.Fprint(w, ".count)")
			return
		}
	case "str":
		fmt.Fprint(w, "String(describing: ")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		fmt.Fprint(w, ")")
		return
	case "int":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "_int(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			usesInt = true
			return
		}
	case "float":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "Double(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			return
		}
	case "input":
		if len(c.Args) == 0 {
			fmt.Fprint(w, "(readLine() ?? \"\")")
			return
		}
	case "padStart":
		if len(c.Args) == 3 {
			fmt.Fprint(w, "_padStart(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ", ")
			c.Args[1].emit(w)
			fmt.Fprint(w, ", ")
			c.Args[2].emit(w)
			fmt.Fprint(w, ")")
			usesPad = true
			return
		}
	case "repeat":
		if len(c.Args) == 2 {
			fmt.Fprint(w, "_repeat(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ", ")
			c.Args[1].emit(w)
			fmt.Fprint(w, ")")
			usesRepeat = true
			return
		}
	case "num":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "_rat_num(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			usesRat = true
			return
		}
	case "denom":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "_rat_denom(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			usesRat = true
			return
		}
	case "split":
		if len(c.Args) == 2 {
			fmt.Fprint(w, "_split(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ", ")
			c.Args[1].emit(w)
			fmt.Fprint(w, ")")
			usesSplit = true
			return
		}
	case "append":
		if len(c.Args) == 2 {
			fmt.Fprint(w, "_append(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ", ")
			c.Args[1].emit(w)
			fmt.Fprint(w, ")")
			usesAppend = true
			return
		}
	case "avg":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "Double((")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ").reduce(0) { s, v in s + ((v as? Double) ?? Double(v as? Int ?? 0)) }")
			fmt.Fprint(w, " ) / Double((")
			c.Args[0].emit(w)
			fmt.Fprint(w, ").count))")
			return
		}
	case "sum":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ").reduce(0) { s, v in s + ((v as? Double) ?? Double(v as? Int ?? 0)) }")
			fmt.Fprint(w, ")")
			return
		}
	case "min":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ".min()!)")
			return
		}
	case "max":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ".max()!)")
			return
		}
	case "substring", "substr":
		if len(c.Args) == 3 {
			fmt.Fprint(w, "String(Array(String(describing: ")
			c.Args[0].emit(w)
			fmt.Fprint(w, "))[")
			c.Args[1].emit(w)
			fmt.Fprint(w, "..<")
			c.Args[2].emit(w)
			fmt.Fprint(w, "])")
			return
		}
	case "upper":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ".uppercased())")
			return
		}
	case "lower":
		if len(c.Args) == 1 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ".lowercased())")
			return
		}
	case "indexOf":
		if len(c.Args) == 2 {
			fmt.Fprint(w, "(")
			fmt.Fprint(w, "String(describing: ")
			c.Args[0].emit(w)
			fmt.Fprint(w, ").range(of: String(describing: ")
			c.Args[1].emit(w)
			fmt.Fprint(w, "))?.lowerBound.utf16Offset(in: String(describing: ")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")) ?? -1)")
			return
		}
	case "parseIntStr":
		if len(c.Args) == 1 || len(c.Args) == 2 {
			fmt.Fprint(w, "Int(String(describing: ")
			c.Args[0].emit(w)
			fmt.Fprint(w, ")")
			if len(c.Args) == 2 {
				fmt.Fprint(w, ", radix: ")
				c.Args[1].emit(w)
			}
			fmt.Fprint(w, ")!")
			return
		}
	case "contains":
		if len(c.Args) == 2 {
			fmt.Fprint(w, "(")
			c.Args[0].emit(w)
			fmt.Fprint(w, ".contains(")
			c.Args[1].emit(w)
			fmt.Fprint(w, "))")
			return
		}
	case "now":
		if len(c.Args) == 0 {
			fmt.Fprint(w, "_now()")
			return
		}
	}
	flags, has := funcMutParams[c.Func]
	inFlags := funcInOutParams[c.Func]
	needTmp := false
	if has {
		for i, f := range flags {
			if f && i < len(c.Args) && i < len(inFlags) && inFlags[i] && !addrable(c.Args[i]) {
				needTmp = true
				break
			}
		}
	}
	if needTmp {
		fmt.Fprint(w, "{ () -> Any in\n")
		for i, f := range flags {
			if f && i < len(c.Args) && i < len(inFlags) && inFlags[i] && !addrable(c.Args[i]) {
				fmt.Fprintf(w, "var _tmp%d = ", i)
				emitNoCast(c.Args[i])
				fmt.Fprint(w, "\n")
			}
		}
		fmt.Fprint(w, "return ")
	}

	fmt.Fprint(w, esc(c.Func))
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if has && i < len(flags) && flags[i] && i < len(inFlags) && inFlags[i] {
			fmt.Fprint(w, "&")
			if addrable(a) {
				emitNoCast(a)
			} else {
				fmt.Fprintf(w, "_tmp%d", i)
			}
			continue
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
	if needTmp {
		fmt.Fprint(w, "\n}()")
	}
}

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Expr != nil {
		fmt.Fprint(w, " ")
		r.Expr.emit(w)
	}
	fmt.Fprint(w, "\n")
}

func (f *FunDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "func %s(", esc(f.Name))
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			if p.InOut {
				fmt.Fprintf(w, "_ %s: inout %s", esc(p.Name), p.Type)
			} else {
				if p.Escaping {
					fmt.Fprintf(w, "_ %s: @escaping %s", esc(p.Name), p.Type)
				} else {
					fmt.Fprintf(w, "_ %s: %s", esc(p.Name), p.Type)
				}
			}
		} else {
			if p.InOut {
				fmt.Fprintf(w, "_ %s: inout Any", esc(p.Name))
			} else {
				fmt.Fprintf(w, "_ %s", esc(p.Name))
			}
		}
	}
	fmt.Fprint(w, ")")
	if f.Ret != "" {
		fmt.Fprintf(w, " -> %s", f.Ret)
	}
	fmt.Fprint(w, " {\n")
	for _, s := range f.Body {
		s.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while ")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	} else {
		fmt.Fprint(w, "true")
	}
	fmt.Fprint(w, " {\n")
	for _, st := range ws.Body {
		st.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for %s in ", esc(fr.Name))
	fr.Start.emit(w)
	fmt.Fprint(w, "..<")
	fr.End.emit(w)
	fmt.Fprint(w, " {\n")
	for _, st := range fr.Body {
		st.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (fe *ForEachStmt) emit(w io.Writer) {
	if fe.CastMap {
		fmt.Fprint(w, "for _item in ")
		fe.Expr.emit(w)
		fmt.Fprint(w, " as! [[String: Any]] {\n")
		fmt.Fprintf(w, "let %s = _item as! [String: Any]\n", esc(fe.Name))
	} else {
		fmt.Fprintf(w, "for %s in ", esc(fe.Name))
		fe.Expr.emit(w)
		if fe.Keys {
			fmt.Fprint(w, ".keys.sorted()")
		}
		fmt.Fprint(w, " {\n")
	}
	for _, st := range fe.Body {
		st.emit(w)
	}
	fmt.Fprint(w, "}\n")
}

func (b *BreakStmt) emit(w io.Writer) { fmt.Fprint(w, "break\n") }

func (c *ContinueStmt) emit(w io.Writer) { fmt.Fprint(w, "continue\n") }

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

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf(`// Generated by Mochi transpiler v%s on %s
import Foundation
import Dispatch
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

let stdout = FileHandle.standardOutput
extension FileHandle {
    func write(_ string: String) {
        if let data = string.data(using: .utf8) {
            self.write(data)
        }
    }
}

func _p(_ v: Any?) -> String {
    if let val = v {
        if let d = val as? Double {
            if d.rounded(.towardZero) == d {
                return String(Int64(d))
            }
        }
        return String(describing: val)
    }
    return "<nil>"
}

`, version(), t.Format("2006-01-02 15:04:05 MST"))
}

func formatCode(src []byte) []byte {
	var out bytes.Buffer
	indent := 0
	scanner := bufio.NewScanner(bytes.NewReader(src))
	for scanner.Scan() {
		line := scanner.Text()
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "}") && indent > 0 {
			indent--
		}
		for i := 0; i < indent; i++ {
			out.WriteString("    ")
		}
		out.WriteString(trimmed)
		out.WriteByte('\n')
		if strings.HasSuffix(trimmed, "{") {
			indent++
		}
	}
	return out.Bytes()
}

func exprString(e Expr) string {
	if n, ok := e.(*NameExpr); ok {
		return n.Name
	}
	var buf bytes.Buffer
	e.emit(&buf)
	return strings.TrimSpace(buf.String())
}

func isAnyType(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func structForField(env *types.Env, field string) (types.StructType, bool) {
	if env == nil {
		return types.StructType{}, false
	}
	var found types.StructType
	count := 0
	for _, st := range env.Structs() {
		if _, ok := st.Fields[field]; ok {
			found = st
			count++
			if count > 1 {
				return types.StructType{}, false
			}
		}
	}
	if count == 1 {
		return found, true
	}
	return types.StructType{}, false
}

func structForMethod(env *types.Env, method string) (types.StructType, bool) {
	if env == nil {
		return types.StructType{}, false
	}
	var found types.StructType
	count := 0
	for _, st := range env.Structs() {
		if _, ok := st.Methods[method]; ok {
			found = st
			count++
			if count > 1 {
				return types.StructType{}, false
			}
		}
	}
	if count == 1 {
		return found, true
	}
	return types.StructType{}, false
}

// Emit returns the Swift source for the program.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	buf.WriteString("extension Double { init(_ v: Any) { if let d = v as? Double { self = d } else if let i = v as? Int { self = Double(i) } else if let i = v as? Int64 { self = Double(i) } else if let s = v as? String { self = Double(s) ?? 0 } else { self = 0 } } }\n")
	if p.UseNow {
		buf.WriteString("var _nowSeed = 0\n")
		buf.WriteString("var _nowSeeded = false\n")
		buf.WriteString("func _now() -> Int {\n")
		buf.WriteString("    if !_nowSeeded {\n")
		buf.WriteString("        if let s = ProcessInfo.processInfo.environment[\"MOCHI_NOW_SEED\"], let v = Int(s) {\n")
		buf.WriteString("            _nowSeed = v\n")
		buf.WriteString("            _nowSeeded = true\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if _nowSeeded {\n")
		buf.WriteString("        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647\n")
		buf.WriteString("        return _nowSeed\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return Int(DispatchTime.now().uptimeNanoseconds)\n")
		buf.WriteString("}\n")
	}
	if p.UseLookupHost {
		buf.WriteString("func _lookup_host(_ host: String) -> [Any] {\n")
		buf.WriteString("    return [[], NSNull()]\n")
		buf.WriteString("}\n")
	}
	if p.UseNum {
		buf.WriteString("func _num(_ v: Any) -> Double {\n")
		buf.WriteString("    if let d = v as? Double { return d }\n")
		buf.WriteString("    if let i = v as? Int { return Double(i) }\n")
		buf.WriteString("    if let i = v as? Int64 { return Double(i) }\n")
		buf.WriteString("    return 0\n")
		buf.WriteString("}\n")
	}
	if p.UseInt {
		buf.WriteString("func _int(_ v: Any) -> Int {\n")
		buf.WriteString("    if let s = v as? String { return Int(s) ?? 0 }\n")
		buf.WriteString("    if let i = v as? Int { return i }\n")
		buf.WriteString("    if let i = v as? Int64 { return Int(i) }\n")
		buf.WriteString("    if let d = v as? Double { return Int(d) }\n")
		if p.UseBigInt {
			buf.WriteString("    if let b = v as? BigInt { return b.toInt() }\n")
		}
		buf.WriteString("    return 0\n")
		buf.WriteString("}\n")
	}
	if p.UseKeys {
		buf.WriteString("func _keys<K,V>(_ m: [K: V]) -> [K] {\n")
		buf.WriteString("    Array(m.keys)\n")
		buf.WriteString("}\n")
	}
	if p.UsePad {
		buf.WriteString("func _padStart(_ s: String, _ w: Int, _ p: String) -> String {\n")
		buf.WriteString("    var out = s\n")
		buf.WriteString("    while out.count < w { out = p + out }\n")
		buf.WriteString("    return out\n")
		buf.WriteString("}\n")
	}
	if p.UseRepeat {
		buf.WriteString("func _repeat(_ s: String, _ n: Int) -> String {\n")
		buf.WriteString("    if n <= 0 { return \"\" }\n")
		buf.WriteString("    return String(repeating: s, count: n)\n")
		buf.WriteString("}\n")
	}
	if p.UseMem {
		buf.WriteString("func _mem() -> Int {\n")
		buf.WriteString("    if let status = try? String(contentsOfFile: \"/proc/self/status\") {\n")
		buf.WriteString("        for line in status.split(separator: \"\\n\") {\n")
		buf.WriteString("            if line.hasPrefix(\"VmRSS:\") {\n")
		buf.WriteString("                let parts = line.split(whereSeparator: { $0 == \" \" || $0 == \"\\t\" })\n")
		buf.WriteString("                if parts.count >= 2, let kb = Int(parts[1]) {\n")
		buf.WriteString("                    return kb * 1024\n")
		buf.WriteString("                }\n")
		buf.WriteString("            }\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return 0\n}\n")
	}
	if p.UseRat {
		buf.WriteString("func _rat_num(_ v: Double) -> Int {\n")
		buf.WriteString("    if v.isNaN { return 0 }\n")
		buf.WriteString("    if v > Double(Int.max) { return Int.max }\n")
		buf.WriteString("    if v < Double(Int.min) { return Int.min }\n")
		buf.WriteString("    return Int(v)\n}\n")
		buf.WriteString("func _rat_denom(_ v: Double) -> Int { 1 }\n")
	}
	if p.UseSHA256 {
		buf.WriteString("func _sha256(_ bs: [Int]) -> [Int] { return Array(repeating: 0, count: 32) }\n")
	}
	if p.UseAppend {
		buf.WriteString("func _append<T>(_ xs: [T], _ v: T) -> [T] {\n")
		buf.WriteString("    var out = xs\n")
		buf.WriteString("    out.append(v)\n")
		buf.WriteString("    return out\n")
		buf.WriteString("}\n")
	}
	if p.UseLen {
		buf.WriteString("func _len(_ v: Any) -> Int {\n")
		buf.WriteString("    if let s = v as? String { return Array(s).count }\n")
		buf.WriteString("    if let a = v as? [Any] { return a.count }\n")
		buf.WriteString("    if let a = v as? [Double] { return a.count }\n")
		buf.WriteString("    if let a = v as? [Int] { return a.count }\n")
		buf.WriteString("    if let m = v as? [AnyHashable: Any] { return m.count }\n")
		buf.WriteString("    return 0\n}\n")
	}
	if p.UseSplit {
		buf.WriteString("func _split(_ s: String, _ sep: String) -> [String] {\n")
		buf.WriteString("    let d = sep.isEmpty ? \" \" : sep\n")
		buf.WriteString("    return s.components(separatedBy: d)\n")
		buf.WriteString("}\n")
	}
	if p.UseFetch {
		buf.WriteString("func _fetch(_ url: String) -> Any {\n")
		buf.WriteString("    let u = URL(string: url)!\n")
		buf.WriteString("    let data = try! Data(contentsOf: u)\n")
		buf.WriteString("    let obj = try! JSONSerialization.jsonObject(with: data, options: [])\n")
		buf.WriteString("    return obj\n")
		buf.WriteString("}\n")
		for name := range p.FetchStructs {
			buf.WriteString(fmt.Sprintf("func _fetch_%s(_ url: String) -> %s {\n", name, name))
			buf.WriteString("    let u = URL(string: url)!\n")
			buf.WriteString("    let data = try! Data(contentsOf: u)\n")
			buf.WriteString(fmt.Sprintf("    return try! JSONDecoder().decode(%s.self, from: data)\n", name))
			buf.WriteString("}\n")
		}
	}
	if p.UseBigInt {
		buf.WriteString("struct BigInt: Comparable, CustomStringConvertible {\n")
		buf.WriteString("    private var digits: [UInt32] = []\n")
		buf.WriteString("    init() {}\n")
		buf.WriteString("    init(_ v: Int) {\n")
		buf.WriteString("        var n = v\n")
		buf.WriteString("        while n > 0 { digits.append(UInt32(n % 1000000000)); n /= 1000000000 }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    init(_ other: BigInt) { digits = other.digits }\n")
		buf.WriteString("    func toInt() -> Int {\n")
		buf.WriteString("        var res: Int64 = 0\n")
		buf.WriteString("        for d in digits.reversed() {\n")
		buf.WriteString("            if res > (Int64.max - Int64(d)) / 1000000000 {\n")
		buf.WriteString("                return Int.max\n")
		buf.WriteString("            }\n")
		buf.WriteString("            res = res * 1000000000 + Int64(d)\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return Int(res)\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static func +(lhs: BigInt, rhs: BigInt) -> BigInt {\n")
		buf.WriteString("        var carry: UInt64 = 0\n")
		buf.WriteString("        var result: [UInt32] = []\n")
		buf.WriteString("        let maxLen = max(lhs.digits.count, rhs.digits.count)\n")
		buf.WriteString("        for i in 0..<maxLen {\n")
		buf.WriteString("            let a = i < lhs.digits.count ? UInt64(lhs.digits[i]) : 0\n")
		buf.WriteString("            let b = i < rhs.digits.count ? UInt64(rhs.digits[i]) : 0\n")
		buf.WriteString("            let sum = a + b + carry\n")
		buf.WriteString("            result.append(UInt32(sum % 1000000000))\n")
		buf.WriteString("            carry = sum / 1000000000\n")
		buf.WriteString("        }\n")
		buf.WriteString("        if carry > 0 { result.append(UInt32(carry)) }\n")
		buf.WriteString("        var r = BigInt(); r.digits = result; return r\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static func -(lhs: BigInt, rhs: BigInt) -> BigInt {\n")
		buf.WriteString("        var borrow: Int64 = 0\n")
		buf.WriteString("        var result: [UInt32] = []\n")
		buf.WriteString("        for i in 0..<lhs.digits.count {\n")
		buf.WriteString("            var a = Int64(lhs.digits[i]) - borrow\n")
		buf.WriteString("            let b = i < rhs.digits.count ? Int64(rhs.digits[i]) : 0\n")
		buf.WriteString("            var diff = a - b\n")
		buf.WriteString("            if diff < 0 { diff += 1000000000; borrow = 1 } else { borrow = 0 }\n")
		buf.WriteString("            result.append(UInt32(diff))\n")
		buf.WriteString("        }\n")
		buf.WriteString("        while result.last == 0 { result.removeLast(); if result.isEmpty { break } }\n")
		buf.WriteString("        var r = BigInt(); r.digits = result; return r\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static func *(lhs: BigInt, rhs: BigInt) -> BigInt {\n")
		buf.WriteString("        if lhs.digits.isEmpty || rhs.digits.isEmpty { return BigInt() }\n")
		buf.WriteString("        var result = Array(repeating: UInt64(0), count: lhs.digits.count + rhs.digits.count)\n")
		buf.WriteString("        for i in 0..<lhs.digits.count {\n")
		buf.WriteString("            var carry: UInt64 = 0\n")
		buf.WriteString("            for j in 0..<rhs.digits.count {\n")
		buf.WriteString("                let idx = i + j\n")
		buf.WriteString("                let prod = UInt64(lhs.digits[i]) * UInt64(rhs.digits[j]) + result[idx] + carry\n")
		buf.WriteString("                result[idx] = prod % 1000000000\n")
		buf.WriteString("                carry = prod / 1000000000\n")
		buf.WriteString("            }\n")
		buf.WriteString("            if carry > 0 { result[i + rhs.digits.count] += carry }\n")
		buf.WriteString("        }\n")
		buf.WriteString("        var resDigits: [UInt32] = result.map { UInt32($0) }\n")
		buf.WriteString("        while resDigits.last == 0 { resDigits.removeLast(); if resDigits.isEmpty { break } }\n")
		buf.WriteString("        var r = BigInt(); r.digits = resDigits; return r\n")
		buf.WriteString("    }\n")
		buf.WriteString("    func bitLength() -> Int {\n")
		buf.WriteString("        if digits.isEmpty { return 0 }\n")
		buf.WriteString("        var v = digits.last!\n")
		buf.WriteString("        var bits = 0\n")
		buf.WriteString("        while v > 0 { v >>= 1; bits += 1 }\n")
		buf.WriteString("        return bits + (digits.count - 1) * 30\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static func <(lhs: BigInt, rhs: BigInt) -> Bool {\n")
		buf.WriteString("        if lhs.digits.count != rhs.digits.count { return lhs.digits.count < rhs.digits.count }\n")
		buf.WriteString("        for i in stride(from: lhs.digits.count-1, through: 0, by: -1) {\n")
		buf.WriteString("            if lhs.digits[i] != rhs.digits[i] { return lhs.digits[i] < rhs.digits[i] }\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return false\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static func >(lhs: BigInt, rhs: BigInt) -> Bool { return rhs < lhs }\n")
		buf.WriteString("    static func <=(lhs: BigInt, rhs: BigInt) -> Bool { return !(rhs < lhs) }\n")
		buf.WriteString("    static func >=(lhs: BigInt, rhs: BigInt) -> Bool { return !(lhs < rhs) }\n")
		buf.WriteString("    static func /(lhs: BigInt, rhs: BigInt) -> BigInt {\n")
		buf.WriteString("        let divisor = rhs.toInt()\n")
		buf.WriteString("        if divisor == 0 { return BigInt() }\n")
		buf.WriteString("        var result: [UInt32] = Array(repeating: 0, count: lhs.digits.count)\n")
		buf.WriteString("        var rem: Int64 = 0\n")
		buf.WriteString("        for i in stride(from: lhs.digits.count - 1, through: 0, by: -1) {\n")
		buf.WriteString("            let cur = rem * 1000000000 + Int64(lhs.digits[i])\n")
		buf.WriteString("            result[i] = UInt32(cur / Int64(divisor))\n")
		buf.WriteString("            rem = cur % Int64(divisor)\n")
		buf.WriteString("        }\n")
		buf.WriteString("        var r = BigInt(); r.digits = result\n")
		buf.WriteString("        while r.digits.last == 0 { r.digits.removeLast(); if r.digits.isEmpty { break } }\n")
		buf.WriteString("        return r\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static func %(lhs: BigInt, rhs: BigInt) -> BigInt {\n")
		buf.WriteString("        let divisor = rhs.toInt()\n")
		buf.WriteString("        if divisor == 0 { return BigInt() }\n")
		buf.WriteString("        var rem: Int64 = 0\n")
		buf.WriteString("        for d in lhs.digits.reversed() {\n")
		buf.WriteString("            rem = (rem * 1000000000 + Int64(d)) % Int64(divisor)\n")
		buf.WriteString("        }\n")
		buf.WriteString("        return BigInt(Int(rem))\n")
		buf.WriteString("    }\n")
		buf.WriteString("    static func +(lhs: BigInt, rhs: Int) -> BigInt { return lhs + BigInt(rhs) }\n")
		buf.WriteString("    static func +(lhs: Int, rhs: BigInt) -> BigInt { return BigInt(lhs) + rhs }\n")
		buf.WriteString("    static func ==(lhs: BigInt, rhs: BigInt) -> Bool { lhs.digits == rhs.digits }\n")
		buf.WriteString("    var description: String {\n")
		buf.WriteString("        if digits.isEmpty { return \"0\" }\n")
		buf.WriteString("        var s = String(digits.last!)\n")
		buf.WriteString("        for d in digits.dropLast().reversed() { s += String(format: \"%09d\", d) }\n")
		buf.WriteString("        return s\n")
		buf.WriteString("    }\n")
		buf.WriteString("}\n")
		buf.WriteString("extension Int { init(_ b: BigInt) { self = b.toInt() } }\n")
		buf.WriteString("extension Double { init(_ b: BigInt) { self = Double(b.toInt()) } }\n")
	}
	needJSON := false
	for _, st := range p.Stmts {
		if _, ok := st.(*SaveStmt); ok {
			needJSON = true
			break
		}
	}
	if needJSON {
		buf.WriteString("func toJsonObj(_ v: Any) -> Any {\n")
		buf.WriteString("    if let m = v as? [String: Any] { return m }\n")
		buf.WriteString("    if let a = v as? [Any] { return a }\n")
		buf.WriteString("    var d: [String: Any] = [:]\n")
		buf.WriteString("    for c in Mirror(reflecting: v).children {\n")
		buf.WriteString("        if let k = c.label { d[k] = c.value }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    return d\n}\n")
	}
	for _, s := range p.Stmts {
		s.emit(&buf)
	}
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		buf.WriteByte('\n')
	}
	return formatCode(buf.Bytes())
}

// Transpile converts a Mochi program into a simple Swift AST.
func Transpile(env *types.Env, prog *parser.Program, benchMain bool) (*Program, error) {
	usesNow = false
	usesLookupHost = false
	usesNum = false
	usesInt = false
	usesKeys = false
	usesMem = false
	usesBigInt = false
	usesPad = false
	usesRepeat = false
	usesRat = false
	usesSHA256 = false
	usesAppend = false
	usesLen = false
	usesSplit = false
	usesFetch = false
	fetchStructs = map[string]bool{}
	funcMutParams = map[string][]bool{}
	funcInOutParams = map[string][]bool{}
	classStructs = map[string]bool{}
	nonCodableStructs = map[string]bool{}
	p := &Program{}
	stmts, err := convertStmts(env, prog.Statements)
	if err != nil {
		return nil, err
	}
	if benchMain {
		usesNow = true
		usesMem = true
		var prefix, body []Stmt
		for _, st := range stmts {
			if isTypeStmt(st) {
				prefix = append(prefix, st)
			} else {
				body = append(body, st)
			}
		}
		stmts = append(prefix, &BenchStmt{Name: "main", Body: body})
	}
	p.Stmts = stmts
	p.UseNow = usesNow
	p.UseLookupHost = usesLookupHost
	p.UseNum = usesNum
	p.UseInt = usesInt
	p.UseKeys = usesKeys
	p.UseMem = usesMem
	p.UseBigInt = usesBigInt
	p.UsePad = usesPad
	p.UseRepeat = usesRepeat
	p.UseRat = usesRat
	p.UseSHA256 = usesSHA256
	p.UseAppend = usesAppend
	p.UseLen = usesLen
	p.UseSplit = usesSplit
	p.UseFetch = usesFetch
	p.FetchStructs = fetchStructs
	return p, nil
}

func findUpdatedVars(env *types.Env, list []*parser.Statement, vars map[string]bool, anyVars map[string]bool) {
	var scanExpr func(*parser.Expr)

	scanCall := func(fn string, args []*parser.Expr) {
		if flags, ok := funcMutParams[fn]; ok {
			for i, a := range args {
				if i < len(flags) && flags[i] {
					if name := rootNameExpr(a); name != "" {
						vars[name] = true
					}
				}
				scanExpr(a)
			}
			return
		}
		for _, a := range args {
			scanExpr(a)
		}
	}

	var scanPrimary func(*parser.Primary)
	scanPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		if p.Call != nil {
			scanCall(p.Call.Func, p.Call.Args)
			return
		}
		if p.Group != nil {
			scanExpr(p.Group)
		}
	}

	var scanPostfix func(*parser.PostfixExpr)
	scanPostfix = func(pe *parser.PostfixExpr) {
		if pe == nil {
			return
		}
		scanPrimary(pe.Target)
		for _, op := range pe.Ops {
			if op.Call != nil {
				fn := rootNamePostfix(pe)
				scanCall(fn, op.Call.Args)
			}
			if op.Index != nil {
				if op.Index.Start != nil {
					scanExpr(op.Index.Start)
				}
				if op.Index.End != nil {
					scanExpr(op.Index.End)
				}
				if op.Index.Step != nil {
					scanExpr(op.Index.Step)
				}
			}
		}
	}

	scanUnary := func(u *parser.Unary) {
		if u == nil {
			return
		}
		scanPostfix(u.Value)
	}

	scanExpr = func(e *parser.Expr) {
		if e == nil {
			return
		}
		if e.Binary != nil {
			scanUnary(e.Binary.Left)
			for _, op := range e.Binary.Right {
				scanPostfix(op.Right)
			}
		}
	}

	for _, st := range list {
		switch {
		case st.Update != nil:
			vars[st.Update.Target] = true
		case st.Assign != nil:
			vars[st.Assign.Name] = true
			if anyVars != nil {
				if t := types.TypeOfExpr(st.Assign.Value, env); t == (types.AnyType{}) {
					if rootNameExpr(st.Assign.Value) != st.Assign.Name {
						anyVars[st.Assign.Name] = true
					}
				}
			}
		case st.Expr != nil:
			scanExpr(st.Expr.Expr)
		case st.Return != nil:
			scanExpr(st.Return.Value)
		case st.Let != nil:
			scanExpr(st.Let.Value)
			if env != nil {
				var t types.Type = types.AnyType{}
				if st.Let.Type != nil {
					t = types.ResolveTypeRef(st.Let.Type, env)
				} else if st.Let.Value != nil {
					t = types.TypeOfExpr(st.Let.Value, env)
				}
				env.SetVar(st.Let.Name, t, false)
			}
		case st.Var != nil:
			scanExpr(st.Var.Value)
			if env != nil {
				var t types.Type = types.AnyType{}
				if st.Var.Type != nil {
					t = types.ResolveTypeRef(st.Var.Type, env)
				} else if st.Var.Value != nil {
					t = types.TypeOfExpr(st.Var.Value, env)
				}
				env.SetVar(st.Var.Name, t, true)
			}
		case st.If != nil:
			findUpdatedVars(env, st.If.Then, vars, anyVars)
			if st.If.Else != nil {
				findUpdatedVars(env, st.If.Else, vars, anyVars)
			}
			if st.If.ElseIf != nil {
				child := &parser.Statement{If: st.If.ElseIf}
				findUpdatedVars(env, []*parser.Statement{child}, vars, anyVars)
			}
		case st.For != nil:
			findUpdatedVars(env, st.For.Body, vars, anyVars)
		case st.While != nil:
			findUpdatedVars(env, st.While.Body, vars, anyVars)
		case st.Test != nil:
			findUpdatedVars(env, st.Test.Body, vars, anyVars)
		case st.Fun != nil:
			findUpdatedVars(env, st.Fun.Body, vars, anyVars)
		}
	}
}

func rootNameExpr(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	if e.Binary != nil {
		return rootNameUnary(e.Binary.Left)
	}
	return ""
}

func rootNameUnary(u *parser.Unary) string {
	if u == nil {
		return ""
	}
	return rootNamePostfix(u.Value)
}

func rootNamePostfix(pe *parser.PostfixExpr) string {
	if pe == nil {
		return ""
	}
	if pe.Target != nil && pe.Target.Selector != nil {
		return pe.Target.Selector.Root
	}
	if pe.Target != nil && pe.Target.Group != nil {
		return rootNameExpr(pe.Target.Group)
	}
	return ""
}

func convertStmts(env *types.Env, list []*parser.Statement) ([]Stmt, error) {
	// pre-scan function declarations to collect mutating parameter info
	for _, st := range list {
		if st.Fun != nil {
			if _, err := convertStmt(env, st); err != nil {
				return nil, err
			}
		}
	}
	updated := map[string]bool{}
	anyUpdated := map[string]bool{}
	findUpdatedVars(env, list, updated, anyUpdated)
	prev := currentUpdated
	prevAny := currentAnyUpdated
	currentUpdated = updated
	currentAnyUpdated = anyUpdated
	defer func() { currentUpdated = prev; currentAnyUpdated = prevAny }()
	var out []Stmt
	for _, st := range list {
		cs, err := convertStmt(env, st)
		if err != nil {
			return nil, err
		}
		// adjust var mutability for let statements
		if vd, ok := cs.(*VarDecl); ok && updated[vd.Name] {
			vd.Const = false
		}
		out = append(out, cs)
	}
	return out, nil
}

func convertStmt(env *types.Env, st *parser.Statement) (Stmt, error) {
	switch {
	case st.Import != nil:
		return convertImport(st.Import), nil
	case st.ExternVar != nil:
		return convertExternVar(st.ExternVar), nil
	case st.ExternFun != nil:
		return convertExternFun(st.ExternFun), nil
	case st.Expr != nil:
		if isNullLiteral(st.Expr.Expr) {
			return EmptyStmt{}, nil
		}
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			return convertSaveStmt(env, se)
		}
		if call != nil && call.Func == "print" {
			if len(call.Args) == 1 {
				arg := call.Args[0]
				if val, str, ok := evalPrintArg(arg); ok {
					return &PrintStmt{Exprs: []Expr{&LitExpr{Value: val, IsString: str}}}, nil
				}
			}
			var args []Expr
			for _, a := range call.Args {
				ex, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				if env != nil {
					if types.IsBoolType(types.TypeOfExpr(a, env)) && boolAsInt(a) {
						ex = &CondExpr{Cond: ex, Then: &LitExpr{Value: "1", IsString: false}, Else: &LitExpr{Value: "0", IsString: false}}
					} else if types.IsMapType(types.TypeOfExpr(a, env)) || types.IsStructType(types.TypeOfExpr(a, env)) {
						ex = &MapStringExpr{Value: ex}
					} else if types.IsListType(types.TypeOfExpr(a, env)) {
						ex = &ArrayStringExpr{Value: ex}
					}
				}
				args = append(args, ex)
			}
			return &PrintStmt{Exprs: args}, nil
		}
		ex, err := convertExpr(env, st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		if isNilExpr(ex) {
			return EmptyStmt{}, nil
		}
		return &ExprStmt{Expr: ex}, nil
	case st.Let != nil:
		var ex Expr
		var err error
		var typ string
		if st.Let.Value != nil {
			if f := fetchExprOnly(st.Let.Value); f != nil {
				urlExpr, err2 := convertExpr(env, f.URL)
				if err2 != nil {
					return nil, err2
				}
				usesFetch = true
				if st.Let.Type != nil {
					typ = toSwiftType(st.Let.Type)
					if fetchStructs == nil {
						fetchStructs = map[string]bool{}
					}
					fetchStructs[typ] = true
					ex = &CallExpr{Func: "_fetch_" + typ, Args: []Expr{urlExpr}}
				} else {
					ex = &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}
				}
			} else if st.Let.Type != nil && (types.IsEmptyListLiteral(st.Let.Value) || isEmptyMapLiteral(st.Let.Value) || isNullLiteral(st.Let.Value)) {
				ex = zeroValue(st.Let.Type)
			} else {
				ex, err = convertExpr(env, st.Let.Value)
				if err != nil {
					return nil, err
				}
			}
			if st.Let.Type != nil {
				typ = toSwiftType(st.Let.Type)
			} else if env != nil {
				t := types.TypeOfExpr(st.Let.Value, env)
				typ = swiftTypeOf(t)
				if !types.IsEmptyListLiteral(st.Let.Value) && !isEmptyMapLiteral(st.Let.Value) && !types.IsMapType(t) && !types.IsListType(t) {
					typ = ""
				}
			}
		} else if st.Let.Type != nil {
			ex = zeroValue(st.Let.Type)
			typ = toSwiftType(st.Let.Type)
		}
		if env != nil {
			var varT types.Type = types.AnyType{}
			if st.Let.Type != nil {
				varT = types.ResolveTypeRef(st.Let.Type, env)
			} else if st.Let.Value != nil {
				varT = types.TypeOfExpr(st.Let.Value, env)
			}
			env.SetVar(st.Let.Name, varT, true)
			swiftT := swiftTypeOf(varT)
			if st.Let.Value != nil && swiftT != "Any" {
				srcSwiftT := swiftTypeOf(types.TypeOfExpr(st.Let.Value, env))
				if srcSwiftT == swiftT {
					// no cast needed
				} else if ie, ok := ex.(*IndexExpr); ok && !ie.Force {
					// optional map lookup
				} else if (swiftT == "Int" || swiftT == "Int64" || swiftT == "Double") &&
					(srcSwiftT == "Int" || srcSwiftT == "Int64" || srcSwiftT == "Double") {
					ex = &CastExpr{Expr: ex, Type: swiftT}
				} else {
					ex = &CastExpr{Expr: ex, Type: swiftT + "!"}
				}
			}
		}
		return &VarDecl{Name: st.Let.Name, Const: true, Type: typ, Expr: ex}, nil
	case st.Var != nil:
		var ex Expr
		var err error
		var typ string
		if st.Var.Value != nil {
			if st.Var.Type != nil && isNullLiteral(st.Var.Value) {
				ex = &LitExpr{Value: "nil", IsString: false}
				typ = toSwiftOptionalType(st.Var.Type)
			} else {
				if st.Var.Type != nil && (types.IsEmptyListLiteral(st.Var.Value) || isEmptyMapLiteral(st.Var.Value)) {
					ex = zeroValue(st.Var.Type)
				} else {
					ex, err = convertExpr(env, st.Var.Value)
					if err != nil {
						return nil, err
					}
				}
				if st.Var.Type != nil {
					typ = toSwiftType(st.Var.Type)
				} else if env != nil {
					t := types.TypeOfExpr(st.Var.Value, env)
					typ = swiftTypeOf(t)
					if typ == "Any?" || typ == "Any" {
						if isMapLiteral(st.Var.Value) {
							typ = "[String: Any?]"
						}
					}
				}
			}
			if typ == "" {
				if _, ok := ex.(*MapLit); ok {
					typ = "[String: Any]"
				} else if al, ok := ex.(*ArrayLit); ok {
					typ = "[Any]"
					if len(al.Elems) == 0 {
						ex = &RawStmt{Code: "[]"}
					}
				}
			}
		} else if st.Var.Type != nil {
			ex = zeroValue(st.Var.Type)
			typ = toSwiftType(st.Var.Type)
		}
		if env != nil {
			var varT types.Type
			if vt, err := env.GetVar(st.Var.Name); err == nil {
				varT = vt
			} else if st.Var.Type != nil {
				varT = types.ResolveTypeRef(st.Var.Type, env)
			} else if st.Var.Value != nil {
				varT = types.TypeOfExpr(st.Var.Value, env)
				if typ == "[String: Any?]" {
					varT = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
				}
			} else {
				varT = types.AnyType{}
			}
			if st.Var.Type == nil && currentAnyUpdated != nil && currentAnyUpdated[st.Var.Name] {
				if _, ok := varT.(types.ListType); !ok {
					varT = types.AnyType{}
				}
			}
			typ = swiftTypeOf(varT)
			ut, okU := env.FindUnionByVariant(swiftTypeOf(varT))
			if okU {
				env.SetVar(st.Var.Name, ut, true)
			} else {
				env.SetVar(st.Var.Name, varT, true)
			}
			swiftT := swiftTypeOf(varT)
			if okU {
				swiftT = ut.Name
			}
			if swiftT != "Any" && st.Var.Value != nil {
				srcSwiftT := swiftTypeOf(types.TypeOfExpr(st.Var.Value, env))
				if srcSwiftT == swiftT {
					// no cast needed
				} else if ie, ok := ex.(*IndexExpr); ok && !ie.Force {
					// optional map lookup
				} else if (swiftT == "Int" || swiftT == "Int64" || swiftT == "Double") &&
					(srcSwiftT == "Int" || srcSwiftT == "Int64" || srcSwiftT == "Double") {
					ex = &CastExpr{Expr: ex, Type: swiftT}
				} else {
					ex = &CastExpr{Expr: ex, Type: swiftT + "!"}
				}
			}
		}
		return &VarDecl{Name: st.Var.Name, Const: false, Type: typ, Expr: ex}, nil
	case st.Type != nil:
		def, err := convertTypeDecl(env, st.Type)
		if err != nil {
			return nil, err
		}
		if def != nil {
			return def, nil
		}
		return &BlockStmt{}, nil
	case st.Test != nil:
		body, err := convertStmts(env, st.Test.Body)
		if err != nil {
			return nil, err
		}
		return &BlockStmt{Stmts: body}, nil
	case st.Bench != nil:
		usesNow = true
		usesMem = true
		body, err := convertStmts(env, st.Bench.Body)
		if err != nil {
			return nil, err
		}
		return &BenchStmt{Name: st.Bench.Name, Body: body}, nil
	case st.Expect != nil:
		cond, err := convertExpr(env, st.Expect.Value)
		if err != nil {
			return nil, err
		}
		return &ExpectStmt{Cond: cond}, nil
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		ex, err := convertExpr(env, st.Assign.Value)
		if err != nil {
			return nil, err
		}
		if env != nil {
			if t, err := env.GetVar(st.Assign.Name); err == nil {
				typ := swiftTypeOf(t)
				if typ != "Any" {
					if isNullLiteral(st.Assign.Value) {
						ex = zeroValue(parserTypeRefFromType(t))
					} else {
						ex = &CastExpr{Expr: ex, Type: typ + "!"}
					}
				}
			}
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: ex}, nil
	case st.Assign != nil && (len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0):
		lhs := Expr(&NameExpr{Name: st.Assign.Name})
		var cur types.Type
		if env != nil {
			if t, err := env.GetVar(st.Assign.Name); err == nil {
				cur = t
			}
		}
		for j, idx := range st.Assign.Index {
			if idx.Start == nil || idx.Colon != nil || idx.End != nil || idx.Colon2 != nil || idx.Step != nil {
				return nil, fmt.Errorf("unsupported index")
			}
			ix, err := convertExpr(env, idx.Start)
			if err != nil {
				return nil, err
			}
			keyStr := false
			keyAny := false
			isLast := j == len(st.Assign.Index)-1 && len(st.Assign.Field) == 0
			if mt, ok := cur.(types.MapType); ok {
				if types.IsStringType(mt.Key) {
					ix = &CastExpr{Expr: ix, Type: "String"}
					keyStr = true
				} else if types.IsAnyType(mt.Key) {
					keyAny = true
				}
				cur = mt.Value
				force := !isLast
				lhs = &IndexExpr{Base: lhs, Index: ix, Force: force, KeyString: keyStr, KeyAny: keyAny}
			} else if lt, ok := cur.(types.ListType); ok {
				if env != nil {
					if ixT := types.TypeOfExpr(idx.Start, env); types.IsAnyType(ixT) {
						ix = &CastExpr{Expr: ix, Type: "Int!"}
					}
				}
				cur = lt.Elem
				lhs = &IndexExpr{Base: lhs, Index: ix}
			} else {
				lhs = &IndexExpr{Base: lhs, Index: ix}
			}
		}
		for _, f := range st.Assign.Field {
			if stt, ok := cur.(types.StructType); ok {
				if ft, ok := stt.Fields[f.Name]; ok {
					cur = ft
				}
				lhs = &FieldExpr{Target: lhs, Name: f.Name}
			} else {
				lhs = &IndexExpr{Base: lhs, Index: &LitExpr{Value: f.Name, IsString: true}, Force: true}
				if mt, ok := cur.(types.MapType); ok {
					cur = mt.Value
				}
			}
		}
		val, err := convertExpr(env, st.Assign.Value)
		if err != nil {
			return nil, err
		}
		if env != nil {
			typ := swiftTypeOf(cur)
			if typ != "Any" {
				if ie, ok := val.(*IndexExpr); ok && !ie.Force {
					// optional lookup value, no cast
				} else {
					val = &CastExpr{Expr: val, Type: typ + "!"}
				}
			}
		}
		return &IndexAssignStmt{Target: lhs, Value: val}, nil
	case st.Fun != nil:
		return convertFunDecl(env, st.Fun)
	case st.Return != nil:
		return convertReturnStmt(env, st.Return)
	case st.While != nil:
		return convertWhileStmt(env, st.While)
	case st.For != nil:
		return convertForStmt(env, st.For)
	case st.Update != nil:
		return convertUpdateStmt(env, st.Update)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.If != nil:
		return convertIfStmt(env, st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertIfStmt(env *types.Env, i *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(env, i.Cond)
	if err != nil {
		return nil, err
	}
	if env != nil {
		t := types.TypeOfExpr(i.Cond, env)
		if ot, ok := t.(types.OptionType); ok {
			t = ot.Elem
		}
		if types.IsAnyType(t) {
			if _, ok := cond.(*IndexExpr); ok {
				cond = &CastExpr{Expr: cond, Type: "Bool!"}
			} else {
				cond = &CastExpr{Expr: cond, Type: "Bool!"}
			}
		}
	}
	thenStmts, err := convertStmts(env, i.Then)
	if err != nil {
		return nil, err
	}
	var elseIf *IfStmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(env, i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = s.(*IfStmt)
	}
	var elseStmts []Stmt
	if i.Else != nil {
		elseStmts, err = convertStmts(env, i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, ElseIf: elseIf, Else: elseStmts}, nil
}

func convertFunDecl(env *types.Env, f *parser.FunStmt) (Stmt, error) {
	fn := &FunDecl{Name: f.Name, Ret: toSwiftType(f.Return)}
	child := types.NewEnv(env)
	updated := map[string]bool{}
	findUpdatedVars(child, f.Body, updated, nil)
	// register function type for calls (and recursion)
	if env != nil {
		var params []types.Type
		for _, p := range f.Params {
			if p.Type != nil {
				params = append(params, types.ResolveTypeRef(p.Type, env))
			} else {
				params = append(params, types.AnyType{})
			}
		}
		var retTyp types.Type = types.VoidType{}
		if f.Return != nil {
			retTyp = types.ResolveTypeRef(f.Return, env)
		}
		env.SetFuncType(f.Name, types.FuncType{Params: params, Return: retTyp})
		env.SetFunc(f.Name, f)
	}

	var retT types.Type
	if f.Return != nil {
		retT = types.ResolveTypeRef(f.Return, env)
	} else {
		retT = types.VoidType{}
	}
	child.SetVar("$retType", retT, false)
	mutFlags := make([]bool, len(f.Params))
	inFlags := make([]bool, len(f.Params))
	for i, p := range f.Params {
		m := updated[p.Name]
		mutFlags[i] = m
		var pt types.Type = types.AnyType{}
		if p.Type != nil {
			pt = types.ResolveTypeRef(p.Type, env)
		}
		child.SetVar(p.Name, pt, true)
		useInOut := m && (types.IsListType(pt) || types.IsMapType(pt) || types.IsStructType(pt) || types.IsUnionType(pt))
		if st, ok := pt.(types.StructType); ok {
			if classStructs[st.Name] {
				useInOut = false
			}
		}
		inFlags[i] = useInOut
		esc := isFuncTypeRef(p.Type) || isFuncType(pt)
		fn.Params = append(fn.Params, Param{Name: p.Name, Type: toSwiftType(p.Type), InOut: useInOut, Escaping: esc})
	}
	funcMutParams[f.Name] = mutFlags
	funcInOutParams[f.Name] = inFlags
	body, err := convertStmts(child, f.Body)
	if err != nil {
		return nil, err
	}
	for i := len(f.Params) - 1; i >= 0; i-- {
		if mutFlags[i] {
			if !(fn.Params[i].InOut) {
				p := f.Params[i]
				body = append([]Stmt{&VarDecl{Name: p.Name, Expr: &NameExpr{Name: p.Name}}}, body...)
			}
		}
	}
	fn.Body = body
	return fn, nil
}

func convertReturnStmt(env *types.Env, r *parser.ReturnStmt) (Stmt, error) {
	var ex Expr
	var err error
	if r.Value != nil {
		ex, err = convertExpr(env, r.Value)
		if err != nil {
			return nil, err
		}
	}
	if env != nil {
		if rt, err := env.GetVar("$retType"); err == nil {
			if _, ok := rt.(types.VoidType); ok {
				ex = nil
			} else {
				typ := swiftTypeOf(rt)
				if typ != "Any" {
					srcTyp := swiftTypeOf(types.TypeOfExpr(r.Value, env))
					if srcTyp != typ {
						ex = &CastExpr{Expr: ex, Type: typ + "!"}
					}
				} else if lit, ok := ex.(*LitExpr); ok && lit.Value == "nil" {
					ex = &RawStmt{Code: "nil as Any?"}
				}
			}
		}
	}
	return &ReturnStmt{Expr: ex}, nil
}

func convertWhileStmt(env *types.Env, wst *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(env, wst.Cond)
	if err != nil {
		return nil, err
	}
	if env != nil {
		t := types.TypeOfExpr(wst.Cond, env)
		if ot, ok := t.(types.OptionType); ok {
			t = ot.Elem
		}
		if types.IsAnyType(t) {
			cond = &CastExpr{Expr: cond, Type: "Bool!"}
		}
	}
	body, err := convertStmts(env, wst.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertImport(im *parser.ImportStmt) Stmt {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	lang := ""
	if im.Lang != nil {
		lang = *im.Lang
	}
	if lang == "go" && im.Path == "mochi/runtime/ffi/go/testpkg" {
		return &RawStmt{Code: fmt.Sprintf(`struct %s {
    static func Add(_ a: Int, _ b: Int) -> Int { return a + b }
    static let Pi = 3.14
    static let Answer = 42
    static func FifteenPuzzleExample() -> String { return "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd" }
    static func ECDSAExample() -> (D: Any?, X: Any?, Y: Any?, Hash: Any?, R: Any?, S: Any?, Valid: Any?) { return (nil, nil, nil, nil, nil, nil, nil) }
    static func MD5Hex(_ s: String) -> String {
        var msg = [UInt8](s.utf8)
        let bitLen = UInt64(msg.count) * 8
        msg.append(0x80)
        while (msg.count %% 64) != 56 { msg.append(0) }
        for i in 0..<8 { msg.append(UInt8((bitLen >> (8 * UInt64(i))) & 0xff)) }
        var a0: UInt32 = 0x67452301
        var b0: UInt32 = 0xefcdab89
        var c0: UInt32 = 0x98badcfe
        var d0: UInt32 = 0x10325476
        let s: [UInt32] = [7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21]
        let K: [UInt32] = (0..<64).map { UInt32(abs(sin(Double($0+1))) * 4294967296.0) }
        func leftrotate(_ x: UInt32, _ c: UInt32) -> UInt32 { return (x << c) | (x >> (32 - c)) }
        for chunkOffset in stride(from: 0, to: msg.count, by: 64) {
            var M = [UInt32](repeating: 0, count: 16)
            for i in 0..<16 {
                let j = chunkOffset + i*4
                M[i] = UInt32(msg[j]) | UInt32(msg[j+1])<<8 | UInt32(msg[j+2])<<16 | UInt32(msg[j+3])<<24
            }
            var A = a0
            var B = b0
            var C = c0
            var D = d0
            for i in 0..<64 {
                var F: UInt32 = 0
                var g = 0
                if i < 16 {
                    F = (B & C) | ((~B) & D)
                    g = i
                } else if i < 32 {
                    F = (D & B) | ((~D) & C)
                    g = (5*i + 1) %% 16
                } else if i < 48 {
                    F = B ^ C ^ D
                    g = (3*i + 5) %% 16
                } else {
                    F = C ^ (B | (~D))
                    g = (7*i) %% 16
                }
                let temp = D
                D = C
                C = B
                B = B &+ leftrotate(A &+ F &+ K[i] &+ M[g], s[i])
                A = temp
            }
            a0 = a0 &+ A
            b0 = b0 &+ B
            c0 = c0 &+ C
            d0 = d0 &+ D
        }
        func toBytes(_ v: UInt32) -> [UInt8] { return [UInt8(v & 0xff), UInt8((v >> 8) & 0xff), UInt8((v >> 16) & 0xff), UInt8((v >> 24) & 0xff)] }
        let digest = toBytes(a0) + toBytes(b0) + toBytes(c0) + toBytes(d0)
        return digest.map { String(format: "%%02x", $0) }.joined()
    }
}
`, alias)}
	}
	if lang == "python" && im.Path == "math" {
		if im.Auto {
			return &RawStmt{Code: fmt.Sprintf("struct %s {\n    static func sqrt(_ x: Double) -> Double { return Foundation.sqrt(x) }\n    static let pi = Double.pi\n}\n", alias)}
		}
		return &RawStmt{Code: fmt.Sprintf("struct %s {}\n", alias)}
	}
	if lang == "go" && im.Path == "net" && im.Auto {
		usesLookupHost = true
		return &RawStmt{Code: fmt.Sprintf("struct %s {\n    static func LookupHost(_ host: String) -> [Any] { return _lookup_host(host) }\n}\n", alias)}
	}
	return &RawStmt{Code: ""}
}

func convertExternVar(ev *parser.ExternVarDecl) Stmt {
	if ev.Root == "math" && len(ev.Tail) == 1 {
		name := ev.Tail[0]
		switch name {
		case "pi":
			return &RawStmt{Code: "extension math { static let pi = Double.pi }"}
		case "e":
			return &RawStmt{Code: "extension math { static let e = 2.718281828459045 }"}
		}
	}
	return &RawStmt{Code: ""}
}

func convertExternFun(ef *parser.ExternFunDecl) Stmt {
	if ef.Root == "math" && len(ef.Tail) == 1 {
		name := ef.Tail[0]
		switch name {
		case "sqrt":
			return &RawStmt{Code: "extension math { static func sqrt(_ x: Double) -> Double { Foundation.sqrt(x) } }"}
		case "pow":
			return &RawStmt{Code: "extension math { static func pow(_ x: Double, _ y: Double) -> Double { Foundation.pow(x, y) } }"}
		case "sin":
			return &RawStmt{Code: "extension math { static func sin(_ x: Double) -> Double { Foundation.sin(x) } }"}
		case "log":
			return &RawStmt{Code: "extension math { static func log(_ x: Double) -> Double { Foundation.log(x) } }"}
		}
	}
	return &RawStmt{Code: ""}
}

func convertForStmt(env *types.Env, fs *parser.ForStmt) (Stmt, error) {
	var child *types.Env
	if env != nil {
		child = types.NewEnv(env)
	}
	if fs.RangeEnd != nil {
		start, err := convertExpr(env, fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(env, fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		if child != nil {
			child.SetVar(fs.Name, types.IntType{}, true)
		}
		body, err := convertStmts(child, fs.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	expr, err := convertExpr(env, fs.Source)
	if err != nil {
		return nil, err
	}
	castMap := false
	keysOnly := false
	if child != nil {
		if t := types.TypeOfExpr(fs.Source, env); t != nil {
			switch tt := t.(type) {
			case types.GroupType:
				castMap = true
				child.SetVar(fs.Name, types.MapType{Key: types.StringType{}, Value: types.AnyType{}}, true)
			case types.ListType:
				switch tt.Elem.(type) {
				case types.GroupType, types.MapType:
					castMap = true
				}
				child.SetVar(fs.Name, tt.Elem, true)
			case types.MapType:
				keysOnly = true
				child.SetVar(fs.Name, tt.Key, true)
			}
		}
	}
	body, err := convertStmts(child, fs.Body)
	if err != nil {
		return nil, err
	}
	return &ForEachStmt{Name: fs.Name, Expr: expr, Body: body, CastMap: castMap, Keys: keysOnly}, nil
}

func convertSaveStmt(env *types.Env, se *parser.SaveExpr) (Stmt, error) {
	src, err := convertExpr(env, se.Src)
	if err != nil {
		return nil, err
	}
	format := parseFormat(se.With)
	path := ""
	if se.Path != nil {
		path = strings.Trim(*se.Path, "\"")
	}
	if format != "jsonl" || (path != "" && path != "-") {
		return nil, fmt.Errorf("unsupported save")
	}
	return &SaveStmt{Src: src}, nil
}

func convertUpdateStmt(env *types.Env, us *parser.UpdateStmt) (Stmt, error) {
	if env == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := env.GetVar(us.Target)
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
	child.SetVar("item", st, true)
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
	}
	var fields []string
	var values []Expr
	locals := make([]string, len(st.Order))
	copy(locals, st.Order)
	for _, it := range us.Set.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(child, it.Value)
		if err != nil {
			return nil, err
		}
		fields = append(fields, key)
		values = append(values, val)
	}
	var cond Expr
	if us.Where != nil {
		cond, err = convertExpr(child, us.Where)
		if err != nil {
			return nil, err
		}
	}
	return &UpdateStmt{Target: us.Target, Fields: fields, Values: values, Cond: cond, Locals: locals}, nil
}

func convertTypeDecl(env *types.Env, td *parser.TypeDecl) (Stmt, error) {
	if td.Alias != nil {
		// simple type alias
		ali := ""
		if td.Alias.Fun != nil {
			// check for self-referential alias like Church numerals
			selfRef := false
			for _, p := range td.Alias.Fun.Params {
				if p.Simple != nil && *p.Simple == td.Name {
					selfRef = true
					break
				}
			}
			if !selfRef && td.Alias.Fun.Return != nil && td.Alias.Fun.Return.Simple != nil && *td.Alias.Fun.Return.Simple == td.Name {
				selfRef = true
			}
			params := make([]string, len(td.Alias.Fun.Params))
			for i, p := range td.Alias.Fun.Params {
				esc := isFuncTypeRef(p)
				if !esc && p.Simple != nil {
					if ali, ok := env.LookupType(*p.Simple); ok {
						esc = isFuncType(ali)
					}
				}
				typ := toSwiftTypeSelf(p, td.Name, env)
				if selfRef && p.Simple != nil && *p.Simple == td.Name {
					esc = false
				}
				if esc {
					params[i] = "@escaping " + typ
				} else {
					params[i] = typ
				}
			}
			ret := "Any"
			if td.Alias.Fun.Return != nil {
				ret = toSwiftTypeSelf(td.Alias.Fun.Return, td.Name, env)
			}
			ali = "(" + strings.Join(params, ", ") + ") -> " + ret
		} else {
			ali = toSwiftType(td.Alias)
		}
		return &RawStmt{Code: fmt.Sprintf("typealias %s = %s\n", td.Name, ali)}, nil
	}
	if len(td.Variants) > 0 {
		if _, ok := env.GetUnion(td.Name); ok {
			variants := make([]UnionVariant, 0, len(td.Variants))
			for _, v := range td.Variants {
				st, ok := env.GetStruct(v.Name)
				if !ok {
					continue
				}
				fields := make([]StructField, len(st.Order))
				for i, n := range st.Order {
					fields[i] = StructField{Name: n, Type: swiftTypeOf(st.Fields[n])}
				}
				variants = append(variants, UnionVariant{Name: v.Name, Fields: fields})
			}
			return &UnionDef{Name: td.Name, Variants: variants}, nil
		}
		return &BlockStmt{}, nil
	}
	var fields []StructField
	fieldTypes := map[string]types.Type{}
	hasFunc := false
	nonCodable := false
	for _, m := range td.Members {
		if m.Field != nil {
			t := types.ResolveTypeRef(m.Field.Type, env)
			swiftT := swiftTypeOf(t)
			fields = append(fields, StructField{Name: m.Field.Name, Type: swiftT})
			fieldTypes[m.Field.Name] = t
			if isFuncType(t) {
				hasFunc = true
				nonCodable = true
			}
			if st2, ok := t.(types.StructType); ok {
				if nonCodableStructs[st2.Name] {
					nonCodable = true
				}
			}
		}
	}
	var methods []Stmt
	for _, m := range td.Members {
		if m.Method != nil {
			child := types.NewEnv(env)
			child.SetVar("self", types.StructType{Name: td.Name}, true)
			for name, ft := range fieldTypes {
				child.SetVar(name, ft, true)
			}
			fn := *m.Method
			fn.Params = append([]*parser.Param{{Name: "self", Type: &parser.TypeRef{Simple: &td.Name}}}, fn.Params...)
			fn.Name = fmt.Sprintf("%s_%s", td.Name, fn.Name)
			md, err := convertFunDecl(child, &fn)
			if err != nil {
				return nil, err
			}
			methods = append(methods, md)
		}
	}
	sd := &StructDef{Name: td.Name, Fields: fields, Class: hasFunc, Codable: !nonCodable}
	if hasFunc {
		classStructs[td.Name] = true
	}
	if nonCodable {
		nonCodableStructs[td.Name] = true
	}
	stmts := []Stmt{sd}
	stmts = append(stmts, methods...)
	return &BlockStmt{Stmts: stmts}, nil
}

func evalPrintArg(arg *parser.Expr) (val string, isString bool, ok bool) {
	lit := arg.Binary.Left.Value.Target.Lit
	if lit != nil && len(arg.Binary.Left.Ops) == 0 && len(arg.Binary.Left.Value.Ops) == 0 && len(arg.Binary.Right) == 0 {
		switch {
		case lit.Str != nil:
			return *lit.Str, true, true
		case lit.Int != nil:
			return fmt.Sprintf("%d", *lit.Int), false, true
		}
	}

	if v, ok := intConst(arg); ok {
		return fmt.Sprintf("%d", v), false, true
	}

	// cast string literal to int
	if lit != nil && lit.Str != nil && len(arg.Binary.Left.Value.Ops) == 1 {
		if c := arg.Binary.Left.Value.Ops[0].Cast; c != nil && c.Type != nil && c.Type.Simple != nil && *c.Type.Simple == "int" {
			return *lit.Str, false, true
		}
	}

	if lit != nil && len(arg.Binary.Right) == 1 {
		op := arg.Binary.Right[0]
		rightLit := op.Right.Target.Lit
		if rightLit != nil && rightLit.Str != nil && lit.Str != nil {
			switch op.Op {
			case "+":
				return *lit.Str + *rightLit.Str, true, true
			case "<", "<=", ">", ">=":
				left := *lit.Str
				right := *rightLit.Str
				var res bool
				switch op.Op {
				case "<":
					res = left < right
				case "<=":
					res = left <= right
				case ">":
					res = left > right
				case ">=":
					res = left >= right
				}
				if res {
					return "1", false, true
				}
				return "0", false, true
			}
		}
	}

	return "", false, false
}

// Print writes a lisp-like representation of the AST to stdout using the ast package.
func Print(prog *Program) {
	fmt.Print(toNode(prog).String())
}

// toNode converts the Program to an ast.Node tree.
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *PrintStmt:
		return &ast.Node{Kind: "print"}
	case *VarDecl:
		return &ast.Node{Kind: "var", Value: st.Name}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name}
	case *FunDecl:
		return &ast.Node{Kind: "fun", Value: st.Name}
	case *ReturnStmt:
		return &ast.Node{Kind: "return"}
	case *WhileStmt:
		return &ast.Node{Kind: "while"}
	case *ForRangeStmt:
		return &ast.Node{Kind: "for-range"}
	case *ForEachStmt:
		return &ast.Node{Kind: "for-each"}
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ExprStmt:
		return &ast.Node{Kind: "expr"}
	default:
		return &ast.Node{Kind: "stmt"}
	}
}

func intConst(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil {
		return 0, false
	}
	return evalIntConstBinary(e.Binary)
}

func evalIntConstBinary(be *parser.BinaryExpr) (int, bool) {
	v, ok := evalIntConstUnary(be.Left)
	if !ok {
		return 0, false
	}
	vals := []int{v}
	ops := []string{}
	for _, op := range be.Right {
		r, ok := evalIntConstPostfix(op.Right)
		if !ok {
			return 0, false
		}
		vals = append(vals, r)
		ops = append(ops, op.Op)
	}

	for i := 0; i < len(ops); {
		switch ops[i] {
		case "*":
			vals[i] *= vals[i+1]
		case "/":
			if vals[i+1] == 0 {
				return 0, false
			}
			vals[i] /= vals[i+1]
		case "%":
			if vals[i+1] == 0 {
				return 0, false
			}
			vals[i] %= vals[i+1]
		default:
			i++
			continue
		}
		vals = append(vals[:i+1], vals[i+2:]...)
		ops = append(ops[:i], ops[i+1:]...)
	}

	res := vals[0]
	for i, op := range ops {
		switch op {
		case "+":
			res += vals[i+1]
		case "-":
			res -= vals[i+1]
		default:
			return 0, false
		}
	}
	return res, true
}

func evalIntConstUnary(u *parser.Unary) (int, bool) {
	val, ok := evalIntConstPostfix(u.Value)
	if !ok {
		return 0, false
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = -val
		case "+":
			// ignore
		default:
			return 0, false
		}
	}
	return val, true
}

func evalIntConstPostfix(p *parser.PostfixExpr) (int, bool) {
	if p == nil || len(p.Ops) != 0 {
		return 0, false
	}
	return evalIntConstPrimary(p.Target)
}

func evalIntConstPrimary(pr *parser.Primary) (int, bool) {
	if pr == nil {
		return 0, false
	}
	if pr.Lit != nil && pr.Lit.Int != nil {
		return int(*pr.Lit.Int), true
	}
	if pr.Group != nil {
		return intConst(pr.Group)
	}
	return 0, false
}

func boolAsInt(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && len(e.Binary.Left.Ops) > 0 {
		for _, op := range e.Binary.Left.Ops {
			if op == "!" {
				return true
			}
		}
	}
	for _, op := range e.Binary.Right {
		switch op.Op {
		case "in":
			return false
		case "&&", "||", "<", "<=", ">", ">=", "==", "!=":
			return true
		}
	}
	return false
}

func isBoolLitExpr(e Expr) bool {
	if l, ok := e.(*LitExpr); ok && !l.IsString {
		return l.Value == "true" || l.Value == "false"
	}
	return false
}

func convertExpr(env *types.Env, e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}

	first, err := convertUnary(env, e.Binary.Left)
	if err != nil {
		return nil, err
	}

	operands := []Expr{first}
	typesList := []types.Type{types.TypeOfUnary(e.Binary.Left, env)}
	ops := []string{}

	for _, op := range e.Binary.Right {
		right, err := convertPostfix(env, op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		typesList = append(typesList, types.TypeOfPostfix(op.Right, env))
		ops = append(ops, op.Op)
	}

	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 4
		case "+", "-":
			return 3
		case "==", "!=", "<", "<=", ">", ">=", "in":
			return 2
		case "&&":
			return 1
		case "||":
			return 0
		default:
			return -1
		}
	}

	var exprStack []Expr
	var typeStack []types.Type
	var opStack []string

	pushResult := func() {
		if len(opStack) == 0 || len(exprStack) < 2 || len(typeStack) < 2 {
			return
		}
		op := opStack[len(opStack)-1]
		opStack = opStack[:len(opStack)-1]

		right := exprStack[len(exprStack)-1]
		rtyp := typeStack[len(typeStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		typeStack = typeStack[:len(typeStack)-1]

		left := exprStack[len(exprStack)-1]
		ltyp := typeStack[len(typeStack)-1]
		exprStack = exprStack[:len(exprStack)-1]
		typeStack = typeStack[:len(typeStack)-1]

		if op == "in" && env != nil {
			inMap := false
			if types.IsMapType(rtyp) || types.IsStructType(rtyp) {
				inMap = true
			} else {
				switch rv := right.(type) {
				case *NameExpr:
					if t, err := env.GetVar(rv.Name); err == nil {
						if types.IsMapType(t) || types.IsStructType(t) {
							inMap = true
						}
					}
				case *MapLit:
					inMap = true
				}
			}
			exprStack = append(exprStack, &BinaryExpr{Left: left, Op: op, Right: right, InMap: inMap})
			typeStack = append(typeStack, types.BoolType{})
			return
		}

		if ot, ok := ltyp.(types.OptionType); ok {
			ltyp = ot.Elem
			left = &CastExpr{Expr: left, Type: swiftTypeOf(ot.Elem) + "!"}
		}
		if ot, ok := rtyp.(types.OptionType); ok {
			rtyp = ot.Elem
			right = &CastExpr{Expr: right, Type: swiftTypeOf(ot.Elem) + "!"}
		}
		if types.IsAnyType(rtyp) && types.IsIntType(ltyp) {
			if _, ok := right.(*IndexExpr); ok {
				right = &CastExpr{Expr: right, Type: "Int!"}
				rtyp = types.IntType{}
			}
		}
		if types.IsAnyType(ltyp) && types.IsIntType(rtyp) {
			left = &CastExpr{Expr: left, Type: "Int!"}
			ltyp = types.IntType{}
		}

		switch op {
		case "==", "!=", "<", "<=", ">", ">=":
			if types.IsStringType(ltyp) && isBoolLitExpr(right) {
				if lit, ok := right.(*LitExpr); ok {
					right = &LitExpr{Value: lit.Value, IsString: true}
					rtyp = types.StringType{}
				}
			} else if types.IsStringType(rtyp) && isBoolLitExpr(left) {
				if lit, ok := left.(*LitExpr); ok {
					left = &LitExpr{Value: lit.Value, IsString: true}
					ltyp = types.StringType{}
				}
			}
			if types.IsAnyType(ltyp) && types.IsAnyType(rtyp) {
				ls := exprString(left)
				rs := exprString(right)
				if ls == "nil" {
					left = &RawStmt{Code: "String(describing: nil as Any?)"}
				} else {
					left = &RawStmt{Code: fmt.Sprintf("String(describing: %s)", ls)}
				}
				if rs == "nil" {
					right = &RawStmt{Code: "String(describing: nil as Any?)"}
				} else {
					right = &RawStmt{Code: fmt.Sprintf("String(describing: %s)", rs)}
				}
			} else if (types.IsStructType(ltyp) || types.IsStructType(rtyp)) ||
				(types.IsListType(ltyp) && types.IsStructType(ltyp.(types.ListType).Elem)) ||
				(types.IsListType(rtyp) && types.IsStructType(rtyp.(types.ListType).Elem)) {
				left = &RawStmt{Code: fmt.Sprintf("String(describing: %s)", exprString(left))}
				right = &RawStmt{Code: fmt.Sprintf("String(describing: %s)", exprString(right))}
			} else if func() bool {
				rt := rtyp
				if ot, ok := rtyp.(types.OptionType); ok {
					rt = ot.Elem
				}
				return types.IsIntType(ltyp) && types.IsAnyType(rt)
			}() {
				right = &CastExpr{Expr: right, Type: "Int!"}
				rtyp = types.IntType{}
			} else if func() bool {
				lt := ltyp
				if ot, ok := ltyp.(types.OptionType); ok {
					lt = ot.Elem
				}
				return types.IsIntType(rtyp) && types.IsAnyType(lt)
			}() {
				left = &CastExpr{Expr: left, Type: "Int!"}
				ltyp = types.IntType{}
			} else if types.IsBoolType(ltyp) && types.IsAnyType(rtyp) {
				right = &CastExpr{Expr: right, Type: "Bool!"}
				rtyp = types.BoolType{}
			} else if types.IsBoolType(rtyp) && types.IsAnyType(ltyp) {
				left = &CastExpr{Expr: left, Type: "Bool!"}
				ltyp = types.BoolType{}
			} else if types.IsFloatType(ltyp) && types.IsAnyType(rtyp) {
				right = &CastExpr{Expr: right, Type: "Double!"}
				rtyp = types.FloatType{}
			} else if types.IsFloatType(rtyp) && types.IsAnyType(ltyp) {
				left = &CastExpr{Expr: left, Type: "Double!"}
				ltyp = types.FloatType{}
			} else if types.IsFloatType(ltyp) && types.IsIntType(rtyp) {
				right = &CastExpr{Expr: right, Type: "Double"}
				rtyp = types.FloatType{}
			} else if types.IsFloatType(rtyp) && types.IsIntType(ltyp) {
				left = &CastExpr{Expr: left, Type: "Double"}
				ltyp = types.FloatType{}
			} else if types.IsStringType(ltyp) && types.IsAnyType(rtyp) {
				right = &CastExpr{Expr: right, Type: "String"}
				rtyp = types.StringType{}
			} else if types.IsStringType(rtyp) && types.IsAnyType(ltyp) {
				left = &CastExpr{Expr: left, Type: "String"}
				ltyp = types.StringType{}
			} else if types.IsBigIntType(ltyp) && types.IsIntType(rtyp) {
				right = &CallExpr{Func: "BigInt", Args: []Expr{right}}
				rtyp = types.BigIntType{}
			} else if types.IsBigIntType(rtyp) && types.IsIntType(ltyp) {
				left = &CallExpr{Func: "BigInt", Args: []Expr{left}}
				ltyp = types.BigIntType{}
			}
		case "&&", "||":
			if types.IsAnyType(ltyp) {
				left = &CastExpr{Expr: left, Type: "Bool!"}
				ltyp = types.BoolType{}
			}
			if types.IsAnyType(rtyp) {
				right = &CastExpr{Expr: right, Type: "Bool!"}
				rtyp = types.BoolType{}
			}
		case "+", "-", "*", "/", "%":
			if types.IsFloatType(ltyp) && types.IsIntType(rtyp) {
				right = &CastExpr{Expr: right, Type: "Double"}
				rtyp = types.FloatType{}
			} else if types.IsFloatType(rtyp) && types.IsIntType(ltyp) {
				left = &CastExpr{Expr: left, Type: "Double"}
				ltyp = types.FloatType{}
			}
			if types.IsBigIntType(ltyp) || types.IsBigIntType(rtyp) {
				if !types.IsBigIntType(ltyp) {
					left = &CallExpr{Func: "BigInt", Args: []Expr{left}}
					ltyp = types.BigIntType{}
				}
				if !types.IsBigIntType(rtyp) {
					right = &CallExpr{Func: "BigInt", Args: []Expr{right}}
					rtyp = types.BigIntType{}
				}
			}
			if op == "+" && (types.IsStringType(ltyp) || types.IsStringType(rtyp)) {
				if types.IsAnyType(ltyp) {
					left = &CastExpr{Expr: left, Type: "String"}
				}
				if types.IsAnyType(rtyp) {
					right = &CastExpr{Expr: right, Type: "String"}
				}
			} else {
				if types.IsAnyType(ltyp) && types.IsAnyType(rtyp) {
					left = &CallExpr{Func: "_num", Args: []Expr{left}}
					right = &CallExpr{Func: "_num", Args: []Expr{right}}
					ltyp = types.FloatType{}
					rtyp = types.FloatType{}
					usesNum = true
				} else {
					if types.IsAnyType(ltyp) {
						if types.IsIntType(rtyp) || types.IsInt64Type(rtyp) {
							left = &CastExpr{Expr: left, Type: "Int!"}
							ltyp = types.IntType{}
						} else {
							left = &CastExpr{Expr: left, Type: "Double!"}
							ltyp = types.FloatType{}
						}
					}
					if types.IsAnyType(rtyp) {
						if types.IsIntType(ltyp) || types.IsInt64Type(ltyp) {
							right = &CastExpr{Expr: right, Type: "Int!"}
							rtyp = types.IntType{}
						} else {
							right = &CastExpr{Expr: right, Type: "Double!"}
							rtyp = types.FloatType{}
						}
					}
				}
			}
		}

		resType := ltyp
		switch op {
		case "==", "!=", "<", "<=", ">", ">=", "in", "&&", "||":
			resType = types.BoolType{}
		case "+", "-", "*", "/", "%":
			if types.IsStringType(ltyp) || types.IsStringType(rtyp) {
				resType = types.StringType{}
			} else if types.IsBigIntType(ltyp) || types.IsBigIntType(rtyp) {
				resType = types.BigIntType{}
			} else if (types.IsIntType(ltyp) || types.IsInt64Type(ltyp)) &&
				(types.IsIntType(rtyp) || types.IsInt64Type(rtyp)) {
				if types.IsInt64Type(ltyp) || types.IsInt64Type(rtyp) {
					resType = types.Int64Type{}
				} else {
					resType = types.IntType{}
				}
			} else {
				resType = types.FloatType{}
			}
		default:
			resType = rtyp
		}

		be := &BinaryExpr{Left: left, Op: op, Right: right, InMap: false}
		if (types.IsIntType(ltyp) || types.IsInt64Type(ltyp)) && (types.IsIntType(rtyp) || types.IsInt64Type(rtyp)) {
			be.IntOp = true
		}
		if op == "%" && (types.IsFloatType(ltyp) || types.IsFloatType(rtyp)) {
			be.FloatMod = true
		}
		exprStack = append(exprStack, be)
		typeStack = append(typeStack, resType)
	}

	exprStack = append(exprStack, operands[0])
	typeStack = append(typeStack, typesList[0])
	for i, op := range ops {
		for len(opStack) > 0 && prec(opStack[len(opStack)-1]) >= prec(op) {
			pushResult()
		}
		opStack = append(opStack, op)
		exprStack = append(exprStack, operands[i+1])
		typeStack = append(typeStack, typesList[i+1])
	}
	for len(opStack) > 0 {
		pushResult()
	}
	if len(exprStack) != 1 {
		return nil, fmt.Errorf("binary conversion failed")
	}
	return exprStack[0], nil
}

func convertUnary(env *types.Env, u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(env, u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			if env != nil {
				t := types.TypeOfUnary(u, env)
				if ot, ok := t.(types.OptionType); ok {
					t = ot.Elem
				}
				if types.IsAnyType(t) {
					expr = &CastExpr{Expr: expr, Type: "Int"}
				}
			}
			expr = &UnaryExpr{Op: "-", Expr: expr}
		case "!":
			if env != nil {
				sub := &parser.Unary{Value: u.Value, Ops: u.Ops[:i]}
				t := types.TypeOfUnary(sub, env)
				if ot, ok := t.(types.OptionType); ok {
					t = ot.Elem
				}
				if types.IsAnyType(t) {
					expr = &CastExpr{Expr: expr, Type: "Bool!"}
				}
			}
			expr = &UnaryExpr{Op: "!", Expr: expr}
		case "+":
			// ignore
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(env *types.Env, p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(env, p.Target)
	if err != nil {
		if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 {
			expr = &NameExpr{Name: p.Target.Selector.Root}
		} else {
			return nil, err
		}
	}
	tail := []string{}
	if p.Target != nil && p.Target.Selector != nil {
		tail = p.Target.Selector.Tail
	}

	// handle `.contains(x)` as `x in expr`
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && len(tail) > 0 && tail[len(tail)-1] == "contains" {
		for _, f := range tail[:len(tail)-1] {
			expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: f, IsString: true}, Force: true}
		}
		arg, err := convertExpr(env, p.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		return &BinaryExpr{Left: arg, Op: "in", Right: expr}, nil
	}

	// handle `.keys()` as `_keys(expr)`
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && len(p.Ops[0].Call.Args) == 0 && len(tail) > 0 && tail[len(tail)-1] == "keys" {
		for _, f := range tail[:len(tail)-1] {
			expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: f, IsString: true}, Force: true}
		}
		usesKeys = true
		return &CallExpr{Func: "_keys", Args: []Expr{expr}}, nil
	}

	// handle `.get(key, default)`
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && len(tail) > 0 && tail[len(tail)-1] == "get" {
		for _, f := range tail[:len(tail)-1] {
			expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: f, IsString: true}, Force: true}
		}
		key, err := convertExpr(env, p.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		var def Expr = &LitExpr{Value: "nil", IsString: false}
		if len(p.Ops[0].Call.Args) > 1 {
			def, err = convertExpr(env, p.Ops[0].Call.Args[1])
			if err != nil {
				return nil, err
			}
		}
		return &MapGetExpr{Map: expr, Key: key, Default: def}, nil
	}

	if len(tail) > 0 {
		var t types.Type
		if env != nil && p.Target != nil && p.Target.Selector != nil {
			if tt, err := env.GetVar(p.Target.Selector.Root); err == nil {
				t = tt
			}
		}
		if t == nil {
			t = types.TypeOfPrimaryBasic(p.Target, env)
		}
		for i, f := range tail {
			if _, ok := t.(types.StructType); ok || t == nil || isAnyType(t) {
				if isAnyType(t) {
					if st, ok := structForField(env, f); ok {
						expr = &CastExpr{Expr: expr, Type: st.Name}
						t = st
					}
				}
				expr = &FieldExpr{Target: expr, Name: f}
				if st, ok := t.(types.StructType); ok {
					if ft, ok := st.Fields[f]; ok {
						t = ft
					}
				}
			} else {
				expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: f, IsString: true}, Force: true}
				if gt, ok := t.(types.GroupType); ok {
					if f == "key" {
						t = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
						if i < len(tail)-1 {
							expr = &CastExpr{Expr: expr, Type: "[String: Any]"}
						}
					} else if f == "items" {
						t = types.ListType{Elem: gt.Elem}
					}
				} else if mt, ok := t.(types.MapType); ok {
					t = mt.Value
					if i < len(tail)-1 {
						if _, ok := t.(types.MapType); ok {
							expr = &CastExpr{Expr: expr, Type: "[String: Any]"}
						}
					}
				}
				if st, ok := t.(types.StructType); ok {
					expr = &CastExpr{Expr: expr, Type: st.Name}
				}
			}
		}
	}

	var baseType types.Type
	if env != nil {
		baseType = types.TypeOfPrimaryBasic(p.Target, env)
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		if op.Index != nil {
			if op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil {
				var start, end Expr
				if op.Index.Start != nil {
					start, err = convertExpr(env, op.Index.Start)
					if err != nil {
						return nil, err
					}
				}
				if op.Index.End != nil {
					end, err = convertExpr(env, op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				isStr := false
				if env != nil {
					if types.IsStringType(baseType) || types.IsAnyType(baseType) {
						isStr = true
					}
					if lt, ok := baseType.(types.ListType); ok && types.IsStringType(lt.Elem) {
						if sv, ok1 := intConst(op.Index.Start); ok1 {
							if ev, ok2 := intConst(op.Index.End); ok2 && ev == sv+1 {
								expr = &IndexExpr{Base: expr, Index: start, AsString: true}
								baseType = lt.Elem
								continue
							}
						}
					}
				}
				expr = &SliceExpr{Base: expr, Start: start, End: end, AsString: isStr}
				if env != nil {
					if lt, ok := baseType.(types.ListType); ok {
						baseType = lt.Elem
					} else if types.IsStringType(baseType) {
						baseType = types.StringType{}
					}
				}
				continue
			}
			if op.Index.Start == nil || op.Index.Colon != nil || op.Index.End != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				return nil, fmt.Errorf("unsupported index")
			}
			idx, err := convertExpr(env, op.Index.Start)
			if err != nil {
				return nil, err
			}
			isStr := false
			force := false
			skipUpdate := false
			origBaseType := baseType
			if env != nil {
				if types.IsStringType(baseType) {
					isStr = true
				}
				if m, ok := baseType.(types.MapType); ok {
					baseType = types.OptionType{Elem: m.Value}
					// Map lookups return optionals; avoid force unwrap
				} else if l, ok := baseType.(types.ListType); ok {
					baseType = l.Elem
					if i+1 < len(p.Ops) && p.Ops[i+1].Index != nil {
						if _, ok2 := baseType.(types.ListType); ok2 {
							skipUpdate = true
						}
					}
				} else if _, ok := baseType.(types.StructType); ok {
					baseType = types.OptionType{Elem: types.AnyType{}}
					force = true
				}
			}
			if env != nil && types.IsAnyType(baseType) {
				if name := rootNameExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: p.Target}}}}); name != "" {
					if t, err := env.GetVar(name); err == nil && !types.IsAnyType(t) {
						baseType = t
					}
				}
			}
			if env != nil && types.IsAnyType(baseType) {
				idxType := types.TypeOfExpr(op.Index.Start, env)
				cast := "[Any]"
				if types.IsStringType(idxType) {
					cast = "[String: Any]"
				} else if types.IsAnyType(idxType) {
					idx = &CastExpr{Expr: idx, Type: "Int!"}
				}
				expr = &IndexExpr{Base: &CastExpr{Expr: expr, Type: cast}, Index: idx, AsString: isStr, Force: force}
			} else {
				keyStr := false
				keyAny := false
				if mt, ok := origBaseType.(types.MapType); ok {
					if types.IsStringType(mt.Key) {
						idx = &CastExpr{Expr: idx, Type: "String"}
						keyStr = true
					} else if types.IsAnyType(mt.Key) {
						keyAny = true
					}
				} else if lt, ok := origBaseType.(types.ListType); ok {
					if env != nil {
						if idxT := types.TypeOfExpr(op.Index.Start, env); types.IsAnyType(idxT) {
							idx = &CastExpr{Expr: idx, Type: "Int!"}
						}
					}
					_ = lt // silence unused if lt not used
				}
				expr = &IndexExpr{Base: expr, Index: idx, AsString: isStr, Force: force, KeyString: keyStr, KeyAny: keyAny}
			}
			if env != nil && !skipUpdate {
				// update baseType after indexing
				switch bt := baseType.(type) {
				case types.OptionType:
					baseType = bt.Elem
				case types.MapType:
					baseType = bt.Value
				case types.ListType:
					baseType = bt.Elem
				default:
					baseType = types.AnyType{}
				}
			}
			continue
		}
		if op.Field != nil && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil && op.Field.Name == "contains" {
			arg, err := convertExpr(env, p.Ops[i+1].Call.Args[0])
			if err != nil {
				return nil, err
			}
			expr = &BinaryExpr{Left: arg, Op: "in", Right: expr}
			i++
			continue
		}
		if op.Field != nil && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil && op.Field.Name == "padStart" {
			a1, err := convertExpr(env, p.Ops[i+1].Call.Args[0])
			if err != nil {
				return nil, err
			}
			a2, err := convertExpr(env, p.Ops[i+1].Call.Args[1])
			if err != nil {
				return nil, err
			}
			usesPad = true
			expr = &CallExpr{Func: "_padStart", Args: []Expr{&CallExpr{Func: "_p", Args: []Expr{expr}}, a1, a2}}
			i++
			continue
		}
		if op.Field != nil {
			// Accessing a field after previous operations (e.g. foo().bar)
			if env != nil {
				if st, ok := baseType.(types.StructType); ok {
					if ft, ok2 := st.Fields[op.Field.Name]; ok2 {
						baseType = ft
					} else {
						baseType = types.AnyType{}
					}
					expr = &FieldExpr{Target: expr, Name: op.Field.Name}
				} else if isAnyType(baseType) {
					if st, ok := structForField(env, op.Field.Name); ok {
						expr = &FieldExpr{Target: &CastExpr{Expr: expr, Type: st.Name}, Name: op.Field.Name}
						baseType = st.Fields[op.Field.Name]
					} else {
						expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: op.Field.Name, IsString: true}, Force: true}
						baseType = types.AnyType{}
					}
				} else {
					expr = &IndexExpr{Base: expr, Index: &LitExpr{Value: op.Field.Name, IsString: true}, Force: true}
					if mt, ok := baseType.(types.MapType); ok {
						baseType = mt.Value
					} else {
						baseType = types.AnyType{}
					}
				}
			} else {
				expr = &FieldExpr{Target: expr, Name: op.Field.Name}
			}
			continue
		}
		if op.Call != nil {
			var ce *CallExpr
			var fnName string
			if fe, ok := expr.(*FieldExpr); ok {
				if n, ok2 := fe.Target.(*NameExpr); ok2 && env != nil {
					if vt, err := env.GetVar(n.Name); err == nil {
						if st, ok3 := vt.(types.StructType); ok3 {
							if _, okm := st.Methods[fe.Name]; okm {
								fnName = fmt.Sprintf("%s_%s", st.Name, fe.Name)
								ce = &CallExpr{Func: fnName, Args: []Expr{&NameExpr{Name: n.Name}}}
							}
						}
					}
				}
			}
			usedSelf := false
			if ce == nil {
				if ne, ok := expr.(*NameExpr); ok && env != nil {
					if st, ok2 := structForMethod(env, ne.Name); ok2 {
						fnName = fmt.Sprintf("%s_%s", st.Name, ne.Name)
						ce = &CallExpr{Func: fnName, Args: []Expr{&NameExpr{Name: "self"}}}
						usedSelf = true
					} else if vt, err := env.GetVar("self"); err == nil {
						if st, ok2 := vt.(types.StructType); ok2 {
							if _, ok3 := st.Methods[ne.Name]; ok3 {
								fnName = fmt.Sprintf("%s_%s", st.Name, ne.Name)
								ce = &CallExpr{Func: fnName, Args: []Expr{&NameExpr{Name: "self"}}}
								usedSelf = true
							} else {
								fnName = ne.Name
								ce = &CallExpr{Func: fnName}
							}
						} else {
							fnName = ne.Name
							ce = &CallExpr{Func: fnName}
						}
					} else {
						fnName = ne.Name
						ce = &CallExpr{Func: fnName}
					}
				} else {
					fnName = exprString(expr)
					ce = &CallExpr{Func: fnName}
				}
			}
			if ce.Func == "now" && len(op.Call.Args) == 0 {
				usesNow = true
				ce.Func = "_now"
			}
			var paramTypes []types.Type
			var mutInfo []bool
			if env != nil {
				if t, err := env.GetVar(fnName); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						paramTypes = ft.Params
						baseType = ft.Return
					}
				}
			}
			if usedSelf && len(paramTypes) > 0 {
				paramTypes = paramTypes[1:]
			}
			if flags, ok := funcMutParams[fnName]; ok {
				mutInfo = flags
			}
			for j, a := range op.Call.Args {
				ae, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				if env != nil && j < len(paramTypes) {
					pt := swiftTypeOf(paramTypes[j])
					if lit, ok := ae.(*LitExpr); ok && lit.Value == "nil" && pt == "Any" {
						ae = &RawStmt{Code: "nil as Any?"}
					} else if pt == "Int" {
						srcT := swiftTypeOf(types.TypeOfExpr(a, env))
						if srcT != "Int" {
							ae = &CallExpr{Func: "_int", Args: []Expr{ae}}
							usesInt = true
						}
					} else if pt != "Any" && !(j < len(mutInfo) && mutInfo[j]) {
						srcT := swiftTypeOf(types.TypeOfExpr(a, env))
						if srcT != pt {
							ae = &CastExpr{Expr: ae, Type: pt + "!"}
						}
					}
				}
				ce.Args = append(ce.Args, ae)
			}
			expr = ce
			continue
		}
		if op.Cast != nil {
			typ := toSwiftType(op.Cast.Type)
			if env != nil {
				resT := types.ResolveTypeRef(op.Cast.Type, env)
				if typ == "Int" && types.IsStringType(baseType) {
					expr = &CastExpr{Expr: expr, Type: "Int!", FromString: true}
					baseType = resT
				} else if st, ok := resT.(types.StructType); ok {
					if ml, ok := expr.(*MapLit); ok {
						var fields []FieldInit
						for j := range ml.Keys {
							if lk, ok := ml.Keys[j].(*LitExpr); ok && lk.IsString {
								fields = append(fields, FieldInit{Name: lk.Value, Value: ml.Values[j]})
							}
						}
						expr = &StructInit{Name: st.Name, Fields: fields}
					} else {
						expr = &CastExpr{Expr: expr, Type: st.Name}
					}
					baseType = resT
				} else {
					if types.IsAnyType(baseType) {
						switch typ {
						case "Int", "Int64", "Double", "Bool", "String":
							typ += "!"
						}
					}
					expr = &CastExpr{Expr: expr, Type: typ}
					baseType = resT
				}
			} else {
				expr = &CastExpr{Expr: expr, Type: typ}
			}
			continue
		}
		return nil, fmt.Errorf("unsupported postfix")
	}
	if env != nil {
		if ce, ok := expr.(*CallExpr); ok && ce.Func == "input" {
			if t := types.TypeOfPostfix(p, env); types.IsStringType(t) {
				return expr, nil
			}
		}
		t := types.TypeOfPostfix(p, env)
		if ot, ok := t.(types.OptionType); ok {
			if ie, ok2 := expr.(*IndexExpr); ok2 && !ie.Force {
				return expr, nil
			}
			t = ot.Elem
		} else if ie, ok := expr.(*IndexExpr); ok && !ie.Force {
			// cast to expected non-optional type
			return &CastExpr{Expr: expr, Type: swiftTypeOf(t) + "!"}, nil
		}
		if baseType != nil && types.EqualTypes(baseType, t) {
			return expr, nil
		}
		if _, ok := expr.(*FieldExpr); !ok {
			switch {
			case types.IsIntType(t):
				if _, ok := expr.(*IndexExpr); ok {
					expr = &CastExpr{Expr: expr, Type: "Int!"}
				} else {
					expr = &CastExpr{Expr: expr, Type: "Int"}
				}
			case types.IsInt64Type(t):
				expr = &CastExpr{Expr: expr, Type: "Int64"}
			case types.IsFloatType(t):
				expr = &CastExpr{Expr: expr, Type: "Double"}
			case types.IsStringType(t):
				expr = &CastExpr{Expr: expr, Type: "String"}
			case types.IsBoolType(t):
				expr = &CastExpr{Expr: expr, Type: "Bool!"}
			case types.IsListType(t), types.IsMapType(t):
				expr = &CastExpr{Expr: expr, Type: swiftTypeOf(t)}
			}
		}
	}
	return expr, nil
}

func convertIfExpr(env *types.Env, i *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(env, i.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(env, i.Then)
	if err != nil {
		return nil, err
	}
	if i.ElseIf != nil {
		elseExpr, err := convertIfExpr(env, i.ElseIf)
		if err != nil {
			return nil, err
		}
		return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
	}
	var elseExpr Expr
	if i.Else != nil {
		elseExpr, err = convertExpr(env, i.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &LitExpr{Value: "0", IsString: false}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatchExpr(env *types.Env, me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(env, me.Target)
	if err != nil {
		return nil, err
	}
	typ := "Any"
	if len(me.Cases) > 0 {
		t0 := types.ExprType(me.Cases[0].Result, env)
		typ = swiftTypeOf(t0)
		for _, c := range me.Cases[1:] {
			if !types.EqualTypes(t0, types.ExprType(c.Result, env)) {
				typ = "Any"
				break
			}
		}
	}
	if len(me.Cases) > 0 {
		var firstVar string
		if name, ok := identName(me.Cases[0].Pattern); ok {
			firstVar = name
		} else if call, ok := callPattern(me.Cases[0].Pattern); ok {
			firstVar = call.Func
		}
		if firstVar != "" {
			if _, ok := env.FindUnionByVariant(firstVar); ok {
				cases := make([]UnionMatchCase, len(me.Cases))
				for i, c := range me.Cases {
					var variant string
					var bindings []string
					if n, ok := identName(c.Pattern); ok {
						variant = n
					} else if call, ok := callPattern(c.Pattern); ok {
						variant = call.Func
						bindings = make([]string, len(call.Args))
						for j, a := range call.Args {
							if nm, ok := identName(a); ok {
								bindings[j] = nm
							}
						}
					} else {
						variant = "_"
					}
					body, err := convertExpr(env, c.Result)
					if err != nil {
						return nil, err
					}
					cases[i] = UnionMatchCase{Variant: variant, Bindings: bindings, Body: body}
				}
				return &UnionMatchExpr{Target: target, Cases: cases, Type: typ}, nil
			}
		}
	}

	var expr Expr = &LitExpr{Value: "nil", IsString: false}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(env, c.Result)
		if err != nil {
			return nil, err
		}
		pat, err := convertExpr(env, c.Pattern)
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*NameExpr); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: pat}
		expr = &CondExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func convertQueryExpr(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil {
		return convertGroupQuery(env, q)
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" &&
		len(q.Froms) == 0 && q.Where == nil && !q.Distinct {
		return convertLeftJoinQuery(env, q)
	}
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	varType := types.TypeOfExpr(q.Source, env)
	if gt, ok := varType.(types.GroupType); ok {
		src = &CastExpr{Expr: &IndexExpr{Base: src, Index: &LitExpr{Value: "items", IsString: true}, Force: true}, Type: "[[String: Any]]"}
		varType = gt.Elem
	} else if lt, ok := varType.(types.ListType); ok {
		if gt2, ok := lt.Elem.(types.GroupType); ok {
			varType = gt2.Elem
		} else {
			varType = lt.Elem
		}
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, varType, true)
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := convertExpr(child, f.Src)
		if err != nil {
			return nil, err
		}
		t := types.TypeOfExpr(f.Src, child)
		if gt, ok := t.(types.GroupType); ok {
			fe = &CastExpr{Expr: &IndexExpr{Base: fe, Index: &LitExpr{Value: "items", IsString: true}, Force: true}, Type: "[[String: Any]]"}
			t = gt.Elem
		} else if lt, ok := t.(types.ListType); ok {
			if gt2, ok := lt.Elem.(types.GroupType); ok {
				t = gt2.Elem
			} else {
				t = lt.Elem
			}
		}
		child.SetVar(f.Var, t, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := convertExpr(child, j.Src)
		if err != nil {
			return nil, err
		}
		jt := types.TypeOfExpr(j.Src, child)
		if gt, ok := jt.(types.GroupType); ok {
			je = &CastExpr{Expr: &IndexExpr{Base: je, Index: &LitExpr{Value: "items", IsString: true}, Force: true}, Type: "[[String: Any]]"}
			jt = gt.Elem
		} else if lt, ok := jt.(types.ListType); ok {
			if gt2, ok := lt.Elem.(types.GroupType); ok {
				jt = gt2.Elem
			} else {
				jt = lt.Elem
			}
		}
		child.SetVar(j.Var, jt, true)
		var on Expr
		if j.On != nil {
			on, err = convertExpr(child, j.On)
			if err != nil {
				return nil, err
			}
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: on}
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(child, q.Where)
		if err != nil {
			return nil, err
		}
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, err
	}
	var sortExpr, skipExpr, takeExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(child, q.Sort)
		if err != nil {
			return nil, err
		}
	}
	if q.Skip != nil {
		skipExpr, err = convertExpr(child, q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(child, q.Take)
		if err != nil {
			return nil, err
		}
	}
	elemType := ""
	if t := types.TypeOfExpr(q.Select, child); t != nil {
		elemType = swiftTypeOf(t)
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Sort: sortExpr, Skip: skipExpr, Take: takeExpr, Select: sel, Elem: elemType}, nil
}

func convertLeftJoinQuery(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	j := q.Joins[0]
	leftSrc, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	lt := types.TypeOfExpr(q.Source, env)
	if llist, ok := lt.(types.ListType); ok {
		lt = llist.Elem
	} else if gt, ok := lt.(types.GroupType); ok {
		lt = gt.Elem
	}
	child.SetVar(q.Var, lt, true)
	rt := types.TypeOfExpr(j.Src, child)
	if rlist, ok := rt.(types.ListType); ok {
		rt = rlist.Elem
	} else if gt, ok := rt.(types.GroupType); ok {
		rt = gt.Elem
	}
	child.SetVar(j.Var, rt, true)
	rightSrc, err := convertExpr(child, j.Src)
	if err != nil {
		return nil, err
	}
	cond, err := convertExpr(child, j.On)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, err
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertGroupQuery(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	if q.Group == nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	t := types.TypeOfExpr(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := t.(types.ListType); ok {
		elemT = lt.Elem
	} else if gt, ok := t.(types.GroupType); ok {
		elemT = gt.Elem
	}
	child.SetVar(q.Var, elemT, true)

	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := convertExpr(child, f.Src)
		if err != nil {
			return nil, err
		}
		ft := types.TypeOfExpr(f.Src, child)
		if lt, ok := ft.(types.ListType); ok {
			ft = lt.Elem
		} else if gt, ok := ft.(types.GroupType); ok {
			ft = gt.Elem
		}
		child.SetVar(f.Var, ft, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}

	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := convertExpr(child, j.Src)
		if err != nil {
			return nil, err
		}
		jt := types.TypeOfExpr(j.Src, child)
		if lt, ok := jt.(types.ListType); ok {
			jt = lt.Elem
		} else if gt, ok := jt.(types.GroupType); ok {
			jt = gt.Elem
		}
		child.SetVar(j.Var, jt, true)
		var on Expr
		if j.On != nil {
			on, err = convertExpr(child, j.On)
			if err != nil {
				return nil, err
			}
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: on}
	}

	key, err := convertExpr(child, q.Group.Exprs[0])
	if err != nil {
		return nil, err
	}
	var keyT types.Type = types.AnyType{}
	if kt := types.TypeOfExpr(q.Group.Exprs[0], child); kt != nil {
		keyT = kt
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(child, q.Where)
		if err != nil {
			return nil, err
		}
	}

	if len(q.Joins) > 0 || len(q.Froms) > 0 {
		elemT = types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	}

	genv := types.NewEnv(env)
	genv.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: elemT}, true)
	sel, err := convertExpr(genv, q.Select)
	if err != nil {
		return nil, err
	}
	var sortExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(genv, q.Sort)
		if err != nil {
			return nil, err
		}
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(genv, q.Group.Having)
		if err != nil {
			return nil, err
		}
	}
	return &GroupByExpr{Var: q.Var, Source: src, Froms: froms, Joins: joins, Key: key, Name: q.Group.Name, Sort: sortExpr, Where: where, Select: sel, Having: having}, nil
}

func convertPrimary(env *types.Env, pr *parser.Primary) (Expr, error) {
	switch {
	case pr == nil:
		return nil, fmt.Errorf("nil primary")
	case pr.Lit != nil:
		if pr.Lit.Str != nil {
			return &LitExpr{Value: *pr.Lit.Str, IsString: true}, nil
		}
		if pr.Lit.Int != nil {
			return &LitExpr{Value: fmt.Sprintf("%d", *pr.Lit.Int), IsString: false}, nil
		}
		if pr.Lit.Float != nil {
			v := *pr.Lit.Float
			s := strconv.FormatFloat(v, 'f', -1, 64)
			if math.Trunc(v) == v && !strings.Contains(s, ".") {
				s += ".0"
			}
			return &LitExpr{Value: s, IsString: false}, nil
		}
		if pr.Lit.Bool != nil {
			if *pr.Lit.Bool {
				return &LitExpr{Value: "true", IsString: false}, nil
			}
			return &LitExpr{Value: "false", IsString: false}, nil
		}
		if pr.Lit.Null {
			return &LitExpr{Value: "nil", IsString: false}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
	case pr.List != nil:
		var elems []Expr
		wrapAny := false
		if env != nil {
			if lt, ok := types.TypeOfPrimaryBasic(pr, env).(types.ListType); ok {
				if isAnyType(lt.Elem) {
					wrapAny = true
				}
			}
		}
		for _, e := range pr.List.Elems {
			ce, err := convertExpr(env, e)
			if err != nil {
				return nil, err
			}
			if wrapAny {
				ce = &CastExpr{Expr: ce, Type: "Any"}
			}
			elems = append(elems, ce)
		}
		return &ArrayLit{Elems: elems}, nil
	case pr.Map != nil:
		var keys []Expr
		var vals []Expr
		for _, it := range pr.Map.Items {
			var k Expr
			if key, ok := types.SimpleStringKey(it.Key); ok {
				k = &LitExpr{Value: key, IsString: true}
			} else {
				var err error
				k, err = convertExpr(env, it.Key)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			keys = append(keys, k)
			vals = append(vals, v)
		}
		return &MapLit{Keys: keys, Values: vals}, nil
	case pr.Struct != nil:
		return convertStructLiteral(env, pr.Struct)
	case pr.Call != nil:
		if pr.Call.Func == "int" && len(pr.Call.Args) == 1 {
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			usesInt = true
			return &CallExpr{Func: "_int", Args: []Expr{arg}}, nil
		}
		if pr.Call.Func == "input" && len(pr.Call.Args) == 0 {
			return &CallExpr{Func: "input"}, nil
		}
		if pr.Call.Func == "append" && len(pr.Call.Args) == 2 {
			left, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			right, err := convertExpr(env, pr.Call.Args[1])
			if err != nil {
				return nil, err
			}
			if env != nil {
				if lt, ok := types.TypeOfExpr(pr.Call.Args[0], env).(types.ListType); ok {
					if _, ok2 := lt.Elem.(types.AnyType); ok2 {
						right = &CastExpr{Expr: right, Type: "Any?"}
					}
				}
			}
			usesAppend = true
			return &CallExpr{Func: "_append", Args: []Expr{left, right}}, nil
		}
		if pr.Call.Func == "now" && len(pr.Call.Args) == 0 {
			usesNow = true
			return &CallExpr{Func: "_now"}, nil
		}
		if pr.Call.Func == "len" && len(pr.Call.Args) == 1 {
			if n, ok := evalLenConst(pr.Call.Args[0]); ok {
				return &LitExpr{Value: fmt.Sprintf("%d", n), IsString: false}, nil
			}
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			asStr := true
			if env != nil {
				t := types.TypeOfExpr(pr.Call.Args[0], env)
				if types.IsListType(t) || types.IsMapType(t) || types.IsStringType(t) {
					asStr = false
				}
			}
			if asStr {
				usesLen = true
				return &CallExpr{Func: "_len", Args: []Expr{arg}}, nil
			}
			return &LenExpr{Value: arg, AsString: false}, nil
		}
		if pr.Call.Func == "str" && len(pr.Call.Args) == 1 {
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: "str", Args: []Expr{arg}}, nil
		}
		if pr.Call.Func == "keys" && len(pr.Call.Args) == 1 {
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			usesKeys = true
			return &CallExpr{Func: "_keys", Args: []Expr{arg}}, nil
		}
		if pr.Call.Func == "padStart" && len(pr.Call.Args) == 3 {
			a0, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			a1, err := convertExpr(env, pr.Call.Args[1])
			if err != nil {
				return nil, err
			}
			a2, err := convertExpr(env, pr.Call.Args[2])
			if err != nil {
				return nil, err
			}
			usesPad = true
			return &CallExpr{Func: "_padStart", Args: []Expr{a0, a1, a2}}, nil
		}
		if pr.Call.Func == "repeat" && len(pr.Call.Args) == 2 {
			a0, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			a1, err := convertExpr(env, pr.Call.Args[1])
			if err != nil {
				return nil, err
			}
			usesRepeat = true
			return &CallExpr{Func: "_repeat", Args: []Expr{a0, a1}}, nil
		}
		if pr.Call.Func == "panic" && len(pr.Call.Args) == 1 {
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: "fatalError", Args: []Expr{arg}}, nil
		}
		if pr.Call.Func == "num" && len(pr.Call.Args) == 1 {
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			usesRat = true
			return &CallExpr{Func: "_rat_num", Args: []Expr{arg}}, nil
		}
		if pr.Call.Func == "denom" && len(pr.Call.Args) == 1 {
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			usesRat = true
			return &CallExpr{Func: "_rat_denom", Args: []Expr{arg}}, nil
		}
		if pr.Call.Func == "split" && len(pr.Call.Args) == 2 {
			a0, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			a1, err := convertExpr(env, pr.Call.Args[1])
			if err != nil {
				return nil, err
			}
			usesSplit = true
			return &CallExpr{Func: "_split", Args: []Expr{a0, a1}}, nil
		}
		if pr.Call.Func == "sha256" && len(pr.Call.Args) == 1 {
			arg, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			usesSHA256 = true
			return &CallExpr{Func: "_sha256", Args: []Expr{arg}}, nil
		}
		if pr.Call.Func == "slice" && len(pr.Call.Args) == 3 {
			base, err := convertExpr(env, pr.Call.Args[0])
			if err != nil {
				return nil, err
			}
			start, err := convertExpr(env, pr.Call.Args[1])
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(env, pr.Call.Args[2])
			if err != nil {
				return nil, err
			}
			asStr := false
			if env != nil {
				t := types.TypeOfExpr(pr.Call.Args[0], env)
				if types.IsStringType(t) {
					asStr = true
				}
			}
			return &SliceExpr{Base: base, Start: start, End: end, AsString: asStr}, nil
		}
		ce := &CallExpr{Func: pr.Call.Func}
		var paramTypes []types.Type
		if env != nil {
			if t, err := env.GetVar(pr.Call.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					paramTypes = ft.Params
				}
			}
		}
		for j, a := range pr.Call.Args {
			ae, err := convertExpr(env, a)
			if err != nil {
				return nil, err
			}
			if env != nil && j < len(paramTypes) {
				pt := swiftTypeOf(paramTypes[j])
				if pt == "Int" {
					srcT := swiftTypeOf(types.TypeOfExpr(a, env))
					if srcT != "Int" {
						ae = &CallExpr{Func: "_int", Args: []Expr{ae}}
						usesInt = true
					}
				} else if pt != "Any" {
					ae = &CastExpr{Expr: ae, Type: pt + "!"}
				}
			}
			ce.Args = append(ce.Args, ae)
		}
		return ce, nil
	case pr.FunExpr != nil && pr.FunExpr.ExprBody != nil:
		child := types.NewEnv(env)
		for _, p := range pr.FunExpr.Params {
			var pt types.Type = types.AnyType{}
			if p.Type != nil {
				pt = types.ResolveTypeRef(p.Type, env)
			}
			child.SetVar(p.Name, pt, false)
		}
		if pr.FunExpr.Return != nil {
			rt := types.ResolveTypeRef(pr.FunExpr.Return, env)
			child.SetVar("$retType", rt, false)
		}
		body, err := convertExpr(child, pr.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		fn := &FunExpr{Ret: toSwiftType(pr.FunExpr.Return)}
		for _, p := range pr.FunExpr.Params {
			var pt types.Type = types.AnyType{}
			if p.Type != nil {
				pt = types.ResolveTypeRef(p.Type, env)
			}
			fn.Params = append(fn.Params, Param{Name: p.Name, Type: toSwiftType(p.Type), Escaping: isFuncTypeRef(p.Type) || isFuncType(pt)})
		}
		fn.Body = body
		return fn, nil
	case pr.FunExpr != nil && pr.FunExpr.BlockBody != nil:
		child := types.NewEnv(env)
		for _, p := range pr.FunExpr.Params {
			var pt types.Type = types.AnyType{}
			if p.Type != nil {
				pt = types.ResolveTypeRef(p.Type, env)
			}
			child.SetVar(p.Name, pt, false)
		}
		ret := toSwiftType(pr.FunExpr.Return)
		if pr.FunExpr.Return != nil {
			rt := types.ResolveTypeRef(pr.FunExpr.Return, env)
			child.SetVar("$retType", rt, false)
		} else {
			ret = "Any"
		}
		body, err := convertStmts(child, pr.FunExpr.BlockBody)
		if err != nil {
			return nil, err
		}
		if pr.FunExpr.Return == nil {
			body = append(body, &ReturnStmt{Expr: &RawStmt{Code: "nil as Any?"}})
		}
		fn := &FunBlock{Ret: ret}
		for _, p := range pr.FunExpr.Params {
			var pt types.Type = types.AnyType{}
			if p.Type != nil {
				pt = types.ResolveTypeRef(p.Type, env)
			}
			fn.Params = append(fn.Params, Param{Name: p.Name, Type: toSwiftType(p.Type), Escaping: isFuncTypeRef(p.Type) || isFuncType(pt)})
		}
		fn.Body = body
		return fn, nil
	case pr.Group != nil:
		return convertExpr(env, pr.Group)
	case pr.If != nil:
		return convertIfExpr(env, pr.If)
	case pr.Match != nil:
		return convertMatchExpr(env, pr.Match)
	case pr.Query != nil:
		return convertQueryExpr(env, pr.Query)
	case pr.Fetch != nil:
		urlExpr, err := convertExpr(env, pr.Fetch.URL)
		if err != nil {
			return nil, err
		}
		usesFetch = true
		return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}, nil
	case pr.Load != nil:
		format := parseFormat(pr.Load.With)
		path := ""
		if pr.Load.Path != nil {
			path = strings.Trim(*pr.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(env, path, format, pr.Load.Type)
		if err != nil {
			return nil, err
		}
		return expr, nil
	case pr.Selector != nil && len(pr.Selector.Tail) == 0:
		if env != nil {
			if selfT, err := env.GetVar("self"); err == nil {
				if st, ok := selfT.(types.StructType); ok {
					if _, ok2 := st.Fields[pr.Selector.Root]; ok2 {
						return &FieldExpr{Target: &NameExpr{Name: "self"}, Name: pr.Selector.Root}, nil
					}
				}
			}
		}
		return &NameExpr{Name: pr.Selector.Root}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func convertStructLiteral(env *types.Env, sl *parser.StructLiteral) (Expr, error) {
	var fields []FieldInit
	var keys []Expr
	var vals []Expr
	for _, f := range sl.Fields {
		v, err := convertExpr(env, f.Value)
		if err != nil {
			return nil, err
		}
		fields = append(fields, FieldInit{Name: f.Name, Value: v})
		keys = append(keys, &LitExpr{Value: f.Name, IsString: true})
		vals = append(vals, v)
	}
	if sl.Name != "" {
		if ut, ok := env.FindUnionByVariant(sl.Name); ok {
			return &UnionInit{Union: ut.Name, Variant: sl.Name, Fields: fields}, nil
		}
		return &StructInit{Name: sl.Name, Fields: fields}, nil
	}
	return &MapLit{Keys: keys, Values: vals}, nil
}

func evalLenConst(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil {
		return 0, false
	}
	left := e.Binary.Left
	if len(left.Ops) != 0 || len(e.Binary.Right) != 0 {
		return 0, false
	}
	t := left.Value.Target
	switch {
	case t.Lit != nil && t.Lit.Str != nil:
		return len(*t.Lit.Str), true
	case t.List != nil:
		return len(t.List.Elems), true
	case t.Map != nil:
		return len(t.Map.Items), true
	default:
		return 0, false
	}
}

func zeroValue(t *parser.TypeRef) Expr {
	if t == nil {
		return &LitExpr{Value: "nil", IsString: false}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			return &ArrayLit{}
		case "map":
			return &MapLit{}
		}
	}
	if t.Simple == nil {
		return &LitExpr{Value: "nil", IsString: false}
	}
	switch *t.Simple {
	case "int":
		return &LitExpr{Value: "0", IsString: false}
	case "float":
		return &LitExpr{Value: "0.0", IsString: false}
	case "string":
		return &LitExpr{Value: "", IsString: true}
	case "bool":
		return &LitExpr{Value: "false", IsString: false}
	default:
		return &LitExpr{Value: toSwiftType(t) + "()", IsString: false}
	}
}

func fetchExprOnly(e *parser.Expr) *parser.FetchExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return nil
	}
	if u.Value == nil || u.Value.Target == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Fetch
}

func isTypeStmt(st Stmt) bool {
	switch s := st.(type) {
	case *StructDef, *UnionDef:
		return true
	case *BlockStmt:
		if len(s.Stmts) > 0 {
			switch s.Stmts[0].(type) {
			case *StructDef, *UnionDef:
				return true
			}
		}
	}
	return false
}

func isEmptyMapLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	if ml := e.Binary.Left.Value.Target.Map; ml != nil {
		return len(ml.Items) == 0
	}
	return false
}

func isNilExpr(e Expr) bool {
	switch x := e.(type) {
	case *LitExpr:
		return !x.IsString && x.Value == "nil"
	case *CondExpr:
		return isNilExpr(x.Then) && isNilExpr(x.Else)
	default:
		return false
	}
}

func isMapLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	return e.Binary.Left.Value.Target.Map != nil
}

func isNullLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if l := p.Target.Lit; l != nil {
		return l.Null
	}
	return false
}

func isFuncTypeRef(t *parser.TypeRef) bool {
	return t != nil && t.Fun != nil
}

func toSwiftTypeSelf(t *parser.TypeRef, self string, env *types.Env) string {
	if t != nil && t.Simple != nil && *t.Simple == self {
		return "(@escaping (Any?) -> Any?) -> (Any?) -> Any?"
	}
	return toSwiftType(t)
}

func isFuncType(t types.Type) bool {
	_, ok := t.(types.FuncType)
	return ok
}

func toSwiftType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = toSwiftType(p)
		}
		ret := "Any"
		if t.Fun.Return != nil {
			ret = toSwiftType(t.Fun.Return)
		}
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			return "[" + toSwiftType(t.Generic.Args[0]) + "]"
		case "map":
			key := toSwiftType(t.Generic.Args[0])
			if key == "Any?" || key == "Any" {
				key = "AnyHashable"
			}
			return "[" + key + ": " + toSwiftType(t.Generic.Args[1]) + "]"
		}
	}
	if t.Simple == nil {
		return "Any"
	}
	switch *t.Simple {
	case "int":
		return "Int"
	case "float":
		return "Double"
	case "bigrat":
		return "Double"
	case "string":
		return "String"
	case "bool":
		return "Bool"
	case "bigint":
		usesBigInt = true
		return "BigInt"
	case "any":
		return "Any?"
	default:
		return *t.Simple
	}
}

func toSwiftOptionalType(t *parser.TypeRef) string {
	if t == nil {
		return "Any?"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = toSwiftType(p)
		}
		ret := "Any"
		if t.Fun.Return != nil {
			ret = toSwiftType(t.Fun.Return)
		}
		return "(" + strings.Join(params, ", ") + ") -> " + ret + "?"
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			return "[" + toSwiftType(t.Generic.Args[0]) + "]?"
		case "map":
			return "[" + toSwiftType(t.Generic.Args[0]) + ": " + toSwiftType(t.Generic.Args[1]) + "]?"
		}
	}
	if t.Simple == nil {
		return "Any?"
	}
	switch *t.Simple {
	case "int":
		return "Int?"
	case "float":
		return "Double?"
	case "bigrat":
		return "Double?"
	case "string":
		return "String?"
	case "bool":
		return "Bool?"
	case "bigint":
		usesBigInt = true
		return "BigInt?"
	case "any":
		return "Any?"
	default:
		return *t.Simple + "?"
	}
}

func swiftTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			if isFuncType(p) {
				params[i] = "@escaping " + swiftTypeOf(p)
			} else {
				params[i] = swiftTypeOf(p)
			}
		}
		ret := "Any"
		if tt.Return != nil {
			ret = swiftTypeOf(tt.Return)
		}
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	case types.IntType:
		return "Int"
	case types.Int64Type:
		return "Int64"
	case types.BigIntType:
		usesBigInt = true
		return "BigInt"
	case types.FloatType, types.BigRatType:
		return "Double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Bool"
	case types.AnyType:
		return "Any?"
	case types.ListType:
		return "[" + swiftTypeOf(tt.Elem) + "]"
	case types.MapType:
		key := swiftTypeOf(tt.Key)
		if key == "Any?" || key == "Any" {
			key = "AnyHashable"
		}
		return "[" + key + ": " + swiftTypeOf(tt.Value) + "]"
	case types.OptionType:
		return swiftTypeOf(tt.Elem) + "?"
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
		return "Any"
	case types.UnionType:
		if tt.Name != "" {
			return tt.Name
		}
		return "Any"
	default:
		return "Any"
	}
}

func parserTypeRefFromType(t types.Type) *parser.TypeRef {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		s := "int"
		return &parser.TypeRef{Simple: &s}
	case types.FloatType, types.BigRatType:
		s := "float"
		return &parser.TypeRef{Simple: &s}
	case types.BoolType:
		s := "bool"
		return &parser.TypeRef{Simple: &s}
	case types.StringType:
		s := "string"
		return &parser.TypeRef{Simple: &s}
	case types.ListType:
		el := parserTypeRefFromType(tt.Elem)
		if el == nil {
			return nil
		}
		return &parser.TypeRef{Generic: &parser.GenericType{Name: "list", Args: []*parser.TypeRef{el}}}
	case types.MapType:
		k := parserTypeRefFromType(tt.Key)
		v := parserTypeRefFromType(tt.Value)
		if k == nil || v == nil {
			return nil
		}
		return &parser.TypeRef{Generic: &parser.GenericType{Name: "map", Args: []*parser.TypeRef{k, v}}}
	case types.StructType:
		if tt.Name != "" {
			s := tt.Name
			return &parser.TypeRef{Simple: &s}
		}
	}
	return nil
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

func identName(e *parser.Expr) (string, bool) {
	return isSimpleIdent(e)
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	ml := e.Binary.Left.Value.Target.Map
	if ml == nil {
		return ""
	}
	for _, it := range ml.Items {
		key, ok := literalString(it.Key)
		if !ok || key != "format" {
			continue
		}
		if v, ok := literalString(it.Value); ok {
			return v
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

func valueToExpr(env *types.Env, v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		if typ != nil && typ.Simple != nil && env != nil {
			if st, ok := env.GetStruct(*typ.Simple); ok {
				var fields []FieldInit
				for _, name := range st.Order {
					ft := parserTypeRefFromType(st.Fields[name])
					fields = append(fields, FieldInit{Name: name, Value: valueToExpr(env, val[name], ft)})
				}
				return &StructInit{Name: st.Name, Fields: fields}
			}
		}
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		keys := make([]Expr, len(names))
		values := make([]Expr, len(names))
		for i, k := range names {
			keys[i] = &LitExpr{Value: k, IsString: true}
			values[i] = valueToExpr(env, val[k], nil)
		}
		return &MapLit{Keys: keys, Values: values}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(env, it, typ)
		}
		return &ArrayLit{Elems: elems}
	case string:
		return &LitExpr{Value: val, IsString: true}
	case bool:
		if val {
			return &LitExpr{Value: "true"}
		}
		return &LitExpr{Value: "false"}
	case int, int64:
		return &LitExpr{Value: fmt.Sprintf("%v", val)}
	case float64, float32:
		f := reflect.ValueOf(val).Float()
		s := strconv.FormatFloat(f, 'f', -1, 64)
		if math.Trunc(f) == f && !strings.Contains(s, ".") {
			s += ".0"
		}
		return &LitExpr{Value: s}
	default:
		return &LitExpr{Value: fmt.Sprintf("%v", val), IsString: true}
	}
}

func dataExprFromFile(env *types.Env, path, format string, typ *parser.TypeRef) (Expr, error) {
	if path == "" {
		return &ArrayLit{}, nil
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
	case "yaml", "":
		if err := yaml.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
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
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(env, v, typ), nil
}

// TestIntConst is a helper for debugging
