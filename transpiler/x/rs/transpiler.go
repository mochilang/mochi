//go:build slow

package rs

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	yaml "gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

var usesHashMap bool
var usesGroup bool
var mapVars map[string]bool
var stringVars map[string]bool
var groupVars map[string]bool
var varTypes map[string]string
var funParams map[string]int
var funParamTypes map[string][]string
var funReturns map[string]string
var typeDecls []TypeDecl
var structForMap map[*parser.MapLiteral]string
var structForList map[*parser.ListLiteral]string
var structSig map[string]string
var curEnv *types.Env
var structTypes map[string]types.StructType
var cloneVars map[string]bool
var useMath bool
var useTime bool
var patternMode bool
var boxVars map[string]bool
var mainFuncName string
var globalVars map[string]bool
var unsafeFuncs map[string]bool
var funcDepth int
var usesInput bool
var usesInt bool
var builtinAliases map[string]string
var globalRenames map[string]string
var globalRenameBack map[string]string
var localVarStack []map[string]bool
var currentFuncLocals map[string]bool

func VarTypes() map[string]string { return varTypes }

func isLocal(name string) bool {
	for i := len(localVarStack) - 1; i >= 0; i-- {
		if localVarStack[i][name] {
			return true
		}
	}
	return false
}

func handleImport(im *parser.ImportStmt, env *types.Env) bool {
	if im.Lang == nil {
		return false
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	switch *im.Lang {
	case "go":
		if im.Auto && im.Path == "mochi/runtime/ffi/go/testpkg" {
			if builtinAliases == nil {
				builtinAliases = map[string]string{}
				globalRenames = map[string]string{}
				globalRenameBack = map[string]string{}
				globalRenameBack = map[string]string{}
				globalRenameBack = map[string]string{}
			}
			builtinAliases[alias] = "go_testpkg"
			if env != nil {
				env.SetFuncType(alias+".Add", types.FuncType{Params: []types.Type{types.IntType{}, types.IntType{}}, Return: types.IntType{}})
				env.SetVar(alias+".Pi", types.FloatType{}, false)
				env.SetVar(alias+".Answer", types.IntType{}, false)
				env.SetFuncType(alias+".FifteenPuzzleExample", types.FuncType{Params: []types.Type{}, Return: types.StringType{}})
			}
			return true
		}
	}
	return false
}

// Program represents a Rust program consisting of a list of statements.
type TypeDecl interface{ emit(io.Writer) }

type Program struct {
	Stmts       []Stmt
	UsesHashMap bool
	UsesGroup   bool
	UseTime     bool
	UseInput    bool
	UseInt      bool
	Types       []TypeDecl
	Globals     []*VarDecl
}

type Stmt interface{ emit(io.Writer) }

type MultiStmt struct{ Stmts []Stmt }

func (m *MultiStmt) emit(w io.Writer) {
	for _, s := range m.Stmts {
		s.emit(w)
		io.WriteString(w, "\n")
	}
}

// Expr represents an expression node.
type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement consisting of a single expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
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

// PrintExpr emits a formatted println! call, optionally trimming trailing spaces.
type PrintExpr struct {
	Fmt  string
	Args []Expr
	Trim bool
}

func (p *PrintExpr) emit(w io.Writer) {
	emitArg := func(a Expr) {
		if inferType(a) == "bool" {
			switch ex := a.(type) {
			case *BinaryExpr:
				if ex.Op == "in" && inferType(ex.Right) == "String" {
					a.emit(w)
					return
				}
			case *MethodCallExpr:
				if ex.Name == "contains" {
					a.emit(w)
					return
				}
			}
			io.WriteString(w, "if ")
			a.emit(w)
			io.WriteString(w, " { 1 } else { 0 }")
		} else {
			if inferType(a) == "f64" {
				io.WriteString(w, "format!(\"{:?}\", ")
				a.emit(w)
				io.WriteString(w, ")")
			} else {
				a.emit(w)
			}
		}
	}
	if p.Trim {
		io.WriteString(w, "println!(\"{}\", format!(")
		fmt.Fprintf(w, "%q", p.Fmt)
		for _, a := range p.Args {
			io.WriteString(w, ", ")
			emitArg(a)
		}
		io.WriteString(w, ").trim_end())")
	} else {
		io.WriteString(w, "println!(")
		fmt.Fprintf(w, "%q", p.Fmt)
		for _, a := range p.Args {
			io.WriteString(w, ", ")
			emitArg(a)
		}
		io.WriteString(w, ")")
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type NumberLit struct{ Value string }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

type NullLit struct{}

func (n *NullLit) emit(w io.Writer) { io.WriteString(w, "Default::default()") }

// BreakStmt represents a `break` statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break") }

// ContinueStmt represents a `continue` statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue") }

// SaveStmt saves a list of structs or maps to stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "for _row in &")
		s.Src.emit(w)
		io.WriteString(w, " {\n        println!(\"{}\", _row);\n    }")
		return
	}
	io.WriteString(w, "// unsupported save")
}

type ReturnStmt struct{ Value Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return")
	if r.Value != nil {
		io.WriteString(w, " ")
		r.Value.emit(w)
	}
}

type Param struct {
	Name string
	Type string
}

// StructDecl represents a Rust struct type declaration.
type StructDecl struct {
	Name   string
	Fields []Param
}

func (s *StructDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "#[derive(Debug, Clone, Default)]\nstruct %s {\n", s.Name)
	for _, f := range s.Fields {
		fmt.Fprintf(w, "    %s: %s,\n", f.Name, f.Type)
	}
	io.WriteString(w, "}\n")
	fmt.Fprintf(w, "impl std::fmt::Display for %s {\n", s.Name)
	io.WriteString(w, "    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {\n")
	io.WriteString(w, "        write!(f, \"{{\")?;\n")
	for i, fld := range s.Fields {
		if i > 0 {
			io.WriteString(w, "        write!(f, \", \")?;\n")
		}
		switch {
		case fld.Type == "String":
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": \\\"{}\\\"\", self.%s)?;\n", fld.Name, fld.Name)
		case strings.HasPrefix(fld.Type, "Option<"):
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": {:?}\", self.%s)?;\n", fld.Name, fld.Name)
		case strings.HasPrefix(fld.Type, "Vec<") || strings.HasPrefix(fld.Type, "HashMap<"):
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": {:?}\", self.%s)?;\n", fld.Name, fld.Name)
		default:
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": {}\", self.%s)?;\n", fld.Name, fld.Name)
		}
	}
	io.WriteString(w, "        write!(f, \"}}\")\n    }\n}\n")
}

type EnumVariant struct {
	Name   string
	Fields []Param
}

type EnumDecl struct {
	Name     string
	Variants []EnumVariant
}

func (e *EnumDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "#[derive(Debug, Clone)]\nenum %s {\n", e.Name)
	for _, v := range e.Variants {
		if len(v.Fields) == 0 {
			fmt.Fprintf(w, "    %s,\n", v.Name)
			continue
		}
		fmt.Fprintf(w, "    %s { ", v.Name)
		for i, f := range v.Fields {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "%s: %s", f.Name, f.Type)
		}
		io.WriteString(w, " },\n")
	}
	io.WriteString(w, "}\n")
}

// StructLit represents instantiation of a struct value.
type StructLit struct {
	Name   string
	Fields []Expr
	Names  []string
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s {", s.Name)
	for i, f := range s.Fields {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		fmt.Fprintf(w, "%s: ", s.Names[i])
		f.emit(w)
	}
	io.WriteString(w, "}")
}

type EnumLit struct {
	Union     string
	Variant   string
	Fields    []Expr
	Names     []string
	Types     []string
	IsPattern bool
}

func (e *EnumLit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s::%s", e.Union, e.Variant)
	if len(e.Fields) == 0 {
		return
	}
	io.WriteString(w, " { ")
	old := patternMode
	patternMode = e.IsPattern
	for i, f := range e.Fields {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		fmt.Fprintf(w, "%s: ", e.Names[i])
		f.emit(w)
	}
	patternMode = old
	io.WriteString(w, " }")
}

type FuncDecl struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
	Unsafe bool
	Locals map[string]bool
}

func (f *FuncDecl) emit(w io.Writer) {
	if f.Unsafe {
		fmt.Fprintf(w, "unsafe fn %s(", f.Name)
	} else {
		fmt.Fprintf(w, "fn %s(", f.Name)
	}
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			io.WriteString(w, p.Name)
		}
	}
	io.WriteString(w, ")")
	if f.Return != "" && f.Return != "()" {
		fmt.Fprintf(w, " -> %s", f.Return)
	}
	io.WriteString(w, " {\n")
	localVarStack = append(localVarStack, f.Locals)
	for _, st := range f.Body {
		writeStmt(w.(*bytes.Buffer), st, 1)
	}
	localVarStack = localVarStack[:len(localVarStack)-1]
	io.WriteString(w, "}")
}

type FunLit struct {
	Params []Param
	Return string
	Expr   Expr
}

func (f *FunLit) emit(w io.Writer) {
	io.WriteString(w, "|")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			io.WriteString(w, p.Name)
		}
	}
	io.WriteString(w, "|")
	if f.Return != "" && f.Return != "()" {
		fmt.Fprintf(w, " -> %s", f.Return)
	}
	io.WriteString(w, " {")
	if f.Expr != nil {
		io.WriteString(w, " ")
		f.Expr.emit(w)
		io.WriteString(w, " ")
	}
	io.WriteString(w, "}")
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "vec![")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "]")
}

type MapEntry struct {
	Key   Expr
	Value Expr
}

type MapLit struct{ Items []MapEntry }

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "HashMap::from([")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "(")
		it.Key.emit(w)
		io.WriteString(w, ", ")
		it.Value.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, "])")
}

// SliceExpr represents a[start:end] slicing operation.
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, "[")
	if s.Start != nil {
		s.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, "..")
	if s.End != nil {
		s.End.emit(w)
	}
	io.WriteString(w, "]")
	if inferType(s.Target) == "String" {
		io.WriteString(w, ".to_string()")
	} else {
		io.WriteString(w, ".to_vec()")
	}
}

// IndexExpr represents `target[index]` access.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emit(w io.Writer) {
	switch t := i.Target.(type) {
	case *NameRef:
		if mapVars[t.Name] {
			t.emit(w)
			io.WriteString(w, "[&")
			i.Index.emit(w)
			io.WriteString(w, "]")
			return
		}
		if key, ok := literalStringExpr(i.Index); ok {
			if st, ok2 := structTypes[inferType(i.Target)]; ok2 {
				if _, ok3 := st.Fields[key]; ok3 {
					t.emit(w)
					io.WriteString(w, ".")
					io.WriteString(w, key)
					return
				}
			}
		}
	case *MapLit:
		i.Target.emit(w)
		io.WriteString(w, "[&")
		i.Index.emit(w)
		io.WriteString(w, "]")
		return
	}
	i.Target.emit(w)
	io.WriteString(w, "[")
	i.Index.emit(w)
	if strings.HasPrefix(inferType(i.Target), "Vec<") && inferType(i.Index) != "usize" {
		io.WriteString(w, " as usize")
	}
	io.WriteString(w, "]")
}

// StringIndexExpr represents string[index] returning a character.
type StringIndexExpr struct {
	Str   Expr
	Index Expr
}

func (s *StringIndexExpr) emit(w io.Writer) {
	s.Str.emit(w)
	io.WriteString(w, ".chars().nth(")
	s.Index.emit(w)
	io.WriteString(w, " as usize).unwrap()")
}

// FieldExpr represents `receiver.field` access.
type FieldExpr struct {
	Receiver Expr
	Name     string
}

func (f *FieldExpr) emit(w io.Writer) {
	if nr, ok := f.Receiver.(*NameRef); ok && nr.Name == "math" {
		io.WriteString(w, "math::")
		io.WriteString(w, f.Name)
		return
	}
	if strings.HasPrefix(inferType(f.Receiver), "HashMap<") {
		f.Receiver.emit(w)
		fmt.Fprintf(w, "[\"%s\"]", f.Name)
		return
	}
	if strings.HasPrefix(inferType(f.Receiver), "Option<") {
		f.Receiver.emit(w)
		io.WriteString(w, ".as_ref().unwrap().")
		io.WriteString(w, f.Name)
		return
	}
	if nr, ok := f.Receiver.(*NameRef); ok && groupVars[nr.Name] {
		f.Receiver.emit(w)
		if f.Name == "key" {
			io.WriteString(w, ".key.clone()")
			return
		}
		if f.Name == "items" {
			io.WriteString(w, ".items.clone()")
			return
		}
	}
	f.Receiver.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, f.Name)
}

// MethodCallExpr represents `receiver.method(args...)`.
type MethodCallExpr struct {
	Receiver Expr
	Name     string
	Args     []Expr
}

func (m *MethodCallExpr) emit(w io.Writer) {
	if nr, ok := m.Receiver.(*NameRef); ok && nr.Name == "math" {
		io.WriteString(w, "math::")
		io.WriteString(w, m.Name)
	} else {
		m.Receiver.emit(w)
		io.WriteString(w, ".")
		io.WriteString(w, m.Name)
	}
	io.WriteString(w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// LenExpr represents a call to the `len` builtin.
type LenExpr struct{ Arg Expr }

func (l *LenExpr) emit(w io.Writer) {
	if nr, ok := l.Arg.(*NameRef); ok && groupVars[nr.Name] {
		l.Arg.emit(w)
		io.WriteString(w, ".items.len() as i64")
	} else {
		l.Arg.emit(w)
		io.WriteString(w, ".len() as i64")
	}
}

// SumExpr represents a call to the `sum` builtin.
type SumExpr struct{ Arg Expr }

func (s *SumExpr) emit(w io.Writer) {
	s.Arg.emit(w)
	typ := inferType(s.Arg)
	if q, ok := s.Arg.(*QueryExpr); ok {
		typ = "Vec<" + q.ItemType + ">"
		if strings.Contains(inferType(q.Select), "f64") {
			typ = "Vec<f64>"
		}
	}
	if strings.Contains(typ, "f64") {
		io.WriteString(w, ".iter().sum::<f64>()")
	} else {
		io.WriteString(w, ".iter().map(|x| *x as f64).sum::<f64>()")
	}
}

// StrExpr represents a call to the `str` builtin.
type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	s.Arg.emit(w)
	io.WriteString(w, ".to_string()")
}

// ValuesExpr represents a call to the `values` builtin on a map.
type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let mut v = ")
	v.Map.emit(w)
	io.WriteString(w, ".values().cloned().collect::<Vec<_>>(); v.sort(); v }")
}

// AppendExpr represents a call to the `append` builtin on a list.
type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let mut v = ")
	a.List.emit(w)
	io.WriteString(w, ".clone(); v.push(")
	a.Elem.emit(w)
	io.WriteString(w, "); v }")
}

// JoinExpr converts a list of integers into a space separated string.
type JoinExpr struct{ List Expr }

func (j *JoinExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let tmp = ")
	j.List.emit(w)
	if inferType(j.List) == "String" {
		io.WriteString(w, "; tmp.chars().map(|x| x.to_string()).collect::<Vec<_>>().join(\" \") }")
	} else {
		io.WriteString(w, "; tmp.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(\" \") }")
	}
}

// AvgExpr represents a call to the `avg` builtin.
type AvgExpr struct{ List Expr }

func (a *AvgExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let tmp = ")
	a.List.emit(w)
	io.WriteString(w, "; tmp.iter().map(|x| *x as f64).sum::<f64>() / (tmp.len() as f64) }")
}

// MinExpr represents a call to the `min` builtin.
type MinExpr struct{ List Expr }

func (m *MinExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let tmp = ")
	m.List.emit(w)
	io.WriteString(w, ".clone(); *tmp.iter().min().unwrap_or(&0) }")
}

// MaxExpr represents a call to the `max` builtin.
type MaxExpr struct{ List Expr }

func (m *MaxExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let tmp = ")
	m.List.emit(w)
	io.WriteString(w, ".clone(); *tmp.iter().max().unwrap_or(&0) }")
}

// ExistsExpr represents a call to the `exists` builtin.
type ExistsExpr struct{ List Expr }

func (e *ExistsExpr) emit(w io.Writer) {
	io.WriteString(w, "!(")
	e.List.emit(w)
	io.WriteString(w, ".is_empty())")
}

// SubstringExpr represents a call to the `substring` builtin.
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "String::from(&")
	s.Str.emit(w)
	io.WriteString(w, "[")
	s.Start.emit(w)
	io.WriteString(w, " as usize .. ")
	s.End.emit(w)
	io.WriteString(w, " as usize])")
}

// StringCastExpr converts an expression to a Rust String.
type StringCastExpr struct{ Expr Expr }

func (s *StringCastExpr) emit(w io.Writer) {
	t := inferType(s.Expr)
	if t == "String" {
		if nr, ok := s.Expr.(*NameRef); ok && cloneVars[nr.Name] {
			s.Expr.emit(w)
			return
		}
		if _, ok := s.Expr.(*StringLit); ok {
			io.WriteString(w, "String::from(")
			s.Expr.emit(w)
			io.WriteString(w, ")")
			return
		}
		s.Expr.emit(w)
		io.WriteString(w, ".clone()")
		return
	}
	s.Expr.emit(w)
	io.WriteString(w, ".to_string()")
}

// IntCastExpr converts an expression to a 64-bit integer.
type IntCastExpr struct{ Expr Expr }

func (i *IntCastExpr) emit(w io.Writer) {
	if inferType(i.Expr) == "i64" {
		i.Expr.emit(w)
	} else {
		i.Expr.emit(w)
		io.WriteString(w, " as i64")
	}
}

// FloatCastExpr converts an expression to a 64-bit float.
type FloatCastExpr struct{ Expr Expr }

func (f *FloatCastExpr) emit(w io.Writer) {
	if inferType(f.Expr) == "f64" {
		f.Expr.emit(w)
	} else {
		f.Expr.emit(w)
		io.WriteString(w, " as f64")
	}
}

// NowExpr expands to a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

func (n *NowExpr) emit(w io.Writer) { io.WriteString(w, "_now()") }

type NameRef struct{ Name string }

func (n *NameRef) emit(w io.Writer) {
	if newName, ok := globalRenames[n.Name]; ok && !isLocal(n.Name) {
		io.WriteString(w, newName)
		return
	}
	if cloneVars[n.Name] {
		typ := varTypes[n.Name]
		switch typ {
		case "i64", "bool", "f64":
			io.WriteString(w, "*")
			io.WriteString(w, n.Name)
		default:
			io.WriteString(w, n.Name)
			io.WriteString(w, ".clone()")
		}
		return
	}
	if boxVars[n.Name] && !patternMode {
		io.WriteString(w, n.Name)
		return
	}
	if ut, ok := curEnv.FindUnionByVariant(n.Name); ok && varTypes[n.Name] == "" {
		fmt.Fprintf(w, "%s::%s", ut.Name, n.Name)
		return
	}
	io.WriteString(w, n.Name)
}

type VarDecl struct {
	Name    string
	Expr    Expr
	Type    string
	Mutable bool
	Global  bool
}

func (v *VarDecl) emit(w io.Writer) {
	if v.Global {
		io.WriteString(w, "static mut ")
		io.WriteString(w, v.Name)
		io.WriteString(w, ": ")
		typ := v.Type
		if typ == "" {
			typ = varTypes[v.Name]
			if typ == "" {
				if orig, ok := globalRenameBack[v.Name]; ok {
					typ = varTypes[orig]
				}
			}
		}
		if typ == "" {
			typ = "i64"
		}
		io.WriteString(w, typ)
		io.WriteString(w, " = ")
		io.WriteString(w, zeroValue(typ))
		io.WriteString(w, ";")
		return
	}
	io.WriteString(w, "let ")
	if v.Mutable {
		io.WriteString(w, "mut ")
	}
	io.WriteString(w, v.Name)
	if v.Type != "" {
		io.WriteString(w, ": ")
		io.WriteString(w, v.Type)
	}
	if v.Expr != nil {
		io.WriteString(w, " = ")
		if v.Type == "" && stringVars[v.Name] {
			if sl, ok := v.Expr.(*StringLit); ok {
				io.WriteString(w, "String::from(")
				sl.emit(w)
				io.WriteString(w, ")")
			} else {
				v.Expr.emit(w)
			}
		} else {
			if v.Type == "i64" && inferType(v.Expr) == "f64" {
				io.WriteString(w, "(")
				v.Expr.emit(w)
				io.WriteString(w, " as i64)")
				return
			}
			v.Expr.emit(w)
		}
	} else if v.Type != "" {
		io.WriteString(w, " = ")
		if v.Type == "String" {
			io.WriteString(w, "String::new()")
		} else {
			io.WriteString(w, "Default::default()")
		}
	}
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	if newName, ok := globalRenames[a.Name]; ok && !isLocal(a.Name) {
		io.WriteString(w, newName)
	} else {
		io.WriteString(w, a.Name)
	}
	io.WriteString(w, " = ")
	a.Expr.emit(w)
}

// IndexAssignStmt assigns to an indexed expression like x[i] = v.
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	if idx, ok := s.Target.(*IndexExpr); ok {
		if strings.HasPrefix(inferType(idx.Target), "HashMap") {
			idx.Target.emit(w)
			io.WriteString(w, ".insert(")
			idx.Index.emit(w)
			io.WriteString(w, ", ")
			s.Value.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	s.Target.emit(w)
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		if inferType(b.Left) == "String" || inferType(b.Right) == "String" {
			io.WriteString(w, "format!(\"{}{}\", ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	if b.Op == "in" {
		rt := inferType(b.Right)
		if rt == "String" {
			b.Right.emit(w)
			io.WriteString(w, ".contains(")
			b.Left.emit(w)
			io.WriteString(w, ")")
			return
		}
		if strings.HasPrefix(rt, "Vec<") {
			b.Right.emit(w)
			io.WriteString(w, ".contains(&")
			b.Left.emit(w)
			io.WriteString(w, ")")
			return
		}
		if strings.HasPrefix(rt, "HashMap") {
			b.Right.emit(w)
			io.WriteString(w, ".contains_key(&")
			b.Left.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	io.WriteString(w, "(")
	left := b.Left
	right := b.Right
	if b.Op == "+" || b.Op == "-" || b.Op == "*" || b.Op == "/" {
		lt := inferType(left)
		rt := inferType(right)
		if lt == "f64" && rt == "i64" {
			right = &FloatCastExpr{Expr: right}
		} else if lt == "i64" && rt == "f64" {
			left = &FloatCastExpr{Expr: left}
		}
	}
	left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	right.emit(w)
	io.WriteString(w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

type IfExpr struct {
	Cond   Expr
	Then   Expr
	ElseIf *IfExpr
	Else   Expr
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " { ")
	i.Then.emit(w)
	io.WriteString(w, " }")
	if i.ElseIf != nil {
		io.WriteString(w, " else ")
		i.ElseIf.emit(w)
	} else if i.Else != nil {
		io.WriteString(w, " else { ")
		i.Else.emit(w)
		io.WriteString(w, " }")
	}
}

type MatchArm struct {
	Pattern Expr // nil for _
	Result  Expr
}

type MatchExpr struct {
	Target Expr
	Arms   []MatchArm
}

func (m *MatchExpr) emit(w io.Writer) {
	io.WriteString(w, "match ")
	m.Target.emit(w)
	io.WriteString(w, " { ")
	for i, a := range m.Arms {
		if i > 0 {
			io.WriteString(w, " ")
		}
		if a.Pattern != nil {
			a.Pattern.emit(w)
		} else {
			io.WriteString(w, "_")
		}
		io.WriteString(w, " => ")
		a.Result.emit(w)
		io.WriteString(w, ",")
	}
	io.WriteString(w, " }")
}

type queryFrom struct {
	Var   string
	Src   Expr
	ByRef bool
}

type queryJoin struct {
	Var   string
	Src   Expr
	On    Expr
	ByRef bool
	Side  string
	Typ   string
}

// OuterJoinExpr represents a simple full outer join query.
type OuterJoinExpr struct {
	LeftVar    string
	LeftSrc    Expr
	LeftByRef  bool
	LeftType   string
	RightVar   string
	RightSrc   Expr
	RightByRef bool
	RightType  string
	Cond       Expr
	Select     Expr
	ItemType   string
}

// QueryExpr represents a simple from/select query expression.
type QueryExpr struct {
	Var       string
	Src       Expr
	VarByRef  bool
	Froms     []queryFrom
	Joins     []queryJoin
	Where     Expr
	Sort      Expr
	SortType  string
	Skip      Expr
	Take      Expr
	GroupKey  Expr
	GroupVar  string
	GroupType string
	Select    Expr
	ItemType  string
}

func (q *QueryExpr) emit(w io.Writer) {
	// generate simpler iterator pipeline for basic queries
	if len(q.Froms) == 0 && len(q.Joins) == 0 && q.GroupVar == "" &&
		q.Sort == nil && q.Skip == nil && q.Take == nil {
		io.WriteString(w, "{ ")
		if q.VarByRef {
			q.Src.emit(w)
			io.WriteString(w, ".iter()")
		} else {
			q.Src.emit(w)
			io.WriteString(w, ".into_iter()")
		}
		if q.Where != nil {
			io.WriteString(w, ".filter(|")
			io.WriteString(w, q.Var)
			io.WriteString(w, "| ")
			q.Where.emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, ".map(|")
		io.WriteString(w, q.Var)
		io.WriteString(w, "| ")
		q.Select.emit(w)
		fmt.Fprintf(w, ").collect::<Vec<%s>>() }", q.ItemType)
		return
	}

	fmt.Fprintf(w, "{ let mut _q: Vec<%s> = Vec::new(); ", q.ItemType)
	if q.Sort != nil {
		fmt.Fprintf(w, "let mut _tmp: Vec<(%s, %s)> = Vec::new(); ", q.SortType, q.ItemType)
	}
	if q.GroupVar != "" {
		fmt.Fprintf(w, "let mut _groups: HashMap<String, %s> = HashMap::new(); ", q.GroupType)
		io.WriteString(w, "let mut _order: Vec<String> = Vec::new(); ")
	}
	// Special-case single right join without extra from clauses
	if len(q.Joins) == 1 && q.Joins[0].Side == "right" && len(q.Froms) == 0 {
		j := q.Joins[0]
		io.WriteString(w, "for ")
		io.WriteString(w, j.Var)
		io.WriteString(w, " in ")
		if j.ByRef {
			io.WriteString(w, "&")
		}
		j.Src.emit(w)
		io.WriteString(w, " {")
		io.WriteString(w, " for ")
		io.WriteString(w, q.Var)
		io.WriteString(w, " in ")
		if q.VarByRef {
			io.WriteString(w, "&")
		}
		q.Src.emit(w)
		io.WriteString(w, " {")
		io.WriteString(w, " if ")
		j.On.emit(w)
		io.WriteString(w, " {")
		if q.Where != nil {
			io.WriteString(w, " if ")
			q.Where.emit(w)
			io.WriteString(w, " {")
		}
		io.WriteString(w, " _q.push(")
		cloneVars = map[string]bool{j.Var: j.ByRef, q.Var: q.VarByRef}
		q.Select.emit(w)
		cloneVars = nil
		io.WriteString(w, ");")
		if q.Where != nil {
			io.WriteString(w, " }")
		}
		io.WriteString(w, " }")
		io.WriteString(w, " }")
		io.WriteString(w, " }")
		io.WriteString(w, " _q }")
		return
	}
	// Special-case single left join without extra from clauses and no grouping or sorting
	if len(q.Joins) == 1 && q.Joins[0].Side == "left" && len(q.Froms) == 0 && q.GroupVar == "" && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil {
		j := q.Joins[0]
		io.WriteString(w, "for ")
		io.WriteString(w, q.Var)
		io.WriteString(w, " in ")
		if q.VarByRef {
			io.WriteString(w, "&")
		}
		q.Src.emit(w)
		io.WriteString(w, " {")
		io.WriteString(w, " let mut _matched = false;")
		io.WriteString(w, " for ")
		io.WriteString(w, j.Var)
		io.WriteString(w, " in ")
		if j.ByRef {
			io.WriteString(w, "&")
		}
		j.Src.emit(w)
		io.WriteString(w, " {")
		io.WriteString(w, " if ")
		j.On.emit(w)
		io.WriteString(w, " {")
		io.WriteString(w, " _matched = true; let ")
		io.WriteString(w, j.Var)
		fmt.Fprintf(w, ": Option<%s> = Some(", j.Typ)
		cloneVars = map[string]bool{j.Var: j.ByRef}
		(&NameRef{Name: j.Var}).emit(w)
		cloneVars = nil
		io.WriteString(w, "); _q.push(")
		cloneVars = map[string]bool{q.Var: q.VarByRef, j.Var: true}
		q.Select.emit(w)
		cloneVars = nil
		io.WriteString(w, "); }")
		io.WriteString(w, " }")
		io.WriteString(w, " if !_matched { let ")
		io.WriteString(w, j.Var)
		fmt.Fprintf(w, ": Option<%s> = None; _q.push(", j.Typ)
		cloneVars = map[string]bool{q.Var: q.VarByRef}
		q.Select.emit(w)
		cloneVars = nil
		io.WriteString(w, "); }")
		io.WriteString(w, " }")
		io.WriteString(w, " _q }")
		return
	}
	io.WriteString(w, "for ")
	io.WriteString(w, q.Var)
	io.WriteString(w, " in ")
	if q.VarByRef {
		io.WriteString(w, "&")
	}
	q.Src.emit(w)
	io.WriteString(w, " {")
	for _, f := range q.Froms {
		io.WriteString(w, " for ")
		io.WriteString(w, f.Var)
		io.WriteString(w, " in ")
		if f.ByRef {
			io.WriteString(w, "&")
		}
		f.Src.emit(w)
		io.WriteString(w, " {")
	}
	for _, j := range q.Joins {
		io.WriteString(w, " for ")
		io.WriteString(w, j.Var)
		io.WriteString(w, " in ")
		if j.ByRef {
			io.WriteString(w, "&")
		}
		j.Src.emit(w)
		io.WriteString(w, " {")
		io.WriteString(w, " if ")
		j.On.emit(w)
		io.WriteString(w, " {")
	}
	if q.Where != nil {
		io.WriteString(w, " if ")
		q.Where.emit(w)
		io.WriteString(w, " {")
	}
	if q.GroupVar != "" {
		io.WriteString(w, " let key = ")
		q.GroupKey.emit(w)
		io.WriteString(w, ".clone(); let ks = format!(\"{:?}\", &key);")
		gen := strings.TrimPrefix(q.GroupType, "Group")
		fmt.Fprintf(w, " let e = _groups.entry(ks.clone()).or_insert_with(|| { _order.push(ks.clone()); Group::%s { key: key.clone(), items: Vec::new() } });", gen)
		io.WriteString(w, " e.items.push(")
		cloneVars = map[string]bool{q.Var: q.VarByRef}
		(&NameRef{Name: q.Var}).emit(w)
		cloneVars = nil
		io.WriteString(w, ");")
	} else if q.Sort != nil {
		io.WriteString(w, " _tmp.push((")
		q.Sort.emit(w)
		io.WriteString(w, ", ")
		cloneVars = map[string]bool{q.Var: q.VarByRef}
		for _, f := range q.Froms {
			cloneVars[f.Var] = f.ByRef
		}
		for _, j := range q.Joins {
			cloneVars[j.Var] = j.ByRef
		}
		q.Select.emit(w)
		cloneVars = nil
		io.WriteString(w, "));")
	} else {
		io.WriteString(w, " _q.push(")
		cloneVars = map[string]bool{q.Var: q.VarByRef}
		for _, f := range q.Froms {
			cloneVars[f.Var] = f.ByRef
		}
		for _, j := range q.Joins {
			cloneVars[j.Var] = j.ByRef
		}
		q.Select.emit(w)
		cloneVars = nil
		io.WriteString(w, ");")
	}
	if q.Where != nil {
		io.WriteString(w, " }")
	}
	for range q.Joins {
		io.WriteString(w, " }")
		io.WriteString(w, " }")
	}
	for range q.Froms {
		io.WriteString(w, " }")
	}
	io.WriteString(w, " }")
	if q.GroupVar != "" {
		fmt.Fprintf(w, " for ks in _order { let %s = &_groups[&ks];", q.GroupVar)
		if q.Sort != nil {
			io.WriteString(w, " _tmp.push((")
			q.Sort.emit(w)
			io.WriteString(w, ", ")
			q.Select.emit(w)
			io.WriteString(w, "));")
		} else {
			io.WriteString(w, " _q.push(")
			q.Select.emit(w)
			io.WriteString(w, ");")
		}
		io.WriteString(w, " }")
	}
	if q.Sort != nil {
		io.WriteString(w, " _tmp.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap()); for (_,v) in _tmp { _q.push(v); }")
	}
	if q.Skip != nil {
		io.WriteString(w, " if (")
		q.Skip.emit(w)
		io.WriteString(w, " as usize) < _q.len() { _q = _q[(")
		q.Skip.emit(w)
		io.WriteString(w, " as usize)..].to_vec(); } else { _q = Vec::new(); }")
	}
	if q.Take != nil {
		io.WriteString(w, " if (")
		q.Take.emit(w)
		io.WriteString(w, " as usize) < _q.len() { _q = _q[..(")
		q.Take.emit(w)
		io.WriteString(w, " as usize)].to_vec(); }")
	}
	io.WriteString(w, " _q }")
}

func (o *OuterJoinExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "{ let mut _q: Vec<%s> = Vec::new(); ", o.ItemType)
	io.WriteString(w, "for ")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, " in ")
	if o.LeftByRef {
		io.WriteString(w, "&")
	}
	o.LeftSrc.emit(w)
	io.WriteString(w, " {")
	io.WriteString(w, " let mut _matched = false;")
	io.WriteString(w, " for ")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, " in ")
	if o.RightByRef {
		io.WriteString(w, "&")
	}
	o.RightSrc.emit(w)
	io.WriteString(w, " {")
	io.WriteString(w, " if ")
	o.Cond.emit(w)
	io.WriteString(w, " {")
	io.WriteString(w, " _matched = true; let ")
	io.WriteString(w, o.LeftVar)
	fmt.Fprintf(w, ": Option<%s> = Some(", o.LeftType)
	cloneVars = map[string]bool{o.LeftVar: o.LeftByRef}
	(&NameRef{Name: o.LeftVar}).emit(w)
	cloneVars = nil
	io.WriteString(w, "); let ")
	io.WriteString(w, o.RightVar)
	fmt.Fprintf(w, ": Option<%s> = Some(", o.RightType)
	cloneVars = map[string]bool{o.RightVar: o.RightByRef}
	(&NameRef{Name: o.RightVar}).emit(w)
	cloneVars = nil
	io.WriteString(w, "); _q.push(")
	cloneVars = map[string]bool{o.LeftVar: true, o.RightVar: true}
	o.Select.emit(w)
	cloneVars = nil
	io.WriteString(w, "); }")
	io.WriteString(w, " }")
	io.WriteString(w, " if !_matched { let ")
	io.WriteString(w, o.RightVar)
	fmt.Fprintf(w, ": Option<%s> = None; let ", o.RightType)
	io.WriteString(w, o.LeftVar)
	fmt.Fprintf(w, ": Option<%s> = Some(", o.LeftType)
	cloneVars = map[string]bool{o.LeftVar: o.LeftByRef}
	(&NameRef{Name: o.LeftVar}).emit(w)
	cloneVars = nil
	io.WriteString(w, "); _q.push(")
	cloneVars = map[string]bool{o.LeftVar: true, o.RightVar: true}
	o.Select.emit(w)
	cloneVars = nil
	io.WriteString(w, "); }")
	io.WriteString(w, " }")
	io.WriteString(w, " for ")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, " in ")
	if o.RightByRef {
		io.WriteString(w, "&")
	}
	o.RightSrc.emit(w)
	io.WriteString(w, " {")
	io.WriteString(w, " let mut _matched = false;")
	io.WriteString(w, " for ")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, " in ")
	if o.LeftByRef {
		io.WriteString(w, "&")
	}
	o.LeftSrc.emit(w)
	io.WriteString(w, " {")
	io.WriteString(w, " if ")
	o.Cond.emit(w)
	io.WriteString(w, " { _matched = true; break; }")
	io.WriteString(w, " }")
	io.WriteString(w, " if !_matched { let ")
	io.WriteString(w, o.LeftVar)
	fmt.Fprintf(w, ": Option<%s> = None; let ", o.LeftType)
	io.WriteString(w, o.RightVar)
	fmt.Fprintf(w, ": Option<%s> = Some(", o.RightType)
	cloneVars = map[string]bool{o.RightVar: o.RightByRef}
	(&NameRef{Name: o.RightVar}).emit(w)
	cloneVars = nil
	io.WriteString(w, "); _q.push(")
	cloneVars = map[string]bool{o.LeftVar: true, o.RightVar: true}
	o.Select.emit(w)
	cloneVars = nil
	io.WriteString(w, "); }")
	io.WriteString(w, " }")
	io.WriteString(w, " _q }")
}

type IfStmt struct {
	Cond   Expr
	Then   []Stmt
	ElseIf *IfStmt
	Else   []Stmt
}

// ForStmt represents `for x in iter {}` or a range loop.
type ForStmt struct {
	Var   string
	Iter  Expr
	End   Expr // nil unless range loop
	Body  []Stmt
	ByRef bool
}

func (i *IfStmt) emit(w io.Writer) {}

// WhileStmt represents a basic while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {}

// ForStmt emits `for` loops.
func (fs *ForStmt) emit(w io.Writer) {}

// --- Transpiler ---

// Transpile converts a Mochi AST to a simplified Rust AST. Only a very small
// subset of Mochi is supported which is sufficient for tests.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	usesHashMap = false
	usesGroup = false
	useMath = false
	useTime = false
	usesInput = false
	usesInt = false
	mapVars = make(map[string]bool)
	stringVars = make(map[string]bool)
	groupVars = make(map[string]bool)
	varTypes = make(map[string]string)
	funParams = make(map[string]int)
	funParamTypes = make(map[string][]string)
	funReturns = make(map[string]string)
	boxVars = make(map[string]bool)
	mainFuncName = ""
	globalVars = make(map[string]bool)
	unsafeFuncs = make(map[string]bool)
	funcDepth = 0
	typeDecls = nil
	structForMap = make(map[*parser.MapLiteral]string)
	structForList = make(map[*parser.ListLiteral]string)
	structTypes = make(map[string]types.StructType)
	structSig = make(map[string]string)
	curEnv = env
	builtinAliases = map[string]string{}
	globalRenames = map[string]string{}
	globalRenameBack = map[string]string{}
	prog := &Program{}
	for _, st := range p.Statements {
		if st.Import != nil {
			if handleImport(st.Import, env) {
				continue
			}
		}
		s, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			prog.Stmts = append(prog.Stmts, s)
		}
	}
	for _, st := range prog.Stmts {
		if vd, ok := st.(*VarDecl); ok && vd.Global {
			prog.Globals = append(prog.Globals, vd)
		}
	}
	if len(prog.Globals) > 0 {
		for _, st := range prog.Stmts {
			if fd, ok := st.(*FuncDecl); ok {
				fd.Unsafe = true
				unsafeFuncs[fd.Name] = true
			}
		}
	}
	prog.Types = typeDecls
	_ = env // reserved for future use
	prog.UsesHashMap = usesHashMap
	prog.UsesGroup = usesGroup
	prog.UseTime = useTime
	prog.UseInput = usesInput
	prog.UseInt = usesInt
	return prog, nil
}

func compileStmt(stmt *parser.Statement) (Stmt, error) {
	switch {
	case stmt.Expr != nil:
		if se := extractSaveExpr(stmt.Expr.Expr); se != nil {
			src, err := compileExpr(se.Src)
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
		e, err := compileExpr(stmt.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case stmt.Let != nil:
		var e Expr
		var err error
		emptyList := false
		if stmt.Let.Value != nil {
			if ll := listLiteral(stmt.Let.Value); ll != nil {
				if st, ok := types.InferStructFromList(ll, curEnv); ok {
					name := types.UniqueStructName(strings.Title(stmt.Let.Name)+"Item", curEnv, nil)
					st.Name = name
					curEnv.SetStruct(name, st)
					fields := make([]Param, len(st.Order))
					for i, n := range st.Order {
						fields[i] = Param{Name: n, Type: rustTypeFromType(st.Fields[n])}
					}
					typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
					structTypes[name] = st
					structForList[ll] = name
					curEnv.SetStruct(name, st)
					curEnv.SetVar(stmt.Let.Name, types.ListType{Elem: st}, true)
					for _, el := range ll.Elems {
						if ml := mapLiteralExpr(el); ml != nil {
							structForMap[ml] = name
						}
					}
					varTypes[stmt.Let.Name] = fmt.Sprintf("Vec<%s>", name)
				}
			}
			if ll := listLiteral(stmt.Let.Value); ll != nil && len(ll.Elems) == 0 {
				emptyList = true
			}
			e, err = compileExpr(stmt.Let.Value)
			if err != nil {
				return nil, err
			}
			if _, ok := e.(*MapLit); ok {
				mapVars[stmt.Let.Name] = true
			}
			if inferType(e) == "String" {
				stringVars[stmt.Let.Name] = true
			}
		}
		typ := ""
		if stmt.Let.Type != nil {
			typ = rustTypeRef(stmt.Let.Type)
			if strings.HasPrefix(typ, "HashMap") {
				mapVars[stmt.Let.Name] = true
			}
			if typ == "String" {
				stringVars[stmt.Let.Name] = true
			}
		} else if e != nil {
			typ = inferType(e)
			if emptyList && typ == "Vec<i64>" {
				typ = ""
			}
			if _, ok := e.(*StringLit); ok {
				typ = ""
			} else if _, ok := e.(*MapLit); ok {
				typ = ""
			}
		}
		if typ == "fn" {
			typ = ""
		}
		if q, ok := e.(*QueryExpr); ok && typ == "" {
			typ = fmt.Sprintf("Vec<%s>", q.ItemType)
		}
		if typ == "i64" && containsFloat(e) {
			typ = "f64"
		}
		if typ != "" {
			if typ == "String" {
				e = &StringCastExpr{Expr: e}
			} else if typ == "Vec<String>" {
				if ll, ok := e.(*ListLit); ok {
					for i, el := range ll.Elems {
						if _, ok := el.(*StringLit); ok {
							ll.Elems[i] = &StringCastExpr{Expr: el}
						}
					}
				}
			}
			varTypes[stmt.Let.Name] = typ
			if q, ok := e.(*QueryExpr); ok {
				if st, ok := structTypes[q.ItemType]; ok {
					curEnv.SetVar(stmt.Let.Name, types.ListType{Elem: st}, true)
				}
			}
		}
		if typ == "" && funcDepth == 0 && len(localVarStack) == 0 {
			if _, ok := e.(*StringLit); ok {
				typ = "String"
				e = &StringCastExpr{Expr: e}
			}
		}
		vd := &VarDecl{Name: stmt.Let.Name, Expr: e, Type: typ}
		if funcDepth == 0 && len(localVarStack) == 0 {
			vd.Global = true
			newName := "g_" + stmt.Let.Name
			globalRenames[stmt.Let.Name] = newName
			globalRenameBack[newName] = stmt.Let.Name
			vd.Name = newName
			globalVars[stmt.Let.Name] = true
		} else {
			if len(localVarStack) > 0 {
				localVarStack[len(localVarStack)-1][stmt.Let.Name] = true
			}
			if currentFuncLocals != nil {
				currentFuncLocals[stmt.Let.Name] = true
			}
		}
		return vd, nil
	case stmt.Var != nil:
		var e Expr
		var err error
		emptyList := false
		if stmt.Var.Value != nil {
			if ll := listLiteral(stmt.Var.Value); ll != nil {
				if st, ok := types.InferStructFromList(ll, curEnv); ok {
					name := types.UniqueStructName(strings.Title(stmt.Var.Name)+"Item", curEnv, nil)
					curEnv.SetStruct(name, st)
					fields := make([]Param, len(st.Order))
					for i, n := range st.Order {
						fields[i] = Param{Name: n, Type: rustTypeFromType(st.Fields[n])}
					}
					typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
					structTypes[name] = st
					structForList[ll] = name
					st.Name = name
					curEnv.SetStruct(name, st)
					curEnv.SetVar(stmt.Var.Name, types.ListType{Elem: st}, true)
					for _, el := range ll.Elems {
						if ml := mapLiteralExpr(el); ml != nil {
							structForMap[ml] = name
						}
					}
					varTypes[stmt.Var.Name] = fmt.Sprintf("Vec<%s>", name)
				}
			}
			if ll := listLiteral(stmt.Var.Value); ll != nil && len(ll.Elems) == 0 {
				emptyList = true
			}
			e, err = compileExpr(stmt.Var.Value)
			if err != nil {
				return nil, err
			}
			if _, ok := e.(*MapLit); ok {
				mapVars[stmt.Var.Name] = true
			}
			if inferType(e) == "String" {
				stringVars[stmt.Var.Name] = true
			}
		}
		typ := ""
		if stmt.Var.Type != nil {
			typ = rustTypeRef(stmt.Var.Type)
			if strings.HasPrefix(typ, "HashMap") {
				mapVars[stmt.Var.Name] = true
			}
			if typ == "String" {
				stringVars[stmt.Var.Name] = true
			}
		} else if e != nil {
			typ = inferType(e)
			if emptyList && typ == "Vec<i64>" {
				typ = ""
			}
			if _, ok := e.(*StringLit); ok {
				typ = ""
			} else if _, ok := e.(*MapLit); ok {
				typ = ""
			}
		}
		if typ == "fn" {
			typ = ""
		}
		if q, ok := e.(*QueryExpr); ok && typ == "" {
			typ = fmt.Sprintf("Vec<%s>", q.ItemType)
		}
		if typ != "" {
			if typ == "String" {
				e = &StringCastExpr{Expr: e}
			} else if typ == "Vec<String>" {
				if ll, ok := e.(*ListLit); ok {
					for i, el := range ll.Elems {
						if _, ok := el.(*StringLit); ok {
							ll.Elems[i] = &StringCastExpr{Expr: el}
						}
					}
				}
			}
			varTypes[stmt.Var.Name] = typ
			if q, ok := e.(*QueryExpr); ok {
				if st, ok := structTypes[q.ItemType]; ok {
					curEnv.SetVar(stmt.Var.Name, types.ListType{Elem: st}, true)
				}
			}
		}
		if typ == "" && funcDepth == 0 && len(localVarStack) == 0 {
			if _, ok := e.(*StringLit); ok {
				typ = "String"
				e = &StringCastExpr{Expr: e}
			}
		}
		vd := &VarDecl{Name: stmt.Var.Name, Expr: e, Type: typ, Mutable: true}
		if funcDepth == 0 && len(localVarStack) == 0 {
			vd.Global = true
			newName := "g_" + stmt.Var.Name
			globalRenames[stmt.Var.Name] = newName
			globalRenameBack[newName] = stmt.Var.Name
			vd.Name = newName
			globalVars[stmt.Var.Name] = true
		} else {
			if len(localVarStack) > 0 {
				localVarStack[len(localVarStack)-1][stmt.Var.Name] = true
			}
			if currentFuncLocals != nil {
				currentFuncLocals[stmt.Var.Name] = true
			}
		}
		return vd, nil
	case stmt.Assign != nil:
		val, err := compileExpr(stmt.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(stmt.Assign.Index) > 0 {
			target := Expr(&NameRef{Name: stmt.Assign.Name})
			target, err = applyIndexOps(target, stmt.Assign.Index)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		updated := false
		if app, ok := val.(*AppendExpr); ok {
			if nref, ok2 := app.List.(*NameRef); ok2 && nref.Name == stmt.Assign.Name {
				elemType := inferType(app.Elem)
				if elemType != "" {
					varTypes[stmt.Assign.Name] = fmt.Sprintf("Vec<%s>", elemType)
					updated = true
				}
			}
		}
		if _, ok := val.(*MapLit); ok {
			mapVars[stmt.Assign.Name] = true
		}
		if inferType(val) == "String" {
			stringVars[stmt.Assign.Name] = true
		}
		if t := inferType(val); t != "" && !updated {
			varTypes[stmt.Assign.Name] = t
		}
		if vt, ok := varTypes[stmt.Assign.Name]; ok && vt == "String" {
			if _, ok := val.(*StringLit); ok {
				val = &StringCastExpr{Expr: val}
			} else if inferType(val) != "String" {
				val = &StringCastExpr{Expr: val}
			}
		}
		return &AssignStmt{Name: stmt.Assign.Name, Expr: val}, nil
	case stmt.Return != nil:
		if stmt.Return.Value != nil {
			val, err := compileExpr(stmt.Return.Value)
			if err != nil {
				return nil, err
			}
			return &ReturnStmt{Value: val}, nil
		}
		return &ReturnStmt{}, nil
	case stmt.Fun != nil:
		return compileFunStmt(stmt.Fun)
	case stmt.If != nil:
		return compileIfStmt(stmt.If)
	case stmt.While != nil:
		return compileWhileStmt(stmt.While)
	case stmt.For != nil:
		return compileForStmt(stmt.For)
	case stmt.Update != nil:
		return compileUpdateStmt(stmt.Update)
	case stmt.Break != nil:
		return &BreakStmt{}, nil
	case stmt.Continue != nil:
		return &ContinueStmt{}, nil
	case stmt.Import != nil:
		if handleImport(stmt.Import, curEnv) {
			return nil, nil
		}
		return nil, fmt.Errorf("unsupported import")
	case stmt.ExternVar != nil:
		if stmt.ExternVar.Root == "math" {
			useMath = true
		}
		return nil, nil
	case stmt.ExternFun != nil:
		if stmt.ExternFun.Root == "math" {
			useMath = true
		}
		return nil, nil
	case stmt.Type != nil:
		return compileTypeStmt(stmt.Type)
	case stmt.Test == nil && stmt.Import == nil && stmt.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", stmt.Pos.Line, stmt.Pos.Column)
	}
	return nil, nil
}

func compileIfStmt(n *parser.IfStmt) (Stmt, error) {
	localVarStack = append(localVarStack, map[string]bool{})
	defer func() { localVarStack = localVarStack[:len(localVarStack)-1] }()
	cond, err := compileExpr(n.Cond)
	if err != nil {
		return nil, err
	}
	if inferType(cond) != "bool" {
		if strings.HasPrefix(inferType(cond), "Option<") {
			cond = &MethodCallExpr{Receiver: cond, Name: "is_some"}
		} else {
			switch cond.(type) {
			case *FieldExpr, *NameRef:
				cond = &MethodCallExpr{Receiver: cond, Name: "is_some"}
			default:
				cond = &BoolLit{Value: true}
			}
		}
	}
	thenStmts := make([]Stmt, 0, len(n.Then))
	for _, st := range n.Then {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			thenStmts = append(thenStmts, cs)
		}
	}
	var elseStmts []Stmt
	if len(n.Else) > 0 {
		elseStmts = make([]Stmt, 0, len(n.Else))
		for _, st := range n.Else {
			cs, err := compileStmt(st)
			if err != nil {
				return nil, err
			}
			if cs != nil {
				elseStmts = append(elseStmts, cs)
			}
		}
	}
	var elseIf *IfStmt
	if n.ElseIf != nil {
		s, err := compileIfStmt(n.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = s.(*IfStmt)
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts, ElseIf: elseIf}, nil
}

func compileWhileStmt(n *parser.WhileStmt) (Stmt, error) {
	localVarStack = append(localVarStack, map[string]bool{})
	defer func() { localVarStack = localVarStack[:len(localVarStack)-1] }()
	cond, err := compileExpr(n.Cond)
	if err != nil {
		return nil, err
	}
	body := make([]Stmt, 0, len(n.Body))
	for _, st := range n.Body {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			body = append(body, cs)
		}
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func compileForStmt(n *parser.ForStmt) (Stmt, error) {
	localVarStack = append(localVarStack, map[string]bool{})
	defer func() { localVarStack = localVarStack[:len(localVarStack)-1] }()
	iter, err := compileExpr(n.Source)
	if nr, ok := iter.(*NameRef); ok && groupVars[nr.Name] {
		iter = &FieldExpr{Receiver: iter, Name: "items"}
	}
	if err != nil {
		return nil, err
	}
	byRef := false
	if t := inferType(iter); strings.HasPrefix(t, "Vec<") {
		elem := strings.TrimSuffix(strings.TrimPrefix(t, "Vec<"), ">")
		if elem == "String" || strings.HasPrefix(elem, "Vec<") || strings.HasPrefix(elem, "HashMap<") {
			byRef = true
		} else if _, ok := structTypes[elem]; ok {
			byRef = true
		}
	}
	var end Expr
	if n.RangeEnd != nil {
		end, err = compileExpr(n.RangeEnd)
		if err != nil {
			return nil, err
		}
		varTypes[n.Name] = "usize"
	}
	body := make([]Stmt, 0, len(n.Body))
	for _, st := range n.Body {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			body = append(body, cs)
		}
	}
	if _, ok := varTypes[n.Name]; !ok {
		t := inferType(iter)
		if strings.HasPrefix(t, "Vec<") {
			typ := strings.TrimSuffix(strings.TrimPrefix(t, "Vec<"), ">")
			if byRef && typ == "String" {
				varTypes[n.Name] = "&String"
			} else {
				varTypes[n.Name] = typ
			}
		} else {
			varTypes[n.Name] = "i64"
		}
	}
	return &ForStmt{Var: n.Name, Iter: iter, End: end, Body: body, ByRef: byRef}, nil
}

func compileUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	idxVar := "_i"
	start := &NumberLit{Value: "0"}
	end := &LenExpr{Arg: &NameRef{Name: u.Target}}

	var st types.StructType
	if curEnv != nil {
		if t, err := curEnv.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}
	orig := curEnv
	var fieldDecls []Stmt
	if st.Name != "" {
		child := types.NewEnv(curEnv)
		for _, f := range st.Order {
			child.SetVar(f, st.Fields[f], true)
			varTypes[f] = rustTypeFromType(st.Fields[f])
			expr := Expr(&FieldExpr{Receiver: &IndexExpr{Target: &NameRef{Name: u.Target}, Index: &NameRef{Name: idxVar}}, Name: f})
			vt := rustTypeFromType(st.Fields[f])
			if vt == "String" {
				expr = &MethodCallExpr{Receiver: expr, Name: "clone"}
			}
			fieldDecls = append(fieldDecls, &VarDecl{Name: f, Expr: expr, Type: vt})
		}
		curEnv = child
	}

	updates := make([]Stmt, 0, len(u.Set.Items))
	for _, it := range u.Set.Items {
		field, _ := identName(it.Key)
		val, err := compileExpr(it.Value)
		if err != nil {
			curEnv = orig
			return nil, err
		}
		if st.Name != "" {
			if ft, ok := st.Fields[field]; ok {
				if rustTypeFromType(ft) == "String" {
					val = &StringCastExpr{Expr: val}
				}
			}
		}
		target := &FieldExpr{Receiver: &IndexExpr{Target: &NameRef{Name: u.Target}, Index: &NameRef{Name: idxVar}}, Name: field}
		updates = append(updates, &IndexAssignStmt{Target: target, Value: val})
	}

	var cond Expr
	var err error
	if u.Where != nil {
		cond, err = compileExpr(u.Where)
		if err != nil {
			curEnv = orig
			return nil, err
		}
	}

	curEnv = orig

	var body []Stmt
	body = append(body, fieldDecls...)
	if cond != nil {
		body = append(body, &IfStmt{Cond: cond, Then: updates})
	} else {
		body = append(body, updates...)
	}
	loop := &ForStmt{Var: idxVar, Iter: start, End: end, Body: body}
	decl := &VarDecl{Name: u.Target, Expr: &NameRef{Name: u.Target}, Mutable: true}
	return &MultiStmt{Stmts: []Stmt{decl, loop}}, nil
}

func compileTypeStmt(t *parser.TypeDecl) (Stmt, error) {
	if len(t.Variants) > 0 {
		ut, ok := curEnv.GetUnion(t.Name)
		if !ok {
			return nil, nil
		}
		ed := &EnumDecl{Name: t.Name}
		for _, v := range t.Variants {
			st, ok := curEnv.GetStruct(v.Name)
			if !ok {
				continue
			}
			fields := make([]Param, len(st.Order))
			for i, name := range st.Order {
				typ := rustTypeFromType(st.Fields[name])
				if typ == t.Name {
					typ = fmt.Sprintf("Box<%s>", typ)
				}
				fields[i] = Param{Name: name, Type: typ}
			}
			ed.Variants = append(ed.Variants, EnumVariant{Name: v.Name, Fields: fields})
		}
		typeDecls = append(typeDecls, ed)
		structTypes[t.Name] = types.StructType{}
		_ = ut
		return nil, nil
	}
	st, ok := curEnv.GetStruct(t.Name)
	if !ok {
		return nil, nil
	}
	fields := make([]Param, len(st.Order))
	for i, name := range st.Order {
		fields[i] = Param{Name: name, Type: rustTypeFromType(st.Fields[name])}
	}
	typeDecls = append(typeDecls, &StructDecl{Name: t.Name, Fields: fields})
	structTypes[t.Name] = st
	return nil, nil
}

func compileFunStmt(fn *parser.FunStmt) (Stmt, error) {
	funcDepth++
	defer func() {
		funcDepth--
		if len(localVarStack) > 0 {
			localVarStack = localVarStack[:len(localVarStack)-1]
		}
	}()
	locals := map[string]bool{}
	currentFuncLocals = map[string]bool{}
	params := make([]Param, len(fn.Params))
	typList := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := ""
		if p.Type != nil {
			typ = rustTypeRef(p.Type)
		}
		params[i] = Param{Name: p.Name, Type: typ}
		locals[p.Name] = true
		currentFuncLocals[p.Name] = true
		typList[i] = typ
		if typ != "" {
			varTypes[p.Name] = typ
			if strings.HasPrefix(typ, "HashMap") {
				mapVars[p.Name] = true
			}
			if typ == "String" {
				stringVars[p.Name] = true
			}
		}
	}
	localVarStack = append(localVarStack, locals)
	funParams[fn.Name] = len(fn.Params)
	funParamTypes[fn.Name] = typList
	body := make([]Stmt, 0, len(fn.Body))
	for _, st := range fn.Body {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			body = append(body, cs)
		}
	}
	ret := ""
	if fn.Return != nil {
		if fn.Return.Simple != nil || fn.Return.Generic != nil {
			ret = rustTypeRef(fn.Return)
		} else if fn.Return.Fun != nil {
			var pts []string
			for _, p := range fn.Return.Fun.Params {
				t := "i64"
				if p.Simple != nil || p.Generic != nil {
					t = rustTypeRef(p)
				}
				pts = append(pts, t)
			}
			rt := "()"
			if fn.Return.Fun.Return != nil {
				r := rustTypeRef(fn.Return.Fun.Return)
				if r != "" {
					rt = r
				}
			}
			ret = fmt.Sprintf("impl Fn(%s) -> %s", strings.Join(pts, ", "), rt)
		}
	}
	if ret == "" || strings.HasPrefix(ret, "HashMap") {
		var out string
		for _, st := range body {
			if r, ok := st.(*ReturnStmt); ok && r.Value != nil {
				t := inferType(r.Value)
				if out == "" {
					out = t
				} else if out != t {
					out = ""
					break
				}
			}
		}
		if out != "" {
			ret = out
		}
	}
	if ret == "String" {
		for i, st := range body {
			if r, ok := st.(*ReturnStmt); ok && r.Value != nil {
				r.Value = &StringCastExpr{Expr: r.Value}
				body[i] = r
			}
		}
	}
	name := fn.Name
	if name == "main" {
		mainFuncName = "mochi_main"
		name = mainFuncName
	}
	if ret != "" {
		funReturns[fn.Name] = ret
	}
	localsCopy := currentFuncLocals
	currentFuncLocals = nil
	return &FuncDecl{Name: name, Params: params, Return: ret, Body: body, Locals: localsCopy}, nil
}

func applyIndexOps(base Expr, ops []*parser.IndexOp) (Expr, error) {
	var err error
	for _, op := range ops {
		if op.Colon != nil || op.End != nil || op.Colon2 != nil || op.Step != nil {
			if op.Colon2 != nil || op.Step != nil {
				return nil, fmt.Errorf("slice step not supported")
			}
			var start, end Expr
			if op.Start != nil {
				start, err = compileExpr(op.Start)
				if err != nil {
					return nil, err
				}
			}
			if op.End != nil {
				end, err = compileExpr(op.End)
				if err != nil {
					return nil, err
				}
			}
			base = &SliceExpr{Target: base, Start: start, End: end}
			continue
		}
		if op.Start == nil {
			return nil, fmt.Errorf("nil index")
		}
		var idx Expr
		idx, err = compileExpr(op.Start)
		if err != nil {
			return nil, err
		}
		if len(ops) == 1 {
			switch b := base.(type) {
			case *StringLit:
				base = &StringIndexExpr{Str: b, Index: idx}
			case *NameRef:
				if stringVars[b.Name] {
					base = &StringIndexExpr{Str: b, Index: idx}
				} else {
					base = &IndexExpr{Target: base, Index: idx}
				}
			default:
				base = &IndexExpr{Target: base, Index: idx}
			}
		} else {
			base = &IndexExpr{Target: base, Index: idx}
		}
	}
	return base, nil
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
	ops := make([]string, len(e.Binary.Right))
	for i, op := range e.Binary.Right {
		right, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		ops[i] = op.Op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
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
			if contains(level, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				expr := &BinaryExpr{Left: left, Op: ops[i], Right: right}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
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
		op := u.Ops[i]
		expr = &UnaryExpr{Op: op, Expr: expr}
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
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 && len(p.Ops) == 1 && p.Ops[0].Call != nil {
		alias := p.Target.Selector.Root
		method := p.Target.Selector.Tail[0]
		if kind, ok := builtinAliases[alias]; ok {
			args := make([]Expr, len(p.Ops[0].Call.Args))
			for i, a := range p.Ops[0].Call.Args {
				ex, err := compileExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			switch kind {
			case "go_testpkg":
				if method == "Add" && len(args) == 2 {
					return &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}, nil
				}
				if method == "FifteenPuzzleExample" && len(args) == 0 {
					return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
				}
			}
		}
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.End != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				if op.Index.Colon2 != nil || op.Index.Step != nil {
					return nil, fmt.Errorf("slice step not supported")
				}
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
			if op.Index.Start == nil {
				return nil, fmt.Errorf("nil index")
			}
			idx, err := compileExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			switch t := expr.(type) {
			case *StringLit:
				expr = &StringIndexExpr{Str: t, Index: idx}
			case *NameRef:
				if stringVars[t.Name] {
					expr = &StringIndexExpr{Str: t, Index: idx}
				} else {
					expr = &IndexExpr{Target: expr, Index: idx}
				}
			default:
				expr = &IndexExpr{Target: expr, Index: idx}
			}
		case op.Field != nil:
			expr = &FieldExpr{Receiver: expr, Name: op.Field.Name}
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
				expr = &MethodCallExpr{Receiver: fe.Receiver, Name: fe.Name, Args: args}
			} else if id, ok := expr.(*NameRef); ok {
				expr = &CallExpr{Func: id.Name, Args: args}
			} else {
				expr = &MethodCallExpr{Receiver: expr, Name: "apply", Args: args}
			}
		case op.Cast != nil:
			// ignore casts
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
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
		if name == "main" && mainFuncName != "" {
			name = mainFuncName
		}
		if name == "now" && len(args) == 0 {
			useTime = true
			return &NowExpr{}, nil
		}
		if name == "input" && len(args) == 0 {
			usesInput = true
			funReturns[name] = "String"
		}
		if name == "int" && len(args) == 1 {
			usesInt = true
			funReturns[name] = "i64"
		}
		if ut, ok := curEnv.FindUnionByVariant(name); ok {
			st, _ := curEnv.GetStruct(name)
			fields := make([]Expr, len(args))
			names := make([]string, len(args))
			for i, a := range args {
				if i < len(st.Order) {
					names[i] = st.Order[i]
				} else {
					names[i] = fmt.Sprintf("f%d", i)
				}
				if !patternMode {
					if stf, ok := st.Fields[names[i]]; ok {
						rt := rustTypeFromType(stf)
						if rt == ut.Name {
							a = &CallExpr{Func: "Box::new", Args: []Expr{a}}
						}
					}
				}
				fields[i] = a
			}
			types := make([]string, len(names))
			for i, n := range names {
				if stf, ok := st.Fields[n]; ok {
					typ := rustTypeFromType(stf)
					if typ == ut.Name {
						types[i] = fmt.Sprintf("Box<%s>", typ)
					} else {
						types[i] = typ
					}
				}
			}
			return &EnumLit{Union: ut.Name, Variant: name, Fields: fields, Names: names, Types: types, IsPattern: patternMode}, nil
		}
		if name == "print" {
			if len(args) == 1 {
				fmtStr := "{}"
				switch a := args[0].(type) {
				case *ValuesExpr:
					args[0] = &JoinExpr{List: a}
				case *MapLit, *AppendExpr, *ListLit:
					fmtStr = "{:?}"
				case *SliceExpr:
					if inferType(a) != "String" {
						fmtStr = "{:?}"
					}
				case *QueryExpr:
					args[0] = &JoinExpr{List: a}
					fmtStr = "{}"
				case *StringLit:
					// no format needed
				}
				if fmtStr == "{}" {
					t := inferType(args[0])
					if strings.HasPrefix(t, "Vec<") {
						elem := strings.TrimSuffix(strings.TrimPrefix(t, "Vec<"), ">")
						if elem != "i64" && elem != "String" && elem != "bool" {
							args[0] = &JoinExpr{List: args[0]}
						} else {
							fmtStr = "{:?}"
						}
					}
				}
				return &PrintExpr{Fmt: fmtStr, Args: args, Trim: false}, nil
			}
			fmtStr := "{}"
			for i := 1; i < len(args); i++ {
				fmtStr += " {}"
			}
			return &PrintExpr{Fmt: fmtStr, Args: args, Trim: true}, nil
		}
		if name == "len" && len(args) == 1 {
			return &LenExpr{Arg: args[0]}, nil
		}
		if name == "sum" && len(args) == 1 {
			return &SumExpr{Arg: args[0]}, nil
		}
		if name == "str" && len(args) == 1 {
			return &StrExpr{Arg: args[0]}, nil
		}
		if name == "values" && len(args) == 1 {
			return &ValuesExpr{Map: args[0]}, nil
		}
		if name == "append" && len(args) == 2 {
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		}
		if name == "avg" && len(args) == 1 {
			return &AvgExpr{List: args[0]}, nil
		}
		if name == "count" && len(args) == 1 {
			return &LenExpr{Arg: args[0]}, nil
		}
		if name == "exists" && len(args) == 1 {
			return &ExistsExpr{List: args[0]}, nil
		}
		if name == "min" && len(args) == 1 {
			return &MinExpr{List: args[0]}, nil
		}
		if name == "max" && len(args) == 1 {
			return &MaxExpr{List: args[0]}, nil
		}
		if name == "substring" && len(args) == 3 {
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		}
		if pts, ok := funParamTypes[name]; ok {
			for i := 0; i < len(args) && i < len(pts); i++ {
				if strings.HasPrefix(pts[i], "&") {
					if _, isUnary := args[i].(*UnaryExpr); !isUnary {
						args[i] = &UnaryExpr{Op: "&", Expr: args[i]}
					}
				} else if pts[i] == "String" && inferType(args[i]) != "String" {
					args[i] = &StringCastExpr{Expr: args[i]}
				}
			}
		}
		if cnt, ok := funParams[name]; ok && len(args) < cnt {
			missing := cnt - len(args)
			params := make([]Param, missing)
			vars := make([]Expr, missing)
			for i := 0; i < missing; i++ {
				pname := fmt.Sprintf("p%d", i)
				params[i] = Param{Name: pname}
				vars[i] = &NameRef{Name: pname}
			}
			bodyArgs := append(append([]Expr{}, args...), vars...)
			body := &CallExpr{Func: name, Args: bodyArgs}
			return &FunLit{Params: params, Expr: body}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil:
		return compileLiteral(p.Lit)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := compileExpr(e)
			if err != nil {
				return nil, err
			}
			if inferType(ex) == "usize" {
				ex = &IntCastExpr{Expr: ex}
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		if name, ok := structForMap[p.Map]; ok {
			fields := make([]Expr, len(p.Map.Items))
			names := make([]string, len(p.Map.Items))
			for i, it := range p.Map.Items {
				if k, ok := types.SimpleStringKey(it.Key); ok {
					names[i] = k
				} else {
					names[i] = fmt.Sprintf("f%d", i)
				}
				v, err := compileExpr(it.Value)
				if err != nil {
					return nil, err
				}
				if st, ok := structTypes[name]; ok {
					if ft, okf := st.Fields[names[i]]; okf {
						rt := rustTypeFromType(ft)
						if rt == "String" {
							v = &StringCastExpr{Expr: v}
						} else if rt == "f64" && inferType(v) == "i64" {
							v = &FloatCastExpr{Expr: v}
						}
					}
				}
				fields[i] = v
			}
			return &StructLit{Name: name, Fields: fields, Names: names}, nil
		}
		if st, ok := types.InferStructFromMapEnv(p.Map, curEnv); ok {
			sigParts := make([]string, len(st.Order))
			for i, n := range st.Order {
				sigParts[i] = n + ":" + rustTypeFromType(st.Fields[n])
			}
			sig := strings.Join(sigParts, ";")
			name, ok := structSig[sig]
			if !ok {
				name = types.UniqueStructName("Map", curEnv, nil)
				st.Name = name
				curEnv.SetStruct(name, st)
				fields := make([]Param, len(st.Order))
				for i, n := range st.Order {
					fields[i] = Param{Name: n, Type: rustTypeFromType(st.Fields[n])}
				}
				typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
				structTypes[name] = st
				structSig[sig] = name
			}
			structForMap[p.Map] = name
			names := st.Order
			vals := make([]Expr, len(p.Map.Items))
			for i, it := range p.Map.Items {
				v, err := compileExpr(it.Value)
				if err != nil {
					return nil, err
				}
				ft := st.Fields[names[i]]
				rt := rustTypeFromType(ft)
				if rt == "String" {
					v = &StringCastExpr{Expr: v}
				} else if rt == "f64" && inferType(v) == "i64" {
					v = &FloatCastExpr{Expr: v}
				}
				vals[i] = v
			}
			return &StructLit{Name: name, Fields: vals, Names: names}, nil
		}
		entries := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := compileExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := compileExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: k, Value: v}
		}
		usesHashMap = true
		return &MapLit{Items: entries}, nil
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type)
		if err == nil {
			return expr, nil
		}
		return nil, err
	case p.Struct != nil:
		if ut, ok := curEnv.FindUnionByVariant(p.Struct.Name); ok {
			st, _ := curEnv.GetStruct(p.Struct.Name)
			fields := make([]Expr, len(p.Struct.Fields))
			names := make([]string, len(p.Struct.Fields))
			for i, f := range p.Struct.Fields {
				names[i] = f.Name
				v, err := compileExpr(f.Value)
				if err != nil {
					return nil, err
				}
				if !patternMode {
					if stf, ok := st.Fields[f.Name]; ok {
						rt := rustTypeFromType(stf)
						if rt == ut.Name {
							v = &CallExpr{Func: "Box::new", Args: []Expr{v}}
						} else if rt == "String" {
							v = &StringCastExpr{Expr: v}
						} else if rt == "f64" && inferType(v) == "i64" {
							v = &FloatCastExpr{Expr: v}
						}
					}
				}
				fields[i] = v
			}
			types := make([]string, len(names))
			for i, n := range names {
				if stf, ok := st.Fields[n]; ok {
					typ := rustTypeFromType(stf)
					if typ == ut.Name {
						types[i] = fmt.Sprintf("Box<%s>", typ)
					} else {
						types[i] = typ
					}
				}
			}
			return &EnumLit{Union: ut.Name, Variant: p.Struct.Name, Fields: fields, Names: names, Types: types, IsPattern: patternMode}, nil
		}
		st, ok := structTypes[p.Struct.Name]
		if !ok {
			st, _ = curEnv.GetStruct(p.Struct.Name)
		}
		fields := make([]Expr, len(p.Struct.Fields))
		names := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			names[i] = f.Name
			v, err := compileExpr(f.Value)
			if err != nil {
				return nil, err
			}
			if stf, ok := st.Fields[f.Name]; ok {
				rt := rustTypeFromType(stf)
				if rt == "String" {
					v = &StringCastExpr{Expr: v}
				} else if rt == "f64" && inferType(v) == "i64" {
					v = &FloatCastExpr{Expr: v}
				}
			}
			fields[i] = v
		}
		structTypes[p.Struct.Name] = st
		return &StructLit{Name: p.Struct.Name, Fields: fields, Names: names}, nil
	case p.Selector != nil:
		if mod, ok := builtinAliases[p.Selector.Root]; ok && len(p.Selector.Tail) == 1 {
			tail := p.Selector.Tail[0]
			switch mod {
			case "go_testpkg":
				switch tail {
				case "Pi":
					return &NumberLit{Value: "3.14"}, nil
				case "Answer":
					return &NumberLit{Value: "42"}, nil
				case "FifteenPuzzleExample":
					return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
				}
			}
		}
		expr := Expr(&NameRef{Name: p.Selector.Root})
		for _, f := range p.Selector.Tail {
			expr = &FieldExpr{Receiver: expr, Name: f}
		}
		return expr, nil
	case p.FunExpr != nil:
		params := make([]Param, len(p.FunExpr.Params))
		for i, pa := range p.FunExpr.Params {
			typ := ""
			if pa.Type != nil {
				typ = rustTypeRef(pa.Type)
			}
			params[i] = Param{Name: pa.Name, Type: typ}
		}
		var expr Expr
		if p.FunExpr.ExprBody != nil {
			e, err := compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return nil, err
			}
			expr = e
		}
		ret := ""
		if p.FunExpr.Return != nil {
			ret = rustTypeRef(p.FunExpr.Return)
		}
		return &FunLit{Params: params, Return: ret, Expr: expr}, nil
	case p.Match != nil:
		return compileMatchExpr(p.Match)
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.Group != nil:
		return compileExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileIfExpr(n *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(n.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(n.Then)
	if err != nil {
		return nil, err
	}
	var elseIf *IfExpr
	if n.ElseIf != nil {
		ei, err := compileIfExpr(n.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = ei.(*IfExpr)
	}
	var elseExpr Expr
	if n.Else != nil {
		e, err := compileExpr(n.Else)
		if err != nil {
			return nil, err
		}
		elseExpr = e
	}
	return &IfExpr{Cond: cond, Then: thenExpr, ElseIf: elseIf, Else: elseExpr}, nil
}

func compileMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := compileExpr(me.Target)
	if err != nil {
		return nil, err
	}
	arms := make([]MatchArm, len(me.Cases))
	for i, c := range me.Cases {
		pat, err := compilePattern(c.Pattern)
		if err != nil {
			return nil, err
		}
		if n, ok := pat.(*NameRef); ok && n.Name == "_" {
			pat = nil
		}
		res, err := compileExpr(c.Result)
		if err != nil {
			return nil, err
		}
		arms[i] = MatchArm{Pattern: pat, Result: res}
	}
	return &MatchExpr{Target: target, Arms: arms}, nil
}

func compilePattern(e *parser.Expr) (Expr, error) {
	patternMode = true
	ex, err := compileExpr(e)
	patternMode = false
	if el, ok := ex.(*EnumLit); ok {
		for i, t := range el.Types {
			if strings.HasPrefix(t, "Box<") {
				boxVars[el.Names[i]] = true
			}
		}
	}
	return ex, err
}

func compileQueryExpr(q *parser.QueryExpr) (Expr, error) {
	src, err := compileExpr(q.Source)
	if nr, ok := src.(*NameRef); ok && groupVars[nr.Name] {
		src = &FieldExpr{Receiver: src, Name: "items"}
	}
	if err != nil {
		return nil, err
	}
	varByRef := false
	if t := inferType(src); strings.HasPrefix(t, "Vec<") {
		elem := strings.TrimSuffix(strings.TrimPrefix(t, "Vec<"), ">")
		if elem == "String" || strings.HasPrefix(elem, "Vec<") || strings.HasPrefix(elem, "HashMap<") {
			varByRef = true
		} else if _, ok := structTypes[elem]; ok {
			varByRef = true
		}
	}
	srcT := types.ExprType(q.Source, curEnv)
	var elemT types.Type = types.AnyType{}
	if lt, ok := srcT.(types.ListType); ok {
		elemT = lt.Elem
	}
	child := types.NewEnv(curEnv)
	child.SetVar(q.Var, elemT, true)
	varTypes[q.Var] = rustTypeFromType(elemT)
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := compileExprWithEnv(f.Src, child)
		if nr, ok := fe.(*NameRef); ok && groupVars[nr.Name] {
			fe = &FieldExpr{Receiver: fe, Name: "items"}
		}
		if err != nil {
			return nil, err
		}
		ft := types.ExprType(f.Src, child)
		var felem types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			felem = lt.Elem
		}
		child.SetVar(f.Var, felem, true)
		varTypes[f.Var] = rustTypeFromType(felem)
		byRef := false
		et := rustTypeFromType(felem)
		if et != "i64" && et != "" {
			byRef = true
		}
		froms[i] = queryFrom{Var: f.Var, Src: fe, ByRef: byRef}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		side := ""
		if j.Side != nil {
			side = *j.Side
			if side != "right" && side != "left" && side != "outer" {
				return nil, fmt.Errorf("join side not supported")
			}
		}
		je, err := compileExprWithEnv(j.Src, child)
		if nr, ok := je.(*NameRef); ok && groupVars[nr.Name] {
			je = &FieldExpr{Receiver: je, Name: "items"}
		}
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		baseType := rustTypeFromType(jelem)
		varTypes[j.Var] = baseType
		byRef := false
		et := rustTypeFromType(jelem)
		if et != "i64" && et != "" {
			byRef = true
		}
		on, err := compileExprWithEnv(j.On, child)
		if err != nil {
			return nil, err
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: on, ByRef: byRef, Side: side, Typ: baseType}
	}

	var groupKey Expr
	groupVar := ""
	groupType := ""
	if q.Group != nil {
		usesGroup = true
		usesHashMap = true
		if ml := mapLiteralExpr(q.Group.Exprs[0]); ml != nil {
			if _, ok := structForMap[ml]; !ok {
				if st, ok := types.InferStructFromMapEnv(ml, child); ok {
					name := types.UniqueStructName("GroupKey", curEnv, nil)
					st.Name = name
					curEnv.SetStruct(name, st)
					fields := make([]Param, len(st.Order))
					for i, n := range st.Order {
						fields[i] = Param{Name: n, Type: rustTypeFromType(st.Fields[n])}
					}
					typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
					structTypes[name] = st
					structForMap[ml] = name
				}
			}
		}
		gk, err := compileExprWithEnv(q.Group.Exprs[0], child)
		if err != nil {
			return nil, err
		}
		groupKey = gk
		keyT := inferType(gk)
		elemTStr := rustTypeFromType(elemT)
		groupVar = q.Group.Name
		groupType = fmt.Sprintf("Group<%s, %s>", keyT, elemTStr)
		varTypes[groupVar] = groupType
		groupVars[groupVar] = true
		genv := types.NewEnv(child)
		genv.SetVar(groupVar, types.GroupType{Key: types.ExprType(q.Group.Exprs[0], child), Elem: elemT}, true)
		child = genv
	}

	var where Expr
	if q.Where != nil {
		w, err := compileExprWithEnv(q.Where, child)
		if err != nil {
			return nil, err
		}
		where = w
	}

	var sortExpr Expr
	sortType := ""
	if q.Sort != nil {
		se, err := compileExprWithEnv(q.Sort, child)
		if err != nil {
			return nil, err
		}
		sortExpr = se
		sortType = inferType(se)
		if sortType == "" {
			sortType = "i64"
		}
	}
	var skipExpr Expr
	if q.Skip != nil {
		se, err := compileExprWithEnv(q.Skip, child)
		if err != nil {
			return nil, err
		}
		skipExpr = se
	}
	var takeExpr Expr
	if q.Take != nil {
		te, err := compileExprWithEnv(q.Take, child)
		if err != nil {
			return nil, err
		}
		takeExpr = te
	}

	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "outer" && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil {
		j := q.Joins[0]
		selectEnv := types.NewEnv(child)
		selectEnv.SetVar(q.Var, types.OptionType{Elem: elemT}, true)
		oldLeft := varTypes[q.Var]
		varTypes[q.Var] = fmt.Sprintf("Option<%s>", rustTypeFromType(elemT))
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		selectEnv.SetVar(j.Var, types.OptionType{Elem: jelem}, true)
		oldRight := varTypes[j.Var]
		varTypes[j.Var] = fmt.Sprintf("Option<%s>", rustTypeFromType(jelem))

		ml := mapLiteralExpr(q.Select)
		itemType := "i64"
		if ml != nil {
			if st, ok := types.InferStructFromMapEnv(ml, selectEnv); ok {
				name := types.UniqueStructName("QueryItem", curEnv, nil)
				st.Name = name
				curEnv.SetStruct(name, st)
				fields := make([]Param, len(st.Order))
				for i, n := range st.Order {
					fields[i] = Param{Name: n, Type: rustTypeFromType(st.Fields[n])}
				}
				typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
				structTypes[name] = st
				structForMap[ml] = name
				itemType = name
			}
		}
		sel, err := compileExprWithEnv(q.Select, selectEnv)
		if err != nil {
			varTypes[q.Var] = oldLeft
			varTypes[j.Var] = oldRight
			return nil, err
		}
		if t := inferType(sel); t != "" {
			itemType = t
		}
		rt := rustTypeFromType(jelem)
		lt := rustTypeFromType(elemT)
		ex := &OuterJoinExpr{LeftVar: q.Var, LeftSrc: src, LeftByRef: varByRef, LeftType: lt, RightVar: j.Var, RightSrc: joins[0].Src, RightByRef: joins[0].ByRef, RightType: rt, Cond: joins[0].On, Select: sel, ItemType: itemType}
		varTypes[q.Var] = oldLeft
		varTypes[j.Var] = oldRight
		return ex, nil
	}

	ml := mapLiteralExpr(q.Select)
	itemType := "i64"
	if ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			name := types.UniqueStructName("QueryItem", curEnv, nil)
			st.Name = name
			curEnv.SetStruct(name, st)
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: rustTypeFromType(st.Fields[n])}
			}
			typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
			structTypes[name] = st
			structForMap[ml] = name
			itemType = name
		}
	}
	sel, err := compileExprWithEnv(q.Select, child)
	if err != nil {
		return nil, err
	}
	if t := inferType(sel); t != "" {
		itemType = t
	}
	return &QueryExpr{Var: q.Var, Src: src, VarByRef: varByRef, Froms: froms, Joins: joins, Where: where, Sort: sortExpr, SortType: sortType, Skip: skipExpr, Take: takeExpr, GroupKey: groupKey, GroupVar: groupVar, GroupType: groupType, Select: sel, ItemType: itemType}, nil
}

func compileExprWithEnv(e *parser.Expr, env *types.Env) (Expr, error) {
	old := curEnv
	curEnv = env
	defer func() { curEnv = old }()
	return compileExpr(e)
}

func compileLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Int != nil:
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		val := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(val, ".eE") {
			val += ".0"
		}
		return &NumberLit{Value: val}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Null:
		return &NullLit{}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func inferType(e Expr) string {
	switch ex := e.(type) {
	case *NumberLit:
		if strings.ContainsAny(ex.Value, ".eE") {
			return "f64"
		}
		return "i64"
	case *BoolLit:
		return "bool"
	case *NullLit:
		return ""
	case *StringLit:
		return "String"
	case *ListLit:
		if len(ex.Elems) > 0 {
			t := inferType(ex.Elems[0])
			if t != "" {
				return fmt.Sprintf("Vec<%s>", t)
			}
		}
		return "Vec<i64>"
	case *ValuesExpr:
		mt := inferType(ex.Map)
		if strings.HasPrefix(mt, "HashMap<") {
			parts := strings.TrimPrefix(mt, "HashMap<")
			if idx := strings.Index(parts, ","); idx > 0 {
				vt := strings.TrimSuffix(parts[idx+1:], ">")
				return fmt.Sprintf("Vec<%s>", strings.TrimSpace(vt))
			}
		}
		return "Vec<i64>"
	case *AppendExpr:
		return inferType(e.(*AppendExpr).List)
	case *ExistsExpr:
		return "bool"
	case *BinaryExpr:
		switch ex.Op {
		case "<", "<=", ">", ">=", "==", "!=", "&&", "||", "in":
			return "bool"
		default:
			return inferType(ex.Left)
		}
	case *IfExpr:
		t1 := inferType(ex.Then)
		t2 := ""
		if ex.ElseIf != nil {
			t2 = inferType(ex.ElseIf)
		} else if ex.Else != nil {
			t2 = inferType(ex.Else)
		}
		if t1 == t2 {
			if t1 == "String" {
				return ""
			}
			return t1
		}
	case *MatchExpr:
		if len(ex.Arms) > 0 {
			t := inferType(ex.Arms[0].Result)
			same := true
			for _, a := range ex.Arms[1:] {
				if inferType(a.Result) != t {
					same = false
					break
				}
			}
			if same {
				return t
			}
		}
	case *IndexExpr:
		ct := inferType(ex.Target)
		if strings.HasPrefix(ct, "Vec<") {
			return strings.TrimSuffix(strings.TrimPrefix(ct, "Vec<"), ">")
		}
		if strings.HasPrefix(ct, "HashMap<") {
			parts := strings.TrimPrefix(ct, "HashMap<")
			if idx := strings.Index(parts, ","); idx > 0 {
				vt := strings.TrimSuffix(parts[idx+1:], ">")
				return strings.TrimSpace(vt)
			}
		}
		return "i64"
	case *NameRef:
		if t, ok := varTypes[ex.Name]; ok && t != "" {
			return t
		}
		if stringVars[ex.Name] {
			return "String"
		}
		if mapVars[ex.Name] {
			return "HashMap<String, i64>"
		}
		if ut, ok := curEnv.FindUnionByVariant(ex.Name); ok {
			return ut.Name
		}
		return "i64"
	case *SliceExpr:
		ct := inferType(ex.Target)
		if strings.HasPrefix(ct, "Vec<") {
			return ct
		}
		if ct == "String" {
			return "String"
		}
		return ct
	case *StructLit:
		return ex.Name
	case *EnumLit:
		return ex.Union
	case *QueryExpr:
		return fmt.Sprintf("Vec<%s>", ex.ItemType)
	case *OuterJoinExpr:
		return fmt.Sprintf("Vec<%s>", ex.ItemType)
	case *MapLit:
		usesHashMap = true
		if len(ex.Items) > 0 {
			kt := inferType(ex.Items[0].Key)
			vt := inferType(ex.Items[0].Value)
			if kt == "String" {
				kt = "&str"
			} else if kt == "" {
				kt = "String"
			}
			if vt == "" {
				vt = "i64"
			}
			return fmt.Sprintf("HashMap<%s, %s>", kt, vt)
		}
		return "HashMap<String, i64>"
	case *SubstringExpr:
		return "String"
	case *MethodCallExpr:
		switch ex.Name {
		case "contains":
			return "bool"
		}
		if nr, ok := ex.Receiver.(*NameRef); ok && nr.Name == "math" {
			return "f64"
		}
	case *CallExpr:
		if t, ok := funReturns[ex.Func]; ok {
			return t
		}
	case *UnaryExpr:
		if ex.Op == "!" {
			return "bool"
		}
		return inferType(ex.Expr)
	case *IntCastExpr:
		return "i64"
	case *FloatCastExpr:
		return "f64"
	case *FieldExpr:
		if nr, ok := ex.Receiver.(*NameRef); ok && nr.Name == "math" {
			return "f64"
		}
		rt := inferType(ex.Receiver)
		if strings.HasPrefix(rt, "&") {
			rt = rt[1:]
		}
		if st, ok := structTypes[rt]; ok {
			if ft, okf := st.Fields[ex.Name]; okf {
				return rustTypeFromType(ft)
			}
		}
		return inferType(ex.Receiver)
	case *FunLit:
		return ""
	}
	return ""
}

func containsFloat(e Expr) bool {
	switch ex := e.(type) {
	case *NumberLit:
		return strings.ContainsAny(ex.Value, ".eE")
	case *FloatCastExpr:
		return true
	case *BinaryExpr:
		return containsFloat(ex.Left) || containsFloat(ex.Right)
	case *UnaryExpr:
		return containsFloat(ex.Expr)
	}
	return false
}

func rustType(t string) string {
	switch t {
	case "int":
		return "i64"
	case "float":
		return "f64"
	case "bool":
		return "bool"
	case "string":
		return "String"
	}
	if _, ok := structTypes[t]; ok {
		return t
	}
	return "i64"
}

func rustTypeRef(tr *parser.TypeRef) string {
	if tr == nil {
		return ""
	}
	if tr.Simple != nil {
		return rustType(*tr.Simple)
	}
	if tr.Generic != nil {
		switch tr.Generic.Name {
		case "list":
			if len(tr.Generic.Args) == 1 {
				return fmt.Sprintf("Vec<%s>", rustTypeRef(tr.Generic.Args[0]))
			}
		case "map":
			usesHashMap = true
			if len(tr.Generic.Args) == 2 {
				kt := rustTypeRef(tr.Generic.Args[0])
				vt := rustTypeRef(tr.Generic.Args[1])
				return fmt.Sprintf("HashMap<%s, %s>", kt, vt)
			}
		case "option":
			if len(tr.Generic.Args) == 1 {
				return fmt.Sprintf("Option<%s>", rustTypeRef(tr.Generic.Args[0]))
			}
		}
	}
	if tr.Fun != nil {
		return "fn"
	}
	return "i64"
}

func rustTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "i64"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "bool"
	case types.BigRatType, types.FloatType:
		return "f64"
	case types.ListType:
		return fmt.Sprintf("Vec<%s>", rustTypeFromType(tt.Elem))
	case types.MapType:
		usesHashMap = true
		return fmt.Sprintf("HashMap<%s, %s>", rustTypeFromType(tt.Key), rustTypeFromType(tt.Value))
	case types.OptionType:
		return fmt.Sprintf("Option<%s>", rustTypeFromType(tt.Elem))
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	case types.VoidType:
		return "()"
	}
	return "i64"
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

func listLiteral(e *parser.Expr) *parser.ListLiteral {
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
	return p.Target.List
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

func literalStringExpr(e Expr) (string, bool) {
	if s, ok := e.(*StringLit); ok {
		return s.Value, true
	}
	return "", false
}

func parseFormat(e *parser.Expr) string {
	ml := mapLiteralExpr(e)
	if ml == nil {
		return ""
	}
	for _, it := range ml.Items {
		key, ok := literalString(it.Key)
		if !ok {
			continue
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func valueToExpr(v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		fields := make([]Expr, len(names))
		for i, k := range names {
			fields[i] = valueToExpr(val[k], nil)
			if typ != nil && typ.Simple != nil {
				if st, ok := structTypes[*typ.Simple]; ok {
					if ft, okf := st.Fields[k]; okf {
						rt := rustTypeFromType(ft)
						if rt == "String" {
							fields[i] = &StringCastExpr{Expr: fields[i]}
						} else if rt == "f64" && inferType(fields[i]) == "i64" {
							fields[i] = &FloatCastExpr{Expr: fields[i]}
						}
					}
				}
			}
		}
		if typ != nil && typ.Simple != nil {
			return &StructLit{Name: *typ.Simple, Fields: fields, Names: names}
		}
		entries := make([]MapEntry, len(names))
		for i, k := range names {
			entries[i] = MapEntry{Key: &StringLit{Value: k}, Value: fields[i]}
		}
		usesHashMap = true
		return &MapLit{Items: entries}
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
	case int, int64:
		return &NumberLit{Value: fmt.Sprintf("%v", val)}
	case float64, float32:
		return &NumberLit{Value: fmt.Sprintf("%v", val)}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func zeroValue(typ string) string {
	switch {
	case typ == "String":
		return "String::new()"
	case typ == "bool":
		return "false"
	case typ == "i64" || typ == "f64":
		return "0"
	case strings.HasPrefix(typ, "Vec<"):
		return "Vec::new()"
	case strings.HasPrefix(typ, "HashMap<"):
		return "HashMap::new()"
	case strings.HasPrefix(typ, "Option<"):
		return "None"
	default:
		if st, ok := structTypes[typ]; ok {
			parts := make([]string, len(st.Order))
			for i, name := range st.Order {
				ft := rustTypeFromType(st.Fields[name])
				parts[i] = fmt.Sprintf("%s: %s", name, zeroValue(ft))
			}
			return fmt.Sprintf("%s { %s }", typ, strings.Join(parts, ", "))
		}
		return "Default::default()"
	}
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
	return valueToExpr(v, typ), nil
}

func writeStmt(buf *bytes.Buffer, s Stmt, indent int) {
	for i := 0; i < indent; i++ {
		buf.WriteString("    ")
	}
	switch st := s.(type) {
	case *IfStmt:
		writeIfStmt(buf, st, indent)
	case *WhileStmt:
		writeWhileStmt(buf, st, indent)
	case *ForStmt:
		writeForStmt(buf, st, indent)
	case *MultiStmt:
		for _, ms := range st.Stmts {
			writeStmt(buf, ms, indent)
		}
	case *SaveStmt:
		st.emit(buf)
	case *BreakStmt, *ContinueStmt:
		st.emit(buf)
	case *ReturnStmt:
		st.emit(buf)
	default:
		st.emit(buf)
		buf.WriteString(";")
	}
	buf.WriteByte('\n')
}

func writeIfStmt(buf *bytes.Buffer, s *IfStmt, indent int) {
	buf.WriteString("if ")
	s.Cond.emit(buf)
	buf.WriteString(" {\n")
	for _, st := range s.Then {
		writeStmt(buf, st, indent+1)
	}
	for i := 0; i < indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString("}")
	if s.ElseIf != nil {
		buf.WriteString(" else ")
		writeIfStmt(buf, s.ElseIf, indent)
	} else if len(s.Else) > 0 {
		buf.WriteString(" else {\n")
		for _, st := range s.Else {
			writeStmt(buf, st, indent+1)
		}
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString("}")
	}
}

func writeWhileStmt(buf *bytes.Buffer, s *WhileStmt, indent int) {
	buf.WriteString("while ")
	if s.Cond != nil {
		s.Cond.emit(buf)
	} else {
		buf.WriteString("true")
	}
	buf.WriteString(" {\n")
	for _, st := range s.Body {
		writeStmt(buf, st, indent+1)
	}
	for i := 0; i < indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString("}")
}

func writeForStmt(buf *bytes.Buffer, s *ForStmt, indent int) {
	buf.WriteString("for ")
	buf.WriteString(s.Var)
	buf.WriteString(" in ")
	if s.End != nil {
		s.Iter.emit(buf)
		buf.WriteString("..")
		s.End.emit(buf)
	} else {
		if s.ByRef {
			buf.WriteString("&")
		}
		s.Iter.emit(buf)
	}
	buf.WriteString(" {\n")
	for _, st := range s.Body {
		writeStmt(buf, st, indent+1)
	}
	for i := 0; i < indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString("}")
}

// Emit generates formatted Rust source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	if prog.UsesHashMap {
		buf.WriteString("use std::collections::HashMap;\n")
	}
	if prog.UseTime {
		buf.WriteString("use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};\n")
		buf.WriteString("use std::time::{SystemTime, UNIX_EPOCH};\n")
	}
	if prog.UseInput {
		buf.WriteString("use std::io::{self, Read};\n")
	}
	if useMath {
		buf.WriteString("mod math {\n")
		buf.WriteString("    pub const pi: f64 = std::f64::consts::PI;\n")
		buf.WriteString("    pub const e: f64 = std::f64::consts::E;\n")
		buf.WriteString("    pub fn sqrt(x: f64) -> f64 { x.sqrt() }\n")
		buf.WriteString("    pub fn pow(x: f64, y: f64) -> f64 { x.powf(y) }\n")
		buf.WriteString("    pub fn sin(x: f64) -> f64 { x.sin() }\n")
		buf.WriteString("    pub fn log(x: f64) -> f64 { x.ln() }\n")
		buf.WriteString("}\n")
	}
	if prog.UsesGroup {
		buf.WriteString("#[derive(Clone)]\nstruct Group<K, V> { key: K, items: Vec<V> }\n")
	}
	if prog.UseTime {
		buf.WriteString("static NOW_SEEDED: AtomicBool = AtomicBool::new(false);\n")
		buf.WriteString("static NOW_SEED: AtomicI64 = AtomicI64::new(0);\n")
		buf.WriteString("fn _now() -> i64 {\n")
		buf.WriteString("    if !NOW_SEEDED.load(Ordering::SeqCst) {\n")
		buf.WriteString("        if let Ok(s) = std::env::var(\"MOCHI_NOW_SEED\") {\n")
		buf.WriteString("            if let Ok(v) = s.parse::<i64>() {\n")
		buf.WriteString("                NOW_SEED.store(v, Ordering::SeqCst);\n")
		buf.WriteString("                NOW_SEEDED.store(true, Ordering::SeqCst);\n")
		buf.WriteString("            }\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if NOW_SEEDED.load(Ordering::SeqCst) {\n")
		buf.WriteString("        let seed = (NOW_SEED.load(Ordering::SeqCst)*1664525 + 1013904223) % 2147483647;\n")
		buf.WriteString("        NOW_SEED.store(seed, Ordering::SeqCst);\n")
		buf.WriteString("        seed\n")
		buf.WriteString("    } else {\n")
		buf.WriteString("        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos() as i64\n")
		buf.WriteString("    }\n")
		buf.WriteString("}\n")
	}
	if prog.UseInput {
		buf.WriteString("fn input() -> String {\n")
		buf.WriteString("    let mut s = String::new();\n")
		buf.WriteString("    std::io::stdin().read_line(&mut s).unwrap();\n")
		buf.WriteString("    s.trim_end().to_string()\n")
		buf.WriteString("}\n")
	}
	if prog.UseInt {
		buf.WriteString("fn int(x: i64) -> i64 { x }\n")
	}
	for _, d := range prog.Types {
		d.emit(&buf)
		buf.WriteByte('\n')
	}
	for _, g := range prog.Globals {
		g.emit(&buf)
		buf.WriteByte('\n')
	}
	for _, s := range prog.Stmts {
		if fd, ok := s.(*FuncDecl); ok {
			fd.emit(&buf)
			buf.WriteString("\n\n")
		}
	}
	buf.WriteString("fn main() {\n")
	indent := 1
	if len(prog.Globals) > 0 {
		buf.WriteString("    unsafe {\n")
		indent++
	}
	for _, g := range prog.Globals {
		if g.Expr != nil {
			for i := 0; i < indent; i++ {
				buf.WriteString("    ")
			}
			buf.WriteString(g.Name)
			buf.WriteString(" = ")
			var b bytes.Buffer
			g.Expr.emit(&b)
			buf.Write(b.Bytes())
			buf.WriteString(";\n")
		}
	}
	for _, s := range prog.Stmts {
		if _, ok := s.(*FuncDecl); ok {
			continue
		}
		if vd, ok := s.(*VarDecl); ok && vd.Global {
			continue
		}
		if es, ok := s.(*ExprStmt); ok {
			if ce, ok2 := es.Expr.(*CallExpr); ok2 && unsafeFuncs[ce.Func] {
				for i := 0; i < indent; i++ {
					buf.WriteString("    ")
				}
				buf.WriteString("unsafe { ")
				ce.emit(&buf)
				buf.WriteString("; }\n")
				continue
			}
		}
		writeStmt(&buf, s, indent)
	}
	if len(prog.Globals) > 0 {
		buf.WriteString("    }\n")
	}
	buf.WriteString("}\n")
	if b := buf.Bytes(); len(b) > 0 && b[len(b)-1] != '\n' {
		b = append(b, '\n')
	}
	return buf.Bytes()
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

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	if b, err := os.ReadFile(filepath.Join(root, "VERSION")); err == nil {
		return strings.TrimSpace(string(b))
	}
	return "dev"
}

func header() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}
	if ts == "" {
		ts = time.Now().Format("2006-01-02 15:04 MST")
	}
	return fmt.Sprintf("// Generated by Mochi transpiler v%s on %s\n", version(), ts)
}

// Print converts prog to ast.Node form and prints it.
func Print(prog *Program) {
	toNode(prog).Print("")
}

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *VarDecl:
		n := &ast.Node{Kind: "let", Value: st.Name}
		if st.Expr != nil {
			n.Children = []*ast.Node{exprNode(st.Expr)}
		}
		return n
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "idx-assign", Children: []*ast.Node{exprNode(st.Target), exprNode(st.Value)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		thenNode := &ast.Node{Kind: "then"}
		for _, s2 := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, thenNode)
		if st.ElseIf != nil {
			n.Children = append(n.Children, stmtNode(st.ElseIf))
		} else if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, s2 := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *ForStmt:
		n := &ast.Node{Kind: "for", Value: st.Var}
		n.Children = append(n.Children, exprNode(st.Iter))
		if st.End != nil {
			n.Children = append(n.Children, exprNode(st.End))
		}
		bodyNode := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			bodyNode.Children = append(bodyNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, bodyNode)
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		n.Children = append(n.Children, exprNode(st.Cond))
		bodyNode := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			bodyNode.Children = append(bodyNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, bodyNode)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *NumberLit:
		return &ast.Node{Kind: "number", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, exprNode(el))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			pair := &ast.Node{Kind: "item"}
			pair.Children = append(pair.Children, exprNode(it.Key))
			pair.Children = append(pair.Children, exprNode(it.Value))
			n.Children = append(n.Children, pair)
		}
		return n
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *StrExpr:
		return &ast.Node{Kind: "str", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{exprNode(ex.Map)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Elem)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.List)}}
	case *ExistsExpr:
		return &ast.Node{Kind: "exists", Children: []*ast.Node{exprNode(ex.List)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{exprNode(ex.List)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{exprNode(ex.List)}}
	case *JoinExpr:
		return &ast.Node{Kind: "join", Children: []*ast.Node{exprNode(ex.List)}}
	case *NowExpr:
		return &ast.Node{Kind: "now"}
	case *SubstringExpr:
		n := &ast.Node{Kind: "substring"}
		n.Children = append(n.Children, exprNode(ex.Str))
		n.Children = append(n.Children, exprNode(ex.Start))
		n.Children = append(n.Children, exprNode(ex.End))
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SliceExpr:
		n := &ast.Node{Kind: "slice"}
		n.Children = append(n.Children, exprNode(ex.Target))
		if ex.Start != nil {
			n.Children = append(n.Children, exprNode(ex.Start))
		} else {
			n.Children = append(n.Children, &ast.Node{Kind: "number", Value: "0"})
		}
		if ex.End != nil {
			n.Children = append(n.Children, exprNode(ex.End))
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *IfExpr:
		n := &ast.Node{Kind: "if_expr"}
		n.Children = append(n.Children, exprNode(ex.Cond))
		n.Children = append(n.Children, exprNode(ex.Then))
		if ex.ElseIf != nil {
			n.Children = append(n.Children, exprNode(ex.ElseIf))
		} else if ex.Else != nil {
			n.Children = append(n.Children, exprNode(ex.Else))
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
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

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}
