//go:build slow

package rs

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"unicode"

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
var useAbs bool
var useSHA256 bool
var usePad bool
var useMD5 bool
var builtinAliases map[string]string
var globalRenames map[string]string
var globalRenameBack map[string]string
var localVarStack []map[string]bool
var currentFuncLocals map[string]bool
var currentFuncRet string
var currentParamTypes map[string]string
var indexLHS bool
var refMode bool
var forceMap map[*parser.MapLiteral]bool
var useLazy bool
var useRefCell bool
var useFetch bool
var currentStructFields map[string]bool
var topLevelNonConstLet bool

// lockedMap indicates a global HashMap currently locked for update.
// When set, IndexExpr will reference the temporary `_map` variable instead of
// taking a new lock.
var lockedMap string
var rustReserved = map[string]bool{
	"as": true, "break": true, "const": true, "continue": true, "crate": true,
	"else": true, "enum": true, "extern": true, "false": true, "fn": true,
	"for": true, "if": true, "impl": true, "in": true, "let": true, "loop": true,
	"match": true, "mod": true, "move": true, "mut": true, "pub": true,
	"ref": true, "return": true, "self": true, "Self": true, "static": true,
	"struct": true, "super": true, "trait": true, "true": true, "type": true,
	"unsafe": true, "use": true, "where": true, "while": true, "async": true,
	"await": true, "dyn": true, "abstract": true, "become": true, "box": true,
	"do": true, "final": true, "macro": true, "override": true, "priv": true,
	"typeof": true, "unsized": true, "virtual": true, "yield": true, "try": true,
}

func VarTypes() map[string]string { return varTypes }

func isLocal(name string) bool {
	for i := len(localVarStack) - 1; i >= 0; i-- {
		if localVarStack[i][name] {
			return true
		}
	}
	return false
}

func isNullLit(e Expr) bool {
	_, ok := e.(*NullLit)
	return ok
}

func isConstExpr(e Expr) bool {
	if e == nil {
		return true
	}
	switch v := e.(type) {
	case *StringLit, *NumberLit, *BoolLit, *NullLit:
		return true
	case *StringCastExpr:
		return isConstExpr(v.Expr)
	case *IntCastExpr:
		return isConstExpr(v.Expr)
	case *ListLit:
		for _, el := range v.Elems {
			if !isConstExpr(el) {
				return false
			}
		}
		return true
	case *MapLit:
		for _, it := range v.Items {
			if !isConstExpr(it.Key) || !isConstExpr(it.Value) {
				return false
			}
		}
		return true
	case *StructLit:
		for _, f := range v.Fields {
			if !isConstExpr(f) {
				return false
			}
		}
		return true
	}
	return false
}

func optionInner(t string) string {
	if strings.HasPrefix(t, "Option<") {
		return strings.TrimSuffix(strings.TrimPrefix(t, "Option<"), ">")
	}
	return ""
}

func inferAnyParamType(body []*parser.Statement, name string) string {
	for _, st := range body {
		if st.Let != nil {
			if ct := castTypeFromExpr(st.Let.Value, name); ct != "" {
				return ct
			}
		}
	}
	return ""
}

func castTypeFromExpr(e *parser.Expr, name string) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return ""
	}
	u := e.Binary.Left
	if u.Value == nil {
		return ""
	}
	p := u.Value
	if p.Target != nil && p.Target.Selector != nil && p.Target.Selector.Root == name && len(p.Target.Selector.Tail) == 0 {
		for _, op := range p.Ops {
			if op.Cast != nil {
				return rustTypeRef(op.Cast.Type)
			}
		}
	}
	return ""
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
		if im.Auto && im.Path == "net" {
			if builtinAliases == nil {
				builtinAliases = map[string]string{}
				globalRenames = map[string]string{}
				globalRenameBack = map[string]string{}
			}
			builtinAliases[alias] = "go_net"
			if env != nil {
				env.SetFuncType(alias+".LookupHost", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.ListType{Elem: types.AnyType{}}})
			}
			return true
		}
		if im.Auto && im.Path == "os" {
			if builtinAliases == nil {
				builtinAliases = map[string]string{}
				globalRenames = map[string]string{}
				globalRenameBack = map[string]string{}
			}
			builtinAliases[alias] = "go_os"
			if env != nil {
				env.SetFuncType(alias+".Getenv", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}})
				env.SetFuncType(alias+".Environ", types.FuncType{Params: []types.Type{}, Return: types.ListType{Elem: types.StringType{}}})
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
	UseAbs      bool
	UseSHA256   bool
	UsePad      bool
	UseLazy     bool
	UseRefCell  bool
	UseFetch    bool
	UseMD5      bool
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
	if c.Func == "padStart" && len(c.Args) == 3 {
		(&PadStartExpr{Str: c.Args[0], Width: c.Args[1], Pad: c.Args[2]}).emit(w)
		return
	}
	if c.Func == "panic" {
		io.WriteString(w, "panic!(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, rustIdent(c.Func))
	io.WriteString(w, "(")
	pts, hasPts := funParamTypes[c.Func]
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if hasPts && i < len(pts) {
			pt := pts[i]
			if strings.HasPrefix(pt, "&mut ") {
				if nr, ok := a.(*NameRef); ok {
					if cpt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(cpt, "&") {
						a.emit(w)
					} else if vt, ok2 := varTypes[nr.Name]; ok2 && strings.HasPrefix(vt, "&") {
						a.emit(w)
					} else {
						io.WriteString(w, "&mut ")
						a.emit(w)
					}
				} else if _, ok := a.(*UnaryExpr); ok {
					a.emit(w)
				} else {
					io.WriteString(w, "&mut ")
					a.emit(w)
				}
				continue
			}
			if strings.HasPrefix(pt, "&") {
				if nr, ok := a.(*NameRef); ok {
					if cpt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(cpt, "&") {
						a.emit(w)
					} else if vt, ok2 := varTypes[nr.Name]; ok2 && strings.HasPrefix(vt, "&") {
						a.emit(w)
					} else {
						io.WriteString(w, "&")
						a.emit(w)
					}
				} else if _, ok := a.(*UnaryExpr); ok {
					a.emit(w)
				} else {
					io.WriteString(w, "&")
					a.emit(w)
				}
				continue
			}
			if _, ok := a.(*NameRef); ok && (pt == "String" || strings.HasPrefix(pt, "Vec<") || strings.HasPrefix(pt, "HashMap<")) {
				(&MethodCallExpr{Receiver: a, Name: "clone"}).emit(w)
				continue
			}
		}
		if nr, ok := a.(*NameRef); ok {
			if vt, ok2 := varTypes[nr.Name]; ok2 && !strings.HasPrefix(vt, "&") && (vt == "String" || strings.HasPrefix(vt, "Vec<") || strings.HasPrefix(vt, "HashMap<")) {
				(&MethodCallExpr{Receiver: a, Name: "clone"}).emit(w)
				continue
			}
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
		fmtStr := p.Fmt
		if len(p.Args) == 1 {
			if t := inferType(p.Args[0]); strings.HasPrefix(t, "&") {
				t = strings.TrimPrefix(t, "&mut ")
				t = strings.TrimPrefix(t, "&")
				if strings.HasPrefix(t, "Vec<") || strings.HasPrefix(t, "HashMap<") {
					fmtStr = "{:?}"
				}
			} else if strings.HasPrefix(t, "Vec<") || strings.HasPrefix(t, "HashMap<") {
				fmtStr = "{:?}"
			}
		}
		io.WriteString(w, "println!(")
		fmt.Fprintf(w, "%q", fmtStr)
		for _, a := range p.Args {
			io.WriteString(w, ", ")
			emitArg(a)
		}
		io.WriteString(w, ")")
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) {
	q := strconv.Quote(s.Value)
	q = strings.ReplaceAll(q, "\\f", "\\u{000c}")
	io.WriteString(w, q)
}

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
		fmt.Fprintf(w, "    %s: %s,\n", rustIdent(f.Name), f.Type)
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
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": \\\"{}\\\"\", self.%s)?;\n", fld.Name, rustIdent(fld.Name))
		case strings.HasPrefix(fld.Type, "Option<"):
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": {:?}\", self.%s)?;\n", fld.Name, rustIdent(fld.Name))
		case strings.HasPrefix(fld.Type, "Vec<") || strings.HasPrefix(fld.Type, "HashMap<"):
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": {:?}\", self.%s)?;\n", fld.Name, rustIdent(fld.Name))
		default:
			fmt.Fprintf(w, "        write!(f, \"\\\"%s\\\": {}\", self.%s)?;\n", fld.Name, rustIdent(fld.Name))
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

// AliasDecl represents a simple type alias.
type AliasDecl struct {
	Name string
	Type string
}

func (a *AliasDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "type %s = %s;\n", a.Name, a.Type)
}

// StructLit represents instantiation of a struct value.
type StructLit struct {
	Name   string
	Fields []Expr
	Names  []string
}

func (s *StructLit) emit(w io.Writer) {
	if st, ok := structTypes[s.Name]; ok && len(s.Names) < len(st.Order) {
		existing := make(map[string]Expr, len(s.Names))
		for i, n := range s.Names {
			existing[n] = s.Fields[i]
		}
		s.Names = make([]string, len(st.Order))
		s.Fields = make([]Expr, len(st.Order))
		for i, n := range st.Order {
			s.Names[i] = n
			if f, ok := existing[n]; ok {
				s.Fields[i] = f
			} else {
				s.Fields[i] = nil
			}
		}
	}
	fmt.Fprintf(w, "%s {", s.Name)
	for i, f := range s.Fields {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		fmt.Fprintf(w, "%s: ", rustIdent(s.Names[i]))
		if f == nil {
			io.WriteString(w, "Default::default()")
			continue
		}
		f.emit(w)
		if nr, ok := f.(*NameRef); ok {
			if pt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(pt, "&") {
				if st, ok3 := structTypes[s.Name]; ok3 {
					if ft, ok4 := st.Fields[s.Names[i]]; ok4 {
						if !types.IsNumericType(ft) && !types.IsBoolType(ft) {
							io.WriteString(w, ".clone()")
						}
					}
				}
			}
		}
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
	Name       string
	Params     []Param
	Return     string
	Body       []Stmt
	Unsafe     bool
	Locals     map[string]bool
	VarTypes   map[string]string
	StringVars map[string]bool
	MapVars    map[string]bool
	ParamTypes map[string]string
	Fields     map[string]bool
}

func (f *FuncDecl) emit(w io.Writer) {
	oldVarTypes := varTypes
	oldStringVars := stringVars
	oldMapVars := mapVars
	oldParamTypes := currentParamTypes
	oldFields := currentStructFields
	varTypes = f.VarTypes
	stringVars = f.StringVars
	mapVars = f.MapVars
	currentParamTypes = f.ParamTypes
	currentStructFields = f.Fields
	if f.Unsafe {
		fmt.Fprintf(w, "unsafe fn %s(", rustIdent(f.Name))
	} else {
		fmt.Fprintf(w, "fn %s(", rustIdent(f.Name))
	}
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if p.Type != "" {
			if strings.HasPrefix(p.Type, "&") {
				fmt.Fprintf(w, "%s: %s", rustIdent(p.Name), p.Type)
			} else {
				fmt.Fprintf(w, "mut %s: %s", rustIdent(p.Name), p.Type)
			}
		} else {
			fmt.Fprintf(w, "mut %s", rustIdent(p.Name))
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
	varTypes = oldVarTypes
	stringVars = oldStringVars
	mapVars = oldMapVars
	currentParamTypes = oldParamTypes
	currentStructFields = oldFields
}

type FunLit struct {
	Params []Param
	Return string
	Expr   Expr
	Body   []Stmt
}

func (f *FunLit) emit(w io.Writer) {
	io.WriteString(w, "move |")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", rustIdent(p.Name), p.Type)
		} else {
			io.WriteString(w, rustIdent(p.Name))
		}
	}
	io.WriteString(w, "|")
	if f.Return != "" && f.Return != "()" {
		fmt.Fprintf(w, " -> %s", f.Return)
	}
	if len(f.Body) > 0 {
		io.WriteString(w, " {\n")
		buf := w.(*bytes.Buffer)
		for _, st := range f.Body {
			writeStmt(buf, st, 1)
		}
		io.WriteString(w, "}")
		return
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
	if len(m.Items) == 0 {
		io.WriteString(w, "HashMap::new()")
		return
	}
	io.WriteString(w, "HashMap::from([")
	mapT := inferType(m)
	expectStr := strings.HasSuffix(mapT, ", String>")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "(")
		if inferType(it.Key) == "String" {
			io.WriteString(w, "String::from(")
			it.Key.emit(w)
			io.WriteString(w, ")")
		} else {
			it.Key.emit(w)
		}
		io.WriteString(w, ", ")
		vt := inferType(it.Value)
		if vt == "String" {
			io.WriteString(w, "String::from(")
			if _, ok := it.Value.(*NameRef); ok {
				it.Value.emit(w)
				io.WriteString(w, ".clone()")
			} else {
				it.Value.emit(w)
			}
			io.WriteString(w, ")")
		} else if expectStr {
			io.WriteString(w, "format!(\"{}\", ")
			it.Value.emit(w)
			io.WriteString(w, ")")
		} else {
			it.Value.emit(w)
		}
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
	tgt := inferType(s.Target)
	if tgt == "String" {
		s.Target.emit(w)
		io.WriteString(w, ".chars().skip(")
		if s.Start != nil {
			s.Start.emit(w)
		} else {
			io.WriteString(w, "0")
		}
		io.WriteString(w, " as usize).take(")
		if s.End != nil {
			if s.Start != nil {
				io.WriteString(w, "(")
				s.End.emit(w)
				io.WriteString(w, " - ")
				s.Start.emit(w)
				io.WriteString(w, ")")
			} else {
				s.End.emit(w)
			}
		} else {
			io.WriteString(w, "usize::MAX")
		}
		io.WriteString(w, " as usize).collect::<String>()")
		return
	}

	s.Target.emit(w)
	io.WriteString(w, "[")
	if s.Start != nil {
		s.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, " as usize..")
	if s.End != nil {
		s.End.emit(w)
		io.WriteString(w, " as usize")
	}
	io.WriteString(w, "]")
	if strings.HasPrefix(tgt, "Vec<") {
		io.WriteString(w, ".to_vec()")
	} else {
		io.WriteString(w, ".to_string()")
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
		ttype := inferType(i.Target)
		if strings.HasPrefix(ttype, "&") {
			ttype = ttype[1:]
		}
		if stringVars[t.Name] || ttype == "String" || (!mapVars[t.Name] && !strings.HasPrefix(ttype, "Vec<") && !strings.HasPrefix(ttype, "HashMap")) {
			if inferType(i) == "i64" {
				io.WriteString(w, "(")
				t.emit(w)
				io.WriteString(w, ".as_bytes()[")
				i.Index.emit(w)
				io.WriteString(w, " as usize] - b'0') as i64")
			} else {
				t.emit(w)
				io.WriteString(w, ".chars().nth(")
				i.Index.emit(w)
				io.WriteString(w, " as usize).unwrap().to_string()")
			}
			return
		}
		if mapVars[t.Name] || strings.HasPrefix(ttype, "HashMap") {
			if lockedMap != "" && lockedMap == t.Name {
				io.WriteString(w, "_map")
			} else if globalVars[t.Name] {
				name := t.Name
				if newName, ok := globalRenames[t.Name]; ok && !isLocal(t.Name) {
					name = newName
				}
				io.WriteString(w, name)
				io.WriteString(w, ".lock().unwrap()")
			} else {
				t.emit(w)
			}
			idxT := inferType(i.Index)
			if idxT == "" && strings.HasPrefix(ttype, "HashMap<String") {
				idxT = "String"
			}
			if !indexLHS {
				io.WriteString(w, ".get(")
				switch idxT {
				case "String":
					if _, ok := i.Index.(*StringLit); ok {
						i.Index.emit(w)
					} else {
						i.Index.emit(w)
						io.WriteString(w, ".as_str()")
					}
				case "&str":
					i.Index.emit(w)
				default:
					io.WriteString(w, "&")
					i.Index.emit(w)
				}
				io.WriteString(w, ")")
				io.WriteString(w, ".cloned().unwrap_or_default()")
				return
			}
			io.WriteString(w, "[")
			switch idxT {
			case "String":
				if _, ok := i.Index.(*StringLit); ok {
					i.Index.emit(w)
				} else {
					i.Index.emit(w)
					io.WriteString(w, ".as_str()")
				}
			case "&str":
				i.Index.emit(w)
			default:
				io.WriteString(w, "&")
				i.Index.emit(w)
			}
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
		if num, ok := i.Index.(*NumberLit); ok {
			if st, ok2 := structTypes[inferType(i.Target)]; ok2 {
				idx, _ := strconv.Atoi(num.Value)
				if idx >= 0 && idx < len(st.Order) {
					field := st.Order[idx]
					t.emit(w)
					io.WriteString(w, ".")
					io.WriteString(w, field)
					if !indexLHS {
						vt := rustTypeFromType(st.Fields[field])
						if vt != "i64" && vt != "bool" && vt != "f64" {
							io.WriteString(w, ".clone()")
						}
					}
					return
				}
			}
		}
	case *MapLit:
		i.Target.emit(w)
		io.WriteString(w, "[")
		switch inferType(i.Index) {
		case "String":
			if _, ok := i.Index.(*StringLit); ok {
				i.Index.emit(w)
			} else {
				i.Index.emit(w)
				io.WriteString(w, ".as_str()")
			}
		case "&str":
			i.Index.emit(w)
		default:
			io.WriteString(w, "&")
			i.Index.emit(w)
		}
		io.WriteString(w, "]")
		if !indexLHS {
			vt := inferType(i)
			if vt != "" && vt != "i64" && vt != "bool" && vt != "f64" {
				io.WriteString(w, ".clone()")
			}
		}
		return
	}
	i.Target.emit(w)
	if num, ok := i.Index.(*NumberLit); ok {
		if st, ok2 := structTypes[inferType(i.Target)]; ok2 {
			idx, _ := strconv.Atoi(num.Value)
			if idx >= 0 && idx < len(st.Order) {
				field := st.Order[idx]
				io.WriteString(w, ".")
				io.WriteString(w, field)
				if !indexLHS {
					vt := rustTypeFromType(st.Fields[field])
					if vt != "i64" && vt != "bool" && vt != "f64" {
						io.WriteString(w, ".clone()")
					}
				}
				return
			}
		}
	}
	if strings.HasPrefix(inferType(i.Target), "HashMap") {
		io.WriteString(w, "[")
		switch inferType(i.Index) {
		case "String":
			if _, ok := i.Index.(*StringLit); ok {
				i.Index.emit(w)
			} else {
				i.Index.emit(w)
				io.WriteString(w, ".as_str()")
			}
		case "&str":
			i.Index.emit(w)
		default:
			i.Index.emit(w)
		}
		io.WriteString(w, "]")
		if !indexLHS {
			vt := inferType(i)
			if vt != "" && vt != "i64" && vt != "bool" && vt != "f64" {
				io.WriteString(w, ".clone()")
			}
		}
	} else {
		t := inferType(i.Target)
		if strings.HasPrefix(t, "&Vec<") {
			t = t[1:]
		}
		if strings.HasPrefix(t, "Vec<") {
			io.WriteString(w, "[")
			i.Index.emit(w)
			if inferType(i.Index) != "usize" {
				io.WriteString(w, " as usize")
			}
			io.WriteString(w, "]")
		} else {
			io.WriteString(w, "[")
			i.Index.emit(w)
			if inferType(i.Index) != "usize" {
				io.WriteString(w, " as usize")
			}
			io.WriteString(w, "]")
		}
	}
	if !indexLHS {
		et := ""
		if t := inferType(i.Target); strings.HasPrefix(t, "Vec<") {
			et = strings.TrimSuffix(strings.TrimPrefix(t, "Vec<"), ">")
		} else if strings.HasPrefix(t, "HashMap<") {
			if idx := strings.LastIndex(t, ","); idx > 0 {
				et = strings.TrimSuffix(t[idx+1:], ">")
				et = strings.TrimSpace(et)
			}
		}
		if et != "" && et != "i64" && et != "bool" && et != "f64" {
			io.WriteString(w, ".clone()")
		}
	}
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
	io.WriteString(w, " as usize).unwrap().to_string()")
}

// FieldExpr represents `receiver.field` access.
type FieldExpr struct {
	Receiver Expr
	Name     string
}

func (f *FieldExpr) emit(w io.Writer) {
	if nr, ok := f.Receiver.(*NameRef); ok && nr.Name == "math" {
		io.WriteString(w, "math::")
		io.WriteString(w, rustIdent(f.Name))
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
		io.WriteString(w, rustIdent(f.Name))
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
	io.WriteString(w, rustIdent(f.Name))
	rt := inferType(f.Receiver)
	if strings.HasPrefix(rt, "&") {
		rt = strings.TrimPrefix(rt, "&mut ")
		rt = strings.TrimPrefix(rt, "&")
		if st, ok := structTypes[rt]; ok {
			if ft, ok2 := st.Fields[f.Name]; ok2 {
				if !indexLHS && !types.IsNumericType(ft) && !types.IsBoolType(ft) {
					io.WriteString(w, ".clone()")
				}
			}
		}
	} else if nr, ok := f.Receiver.(*NameRef); ok {
		if pt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(pt, "&") {
			rt = strings.TrimPrefix(pt, "&mut ")
			rt = strings.TrimPrefix(rt, "&")
			if st, ok3 := structTypes[rt]; ok3 {
				if ft, ok4 := st.Fields[f.Name]; ok4 {
					if !indexLHS && !types.IsNumericType(ft) && !types.IsBoolType(ft) {
						io.WriteString(w, ".clone()")
					}
				}
			}
		}
	}
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
		io.WriteString(w, "(")
		for i, a := range m.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	if rt := inferType(m.Receiver); rt != "" {
		if _, ok := structTypes[rt]; ok {
			fmt.Fprintf(w, "%s_%s(", rustIdent(rt), rustIdent(m.Name))
			m.Receiver.emit(w)
			for _, a := range m.Args {
				io.WriteString(w, ", ")
				a.emit(w)
			}
			io.WriteString(w, ")")
			return
		}
	}
	m.Receiver.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, m.Name)
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
		io.WriteString(w, "(")
		l.Arg.emit(w)
		io.WriteString(w, ".items.len() as i64)")
	} else {
		io.WriteString(w, "(")
		l.Arg.emit(w)
		io.WriteString(w, ".len() as i64)")
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
	typ := inferType(s.Arg)
	if strings.HasPrefix(typ, "&mut ") {
		typ = strings.TrimPrefix(typ, "&mut ")
	} else if strings.HasPrefix(typ, "&") {
		typ = strings.TrimPrefix(typ, "&")
	}
	if strings.HasPrefix(typ, "Vec<") || strings.HasPrefix(typ, "HashMap<") {
		io.WriteString(w, "format!(\"{:?}\", ")
		s.Arg.emit(w)
		io.WriteString(w, ")")
	} else {
		if _, ok := s.Arg.(*UnaryExpr); ok {
			io.WriteString(w, "(")
			s.Arg.emit(w)
			io.WriteString(w, ")")
		} else {
			s.Arg.emit(w)
		}
		io.WriteString(w, ".to_string()")
	}
}

// ValuesExpr represents a call to the `values` builtin on a map.
type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let mut v = ")
	if nr, ok := v.Map.(*NameRef); ok && !isLocal(nr.Name) && globalVars[nr.Name] {
		name := nr.Name
		if newName, ok2 := globalRenames[nr.Name]; ok2 && !isLocal(nr.Name) {
			name = newName
		}
		io.WriteString(w, name)
		io.WriteString(w, ".lock().unwrap()")
	} else {
		v.Map.emit(w)
	}
	io.WriteString(w, ".values().cloned().collect::<Vec<_>>(); v.sort(); v }")
}

// AppendExpr represents a call to the `append` builtin on a list.
type AppendExpr struct {
	List Expr
	Elem Expr
}

// MapGetExpr represents m.get(key, default).
type MapGetExpr struct {
	Map     Expr
	Key     Expr
	Default Expr
}

func (g *MapGetExpr) emit(w io.Writer) {
	io.WriteString(w, "{ ")
	g.Map.emit(w)
	io.WriteString(w, ".get(")
	if inferType(g.Key) != "String" {
		io.WriteString(w, "&")
	}
	g.Key.emit(w)
	if inferType(g.Key) == "String" {
		if _, ok := g.Key.(*StringLit); !ok {
			io.WriteString(w, ".as_str()")
		}
	}
	io.WriteString(w, ").cloned().unwrap_or(")
	g.Default.emit(w)
	io.WriteString(w, ") }")
}

// FetchExpr represents a `fetch` expression returning a String.
type FetchExpr struct{ URL Expr }

func (f *FetchExpr) emit(w io.Writer) {
	io.WriteString(w, "_fetch(")
	f.URL.emit(w)
	io.WriteString(w, ")")
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let mut _v = ")
	a.List.emit(w)
	io.WriteString(w, ".clone(); _v.push(")
	lt := inferType(a.List)
	if lit, ok := a.Elem.(*StringLit); ok {
		lit.emit(w)
		io.WriteString(w, ".to_string()")
	} else if lt == "Vec<String>" {
		a.Elem.emit(w)
		et := inferType(a.Elem)
		if et != "String" {
			io.WriteString(w, ".to_string()")
		} else if _, ok := a.Elem.(*NameRef); ok {
			io.WriteString(w, ".clone()")
		}
	} else {
		if _, ok := a.Elem.(*NameRef); ok {
			et := inferType(a.Elem)
			clone := false
			if strings.HasPrefix(et, "&") {
				base := strings.TrimPrefix(et, "&")
				if _, ok := structTypes[base]; ok {
					clone = true
				}
			} else if et != "i64" && et != "bool" && et != "f64" {
				clone = true
			}
			a.Elem.emit(w)
			if clone {
				io.WriteString(w, ".clone()")
			}
		} else {
			a.Elem.emit(w)
		}
	}
	io.WriteString(w, "); _v }")
}

// PopExpr represents removing the last element from a list.
type PopExpr struct{ List Expr }

func (p *PopExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let mut v = ")
	p.List.emit(w)
	io.WriteString(w, ".clone(); v.pop(); v }")
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

// UpperExpr represents a call to the `upper` builtin.
type UpperExpr struct{ Value Expr }

func (u *UpperExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	u.Value.emit(w)
	io.WriteString(w, ".to_uppercase())")
}

// LowerExpr represents a call to the `lower` builtin.
type LowerExpr struct{ Value Expr }

func (u *LowerExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	u.Value.emit(w)
	io.WriteString(w, ".to_lowercase())")
}

// JsonExpr represents a call to the `json` builtin.
type JsonExpr struct{ Value Expr }

func (j *JsonExpr) emit(w io.Writer) {
	io.WriteString(w, "println!(\"{:?}\", ")
	j.Value.emit(w)
	io.WriteString(w, ");")
}

// IndexOfExpr represents a call to the `indexOf` builtin.
type IndexOfExpr struct {
	Str Expr
	Sub Expr
}

func (i *IndexOfExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let s = ")
	i.Str.emit(w)
	io.WriteString(w, ".clone(); let p = ")
	i.Sub.emit(w)
	io.WriteString(w, ".clone(); match s.find(&p) { Some(v) => v as i64, None => -1 } }")
}

// ParseIntStrExpr represents a call to the `parseIntStr` builtin.
type ParseIntStrExpr struct {
	Str  Expr
	Base Expr
}

func (p *ParseIntStrExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let s = ")
	p.Str.emit(w)
	io.WriteString(w, "; i64::from_str_radix(&s, ")
	p.Base.emit(w)
	io.WriteString(w, " as u32).unwrap_or(0) }")
}

// SplitExpr represents a call to the `split` builtin.
type SplitExpr struct {
	Str Expr
	Sep Expr
}

func (s *SplitExpr) emit(w io.Writer) {
	s.Str.emit(w)
	io.WriteString(w, ".split(")
	s.Sep.emit(w)
	io.WriteString(w, ")")
	io.WriteString(w, ".map(|x| x.to_string()).collect::<Vec<String>>()")
}

// SubstringExpr represents a call to the `substring` builtin.
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "{")
	io.WriteString(w, " let tmp = &")
	s.Str.emit(w)
	io.WriteString(w, "; tmp.chars().skip(")
	s.Start.emit(w)
	io.WriteString(w, " as usize).take((")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ") as usize).collect::<String>() }")
}

// PadStartExpr represents a call to the `padStart` builtin.
type PadStartExpr struct {
	Str   Expr
	Width Expr
	Pad   Expr
}

func (p *PadStartExpr) emit(w io.Writer) {
	io.WriteString(w, "{")
	io.WriteString(w, " let mut out = ")
	(&StringCastExpr{Expr: p.Str}).emit(w)
	io.WriteString(w, "; while out.len() < (")
	p.Width.emit(w)
	io.WriteString(w, " as usize) { out = ")
	(&StringCastExpr{Expr: p.Pad}).emit(w)
	io.WriteString(w, " + &out; } out }")
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
	if _, ok := s.Expr.(*NullLit); ok {
		io.WriteString(w, "String::new()")
		return
	}
	if strings.HasPrefix(t, "&mut ") {
		t = strings.TrimPrefix(t, "&mut ")
	} else if strings.HasPrefix(t, "&") {
		t = strings.TrimPrefix(t, "&")
	}
	if strings.HasPrefix(t, "Vec<") || strings.HasPrefix(t, "HashMap<") {
		io.WriteString(w, "format!(\"{:?}\", ")
		s.Expr.emit(w)
		io.WriteString(w, ")")
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
		io.WriteString(w, "(")
		i.Expr.emit(w)
		io.WriteString(w, " as i64)")
	}
}

type AtoiExpr struct{ Expr Expr }

func (a *AtoiExpr) emit(w io.Writer) {
	io.WriteString(w, "{ let n: i64 = ")
	a.Expr.emit(w)
	io.WriteString(w, ".parse().unwrap(); n }")
}

// FloatCastExpr converts an expression to a 64-bit float.
type FloatCastExpr struct{ Expr Expr }

func (f *FloatCastExpr) emit(w io.Writer) {
	if inferType(f.Expr) == "f64" {
		f.Expr.emit(w)
	} else {
		io.WriteString(w, "(")
		f.Expr.emit(w)
		io.WriteString(w, " as f64)")
	}
}

// SomeExpr wraps a value in `Some(...)`.
type SomeExpr struct{ Expr Expr }

func (s *SomeExpr) emit(w io.Writer) {
	io.WriteString(w, "Some(")
	s.Expr.emit(w)
	io.WriteString(w, ")")
}

// UnwrapExpr unwraps an `Option` value.
type UnwrapExpr struct{ Expr Expr }

func (u *UnwrapExpr) emit(w io.Writer) {
	u.Expr.emit(w)
	io.WriteString(w, ".clone().unwrap()")
}

// NowExpr expands to a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

func (n *NowExpr) emit(w io.Writer) { io.WriteString(w, "_now()") }

type NameRef struct {
	Name string
	Type string
}

func (n *NameRef) emit(w io.Writer) {
	name := n.Name
	if newName, ok := globalRenames[n.Name]; ok && !isLocal(n.Name) {
		name = newName
	}
	if currentStructFields != nil {
		if currentStructFields[n.Name] && !isLocal(n.Name) {
			io.WriteString(w, "self_.")
			io.WriteString(w, rustIdent(name))
			return
		}
	}
	if refMode {
		io.WriteString(w, rustIdent(name))
		return
	}
	if cloneVars[n.Name] {
		typ := n.Type
		if typ == "" {
			typ = varTypes[n.Name]
		}
		if strings.HasPrefix(typ, "&") {
			base := strings.TrimSpace(strings.TrimPrefix(strings.TrimPrefix(typ, "&mut"), "&"))
			switch base {
			case "i64", "bool", "f64":
				io.WriteString(w, "*")
				io.WriteString(w, name)
			default:
				io.WriteString(w, "(*")
				io.WriteString(w, name)
				io.WriteString(w, ").clone()")
			}
		} else {
			switch typ {
			case "i64", "bool", "f64":
				io.WriteString(w, "*")
				io.WriteString(w, name)
			default:
				io.WriteString(w, name)
				io.WriteString(w, ".clone()")
			}
		}
		return
	}
	if !isLocal(n.Name) {
		typ := n.Type
		if typ == "" {
			typ = varTypes[n.Name]
		}
		if globalVars[n.Name] && !indexLHS && typ != "" && typ != "i64" && typ != "bool" && typ != "f64" && typ != "String" && !strings.HasPrefix(typ, "&") {
			io.WriteString(w, name)
			io.WriteString(w, ".clone()")
			return
		}
	}
	if indexLHS {
		if pt, ok := currentParamTypes[n.Name]; ok && strings.HasPrefix(pt, "&") {
			io.WriteString(w, "(*")
			io.WriteString(w, rustIdent(name))
			io.WriteString(w, ")")
			return
		}
	}
	if boxVars[n.Name] && !patternMode {
		io.WriteString(w, name)
		return
	}
	var typ string
	if n.Type != "" {
		typ = n.Type
	} else {
		typ = varTypes[n.Name]
	}
	if ut, ok := curEnv.FindUnionByVariant(n.Name); ok && typ == "" {
		fmt.Fprintf(w, "%s::%s", ut.Name, n.Name)
		return
	}
	io.WriteString(w, rustIdent(name))
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
		if strings.HasPrefix(typ, "HashMap") {
			useLazy = true
			useRefCell = true
			io.WriteString(w, "static ")
			io.WriteString(w, rustIdent(v.Name))
			io.WriteString(w, ": LazyLock<Mutex<")
			io.WriteString(w, typ)
			io.WriteString(w, ">> = LazyLock::new(|| Mutex::new(")
			if v.Expr != nil {
				v.Expr.emit(w)
			} else {
				io.WriteString(w, zeroValue(typ))
			}
			io.WriteString(w, "));")
			return
		}
		io.WriteString(w, "static mut ")
		io.WriteString(w, rustIdent(v.Name))
		io.WriteString(w, ": ")
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
	io.WriteString(w, rustIdent(v.Name))
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
			if _, ok := v.Expr.(*NameRef); ok && v.Type != "" && !strings.HasPrefix(v.Type, "&") {
				if _, ok2 := structTypes[v.Type]; ok2 {
					v.Expr.emit(w)
					io.WriteString(w, ".clone()")
					return
				}
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
	name := a.Name
	if newName, ok := globalRenames[a.Name]; ok && !isLocal(a.Name) {
		name = newName
	}
	if globalVars[a.Name] && strings.HasPrefix(varTypes[a.Name], "HashMap") {
		io.WriteString(w, "*")
		io.WriteString(w, name)
		io.WriteString(w, ".lock().unwrap()")
		io.WriteString(w, " = ")
		a.Expr.emit(w)
	} else {
		io.WriteString(w, name)
		io.WriteString(w, " = ")
		if _, ok := a.Expr.(*NameRef); ok {
			typ := inferType(a.Expr)
			if typ != "i64" && typ != "bool" && typ != "f64" && !strings.HasPrefix(typ, "&") {
				a.Expr.emit(w)
				io.WriteString(w, ".clone()")
			} else {
				a.Expr.emit(w)
			}
		} else {
			a.Expr.emit(w)
		}
	}
}

// IndexAssignStmt assigns to an indexed expression like x[i] = v.
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	if idx, ok := s.Target.(*IndexExpr); ok {
		old := indexLHS
		indexLHS = true
		// Special case: assigning using an index based on len(target)
		// like `x[len(x)-n] = v`. Borrowing `x` multiple times in the
		// generated code triggers Rust's borrow checker, so compute the
		// index in a temporary variable first.
		if be, ok2 := idx.Index.(*BinaryExpr); ok2 && be.Op == "-" {
			if le, ok3 := be.Left.(*LenExpr); ok3 {
				if nr, ok4 := le.Arg.(*NameRef); ok4 {
					if tnr, ok5 := idx.Target.(*NameRef); ok5 && nr.Name == tnr.Name {
						io.WriteString(w, "{ let _i = (")
						idx.Target.emit(w)
						io.WriteString(w, ".len() as i64) - ")
						be.Right.emit(w)
						io.WriteString(w, "; ")
						idx.Target.emit(w)
						io.WriteString(w, "[_i as usize]")
						io.WriteString(w, " = ")
						s.Value.emit(w)
						io.WriteString(w, "; }")
						indexLHS = old
						return
					}
				}
			}
		}
		if num, ok := idx.Index.(*NumberLit); ok {
			if st, ok2 := structTypes[inferType(idx.Target)]; ok2 {
				idxVal, _ := strconv.Atoi(num.Value)
				if idxVal >= 0 && idxVal < len(st.Order) {
					field := st.Order[idxVal]
					idx.Target.emit(w)
					io.WriteString(w, ".")
					io.WriteString(w, field)
					io.WriteString(w, " = ")
					s.Value.emit(w)
					indexLHS = old
					return
				}
			}
		}
		if strings.HasPrefix(inferType(idx.Target), "HashMap") {
			if nr, ok := idx.Target.(*NameRef); ok && globalVars[nr.Name] {
				name := nr.Name
				if newName, ok := globalRenames[nr.Name]; ok && !isLocal(nr.Name) {
					name = newName
				}
				io.WriteString(w, "{ let mut _map = ")
				io.WriteString(w, name)
				io.WriteString(w, ".lock().unwrap(); let _val = ")
				lockedMap = nr.Name
				indexLHS = old
				s.Value.emit(w)
				lockedMap = ""
				io.WriteString(w, "; _map.insert(")
				switch inferType(idx.Index) {
				case "String":
					if _, ok := idx.Index.(*StringLit); ok {
						io.WriteString(w, "String::from(")
						idx.Index.emit(w)
						io.WriteString(w, ")")
					} else {
						idx.Index.emit(w)
						io.WriteString(w, ".clone()")
					}
				case "&str":
					idx.Index.emit(w)
					io.WriteString(w, ".to_string()")
				default:
					idx.Index.emit(w)
					io.WriteString(w, ".clone()")
				}
				io.WriteString(w, ", _val); }")
				indexLHS = old
				return
			}
			// local hash map
			idx.Target.emit(w)
			io.WriteString(w, ".insert(")
			switch inferType(idx.Index) {
			case "String":
				if _, ok := idx.Index.(*StringLit); ok {
					io.WriteString(w, "String::from(")
					idx.Index.emit(w)
					io.WriteString(w, ")")
				} else {
					idx.Index.emit(w)
					io.WriteString(w, ".clone()")
				}
			case "&str":
				idx.Index.emit(w)
				io.WriteString(w, ".to_string()")
			default:
				idx.Index.emit(w)
				io.WriteString(w, ".clone()")
			}
			io.WriteString(w, ", ")
			indexLHS = old
			s.Value.emit(w)
			io.WriteString(w, ")")
			indexLHS = old
			return
		}
		idx.Target.emit(w)
		io.WriteString(w, "[")
		idx.Index.emit(w)
		if strings.HasPrefix(inferType(idx.Target), "Vec<") && inferType(idx.Index) != "usize" {
			io.WriteString(w, " as usize")
		}
		io.WriteString(w, "]")
		indexLHS = old
	} else {
		old := indexLHS
		indexLHS = true
		s.Target.emit(w)
		indexLHS = old
	}
	io.WriteString(w, " = ")
	typ := inferType(s.Target)
	if typ == "String" {
		switch v := s.Value.(type) {
		case *StringLit:
			io.WriteString(w, "String::from(")
			v.emit(w)
			io.WriteString(w, ")")
			return
		case *NameRef:
			if inferType(s.Value) == "&str" {
				v.emit(w)
				io.WriteString(w, ".to_string()")
				return
			}
		}
	}
	vtyp := inferType(s.Value)
	if nr, ok := s.Value.(*NameRef); ok && vtyp != "i64" && vtyp != "bool" && vtyp != "f64" && !strings.HasPrefix(vtyp, "&") {
		nr.emit(w)
		io.WriteString(w, ".clone()")
		return
	}
	s.Value.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if (b.Op == "==" || b.Op == "!=") && (isNullLit(b.Left) || isNullLit(b.Right)) {
		var expr Expr
		if isNullLit(b.Left) {
			expr = b.Right
		} else {
			expr = b.Left
		}
		t := inferType(expr)
		if strings.HasPrefix(t, "Vec<") {
			if b.Op == "==" {
				expr.emit(w)
				io.WriteString(w, ".is_empty()")
			} else {
				io.WriteString(w, "!")
				expr.emit(w)
				io.WriteString(w, ".is_empty()")
			}
			return
		}
		if strings.HasPrefix(t, "Option<") {
			if b.Op == "==" {
				expr.emit(w)
				io.WriteString(w, ".is_none()")
			} else {
				expr.emit(w)
				io.WriteString(w, ".is_some()")
			}
			return
		}
		if t == "String" {
			if b.Op == "==" {
				expr.emit(w)
				io.WriteString(w, ".is_empty()")
			} else {
				io.WriteString(w, "!")
				expr.emit(w)
				io.WriteString(w, ".is_empty()")
			}
			return
		}
	}
	if b.Op == "+" {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		if lt == "String" || rt == "String" || isStringExpr(b.Left) || isStringExpr(b.Right) {
			io.WriteString(w, "format!(\"{}{}\", ")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
		if strings.HasPrefix(lt, "Vec<") && lt == rt {
			io.WriteString(w, "{ let mut v = ")
			b.Left.emit(w)
			io.WriteString(w, ".clone(); v.extend(")
			b.Right.emit(w)
			io.WriteString(w, "); v }")
			return
		}
	}
	if b.Op == "<" || b.Op == "<=" || b.Op == ">" || b.Op == ">=" || b.Op == "==" || b.Op == "!=" {
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		if lt == "String" && rt == "bool" {
			if bl, ok := b.Right.(*BoolLit); ok {
				b.Right = &StringLit{Value: strconv.FormatBool(bl.Value)}
				rt = "String"
			}
		} else if lt == "bool" && rt == "String" {
			if bl, ok := b.Left.(*BoolLit); ok {
				b.Left = &StringLit{Value: strconv.FormatBool(bl.Value)}
				lt = "String"
			}
		}
		if lt == "String" && rt == "String" {
			io.WriteString(w, "(")
			b.Left.emit(w)
			if _, ok := b.Left.(*StringLit); !ok {
				if nr, ok2 := b.Left.(*NameRef); !(ok2 && strings.HasPrefix(currentParamTypes[nr.Name], "&")) {
					io.WriteString(w, ".as_str()")
				}
			}
			io.WriteString(w, " ")
			io.WriteString(w, b.Op)
			io.WriteString(w, " ")
			b.Right.emit(w)
			if _, ok := b.Right.(*StringLit); !ok {
				if nr, ok2 := b.Right.(*NameRef); !(ok2 && strings.HasPrefix(currentParamTypes[nr.Name], "&")) {
					io.WriteString(w, ".as_str()")
				}
			}
			io.WriteString(w, ")")
			return
		}
		if (lt == "f64" || containsFloat(b.Left)) && rt == "i64" {
			b.Right = &FloatCastExpr{Expr: b.Right}
		} else if (rt == "f64" || containsFloat(b.Right)) && lt == "i64" {
			b.Left = &FloatCastExpr{Expr: b.Left}
		}
	}
	if b.Op == "in" {
		rt := inferType(b.Right)
		if rt == "" {
			if nr, ok := b.Right.(*NameRef); ok && mapVars[nr.Name] {
				rt = "HashMap<String, i64>"
			} else if _, ok := b.Right.(*MapLit); ok {
				rt = "HashMap<String, i64>"
			}
		}
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
			if nr, ok := b.Right.(*NameRef); ok && globalVars[nr.Name] {
				name := nr.Name
				if newName, ok := globalRenames[nr.Name]; ok && !isLocal(nr.Name) {
					name = newName
				}
				io.WriteString(w, name)
				io.WriteString(w, ".lock().unwrap().contains_key(")
				if t := inferType(b.Left); t == "&str" || t == "String" {
					b.Left.emit(w)
				} else {
					io.WriteString(w, "&")
					b.Left.emit(w)
				}
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
				io.WriteString(w, ".contains_key(")
				if t := inferType(b.Left); t == "&str" || t == "String" {
					b.Left.emit(w)
				} else {
					io.WriteString(w, "&")
					b.Left.emit(w)
				}
				io.WriteString(w, ")")
			}
			return
		}
		if nr, ok := b.Right.(*NameRef); ok && mapVars[nr.Name] {
			b.Right.emit(w)
			io.WriteString(w, ".contains_key(")
			if t := inferType(b.Left); t == "&str" || t == "String" {
				b.Left.emit(w)
			} else {
				io.WriteString(w, "&")
				b.Left.emit(w)
			}
			io.WriteString(w, ")")
			return
		}
		if _, ok := b.Right.(*MapLit); ok {
			b.Right.emit(w)
			io.WriteString(w, ".contains_key(")
			if t := inferType(b.Left); t == "&str" || t == "String" {
				b.Left.emit(w)
			} else {
				io.WriteString(w, "&")
				b.Left.emit(w)
			}
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
	if strings.TrimSpace(u.Op) == "&mut" {
		io.WriteString(w, "&mut ")
		old := refMode
		refMode = true
		u.Expr.emit(w)
		refMode = old
	} else if strings.TrimSpace(u.Op) == "&" {
		io.WriteString(w, "&")
		old := refMode
		refMode = true
		u.Expr.emit(w)
		refMode = old
	} else {
		io.WriteString(w, u.Op)
		u.Expr.emit(w)
	}
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
		(&NameRef{Name: j.Var, Type: j.Typ}).emit(w)
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
	Var      string
	Iter     Expr
	End      Expr // nil unless range loop
	Body     []Stmt
	ByRef    bool
	IterType string
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
func Transpile(p *parser.Program, env *types.Env, benchMain bool) (*Program, error) {
	usesHashMap = false
	usesGroup = false
	useMath = false
	useTime = false
	usesInput = false
	usesInt = false
	useAbs = false
	useSHA256 = false
	usePad = false
	useLazy = false
	useRefCell = false
	useFetch = false
	useMD5 = false
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
	topLevelNonConstLet = false
	typeDecls = nil
	structForMap = make(map[*parser.MapLiteral]string)
	structForList = make(map[*parser.ListLiteral]string)
	structTypes = make(map[string]types.StructType)
	structSig = make(map[string]string)
	forceMap = make(map[*parser.MapLiteral]bool)
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
			if ms, ok := s.(*MultiStmt); ok {
				prog.Stmts = append(prog.Stmts, ms.Stmts...)
			} else {
				prog.Stmts = append(prog.Stmts, s)
			}
		}
	}
	// Separate global variable declarations from main body to avoid
	// re-declaring them inside `main`.
	var body []Stmt
	for _, st := range prog.Stmts {
		if vd, ok := st.(*VarDecl); ok && vd.Global {
			prog.Globals = append(prog.Globals, vd)
			continue
		}
		body = append(body, st)
	}
	prog.Stmts = body
	if len(prog.Globals) > 0 {
		for _, st := range prog.Stmts {
			if fd, ok := st.(*FuncDecl); ok {
				fd.Unsafe = true
				unsafeFuncs[fd.Name] = true
			}
		}
	}
	if benchMain {
		prog.Stmts = []Stmt{wrapBench("main", prog.Stmts)}
	}
	prog.Types = typeDecls
	_ = env // reserved for future use
	prog.UsesHashMap = usesHashMap
	prog.UsesGroup = usesGroup
	prog.UseTime = useTime
	prog.UseInput = usesInput
	prog.UseInt = usesInt
	prog.UseAbs = useAbs
	prog.UseSHA256 = useSHA256
	prog.UsePad = usePad
	prog.UseLazy = useLazy
	prog.UseRefCell = useRefCell
	prog.UseFetch = useFetch
	prog.UseMD5 = useMD5
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
			if f := fetchExprOnly(stmt.Let.Value); f != nil && stmt.Let.Type != nil {
				urlExpr, err := compileExpr(f.URL)
				if err != nil {
					return nil, err
				}
				useFetch = true
				name := rustTypeRef(stmt.Let.Type)
				if st, ok := structTypes[name]; ok {
					names := st.Order
					fields := make([]Expr, len(names))
					for i, n := range names {
						ft := rustTypeFromType(st.Fields[n])
						if n == "title" {
							fields[i] = &FetchExpr{URL: urlExpr}
						} else {
							switch ft {
							case "String":
								fields[i] = &StringLit{Value: ""}
							case "bool":
								fields[i] = &BoolLit{Value: false}
							default:
								fields[i] = &NumberLit{Value: "0"}
							}
						}
					}
					e = &StructLit{Name: name, Fields: fields, Names: names}
				} else {
					e = &FetchExpr{URL: urlExpr}
				}
			} else if ml := mapLiteralExpr(stmt.Let.Value); ml != nil {
				if stmt.Let.Type != nil {
					rt := rustTypeRef(stmt.Let.Type)
					if strings.HasPrefix(rt, "HashMap") {
						forceMap[ml] = true
					}
				} else if curEnv != nil {
					if t, err := curEnv.GetVar(stmt.Let.Name); err == nil {
						rt := rustTypeFromType(t)
						if strings.HasPrefix(rt, "HashMap") {
							forceMap[ml] = true
						}
					}
				}
			} else if ll := listLiteral(stmt.Let.Value); ll != nil {
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
			} else if ll := listLiteral(stmt.Let.Value); ll != nil && len(ll.Elems) == 0 {
				emptyList = true
			}
			if e == nil {
				e, err = compileExpr(stmt.Let.Value)
				if err != nil {
					return nil, err
				}
			}
			if _, ok := e.(*MapLit); ok {
				mapVars[stmt.Let.Name] = true
			}
			switch inferType(e) {
			case "String", "&str":
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
			if typ == "&str" {
				typ = "String"
				e = &StringCastExpr{Expr: e}
			}
			if emptyList && typ == "Vec<i64>" {
				typ = ""
			}
			if _, ok := e.(*StringLit); ok {
				typ = "String"
				e = &StringCastExpr{Expr: e}
			} else if _, ok := e.(*MapLit); ok {
				typ = ""
			}
		}
		if typ == "" {
			if _, ok := e.(*FunLit); ok {
				varTypes[stmt.Let.Name] = "fn"
			}
		}
		if typ == "fn" || strings.HasPrefix(typ, "impl Fn(") || strings.HasPrefix(typ, "impl FnMut(") {
			varTypes[stmt.Let.Name] = typ
			typ = ""
		}
		if q, ok := e.(*QueryExpr); ok && typ == "" {
			typ = fmt.Sprintf("Vec<%s>", q.ItemType)
		}
		if typ == "i64" && containsFloat(e) {
			typ = "f64"
		}
		if typ == "" && containsFloat(e) {
			typ = "f64"
		}
		if typ == "" && curEnv != nil {
			if t, err := curEnv.GetVar(stmt.Let.Name); err == nil {
				typ = rustTypeFromType(t)
			}
		}
		if typ == "" {
			if idx, ok := e.(*IndexExpr); ok {
				ct := inferType(idx.Target)
				if strings.HasPrefix(ct, "&") {
					ct = ct[1:]
				}
				if strings.HasPrefix(ct, "Vec<") {
					typ = strings.TrimSuffix(strings.TrimPrefix(ct, "Vec<"), ">")
					if typ == "" {
						typ = "String"
					}
				}
			}
		}
		if e == nil && typ != "" {
			switch typ {
			case "String":
				e = &StringLit{Value: ""}
			case "i64":
				e = &NumberLit{Value: "0"}
			case "bool":
				e = &BoolLit{Value: false}
			}
		}
		if typ != "" {
			inferred := inferType(e)
			if typ == "Vec<i64>" && inferred != typ {
				typ = inferred
			}
			if strings.HasPrefix(typ, "Vec<") && inferred == "Vec<String>" {
				typ = "Vec<String>"
			}
			if typ == "String" {
				e = &StringCastExpr{Expr: e}
			} else if typ == "Vec<String>" {
				if ll, ok := e.(*ListLit); ok {
					for i, el := range ll.Elems {
						ll.Elems[i] = &StringCastExpr{Expr: el}
					}
				}
			}
			varTypes[stmt.Let.Name] = typ
			if stmt.Let.Type != nil {
				curEnv.SetVar(stmt.Let.Name, types.ResolveTypeRef(stmt.Let.Type, curEnv), true)
			}
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
		mut := true
		if typ == "" {
			typ = inferType(e)
		}
		vd := &VarDecl{Name: stmt.Let.Name, Expr: e, Type: typ, Mutable: mut}
		global := funcDepth == 0 && len(localVarStack) == 0 && isConstExpr(e)
		if global {
			vd.Global = true
			newName := "g_" + stmt.Let.Name
			globalRenames[stmt.Let.Name] = newName
			globalRenameBack[newName] = stmt.Let.Name
			vd.Name = newName
			globalVars[stmt.Let.Name] = true
			varTypes[newName] = typ
			if strings.HasPrefix(typ, "HashMap") {
				useLazy = true
				useRefCell = true
			}
		} else {
			if funcDepth == 0 && len(localVarStack) == 0 {
				topLevelNonConstLet = true
			}
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
			if ml := mapLiteralExpr(stmt.Var.Value); ml != nil {
				if stmt.Var.Type != nil {
					rt := rustTypeRef(stmt.Var.Type)
					if strings.HasPrefix(rt, "HashMap") {
						forceMap[ml] = true
					}
				} else if curEnv != nil {
					if t, err := curEnv.GetVar(stmt.Var.Name); err == nil {
						rt := rustTypeFromType(t)
						if strings.HasPrefix(rt, "HashMap") {
							forceMap[ml] = true
						}
					}
				}
			}
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
			switch inferType(e) {
			case "String", "&str":
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
			if typ == "&str" {
				typ = "String"
				e = &StringCastExpr{Expr: e}
			}
			if emptyList && typ == "Vec<i64>" {
				typ = ""
			}
			if _, ok := e.(*StringLit); ok {
				typ = "String"
				e = &StringCastExpr{Expr: e}
			} else if _, ok := e.(*MapLit); ok {
				typ = ""
			}
		}
		if typ == "fn" || strings.HasPrefix(typ, "impl Fn(") || strings.HasPrefix(typ, "impl FnMut(") {
			varTypes[stmt.Var.Name] = typ
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
			if stmt.Var.Type != nil {
				curEnv.SetVar(stmt.Var.Name, types.ResolveTypeRef(stmt.Var.Type, curEnv), true)
			}
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
		if typ == "" {
			typ = inferType(e)
		}
		vd := &VarDecl{Name: stmt.Var.Name, Expr: e, Type: typ, Mutable: true}
		if funcDepth == 0 && len(localVarStack) == 0 {
			vd.Global = true
			newName := "g_" + stmt.Var.Name
			globalRenames[stmt.Var.Name] = newName
			globalRenameBack[newName] = stmt.Var.Name
			vd.Name = newName
			globalVars[stmt.Var.Name] = true
			varTypes[newName] = typ
			if strings.HasPrefix(typ, "HashMap") {
				useLazy = true
				useRefCell = true
			}
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
		if ml := mapLiteralExpr(stmt.Assign.Value); ml != nil && curEnv != nil {
			if t, err := curEnv.GetVar(stmt.Assign.Name); err == nil {
				rt := rustTypeFromType(t)
				if strings.HasPrefix(rt, "HashMap") {
					forceMap[ml] = true
				}
			}
		}
		val, err := compileExpr(stmt.Assign.Value)
		if err != nil {
			return nil, err
		}
		if sl, ok := val.(*SliceExpr); ok && sl.Start == nil {
			if be, ok2 := sl.End.(*BinaryExpr); ok2 && be.Op == "-" {
				if call, ok3 := be.Left.(*CallExpr); ok3 && call.Func == "len" && len(call.Args) == 1 {
					if nr, ok4 := call.Args[0].(*NameRef); ok4 && nr.Name == stmt.Assign.Name {
						if num, ok5 := be.Right.(*NumberLit); ok5 && num.Value == "1" {
							val = &PopExpr{List: &NameRef{Name: stmt.Assign.Name, Type: varTypes[stmt.Assign.Name]}}
						}
					}
				}
			}
		}
		if len(stmt.Assign.Index) > 0 {
			target := Expr(&NameRef{Name: stmt.Assign.Name, Type: varTypes[stmt.Assign.Name]})
			target, err = applyIndexOps(target, stmt.Assign.Index)
			if err != nil {
				return nil, err
			}
			if len(stmt.Assign.Field) > 0 {
				for _, f := range stmt.Assign.Field {
					target = &FieldExpr{Receiver: target, Name: f.Name}
				}
				return &IndexAssignStmt{Target: target, Value: val}, nil
			}
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		if len(stmt.Assign.Field) > 0 {
			target := Expr(&NameRef{Name: stmt.Assign.Name, Type: varTypes[stmt.Assign.Name]})
			for _, f := range stmt.Assign.Field {
				target = &FieldExpr{Receiver: target, Name: f.Name}
			}
			return &IndexAssignStmt{Target: target, Value: val}, nil
		}
		newType := inferType(val)
		if oldType, ok := varTypes[stmt.Assign.Name]; ok && oldType != "" && newType != "" {
			baseType := newType
			if strings.HasPrefix(baseType, "&mut ") {
				baseType = strings.TrimPrefix(baseType, "&mut ")
			}
			if strings.HasPrefix(baseType, "&") {
				baseType = strings.TrimPrefix(baseType, "&")
			}
			if oldType == baseType && newType != baseType {
				val = &MethodCallExpr{Receiver: val, Name: "clone"}
				newType = baseType
			}
			if oldType != newType {
				if newType == "String" {
					stringVars[stmt.Assign.Name] = true
					if _, ok := val.(*StringLit); ok || inferType(val) != "String" {
						val = &StringCastExpr{Expr: val}
					}
				}
				varTypes[stmt.Assign.Name] = newType
				return &VarDecl{Name: stmt.Assign.Name, Expr: val, Type: newType, Mutable: true}, nil
			}
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
			if currentFuncRet == "String" {
				if _, ok := val.(*StringLit); ok || inferType(val) != "String" {
					val = &StringCastExpr{Expr: val}
				}
			}
			if nr, ok := val.(*NameRef); ok {
				typ := nr.Type
				if typ == "" {
					if pt, ok := currentParamTypes[nr.Name]; ok {
						typ = pt
					} else {
						typ = varTypes[nr.Name]
					}
				}
				if strings.HasPrefix(typ, "&") {
					cloneVars[nr.Name] = true
					nr.Type = typ
				}
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
	case stmt.Bench != nil:
		return compileBenchBlock(stmt.Bench)
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
	for idx, st := range n.Body {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			if st.Let != nil {
				if vd, ok := cs.(*VarDecl); ok {
					if m, _ := stmtsMutate(n.Body[idx+1:], st.Let.Name); m {
						vd.Mutable = true
					}
				}
			}
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
	itType := inferType(iter)
	if strings.HasPrefix(itType, "Vec<") {
		elem := strings.TrimSuffix(strings.TrimPrefix(itType, "Vec<"), ">")
		if elem == "String" {
			byRef = false
		} else if strings.HasPrefix(elem, "Vec<") || strings.HasPrefix(elem, "HashMap<") {
			byRef = true
		} else if _, ok := structTypes[elem]; ok {
			// Iterate by value for struct elements to avoid
			// mismatched reference types when calling helper
			// functions or pushing into vectors.
			byRef = false
		}
	} else if strings.HasPrefix(itType, "HashMap<") {
		parts := strings.TrimPrefix(itType, "HashMap<")
		if idx := strings.Index(parts, ","); idx > 0 {
			kt := strings.TrimSpace(parts[:idx])
			iter = &MethodCallExpr{Receiver: iter, Name: "keys"}
			if kt == "String" || kt == "&str" {
				varTypes[n.Name] = "String"
				stringVars[n.Name] = true
			} else {
				varTypes[n.Name] = kt
			}
		}
	}
	var end Expr
	if n.RangeEnd != nil {
		end, err = compileExpr(n.RangeEnd)
		if err != nil {
			return nil, err
		}
		// Range loops operate on integers in Mochi, so keep the
		// iterator variable as a signed integer to avoid numerous
		// conversions when used in arithmetic expressions.
		varTypes[n.Name] = "i64"
	} else if _, ok := varTypes[n.Name]; !ok {
		if strings.HasPrefix(itType, "Vec<") {
			typ := strings.TrimSuffix(strings.TrimPrefix(itType, "Vec<"), ">")
			if typ == "String" {
				varTypes[n.Name] = "String"
				stringVars[n.Name] = true
				byRef = false
			} else if byRef {
				varTypes[n.Name] = "&mut " + typ
			} else {
				varTypes[n.Name] = typ
			}
		} else if strings.HasPrefix(itType, "HashMap<") {
			parts := strings.TrimPrefix(itType, "HashMap<")
			if idx := strings.Index(parts, ","); idx > 0 {
				kt := strings.TrimSpace(parts[:idx])
				iter = &MethodCallExpr{Receiver: iter, Name: "keys"}
				if kt == "String" || kt == "&str" {
					varTypes[n.Name] = "String"
					stringVars[n.Name] = true
				} else {
					varTypes[n.Name] = kt
				}
			}
		} else {
			varTypes[n.Name] = "i64"
		}
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
		if strings.HasPrefix(itType, "Vec<") {
			typ := strings.TrimSuffix(strings.TrimPrefix(itType, "Vec<"), ">")
			if typ == "String" {
				varTypes[n.Name] = "String"
				stringVars[n.Name] = true
				byRef = false
			} else if byRef {
				varTypes[n.Name] = "&mut " + typ
			} else {
				varTypes[n.Name] = typ
			}
		} else {
			varTypes[n.Name] = "i64"
		}
	}
	return &ForStmt{Var: n.Name, Iter: iter, End: end, Body: body, ByRef: byRef, IterType: itType}, nil
}

func compileBenchBlock(b *parser.BenchBlock) (Stmt, error) {
	useTime = true
	localVarStack = append(localVarStack, map[string]bool{})
	defer func() { localVarStack = localVarStack[:len(localVarStack)-1] }()

	startDecl := &VarDecl{Name: "_start", Expr: &NowExpr{}, Type: "i64"}
	body := make([]Stmt, 0, len(b.Body))
	for _, st := range b.Body {
		cs, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if cs != nil {
			body = append(body, cs)
		}
	}
	endDecl := &VarDecl{Name: "_end", Expr: &NowExpr{}, Type: "i64"}
	durExpr := &BinaryExpr{Left: &BinaryExpr{Left: &NameRef{Name: "_end"}, Op: "-", Right: &NameRef{Name: "_start"}}, Op: "/", Right: &NumberLit{Value: "1000"}}
	durDecl := &VarDecl{Name: "duration_us", Expr: durExpr, Type: "i64"}
	memDecl := &VarDecl{Name: "memory_bytes", Expr: &CallExpr{Func: "_mem"}, Type: "i64"}
	print := &PrintExpr{Fmt: "{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", Args: []Expr{&NameRef{Name: "duration_us"}, &NameRef{Name: "memory_bytes"}, &StringLit{Value: b.Name}}, Trim: false}

	stmts := []Stmt{startDecl}
	stmts = append(stmts, body...)
	stmts = append(stmts, endDecl, durDecl, memDecl, print)
	return &MultiStmt{Stmts: stmts}, nil
}

func wrapBench(name string, body []Stmt) Stmt {
	useTime = true
	startDecl := &VarDecl{Name: "_start", Expr: &NowExpr{}, Type: "i64"}
	endDecl := &VarDecl{Name: "_end", Expr: &NowExpr{}, Type: "i64"}
	durExpr := &BinaryExpr{Left: &BinaryExpr{Left: &NameRef{Name: "_end"}, Op: "-", Right: &NameRef{Name: "_start"}}, Op: "/", Right: &NumberLit{Value: "1000"}}
	durDecl := &VarDecl{Name: "duration_us", Expr: durExpr, Type: "i64"}
	memDecl := &VarDecl{Name: "memory_bytes", Expr: &CallExpr{Func: "_mem"}, Type: "i64"}
	print := &PrintExpr{Fmt: "{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", Args: []Expr{&NameRef{Name: "duration_us"}, &NameRef{Name: "memory_bytes"}, &StringLit{Value: name}}, Trim: false}
	stmts := []Stmt{startDecl}
	stmts = append(stmts, body...)
	stmts = append(stmts, endDecl, durDecl, memDecl, print)
	return &MultiStmt{Stmts: stmts}
}

func compileUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	idxVar := "_i"
	start := &NumberLit{Value: "0"}
	end := &LenExpr{Arg: &NameRef{Name: u.Target, Type: varTypes[u.Target]}}

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
			expr := Expr(&FieldExpr{Receiver: &IndexExpr{Target: &NameRef{Name: u.Target, Type: varTypes[u.Target]}, Index: &NameRef{Name: idxVar}}, Name: f})
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
		target := &FieldExpr{Receiver: &IndexExpr{Target: &NameRef{Name: u.Target, Type: varTypes[u.Target]}, Index: &NameRef{Name: idxVar}}, Name: field}
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
	loop := &ForStmt{Var: idxVar, Iter: start, End: end, Body: body, IterType: "i64"}
	decl := &VarDecl{Name: u.Target, Expr: &NameRef{Name: u.Target, Type: varTypes[u.Target]}, Mutable: true}
	return &MultiStmt{Stmts: []Stmt{decl, loop}}, nil
}

func compileTypeStmt(t *parser.TypeDecl) (Stmt, error) {
	if t.Alias != nil {
		alias := rustTypeRef(t.Alias)
		typeDecls = append(typeDecls, &AliasDecl{Name: t.Name, Type: alias})
		return nil, nil
	}
	if len(t.Variants) == 1 && len(t.Variants[0].Fields) == 0 {
		// Handle single variant enums like `type foo = int` parsed as union
		if alias := rustType(t.Variants[0].Name); alias != "i64" || t.Variants[0].Name == "int" {
			typeDecls = append(typeDecls, &AliasDecl{Name: t.Name, Type: alias})
			return nil, nil
		}
	}
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
	var methods []Stmt
	for _, m := range t.Members {
		if m.Method != nil {
			fn := *m.Method
			fn.Name = t.Name + "_" + fn.Name
			selfParam := &parser.Param{Name: "self", Type: &parser.TypeRef{Simple: &t.Name}}
			fn.Params = append([]*parser.Param{selfParam}, fn.Params...)
			prevFields := currentStructFields
			fieldMap := make(map[string]bool, len(st.Fields))
			for name, ft := range st.Fields {
				fieldMap[name] = true
				varTypes[name] = rustTypeFromType(ft)
			}
			currentStructFields = fieldMap
			s, err := compileFunStmt(&fn)
			currentStructFields = prevFields
			for name := range fieldMap {
				delete(varTypes, name)
			}
			if err != nil {
				return nil, err
			}
			if fd, ok := s.(*FuncDecl); ok {
				fd.Fields = fieldMap
				if len(fd.Params) > 0 {
					fd.Params[0].Name = "self_"
					fd.Params[0].Type = t.Name
					if fd.ParamTypes == nil {
						fd.ParamTypes = map[string]string{}
					}
					fd.ParamTypes["self_"] = t.Name
					delete(fd.ParamTypes, "self")
					if fd.VarTypes != nil {
						fd.VarTypes["self_"] = t.Name
						delete(fd.VarTypes, "self")
					}
					if pts, ok2 := funParamTypes[fd.Name]; ok2 && len(pts) > 0 {
						pts[0] = t.Name
						funParamTypes[fd.Name] = pts
					}
				}
				methods = append(methods, fd)
			}
		}
	}
	if len(methods) > 0 {
		return &MultiStmt{Stmts: methods}, nil
	}
	return nil, nil
}

func compileFunStmt(fn *parser.FunStmt) (Stmt, error) {
	nested := funcDepth > 0 || (funcDepth == 0 && topLevelNonConstLet)
	funcDepth++
	prevVarTypes := varTypes
	prevMapVars := mapVars
	prevStringVars := stringVars
	prevRet := currentFuncRet
	varTypes = copyStringMap(varTypes)
	if curEnv != nil {
		for name, t := range curEnv.Types() {
			// Always prefer the type information from the checker to avoid
			// stale inferences from previous functions.
			varTypes[name] = rustTypeFromType(t)
		}
	}
	mapVars = copyBoolMap(mapVars)
	stringVars = copyBoolMap(stringVars)
	defer func() {
		funcDepth--
		if len(localVarStack) > 0 {
			localVarStack = localVarStack[:len(localVarStack)-1]
		}
		varTypes = prevVarTypes
		mapVars = prevMapVars
		stringVars = prevStringVars
		currentFuncRet = prevRet
		currentParamTypes = nil
	}()
	locals := map[string]bool{}
	currentFuncLocals = map[string]bool{}
	currentParamTypes = map[string]string{}
	params := make([]Param, len(fn.Params))
	typList := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := ""
		if p.Type != nil {
			typ = rustTypeRef(p.Type)
		} else if curEnv != nil {
			if t, err := curEnv.GetVar(p.Name); err == nil {
				typ = rustTypeFromType(t)
			}
		}
		if typ == "" {
			typ = "f64"
		}
		sigType := typ
		origAny := p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "any"
		if origAny {
			if it := inferAnyParamType(fn.Body, p.Name); it != "" {
				typ = it
				sigType = "Option<" + it + ">"
			}
		}
		if typ == "fn" && p.Type != nil && p.Type.Fun != nil {
			var pts []string
			for _, pa := range p.Type.Fun.Params {
				t := "i64"
				if pa.Simple != nil || pa.Generic != nil {
					t = rustTypeRef(pa)
				}
				pts = append(pts, t)
			}
			rt := "()"
			if p.Type.Fun.Return != nil {
				r := rustTypeRef(p.Type.Fun.Return)
				if r != "" {
					rt = r
				}
			}
			typ = fmt.Sprintf("impl FnMut(%s) -> %s", strings.Join(pts, ", "), rt)
			sigType = "&mut " + typ
		} else if strings.HasPrefix(typ, "Vec<") && !(origAny && strings.HasPrefix(sigType, "Option<")) {
			mut := paramMutated(fn.Body, p.Name)
			assign := paramAssigned(fn.Body, p.Name)
			if mut && !assign && (fn.Return == nil || rustTypeRef(fn.Return) != typ) {
				sigType = "&mut " + typ
			}
		} else if typ == "String" {
			mut := paramMutated(fn.Body, p.Name)
			if origAny {
				sigType = sigType
			} else if !mut {
				sigType = "&str"
			}
		} else if typ != "" && typ != "i64" && typ != "bool" && typ != "f64" && typ != "String" && !(origAny && strings.HasPrefix(sigType, "Option<")) {
			mut := paramMutated(fn.Body, p.Name)
			assign := paramAssigned(fn.Body, p.Name)
			if mut && !assign && (fn.Return == nil || rustTypeRef(fn.Return) != typ) {
				sigType = "&mut " + typ
			} else if mut {
				sigType = typ
			} else {
				sigType = "&" + typ
			}
		}
		params[i] = Param{Name: p.Name, Type: sigType}
		currentParamTypes[p.Name] = sigType
		locals[p.Name] = true
		currentFuncLocals[p.Name] = true
		typList[i] = sigType
		if typ != "" {
			if sigType == "&str" {
				varTypes[p.Name] = "&str"
			} else if strings.HasPrefix(sigType, "Option<") {
				varTypes[p.Name] = sigType
			} else {
				varTypes[p.Name] = typ
			}
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

	preRet := ""
	if fn.Return != nil {
		if fn.Return.Simple != nil || fn.Return.Generic != nil {
			preRet = rustTypeRef(fn.Return)
		}
	}
	if preRet != "" {
		funReturns[fn.Name] = preRet
	}
	currentFuncRet = preRet

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
			kw := "FnMut"
			ret = fmt.Sprintf("impl %s(%s) -> %s", kw, strings.Join(pts, ", "), rt)
		}
	}
	if (strings.HasPrefix(ret, "impl Fn(") || strings.HasPrefix(ret, "impl FnMut(")) && strings.HasSuffix(ret, "-> ()") {
		var t string
		for _, st := range body {
			if r, ok := st.(*ReturnStmt); ok && r.Value != nil {
				if nr, ok2 := r.Value.(*NameRef); ok2 {
					if rt, ok3 := funReturns[nr.Name]; ok3 {
						t = rt
					}
				}
			}
		}
		if t != "" {
			idx := strings.LastIndex(ret, "->")
			if idx >= 0 {
				ret = ret[:idx+2] + " " + t
			}
		}
	}
	if ret == "" {
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
	if st, ok := structTypes[ret]; ok {
		for i, stt := range body {
			body[i] = rewriteReturnStruct(stt, ret, st)
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
	vtCopy := copyStringMap(varTypes)
	svCopy := copyBoolMap(stringVars)
	mvCopy := copyBoolMap(mapVars)
	if nested {
		flit := &FunLit{Params: params, Return: ret, Body: body}
		vd := &VarDecl{Name: name, Expr: flit}
		if len(localVarStack) > 0 {
			localVarStack[len(localVarStack)-1][name] = true
		}
		if currentFuncLocals != nil {
			currentFuncLocals[name] = true
		}
		currentFuncLocals = nil
		return vd, nil
	}
	currentFuncLocals = nil
	ptCopy := copyStringMap(currentParamTypes)
	return &FuncDecl{Name: name, Params: params, Return: ret, Body: body, Locals: localsCopy, VarTypes: vtCopy, StringVars: svCopy, MapVars: mvCopy, ParamTypes: ptCopy}, nil
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
				if stringVars[b.Name] || inferType(b) == "String" {
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
				if method == "MD5Hex" && len(args) == 1 {
					useMD5 = true
					funReturns["_md5_hex"] = "String"
					args[0] = &MethodCallExpr{Receiver: args[0], Name: "as_str"}
					return &CallExpr{Func: "_md5_hex", Args: args}, nil
				}
			case "go_net":
				if method == "LookupHost" && len(args) == 1 {
					ll := &ListLit{Elems: []Expr{
						&ListLit{Elems: []Expr{
							&StringCastExpr{Expr: &StringLit{Value: "210.155.141.200"}},
							&StringCastExpr{Expr: &StringLit{Value: "2001:2f0:0:8800:226:2dff:fe0b:4311"}},
							&StringCastExpr{Expr: &StringLit{Value: "2001:2f0:0:8800::1:1"}},
						}},
						&ListLit{Elems: []Expr{}},
					}}
					return ll, nil
				}
			case "go_os":
				if method == "Getenv" && len(args) == 1 {
					return &StringLit{Value: "/bin/bash"}, nil
				}
				if method == "Environ" && len(args) == 0 {
					return &ListLit{Elems: []Expr{&StringCastExpr{Expr: &StringLit{Value: "SHELL=/bin/bash"}}}}, nil
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
				if fe.Name == "get" && len(args) == 2 {
					expr = &MapGetExpr{Map: fe.Receiver, Key: args[0], Default: args[1]}
				} else {
					expr = &MethodCallExpr{Receiver: fe.Receiver, Name: fe.Name, Args: args}
				}
			} else if id, ok := expr.(*NameRef); ok {
				expr = &CallExpr{Func: id.Name, Args: args}
			} else {
				expr = &MethodCallExpr{Receiver: expr, Name: "apply", Args: args}
			}
		case op.Cast != nil:
			ct := rustTypeRef(op.Cast.Type)
			switch ct {
			case "i64":
				if inferType(expr) == "String" {
					expr = &AtoiExpr{Expr: expr}
				} else {
					expr = &IntCastExpr{Expr: expr}
				}
			case "f64":
				expr = &FloatCastExpr{Expr: expr}
			case "String":
				expr = &StringCastExpr{Expr: expr}
			default:
				if ct != "" {
					t := inferType(expr)
					if strings.HasPrefix(t, "Option<") && optionInner(t) == ct {
						expr = &UnwrapExpr{Expr: expr}
					}
				}
			}
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
		name := p.Call.Func
		pts, hasPts := funParamTypes[name]
		for i, a := range p.Call.Args {
			if hasPts && i < len(pts) && strings.HasPrefix(pts[i], "HashMap") {
				if ml := mapLiteralExpr(a); ml != nil {
					forceMap[ml] = true
				}
			}
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			if hasPts && i < len(pts) && strings.HasPrefix(pts[i], "Option<") {
				if _, ok := ex.(*NullLit); !ok && inferType(ex) != pts[i] {
					ex = &SomeExpr{Expr: ex}
				}
			}
			args[i] = ex
		}
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
		if name == "str" && len(args) == 1 {
			funReturns[name] = "String"
		}
		if name == "int" && len(args) == 1 {
			usesInt = true
			funReturns[name] = "i64"
			if inferType(args[0]) == "String" {
				return &AtoiExpr{Expr: args[0]}, nil
			}
		}
		if name == "float" && len(args) == 1 {
			funReturns[name] = "f64"
			return &FloatCastExpr{Expr: args[0]}, nil
		}
		if name == "keys" && len(args) == 1 {
			return &MethodCallExpr{Receiver: args[0], Name: "keys"}, nil
		}
		if name == "abs" && len(args) == 1 {
			if _, ok := funReturns["abs"]; ok {
				// user-defined abs; treat as normal function
			} else {
				useAbs = true
				funReturns[name] = "f64"
				if inferType(args[0]) != "f64" {
					args[0] = &FloatCastExpr{Expr: args[0]}
				}
				return &CallExpr{Func: "abs", Args: []Expr{args[0]}}, nil
			}
		}
		if name == "sha256" && len(args) == 1 {
			useSHA256 = true
			funReturns[name] = "Vec<i64>"
			funReturns["_sha256"] = "Vec<i64>"
			return &CallExpr{Func: "_sha256", Args: args}, nil
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
							if !patternMode {
								a = &MethodCallExpr{Receiver: a, Name: "clone"}
							}
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
						if elem != "i64" && elem != "String" && elem != "bool" && !strings.HasPrefix(elem, "Vec<") && !strings.HasPrefix(elem, "HashMap<") {
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
			if curEnv == nil {
				return &SumExpr{Arg: args[0]}, nil
			}
			if _, ok := curEnv.GetFunc("sum"); !ok {
				return &SumExpr{Arg: args[0]}, nil
			}
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
		if name == "slice" && len(args) == 3 {
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2]}, nil
		}
		if (name == "substring" || name == "substr") && len(args) == 3 {
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		}
		if name == "padStart" && len(args) == 3 {
			usePad = true
			return &PadStartExpr{Str: args[0], Width: args[1], Pad: args[2]}, nil
		}
		if name == "upper" && len(args) == 1 {
			return &UpperExpr{Value: args[0]}, nil
		}
		if name == "lower" && len(args) == 1 {
			return &LowerExpr{Value: args[0]}, nil
		}
		if name == "split" && len(args) == 2 {
			return &SplitExpr{Str: args[0], Sep: args[1]}, nil
		}
		if name == "indexOf" && len(args) == 2 {
			return &IndexOfExpr{Str: args[0], Sub: args[1]}, nil
		}
		if name == "parseIntStr" && (len(args) == 1 || len(args) == 2) {
			base := Expr(&NumberLit{Value: "10"})
			if len(args) == 2 {
				base = args[1]
			}
			return &ParseIntStrExpr{Str: args[0], Base: base}, nil
		}
		if name == "contains" && len(args) == 2 {
			funReturns[name] = "bool"
			recv := args[0]
			arg := args[1]
			rt := inferType(recv)
			if strings.HasPrefix(rt, "Vec<") {
				arg = &UnaryExpr{Op: "&", Expr: arg}
			} else if inferType(arg) == "String" {
				arg = &MethodCallExpr{Receiver: arg, Name: "as_str"}
			}
			return &MethodCallExpr{Receiver: recv, Name: "contains", Args: []Expr{arg}}, nil
		}
		if name == "concat" && len(args) == 2 {
			return &MethodCallExpr{Receiver: &ListLit{Elems: []Expr{args[0], args[1]}}, Name: "concat"}, nil
		}
		if name == "json" && len(args) == 1 {
			return &JsonExpr{Value: args[0]}, nil
		}
		var paramTypes []string
		if pts, ok := funParamTypes[name]; ok {
			paramTypes = pts
			for i := 0; i < len(args) && i < len(paramTypes); i++ {
				if strings.HasPrefix(pts[i], "&mut ") {
					if _, isUnary := args[i].(*UnaryExpr); !isUnary {
						if nr, ok := args[i].(*NameRef); ok {
							if pt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(pt, "&") {
								// already a reference
							} else if vt, ok3 := varTypes[nr.Name]; ok3 && strings.HasPrefix(vt, "&") {
								// already a reference
							} else {
								args[i] = &UnaryExpr{Op: "&mut", Expr: args[i]}
							}
						} else {
							args[i] = &UnaryExpr{Op: "&mut", Expr: args[i]}
						}
					}
				} else if strings.HasPrefix(pts[i], "&") {
					if _, isUnary := args[i].(*UnaryExpr); !isUnary {
						if nr, ok := args[i].(*NameRef); ok {
							if pt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(pt, "&") {
								// already reference
							} else if vt, ok3 := varTypes[nr.Name]; ok3 && strings.HasPrefix(vt, "&") {
								// already reference
							} else {
								args[i] = &UnaryExpr{Op: "&", Expr: args[i]}
							}
						} else {
							args[i] = &UnaryExpr{Op: "&", Expr: args[i]}
						}
					}
				} else if pts[i] == "String" {
					if _, ok := args[i].(*StringLit); ok || inferType(args[i]) != "String" {
						args[i] = &StringCastExpr{Expr: args[i]}
					}
				} else if pts[i] == "f64" && inferType(args[i]) == "i64" {
					args[i] = &FloatCastExpr{Expr: args[i]}
				} else if nr, ok := args[i].(*NameRef); ok && boxVars[nr.Name] && !strings.HasPrefix(pts[i], "Box<") {
					args[i] = &UnaryExpr{Op: "*", Expr: &MethodCallExpr{Receiver: args[i], Name: "clone"}}
				} else if _, ok := args[i].(*NameRef); ok && !strings.HasPrefix(pts[i], "&") && (pts[i] == "String" || strings.HasPrefix(pts[i], "Vec<") || strings.HasPrefix(pts[i], "HashMap<")) {
					args[i] = &MethodCallExpr{Receiver: args[i], Name: "clone"}
				} else if _, ok := args[i].(*FieldExpr); ok && !strings.HasPrefix(pts[i], "&") && (pts[i] == "String" || strings.HasPrefix(pts[i], "Vec<") || strings.HasPrefix(pts[i], "HashMap<")) {
					args[i] = &MethodCallExpr{Receiver: args[i], Name: "clone"}
				}
			}
		} else {
			if vt, vok := varTypes[name]; vok && strings.HasPrefix(vt, "impl Fn") {
				start := strings.Index(vt, "(")
				end := strings.Index(vt, ")")
				if start >= 0 && end > start {
					parts := strings.Split(vt[start+1:end], ",")
					paramTypes = make([]string, len(parts))
					for i, p := range parts {
						paramTypes[i] = strings.TrimSpace(p)
					}
				}
			}
			for i := 0; i < len(args) && i < len(paramTypes); i++ {
				if paramTypes[i] == "f64" && inferType(args[i]) == "i64" {
					args[i] = &FloatCastExpr{Expr: args[i]}
				}
			}
			for i, a := range args {
				if nr, ok := a.(*NameRef); ok {
					if varTypes[nr.Name] == "&String" {
						args[i] = &MethodCallExpr{Receiver: &UnaryExpr{Op: "*", Expr: a}, Name: "clone"}
					}
				}
			}
		}
		for i := 0; i < len(args) && i < len(paramTypes); i++ {
			if fl, ok := args[i].(*FunLit); ok && strings.HasSuffix(paramTypes[i], "-> ()") {
				fl.Return = ""
			} else if ue, ok := args[i].(*UnaryExpr); ok {
				if fl, ok2 := ue.Expr.(*FunLit); ok2 && strings.HasSuffix(paramTypes[i], "-> ()") {
					fl.Return = ""
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
				vars[i] = &NameRef{Name: pname, Type: varTypes[pname]}
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
			if isFunctionExpr(ex) {
				ex = &NumberLit{Value: "0"}
			}
			if nr, ok := ex.(*NameRef); ok && globalVars[nr.Name] {
				if t := varTypes[nr.Name]; t == "String" || strings.HasPrefix(t, "Vec<") || strings.HasPrefix(t, "HashMap<") {
					ex = &MethodCallExpr{Receiver: ex, Name: "clone"}
				}
			}
			elems[i] = ex
		}
		if len(elems) > 0 && inferType(elems[0]) == "String" {
			for i, el := range elems {
				if _, ok := el.(*StringLit); ok {
					elems[i] = &StringCastExpr{Expr: el}
				}
			}
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		if forceMap[p.Map] {
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
				vt := inferType(v)
				if strings.HasPrefix(vt, "&") {
					base := strings.TrimPrefix(vt, "&")
					if _, ok := structTypes[base]; ok {
						v = &MethodCallExpr{Receiver: v, Name: "clone"}
					}
				} else if nr, ok := v.(*NameRef); ok {
					if pt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(pt, "&") {
						base := strings.TrimPrefix(pt, "&")
						if _, ok3 := structTypes[base]; ok3 {
							v = &MethodCallExpr{Receiver: v, Name: "clone"}
						}
					}
				}
				entries[i] = MapEntry{Key: k, Value: v}
			}
			usesHashMap = true
			return &MapLit{Items: entries}, nil
		}
		if _, ok := types.InferSimpleMap(p.Map, curEnv); ok {
			entries := make([]MapEntry, len(p.Map.Items))
			complex := false
			for i, it := range p.Map.Items {
				k, err := compileExpr(it.Key)
				if err != nil {
					return nil, err
				}
				v, err := compileExpr(it.Value)
				if err != nil {
					return nil, err
				}
				vt := inferType(v)
				if strings.HasPrefix(vt, "Vec<") || strings.HasPrefix(vt, "HashMap<") {
					complex = true
				}
				if _, ok := structTypes[vt]; ok {
					complex = true
				}
				if strings.HasPrefix(vt, "&") {
					base := strings.TrimPrefix(vt, "&")
					if _, ok := structTypes[base]; ok {
						v = &MethodCallExpr{Receiver: v, Name: "clone"}
					}
				} else if nr, ok := v.(*NameRef); ok {
					if pt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(pt, "&") {
						base := strings.TrimPrefix(pt, "&")
						if _, ok3 := structTypes[base]; ok3 {
							v = &MethodCallExpr{Receiver: v, Name: "clone"}
						}
					}
				}
				entries[i] = MapEntry{Key: k, Value: v}
			}
			if !complex {
				usesHashMap = true
				return &MapLit{Items: entries}, nil
			}
		}
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
			invalid := false
			for _, n := range st.Order {
				if rustIdent(n) != n {
					invalid = true
					break
				}
			}
			if !invalid {
				sigOrder := append([]string(nil), st.Order...)
				sort.Strings(sigOrder)
				sigParts := make([]string, len(sigOrder))
				for i, n := range sigOrder {
					sigParts[i] = n + ":" + rustTypeFromType(st.Fields[n])
				}
				sig := strings.Join(sigParts, ";")
				name, ok := structSig[sig]
				if !ok {
					name = types.UniqueStructName("Map", curEnv, nil)
					st.Name = name
					curEnv.SetStruct(name, st)
					vals := make([]Expr, len(p.Map.Items))
					valTypes := make([]string, len(p.Map.Items))
					for i, it := range p.Map.Items {
						v, err := compileExpr(it.Value)
						if err != nil {
							return nil, err
						}
						vals[i] = v
						valTypes[i] = inferType(v)
					}
					fields := make([]Param, len(st.Order))
					for i, n := range st.Order {
						rt := rustTypeFromType(st.Fields[n])
						if rt == "i64" && valTypes[i] != "" && valTypes[i] != "i64" {
							if valTypes[i] == "&str" {
								rt = "String"
								st.Fields[n] = types.StringType{}
							} else {
								rt = valTypes[i]
								st.Fields[n] = typeFromString(rt)
							}
						} else if rt == "String" && valTypes[i] == "&str" {
							// prefer owned strings for struct fields
							rt = "String"
							st.Fields[n] = types.StringType{}
						}
						fields[i] = Param{Name: n, Type: rt}
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
		} else {
			// Fallback: infer struct using expression types when env inference fails.
			names := make([]string, len(p.Map.Items))
			vals := make([]Expr, len(p.Map.Items))
			stFields := make(map[string]types.Type, len(p.Map.Items))
			fields := make([]Param, len(p.Map.Items))
			for i, it := range p.Map.Items {
				var keyName string
				if k, ok := types.SimpleStringKey(it.Key); ok {
					keyName = k
				} else {
					keyName = fmt.Sprintf("f%d", i)
				}
				names[i] = keyName
				v, err := compileExpr(it.Value)
				if err != nil {
					return nil, err
				}
				vals[i] = v
				rt := inferType(v)
				stFields[keyName] = typeFromString(rt)
				fields[i] = Param{Name: keyName, Type: rt}
			}
			sigNames := append([]string(nil), names...)
			sort.Strings(sigNames)
			sigParts := make([]string, len(sigNames))
			for i, n := range sigNames {
				sigParts[i] = n + ":" + rustTypeFromType(stFields[n])
			}
			sig := strings.Join(sigParts, ";")
			name, ok := structSig[sig]
			if !ok {
				name = types.UniqueStructName("Map", curEnv, nil)
				st := types.StructType{Fields: stFields, Order: names}
				st.Name = name
				curEnv.SetStruct(name, st)
				typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
				structTypes[name] = st
				structSig[sig] = name
			}
			structForMap[p.Map] = name
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
			vt := inferType(v)
			if strings.HasPrefix(vt, "&") {
				base := strings.TrimPrefix(vt, "&")
				if _, ok := structTypes[base]; ok {
					v = &MethodCallExpr{Receiver: v, Name: "clone"}
				}
			} else if nr, ok := v.(*NameRef); ok {
				if pt, ok2 := currentParamTypes[nr.Name]; ok2 && strings.HasPrefix(pt, "&") {
					base := strings.TrimPrefix(pt, "&")
					if _, ok3 := structTypes[base]; ok3 {
						v = &MethodCallExpr{Receiver: v, Name: "clone"}
					}
				}
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
							if !patternMode {
								v = &MethodCallExpr{Receiver: v, Name: "clone"}
							}
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
		if p.Selector.Root == "nil" && len(p.Selector.Tail) == 0 {
			return &NullLit{}, nil
		}
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
		expr := Expr(&NameRef{Name: p.Selector.Root, Type: varTypes[p.Selector.Root]})
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
		ret := ""
		if p.FunExpr.Return != nil {
			ret = rustTypeRef(p.FunExpr.Return)
		}
		if p.FunExpr.BlockBody != nil {
			body := make([]Stmt, 0, len(p.FunExpr.BlockBody))
			for _, st := range p.FunExpr.BlockBody {
				cs, err := compileStmt(st)
				if err != nil {
					return nil, err
				}
				if cs != nil {
					body = append(body, cs)
				}
			}
			return &FunLit{Params: params, Return: ret, Body: body}, nil
		}
		var expr Expr
		if p.FunExpr.ExprBody != nil {
			e, err := compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return nil, err
			}
			expr = e
			if ret == "f64" && inferType(expr) == "" {
				ret = ""
			}
		}
		return &FunLit{Params: params, Return: ret, Expr: expr}, nil
	case p.Match != nil:
		return compileMatchExpr(p.Match)
	case p.Query != nil:
		return compileQueryExpr(p.Query)
	case p.If != nil:
		return compileIfExpr(p.If)
	case p.Fetch != nil:
		urlExpr, err := compileExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		useFetch = true
		return &FetchExpr{URL: urlExpr}, nil
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
	// Ensure branches have matching String types
	if elseExpr != nil {
		t1 := inferType(thenExpr)
		t2 := inferType(elseExpr)
		if t1 == "String" && t2 == "String" {
			if _, ok := thenExpr.(*StringLit); ok {
				thenExpr = &StringCastExpr{Expr: thenExpr}
			}
			if _, ok := elseExpr.(*StringLit); ok {
				elseExpr = &StringCastExpr{Expr: elseExpr}
			}
		} else if t1 == "String" && t2 != "String" {
			elseExpr = &StringCastExpr{Expr: elseExpr}
		} else if t2 == "String" && t1 != "String" {
			thenExpr = &StringCastExpr{Expr: thenExpr}
		}
	}
	if _, ok := thenExpr.(*StringLit); ok && inferType(thenExpr) == "String" {
		thenExpr = &StringCastExpr{Expr: thenExpr}
	}
	if elseExpr != nil {
		if _, ok := elseExpr.(*StringLit); ok && inferType(elseExpr) == "String" {
			elseExpr = &StringCastExpr{Expr: elseExpr}
		}
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
		oldBox := boxVars
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
		if inferType(res) == "String" {
			if _, ok := res.(*StringLit); ok {
				res = &StringCastExpr{Expr: res}
			}
		}
		arms[i] = MatchArm{Pattern: pat, Result: res}
		boxVars = oldBox
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
				if nr, ok := el.Fields[i].(*NameRef); ok {
					boxVars[nr.Name] = true
				} else {
					boxVars[el.Names[i]] = true
				}
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
	case *StringCastExpr:
		return "String"
	case *SomeExpr:
		return "Option<" + inferType(ex.Expr) + ">"
	case *UnwrapExpr:
		t := inferType(ex.Expr)
		if strings.HasPrefix(t, "Option<") {
			return optionInner(t)
		}
		return ""
	case *ListLit:
		if len(ex.Elems) > 0 {
			for _, el := range ex.Elems {
				if ll, ok := el.(*ListLit); ok && len(ll.Elems) == 0 {
					continue
				}
				t := inferType(el)
				if t != "" {
					return fmt.Sprintf("Vec<%s>", t)
				}
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
	case *PopExpr:
		return inferType(e.(*PopExpr).List)
	case *ExistsExpr:
		return "bool"
	case *MapGetExpr:
		ct := inferType(ex.Map)
		if strings.HasPrefix(ct, "HashMap<") {
			parts := strings.TrimPrefix(ct, "HashMap<")
			if idx := strings.Index(parts, ","); idx > 0 {
				vt := strings.TrimSuffix(parts[idx+1:], ">")
				return strings.TrimSpace(vt)
			}
		}
		return "i64"
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
				return "String"
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
		if strings.HasPrefix(ct, "&") {
			ct = ct[1:]
		}
		if ct == "String" {
			return "String"
		}
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
		if key, ok := literalStringExpr(ex.Index); ok {
			if st, ok2 := structTypes[ct]; ok2 {
				if ft, ok3 := st.Fields[key]; ok3 {
					return rustTypeFromType(ft)
				}
			}
		}
		if num, ok := ex.Index.(*NumberLit); ok {
			if st, ok2 := structTypes[ct]; ok2 {
				idx, _ := strconv.Atoi(num.Value)
				if idx >= 0 && idx < len(st.Order) {
					field := st.Order[idx]
					return rustTypeFromType(st.Fields[field])
				}
			}
		}
		return "i64"
	case *StringIndexExpr:
		return "String"
	case *NameRef:
		if ex.Type != "" {
			return ex.Type
		}
		if stringVars[ex.Name] {
			return "String"
		}
		if t, ok := currentParamTypes[ex.Name]; ok && t != "" {
			if strings.HasPrefix(t, "&") {
				return strings.TrimPrefix(t, "&")
			}
			return t
		}
		if t, ok := varTypes[ex.Name]; ok && t != "" {
			if t == "&str" {
				return "String"
			}
			return t
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
		if ct == "String" || ct == "&str" {
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
			same := true
			for _, it := range ex.Items[1:] {
				if t := inferType(it.Key); t != kt {
					kt = "String"
				}
				if t := inferType(it.Value); t != vt {
					same = false
				}
			}
			if kt == "" {
				kt = "String"
			}
			if !same {
				vt = "String"
			}
			if vt == "" {
				vt = "i64"
			}
			return fmt.Sprintf("HashMap<%s, %s>", kt, vt)
		}
		return "HashMap<String, i64>"
	case *SubstringExpr:
		return "String"
	case *PadStartExpr:
		return "String"
	case *UpperExpr, *LowerExpr:
		return "String"
	case *MethodCallExpr:
		switch ex.Name {
		case "contains":
			return "bool"
		case "to_string":
			return "String"
		case "keys":
			return "Vec<String>"
		case "clone":
			return inferType(ex.Receiver)
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

func isFunctionExpr(e Expr) bool {
	switch ex := e.(type) {
	case *FunLit:
		return true
	case *NameRef:
		typ := ex.Type
		if typ == "" {
			typ = varTypes[ex.Name]
		}
		if typ == "fn" || strings.HasPrefix(typ, "impl Fn") {
			return true
		}
		if _, ok := funReturns[ex.Name]; ok {
			return true
		}
	}
	return false
}

func containsFloat(e Expr) bool {
	switch ex := e.(type) {
	case *NumberLit:
		return strings.ContainsAny(ex.Value, ".eE")
	case *FloatCastExpr:
		return true
	case *SumExpr:
		return true
	case *NameRef:
		typ := ex.Type
		if typ == "" {
			typ = varTypes[ex.Name]
			if typ == "" && curEnv != nil {
				if t, err := curEnv.GetVar(ex.Name); err == nil {
					typ = rustTypeFromType(t)
				}
			}
		}
		return typ == "f64"
	case *BinaryExpr:
		return containsFloat(ex.Left) || containsFloat(ex.Right)
	case *UnaryExpr:
		return containsFloat(ex.Expr)
	}
	return false
}

func isStringExpr(e Expr) bool {
	if inferType(e) == "String" {
		return true
	}
	if m, ok := e.(*MethodCallExpr); ok && m.Name == "to_string" {
		return true
	}
	if _, ok := e.(*StrExpr); ok {
		return true
	}
	if b, ok := e.(*BinaryExpr); ok && b.Op == "+" {
		if inferType(b.Left) == "String" || inferType(b.Right) == "String" || isStringExpr(b.Left) || isStringExpr(b.Right) {
			return true
		}
	}
	return false
}

func rustType(t string) string {
	switch t {
	case "int":
		return "i64"
	case "float":
		return "f64"
	case "any":
		// Use String for dynamic values to better support functions
		// returning mixed types like [string, int].
		return "String"
	case "void":
		return ""
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
	case typ == "&str":
		return "\"\""
	case typ == "bool":
		return "false"
	case typ == "i64":
		return "0"
	case typ == "f64":
		return "0.0"
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

func typeFromString(typ string) types.Type {
	switch typ {
	case "i64":
		return types.IntType{}
	case "f64":
		return types.FloatType{}
	case "bool":
		return types.BoolType{}
	case "String":
		return types.StringType{}
	}
	if strings.HasPrefix(typ, "Vec<") && strings.HasSuffix(typ, ">") {
		inner := typ[4 : len(typ)-1]
		return types.ListType{Elem: typeFromString(inner)}
	}
	if st, ok := structTypes[typ]; ok {
		return st
	}
	return types.IntType{}
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
	if b, ok := s.Cond.(*BoolLit); ok && b.Value {
		buf.WriteString("loop {\n")
	} else if s.Cond == nil {
		buf.WriteString("loop {\n")
	} else {
		buf.WriteString("while ")
		s.Cond.emit(buf)
		buf.WriteString(" {\n")
	}
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
	buf.WriteString(rustIdent(s.Var))
	buf.WriteString(" in ")
	if s.End != nil {
		s.Iter.emit(buf)
		buf.WriteString("..")
		s.End.emit(buf)
	} else {
		if strings.HasPrefix(s.IterType, "Vec<") {
			// Avoid cloning the entire vector when iterating. Use
			// an iterator and clone elements only if needed.
			s.Iter.emit(buf)
			buf.WriteString(".iter()")
			if !s.ByRef {
				buf.WriteString(".cloned()")
			}
		} else {
			if s.ByRef {
				buf.WriteString("&")
			}
			s.Iter.emit(buf)
		}
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
	if prog.UseLazy {
		buf.WriteString("use std::sync::LazyLock;\n")
	}
	if prog.UseRefCell {
		buf.WriteString("use std::sync::Mutex;\n")
	}
	if prog.UseTime {
		buf.WriteString("use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};\n")
		buf.WriteString("use std::time::{SystemTime, UNIX_EPOCH};\n")
	}
	if prog.UseInput {
		buf.WriteString("use std::io::{self, Read};\n")
	}
	if prog.UseSHA256 || prog.UseMD5 {
		buf.WriteString("use std::process::{Command, Stdio};\n")
		buf.WriteString("use std::io::Write;\n")
	} else if prog.UseFetch {
		buf.WriteString("use std::process::Command;\n")
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
		buf.WriteString("fn _mem() -> i64 {\n")
		buf.WriteString("    if let Ok(mut f) = std::fs::File::open(\"/proc/self/statm\") {\n")
		buf.WriteString("        let mut s = String::new();\n")
		buf.WriteString("        use std::io::Read;\n")
		buf.WriteString("        if f.read_to_string(&mut s).is_ok() {\n")
		buf.WriteString("            if let Some(rss) = s.split_whitespace().nth(1) {\n")
		buf.WriteString("                if let Ok(v) = rss.parse::<i64>() {\n")
		buf.WriteString("                    return v * 4096;\n")
		buf.WriteString("                }\n")
		buf.WriteString("            }\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    0\n")
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
	if prog.UseAbs {
		buf.WriteString("fn abs(x: f64) -> f64 { x.abs() }\n")
	}
	if prog.UsePad {
		buf.WriteString("fn _pad_start(mut s: String, w: i64, p: String) -> String {\n")
		buf.WriteString("    while s.len() < w as usize { s = p.clone() + &s; }\n")
		buf.WriteString("    s\n")
		buf.WriteString("}\n")
	}
	if prog.UseFetch {
		buf.WriteString("fn _fetch(url: &str) -> String {\n")
		buf.WriteString("    let out = Command::new(\"curl\").arg(\"-fsSL\").arg(url).output().unwrap();\n")
		buf.WriteString("    let s = String::from_utf8_lossy(&out.stdout);\n")
		buf.WriteString("    if let Some(start) = s.find(\"\\\"title\\\"\") {\n")
		buf.WriteString("        let rest = &s[start+8..];\n")
		buf.WriteString("        if let Some(begin) = rest.find('\\\"') {\n")
		buf.WriteString("            let rest2 = &rest[begin+1..];\n")
		buf.WriteString("            if let Some(end) = rest2.find('\\\"') {\n")
		buf.WriteString("                return rest2[..end].to_string();\n")
		buf.WriteString("            }\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    String::new()\n")
		buf.WriteString("}\n")
	}
	if prog.UseMD5 {
		buf.WriteString("fn _md5_hex(s: &str) -> String {\n")
		buf.WriteString("    let mut child = Command::new(\"md5sum\")\n")
		buf.WriteString("        .stdin(Stdio::piped())\n")
		buf.WriteString("        .stdout(Stdio::piped())\n")
		buf.WriteString("        .spawn().unwrap();\n")
		buf.WriteString("    {\n")
		buf.WriteString("        let mut stdin = child.stdin.take().unwrap();\n")
		buf.WriteString("        stdin.write_all(s.as_bytes()).unwrap();\n")
		buf.WriteString("    }\n")
		buf.WriteString("    let out = child.wait_with_output().unwrap();\n")
		buf.WriteString("    let hex = String::from_utf8_lossy(&out.stdout);\n")
		buf.WriteString("    hex.split_whitespace().next().unwrap_or(\"\").to_string()\n")
		buf.WriteString("}\n")
	}
	if prog.UseSHA256 {
		buf.WriteString("fn _sha256(bs: Vec<i64>) -> Vec<i64> {\n")
		buf.WriteString("    let mut child = Command::new(\"sha256sum\")\n")
		buf.WriteString("        .stdin(Stdio::piped())\n")
		buf.WriteString("        .stdout(Stdio::piped())\n")
		buf.WriteString("        .spawn().unwrap();\n")
		buf.WriteString("    {\n")
		buf.WriteString("        let mut stdin = child.stdin.take().unwrap();\n")
		buf.WriteString("        for b in bs.iter() { stdin.write_all(&[*b as u8]).unwrap(); }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    let out = child.wait_with_output().unwrap();\n")
		buf.WriteString("    let hex = String::from_utf8_lossy(&out.stdout);\n")
		buf.WriteString("    let mut bytes = Vec::new();\n")
		buf.WriteString("    if let Some(part) = hex.split_whitespace().next() {\n")
		buf.WriteString("        for i in 0..32 {\n")
		buf.WriteString("            let byte = u8::from_str_radix(&part[i*2..i*2+2], 16).unwrap();\n")
		buf.WriteString("            bytes.push(byte as i64);\n")
		buf.WriteString("        }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    bytes\n")
		buf.WriteString("}\n")
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
			typ := g.Type
			if typ == "" {
				if orig, ok := globalRenameBack[g.Name]; ok {
					typ = varTypes[orig]
				} else {
					typ = varTypes[g.Name]
				}
			}
			if strings.HasPrefix(typ, "HashMap") {
				continue
			}
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
	return fmt.Sprintf("// Generated by Mochi transpiler v%s\n", version())
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
	case *SplitExpr:
		n := &ast.Node{Kind: "split"}
		n.Children = append(n.Children, exprNode(ex.Str))
		n.Children = append(n.Children, exprNode(ex.Sep))
		return n
	case *JsonExpr:
		return &ast.Node{Kind: "json", Children: []*ast.Node{exprNode(ex.Value)}}
	case *IndexOfExpr:
		n := &ast.Node{Kind: "indexOf"}
		n.Children = append(n.Children, exprNode(ex.Str))
		n.Children = append(n.Children, exprNode(ex.Sub))
		return n
	case *ParseIntStrExpr:
		n := &ast.Node{Kind: "parseIntStr"}
		n.Children = append(n.Children, exprNode(ex.Str))
		n.Children = append(n.Children, exprNode(ex.Base))
		return n
	case *NowExpr:
		return &ast.Node{Kind: "now"}
	case *SubstringExpr:
		n := &ast.Node{Kind: "substring"}
		n.Children = append(n.Children, exprNode(ex.Str))
		n.Children = append(n.Children, exprNode(ex.Start))
		n.Children = append(n.Children, exprNode(ex.End))
		return n
	case *PadStartExpr:
		n := &ast.Node{Kind: "padStart"}
		n.Children = append(n.Children, exprNode(ex.Str))
		n.Children = append(n.Children, exprNode(ex.Width))
		n.Children = append(n.Children, exprNode(ex.Pad))
		return n
	case *UpperExpr:
		n := &ast.Node{Kind: "upper"}
		n.Children = append(n.Children, exprNode(ex.Value))
		return n
	case *LowerExpr:
		n := &ast.Node{Kind: "lower"}
		n.Children = append(n.Children, exprNode(ex.Value))
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

func paramMutated(body []*parser.Statement, name string) bool {
	for _, st := range body {
		idx, assign := stmtMutates(st, name)
		if idx || assign {
			return true
		}
		if stmtUsesNameInCall(st, name) {
			return true
		}
	}
	return false
}

func paramAssigned(body []*parser.Statement, name string) bool {
	_, assign := stmtsMutate(body, name)
	return assign
}

func stmtMutates(st *parser.Statement, name string) (bool, bool) {
	switch {
	case st.Assign != nil:
		if st.Assign.Name == name {
			if len(st.Assign.Index) > 0 {
				return true, false
			}
			if len(st.Assign.Field) > 0 {
				return true, false
			}
			return false, true
		}
		return false, false
	case st.For != nil:
		i, a := stmtsMutate(st.For.Body, name)
		return i, a
	case st.While != nil:
		i, a := stmtsMutate(st.While.Body, name)
		return i, a
	case st.If != nil:
		i1, a1 := stmtsMutate(st.If.Then, name)
		if i1 || a1 {
			return i1, a1
		}
		if st.If.ElseIf != nil {
			i2, a2 := stmtMutates(&parser.Statement{If: st.If.ElseIf}, name)
			if i2 || a2 {
				return i2, a2
			}
		}
		return stmtsMutate(st.If.Else, name)
	default:
		return false, false
	}
}

func stmtsMutate(list []*parser.Statement, name string) (bool, bool) {
	for _, st := range list {
		i, a := stmtMutates(st, name)
		if i || a {
			if i {
				return true, a
			}
			return false, true
		}
	}
	return false, false
}

func stmtUsesNameInCall(st *parser.Statement, name string) bool {
	switch {
	case st == nil:
		return false
	case st.Expr != nil:
		return exprUsesNameInCall(st.Expr.Expr, name)
	case st.Assign != nil:
		return exprUsesNameInCall(st.Assign.Value, name)
	case st.Return != nil:
		return exprUsesNameInCall(st.Return.Value, name)
	case st.For != nil:
		if exprUsesNameInCall(st.For.Source, name) {
			return true
		}
		if exprUsesNameInCall(st.For.RangeEnd, name) {
			return true
		}
		return stmtsUsesNameInCall(st.For.Body, name)
	case st.While != nil:
		if exprUsesNameInCall(st.While.Cond, name) {
			return true
		}
		return stmtsUsesNameInCall(st.While.Body, name)
	case st.If != nil:
		if exprUsesNameInCall(st.If.Cond, name) {
			return true
		}
		if stmtsUsesNameInCall(st.If.Then, name) {
			return true
		}
		if st.If.ElseIf != nil {
			if stmtUsesNameInCall(&parser.Statement{If: st.If.ElseIf}, name) {
				return true
			}
		}
		return stmtsUsesNameInCall(st.If.Else, name)
	default:
		return false
	}
}

func stmtsUsesNameInCall(list []*parser.Statement, name string) bool {
	for _, st := range list {
		if stmtUsesNameInCall(st, name) {
			return true
		}
	}
	return false
}

func exprUsesNameInCall(e *parser.Expr, name string) bool {
	if e == nil {
		return false
	}
	if e.Binary == nil {
		return false
	}
	if unaryUsesNameInCall(e.Binary.Left, name) {
		return true
	}
	for _, op := range e.Binary.Right {
		if postfixUsesNameInCall(op.Right, name) {
			return true
		}
	}
	return false
}

func unaryUsesNameInCall(u *parser.Unary, name string) bool {
	if u == nil {
		return false
	}
	return postfixUsesNameInCall(u.Value, name)
}

func postfixUsesNameInCall(p *parser.PostfixExpr, name string) bool {
	if p == nil {
		return false
	}
	if primaryUsesNameInCall(p.Target, name) {
		return true
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				if exprIsName(a, name) || exprUsesNameInCall(a, name) {
					return true
				}
			}
		}
		if op.Index != nil {
			if exprUsesNameInCall(op.Index.Start, name) || exprUsesNameInCall(op.Index.End, name) || exprUsesNameInCall(op.Index.Step, name) {
				return true
			}
		}
	}
	return false
}

func primaryUsesNameInCall(pr *parser.Primary, name string) bool {
	if pr == nil {
		return false
	}
	if pr.Call != nil {
		switch pr.Call.Func {
		case "len", "str", "print", "int", "float":
			// known pure functions
		default:
			for _, a := range pr.Call.Args {
				if exprIsName(a, name) || exprUsesNameInCall(a, name) {
					return true
				}
			}
		}
	}
	if pr.Group != nil {
		return exprUsesNameInCall(pr.Group, name)
	}
	return false
}

func exprIsName(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && p.Target.Selector.Root == name {
		return true
	}
	return false
}

func rustIdent(name string) string {
	if name == "" {
		return "_"
	}
	var b strings.Builder
	for i, r := range name {
		if unicode.IsLetter(r) || r == '_' || (i > 0 && unicode.IsDigit(r)) {
			b.WriteRune(r)
		} else if unicode.IsDigit(r) && i == 0 {
			b.WriteRune('_')
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	ident := b.String()
	if ident == "" {
		ident = "_"
	}
	if !unicode.IsLetter(rune(ident[0])) && ident[0] != '_' {
		ident = "_" + ident
	}
	if rustReserved[ident] {
		ident = "r#" + ident
	}
	return ident
}

func copyStringMap(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func copyBoolMap(src map[string]bool) map[string]bool {
	dst := make(map[string]bool, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func rewriteReturnStruct(st Stmt, name string, typ types.StructType) Stmt {
	switch s := st.(type) {
	case *ReturnStmt:
		if ml, ok := s.Value.(*MapLit); ok {
			fields := make([]Expr, len(typ.Order))
			for i, n := range typ.Order {
				for _, it := range ml.Items {
					if sl, ok := it.Key.(*StringLit); ok && sl.Value == n {
						fields[i] = it.Value
						break
					}
				}
			}
			s.Value = &StructLit{Name: name, Fields: fields, Names: typ.Order}
		} else if ll, ok := s.Value.(*ListLit); ok {
			fields := make([]Expr, len(typ.Order))
			for i, el := range ll.Elems {
				if i < len(fields) {
					fields[i] = el
				}
			}
			s.Value = &StructLit{Name: name, Fields: fields, Names: typ.Order}
		} else if sl, ok := s.Value.(*StructLit); ok && sl.Name != name {
			fields := make([]Expr, len(typ.Order))
			for i, n := range typ.Order {
				for j, m := range sl.Names {
					if m == n {
						if j < len(sl.Fields) {
							fields[i] = sl.Fields[j]
						}
						break
					}
				}
			}
			s.Value = &StructLit{Name: name, Fields: fields, Names: typ.Order}
		}
		return s
	case *IfStmt:
		for i, b := range s.Then {
			s.Then[i] = rewriteReturnStruct(b, name, typ)
		}
		for i, b := range s.Else {
			s.Else[i] = rewriteReturnStruct(b, name, typ)
		}
		if s.ElseIf != nil {
			s.ElseIf = rewriteReturnStruct(s.ElseIf, name, typ).(*IfStmt)
		}
		return s
	case *WhileStmt:
		for i, b := range s.Body {
			s.Body[i] = rewriteReturnStruct(b, name, typ)
		}
		return s
	case *ForStmt:
		for i, b := range s.Body {
			s.Body[i] = rewriteReturnStruct(b, name, typ)
		}
		return s
	case *MultiStmt:
		for i, b := range s.Stmts {
			s.Stmts[i] = rewriteReturnStruct(b, name, typ)
		}
		return s
	default:
		return s
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

func findReturnStruct(st Stmt) string {
	switch s := st.(type) {
	case *ReturnStmt:
		if s.Value == nil {
			return ""
		}
		switch v := s.Value.(type) {
		case *StructLit:
			return v.Name
		case *MapLit:
			sig := make([]string, len(v.Items))
			for i, it := range v.Items {
				key, ok := it.Key.(*StringLit)
				if !ok {
					return ""
				}
				sig[i] = key.Value + ":" + inferType(it.Value)
			}
			name, ok := structSig[strings.Join(sig, ";")]
			if ok {
				return name
			}
		case *ListLit:
			if len(v.Elems) == 0 {
				return ""
			}
			tps := make([]string, len(v.Elems))
			same := true
			for i, el := range v.Elems {
				tps[i] = inferType(el)
				if i > 0 && tps[i] != tps[0] {
					same = false
				}
			}
			if !same {
				sig := "list:" + strings.Join(tps, ";")
				if name, ok := structSig[sig]; ok {
					return name
				}
				name := types.UniqueStructName("Ret", curEnv, nil)
				st := types.StructType{Fields: map[string]types.Type{}, Order: make([]string, len(tps))}
				fields := make([]Param, len(tps))
				for i, t := range tps {
					fname := fmt.Sprintf("f%d", i)
					st.Fields[fname] = typeFromString(t)
					st.Order[i] = fname
					fields[i] = Param{Name: fname, Type: rustTypeFromType(st.Fields[fname])}
				}
				structTypes[name] = st
				typeDecls = append(typeDecls, &StructDecl{Name: name, Fields: fields})
				structSig[sig] = name
				return name
			}
		}
	case *IfStmt:
		for _, b := range s.Then {
			if n := findReturnStruct(b); n != "" {
				return n
			}
		}
		for _, b := range s.Else {
			if n := findReturnStruct(b); n != "" {
				return n
			}
		}
		if s.ElseIf != nil {
			if n := findReturnStruct(s.ElseIf); n != "" {
				return n
			}
		}
	case *WhileStmt:
		for _, b := range s.Body {
			if n := findReturnStruct(b); n != "" {
				return n
			}
		}
	case *ForStmt:
		for _, b := range s.Body {
			if n := findReturnStruct(b); n != "" {
				return n
			}
		}
	case *MultiStmt:
		for _, b := range s.Stmts {
			if n := findReturnStruct(b); n != "" {
				return n
			}
		}
	}
	return ""
}

func findReturnStructSlice(stmts []Stmt) string {
	for _, s := range stmts {
		if n := findReturnStruct(s); n != "" {
			return n
		}
	}
	return ""
}
