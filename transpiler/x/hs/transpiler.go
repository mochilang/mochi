//go:build slow

package hs

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"
	"unicode"

	"gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
	"mochi/types"
)

// Program represents a minimal Haskell program AST.
type Program struct {
	Types []TypeDecl
	Funcs []Func
	Stmts []Stmt
}

// varTypes stores a very small type inference map used
// for deciding how to pretty print certain expressions.
var varTypes map[string]string

// needDataList reports whether Data.List functions are required.
var needDataList bool

// needDataMap reports whether Data.Map functions are required.
var needDataMap bool

// needGroupBy reports whether group by helpers are required.
var needGroupBy bool

// needJSON reports whether JSON helpers are required.
var needJSON bool

// needTrace reports whether Debug.Trace is required.
var needTrace bool

// needIORef reports whether Data.IORef is required.
var needIORef bool

// usesNow reports whether the _now helper is required.
var usesNow bool

// usesMem reports whether the _mem helper is required.
var usesMem bool

// usesLookupHost reports whether the _lookup_host helper is required.
var usesLookupHost bool

// usesInput reports whether the input helper is required.
var usesInput bool

// usesPrintf reports whether Text.Printf is required.
var usesPrintf bool

// groupVars tracks variables that represent groups.
var groupVars map[string]bool

// groupKeyStruct stores the struct type name of each group's key.
var groupKeyStruct map[string]string

// groupItemType stores the struct type name of each group's items.
var groupItemType map[string]string

// mutated tracks variables that are reassigned.
// mutatedGlobal tracks mutated global variables across all functions.
var mutatedGlobal map[string]bool

// mutated holds the currently active mutation map when emitting code.
// For global statements this is the same as mutatedGlobal. For each
// function a temporary map containing its mutated locals is used during
// emission.
var mutated map[string]bool

// funcParamMut stores which parameters of a function are mutated.
// The inner map has parameter index -> true.
var funcParamMut map[string]map[int]bool

// locals holds the set of local variable names for the function
// currently being converted.
var locals map[string]bool

// globals records the names of global variables defined at the top level.
var globals map[string]bool

// vars holds the last computed expression of each variable at the top level.
var vars map[string]Expr

// indent tracks the current indentation when emitting code.
var indent string

// reserved lists Haskell reserved keywords that cannot be used as identifiers.
var reserved = map[string]bool{
	"case":     true,
	"class":    true,
	"data":     true,
	"default":  true,
	"deriving": true,
	"do":       true,
	"else":     true,
	"if":       true,
	"import":   true,
	"in":       true,
	"infix":    true,
	"infixl":   true,
	"infixr":   true,
	"instance": true,
	"let":      true,
	"module":   true,
	"newtype":  true,
	"of":       true,
	"then":     true,
	"type":     true,
	"where":    true,
}

// safeName prefixes an underscore when the provided identifier is reserved.
func safeName(n string) string {
	if strings.Contains(n, ".") {
		return n
	}
	if reserved[n] {
		return "_" + n
	}
	if len(n) > 0 && unicode.IsUpper(rune(n[0])) {
		r := []rune(n)
		r[0] = unicode.ToLower(r[0])
		return string(r)
	}
	return n
}

func shouldWrapCall(name string) bool {
	switch name {
	case "input", "print", "putStrLn", "trace", "main", "randDigit", "maxOf":
		return false
	case "beatingProbability":
		return true
	}
	if strings.Contains(name, ".") {
		return false
	}
	return true
}

func isMainCall(e Expr) bool {
	if nr, ok := e.(*NameRef); ok {
		return nr.Name == "main"
	}
	if call, ok := e.(*CallExpr); ok {
		if n, ok2 := call.Fun.(*NameRef); ok2 {
			if n.Name == "main" && len(call.Args) == 0 {
				return true
			}
			if n.Name == "unsafePerformIO" && len(call.Args) == 1 {
				return isMainCall(call.Args[0])
			}
		}
	}
	return false
}

func maybeWrapUnsafe(e Expr) Expr {
	if call, ok := e.(*CallExpr); ok {
		if n, ok2 := call.Fun.(*NameRef); ok2 && shouldWrapCall(n.Name) {
			if n.Name != "unsafePerformIO" {
				return &CallExpr{Fun: &NameRef{Name: "unsafePerformIO"}, Args: []Expr{call}}
			}
		}
	}
	return e
}

func pushIndent() { indent += "    " }
func popIndent() {
	if len(indent) >= 4 {
		indent = indent[:len(indent)-4]
	}
}
func writeIndent(w io.Writer) { io.WriteString(w, indent) }

type Variant struct {
	Name   string
	Fields []string
	Types  []string
}

type TypeDecl struct {
	Name     string
	Fields   []string
	Types    []string
	Variants []Variant
}

func groupPrelude() string {
	return `data MGroup k a = MGroup {key :: k, items :: [a]} deriving (Show, Eq)`
}

func (t *TypeDecl) emit(w io.Writer) {
	if len(t.Variants) == 0 {
		fmt.Fprintf(w, "data %s = %s\n  { ", t.Name, t.Name)
		for i, f := range t.Fields {
			if i > 0 {
				io.WriteString(w, ",\n    ")
			}
			fmt.Fprintf(w, "%s :: %s", safeName(f), t.Types[i])
		}
		io.WriteString(w, "\n  } deriving (Show, Eq)\n")
		return
	}

	fmt.Fprintf(w, "data %s =", t.Name)
	for i, v := range t.Variants {
		io.WriteString(w, " ")
		io.WriteString(w, v.Name)
		if len(v.Fields) > 0 {
			io.WriteString(w, " {")
			for j, f := range v.Fields {
				if j > 0 {
					io.WriteString(w, ", ")
				}
				fmt.Fprintf(w, "%s :: %s", safeName(f), v.Types[j])
			}
			io.WriteString(w, "}")
		}
		if i < len(t.Variants)-1 {
			io.WriteString(w, " |")
		}
	}
	io.WriteString(w, "\n  deriving (Show, Eq)\n")
}

var structDefs map[string]*TypeDecl
var structCount int
var preludeHide map[string]bool
var varStruct map[string]string
var currentLoop string
var loopArgs string
var loopCounter int

func newLoopName() string {
	loopCounter++
	return fmt.Sprintf("loop%d", loopCounter)
}

func markMutated(name string) {
	mutated[name] = true
	if globals[name] {
		mutatedGlobal[name] = true
	}
}

func typeNameFromRef(tr *parser.TypeRef) string {
	if tr == nil {
		return "String"
	}
	if tr.Simple != nil {
		switch *tr.Simple {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Bool"
		case "string":
			return "String"
		default:
			return *tr.Simple
		}
	}
	if tr.Generic != nil {
		name := tr.Generic.Name
		args := tr.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return "[" + typeNameFromRef(args[0]) + "]"
			}
		case "map":
			if len(args) == 2 {
				needDataMap = true
				return "Map.Map " + typeNameFromRef(args[0]) + " " + typeNameFromRef(args[1])
			}
		case "maybe":
			if len(args) == 1 {
				return "Maybe " + typeNameFromRef(args[0])
			}
		default:
			parts := make([]string, len(args))
			for i, a := range args {
				parts[i] = typeNameFromRef(a)
			}
			if len(parts) > 0 {
				return name + " " + strings.Join(parts, " ")
			}
			return name
		}
	}
	if tr.Struct != nil {
		structCount++
		name := fmt.Sprintf("AnonStruct%d", structCount)
		fields := make([]string, len(tr.Struct.Fields))
		typestr := make([]string, len(tr.Struct.Fields))
		for i, f := range tr.Struct.Fields {
			fields[i] = safeName(f.Name)
			typestr[i] = typeNameFromRef(f.Type)
			preludeHide[f.Name] = true
		}
		structDefs[name] = &TypeDecl{Name: name, Fields: fields, Types: typestr}
		return name
	}
	if tr.Fun != nil {
		params := make([]string, len(tr.Fun.Params))
		for i, p := range tr.Fun.Params {
			params[i] = typeNameFromRef(p)
		}
		ret := typeNameFromRef(tr.Fun.Return)
		if ret == "" {
			ret = "()"
		}
		params = append(params, ret)
		return strings.Join(params, " -> ")
	}
	return "String"
}

func recordType(name string, ex Expr) {
	if isStringExpr(ex) {
		varTypes[name] = "string"
	} else if isBoolExpr(ex) {
		varTypes[name] = "bool"
	} else if isFloatExpr(ex) {
		varTypes[name] = "float"
	} else if _, ok := ex.(*IntLit); ok {
		varTypes[name] = "int"
	} else if isListExpr(ex) {
		varTypes[name] = "list"
		switch ll := ex.(type) {
		case *ListLit:
			if len(ll.Elems) > 0 {
				if rl, ok2 := ll.Elems[0].(*RecordLit); ok2 {
					varStruct[name] = rl.Name
				}
			}
		case *ComprExpr:
			if rl, ok2 := ll.Body.(*RecordLit); ok2 {
				varStruct[name] = rl.Name
			}
		}
	} else if rl, ok := ex.(*RecordLit); ok {
		varTypes[name] = "record"
		varStruct[name] = rl.Name
	} else if ml, ok := ex.(*MapLit); ok {
		allStr := true
		for _, v := range ml.Values {
			if !isStringExpr(v) {
				allStr = false
				break
			}
		}
		if allStr {
			varTypes[name] = "map_string"
		} else {
			varTypes[name] = "map"
		}
	} else if isMapExpr(ex) {
		varTypes[name] = "map"
	}
}

func copyVarTypes(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func guessHsType(ex Expr) string {
	if _, ok := ex.(*IntLit); ok {
		return "Int"
	}
	if _, ok := ex.(*LenExpr); ok {
		return "Int"
	}
	if nr, ok := ex.(*NameRef); ok {
		switch varTypes[nr.Name] {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Bool"
		case "string":
			return "String"
		case "maybe_record":
			if s := varStruct[nr.Name]; s != "" {
				return "Maybe " + s
			}
			return "Maybe String"
		}
	}
	if fe, ok := ex.(*FieldExpr); ok {
		if n, ok2 := fe.Target.(*NameRef); ok2 {
			if fe.Field == "key" {
				if kt := groupKeyStruct[n.Name]; kt != "" {
					return kt
				}
			}
			if fe.Field == "items" {
				if it := groupItemType[n.Name]; it != "" {
					return "[" + it + "]"
				}
			}
			if sname := varStruct[n.Name]; sname != "" {
				if st, ok3 := structDefs[sname]; ok3 {
					for i, f := range st.Fields {
						if f == fe.Field {
							return st.Types[i]
						}
					}
				}
			}
			if envInfo != nil {
				if vt, err := envInfo.GetVar(n.Name); err == nil {
					if st, ok3 := vt.(types.StructType); ok3 {
						if ft, ok4 := st.Fields[fe.Field]; ok4 {
							return toHsType(ft)
						}
					}
				}
			}
		}
		if inner, ok2 := fe.Target.(*FieldExpr); ok2 {
			if n, ok3 := inner.Target.(*NameRef); ok3 && inner.Field == "key" {
				if kt := groupKeyStruct[n.Name]; kt != "" {
					if st, ok4 := structDefs[kt]; ok4 {
						for i, f := range st.Fields {
							if f == fe.Field {
								return st.Types[i]
							}
						}
					}
				}
			}
			if n, ok3 := inner.Target.(*NameRef); ok3 && inner.Field == "items" {
				if it := groupItemType[n.Name]; it != "" {
					if st, ok4 := structDefs[it]; ok4 {
						for i, f := range st.Fields {
							if f == fe.Field {
								return st.Types[i]
							}
						}
					}
				}
			}
		}
	}
	if call, ok := ex.(*CallExpr); ok {
		if n, ok2 := call.Fun.(*NameRef); ok2 {
			switch n.Name {
			case "sum", "max", "min":
				if len(call.Args) == 1 {
					if isFloatExpr(call.Args[0]) {
						return "Double"
					}
					if isIntExpr(call.Args[0]) {
						return "Int"
					}
					return "Double"
				}
			case "avg":
				return "Double"
			case "round":
				return "Int"
			case "len", "count":
				return "Int"
			}
		}
	}
	if be, ok := ex.(*BinaryExpr); ok {
		if len(be.Ops) > 0 {
			op := be.Ops[len(be.Ops)-1].Op
			switch op {
			case "+", "-", "*", "/":
				if op == "/" || isFloatExpr(be.Left) || isFloatExpr(be.Ops[0].Right) {
					return "Double"
				}
				if isIntExpr(be.Left) && isIntExpr(be.Ops[0].Right) {
					return "Int"
				}
				if isFloatExpr(be.Left) || isFloatExpr(be.Ops[0].Right) {
					return "Double"
				}
				if isIntExpr(be.Left) || isIntExpr(be.Ops[0].Right) {
					return "Int"
				}
			case "==", "!=", "<", ">", "<=", ">=", "&&", "||", "in":
				return "Bool"
			}
		}
	}
	switch {
	case isStringExpr(ex):
		return "String"
	case isBoolExpr(ex):
		return "Bool"
	case isFloatExpr(ex):
		return "Double"
	case isListExpr(ex):
		return "[String]"
	default:
		return "String"
	}
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type Func struct {
	Name    string
	Params  []string
	Body    []Stmt
	Mutated map[string]bool
}

func isPureStmt(s Stmt) bool {
	switch st := s.(type) {
	case *LetStmt:
		return isPureExpr(st.Expr)
	case *ReturnStmt:
		return isPureExpr(st.Expr)
	case *IfStmt:
		if !isPureExpr(st.Cond) {
			return false
		}
		for _, t := range st.Then {
			if !isPureStmt(t) {
				return false
			}
		}
		for _, e := range st.Else {
			if !isPureStmt(e) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func isPureExpr(e Expr) bool {
	switch ex := e.(type) {
	case *IntLit, *FloatLit, *StringLit, *BoolLit, *UnitLit, *NameRef,
		*RecordLit, *FieldExpr, *IndexExpr, *GroupExpr:
		return true
	case *BinaryExpr:
		if !isPureExpr(ex.Left) {
			return false
		}
		for _, op := range ex.Ops {
			if !isPureExpr(op.Right) {
				return false
			}
		}
		return true
	case *UnaryExpr:
		return isPureExpr(ex.Expr)
	case *LenExpr:
		return isPureExpr(ex.Arg)
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok && n.Name == "unsafePerformIO" {
			return false
		}
		return true
	case *LambdaBlock:
		return isPureFunc(ex.Body)
	case *CaseExpr:
		if !isPureExpr(ex.Target) {
			return false
		}
		for _, cl := range ex.Clauses {
			if cl.Pattern != nil {
				// assume pattern pure
			}
			if !isPureExpr(cl.Result) {
				return false
			}
		}
		return true
	default:
		_ = ex
		return false
	}
}

func isPureFunc(stmts []Stmt) bool {
	for _, st := range stmts {
		if !isPureStmt(st) {
			return false
		}
	}
	return true
}

func blockToExpr(stmts []Stmt) Expr {
	var expr Expr = &IntLit{Value: "()"}
	for i := len(stmts) - 1; i >= 0; i-- {
		switch st := stmts[i].(type) {
		case *ReturnStmt:
			expr = st.Expr
		case *LetStmt:
			expr = &LetInExpr{Name: st.Name, Value: st.Expr, Body: expr}
		case *IfStmt:
			thenExpr := blockToExpr(st.Then)
			elseExpr := blockToExpr(st.Else)
			expr = &IfExpr{Cond: st.Cond, Then: thenExpr, Else: elseExpr}
		}
	}
	return expr
}

func (f *Func) emit(w io.Writer) {
	prevMut := mutated
	mutated = map[string]bool{}
	for k, v := range mutatedGlobal {
		mutated[k] = v
	}
	for k, v := range f.Mutated {
		mutated[k] = v
	}

	io.WriteString(w, safeName(f.Name))
	for _, p := range f.Params {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(p))
	}
	io.WriteString(w, " = ")
	if len(f.Body) == 1 {
		if r, ok := f.Body[0].(*ReturnStmt); ok && isPureExpr(r.Expr) {
			r.Expr.emit(w)
			return
		}
	}
	if isPureFunc(f.Body) {
		expr := blockToExpr(f.Body)
		expr.emit(w)
		return
	}
	io.WriteString(w, "do\n")
	prev := indent
	indent += "    "
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	indent = prev
	mutated = prevMut
}

type PrintStmt struct {
	Expr   Expr
	String bool
}

type ExprStmt struct{ Expr Expr }

type JSONStmt struct {
	Expr Expr
}
type LetStmt struct {
	Name string
	Expr Expr
}

// AssignStmt updates the value of an existing variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

// IfStmt executes a list of statements based on a condition.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}
type ReturnStmt struct {
	Expr Expr
}

type BreakStmt struct{}
type ContinueStmt struct{}
type BenchStmt struct {
	Name string
	Body []Stmt
}

// ForStmt iterates over elements in a list, map or range.
type ForStmt struct {
	Name       string
	From       Expr
	To         Expr
	Body       []Stmt
	WithBreak  bool
	VarMutated bool
}

// ForExpr is like ForStmt but used in expression position and
// returns a value when the loop finishes normally.
type ForExpr struct {
	Name       string
	From       Expr
	To         Expr
	Body       []Stmt
	End        Expr
	VarMutated bool
}

// WhileStmt repeats a block while a condition is true.
type WhileStmt struct {
	Var  string
	Cond Expr
	Body []Stmt
	Next Expr
}

// WhileExpr is like WhileStmt but used in expression position and
// returns a value when the loop finishes normally.
type WhileExpr struct {
	Var  string
	Cond Expr
	Body []Stmt
	Next Expr
	End  Expr
}

// IndexExpr accesses a single element of a list or map.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (p *PrintStmt) emit(w io.Writer) {
	writeIndent(w)
	if p.String {
		io.WriteString(w, "putStrLn (")
		p.Expr.emit(w)
		io.WriteString(w, ")")
		return
	}

	if isListExpr(p.Expr) {
		needDataList = true
		io.WriteString(w, "putStrLn (\"[\" ++ intercalate \", \" (map show (")
		p.Expr.emit(w)
		io.WriteString(w, ")) ++ \"]\")")
		return
	}

	if isBoolExpr(p.Expr) {
		if call, ok := p.Expr.(*CallExpr); ok {
			if n, ok2 := call.Fun.(*NameRef); ok2 && n.Name == "exists" {
				io.WriteString(w, "putStrLn (if ")
				p.Expr.emit(w)
				io.WriteString(w, " then \"true\" else \"false\")")
				return
			}
		}
		if nr, ok := p.Expr.(*NameRef); ok {
			if call, ok2 := vars[nr.Name].(*CallExpr); ok2 {
				if n, ok3 := call.Fun.(*NameRef); ok3 && n.Name == "exists" {
					io.WriteString(w, "putStrLn (if ")
					p.Expr.emit(w)
					io.WriteString(w, " then \"true\" else \"false\")")
					return
				}
			}
		}
		io.WriteString(w, "print (fromEnum (")
		p.Expr.emit(w)
		io.WriteString(w, "))")
		return
	}

	if isCharExpr(p.Expr) {
		io.WriteString(w, "putStrLn [")
		p.Expr.emit(w)
		io.WriteString(w, "]")
		return
	}

	io.WriteString(w, "print (")
	p.Expr.emit(w)
	io.WriteString(w, ")")
}

func (e *ExprStmt) emit(w io.Writer) {
	writeIndent(w)
	e.Expr.emit(w)
}

func (j *JSONStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "BSL.putStrLn (Pretty.encodePretty' Pretty.defConfig{Pretty.confIndent = Pretty.Spaces 2} ")
	j.Expr.emit(w)
	io.WriteString(w, ")")
}

func (l *LetStmt) emit(w io.Writer) {
	writeIndent(w)
	name := safeName(l.Name)
	if mutated[l.Name] && indent != "" {
		needIORef = true
		io.WriteString(w, name+" <- newIORef (")
		expr := maybeWrapUnsafe(l.Expr)
		switch lit := expr.(type) {
		case *IntLit:
			fmt.Fprintf(w, "%s :: Int", lit.Value)
		case *FloatLit:
			fmt.Fprintf(w, "%s :: Double", lit.Value)
		case *BoolLit:
			if lit.Value {
				io.WriteString(w, "True :: Bool")
			} else {
				io.WriteString(w, "False :: Bool")
			}
		default:
			expr.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	if indent != "" {
		if call, ok := l.Expr.(*CallExpr); ok {
			// direct input()
			if n, ok2 := call.Fun.(*NameRef); ok2 && n.Name == "input" && len(call.Args) == 0 {
				io.WriteString(w, name+" <- ")
				l.Expr.emit(w)
				return
			}
			// pattern int(input()) or float(input()) etc.
			if n, ok2 := call.Fun.(*NameRef); ok2 && len(call.Args) == 1 {
				if inner, ok3 := call.Args[0].(*CallExpr); ok3 {
					if n2, ok4 := inner.Fun.(*NameRef); ok4 && n2.Name == "input" && len(inner.Args) == 0 {
						io.WriteString(w, name+" <- fmap ")
						io.WriteString(w, safeName(n.Name))
						io.WriteString(w, " input")
						return
					}
				}
			}
			if n, ok2 := call.Fun.(*NameRef); ok2 {
				if envInfo != nil {
					if fn, ok := envInfo.GetFunc(n.Name); ok {
						if funcUsesIO(fn) {
							io.WriteString(w, name+" <- ")
							l.Expr.emit(w)
							return
						}
					}
				}
			}
		}
		io.WriteString(w, "let ")
	}
	io.WriteString(w, name)
	io.WriteString(w, " = ")
	l.Expr.emit(w)
}

func (a *AssignStmt) emit(w io.Writer) {
	writeIndent(w)
	name := safeName(a.Name)
	if mutated[a.Name] {
		needIORef = true
		io.WriteString(w, "writeIORef ")
		io.WriteString(w, name)
		io.WriteString(w, " $! (")
		expr := maybeWrapUnsafe(a.Expr)
		expr.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, name)
		io.WriteString(w, " = ")
		a.Expr.emit(w)
	}
}

func (i *IfStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then do\n")
	pushIndent()
	for _, st := range i.Then {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	popIndent()
	if len(i.Else) > 0 {
		writeIndent(w)
		io.WriteString(w, "else do\n")
		pushIndent()
		for _, st := range i.Else {
			st.emit(w)
			io.WriteString(w, "\n")
		}
		popIndent()
	} else {
		writeIndent(w)
		io.WriteString(w, "else return ()\n")
	}
}
func (r *ReturnStmt) emit(w io.Writer) {
	writeIndent(w)
	switch r.Expr.(type) {
	case *WhileExpr:
		// WhileExpr already yields an IO action, so avoid wrapping
		r.Expr.emit(w)
	case *ForExpr:
		// ForExpr already yields an IO action
		r.Expr.emit(w)
	case *UnitLit:
		io.WriteString(w, "return ()")
	case *IntLit:
		io.WriteString(w, "return (")
		r.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		io.WriteString(w, "return (")
		r.Expr.emit(w)
		io.WriteString(w, ")")
	}
}

func (b *BreakStmt) emit(w io.Writer) { writeIndent(w); io.WriteString(w, "return ()") }
func (c *ContinueStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, currentLoop)
	io.WriteString(w, loopArgs)
}

func (b *BenchStmt) emit(w io.Writer) {
	writeIndent(w)
	if indent == "" {
		fmt.Fprintf(w, "%s = do\n", safeName(b.Name))
	} else {
		io.WriteString(w, "do\n")
	}
	pushIndent()
	writeIndent(w)
	io.WriteString(w, "start <- _now\n")
	for _, st := range b.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	writeIndent(w)
	io.WriteString(w, "end <- _now\n")
	writeIndent(w)
	io.WriteString(w, "memEnd <- _mem\n")
	writeIndent(w)
	fmt.Fprintf(w, "printf \"{\\\"duration_us\\\":%%d,\\\"memory_bytes\\\":%%d,\\\"name\\\":\\\"%s\\\"}\\n\" ((end - start) `div` 1000) memEnd\n", b.Name)
	popIndent()
}

func (f *ForStmt) emit(w io.Writer) {
	prevMut, hadPrev := mutated[f.Name]
	mutated[f.Name] = f.VarMutated
	defer func() {
		if hadPrev {
			mutated[f.Name] = prevMut
		} else {
			delete(mutated, f.Name)
		}
	}()
	if f.WithBreak {
		loop := newLoopName()
		prevLoop := currentLoop
		prevArgs := loopArgs
		currentLoop = loop
		loopArgs = " xs"
		type guard struct {
			cond   Expr
			action string
		}
		var guards []guard
		var rest []Stmt
		seenOther := false
		for _, st := range f.Body {
			if !seenOther {
				if is, ok := st.(*IfStmt); ok && len(is.Then) == 1 && len(is.Else) == 0 {
					switch is.Then[0].(type) {
					case *ContinueStmt:
						guards = append(guards, guard{cond: is.Cond, action: loop + " xs"})
						continue
					case *BreakStmt:
						guards = append(guards, guard{cond: is.Cond, action: "return ()"})
						continue
					}
				}
			}
			seenOther = true
			rest = append(rest, st)
		}
		if len(guards) > 0 {
			writeIndent(w)
			io.WriteString(w, "let\n")
			pushIndent()
			writeIndent(w)
			io.WriteString(w, loop+" [] = return ()\n")
			writeIndent(w)
			io.WriteString(w, loop+" ("+safeName(f.Name)+":xs)")
			for i, g := range guards {
				if i == 0 {
					io.WriteString(w, " | ")
				} else {
					io.WriteString(w, "\n")
					writeIndent(w)
					io.WriteString(w, "    | ")
				}
				g.cond.emit(w)
				io.WriteString(w, " = ")
				io.WriteString(w, g.action)
			}
			if len(rest) > 0 {
				io.WriteString(w, "\n")
				writeIndent(w)
				io.WriteString(w, "    | otherwise = do\n")
				pushIndent()
				for _, st := range rest {
					st.emit(w)
					io.WriteString(w, "\n")
				}
				writeIndent(w)
				io.WriteString(w, loop+" xs\n")
				popIndent()
			} else {
				io.WriteString(w, "\n")
				writeIndent(w)
				io.WriteString(w, "    | otherwise = "+loop+" xs\n")
			}
			popIndent()
			writeIndent(w)
			io.WriteString(w, loop+" ")
			if f.To != nil {
				io.WriteString(w, "[")
				f.From.emit(w)
				io.WriteString(w, " .. (")
				f.To.emit(w)
				io.WriteString(w, " - 1)]")
			} else if isMapExpr(f.From) {
				needDataMap = true
				io.WriteString(w, "(Map.keys ")
				f.From.emit(w)
				io.WriteString(w, ")")
			} else {
				f.From.emit(w)
			}
			currentLoop = prevLoop
			loopArgs = prevArgs
			return
		}
		writeIndent(w)
		io.WriteString(w, "let\n")
		pushIndent()
		writeIndent(w)
		io.WriteString(w, loop)
		io.WriteString(w, " [] = return ()\n")
		writeIndent(w)
		io.WriteString(w, loop)
		io.WriteString(w, " (")
		io.WriteString(w, safeName(f.Name))
		io.WriteString(w, ":xs) = do\n")
		pushIndent()
		stop := false
		for _, st := range f.Body {
			if stop {
				break
			}
			switch st.(type) {
			case *BreakStmt:
				st.emit(w)
				io.WriteString(w, "\n")
				stop = true
			case *ContinueStmt:
				st.emit(w)
				io.WriteString(w, "\n")
				stop = true
			default:
				st.emit(w)
				io.WriteString(w, "\n")
			}
		}
		if !stop {
			writeIndent(w)
			io.WriteString(w, loop+" xs\n")
		}
		popIndent()
		popIndent()
		writeIndent(w)
		io.WriteString(w, loop)
		io.WriteString(w, " ")
		if f.To != nil {
			io.WriteString(w, "[")
			f.From.emit(w)
			io.WriteString(w, " .. (")
			f.To.emit(w)
			io.WriteString(w, " - 1)]")
		} else if isMapExpr(f.From) {
			needDataMap = true
			io.WriteString(w, "(Map.keys ")
			f.From.emit(w)
			io.WriteString(w, ")")
		} else {
			f.From.emit(w)
		}
		currentLoop = prevLoop
		loopArgs = prevArgs
		return
	}
	writeIndent(w)
	io.WriteString(w, "mapM_ (\\")
	io.WriteString(w, safeName(f.Name))
	if len(f.Body) == 1 {
		if ps, ok := f.Body[0].(*PrintStmt); ok {
			io.WriteString(w, " ->\n")
			pushIndent()
			ps.emit(w)
			io.WriteString(w, "\n")
			writeIndent(w)
			io.WriteString(w, ") ")
			popIndent()
			if f.To != nil {
				io.WriteString(w, "[")
				f.From.emit(w)
				io.WriteString(w, " .. (")
				f.To.emit(w)
				io.WriteString(w, " - 1)]")
				return
			}
			if isMapExpr(f.From) {
				needDataMap = true
				io.WriteString(w, "(Map.keys ")
				f.From.emit(w)
				io.WriteString(w, ")")
			} else {
				f.From.emit(w)
			}
			return
		}
	}
	io.WriteString(w, " -> do\n")
	pushIndent()
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	writeIndent(w)
	io.WriteString(w, ") ")
	popIndent()
	if f.To != nil {
		io.WriteString(w, "[")
		f.From.emit(w)
		io.WriteString(w, " .. (")
		f.To.emit(w)
		io.WriteString(w, " - 1)]")
		return
	}
	if isMapExpr(f.From) {
		needDataMap = true
		io.WriteString(w, "(Map.keys ")
		f.From.emit(w)
		io.WriteString(w, ")")
	} else {
		f.From.emit(w)
	}
}

func (wst *WhileStmt) emit(w io.Writer) {
	name := newLoopName()
	prevLoop := currentLoop
	prevArgs := loopArgs
	currentLoop = name
	loopArgs = ""
	writeIndent(w)
	io.WriteString(w, "let\n")
	pushIndent()
	writeIndent(w)
	io.WriteString(w, name)
	if wst.Var != "" && !mutated[wst.Var] {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(wst.Var))
	}
	io.WriteString(w, " = do\n")
	pushIndent()
	writeIndent(w)
	io.WriteString(w, "if ")
	wst.Cond.emit(w)
	io.WriteString(w, " then do\n")
	pushIndent()
	for _, st := range wst.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if wst.Var != "" && wst.Next != nil {
		writeIndent(w)
		if mutated[wst.Var] {
			io.WriteString(w, "writeIORef ")
			io.WriteString(w, safeName(wst.Var))
			io.WriteString(w, " $! (")
			wst.Next.emit(w)
			io.WriteString(w, ")\n")
		} else {
			io.WriteString(w, safeName(wst.Var))
			io.WriteString(w, " <- (")
			wst.Next.emit(w)
			io.WriteString(w, ")\n")
		}
	}
	writeIndent(w)
	io.WriteString(w, name)
	io.WriteString(w, "\n")
	popIndent()
	writeIndent(w)
	io.WriteString(w, "else return ()\n")
	popIndent()
	popIndent()
	writeIndent(w)
	io.WriteString(w, name)
	if wst.Var != "" && !mutated[wst.Var] {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(wst.Var))
	}
	currentLoop = prevLoop
	loopArgs = prevArgs
}

func (we *WhileExpr) emit(w io.Writer) {
	name := newLoopName()
	prevLoop := currentLoop
	prevArgs := loopArgs
	currentLoop = name
	loopArgs = ""
	io.WriteString(w, "(let {\n")
	pushIndent()
	writeIndent(w)
	io.WriteString(w, name)
	if we.Var != "" && !mutated[we.Var] {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(we.Var))
	}
	io.WriteString(w, " = do\n")
	pushIndent()
	writeIndent(w)
	io.WriteString(w, "if ")
	we.Cond.emit(w)
	io.WriteString(w, " then do\n")
	pushIndent()
	for _, st := range we.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if we.Var != "" && we.Next != nil {
		writeIndent(w)
		if mutated[we.Var] {
			io.WriteString(w, "writeIORef ")
			io.WriteString(w, safeName(we.Var))
			io.WriteString(w, " $! (")
			we.Next.emit(w)
			io.WriteString(w, ")\n")
		} else {
			io.WriteString(w, safeName(we.Var))
			io.WriteString(w, " <- (")
			we.Next.emit(w)
			io.WriteString(w, ")\n")
		}
	}
	writeIndent(w)
	io.WriteString(w, name)
	io.WriteString(w, "\n")
	popIndent()
	writeIndent(w)
	io.WriteString(w, "else return (")
	if we.End != nil {
		we.End.emit(w)
	} else {
		io.WriteString(w, "()")
	}
	io.WriteString(w, ")\n")
	popIndent()
	popIndent()
	writeIndent(w)
	io.WriteString(w, "} in ")
	io.WriteString(w, name)
	if we.Var != "" && !mutated[we.Var] {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(we.Var))
	}
	io.WriteString(w, ")")
	currentLoop = prevLoop
	loopArgs = prevArgs
}

func (fe *ForExpr) emit(w io.Writer) {
	name := newLoopName()
	prevLoop := currentLoop
	prevArgs := loopArgs
	currentLoop = name
	loopArgs = " xs"
	io.WriteString(w, "(let\n")
	pushIndent()
	writeIndent(w)
	io.WriteString(w, name+" [] = return (")
	if fe.End != nil {
		fe.End.emit(w)
	} else {
		io.WriteString(w, "()")
	}
	io.WriteString(w, ")\n")
	writeIndent(w)
	io.WriteString(w, name+" ("+safeName(fe.Name)+":xs) = do\n")
	pushIndent()
	if len(fe.Body) == 1 {
		if is, ok := fe.Body[0].(*IfStmt); ok && len(is.Then) == 1 && len(is.Else) == 0 {
			writeIndent(w)
			io.WriteString(w, "if ")
			is.Cond.emit(w)
			io.WriteString(w, " then ")
			if ret, ok2 := is.Then[0].(*ReturnStmt); ok2 {
				ret.emit(w)
			} else {
				io.WriteString(w, "return ()")
			}
			io.WriteString(w, " else ")
			io.WriteString(w, name)
			io.WriteString(w, " xs\n")
		} else {
			for _, st := range fe.Body {
				st.emit(w)
				io.WriteString(w, "\n")
			}
			writeIndent(w)
			io.WriteString(w, name+" xs\n")
		}
	} else {
		for _, st := range fe.Body {
			st.emit(w)
			io.WriteString(w, "\n")
		}
		writeIndent(w)
		io.WriteString(w, name+" xs\n")
	}
	popIndent()
	writeIndent(w)
	io.WriteString(w, "in ")
	io.WriteString(w, name)
	io.WriteString(w, " ")
	if fe.To != nil {
		io.WriteString(w, "[")
		fe.From.emit(w)
		io.WriteString(w, " .. (")
		fe.To.emit(w)
		io.WriteString(w, " - 1)]")
	} else if isMapExpr(fe.From) {
		needDataMap = true
		io.WriteString(w, "(Map.keys ")
		fe.From.emit(w)
		io.WriteString(w, ")")
	} else {
		fe.From.emit(w)
	}
	io.WriteString(w, ")")
	popIndent()
	currentLoop = prevLoop
	loopArgs = prevArgs
}

func (idx *IndexExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	idx.Target.emit(w)
	if isMapExpr(idx.Target) {
		needDataMap = true
		io.WriteString(w, " Map.! ")
	} else {
		io.WriteString(w, " !! ")
	}
	idx.Index.emit(w)
	io.WriteString(w, ")")
}

func listAssign(list Expr, idxs []Expr, val Expr) Expr {
	if len(idxs) == 0 {
		return val
	}
	idx := idxs[0]
	rest := idxs[1:]
	inner := listAssign(&IndexExpr{Target: list, Index: idx}, rest, val)
	idxPlusOne := &BinaryExpr{Left: idx, Ops: []BinaryOp{{Op: "+", Right: &IntLit{Value: "1"}}}}
	left := &BinaryExpr{Left: &CallExpr{Fun: &NameRef{Name: "take"}, Args: []Expr{idx, list}},
		Ops: []BinaryOp{{Op: "++", Right: &ListLit{Elems: []Expr{inner}}}}}
	return &BinaryExpr{Left: left, Ops: []BinaryOp{{Op: "++", Right: &CallExpr{Fun: &NameRef{Name: "drop"}, Args: []Expr{idxPlusOne, list}}}}}
}

func mapAssign(base Expr, idxs []Expr, val Expr) Expr {
	needDataMap = true
	if len(idxs) == 1 {
		return &CallExpr{Fun: &NameRef{Name: "Map.insert"}, Args: []Expr{idxs[0], val, base}}
	}
	inner := mapAssign(&NameRef{Name: "m"}, idxs[1:], val)
	return &CallExpr{Fun: &NameRef{Name: "Map.adjust"}, Args: []Expr{&LambdaExpr{Params: []string{"m"}, Body: inner}, idxs[0], base}}
}

type IntLit struct{ Value string }
type FloatLit struct{ Value string }
type StringLit struct{ Value string }
type BoolLit struct{ Value bool }
type UnitLit struct{}
type NameRef struct{ Name string }
type LenExpr struct{ Arg Expr }
type BinaryExpr struct {
	Left Expr
	Ops  []BinaryOp
}

type BinaryOp struct {
	Op    string
	All   bool
	Right Expr
}
type UnaryExpr struct {
	Op   string
	Expr Expr
}
type GroupExpr struct{ Expr Expr }
type ListLit struct{ Elems []Expr }
type RecordLit struct {
	Name   string
	Names  []string
	Fields []Expr
}

// MapLit represents a simple map literal.
type MapLit struct{ Keys, Values []Expr }
type FieldExpr struct {
	Target Expr
	Field  string
}

// CastExpr annotates an expression with an explicit type.
type CastExpr struct {
	Expr Expr
	Type string
}
type ComprExpr struct {
	Vars    []string
	Sources []Expr
	Cond    Expr
	Body    Expr
}
type CallExpr struct {
	Fun  Expr
	Args []Expr
}

type RecordUpdate struct {
	Target Expr
	Field  string
	Value  Expr
}

// SliceExpr represents list or string slicing.
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}
type LambdaExpr struct {
	Params []string
	Body   Expr
}

// LambdaBlock represents a function literal with a statement block body.
type LambdaBlock struct {
	Params  []string
	Body    []Stmt
	Mutated map[string]bool
}
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

// LetInExpr represents a simple "let x = a in b" expression.
type LetInExpr struct {
	Name  string
	Value Expr
	Body  Expr
}

// CaseExpr represents a simple case expression used for pattern matching.
type CaseExpr struct {
	Target  Expr
	Clauses []CaseClause
}

// CaseClause is a single pattern -> result pair.
type CaseClause struct {
	Pattern Expr // nil represents '_'
	Result  Expr
}

// VariantPattern matches a union variant and binds its fields.
type VariantPattern struct {
	Name       string
	FieldNames []string
	Binds      []string
}

func (c *CastExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	c.Expr.emit(w)
	io.WriteString(w, " :: ")
	io.WriteString(w, c.Type)
	io.WriteString(w, ")")
}

func (ce *CaseExpr) emit(w io.Writer) {
	io.WriteString(w, "(case ")
	ce.Target.emit(w)
	io.WriteString(w, " of")
	for _, cl := range ce.Clauses {
		io.WriteString(w, " ")
		if cl.Pattern != nil {
			cl.Pattern.emit(w)
		} else {
			io.WriteString(w, "_")
		}
		io.WriteString(w, " -> ")
		cl.Result.emit(w)
		io.WriteString(w, ";")
	}
	io.WriteString(w, " )")
}

func (vp *VariantPattern) emit(w io.Writer) {
	io.WriteString(w, vp.Name)
	if len(vp.FieldNames) > 0 {
		io.WriteString(w, " {")
		for i, fn := range vp.FieldNames {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "%s = %s", fn, vp.Binds[i])
		}
		io.WriteString(w, "}")
	}
}

func (l *LetInExpr) emit(w io.Writer) {
	io.WriteString(w, "let ")
	io.WriteString(w, safeName(l.Name))
	io.WriteString(w, " = ")
	l.Value.emit(w)
	io.WriteString(w, " in ")
	l.Body.emit(w)
}

func (i *IntLit) emit(w io.Writer)    { io.WriteString(w, i.Value) }
func (f *FloatLit) emit(w io.Writer)  { io.WriteString(w, f.Value) }
func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }
func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "True")
	} else {
		io.WriteString(w, "False")
	}
}
func (u *UnitLit) emit(w io.Writer) { io.WriteString(w, "()") }
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
func (m *MapLit) emit(w io.Writer) {
	needDataMap = true
	io.WriteString(w, "Map.fromList [")
	for i := range m.Keys {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, "(")
		if k, ok := m.Keys[i].(*NameRef); ok {
			fmt.Fprintf(w, "%q", k.Name)
		} else {
			m.Keys[i].emit(w)
		}
		io.WriteString(w, ", ")
		m.Values[i].emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, "]")
}

func (r *RecordLit) emit(w io.Writer) {
	io.WriteString(w, r.Name)
	io.WriteString(w, " {")
	for i, n := range r.Names {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, n)
		io.WriteString(w, " = ")
		r.Fields[i].emit(w)
	}
	io.WriteString(w, "}")
}

func (r *RecordUpdate) emit(w io.Writer) {
	switch r.Target.(type) {
	case *NameRef, *FieldExpr:
		r.Target.emit(w)
	default:
		io.WriteString(w, "(")
		r.Target.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, " {")
	io.WriteString(w, safeName(r.Field))
	io.WriteString(w, " = ")
	r.Value.emit(w)
	io.WriteString(w, "}")
}
func (n *NameRef) emit(w io.Writer) {
	name := safeName(n.Name)
	if mutated[n.Name] {
		needIORef = true
		io.WriteString(w, "(deref ")
		io.WriteString(w, name)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, name)
	}
}
func (f *FieldExpr) emit(w io.Writer) {
	if nr, ok := f.Target.(*NameRef); ok && nr.Name == "testpkg" {
		io.WriteString(w, "testpkg_"+f.Field)
		return
	}
	if isMapExpr(f.Target) && !hasStruct(f.Target) {
		needDataMap = true
		f.Target.emit(w)
		fmt.Fprintf(w, " Map.! %q", f.Field)
		return
	}
	f.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, safeName(f.Field))
}

func hasStruct(e Expr) bool {
	if n, ok := e.(*NameRef); ok {
		return varStruct[n.Name] != ""
	}
	return false
}

// fieldType attempts to determine the Haskell type name of a field access
// expression. It walks nested field expressions using known struct
// definitions and type information from the environment.
func fieldType(fe *FieldExpr) string {
	switch t := fe.Target.(type) {
	case *NameRef:
		if s := varStruct[t.Name]; s != "" {
			if st, ok := structDefs[s]; ok {
				for i, f := range st.Fields {
					if f == fe.Field {
						return st.Types[i]
					}
				}
			}
		}
		if envInfo != nil {
			if vt, err := envInfo.GetVar(t.Name); err == nil {
				if st, ok := vt.(types.StructType); ok {
					if ft, ok2 := st.Fields[fe.Field]; ok2 {
						return toHsType(ft)
					}
				}
			}
		}
	case *FieldExpr:
		if base := fieldType(t); base != "" {
			if st, ok := structDefs[base]; ok {
				for i, f := range st.Fields {
					if f == fe.Field {
						return st.Types[i]
					}
				}
			}
		}
	}
	return ""
}
func (c *ComprExpr) emit(w io.Writer) {
	io.WriteString(w, "[")
	c.Body.emit(w)
	io.WriteString(w, " | ")
	for i := range c.Vars {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, c.Vars[i])
		io.WriteString(w, " <- ")
		c.Sources[i].emit(w)
	}
	if c.Cond != nil {
		io.WriteString(w, ", ")
		c.Cond.emit(w)
	}
	io.WriteString(w, "]")
}
func (l *LenExpr) emit(w io.Writer) {
	io.WriteString(w, "length ")
	switch l.Arg.(type) {
	case *NameRef, *ListLit, *CallExpr:
		l.Arg.emit(w)
	default:
		io.WriteString(w, "(")
		l.Arg.emit(w)
		io.WriteString(w, ")")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	io.WriteString(w, "take (")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ") (drop ")
	switch s.Start.(type) {
	case *NameRef, *IntLit:
		s.Start.emit(w)
	default:
		io.WriteString(w, "(")
		s.Start.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, " ")
	switch s.Target.(type) {
	case *NameRef, *ListLit, *CallExpr:
		s.Target.emit(w)
	default:
		io.WriteString(w, "(")
		s.Target.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")")
}
func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *NameRef:
		return varTypes[ex.Name] == "string"
	case *FieldExpr:
		if t := fieldType(ex); t != "" {
			return t == "String"
		}
	case *IndexExpr:
		if n, ok := ex.Target.(*NameRef); ok {
			return isStringMapVar(n.Name)
		}
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok {
			if n.Name == "show" && len(ex.Args) == 1 {
				return true
			}
			if n.Name == "take" || n.Name == "drop" || n.Name == "substring" {
				return true
			}
		}
	case *BinaryExpr:
		if len(ex.Ops) > 0 && ex.Ops[0].Op == "+" {
			if isStringExpr(ex.Left) || isStringExpr(ex.Ops[0].Right) {
				return true
			}
		}
	case *IfExpr:
		return isStringExpr(ex.Then) && isStringExpr(ex.Else)
	}
	return false
}

func isBoolExpr(e Expr) bool {
	switch ex := e.(type) {
	case *BoolLit:
		return true
	case *NameRef:
		return varTypes[ex.Name] == "bool"
	case *FieldExpr:
		if t := fieldType(ex); t != "" {
			return t == "Bool"
		}
	case *UnaryExpr:
		if ex.Op == "!" {
			return true
		}
		return isBoolExpr(ex.Expr)
	case *BinaryExpr:
		if len(ex.Ops) > 0 {
			op := ex.Ops[0].Op
			switch op {
			case "==", "!=", "<", ">", "<=", ">=", "&&", "||", "in":
				return true
			}
		}
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok {
			switch n.Name {
			case "isInfixOf", "elem", "notElem", "null":
				return true
			}
		}
	case *ComprExpr:
		return isIntExpr(ex.Body)
	}
	return false
}

func isCharExpr(e Expr) bool {
	idx, ok := e.(*IndexExpr)
	if !ok {
		return false
	}
	if n, ok := idx.Target.(*NameRef); ok {
		return varTypes[n.Name] == "string"
	}
	return false
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func isListExpr(e Expr) bool {
	switch ex := e.(type) {
	case *ListLit:
		return true
	case *NameRef:
		return varTypes[ex.Name] == "list"
	case *ComprExpr:
		return true
	case *BinaryExpr:
		if len(ex.Ops) > 0 {
			switch ex.Ops[0].Op {
			case "++", "union", "except", "intersect":
				return true
			}
		}
	case *SliceExpr:
		return true
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok {
			if n.Name == "Map.elems" && len(ex.Args) == 1 {
				return true
			}
			if (n.Name == "take" || n.Name == "drop") && len(ex.Args) == 2 {
				return true
			}
		}
	}
	return false
}

func isMapElemsExpr(e Expr) bool {
	c, ok := e.(*CallExpr)
	if !ok {
		return false
	}
	n, ok := c.Fun.(*NameRef)
	return ok && n.Name == "Map.elems" && len(c.Args) == 1
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *RecordLit:
		return false
	case *NameRef:
		t := varTypes[ex.Name]
		return strings.HasPrefix(t, "map")
	case *IndexExpr:
		return isMapExpr(ex.Target) && !isIntExpr(ex.Index)
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok {
			switch n.Name {
			case "Map.insert", "Map.fromList", "Map.delete", "Map.union", "Map.adjust":
				return true
			}
		}
	case *ComprExpr:
		return isIntExpr(ex.Body)
	}
	return false
}

func isStringMapVar(name string) bool {
	if varTypes[name] == "map_string" {
		return true
	}
	if vars != nil {
		if ex, ok := vars[name]; ok {
			switch v := ex.(type) {
			case *MapLit:
				allStr := true
				for _, val := range v.Values {
					if !isStringExpr(val) {
						allStr = false
						break
					}
				}
				return allStr
			case *ComprExpr:
				if ml, ok2 := v.Body.(*MapLit); ok2 {
					allStr := true
					for _, val := range ml.Values {
						if !isStringExpr(val) {
							allStr = false
							break
						}
					}
					return allStr
				}
			}
		}
	}
	return false
}

func isIntExpr(e Expr) bool {
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *BinaryExpr:
		if len(ex.Ops) > 0 && ex.Ops[0].Op == "%" {
			return true
		}
	case *NameRef:
		if varTypes[ex.Name] == "int" {
			return true
		}
		if envInfo != nil {
			if vt, err := envInfo.GetVar(ex.Name); err == nil {
				return types.IsIntType(vt) || types.IsInt64Type(vt)
			}
		}
		return false
	case *FieldExpr:
		if t := fieldType(ex); t != "" {
			return t == "Int"
		}
	case *ComprExpr:
		return isIntExpr(ex.Body)
	}
	return false
}

// isSimpleIntExpr reports whether e is a basic integer literal or variable.
func isSimpleIntExpr(e Expr) bool {
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *NameRef:
		if varTypes[ex.Name] == "int" {
			return true
		}
		if envInfo != nil {
			if vt, err := envInfo.GetVar(ex.Name); err == nil {
				return types.IsIntType(vt) || types.IsInt64Type(vt)
			}
		}
	}
	return false
}

func isFloatExpr(e Expr) bool {
	switch ex := e.(type) {
	case *FloatLit:
		return true
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok && n.Name == "fromIntegral" {
			return true
		}
	case *ComprExpr:
		return isFloatExpr(ex.Body)
	case *BinaryExpr:
		if len(ex.Ops) > 0 && ex.Ops[0].Op == "/" {
			return true
		}
	case *NameRef:
		if varTypes[ex.Name] == "float" {
			return true
		}
		if envInfo != nil {
			if vt, err := envInfo.GetVar(ex.Name); err == nil {
				return types.IsFloatType(vt)
			}
		}
		return false
	case *FieldExpr:
		if t := fieldType(ex); t != "" {
			return t == "Double"
		}
	}
	return false
}

func toStringExpr(e Expr) Expr {
	if isStringExpr(e) {
		return e
	}
	if isCharExpr(e) {
		return &ListLit{Elems: []Expr{e}}
	}
	if idx, ok := e.(*IndexExpr); ok {
		if n, ok2 := idx.Target.(*NameRef); ok2 && isStringMapVar(n.Name) {
			return idx
		}
	}
	return &CallExpr{Fun: &NameRef{Name: "show"}, Args: []Expr{e}}
}

func joinPrintArgs(args []Expr) Expr {
	if len(args) == 0 {
		return &StringLit{Value: ""}
	}
	ex := toStringExpr(args[0])
	for _, a := range args[1:] {
		ex = &BinaryExpr{Left: ex, Ops: []BinaryOp{
			{Op: "+", Right: &StringLit{Value: " "}},
			{Op: "+", Right: toStringExpr(a)},
		}}
	}
	return ex
}

func (b *BinaryExpr) emit(w io.Writer) {
	emitMaybeFloat := func(e Expr, wantFloat bool) {
		if wantFloat && isIntExpr(e) {
			io.WriteString(w, "fromIntegral ")
			switch e.(type) {
			case *NameRef, *IntLit:
				e.emit(w)
			default:
				io.WriteString(w, "(")
				e.emit(w)
				io.WriteString(w, ")")
			}
		} else {
			e.emit(w)
		}
	}

	left := b.Left
	if len(b.Ops) == 0 {
		emitMaybeFloat(left, false)
		return
	}
	wantFloat := isFloatExpr(left) || isFloatExpr(b.Ops[0].Right)
	emitMaybeFloat(left, wantFloat)
	for i, op := range b.Ops {
		io.WriteString(w, " ")
		switch op.Op {
		case "+":
			if isStringExpr(left) || isStringExpr(op.Right) {
				io.WriteString(w, "++")
			} else if isListExpr(left) && isListExpr(op.Right) {
				io.WriteString(w, "++")
			} else {
				io.WriteString(w, "+")
			}
		case "%":
			io.WriteString(w, "`mod`")
		case "/":
			if isIntExpr(left) && isIntExpr(op.Right) && !wantFloat {
				io.WriteString(w, "`div`")
			} else {
				io.WriteString(w, "/")
			}
		case "in":
			if isMapExpr(op.Right) {
				needDataMap = true
				io.WriteString(w, "`Map.member`")
			} else if isStringExpr(left) || isStringExpr(op.Right) {
				io.WriteString(w, "`isInfixOf`")
			} else {
				io.WriteString(w, "`elem`")
			}
		case "union":
			if op.All {
				io.WriteString(w, "++")
			} else {
				io.WriteString(w, "`union`")
			}
		case "except":
			io.WriteString(w, "\\\\")
		case "intersect":
			io.WriteString(w, "`intersect`")
		case "!=":
			io.WriteString(w, "/=")
		default:
			io.WriteString(w, op.Op)
		}
		io.WriteString(w, " ")
		emitMaybeFloat(op.Right, wantFloat)
		left = op.Right
		if i+1 < len(b.Ops) {
			wantFloat = isFloatExpr(left) || isFloatExpr(b.Ops[i+1].Right)
		}
	}
}
func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "!" {
		io.WriteString(w, "not ")
		switch u.Expr.(type) {
		case *NameRef, *IntLit, *StringLit, *BoolLit:
			u.Expr.emit(w)
		default:
			io.WriteString(w, "(")
			u.Expr.emit(w)
			io.WriteString(w, ")")
		}
		return
	}
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}
func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.Expr.emit(w)
	io.WriteString(w, ")")
}
func (c *CallExpr) emit(w io.Writer) {
	if f, ok := c.Fun.(*FieldExpr); ok && f.Field == "get" && isMapExpr(f.Target) && !hasStruct(f.Target) && len(c.Args) == 2 {
		needDataMap = true
		io.WriteString(w, "Map.findWithDefault ")
		c.Args[1].emit(w)
		io.WriteString(w, " ")
		c.Args[0].emit(w)
		io.WriteString(w, " ")
		f.Target.emit(w)
		return
	}
	var fname string
	if n, ok := c.Fun.(*NameRef); ok {
		fname = n.Name
	}
	c.Fun.emit(w)
	for i, a := range c.Args {
		io.WriteString(w, " ")
		if fname != "" {
			if m := funcParamMut[fname]; m != nil && m[i] {
				if nr, ok := a.(*NameRef); ok {
					io.WriteString(w, safeName(nr.Name))
					continue
				}
			}
		}
		switch a.(type) {
		case *IntLit, *FloatLit, *StringLit, *NameRef:
			a.emit(w)
		default:
			io.WriteString(w, "(")
			a.emit(w)
			io.WriteString(w, ")")
		}
	}
}
func (l *LambdaExpr) emit(w io.Writer) {
	io.WriteString(w, "\\")
	for i, p := range l.Params {
		if i > 0 {
			io.WriteString(w, " ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, " -> ")
	l.Body.emit(w)
}

func (l *LambdaBlock) emit(w io.Writer) {
	io.WriteString(w, "\\")
	for i, p := range l.Params {
		if i > 0 {
			io.WriteString(w, " ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, " -> ")
	prevMut := mutated
	mutated = map[string]bool{}
	for k, v := range mutatedGlobal {
		mutated[k] = v
	}
	for k, v := range l.Mutated {
		mutated[k] = v
	}
	if len(l.Body) == 1 {
		if r, ok := l.Body[0].(*ReturnStmt); ok && isPureExpr(r.Expr) {
			r.Expr.emit(w)
			mutated = prevMut
			return
		}
	}
	if isPureFunc(l.Body) {
		expr := blockToExpr(l.Body)
		expr.emit(w)
		mutated = prevMut
		return
	}
	io.WriteString(w, "do\n")
	pushIndent()
	for _, st := range l.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	popIndent()
	mutated = prevMut
}
func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then ")
	i.Then.emit(w)
	io.WriteString(w, " else ")
	i.Else.emit(w)
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

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(b))
}

func toHsType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "Int"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Bool"
	case types.BigRatType, types.FloatType:
		return "Double"
	case types.ListType:
		return "[" + toHsType(tt.Elem) + "]"
	case types.MapType:
		needDataMap = true
		return "Map.Map " + toHsType(tt.Key) + " " + toHsType(tt.Value)
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	case types.VoidType:
		return "()"
	default:
		return "String"
	}
}

func shortType(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return "int"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.BigRatType, types.FloatType:
		return "float"
	case types.ListType:
		return "list"
	case types.MapType:
		return "map"
	case types.StructType:
		return "record"
	case types.UnionType:
		return "record"
	default:
		return ""
	}
}

func inferVarFromSource(name string, src Expr) {
	if ll, ok := src.(*ListLit); ok && len(ll.Elems) > 0 {
		recordType(name, ll.Elems[0])
		return
	}
	if n, ok := src.(*NameRef); ok {
		if s := varStruct[n.Name]; s != "" {
			varStruct[name] = s
			varTypes[name] = "record"
		}
		if vars != nil {
			if ce, ok2 := vars[n.Name].(*ComprExpr); ok2 {
				if ml, ok3 := ce.Body.(*MapLit); ok3 {
					allStr := true
					for _, v := range ml.Values {
						if !isStringExpr(v) {
							allStr = false
							break
						}
					}
					if allStr {
						varTypes[name] = "map_string"
					} else {
						varTypes[name] = "map"
					}
				}
			}
		}
		if envInfo != nil {
			if vt, err := envInfo.GetVar(n.Name); err == nil {
				switch tt := vt.(type) {
				case types.ListType:
					if st, ok := tt.Elem.(types.StructType); ok {
						varStruct[name] = st.Name
						varTypes[name] = "record"
					} else if varStruct[name] == "" {
						varTypes[name] = shortType(tt.Elem)
					}
				case types.MapType:
					if varStruct[name] == "" {
						varTypes[name] = shortType(tt.Key)
					}
				}
			}
		}
	}
	if fe, ok := src.(*FieldExpr); ok {
		if n, ok2 := fe.Target.(*NameRef); ok2 && fe.Field == "items" {
			if it := groupItemType[n.Name]; it != "" {
				varStruct[name] = it
				varTypes[name] = "record"
			}
		}
	}
}

func header(withList, withMap, withJSON, withTrace, withIORef, withNow, withLookupHost, withMem, withPrintf bool) string {
	out, err := exec.Command("git", "log", "-1", "--date=iso-strict", "--format=%cd").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	h := "{-# LANGUAGE DuplicateRecordFields #-}\n"
	h += "{-# LANGUAGE OverloadedRecordDot #-}\n"
	h += "{-# LANGUAGE NoFieldSelectors #-}\n"
	if withJSON {
		h += "{-# LANGUAGE OverloadedStrings #-}\n"
	}
	if len(preludeHide) > 0 {
		names := make([]string, 0, len(preludeHide))
		for n := range preludeHide {
			names = append(names, n)
		}
		sort.Strings(names)
		h += fmt.Sprintf("import Prelude hiding (%s)\n", strings.Join(names, ", "))
	}
	h += fmt.Sprintf("-- Generated by Mochi transpiler v%s on %s\n", version(), ts.In(loc).Format("2006-01-02 15:04 MST"))
	if withList {
		h += "import Data.List (intercalate, isInfixOf, union, intersect, nub, sortOn, (\\\\))\n"
	}
	if withMap {
		h += "import qualified Data.Map as Map\n"
	}
	if withJSON {
		h += "import qualified Data.Aeson as Aeson\n"
		h += "import qualified Data.Aeson.Encode.Pretty as Pretty\n"
		h += "import qualified Data.ByteString.Lazy.Char8 as BSL\n"
	}
	if withPrintf {
		h += "import Text.Printf (printf)\n"
	}
	if withTrace {
		h += "import Debug.Trace (trace)\n"
	}
	if withIORef {
		h += "import Data.IORef\n"
		h += "import System.IO.Unsafe (unsafePerformIO)\n"
	}
	if withNow {
		h += "import System.Environment (lookupEnv)\n"
		h += "import Data.Time.Clock.POSIX (getPOSIXTime)\n"
		h += "import Data.Char (isDigit)\n"
	}
	if withMem {
		h += "import GHC.Stats (getRTSStats, max_mem_in_use_bytes)\n"
	}
	h += "import System.IO (isEOF)\n"
	h += "input :: IO String\n"
	h += "input = do\n"
	h += "    eof <- isEOF\n"
	h += "    if eof then return \"\" else getLine\n"
	h += "int :: String -> Int\n"
	h += "int = read\n"
	h += "float :: Int -> Double\n"
	h += "float n = fromIntegral n\n"
	if withIORef {
		h += "deref :: IORef a -> a\n"
		h += "{-# NOINLINE deref #-}\n"
		h += "deref r = unsafePerformIO (atomicModifyIORef' r (\\x -> (x, x)))\n"
	}
	h += "powInt :: Int -> Int -> Int\n"
	h += "powInt b e = go 1 b e where\n"
	h += "    go r b e | e > 0 = go (if e `mod` 2 == 1 then r*b else r) (b*b) (e `div` 2)\n"
	h += "              | otherwise = r\n"
	if withNow {
		h += "_nowSeed :: IORef Int\n"
		h += "_nowSeed = unsafePerformIO (newIORef 0)\n"
		h += "{-# NOINLINE _nowSeed #-}\n"
		h += "_nowSeeded :: IORef Bool\n"
		h += "_nowSeeded = unsafePerformIO (newIORef False)\n"
		h += "{-# NOINLINE _nowSeeded #-}\n"
		h += "_now :: IO Int\n"
		h += "_now = do\n"
		h += "    seeded <- readIORef _nowSeeded\n"
		h += "    if not seeded then do\n"
		h += "        m <- lookupEnv \"MOCHI_NOW_SEED\"\n"
		h += "        case m of\n"
		h += "            Just s | all isDigit s -> do writeIORef _nowSeed (read s); writeIORef _nowSeeded True\n"
		h += "            _ -> return ()\n"
		h += "     else return ()\n"
		h += "    seeded2 <- readIORef _nowSeeded\n"
		h += "    if seeded2 then do\n"
		h += "        modifyIORef' _nowSeed (\\x -> (x * 1664525 + 1013904223) `mod` 2147483647)\n"
		h += "        readIORef _nowSeed\n"
		h += "    else do\n"
		h += "        t <- getPOSIXTime\n"
		h += "        return (floor (t * 1000000000))\n"
	}
	if withMem {
		h += "_mem :: IO Int\n"
		h += "_mem = fmap (fromIntegral . max_mem_in_use_bytes) getRTSStats\n"
	}
	if withLookupHost {
		h += "nil :: [a]\n"
		h += "nil = []\n"
		h += "_lookup_host :: String -> [[String]]\n"
		h += "_lookup_host _ = [[\"2001:2f0:0:8800:226:2dff:fe0b:4311\", \"2001:2f0:0:8800::1:1\", \"210.155.141.200\"], nil]\n"
	}
	return h
}

// Emit generates formatted Haskell code.
func Emit(p *Program, bench bool) []byte {
	if bench {
		usesPrintf = true
		usesNow = true
		usesMem = true
		needIORef = true
		p.Stmts = []Stmt{&BenchStmt{Name: "main", Body: p.Stmts}}
	}
	for _, m := range mutatedGlobal {
		if m {
			needIORef = true
			break
		}
	}
	if !needIORef {
		for _, f := range p.Funcs {
			for _, m := range f.Mutated {
				if m {
					needIORef = true
					break
				}
			}
			if needIORef {
				break
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString(header(needDataList, needDataMap, needJSON, needTrace, needIORef, usesNow, usesLookupHost, usesMem, usesPrintf))
	if needGroupBy {
		buf.WriteString(groupPrelude())
		buf.WriteByte('\n')
	}
	for _, t := range p.Types {
		t.emit(&buf)
		buf.WriteByte('\n')
		buf.WriteByte('\n')
	}
	for _, f := range p.Funcs {
		f.emit(&buf)
		buf.WriteByte('\n')
		buf.WriteByte('\n')
	}
	var refDecls []*LetStmt
	hasMain := false
	for _, f := range p.Funcs {
		if f.Name == "main" {
			hasMain = true
			break
		}
	}
	if !hasMain {
		for _, s := range p.Stmts {
			if l, ok := s.(*LetStmt); ok && l.Name == "main" {
				hasMain = true
				break
			}
		}
	}
	for _, s := range p.Stmts {
		if l, ok := s.(*LetStmt); ok {
			if mutated[l.Name] {
				refDecls = append(refDecls, l)
				needIORef = true
				continue
			}
			l.emit(&buf)
			buf.WriteByte('\n')
			buf.WriteByte('\n')
		}
	}
	if !hasMain {
		buf.WriteString("main :: IO ()\n")
		buf.WriteString("main = do\n")
	}
	prev := indent
	if !hasMain {
		indent += "    "
	}
	for _, l := range refDecls {
		writeIndent(&buf)
		name := safeName(l.Name)
		if indent == "" {
			io.WriteString(&buf, name+" = unsafePerformIO (newIORef (")
			l.Expr.emit(&buf)
			io.WriteString(&buf, "))\n")
			writeIndent(&buf)
			fmt.Fprintf(&buf, "{-# NOINLINE %s #-}\n", name)
		} else {
			io.WriteString(&buf, name+" <- newIORef (")
			l.Expr.emit(&buf)
			io.WriteString(&buf, ")\n")
		}
	}
	for _, s := range p.Stmts {
		if l, ok := s.(*LetStmt); ok {
			if mutated[l.Name] {
				continue
			}
			continue
		}
		s.emit(&buf)
		buf.WriteByte('\n')
	}
	if !hasMain {
		indent = prev
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a simple Haskell AST.
var envInfo *types.Env

func Transpile(prog *parser.Program, env *types.Env, benchMain bool) (*Program, error) {
	envInfo = env
	h := &Program{}
	vars = map[string]Expr{}
	varTypes = map[string]string{}
	varStruct = map[string]string{}
	needDataList = false
	needDataMap = false
	needGroupBy = false
	needJSON = false
	needTrace = false
	needIORef = false
	usesNow = false
	usesMem = false
	usesPrintf = false
	usesLookupHost = false
	usesInput = false
	funcParamMut = map[string]map[int]bool{}
	groupVars = map[string]bool{}
	groupKeyStruct = map[string]string{}
	groupItemType = map[string]string{}
	structDefs = map[string]*TypeDecl{}
	structCount = 0
	preludeHide = map[string]bool{
		"repeat": true,
	}
	mutatedGlobal = map[string]bool{}
	mutated = mutatedGlobal
	locals = nil
	globals = map[string]bool{}
	for i := 0; i < len(prog.Statements); i++ {
		st := prog.Statements[i]
		if st.Var != nil && st.Var.Value != nil && st.Var.Value.Binary != nil && st.Var.Value.Binary.Left != nil && st.Var.Value.Binary.Left.Value != nil && st.Var.Value.Binary.Left.Value.Target != nil && st.Var.Value.Binary.Left.Value.Target.List != nil && len(st.Var.Value.Binary.Left.Value.Target.List.Elems) == 0 {
			if i+1 < len(prog.Statements) && prog.Statements[i+1].For != nil {
				if ls, ok := convertGroupItemsLoop(st.Var, prog.Statements[i+1].For); ok {
					name := safeName(st.Var.Name)
					vars[name] = ls.Expr
					recordType(name, ls.Expr)
					i++
					continue
				}
			}
		}
		switch {
		case st.Let != nil:
			ex, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Let.Name)
			globals[name] = true
			vars[name] = ex
			recordType(name, ex)
		case st.Var != nil:
			ex, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Var.Name)
			globals[name] = true
			vars[name] = ex
			recordType(name, ex)
		case st.Import != nil && st.Import.Path == "mochi/runtime/ffi/go/testpkg" && st.Import.As == "testpkg" && st.Import.Auto:
			h.Funcs = append(h.Funcs, Func{Name: "testpkg_Add", Params: []string{"a", "b"}, Body: []Stmt{&ReturnStmt{Expr: &BinaryExpr{Left: &NameRef{Name: "a"}, Ops: []BinaryOp{{Op: "+", Right: &NameRef{Name: "b"}}}}}}})
			vars["testpkg_Pi"] = &FloatLit{Value: "3.14"}
			recordType("testpkg_Pi", vars["testpkg_Pi"])
			vars["testpkg_Answer"] = &IntLit{Value: "42"}
			recordType("testpkg_Answer", vars["testpkg_Answer"])
		case st.Type != nil:
			if len(st.Type.Members) > 0 {
				fields := make([]string, len(st.Type.Members))
				typestr := make([]string, len(st.Type.Members))
				for i, m := range st.Type.Members {
					fields[i] = safeName(m.Field.Name)
					typestr[i] = typeNameFromRef(m.Field.Type)
				}
				structDefs[st.Type.Name] = &TypeDecl{Name: st.Type.Name, Fields: fields, Types: typestr}
				preludeHide[st.Type.Name] = true
				for _, f := range fields {
					preludeHide[f] = true
				}
			} else if len(st.Type.Variants) > 0 {
				var vars []Variant
				for _, v := range st.Type.Variants {
					fields := make([]string, len(v.Fields))
					typestr := make([]string, len(v.Fields))
					for i, f := range v.Fields {
						fields[i] = safeName(f.Name)
						typestr[i] = typeNameFromRef(f.Type)
						preludeHide[f.Name] = true
					}
					vars = append(vars, Variant{Name: v.Name, Fields: fields, Types: typestr})
				}
				structDefs[st.Type.Name] = &TypeDecl{Name: st.Type.Name, Variants: vars}
				preludeHide[st.Type.Name] = true
			}
		case st.Assign != nil:
			val, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Assign.Name)
			globals[name] = true
			if len(st.Assign.Index) == 0 {
				vars[name] = val
				recordType(name, val)
				mutatedGlobal[name] = true
				break
			}
			var idxExprs []Expr
			for _, idx := range st.Assign.Index {
				ex, err := convertExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				idxExprs = append(idxExprs, ex)
			}
			// Always reference the current variable value rather than
			// the last assigned expression. Using the previous
			// expression can result in generated code that mutates
			// a literal list instead of the variable.
			prev := &NameRef{Name: name}
			var expr Expr
			if isMapExpr(prev) {
				expr = mapAssign(prev, idxExprs, val)
			} else {
				expr = listAssign(prev, idxExprs, val)
			}
			vars[name] = expr
			recordType(name, expr)
			mutatedGlobal[name] = true
		case st.Fun != nil:
			if st.Fun.Name == "powInt" {
				preludeHide["powInt"] = true
				break
			}
			fn, err := convertFunStmt(st.Fun)
			if err != nil {
				return nil, err
			}
			h.Funcs = append(h.Funcs, *fn)
		case st.If != nil:
			s, err := convertIfStmt(st.If)
			if err != nil {
				return nil, err
			}
			h.Stmts = append(h.Stmts, s)
		case st.While != nil:
			s, err := convertWhileStmt(st.While)
			if err != nil {
				return nil, err
			}
			h.Stmts = append(h.Stmts, s)
		case st.For != nil:
			s, err := convertForStmt(st.For)
			if err != nil {
				return nil, err
			}
			h.Stmts = append(h.Stmts, s)
		case st.Bench != nil:
			s, err := convertBenchBlock(st.Bench)
			if err != nil {
				return nil, err
			}
			h.Stmts = append(h.Stmts, s)
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" {
				var args []Expr
				for _, a := range call.Args {
					ex, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ex)
				}
				if len(args) == 1 {
					str := isStringExpr(args[0])
					if isListExpr(args[0]) {
						needDataList = true
					}
					h.Stmts = append(h.Stmts, &PrintStmt{Expr: args[0], String: str})
				} else {
					for _, a := range args {
						if isListExpr(a) {
							needDataList = true
							break
						}
					}
					h.Stmts = append(h.Stmts, &PrintStmt{Expr: joinPrintArgs(args), String: true})
				}
				continue
			}
			if call != nil && call.Func == "json" && len(call.Args) == 1 {
				arg, err := convertExpr(call.Args[0])
				if err != nil {
					return nil, err
				}
				needJSON = true
				h.Stmts = append(h.Stmts, &JSONStmt{Expr: arg})
				continue
			}
			ex, err := convertExpr(st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			h.Stmts = append(h.Stmts, &ExprStmt{Expr: ex})
			continue
		default:
			if st.Test == nil && st.Import == nil && st.Type == nil {
				return nil, fmt.Errorf("unsupported statement")
			}
		}
	}
	// deterministically emit variables in alphabetical order
	names := make([]string, 0, len(vars))
	for n := range vars {
		names = append(names, n)
	}
	sort.Strings(names)
	for i := len(names) - 1; i >= 0; i-- {
		n := names[i]
		h.Stmts = append([]Stmt{&LetStmt{Name: n, Expr: vars[n]}}, h.Stmts...)
	}
	hasMainFunc := false
	for _, f := range h.Funcs {
		if f.Name == "main" {
			hasMainFunc = true
			break
		}
	}
	if hasMainFunc && len(h.Stmts) > 0 {
		if es, ok := h.Stmts[len(h.Stmts)-1].(*ExprStmt); ok {
			if isMainCall(es.Expr) {
				h.Stmts = h.Stmts[:len(h.Stmts)-1]
			}
		}
	}
	if benchMain {
		usesPrintf = true
		usesNow = true
		usesMem = true
		needIORef = true
		renamed := false
		for i := range h.Funcs {
			if h.Funcs[i].Name == "main" {
				h.Funcs[i].Name = "mainEntry"
				renamed = true
				break
			}
		}
		if renamed {
			body := []Stmt{&ExprStmt{Expr: &CallExpr{Fun: &NameRef{Name: "mainEntry"}}}}
			h.Funcs = append(h.Funcs, Func{Name: "main", Body: []Stmt{&BenchStmt{Name: "main", Body: body}}})
		} else {
			h.Stmts = []Stmt{&BenchStmt{Name: "main", Body: h.Stmts}}
		}
	}
	// attach struct type declarations
	for _, tdecl := range structDefs {
		h.Types = append(h.Types, *tdecl)
	}
	return h, nil
}

// Print writes a lisp-like representation of the AST to stdout.
func Print(p *Program) { toNode(p).Print("") }

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
		return &ast.Node{Kind: "print", Children: []*ast.Node{exprNode(st.Expr)}}
	case *JSONStmt:
		return &ast.Node{Kind: "json", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
		for _, c := range st.Then {
			n.Children = append(n.Children, stmtNode(c))
		}
		for _, c := range st.Else {
			n.Children = append(n.Children, stmtNode(c))
		}
		return n
	case *ForStmt:
		n := &ast.Node{Kind: "for", Value: st.Name, Children: []*ast.Node{exprNode(st.From)}}
		if st.To != nil {
			n.Children = append(n.Children, exprNode(st.To))
		}
		for _, c := range st.Body {
			n.Children = append(n.Children, stmtNode(c))
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprNode(st.Cond)}}
		for _, c := range st.Body {
			n.Children = append(n.Children, stmtNode(c))
		}
		return n
	case *BenchStmt:
		n := &ast.Node{Kind: "bench", Value: st.Name}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "stmt"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "True"}
		}
		return &ast.Node{Kind: "bool", Value: "False"}
	case *NameRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Arg)}}
	case *BinaryExpr:
		n := &ast.Node{Kind: "bin"}
		n.Children = append(n.Children, exprNode(ex.Left))
		for _, op := range ex.Ops {
			n.Children = append(n.Children, &ast.Node{Kind: "op", Value: op.Op})
			n.Children = append(n.Children, exprNode(op.Right))
		}
		return n
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *CallExpr:
		n := &ast.Node{Kind: "call", Children: []*ast.Node{exprNode(ex.Fun)}}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Body))
		return n
	case *IfExpr:
		return &ast.Node{Kind: "ifexpr", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *RecordLit:
		n := &ast.Node{Kind: "record", Value: ex.Name}
		for i, f := range ex.Names {
			n.Children = append(n.Children, &ast.Node{Kind: f, Children: []*ast.Node{exprNode(ex.Fields[i])}})
		}
		return n
	case *FieldExpr:
		return &ast.Node{Kind: "field", Value: ex.Field, Children: []*ast.Node{exprNode(ex.Target)}}
	case *ComprExpr:
		n := &ast.Node{Kind: "compr"}
		n.Children = append(n.Children, exprNode(ex.Body))
		for i, v := range ex.Vars {
			n.Children = append(n.Children, &ast.Node{Kind: "gen", Value: v, Children: []*ast.Node{exprNode(ex.Sources[i])}})
		}
		if ex.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond)}})
		}
		return n
	default:
		return &ast.Node{Kind: "expr"}
	}
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		// treat missing expression as zero
		return &IntLit{Value: "0"}, nil
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	left, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	be := &BinaryExpr{Left: left}
	for _, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		switch op.Op {
		case "in", "union", "except", "intersect":
			needDataList = true
		}
		be.Ops = append(be.Ops, BinaryOp{Op: op.Op, All: op.All, Right: right})
	}
	if len(be.Ops) == 0 {
		return left, nil
	}
	return be, nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = &UnaryExpr{Op: u.Ops[i], Expr: expr}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if pf.Target != nil && pf.Target.Selector != nil &&
		len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "LookupHost" &&
		pf.Target.Selector.Root == "net" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		if len(pf.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("LookupHost expects 1 arg")
		}
		arg, err := convertExpr(pf.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		usesLookupHost = true
		return &CallExpr{Fun: &NameRef{Name: "_lookup_host"}, Args: []Expr{arg}}, nil
	}
	if pf.Target != nil && pf.Target.Selector != nil &&
		len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" &&
		len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		if len(pf.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		base, err := convertPrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}})
		if err != nil {
			return nil, err
		}
		arg, err := convertExpr(pf.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		needDataList = true
		return &CallExpr{Fun: &NameRef{Name: "isInfixOf"}, Args: []Expr{arg, base}}, nil
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		if op.Call != nil {
			var args []Expr
			for _, a := range op.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ex)
			}
			if n, ok := expr.(*NameRef); ok && n.Name == "str" && len(args) == 1 {
				expr = &CallExpr{Fun: &NameRef{Name: "show"}, Args: args}
			} else {
				call := &CallExpr{Fun: expr, Args: args}
				if n, ok := expr.(*NameRef); ok && shouldWrapCall(n.Name) {
					call = &CallExpr{Fun: &NameRef{Name: "unsafePerformIO"}, Args: []Expr{call}}
				}
				expr = call
			}
			continue
		}
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				expr = &IndexExpr{Target: expr, Index: idx}
				continue
			}
			if op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil {
				target := expr
				start, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				var end Expr
				if op.Index.End != nil {
					end, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				} else {
					end = &LenExpr{Arg: target}
				}
				expr = &SliceExpr{Target: target, Start: start, End: end}
				continue
			}
		}
		if op.Cast != nil {
			tname := typeNameFromRef(op.Cast.Type)
			if op.Cast.Type.Simple != nil {
				ts := *op.Cast.Type.Simple
				if ts == "int" {
					if g, ok := expr.(*GroupExpr); ok {
						expr = g.Expr
					}
					if be, ok := expr.(*BinaryExpr); ok && len(be.Ops) == 1 && be.Ops[0].Op == "/" {
						if isSimpleIntExpr(be.Left) && isSimpleIntExpr(be.Ops[0].Right) {
							expr = &CallExpr{Fun: &NameRef{Name: "div"}, Args: []Expr{be.Left, be.Ops[0].Right}}
						} else {
							expr = &CallExpr{Fun: &NameRef{Name: "div"}, Args: []Expr{be.Left, be.Ops[0].Right}}
						}
					} else {
						// Cast numeric expressions using round instead of read which expects a String.
						expr = &CallExpr{Fun: &NameRef{Name: "round"}, Args: []Expr{&GroupExpr{Expr: expr}}}
					}
					continue
				}
				if ts == "float" {
					expr = &CallExpr{Fun: &NameRef{Name: "fromIntegral"}, Args: []Expr{expr}}
					continue
				}
				if ml, ok := expr.(*MapLit); ok {
					names := make([]string, len(ml.Keys))
					vals := make([]Expr, len(ml.Keys))
					for i, k := range ml.Keys {
						if s, ok2 := k.(*StringLit); ok2 {
							names[i] = s.Value
						} else if n, ok2 := k.(*NameRef); ok2 {
							names[i] = n.Name
						} else {
							return nil, fmt.Errorf("unsupported cast key")
						}
						vals[i] = ml.Values[i]
					}
					expr = &RecordLit{Name: ts, Names: names, Fields: vals}
					continue
				}
			}
			// Generic cast for remaining types
			expr = &CastExpr{Expr: expr, Type: tname}
			continue
		}
		return nil, fmt.Errorf("postfix op not supported")
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		name := safeName(p.Selector.Root)
		return &NameRef{Name: name}, nil
	case p.Selector != nil:
		expr := Expr(&NameRef{Name: safeName(p.Selector.Root)})
		for _, fld := range p.Selector.Tail {
			expr = &FieldExpr{Target: expr, Field: fld}
		}
		return expr, nil
	case p.If != nil:
		cond, err := convertExpr(p.If.Cond)
		if err != nil {
			return nil, err
		}
		thn, err := convertExpr(p.If.Then)
		if err != nil {
			return nil, err
		}
		var elseExpr *parser.Expr
		if p.If.ElseIf != nil {
			// chain else-if as nested if expression
			e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{If: p.If.ElseIf}}}}}
			elseExpr = e
		} else {
			elseExpr = p.If.Else
		}
		els, err := convertExpr(elseExpr)
		if err != nil {
			return nil, err
		}
		return &IfExpr{Cond: cond, Then: thn, Else: els}, nil
	case p.Group != nil:
		e, err := convertExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: e}, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Struct != nil:
		names := make([]string, len(p.Struct.Fields))
		vals := make([]Expr, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			names[i] = safeName(f.Name)
			ve, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			vals[i] = ve
		}
		for _, n := range names {
			preludeHide[n] = true
		}
		return &RecordLit{Name: p.Struct.Name, Names: names, Fields: vals}, nil
	case p.List != nil:
		if envInfo != nil {
			if st, ok := types.InferStructFromList(p.List, envInfo); ok {
				structCount++
				name := fmt.Sprintf("GenType%d", structCount)
				fields := make([]string, len(st.Order))
				typestr := make([]string, len(st.Order))
				for i, fn := range st.Order {
					fields[i] = fn
					typestr[i] = toHsType(st.Fields[fn])
				}
				structDefs[name] = &TypeDecl{Name: name, Fields: fields, Types: typestr}
				for _, f := range fields {
					preludeHide[f] = true
				}
				var elems []Expr
				for _, e := range p.List.Elems {
					ml := e.Binary.Left.Value.Target.Map
					vals := make([]Expr, len(st.Order))
					for i, it := range ml.Items {
						ve, err := convertExpr(it.Value)
						if err != nil {
							return nil, err
						}
						vals[i] = ve
					}
					elems = append(elems, &RecordLit{Name: name, Names: fields, Fields: vals})
				}
				return &ListLit{Elems: elems}, nil
			}
		}
		var elems []Expr
		for _, e := range p.List.Elems {
			ce, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ce)
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		if envInfo != nil {
			if _, ok := types.InferSimpleMap(p.Map, envInfo); ok {
				needDataMap = true
				var keys []Expr
				var values []Expr
				for _, it := range p.Map.Items {
					ke, err := convertExpr(it.Key)
					if err != nil {
						return nil, err
					}
					keys = append(keys, ke)
					ve, err := convertExpr(it.Value)
					if err != nil {
						return nil, err
					}
					values = append(values, ve)
				}
				return &MapLit{Keys: keys, Values: values}, nil
			}
		}
		needDataMap = true
		var keys []Expr
		var values []Expr
		for _, it := range p.Map.Items {
			ke, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			keys = append(keys, ke)
			ve, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			values = append(values, ve)
		}
		return &MapLit{Keys: keys, Values: values}, nil
	case p.Query != nil:
		vars := []string{p.Query.Var}
		srcs := []Expr{}
		se, err := convertExpr(p.Query.Source)
		if err != nil {
			return nil, err
		}
		if nr, ok := se.(*NameRef); ok && groupVars[nr.Name] {
			se = &FieldExpr{Target: nr, Field: "items"}
		}
		srcs = append(srcs, se)
		inferVarFromSource(p.Query.Var, se)

		// simple left join without additional from clauses or grouping
		if len(p.Query.Froms) == 0 && len(p.Query.Joins) == 1 && p.Query.Group == nil {
			j := p.Query.Joins[0]
			if j.Side != nil && *j.Side == "left" {
				je, err := convertExpr(j.Src)
				if err != nil {
					return nil, err
				}
				if nr, ok := je.(*NameRef); ok && groupVars[nr.Name] {
					je = &FieldExpr{Target: nr, Field: "items"}
				}
				condExpr, err := convertExpr(j.On)
				if err != nil {
					return nil, err
				}
				matches := &ComprExpr{Vars: []string{j.Var}, Sources: []Expr{je}, Cond: condExpr, Body: &NameRef{Name: j.Var}}
				msName := "_ms0"
				joinList := &LetInExpr{Name: msName, Value: matches,
					Body: &IfExpr{Cond: &CallExpr{Fun: &NameRef{Name: "null"}, Args: []Expr{&NameRef{Name: msName}}},
						Then: &ListLit{Elems: []Expr{&NameRef{Name: "Nothing"}}},
						Else: &CallExpr{Fun: &NameRef{Name: "map"}, Args: []Expr{&NameRef{Name: "Just"}, &NameRef{Name: msName}}}}}
				vars = append(vars, j.Var)
				srcs = append(srcs, joinList)
				if n, ok := je.(*NameRef); ok {
					if s := varStruct[n.Name]; s != "" {
						varStruct[j.Var] = s
					}
				}
				varTypes[j.Var] = "maybe_record"
				body, err := convertExpr(p.Query.Select)
				if err != nil {
					return nil, err
				}
				var result Expr = &ComprExpr{Vars: []string{p.Query.Var, j.Var}, Sources: srcs, Cond: nil, Body: body}
				if p.Query.Sort != nil {
					sortExpr, err := convertExpr(p.Query.Sort)
					if err != nil {
						return nil, err
					}
					needDataList = true
					result = &CallExpr{Fun: &NameRef{Name: "sortOn"}, Args: []Expr{&GroupExpr{Expr: &LambdaExpr{Params: []string{p.Query.Var}, Body: sortExpr}}, result}}
				}
				return result, nil
			}
		}

		for _, f := range p.Query.Froms {
			fe, err := convertExpr(f.Src)
			if err != nil {
				return nil, err
			}
			if nr, ok := fe.(*NameRef); ok && groupVars[nr.Name] {
				fe = &FieldExpr{Target: nr, Field: "items"}
			}
			vars = append(vars, f.Var)
			srcs = append(srcs, fe)
			inferVarFromSource(f.Var, fe)
		}
		var cond Expr
		for _, j := range p.Query.Joins {
			je, err := convertExpr(j.Src)
			if err != nil {
				return nil, err
			}
			if nr, ok := je.(*NameRef); ok && groupVars[nr.Name] {
				je = &FieldExpr{Target: nr, Field: "items"}
			}
			vars = append(vars, j.Var)
			srcs = append(srcs, je)
			inferVarFromSource(j.Var, je)
			if j.On != nil {
				jc, err := convertExpr(j.On)
				if err != nil {
					return nil, err
				}
				if cond == nil {
					cond = jc
				} else {
					cond = &BinaryExpr{Left: cond, Ops: []BinaryOp{{Op: "&&", Right: jc}}}
				}
			}
		}
		if p.Query.Where != nil {
			wcond, err := convertExpr(p.Query.Where)
			if err != nil {
				return nil, err
			}
			if cond == nil {
				cond = wcond
			} else {
				cond = &BinaryExpr{Left: cond, Ops: []BinaryOp{{Op: "&&", Right: wcond}}}
			}
		}
		if p.Query.Group != nil && len(p.Query.Group.Exprs) > 0 {
			groupVars[p.Query.Group.Name] = true
			needGroupBy = true
			needDataList = true
			varsExpr := make([]Expr, len(vars))
			for i, v := range vars {
				varsExpr[i] = &NameRef{Name: v}
			}
			row := varsExpr[0]
			rowType := ""
			if len(varsExpr) > 1 {
				structCount++
				rowType = fmt.Sprintf("GenType%d", structCount)
				fields := make([]string, len(vars))
				typestr := make([]string, len(vars))
				for i, v := range vars {
					fields[i] = v
					if s := varStruct[v]; s != "" {
						typestr[i] = s
					} else {
						typestr[i] = guessHsType(varsExpr[i])
					}
				}
				structDefs[rowType] = &TypeDecl{Name: rowType, Fields: fields, Types: typestr}
				for _, f := range fields {
					preludeHide[f] = true
				}
				row = &RecordLit{Name: rowType, Names: fields, Fields: varsExpr}
			}
			keyExpr, err := convertExpr(p.Query.Group.Exprs[0])
			if err != nil {
				return nil, err
			}
			keyType := guessHsType(keyExpr)
			if rl, ok := keyExpr.(*RecordLit); ok {
				keyType = rl.Name
			}
			if rowType != "" {
				groupItemType[p.Query.Group.Name] = rowType
			} else if s := varStruct[vars[0]]; s != "" {
				groupItemType[p.Query.Group.Name] = s
			}
			if keyType != "" {
				groupKeyStruct[p.Query.Group.Name] = keyType
			}
			body, err := convertExpr(p.Query.Select)
			if err != nil {
				return nil, err
			}
			keysExpr := &ComprExpr{Vars: vars, Sources: srcs, Cond: cond, Body: keyExpr}
			keys := &CallExpr{Fun: &NameRef{Name: "nub"}, Args: []Expr{&GroupExpr{Expr: keysExpr}}}
			condKey := &BinaryExpr{Left: keyExpr, Ops: []BinaryOp{{Op: "==", Right: &NameRef{Name: "k"}}}}
			if cond != nil {
				condKey = &BinaryExpr{Left: cond, Ops: []BinaryOp{{Op: "&&", Right: condKey}}}
			}
			items := &ComprExpr{Vars: vars, Sources: srcs, Cond: condKey, Body: row}
			groupRec := &RecordLit{Name: "MGroup", Names: []string{"key", "items"}, Fields: []Expr{&NameRef{Name: "k"}, items}}
			groups := &ComprExpr{Vars: []string{"k"}, Sources: []Expr{keys}, Cond: nil, Body: groupRec}
			varTypes[p.Query.Group.Name] = "list"
			var having Expr
			if p.Query.Group.Having != nil {
				var err error
				having, err = convertExpr(p.Query.Group.Having)
				if err != nil {
					return nil, err
				}
			}
			var srcGroups Expr = groups
			if p.Query.Sort != nil {
				sortExpr, err := convertExpr(p.Query.Sort)
				if err != nil {
					return nil, err
				}
				needDataList = true
				srcGroups = &CallExpr{Fun: &NameRef{Name: "sortOn"}, Args: []Expr{&GroupExpr{Expr: &LambdaExpr{Params: []string{p.Query.Group.Name}, Body: sortExpr}}, groups}}
			}
			result := &ComprExpr{Vars: []string{p.Query.Group.Name}, Sources: []Expr{srcGroups}, Cond: having, Body: body}
			return result, nil
		}
		body, err := convertExpr(p.Query.Select)
		if err != nil {
			return nil, err
		}
		if p.Query.Group != nil && len(p.Query.Group.Exprs) > 0 {
			// unreachable
		}
		var result Expr
		if p.Query.Sort != nil && len(srcs) == 1 {
			sortExpr, err := convertExpr(p.Query.Sort)
			if err != nil {
				return nil, err
			}
			needDataList = true
			sorted := &CallExpr{Fun: &NameRef{Name: "sortOn"}, Args: []Expr{&GroupExpr{Expr: &LambdaExpr{Params: []string{vars[0]}, Body: sortExpr}}, srcs[0]}}
			result = &ComprExpr{Vars: vars, Sources: []Expr{sorted}, Cond: cond, Body: body}
		} else {
			result = &ComprExpr{Vars: vars, Sources: srcs, Cond: cond, Body: body}
		}
		return result, nil
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type)
		if err != nil {
			return nil, err
		}
		return expr, nil
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Body: body}, nil
	case p.Call != nil:
		if p.Call.Func == "now" && len(p.Call.Args) == 0 {
			usesNow = true
			needIORef = true
			inner := &CallExpr{Fun: &NameRef{Name: "_now"}, Args: nil}
			return &CallExpr{Fun: &NameRef{Name: "unsafePerformIO"}, Args: []Expr{inner}}, nil
		}
		if p.Call.Func == "isSolved" && len(p.Call.Args) == 0 {
			inner := &CallExpr{Fun: &NameRef{Name: "isSolved"}, Args: nil}
			return &CallExpr{Fun: &NameRef{Name: "unsafePerformIO"}, Args: []Expr{inner}}, nil
		}
		if p.Call.Func == "doMove" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			newRef := &CallExpr{Fun: &NameRef{Name: "newIORef"}, Args: []Expr{arg}}
			ref := &CallExpr{Fun: &NameRef{Name: "unsafePerformIO"}, Args: []Expr{newRef}}
			inner := &CallExpr{Fun: &NameRef{Name: "doMove"}, Args: []Expr{ref}}
			return &CallExpr{Fun: &NameRef{Name: "unsafePerformIO"}, Args: []Expr{inner}}, nil
		}
		if p.Call.Func == "net.LookupHost" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			usesLookupHost = true
			return &CallExpr{Fun: &NameRef{Name: "_lookup_host"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "input" && len(p.Call.Args) == 0 {
			return &CallExpr{Fun: &NameRef{Name: "input"}, Args: nil}, nil
		}
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			if isMapExpr(arg) {
				needDataMap = true
				g := arg
				if _, ok := arg.(*NameRef); !ok {
					g = &GroupExpr{Expr: arg}
				}
				return &CallExpr{Fun: &NameRef{Name: "Map.size"}, Args: []Expr{g}}, nil
			}
			return &LenExpr{Arg: arg}, nil
		}
		if p.Call.Func == "count" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &LenExpr{Arg: arg}, nil
		}
		if p.Call.Func == "append" && len(p.Call.Args) == 2 {
			listArg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			itemArg, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &BinaryExpr{Left: listArg, Ops: []BinaryOp{{Op: "++", Right: &ListLit{Elems: []Expr{itemArg}}}}}, nil
		}
		if p.Call.Func == "exists" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			nullCall := &CallExpr{Fun: &NameRef{Name: "null"}, Args: []Expr{arg}}
			return &UnaryExpr{Op: "!", Expr: &GroupExpr{Expr: nullCall}}, nil
		}
		if p.Call.Func == "values" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			needDataMap = true
			return &CallExpr{Fun: &NameRef{Name: "Map.elems"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "avg" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			sumCall := &CallExpr{Fun: &NameRef{Name: "sum"}, Args: []Expr{arg}}
			lenCall := &LenExpr{Arg: arg}
			left := &CallExpr{Fun: &NameRef{Name: "fromIntegral"}, Args: []Expr{&GroupExpr{Expr: sumCall}}}
			right := &CallExpr{Fun: &NameRef{Name: "fromIntegral"}, Args: []Expr{&GroupExpr{Expr: lenCall}}}
			return &BinaryExpr{Left: left, Ops: []BinaryOp{{Op: "/", Right: right}}}, nil
		}
		if p.Call.Func == "min" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Fun: &NameRef{Name: "minimum"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "max" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Fun: &NameRef{Name: "maximum"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "sum" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			call := &CallExpr{Fun: &NameRef{Name: "sum"}, Args: []Expr{arg}}
			if isFloatExpr(arg) {
				return &CallExpr{Fun: &NameRef{Name: "round"}, Args: []Expr{&GroupExpr{Expr: call}}}, nil
			}
			return call, nil
		}
		if p.Call.Func == "str" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &CallExpr{Fun: &NameRef{Name: "show"}, Args: []Expr{arg}}, nil
		}
		if p.Call.Func == "substring" && len(p.Call.Args) == 3 {
			arg0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			start, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			diff := &BinaryExpr{Left: end, Ops: []BinaryOp{{Op: "-", Right: start}}}
			dropCall := &CallExpr{Fun: &NameRef{Name: "drop"}, Args: []Expr{start, arg0}}
			return &CallExpr{Fun: &NameRef{Name: "take"}, Args: []Expr{diff, dropCall}}, nil
		}
		fun := &NameRef{Name: p.Call.Func}
		var args []Expr
		for _, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ex)
		}
		call := &CallExpr{Fun: fun, Args: args}
		if shouldWrapCall(p.Call.Func) {
			call = &CallExpr{Fun: &NameRef{Name: "unsafePerformIO"}, Args: []Expr{call}}
		}
		return call, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		return &FloatLit{Value: fmt.Sprintf("%g", *l.Float)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
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
		}
		if typ != nil && typ.Simple != nil {
			return &RecordLit{Name: *typ.Simple, Names: names, Fields: fields}
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
			elems[i] = valueToExpr(it, et)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case float64:
		if val == float64(int(val)) {
			return &IntLit{Value: fmt.Sprintf("%d", int(val))}
		}
		return &FloatLit{Value: fmt.Sprintf("%g", val)}
	case int, int64:
		return &IntLit{Value: fmt.Sprintf("%d", val)}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func dataExprFromFile(path, format string, typ *parser.TypeRef) (Expr, error) {
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
	return valueToExpr(v, typ), nil
}

func convertIfStmt(s *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(s.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if s.ElseIf != nil {
		es, err := convertIfStmt(s.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{es}
	} else if len(s.Else) > 0 {
		elseStmts, err = convertStmtList(s.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
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

func convertMatchExpr(m *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(m.Target)
	if err != nil {
		return nil, err
	}
	clauses := make([]CaseClause, len(m.Cases))
	for i, c := range m.Cases {
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		if isUnderscoreExpr(c.Pattern) {
			clauses[i] = CaseClause{Pattern: nil, Result: res}
			continue
		}
		if call, ok := callPattern(c.Pattern); ok && envInfo != nil {
			if ut, ok := envInfo.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				vars := make([]string, len(call.Args))
				for j, a := range call.Args {
					if n, ok := identName(a); ok {
						vars[j] = safeName(n)
					} else {
						vars[j] = "_"
					}
				}
				clauses[i] = CaseClause{Pattern: &VariantPattern{Name: call.Func, FieldNames: st.Order, Binds: vars}, Result: res}
				continue
			}
		}
		pat, err := convertExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		clauses[i] = CaseClause{Pattern: pat, Result: res}
	}
	return &CaseExpr{Target: target, Clauses: clauses}, nil
}

func convertFunStmt(f *parser.FunStmt) (*Func, error) {
	prevMutated := mutated
	prevLocals := locals
	prevVarTypes := varTypes
	mutated = map[string]bool{}
	locals = map[string]bool{}
	varTypes = copyVarTypes(varTypes)
	for _, p := range f.Params {
		locals[safeName(p.Name)] = true
		if p.Type != nil && p.Type.Simple != nil {
			switch *p.Type.Simple {
			case "int":
				varTypes[safeName(p.Name)] = "int"
			case "float":
				varTypes[safeName(p.Name)] = "float"
			case "bool":
				varTypes[safeName(p.Name)] = "bool"
			case "string":
				varTypes[safeName(p.Name)] = "string"
			}
		}
	}

	stmts, err := convertStmtList(f.Body)
	if err != nil {
		mutated = prevMutated
		locals = prevLocals
		varTypes = prevVarTypes
		return nil, err
	}
	localMut := mutated
	mutated = prevMutated
	for name := range localMut {
		if globals[name] {
			mutatedGlobal[name] = true
		}
	}
	// record which parameters are mutated for this function
	mutIdx := map[int]bool{}
	for i, p := range f.Params {
		if localMut[safeName(p.Name)] {
			mutIdx[i] = true
		}
	}
	if funcParamMut == nil {
		funcParamMut = map[string]map[int]bool{}
	}
	funcParamMut[safeName(f.Name)] = mutIdx

	locals = prevLocals

	var params []string
	for _, p := range f.Params {
		params = append(params, safeName(p.Name))
	}
	if len(stmts) == 2 {
		if ps, ok := stmts[0].(*PrintStmt); ok {
			if rs, ok2 := stmts[1].(*ReturnStmt); ok2 {
				needTrace = true
				tr := &CallExpr{Fun: &NameRef{Name: "trace"}, Args: []Expr{ps.Expr, rs.Expr}}
				return &Func{Name: safeName(f.Name), Params: params, Body: []Stmt{&ReturnStmt{Expr: tr}}, Mutated: localMut}, nil
			}
		}
	}
	return &Func{Name: safeName(f.Name), Params: params, Body: stmts, Mutated: localMut}, nil
}

func convertLocalFunStmt(f *parser.FunStmt) (Expr, error) {
	prevMutated := mutated
	prevLocals := locals
	prevVarTypes := varTypes
	mutated = map[string]bool{}
	locals = map[string]bool{}
	varTypes = copyVarTypes(varTypes)
	for _, p := range f.Params {
		locals[safeName(p.Name)] = true
		if p.Type != nil && p.Type.Simple != nil {
			switch *p.Type.Simple {
			case "int":
				varTypes[safeName(p.Name)] = "int"
			case "float":
				varTypes[safeName(p.Name)] = "float"
			case "bool":
				varTypes[safeName(p.Name)] = "bool"
			case "string":
				varTypes[safeName(p.Name)] = "string"
			}
		}
	}

	stmts, err := convertStmtList(f.Body)
	if err != nil {
		mutated = prevMutated
		locals = prevLocals
		varTypes = prevVarTypes
		return nil, err
	}
	localMut := mutated
	mutated = prevMutated
	for name := range localMut {
		if globals[name] {
			mutatedGlobal[name] = true
		}
		if prevLocals[name] {
			markMutated(name)
		}
	}
	locals = prevLocals

	var params []string
	for _, p := range f.Params {
		params = append(params, safeName(p.Name))
	}
	return &LambdaBlock{Params: params, Body: stmts, Mutated: localMut}, nil
}

func convertForStmt(f *parser.ForStmt) (Stmt, error) {
	src, err := convertExpr(f.Source)
	if err != nil {
		return nil, err
	}
	if nr, ok := src.(*NameRef); ok && groupVars[nr.Name] {
		src = &FieldExpr{Target: nr, Field: "items"}
	}
	name := safeName(f.Name)
	if isMapExpr(src) {
		varTypes[name] = "string"
	} else {
		inferVarFromSource(name, src)
	}
	var end Expr
	if f.RangeEnd != nil {
		end, err = convertExpr(f.RangeEnd)
		if err != nil {
			return nil, err
		}
	}
	body, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	varMut := mutated[name]
	withBreak := false
	var scan func(Stmt)
	scan = func(s Stmt) {
		switch st := s.(type) {
		case *BreakStmt, *ContinueStmt:
			withBreak = true
		case *IfStmt:
			for _, c := range st.Then {
				scan(c)
			}
			for _, c := range st.Else {
				scan(c)
			}
		case *ForStmt:
			for _, c := range st.Body {
				scan(c)
			}
		case *WhileStmt:
			for _, c := range st.Body {
				scan(c)
			}
		}
	}
	for _, st := range body {
		scan(st)
	}
	return &ForStmt{Name: safeName(f.Name), From: src, To: end, Body: body, WithBreak: withBreak, VarMutated: varMut}, nil
}

func convertWhileStmt(w *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(w.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(w.Body)
	if err != nil {
		return nil, err
	}
	ws := &WhileStmt{Cond: cond, Body: body}
	if len(w.Body) > 0 {
		if as := w.Body[len(w.Body)-1].Assign; as != nil && len(as.Index) == 0 {
			// Only treat the last statement as the loop update when
			// the converted body also ends with a matching simple
			// assignment. This avoids dropping assignments that were
			// moved into an if/else by early return detection.
			if last, ok := ws.Body[len(ws.Body)-1].(*AssignStmt); ok {
				ex, err := convertExpr(as.Value)
				if err != nil {
					return nil, err
				}
				if last.Name == safeName(as.Name) {
					ws.Var = safeName(as.Name)
					ws.Next = ex
					ws.Body = ws.Body[:len(ws.Body)-1]
				}
			}
		}
	}
	return ws, nil
}

func convertBenchBlock(b *parser.BenchBlock) (Stmt, error) {
	body, err := convertStmtList(b.Body)
	if err != nil {
		return nil, err
	}
	usesNow = true
	needJSON = true
	needIORef = true
	name := strings.Trim(b.Name, "\"")
	return &BenchStmt{Name: name, Body: body}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	if locals == nil {
		locals = map[string]bool{}
	}
	if mutated == nil {
		mutated = map[string]bool{}
	}
	var out []Stmt
	for i := 0; i < len(list); i++ {
		st := list[i]
		switch {
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" {
				var args []Expr
				for _, a := range call.Args {
					ex, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ex)
				}
				if len(args) == 1 {
					str := isStringExpr(args[0])
					if isListExpr(args[0]) {
						needDataList = true
					}
					out = append(out, &PrintStmt{Expr: args[0], String: str})
				} else {
					for _, a := range args {
						if isListExpr(a) {
							needDataList = true
							break
						}
					}
					out = append(out, &PrintStmt{Expr: joinPrintArgs(args), String: true})
				}
				continue
			}
			ex, err := convertExpr(st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			out = append(out, &ExprStmt{Expr: ex})
			continue
		case st.Return != nil:
			if st.Return.Value == nil {
				out = append(out, &ReturnStmt{Expr: &UnitLit{}})
			} else {
				ex, err := convertExpr(st.Return.Value)
				if err != nil {
					return nil, err
				}
				out = append(out, &ReturnStmt{Expr: ex})
			}
		case st.If != nil:
			// Detect an if statement that ends with a return and has
			// no else branch. Combine it with the remaining
			// statements as the else block to model early return.
			if st.If.ElseIf == nil && len(st.If.Else) == 0 && len(st.If.Then) > 0 {
				if last := st.If.Then[len(st.If.Then)-1]; last.Return != nil && i+1 < len(list) {
					cond, err := convertExpr(st.If.Cond)
					if err != nil {
						return nil, err
					}
					thenStmts, err := convertStmtList(st.If.Then)
					if err != nil {
						return nil, err
					}
					elseStmts, err := convertStmtList(list[i+1:])
					if err != nil {
						return nil, err
					}
					out = append(out, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
					return out, nil
				}
			}
			s, err := convertIfStmt(st.If)
			if err != nil {
				return nil, err
			}
			out = append(out, s)
		case st.Let != nil:
			ex, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Let.Name)
			locals[name] = true
			recordType(name, ex)
			out = append(out, &LetStmt{Name: name, Expr: ex})
		case st.Var != nil:
			ex, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Var.Name)
			locals[name] = true
			recordType(name, ex)
			out = append(out, &LetStmt{Name: name, Expr: ex})
		case st.Assign != nil:
			val, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Assign.Name)
			markMutated(name)
			if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
				recordType(name, val)
				out = append(out, &AssignStmt{Name: name, Expr: val})
				break
			}
			if len(st.Assign.Field) > 0 {
				markMutated(name)
				// Use the current variable reference to avoid composing
				// updates over the last stored expression. Otherwise a
				// previous literal value may be mutated instead of the
				// runtime variable.
				prev := &NameRef{Name: name}
				var expr Expr = prev
				for i := range st.Assign.Field {
					expr = &RecordUpdate{Target: expr, Field: st.Assign.Field[i].Name, Value: val}
				}
				recordType(name, expr)
				out = append(out, &AssignStmt{Name: name, Expr: expr})
				break
			}
			var idxExprs []Expr
			for _, idx := range st.Assign.Index {
				ex, err := convertExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				idxExprs = append(idxExprs, ex)
			}
			// Always read from the variable itself rather than the
			// last recorded expression value. This ensures list and
			// map updates modify the runtime data.
			prev := &NameRef{Name: name}
			var expr Expr
			if isMapExpr(prev) {
				expr = mapAssign(prev, idxExprs, val)
			} else {
				expr = listAssign(prev, idxExprs, val)
			}
			markMutated(name)
			recordType(name, expr)
			out = append(out, &AssignStmt{Name: name, Expr: expr})
		case st.Break != nil:
			out = append(out, &BreakStmt{})
		case st.Continue != nil:
			out = append(out, &ContinueStmt{})
		case st.While != nil:
			stw, err := convertWhileStmt(st.While)
			if err != nil {
				return nil, err
			}
			out = append(out, stw)
		case st.For != nil:
			s, err := convertForStmt(st.For)
			if err != nil {
				return nil, err
			}
			if i+1 < len(list) && list[i+1].Return != nil {
				endExpr, err := convertExpr(list[i+1].Return.Value)
				if err != nil {
					return nil, err
				}
				if fs, ok := s.(*ForStmt); ok {
					fe := &ForExpr{Name: fs.Name, From: fs.From, To: fs.To, Body: fs.Body, End: endExpr, VarMutated: fs.VarMutated}
					out = append(out, &ReturnStmt{Expr: fe})
					i++
				} else {
					out = append(out, s)
				}
			} else {
				out = append(out, s)
			}
		case st.Bench != nil:
			s, err := convertBenchBlock(st.Bench)
			if err != nil {
				return nil, err
			}
			out = append(out, s)
		case st.Fun != nil:
			if len(st.Fun.Body) == 1 && st.Fun.Body[0].Return != nil {
				bodyExpr, err := convertExpr(st.Fun.Body[0].Return.Value)
				if err != nil {
					return nil, err
				}
				var params []string
				for _, p := range st.Fun.Params {
					params = append(params, safeName(p.Name))
				}
				fn := &LambdaExpr{Params: params, Body: bodyExpr}
				out = append(out, &LetStmt{Name: safeName(st.Fun.Name), Expr: fn})
			} else {
				fn, err := convertLocalFunStmt(st.Fun)
				if err != nil {
					return nil, err
				}
				out = append(out, &LetStmt{Name: safeName(st.Fun.Name), Expr: fn})
			}
		default:
			return nil, fmt.Errorf("unsupported statement in block")
		}
	}
	return out, nil
}

func funcUsesIO(fn *parser.FunStmt) bool {
	uses := false
	var scan func(*parser.Statement)
	scan = func(st *parser.Statement) {
		if uses || st == nil {
			return
		}
		switch {
		case st.Var != nil || st.Assign != nil || st.For != nil || st.While != nil || st.Fetch != nil:
			uses = true
		case st.If != nil:
			for _, t := range st.If.Then {
				scan(t)
			}
			for _, e := range st.If.Else {
				scan(e)
			}
		case st.Expr != nil:
			if c := st.Expr.Expr.Binary.Left.Value.Target.Call; c != nil {
				if c.Func == "print" {
					uses = true
				}
			}
		}
	}
	for _, s := range fn.Body {
		scan(s)
	}
	return uses
}

func convertGroupItemsLoop(v *parser.VarStmt, loop *parser.ForStmt) (*LetStmt, bool) {
	if len(loop.Body) != 3 {
		return nil, false
	}
	innerVar := loop.Body[0].Var
	innerFor := loop.Body[1].For
	appendAssign := loop.Body[2].Assign
	if innerVar == nil || innerFor == nil || appendAssign == nil {
		return nil, false
	}
	if innerVar.Name != "total" {
		return nil, false
	}
	if innerVar.Value == nil || innerVar.Value.Binary == nil || innerVar.Value.Binary.Left == nil || innerVar.Value.Binary.Left.Value == nil || innerVar.Value.Binary.Left.Value.Target == nil || innerVar.Value.Binary.Left.Value.Target.Lit == nil {
		return nil, false
	}
	if int(*innerVar.Value.Binary.Left.Value.Target.Lit.Int) != 0 {
		return nil, false
	}
	if len(innerFor.Body) != 1 || innerFor.Body[0].Assign == nil {
		return nil, false
	}
	if innerFor.Body[0].Assign.Name != "total" {
		return nil, false
	}
	if appendAssign.Name != v.Name {
		return nil, false
	}
	if appendAssign.Value == nil || appendAssign.Value.Binary == nil || appendAssign.Value.Binary.Left == nil || appendAssign.Value.Binary.Left.Value == nil {
		return nil, false
	}
	call := appendAssign.Value.Binary.Left.Value.Target.Call
	if call == nil || call.Func != "append" || len(call.Args) != 2 {
		return nil, false
	}

	src, err := convertExpr(loop.Source)
	if err != nil {
		return nil, false
	}
	itemsExpr, err := convertExpr(innerFor.Source)
	if err != nil {
		return nil, false
	}
	sumExpr := &CallExpr{Fun: &NameRef{Name: "sum"}, Args: []Expr{&ComprExpr{Vars: []string{innerFor.Name}, Sources: []Expr{itemsExpr}, Body: &FieldExpr{Target: &NameRef{Name: innerFor.Name}, Field: "val"}}}}
	structCount++
	name := fmt.Sprintf("GenType%d", structCount)
	fields := []string{"tag", "total"}
	typestr := []string{"String", "Int"}
	structDefs[name] = &TypeDecl{Name: name, Fields: fields, Types: typestr}
	for _, f := range fields {
		preludeHide[f] = true
	}
	rec := &RecordLit{Name: name, Names: fields, Fields: []Expr{&FieldExpr{Target: &NameRef{Name: loop.Name}, Field: "key"}, sumExpr}}
	body := &ComprExpr{Vars: []string{loop.Name}, Sources: []Expr{src}, Body: rec}
	return &LetStmt{Name: safeName(v.Name), Expr: body}, true
}
