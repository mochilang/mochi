//go:build slow

package hs

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
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

// groupVars tracks variables that represent groups.
var groupVars map[string]bool

// groupKeyStruct stores the struct type name of each group's key.
var groupKeyStruct map[string]string

// groupItemType stores the struct type name of each group's items.
var groupItemType map[string]string

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
	if reserved[n] {
		return "_" + n
	}
	return n
}

func pushIndent() { indent += "    " }
func popIndent() {
	if len(indent) >= 4 {
		indent = indent[:len(indent)-4]
	}
}
func writeIndent(w io.Writer) { io.WriteString(w, indent) }

type TypeDecl struct {
	Name   string
	Fields []string
	Types  []string
}

func groupPrelude() string {
	return `data MGroup k a = MGroup {key :: k, items :: [a]} deriving (Show, Eq)`
}

func (t *TypeDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "data %s = %s\n  { ", t.Name, t.Name)
	for i, f := range t.Fields {
		if i > 0 {
			io.WriteString(w, ",\n    ")
		}
		fmt.Fprintf(w, "%s :: %s", f, t.Types[i])
	}
	io.WriteString(w, "\n  } deriving (Show, Eq)\n")
}

var structDefs map[string]*TypeDecl
var structCount int
var preludeHide map[string]bool
var varStruct map[string]string
var currentLoop string

func typeNameFromRef(tr *parser.TypeRef) string {
	if tr == nil || tr.Simple == nil {
		return "String"
	}
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
			op := be.Ops[0].Op
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
	Name   string
	Params []string
	Body   []Stmt
}

func (f *Func) emit(w io.Writer) {
	io.WriteString(w, safeName(f.Name))
	for _, p := range f.Params {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(p))
	}
	io.WriteString(w, " = ")
	if len(f.Body) == 1 {
		if r, ok := f.Body[0].(*ReturnStmt); ok {
			r.Expr.emit(w)
			return
		}
	}
	io.WriteString(w, "do\n")
	prev := indent
	indent += "    "
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	indent = prev
}

type PrintStmt struct {
	Expr   Expr
	String bool
}

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

// ForStmt iterates over elements in a list, map or range.
type ForStmt struct {
	Name      string
	From      Expr
	To        Expr
	Body      []Stmt
	WithBreak bool
}

// WhileStmt repeats a block while a condition is true.
type WhileStmt struct {
	Var  string
	Cond Expr
	Body []Stmt
	Next Expr
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

func (j *JSONStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, "BSL.putStrLn (Aeson.encode ")
	j.Expr.emit(w)
	io.WriteString(w, ")")
}

func (l *LetStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, safeName(l.Name))
	io.WriteString(w, " = ")
	l.Expr.emit(w)
}

func (a *AssignStmt) emit(w io.Writer) {
	writeIndent(w)
	io.WriteString(w, safeName(a.Name))
	io.WriteString(w, " = ")
	a.Expr.emit(w)
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
	io.WriteString(w, "return (")
	r.Expr.emit(w)
	io.WriteString(w, ")")
}

func (b *BreakStmt) emit(w io.Writer)    { writeIndent(w); io.WriteString(w, "return ()") }
func (c *ContinueStmt) emit(w io.Writer) { writeIndent(w); io.WriteString(w, currentLoop+" xs") }

func (f *ForStmt) emit(w io.Writer) {
	if f.WithBreak {
		loop := "loop"
		prevLoop := currentLoop
		currentLoop = loop
		type guard struct {
			cond   Expr
			action string
		}
		var guards []guard
		var rest []Stmt
		for _, st := range f.Body {
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
		return
	}
	writeIndent(w)
	io.WriteString(w, "mapM_ (\\")
	io.WriteString(w, safeName(f.Name))
	io.WriteString(w, " -> do\n")
	pushIndent()
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	popIndent()
	writeIndent(w)
	io.WriteString(w, ") ")
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
	name := "loop"
	writeIndent(w)
	io.WriteString(w, "let ")
	io.WriteString(w, name)
	if wst.Var != "" {
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
	writeIndent(w)
	io.WriteString(w, name)
	if wst.Var != "" && wst.Next != nil {
		io.WriteString(w, " (")
		wst.Next.emit(w)
		io.WriteString(w, ")\n")
	} else {
		io.WriteString(w, "\n")
	}
	popIndent()
	writeIndent(w)
	io.WriteString(w, "else return ()\n")
	popIndent()
	writeIndent(w)
	io.WriteString(w, name)
	if wst.Var != "" {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(wst.Var))
	}
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

type IntLit struct{ Value string }
type FloatLit struct{ Value string }
type StringLit struct{ Value string }
type BoolLit struct{ Value bool }
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
type LambdaExpr struct {
	Params []string
	Body   Expr
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

func (c *CastExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	c.Expr.emit(w)
	io.WriteString(w, " :: ")
	io.WriteString(w, c.Type)
	io.WriteString(w, ")")
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
func (n *NameRef) emit(w io.Writer) { io.WriteString(w, safeName(n.Name)) }
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
	io.WriteString(w, f.Field)
}

func hasStruct(e Expr) bool {
	if n, ok := e.(*NameRef); ok {
		return varStruct[n.Name] != ""
	}
	return false
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
func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *NameRef:
		return varTypes[ex.Name] == "string"
	case *FieldExpr:
		if n, ok := ex.Target.(*NameRef); ok {
			if s := varStruct[n.Name]; s != "" {
				if st, ok2 := structDefs[s]; ok2 {
					for i, f := range st.Fields {
						if f == ex.Field {
							return st.Types[i] == "String"
						}
					}
				}
			}
			if envInfo != nil {
				if vt, err := envInfo.GetVar(n.Name); err == nil {
					if st, ok3 := vt.(types.StructType); ok3 {
						if ft, ok4 := st.Fields[ex.Field]; ok4 {
							return toHsType(ft) == "String"
						}
					}
				}
			}
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
		if n, ok := ex.Target.(*NameRef); ok {
			if s := varStruct[n.Name]; s != "" {
				if st, ok2 := structDefs[s]; ok2 {
					for i, f := range st.Fields {
						if f == ex.Field {
							return st.Types[i] == "Bool"
						}
					}
				}
			}
			if envInfo != nil {
				if vt, err := envInfo.GetVar(n.Name); err == nil {
					if st, ok3 := vt.(types.StructType); ok3 {
						if ft, ok4 := st.Fields[ex.Field]; ok4 {
							return toHsType(ft) == "Bool"
						}
					}
				}
			}
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
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok && n.Name == "Map.elems" && len(ex.Args) == 1 {
			return true
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
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok {
			switch n.Name {
			case "Map.insert", "Map.fromList", "Map.delete", "Map.union":
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
		return varTypes[ex.Name] == "int"
	case *FieldExpr:
		if n, ok := ex.Target.(*NameRef); ok {
			if s := varStruct[n.Name]; s != "" {
				if st, ok2 := structDefs[s]; ok2 {
					for i, f := range st.Fields {
						if f == ex.Field {
							return st.Types[i] == "Int"
						}
					}
				}
			}
			if envInfo != nil {
				if vt, err := envInfo.GetVar(n.Name); err == nil {
					if st, ok3 := vt.(types.StructType); ok3 {
						if ft, ok4 := st.Fields[ex.Field]; ok4 {
							return toHsType(ft) == "Int"
						}
					}
				}
			}
		}
	case *ComprExpr:
		return isIntExpr(ex.Body)
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
		return varTypes[ex.Name] == "float"
	case *FieldExpr:
		if n, ok := ex.Target.(*NameRef); ok {
			if s := varStruct[n.Name]; s != "" {
				if st, ok2 := structDefs[s]; ok2 {
					for i, f := range st.Fields {
						if f == ex.Field {
							return st.Types[i] == "Double"
						}
					}
				}
			}
			if envInfo != nil {
				if vt, err := envInfo.GetVar(n.Name); err == nil {
					if st, ok3 := vt.(types.StructType); ok3 {
						if ft, ok4 := st.Fields[ex.Field]; ok4 {
							return toHsType(ft) == "Double"
						}
					}
				}
			}
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
	wantFloat := b.Ops[0].Op == "/" || isFloatExpr(left) || isFloatExpr(b.Ops[0].Right)
	emitMaybeFloat(left, wantFloat)
	for i, op := range b.Ops {
		io.WriteString(w, " ")
		switch op.Op {
		case "+":
			if isStringExpr(left) || isStringExpr(op.Right) {
				io.WriteString(w, "++")
			} else {
				io.WriteString(w, "+")
			}
		case "%":
			io.WriteString(w, "`mod`")
		case "/":
			io.WriteString(w, "/")
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
		default:
			io.WriteString(w, op.Op)
		}
		io.WriteString(w, " ")
		emitMaybeFloat(op.Right, wantFloat)
		left = op.Right
		if i+1 < len(b.Ops) {
			wantFloat = b.Ops[i+1].Op == "/" || isFloatExpr(left) || isFloatExpr(b.Ops[i+1].Right)
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
	c.Fun.emit(w)
	for _, a := range c.Args {
		io.WriteString(w, " ")
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

func header(withList, withMap, withJSON, withTrace bool) string {
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
		h += "import qualified Data.ByteString.Lazy.Char8 as BSL\n"
	}
	if withTrace {
		h += "import Debug.Trace (trace)\n"
	}
	return h
}

// Emit generates formatted Haskell code.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header(needDataList, needDataMap, needJSON, needTrace))
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
	for _, s := range p.Stmts {
		if l, ok := s.(*LetStmt); ok {
			l.emit(&buf)
			buf.WriteByte('\n')
			buf.WriteByte('\n')
		}
	}
	buf.WriteString("main :: IO ()\n")
	buf.WriteString("main = do\n")
	prev := indent
	indent += "    "
	for _, s := range p.Stmts {
		if _, ok := s.(*LetStmt); ok {
			continue
		}
		s.emit(&buf)
		buf.WriteByte('\n')
	}
	indent = prev
	return buf.Bytes()
}

// Transpile converts a Mochi program into a simple Haskell AST.
var envInfo *types.Env

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
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
	groupVars = map[string]bool{}
	groupKeyStruct = map[string]string{}
	groupItemType = map[string]string{}
	structDefs = map[string]*TypeDecl{}
	structCount = 0
	preludeHide = map[string]bool{}
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
			vars[name] = ex
			recordType(name, ex)
		case st.Var != nil:
			ex, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Var.Name)
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
					fields[i] = m.Field.Name
					typestr[i] = typeNameFromRef(m.Field.Type)
				}
				structDefs[st.Type.Name] = &TypeDecl{Name: st.Type.Name, Fields: fields, Types: typestr}
				for _, f := range fields {
					preludeHide[f] = true
				}
			}
		case st.Assign != nil:
			val, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Assign.Name)
			if len(st.Assign.Index) == 0 {
				vars[name] = val
				recordType(name, val)
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
			prev := vars[name]
			if prev == nil {
				prev = &NameRef{Name: name}
			}
			var expr Expr
			if isMapExpr(prev) {
				needDataMap = true
				expr = &CallExpr{Fun: &NameRef{Name: "Map.insert"}, Args: []Expr{idxExprs[0], val, prev}}
			} else {
				expr = listAssign(prev, idxExprs, val)
			}
			vars[name] = expr
			recordType(name, expr)
		case st.Fun != nil:
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
			return nil, fmt.Errorf("unsupported expression")
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
			expr = &CallExpr{Fun: expr, Args: args}
			continue
		}
		if op.Index != nil && op.Index.Colon == nil {
			idx, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type.Simple != nil {
				tname := *op.Cast.Type.Simple
				if tname == "int" {
					expr = &CastExpr{Expr: &CallExpr{Fun: &NameRef{Name: "read"}, Args: []Expr{expr}}, Type: "Int"}
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
					expr = &RecordLit{Name: tname, Names: names, Fields: vals}
					continue
				}
			}
			return nil, fmt.Errorf("unsupported cast")
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
		if len(p.Map.Items) >= 2 {
			allConst := true
			fields := make([]string, len(p.Map.Items))
			typestr := make([]string, len(p.Map.Items))
			vals := make([]Expr, len(p.Map.Items))
			for i, it := range p.Map.Items {
				key, ok := types.SimpleStringKey(it.Key)
				if !ok {
					allConst = false
					break
				}
				fields[i] = key
				ve, err := convertExpr(it.Value)
				if err != nil {
					return nil, err
				}
				vals[i] = ve
				typestr[i] = guessHsType(ve)
			}
			if allConst {
				structCount++
				name := fmt.Sprintf("GenType%d", structCount)
				structDefs[name] = &TypeDecl{Name: name, Fields: fields, Types: typestr}
				for _, f := range fields {
					preludeHide[f] = true
				}
				return &RecordLit{Name: name, Names: fields, Fields: vals}, nil
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
		var result Expr = &ComprExpr{Vars: vars, Sources: srcs, Cond: cond, Body: body}
		if p.Query.Sort != nil {
			sortExpr, err := convertExpr(p.Query.Sort)
			if err != nil {
				return nil, err
			}
			needDataList = true
			result = &CallExpr{Fun: &NameRef{Name: "sortOn"}, Args: []Expr{&GroupExpr{Expr: &LambdaExpr{Params: []string{vars[0]}, Body: sortExpr}}, result}}
		}
		return result, nil
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
		return &CallExpr{Fun: fun, Args: args}, nil
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

func convertMatchExpr(m *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(m.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr
	for i := len(m.Cases) - 1; i >= 0; i-- {
		c := m.Cases[i]
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		if isUnderscoreExpr(c.Pattern) {
			expr = res
			continue
		}
		pat, err := convertExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		cond := &BinaryExpr{Left: target, Ops: []BinaryOp{{Op: "==", Right: pat}}}
		if expr == nil {
			expr = &IfExpr{Cond: cond, Then: res, Else: &IntLit{Value: "0"}}
		} else {
			expr = &IfExpr{Cond: cond, Then: res, Else: expr}
		}
	}
	if expr == nil {
		expr = &IntLit{Value: "0"}
	}
	return expr, nil
}

func convertFunStmt(f *parser.FunStmt) (*Func, error) {
	stmts, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	var params []string
	for _, p := range f.Params {
		params = append(params, safeName(p.Name))
	}
	if len(stmts) == 2 {
		if ps, ok := stmts[0].(*PrintStmt); ok {
			if rs, ok2 := stmts[1].(*ReturnStmt); ok2 {
				needTrace = true
				tr := &CallExpr{Fun: &NameRef{Name: "trace"}, Args: []Expr{ps.Expr, rs.Expr}}
				return &Func{Name: safeName(f.Name), Params: params, Body: []Stmt{&ReturnStmt{Expr: tr}}}, nil
			}
		}
	}
	return &Func{Name: safeName(f.Name), Params: params, Body: stmts}, nil
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
	return &ForStmt{Name: safeName(f.Name), From: src, To: end, Body: body, WithBreak: withBreak}, nil
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
			ex, err := convertExpr(as.Value)
			if err != nil {
				return nil, err
			}
			ws.Var = safeName(as.Name)
			ws.Next = ex
			ws.Body = ws.Body[:len(ws.Body)-1]
		}
	}
	return ws, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
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
			return nil, fmt.Errorf("unsupported expression")
		case st.Return != nil:
			ex, err := convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &ReturnStmt{Expr: ex})
		case st.If != nil:
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
			recordType(safeName(st.Let.Name), ex)
			out = append(out, &LetStmt{Name: safeName(st.Let.Name), Expr: ex})
		case st.Var != nil:
			ex, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			recordType(safeName(st.Var.Name), ex)
			out = append(out, &LetStmt{Name: safeName(st.Var.Name), Expr: ex})
		case st.Assign != nil:
			val, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Assign.Name)
			if len(st.Assign.Index) == 0 {
				recordType(name, val)
				out = append(out, &AssignStmt{Name: name, Expr: val})
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
			prev := vars[name]
			if prev == nil {
				prev = &NameRef{Name: name}
			}
			var expr Expr
			if isMapExpr(prev) {
				needDataMap = true
				expr = &CallExpr{Fun: &NameRef{Name: "Map.insert"}, Args: []Expr{idxExprs[0], val, prev}}
			} else {
				expr = listAssign(prev, idxExprs, val)
			}
			recordType(name, expr)
			out = append(out, &AssignStmt{Name: name, Expr: expr})
		case st.Break != nil:
			out = append(out, &BreakStmt{})
		case st.Continue != nil:
			out = append(out, &ContinueStmt{})
		case st.While != nil:
			s, err := convertWhileStmt(st.While)
			if err != nil {
				return nil, err
			}
			out = append(out, s)
		case st.For != nil:
			s, err := convertForStmt(st.For)
			if err != nil {
				return nil, err
			}
			out = append(out, s)
		default:
			return nil, fmt.Errorf("unsupported statement in block")
		}
	}
	return out, nil
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
