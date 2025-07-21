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

// groupVars tracks variables that represent groups.
var groupVars map[string]bool

// vars holds the last computed expression of each variable at the top level.
var vars map[string]Expr

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

type TypeDecl struct {
	Name   string
	Fields []string
	Types  []string
}

func groupPrelude() string {
	return `data MGroup k a = MGroup {key :: k, items :: [a]} deriving (Show)`
}

func (t *TypeDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "data %s = %s\n  { ", t.Name, t.Name)
	for i, f := range t.Fields {
		if i > 0 {
			io.WriteString(w, ",\n    ")
		}
		fmt.Fprintf(w, "%s :: %s", f, t.Types[i])
	}
	io.WriteString(w, "\n  } deriving (Show)\n")
}

var structDefs map[string]*TypeDecl
var structCount int
var preludeHide map[string]bool

func recordType(name string, ex Expr) {
	if isStringExpr(ex) {
		varTypes[name] = "string"
	} else if isBoolExpr(ex) {
		varTypes[name] = "bool"
	} else if isListExpr(ex) {
		varTypes[name] = "list"
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
	for _, st := range f.Body {
		io.WriteString(w, "    ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
}

type PrintStmt struct {
	Expr   Expr
	String bool
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

// ForStmt iterates over elements in a list, map or range.
type ForStmt struct {
	Name string
	From Expr
	To   Expr
	Body []Stmt
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
	if p.String {
		io.WriteString(w, "putStrLn (")
		p.Expr.emit(w)
		io.WriteString(w, ")")
		return
	}

	if isListExpr(p.Expr) {
		needDataList = true
		if isMapElemsExpr(p.Expr) {
			io.WriteString(w, "putStrLn (intercalate \" \" (map show (")
			p.Expr.emit(w)
			io.WriteString(w, ")))")
		} else {
			io.WriteString(w, "putStrLn (\"[\" ++ intercalate \", \" (map show (")
			p.Expr.emit(w)
			io.WriteString(w, ")) ++ \"]\")")
		}
		return
	}

	if isBoolExpr(p.Expr) {
		io.WriteString(w, "putStrLn (")
		switch p.Expr.(type) {
		case *BoolLit, *NameRef:
			io.WriteString(w, "if ")
			p.Expr.emit(w)
			io.WriteString(w, " then \"true\" else \"false\"")
		default:
			io.WriteString(w, "if ")
			p.Expr.emit(w)
			io.WriteString(w, " then \"1\" else \"0\"")
		}
		io.WriteString(w, ")")
		return
	}

	if _, ok := p.Expr.(*StringLit); ok {
		io.WriteString(w, "putStrLn (")
	} else {
		io.WriteString(w, "print (")
	}
	p.Expr.emit(w)
	io.WriteString(w, ")")
}

func (l *LetStmt) emit(w io.Writer) {
	io.WriteString(w, safeName(l.Name))
	io.WriteString(w, " = ")
	l.Expr.emit(w)
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, safeName(a.Name))
	io.WriteString(w, " = ")
	a.Expr.emit(w)
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if ")
	i.Cond.emit(w)
	io.WriteString(w, " then do\n")
	for _, st := range i.Then {
		io.WriteString(w, "        ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if len(i.Else) > 0 {
		io.WriteString(w, "    else do\n")
		for _, st := range i.Else {
			io.WriteString(w, "        ")
			st.emit(w)
			io.WriteString(w, "\n")
		}
	}
}
func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return (")
	r.Expr.emit(w)
	io.WriteString(w, ")")
}

func (f *ForStmt) emit(w io.Writer) {
	io.WriteString(w, "mapM_ (\\")
	io.WriteString(w, safeName(f.Name))
	io.WriteString(w, " -> do\n")
	for _, st := range f.Body {
		io.WriteString(w, "        ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "        ) ")
	if f.To != nil {
		io.WriteString(w, "[")
		f.From.emit(w)
		io.WriteString(w, " .. ")
		io.WriteString(w, "(")
		f.To.emit(w)
		io.WriteString(w, " - 1)")
		io.WriteString(w, "]")
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
	io.WriteString(w, "let ")
	io.WriteString(w, name)
	if wst.Var != "" {
		io.WriteString(w, " ")
		io.WriteString(w, safeName(wst.Var))
	}
	io.WriteString(w, " = do\n")
	io.WriteString(w, "            if ")
	wst.Cond.emit(w)
	io.WriteString(w, " then do\n")
	for _, st := range wst.Body {
		io.WriteString(w, "                ")
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "                ")
	io.WriteString(w, name)
	if wst.Var != "" && wst.Next != nil {
		io.WriteString(w, " (")
		wst.Next.emit(w)
		io.WriteString(w, ")\n")
	} else {
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "            else return ()\n")
	io.WriteString(w, "    ")
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

type IntLit struct{ Value string }
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

func (i *IntLit) emit(w io.Writer)    { io.WriteString(w, i.Value) }
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
		m.Keys[i].emit(w)
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
	if isMapExpr(f.Target) {
		needDataMap = true
		f.Target.emit(w)
		fmt.Fprintf(w, " Map.! %q", f.Field)
		return
	}
	f.Target.emit(w)
	io.WriteString(w, ".")
	io.WriteString(w, f.Field)
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
	l.Arg.emit(w)
}
func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *NameRef:
		return varTypes[ex.Name] == "string"
	case *IndexExpr:
		if n, ok := ex.Target.(*NameRef); ok {
			return varTypes[n.Name] == "map_string"
		}
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok && n.Name == "show" && len(ex.Args) == 1 {
			return true
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
	}
	return false
}

func isListExpr(e Expr) bool {
	switch ex := e.(type) {
	case *ListLit:
		return true
	case *NameRef:
		return varTypes[ex.Name] == "list"
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
	}
	return false
}

func isFloatExpr(e Expr) bool {
	switch ex := e.(type) {
	case *CallExpr:
		if n, ok := ex.Fun.(*NameRef); ok && n.Name == "fromIntegral" {
			return true
		}
	case *BinaryExpr:
		if len(ex.Ops) > 0 && ex.Ops[0].Op == "/" {
			return true
		}
	}
	return false
}

func toStringExpr(e Expr) Expr {
	if isStringExpr(e) {
		return e
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
	left := b.Left
	left.emit(w)
	for _, op := range b.Ops {
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
			if isFloatExpr(left) || isFloatExpr(op.Right) {
				io.WriteString(w, "/")
			} else {
				io.WriteString(w, "`div`")
			}
		case "in":
			if isStringExpr(left) || isStringExpr(op.Right) {
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
		op.Right.emit(w)
		left = op.Right
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
		a.emit(w)
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

func header(withList bool, withMap bool) string {
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
	return h
}

// Emit generates formatted Haskell code.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header(needDataList, needDataMap))
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
	for _, s := range p.Stmts {
		if _, ok := s.(*LetStmt); ok {
			continue
		}
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteByte('\n')
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a simple Haskell AST.
var envInfo *types.Env

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	envInfo = env
	h := &Program{}
	vars = map[string]Expr{}
	varTypes = map[string]string{}
	needDataList = false
	needDataMap = false
	needGroupBy = false
	groupVars = map[string]bool{}
	structDefs = map[string]*TypeDecl{}
	structCount = 0
	preludeHide = map[string]bool{}
	for _, st := range prog.Statements {
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
		case st.Assign != nil:
			ex, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			name := safeName(st.Assign.Name)
			vars[name] = ex
			recordType(name, ex)
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
		if groupVars[name] {
			return &FieldExpr{Target: &NameRef{Name: name}, Field: "items"}, nil
		}
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
		if len(p.Map.Items) >= 3 && envInfo != nil {
			if st, ok := types.InferStructFromMapEnv(p.Map, envInfo); ok {
				structCount++
				name := fmt.Sprintf("GenType%d", structCount)
				fields := make([]string, len(st.Order))
				typestr := make([]string, len(st.Order))
				vals := make([]Expr, len(st.Order))
				for i, it := range p.Map.Items {
					fields[i] = st.Order[i]
					typestr[i] = toHsType(st.Fields[st.Order[i]])
					ve, err := convertExpr(it.Value)
					if err != nil {
						return nil, err
					}
					vals[i] = ve
				}
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
		srcs = append(srcs, se)
		for _, f := range p.Query.Froms {
			fe, err := convertExpr(f.Src)
			if err != nil {
				return nil, err
			}
			vars = append(vars, f.Var)
			srcs = append(srcs, fe)
		}
		var cond Expr
		for _, j := range p.Query.Joins {
			je, err := convertExpr(j.Src)
			if err != nil {
				return nil, err
			}
			vars = append(vars, j.Var)
			srcs = append(srcs, je)
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
		}
		body, err := convertExpr(p.Query.Select)
		if err != nil {
			return nil, err
		}
		if p.Query.Group != nil && len(p.Query.Group.Exprs) > 0 {
			needGroupBy = true
			needDataList = true
			varsExpr := make([]Expr, len(vars))
			for i, v := range vars {
				varsExpr[i] = &NameRef{Name: v}
			}
			row := varsExpr[0]
			if len(varsExpr) > 1 {
				row = &ListLit{Elems: varsExpr}
			}
			list := &ComprExpr{Vars: vars, Sources: srcs, Cond: cond, Body: row}
			keyExpr, err := convertExpr(p.Query.Group.Exprs[0])
			if err != nil {
				return nil, err
			}
			mapKeys := &CallExpr{Fun: &NameRef{Name: "map"}, Args: []Expr{&GroupExpr{Expr: &LambdaExpr{Params: vars, Body: keyExpr}}, list}}
			keys := &CallExpr{Fun: &NameRef{Name: "nub"}, Args: []Expr{&GroupExpr{Expr: mapKeys}}}
			condKey := &BinaryExpr{Left: keyExpr, Ops: []BinaryOp{{Op: "==", Right: &NameRef{Name: "k"}}}}
			if cond != nil {
				condKey = &BinaryExpr{Left: cond, Ops: []BinaryOp{{Op: "&&", Right: condKey}}}
			}
			items := &ComprExpr{Vars: vars, Sources: srcs, Cond: condKey, Body: row}
			groupRec := &RecordLit{Name: "MGroup", Names: []string{"key", "items"}, Fields: []Expr{&NameRef{Name: "k"}, items}}
			groups := &ComprExpr{Vars: []string{"k"}, Sources: []Expr{keys}, Cond: nil, Body: groupRec}
			varTypes[p.Query.Group.Name] = "list"
			var result Expr = &ComprExpr{Vars: []string{p.Query.Group.Name}, Sources: []Expr{groups}, Cond: nil, Body: body}
			if p.Query.Sort != nil {
				sortExpr, err := convertExpr(p.Query.Sort)
				if err != nil {
					return nil, err
				}
				needDataList = true
				result = &CallExpr{Fun: &NameRef{Name: "sortOn"}, Args: []Expr{&GroupExpr{Expr: &LambdaExpr{Params: []string{p.Query.Group.Name}, Body: sortExpr}}, result}}
			}
			return result, nil
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

func convertFunStmt(f *parser.FunStmt) (*Func, error) {
	stmts, err := convertStmtList(f.Body)
	if err != nil {
		return nil, err
	}
	var params []string
	for _, p := range f.Params {
		params = append(params, safeName(p.Name))
	}
	return &Func{Name: safeName(f.Name), Params: params, Body: stmts}, nil
}

func convertForStmt(f *parser.ForStmt) (Stmt, error) {
	src, err := convertExpr(f.Source)
	if err != nil {
		return nil, err
	}
	if isMapExpr(src) {
		varTypes[safeName(f.Name)] = "string"
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
	return &ForStmt{Name: safeName(f.Name), From: src, To: end, Body: body}, nil
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
			out = append(out, &LetStmt{Name: safeName(st.Let.Name), Expr: ex})
		case st.Var != nil:
			ex, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &LetStmt{Name: safeName(st.Var.Name), Expr: ex})
		case st.Assign != nil && len(st.Assign.Index) == 0:
			ex, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssignStmt{Name: safeName(st.Assign.Name), Expr: ex})
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
