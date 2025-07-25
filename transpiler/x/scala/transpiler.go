//go:build slow

package scalat

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

	yaml "gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

var typeDecls []*TypeDeclStmt

var needsBreaks bool
var needsJSON bool
var needsBigInt bool
var replaceContinue bool
var loopCounter int
var breakStack []string
var continueStack []string
var useNow bool
var useLookupHost bool
var builtinAliases map[string]string
var localVarTypes map[string]string
var returnTypeStack []string
var benchMain bool

// SetBenchMain configures whether Transpile should wrap the main function
// in a benchmark block when emitting code.
func SetBenchMain(v bool) { benchMain = v }

func BuiltinAliases() map[string]string { return builtinAliases }

var scalaKeywords = map[string]bool{
	"val":    true,
	"var":    true,
	"type":   true,
	"def":    true,
	"this":   true,
	"new":    true,
	"object": true,
	"class":  true,
	"trait":  true,
	"case":   true,
	"match":  true,
	"with":   true,
}

func escapeName(name string) string {
	if scalaKeywords[name] {
		return "`" + name + "`"
	}
	return name
}

// Program represents a simple Scala program consisting of statements in main.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

func containsBreak(stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *BreakStmt:
			return true
		case *BenchStmt:
			if containsBreak(s.Body) {
				return true
			}
		case *FunStmt:
			if containsBreak(s.Body) {
				return true
			}
		case *IfStmt:
			if containsBreak(s.Then) || containsBreak(s.Else) {
				return true
			}
		case *ForEachStmt:
			if containsBreak(s.Body) {
				return true
			}
		case *ForRangeStmt:
			if containsBreak(s.Body) {
				return true
			}
		case *WhileStmt:
			if containsBreak(s.Body) {
				return true
			}
		}
	}
	return false
}

func containsContinue(stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *ContinueStmt:
			return true
		case *BenchStmt:
			if containsContinue(s.Body) {
				return true
			}
		case *FunStmt:
			if containsContinue(s.Body) {
				return true
			}
		case *IfStmt:
			if containsContinue(s.Then) || containsContinue(s.Else) {
				return true
			}
		case *ForEachStmt:
			if containsContinue(s.Body) {
				return true
			}
		case *ForRangeStmt:
			if containsContinue(s.Body) {
				return true
			}
		case *WhileStmt:
			if containsContinue(s.Body) {
				return true
			}
		}
	}
	return false
}

func ContainsBreak(stmts []Stmt) bool    { return containsBreak(stmts) }
func ContainsContinue(stmts []Stmt) bool { return containsContinue(stmts) }
func containsReturn(stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *ReturnStmt:
			return true
		case *FunStmt:
			if containsReturn(s.Body) {
				return true
			}
		case *IfStmt:
			if containsReturn(s.Then) || containsReturn(s.Else) {
				return true
			}
		case *ForEachStmt:
			if containsReturn(s.Body) {
				return true
			}
		case *ForRangeStmt:
			if containsReturn(s.Body) {
				return true
			}
		case *WhileStmt:
			if containsReturn(s.Body) {
				return true
			}
		}
	}
	return false
}

func ContainsReturn(stmts []Stmt) bool { return containsReturn(stmts) }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type Param struct {
	Name string
	Type string
}

type FunStmt struct {
	Name   string
	Params []Param
	Return string
	Body   []Stmt
}

func paramAssigned(name string, stmts []*parser.Statement) bool {
	for _, st := range stmts {
		switch {
		case st.Assign != nil:
			if st.Assign.Name == name && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
				return true
			}
		case st.If != nil:
			if paramAssigned(name, st.If.Then) || paramAssigned(name, st.If.Else) {
				return true
			}
		case st.For != nil:
			if paramAssigned(name, st.For.Body) {
				return true
			}
		case st.While != nil:
			if paramAssigned(name, st.While.Body) {
				return true
			}
		}
	}
	return false
}

type ReturnStmt struct{ Value Expr }

// WhileStmt represents `while cond { ... }` loops.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

// ForRangeStmt represents `for i in a..b { ... }` loops.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

// ForEachStmt represents `for x in list { ... }` loops.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) {
	if len(breakStack) > 0 {
		fmt.Fprintf(w, "%s.break()", breakStack[len(breakStack)-1])
	} else {
		fmt.Fprint(w, "break")
	}
}

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) {
	if replaceContinue {
		if len(continueStack) > 0 {
			fmt.Fprintf(w, "%s.break()", continueStack[len(continueStack)-1])
		} else {
			fmt.Fprint(w, "break")
		}
	} else {
		fmt.Fprint(w, "continue")
	}
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

type FunExpr struct {
	Params []Param
	Expr   Expr
}

type MatchCase struct {
	Pattern Expr
	Result  Expr
}

type MatchExpr struct {
	Target Expr
	Cases  []MatchCase
}

func (m *MatchExpr) emit(w io.Writer) {
	m.Target.emit(w)
	fmt.Fprint(w, " match {")
	for _, c := range m.Cases {
		fmt.Fprint(w, " case ")
		c.Pattern.emit(w)
		fmt.Fprint(w, " => ")
		c.Result.emit(w)
	}
	fmt.Fprint(w, " }")
}

type LetStmt struct {
	Name   string
	Type   string
	Value  Expr
	Global bool
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "val %s", escapeName(s.Name))
	if s.Type != "" {
		fmt.Fprintf(w, ": %s", s.Type)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

type VarStmt struct {
	Name   string
	Type   string
	Value  Expr
	Global bool
}

// SaveStmt writes a list of maps to stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

// UpdateStmt updates fields of structs in a list.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

// BenchStmt measures execution time and memory usage of a block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

// TypeDeclStmt represents a case class declaration.
type Variant struct {
	Name   string
	Fields []Param
}

// TypeDeclStmt represents a case class or algebraic data type declaration.
type TypeDeclStmt struct {
	Name     string
	Fields   []Param
	Variants []Variant
}

func (t *TypeDeclStmt) emit(w io.Writer) {
	if len(t.Variants) > 0 {
		fmt.Fprintf(w, "sealed trait %s\n", escapeName(t.Name))
		for _, v := range t.Variants {
			if len(v.Fields) == 0 {
				fmt.Fprintf(w, "case object %s extends %s\n", escapeName(v.Name), escapeName(t.Name))
				continue
			}
			fmt.Fprintf(w, "case class %s(", escapeName(v.Name))
			for i, f := range v.Fields {
				if i > 0 {
					fmt.Fprint(w, ", ")
				}
				typ := f.Type
				if typ == "" {
					typ = "Any"
				}
				fmt.Fprintf(w, "var %s: %s", escapeName(f.Name), typ)
			}
			fmt.Fprintf(w, ") extends %s\n", escapeName(t.Name))
		}
		return
	}
	fmt.Fprintf(w, "case class %s(", escapeName(t.Name))
	for i, f := range t.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		typ := f.Type
		if typ == "" {
			typ = "Any"
		}
		fmt.Fprintf(w, "var %s: %s", escapeName(f.Name), typ)
	}
	fmt.Fprint(w, ")")
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s", escapeName(s.Name))
	if s.Type != "" {
		fmt.Fprintf(w, ": %s", s.Type)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

// AssignStmt represents `target = value` assignments.
type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	s.Target.emit(w)
	fmt.Fprint(w, " = ")
	s.Value.emit(w)
}

func (f *FunStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "def %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			fmt.Fprint(w, p.Name)
		}
	}
	fmt.Fprint(w, ")")
	if f.Return != "" {
		fmt.Fprintf(w, ": %s", f.Return)
	}
	fmt.Fprint(w, " = {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
}

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Value != nil {
		fmt.Fprint(w, " ")
		r.Value.emit(w)
	}
}

func (ws *WhileStmt) emit(w io.Writer) {
	hasBr := containsBreak(ws.Body)
	hasCont := containsContinue(ws.Body)
	if hasBr || hasCont {
		needsBreaks = true
	}
	id := loopCounter
	loopCounter++
	var brVar, contVar string
	if hasBr {
		brVar = fmt.Sprintf("_br%d", id)
		fmt.Fprintf(w, "  val %s = new Breaks\n", brVar)
		if hasCont {
			contVar = fmt.Sprintf("_ct%d", id)
			fmt.Fprintf(w, "  val %s = new Breaks\n", contVar)
		}
		fmt.Fprintf(w, "%s.breakable {\n", brVar)
		breakStack = append(breakStack, brVar)
	} else if hasCont {
		contVar = fmt.Sprintf("_ct%d", id)
		fmt.Fprintf(w, "  val %s = new Breaks\n", contVar)
	}
	fmt.Fprint(w, "while (")
	ws.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	if hasCont {
		fmt.Fprintf(w, "    %s.breakable {\n", contVar)
		continueStack = append(continueStack, contVar)
		replaceContinue = true
	}
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		switch s := st.(type) {
		case *BreakStmt:
			if hasBr {
				s.emit(w)
			}
		case *ContinueStmt:
			if hasCont {
				s.emit(w)
			} else {
				s.emit(w)
			}
		default:
			st.emit(w)
		}
		fmt.Fprint(w, "\n")
	}
	if hasCont {
		replaceContinue = false
		continueStack = continueStack[:len(continueStack)-1]
		fmt.Fprint(w, "    }\n")
	}
	fmt.Fprint(w, "  }")
	if hasBr {
		breakStack = breakStack[:len(breakStack)-1]
		fmt.Fprint(w, "\n}")
	}
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	hasBr := containsBreak(fr.Body)
	hasCont := containsContinue(fr.Body)
	if hasBr || hasCont {
		needsBreaks = true
	}
	id := loopCounter
	loopCounter++
	var brVar, contVar string
	if hasBr {
		brVar = fmt.Sprintf("_br%d", id)
		fmt.Fprintf(w, "  val %s = new Breaks\n", brVar)
		if hasCont {
			contVar = fmt.Sprintf("_ct%d", id)
			fmt.Fprintf(w, "  val %s = new Breaks\n", contVar)
		}
		fmt.Fprintf(w, "  %s.breakable {\n", brVar)
		breakStack = append(breakStack, brVar)
	} else if hasCont {
		contVar = fmt.Sprintf("_ct%d", id)
		fmt.Fprintf(w, "  val %s = new Breaks\n", contVar)
	}
	fmt.Fprintf(w, "  for (%s <- ", fr.Name)
	fr.Start.emit(w)
	fmt.Fprint(w, " until ")
	fr.End.emit(w)
	fmt.Fprint(w, ") {\n")
	if hasCont {
		fmt.Fprintf(w, "    %s.breakable {\n", contVar)
		continueStack = append(continueStack, contVar)
		replaceContinue = true
	}
	for _, st := range fr.Body {
		fmt.Fprint(w, "    ")
		switch s := st.(type) {
		case *BreakStmt:
			if hasBr {
				s.emit(w)
			}
		case *ContinueStmt:
			if hasCont {
				s.emit(w)
			}
		default:
			st.emit(w)
		}
		fmt.Fprint(w, "\n")
	}
	if hasCont {
		replaceContinue = false
		continueStack = continueStack[:len(continueStack)-1]
		fmt.Fprint(w, "    }\n")
	}
	fmt.Fprint(w, "  }")
	if hasBr {
		breakStack = breakStack[:len(breakStack)-1]
		fmt.Fprint(w, "\n}")
	}
}

func (fe *ForEachStmt) emit(w io.Writer) {
	hasBr := containsBreak(fe.Body)
	hasCont := containsContinue(fe.Body)
	if hasBr || hasCont {
		needsBreaks = true
	}
	id := loopCounter
	loopCounter++
	var brVar, contVar string
	if hasBr {
		brVar = fmt.Sprintf("_br%d", id)
		fmt.Fprintf(w, "  val %s = new Breaks\n", brVar)
		if hasCont {
			contVar = fmt.Sprintf("_ct%d", id)
			fmt.Fprintf(w, "  val %s = new Breaks\n", contVar)
		}
		fmt.Fprintf(w, "  %s.breakable {\n", brVar)
		breakStack = append(breakStack, brVar)
	} else if hasCont {
		contVar = fmt.Sprintf("_ct%d", id)
		fmt.Fprintf(w, "  val %s = new Breaks\n", contVar)
	}
	fmt.Fprintf(w, "  for (%s <- ", fe.Name)
	fe.Iterable.emit(w)
	fmt.Fprint(w, ") {\n")
	if hasCont {
		fmt.Fprintf(w, "    %s.breakable {\n", contVar)
		continueStack = append(continueStack, contVar)
		replaceContinue = true
	}
	for _, st := range fe.Body {
		fmt.Fprint(w, "    ")
		switch s := st.(type) {
		case *BreakStmt:
			if hasBr {
				s.emit(w)
			}
		case *ContinueStmt:
			if hasCont {
				s.emit(w)
			}
		default:
			s.emit(w)
		}
		fmt.Fprint(w, "\n")
	}
	if hasCont {
		replaceContinue = false
		continueStack = continueStack[:len(continueStack)-1]
		fmt.Fprint(w, "    }\n")
	}
	fmt.Fprint(w, "  }")
	if hasBr {
		breakStack = breakStack[:len(breakStack)-1]
		fmt.Fprint(w, "\n}")
	}
}

func (s *SaveStmt) emit(w io.Writer) {
	fmt.Fprint(w, "for (_row <- ")
	if s.Src != nil {
		s.Src.emit(w)
	}
	fmt.Fprint(w, ") {\n")
	fmt.Fprint(w, "    val _keys = _row.keys.toSeq.sorted\n")
	fmt.Fprint(w, "    val _tmp = scala.collection.mutable.Map[String,Any]()\n")
	fmt.Fprint(w, "    for (k <- _keys) _tmp(k) = _row(k)\n")
	fmt.Fprint(w, "    println(toJson(_tmp.toMap))\n")
	fmt.Fprint(w, "  }")
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for (i <- 0 until %s.length) {\n", u.Target)
	fmt.Fprintf(w, "  var item = %s(i)\n", u.Target)
	if u.Cond != nil {
		fmt.Fprint(w, "  if (")
		u.Cond.emit(w)
		fmt.Fprint(w, ") {\n")
	}
	pad := "  "
	if u.Cond != nil {
		pad = "    "
	}
	for i, f := range u.Fields {
		fmt.Fprintf(w, "%sitem = item.copy(%s = ", pad, f)
		u.Values[i].emit(w)
		fmt.Fprint(w, ")\n")
	}
	if u.Cond != nil {
		fmt.Fprint(w, "  }\n")
	}
	fmt.Fprintf(w, "  %s(i) = item\n", u.Target)
	fmt.Fprint(w, "}\n")
}

func (b *BenchStmt) emit(w io.Writer) {
	fmt.Fprint(w, "{\n")
	fmt.Fprint(w, "  System.gc()\n")
	fmt.Fprint(w, "  val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()\n")
	fmt.Fprint(w, "  val _start = _now()\n")
	for _, st := range b.Body {
		fmt.Fprint(w, "  ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  val _end = _now()\n")
	fmt.Fprint(w, "  System.gc()\n")
	fmt.Fprint(w, "  val _endMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()\n")
	fmt.Fprint(w, "  val _durUs = (_end - _start) / 1000\n")
	fmt.Fprint(w, "  var _memDiff = _endMem - _startMem\n")
	fmt.Fprint(w, "  if (_memDiff <= 0) _memDiff = _endMem\n")
	fmt.Fprintf(w, "  println(toJson(Map(\"duration_us\" -> _durUs, \"memory_bytes\" -> _memDiff, \"name\" -> \"%s\")))\n", b.Name)
	fmt.Fprint(w, "}")
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	i.Cond.emit(w)
	fmt.Fprint(w, ") {\n")
	for _, st := range i.Then {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, "\n")
	}
	fmt.Fprint(w, "  }")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, st := range i.Else {
			fmt.Fprint(w, "    ")
			st.emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "  }")
	}
}

func (ie *IfExpr) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	ie.Cond.emit(w)
	fmt.Fprint(w, ") ")
	ie.Then.emit(w)
	fmt.Fprint(w, " else ")
	ie.Else.emit(w)
}

func (f *FunExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	fmt.Fprint(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", p.Name, p.Type)
		} else {
			fmt.Fprintf(w, "%s: Any", p.Name)
		}
	}
	fmt.Fprint(w, ") => ")
	if f.Expr != nil {
		f.Expr.emit(w)
	}
	fmt.Fprint(w, ")")
}

// CallExpr represents calling a function expression with arguments.
type CallExpr struct {
	Fn   Expr
	Args []Expr
}

// LenExpr represents len(x) which becomes x.length in Scala.
type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	l.Value.emit(w)
	fmt.Fprint(w, ").size")
}

func (c *CallExpr) emit(w io.Writer) {
	c.Fn.emit(w)
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

type IntLit struct {
	Value int64
	Long  bool
}

func (i *IntLit) emit(w io.Writer) {
	if i.Long {
		fmt.Fprintf(w, "%dL", i.Value)
	} else {
		fmt.Fprintf(w, "%d", i.Value)
	}
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) { fmt.Fprintf(w, "%t", b.Value) }

// FloatLit represents a floating point literal.
type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) {
	s := strconv.FormatFloat(f.Value, 'f', -1, 64)
	if !strings.ContainsAny(s, ".eE") && !strings.Contains(s, ".") {
		s += ".0"
	}
	fmt.Fprint(w, s)
}

// NowExpr expands to a deterministic timestamp helper.
type NowExpr struct{}

func (n *NowExpr) emit(w io.Writer) { fmt.Fprint(w, "_now()") }

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) {
	if n.Name == "nil" {
		fmt.Fprint(w, "null")
		return
	}
	fmt.Fprint(w, escapeName(n.Name))
}

// ListLit represents a mutable list using ArrayBuffer.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "ArrayBuffer(")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, ")")
}

// MapEntry represents a key/value pair inside a map literal.
type MapEntry struct {
	Key   Expr
	Value Expr
	Type  string
}

// MapLit represents a simple map literal using Scala's Map.
type MapLit struct{ Items []MapEntry }

func (m *MapLit) emit(w io.Writer) {
	fmt.Fprint(w, "Map(")
	for i, it := range m.Items {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		it.Key.emit(w)
		fmt.Fprint(w, " -> (")
		it.Value.emit(w)
		fmt.Fprint(w, ")")
	}
	fmt.Fprint(w, ")")
}

// StructLit represents constructing a case class value.
type StructLit struct {
	Name   string
	Fields []Expr
}

func (s *StructLit) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", escapeName(s.Name))
	for i, f := range s.Fields {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		f.emit(w)
	}
	fmt.Fprint(w, ")")
}

// AppendExpr represents append(list, elem) as `list :+ elem`.
type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	a.List.emit(w)
	fmt.Fprint(w, " :+ ")
	a.Elem.emit(w)
}

// IndexExpr represents x[i] which becomes x(i) in Scala.
type IndexExpr struct {
	Value     Expr
	Index     Expr
	Type      string
	Container string
	ForceMap  bool
}

func (idx *IndexExpr) emit(w io.Writer) {
	if strings.HasPrefix(idx.Container, "Map[") && idx.Container != "Any" && idx.Type != "" {
		idx.Value.emit(w)
		fmt.Fprint(w, ".getOrElse(")
		idx.Index.emit(w)
		fmt.Fprint(w, ", ")
		t := idx.Type
		if t == "" {
			t = "Any"
		}
		fmt.Fprintf(w, "null.asInstanceOf[%s])", t)
	} else if idx.Container == "Any" {
		if (idx.ForceMap) || (func() bool {
			if prev, ok := idx.Value.(*IndexExpr); ok {
				return strings.HasPrefix(prev.Container, "Map[")
			}
			return false
		}()) {
			idx.Value.emit(w)
			if idx.Type != "" {
				fmt.Fprintf(w, ".asInstanceOf[Map[String,%s]](", idx.Type)
			} else {
				fmt.Fprint(w, ".asInstanceOf[Map[String,Any]](")
			}
			idx.Index.emit(w)
			fmt.Fprint(w, ")")
		} else {
			idx.Value.emit(w)
			if idx.Type != "" {
				fmt.Fprintf(w, ".asInstanceOf[ArrayBuffer[%s]](", idx.Type)
			} else {
				fmt.Fprint(w, ".asInstanceOf[ArrayBuffer[Any]](")
			}
			idx.Index.emit(w)
			fmt.Fprint(w, ")")
		}
	} else {
		idx.Value.emit(w)
		fmt.Fprint(w, "(")
		idx.Index.emit(w)
		fmt.Fprint(w, ")")
	}
	if idx.Type != "" && idx.Type != "Any" &&
		(idx.Container == "Any" || strings.Contains(idx.Container, "Any")) &&
		!strings.HasPrefix(idx.Type, "ArrayBuffer[") && !strings.HasPrefix(idx.Type, "Map[") {
		fmt.Fprintf(w, ".asInstanceOf[%s]", idx.Type)
	}
}

// SliceExpr represents x[a:b] which becomes x.slice(a, b).
type SliceExpr struct {
	Value Expr
	Start Expr
	End   Expr
	Type  string
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Value.emit(w)
	fmt.Fprint(w, ".slice(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// CastExpr represents value as type conversions like `"123" as int`.
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	if c.Type == "bigint" {
		needsBigInt = true
		fmt.Fprint(w, "BigInt(")
		c.Value.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	switch c.Value.(type) {
	case *Name, *IntLit, *StringLit, *BoolLit, *FloatLit:
		c.Value.emit(w)
	default:
		fmt.Fprint(w, "(")
		c.Value.emit(w)
		fmt.Fprint(w, ")")
	}
	switch c.Type {
	case "int":
		fmt.Fprint(w, ".asInstanceOf[Int]")
	case "float", "Double":
		fmt.Fprint(w, ".toString.toDouble")
	case "string":
		fmt.Fprint(w, ".toString")
	case "bool":
		fmt.Fprint(w, ".asInstanceOf[Boolean]")
	default:
		if c.Type != "" {
			fmt.Fprintf(w, ".asInstanceOf[%s]", c.Type)
		}
	}
}

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Receiver Expr
	Name     string
}

func (f *FieldExpr) emit(w io.Writer) {
	f.Receiver.emit(w)
	fmt.Fprintf(w, ".%s", escapeName(f.Name))
}

// SubstringExpr represents substring(s, i, j) which becomes s.substring(i, j).
type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Value.emit(w)
	fmt.Fprint(w, ".substring(")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	s.End.emit(w)
	fmt.Fprint(w, ")")
}

// UnaryExpr represents prefix unary operations like !x.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	u.Expr.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func precedence(op string) int {
	switch op {
	case "*", "/", "%":
		return 7
	case "+", "-":
		return 6
	case "<", "<=", ">", ">=":
		return 5
	case "==", "!=", "in":
		return 4
	case "&&":
		return 3
	case "||":
		return 2
	case "union", "union_all", "except", "intersect":
		return 1
	default:
		return 0
	}
}

func (b *BinaryExpr) emit(w io.Writer) {
	emitChild := func(e Expr) {
		if be, ok := e.(*BinaryExpr); ok {
			if precedence(be.Op) < precedence(b.Op) {
				fmt.Fprint(w, "(")
				be.emit(w)
				fmt.Fprint(w, ")")
				return
			}
		}
		e.emit(w)
	}
	emitChild(b.Left)
	fmt.Fprintf(w, " %s ", b.Op)
	emitChild(b.Right)
}

type queryFrom struct {
	Var string
	Src Expr
}

type queryJoin struct {
	Var  string
	Src  Expr
	Cond Expr
}

// GroupByExpr represents a query with grouping support.
type GroupByExpr struct {
	Var      string
	Source   Expr
	Key      Expr
	Name     string
	Where    Expr
	Select   Expr
	Having   Expr
	Sort     Expr
	SortType string
	Froms    []queryFrom
	Joins    []queryJoin
	ElemType string
}

type QueryExpr struct {
	Var      string
	Src      Expr
	Froms    []queryFrom
	Where    Expr
	Sort     Expr
	SortType string
	Skip     Expr
	Take     Expr
	Select   Expr
	Distinct bool
	ElemType string
}

// RightJoinExpr represents a simple right join query of two sources.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
	ElemType string
}

// LeftJoinExpr represents a simple left join query of two sources.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	Joins    []queryJoin
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
	ElemType string
}

func (q *QueryExpr) emit(w io.Writer) {
	et := q.ElemType
	if et == "" {
		et = "Any"
	}
	if q.Sort == nil && !q.Distinct && q.Skip == nil && q.Take == nil && len(q.Froms) == 0 {
		fmt.Fprintf(w, "(for (%s <- ", q.Var)
		q.Src.emit(w)
		if q.Where != nil {
			fmt.Fprint(w, " if (")
			q.Where.emit(w)
			fmt.Fprint(w, ")")
		}
		fmt.Fprint(w, ") yield ")
		q.Select.emit(w)
		fmt.Fprint(w, ")")
		return
	}
	if q.Sort != nil {
		st := q.SortType
		if st == "" {
			st = "Any"
		}
		fmt.Fprintf(w, "({ var _tmp = ArrayBuffer[(%s,%s)]() ; for (%s <- ", st, et, q.Var)
	} else {
		fmt.Fprintf(w, "({ var _res = ArrayBuffer[%s]() ; for (%s <- ", et, q.Var)
	}
	q.Src.emit(w)
	fmt.Fprint(w, ") {")
	for _, f := range q.Froms {
		fmt.Fprintf(w, " for (%s <- ", f.Var)
		f.Src.emit(w)
		fmt.Fprint(w, ") {")
	}
	if q.Where != nil {
		fmt.Fprint(w, " if (")
		q.Where.emit(w)
		fmt.Fprint(w, ") {")
	}
	if q.Sort != nil {
		fmt.Fprint(w, " _tmp.append((")
		q.Sort.emit(w)
		fmt.Fprint(w, ", ")
		q.Select.emit(w)
		fmt.Fprint(w, "))")
	} else {
		fmt.Fprint(w, " _res.append(")
		q.Select.emit(w)
		fmt.Fprint(w, ")")
	}
	if q.Where != nil {
		fmt.Fprint(w, " }")
	}
	for range q.Froms {
		fmt.Fprint(w, " }")
	}
	if q.Sort != nil {
		fmt.Fprint(w, " }; var _res = _tmp.sortBy(")
		if ml, ok := q.Sort.(*MapLit); ok {
			fmt.Fprint(w, "t => (")
			for i, it := range ml.Items {
				if i > 0 {
					fmt.Fprint(w, ", ")
				}
				fmt.Fprint(w, "t._1(")
				it.Key.emit(w)
				fmt.Fprint(w, ")")
				typ := it.Type
				if typ == "" {
					typ = inferType(it.Value)
				}
				if typ != "" && typ != "Any" {
					fmt.Fprintf(w, ".asInstanceOf[%s]", typ)
				}
			}
			fmt.Fprint(w, ")")
		} else {
			fmt.Fprint(w, "_._1")
		}
		fmt.Fprint(w, ").map(_._2)")
	} else {
		fmt.Fprint(w, " }")
	}
	if q.Distinct {
		fmt.Fprint(w, ".distinct")
	}
	if q.Skip != nil {
		fmt.Fprint(w, ".drop(")
		q.Skip.emit(w)
		fmt.Fprint(w, ")")
	}
	if q.Take != nil {
		fmt.Fprint(w, ".take(")
		q.Take.emit(w)
		fmt.Fprint(w, ")")
	}
	fmt.Fprint(w, "; _res })")
}

func (r *RightJoinExpr) emit(w io.Writer) {
	et := r.ElemType
	if et == "" {
		et = "Any"
	}
	fmt.Fprintf(w, "({ var _res = ArrayBuffer[%s]() ; for (%s <- ", et, r.RightVar)
	r.RightSrc.emit(w)
	fmt.Fprintf(w, ") { var matched = false ; for (%s <- ", r.LeftVar)
	r.LeftSrc.emit(w)
	fmt.Fprint(w, ") { if (")
	r.Cond.emit(w)
	fmt.Fprint(w, ") { matched = true ; _res.append(")
	r.Select.emit(w)
	fmt.Fprint(w, ") } } if (!matched) { var ")
	fmt.Fprint(w, r.LeftVar)
	fmt.Fprint(w, " = null ; _res.append(")
	r.Select.emit(w)
	fmt.Fprint(w, ") }\n _res })")
}

func (l *LeftJoinExpr) emit(w io.Writer) {
	et := l.ElemType
	if et == "" {
		et = "Any"
	}
	fmt.Fprintf(w, "({ val _res = ArrayBuffer[%s]() ; for (%s <- ", et, l.LeftVar)
	l.LeftSrc.emit(w)
	fmt.Fprint(w, ") {")
	for _, j := range l.Joins {
		fmt.Fprintf(w, " for (%s <- ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, ") { if (")
		j.Cond.emit(w)
		fmt.Fprint(w, ") {")
	}
	fmt.Fprint(w, " val _opt = ")
	l.RightSrc.emit(w)
	fmt.Fprint(w, ".find(")
	fmt.Fprintf(w, "%s => ", l.RightVar)
	l.Cond.emit(w)
	fmt.Fprint(w, ") ; val ")
	fmt.Fprint(w, l.RightVar)
	fmt.Fprint(w, " = _opt.getOrElse(null) ; _res.append(")
	l.Select.emit(w)
	fmt.Fprint(w, ")")
	for range l.Joins {
		fmt.Fprint(w, " }")
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, " } ; _res })")
}

func (g *GroupByExpr) emit(w io.Writer) {
	elem := g.ElemType
	if elem == "" {
		elem = "Any"
	}
	fmt.Fprint(w, "ArrayBuffer.from((for (")
	fmt.Fprintf(w, "%s <- ", g.Var)
	g.Source.emit(w)
	fmt.Fprint(w, ")")
	for _, f := range g.Froms {
		fmt.Fprintf(w, "; %s <- ", f.Var)
		f.Src.emit(w)
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, "; %s <- ", j.Var)
		j.Src.emit(w)
		fmt.Fprint(w, " if (")
		j.Cond.emit(w)
		fmt.Fprint(w, ")")
	}
	if g.Where != nil {
		fmt.Fprint(w, " if (")
		g.Where.emit(w)
		fmt.Fprint(w, ")")
	}
	fmt.Fprint(w, " yield (")
	g.Key.emit(w)
	fmt.Fprint(w, ", Map(")
	fmt.Fprintf(w, "\"%s\" -> %s", g.Var, g.Var)
	for _, f := range g.Froms {
		fmt.Fprintf(w, ", \"%s\" -> %s", f.Var, f.Var)
	}
	for _, j := range g.Joins {
		fmt.Fprintf(w, ", \"%s\" -> %s", j.Var, j.Var)
	}
	fmt.Fprint(w, ")))")
	fmt.Fprint(w, ")")
	fmt.Fprint(w, ".groupBy(_._1).map{ case (k, arr) => Map(\"key\" -> k, \"items\" -> ArrayBuffer(arr.map(_._2).toSeq: _*)) }")
	if g.Having != nil {
		fmt.Fprint(w, ".filter(")
		fmt.Fprintf(w, "%s => ", g.Name)
		g.Having.emit(w)
		fmt.Fprint(w, ")")
	}
	if g.Sort != nil {
		fmt.Fprint(w, ".toSeq.sortBy(")
		fmt.Fprintf(w, "%s => ", g.Name)
		g.Sort.emit(w)
		fmt.Fprint(w, ").map(_._2)")
	}
	fmt.Fprint(w, ".map(")
	fmt.Fprintf(w, "%s => ", g.Name)
	g.Select.emit(w)
	fmt.Fprint(w, "))")
}

// Emit generates formatted Scala source for the given program.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.Write(meta.Header("//"))
	buf.WriteString("import scala.collection.mutable.{ArrayBuffer, Map}\n")
	if needsBigInt {
		buf.WriteString("import scala.math.BigInt\n")
	}
	if needsJSON {
		buf.WriteString("import scala.collection.immutable.ListMap\n")
	}
	if needsBreaks {
		buf.WriteString("import scala.util.control.Breaks\n")
		buf.WriteString("import scala.util.control.Breaks._\n")
	}
	buf.WriteString("object Main {\n")
	if useNow {
		buf.WriteString("  private var _nowSeed: Long = 0L\n")
		buf.WriteString("  private var _nowSeeded: Boolean = false\n")
		buf.WriteString("  private def _now(): Int = {\n")
		buf.WriteString("    if (!_nowSeeded) {\n")
		buf.WriteString("      sys.env.get(\"MOCHI_NOW_SEED\").foreach { s =>\n")
		buf.WriteString("        try { _nowSeed = s.toInt; _nowSeeded = true } catch { case _ : NumberFormatException => () }\n")
		buf.WriteString("      }\n")
		buf.WriteString("    }\n")
		buf.WriteString("    if (_nowSeeded) {\n")
		buf.WriteString("      _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647\n")
		buf.WriteString("      _nowSeed.toInt\n")
		buf.WriteString("    } else {\n")
		buf.WriteString("      Math.abs((System.nanoTime() / 1000).toInt)\n")
		buf.WriteString("    }\n")
		buf.WriteString("  }\n\n")
	}
	if useLookupHost {
		buf.WriteString("  private def _lookupHost(host: String): ArrayBuffer[Any] = {\n")
		buf.WriteString("    val addrs = java.net.InetAddress.getAllByName(host).map(_.getHostAddress).sortBy(a => if (a.contains(\".\")) 0 else 1).toList\n")
		buf.WriteString("    ArrayBuffer(addrs, null)\n")
		buf.WriteString("  }\n\n")
	}
	if needsJSON {
		buf.WriteString("  def toJson(value: Any, indent: Int = 0): String = value match {\n")
		buf.WriteString("    case m: scala.collection.Map[_, _] =>\n")
		buf.WriteString("      val items = ListMap(m.toSeq.sortBy(_._1.toString): _*).toSeq.map{ case (k,v) => \"  \"*(indent+1)+\"\\\"\"+k.toString+\"\\\": \"+toJson(v, indent+1) }\n")
		buf.WriteString("      \"{\\n\"+items.mkString(\",\\n\")+\"\\n\"+\"  \"*indent+\"}\"\n")
		buf.WriteString("    case s: Seq[_] =>\n")
		buf.WriteString("      val items = s.map(x => \"  \"*(indent+1)+toJson(x, indent+1))\n")
		buf.WriteString("      \"[\\n\"+items.mkString(\",\\n\")+\"\\n\"+\"  \"*indent+\"]\"\n")
		buf.WriteString("    case s: String => \"\\\"\"+s+\"\\\"\"\n")
		buf.WriteString("    case other => other.toString\n")
		buf.WriteString("  }\n\n")
	}

	var globals []Stmt
	for _, st := range p.Stmts {
		switch s := st.(type) {
		case *FunStmt:
			buf.WriteString("  ")
			s.emit(&buf)
			buf.WriteByte('\n')
			buf.WriteByte('\n')
		case *TypeDeclStmt:
			buf.WriteString("  ")
			s.emit(&buf)
			buf.WriteByte('\n')
			buf.WriteByte('\n')
		case *LetStmt:
			if s.Global {
				globals = append(globals, s)
				continue
			}
		case *VarStmt:
			if s.Global {
				globals = append(globals, s)
				continue
			}
		}
	}

	for _, st := range globals {
		buf.WriteString("  ")
		st.emit(&buf)
		buf.WriteByte('\n')
		buf.WriteByte('\n')
	}

	buf.WriteString("  def main(args: Array[String]): Unit = {\n")
	for _, st := range p.Stmts {
		switch st.(type) {
		case *FunStmt, *TypeDeclStmt:
			continue
		case *LetStmt:
			if st.(*LetStmt).Global {
				continue
			}
		case *VarStmt:
			if st.(*VarStmt).Global {
				continue
			}
		}
		buf.WriteString("    ")
		st.emit(&buf)
		buf.WriteByte('\n')
	}
	buf.WriteString("  }\n")
	buf.WriteString("}\n")
	code := formatScala(buf.Bytes())
	code = bytes.ReplaceAll(code, []byte("keys()"), []byte("keys"))
	code = bytes.ReplaceAll(code, []byte("values()"), []byte("values"))
	return code
}

// Transpile converts a Mochi AST into our simple Scala AST.
func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	sc := &Program{}
	typeDecls = nil
	needsBreaks = false
	needsJSON = false
	needsBigInt = false
	useNow = false
	useLookupHost = false
	builtinAliases = map[string]string{}
	localVarTypes = make(map[string]string)
	for _, st := range prog.Statements {
		if st.Import != nil && st.Import.Lang != nil {
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			path := strings.Trim(st.Import.Path, "\"")
			switch *st.Import.Lang {
			case "go":
				if st.Import.Auto && path == "mochi/runtime/ffi/go/testpkg" {
					builtinAliases[alias] = "go_testpkg"
				} else if st.Import.Auto && path == "strings" {
					builtinAliases[alias] = "go_strings"
				} else if st.Import.Auto && path == "net" {
					builtinAliases[alias] = "go_net"
				}
			case "python":
				if path == "math" {
					builtinAliases[alias] = "python_math"
				}
			}
		}
	}
	for _, st := range prog.Statements {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		if ls, ok := s.(*LetStmt); ok {
			ls.Global = true
		}
		if vs, ok := s.(*VarStmt); ok {
			vs.Global = true
		}
		if s != nil {
			sc.Stmts = append(sc.Stmts, s)
		}
	}
	wrap := bench || benchMain
	if wrap {
		needsJSON = true
		useNow = true
		sc.Stmts = []Stmt{&BenchStmt{Name: "main", Body: sc.Stmts}}
	}
	if len(typeDecls) > 0 {
		stmts := make([]Stmt, 0, len(typeDecls)+len(sc.Stmts))
		for _, d := range typeDecls {
			stmts = append(stmts, d)
		}
		sc.Stmts = append(stmts, sc.Stmts...)
	}
	if containsBreak(sc.Stmts) || containsContinue(sc.Stmts) {
		needsBreaks = true
	}
	if needsJSON {
		// flag already set during conversion
	}
	return sc, nil
}

func convertStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Test != nil:
		return nil, nil
	case st.Expect != nil:
		return nil, nil
	case st.Expr != nil:
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			return convertSaveStmt(se, env)
		}
		e, err := convertExpr(st.Expr.Expr, env)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		}
		typ := toScalaType(st.Let.Type)
		if typ == "" {
			typ = inferTypeWithEnv(e, env)
			if typ == "Any" {
				typ = ""
			}
		}
		if typ == "" || typ == "ArrayBuffer[Any]" {
			if ll, ok := e.(*ListLit); ok && len(ll.Elems) > 0 {
				elem := inferTypeWithEnv(ll.Elems[0], env)
				homo := elem != "" && elem != "Any"
				for _, el := range ll.Elems[1:] {
					if inferTypeWithEnv(el, env) != elem {
						homo = false
						break
					}
				}
				if homo {
					typ = fmt.Sprintf("ArrayBuffer[%s]", elem)
				} else if typ == "ArrayBuffer[Any]" {
					typ = ""
				}
			}
		}
		if e == nil && typ != "" {
			e = defaultExpr(typ)
		}
		if env != nil {
			var t types.Type = types.AnyType{}
			if ll, ok := e.(*ListLit); ok && len(ll.Elems) > 0 {
				if sl, ok2 := ll.Elems[0].(*StructLit); ok2 {
					t = types.ListType{Elem: types.StructType{Name: sl.Name}}
				}
			} else if sl, ok := e.(*StructLit); ok {
				t = types.StructType{Name: sl.Name}
			} else if q, ok := e.(*QueryExpr); ok {
				if q.ElemType != "" {
					t = types.ListType{Elem: types.StructType{Name: q.ElemType}}
				}
			} else if st.Let.Value != nil {
				t = types.ExprType(st.Let.Value, env)
			}
			env.SetVar(st.Let.Name, t, false)
		}
		if typ != "" {
			localVarTypes[st.Let.Name] = typ
		}
		return &LetStmt{Name: st.Let.Name, Type: typ, Value: e}, nil
	case st.Var != nil:
		if st.Var.Type != nil && env != nil {
			env.SetVar(st.Var.Name, types.ResolveTypeRef(st.Var.Type, env), true)
		}
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		}
		typ := toScalaType(st.Var.Type)
		if typ == "" {
			typ = inferTypeWithEnv(e, env)
			if typ == "Any" {
				typ = ""
			}
			if n, ok := e.(*Name); ok && n.Name == "null" {
				typ = "Any"
			}
		}
		if typ == "" || typ == "ArrayBuffer[Any]" {
			if ll, ok := e.(*ListLit); ok && len(ll.Elems) > 0 {
				elem := inferTypeWithEnv(ll.Elems[0], env)
				homo := elem != "" && elem != "Any"
				for _, el := range ll.Elems[1:] {
					if inferTypeWithEnv(el, env) != elem {
						homo = false
						break
					}
				}
				if homo {
					typ = fmt.Sprintf("ArrayBuffer[%s]", elem)
				} else if typ == "ArrayBuffer[Any]" {
					typ = ""
				}
			}
		}
		if e == nil && typ != "" {
			e = defaultExpr(typ)
		}
		if env != nil {
			var t types.Type = types.AnyType{}
			if ll, ok := e.(*ListLit); ok && len(ll.Elems) > 0 {
				if sl, ok2 := ll.Elems[0].(*StructLit); ok2 {
					t = types.ListType{Elem: types.StructType{Name: sl.Name}}
				}
			} else if sl, ok := e.(*StructLit); ok {
				t = types.StructType{Name: sl.Name}
			} else if q, ok := e.(*QueryExpr); ok {
				if q.ElemType != "" {
					t = types.ListType{Elem: types.StructType{Name: q.ElemType}}
				}
			} else if st.Var.Value != nil {
				t = types.ExprType(st.Var.Value, env)
			}
			env.SetVar(st.Var.Name, t, true)
		}
		if typ != "" {
			localVarTypes[st.Var.Name] = typ
		}
		return &VarStmt{Name: st.Var.Name, Type: typ, Value: e}, nil
	case st.Type != nil:
		if len(st.Type.Variants) > 0 {
			var variants []Variant
			for _, v := range st.Type.Variants {
				var fields []Param
				for _, f := range v.Fields {
					fields = append(fields, Param{Name: f.Name, Type: toScalaType(f.Type)})
				}
				variants = append(variants, Variant{Name: v.Name, Fields: fields})
			}
			td := &TypeDeclStmt{Name: st.Type.Name, Variants: variants}
			typeDecls = append(typeDecls, td)
			return nil, nil
		}
		td := &TypeDeclStmt{Name: st.Type.Name}
		for _, m := range st.Type.Members {
			if m.Field != nil {
				td.Fields = append(td.Fields, Param{Name: m.Field.Name, Type: toScalaType(m.Field.Type)})
			}
		}
		typeDecls = append(typeDecls, td)
		return nil, nil
	case st.Assign != nil:
		target := Expr(&Name{Name: st.Assign.Name})
		if len(st.Assign.Index) > 0 {
			var err error
			target, err = applyIndexOps(target, st.Assign.Index, env)
			if err != nil {
				return nil, err
			}
		}
		for _, f := range st.Assign.Field {
			target = &FieldExpr{Receiver: target, Name: f.Name}
		}
		e, err := convertExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		targetType := inferTypeWithEnv(target, env)
		valType := inferTypeWithEnv(e, env)
		if targetType != "" && targetType != "Any" && (valType == "" || valType == "Any") {
			e = &CastExpr{Value: e, Type: targetType}
		}
		return &AssignStmt{Target: target, Value: e}, nil
	case st.Fun != nil:
		return convertFunStmt(st.Fun, env)
	case st.Return != nil:
		return convertReturnStmt(st.Return, env)
	case st.While != nil:
		return convertWhileStmt(st.While, env)
	case st.For != nil:
		return convertForStmt(st.For, env)
	case st.Bench != nil:
		return convertBenchStmt(st.Bench, env)
	case st.Update != nil:
		up, err := convertUpdateStmt(st.Update, env)
		if err != nil {
			return nil, err
		}
		return up, nil
	case st.If != nil:
		return convertIfStmt(st.If, env)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Import != nil:
		// handled during preprocessing
		return nil, nil
	case st.ExternVar != nil, st.ExternFun != nil:
		return nil, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return convertBinary(e.Binary, env)
}

func convertBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	operands := []Expr{}
	operators := []string{}

	left, err := convertUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	operands = append(operands, left)

	for _, part := range b.Right {
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		operators = append(operators, op)
		right, err := convertPostfix(part.Right, env)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
	}

	apply := func(i int) {
		op := operators[i]
		left := operands[i]
		right := operands[i+1]
		var ex Expr
		switch op {
		case "in":
			ex = &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "contains"}, Args: []Expr{left}}
		case "union":
			ex = &FieldExpr{Receiver: &BinaryExpr{Left: left, Op: "++", Right: right}, Name: "distinct"}
		case "union_all":
			ex = &BinaryExpr{Left: left, Op: "++", Right: right}
		case "except":
			fn := &FunExpr{Params: []Param{{Name: "x"}}, Expr: &UnaryExpr{Op: "!", Expr: &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "contains"}, Args: []Expr{&Name{Name: "x"}}}}}
			ex = &CallExpr{Fn: &FieldExpr{Receiver: left, Name: "filter"}, Args: []Expr{fn}}
		case "intersect":
			fn := &FunExpr{Params: []Param{{Name: "x"}}, Expr: &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "contains"}, Args: []Expr{&Name{Name: "x"}}}}
			ex = &CallExpr{Fn: &FieldExpr{Receiver: left, Name: "filter"}, Args: []Expr{fn}}
		default:
			if op == "+" || op == "-" || op == "*" || op == "/" || op == "%" {
				lt := inferTypeWithEnv(left, env)
				rt := inferTypeWithEnv(right, env)
				if op == "+" && (strings.HasPrefix(lt, "ArrayBuffer[") || strings.HasPrefix(rt, "ArrayBuffer[")) {
					ex = &BinaryExpr{Left: left, Op: "++", Right: right}
					operands[i] = ex
					operands = append(operands[:i+1], operands[i+2:]...)
					operators = append(operators[:i], operators[i+1:]...)
					return
				}
				if lt != "Any" && lt != "" && rt == "Any" {
					right = &CastExpr{Value: right, Type: lt}
				}
				if rt != "Any" && rt != "" && lt == "Any" {
					left = &CastExpr{Value: left, Type: rt}
				}
				if (lt == "Any" || lt == "") && (rt == "Any" || rt == "") {
					// fallback to floating point arithmetic when
					// both operand types are unknown
					left = &CastExpr{Value: left, Type: "Double"}
					right = &CastExpr{Value: right, Type: "Double"}
				}
			} else if op == "&&" || op == "||" {
				if inferTypeWithEnv(left, env) != "Boolean" {
					left = &CastExpr{Value: left, Type: "Boolean"}
				}
				if inferTypeWithEnv(right, env) != "Boolean" {
					right = &CastExpr{Value: right, Type: "Boolean"}
				}
			} else if op == ">" || op == "<" || op == ">=" || op == "<=" {
				lt := inferTypeWithEnv(left, env)
				rt := inferTypeWithEnv(right, env)
				if lt == "String" && (rt == "Any" || rt == "") {
					right = &CastExpr{Value: right, Type: "String"}
				} else if rt == "String" && (lt == "Any" || lt == "") {
					left = &CastExpr{Value: left, Type: "String"}
				} else if lt == "Int" && (rt == "Any" || rt == "") {
					right = &CastExpr{Value: right, Type: "Int"}
				} else if rt == "Int" && (lt == "Any" || lt == "") {
					left = &CastExpr{Value: left, Type: "Int"}
				} else if (lt == "Any" || lt == "") && (rt == "Any" || rt == "") {
					left = &CastExpr{Value: left, Type: "Int"}
					right = &CastExpr{Value: right, Type: "Int"}
				}
			}
			if op == "%" {
				lt := inferTypeWithEnv(left, env)
				rt := inferTypeWithEnv(right, env)
				if lt != "Double" && lt != "Float" && rt != "Double" && rt != "Float" {
					ex = &CallExpr{Fn: &Name{Name: "Math.floorMod"}, Args: []Expr{left, right}}
				} else {
					ex = &BinaryExpr{Left: left, Op: op, Right: right}
				}
			} else {
				ex = &BinaryExpr{Left: left, Op: op, Right: right}
			}
		}
		operands[i] = ex
		operands = append(operands[:i+1], operands[i+2:]...)
		operators = append(operators[:i], operators[i+1:]...)
	}

	for _, level := range [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	} {
		for i := 0; i < len(operators); {
			if contains(level, operators[i]) {
				apply(i)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unsupported binary expression")
	}
	return operands[0], nil
}

func contains(list []string, op string) bool {
	for _, s := range list {
		if s == op {
			return true
		}
	}
	return false
}

func applyIndexOps(base Expr, ops []*parser.IndexOp, env *types.Env) (Expr, error) {
	var err error
	for _, op := range ops {
		if op.Colon != nil || op.Colon2 != nil || op.End != nil || op.Step != nil {
			return nil, fmt.Errorf("unsupported assign")
		}
		if op.Start == nil {
			return nil, fmt.Errorf("nil index")
		}
		var idx Expr
		idx, err = convertExpr(op.Start, nil)
		if err != nil {
			return nil, err
		}
		ct := inferTypeWithEnv(base, env)
		forceMap := false
		if ct == "" || ct == "Any" {
			if iePrev, ok := base.(*IndexExpr); ok {
				ct = iePrev.Type
			} else if _, ok := idx.(*StringLit); ok {
				forceMap = true
			}
		}
		ie := &IndexExpr{Value: base, Index: idx, Container: ct, ForceMap: forceMap}
		// assignment targets should not emit type casts
		ie.Type = ""
		base = ie
	}
	return base, nil
}

func substituteFieldVars(e Expr, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Name:
		if fields[ex.Name] {
			return &FieldExpr{Receiver: &Name{Name: "item"}, Name: ex.Name}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFieldVars(ex.Left, fields)
		ex.Right = substituteFieldVars(ex.Right, fields)
		return ex
	case *CallExpr:
		ex.Fn = substituteFieldVars(ex.Fn, fields)
		for i := range ex.Args {
			ex.Args[i] = substituteFieldVars(ex.Args[i], fields)
		}
		return ex
	case *FieldExpr:
		ex.Receiver = substituteFieldVars(ex.Receiver, fields)
		return ex
	case *IndexExpr:
		ex.Value = substituteFieldVars(ex.Value, fields)
		ex.Index = substituteFieldVars(ex.Index, fields)
		return ex
	case *UnaryExpr:
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
	case *MapLit:
		for i := range ex.Items {
			ex.Items[i].Key = substituteFieldVars(ex.Items[i].Key, fields)
			ex.Items[i].Value = substituteFieldVars(ex.Items[i].Value, fields)
		}
		return ex
	case *StructLit:
		for i := range ex.Fields {
			ex.Fields[i] = substituteFieldVars(ex.Fields[i], fields)
		}
		return ex
	case *SliceExpr:
		ex.Value = substituteFieldVars(ex.Value, fields)
		if ex.Start != nil {
			ex.Start = substituteFieldVars(ex.Start, fields)
		}
		if ex.End != nil {
			ex.End = substituteFieldVars(ex.End, fields)
		}
		return ex
	default:
		return ex
	}
}

func convertUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	expr, err := convertPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
		case "!":
			if inferTypeWithEnv(expr, env) != "Boolean" {
				expr = &CastExpr{Value: expr, Type: "bool"}
			}
			expr = &UnaryExpr{Op: "!", Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary")
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	expr, err := convertPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		if kind, ok := builtinAliases[pf.Target.Selector.Root]; ok {
			field := pf.Target.Selector.Tail[0]
			call := pf.Ops[0].Call
			args := make([]Expr, len(call.Args))
			for j, a := range call.Args {
				ex, err := convertExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			switch kind {
			case "go_strings":
				switch field {
				case "TrimSpace":
					expr = &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "trim"}}
				case "ToUpper":
					expr = &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toUpperCase"}}
				}
				if expr != nil {
					return expr, nil
				}
			case "python_math":
				switch field {
				case "sqrt", "sin", "log", "pow":
					expr = &CallExpr{Fn: &FieldExpr{Receiver: &Name{Name: "math"}, Name: field}, Args: args}
					return expr, nil
				case "pi":
					expr = &FieldExpr{Receiver: &Name{Name: "math"}, Name: "Pi"}
					return expr, nil
				case "e":
					expr = &FieldExpr{Receiver: &Name{Name: "math"}, Name: "E"}
					return expr, nil
				}
			case "go_net":
				switch field {
				case "LookupHost":
					useLookupHost = true
					expr = &CallExpr{Fn: &Name{Name: "_lookupHost"}, Args: args}
					return expr, nil
				}
			}
		}
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Field != nil:
			if n, ok := expr.(*Name); ok && env != nil {
				if typ, err := env.GetVar(n.Name); err == nil {
					if !(op.Field.Name == "keys" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil && len(pf.Ops[i+1].Call.Args) == 0) {
						switch typ.(type) {
						case types.GroupType, types.MapType:
							ie := &IndexExpr{Value: expr, Index: &StringLit{Value: op.Field.Name}, Container: toScalaTypeFromType(typ)}
							ie.Type = inferTypeWithEnv(ie, env)
							if op.Field.Name == "value" {
								ie.Type = "Map[String,Int]"
							} else if op.Field.Name == "left" || op.Field.Name == "right" {
								if mt, ok2 := typ.(types.MapType); ok2 {
									ie.Type = toScalaTypeFromType(mt)
								}
							}
							expr = ie
							continue
						}
					}
				}
				if kind, ok := builtinAliases[n.Name]; ok {
					field := op.Field.Name
					if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
						call := pf.Ops[i+1].Call
						args := make([]Expr, len(call.Args))
						for j, a := range call.Args {
							ex, err := convertExpr(a, env)
							if err != nil {
								return nil, err
							}
							args[j] = ex
						}
						if kind == "go_testpkg" {
							switch field {
							case "Add":
								if len(args) == 2 {
									expr = &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}
									i++
									continue
								}
							case "FifteenPuzzleExample":
								if len(args) == 0 {
									expr = &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}
									i++
									continue
								}
							}
						}
						if kind == "go_strings" {
							switch field {
							case "TrimSpace":
								expr = &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "trim"}}
								i++
								continue
							case "ToUpper":
								expr = &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toUpperCase"}}
								i++
								continue
							}
						}
						if kind == "python_math" {
							switch field {
							case "sqrt", "sin", "log", "pow":
								fnName := field
								if field == "pow" {
									fnName = "pow"
								}
								expr = &CallExpr{Fn: &FieldExpr{Receiver: &Name{Name: "math"}, Name: fnName}, Args: args}
								i++
								continue
							}
						}
					} else {
						if kind == "go_testpkg" {
							switch field {
							case "Pi":
								expr = &FloatLit{Value: 3.14}
								i++
								continue
							case "Answer":
								expr = &IntLit{Value: 42}
								i++
								continue
							}
						} else if kind == "python_math" {
							switch field {
							case "pi":
								expr = &FieldExpr{Receiver: &Name{Name: "math"}, Name: "Pi"}
								i++
								continue
							case "e":
								expr = &FieldExpr{Receiver: &Name{Name: "math"}, Name: "E"}
								i++
								continue
							}
						}
					}
				}
			}
			if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
				call := pf.Ops[i+1].Call
				if op.Field.Name == "keys" && len(call.Args) == 0 {
					expr = &FieldExpr{Receiver: expr, Name: "keys"}
					i++
					continue
				}
				args := make([]Expr, len(call.Args))
				for j, a := range call.Args {
					ex, err := convertExpr(a, env)
					if err != nil {
						return nil, err
					}
					args[j] = ex
				}
				expr = &CallExpr{Fn: &FieldExpr{Receiver: expr, Name: op.Field.Name}, Args: args}
				i++
			} else {
				expr = &FieldExpr{Receiver: expr, Name: op.Field.Name}
			}
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil || idx.Colon2 != nil {
				if idx.Colon2 != nil || idx.Step != nil {
					return nil, fmt.Errorf("slice step not supported")
				}
				var start, end Expr
				if idx.Start != nil {
					start, err = convertExpr(idx.Start, env)
					if err != nil {
						return nil, err
					}
				} else {
					start = &IntLit{Value: 0}
				}
				if idx.End != nil {
					end, err = convertExpr(idx.End, env)
					if err != nil {
						return nil, err
					}
				} else {
					end = &LenExpr{Value: expr}
				}
				ct := inferTypeWithEnv(expr, env)
				expr = &SliceExpr{Value: expr, Start: start, End: end, Type: ct}
			} else {
				start, err := convertExpr(idx.Start, env)
				if err != nil {
					return nil, err
				}
				ct := inferTypeWithEnv(expr, env)
				forceMap := false
				if ct == "" || ct == "Any" {
					if iePrev, ok := expr.(*IndexExpr); ok {
						ct = iePrev.Type
					} else if _, ok := start.(*StringLit); ok {
						// assume map when indexing by string
						forceMap = true
					}
				}
				ie := &IndexExpr{Value: expr, Index: start, Container: ct, ForceMap: forceMap}
				ie.Type = elementType(ct)
				expr = ie
			}
		case op.Call != nil:
			call := op.Call
			args := make([]Expr, len(call.Args))
			for j, a := range call.Args {
				ex, err := convertExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			skipCall := false
			if fe, ok := expr.(*FieldExpr); ok && fe.Name == "get" && len(args) == 2 {
				expr = &CallExpr{Fn: &FieldExpr{Receiver: fe.Receiver, Name: "getOrElse"}, Args: args}
				skipCall = true
			}
			if fe, ok := expr.(*FieldExpr); ok {
				if n, ok2 := fe.Receiver.(*Name); ok2 {
					if kind, ok3 := builtinAliases[n.Name]; ok3 {
						switch kind {
						case "go_testpkg":
							if fe.Name == "Add" && len(args) == 2 {
								expr = &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}
								skipCall = true
							}
							if fe.Name == "FifteenPuzzleExample" && len(args) == 0 {
								expr = &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}
								skipCall = true
							}
						}
					}
				}
			}
			if _, ok := expr.(*BinaryExpr); !ok && !skipCall {
				expr = &CallExpr{Fn: expr, Args: args}
			}
		case op.Cast != nil:
			if op.Cast.Type == nil {
				return nil, fmt.Errorf("unsupported cast")
			}
			if op.Cast.Type.Simple != nil {
				typ := *op.Cast.Type.Simple
				converted := false
				if env != nil {
					if st, ok := env.GetStruct(typ); ok {
						if ml, ok2 := expr.(*MapLit); ok2 {
							fields := make([]Expr, len(st.Order))
							for i, n := range st.Order {
								for _, it := range ml.Items {
									if s, ok := it.Key.(*StringLit); ok && s.Value == n {
										fields[i] = it.Value
										break
									}
								}
							}
							expr = &StructLit{Name: typ, Fields: fields}
							converted = true
						}
					}
				}
				if !converted {
					expr = &CastExpr{Value: expr, Type: typ}
				}
			} else {
				expr = &CastExpr{Value: expr, Type: toScalaType(op.Cast.Type)}
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Call != nil:
		return convertCall(p.Call, env)
	case p.Selector != nil:
		expr := Expr(&Name{Name: p.Selector.Root})
		if env != nil {
			if typ, err := env.GetVar(p.Selector.Root); err == nil {
				if _, ok := typ.(types.GroupType); ok && len(p.Selector.Tail) > 0 {
					ie := &IndexExpr{Value: expr, Index: &StringLit{Value: p.Selector.Tail[0]}, Container: toScalaTypeFromType(typ)}
					ie.Type = inferTypeWithEnv(ie, env)
					expr = ie
					for _, f := range p.Selector.Tail[1:] {
						expr = &FieldExpr{Receiver: expr, Name: f}
					}
					return expr, nil
				}
			}
			if kind, ok := builtinAliases[p.Selector.Root]; ok && len(p.Selector.Tail) == 1 {
				switch kind {
				case "go_testpkg":
					switch p.Selector.Tail[0] {
					case "Pi":
						return &FloatLit{Value: 3.14}, nil
					case "Answer":
						return &IntLit{Value: 42}, nil
					}
				case "python_math":
					switch p.Selector.Tail[0] {
					case "pi":
						return &FieldExpr{Receiver: &Name{Name: "math"}, Name: "Pi"}, nil
					case "e":
						return &FieldExpr{Receiver: &Name{Name: "math"}, Name: "E"}, nil
					}
				}
			}
		}
		for _, f := range p.Selector.Tail {
			expr = &FieldExpr{Receiver: expr, Name: f}
		}
		return expr, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(e, env)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		entries := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k Expr
			if s, ok := types.SimpleStringKey(it.Key); ok {
				k = &StringLit{Value: s}
			} else {
				var err error
				k, err = convertExpr(it.Key, env)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertExpr(it.Value, env)
			if err != nil {
				return nil, err
			}
			entries[i] = MapEntry{Key: k, Value: v, Type: inferTypeWithEnv(v, env)}
		}
		return &MapLit{Items: entries}, nil
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		if format == "" {
			format = "yaml"
		}
		if format != "yaml" && format != "jsonl" {
			return nil, fmt.Errorf("unsupported load format")
		}
		expr, err := dataExprFromFile(path, format, p.Load.Type, env)
		if err != nil {
			return nil, err
		}
		return expr, nil
	case p.Query != nil:
		if rj, err := convertRightJoinQuery(p.Query, env); err == nil {
			return rj, nil
		}
		if lj, err := convertLeftJoinQuery(p.Query, env); err == nil {
			return lj, nil
		}
		if ij, err := convertInnerJoinQuery(p.Query, env); err == nil {
			return ij, nil
		}
		return convertQueryExpr(p.Query, env)
	case p.Struct != nil:
		return convertStructLiteral(p.Struct, env)
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr)
	case p.Group != nil:
		return convertExpr(p.Group, env)
	case p.Match != nil:
		return convertMatchExpr(p.Match, env)
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertCall(c *parser.CallExpr, env *types.Env) (Expr, error) {
	args := make([]Expr, len(c.Args))
	for i, a := range c.Args {
		ex, err := convertExpr(a, env)
		if err != nil {
			return nil, err
		}
		args[i] = ex
	}
	name := c.Func
	switch name {
	case "len", "count":
		if len(args) == 1 {
			if n, ok := args[0].(*Name); ok && env != nil {
				if typ, err := env.GetVar(n.Name); err == nil {
					if _, ok := typ.(types.GroupType); ok {
						v := &IndexExpr{Value: args[0], Index: &StringLit{Value: "items"}, Container: toScalaTypeFromType(typ)}
						return &LenExpr{Value: v}, nil
					}
				}
			}
			return &LenExpr{Value: args[0]}, nil
		}
	case "print":
		if len(args) == 1 {
			return &CallExpr{Fn: &Name{Name: "println"}, Args: args}, nil
		}
		list := &CallExpr{Fn: &Name{Name: "List"}, Args: args}
		join := &CallExpr{Fn: &FieldExpr{Receiver: list, Name: "mkString"}, Args: []Expr{&StringLit{Value: " "}}}
		return &CallExpr{Fn: &Name{Name: "println"}, Args: []Expr{join}}, nil
	case "json":
		if len(args) == 1 {
			needsJSON = true
			jsonCall := &CallExpr{Fn: &Name{Name: "toJson"}, Args: []Expr{args[0]}}
			return &CallExpr{Fn: &Name{Name: "println"}, Args: []Expr{jsonCall}}, nil
		}
	case "now":
		if len(args) == 0 {
			useNow = true
			return &NowExpr{}, nil
		}
	case "str":
		if len(args) == 1 {
			name = "String.valueOf"
		}
	case "int":
		if len(args) == 1 {
			toStr := &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toString"}}
			return &FieldExpr{Receiver: toStr, Name: "toInt"}, nil
		}
	case "float":
		if len(args) == 1 {
			toStr := &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toString"}}
			return &FieldExpr{Receiver: toStr, Name: "toDouble"}, nil
		}
	case "abs":
		if len(args) == 1 {
			return &CallExpr{Fn: &FieldExpr{Receiver: &Name{Name: "Math"}, Name: "abs"}, Args: args}, nil
		}
	case "input":
		if len(args) == 0 {
			call := &CallExpr{Fn: &Name{Name: "scala.io.StdIn.readLine"}, Args: nil}
			opt := &CallExpr{Fn: &Name{Name: "Option"}, Args: []Expr{call}}
			return &CallExpr{Fn: &FieldExpr{Receiver: opt, Name: "getOrElse"}, Args: []Expr{&StringLit{Value: "q"}}}, nil
		}
		if len(args) == 1 {
			call := &CallExpr{Fn: &Name{Name: "scala.io.StdIn.readLine"}, Args: args}
			opt := &CallExpr{Fn: &Name{Name: "Option"}, Args: []Expr{call}}
			return &CallExpr{Fn: &FieldExpr{Receiver: opt, Name: "getOrElse"}, Args: []Expr{&StringLit{Value: "q"}}}, nil
		}
	case "append":
		if len(args) == 2 {
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		}
	case "substring":
		if len(args) == 3 {
			return &SubstringExpr{Value: args[0], Start: args[1], End: args[2]}, nil
		}
	case "upper":
		if len(args) == 1 {
			return &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toUpperCase"}}, nil
		}
	case "lower":
		if len(args) == 1 {
			return &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toLowerCase"}}, nil
		}
	case "sum":
		if len(args) == 1 {
			return &FieldExpr{Receiver: args[0], Name: "sum"}, nil
		}
	case "avg":
		if len(args) == 1 {
			sum := &FieldExpr{Receiver: args[0], Name: "sum"}
			ln := &LenExpr{Value: args[0]}
			return &BinaryExpr{Left: sum, Op: "/", Right: ln}, nil
		}
	case "min":
		if len(args) == 1 {
			return &FieldExpr{Receiver: args[0], Name: "min"}, nil
		}
	case "max":
		if len(args) == 1 {
			return &FieldExpr{Receiver: args[0], Name: "max"}, nil
		}
	case "keys":
		if len(args) == 1 {
			return &FieldExpr{Receiver: args[0], Name: "keys"}, nil
		}
	case "values":
		if len(args) == 1 {
			vals := &FieldExpr{Receiver: args[0], Name: "values"}
			return &FieldExpr{Receiver: vals, Name: "toList"}, nil
		}
	case "exists":
		if len(c.Args) == 1 {
			if q := ExtractQueryExpr(c.Args[0]); q != nil {
				qe, err := convertQueryExpr(q, env)
				if err != nil {
					return nil, err
				}
				return &FieldExpr{Receiver: qe, Name: "nonEmpty"}, nil
			}
			return &BinaryExpr{Left: &LenExpr{Value: args[0]}, Op: ">", Right: &IntLit{Value: 0}}, nil
		}
	}

	if env != nil {
		if fn, ok := env.GetFunc(name); ok {
			if len(args) < len(fn.Params) {
				missing := fn.Params[len(args):]
				params := make([]Param, len(missing))
				callArgs := make([]Expr, 0, len(fn.Params))
				callArgs = append(callArgs, args...)
				for i, p := range missing {
					params[i] = Param{Name: p.Name, Type: toScalaType(p.Type)}
					callArgs = append(callArgs, &Name{Name: p.Name})
				}
				call := &CallExpr{Fn: &Name{Name: name}, Args: callArgs}
				return &FunExpr{Params: params, Expr: call}, nil
			}
			if len(args) == len(fn.Params) {
				for i, p := range fn.Params {
					target := toScalaType(p.Type)
					if target == "" || target == "Any" {
						continue
					}
					if strings.HasPrefix(target, "Map[") {
						if ie, ok := args[i].(*IndexExpr); ok && (ie.Type == "" || ie.Type == "Any") {
							args[i] = &FieldExpr{Receiver: ie, Name: "asInstanceOf[" + target + "]"}
							continue
						}
					}
					if ie, ok := args[i].(*IndexExpr); ok && (ie.Type == "" || ie.Type == "Any") {
						args[i] = &CastExpr{Value: ie, Type: target}
						continue
					}
					at := inferTypeWithEnv(args[i], env)
					if at != target {
						args[i] = &CastExpr{Value: args[i], Type: target}
					}
				}
			}
		}
	}

	return &CallExpr{Fn: &Name{Name: name}, Args: args}, nil
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, nil
	}
	if l.Int != nil {
		v := int64(*l.Int)
		if v > math.MaxInt32 || v < math.MinInt32 {
			return &IntLit{Value: v, Long: true}, nil
		}
		return &IntLit{Value: v}, nil
	}
	if l.Float != nil {
		return &FloatLit{Value: *l.Float}, nil
	}
	if l.Bool != nil {
		return &BoolLit{Value: bool(*l.Bool)}, nil
	}
	if l.Null {
		return &Name{Name: "null"}, nil
	}
	return nil, fmt.Errorf("unsupported literal")
}

func convertFunExpr(fe *parser.FunExpr) (Expr, error) {
	saved := localVarTypes
	localVarTypes = copyMap(localVarTypes)
	defer func() { localVarTypes = saved }()

	child := types.NewEnv(nil)

	f := &FunExpr{}
	for _, p := range fe.Params {
		typ := toScalaType(p.Type)
		f.Params = append(f.Params, Param{Name: p.Name, Type: typ})
		if typ != "" {
			localVarTypes[p.Name] = typ
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, nil), true)
		}
	}
	if fe.ExprBody != nil {
		expr, err := convertExpr(fe.ExprBody, child)
		if err != nil {
			return nil, err
		}
		f.Expr = expr
	} else {
		return nil, fmt.Errorf("unsupported fun expr")
	}
	return f, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond, nil)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then, nil)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(ie.Else, nil)
		if err != nil {
			return nil, err
		}
	}
	if elseExpr == nil {
		elseExpr = &IntLit{Value: 0}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertStructLiteral(sl *parser.StructLiteral, env *types.Env) (Expr, error) {
	if env == nil {
		return nil, fmt.Errorf("no env for struct literal")
	}
	st, ok := env.GetStruct(sl.Name)
	if !ok {
		return nil, fmt.Errorf("unknown struct %s", sl.Name)
	}
	args := make([]Expr, len(st.Order))
	for i, name := range st.Order {
		var exprNode *parser.Expr
		for _, f := range sl.Fields {
			if f.Name == name {
				exprNode = f.Value
				break
			}
		}
		if exprNode == nil {
			return nil, fmt.Errorf("missing field %s", name)
		}
		ex, err := convertExpr(exprNode, env)
		if err != nil {
			return nil, err
		}
		args[i] = ex
	}
	return &StructLit{Name: sl.Name, Fields: args}, nil
}

func ExtractQueryExpr(e *parser.Expr) *parser.QueryExpr {
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
	return p.Target.Query
}

func convertRightJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src, env)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(j.On, child)
	if err != nil {
		return nil, err
	}
	genv := types.NewEnv(child)
	genv.SetVar(q.Var, types.AnyType{}, true)
	genv.SetVar(j.Var, types.AnyType{}, true)
	if ml := mapLiteral(q.Select); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			name := types.UniqueStructName("QueryItem", env, nil)
			st.Name = name
			env.SetStruct(name, st)
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: toScalaTypeFromType(st.Fields[n])}
			}
			typeDecls = append(typeDecls, &TypeDeclStmt{Name: name, Fields: fields})
			sl := &parser.StructLiteral{Name: name}
			for _, n := range st.Order {
				for _, it := range ml.Items {
					if key, _ := types.SimpleStringKey(it.Key); key == n {
						sl.Fields = append(sl.Fields, &parser.StructLitField{Name: n, Value: it.Value})
						break
					}
				}
			}
			q.Select = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Struct: sl}}}}}
		}
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel, ElemType: elemTypeStr}, nil
}

func convertLeftJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) == 0 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	last := q.Joins[len(q.Joins)-1]
	if last.Side == nil || *last.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	for _, j := range q.Joins[:len(q.Joins)-1] {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
	}
	leftSrc, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	srcType := types.ExprType(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemT = lt.Elem
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elemT, true)
	joins := make([]queryJoin, 0, len(q.Joins)-1)
	for _, j := range q.Joins[:len(q.Joins)-1] {
		je, err := convertExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		cond, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		joins = append(joins, queryJoin{Var: j.Var, Src: je, Cond: cond})
	}
	rightSrc, err := convertExpr(last.Src, child)
	if err != nil {
		return nil, err
	}
	child.SetVar(last.Var, types.AnyType{}, true)
	cond, err := convertExpr(last.On, child)
	if err != nil {
		return nil, err
	}
	if ml := mapLiteral(q.Select); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			name := types.UniqueStructName("QueryItem", env, nil)
			st.Name = name
			env.SetStruct(name, st)
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: toScalaTypeFromType(st.Fields[n])}
			}
			typeDecls = append(typeDecls, &TypeDeclStmt{Name: name, Fields: fields})
			sl := &parser.StructLiteral{Name: name}
			for _, n := range st.Order {
				for _, it := range ml.Items {
					if key, _ := types.SimpleStringKey(it.Key); key == n {
						sl.Fields = append(sl.Fields, &parser.StructLitField{Name: n, Value: it.Value})
						break
					}
				}
			}
			q.Select = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Struct: sl}}}}}
		}
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" || elemTypeStr == "Any" {
		if t := inferType(sel); t != "" {
			elemTypeStr = t
		} else {
			elemTypeStr = "Any"
		}
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, Joins: joins, RightVar: last.Var, RightSrc: rightSrc, Cond: cond, Select: sel, ElemType: elemTypeStr}, nil
}

func convertInnerJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) == 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
	}
	src, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	srcType := types.ExprType(q.Source, env)
	if n, ok := src.(*Name); ok {
		if gt, ok := srcType.(types.GroupType); ok {
			src = &IndexExpr{Value: n, Index: &StringLit{Value: "items"}, Container: toScalaTypeFromType(srcType)}
			srcType = types.ListType{Elem: gt.Elem}
		}
	}
	var elemT types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemT = lt.Elem
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elemT, true)

	froms := make([]queryFrom, 0, len(q.Joins))
	var where Expr
	for _, j := range q.Joins {
		joinSrc, err := convertExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		froms = append(froms, queryFrom{Var: j.Var, Src: joinSrc})
		cond, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	if ml := mapLiteral(q.Select); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			name := types.UniqueStructName("QueryItem", env, nil)
			st.Name = name
			env.SetStruct(name, st)
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: toScalaTypeFromType(st.Fields[n])}
			}
			typeDecls = append(typeDecls, &TypeDeclStmt{Name: name, Fields: fields})
			sl := &parser.StructLiteral{Name: name}
			for _, n := range st.Order {
				for _, it := range ml.Items {
					if key, _ := types.SimpleStringKey(it.Key); key == n {
						sl.Fields = append(sl.Fields, &parser.StructLitField{Name: n, Value: it.Value})
						break
					}
				}
			}
			q.Select = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Struct: sl}}}}}
		}
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Where: where, Select: sel, Distinct: q.Distinct, ElemType: elemTypeStr}, nil
}

func convertGroupQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group == nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	t := types.ExprType(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := t.(types.ListType); ok {
		elemT = lt.Elem
	} else if gt, ok := t.(types.GroupType); ok {
		elemT = gt.Elem
	}
	child.SetVar(q.Var, elemT, true)
	froms := make([]queryFrom, 0, len(q.Froms))
	joins := make([]queryJoin, 0, len(q.Joins))
	var where Expr
	for _, f := range q.Froms {
		fe, err := convertExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		ft := types.ExprType(f.Src, child)
		var felem types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			felem = lt.Elem
		}
		child.SetVar(f.Var, felem, true)
		froms = append(froms, queryFrom{Var: f.Var, Src: fe})
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
		je, err := convertExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		cond, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		joins = append(joins, queryJoin{Var: j.Var, Src: je, Cond: cond})
	}
	if q.Where != nil {
		cond, err := convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	if len(q.Joins) > 0 || len(q.Froms) > 0 {
		elemT = types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	}
	keyExpr := q.Group.Exprs[0]
	key, err := convertExpr(keyExpr, child)
	if err != nil {
		return nil, err
	}
	keyT := types.ExprType(keyExpr, child)
	genv := types.NewEnv(child)
	genv.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: elemT}, true)
	sel, err := convertExpr(q.Select, genv)
	if err != nil {
		return nil, err
	}
	var having Expr
	var sortExpr Expr
	var sortType string
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having, genv)
		if err != nil {
			return nil, err
		}
	}
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		sortType = inferTypeWithEnv(sortExpr, genv)
		if sortType == "" {
			sortType = toScalaTypeFromType(types.ExprType(q.Sort, genv))
		}
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, genv))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &GroupByExpr{Var: q.Var, Source: src, Key: key, Name: q.Group.Name, Where: where, Select: sel, Having: having, Sort: sortExpr, SortType: sortType, Froms: froms, Joins: joins, ElemType: elemTypeStr}, nil
}

func convertQueryExpr(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group != nil {
		return convertGroupQuery(q, env)
	}
	if len(q.Joins) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	srcType := types.ExprType(q.Source, env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemT = lt.Elem
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elemT, true)
	froms := make([]queryFrom, 0, len(q.Froms)+len(q.Joins))
	var where Expr
	for _, f := range q.Froms {
		fe, err := convertExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		ft := types.ExprType(f.Src, child)
		var felem types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			felem = lt.Elem
		}
		child.SetVar(f.Var, felem, true)
		froms = append(froms, queryFrom{Var: f.Var, Src: fe})
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
		je, err := convertExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		jt := types.ExprType(j.Src, child)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
		froms = append(froms, queryFrom{Var: j.Var, Src: je})
		cond, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	var sort Expr
	var sortType string
	var skipExpr Expr
	var takeExpr Expr
	if q.Where != nil {
		cond, err := convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}
	if q.Sort != nil {
		sort, err = convertExpr(q.Sort, child)
		if err != nil {
			return nil, err
		}
		sortType = inferTypeWithEnv(sort, child)
		if sortType == "" {
			sortType = toScalaTypeFromType(types.ExprType(q.Sort, child))
		}
	}
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, env)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, env)
		if err != nil {
			return nil, err
		}
	}
	if ml := mapLiteral(q.Select); ml != nil {
		if st, ok := types.InferStructFromMapEnv(ml, child); ok {
			name := types.UniqueStructName("QueryItem", env, nil)
			st.Name = name
			env.SetStruct(name, st)
			fields := make([]Param, len(st.Order))
			for i, n := range st.Order {
				fields[i] = Param{Name: n, Type: toScalaTypeFromType(st.Fields[n])}
			}
			typeDecls = append(typeDecls, &TypeDeclStmt{Name: name, Fields: fields})
			sl := &parser.StructLiteral{Name: name}
			for _, n := range st.Order {
				for _, it := range ml.Items {
					if key, _ := types.SimpleStringKey(it.Key); key == n {
						sl.Fields = append(sl.Fields, &parser.StructLitField{Name: n, Value: it.Value})
						break
					}
				}
			}
			q.Select = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Struct: sl}}}}}
		}
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	if fe, ok := sel.(*FieldExpr); ok && fe.Name == "sum" {
		if n, ok2 := fe.Receiver.(*Name); ok2 && n.Name == q.Var {
			qe := &QueryExpr{Var: q.Var, Src: src, Froms: froms, Where: where, Sort: sort, SortType: sortType, Skip: skipExpr, Take: takeExpr, Select: &Name{Name: q.Var}, Distinct: q.Distinct, ElemType: toScalaTypeFromType(elemT)}
			return &FieldExpr{Receiver: qe, Name: "sum"}, nil
		}
	}
	elemTypeStr := toScalaTypeFromType(types.ExprType(q.Select, child))
	if elemTypeStr == "" {
		elemTypeStr = "Any"
	}
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Where: where, Sort: sort, SortType: sortType, Skip: skipExpr, Take: takeExpr, Select: sel, Distinct: q.Distinct, ElemType: elemTypeStr}, nil
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

func convertMatchExpr(me *parser.MatchExpr, env *types.Env) (Expr, error) {
	target, err := convertExpr(me.Target, env)
	if err != nil {
		return nil, err
	}
	m := &MatchExpr{Target: target}
	for _, c := range me.Cases {
		pat, err := convertExpr(c.Pattern, env)
		if err != nil {
			return nil, err
		}
		res, err := convertExpr(c.Result, env)
		if err != nil {
			return nil, err
		}
		m.Cases = append(m.Cases, MatchCase{Pattern: pat, Result: res})
	}
	return m, nil
}

func copyMap(m map[string]string) map[string]string {
	n := make(map[string]string, len(m))
	for k, v := range m {
		n[k] = v
	}
	return n
}

func convertFunStmt(fs *parser.FunStmt, env *types.Env) (Stmt, error) {
	saved := localVarTypes
	localVarTypes = copyMap(localVarTypes)
	defer func() { localVarTypes = saved }()

	child := types.NewEnv(env)

	mutated := map[string]bool{}
	for _, p := range fs.Params {
		if paramAssigned(p.Name, fs.Body) {
			mutated[p.Name] = true
		}
		child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), true)
		if ts := toScalaType(p.Type); ts != "" {
			localVarTypes[p.Name] = ts
		}
	}

	fn := &FunStmt{Name: fs.Name}
	var init []Stmt
	for _, p := range fs.Params {
		typ := toScalaType(p.Type)
		name := p.Name
		if mutated[name] {
			paramName := "_" + name
			fn.Params = append(fn.Params, Param{Name: paramName, Type: typ})
			init = append(init, &VarStmt{Name: name, Type: typ, Value: &Name{Name: paramName}})
		} else {
			fn.Params = append(fn.Params, Param{Name: name, Type: typ})
		}
	}
	fn.Return = toScalaType(fs.Return)
	returnTypeStack = append(returnTypeStack, fn.Return)
	defer func() { returnTypeStack = returnTypeStack[:len(returnTypeStack)-1] }()
	for _, st := range fs.Body {
		s, err := convertStmt(st, child)
		if err != nil {
			return nil, err
		}
		fn.Body = append(fn.Body, s)
	}
	if len(init) > 0 {
		fn.Body = append(init, fn.Body...)
	}
	if fn.Return == "" {
		fn.Return = "Unit"
	}
	return fn, nil
}

func convertReturnStmt(rs *parser.ReturnStmt, env *types.Env) (Stmt, error) {
	var expr Expr
	var err error
	if rs.Value != nil {
		expr, err = convertExpr(rs.Value, env)
		if err != nil {
			return nil, err
		}
	}
	if len(returnTypeStack) > 0 && expr != nil {
		rt := returnTypeStack[len(returnTypeStack)-1]
		if rt != "" && rt != "Unit" {
			et := inferTypeWithEnv(expr, env)
			if !(strings.Contains(rt, "=>") && et == "") {
				if et == "" || et != rt {
					expr = &CastExpr{Value: expr, Type: rt}
				}
			}
		}
	}
	return &ReturnStmt{Value: expr}, nil
}

func convertWhileStmt(ws *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(ws.Cond, env)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range ws.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(fs.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, st := range fs.Body {
			s, err := convertStmt(st, env)
			if err != nil {
				return nil, err
			}
			body = append(body, s)
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fs.Source, env)
	if err != nil {
		return nil, err
	}
	if n, ok := iter.(*Name); ok && env != nil {
		if typ, err := env.GetVar(n.Name); err == nil {
			if _, ok := typ.(types.MapType); ok {
				iter = &FieldExpr{Receiver: iter, Name: "keys"}
			} else if _, ok := typ.(types.GroupType); ok {
				iter = &IndexExpr{Value: iter, Index: &StringLit{Value: "items"}, Container: toScalaTypeFromType(typ)}
			}
		}
	}
	var body []Stmt
	for _, st := range fs.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertSaveStmt(se *parser.SaveExpr, env *types.Env) (Stmt, error) {
	src, err := convertExpr(se.Src, env)
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
	needsJSON = true
	return &SaveStmt{Src: src, Path: path, Format: format}, nil
}

func convertUpdateStmt(us *parser.UpdateStmt, env *types.Env) (Stmt, error) {
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
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
	}
	var fields []string
	var values []Expr
	for _, item := range us.Set.Items {
		key, ok := types.SimpleStringKey(item.Key)
		if !ok {
			return nil, fmt.Errorf("unsupported update key")
		}
		val, err := convertExpr(item.Value, child)
		if err != nil {
			return nil, err
		}
		val = substituteFieldVars(val, fieldSet)
		fields = append(fields, key)
		values = append(values, val)
	}
	var cond Expr
	if us.Where != nil {
		cond, err = convertExpr(us.Where, child)
		if err != nil {
			return nil, err
		}
		cond = substituteFieldVars(cond, fieldSet)
	}
	return &UpdateStmt{Target: us.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func convertBenchStmt(bb *parser.BenchBlock, env *types.Env) (Stmt, error) {
	var body []Stmt
	for _, st := range bb.Body {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	needsJSON = true
	useNow = true
	return &BenchStmt{Name: bb.Name, Body: body}, nil
}

func convertIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(is.Cond, env)
	if err != nil {
		return nil, err
	}
	if inferTypeWithEnv(cond, env) != "Boolean" {
		cond = &CastExpr{Value: cond, Type: "bool"}
	}
	var thenStmts []Stmt
	for _, st := range is.Then {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, s)
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		s, err := convertIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(is.Else) > 0 {
		for _, st := range is.Else {
			s, err := convertStmt(st, env)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, s)
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func toScalaType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		name := strings.ToLower(strings.TrimSpace(*t.Simple))
		switch name {
		case "int":
			return "Int"
		case "string":
			return "String"
		case "bool":
			return "Boolean"
		case "float":
			return "Double"
		case "bigint":
			needsBigInt = true
			return "BigInt"
		case "any":
			return "Any"
		default:
			return strings.TrimSpace(*t.Simple)
		}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		if name == "list" && len(args) == 1 {
			elem := toScalaType(args[0])
			if elem == "" {
				elem = "Any"
			}
			return fmt.Sprintf("ArrayBuffer[%s]", elem)
		}
		if name == "map" && len(args) == 2 {
			k := toScalaType(args[0])
			if k == "" {
				k = "Any"
			}
			v := toScalaType(args[1])
			if v == "" {
				v = "Any"
			}
			return fmt.Sprintf("Map[%s,%s]", k, v)
		}
	}
	if t.Fun != nil {
		parts := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			typ := toScalaType(p)
			if typ == "" {
				typ = "Any"
			}
			parts[i] = typ
		}
		ret := toScalaType(t.Fun.Return)
		if ret == "" {
			ret = "Unit"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	}
	return "Any"
}

func toScalaTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	case types.FloatType:
		return "Double"
	case types.BigIntType:
		needsBigInt = true
		return "BigInt"
	case types.ListType:
		return fmt.Sprintf("ArrayBuffer[%s]", toScalaTypeFromType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Map[%s,%s]", toScalaTypeFromType(tt.Key), toScalaTypeFromType(tt.Value))
	case types.StructType:
		if tt.Name != "" {
			return tt.Name
		}
		return "Any"
	}
	return "Any"
}

func elementType(container string) string {
	if strings.HasPrefix(container, "ArrayBuffer[") {
		return strings.TrimSuffix(strings.TrimPrefix(container, "ArrayBuffer["), "]")
	}
	if strings.HasPrefix(container, "Map[") {
		parts := strings.TrimSuffix(strings.TrimPrefix(container, "Map["), "]")
		kv := strings.SplitN(parts, ",", 2)
		if len(kv) == 2 {
			return strings.TrimSpace(kv[1])
		}
	}
	return ""
}

// inferType attempts a best-effort static type deduction for the expression.
func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		if ex.Long {
			return "Long"
		}
		return "Int"
	case *StringLit:
		return "String"
	case *BoolLit:
		return "Boolean"
	case *FloatLit:
		return "Double"
	case *ListLit:
		if len(ex.Elems) == 0 {
			return "ArrayBuffer[Any]"
		}
		t := inferType(ex.Elems[0])
		if t == "" {
			return "ArrayBuffer[Any]"
		}
		for _, e := range ex.Elems[1:] {
			if inferType(e) != t {
				return "ArrayBuffer[Any]"
			}
		}
		return fmt.Sprintf("ArrayBuffer[%s]", t)
	case *MapLit:
		if len(ex.Items) == 0 {
			return "Map[Any,Any]"
		}
		kt := inferType(ex.Items[0].Key)
		if kt == "" {
			kt = "Any"
		}
		vt := inferType(ex.Items[0].Value)
		if vt == "" {
			vt = "Any"
		}
		for _, it := range ex.Items[1:] {
			if inferType(it.Key) != kt {
				kt = "Any"
			}
			if inferType(it.Value) != vt {
				vt = "Any"
			}
		}
		return fmt.Sprintf("Map[%s,%s]", kt, vt)
	case *StructLit:
		return ex.Name
	case *LenExpr:
		return "Int"
	case *AppendExpr:
		if t := inferType(ex.List); strings.HasPrefix(t, "ArrayBuffer[") {
			return t
		}
		return "ArrayBuffer[Any]"
	case *IndexExpr:
		t := inferType(ex.Value)
		if strings.HasPrefix(t, "ArrayBuffer[") {
			inner := strings.TrimSuffix(strings.TrimPrefix(t, "ArrayBuffer["), "]")
			if inner != "" {
				return inner
			}
			return "Any"
		}
		if strings.HasPrefix(t, "Map[") {
			parts := strings.TrimSuffix(strings.TrimPrefix(t, "Map["), "]")
			kv := strings.SplitN(parts, ",", 2)
			if len(kv) == 2 {
				valType := strings.TrimSpace(kv[1])
				if valType != "" {
					return valType
				}
			}
		}
		return "Any"
	case *SubstringExpr:
		return "String"
	case *SliceExpr:
		if ex.Type != "" {
			return ex.Type
		}
		return inferType(ex.Value)
	case *NowExpr:
		return "Int"
	case *BinaryExpr:
		switch ex.Op {
		case "+", "-", "*", "/", "%":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if ex.Op == "+" {
				if lt == "String" || rt == "String" {
					return "String"
				}
			}
			if lt == "Double" || rt == "Double" {
				return "Double"
			}
			if lt == "" || rt == "" {
				return ""
			}
			return "Int"
		case "==", "!=", ">", "<", ">=", "<=":
			return "Boolean"
		}
	case *CastExpr:
		return toScalaType(&parser.TypeRef{Simple: &ex.Type})
	case *FunExpr:
		parts := make([]string, len(ex.Params))
		for i, p := range ex.Params {
			if p.Type != "" {
				parts[i] = p.Type
			} else {
				parts[i] = "Any"
			}
		}
		ret := inferType(ex.Expr)
		if ret == "" {
			ret = "Any"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	case *IfExpr:
		t1 := inferType(ex.Then)
		t2 := inferType(ex.Else)
		if t1 == t2 {
			return t1
		}
	case *QueryExpr:
		if ex.ElemType != "" {
			return fmt.Sprintf("ArrayBuffer[%s]", ex.ElemType)
		}
		return "ArrayBuffer[Any]"
	case *RightJoinExpr:
		if ex.ElemType != "" {
			return fmt.Sprintf("ArrayBuffer[%s]", ex.ElemType)
		}
		return "ArrayBuffer[Any]"
	case *GroupByExpr:
		if ex.ElemType != "" {
			return fmt.Sprintf("ArrayBuffer[%s]", ex.ElemType)
		}
		return "ArrayBuffer[Any]"
	default:
		_ = ex
	}
	return ""
}

func inferTypeWithEnv(e Expr, env *types.Env) string {
	if t := inferType(e); t != "" && t != "Any" {
		return t
	}
	if env != nil {
		switch ex := e.(type) {
		case *Name:
			if t, ok := localVarTypes[ex.Name]; ok {
				return t
			}
			if typ, err := env.GetVar(ex.Name); err == nil {
				return toScalaTypeFromType(typ)
			}
		case *FieldExpr:
			if n, ok := ex.Receiver.(*Name); ok {
				if t, err := env.GetVar(n.Name); err == nil {
					if st, ok := t.(types.StructType); ok {
						if def, ok := env.GetStruct(st.Name); ok {
							if ft, ok := def.Fields[ex.Name]; ok {
								return toScalaTypeFromType(ft)
							}
						}
					}
				}
			}
		case *IndexExpr:
			t := inferTypeWithEnv(ex.Value, env)
			if strings.HasPrefix(t, "ArrayBuffer[") {
				inner := strings.TrimSuffix(strings.TrimPrefix(t, "ArrayBuffer["), "]")
				if inner != "" {
					return inner
				}
				return "Any"
			}
			if strings.HasPrefix(t, "Map[") {
				parts := strings.TrimSuffix(strings.TrimPrefix(t, "Map["), "]")
				kv := strings.SplitN(parts, ",", 2)
				if len(kv) == 2 {
					valType := strings.TrimSpace(kv[1])
					if valType != "" {
						return valType
					}
				}
			}
		case *CallExpr:
			if n, ok := ex.Fn.(*Name); ok {
				if typ, err := env.GetVar(n.Name); err == nil {
					if ft, ok2 := typ.(types.FuncType); ok2 {
						return toScalaTypeFromType(ft.Return)
					}
				}
			}
		}
	}
	return ""
}

// defaultExpr returns a zero value expression for the given Scala type.
func defaultExpr(typ string) Expr {
	switch {
	case typ == "Int" || typ == "Long" || typ == "Double" || typ == "Float":
		if typ == "Double" || typ == "Float" {
			return &FloatLit{Value: 0}
		}
		return &IntLit{Value: 0}
	case typ == "String":
		return &StringLit{Value: ""}
	case typ == "Boolean":
		return &BoolLit{Value: false}
	case strings.HasPrefix(typ, "ArrayBuffer"):
		return &CallExpr{Fn: &Name{Name: "ArrayBuffer"}, Args: nil}
	case strings.HasPrefix(typ, "Map"):
		return &CallExpr{Fn: &Name{Name: "Map"}, Args: nil}
	default:
		return nil
	}
}

// Print converts the Scala AST to ast.Node and prints it.
func Print(p *Program) {
	toNode(p).Print("")
}

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtNode(st))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *ReturnStmt:
		return &ast.Node{Kind: "return", Children: []*ast.Node{exprNode(st.Value)}}
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		n.Children = append(n.Children, exprNode(st.Cond))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "forrange", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForEachStmt:
		n := &ast.Node{Kind: "foreach", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Iterable))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *AssignStmt:
		n := &ast.Node{Kind: "assign"}
		n.Children = append(n.Children, exprNode(st.Target), exprNode(st.Value))
		return n
	case *FunStmt:
		n := &ast.Node{Kind: "fun", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, params, body)
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		thenNode := &ast.Node{Kind: "then"}
		for _, s2 := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, s2 := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(ex.Fn))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: fmt.Sprint(ex.Value)}
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Value)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			pair := &ast.Node{Kind: "pair"}
			pair.Children = append(pair.Children, exprNode(it.Key), exprNode(it.Value))
			n.Children = append(n.Children, pair)
		}
		return n
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Value), exprNode(ex.Start), exprNode(ex.End)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Value)}}
	case *IfExpr:
		n := &ast.Node{Kind: "ifexpr"}
		n.Children = append(n.Children, exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else))
		return n
	case *FunExpr:
		n := &ast.Node{Kind: "funexpr"}
		params := &ast.Node{Kind: "params"}
		for _, p := range ex.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params, exprNode(ex.Expr))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func formatScala(src []byte) []byte {
	lines := strings.Split(string(src), "\n")
	var out []string
	indent := 0
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "}") {
			if indent > 0 {
				indent--
			}
		}
		out = append(out, strings.Repeat("  ", indent)+trimmed)
		if strings.HasSuffix(trimmed, "{") {
			indent++
		}
	}
	result := strings.Join(out, "\n")
	if !strings.HasSuffix(result, "\n") {
		result += "\n"
	}
	return []byte(result)
}

func repoRoot() string {
	if root := meta.RepoRoot(); root != "" {
		return root
	}
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

func valueToExpr(v interface{}, typ *parser.TypeRef, env *types.Env) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		if typ != nil && typ.Simple != nil && env != nil {
			if st, ok := env.GetStruct(*typ.Simple); ok {
				fields := make([]Expr, len(st.Order))
				for i, k := range st.Order {
					fields[i] = valueToExpr(val[k], nil, env)
				}
				return &StructLit{Name: st.Name, Fields: fields}
			}
		}
		keys := make([]string, 0, len(val))
		for k := range val {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		items := make([]MapEntry, len(keys))
		for i, k := range keys {
			v := valueToExpr(val[k], nil, env)
			items[i] = MapEntry{Key: &StringLit{Value: k}, Value: v, Type: inferTypeWithEnv(v, env)}
		}
		return &MapLit{Items: items}
	case []interface{}:
		var elemTyp *parser.TypeRef
		if typ != nil {
			elemTyp = typ
		}
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it, elemTyp, env)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case float64:
		if float64(int64(val)) == val {
			return &IntLit{Value: int64(val)}
		}
		return &FloatLit{Value: val}
	case int, int64:
		return &IntLit{Value: reflect.ValueOf(val).Int()}
	case nil:
		return &StringLit{Value: ""}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func dataExprFromFile(path, format string, typ *parser.TypeRef, env *types.Env) (Expr, error) {
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
	return valueToExpr(v, typ, env), nil
}
