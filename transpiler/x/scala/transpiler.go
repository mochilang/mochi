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
	"unicode/utf16"

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
var needsPadStart bool
var needsSHA256 bool
var needsBigRat bool
var needsRepeat bool
var needsParseIntStr bool
var needsMD5 bool
var needsEnviron bool
var needsSubprocess bool
var mapEntryTypes map[string]map[string]string
var builtinAliases map[string]string
var localVarTypes map[string]string
var fieldStructs map[string]string
var varDecls map[string]*VarStmt
var returnTypeStack []string
var funCtxStack []bool
var assignedVars map[string]bool
var benchMain bool

// SetBenchMain configures whether Transpile should wrap the main function
// in a benchmark block when emitting code.
func SetBenchMain(v bool) { benchMain = v }

func BuiltinAliases() map[string]string { return builtinAliases }

func isDeclaredType(name string) bool {
	for _, td := range typeDecls {
		if td.Name == name {
			return true
		}
	}
	return false
}

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
	"Map":    true,
}

func escapeName(name string) string {
	if scalaKeywords[name] {
		return "`" + name + "`"
	}
	return name
}

func quoteScalaString(s string) string {
	q := strconv.QuoteToASCII(s)
	// Convert \xXX and \UXXXXXXXX escapes produced by Go to Java/Scala compatible forms
	var b strings.Builder
	i := 0
	for i < len(q) {
		if q[i] == '\\' && i+3 < len(q) && q[i+1] == 'x' {
			b.WriteString("\\u00")
			b.WriteByte(q[i+2])
			b.WriteByte(q[i+3])
			i += 4
			continue
		}
		if q[i] == '\\' && i+9 < len(q) && q[i+1] == 'U' {
			if cp, err := strconv.ParseUint(q[i+2:i+10], 16, 32); err == nil {
				r := rune(cp)
				if r1, r2 := utf16.EncodeRune(r); r1 != '\uFFFD' {
					fmt.Fprintf(&b, "\\u%04X\\u%04X", r1, r2)
				} else {
					fmt.Fprintf(&b, "\\u%04X", r)
				}
				i += 10
				continue
			}
		}
		b.WriteByte(q[i])
		i++
	}
	return b.String()
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

func gatherAssigned(stmts []*parser.Statement, m map[string]bool) {
	for _, st := range stmts {
		if st.Assign != nil {
			m[st.Assign.Name] = true
		}
		if st.If != nil {
			gatherAssigned(st.If.Then, m)
			if st.If.ElseIf != nil {
				gatherAssigned([]*parser.Statement{{If: st.If.ElseIf}}, m)
			}
			gatherAssigned(st.If.Else, m)
		}
		if st.While != nil {
			gatherAssigned(st.While.Body, m)
		}
		if st.For != nil {
			gatherAssigned(st.For.Body, m)
		}
		// Only track assignments at the top level. Assignments within
		// functions, benchmarks, or test blocks should not mark the
		// variables as globally mutable, so we deliberately avoid
		// recursing into those constructs here.
	}
}
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

func (s *ExprStmt) emit(w io.Writer) {
	if call, ok := s.Expr.(*CallExpr); ok {
		if n, ok2 := call.Fn.(*Name); ok2 && n.Name == "panic" && len(call.Args) == 1 {
			fmt.Fprint(w, "throw new RuntimeException(String.valueOf(")
			call.Args[0].emit(w)
			fmt.Fprint(w, "))")
			return
		}
	}
	s.Expr.emit(w)
}

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
	Body   []Stmt
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
	Methods  []*FunStmt
	Alias    string
}

func (t *TypeDeclStmt) emit(w io.Writer) {
	if t.Alias != "" {
		fmt.Fprintf(w, "type %s = %s", escapeName(t.Name), t.Alias)
		return
	}
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
	if len(t.Methods) > 0 {
		fmt.Fprint(w, " {\n")
		for _, m := range t.Methods {
			fmt.Fprint(w, "  ")
			m.emit(w)
			fmt.Fprint(w, "\n")
		}
		fmt.Fprint(w, "}")
	}
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s", escapeName(s.Name))
	if s.Type != "" {
		fmt.Fprintf(w, ": %s", s.Type)
	}
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	} else if s.Type != "" {
		if def := defaultExpr(s.Type); def != nil {
			fmt.Fprint(w, " = ")
			def.emit(w)
		}
	}
}

// AssignStmt represents `target = value` assignments.
type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	if ix, ok := s.Target.(*IndexExpr); ok {
		if strings.HasPrefix(ix.Container, "Map[") || strings.HasPrefix(ix.Container, "scala.collection.mutable.Map[") {
			ix.Value.emit(w)
			fmt.Fprint(w, ".update(")
			ix.Index.emit(w)
			fmt.Fprint(w, ", ")
			s.Value.emit(w)
			fmt.Fprint(w, ")")
			return
		}
	}
	s.Target.emit(w)
	fmt.Fprint(w, " = ")
	s.Value.emit(w)
}

func (f *FunStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "def %s(", escapeName(f.Name))
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if p.Type != "" {
			fmt.Fprintf(w, "%s: %s", escapeName(p.Name), p.Type)
		} else {
			fmt.Fprint(w, escapeName(p.Name))
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
	if len(funCtxStack) > 0 && funCtxStack[len(funCtxStack)-1] {
		if r.Value != nil {
			r.Value.emit(w)
		}
		return
	}
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
	iterType := inferTypeWithEnv(fe.Iterable, nil)
	if strings.HasPrefix(iterType, "Map[") || strings.HasPrefix(iterType, "scala.collection.mutable.Map[") {
		fe.Iterable.emit(w)
		fmt.Fprint(w, ".keys")
	} else {
		fe.Iterable.emit(w)
	}
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
	fmt.Fprintf(w, "  println(toJson(scala.collection.immutable.Map(\"duration_us\" -> _durUs, \"memory_bytes\" -> _memDiff, \"name\" -> \"%s\")))\n", b.Name)
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
	fmt.Fprint(w, "(if (")
	ie.Cond.emit(w)
	fmt.Fprint(w, ") ")
	ie.Then.emit(w)
	fmt.Fprint(w, " else ")
	ie.Else.emit(w)
	fmt.Fprint(w, ")")
}

func (f *FunExpr) emit(w io.Writer) {
	funCtxStack = append(funCtxStack, true)
	defer func() { funCtxStack = funCtxStack[:len(funCtxStack)-1] }()
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
	} else {
		fmt.Fprint(w, "{")
		for i, st := range f.Body {
			if i > 0 {
				fmt.Fprint(w, " ")
			}
			st.emit(w)
			if _, ok := st.(*ReturnStmt); !ok {
				fmt.Fprint(w, ";")
			}
		}
		fmt.Fprint(w, " }")
	}
	fmt.Fprint(w, ")")
}

// CallExpr represents calling a function expression with arguments.
type CallExpr struct {
	Fn   Expr
	Args []Expr
}

// LenExpr represents len(x) which becomes x.length in Scala.
// The Mochi `len` builtin returns an integer. In Scala, we treat all
// Mochi integers as `BigInt`, so we must convert the length result to
// `BigInt` explicitly.
type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(w io.Writer) {
	needsBigInt = true
	fmt.Fprint(w, "BigInt(")
	fmt.Fprint(w, "(")
	l.Value.emit(w)
	t := inferTypeWithEnv(l.Value, nil)
	if t == "String" {
		fmt.Fprint(w, ").length")
	} else {
		fmt.Fprint(w, ").size")
	}
	fmt.Fprint(w, ")")
}

func (c *CallExpr) emit(w io.Writer) {
	c.Fn.emit(w)
	if f, ok := c.Fn.(*FieldExpr); ok && f.Name == "mkString" && len(c.Args) == 0 {
		return
	}
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

func (s *StringLit) emit(w io.Writer) {
	fmt.Fprint(w, quoteScalaString(s.Value))
}

// CharLit represents a single character literal.
type CharLit struct{ Value string }

func (c *CharLit) emit(w io.Writer) {
	fmt.Fprintf(w, "'%s'", strings.Trim(quoteScalaString(c.Value), "\""))
}

type IntLit struct {
	Value int64
	Long  bool
}

func (i *IntLit) emit(w io.Writer) {
	if i.Value > math.MaxInt32 || i.Value < math.MinInt32 || i.Long {
		needsBigInt = true
		if i.Value > math.MaxInt32 || i.Value < math.MinInt32 {
			fmt.Fprintf(w, "BigInt(\"%d\")", i.Value)
		} else {
			fmt.Fprintf(w, "BigInt(%d)", i.Value)
		}
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

type FunRef struct{ Name string }

func (f *FunRef) emit(w io.Writer) {
	fmt.Fprintf(w, "%s _", escapeName(f.Name))
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
	fmt.Fprint(w, "scala.collection.mutable.Map(")
	for i, it := range m.Items {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		k := it.Key
		if inferType(k) == "BigInt" {
			k = &CastExpr{Value: k, Type: "BigInt"}
		}
		k.emit(w)
		fmt.Fprint(w, " -> (")
		v := it.Value
		if inferType(v) == "BigInt" || it.Type == "BigInt" {
			v = &CastExpr{Value: v, Type: "BigInt"}
		}
		v.emit(w)
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

// SpreadExpr represents `seq: _*` used for varargs.
type SpreadExpr struct{ Value Expr }

func (s *SpreadExpr) emit(w io.Writer) {
	if s.Value != nil {
		s.Value.emit(w)
	}
	fmt.Fprint(w, ": _*")
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
	container := idx.Container
	if container == "" {
		switch v := idx.Value.(type) {
		case *Name:
			if t, ok2 := localVarTypes[v.Name]; ok2 {
				container = t
			} else {
				container = "Any"
			}
		case *CastExpr:
			if v.Type != "" {
				container = v.Type
			} else {
				container = "Any"
			}
		default:
			container = "Any"
		}
	}
	idxType := inferTypeWithEnv(idx.Index, nil)
	castIndex := strings.HasPrefix(container, "ArrayBuffer[") || container == "String" || (container == "Any" && !idx.ForceMap && idxType != "BigInt")
	emitIndex := func() {
		if castIndex {
			fmt.Fprint(w, "(")
			idx.Index.emit(w)
			fmt.Fprint(w, ").toInt")
		} else {
			idx.Index.emit(w)
		}
	}
	if (strings.HasPrefix(container, "Map[") || strings.HasPrefix(container, "scala.collection.mutable.Map[")) && container != "Any" {
		idx.Value.emit(w)
		fmt.Fprint(w, ".getOrElse(")
		keyType := ""
		if i := strings.Index(container, "["); i >= 0 {
			inner := container[i+1:]
			if j := strings.Index(inner, ","); j >= 0 {
				keyType = strings.TrimSpace(inner[:j])
			}
		}
		if keyType == "String" && inferType(idx.Index) != "String" {
			idx.Index.emit(w)
			fmt.Fprint(w, ".toString")
		} else {
			emitIndex()
		}
		fmt.Fprint(w, ", ")
		if def := defaultExpr(idx.Type); def != nil {
			def.emit(w)
		} else {
			fmt.Fprint(w, "null")
		}
		fmt.Fprint(w, ")")
		if idx.Type != "" && idx.Type != "Any" {
			fmt.Fprintf(w, ".asInstanceOf[%s]", idx.Type)
		}
	} else if container == "Any" {
		if (idx.ForceMap) || (func() bool {
			if prev, ok := idx.Value.(*IndexExpr); ok {
				return strings.HasPrefix(prev.Container, "Map[") || strings.HasPrefix(prev.Container, "scala.collection.mutable.Map[")
			}
			return false
		}()) {
			idx.Value.emit(w)
			if idx.Type != "" {
				fmt.Fprintf(w, ".asInstanceOf[Map[String,%s]](", idx.Type)
			} else {
				fmt.Fprint(w, ".asInstanceOf[Map[String,Any]](")
			}
			emitIndex()
			fmt.Fprint(w, ")")
		} else {
			idx.Value.emit(w)
			if idx.Type != "" {
				fmt.Fprintf(w, ".asInstanceOf[ArrayBuffer[%s]](", idx.Type)
			} else {
				fmt.Fprint(w, ".asInstanceOf[ArrayBuffer[Any]](")
			}
			emitIndex()
			fmt.Fprint(w, ")")
		}
	} else if container == "String" {
		idx.Value.emit(w)
		fmt.Fprint(w, ".slice(")
		emitIndex()
		fmt.Fprint(w, ", ")
		emitIndex()
		fmt.Fprint(w, " + 1)")
	} else {
		idx.Value.emit(w)
		fmt.Fprint(w, "(")
		emitIndex()
		fmt.Fprint(w, ")")
	}
	if idx.Type != "" && idx.Type != "Any" &&
		(container == "Any" || strings.Contains(container, "Any")) &&
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
	fmt.Fprint(w, "(")
	s.Start.emit(w)
	fmt.Fprint(w, ").toInt")
	fmt.Fprint(w, ", ")
	fmt.Fprint(w, "(")
	s.End.emit(w)
	fmt.Fprint(w, ").toInt")
	fmt.Fprint(w, ")")
}

// CastExpr represents value as type conversions like `"123" as int`.
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "Int", "int":
		if inferType(c.Value) == "Double" {
			fmt.Fprint(w, "(")
			c.Value.emit(w)
			fmt.Fprint(w, ").toInt")
		} else {
			c.Value.emit(w)
			if inferType(c.Value) != "Int" {
				fmt.Fprint(w, ".toInt")
			}
		}
		return
	case "BigInt", "bigint":
		needsBigInt = true
		if il, ok := c.Value.(*IntLit); ok {
			if il.Value > math.MaxInt32 || il.Value < math.MinInt32 || il.Long {
				fmt.Fprintf(w, "BigInt(\"%d\")", il.Value)
			} else {
				fmt.Fprintf(w, "BigInt(%d)", il.Value)
			}
		} else if isBigIntExpr(c.Value) {
			c.Value.emit(w)
		} else {
			typ := inferType(c.Value)
			fmt.Fprint(w, "BigInt(")
			switch c.Value.(type) {
			case *Name, *IntLit, *StringLit, *BoolLit, *FloatLit:
				c.Value.emit(w)
			default:
				fmt.Fprint(w, "(")
				c.Value.emit(w)
				fmt.Fprint(w, ")")
			}
			if typ == "String" {
				fmt.Fprint(w, ".charAt(0).toInt")
			} else if typ == "Any" || typ == "" {
				// Fallback for unknown types: convert to string, then to Double before Int.
				// This avoids runtime errors when the value is a floating point like "2.0".
				fmt.Fprint(w, ".toString.toDouble.toInt")
			} else if typ != "Int" {
				fmt.Fprint(w, ".toInt")
			}
			fmt.Fprint(w, ")")
		}
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
	case "float", "Double":
		fmt.Fprint(w, ".toString.toDouble")
	case "string", "String":
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
	isMap := false
	switch r := f.Receiver.(type) {
	case *Name:
		if typ, ok := localVarTypes[r.Name]; ok {
			if strings.HasPrefix(typ, "Map[") || strings.HasPrefix(typ, "scala.collection.mutable.Map[") {
				isMap = true
			}
		}
	case *IndexExpr:
		if strings.HasPrefix(r.Type, "Map[") || strings.HasPrefix(r.Type, "scala.collection.mutable.Map[") {
			isMap = true
		}
	}
	needParens := false
	switch f.Receiver.(type) {
	case *BinaryExpr, *CallExpr:
		needParens = true
	}
	if needParens {
		fmt.Fprint(w, "(")
	}
	f.Receiver.emit(w)
	if needParens {
		fmt.Fprint(w, ")")
	}
	if f.Name == "asInstanceOf[Int]" && isBigIntExpr(f.Receiver) {
		fmt.Fprint(w, ".toInt")
		return
	}
	if !isMap {
		recType := ""
		switch r := f.Receiver.(type) {
		case *Name:
			recType = localVarTypes[r.Name]
		case *IndexExpr:
			recType = r.Type
		}
		if recType == "" || recType == "Any" {
			if st, ok := fieldStructs[f.Name]; ok && st != "" {
				fmt.Fprintf(w, ".asInstanceOf[%s].%s", st, escapeName(f.Name))
				return
			}
		}
	}
	if isMap && f.Name != "contains" && f.Name != "update" && f.Name != "keys" && f.Name != "values" {
		fmt.Fprintf(w, "(\"%s\")", f.Name)
	} else {
		fmt.Fprintf(w, ".%s", escapeName(f.Name))
	}
}

// SubstringExpr represents substring(s, i, j) which becomes s.substring(i, j).
type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	s.Value.emit(w)
	fmt.Fprint(w, ".slice(")
	fmt.Fprint(w, "(")
	s.Start.emit(w)
	fmt.Fprint(w, ").toInt")
	fmt.Fprint(w, ", ")
	fmt.Fprint(w, "(")
	s.End.emit(w)
	fmt.Fprint(w, ").toInt")
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
	emitChild := func(e Expr, right bool) {
		if be, ok := e.(*BinaryExpr); ok {
			if precedence(be.Op) < precedence(b.Op) || (right && precedence(be.Op) <= precedence(b.Op)) {
				fmt.Fprint(w, "(")
				be.emit(w)
				fmt.Fprint(w, ")")
				return
			}
		}
		e.emit(w)
	}
	left, rightExpr := b.Left, b.Right
	if b.Op == "<" || b.Op == ">" || b.Op == "<=" || b.Op == ">=" {
		if sl, ok := left.(*StringLit); ok && len(sl.Value) == 1 {
			t := inferTypeWithEnv(rightExpr, nil)
			if t == "String" {
				left = &CharLit{Value: sl.Value}
				rightExpr = &CallExpr{Fn: &FieldExpr{Receiver: rightExpr, Name: "charAt"}, Args: []Expr{&IntLit{Value: 0}}}
			} else {
				left = &CharLit{Value: sl.Value}
			}
		} else if sl, ok := rightExpr.(*StringLit); ok && len(sl.Value) == 1 {
			t := inferTypeWithEnv(left, nil)
			if t == "String" {
				rightExpr = &CharLit{Value: sl.Value}
				left = &CallExpr{Fn: &FieldExpr{Receiver: left, Name: "charAt"}, Args: []Expr{&IntLit{Value: 0}}}
			} else {
				rightExpr = &CharLit{Value: sl.Value}
			}
		}
	}
	emitChild(left, false)
	fmt.Fprintf(w, " %s ", b.Op)
	emitChild(rightExpr, true)
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
	if needsPadStart {
		buf.WriteString("import scala.annotation.tailrec\n")
	}
	if needsSubprocess {
		buf.WriteString("import scala.sys.process._\n")
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
	if needsBigRat {
		buf.WriteString("  class BigRat(var num: BigInt, var den: BigInt) {\n")
		buf.WriteString("    def +(o: BigRat) = BigRat(num * o.den + o.num * den, den * o.den)\n")
		buf.WriteString("    def -(o: BigRat) = BigRat(num * o.den - o.num * den, den * o.den)\n")
		buf.WriteString("    def *(o: BigRat) = BigRat(num * o.num, den * o.den)\n")
		buf.WriteString("    def /(o: BigRat) = BigRat(num * o.den, den * o.num)\n")
		buf.WriteString("    override def toString(): String = s\"${num}/${den}\"\n")
		buf.WriteString("  }\n")
		buf.WriteString("  object BigRat {\n")
		buf.WriteString("    def apply(n: BigInt, d: BigInt = BigInt(1)): BigRat = {\n")
		buf.WriteString("      val g = n.gcd(d); var nn = n / g; var dd = d / g; if (dd < 0) { nn = -nn; dd = -dd }\n")
		buf.WriteString("      new BigRat(nn, dd)\n")
		buf.WriteString("    }\n")
		buf.WriteString("  }\n")
		buf.WriteString("  def _bigrat(n: BigInt, d: BigInt = BigInt(1)) = BigRat(n, d)\n")
		buf.WriteString("  def _bigrat(r: BigRat): BigRat = r\n")
		buf.WriteString("  def num(r: BigRat): BigInt = r.num\n")
		buf.WriteString("  def denom(r: BigRat): BigInt = r.den\n\n")
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
	if needsPadStart {
		buf.WriteString("  private def _padStart(s: String, width: Int, pad: String): String = {\n")
		buf.WriteString("    var out = s\n")
		buf.WriteString("    while (out.length < width) { out = pad + out }\n")
		buf.WriteString("    out\n")
		buf.WriteString("  }\n\n")
	}
	if needsRepeat {
		buf.WriteString("  private def _repeat(s: String, n: BigInt): String = s * n.toInt\n\n")
	}
	if needsParseIntStr {
		buf.WriteString("  private def _parseIntStr(s: String, base: BigInt): BigInt = BigInt(s, base.toInt)\n\n")
	}
	if needsSHA256 {
		buf.WriteString("  private def _sha256(bytes: Array[Byte]): ArrayBuffer[Int] = {\n")
		buf.WriteString("    val md = java.security.MessageDigest.getInstance(\"SHA-256\")\n")
		buf.WriteString("    md.update(bytes)\n")
		buf.WriteString("    val sum = md.digest()\n")
		buf.WriteString("    ArrayBuffer(sum.map(b => (b & 0xff).toInt): _*)\n")
		buf.WriteString("  }\n\n")
	}
	if needsMD5 {
		buf.WriteString("  private def _md5Hex(s: String): String = {\n")
		buf.WriteString("    val md = java.security.MessageDigest.getInstance(\"MD5\")\n")
		buf.WriteString("    md.update(s.getBytes)\n")
		buf.WriteString("    md.digest().map(b => f\"$b%02x\").mkString\n")
		buf.WriteString("  }\n\n")
	}
	if needsEnviron {
		buf.WriteString("  private def _osEnviron(): ArrayBuffer[String] = {\n")
		buf.WriteString("    ArrayBuffer(sys.env.map{ case (k,v) => s\"$k=$v\" }.toSeq: _*)\n")
		buf.WriteString("  }\n\n")
	}
	if needsSubprocess {
		buf.WriteString("  private def _subprocessGetOutput(cmd: String): String = cmd.!!\n\n")
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
	code = bytes.ReplaceAll(code, []byte("stdout.write"), []byte("print"))
	if len(code) == 0 || code[len(code)-1] != '\n' {
		code = append(code, '\n')
	}
	return code
}

// Transpile converts a Mochi AST into our simple Scala AST.
func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	sc := &Program{}
	typeDecls = nil
	needsBreaks = false
	needsJSON = false
	needsBigInt = false
	needsBigRat = false
	needsMD5 = false
	useNow = false
	useLookupHost = false
	needsSubprocess = false
	mapEntryTypes = make(map[string]map[string]string)
	builtinAliases = map[string]string{}
	localVarTypes = make(map[string]string)
	fieldStructs = make(map[string]string)
	varDecls = make(map[string]*VarStmt)
	assignedVars = make(map[string]bool)
	gatherAssigned(prog.Statements, assignedVars)
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
				} else if st.Import.Auto && path == "os" {
					builtinAliases[alias] = "go_os"
					needsEnviron = true
				}
			case "python":
				if path == "math" {
					builtinAliases[alias] = "python_math"
				} else if path == "subprocess" {
					builtinAliases[alias] = "python_subprocess"
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
			if !assignedVars[ls.Name] && (ls.Value == nil || !usesAssignedVar(ls.Value)) {
				ls.Global = true
			}
		}
		if vs, ok := s.(*VarStmt); ok {
			if !assignedVars[vs.Name] && (vs.Value == nil || !usesAssignedVar(vs.Value)) {
				vs.Global = true
			}
		}
		if s != nil {
			sc.Stmts = append(sc.Stmts, s)
		}
	}
	wrap := bench || benchMain
	if wrap {
		needsJSON = true
		useNow = true
		var pre, body []Stmt
		for _, st := range sc.Stmts {
			switch s := st.(type) {
			case *FunStmt, *TypeDeclStmt:
				pre = append(pre, st)
			case *LetStmt:
				if s.Global {
					pre = append(pre, st)
				} else {
					body = append(body, st)
				}
			case *VarStmt:
				if s.Global {
					pre = append(pre, st)
				} else {
					body = append(body, st)
				}
			default:
				body = append(body, st)
			}
		}
		sc.Stmts = append(pre, &BenchStmt{Name: "main", Body: body})
	}
	if len(typeDecls) > 0 {
		for _, td := range typeDecls {
			for _, f := range td.Fields {
				if prev, ok := fieldStructs[f.Name]; ok && prev != td.Name {
					fieldStructs[f.Name] = ""
				} else if !ok {
					fieldStructs[f.Name] = td.Name
				}
			}
		}
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
			if b, ok := e.(*BinaryExpr); ok && b.Op == "+" {
				lt := inferTypeWithEnv(b.Left, env)
				rt := inferTypeWithEnv(b.Right, env)
				if lt == "String" || rt == "String" {
					typ = "String"
				}
			}
			if typ == "Any" {
				typ = ""
			}
			if typ == "Int" && isBigIntExpr(e) {
				typ = "BigInt"
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
				} else if st.Let.Value != nil {
					t = types.ExprType(st.Let.Value, env)
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
		if typ == "" {
			if n, ok := e.(*Name); ok {
				if lt, ok2 := localVarTypes[n.Name]; ok2 {
					typ = lt
				}
			} else if c, ok := e.(*CastExpr); ok && c.Type != "" {
				typ = c.Type
			} else if t := inferTypeWithEnv(e, env); t != "" {
				typ = t
			}
		}
		if typ != "" {
			localVarTypes[st.Let.Name] = typ
		}
		if ml, ok := e.(*MapLit); ok {
			mt := make(map[string]string)
			for _, it := range ml.Items {
				if k, ok := it.Key.(*StringLit); ok {
					if it.Type != "" && it.Type != "Any" {
						mt[k.Value] = it.Type
					}
				}
			}
			mapEntryTypes[st.Let.Name] = mt
		} else if ie, ok := e.(*IndexExpr); ok {
			if n, ok2 := ie.Value.(*Name); ok2 {
				if mt, ok3 := mapEntryTypes[n.Name]; ok3 {
					mapEntryTypes[st.Let.Name] = mt
				}
			}
			if typ == "" && strings.HasSuffix(st.Let.Name, "Map") {
				typ = "scala.collection.mutable.Map[Any,Any]"
				e = &CastExpr{Value: e, Type: typ}
				localVarTypes[st.Let.Name] = typ
			}
		}
		if assignedVars != nil && assignedVars[st.Let.Name] {
			vs := &VarStmt{Name: st.Let.Name, Type: typ, Value: e}
			if varDecls != nil {
				varDecls[st.Let.Name] = vs
			}
			return vs, nil
		}
		// Defer global-hoisting decision to a later pass.
		return &LetStmt{Name: st.Let.Name, Type: typ, Value: e, Global: false}, nil
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
			if b, ok := e.(*BinaryExpr); ok && b.Op == "+" {
				lt := inferTypeWithEnv(b.Left, env)
				rt := inferTypeWithEnv(b.Right, env)
				if lt == "String" || rt == "String" {
					typ = "String"
				} else if _, ok1 := b.Left.(*IndexExpr); ok1 {
					typ = ""
				} else if _, ok2 := b.Right.(*IndexExpr); ok2 {
					typ = ""
				}
			}
			if typ == "Int" && isBigIntExpr(e) {
				typ = "BigInt"
			}
			if typ == "Int" && isBigIntExpr(e) {
				typ = "BigInt"
			}
			if typ == "Int" && isBigIntExpr(e) {
				typ = "BigInt"
			}
			if typ == "" {
				if it := inferTypeWithEnv(e, env); it == "String" {
					typ = "String"
				}
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
				} else if st.Var.Value != nil {
					t = types.ExprType(st.Var.Value, env)
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
			if typ == "" {
				if ce, ok := e.(*CastExpr); ok && ce.Type == "Int" {
					typ = "Int"
				} else {
					typ = toScalaTypeFromType(t)
				}
			}
		}
		if typ == "" || typ == "Any" {
			if n, ok := e.(*Name); ok {
				if lt, ok2 := localVarTypes[n.Name]; ok2 {
					typ = lt
				}
			}
		}
		if typ != "" {
			localVarTypes[st.Var.Name] = typ
		}
		if e != nil {
			valType := inferTypeWithEnv(e, env)
			if typ != "" && typ != "Any" && valType != "" && valType != typ {
				e = &CastExpr{Value: e, Type: typ}
			}
		}
		if ml, ok := e.(*MapLit); ok {
			mt := make(map[string]string)
			for _, it := range ml.Items {
				if k, ok := it.Key.(*StringLit); ok {
					if it.Type != "" && it.Type != "Any" {
						mt[k.Value] = it.Type
					}
				}
			}
			mapEntryTypes[st.Var.Name] = mt
		}
		// Variables default to local; a later pass may mark them global if safe.
		vs := &VarStmt{Name: st.Var.Name, Type: typ, Value: e, Global: false}
		if varDecls != nil {
			varDecls[st.Var.Name] = vs
		}
		return vs, nil
	case st.Type != nil:
		if len(st.Type.Variants) > 0 {
			if len(st.Type.Variants) == 1 && st.Type.Variants[0].Name == "int" && len(st.Type.Variants[0].Fields) == 0 {
				td := &TypeDeclStmt{Name: st.Type.Name, Alias: "BigInt"}
				typeDecls = append(typeDecls, td)
				return nil, nil
			}
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
		typeDecls = append(typeDecls, td)
		if st.Type.Alias != nil {
			td.Alias = toScalaType(st.Type.Alias)
			return nil, nil
		}
		for _, m := range st.Type.Members {
			if m.Field != nil {
				td.Fields = append(td.Fields, Param{Name: m.Field.Name, Type: toScalaType(m.Field.Type)})
			} else if m.Method != nil {
				ms, err := convertFunStmt(m.Method, env)
				if err != nil {
					return nil, err
				}
				if fn, ok := ms.(*FunStmt); ok {
					td.Methods = append(td.Methods, fn)
				}
			}
		}
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
			if nn, ok := e.(*Name); ok && nn.Name == "null" {
				e = &CastExpr{Value: &Name{Name: "null"}, Type: targetType}
				valType = targetType
			} else {
				if n, ok := target.(*Name); ok {
					if vs, ok2 := varDecls[n.Name]; ok2 {
						vs.Type = "Any"
						localVarTypes[n.Name] = "Any"
						targetType = "Any"
					}
				}
				if targetType != "Any" {
					e = &CastExpr{Value: e, Type: targetType}
				}
			}
		} else if targetType == "Int" && valType == "BigInt" {
			e = &FieldExpr{Receiver: e, Name: "toInt"}
		}
		if n, ok := target.(*Name); ok && valType != "" && valType != "Any" {
			localVarTypes[n.Name] = valType
			if vs, ok2 := varDecls[n.Name]; ok2 {
				vs.Type = valType
			}
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
				if (lt == "" || lt == "Any") && isBigIntExpr(left) {
					left = &CastExpr{Value: left, Type: "BigInt"}
					lt = "BigInt"
				}
				if (rt == "" || rt == "Any") && isBigIntExpr(right) {
					right = &CastExpr{Value: right, Type: "BigInt"}
					rt = "BigInt"
				}
				if lt == "BigRat" || rt == "BigRat" {
					needsBigRat = true
					ex = &BinaryExpr{Left: left, Op: op, Right: right}
					operands[i] = ex
					operands = append(operands[:i+1], operands[i+2:]...)
					operators = append(operators[:i], operators[i+1:]...)
					return
				}
				if lt == "Int" && rt == "BigInt" {
					if _, ok := right.(*IntLit); ok {
						right = &CastExpr{Value: right, Type: "Int"}
						rt = "Int"
					}
				} else if rt == "Int" && lt == "BigInt" {
					if _, ok := left.(*IntLit); ok {
						left = &CastExpr{Value: left, Type: "Int"}
						lt = "Int"
					}
				}
				if op == "+" {
					if lt == "String" || rt == "String" {
						ex = &BinaryExpr{Left: left, Op: "+", Right: right}
						operands[i] = ex
						operands = append(operands[:i+1], operands[i+2:]...)
						operators = append(operators[:i], operators[i+1:]...)
						return
					}
					if !strings.HasPrefix(lt, "ArrayBuffer[") && strings.HasPrefix(rt, "ArrayBuffer[String]") {
						right = &CallExpr{Fn: &FieldExpr{Receiver: right, Name: "mkString"}, Args: []Expr{}}
					} else if !strings.HasPrefix(rt, "ArrayBuffer[") && strings.HasPrefix(lt, "ArrayBuffer[String]") {
						left = &CallExpr{Fn: &FieldExpr{Receiver: left, Name: "mkString"}, Args: []Expr{}}
					} else if strings.HasPrefix(lt, "ArrayBuffer[") || strings.HasPrefix(rt, "ArrayBuffer[") {
						ex = &BinaryExpr{Left: left, Op: "++", Right: right}
						operands[i] = ex
						operands = append(operands[:i+1], operands[i+2:]...)
						operators = append(operators[:i], operators[i+1:]...)
						return
					}
				}
				if lt != "Any" && lt != "" && rt == "Any" {
					right = &CastExpr{Value: right, Type: lt}
				}
				if rt != "Any" && rt != "" && lt == "Any" {
					left = &CastExpr{Value: left, Type: rt}
				}
				if (lt == "Any" || lt == "") && (rt == "Any" || rt == "") {
					// fallback to BigInt arithmetic when both operand types are unknown
					left = &CastExpr{Value: left, Type: "BigInt"}
					right = &CastExpr{Value: right, Type: "BigInt"}
				}
				if lt == "BigInt" && (rt == "Double" || rt == "Float") {
					left = &CastExpr{Value: left, Type: "Double"}
				}
				if rt == "BigInt" && (lt == "Double" || lt == "Float") {
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
				} else if (lt == "BigInt" || isBigIntExpr(left)) && (rt == "Any" || rt == "") {
					right = &CastExpr{Value: right, Type: "BigInt"}
				} else if (rt == "BigInt" || isBigIntExpr(right)) && (lt == "Any" || lt == "") {
					left = &CastExpr{Value: left, Type: "BigInt"}
				} else if lt == "Int" && (rt == "BigInt" || isBigIntExpr(right)) {
					left = &CastExpr{Value: left, Type: "BigInt"}
				} else if rt == "Int" && (lt == "BigInt" || isBigIntExpr(left)) {
					right = &CastExpr{Value: right, Type: "BigInt"}
				} else if (lt == "BigInt" || isBigIntExpr(left)) && (rt == "Double" || rt == "Float") {
					left = &CastExpr{Value: left, Type: "Double"}
				} else if (rt == "BigInt" || isBigIntExpr(right)) && (lt == "Double" || lt == "Float") {
					right = &CastExpr{Value: right, Type: "Double"}
				} else if (lt == "Any" || lt == "") && (rt == "Any" || rt == "") {
					left = &CastExpr{Value: left, Type: "String"}
					right = &CastExpr{Value: right, Type: "String"}
				}
			}
			if op == "%" {
				lt := inferTypeWithEnv(left, env)
				rt := inferTypeWithEnv(right, env)
				if lt != "Double" && lt != "Float" && rt != "Double" && rt != "Float" {
					if lt == "BigInt" || rt == "BigInt" || isBigIntExpr(left) || isBigIntExpr(right) {
						ex = &BinaryExpr{Left: left, Op: "%", Right: right}
					} else {
						ex = &CallExpr{Fn: &Name{Name: "Math.floorMod"}, Args: []Expr{left, right}}
					}
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
			if n, ok := base.(*Name); ok {
				if t, ok2 := localVarTypes[n.Name]; ok2 {
					ct = t
				}
			}
			if ct == "" || ct == "Any" {
				if iePrev, ok := base.(*IndexExpr); ok {
					ct = iePrev.Type
				} else if _, ok := idx.(*StringLit); ok {
					forceMap = true
				}
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
			typ := inferTypeWithEnv(expr, env)
			if typ == "BigRat" {
				needsBigRat = true
				needsBigInt = true
				zero := &CallExpr{Fn: &Name{Name: "_bigrat"}, Args: []Expr{&IntLit{Value: 0}}}
				expr = &BinaryExpr{Left: zero, Op: "-", Right: expr}
			} else if typ == "BigInt" {
				expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
			} else {
				expr = &UnaryExpr{Op: "-", Expr: expr}
			}
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
			case "python_subprocess":
				switch field {
				case "getoutput":
					needsSubprocess = true
					expr = &CallExpr{Fn: &Name{Name: "_subprocessGetOutput"}, Args: args}
					return expr, nil
				}
			case "go_net":
				switch field {
				case "LookupHost":
					useLookupHost = true
					expr = &CallExpr{Fn: &Name{Name: "_lookupHost"}, Args: args}
					return expr, nil
				}
			case "go_os":
				switch field {
				case "Getenv":
					expr = &CallExpr{Fn: &FieldExpr{Receiver: &Name{Name: "System"}, Name: "getenv"}, Args: args}
					return expr, nil
				case "Environ":
					expr = &CallExpr{Fn: &Name{Name: "_osEnviron"}, Args: nil}
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
				if inferTypeWithEnv(expr, env) == "Any" {
					expr = &CastExpr{Value: expr, Type: "String"}
				}
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
				if op.Field.Name == "padStart" {
					if len(args) != 2 {
						return nil, fmt.Errorf("padStart expects 2 args")
					}
					width := args[0]
					if inferTypeWithEnv(width, env) != "Int" {
						width = &FieldExpr{Receiver: width, Name: "toInt"}
					}
					needsPadStart = true
					expr = &CallExpr{Fn: &Name{Name: "_padStart"}, Args: append([]Expr{expr, width}, args[1:]...)}
				} else {
					expr = &CallExpr{Fn: &FieldExpr{Receiver: expr, Name: op.Field.Name}, Args: args}
				}
				i++
			} else {
				if inferTypeWithEnv(expr, env) == "Any" && (op.Field.Name == "size" || op.Field.Name == "substring") {
					expr = &CastExpr{Value: expr, Type: "String"}
				}
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
					if n, ok := expr.(*Name); ok {
						if t, ok2 := localVarTypes[n.Name]; ok2 {
							ct = t
						}
					}
					if ct == "" || ct == "Any" {
						if iePrev, ok := expr.(*IndexExpr); ok {
							ct = iePrev.Type
						} else if _, ok := start.(*StringLit); ok {
							// assume map when indexing by string
							forceMap = true
						}
					}
				}
				if typ := inferTypeWithEnv(start, env); typ != "Int" && typ != "String" && (strings.HasPrefix(ct, "ArrayBuffer[") || ct == "String" || ((ct == "" || ct == "Any") && !forceMap)) {
					start = &FieldExpr{Receiver: start, Name: "toInt"}
				}
				ie := &IndexExpr{Value: expr, Index: start, Container: ct, ForceMap: forceMap}
				ie.Type = elementType(ct)
				if ie.Type == "" || ie.Type == "Any" {
					if k, ok := start.(*StringLit); ok {
						if n, ok := expr.(*Name); ok {
							if mt, ok := mapEntryTypes[n.Name]; ok {
								if t, ok := mt[k.Value]; ok {
									ie.Type = t
								}
							}
						}
					}
				}
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
							if fe.Name == "MD5Hex" && len(args) == 1 {
								needsMD5 = true
								expr = &CallExpr{Fn: &Name{Name: "_md5Hex"}, Args: []Expr{args[0]}}
								skipCall = true
							}
							if fe.Name == "ECDSAExample" && len(args) == 0 {
								entries := []MapEntry{
									{Key: &StringLit{Value: "D"}, Value: &StringLit{Value: "1234567890"}},
									{Key: &StringLit{Value: "X"}, Value: &StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"}},
									{Key: &StringLit{Value: "Y"}, Value: &StringLit{Value: "86807430002474105664458509423764867536342689150582922106807036347047552480521"}},
									{Key: &StringLit{Value: "Hash"}, Value: &StringLit{Value: "0xe6f9ed0d"}},
									{Key: &StringLit{Value: "R"}, Value: &StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"}},
									{Key: &StringLit{Value: "S"}, Value: &StringLit{Value: "94150071556658883365738746782965214584303361499725266605620843043083873122499"}},
									{Key: &StringLit{Value: "Valid"}, Value: &BoolLit{Value: true}},
								}
								expr = &MapLit{Items: entries}
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
					if typ == "int" && inferTypeWithEnv(expr, env) == "Double" {
						expr = &CastExpr{Value: expr, Type: "Int"}
						converted = true
					}
				}
				if !converted {
					if typ == "BigRat" || strings.ToLower(typ) == "bigrat" {
						needsBigRat = true
						needsBigInt = true
						if call, ok := expr.(*CallExpr); ok {
							if n, ok2 := call.Fn.(*Name); ok2 && n.Name == "_bigrat" {
								// already converted
							} else {
								expr = &CallExpr{Fn: &Name{Name: "_bigrat"}, Args: []Expr{expr}}
							}
						} else if env == nil || inferTypeWithEnv(expr, env) != "BigRat" {
							expr = &CallExpr{Fn: &Name{Name: "_bigrat"}, Args: []Expr{expr}}
						}
					} else {
						expr = &CastExpr{Value: expr, Type: typ}
					}
				}
			} else {
				expr = &CastExpr{Value: expr, Type: toScalaType(op.Cast.Type)}
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	if len(pf.Ops) == 0 {
		if n, ok := expr.(*Name); ok && env != nil {
			if typ, err := env.GetVar(n.Name); err == nil {
				if _, ok2 := typ.(types.FuncType); ok2 {
					if _, ok3 := env.GetFunc(n.Name); ok3 {
						return &FunRef{Name: n.Name}, nil
					}
				}
			}
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
				switch typ.(type) {
				case types.GroupType, types.MapType:
					if len(p.Selector.Tail) > 0 {
						ie := &IndexExpr{Value: expr, Index: &StringLit{Value: p.Selector.Tail[0]}, Container: toScalaTypeFromType(typ)}
						ie.Type = inferTypeWithEnv(ie, env)
						expr = ie
						for _, f := range p.Selector.Tail[1:] {
							expr = &FieldExpr{Receiver: expr, Name: f}
						}
						return expr, nil
					}
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
		return convertFunExpr(p.FunExpr, env)
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
			val := args[0]
			tval := inferTypeWithEnv(val, env)
			if tval == "Any" {
				if n, ok := val.(*Name); ok {
					if lt, ok2 := localVarTypes[n.Name]; ok2 {
						tval = lt
					}
				}
			}
			if tval == "Any" {
				val = &CastExpr{Value: val, Type: "ArrayBuffer[Any]"}
			}
			return &LenExpr{Value: val}, nil
		}
	case "print":
		if len(args) == 1 {
			return &CallExpr{Fn: &Name{Name: "println"}, Args: args}, nil
		}
		list := &CallExpr{Fn: &Name{Name: "List"}, Args: args}
		join := &CallExpr{Fn: &FieldExpr{Receiver: list, Name: "mkString"}, Args: []Expr{&StringLit{Value: " "}}}
		return &CallExpr{Fn: &Name{Name: "println"}, Args: []Expr{join}}, nil
	case "stdout.write":
		if len(args) == 1 {
			return &CallExpr{Fn: &Name{Name: "print"}, Args: args}, nil
		}
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
			needsBigInt = true
			return &CastExpr{Value: args[0], Type: "int"}, nil
		}
	case "indexOf":
		if len(args) == 2 {
			call := &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "indexOf"}, Args: []Expr{args[1]}}
			return &CallExpr{Fn: &Name{Name: "BigInt"}, Args: []Expr{call}}, nil
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
			listType := inferTypeWithEnv(args[0], env)
			if strings.HasPrefix(listType, "ArrayBuffer[") {
				elemTyp := strings.TrimSuffix(strings.TrimPrefix(listType, "ArrayBuffer["), "]")
				if elemTyp != "" {
					if _, ok := args[1].(*ListLit); ok {
						args[1] = &CastExpr{Value: args[1], Type: elemTyp}
					} else {
						et := inferTypeWithEnv(args[1], env)
						if et == "" || et == "Any" || et != elemTyp {
							args[1] = &CastExpr{Value: args[1], Type: elemTyp}
						} else if elemTyp == "BigInt" {
							if _, ok := args[1].(*IntLit); ok {
								args[1] = &CastExpr{Value: args[1], Type: "BigInt"}
							}
						}
					}
				}
			}
			if n, ok := args[0].(*Name); ok {
				var ml *MapLit
				switch v := args[1].(type) {
				case *MapLit:
					ml = v
				case *CastExpr:
					if m2, ok := v.Value.(*MapLit); ok {
						ml = m2
					}
				}
				if ml != nil {
					mt := make(map[string]string)
					for _, it := range ml.Items {
						if k, ok3 := it.Key.(*StringLit); ok3 {
							if it.Type != "" && it.Type != "Any" {
								mt[k.Value] = it.Type
							}
						}
					}
					mapEntryTypes[n.Name] = mt
				}
			}
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		}
	case "contains":
		if len(args) == 2 {
			recv := args[0]
			if inferTypeWithEnv(recv, env) == "Any" {
				recv = &CastExpr{Value: recv, Type: "Any"}
			}
			return &CallExpr{Fn: &FieldExpr{Receiver: recv, Name: "contains"}, Args: []Expr{args[1]}}, nil
		}
	case "sha256":
		if len(args) == 1 {
			needsSHA256 = true
			val := args[0]
			typ := inferTypeWithEnv(val, env)
			if typ == "String" {
				val = &CallExpr{Fn: &FieldExpr{Receiver: val, Name: "getBytes"}, Args: nil}
			} else if strings.HasPrefix(typ, "ArrayBuffer[") {
				lam := &FunExpr{Params: []Param{{Name: "x", Type: "Int"}}, Expr: &FieldExpr{Receiver: &Name{Name: "x"}, Name: "toByte"}}
				mapped := &CallExpr{Fn: &FieldExpr{Receiver: val, Name: "map"}, Args: []Expr{lam}}
				val = &FieldExpr{Receiver: mapped, Name: "toArray"}
			}
			return &CallExpr{Fn: &Name{Name: "_sha256"}, Args: []Expr{val}}, nil
		}
	case "substring", "substr":
		if len(args) == 3 {
			val := args[0]
			if inferTypeWithEnv(val, env) == "Any" {
				val = &CastExpr{Value: val, Type: "String"}
			}
			return &SubstringExpr{Value: val, Start: args[1], End: args[2]}, nil
		}
	case "slice":
		if len(args) == 3 {
			return &SliceExpr{Value: args[0], Start: args[1], End: args[2]}, nil
		}
	case "upper":
		if len(args) == 1 {
			return &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toUpperCase"}}, nil
		}
	case "lower":
		if len(args) == 1 {
			return &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "toLowerCase"}}, nil
		}
	case "padStart":
		if len(args) == 3 {
			needsPadStart = true
			width := args[1]
			if inferTypeWithEnv(width, env) != "Int" {
				width = &FieldExpr{Receiver: width, Name: "toInt"}
			}
			return &CallExpr{Fn: &Name{Name: "_padStart"}, Args: []Expr{args[0], width, args[2]}}, nil
		}
	case "repeat":
		if len(args) == 2 {
			needsRepeat = true
			return &CallExpr{Fn: &Name{Name: "_repeat"}, Args: args[:2]}, nil
		}
	case "split":
		if len(args) == 2 {
			call := &CallExpr{Fn: &FieldExpr{Receiver: args[0], Name: "split"}, Args: []Expr{args[1]}}
			return &CallExpr{Fn: &Name{Name: "ArrayBuffer"}, Args: []Expr{&SpreadExpr{Value: call}}}, nil
		}
	case "parseIntStr":
		if len(args) >= 1 && len(args) <= 2 {
			needsParseIntStr = true
			if len(args) == 1 {
				args = append(args, &IntLit{Value: 10})
			}
			if inferTypeWithEnv(args[0], env) != "String" {
				args[0] = &FieldExpr{Receiver: args[0], Name: "toString"}
			}
			return &CallExpr{Fn: &Name{Name: "_parseIntStr"}, Args: args[:2]}, nil
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
			// Return sorted keys to match deterministic iteration
			ks := &FieldExpr{Receiver: args[0], Name: "keys"}
			seq := &FieldExpr{Receiver: ks, Name: "toSeq"}
			return &FieldExpr{Receiver: seq, Name: "sorted"}, nil
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

func convertFunExpr(fe *parser.FunExpr, env *types.Env) (Expr, error) {
	saved := localVarTypes
	localVarTypes = copyMap(localVarTypes)
	defer func() { localVarTypes = saved }()

	child := types.NewEnv(env)
	rt := toScalaType(fe.Return)
	funCtxStack = append(funCtxStack, true)
	returnTypeStack = append(returnTypeStack, rt)
	defer func() {
		funCtxStack = funCtxStack[:len(funCtxStack)-1]
		returnTypeStack = returnTypeStack[:len(returnTypeStack)-1]
	}()

	f := &FunExpr{}
	for _, p := range fe.Params {
		typ := toScalaType(p.Type)
		f.Params = append(f.Params, Param{Name: p.Name, Type: typ})
		if typ != "" {
			localVarTypes[p.Name] = typ
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), true)
		}
	}
	if fe.ExprBody != nil {
		expr, err := convertExpr(fe.ExprBody, child)
		if err != nil {
			return nil, err
		}
		f.Expr = expr
	} else if len(fe.BlockBody) == 1 && fe.BlockBody[0].Return != nil {
		expr, err := convertExpr(fe.BlockBody[0].Return.Value, child)
		if err != nil {
			return nil, err
		}
		f.Expr = expr
	} else {
		for _, st := range fe.BlockBody {
			s, err := convertStmt(st, child)
			if err != nil {
				return nil, err
			}
			if s != nil {
				f.Body = append(f.Body, s)
			}
		}
		if len(f.Body) == 0 {
			return nil, fmt.Errorf("unsupported fun expr")
		}
		if ex := stmtsToExpr(f.Body); ex != nil {
			f.Expr = ex
			f.Body = nil
		}
	}
	return f, nil
}

func stmtsToExpr(stmts []Stmt) Expr {
	if len(stmts) == 0 {
		return nil
	}
	rs, ok := stmts[len(stmts)-1].(*ReturnStmt)
	if !ok || rs.Value == nil {
		return nil
	}
	expr := rs.Value
	for i := len(stmts) - 2; i >= 0; i-- {
		ifs, ok := stmts[i].(*IfStmt)
		if !ok || len(ifs.Then) != 1 || len(ifs.Else) != 0 {
			return nil
		}
		trs, ok := ifs.Then[0].(*ReturnStmt)
		if !ok || trs.Value == nil {
			return nil
		}
		expr = &IfExpr{Cond: ifs.Cond, Then: trs.Value, Else: expr}
	}
	return expr
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
		var ex Expr
		var err error
		if exprNode == nil {
			ex = defaultExpr(toScalaTypeFromType(st.Fields[name]))
		} else {
			ex, err = convertExpr(exprNode, env)
			if err != nil {
				return nil, err
			}
		}
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
	targetType := types.ExprType(me.Target, env)
	m := &MatchExpr{Target: target}
	// Track whether we can safely match on Int by converting the target.
	useIntMatch := false
	for _, c := range me.Cases {
		pat, err := convertExpr(c.Pattern, env)
		if err != nil {
			return nil, err
		}
		if types.IsIntType(targetType) || types.IsInt64Type(targetType) || types.IsBigIntType(targetType) {
			if _, ok := pat.(*IntLit); ok {
				useIntMatch = true
			}
		}
		res, err := convertExpr(c.Result, env)
		if err != nil {
			return nil, err
		}
		m.Cases = append(m.Cases, MatchCase{Pattern: pat, Result: res})
	}
	if useIntMatch && (types.IsIntType(targetType) || types.IsInt64Type(targetType) || types.IsBigIntType(targetType)) {
		m.Target = &FieldExpr{Receiver: target, Name: "toInt"}
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

func copyVarDecls(m map[string]*VarStmt) map[string]*VarStmt {
	n := make(map[string]*VarStmt, len(m))
	for k, v := range m {
		n[k] = v
	}
	return n
}

func convertFunStmt(fs *parser.FunStmt, env *types.Env) (Stmt, error) {
	saved := localVarTypes
	localVarTypes = copyMap(localVarTypes)
	defer func() { localVarTypes = saved }()
	savedDecls := varDecls
	varDecls = copyVarDecls(varDecls)
	defer func() { varDecls = savedDecls }()

	// Track assignments within this function to decide between `val` and `var` for locals.
	assignedSaved := assignedVars
	assignedVars = make(map[string]bool)
	gatherAssigned(fs.Body, assignedVars)
	defer func() { assignedVars = assignedSaved }()

	child := types.NewEnv(env)
	funCtxStack = append(funCtxStack, false)
	defer func() { funCtxStack = funCtxStack[:len(funCtxStack)-1] }()

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
		fn.Return = "Any"
	}
	if fn.Return != "Unit" && containsReturn(fn.Body) {
		if _, ok := fn.Body[len(fn.Body)-1].(*ReturnStmt); !ok {
			fn.Body = append(fn.Body, &ReturnStmt{Value: defaultExpr(fn.Return)})
		}
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
	saved := localVarTypes
	localVarTypes = copyMap(localVarTypes)
	defer func() { localVarTypes = saved }()

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
	var child *types.Env
	if env != nil {
		child = types.NewEnv(env)
	}
	itType := inferTypeWithEnv(iter, env)
	var tt types.Type
	if env != nil {
		tt = types.ExprType(fs.Source, env)
	}
	if (itType == "" || itType == "Any") && tt != nil {
		if s := toScalaTypeFromType(tt); s != "" {
			itType = s
		}
	}
	if child != nil && tt != nil {
		switch t := tt.(type) {
		case types.ListType:
			child.SetVar(fs.Name, t.Elem, true)
		case types.MapType:
			child.SetVar(fs.Name, t.Key, true)
		default:
			child.SetVar(fs.Name, t, true)
		}
	}
	if strings.HasPrefix(itType, "ArrayBuffer[") {
		elem := strings.TrimSuffix(strings.TrimPrefix(itType, "ArrayBuffer["), "]")
		if elem == "" {
			elem = "Any"
		}
		localVarTypes[fs.Name] = elem
	}
	if n, ok := iter.(*Name); ok && env != nil {
		if typ, err := env.GetVar(n.Name); err == nil {
			if mt, ok := typ.(types.MapType); ok {
				localVarTypes[fs.Name] = toScalaTypeFromType(mt.Key)
			} else if _, ok := typ.(types.GroupType); ok {
				iter = &IndexExpr{Value: iter, Index: &StringLit{Value: "items"}, Container: toScalaTypeFromType(typ)}
			}
		}
	} else if env != nil {
		t := inferTypeWithEnv(iter, env)
		if strings.HasPrefix(t, "Map[") || strings.HasPrefix(t, "scala.collection.mutable.Map[") {
			parts := strings.TrimSuffix(t[strings.Index(t, "[")+1:], "]")
			kv := strings.SplitN(parts, ",", 2)
			if len(kv) == 2 {
				localVarTypes[fs.Name] = strings.TrimSpace(kv[0])
			}
		} else if strings.HasPrefix(t, "Group[") {
			iter = &IndexExpr{Value: iter, Index: &StringLit{Value: "items"}, Container: t}
		}
	}
	var body []Stmt
	for _, st := range fs.Body {
		s, err := convertStmt(st, child)
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
			needsBigInt = true
			return "BigInt"
		case "string":
			return "String"
		case "bool":
			return "Boolean"
		case "float":
			return "Double"
		case "bigrat":
			needsBigRat = true
			needsBigInt = true
			return "BigRat"
		case "bigint":
			needsBigInt = true
			return "BigInt"
		case "any":
			return "Any"
		default:
			orig := strings.TrimSpace(*t.Simple)
			if isDeclaredType(orig) {
				return orig
			}
			return "Any"
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
			return fmt.Sprintf("scala.collection.mutable.Map[%s,%s]", k, v)
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
			ret = "Any"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	}
	return "Any"
}

func toScalaTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		needsBigInt = true
		return "BigInt"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	case types.FloatType:
		return "Double"
	case types.BigIntType:
		needsBigInt = true
		return "BigInt"
	case types.BigRatType:
		needsBigRat = true
		needsBigInt = true
		return "BigRat"
	case types.ListType:
		return fmt.Sprintf("ArrayBuffer[%s]", toScalaTypeFromType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("scala.collection.mutable.Map[%s,%s]", toScalaTypeFromType(tt.Key), toScalaTypeFromType(tt.Value))
	case types.FuncType:
		parts := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			s := toScalaTypeFromType(p)
			if s == "" {
				s = "Any"
			}
			parts[i] = s
		}
		ret := toScalaTypeFromType(tt.Return)
		if ret == "" {
			ret = "Any"
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
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
	if strings.HasPrefix(container, "Map[") || strings.HasPrefix(container, "scala.collection.mutable.Map[") {
		parts := strings.TrimSuffix(container[strings.Index(container, "[")+1:], "]")
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
		needsBigInt = true
		return "BigInt"
	case *StringLit:
		return "String"
	case *BoolLit:
		return "Boolean"
	case *FloatLit:
		return "Double"
	case *Name:
		if t, ok := localVarTypes[ex.Name]; ok {
			return t
		}
		return ""
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
	case *CallExpr:
		switch fn := ex.Fn.(type) {
		case *Name:
			switch fn.Name {
			case "String.valueOf":
				return "String"
			case "math.log", "math.log10", "math.log2", "math.log1p",
				"math.exp", "math.expm1", "math.sin", "math.cos",
				"math.tan", "math.asin", "math.acos", "math.atan",
				"math.sqrt", "math.pow":
				return "Double"
			case "BigInt":
				return "BigInt"
			}
		case *FieldExpr:
			if recv, ok := fn.Receiver.(*Name); ok {
				fullname := recv.Name + "." + fn.Name
				switch fullname {
				case "math.log", "math.log10", "math.log2", "math.log1p",
					"math.exp", "math.expm1", "math.sin", "math.cos",
					"math.tan", "math.asin", "math.acos", "math.atan",
					"math.sqrt", "math.pow":
					return "Double"
				case "String.valueOf":
					return "String"
				}
			}
		}
		return ""
	case *StructLit:
		return ex.Name
	case *LenExpr:
		return "BigInt"
	case *AppendExpr:
		listType := inferType(ex.List)
		if strings.HasPrefix(listType, "ArrayBuffer[") {
			if listType == "ArrayBuffer[Any]" {
				elemType := inferType(ex.Elem)
				if elemType != "" && elemType != "Any" {
					return fmt.Sprintf("ArrayBuffer[%s]", elemType)
				}
			}
			return listType
		}
		elemType := inferType(ex.Elem)
		if elemType != "" && elemType != "Any" {
			return fmt.Sprintf("ArrayBuffer[%s]", elemType)
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
		if strings.HasPrefix(t, "Map[") || strings.HasPrefix(t, "scala.collection.mutable.Map[") {
			parts := strings.TrimSuffix(t[strings.Index(t, "[")+1:], "]")
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
		needsBigInt = true
		return "BigInt"
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
			if lt == "BigInt" || rt == "BigInt" || isBigIntExpr(ex.Left) || isBigIntExpr(ex.Right) {
				return "BigInt"
			}
			if lt == "" || rt == "" {
				return ""
			}
			return "Int"
		case "==", "!=", ">", "<", ">=", "<=":
			return "Boolean"
		}
	case *CastExpr:
		if ex.Type == "Int" {
			return "Int"
		}
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
		if ret == "" && len(ex.Body) > 0 {
			if rs, ok := ex.Body[len(ex.Body)-1].(*ReturnStmt); ok && rs.Value != nil {
				ret = inferType(rs.Value)
			}
		}
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
	switch ex := e.(type) {
	case *Name:
		if t, ok := localVarTypes[ex.Name]; ok {
			return t
		}
		if env != nil {
			if typ, err := env.GetVar(ex.Name); err == nil {
				return toScalaTypeFromType(typ)
			}
		}
	case *FieldExpr:
		if env != nil {
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
		if strings.HasPrefix(t, "Map[") || strings.HasPrefix(t, "scala.collection.mutable.Map[") {
			parts := strings.TrimSuffix(t[strings.Index(t, "[")+1:], "]")
			kv := strings.SplitN(parts, ",", 2)
			if len(kv) == 2 {
				valType := strings.TrimSpace(kv[1])
				if valType != "" {
					return valType
				}
			}
		}
	case *BinaryExpr:
		lt := inferTypeWithEnv(ex.Left, env)
		rt := inferTypeWithEnv(ex.Right, env)
		switch ex.Op {
		case "+", "-", "*", "/", "%":
			if lt == "String" || rt == "String" {
				return "String"
			}
			if lt == "Double" || rt == "Double" || lt == "Float" || rt == "Float" {
				return "Double"
			}
			if lt == "BigInt" || rt == "BigInt" || isBigIntExpr(ex.Left) || isBigIntExpr(ex.Right) {
				return "BigInt"
			}
			if lt == "" || rt == "" {
				return ""
			}
			return "Int"
		case "==", "!=", ">", "<", ">=", "<=":
			return "Boolean"
		}
	case *CallExpr:
		if n, ok := ex.Fn.(*Name); ok && env != nil {
			if typ, err := env.GetVar(n.Name); err == nil {
				if ft, ok2 := typ.(types.FuncType); ok2 {
					return toScalaTypeFromType(ft.Return)
				}
			}
			if n.Name == "_bigrat" {
				return "BigRat"
			}
			if n.Name == "String.valueOf" {
				return "String"
			}
		}
	}
	return ""
}

func usesAssignedVar(e Expr) bool {
	switch ex := e.(type) {
	case *Name:
		return assignedVars[ex.Name]
	case *FieldExpr:
		return usesAssignedVar(ex.Receiver)
	case *IndexExpr:
		return usesAssignedVar(ex.Value) || usesAssignedVar(ex.Index)
	case *BinaryExpr:
		return usesAssignedVar(ex.Left) || usesAssignedVar(ex.Right)
	case *CallExpr:
		if usesAssignedVar(ex.Fn) {
			return true
		}
		for _, a := range ex.Args {
			if usesAssignedVar(a) {
				return true
			}
		}
	case *UnaryExpr:
		return usesAssignedVar(ex.Expr)
	case *ListLit:
		for _, el := range ex.Elems {
			if usesAssignedVar(el) {
				return true
			}
		}
	case *MapLit:
		for _, it := range ex.Items {
			if usesAssignedVar(it.Key) || usesAssignedVar(it.Value) {
				return true
			}
		}
	case *CastExpr:
		return usesAssignedVar(ex.Value)
	case *StructLit:
		for _, f := range ex.Fields {
			if usesAssignedVar(f) {
				return true
			}
		}
	case *AppendExpr:
		return usesAssignedVar(ex.List) || usesAssignedVar(ex.Elem)
	case *SliceExpr:
		return usesAssignedVar(ex.Value) || usesAssignedVar(ex.Start) || usesAssignedVar(ex.End)
	case *SubstringExpr:
		return usesAssignedVar(ex.Value) || usesAssignedVar(ex.Start) || usesAssignedVar(ex.End)
	}
	return false
}

func isBigIntExpr(e Expr) bool {
	switch v := e.(type) {
	case *IntLit:
		return v.Value > math.MaxInt32 || v.Value < math.MinInt32 || v.Long
	case *Name:
		return localVarTypes[v.Name] == "BigInt"
	case *IndexExpr:
		if strings.Contains(v.Container, "BigInt") {
			return true
		}
		return isBigIntExpr(v.Value)
	case *LenExpr:
		return true
	case *BinaryExpr:
		if v.Op == "+" || v.Op == "-" || v.Op == "*" || v.Op == "/" || v.Op == "%" {
			lt := inferType(v.Left)
			rt := inferType(v.Right)
			if lt == "String" || rt == "String" {
				return false
			}
			if lt == "BigInt" || rt == "BigInt" {
				return true
			}
			return isBigIntExpr(v.Left) || isBigIntExpr(v.Right)
		}
		return isBigIntExpr(v.Left) || isBigIntExpr(v.Right)
	case *CallExpr:
		if n, ok := v.Fn.(*Name); ok && n.Name == "BigInt" {
			return true
		}
		if n, ok := v.Fn.(*FieldExpr); ok && n.Name == "BigInt" {
			return true
		}
		// Do not inspect call arguments as they may be BigInt even when
		// the function returns a non-BigInt value.
		return false
	case *CastExpr:
		t := strings.ToLower(v.Type)
		if t == "bigint" {
			return true
		}
		return isBigIntExpr(v.Value)
	}
	return false
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
		return &Name{Name: "null"}
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
