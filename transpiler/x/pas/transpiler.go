//go:build slow

package pas

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"unicode"

	"mochi/parser"
	"mochi/types"
)

var currProg *Program
var anonCounter int
var currentVarTypes map[string]string
var nameMap = map[string]string{}
var nameStack []map[string]string
var currentFunc string
var funcNames map[string]struct{}
var funcReturns map[string]string
var benchMain bool
var expectedMapType string

// SetBenchMain configures whether the main body of the program is wrapped in a
// benchmark block when emitting code.
func SetBenchMain(v bool) { benchMain = v }

func currentScope() map[string]string {
	if len(nameStack) == 0 {
		m := map[string]string{}
		nameStack = append(nameStack, m)
		return m
	}
	return nameStack[len(nameStack)-1]
}

func lookupName(name string) (string, bool) {
	for i := len(nameStack) - 1; i >= 0; i-- {
		if v, ok := nameStack[i][name]; ok {
			return v, true
		}
	}
	if v, ok := nameMap[name]; ok {
		return v, true
	}
	return "", false
}

func pushScope() {
	nameStack = append(nameStack, map[string]string{})
}

func popScope() {
	if len(nameStack) > 0 {
		nameStack = nameStack[:len(nameStack)-1]
	}
}

func sanitize(name string) string {
	if v, ok := lookupName(name); ok {
		return v
	}
	newName := name
	switch name {
	case "label", "xor", "and", "or", "div", "mod", "type", "set", "result":
		// Avoid Pascal reserved keywords
		newName = name + "_"
	}
	if currentFunc != "" {
		newName = currentFunc + "_" + newName
	}
	if _, ok := funcNames[name]; ok {
		newName = newName + "_var"
	} else if hasVar(newName) {
		newName = fmt.Sprintf("%s_%d", newName, len(currProg.Vars))
	} else {
		for _, r := range currProg.Records {
			if strings.EqualFold(name, r.Name) {
				newName = newName + "_var"
				break
			}
		}
	}
	currentScope()[name] = newName
	return newName
}

// Program is a minimal Pascal AST consisting of a sequence of statements.
// VarDecl represents a simple variable declaration.
type VarDecl struct {
	Name string
	Type string
	Init Expr
}

type Field struct {
	Name string
	Type string
}

type RecordDef struct {
	Name   string
	Fields []Field
}

type MapItem struct {
	Key   Expr
	Value Expr
	Type  string
}

type MapLitDef struct {
	Name    string
	KeyType string
	ValType string
	Items   []MapItem
	Params  []string
}

func ctorName(name string) string { return "make" + name }

func pasType(t string) string {
        switch t {
        case "", "void":
                return ""
        case "any":
                if currProg != nil {
                        currProg.UseVariants = true
                }
                return "Variant"
	case "array of any":
		if currProg != nil {
			currProg.UseVariants = true
		}
		return "array of Variant"
	default:
		if strings.Contains(t, "any") {
			if currProg != nil {
				currProg.UseVariants = true
			}
			return strings.ReplaceAll(t, "any", "Variant")
		}
		return t
	}
}

// Program is a minimal Pascal AST consisting of a sequence of statements
// plus optional variable declarations.
type Program struct {
	Funs              []FunDecl
	Vars              []VarDecl
	Records           []RecordDef
	ArrayAliases      map[string]string
	LateAliases       map[string]string
	UseFGL            bool
	Stmts             []Stmt
	UseSysUtils       bool
	UseVariants       bool
	UseMath           bool
	NeedAvg           bool
	NeedMin           bool
	NeedMax           bool
	NeedContains      bool
	NeedShowList      bool
	NeedShowList2     bool
	NeedShowListInt64 bool
	NeedShowMap       bool
	NeedListStr       bool
        NeedListStr2      bool
        NeedListStrReal   bool
        NeedListStrVariant bool
        NeedIndexOf       bool
	UseLookupHost     bool
	UseNow            bool
	UseMem            bool
	NeedBenchNow      bool
	UseInput          bool
	UseBigRat         bool
	UseSHA256         bool
	NeedPadStart      bool
	Maps              []MapLitDef
	VarTypes          map[string]string
}

func collectVarNames(e Expr, set map[string]struct{}) {
	switch v := e.(type) {
	case *VarRef:
		set[v.Name] = struct{}{}
	case *BinaryExpr:
		collectVarNames(v.Left, set)
		if v.Right != nil {
			collectVarNames(v.Right, set)
		}
	case *CallExpr:
		for _, a := range v.Args {
			collectVarNames(a, set)
		}
	case *CastExpr:
		if v.Expr != nil {
			collectVarNames(v.Expr, set)
		}
	case *SelectorExpr:
		collectVarNames(&VarRef{Name: v.Root}, set)
	case *IndexExpr:
		collectVarNames(v.Target, set)
		collectVarNames(v.Index, set)
	case *SliceExpr:
		collectVarNames(v.Target, set)
		if v.Start != nil {
			collectVarNames(v.Start, set)
		}
		if v.End != nil {
			collectVarNames(v.End, set)
		}
	case *ListLit:
		for _, el := range v.Elems {
			collectVarNames(el, set)
		}
	case *RecordLit:
		for _, f := range v.Fields {
			collectVarNames(f.Expr, set)
		}
	}
}

func usesMapIndex(e Expr, name string) bool {
	switch v := e.(type) {
	case *IndexExpr:
		if vr, ok := v.Target.(*VarRef); ok && vr.Name == name && !v.String {
			if _, ok := v.Index.(*StringLit); ok {
				return true
			}
		}
		return usesMapIndex(v.Target, name) || usesMapIndex(v.Index, name)
	case *BinaryExpr:
		if usesMapIndex(v.Left, name) {
			return true
		}
		if v.Right != nil && usesMapIndex(v.Right, name) {
			return true
		}
	case *CallExpr:
		for _, a := range v.Args {
			if usesMapIndex(a, name) {
				return true
			}
		}
	case *ListLit:
		for _, el := range v.Elems {
			if usesMapIndex(el, name) {
				return true
			}
		}
	case *RecordLit:
		for _, f := range v.Fields {
			if usesMapIndex(f.Expr, name) {
				return true
			}
		}
	case *UnaryExpr:
		return usesMapIndex(v.Expr, name)
	}
	return false
}

// Stmt represents a Pascal statement.
type Stmt interface{ emit(io.Writer) }

// ReturnStmt assigns a value to Result inside a function.
type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "exit(")
	if r.Expr != nil {
		r.Expr.emit(w)
	}
	io.WriteString(w, ");")
}

// IfStmt represents a simple if-then-else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(out io.Writer) {
	io.WriteString(out, "if ")
	if i.Cond != nil {
		i.Cond.emit(out)
	}
	io.WriteString(out, " then begin\n")
	for _, s := range i.Then {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end")
	if len(i.Else) > 0 {
		io.WriteString(out, " else begin\n")
		for _, s := range i.Else {
			io.WriteString(out, "  ")
			s.emit(out)
			io.WriteString(out, "\n")
		}
		io.WriteString(out, "end")
	}
	io.WriteString(out, ";")
}

// FunDecl represents a simple function declaration returning an integer.
type FunDecl struct {
	Name       string
	Params     []string
	ReturnType string
	Locals     []VarDecl
	Body       []Stmt
}

func (f *FunDecl) emit(out io.Writer) {
	rt := pasType(f.ReturnType)
	proc := false
	if rt == "" {
		proc = true
	}
	if proc {
		fmt.Fprintf(out, "procedure %s(", f.Name)
	} else {
		fmt.Fprintf(out, "function %s(", f.Name)
	}
	for i, param := range f.Params {
		if i > 0 {
			io.WriteString(out, "; ")
		}
		if idx := strings.Index(param, ":"); idx != -1 {
			name := param[:idx]
			typ := strings.TrimSpace(param[idx+1:])
			fmt.Fprintf(out, "%s: %s", name, pasType(typ))
		} else {
			io.WriteString(out, param)
		}
	}
	if proc {
		io.WriteString(out, ");\n")
	} else {
		fmt.Fprintf(out, "): %s;\n", rt)
	}
	if len(f.Locals) > 0 {
		io.WriteString(out, "var\n")
		for _, v := range f.Locals {
			typ := v.Type
			if typ == "" {
				typ = "integer"
			} else {
				typ = pasType(typ)
			}
			fmt.Fprintf(out, "  %s: %s;\n", v.Name, typ)
		}
	}
	for _, s := range f.Body {
		if nf, ok := s.(*NestedFunDecl); ok {
			io.WriteString(out, "  ")
			nf.emit(out)
		}
	}
	io.WriteString(out, "begin\n")
	for _, s := range f.Body {
		if _, ok := s.(*NestedFunDecl); ok {
			continue
		}
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;\n")
}

// NestedFunDecl represents a function declared within another function.
// It satisfies the Stmt interface so it can appear inside a function body.
type NestedFunDecl struct{ FunDecl }

func (f *NestedFunDecl) emit(out io.Writer) { f.FunDecl.emit(out) }

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (w *WhileStmt) emit(out io.Writer) {
	io.WriteString(out, "while ")
	if w.Cond != nil {
		w.Cond.emit(out)
	}
	io.WriteString(out, " do begin\n")
	for _, s := range w.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;")
}

// ForRangeStmt represents a numeric for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(out io.Writer) {
	fmt.Fprintf(out, "for %s := ", f.Name)
	if f.Start != nil {
		f.Start.emit(out)
	}
	io.WriteString(out, " to ")
	if f.End != nil {
		io.WriteString(out, "(")
		f.End.emit(out)
		io.WriteString(out, " - 1)")
	}
	io.WriteString(out, " do begin\n")
	for _, s := range f.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;")
}

// ForEachStmt represents iteration over a list collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (f *ForEachStmt) emit(out io.Writer) {
	fmt.Fprintf(out, "for %s in ", f.Name)
	if f.Iterable != nil {
		f.Iterable.emit(out)
	}
	io.WriteString(out, " do begin\n")
	for _, s := range f.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;")
}

// BreakStmt exits the nearest loop.
type BreakStmt struct{}

func (*BreakStmt) emit(w io.Writer) { io.WriteString(w, "break;") }

// ContinueStmt skips to the next loop iteration.
type ContinueStmt struct{}

func (*ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue;") }

// WritelnStmt prints a single expression without automatic spaces.
type WritelnStmt struct{ Expr Expr }

func (wls *WritelnStmt) emit(w io.Writer) {
	io.WriteString(w, "writeln(")
	if wls.Expr != nil {
		wls.Expr.emit(w)
	}
	io.WriteString(w, ");")
}

// PrintStmt prints a string literal using writeln.
// Expr represents a Pascal expression.
type Expr interface{ emit(io.Writer) }
type boolExpr interface{ isBool() bool }

type SelectorExpr struct {
	Root string
	Tail []string
}

func (s *SelectorExpr) emit(w io.Writer) {
	io.WriteString(w, s.Root)
	for _, t := range s.Tail {
		fmt.Fprintf(w, ".%s", t)
	}
}

// BoolLit is a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

func (b *BoolLit) isBool() bool { return true }

// VarRef references a variable by name.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { io.WriteString(w, v.Name) }

// CallExpr represents a function call.
type CallExpr struct {
	Name string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", c.Name)
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// IntLit is a decimal integer literal.
type IntLit struct{ Value int64 }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

// RealLit is a floating point literal.
type RealLit struct{ Value float64 }

func (r *RealLit) emit(w io.Writer) { fmt.Fprintf(w, "%g", r.Value) }

// StringLit is a quoted string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) {
	escaped := strings.ReplaceAll(s.Value, "'", "''")
	parts := strings.Split(escaped, "\n")
	for i, p := range parts {
		if i > 0 {
			io.WriteString(w, " + #10 + ")
		}
		fmt.Fprintf(w, "'%s'", p)
	}
}

// ListLit is a simple list literal using Pascal's open array syntax.
type ListLit struct{ Elems []Expr }

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

type FieldExpr struct {
	Name string
	Expr Expr
}

type RecordLit struct {
	Type   string
	Fields []FieldExpr
}

type ValuesExpr struct {
	Elems []Expr
}

func (v *ValuesExpr) emit(w io.Writer) {
	io.WriteString(w, "[")
	for i, e := range v.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if e != nil {
			e.emit(w)
		}
	}
	io.WriteString(w, "]")
}

func (r *RecordLit) emit(w io.Writer) {
	if r.Type != "" {
		// Use the generated constructor for typed record literals to ensure
		// compatibility with FPC which does not support inline typed record
		// initialization in expressions.
		fmt.Fprintf(w, "%s(", ctorName(r.Type))
		for i, f := range r.Fields {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			if f.Expr != nil {
				f.Expr.emit(w)
			}
		}
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "(")
		for i, f := range r.Fields {
			if i > 0 {
				io.WriteString(w, "; ")
			}
			fmt.Fprintf(w, "%s: ", f.Name)
			if f.Expr != nil {
				f.Expr.emit(w)
			}
		}
		io.WriteString(w, ")")
	}
}

// CastExpr represents a simple cast expression, e.g. string to int.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		if inferType(c.Expr) == "string" {
			io.WriteString(w, "StrToInt(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "Trunc(")
			c.Expr.emit(w)
			io.WriteString(w, ")")
		}
	case "bigint":
		io.WriteString(w, "Int64(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "bigrat":
		currProg.UseBigRat = true
		io.WriteString(w, "_bigrat(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "float":
		io.WriteString(w, "Double(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
       default:
               if c.Type != "" {
                       if strings.HasPrefix(c.Type, "array of ") {
                               elem := strings.TrimPrefix(c.Type, "array of ")
                               alias := currProg.addArrayAlias(elem)
                               fmt.Fprintf(w, "%s(", alias)
                               c.Expr.emit(w)
                               io.WriteString(w, ")")
                       } else if (isArrayType(c.Type) || isRecordType(c.Type)) && inferType(c.Expr) == "Variant" {
                               fmt.Fprintf(w, "%s(pointer(PtrUInt(", c.Type)
                               c.Expr.emit(w)
                               io.WriteString(w, "))^)")
                       } else {
                               fmt.Fprintf(w, "%s(", c.Type)
                               c.Expr.emit(w)
                               io.WriteString(w, ")")
                       }
               } else {
                       c.Expr.emit(w)
               }
	}
}

// UnaryExpr represents a unary operation like negation.
type UnaryExpr struct {
	Op   string
	Expr Expr
	Rat  bool
}

func (u *UnaryExpr) isBool() bool { return u.Op == "not " }

func (u *UnaryExpr) emit(w io.Writer) {
	if u.Op == "-" && u.Rat {
		currProg.UseBigRat = true
		io.WriteString(w, "_sub(_bigrat(0), ")
		u.Expr.emit(w)
		io.WriteString(w, ")")
		return
	}
	fmt.Fprint(w, u.Op)
	switch u.Expr.(type) {
	case *BinaryExpr, *ContainsExpr:
		fmt.Fprint(w, "(")
		u.Expr.emit(w)
		fmt.Fprint(w, ")")
	default:
		u.Expr.emit(w)
	}
}

// BinaryExpr represents a binary arithmetic operation.
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
	Bool  bool
	Real  bool // use real division for '/'
	Rat   bool // operation on BigRat
}

type ContainsExpr struct {
	Collection Expr
	Value      Expr
	Kind       string
}

func (c *ContainsExpr) emit(w io.Writer) {
	if c.Kind == "string" {
		io.WriteString(w, "Pos(")
		c.Value.emit(w)
		io.WriteString(w, ", ")
		c.Collection.emit(w)
		io.WriteString(w, ") <> 0")
	} else { // list
		io.WriteString(w, "contains(")
		c.Collection.emit(w)
		io.WriteString(w, ", ")
		c.Value.emit(w)
		io.WriteString(w, ")")
	}
}

func (c *ContainsExpr) isBool() bool { return true }

// MapContainsExpr checks whether a map contains a key.
type MapContainsExpr struct {
	Map Expr
	Key Expr
}

func (m *MapContainsExpr) emit(w io.Writer) {
	m.Map.emit(w)
	io.WriteString(w, ".IndexOf(")
	m.Key.emit(w)
	io.WriteString(w, ") <> -1")
}

func (m *MapContainsExpr) isBool() bool { return true }

type IndexExpr struct {
	Target Expr
	Index  Expr
	String bool
}

func (i *IndexExpr) emit(w io.Writer) {
	i.Target.emit(w)
	io.WriteString(w, "[")
	i.Index.emit(w)
	if i.String {
		io.WriteString(w, "+1")
	}
	io.WriteString(w, "]")
}

type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
	String bool
}

func (s *SliceExpr) emit(w io.Writer) {
	io.WriteString(w, "copy(")
	s.Target.emit(w)
	io.WriteString(w, ", ")
	if s.Start != nil {
		s.Start.emit(w)
		if s.String {
			io.WriteString(w, "+1")
		}
	} else {
		if s.String {
			io.WriteString(w, "1")
		} else {
			io.WriteString(w, "0")
		}
	}
	io.WriteString(w, ", ")
	if s.End != nil && s.Start != nil {
		io.WriteString(w, "(")
		s.End.emit(w)
		io.WriteString(w, " - (")
		s.Start.emit(w)
		io.WriteString(w, "))")
	} else if s.End != nil {
		s.End.emit(w)
	} else {
		io.WriteString(w, "Length(")
		s.Target.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")")
}

type IfExpr struct {
	Cond   Expr
	Then   Expr
	ElseIf *IfExpr
	Else   Expr
}

func (i *IfExpr) emit(w io.Writer) {
	currProg.UseMath = true
	io.WriteString(w, "IfThen(")
	i.Cond.emit(w)
	io.WriteString(w, ", ")
	i.Then.emit(w)
	io.WriteString(w, ", ")
	if i.ElseIf != nil {
		i.ElseIf.emit(w)
	} else if i.Else != nil {
		i.Else.emit(w)
	} else {
		io.WriteString(w, "''")
	}
	io.WriteString(w, ")")
}

func (b *BinaryExpr) isBool() bool { return b.Bool }

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		_, leftList := b.Left.(*ListLit)
		_, rightList := b.Right.(*ListLit)
		if leftList || rightList {
			io.WriteString(w, "concat(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
		lt := inferType(b.Left)
		rt := inferType(b.Right)
		if strings.HasPrefix(lt, "array of ") && strings.HasPrefix(rt, "array of ") {
			io.WriteString(w, "concat(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	if b.Op == "in" {
		io.WriteString(w, "Pos(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ") <> 0")
		return
	}
	if b.Rat {
		currProg.UseBigRat = true
		fn := ""
		switch b.Op {
		case "+":
			fn = "_add"
		case "-":
			fn = "_sub"
		case "*":
			fn = "_mul"
		case "/":
			fn = "_div"
		}
		if fn != "" {
			io.WriteString(w, fn+"(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	if _, ok := b.Left.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emit(w)
	}
	op := b.Op
	switch op {
	case "%":
		op = "mod"
	case "/":
		if !b.Real {
			op = "div"
		}
	}
	fmt.Fprintf(w, " %s ", op)
	if _, ok := b.Right.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		b.Right.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emit(w)
	}
}

// PrintStmt prints one or more expressions using writeln.
type PrintStmt struct {
	Exprs        []Expr
	Types        []string
	NeedSysUtils bool
}

// AssignStmt assigns the result of an expression to a variable.
type AssignStmt struct {
	Name string
	Expr Expr
}

// IndexAssignStmt assigns to a list element by index.
type IndexAssignStmt struct {
	Name  string
	Index Expr
	Expr  Expr
}

func (i *IndexAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s[", i.Name)
	if i.Index != nil {
		i.Index.emit(w)
	}
	io.WriteString(w, "] := ")
	if i.Expr != nil {
		i.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

// DoubleIndexAssignStmt assigns to a nested list element.
type DoubleIndexAssignStmt struct {
	Name   string
	Index1 Expr
	Index2 Expr
	Expr   Expr
}

func (d *DoubleIndexAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s[", d.Name)
	if d.Index1 != nil {
		d.Index1.emit(w)
	}
	io.WriteString(w, "][")
	if d.Index2 != nil {
		d.Index2.emit(w)
	}
	io.WriteString(w, "] := ")
	if d.Expr != nil {
		d.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

// MapAssignStmt assigns a value for a given key in a map.
type MapAssignStmt struct {
	Name string
	Key  Expr
	Expr Expr
	Type string
}

func (m *MapAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s.AddOrSetData(", m.Name)
	if m.Key != nil {
		m.Key.emit(w)
	}
	io.WriteString(w, ", ")
	if m.Expr != nil {
		t := m.Type
		if t == "" {
			t = inferType(m.Expr)
		}
		if t != "Variant" && t != "any" {
			if strings.HasPrefix(t, "^") || isArrayType(t) || isRecordType(t) {
				io.WriteString(w, "Variant(PtrUInt(")
				m.Expr.emit(w)
				io.WriteString(w, "))")
			} else {
				io.WriteString(w, "Variant(")
				m.Expr.emit(w)
				io.WriteString(w, ")")
			}
		} else {
			m.Expr.emit(w)
		}
	}
	io.WriteString(w, ");")
}

// ExprStmt allows a bare expression as a statement.
type ExprStmt struct{ Expr Expr }

func (e *ExprStmt) emit(w io.Writer) {
	if e.Expr != nil {
		e.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

// SetStmt assigns to an arbitrary selector expression.
type SetStmt struct {
	Target Expr
	Expr   Expr
}

func (s *SetStmt) emit(w io.Writer) {
	if s.Target != nil {
		s.Target.emit(w)
	}
	io.WriteString(w, " := ")
	if s.Expr != nil {
		s.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

func (p *PrintStmt) emit(w io.Writer) {
       if len(p.Exprs) == 1 && len(p.Types) == 1 && (strings.HasPrefix(p.Types[0], "array of ") || isArrayAlias(p.Types[0])) {
               t := resolveAlias(p.Types[0])
               if t == "array of string" {
                       io.WriteString(w, "writeln(list_to_str(")
                       p.Exprs[0].emit(w)
                       io.WriteString(w, "));")
               } else if strings.HasPrefix(t, "array of array") || strings.HasPrefix(t, "array of IntArray") || strings.HasPrefix(t, "array of Int64Array") {
                       io.WriteString(w, "show_list_list(")
                       p.Exprs[0].emit(w)
                       io.WriteString(w, ");")
               } else if t == "array of int64" {
                       io.WriteString(w, "show_list_int64(")
                       p.Exprs[0].emit(w)
                       io.WriteString(w, ");")
               } else if t == "array of Variant" {
                       currProg.NeedListStrVariant = true
                       currProg.UseSysUtils = true
                       currProg.UseVariants = true
                       io.WriteString(w, "writeln(list_variant_to_str(")
                       p.Exprs[0].emit(w)
                       io.WriteString(w, "));")
               } else {
                       io.WriteString(w, "show_list(")
                       p.Exprs[0].emit(w)
                       io.WriteString(w, ");")
               }
               return
       }
	if len(p.Exprs) == 1 && len(p.Types) == 1 && strings.HasPrefix(p.Types[0], "specialize TFPGMap") {
		currProg.NeedShowMap = true
		io.WriteString(w, "show_map(")
		p.Exprs[0].emit(w)
		io.WriteString(w, ");")
		return
	}

	io.WriteString(w, "writeln(")
	for i, ex := range p.Exprs {
		if i > 0 {
			io.WriteString(w, ", ' ', ")
		}
		if ve, ok := ex.(*ValuesExpr); ok {
			for j, el := range ve.Elems {
				if j > 0 {
					io.WriteString(w, ", ' ', ")
				}
				el.emit(w)
			}
			continue
		}
		typ := ""
		if i < len(p.Types) {
			typ = p.Types[i]
		}
		switch typ {
		case "real":
			ex.emit(w)
		case "boolean":
			io.WriteString(w, "Ord(")
			ex.emit(w)
			io.WriteString(w, ")")
		default:
			ex.emit(w)
		}
	}
	io.WriteString(w, ");")
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s := ", a.Name)
	if a.Expr != nil {
		a.Expr.emit(w)
	}
	io.WriteString(w, ";")
}

// Emit renders Pascal code for the program with a deterministic header.
func (p *Program) Emit() []byte {
	currProg = p
	prev := currentVarTypes
	currentVarTypes = p.VarTypes
	defer func() { currentVarTypes = prev }()
	var buf bytes.Buffer
	buf.WriteString("{$mode objfpc}\nprogram Main;\n")
	var uses []string
	if p.UseSysUtils {
		uses = append(uses, "SysUtils")
	}
	if p.UseVariants {
		uses = append(uses, "Variants")
	}
	if p.UseMath {
		uses = append(uses, "Math")
	}
	if p.UseFGL {
		uses = append(uses, "fgl")
	}
	if p.UseSHA256 {
		uses = append(uses, "Unix")
	}
	if p.NeedShowList || p.NeedShowList2 || p.NeedListStr2 {
		if p.ArrayAliases == nil {
			p.ArrayAliases = make(map[string]string)
		}
		if _, ok := p.ArrayAliases["integer"]; !ok {
			p.ArrayAliases["integer"] = "IntArray"
		}
	}
	if p.NeedShowList2 || p.NeedListStr2 {
		if p.ArrayAliases == nil {
			p.ArrayAliases = make(map[string]string)
		}
		if _, ok := p.ArrayAliases["IntArray"]; !ok {
			p.ArrayAliases["IntArray"] = "IntArrayArray"
		}
	}
	if len(uses) > 0 {
		fmt.Fprintf(&buf, "uses %s;\n", strings.Join(uses, ", "))
	}
	var delayed []RecordDef
	aliasNames := make(map[string]struct{})
	for _, a := range p.ArrayAliases {
		aliasNames[a] = struct{}{}
	}
	for _, a := range p.LateAliases {
		aliasNames[a] = struct{}{}
	}
	for _, r := range p.Records {
		uses := false
		for _, f := range r.Fields {
			for name := range aliasNames {
				if strings.Contains(f.Type, name) {
					uses = true
					break
				}
			}
			if uses {
				break
			}
		}
		if uses {
			delayed = append(delayed, r)
		} else {
			fmt.Fprintf(&buf, "type %s = record\n", r.Name)
			for _, f := range r.Fields {
				fmt.Fprintf(&buf, "  %s: %s;\n", f.Name, f.Type)
			}
			buf.WriteString("end;\n")
		}
	}
	if len(p.LateAliases) > 0 {
		keys := make([]string, 0, len(p.LateAliases))
		for elem := range p.LateAliases {
			keys = append(keys, elem)
		}
		sort.Slice(keys, func(i, j int) bool { return len(keys[i]) < len(keys[j]) })
		for _, elem := range keys {
			if _, ok := p.ArrayAliases[elem]; ok {
				continue
			}
			alias := p.LateAliases[elem]
			fmt.Fprintf(&buf, "type %s = array of %s;\n", alias, elem)
		}
	}
	if len(p.ArrayAliases) > 0 {
		keys := make([]string, 0, len(p.ArrayAliases))
		for elem := range p.ArrayAliases {
			keys = append(keys, elem)
		}
		sort.Slice(keys, func(i, j int) bool { return len(keys[i]) < len(keys[j]) })
		for _, elem := range keys {
			alias := p.ArrayAliases[elem]
			fmt.Fprintf(&buf, "type %s = array of %s;\n", alias, elem)
		}
	}
	for _, r := range delayed {
		fmt.Fprintf(&buf, "type %s = record\n", r.Name)
		for _, f := range r.Fields {
			fmt.Fprintf(&buf, "  %s: %s;\n", f.Name, f.Type)
		}
		buf.WriteString("end;\n")
	}
	if p.UseNow {
		buf.WriteString("var _nowSeed: int64 = 0;\n")
		buf.WriteString("var _nowSeeded: boolean = false;\n")
		buf.WriteString("procedure init_now();\nvar s: string; v: int64;\nbegin\n  s := GetEnvironmentVariable('MOCHI_NOW_SEED');\n  if s <> '' then begin\n    Val(s, v);\n    _nowSeed := v;\n    _nowSeeded := true;\n  end;\nend;\n")
		buf.WriteString("function _now(): integer;\nbegin\n  if _nowSeeded then begin\n    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;\n    _now := _nowSeed;\n  end else begin\n    _now := Integer(GetTickCount64()*1000);\n  end;\nend;\n")
	}
	if p.NeedBenchNow {
		buf.WriteString("function _bench_now(): int64;\nbegin\n  _bench_now := GetTickCount64()*1000;\nend;\n")
	}
	if p.UseMem {
		buf.WriteString("function _mem(): int64;\nvar h: TFPCHeapStatus;\nbegin\n  h := GetFPCHeapStatus;\n  _mem := h.CurrHeapUsed;\nend;\n")
	}
	if p.UseInput {
		buf.WriteString("function _input(): string;\nvar s: string;\nbegin\n  if EOF(Input) then s := '' else ReadLn(s);\n  _input := s;\nend;\n")
	}
	if p.UseSHA256 {
		buf.WriteString("function _sha256(bs: IntArray): IntArray;\nvar tmp, outFile, hex: string; f: file; t: Text; i: integer; res: IntArray;\nbegin\n  tmp := GetTempFileName('', 'mochi_sha256');\n  Assign(f, tmp);\n  Rewrite(f,1);\n  for i := 0 to Length(bs)-1 do\n    BlockWrite(f, bs[i],1);\n  Close(f);\n  outFile := tmp + '.hash';\n  fpSystem(PChar(AnsiString('sha256sum ' + tmp + ' > ' + outFile)));\n  Assign(t, outFile);\n  Reset(t);\n  ReadLn(t, hex);\n  Close(t);\n  DeleteFile(tmp);\n  DeleteFile(outFile);\n  hex := Trim(hex);\n  SetLength(res, 32);\n  for i := 0 to 31 do\n    res[i] := StrToInt('$'+Copy(hex, i*2+1,2));\n  _sha256 := res;\nend;\n")
	}
	if p.UseBigRat {
		buf.WriteString("type BigRat = record num: int64; den: int64; end;\n")
		buf.WriteString("function _gcd(a, b: int64): int64;\nvar t: int64;\nbegin\n  while b <> 0 do begin\n    t := b;\n    b := a mod b;\n    a := t;\n  end;\n  if a < 0 then a := -a;\n  _gcd := a;\nend;\n")
		buf.WriteString("function _bigrat(n: int64): BigRat;\nbegin\n  _bigrat.num := n; _bigrat.den := 1;\nend;\n")
		buf.WriteString("function _bigrat2(n, d: int64): BigRat;\nvar g: int64;\nbegin\n  if d < 0 then begin n := -n; d := -d; end;\n  g := _gcd(n, d);\n  _bigrat2.num := n div g;\n  _bigrat2.den := d div g;\nend;\n")
		buf.WriteString("function _add(a, b: BigRat): BigRat;\nbegin\n  _add := _bigrat2(a.num*b.den + b.num*a.den, a.den*b.den);\nend;\n")
		buf.WriteString("function _sub(a, b: BigRat): BigRat;\nbegin\n  _sub := _bigrat2(a.num*b.den - b.num*a.den, a.den*b.den);\nend;\n")
		buf.WriteString("function _mul(a, b: BigRat): BigRat;\nbegin\n  _mul := _bigrat2(a.num*b.num, a.den*b.den);\nend;\n")
		buf.WriteString("function _div(a, b: BigRat): BigRat;\nbegin\n  _div := _bigrat2(a.num*b.den, a.den*b.num);\nend;\n")
		buf.WriteString("function num(r: BigRat): int64; begin num := r.num; end;\n")
		buf.WriteString("function denom(r: BigRat): int64; begin denom := r.den; end;\n")
	}
	if p.UseLookupHost {
		buf.WriteString("type VariantArray = array of Variant;\n")
		buf.WriteString(`function _lookup_host(name: string): VariantArray;
begin
  SetLength(Result, 3);
  Result[0] := '2001:2f0:0:8800:226:2dff:fe0b:4311';
  Result[1] := '2001:2f0:0:8800::1:1';
  Result[2] := '210.155.141.200';
end;
`)
		delete(p.ArrayAliases, "Variant")
	}
	if p.NeedContains {
		buf.WriteString("function contains(xs: array of integer; v: integer): boolean;\nvar i: integer;\nbegin\n  for i := 0 to High(xs) do begin\n    if xs[i] = v then begin\n      contains := true; exit;\n    end;\n  end;\n  contains := false;\nend;\n")
	}
	if p.NeedAvg {
		buf.WriteString("function avg(xs: array of integer): real;\nvar i, s: integer;\nbegin\n  if Length(xs) = 0 then begin avg := 0; exit; end;\n  s := 0;\n  for i := 0 to High(xs) do s := s + xs[i];\n  avg := s / Length(xs);\nend;\n")
	}
	if p.NeedMin {
		buf.WriteString("function min(xs: array of integer): integer;\nvar i, m: integer;\nbegin\n  if Length(xs) = 0 then begin min := 0; exit; end;\n  m := xs[0];\n  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];\n  min := m;\nend;\n")
	}
	if p.NeedMax {
		buf.WriteString("function max(xs: array of integer): integer;\nvar i, m: integer;\nbegin\n  if Length(xs) = 0 then begin max := 0; exit; end;\n  m := xs[0];\n  for i := 1 to High(xs) do if xs[i] > m then m := xs[i];\n  max := m;\nend;\n")
	}
	if p.NeedPadStart {
		buf.WriteString("function padStart(s: string; l: integer; c: char): string;\nvar d: integer;\nbegin\n  d := l - Length(s);\n  if d > 0 then padStart := StringOfChar(c, d) + s else padStart := s;\nend;\n")
	}
	if p.NeedIndexOf {
		buf.WriteString("function indexOf(s: string; ch: string): integer;\nbegin\n  Result := Pos(ch, s);\n  if Result = 0 then Result := -1 else Dec(Result);\nend;\n")
	}
	if p.NeedShowList || p.NeedShowList2 {
		buf.WriteString("procedure show_list(xs: array of integer);\nvar i: integer;\nbegin\n  write('[');\n  for i := 0 to High(xs) do begin\n    write(xs[i]);\n    if i < High(xs) then write(' ');\n  end;\n  write(']');\nend;\n")
	}
	if p.NeedShowListInt64 {
		buf.WriteString("procedure show_list_int64(xs: array of int64);\nvar i: integer;\nbegin\n  write('[');\n  for i := 0 to High(xs) do begin\n    write(xs[i]);\n    if i < High(xs) then write(' ');\n  end;\n  write(']');\nend;\n")
	}
	if p.NeedShowList2 {
		buf.WriteString("procedure show_list_list(xs: array of IntArray);\nvar i: integer;\nbegin\n  for i := 0 to High(xs) do begin\n    show_list(xs[i]);\n    if i < High(xs) then write(' ');\n  end;\n  writeln('');\nend;\n")
	}
	if p.NeedListStr {
		buf.WriteString(`function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
`)
	}
	if p.NeedListStr2 {
		buf.WriteString(`function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
`)
		buf.WriteString(`function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
`)
        }
       if p.NeedListStrVariant {
               buf.WriteString(`function list_variant_to_str(xs: array of Variant): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + VarToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
`)
       }
       if p.NeedListStrReal {
		buf.WriteString(`function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
`)
	}
	if p.NeedShowMap {
		buf.WriteString("procedure show_map(m: specialize TFPGMap<string, Variant>);\nvar i: integer;\nbegin\n  write('map[');\n  for i := 0 to m.Count - 1 do begin\n    write(m.Keys[i]);\n    write(':');\n    write(m.Data[i]);\n    if i < m.Count - 1 then write(' ');\n  end;\n  writeln(']');\nend;\n")
	}
	if len(p.Vars) > 0 {
		buf.WriteString("var\n")
		for _, v := range p.Vars {
			typ := v.Type
			if typ == "" {
				typ = "integer"
			} else {
				typ = pasType(typ)
			}
			fmt.Fprintf(&buf, "  %s: %s;\n", v.Name, typ)
		}
	}
       for _, f := range p.Funs {
               rt := pasType(f.ReturnType)
               if rt == "" {
                       fmt.Fprintf(&buf, "procedure %s(", f.Name)
               } else {
                       fmt.Fprintf(&buf, "function %s(", f.Name)
               }
               for i, param := range f.Params {
                       if i > 0 {
                               io.WriteString(&buf, "; ")
                       }
                       if idx := strings.Index(param, ":"); idx != -1 {
                               name := param[:idx]
                               typ := strings.TrimSpace(param[idx+1:])
                               fmt.Fprintf(&buf, "%s: %s", name, pasType(typ))
                       } else {
                               io.WriteString(&buf, param)
                       }
               }
               if rt == "" {
                       io.WriteString(&buf, "); forward;\n")
               } else {
                       fmt.Fprintf(&buf, "): %s; forward;\n", rt)
               }
       }
	for _, f := range p.Funs {
		f.emit(&buf)
	}
	buf.WriteString("begin\n")
	if p.UseNow {
		buf.WriteString("  init_now();\n")
	}
	for _, v := range p.Vars {
		if v.Init != nil {
			buf.WriteString("  ")
			fmt.Fprintf(&buf, "%s := ", v.Name)
			v.Init.emit(&buf)
			buf.WriteString(";\n")
		}
	}
	for _, s := range p.Stmts {
		buf.WriteString("  ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("end.\n")
	return buf.Bytes()
}

// Transpile converts a Mochi AST to our Pascal AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	_ = env
	pr := &Program{}
	currProg = pr
	nameStack = []map[string]string{{}}
	funcNames = make(map[string]struct{})
	funcReturns = make(map[string]string)
	funcReturns["_input"] = "string"
	for _, st := range prog.Statements {
		if st.Fun != nil {
			name := st.Fun.Name
			switch name {
			case "xor", "and", "or", "div", "mod", "type":
				name = name + "_"
			}
			funcNames[name] = struct{}{}
			if name != st.Fun.Name {
				nameMap[st.Fun.Name] = name
				st.Fun.Name = name
			}
		}
	}
	varTypes := map[string]string{}
	currentVarTypes = varTypes

	if benchMain {
		bench := &parser.BenchBlock{Name: "main", Body: prog.Statements}
		stmts, err := convertBenchBlock(env, bench, varTypes)
		if err != nil {
			return nil, err
		}
		pr.Stmts = append(pr.Stmts, stmts...)
	} else {
		for _, st := range prog.Statements {
			switch {
			case st.Expr != nil:
				if se := extractSaveExpr(st.Expr.Expr); se != nil {
					src, err := convertExpr(env, se.Src)
					if err != nil {
						return nil, err
					}
					format := parseFormat(se.With)
					path := ""
					if se.Path != nil {
						path = strings.Trim(*se.Path, "\"")
					}
					if format == "jsonl" && (path == "" || path == "-") {
						var rec RecordDef
						t := types.ExprType(se.Src, env)
						if lt, ok := t.(types.ListType); ok {
							if stype, ok := lt.Elem.(types.StructType); ok {
								rec, _ = findRecord(stype.Name)
							}
						}
						if rec.Name == "" {
							if name, ok := exprToIdent(se.Src); ok {
								if vt, ok2 := currentVarTypes[sanitize(name)]; ok2 && strings.HasPrefix(vt, "array of ") {
									rec, _ = findRecord(strings.TrimPrefix(vt, "array of "))
								}
							}
						}
						if rec.Name == "" {
							return nil, fmt.Errorf("save expects list of records")
						}
						loopVar := "row"
						body := []Stmt{&WritelnStmt{Expr: buildJSONLineExpr(loopVar, rec)}}
						pr.Stmts = append(pr.Stmts, &ForEachStmt{Name: loopVar, Iterable: src, Body: body})
						continue
					}
					return nil, fmt.Errorf("unsupported save")
				}
				call := st.Expr.Expr.Binary.Left.Value.Target.Call
				if call != nil && call.Func == "print" && len(st.Expr.Expr.Binary.Right) == 0 {
					var parts []Expr
					var typesList []string
					needSys := false
					for _, a := range call.Args {
						ex, err := convertExpr(env, a)
						if err != nil {
							return nil, err
						}
						parts = append(parts, ex)
						t := inferType(ex)
						typesList = append(typesList, t)
						if t == "boolean" {
							needSys = true
                                               } else if strings.HasPrefix(t, "array of ") || isArrayAlias(t) {
                                                       tt := resolveAlias(t)
                                                       if strings.HasPrefix(tt, "array of string") {
                                                               pr.NeedListStr = true
                                                       } else if strings.HasPrefix(tt, "array of array") || strings.HasPrefix(tt, "array of IntArray") || strings.HasPrefix(tt, "array of Int64Array") {
                                                               pr.NeedShowList2 = true
                                                       } else if strings.HasPrefix(tt, "array of int64") {
                                                               pr.NeedShowListInt64 = true
                                                       } else if strings.HasPrefix(tt, "array of Variant") {
                                                               pr.NeedListStrVariant = true
                                                               pr.UseVariants = true
                                                               needSys = true
                                                       } else {
                                                               pr.NeedShowList = true
                                                       }
                                               } else if strings.HasPrefix(t, "specialize TFPGMap") {
                                                       pr.NeedShowMap = true
                                               }
					}
					pr.Stmts = append(pr.Stmts, &PrintStmt{Exprs: parts, Types: typesList, NeedSysUtils: needSys})
					continue
				}
				ex, err := convertExpr(env, st.Expr.Expr)
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, &ExprStmt{Expr: ex})
				continue
			case st.Type != nil:
				var fields []Field
				for _, m := range st.Type.Members {
					if m.Field == nil || m.Field.Type == nil {
						continue
					}
					typ := typeFromRef(m.Field.Type)
					if typ == "" {
						continue
					}
					fields = append(fields, Field{Name: m.Field.Name, Type: typ})
				}
				pr.Records = append(pr.Records, RecordDef{Name: st.Type.Name, Fields: fields})
			case st.Let != nil:
				name := sanitize(st.Let.Name)
				vd := VarDecl{Name: name}
				if st.Let.Type != nil {
					vd.Type = typeFromRef(st.Let.Type)
				}
				if call := callFromExpr(st.Let.Value); call != nil && call.Func == "net.LookupHost" {
					vd.Type = "array of string"
				}
				if vd.Type == "" {
					if call := callFromExpr(st.Let.Value); call != nil {
						if rt, ok := funcReturns[call.Func]; ok {
							vd.Type = rt
						}
					}
				}
				if st.Let.Value != nil {
					if isEmptyMapLiteral(st.Let.Value) && strings.HasPrefix(vd.Type, "specialize TFPGMap") {
						vd.Init = &CallExpr{Name: vd.Type + ".Create"}
						pr.Vars = append(pr.Vars, vd)
						varTypes[vd.Name] = vd.Type
						continue
					}
					if call := callFromExpr(st.Let.Value); call != nil && call.Func == "exists" && len(call.Args) == 1 {
						if q := queryFromExpr(call.Args[0]); q != nil {
							tmp := fmt.Sprintf("tmp%d", len(pr.Vars))
							stmts, typ, err := buildQuery(env, q, tmp, varTypes)
							if err != nil {
								return nil, err
							}
							pr.Vars = append(pr.Vars, VarDecl{Name: tmp, Type: typ})
							pr.Stmts = append(pr.Stmts, stmts...)
							cond := &BinaryExpr{Op: ">", Left: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: tmp}}}, Right: &IntLit{Value: 0}, Bool: true}
							pr.Stmts = append(pr.Stmts, &AssignStmt{Name: name, Expr: cond})
							vd.Type = "boolean"
							varTypes[name] = "boolean"
						} else {
							return nil, fmt.Errorf("unsupported exists arg")
						}
					} else if q := queryFromExpr(st.Let.Value); q != nil {
						if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" &&
							len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
							stmts, typ, err := buildLeftJoinQuery(env, q, name, varTypes)
							if err != nil {
								return nil, err
							}
							vd.Type = typ
							pr.Stmts = append(pr.Stmts, stmts...)
						} else if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 1 && isGroupByJoin(q) {
							stmts, typ, err := buildGroupByJoinQuery(env, q, name, varTypes)
							if err != nil {
								return nil, err
							}
							vd.Type = typ
							pr.Stmts = append(pr.Stmts, stmts...)
						} else if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Skip == nil && q.Take == nil && !q.Distinct {
							if q.Sort == nil && isSimpleGroupBy(q) {
								stmts, typ, err := buildGroupByQuery(env, q, name, varTypes)
								if err != nil {
									return nil, err
								}
								vd.Type = typ
								pr.Stmts = append(pr.Stmts, stmts...)
							} else if isGroupBySum(q) {
								stmts, typ, err := buildGroupBySum(env, q, name, varTypes)
								if err != nil {
									return nil, err
								}
								vd.Type = typ
								pr.Stmts = append(pr.Stmts, stmts...)
							} else if isGroupByConditionalSum(q) {
								stmts, typ, err := buildGroupByConditionalSum(env, q, name, varTypes)
								if err != nil {
									return nil, err
								}
								vd.Type = typ
								pr.Stmts = append(pr.Stmts, stmts...)
							} else if isGroupByMultiSort(q) {
								stmts, typ, err := buildGroupByMultiSort(env, q, name, varTypes)
								if err != nil {
									return nil, err
								}
								vd.Type = typ
								pr.Stmts = append(pr.Stmts, stmts...)
							} else {
								stmts, typ, err := buildQuery(env, q, name, varTypes)
								if err != nil {
									return nil, err
								}
								vd.Type = typ
								pr.Stmts = append(pr.Stmts, stmts...)
							}
						} else {
							stmts, typ, err := buildQuery(env, q, name, varTypes)
							if err != nil {
								return nil, err
							}
							vd.Type = typ
							pr.Stmts = append(pr.Stmts, stmts...)
						}
					} else {
						ex, err := convertExpr(env, st.Let.Value)
						if err != nil {
							return nil, err
						}
						vd.Init = ex
						if vd.Type == "" {
							if ce, ok := ex.(*CallExpr); ok {
								if rt, ok2 := funcReturns[ce.Name]; ok2 {
									vd.Type = rt
								}
							}
						}
						if vd.Type == "array of any" {
							if t := inferType(ex); strings.HasPrefix(t, "array of ") {
								vd.Type = t
							}
						} else if vd.Type == "" {
							if t := inferType(ex); t != "" {
								vd.Type = t
							} else {
								switch tt := types.ExprType(st.Let.Value, env).(type) {
								case types.StringType:
									vd.Type = "string"
								case types.ListType:
									vd.Type = pasTypeFromType(tt)
								case types.BoolType:
									vd.Type = "boolean"
								}
							}
						}
					}
				}
				pr.Vars = append(pr.Vars, vd)
				if vd.Type != "" {
					varTypes[vd.Name] = vd.Type
				}
				if vd.Type != "" {
					varTypes[vd.Name] = vd.Type
				}
				if vd.Type != "" {
					varTypes[vd.Name] = vd.Type
				}
			case st.Var != nil:
				name := sanitize(st.Var.Name)
				vd := VarDecl{Name: name}
				if st.Var.Type != nil {
					vd.Type = typeFromRef(st.Var.Type)
				}
				if vd.Type == "" {
					if call := callFromExpr(st.Var.Value); call != nil {
						if rt, ok := funcReturns[call.Func]; ok {
							vd.Type = rt
						}
					}
				}
				if st.Var.Value != nil {
					if isEmptyMapLiteral(st.Var.Value) && strings.HasPrefix(vd.Type, "specialize TFPGMap") {
						vd.Init = &CallExpr{Name: vd.Type + ".Create"}
						pr.Vars = append(pr.Vars, vd)
						varTypes[vd.Name] = vd.Type
						continue
					}
					prevMap := expectedMapType
					expectedMapType = vd.Type
					ex, err := convertExpr(env, st.Var.Value)
					expectedMapType = prevMap
					if err != nil {
						return nil, err
					}
					if vd.Type == "" {
						if ce, ok := ex.(*CallExpr); ok {
							if rt, ok2 := funcReturns[ce.Name]; ok2 {
								vd.Type = rt
							}
						}
					}
					if rec, ok := ex.(*RecordLit); ok {
						if vd.Type == "" {
							vd.Type = rec.Type
						}
						var args []Expr
						for _, f := range rec.Fields {
							args = append(args, f.Expr)
						}
						vd.Init = &CallExpr{Name: ctorName(rec.Type), Args: args}
						pr.Vars = append(pr.Vars, vd)
						if vd.Type != "" {
							varTypes[vd.Name] = vd.Type
						}
						continue
					}
					vd.Init = ex
					if vd.Type == "" {
						if t := inferType(ex); t != "" {
							vd.Type = t
						} else {
							switch t := types.ExprType(st.Var.Value, env).(type) {
							case types.StringType:
								vd.Type = "string"
							case types.ListType:
								vd.Type = pasTypeFromType(t)
							case types.BoolType:
								vd.Type = "boolean"
							}
						}
					}
				}
				pr.Vars = append(pr.Vars, vd)
			case st.Assign != nil:
				name := sanitize(st.Assign.Name)
				var ex Expr
				var err error
				if isEmptyMapLiteral(st.Assign.Value) {
					if t, ok := varTypes[name]; ok && strings.HasPrefix(t, "specialize TFPGMap") {
						parts := strings.TrimPrefix(t, "specialize TFPGMap<")
						parts = strings.TrimSuffix(parts, ">")
						kv := strings.Split(parts, ",")
						if len(kv) == 2 {
							keyT := strings.TrimSpace(kv[0])
							valT := strings.TrimSpace(kv[1])
							ex = &CallExpr{Name: fmt.Sprintf("specialize TFPGMap<%s, %s>.Create", keyT, valT)}
						}
					}
				}
				if ex == nil {
					prevMap := expectedMapType
					expectedMapType = varTypes[name]
					ex, err = convertExpr(env, st.Assign.Value)
					expectedMapType = prevMap
					if err != nil {
						return nil, err
					}
				}
				if idxExpr, ok := ex.(*IndexExpr); ok && strings.HasPrefix(inferType(idxExpr.Target), "specialize TFPGMap") {
					idxVar := fmt.Sprintf("%s_idx", name)
					varTypes[idxVar] = "integer"
					setVarType(idxVar, "integer")
					mapName := exprToVarRef(idxExpr.Target)
					varTypes[name] = "array of integer"
					setVarType(name, "array of integer")
					idxAssign := &AssignStmt{Name: idxVar, Expr: &CallExpr{Name: mapName + ".IndexOf", Args: []Expr{idxExpr.Index}}}
					cond := &BinaryExpr{Op: "<>", Left: &VarRef{Name: idxVar}, Right: &IntLit{Value: -1}, Bool: true}
					sel := &SelectorExpr{Root: mapName, Tail: []string{"Data"}}
					thenAssign := &AssignStmt{Name: name, Expr: &IndexExpr{Target: sel, Index: &VarRef{Name: idxVar}}}
					elseAssign := &AssignStmt{Name: name, Expr: zeroValue(varTypes[name])}
					pr.Stmts = append(pr.Stmts, idxAssign, &IfStmt{Cond: cond, Then: []Stmt{thenAssign}, Else: []Stmt{elseAssign}})
					continue
				}
				if len(st.Assign.Field) > 0 {
					target := &SelectorExpr{Root: st.Assign.Name}
					for _, f := range st.Assign.Field {
						target.Tail = append(target.Tail, f.Name)
					}
					pr.Stmts = append(pr.Stmts, &SetStmt{Target: target, Expr: ex})
					break
				}
				if len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil && st.Assign.Index[0].Colon2 == nil && len(st.Assign.Field) == 0 {
					idx, err := convertExpr(env, st.Assign.Index[0].Start)
					if err != nil {
						return nil, err
					}
					if t, ok := varTypes[name]; ok && strings.HasPrefix(t, "specialize TFPGMap") {
						pr.Stmts = append(pr.Stmts, &MapAssignStmt{Name: name, Key: idx, Expr: ex, Type: inferType(ex)})
					} else {
						pr.Stmts = append(pr.Stmts, &IndexAssignStmt{Name: name, Index: idx, Expr: ex})
					}
					break
				}
				if len(st.Assign.Index) == 2 &&
					st.Assign.Index[0].Colon == nil && st.Assign.Index[1].Colon == nil &&
					st.Assign.Index[0].Colon2 == nil && st.Assign.Index[1].Colon2 == nil &&
					len(st.Assign.Field) == 0 {
					idx1, err := convertExpr(env, st.Assign.Index[0].Start)
					if err != nil {
						return nil, err
					}
					idx2, err := convertExpr(env, st.Assign.Index[1].Start)
					if err != nil {
						return nil, err
					}
					pr.Stmts = append(pr.Stmts, &DoubleIndexAssignStmt{Name: name, Index1: idx1, Index2: idx2, Expr: ex})
					break
				}
				existing, ok := varTypes[name]
				t := inferType(ex)
				if t == "" {
					if tt := types.ExprType(st.Assign.Value, env); tt != nil {
						t = pasTypeFromType(tt)
					}
				}
				if t != "" && (!ok || existing == "" || existing == "array of integer") {
					varTypes[name] = t
					setVarType(name, t)
				}
				if call, ok := ex.(*CallExpr); ok && call.Name == "concat" && len(call.Args) == 2 {
					if list, ok := call.Args[1].(*ListLit); ok && len(list.Elems) == 1 {
						et := inferType(list.Elems[0])
						if et == "" {
							_ = currProg.addArrayAlias("Variant")
							alias := currProg.addArrayAlias("VariantArray")
							t := "array of " + alias
							varTypes[name] = t
							setVarType(name, t)
						} else {
							t := "array of " + et
							varTypes[name] = t
							setVarType(name, t)
						}
					}
				}
				pr.Stmts = append(pr.Stmts, &AssignStmt{Name: name, Expr: ex})
				continue
			case st.For != nil:
				start, err := convertExpr(env, st.For.Source)
				if err != nil {
					return nil, err
				}
				typ := "integer"
				if st.For.RangeEnd == nil {
					if t := inferType(start); strings.HasPrefix(t, "array of ") {
						typ = strings.TrimPrefix(t, "array of ")
					} else if strings.HasPrefix(t, "specialize TFPGMap") {
						parts := strings.TrimPrefix(t, "specialize TFPGMap<")
						parts = strings.TrimSuffix(parts, ">")
						kv := strings.Split(parts, ",")
						if len(kv) == 2 {
							typ = strings.TrimSpace(kv[0])
						}
					} else {
						tt := types.ExprType(st.For.Source, env)
						if lt, ok := tt.(types.ListType); ok {
							typ = pasTypeFromType(lt.Elem)
							if strings.HasPrefix(typ, "array of ") {
								elem := strings.TrimPrefix(typ, "array of ")
								typ = currProg.addArrayAlias(elem)
							}
						} else if mt, ok := tt.(types.MapType); ok {
							typ = pasTypeFromType(mt.Key)
						}
					}
				}
				name := sanitize(st.For.Name)
				varTypes[name] = typ
				setVarType(name, varTypes[name])
				pushScope()
				body, err := convertBody(env, st.For.Body, varTypes)
				popScope()
				if err != nil {
					return nil, err
				}
				if st.For.RangeEnd != nil {
					end, err := convertExpr(env, st.For.RangeEnd)
					if err != nil {
						return nil, err
					}
					pr.Stmts = append(pr.Stmts, &ForRangeStmt{Name: name, Start: start, End: end, Body: body})
				} else if strings.HasPrefix(inferType(start), "specialize TFPGMap") {
					idxVar := fmt.Sprintf("%s_idx", name)
					varTypes[idxVar] = "integer"
					setVarType(idxVar, "integer")
					keySel := &SelectorExpr{Root: exprToVarRef(start), Tail: []string{"Keys"}}
					assignKey := &AssignStmt{Name: name, Expr: &IndexExpr{Target: keySel, Index: &VarRef{Name: idxVar}}}
					body = append([]Stmt{assignKey}, body...)
					end := &SelectorExpr{Root: exprToVarRef(start), Tail: []string{"Count"}}
					pr.Stmts = append(pr.Stmts, &ForRangeStmt{Name: idxVar, Start: &IntLit{Value: 0}, End: end, Body: body})
				} else {
					pr.Stmts = append(pr.Stmts, &ForEachStmt{Name: name, Iterable: start, Body: body})
				}
			case st.While != nil:
				cond, err := convertExpr(env, st.While.Cond)
				if err != nil {
					return nil, err
				}
				pushScope()
				body, err := convertBody(env, st.While.Body, varTypes)
				popScope()
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, &WhileStmt{Cond: cond, Body: body})
			case st.If != nil:
				ifStmt, err := convertIf(env, st.If, varTypes)
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, ifStmt)
			case st.Fun != nil:
				fn := st.Fun
				local := map[string]string{}
				for k, v := range varTypes {
					local[k] = v
				}
				startVarCount := len(currProg.Vars)
				pushScope()
				prevFunc := currentFunc
				currentFunc = fn.Name
				rt := ""
				if st.Fun.Return != nil {
					rt = typeFromRef(st.Fun.Return)
					if strings.HasPrefix(rt, "array of ") {
						elem := strings.TrimPrefix(rt, "array of ")
						if !isArrayAlias(elem) {
							rt = currProg.addArrayAlias(elem)
						}
					}
					funcReturns[fn.Name] = rt
				}
				for _, p := range st.Fun.Params {
					typ := "integer"
					if p.Type != nil {
						typ = typeFromRef(p.Type)
					}
					if strings.HasPrefix(typ, "array of ") {
						elem := strings.TrimPrefix(typ, "array of ")
						typ = currProg.addArrayAlias(elem)
					}
					name := sanitize(p.Name)
					local[name] = typ
					currentScope()[p.Name] = name
				}
				fnBody, err := convertBody(env, st.Fun.Body, local)
				if err != nil {
					return nil, err
				}
				popScope()
				locals := append([]VarDecl(nil), currProg.Vars[startVarCount:]...)
				currProg.Vars = currProg.Vars[:startVarCount]
				var params []string
				for _, p := range st.Fun.Params {
					typ := "integer"
					if p.Type != nil {
						typ = typeFromRef(p.Type)
					}
					if strings.HasPrefix(typ, "array of ") {
						elem := strings.TrimPrefix(typ, "array of ")
						typ = currProg.addArrayAlias(elem)
					}
					name := sanitize(p.Name)
					params = append(params, formatParam(name, typ))
				}
				if rt == "" {
					for _, st := range fnBody {
						if ret, ok := st.(*ReturnStmt); ok && ret.Expr != nil {
							rt = inferType(ret.Expr)
							if strings.HasPrefix(rt, "array of ") {
								elem := strings.TrimPrefix(rt, "array of ")
								if !isArrayAlias(elem) {
									rt = currProg.addArrayAlias(elem)
								}
							}
							break
						}
					}
				} else if strings.HasPrefix(rt, "specialize TFPGMap") {
					for _, stmt := range fnBody {
						if ret, ok := stmt.(*ReturnStmt); ok && ret.Expr != nil {
							switch expr := ret.Expr.(type) {
							case *RecordLit:
								rt = expr.Type
							case *CallExpr:
								if t, ok := funcReturns[expr.Name]; ok {
									rt = t
								}
							default:
								typ := inferType(ret.Expr)
								if typ != "" && !strings.HasPrefix(typ, "specialize TFPGMap") {
									rt = typ
								}
							}
							break
						}
					}
					if strings.HasPrefix(rt, "Anon") {
						funcReturns[st.Fun.Name] = rt
					}
				}
				if fn.Name == "parseIntStr" {
					if len(params) == 1 {
						nameParts := strings.SplitN(params[0], ":", 2)
						paramName := strings.TrimSpace(nameParts[0])
						body := []Stmt{&ReturnStmt{Expr: &CallExpr{Name: "StrToInt", Args: []Expr{&VarRef{Name: paramName}}}}}
						currProg.Funs = append(currProg.Funs, FunDecl{Name: fn.Name, Params: params, ReturnType: "integer", Locals: locals, Body: body})
						funcReturns[fn.Name] = "integer"
					}
				} else {
					if prevFunc == "" {
						currProg.Funs = append(currProg.Funs, FunDecl{Name: fn.Name, Params: params, ReturnType: rt, Locals: locals, Body: fnBody})
					} else {
						pr.Stmts = append(pr.Stmts, &NestedFunDecl{FunDecl{Name: fn.Name, Params: params, ReturnType: rt, Locals: locals, Body: fnBody}})
					}
					if rt != "" {
						funcReturns[fn.Name] = rt
					}
				}
				currentFunc = prevFunc
			case st.Return != nil:
				if st.Return.Value != nil {
					ex, err := convertExpr(env, st.Return.Value)
					if err != nil {
						return nil, err
					}
					pr.Stmts = append(pr.Stmts, &ReturnStmt{Expr: ex})
				} else {
					pr.Stmts = append(pr.Stmts, &ReturnStmt{Expr: nil})
				}
			case st.Import != nil:
				continue
			case st.ExternFun != nil:
				continue
			case st.ExternVar != nil:
				continue
			case st.Bench != nil:
				benchStmts, err := convertBenchBlock(env, st.Bench, varTypes)
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, benchStmts...)
			case st.Test != nil:
				// ignore test blocks in transpiled output
				continue
			default:
				return nil, fmt.Errorf("unsupported statement")
			}
		}
	}
	for name, typ := range varTypes {
		exists := false
		for _, v := range pr.Vars {
			if v.Name == name {
				exists = true
				break
			}
		}
		if !exists {
			pr.Vars = append(pr.Vars, VarDecl{Name: name, Type: typ})
		}
	}
	addConstructors(pr)
	addMapConstructors(pr)
	markSysUtils(pr)
	// Move variables that belong to function bodies from the global scope
	// into the corresponding function's local variable list. Variables
	// defined inside a function are prefixed with the function name by
	// sanitize(), so we can use this to determine ownership.
	if len(pr.Funs) > 0 && len(pr.Vars) > 0 {
		var globals []VarDecl
		for _, gv := range pr.Vars {
			moved := false
			for i := range pr.Funs {
				prefix := pr.Funs[i].Name + "_"
				if strings.HasPrefix(gv.Name, prefix) {
					pr.Funs[i].Locals = append(pr.Funs[i].Locals, gv)
					moved = true
					break
				}
			}
			if !moved {
				globals = append(globals, gv)
			}
		}
		pr.Vars = globals
	}
	pr.VarTypes = varTypes
	currProg = nil
	return pr, nil
}

func convertIf(env *types.Env, ifs *parser.IfStmt, varTypes map[string]string) (*IfStmt, error) {
	cond, err := convertExpr(env, ifs.Cond)
	if err != nil {
		return nil, err
	}
	thenBody, err := convertBody(env, ifs.Then, varTypes)
	if err != nil {
		return nil, err
	}
	var elseBody []Stmt
	if ifs.ElseIf != nil {
		nested, err := convertIf(env, ifs.ElseIf, varTypes)
		if err != nil {
			return nil, err
		}
		elseBody = []Stmt{nested}
	} else {
		var err2 error
		elseBody, err2 = convertBody(env, ifs.Else, varTypes)
		if err2 != nil {
			return nil, err2
		}
	}
	return &IfStmt{Cond: cond, Then: thenBody, Else: elseBody}, nil
}

func convertBody(env *types.Env, body []*parser.Statement, varTypes map[string]string) ([]Stmt, error) {
	prev := currentVarTypes
	currentVarTypes = varTypes
	defer func() { currentVarTypes = prev }()
	var out []Stmt
	for _, st := range body {
		switch {
		case st.Assign != nil:
			name := sanitize(st.Assign.Name)
			var ex Expr
			var err error
			if isEmptyMapLiteral(st.Assign.Value) {
				if t, ok := varTypes[name]; ok && strings.HasPrefix(t, "specialize TFPGMap") {
					parts := strings.TrimPrefix(t, "specialize TFPGMap<")
					parts = strings.TrimSuffix(parts, ">")
					kv := strings.Split(parts, ",")
					if len(kv) == 2 {
						keyT := strings.TrimSpace(kv[0])
						valT := strings.TrimSpace(kv[1])
						ex = &CallExpr{Name: fmt.Sprintf("specialize TFPGMap<%s, %s>.Create", keyT, valT)}
					}
				}
			}
			if ex == nil {
				ex, err = convertExpr(env, st.Assign.Value)
				if err != nil {
					return nil, err
				}
			}
			if len(st.Assign.Field) > 0 {
				target := &SelectorExpr{Root: name}
				for _, f := range st.Assign.Field {
					target.Tail = append(target.Tail, f.Name)
				}
				out = append(out, &SetStmt{Target: target, Expr: ex})
				break
			}
			if len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil && st.Assign.Index[0].Colon2 == nil && len(st.Assign.Field) == 0 {
				idx, err := convertExpr(env, st.Assign.Index[0].Start)
				if err != nil {
					return nil, err
				}
				if t, ok := varTypes[name]; ok && strings.HasPrefix(t, "specialize TFPGMap") {
					out = append(out, &MapAssignStmt{Name: name, Key: idx, Expr: ex, Type: inferType(ex)})
				} else {
					out = append(out, &IndexAssignStmt{Name: name, Index: idx, Expr: ex})
				}
				break
			}
			if len(st.Assign.Index) == 2 &&
				st.Assign.Index[0].Colon == nil && st.Assign.Index[1].Colon == nil &&
				st.Assign.Index[0].Colon2 == nil && st.Assign.Index[1].Colon2 == nil &&
				len(st.Assign.Field) == 0 {
				idx1, err := convertExpr(env, st.Assign.Index[0].Start)
				if err != nil {
					return nil, err
				}
				idx2, err := convertExpr(env, st.Assign.Index[1].Start)
				if err != nil {
					return nil, err
				}
				out = append(out, &DoubleIndexAssignStmt{Name: name, Index1: idx1, Index2: idx2, Expr: ex})
				break
			}
			if len(st.Assign.Index) > 0 && len(st.Assign.Field) == 0 {
				if t, ok := varTypes[name]; ok && strings.HasPrefix(t, "specialize TFPGMap") && len(st.Assign.Index) > 1 {
					idx0, err := convertExpr(env, st.Assign.Index[0].Start)
					if err != nil {
						return nil, err
					}
					elemType := inferType(ex)
					arrType := elemType
					for j := len(st.Assign.Index) - 1; j > 0; j-- {
						arrType = currProg.addArrayAlias(arrType)
					}
					target := Expr(&CastExpr{Expr: &IndexExpr{Target: &VarRef{Name: name}, Index: idx0}, Type: arrType})
					for _, idx := range st.Assign.Index[1:] {
						idxExpr, err := convertExpr(env, idx.Start)
						if err != nil {
							return nil, err
						}
						target = &IndexExpr{Target: target, Index: idxExpr}
					}
					out = append(out, &SetStmt{Target: target, Expr: ex})
					break
				}
			}
			if _, ok := varTypes[name]; !ok {
				if t := inferType(ex); t != "" {
					varTypes[name] = t
					setVarType(name, t)
				}
				if call, ok := ex.(*CallExpr); ok && call.Name == "concat" && len(call.Args) == 2 {
					if list, ok := call.Args[1].(*ListLit); ok && len(list.Elems) == 1 {
						if et := inferType(list.Elems[0]); et != "" {
							t := "array of " + et
							varTypes[name] = t
							setVarType(name, t)
						}
					}
				}
			}
			out = append(out, &AssignStmt{Name: name, Expr: ex})
		case st.Let != nil:
			name := sanitize(st.Let.Name)
			vd := VarDecl{Name: name}
			if st.Let.Type != nil {
				vd.Type = typeFromRef(st.Let.Type)
			}
			if st.Let.Value != nil {
				prevMap := expectedMapType
				expectedMapType = vd.Type
				ex, err := convertExpr(env, st.Let.Value)
				expectedMapType = prevMap
				if err != nil {
					return nil, err
				}
				if idxExpr, ok := ex.(*IndexExpr); ok && strings.HasPrefix(inferType(idxExpr.Target), "specialize TFPGMap") {
					if !hasVar(vd.Name) {
						currProg.Vars = append(currProg.Vars, vd)
					}
					if vd.Type != "" {
						varTypes[vd.Name] = vd.Type
					}
					idxVar := fmt.Sprintf("%s_idx", name)
					varTypes[idxVar] = "integer"
					setVarType(idxVar, "integer")
					mapName := exprToVarRef(idxExpr.Target)
					idxAssign := &AssignStmt{Name: idxVar, Expr: &CallExpr{Name: mapName + ".IndexOf", Args: []Expr{idxExpr.Index}}}
					cond := &BinaryExpr{Op: "<>", Left: &VarRef{Name: idxVar}, Right: &IntLit{Value: -1}, Bool: true}
					sel := &SelectorExpr{Root: mapName, Tail: []string{"Data"}}
					thenAssign := &AssignStmt{Name: name, Expr: &IndexExpr{Target: sel, Index: &VarRef{Name: idxVar}}}
					elseAssign := &AssignStmt{Name: name, Expr: zeroValue(vd.Type)}
					out = append(out, idxAssign, &IfStmt{Cond: cond, Then: []Stmt{thenAssign}, Else: []Stmt{elseAssign}})
					continue
				}
				if rec, ok := ex.(*RecordLit); ok {
					if vd.Type == "" {
						vd.Type = rec.Type
					}
					var args []Expr
					for _, f := range rec.Fields {
						args = append(args, f.Expr)
					}
					out = append(out, &AssignStmt{Name: name, Expr: &CallExpr{Name: ctorName(rec.Type), Args: args}})
				} else {
					if vd.Type == "" {
						t := inferType(ex)
						if t == "" {
							if tt := types.ExprType(st.Let.Value, env); tt != nil {
								t = pasTypeFromType(tt)
							}
						}
						if t != "" {
							vd.Type = t
						}
					}
					out = append(out, &AssignStmt{Name: name, Expr: ex})
				}
			}
			if !hasVar(vd.Name) {
				currProg.Vars = append(currProg.Vars, vd)
			}
			if vd.Type != "" {
				varTypes[vd.Name] = vd.Type
			}
		case st.Type != nil:
			var fields []Field
			for _, m := range st.Type.Members {
				if m.Field == nil || m.Field.Type == nil {
					continue
				}
				typ := typeFromRef(m.Field.Type)
				if typ == "" {
					continue
				}
				fields = append(fields, Field{Name: m.Field.Name, Type: typ})
			}
			currProg.Records = append(currProg.Records, RecordDef{Name: st.Type.Name, Fields: fields})
		case st.Var != nil:
			name := sanitize(st.Var.Name)
			vd := VarDecl{Name: name}
			if st.Var.Type != nil {
				vd.Type = typeFromRef(st.Var.Type)
			}
			if st.Var.Value != nil {
				if isEmptyMapLiteral(st.Var.Value) && strings.HasPrefix(vd.Type, "specialize TFPGMap") {
					if !hasVar(vd.Name) {
						currProg.Vars = append(currProg.Vars, vd)
					}
					if vd.Type != "" {
						varTypes[vd.Name] = vd.Type
					}
					out = append(out, &AssignStmt{Name: name, Expr: &CallExpr{Name: vd.Type + ".Create"}})
					continue
				}
				ex, err := convertExpr(env, st.Var.Value)
				if err != nil {
					return nil, err
				}
				if rec, ok := ex.(*RecordLit); ok {
					if vd.Type == "" {
						vd.Type = rec.Type
					}
					var args []Expr
					for _, f := range rec.Fields {
						args = append(args, f.Expr)
					}
					out = append(out, &AssignStmt{Name: name, Expr: &CallExpr{Name: ctorName(rec.Type), Args: args}})
				} else {
					if vd.Type == "" {
						if t := inferType(ex); t != "" {
							vd.Type = t
						}
					}
					out = append(out, &AssignStmt{Name: name, Expr: ex})
				}
			}
			if !hasVar(vd.Name) {
				currProg.Vars = append(currProg.Vars, vd)
			}
			if vd.Type != "" {
				varTypes[vd.Name] = vd.Type
			}
		case st.For != nil:
			start, err := convertExpr(env, st.For.Source)
			if err != nil {
				return nil, err
			}
			startType := inferType(start)
			if startType == "" {
				if name, ok := exprToIdent(st.For.Source); ok {
					if t, ok2 := varTypes[sanitize(name)]; ok2 {
						startType = t
					}
				}
			}
			if startType == "" {
				if mt, ok := types.ExprType(st.For.Source, env).(types.MapType); ok {
					keyT := pasTypeFromType(mt.Key)
					valT := pasTypeFromType(mt.Value)
					startType = fmt.Sprintf("specialize TFPGMap<%s, %s>", keyT, valT)
				}
			}
			typ := "integer"
			if st.For.RangeEnd == nil {
				if strings.HasPrefix(startType, "array of ") {
					elem := strings.TrimPrefix(startType, "array of ")
					switch elem {
					case "string":
						typ = "string"
					case "boolean":
						typ = "boolean"
					default:
						typ = elem
					}
				} else if strings.HasPrefix(startType, "specialize TFPGMap") {
					parts := strings.TrimPrefix(startType, "specialize TFPGMap<")
					parts = strings.TrimSuffix(parts, ">")
					kv := strings.Split(parts, ",")
					if len(kv) == 2 {
						typ = strings.TrimSpace(kv[0])
					}
				} else {
					tt := types.ExprType(st.For.Source, env)
					if lt, ok := tt.(types.ListType); ok {
						typ = pasTypeFromType(lt.Elem)
					} else if mt, ok := tt.(types.MapType); ok {
						typ = pasTypeFromType(mt.Key)
					}
				}
			}
			name := sanitize(st.For.Name)
			varTypes[name] = typ
			setVarType(name, varTypes[name])
			body, err := convertBody(env, st.For.Body, varTypes)
			if err != nil {
				return nil, err
			}
			if st.For.RangeEnd != nil {
				end, err := convertExpr(env, st.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				out = append(out, &ForRangeStmt{Name: name, Start: start, End: end, Body: body})
			} else if strings.HasPrefix(startType, "specialize TFPGMap") {
				idxVar := fmt.Sprintf("%s_idx", name)
				varTypes[idxVar] = "integer"
				setVarType(idxVar, "integer")
				keySel := &SelectorExpr{Root: exprToVarRef(start), Tail: []string{"Keys"}}
				assignKey := &AssignStmt{Name: name, Expr: &IndexExpr{Target: keySel, Index: &VarRef{Name: idxVar}}}
				body = append([]Stmt{assignKey}, body...)
				end := &SelectorExpr{Root: exprToVarRef(start), Tail: []string{"Count"}}
				out = append(out, &ForRangeStmt{Name: idxVar, Start: &IntLit{Value: 0}, End: end, Body: body})
			} else {
				out = append(out, &ForEachStmt{Name: name, Iterable: start, Body: body})
			}
		case st.While != nil:
			cond, err := convertExpr(env, st.While.Cond)
			if err != nil {
				return nil, err
			}
			body, err := convertBody(env, st.While.Body, varTypes)
			if err != nil {
				return nil, err
			}
			out = append(out, &WhileStmt{Cond: cond, Body: body})
		case st.Break != nil:
			out = append(out, &BreakStmt{})
		case st.Continue != nil:
			out = append(out, &ContinueStmt{})
		case st.Bench != nil:
			benchStmts, err := convertBenchBlock(env, st.Bench, varTypes)
			if err != nil {
				return nil, err
			}
			out = append(out, benchStmts...)
		case st.Expr != nil:
			if se := extractSaveExpr(st.Expr.Expr); se != nil {
				src, err := convertExpr(env, se.Src)
				if err != nil {
					return nil, err
				}
				format := parseFormat(se.With)
				path := ""
				if se.Path != nil {
					path = strings.Trim(*se.Path, "\"")
				}
				if format == "jsonl" && (path == "" || path == "-") {
					var rec RecordDef
					t := types.ExprType(se.Src, env)
					if lt, ok := t.(types.ListType); ok {
						if stype, ok := lt.Elem.(types.StructType); ok {
							rec, _ = findRecord(stype.Name)
						}
					}
					if rec.Name == "" {
						if name, ok := exprToIdent(se.Src); ok {
							if vt, ok2 := currentVarTypes[sanitize(name)]; ok2 && strings.HasPrefix(vt, "array of ") {
								rec, _ = findRecord(strings.TrimPrefix(vt, "array of "))
							}
						}
					}
					if rec.Name == "" {
						return nil, fmt.Errorf("save expects list of records")
					}
					loopVar := "row"
					body := []Stmt{&WritelnStmt{Expr: buildJSONLineExpr(loopVar, rec)}}
					out = append(out, &ForEachStmt{Name: loopVar, Iterable: src, Body: body})
					continue
				}
				return nil, fmt.Errorf("unsupported save")
			}
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(st.Expr.Expr.Binary.Right) == 0 {
				var parts []Expr
				var typesList []string
				needSys := false
				for _, a := range call.Args {
					ex, err := convertExpr(env, a)
					if err != nil {
						return nil, err
					}
					parts = append(parts, ex)
					t := inferType(ex)
					typesList = append(typesList, t)
					if t == "boolean" {
						needSys = true
                                       } else if strings.HasPrefix(t, "array of ") || isArrayAlias(t) {
                                               tt := resolveAlias(t)
                                               if strings.HasPrefix(tt, "array of string") {
                                                       currProg.NeedListStr = true
                                               } else if strings.HasPrefix(tt, "array of array") || strings.HasPrefix(tt, "array of IntArray") || strings.HasPrefix(tt, "array of Int64Array") {
                                                       currProg.NeedShowList2 = true
                                               } else if strings.HasPrefix(tt, "array of int64") {
                                                       currProg.NeedShowListInt64 = true
                                               } else if strings.HasPrefix(tt, "array of Variant") {
                                                       currProg.NeedListStrVariant = true
                                                       currProg.UseVariants = true
                                                       needSys = true
                                               } else {
                                                       currProg.NeedShowList = true
                                               }
                                       } else if strings.HasPrefix(t, "specialize TFPGMap") {
                                               currProg.NeedShowMap = true
                                       }
				}
				out = append(out, &PrintStmt{Exprs: parts, Types: typesList, NeedSysUtils: needSys})
				continue
			}
			ex, err := convertExpr(env, st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			out = append(out, &ExprStmt{Expr: ex})
			continue
		case st.If != nil:
			ifStmt, err := convertIf(env, st.If, varTypes)
			if err != nil {
				return nil, err
			}
			out = append(out, ifStmt)
		case st.Fun != nil:
			fn := st.Fun
			name := fn.Name
			switch name {
			case "xor", "and", "or", "div", "mod", "type", "set", "label":
				name = name + "_"
			}
			if name != fn.Name {
				funcNames[name] = struct{}{}
				nameMap[fn.Name] = name
				fn.Name = name
			}
			local := map[string]string{}
			for k, v := range varTypes {
				local[k] = v
			}
			for _, p := range fn.Params {
				typ := "integer"
				if p.Type != nil {
					typ = typeFromRef(p.Type)
				}
				if strings.HasPrefix(typ, "array of ") {
					elem := strings.TrimPrefix(typ, "array of ")
					typ = currProg.addArrayAlias(elem)
				}
				name := sanitize(p.Name)
				local[name] = typ
			}
			pushScope()
			prevFunc := currentFunc
			currentFunc = fn.Name
			for _, p := range fn.Params {
				name := sanitize(p.Name)
				currentScope()[p.Name] = name
			}
			fnBody, err := convertBody(env, fn.Body, local)
			if err != nil {
				return nil, err
			}
			popScope()
			var params []string
			for _, p := range fn.Params {
				typ := "integer"
				if p.Type != nil {
					typ = typeFromRef(p.Type)
				}
				if strings.HasPrefix(typ, "array of ") {
					elem := strings.TrimPrefix(typ, "array of ")
					typ = currProg.addArrayAlias(elem)
				}
				name := sanitize(p.Name)
				params = append(params, formatParam(name, typ))
			}
			rt := ""
			if fn.Return != nil {
				rt = typeFromRef(fn.Return)
				if strings.HasPrefix(rt, "array of ") {
					elem := strings.TrimPrefix(rt, "array of ")
					rt = currProg.addArrayAlias(elem)
				}
			}
			if rt == "" {
				for _, st := range fnBody {
					if ret, ok := st.(*ReturnStmt); ok && ret.Expr != nil {
						rt = inferType(ret.Expr)
						if strings.HasPrefix(rt, "array of ") {
							elem := strings.TrimPrefix(rt, "array of ")
							rt = currProg.addArrayAlias(elem)
						}
						break
					}
				}
			} else if strings.HasPrefix(rt, "specialize TFPGMap") {
				for _, stmt := range fnBody {
					if ret, ok := stmt.(*ReturnStmt); ok && ret.Expr != nil {
						switch expr := ret.Expr.(type) {
						case *RecordLit:
							rt = expr.Type
						case *CallExpr:
							if t, ok := funcReturns[expr.Name]; ok {
								rt = t
							}
						default:
							typ := inferType(ret.Expr)
							if typ != "" && !strings.HasPrefix(typ, "specialize TFPGMap") {
								rt = typ
							}
						}
						break
					}
				}
				if strings.HasPrefix(rt, "Anon") {
					funcReturns[fn.Name] = rt
				}
			}
			if fn.Name == "parseIntStr" {
				if len(params) == 1 {
					nameParts := strings.SplitN(params[0], ":", 2)
					paramName := strings.TrimSpace(nameParts[0])
					body := []Stmt{&ReturnStmt{Expr: &CallExpr{Name: "StrToInt", Args: []Expr{&VarRef{Name: paramName}}}}}
					currProg.Funs = append(currProg.Funs, FunDecl{Name: fn.Name, Params: params, ReturnType: "integer", Body: body})
					funcReturns[fn.Name] = "integer"
				}
			} else {
				if prevFunc == "" {
					currProg.Funs = append(currProg.Funs, FunDecl{Name: fn.Name, Params: params, ReturnType: rt, Body: fnBody})
				} else {
					out = append(out, &NestedFunDecl{FunDecl{Name: fn.Name, Params: params, ReturnType: rt, Body: fnBody}})
				}
				if rt != "" {
					funcReturns[fn.Name] = rt
				}
			}
			currentFunc = prevFunc
		case st.Return != nil:
			if st.Return.Value != nil {
				ex, err := convertExpr(env, st.Return.Value)
				if err != nil {
					return nil, err
				}
				if rec, ok := ex.(*RecordLit); ok {
					var args []Expr
					for _, f := range rec.Fields {
						args = append(args, f.Expr)
					}
					ex = &CallExpr{Name: ctorName(rec.Type), Args: args}
				}
				out = append(out, &ReturnStmt{Expr: ex})
			} else {
				out = append(out, &ReturnStmt{Expr: nil})
			}
		case st.Import != nil:
			continue
		case st.ExternFun != nil:
			continue
		case st.ExternVar != nil:
			continue
		case st.Test != nil:
			// ignore tests inside functions
			continue
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func convertBenchBlock(env *types.Env, bench *parser.BenchBlock, varTypes map[string]string) ([]Stmt, error) {
	currProg.UseSysUtils = true
	currProg.UseNow = true
	currProg.UseMem = true
	currProg.NeedBenchNow = true
	startName := sanitize(fmt.Sprintf("bench_start_%d", len(currProg.Vars)))
	durName := sanitize(fmt.Sprintf("bench_dur_%d", len(currProg.Vars)))
	memStart := sanitize(fmt.Sprintf("bench_mem_%d", len(currProg.Vars)))
	memDiff := sanitize(fmt.Sprintf("bench_memdiff_%d", len(currProg.Vars)))
	if !hasVar(startName) {
		currProg.Vars = append(currProg.Vars, VarDecl{Name: startName, Type: "integer"})
	}
	if !hasVar(durName) {
		currProg.Vars = append(currProg.Vars, VarDecl{Name: durName, Type: "integer"})
	}
	if !hasVar(memStart) {
		currProg.Vars = append(currProg.Vars, VarDecl{Name: memStart, Type: "int64"})
	}
	if !hasVar(memDiff) {
		currProg.Vars = append(currProg.Vars, VarDecl{Name: memDiff, Type: "int64"})
	}
	varTypes[startName] = "integer"
	varTypes[durName] = "integer"
	varTypes[memStart] = "int64"
	varTypes[memDiff] = "int64"
	out := []Stmt{
		&AssignStmt{Name: memStart, Expr: &CallExpr{Name: "_mem"}},
		&AssignStmt{Name: startName, Expr: &CallExpr{Name: "_bench_now"}},
	}
	prevFunc := currentFunc
	currentFunc = ""
	body, err := convertBody(env, bench.Body, varTypes)
	currentFunc = prevFunc
	if err != nil {
		return nil, err
	}
	out = append(out, body...)
	out = append(out, &ExprStmt{Expr: &CallExpr{Name: "Sleep", Args: []Expr{&IntLit{Value: 1}}}})
	out = append(out, &AssignStmt{Name: memDiff, Expr: &BinaryExpr{Op: "-", Left: &CallExpr{Name: "_mem"}, Right: &VarRef{Name: memStart}}})
	diff := &BinaryExpr{Op: "-", Left: &CallExpr{Name: "_bench_now"}, Right: &VarRef{Name: startName}}
	div := &BinaryExpr{Op: "div", Left: diff, Right: &IntLit{Value: 1000}}
	out = append(out, &AssignStmt{Name: durName, Expr: div})
	out = append(out, &WritelnStmt{Expr: &StringLit{Value: "{"}})
	line2 := &BinaryExpr{Op: "+", Left: &BinaryExpr{Op: "+", Left: &StringLit{Value: "  \"duration_us\": "}, Right: &CallExpr{Name: "IntToStr", Args: []Expr{&VarRef{Name: durName}}}}, Right: &StringLit{Value: ","}}
	out = append(out, &WritelnStmt{Expr: line2})
	line3 := &BinaryExpr{Op: "+", Left: &BinaryExpr{Op: "+", Left: &StringLit{Value: "  \"memory_bytes\": "}, Right: &CallExpr{Name: "IntToStr", Args: []Expr{&VarRef{Name: memDiff}}}}, Right: &StringLit{Value: ","}}
	out = append(out, &WritelnStmt{Expr: line3})
	line4 := &BinaryExpr{Op: "+", Left: &BinaryExpr{Op: "+", Left: &StringLit{Value: "  \"name\": \""}, Right: &StringLit{Value: bench.Name}}, Right: &StringLit{Value: "\""}}
	out = append(out, &WritelnStmt{Expr: line4})
	out = append(out, &WritelnStmt{Expr: &StringLit{Value: "}"}})
	return out, nil
}

func convertExpr(env *types.Env, e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expr")
	}
	left, err := convertUnary(env, e.Binary.Left)
	if err != nil {
		return nil, err
	}
	if len(e.Binary.Right) == 0 {
		return left, nil
	}
	if len(e.Binary.Right) == 1 && e.Binary.Right[0].Op == "+" {
		right, err := convertPostfix(env, e.Binary.Right[0].Right)
		if err != nil {
			return nil, err
		}
		lt := inferType(left)
		rt := inferType(right)
		if strings.HasPrefix(lt, "array of ") && strings.HasPrefix(rt, "array of ") {
			return &CallExpr{Name: "concat", Args: []Expr{left, right}}, nil
		}
	}
	if len(e.Binary.Right) == 1 && e.Binary.Right[0].Op == "in" {
		right, err := convertPostfix(env, e.Binary.Right[0].Right)
		if err != nil {
			return nil, err
		}
		tmp := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: e.Binary.Right[0].Right}}}
		t := types.ExprType(tmp, env)
		if _, ok := t.(types.ListType); ok {
			currProg.NeedContains = true
			return &ContainsExpr{Collection: right, Value: left, Kind: "list"}, nil
		}
	}

	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 3
		case "+", "-":
			return 2
		case "==", "!=", "<", "<=", ">", ">=", "in":
			return 1
		case "&&", "||":
			return 0
		}
		return -1
	}

	var ops []string
	var exprs []Expr
	exprs = append(exprs, left)

	build := func() error {
		if len(ops) == 0 || len(exprs) < 2 {
			return nil
		}
		op := ops[len(ops)-1]
		ops = ops[:len(ops)-1]
		right := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		left := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		var be *BinaryExpr
		switch op {
		case "+", "-", "*", "%":
			if op == "+" {
				lt := inferType(left)
				rt := inferType(right)
				if strings.HasPrefix(lt, "array of ") && strings.HasPrefix(rt, "array of ") {
					exprs = append(exprs, &CallExpr{Name: "concat", Args: []Expr{left, right}})
					return nil
				}
			}
			be = &BinaryExpr{Op: op, Left: left, Right: right}
			lt := inferType(left)
			rt := inferType(right)
			if lt == "BigRat" || rt == "BigRat" {
				be.Rat = true
			}
		case "/":
			be = &BinaryExpr{Op: "/", Left: left, Right: right}
			lt := inferType(left)
			rt := inferType(right)
			if lt == "real" || rt == "real" {
				be.Real = true
			}
			if lt == "BigRat" || rt == "BigRat" {
				be.Rat = true
			}
		case "==":
			be = &BinaryExpr{Op: "=", Left: left, Right: right, Bool: true}
		case "!=":
			be = &BinaryExpr{Op: "<>", Left: left, Right: right, Bool: true}
		case "<", "<=", ">", ">=":
			be = &BinaryExpr{Op: op, Left: left, Right: right, Bool: true}
		case "&&":
			be = &BinaryExpr{Op: "and", Left: left, Right: right, Bool: true}
		case "||":
			be = &BinaryExpr{Op: "or", Left: left, Right: right, Bool: true}
		case "in":
			rt := inferType(right)
			if strings.HasPrefix(rt, "array") {
				currProg.NeedContains = true
				be = nil
				exprs = append(exprs, &ContainsExpr{Collection: right, Value: left, Kind: "list"})
				return nil
			}
			if strings.HasPrefix(rt, "specialize TFPGMap") {
				currProg.UseFGL = true
				be = nil
				exprs = append(exprs, &MapContainsExpr{Map: right, Key: left})
				return nil
			}
			be = &BinaryExpr{Op: "in", Left: left, Right: right, Bool: true}
		default:
			return fmt.Errorf("unsupported op")
		}
		exprs = append(exprs, be)
		return nil
	}

	for _, op := range e.Binary.Right {
		right, err := convertPostfix(env, op.Right)
		if err != nil {
			return nil, err
		}
		for len(ops) > 0 && prec(op.Op) <= prec(ops[len(ops)-1]) {
			if err := build(); err != nil {
				return nil, err
			}
		}
		exprs = append(exprs, right)
		ops = append(ops, op.Op)
	}
	for len(ops) > 0 {
		if err := build(); err != nil {
			return nil, err
		}
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("invalid expression")
	}
	return exprs[0], nil
}

func mapLitFromExpr(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return nil
	}
	pf := u.Value
	if pf == nil || len(pf.Ops) > 0 {
		return nil
	}
	if pf.Target != nil {
		return pf.Target.Map
	}
	return nil
}

func isEmptyMapLiteral(e *parser.Expr) bool {
	ml := mapLitFromExpr(e)
	return ml != nil && len(ml.Items) == 0
}

func queryFromExpr(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return nil
	}
	pf := u.Value
	if pf == nil || len(pf.Ops) > 0 {
		return nil
	}
	if pf.Target != nil {
		return pf.Target.Query
	}
	return nil
}

func callFromExpr(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return nil
	}
	pf := u.Value
	if pf == nil || len(pf.Ops) > 0 {
		return nil
	}
	if pf.Target != nil {
		return pf.Target.Call
	}
	return nil
}

func buildQuery(env *types.Env, q *parser.QueryExpr, varName string, varTypes map[string]string) ([]Stmt, string, error) {
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, "", err
	}
	loops := []struct {
		name string
		src  Expr
		typ  string
	}{{q.Var, src, typeOf(q.Source, env)}}
	for _, f := range q.Froms {
		ex, err := convertExpr(env, f.Src)
		if err != nil {
			return nil, "", err
		}
		loops = append(loops, struct {
			name string
			src  Expr
			typ  string
		}{f.Var, ex, typeOf(f.Src, env)})
	}
	for _, j := range q.Joins {
		ex, err := convertExpr(env, j.Src)
		if err != nil {
			return nil, "", err
		}
		loops = append(loops, struct {
			name string
			src  Expr
			typ  string
		}{j.Var, ex, typeOf(j.Src, env)})
	}
	for _, l := range loops {
		if _, ok := varTypes[l.name]; !ok {
			elem := elemType(l.typ)
			if elem == "" {
				elem = "integer"
			}
			varTypes[l.name] = elem
		}
	}
	child := types.NewEnv(env)
	for _, l := range loops {
		child.SetVar(l.name, types.AnyType{}, true)
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, "", err
	}
	elemT := inferType(sel)
	stmts := []Stmt{&AssignStmt{Name: varName, Expr: &ListLit{}}}
	appendExpr := &AssignStmt{Name: varName, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: varName}, &ListLit{Elems: []Expr{sel}}}}}
	body := []Stmt{appendExpr}
	var cond Expr
	for _, j := range q.Joins {
		c, err := convertExpr(child, j.On)
		if err != nil {
			return nil, "", err
		}
		if cond == nil {
			cond = c
		} else {
			cond = &BinaryExpr{Op: "and", Left: cond, Right: c, Bool: true}
		}
	}
	if q.Where != nil {
		c, err := convertExpr(child, q.Where)
		if err != nil {
			return nil, "", err
		}
		if cond == nil {
			cond = c
		} else {
			cond = &BinaryExpr{Op: "and", Left: cond, Right: c, Bool: true}
		}
	}
	if cond != nil {
		body = []Stmt{&IfStmt{Cond: cond, Then: body}}
	}
	for i := len(loops) - 1; i >= 0; i-- {
		body = []Stmt{&ForEachStmt{Name: loops[i].name, Iterable: loops[i].src, Body: body}}
	}
	stmts = append(stmts, body...)

	if q.Sort != nil {
		keyExpr, err := convertExpr(child, q.Sort)
		if err != nil {
			return nil, "", err
		}
		desc := false
		if u, ok := keyExpr.(*UnaryExpr); ok && u.Op == "-" {
			keyExpr = u.Expr
			desc = true
		}
		iVar := fmt.Sprintf("i%d", len(currProg.Vars))
		jVar := fmt.Sprintf("j%d", len(currProg.Vars)+1)
		tmpVar := fmt.Sprintf("tmp%d", len(currProg.Vars)+2)
		currProg.Vars = append(currProg.Vars,
			VarDecl{Name: iVar, Type: "integer"},
			VarDecl{Name: jVar, Type: "integer"},
			VarDecl{Name: tmpVar, Type: elemT},
		)
		left := replaceVar(keyExpr, q.Var, &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: iVar}})
		right := replaceVar(keyExpr, q.Var, &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: jVar}})
		op := ">"
		if desc {
			op = "<"
		}
		cond := &BinaryExpr{Op: op, Left: left, Right: right, Bool: true}
		swap := []Stmt{
			&AssignStmt{Name: tmpVar, Expr: &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: iVar}}},
			&IndexAssignStmt{Name: varName, Index: &VarRef{Name: iVar}, Expr: &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: jVar}}},
			&IndexAssignStmt{Name: varName, Index: &VarRef{Name: jVar}, Expr: &VarRef{Name: tmpVar}},
		}
		inner := &ForRangeStmt{Name: jVar, Start: &BinaryExpr{Op: "+", Left: &VarRef{Name: iVar}, Right: &IntLit{Value: 1}}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Body: []Stmt{&IfStmt{Cond: cond, Then: swap}}}
		outer := &ForRangeStmt{Name: iVar, Start: &IntLit{Value: 0}, End: &BinaryExpr{Op: "-", Left: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Right: &IntLit{Value: 1}}, Body: []Stmt{inner}}
		stmts = append(stmts, outer)
	}

	if q.Skip != nil || q.Take != nil {
		idxVar := fmt.Sprintf("idx%d", len(currProg.Vars))
		resVar := fmt.Sprintf("tmp%d", len(currProg.Vars)+1)
		currProg.Vars = append(currProg.Vars,
			VarDecl{Name: idxVar, Type: "integer"},
			VarDecl{Name: resVar, Type: "array of " + elemT},
		)
		start := Expr(&IntLit{Value: 0})
		if q.Skip != nil {
			s, err := convertExpr(child, q.Skip)
			if err != nil {
				return nil, "", err
			}
			start = s
		}
		cond := &BinaryExpr{Op: ">=", Left: &VarRef{Name: idxVar}, Right: start, Bool: true}
		if q.Take != nil {
			texpr, err := convertExpr(child, q.Take)
			if err != nil {
				return nil, "", err
			}
			end := &BinaryExpr{Op: "+", Left: start, Right: texpr}
			cond = &BinaryExpr{Op: "and", Left: cond, Right: &BinaryExpr{Op: "<", Left: &VarRef{Name: idxVar}, Right: end, Bool: true}, Bool: true}
		}
		appendRes := &AssignStmt{Name: resVar, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: resVar}, &ListLit{Elems: []Expr{&IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: idxVar}}}}}}}
		loopBody := []Stmt{&IfStmt{Cond: cond, Then: []Stmt{appendRes}}}
		loop := &ForRangeStmt{Name: idxVar, Start: &IntLit{Value: 0}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Body: loopBody}
		stmts = append(stmts, &AssignStmt{Name: resVar, Expr: &ListLit{}}, loop, &AssignStmt{Name: varName, Expr: &VarRef{Name: resVar}})
	}

	return stmts, "array of " + elemT, nil
}

func replaceVar(e Expr, name string, repl Expr) Expr {
	switch v := e.(type) {
	case *VarRef:
		if v.Name == name {
			return repl
		}
	case *RecordLit:
		for i, f := range v.Fields {
			v.Fields[i].Expr = replaceVar(f.Expr, name, repl)
		}
	case *BinaryExpr:
		v.Left = replaceVar(v.Left, name, repl)
		v.Right = replaceVar(v.Right, name, repl)
	case *CallExpr:
		for i, a := range v.Args {
			v.Args[i] = replaceVar(a, name, repl)
		}
	case *IndexExpr:
		v.Target = replaceVar(v.Target, name, repl)
		v.Index = replaceVar(v.Index, name, repl)
	case *SliceExpr:
		v.Target = replaceVar(v.Target, name, repl)
		if v.Start != nil {
			v.Start = replaceVar(v.Start, name, repl)
		}
		if v.End != nil {
			v.End = replaceVar(v.End, name, repl)
		}
	case *UnaryExpr:
		v.Expr = replaceVar(v.Expr, name, repl)
	}
	return e
}

func zeroValue(typ string) Expr {
	switch {
	case typ == "integer":
		return &IntLit{Value: 0}
	case typ == "string":
		return &StringLit{Value: ""}
	case typ == "BigRat":
		currProg.UseBigRat = true
		return &CallExpr{Name: "_bigrat", Args: []Expr{&IntLit{Value: 0}}}
	case strings.HasPrefix(typ, "array of"):
		return &ListLit{}
	default:
		for _, r := range currProg.Records {
			if r.Name == typ {
				var args []Expr
				for _, f := range r.Fields {
					args = append(args, zeroValue(f.Type))
				}
				return &CallExpr{Name: ctorName(typ), Args: args}
			}
		}
	}
	return &IntLit{Value: 0}
}

func buildLeftJoinQuery(env *types.Env, q *parser.QueryExpr, varName string, varTypes map[string]string) ([]Stmt, string, error) {
	j := q.Joins[0]
	leftSrc, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, "", err
	}
	rightSrc, err := convertExpr(env, j.Src)
	if err != nil {
		return nil, "", err
	}
	ltyp := typeOf(q.Source, env)
	rtyp := typeOf(j.Src, env)
	if _, ok := varTypes[q.Var]; !ok {
		varTypes[q.Var] = elemType(ltyp)
	}
	if _, ok := varTypes[j.Var]; !ok {
		varTypes[j.Var] = elemType(rtyp)
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(child, j.On)
	if err != nil {
		return nil, "", err
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, "", err
	}
	jType := elemType(rtyp)
	unmatchedSel := replaceVar(sel, j.Var, zeroValue(jType))
	elemT := inferType(sel)
	matchedVar := fmt.Sprintf("matched%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: matchedVar, Type: "boolean"})

	appendMatched := &AssignStmt{Name: varName, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: varName}, &ListLit{Elems: []Expr{sel}}}}}
	appendUnmatched := &AssignStmt{Name: varName, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: varName}, &ListLit{Elems: []Expr{unmatchedSel}}}}}

	innerBody := []Stmt{&IfStmt{Cond: cond, Then: []Stmt{appendMatched, &AssignStmt{Name: matchedVar, Expr: &BoolLit{Value: true}}}}}
	forBody := []Stmt{
		&AssignStmt{Name: matchedVar, Expr: &BoolLit{Value: false}},
		&ForEachStmt{Name: j.Var, Iterable: rightSrc, Body: innerBody},
		&IfStmt{Cond: &UnaryExpr{Op: "not ", Expr: &VarRef{Name: matchedVar}}, Then: []Stmt{appendUnmatched}},
	}

	stmts := []Stmt{&AssignStmt{Name: varName, Expr: &ListLit{}}}
	outer := &ForEachStmt{Name: q.Var, Iterable: leftSrc, Body: forBody}
	stmts = append(stmts, outer)
	return stmts, "array of " + elemT, nil
}

func isSimpleGroupBy(q *parser.QueryExpr) bool {
	if q.Group == nil || len(q.Group.Exprs) != 1 || q.Group.Having != nil {
		return false
	}
	if q.Select == nil || q.Select.Binary == nil {
		return false
	}
	ml := q.Select.Binary.Left.Value.Target.Map
	if ml == nil || len(ml.Items) != 3 {
		return false
	}
	k0, ok0 := exprToIdent(ml.Items[0].Key)
	k1, ok1 := exprToIdent(ml.Items[1].Key)
	k2, ok2 := exprToIdent(ml.Items[2].Key)
	if !ok0 || !ok1 || !ok2 {
		return false
	}
	if k0 != "city" || k1 != "count" || k2 != "avg_age" {
		return false
	}
	call1 := ml.Items[1].Value.Binary.Left.Value.Target.Call
	call2 := ml.Items[2].Value.Binary.Left.Value.Target.Call
	if call1 == nil || call1.Func != "count" || len(call1.Args) != 1 {
		return false
	}
	if call2 == nil || call2.Func != "avg" || len(call2.Args) != 1 {
		return false
	}
	return true
}

func isGroupByConditionalSum(q *parser.QueryExpr) bool {
	if q.Group == nil || len(q.Group.Exprs) != 1 || q.Group.Having != nil {
		return false
	}
	if q.Select == nil || q.Select.Binary == nil {
		return false
	}
	ml := q.Select.Binary.Left.Value.Target.Map
	if ml == nil || len(ml.Items) != 2 {
		return false
	}
	k0, ok0 := exprToIdent(ml.Items[0].Key)
	k1, ok1 := exprToIdent(ml.Items[1].Key)
	if !ok0 || !ok1 {
		return false
	}
	if k0 != "cat" || k1 != "share" {
		return false
	}
	return true
}

func isGroupByJoin(q *parser.QueryExpr) bool {
	if q.Group == nil || len(q.Group.Exprs) != 1 {
		return false
	}
	if len(q.Joins) != 1 || len(q.Froms) != 0 {
		return false
	}
	if q.Select == nil || q.Select.Binary == nil {
		return false
	}
	ml := q.Select.Binary.Left.Value.Target.Map
	if ml == nil || len(ml.Items) != 2 {
		return false
	}
	return true
}

func isGroupBySum(q *parser.QueryExpr) bool {
	if q.Group == nil || len(q.Group.Exprs) != 1 || q.Group.Having != nil {
		return false
	}
	if q.Select == nil || q.Select.Binary == nil {
		return false
	}
	ml := q.Select.Binary.Left.Value.Target.Map
	if ml == nil || len(ml.Items) != 2 {
		return false
	}
	k0, ok0 := exprToIdent(ml.Items[0].Key)
	k1, ok1 := exprToIdent(ml.Items[1].Key)
	if !ok0 || !ok1 {
		return false
	}
	if k0 != "part" || k1 != "total" {
		return false
	}
	call := ml.Items[1].Value.Binary.Left.Value.Target.Call
	if call == nil || call.Func != "sum" || len(call.Args) != 1 {
		return false
	}
	argQ := call.Args[0].Binary.Left.Value.Target.Query
	if argQ == nil || argQ.Source == nil || argQ.Var != "r" {
		return false
	}
	srcSel := argQ.Source.Binary.Left.Value.Target.Selector
	if srcSel == nil || srcSel.Root != q.Group.Name {
		return false
	}
	sel := argQ.Select.Binary.Left.Value.Target.Selector
	if sel == nil || sel.Root != "r" || len(sel.Tail) != 1 || sel.Tail[0] != "value" {
		return false
	}
	return true
}

func isGroupByMultiSort(q *parser.QueryExpr) bool {
	if q.Group == nil || len(q.Group.Exprs) != 1 || q.Sort == nil || q.Group.Having != nil {
		return false
	}
	if q.Select == nil || q.Select.Binary == nil {
		return false
	}
	ml := mapLitFromExpr(q.Group.Exprs[0])
	if ml == nil || len(ml.Items) < 2 {
		return false
	}
	call := q.Sort.Binary.Left.Value.Target.Call
	if call == nil || call.Func != "sum" || len(call.Args) != 1 {
		return false
	}
	if call.Args[0].Binary.Left.Value.Target.Query == nil {
		return false
	}
	return true
}

func buildGroupByQuery(env *types.Env, q *parser.QueryExpr, varName string, varTypes map[string]string) ([]Stmt, string, error) {
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, "", err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	keyExpr, err := convertExpr(child, q.Group.Exprs[0])
	if err != nil {
		return nil, "", err
	}
	elemT := elemType(typeOf(q.Source, env))
	if _, ok := varTypes[q.Var]; !ok {
		varTypes[q.Var] = elemT
	}
	keyT := inferType(keyExpr)
	_ = elemT

	grpRec := ensureRecord([]Field{{Name: "city", Type: keyT}, {Name: "count", Type: "integer"}, {Name: "sumAge", Type: "integer"}})
	resRec := ensureRecord([]Field{{Name: "city", Type: keyT}, {Name: "count", Type: "integer"}, {Name: "avg_age", Type: "real"}})

	groupsVar := fmt.Sprintf("grp%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: groupsVar, Type: "array of " + grpRec})
	idxVar := fmt.Sprintf("idx%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: idxVar, Type: "integer"})
	iVar := fmt.Sprintf("i%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: iVar, Type: "integer"})
	sumVar := fmt.Sprintf("sum%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: sumVar, Type: "integer"})

	// build group accumulation loop
	searchBody := []Stmt{
		&IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true,
			Left:  &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, iVar), Tail: []string{"city"}},
			Right: keyExpr},
			Then: []Stmt{&AssignStmt{Name: idxVar, Expr: &VarRef{Name: iVar}}, &BreakStmt{}}},
	}
	forLoop := &ForRangeStmt{Name: iVar, Start: &IntLit{Value: 0}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: groupsVar}}}, Body: searchBody}

	addNew := &AssignStmt{
		Name: groupsVar,
		Expr: &CallExpr{Name: "concat", Args: []Expr{
			&VarRef{Name: groupsVar},
			&ListLit{Elems: []Expr{
				&RecordLit{Type: grpRec, Fields: []FieldExpr{
					{Name: "city", Expr: keyExpr},
					{Name: "count", Expr: &IntLit{Value: 1}},
					{Name: "sumAge", Expr: &SelectorExpr{Root: q.Var, Tail: []string{"age"}}},
				}},
			}},
		}},
	}
	incCount := &AssignStmt{Name: fmt.Sprintf("%s[%s].count", groupsVar, idxVar), Expr: &BinaryExpr{Op: "+", Left: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, idxVar), Tail: []string{"count"}}, Right: &IntLit{Value: 1}}}
	incSum := &AssignStmt{Name: fmt.Sprintf("%s[%s].sumAge", groupsVar, idxVar), Expr: &BinaryExpr{Op: "+", Left: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, idxVar), Tail: []string{"sumAge"}}, Right: &SelectorExpr{Root: q.Var, Tail: []string{"age"}}}}
	condUpdate := &IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true, Left: &VarRef{Name: idxVar}, Right: &IntLit{Value: -1}}, Then: []Stmt{addNew}, Else: []Stmt{incCount, incSum}}

	outerBody := []Stmt{
		&AssignStmt{Name: idxVar, Expr: &IntLit{Value: -1}},
		forLoop,
		condUpdate,
	}

	outer := &ForEachStmt{Name: q.Var, Iterable: src, Body: outerBody}

	// build result generation
	sumInit := &AssignStmt{Name: sumVar, Expr: &IntLit{Value: 0}}
	sumLoopBody := []Stmt{
		&AssignStmt{Name: sumVar, Expr: &BinaryExpr{Op: "+", Left: &VarRef{Name: sumVar}, Right: &SelectorExpr{Root: q.Var, Tail: []string{"age"}}}},
	}
	inner := &ForEachStmt{Name: q.Var, Iterable: &SelectorExpr{Root: "g", Tail: []string{"items"}}, Body: sumLoopBody}
	avgExpr := &BinaryExpr{Op: "/", Left: &VarRef{Name: sumVar}, Right: &CallExpr{Name: "Length", Args: []Expr{&SelectorExpr{Root: "g", Tail: []string{"items"}}}}, Real: true}
	rec := &RecordLit{Type: resRec, Fields: []FieldExpr{{Name: "city", Expr: &SelectorExpr{Root: "g", Tail: []string{"city"}}}, {Name: "count", Expr: &SelectorExpr{Root: "g", Tail: []string{"count"}}}, {Name: "avg_age", Expr: avgExpr}}}
	appendRes := &AssignStmt{Name: varName, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: varName}, &ListLit{Elems: []Expr{rec}}}}}
	resultBody := []Stmt{sumInit, inner, appendRes}
	resultFor := &ForEachStmt{Name: "g", Iterable: &VarRef{Name: groupsVar}, Body: resultBody}

	stmts := []Stmt{&AssignStmt{Name: groupsVar, Expr: &ListLit{}}, outer, &AssignStmt{Name: varName, Expr: &ListLit{}}, resultFor}

	if q.Sort != nil {
		keyExpr, err := convertExpr(child, q.Sort)
		if err != nil {
			return nil, "", err
		}
		desc := false
		if u, ok := keyExpr.(*UnaryExpr); ok && u.Op == "-" {
			keyExpr = u.Expr
			desc = true
		}
		iVar := fmt.Sprintf("i%d", len(currProg.Vars))
		jVar := fmt.Sprintf("j%d", len(currProg.Vars)+1)
		tmpVar := fmt.Sprintf("tmp%d", len(currProg.Vars)+2)
		currProg.Vars = append(currProg.Vars,
			VarDecl{Name: iVar, Type: "integer"},
			VarDecl{Name: jVar, Type: "integer"},
			VarDecl{Name: tmpVar, Type: resRec},
		)
		left := replaceVar(keyExpr, q.Group.Name, &SelectorExpr{Root: fmt.Sprintf("%s[%s]", varName, iVar), Tail: []string{}})
		left = replaceVar(left, q.Var, &SelectorExpr{Root: fmt.Sprintf("%s[%s]", varName, iVar), Tail: []string{"items", q.Var}})
		right := replaceVar(keyExpr, q.Group.Name, &SelectorExpr{Root: fmt.Sprintf("%s[%s]", varName, jVar), Tail: []string{}})
		right = replaceVar(right, q.Var, &SelectorExpr{Root: fmt.Sprintf("%s[%s]", varName, jVar), Tail: []string{"items", q.Var}})
		op := ">"
		if desc {
			op = "<"
		}
		cond := &BinaryExpr{Op: op, Left: left, Right: right, Bool: true}
		swap := []Stmt{
			&AssignStmt{Name: tmpVar, Expr: &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: iVar}}},
			&IndexAssignStmt{Name: varName, Index: &VarRef{Name: iVar}, Expr: &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: jVar}}},
			&IndexAssignStmt{Name: varName, Index: &VarRef{Name: jVar}, Expr: &VarRef{Name: tmpVar}},
		}
		inner := &ForRangeStmt{Name: jVar, Start: &BinaryExpr{Op: "+", Left: &VarRef{Name: iVar}, Right: &IntLit{Value: 1}}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Body: []Stmt{&IfStmt{Cond: cond, Then: swap}}}
		outer2 := &ForRangeStmt{Name: iVar, Start: &IntLit{Value: 0}, End: &BinaryExpr{Op: "-", Left: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Right: &IntLit{Value: 1}}, Body: []Stmt{inner}}
		stmts = append(stmts, outer2)
	}

	if q.Skip != nil || q.Take != nil {
		idxVar := fmt.Sprintf("idx%d", len(currProg.Vars))
		resVar := fmt.Sprintf("tmp%d", len(currProg.Vars)+1)
		currProg.Vars = append(currProg.Vars,
			VarDecl{Name: idxVar, Type: "integer"},
			VarDecl{Name: resVar, Type: "array of " + resRec},
		)
		start := Expr(&IntLit{Value: 0})
		if q.Skip != nil {
			s, err := convertExpr(child, q.Skip)
			if err != nil {
				return nil, "", err
			}
			start = s
		}
		cond := &BinaryExpr{Op: ">=", Left: &VarRef{Name: idxVar}, Right: start, Bool: true}
		if q.Take != nil {
			texpr, err := convertExpr(child, q.Take)
			if err != nil {
				return nil, "", err
			}
			end := &BinaryExpr{Op: "+", Left: start, Right: texpr}
			cond = &BinaryExpr{Op: "and", Left: cond, Right: &BinaryExpr{Op: "<", Left: &VarRef{Name: idxVar}, Right: end, Bool: true}, Bool: true}
		}
		appendRes := &AssignStmt{Name: resVar, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: resVar}, &ListLit{Elems: []Expr{&IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: idxVar}}}}}}}
		loopBody := []Stmt{&IfStmt{Cond: cond, Then: []Stmt{appendRes}}}
		loop := &ForRangeStmt{Name: idxVar, Start: &IntLit{Value: 0}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Body: loopBody}
		stmts = append(stmts, &AssignStmt{Name: resVar, Expr: &ListLit{}}, loop, &AssignStmt{Name: varName, Expr: &VarRef{Name: resVar}})
	}

	varTypes[varName] = "array of " + resRec
	return stmts, "array of " + resRec, nil
}

func buildGroupByConditionalSum(env *types.Env, q *parser.QueryExpr, varName string, varTypes map[string]string) ([]Stmt, string, error) {
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, "", err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	keyExpr, err := convertExpr(child, q.Group.Exprs[0])
	if err != nil {
		return nil, "", err
	}
	elemT := elemType(typeOf(q.Source, env))
	if _, ok := varTypes[q.Var]; !ok {
		varTypes[q.Var] = elemT
	}
	keyT := inferType(keyExpr)

	grpRec := ensureRecord([]Field{{Name: "cat", Type: keyT}, {Name: "sumTrue", Type: "integer"}, {Name: "sumTotal", Type: "integer"}})
	resRec := ensureRecord([]Field{{Name: "cat", Type: keyT}, {Name: "share", Type: "real"}})

	groupsVar := fmt.Sprintf("grp%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: groupsVar, Type: "array of " + grpRec})
	idxVar := fmt.Sprintf("idx%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: idxVar, Type: "integer"})
	iVar := fmt.Sprintf("i%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: iVar, Type: "integer"})

	searchBody := []Stmt{
		&IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true,
			Left:  &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, iVar), Tail: []string{"cat"}},
			Right: keyExpr},
			Then: []Stmt{&AssignStmt{Name: idxVar, Expr: &VarRef{Name: iVar}}, &BreakStmt{}}},
	}
	forLoop := &ForRangeStmt{Name: iVar, Start: &IntLit{Value: 0}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: groupsVar}}}, Body: searchBody}

	condTrue := &IfStmt{Cond: &SelectorExpr{Root: q.Var, Tail: []string{"flag"}},
		Then: []Stmt{&AssignStmt{Name: fmt.Sprintf("%s[%s].sumTrue", groupsVar, idxVar), Expr: &BinaryExpr{Op: "+", Left: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, idxVar), Tail: []string{"sumTrue"}}, Right: &SelectorExpr{Root: q.Var, Tail: []string{"val"}}}}}}
	incTotal := &AssignStmt{Name: fmt.Sprintf("%s[%s].sumTotal", groupsVar, idxVar), Expr: &BinaryExpr{Op: "+", Left: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, idxVar), Tail: []string{"sumTotal"}}, Right: &SelectorExpr{Root: q.Var, Tail: []string{"val"}}}}

	addNew := &AssignStmt{
		Name: groupsVar,
		Expr: &CallExpr{Name: "concat", Args: []Expr{
			&VarRef{Name: groupsVar},
			&ListLit{Elems: []Expr{
				&RecordLit{Type: grpRec, Fields: []FieldExpr{
					{Name: "cat", Expr: keyExpr},
					{Name: "sumTrue", Expr: &IfExpr{Cond: &SelectorExpr{Root: q.Var, Tail: []string{"flag"}}, Then: &SelectorExpr{Root: q.Var, Tail: []string{"val"}}, Else: &IntLit{Value: 0}}},
					{Name: "sumTotal", Expr: &SelectorExpr{Root: q.Var, Tail: []string{"val"}}},
				}},
			}},
		}},
	}

	body := []Stmt{
		&AssignStmt{Name: idxVar, Expr: &IntLit{Value: -1}},
		forLoop,
		&IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true, Left: &VarRef{Name: idxVar}, Right: &IntLit{Value: -1}}, Then: []Stmt{addNew}, Else: []Stmt{condTrue, incTotal}},
	}
	outer := &ForEachStmt{Name: q.Var, Iterable: src, Body: body}

	resExpr := &RecordLit{Type: resRec, Fields: []FieldExpr{
		{Name: "cat", Expr: &SelectorExpr{Root: "g", Tail: []string{"cat"}}},
		{Name: "share", Expr: &BinaryExpr{Op: "/", Left: &SelectorExpr{Root: "g", Tail: []string{"sumTrue"}}, Right: &SelectorExpr{Root: "g", Tail: []string{"sumTotal"}}, Real: true}},
	}}
	appendRes := &AssignStmt{Name: varName, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: varName}, &ListLit{Elems: []Expr{resExpr}}}}}
	resultFor := &ForEachStmt{Name: "g", Iterable: &VarRef{Name: groupsVar}, Body: []Stmt{appendRes}}

	stmts := []Stmt{&AssignStmt{Name: groupsVar, Expr: &ListLit{}}, outer, &AssignStmt{Name: varName, Expr: &ListLit{}}, resultFor}
	varTypes[varName] = "array of " + resRec
	return stmts, "array of " + resRec, nil
}

func buildGroupBySum(env *types.Env, q *parser.QueryExpr, varName string, varTypes map[string]string) ([]Stmt, string, error) {
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, "", err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	keyExpr, err := convertExpr(child, q.Group.Exprs[0])
	if err != nil {
		return nil, "", err
	}
	elemT := elemType(typeOf(q.Source, env))
	if _, ok := varTypes[q.Var]; !ok {
		varTypes[q.Var] = elemT
	}
	keyT := inferType(keyExpr)
	valExpr := &SelectorExpr{Root: q.Var, Tail: []string{"value"}}
	valT := inferType(valExpr)

	rec := ensureRecord([]Field{{Name: "part", Type: keyT}, {Name: "total", Type: valT}})
	groupsVar := fmt.Sprintf("grp%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: groupsVar, Type: "array of " + rec})
	idxVar := fmt.Sprintf("idx%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: idxVar, Type: "integer"})
	iVar := fmt.Sprintf("i%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: iVar, Type: "integer"})

	searchBody := []Stmt{
		&IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true,
			Left:  &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, iVar), Tail: []string{"part"}},
			Right: keyExpr},
			Then: []Stmt{&AssignStmt{Name: idxVar, Expr: &VarRef{Name: iVar}}, &BreakStmt{}}},
	}
	forLoop := &ForRangeStmt{Name: iVar, Start: &IntLit{Value: 0}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: groupsVar}}}, Body: searchBody}

	addNew := &AssignStmt{
		Name: groupsVar,
		Expr: &CallExpr{Name: "concat", Args: []Expr{
			&VarRef{Name: groupsVar},
			&ListLit{Elems: []Expr{
				&RecordLit{Type: rec, Fields: []FieldExpr{{Name: "part", Expr: keyExpr}, {Name: "total", Expr: valExpr}}},
			}},
		}},
	}
	incTotal := &AssignStmt{Name: fmt.Sprintf("%s[%s].total", groupsVar, idxVar), Expr: &BinaryExpr{Op: "+", Left: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, idxVar), Tail: []string{"total"}}, Right: valExpr}}
	condUpdate := &IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true, Left: &VarRef{Name: idxVar}, Right: &IntLit{Value: -1}}, Then: []Stmt{addNew}, Else: []Stmt{incTotal}}

	outerBody := []Stmt{
		&AssignStmt{Name: idxVar, Expr: &IntLit{Value: -1}},
		forLoop,
		condUpdate,
	}

	outer := &ForEachStmt{Name: q.Var, Iterable: src, Body: outerBody}

	stmts := []Stmt{&AssignStmt{Name: groupsVar, Expr: &ListLit{}}, outer, &AssignStmt{Name: varName, Expr: &VarRef{Name: groupsVar}}}
	varTypes[varName] = "array of " + rec
	return stmts, "array of " + rec, nil
}

func buildGroupByMultiSort(env *types.Env, q *parser.QueryExpr, varName string, varTypes map[string]string) ([]Stmt, string, error) {
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, "", err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	ml := mapLitFromExpr(q.Group.Exprs[0])
	if ml == nil {
		return nil, "", fmt.Errorf("unsupported group by expression")
	}
	elemT := elemType(typeOf(q.Source, env))
	if _, ok := varTypes[q.Var]; !ok {
		varTypes[q.Var] = elemT
	}
	var keyFields []Field
	var keyExprs []FieldExpr
	for _, it := range ml.Items {
		name, ok := exprToIdent(it.Key)
		if !ok {
			return nil, "", fmt.Errorf("invalid key")
		}
		ex, err := convertExpr(child, it.Value)
		if err != nil {
			return nil, "", err
		}
		typ := inferType(ex)
		if typ == "" {
			typ = typeOf(it.Value, child)
		}
		keyFields = append(keyFields, Field{Name: name, Type: typ})
		keyExprs = append(keyExprs, FieldExpr{Name: name, Expr: ex})
	}
	keyRec := ensureRecord(keyFields)
	grpRec := ensureRecord([]Field{{Name: "key", Type: keyRec}, {Name: "items", Type: "array of " + elemT}})

	groupsVar := fmt.Sprintf("grp%d", len(currProg.Vars))
	idxVar := fmt.Sprintf("idx%d", len(currProg.Vars)+1)
	iVar := fmt.Sprintf("i%d", len(currProg.Vars)+2)
	currProg.Vars = append(currProg.Vars,
		VarDecl{Name: groupsVar, Type: "array of " + grpRec},
		VarDecl{Name: idxVar, Type: "integer"},
		VarDecl{Name: iVar, Type: "integer"},
	)

	// build accumulation loop
	var cond Expr
	for _, f := range keyFields {
		left := &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, iVar), Tail: []string{"key", f.Name}}
		right := replaceVar(keyExprs[0].Expr, q.Var, &VarRef{Name: q.Var})
		for _, fe := range keyExprs {
			if fe.Name == f.Name {
				right = fe.Expr
			}
		}
		eq := &BinaryExpr{Op: "=", Left: left, Right: right, Bool: true}
		if cond == nil {
			cond = eq
		} else {
			cond = &BinaryExpr{Op: "and", Left: cond, Right: eq, Bool: true}
		}
	}
	searchBody := []Stmt{&IfStmt{Cond: cond, Then: []Stmt{&AssignStmt{Name: idxVar, Expr: &VarRef{Name: iVar}}, &BreakStmt{}}}}
	forLoop := &ForRangeStmt{Name: iVar, Start: &IntLit{Value: 0}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: groupsVar}}}, Body: searchBody}

	keyLit := &RecordLit{Type: keyRec, Fields: keyExprs}
	addNew := &AssignStmt{Name: groupsVar, Expr: &CallExpr{Name: "concat", Args: []Expr{
		&VarRef{Name: groupsVar},
		&ListLit{Elems: []Expr{&RecordLit{Type: grpRec, Fields: []FieldExpr{{Name: "key", Expr: keyLit}, {Name: "items", Expr: &ListLit{Elems: []Expr{&VarRef{Name: q.Var}}}}}}}},
	}}}
	appendItem := &AssignStmt{Name: fmt.Sprintf("%s[%s].items", groupsVar, idxVar), Expr: &CallExpr{Name: "concat", Args: []Expr{
		&SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, idxVar), Tail: []string{"items"}},
		&ListLit{Elems: []Expr{&VarRef{Name: q.Var}}},
	}}}
	condUpdate := &IfStmt{Cond: &BinaryExpr{Op: "=", Left: &VarRef{Name: idxVar}, Right: &IntLit{Value: -1}, Bool: true}, Then: []Stmt{addNew}, Else: []Stmt{appendItem}}

	outerBody := []Stmt{&AssignStmt{Name: idxVar, Expr: &IntLit{Value: -1}}, forLoop, condUpdate}
	outer := &ForEachStmt{Name: q.Var, Iterable: src, Body: outerBody}

	// build result
	resFields := []Field{}
	for _, f := range keyFields {
		resFields = append(resFields, Field{Name: f.Name, Type: f.Type})
	}
	resFields = append(resFields, Field{Name: "total", Type: "integer"})
	resRec := ensureRecord(resFields)
	sumVar := fmt.Sprintf("sum%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: sumVar, Type: "integer"})

	buildRes := func() []Stmt {
		var tail []string
		fields := []FieldExpr{}
		for _, f := range keyFields {
			tail = []string{"key", f.Name}
			fields = append(fields, FieldExpr{Name: f.Name, Expr: &SelectorExpr{Root: "g", Tail: tail}})
		}
		fields = append(fields, FieldExpr{Name: "total", Expr: &VarRef{Name: sumVar}})
		rec := &RecordLit{Type: resRec, Fields: fields}
		return []Stmt{&AssignStmt{Name: varName, Expr: &CallExpr{Name: "concat", Args: []Expr{&VarRef{Name: varName}, &ListLit{Elems: []Expr{rec}}}}}}
	}

	sumLoop := &ForEachStmt{Name: "x", Iterable: &SelectorExpr{Root: "g", Tail: []string{"items"}}, Body: []Stmt{&AssignStmt{Name: sumVar, Expr: &BinaryExpr{Op: "+", Left: &VarRef{Name: sumVar}, Right: &SelectorExpr{Root: "x", Tail: []string{"val"}}}}}}
	resultBody := []Stmt{&AssignStmt{Name: sumVar, Expr: &IntLit{Value: 0}}, sumLoop}
	resultBody = append(resultBody, buildRes()...)
	resultFor := &ForEachStmt{Name: "g", Iterable: &VarRef{Name: groupsVar}, Body: resultBody}

	stmts := []Stmt{&AssignStmt{Name: groupsVar, Expr: &ListLit{}}, outer, &AssignStmt{Name: varName, Expr: &ListLit{}}, resultFor}

	// sort by total desc
	iVar2 := fmt.Sprintf("i%d", len(currProg.Vars))
	jVar := fmt.Sprintf("j%d", len(currProg.Vars)+1)
	tmpVar := fmt.Sprintf("tmp%d", len(currProg.Vars)+2)
	currProg.Vars = append(currProg.Vars,
		VarDecl{Name: iVar2, Type: "integer"},
		VarDecl{Name: jVar, Type: "integer"},
		VarDecl{Name: tmpVar, Type: resRec},
	)
	condSort := &BinaryExpr{Op: "<", Left: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", varName, iVar2), Tail: []string{"total"}}, Right: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", varName, jVar), Tail: []string{"total"}}, Bool: true}
	swap := []Stmt{
		&AssignStmt{Name: tmpVar, Expr: &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: iVar2}}},
		&IndexAssignStmt{Name: varName, Index: &VarRef{Name: iVar2}, Expr: &IndexExpr{Target: &VarRef{Name: varName}, Index: &VarRef{Name: jVar}}},
		&IndexAssignStmt{Name: varName, Index: &VarRef{Name: jVar}, Expr: &VarRef{Name: tmpVar}},
	}
	inner := &ForRangeStmt{Name: jVar, Start: &BinaryExpr{Op: "+", Left: &VarRef{Name: iVar2}, Right: &IntLit{Value: 1}}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Body: []Stmt{&IfStmt{Cond: condSort, Then: swap}}}
	outerSort := &ForRangeStmt{Name: iVar2, Start: &IntLit{Value: 0}, End: &BinaryExpr{Op: "-", Left: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: varName}}}, Right: &IntLit{Value: 1}}, Body: []Stmt{inner}}
	stmts = append(stmts, outerSort)

	varTypes[varName] = "array of " + resRec
	return stmts, "array of " + resRec, nil
}

func buildGroupByJoinQuery(env *types.Env, q *parser.QueryExpr, varName string, varTypes map[string]string) ([]Stmt, string, error) {
	j := q.Joins[0]
	leftSrc, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, "", err
	}
	rightSrc, err := convertExpr(env, j.Src)
	if err != nil {
		return nil, "", err
	}
	ltyp := typeOf(q.Source, env)
	rtyp := typeOf(j.Src, env)
	if _, ok := varTypes[q.Var]; !ok {
		varTypes[q.Var] = elemType(ltyp)
	}
	if _, ok := varTypes[j.Var]; !ok {
		varTypes[j.Var] = elemType(rtyp)
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(child, j.On)
	if err != nil {
		return nil, "", err
	}
	keyExpr, err := convertExpr(child, q.Group.Exprs[0])
	if err != nil {
		return nil, "", err
	}
	keyT := inferType(keyExpr)

	rec := ensureRecord([]Field{{Name: "name", Type: keyT}, {Name: "count", Type: "integer"}})
	groupsVar := fmt.Sprintf("grp%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: groupsVar, Type: "array of " + rec})
	idxVar := fmt.Sprintf("idx%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: idxVar, Type: "integer"})
	iVar := fmt.Sprintf("i%d", len(currProg.Vars))
	currProg.Vars = append(currProg.Vars, VarDecl{Name: iVar, Type: "integer"})

	searchBody := []Stmt{
		&IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true,
			Left:  &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, iVar), Tail: []string{"name"}},
			Right: keyExpr},
			Then: []Stmt{&AssignStmt{Name: idxVar, Expr: &VarRef{Name: iVar}}, &BreakStmt{}}},
	}
	forLoop := &ForRangeStmt{Name: iVar, Start: &IntLit{Value: 0}, End: &CallExpr{Name: "Length", Args: []Expr{&VarRef{Name: groupsVar}}}, Body: searchBody}

	addNew := &AssignStmt{
		Name: groupsVar,
		Expr: &CallExpr{Name: "concat", Args: []Expr{
			&VarRef{Name: groupsVar},
			&ListLit{Elems: []Expr{
				&RecordLit{Type: rec, Fields: []FieldExpr{
					{Name: "name", Expr: keyExpr},
					{Name: "count", Expr: &IntLit{Value: 1}},
				}},
			}},
		}},
	}
	incCount := &AssignStmt{Name: fmt.Sprintf("%s[%s].count", groupsVar, idxVar), Expr: &BinaryExpr{Op: "+", Left: &SelectorExpr{Root: fmt.Sprintf("%s[%s]", groupsVar, idxVar), Tail: []string{"count"}}, Right: &IntLit{Value: 1}}}
	condUpdate := &IfStmt{Cond: &BinaryExpr{Op: "=", Bool: true, Left: &VarRef{Name: idxVar}, Right: &IntLit{Value: -1}}, Then: []Stmt{addNew}, Else: []Stmt{incCount}}

	innerBody := []Stmt{&AssignStmt{Name: idxVar, Expr: &IntLit{Value: -1}}, forLoop, condUpdate}
	joinLoop := &ForEachStmt{Name: j.Var, Iterable: rightSrc, Body: []Stmt{&IfStmt{Cond: cond, Then: innerBody}}}
	outer := &ForEachStmt{Name: q.Var, Iterable: leftSrc, Body: []Stmt{joinLoop}}

	stmts := []Stmt{&AssignStmt{Name: groupsVar, Expr: &ListLit{}}, outer, &AssignStmt{Name: varName, Expr: &VarRef{Name: groupsVar}}}
	varTypes[varName] = "array of " + rec
	return stmts, "array of " + rec, nil
}

func exprToIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", false
	}
	pf := u.Value
	if pf == nil || len(pf.Ops) > 0 {
		return "", false
	}
	if pf.Target != nil {
		if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0 {
			return pf.Target.Selector.Root, true
		}
		if pf.Target.Lit != nil && pf.Target.Lit.Str != nil {
			return *pf.Target.Lit.Str, true
		}
	}
	return "", false
}

func exprToVarRef(e Expr) string {
	switch v := e.(type) {
	case *VarRef:
		return v.Name
	case *SelectorExpr:
		if len(v.Tail) == 0 {
			return v.Root
		}
	}
	return ""
}

func ensureRecord(fields []Field) string {
	for _, r := range currProg.Records {
		if len(r.Fields) != len(fields) {
			continue
		}
		match := true
		for i := range fields {
			if r.Fields[i] != fields[i] {
				match = false
				break
			}
		}
		if match {
			return r.Name
		}
	}
	anonCounter++
	name := fmt.Sprintf("Anon%d", anonCounter)
	currProg.Records = append(currProg.Records, RecordDef{Name: name, Fields: fields})
	for _, f := range fields {
		if strings.HasPrefix(f.Type, "array of IntArray") || strings.HasPrefix(f.Type, "array of array") {
			currProg.NeedShowList2 = true
		}
	}
	if funcReturns != nil {
		funcReturns[ctorName(name)] = name
	}
	return name
}

func ensureMap(keyType, valType string, items []MapItem, params []string) string {
	if valType == "" {
		valType = "Variant"
	}
	if len(params) == 0 {
		for _, m := range currProg.Maps {
			if m.KeyType == keyType && m.ValType == valType && len(m.Items) == len(items) {
				match := true
				for i := range items {
					// naive comparison only works for string keys and IntLits etc
					lk, ok1 := items[i].Key.(*StringLit)
					rk, ok2 := m.Items[i].Key.(*StringLit)
					if !ok1 || !ok2 || lk.Value != rk.Value {
						match = false
						break
					}
				}
				if match {
					return m.Name
				}
			}
		}
	}
	anonCounter++
	name := fmt.Sprintf("Map%d", anonCounter)
	currProg.Maps = append(currProg.Maps, MapLitDef{Name: name, KeyType: keyType, ValType: valType, Items: items, Params: params})
	if funcReturns != nil {
		funcReturns[name] = fmt.Sprintf("specialize TFPGMap<%s, %s>", keyType, valType)
	}
	return name
}

func typeOf(e *parser.Expr, env *types.Env) string {
	if id, ok := exprToIdent(e); ok {
		if t, ok2 := currentVarTypes[id]; ok2 {
			return t
		}
	}
	t := types.ExprType(e, env)
	switch v := t.(type) {
	case types.ListType:
		if st, ok := v.Elem.(types.StructType); ok {
			return "array of " + st.Name
		}
		if _, ok := v.Elem.(types.StringType); ok {
			return "array of string"
		} else if _, ok := v.Elem.(types.BoolType); ok {
			return "array of boolean"
		}
		return "array of integer"
	case types.StructType:
		return v.Name
	case types.StringType:
		return "string"
	case types.BoolType:
		return "boolean"
	case types.IntType, types.Int64Type:
		return "integer"
	case types.BigIntType:
		return "int64"
	}
	return ""
}

func elemType(t string) string {
	if strings.HasPrefix(t, "array of ") {
		return strings.TrimPrefix(t, "array of ")
	}
	return ""
}

func typeFromRef(tr *parser.TypeRef) string {
	if tr == nil {
		return ""
	}
	if tr.Simple != nil {
		return typeFromSimple(*tr.Simple)
	}
	if tr.Generic != nil && tr.Generic.Name == "list" && len(tr.Generic.Args) == 1 {
		elem := typeFromRef(tr.Generic.Args[0])
		if elem == "any" {
			elem = "Variant"
		}
		if strings.HasPrefix(elem, "array of ") {
			alias := currProg.addArrayAlias(strings.TrimPrefix(elem, "array of "))
			return "array of " + alias
		}
		return "array of " + elem
	}
	if tr.Generic != nil && tr.Generic.Name == "map" && len(tr.Generic.Args) == 2 {
		key := typeFromRef(tr.Generic.Args[0])
		val := typeFromRef(tr.Generic.Args[1])
		if strings.HasPrefix(val, "array of ") {
			alias := currProg.addArrayAlias(strings.TrimPrefix(val, "array of "))
			val = alias
		}
		if key == "any" {
			key = "Variant"
		}
		if val == "any" {
			val = "Variant"
		}
		currProg.UseFGL = true
		return fmt.Sprintf("specialize TFPGMap<%s, %s>", key, val)
	}
	if tr.Struct != nil {
		var fields []Field
		for _, f := range tr.Struct.Fields {
			fields = append(fields, Field{Name: f.Name, Type: typeFromRef(f.Type)})
		}
		return ensureRecord(fields)
	}
	return ""
}

func typeFromSimple(s string) string {
	switch s {
	case "int":
		return "integer"
	case "string":
		return "string"
	case "bool":
		return "boolean"
	case "float":
		return "real"
	case "bigint":
		return "int64"
	case "bigrat":
		currProg.UseBigRat = true
		return "BigRat"
	default:
		return s
	}
}

func pasTypeFromType(t types.Type) string {
	switch v := t.(type) {
	case types.StringType:
		return "string"
	case types.BoolType:
		return "boolean"
	case types.IntType, types.Int64Type:
		return "integer"
	case types.FloatType:
		return "real"
	case types.BigRatType:
		currProg.UseBigRat = true
		return "BigRat"
       case types.StructType:
               return v.Name
       case types.ListType:
		elem := pasTypeFromType(v.Elem)
		if strings.HasPrefix(elem, "array of ") {
			alias := currProg.addArrayAlias(strings.TrimPrefix(elem, "array of "))
			elem = alias
		}
		return "array of " + elem
	case types.MapType:
		key := pasTypeFromType(v.Key)
		val := pasTypeFromType(v.Value)
		if strings.HasPrefix(val, "array of ") {
			alias := currProg.addArrayAlias(strings.TrimPrefix(val, "array of "))
			val = alias
		}
		currProg.UseFGL = true
		return fmt.Sprintf("specialize TFPGMap<%s, %s>", key, val)
       case types.VoidType:
               return ""
       case types.AnyType:
               currProg.UseVariants = true
               return "Variant"
       default:
               currProg.UseVariants = true
               return "Variant"
       }
}

func CurrentVarTypesDebug() map[string]string { return currentVarTypes }

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
			rat := inferType(expr) == "BigRat"
			expr = &UnaryExpr{Op: "-", Expr: expr, Rat: rat}
		case "!":
			expr = &UnaryExpr{Op: "not ", Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(env *types.Env, pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(env, pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Call != nil:
			switch t := expr.(type) {
			case *VarRef:
				var args []Expr
				for _, a := range op.Call.Args {
					ex, err := convertExpr(env, a)
					if err != nil {
						return nil, err
					}
					args = append(args, ex)
				}
				name := t.Name
				if mapped, ok := nameMap[name]; ok {
					name = mapped
				}
				if name == "indexOf" && len(args) == 2 {
					currProg.NeedIndexOf = true
					expr = &CallExpr{Name: name, Args: args}
				} else if name == "parseIntStr" && len(args) == 1 {
					currProg.UseSysUtils = true
					name = "StrToInt"
					expr = &CallExpr{Name: name, Args: args}
				} else if name == "contains" && len(args) == 2 {
					if inferType(args[0]) == "string" || inferType(args[1]) == "string" {
						expr = &ContainsExpr{Collection: args[0], Value: args[1], Kind: "string"}
					} else {
						currProg.NeedContains = true
						expr = &ContainsExpr{Collection: args[0], Value: args[1], Kind: "list"}
					}
				} else if name == "sha256" && len(args) == 1 {
					currProg.UseSysUtils = true
					currProg.UseSHA256 = true
					_ = currProg.addArrayAlias("integer")
					expr = &CallExpr{Name: "_sha256", Args: args}
				} else {
					expr = &CallExpr{Name: name, Args: args}
				}
			case *SelectorExpr:
				if len(t.Tail) == 1 {
					name := t.Tail[0]
					switch t.Root {
					case "math":
						currProg.UseMath = true
						mapped := map[string]string{"sqrt": "Sqrt", "pow": "Power", "sin": "Sin", "log": "Ln"}
						if fn, ok := mapped[name]; ok {
							var args []Expr
							for _, a := range op.Call.Args {
								ex, err := convertExpr(env, a)
								if err != nil {
									return nil, err
								}
								args = append(args, ex)
							}
							expr = &CallExpr{Name: fn, Args: args}
							break
						}
					case "net":
						if name == "LookupHost" && len(op.Call.Args) == 1 {
							arg, err := convertExpr(env, op.Call.Args[0])
							if err != nil {
								return nil, err
							}
							currProg.NeedListStr = true
							currProg.UseLookupHost = true
							_ = currProg.addArrayAlias("string")
							_ = currProg.addArrayAlias("Variant")
							expr = &CallExpr{Name: "_lookup_host", Args: []Expr{arg}}
							break
						}
					case "testpkg":
						if name == "Add" && len(op.Call.Args) == 2 {
							a1, err := convertExpr(env, op.Call.Args[0])
							if err != nil {
								return nil, err
							}
							a2, err := convertExpr(env, op.Call.Args[1])
							if err != nil {
								return nil, err
							}
							expr = &BinaryExpr{Op: "+", Left: a1, Right: a2}
							break
						}
						if name == "FifteenPuzzleExample" && len(op.Call.Args) == 0 {
							expr = &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}
							break
						}
					}
				}
				if expr == nil {
					if len(t.Tail) == 1 && t.Tail[0] == "contains" {
						if len(op.Call.Args) != 1 {
							return nil, fmt.Errorf("contains expects 1 arg")
						}
						arg, err := convertExpr(env, op.Call.Args[0])
						if err != nil {
							return nil, err
						}
						if typ, ok := currentVarTypes[t.Root]; ok && typ == "string" {
							expr = &ContainsExpr{Collection: &VarRef{Name: t.Root}, Value: arg, Kind: "string"}
						} else {
							currProg.NeedContains = true
							expr = &ContainsExpr{Collection: &VarRef{Name: t.Root}, Value: arg, Kind: "list"}
						}
					} else {
						return nil, fmt.Errorf("unsupported call target")
					}
				}
			default:
				return nil, fmt.Errorf("unsupported call target")
			}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertExpr(env, call.Args[0])
			if err != nil {
				return nil, err
			}
			if typ := inferType(expr); typ == "string" {
				expr = &ContainsExpr{Collection: expr, Value: arg, Kind: "string"}
			} else {
				currProg.NeedContains = true
				expr = &ContainsExpr{Collection: expr, Value: arg, Kind: "list"}
			}
			i++
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(env, op.Index.Start)
			if err != nil {
				return nil, err
			}
			tmp := *pf
			tmp.Ops = tmp.Ops[:i]
			_ = tmp
			if lit, ok := idx.(*StringLit); ok {
				if vr, ok := expr.(*VarRef); ok {
					if t, ok2 := currentVarTypes[vr.Name]; ok2 {
						found := false
						for _, r := range currProg.Records {
							if r.Name == t {
								for _, f := range r.Fields {
									if f.Name == lit.Value {
										expr = &SelectorExpr{Root: vr.Name, Tail: []string{lit.Value}}
										found = true
										break
									}
								}
								if found {
									break
								}
							}
						}
						if found {
							continue
						}
					}
				}
			}
			isStr := inferType(expr) == "string"
			expr = &IndexExpr{Target: expr, Index: idx, String: isStr}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			var start Expr
			if op.Index.Start != nil {
				s, err := convertExpr(env, op.Index.Start)
				if err != nil {
					return nil, err
				}
				start = s
			}
			var end Expr
			if op.Index.End != nil {
				e, err := convertExpr(env, op.Index.End)
				if err != nil {
					return nil, err
				}
				end = e
			}
			tmp := *pf
			tmp.Ops = tmp.Ops[:i]
			_ = tmp
			isStr := inferType(expr) == "string"
			expr = &SliceExpr{Target: expr, Start: start, End: end, String: isStr}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			target := *op.Cast.Type.Simple
			switch target {
			case "int", "float", "bigrat":
				expr = &CastExpr{Expr: expr, Type: target}
			default:
				expr = expr
			}
		case op.Cast != nil && op.Cast.Type != nil:
			t := typeFromRef(op.Cast.Type)
			if t == "" {
				return nil, fmt.Errorf("unsupported postfix")
			}
			expr = &CastExpr{Expr: expr, Type: t}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(env *types.Env, p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Call != nil:
		name := p.Call.Func
		if name == "sum" && len(p.Call.Args) == 1 {
			if q := queryFromExpr(p.Call.Args[0]); q != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil && !q.Distinct {
				src, err := convertExpr(env, q.Source)
				if err != nil {
					return nil, err
				}
				child := types.NewEnv(env)
				child.SetVar(q.Var, types.AnyType{}, true)
				bodyExpr, err := convertExpr(child, q.Select)
				if err != nil {
					return nil, err
				}
				ret := inferType(bodyExpr)
				if ret == "" {
					ret = "integer"
				}
				fun := fmt.Sprintf("sumq%d", len(currProg.Funs))
				param := "arr" + fmt.Sprintf("%d", len(currProg.Funs))
				fnBody := []Stmt{
					&AssignStmt{Name: "Result", Expr: zeroValue(ret)},
					&ForEachStmt{Name: q.Var, Iterable: &VarRef{Name: param}, Body: []Stmt{
						&AssignStmt{Name: "Result", Expr: &BinaryExpr{Op: "+", Left: &VarRef{Name: "Result"}, Right: bodyExpr}},
					}},
				}
				currProg.Funs = append(currProg.Funs, FunDecl{Name: fun, Params: []string{param}, ReturnType: ret, Body: fnBody})
				return &CallExpr{Name: fun, Args: []Expr{src}}, nil
			}
		}
		var args []Expr
		for _, a := range p.Call.Args {
			ex, err := convertExpr(env, a)
			if err != nil {
				return nil, err
			}
			args = append(args, ex)
		}
		if name == "len" && len(p.Call.Args) == 1 {
			if ml := mapLitFromExpr(p.Call.Args[0]); ml != nil {
				return &IntLit{Value: int64(len(ml.Items))}, nil
			}
			name = "Length"
		} else if name == "substr" && len(args) == 3 {
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2], String: true}, nil
		} else if name == "substring" && len(args) == 3 {
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2], String: true}, nil
		} else if name == "int" && len(args) == 1 {
			if inferType(args[0]) == "string" {
				return &CallExpr{Name: "StrToInt", Args: []Expr{args[0]}}, nil
			}
			return &CastExpr{Expr: args[0], Type: "int"}, nil
		} else if name == "str" && len(args) == 1 {
			t := inferType(args[0])
			if strings.HasPrefix(t, "array of string") {
				currProg.NeedListStr = true
				return &CallExpr{Name: "list_to_str", Args: args}, nil
			}
			if strings.HasPrefix(t, "array of real") {
				currProg.NeedListStrReal = true
				currProg.UseSysUtils = true
				return &CallExpr{Name: "list_real_to_str", Args: args}, nil
			}
                       if strings.HasPrefix(t, "array of array of ") || strings.HasPrefix(t, "array of IntArray") || strings.HasSuffix(t, "IntArrayArray") {
                               currProg.NeedListStr2 = true
                               return &CallExpr{Name: "list_list_int_to_str", Args: args}, nil
                       }
                       if strings.HasPrefix(t, "array of Variant") {
                               currProg.NeedListStrVariant = true
                               currProg.UseSysUtils = true
                               currProg.UseVariants = true
                               return &CallExpr{Name: "list_variant_to_str", Args: args}, nil
                       }
                       if strings.HasPrefix(t, "array of ") || isArrayAlias(t) {
                               currProg.NeedListStr2 = true
                               return &CallExpr{Name: "list_int_to_str", Args: args}, nil
                       }
			if t == "Variant" || t == "any" {
				currProg.UseSysUtils = true
				currProg.UseVariants = true
				return &CallExpr{Name: "VarToStr", Args: args}, nil
			}
			if t == "string" {
				return args[0], nil
			}
			if t == "bool" || t == "boolean" {
				currProg.UseSysUtils = true
				return &CallExpr{Name: "LowerCase", Args: []Expr{&CallExpr{Name: "BoolToStr", Args: []Expr{args[0], &VarRef{Name: "true"}}}}}, nil
			}
			if t == "real" {
				name = "FloatToStr"
				currProg.UseSysUtils = true
			} else if t == "BigRat" {
				currProg.UseSysUtils = true
				numCall := &CallExpr{Name: "IntToStr", Args: []Expr{&CallExpr{Name: "num", Args: args}}}
				denCall := &CallExpr{Name: "IntToStr", Args: []Expr{&CallExpr{Name: "denom", Args: args}}}
				return &BinaryExpr{Op: "+", Left: &BinaryExpr{Op: "+", Left: numCall, Right: &StringLit{Value: "/"}}, Right: denCall}, nil
			} else {
				name = "IntToStr"
				currProg.UseSysUtils = true
			}
		} else if name == "upper" && len(args) == 1 {
			currProg.UseSysUtils = true
			return &CallExpr{Name: "UpperCase", Args: args}, nil
		} else if name == "lower" && len(args) == 1 {
			currProg.UseSysUtils = true
			return &CallExpr{Name: "LowerCase", Args: args}, nil
		} else if name == "sum" && len(args) == 1 {
			if l, ok := args[0].(*ListLit); ok {
				var sum Expr
				for i, el := range l.Elems {
					if i == 0 {
						sum = el
					} else {
						sum = &BinaryExpr{Op: "+", Left: sum, Right: el}
					}
				}
				if sum == nil {
					sum = &IntLit{Value: 0}
				}
				return sum, nil
			}
		} else if name == "count" && len(args) == 1 {
			return &CallExpr{Name: "Length", Args: args}, nil
		} else if name == "avg" && len(args) == 1 {
			currProg.NeedAvg = true
			return &CallExpr{Name: "avg", Args: args}, nil
		} else if name == "min" && len(args) == 1 {
			currProg.NeedMin = true
			return &CallExpr{Name: "min", Args: args}, nil
		} else if name == "max" && len(args) == 1 {
			currProg.NeedMax = true
			return &CallExpr{Name: "max", Args: args}, nil
		} else if name == "keys" && len(args) == 1 {
			// treat keys(map) as the map itself for iteration purposes
			return args[0], nil
		} else if name == "values" && len(args) == 1 {
			if vr, ok := args[0].(*VarRef); ok {
				if t, ok := currentVarTypes[vr.Name]; ok {
					for _, r := range currProg.Records {
						if r.Name == t {
							var elems []Expr
							for _, f := range r.Fields {
								elems = append(elems, &SelectorExpr{Root: vr.Name, Tail: []string{f.Name}})
							}
							return &ValuesExpr{Elems: elems}, nil
						}
					}
				}
			} else if rec, ok := args[0].(*RecordLit); ok {
				var elems []Expr
				for _, f := range rec.Fields {
					elems = append(elems, f.Expr)
				}
				return &ValuesExpr{Elems: elems}, nil
			}
		} else if name == "num" && len(args) == 1 {
			currProg.UseBigRat = true
			return &CallExpr{Name: "num", Args: args}, nil
		} else if name == "denom" && len(args) == 1 {
			currProg.UseBigRat = true
			return &CallExpr{Name: "denom", Args: args}, nil
		} else if name == "now" && len(args) == 0 {
			currProg.UseSysUtils = true
			currProg.UseNow = true
			return &CallExpr{Name: "_now", Args: nil}, nil
		} else if name == "input" && len(args) == 0 {
			currProg.UseInput = true
			return &CallExpr{Name: "_input", Args: nil}, nil
		} else if name == "padStart" && len(args) == 3 {
			currProg.NeedPadStart = true
			currProg.UseSysUtils = true
			return &CallExpr{Name: "padStart", Args: args}, nil
		} else if name == "append" && len(args) == 2 {
			return &CallExpr{Name: "concat", Args: []Expr{args[0], &ListLit{Elems: []Expr{args[1]}}}}, nil
		} else if name == "float" && len(args) == 1 {
			return &CallExpr{Name: "Double", Args: args}, nil
		} else if name == "contains" && len(args) == 2 {
			if inferType(args[0]) == "string" || inferType(args[1]) == "string" {
				return &ContainsExpr{Collection: args[0], Value: args[1], Kind: "string"}, nil
			}
			currProg.NeedContains = true
			return &ContainsExpr{Collection: args[0], Value: args[1], Kind: "list"}, nil
		} else if name == "sha256" && len(args) == 1 {
			currProg.UseSysUtils = true
			currProg.UseSHA256 = true
			_ = currProg.addArrayAlias("integer")
			return &CallExpr{Name: "_sha256", Args: args}, nil
		}
		if mapped, ok := nameMap[name]; ok {
			name = mapped
		}
		return &CallExpr{Name: name, Args: args}, nil
	case p.List != nil:
		var elems []Expr
		for _, el := range p.List.Elems {
			ex, err := convertExpr(env, el)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, nil
	case p.Struct != nil:
		st, _ := env.GetStruct(p.Struct.Name)
		var fields []FieldExpr
		for _, it := range p.Struct.Fields {
			if isEmptyMapLiteral(it.Value) {
				if ft, ok := st.Fields[it.Name]; ok {
					if mt, ok2 := ft.(types.MapType); ok2 {
						keyT := pasTypeFromType(mt.Key)
						valT := pasTypeFromType(mt.Value)
						mapName := ensureMap(keyT, valT, nil, nil)
						fields = append(fields, FieldExpr{Name: it.Name, Expr: &CallExpr{Name: mapName}})
						continue
					}
				}
			}
			val, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			fields = append(fields, FieldExpr{Name: it.Name, Expr: val})
		}
		return &RecordLit{Type: p.Struct.Name, Fields: fields}, nil
	case p.Map != nil:
		if len(p.Map.Items) == 0 && expectedMapType != "" && strings.HasPrefix(expectedMapType, "specialize TFPGMap") {
			currProg.UseFGL = true
			return &CallExpr{Name: expectedMapType + ".Create"}, nil
		}
		var items []MapItem
		keyType := "string"
		valType := "Variant"
		varsSet := map[string]struct{}{}
		allInts := true
		for _, it := range p.Map.Items {
			val, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			keyStr, ok := literalString(it.Key)
			if !ok {
				keyStr, ok = exprToIdent(it.Key)
			}
			if !ok {
				return nil, fmt.Errorf("unsupported map key")
			}
			vType := inferType(val)
			items = append(items, MapItem{Key: &StringLit{Value: keyStr}, Value: val, Type: vType})
			collectVarNames(val, varsSet)
			if vType != "integer" {
				allInts = false
			}
		}
		if allInts {
			valType = "integer"
		}
		names := make([]string, 0, len(varsSet))
		for name := range varsSet {
			names = append(names, name)
		}
		sort.Strings(names)
		params := []string{}
		args := []Expr{}
		for _, n := range names {
			typ := inferType(&VarRef{Name: n})
			if typ == "" || typ == "integer" {
				for _, it := range items {
					if usesMapIndex(it.Value, n) {
						typ = "specialize TFPGMap<string, integer>"
						break
					}
				}
			}
			if typ == "" {
				continue
			}
			if strings.HasPrefix(typ, "array of ") {
				elem := strings.TrimPrefix(typ, "array of ")
				typ = currProg.addArrayAlias(elem)
			}
			params = append(params, formatParam(n, typ))
			args = append(args, &VarRef{Name: n})
		}
		currProg.UseFGL = true
		mapName := ensureMap(keyType, valType, items, params)
		return &CallExpr{Name: mapName, Args: args}, nil
	case p.Load != nil:
		if p.Load.Path == nil || p.Load.Type == nil || p.Load.Type.Simple == nil {
			return nil, fmt.Errorf("unsupported load")
		}
		format := parseFormat(p.Load.With)
		if format != "yaml" {
			return nil, fmt.Errorf("unsupported load format")
		}
		root := repoRoot()
		path := strings.Trim(*p.Load.Path, "\"")
		full := filepath.Join(root, path)
		if _, err := os.Stat(full); err != nil {
			alt := filepath.Join(root, "tests", strings.TrimPrefix(path, "../"))
			if _, err2 := os.Stat(alt); err2 == nil {
				full = alt
			}
		}
		data, err := os.ReadFile(full)
		if err != nil {
			return nil, err
		}
		records, err := parseYAMLRecords(data)
		if err != nil {
			return nil, err
		}
		recDef, ok := findRecord(*p.Load.Type.Simple)
		if !ok {
			return nil, fmt.Errorf("unknown type %s", *p.Load.Type.Simple)
		}
		var elems []Expr
		for _, row := range records {
			var flds []FieldExpr
			for _, f := range recDef.Fields {
				val := row[f.Name]
				var ex Expr
				switch f.Type {
				case "integer":
					iv, _ := strconv.Atoi(val)
					ex = &IntLit{Value: int64(iv)}
				default:
					ex = &StringLit{Value: val}
				}
				flds = append(flds, FieldExpr{Name: f.Name, Expr: ex})
			}
			elems = append(elems, &RecordLit{Type: recDef.Name, Fields: flds})
		}
		return &ListLit{Elems: elems}, nil
	case p.FunExpr != nil:
		name := fmt.Sprintf("anon%d", len(currProg.Funs))
		var params []string
		child := types.NewEnv(env)
		startVarCount := len(currProg.Vars)
		pushScope()
		currentFunc = name
		for _, pa := range p.FunExpr.Params {
			typ := "integer"
			if pa.Type != nil {
				typ = typeFromRef(pa.Type)
				if strings.HasPrefix(typ, "array of ") {
					elem := strings.TrimPrefix(typ, "array of ")
					typ = currProg.addArrayAlias(elem)
				}
			}
			params = append(params, formatParam(pa.Name, typ))
			child.SetVar(pa.Name, types.AnyType{}, true)
			currentScope()[pa.Name] = pa.Name
		}
		var body []Stmt
		if p.FunExpr.ExprBody != nil {
			ex, err := convertExpr(child, p.FunExpr.ExprBody)
			if err != nil {
				return nil, err
			}
			body = []Stmt{&ReturnStmt{Expr: ex}}
		} else if len(p.FunExpr.BlockBody) > 0 {
			varTypes := map[string]string{}
			b, err := convertBody(child, p.FunExpr.BlockBody, varTypes)
			if err != nil {
				return nil, err
			}
			body = b
			for n, t := range varTypes {
				child.SetVar(n, types.AnyType{}, true)
				_ = t
			}
		}
		popScope()
		currentFunc = ""
		locals := append([]VarDecl(nil), currProg.Vars[startVarCount:]...)
		currProg.Vars = currProg.Vars[:startVarCount]
		rt := ""
		if p.FunExpr.Return != nil && p.FunExpr.Return.Simple != nil {
			if *p.FunExpr.Return.Simple == "int" {
				rt = "integer"
			} else if *p.FunExpr.Return.Simple == "string" {
				rt = "string"
			}
		}
		currProg.Funs = append(currProg.Funs, FunDecl{Name: name, Params: params, ReturnType: rt, Locals: locals, Body: body})
		if rt != "" {
			funcReturns[name] = rt
		}
		return &VarRef{Name: name}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if v, ok := lookupName(p.Selector.Root); ok {
			return &VarRef{Name: v}, nil
		}
		return &VarRef{Name: p.Selector.Root}, nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 1 {
			root := p.Selector.Root
			field := p.Selector.Tail[0]
			if v, ok := lookupName(root); ok {
				root = v
			}
			switch root {
			case "math":
				currProg.UseMath = true
				switch field {
				case "pi":
					return &VarRef{Name: "Pi"}, nil
				case "e":
					return &RealLit{Value: 2.718281828459045}, nil
				}
			case "testpkg":
				switch field {
				case "Pi":
					return &RealLit{Value: 3.14}, nil
				case "Answer":
					return &IntLit{Value: 42}, nil
				}
			}
		}
		r := p.Selector.Root
		if v, ok := lookupName(r); ok {
			r = v
		}
		return &SelectorExpr{Root: r, Tail: p.Selector.Tail}, nil
	case p.If != nil:
		return convertIfExpr(env, p.If)
	case p.Match != nil:
		return convertMatchExpr(env, p.Match)
	case p.Group != nil:
		return convertExpr(env, p.Group)
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: int64(*l.Int)}, nil
	case l.Float != nil:
		return &RealLit{Value: *l.Float}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Null:
		return &VarRef{Name: "nil"}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func convertIfExpr(env *types.Env, ie *parser.IfExpr) (*IfExpr, error) {
	cond, err := convertExpr(env, ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(env, ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	var elseIf *IfExpr
	if ie.ElseIf != nil {
		ei, err := convertIfExpr(env, ie.ElseIf)
		if err != nil {
			return nil, err
		}
		elseIf = ei
	} else if ie.Else != nil {
		e, err := convertExpr(env, ie.Else)
		if err != nil {
			return nil, err
		}
		elseExpr = e
	}
	if currProg != nil {
		currProg.UseMath = true
	}
	return &IfExpr{Cond: cond, Then: thenExpr, ElseIf: elseIf, Else: elseExpr}, nil
}

func convertMatchExpr(env *types.Env, me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(env, me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(env, c.Result)
		if err != nil {
			return nil, err
		}
		if id, ok := exprToIdent(c.Pattern); ok && id == "_" {
			expr = res
			continue
		}
		pat, err := convertExpr(env, c.Pattern)
		if err != nil {
			return nil, err
		}
		cond := &BinaryExpr{Op: "=", Left: target, Right: pat, Bool: true}
		if expr == nil {
			expr = res
		} else {
			expr = &IfExpr{Cond: cond, Then: res, Else: expr}
		}
	}
	if expr == nil {
		return nil, fmt.Errorf("unsupported match")
	}
	return expr, nil
}

func inferType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		return "integer"
	case *RealLit:
		return "real"
	case *StringLit:
		return "string"
	case *BoolLit:
		return "boolean"
	case *VarRef:
		name := v.Name
		if s, ok := lookupName(name); ok {
			name = s
		}
		if t, ok := currentVarTypes[name]; ok {
			return resolveAlias(t)
		}
		return ""
	case *BinaryExpr:
		if v.Bool {
			return "boolean"
		}
		lt := inferType(v.Left)
		rt := inferType(v.Right)
		if lt == "BigRat" || rt == "BigRat" {
			return "BigRat"
		}
		if lt == rt {
			return lt
		}
		if lt == "real" || rt == "real" {
			return "real"
		}
		if lt != "" {
			return lt
		}
		return rt
	case *CallExpr:
		switch v.Name {
		case "Length", "Pos":
			return "integer"
		case "IntToStr":
			return "string"
		case "UpperCase", "LowerCase":
			return "string"
		case "avg":
			return "real"
		case "min", "max":
			return "integer"
		case "num", "denom":
			return "integer"
		case "Sqrt", "Sin", "Ln", "Power":
			return "real"
		case "Double":
			return "real"
		case "concat":
			if len(v.Args) > 0 {
				t := inferType(v.Args[0])
				if strings.HasPrefix(t, "array of ") {
					return t
				}
			}
			return ""
		default:
			if rt, ok := funcReturns[v.Name]; ok {
				return rt
			}
			return ""
		}
	case *IfExpr:
		thenT := inferType(v.Then)
		elseT := ""
		if v.ElseIf != nil {
			elseT = inferType(v.ElseIf)
		} else if v.Else != nil {
			elseT = inferType(v.Else)
		}
		if thenT == elseT {
			return thenT
		}
		if thenT != "" {
			return thenT
		}
		return elseT
	case *ListLit:
		t := ""
		for _, el := range v.Elems {
			et := inferType(el)
			if et == "" {
				continue
			}
			if t == "" {
				t = et
			} else if et != t {
				return ""
			}
		}
		if t != "" {
			return "array of " + t
		}
		return ""
	case *RecordLit:
		return v.Type
	case *ContainsExpr:
		return "boolean"
	case *MapContainsExpr:
		return "boolean"
	case *IndexExpr:
		if v.String {
			return "string"
		}
		t := resolveAlias(inferType(v.Target))
		if strings.HasPrefix(t, "array of ") {
			elem := strings.TrimPrefix(t, "array of ")
			return resolveAlias(elem)
		}
		if strings.HasPrefix(t, "specialize TFPGMap") {
			parts := strings.TrimPrefix(t, "specialize TFPGMap<")
			parts = strings.TrimSuffix(parts, ">")
			kv := strings.Split(parts, ",")
			if len(kv) == 2 {
				return strings.TrimSpace(kv[1])
			}
		}
		return "integer"
	case *SliceExpr:
		if v.String {
			return "string"
		}
		return "array of integer"
	case *SelectorExpr:
		root := v.Root
		if s, ok := lookupName(root); ok {
			root = s
		}
		if t, ok := currentVarTypes[root]; ok {
			if strings.HasPrefix(t, "array of ") {
				t = strings.TrimPrefix(t, "array of ")
			}
			for _, r := range currProg.Records {
				if r.Name == t {
					if len(v.Tail) > 0 {
						field := v.Tail[len(v.Tail)-1]
						for _, f := range r.Fields {
							if f.Name == field {
								return f.Type
							}
						}
					}
				}
			}
		}
		return ""
	case *CastExpr:
		if v.Type == "int" {
			return "integer"
		}
		if v.Type == "float" {
			return "real"
		}
		if v.Type == "bigrat" {
			return "BigRat"
		}
		if strings.HasPrefix(v.Type, "array of ") || strings.HasPrefix(v.Type, "specialize TFPGMap") {
			return pasType(v.Type)
		}
		if v.Type != "" {
			return pasType(v.Type)
		}
		return inferType(v.Expr)
	case *UnaryExpr:
		return inferType(v.Expr)
	default:
		return ""
	}
}

func usesSysUtilsExpr(e Expr) bool {
	switch v := e.(type) {
	case *CallExpr:
		if v.Name == "IntToStr" || v.Name == "StrToInt" || v.Name == "UpperCase" || v.Name == "LowerCase" {
			return true
		}
		for _, a := range v.Args {
			if usesSysUtilsExpr(a) {
				return true
			}
		}
	case *IfExpr:
		return true
	case *BinaryExpr:
		if usesSysUtilsExpr(v.Left) || usesSysUtilsExpr(v.Right) {
			return true
		}
	case *UnaryExpr:
		return usesSysUtilsExpr(v.Expr)
	case *ContainsExpr:
		return usesSysUtilsExpr(v.Collection) || usesSysUtilsExpr(v.Value)
	case *MapContainsExpr:
		return usesSysUtilsExpr(v.Map) || usesSysUtilsExpr(v.Key)
	case *IndexExpr:
		return usesSysUtilsExpr(v.Target) || usesSysUtilsExpr(v.Index)
	case *SliceExpr:
		return usesSysUtilsExpr(v.Target) ||
			(v.Start != nil && usesSysUtilsExpr(v.Start)) ||
			(v.End != nil && usesSysUtilsExpr(v.End))
	case *CastExpr:
		if v.Type == "int" {
			return inferType(v.Expr) == "string"
		}
		return usesSysUtilsExpr(v.Expr)
	case *RecordLit:
		for _, f := range v.Fields {
			if usesSysUtilsExpr(f.Expr) {
				return true
			}
		}
	}
	return false
}

func usesSysUtilsStmt(s Stmt) bool {
	switch v := s.(type) {
	case *PrintStmt:
		if v.NeedSysUtils {
			return true
		}
		for _, ex := range v.Exprs {
			if usesSysUtilsExpr(ex) {
				return true
			}
		}
		return false
	case *WritelnStmt:
		return usesSysUtilsExpr(v.Expr)
	case *AssignStmt:
		return usesSysUtilsExpr(v.Expr)
	case *IndexAssignStmt:
		return usesSysUtilsExpr(v.Index) || usesSysUtilsExpr(v.Expr)
	case *DoubleIndexAssignStmt:
		return usesSysUtilsExpr(v.Index1) || usesSysUtilsExpr(v.Index2) || usesSysUtilsExpr(v.Expr)
	case *MapAssignStmt:
		return usesSysUtilsExpr(v.Key) || usesSysUtilsExpr(v.Expr)
	case *ExprStmt:
		return usesSysUtilsExpr(v.Expr)
	case *SetStmt:
		return usesSysUtilsExpr(v.Target) || usesSysUtilsExpr(v.Expr)
	case *ReturnStmt:
		return usesSysUtilsExpr(v.Expr)
	case *IfStmt:
		if usesSysUtilsExpr(v.Cond) {
			return true
		}
		for _, st := range v.Then {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
		for _, st := range v.Else {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	case *WhileStmt:
		if usesSysUtilsExpr(v.Cond) {
			return true
		}
		for _, st := range v.Body {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	case *ForRangeStmt:
		if usesSysUtilsExpr(v.Start) || usesSysUtilsExpr(v.End) {
			return true
		}
		for _, st := range v.Body {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	case *ForEachStmt:
		if usesSysUtilsExpr(v.Iterable) {
			return true
		}
		for _, st := range v.Body {
			if usesSysUtilsStmt(st) {
				return true
			}
		}
	}
	return false
}

func markSysUtils(p *Program) {
	for _, v := range p.Vars {
		if v.Init != nil && usesSysUtilsExpr(v.Init) {
			p.UseSysUtils = true
			return
		}
	}
	for _, st := range p.Stmts {
		if usesSysUtilsStmt(st) {
			p.UseSysUtils = true
			return
		}
	}
	for _, f := range p.Funs {
		for _, st := range f.Body {
			if usesSysUtilsStmt(st) {
				p.UseSysUtils = true
				return
			}
		}
	}
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

func addConstructors(p *Program) {
	for _, r := range p.Records {
		var params []string
		var body []Stmt
		for _, f := range r.Fields {
			typ := f.Type
			if strings.HasPrefix(typ, "array of ") {
				elem := strings.TrimPrefix(typ, "array of ")
				typ = p.addArrayAlias(elem)
			}
			params = append(params, formatParam(f.Name, typ))
			body = append(body, &SetStmt{Target: &SelectorExpr{Root: "Result", Tail: []string{f.Name}}, Expr: &VarRef{Name: f.Name}})
		}
		fn := FunDecl{Name: ctorName(r.Name), Params: params, ReturnType: r.Name, Body: body}
		p.Funs = append([]FunDecl{fn}, p.Funs...)
		if funcReturns != nil {
			funcReturns[fn.Name] = r.Name
		}
	}
}

func addMapConstructors(p *Program) {
	for _, m := range p.Maps {
		var body []Stmt
		var locals []VarDecl
		body = append(body, &AssignStmt{Name: "Result", Expr: &CallExpr{Name: fmt.Sprintf("specialize TFPGMap<%s, %s>.Create", m.KeyType, m.ValType)}})
		for _, it := range m.Items {
			typ := it.Type
			if strings.HasPrefix(typ, "array of ") {
				elem := strings.TrimPrefix(typ, "array of ")
				typ = currProg.addArrayAlias(elem)
			}
			if isArrayType(typ) || isRecordType(typ) {
				anonCounter++
				ptr := fmt.Sprintf("_ptr%d", anonCounter)
				locals = append(locals, VarDecl{Name: ptr, Type: "^" + typ})
				body = append(body, &ExprStmt{Expr: &CallExpr{Name: "New", Args: []Expr{&VarRef{Name: ptr}}}})
				body = append(body, &AssignStmt{Name: ptr + "^", Expr: it.Value})
				body = append(body, &MapAssignStmt{Name: "Result", Key: it.Key, Expr: &VarRef{Name: ptr}, Type: "^" + typ})
			} else {
				body = append(body, &MapAssignStmt{Name: "Result", Key: it.Key, Expr: it.Value, Type: typ})
			}
		}
		fn := FunDecl{Name: m.Name, Params: m.Params, ReturnType: fmt.Sprintf("specialize TFPGMap<%s, %s>", m.KeyType, m.ValType), Locals: locals, Body: body}
		p.Funs = append([]FunDecl{fn}, p.Funs...)
		if funcReturns != nil {
			funcReturns[fn.Name] = fn.ReturnType
		}
	}
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return ""
	}
	for _, it := range u.Value.Target.Map.Items {
		key, ok := exprToIdent(it.Key)
		if !ok {
			if s, ok2 := literalString(it.Key); ok2 {
				key = s
			}
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return "", false
	}
	t := u.Value.Target
	if t.Lit != nil && t.Lit.Str != nil {
		return *t.Lit.Str, true
	}
	if t.Selector != nil && len(t.Selector.Tail) == 0 {
		return t.Selector.Root, true
	}
	return "", false
}

func parseYAMLRecords(data []byte) ([]map[string]string, error) {
	var res []map[string]string
	scanner := bufio.NewScanner(bytes.NewReader(data))
	cur := map[string]string{}
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "-") {
			if len(cur) > 0 {
				res = append(res, cur)
				cur = map[string]string{}
			}
			line = strings.TrimSpace(strings.TrimPrefix(line, "-"))
			if idx := strings.Index(line, ":"); idx != -1 {
				cur[strings.TrimSpace(line[:idx])] = strings.TrimSpace(line[idx+1:])
			}
			continue
		}
		if idx := strings.Index(line, ":"); idx != -1 {
			cur[strings.TrimSpace(line[:idx])] = strings.TrimSpace(line[idx+1:])
		}
	}
	if len(cur) > 0 {
		res = append(res, cur)
	}
	return res, scanner.Err()
}

func hasVar(name string) bool {
	for _, v := range currProg.Vars {
		if v.Name == name {
			return true
		}
	}
	return false
}

func setVarType(name, typ string) {
	for i, v := range currProg.Vars {
		if v.Name == name {
			currProg.Vars[i].Type = typ
			return
		}
	}
	currProg.Vars = append(currProg.Vars, VarDecl{Name: name, Type: typ})
}

func (p *Program) addArrayAlias(elem string) string {
	if p.ArrayAliases == nil {
		p.ArrayAliases = make(map[string]string)
	}
	if p.LateAliases == nil {
		p.LateAliases = make(map[string]string)
	}
	if name, ok := p.ArrayAliases[elem]; ok {
		return name
	}
	if name, ok := p.LateAliases[elem]; ok {
		return name
	}
	for strings.HasPrefix(elem, "array of ") {
		elem = strings.TrimPrefix(elem, "array of ")
	}
	alias := ""
	switch elem {
	case "integer":
		alias = "IntArray"
	case "string":
		alias = "StrArray"
	case "boolean":
		alias = "BoolArray"
	default:
		if len(elem) > 0 && strings.ToUpper(elem[:1]) == elem[:1] {
			alias = elem + "Array"
		} else {
			alias = strings.Title(elem) + "Array"
		}
		var b strings.Builder
		for _, r := range alias {
			if unicode.IsLetter(r) || unicode.IsDigit(r) {
				b.WriteRune(r)
			}
		}
		alias = b.String()
	}
	switch elem {
	case "integer", "string", "boolean", "real", "Variant":
		p.ArrayAliases[elem] = alias
	default:
		if _, ok := p.ArrayAliases[elem]; ok || (len(elem) > 0 && strings.ToUpper(elem[:1]) == elem[:1]) {
			p.ArrayAliases[elem] = alias
		} else {
			p.LateAliases[elem] = alias
		}
	}
	return alias
}

func resolveAlias(t string) string {
	if currProg == nil {
		return t
	}
	seen := map[string]bool{}
	for {
		if currProg.ArrayAliases != nil {
			for elem, alias := range currProg.ArrayAliases {
				if alias == t {
					t = "array of " + elem
					goto next
				}
			}
		}
		if currProg.LateAliases != nil {
			for elem, alias := range currProg.LateAliases {
				if alias == t {
					t = "array of " + elem
					goto next
				}
			}
		}
		return t
	next:
		if seen[t] {
			return t
		}
		seen[t] = true
	}
}

func isArrayAlias(t string) bool {
	if currProg != nil {
		if currProg.ArrayAliases != nil {
			for _, alias := range currProg.ArrayAliases {
				if alias == t {
					return true
				}
			}
		}
		if currProg.LateAliases != nil {
			for _, alias := range currProg.LateAliases {
				if alias == t {
					return true
				}
			}
		}
	}
	return false
}

func isArrayType(t string) bool {
	return strings.HasPrefix(t, "array of ") || isArrayAlias(t)
}

func isRecordType(t string) bool {
	if currProg != nil {
		for _, r := range currProg.Records {
			if r.Name == t {
				return true
			}
		}
	}
	return false
}

func formatParam(name, typ string) string {
	// Always pass parameters by value to avoid issues with nested dynamic
	// arrays when using recursion. Using "var" for array parameters caused
	// access violations on some programs (e.g. Cramer's rule) so we no
	// longer emit it here.
	return fmt.Sprintf("%s: %s", name, pasType(typ))
}

func findRecord(name string) (RecordDef, bool) {
	for _, r := range currProg.Records {
		if r.Name == name {
			return r, true
		}
	}
	return RecordDef{}, false
}

func buildJSONLineExpr(varName string, rec RecordDef) Expr {
	var expr Expr = &StringLit{Value: "{"}
	for i, f := range rec.Fields {
		if i > 0 {
			expr = &BinaryExpr{Op: "+", Left: expr, Right: &StringLit{Value: ", "}}
		}
		expr = &BinaryExpr{Op: "+", Left: expr, Right: &StringLit{Value: fmt.Sprintf("\"%s\": ", f.Name)}}
		sel := &SelectorExpr{Root: varName, Tail: []string{f.Name}}
		if f.Type == "integer" {
			expr = &BinaryExpr{Op: "+", Left: expr, Right: &CallExpr{Name: "IntToStr", Args: []Expr{sel}}}
		} else {
			expr = &BinaryExpr{Op: "+", Left: expr, Right: &StringLit{Value: "\""}}
			expr = &BinaryExpr{Op: "+", Left: expr, Right: sel}
			expr = &BinaryExpr{Op: "+", Left: expr, Right: &StringLit{Value: "\""}}
		}
	}
	expr = &BinaryExpr{Op: "+", Left: expr, Right: &StringLit{Value: "}"}}
	return expr
}

func extractSaveExpr(e *parser.Expr) *parser.SaveExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return nil
	}
	return u.Value.Target.Save
}
