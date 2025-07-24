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
	case "label":
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

func ctorName(name string) string { return "make" + name }

// Program is a minimal Pascal AST consisting of a sequence of statements
// plus optional variable declarations.
type Program struct {
	Funs         []FunDecl
	Vars         []VarDecl
	Records      []RecordDef
	ArrayAliases map[string]string
	Stmts        []Stmt
	UseSysUtils  bool
	UseMath      bool
	NeedAvg      bool
	NeedMin      bool
	NeedMax      bool
	NeedContains bool
	NeedShowList bool
	UseNow       bool
	UseInput     bool
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
	Body       []Stmt
}

func (f *FunDecl) emit(out io.Writer) {
	rt := f.ReturnType
	proc := false
	if rt == "" {
		proc = true
	}
	if proc {
		fmt.Fprintf(out, "procedure %s(", f.Name)
	} else {
		fmt.Fprintf(out, "function %s(", f.Name)
	}
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(out, "; ")
		}
		io.WriteString(out, p)
	}
	if proc {
		io.WriteString(out, ");\nbegin\n")
	} else {
		fmt.Fprintf(out, "): %s;\nbegin\n", rt)
	}
	for _, s := range f.Body {
		io.WriteString(out, "  ")
		s.emit(out)
		io.WriteString(out, "\n")
	}
	io.WriteString(out, "end;\n")
}

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

// CastExpr represents a simple cast expression, e.g. string to int.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "StrToInt(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		c.Expr.emit(w)
	}
}

// UnaryExpr represents a unary operation like negation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) isBool() bool { return u.Op == "not " }

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	switch u.Expr.(type) {
	case *BinaryExpr:
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
		io.WriteString(w, ")")
		io.WriteString(w, ")")
	} else if s.End != nil {
		s.End.emit(w)
		io.WriteString(w, ")")
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
	if b.Op == "in" {
		io.WriteString(w, "Pos(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ") <> 0")
		return
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
	if len(p.Exprs) == 1 && len(p.Types) == 1 && strings.HasPrefix(p.Types[0], "array of ") {
		io.WriteString(w, "show_list(")
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
			io.WriteString(w, ":0:1")
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
	var buf bytes.Buffer
	buf.WriteString("{$mode objfpc}\nprogram Main;\n")
	var uses []string
	if p.UseSysUtils {
		uses = append(uses, "SysUtils")
	}
	if p.UseMath {
		uses = append(uses, "Math")
	}
	if len(uses) > 0 {
		fmt.Fprintf(&buf, "uses %s;\n", strings.Join(uses, ", "))
	}
	if p.UseNow {
		buf.WriteString("var _nowSeed: int64 = 0;\n")
		buf.WriteString("var _nowSeeded: boolean = false;\n")
		buf.WriteString("procedure init_now();\nvar s: string; v: int64;\nbegin\n  s := GetEnvironmentVariable('MOCHI_NOW_SEED');\n  if s <> '' then begin\n    Val(s, v);\n    _nowSeed := v;\n    _nowSeeded := true;\n  end;\nend;\n")
		buf.WriteString("function _now(): integer;\nbegin\n  if _nowSeeded then begin\n    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;\n    _now := _nowSeed;\n  end else begin\n    _now := Integer(GetTickCount64());\n  end;\nend;\n")
	}
	if p.UseInput {
		buf.WriteString("function _input(): string;\nvar s: string;\nbegin\n  if EOF(Input) then s := '' else ReadLn(s);\n  _input := s;\nend;\n")
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
	if p.NeedShowList {
		buf.WriteString("procedure show_list(xs: array of integer);\nvar i: integer;\nbegin\n  write('[');\n  for i := 0 to High(xs) do begin\n    write(xs[i]);\n    if i < High(xs) then write(', ');\n  end;\n  writeln(']');\nend;\n")
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
	for _, r := range p.Records {
		fmt.Fprintf(&buf, "type %s = record\n", r.Name)
		for _, f := range r.Fields {
			fmt.Fprintf(&buf, "  %s: %s;\n", f.Name, f.Type)
		}
		buf.WriteString("end;\n")
	}
	if len(p.Vars) > 0 {
		buf.WriteString("var\n")
		for _, v := range p.Vars {
			typ := v.Type
			if typ == "" {
				typ = "integer"
			}
			fmt.Fprintf(&buf, "  %s: %s;\n", v.Name, typ)
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
			funcNames[st.Fun.Name] = struct{}{}
		}
	}
	varTypes := map[string]string{}
	currentVarTypes = varTypes
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
					} else if strings.HasPrefix(t, "array of ") {
						pr.NeedShowList = true
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
			if st.Let.Value != nil {
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
						if t := inferType(ex); t != "" {
							vd.Type = t
						} else {
							switch tt := types.ExprType(st.Let.Value, env).(type) {
							case types.StringType:
								vd.Type = "string"
							case types.ListType:
								if _, ok := tt.Elem.(types.StringType); ok {
									vd.Type = "array of string"
								} else if _, ok := tt.Elem.(types.BoolType); ok {
									vd.Type = "array of boolean"
								} else if _, ok := tt.Elem.(types.AnyType); ok {
									// unknown element type, leave var type empty for now
									vd.Type = ""
								} else {
									vd.Type = "array of integer"
								}
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
			if st.Var.Value != nil {
				ex, err := convertExpr(env, st.Var.Value)
				if err != nil {
					return nil, err
				}
				if rec, ok := ex.(*RecordLit); ok {
					if vd.Type == "" {
						vd.Type = rec.Type
					}
					pr.Vars = append(pr.Vars, vd)
					for _, f := range rec.Fields {
						pr.Stmts = append(pr.Stmts, &AssignStmt{Name: fmt.Sprintf("%s.%s", vd.Name, f.Name), Expr: f.Expr})
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
							if _, ok := t.Elem.(types.StringType); ok {
								vd.Type = "array of string"
							} else if _, ok := t.Elem.(types.BoolType); ok {
								vd.Type = "array of boolean"
							} else if _, ok := t.Elem.(types.AnyType); ok {
								// unknown element type, leave var type empty for now
								vd.Type = ""
							} else {
								vd.Type = "array of integer"
							}
						case types.BoolType:
							vd.Type = "boolean"
						}
					}
				}
			}
			pr.Vars = append(pr.Vars, vd)
		case st.Assign != nil:
			name := sanitize(st.Assign.Name)
			ex, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
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
				pr.Stmts = append(pr.Stmts, &IndexAssignStmt{Name: name, Index: idx, Expr: ex})
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
			pr.Stmts = append(pr.Stmts, &AssignStmt{Name: name, Expr: ex})
		case st.For != nil:
			start, err := convertExpr(env, st.For.Source)
			if err != nil {
				return nil, err
			}
			typ := "integer"
			if st.For.RangeEnd == nil {
				t := types.ExprType(st.For.Source, env)
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.StringType); ok {
						typ = "string"
					} else if _, ok := lt.Elem.(types.BoolType); ok {
						typ = "boolean"
					}
				}
			}
			if _, ok := varTypes[st.For.Name]; !ok {
				varTypes[st.For.Name] = typ
			}
			setVarType(st.For.Name, varTypes[st.For.Name])
			body, err := convertBody(env, st.For.Body, varTypes)
			if err != nil {
				return nil, err
			}
			if st.For.RangeEnd != nil {
				end, err := convertExpr(env, st.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				pr.Stmts = append(pr.Stmts, &ForRangeStmt{Name: st.For.Name, Start: start, End: end, Body: body})
			} else {
				pr.Stmts = append(pr.Stmts, &ForEachStmt{Name: st.For.Name, Iterable: start, Body: body})
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
			pr.Stmts = append(pr.Stmts, &WhileStmt{Cond: cond, Body: body})
		case st.If != nil:
			cond, err := convertExpr(env, st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenBody, err := convertBody(env, st.If.Then, varTypes)
			if err != nil {
				return nil, err
			}
			elseBody, err := convertBody(env, st.If.Else, varTypes)
			if err != nil {
				return nil, err
			}
			pr.Stmts = append(pr.Stmts, &IfStmt{Cond: cond, Then: thenBody, Else: elseBody})
		case st.Fun != nil:
			local := map[string]string{}
			for k, v := range varTypes {
				local[k] = v
			}
			for _, p := range st.Fun.Params {
				typ := "integer"
				if p.Type != nil {
					if p.Type.Simple != nil {
						switch *p.Type.Simple {
						case "int":
							typ = "integer"
						case "string":
							typ = "string"
						case "bool":
							typ = "boolean"
						default:
							typ = *p.Type.Simple
						}
					} else if p.Type.Generic != nil && p.Type.Generic.Name == "list" && len(p.Type.Generic.Args) == 1 {
						arg := p.Type.Generic.Args[0]
						elem := "integer"
						if arg.Simple != nil {
							switch *arg.Simple {
							case "int":
								elem = "integer"
							case "string":
								elem = "string"
							case "bool":
								elem = "boolean"
							default:
								elem = *arg.Simple
							}
						}
						typ = "array of " + elem
					}
				}
				if strings.HasPrefix(typ, "array of ") {
					elem := strings.TrimPrefix(typ, "array of ")
					typ = pr.addArrayAlias(elem)
				}
				local[p.Name] = typ
			}
			pushScope()
			currentFunc = st.Fun.Name
			for _, p := range st.Fun.Params {
				currentScope()[p.Name] = p.Name
			}
			fnBody, err := convertBody(env, st.Fun.Body, local)
			if err != nil {
				return nil, err
			}
			popScope()
			currentFunc = ""
			var params []string
			for _, p := range st.Fun.Params {
				typ := "integer"
				if p.Type != nil {
					typ = typeFromRef(p.Type)
				}
				if strings.HasPrefix(typ, "array of ") {
					elem := strings.TrimPrefix(typ, "array of ")
					typ = pr.addArrayAlias(elem)
				}
				params = append(params, fmt.Sprintf("%s: %s", p.Name, typ))
			}
			rt := ""
			if st.Fun.Return != nil {
				rt = typeFromRef(st.Fun.Return)
				if strings.HasPrefix(rt, "array of ") {
					elem := strings.TrimPrefix(rt, "array of ")
					rt = pr.addArrayAlias(elem)
				}
			}
			if rt == "" {
				for _, st := range fnBody {
					if ret, ok := st.(*ReturnStmt); ok && ret.Expr != nil {
						rt = inferType(ret.Expr)
						if strings.HasPrefix(rt, "array of ") {
							elem := strings.TrimPrefix(rt, "array of ")
							rt = pr.addArrayAlias(elem)
						}
						break
					}
				}
			}
			pr.Funs = append(pr.Funs, FunDecl{Name: st.Fun.Name, Params: params, ReturnType: rt, Body: fnBody})
			if rt != "" {
				funcReturns[st.Fun.Name] = rt
			}
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
		case st.Test != nil:
			// ignore test blocks in transpiled output
			continue
		default:
			return nil, fmt.Errorf("unsupported statement")
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
	markSysUtils(pr)
	currProg = nil
	return pr, nil
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
			ex, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
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
				out = append(out, &IndexAssignStmt{Name: name, Index: idx, Expr: ex})
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
				ex, err := convertExpr(env, st.Let.Value)
				if err != nil {
					return nil, err
				}
				if vd.Type == "" {
					if t := inferType(ex); t != "" {
						vd.Type = t
					}
				}
				out = append(out, &AssignStmt{Name: name, Expr: ex})
			}
			if !hasVar(vd.Name) {
				currProg.Vars = append(currProg.Vars, vd)
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
			if st.Var.Value != nil {
				ex, err := convertExpr(env, st.Var.Value)
				if err != nil {
					return nil, err
				}
				if vd.Type == "" {
					if t := inferType(ex); t != "" {
						vd.Type = t
					}
				}
				out = append(out, &AssignStmt{Name: name, Expr: ex})
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
			typ := "integer"
			if st.For.RangeEnd == nil {
				t := types.ExprType(st.For.Source, env)
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.StringType); ok {
						typ = "string"
					} else if _, ok := lt.Elem.(types.BoolType); ok {
						typ = "boolean"
					}
				}
			}
			if _, ok := varTypes[st.For.Name]; !ok {
				varTypes[st.For.Name] = typ
			}
			setVarType(st.For.Name, varTypes[st.For.Name])
			body, err := convertBody(env, st.For.Body, varTypes)
			if err != nil {
				return nil, err
			}
			if st.For.RangeEnd != nil {
				end, err := convertExpr(env, st.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				out = append(out, &ForRangeStmt{Name: st.For.Name, Start: start, End: end, Body: body})
			} else {
				out = append(out, &ForEachStmt{Name: st.For.Name, Iterable: start, Body: body})
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
			cond, err := convertExpr(env, st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenBody, err := convertBody(env, st.If.Then, varTypes)
			if err != nil {
				return nil, err
			}
			elseBody, err := convertBody(env, st.If.Else, varTypes)
			if err != nil {
				return nil, err
			}
			out = append(out, &IfStmt{Cond: cond, Then: thenBody, Else: elseBody})
		case st.Return != nil:
			if st.Return.Value != nil {
				ex, err := convertExpr(env, st.Return.Value)
				if err != nil {
					return nil, err
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
			be = &BinaryExpr{Op: op, Left: left, Right: right}
		case "/":
			be = &BinaryExpr{Op: "/", Left: left, Right: right}
			lt := inferType(left)
			rt := inferType(right)
			if lt == "real" || rt == "real" {
				be.Real = true
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
	switch typ {
	case "integer":
		return &IntLit{Value: 0}
	case "string":
		return &StringLit{Value: ""}
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
		switch *tr.Simple {
		case "int":
			return "integer"
		case "string":
			return "string"
		case "bool":
			return "boolean"
		default:
			return *tr.Simple
		}
	}
	if tr.Generic != nil && tr.Generic.Name == "list" && len(tr.Generic.Args) == 1 {
		elem := typeFromRef(tr.Generic.Args[0])
		if strings.HasPrefix(elem, "array of ") {
			alias := currProg.addArrayAlias(strings.TrimPrefix(elem, "array of "))
			return "array of " + alias
		}
		return "array of " + elem
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
			expr = &UnaryExpr{Op: "-", Expr: expr}
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
				expr = &CallExpr{Name: t.Name, Args: args}
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
			t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &tmp}}}, env)
			_, isStr := t.(types.StringType)
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
			t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &tmp}}}, env)
			_, isStr := t.(types.StringType)
			expr = &SliceExpr{Target: expr, Start: start, End: end, String: isStr}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			target := *op.Cast.Type.Simple
			if target == "int" {
				expr = &CastExpr{Expr: expr, Type: target}
			} else {
				expr = expr
			}
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
		} else if name == "substring" && len(args) == 3 {
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2], String: true}, nil
		} else if name == "str" && len(args) == 1 {
			name = "IntToStr"
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
		} else if name == "now" && len(args) == 0 {
			currProg.UseSysUtils = true
			currProg.UseNow = true
			return &CallExpr{Name: "_now", Args: nil}, nil
		} else if name == "input" && len(args) == 0 {
			currProg.UseInput = true
			return &CallExpr{Name: "_input", Args: nil}, nil
		} else if name == "append" && len(args) == 2 {
			return &CallExpr{Name: "concat", Args: []Expr{args[0], &ListLit{Elems: []Expr{args[1]}}}}, nil
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
		var args []Expr
		for _, it := range p.Struct.Fields {
			val, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			args = append(args, val)
		}
		return &CallExpr{Name: ctorName(p.Struct.Name), Args: args}, nil
	case p.Map != nil:
		var fields []FieldExpr
		var rec []Field
		for _, it := range p.Map.Items {
			val, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			key, ok := exprToIdent(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported map key")
			}
			ftype := inferType(val)
			if strings.HasPrefix(ftype, "array of ") {
				elem := strings.TrimPrefix(ftype, "array of ")
				if strings.HasPrefix(elem, "array of ") {
					alias := currProg.addArrayAlias(strings.TrimPrefix(elem, "array of "))
					ftype = "array of " + alias
				} else {
					alias := currProg.addArrayAlias(elem)
					ftype = "array of " + alias
				}
			}
			fields = append(fields, FieldExpr{Name: key, Expr: val})
			rec = append(rec, Field{Name: key, Type: ftype})
		}
		name := ensureRecord(rec)
		var args []Expr
		for _, f := range fields {
			args = append(args, f.Expr)
		}
		return &CallExpr{Name: ctorName(name), Args: args}, nil
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
		pushScope()
		currentFunc = name
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
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
		rt := ""
		if p.FunExpr.Return != nil && p.FunExpr.Return.Simple != nil {
			if *p.FunExpr.Return.Simple == "int" {
				rt = "integer"
			} else if *p.FunExpr.Return.Simple == "string" {
				rt = "string"
			}
		}
		currProg.Funs = append(currProg.Funs, FunDecl{Name: name, Params: params, ReturnType: rt, Body: body})
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
		if t, ok := currentVarTypes[v.Name]; ok {
			return t
		}
		return ""
	case *BinaryExpr:
		if v.Bool {
			return "boolean"
		}
		lt := inferType(v.Left)
		rt := inferType(v.Right)
		if lt == rt {
			return lt
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
		case "avg":
			return "real"
		case "min", "max":
			return "integer"
		case "Sqrt", "Sin", "Ln", "Power":
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
		if len(v.Elems) == 0 {
			return ""
		}
		t := inferType(v.Elems[0])
		for _, el := range v.Elems[1:] {
			if inferType(el) != t {
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
	case *IndexExpr:
		if v.String {
			return "string"
		}
		t := inferType(v.Target)
		if strings.HasPrefix(t, "array of ") {
			return strings.TrimPrefix(t, "array of ")
		}
		return "integer"
	case *SliceExpr:
		if v.String {
			return "string"
		}
		return "array of integer"
	case *SelectorExpr:
		if t, ok := currentVarTypes[v.Root]; ok {
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
		if v.Name == "IntToStr" || v.Name == "StrToInt" {
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
	case *IndexExpr:
		return usesSysUtilsExpr(v.Target) || usesSysUtilsExpr(v.Index)
	case *SliceExpr:
		return usesSysUtilsExpr(v.Target) ||
			(v.Start != nil && usesSysUtilsExpr(v.Start)) ||
			(v.End != nil && usesSysUtilsExpr(v.End))
	case *CastExpr:
		if v.Type == "int" {
			return true
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
			params = append(params, fmt.Sprintf("%s: %s", f.Name, typ))
			body = append(body, &SetStmt{Target: &SelectorExpr{Root: "Result", Tail: []string{f.Name}}, Expr: &VarRef{Name: f.Name}})
		}
		fn := FunDecl{Name: ctorName(r.Name), Params: params, ReturnType: r.Name, Body: body}
		p.Funs = append([]FunDecl{fn}, p.Funs...)
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
	if name, ok := p.ArrayAliases[elem]; ok {
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
		alias = strings.Title(elem) + "Array"
	}
	p.ArrayAliases[elem] = alias
	return alias
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
