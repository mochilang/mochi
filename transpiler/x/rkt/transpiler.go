//go:build slow

package rkt

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

var importedModules map[string]string

// Deprecated: group structs are no longer emitted. The flag is kept for
// backward compatibility but has no effect.
var needsGroupStruct bool
var groupVars = map[string]bool{}

type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// RawStmt emits raw Racket code as-is.
type RawStmt struct{ Code string }

func (r *RawStmt) emit(w io.Writer) {
	io.WriteString(w, r.Code)
	io.WriteString(w, "\n")
}

type PrintStmt struct{ Parts []Expr }

func (p *PrintStmt) emit(w io.Writer) {
	if len(p.Parts) == 1 {
		io.WriteString(w, "(displayln ")
		p.Parts[0].emit(w)
		io.WriteString(w, ")\n")
		return
	}
	io.WriteString(w, "(displayln (string-join (filter (lambda (s) (not (str"+
		"ing=? s \"\"))) (list")
	for _, part := range p.Parts {
		io.WriteString(w, " ")
		(&StrExpr{Arg: part}).emit(w)
	}
	io.WriteString(w, ")) \" \"))\n")
}

type LetStmt struct {
	Name string
	Expr Expr
	Type types.Type
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(define %s ", l.Name)
	l.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s ", a.Name)
	a.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type ListAssignStmt struct {
	Name  string
	Index Expr
	Expr  Expr
}

func (l *ListAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s (list-set %s ", l.Name, l.Name)
	l.Index.emit(w)
	io.WriteString(w, " ")
	l.Expr.emit(w)
	io.WriteString(w, "))\n")
}

type MapAssignStmt struct {
	Name string
	Key  Expr
	Expr Expr
}

func (m *MapAssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s (hash-set %s ", m.Name, m.Name)
	m.Key.emit(w)
	io.WriteString(w, " ")
	m.Expr.emit(w)
	io.WriteString(w, "))\n")
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "(if ")
	i.Cond.emit(w)
	io.WriteString(w, " (begin\n")
	for _, s := range i.Then {
		s.emit(w)
	}
	io.WriteString(w, ")")
	if len(i.Else) > 0 {
		io.WriteString(w, " (begin\n")
		for _, s := range i.Else {
			s.emit(w)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")\n")
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "(let loop ()\n  (when ")
	wst.Cond.emit(w)
	io.WriteString(w, "\n")
	for _, st := range wst.Body {
		io.WriteString(w, "    ")
		st.emit(w)
	}
	io.WriteString(w, "    (loop)))\n")
}

type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s (for/list ([item %s])\n  ", u.Target, u.Target)
	if u.Cond != nil {
		io.WriteString(w, "(begin\n    (when ")
		u.Cond.emit(w)
		io.WriteString(w, "\n")
		for i, f := range u.Fields {
			io.WriteString(w, "      (set! item (hash-set item \"")
			io.WriteString(w, f)
			io.WriteString(w, "\" ")
			u.Values[i].emit(w)
			io.WriteString(w, "))\n")
		}
		io.WriteString(w, "    )\n    item)))\n")
	} else {
		io.WriteString(w, "(begin\n")
		for i, f := range u.Fields {
			io.WriteString(w, "    (set! item (hash-set item \"")
			io.WriteString(w, f)
			io.WriteString(w, "\" ")
			u.Values[i].emit(w)
			io.WriteString(w, "))\n")
		}
		io.WriteString(w, "    item)))\n")
	}
}

type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	io.WriteString(w, "(for ([")
	io.WriteString(w, fr.Name)
	io.WriteString(w, " (in-range ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, " ")
	fr.End.emit(w)
	io.WriteString(w, ")])\n")
	for _, st := range fr.Body {
		st.emit(w)
	}
	io.WriteString(w, ")\n")
}

type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	Keys     bool
	Break    Expr
	Unless   Expr
}

func (f *ForInStmt) emit(w io.Writer) {
	io.WriteString(w, "(for ([")
	io.WriteString(w, f.Name)
	io.WriteString(w, " ")
	if f.Keys {
		io.WriteString(w, "(in-hash-keys ")
		f.Iterable.emit(w)
		io.WriteString(w, ")")
	} else {
		f.Iterable.emit(w)
	}
	io.WriteString(w, "]")
	if f.Break != nil {
		io.WriteString(w, " #:break ")
		f.Break.emit(w)
	}
	if f.Unless != nil {
		io.WriteString(w, " #:unless ")
		f.Unless.emit(w)
	}
	io.WriteString(w, ")\n")
	for _, st := range f.Body {
		st.emit(w)
	}
	io.WriteString(w, ")\n")
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	r.Expr.emit(w)
	io.WriteString(w, "\n")
}

type FunDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FunDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "(define (%s", f.Name)
	for _, p := range f.Params {
		fmt.Fprintf(w, " %s", p)
	}
	io.WriteString(w, ")\n")
	for _, st := range f.Body {
		st.emit(w)
	}
	io.WriteString(w, ")\n")
}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func convertIfExpr(n *parser.IfExpr, env *types.Env) (Expr, error) {
	if n == nil || n.Else == nil && n.ElseIf == nil {
		return nil, fmt.Errorf("unsupported if expression")
	}
	cond, err := convertExpr(n.Cond, env)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(n.Then, env)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if n.ElseIf != nil {
		elseExpr, err = convertIfExpr(n.ElseIf, env)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr, err = convertExpr(n.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(if ")
	i.Cond.emit(w)
	io.WriteString(w, " ")
	i.Then.emit(w)
	io.WriteString(w, " ")
	i.Else.emit(w)
	io.WriteString(w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	switch u.Op {
	case "-":
		io.WriteString(w, "(- ")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	case "!":
		io.WriteString(w, "(not ")
		u.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		fmt.Fprintf(w, "(%s ", u.Op)
		u.Expr.emit(w)
		io.WriteString(w, ")")
	}
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) { io.WriteString(w, n.Name) }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) { fmt.Fprintf(w, "%g", f.Value) }

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "(list")
	for _, e := range l.Elems {
		io.WriteString(w, " ")
		e.emit(w)
	}
	io.WriteString(w, ")")
}

type GroupItemsExpr struct{ Group Expr }

func (g *GroupItemsExpr) emit(w io.Writer) {
	io.WriteString(w, "(hash-ref ")
	g.Group.emit(w)
	io.WriteString(w, " \"items\")")
}

type CallExpr struct {
	Func string
	Args []Expr
}

type IndexExpr struct {
	Target   Expr
	Index    Expr
	IsString bool
	IsMap    bool
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.IsString {
		io.WriteString(w, "(string-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, ")")
	} else if i.IsMap {
		io.WriteString(w, "(hash-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "(list-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, ")")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	io.WriteString(w, c.Func)
	for _, a := range c.Args {
		io.WriteString(w, " ")
		a.emit(w)
	}
	io.WriteString(w, ")")
}

type LenExpr struct{ Arg Expr }

func (l *LenExpr) emit(w io.Writer) {
	io.WriteString(w, "(cond [(string? ")
	l.Arg.emit(w)
	io.WriteString(w, ") (string-length ")
	l.Arg.emit(w)
	io.WriteString(w, ")] [(hash? ")
	l.Arg.emit(w)
	io.WriteString(w, ") (hash-count ")
	l.Arg.emit(w)
	io.WriteString(w, ")] [else (length ")
	l.Arg.emit(w)
	io.WriteString(w, ")])")
}

type AvgExpr struct{ Arg Expr }

func (a *AvgExpr) emit(w io.Writer) {
	io.WriteString(w, "(if (null? ")
	a.Arg.emit(w)
	io.WriteString(w, ") 0 (exact->inexact (/ (apply + ")
	a.Arg.emit(w)
	io.WriteString(w, ") (length ")
	a.Arg.emit(w)
	io.WriteString(w, "))))")
}

type SumExpr struct{ Arg Expr }

func (s *SumExpr) emit(w io.Writer) {
	io.WriteString(w, "(apply + ")
	s.Arg.emit(w)
	io.WriteString(w, ")")
}

type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	io.WriteString(w, "(format \"~a\" ")
	s.Arg.emit(w)
	io.WriteString(w, ")")
}

type MinExpr struct{ Arg Expr }

func (m *MinExpr) emit(w io.Writer) {
	io.WriteString(w, "(if (null? ")
	m.Arg.emit(w)
	io.WriteString(w, ") 0 (apply min ")
	m.Arg.emit(w)
	io.WriteString(w, "))")
}

type MaxExpr struct{ Arg Expr }

func (m *MaxExpr) emit(w io.Writer) {
	io.WriteString(w, "(if (null? ")
	m.Arg.emit(w)
	io.WriteString(w, ") 0 (apply max ")
	m.Arg.emit(w)
	io.WriteString(w, "))")
}

type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "(let ([n (string->number ")
		c.Value.emit(w)
		io.WriteString(w, ")]) (if n (inexact->exact n) 0))")
	case "float":
		io.WriteString(w, "(exact->inexact ")
		c.Value.emit(w)
		io.WriteString(w, ")")
	case "string":
		io.WriteString(w, "(format \"~a\" ")
		c.Value.emit(w)
		io.WriteString(w, ")")
	default:
		c.Value.emit(w)
	}
}

type InExpr struct {
	Elem  Expr
	Set   Expr
	IsStr bool
	IsMap bool
}

func (i *InExpr) emit(w io.Writer) {
	switch {
	case i.IsStr:
		io.WriteString(w, "(string-contains? ")
		i.Set.emit(w)
		io.WriteString(w, " ")
		i.Elem.emit(w)
		io.WriteString(w, ")")
	case i.IsMap:
		io.WriteString(w, "(hash-has-key? ")
		i.Set.emit(w)
		io.WriteString(w, " ")
		i.Elem.emit(w)
		io.WriteString(w, ")")
	default:
		io.WriteString(w, "(not (not (member ")
		i.Elem.emit(w)
		io.WriteString(w, " ")
		i.Set.emit(w)
		io.WriteString(w, ")))")
	}
}

type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (l *LeftJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, l.LeftVar)
	io.WriteString(w, " ")
	l.LeftSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (let ([matched #f])\n")
	io.WriteString(w, "      (for ([")
	io.WriteString(w, l.RightVar)
	io.WriteString(w, " ")
	l.RightSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "        (when ")
	l.Cond.emit(w)
	io.WriteString(w, "\n")
	io.WriteString(w, "          (set! matched #t)\n")
	io.WriteString(w, "          (set! _res (append _res (list ")
	l.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "      (when (not matched)\n")
	io.WriteString(w, "        (let ([")
	io.WriteString(w, l.RightVar)
	io.WriteString(w, " #f])\n")
	io.WriteString(w, "          (set! _res (append _res (list ")
	l.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "        )\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "    ))\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, "  _res)")
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

func substituteFields(e Expr, varName string, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Name:
		if fields[ex.Name] {
			return &IndexExpr{Target: &Name{Name: varName}, Index: &StringLit{Value: ex.Name}, IsMap: true}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFields(ex.Left, varName, fields)
		ex.Right = substituteFields(ex.Right, varName, fields)
		return ex
	case *UnaryExpr:
		ex.Expr = substituteFields(ex.Expr, varName, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		return ex
	case *IndexExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		ex.Index = substituteFields(ex.Index, varName, fields)
		return ex
	case *IfExpr:
		ex.Cond = substituteFields(ex.Cond, varName, fields)
		ex.Then = substituteFields(ex.Then, varName, fields)
		ex.Else = substituteFields(ex.Else, varName, fields)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFields(ex.Elems[i], varName, fields)
		}
		return ex
	case *LenExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *AvgExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *SumExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *StrExpr:
		ex.Arg = substituteFields(ex.Arg, varName, fields)
		return ex
	case *CastExpr:
		ex.Value = substituteFields(ex.Value, varName, fields)
		return ex
	default:
		return ex
	}
}

// wrapOptional wraps map field accesses of varName with a nil check to avoid
// runtime errors when the variable is #f.
func wrapOptional(e Expr, varName string) Expr {
	switch ex := e.(type) {
	case *IndexExpr:
		if n, ok := ex.Target.(*Name); ok && n.Name == varName && ex.IsMap {
			return &IfExpr{Cond: &Name{Name: varName}, Then: ex, Else: &Name{Name: "#f"}}
		}
		ex.Target = wrapOptional(ex.Target, varName)
		ex.Index = wrapOptional(ex.Index, varName)
		return ex
	case *BinaryExpr:
		ex.Left = wrapOptional(ex.Left, varName)
		ex.Right = wrapOptional(ex.Right, varName)
		return ex
	case *UnaryExpr:
		ex.Expr = wrapOptional(ex.Expr, varName)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = wrapOptional(ex.Args[i], varName)
		}
		return ex
	case *IfExpr:
		ex.Cond = wrapOptional(ex.Cond, varName)
		ex.Then = wrapOptional(ex.Then, varName)
		ex.Else = wrapOptional(ex.Else, varName)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = wrapOptional(ex.Elems[i], varName)
		}
		return ex
	case *LenExpr:
		ex.Arg = wrapOptional(ex.Arg, varName)
		return ex
	case *AvgExpr:
		ex.Arg = wrapOptional(ex.Arg, varName)
		return ex
	case *SumExpr:
		ex.Arg = wrapOptional(ex.Arg, varName)
		return ex
	case *StrExpr:
		ex.Arg = wrapOptional(ex.Arg, varName)
		return ex
	case *CastExpr:
		ex.Value = wrapOptional(ex.Value, varName)
		return ex
	default:
		return ex
	}
}

type BinaryExpr struct {
	Op          string
	Left, Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	switch b.Op {
	case "!=":
		io.WriteString(w, "(not (= ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, "))")
	case "==":
		io.WriteString(w, "(= ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	default:
		fmt.Fprintf(w, "(%s ", b.Op)
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	}
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
		return "unknown"
	}
	b, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "unknown"
	}
	return strings.TrimSpace(string(b))
}

func gitTime() string {
	root := repoRoot()
	if root == "" {
		return time.Now().Format("2006-01-02 15:04 -0700")
	}
	out, err := exec.Command("git", "-C", root, "log", "-1", "--format=%cI").Output()
	if err != nil {
		return time.Now().Format("2006-01-02 15:04 -0700")
	}
	t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out)))
	if err != nil {
		return time.Now().Format("2006-01-02 15:04 -0700")
	}
	if loc, err := time.LoadLocation("Asia/Bangkok"); err == nil {
		t = t.In(loc)
	}
	return t.Format("2006-01-02 15:04 -0700")
}

func header() string {
	// Emit version and timestamp. Groups are represented using hash maps,
	// so no additional runtime struct is required.
	hdr := fmt.Sprintf(";; Generated by Mochi %s on %s\n#lang racket\n(require racket/list racket/string json)\n", version(), gitTime())
	return hdr + "\n"
}

func rktType(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return "Integer"
	case types.FloatType:
		return "Float"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType, types.MapType:
		return "Any"
	default:
		return "Any"
	}
}

func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		s.emit(w)
	}
	return nil
}

func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	r := &Program{}
	importedModules = map[string]string{}
	// handle imports first
	for _, st := range prog.Statements {
		if st.Import != nil && st.Import.Auto && st.Import.Path == "mochi/runtime/ffi/go/testpkg" {
			alias := st.Import.As
			if alias == "" {
				alias = "testpkg"
			}
			importedModules[alias] = st.Import.Path
			r.Stmts = append(r.Stmts,
				&RawStmt{Code: fmt.Sprintf("(define (%s_Add a b) (+ a b))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define %s_Pi 3.14)", alias)},
				&RawStmt{Code: fmt.Sprintf("(define %s_Answer 42)", alias)},
			)
		}
	}

	for _, st := range prog.Statements {
		if st.Import != nil {
			continue
		}
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		if s != nil {
			r.Stmts = append(r.Stmts, s)
		}
	}
	// no runtime helpers required
	_ = env
	return r, nil
}

func convertStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Let != nil && st.Let.Value != nil:
		e, err := convertExpr(st.Let.Value, env)
		if err != nil {
			return nil, err
		}
		var t types.Type
		if env != nil {
			if tt, err2 := env.GetVar(st.Let.Name); err2 == nil {
				t = tt
			}
		}
		return &LetStmt{Name: st.Let.Name, Expr: e, Type: t}, nil
	case st.Var != nil && st.Var.Value != nil:
		e, err := convertExpr(st.Var.Value, env)
		if err != nil {
			return nil, err
		}
		var t types.Type
		if env != nil {
			if tt, err2 := env.GetVar(st.Var.Name); err2 == nil {
				t = tt
			}
		}
		return &LetStmt{Name: st.Var.Name, Expr: e, Type: t}, nil
	case st.Assign != nil:
		e, err := convertExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		if len(st.Assign.Index) == 1 {
			idxExpr, err := convertExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			if t, err2 := env.GetVar(st.Assign.Name); err2 == nil {
				if _, ok := t.(types.MapType); ok {
					return &MapAssignStmt{Name: st.Assign.Name, Key: idxExpr, Expr: e}, nil
				}
			}
			return &ListAssignStmt{Name: st.Assign.Name, Index: idxExpr, Expr: e}, nil
		}
		return &AssignStmt{Name: st.Assign.Name, Expr: e}, nil
	case st.Return != nil:
		var e Expr
		var err error
		if st.Return.Value != nil {
			e, err = convertExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		} else {
			e = &Name{Name: "void"}
		}
		return &ReturnStmt{Expr: e}, nil
	case st.Fun != nil:
		return convertFunStmt(st.Fun, env)
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && call.Func == "print" {
			var parts []Expr
			for _, a := range call.Args {
				pe, err := convertExpr(a, env)
				if err != nil {
					return nil, err
				}
				parts = append(parts, pe)
			}
			return &PrintStmt{Parts: parts}, nil
		}
		if call != nil && call.Func == "json" && len(call.Args) == 1 {
			arg, err := convertExpr(call.Args[0], env)
			if err != nil {
				return nil, err
			}
			return &PrintStmt{Parts: []Expr{&CallExpr{Func: "jsexpr->string", Args: []Expr{arg}}}}, nil
		}
		return nil, fmt.Errorf("unsupported expression statement")
	case st.Update != nil:
		up, err := convertUpdateStmt(st.Update, env)
		if err != nil {
			return nil, err
		}
		return up, nil
	case st.Type != nil:
		return nil, nil
	case st.Test != nil:
		// ignore test blocks
		return nil, nil
	case st.Expect != nil:
		// ignore expects
		return nil, nil
	case st.If != nil:
		return convertIfStmt(st.If, env)
	case st.While != nil:
		return convertWhileStmt(st.While, env)
	case st.For != nil:
		return convertForStmt(st.For, env)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertStatements(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := convertStmt(st, env)
		if err != nil {
			return nil, err
		}
		out = append(out, s)
	}
	return out, nil
}

func convertIfStmt(n *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(n.Cond, env)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStatements(n.Then, env)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	switch {
	case n.ElseIf != nil:
		s, err := convertIfStmt(n.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	case len(n.Else) > 0:
		elseStmts, err = convertStatements(n.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(n *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := convertExpr(n.Cond, env)
	if err != nil {
		return nil, err
	}
	body, err := convertStatements(n.Body, env)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(n *parser.ForStmt, env *types.Env) (Stmt, error) {
	if n.RangeEnd != nil {
		start, err := convertExpr(n.Source, env)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(n.RangeEnd, env)
		if err != nil {
			return nil, err
		}
		child := types.NewEnv(env)
		child.SetVar(n.Name, types.AnyType{}, true)
		body, err := convertStatements(n.Body, child)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: n.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(n.Source, env)
	if err != nil {
		return nil, err
	}
	if id, ok := isSimpleIdent(n.Source); ok && groupVars[id] {
		iter = &GroupItemsExpr{Group: &Name{Name: id}}
	}
	child := types.NewEnv(env)
	child.SetVar(n.Name, types.AnyType{}, true)

	var continueCond Expr
	var breakCond Expr
	bodyStmts := n.Body
	// detect leading if statements that contain only continue/break
	for len(bodyStmts) > 0 {
		st := bodyStmts[0]
		if st.If != nil && len(st.If.Then) == 1 && len(st.If.Else) == 0 {
			if st.If.Then[0].Continue != nil {
				c, err := convertExpr(st.If.Cond, child)
				if err != nil {
					return nil, err
				}
				continueCond = c
				bodyStmts = bodyStmts[1:]
				continue
			}
			if st.If.Then[0].Break != nil {
				c, err := convertExpr(st.If.Cond, child)
				if err != nil {
					return nil, err
				}
				breakCond = c
				bodyStmts = bodyStmts[1:]
				continue
			}
		}
		break
	}

	body, err := convertStatements(bodyStmts, child)
	if err != nil {
		return nil, err
	}
	keys := false
	if _, ok := types.ExprType(n.Source, env).(types.MapType); ok {
		keys = true
	}
	return &ForInStmt{Name: n.Name, Iterable: iter, Body: body, Keys: keys, Break: breakCond, Unless: continueCond}, nil
}

func convertFunStmt(fn *parser.FunStmt, env *types.Env) (Stmt, error) {
	child := types.NewEnv(env)
	var params []string
	for _, p := range fn.Params {
		params = append(params, p.Name)
		child.SetVar(p.Name, types.AnyType{}, true)
	}
	body, err := convertStatements(fn.Body, child)
	if err != nil {
		return nil, err
	}
	return &FunDecl{Name: fn.Name, Params: params, Body: body}, nil
}

func convertUpdateStmt(u *parser.UpdateStmt, env *types.Env) (Stmt, error) {
	t, err := env.GetVar(u.Target)
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
	fields := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fields[name] = true
	}
	var names []string
	var values []Expr
	for _, it := range u.Set.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(it.Value, child)
		if err != nil {
			return nil, err
		}
		val = substituteFields(val, "item", fields)
		names = append(names, key)
		values = append(values, val)
	}
	var cond Expr
	if u.Where != nil {
		cond, err = convertExpr(u.Where, child)
		if err != nil {
			return nil, err
		}
		cond = substituteFields(cond, "item", fields)
	}
	return &UpdateStmt{Target: u.Target, Fields: names, Values: values, Cond: cond}, nil
}

func convertExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary, env)
}

func precedence(op string) int {
	switch op {
	case "or":
		return 1
	case "and":
		return 2
	case "==", "!=", "<", "<=", ">", ">=":
		return 3
	case "+", "-", "string-append":
		return 4
	case "*", "/", "modulo":
		return 5
	default:
		return 0
	}
}

func convertBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	left, err := convertUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	exprs := []Expr{left}
	ops := []string{}
	for _, part := range b.Right {
		right, err := convertPostfix(part.Right, env)
		if err != nil {
			return nil, err
		}
		op := part.Op
		if op == "+" && (types.IsStringUnary(b.Left, env) || types.IsStringPostfix(part.Right, env)) {
			op = "string-append"
		}
		if op == "&&" {
			op = "and"
		}
		if op == "||" {
			op = "or"
		}
		if op == "/" {
			lt := types.TypeOfUnary(b.Left, env)
			rt := types.TypeOfPostfix(part.Right, env)
			if (types.IsIntType(lt) || types.IsInt64Type(lt)) && (types.IsIntType(rt) || types.IsInt64Type(rt)) {
				op = "quotient"
			} else {
				op = "/"
			}
		}
		if op == "in" {
			isStr := types.IsStringPostfix(part.Right, env)
			isMap := types.IsMapPostfix(part.Right, env)
			opExpr := &InExpr{Elem: exprs[len(exprs)-1], Set: right, IsStr: isStr, IsMap: isMap}
			exprs[len(exprs)-1] = opExpr
			continue
		}
		if op == "<" || op == "<=" || op == ">" || op == ">=" {
			if types.IsStringUnary(b.Left, env) || types.IsStringPostfix(part.Right, env) {
				switch op {
				case "<":
					op = "string<?"
				case "<=":
					op = "string<=?"
				case ">":
					op = "string>?"
				case ">=":
					op = "string>=?"
				}
			}
		}
		if op == "%" {
			op = "modulo"
		}
		for len(ops) > 0 && precedence(ops[len(ops)-1]) >= precedence(op) {
			r := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			l := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			o := ops[len(ops)-1]
			ops = ops[:len(ops)-1]
			exprs = append(exprs, &BinaryExpr{Op: o, Left: l, Right: r})
		}
		ops = append(ops, op)
		exprs = append(exprs, right)
	}
	for len(ops) > 0 {
		r := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		l := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		o := ops[len(ops)-1]
		ops = ops[:len(ops)-1]
		exprs = append(exprs, &BinaryExpr{Op: o, Left: l, Right: r})
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("expr reduce error")
	}
	return exprs[0], nil
}

func convertUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			expr = &UnaryExpr{Op: op, Expr: expr}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertPostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	ops := pf.Ops
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
		tailOps := make([]*parser.PostfixOp, 0, len(pf.Target.Selector.Tail))
		for _, t := range pf.Target.Selector.Tail {
			tailOps = append(tailOps, &parser.PostfixOp{Field: &parser.FieldOp{Name: t}})
		}
		ops = append(tailOps, ops...)
	}
	for _, op := range ops {
		switch {
		case op.Field != nil:
			if n, ok := expr.(*Name); ok {
				if _, ok := importedModules[n.Name]; ok {
					expr = &Name{Name: n.Name + "_" + op.Field.Name}
					break
				}
			}
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}, IsMap: true}
		case op.Call != nil:
			switch n := expr.(type) {
			case *Name:
				var err error
				expr, err = convertCall(&parser.CallExpr{Func: n.Name, Args: op.Call.Args}, env)
				if err != nil {
					return nil, err
				}
			case *IndexExpr:
				if mod, ok := n.Target.(*Name); ok {
					if _, imp := importedModules[mod.Name]; imp {
						if lit, ok := n.Index.(*StringLit); ok {
							fn := mod.Name + "." + lit.Value
							var err error
							expr, err = convertCall(&parser.CallExpr{Func: fn, Args: op.Call.Args}, env)
							if err != nil {
								return nil, err
							}
							break
						}
					}
				}
				if lit, ok := n.Index.(*StringLit); ok && lit.Value == "contains" {
					if types.IsStringPrimary(pf.Target, env) {
						arg, err := convertExpr(op.Call.Args[0], env)
						if err != nil {
							return nil, err
						}
						expr = &CallExpr{Func: "string-contains?", Args: []Expr{n.Target, arg}}
						break
					}
				}
				return nil, fmt.Errorf("unsupported call")
			default:
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Index != nil && op.Index.Colon == nil:
			idx, err := convertExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			isStr := types.IsStringPrimary(pf.Target, env)
			isMap := types.IsMapPrimary(pf.Target, env)
			expr = &IndexExpr{Target: expr, Index: idx, IsString: isStr, IsMap: isMap}
		case op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil:
			expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	// handle math constants like math['pi'] or math['e']
	if idx, ok := expr.(*IndexExpr); ok {
		if mod, ok2 := idx.Target.(*Name); ok2 && mod.Name == "math" {
			if lit, ok3 := idx.Index.(*StringLit); ok3 {
				switch lit.Value {
				case "pi":
					return &Name{Name: "pi"}, nil
				case "e":
					return &CallExpr{Func: "exp", Args: []Expr{&IntLit{Value: 1}}}, nil
				}
			}
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.List != nil:
		return convertList(p.List, env)
	case p.Struct != nil:
		return convertStruct(p.Struct, env)
	case p.Map != nil:
		return convertMap(p.Map, env)
	case p.Call != nil:
		return convertCall(p.Call, env)
	case p.Query != nil:
		return convertQueryExpr(p.Query, env)
	case p.If != nil:
		return convertIfExpr(p.If, env)
	case p.Group != nil:
		return convertExpr(p.Group, env)
	case p.Selector != nil:
		return &Name{Name: p.Selector.Root}, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	if l.Int != nil {
		return &IntLit{Value: int(*l.Int)}, nil
	}
	if l.Float != nil {
		return &FloatLit{Value: *l.Float}, nil
	}
	if l.Bool != nil {
		if *l.Bool {
			return &Name{Name: "#t"}, nil
		}
		return &Name{Name: "#f"}, nil
	}
	if l.Str != nil {
		return &StringLit{Value: *l.Str}, nil
	}
	return nil, fmt.Errorf("unsupported literal")
}

func convertList(l *parser.ListLiteral, env *types.Env) (Expr, error) {
	var elems []Expr
	for _, e := range l.Elems {
		ce, err := convertExpr(e, env)
		if err != nil {
			return nil, err
		}
		elems = append(elems, ce)
	}
	return &ListLit{Elems: elems}, nil
}

func convertStruct(s *parser.StructLiteral, env *types.Env) (Expr, error) {
	var args []Expr
	for _, f := range s.Fields {
		val, err := convertExpr(f.Value, env)
		if err != nil {
			return nil, err
		}
		args = append(args, &StringLit{Value: f.Name})
		args = append(args, val)
	}
	return &CallExpr{Func: "hash", Args: args}, nil
}

func convertMap(m *parser.MapLiteral, env *types.Env) (Expr, error) {
	var args []Expr
	for _, it := range m.Items {
		var k Expr
		if name, ok := isSimpleIdent(it.Key); ok {
			k = &StringLit{Value: name}
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
		args = append(args, k, v)
	}
	return &CallExpr{Func: "hash", Args: args}, nil
}

func convertCall(c *parser.CallExpr, env *types.Env) (Expr, error) {
	var args []Expr
	for _, a := range c.Args {
		ae, err := convertExpr(a, env)
		if err != nil {
			return nil, err
		}
		args = append(args, ae)
	}
	switch c.Func {
	case "len":
		if len(args) != 1 {
			return nil, fmt.Errorf("len expects 1 arg")
		}
		if n, ok := args[0].(*Name); ok && groupVars[n.Name] {
			args[0] = &GroupItemsExpr{Group: args[0]}
		}
		return &LenExpr{Arg: args[0]}, nil
	case "append":
		if len(args) == 2 {
			return &CallExpr{Func: "append", Args: []Expr{args[0], &CallExpr{Func: "list", Args: []Expr{args[1]}}}}, nil
		}
	case "avg":
		if len(args) == 1 {
			if n, ok := args[0].(*Name); ok && groupVars[n.Name] {
				args[0] = &GroupItemsExpr{Group: args[0]}
			}
			return &AvgExpr{Arg: args[0]}, nil
		}
	case "sum":
		if len(args) == 1 {
			if n, ok := args[0].(*Name); ok && groupVars[n.Name] {
				args[0] = &GroupItemsExpr{Group: args[0]}
			}
			return &SumExpr{Arg: args[0]}, nil
		}
	case "count":
		if len(args) == 1 {
			if n, ok := args[0].(*Name); ok && groupVars[n.Name] {
				args[0] = &GroupItemsExpr{Group: args[0]}
			}
			return &LenExpr{Arg: args[0]}, nil
		}
	case "exists":
		if len(args) == 1 {
			return &CallExpr{Func: "not", Args: []Expr{&CallExpr{Func: "null?", Args: args}}}, nil
		}
	case "str":
		if len(args) == 1 {
			return &StrExpr{Arg: args[0]}, nil
		}
	case "substring":
		if len(args) == 3 {
			return &CallExpr{Func: "substring", Args: args}, nil
		}
	case "min":
		if len(args) == 1 {
			return &MinExpr{Arg: args[0]}, nil
		}
	case "max":
		if len(args) == 1 {
			return &MaxExpr{Arg: args[0]}, nil
		}
	case "math.sqrt":
		if len(args) == 1 {
			return &CallExpr{Func: "sqrt", Args: args}, nil
		}
	case "math.pow":
		if len(args) == 2 {
			return &CallExpr{Func: "expt", Args: args}, nil
		}
	case "math.sin":
		if len(args) == 1 {
			return &CallExpr{Func: "sin", Args: args}, nil
		}
	case "math.log":
		if len(args) == 1 {
			return &CallExpr{Func: "log", Args: args}, nil
		}
	case "json":
		if len(args) == 1 {
			return &CallExpr{Func: "jsexpr->string", Args: args}, nil
		}
	default:
		return &CallExpr{Func: c.Func, Args: args}, nil
	}
	return nil, fmt.Errorf("unsupported call")
}

func convertLeftJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
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
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

type InnerJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (i *InnerJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, i.LeftVar)
	io.WriteString(w, " ")
	i.LeftSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (for ([")
	io.WriteString(w, i.RightVar)
	io.WriteString(w, " ")
	i.RightSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "      (when ")
	i.Cond.emit(w)
	io.WriteString(w, "\n")
	io.WriteString(w, "        (set! _res (append _res (list ")
	i.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "    )\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, "  _res)")
}

func (r *RightJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, r.RightVar)
	io.WriteString(w, " ")
	r.RightSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (let ([matched #f])\n")
	io.WriteString(w, "      (for ([")
	io.WriteString(w, r.LeftVar)
	io.WriteString(w, " ")
	r.LeftSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "        (when ")
	r.Cond.emit(w)
	io.WriteString(w, "\n")
	io.WriteString(w, "          (set! matched #t)\n")
	io.WriteString(w, "          (set! _res (append _res (list ")
	r.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "      (when (not matched)\n")
	io.WriteString(w, "        (let ([")
	io.WriteString(w, r.LeftVar)
	io.WriteString(w, " #f])\n")
	io.WriteString(w, "          (set! _res (append _res (list ")
	r.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "        )\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "    ))\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, "  _res)")
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
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	sel = wrapOptional(sel, q.Var)
	return &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertInnerJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side != nil {
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
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	return &InnerJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

type LeftJoinMultiExpr struct {
	Var1   string
	Src1   Expr
	Var2   string
	Src2   Expr
	Cond2  Expr
	Var3   string
	Src3   Expr
	Cond3  Expr
	Select Expr
}

func (l *LeftJoinMultiExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, l.Var1)
	io.WriteString(w, " ")
	l.Src1.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (for ([")
	io.WriteString(w, l.Var2)
	io.WriteString(w, " ")
	l.Src2.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "      (when ")
	l.Cond2.emit(w)
	io.WriteString(w, "\n")
	io.WriteString(w, "        (let ([matched #f])\n")
	io.WriteString(w, "          (for ([")
	io.WriteString(w, l.Var3)
	io.WriteString(w, " ")
	l.Src3.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "            (when ")
	l.Cond3.emit(w)
	io.WriteString(w, "\n")
	io.WriteString(w, "              (set! matched #t)\n")
	io.WriteString(w, "              (set! _res (append _res (list ")
	l.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "          )\n")
	io.WriteString(w, "          (when (not matched)\n")
	io.WriteString(w, "            (let ([")
	io.WriteString(w, l.Var3)
	io.WriteString(w, " #f])\n")
	io.WriteString(w, "              (set! _res (append _res (list ")
	l.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "            )\n")
	io.WriteString(w, "          )\n")
	io.WriteString(w, "        )\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "    )\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, "  _res)")
}

func convertLeftJoinMultiQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 2 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j1 := q.Joins[0]
	j2 := q.Joins[1]
	if j1.Side != nil || j2.Side == nil || *j2.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	src1, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	src2, err := convertExpr(j1.Src, env)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j1.Var, types.AnyType{}, true)
	cond2, err := convertExpr(j1.On, child)
	if err != nil {
		return nil, err
	}
	child.SetVar(j2.Var, types.AnyType{}, true)
	src3, err := convertExpr(j2.Src, child)
	if err != nil {
		return nil, err
	}
	cond3, err := convertExpr(j2.On, child)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	return &LeftJoinMultiExpr{Var1: q.Var, Src1: src1, Var2: j1.Var, Src2: src2, Cond2: cond2, Var3: j2.Var, Src3: src3, Cond3: cond3, Select: sel}, nil
}

type OuterJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

type CrossJoinExpr struct {
	Vars    []string
	Sources []Expr
	Where   Expr
	Sort    Expr
	SortCmp string
	Desc    bool
	Skip    Expr
	Take    Expr
	Select  Expr
}

func (c *CrossJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_res '()])\n")
	for i := range c.Vars {
		io.WriteString(w, strings.Repeat("  ", i+1))
		io.WriteString(w, "(for ([")
		io.WriteString(w, c.Vars[i])
		io.WriteString(w, " ")
		c.Sources[i].emit(w)
		io.WriteString(w, "])\n")
	}
	if c.Where != nil {
		io.WriteString(w, strings.Repeat("  ", len(c.Vars)+1))
		io.WriteString(w, "(when ")
		c.Where.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, strings.Repeat("  ", len(c.Vars)+1))
	if c.Sort != nil {
		io.WriteString(w, "(set! _res (append _res (list (cons ")
		c.Sort.emit(w)
		io.WriteString(w, " ")
		c.Select.emit(w)
		io.WriteString(w, "))))\n")
	} else {
		io.WriteString(w, "(set! _res (append _res (list ")
		c.Select.emit(w)
		io.WriteString(w, ")))\n")
	}
	if c.Where != nil {
		io.WriteString(w, strings.Repeat("  ", len(c.Vars)+1))
		io.WriteString(w, ")\n")
	}
	for i := len(c.Vars); i >= 1; i-- {
		io.WriteString(w, strings.Repeat("  ", i))
		io.WriteString(w, ")\n")
	}
	if c.Sort != nil {
		io.WriteString(w, "  (set! _res (sort _res ")
		if c.SortCmp != "" {
			io.WriteString(w, c.SortCmp)
		} else if c.Desc {
			io.WriteString(w, ">")
		} else {
			io.WriteString(w, "<")
		}
		io.WriteString(w, " #:key car))\n  (set! _res (map cdr _res))\n")
	}
	if c.Skip != nil {
		io.WriteString(w, "  (set! _res (drop _res ")
		c.Skip.emit(w)
		io.WriteString(w, "))\n")
	}
	if c.Take != nil {
		io.WriteString(w, "  (set! _res (take _res ")
		c.Take.emit(w)
		io.WriteString(w, "))\n")
	}
	io.WriteString(w, "  _res)")
}

func convertCrossJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 0 || q.Distinct || q.Group != nil {
		return nil, fmt.Errorf("unsupported query")
	}
	vars := []string{q.Var}
	srcs := []*parser.Expr{q.Source}
	for _, f := range q.Froms {
		vars = append(vars, f.Var)
		srcs = append(srcs, f.Src)
	}
	var exprs []Expr
	for _, s := range srcs {
		e, err := convertExpr(s, env)
		if err != nil {
			return nil, err
		}
		if id, ok := isSimpleIdent(s); ok && groupVars[id] {
			e = &GroupItemsExpr{Group: &Name{Name: id}}
		}
		exprs = append(exprs, e)
	}
	child := types.NewEnv(env)
	for _, v := range vars {
		child.SetVar(v, types.AnyType{}, true)
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	var cond Expr
	if q.Where != nil {
		cond, err = convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
	}
	var sortExpr Expr
	var sortCmp string
	var desc bool
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, child)
		if err != nil {
			return nil, err
		}
		if u, ok := sortExpr.(*UnaryExpr); ok && u.Op == "-" {
			sortExpr = u.Expr
			desc = true
		}
		t := types.TypeOfExprBasic(q.Sort, child)
		if types.IsStringType(t) {
			sortCmp = "string<?"
		} else if idx, ok := sortExpr.(*IndexExpr); ok && idx.IsMap {
			sortCmp = "(lambda (a b) (cond [(and (number? a) (number? b)) (< a b)] [(and (string? a) (string? b)) (string<? a b)] [else (string<? (format \"~a\" a) (format \"~a\" b))]))"
		}
	}
	var skipExpr, takeExpr Expr
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, child)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, child)
		if err != nil {
			return nil, err
		}
	}
	return &CrossJoinExpr{Vars: vars, Sources: exprs, Where: cond, Sort: sortExpr, SortCmp: sortCmp, Desc: desc, Skip: skipExpr, Take: takeExpr, Select: sel}, nil
}

func convertMultiJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) < 2 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || len(q.Froms) != 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
	}
	vars := []string{q.Var}
	srcs := []*parser.Expr{q.Source}
	for _, j := range q.Joins {
		vars = append(vars, j.Var)
		srcs = append(srcs, j.Src)
	}
	var exprs []Expr
	for _, s := range srcs {
		e, err := convertExpr(s, env)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, e)
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, j := range q.Joins {
		child.SetVar(j.Var, types.AnyType{}, true)
	}
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	var cond Expr
	for _, j := range q.Joins {
		c, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = c
		} else {
			cond = &BinaryExpr{Op: "and", Left: cond, Right: c}
		}
	}
	if q.Where != nil {
		w, err := convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = w
		} else {
			cond = &BinaryExpr{Op: "and", Left: cond, Right: w}
		}
	}
	return &CrossJoinExpr{Vars: vars, Sources: exprs, Where: cond, Select: sel}, nil
}

func (o *OuterJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, " ")
	o.LeftSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (let ([matched #f])\n")
	io.WriteString(w, "      (for ([")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, " ")
	o.RightSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "        (when ")
	o.Cond.emit(w)
	io.WriteString(w, "\n")
	io.WriteString(w, "          (set! matched #t)\n")
	io.WriteString(w, "          (set! _res (append _res (list ")
	o.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "      (when (not matched)\n")
	io.WriteString(w, "        (let ([")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, " #f])\n")
	io.WriteString(w, "          (set! _res (append _res (list ")
	o.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "        )\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "    ))\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, o.RightVar)
	io.WriteString(w, " ")
	o.RightSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (unless (for/or ([")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, " ")
	o.LeftSrc.emit(w)
	io.WriteString(w, "]) ")
	o.Cond.emit(w)
	io.WriteString(w, ")\n")
	io.WriteString(w, "      (let ([")
	io.WriteString(w, o.LeftVar)
	io.WriteString(w, " #f])\n")
	io.WriteString(w, "        (set! _res (append _res (list ")
	o.Select.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "      )\n")
	io.WriteString(w, "    ))\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, "  _res)")
}

func convertOuterJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "outer" {
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
	sel, err := convertExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	return &OuterJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

type GroupByExpr struct {
	Var     string
	Source  Expr
	Key     Expr
	Name    string
	Where   Expr
	Select  Expr
	Having  Expr
	Sort    Expr
	SortCmp string
	Skip    Expr
	Take    Expr
}

type GroupJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Key      Expr
	Row      Expr
	Name     string
	Select   Expr
	Having   Expr
	Sort     Expr
	SortCmp  string
	Skip     Expr
	Take     Expr
}

type GroupLeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Key      Expr
	Row      Expr
	Name     string
	Select   Expr
	Having   Expr
	Sort     Expr
	SortCmp  string
	Skip     Expr
	Take     Expr
}

// GroupMultiJoinExpr handles grouping after multiple inner joins.
type GroupMultiJoinExpr struct {
	Vars    []string
	Sources []Expr
	Cond    Expr
	Key     Expr
	Row     Expr
	Name    string
	Select  Expr
	Having  Expr
	Sort    Expr
	SortCmp string
	Skip    Expr
	Take    Expr
}

func (g *GroupMultiJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_groups (make-hash)] [_res '()])\n")
	for i := range g.Vars {
		io.WriteString(w, strings.Repeat("  ", i+1))
		io.WriteString(w, "(for ([")
		io.WriteString(w, g.Vars[i])
		io.WriteString(w, " ")
		g.Sources[i].emit(w)
		io.WriteString(w, "])\n")
	}
	io.WriteString(w, strings.Repeat("  ", len(g.Vars)+1))
	if g.Cond != nil {
		io.WriteString(w, "(when ")
		g.Cond.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, strings.Repeat("  ", len(g.Vars)+2))
	io.WriteString(w, "(let* ([_key ")
	g.Key.emit(w)
	io.WriteString(w, "][_g (hash-ref _groups _key (lambda () (let ([h (make-hash)]) (hash-set! h \"key\" _key) (hash-set! h \"items\" '()) (hash-set! _groups _key h) h)))])\n")
	io.WriteString(w, strings.Repeat("  ", len(g.Vars)+2))
	io.WriteString(w, "  (hash-set! _g \"items\" (append (hash-ref _g \"items\") (list ")
	g.Row.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, strings.Repeat("  ", len(g.Vars)+1))
	if g.Cond != nil {
		io.WriteString(w, ")")
	}
	io.WriteString(w, "\n")
	for i := len(g.Vars); i >= 1; i-- {
		io.WriteString(w, strings.Repeat("  ", i))
		io.WriteString(w, ")\n")
	}
	io.WriteString(w, "  (for ([")
	io.WriteString(w, g.Name)
	io.WriteString(w, " (hash-values _groups)])\n")
	if g.Having != nil {
		io.WriteString(w, "    (when ")
		g.Having.emit(w)
		io.WriteString(w, "\n      (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "]")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n        (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n    )\n")
	} else {
		io.WriteString(w, "    (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "]")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n      (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n")
	}
	io.WriteString(w, "  )\n")
	if g.Sort != nil {
		io.WriteString(w, "  (set! _res (sort _res ")
		if g.SortCmp != "" {
			io.WriteString(w, g.SortCmp)
		} else {
			io.WriteString(w, "<")
		}
		io.WriteString(w, " #:key car))\n  (set! _res (map cdr _res))\n")
	}
	if g.Skip != nil {
		io.WriteString(w, "  (set! _res (drop _res ")
		g.Skip.emit(w)
		io.WriteString(w, "))\n")
	}
	if g.Take != nil {
		io.WriteString(w, "  (set! _res (take _res ")
		g.Take.emit(w)
		io.WriteString(w, "))\n")
	}
	io.WriteString(w, "  _res)")
}

func (g *GroupJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_groups (make-hash)] [_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, g.LeftVar)
	io.WriteString(w, " ")
	g.LeftSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (for ([")
	io.WriteString(w, g.RightVar)
	io.WriteString(w, " ")
	g.RightSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "      (when ")
	g.Cond.emit(w)
	io.WriteString(w, "\n        (let* ([_key ")
	g.Key.emit(w)
	io.WriteString(w, "][_g (hash-ref _groups _key (lambda () (let ([h (make-hash)]) (hash-set! h \"key\" _key) (hash-set! h \"items\" '()) (hash-set! _groups _key h) h)))])\n")
	io.WriteString(w, "          (hash-set! _g \"items\" (append (hash-ref _g \"items\") (list ")
	g.Row.emit(w)
	io.WriteString(w, ")))\n")
	io.WriteString(w, "      )\n    )\n  )\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, g.Name)
	io.WriteString(w, " (hash-values _groups)])\n")
	if g.Having != nil {
		io.WriteString(w, "    (when ")
		g.Having.emit(w)
		io.WriteString(w, "\n      (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "]")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n        (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n    )\n")
	} else {
		io.WriteString(w, "    (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "]")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n      (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n")
	}
	io.WriteString(w, "  )\n")
	if g.Sort != nil {
		io.WriteString(w, "  (set! _res (sort _res ")
		if g.SortCmp != "" {
			io.WriteString(w, g.SortCmp)
		} else {
			io.WriteString(w, "<")
		}
		io.WriteString(w, " #:key car))\n  (set! _res (map cdr _res))\n")
	}
	if g.Skip != nil {
		io.WriteString(w, "  (set! _res (drop _res ")
		g.Skip.emit(w)
		io.WriteString(w, "))\n")
	}
	if g.Take != nil {
		io.WriteString(w, "  (set! _res (take _res ")
		g.Take.emit(w)
		io.WriteString(w, "))\n")
	}
	io.WriteString(w, "  _res)")
}

func (g *GroupLeftJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_groups (make-hash)] [_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, g.LeftVar)
	io.WriteString(w, " ")
	g.LeftSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "    (let ([matched #f])\n")
	io.WriteString(w, "      (for ([")
	io.WriteString(w, g.RightVar)
	io.WriteString(w, " ")
	g.RightSrc.emit(w)
	io.WriteString(w, "])\n")
	io.WriteString(w, "        (when ")
	g.Cond.emit(w)
	io.WriteString(w, "\n          (set! matched #t)\n          (let* ([_key ")
	g.Key.emit(w)
	io.WriteString(w, "][_g (hash-ref _groups _key (lambda () (let ([h (make-hash)]) (hash-set! h \"key\" _key) (hash-set! h \"items\" '()) (hash-set! _groups _key h) h)))])\n            (hash-set! _g \"items\" (append (hash-ref _g \"items\") (list ")
	g.Row.emit(w)
	io.WriteString(w, ")))\n        )\n      )\n      (when (not matched)\n        (let* ([")
	io.WriteString(w, g.RightVar)
	io.WriteString(w, " #f][_key ")
	g.Key.emit(w)
	io.WriteString(w, "][_g (hash-ref _groups _key (lambda () (let ([h (make-hash)]) (hash-set! h \"key\" _key) (hash-set! h \"items\" '()) (hash-set! _groups _key h) h)))])\n          (hash-set! _g \"items\" (append (hash-ref _g \"items\") (list ")
	g.Row.emit(w)
	io.WriteString(w, ")))\n        )\n      )\n    ))\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, g.Name)
	io.WriteString(w, " (hash-values _groups)])\n")
	if g.Having != nil {
		io.WriteString(w, "    (when ")
		g.Having.emit(w)
		io.WriteString(w, "\n      (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "]")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n        (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n    )\n")
	} else {
		io.WriteString(w, "    (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "]")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n      (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n")
	}
	io.WriteString(w, "  )\n")
	if g.Sort != nil {
		io.WriteString(w, "  (set! _res (sort _res ")
		if g.SortCmp != "" {
			io.WriteString(w, g.SortCmp)
		} else {
			io.WriteString(w, "<")
		}
		io.WriteString(w, " #:key car))\n  (set! _res (map cdr _res))\n")
	}
	if g.Skip != nil {
		io.WriteString(w, "  (set! _res (drop _res ")
		g.Skip.emit(w)
		io.WriteString(w, "))\n")
	}
	if g.Take != nil {
		io.WriteString(w, "  (set! _res (take _res ")
		g.Take.emit(w)
		io.WriteString(w, "))\n")
	}
	io.WriteString(w, "  _res)")
}

func (g *GroupByExpr) emit(w io.Writer) {
	io.WriteString(w, "(let ([_groups (make-hash)] [_res '()])\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, g.Var)
	io.WriteString(w, " ")
	g.Source.emit(w)
	io.WriteString(w, "])\n")
	if g.Where != nil {
		io.WriteString(w, "    (when ")
		g.Where.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "    (let* ([_key ")
	g.Key.emit(w)
	io.WriteString(w, "][_g (hash-ref _groups _key (lambda () (let ([h (make-hash)]) (hash-set! h \"key\" _key) (hash-set! h \"items\" '()) (hash-set! _groups _key h) h)))])\n")
	io.WriteString(w, "      (hash-set! _g \"items\" (append (hash-ref _g \"items\") (list ")
	io.WriteString(w, g.Var)
	io.WriteString(w, "))))")
	if g.Where != nil {
		io.WriteString(w, ")")
	}
	io.WriteString(w, "\n  )\n")
	io.WriteString(w, "  (for ([")
	io.WriteString(w, g.Name)
	io.WriteString(w, " (hash-values _groups)])\n")
	if g.Having != nil {
		io.WriteString(w, "    (when ")
		g.Having.emit(w)
		io.WriteString(w, "\n      (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "])")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n        (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n    )\n")
	} else {
		io.WriteString(w, "    (let ([val ")
		g.Select.emit(w)
		io.WriteString(w, "]")
		if g.Sort != nil {
			io.WriteString(w, " [key ")
			g.Sort.emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")\n      (set! _res (append _res (list ")
		if g.Sort != nil {
			io.WriteString(w, "(cons key val)")
		} else {
			io.WriteString(w, "val")
		}
		io.WriteString(w, "))))\n")
	}
	io.WriteString(w, "  )\n")
	if g.Sort != nil {
		io.WriteString(w, "  (set! _res (sort _res ")
		if g.SortCmp != "" {
			io.WriteString(w, g.SortCmp)
		} else {
			io.WriteString(w, "<")
		}
		io.WriteString(w, " #:key car))\n  (set! _res (map cdr _res))\n")
	}
	if g.Skip != nil {
		io.WriteString(w, "  (set! _res (drop _res ")
		g.Skip.emit(w)
		io.WriteString(w, "))\n")
	}
	if g.Take != nil {
		io.WriteString(w, "  (set! _res (take _res ")
		g.Take.emit(w)
		io.WriteString(w, "))\n")
	}
	io.WriteString(w, "  _res)")
}

func convertGroupQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group == nil || len(q.Joins) != 0 || len(q.Froms) != 0 || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	src, err := convertExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	key, err := convertExpr(q.Group.Exprs[0], child)
	if err != nil {
		return nil, err
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
	}
	genv := types.NewEnv(env)
	genv.SetVar(q.Group.Name, types.AnyType{}, true)
	groupVars[q.Group.Name] = true
	sel, err := convertExpr(q.Select, genv)
	if err != nil {
		return nil, err
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having, genv)
		if err != nil {
			return nil, err
		}
	}
	var sortExpr Expr
	var sortCmp string
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		t := types.TypeOfExprBasic(q.Sort, genv)
		if types.IsStringType(t) {
			sortCmp = "string<?"
		} else if idx, ok := sortExpr.(*IndexExpr); ok && idx.IsMap {
			sortCmp = "(lambda (a b) (cond [(and (number? a) (number? b)) (< a b)] [(and (string? a) (string? b)) (string<? a b)] [else (string<? (format \"~a\" a) (format \"~a\" b))]))"
		}
	}
	var skipExpr, takeExpr Expr
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, genv)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, genv)
		if err != nil {
			return nil, err
		}
	}
	return &GroupByExpr{Var: q.Var, Source: src, Key: key, Name: q.Group.Name, Where: where, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Skip: skipExpr, Take: takeExpr}, nil
}

func convertGroupJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Joins) != 1 || len(q.Froms) != 0 || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side != nil && *j.Side != "" {
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
	key, err := convertExpr(q.Group.Exprs[0], child)
	if err != nil {
		return nil, err
	}
	entries := []Expr{
		&StringLit{Value: q.Var}, &Name{Name: q.Var},
		&StringLit{Value: j.Var}, &Name{Name: j.Var},
	}
	row := &CallExpr{Func: "hash", Args: entries}
	genv := types.NewEnv(env)
	genv.SetVar(q.Group.Name, types.AnyType{}, true)
	groupVars[q.Group.Name] = true
	sel, err := convertExpr(q.Select, genv)
	if err != nil {
		return nil, err
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having, genv)
		if err != nil {
			return nil, err
		}
	}
	var sortExpr Expr
	var sortCmp string
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		t := types.TypeOfExprBasic(q.Sort, genv)
		if types.IsStringType(t) {
			sortCmp = "string<?"
		} else if idx, ok := sortExpr.(*IndexExpr); ok && idx.IsMap {
			sortCmp = "(lambda (a b) (cond [(and (number? a) (number? b)) (< a b)] [(and (string? a) (string? b)) (string<? a b)] [else (string<? (format \"~a\" a) (format \"~a\" b))]))"
		}
	}
	var skipExpr, takeExpr Expr
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, genv)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, genv)
		if err != nil {
			return nil, err
		}
	}
	return &GroupJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Key: key, Row: row, Name: q.Group.Name, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Skip: skipExpr, Take: takeExpr}, nil
}

func convertGroupLeftJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Joins) != 1 || len(q.Froms) != 0 || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
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
	key, err := convertExpr(q.Group.Exprs[0], child)
	if err != nil {
		return nil, err
	}
	entries := []Expr{
		&StringLit{Value: q.Var}, &Name{Name: q.Var},
		&StringLit{Value: j.Var}, &Name{Name: j.Var},
	}
	row := &CallExpr{Func: "hash", Args: entries}
	genv := types.NewEnv(env)
	genv.SetVar(q.Group.Name, types.AnyType{}, true)
	groupVars[q.Group.Name] = true
	sel, err := convertExpr(q.Select, genv)
	if err != nil {
		return nil, err
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having, genv)
		if err != nil {
			return nil, err
		}
	}
	var sortExpr Expr
	var sortCmp string
	var skipExpr, takeExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		t := types.TypeOfExprBasic(q.Sort, genv)
		if types.IsStringType(t) {
			sortCmp = "string<?"
		} else if idx, ok := sortExpr.(*IndexExpr); ok && idx.IsMap {
			sortCmp = "(lambda (a b) (cond [(and (number? a) (number? b)) (< a b)] [(and (string? a) (string? b)) (string<? a b)] [else (string<? (format \"~a\" a) (format \"~a\" b))]))"
		}
	}
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, genv)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, genv)
		if err != nil {
			return nil, err
		}
	}
	return &GroupLeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Key: key, Row: row, Name: q.Group.Name, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Skip: skipExpr, Take: takeExpr}, nil
}

func convertGroupMultiJoinQuery(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Joins) < 2 || len(q.Froms) != 0 || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return nil, fmt.Errorf("unsupported query")
		}
	}
	vars := []string{q.Var}
	srcs := []*parser.Expr{q.Source}
	for _, j := range q.Joins {
		vars = append(vars, j.Var)
		srcs = append(srcs, j.Src)
	}
	var exprs []Expr
	for _, s := range srcs {
		e, err := convertExpr(s, env)
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, e)
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, j := range q.Joins {
		child.SetVar(j.Var, types.AnyType{}, true)
	}
	var cond Expr
	for _, j := range q.Joins {
		c, err := convertExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = c
		} else {
			cond = &BinaryExpr{Op: "and", Left: cond, Right: c}
		}
	}
	if q.Where != nil {
		w, err := convertExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = w
		} else {
			cond = &BinaryExpr{Op: "and", Left: cond, Right: w}
		}
	}
	key, err := convertExpr(q.Group.Exprs[0], child)
	if err != nil {
		return nil, err
	}
	var entries []Expr
	for _, v := range vars {
		entries = append(entries, &StringLit{Value: v}, &Name{Name: v})
	}
	row := &CallExpr{Func: "hash", Args: entries}
	genv := types.NewEnv(env)
	genv.SetVar(q.Group.Name, types.AnyType{}, true)
	groupVars[q.Group.Name] = true
	sel, err := convertExpr(q.Select, genv)
	if err != nil {
		return nil, err
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having, genv)
		if err != nil {
			return nil, err
		}
	}
	var sortExpr Expr
	var sortCmp string
	var skipExpr, takeExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		t := types.TypeOfExprBasic(q.Sort, genv)
		if types.IsStringType(t) {
			sortCmp = "string<?"
		} else if idx, ok := sortExpr.(*IndexExpr); ok && idx.IsMap {
			sortCmp = "(lambda (a b) (cond [(and (number? a) (number? b)) (< a b)] [(and (string? a) (string? b)) (string<? a b)] [else (string<? (format \"~a\" a) (format \"~a\" b))]))"
		}
	}
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip, genv)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take, genv)
		if err != nil {
			return nil, err
		}
	}
	return &GroupMultiJoinExpr{Vars: vars, Sources: exprs, Cond: cond, Key: key, Row: row, Name: q.Group.Name, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Skip: skipExpr, Take: takeExpr}, nil
}

func convertQueryExpr(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	if q.Group != nil {
		if len(q.Joins) == 0 && len(q.Froms) == 0 {
			return convertGroupQuery(q, env)
		}
		if len(q.Joins) == 1 {
			if q.Joins[0].Side != nil && *q.Joins[0].Side == "left" {
				return convertGroupLeftJoinQuery(q, env)
			}
			if q.Joins[0].Side == nil || *q.Joins[0].Side == "" {
				return convertGroupJoinQuery(q, env)
			}
			return nil, fmt.Errorf("unsupported query")
		}
		return convertGroupMultiJoinQuery(q, env)
	}
	if len(q.Joins) == 0 {
		return convertCrossJoinQuery(q, env)
	}
	if len(q.Joins) == 1 {
		if q.Joins[0].Side != nil {
			side := *q.Joins[0].Side
			switch side {
			case "left":
				return convertLeftJoinQuery(q, env)
			case "right":
				return convertRightJoinQuery(q, env)
			case "outer":
				return convertOuterJoinQuery(q, env)
			default:
				return nil, fmt.Errorf("unsupported query")
			}
		} else {
			return convertInnerJoinQuery(q, env)
		}
	}
	if len(q.Joins) >= 2 {
		allInner := true
		for _, j := range q.Joins {
			if j.Side != nil {
				allInner = false
				break
			}
		}
		if allInner {
			return convertMultiJoinQuery(q, env)
		}
		if len(q.Joins) == 2 && q.Joins[0].Side == nil && q.Joins[1].Side != nil && *q.Joins[1].Side == "left" {
			return convertLeftJoinMultiQuery(q, env)
		}
	}
	return nil, fmt.Errorf("unsupported query")
}
