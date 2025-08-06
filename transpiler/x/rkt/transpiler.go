//go:build slow

package rkt

import (
	"bufio"
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

	yaml "gopkg.in/yaml.v3"

	"mochi/parser"
	"mochi/types"
)

var importedModules map[string]string

// Deprecated: group structs are no longer emitted. The flag is kept for
// backward compatibility but has no effect.
var needsGroupStruct bool
var groupVars = map[string]bool{}

// benchMain controls whether the generated program is wrapped in a benchmark
// block when emitting code. When enabled the resulting output is a JSON object
// containing duration and memory statistics.
var benchMain bool

var continueLabels []string

// funcNameMap tracks sanitized names for user defined functions so that
// subsequent calls can use the sanitized version rather than clashing with
// Racket builtins such as `list` or `length`.
var funcNameMap = map[string]string{}

func pushContinue(label string) {
	continueLabels = append(continueLabels, label)
}

func popContinue() {
	if len(continueLabels) > 0 {
		continueLabels = continueLabels[:len(continueLabels)-1]
	}
}

func currentContinue() string {
	if len(continueLabels) == 0 {
		return ""
	}
	return continueLabels[len(continueLabels)-1]
}

// SetBenchMain configures benchmark wrapping for the main program.
func SetBenchMain(v bool) { benchMain = v }

func sanitizeName(name string) string {
	switch name {
	case "list":
		return "list_"
	case "length":
		return "length_"
	case "format":
		return "format_"
	case "lower":
		return "lower_"
	case "upper":
		return "upper_"
	case "slice":
		return "slice_"
	case "quotient":
		return "quotient_"
	case "panic":
		return "panic_"
	default:
		return name
	}
}

type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type StmtList []Stmt

func (s StmtList) emit(w io.Writer) {
	for _, st := range s {
		if st != nil {
			st.emit(w)
		}
	}
}

type Expr interface{ emit(io.Writer) }

// LetExpr represents a "let" expression with local bindings.
type LetExpr struct {
	Binds []LetBind
	Body  Expr
}

// LetBind holds a single variable binding for LetExpr.
type LetBind struct {
	Name string
	Expr Expr
}

func (l *LetExpr) emit(w io.Writer) {
	io.WriteString(w, "(let (")
	for i, b := range l.Binds {
		if i > 0 {
			io.WriteString(w, " ")
		}
		io.WriteString(w, "[")
		io.WriteString(w, sanitizeName(b.Name))
		io.WriteString(w, " ")
		b.Expr.emit(w)
		io.WriteString(w, "]")
	}
	io.WriteString(w, ") ")
	if l.Body != nil {
		l.Body.emit(w)
	} else {
		io.WriteString(w, "(void)")
	}
	io.WriteString(w, ")")
}

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
	io.WriteString(w, "(displayln (string-join (map (lambda (x) (format \"~a\" x)) (list")
	for _, part := range p.Parts {
		io.WriteString(w, " ")
		part.emit(w)
	}
	io.WriteString(w, ")) \" \"))\n")
}

type LetStmt struct {
	Name string
	Expr Expr
	Type types.Type
}

func (l *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(define %s ", sanitizeName(l.Name))
	l.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (a *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "(set! %s ", sanitizeName(a.Name))
	a.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type ListAssignStmt struct {
	Name  string
	Index Expr
	Expr  Expr
}

func (l *ListAssignStmt) emit(w io.Writer) {
	name := sanitizeName(l.Name)
	fmt.Fprintf(w, "(set! %s (list-set %s (int ", name, name)
	l.Index.emit(w)
	io.WriteString(w, ") ")
	l.Expr.emit(w)
	io.WriteString(w, "))\n")
}

type MapAssignStmt struct {
	Name string
	Key  Expr
	Expr Expr
}

func (m *MapAssignStmt) emit(w io.Writer) {
	name := sanitizeName(m.Name)
	fmt.Fprintf(w, "(set! %s (hash-set (or %s (hash)) ", name, name)
	m.Key.emit(w)
	io.WriteString(w, " ")
	m.Expr.emit(w)
	io.WriteString(w, "))\n")
}

type DoubleListAssignStmt struct {
	Name   string
	Index1 Expr
	Index2 Expr
	Expr   Expr
}

type DoubleMapAssignStmt struct {
	Name string
	Key1 Expr
	Key2 Expr
	Expr Expr
}

type ListMapAssignStmt struct {
	Name  string
	Index Expr
	Key   Expr
	Expr  Expr
}

func (d *DoubleListAssignStmt) emit(w io.Writer) {
	name := sanitizeName(d.Name)
	fmt.Fprintf(w, "(set! %s (list-set %s ", name, name)
	d.Index1.emit(w)
	io.WriteString(w, " (list-set (list-ref ")
	io.WriteString(w, name)
	io.WriteString(w, " ")
	d.Index1.emit(w)
	io.WriteString(w, ") ")
	d.Index2.emit(w)
	io.WriteString(w, " ")
	d.Expr.emit(w)
	io.WriteString(w, ")))\n")
}

func (d *DoubleMapAssignStmt) emit(w io.Writer) {
	name := sanitizeName(d.Name)
	fmt.Fprintf(w, "(set! %s (hash-set %s ", name, name)
	d.Key1.emit(w)
	io.WriteString(w, " (hash-set (hash-ref ")
	io.WriteString(w, name)
	io.WriteString(w, " ")
	d.Key1.emit(w)
	io.WriteString(w, ") ")
	d.Key2.emit(w)
	io.WriteString(w, " ")
	d.Expr.emit(w)
	io.WriteString(w, ")))\n")
}

func (l *ListMapAssignStmt) emit(w io.Writer) {
	name := sanitizeName(l.Name)
	fmt.Fprintf(w, "(set! %s (list-set %s ", name, name)
	l.Index.emit(w)
	io.WriteString(w, " (hash-set (list-ref ")
	io.WriteString(w, name)
	io.WriteString(w, " ")
	l.Index.emit(w)
	io.WriteString(w, ") ")
	l.Key.emit(w)
	io.WriteString(w, " ")
	l.Expr.emit(w)
	io.WriteString(w, ")))\n")
}

type ExprStmt struct{ Expr Expr }

func (e *ExprStmt) emit(w io.Writer) {
	e.Expr.emit(w)
	io.WriteString(w, "\n")
}

type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "(for ([_row ")
		if s.Src != nil {
			s.Src.emit(w)
		} else {
			io.WriteString(w, "'()")
		}
		io.WriteString(w, "]) (displayln (jsexpr->string _row)))\n")
		return
	}
	io.WriteString(w, ";; unsupported save\n")
}

type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(w io.Writer) {
	// Emit global declarations first so that benchmarked body can invoke them.
	var body []Stmt
	for _, st := range b.Body {
		switch st.(type) {
		case *FunDecl, StmtList, *LetStmt:
			st.emit(w)
		default:
			body = append(body, st)
		}
	}
	fmt.Fprintln(w, "(let* ([_start_mem (current-memory-use)] [_start (now)])")
	fmt.Fprintln(w, "  (let/ec _return (begin")
	for _, st := range body {
		st.emit(w)
	}
	fmt.Fprintln(w, "    (void)")
	fmt.Fprintln(w, "  ))")
	fmt.Fprintln(w, "  (let* ([_end (now)] [_end_mem (current-memory-use)]")
	fmt.Fprintln(w, "         [_dur (- _end _start)]")
	fmt.Fprintln(w, "         [_dur_us _dur]")
	fmt.Fprintln(w, "         [_mem (max 0 (- _end_mem _start_mem))])")
	io.WriteString(w, "    (displayln \"{\")\n")
	io.WriteString(w, "    (displayln (format \"  \\\"duration_us\\\": ~a,\" _dur_us))\n")
	io.WriteString(w, "    (displayln (format \"  \\\"memory_bytes\\\": ~a,\" _mem))\n")
	fmt.Fprintf(w, "    (displayln \"  \\\"name\\\": \\\"%s\\\"\")\n", b.Name)
	io.WriteString(w, "    (displayln \"}\")\n")
	io.WriteString(w, "  )\n")
	io.WriteString(w, ")\n")
}

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "(if ")
	i.Cond.emit(w)
	if len(i.Then) > 0 {
		io.WriteString(w, " (let ()\n")
		for _, s := range i.Then {
			s.emit(w)
		}
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, " (void)")
	}
	if len(i.Else) > 0 {
		io.WriteString(w, " (let ()\n")
		for _, s := range i.Else {
			s.emit(w)
		}
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, " (void)")
	}
	io.WriteString(w, ")\n")
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "(let/ec _break (let loop ()\n  (if ")
	wst.Cond.emit(w)
	io.WriteString(w, " (let/ec _cont\n")
	pushContinue("_cont")
	for _, st := range wst.Body {
		io.WriteString(w, "    ")
		st.emit(w)
	}
	popContinue()
	io.WriteString(w, "    (loop)) (void))))\n")
}

type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "(_break)\n") }

type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) {
	label := currentContinue()
	if label == "" {
		io.WriteString(w, "(void)\n")
		return
	}
	io.WriteString(w, "(")
	io.WriteString(w, label)
	io.WriteString(w, ")\n")
}

type RangeContinueStmt struct{ Var string }

func (c *RangeContinueStmt) emit(w io.Writer) {
	io.WriteString(w, "(begin (set! ")
	io.WriteString(w, c.Var)
	io.WriteString(w, " (+ ")
	io.WriteString(w, c.Var)
	io.WriteString(w, " 1)) (")
	io.WriteString(w, currentContinue())
	io.WriteString(w, "))\n")
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
	io.WriteString(w, "(let/ec _break (let (")
	io.WriteString(w, "[")
	io.WriteString(w, sanitizeName(fr.Name))
	io.WriteString(w, " ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, "])\n  (let loop ()\n    (when (< ")
	io.WriteString(w, sanitizeName(fr.Name))
	io.WriteString(w, " ")
	fr.End.emit(w)
	io.WriteString(w, ")\n")
	pushContinue("loop")
	for _, st := range fr.Body {
		st.emit(w)
	}
	popContinue()
	io.WriteString(w, "      (set! ")
	io.WriteString(w, sanitizeName(fr.Name))
	io.WriteString(w, " (+ ")
	io.WriteString(w, sanitizeName(fr.Name))
	io.WriteString(w, " 1))\n      (loop)))\n))\n")
}

type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	Keys     bool
	String   bool
	Break    Expr
	Unless   Expr
}

func (f *ForInStmt) emit(w io.Writer) {
	if f.String {
		io.WriteString(w, " (let/ec _break (for ([__i (in-range (string-length ")
		f.Iterable.emit(w)
		io.WriteString(w, "))])")
		if f.Break != nil {
			io.WriteString(w, " #:break ")
			f.Break.emit(w)
		}
		if f.Unless != nil {
			io.WriteString(w, " #:unless ")
			f.Unless.emit(w)
		}
		io.WriteString(w, "\n  (define ")
		io.WriteString(w, sanitizeName(f.Name))
		io.WriteString(w, " (substring ")
		f.Iterable.emit(w)
		io.WriteString(w, " __i (+ __i 1)))\n  (let/ec _cont\n")
		pushContinue("_cont")
		for _, st := range f.Body {
			if _, ok := st.(*ContinueStmt); ok {
				io.WriteString(w, "    (_cont)\n")
			} else {
				st.emit(w)
			}
		}
		popContinue()
		io.WriteString(w, "  )))\n")
		return
	}
	io.WriteString(w, "(let/ec _break (for ([")
	io.WriteString(w, sanitizeName(f.Name))
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
	io.WriteString(w, ")\n  (let/ec _cont\n")
	pushContinue("_cont")
	for _, st := range f.Body {
		if _, ok := st.(*ContinueStmt); ok {
			io.WriteString(w, "    (_cont)\n")
		} else {
			st.emit(w)
		}
	}
	popContinue()
	io.WriteString(w, "  )))\n")
}

type ReturnStmt struct{ Expr Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "(_return ")
	r.Expr.emit(w)
	io.WriteString(w, ")\n")
}

type FunDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FunDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "(define (%s", f.Name)
	for _, p := range f.Params {
		fmt.Fprintf(w, " %s", sanitizeName(p))
	}
	io.WriteString(w, ")\n  (let/ec _return (begin\n")
	if len(f.Body) == 0 {
		io.WriteString(w, "    (void)\n")
	} else {
		for _, st := range f.Body {
			st.emit(w)
		}
	}
	io.WriteString(w, "))\n")
	io.WriteString(w, ")\n")
}

type LambdaExpr struct {
	Params []string
	Body   []Stmt
	Expr   Expr
}

func (l *LambdaExpr) emit(w io.Writer) {
	io.WriteString(w, "(lambda (")
	for i, p := range l.Params {
		if i > 0 {
			io.WriteString(w, " ")
		}
		io.WriteString(w, sanitizeName(p))
	}
	io.WriteString(w, ")")
	if len(l.Body) > 0 {
		io.WriteString(w, "\n  (let/ec _return (begin\n")
		for _, st := range l.Body {
			st.emit(w)
		}
		io.WriteString(w, ")))")
	} else if l.Expr != nil {
		io.WriteString(w, " ")
		l.Expr.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, ")")
	}
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

func (n *Name) emit(w io.Writer) {
	if n.Name == "list" {
		io.WriteString(w, "list_")
	} else {
		io.WriteString(w, n.Name)
	}
}

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) {
	if f.Value == float64(int(f.Value)) {
		fmt.Fprintf(w, "%.1f", f.Value)
	} else {
		fmt.Fprintf(w, "%g", f.Value)
	}
}

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

// InvokeExpr calls the result of evaluating an expression as a function.
type InvokeExpr struct {
	Callee Expr
	Args   []Expr
}

type IndexExpr struct {
	Target   Expr
	Index    Expr
	IsString bool
	IsMap    bool
}

type SliceExpr struct {
	Target   Expr
	Start    Expr
	End      Expr
	IsString bool
}

type MatchCase struct {
	Pattern Expr
	Expr    Expr
	Default bool
}

type MatchExpr struct {
	Value Expr
	Cases []MatchCase
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.IsString {
		io.WriteString(w, "(substring ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, " (+ ")
		i.Index.emit(w)
		io.WriteString(w, " 1))")
	} else if i.IsMap {
		io.WriteString(w, "(if ")
		i.Target.emit(w)
		io.WriteString(w, " (hash-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, " #f) #f)")
	} else {
		io.WriteString(w, "(if ")
		i.Target.emit(w)
		io.WriteString(w, " (if (hash? ")
		i.Target.emit(w)
		io.WriteString(w, ") (hash-ref ")
		i.Target.emit(w)
		io.WriteString(w, " ")
		i.Index.emit(w)
		io.WriteString(w, " #f) (list-ref ")
		i.Target.emit(w)
		io.WriteString(w, " (int ")
		i.Index.emit(w)
		io.WriteString(w, "))) #f)")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	io.WriteString(w, "(slice ")
	s.Target.emit(w)
	io.WriteString(w, " ")
	if s.Start != nil {
		s.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, " ")
	if s.End != nil {
		s.End.emit(w)
	} else {
		if s.IsString {
			io.WriteString(w, "(string-length ")
		} else {
			io.WriteString(w, "(length ")
		}
		s.Target.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")")
}

func (m *MatchExpr) emit(w io.Writer) {
	io.WriteString(w, "(match ")
	m.Value.emit(w)
	io.WriteString(w, "\n")
	for _, c := range m.Cases {
		io.WriteString(w, "  [")
		if c.Default {
			io.WriteString(w, "_")
		} else {
			c.Pattern.emit(w)
		}
		io.WriteString(w, " ")
		c.Expr.emit(w)
		io.WriteString(w, "]\n")
	}
	io.WriteString(w, ")")
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

func (i *InvokeExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	i.Callee.emit(w)
	for _, a := range i.Args {
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
		io.WriteString(w, "(int ")
		c.Value.emit(w)
		io.WriteString(w, ")")
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

func literalIntUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	return literalIntPostfix(u.Value)
}

func literalIntPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 || p.Target == nil {
		return false
	}
	return p.Target.Lit != nil && p.Target.Lit.Int != nil
}

func exprIsInt(e Expr, env *types.Env) bool {
	switch v := e.(type) {
	case *IntLit:
		return true
	case *Name:
		if env != nil {
			if t, err := env.GetVar(v.Name); err == nil {
				if _, ok := t.(types.IntType); ok {
					return true
				}
				if _, ok := t.(types.Int64Type); ok {
					return true
				}
				if _, ok := t.(types.BigIntType); ok {
					return true
				}
			}
		}
	case *BinaryExpr:
		switch v.Op {
		case "quotient", "modulo", "+", "-", "*":
			return exprIsInt(v.Left, env) && exprIsInt(v.Right, env)
		}
	}
	return false
}

func isBoolExpr(e Expr) bool {
	if n, ok := e.(*Name); ok {
		return n.Name == "#t" || n.Name == "#f"
	}
	return false
}

// heuristicInt returns true if the expression appears to represent an
// integer value using best-effort heuristics. Unlike exprIsInt it may
// inspect the provided environment for variable types when available so
// that names with float types are not mistaken for integers.
func heuristicInt(e Expr, env *types.Env) bool {
	switch v := e.(type) {
	case *IntLit:
		return true
	case *Name:
		if env != nil {
			if t, err := env.GetVar(v.Name); err == nil {
				if types.IsIntType(t) || types.IsInt64Type(t) || types.IsBigIntType(t) {
					return true
				}
				return false
			}
		}
		return false
	case *BinaryExpr:
		switch v.Op {
		case "quotient", "modulo", "+", "-", "*":
			return heuristicInt(v.Left, env) && heuristicInt(v.Right, env)
		}
	}
	return false
}

// plusOne checks if e represents start + 1 where start is the same expression
// as the provided start parameter. It handles simple cases produced by the
// parser for slice boundaries.
func plusOne(e Expr, start Expr) bool {
	b, ok := e.(*BinaryExpr)
	if !ok || b.Op != "+" {
		return false
	}
	if l, ok := b.Left.(*IntLit); ok {
		if l.Value == 1 {
			if r, ok2 := b.Right.(*Name); ok2 {
				if s, ok3 := start.(*Name); ok3 && r.Name == s.Name {
					return true
				}
			}
			if r, ok2 := b.Right.(*IntLit); ok2 {
				if s, ok3 := start.(*IntLit); ok3 && r.Value == s.Value {
					return true
				}
			}
		}
	}
	if r, ok := b.Right.(*IntLit); ok {
		if r.Value == 1 {
			if l, ok2 := b.Left.(*Name); ok2 {
				if s, ok3 := start.(*Name); ok3 && l.Name == s.Name {
					return true
				}
			}
			if l, ok2 := b.Left.(*IntLit); ok2 {
				if s, ok3 := start.(*IntLit); ok3 && l.Value == s.Value {
					return true
				}
			}
		}
	}
	return false
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
		io.WriteString(w, "(not (equal? ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, "))")
	case "==":
		io.WriteString(w, "(equal? ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	case "string!=":
		io.WriteString(w, "(not (string=? ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, "))")
	case "string=?":
		io.WriteString(w, "(string=? ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	case "+":
		io.WriteString(w, "(let ([__l ")
		b.Left.emit(w)
		io.WriteString(w, "] [__r ")
		b.Right.emit(w)
		io.WriteString(w, "]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))")
	case "list-append":
		io.WriteString(w, "(append ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")")
	case "fmod":
		io.WriteString(w, "(- ")
		b.Left.emit(w)
		io.WriteString(w, " (* (floor (/ ")
		b.Left.emit(w)
		io.WriteString(w, " ")
		b.Right.emit(w)
		io.WriteString(w, ")) ")
		b.Right.emit(w)
		io.WriteString(w, "))")
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
	hdr := fmt.Sprintf(";; Generated by Mochi %s on %s\n#lang racket/base\n(require racket/list racket/string racket/math racket/match racket/system racket/port net/url net/http-client json openssl/sha1 openssl/md5)\n", version(), gitTime())
	hdr += "(define nowSeed (let ([s (getenv \"MOCHI_NOW_SEED\")]) (and s (string->number s))))\n"
	hdr += "(define (now)\n  (if nowSeed\n      (begin (set! nowSeed (modulo (+ (* nowSeed 1664525) 1013904223) 2147483647)) nowSeed)\n      (inexact->exact (floor (* (current-inexact-milliseconds) 1000)))))\n"
	hdr += "(define (int x)\n  (cond\n    [(number? x) (inexact->exact (truncate x))]\n    [(string? x) (let ([n (string->number x)]) (if n (inexact->exact (truncate n)) 0))]\n    [else 0]))\n"
	hdr += "(define (float x)\n  (cond\n    [(number? x) (exact->inexact x)]\n    [(string? x) (let ([n (string->number x)]) (if n (exact->inexact n) 0.0))]\n    [else 0.0]))\n"
	hdr += "(define (input) (let ([ln (read-line)]) (if (eof-object? ln) \"\" ln)))\n"
	hdr += "(define (upper s) (string-upcase s))\n"
	hdr += "(define (lower s) (string-downcase s))\n"
	hdr += "(define (sublist lst start end)\n  (if (string? lst)\n      (substring lst start end)\n      (take (drop lst start) (- end start))))\n\n"
	hdr += "(define (slice seq start end)\n  (define len (if (string? seq) (string-length seq) (length seq)))\n  (define s (int start))\n  (define e (int end))\n  (when (< s 0) (set! s (+ len s)))\n  (when (< e 0) (set! e (+ len e)))\n  (set! s (max 0 (min len s)))\n  (set! e (max 0 (min len e)))\n  (when (< e s) (set! e s))\n  (if (string? seq) (substring seq s e) (sublist seq s e)))\n"
	hdr += "(define (pad-start s width ch)\n  (let ([s (format \"~a\" s)])\n    (if (< (string-length s) width)\n        (string-append (make-string (- width (string-length s)) (string-ref ch 0)) s)\n        s)))\n"
	hdr += "(define (index-of s ch)\n  (cond\n    [(string? s)\n     (let loop ([i 0])\n       (cond [(>= i (string-length s)) -1]\n             [(string=? (substring s i (add1 i)) ch) i]\n             [else (loop (add1 i))]))]\n    [else\n     (let loop ([i 0] [lst s])\n       (cond [(null? lst) -1]\n             [(equal? (car lst) ch) i]\n             [else (loop (add1 i) (cdr lst))]))]))\n"
	hdr += "(define (_repeat s n)\n  (cond\n    [(string? s) (apply string-append (make-list (int n) s))]\n    [(list? s) (apply append (make-list (int n) s))]\n    [else '()]))\n"
	hdr += "(define (_parse-int-str s base) (int (string->number s base)))\n"
	hdr += "(define (_sha256 bs) (bytes->list (sha256-bytes (list->bytes bs))))\n"
	hdr += "(define (_fetch url) (let ([p (get-pure-port (string->url url))]) (define txt (port->string p)) (close-input-port p) (string->jsexpr txt)))\n"
	hdr += "(define (num r) (numerator r))\n"
	hdr += "(define (denom r) (denominator r))\n"
	hdr += "(define (panic msg) (error msg))\n"
	hdr += "(define Object (hash \"keys\" (lambda (self m) (let ([lst (hash-keys m)]) (hash \"join\" (lambda (s sep) (string-join lst sep)))))))\n"
	// Provide a minimal stdout object with a write method so that programs
	// using `stdout.write` for output can run without additional setup.
	hdr += "(define stdout (hash \"write\" (lambda (s) (display s))))\n"
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

func zeroValue(t *parser.TypeRef, env *types.Env) Expr {
	if t == nil {
		return nil
	}
	if env == nil {
		env = types.NewEnv(nil)
	}
	typ := types.ResolveTypeRef(t, env)
	switch typ.(type) {
	case types.IntType, types.Int64Type:
		return &IntLit{Value: 0}
	case types.FloatType:
		return &FloatLit{Value: 0}
	case types.BoolType:
		return &Name{Name: "#f"}
	case types.StringType:
		return &StringLit{Value: ""}
	case types.ListType:
		return &ListLit{}
	case types.MapType, types.StructType:
		return &CallExpr{Func: "hash"}
	default:
		return nil
	}
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

func valueToExpr(v interface{}, typ *parser.TypeRef, env *types.Env) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		var args []Expr
		for _, k := range names {
			args = append(args, &StringLit{Value: k})
			args = append(args, valueToExpr(val[k], nil, env))
		}
		return &CallExpr{Func: "hash", Args: args}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it, nil, env)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		if val {
			return &Name{Name: "#t"}
		}
		return &Name{Name: "#f"}
	case float64:
		if val == float64(int(val)) {
			return &IntLit{Value: int(val)}
		}
		return &FloatLit{Value: val}
	case int:
		return &IntLit{Value: val}
	case int64:
		return &IntLit{Value: int(val)}
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
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
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

func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		s.emit(w)
	}
	return nil
}

func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	benchMain = bench
	r := &Program{}
	importedModules = map[string]string{}
	if env != nil {
		env.SetVar("input", types.FuncType{Return: types.StringType{}}, false)
	}
	// handle imports first
	for _, st := range prog.Statements {
		if st.Import == nil {
			continue
		}
		alias := st.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(st.Import.Path)
		}
		importedModules[alias] = st.Import.Path
		if st.Import.Lang != nil && *st.Import.Lang == "go" && st.Import.Path == "strings" {
			r.Stmts = append(r.Stmts,
				&RawStmt{Code: fmt.Sprintf("(define (%s_TrimSpace s) (string-trim s))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_ToUpper s) (string-upcase s))", alias)},
			)
			if env != nil {
				env.SetVar(alias+".TrimSpace", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}, Pure: true}, false)
				env.SetVar(alias+".ToUpper", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}, Pure: true}, false)
			}
		} else if st.Import.Lang != nil && *st.Import.Lang == "python" && st.Import.Path == "math" {
			r.Stmts = append(r.Stmts,
				&RawStmt{Code: fmt.Sprintf("(define %s_pi pi)", alias)},
				&RawStmt{Code: fmt.Sprintf("(define %s_e (exp 1))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_sqrt x) (sqrt x))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_pow x y) (expt x y))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_sin x) (sin x))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_log x) (log x))", alias)},
			)
			if env != nil {
				env.SetVar(alias+".pi", types.FloatType{}, false)
				env.SetVar(alias+".e", types.FloatType{}, false)
				env.SetVar(alias+".sqrt", types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}, Pure: true}, false)
				env.SetVar(alias+".pow", types.FuncType{Params: []types.Type{types.FloatType{}, types.FloatType{}}, Return: types.FloatType{}, Pure: true}, false)
				env.SetVar(alias+".sin", types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}, Pure: true}, false)
				env.SetVar(alias+".log", types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}, Pure: true}, false)
			}
		} else if st.Import.Lang != nil && *st.Import.Lang == "python" && st.Import.Path == "subprocess" {
			r.Stmts = append(r.Stmts,
				&RawStmt{Code: fmt.Sprintf("(define (%s_getoutput cmd) (with-output-to-string (lambda () (system cmd))))", alias)},
			)
			if env != nil {
				env.SetVar(alias+".getoutput", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}, Pure: false}, false)
			}
		} else if st.Import.Lang != nil && *st.Import.Lang == "go" && st.Import.Path == "net" {
			r.Stmts = append(r.Stmts,
				&RawStmt{Code: fmt.Sprintf("(define (%s_LookupHost host) (list '() #f))", alias)},
			)
			if env != nil {
				env.SetVar(alias+".LookupHost", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.ListType{Elem: types.AnyType{}}, Pure: false}, false)
			}
		} else if st.Import.Lang != nil && *st.Import.Lang == "go" && st.Import.Path == "os" {
			r.Stmts = append(r.Stmts,
				&RawStmt{Code: fmt.Sprintf("(define (%s_Getenv name) (or (getenv name) \"\"))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_Environ)", alias)},
				&RawStmt{Code: "  (for/list ([n (environment-variables-names (current-environment-variables))]) (let ([s (bytes->string/utf-8 n)]) (string-append s \"=\" (or (getenv s) \"\")))) )"},
			)
			if env != nil {
				env.SetVar(alias+".Getenv", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}, Pure: true}, false)
				env.SetVar(alias+".Environ", types.FuncType{Params: []types.Type{}, Return: types.ListType{Elem: types.StringType{}}, Pure: true}, false)
			}
		} else if st.Import.Auto && st.Import.Path == "mochi/runtime/ffi/go/testpkg" {
			if alias == "" {
				alias = "testpkg"
			}
			r.Stmts = append(r.Stmts,
				&RawStmt{Code: fmt.Sprintf("(define (%s_Add a b) (+ a b))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define %s_Pi 3.14)", alias)},
				&RawStmt{Code: fmt.Sprintf("(define %s_Answer 42)", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_FifteenPuzzleExample) \"Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd\")", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_ECDSAExample) (hash \"D\" \"1234567890\" \"X\" \"43162711582587979080031819627904423023685561091192625653251495188141318209988\" \"Y\" \"86807430002474105664458509423764867536342689150582922106807036347047552480521\" \"Hash\" \"0xe6f9ed0d\" \"R\" \"43162711582587979080031819627904423023685561091192625653251495188141318209988\" \"S\" \"94150071556658883365738746782965214584303361499725266605620843043083873122499\" \"Valid\" #t))", alias)},
				&RawStmt{Code: fmt.Sprintf("(define (%s_MD5Hex s) (let ([in (open-input-string (format \"~a\" s))]) (define b (md5-bytes in)) (close-input-port in) (bytes->hex-string b)))", alias)},
			)
			if env != nil {
				env.SetVar(alias+".Add", types.FuncType{Params: []types.Type{types.IntType{}, types.IntType{}}, Return: types.IntType{}, Pure: true}, false)
				env.SetVar(alias+".Pi", types.FloatType{}, false)
				env.SetVar(alias+".Answer", types.IntType{}, false)
				env.SetVar(alias+".FifteenPuzzleExample", types.FuncType{Params: []types.Type{}, Return: types.StringType{}, Pure: true}, false)
				env.SetVar(alias+".ECDSAExample", types.FuncType{Params: []types.Type{}, Return: types.MapType{Key: types.StringType{}, Value: types.AnyType{}}, Pure: true}, false)
				env.SetVar(alias+".MD5Hex", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}, Pure: true}, false)
			}
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
	if benchMain {
		r.Stmts = []Stmt{&BenchStmt{Name: "main", Body: r.Stmts}}
	}
	// no runtime helpers required
	_ = env
	return r, nil
}

func convertStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		} else {
			e = zeroValue(st.Let.Type, env)
		}
		var t types.Type
		if env != nil {
			if st.Let.Type != nil {
				t = types.ResolveTypeRef(st.Let.Type, env)
			} else if st.Let.Value != nil {
				t = types.TypeOfExprBasic(st.Let.Value, env)
				if _, ok := t.(types.AnyType); ok {
					if call := st.Let.Value.Binary.Left.Value.Target.Call; call != nil {
						if ft, err := env.GetVar(call.Func); err == nil {
							if fn, ok := ft.(types.FuncType); ok {
								t = fn.Return
							}
						}
					}
				}
				if _, ok := t.(types.AnyType); ok && st.Let.Value != nil {
					if types.IsStringExpr(st.Let.Value, env) {
						t = types.StringType{}
					}
				}
			}
			env.SetVar(st.Let.Name, t, true)
		}
		return &LetStmt{Name: st.Let.Name, Expr: e, Type: t}, nil
	case st.Var != nil:
		var e Expr
		var err error
		if st.Var.Value != nil {
			e, err = convertExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		} else {
			e = zeroValue(st.Var.Type, env)
		}
		var t types.Type
		if env != nil {
			if st.Var.Type != nil {
				t = types.ResolveTypeRef(st.Var.Type, env)
			} else if st.Var.Value != nil {
				t = types.TypeOfExprBasic(st.Var.Value, env)
				if _, ok := t.(types.AnyType); ok {
					if call := st.Var.Value.Binary.Left.Value.Target.Call; call != nil {
						if ft, err := env.GetVar(call.Func); err == nil {
							if fn, ok := ft.(types.FuncType); ok {
								t = fn.Return
							}
						}
					}
				}
				if _, ok := t.(types.AnyType); ok && st.Var.Value != nil {
					if types.IsStringExpr(st.Var.Value, env) {
						t = types.StringType{}
					}
				}
			}
			env.SetVar(st.Var.Name, t, true)
		}
		return &LetStmt{Name: st.Var.Name, Expr: e, Type: t}, nil
	case st.Assign != nil:
		e, err := convertExpr(st.Assign.Value, env)
		if err != nil {
			return nil, err
		}
		if len(st.Assign.Index) == 1 && st.Assign.Index[0].Colon == nil {
			idxExpr, err := convertExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			if t, err2 := env.GetVar(st.Assign.Name); err2 == nil {
				if _, ok := t.(types.MapType); ok {
					return &MapAssignStmt{Name: st.Assign.Name, Key: idxExpr, Expr: e}, nil
				}
				if _, ok := t.(types.AnyType); ok {
					idxType := types.TypeOfExprBasic(st.Assign.Index[0].Start, env)
					if _, ok := idxType.(types.StringType); ok {
						return &MapAssignStmt{Name: st.Assign.Name, Key: idxExpr, Expr: e}, nil
					}
				}
			}
			return &ListAssignStmt{Name: st.Assign.Name, Index: idxExpr, Expr: e}, nil
		}
		if len(st.Assign.Index) == 2 && st.Assign.Index[0].Colon == nil && st.Assign.Index[1].Colon == nil {
			idx1, err := convertExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			idx2, err := convertExpr(st.Assign.Index[1].Start, env)
			if err != nil {
				return nil, err
			}
			if t, err2 := env.GetVar(st.Assign.Name); err2 == nil {
				switch vt := t.(type) {
				case types.MapType:
					if _, ok := vt.Value.(types.MapType); ok {
						return &DoubleMapAssignStmt{Name: st.Assign.Name, Key1: idx1, Key2: idx2, Expr: e}, nil
					}
				case types.ListType:
					if _, ok := vt.Elem.(types.MapType); ok {
						if _, ok := types.TypeOfExprBasic(st.Assign.Index[1].Start, env).(types.StringType); ok {
							return &ListMapAssignStmt{Name: st.Assign.Name, Index: idx1, Key: idx2, Expr: e}, nil
						}
					}
				}
			}
			return &DoubleListAssignStmt{Name: st.Assign.Name, Index1: idx1, Index2: idx2, Expr: e}, nil
		}
		if len(st.Assign.Field) == 1 && len(st.Assign.Index) == 0 {
			key := &StringLit{Value: st.Assign.Field[0].Name}
			return &MapAssignStmt{Name: st.Assign.Name, Key: key, Expr: e}, nil
		}
		if _, err2 := env.GetVar(st.Assign.Name); err2 != nil && env != nil {
			typ := types.TypeOfExprBasic(st.Assign.Value, env)
			env.SetVar(st.Assign.Name, typ, true)
			return &LetStmt{Name: st.Assign.Name, Expr: e, Type: typ}, nil
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
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			return convertSaveStmt(se, env)
		}
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
		if call != nil {
			expr, err := convertCall(call, env)
			if err != nil {
				return nil, err
			}
			return &ExprStmt{Expr: expr}, nil
		}
		expr, err := convertExpr(st.Expr.Expr, env)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: expr}, nil
	case st.Update != nil:
		up, err := convertUpdateStmt(st.Update, env)
		if err != nil {
			return nil, err
		}
		return up, nil
	case st.Type != nil:
		var fields []string
		for _, mem := range st.Type.Members {
			if mem.Field != nil {
				fields = append(fields, mem.Field.Name)
			}
		}
		var stmts []Stmt
		methodParams := map[string]int{}
		for _, mem := range st.Type.Members {
			if mem.Method != nil {
				methodParams[mem.Method.Name] = len(mem.Method.Params) + 1
			}
		}
		for _, mem := range st.Type.Members {
			if mem.Method != nil {
				child := types.NewEnv(env)
				child.SetVar("self", types.AnyType{}, true)
				params := []string{"self"}
				for _, p := range mem.Method.Params {
					params = append(params, p.Name)
					if env != nil {
						var pt types.Type = types.AnyType{}
						if p.Type != nil {
							pt = types.ResolveTypeRef(p.Type, env)
						}
						child.SetVar(p.Name, pt, true)
					}
				}
				for name, cnt := range methodParams {
					child.SetVar(name, types.FuncType{Params: make([]types.Type, cnt)}, true)
				}
				for _, f := range fields {
					child.SetVar(f, types.AnyType{}, true)
				}
				body, err := convertStatements(mem.Method.Body, child)
				if err != nil {
					return nil, err
				}
				var pre []Stmt
				for _, f := range fields {
					pre = append(pre, &LetStmt{Name: f, Expr: &IndexExpr{Target: &Name{Name: "self"}, Index: &StringLit{Value: f}, IsMap: true}})
				}
				body = append(pre, body...)
				stmts = append(stmts, &FunDecl{Name: mem.Method.Name, Params: params, Body: body})
			}
		}
		if len(stmts) > 0 {
			return StmtList(stmts), nil
		}
		return nil, nil
	case st.ExternVar != nil:
		// extern variables are provided by imported modules
		return nil, nil
	case st.ExternFun != nil:
		// extern functions are implemented in imported modules
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
	case st.Bench != nil:
		child := types.NewEnv(env)
		body, err := convertStatements(st.Bench.Body, child)
		if err != nil {
			return nil, err
		}
		return &BenchStmt{Name: st.Bench.Name, Body: body}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
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
		for i, st := range body {
			body[i] = replaceRangeContinue(st, n.Name)
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
	stringIter := false
	if _, ok := types.ExprType(n.Source, env).(types.StringType); ok {
		stringIter = true
	}
	return &ForInStmt{Name: n.Name, Iterable: iter, Body: body, Keys: keys, String: stringIter, Break: breakCond, Unless: continueCond}, nil
}

func replaceRangeContinue(st Stmt, varName string) Stmt {
	switch s := st.(type) {
	case *ContinueStmt:
		if varName == "" {
			return s
		}
		return &RangeContinueStmt{Var: varName}
	case *IfStmt:
		for i, c := range s.Then {
			s.Then[i] = replaceRangeContinue(c, varName)
		}
		for i, c := range s.Else {
			s.Else[i] = replaceRangeContinue(c, varName)
		}
		return s
	case *WhileStmt:
		for i, c := range s.Body {
			s.Body[i] = replaceRangeContinue(c, varName)
		}
		return s
	case *ForRangeStmt:
		for i, c := range s.Body {
			s.Body[i] = replaceRangeContinue(c, s.Name)
		}
		return s
	case *ForInStmt:
		for i, c := range s.Body {
			s.Body[i] = replaceRangeContinue(c, s.Name)
		}
		return s
	default:
		return s
	}
}

func convertFunStmt(fn *parser.FunStmt, env *types.Env) (Stmt, error) {
	child := types.NewEnv(env)
	var params []string
	for _, p := range fn.Params {
		params = append(params, p.Name)
		var typ types.Type = types.AnyType{}
		if p.Type != nil {
			typ = types.ResolveTypeRef(p.Type, env)
		}
		child.SetVar(p.Name, typ, true)
	}
	body, err := convertStatements(fn.Body, child)
	if err != nil {
		return nil, err
	}
	name := sanitizeName(fn.Name)
	if name != fn.Name {
		funcNameMap[fn.Name] = name
	}
	return &FunDecl{Name: name, Params: params, Body: body}, nil
}

func convertFunExpr(fn *parser.FunExpr, env *types.Env) (Expr, error) {
	child := types.NewEnv(env)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
		var typ types.Type = types.AnyType{}
		if p.Type != nil {
			typ = types.ResolveTypeRef(p.Type, env)
		}
		child.SetVar(p.Name, typ, true)
	}
	if fn.ExprBody != nil {
		expr, err := convertExpr(fn.ExprBody, child)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Expr: expr}, nil
	}
	stmts, err := convertStatements(fn.BlockBody, child)
	if err != nil {
		return nil, err
	}
	return &LambdaExpr{Params: params, Body: stmts}, nil
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
	return &SaveStmt{Src: src, Path: path, Format: format}, nil
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
	case "==", "!=", "string=?", "string!=", "<", "<=", ">", ">=",
		"string<?", "string<=?", "string>?", "string>=?":
		return 3
	case "+", "-", "string-append":
		return 4
	case "*", "/", "modulo", "quotient":
		return 5
	default:
		return 0
	}
}

func simplifyBoolComparison(expr Expr, op, boolName string) Expr {
	switch op {
	case "==":
		if boolName == "#t" {
			return expr
		}
		return &UnaryExpr{Op: "!", Expr: expr}
	case "!=":
		if boolName == "#t" {
			return &UnaryExpr{Op: "!", Expr: expr}
		}
		return expr
	}
	return &BinaryExpr{Op: op, Left: expr, Right: &Name{Name: boolName}}
}

func convertBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	left, err := convertUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	exprs := []Expr{left}
	ops := []string{}
	leftType := types.TypeOfUnary(b.Left, env)
	for _, part := range b.Right {
		right, err := convertPostfix(part.Right, env)
		if err != nil {
			return nil, err
		}
		op := part.Op
		if op == "union" {
			left := exprs[len(exprs)-1]
			var u Expr
			if part.All {
				u = &CallExpr{Func: "append", Args: []Expr{left, right}}
			} else {
				u = &CallExpr{Func: "remove-duplicates", Args: []Expr{&CallExpr{Func: "append", Args: []Expr{left, right}}}}
			}
			exprs[len(exprs)-1] = u
			leftType = types.AnyType{}
			continue
		}
		currLeft := exprs[len(exprs)-1]
		if op == "==" || op == "!=" {
			if (types.IsStringType(leftType) || types.IsStringPostfix(part.Right, env)) && !isBoolExpr(currLeft) && !isBoolExpr(right) {
				if op == "==" {
					op = "string=?"
				} else {
					op = "string!="
				}
				leftIsStr := types.IsStringType(leftType)
				rightIsStr := types.IsStringPostfix(part.Right, env)
				if leftIsStr && !rightIsStr {
					if n, ok := right.(*Name); ok {
						if n.Name == "#t" {
							right = &StringLit{Value: "true"}
						} else if n.Name == "#f" {
							right = &StringLit{Value: "false"}
						}
					}
				}
				if rightIsStr && !leftIsStr {
					if n, ok := currLeft.(*Name); ok {
						if n.Name == "#t" {
							currLeft = &StringLit{Value: "true"}
						} else if n.Name == "#f" {
							currLeft = &StringLit{Value: "false"}
						}
						exprs[len(exprs)-1] = currLeft
					}
				}
			}
			if n, ok := right.(*Name); ok && (n.Name == "#t" || n.Name == "#f") {
				exprs[len(exprs)-1] = simplifyBoolComparison(currLeft, op, n.Name)
				continue
			}
			if n, ok := currLeft.(*Name); ok && (n.Name == "#t" || n.Name == "#f") {
				exprs[len(exprs)-1] = simplifyBoolComparison(right, op, n.Name)
				continue
			}
		}
		if op == "+" {
			stringLeft := false
			listLeft := false
			if t := types.TypeOfPostfixBasic(b.Left, env); t != nil {
				if _, ok := t.(types.StringType); ok {
					stringLeft = true
				} else if _, ok := t.(types.ListType); ok {
					listLeft = true
				}
			}
			stringRight := false
			listRight := false
			if t := types.TypeOfPostfixBasic(&parser.Unary{Value: part.Right}, env); t != nil {
				if _, ok := t.(types.StringType); ok {
					stringRight = true
				} else if _, ok := t.(types.ListType); ok {
					listRight = true
				}
			}
			if !stringLeft {
				if _, ok := currLeft.(*StringLit); ok {
					stringLeft = true
				}
				if lb, ok := currLeft.(*BinaryExpr); ok && lb.Op == "string-append" {
					stringLeft = true
				}
			}
			if !listLeft {
				if lb, ok := currLeft.(*BinaryExpr); ok && lb.Op == "list-append" {
					listLeft = true
				}
			}
			if !stringRight {
				if _, ok := right.(*StringLit); ok {
					stringRight = true
				}
			}
			if !listRight {
				if rb, ok := right.(*BinaryExpr); ok && rb.Op == "list-append" {
					listRight = true
				}
			}
			if stringLeft || stringRight {
				op = "string-append"
			} else if listLeft || listRight {
				op = "list-append"
			}
		}
		if op == "&&" {
			op = "and"
		}
		if op == "||" {
			op = "or"
		}
		if op == "/" {
			ltInt := exprIsInt(currLeft, env) || heuristicInt(currLeft, env)
			rt := types.TypeOfPostfix(part.Right, env)
			rtInt := types.IsIntType(rt) || types.IsInt64Type(rt) || types.IsBigIntType(rt)
			if types.IsFloatType(leftType) || types.IsFloatType(rt) {
				op = "/"
			} else if lt := leftType; (types.IsIntType(lt) || types.IsInt64Type(lt) || types.IsBigIntType(lt)) && rtInt {
				op = "quotient"
			} else if ltInt && (rtInt || heuristicInt(right, env)) {
				op = "quotient"
			} else {
				lt := leftType
				if literalIntPostfix(part.Right) && !types.IsFloatType(lt) {
					op = "quotient"
				} else if (types.IsIntType(lt) || types.IsInt64Type(lt) || types.IsBigIntType(lt)) && rtInt {
					op = "quotient"
				} else {
					op = "/"
				}
			}
		}
		if op == "in" {
			isStr := types.IsStringPostfix(part.Right, env)
			isMap := types.IsMapPostfix(part.Right, env)
			if !isMap {
				if call := part.Right.Target.Call; call != nil {
					if call.Func == "hash-ref" || call.Func == "hash-keys" {
						isMap = true
					}
				}
			}
			if !isMap && len(part.Right.Ops) == 1 && part.Right.Ops[0].Field != nil {
				if types.IsMapPrimary(part.Right.Target, env) {
					isMap = true
				}
			}
			if !isMap {
				if types.IsMapType(types.TypeOfPostfix(part.Right, env)) {
					isMap = true
				}
			}
			if !isMap {
				if idx, ok := right.(*IndexExpr); ok && idx.IsMap {
					isMap = true
				}
			}
			opExpr := &InExpr{Elem: exprs[len(exprs)-1], Set: right, IsStr: isStr, IsMap: isMap}
			exprs[len(exprs)-1] = opExpr
			continue
		}
		if op == "<" || op == "<=" || op == ">" || op == ">=" {
			leftIsString := false
			if _, ok := leftType.(types.StringType); ok {
				leftIsString = true
			}
			rightIsString := false
			if _, ok := types.TypeOfPostfix(part.Right, env).(types.StringType); ok {
				rightIsString = true
			}
			if leftIsString || rightIsString {
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
			lt := leftType
			rt := types.TypeOfPostfix(part.Right, env)
			if types.IsFloatType(lt) || types.IsFloatType(rt) {
				left := exprs[len(exprs)-1]
				exprs[len(exprs)-1] = &BinaryExpr{Op: "fmod", Left: left, Right: right}
				continue
			}
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
		switch op {
		case "&&", "||", "and", "or", "==", "!=", "string=?", "string!=", "<", "<=", ">", ">=", "string<?", "string<=?", "string>?", "string>=?", "in":
			leftType = types.BoolType{}
		default:
			leftType = types.AnyType{}
		}
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
	t := types.TypeOfPrimaryBasic(pf.Target, env)
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
					t = types.AnyType{}
					break
				}
			}
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}, IsMap: true}
			if st, ok := t.(types.StructType); ok {
				if ft, ok2 := st.Fields[op.Field.Name]; ok2 {
					t = ft
				} else {
					t = types.AnyType{}
				}
			} else if mt, ok := t.(types.MapType); ok {
				t = mt.Value
			} else {
				t = types.AnyType{}
			}
		case op.Call != nil:
			switch n := expr.(type) {
			case *Name:
				var err error
				expr, err = convertCall(&parser.CallExpr{Func: n.Name, Args: op.Call.Args}, env)
				if err != nil {
					return nil, err
				}
				if ft, err := env.GetVar(n.Name); err == nil {
					if fn, ok := ft.(types.FuncType); ok {
						t = fn.Return
					}
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
							if ft, err := env.GetVar(fn); err == nil {
								if fnType, ok := ft.(types.FuncType); ok {
									t = fnType.Return
								}
							}
							break
						}
					}
				}
				if lit, ok := n.Index.(*StringLit); ok {
					if lit.Value == "keys" {
						if len(op.Call.Args) == 0 && types.IsMapPrimary(pf.Target, env) {
							expr = &CallExpr{Func: "hash-keys", Args: []Expr{n.Target}}
							break
						}
					} else if lit.Value == "contains" {
						arg, err := convertExpr(op.Call.Args[0], env)
						if err != nil {
							return nil, err
						}
						switch {
						case types.IsStringPrimary(pf.Target, env):
							expr = &CallExpr{Func: "string-contains?", Args: []Expr{n.Target, arg}}
						case types.IsMapPrimary(pf.Target, env):
							expr = &CallExpr{Func: "hash-has-key?", Args: []Expr{n.Target, arg}}
						default:
							expr = &CallExpr{Func: "member", Args: []Expr{arg, n.Target}}
						}
						break
					} else if lit.Value == "padStart" {
						if len(op.Call.Args) == 2 {
							arg1, err := convertExpr(op.Call.Args[0], env)
							if err != nil {
								return nil, err
							}
							arg2, err := convertExpr(op.Call.Args[1], env)
							if err != nil {
								return nil, err
							}
							expr = &CallExpr{Func: "pad-start", Args: []Expr{n.Target, arg1, arg2}}
							break
						}
					} else if lit.Value == "repeat" {
						if len(op.Call.Args) == 1 {
							arg1, err := convertExpr(op.Call.Args[0], env)
							if err != nil {
								return nil, err
							}
							expr = &CallExpr{Func: "_repeat", Args: []Expr{n.Target, arg1}}
							break
						}
					} else if lit.Value == "get" {
						if len(op.Call.Args) == 2 {
							key, err := convertExpr(op.Call.Args[0], env)
							if err != nil {
								return nil, err
							}
							def, err := convertExpr(op.Call.Args[1], env)
							if err != nil {
								return nil, err
							}
							expr = &CallExpr{Func: "hash-ref", Args: []Expr{n.Target, key, def}}
							break
						}
					} else if pf.Target != nil && pf.Target.Selector != nil && env != nil {
						if rt, err := env.GetVar(pf.Target.Selector.Root); err == nil {
							if st, ok := rt.(types.StructType); ok {
								if m, ok := st.Methods[lit.Value]; ok {
									var margs []Expr
									margs = append(margs, n.Target)
									for _, a := range op.Call.Args {
										arg, err := convertExpr(a, env)
										if err != nil {
											return nil, err
										}
										margs = append(margs, arg)
									}
									expr = &CallExpr{Func: lit.Value, Args: margs}
									t = m.Type.Return
									break
								}
							}
						}
					} else if types.IsStructType(types.TypeOfPrimaryBasic(pf.Target, env)) {
						if st, ok := types.TypeOfPrimaryBasic(pf.Target, env).(types.StructType); ok {
							if m, ok := st.Methods[lit.Value]; ok {
								var margs []Expr
								margs = append(margs, n.Target)
								for _, a := range op.Call.Args {
									arg, err := convertExpr(a, env)
									if err != nil {
										return nil, err
									}
									margs = append(margs, arg)
								}
								expr = &CallExpr{Func: lit.Value, Args: margs}
								t = m.Type.Return
								break
							}
						}
					}
					// call the function stored in the field, passing the target as first argument
					var args []Expr
					args = append(args, n.Target)
					for _, a := range op.Call.Args {
						arg, err := convertExpr(a, env)
						if err != nil {
							return nil, err
						}
						args = append(args, arg)
					}
					expr = &InvokeExpr{Callee: expr, Args: args}
					if fn, ok := t.(types.FuncType); ok {
						t = fn.Return
					} else {
						t = types.AnyType{}
					}
					break
				}
			default:
				var args []Expr
				for _, a := range op.Call.Args {
					arg, err := convertExpr(a, env)
					if err != nil {
						return nil, err
					}
					args = append(args, arg)
				}
				expr = &InvokeExpr{Callee: expr, Args: args}
				t = types.AnyType{}
			}
		case op.Index != nil && op.Index.Colon == nil:
			idx, err := convertExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			isStr := types.IsStringType(t)
			isMap := types.IsMapType(t)
			expr = &IndexExpr{Target: expr, Index: idx, IsString: isStr, IsMap: isMap}
			if isMap {
				if mt, ok := t.(types.MapType); ok {
					t = mt.Value
				} else {
					t = types.AnyType{}
				}
			} else if lt, ok := t.(types.ListType); ok {
				t = lt.Elem
			} else {
				t = types.AnyType{}
			}
		case op.Index != nil && op.Index.Colon != nil:
			if op.Index.Colon2 != nil || op.Index.Step != nil {
				return nil, fmt.Errorf("slice step not supported")
			}
			var start, end Expr
			if op.Index.Start != nil {
				start, err = convertExpr(op.Index.Start, env)
				if err != nil {
					return nil, err
				}
			}
			if op.Index.End != nil {
				end, err = convertExpr(op.Index.End, env)
				if err != nil {
					return nil, err
				}
			}
			isStr := types.IsStringPrimary(pf.Target, env)
			if !isStr && start != nil && end != nil && plusOne(end, start) {
				expr = &IndexExpr{Target: expr, Index: start, IsString: false, IsMap: false}
			} else {
				expr = &SliceExpr{Target: expr, Start: start, End: end, IsString: isStr}
			}
		case op.Cast != nil && op.Cast.Type != nil:
			if op.Cast.Type.Simple != nil {
				expr = &CastExpr{Value: expr, Type: *op.Cast.Type.Simple}
			} else if op.Cast.Type.Generic != nil {
				expr = &CastExpr{Value: expr, Type: op.Cast.Type.Generic.Name}
			} else {
				expr = &CastExpr{Value: expr}
			}
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
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		return dataExprFromFile(path, format, p.Load.Type, env)
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr, env)
	case p.Call != nil:
		return convertCall(p.Call, env)
	case p.Match != nil:
		return convertMatchExpr(p.Match, env)
	case p.Query != nil:
		return convertQueryExpr(p.Query, env)
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL, env)
		if err != nil {
			return nil, err
		}
		if p.Fetch.With != nil {
			optsExpr, err := convertExpr(p.Fetch.With, env)
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr, optsExpr}}, nil
		}
		return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}, nil
	case p.If != nil:
		return convertIfExpr(p.If, env)
	case p.Group != nil:
		return convertExpr(p.Group, env)
	case p.Selector != nil:
		if p.Selector.Root == "nil" {
			return &Name{Name: "#f"}, nil
		}
		return &Name{Name: sanitizeName(p.Selector.Root)}, nil
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
	if l.Null {
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
	if env != nil {
		if ut, ok := env.FindUnionByVariant(s.Name); ok {
			st := ut.Variants[s.Name]
			args := []Expr{&StringLit{Value: "tag"}, &StringLit{Value: s.Name}}
			for i, f := range s.Fields {
				val, err := convertExpr(f.Value, env)
				if err != nil {
					return nil, err
				}
				key := f.Name
				if i < len(st.Order) {
					key = st.Order[i]
				}
				args = append(args, &StringLit{Value: key}, val)
			}
			return &CallExpr{Func: "hash", Args: args}, nil
		}
	}
	var args []Expr
	for _, f := range s.Fields {
		val, err := convertExpr(f.Value, env)
		if err != nil {
			return nil, err
		}
		args = append(args, &StringLit{Value: f.Name}, val)
	}
	if env != nil {
		if st, ok := env.GetStruct(s.Name); ok {
			for name := range st.Methods {
				args = append(args, &StringLit{Value: name}, &Name{Name: name})
			}
		}
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

func convertMatchExpr(m *parser.MatchExpr, env *types.Env) (Expr, error) {
	target, err := convertExpr(m.Target, env)
	if err != nil {
		return nil, err
	}
	var expr Expr = &Name{Name: "#f"}
	for i := len(m.Cases) - 1; i >= 0; i-- {
		c := m.Cases[i]
		cond, binds, err := patternCond(c.Pattern, target, env)
		if err != nil {
			return nil, err
		}
		body, err := convertExpr(c.Result, env)
		if err != nil {
			return nil, err
		}
		if len(binds) > 0 {
			body = &LetExpr{Binds: binds, Body: body}
		}
		expr = &IfExpr{Cond: cond, Then: body, Else: expr}
	}
	return expr, nil
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
	if fn, ok := funcNameMap[c.Func]; ok {
		return &CallExpr{Func: fn, Args: args}, nil
	}
	if env != nil {
		if _, err := env.GetVar("self"); err == nil {
			if ftv, err := env.GetVar(c.Func); err == nil {
				if ft, ok := ftv.(types.FuncType); ok {
					if len(ft.Params) == len(args)+1 {
						args = append([]Expr{&Name{Name: "self"}}, args...)
					}
				}
			}
		}
		if ut, ok := env.FindUnionByVariant(c.Func); ok {
			st := ut.Variants[c.Func]
			if len(args) == len(st.Order) {
				entries := []Expr{&StringLit{Value: "tag"}, &StringLit{Value: c.Func}}
				for i, name := range st.Order {
					entries = append(entries, &StringLit{Value: name}, args[i])
				}
				return &CallExpr{Func: "hash", Args: entries}, nil
			}
		}
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
	case "concat":
		if len(args) >= 2 {
			if env != nil {
				t := types.TypeOfExprBasic(c.Args[0], env)
				if _, ok := t.(types.StringType); ok {
					return &CallExpr{Func: "string-append", Args: args}, nil
				}
			}
			return &CallExpr{Func: "append", Args: args}, nil
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
			return &CallExpr{Func: "list?", Args: args}, nil
		}
	case "keys":
		if len(args) == 1 {
			return &CallExpr{Func: "hash-keys", Args: args}, nil
		}
	case "str":
		if len(args) == 1 {
			return &StrExpr{Arg: args[0]}, nil
		}
	case "substring":
		if len(args) == 3 {
			return &CallExpr{Func: "slice", Args: args}, nil
		}
	case "substr":
		if len(args) == 3 {
			return &CallExpr{Func: "substring", Args: args}, nil
		}
	case "slice":
		if len(args) == 3 {
			isStr := false
			if env != nil {
				t := types.TypeOfExprBasic(c.Args[0], env)
				if _, ok := t.(types.StringType); ok {
					isStr = true
				}
			}
			return &SliceExpr{Target: args[0], Start: args[1], End: args[2], IsString: isStr}, nil
		}
	case "contains":
		if len(args) == 2 {
			if env != nil {
				t := types.TypeOfExprBasic(c.Args[0], env)
				switch t.(type) {
				case types.MapType:
					return &CallExpr{Func: "hash-has-key?", Args: []Expr{args[0], args[1]}}, nil
				case types.ListType:
					return &CallExpr{Func: "member", Args: []Expr{args[1], args[0]}}, nil
				case types.StringType:
					return &CallExpr{Func: "string-contains?", Args: args}, nil
				}
			}
			return &CallExpr{Func: "member", Args: []Expr{args[1], args[0]}}, nil
		}
	case "sha256":
		if len(args) == 1 {
			return &CallExpr{Func: "_sha256", Args: args}, nil
		}
	case "padStart":
		if len(args) == 3 {
			// args[0]=string, args[1]=length, args[2]=pad string
			return &CallExpr{Func: "pad-start", Args: args}, nil
		}
	case "indexOf":
		if len(args) == 2 {
			return &CallExpr{Func: "index-of", Args: args}, nil
		}
	case "split":
		if len(args) == 2 {
			return &CallExpr{Func: "string-split", Args: args}, nil
		}
	case "write":
		if len(args) == 2 {
			if n, ok := args[0].(*Name); ok && n.Name == "stdout" {
				return &CallExpr{Func: "display", Args: []Expr{args[1]}}, nil
			}
		}
	case "repeat":
		if len(args) == 2 {
			return &CallExpr{Func: "_repeat", Args: args}, nil
		}
	case "fetch":
		if len(args) == 1 {
			return &CallExpr{Func: "_fetch", Args: args}, nil
		}
	case "parseIntStr":
		if len(args) == 1 || len(args) == 2 {
			v0 := args[0]
			var v1 Expr = &IntLit{Value: 10}
			if len(args) == 2 {
				v1 = args[1]
			}
			return &CallExpr{Func: "_parse-int-str", Args: []Expr{v0, v1}}, nil
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
	case "pow":
		if len(args) == 2 {
			if env != nil {
				t0 := types.TypeOfExprBasic(c.Args[0], env)
				t1 := types.TypeOfExprBasic(c.Args[1], env)
				if (types.IsNumericType(t0) || types.IsFloatType(t0)) && (types.IsNumericType(t1) || types.IsFloatType(t1)) {
					return &CallExpr{Func: "expt", Args: args}, nil
				}
			}
			return &CallExpr{Func: c.Func, Args: args}, nil
		}
	case "math.sin":
		if len(args) == 1 {
			return &CallExpr{Func: "sin", Args: args}, nil
		}
	case "math.log":
		if len(args) == 1 {
			return &CallExpr{Func: "log", Args: args}, nil
		}
	case "now":
		if len(args) == 0 {
			return &CallExpr{Func: "now"}, nil
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

func patternCond(pat *parser.Expr, target Expr, env *types.Env) (Expr, []LetBind, error) {
	if pat == nil {
		return nil, nil, fmt.Errorf("nil pattern")
	}
	if call, ok := callPattern(pat); ok && env != nil {
		if ut, ok := env.FindUnionByVariant(call.Func); ok {
			st := ut.Variants[call.Func]
			cond := &BinaryExpr{Op: "==", Left: &IndexExpr{Target: target, Index: &StringLit{Value: "tag"}, IsMap: true}, Right: &StringLit{Value: call.Func}}
			binds := []LetBind{}
			for i, arg := range call.Args {
				field := st.Order[i]
				if name, ok := identName(arg); ok {
					if name != "_" {
						binds = append(binds, LetBind{Name: name, Expr: &IndexExpr{Target: target, Index: &StringLit{Value: field}, IsMap: true}})
					}
				} else {
					val, err := convertExpr(arg, env)
					if err != nil {
						return nil, nil, err
					}
					part := &BinaryExpr{Op: "==", Left: &IndexExpr{Target: target, Index: &StringLit{Value: field}, IsMap: true}, Right: val}
					cond = &BinaryExpr{Op: "and", Left: cond, Right: part}
				}
			}
			return cond, binds, nil
		}
	}
	if name, ok := identName(pat); ok {
		if name == "_" {
			return &Name{Name: "#t"}, nil, nil
		}
		if env != nil {
			if _, ok := env.FindUnionByVariant(name); ok {
				return &BinaryExpr{Op: "==", Left: &IndexExpr{Target: target, Index: &StringLit{Value: "tag"}, IsMap: true}, Right: &StringLit{Value: name}}, nil, nil
			}
		}
	}
	expr, err := convertExpr(pat, env)
	if err != nil {
		return nil, nil, err
	}
	return &BinaryExpr{Op: "==", Left: target, Right: expr}, nil, nil
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	pf := u.Value
	if len(pf.Ops) == 0 && pf.Target != nil && pf.Target.Call != nil {
		return pf.Target.Call, true
	}
	return nil, false
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	pf := u.Value
	if len(pf.Ops) == 0 && pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0 {
		return pf.Target.Selector.Root, true
	}
	return "", false
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
	if c.Sort == nil && c.Skip == nil && c.Take == nil {
		io.WriteString(w, "(for*/list (")
		for i := range c.Vars {
			if i > 0 {
				io.WriteString(w, " ")
			}
			io.WriteString(w, "[")
			io.WriteString(w, c.Vars[i])
			io.WriteString(w, " ")
			c.Sources[i].emit(w)
			io.WriteString(w, "]")
		}
		io.WriteString(w, ")")
		if c.Where != nil {
			io.WriteString(w, " #:when ")
			c.Where.emit(w)
		}
		io.WriteString(w, " ")
		c.Select.emit(w)
		io.WriteString(w, ")")
		return
	}

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
	Desc    bool
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
	Desc     bool
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
	Desc     bool
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
	Desc    bool
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
		} else if g.Desc {
			io.WriteString(w, ">")
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
		} else if g.Desc {
			io.WriteString(w, ">")
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
		} else if g.Desc {
			io.WriteString(w, ">")
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
		} else if g.Desc {
			io.WriteString(w, ">")
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
	var desc bool
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		if u, ok := sortExpr.(*UnaryExpr); ok && u.Op == "-" {
			sortExpr = u.Expr
			desc = true
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
	return &GroupByExpr{Var: q.Var, Source: src, Key: key, Name: q.Group.Name, Where: where, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Desc: desc, Skip: skipExpr, Take: takeExpr}, nil
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
	var desc bool
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		if u, ok := sortExpr.(*UnaryExpr); ok && u.Op == "-" {
			sortExpr = u.Expr
			desc = true
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
	return &GroupJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Key: key, Row: row, Name: q.Group.Name, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Desc: desc, Skip: skipExpr, Take: takeExpr}, nil
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
	var desc bool
	var skipExpr, takeExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		if u, ok := sortExpr.(*UnaryExpr); ok && u.Op == "-" {
			sortExpr = u.Expr
			desc = true
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
	return &GroupLeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Key: key, Row: row, Name: q.Group.Name, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Desc: desc, Skip: skipExpr, Take: takeExpr}, nil
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
	var desc bool
	var skipExpr, takeExpr Expr
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		if u, ok := sortExpr.(*UnaryExpr); ok && u.Op == "-" {
			sortExpr = u.Expr
			desc = true
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
	return &GroupMultiJoinExpr{Vars: vars, Sources: exprs, Cond: cond, Key: key, Row: row, Name: q.Group.Name, Select: sel, Having: having, Sort: sortExpr, SortCmp: sortCmp, Desc: desc, Skip: skipExpr, Take: takeExpr}, nil
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
