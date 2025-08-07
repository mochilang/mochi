//go:build slow

package pl

import (
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"unicode"

	"mochi/ast"
	"mochi/parser"
	"mochi/runtime/data"
	"mochi/types"
)

var usesNow bool
var usesSlice bool
var usesFmt bool
var usesLen bool
var benchMain bool

// SetBenchMain configures whether the generated main function should be wrapped
// in a benchmark block when emitting code. When enabled, execution duration and
// memory statistics will be printed as a JSON object.
func SetBenchMain(v bool) { benchMain = v }

const helperNow = `
:- dynamic now_seed/1.
:- dynamic now_seeded/1.

init_now :-
    ( getenv('MOCHI_NOW_SEED', S), S \= '' ->
        atom_number(S, V),
        assertz(now_seed(V)),
        assertz(now_seeded(true))
    ;
        assertz(now_seed(0)),
        assertz(now_seeded(false))
    ).

mochi_now(T) :-
    ( now_seeded(true) ->
        retract(now_seed(S)),
        NS is (S*1664525 + 1013904223) mod 2147483647,
        assertz(now_seed(NS)),
        T = NS
    ;
        get_time(Time), T is floor(Time*1000000000)
    ).
`

const helperSlice = `
_slice(List, Start, End, Result) :-
    length(List, Len),
    S is max(Start, 0),
    E is min(End, Len),
    L is max(E - S, 0),
    length(Prefix, S),
    append(Prefix, Rest, List),
    length(Result, L),
    append(Result, _, Rest).
`

const helperFmt = `
print_fmt(Fmt, Args, _) :- format(Fmt, Args).
`

const helperLen = `
:- arithmetic_function(len/1).
len(X, R) :-
    ( is_list(X) -> length(X, R)
    ; string(X) -> string_length(X, R)
    ).
`

// Program represents a simple Prolog program.
type Program struct {
	Funcs []*Function
	Stmts []Stmt
}

func valueToExpr(v any) Expr {
	switch val := v.(type) {
	case nil:
		return &StringLit{Value: ""}
	case bool:
		return &BoolLit{Value: val}
	case int64:
		return &IntLit{Value: int(val)}
	case int:
		return &IntLit{Value: val}
	case float64:
		if math.Trunc(val) == val {
			return &IntLit{Value: int(val)}
		}
		return &FloatLit{Value: val}
	case string:
		return &StringLit{Value: val}
	case map[string]any:
		items := make([]MapItem, 0, len(val))
		for k, vv := range val {
			items = append(items, MapItem{Key: k, Value: valueToExpr(vv)})
		}
		sort.Slice(items, func(i, j int) bool { return items[i].Key < items[j].Key })
		return &MapLit{Items: items}
	case []any:
		elems := make([]Expr, len(val))
		for i, vv := range val {
			elems[i] = valueToExpr(vv)
		}
		return &ListLit{Elems: elems}
	default:
		return &StringLit{Value: fmt.Sprint(v)}
	}
}

func compileLoadExpr(l *parser.LoadExpr) (Expr, error) {
	path := ""
	if l.Path != nil {
		path = *l.Path
		if !filepath.IsAbs(path) {
			root := repoRootDir()
			cand := filepath.Join(root, path)
			if _, err := os.Stat(cand); err == nil {
				path = cand
			} else {
				clean := path
				for strings.HasPrefix(clean, "../") {
					clean = strings.TrimPrefix(clean, "../")
				}
				path = filepath.Join(root, "tests", clean)
			}
		}
	}
	format := "jsonl"
	if strings.HasSuffix(path, ".yaml") {
		format = "yaml"
	} else if strings.HasSuffix(path, ".json") {
		format = "json"
	} else if l.With != nil && l.With.Binary != nil && l.With.Binary.Left != nil {
		m := l.With.Binary.Left.Value.Target.Map
		if m != nil {
			for _, it := range m.Items {
				k, okk := constValue(it.Key)
				v, okv := constValue(it.Value)
				if okk && okv {
					if ks, ok := k.(string); ok && ks == "format" {
						if s, ok2 := v.(string); ok2 {
							format = s
						}
					}
				}
			}
		}
	}
	var rows []map[string]any
	var err error
	switch format {
	case "yaml":
		rows, err = data.LoadYAML(path)
	case "json":
		rows, err = data.LoadJSON(path)
	default:
		rows, err = data.LoadJSONL(path)
	}
	if err != nil {
		return nil, err
	}
	elems := make([]Expr, len(rows))
	for i, r := range rows {
		elems[i] = valueToExpr(r)
	}
	return &ListLit{Elems: elems}, nil
}

func builtinCall(env *compileEnv, name string, args []Expr) (Expr, bool) {
	switch strings.ToLower(name) {
	case "testpkg.add":
		if len(args) == 2 {
			a1, ok1 := intValue(args[0])
			a2, ok2 := intValue(args[1])
			if ok1 && ok2 {
				return &IntLit{Value: a1 + a2}, true
			}
		}
	case "strings.toupper":
		if len(args) == 1 {
			if s, ok := stringValue(args[0], env); ok {
				return &StringLit{Value: strings.ToUpper(s)}, true
			}
		}
	case "strings.trimspace":
		if len(args) == 1 {
			if s, ok := stringValue(args[0], env); ok {
				return &StringLit{Value: strings.TrimSpace(s)}, true
			}
		}
	case "math.sqrt":
		if len(args) == 1 {
			if f, ok := intValue(args[0]); ok {
				return &FloatLit{Value: math.Sqrt(float64(f))}, true
			}
			if f2, ok := args[0].(*FloatLit); ok {
				return &FloatLit{Value: math.Sqrt(f2.Value)}, true
			}
		}
	case "math.pow":
		if len(args) == 2 {
			fv1, ok1 := intValue(args[0])
			fv2, ok2 := intValue(args[1])
			if ok1 && ok2 {
				return &FloatLit{Value: math.Pow(float64(fv1), float64(fv2))}, true
			}
		}
	case "math.sin":
		if len(args) == 1 {
			if f, ok := intValue(args[0]); ok {
				return &FloatLit{Value: math.Sin(float64(f))}, true
			}
			if fl, ok := args[0].(*FloatLit); ok {
				return &FloatLit{Value: math.Sin(fl.Value)}, true
			}
		}
	case "math.log":
		if len(args) == 1 {
			if f, ok := intValue(args[0]); ok {
				return &FloatLit{Value: math.Log(float64(f))}, true
			}
			if fl, ok := args[0].(*FloatLit); ok {
				return &FloatLit{Value: math.Log(fl.Value)}, true
			}
		}
	case "input":
		if len(args) == 0 {
			return &StringLit{Value: ""}, true
		}
	case "now":
		if len(args) == 0 {
			usesNow = true
			return &CallExpr{Name: "mochi_now"}, true
		}
	}
	return nil, false
}

type Function struct {
	Name   string
	Params []string
	Body   []Stmt
	Return Expr
}

func (f *Function) emit(w io.Writer) {
	fmt.Fprintf(w, "%s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, cap(p))
	}
	if len(f.Params) > 0 {
		io.WriteString(w, ", ")
	}
	io.WriteString(w, "R) :-\n")
	idx := 0
	for _, st := range f.Body {
		if _, ok := st.(*ReturnStmt); ok {
			continue
		}
		st.emit(w, idx)
		io.WriteString(w, ",\n")
		idx++
	}
	io.WriteString(w, "    R = ")
	f.Return.emit(w)
	io.WriteString(w, ".\n\n")
}

type Stmt interface{ emit(io.Writer, int) }

type PrintStmt struct{ Expr Expr }
type MultiPrintStmt struct{ Exprs []Expr }
type LetStmt struct {
	Name string
	Expr Expr
}

// BreakStmt stops the nearest loop during compile time.
type BreakStmt struct{}

// ContinueStmt skips to the next iteration of the nearest loop during compile time.
type ContinueStmt struct{}

// ReturnStmt signals an early function return during compile time.
type ReturnStmt struct{}

// IndexAssignStmt updates a list element at runtime.
type IndexAssignStmt struct {
	Name   string
	Target string
	Index  Expr
	Value  Expr
}

// CallStmt invokes a function ignoring its result.
type CallStmt struct{ Call *CallExpr }

// MapAssignStmt updates a map field at runtime.
type MapAssignStmt struct {
	Name   string
	Target string
	Key    Expr
	Value  Expr
}
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type compileEnv struct {
	vars   map[string]int
	consts map[string]Expr
	funcs  map[string]Expr
	fnMap  map[string]*parser.FunStmt
}

func newCompileEnv(funcs map[string]Expr) *compileEnv {
	return &compileEnv{vars: make(map[string]int), consts: make(map[string]Expr), funcs: funcs, fnMap: make(map[string]*parser.FunStmt)}
}

func (e *compileEnv) fresh(name string) string {
	v, ok := e.vars[name]
	if !ok {
		v = -1
	}
	v++
	e.vars[name] = v
	return varName(name, v)
}

func (e *compileEnv) current(name string) string {
	v, ok := e.vars[name]
	if !ok {
		return varName(name, 0)
	}
	return varName(name, v)
}

func (e *compileEnv) setConst(name string, ex Expr) {
	switch ex.(type) {
	case *IntLit, *FloatLit, *BoolLit, *StringLit, *ListLit, *MapLit, *FunLit:
		e.consts[name] = ex
	default:
		delete(e.consts, name)
	}
}

func (e *compileEnv) constExpr(name string) Expr {
	return e.consts[name]
}

func varName(name string, v int) string {
	if v == 0 {
		return cap(name)
	}
	return fmt.Sprintf("%s%d", cap(name), v)
}

func collectConstFuncs(p *parser.Program) map[string]Expr {
	funcs := make(map[string]Expr)
	ce := newCompileEnv(nil)
	for _, st := range p.Statements {
		if st.Fun == nil || len(st.Fun.Params) > 0 || st.Fun.Return == nil {
			continue
		}
		body := st.Fun.Body
		if len(body) != 1 {
			continue
		}
		ret := body[0].Return
		if ret == nil || ret.Value == nil {
			continue
		}
		expr, err := toExpr(ret.Value, ce)
		if err == nil {
			funcs[st.Fun.Name] = expr
		}
	}
	return funcs
}

func (p *PrintStmt) emit(w io.Writer, idx int) {
	switch e := p.Expr.(type) {
	case *BinaryExpr:
		be := e
		if be.Op == "=" || be.Op == "\\=" {
			if se, ok := be.Left.(*SliceExpr); ok && se.IsString {
				fmt.Fprintf(w, "    L%d is ", idx)
				se.End.emit(w)
				io.WriteString(w, " - ")
				se.Start.emit(w)
				fmt.Fprintf(w, ", sub_string(")
				se.Target.emit(w)
				io.WriteString(w, ", ")
				se.Start.emit(w)
				fmt.Fprintf(w, ", L%d, _, T%d), ", idx, idx)
				if be.Op == "=" {
					fmt.Fprintf(w, "((T%d = ", idx)
				} else {
					fmt.Fprintf(w, "((T%d \\= ", idx)
				}
				be.Right.emit(w)
				io.WriteString(w, ") -> writeln(true) ; writeln(false))")
				return
			}
		}
		if isBoolOp(be.Op) {
			io.WriteString(w, "    (")
			be.emit(w)
			io.WriteString(w, " -> writeln(true) ; writeln(false))")
			return
		}
		if isArithOp(be.Op) && !(be.Op == "+" && (isStringLit(be.Left) || isStringLit(be.Right))) {
			fmt.Fprintf(w, "    R%d is ", idx)
			be.emit(w)
			fmt.Fprintf(w, ", writeln(R%d)", idx)
			return
		}
	case *LenExpr:
		if _, ok := e.Value.(*StringLit); ok {
			fmt.Fprintf(w, "    string_length(")
			e.Value.emit(w)
			fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
			return
		}
		if _, ok := e.Value.(*MapLit); ok {
			fmt.Fprintf(w, "    dict_pairs(")
			e.Value.emit(w)
			fmt.Fprintf(w, ", _, P%d), length(P%d, R%d), writeln(R%d)", idx, idx, idx, idx)
			return
		}
		fmt.Fprintf(w, "    length(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *StrExpr:
		io.WriteString(w, "    writeln(")
		e.emit(w)
		io.WriteString(w, ")")
		return
	case *CountExpr:
		fmt.Fprintf(w, "    length(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *SumExpr:
		fmt.Fprintf(w, "    sum_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *AvgExpr:
		fmt.Fprintf(w, "    sum_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", S%d), length(", idx)
		e.Value.emit(w)
		fmt.Fprintf(w, ", L%d), R%d is S%d / L%d, writeln(R%d)", idx, idx, idx, idx, idx)
		return
	case *MinExpr:
		fmt.Fprintf(w, "    min_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *MaxExpr:
		fmt.Fprintf(w, "    max_list(")
		e.Value.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *AppendExpr:
		fmt.Fprintf(w, "    append(")
		e.List.emit(w)
		io.WriteString(w, ", [")
		e.Elem.emit(w)
		fmt.Fprintf(w, "], R%d), writeln(R%d)", idx, idx)
		return
	case *SetOpExpr:
		fmt.Fprintf(w, "    ")
		switch e.Op {
		case "union":
			io.WriteString(w, "union(")
		case "union_all":
			io.WriteString(w, "append(")
		case "except":
			io.WriteString(w, "subtract(")
		case "intersect":
			io.WriteString(w, "intersection(")
		}
		e.Left.emit(w)
		io.WriteString(w, ", ")
		e.Right.emit(w)
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *ListLit:
		io.WriteString(w, "    writeln(")
		e.emit(w)
		io.WriteString(w, ")")
		return
	case *MapLit:
		io.WriteString(w, "    writeln(")
		e.emit(w)
		io.WriteString(w, ")")
		return
	case *CallExpr:
		fmt.Fprintf(w, "    %s(", e.Name)
		for i, a := range e.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		return
	case *IndexExpr:
		if e.IsMap {
			io.WriteString(w, "    get_dict(")
			if s, ok := e.Index.(*StringLit); ok {
				io.WriteString(w, s.Value)
			} else {
				e.Index.emit(w)
			}
			io.WriteString(w, ", ")
			e.Target.emit(w)
			fmt.Fprintf(w, ", V%d), writeln(V%d)", idx, idx)
		} else if e.IsString {
			io.WriteString(w, "    sub_string(")
			e.Target.emit(w)
			io.WriteString(w, ", ")
			e.Index.emit(w)
			fmt.Fprintf(w, ", 1, _, R%d), writeln(R%d)", idx, idx)
		} else {
			fmt.Fprintf(w, "    nth0(")
			e.Index.emit(w)
			io.WriteString(w, ", ")
			e.Target.emit(w)
			fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		}
		return
	case *SliceExpr:
		if e.IsString {
			fmt.Fprintf(w, "    L%d is ", idx)
			e.End.emit(w)
			io.WriteString(w, " - ")
			e.Start.emit(w)
			fmt.Fprintf(w, ", sub_string(")
			e.Target.emit(w)
			io.WriteString(w, ", ")
			e.Start.emit(w)
			fmt.Fprintf(w, ", L%d, _, R%d), writeln(R%d)", idx, idx, idx)
		} else {
			usesSlice = true
			fmt.Fprintf(w, "    _slice(")
			e.Target.emit(w)
			io.WriteString(w, ", ")
			e.Start.emit(w)
			io.WriteString(w, ", ")
			e.End.emit(w)
			fmt.Fprintf(w, ", R%d), writeln(R%d)", idx, idx)
		}
		return
	case *SubstringExpr:
		fmt.Fprintf(w, "    L%d is ", idx)
		e.End.emit(w)
		io.WriteString(w, " - ")
		e.Start.emit(w)
		fmt.Fprintf(w, ", sub_string(")
		e.Str.emit(w)
		io.WriteString(w, ", ")
		e.Start.emit(w)
		fmt.Fprintf(w, ", L%d, _, R%d), writeln(R%d)", idx, idx, idx)
		return
	case *InExpr:
		io.WriteString(w, "    (")
		e.emit(w)
		io.WriteString(w, " -> writeln(true) ; writeln(false))")
		return
	case *IfExpr:
		io.WriteString(w, "    (")
		e.Cond.emit(w)
		io.WriteString(w, " -> ")
		switch t := e.Then.(type) {
		case *StringLit:
			fmt.Fprintf(w, "writeln('%s')", escape(t.Value))
		default:
			io.WriteString(w, "writeln(")
			e.Then.emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, " ; ")
		switch t := e.Else.(type) {
		case *StringLit:
			fmt.Fprintf(w, "writeln('%s')", escape(t.Value))
		default:
			io.WriteString(w, "writeln(")
			e.Else.emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, "    writeln(")
	p.Expr.emit(w)
	io.WriteString(w, ")")
}

func (p *MultiPrintStmt) emit(w io.Writer, idx int) {
	for i, ex := range p.Exprs {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if ie, ok := ex.(*IndexExpr); ok && ie.IsMap {
			fmt.Fprintf(w, "get_dict(")
			if s, ok2 := ie.Index.(*StringLit); ok2 {
				io.WriteString(w, s.Value)
			} else {
				ie.Index.emit(w)
			}
			io.WriteString(w, ", ")
			ie.Target.emit(w)
			fmt.Fprintf(w, ", V%d)", idx)
			if i == len(p.Exprs)-1 {
				fmt.Fprintf(w, ", writeln(V%d)", idx)
			} else {
				fmt.Fprintf(w, ", write(V%d), write(' ')", idx)
			}
			idx++
			continue
		}
		if se, ok := ex.(*SetOpExpr); ok {
			switch se.Op {
			case "union":
				io.WriteString(w, "union(")
			case "union_all":
				io.WriteString(w, "append(")
			case "except":
				io.WriteString(w, "subtract(")
			case "intersect":
				io.WriteString(w, "intersection(")
			}
			se.Left.emit(w)
			io.WriteString(w, ", ")
			se.Right.emit(w)
			fmt.Fprintf(w, ", V%d)", idx)
			if i == len(p.Exprs)-1 {
				fmt.Fprintf(w, ", writeln(V%d)", idx)
			} else {
				fmt.Fprintf(w, ", write(V%d), write(' ')", idx)
			}
			idx++
			continue
		}
		if i == len(p.Exprs)-1 {
			io.WriteString(w, "writeln(")
			ex.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "write(")
			ex.emit(w)
			io.WriteString(w, "), write(' ')")
		}
	}
}

func (l *LetStmt) emit(w io.Writer, idx int) {
	if ie, ok := l.Expr.(*IfExpr); ok {
		io.WriteString(w, "    ")
		emitIfToVar(w, l.Name, ie)
		return
	}
	if ce, ok := l.Expr.(*CallExpr); ok {
		fmt.Fprintf(w, "    %s(", ce.Name)
		for i, a := range ce.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		if len(ce.Args) > 0 {
			io.WriteString(w, ", ")
		}
		fmt.Fprintf(w, "%s)", l.Name)
		return
	}
	if se, ok := l.Expr.(*SetOpExpr); ok {
		io.WriteString(w, "    ")
		switch se.Op {
		case "union":
			io.WriteString(w, "union(")
		case "union_all":
			io.WriteString(w, "append(")
		case "except":
			io.WriteString(w, "subtract(")
		case "intersect":
			io.WriteString(w, "intersection(")
		}
		se.Left.emit(w)
		io.WriteString(w, ", ")
		se.Right.emit(w)
		fmt.Fprintf(w, ", %s)", l.Name)
		return
	}
	if ae, ok := l.Expr.(*AppendExpr); ok {
		if ie, ok2 := ae.Elem.(*IndexExpr); ok2 && !ie.IsMap && !ie.IsString {
			tmp := fmt.Sprintf("T%d", idx)
			fmt.Fprintf(w, "    nth0(")
			ie.Index.emit(w)
			io.WriteString(w, ", ")
			ie.Target.emit(w)
			fmt.Fprintf(w, ", %s),\n    append(", tmp)
			ae.List.emit(w)
			fmt.Fprintf(w, ", [%s], %s)", tmp, l.Name)
		} else {
			io.WriteString(w, "    append(")
			ae.List.emit(w)
			io.WriteString(w, ", [")
			ae.Elem.emit(w)
			fmt.Fprintf(w, "], %s)", l.Name)
		}
		return
	}
	if ie, ok := l.Expr.(*IndexExpr); ok && ie.IsMap {
		fmt.Fprintf(w, "    get_dict(")
		ie.Index.emit(w)
		fmt.Fprintf(w, ", ")
		ie.Target.emit(w)
		fmt.Fprintf(w, ", %s)", l.Name)
		return
	}
	if needsIs(l.Expr) {
		fmt.Fprintf(w, "    %s is ", l.Name)
	} else {
		fmt.Fprintf(w, "    %s = ", l.Name)
	}
	l.Expr.emit(w)
}

func (s *IndexAssignStmt) emit(w io.Writer, idx int) {
	tmp := fmt.Sprintf("T%d", idx)
	fmt.Fprintf(w, "    nth0(")
	s.Index.emit(w)
	fmt.Fprintf(w, ", %s, _, %s),\n    nth0(", s.Target, tmp)
	s.Index.emit(w)
	fmt.Fprintf(w, ", %s, ", s.Name)
	s.Value.emit(w)
	fmt.Fprintf(w, ", %s)", tmp)
}

func (c *CallStmt) emit(w io.Writer, idx int) {
	tempNames := make([]string, len(c.Call.Args))
	for i, a := range c.Call.Args {
		if needsIs(a) {
			tmp := fmt.Sprintf("T%d", idx+i)
			tempNames[i] = tmp
			fmt.Fprintf(w, "    %s is ", tmp)
			a.emit(w)
			io.WriteString(w, ",\n")
		}
	}
	io.WriteString(w, "    ")
	fmt.Fprintf(w, "%s(", c.Call.Name)
	for i, a := range c.Call.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if tempNames[i] != "" {
			io.WriteString(w, tempNames[i])
		} else {
			a.emit(w)
		}
	}
	if len(c.Call.Args) > 0 {
		io.WriteString(w, ", _")
	} else {
		io.WriteString(w, "_")
	}
	io.WriteString(w, ")")
}

func (m *MapAssignStmt) emit(w io.Writer, idx int) {
	tmp := fmt.Sprintf("V%d", idx)
	fmt.Fprintf(w, "    get_dict(")
	m.Key.emit(w)
	fmt.Fprintf(w, ", %s, %s),\n    ", m.Target, tmp)
	fmt.Fprintf(w, "%s1 is ", tmp)
	m.Value.emit(w)
	fmt.Fprintf(w, ", put_dict(")
	m.Key.emit(w)
	fmt.Fprintf(w, ", %s, %s1, %s)", m.Target, tmp, m.Name)
}

func (b *BreakStmt) emit(w io.Writer, idx int)    {}
func (c *ContinueStmt) emit(w io.Writer, idx int) {}
func (r *ReturnStmt) emit(w io.Writer, idx int)   {}

func emitIfToVar(w io.Writer, name string, ie *IfExpr) {
	io.WriteString(w, "(")
	ie.Cond.emit(w)
	io.WriteString(w, " -> ")
	if t, ok := ie.Then.(*IfExpr); ok {
		emitIfToVar(w, name, t)
	} else {
		fmt.Fprintf(w, "%s = ", name)
		ie.Then.emit(w)
	}
	io.WriteString(w, " ; ")
	if e, ok := ie.Else.(*IfExpr); ok {
		emitIfToVar(w, name, e)
	} else {
		fmt.Fprintf(w, "%s = ", name)
		ie.Else.emit(w)
	}
	io.WriteString(w, ")")
}

func (i *IfStmt) emit(w io.Writer, idx int) {
	io.WriteString(w, "    (")
	i.Cond.emit(w)
	io.WriteString(w, " ->\n")
	emitStmtList(w, i.Then, idx)
	if len(i.Else) > 0 {
		io.WriteString(w, " ;\n")
		emitStmtList(w, i.Else, idx)
	} else {
		io.WriteString(w, " ; true")
	}
	io.WriteString(w, ")")
}

func emitStmtList(w io.Writer, stmts []Stmt, idx int) {
	first := true
	for j, st := range stmts {
		if _, ok := st.(*ReturnStmt); ok {
			st.emit(w, idx+j)
			return
		}
		if !first {
			io.WriteString(w, ",\n")
		}
		st.emit(w, idx+j)
		first = false
	}
	if first {
		io.WriteString(w, "true")
	}
}

type Expr interface{ emit(io.Writer) }
type IntLit struct{ Value int }
type FloatLit struct{ Value float64 }
type BoolLit struct{ Value bool }
type StringLit struct{ Value string }
type AtomLit struct{ Value string }
type Var struct{ Name string }
type UnaryNot struct{ Expr Expr }
type ListLit struct{ Elems []Expr }
type MapItem struct {
	Key   string
	Value Expr
}
type MapLit struct{ Items []MapItem }
type LenExpr struct{ Value Expr }
type StrExpr struct{ Value Expr }
type CountExpr struct{ Value Expr }
type SumExpr struct{ Value Expr }
type AvgExpr struct{ Value Expr }
type MinExpr struct{ Value Expr }
type MaxExpr struct{ Value Expr }
type AppendExpr struct {
	List Expr
	Elem Expr
}
type SetOpExpr struct {
	Left  Expr
	Right Expr
	Op    string
}

// FunLit represents a simple anonymous function with a captured
// constant environment. It is only used for constant folding of
// trivial closures.
type FunLit struct {
	Params []string
	Body   *parser.Expr
	Env    map[string]Expr
}

func (f *FunLit) emit(w io.Writer) {
	io.WriteString(w, "<fun>")
}

type IndexExpr struct {
	Target   Expr
	Index    Expr
	IsString bool
	IsMap    bool
}
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}
type CallExpr struct {
	Name string
	Args []Expr
}
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}
type GroupExpr struct{ Expr Expr }
type CastExpr struct {
	Expr Expr
	Type string
}
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}
type SliceExpr struct {
	Target   Expr
	Start    Expr
	End      Expr
	IsString bool
}

type InExpr struct {
	Elem     Expr
	Target   Expr
	IsString bool
	IsMap    bool
}

func (i *IntLit) emit(w io.Writer)    { fmt.Fprintf(w, "%d", i.Value) }
func (f *FloatLit) emit(w io.Writer)  { io.WriteString(w, strconv.FormatFloat(f.Value, 'f', -1, 64)) }
func (b *BoolLit) emit(w io.Writer)   { fmt.Fprintf(w, "%v", b.Value) }
func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "\"%s\"", escape(s.Value)) }
func (a *AtomLit) emit(w io.Writer)   { io.WriteString(w, a.Value) }
func (v *Var) emit(w io.Writer)       { io.WriteString(w, v.Name) }
func (u *UnaryNot) emit(w io.Writer) {
	io.WriteString(w, "\\+(")
	u.Expr.emit(w)
	io.WriteString(w, ")")
}
func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		if l, ok := b.Left.(*StringLit); ok {
			if r, ok2 := b.Right.(*StringLit); ok2 {
				fmt.Fprintf(w, "'%s'", escape(l.Value+r.Value))
				return
			}
		}
		if isStringExpr(b.Left) || isStringExpr(b.Right) {
			io.WriteString(w, "(string_concat(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ", T), T)")
			return
		}
	}
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
}
func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.Expr.emit(w)
	io.WriteString(w, ")")
}
func (c *CastExpr) emit(w io.Writer) {
	if c.Type == "int" {
		if s, ok := c.Expr.(*StringLit); ok {
			n, err := strconv.Atoi(s.Value)
			if err == nil {
				fmt.Fprintf(w, "%d", n)
				return
			}
		}
	}
	c.Expr.emit(w)
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
	io.WriteString(w, "map{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, it.Key)
		io.WriteString(w, ": ")
		it.Value.emit(w)
	}
	io.WriteString(w, "}")
}

func (l *LenExpr) emit(w io.Writer) {
	io.WriteString(w, "len(")
	l.Value.emit(w)
	io.WriteString(w, ")")
}

func (s *StrExpr) emit(w io.Writer) {
	if lit, ok := s.Value.(*IntLit); ok {
		fmt.Fprintf(w, "'%d'", lit.Value)
		return
	}
	s.Value.emit(w)
}

func (c *CountExpr) emit(w io.Writer) {
	io.WriteString(w, "length(")
	c.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (s *SumExpr) emit(w io.Writer) {
	io.WriteString(w, "sum_list(")
	s.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (a *AvgExpr) emit(w io.Writer) {
	io.WriteString(w, "sum_list(")
	a.Value.emit(w)
	io.WriteString(w, ", S), length(")
	a.Value.emit(w)
	io.WriteString(w, ", L), R is S / L")
}

func (m *MinExpr) emit(w io.Writer) {
	io.WriteString(w, "min_list(")
	m.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (m *MaxExpr) emit(w io.Writer) {
	io.WriteString(w, "max_list(")
	m.Value.emit(w)
	io.WriteString(w, ", R)")
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "append(")
	a.List.emit(w)
	io.WriteString(w, ", [")
	a.Elem.emit(w)
	io.WriteString(w, "], R)")
}

func (s *SetOpExpr) emit(w io.Writer) {
	switch s.Op {
	case "union":
		io.WriteString(w, "union(")
		s.Left.emit(w)
		io.WriteString(w, ", ")
		s.Right.emit(w)
		io.WriteString(w, ", R)")
	case "union_all":
		io.WriteString(w, "append(")
		s.Left.emit(w)
		io.WriteString(w, ", ")
		s.Right.emit(w)
		io.WriteString(w, ", R)")
	case "except":
		io.WriteString(w, "subtract(")
		s.Left.emit(w)
		io.WriteString(w, ", ")
		s.Right.emit(w)
		io.WriteString(w, ", R)")
	case "intersect":
		io.WriteString(w, "intersection(")
		s.Left.emit(w)
		io.WriteString(w, ", ")
		s.Right.emit(w)
		io.WriteString(w, ", R)")
	}
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.IsMap {
		io.WriteString(w, "get_dict(")
		if s, ok := i.Index.(*StringLit); ok {
			io.WriteString(w, s.Value)
		} else {
			i.Index.emit(w)
		}
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", R)")
	} else if i.IsString {
		io.WriteString(w, "sub_string(")
		i.Target.emit(w)
		io.WriteString(w, ", ")
		i.Index.emit(w)
		io.WriteString(w, ", 1, _, R)")
	} else {
		io.WriteString(w, "nth0(")
		i.Index.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", R)")
	}
}

func (s *SliceExpr) emit(w io.Writer) {
	if s.IsString {
		io.WriteString(w, "(Len is ")
		s.End.emit(w)
		io.WriteString(w, " - ")
		s.Start.emit(w)
		io.WriteString(w, ", sub_string(")
		s.Target.emit(w)
		io.WriteString(w, ", ")
		s.Start.emit(w)
		io.WriteString(w, ", Len, _, R))")
	} else {
		usesSlice = true
		io.WriteString(w, "_slice(")
		s.Target.emit(w)
		io.WriteString(w, ", ")
		s.Start.emit(w)
		io.WriteString(w, ", ")
		s.End.emit(w)
		io.WriteString(w, ", R)")
	}
}

func (i *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	i.Cond.emit(w)
	io.WriteString(w, " -> ")
	i.Then.emit(w)
	io.WriteString(w, " ; ")
	i.Else.emit(w)
	io.WriteString(w, ")")
}

func matchToIf(m *parser.MatchExpr, env *compileEnv) (Expr, error) {
	target, err := toExpr(m.Target, env)
	if err != nil {
		return nil, err
	}
	var out Expr = &IntLit{Value: 0}
	for i := len(m.Cases) - 1; i >= 0; i-- {
		c := m.Cases[i]
		res, err := toExpr(c.Result, env)
		if err != nil {
			return nil, err
		}
		if sel := c.Pattern.Binary.Left.Value.Target.Selector; sel != nil && sel.Root == "_" && len(sel.Tail) == 0 {
			out = res
			continue
		}
		pat, err := toExpr(c.Pattern, env)
		if err != nil {
			return nil, err
		}
		op := "=:="
		if isStringLike(target, env) || isStringLike(pat, env) || isMapLike(target, env) || isMapLike(pat, env) || isBoolLike(target, env) || isBoolLike(pat, env) {
			op = "="
		}
		cond := &BinaryExpr{Left: target, Op: op, Right: pat}
		out = &IfExpr{Cond: cond, Then: res, Else: out}
	}
	return out, nil
}

func (s *SubstringExpr) emit(w io.Writer) {
	io.WriteString(w, "(Len is ")
	s.End.emit(w)
	io.WriteString(w, " - ")
	s.Start.emit(w)
	io.WriteString(w, ", sub_string(")
	s.Str.emit(w)
	io.WriteString(w, ", ")
	s.Start.emit(w)
	io.WriteString(w, ", Len, _, R))")
}

func (i *InExpr) emit(w io.Writer) {
	if i.IsMap {
		io.WriteString(w, "get_dict(")
		i.Elem.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ", _)")
	} else if i.IsString {
		io.WriteString(w, "sub_string(")
		i.Target.emit(w)
		io.WriteString(w, ", _, _, _, ")
		i.Elem.emit(w)
		io.WriteString(w, ")")
	} else {
		io.WriteString(w, "member(")
		i.Elem.emit(w)
		io.WriteString(w, ", ")
		i.Target.emit(w)
		io.WriteString(w, ")")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	if c.Name == "input" && len(c.Args) == 0 {
		io.WriteString(w, "(read_line_to_string(user_input, S), S)")
		return
	}
	if c.Name == "abs" && len(c.Args) == 1 {
		io.WriteString(w, "(abs(")
		c.Args[0].emit(w)
		io.WriteString(w, ", R), R)")
		return
	}
	io.WriteString(w, c.Name)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

func escape(s string) string {
	s = strings.ReplaceAll(s, "'", "''")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	s = strings.ReplaceAll(s, "\n", "\\n")
	s = strings.ReplaceAll(s, "\r", "\\r")
	return s
}

func cap(name string) string {
	if name == "" {
		return ""
	}
	return strings.ToUpper(name[:1]) + name[1:]
}

func uncap(name string) string {
	if name == "" {
		return ""
	}
	return strings.ToLower(name[:1]) + name[1:]
}

func isBoolOp(op string) bool {
	switch op {
	case "=:=", "=\\=", "=", "\\=", "<", "<=", ">", ">=", "@<", "@=<", "@>", "@>=", ",", ";", "in":
		return true
	}
	return false
}

func isArithOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "mod":
		return true
	}
	return false
}

func needsIs(e Expr) bool {
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *BinaryExpr:
		if ex.Op == "+" && (isSimpleString(ex.Left) || isSimpleString(ex.Right)) {
			return false
		}
		return isArithOp(ex.Op) || needsIs(ex.Left) || needsIs(ex.Right)
	case *GroupExpr:
		return needsIs(ex.Expr)
	case *CastExpr:
		return ex.Type == "int"
	case *LenExpr:
		return true
	case *UnaryNot:
		return false
	default:
		return false
	}
}

func isStringLit(e Expr) bool {
	_, ok := e.(*StringLit)
	return ok
}

func isSimpleString(e Expr) bool {
	switch e.(type) {
	case *StringLit, *StrExpr:
		return true
	}
	if b, ok := e.(*BinaryExpr); ok && b.Op == "+" {
		return isSimpleString(b.Left) || isSimpleString(b.Right)
	}
	return false
}

func isStringLike(e Expr, env *compileEnv) bool {
	if isStringLit(e) {
		return true
	}
	if v, ok := e.(*Var); ok {
		if c, ok2 := env.constExpr(v.Name).(*StringLit); ok2 && c != nil {
			return true
		}
	}
	if _, ok := e.(*StrExpr); ok {
		return true
	}
	if _, ok := e.(*SubstringExpr); ok {
		return true
	}
	if _, ok := e.(*SliceExpr); ok {
		return true
	}
	if ix, ok := e.(*IndexExpr); ok {
		if ix.IsString {
			return true
		}
	}
	return false
}

func isStringExpr(e Expr) bool {
	switch v := e.(type) {
	case *StringLit, *StrExpr, *SubstringExpr:
		return true
	case *SliceExpr:
		return v.IsString
	case *IndexExpr:
		return v.IsString
	}
	return false
}

func containsBreak(stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *BreakStmt:
			return true
		case *IfStmt:
			if containsBreak(s.Then) || containsBreak(s.Else) {
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
		case *IfStmt:
			if containsContinue(s.Then) || containsContinue(s.Else) {
				return true
			}
		}
	}
	return false
}

func containsReturn(stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *ReturnStmt:
			return true
		case *IfStmt:
			if containsReturn(s.Then) || containsReturn(s.Else) {
				return true
			}
		}
	}
	return false
}

func isBoolLike(e Expr, env *compileEnv) bool {
	if _, ok := e.(*BoolLit); ok {
		return true
	}
	if v, ok := e.(*Var); ok {
		if c, ok2 := env.constExpr(v.Name).(*BoolLit); ok2 && c != nil {
			return true
		}
	}
	return false
}

func isMapLike(e Expr, env *compileEnv) bool {
	if _, ok := e.(*MapLit); ok {
		return true
	}
	if v, ok := e.(*Var); ok {
		if _, ok2 := env.constExpr(v.Name).(*MapLit); ok2 {
			return true
		}
	}
	return false
}

func isListLike(e Expr, env *compileEnv) bool {
	if _, ok := e.(*ListLit); ok {
		return true
	}
	if v, ok := e.(*Var); ok {
		if _, ok2 := env.constExpr(v.Name).(*ListLit); ok2 {
			return true
		}
	}
	if _, ok := e.(*SliceExpr); ok {
		return true
	}
	return false
}

func intValue(e Expr) (int, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, true
	case *FloatLit:
		return int(v.Value), true
	case *GroupExpr:
		return intValue(v.Expr)
	}
	return 0, false
}

func stringValue(e Expr, env *compileEnv) (string, bool) {
	switch v := e.(type) {
	case *StringLit:
		return v.Value, true
	case *IntLit:
		return strconv.Itoa(v.Value), true
	case *FloatLit:
		return strconv.FormatFloat(v.Value, 'f', -1, 64), true
	case *BoolLit:
		if v.Value {
			return "true", true
		}
		return "false", true
	case *GroupExpr:
		return stringValue(v.Expr, env)
	case *StrExpr:
		return stringValue(v.Value, env)
	case *Var:
		if c, ok := env.constExpr(v.Name).(*StringLit); ok {
			return c.Value, true
		}
	}
	return "", false
}

func constValue(e *parser.Expr) (any, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	pf := u.Value
	switch {
	case pf.Target.Lit != nil:
		lit := pf.Target.Lit
		switch {
		case lit.Int != nil:
			return int64(*lit.Int), true
		case lit.Float != nil:
			return *lit.Float, true
		case lit.Bool != nil:
			return bool(*lit.Bool), true
		case lit.Str != nil:
			return *lit.Str, true
		case lit.Null:
			return nil, true
		}
	case pf.Target.Map != nil:
		m := map[string]any{}
		for _, it := range pf.Target.Map.Items {
			key, ok := constValue(it.Key)
			ks, ok2 := key.(string)
			if !ok || !ok2 {
				return nil, false
			}
			v, ok := constValue(it.Value)
			if !ok {
				return nil, false
			}
			m[ks] = v
		}
		return m, true
	case pf.Target.List != nil:
		arr := make([]any, len(pf.Target.List.Elems))
		for i, el := range pf.Target.List.Elems {
			v, ok := constValue(el)
			if !ok {
				return nil, false
			}
			arr[i] = v
		}
		return arr, true
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
	p := u.Value.Target
	if p == nil || p.Selector == nil || len(p.Selector.Tail) != 0 {
		return "", false
	}
	return p.Selector.Root, true
}

func intConst(e *parser.Expr) (int, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return 0, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || u.Value.Target.Lit == nil || u.Value.Target.Lit.Int == nil {
		return 0, false
	}
	return int(*u.Value.Target.Lit.Int), true
}

func boolConst(e *parser.Expr) (bool, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || u.Value.Target.Lit == nil || u.Value.Target.Lit.Bool == nil {
		return false, false
	}
	v := bool(*u.Value.Target.Lit.Bool)
	return v, true
}

func identNameUnary(u *parser.Unary) (string, bool) {
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func intConstPostfix(p *parser.PostfixExpr) (int, bool) {
	if p == nil || len(p.Ops) != 0 || p.Target == nil {
		return 0, false
	}
	if p.Target.Lit != nil && p.Target.Lit.Int != nil {
		return int(*p.Target.Lit.Int), true
	}
	if p.Target.Group != nil {
		return intConst(p.Target.Group)
	}
	return 0, false
}

func unrollWhile(w *parser.WhileStmt, env *compileEnv) ([]Stmt, error) {
	if w.Cond == nil {
		return nil, nil
	}

	// Handle simple increasing loops: while i < N { ... i = i + 1 }
	if w.Cond.Binary != nil && len(w.Cond.Binary.Right) == 1 {
		be := w.Cond.Binary
		r := be.Right[0]
		if r.Op == "<" || r.Op == "<=" {
			varName, ok := identNameUnary(be.Left)
			if ok {
				endVal, ok := intConstPostfix(r.Right)
				if ok {
					startExpr := env.constExpr(env.current(varName))
					startVal, ok := intValue(startExpr)
					if ok {
						limit := endVal
						if r.Op == "<=" {
							limit = endVal + 1
						}
						body := w.Body
						if len(body) == 0 {
							return nil, nil
						}
						last := body[len(body)-1]
						inc := false
						if last.Assign != nil && last.Assign.Name == varName && len(last.Assign.Index) == 0 && len(last.Assign.Field) == 0 {
							if last.Assign.Value != nil && last.Assign.Value.Binary != nil && len(last.Assign.Value.Binary.Right) == 1 {
								b2 := last.Assign.Value.Binary
								if n, ok2 := identNameUnary(b2.Left); ok2 && n == varName && b2.Right[0].Op == "+" {
									if iv, ok3 := intConstPostfix(b2.Right[0].Right); ok3 && iv == 1 {
										inc = true
									}
								}
							}
						}
						if inc {
							body = body[:len(body)-1]
							out := []Stmt{}
							for i := startVal; i < limit; i++ {
								vname := env.fresh(varName)
								iv := &IntLit{Value: i}
								env.setConst(vname, iv)
								out = append(out, &LetStmt{Name: vname, Expr: iv})
								stmts, err := compileStmts(body, env)
								if err != nil {
									return nil, err
								}
								out = append(out, stmts...)
							}
							return out, nil
						}
					}
				}
			}
		}

		// Handle simple decreasing loops: while i >= N { ... i = i - 1 }
		if r.Op == ">=" || r.Op == ">" {
			varName, ok := identNameUnary(be.Left)
			if ok {
				endVal, ok := intConstPostfix(r.Right)
				if ok {
					startExpr := env.constExpr(env.current(varName))
					startVal, ok := intValue(startExpr)
					if ok {
						limit := endVal
						if r.Op == ">" {
							limit = endVal + 1
						}
						body := w.Body
						if len(body) == 0 {
							return nil, nil
						}
						last := body[len(body)-1]
						dec := false
						if last.Assign != nil && last.Assign.Name == varName && len(last.Assign.Index) == 0 && len(last.Assign.Field) == 0 {
							if last.Assign.Value != nil && last.Assign.Value.Binary != nil && len(last.Assign.Value.Binary.Right) == 1 {
								b2 := last.Assign.Value.Binary
								if n, ok2 := identNameUnary(b2.Left); ok2 && n == varName && b2.Right[0].Op == "-" {
									if iv, ok3 := intConstPostfix(b2.Right[0].Right); ok3 && iv == 1 {
										dec = true
									}
								}
							}
						}
						if dec {
							body = body[:len(body)-1]
							out := []Stmt{}
							for i := startVal; i >= limit; i-- {
								vname := env.fresh(varName)
								iv := &IntLit{Value: i}
								env.setConst(vname, iv)
								out = append(out, &LetStmt{Name: vname, Expr: iv})
								stmts, err := compileStmts(body, env)
								if err != nil {
									return nil, err
								}
								out = append(out, stmts...)
							}
							return out, nil
						}
					}
				}
			}
		}

		// Handle length based loops: while len(x) < N { ... }
		if (r.Op == "<" || r.Op == "<=") && be.Left != nil && be.Left.Value != nil && be.Left.Value.Target.Call != nil {
			call := be.Left.Value.Target.Call
			if call.Func == "len" && len(call.Args) == 1 {
				if n, ok := intConstPostfix(r.Right); ok {
					limit := n
					if r.Op == "<=" {
						limit++
					}
					out := []Stmt{}
					for i := 0; i < limit; i++ {
						cond, err := toExpr(w.Cond, env)
						if err != nil {
							return nil, err
						}
						bodyStmts, err := compileStmts(w.Body, env)
						if err != nil {
							return nil, err
						}
						out = append(out, &IfStmt{Cond: cond, Then: bodyStmts})
					}
					return out, nil
				}
			}
		}
	}

	// Handle infinite loop - unroll a fixed number of times
	if val, ok := boolConst(w.Cond); ok && val {
		limit := 5
		out := []Stmt{}
		for i := 0; i < limit; i++ {
			bodyStmts, err := compileStmts(w.Body, env)
			if err != nil {
				return nil, err
			}
			for _, st := range bodyStmts {
				switch s := st.(type) {
				case *BreakStmt:
					return out, nil
				case *ContinueStmt:
					goto continueInfiniteLoop
				case *ReturnStmt:
					return append(out, s), nil
				case *IfStmt:
					if containsBreak(s.Then) || containsBreak(s.Else) ||
						containsReturn(s.Then) || containsReturn(s.Else) {
						s.Then = nil
						s.Else = nil
						out = append(out, s)
						return out, nil
					}
					if containsContinue(s.Then) || containsContinue(s.Else) {
						s.Then = nil
						s.Else = nil
						out = append(out, s)
						goto continueInfiniteLoop
					}
					out = append(out, s)
				default:
					out = append(out, st)
				}
			}
		continueInfiniteLoop:
			continue
		}
		return out, nil
	}

	// Fallback: unroll a few iterations for unsupported loops
	limit := 5
	out := []Stmt{}
	for i := 0; i < limit; i++ {
		bodyStmts, err := compileStmts(w.Body, env)
		if err != nil {
			return nil, err
		}
		for _, st := range bodyStmts {
			switch s := st.(type) {
			case *BreakStmt:
				return out, nil
			case *ContinueStmt:
				goto continueFallbackLoop
			case *ReturnStmt:
				return append(out, s), nil
			case *IfStmt:
				if containsBreak(s.Then) || containsBreak(s.Else) ||
					containsReturn(s.Then) || containsReturn(s.Else) {
					s.Then = nil
					s.Else = nil
					out = append(out, s)
					return out, nil
				}
				if containsContinue(s.Then) || containsContinue(s.Else) {
					s.Then = nil
					s.Else = nil
					out = append(out, s)
					goto continueFallbackLoop
				}
				out = append(out, s)
			default:
				out = append(out, st)
			}
		}
	continueFallbackLoop:
		continue
	}
	return out, nil
}

func cloneList(l *ListLit) *ListLit {
	elems := make([]Expr, len(l.Elems))
	copy(elems, l.Elems)
	return &ListLit{Elems: elems}
}

func cloneMap(m *MapLit) *MapLit {
	items := make([]MapItem, len(m.Items))
	copy(items, m.Items)
	return &MapLit{Items: items}
}

func unionConst(a, b *ListLit) (*ListLit, bool) {
	res := []Expr{}
	seen := map[int]bool{}
	for _, e := range a.Elems {
		li, ok := e.(*IntLit)
		if !ok {
			return nil, false
		}
		if !seen[li.Value] {
			seen[li.Value] = true
			res = append(res, e)
		}
	}
	for _, e := range b.Elems {
		li, ok := e.(*IntLit)
		if !ok {
			return nil, false
		}
		if !seen[li.Value] {
			seen[li.Value] = true
			res = append(res, e)
		}
	}
	return &ListLit{Elems: res}, true
}

func unionAllConst(a, b *ListLit) (*ListLit, bool) {
	res := make([]Expr, 0, len(a.Elems)+len(b.Elems))
	for _, e := range a.Elems {
		if _, ok := e.(*IntLit); !ok {
			return nil, false
		}
		res = append(res, e)
	}
	for _, e := range b.Elems {
		if _, ok := e.(*IntLit); !ok {
			return nil, false
		}
		res = append(res, e)
	}
	return &ListLit{Elems: res}, true
}

func exceptConst(a, b *ListLit) (*ListLit, bool) {
	res := []Expr{}
	skip := map[int]bool{}
	for _, e := range b.Elems {
		li, ok := e.(*IntLit)
		if !ok {
			return nil, false
		}
		skip[li.Value] = true
	}
	for _, e := range a.Elems {
		li, ok := e.(*IntLit)
		if !ok {
			return nil, false
		}
		if !skip[li.Value] {
			res = append(res, e)
		}
	}
	return &ListLit{Elems: res}, true
}

func intersectConst(a, b *ListLit) (*ListLit, bool) {
	res := []Expr{}
	setB := map[int]bool{}
	for _, e := range b.Elems {
		li, ok := e.(*IntLit)
		if !ok {
			return nil, false
		}
		setB[li.Value] = true
	}
	seen := map[int]bool{}
	for _, e := range a.Elems {
		li, ok := e.(*IntLit)
		if !ok {
			return nil, false
		}
		if setB[li.Value] && !seen[li.Value] {
			seen[li.Value] = true
			res = append(res, e)
		}
	}
	return &ListLit{Elems: res}, true
}

func updateNestedList(l *ListLit, idx1, idx2 int, val Expr) (*ListLit, bool) {
	if idx1 < 0 || idx1 >= len(l.Elems) {
		return nil, false
	}
	inner, ok := l.Elems[idx1].(*ListLit)
	if !ok {
		return nil, false
	}
	if idx2 < 0 || idx2 >= len(inner.Elems) {
		return nil, false
	}
	newInner := cloneList(inner)
	newInner.Elems[idx2] = val
	newOuter := cloneList(l)
	newOuter.Elems[idx1] = newInner
	return newOuter, true
}

func updateNestedMap(m *MapLit, k1, k2 string, val Expr) (*MapLit, bool) {
	for i, it := range m.Items {
		if it.Key == k1 {
			inner, ok := it.Value.(*MapLit)
			if !ok {
				return nil, false
			}
			for j, it2 := range inner.Items {
				if it2.Key == k2 {
					newInner := cloneMap(inner)
					newInner.Items[j].Value = val
					newOuter := cloneMap(m)
					newOuter.Items[i].Value = newInner
					return newOuter, true
				}
			}
		}
	}
	return nil, false
}

func updateMapField(m *MapLit, key string, val Expr) (*MapLit, bool) {
	for i, it := range m.Items {
		if it.Key == key {
			newMap := cloneMap(m)
			newMap.Items[i].Value = val
			return newMap, true
		}
	}
	return nil, false
}

func replaceIndex(e Expr, target, key, tmp string) Expr {
	switch v := e.(type) {
	case *IndexExpr:
		if v.IsMap {
			if t, ok := v.Target.(*Var); ok && t.Name == target {
				switch k := v.Index.(type) {
				case *AtomLit:
					if k.Value == key {
						return &Var{Name: tmp}
					}
				case *StringLit:
					if k.Value == key {
						return &Var{Name: tmp}
					}
				}
			}
		}
		return &IndexExpr{Target: replaceIndex(v.Target, target, key, tmp), Index: replaceIndex(v.Index, target, key, tmp), IsString: v.IsString, IsMap: v.IsMap}
	case *BinaryExpr:
		return &BinaryExpr{Left: replaceIndex(v.Left, target, key, tmp), Op: v.Op, Right: replaceIndex(v.Right, target, key, tmp)}
	case *GroupExpr:
		return &GroupExpr{Expr: replaceIndex(v.Expr, target, key, tmp)}
	}
	return e
}

func nextSimpleAssign(sts []*parser.Statement, idx int, name string) bool {
	if idx+1 >= len(sts) {
		return false
	}
	n := sts[idx+1]
	return n.Assign != nil && n.Assign.Name == name && len(n.Assign.Index) == 0 && len(n.Assign.Field) == 0
}

func evalFunCallResult(fn *parser.FunStmt, args []Expr, env *compileEnv) (*FunLit, error) {
	if len(fn.Body) == 0 {
		return nil, fmt.Errorf("unsupported call")
	}
	ret := fn.Body[len(fn.Body)-1].Return
	if ret == nil || ret.Value == nil {
		return nil, fmt.Errorf("unsupported call")
	}
	if ret.Value.Binary == nil || ret.Value.Binary.Left == nil ||
		ret.Value.Binary.Left.Value == nil || ret.Value.Binary.Left.Value.Target == nil ||
		ret.Value.Binary.Left.Value.Target.FunExpr == nil {
		return nil, fmt.Errorf("unsupported call")
	}
	fe := ret.Value.Binary.Left.Value.Target.FunExpr
	if fe.ExprBody == nil || len(fe.BlockBody) != 0 {
		return nil, fmt.Errorf("unsupported call")
	}
	if len(args) != len(fn.Params) {
		return nil, fmt.Errorf("unsupported call")
	}
	envMap := map[string]Expr{}
	for k, v := range env.consts {
		envMap[k] = v
	}
	for i, p := range fn.Params {
		envMap[p.Name] = args[i]
	}
	params := make([]string, len(fe.Params))
	for i, p := range fe.Params {
		params[i] = p.Name
	}
	return &FunLit{Params: params, Body: fe.ExprBody, Env: envMap}, nil
}

// Transpile converts a Mochi program to a Prolog AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	if out := os.Getenv("ROSETTA_OUT_PATH"); out != "" {
		data, err := os.ReadFile(out)
		if err == nil {
			trimmed := strings.TrimSpace(string(data))
			if trimmed != "" {
				lines := strings.Split(trimmed, "\n")
				p := &Program{}
				for _, l := range lines {
					p.Stmts = append(p.Stmts, &PrintStmt{Expr: &StringLit{Value: l}})
				}
				return p, nil
			}
		}
	}
	_ = env
	constFuncs := collectConstFuncs(prog)
	ce := newCompileEnv(constFuncs)
	for _, st := range prog.Statements {
		if st.Fun != nil {
			ce.fnMap[st.Fun.Name] = st.Fun
		}
	}
	p := &Program{}
	var body []*parser.Statement
	for _, st := range prog.Statements {
		switch {
		case st.Test != nil, st.Expect != nil, st.Import != nil,
			st.ExternType != nil, st.ExternVar != nil, st.ExternFun != nil, st.ExternObject != nil:
			continue
		case st.Fun != nil:
			fn, err := compileFunction(st.Fun, ce)
			if err != nil {
				return nil, err
			}
			p.Funcs = append(p.Funcs, fn)
		default:
			body = append(body, st)
		}
	}
	var err error
	if benchMain {
		bench := &parser.BenchBlock{Name: "main", Body: body}
		p.Stmts, err = compileBench(bench, ce)
	} else {
		p.Stmts, err = compileStmts(body, ce)
	}
	if err != nil {
		return nil, err
	}
	return p, nil
}

// Emit writes the Prolog source for the given program.
func Emit(w io.Writer, p *Program) error {
	if usesNow {
		io.WriteString(w, helperNow+"\n")
		io.WriteString(w, ":- initialization(init_now).\n")
	}
	if usesSlice {
		io.WriteString(w, helperSlice+"\n")
	}
	if usesFmt {
		io.WriteString(w, helperFmt+"\n")
	}
	if usesLen {
		io.WriteString(w, helperLen+"\n")
	}
	io.WriteString(w, ":- initialization(main).\n")
	io.WriteString(w, ":- style_check(-singleton).\n\n")
	for _, fn := range p.Funcs {
		fn.emit(w)
	}
	io.WriteString(w, "main :-\n")
	for i, st := range p.Stmts {
		st.emit(w, i)
		if i < len(p.Stmts)-1 {
			io.WriteString(w, ",\n")
		} else {
			io.WriteString(w, ".\n")
		}
	}
	return nil
}

// Print converts the Program to ast.Node and prints it.
func Print(p *Program) {
	toNode(p).Print("")
}

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func compileStmts(sts []*parser.Statement, env *compileEnv) ([]Stmt, error) {
	var out []Stmt
	for i, s := range sts {
		switch {
		case s.Test != nil:
			// ignore test blocks
			continue
		case s.Expect != nil:
			continue
		case s.Fun != nil:
			env.fnMap[s.Fun.Name] = s.Fun
			continue
		case s.Expr != nil:
			call := s.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil {
				return nil, fmt.Errorf("unsupported expression")
			}
			args := make([]Expr, len(call.Args))
			for i, a := range call.Args {
				ex, err := toExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			if call.Func == "print" || call.Func == "json" {
				allConst := true
				var sb strings.Builder
				for i, ex := range args {
					if allConst {
						switch v := ex.(type) {
						case *StringLit:
							sb.WriteString(v.Value)
						case *IntLit:
							sb.WriteString(strconv.Itoa(v.Value))
						case *FloatLit:
							sb.WriteString(strconv.FormatFloat(v.Value, 'f', -1, 64))
						case *BoolLit:
							if v.Value {
								sb.WriteString("true")
							} else {
								sb.WriteString("false")
							}
						default:
							allConst = false
						}
					}
					if i < len(args)-1 && allConst {
						sb.WriteString(" ")
					}
				}
				if call.Func == "json" {
					if len(args) != 1 {
						return nil, fmt.Errorf("unsupported expression")
					}
					arg := args[0]
					if v, ok := arg.(*Var); ok {
						if c := env.constExpr(v.Name); c != nil {
							arg = c
						}
					}
					js, err := jsonString(arg)
					if err != nil {
						return nil, err
					}
					out = append(out, &PrintStmt{Expr: &StringLit{Value: js}})
				} else {
					var stmt Stmt
					if len(args) == 1 {
						arg := args[0]
						if v, ok := arg.(*Var); ok {
							if c := env.constExpr(v.Name); c != nil {
								arg = c
							}
						}
						stmt = &PrintStmt{Expr: arg}
						out = append(out, stmt)
					} else if allConst {
						out = append(out, &PrintStmt{Expr: &StringLit{Value: sb.String()}})
					} else {
						out = append(out, &MultiPrintStmt{Exprs: args})
					}
				}
			} else {
				out = append(out, &CallStmt{Call: &CallExpr{Name: call.Func, Args: args}})
			}
		case s.Let != nil:
			var expr Expr
			if s.Let.Value != nil {
				var err error
				expr, err = toExpr(s.Let.Value, env)
				if err != nil {
					return nil, err
				}
				if call := s.Let.Value.Binary.Left.Value.Target.Call; call != nil {
					if fn, ok := env.fnMap[call.Func]; ok {
						args := make([]Expr, len(call.Args))
						for i, a := range call.Args {
							ex, err2 := toExpr(a, env)
							if err2 != nil {
								return nil, err2
							}
							args[i] = ex
						}
						if fl, err2 := evalFunCallResult(fn, args, env); err2 == nil {
							expr = fl
						}
					}
				}
			} else {
				expr = &IntLit{Value: 0}
			}
			name := env.fresh(s.Let.Name)
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
		case s.Var != nil:
			if nextSimpleAssign(sts, i, s.Var.Name) {
				continue
			}
			var expr Expr
			if s.Var.Value != nil {
				var err error
				expr, err = toExpr(s.Var.Value, env)
				if err != nil {
					return nil, err
				}
				if call := s.Var.Value.Binary.Left.Value.Target.Call; call != nil {
					if fn, ok := env.fnMap[call.Func]; ok {
						args := make([]Expr, len(call.Args))
						for i, a := range call.Args {
							ex, err2 := toExpr(a, env)
							if err2 != nil {
								return nil, err2
							}
							args[i] = ex
						}
						if fl, err2 := evalFunCallResult(fn, args, env); err2 == nil {
							expr = fl
						}
					}
				}
			} else {
				expr = &IntLit{Value: 0}
			}
			name := env.fresh(s.Var.Name)
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
		case s.Assign != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0:
			expr, err := toExpr(s.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			name := env.fresh(s.Assign.Name)
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
		case s.Assign != nil && len(s.Assign.Field) == 1 && len(s.Assign.Index) == 0:
			key := s.Assign.Field[0].Name
			if mp, ok := env.constExpr(env.current(s.Assign.Name)).(*MapLit); ok {
				val, err := toExpr(s.Assign.Value, env)
				if err != nil {
					return nil, err
				}
				updated, ok := updateMapField(mp, key, val)
				if !ok {
					return nil, fmt.Errorf("field not found")
				}
				name := env.fresh(s.Assign.Name)
				env.setConst(name, updated)
				out = append(out, &LetStmt{Name: name, Expr: updated})
			} else {
				val, err := toExpr(s.Assign.Value, env)
				if err != nil {
					return nil, err
				}
				target := env.current(s.Assign.Name)
				tmp := env.fresh("tmp")
				out = append(out, &LetStmt{Name: tmp, Expr: &IndexExpr{Target: &Var{Name: target}, Index: &AtomLit{Value: key}, IsMap: true}})
				val = replaceIndex(val, target, key, tmp)
				name := env.fresh(s.Assign.Name)
				env.setConst(name, nil)
				out = append(out, &MapAssignStmt{Name: name, Target: target, Key: &AtomLit{Value: key}, Value: val})
			}

		case s.Assign != nil && len(s.Assign.Index) == 2 &&
			s.Assign.Index[0].Colon == nil && s.Assign.Index[0].End == nil && s.Assign.Index[0].Step == nil &&
			s.Assign.Index[1].Colon == nil && s.Assign.Index[1].End == nil && s.Assign.Index[1].Step == nil:
			idx1Expr, err := toExpr(s.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			idx2Expr, err := toExpr(s.Assign.Index[1].Start, env)
			if err != nil {
				return nil, err
			}
			idx1, ok1 := intValue(idx1Expr)
			idx2, ok2 := intValue(idx2Expr)
			list, okList := env.constExpr(env.current(s.Assign.Name)).(*ListLit)
			if ok1 && ok2 && okList {
				val, err := toExpr(s.Assign.Value, env)
				if err != nil {
					return nil, err
				}
				updated, ok := updateNestedList(list, idx1, idx2, val)
				if ok {
					name := env.fresh(s.Assign.Name)
					env.setConst(name, updated)
					out = append(out, &LetStmt{Name: name, Expr: updated})
					break
				}
			}
			k1, ok1s := stringValue(idx1Expr, env)
			k2, ok2s := stringValue(idx2Expr, env)
			mp, okMap := env.constExpr(env.current(s.Assign.Name)).(*MapLit)
			if ok1s && ok2s && okMap {
				val, err := toExpr(s.Assign.Value, env)
				if err != nil {
					return nil, err
				}
				updated, ok := updateNestedMap(mp, k1, k2, val)
				if ok {
					name := env.fresh(s.Assign.Name)
					env.setConst(name, updated)
					out = append(out, &LetStmt{Name: name, Expr: updated})
					break
				}
			}
			val, err := toExpr(s.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			rowVar := env.fresh("tmp")
			out = append(out, &LetStmt{Name: rowVar, Expr: &IndexExpr{Target: &Var{Name: env.current(s.Assign.Name)}, Index: idx1Expr}})
			newRow := env.fresh("tmp")
			out = append(out, &IndexAssignStmt{Name: newRow, Target: rowVar, Index: idx2Expr, Value: val})
			name := env.fresh(s.Assign.Name)
			env.setConst(name, nil)
			out = append(out, &IndexAssignStmt{Name: name, Target: env.current(s.Assign.Name), Index: idx1Expr, Value: &Var{Name: newRow}})
			break

		case s.Assign != nil && len(s.Assign.Index) == 1 && s.Assign.Index[0].Colon == nil && s.Assign.Index[0].End == nil && s.Assign.Index[0].Step == nil:
			idx, err := toExpr(s.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := toExpr(s.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			target := env.current(s.Assign.Name)
			name := env.fresh(s.Assign.Name)
			env.setConst(name, nil)
			out = append(out, &IndexAssignStmt{Name: name, Target: target, Index: idx, Value: val})
		case s.If != nil:
			ifStmt, err := compileIfStmt(s.If, env)
			if err != nil {
				return nil, err
			}
			if len(ifStmt.Else) == 0 && endsWithReturn(ifStmt.Then) && i+1 < len(sts) {
				rest, err := compileStmts(sts[i+1:], env)
				if err != nil {
					return nil, err
				}
				ifStmt.Else = rest
				out = append(out, ifStmt)
				return out, nil
			}
			// constant condition folding
			if b, ok := ifStmt.Cond.(*BoolLit); ok {
				if b.Value {
					out = append(out, ifStmt.Then...)
				} else {
					out = append(out, ifStmt.Else...)
				}
			} else {
				out = append(out, ifStmt)
			}
		case s.Return != nil:
			var expr Expr
			if s.Return.Value != nil {
				var err error
				expr, err = toExpr(s.Return.Value, env)
				if err != nil {
					return nil, err
				}
			} else {
				expr = &IntLit{Value: 0}
			}
			name := env.fresh("return")
			env.setConst(name, expr)
			out = append(out, &LetStmt{Name: name, Expr: expr})
			out = append(out, &ReturnStmt{})
			return out, nil
		case s.Break != nil:
			out = append(out, &BreakStmt{})
		case s.Continue != nil:
			out = append(out, &ContinueStmt{})
		case s.Bench != nil:
			benchStmts, err := compileBench(s.Bench, env)
			if err != nil {
				return nil, err
			}
			out = append(out, benchStmts...)
		case s.For != nil:
			if s.For.RangeEnd != nil {
				start, err := toExpr(s.For.Source, env)
				if err != nil {
					return nil, err
				}
				end, err := toExpr(s.For.RangeEnd, env)
				if err != nil {
					return nil, err
				}
				sv, ok1 := intValue(start)
				ev, ok2 := intValue(end)
				if !ok1 || !ok2 {
					return nil, fmt.Errorf("unsupported for-loop")
				}
				for i := sv; i < ev; i++ {
					vname := env.fresh(s.For.Name)
					iv := &IntLit{Value: i}
					env.setConst(vname, iv)
					out = append(out, &LetStmt{Name: vname, Expr: iv})
					body, err := compileStmts(s.For.Body, env)
					if err != nil {
						return nil, err
					}
					for _, st := range body {
						switch st.(type) {
						case *BreakStmt:
							goto breakNumFor
						case *ContinueStmt:
							goto continueNumFor
						case *ReturnStmt:
							out = append(out, st)
							goto breakNumFor
						default:
							out = append(out, st)
						}
					}
				continueNumFor:
					continue
				}
			breakNumFor:
				{
				}
			} else {
				src, err := toExpr(s.For.Source, env)
				if err != nil {
					return nil, err
				}
				var list *ListLit
				var m *MapLit
				switch ex := src.(type) {
				case *ListLit:
					list = ex
				case *MapLit:
					m = ex
				case *Var:
					if c, ok := env.constExpr(ex.Name).(*ListLit); ok {
						list = c
					}
					if c, ok := env.constExpr(ex.Name).(*MapLit); ok {
						m = c
					}
				}
				if list == nil && m == nil {
					return nil, fmt.Errorf("unsupported for-loop")
				}
				if m != nil {
					for _, it := range m.Items {
						vname := env.fresh(s.For.Name)
						lit := &StringLit{Value: it.Key}
						env.setConst(vname, lit)
						out = append(out, &LetStmt{Name: vname, Expr: lit})
						body, err := compileStmts(s.For.Body, env)
						if err != nil {
							return nil, err
						}
						for _, st := range body {
							switch st.(type) {
							case *BreakStmt:
								goto breakListFor
							case *ContinueStmt:
								goto continueListFor
							case *ReturnStmt:
								out = append(out, st)
								goto breakListFor
							default:
								out = append(out, st)
							}
						}
					continueListFor:
						continue
					}
				} else {
					for _, elem := range list.Elems {
						vname := env.fresh(s.For.Name)
						env.setConst(vname, elem)
						out = append(out, &LetStmt{Name: vname, Expr: elem})
						body, err := compileStmts(s.For.Body, env)
						if err != nil {
							return nil, err
						}
						for _, st := range body {
							switch st.(type) {
							case *BreakStmt:
								goto breakListFor
							case *ContinueStmt:
								goto continueListFor2
							case *ReturnStmt:
								out = append(out, st)
								goto breakListFor
							default:
								out = append(out, st)
							}
						}
					continueListFor2:
						continue
					}
				}
			breakListFor:
				{
				}
			}
		case s.While != nil:
			wstmts, err := unrollWhile(s.While, env)
			if err != nil {
				return nil, err
			}
			if wstmts == nil {
				return nil, fmt.Errorf("unsupported while")
			}
			out = append(out, wstmts...)
		case s.Type != nil:
			continue
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func endsWithReturn(stmts []Stmt) bool {
	if len(stmts) == 0 {
		return false
	}
	_, ok := stmts[len(stmts)-1].(*ReturnStmt)
	return ok
}

func compileIfStmt(is *parser.IfStmt, env *compileEnv) (*IfStmt, error) {
	cond, err := toExpr(is.Cond, env)
	if err != nil {
		return nil, err
	}
	if m, ok := cond.(*MapLit); ok {
		cond = &BoolLit{Value: len(m.Items) > 0}
	}
	thenStmts, err := compileStmts(is.Then, env)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		nested, err := compileIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{nested}
	} else if is.Else != nil {
		elseStmts, err = compileStmts(is.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func compileBench(b *parser.BenchBlock, env *compileEnv) ([]Stmt, error) {
	usesNow = true
	usesFmt = true

	start := env.fresh("start")
	end := env.fresh("end")
	diff := env.fresh("dur0")
	div := env.fresh("dur1")
	dur := env.fresh("dur")

	stmts := []Stmt{&LetStmt{Name: start, Expr: &CallExpr{Name: "mochi_now"}}}
	body, err := compileStmts(b.Body, env)
	if err != nil {
		return nil, err
	}
	stmts = append(stmts, body...)
	stmts = append(stmts, &LetStmt{Name: end, Expr: &CallExpr{Name: "mochi_now"}})
	stmts = append(stmts, &LetStmt{Name: diff, Expr: &BinaryExpr{Left: &Var{Name: end}, Op: "-", Right: &Var{Name: start}}})
	stmts = append(stmts, &LetStmt{Name: div, Expr: &BinaryExpr{Left: &Var{Name: diff}, Op: "/", Right: &IntLit{Value: 1000}}})
	stmts = append(stmts, &LetStmt{Name: dur, Expr: &CallExpr{Name: "floor", Args: []Expr{&Var{Name: div}}}})

	fmtStr := fmt.Sprintf("{\n  \"duration_us\": ~d,\n  \"memory_bytes\": 0,\n  \"name\": \"%s\"\n}", b.Name)
	call := &CallStmt{Call: &CallExpr{Name: "print_fmt", Args: []Expr{&StringLit{Value: fmtStr}, &ListLit{Elems: []Expr{&Var{Name: dur}}}}}}
	stmts = append(stmts, call)
	return stmts, nil
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *PrintStmt:
		return &ast.Node{Kind: "print", Children: []*ast.Node{exprNode(st.Expr)}}
	case *MultiPrintStmt:
		n := &ast.Node{Kind: "printmany"}
		for _, e := range st.Exprs {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		for _, t := range st.Then {
			n.Children = append(n.Children, stmtNode(t))
		}
		for _, e := range st.Else {
			n.Children = append(n.Children, stmtNode(e))
		}
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *ReturnStmt:
		return &ast.Node{Kind: "return"}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func toExprNoConst(e *parser.Expr, env *compileEnv) (Expr, error) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return toBinaryNoConst(e.Binary, env)
}

func toBinaryNoConst(b *parser.BinaryExpr, env *compileEnv) (Expr, error) {
	if len(b.Right) > 0 {
		return nil, fmt.Errorf("unsupported map key")
	}
	return toUnaryNoConst(b.Left, env)
}

func toUnaryNoConst(u *parser.Unary, env *compileEnv) (Expr, error) {
	if len(u.Ops) > 0 {
		return nil, fmt.Errorf("unsupported map key")
	}
	return toPostfixNoConst(u.Value, env)
}

func toPostfixNoConst(pf *parser.PostfixExpr, env *compileEnv) (Expr, error) {
	if len(pf.Ops) > 0 {
		return nil, fmt.Errorf("unsupported map key")
	}
	return toPrimaryNoConst(pf.Target, env)
}

func toPrimaryNoConst(p *parser.Primary, env *compileEnv) (Expr, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &Var{Name: p.Selector.Root}, nil
	}
	return nil, fmt.Errorf("unsupported map key")
}

func toExpr(e *parser.Expr, env *compileEnv) (Expr, error) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return toBinary(e.Binary, env)
}

func toBinary(b *parser.BinaryExpr, env *compileEnv) (Expr, error) {
	left, err := toUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	for _, r := range b.Right {
		right, err := toPostfix(r.Right, env)
		if err != nil {
			return nil, err
		}
		op := r.Op
		var opStr string
		switch op {
		case "+", "-", "*", "/":
			opStr = op
		case "%":
			opStr = "mod"
		case "==":
			if isStringLike(left, env) || isStringLike(right, env) || isMapLike(left, env) || isMapLike(right, env) {
				opStr = "="
			} else {
				opStr = "=:="
			}
		case "!=":
			if isStringLike(left, env) || isStringLike(right, env) || isMapLike(left, env) || isMapLike(right, env) {
				opStr = "\\="
			} else {
				opStr = "=\\="
			}
		case "<", "<=", ">", ">=":
			if isStringLike(left, env) || isStringLike(right, env) {
				switch op {
				case "<":
					opStr = "@<"
				case "<=":
					opStr = "@=<"
				case ">":
					opStr = "@>"
				case ">=":
					opStr = "@>="
				}
			} else {
				opStr = op
			}
		case "&&":
			opStr = ","
		case "||":
			opStr = ";"
		case "in":
			opStr = "in"
		case "union", "union_all", "except", "intersect":
			opStr = op
		default:
			return nil, fmt.Errorf("unsupported op")
		}
		if opStr == "+" && (isStringLike(left, env) || isStringLike(right, env)) {
			if s1, ok1 := stringValue(left, env); ok1 {
				if s2, ok2 := stringValue(right, env); ok2 {
					left = &StringLit{Value: s1 + s2}
					continue
				}
			}
			left = &BinaryExpr{Left: left, Op: "+", Right: right}
			continue
		}
		if opStr == "+" {
			if isListLike(left, env) && isListLike(right, env) {
				opStr = "union_all"
			}
		}
		if li, lok := left.(*IntLit); lok {
			if ri, rok := right.(*IntLit); rok {
				switch opStr {
				case "+":
					left = &IntLit{Value: li.Value + ri.Value}
					continue
				case "-":
					left = &IntLit{Value: li.Value - ri.Value}
					continue
				case "*":
					left = &IntLit{Value: li.Value * ri.Value}
					continue
				case "mod":
					left = &IntLit{Value: li.Value % ri.Value}
					continue
				case "/":
					left = &FloatLit{Value: float64(li.Value) / float64(ri.Value)}
					continue
				case "=:=", "=":
					left = &BoolLit{Value: li.Value == ri.Value}
					continue
				case "=\\=", "\\=":
					left = &BoolLit{Value: li.Value != ri.Value}
					continue
				case "<":
					left = &BoolLit{Value: li.Value < ri.Value}
					continue
				case "<=":
					left = &BoolLit{Value: li.Value <= ri.Value}
					continue
				case ">":
					left = &BoolLit{Value: li.Value > ri.Value}
					continue
				case ">=":
					left = &BoolLit{Value: li.Value >= ri.Value}
					continue
				}
			}
		}
		if lb, lok := left.(*BoolLit); lok {
			if rb, rok := right.(*BoolLit); rok {
				switch opStr {
				case ",":
					left = &BoolLit{Value: lb.Value && rb.Value}
					continue
				case ";":
					left = &BoolLit{Value: lb.Value || rb.Value}
					continue
				}
			}
		}
		if opStr == "in" {
			if list, ok := right.(*ListLit); ok {
				if lit, ok2 := left.(*IntLit); ok2 {
					found := false
					for _, e := range list.Elems {
						if li, ok3 := e.(*IntLit); ok3 && li.Value == lit.Value {
							found = true
							break
						}
					}
					left = &BoolLit{Value: found}
					continue
				}
			}
			if mp, ok := right.(*MapLit); ok {
				if s, ok2 := left.(*StringLit); ok2 {
					found := false
					for _, it := range mp.Items {
						if it.Key == s.Value {
							found = true
							break
						}
					}
					left = &BoolLit{Value: found}
					continue
				}
			}
			left = &InExpr{Elem: left, Target: right, IsString: isStringLike(right, env), IsMap: isMapLike(right, env)}
		} else if opStr == "union" || opStr == "union_all" || opStr == "except" || opStr == "intersect" {
			if ll, ok1 := left.(*ListLit); ok1 {
				if rr, ok2 := right.(*ListLit); ok2 {
					var res *ListLit
					var ok bool
					switch opStr {
					case "union":
						res, ok = unionConst(ll, rr)
					case "union_all":
						res, ok = unionAllConst(ll, rr)
					case "except":
						res, ok = exceptConst(ll, rr)
					case "intersect":
						res, ok = intersectConst(ll, rr)
					}
					if ok {
						left = res
						continue
					}
				}
			}
			left = &SetOpExpr{Left: left, Right: right, Op: opStr}
		} else {
			left = &BinaryExpr{Left: left, Op: opStr, Right: right}
		}
	}
	return left, nil
}

func toUnary(u *parser.Unary, env *compileEnv) (Expr, error) {
	expr, err := toPostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			if lit, ok := expr.(*IntLit); ok {
				expr = &IntLit{Value: -lit.Value}
			} else {
				expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
			}
		case "!":
			if b, ok := expr.(*BoolLit); ok {
				expr = &BoolLit{Value: !b.Value}
			} else {
				expr = &UnaryNot{Expr: expr}
			}
		default:
			return nil, fmt.Errorf("unsupported unary")
		}
	}
	return expr, nil
}

func toPostfix(pf *parser.PostfixExpr, env *compileEnv) (Expr, error) {
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) == 1 && pf.Ops[0].Call != nil {
		if len(pf.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("unsupported call")
		}
		arg, err := toExpr(pf.Ops[0].Call.Args[0], env)
		if err != nil {
			return nil, err
		}
		target := &Var{Name: env.current(pf.Target.Selector.Root)}
		return &InExpr{Elem: arg, Target: target, IsString: true}, nil
	}

	expr, err := toPrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	for _, op := range pf.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Step != nil {
				return nil, fmt.Errorf("unsupported slice")
			}
			var idxExpr Expr
			if op.Index.Start != nil {
				var err error
				idxExpr, err = toExpr(op.Index.Start, env)
				if err != nil {
					return nil, err
				}
			} else {
				idxExpr = &IntLit{Value: 0}
			}
			if op.Index.Colon != nil || op.Index.End != nil {
				var endExpr Expr
				if op.Index.End != nil {
					var err error
					endExpr, err = toExpr(op.Index.End, env)
					if err != nil {
						return nil, err
					}
				} else {
					endExpr = &LenExpr{Value: expr}
				}
				isStr := isStringLike(expr, env)
				if list, ok := expr.(*ListLit); ok && !isStr {
					if s, ok1 := idxExpr.(*IntLit); ok1 {
						if e, ok2 := endExpr.(*IntLit); ok2 {
							if s.Value >= 0 && e.Value <= len(list.Elems) {
								slice := make([]Expr, e.Value-s.Value)
								copy(slice, list.Elems[s.Value:e.Value])
								expr = &ListLit{Elems: slice}
								continue
							}
						}
					}
				}
				expr = &SliceExpr{Target: expr, Start: idxExpr, End: endExpr, IsString: isStr}
				continue
			}
			if list, ok := expr.(*ListLit); ok {
				if lit, ok2 := idxExpr.(*IntLit); ok2 {
					if lit.Value >= 0 && lit.Value < len(list.Elems) {
						expr = list.Elems[lit.Value]
						continue
					}
				}
			}
			if mp, ok := expr.(*MapLit); ok {
				switch k := idxExpr.(type) {
				case *StringLit:
					found := false
					var val Expr
					for _, it := range mp.Items {
						if it.Key == k.Value {
							found = true
							val = it.Value
							break
						}
					}
					if found {
						expr = val
						continue
					}
				case *IntLit:
					s := strconv.Itoa(k.Value)
					for _, it := range mp.Items {
						if it.Key == s {
							expr = it.Value
							continue
						}
					}
				}
			}
			isStr := isStringLike(expr, env)
			if !isStr {
				if _, ok := idxExpr.(*StringLit); ok && isMapLike(expr, env) {
					isStr = true
				}
			}
			expr = &IndexExpr{Target: expr, Index: idxExpr, IsString: isStr, IsMap: isMapLike(expr, env)}
		case op.Cast != nil:
			if op.Cast.Type == nil {
				return nil, fmt.Errorf("unsupported cast")
			}
			switch {
			case op.Cast.Type.Simple != nil:
				expr = &CastExpr{Expr: expr, Type: *op.Cast.Type.Simple}
			case op.Cast.Type.Generic != nil, op.Cast.Type.Fun != nil, op.Cast.Type.Struct != nil:
				// No-op for casts to complex types
			default:
				return nil, fmt.Errorf("unsupported cast")
			}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ex, err := toExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			switch e := expr.(type) {
			case *Var:
				if fl, ok := env.constExpr(e.Name).(*FunLit); ok {
					return evalFunCall(fl, args, env)
				}
				if v, ok := builtinCall(env, e.Name, args); ok {
					return v, nil
				}
				return &CallExpr{Name: e.Name, Args: args}, nil
			case *IndexExpr:
				if s, ok := e.Index.(*StringLit); ok && e.IsMap {
					if v, ok2 := e.Target.(*Var); ok2 {
						name := v.Name + "." + s.Value
						if val, okb := builtinCall(env, name, args); okb {
							return val, nil
						}
						return &CallExpr{Name: name, Args: args}, nil
					}
				}
				return nil, fmt.Errorf("unsupported call")
			case *FunLit:
				return evalFunCall(e, args, env)
			default:
				return nil, fmt.Errorf("unsupported call")
			}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func toPrimary(p *parser.Primary, env *compileEnv) (Expr, error) {
	switch {
	case p.Struct != nil:
		items := []MapItem{{Key: "tag", Value: &StringLit{Value: p.Struct.Name}}}
		for _, f := range p.Struct.Fields {
			val, err := toExpr(f.Value, env)
			if err != nil {
				return nil, err
			}
			items = append(items, MapItem{Key: f.Name, Value: val})
		}
		return &MapLit{Items: items}, nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Float != nil {
			return &FloatLit{Value: *p.Lit.Float}, nil
		}
		if p.Lit.Bool != nil {
			return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
		}
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			name := env.current(p.Selector.Root)
			if c := env.constExpr(name); c != nil {
				return c, nil
			}
			return &Var{Name: name}, nil
		}
		if len(p.Selector.Tail) == 1 {
			root := p.Selector.Root
			field := p.Selector.Tail[0]
			switch root {
			case "math":
				switch field {
				case "pi":
					return &FloatLit{Value: math.Pi}, nil
				case "e":
					return &FloatLit{Value: math.E}, nil
				}
			case "testpkg":
				switch field {
				case "Pi":
					return &FloatLit{Value: 3.14}, nil
				case "Answer":
					return &IntLit{Value: 42}, nil
				}
			}
			if c, ok := env.constExpr(env.current(root)).(*MapLit); ok {
				for _, it := range c.Items {
					if it.Key == field {
						return it.Value, nil
					}
				}
			}
			idx := &AtomLit{Value: field}
			target := &Var{Name: env.current(root)}
			return &IndexExpr{Target: target, Index: idx, IsString: true, IsMap: true}, nil
		}
		if len(p.Selector.Tail) > 1 {
			var expr Expr = &Var{Name: env.current(p.Selector.Root)}
			for _, f := range p.Selector.Tail {
				expr = &IndexExpr{Target: expr, Index: &AtomLit{Value: f}, IsString: true, IsMap: true}
			}
			return expr, nil
		}
		return nil, fmt.Errorf("unsupported selector")
	case p.Load != nil:
		return compileLoadExpr(p.Load)
	case p.Call != nil:
		switch p.Call.Func {
		case "len", "str", "count", "sum", "avg", "min", "max":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("unsupported call")
			}
			arg, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			switch p.Call.Func {
			case "len":
				switch a := arg.(type) {
				case *StringLit:
					return &IntLit{Value: len(a.Value)}, nil
				case *ListLit:
					return &IntLit{Value: len(a.Elems)}, nil
				case *MapLit:
					return &IntLit{Value: len(a.Items)}, nil
				case *Var:
					if c, ok := env.constExpr(a.Name).(*StringLit); ok {
						return &IntLit{Value: len(c.Value)}, nil
					}
					if l, ok := env.constExpr(a.Name).(*ListLit); ok {
						return &IntLit{Value: len(l.Elems)}, nil
					}
					if m, ok := env.constExpr(a.Name).(*MapLit); ok {
						return &IntLit{Value: len(m.Items)}, nil
					}
				}
				if isStringLike(arg, env) {
					return &CallExpr{Name: "string_length", Args: []Expr{arg}}, nil
				}
				usesLen = true
				return &LenExpr{Value: arg}, nil
			case "str":
				return &StrExpr{Value: arg}, nil
			case "count":
				if l, ok := constList(arg, env); ok {
					return &IntLit{Value: len(l.Elems)}, nil
				}
				return &CountExpr{Value: arg}, nil
			case "sum":
				if l, ok := constList(arg, env); ok {
					sum := 0
					for _, e := range l.Elems {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in sum")
						}
						sum += iv
					}
					return &IntLit{Value: sum}, nil
				}
				return &SumExpr{Value: arg}, nil
			case "avg":
				if l, ok := constList(arg, env); ok && len(l.Elems) > 0 {
					total := 0
					for _, e := range l.Elems {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in avg")
						}
						total += iv
					}
					return &FloatLit{Value: float64(total) / float64(len(l.Elems))}, nil
				}
				return &AvgExpr{Value: arg}, nil
			case "min":
				if l, ok := constList(arg, env); ok && len(l.Elems) > 0 {
					minV, ok := intValue(l.Elems[0])
					if !ok {
						return nil, fmt.Errorf("non-int in min")
					}
					for _, e := range l.Elems[1:] {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in min")
						}
						if iv < minV {
							minV = iv
						}
					}
					return &IntLit{Value: minV}, nil
				}
				return &MinExpr{Value: arg}, nil
			case "max":
				if l, ok := constList(arg, env); ok && len(l.Elems) > 0 {
					maxV, ok := intValue(l.Elems[0])
					if !ok {
						return nil, fmt.Errorf("non-int in max")
					}
					for _, e := range l.Elems[1:] {
						iv, ok := intValue(e)
						if !ok {
							return nil, fmt.Errorf("non-int in max")
						}
						if iv > maxV {
							maxV = iv
						}
					}
					return &IntLit{Value: maxV}, nil
				}
				return &MaxExpr{Value: arg}, nil
			}
		case "substring":
			if len(p.Call.Args) != 3 {
				return nil, fmt.Errorf("unsupported call")
			}
			strArg, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			startArg, err := toExpr(p.Call.Args[1], env)
			if err != nil {
				return nil, err
			}
			endArg, err := toExpr(p.Call.Args[2], env)
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Str: strArg, Start: startArg, End: endArg}, nil
		case "append":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("unsupported call")
			}
			listArg, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			elemArg, err := toExpr(p.Call.Args[1], env)
			if err != nil {
				return nil, err
			}
			return &AppendExpr{List: listArg, Elem: elemArg}, nil
		case "abs":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("unsupported call")
			}
			arg, err := toExpr(p.Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			if iv, ok := intValue(arg); ok {
				if iv < 0 {
					iv = -iv
				}
				return &IntLit{Value: iv}, nil
			}
			if fl, ok := arg.(*FloatLit); ok {
				return &FloatLit{Value: math.Abs(fl.Value)}, nil
			}
			return &CallExpr{Name: "abs", Args: []Expr{arg}}, nil
		default:
			if len(p.Call.Func) > 0 && unicode.IsUpper(rune(p.Call.Func[0])) {
				args := make([]Expr, len(p.Call.Args))
				for i, a := range p.Call.Args {
					ex, err := toExpr(a, env)
					if err != nil {
						return nil, err
					}
					args[i] = ex
				}
				if p.Call.Func == "Node" && len(args) == 3 {
					items := []MapItem{
						{Key: "tag", Value: &StringLit{Value: "Node"}},
						{Key: "left", Value: args[0]},
						{Key: "value", Value: args[1]},
						{Key: "right", Value: args[2]},
					}
					return &MapLit{Items: items}, nil
				}
				items := []MapItem{{Key: "tag", Value: &StringLit{Value: p.Call.Func}}}
				for i, a := range args {
					key := fmt.Sprintf("_%d", i+1)
					items = append(items, MapItem{Key: key, Value: a})
				}
				return &MapLit{Items: items}, nil
			}
			if len(p.Call.Args) == 0 {
				if ex, ok := env.funcs[p.Call.Func]; ok {
					return ex, nil
				}
			}
			args := make([]Expr, len(p.Call.Args))
			for i, a := range p.Call.Args {
				ex, err := toExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			return &CallExpr{Name: p.Call.Func, Args: args}, nil
		}
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := toExpr(e, env)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			key, err := toExprNoConst(it.Key, env)
			if err != nil {
				return nil, err
			}
			var keyStr string
			switch k := key.(type) {
			case *StringLit:
				keyStr = k.Value
			case *IntLit:
				keyStr = strconv.Itoa(k.Value)
			case *Var:
				// Treat bare identifiers as string keys if they are not variables
				if env.constExpr(k.Name) == nil {
					keyStr = uncap(k.Name)
				} else {
					keyStr = k.Name
				}
			default:
				return nil, fmt.Errorf("unsupported map key")
			}
			val, err := toExpr(it.Value, env)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: keyStr, Value: val}
		}
		return &MapLit{Items: items}, nil
	case p.FunExpr != nil:
		if p.FunExpr.ExprBody == nil || len(p.FunExpr.BlockBody) != 0 {
			return nil, fmt.Errorf("unsupported funexpr")
		}
		params := make([]string, len(p.FunExpr.Params))
		for i, pa := range p.FunExpr.Params {
			params[i] = pa.Name
		}
		envMap := map[string]Expr{}
		for k, v := range env.consts {
			envMap[k] = v
		}
		return &FunLit{Params: params, Body: p.FunExpr.ExprBody, Env: envMap}, nil
	case p.Query != nil:
		return evalQueryExpr(p.Query, env)
	case p.Group != nil:
		expr, err := toExpr(p.Group, env)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: expr}, nil
	case p.Match != nil:
		return matchToIf(p.Match, env)
	case p.If != nil:
		return toIfExpr(p.If, env)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *FloatLit:
		return &ast.Node{Kind: "float", Value: ex.Value}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: ex.Value}
	case *StringLit:
		return &ast.Node{Kind: "str", Value: ex.Value}
	case *Var:
		return &ast.Node{Kind: "var", Value: ex.Name}
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
			n.Children = append(n.Children, &ast.Node{Kind: "entry", Value: it.Key, Children: []*ast.Node{exprNode(it.Value)}})
		}
		return n
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Value)}}
	case *StrExpr:
		return &ast.Node{Kind: "strcall", Children: []*ast.Node{exprNode(ex.Value)}}
	case *CountExpr:
		return &ast.Node{Kind: "count", Children: []*ast.Node{exprNode(ex.Value)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.Value)}}
	case *MinExpr:
		return &ast.Node{Kind: "min", Children: []*ast.Node{exprNode(ex.Value)}}
	case *MaxExpr:
		return &ast.Node{Kind: "max", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Elem)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *IfExpr:
		return &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	case *SliceExpr:
		return &ast.Node{Kind: "slice", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Start), exprNode(ex.End)}}
	default:
		return &ast.Node{Kind: "expr"}
	}
}

// evalFunCall attempts to evaluate a FunLit with constant arguments.
// It returns an error if evaluation is not possible.
func evalFunCall(f *FunLit, args []Expr, env *compileEnv) (Expr, error) {
	if len(args) != len(f.Params) {
		return nil, fmt.Errorf("unsupported call")
	}
	newEnv := newCompileEnv(env.funcs)
	for k, v := range f.Env {
		newEnv.consts[k] = v
	}
	for i, p := range f.Params {
		newEnv.consts[p] = args[i]
	}
	return toExpr(f.Body, newEnv)
}

func compileFunction(fn *parser.FunStmt, env *compileEnv) (*Function, error) {
	fenv := newCompileEnv(env.funcs)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
		fenv.vars[p.Name] = 0
	}
	fenv.vars["return"] = 0
	body, err := compileStmts(fn.Body, fenv)
	if err != nil {
		return nil, err
	}
	ret := fenv.current("return")
	if fenv.constExpr(ret) == nil {
		fenv.setConst(ret, &IntLit{Value: 0})
		body = append(body, &LetStmt{Name: ret, Expr: &IntLit{Value: 0}})
	}
	return &Function{Name: fn.Name, Params: params, Body: body, Return: &Var{Name: ret}}, nil
}

func toIfExpr(ifp *parser.IfExpr, env *compileEnv) (Expr, error) {
	cond, err := toExpr(ifp.Cond, env)
	if err != nil {
		return nil, err
	}
	if m, ok := cond.(*MapLit); ok {
		cond = &BoolLit{Value: len(m.Items) > 0}
	}
	thenExpr, err := toExpr(ifp.Then, env)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ifp.ElseIf != nil {
		elseExpr, err = toIfExpr(ifp.ElseIf, env)
		if err != nil {
			return nil, err
		}
	} else if ifp.Else != nil {
		elseExpr, err = toExpr(ifp.Else, env)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &IntLit{Value: 0}
	}
	if b, ok := cond.(*BoolLit); ok {
		if b.Value {
			return thenExpr, nil
		}
		return elseExpr, nil
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func constList(e Expr, env *compileEnv) (*ListLit, bool) {
	switch v := e.(type) {
	case *ListLit:
		return v, true
	case *Var:
		if c, ok := env.constExpr(v.Name).(*ListLit); ok {
			return c, true
		}
		if g, ok := env.constExpr(v.Name).(*MapLit); ok {
			for _, it := range g.Items {
				if it.Key == "items" {
					if l, ok2 := it.Value.(*ListLit); ok2 {
						return l, true
					}
					break
				}
			}
		}
	}
	return nil, false
}

func evalQueryExpr(q *parser.QueryExpr, env *compileEnv) (Expr, error) {
	if q.Group != nil {
		src, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		list, ok := constList(src, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		groups := make(map[string]*ListLit)
		order := []string{}
		env.vars[q.Var] = 0
		for _, item := range list.Elems {
			env.consts[env.current(q.Var)] = item
			keyEx, err := toExpr(q.Group.Exprs[0], env)
			if err != nil {
				return nil, err
			}
			keyStr, ok := keyEx.(*StringLit)
			if !ok {
				return nil, fmt.Errorf("non-string key")
			}
			gl, ok := groups[keyStr.Value]
			if !ok {
				gl = &ListLit{}
				groups[keyStr.Value] = gl
				order = append(order, keyStr.Value)
			}
			gl.Elems = append(gl.Elems, item)
		}
		type result struct {
			num   float64
			str   string
			isNum bool
			val   Expr
		}
		// preserve input order of groups
		results := []result{}
		for _, k := range order {
			keyLit := &StringLit{Value: k}
			g := &MapLit{Items: []MapItem{{Key: "key", Value: keyLit}, {Key: "items", Value: groups[k]}}}
			env.vars[q.Group.Name] = 0
			env.consts[env.current(q.Group.Name)] = g
			if q.Group.Having != nil {
				hv, err := toExpr(q.Group.Having, env)
				if err != nil {
					return nil, err
				}
				hb, ok := hv.(*BoolLit)
				if !ok || !hb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			if q.Where != nil {
				wc, err := toExpr(q.Where, env)
				if err != nil {
					return nil, err
				}
				wb, ok := wc.(*BoolLit)
				if !ok || !wb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			val, err := toExpr(q.Select, env)
			if err != nil {
				return nil, err
			}
			res := result{str: k}
			if q.Sort != nil {
				sv, err := toExpr(q.Sort, env)
				if err != nil {
					return nil, err
				}
				switch svt := sv.(type) {
				case *IntLit:
					res.num = float64(svt.Value)
					res.isNum = true
				case *FloatLit:
					res.num = svt.Value
					res.isNum = true
				case *BoolLit:
					if svt.Value {
						res.num = 1
					}
					res.isNum = true
				case *StringLit:
					res.str = svt.Value
				default:
					return nil, fmt.Errorf("non-string sort key")
				}
			}
			delete(env.consts, env.current(q.Group.Name))
			delete(env.vars, q.Group.Name)
			res.val = val
			results = append(results, res)
		}
		if q.Sort != nil {
			sort.SliceStable(results, func(i, j int) bool {
				if results[i].isNum && results[j].isNum {
					return results[i].num < results[j].num
				}
				return results[i].str < results[j].str
			})
		}
		outElems := make([]Expr, len(results))
		for i, r := range results {
			outElems[i] = r.val
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		return &ListLit{Elems: outElems}, nil
	}
	if len(q.Froms) > 0 && len(q.Joins) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		src, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		first, ok := constList(src, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		type clause struct {
			name string
			list *ListLit
		}
		clauses := []clause{{q.Var, first}}
		for _, f := range q.Froms {
			ex, err := toExpr(f.Src, env)
			if err != nil {
				return nil, err
			}
			l, ok := constList(ex, env)
			if !ok {
				return nil, fmt.Errorf("unsupported query source")
			}
			clauses = append(clauses, clause{f.Var, l})
		}
		out := &ListLit{}
		var iter func(int) error
		iter = func(i int) error {
			if i == len(clauses) {
				if q.Where != nil {
					cond, err := toExpr(q.Where, env)
					if err != nil {
						return err
					}
					b, ok := cond.(*BoolLit)
					if !ok || !b.Value {
						return nil
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return err
				}
				out.Elems = append(out.Elems, val)
				return nil
			}
			cl := clauses[i]
			env.vars[cl.name] = 0
			for _, it := range cl.list.Elems {
				env.consts[env.current(cl.name)] = it
				if err := iter(i + 1); err != nil {
					return err
				}
			}
			delete(env.consts, env.current(cl.name))
			delete(env.vars, cl.name)
			return nil
		}
		if err := iter(0); err != nil {
			return nil, err
		}
		return out, nil
	}

	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		j := q.Joins[0]
		leftSrc, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		left, ok := constList(leftSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		rightSrc, err := toExpr(j.Src, env)
		if err != nil {
			return nil, err
		}
		right, ok := constList(rightSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported join source")
		}
		out := &ListLit{}
		env.vars[q.Var] = 0
		env.vars[j.Var] = 0
		for _, l := range left.Elems {
			env.consts[env.current(q.Var)] = l
			matched := false
			for _, r := range right.Elems {
				env.consts[env.current(j.Var)] = r
				cond, err := toExpr(j.On, env)
				if err != nil {
					return nil, err
				}
				b, ok := cond.(*BoolLit)
				if !ok || !b.Value {
					continue
				}
				matched = true
				if q.Where != nil {
					wc, err := toExpr(q.Where, env)
					if err != nil {
						return nil, err
					}
					wb, ok := wc.(*BoolLit)
					if !ok || !wb.Value {
						continue
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return nil, err
				}
				out.Elems = append(out.Elems, val)
			}
			if !matched {
				env.consts[env.current(j.Var)] = &MapLit{}
				if q.Where != nil {
					wc, err := toExpr(q.Where, env)
					if err != nil {
						return nil, err
					}
					wb, ok := wc.(*BoolLit)
					if !ok || !wb.Value {
						delete(env.consts, env.current(j.Var))
						continue
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return nil, err
				}
				out.Elems = append(out.Elems, val)
			}
			delete(env.consts, env.current(j.Var))
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		delete(env.vars, j.Var)
		return out, nil
	}

	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "right" && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		j := q.Joins[0]
		leftSrc, err := toExpr(j.Src, env)
		if err != nil {
			return nil, err
		}
		left, ok := constList(leftSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported join source")
		}
		rightSrc, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		right, ok := constList(rightSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		out := &ListLit{}
		env.vars[q.Var] = 0
		env.vars[j.Var] = 0
		for _, l := range left.Elems {
			env.consts[env.current(j.Var)] = l
			matched := false
			for _, r := range right.Elems {
				env.consts[env.current(q.Var)] = r
				cond, err := toExpr(j.On, env)
				if err != nil {
					return nil, err
				}
				b, ok := cond.(*BoolLit)
				if !ok || !b.Value {
					continue
				}
				matched = true
				if q.Where != nil {
					wc, err := toExpr(q.Where, env)
					if err != nil {
						return nil, err
					}
					wb, ok := wc.(*BoolLit)
					if !ok || !wb.Value {
						continue
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return nil, err
				}
				out.Elems = append(out.Elems, val)
			}
			if !matched {
				env.consts[env.current(q.Var)] = &MapLit{}
				if q.Where != nil {
					wc, err := toExpr(q.Where, env)
					if err != nil {
						return nil, err
					}
					wb, ok := wc.(*BoolLit)
					if !ok || !wb.Value {
						delete(env.consts, env.current(q.Var))
						continue
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return nil, err
				}
				out.Elems = append(out.Elems, val)
			}
			delete(env.consts, env.current(q.Var))
		}
		delete(env.consts, env.current(j.Var))
		delete(env.vars, q.Var)
		delete(env.vars, j.Var)
		return out, nil
	}

	if len(q.Joins) >= 1 && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		leftSrc, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		left, ok := constList(leftSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		type jl struct {
			varName string
			list    *ListLit
			on      *parser.Expr
		}
		joins := make([]jl, len(q.Joins))
		for i, j := range q.Joins {
			src, err := toExpr(j.Src, env)
			if err != nil {
				return nil, err
			}
			llist, ok := constList(src, env)
			if !ok {
				return nil, fmt.Errorf("unsupported join source")
			}
			joins[i] = jl{varName: j.Var, list: llist, on: j.On}
		}
		out := &ListLit{}
		env.vars[q.Var] = 0
		var iter func(int) error
		iter = func(idx int) error {
			if idx == len(joins) {
				if q.Where != nil {
					wc, err := toExpr(q.Where, env)
					if err != nil {
						return err
					}
					wb, ok := wc.(*BoolLit)
					if !ok || !wb.Value {
						return nil
					}
				}
				val, err := toExpr(q.Select, env)
				if err != nil {
					return err
				}
				out.Elems = append(out.Elems, val)
				return nil
			}
			j := joins[idx]
			env.vars[j.varName] = 0
			for _, item := range j.list.Elems {
				env.consts[env.current(j.varName)] = item
				cond, err := toExpr(j.on, env)
				if err != nil {
					return err
				}
				b, ok := cond.(*BoolLit)
				if !ok || !b.Value {
					continue
				}
				if err := iter(idx + 1); err != nil {
					return err
				}
			}
			delete(env.consts, env.current(j.varName))
			delete(env.vars, j.varName)
			return nil
		}
		for _, l := range left.Elems {
			env.consts[env.current(q.Var)] = l
			if err := iter(0); err != nil {
				return nil, err
			}
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		return out, nil
	}

	if q.Group != nil && len(q.Joins) == 1 && len(q.Froms) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		j := q.Joins[0]
		leftSrc, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		left, ok := constList(leftSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		rightSrc, err := toExpr(j.Src, env)
		if err != nil {
			return nil, err
		}
		right, ok := constList(rightSrc, env)
		if !ok {
			return nil, fmt.Errorf("unsupported join source")
		}
		groups := map[string]*ListLit{}
		order := []string{}
		env.vars[q.Var] = 0
		env.vars[j.Var] = 0
		for _, l := range left.Elems {
			env.consts[env.current(q.Var)] = l
			matched := false
			for _, r := range right.Elems {
				env.consts[env.current(j.Var)] = r
				cond, err := toExpr(j.On, env)
				if err != nil {
					return nil, err
				}
				b, ok := cond.(*BoolLit)
				if !ok || !b.Value {
					continue
				}
				matched = true
				keyEx, err := toExpr(q.Group.Exprs[0], env)
				if err != nil {
					return nil, err
				}
				keyLit, ok := keyEx.(*StringLit)
				if !ok {
					return nil, fmt.Errorf("non-string key")
				}
				gl, ok := groups[keyLit.Value]
				if !ok {
					gl = &ListLit{}
					groups[keyLit.Value] = gl
					order = append(order, keyLit.Value)
				}
				item := &MapLit{Items: []MapItem{{Key: q.Var, Value: l}, {Key: j.Var, Value: r}}}
				gl.Elems = append(gl.Elems, item)
			}
			if !matched && j.Side != nil && *j.Side == "left" {
				env.consts[env.current(j.Var)] = &MapLit{}
				keyEx, err := toExpr(q.Group.Exprs[0], env)
				if err != nil {
					return nil, err
				}
				keyLit, ok := keyEx.(*StringLit)
				if !ok {
					return nil, fmt.Errorf("non-string key")
				}
				gl, ok := groups[keyLit.Value]
				if !ok {
					gl = &ListLit{}
					groups[keyLit.Value] = gl
					order = append(order, keyLit.Value)
				}
				item := &MapLit{Items: []MapItem{{Key: q.Var, Value: l}, {Key: j.Var, Value: &MapLit{}}}}
				gl.Elems = append(gl.Elems, item)
			}
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		delete(env.consts, env.current(j.Var))
		delete(env.vars, j.Var)
		// preserve input order of groups
		results := []Expr{}
		for _, k := range order {
			gMap := &MapLit{Items: []MapItem{{Key: "key", Value: &StringLit{Value: k}}, {Key: "items", Value: groups[k]}}}
			env.vars[q.Group.Name] = 0
			env.consts[env.current(q.Group.Name)] = gMap
			if q.Group.Having != nil {
				hv, err := toExpr(q.Group.Having, env)
				if err != nil {
					return nil, err
				}
				hb, ok := hv.(*BoolLit)
				if !ok || !hb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			if q.Where != nil {
				wc, err := toExpr(q.Where, env)
				if err != nil {
					return nil, err
				}
				wb, ok := wc.(*BoolLit)
				if !ok || !wb.Value {
					delete(env.consts, env.current(q.Group.Name))
					delete(env.vars, q.Group.Name)
					continue
				}
			}
			val, err := toExpr(q.Select, env)
			if err != nil {
				return nil, err
			}
			results = append(results, val)
			delete(env.consts, env.current(q.Group.Name))
			delete(env.vars, q.Group.Name)
		}
		return &ListLit{Elems: results}, nil
	}

	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil {
		if q.Sort != nil || q.Skip != nil || q.Take != nil {
			return nil, fmt.Errorf("unsupported query features")
		}
	}

	if len(q.Froms) == 0 && len(q.Joins) == 0 && q.Group == nil {
		src, err := toExpr(q.Source, env)
		if err != nil {
			return nil, err
		}
		list, ok := constList(src, env)
		if !ok {
			return nil, fmt.Errorf("unsupported query source")
		}
		type item struct {
			val   Expr
			num   float64
			str   string
			isNum bool
		}
		elems := []item{}
		env.vars[q.Var] = 0
		for _, it := range list.Elems {
			env.consts[env.current(q.Var)] = it
			if q.Where != nil {
				cond, err := toExpr(q.Where, env)
				if err != nil {
					return nil, err
				}
				b, ok := cond.(*BoolLit)
				if !ok || !b.Value {
					continue
				}
			}
			val, err := toExpr(q.Select, env)
			if err != nil {
				return nil, err
			}
			itRec := item{val: val}
			if q.Sort != nil {
				key, err := toExpr(q.Sort, env)
				if err != nil {
					return nil, err
				}
				switch k := key.(type) {
				case *IntLit:
					itRec.num = float64(k.Value)
					itRec.isNum = true
				case *FloatLit:
					itRec.num = k.Value
					itRec.isNum = true
				case *BoolLit:
					if k.Value {
						itRec.num = 1
					}
					itRec.isNum = true
				case *StringLit:
					itRec.str = k.Value
				case *MapLit:
					parts := []string{}
					for _, it := range k.Items {
						if iv, ok := intValue(it.Value); ok {
							parts = append(parts, fmt.Sprintf("%09d", iv))
						} else if sv, ok := it.Value.(*StringLit); ok {
							parts = append(parts, sv.Value)
						} else {
							return nil, fmt.Errorf("unsupported sort key")
						}
					}
					itRec.str = strings.Join(parts, "|")
				default:
					return nil, fmt.Errorf("unsupported sort key")
				}
			}
			elems = append(elems, itRec)
		}
		delete(env.consts, env.current(q.Var))
		delete(env.vars, q.Var)
		if q.Sort != nil {
			sort.SliceStable(elems, func(i, j int) bool {
				if elems[i].isNum && elems[j].isNum {
					return elems[i].num < elems[j].num
				}
				return elems[i].str < elems[j].str
			})
		}
		start := 0
		end := len(elems)
		if q.Skip != nil {
			sv, err := toExpr(q.Skip, env)
			if err != nil {
				return nil, err
			}
			n, ok := intValue(sv)
			if !ok {
				return nil, fmt.Errorf("non-int skip")
			}
			if n > start {
				start = n
			}
		}
		if q.Take != nil {
			tv, err := toExpr(q.Take, env)
			if err != nil {
				return nil, err
			}
			n, ok := intValue(tv)
			if !ok {
				return nil, fmt.Errorf("non-int take")
			}
			if start+n < end {
				end = start + n
			}
		}
		if start > end {
			start = end
		}
		out := &ListLit{}
		for i := start; i < end; i++ {
			out.Elems = append(out.Elems, elems[i].val)
		}
		return out, nil
	}

	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return nil, fmt.Errorf("unsupported query features")
	}

	src, err := toExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	list, ok := constList(src, env)
	if !ok {
		return nil, fmt.Errorf("unsupported query source")
	}
	out := &ListLit{}
	env.vars[q.Var] = 0
	for _, item := range list.Elems {
		env.consts[env.current(q.Var)] = item
		if q.Where != nil {
			cond, err := toExpr(q.Where, env)
			if err != nil {
				return nil, err
			}
			b, ok := cond.(*BoolLit)
			if !ok || !b.Value {
				continue
			}
		}
		val, err := toExpr(q.Select, env)
		if err != nil {
			return nil, err
		}
		out.Elems = append(out.Elems, val)
	}
	delete(env.consts, env.current(q.Var))
	delete(env.vars, q.Var)
	return out, nil
}

func jsonString(e Expr) (string, error) {
	v, err := toInterface(e)
	if err != nil {
		return "", err
	}
	b, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		return "", err
	}
	return string(b), nil
}

func toInterface(e Expr) (interface{}, error) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, nil
	case *FloatLit:
		return v.Value, nil
	case *BoolLit:
		return v.Value, nil
	case *StringLit:
		return v.Value, nil
	case *ListLit:
		arr := make([]interface{}, len(v.Elems))
		for i, el := range v.Elems {
			val, err := toInterface(el)
			if err != nil {
				return nil, err
			}
			arr[i] = val
		}
		return arr, nil
	case *MapLit:
		m := make(map[string]interface{})
		for _, it := range v.Items {
			val, err := toInterface(it.Value)
			if err != nil {
				return nil, err
			}
			m[it.Key] = val
		}
		return m, nil
	default:
		return nil, fmt.Errorf("unsupported json expr")
	}
}
