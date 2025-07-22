//go:build slow

package php

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

var transpileEnv *types.Env
var funcStack [][]string

var builtinNames = map[string]struct{}{
	"print": {}, "len": {}, "substring": {}, "count": {}, "sum": {}, "avg": {},
	"str": {}, "min": {}, "max": {}, "append": {}, "json": {}, "exists": {},
	"values": {}, "load": {}, "save": {}, "now": {}, "input": {},
}
var closureNames = map[string]bool{}
var groupStack []string
var globalNames []string
var importedModules = map[string]struct{}{}

// --- Simple PHP AST ---

type Program struct {
	Env   *types.Env
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

// IfStmt represents a conditional statement with optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if (")
	s.Cond.emit(w)
	io.WriteString(w, ") {\n")
	for _, st := range s.Then {
		io.WriteString(w, "  ")
		st.emit(w)
		io.WriteString(w, ";\n")
	}
	io.WriteString(w, "}")
	if len(s.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range s.Else {
			io.WriteString(w, "  ")
			st.emit(w)
			io.WriteString(w, ";\n")
		}
		io.WriteString(w, "}")
	}
}

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	io.WriteString(w, "while (")
	ws.Cond.emit(w)
	io.WriteString(w, ") {\n")
	for _, st := range ws.Body {
		io.WriteString(w, "  ")
		st.emit(w)
		if _, ok := st.(*IfStmt); ok {
			io.WriteString(w, "\n")
		} else {
			io.WriteString(w, ";\n")
		}
	}
	io.WriteString(w, "}")
}

// ForRangeStmt represents iteration over numeric ranges.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "for ($%s = ", fr.Name)
	fr.Start.emit(w)
	fmt.Fprintf(w, "; $%s < ", fr.Name)
	fr.End.emit(w)
	fmt.Fprintf(w, "; $%s++) {\n", fr.Name)
	for _, st := range fr.Body {
		io.WriteString(w, "  ")
		st.emit(w)
		if _, ok := st.(*IfStmt); ok {
			io.WriteString(w, "\n")
		} else {
			io.WriteString(w, ";\n")
		}
	}
	io.WriteString(w, "}")
}

// ForEachStmt represents foreach iteration over lists or maps.
type ForEachStmt struct {
	Name string
	Expr Expr
	Keys bool
	Body []Stmt
}

func (fe *ForEachStmt) emit(w io.Writer) {
	io.WriteString(w, "foreach (")
	if fe.Keys {
		io.WriteString(w, "array_keys(")
		fe.Expr.emit(w)
		fmt.Fprintf(w, ") as $%s) {\n", fe.Name)
	} else {
		fe.Expr.emit(w)
		fmt.Fprintf(w, " as $%s) {\n", fe.Name)
	}
	for _, st := range fe.Body {
		io.WriteString(w, "  ")
		st.emit(w)
		if _, ok := st.(*IfStmt); ok {
			io.WriteString(w, "\n")
		} else {
			io.WriteString(w, ";\n")
		}
	}
	io.WriteString(w, "}")
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "$%s = ", s.Name)
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "null")
	}
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "$%s = ", s.Name)
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "null")
	}
}

// QueryLetStmt assigns the result of a query expression without wrapping it in
// an anonymous function. It expands the query into nested loops that append
// directly into the target slice.
type QueryLetStmt struct {
	Name  string
	Query *QueryExpr
}

func (s *QueryLetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "$%s = [];%s", s.Name, "\n")
	s.Query.emitInto(w, s.Name, 0)
}

// FuncDecl represents a simple function declaration.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FuncDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "function %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "$%s", p)
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range f.Body {
		fmt.Fprint(w, "  ")
		st.emit(w)
		if _, ok := st.(*IfStmt); ok {
			fmt.Fprint(w, "\n")
		} else {
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprint(w, "}")
}

// ClosureExpr represents an anonymous function with optional captured variables.
type ClosureExpr struct {
	Params []string
	Uses   []string
	Body   []Stmt
}

func (c *ClosureExpr) emit(w io.Writer) {
	fmt.Fprint(w, "function(")
	for i, p := range c.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "$%s", p)
	}
	fmt.Fprint(w, ")")
	if len(c.Uses) > 0 {
		fmt.Fprint(w, " use (")
		for i, u := range c.Uses {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			fmt.Fprintf(w, "$%s", u)
		}
		fmt.Fprint(w, ")")
	}
	fmt.Fprint(w, " {\n")
	for _, st := range c.Body {
		fmt.Fprint(w, "  ")
		st.emit(w)
		if _, ok := st.(*IfStmt); ok {
			fmt.Fprint(w, "\n")
		} else {
			fmt.Fprint(w, ";\n")
		}
	}
	fmt.Fprint(w, "}")
}

// ReturnStmt returns from a function.
type ReturnStmt struct{ Value Expr }

func (r *ReturnStmt) emit(w io.Writer) {
	fmt.Fprint(w, "return")
	if r.Value != nil {
		fmt.Fprint(w, " ")
		r.Value.emit(w)
	}
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "$%s = ", s.Name)
	s.Value.emit(w)
}

// IndexAssignStmt assigns to an element or field of a list or map.
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer) {
	s.Target.emit(w)
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

// BreakStmt represents a break statement inside loops.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break") }

// ContinueStmt represents a continue statement inside loops.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer) { io.WriteString(w, "continue") }

// SaveStmt writes rows into a file or stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

// UpdateStmt represents an `update` statement on a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" {
		if s.Path == "" || s.Path == "-" {
			io.WriteString(w, "foreach (")
			s.Src.emit(w)
			io.WriteString(w, " as $_row) {\n  $j = json_encode($_row);\n  $j = str_replace(\":\", \": \", $j);\n  $j = str_replace(\",\", \", \", $j);\n  echo $j . PHP_EOL;\n}\n")
			return
		}
		p := s.Path
		if strings.HasPrefix(p, "../") {
			clean := strings.TrimPrefix(p, "../")
			root := repoRoot()
			p = filepath.ToSlash(filepath.Join(root, "tests", clean))
		}
		fmt.Fprintf(w, "$__f = fopen(%q, 'w');\n", p)
		io.WriteString(w, "foreach (")
		s.Src.emit(w)
		io.WriteString(w, " as $_row) { $j = json_encode($_row); $j = str_replace(\":\", \": \", $j); $j = str_replace(\",\", \", \", $j); fwrite($__f, $j . PHP_EOL); }\n")
		io.WriteString(w, "fclose($__f);")
	}
}

// LoadExpr loads a YAML file into a list of maps.
type LoadExpr struct {
	Path   string
	Format string
}

func (l *LoadExpr) emit(w io.Writer) {
	switch l.Format {
	case "yaml":
		fmt.Fprintf(w, "(function() { $lines = file(%q, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES); $rows = []; $curr = []; foreach ($lines as $line) { $line = trim($line); if (str_starts_with($line, '-')) { if ($curr) $rows[] = $curr; $curr = []; $line = trim(substr($line, 1)); if ($line !== '') { [$k,$v] = array_map('trim', explode(':', $line, 2)); $curr[$k] = is_numeric($v) ? (int)$v : $v; } } else { [$k,$v] = array_map('trim', explode(':', $line, 2)); $curr[$k] = is_numeric($v) ? (int)$v : $v; } } if ($curr) $rows[] = $curr; return $rows; })()", l.Path)
	case "jsonl":
		fmt.Fprintf(w, "(function() { $rows = []; foreach (file(%q, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) { $line = trim($line); if ($line === '') continue; $rows[] = json_decode($line, true); } return $rows; })()", l.Path)
	case "json":
		fmt.Fprintf(w, "json_decode(file_get_contents(%q), true)", l.Path)
	default:
		fmt.Fprint(w, "[]")
	}
}

func (u *UpdateStmt) emit(w io.Writer) {
	io.WriteString(w, "foreach ($"+u.Target+" as $idx => $item) {\n")
	inner := "  "
	if u.Cond != nil {
		io.WriteString(w, inner+"if (")
		u.Cond.emit(w)
		io.WriteString(w, ") {\n")
		inner += "  "
	}
	for i, f := range u.Fields {
		io.WriteString(w, inner)
		fmt.Fprintf(w, "$item['%s'] = ", f)
		u.Values[i].emit(w)
		io.WriteString(w, ";\n")
	}
	if u.Cond != nil {
		inner = inner[:len(inner)-2]
		io.WriteString(w, inner+"}\n")
	}
	fmt.Fprintf(w, "  $%s[$idx] = $item;\n", u.Target)
	io.WriteString(w, "}")
}

type Expr interface{ emit(io.Writer) }

type CallExpr struct {
	Func string
	Args []Expr
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

// GroupExpr represents a parenthesized sub-expression.
type GroupExpr struct{ X Expr }

// IntDivExpr represents integer division of Left / Right.
type IntDivExpr struct {
	Left  Expr
	Right Expr
}

// SumExpr represents summation of all elements in a list.
type SumExpr struct {
	List Expr
	Uses []string
}

type Var struct{ Name string }

type IntLit struct{ Value int }

type FloatLit struct{ Value float64 }

type BoolLit struct{ Value bool }

type StringLit struct{ Value string }

// Name represents a bare identifier or constant without the '$' prefix.
type Name struct{ Value string }

type UnaryExpr struct {
	Op string
	X  Expr
}

type ListLit struct{ Elems []Expr }

type MapEntry struct {
	Key   Expr
	Value Expr
}

type MapLit struct{ Items []MapEntry }

// SubstringExpr represents substring(str, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

// IndexExpr represents array indexing: x[i].
type IndexExpr struct {
	X     Expr
	Index Expr
}

// SliceExpr represents array slicing: x[start:end].
type SliceExpr struct {
	X     Expr
	Start Expr
	End   Expr
}

// MatchArm represents one arm of a match expression.
type MatchArm struct {
	Pattern Expr // nil for default
	Result  Expr
}

// MatchExpr represents PHP's match expression.
type MatchExpr struct {
	Target Expr
	Arms   []MatchArm
}

type QueryLoop struct {
	Name   string
	Source Expr
}

// LeftJoinLoop is used inside grouped queries to represent a left join clause.
type LeftJoinLoop struct {
	Name   string
	Source Expr
	Cond   Expr
}

type QueryExpr struct {
	Loops  []interface{}
	Where  Expr
	Select Expr
	Uses   []string
}

// GroupByExpr represents a simple group by query without joins or sorting.
type GroupByExpr struct {
	Loops   []interface{}
	Key     Expr
	Name    string
	Where   Expr
	Select  Expr
	Having  Expr
	SortKey bool
	Sort    Expr
	Uses    []string
}

// RightJoinExpr represents a simple right join query with one join clause and
// no additional query modifiers.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
	Uses     []string
}

// LeftJoinExpr represents a simple left join query with one join clause.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
	Uses     []string
}

// OuterJoinExpr represents a full outer join with one join clause.
type OuterJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
	Uses     []string
}

// emitInto expands the query into nested foreach loops and appends the
// selected value into the provided result variable.
func (q *QueryExpr) emitInto(w io.Writer, res string, level int) {
	if len(q.Loops) == 0 {
		return
	}
	var emitLoops func(int, int)
	emitLoops = func(idx int, lvl int) {
		ind := strings.Repeat("  ", lvl)
		if idx >= len(q.Loops) {
			if q.Where != nil {
				fmt.Fprintf(w, "%sif (", ind)
				q.Where.emit(w)
				fmt.Fprint(w, ") {\n")
				lvl++
				ind = strings.Repeat("  ", lvl)
			}
			fmt.Fprintf(w, "%s$%s[] = ", ind, res)
			q.Select.emit(w)
			fmt.Fprint(w, ";\n")
			if q.Where != nil {
				lvl--
				ind = strings.Repeat("  ", lvl)
				fmt.Fprintf(w, "%s}\n", ind)
			}
			return
		}
		loop := q.Loops[idx]
		switch lp := loop.(type) {
		case QueryLoop:
			fmt.Fprintf(w, "%sforeach (", ind)
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", lp.Name)
			emitLoops(idx+1, lvl+1)
			fmt.Fprintf(w, "%s}\n", ind)
		case LeftJoinLoop:
			fmt.Fprintf(w, "%s$matched = false;\n", ind)
			fmt.Fprintf(w, "%sforeach (", ind)
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", lp.Name)
			fmt.Fprintf(w, "%s  if (!(", ind)
			lp.Cond.emit(w)
			fmt.Fprint(w, ")) continue;\n")
			fmt.Fprintf(w, "%s  $matched = true;\n", ind)
			emitLoops(idx+1, lvl+1)
			fmt.Fprintf(w, "%s}\n", ind)
			fmt.Fprintf(w, "%sif (!$matched) {\n", ind)
			fmt.Fprintf(w, "%s  $%s = null;\n", ind, lp.Name)
			emitLoops(idx+1, lvl+1)
			fmt.Fprintf(w, "%s}\n", ind)
		}
	}
	emitLoops(0, level)
}

// CondExpr represents a conditional expression using PHP's ternary operator.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	c.Cond.emit(w)
	io.WriteString(w, " ? ")
	c.Then.emit(w)
	io.WriteString(w, " : ")
	c.Else.emit(w)
	io.WriteString(w, ")")
}

func (m *MatchExpr) emit(w io.Writer) {
	io.WriteString(w, "match(")
	m.Target.emit(w)
	io.WriteString(w, ") {\n")
	for _, a := range m.Arms {
		io.WriteString(w, "    ")
		if a.Pattern != nil {
			a.Pattern.emit(w)
			io.WriteString(w, " => ")
		} else {
			io.WriteString(w, "default => ")
		}
		a.Result.emit(w)
		io.WriteString(w, ",\n")
	}
	io.WriteString(w, "}")
}

func (c *CallExpr) emit(w io.Writer) {
	if c.Func == "echo" {
		io.WriteString(w, "echo ")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, " . \" \" . ")
			}
			a.emit(w)
		}
		io.WriteString(w, ", PHP_EOL")
		return
	}
	fmt.Fprint(w, c.Func)
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

func (q *QueryExpr) emit(w io.Writer) {
	io.WriteString(w, "(function()")
	if len(q.Uses) > 0 {
		io.WriteString(w, " use (")
		for i, u := range q.Uses {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "$%s", u)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, " {\n")
	io.WriteString(w, "  $result = [];\n")
	var emitLoops func(int, int)
	emitLoops = func(idx, level int) {
		indent := strings.Repeat("  ", level)
		if idx >= len(q.Loops) {
			if q.Where != nil {
				io.WriteString(w, indent)
				io.WriteString(w, "if (")
				q.Where.emit(w)
				io.WriteString(w, ") {\n")
				level++
				indent = strings.Repeat("  ", level)
			}
			io.WriteString(w, indent)
			io.WriteString(w, "$result[] = ")
			q.Select.emit(w)
			io.WriteString(w, ";\n")
			if q.Where != nil {
				level--
				indent = strings.Repeat("  ", level)
				io.WriteString(w, indent)
				io.WriteString(w, "}\n")
			}
			return
		}
		loop := q.Loops[idx]
		switch lp := loop.(type) {
		case QueryLoop:
			io.WriteString(w, indent)
			io.WriteString(w, "foreach (")
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", lp.Name)
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
		case LeftJoinLoop:
			io.WriteString(w, indent)
			io.WriteString(w, "$matched = false;\n")
			io.WriteString(w, indent)
			io.WriteString(w, "foreach (")
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", lp.Name)
			io.WriteString(w, indent+"  if (!(")
			lp.Cond.emit(w)
			io.WriteString(w, ")) continue;\n")
			io.WriteString(w, indent+"  $matched = true;\n")
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
			io.WriteString(w, indent)
			io.WriteString(w, "if (!$matched) {\n")
			fmt.Fprintf(w, indent+"  $%s = null;\n", lp.Name)
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
		}
	}
	emitLoops(0, 1)
	io.WriteString(w, "  return $result;\n")
	io.WriteString(w, "})()")
}

func (g *GroupByExpr) emit(w io.Writer) {
	io.WriteString(w, "(function()")
	if len(g.Uses) > 0 {
		io.WriteString(w, " use (")
		for i, u := range g.Uses {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "$%s", u)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, " {\n")
	io.WriteString(w, "  $groups = [];\n")
	var emitLoops func(int, int)
	emitLoops = func(idx, level int) {
		indent := strings.Repeat("  ", level)
		if idx >= len(g.Loops) {
			if g.Where != nil {
				io.WriteString(w, indent)
				io.WriteString(w, "if (")
				g.Where.emit(w)
				io.WriteString(w, ") {\n")
				level++
				indent = strings.Repeat("  ", level)
			}
			io.WriteString(w, indent)
			io.WriteString(w, "$key = ")
			g.Key.emit(w)
			io.WriteString(w, ";\n")
			io.WriteString(w, indent)
			io.WriteString(w, "$k = json_encode($key);\n")
			io.WriteString(w, indent)
			io.WriteString(w, "if (!array_key_exists($k, $groups)) {\n")
			io.WriteString(w, indent)
			io.WriteString(w, "  $groups[$k] = ['key' => $key, 'items' => []];\n")
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
			io.WriteString(w, indent)
			io.WriteString(w, "$groups[$k]['items'][] = ")
			if len(g.Loops) == 1 {
				if ql, ok := g.Loops[0].(QueryLoop); ok {
					fmt.Fprintf(w, "$%s;\n", ql.Name)
				} else if ll, ok := g.Loops[0].(LeftJoinLoop); ok {
					fmt.Fprintf(w, "$%s;\n", ll.Name)
				}
			} else {
				io.WriteString(w, "[")
				for i, lp := range g.Loops {
					if i > 0 {
						io.WriteString(w, ", ")
					}
					switch l := lp.(type) {
					case QueryLoop:
						fmt.Fprintf(w, "'%s' => $%s", l.Name, l.Name)
					case LeftJoinLoop:
						fmt.Fprintf(w, "'%s' => $%s", l.Name, l.Name)
					}
				}
				io.WriteString(w, "];\n")
			}
			if g.Where != nil {
				level--
				indent = strings.Repeat("  ", level)
				io.WriteString(w, indent)
				io.WriteString(w, "}\n")
			}
			return
		}
		loop := g.Loops[idx]
		switch lp := loop.(type) {
		case QueryLoop:
			io.WriteString(w, indent)
			io.WriteString(w, "foreach (")
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", lp.Name)
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
		case LeftJoinLoop:
			io.WriteString(w, indent)
			io.WriteString(w, "$matched = false;\n")
			io.WriteString(w, indent)
			io.WriteString(w, "foreach (")
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", lp.Name)
			io.WriteString(w, indent+"  if (!(")
			lp.Cond.emit(w)
			io.WriteString(w, ")) continue;\n")
			io.WriteString(w, indent+"  $matched = true;\n")
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
			io.WriteString(w, indent)
			io.WriteString(w, "if (!$matched) {\n")
			fmt.Fprintf(w, indent+"  $%s = null;\n", lp.Name)
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
		}
	}
	emitLoops(0, 1)
	if g.SortKey {
		io.WriteString(w, "  ksort($groups);\n")
	}
	io.WriteString(w, "  $result = [];\n")
	io.WriteString(w, "  foreach ($groups as $")
	io.WriteString(w, g.Name)
	io.WriteString(w, ") {\n")
	if g.Having != nil {
		io.WriteString(w, "    if (")
		g.Having.emit(w)
		io.WriteString(w, ") {\n")
	}
	if g.Sort != nil {
		io.WriteString(w, "      $result[] = [")
		g.Sort.emit(w)
		io.WriteString(w, ", ")
		g.Select.emit(w)
		io.WriteString(w, "];\n")
	} else {
		io.WriteString(w, "      $result[] = ")
		g.Select.emit(w)
		io.WriteString(w, ";\n")
	}
	if g.Having != nil {
		io.WriteString(w, "    }\n")
	}
	io.WriteString(w, "  }\n")
	if g.Sort != nil {
		io.WriteString(w, "  usort($result, function($a, $b) { return $a[0] <=> $b[0]; });\n")
		io.WriteString(w, "  $result = array_map(fn($r) => $r[1], $result);\n")
	}
	io.WriteString(w, "  return $result;\n")
	io.WriteString(w, "})()")
}

func (r *RightJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(function()")
	if len(r.Uses) > 0 {
		io.WriteString(w, " use (")
		for i, u := range r.Uses {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "$%s", u)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, " {\n")
	io.WriteString(w, "  $result = [];\n")
	io.WriteString(w, "  foreach (")
	r.RightSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", r.RightVar)
	io.WriteString(w, "    $matched = false;\n")
	io.WriteString(w, "    foreach (")
	r.LeftSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", r.LeftVar)
	io.WriteString(w, "      if (!(")
	r.Cond.emit(w)
	io.WriteString(w, ")) continue;\n")
	io.WriteString(w, "      $matched = true;\n")
	io.WriteString(w, "      $result[] = ")
	r.Select.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if (!$matched) {\n")
	fmt.Fprintf(w, "      $%s = null;\n", r.LeftVar)
	io.WriteString(w, "      $result[] = ")
	r.Select.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "    }\n  }\n  return $result;\n")
	io.WriteString(w, "})()")
}

func (l *LeftJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(function()")
	if len(l.Uses) > 0 {
		io.WriteString(w, " use (")
		for i, u := range l.Uses {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "$%s", u)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, " {\n")
	io.WriteString(w, "  $result = [];\n")
	io.WriteString(w, "  foreach (")
	l.LeftSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", l.LeftVar)
	io.WriteString(w, "    $matched = false;\n")
	io.WriteString(w, "    foreach (")
	l.RightSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", l.RightVar)
	io.WriteString(w, "      if (!(")
	l.Cond.emit(w)
	io.WriteString(w, ")) continue;\n")
	io.WriteString(w, "      $matched = true;\n")
	io.WriteString(w, "      $result[] = ")
	l.Select.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if (!$matched) {\n")
	fmt.Fprintf(w, "      $%s = null;\n", l.RightVar)
	io.WriteString(w, "      $result[] = ")
	l.Select.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "    }\n  }\n  return $result;\n")
	io.WriteString(w, "})()")
}

func (o *OuterJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "(function()")
	if len(o.Uses) > 0 {
		io.WriteString(w, " use (")
		for i, u := range o.Uses {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "$%s", u)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, " {\n")
	io.WriteString(w, "  $result = [];\n")
	io.WriteString(w, "  foreach (")
	o.LeftSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", o.LeftVar)
	io.WriteString(w, "    $matched = false;\n")
	io.WriteString(w, "    foreach (")
	o.RightSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", o.RightVar)
	io.WriteString(w, "      if (!(")
	o.Cond.emit(w)
	io.WriteString(w, ")) continue;\n")
	io.WriteString(w, "      $matched = true;\n")
	io.WriteString(w, "      $result[] = ")
	o.Select.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if (!$matched) {\n")
	fmt.Fprintf(w, "      $%s = null;\n", o.RightVar)
	io.WriteString(w, "      $result[] = ")
	o.Select.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "    }\n  }\n")
	io.WriteString(w, "  foreach (")
	o.RightSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", o.RightVar)
	io.WriteString(w, "    $matched = false;\n")
	io.WriteString(w, "    foreach (")
	o.LeftSrc.emit(w)
	fmt.Fprintf(w, " as $%s) {\n", o.LeftVar)
	io.WriteString(w, "      if (!(")
	o.Cond.emit(w)
	io.WriteString(w, ")) continue;\n")
	io.WriteString(w, "      $matched = true;\n")
	io.WriteString(w, "      break;\n")
	io.WriteString(w, "    }\n")
	io.WriteString(w, "    if (!$matched) {\n")
	fmt.Fprintf(w, "      $%s = null;\n", o.LeftVar)
	io.WriteString(w, "      $result[] = ")
	o.Select.emit(w)
	io.WriteString(w, ";\n")
	io.WriteString(w, "    }\n  }\n  return $result;\n")
	io.WriteString(w, "})()")
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		_, leftStr := b.Left.(*StringLit)
		_, rightStr := b.Right.(*StringLit)
		if leftStr || rightStr {
			b.Left.emit(w)
			fmt.Fprint(w, " . ")
			b.Right.emit(w)
		} else {
			b.Left.emit(w)
			fmt.Fprint(w, " + ")
			b.Right.emit(w)
		}
		return
	} else if b.Op == "in" {
		if isListExpr(b.Right) {
			fmt.Fprint(w, "in_array(")
			b.Left.emit(w)
			fmt.Fprint(w, ", ")
			b.Right.emit(w)
			fmt.Fprint(w, ")")
			return
		}
		if isMapExpr(b.Right) {
			fmt.Fprint(w, "array_key_exists(")
			b.Left.emit(w)
			fmt.Fprint(w, ", ")
			b.Right.emit(w)
			fmt.Fprint(w, ")")
			return
		}
		if isStringExpr(b.Right) {
			fmt.Fprint(w, "str_contains(")
			b.Right.emit(w)
			fmt.Fprint(w, ", ")
			b.Left.emit(w)
			fmt.Fprint(w, ")")
			return
		}
	} else if b.Op == "union" {
		fmt.Fprint(w, "array_values(array_unique(array_merge(")
		b.Left.emit(w)
		fmt.Fprint(w, ", ")
		b.Right.emit(w)
		fmt.Fprint(w, "), SORT_REGULAR))")
		return
	} else if b.Op == "union_all" {
		fmt.Fprint(w, "array_merge(")
		b.Left.emit(w)
		fmt.Fprint(w, ", ")
		b.Right.emit(w)
		fmt.Fprint(w, ")")
		return
	} else if b.Op == "except" {
		fmt.Fprint(w, "array_values(array_diff(")
		b.Left.emit(w)
		fmt.Fprint(w, ", ")
		b.Right.emit(w)
		fmt.Fprint(w, "))")
		return
	} else if b.Op == "intersect" {
		fmt.Fprint(w, "array_values(array_intersect(")
		b.Left.emit(w)
		fmt.Fprint(w, ", ")
		b.Right.emit(w)
		fmt.Fprint(w, "))")
		return
	}
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
}

func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.X.emit(w)
	io.WriteString(w, ")")
}

func (d *IntDivExpr) emit(w io.Writer) {
	io.WriteString(w, "intdiv(")
	d.Left.emit(w)
	io.WriteString(w, ", ")
	d.Right.emit(w)
	io.WriteString(w, ")")
}

func (s *SumExpr) emit(w io.Writer) {
	io.WriteString(w, "(function()")
	if len(s.Uses) > 0 {
		io.WriteString(w, " use (")
		for i, u := range s.Uses {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			fmt.Fprintf(w, "$%s", u)
		}
		io.WriteString(w, ")")
	}
	io.WriteString(w, " { $s = 0; foreach (")
	s.List.emit(w)
	io.WriteString(w, " as $_v) { $s += $_v; } return $s; })()")
}

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	u.X.emit(w)
}

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "[")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "]")
}

func (m *MapLit) emit(w io.Writer) {
	fmt.Fprint(w, "[")
	for i, it := range m.Items {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		it.Key.emit(w)
		fmt.Fprint(w, " => ")
		it.Value.emit(w)
	}
	fmt.Fprint(w, "]")
}

func (s *SubstringExpr) emit(w io.Writer) {
	fmt.Fprint(w, "substr(")
	s.Str.emit(w)
	fmt.Fprint(w, ", ")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
	fmt.Fprint(w, ")")
}

func (i *IndexExpr) emit(w io.Writer) {
	i.X.emit(w)
	fmt.Fprint(w, "[")
	i.Index.emit(w)
	fmt.Fprint(w, "]")
}

func (s *SliceExpr) emit(w io.Writer) {
	fmt.Fprint(w, "array_slice(")
	s.X.emit(w)
	fmt.Fprint(w, ", ")
	s.Start.emit(w)
	fmt.Fprint(w, ", ")
	(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
	fmt.Fprint(w, ")")
}

func (v *Var) emit(w io.Writer) { fmt.Fprintf(w, "$%s", v.Name) }

func (i *IntLit) emit(w io.Writer) { fmt.Fprint(w, i.Value) }

func (f *FloatLit) emit(w io.Writer) {
	s := strconv.FormatFloat(f.Value, 'f', -1, 64)
	if !strings.Contains(s, ".") {
		s += ".0"
	}
	fmt.Fprint(w, s)
}

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		fmt.Fprint(w, "true")
	} else {
		fmt.Fprint(w, "false")
	}
}

func (n *Name) emit(w io.Writer) { io.WriteString(w, n.Value) }

// Emit writes formatted PHP source to w.
func Emit(w io.Writer, p *Program) error {
	transpileEnv = p.Env
	defer func() { transpileEnv = nil }()
	if _, err := io.WriteString(w, "<?php\n"); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		s.emit(w)
		switch s.(type) {
		case *IfStmt, *FuncDecl, *WhileStmt, *ForRangeStmt, *ForEachStmt, *QueryLetStmt:
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	return nil
}

// Transpile converts a Mochi program into our PHP AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	transpileEnv = env
	globalNames = nil
	closureNames = map[string]bool{}
	defer func() { transpileEnv = nil }()
	p := &Program{Env: env}
	for _, st := range prog.Statements {
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if conv != nil {
			p.Stmts = append(p.Stmts, conv)
		}
	}
	return p, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	operands := []Expr{}
	ops := []string{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, p := range b.Right {
		r, err := convertPostfix(p.Right)
		if err != nil {
			return nil, err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, r)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	apply := func(left Expr, op string, right Expr) Expr {
		switch op {
		case "/":
			return &BinaryExpr{Left: left, Op: "/", Right: right}
		case "in":
			if isListExpr(right) {
				return &CallExpr{Func: "in_array", Args: []Expr{left, right}}
			}
			if isStringExpr(right) {
				cmp := &CallExpr{Func: "strpos", Args: []Expr{right, left}}
				return &BinaryExpr{Left: cmp, Op: "!==", Right: &BoolLit{Value: false}}
			}
		}
		return &BinaryExpr{Left: left, Op: op, Right: right}
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					expr := apply(operands[i], ops[i], operands[i+1])
					operands[i] = expr
					operands = append(operands[:i+1], operands[i+2:]...)
					ops = append(ops[:i], ops[i+1:]...)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	x, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			x = &UnaryExpr{Op: op, X: x}
		default:
			return nil, fmt.Errorf("unary op %s not supported", op)
		}
	}
	return x, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	e, err := convertPrimary(pf.Target)
	if err != nil {
		if pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) > 0 {
			e = &Var{Name: pf.Target.Selector.Root}
		} else {
			return nil, err
		}
	}

	tail := []string{}
	if pf.Target != nil && pf.Target.Selector != nil {
		tail = pf.Target.Selector.Tail
	}

	if len(tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		method := tail[0]
		args := make([]Expr, len(pf.Ops[0].Call.Args))
		for i, a := range pf.Ops[0].Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		switch method {
		case "contains":
			if isStringExpr(e) {
				return &CallExpr{Func: "str_contains", Args: append([]Expr{e}, args...)}, nil
			}
			if isListExpr(e) {
				return &CallExpr{Func: "in_array", Args: append(args, e)}, nil
			}
			return nil, fmt.Errorf("contains on unsupported type")
		default:
			if pf.Target != nil && pf.Target.Selector != nil {
				root := pf.Target.Selector.Root
				if _, ok := importedModules[root]; ok {
					call := fmt.Sprintf("$%s['%s']", root, method)
					return &CallExpr{Func: call, Args: args}, nil
				}
			}
			return nil, fmt.Errorf("method %s not supported", method)
		}
	}

	for _, f := range tail {
		e = &IndexExpr{X: e, Index: &StringLit{Value: f}}
	}

	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Cast != nil:
			if op.Cast.Type == nil || op.Cast.Type.Simple == nil {
				return nil, fmt.Errorf("unsupported cast")
			}
			switch *op.Cast.Type.Simple {
			case "int":
				e = &CallExpr{Func: "intval", Args: []Expr{e}}
			case "string":
				e = &CallExpr{Func: "strval", Args: []Expr{e}}
			case "bool":
				e = &CallExpr{Func: "boolval", Args: []Expr{e}}
			default:
				// ignore casts to user types
			}
		case op.Field != nil && op.Field.Name == "contains":
			if i+1 >= len(pf.Ops) || pf.Ops[i+1].Call == nil {
				return nil, fmt.Errorf("method contains requires args")
			}
			args := make([]Expr, len(pf.Ops[i+1].Call.Args))
			for j, a := range pf.Ops[i+1].Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			if isStringExpr(e) {
				e = &CallExpr{Func: "str_contains", Args: append([]Expr{e}, args...)}
			} else if isListExpr(e) {
				e = &CallExpr{Func: "in_array", Args: append(args, e)}
			} else {
				return nil, fmt.Errorf("contains on unsupported type")
			}
			i++
		case op.Field != nil:
			e = &IndexExpr{X: e, Index: &StringLit{Value: op.Field.Name}}
		case op.Index != nil:
			var start Expr = &IntLit{Value: 0}
			if op.Index.Start != nil {
				s, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				start = s
			}
			if op.Index.Colon == nil {
				if op.Index.End != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
					return nil, fmt.Errorf("unsupported index expression")
				}
				if isStringExpr(e) {
					end := &BinaryExpr{Left: start, Op: "+", Right: &IntLit{Value: 1}}
					e = &SubstringExpr{Str: e, Start: start, End: end}
				} else {
					e = &IndexExpr{X: e, Index: start}
				}
			} else {
				if op.Index.End == nil || op.Index.Step != nil || op.Index.Colon2 != nil {
					return nil, fmt.Errorf("unsupported slice expression")
				}
				end, err := convertExpr(op.Index.End)
				if err != nil {
					return nil, err
				}
				if isStringExpr(e) {
					e = &SubstringExpr{Str: e, Start: start, End: end}
				} else {
					e = &SliceExpr{X: e, Start: start, End: end}
				}
			}
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return e, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		callName := name
		if transpileEnv != nil {
			if _, builtin := builtinNames[name]; !builtin {
				if closureNames[name] {
					callName = "$" + name
				}
			}
		}
		if name == "print" {
			callName = "echo"
			for i := range args {
				if isListExpr(args[i]) || isMapExpr(args[i]) || isGroupArg(args[i]) {
					enc := &CallExpr{Func: "json_encode", Args: []Expr{args[i], &IntLit{Value: 1344}}}
					spaced := &CallExpr{Func: "str_replace", Args: []Expr{&StringLit{Value: ","}, &StringLit{Value: ", "}, enc}}
					spaced = &CallExpr{Func: "str_replace", Args: []Expr{&StringLit{Value: ":"}, &StringLit{Value: ": "}, spaced}}
					quoted := &CallExpr{Func: "str_replace", Args: []Expr{&StringLit{Value: "\""}, &StringLit{Value: "'"}, spaced}}
					boolTrue := &CallExpr{Func: "str_replace", Args: []Expr{&StringLit{Value: "true"}, &StringLit{Value: "True"}, quoted}}
					boolFalse := &CallExpr{Func: "str_replace", Args: []Expr{&StringLit{Value: "false"}, &StringLit{Value: "False"}, boolTrue}}
					args[i] = boolFalse
				} else {
					arg := maybeBoolString(args[i])
					if !isStringExpr(arg) {
						arg = maybeFloatString(arg)
					}
					args[i] = arg
				}
			}
		} else if name == "len" {
			if len(args) == 1 {
				if isListArg(args[0]) || isMapArg(args[0]) || isListExpr(args[0]) || isMapExpr(args[0]) {
					callName = "count"
				} else {
					callName = "strlen"
				}
			}
		} else if name == "substring" {
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		} else if name == "count" {
			if len(args) != 1 {
				return nil, fmt.Errorf("count expects 1 arg")
			}
			if isGroupArg(args[0]) {
				arg := &IndexExpr{X: args[0], Index: &StringLit{Value: "items"}}
				return &CallExpr{Func: "count", Args: []Expr{arg}}, nil
			}
			name = "count"
		} else if name == "sum" {
			if len(args) != 1 {
				return nil, fmt.Errorf("sum expects 1 arg")
			}
			if isGroupArg(args[0]) {
				arg := &IndexExpr{X: args[0], Index: &StringLit{Value: "items"}}
				return &CallExpr{Func: "array_sum", Args: []Expr{arg}}, nil
			}
			return &CallExpr{Func: "array_sum", Args: args}, nil
		} else if name == "avg" {
			if len(args) != 1 {
				return nil, fmt.Errorf("avg expects 1 arg")
			}
			var list Expr = args[0]
			if isGroupArg(args[0]) {
				list = &IndexExpr{X: args[0], Index: &StringLit{Value: "items"}}
			}
			frac := &BinaryExpr{Left: &CallExpr{Func: "array_sum", Args: []Expr{list}}, Op: "/", Right: &CallExpr{Func: "count", Args: []Expr{list}}}
			return frac, nil
		} else if name == "str" {
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects 1 arg")
			}
			return &CallExpr{Func: "strval", Args: args}, nil
		} else if name == "min" || name == "max" {
			if len(args) != 1 {
				return nil, fmt.Errorf("%s expects 1 arg", name)
			}
			return &CallExpr{Func: callName, Args: args}, nil
		} else if name == "append" {
			if len(args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			tmp := &ListLit{Elems: []Expr{args[1]}}
			return &CallExpr{Func: "array_merge", Args: []Expr{args[0], tmp}}, nil
		} else if name == "json" {
			if len(args) != 1 {
				return nil, fmt.Errorf("json expects 1 arg")
			}
			pretty := &CallExpr{Func: "json_encode", Args: []Expr{args[0], &IntLit{Value: 128}}}
			inner := &CallExpr{Func: "str_replace", Args: []Expr{&StringLit{Value: "    "}, &StringLit{Value: "  "}, pretty}}
			return &CallExpr{Func: "echo", Args: []Expr{inner}}, nil
		} else if name == "exists" {
			if len(args) != 1 {
				return nil, fmt.Errorf("exists expects 1 arg")
			}
			count := &CallExpr{Func: "count", Args: []Expr{args[0]}}
			return &BinaryExpr{Left: count, Op: ">", Right: &IntLit{Value: 0}}, nil
		} else if name == "values" {
			if len(args) != 1 {
				return nil, fmt.Errorf("values expects 1 arg")
			}
			var target Expr = args[0]
			if isGroupArg(args[0]) {
				target = &IndexExpr{X: args[0], Index: &StringLit{Value: "items"}}
			}
			return &CallExpr{Func: "array_values", Args: []Expr{target}}, nil
		} else if name == "now" {
			if len(args) != 0 {
				return nil, fmt.Errorf("now expects no args")
			}
			return &CallExpr{Func: "hrtime", Args: []Expr{&BoolLit{Value: true}}}, nil
		} else if name == "input" {
			if len(args) != 0 {
				return nil, fmt.Errorf("input expects no args")
			}
			fgets := &CallExpr{Func: "fgets", Args: []Expr{&Name{Value: "STDIN"}}}
			return &CallExpr{Func: "trim", Args: []Expr{fgets}}, nil
		}
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(name); err == nil {
				if ft, ok := t.(types.FuncType); ok && len(args) < len(ft.Params) {
					rem := len(ft.Params) - len(args)
					params := make([]string, rem)
					for i := range params {
						params[i] = fmt.Sprintf("a%d", i)
					}
					fullArgs := append([]Expr{}, args...)
					for _, p := range params {
						fullArgs = append(fullArgs, &Var{Name: p})
					}
					uses := []string{}
					if closureNames[name] {
						uses = append(uses, name)
					}
					body := []Stmt{&ReturnStmt{Value: &CallExpr{Func: callName, Args: fullArgs}}}
					return &ClosureExpr{Params: params, Uses: uses, Body: body}, nil
				}
			}
		}
		return &CallExpr{Func: callName, Args: args}, nil
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		items := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			if v, ok := k.(*Var); ok {
				if transpileEnv != nil {
					if t, err := transpileEnv.GetVar(v.Name); err != nil || isFuncType(t) {
						k = &StringLit{Value: v.Name}
					}
				} else {
					k = &StringLit{Value: v.Name}
				}
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapEntry{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Struct != nil:
		items := make([]MapEntry, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapEntry{Key: &StringLit{Value: f.Name}, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, p2 := range p.FunExpr.Params {
			params[i] = p2.Name
		}
		funcStack = append(funcStack, params)
		var body []Stmt
		if p.FunExpr.ExprBody != nil {
			ex, err := convertExpr(p.FunExpr.ExprBody)
			if err != nil {
				funcStack = funcStack[:len(funcStack)-1]
				return nil, err
			}
			body = []Stmt{&ReturnStmt{Value: ex}}
		} else {
			var err error
			body, err = convertStmtList(p.FunExpr.BlockBody)
			if err != nil {
				funcStack = funcStack[:len(funcStack)-1]
				return nil, err
			}
		}
		funcStack = funcStack[:len(funcStack)-1]
		uses := []string{}
		for _, frame := range funcStack {
			uses = append(uses, frame...)
		}
		return &ClosureExpr{Params: params, Uses: uses, Body: body}, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 {
			return nil, fmt.Errorf("selector tail not supported")
		}
		return &Var{Name: p.Selector.Root}, nil
	case p.Group != nil:
		ex, err := convertExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{X: ex}, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		clean := path
		for strings.HasPrefix(clean, "../") {
			clean = strings.TrimPrefix(clean, "../")
		}
		if path != "" && strings.HasPrefix(path, "../") {
			root := repoRoot()
			path = filepath.ToSlash(filepath.Join(root, "tests", clean))
		}
		return &LoadExpr{Path: path, Format: format}, nil
	default:
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Int != nil:
		return &IntLit{Value: int(*l.Int)}, nil
	case l.Float != nil:
		return &FloatLit{Value: *l.Float}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	}
	return nil, fmt.Errorf("unsupported literal")
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Expr != nil:
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			src, err := convertExpr(se.Src)
			if err != nil {
				return nil, err
			}
			format := parseFormat(se.With)
			path := ""
			if se.Path != nil {
				path = strings.Trim(*se.Path, "\"")
			}
			return &SaveStmt{Src: src, Path: path, Format: format}, nil
		}
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(st.Let.Name); err == nil {
				if _, ok := t.(types.FuncType); ok {
					closureNames[st.Let.Name] = true
				}
			}
		}
		var val Expr
		if st.Let.Value != nil {
			v, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			if q, ok := v.(*QueryExpr); ok {
				if len(funcStack) == 0 {
					globalNames = append(globalNames, st.Let.Name)
				}
				return &QueryLetStmt{Name: st.Let.Name, Query: q}, nil
			}
			val = v
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil {
			switch *st.Let.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "bigint":
				val = &IntLit{Value: 0}
			case "bool":
				val = &BoolLit{Value: false}
			case "string":
				val = &StringLit{Value: ""}
			}
		}
		if transpileEnv != nil {
			var typ types.Type
			if st.Let.Type != nil {
				typ = simpleResolveType(st.Let.Type, transpileEnv)
			} else if st.Let.Value != nil {
				typ = types.ExprType(st.Let.Value, transpileEnv)
			}
			if typ != nil {
				transpileEnv.SetVar(st.Let.Name, typ, false)
			}
		}
		if len(funcStack) == 0 {
			globalNames = append(globalNames, st.Let.Name)
		}
		return &LetStmt{Name: st.Let.Name, Value: val}, nil
	case st.Var != nil:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(st.Var.Name); err == nil {
				if _, ok := t.(types.FuncType); ok {
					closureNames[st.Var.Name] = true
				}
			}
		}
		var val Expr
		if st.Var.Value != nil {
			v, err := convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			val = v
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil {
			switch *st.Var.Type.Simple {
			case "int":
				val = &IntLit{Value: 0}
			case "bigint":
				val = &IntLit{Value: 0}
			case "bool":
				val = &BoolLit{Value: false}
			case "string":
				val = &StringLit{Value: ""}
			}
		}
		if transpileEnv != nil {
			var typ types.Type
			if st.Var.Type != nil {
				typ = simpleResolveType(st.Var.Type, transpileEnv)
			} else if st.Var.Value != nil {
				typ = types.ExprType(st.Var.Value, transpileEnv)
			}
			if typ != nil {
				transpileEnv.SetVar(st.Var.Name, typ, true)
			}
		}
		if len(funcStack) == 0 {
			globalNames = append(globalNames, st.Var.Name)
		}
		return &VarStmt{Name: st.Var.Name, Value: val}, nil
	case st.Assign != nil:
		val, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			return &AssignStmt{Name: st.Assign.Name, Value: val}, nil
		}
		target, err := buildAssignTarget(st.Assign.Name, st.Assign.Index, st.Assign.Field)
		if err != nil {
			return nil, err
		}
		return &IndexAssignStmt{Target: target, Value: val}, nil
	case st.Update != nil:
		up, err := convertUpdate(st.Update)
		if err != nil {
			return nil, err
		}
		return up, nil
	case st.Return != nil:
		var val Expr
		if st.Return.Value != nil {
			v, err := convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
			val = v
		}
		return &ReturnStmt{Value: val}, nil
	case st.Fun != nil:
		topLevel := len(funcStack) == 0
		if !topLevel {
			closureNames[st.Fun.Name] = true
		}
		params := make([]string, len(st.Fun.Params))
		savedEnv := transpileEnv
		childEnv := transpileEnv
		if transpileEnv != nil {
			childEnv = types.NewEnv(transpileEnv)
			for i, p := range st.Fun.Params {
				params[i] = p.Name
				childEnv.SetVar(p.Name, simpleResolveType(p.Type, transpileEnv), true)
			}
			transpileEnv = childEnv
		} else {
			for i, p := range st.Fun.Params {
				params[i] = p.Name
			}
		}
		funcStack = append(funcStack, params)
		wasTop := topLevel
		body, err := convertStmtList(st.Fun.Body)
		funcStack = funcStack[:len(funcStack)-1]
		if transpileEnv != nil {
			transpileEnv = savedEnv
		}
		if err != nil {
			return nil, err
		}
		uses := []string{}
		for _, frame := range funcStack {
			uses = append(uses, frame...)
		}
		if len(funcStack) == 0 {
			uses = append(uses, globalNames...)
		}
		if wasTop {
			decl := &FuncDecl{Name: st.Fun.Name, Params: params, Body: body}
			globalNames = append(globalNames, st.Fun.Name)
			return decl, nil
		}
		clo := &ClosureExpr{Params: params, Uses: uses, Body: body}
		if len(funcStack) == 0 {
			globalNames = append(globalNames, st.Fun.Name)
		}
		return &LetStmt{Name: st.Fun.Name, Value: clo}, nil
	case st.While != nil:
		cond, err := convertExpr(st.While.Cond)
		if err != nil {
			return nil, err
		}
		body, err := convertStmtList(st.While.Body)
		if err != nil {
			return nil, err
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case st.For != nil:
		savedEnv := transpileEnv
		if transpileEnv != nil {
			child := types.NewEnv(transpileEnv)
			if st.For.RangeEnd != nil {
				child.SetVar(st.For.Name, types.IntType{}, true)
			} else {
				t := types.ExprType(st.For.Source, transpileEnv)
				if mt, ok := t.(types.MapType); ok {
					child.SetVar(st.For.Name, mt.Key, true)
				} else if lt, ok := t.(types.ListType); ok {
					child.SetVar(st.For.Name, lt.Elem, true)
				} else if _, ok := t.(types.StringType); ok {
					child.SetVar(st.For.Name, types.StringType{}, true)
				} else {
					child.SetVar(st.For.Name, types.AnyType{}, true)
				}
			}
			transpileEnv = child
		}
		body, err := convertStmtList(st.For.Body)
		if transpileEnv != nil {
			transpileEnv = savedEnv
		}
		if err != nil {
			return nil, err
		}
		if st.For.RangeEnd != nil {
			start, err := convertExpr(st.For.Source)
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(st.For.RangeEnd)
			if err != nil {
				return nil, err
			}
			return &ForRangeStmt{Name: st.For.Name, Start: start, End: end, Body: body}, nil
		}
		expr, err := convertExpr(st.For.Source)
		if err != nil {
			return nil, err
		}
		keys := false
		if transpileEnv != nil {
			t := types.ExprType(st.For.Source, savedEnv)
			if types.IsMapType(t) {
				keys = true
			}
		}
		return &ForEachStmt{Name: st.For.Name, Expr: expr, Keys: keys, Body: body}, nil
	case st.Import != nil:
		return convertImport(st.Import)
	case st.ExternVar != nil, st.ExternFun != nil, st.ExternType != nil, st.ExternObject != nil:
		return nil, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.Type != nil:
		return nil, nil
	case st.Test != nil:
		return nil, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	out := []Stmt{}
	for _, s := range list {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func convertIfStmt(ifst *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(ifst.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts := make([]Stmt, len(ifst.Then))
	for i, s := range ifst.Then {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		thenStmts[i] = st
	}
	var elseStmts []Stmt
	if ifst.ElseIf != nil {
		st, err := convertIfStmt(ifst.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{st}
	} else if len(ifst.Else) > 0 {
		elseStmts = make([]Stmt, len(ifst.Else))
		for i, s := range ifst.Else {
			st, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			elseStmts[i] = st
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then)
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
		elseExpr, err = convertExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &BoolLit{Value: false}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	arms := make([]MatchArm, len(me.Cases))
	for i, c := range me.Cases {
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		var pat Expr
		if c.Pattern != nil {
			pex, err := convertExpr(c.Pattern)
			if err != nil {
				return nil, err
			}
			if v, ok := pex.(*Var); ok && v.Name == "_" {
				pex = nil
			}
			pat = pex
		}
		arms[i] = MatchArm{Pattern: pat, Result: res}
	}
	return &MatchExpr{Target: target, Arms: arms}, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil {
		return convertGroupQuery(q)
	}
	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "right" && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct && q.Where == nil {
		j := q.Joins[0]
		leftSrc, err := convertExpr(q.Source)
		if err != nil {
			return nil, err
		}
		rightSrc, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		saved := funcStack
		funcStack = append(funcStack, []string{q.Var, j.Var})
		cond, err := convertExpr(j.On)
		if err != nil {
			funcStack = saved
			return nil, err
		}
		sel, err := convertExpr(q.Select)
		if err != nil {
			funcStack = saved
			return nil, err
		}
		funcStack = saved
		uses := []string{}
		for _, frame := range funcStack {
			uses = append(uses, frame...)
		}
		if len(funcStack) == 0 {
			uses = append(uses, globalNames...)
		}
		rj := &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel, Uses: uses}
		return rj, nil
	}

	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct && q.Where == nil {
		j := q.Joins[0]
		leftSrc, err := convertExpr(q.Source)
		if err != nil {
			return nil, err
		}
		rightSrc, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		saved := funcStack
		funcStack = append(funcStack, []string{q.Var, j.Var})
		cond, err := convertExpr(j.On)
		if err != nil {
			funcStack = saved
			return nil, err
		}
		sel, err := convertExpr(q.Select)
		if err != nil {
			funcStack = saved
			return nil, err
		}
		funcStack = saved
		uses := []string{}
		for _, frame := range funcStack {
			uses = append(uses, frame...)
		}
		if len(funcStack) == 0 {
			uses = append(uses, globalNames...)
		}
		lj := &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel, Uses: uses}
		return lj, nil
	}

	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "outer" && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct && q.Where == nil {
		j := q.Joins[0]
		leftSrc, err := convertExpr(q.Source)
		if err != nil {
			return nil, err
		}
		rightSrc, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		saved := funcStack
		funcStack = append(funcStack, []string{q.Var, j.Var})
		cond, err := convertExpr(j.On)
		if err != nil {
			funcStack = saved
			return nil, err
		}
		sel, err := convertExpr(q.Select)
		if err != nil {
			funcStack = saved
			return nil, err
		}
		funcStack = saved
		uses := []string{}
		for _, frame := range funcStack {
			uses = append(uses, frame...)
		}
		if len(funcStack) == 0 {
			uses = append(uses, globalNames...)
		}
		oj := &OuterJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel, Uses: uses}
		return oj, nil
	}

	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	loops := []interface{}{QueryLoop{Name: q.Var, Source: groupItemsExpr(src)}}
	var where Expr
	for _, f := range q.Froms {
		ex, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		loops = append(loops, QueryLoop{Name: f.Var, Source: groupItemsExpr(ex)})
	}
	for _, j := range q.Joins {
		ex, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		if j.Side != nil && *j.Side == "left" {
			cond, err := convertExpr(j.On)
			if err != nil {
				return nil, err
			}
			loops = append(loops, LeftJoinLoop{Name: j.Var, Source: groupItemsExpr(ex), Cond: cond})
		} else if j.Side == nil || *j.Side == "inner" {
			loops = append(loops, QueryLoop{Name: j.Var, Source: groupItemsExpr(ex)})
			cond, err := convertExpr(j.On)
			if err != nil {
				return nil, err
			}
			if where == nil {
				where = cond
			} else {
				where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
			}
		} else {
			return nil, fmt.Errorf("unsupported join")
		}
	}
	funcStack = append(funcStack, nil)
	for _, lp := range loops {
		switch l := lp.(type) {
		case QueryLoop:
			funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], l.Name)
		case LeftJoinLoop:
			funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], l.Name)
		}
	}
	if q.Where != nil {
		w, err := convertExpr(q.Where)
		if err != nil {
			funcStack = funcStack[:len(funcStack)-1]
			return nil, err
		}
		if where == nil {
			where = w
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: w}
		}
	}
	sel, err := convertExpr(q.Select)
	if err != nil {
		funcStack = funcStack[:len(funcStack)-1]
		return nil, err
	}
	funcStack = funcStack[:len(funcStack)-1]
	uses := []string{}
	for _, frame := range funcStack {
		uses = append(uses, frame...)
	}
	if len(funcStack) == 0 {
		uses = append(uses, globalNames...)
	}
	if c, ok := sel.(*CallExpr); ok && c.Func == "array_sum" && len(c.Args) == 1 {
		if v, ok2 := c.Args[0].(*Var); ok2 && v.Name == q.Var && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
			qexpr := &QueryExpr{Loops: loops, Where: where, Select: &Var{Name: q.Var}, Uses: uses}
			return &SumExpr{List: qexpr, Uses: uses}, nil
		}
	}
	return &QueryExpr{Loops: loops, Where: where, Select: sel, Uses: uses}, nil
}

func convertGroupQuery(q *parser.QueryExpr) (Expr, error) {
	if q.Group == nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}

	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	loops := []interface{}{QueryLoop{Name: q.Var, Source: groupItemsExpr(src)}}
	for _, f := range q.Froms {
		ex, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		loops = append(loops, QueryLoop{Name: f.Var, Source: groupItemsExpr(ex)})
	}
	var where Expr
	for _, j := range q.Joins {
		ex, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		if j.Side != nil && *j.Side == "left" {
			cond, err := convertExpr(j.On)
			if err != nil {
				return nil, err
			}
			loops = append(loops, LeftJoinLoop{Name: j.Var, Source: groupItemsExpr(ex), Cond: cond})
		} else if j.Side == nil || *j.Side == "inner" {
			loops = append(loops, QueryLoop{Name: j.Var, Source: groupItemsExpr(ex)})
			cond, err := convertExpr(j.On)
			if err != nil {
				return nil, err
			}
			if where == nil {
				where = cond
			} else {
				where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
			}
		} else {
			return nil, fmt.Errorf("unsupported join")
		}
	}

	funcStack = append(funcStack, nil)
	for _, lp := range loops {
		switch l := lp.(type) {
		case QueryLoop:
			funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], l.Name)
		case LeftJoinLoop:
			funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], l.Name)
		}
	}
	key, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		funcStack = funcStack[:len(funcStack)-1]
		return nil, err
	}
	if q.Where != nil {
		w, err := convertExpr(q.Where)
		if err != nil {
			funcStack = funcStack[:len(funcStack)-1]
			return nil, err
		}
		if where == nil {
			where = w
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: w}
		}
	}
	funcStack = append(funcStack, []string{q.Group.Name})
	groupStack = append(groupStack, q.Group.Name)
	defer func() { groupStack = groupStack[:len(groupStack)-1] }()
	sel, err := convertExpr(q.Select)
	if err != nil {
		funcStack = funcStack[:len(funcStack)-2]
		return nil, err
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			funcStack = funcStack[:len(funcStack)-2]
			return nil, err
		}
	}
	sortKey := false
	var sortExpr Expr
	if q.Sort != nil {
		ex, err := convertExpr(q.Sort)
		if err != nil {
			funcStack = funcStack[:len(funcStack)-2]
			return nil, err
		}
		if ie, ok := ex.(*IndexExpr); ok {
			if v, ok2 := ie.X.(*Var); ok2 && v.Name == q.Group.Name {
				if s, ok3 := ie.Index.(*StringLit); ok3 && s.Value == "key" {
					sortKey = true
				} else {
					sortExpr = ex
				}
			} else {
				sortExpr = ex
			}
		} else {
			sortExpr = ex
		}
	}
	funcStack = funcStack[:len(funcStack)-1]
	funcStack = funcStack[:len(funcStack)-1]
	uses := []string{}
	for _, frame := range funcStack {
		uses = append(uses, frame...)
	}
	if len(funcStack) == 0 {
		uses = append(uses, globalNames...)
	}
	return &GroupByExpr{Loops: loops, Key: key, Name: q.Group.Name, Where: where, Select: sel, Having: having, SortKey: sortKey, Sort: sortExpr, Uses: uses}, nil
}

func isListArg(e Expr) bool {
	switch v := e.(type) {
	case *ListLit:
		return true
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	}
	return false
}

func isMapArg(e Expr) bool {
	switch v := e.(type) {
	case *MapLit:
		return true
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if types.IsMapType(t) {
					return true
				}
			}
		}
	}
	return false
}

func isGroupArg(e Expr) bool {
	switch v := e.(type) {
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if _, ok := t.(types.GroupType); ok {
					return true
				}
			}
		}
		for _, n := range groupStack {
			if n == v.Name {
				return true
			}
		}
	}
	return false
}

func isFuncType(t types.Type) bool {
	if t == nil {
		return false
	}
	_, ok := t.(types.FuncType)
	return ok
}

func groupItemsExpr(e Expr) Expr {
	if v, ok := e.(*Var); ok {
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if _, ok := t.(types.GroupType); ok {
					return &IndexExpr{X: e, Index: &StringLit{Value: "items"}}
				}
			}
		}
		for _, n := range groupStack {
			if n == v.Name {
				return &IndexExpr{X: e, Index: &StringLit{Value: "items"}}
			}
		}
	}
	return e
}

func simpleResolveType(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "bigint":
			return types.BigIntType{}
		case "int64":
			return types.Int64Type{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
			if ut, ok := env.GetUnion(*t.Simple); ok {
				return ut
			}
			if ut, ok := env.FindUnionByVariant(*t.Simple); ok {
				return ut
			}
			return types.AnyType{}
		}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return types.ListType{Elem: simpleResolveType(t.Generic.Args[0], env)}
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return types.MapType{Key: simpleResolveType(t.Generic.Args[0], env), Value: simpleResolveType(t.Generic.Args[1], env)}
			}
		}
	}
	return types.AnyType{}
}

func isListExpr(e Expr) bool {
	if isListArg(e) {
		return true
	}
	if c, ok := e.(*CallExpr); ok {
		switch c.Func {
		case "array_merge", "array_slice", "array_values", "array_diff", "array_intersect", "array_unique":
			return true
		}
	} else if b, ok := e.(*BinaryExpr); ok {
		switch b.Op {
		case "union", "union_all", "except", "intersect":
			return true
		}
	} else if _, ok := e.(*SliceExpr); ok {
		return true
	}
	return false
}

func isMapExpr(e Expr) bool {
	if isMapArg(e) {
		return true
	}
	return false
}

func isStringExpr(e Expr) bool {
	switch v := e.(type) {
	case *StringLit:
		return true
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	case *CondExpr:
		if isStringExpr(v.Then) && isStringExpr(v.Else) {
			return true
		}
	}
	return false
}

func isBoolExpr(e Expr) bool {
	switch v := e.(type) {
	case *BoolLit:
		return true
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if _, ok := t.(types.BoolType); ok {
					return true
				}
			}
		}
	case *BinaryExpr:
		switch v.Op {
		case "<", "<=", ">", ">=", "==", "!=", "===", "!==", "&&", "||", "in":
			return true
		}
	case *CallExpr:
		switch v.Func {
		case "str_contains", "in_array":
			return true
		}
	case *IndexExpr:
		if s, ok := v.Index.(*StringLit); ok {
			if vv, ok2 := v.X.(*Var); ok2 && transpileEnv != nil {
				if t, err := transpileEnv.GetVar(vv.Name); err == nil {
					if ft := types.FieldType(t, []string{s.Value}); ft != nil {
						if _, ok3 := ft.(types.BoolType); ok3 {
							return true
						}
					}
				}
			}
		}
	case *CondExpr:
		if isBoolExpr(v.Then) && isBoolExpr(v.Else) {
			return true
		}
	}
	return false
}

func convertUpdate(u *parser.UpdateStmt) (*UpdateStmt, error) {
	if transpileEnv == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := transpileEnv.GetVar(u.Target)
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
	child := types.NewEnv(transpileEnv)
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
	}
	prev := transpileEnv
	transpileEnv = child
	var fields []string
	var values []Expr
	for _, item := range u.Set.Items {
		key, ok := isSimpleIdent(item.Key)
		if !ok {
			key, ok = literalString(item.Key)
			if !ok {
				transpileEnv = prev
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(item.Value)
		if err != nil {
			transpileEnv = prev
			return nil, err
		}
		val = substituteFields(val, "item", fieldSet)
		fields = append(fields, key)
		values = append(values, val)
	}
	var cond Expr
	if u.Where != nil {
		c, err := convertExpr(u.Where)
		if err != nil {
			transpileEnv = prev
			return nil, err
		}
		cond = substituteFields(c, "item", fieldSet)
	}
	transpileEnv = prev
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func maybeBoolString(e Expr) Expr {
	if isBoolExpr(e) {
		return &CondExpr{Cond: e, Then: &StringLit{Value: "true"}, Else: &StringLit{Value: "false"}}
	}
	return e
}

func maybeFloatString(e Expr) Expr {
	return &CallExpr{Func: "json_encode", Args: []Expr{e, &IntLit{Value: 1344}}}
}

func convertImport(im *parser.ImportStmt) (Stmt, error) {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	importedModules[alias] = struct{}{}
	path := strings.Trim(im.Path, "\"")
	if im.Lang == nil {
		return &LetStmt{Name: alias, Value: &MapLit{}}, nil
	}
	lang := *im.Lang
	if lang == "python" && path == "math" {
		items := []MapEntry{
			{Key: &StringLit{Value: "sqrt"}, Value: &ClosureExpr{Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Value: &CallExpr{Func: "sqrt", Args: []Expr{&Var{Name: "x"}}}}}}},
			{Key: &StringLit{Value: "pow"}, Value: &ClosureExpr{Params: []string{"x", "y"}, Body: []Stmt{&ReturnStmt{Value: &CallExpr{Func: "pow", Args: []Expr{&Var{Name: "x"}, &Var{Name: "y"}}}}}}},
			{Key: &StringLit{Value: "sin"}, Value: &ClosureExpr{Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Value: &CallExpr{Func: "sin", Args: []Expr{&Var{Name: "x"}}}}}}},
			{Key: &StringLit{Value: "log"}, Value: &ClosureExpr{Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Value: &CallExpr{Func: "log", Args: []Expr{&Var{Name: "x"}}}}}}},
			{Key: &StringLit{Value: "pi"}, Value: &Name{Value: "M_PI"}},
			{Key: &StringLit{Value: "e"}, Value: &Name{Value: "M_E"}},
		}
		return &LetStmt{Name: alias, Value: &MapLit{Items: items}}, nil
	}
	if lang == "go" && im.Auto && strings.Contains(path, "testpkg") {
		items := []MapEntry{
			{Key: &StringLit{Value: "Add"}, Value: &ClosureExpr{Params: []string{"a", "b"}, Body: []Stmt{&ReturnStmt{Value: &BinaryExpr{Left: &Var{Name: "a"}, Op: "+", Right: &Var{Name: "b"}}}}}},
			{Key: &StringLit{Value: "Pi"}, Value: &FloatLit{Value: 3.14}},
			{Key: &StringLit{Value: "Answer"}, Value: &IntLit{Value: 42}},
			{Key: &StringLit{Value: "FifteenPuzzleExample"}, Value: &ClosureExpr{Params: []string{}, Body: []Stmt{&ReturnStmt{Value: &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}}}}},
		}
		return &LetStmt{Name: alias, Value: &MapLit{Items: items}}, nil
	}
	return &LetStmt{Name: alias, Value: &MapLit{}}, nil
}

func buildAssignTarget(name string, idx []*parser.IndexOp, fields []*parser.FieldOp) (Expr, error) {
	var target Expr = &Var{Name: name}
	for _, op := range idx {
		if op.Start == nil || op.Colon != nil || op.End != nil || op.Colon2 != nil || op.Step != nil {
			return nil, fmt.Errorf("unsupported index")
		}
		ex, err := convertExpr(op.Start)
		if err != nil {
			return nil, err
		}
		target = &IndexExpr{X: target, Index: ex}
	}
	for _, f := range fields {
		target = &IndexExpr{X: target, Index: &StringLit{Value: f.Name}}
	}
	return target, nil
}

// Convert the PHP AST to a generic ast.Node for debugging.
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr_stmt", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let_stmt", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		child := &ast.Node{Kind: "null"}
		if st.Value != nil {
			child = exprNode(st.Value)
		}
		return &ast.Node{Kind: "var_stmt", Value: st.Name, Children: []*ast.Node{child}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign_stmt", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "index_assign", Children: []*ast.Node{exprNode(st.Target), exprNode(st.Value)}}
	case *ReturnStmt:
		child := &ast.Node{Kind: "null"}
		if st.Value != nil {
			child = exprNode(st.Value)
		}
		return &ast.Node{Kind: "return_stmt", Children: []*ast.Node{child}}
	case *FuncDecl:
		n := &ast.Node{Kind: "func_decl", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params)
		body := &ast.Node{Kind: "body"}
		for _, bs := range st.Body {
			body.Children = append(body.Children, stmtNode(bs))
		}
		n.Children = append(n.Children, body)
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while_stmt", Children: []*ast.Node{exprNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for_range", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForEachStmt:
		n := &ast.Node{Kind: "for_each", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Expr))
		if st.Keys {
			n.Children = append(n.Children, &ast.Node{Kind: "keys"})
		}
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *SaveStmt:
		n := &ast.Node{Kind: "save"}
		n.Children = append(n.Children, exprNode(st.Src))
		n.Children = append(n.Children, &ast.Node{Kind: "path", Value: st.Path})
		n.Children = append(n.Children, &ast.Node{Kind: "format", Value: st.Format})
		return n
	case *UpdateStmt:
		n := &ast.Node{Kind: "update", Value: st.Target}
		setNode := &ast.Node{Kind: "set"}
		for i, f := range st.Fields {
			pair := &ast.Node{Kind: "pair", Children: []*ast.Node{&ast.Node{Kind: "string", Value: f}, exprNode(st.Values[i])}}
			setNode.Children = append(setNode.Children, pair)
		}
		n.Children = append(n.Children, setNode)
		if st.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprNode(st.Cond)}})
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if_stmt", Children: []*ast.Node{exprNode(st.Cond)}}
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
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	if e == nil {
		return &ast.Node{Kind: "null"}
	}
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.X)}}
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	case *Var:
		return &ast.Node{Kind: "var", Value: ex.Name}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *BoolLit:
		return &ast.Node{Kind: "bool", Value: ex.Value}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			entry := &ast.Node{Kind: "entry"}
			entry.Children = append(entry.Children, exprNode(it.Key), exprNode(it.Value))
			n.Children = append(n.Children, entry)
		}
		return n
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.X)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.List)}}
	case *IntDivExpr:
		return &ast.Node{Kind: "intdiv", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.X), exprNode(ex.Index)}}
	case *SliceExpr:
		return &ast.Node{Kind: "slice", Children: []*ast.Node{exprNode(ex.X), exprNode(ex.Start), exprNode(ex.End)}}
	case *GroupByExpr:
		n := &ast.Node{Kind: "group_by"}
		for _, lp := range ex.Loops {
			switch l := lp.(type) {
			case QueryLoop:
				n.Children = append(n.Children, &ast.Node{Kind: "loop", Value: l.Name, Children: []*ast.Node{exprNode(l.Source)}})
			case LeftJoinLoop:
				child := &ast.Node{Kind: "left_loop", Value: l.Name}
				child.Children = append(child.Children, exprNode(l.Source), exprNode(l.Cond))
				n.Children = append(n.Children, child)
			}
		}
		n.Children = append(n.Children,
			&ast.Node{Kind: "key", Children: []*ast.Node{exprNode(ex.Key)}},
			&ast.Node{Kind: "name", Value: ex.Name},
		)
		if ex.Where != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprNode(ex.Where)}})
		}
		n.Children = append(n.Children, &ast.Node{Kind: "select", Children: []*ast.Node{exprNode(ex.Select)}})
		if ex.Having != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "having", Children: []*ast.Node{exprNode(ex.Having)}})
		}
		return n
	case *RightJoinExpr:
		n := &ast.Node{Kind: "right_join"}
		n.Children = append(n.Children,
			&ast.Node{Kind: "left_var", Value: ex.LeftVar},
			exprNode(ex.LeftSrc),
			&ast.Node{Kind: "right_var", Value: ex.RightVar},
			exprNode(ex.RightSrc),
			exprNode(ex.Cond),
			exprNode(ex.Select))
		return n
	case *LeftJoinExpr:
		n := &ast.Node{Kind: "left_join"}
		n.Children = append(n.Children,
			&ast.Node{Kind: "left_var", Value: ex.LeftVar},
			exprNode(ex.LeftSrc),
			&ast.Node{Kind: "right_var", Value: ex.RightVar},
			exprNode(ex.RightSrc),
			exprNode(ex.Cond),
			exprNode(ex.Select))
		return n
	case *OuterJoinExpr:
		n := &ast.Node{Kind: "outer_join"}
		n.Children = append(n.Children,
			&ast.Node{Kind: "left_var", Value: ex.LeftVar},
			exprNode(ex.LeftSrc),
			&ast.Node{Kind: "right_var", Value: ex.RightVar},
			exprNode(ex.RightSrc),
			exprNode(ex.Cond),
			exprNode(ex.Select))
		return n
	case *LoadExpr:
		n := &ast.Node{Kind: "load"}
		n.Children = append(n.Children,
			&ast.Node{Kind: "path", Value: ex.Path},
			&ast.Node{Kind: "format", Value: ex.Format})
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the AST in Lisp-like form to stdout.
func Print(p *Program) { toNode(p).Print("") }

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
		key, ok := literalString(it.Key)
		if ok && key == "format" {
			if v, ok := literalString(it.Value); ok {
				return v
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
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func substituteFields(e Expr, varName string, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Var:
		if fields[ex.Name] {
			return &IndexExpr{X: &Var{Name: varName}, Index: &StringLit{Value: ex.Name}}
		}
		return ex
	case *BinaryExpr:
		ex.Left = substituteFields(ex.Left, varName, fields)
		ex.Right = substituteFields(ex.Right, varName, fields)
		return ex
	case *UnaryExpr:
		ex.X = substituteFields(ex.X, varName, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		return ex
	case *IndexExpr:
		ex.X = substituteFields(ex.X, varName, fields)
		ex.Index = substituteFields(ex.Index, varName, fields)
		return ex
	case *SliceExpr:
		ex.X = substituteFields(ex.X, varName, fields)
		ex.Start = substituteFields(ex.Start, varName, fields)
		ex.End = substituteFields(ex.End, varName, fields)
		return ex
	case *CondExpr:
		ex.Cond = substituteFields(ex.Cond, varName, fields)
		ex.Then = substituteFields(ex.Then, varName, fields)
		ex.Else = substituteFields(ex.Else, varName, fields)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = substituteFields(ex.Elems[i], varName, fields)
		}
		return ex
	case *MapLit:
		for i := range ex.Items {
			ex.Items[i].Key = substituteFields(ex.Items[i].Key, varName, fields)
			ex.Items[i].Value = substituteFields(ex.Items[i].Value, varName, fields)
		}
		return ex
	default:
		return ex
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
