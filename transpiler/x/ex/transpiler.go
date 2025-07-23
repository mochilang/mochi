//go:build slow

package ex

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	"gopkg.in/yaml.v3"

	"mochi/parser"
	"mochi/types"
)

var funcDepth int

// builtinAliases maps import aliases to special built-in modules.
var builtinAliases map[string]string

// globalVars tracks variables defined before the first function declaration.
var globalVars map[string]struct{}

var loopCounter int

func moduleAttrName(name string) string {
	if len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
		return strings.ToLower(name)
	}
	return name
}

func uniqueWhileName() string {
	loopCounter++
	if loopCounter == 1 {
		return "while_fun"
	}
	return fmt.Sprintf("while_fun_%d", loopCounter)
}

// moduleMode is true when emitting a module with functions.
var moduleMode bool

// Program represents a sequence of Elixir statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer, int) }

// VarRef references a variable name or dotted selector.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) {
        if moduleMode {
                if _, ok := globalVars[v.Name]; ok {
                        fmt.Fprintf(w, "Process.get(:%s)", moduleAttrName(v.Name))
                        return
                }
                io.WriteString(w, v.Name)
                return
        }
	io.WriteString(w, v.Name)
}

// LetStmt binds a variable optionally to a value.
type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, s.Name)
	io.WriteString(w, " = ")
	if s.Value != nil {
		s.Value.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
}

func (s *LetStmt) emitGlobal(w io.Writer, indent int) {
        for i := 0; i < indent; i++ {
                io.WriteString(w, "  ")
        }
        io.WriteString(w, "Process.put(:")
        io.WriteString(w, moduleAttrName(s.Name))
        io.WriteString(w, ", ")
        if s.Value != nil {
                s.Value.emit(w)
        } else {
                io.WriteString(w, "nil")
        }
        io.WriteString(w, ")")
}

// AssignStmt reassigns a variable.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
        if moduleMode {
                if _, ok := globalVars[s.Name]; ok {
                        fmt.Fprintf(w, "Process.put(:%s, ", moduleAttrName(s.Name))
                        s.Value.emit(w)
                        io.WriteString(w, ")")
                        return
                }
                io.WriteString(w, s.Name)
        } else {
                io.WriteString(w, s.Name)
        }
        io.WriteString(w, " = ")
        s.Value.emit(w)
}

type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement consisting solely of an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	s.Expr.emit(w)
}

// BreakStmt represents a break statement inside loops.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "throw :break")
}

// ContinueStmt represents a continue statement inside loops.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "throw :continue")
}

// ReturnStmt returns from a function optionally with a value.
type ReturnStmt struct{ Value Expr }

func (r *ReturnStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "throw {:return, ")
	if r.Value != nil {
		r.Value.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
	io.WriteString(w, "}")
}

// IfStmt is a simple if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
	Vars []string
}

func (s *IfStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	if len(s.Vars) > 0 {
		io.WriteString(w, "{")
		for i, v := range s.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "} = if ")
	} else {
		io.WriteString(w, "if ")
	}
	s.Cond.emit(w)
	io.WriteString(w, " do\n")
	for _, st := range s.Then {
		st.emit(w, indent+1)
		io.WriteString(w, "\n")
	}
	if len(s.Vars) > 0 {
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "{")
		for i, v := range s.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "}\n")
	}
	if len(s.Else) > 0 || len(s.Vars) > 0 {
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "else\n")
		for _, st := range s.Else {
			st.emit(w, indent+1)
			io.WriteString(w, "\n")
		}
		if len(s.Vars) > 0 {
			for i := 0; i < indent+1; i++ {
				io.WriteString(w, "  ")
			}
			io.WriteString(w, "{")
			for i, v := range s.Vars {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				io.WriteString(w, v)
			}
			io.WriteString(w, "}\n")
		}
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end")
}

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond   Expr
	Body   []Stmt
	Vars   []string
	Simple bool
}

func (wst *WhileStmt) emit(w io.Writer, indent int) {
	name := uniqueWhileName()
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, name)
	io.WriteString(w, " = fn ")
	io.WriteString(w, name)
	for _, v := range wst.Vars {
		io.WriteString(w, ", ")
		io.WriteString(w, v)
	}
	io.WriteString(w, " ->\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "if ")
	wst.Cond.emit(w)
	io.WriteString(w, " do\n")
	if wst.Simple {
		for _, st := range wst.Body {
			st.emit(w, indent+2)
			io.WriteString(w, "\n")
		}
	} else {
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "try do\n")
		for _, st := range wst.Body {
			st.emit(w, indent+3)
			io.WriteString(w, "\n")
		}
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "catch\n")
		for i := 0; i < indent+3; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, ":continue -> nil\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "end\n")
	}
	for i := 0; i < indent+2; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, name)
	io.WriteString(w, ".(")
	io.WriteString(w, name)
	for _, v := range wst.Vars {
		io.WriteString(w, ", ")
		io.WriteString(w, v)
	}
	io.WriteString(w, ")\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "else\n")
	for i := 0; i < indent+2; i++ {
		io.WriteString(w, "  ")
	}
	if len(wst.Vars) == 0 {
		io.WriteString(w, "nil\n")
	} else if len(wst.Vars) == 1 {
		io.WriteString(w, wst.Vars[0])
		io.WriteString(w, "\n")
	} else {
		io.WriteString(w, "{")
		for i, v := range wst.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "}\n")
	}
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	if len(wst.Vars) == 0 {
		io.WriteString(w, "try do\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, name)
		io.WriteString(w, ".(")
		io.WriteString(w, name)
		io.WriteString(w, ")\n")
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "catch\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, ":break -> nil\n")
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "end\n")
	} else if len(wst.Vars) == 1 {
		io.WriteString(w, wst.Vars[0])
		io.WriteString(w, " = try do\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, name)
		io.WriteString(w, ".(")
		io.WriteString(w, name)
		io.WriteString(w, ", ")
		io.WriteString(w, wst.Vars[0])
		io.WriteString(w, ")\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "catch\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, ":break -> ")
		io.WriteString(w, wst.Vars[0])
		io.WriteString(w, "\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "end\n")
	} else {
		io.WriteString(w, "{")
		for i, v := range wst.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "} = try do\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, name)
		io.WriteString(w, ".(")
		io.WriteString(w, name)
		for _, v := range wst.Vars {
			io.WriteString(w, ", ")
			io.WriteString(w, v)
		}
		io.WriteString(w, ")\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "catch\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, ":break -> {")
		for i, v := range wst.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "}\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "end\n")
	}
	return
}

// ForStmt represents a basic for loop over a collection or range.
type ForStmt struct {
	Name   string
	Start  Expr
	End    Expr // optional, when non-nil compile as range
	Source Expr // used when End is nil
	Body   []Stmt
	Vars   []string
	Simple bool
}

func (fs *ForStmt) emit(w io.Writer, indent int) {
	if len(fs.Vars) > 0 && fs.End != nil {
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "{")
		for i, v := range fs.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "} = Enum.reduce((")
		fs.Start.emit(w)
		io.WriteString(w, "..(")
		fs.End.emit(w)
		io.WriteString(w, " - 1)), {")
		for i, v := range fs.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "}, fn ")
		io.WriteString(w, fs.Name)
		io.WriteString(w, ", {")
		for i, v := range fs.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "} ->\n")
		for _, st := range fs.Body {
			st.emit(w, indent+1)
			io.WriteString(w, "\n")
		}
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "{")
		for i, v := range fs.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, v)
		}
		io.WriteString(w, "}\n")
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "end)")
		return
	}
	if fs.Simple {
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "Enum.each(")
		if fs.End != nil {
			io.WriteString(w, "(")
			fs.Start.emit(w)
			io.WriteString(w, "..(")
			fs.End.emit(w)
			io.WriteString(w, " - 1))")
		} else {
			fs.Source.emit(w)
		}
		io.WriteString(w, ", fn ")
		io.WriteString(w, fs.Name)
		io.WriteString(w, " ->\n")
		for _, st := range fs.Body {
			st.emit(w, indent+1)
			io.WriteString(w, "\n")
		}
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "end)")
		return
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "try do\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "for ")
	io.WriteString(w, fs.Name)
	io.WriteString(w, " <- ")
	if fs.End != nil {
		io.WriteString(w, "(")
		fs.Start.emit(w)
		io.WriteString(w, "..(")
		fs.End.emit(w)
		io.WriteString(w, " - 1)")
		io.WriteString(w, ")")
	} else {
		fs.Source.emit(w)
	}
	io.WriteString(w, " do\n")
	for i := 0; i < indent+2; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "try do\n")
	for _, st := range fs.Body {
		st.emit(w, indent+3)
		io.WriteString(w, "\n")
	}
	for i := 0; i < indent+2; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "catch\n")
	for i := 0; i < indent+3; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, ":continue -> nil\n")
	for i := 0; i < indent+2; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "catch\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, ":break -> nil\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end")
}

// SaveStmt writes a list of maps or structs in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (s *SaveStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "Enum.each(")
		s.Src.emit(w)
		io.WriteString(w, ", fn row ->\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "row = if is_map(row) and Map.has_key?(row, :__struct__), do: Map.from_struct(row), else: row\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "keys = Map.keys(row) |> Enum.sort()\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "json = Enum.map_join(keys, \",\", fn k -> v = Map.get(row, k); val = if is_binary(v), do: \"\\\"\"<>v<>\"\\\"\", else: to_string(v); \"\\\"\"<>to_string(k)<>\"\\\": \"<>val end)\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "IO.puts(\"{\" <> json <> \"}\")\n")
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "end)")
		return
	}
	io.WriteString(w, "# unsupported save")
}

// FuncDecl defines a simple function.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (fn *FuncDecl) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "def ")
	io.WriteString(w, fn.Name)
	io.WriteString(w, "(")
	for i, p := range fn.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ") do\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "try do\n")
	for _, st := range fn.Body {
		st.emit(w, indent+2)
		io.WriteString(w, "\n")
	}
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "catch\n")
	for i := 0; i < indent+2; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "{:return, val} -> val\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end")
}

// AnonFun represents an anonymous function expression.
type AnonFun struct {
	Params []string
	Body   []Stmt
}

func (af *AnonFun) emit(w io.Writer) {
	io.WriteString(w, "fn ")
	for i, p := range af.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, " ->")
	if len(af.Body) == 1 {
		if es, ok := af.Body[0].(*ExprStmt); ok {
			io.WriteString(w, " ")
			es.Expr.emit(w)
			io.WriteString(w, " end")
			return
		}
	}
	io.WriteString(w, "\n")
	for _, st := range af.Body {
		st.emit(w, 1)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}

// CondExpr represents a conditional expression.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) {
	io.WriteString(w, "if ")
	c.Cond.emit(w)
	io.WriteString(w, ", do: ")
	c.Then.emit(w)
	if c.Else != nil {
		io.WriteString(w, ", else: ")
		c.Else.emit(w)
	}
}

// CaseExpr represents a simple match/case expression.
type CaseExpr struct {
	Target  Expr
	Clauses []CaseClause
}

// CaseClause represents a single pattern clause within a CaseExpr.
type CaseClause struct {
	Pattern Expr // nil represents '_'
	Result  Expr
}

func (ce *CaseExpr) emit(w io.Writer) {
	io.WriteString(w, "case ")
	ce.Target.emit(w)
	io.WriteString(w, " do\n")
	for _, cl := range ce.Clauses {
		io.WriteString(w, "  ")
		if cl.Pattern != nil {
			cl.Pattern.emit(w)
		} else {
			io.WriteString(w, "_")
		}
		io.WriteString(w, " -> ")
		cl.Result.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}

// GroupExpr wraps another expression in parentheses.
type GroupExpr struct{ Expr Expr }

func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.Expr.emit(w)
	io.WriteString(w, ")")
}

// BinaryExpr represents a binary operation such as 1 + 2.
type BinaryExpr struct {
	Left      Expr
	Op        string
	Right     Expr
	MapIn     bool
	StrConcat bool
}

func (b *BinaryExpr) emit(w io.Writer) {
	isInt := func(e Expr) bool {
		if n, ok := e.(*NumberLit); ok {
			return !strings.Contains(n.Value, ".")
		}
		return false
	}
	isString := func(e Expr) bool {
		switch t := e.(type) {
		case *StringLit:
			return true
		case *CallExpr:
			if t.Func == "to_string" || strings.HasSuffix(t.Func, ".to_string") {
				return true
			}
		}
		return false
	}
	if b.Op == "/" && isInt(b.Left) && isInt(b.Right) {
		io.WriteString(w, "div(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "%" {
		io.WriteString(w, "rem(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "+" && (b.StrConcat || isString(b.Left) || isString(b.Right)) {
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, " <> ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "++" {
		io.WriteString(w, "(")
		b.Left.emit(w)
		io.WriteString(w, " ++ ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "in" && b.MapIn {
		io.WriteString(w, "Map.has_key?(")
		b.Right.emit(w)
		io.WriteString(w, ", ")
		b.Left.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "in" && (isString(b.Left) || isString(b.Right)) {
		io.WriteString(w, "String.contains?(")
		b.Right.emit(w)
		io.WriteString(w, ", ")
		b.Left.emit(w)
		io.WriteString(w, ")")
		return
	}
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, b.Op)
	io.WriteString(w, " ")
	b.Right.emit(w)
}

// UnaryExpr represents a prefix unary operation.
type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	io.WriteString(w, u.Op)
	u.Expr.emit(w)
}

// CallExpr represents a function call.
type CallExpr struct {
	Func string
	Args []Expr
	Var  bool
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	if c.Var {
		io.WriteString(w, ".(")
	} else {
		io.WriteString(w, "(")
	}
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// IndexExpr represents indexing into a list or string.
type IndexExpr struct {
	Target       Expr
	Index        Expr
	IsString     bool
	UseMapSyntax bool
}

func (i *IndexExpr) emit(w io.Writer) {
	if i.UseMapSyntax {
		if a, ok := i.Index.(*AtomLit); ok {
			i.Target.emit(w)
			io.WriteString(w, ".")
			io.WriteString(w, a.Name)
			return
		}
		i.Target.emit(w)
		io.WriteString(w, "[")
		i.Index.emit(w)
		io.WriteString(w, "]")
		return
	}
	if i.IsString {
		io.WriteString(w, "String.at(")
	} else {
		io.WriteString(w, "Enum.at(")
	}
	i.Target.emit(w)
	io.WriteString(w, ", ")
	i.Index.emit(w)
	io.WriteString(w, ")")
}

// StringLit is a quoted string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

// NumberLit is a numeric literal.
type NumberLit struct{ Value string }

func (n *NumberLit) emit(w io.Writer) { io.WriteString(w, n.Value) }

// BoolLit is a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

// AtomLit represents a simple atom key like :id.
type AtomLit struct{ Name string }

func (a *AtomLit) emit(w io.Writer) { io.WriteString(w, a.Name) }

// InterpString represents a string with interpolated expressions.
type InterpString struct{ Parts []interface{} }

func escape(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	return s
}

func (s *InterpString) emit(w io.Writer) {
	io.WriteString(w, "\"")
	for _, p := range s.Parts {
		switch v := p.(type) {
		case string:
			io.WriteString(w, escape(v))
		case Expr:
			io.WriteString(w, "#{")
			v.emit(w)
			io.WriteString(w, "}")
		}
	}
	io.WriteString(w, "\"")
}

// ListLit is a list literal like [1,2,3].
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

// MapLit represents a map literal like %{key => value}.
type MapLit struct{ Items []MapItem }

type MapItem struct {
	Key   Expr
	Value Expr
}

// StructUpdateExpr updates a field of a struct or map.
type StructUpdateExpr struct {
	Target Expr
	Field  string
	Value  Expr
}

func (s *StructUpdateExpr) emit(w io.Writer) {
	io.WriteString(w, "%{")
	s.Target.emit(w)
	io.WriteString(w, " | ")
	io.WriteString(w, s.Field)
	io.WriteString(w, ": ")
	s.Value.emit(w)
	io.WriteString(w, "}")
}

// UpdateStmt modifies items within a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (u *UpdateStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, u.Target)
	io.WriteString(w, " = Enum.map(")
	io.WriteString(w, u.Target)
	io.WriteString(w, ", fn item -> ")
	expr := Expr(&VarRef{Name: "item"})
	for i, f := range u.Fields {
		expr = &StructUpdateExpr{Target: expr, Field: f, Value: u.Values[i]}
	}
	if u.Cond != nil {
		ce := &CondExpr{Cond: u.Cond, Then: expr, Else: &VarRef{Name: "item"}}
		ce.emit(w)
	} else {
		expr.emit(w)
	}
	io.WriteString(w, " end)")
	io.WriteString(w, "\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "_ = ")
	io.WriteString(w, u.Target)
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "%{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if a, ok := it.Key.(*AtomLit); ok {
			io.WriteString(w, a.Name)
			io.WriteString(w, ": ")
		} else {
			it.Key.emit(w)
			io.WriteString(w, " => ")
		}
		it.Value.emit(w)
	}
	io.WriteString(w, "}")
}

// CompGen represents a generator within a list comprehension.
type CompGen struct {
	Var string
	Src Expr
}

// Comprehension represents an Elixir comprehension expression.
type Comprehension struct {
	Gens   []CompGen
	Filter Expr
	Body   Expr
}

func (c *Comprehension) emit(w io.Writer) {
	io.WriteString(w, "for ")
	for i, g := range c.Gens {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, g.Var)
		io.WriteString(w, " <- ")
		g.Src.emit(w)
	}
	if c.Filter != nil {
		io.WriteString(w, ", ")
		c.Filter.emit(w)
	}
	io.WriteString(w, " do\n")
	indent := 1
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	if m, ok := c.Body.(*MapLit); ok && len(m.Items) > 1 {
		io.WriteString(w, "%{\n")
		for i, it := range m.Items {
			for j := 0; j < indent+1; j++ {
				io.WriteString(w, "  ")
			}
			if a, ok := it.Key.(*AtomLit); ok {
				io.WriteString(w, a.Name)
				io.WriteString(w, ": ")
			} else {
				it.Key.emit(w)
				io.WriteString(w, " => ")
			}
			it.Value.emit(w)
			if i < len(m.Items)-1 {
				io.WriteString(w, ",\n")
			} else {
				io.WriteString(w, "\n")
			}
		}
		for i := 0; i < indent; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "}")
	} else {
		c.Body.emit(w)
	}
	io.WriteString(w, "\nend")
}

// GroupByExpr represents a basic group by query without joins or sorting.
type GroupByExpr struct {
	Var    string
	Source Expr
	Key    Expr
	Name   string
	Select Expr
}

// GroupBySortExpr represents a grouped query sorted by an expression.
type GroupBySortExpr struct {
	Var    string
	Source Expr
	Key    Expr
	Name   string
	Sort   Expr
	Select Expr
}

// GroupByHavingExpr represents a grouped query with a having filter.
type GroupByHavingExpr struct {
	Var    string
	Source Expr
	Key    Expr
	Name   string
	Having Expr
	Select Expr
}

// NilLit represents a `nil` literal.
type NilLit struct{}

// LoadExpr loads a YAML file into a list of maps.
type LoadExpr struct {
	Path   string
	Format string
}

func (l *LoadExpr) emit(w io.Writer) {
	if l.Format == "yaml" {
		io.WriteString(w, "(fn ->\n")
		fmt.Fprintf(w, "  lines = File.read!(%q) |> String.split(\"\\n\", trim: true)\n", l.Path)
		io.WriteString(w, "  {rows, curr} = Enum.reduce(lines, {[], %{}}, fn line, {rows, curr} ->\n")
		io.WriteString(w, "    line = String.trim(line)\n")
		io.WriteString(w, "    if String.starts_with?(line, \"-\") do\n")
		io.WriteString(w, "      rows = if map_size(curr) > 0, do: rows ++ [curr], else: rows\n")
		io.WriteString(w, "      curr = %{}\n")
		io.WriteString(w, "      line = line |> String.trim_leading(\"-\") |> String.trim()\n")
		io.WriteString(w, "      if line != \"\" do\n")
		io.WriteString(w, "        [k, v] = line |> String.split(\":\", parts: 2) |> Enum.map(&String.trim/1)\n")
		io.WriteString(w, "        v = if Regex.match?(~r/^[-]?\\d+$/, v), do: String.to_integer(v), else: v\n")
		io.WriteString(w, "        {rows, Map.put(curr, String.to_atom(k), v)}\n")
		io.WriteString(w, "      else\n")
		io.WriteString(w, "        {rows, curr}\n")
		io.WriteString(w, "      end\n")
		io.WriteString(w, "    else\n")
		io.WriteString(w, "      [k, v] = line |> String.split(\":\", parts: 2) |> Enum.map(&String.trim/1)\n")
		io.WriteString(w, "      v = if Regex.match?(~r/^[-]?\\d+$/, v), do: String.to_integer(v), else: v\n")
		io.WriteString(w, "      {rows, Map.put(curr, String.to_atom(k), v)}\n")
		io.WriteString(w, "    end\n  end)\n")
		io.WriteString(w, "  rows = if map_size(curr) > 0, do: rows ++ [curr], else: rows\n")
		io.WriteString(w, "  rows\nend).()")
	}
}

// LeftJoinExpr represents a simple left join without additional clauses.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	On       Expr
	Select   Expr
}

func (g *GroupByExpr) emit(w io.Writer) {
	io.WriteString(w, "Enum.group_by(")
	g.Source.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, g.Var)
	io.WriteString(w, " -> ")
	g.Key.emit(w)
	io.WriteString(w, " end)\n  |> Enum.map(fn {key, items} ->\n    ")
	io.WriteString(w, g.Name)
	io.WriteString(w, " = %{key: key, items: items}\n    ")
	g.Select.emit(w)
	io.WriteString(w, "\n  end")
	io.WriteString(w, ")")
}

func (g *GroupBySortExpr) emit(w io.Writer) {
	io.WriteString(w, "Enum.group_by(")
	g.Source.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, g.Var)
	io.WriteString(w, " -> ")
	g.Key.emit(w)
	io.WriteString(w, " end)\n  |> Enum.map(fn {key, items} -> %{key: key, items: items} end)\n  |> Enum.sort_by(fn g -> ")
	g.Sort.emit(w)
	io.WriteString(w, " end)\n  |> Enum.map(fn g ->\n    ")
	g.Select.emit(w)
	io.WriteString(w, "\n  end")
	io.WriteString(w, ")")
}

func (g *GroupByHavingExpr) emit(w io.Writer) {
	io.WriteString(w, "Enum.group_by(")
	g.Source.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, g.Var)
	io.WriteString(w, " -> ")
	g.Key.emit(w)
	io.WriteString(w, " end)\n  |> Enum.map(fn {key, items} -> %{key: key, items: items} end)\n  |> Enum.filter(fn g -> ")
	g.Having.emit(w)
	io.WriteString(w, " end)\n  |> Enum.map(fn g ->\n    ")
	io.WriteString(w, g.Name)
	io.WriteString(w, " = g\n    ")
	g.Select.emit(w)
	io.WriteString(w, "\n  end")
	io.WriteString(w, ")")
}

func (n *NilLit) emit(w io.Writer) { io.WriteString(w, "nil") }

func (lj *LeftJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "Enum.flat_map(")
	lj.LeftSrc.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, lj.LeftVar)
	io.WriteString(w, " ->\n  matches = Enum.filter(")
	lj.RightSrc.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, lj.RightVar)
	io.WriteString(w, " -> ")
	lj.On.emit(w)
	io.WriteString(w, " end)\n  list = if Enum.empty?(matches), do: [nil], else: matches\n  Enum.map(list, fn ")
	io.WriteString(w, lj.RightVar)
	io.WriteString(w, " -> ")
	lj.Select.emit(w)
	io.WriteString(w, " end)\nend)")
}

// OuterJoinExpr represents a simple full outer join without additional clauses.
type OuterJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	On       Expr
	Select   Expr
}

func (oj *OuterJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "Enum.concat([")
	// left side with possible nil right rows
	io.WriteString(w, "Enum.flat_map(")
	oj.LeftSrc.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, oj.LeftVar)
	io.WriteString(w, " ->\n  matches = Enum.filter(")
	oj.RightSrc.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, oj.RightVar)
	io.WriteString(w, " -> ")
	oj.On.emit(w)
	io.WriteString(w, " end)\n  list = if Enum.empty?(matches), do: [nil], else: matches\n  Enum.map(list, fn ")
	io.WriteString(w, oj.RightVar)
	io.WriteString(w, " -> ")
	oj.Select.emit(w)
	io.WriteString(w, " end)\nend)")
	io.WriteString(w, ", ")
	// right side with rows not matched on left
	io.WriteString(w, "Enum.flat_map(")
	oj.RightSrc.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, oj.RightVar)
	io.WriteString(w, " ->\n  exists = Enum.any?(")
	oj.LeftSrc.emit(w)
	io.WriteString(w, ", fn ")
	io.WriteString(w, oj.LeftVar)
	io.WriteString(w, " -> ")
	oj.On.emit(w)
	io.WriteString(w, " end)\n  if exists do\n    []\n  else\n    ")
	io.WriteString(w, oj.LeftVar)
	io.WriteString(w, " = nil\n    [")
	oj.Select.emit(w)
	io.WriteString(w, "]\n  end\nend)")
	io.WriteString(w, "])")
}

// CastExpr represents a simple cast like expr as int.
type CastExpr struct {
	Expr Expr
	Type string
}

func (c *CastExpr) emit(w io.Writer) {
	switch c.Type {
	case "int":
		io.WriteString(w, "String.to_integer(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		c.Expr.emit(w)
	}
}

// Emit generates Elixir source from the AST.
func Emit(p *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	funcsExist := false
	for _, st := range p.Stmts {
		if _, ok := st.(*FuncDecl); ok {
			funcsExist = true
			break
		}
	}
	if !funcsExist {
		globalVars = map[string]struct{}{}
	}
	moduleMode = true
	buf.WriteString("defmodule Main do\n")
	buf.WriteString(nowHelper(1))
	var globals []Stmt
	var funcs []Stmt
	var main []Stmt
	foundFunc := false
	for _, st := range p.Stmts {
		if _, ok := st.(*FuncDecl); ok {
			foundFunc = true
			funcs = append(funcs, st)
			continue
		}
		if funcsExist && !foundFunc {
			globals = append(globals, st)
		} else {
			if es, ok := st.(*ExprStmt); ok {
				if call, ok := es.Expr.(*CallExpr); ok && call.Func == "main" && len(call.Args) == 0 {
					continue
				}
			}
			main = append(main, st)
		}
	}
	for _, st := range globals {
		if ls, ok := st.(*LetStmt); ok {
			ls.emitGlobal(&buf, 1)
		} else {
			st.emit(&buf, 1)
		}
		buf.WriteString("\n")
	}
	for _, st := range funcs {
		st.emit(&buf, 1)
		buf.WriteString("\n")
	}
	buf.WriteString("  def main() do\n")
	for _, st := range main {
		st.emit(&buf, 2)
		buf.WriteString("\n")
	}
	buf.WriteString("  end\nend\n")
	buf.WriteString("Main.main()\n")
	moduleMode = false
	return buf.Bytes()
}

// Transpile converts a Mochi program into an Elixir AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	res := &Program{}
	builtinAliases = make(map[string]string)
	globalVars = make(map[string]struct{})
	foundFunc := false
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
				}
			case "python":
				if path == "math" {
					builtinAliases[alias] = "python_math"
				}
			}
		}
		if !foundFunc {
			if st.Fun != nil {
				foundFunc = true
			} else if st.Let != nil {
				globalVars[st.Let.Name] = struct{}{}
			} else if st.Var != nil {
				globalVars[st.Var.Name] = struct{}{}
			}
		}
	}
	for _, st := range prog.Statements {
		stmt, err := compileStmt(st, env)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			res.Stmts = append(res.Stmts, stmt)
		}
	}
	_ = env
	return res, nil
}

func compileStmt(st *parser.Statement, env *types.Env) (Stmt, error) {
	switch {
	case st.Expr != nil:
		if se := extractSaveExpr(st.Expr.Expr); se != nil {
			src, err := compileExpr(se.Src, env)
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
		e, err := compileExpr(st.Expr.Expr, env)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var val Expr
		if st.Let.Value != nil {
			var err error
			val, err = compileExpr(st.Let.Value, env)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil {
			switch *st.Let.Type.Simple {
			case "int":
				val = &NumberLit{Value: "0"}
			case "string":
				val = &StringLit{Value: ""}
			case "bool":
				val = &BoolLit{Value: false}
			case "list":
				val = &ListLit{}
			case "map":
				val = &MapLit{}
			}
		}
		if _, err := env.GetVar(st.Let.Name); err != nil {
			if _, ok := val.(*AnonFun); ok {
				env.SetVar(st.Let.Name, types.FuncType{}, false)
			} else if st.Let.Type != nil {
				env.SetVar(st.Let.Name, types.ResolveTypeRef(st.Let.Type, env), false)
			} else if ld := extractLoadExpr(st.Let.Value); ld != nil && ld.Type != nil {
				t := types.ResolveTypeRef(ld.Type, env)
				env.SetVar(st.Let.Name, types.ListType{Elem: t}, false)
			} else if st.Let.Value != nil {
				env.SetVar(st.Let.Name, types.TypeOfExprBasic(st.Let.Value, env), false)
			} else {
				env.SetVar(st.Let.Name, inferStaticType(val), false)
			}
		}
		return &LetStmt{Name: st.Let.Name, Value: val}, nil
	case st.Var != nil:
		var val Expr
		if st.Var.Value != nil {
			var err error
			val, err = compileExpr(st.Var.Value, env)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil {
			switch *st.Var.Type.Simple {
			case "int":
				val = &NumberLit{Value: "0"}
			case "string":
				val = &StringLit{Value: ""}
			case "bool":
				val = &BoolLit{Value: false}
			case "list":
				val = &ListLit{}
			case "map":
				val = &MapLit{}
			}
		}
		if _, err := env.GetVar(st.Var.Name); err != nil {
			if _, ok := val.(*AnonFun); ok {
				env.SetVar(st.Var.Name, types.FuncType{}, false)
			} else if st.Var.Type != nil {
				env.SetVar(st.Var.Name, types.ResolveTypeRef(st.Var.Type, env), false)
			} else if ld := extractLoadExpr(st.Var.Value); ld != nil && ld.Type != nil {
				t := types.ResolveTypeRef(ld.Type, env)
				env.SetVar(st.Var.Name, types.ListType{Elem: t}, false)
			} else if st.Var.Value != nil {
				env.SetVar(st.Var.Name, types.TypeOfExprBasic(st.Var.Value, env), false)
			} else {
				env.SetVar(st.Var.Name, inferStaticType(val), false)
			}
		}
		return &LetStmt{Name: st.Var.Name, Value: val}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: st.Assign.Name, Value: val}, nil
		}
		if len(st.Assign.Field) == 1 && len(st.Assign.Index) == 0 {
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			key := &AtomLit{Name: ":" + st.Assign.Field[0].Name}
			call := &CallExpr{Func: "Map.put", Args: []Expr{&VarRef{Name: st.Assign.Name}, key, val}}
			return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
		}
		if len(st.Assign.Index) == 1 && len(st.Assign.Field) == 0 {
			idx, err := compileExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			t, _ := env.GetVar(st.Assign.Name)
			var call *CallExpr
			switch t.(type) {
			case types.MapType:
				call = &CallExpr{Func: "Map.put", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, val}}
			default:
				// Fallback to list semantics when the type is unknown
				call = &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, val}}
			}
			return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
		}
		if len(st.Assign.Index) == 2 && len(st.Assign.Field) == 0 {
			idx0, err := compileExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			idx1, err := compileExpr(st.Assign.Index[1].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			t, _ := env.GetVar(st.Assign.Name)
			if outerList, ok := t.(types.ListType); ok {
				if _, ok := outerList.Elem.(types.ListType); ok {
					inner := &CallExpr{Func: "Enum.at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0}}
					innerUpdate := &CallExpr{Func: "List.replace_at", Args: []Expr{inner, idx1, val}}
					call := &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0, innerUpdate}}
					return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
				}
			}
			if outerMap, ok := t.(types.MapType); ok {
				if _, ok := outerMap.Value.(types.MapType); ok {
					inner := &IndexExpr{Target: &VarRef{Name: st.Assign.Name}, Index: idx0, UseMapSyntax: true}
					innerUpdate := &CallExpr{Func: "Map.put", Args: []Expr{inner, idx1, val}}
					call := &CallExpr{Func: "Map.put", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0, innerUpdate}}
					return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
				}
			}
			return nil, fmt.Errorf("unsupported indexed assignment at %d:%d", st.Pos.Line, st.Pos.Column)
		}
		return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
	case st.If != nil:
		if st.If.ElseIf == nil && len(st.If.Then) == 1 && len(st.If.Else) == 1 {
			th := st.If.Then[0]
			el := st.If.Else[0]
			if th.Assign != nil && el.Assign != nil &&
				th.Assign.Name == el.Assign.Name &&
				len(th.Assign.Index) == 0 && len(th.Assign.Field) == 0 &&
				len(el.Assign.Index) == 0 && len(el.Assign.Field) == 0 {
				cond, err := compileExpr(st.If.Cond, env)
				if err != nil {
					return nil, err
				}
				thenExpr, err := compileExpr(th.Assign.Value, env)
				if err != nil {
					return nil, err
				}
				elseExpr, err := compileExpr(el.Assign.Value, env)
				if err != nil {
					return nil, err
				}
				ce := &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}
				return &AssignStmt{Name: th.Assign.Name, Value: ce}, nil
			}
		}
		return compileIfStmt(st.If, env)
	case st.While != nil:
		return compileWhileStmt(st.While, env)
	case st.For != nil:
		return compileForStmt(st.For, env)
	case st.Update != nil:
		return compileUpdateStmt(st.Update, env)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Return != nil:
		var val Expr
		if st.Return.Value != nil {
			var err error
			val, err = compileExpr(st.Return.Value, env)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: val}, nil
	case st.Fun != nil:
		child := types.NewEnv(env)
		for _, p := range st.Fun.Params {
			if p.Type != nil {
				child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), false)
			} else {
				child.SetVar(p.Name, types.AnyType{}, false)
			}
		}
		funcDepth++
		body := make([]Stmt, 0, len(st.Fun.Body))
		for _, b := range st.Fun.Body {
			bs, err := compileStmt(b, child)
			if err != nil {
				funcDepth--
				return nil, err
			}
			if bs != nil {
				body = append(body, bs)
			}
		}
		funcDepth--
		params := make([]string, len(st.Fun.Params))
		for i, p := range st.Fun.Params {
			params[i] = p.Name
		}
		if len(body) == 2 {
			if ifs, ok := body[0].(*IfStmt); ok && len(ifs.Then) == 1 {
				if ret1, ok := ifs.Then[0].(*ReturnStmt); ok {
					if ret2, ok := body[1].(*ReturnStmt); ok {
						cond := ifs.Cond
						thenExpr := ret1.Value
						elseExpr := ret2.Value
						ce := &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}
						body = []Stmt{&ReturnStmt{Value: ce}}
					}
				}
			}
		}
		if funcDepth > 0 {
			env.SetVar(st.Fun.Name, types.FuncType{}, false)
			return &AssignStmt{Name: st.Fun.Name, Value: &AnonFun{Params: params, Body: body}}, nil
		}
		return &FuncDecl{Name: st.Fun.Name, Params: params, Body: body}, nil
	case st.ExternVar != nil:
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.Type != nil:
		// Type declarations are ignored by this simple transpiler
		return nil, nil
	default:
		if st.Test == nil && st.Import == nil && st.Type == nil {
			return nil, fmt.Errorf("unsupported statement at %d:%d", st.Pos.Line, st.Pos.Column)
		}
	}
	return nil, nil
}

func compileIfStmt(is *parser.IfStmt, env *types.Env) (Stmt, error) {
	cond, err := compileExpr(is.Cond, env)
	if err != nil {
		return nil, err
	}
	envBefore := env.Copy()
	thenStmts := make([]Stmt, 0, len(is.Then))
	for _, s := range is.Then {
		st, err := compileStmt(s, env)
		if err != nil {
			return nil, err
		}
		if st != nil {
			thenStmts = append(thenStmts, st)
		}
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		elseStmt, err := compileIfStmt(is.ElseIf, env)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{elseStmt}
	} else if len(is.Else) > 0 {
		for _, s := range is.Else {
			st, err := compileStmt(s, env)
			if err != nil {
				return nil, err
			}
			if st != nil {
				elseStmts = append(elseStmts, st)
			}
		}
	}
	vars := gatherMutVars(append(append([]Stmt{}, thenStmts...), elseStmts...), envBefore)
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts, Vars: vars}, nil
}

func compileWhileStmt(ws *parser.WhileStmt, env *types.Env) (Stmt, error) {
	cond, err := compileExpr(ws.Cond, env)
	if err != nil {
		return nil, err
	}
	bodyEnv := types.NewEnv(env)
	body := make([]Stmt, 0, len(ws.Body))
	for _, s := range ws.Body {
		st, err := compileStmt(s, bodyEnv)
		if err != nil {
			return nil, err
		}
		if st != nil {
			body = append(body, st)
		}
	}
	vars := gatherMutVars(body, env)
	simple := true
	var check func([]Stmt)
	check = func(sts []Stmt) {
		for _, st := range sts {
			switch t := st.(type) {
			case *BreakStmt, *ContinueStmt:
				simple = false
			case *IfStmt:
				check(t.Then)
				check(t.Else)
			case *ForStmt:
				check(t.Body)
			case *WhileStmt:
				check(t.Body)
			}
		}
	}
	check(body)
	return &WhileStmt{Cond: cond, Body: body, Vars: vars, Simple: simple}, nil
}

func gatherMutVars(stmts []Stmt, env *types.Env) []string {
	set := map[string]struct{}{}
	var walk func([]Stmt)
	walk = func(ss []Stmt) {
		for _, s := range ss {
			switch t := s.(type) {
			case *AssignStmt:
                               if _, err := env.GetVar(t.Name); err == nil {
                                       if _, ok := globalVars[t.Name]; !ok {
                                               set[t.Name] = struct{}{}
                                       }
                               }
			case *IfStmt:
                               for _, v := range t.Vars {
                                       if _, err := env.GetVar(v); err == nil {
                                               if _, ok := globalVars[v]; !ok {
                                                       set[v] = struct{}{}
                                               }
                                       }
                               }
				walk(t.Then)
				walk(t.Else)
			case *ForStmt:
				walk(t.Body)
			case *WhileStmt:
				walk(t.Body)
			}
		}
	}
	walk(stmts)
	vars := make([]string, 0, len(set))
	for v := range set {
		vars = append(vars, v)
	}
	sort.Strings(vars)
	return vars
}

func compileForStmt(fs *parser.ForStmt, env *types.Env) (Stmt, error) {
	start, err := compileExpr(fs.Source, env)
	if err != nil {
		return nil, err
	}
	var end Expr
	if fs.RangeEnd != nil {
		end, err = compileExpr(fs.RangeEnd, env)
		if err != nil {
			return nil, err
		}
	}
	bodyEnv := types.NewEnv(env)
	body := make([]Stmt, 0, len(fs.Body))
	for _, s := range fs.Body {
		st, err := compileStmt(s, bodyEnv)
		if err != nil {
			return nil, err
		}
		if st != nil {
			body = append(body, st)
		}
	}
	src := start
	if fs.RangeEnd == nil {
		if _, ok := types.TypeOfExprBasic(fs.Source, env).(types.MapType); ok {
			src = &CallExpr{Func: "Map.keys", Args: []Expr{start}}
		}
	}
	if end != nil {
		src = nil
	}
	simple := true
	var check func([]Stmt)
	check = func(stmts []Stmt) {
		for _, s := range stmts {
			switch t := s.(type) {
			case *BreakStmt, *ContinueStmt:
				simple = false
			case *IfStmt:
				check(t.Then)
				check(t.Else)
			case *ForStmt:
				check(t.Body)
			case *WhileStmt:
				check(t.Body)
			}
		}
	}
	check(body)
	vars := gatherMutVars(body, env)
	res := &ForStmt{Name: fs.Name, Start: start, End: end, Source: src, Body: body, Vars: vars, Simple: simple}
	return res, nil
}

func compileUpdateStmt(us *parser.UpdateStmt, env *types.Env) (Stmt, error) {
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
	for _, f := range st.Order {
		child.SetVar(f, st.Fields[f], false)
	}
	fieldSet := map[string]bool{}
	for _, f := range st.Order {
		fieldSet[f] = true
	}
	fields := make([]string, len(us.Set.Items))
	values := make([]Expr, len(us.Set.Items))
	for i, it := range us.Set.Items {
		name, ok := identName(it.Key)
		if !ok {
			name, ok = literalString(it.Key)
			if !ok {
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := compileExpr(it.Value, child)
		if err != nil {
			return nil, err
		}
		val = substituteFieldRefs(val, fieldSet)
		fields[i] = name
		values[i] = val
	}
	var cond Expr
	if us.Where != nil {
		var err error
		cond, err = compileExpr(us.Where, child)
		if err != nil {
			return nil, err
		}
		cond = substituteFieldRefs(cond, fieldSet)
	}
	return &UpdateStmt{Target: us.Target, Fields: fields, Values: values, Cond: cond}, nil
}

func compileFunExpr(fn *parser.FunExpr, env *types.Env) (Expr, error) {
	child := types.NewEnv(env)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
		if p.Type != nil {
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), false)
		} else {
			child.SetVar(p.Name, types.AnyType{}, false)
		}
	}
	var body []Stmt
	if fn.ExprBody != nil {
		ex, err := compileExpr(fn.ExprBody, child)
		if err != nil {
			return nil, err
		}
		body = []Stmt{&ExprStmt{Expr: ex}}
	} else {
		for _, s := range fn.BlockBody {
			st, err := compileStmt(s, child)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
	}
	return &AnonFun{Params: params, Body: body}, nil
}

func compileExpr(e *parser.Expr, env *types.Env) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return compileBinary(e.Binary, env)
}

func compileIfExpr(ie *parser.IfExpr, env *types.Env) (Expr, error) {
	cond, err := compileExpr(ie.Cond, env)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(ie.Then, env)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = compileIfExpr(ie.ElseIf, env)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = compileExpr(ie.Else, env)
		if err != nil {
			return nil, err
		}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func compileMatchExpr(me *parser.MatchExpr, env *types.Env) (Expr, error) {
	target, err := compileExpr(me.Target, env)
	if err != nil {
		return nil, err
	}
	clauses := make([]CaseClause, len(me.Cases))
	for i, c := range me.Cases {
		var pat Expr
		if !isUnderscoreExpr(c.Pattern) {
			p, err := compilePattern(c.Pattern, env)
			if err != nil {
				return nil, err
			}
			pat = p
		}
		res, err := compileExpr(c.Result, env)
		if err != nil {
			return nil, err
		}
		clauses[i] = CaseClause{Pattern: pat, Result: res}
	}
	return &CaseExpr{Target: target, Clauses: clauses}, nil
}

func compilePattern(e *parser.Expr, env *types.Env) (Expr, error) {
	if name, ok := isSimpleIdent(e); ok {
		if _, err := env.GetVar(name); err != nil {
			if _, ok := env.GetStruct(name); ok {
				return &AtomLit{Name: ":" + name}, nil
			}
		}
	}

	if call, ok := isSimpleCall(e); ok {
		if st, ok2 := env.GetStruct(call.Func); ok2 && len(call.Args) == len(st.Order) {
			items := make([]MapItem, len(call.Args))
			for i, a := range call.Args {
				val, err := compilePattern(a, env)
				if err != nil {
					return nil, err
				}
				items[i] = MapItem{Key: &AtomLit{Name: st.Order[i]}, Value: val}
			}
			return &MapLit{Items: items}, nil
		}
	}
	return compileExpr(e, env)
}

func compileQueryExpr(q *parser.QueryExpr, env *types.Env) (Expr, error) {
	src, err := compileExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	env.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
	if name, ok := isSimpleIdent(q.Source); ok {
		if t, err := env.GetVar(name); err == nil {
			if _, ok := t.(types.GroupType); ok {
				src = &IndexExpr{Target: &VarRef{Name: name}, Index: &AtomLit{Name: "items"}, UseMapSyntax: true}
			}
		}
	}
	if q.Group != nil && len(q.Group.Exprs) == 1 && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
		key, err := compileExpr(q.Group.Exprs[0], child)
		if err != nil {
			return nil, err
		}
		genv := types.NewEnv(env)
		genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
		sel, err := compileExpr(q.Select, genv)
		if err != nil {
			return nil, err
		}
		if q.Group.Having == nil {
			return &GroupByExpr{Var: q.Var, Source: src, Key: key, Name: q.Group.Name, Select: sel}, nil
		}
		having, err := compileExpr(q.Group.Having, genv)
		if err != nil {
			return nil, err
		}
		return &GroupByHavingExpr{Var: q.Var, Source: src, Key: key, Name: q.Group.Name, Having: having, Select: sel}, nil
	}
	if q.Group != nil && len(q.Group.Exprs) == 1 && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort != nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
		key, err := compileExpr(q.Group.Exprs[0], child)
		if err != nil {
			return nil, err
		}
		genv := types.NewEnv(env)
		genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
		sel, err := compileExpr(q.Select, genv)
		if err != nil {
			return nil, err
		}
		sortExpr, err := compileExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		return &GroupBySortExpr{Var: q.Var, Source: src, Key: key, Name: q.Group.Name, Sort: sortExpr, Select: sel}, nil
	}

	if q.Group != nil && len(q.Group.Exprs) == 1 && len(q.Froms) == 0 && len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		lj := q.Joins[0]
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
		je, err := compileExpr(lj.Src, child)
		if err != nil {
			return nil, err
		}
		child.SetVar(lj.Var, elemTypeOfExpr(lj.Src, child), true)
		onExpr, err := compileExpr(lj.On, child)
		if err != nil {
			return nil, err
		}
		items := []MapItem{
			{Key: &AtomLit{Name: q.Var}, Value: &VarRef{Name: q.Var}},
			{Key: &AtomLit{Name: lj.Var}, Value: &VarRef{Name: lj.Var}},
		}
		selRow := &MapLit{Items: items}
		joinExpr := &LeftJoinExpr{LeftVar: q.Var, LeftSrc: src, RightVar: lj.Var, RightSrc: je, On: onExpr, Select: selRow}

		child2 := types.NewEnv(env)
		child2.SetVar(q.Var, types.AnyType{}, true)
		child2.SetVar(lj.Var, types.AnyType{}, true)
		key, err := compileExpr(q.Group.Exprs[0], child2)
		if err != nil {
			return nil, err
		}
		genv := types.NewEnv(env)
		genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
		sel, err := compileExpr(q.Select, genv)
		if err != nil {
			return nil, err
		}
		pattern := fmt.Sprintf("%%{%s: %s, %s: %s}", q.Var, q.Var, lj.Var, lj.Var)
		return &GroupByExpr{Var: pattern, Source: joinExpr, Key: key, Name: q.Group.Name, Select: sel}, nil
	}

	if q.Group != nil && len(q.Group.Exprs) == 1 && (len(q.Froms) > 0 || len(q.Joins) > 0) && q.Skip == nil && q.Take == nil && !q.Distinct {
		gens := []CompGen{{Var: q.Var, Src: src}}
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
		varNames := []string{q.Var}
		var filt Expr
		for _, f := range q.Froms {
			fe, err := compileExpr(f.Src, child)
			if err != nil {
				return nil, err
			}
			child.SetVar(f.Var, elemTypeOfExpr(f.Src, child), true)
			varNames = append(varNames, f.Var)
			gens = append(gens, CompGen{Var: f.Var, Src: fe})
		}
		for _, j := range q.Joins {
			if j.Side != nil {
				return nil, fmt.Errorf("join side not supported")
			}
			je, err := compileExpr(j.Src, child)
			if err != nil {
				return nil, err
			}
			child.SetVar(j.Var, elemTypeOfExpr(j.Src, child), true)
			varNames = append(varNames, j.Var)
			gens = append(gens, CompGen{Var: j.Var, Src: je})
			if j.On != nil {
				onExpr, err := compileExpr(j.On, child)
				if err != nil {
					return nil, err
				}
				if onExpr != nil {
					if filt == nil {
						filt = onExpr
					} else {
						filt = &BinaryExpr{Left: filt, Op: "&&", Right: onExpr}
					}
				}
			}
		}
		if q.Where != nil {
			filt, err = compileExpr(q.Where, child)
			if err != nil {
				return nil, err
			}
		}
		items := make([]MapItem, len(varNames))
		for i, name := range varNames {
			items[i] = MapItem{Key: &AtomLit{Name: name}, Value: &VarRef{Name: name}}
		}
		body := &MapLit{Items: items}
		comp := &Comprehension{Gens: gens, Filter: filt, Body: body}

		key, err := compileExpr(q.Group.Exprs[0], child)
		if err != nil {
			return nil, err
		}
		genv := types.NewEnv(env)
		genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: types.AnyType{}}, true)
		sel, err := compileExpr(q.Select, genv)
		if err != nil {
			return nil, err
		}
		used := map[string]struct{}{}
		gatherVarsExpr(q.Group.Exprs[0], used)
		patParts := make([]string, len(varNames))
		for i, n := range varNames {
			if _, ok := used[n]; ok {
				patParts[i] = fmt.Sprintf("%s: %s", n, n)
			} else {
				patParts[i] = fmt.Sprintf("%s: _%s", n, n)
			}
		}
		pattern := fmt.Sprintf("%%{%s}", strings.Join(patParts, ", "))
		if q.Sort == nil {
			return &GroupByExpr{Var: pattern, Source: comp, Key: key, Name: q.Group.Name, Select: sel}, nil
		}
		sortExpr, err := compileExpr(q.Sort, genv)
		if err != nil {
			return nil, err
		}
		return &GroupBySortExpr{Var: pattern, Source: comp, Key: key, Name: q.Group.Name, Sort: sortExpr, Select: sel}, nil
	}
	if len(q.Joins) == 2 && q.Joins[0].Side == nil && q.Joins[1].Side != nil && *q.Joins[1].Side == "left" && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil && !q.Distinct {
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
		j0 := q.Joins[0]
		js0, err := compileExpr(j0.Src, child)
		if err != nil {
			return nil, err
		}
		child.SetVar(j0.Var, elemTypeOfExpr(j0.Src, child), true)
		on0, err := compileExpr(j0.On, child)
		if err != nil {
			return nil, err
		}
		j1 := q.Joins[1]
		js1, err := compileExpr(j1.Src, child)
		if err != nil {
			return nil, err
		}
		child.SetVar(j1.Var, elemTypeOfExpr(j1.Src, child), true)
		on1, err := compileExpr(j1.On, child)
		if err != nil {
			return nil, err
		}
		sel, err := compileExpr(q.Select, child)
		if err != nil {
			return nil, err
		}
		filter0 := &CallExpr{Func: "Enum.filter", Args: []Expr{js0, &AnonFun{Params: []string{j0.Var}, Body: []Stmt{&ReturnStmt{Value: on0}}}}}
		letMatches := &LetStmt{Name: "matches", Value: &CallExpr{Func: "Enum.filter", Args: []Expr{js1, &AnonFun{Params: []string{j1.Var}, Body: []Stmt{&ReturnStmt{Value: on1}}}}}}
		condEmpty := &CallExpr{Func: "Enum.empty?", Args: []Expr{&VarRef{Name: "matches"}}}
		listExpr := &CondExpr{Cond: condEmpty, Then: &ListLit{Elems: []Expr{&NilLit{}}}, Else: &VarRef{Name: "matches"}}
		letList := &LetStmt{Name: "list", Value: listExpr}
		mapFn := &AnonFun{Params: []string{j1.Var}, Body: []Stmt{&ReturnStmt{Value: sel}}}
		mapCall := &CallExpr{Func: "Enum.map", Args: []Expr{&VarRef{Name: "list"}, mapFn}}
		innerFun := &AnonFun{Params: []string{j0.Var}, Body: []Stmt{letMatches, letList, &ReturnStmt{Value: mapCall}}}
		innerFlat := &CallExpr{Func: "Enum.flat_map", Args: []Expr{filter0, innerFun}}
		outerFun := &AnonFun{Params: []string{q.Var}, Body: []Stmt{&ReturnStmt{Value: innerFlat}}}
		return &CallExpr{Func: "Enum.flat_map", Args: []Expr{src, outerFun}}, nil
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "outer" && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil && !q.Distinct {
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
		j := q.Joins[0]
		je, err := compileExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		child.SetVar(j.Var, elemTypeOfExpr(j.Src, child), true)
		onExpr, err := compileExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		sel, err := compileExpr(q.Select, child)
		if err != nil {
			return nil, err
		}
		return &OuterJoinExpr{LeftVar: q.Var, LeftSrc: src, RightVar: j.Var, RightSrc: je, On: onExpr, Select: sel}, nil
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil && !q.Distinct {
		child := types.NewEnv(env)
		child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
		j := q.Joins[0]
		je, err := compileExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		child.SetVar(j.Var, elemTypeOfExpr(j.Src, child), true)
		onExpr, err := compileExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		sel, err := compileExpr(q.Select, child)
		if err != nil {
			return nil, err
		}
		return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: src, RightVar: j.Var, RightSrc: je, On: onExpr, Select: sel}, nil
	}
	gens := []CompGen{{Var: q.Var, Src: src}}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elemTypeOfExpr(q.Source, env), true)
	var filt Expr
	for _, f := range q.Froms {
		fe, err := compileExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		child.SetVar(f.Var, elemTypeOfExpr(f.Src, child), true)
		gens = append(gens, CompGen{Var: f.Var, Src: fe})
	}
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side == "right" && len(q.Froms) == 0 {
			je, err := compileExpr(j.Src, child)
			if err != nil {
				return nil, err
			}
			child.SetVar(j.Var, elemTypeOfExpr(j.Src, child), true)
			gens = append([]CompGen{{Var: j.Var, Src: je}}, gens...)
			if j.On != nil {
				onExpr, err := compileExpr(j.On, child)
				if err != nil {
					return nil, err
				}
				if onExpr != nil {
					if filt == nil {
						filt = onExpr
					} else {
						filt = &BinaryExpr{Left: filt, Op: "&&", Right: onExpr}
					}
				}
			}
			continue
		}
		if j.Side != nil {
			return nil, fmt.Errorf("join side not supported")
		}
		je, err := compileExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		child.SetVar(j.Var, elemTypeOfExpr(j.Src, child), true)
		gens = append(gens, CompGen{Var: j.Var, Src: je})
		if j.On != nil {
			onExpr, err := compileExpr(j.On, child)
			if err != nil {
				return nil, err
			}
			if onExpr != nil {
				if filt == nil {
					filt = onExpr
				} else {
					filt = &BinaryExpr{Left: filt, Op: "&&", Right: onExpr}
				}
			}
		}
	}
	if q.Where != nil {
		filt, err = compileExpr(q.Where, child)
		if err != nil {
			return nil, err
		}
	}
	sel, err := compileExpr(q.Select, child)
	if err != nil {
		return nil, err
	}
	var base Expr
	if ce, ok := sel.(*CallExpr); ok && len(ce.Args) == 1 {
		switch ce.Func {
		case "Enum.sum", "Enum.min", "Enum.max", "Enum.count", "Enum.avg":
			comp := &Comprehension{Gens: gens, Filter: filt, Body: ce.Args[0]}
			base = &CallExpr{Func: ce.Func, Args: []Expr{comp}}
		}
	}
	if base == nil {
		base = Expr(&Comprehension{Gens: gens, Filter: filt, Body: sel})
	}
	if q.Sort != nil {
		key, err := compileExpr(q.Sort, child)
		if err != nil {
			return nil, err
		}
		pairBody := &MapLit{Items: []MapItem{{Key: &AtomLit{Name: "k"}, Value: key}, {Key: &AtomLit{Name: "v"}, Value: sel}}}
		pairComp := &Comprehension{Gens: gens, Filter: filt, Body: pairBody}
		sorter := &CallExpr{Func: "Enum.sort_by", Args: []Expr{pairComp, &AnonFun{Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Value: &IndexExpr{Target: &VarRef{Name: "x"}, Index: &AtomLit{Name: "k"}, UseMapSyntax: true}}}}}}
		base = sorter
		if q.Skip != nil {
			s, err := compileExpr(q.Skip, env)
			if err != nil {
				return nil, err
			}
			base = &CallExpr{Func: "Enum.drop", Args: []Expr{base, s}}
		}
		if q.Take != nil {
			texpr, err := compileExpr(q.Take, env)
			if err != nil {
				return nil, err
			}
			base = &CallExpr{Func: "Enum.take", Args: []Expr{base, texpr}}
		}
		mapper := &CallExpr{Func: "Enum.map", Args: []Expr{base, &AnonFun{Params: []string{"x"}, Body: []Stmt{&ReturnStmt{Value: &IndexExpr{Target: &VarRef{Name: "x"}, Index: &AtomLit{Name: "v"}, UseMapSyntax: true}}}}}}
		return mapper, nil
	}
	if q.Skip != nil {
		s, err := compileExpr(q.Skip, env)
		if err != nil {
			return nil, err
		}
		base = &CallExpr{Func: "Enum.drop", Args: []Expr{base, s}}
	}
	if q.Take != nil {
		texpr, err := compileExpr(q.Take, env)
		if err != nil {
			return nil, err
		}
		base = &CallExpr{Func: "Enum.take", Args: []Expr{base, texpr}}
	}
	return base, nil
}

func compileUnary(u *parser.Unary, env *types.Env) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := compilePostfix(u.Value, env)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = &UnaryExpr{Op: u.Ops[i], Expr: expr}
	}
	return expr, nil
}

func compileBinary(b *parser.BinaryExpr, env *types.Env) (Expr, error) {
	left, err := compileUnary(b.Left, env)
	if err != nil {
		return nil, err
	}
	operands := []Expr{left}
	typesSlice := []types.Type{types.TypeOfUnary(b.Left, env)}
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		expr, err := compilePostfix(op.Right, env)
		if err != nil {
			return nil, err
		}
		ops[i] = op
		operands = append(operands, expr)
		typesSlice = append(typesSlice, types.TypeOfPostfix(op.Right, env))
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
	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}
	for _, level := range levels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i].Op) {
				opName := ops[i].Op
				if opName == "union" && ops[i].All {
					opName = "union_all"
				}
				var expr Expr
				switch opName {
				case "union":
					concat := &BinaryExpr{Left: operands[i], Op: "++", Right: operands[i+1]}
					expr = &CallExpr{Func: "Enum.uniq", Args: []Expr{concat}}
				case "union_all":
					expr = &BinaryExpr{Left: operands[i], Op: "++", Right: operands[i+1]}
				case "except":
					param := "x"
					cond := &UnaryExpr{Op: "!", Expr: &CallExpr{Func: "Enum.member?", Args: []Expr{operands[i+1], &VarRef{Name: param}}}}
					fn := &AnonFun{Params: []string{param}, Body: []Stmt{&ExprStmt{Expr: cond}}}
					expr = &CallExpr{Func: "Enum.filter", Args: []Expr{operands[i], fn}}
				case "intersect":
					param := "x"
					cond := &CallExpr{Func: "Enum.member?", Args: []Expr{operands[i+1], &VarRef{Name: param}}}
					fn := &AnonFun{Params: []string{param}, Body: []Stmt{&ExprStmt{Expr: cond}}}
					expr = &CallExpr{Func: "Enum.filter", Args: []Expr{operands[i], fn}}
				default:
					bin := &BinaryExpr{Left: operands[i], Op: opName, Right: operands[i+1]}
					if opName == "in" {
						if _, ok := types.TypeOfPostfix(ops[i].Right, env).(types.MapType); ok {
							bin.MapIn = true
						}
					} else if opName == "+" {
						if _, ok := typesSlice[i].(types.StringType); ok {
							bin.StrConcat = true
						}
						if _, ok := typesSlice[i+1].(types.StringType); ok {
							bin.StrConcat = true
						}
					}
					expr = bin
				}
				operands[i] = expr
				if b, ok := expr.(*BinaryExpr); ok && b.Op == "+" && b.StrConcat {
					typesSlice[i] = types.StringType{}
				} else {
					typesSlice[i] = types.AnyType{}
				}
				operands = append(operands[:i+1], operands[i+2:]...)
				typesSlice = append(typesSlice[:i+1], typesSlice[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return nil, fmt.Errorf("invalid expression")
	}
	return operands[0], nil
}

func compilePostfix(pf *parser.PostfixExpr, env *types.Env) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		if pf.Target.Selector.Tail[0] == "contains" {
			arg, err := compileExpr(pf.Ops[0].Call.Args[0], env)
			if err != nil {
				return nil, err
			}
			base, err := compilePrimary(&parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}, env)
			if err != nil {
				return nil, err
			}
			expr := &CallExpr{Func: "String.contains?", Args: []Expr{base, arg}}
			return expr, nil
		}
		args := make([]Expr, len(pf.Ops[0].Call.Args))
		for i, a := range pf.Ops[0].Call.Args {
			ce, err := compileExpr(a, env)
			if err != nil {
				return nil, err
			}
			args[i] = ce
		}
		alias := pf.Target.Selector.Root
		method := pf.Target.Selector.Tail[0]
		if kind, ok := builtinAliases[alias]; ok {
			switch kind {
			case "go_testpkg":
				if method == "Add" && len(args) == 2 {
					return &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}, nil
				}
			case "python_math":
				switch method {
				case "sqrt", "sin", "log":
					if len(args) == 1 {
						return &CallExpr{Func: ":math." + strings.ToLower(method), Args: args}, nil
					}
				case "pow":
					if len(args) == 2 {
						return &CallExpr{Func: ":math.pow", Args: args}, nil
					}
				}
			}
		}
		funcName := alias + "." + method
		return &CallExpr{Func: funcName, Args: args}, nil
	}
	expr, err := compilePrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	typ := types.TypeOfPrimary(pf.Target, env)
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				typName := *op.Cast.Type.Simple
				if _, ok := env.GetStruct(typName); ok {
					if ml, ok := expr.(*MapLit); ok {
						for i, it := range ml.Items {
							if s, ok := it.Key.(*StringLit); ok {
								ml.Items[i].Key = &AtomLit{Name: s.Value}
							}
						}
					}
				} else {
					expr = &CastExpr{Expr: expr, Type: typName}
				}
			} else {
				return nil, fmt.Errorf("unsupported cast")
			}
		} else if op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil {
			idx, err := compileExpr(op.Index.Start, env)
			if err != nil {
				return nil, err
			}
			switch tt := typ.(type) {
			case types.StringType:
				expr = &IndexExpr{Target: expr, Index: idx, IsString: true}
				typ = types.StringType{}
			case types.ListType:
				expr = &IndexExpr{Target: expr, Index: idx, IsString: false}
				typ = tt.Elem
			case types.MapType:
				expr = &IndexExpr{Target: expr, Index: idx, UseMapSyntax: true}
				typ = tt.Value
			default:
				expr = &IndexExpr{Target: expr, Index: idx}
				typ = types.AnyType{}
			}
		} else if op.Index != nil && (op.Index.Colon != nil || op.Index.Colon2 != nil) {
			var start Expr = &NumberLit{Value: "0"}
			if op.Index.Start != nil {
				s, err := compileExpr(op.Index.Start, env)
				if err != nil {
					return nil, err
				}
				start = s
			}
			var end Expr
			var diff Expr
			base := expr
			if op.Index.End != nil {
				var err error
				end, err = compileExpr(op.Index.End, env)
				if err != nil {
					return nil, err
				}
				diff = &BinaryExpr{Left: end, Op: "-", Right: start}
			} else {
				var lengthCall *CallExpr
				switch typ.(type) {
				case types.StringType:
					lengthCall = &CallExpr{Func: "String.length", Args: []Expr{base}}
				case types.ListType:
					lengthCall = &CallExpr{Func: "length", Args: []Expr{base}}
				default:
					lengthCall = &CallExpr{Func: "Enum.count", Args: []Expr{base}}
				}
				diff = &BinaryExpr{Left: lengthCall, Op: "-", Right: start}
			}
			switch tt := typ.(type) {
			case types.StringType:
				expr = &CallExpr{Func: "String.slice", Args: []Expr{base, start, diff}}
				typ = types.StringType{}
			case types.ListType:
				expr = &CallExpr{Func: "Enum.slice", Args: []Expr{base, start, diff}}
				typ = tt.Elem
			default:
				expr = &CallExpr{Func: "Enum.slice", Args: []Expr{base, start, diff}}
				typ = types.AnyType{}
			}
		} else if op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("unsupported contains call")
			}
			arg, err := compileExpr(call.Args[0], env)
			if err != nil {
				return nil, err
			}
			expr = &CallExpr{Func: "String.contains?", Args: []Expr{expr, arg}}
			typ = types.BoolType{}
			i++
		} else if op.Field != nil && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
			call := pf.Ops[i+1].Call
			args := make([]Expr, len(call.Args))
			for j, a := range call.Args {
				ce, err := compileExpr(a, env)
				if err != nil {
					return nil, err
				}
				args[j] = ce
			}
			if v, ok := expr.(*VarRef); ok {
				methodName := v.Name + "." + op.Field.Name
				expr = &CallExpr{Func: methodName, Args: args}
			} else {
				// treat as map field returning function and call it
				expr = &CallExpr{Func: "" /*unused*/, Args: append([]Expr{expr}, args...)}
			}
			typ = types.AnyType{}
			i++
		} else if op.Field != nil {
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}, UseMapSyntax: true}
			switch tt := typ.(type) {
			case types.StructType:
				if t, ok := tt.Fields[op.Field.Name]; ok {
					typ = t
				} else {
					typ = types.AnyType{}
				}
			case types.MapType:
				typ = tt.Value
			default:
				typ = types.AnyType{}
			}
		} else {
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func compilePrimary(p *parser.Primary, env *types.Env) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a, env)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		if idx := strings.Index(name, "."); idx > 0 {
			alias := name[:idx]
			method := name[idx+1:]
			if kind, ok := builtinAliases[alias]; ok {
				switch kind {
				case "go_testpkg":
					if method == "Add" && len(args) == 2 {
						return &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}, nil
					}
				case "python_math":
					switch method {
					case "sqrt", "sin", "log":
						if len(args) == 1 {
							return &CallExpr{Func: ":math." + strings.ToLower(method), Args: args}, nil
						}
					case "pow":
						if len(args) == 2 {
							return &CallExpr{Func: ":math.pow", Args: args}, nil
						}
					}
				}
			}
		}
		switch name {
		case "print":
			if len(args) == 1 {
				t := types.TypeOfExprBasic(p.Call.Args[0], env)
				switch t.(type) {
				case types.StringType, types.IntType, types.FloatType, types.BoolType:
					return &CallExpr{Func: "IO.puts", Args: []Expr{args[0]}}, nil
				default:
					return &CallExpr{Func: "IO.inspect", Args: []Expr{args[0]}}, nil
				}
			} else {
				parts := make([]interface{}, 0, len(args)*2-1)
				for i, a := range args {
					if i > 0 {
						parts = append(parts, " ")
					}
					if s, ok := a.(*StringLit); ok {
						parts = append(parts, s.Value)
					} else {
						t := types.TypeOfExprBasic(p.Call.Args[i], env)
						switch t.(type) {
						case types.StringType, types.IntType, types.FloatType, types.BoolType:
							parts = append(parts, a)
						default:
							parts = append(parts, &CallExpr{Func: "Kernel.to_string", Args: []Expr{a}})
						}
					}
				}
				str := &InterpString{Parts: parts}
				return &CallExpr{Func: "IO.puts", Args: []Expr{str}}, nil
			}
		case "count":
			name = "Enum.count"
			if len(args) == 1 {
				if v, ok := isSimpleIdent(p.Call.Args[0]); ok {
					if t, err := env.GetVar(v); err == nil {
						if _, ok := t.(types.GroupType); ok {
							arg := &IndexExpr{Target: &VarRef{Name: v}, Index: &AtomLit{Name: "items"}, UseMapSyntax: true}
							return &CallExpr{Func: name, Args: []Expr{arg}}, nil
						}
					}
				}
			}
		case "len":
			name = "length"
			if len(args) == 1 {
				t := types.TypeOfExprBasic(p.Call.Args[0], env)
				if _, ok := t.(types.StringType); ok {
					name = "String.length"
				} else if _, ok := t.(types.MapType); ok {
					name = "map_size"
				}
			}
		case "sum":
			name = "Enum.sum"
		case "min":
			name = "Enum.min"
		case "max":
			name = "Enum.max"
		case "avg":
			if len(args) == 1 {
				sumCall := &CallExpr{Func: "Enum.sum", Args: []Expr{args[0]}}
				countCall := &CallExpr{Func: "Enum.count", Args: []Expr{args[0]}}
				remCall := &BinaryExpr{Left: sumCall, Op: "%", Right: countCall}
				cond := &BinaryExpr{Left: remCall, Op: "==", Right: &NumberLit{Value: "0"}}
				intDiv := &CallExpr{Func: "div", Args: []Expr{sumCall, countCall}}
				divExpr := &BinaryExpr{Left: sumCall, Op: "/", Right: countCall}
				cexpr := &CondExpr{Cond: cond, Then: intDiv, Else: divExpr}
				return &GroupExpr{Expr: cexpr}, nil
			}
		case "str":
			name = "to_string"
		case "append":
			if len(args) == 2 {
				list := args[0]
				elemList := &ListLit{Elems: []Expr{args[1]}}
				return &BinaryExpr{Left: list, Op: "++", Right: elemList}, nil
			}
		case "now":
			if len(args) == 0 {
				return &CallExpr{Func: "_now", Args: nil}, nil
			}
		case "input":
			if len(args) == 0 {
				gets := &CallExpr{Func: "IO.gets", Args: []Expr{&StringLit{Value: ""}}}
				trim := &CallExpr{Func: "String.trim", Args: []Expr{gets}}
				return trim, nil
			}
		case "values":
			if len(args) == 1 {
				inner := &CallExpr{Func: "Map.values", Args: []Expr{args[0]}}
				sort := &CallExpr{Func: "Enum.sort", Args: []Expr{inner}}
				return &CallExpr{Func: "Enum.join", Args: []Expr{sort, &StringLit{Value: " "}}}, nil
			}
		case "substring":
			if len(args) == 3 {
				diff := &BinaryExpr{Left: args[2], Op: "-", Right: args[1]}
				return &CallExpr{Func: "String.slice", Args: []Expr{args[0], args[1], diff}}, nil
			}
		case "exists":
			if len(args) == 1 {
				return &CallExpr{Func: "Enum.any?", Args: []Expr{args[0]}}, nil
			}
		case "json":
			if len(args) == 1 {
				enc := &CallExpr{Func: "Jason.encode!", Args: []Expr{args[0]}}
				return &CallExpr{Func: "IO.puts", Args: []Expr{enc}}, nil
			}
		}
		if fn, ok := env.GetFunc(name); ok {
			if len(args) < len(fn.Params) {
				remain := fn.Params[len(args):]
				params := make([]string, len(remain))
				callArgs := append([]Expr{}, args...)
				for i, p := range remain {
					params[i] = p.Name
					callArgs = append(callArgs, &VarRef{Name: p.Name})
				}
				call := &CallExpr{Func: name, Args: callArgs}
				body := []Stmt{&ExprStmt{Expr: call}}
				return &AnonFun{Params: params, Body: body}, nil
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
		if _, err := env.GetVar(name); err == nil {
			return &CallExpr{Func: name, Args: args, Var: true}, nil
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil:
		return compileLiteral(p.Lit)
	case p.Selector != nil:
		if kind, ok := builtinAliases[p.Selector.Root]; ok && len(p.Selector.Tail) == 1 {
			switch kind {
			case "go_testpkg":
				switch p.Selector.Tail[0] {
				case "Pi":
					return &NumberLit{Value: "3.14"}, nil
				case "Answer":
					return &NumberLit{Value: "42"}, nil
				}
			case "python_math":
				switch p.Selector.Tail[0] {
				case "pi":
					return &CallExpr{Func: ":math.pi", Args: nil}, nil
				case "e":
					return &CallExpr{Func: ":math.exp", Args: []Expr{&NumberLit{Value: "1"}}}, nil
				}
			}
		}
		var expr Expr = &VarRef{Name: p.Selector.Root}
		if len(p.Selector.Tail) == 0 {
			if isZeroVariant(p.Selector.Root, env) {
				expr = &AtomLit{Name: ":" + p.Selector.Root}
			} else if _, err := env.GetVar(p.Selector.Root); err != nil {
				// keep VarRef
			}
		}
		for _, t := range p.Selector.Tail {
			expr = &IndexExpr{Target: expr, Index: &AtomLit{Name: t}, UseMapSyntax: true}
		}
		return expr, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, el := range p.List.Elems {
			ex, err := compileExpr(el, env)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k Expr
			if name, ok := isSimpleIdent(it.Key); ok {
				k = &AtomLit{Name: name}
			} else {
				var err error
				k, err = compileExpr(it.Key, env)
				if err != nil {
					return nil, err
				}
			}
			v, err := compileExpr(it.Value, env)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Struct != nil:
		items := make([]MapItem, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := compileExpr(f.Value, env)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: &AtomLit{Name: f.Name}, Value: v}
		}
		return &MapLit{Items: items}, nil
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
		if expr, err := dataExprFromFile(path, format); err == nil {
			return expr, nil
		}
		return &LoadExpr{Path: path, Format: format}, nil
	case p.Query != nil:
		return compileQueryExpr(p.Query, env)
	case p.FunExpr != nil:
		return compileFunExpr(p.FunExpr, env)
	case p.If != nil:
		return compileIfExpr(p.If, env)
	case p.Match != nil:
		return compileMatchExpr(p.Match, env)
	case p.Group != nil:
		ex, err := compileExpr(p.Group, env)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: ex}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &NumberLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		val := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.Contains(val, ".") {
			val = val + ".0"
		}
		return &NumberLit{Value: val}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
}

func inferStaticType(e Expr) types.Type {
	switch v := e.(type) {
	case *NumberLit:
		if strings.ContainsAny(v.Value, ".eE") {
			return types.FloatType{}
		}
		return types.IntType{}
	case *StringLit, *InterpString:
		return types.StringType{}
	case *BoolLit:
		return types.BoolType{}
	case *ListLit:
		if len(v.Elems) == 0 {
			return types.ListType{Elem: types.AnyType{}}
		}
		t := inferStaticType(v.Elems[0])
		for _, el := range v.Elems[1:] {
			if !types.EqualTypes(t, inferStaticType(el)) {
				t = types.AnyType{}
				break
			}
		}
		return types.ListType{Elem: t}
	case *MapLit:
		if len(v.Items) == 0 {
			return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
		}
		kt := inferStaticType(v.Items[0].Key)
		vt := inferStaticType(v.Items[0].Value)
		for _, it := range v.Items[1:] {
			if !types.EqualTypes(kt, inferStaticType(it.Key)) {
				kt = types.AnyType{}
			}
			if !types.EqualTypes(vt, inferStaticType(it.Value)) {
				vt = types.AnyType{}
			}
		}
		return types.MapType{Key: kt, Value: vt}
	case *LoadExpr:
		return types.ListType{Elem: types.MapType{Key: types.StringType{}, Value: types.AnyType{}}}
	case *NilLit:
		return types.AnyType{}
	default:
		return types.AnyType{}
	}
}

func header() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
		ts = ts.In(loc)
	}
	return fmt.Sprintf("# Code generated by Mochi transpiler %s\n", ts.Format("2006-01-02 15:04 -0700"))
}

func nowHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _now() do\n")
	buf.WriteString(pad + "  seeded = Process.get(:_now_seeded, false)\n")
	buf.WriteString(pad + "  seed = Process.get(:_now_seed, 0)\n")
	buf.WriteString(pad + "  if !seeded do\n")
	buf.WriteString(pad + "    case System.get_env(\"MOCHI_NOW_SEED\") do\n")
	buf.WriteString(pad + "      nil -> :ok\n")
	buf.WriteString(pad + "      s ->\n")
	buf.WriteString(pad + "        case Integer.parse(s) do\n")
	buf.WriteString(pad + "          {v, \"\"} ->\n")
	buf.WriteString(pad + "            Process.put(:_now_seed, v)\n")
	buf.WriteString(pad + "            Process.put(:_now_seeded, true)\n")
	buf.WriteString(pad + "            seed = v\n")
	buf.WriteString(pad + "            seeded = true\n")
	buf.WriteString(pad + "          _ -> :ok\n")
	buf.WriteString(pad + "        end\n")
	buf.WriteString(pad + "    end\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "  if seeded do\n")
	buf.WriteString(pad + "    seed = rem(seed * 1664525 + 1013904223, 2147483647)\n")
	buf.WriteString(pad + "    Process.put(:_now_seed, seed)\n")
	buf.WriteString(pad + "    seed\n")
	buf.WriteString(pad + "  else\n")
	buf.WriteString(pad + "    System.os_time(:nanosecond)\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func elemTypeOfExpr(e *parser.Expr, env *types.Env) types.Type {
	switch t := types.TypeOfExpr(e, env).(type) {
	case types.ListType:
		return t.Elem
	case types.GroupType:
		return t.Elem
	case types.MapType:
		return t.Value
	default:
		return types.AnyType{}
	}
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

func isSimpleCall(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return nil, false
	}
	if p.Target.Call != nil {
		return p.Target.Call, true
	}
	return nil, false
}

func isZeroVariant(name string, env *types.Env) bool {
	st, ok := env.GetStruct(name)
	if !ok || len(st.Fields) > 0 {
		return false
	}
	if u, ok := env.FindUnionByVariant(name); ok {
		_, ok = u.Variants[name]
		return ok
	}
	return false
}

func gatherVarsExpr(e *parser.Expr, set map[string]struct{}) {
	if e == nil || e.Binary == nil {
		return
	}
	gatherVarsUnary(e.Binary.Left, set)
	for _, op := range e.Binary.Right {
		gatherVarsPostfix(op.Right, set)
	}
}

func gatherVarsUnary(u *parser.Unary, set map[string]struct{}) {
	if u == nil {
		return
	}
	gatherVarsPostfix(u.Value, set)
}

func gatherVarsPostfix(pf *parser.PostfixExpr, set map[string]struct{}) {
	if pf == nil {
		return
	}
	gatherVarsPrimary(pf.Target, set)
	for _, op := range pf.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				gatherVarsExpr(a, set)
			}
		} else if op.Index != nil {
			gatherVarsExpr(op.Index.Start, set)
			gatherVarsExpr(op.Index.End, set)
			gatherVarsExpr(op.Index.Step, set)
		}
	}
}

func gatherVarsPrimary(p *parser.Primary, set map[string]struct{}) {
	if p == nil {
		return
	}
	switch {
	case p.Selector != nil:
		set[p.Selector.Root] = struct{}{}
	case p.Call != nil:
		for _, a := range p.Call.Args {
			gatherVarsExpr(a, set)
		}
	case p.List != nil:
		for _, el := range p.List.Elems {
			gatherVarsExpr(el, set)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			gatherVarsExpr(it.Key, set)
			gatherVarsExpr(it.Value, set)
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			gatherVarsExpr(f.Value, set)
		}
	case p.Group != nil:
		gatherVarsExpr(p.Group, set)
	}
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

func extractLoadExpr(e *parser.Expr) *parser.LoadExpr {
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
	return p.Target.Load
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

func valueToExpr(v interface{}) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		keys := make([]string, 0, len(val))
		for k := range val {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		items := make([]MapItem, len(keys))
		for i, k := range keys {
			items[i] = MapItem{Key: &AtomLit{Name: k}, Value: valueToExpr(val[k])}
		}
		return &MapLit{Items: items}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case float64:
		if math.Trunc(val) == val {
			return &NumberLit{Value: fmt.Sprintf("%d", int64(val))}
		}
		s := strconv.FormatFloat(val, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") && !strings.Contains(s, ".") {
			s += ".0"
		}
		return &NumberLit{Value: s}
	case nil:
		return &NilLit{}
	default:
		return &NilLit{}
	}
}

func dataExprFromFile(path, format string) (Expr, error) {
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
		var arr []interface{}
		for _, line := range bytes.Split(data, []byte{'\n'}) {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var item interface{}
			if err := json.Unmarshal(line, &item); err == nil {
				arr = append(arr, item)
			}
		}
		v = arr
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v), nil
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return "", false
	}
	if len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func substituteFieldRefs(e Expr, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *VarRef:
		if fields[ex.Name] {
			return &IndexExpr{Target: &VarRef{Name: "item"}, Index: &AtomLit{Name: ex.Name}, UseMapSyntax: true}
		}
		return ex
	case *BinaryExpr:
		return &BinaryExpr{Left: substituteFieldRefs(ex.Left, fields), Op: ex.Op, Right: substituteFieldRefs(ex.Right, fields), MapIn: ex.MapIn}
	case *UnaryExpr:
		return &UnaryExpr{Op: ex.Op, Expr: substituteFieldRefs(ex.Expr, fields)}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i, a := range ex.Args {
			args[i] = substituteFieldRefs(a, fields)
		}
		return &CallExpr{Func: ex.Func, Args: args, Var: ex.Var}
	case *CondExpr:
		return &CondExpr{Cond: substituteFieldRefs(ex.Cond, fields), Then: substituteFieldRefs(ex.Then, fields), Else: substituteFieldRefs(ex.Else, fields)}
	case *GroupExpr:
		return &GroupExpr{Expr: substituteFieldRefs(ex.Expr, fields)}
	case *IndexExpr:
		return &IndexExpr{Target: substituteFieldRefs(ex.Target, fields), Index: substituteFieldRefs(ex.Index, fields), IsString: ex.IsString, UseMapSyntax: ex.UseMapSyntax}
	case *ListLit:
		elems := make([]Expr, len(ex.Elems))
		for i, el := range ex.Elems {
			elems[i] = substituteFieldRefs(el, fields)
		}
		return &ListLit{Elems: elems}
	case *MapLit:
		items := make([]MapItem, len(ex.Items))
		for i, it := range ex.Items {
			items[i] = MapItem{Key: substituteFieldRefs(it.Key, fields), Value: substituteFieldRefs(it.Value, fields)}
		}
		return &MapLit{Items: items}
	default:
		return ex
	}
}
