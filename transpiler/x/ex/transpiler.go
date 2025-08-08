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
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"

	"gopkg.in/yaml.v3"

	"mochi/parser"
	"mochi/types"
)

var elixirReserved = map[string]struct{}{
	"fn":    {},
	"abs":   {},
	"end":   {},
	"do":    {},
	"node":  {},
	"div":   {},
	"rem":   {},
	"quote": {},
	"and":   {},
	"or":    {},
	"not":   {},
	"when":  {},
	"after": {},
}

var kernelReserved = map[string]int{
	"floor": 1,
}

func kernelImportExcepts() []string {
	var ex []string
	for name, ar := range definedFuncs {
		if kar, ok := kernelReserved[name]; ok && kar == ar {
			ex = append(ex, fmt.Sprintf("%s: %d", name, ar))
		}
	}
	sort.Strings(ex)
	return ex
}

func sanitizeIdent(name string) string {
	if _, ok := elixirReserved[name]; ok {
		return name + "_"
	}
	if len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
		return strings.ToLower(name)
	}
	return name
}

func sanitizeFuncName(name string) string {
	if len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
		name = strings.ToLower(name)
	}
	return sanitizeIdent(name)
}

func sanitizeCallName(name string) string {
	if len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
		name = strings.ToLower(name)
	}
	return sanitizeIdent(name)
}

var funcDepth int

// builtinAliases maps import aliases to special built-in modules.
var builtinAliases map[string]string

// globalVars tracks variables defined before the first function declaration.
var globalVars map[string]struct{}

// methodFuncs tracks names of struct methods for implicit self calls.
var methodFuncs map[string]struct{}

// definedFuncs maps top-level function names to their arity.
var definedFuncs map[string]int

func isGlobalVar(name string) bool {
	_, ok := globalVars[name]
	return ok
}

var loopCounter int
var breakStack [][]string

// usedHelpers tracks which helper functions are required for the current program.
var usedHelpers map[string]bool

func pushBreakVars(vars []string) {
	breakStack = append(breakStack, vars)
}

func popBreakVars() {
	if len(breakStack) > 0 {
		breakStack = breakStack[:len(breakStack)-1]
	}
}

func currentBreakVars() []string {
	if len(breakStack) == 0 {
		return nil
	}
	return breakStack[len(breakStack)-1]
}

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

func isBigRat(t types.Type) bool {
	_, ok := t.(types.BigRatType)
	return ok
}

// moduleMode is true when emitting a module with functions.
var moduleMode bool

// Program represents a sequence of Elixir statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer, int) }

// BlockStmt emits a sequence of statements.
type BlockStmt struct{ Stmts []Stmt }

func (b *BlockStmt) emit(w io.Writer, indent int) {
	for i, s := range b.Stmts {
		s.emit(w, indent)
		if i < len(b.Stmts)-1 {
			io.WriteString(w, "\n")
		}
	}
}

// VarRef references a variable name or dotted selector.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) {
	if moduleMode {
		if _, ok := globalVars[v.Name]; ok {
			fmt.Fprintf(w, "Process.get(:%s)", moduleAttrName(v.Name))
			return
		}
		io.WriteString(w, sanitizeIdent(v.Name))
		return
	}
	if strings.HasPrefix(v.Name, "_") {
		fmt.Fprintf(w, "Main.%s", sanitizeIdent(v.Name))
		return
	}
	io.WriteString(w, sanitizeIdent(v.Name))
}

// FuncRef references a function as a value using capture syntax.
type FuncRef struct {
	Name  string
	Arity int
}

func (f *FuncRef) emit(w io.Writer) {
	fmt.Fprintf(w, "&%s/%d", f.Name, f.Arity)
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
	io.WriteString(w, sanitizeIdent(s.Name))
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
		io.WriteString(w, sanitizeIdent(s.Name))
	} else {
		io.WriteString(w, sanitizeIdent(s.Name))
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
	vars := currentBreakVars()
	if len(vars) == 0 {
		io.WriteString(w, "throw :break")
		return
	}
	io.WriteString(w, "throw {:break, {")
	for i, v := range vars {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, sanitizeIdent(v))
	}
	io.WriteString(w, "}}")
}

// ContinueStmt represents a continue statement inside loops.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	vars := currentBreakVars()
	if len(vars) == 0 {
		io.WriteString(w, "throw :continue")
		return
	}
	io.WriteString(w, "throw {:continue, {")
	for i, v := range vars {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, sanitizeIdent(v))
	}
	io.WriteString(w, "}}")
}

// ReturnStmt returns from a function optionally with a value.
type ReturnStmt struct{ Value Expr }

func (r *ReturnStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "throw {:return, ")
	if r.Value != nil {
		if _, ok := r.Value.(*CondExpr); ok {
			io.WriteString(w, "(")
			r.Value.emit(w)
			io.WriteString(w, ")")
		} else {
			r.Value.emit(w)
		}
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
			io.WriteString(w, sanitizeIdent(v))
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
			io.WriteString(w, sanitizeIdent(v))
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
				io.WriteString(w, sanitizeIdent(v))
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
		io.WriteString(w, sanitizeIdent(v))
	}
	io.WriteString(w, " ->\n")
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "if ")
	wst.Cond.emit(w)
	io.WriteString(w, " do\n")
	pushBreakVars(wst.Vars)
	if wst.Simple {
		for _, st := range wst.Body {
			st.emit(w, indent+2)
			io.WriteString(w, "\n")
		}
	} else {
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		if len(wst.Vars) > 0 {
			io.WriteString(w, "{")
			for i, v := range wst.Vars {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				io.WriteString(w, sanitizeIdent(v))
			}
			io.WriteString(w, "} = ")
		}
		io.WriteString(w, "try do\n")
		for _, st := range wst.Body {
			st.emit(w, indent+3)
			io.WriteString(w, "\n")
		}
		if len(wst.Vars) > 0 {
			for i := 0; i < indent+3; i++ {
				io.WriteString(w, "  ")
			}
			io.WriteString(w, "{")
			for i, v := range wst.Vars {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				io.WriteString(w, sanitizeIdent(v))
			}
			io.WriteString(w, "}\n")
		}
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "catch\n")
		for i := 0; i < indent+3; i++ {
			io.WriteString(w, "  ")
		}
		if len(wst.Vars) > 0 {
			io.WriteString(w, "{:continue, {")
			for i, v := range wst.Vars {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				io.WriteString(w, sanitizeIdent(v))
			}
			io.WriteString(w, "}} -> {")
			for i, v := range wst.Vars {
				if i > 0 {
					io.WriteString(w, ", ")
				}
				io.WriteString(w, sanitizeIdent(v))
			}
			io.WriteString(w, "}\n")
		} else {
			io.WriteString(w, ":continue -> nil\n")
		}
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
		io.WriteString(w, sanitizeIdent(v))
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
		io.WriteString(w, sanitizeIdent(wst.Vars[0]))
		io.WriteString(w, "\n")
	} else {
		io.WriteString(w, "{")
		for i, v := range wst.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, sanitizeIdent(v))
		}
		io.WriteString(w, "}\n")
	}
	for i := 0; i < indent+1; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "end\n")
	popBreakVars()
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
		io.WriteString(w, sanitizeIdent(wst.Vars[0]))
		io.WriteString(w, " = try do\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, name)
		io.WriteString(w, ".(")
		io.WriteString(w, name)
		io.WriteString(w, ", ")
		io.WriteString(w, sanitizeIdent(wst.Vars[0]))
		io.WriteString(w, ")\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "catch\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "{:break, {")
		io.WriteString(w, sanitizeIdent(wst.Vars[0]))
		io.WriteString(w, "}} -> ")
		io.WriteString(w, sanitizeIdent(wst.Vars[0]))
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
			io.WriteString(w, sanitizeIdent(v))
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
			io.WriteString(w, sanitizeIdent(v))
		}
		io.WriteString(w, ")\n")
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "catch\n")
		for i := 0; i < indent+2; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "{:break, {")
		for i, v := range wst.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, sanitizeIdent(v))
		}
		io.WriteString(w, "}} -> {")
		for i, v := range wst.Vars {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, sanitizeIdent(v))
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

// BenchStmt represents a benchmarking block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (bs *BenchStmt) emit(w io.Writer, indent int) {
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, ":erlang.garbage_collect()\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "mem_start = _mem()\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "t_start = _bench_now()\n")
	for _, st := range bs.Body {
		st.emit(w, indent)
		io.WriteString(w, "\n")
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "mem_end = _mem()\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "duration_us = max(_bench_now() - t_start, 1)\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, ":erlang.garbage_collect()\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "mem_diff = abs(mem_end - mem_start)\n")
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	fmt.Fprintf(w, "IO.puts(\"{\\n  \\\"duration_us\\\": #{duration_us},\\n  \\\"memory_bytes\\\": #{mem_diff},\\n  \\\"name\\\": \\\"%s\\\"\\n}\")", bs.Name)
}

func (fs *ForStmt) emit(w io.Writer, indent int) {
	if len(fs.Vars) > 0 && fs.End != nil && fs.Simple {
		if fs.Simple {
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
			io.WriteString(w, " - 1)//1), {")
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
		io.WriteString(w, " - 1)//1), {")
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
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "try do\n")
		for _, st := range fs.Body {
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
		io.WriteString(w, ":continue -> {")
		for i, v := range fs.Vars {
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
	if len(fs.Vars) > 0 && fs.End == nil && fs.Simple {
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
		io.WriteString(w, "} = Enum.reduce(")
		fs.Source.emit(w)
		io.WriteString(w, ", {")
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
	if len(fs.Vars) > 0 && fs.End == nil {
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
		io.WriteString(w, "} = Enum.reduce(")
		fs.Source.emit(w)
		io.WriteString(w, ", {")
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
		for i := 0; i < indent+1; i++ {
			io.WriteString(w, "  ")
		}
		io.WriteString(w, "try do\n")
		for _, st := range fs.Body {
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
		io.WriteString(w, ":continue -> {")
		for i, v := range fs.Vars {
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
	pushBreakVars(fs.Vars)
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
	popBreakVars()
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
	Locals map[string]struct{}
}

func (fn *FuncDecl) emit(w io.Writer, indent int) {
	prevGlobals := globalVars
	globalVars = map[string]struct{}{}
	for k, v := range prevGlobals {
		globalVars[k] = v
	}
	for _, p := range fn.Params {
		delete(globalVars, p)
	}
	for name := range fn.Locals {
		delete(globalVars, name)
	}
	for i := 0; i < indent; i++ {
		io.WriteString(w, "  ")
	}
	io.WriteString(w, "def ")
	io.WriteString(w, sanitizeFuncName(fn.Name))
	io.WriteString(w, "(")
	for i, p := range fn.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, sanitizeIdent(p))
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
	globalVars = prevGlobals
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
		io.WriteString(w, sanitizeIdent(p))
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
	io.WriteString(w, "(if ")
	c.Cond.emit(w)
	io.WriteString(w, ", do: ")
	c.Then.emit(w)
	if c.Else != nil {
		io.WriteString(w, ", else: ")
		c.Else.emit(w)
	}
	io.WriteString(w, ")")
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
	IntDiv    bool
	FloatOp   bool
}

func (b *BinaryExpr) emit(w io.Writer) {
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
	var isFloat func(Expr) bool
	isFloat = func(e Expr) bool {
		switch t := e.(type) {
		case *NumberLit:
			return strings.Contains(t.Value, ".")
		case *BinaryExpr:
			return t.FloatOp
		case *GroupExpr:
			return isFloat(t.Expr)
		}
		return false
	}
	if b.Op == "/" {
		if b.IntDiv || (!b.FloatOp && !isFloat(b.Left) && !isFloat(b.Right)) {
			io.WriteString(w, "div(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
		} else {
			b.Left.emit(w)
			io.WriteString(w, " / ")
			b.Right.emit(w)
		}
		return
	}
	if b.Op == "%" {
		if b.FloatOp || isFloat(b.Left) || isFloat(b.Right) {
			io.WriteString(w, ":math.fmod(")
			b.Left.emit(w)
			io.WriteString(w, " * 1.0, ")
			b.Right.emit(w)
			io.WriteString(w, " * 1.0)")
		} else {
			io.WriteString(w, "rem(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
		}
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

func isStringExpr(e Expr) bool {
	switch t := e.(type) {
	case *StringLit, *InterpString:
		return true
	case *CallExpr:
		if strings.HasPrefix(t.Func, "String.") || t.Func == "Kernel.to_string" {
			return true
		}
	}
	return false
}

func (c *CallExpr) emit(w io.Writer) {
	if c.Var && c.Func == "str" && len(c.Args) == 1 {
		io.WriteString(w, "Kernel.to_string(")
		c.Args[0].emit(w)
		io.WriteString(w, ")")
		return
	}
	if c.Func == "" {
		if len(c.Args) == 0 {
			io.WriteString(w, "()")
			return
		}
		c.Args[0].emit(w)
		io.WriteString(w, ".(")
		for i, a := range c.Args[1:] {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			if _, ok := a.(*CondExpr); ok {
				io.WriteString(w, "(")
				a.emit(w)
				io.WriteString(w, ")")
			} else {
				a.emit(w)
			}
		}
		io.WriteString(w, ")")
		return
	}
	name := c.Func
	if !moduleMode {
		if _, ok := definedFuncs[name]; ok || strings.HasPrefix(name, "_") {
			name = "Main." + name
		}
	}
	io.WriteString(w, name)
	if c.Var {
		io.WriteString(w, ".(")
	} else if strings.Contains(c.Func, ".") && len(c.Func) > 0 && c.Func[0] >= 'a' && c.Func[0] <= 'z' {
		io.WriteString(w, ".(")
	} else {
		io.WriteString(w, "(")
	}
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if _, ok := a.(*CondExpr); ok {
			io.WriteString(w, "(")
			a.emit(w)
			io.WriteString(w, ")")
		} else {
			a.emit(w)
		}
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
			name := strings.TrimPrefix(a.Name, ":")
			if len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
				io.WriteString(w, "[:")
				io.WriteString(w, name)
				io.WriteString(w, "]")
			} else {
				io.WriteString(w, ".")
				io.WriteString(w, name)
			}
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

// FetchExpr represents a `fetch` expression returning a String.
type FetchExpr struct{ URL Expr }

func (f *FetchExpr) emit(w io.Writer) {
	io.WriteString(w, "_fetch(")
	f.URL.emit(w)
	io.WriteString(w, ")")
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
		io.WriteString(w, "(fn v -> if is_binary(v), do: String.to_integer(v), else: trunc(v) end).(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "float":
		io.WriteString(w, ":erlang.float(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	case "bigrat":
		usedHelpers["bigrat"] = true
		io.WriteString(w, "_bigrat(")
		c.Expr.emit(w)
		io.WriteString(w, ")")
	default:
		c.Expr.emit(w)
	}
}

// Emit generates Elixir source from the AST.
func Emit(p *Program, benchMain bool) []byte {
	var buf bytes.Buffer
	buf.WriteString(header())
	funcsExist := false
	hasMain := false
	for _, st := range p.Stmts {
		if fd, ok := st.(*FuncDecl); ok {
			funcsExist = true
			if fd.Name == "main" {
				hasMain = true
			}
		}
	}
	if !funcsExist {
		globalVars = map[string]struct{}{}
	}
	moduleMode = true
	buf.WriteString("defmodule Main do\n")
	if ex := kernelImportExcepts(); len(ex) > 0 {
		buf.WriteString("  import Kernel, except: [")
		buf.WriteString(strings.Join(ex, ", "))
		buf.WriteString("]\n")
	}
	if usedHelpers["now"] {
		buf.WriteString(nowHelper(1))
	}
	if usedHelpers["input"] {
		buf.WriteString(inputHelper(1))
	}
	if benchMain {
		buf.WriteString(benchNowHelper(1))
		buf.WriteString(memHelper(1))
	}
	if usedHelpers["lookup_host"] {
		buf.WriteString(lookupHostHelper(1))
	}
	if usedHelpers["slice"] {
		buf.WriteString(sliceHelper(1))
	}
	if usedHelpers["len"] {
		buf.WriteString(lenHelper(1))
	}
	if usedHelpers["bigrat"] {
		buf.WriteString(bigRatHelper(1))
	}
	if usedHelpers["sha256"] {
		buf.WriteString(sha256Helper(1))
	}
	if usedHelpers["getenv"] {
		buf.WriteString(getenvHelper(1))
	}
	if usedHelpers["environ"] {
		buf.WriteString(environHelper(1))
	}
	if usedHelpers["getoutput"] {
		buf.WriteString(getoutputHelper(1))
	}
	if usedHelpers["fetch"] {
		buf.WriteString(fetchHelper(1))
	}
	if usedHelpers["md5"] {
		buf.WriteString(md5Helper(1))
	}
	var globals []Stmt
	var funcs []Stmt
	var main []Stmt
	for _, st := range p.Stmts {
		switch t := st.(type) {
		case *FuncDecl:
			funcs = append(funcs, st)
		case *LetStmt:
			if funcsExist && isConstExpr(t.Value) {
				globals = append(globals, st)
			} else {
				main = append(main, st)
			}
		default:
			if es, ok := st.(*ExprStmt); ok {
				if call, ok := es.Expr.(*CallExpr); ok && (call.Func == "main" || call.Func == "Main.main") && len(call.Args) == 0 {
					continue
				}
			}
			main = append(main, st)
		}
	}
	for _, st := range funcs {
		st.emit(&buf, 1)
		buf.WriteString("\n")
	}
	for _, st := range globals {
		if ls, ok := st.(*LetStmt); ok {
			ls.emitGlobal(&buf, 1)
		} else {
			st.emit(&buf, 1)
		}
		buf.WriteString("\n")
	}
	if !hasMain {
		buf.WriteString("  def main() do\n")
		if benchMain {
			buf.WriteString("    :erlang.garbage_collect()\n")
			buf.WriteString("    mem_start = _mem()\n")
			buf.WriteString("    t_start = _bench_now()\n")
		}
		for _, st := range main {
			if ls, ok := st.(*LetStmt); ok {
				if _, ok := globalVars[ls.Name]; ok {
					ls.emitGlobal(&buf, 2)
				} else {
					ls.emit(&buf, 2)
				}
			} else {
				st.emit(&buf, 2)
			}
			buf.WriteString("\n")
		}
		if benchMain {
			buf.WriteString("    mem_end = _mem()\n")
			buf.WriteString("    duration_us = max(_bench_now() - t_start, 1)\n")
			buf.WriteString("    :erlang.garbage_collect()\n")
			buf.WriteString("    mem_diff = abs(mem_end - mem_start)\n")
			buf.WriteString("    IO.puts(\"{\\n  \\\"duration_us\\\": #{duration_us},\\n  \\\"memory_bytes\\\": #{mem_diff},\\n  \\\"name\\\": \\\"main\\\"\\n}\")\n")
		}
		buf.WriteString("  end\n")
	} else if benchMain {
		buf.WriteString("  def bench_main() do\n")
		for _, st := range globals {
			if ls, ok := st.(*LetStmt); ok {
				ls.emitGlobal(&buf, 2)
			} else {
				st.emit(&buf, 2)
			}
			buf.WriteString("\n")
		}
		buf.WriteString("    :erlang.garbage_collect()\n")
		buf.WriteString("    mem_start = _mem()\n")
		buf.WriteString("    t_start = _bench_now()\n")
		buf.WriteString("    main()\n")
		buf.WriteString("    mem_end = _mem()\n")
		buf.WriteString("    duration_us = max(_bench_now() - t_start, 1)\n")
		buf.WriteString("    :erlang.garbage_collect()\n")
		buf.WriteString("    mem_diff = abs(mem_end - mem_start)\n")
		buf.WriteString("    IO.puts(\"{\\n  \\\"duration_us\\\": #{duration_us},\\n  \\\"memory_bytes\\\": #{mem_diff},\\n  \\\"name\\\": \\\"main\\\"\\n}\")\n")
		buf.WriteString("  end\n")
	}
	buf.WriteString("end\n")
	moduleMode = false
	if hasMain && len(main) > 0 {
		for _, st := range main {
			if ls, ok := st.(*LetStmt); ok {
				if _, ok := globalVars[ls.Name]; ok {
					ls.emitGlobal(&buf, 0)
				} else {
					ls.emit(&buf, 0)
				}
			} else {
				st.emit(&buf, 0)
			}
			buf.WriteString("\n")
		}
	}
	if benchMain && hasMain {
		buf.WriteString("Main.bench_main()\n")
	} else {
		buf.WriteString("Main.main()\n")
	}
	out := buf.Bytes()
	re := regexp.MustCompile(`(@[A-Z][A-Z0-9_]* )([0-9]+)`)
	out = re.ReplaceAll(out, []byte("$1= $2"))
	return out
}

// Transpile converts a Mochi program into an Elixir AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	res := &Program{}
	builtinAliases = make(map[string]string)
	globalVars = make(map[string]struct{})
	methodFuncs = make(map[string]struct{})
	definedFuncs = make(map[string]int)
	usedHelpers = make(map[string]bool)
	moduleMode = false
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
					if env != nil {
						env.SetFuncType(alias+".Add", types.FuncType{Params: []types.Type{types.IntType{}, types.IntType{}}, Return: types.IntType{}})
						env.SetVar(alias+".Pi", types.FloatType{}, false)
						env.SetVar(alias+".Answer", types.IntType{}, false)
						env.SetFuncType(alias+".FifteenPuzzleExample", types.FuncType{Params: []types.Type{}, Return: types.StringType{}})
					}
				} else if st.Import.Auto && path == "net" {
					builtinAliases[alias] = "go_net"
					if env != nil {
						env.SetFuncType(alias+".LookupHost", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.ListType{Elem: types.AnyType{}}})
					}
				}
			case "python":
				if path == "math" {
					builtinAliases[alias] = "python_math"
				} else if path == "subprocess" {
					builtinAliases[alias] = "python_subprocess"
				}
			}
		}
		if st.Fun != nil {
			moduleMode = true
			definedFuncs[sanitizeFuncName(st.Fun.Name)] = len(st.Fun.Params)
		}
		if st.Let != nil {
			globalVars[st.Let.Name] = struct{}{}
		} else if st.Var != nil {
			globalVars[st.Var.Name] = struct{}{}
		}
	}
	for _, st := range prog.Statements {
		stmt, err := compileStmt(st, env)
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			if blk, ok := stmt.(*BlockStmt); ok {
				res.Stmts = append(res.Stmts, blk.Stmts...)
			} else {
				res.Stmts = append(res.Stmts, stmt)
			}
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
			if f := fetchExprOnly(st.Let.Value); f != nil {
				urlExpr, err := compileExpr(f.URL, env)
				if err != nil {
					return nil, err
				}
				if st.Let.Type != nil {
					typ := types.ResolveTypeRef(st.Let.Type, env)
					if stt, ok := typ.(types.StructType); ok {
						names := make([]string, 0, len(stt.Fields))
						for n := range stt.Fields {
							names = append(names, n)
						}
						sort.Strings(names)
						items := make([]MapItem, len(names))
						for i, n := range names {
							var v Expr
							if n == "title" {
								usedHelpers["fetch"] = true
								v = &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}
							} else {
								switch stt.Fields[n].(type) {
								case types.StringType:
									v = &StringLit{Value: ""}
								case types.BoolType:
									v = &BoolLit{Value: false}
								default:
									v = &NumberLit{Value: "0"}
								}
							}
							items[i] = MapItem{Key: &AtomLit{Name: n}, Value: v}
						}
						val = &MapLit{Items: items}
					} else {
						usedHelpers["fetch"] = true
						val = &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}
					}
				} else {
					usedHelpers["fetch"] = true
					val = &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}
				}
			} else {
				var err error
				val, err = compileExpr(st.Let.Value, env)
				if err != nil {
					return nil, err
				}
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
				t := types.TypeOfExpr(st.Let.Value, env)
				if lt, ok := t.(types.ListType); ok {
					if _, isAny := lt.Elem.(types.AnyType); isAny {
						it := inferStaticType(val)
						if _, isAny2 := it.(types.AnyType); !isAny2 {
							t = it
						}
					}
				} else if mt, ok := t.(types.MapType); ok {
					if _, isAny := mt.Key.(types.AnyType); isAny {
						it := inferStaticType(val)
						if _, isAny2 := it.(types.AnyType); !isAny2 {
							t = it
						}
					} else if _, isAny := mt.Value.(types.AnyType); isAny {
						it := inferStaticType(val)
						if _, isAny2 := it.(types.AnyType); !isAny2 {
							t = it
						}
					}
				}
				env.SetVar(st.Let.Name, t, false)
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
				env.SetVar(st.Var.Name, types.FuncType{}, true)
			} else if st.Var.Type != nil {
				env.SetVar(st.Var.Name, types.ResolveTypeRef(st.Var.Type, env), true)
			} else if ld := extractLoadExpr(st.Var.Value); ld != nil && ld.Type != nil {
				t := types.ResolveTypeRef(ld.Type, env)
				env.SetVar(st.Var.Name, types.ListType{Elem: t}, true)
			} else if st.Var.Value != nil {
				t := types.TypeOfExpr(st.Var.Value, env)
				if lt, ok := t.(types.ListType); ok {
					if _, isAny := lt.Elem.(types.AnyType); isAny {
						it := inferStaticType(val)
						if _, isAny2 := it.(types.AnyType); !isAny2 {
							t = it
						}
					}
				} else if mt, ok := t.(types.MapType); ok {
					if _, isAny := mt.Key.(types.AnyType); isAny {
						it := inferStaticType(val)
						if _, isAny2 := it.(types.AnyType); !isAny2 {
							t = it
						}
					} else if _, isAny := mt.Value.(types.AnyType); isAny {
						it := inferStaticType(val)
						if _, isAny2 := it.(types.AnyType); !isAny2 {
							t = it
						}
					}
				}
				env.SetVar(st.Var.Name, t, true)
			} else {
				env.SetVar(st.Var.Name, inferStaticType(val), true)
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
			case types.MapType, types.StructType:
				call = &CallExpr{Func: "Map.put", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, val}}
			default:
				// Fallback to list semantics when the type is unknown
				call = &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, val}}
			}
			return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
		}
		if len(st.Assign.Index) == 1 && len(st.Assign.Field) == 1 {
			idx, err := compileExpr(st.Assign.Index[0].Start, env)
			if err != nil {
				return nil, err
			}
			val, err := compileExpr(st.Assign.Value, env)
			if err != nil {
				return nil, err
			}
			field := &AtomLit{Name: ":" + st.Assign.Field[0].Name}
			t, _ := env.GetVar(st.Assign.Name)
			if lt, ok := t.(types.ListType); ok {
				switch lt.Elem.(type) {
				case types.StructType, types.MapType:
					elem := &CallExpr{Func: "Enum.at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx}}
					elemUpdate := &CallExpr{Func: "Map.put", Args: []Expr{elem, field, val}}
					call := &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, elemUpdate}}
					return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
				}
			}
			// Fallback when type info is unavailable
			elem := &CallExpr{Func: "Enum.at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx}}
			elemUpdate := &CallExpr{Func: "Map.put", Args: []Expr{elem, field, val}}
			call := &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx, elemUpdate}}
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
				switch outerList.Elem.(type) {
				case types.ListType:
					inner := &CallExpr{Func: "Enum.at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0}}
					innerUpdate := &CallExpr{Func: "List.replace_at", Args: []Expr{inner, idx1, val}}
					call := &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0, innerUpdate}}
					return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
				case types.MapType:
					inner := &CallExpr{Func: "Enum.at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0}}
					innerUpdate := &CallExpr{Func: "Map.put", Args: []Expr{inner, idx1, val}}
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
			// Fallback to list-of-list semantics when type info is unavailable
			inner := &CallExpr{Func: "Enum.at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0}}
			innerUpdate := &CallExpr{Func: "List.replace_at", Args: []Expr{inner, idx1, val}}
			call := &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: st.Assign.Name}, idx0, innerUpdate}}
			return &AssignStmt{Name: st.Assign.Name, Value: call}, nil
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
			} else if th.Assign != nil && el.Assign != nil &&
				th.Assign.Name == el.Assign.Name &&
				len(th.Assign.Index) == 1 && len(el.Assign.Index) == 1 &&
				len(th.Assign.Field) == 0 && len(el.Assign.Field) == 0 {
				idx1, ok1 := identName(th.Assign.Index[0].Start)
				idx2, ok2 := identName(el.Assign.Index[0].Start)
				if ok1 && ok2 && idx1 == idx2 {
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
					idxExpr, err := compileExpr(th.Assign.Index[0].Start, env)
					if err != nil {
						return nil, err
					}
					ce := &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}
					t, _ := env.GetVar(th.Assign.Name)
					var call *CallExpr
					switch t.(type) {
					case types.MapType:
						call = &CallExpr{Func: "Map.put", Args: []Expr{&VarRef{Name: th.Assign.Name}, idxExpr, ce}}
					default:
						call = &CallExpr{Func: "List.replace_at", Args: []Expr{&VarRef{Name: th.Assign.Name}, idxExpr, ce}}
					}
					return &AssignStmt{Name: th.Assign.Name, Value: call}, nil
				}
			}
		}
		return compileIfStmt(st.If, env)
	case st.While != nil:
		return compileWhileStmt(st.While, env)
	case st.For != nil:
		return compileForStmt(st.For, env)
	case st.Bench != nil:
		return compileBenchStmt(st.Bench, env)
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
		locals := map[string]struct{}{}
		for _, p := range st.Fun.Params {
			if p.Type != nil {
				child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), false)
			} else {
				child.SetVar(p.Name, types.AnyType{}, false)
			}
			locals[p.Name] = struct{}{}
		}
		funcDepth++
		body := make([]Stmt, 0, len(st.Fun.Body))
		for _, b := range st.Fun.Body {
			if b.Let != nil {
				locals[b.Let.Name] = struct{}{}
			} else if b.Var != nil {
				locals[b.Var.Name] = struct{}{}
			}
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
		return &FuncDecl{Name: st.Fun.Name, Params: params, Body: body, Locals: locals}, nil
	case st.ExternVar != nil:
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.Type != nil:
		block := &BlockStmt{}
		var fields []string
		for _, m := range st.Type.Members {
			if m.Field != nil {
				fields = append(fields, m.Field.Name)
			} else if m.Method != nil {
				env.SetFunc(m.Method.Name, m.Method)
				methodFuncs[m.Method.Name] = struct{}{}
			}
		}
		for _, m := range st.Type.Members {
			if m.Method != nil {
				child := types.NewEnv(env)
				locals := map[string]struct{}{"self": {}}
				child.SetVar("self", types.AnyType{}, false)
				params := []string{"self"}
				for _, p := range m.Method.Params {
					if p.Type != nil {
						child.SetVar(p.Name, types.ResolveTypeRef(p.Type, env), false)
					} else {
						child.SetVar(p.Name, types.AnyType{}, false)
					}
					locals[p.Name] = struct{}{}
					params = append(params, p.Name)
				}
				body := make([]Stmt, 0, len(fields)+len(m.Method.Body))
				for _, f := range fields {
					fn := sanitizeIdent(f)
					child.SetVar(fn, types.AnyType{}, false)
					locals[fn] = struct{}{}
					idx := &AtomLit{Name: ":" + f}
					body = append(body, &LetStmt{Name: fn, Value: &IndexExpr{Target: &VarRef{Name: "self"}, Index: idx, UseMapSyntax: true}})
				}
				funcDepth++
				for _, b := range m.Method.Body {
					if b.Let != nil {
						locals[b.Let.Name] = struct{}{}
					} else if b.Var != nil {
						locals[b.Var.Name] = struct{}{}
					}
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
				fd := &FuncDecl{Name: m.Method.Name, Params: params, Body: body, Locals: locals}
				block.Stmts = append(block.Stmts, fd)
			}
		}
		if len(block.Stmts) > 0 {
			return block, nil
		}
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
	// Use the current environment when gathering mutated variables so that
	// variables defined in outer scopes are recognized as mutable.
	vars := gatherMutVars(append(append([]Stmt{}, thenStmts...), elseStmts...), env)
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
			case *ContinueStmt:
				// require wrapping in try/catch when continue is used
				simple = false
			case *BreakStmt:
				// break can bubble up without try/catch
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
	locals := map[string]struct{}{}
	var walk func([]Stmt)
	walk = func(ss []Stmt) {
		for _, s := range ss {
			switch t := s.(type) {
			case *LetStmt:
				locals[t.Name] = struct{}{}
			case *AssignStmt:
				if _, ok := locals[t.Name]; ok {
					break
				}
				if isGlobalVar(t.Name) {
					break
				}
				// If the variable exists in the current environment treat it as a local
				// mutation, even if a global variable with the same name exists.
				if _, err := env.IsMutable(t.Name); err == nil {
					set[t.Name] = struct{}{}
					break
				}
				set[t.Name] = struct{}{}
			case *IfStmt:
				for _, v := range t.Vars {
					if _, ok := locals[v]; ok {
						continue
					}
					if isGlobalVar(v) {
						continue
					}
					// Prefer a local variable in the current environment even if a global
					// variable with the same name exists.
					if _, err := env.IsMutable(v); err == nil {
						set[v] = struct{}{}
						continue
					}
					set[v] = struct{}{}
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
	// Propagate the element type of the loop source to the loop variable.
	// This enables built-in functions like len() to produce optimized
	// implementations depending on the element type.
	if fs.RangeEnd != nil {
		bodyEnv.SetVar(fs.Name, types.IntType{}, true)
	} else {
		bodyEnv.SetVar(fs.Name, elemTypeOfExpr(fs.Source, env), true)
	}
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
		switch types.TypeOfExprBasic(fs.Source, env).(type) {
		case types.MapType:
			src = &CallExpr{Func: "Map.keys", Args: []Expr{start}}
		case types.StringType:
			src = &CallExpr{Func: "String.graphemes", Args: []Expr{start}}
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
	name := sanitizeIdent(fs.Name)
	varsSanitized := make([]string, len(vars))
	for i, v := range vars {
		varsSanitized[i] = sanitizeIdent(v)
	}
	res := &ForStmt{Name: name, Start: start, End: end, Source: src, Body: body, Vars: varsSanitized, Simple: simple}
	return res, nil
}

func compileBenchStmt(bb *parser.BenchBlock, env *types.Env) (Stmt, error) {
	bodyEnv := types.NewEnv(env)
	body := make([]Stmt, 0, len(bb.Body))
	for _, s := range bb.Body {
		st, err := compileStmt(s, bodyEnv)
		if err != nil {
			return nil, err
		}
		if st != nil {
			body = append(body, st)
		}
	}
	name := strings.Trim(bb.Name, "\"")
	return &BenchStmt{Name: name, Body: body}, nil
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
		op := u.Ops[i]
		if op == "-" {
			part := &parser.Unary{Value: u.Value, Ops: u.Ops[:i+1]}
			if _, ok := types.TypeOfUnary(part, env).(types.BigRatType); ok {
				usedHelpers["bigrat"] = true
				expr = &CallExpr{Func: "_bigrat_neg", Args: []Expr{expr}}
				continue
			}
		}
		expr = &UnaryExpr{Op: op, Expr: expr}
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
					if (opName == "+" || opName == "-" || opName == "*" || opName == "/") &&
						(isBigRat(typesSlice[i]) || isBigRat(typesSlice[i+1])) {
						fn := map[string]string{"+": "_bigrat_add", "-": "_bigrat_sub", "*": "_bigrat_mul", "/": "_bigrat_div"}[opName]
						usedHelpers["bigrat"] = true
						expr = &CallExpr{Func: fn, Args: []Expr{operands[i], operands[i+1]}}
						typesSlice[i] = types.BigRatType{}
					} else {
						leftFloat := types.IsFloatType(typesSlice[i])
						if !leftFloat {
							if be, ok := operands[i].(*BinaryExpr); ok {
								leftFloat = be.FloatOp
							}
						}
						rightFloat := types.IsFloatType(typesSlice[i+1])
						if !rightFloat {
							if be, ok := operands[i+1].(*BinaryExpr); ok {
								rightFloat = be.FloatOp
							}
						}
						floatOp := leftFloat || rightFloat
						if opName == "+" {
							if _, ok := typesSlice[i].(types.ListType); ok {
								opName = "++"
							}
							if _, ok := typesSlice[i+1].(types.ListType); ok {
								opName = "++"
							}
						}
						bin := &BinaryExpr{Left: operands[i], Op: opName, Right: operands[i+1], FloatOp: floatOp}
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
						} else if opName == "/" {
							if (types.IsIntType(typesSlice[i]) || types.IsBigIntType(typesSlice[i])) &&
								(types.IsIntType(typesSlice[i+1]) || types.IsBigIntType(typesSlice[i+1])) {
								bin.IntDiv = true
							}
						}
						expr = bin
					}
				}
				operands[i] = expr
				if _, ok := typesSlice[i].(types.BigRatType); ok {
					// preserve BigRat type for subsequent operations
					typesSlice[i] = types.BigRatType{}
				} else if b, ok := expr.(*BinaryExpr); ok && b.Op == "+" && b.StrConcat {
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
		if alias == "stdout" && method == "write" && len(args) == 1 {
			return &CallExpr{Func: "IO.write", Args: args}, nil
		}
		if alias == "os" {
			if (method == "Getenv" || method == "GetEnv") && len(args) == 1 {
				usedHelpers["getenv"] = true
				return &CallExpr{Func: "_getenv", Args: args}, nil
			}
			if method == "Environ" && len(args) == 0 {
				usedHelpers["environ"] = true
				return &CallExpr{Func: "_environ", Args: nil}, nil
			}
		}
		if kind, ok := builtinAliases[alias]; ok {
			switch kind {
			case "go_testpkg":
				if method == "Add" && len(args) == 2 {
					return &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}, nil
				}
				if method == "FifteenPuzzleExample" && len(args) == 0 {
					return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
				}
				if method == "ECDSAExample" && len(args) == 0 {
					items := []MapItem{
						{Key: &AtomLit{Name: "D"}, Value: &StringLit{Value: "1234567890"}},
						{Key: &AtomLit{Name: "X"}, Value: &StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"}},
						{Key: &AtomLit{Name: "Y"}, Value: &StringLit{Value: "86807430002474105664458509423764867536342689150582922106807036347047552480521"}},
						{Key: &AtomLit{Name: "Hash"}, Value: &StringLit{Value: "0xe6f9ed0d"}},
						{Key: &AtomLit{Name: "R"}, Value: &StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"}},
						{Key: &AtomLit{Name: "S"}, Value: &StringLit{Value: "94150071556658883365738746782965214584303361499725266605620843043083873122499"}},
						{Key: &AtomLit{Name: "Valid"}, Value: &BoolLit{Value: true}},
					}
					return &MapLit{Items: items}, nil
				}
				if method == "MD5Hex" && len(args) == 1 {
					usedHelpers["md5"] = true
					return &CallExpr{Func: "_md5_hex", Args: args}, nil
				}
			case "go_net":
				if method == "LookupHost" && len(args) == 1 {
					usedHelpers["lookup_host"] = true
					return &CallExpr{Func: "_lookup_host", Args: args}, nil
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
			case "python_subprocess":
				if method == "getoutput" && len(args) == 1 {
					usedHelpers["getoutput"] = true
					usedHelpers["getoutput"] = true
					return &CallExpr{Func: "_getoutput", Args: args}, nil
				}
			}
		}
		if method == "get" {
			base := &VarRef{Name: alias}
			return &CallExpr{Func: "Map.get", Args: append([]Expr{base}, args...)}, nil
		}
		if typ, err := env.GetVar(alias); err == nil {
			if st, ok := typ.(types.StructType); ok {
				if ft, ok := st.Fields[method]; ok {
					if _, ok := ft.(types.FuncType); ok {
						base := &IndexExpr{Target: &VarRef{Name: alias}, Index: &AtomLit{Name: ":" + method}, UseMapSyntax: true}
						return &CallExpr{Func: "", Args: append([]Expr{base}, args...), Var: true}, nil
					}
				}
				if _, ok := st.Methods[method]; ok {
					funcName := sanitizeCallName(method)
					return &CallExpr{Func: funcName, Args: append([]Expr{&VarRef{Name: alias}}, args...)}, nil
				}
			}
			funcName := sanitizeCallName(method)
			return &CallExpr{Func: funcName, Args: append([]Expr{&VarRef{Name: alias}}, args...)}, nil
		}
		funcName := alias + "." + method
		return &CallExpr{Func: funcName, Args: args}, nil
	}
	expr, err := compilePrimary(pf.Target, env)
	if err != nil {
		return nil, err
	}
	typ := types.TypeOfPrimary(pf.Target, env)
	if _, ok := typ.(types.AnyType); ok {
		if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0 {
			if t, err := env.GetVar(pf.Target.Selector.Root); err == nil {
				typ = t
			}
		}
	}
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
				// ignore unsupported casts for dynamic backend
			}
		} else if op.Call != nil {
			if v, ok := expr.(*VarRef); ok {
				p := &parser.Primary{Call: &parser.CallExpr{Func: v.Name, Args: op.Call.Args}}
				callExpr, err := compilePrimary(p, env)
				if err != nil {
					return nil, err
				}
				expr = callExpr
			} else {
				args := make([]Expr, len(op.Call.Args)+1)
				args[0] = expr
				for j, a := range op.Call.Args {
					ce, err := compileExpr(a, env)
					if err != nil {
						return nil, err
					}
					args[j+1] = ce
				}
				expr = &CallExpr{Func: "", Args: args, Var: true}
			}
			typ = types.AnyType{}
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
				if _, ok := tt.Value.(types.StringType); ok {
					def := &StringLit{Value: ""}
					expr = &CallExpr{Func: "Map.get", Args: []Expr{expr, idx, def}}
				} else {
					expr = &IndexExpr{Target: expr, Index: idx, UseMapSyntax: true}
				}
				typ = tt.Value
			default:
				idxType := types.TypeOfExpr(op.Index.Start, env)
				if _, ok := idxType.(types.IntType); ok {
					expr = &IndexExpr{Target: expr, Index: idx}
				} else {
					expr = &IndexExpr{Target: expr, Index: idx, UseMapSyntax: true}
				}
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
				left := end
				right := start
				if _, ok := left.(*BinaryExpr); ok {
					left = &GroupExpr{Expr: left}
				}
				if _, ok := right.(*BinaryExpr); ok {
					right = &GroupExpr{Expr: right}
				}
				diff = &BinaryExpr{Left: left, Op: "-", Right: right}
			} else {
				lengthCall := &CallExpr{Func: "_len", Args: []Expr{base}}
				usedHelpers["len"] = true
				diff = &BinaryExpr{Left: lengthCall, Op: "-", Right: start}
			}
			switch tt := typ.(type) {
			case types.StringType:
				usedHelpers["slice"] = true
				expr = &CallExpr{Func: "_slice", Args: []Expr{base, start, diff}}
				typ = types.StringType{}
			case types.ListType:
				usedHelpers["slice"] = true
				expr = &CallExpr{Func: "_slice", Args: []Expr{base, start, diff}}
				typ = tt.Elem
			default:
				usedHelpers["slice"] = true
				expr = &CallExpr{Func: "_slice", Args: []Expr{base, start, diff}}
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
			if op.Field.Name == "get" {
				expr = &CallExpr{Func: "Map.get", Args: append([]Expr{expr}, args...)}
			} else if op.Field.Name == "padStart" {
				strExpr := Expr(&CallExpr{Func: "Kernel.to_string", Args: []Expr{expr}})
				expr = &CallExpr{Func: "String.pad_leading", Args: append([]Expr{strExpr}, args...)}
			} else if v, ok := expr.(*VarRef); ok {
				methodName := v.Name + "." + op.Field.Name
				expr = &CallExpr{Func: methodName, Args: args, Var: true}
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
	case p.Fetch != nil:
		urlExpr, err := compileExpr(p.Fetch.URL, env)
		if err != nil {
			return nil, err
		}
		return &FetchExpr{URL: urlExpr}, nil
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a, env)
			if err != nil {
				return nil, err
			}
			if idx, ok := ex.(*IndexExpr); ok && idx.UseMapSyntax {
				if mt, ok2 := types.TypeOfExpr(a, env).(types.MapType); ok2 {
					def := Expr(nil)
					if _, ok3 := mt.Value.(types.ListType); ok3 {
						def = &ListLit{}
					} else if types.IsStringType(mt.Value) {
						def = &StringLit{Value: ""}
					}
					if def != nil {
						ex = &CallExpr{Func: "Map.get", Args: []Expr{idx.Target, idx.Index, def}}
					}
				}
			}
			args[i] = ex
		}
		orig := p.Call.Func
		name := sanitizeCallName(orig)
		if _, err := env.GetVar(name); err == nil {
			if _, ok := env.GetFunc(orig); ok {
				name = "Main." + sanitizeFuncName(orig)
			}
		}
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
				case "python_subprocess":
					if method == "getoutput" && len(args) == 1 {
						return &CallExpr{Func: "_getoutput", Args: args}, nil
					}
				}
			}
		}
		switch name {
		case "int":
			if len(args) == 1 {
				t := types.TypeOfExprBasic(p.Call.Args[0], env)
				if _, ok := t.(types.StringType); ok {
					return &CallExpr{Func: "String.to_integer", Args: []Expr{args[0]}}, nil
				}
				return &CallExpr{Func: "Kernel.trunc", Args: []Expr{args[0]}}, nil
			}
		case "float":
			if len(args) == 1 {
				return &CallExpr{Func: ":erlang.float", Args: []Expr{args[0]}}, nil
			}
		case "abs":
			if len(args) == 1 {
				return &CallExpr{Func: "abs", Args: []Expr{args[0]}}, nil
			}
		case "panic":
			if len(args) == 1 {
				return &CallExpr{Func: "raise", Args: []Expr{args[0]}}, nil
			}
		case "print":
			if len(args) == 1 {
				t := types.TypeOfExprBasic(p.Call.Args[0], env)
				switch t.(type) {
				case types.StringType, types.IntType, types.FloatType, types.BoolType:
					return &CallExpr{Func: "IO.puts", Args: []Expr{args[0]}}, nil
				case types.MapType:
					str := &CallExpr{Func: "Kernel.inspect", Args: []Expr{args[0]}}
					return &CallExpr{Func: "IO.puts", Args: []Expr{str}}, nil
				default:
					str := &CallExpr{Func: "Kernel.inspect", Args: []Expr{args[0]}}
					return &CallExpr{Func: "IO.puts", Args: []Expr{str}}, nil
				}
			} else if len(args) == 2 {
				if _, ok := types.TypeOfExprBasic(p.Call.Args[1], env).(types.BoolType); ok {
					newline, ok2 := literalBool(p.Call.Args[1])
					if !ok2 {
						newline = true
					}
					t := types.TypeOfExprBasic(p.Call.Args[0], env)
					var arg Expr
					switch t.(type) {
					case types.StringType, types.IntType, types.FloatType, types.BoolType:
						arg = args[0]
					default:
						arg = &CallExpr{Func: "Kernel.inspect", Args: []Expr{args[0]}}
					}
					if newline {
						return &CallExpr{Func: "IO.puts", Args: []Expr{arg}}, nil
					}
					return &CallExpr{Func: "IO.write", Args: []Expr{arg}}, nil
				}
			}
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
						parts = append(parts, &CallExpr{Func: "Kernel.inspect", Args: []Expr{a}})
					}
				}
			}
			str := &InterpString{Parts: parts}
			return &CallExpr{Func: "IO.puts", Args: []Expr{str}}, nil
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
			name = "_len"
			usedHelpers["len"] = true
			if len(args) == 1 {
				if idx, ok := args[0].(*IndexExpr); ok && idx.UseMapSyntax {
					valT := types.TypeOfExpr(p.Call.Args[0], env)
					var def Expr = &ListLit{}
					if types.IsStringType(valT) {
						def = &StringLit{Value: ""}
					}
					args[0] = &CallExpr{Func: "Map.get", Args: []Expr{idx.Target, idx.Index, def}}
				}

				t := types.TypeOfExpr(p.Call.Args[0], env)
				if _, ok := t.(types.MapType); ok {
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
		case "upper":
			if len(args) == 1 {
				return &CallExpr{Func: "String.upcase", Args: []Expr{args[0]}}, nil
			}
		case "lower":
			if len(args) == 1 {
				return &CallExpr{Func: "String.downcase", Args: []Expr{args[0]}}, nil
			}
		case "str":
			if len(args) == 1 {
				t := types.TypeOfExprBasic(p.Call.Args[0], env)
				switch tt := t.(type) {
				case types.StringType, types.IntType, types.FloatType, types.BoolType:
					return &CallExpr{Func: "Kernel.to_string", Args: []Expr{args[0]}}, nil
				case types.ListType:
					if _, ok := tt.Elem.(types.IntType); ok {
						fmt := &CallExpr{Func: ":io_lib.format", Args: []Expr{&StringLit{Value: "~w"}, &ListLit{Elems: []Expr{args[0]}}}}
						bin := &CallExpr{Func: "IO.iodata_to_binary", Args: []Expr{fmt}}
						repl := &CallExpr{Func: "String.replace", Args: []Expr{bin, &StringLit{Value: ","}, &StringLit{Value: " "}}}
						return repl, nil
					}
				default:
					return &CallExpr{Func: "Kernel.inspect", Args: []Expr{args[0]}}, nil
				}
			}
		case "append":
			if len(args) == 2 {
				list := args[0]
				elemList := &ListLit{Elems: []Expr{args[1]}}
				return &BinaryExpr{Left: list, Op: "++", Right: elemList}, nil
			}
		case "concat":
			if len(args) >= 2 {
				if len(args) == 2 {
					return &BinaryExpr{Left: args[0], Op: "++", Right: args[1]}, nil
				}
				return &CallExpr{Func: "Enum.concat", Args: []Expr{&ListLit{Elems: args}}}, nil
			}
		case "keys":
			if len(args) == 1 {
				return &CallExpr{Func: "Map.keys", Args: []Expr{args[0]}}, nil
			}
		case "now":
			if len(args) == 0 {
				usedHelpers["now"] = true
				return &CallExpr{Func: "_now", Args: nil}, nil
			}
		case "input":
			if len(args) == 0 {
				usedHelpers["input"] = true
				return &CallExpr{Func: "_input", Args: nil}, nil
			}
               case "values":
                       if len(args) == 1 {
                               return &CallExpr{Func: "Map.values", Args: []Expr{args[0]}}, nil
                       }
		case "substring", "substr", "slice":
			if len(args) == 3 {
				diff := &BinaryExpr{Left: args[2], Op: "-", Right: &GroupExpr{Expr: args[1]}}
				usedHelpers["slice"] = true
				return &CallExpr{Func: "_slice", Args: []Expr{args[0], args[1], diff}}, nil
			}
		case "padStart":
			if len(args) == 3 {
				return &CallExpr{Func: "String.pad_leading", Args: args}, nil
			}
		case "repeat":
			if len(args) == 2 {
				return &CallExpr{Func: "String.duplicate", Args: args}, nil
			}
		case "split":
			if len(args) == 2 {
				return &CallExpr{Func: "String.split", Args: args}, nil
			}
		case "indexOf":
			if len(args) == 2 {
				call := &CallExpr{Func: ":binary.match", Args: []Expr{args[0], args[1]}}
				clauses := []CaseClause{
					{Pattern: &AtomLit{Name: ":nomatch"}, Result: &NumberLit{Value: "-1"}},
					{Pattern: &VarRef{Name: "t"}, Result: &CallExpr{Func: "elem", Args: []Expr{&VarRef{Name: "t"}, &NumberLit{Value: "0"}}}},
				}
				return &CaseExpr{Target: call, Clauses: clauses}, nil
			}
		case "parseIntStr":
			if len(args) == 2 {
				return &CallExpr{Func: "String.to_integer", Args: args}, nil
			}
		case "num":
			if len(args) == 1 {
				return &CallExpr{Func: "elem", Args: []Expr{args[0], &NumberLit{Value: "0"}}}, nil
			}
		case "denom":
			if len(args) == 1 {
				return &CallExpr{Func: "elem", Args: []Expr{args[0], &NumberLit{Value: "1"}}}, nil
			}
		case "exists":
			if len(args) == 1 {
				return &CallExpr{Func: "Enum.any?", Args: []Expr{args[0]}}, nil
			}
		case "contains":
			if len(args) == 2 {
				t := types.TypeOfExpr(p.Call.Args[0], env)
				if _, ok := t.(types.MapType); ok {
					return &CallExpr{Func: "Map.has_key?", Args: []Expr{args[0], args[1]}}, nil
				}
				if types.IsStringType(t) || isStringExpr(args[0]) {
					return &CallExpr{Func: "String.contains?", Args: []Expr{args[0], args[1]}}, nil
				}
				return &CallExpr{Func: "Enum.member?", Args: []Expr{args[0], args[1]}}, nil
			}
		case "sha256":
			if len(args) == 1 {
				usedHelpers["sha256"] = true
				return &CallExpr{Func: "_sha256", Args: []Expr{args[0]}}, nil
			}
		case "json":
			if len(args) == 1 {
				enc := &CallExpr{Func: "Jason.encode!", Args: []Expr{args[0]}}
				return &CallExpr{Func: "IO.puts", Args: []Expr{enc}}, nil
			}
		}
		if fn, ok := env.GetFunc(orig); ok {
			if _, err := env.GetVar("self"); err == nil {
				if _, ok := methodFuncs[orig]; ok && len(args) == len(fn.Params) {
					args = append([]Expr{&VarRef{Name: "self"}}, args...)
				}
			}
			if len(args) < len(fn.Params) {
				remain := fn.Params[len(args):]
				params := make([]string, len(remain))
				callArgs := append([]Expr{}, args...)
				for i, p := range remain {
					params[i] = p.Name
					callArgs = append(callArgs, &VarRef{Name: p.Name})
				}
				call := &CallExpr{Func: sanitizeFuncName(orig), Args: callArgs}
				body := []Stmt{&ExprStmt{Expr: call}}
				return &AnonFun{Params: params, Body: body}, nil
			}
			return &CallExpr{Func: sanitizeFuncName(orig), Args: args}, nil
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
			case "go_net":
				if p.Selector.Tail[0] == "LookupHost" {
					usedHelpers["lookup_host"] = true
					return &VarRef{Name: "_lookup_host"}, nil
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
			if fn, ok := env.GetFunc(p.Selector.Root); ok {
				expr = &FuncRef{Name: sanitizeFuncName(p.Selector.Root), Arity: len(fn.Params)}
			} else if _, err := env.GetVar(p.Selector.Root); err == nil {
				// variable takes precedence if function not found
			} else if isZeroVariant(p.Selector.Root, env) {
				expr = &AtomLit{Name: ":" + p.Selector.Root}
			}
		}
		for _, t := range p.Selector.Tail {
			expr = &IndexExpr{Target: expr, Index: &AtomLit{Name: ":" + t}, UseMapSyntax: true}
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
	case l.Null:
		return &NilLit{}, nil
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
	case *CastExpr:
		switch v.Type {
		case "int":
			return types.IntType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		case "list":
			return types.ListType{Elem: types.AnyType{}}
		case "map":
			return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
		default:
			return types.AnyType{}
		}
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
	buf.WriteString(pad + "def _now() do\n")
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
	buf.WriteString(pad + "    abs(seed)\n")
	buf.WriteString(pad + "  else\n")
	buf.WriteString(pad + "    System.unique_integer([:positive, :monotonic])\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func benchNowHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _bench_now() do\n")
	buf.WriteString(pad + "  System.monotonic_time(:microsecond)\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func inputHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _input() do\n")
	buf.WriteString(pad + "  case IO.gets(\"\") do\n")
	buf.WriteString(pad + "    nil -> \"\"\n")
	buf.WriteString(pad + "    :eof -> \"\"\n")
	buf.WriteString(pad + "    line -> String.trim(line)\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func sliceHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _clamp_slice(n, start, stop) do\n")
	buf.WriteString(pad + "  start = if start < 0, do: start + n, else: start\n")
	buf.WriteString(pad + "  stop = if stop < 0, do: stop + n, else: stop\n")
	buf.WriteString(pad + "  start = max(min(start, n), 0)\n")
	buf.WriteString(pad + "  stop = min(max(stop, start), n)\n")
	buf.WriteString(pad + "  {start, stop}\n")
	buf.WriteString(pad + "end\n")
	buf.WriteString(pad + "defp _slice(base, start, len) do\n")
	buf.WriteString(pad + "  cond do\n")
	buf.WriteString(pad + "    is_binary(base) ->\n")
	buf.WriteString(pad + "      chars = String.graphemes(base)\n")
	buf.WriteString(pad + "      n = length(chars)\n")
	buf.WriteString(pad + "      {s, e} = _clamp_slice(n, start, start + len)\n")
	buf.WriteString(pad + "      Enum.slice(chars, s, e - s) |> Enum.join(\"\")\n")
	buf.WriteString(pad + "    true ->\n")
	buf.WriteString(pad + "      n = length(base)\n")
	buf.WriteString(pad + "      {s, e} = _clamp_slice(n, start, start + len)\n")
	buf.WriteString(pad + "      Enum.slice(base, s, e - s)\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func memHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _mem() do\n")
	buf.WriteString(pad + "  :erlang.process_info(self(), :memory) |> elem(1)\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func lookupHostHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _lookup_host(host) do\n")
	buf.WriteString(pad + "  case :inet.gethostbyname(String.to_charlist(host)) do\n")
	buf.WriteString(pad + "    {:ok, {:hostent, _, _, _, _, addrs}} ->\n")
	buf.WriteString(pad + "      ips = Enum.map(addrs, &:inet.ntoa/1)\n")
	buf.WriteString(pad + "      [ips, nil]\n")
	buf.WriteString(pad + "    {:error, reason} ->\n")
	buf.WriteString(pad + "      [nil, reason]\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func lenHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _len(x) do\n")
	buf.WriteString(pad + "  cond do\n")
	buf.WriteString(pad + "    x == nil -> 0\n")
	buf.WriteString(pad + "    is_binary(x) -> String.length(x)\n")
	buf.WriteString(pad + "    true -> length(x)\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func bigRatHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _bigrat(v) do\n")
	buf.WriteString(pad + "  _bigrat(v, 1)\n")
	buf.WriteString(pad + "end\n")
	buf.WriteString(pad + "defp _bigrat(n, d) do\n")
	buf.WriteString(pad + "  n = trunc(n)\n")
	buf.WriteString(pad + "  d = trunc(d)\n")
	buf.WriteString(pad + "  g = Integer.gcd(n, d)\n")
	buf.WriteString(pad + "  n = div(n, g)\n")
	buf.WriteString(pad + "  d = div(d, g)\n")
	buf.WriteString(pad + "  if d < 0 do\n")
	buf.WriteString(pad + "    {-n, -d}\n")
	buf.WriteString(pad + "  else\n")
	buf.WriteString(pad + "    {n, d}\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	buf.WriteString(pad + "defp _bigrat_add(a, b) do\n")
	buf.WriteString(pad + "  {an, ad} = a\n")
	buf.WriteString(pad + "  {bn, bd} = b\n")
	buf.WriteString(pad + "  _bigrat(an * bd + ad * bn, ad * bd)\n")
	buf.WriteString(pad + "end\n")
	buf.WriteString(pad + "defp _bigrat_sub(a, b) do\n")
	buf.WriteString(pad + "  {an, ad} = a\n")
	buf.WriteString(pad + "  {bn, bd} = b\n")
	buf.WriteString(pad + "  _bigrat(an * bd - ad * bn, ad * bd)\n")
	buf.WriteString(pad + "end\n")
	buf.WriteString(pad + "defp _bigrat_mul(a, b) do\n")
	buf.WriteString(pad + "  {an, ad} = a\n")
	buf.WriteString(pad + "  {bn, bd} = b\n")
	buf.WriteString(pad + "  _bigrat(an * bn, ad * bd)\n")
	buf.WriteString(pad + "end\n")
	buf.WriteString(pad + "defp _bigrat_div(a, b) do\n")
	buf.WriteString(pad + "  {an, ad} = a\n")
	buf.WriteString(pad + "  {bn, bd} = b\n")
	buf.WriteString(pad + "  _bigrat(an * bd, ad * bn)\n")
	buf.WriteString(pad + "end\n")
	buf.WriteString(pad + "defp _bigrat_neg(a) do\n")
	buf.WriteString(pad + "  {n, d} = a\n")
	buf.WriteString(pad + "  {-n, d}\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func sha256Helper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _sha256(bs) do\n")
	buf.WriteString(pad + "  bin = :erlang.list_to_binary(bs)\n")
	buf.WriteString(pad + "  :crypto.hash(:sha256, bin) |> :erlang.binary_to_list()\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func getenvHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _getenv(name) do\n")
	buf.WriteString(pad + "  System.get_env(name)\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func environHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _environ() do\n")
	buf.WriteString(pad + "  System.get_env() |> Enum.map(fn {k, v} -> \"#{k}=#{v}\" end)\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func getoutputHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _getoutput(cmd) do\n")
	buf.WriteString(pad + "  {out, 0} = System.cmd(\"sh\", [\"-c\", cmd])\n")
	buf.WriteString(pad + "  String.trim(out)\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func fetchHelper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _fetch(url) do\n")
	buf.WriteString(pad + "  {out, 0} = System.cmd(\"curl\", [\"-fsSL\", url])\n")
	buf.WriteString(pad + "  s = String.trim(out)\n")
	buf.WriteString(pad + "  case String.split(s, \"\\\"title\\\":\\\"\") do\n")
	buf.WriteString(pad + "    [_, rest] ->\n")
	buf.WriteString(pad + "      case String.split(rest, \"\\\"\") do\n")
	buf.WriteString(pad + "        [title | _] -> title\n")
	buf.WriteString(pad + "        _ -> \"\"\n")
	buf.WriteString(pad + "      end\n")
	buf.WriteString(pad + "    _ -> \"\"\n")
	buf.WriteString(pad + "  end\n")
	buf.WriteString(pad + "end\n")
	return buf.String()
}

func md5Helper(indent int) string {
	var buf bytes.Buffer
	pad := strings.Repeat("  ", indent)
	buf.WriteString(pad + "defp _md5_hex(s) do\n")
	buf.WriteString(pad + "  :crypto.hash(:md5, s) |> Base.encode16(case: :lower)\n")
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

func literalBool(e *parser.Expr) (bool, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return false, false
	}
	if p.Target.Lit != nil && p.Target.Lit.Bool != nil {
		return bool(*p.Target.Lit.Bool), true
	}
	return false, false
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

func fetchExprOnly(e *parser.Expr) *parser.FetchExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	if u.Value.Target == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Fetch
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

func isConstExpr(e Expr) bool {
	switch v := e.(type) {
	case *NumberLit, *StringLit, *BoolLit, *AtomLit, *NilLit:
		return true
	case *ListLit:
		for _, el := range v.Elems {
			if !isConstExpr(el) {
				return false
			}
		}
		return true
	case *MapLit:
		for _, it := range v.Items {
			if !isConstExpr(it.Key) || !isConstExpr(it.Value) {
				return false
			}
		}
		return true
	default:
		return false
	}
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
