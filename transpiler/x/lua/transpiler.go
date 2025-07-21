//go:build slow

package lua

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
	"unicode"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

var currentEnv *types.Env
var loopCounter int
var continueLabels []string

// Program represents a simple Lua program consisting of a sequence of
// statements.
type Program struct {
	Stmts []Stmt
	Env   *types.Env
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }
type AssignStmt struct {
	Name  string
	Value Expr
}
type QueryAssignStmt struct {
	Name  string
	Query *QueryComp
}
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}
type FunStmt struct {
	Name   string
	Params []string
	Body   []Stmt
}
type ReturnStmt struct{ Value Expr }

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

type BreakStmt struct{}

type ContinueStmt struct{}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

type CallExpr struct {
	Func string
	Args []Expr
}

type StringLit struct{ Value string }
type IntLit struct{ Value int }
type FloatLit struct{ Value float64 }
type BoolLit struct{ Value bool }
type Ident struct{ Name string }
type ListLit struct{ Elems []Expr }
type MapLit struct{ Keys, Values []Expr }
type MapItem struct{ Key, Value Expr }
type IndexExpr struct {
	Target Expr
	Index  Expr
	Kind   string
}
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
	Kind   string
}
type FunExpr struct {
	Params []string
	Body   []Stmt
	Expr   Expr
}
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

type UnaryExpr struct {
	Op    string
	Value Expr
}

type MatchArm struct {
	Pattern Expr // nil means wildcard
	Result  Expr
}

type MatchExpr struct {
	Target Expr
	Arms   []MatchArm
}

type QueryComp struct {
	Vars     []string
	Sources  []Expr
	Sides    []string
	Body     Expr
	Where    Expr
	GroupKey Expr
	GroupVar string
	Having   Expr
	Agg      string
	SortKey  Expr
	Skip     Expr
	Take     Expr
}

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

func (c *CallExpr) emit(w io.Writer) {
	switch c.Func {
	case "print":
		if len(c.Args) == 1 && (isListExpr(c.Args[0]) || isMapExpr(c.Args[0])) {
			(&CallExpr{Func: "json", Args: c.Args}).emit(w)
			return
		}

		var fmtBuf strings.Builder
		var exprs []Expr
		for i, a := range c.Args {
			if i > 0 {
				fmtBuf.WriteByte(' ')
			}
			if s, ok := a.(*StringLit); ok {
				fmtBuf.WriteString(s.Value)
			} else if isIntExpr(a) || isBoolExpr(a) {
				fmtBuf.WriteString("%d")
				if isBoolExpr(a) && !isIntExpr(a) {
					exprs = append(exprs, &IfExpr{Cond: a, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}})
				} else {
					exprs = append(exprs, a)
				}
			} else {
				fmtBuf.WriteString("%s")
				exprs = append(exprs, a)
			}
		}

		switch {
		case len(exprs) == 0:
			io.WriteString(w, "print(")
			fmt.Fprintf(w, "%q", fmtBuf.String())
			io.WriteString(w, ")")
		case len(exprs) == 1 && (fmtBuf.String() == "%s" || fmtBuf.String() == "%d"):
			io.WriteString(w, "print(")
			exprs[0].emit(w)
			io.WriteString(w, ")")
		default:
			io.WriteString(w, "print(string.format(")
			fmt.Fprintf(w, "%q", fmtBuf.String())
			for _, a := range exprs {
				io.WriteString(w, ", ")
				a.emit(w)
			}
			io.WriteString(w, "))")
		}
		return
	case "len", "count":
		io.WriteString(w, "(function(v)\n  if type(v) == 'table' and v.items ~= nil then\n    return #v.items\n  elseif type(v) == 'table' and (v[1] == nil) then\n    local c = 0\n    for _ in pairs(v) do c = c + 1 end\n    return c\n  else\n    return #v\n  end\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "str":
		io.WriteString(w, "tostring(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "substring":
		io.WriteString(w, "string.sub(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, " + 1, ")
		if len(c.Args) > 2 {
			c.Args[2].emit(w)
		}
		io.WriteString(w, ")")
	case "min":
		io.WriteString(w, "(function(lst)\n  local m = nil\n  for _, v in ipairs(lst) do\n    if m == nil or v < m then\n      m = v\n    end\n  end\n  return m\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "max":
		io.WriteString(w, "(function(lst)\n  local m = nil\n  for _, v in ipairs(lst) do\n    if m == nil or v > m then\n      m = v\n    end\n  end\n  return m\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "values":
		io.WriteString(w, "(function(m)\n  local keys = {}\n  for k in pairs(m) do\n    table.insert(keys, k)\n  end\n  table.sort(keys, function(a,b) return a<b end)\n  local res = {}\n  for _, k in ipairs(keys) do\n    table.insert(res, m[k])\n  end\n  return res\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "append":
		io.WriteString(w, "(function(lst, item)\n  local res = {table.unpack(lst)}\n  table.insert(res, item)\n  return res\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ")")
	case "avg":
		io.WriteString(w, "(function(lst)\n  local sum = 0\n  for _, v in ipairs(lst) do\n    sum = sum + v\n  end\n  if #lst == 0 then\n    return 0\n  end\n  local r = sum / #lst\n  if r == math.floor(r) then return math.floor(r) end\n  return string.format('%.15f', r)\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "sum":
		io.WriteString(w, "(function(lst)\n  local s = 0\n  for _, v in ipairs(lst) do\n    s = s + v\n  end\n  return s\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "exists":
		io.WriteString(w, "(#(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ") > 0)")
	case "contains":
		if len(c.Args) > 0 && isMapExpr(c.Args[0]) {
			io.WriteString(w, "(function(m, k)\n  return m[k] ~= nil\nend)(")
			c.Args[0].emit(w)
			io.WriteString(w, ", ")
			if len(c.Args) > 1 {
				c.Args[1].emit(w)
			}
			io.WriteString(w, ")")
		} else if len(c.Args) > 0 && isStringExpr(c.Args[0]) {
			io.WriteString(w, "(string.find(")
			c.Args[0].emit(w)
			io.WriteString(w, ", ")
			if len(c.Args) > 1 {
				c.Args[1].emit(w)
			}
			io.WriteString(w, ", 1, true) ~= nil)")
		} else {
			io.WriteString(w, "(function(lst, v)\n  for _, x in ipairs(lst) do\n    if x == v then\n      return true\n    end\n  end\n  return false\nend)(")
			if len(c.Args) > 0 {
				c.Args[0].emit(w)
			}
			io.WriteString(w, ", ")
			if len(c.Args) > 1 {
				c.Args[1].emit(w)
			}
			io.WriteString(w, ")")
		}
	case "json":
		io.WriteString(w, `;
(function(v)
  local function encode(x)
    if type(x) == "table" then
      if #x > 0 then
        local parts = {"["}
        for i, val in ipairs(x) do
          parts[#parts+1] = encode(val)
          if i < #x then parts[#parts+1] = ", " end
        end
        parts[#parts+1] = "]"
        return table.concat(parts)
      else
        local keys = {}
        for k in pairs(x) do table.insert(keys, k) end
        table.sort(keys, function(a,b) return tostring(a) < tostring(b) end)
        local parts = {"{"}
        for i, k in ipairs(keys) do
          parts[#parts+1] = "'" .. tostring(k) .. "': " .. encode(x[k])
          if i < #keys then parts[#parts+1] = ", " end
        end
        parts[#parts+1] = "}"
        return table.concat(parts)
      end
    elseif type(x) == "string" then
      return "'" .. x .. "'"
    else
      return tostring(x)
    end
  end
  print(encode(v))
end)(`)
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, `)`)
	default:
		io.WriteString(w, c.Func)
		io.WriteString(w, "(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			a.emit(w)
		}
		io.WriteString(w, ")")
	}
}

func (a *AssignStmt) emit(w io.Writer) {
	io.WriteString(w, a.Name)
	io.WriteString(w, " = ")
	if a.Value == nil {
		io.WriteString(w, "nil")
	} else {
		a.Value.emit(w)
	}
}

func (qa *QueryAssignStmt) emit(w io.Writer) {
	io.WriteString(w, qa.Name)
	io.WriteString(w, " = ")
	qa.Query.emit(w)
	w.Write([]byte{'\n'})
}

func (a *IndexAssignStmt) emit(w io.Writer) {
	a.Target.emit(w)
	io.WriteString(w, " = ")
	if a.Value == nil {
		io.WriteString(w, "nil")
	} else {
		a.Value.emit(w)
	}
}

func (f *FunStmt) emit(w io.Writer) {
	io.WriteString(w, "function ")
	io.WriteString(w, f.Name)
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ")\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "return")
	if r.Value != nil {
		io.WriteString(w, " ")
		r.Value.emit(w)
	}
}

func (i *IfStmt) emit(w io.Writer) {
	io.WriteString(w, "if ")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, " then\n")
	for _, st := range i.Then {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if len(i.Else) > 0 {
		io.WriteString(w, "else\n")
		for _, st := range i.Else {
			st.emit(w)
			io.WriteString(w, "\n")
		}
	}
	io.WriteString(w, "end")
}

func (wst *WhileStmt) emit(w io.Writer) {
	var label string
	if hasContinue(wst.Body) {
		label = newContinueLabel()
		continueLabels = append(continueLabels, label)
	}
	io.WriteString(w, "while ")
	if wst.Cond != nil {
		wst.Cond.emit(w)
	}
	io.WriteString(w, " do\n")
	for _, st := range wst.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if label != "" {
		io.WriteString(w, "::")
		io.WriteString(w, label)
		io.WriteString(w, "::\n")
		continueLabels = continueLabels[:len(continueLabels)-1]
	}
	io.WriteString(w, "end")
}

func (fr *ForRangeStmt) emit(w io.Writer) {
	var label string
	if hasContinue(fr.Body) {
		label = newContinueLabel()
		continueLabels = append(continueLabels, label)
	}
	io.WriteString(w, "for ")
	io.WriteString(w, fr.Name)
	io.WriteString(w, " = ")
	if fr.Start != nil {
		fr.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, ", ")
	if fr.End != nil {
		fr.End.emit(w)
		io.WriteString(w, " - 1")
	}
	io.WriteString(w, " do\n")
	for _, st := range fr.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if label != "" {
		io.WriteString(w, "::")
		io.WriteString(w, label)
		io.WriteString(w, "::\n")
		continueLabels = continueLabels[:len(continueLabels)-1]
	}
	io.WriteString(w, "end")
}

func (fi *ForInStmt) emit(w io.Writer) {
	var label string
	if hasContinue(fi.Body) {
		label = newContinueLabel()
		continueLabels = append(continueLabels, label)
	}
	io.WriteString(w, "for ")
	if isMapExpr(fi.Iterable) {
		io.WriteString(w, fi.Name)
		io.WriteString(w, " in pairs(")
	} else {
		io.WriteString(w, "_, ")
		io.WriteString(w, fi.Name)
		io.WriteString(w, " in ipairs(")
	}
	if fi.Iterable != nil {
		fi.Iterable.emit(w)
	}
	io.WriteString(w, ") do\n")
	for _, st := range fi.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if label != "" {
		io.WriteString(w, "::")
		io.WriteString(w, label)
		io.WriteString(w, "::\n")
		continueLabels = continueLabels[:len(continueLabels)-1]
	}
	io.WriteString(w, "end")
}

func (b *BreakStmt) emit(w io.Writer) { io.WriteString(w, "break") }

func (c *ContinueStmt) emit(w io.Writer) {
	if len(continueLabels) == 0 {
		io.WriteString(w, "-- continue")
		return
	}
	io.WriteString(w, "goto ")
	io.WriteString(w, continueLabels[len(continueLabels)-1])
}

func (ie *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "((")
	ie.Cond.emit(w)
	io.WriteString(w, ") and (")
	ie.Then.emit(w)
	io.WriteString(w, ") or (")
	ie.Else.emit(w)
	io.WriteString(w, "))")
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }
func (i *IntLit) emit(w io.Writer)    { fmt.Fprintf(w, "%d", i.Value) }
func (f *FloatLit) emit(w io.Writer)  { fmt.Fprintf(w, "%g", f.Value) }
func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}
func (l *ListLit) emit(w io.Writer) {
	io.WriteString(w, "{")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, "}")
}

func (m *MapLit) emit(w io.Writer) {
	io.WriteString(w, "{")
	for i := range m.Keys {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if s, ok := m.Keys[i].(*StringLit); ok && isLuaIdent(s.Value) {
			io.WriteString(w, s.Value)
			io.WriteString(w, " = ")
			m.Values[i].emit(w)
		} else {
			io.WriteString(w, "[")
			m.Keys[i].emit(w)
			io.WriteString(w, "] = ")
			m.Values[i].emit(w)
		}
	}
	io.WriteString(w, "}")
}

func (ix *IndexExpr) emit(w io.Writer) {
	switch ix.Kind {
	case "string":
		io.WriteString(w, "string.sub(")
		ix.Target.emit(w)
		io.WriteString(w, ", (")
		ix.Index.emit(w)
		io.WriteString(w, " + 1), (")
		ix.Index.emit(w)
		io.WriteString(w, " + 1)")
		io.WriteString(w, ")")
	case "list":
		ix.Target.emit(w)
		io.WriteString(w, "[")
		ix.Index.emit(w)
		io.WriteString(w, " + 1]")
	default: // map
		if s, ok := ix.Index.(*StringLit); ok && isLuaIdent(s.Value) {
			ix.Target.emit(w)
			io.WriteString(w, ".")
			io.WriteString(w, s.Value)
		} else {
			ix.Target.emit(w)
			io.WriteString(w, "[")
			ix.Index.emit(w)
			io.WriteString(w, "]")
		}
	}
}

func (sx *SliceExpr) emit(w io.Writer) {
	if sx.Kind == "string" {
		io.WriteString(w, "string.sub(")
		sx.Target.emit(w)
		io.WriteString(w, ", ")
		if sx.Start != nil {
			io.WriteString(w, "(")
			sx.Start.emit(w)
			io.WriteString(w, " + 1)")
		} else {
			io.WriteString(w, "1")
		}
		io.WriteString(w, ", ")
		if sx.End != nil {
			sx.End.emit(w)
		} else {
			io.WriteString(w, "#")
			sx.Target.emit(w)
		}
		io.WriteString(w, ")")
		return
	}
	io.WriteString(w, "(function(lst,s,e)\n  local r={}\n  for i=s+1,e do\n    r[#r+1]=lst[i]\n  end\n  return r\nend)(")
	sx.Target.emit(w)
	io.WriteString(w, ", ")
	if sx.Start != nil {
		sx.Start.emit(w)
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, ", ")
	if sx.End != nil {
		sx.End.emit(w)
	} else {
		io.WriteString(w, "#")
		sx.Target.emit(w)
	}
	io.WriteString(w, ")")
}

func (f *FunExpr) emit(w io.Writer) {
	io.WriteString(w, "function(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, p)
	}
	io.WriteString(w, ")\n")
	if f.Expr != nil {
		io.WriteString(w, "return ")
		f.Expr.emit(w)
		io.WriteString(w, "\n")
	}
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
}
func (id *Ident) emit(w io.Writer) { io.WriteString(w, id.Name) }

func isStringExpr(e Expr) bool {
	switch ex := e.(type) {
	case *StringLit:
		return true
	case *Ident:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	case *BinaryExpr:
		if ex.Op == ".." || ex.Op == "+" {
			return isStringExpr(ex.Left) || isStringExpr(ex.Right)
		}
	case *SliceExpr:
		if ex.Kind == "string" {
			return true
		}
	case *IndexExpr:
		if ex.Kind == "string" {
			return true
		}
	}
	return false
}

func isLuaIdent(s string) bool {
	if s == "" {
		return false
	}
	r := []rune(s)
	if !(unicode.IsLetter(r[0]) || r[0] == '_') {
		return false
	}
	for _, ch := range r[1:] {
		if !(unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_') {
			return false
		}
	}
	return true
}

func isIntExpr(e Expr) bool {
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *Ident:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				if _, ok := t.(types.IntType); ok {
					return true
				}
			}
		}
	case *BinaryExpr:
		switch ex.Op {
		case "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=":
			return isIntExpr(ex.Left) && isIntExpr(ex.Right)
		}
	}
	return false
}

func isBoolExpr(e Expr) bool {
	switch ex := e.(type) {
	case *BoolLit:
		return true
	case *Ident:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				if _, ok := t.(types.BoolType); ok {
					return true
				}
			}
		}
	case *BinaryExpr:
		switch ex.Op {
		case "&&", "||":
			return true
		case "==", "!=", "<", "<=", ">", ">=":
			return isIntExpr(ex.Left) && isIntExpr(ex.Right)
		}
	case *UnaryExpr:
		if ex.Op == "!" {
			return true
		}
	case *IfExpr:
		return isBoolExpr(ex.Then) && isBoolExpr(ex.Else)
	}
	return false
}

func isMapExpr(e Expr) bool {
	switch ex := e.(type) {
	case *MapLit:
		return true
	case *Ident:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	case *IndexExpr:
		if ex.Kind == "map" {
			if s, ok := ex.Index.(*StringLit); ok && s.Value == "items" {
				return false
			}
			return true
		}
	}
	return false
}

func isListExpr(e Expr) bool {
	switch ex := e.(type) {
	case *ListLit:
		return true
	case *Ident:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	case *SliceExpr:
		if ex.Kind == "list" {
			return true
		}
	case *IndexExpr:
		if ex.Kind == "list" {
			return true
		}
		if s, ok := ex.Index.(*StringLit); ok && s.Value == "items" {
			return true
		}
	case *CallExpr:
		if ex.Func == "values" {
			return true
		}
	case *BinaryExpr:
		switch ex.Op {
		case "union", "union_all", "except", "intersect":
			return true
		}
	}
	return false
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "in" {
		if isStringExpr(b.Right) {
			io.WriteString(w, "(string.find(")
			b.Right.emit(w)
			io.WriteString(w, ", ")
			b.Left.emit(w)
			io.WriteString(w, ", 1, true) ~= nil)")
			return
		}
		if isMapExpr(b.Right) {
			io.WriteString(w, "(")
			b.Right.emit(w)
			io.WriteString(w, "[")
			b.Left.emit(w)
			io.WriteString(w, "] ~= nil)")
			return
		}
		io.WriteString(w, "(function(lst,v) for _,x in ipairs(lst) do if x==v then return true end end return false end)(")
		b.Right.emit(w)
		io.WriteString(w, ", ")
		b.Left.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "union" {
		io.WriteString(w, "(function(a,b)\n  local seen={}\n  local res={}\n  for _,v in ipairs(a) do if not seen[v] then seen[v]=true res[#res+1]=v end end\n  for _,v in ipairs(b) do if not seen[v] then seen[v]=true res[#res+1]=v end end\n  return res\nend)(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "union_all" {
		io.WriteString(w, "(function(a,b)\n  local res={table.unpack(a)}\n  for _,v in ipairs(b) do res[#res+1]=v end\n  return res\nend)(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "except" {
		io.WriteString(w, "(function(a,b)\n  local m={}\n  for _,v in ipairs(b) do m[v]=true end\n  local res={}\n  for _,v in ipairs(a) do if not m[v] then res[#res+1]=v end end\n  return res\nend)(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if b.Op == "intersect" {
		io.WriteString(w, "(function(a,b)\n  local m={}\n  for _,v in ipairs(a) do m[v]=true end\n  local res={}\n  for _,v in ipairs(b) do if m[v] then res[#res+1]=v end end\n  return res\nend)(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	op := b.Op
	if op == "!=" {
		op = "~="
	}
	if op == "+" && (isStringExpr(b.Left) || isStringExpr(b.Right)) {
		op = ".."
	} else if op == "&&" {
		op = "and"
	} else if op == "||" {
		op = "or"
	} else if op == "/" {
		op = "//"
	}
	io.WriteString(w, "(")
	b.Left.emit(w)
	io.WriteString(w, " ")
	io.WriteString(w, op)
	io.WriteString(w, " ")
	b.Right.emit(w)
	io.WriteString(w, ")")
}

func (u *UnaryExpr) emit(w io.Writer) {
	switch u.Op {
	case "!":
		io.WriteString(w, "(not ")
		if u.Value != nil {
			u.Value.emit(w)
		}
		io.WriteString(w, ")")
	case "-":
		io.WriteString(w, "(-")
		if u.Value != nil {
			u.Value.emit(w)
		}
		io.WriteString(w, ")")
	}
}

func (m *MatchExpr) emit(w io.Writer) {
	io.WriteString(w, "(function(_m)\n")
	for i, a := range m.Arms {
		if i == 0 {
			io.WriteString(w, "  if ")
		} else {
			io.WriteString(w, "  elseif ")
		}
		if a.Pattern != nil {
			io.WriteString(w, "_m == ")
			a.Pattern.emit(w)
		} else {
			io.WriteString(w, "true")
		}
		io.WriteString(w, " then\n    return ")
		a.Result.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "  end\nend)(")
	m.Target.emit(w)
	io.WriteString(w, ")")
}

func (qc *QueryComp) emit(w io.Writer) {
	if qc.GroupKey != nil {
		io.WriteString(w, "(function()\n")
		io.WriteString(w, "local groups = {}\nlocal orderKeys = {}\n")
		if len(qc.Vars) > 0 {
			io.WriteString(w, "for _, ")
			io.WriteString(w, qc.Vars[0])
			io.WriteString(w, " in ipairs(")
			qc.Sources[0].emit(w)
			io.WriteString(w, ") do\n")
		}
		if qc.Where != nil {
			io.WriteString(w, "  if ")
			qc.Where.emit(w)
			io.WriteString(w, " then\n")
		}
		io.WriteString(w, "    local key = ")
		qc.GroupKey.emit(w)
		io.WriteString(w, "\n    local ks = tostring(key)\n    local g = groups[ks]\n    if g == nil then\n      g = {key = key, items = {}}\n      groups[ks] = g\n      table.insert(orderKeys, ks)\n    end\n    table.insert(g.items, ")
		io.WriteString(w, qc.Vars[0])
		io.WriteString(w, ")\n")
		if qc.Where != nil {
			io.WriteString(w, "  end\n")
		}
		if len(qc.Vars) > 0 {
			io.WriteString(w, "end\n")
		}
		if qc.SortKey != nil {
			io.WriteString(w, "local __tmp = {}\nlocal res = {}\n")
		} else {
			io.WriteString(w, "local res = {}\n")
		}
		io.WriteString(w, "for _, ks in ipairs(orderKeys) do\n  local g = groups[ks]\n")
		if qc.Having != nil {
			io.WriteString(w, "  if ")
			qc.Having.emit(w)
			io.WriteString(w, " then\n")
		}
		if qc.SortKey != nil {
			io.WriteString(w, "  table.insert(__tmp, {k = ")
			qc.SortKey.emit(w)
			io.WriteString(w, ", v = ")
		} else {
			io.WriteString(w, "  table.insert(res, ")
		}
		if qc.Body != nil {
			qc.Body.emit(w)
		} else {
			io.WriteString(w, "nil")
		}
		if qc.SortKey != nil {
			io.WriteString(w, "})\n")
		} else {
			io.WriteString(w, ")\n")
		}
		if qc.Having != nil {
			io.WriteString(w, "  end\n")
		}
		io.WriteString(w, "end\n")
		if qc.SortKey != nil {
			if ml, ok := qc.SortKey.(*MapLit); ok {
				io.WriteString(w, "table.sort(__tmp, function(a,b)\n")
				for _, k := range ml.Keys {
					if s, ok := k.(*StringLit); ok {
						field := s.Value
						io.WriteString(w, "  if a.k."+field+" ~= b.k."+field+" then return a.k."+field+" < b.k."+field+" end\n")
					}
				}
				io.WriteString(w, "  return false\nend)\n")
			} else {
				io.WriteString(w, "table.sort(__tmp, function(a,b) return a.k < b.k end)\n")
			}
			io.WriteString(w, "for i,p in ipairs(__tmp) do res[i] = p.v end\n")
		}
		if qc.Skip != nil || qc.Take != nil {
			io.WriteString(w, "local _slice = {}\nlocal _start = 1")
			if qc.Skip != nil {
				io.WriteString(w, " + (")
				qc.Skip.emit(w)
				io.WriteString(w, ")")
			}
			io.WriteString(w, "\nlocal _stop = #res")
			if qc.Take != nil {
				io.WriteString(w, "\nif (")
				qc.Take.emit(w)
				io.WriteString(w, ") < _stop - _start + 1 then _stop = _start + (")
				qc.Take.emit(w)
				io.WriteString(w, ") - 1 end")
			}
			io.WriteString(w, "\nfor i=_start,_stop do _slice[#_slice+1] = res[i] end\nres = _slice\n")
		}
		io.WriteString(w, "return res\nend)()")
		return
	}
	if qc.Agg != "" && qc.GroupKey == nil {
		io.WriteString(w, "(function()\n  local _tmp = {}\n")
	} else if qc.SortKey != nil {
		io.WriteString(w, "(function()\n  local __tmp = {}\n  local _res = {}\n")
	} else {
		io.WriteString(w, "(function()\n  local _res = {}\n")
	}
	order := make([]int, len(qc.Vars))
	idx := 0
	for i := range qc.Vars {
		if i < len(qc.Sides) && qc.Sides[i] == "right" {
			order[idx] = i
			idx++
		}
	}
	for i := range qc.Vars {
		if i >= len(qc.Sides) || qc.Sides[i] != "right" {
			order[idx] = i
			idx++
		}
	}
	for _, i := range order {
		v := qc.Vars[i]
		io.WriteString(w, "  for _, ")
		io.WriteString(w, v)
		io.WriteString(w, " in ipairs(")
		qc.Sources[i].emit(w)
		io.WriteString(w, ") do\n")
	}
	if qc.Where != nil {
		io.WriteString(w, "    if ")
		qc.Where.emit(w)
		io.WriteString(w, " then\n")
	}
	io.WriteString(w, "    ")
	if qc.Agg != "" && qc.GroupKey == nil {
		io.WriteString(w, "table.insert(_tmp, ")
	} else if qc.SortKey != nil {
		io.WriteString(w, "table.insert(__tmp, {k = ")
		qc.SortKey.emit(w)
		io.WriteString(w, ", v = ")
	} else {
		io.WriteString(w, "table.insert(_res, ")
	}
	if qc.Body != nil {
		qc.Body.emit(w)
	} else {
		io.WriteString(w, "nil")
	}
	if qc.Agg != "" && qc.GroupKey == nil {
		io.WriteString(w, ")\n")
	} else if qc.SortKey != nil {
		io.WriteString(w, "})\n")
	} else {
		io.WriteString(w, ")\n")
	}
	if qc.Where != nil {
		io.WriteString(w, "    end\n")
	}
	for range qc.Vars {
		io.WriteString(w, "  end\n")
	}
	if qc.Agg != "" && qc.GroupKey == nil {
		io.WriteString(w, "  return ")
		(&CallExpr{Func: qc.Agg, Args: []Expr{&Ident{Name: "_tmp"}}}).emit(w)
		io.WriteString(w, "\nend)()")
	} else if qc.SortKey != nil {
		if ml, ok := qc.SortKey.(*MapLit); ok {
			io.WriteString(w, "  table.sort(__tmp, function(a,b)\n")
			for _, k := range ml.Keys {
				if s, ok := k.(*StringLit); ok {
					field := s.Value
					io.WriteString(w, "    if a.k."+field+" ~= b.k."+field+" then return a.k."+field+" < b.k."+field+" end\n")
				}
			}
			io.WriteString(w, "    return false\n  end)\n")
		} else {
			io.WriteString(w, "  table.sort(__tmp, function(a,b) return a.k < b.k end)\n")
		}
		io.WriteString(w, "  for i,p in ipairs(__tmp) do _res[i] = p.v end\n")
		if qc.Skip != nil || qc.Take != nil {
			io.WriteString(w, "  local _slice = {}\n  local _start = 1")
			if qc.Skip != nil {
				io.WriteString(w, " + (")
				qc.Skip.emit(w)
				io.WriteString(w, ")")
			}
			io.WriteString(w, "\n  local _stop = #_res")
			if qc.Take != nil {
				io.WriteString(w, "\n  if (")
				qc.Take.emit(w)
				io.WriteString(w, ") < _stop - _start + 1 then _stop = _start + (")
				qc.Take.emit(w)
				io.WriteString(w, ") - 1 end")
			}
			io.WriteString(w, "\n  for i=_start,_stop do _slice[#_slice+1] = _res[i] end\n  _res = _slice\n")
		}
		io.WriteString(w, "  return _res\nend)()")
	} else {
		if qc.Skip != nil || qc.Take != nil {
			io.WriteString(w, "  local _slice = {}\n  local _start = 1")
			if qc.Skip != nil {
				io.WriteString(w, " + (")
				qc.Skip.emit(w)
				io.WriteString(w, ")")
			}
			io.WriteString(w, "\n  local _stop = #_res")
			if qc.Take != nil {
				io.WriteString(w, "\n  if (")
				qc.Take.emit(w)
				io.WriteString(w, ") < _stop - _start + 1 then _stop = _start + (")
				qc.Take.emit(w)
				io.WriteString(w, ") - 1 end")
			}
			io.WriteString(w, "\n  for i=_start,_stop do _slice[#_slice+1] = _res[i] end\n  _res = _slice\n")
		}
		io.WriteString(w, "  return _res\nend)()")
	}
}

func newContinueLabel() string {
	loopCounter++
	lbl := fmt.Sprintf("__cont_%d", loopCounter)
	return lbl
}

func hasContinue(stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *ContinueStmt:
			return true
		case *ForRangeStmt:
			if hasContinue(s.Body) {
				return true
			}
		case *ForInStmt:
			if hasContinue(s.Body) {
				return true
			}
		case *WhileStmt:
			if hasContinue(s.Body) {
				return true
			}
		case *IfStmt:
			if hasContinue(s.Then) || hasContinue(s.Else) {
				return true
			}
		case *FunStmt:
			if hasContinue(s.Body) {
				return true
			}
		}
	}
	return false
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

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func gitTime() time.Time {
	root := repoRoot()
	if root == "" {
		return time.Now()
	}
	out, err := exec.Command("git", "-C", root, "log", "-1", "--format=%cI").Output()
	if err != nil {
		return time.Now()
	}
	t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out)))
	if err != nil {
		return time.Now()
	}
	return t
}

func header() string {
	t := gitTime().In(time.FixedZone("GMT+7", 7*3600))
	return fmt.Sprintf("-- Generated by Mochi v%s on %s\n", version(), t.Format("2006-01-02 15:04 MST"))
}

func formatLua(b []byte) []byte {
	lines := strings.Split(strings.TrimSpace(string(b)), "\n")
	indent := 0
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		lower := strings.TrimSpace(trimmed)
		if strings.HasPrefix(lower, "end") || strings.HasPrefix(lower, "else") || strings.HasPrefix(lower, "elseif") {
			indent--
			if indent < 0 {
				indent = 0
			}
		}
		lines[i] = strings.Repeat("  ", indent) + trimmed
		if strings.HasSuffix(trimmed, "then") || strings.HasSuffix(trimmed, "do") || strings.HasPrefix(trimmed, "function") {
			indent++
		}
		if lower == "else" || strings.HasPrefix(lower, "elseif") {
			indent++
		}
	}
	out := strings.Join(lines, "\n")
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}

// Emit converts the AST back into Lua source code with a standard header.
func Emit(p *Program) []byte {
	var b bytes.Buffer
	b.WriteString(header())
	prevEnv := currentEnv
	currentEnv = p.Env
	for i, st := range p.Stmts {
		if i > 0 {
			b.WriteByte('\n')
		}
		st.emit(&b)
		b.WriteByte('\n')
	}
	currentEnv = prevEnv
	return formatLua(b.Bytes())
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
	left, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	exprs := []Expr{left}
	ops := []string{}
	for _, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		if op.Op == "in" {
			// handled during emission
		}
		exprs = append(exprs, right)
		if op.Op == "union" && op.All {
			ops = append(ops, "union_all")
		} else {
			ops = append(ops, op.Op)
		}
	}
	expr, err := buildPrecedence(exprs, ops)
	if err != nil {
		return nil, err
	}
	return expr, nil
}

func buildPrecedence(exprs []Expr, ops []string) (Expr, error) {
	// handle *, / first
	prec := func(op string) int {
		switch op {
		case "*", "/", "%":
			return 2
		case "+", "-":
			return 1
		case "union", "union_all", "except", "intersect":
			return -1
		default:
			return 0
		}
	}
	for {
		idx := -1
		for i, op := range ops {
			if prec(op) == 2 {
				idx = i
				break
			}
		}
		if idx == -1 {
			break
		}
		expr := &BinaryExpr{Left: exprs[idx], Op: ops[idx], Right: exprs[idx+1]}
		exprs = append(exprs[:idx], append([]Expr{expr}, exprs[idx+2:]...)...)
		ops = append(ops[:idx], ops[idx+1:]...)
	}
	for len(ops) > 0 {
		expr := &BinaryExpr{Left: exprs[0], Op: ops[0], Right: exprs[1]}
		exprs = append([]Expr{expr}, exprs[2:]...)
		ops = ops[1:]
	}
	if len(exprs) != 1 {
		return nil, fmt.Errorf("invalid expression")
	}
	return exprs[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
		case "!":
			expr = &IfExpr{Cond: expr, Then: &IntLit{Value: 0}, Else: &IntLit{Value: 1}}
		default:
			return nil, fmt.Errorf("unsupported unary operator")
		}
	}
	return expr, nil
}

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	var ops []*parser.PostfixOp
	expr, err := convertPrimary(p.Target)
	if sel := p.Target.Selector; sel != nil && len(sel.Tail) == 1 {
		expr = &Ident{Name: sel.Root}
		ops = append([]*parser.PostfixOp{{Field: &parser.FieldOp{Name: sel.Tail[0]}}}, p.Ops...)
	} else if err != nil {
		return nil, err
	} else {
		ops = p.Ops
	}
	for i := 0; i < len(ops); i++ {
		op := ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			kind := "list"
			if isStringExpr(expr) {
				kind = "string"
			} else if isMapExpr(expr) {
				kind = "map"
			}
			expr = &IndexExpr{Target: expr, Index: idx, Kind: kind}
		case op.Index != nil:
			var start, end Expr
			var err error
			if op.Index.Start != nil {
				start, err = convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
			}
			if op.Index.End != nil {
				end, err = convertExpr(op.Index.End)
				if err != nil {
					return nil, err
				}
			}
			kind := "list"
			if isStringExpr(expr) {
				kind = "string"
			}
			expr = &SliceExpr{Target: expr, Start: start, End: end, Kind: kind}
		case op.Field != nil:
			if i+1 < len(ops) && ops[i+1].Call != nil {
				call := ops[i+1].Call
				i++
				var args []Expr
				args = append(args, expr)
				for _, a := range call.Args {
					ae, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ae)
				}
				// contains handled during emission
				expr = &CallExpr{Func: op.Field.Name, Args: args}
			} else {
				expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}, Kind: "map"}
			}
		case op.Call != nil:
			var args []Expr
			for _, a := range op.Call.Args {
				ae, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ae)
			}
			if id, ok := expr.(*Ident); ok {
				expr = &CallExpr{Func: id.Name, Args: args}
			} else {
				return nil, fmt.Errorf("unsupported call")
			}
		case op.Cast != nil:
			// ignore cast
		default:
			return nil, fmt.Errorf("postfix ops not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Call != nil:
		ce := &CallExpr{Func: p.Call.Func}
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ae)
		}
		switch p.Call.Func {
		case "append", "avg", "sum", "contains", "len", "count":
			// handled during emission
			return ce, nil
		}
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(p.Call.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if len(p.Call.Args) < len(ft.Params) {
						params := []string{}
						for i := len(p.Call.Args); i < len(ft.Params); i++ {
							params = append(params, fmt.Sprintf("p%d", i-len(p.Call.Args)))
						}
						args := append([]Expr{}, ce.Args...)
						for _, p := range params {
							args = append(args, &Ident{Name: p})
						}
						body := []Stmt{&ReturnStmt{Value: &CallExpr{Func: p.Call.Func, Args: args}}}
						return &FunExpr{Params: params, Body: body}, nil
					}
				}
			}
		}
		return ce, nil
	case p.List != nil:
		var elems []Expr
		for _, e := range p.List.Elems {
			ce, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ce)
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		var keys []Expr
		var values []Expr
		for _, it := range p.Map.Items {
			ke, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			if id, ok := ke.(*Ident); ok {
				ke = &StringLit{Value: id.Name}
			}
			ve, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			keys = append(keys, ke)
			values = append(values, ve)
		}
		return &MapLit{Keys: keys, Values: values}, nil
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.FunExpr != nil:
		fe := &FunExpr{}
		for _, pa := range p.FunExpr.Params {
			fe.Params = append(fe.Params, pa.Name)
		}
		if p.FunExpr.ExprBody != nil {
			expr, err := convertExpr(p.FunExpr.ExprBody)
			if err != nil {
				return nil, err
			}
			fe.Expr = expr
		}
		for _, st := range p.FunExpr.BlockBody {
			s, err := convertStmt(st)
			if err != nil {
				return nil, err
			}
			fe.Body = append(fe.Body, s)
		}
		return fe, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Selector != nil:
		expr := Expr(&Ident{Name: p.Selector.Root})
		for _, name := range p.Selector.Tail {
			expr = &IndexExpr{Target: expr, Index: &StringLit{Value: name}, Kind: "map"}
		}
		return expr, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	case p.If != nil:
		return convertIfExpr(p.If)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: int(*l.Int)}, nil
	case l.Float != nil:
		return &FloatLit{Value: *l.Float}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	default:
		return nil, fmt.Errorf("unsupported literal")
	}
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
		elseExpr = &Ident{Name: "nil"}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	arms := make([]MatchArm, len(me.Cases))
	for i, c := range me.Cases {
		pat, err := convertExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		if id, ok := pat.(*Ident); ok && id.Name == "_" {
			pat = nil
		}
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		arms[i] = MatchArm{Pattern: pat, Result: res}
	}
	return &MatchExpr{Target: target, Arms: arms}, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	vars := []string{q.Var}
	sources := []Expr{}
	sides := []string{""}
	first, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if id, ok := first.(*Ident); ok && currentEnv != nil {
		if t, err := currentEnv.GetVar(id.Name); err == nil {
			if _, ok := t.(types.GroupType); ok {
				first = &IndexExpr{Target: first, Index: &StringLit{Value: "items"}, Kind: "map"}
			}
		}
	}
	sources = append(sources, first)
	for _, fc := range q.Froms {
		expr, err := convertExpr(fc.Src)
		if err != nil {
			return nil, err
		}
		if id, ok := expr.(*Ident); ok && currentEnv != nil {
			if t, err := currentEnv.GetVar(id.Name); err == nil {
				if _, ok := t.(types.GroupType); ok {
					expr = &IndexExpr{Target: expr, Index: &StringLit{Value: "items"}, Kind: "map"}
				}
			}
		}
		vars = append(vars, fc.Var)
		sources = append(sources, expr)
		sides = append(sides, "")
	}

	var where Expr
	for _, jc := range q.Joins {
		expr, err := convertExpr(jc.Src)
		if err != nil {
			return nil, err
		}
		if id, ok := expr.(*Ident); ok && currentEnv != nil {
			if t, err := currentEnv.GetVar(id.Name); err == nil {
				if _, ok := t.(types.GroupType); ok {
					expr = &IndexExpr{Target: expr, Index: &StringLit{Value: "items"}, Kind: "map"}
				}
			}
		}
		vars = append(vars, jc.Var)
		sources = append(sources, expr)
		side := ""
		if jc.Side != nil {
			side = *jc.Side
		}
		sides = append(sides, side)
		cond, err := convertExpr(jc.On)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}

	if q.Where != nil {
		cond, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		if where == nil {
			where = cond
		} else {
			where = &BinaryExpr{Left: where, Op: "&&", Right: cond}
		}
	}

	var groupExpr Expr
	var groupVar string
	var having Expr
	if q.Group != nil {
		if len(q.Group.Exprs) != 1 {
			return nil, fmt.Errorf("unsupported query")
		}
		var err error
		groupExpr, err = convertExpr(q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		groupVar = q.Group.Name
		if q.Group.Having != nil {
			having, err = convertExpr(q.Group.Having)
			if err != nil {
				return nil, err
			}
		}
	}

	env := currentEnv
	if groupVar != "" {
		env = types.NewEnv(currentEnv)
		env.SetVar(groupVar, types.GroupType{}, false)
	}
	prevEnv := currentEnv
	currentEnv = env
	body, err := convertExpr(q.Select)
	if err != nil {
		return nil, err
	}
	var sortKey Expr
	if q.Sort != nil {
		sortKey, err = convertExpr(q.Sort)
		if err != nil {
			return nil, err
		}
	}
	agg := ""
	if call, ok := body.(*CallExpr); ok && groupExpr == nil {
		switch call.Func {
		case "sum", "count", "avg", "min", "max":
			if len(call.Args) == 1 {
				agg = call.Func
				body = call.Args[0]
			}
		}
	}
	currentEnv = prevEnv
	var skipExpr, takeExpr Expr
	if q.Skip != nil {
		skipExpr, err = convertExpr(q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(q.Take)
		if err != nil {
			return nil, err
		}
	}
	return &QueryComp{Vars: vars, Sources: sources, Sides: sides, Body: body, Where: where, GroupKey: groupExpr, GroupVar: groupVar, Having: having, Agg: agg, SortKey: sortKey, Skip: skipExpr, Take: takeExpr}, nil
}

func convertIfStmt(is *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, st := range is.Then {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		thenStmts = append(thenStmts, s)
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		s, err := convertIfStmt(is.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(is.Else) > 0 {
		for _, st := range is.Else {
			s, err := convertStmt(st)
			if err != nil {
				return nil, err
			}
			elseStmts = append(elseStmts, s)
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertReturnStmt(rs *parser.ReturnStmt) (Stmt, error) {
	var expr Expr
	var err error
	if rs.Value != nil {
		expr, err = convertExpr(rs.Value)
		if err != nil {
			return nil, err
		}
	}
	return &ReturnStmt{Value: expr}, nil
}

func convertWhileStmt(ws *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range ws.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fs *parser.ForStmt) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, st := range fs.Body {
			s, err := convertStmt(st)
			if err != nil {
				return nil, err
			}
			body = append(body, s)
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fs.Source)
	if err != nil {
		return nil, err
	}
	var body []Stmt
	for _, st := range fs.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		body = append(body, s)
	}
	return &ForInStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertFunStmt(fs *parser.FunStmt) (Stmt, error) {
	f := &FunStmt{Name: fs.Name}
	for _, p := range fs.Params {
		f.Params = append(f.Params, p.Name)
	}
	for _, st := range fs.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		f.Body = append(f.Body, s)
	}
	return f, nil
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Type != nil:
		return nil, nil
	case st.ExternType != nil:
		return nil, nil
	case st.Expr != nil:
		expr, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: expr}, nil
	case st.Let != nil:
		var expr Expr
		var err error
		if st.Let.Value != nil {
			expr, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil {
			switch *st.Let.Type.Simple {
			case "int":
				expr = &IntLit{Value: 0}
			case "string":
				expr = &StringLit{Value: ""}
			case "bool":
				expr = &BoolLit{Value: false}
			}
		}
		if qc, ok := expr.(*QueryComp); ok {
			return &QueryAssignStmt{Name: st.Let.Name, Query: qc}, nil
		}
		return &AssignStmt{Name: st.Let.Name, Value: expr}, nil
	case st.Var != nil:
		var expr Expr
		var err error
		if st.Var.Value != nil {
			expr, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil {
			switch *st.Var.Type.Simple {
			case "int":
				expr = &IntLit{Value: 0}
			case "string":
				expr = &StringLit{Value: ""}
			case "bool":
				expr = &BoolLit{Value: false}
			}
		}
		if qc, ok := expr.(*QueryComp); ok {
			return &QueryAssignStmt{Name: st.Var.Name, Query: qc}, nil
		}
		return &AssignStmt{Name: st.Var.Name, Value: expr}, nil
	case st.Assign != nil:
		expr, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(st.Assign.Index) > 0 {
			target := Expr(&Ident{Name: st.Assign.Name})
			for _, idx := range st.Assign.Index {
				iexpr, err := convertExpr(idx.Start)
				if err != nil {
					return nil, err
				}
				kind := "list"
				if isStringExpr(target) {
					kind = "string"
				} else if isMapExpr(target) {
					kind = "map"
				}
				target = &IndexExpr{Target: target, Index: iexpr, Kind: kind}
			}
			return &IndexAssignStmt{Target: target, Value: expr}, nil
		}
		if len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("unsupported assignment")
		}
		if qc, ok := expr.(*QueryComp); ok {
			return &QueryAssignStmt{Name: st.Assign.Name, Query: qc}, nil
		}
		return &AssignStmt{Name: st.Assign.Name, Value: expr}, nil
	case st.Fun != nil:
		return convertFunStmt(st.Fun)
	case st.Return != nil:
		return convertReturnStmt(st.Return)
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Test != nil:
		for _, s := range st.Test.Body {
			if s.Expect != nil {
				// ignore expectations
				continue
			}
			if _, err := convertStmt(s); err != nil {
				return nil, err
			}
		}
		return nil, nil
	case st.Expect != nil:
		return nil, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

// Transpile converts a Mochi AST into a simple Lua AST supporting variable
// declarations, assignments, `if` statements and expressions, and calls like
// `print`. Expressions handle unary negation, arithmetic, comparison and basic
// boolean operators.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	currentEnv = env
	lp := &Program{Env: env}
	for _, st := range prog.Statements {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			lp.Stmts = append(lp.Stmts, s)
		}
	}
	currentEnv = nil
	return lp, nil
}

// Print renders a tree representation of the Lua AST to stdout. It is
// useful for debugging and tests.
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
	case *AssignStmt:
		var child *ast.Node
		if st.Value != nil {
			child = exprNode(st.Value)
		}
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{child}}
	case *QueryAssignStmt:
		n := &ast.Node{Kind: "assign", Value: st.Name}
		q := exprNode(st.Query)
		n.Children = append(n.Children, q)
		return n
	case *IndexAssignStmt:
		n := &ast.Node{Kind: "index_assign"}
		n.Children = append(n.Children, exprNode(st.Target), exprNode(st.Value))
		return n
	case *FunStmt:
		n := &ast.Node{Kind: "fun", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params)
		body := &ast.Node{Kind: "body"}
		for _, s2 := range st.Body {
			body.Children = append(body.Children, stmtNode(s2))
		}
		n.Children = append(n.Children, body)
		return n
	case *ReturnStmt:
		var child *ast.Node
		if st.Value != nil {
			child = exprNode(st.Value)
		}
		return &ast.Node{Kind: "return", Children: []*ast.Node{child}}
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
		{
			n := &ast.Node{Kind: "for_range", Value: st.Name}
			n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
			body := &ast.Node{Kind: "body"}
			for _, s2 := range st.Body {
				body.Children = append(body.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, body)
			return n
		}
	case *ForInStmt:
		{
			n := &ast.Node{Kind: "for_in", Value: st.Name}
			n.Children = append(n.Children, exprNode(st.Iterable))
			body := &ast.Node{Kind: "body"}
			for _, s2 := range st.Body {
				body.Children = append(body.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, body)
			return n
		}
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	if e == nil {
		return &ast.Node{Kind: "nil"}
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
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprintf("%d", ex.Value)}
	case *Ident:
		return &ast.Node{Kind: "ident", Value: ex.Name}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e2 := range ex.Elems {
			n.Children = append(n.Children, exprNode(e2))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for i := range ex.Keys {
			pair := &ast.Node{Kind: "pair"}
			pair.Children = append(pair.Children, exprNode(ex.Keys[i]), exprNode(ex.Values[i]))
			n.Children = append(n.Children, pair)
		}
		return n
	case *IndexExpr:
		n := &ast.Node{Kind: "index"}
		n.Children = append(n.Children, exprNode(ex.Target), exprNode(ex.Index))
		return n
	case *SliceExpr:
		n := &ast.Node{Kind: "slice"}
		n.Children = append(n.Children, exprNode(ex.Target))
		if ex.Start != nil {
			n.Children = append(n.Children, exprNode(ex.Start))
		} else {
			n.Children = append(n.Children, &ast.Node{Kind: "nil"})
		}
		if ex.End != nil {
			n.Children = append(n.Children, exprNode(ex.End))
		} else {
			n.Children = append(n.Children, &ast.Node{Kind: "nil"})
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *IfExpr:
		n := &ast.Node{Kind: "cond"}
		n.Children = append(n.Children, exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else))
		return n
	case *MatchExpr:
		n := &ast.Node{Kind: "match"}
		n.Children = append(n.Children, exprNode(ex.Target))
		for _, a := range ex.Arms {
			arm := &ast.Node{Kind: "case"}
			if a.Pattern != nil {
				arm.Children = append(arm.Children, exprNode(a.Pattern))
			} else {
				arm.Children = append(arm.Children, &ast.Node{Kind: "wild"})
			}
			arm.Children = append(arm.Children, exprNode(a.Result))
			n.Children = append(n.Children, arm)
		}
		return n
	case *QueryComp:
		n := &ast.Node{Kind: "query"}
		for i, v := range ex.Vars {
			clause := &ast.Node{Kind: "from", Value: v}
			clause.Children = append(clause.Children, exprNode(ex.Sources[i]))
			if i < len(ex.Sides) && ex.Sides[i] != "" {
				clause.Children = append(clause.Children, &ast.Node{Kind: ex.Sides[i]})
			}
			n.Children = append(n.Children, clause)
		}
		if ex.Where != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprNode(ex.Where)}})
		}
		n.Children = append(n.Children, exprNode(ex.Body))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
