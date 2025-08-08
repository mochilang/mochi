//go:build slow

package lua

import (
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
	"unicode"

	"gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

var currentEnv *types.Env
var loopCounter int
var continueLabels []string
var structCount int
var funcDepth int
var benchMainFlag bool
var currentStruct string

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code. When enabled, programs print a JSON
// object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMainFlag = v }

const helperNow = `
local _now_seed = 0
local _now_seeded = false
do
  local s = os.getenv("MOCHI_NOW_SEED")
  if s and s ~= "" then
    local v = tonumber(s)
    if v then
      _now_seed = v
      _now_seeded = true
    end
  end
end
local function _now()
  if _now_seeded then
    -- keep the seed within safe integer range for Lua (53 bits)
    _now_seed = (_now_seed * 1664525 + 1013904223) % 9007199254740991
    return _now_seed % 1000000000
  end
  return os.time() * 1000000000 + math.floor(os.clock() * 1000000000)
end
`

const helperPadStart = `
local function _padStart(s, len, ch)
  if ch == nil or ch == '' then ch = ' ' end
  if #s >= len then return s end
  local fill = string.sub(ch, 1, 1)
  return string.rep(fill, len - #s) .. s
end
`

const helperBigRat = `
local function _gcd(a, b)
  a = math.abs(a)
  b = math.abs(b)
  while b ~= 0 do
    a, b = b, a % b
  end
  return a
end
local function _bigrat(n, d)
  if type(n) == 'table' and n.num ~= nil and n.den ~= nil and d == nil then
    return n
  end
  if d == nil then d = 1 end
  if d < 0 then n, d = -n, -d end
  local g = _gcd(n, d)
  return {num = n // g, den = d // g}
end
local function _add(a, b)
  return _bigrat(a.num * b.den + b.num * a.den, a.den * b.den)
end
local function _sub(a, b)
  return _bigrat(a.num * b.den - b.num * a.den, a.den * b.den)
end
local function _mul(a, b)
  return _bigrat(a.num * b.num, a.den * b.den)
end
local function _div(a, b)
  return _bigrat(a.num * b.den, a.den * b.num)
end
function num(x)
  if type(x) == 'table' and x.num ~= nil then return x.num end
  return x
end
function denom(x)
  if type(x) == 'table' and x.den ~= nil then return x.den end
  return 1
end
`

const helperSHA256 = `
local function _sha256(bs)
  local tmp = os.tmpname()
  local f = assert(io.open(tmp, 'wb'))
  for i = 1, #bs do
    f:write(string.char(bs[i]))
  end
  f:close()
  local p = io.popen('sha256sum ' .. tmp)
  local out = p:read('*l') or ''
  p:close()
  os.remove(tmp)
  local hex = string.sub(out, 1, 64)
  local res = {}
  for i = 1, #hex, 2 do
    res[#res+1] = tonumber(string.sub(hex, i, i+1), 16)
  end
  return res
end
`

const helperMD5 = `
local function _md5(bs)
  local tmp = os.tmpname()
  local f = assert(io.open(tmp, 'wb'))
  for i = 1, #bs do
    f:write(string.char(bs[i]))
  end
  f:close()
  local p = io.popen('md5sum ' .. tmp)
  local out = p:read('*l') or ''
  p:close()
  os.remove(tmp)
  local hex = string.sub(out, 1, 32)
  local res = {}
  for i = 1, #hex, 2 do
    res[#res+1] = tonumber(string.sub(hex, i, i+1), 16)
  end
  return res
end
`

const helperGetOutput = `
local function _getoutput(cmd)
  local p = io.popen(cmd)
  if not p then return '' end
  local out = p:read('*a') or ''
  p:close()
  -- trim trailing newlines from different platforms
  out = string.gsub(out, '\r?\n$', '')
  return out
end
`

const helperEnviron = `
local function _environ()
  local p = io.popen('env')
  local t = {}
  if p then
    for line in p:lines() do
      t[#t+1] = line
    end
    p:close()
  end
  return t
end
`

const helperPanic = `
local function _panic(msg)
  io.stderr:write(tostring(msg))
  os.exit(1)
end
`

const helperIndexOf = `
local function _indexOf(s, ch)
  if type(s) == 'string' then
    for i = 1, #s do
      if string.sub(s, i, i) == ch then
        return i - 1
      end
    end
  elseif type(s) == 'table' then
    for i, v in ipairs(s) do
      if v == ch then
        return i - 1
      end
    end
  end
  return -1
end
`

const helperExists = `
local function _exists(v)
  if type(v) == 'table' then
    if v.items ~= nil then return #v.items > 0 end
    if v[1] ~= nil or #v > 0 then return #v > 0 end
    return next(v) ~= nil
  elseif type(v) == 'string' then
    return #v > 0
  else
    return false
  end
end
`

const helperEqual = `
local function _equal(a, b)
  if a == b then return true end
  if type(a) ~= type(b) then return false end
  if type(a) ~= 'table' then return a == b end
  local count = 0
  for k, v in pairs(a) do
    if not _equal(v, b[k]) then return false end
    count = count + 1
  end
  for k in pairs(b) do
    count = count - 1
  end
  return count == 0
end
`

const helperStr = `
local function _str(v)
  if type(v) == 'number' then
    local s = tostring(v)
    s = string.gsub(s, '%.0+$', '')
    return s
  end
  return tostring(v)
end
`

const helperParseIntStr = `
local function _parseIntStr(str)
  if type(str) == 'table' then
    str = table.concat(str)
  end
  local n = tonumber(str, 10)
  if n == nil then return 0 end
  return math.floor(n)
end
`

const helperFetch = `
local function _fetch(url)
  local p = io.popen('curl -fsSL ' .. url)
  if not p then return '' end
  local out = p:read('*a') or ''
  p:close()
  local title = string.match(out, '"title"%s*:%s*"([^"]+)"')
  if title then return {title = title} end
  return out
end
`

const helperSlice = `
local function slice(lst, s, e)
  if s < 0 then s = #lst + s end
  if e == nil then
    e = #lst
  elseif e < 0 then
    e = #lst + e
  end
  local r = {}
  for i = s + 1, e do
    r[#r+1] = lst[i]
  end
  return r
end
`

const helperSplit = `
local function _split(s, sep)
  local t = {}
  local pattern = string.format("([^%s]+)", sep)
  string.gsub(s, pattern, function(c) t[#t+1] = c end)
  return t
end
`

const helperSubstring = `
local function _substring(s, i, j)
  i = i + 1
  if j == nil then j = #s end
  local si = utf8.offset(s, i)
  if not si then return '' end
  local sj = utf8.offset(s, j+1)
  if not sj then sj = -1 end
  return string.sub(s, si, sj-1)
end
`

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
	Local bool
}
type QueryAssignStmt struct {
	Name  string
	Query *QueryComp
	Local bool
}
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

// SaveStmt saves a list of maps to stdout in JSONL format.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}
type FunStmt struct {
	Name   string
	Params []string
	Body   []Stmt
	Env    *types.Env
	Local  bool
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

// MultiStmt represents a sequence of statements emitted consecutively.
type MultiStmt struct{ Stmts []Stmt }

func (m *MultiStmt) emit(w io.Writer) {
	for i, st := range m.Stmts {
		if i > 0 {
			io.WriteString(w, "\n")
		}
		st.emit(w)
	}
}

type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

// BenchStmt represents a benchmarking block that records execution time
// and memory usage of the enclosed statements.
type BenchStmt struct {
	Name string
	Body []Stmt
}

type BreakStmt struct{}

type ContinueStmt struct{}

type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

type CallExpr struct {
	Func       string
	Args       []Expr
	ParamTypes []types.Type
}

type DynCallExpr struct {
	Fn   Expr
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
	Env    *types.Env
}
type ListCastExpr struct{ Expr Expr }
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

type UnaryExpr struct {
	Op    string
	Value Expr
}

func (dc *DynCallExpr) emit(w io.Writer) {
	if ix, ok := dc.Fn.(*IndexExpr); ok {
		if id, ok2 := ix.Target.(*Ident); ok2 {
			if s, ok3 := ix.Index.(*StringLit); ok3 {
				io.WriteString(w, sanitizeName(s.Value))
				io.WriteString(w, "(")
				id.emit(w)
				for _, a := range dc.Args {
					io.WriteString(w, ", ")
					a.emit(w)
				}
				io.WriteString(w, ")")
				return
			}
		}
		if s, ok2 := ix.Index.(*StringLit); ok2 && s.Value == "slice" {
			io.WriteString(w, "slice(")
			ix.Target.emit(w)
			for _, a := range dc.Args {
				io.WriteString(w, ", ")
				a.emit(w)
			}
			io.WriteString(w, ")")
			return
		}
	}
	dc.Fn.emit(w)
	io.WriteString(w, "(")
	for i, a := range dc.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

type MatchArm struct {
	Pattern Expr // nil means wildcard
	Variant string
	Vars    []string
	Fields  []string
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

type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
}

func (s *ExprStmt) emit(w io.Writer) {
	switch s.Expr.(type) {
	case *CallExpr, *DynCallExpr:
		s.Expr.emit(w)
	default:
		io.WriteString(w, "do local _ = ")
		s.Expr.emit(w)
		io.WriteString(w, " end")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	if strings.Contains(c.Func, ".") {
		parts := strings.SplitN(c.Func, ".", 2)
		if len(parts) == 2 {
			if currentEnv != nil {
				if _, err := currentEnv.GetVar(parts[1]); err == nil {
					io.WriteString(w, sanitizeName(parts[1]))
					io.WriteString(w, "(")
					io.WriteString(w, sanitizeName(parts[0]))
					for _, a := range c.Args {
						io.WriteString(w, ", ")
						a.emit(w)
					}
					io.WriteString(w, ")")
					return
				}
			}
		}
	}
	switch c.Func {
	case "print":
		if len(c.Args) == 1 && (isListExpr(c.Args[0]) || isMapExpr(c.Args[0])) {
			io.WriteString(w, "print(")
			(&CallExpr{Func: "repr", Args: c.Args}).emit(w)
			io.WriteString(w, ")")
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
			} else if isListExpr(a) || isMapExpr(a) {
				fmtBuf.WriteString("%s")
				exprs = append(exprs, &CallExpr{Func: "repr", Args: []Expr{a}})
			} else if isIntExpr(a) || isBoolExpr(a) {
				fmtBuf.WriteString("%d")
				if isBoolExpr(a) && !isIntExpr(a) {
					exprs = append(exprs, &IfExpr{Cond: a, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}})
				} else {
					exprs = append(exprs, a)
				}
			} else {
				fmtBuf.WriteString("%s")
				exprs = append(exprs, &IfExpr{
					Cond: &BinaryExpr{Left: &CallExpr{Func: "type", Args: []Expr{a}}, Op: "==", Right: &StringLit{Value: "table"}},
					Then: &CallExpr{Func: "repr", Args: []Expr{a}},
					Else: a,
				})
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
			io.WriteString(w, "print((string.gsub(string.format(")
			fmt.Fprintf(w, "%q", fmtBuf.String())
			for _, a := range exprs {
				io.WriteString(w, ", ")
				a.emit(w)
			}
			io.WriteString(w, "), \"%s+$\", \"\")))")
		}
		return
	case "len", "count":
		io.WriteString(w, "(function(v)\n  if type(v) == 'table' and v.items ~= nil then\n    return #v.items\n  elseif type(v) == 'table' and (v[1] == nil) then\n    local c = 0\n    for _ in pairs(v) do c = c + 1 end\n    return c\n  elseif type(v) == 'string' then\n    local l = utf8.len(v)\n    if l then return l end\n    return #v\n  elseif type(v) == 'table' then\n    return #v\n  else\n    return 0\n  end\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "get":
		// map get with optional default
		io.WriteString(w, "((function(m,k,d) local v=m[k] if v==nil then return d end return v end)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		} else {
			io.WriteString(w, "nil")
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		} else {
			io.WriteString(w, "nil")
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 2 {
			c.Args[2].emit(w)
		} else {
			io.WriteString(w, "nil")
		}
		io.WriteString(w, "))")
	case "int":
		io.WriteString(w, "math.floor(tonumber(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ") or 0)")
	case "float":
		io.WriteString(w, "tonumber(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "str":
		if len(c.Args) > 0 && (isListExpr(c.Args[0]) || isMapExpr(c.Args[0])) {
			(&CallExpr{Func: "repr", Args: c.Args}).emit(w)
		} else if len(c.Args) > 0 && isBigRatExpr(c.Args[0]) {
			io.WriteString(w, "(_str(")
			c.Args[0].emit(w)
			io.WriteString(w, ".num) .. '/' .. _str(")
			c.Args[0].emit(w)
			io.WriteString(w, ".den))")
		} else {
			io.WriteString(w, "_str(")
			if len(c.Args) > 0 {
				c.Args[0].emit(w)
			}
			io.WriteString(w, ")")
		}
	case "substring":
		io.WriteString(w, "_substring(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 2 {
			c.Args[2].emit(w)
		}
		io.WriteString(w, ")")
	case "padStart":
		io.WriteString(w, "_padStart(")
		if len(c.Args) > 0 {
			io.WriteString(w, "tostring(")
			c.Args[0].emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 2 {
			c.Args[2].emit(w)
		}
		io.WriteString(w, ")")
	case "repeat":
		io.WriteString(w, "string.rep(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ")")
	case "sha256":
		io.WriteString(w, "_sha256(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "MD5Hex":
		io.WriteString(w, "(function(bs) if type(bs)=='string' then local t={} for i=1,#bs do t[#t+1]=string.byte(bs,i) end bs=t end local res=_md5(bs) local t2={} for i=1,#res do t2[#t2+1]=string.format('%02x',res[i]) end return table.concat(t2) end)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "min":
		if len(c.Args) == 2 {
			io.WriteString(w, "math.min(")
			c.Args[0].emit(w)
			io.WriteString(w, ", ")
			c.Args[1].emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "(function(lst)\n  local m = nil\n  for _, v in ipairs(lst) do\n    if m == nil or v < m then\n      m = v\n    end\n  end\n  return m\nend)(")
			if len(c.Args) > 0 {
				c.Args[0].emit(w)
			}
			io.WriteString(w, ")")
		}
	case "max":
		if len(c.Args) == 2 {
			io.WriteString(w, "math.max(")
			c.Args[0].emit(w)
			io.WriteString(w, ", ")
			c.Args[1].emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "(function(lst)\n  local m = nil\n  for _, v in ipairs(lst) do\n    if m == nil or v > m then\n      m = v\n    end\n  end\n  return m\nend)(")
			if len(c.Args) > 0 {
				c.Args[0].emit(w)
			}
			io.WriteString(w, ")")
		}
	case "values":
		io.WriteString(w, "(function(m)\n  local keys = {}\n  for k in pairs(m) do\n    if k ~= '__name' and k ~= '__order' then table.insert(keys, k) end\n  end\n  table.sort(keys, function(a,b) return a<b end)\n  local res = {}\n  for _, k in ipairs(keys) do\n    table.insert(res, m[k])\n  end\n  return res\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "keys":
		io.WriteString(w, "(function(m)\n  local keys = {}\n  for k in pairs(m) do\n    if k ~= '__name' and k ~= '__order' then table.insert(keys, k) end\n  end\n  table.sort(keys, function(a,b) return a<b end)\n  return keys\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "append":
		// Mutate the list in place so that existing references remain valid.
		io.WriteString(w, "(function(lst, item)\n  lst = lst or {}\n  table.insert(lst, item)\n  return lst\nend)(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ")")
	case "concat":
		io.WriteString(w, "(function(a, b)\n  local res = {table.unpack(a or {})}\n  for _, v in ipairs(b or {}) do\n    res[#res+1] = v\n  end\n  return res\nend)(")
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
		io.WriteString(w, "_exists(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
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
	case "now":
		io.WriteString(w, "_now()")
	case "net.LookupHost":
		io.WriteString(w, `(function(host)
  return {"172.20.0.3", _nil}
end)(`)
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, `)`)
	case "LookupHost":
		if len(c.Args) > 0 {
			if id, ok := c.Args[0].(*Ident); ok && id.Name == "net" {
				io.WriteString(w, `(function(host)
  return {"172.20.0.3", _nil}
end)(`)
				if len(c.Args) > 1 {
					c.Args[1].emit(w)
				}
				io.WriteString(w, `)`)
				return
			}
		}
	case "indexOf":
		io.WriteString(w, "_indexOf(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ")")
	case "split":
		io.WriteString(w, "_split(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ")")
	case "slice":
		io.WriteString(w, "slice(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 1 {
			c.Args[1].emit(w)
		}
		io.WriteString(w, ", ")
		if len(c.Args) > 2 {
			c.Args[2].emit(w)
		}
		io.WriteString(w, ")")
	case "parseIntStr":
		io.WriteString(w, "_parseIntStr(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "getoutput":
		io.WriteString(w, "_getoutput(")
		start := 0
		if len(c.Args) > 1 {
			if _, ok := c.Args[0].(*Ident); ok {
				start = 1
			}
		}
		for i := start; i < len(c.Args); i++ {
			if i > start {
				io.WriteString(w, ", ")
			}
			c.Args[i].emit(w)
		}
		io.WriteString(w, ")")
	case "upper":
		io.WriteString(w, "string.upper(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
	case "lower":
		io.WriteString(w, "string.lower(")
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, ")")
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
        table.sort(keys, function(a,b) return tostring(a) > tostring(b) end)
        local parts = {"{"}
        for i, k in ipairs(keys) do
          parts[#parts+1] = '"' .. tostring(k) .. '": ' .. encode(x[k])
          if i < #keys then parts[#parts+1] = ", " end
        end
        parts[#parts+1] = "}"
        return table.concat(parts)
      end
    elseif type(x) == "string" then
      return '"' .. x .. '"'
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
	case "repr":
		io.WriteString(w, `
(function(v)
  local function encode(x)
    if type(x) == "table" then
      if x.__name and x.__order then
        local parts = {x.__name, " {"}
        for i, k in ipairs(x.__order) do
          if i > 1 then parts[#parts+1] = ", " end
          parts[#parts+1] = k .. " = " .. encode(x[k])
        end
        parts[#parts+1] = "}"
        return table.concat(parts)
      elseif #x > 0 then
        local allTables = true
        for _, v in ipairs(x) do
          if type(v) ~= "table" then allTables = false break end
        end
        local parts = {}
        if not allTables then parts[#parts+1] = "[" end
        for i, val in ipairs(x) do
          parts[#parts+1] = encode(val)
          if i < #x then parts[#parts+1] = " " end
        end
        if not allTables then parts[#parts+1] = "]" end
        return table.concat(parts)
      else
        local keys = {}
        for k in pairs(x) do if k ~= "__name" and k ~= "__order" then table.insert(keys, k) end end
        table.sort(keys, function(a,b) return tostring(a) > tostring(b) end)
        local parts = {"{"}
        for i, k in ipairs(keys) do
          parts[#parts+1] = "'" .. tostring(k) .. "': " .. encode(x[k])
          if i < #keys then parts[#parts+1] = ", " end
        end
        parts[#parts+1] = "}"
        return table.concat(parts)
      end
    elseif type(x) == "string" then
      return '"' .. x .. '"'
    else
      return tostring(x)
    end
  end
  return encode(v)
end)(`)
		if len(c.Args) > 0 {
			c.Args[0].emit(w)
		}
		io.WriteString(w, `)`)
	default:
		io.WriteString(w, sanitizeName(c.Func))
		io.WriteString(w, "(")
		for i, a := range c.Args {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			if i < len(c.ParamTypes) && needsCopyArg(c.ParamTypes[i], exprType(c)) {
				io.WriteString(w, "(function(_t) local _c={} for k,v in pairs(_t) do _c[k]=v end return _c end)(")
				a.emit(w)
				io.WriteString(w, ")")
			} else {
				a.emit(w)
			}
		}
		io.WriteString(w, ")")
	}
}

func (a *AssignStmt) emit(w io.Writer) {
	if a.Local {
		io.WriteString(w, "local ")
	}
	io.WriteString(w, sanitizeName(a.Name))
	io.WriteString(w, " = ")
	if a.Value == nil {
		io.WriteString(w, "nil")
	} else {
		a.Value.emit(w)
	}
}

func (qa *QueryAssignStmt) emit(w io.Writer) {
	if qa.Local {
		io.WriteString(w, "local ")
	}
	io.WriteString(w, sanitizeName(qa.Name))
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

func (s *SaveStmt) emit(w io.Writer) {
	if s.Format == "jsonl" && (s.Path == "" || s.Path == "-") {
		io.WriteString(w, "for _, _row in ipairs(")
		s.Src.emit(w)
		io.WriteString(w, ") do\n  ")
		(&CallExpr{Func: "json", Args: []Expr{&Ident{Name: "_row"}}}).emit(w)
		io.WriteString(w, "\nend")
		return
	}
	io.WriteString(w, "-- unsupported save")
}

func (f *FunStmt) emit(w io.Writer) {
	prev := currentEnv
	if f.Env != nil {
		currentEnv = f.Env
	}
	if f.Local {
		io.WriteString(w, "local ")
	}
	io.WriteString(w, "function ")
	io.WriteString(w, sanitizeName(f.Name))
	io.WriteString(w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, sanitizeName(p))
	}
	io.WriteString(w, ")\n")
	for _, st := range f.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "end")
	currentEnv = prev
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
	if label != "" && isTrueCond(wst.Cond) {
		io.WriteString(w, "::")
		io.WriteString(w, label)
		io.WriteString(w, "::\n")
	}
	for _, st := range wst.Body {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	if label != "" && !isTrueCond(wst.Cond) {
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
	io.WriteString(w, sanitizeName(fr.Name))
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
	if isStringExpr(fi.Iterable) {
		tmp := fmt.Sprintf("_s%d", loopCounter)
		idx := fmt.Sprintf("_i%d", loopCounter)
		loopCounter++
		io.WriteString(w, "local ")
		io.WriteString(w, tmp)
		io.WriteString(w, " = ")
		fi.Iterable.emit(w)
		io.WriteString(w, "\nfor ")
		io.WriteString(w, idx)
		io.WriteString(w, " = 1, #")
		io.WriteString(w, tmp)
		io.WriteString(w, " do\n  local ")
		io.WriteString(w, sanitizeName(fi.Name))
		io.WriteString(w, " = string.sub(")
		io.WriteString(w, tmp)
		io.WriteString(w, ", ")
		io.WriteString(w, idx)
		io.WriteString(w, ", ")
		io.WriteString(w, idx)
		io.WriteString(w, ")\n")
	} else {
		io.WriteString(w, "for ")
		if isMapExpr(fi.Iterable) {
			io.WriteString(w, sanitizeName(fi.Name))
			io.WriteString(w, " in pairs(")
		} else {
			io.WriteString(w, "_, ")
			io.WriteString(w, sanitizeName(fi.Name))
			io.WriteString(w, " in ipairs(")
		}
		if fi.Iterable != nil {
			fi.Iterable.emit(w)
		}
		io.WriteString(w, ") do\n")
	}
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

func (b *BenchStmt) emit(w io.Writer) {
	io.WriteString(w, "do\n")
	io.WriteString(w, "  collectgarbage()\n")
	io.WriteString(w, "  local _bench_start_mem = collectgarbage('count') * 1024\n")
	io.WriteString(w, "  local _bench_start = os.clock()\n")
	var funs []Stmt
	var rest []Stmt
	for _, st := range b.Body {
		if _, ok := st.(*FunStmt); ok {
			funs = append(funs, st)
		} else {
			rest = append(rest, st)
		}
	}
	stmts := append(funs, rest...)
	for _, st := range stmts {
		st.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "  local _bench_end = os.clock()\n")
	io.WriteString(w, "  collectgarbage()\n")
	io.WriteString(w, "  local _bench_end_mem = collectgarbage('count') * 1024\n")
	io.WriteString(w, "  local _bench_duration_us = math.floor((_bench_end - _bench_start) * 1000000)\n")
	io.WriteString(w, "  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))\n")
	io.WriteString(w, "  print('{\\n  \"duration_us\": ' .. _bench_duration_us .. ',\\n  \"memory_bytes\": ' .. _bench_mem .. ',\\n  \"name\": \"")
	io.WriteString(w, b.Name)
	io.WriteString(w, "\"\\n}')\n")
	io.WriteString(w, "end")
}

func (u *UpdateStmt) emit(w io.Writer) {
	idx := loopCounter
	loopCounter++
	item := "item"
	io.WriteString(w, "for _i")
	fmt.Fprintf(w, "%d = 1, #", idx)
	io.WriteString(w, u.Target)
	io.WriteString(w, " do\n")
	io.WriteString(w, "  local ")
	io.WriteString(w, item)
	io.WriteString(w, " = ")
	io.WriteString(w, u.Target)
	io.WriteString(w, "[_i")
	fmt.Fprintf(w, "%d]", idx)
	io.WriteString(w, "\n")
	if u.Cond != nil {
		io.WriteString(w, "  if ")
		u.Cond.emit(w)
		io.WriteString(w, " then\n")
	}
	pad := "  "
	if u.Cond != nil {
		pad = "    "
	}
	for i, f := range u.Fields {
		io.WriteString(w, pad)
		io.WriteString(w, item)
		io.WriteString(w, "[")
		fmt.Fprintf(w, "%q", f)
		io.WriteString(w, "] = ")
		u.Values[i].emit(w)
		io.WriteString(w, "\n")
	}
	if u.Cond != nil {
		io.WriteString(w, "  end\n")
	}
	io.WriteString(w, "  ")
	io.WriteString(w, u.Target)
	io.WriteString(w, "[_i")
	fmt.Fprintf(w, "%d] = %s\n", idx, item)
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
	case "list", "list_elem":
		// If the target is known to be a list, always apply the
		// +1 offset for Lua's 1-based indexing. Only fall back to a
		// plain map lookup when the index is non-numeric **and** we
		// cannot determine that the target is a list. This prevents
		// expressions such as xs[j] from being treated as map access
		// when the type information for j isn't available.
		if !isIntExpr(ix.Index) && !isFloatExpr(ix.Index) {
			if _, ok := exprType(ix.Target).(types.ListType); !ok {
				ix.Target.emit(w)
				io.WriteString(w, "[")
				ix.Index.emit(w)
				io.WriteString(w, "]")
				return
			}
		}
		ix.Target.emit(w)
		io.WriteString(w, "[")
		ix.Index.emit(w)
		io.WriteString(w, " + 1]")
	default: // map
		if _, ok := ix.Index.(*StringLit); !ok {
			if _, ok := exprType(ix.Target).(types.ListType); ok {
				ix.Target.emit(w)
				io.WriteString(w, "[")
				ix.Index.emit(w)
				io.WriteString(w, " + 1]")
				return
			}
			if _, ok := exprType(ix.Target).(types.StringType); ok {
				io.WriteString(w, "string.sub(")
				ix.Target.emit(w)
				io.WriteString(w, ", (")
				ix.Index.emit(w)
				io.WriteString(w, " + 1), (")
				ix.Index.emit(w)
				io.WriteString(w, " + 1)")
				io.WriteString(w, ")")
				return
			}
			if _, ok := exprType(ix.Target).(types.MapType); ok {
				ix.Target.emit(w)
				io.WriteString(w, "[")
				ix.Index.emit(w)
				io.WriteString(w, "]")
				return
			}
			// Fallback: decide based on the index expression. If it
			// looks numeric, treat as a list; otherwise emit a map
			// lookup without the +1 offset.
			if isIntExpr(ix.Index) || isFloatExpr(ix.Index) {
				ix.Target.emit(w)
				io.WriteString(w, "[")
				ix.Index.emit(w)
				io.WriteString(w, " + 1]")
			} else {
				ix.Target.emit(w)
				io.WriteString(w, "[")
				ix.Index.emit(w)
				io.WriteString(w, "]")
			}
			return
		}
		if _, ok := exprType(ix.Target).(types.ListType); ok {
			ix.Target.emit(w)
			io.WriteString(w, "[")
			ix.Index.emit(w)
			io.WriteString(w, " + 1]")
		} else if _, ok := exprType(ix.Target).(types.StringType); ok {
			io.WriteString(w, "string.sub(")
			ix.Target.emit(w)
			io.WriteString(w, ", (")
			ix.Index.emit(w)
			io.WriteString(w, " + 1), (")
			ix.Index.emit(w)
			io.WriteString(w, " + 1)")
			io.WriteString(w, ")")
		} else if s, ok := ix.Index.(*StringLit); ok && isLuaIdent(s.Value) {
			ix.Target.emit(w)
			io.WriteString(w, ".")
			io.WriteString(w, s.Value)
		} else {
			ix.Target.emit(w)
			io.WriteString(w, "[")
			ix.Index.emit(w)
			if _, ok := exprType(ix.Target).(types.MapType); ok {
				io.WriteString(w, "]")
			} else {
				// For struct fields or other non-map targets, no index offset
				// is required. Use a direct lookup without the +1 adjustment.
				io.WriteString(w, "]")
			}
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
	prev := currentEnv
	if f.Env != nil {
		currentEnv = f.Env
	}
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
	currentEnv = prev
}

func (c *ListCastExpr) emit(w io.Writer) { c.Expr.emit(w) }
func sanitizeName(n string) string {
	switch n {
	case "table", "string", "math", "io", "os", "coroutine", "utf8", "repeat":
		return "_" + n
	}
	switch n {
	case "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while":
		return "_" + n
	}
	return n
}

func (id *Ident) emit(w io.Writer) { io.WriteString(w, sanitizeName(id.Name)) }

func isStringExpr(e Expr) bool {
	if _, ok := exprType(e).(types.StringType); ok {
		return true
	}
	switch ex := e.(type) {
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
	switch s {
	case "and", "break", "do", "else", "elseif", "end", "false", "for",
		"function", "goto", "if", "in", "local", "nil", "not", "or",
		"repeat", "return", "then", "true", "until", "while":
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

func isTrueCond(e Expr) bool {
	if e == nil {
		return true
	}
	if b, ok := e.(*BoolLit); ok {
		return b.Value
	}
	return false
}

func isIntExpr(e Expr) bool {
	if t := exprType(e); t != nil {
		switch t.(type) {
		case types.IntType, types.BigIntType:
			return true
		}
	}
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *CallExpr:
		if ex.Func == "len" || ex.Func == "count" {
			return true
		}
	case *BinaryExpr:
		switch ex.Op {
		case "+", "-", "*", "/", "%":
			return isIntExpr(ex.Left) && isIntExpr(ex.Right)
		}
	}
	return false
}

func isFloatExpr(e Expr) bool {
	if t := exprType(e); t != nil {
		switch t.(type) {
		case types.FloatType, types.BigRatType:
			return true
		}
	}
	switch ex := e.(type) {
	case *FloatLit:
		return true
	case *BinaryExpr:
		switch ex.Op {
		case "+", "-", "*", "/", "%":
			return isFloatExpr(ex.Left) || isFloatExpr(ex.Right)
		}
	}
	return false
}

func isBigRatExpr(e Expr) bool {
	switch ex := e.(type) {
	case *Ident:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				if _, ok := t.(types.BigRatType); ok {
					return true
				}
			}
		}
	case *CallExpr:
		if ex.Func == "_bigrat" {
			return true
		}
	}
	if _, ok := exprType(e).(types.BigRatType); ok {
		return true
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
		case "&&", "||", "==", "!=", "<", "<=", ">", ">=", "in":
			return true
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
	switch exprType(e).(type) {
	case types.MapType, types.StructType:
		return true
	}
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
			if s, ok2 := ex.Index.(*StringLit); ok2 && s.Value == "items" {
				return false
			}
			if _, ok := exprType(e).(types.MapType); ok {
				return true
			}
			if _, ok := exprType(e).(types.StructType); ok {
				return true
			}
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
		if ex.Func == "values" || ex.Func == "append" {
			return true
		}
	case *ListCastExpr:
		return true
	case *BinaryExpr:
		switch ex.Op {
		case "union", "union_all", "except", "intersect":
			return true
		}
	}
	if _, ok := exprType(e).(types.ListType); ok {
		return true
	}
	return false
}

func needsCopyArg(param types.Type, ret types.Type) bool {
	p, ok := param.(types.StructType)
	if !ok {
		return false
	}
	if r, ok := ret.(types.StructType); ok && r.Name == p.Name {
		return true
	}
	return false
}

func exprType(e Expr) types.Type {
	switch ex := e.(type) {
	case *Ident:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				return t
			}
		}
	case *IndexExpr:
		t := exprType(ex.Target)
		if s, ok := ex.Index.(*StringLit); ok {
			switch tt := t.(type) {
			case types.StructType:
				if ft, ok := tt.Fields[s.Value]; ok {
					return ft
				}
			case types.MapType:
				return tt.Value
			}
		} else {
			switch tt := t.(type) {
			case types.ListType:
				return tt.Elem
			case types.StringType:
				return types.StringType{}
			case types.StructType:
				var common types.Type
				for _, ft := range tt.Fields {
					if common == nil {
						common = ft
					} else if common != ft {
						return types.AnyType{}
					}
				}
				if common != nil {
					return common
				}
			case types.MapType:
				return tt.Value
			}
		}
	case *StringLit:
		return types.StringType{}
	case *IntLit:
		return types.IntType{}
	case *FloatLit:
		return types.FloatType{}
	case *BoolLit:
		return types.BoolType{}
	case *CallExpr:
		if ex.Func == "tostring" {
			return types.StringType{}
		}
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					return ft.Return
				}
			}
		}
	}
	return types.AnyType{}
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
		io.WriteString(w, "(function(a,b)\n  local res={table.unpack(a or {})}\n  for _,v in ipairs(b or {}) do res[#res+1]=v end\n  return res\nend)(")
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
	if (b.Op == "==" || b.Op == "!=") && (isListExpr(b.Left) || isListExpr(b.Right) || isMapExpr(b.Left) || isMapExpr(b.Right)) {
		if b.Op == "==" {
			io.WriteString(w, "_equal(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "(not _equal(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, "))")
		}
		return
	}
	op := b.Op
	if isBigRatExpr(b.Left) || isBigRatExpr(b.Right) {
		localOp := ""
		switch op {
		case "+":
			localOp = "_add"
		case "-":
			localOp = "_sub"
		case "*":
			localOp = "_mul"
		case "/":
			localOp = "_div"
		default:
			localOp = ""
		}
		if localOp != "" {
			io.WriteString(w, localOp)
			io.WriteString(w, "(")
			b.Left.emit(w)
			io.WriteString(w, ", ")
			b.Right.emit(w)
			io.WriteString(w, ")")
			return
		}
	}
	if op == "!=" {
		op = "~="
	}
	if op == "&&" {
		op = "and"
	} else if op == "||" {
		op = "or"
	} else if op == "/" {
		if isFloatExpr(b.Left) || isFloatExpr(b.Right) {
			op = "/"
		} else {
			op = "//"
		}
	}
	if op == "+" && isListExpr(b.Left) && isListExpr(b.Right) {
		io.WriteString(w, "(function(a,b) local res={table.unpack(a or {})} for _,v in ipairs(b or {}) do res[#res+1]=v end return res end)(")
		b.Left.emit(w)
		io.WriteString(w, ", ")
		b.Right.emit(w)
		io.WriteString(w, ")")
		return
	}
	if op == "+" && !isStringExpr(b.Left) && !isStringExpr(b.Right) && (isIntExpr(b.Left) || isFloatExpr(b.Left)) && (isIntExpr(b.Right) || isFloatExpr(b.Right)) {
		// numeric addition, keep '+'
	} else if op == "+" && (isStringExpr(b.Left) || isStringExpr(b.Right)) {
		op = ".."
	} else if op == "+" && !isStringExpr(b.Left) && !isStringExpr(b.Right) && !(isIntExpr(b.Left) || isFloatExpr(b.Left)) && !(isIntExpr(b.Right) || isFloatExpr(b.Right)) {
		op = ".."
	}

	io.WriteString(w, "(")
	if op == ".." {
		if isStringExpr(b.Left) {
			b.Left.emit(w)
		} else {
			io.WriteString(w, "tostring(")
			b.Left.emit(w)
			io.WriteString(w, ")")
		}
		io.WriteString(w, " ")
		io.WriteString(w, op)
		io.WriteString(w, " ")
		if isStringExpr(b.Right) {
			b.Right.emit(w)
		} else {
			io.WriteString(w, "tostring(")
			b.Right.emit(w)
			io.WriteString(w, ")")
		}
	} else {
		b.Left.emit(w)
		io.WriteString(w, " ")
		io.WriteString(w, op)
		io.WriteString(w, " ")
		b.Right.emit(w)
	}
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
		if isBigRatExpr(u.Value) {
			io.WriteString(w, "_sub(_bigrat(0), ")
			if u.Value != nil {
				u.Value.emit(w)
			}
			io.WriteString(w, ")")
		} else {
			io.WriteString(w, "(-")
			if u.Value != nil {
				u.Value.emit(w)
			}
			io.WriteString(w, ")")
		}
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
		if a.Variant != "" {
			io.WriteString(w, "_m['__name'] == ")
			fmt.Fprintf(w, "%q", a.Variant)
		} else if a.Pattern != nil {
			io.WriteString(w, "_m == ")
			a.Pattern.emit(w)
		} else {
			io.WriteString(w, "true")
		}
		io.WriteString(w, " then\n")
		for i, v := range a.Vars {
			io.WriteString(w, "    local ")
			io.WriteString(w, v)
			io.WriteString(w, " = _m[")
			fmt.Fprintf(w, "%q", a.Fields[i])
			io.WriteString(w, "]\n")
		}
		io.WriteString(w, "    return ")
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
			io.WriteString(w, "local __tmp = {}\nlocal res = {}\nlocal _idx = 0\n")
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
			io.WriteString(w, "  _idx = _idx + 1\n  table.insert(__tmp, {i = _idx, k = ")
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
				io.WriteString(w, "  return a.i < b.i\nend)\n")
			} else {
				io.WriteString(w, "table.sort(__tmp, function(a,b) if a.k == b.k then return a.i < b.i end return a.k < b.k end)\n")
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
		io.WriteString(w, "(function()\n  local __tmp = {}\n  local _res = {}\n  local _idx = 0\n")
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
		io.WriteString(w, "_idx = _idx + 1\n  table.insert(__tmp, {i = _idx, k = ")
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
		(&CallExpr{Func: qc.Agg, Args: []Expr{&Ident{Name: "_tmp"}}, ParamTypes: []types.Type{types.ListType{Elem: types.AnyType{}}}}).emit(w)
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
			io.WriteString(w, "    return a.i < b.i\n  end)\n")
		} else {
			io.WriteString(w, "  table.sort(__tmp, function(a,b) if a.k == b.k then return a.i < b.i end return a.k < b.k end)\n")
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

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

// identNameUnary returns the identifier name represented by a unary expression
// if it is a simple identifier expression.
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

// intValuePostfix returns the integer literal value represented by the postfix
// expression if it is a plain integer literal without any postfix operators.
func intValuePostfix(p *parser.PostfixExpr) (int, bool) {
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Int == nil {
		return 0, false
	}
	return int(*p.Target.Lit.Int), true
}

// intValue returns the integer value represented by the expression if it is a
// literal integer.
func intValue(e *parser.Expr) (int, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return 0, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return 0, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Int == nil {
		return 0, false
	}
	return int(*p.Target.Lit.Int), true
}

// isStartPlusOne reports whether end represents the expression start + 1.
func isStartPlusOne(start, end *parser.Expr) bool {
	if start == nil || end == nil || end.Binary == nil || len(end.Binary.Right) != 1 {
		return false
	}
	op := end.Binary.Right[0]
	if op.Op != "+" {
		return false
	}
	startName, ok := identName(start)
	if !ok {
		return false
	}
	leftName, ok := identNameUnary(end.Binary.Left)
	if !ok || leftName != startName {
		return false
	}
	if val, ok := intValuePostfix(op.Right); ok && val == 1 {
		return true
	}
	return false
}

func replaceFields(e Expr, target Expr, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Ident:
		if fields[ex.Name] {
			return &IndexExpr{Target: target, Index: &StringLit{Value: ex.Name}, Kind: "map"}
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceFields(ex.Left, target, fields)
		ex.Right = replaceFields(ex.Right, target, fields)
		return ex
	case *UnaryExpr:
		ex.Value = replaceFields(ex.Value, target, fields)
		return ex
	case *CallExpr:
		for i := range ex.Args {
			ex.Args[i] = replaceFields(ex.Args[i], target, fields)
		}
		return ex
	case *IndexExpr:
		ex.Target = replaceFields(ex.Target, target, fields)
		ex.Index = replaceFields(ex.Index, target, fields)
		return ex
	case *SliceExpr:
		ex.Target = replaceFields(ex.Target, target, fields)
		if ex.Start != nil {
			ex.Start = replaceFields(ex.Start, target, fields)
		}
		if ex.End != nil {
			ex.End = replaceFields(ex.End, target, fields)
		}
		return ex
	case *IfExpr:
		ex.Cond = replaceFields(ex.Cond, target, fields)
		ex.Then = replaceFields(ex.Then, target, fields)
		ex.Else = replaceFields(ex.Else, target, fields)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceFields(ex.Elems[i], target, fields)
		}
		return ex
	case *MapLit:
		for i := range ex.Keys {
			ex.Keys[i] = replaceFields(ex.Keys[i], target, fields)
			ex.Values[i] = replaceFields(ex.Values[i], target, fields)
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
	return fmt.Sprintf("-- Generated by Mochi v%s on %s\n"+
		"function input()\n  local line = io.read('*l')\n  if line == nil then return '' end\n  return line\nend\n"+
		"local _nil = {}\n",
		version(), t.Format("2006-01-02 15:04 MST"))
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

func mapLiteralExpr(e *parser.Expr) *parser.MapLiteral {
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
	ml := mapLiteralExpr(e)
	if ml == nil {
		return ""
	}
	for _, it := range ml.Items {
		key, ok := literalString(it.Key)
		if !ok {
			continue
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func valueToExpr(v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		keys := make([]Expr, len(names))
		vals := make([]Expr, len(names))
		for i, k := range names {
			keys[i] = &StringLit{Value: k}
			vals[i] = valueToExpr(val[k], nil)
		}
		return &MapLit{Keys: keys, Values: vals}
	case []interface{}:
		elems := make([]Expr, len(val))
		for i, it := range val {
			elems[i] = valueToExpr(it, typ)
		}
		return &ListLit{Elems: elems}
	case string:
		return &StringLit{Value: val}
	case bool:
		return &BoolLit{Value: val}
	case int64:
		return &IntLit{Value: int(val)}
	case int:
		return &IntLit{Value: val}
	case float64:
		return &FloatLit{Value: val}
	case float32:
		return &FloatLit{Value: float64(val)}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func dataExprFromFile(path, format string, typ *parser.TypeRef) (Expr, error) {
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
	case "json", "":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v, typ), nil
}

// collectHelpers traverses the program and returns a set of helper names that
// are required by the emitted Lua code.
func collectHelpers(p *Program) map[string]bool {
        used := map[string]bool{}

        prevEnv := currentEnv
        currentEnv = p.Env
        defer func() { currentEnv = prevEnv }()

        var walkStmt func(Stmt)
        var walkExpr func(Expr)
	walkExpr = func(e Expr) {
		switch ex := e.(type) {
		case *CallExpr:
			switch ex.Func {
			case "now":
				used["now"] = true
			case "_bigrat", "_add", "_sub", "_mul", "_div", "num", "denom":
				used["bigrat"] = true
			case "padStart":
				used["padStart"] = true
			case "sha256":
				used["sha256"] = true
			case "MD5Hex":
				used["md5"] = true
			case "getoutput":
				used["getoutput"] = true
			case "indexOf":
				used["indexOf"] = true
			case "exists":
				used["exists"] = true
			case "str":
				used["str"] = true
			case "split":
				used["split"] = true
			case "substring":
				used["substring"] = true
			case "slice":
				used["slice"] = true
			case "parseIntStr":
				used["parseIntStr"] = true
			case "_fetch":
				used["fetch"] = true
			case "_environ":
				used["environ"] = true
			case "_panic":
				used["panic"] = true
			}
			for _, a := range ex.Args {
				walkExpr(a)
			}
		case *DynCallExpr:
			if ix, ok := ex.Fn.(*IndexExpr); ok {
				if s, ok2 := ix.Index.(*StringLit); ok2 {
					switch s.Value {
					case "slice":
						used["slice"] = true
					case "split":
						used["split"] = true
					case "substring":
						used["substring"] = true
					case "now":
						used["now"] = true
					}
				}
				walkExpr(ix.Target)
				walkExpr(ix.Index)
			} else {
				walkExpr(ex.Fn)
			}
			for _, a := range ex.Args {
				walkExpr(a)
			}
		case *IfExpr:
			walkExpr(ex.Cond)
			walkExpr(ex.Then)
			walkExpr(ex.Else)
		case *BinaryExpr:
			if (ex.Op == "==" || ex.Op == "!=") && (isListExpr(ex.Left) || isListExpr(ex.Right) || isMapExpr(ex.Left) || isMapExpr(ex.Right)) {
				used["equal"] = true
			}
			walkExpr(ex.Left)
			walkExpr(ex.Right)
		case *UnaryExpr:
			walkExpr(ex.Value)
		case *ListLit:
			for _, it := range ex.Elems {
				walkExpr(it)
			}
		case *MapLit:
			for i := range ex.Keys {
				walkExpr(ex.Keys[i])
				walkExpr(ex.Values[i])
			}
		case *IndexExpr:
			walkExpr(ex.Target)
			walkExpr(ex.Index)
		case *SliceExpr:
			used["slice"] = true
			walkExpr(ex.Target)
			if ex.Start != nil {
				walkExpr(ex.Start)
			}
			if ex.End != nil {
				walkExpr(ex.End)
			}
		case *FunExpr:
			for _, st := range ex.Body {
				walkStmt(st)
			}
			if ex.Expr != nil {
				walkExpr(ex.Expr)
			}
		case *MatchExpr:
			walkExpr(ex.Target)
			for _, a := range ex.Arms {
				if a.Pattern != nil {
					walkExpr(a.Pattern)
				}
				walkExpr(a.Result)
			}
		case *QueryComp:
			for _, s := range ex.Sources {
				walkExpr(s)
			}
			if ex.Body != nil {
				walkExpr(ex.Body)
			}
			if ex.Where != nil {
				walkExpr(ex.Where)
			}
			if ex.GroupKey != nil {
				walkExpr(ex.GroupKey)
			}
			if ex.Having != nil {
				walkExpr(ex.Having)
			}
			if ex.SortKey != nil {
				walkExpr(ex.SortKey)
			}
			if ex.Skip != nil {
				walkExpr(ex.Skip)
			}
			if ex.Take != nil {
				walkExpr(ex.Take)
			}
		}
	}

	walkStmt = func(s Stmt) {
		switch st := s.(type) {
		case *ExprStmt:
			walkExpr(st.Expr)
		case *AssignStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *QueryAssignStmt:
			walkExpr(st.Query)
		case *SaveStmt:
			walkExpr(st.Src)
		case *FunStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ReturnStmt:
			walkExpr(st.Value)
		case *IfStmt:
			walkExpr(st.Cond)
			for _, b := range st.Then {
				walkStmt(b)
			}
			for _, b := range st.Else {
				walkStmt(b)
			}
		case *WhileStmt:
			walkExpr(st.Cond)
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForRangeStmt:
			if st.Start != nil {
				walkExpr(st.Start)
			}
			if st.End != nil {
				walkExpr(st.End)
			}
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForInStmt:
			walkExpr(st.Iterable)
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *BenchStmt:
			used["now"] = true
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *UpdateStmt:
			for _, v := range st.Values {
				walkExpr(v)
			}
			if st.Cond != nil {
				walkExpr(st.Cond)
			}
		}
	}

	for _, st := range p.Stmts {
		walkStmt(st)
	}
	return used
}

// Emit converts the AST back into Lua source code with a standard header.
func Emit(p *Program) []byte {
	var b bytes.Buffer
	b.WriteString(header())

	used := collectHelpers(p)
	if used["now"] {
		b.WriteString(helperNow)
	}
	if used["padStart"] {
		b.WriteString(helperPadStart)
	}
	if used["bigrat"] {
		b.WriteString(helperBigRat)
	}
	if used["sha256"] {
		b.WriteString(helperSHA256)
	}
	if used["md5"] {
		b.WriteString(helperMD5)
	}
	if used["getoutput"] {
		b.WriteString(helperGetOutput)
	}
	if used["indexOf"] {
		b.WriteString(helperIndexOf)
	}
	if used["environ"] {
		b.WriteString(helperEnviron)
	}
	if used["panic"] {
		b.WriteString(helperPanic)
	}
	if used["exists"] {
		b.WriteString(helperExists)
	}
	if used["equal"] {
		b.WriteString(helperEqual)
	}
	if used["str"] {
		b.WriteString(helperStr)
	}
	if used["parseIntStr"] {
		b.WriteString(helperParseIntStr)
	}
	if used["fetch"] {
		b.WriteString(helperFetch)
	}
	if used["split"] {
		b.WriteString(helperSplit)
	}
	if used["substring"] {
		b.WriteString(helperSubstring)
	}
	if used["slice"] {
		b.WriteString(helperSlice)
	}

	prevEnv := currentEnv
	currentEnv = p.Env
	var funs []Stmt
	var rest []Stmt
	for _, st := range p.Stmts {
		if _, ok := st.(*FunStmt); ok {
			funs = append(funs, st)
		} else {
			rest = append(rest, st)
		}
	}
	stmts := append(funs, rest...)
	for i, st := range stmts {
		if i > 0 {
			b.WriteByte('\n')
		}
		st.emit(&b)
		b.WriteString(";\n")
	}
	currentEnv = prevEnv
	code := formatLua(b.Bytes())
	if len(code) == 0 || code[len(code)-1] != '\n' {
		code = append(code, '\n')
	}
	return code
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
			return 3
		case "+", "-":
			return 2
		case "==", "!=", "<", "<=", ">", ">=", "in":
			return 1
		case "&&":
			return 0
		case "||":
			return -1
		case "union", "union_all", "except", "intersect":
			return -2
		default:
			return 0
		}
	}
	for len(ops) > 0 {
		best := 0
		bestPrec := prec(ops[0])
		for i, op := range ops {
			if p := prec(op); p > bestPrec {
				bestPrec = p
				best = i
			}
		}
		expr := &BinaryExpr{Left: exprs[best], Op: ops[best], Right: exprs[best+1]}
		exprs = append(exprs[:best], append([]Expr{expr}, exprs[best+2:]...)...)
		ops = append(ops[:best], ops[best+1:]...)
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
			expr = &UnaryExpr{Op: "-", Value: expr}
		case "!":
			expr = &UnaryExpr{Op: "!", Value: expr}
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
			kind := "list_elem"
			if isStringExpr(expr) {
				kind = "string"
			} else if isMapExpr(expr) {
				kind = "map"
			} else if id, ok := expr.(*Ident); ok && strings.HasSuffix(id.Name, "Map") {
				kind = "map"
			} else if _, ok := idx.(*StringLit); ok {
				kind = "map"
			} else if !isIntExpr(idx) && !isFloatExpr(idx) && !isListExpr(expr) {
				// default to map access when index is not clearly numeric and target is not a list
				kind = "map"
			}
			if _, ok := exprType(expr).(types.ListType); ok {
				// force list indexing when the target type is a list
				kind = "list_elem"
			}
			if ie, ok := expr.(*IndexExpr); ok && ie.Kind == "map" {
				if _, ok := exprType(ie).(types.ListType); ok {
					kind = "list_elem"
				}
			}
			expr = &IndexExpr{Target: expr, Index: idx, Kind: kind}
		case op.Index != nil:
			// If this is a slice of a single element like a[i:i+1]
			// convert it into an index access so that the result
			// has the element type rather than a list. Several
			// Rosetta examples use this pattern to fetch a single
			// string from a list of strings.
			if op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil && op.Index.Start != nil && op.Index.End != nil {
				if isStartPlusOne(op.Index.Start, op.Index.End) {
					idx, err := convertExpr(op.Index.Start)
					if err != nil {
						return nil, err
					}
					kind := "list_elem"
					if isStringExpr(expr) {
						kind = "string"
					} else if isMapExpr(expr) {
						kind = "map"
					}
					expr = &IndexExpr{Target: expr, Index: idx, Kind: kind}
					continue
				}
			}
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
				for _, a := range call.Args {
					ae, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ae)
				}
				if op.Field.Name == "keys" || op.Field.Name == "values" || op.Field.Name == "get" {
					// built-in map methods
					args = append([]Expr{expr}, args...)
					expr = &CallExpr{Func: op.Field.Name, Args: args}
				} else if op.Field.Name == "padStart" {
					// string method
					args = append([]Expr{expr}, args...)
					expr = &CallExpr{Func: "padStart", Args: args}
				} else if id, ok := expr.(*Ident); ok {
					if currentEnv != nil {
						if t, err := currentEnv.GetVar(id.Name); err == nil {
							if st, ok := t.(types.StructType); ok {
								if info, ok := currentEnv.GetStruct(st.Name); ok {
									if m, ok := info.Methods[op.Field.Name]; ok {
										args = append([]Expr{expr}, args...)
										expr = &CallExpr{Func: op.Field.Name, Args: args, ParamTypes: m.Type.Params}
										continue
									}
								}
							}
						}
					}
					if currentEnv != nil {
						if t, err := currentEnv.GetVar(op.Field.Name); err == nil {
							if ft, ok := t.(types.FuncType); ok {
								args = append([]Expr{expr}, args...)
								expr = &CallExpr{Func: op.Field.Name, Args: args, ParamTypes: ft.Params}
								continue
							}
						}
					}
					if !isMapExpr(expr) && !isListExpr(expr) && !isStringExpr(expr) {
						args = append([]Expr{expr}, args...)
						expr = &CallExpr{Func: op.Field.Name, Args: args}
						continue
					}
					if id.Name == "net" {
						// treat as a namespaced function call for known package
						name := id.Name + "." + op.Field.Name
						cexpr := &CallExpr{Func: name, Args: args}
						if currentEnv != nil {
							if t, err := currentEnv.GetVar(name); err == nil {
								if ft, ok := t.(types.FuncType); ok {
									cexpr.ParamTypes = ft.Params
								}
							}
						}
						expr = cexpr
					} else if id.Name == "math" {
						// math functions map directly to the math package
						name := id.Name + "." + op.Field.Name
						cexpr := &CallExpr{Func: name, Args: args}
						if currentEnv != nil {
							if t, err := currentEnv.GetVar(name); err == nil {
								if ft, ok := t.(types.FuncType); ok {
									cexpr.ParamTypes = ft.Params
								}
							}
						}
						expr = cexpr
					} else if id.Name == "os" && op.Field.Name == "Getenv" {
						expr = &CallExpr{Func: "os.getenv", Args: args}
						continue
					} else if id.Name == "os" && op.Field.Name == "Environ" {
						expr = &CallExpr{Func: "_environ"}
						continue
					} else if id.Name == "stdout" && op.Field.Name == "write" {
						// map stdout.write to io.write
						expr = &CallExpr{Func: "io.write", Args: args}
					} else {
						// call function stored in a table field
						fn := &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}, Kind: "map"}
						expr = &DynCallExpr{Fn: fn, Args: args}
					}
				} else {
					fn := &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}, Kind: "map"}
					expr = &DynCallExpr{Fn: fn, Args: args}
				}
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
				cexpr := &CallExpr{Func: id.Name, Args: args}
				if currentEnv != nil {
					if t, err := currentEnv.GetVar(id.Name); err == nil {
						if ft, ok := t.(types.FuncType); ok {
							cexpr.ParamTypes = ft.Params
						}
					}
				}
				expr = cexpr
			} else if ix, ok := expr.(*IndexExpr); ok {
				// Call a function stored in a table field
				expr = &DynCallExpr{Fn: ix, Args: args}
			} else {
				expr = &DynCallExpr{Fn: expr, Args: args}
			}
		case op.Cast != nil:
			if op.Cast.Type != nil {
				if op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "list" {
					expr = &ListCastExpr{Expr: expr}
				} else if op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "list" {
					expr = &ListCastExpr{Expr: expr}
				} else if op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int" {
					expr = &CallExpr{Func: "int", Args: []Expr{expr}}
				} else if op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "bigrat" {
					expr = &CallExpr{Func: "_bigrat", Args: []Expr{expr}}
				}
			}
		default:
			return nil, fmt.Errorf("postfix ops not supported")
		}
	}
	expr = normalizeDynCall(expr)
	return expr, nil
}

func normalizeDynCall(e Expr) Expr {
	if dc, ok := e.(*DynCallExpr); ok {
		if ix, ok := dc.Fn.(*IndexExpr); ok {
			if id, ok := ix.Target.(*Ident); ok {
				if sl, ok := ix.Index.(*StringLit); ok {
					if currentEnv != nil {
						if t, err := currentEnv.GetVar(sl.Value); err == nil {
							if ft, ok := t.(types.FuncType); ok {
								args := append([]Expr{id}, dc.Args...)
								return &CallExpr{Func: sl.Value, Args: args, ParamTypes: ft.Params}
							}
						}
					}
					if !isMapExpr(ix.Target) && !isListExpr(ix.Target) && !isStringExpr(ix.Target) {
						args := append([]Expr{id}, dc.Args...)
						return &CallExpr{Func: sl.Value, Args: args}
					}
				}
			}
		}
	}
	return e
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Lit != nil:
		return convertLiteral(p.Lit)
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}, nil
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		return dataExprFromFile(path, format, p.Load.Type)
	case p.Call != nil:
		ce := &CallExpr{Func: p.Call.Func}
		var ft types.FuncType
		var hasFT bool
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(p.Call.Func); err == nil {
				if ftt, ok := t.(types.FuncType); ok {
					ft = ftt
					hasFT = true
					ce.ParamTypes = ft.Params
				}
			} else if currentStruct != "" {
				if st, ok := currentEnv.GetStruct(currentStruct); ok {
					if m, ok := st.Methods[p.Call.Func]; ok {
						ft = m.Type
						hasFT = true
						ce.ParamTypes = ft.Params
					}
				}
			}
		}
		insertedSelf := false
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			ce.Args = append(ce.Args, ae)
		}
		if hasFT && currentStruct != "" && len(ft.Params) > 0 {
			if stParam, ok := ft.Params[0].(types.StructType); ok && stParam.Name == currentStruct {
				if len(ce.Args) == len(ft.Params)-1 {
					ce.Args = append([]Expr{&Ident{Name: "self"}}, ce.Args...)
					insertedSelf = true
				}
			}
		}
		switch p.Call.Func {
		case "append", "avg", "sum", "contains", "len", "count", "now", "sha256", "indexOf", "parseIntStr", "repeat", "_environ", "int", "float", "str":
			// handled during emission
			return ce, nil
		case "panic":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("panic expects 1 arg")
			}
			ce.Func = "_panic"
			return ce, nil
		case "substr":
			ce.Func = "substring"
			return ce, nil
		}
		// Avoid rewriting calls into field accesses when the callee is
		// not found in the current environment. This preserves
		// recursive and forward function calls.
		if hasFT {
			argCount := len(p.Call.Args)
			if insertedSelf {
				argCount++
			}
			if argCount < len(ft.Params) {
				params := []string{}
				for i := argCount; i < len(ft.Params); i++ {
					params = append(params, fmt.Sprintf("p%d", i-argCount))
				}
				args := append([]Expr{}, ce.Args...)
				for _, p := range params {
					args = append(args, &Ident{Name: p})
				}
				body := []Stmt{&ReturnStmt{Value: &CallExpr{Func: p.Call.Func, Args: args, ParamTypes: ft.Params}}}
				return &FunExpr{Params: params, Body: body}, nil
			}
		}
		return ce, nil
	case p.Struct != nil:
		var keys []Expr
		var values []Expr
		if currentEnv != nil {
			if _, ok := currentEnv.FindUnionByVariant(p.Struct.Name); ok {
				keys = append(keys, &StringLit{Value: "__name"})
				values = append(values, &StringLit{Value: p.Struct.Name})
			} else if _, ok := currentEnv.GetStruct(p.Struct.Name); !ok {
				return nil, fmt.Errorf("unknown struct %s", p.Struct.Name)
			}
		}
		for _, f := range p.Struct.Fields {
			v, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			keys = append(keys, &StringLit{Value: f.Name})
			values = append(values, v)
		}
		return &MapLit{Keys: keys, Values: values}, nil
	case p.List != nil:
		if st, ok := types.InferStructFromList(p.List, currentEnv); ok {
			structCount++
			name := fmt.Sprintf("GenType%d", structCount)
			currentEnv.SetStruct(name, st)
			var elems []Expr
			for _, e := range p.List.Elems {
				ml := e.Binary.Left.Value.Target.Map
				keys := []Expr{&StringLit{Value: "__name"}, &StringLit{Value: "__order"}}
				values := []Expr{&StringLit{Value: name}}
				orderVals := make([]Expr, len(st.Order))
				for i, f := range st.Order {
					orderVals[i] = &StringLit{Value: f}
				}
				values = append(values, &ListLit{Elems: orderVals})
				for _, it := range ml.Items {
					ve, err := convertExpr(it.Value)
					if err != nil {
						return nil, err
					}
					if s, ok := literalString(it.Key); ok {
						keys = append(keys, &StringLit{Value: s})
					} else {
						k, err := convertExpr(it.Key)
						if err != nil {
							return nil, err
						}
						keys = append(keys, k)
					}
					values = append(values, ve)
				}
				elems = append(elems, &MapLit{Keys: keys, Values: values})
			}
			return &ListLit{Elems: elems}, nil
		}
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
		if st, ok := types.InferStructFromMapEnv(p.Map, currentEnv); ok {
			structCount++
			name := fmt.Sprintf("GenType%d", structCount)
			currentEnv.SetStruct(name, st)
			keys := []Expr{&StringLit{Value: "__name"}, &StringLit{Value: "__order"}}
			values := []Expr{&StringLit{Value: name}}
			orderVals := make([]Expr, len(st.Order))
			for i, f := range st.Order {
				orderVals[i] = &StringLit{Value: f}
			}
			values = append(values, &ListLit{Elems: orderVals})
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
		}
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
		child := types.NewEnv(currentEnv)
		for _, pa := range p.FunExpr.Params {
			fe.Params = append(fe.Params, pa.Name)
			if pa.Type != nil {
				child.SetVar(pa.Name, types.ResolveTypeRef(pa.Type, currentEnv), false)
			} else {
				child.SetVar(pa.Name, types.AnyType{}, false)
			}
		}
		prev := currentEnv
		currentEnv = child
		funcDepth++
		if p.FunExpr.ExprBody != nil {
			expr, err := convertExpr(p.FunExpr.ExprBody)
			if err != nil {
				currentEnv = prev
				funcDepth--
				return nil, err
			}
			fe.Expr = expr
		}
		for _, st := range p.FunExpr.BlockBody {
			s, err := convertStmt(st)
			if err != nil {
				currentEnv = prev
				funcDepth--
				return nil, err
			}
			fe.Body = append(fe.Body, s)
		}
		currentEnv = prev
		funcDepth--
		fe.Env = child
		return fe, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 && currentEnv != nil {
			if _, ok := currentEnv.FindUnionByVariant(p.Selector.Root); ok {
				return &MapLit{Keys: []Expr{&StringLit{Value: "__name"}}, Values: []Expr{&StringLit{Value: p.Selector.Root}}}, nil
			}
		}
		expr := Expr(&Ident{Name: p.Selector.Root})
		t := exprType(expr)
		for _, name := range p.Selector.Tail {
			if st, ok := t.(types.StructType); ok {
				if ft, ok := st.Fields[name]; ok {
					t = ft
				} else {
					t = types.AnyType{}
				}
			}
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
	case l.Null:
		return &Ident{Name: "nil"}, nil
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
		var variant string
		var vars []string
		var fields []string
		var pat Expr
		if call, ok := callPattern(c.Pattern); ok && currentEnv != nil {
			if ut, ok := currentEnv.FindUnionByVariant(call.Func); ok {
				variant = call.Func
				st := ut.Variants[call.Func]
				fields = st.Order
				for _, a := range call.Args {
					if name, ok := identName(a); ok {
						vars = append(vars, name)
					}
				}
			}
		} else if name, ok := identName(c.Pattern); ok {
			if _, ok := currentEnv.FindUnionByVariant(name); ok {
				variant = name
			}
		}
		if variant == "" {
			var err error
			pat, err = convertExpr(c.Pattern)
			if err != nil {
				return nil, err
			}
			if id, ok := pat.(*Ident); ok && id.Name == "_" {
				pat = nil
			}
		}
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		arms[i] = MatchArm{Pattern: pat, Variant: variant, Vars: vars, Fields: fields, Result: res}
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
		child := types.NewEnv(currentEnv)
		child.SetVar(fs.Name, types.IntType{}, true)
		prev := currentEnv
		currentEnv = child
		var body []Stmt
		for _, st := range fs.Body {
			s, err := convertStmt(st)
			if err != nil {
				currentEnv = prev
				return nil, err
			}
			body = append(body, s)
		}
		currentEnv = prev
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fs.Source)
	if err != nil {
		return nil, err
	}
	var elemType types.Type = types.AnyType{}
	if lt, ok := exprType(iter).(types.ListType); ok {
		elemType = lt.Elem
	} else if _, ok := exprType(iter).(types.StringType); ok {
		elemType = types.StringType{}
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(fs.Name, elemType, true)
	prev := currentEnv
	currentEnv = child
	var body []Stmt
	for _, st := range fs.Body {
		s, err := convertStmt(st)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
		body = append(body, s)
	}
	currentEnv = prev
	return &ForInStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertBenchStmt(bs *parser.BenchBlock) (Stmt, error) {
	var body []Stmt
	for _, st := range bs.Body {
		s, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			body = append(body, s)
		}
	}
	return &BenchStmt{Name: bs.Name, Body: body}, nil
}

func convertImportStmt(im *parser.ImportStmt) (Stmt, error) {
	if im.Lang == nil {
		return nil, nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	path := strings.Trim(im.Path, "\"")
	switch *im.Lang {
	case "python":
		if path == "math" {
			pkg := &MapLit{
				Keys: []Expr{
					&StringLit{Value: "sqrt"},
					&StringLit{Value: "pow"},
					&StringLit{Value: "sin"},
					&StringLit{Value: "log"},
					&StringLit{Value: "pi"},
					&StringLit{Value: "e"},
				},
				Values: []Expr{
					&Ident{Name: "math.sqrt"},
					&Ident{Name: "math.pow"},
					&Ident{Name: "math.sin"},
					&Ident{Name: "math.log"},
					&Ident{Name: "math.pi"},
					&CallExpr{Func: "math.exp", Args: []Expr{&IntLit{Value: 1}}},
				},
			}
			return &AssignStmt{Name: alias, Value: pkg}, nil
		}
	case "go":
		if path == "mochi/runtime/ffi/go/testpkg" {
			fn := &FunExpr{Params: []string{"a", "b"}, Expr: &BinaryExpr{Left: &Ident{Name: "a"}, Op: "+", Right: &Ident{Name: "b"}}}
			fp := &FunExpr{Params: []string{}, Expr: &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}}
			ecdsa := &FunExpr{
				Params: []string{},
				Expr: &MapLit{
					Keys: []Expr{
						&StringLit{Value: "D"},
						&StringLit{Value: "X"},
						&StringLit{Value: "Y"},
						&StringLit{Value: "Hash"},
						&StringLit{Value: "R"},
						&StringLit{Value: "S"},
						&StringLit{Value: "Valid"},
					},
					Values: []Expr{
						&StringLit{Value: "1234567890"},
						&StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"},
						&StringLit{Value: "86807430002474105664458509423764867536342689150582922106807036347047552480521"},
						&StringLit{Value: "0xe6f9ed0d"},
						&StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"},
						&StringLit{Value: "94150071556658883365738746782965214584303361499725266605620843043083873122499"},
						&BoolLit{Value: true},
					},
				},
			}
			pkg := &MapLit{
				Keys: []Expr{
					&StringLit{Value: "Add"},
					&StringLit{Value: "Pi"},
					&StringLit{Value: "Answer"},
					&StringLit{Value: "FifteenPuzzleExample"},
					&StringLit{Value: "ECDSAExample"},
					&StringLit{Value: "MD5Hex"},
				},
				Values: []Expr{
					fn,
					&FloatLit{Value: 3.14},
					&IntLit{Value: 42},
					fp,
					ecdsa,
					&FunExpr{Params: []string{"bs"}, Expr: &CallExpr{Func: "MD5Hex", Args: []Expr{&Ident{Name: "bs"}}}},
				},
			}
			return &AssignStmt{Name: alias, Value: pkg}, nil
		}
	}
	return nil, nil
}

func convertUpdateStmt(u *parser.UpdateStmt) (Stmt, error) {
	if currentEnv == nil {
		return nil, fmt.Errorf("missing env")
	}
	t, err := currentEnv.GetVar(u.Target)
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
	child := types.NewEnv(currentEnv)
	fields := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fields[name] = true
	}
	prev := currentEnv
	currentEnv = child
	var fNames []string
	var vals []Expr
	for _, it := range u.Set.Items {
		key, _ := identName(it.Key)
		val, err := convertExpr(it.Value)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
		val = replaceFields(val, &Ident{Name: "item"}, fields)
		fNames = append(fNames, key)
		vals = append(vals, val)
	}
	var cond Expr
	if u.Where != nil {
		c, err := convertExpr(u.Where)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
		cond = replaceFields(c, &Ident{Name: "item"}, fields)
	}
	currentEnv = prev
	return &UpdateStmt{Target: u.Target, Fields: fNames, Values: vals, Cond: cond}, nil
}

func convertFunStmt(fs *parser.FunStmt) (Stmt, error) {
	local := funcDepth > 0
	f := &FunStmt{Name: fs.Name, Local: local}
	child := types.NewEnv(currentEnv)
	var params []types.Type
	for _, p := range fs.Params {
		f.Params = append(f.Params, p.Name)
		if p.Type != nil {
			t := types.ResolveTypeRef(p.Type, currentEnv)
			child.SetVar(p.Name, t, false)
			params = append(params, t)
		} else {
			child.SetVar(p.Name, types.AnyType{}, false)
			params = append(params, types.AnyType{})
		}
	}
	var retType types.Type = types.VoidType{}
	if fs.Return != nil {
		retType = types.ResolveTypeRef(fs.Return, currentEnv)
	}
	ft := types.FuncType{Params: params, Return: retType}
	child.SetVar(fs.Name, ft, false)
	if currentEnv != nil {
		currentEnv.SetVar(fs.Name, ft, false)
	}
	prev := currentEnv
	currentEnv = child
	funcDepth++
	for _, st := range fs.Body {
		s, err := convertStmt(st)
		if err != nil {
			currentEnv = prev
			funcDepth--
			return nil, err
		}
		f.Body = append(f.Body, s)
	}
	currentEnv = prev
	funcDepth--
	f.Env = child
	return f, nil
}

func convertMethodStmt(fs *parser.FunStmt, structName string, fields []string) (Stmt, error) {
	local := funcDepth > 0
	f := &FunStmt{Name: fs.Name, Local: local}
	child := types.NewEnv(currentEnv)
	f.Params = append(f.Params, "self")
	if structName != "" {
		child.SetVar("self", types.StructType{Name: structName}, false)
	} else {
		child.SetVar("self", types.AnyType{}, false)
	}
	for _, p := range fs.Params {
		f.Params = append(f.Params, p.Name)
		if p.Type != nil {
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, currentEnv), false)
		} else {
			child.SetVar(p.Name, types.AnyType{}, false)
		}
	}
	prev := currentEnv
	prevStruct := currentStruct
	currentStruct = structName
	currentEnv = child
	funcDepth++
	// preload fields as locals
	for _, name := range fields {
		assign := &AssignStmt{Local: true, Name: name, Value: &IndexExpr{Target: &Ident{Name: "self"}, Index: &StringLit{Value: name}, Kind: "map"}}
		f.Body = append(f.Body, assign)
	}
	for _, st := range fs.Body {
		s, err := convertStmt(st)
		if err != nil {
			currentEnv = prev
			currentStruct = prevStruct
			funcDepth--
			return nil, err
		}
		f.Body = append(f.Body, s)
	}
	currentEnv = prev
	funcDepth--
	f.Env = child
	return f, nil
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Import != nil:
		return convertImportStmt(st.Import)
	case st.Type != nil:
		ms := &MultiStmt{}
		var fields []string
		fieldTypes := map[string]types.Type{}
		methods := map[string]types.Method{}
		if st.Type.Members != nil {
			for _, m := range st.Type.Members {
				if m.Field != nil {
					fields = append(fields, m.Field.Name)
					var ft types.Type = types.AnyType{}
					if m.Field.Type != nil {
						ft = types.ResolveTypeRef(m.Field.Type, currentEnv)
					}
					fieldTypes[m.Field.Name] = ft
				}
			}
			// predeclare methods so they are available during translation
			for _, m := range st.Type.Members {
				if m.Method != nil {
					params := []types.Type{types.StructType{Name: st.Type.Name}}
					for _, p := range m.Method.Params {
						if p.Type != nil {
							params = append(params, types.ResolveTypeRef(p.Type, currentEnv))
						} else {
							params = append(params, types.AnyType{})
						}
					}
					var ret types.Type = types.VoidType{}
					if m.Method.Return != nil {
						ret = types.ResolveTypeRef(m.Method.Return, currentEnv)
					}
					ft := types.FuncType{Params: params, Return: ret}
					methods[m.Method.Name] = types.Method{Decl: m.Method, Type: ft}
					currentEnv.SetVar(m.Method.Name, ft, true)
				}
			}
			stInfo := types.StructType{Name: st.Type.Name, Fields: fieldTypes, Order: fields, Methods: methods}
			currentEnv.SetStruct(st.Type.Name, stInfo)
			for _, m := range st.Type.Members {
				if m.Method != nil {
					fn, err := convertMethodStmt(m.Method, st.Type.Name, fields)
					if err != nil {
						return nil, err
					}
					ms.Stmts = append(ms.Stmts, fn)
				}
			}
		}
		if len(ms.Stmts) == 0 {
			return nil, nil
		}
		return ms, nil
	case st.ExternType != nil:
		return nil, nil
	case st.ExternVar != nil:
		return nil, nil
	case st.ExternFun != nil:
		return nil, nil
	case st.ExternObject != nil:
		return nil, nil
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
			currentEnv.SetVar(st.Let.Name, types.ListType{Elem: types.AnyType{}}, false)
			local := funcDepth > 0
			return &QueryAssignStmt{Name: st.Let.Name, Query: qc, Local: local}, nil
		}
		if currentEnv != nil {
			if st.Let.Type != nil {
				currentEnv.SetVar(st.Let.Name, types.ResolveTypeRef(st.Let.Type, currentEnv), false)
			} else if st.Let.Value != nil {
				currentEnv.SetVar(st.Let.Name, types.ExprType(st.Let.Value, currentEnv), false)
			} else {
				currentEnv.SetVar(st.Let.Name, exprType(expr), false)
			}
		}
		local := funcDepth > 0
		return &AssignStmt{Name: st.Let.Name, Value: expr, Local: local}, nil
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
			currentEnv.SetVar(st.Var.Name, types.ListType{Elem: types.AnyType{}}, false)
			local := funcDepth > 0
			return &QueryAssignStmt{Name: st.Var.Name, Query: qc, Local: local}, nil
		}
		if currentEnv != nil {
			if st.Var.Type != nil {
				currentEnv.SetVar(st.Var.Name, types.ResolveTypeRef(st.Var.Type, currentEnv), false)
			} else if st.Var.Value != nil {
				currentEnv.SetVar(st.Var.Name, types.ExprType(st.Var.Value, currentEnv), false)
			} else {
				currentEnv.SetVar(st.Var.Name, exprType(expr), false)
			}
		}
		local := funcDepth > 0
		return &AssignStmt{Name: st.Var.Name, Value: expr, Local: local}, nil
	case st.Assign != nil:
		expr, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		// Build a postfix expression representing the assignment target.
		p := &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: st.Assign.Name}}}
		for _, idx := range st.Assign.Index {
			p.Ops = append(p.Ops, &parser.PostfixOp{Index: idx})
		}
		for _, f := range st.Assign.Field {
			p.Ops = append(p.Ops, &parser.PostfixOp{Field: f})
		}
		target, err := convertPostfix(p)
		if err != nil {
			return nil, err
		}
		if id, ok := target.(*Ident); ok {
			if qc, ok := expr.(*QueryComp); ok {
				return &QueryAssignStmt{Name: id.Name, Query: qc}, nil
			}
			return &AssignStmt{Name: id.Name, Value: expr}, nil
		}
		return &IndexAssignStmt{Target: target, Value: expr}, nil
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
	case st.Update != nil:
		return convertUpdateStmt(st.Update)
	case st.Bench != nil:
		return convertBenchStmt(st.Bench)
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
func Transpile(prog *parser.Program, env *types.Env, benchMain bool) (*Program, error) {
	currentEnv = env
	// reset per-program state to avoid leaking values between compilations
	loopCounter = 0
	continueLabels = nil
	structCount = 0
	funcDepth = 0
	currentStruct = ""
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
	if benchMain || benchMainFlag {
		lp.Stmts = []Stmt{&BenchStmt{Name: "main", Body: lp.Stmts}}
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
	case *BenchStmt:
		{
			n := &ast.Node{Kind: "bench", Value: st.Name}
			body := &ast.Node{Kind: "body"}
			for _, s2 := range st.Body {
				body.Children = append(body.Children, stmtNode(s2))
			}
			n.Children = append(n.Children, body)
			return n
		}
	case *UpdateStmt:
		n := &ast.Node{Kind: "update", Value: st.Target}
		for i, f := range st.Fields {
			assign := &ast.Node{Kind: "field", Value: f, Children: []*ast.Node{exprNode(st.Values[i])}}
			n.Children = append(n.Children, assign)
		}
		if st.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "where", Children: []*ast.Node{exprNode(st.Cond)}})
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
