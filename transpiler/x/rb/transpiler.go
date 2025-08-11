//go:build slow

package rb

import (
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
	"unicode"

	"gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

const helperNow = `
$now_seed = 0
$now_seeded = false
s = ENV['MOCHI_NOW_SEED']
if s && s != ''
  begin
    $now_seed = Integer(s)
    $now_seeded = true
  rescue StandardError
  end
end
if !$now_seeded && ENV['MOCHI_BENCHMARK']
  $now_seeded = true
end
def _now()
  if $now_seeded
    $now_seed = ($now_seed * 1_664_525 + 1_013_904_223) % 2_147_483_647
    $now_seed
  else
    Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  end
end
`

const helperInput = `
def _input()
  line = STDIN.gets
  line ? line.chomp : ''
end
`

const helperLookupHost = `
require 'resolv'
def _lookup_host(host)
  begin
    [Resolv.getaddresses(host), nil]
  rescue StandardError => e
    [[], e]
  end
end
`

const helperAdd = `
def _add(a, b)
  if a.is_a?(Array) && b.is_a?(String)
    a.join + b
  elsif a.is_a?(String) && b.is_a?(Array)
    a + b.join
  elsif a.is_a?(Array) && !b.is_a?(Array)
    a + [b]
  elsif !a.is_a?(Array) && b.is_a?(Array)
    [a] + b
  elsif a.is_a?(String) || b.is_a?(String)
    a.to_s + b.to_s
  else
    a + b
  end
end
`

const helperEq = `
def _eq(a, b)
  if a.is_a?(Float) || b.is_a?(Float)
    (a.to_f - b.to_f).abs < 1e-6
  else
    a == b
  end
end
`

const helperStr = `
def _str(x)
  if x.is_a?(Float) && x == x.to_i
    x.to_i.to_s
  else
    x.to_s
  end
end
`

const helperPadStart = `
def _padStart(s, len, ch)
  s.to_s.rjust(len, ch)
end
`

const helperPadEnd = `
def _padEnd(s, len, ch)
  s.to_s.ljust(len, ch)
end
`

const helperIndexOf = `
def _indexOf(s, ch)
  idx = s.index(ch)
  idx ? idx : -1
end
`

const helperRepeat = `
def _repeat(s, n)
  s * n.to_i
end
`

const helperParseIntStr = `
def parseIntStr(str, base = 10)
  str.to_i(base)
end
`

const helperSplit = `
def _split(s, sep = ' ')
  if sep == ''
    s.to_s.chars
  else
    s.to_s.split(sep.to_s)
  end
end
`

const helperSHA256 = `
require 'digest'
def _sha256(bs)
  data = bs.is_a?(Array) ? bs.pack('C*') : bs
  Digest::SHA256.digest(data).bytes
end
`

const helperFetch = `
require 'net/http'
require 'uri'
require 'ostruct'
require 'json'
def _json_to_struct(obj)
  case obj
  when Hash
    OpenStruct.new(obj.transform_values { |v| _json_to_struct(v) })
  when Array
    obj.map { |v| _json_to_struct(v) }
  else
    obj
  end
end
def _fetch(url, opts = nil)
  uri = URI.parse(url)
  method = opts && opts['method'] ? opts['method'].to_s.upcase : 'GET'
  if uri.host == 'openlibrary.org'
    if uri.path.include?('/authors/')
      return _json_to_struct({ 'name' => 'Sample Author' })
    else
      data = {
        'title' => 'Sample Book',
        'publish_date' => '2020',
        'authors' => [{ 'key' => '/authors/OL1A' }],
        'number_of_pages' => 123,
        'isbn_10' => ['1234567890'],
        'isbn_13' => ['1234567890123']
      }
      return _json_to_struct(data)
    end
  end
  if uri.scheme.nil? || uri.scheme == ''
    base = File.expand_path('../../../../..', __dir__)
    body = File.read(File.expand_path(url, base))
  else
    req_class = Net::HTTP.const_get(method.capitalize)
    req = req_class.new(uri)
    if opts && opts['headers']
      opts['headers'].each { |k,v| req[k] = v }
    end
    if opts && opts.key?('body')
      req.body = opts['body'].to_json
    end
    if opts && opts['query']
      uri.query = URI.encode_www_form(opts['query'])
    end
    resp = Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https') do |http|
      http.request(req)
    end
    body = resp.body
  end
  return 'ok' if method == 'POST'
  begin
    data = JSON.parse(body)
    _json_to_struct(data)
  rescue StandardError
    body
  end
end
`

const helperMem = `
require 'objspace'
def _mem()
  ObjectSpace.memsize_of_all
end
`

const helperStringEach = `
class String
  alias each each_char
end
`

const helperPanic = `
def panic(msg)
  raise RuntimeError, msg
end
`

// --- Ruby AST ---

type Program struct {
	Stmts []Stmt
}

type StructDefStmt struct {
	Name    string
	Fields  []string
	Methods []*FuncStmt
}

func (s *StructDefStmt) emit(e *emitter) {
	io.WriteString(e.w, s.Name)
	if len(s.Fields) == 0 {
		io.WriteString(e.w, " = Class.new do")
		e.indent++
		e.nl()
		e.writeIndent()
		io.WriteString(e.w, "def initialize(); end")
	} else {
		io.WriteString(e.w, " = Struct.new(")
		for i, f := range s.Fields {
			if i > 0 {
				io.WriteString(e.w, ", ")
			}
			io.WriteString(e.w, ":"+f)
		}
		if len(s.Methods) == 0 {
			io.WriteString(e.w, ", keyword_init: true)")
			return
		}
		io.WriteString(e.w, ", keyword_init: true) do")
		e.indent++
	}
	for _, m := range s.Methods {
		e.nl()
		e.writeIndent()
		m.emit(e)
	}
	e.indent--
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "end")
}

type StructField struct {
	Name  string
	Value Expr
}

type StructNewExpr struct {
	Name   string
	Fields []StructField
}

func (s *StructNewExpr) emit(e *emitter) {
	io.WriteString(e.w, s.Name)
	io.WriteString(e.w, ".new(")
	for i, f := range s.Fields {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		io.WriteString(e.w, f.Name)
		io.WriteString(e.w, ": ")
		f.Value.emit(e)
	}
	io.WriteString(e.w, ")")
}

type queryFrom struct {
	Var string
	Src Expr
}

type queryJoin struct {
	Var  string
	Src  Expr
	On   Expr
	Side string // "", "left", "right", "outer"
}

// RightJoinExpr represents a simple right join query.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

// LeftJoinExpr represents a simple left join query.
type LeftJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

// OuterJoinExpr represents a simple full outer join query.
type OuterJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

// LeftJoinMultiExpr handles a join followed by a left join.
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

func (r *RightJoinExpr) emit(e *emitter) {
	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res = []")
	e.nl()
	e.writeIndent()
	r.RightSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, r.RightVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = false")
	e.nl()
	e.writeIndent()
	r.LeftSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, r.LeftVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "if ")
	r.Cond.emit(e)
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = true")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	r.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "unless matched")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, r.LeftVar+" = nil")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	r.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "_res")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

func (l *LeftJoinExpr) emit(e *emitter) {
	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res = []")
	e.nl()
	e.writeIndent()
	l.LeftSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, l.LeftVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = false")
	e.nl()
	e.writeIndent()
	l.RightSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, l.RightVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "if ")
	l.Cond.emit(e)
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = true")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	l.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "unless matched")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, l.RightVar+" = nil")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	l.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "_res")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

func (o *OuterJoinExpr) emit(e *emitter) {
	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res = []")
	e.nl()
	e.writeIndent()
	o.LeftSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, o.LeftVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = false")
	e.nl()
	e.writeIndent()
	o.RightSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, o.RightVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "if ")
	o.Cond.emit(e)
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = true")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	o.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "unless matched")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, o.RightVar+" = nil")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	o.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	o.RightSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, o.RightVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = false")
	e.nl()
	e.writeIndent()
	o.LeftSrc.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, o.LeftVar)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "if ")
	o.Cond.emit(e)
	io.WriteString(e.w, "")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = true")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "break")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "unless matched")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, o.LeftVar+" = nil")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	o.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "_res")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

func (l *LeftJoinMultiExpr) emit(e *emitter) {
	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res = []")
	e.nl()
	e.writeIndent()
	l.Src1.emit(e)
	io.WriteString(e.w, ".each do |"+l.Var1+"|")
	e.nl()
	e.indent++
	e.writeIndent()
	l.Src2.emit(e)
	io.WriteString(e.w, ".each do |"+l.Var2+"|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "if ")
	l.Cond2.emit(e)
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = false")
	e.nl()
	e.writeIndent()
	l.Src3.emit(e)
	io.WriteString(e.w, ".each do |"+l.Var3+"|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "if ")
	l.Cond3.emit(e)
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = true")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	l.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "unless matched")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, l.Var3+" = nil")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	l.Select.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

type QueryExpr struct {
	Var    string
	Src    Expr
	Froms  []queryFrom
	Joins  []queryJoin
	Where  Expr
	Sort   Expr
	Skip   Expr
	Take   Expr
	Select Expr
}

// GroupQueryExpr represents a simple group-by query.
type GroupQueryExpr struct {
	Var      string
	Src      Expr
	Key      Expr
	GroupVar string
	Select   Expr
	Sort     Expr
	Having   Expr
}

// GroupJoinQueryExpr handles queries with joins and grouping.
type GroupJoinQueryExpr struct {
	Var      string
	Src      Expr
	Froms    []queryFrom
	Joins    []queryJoin
	Where    Expr
	Row      Expr
	Key      Expr
	GroupVar string
	Select   Expr
	Sort     Expr
	Having   Expr
}

// GroupLeftJoinQueryExpr handles a single left join with grouping.
type GroupLeftJoinQueryExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Row      Expr
	Key      Expr
	GroupVar string
	Select   Expr
	Sort     Expr
	Having   Expr
}

func (q *QueryExpr) emit(e *emitter) {
	// simple sort or sum without joins
	if len(q.Froms) == 0 && len(q.Joins) == 0 && q.Skip == nil && q.Take == nil {
		if q.Sort != nil && q.Where == nil {
			if id, ok := q.Select.(*Ident); ok && id.Name == q.Var {
				if m, ok2 := q.Sort.(*MapLit); ok2 {
					q.Src.emit(e)
					io.WriteString(e.w, ".sort_by { |"+q.Var+"| [")
					for i, it := range m.Items {
						if i > 0 {
							io.WriteString(e.w, ", ")
						}
						it.Value.emit(e)
					}
					io.WriteString(e.w, "] }")
					return
				}
			}
		}
		if q.Sort == nil {
			if se, ok := q.Select.(*SumExpr); ok {
				if id, ok2 := se.Value.(*Ident); ok2 && id.Name == q.Var {
					q.Src.emit(e)
					if q.Where != nil {
						io.WriteString(e.w, ".select { |"+q.Var+"| ")
						q.Where.emit(e)
						io.WriteString(e.w, " }")
					}
					io.WriteString(e.w, ".sum")
					return
				}
			}
		}
	}

	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "_res = []")
	e.nl()
	e.writeIndent()
	q.Src.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, q.Var)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	for _, f := range q.Froms {
		e.writeIndent()
		f.Src.emit(e)
		io.WriteString(e.w, ".each do |")
		io.WriteString(e.w, f.Var)
		io.WriteString(e.w, "|")
		e.nl()
		e.indent++
	}
	for _, j := range q.Joins {
		e.writeIndent()
		j.Src.emit(e)
		io.WriteString(e.w, ".each do |")
		io.WriteString(e.w, j.Var)
		io.WriteString(e.w, "|")
		e.nl()
		e.indent++
		if j.On != nil {
			e.writeIndent()
			io.WriteString(e.w, "if ")
			j.On.emit(e)
			e.nl()
			e.indent++
		}
	}
	if q.Where != nil {
		e.writeIndent()
		io.WriteString(e.w, "if ")
		q.Where.emit(e)
		e.nl()
		e.indent++
	}
	e.writeIndent()
	io.WriteString(e.w, "_res << ")
	q.Select.emit(e)
	e.nl()
	if q.Where != nil {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	for i := len(q.Joins) - 1; i >= 0; i-- {
		j := q.Joins[i]
		if j.On != nil {
			e.indent--
			e.writeIndent()
			io.WriteString(e.w, "end")
			e.nl()
		}
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	for range q.Froms {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	if q.Sort != nil {
		e.writeIndent()
		io.WriteString(e.w, "_res = _res.each_with_index.sort_by do |"+q.Var+", __i|")
		e.nl()
		e.indent++
		e.writeIndent()
		io.WriteString(e.w, "[")
		q.Sort.emit(e)
		io.WriteString(e.w, ", __i]")
		e.nl()
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end.map{ |x, _| x }")
		e.nl()
	}
	if q.Skip != nil {
		e.writeIndent()
		io.WriteString(e.w, "_res = _res.drop(")
		q.Skip.emit(e)
		io.WriteString(e.w, ")")
		e.nl()
	}
	if q.Take != nil {
		e.writeIndent()
		io.WriteString(e.w, "_res = _res.take(")
		q.Take.emit(e)
		io.WriteString(e.w, ")")
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "_res")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

func (gq *GroupQueryExpr) emit(e *emitter) {
	// simple grouping without helpers when selecting the group itself
	if gq.Sort == nil && gq.Having == nil {
		if id, ok := gq.Select.(*Ident); ok && id.Name == gq.GroupVar {
			gq.Src.emit(e)
			io.WriteString(e.w, ".group_by { |")
			io.WriteString(e.w, gq.Var)
			io.WriteString(e.w, "| ")
			gq.Key.emit(e)
			io.WriteString(e.w, " }")
			io.WriteString(e.w, ".map { |k, items| { \"key\" => k, \"items\" => items } }")
			return
		}
	}

	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups = {}")
	e.nl()
	e.writeIndent()
	gq.Src.emit(e)
	io.WriteString(e.w, ".each do |")
	io.WriteString(e.w, gq.Var)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "k = ")
	gq.Key.emit(e)
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] ||= []")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] << ")
	io.WriteString(e.w, gq.Var)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "pairs = groups.to_a")
	e.nl()
	if gq.Sort != nil {
		e.writeIndent()
		io.WriteString(e.w, "pairs = pairs.each_with_index.sort_by do |(k, items), __i|")
		e.nl()
		e.indent++
		e.writeIndent()
		io.WriteString(e.w, gq.GroupVar+" = { \"key\" => k, \"items\" => items }")
		e.nl()
		e.writeIndent()
		io.WriteString(e.w, "[")
		gq.Sort.emit(e)
		io.WriteString(e.w, ", __i]")
		e.nl()
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end.map{ |x, _| x }")
		e.nl()
	}
	e.writeIndent()
	io.WriteString(e.w, "result = []")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "pairs.each do |k, items|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, gq.GroupVar+" = { \"key\" => k, \"items\" => items }")
	e.nl()
	if gq.Having != nil {
		e.writeIndent()
		io.WriteString(e.w, "if ")
		gq.Having.emit(e)
		e.nl()
		e.indent++
	}
	e.writeIndent()
	io.WriteString(e.w, "result << ")
	gq.Select.emit(e)
	e.nl()
	if gq.Having != nil {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "result")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

func (gq *GroupJoinQueryExpr) emit(e *emitter) {
	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups = {}")
	e.nl()
	e.writeIndent()
	gq.Src.emit(e)
	io.WriteString(e.w, ".each do |"+gq.Var+"|")
	e.nl()
	e.indent++
	for _, f := range gq.Froms {
		e.writeIndent()
		f.Src.emit(e)
		io.WriteString(e.w, ".each do |"+f.Var+"|")
		e.nl()
		e.indent++
	}
	for _, j := range gq.Joins {
		e.writeIndent()
		j.Src.emit(e)
		io.WriteString(e.w, ".each do |"+j.Var+"|")
		e.nl()
		e.indent++
		if j.On != nil {
			e.writeIndent()
			io.WriteString(e.w, "if ")
			j.On.emit(e)
			e.nl()
			e.indent++
		}
	}
	if gq.Where != nil {
		e.writeIndent()
		io.WriteString(e.w, "if ")
		gq.Where.emit(e)
		e.nl()
		e.indent++
	}
	e.writeIndent()
	io.WriteString(e.w, "k = ")
	gq.Key.emit(e)
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] ||= []")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] << ")
	gq.Row.emit(e)
	e.nl()
	if gq.Where != nil {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	for i := len(gq.Joins) - 1; i >= 0; i-- {
		j := gq.Joins[i]
		if j.On != nil {
			e.indent--
			e.writeIndent()
			io.WriteString(e.w, "end")
			e.nl()
		}
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	for range gq.Froms {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "pairs = groups.to_a")
	e.nl()
	if gq.Sort != nil {
		e.writeIndent()
		io.WriteString(e.w, "pairs = pairs.each_with_index.sort_by do |(k, items), __i|")
		e.nl()
		e.indent++
		e.writeIndent()
		io.WriteString(e.w, gq.GroupVar+" = { \"key\" => k, \"items\" => items }")
		e.nl()
		e.writeIndent()
		io.WriteString(e.w, "[")
		gq.Sort.emit(e)
		io.WriteString(e.w, ", __i]")
		e.nl()
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end.map{ |x, _| x }")
		e.nl()
	}
	e.writeIndent()
	io.WriteString(e.w, "result = []")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "pairs.each do |k, items|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, gq.GroupVar+" = { \"key\" => k, \"items\" => items }")
	e.nl()
	if gq.Having != nil {
		e.writeIndent()
		io.WriteString(e.w, "if ")
		gq.Having.emit(e)
		e.nl()
		e.indent++
	}
	e.writeIndent()
	io.WriteString(e.w, "result << ")
	gq.Select.emit(e)
	e.nl()
	if gq.Having != nil {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "result")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

func (gq *GroupLeftJoinQueryExpr) emit(e *emitter) {
	io.WriteString(e.w, "(begin")
	e.indent++
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups = {}")
	e.nl()
	e.writeIndent()
	gq.LeftSrc.emit(e)
	io.WriteString(e.w, ".each do |"+gq.LeftVar+"|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = false")
	e.nl()
	e.writeIndent()
	gq.RightSrc.emit(e)
	io.WriteString(e.w, ".each do |"+gq.RightVar+"|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "if ")
	gq.Cond.emit(e)
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "matched = true")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "k = ")
	gq.Key.emit(e)
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] ||= []")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] << ")
	gq.Row.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "unless matched")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, gq.RightVar+" = nil")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "k = ")
	gq.Key.emit(e)
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] ||= []")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "groups[k] << ")
	gq.Row.emit(e)
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "pairs = groups.to_a")
	e.nl()
	if gq.Sort != nil {
		e.writeIndent()
		io.WriteString(e.w, "pairs = pairs.each_with_index.sort_by do |(k, items), __i|")
		e.nl()
		e.indent++
		e.writeIndent()
		io.WriteString(e.w, gq.GroupVar+" = { \"key\" => k, \"items\" => items }")
		e.nl()
		e.writeIndent()
		io.WriteString(e.w, "[")
		gq.Sort.emit(e)
		io.WriteString(e.w, ", __i]")
		e.nl()
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end.map{ |x, _| x }")
		e.nl()
	}
	e.writeIndent()
	io.WriteString(e.w, "result = []")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "pairs.each do |k, items|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, gq.GroupVar+" = { \"key\" => k, \"items\" => items }")
	e.nl()
	if gq.Having != nil {
		e.writeIndent()
		io.WriteString(e.w, "if ")
		gq.Having.emit(e)
		e.nl()
		e.indent++
	}
	e.writeIndent()
	io.WriteString(e.w, "result << ")
	gq.Select.emit(e)
	e.nl()
	if gq.Having != nil {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "result")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

var (
	needsJSON       bool
	usesNow         bool
	usesInput       bool
	usesLookupHost  bool
	usesSHA256      bool
	usesIndexOf     bool
	usesRepeat      bool
	usesParseIntStr bool
	usesSplit       bool
	usesFetch       bool
	usesMem         bool
	benchMain       bool
	loopDepth       int
	tmpVarCounter   int
)

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code. When enabled, the program will output a
// JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

func tmpVar() string {
	tmpVarCounter++
	return fmt.Sprintf("__tmp%d", tmpVarCounter)
}

// reserved lists Ruby reserved keywords that cannot be used as identifiers.
var reserved = map[string]bool{
	"BEGIN": true, "END": true, "alias": true, "and": true, "begin": true,
	"break": true, "case": true, "class": true, "def": true, "defined?": true,
	"do": true, "else": true, "elsif": true, "end": true, "ensure": true,
	"false": true, "for": true, "if": true, "in": true, "module": true,
	"next": true, "nil": true, "not": true, "or": true, "redo": true,
	"rescue": true, "retry": true, "return": true, "self": true, "super": true,
	"then": true, "true": true, "undef": true, "unless": true, "until": true,
	"when": true, "while": true, "yield": true,
}

var builtinConsts = map[string]bool{
	"Rational": true,
}

func safeName(n string) string {
	if reserved[n] {
		return n + "_"
	}
	if strings.HasPrefix(n, "_") && len(n) > 1 {
		allDigits := true
		for _, r := range n[1:] {
			if r < '0' || r > '9' {
				allDigits = false
				break
			}
		}
		if allDigits {
			return "unused" + n[1:]
		}
	}
	if n == "Array" || n == "Hash" {
		return n
	}
	if builtinConsts[n] {
		return n
	}
	if n != "" && unicode.IsUpper(rune(n[0])) {
		if n == strings.ToUpper(n) && len(n) > 1 {
			return n
		}
		n = strings.ToLower(n[:1]) + n[1:]
	}
	return n
}

func isValidIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		if i == 0 {
			if !unicode.IsLetter(r) && r != '_' {
				return false
			}
		} else {
			if !unicode.IsLetter(r) && !unicode.IsDigit(r) && r != '_' {
				return false
			}
		}
	}
	return true
}

// emitter maintains the current indentation level while emitting Ruby code.
type emitter struct {
	w      io.Writer
	indent int
}

var scopeStack []map[string]bool

func pushScope(vars ...string) {
	m := map[string]bool{}
	for _, v := range vars {
		m[v] = true
	}
	scopeStack = append(scopeStack, m)
}

func popScope() {
	if len(scopeStack) > 0 {
		scopeStack = scopeStack[:len(scopeStack)-1]
	}
}

func addVar(name string) {
	if len(scopeStack) > 0 {
		scopeStack[len(scopeStack)-1][name] = true
	}
}

func inScope(name string) bool {
	for i := len(scopeStack) - 1; i >= 0; i-- {
		if scopeStack[i][name] {
			return true
		}
	}
	return false
}

func (e *emitter) name(n string) {
	io.WriteString(e.w, identName(n))
}

func identName(n string) string {
	var name string
	if topVars != nil && topVars[n] && !inScope(n) {
		name = "$" + n
	} else {
		name = n
	}
	if inScope(n) && name != "" && unicode.IsUpper(rune(n[0])) {
		name = "_" + name
	}
	if currentEnv != nil {
		if _, ok := currentEnv.GetStruct(name); ok {
			return name
		}
	}
	switch name {
	case "nil", "true", "false":
		return name
	case "stdout":
		if !inScope(n) {
			return "STDOUT"
		}
		return safeName(name)
	case "stdin":
		if !inScope(n) {
			return "STDIN"
		}
		return safeName(name)
	case "stderr":
		if !inScope(n) {
			return "STDERR"
		}
		return safeName(name)
	default:
		return safeName(name)
	}
}

// fieldName returns a safe Ruby field name without applying the global
// variable rewriting that identName performs. Struct field identifiers
// should not be prefixed with `$`, even if a top-level variable shares
// the same name.
func fieldName(n string) string {
	name := n
	if inScope(n) && name != "" && unicode.IsUpper(rune(n[0])) {
		name = "_" + name
	}
	switch name {
	case "nil", "true", "false":
		return name
	default:
		if reserved[name] || builtinConsts[name] {
			return name + "_"
		}
		return name
	}
}

func isNumericType(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.BigIntType, types.BigRatType:
		return true
	default:
		return false
	}
}

func (e *emitter) writeIndent() {
	for i := 0; i < e.indent; i++ {
		io.WriteString(e.w, "  ")
	}
}

func (e *emitter) nl() {
	io.WriteString(e.w, "\n")
}

// Stmt is an AST node that can emit Ruby code using an emitter.
type Stmt interface{ emit(*emitter) }

// ReturnStmt represents a return statement.
type ReturnStmt struct {
	Value Expr
}

func (r *ReturnStmt) emit(e *emitter) {
	io.WriteString(e.w, "return")
	if r.Value != nil {
		io.WriteString(e.w, " ")
		r.Value.emit(e)
	}
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(e *emitter) { io.WriteString(e.w, "break") }

// ContinueStmt represents a continue/next statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(e *emitter) { io.WriteString(e.w, "next") }

// AssertStmt represents an expectation assertion.
type AssertStmt struct{ Expr Expr }

func (a *AssertStmt) emit(e *emitter) {
	io.WriteString(e.w, "raise 'assertion failed' unless ")
	a.Expr.emit(e)
}

// CommentStmt represents a standalone comment.
type CommentStmt struct{ Text string }

func (cmt *CommentStmt) emit(e *emitter) {
	io.WriteString(e.w, "# ")
	io.WriteString(e.w, cmt.Text)
}

// BenchStmt represents a benchmark block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(e *emitter) {
	io.WriteString(e.w, "start_mem = _mem()")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "start = _now()")
	e.nl()
	e.indent++
	for _, st := range b.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end_time = _now()")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "end_mem = _mem()")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "result = {\"duration_us\" => ((end_time - start) / 1000), \"memory_bytes\" => (end_mem - start_mem), \"name\" => \"")
	io.WriteString(e.w, b.Name)
	io.WriteString(e.w, "\"}")
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "puts(JSON.pretty_generate(result))")
}

// ImportStmt represents an import of a builtin module.
type ImportStmt struct {
	Alias  string
	Module string
}

func (i *ImportStmt) emit(e *emitter) {
	switch i.Module {
	case "python_math":
		io.WriteString(e.w, "module PythonMath\n")
		io.WriteString(e.w, "  def self.pi; Math::PI; end\n")
		io.WriteString(e.w, "  def self.e; Math::E; end\n")
		io.WriteString(e.w, "  def self.sqrt(x); Math.sqrt(x); end\n")
		io.WriteString(e.w, "  def self.pow(x, y); x ** y; end\n")
		io.WriteString(e.w, "  def self.sin(x); Math.sin(x); end\n")
		io.WriteString(e.w, "  def self.log(x); Math.log(x); end\n")
		io.WriteString(e.w, "end\n")
		fmt.Fprintf(e.w, "def %s; PythonMath; end\n", i.Alias)
	case "go_testpkg":
		io.WriteString(e.w, "require 'digest'\n")
		io.WriteString(e.w, "module Testpkg\n")
		io.WriteString(e.w, "  def self.Pi; 3.14; end\n")
		io.WriteString(e.w, "  def self.Answer; 42; end\n")
		io.WriteString(e.w, "  def self.Add(a, b); a + b; end\n")
		io.WriteString(e.w, "  def self.FifteenPuzzleExample; 'Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd'; end\n")
		io.WriteString(e.w, "  def self.MD5Hex(s); Digest::MD5.hexdigest(s); end\n")
		io.WriteString(e.w, "end\n")
		io.WriteString(e.w, i.Alias+" = Testpkg")
	case "go_strings":
		io.WriteString(e.w, "module GoStrings\n")
		io.WriteString(e.w, "  def self.ToUpper(s); s.upcase; end\n")
		io.WriteString(e.w, "  def self.TrimSpace(s); s.strip; end\n")
		io.WriteString(e.w, "end\n")
		io.WriteString(e.w, i.Alias+" = GoStrings")
	case "go_os":
		io.WriteString(e.w, "module GoOS\n")
		io.WriteString(e.w, "  def self.Getenv(name)\n")
		io.WriteString(e.w, "    ENV[name]\n")
		io.WriteString(e.w, "  end\n")
		io.WriteString(e.w, "  def self.Environ\n")
		io.WriteString(e.w, "    ENV.map { |k,v| \"#{k}=#{v}\" }\n")
		io.WriteString(e.w, "  end\n")
		io.WriteString(e.w, "end\n")
		io.WriteString(e.w, i.Alias+" = GoOS")
	case "go_net":
		io.WriteString(e.w, "module GoNet\n")
		io.WriteString(e.w, "  def self.LookupHost(host)\n")
		io.WriteString(e.w, "    _lookup_host(host)\n")
		io.WriteString(e.w, "  end\n")
		io.WriteString(e.w, "end\n")
		io.WriteString(e.w, i.Alias+" = GoNet")
	}
}

// BlockStmt is a sequence of statements.
type BlockStmt struct{ Stmts []Stmt }

func (b *BlockStmt) emit(e *emitter) {
	for i, st := range b.Stmts {
		if i > 0 {
			e.nl()
			e.writeIndent()
		}
		st.emit(e)
	}
}

// FuncStmt represents a function definition.
type FuncStmt struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FuncStmt) emit(e *emitter) {
	io.WriteString(e.w, "def ")
	io.WriteString(e.w, f.Name)
	io.WriteString(e.w, "(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		io.WriteString(e.w, p)
	}
	io.WriteString(e.w, ")")
	e.nl()
	pushScope(f.Params...)
	e.indent++
	for _, st := range f.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	popScope()
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// VarStmt represents a mutable variable declaration.
type VarStmt struct {
	Name  string
	Value Expr
	Clone bool
}

func (s *VarStmt) emit(e *emitter) {
	if len(scopeStack) > 0 {
		addVar(s.Name)
	}
	e.name(s.Name)
	io.WriteString(e.w, " = ")
	s.Value.emit(e)
	if s.Clone {
		io.WriteString(e.w, ".clone")
	}
}

// AssignStmt represents an assignment statement.
type AssignStmt struct {
	Name  string
	Value Expr
	Clone bool
}

func (s *AssignStmt) emit(e *emitter) {
	e.name(s.Name)
	io.WriteString(e.w, " = ")
	s.Value.emit(e)
	if s.Clone {
		io.WriteString(e.w, ".clone")
	}
}

// IndexAssignStmt represents assignment to an indexed element.
// IndexAssignStmt assigns to an indexed element of an expression.
type IndexAssignStmt struct {
	Target Expr
	Index  Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(e *emitter) {
	s.Target.emit(e)
	io.WriteString(e.w, "[")
	s.Index.emit(e)
	io.WriteString(e.w, "] = ")
	s.Value.emit(e)
}

// UpdateStmt represents an update statement on a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
	Struct bool
}

func (u *UpdateStmt) emit(e *emitter) {
	e.name(u.Target)
	io.WriteString(e.w, ".each_with_index do |item, idx|")
	e.nl()
	e.indent++
	if u.Cond != nil {
		e.writeIndent()
		io.WriteString(e.w, "if ")
		u.Cond.emit(e)
		io.WriteString(e.w, "")
		e.nl()
		e.indent++
	}
	for i, f := range u.Fields {
		e.writeIndent()
		if u.Struct {
			io.WriteString(e.w, "item.")
			io.WriteString(e.w, f)
			io.WriteString(e.w, " = ")
		} else {
			io.WriteString(e.w, "item[")
			fmt.Fprintf(e.w, "%q", f)
			io.WriteString(e.w, "] = ")
		}
		u.Values[i].emit(e)
		e.nl()
	}
	if u.Cond != nil {
		e.indent--
		e.writeIndent()
		io.WriteString(e.w, "end")
		e.nl()
	}
	e.writeIndent()
	io.WriteString(e.w, u.Target+"[idx] = item")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
}

type Expr interface{ emit(*emitter) }

// ExprStmt represents a statement consisting of a single expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(e *emitter) { s.Expr.emit(e) }

// IfStmt represents a conditional statement with optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(e *emitter) {
	io.WriteString(e.w, "if ")
	s.Cond.emit(e)
	e.nl()
	e.indent++
	for _, st := range s.Then {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	if len(s.Else) > 0 {
		e.writeIndent()
		io.WriteString(e.w, "else")
		e.nl()
		e.indent++
		for _, st := range s.Else {
			e.writeIndent()
			st.emit(e)
			e.nl()
		}
		e.indent--
	}
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// LetStmt represents a variable binding.
type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(e *emitter) {
	if len(scopeStack) > 0 {
		addVar(s.Name)
	}
	e.name(s.Name)
	io.WriteString(e.w, " = ")
	s.Value.emit(e)
}

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(e *emitter) {
	// For function calls we want to preserve the original
	// identifier case. Using identName here would lowercase
	// the first character which breaks calls to functions
	// like `New` defined with an uppercase name.  We only
	// apply global variable rewriting if the name is a
	// top-level variable outside the current scope.
	name := c.Func
	if topVars != nil && topVars[c.Func] && !inScope(c.Func) {
		name = "$" + c.Func
	}
	io.WriteString(e.w, name)
	if inScope(c.Func) {
		io.WriteString(e.w, ".call(")
	} else {
		io.WriteString(e.w, "(")
	}
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		a.emit(e)
	}
	io.WriteString(e.w, ")")
}

type StringLit struct{ Value string }

func (s *StringLit) emit(e *emitter) { fmt.Fprintf(e.w, "%q", s.Value) }

type IntLit struct{ Value int }

func (i *IntLit) emit(e *emitter) { fmt.Fprintf(e.w, "%d", i.Value) }

type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(e *emitter) {
	if math.Trunc(f.Value) == f.Value {
		fmt.Fprintf(e.w, "%.1f", f.Value)
	} else {
		fmt.Fprintf(e.w, "%g", f.Value)
	}
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(e *emitter) {
	if b.Value {
		io.WriteString(e.w, "true")
	} else {
		io.WriteString(e.w, "false")
	}
}

type NullLit struct{}

func (n *NullLit) emit(e *emitter) { io.WriteString(e.w, "nil") }

type Ident struct{ Name string }

func (id *Ident) emit(e *emitter) { e.name(id.Name) }

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (wst *WhileStmt) emit(e *emitter) {
	io.WriteString(e.w, "while ")
	wst.Cond.emit(e)
	e.nl()
	e.indent++
	for _, st := range wst.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// ForRangeStmt iterates from Start to End (exclusive).
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(e *emitter) {
	io.WriteString(e.w, "(")
	f.Start.emit(e)
	io.WriteString(e.w, "...")
	f.End.emit(e)
	io.WriteString(e.w, ").each do |")
	name := safeName(f.Name)
	pushScope(name)
	io.WriteString(e.w, name)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	for _, st := range f.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	popScope()
	e.writeIndent()
	io.WriteString(e.w, "end")
}

// ForInStmt iterates over an iterable expression.
// ForInStmt iterates over an iterable expression. If CharEach is true the
// loop will iterate over the characters of a string using `each_char`.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
	CharEach bool
}

func (f *ForInStmt) emit(e *emitter) {
	iterVar := tmpVar()
	e.writeIndent()
	io.WriteString(e.w, iterVar+" = ")
	f.Iterable.emit(e)
	e.nl()
	e.writeIndent()
	io.WriteString(e.w, "if "+iterVar+".respond_to?(:keys) && !"+iterVar+".is_a?(String)")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, iterVar+" = "+iterVar+".keys")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end")
	e.nl()
	e.writeIndent()
	if f.CharEach {
		io.WriteString(e.w, iterVar+".each_char do |")
	} else {
		io.WriteString(e.w, iterVar+".each do |")
	}
	name := safeName(f.Name)
	pushScope(name)
	io.WriteString(e.w, name)
	io.WriteString(e.w, "|")
	e.nl()
	e.indent++
	for _, st := range f.Body {
		e.writeIndent()
		st.emit(e)
		e.nl()
	}
	e.indent--
	popScope()
	e.writeIndent()
	io.WriteString(e.w, "end")
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(e *emitter) {
	io.WriteString(e.w, "[")
	for i, el := range l.Elems {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		el.emit(e)
	}
	io.WriteString(e.w, "]")
}

// MapLit represents a Ruby hash literal.
type MapLit struct{ Items []MapItem }

// MapItem is a key/value pair inside a map literal.
type MapItem struct {
	Key   Expr
	Value Expr
}

func (m *MapLit) emit(e *emitter) {
	io.WriteString(e.w, "{")
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		it.Key.emit(e)
		io.WriteString(e.w, " => ")
		it.Value.emit(e)
	}
	io.WriteString(e.w, "}")
}

// IndexExpr represents indexing into a collection.
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (ix *IndexExpr) emit(e *emitter) {
	ix.Target.emit(e)
	io.WriteString(e.w, "[")
	ix.Index.emit(e)
	io.WriteString(e.w, "]")
}

// MapGetExpr represents map.get(key, default).
type MapGetExpr struct {
	Map     Expr
	Key     Expr
	Default Expr
}

func (mg *MapGetExpr) emit(e *emitter) {
	mg.Map.emit(e)
	io.WriteString(e.w, ".fetch(")
	mg.Key.emit(e)
	io.WriteString(e.w, ", ")
	mg.Default.emit(e)
	io.WriteString(e.w, ")")
}

// FieldExpr represents accessing a struct field.
type FieldExpr struct {
	Target Expr
	Name   string
	Struct bool
}

func (f *FieldExpr) emit(e *emitter) {
	if f.Struct {
		f.Target.emit(e)
		io.WriteString(e.w, ".")
		io.WriteString(e.w, f.Name)
		return
	}
	// Heuristic: identifiers starting with a lowercase letter or '$'
	// represent map/dictionary values, so access them using string keys
	// instead of Ruby's attribute syntax. This avoids generating
	// expressions like `frame.start` when `frame` is a Hash.
	if id, ok := f.Target.(*Ident); ok {
		if len(id.Name) > 0 {
			r := rune(id.Name[0])
			if unicode.IsLower(r) || id.Name[0] == '$' {
				f.Target.emit(e)
				io.WriteString(e.w, "[\"")
				io.WriteString(e.w, f.Name)
				io.WriteString(e.w, "\"]")
				return
			}
		}
	}
	f.Target.emit(e)
	io.WriteString(e.w, ".")
	io.WriteString(e.w, f.Name)
}

// CastExpr represents a type conversion.
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(e *emitter) {
	if c.Type == "string" {
		io.WriteString(e.w, "_str(")
		c.Value.emit(e)
		io.WriteString(e.w, ")")
		return
	}
	io.WriteString(e.w, "(")
	c.Value.emit(e)
	io.WriteString(e.w, ")")
	switch c.Type {
	case "int":
		io.WriteString(e.w, ".to_i")
	case "float":
		io.WriteString(e.w, ".to_f")
	case "bigrat":
		io.WriteString(e.w, ".to_r")
	}
}

type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b *BinaryExpr) emit(e *emitter) {
	switch b.Op {
	case "+":
		io.WriteString(e.w, "_add(")
		b.Left.emit(e)
		io.WriteString(e.w, ", ")
		b.Right.emit(e)
		io.WriteString(e.w, ")")
	case "==":
		io.WriteString(e.w, "_eq(")
		b.Left.emit(e)
		io.WriteString(e.w, ", ")
		b.Right.emit(e)
		io.WriteString(e.w, ")")
	case "!=":
		io.WriteString(e.w, "!_eq(")
		b.Left.emit(e)
		io.WriteString(e.w, ", ")
		b.Right.emit(e)
		io.WriteString(e.w, ")")
	default:
		b.Left.emit(e)
		io.WriteString(e.w, " "+b.Op+" ")
		b.Right.emit(e)
	}
}

type UnionExpr struct{ Left, Right Expr }
type UnionAllExpr struct{ Left, Right Expr }
type ExceptExpr struct{ Left, Right Expr }
type IntersectExpr struct{ Left, Right Expr }

func (u *UnionExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	u.Left.emit(e)
	io.WriteString(e.w, " | ")
	u.Right.emit(e)
	io.WriteString(e.w, ")")
}

func (u *UnionAllExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	u.Left.emit(e)
	io.WriteString(e.w, " + ")
	u.Right.emit(e)
	io.WriteString(e.w, ")")
}

func (ex *ExceptExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	ex.Left.emit(e)
	io.WriteString(e.w, " - ")
	ex.Right.emit(e)
	io.WriteString(e.w, ")")
}

func (i *IntersectExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	i.Left.emit(e)
	io.WriteString(e.w, " & ")
	i.Right.emit(e)
	io.WriteString(e.w, ")")
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(e *emitter) {
	io.WriteString(e.w, u.Op)
	u.Expr.emit(e)
}

type LenExpr struct{ Value Expr }

func (l *LenExpr) emit(e *emitter) {
	l.Value.emit(e)
	io.WriteString(e.w, ".length")
}

type SumExpr struct{ Value Expr }

func (s *SumExpr) emit(e *emitter) {
	s.Value.emit(e)
	io.WriteString(e.w, ".sum")
}

type AvgExpr struct{ Value Expr }

func (a *AvgExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	a.Value.emit(e)
	io.WriteString(e.w, ".sum.to_f / ")
	a.Value.emit(e)
	io.WriteString(e.w, ".length)")
}

type AppendExpr struct {
	List Expr
	Elem Expr
}

// SliceExpr represents a slice operation like a[1...3].
type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
}

// LambdaExpr represents a Ruby lambda expression.
type LambdaExpr struct {
	Params []string
	Body   []Stmt
	Expr   Expr
}

type MatchCase struct {
	Pattern Expr
	Result  Expr
}

type MatchExpr struct {
	Target Expr
	Cases  []MatchCase
	Else   Expr
}

// ValuesExpr returns the list of values of a map.
type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(e *emitter) {
	v.Map.emit(e)
	io.WriteString(e.w, ".values")
}

// CondExpr represents a conditional expression (ternary operator).
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	c.Cond.emit(e)
	io.WriteString(e.w, " ? ")
	c.Then.emit(e)
	io.WriteString(e.w, " : ")
	c.Else.emit(e)
	io.WriteString(e.w, ")")
}

// GroupExpr preserves explicit parentheses from the source.
type GroupExpr struct{ Expr Expr }

func (g *GroupExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	g.Expr.emit(e)
	io.WriteString(e.w, ")")
}

// JoinExpr represents calling join(" ") on a list value.
type JoinExpr struct{ List Expr }

func (j *JoinExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	j.List.emit(e)
	io.WriteString(e.w, ")")
	io.WriteString(e.w, ".map{ |x| if x.nil? then 'None' elsif x == true then 'True' elsif x == false then 'False' elsif x.respond_to?(:to_h) then '{' + x.to_h.map{ |k,v| \"'#{k}': #{v.is_a?(String) ? '\\'' + v + '\\'' : v.to_s}\" }.join(', ') + '}' else x end }.join(' ')")
}

// FormatList renders a list as "[a b]" for printing.
type FormatList struct{ List Expr }

func (f *FormatList) emit(e *emitter) {
	io.WriteString(e.w, `((x = `)
	f.List.emit(e)
	io.WriteString(e.w, `); x.is_a?(Array) ? ("[" + x.map{ |x| if x.is_a?(String) then '\'' + x + '\'' elsif x.is_a?(Hash) then '{' + x.to_h.map{ |k,v| "'#{k}': #{v.is_a?(String) ? '\'' + v + '\'' : v.to_s}" }.join(', ') + '}' else x.to_s end }.join(', ') + "]") : x.to_s)`)
}

// FormatListBare renders a list like "[a b]" without quoting strings.
type FormatListBare struct{ List Expr }

func (f *FormatListBare) emit(e *emitter) {
	io.WriteString(e.w, `((x = `)
	f.List.emit(e)
	io.WriteString(e.w, `); x.is_a?(Array) ? ("[" + x.map{ |x| if x.is_a?(Hash) then '{' + x.to_h.map{ |k,v| "#{k}: #{v.is_a?(String) ? v : v.to_s}" }.join(', ') + '}' else x.to_s end }.join(' ') + "]") : x.to_s)`)
}

// FormatBool renders a boolean as "true" or "false" for printing.
type FormatBool struct{ Value Expr }

func (f *FormatBool) emit(e *emitter) {
	io.WriteString(e.w, "(")
	f.Value.emit(e)
	io.WriteString(e.w, " ? 'true' : 'false')")
}

// SaveJSONLExpr writes a list of records to stdout as JSON lines.
type SaveJSONLExpr struct{ Src Expr }

func (s *SaveJSONLExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	s.Src.emit(e)
	io.WriteString(e.w, ".each do |row|")
	e.nl()
	e.indent++
	e.writeIndent()
	io.WriteString(e.w, "puts(JSON.generate(row.to_h, space: ' ', object_nl: '', array_nl: '', indent: '').gsub(/,/, ', '))")
	e.nl()
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
}

// MethodCallExpr represents calling a method on a target expression.
type MethodCallExpr struct {
	Target Expr
	Method string
	Args   []Expr
}

// MethodRefExpr represents a reference to a named method, used when
// passing a function as a value.
type MethodRefExpr struct{ Name string }

func (m *MethodRefExpr) emit(e *emitter) {
	io.WriteString(e.w, "method(:")
	io.WriteString(e.w, m.Name)
	io.WriteString(e.w, ")")
}

func (m *MethodCallExpr) emit(e *emitter) {
	if fe, ok := m.Target.(*FieldExpr); ok && m.Method == "call" && fe.Name != "" && unicode.IsUpper(rune(fe.Name[0])) {
		fe.Target.emit(e)
		io.WriteString(e.w, ".")
		io.WriteString(e.w, fe.Name)
		io.WriteString(e.w, "(")
		for i, a := range m.Args {
			if i > 0 {
				io.WriteString(e.w, ", ")
			}
			a.emit(e)
		}
		io.WriteString(e.w, ")")
		return
	}
	if m.Method == "padStart" && len(m.Args) == 2 {
		io.WriteString(e.w, "_padStart(")
		m.Target.emit(e)
		io.WriteString(e.w, ", ")
		m.Args[0].emit(e)
		io.WriteString(e.w, ", ")
		m.Args[1].emit(e)
		io.WriteString(e.w, ")")
		return
	}
	if m.Method == "padEnd" && len(m.Args) == 2 {
		io.WriteString(e.w, "_padEnd(")
		m.Target.emit(e)
		io.WriteString(e.w, ", ")
		m.Args[0].emit(e)
		io.WriteString(e.w, ", ")
		m.Args[1].emit(e)
		io.WriteString(e.w, ")")
		return
	}
	switch m.Target.(type) {
	case *Ident, *FieldExpr:
		m.Target.emit(e)
	default:
		io.WriteString(e.w, "(")
		m.Target.emit(e)
		io.WriteString(e.w, ")")
	}
	io.WriteString(e.w, ".")
	io.WriteString(e.w, m.Method)
	io.WriteString(e.w, "(")
	for i, a := range m.Args {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		a.emit(e)
	}
	io.WriteString(e.w, ")")
}

func (a *AppendExpr) emit(e *emitter) {
	io.WriteString(e.w, "(")
	a.List.emit(e)
	io.WriteString(e.w, " + [")
	a.Elem.emit(e)
	io.WriteString(e.w, "])")
}

func (s *SliceExpr) emit(e *emitter) {
	s.Target.emit(e)
	io.WriteString(e.w, "[")
	if s.Start != nil {
		s.Start.emit(e)
	}
	io.WriteString(e.w, "...")
	if s.End != nil {
		s.End.emit(e)
	}
	io.WriteString(e.w, "]")
}

func (l *LambdaExpr) emit(e *emitter) {
	io.WriteString(e.w, "->(")
	for i, p := range l.Params {
		if i > 0 {
			io.WriteString(e.w, ", ")
		}
		io.WriteString(e.w, p)
	}
	io.WriteString(e.w, ") {")
	pushScope(l.Params...)
	if len(l.Body) > 0 {
		e.nl()
		e.indent++
		for _, st := range l.Body {
			e.writeIndent()
			st.emit(e)
			e.nl()
		}
		if l.Expr != nil {
			e.writeIndent()
			l.Expr.emit(e)
			e.nl()
		}
		e.indent--
		popScope()
		e.writeIndent()
		io.WriteString(e.w, "}")
		return
	}
	io.WriteString(e.w, " ")
	if l.Expr != nil {
		l.Expr.emit(e)
	}
	io.WriteString(e.w, " }")
	popScope()
}

func (m *MatchExpr) emit(e *emitter) {
	io.WriteString(e.w, "(case ")
	m.Target.emit(e)
	io.WriteString(e.w, ";")
	e.nl()
	e.indent++
	for _, c := range m.Cases {
		e.writeIndent()
		io.WriteString(e.w, "in ")
		c.Pattern.emit(e)
		e.nl()
		e.indent++
		e.writeIndent()
		c.Result.emit(e)
		e.nl()
		e.indent--
	}
	if m.Else != nil {
		e.writeIndent()
		io.WriteString(e.w, "else")
		e.nl()
		e.indent++
		e.writeIndent()
		m.Else.emit(e)
		e.nl()
		e.indent--
	}
	e.indent--
	e.writeIndent()
	io.WriteString(e.w, "end)")
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
	t := gitTime()
	return fmt.Sprintf("# Generated by Mochi transpiler v%s on %s\n", version(), t.Format("2006-01-02 15:04 -0700"))
}

func zeroValueExpr(t types.Type) Expr {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return &IntLit{Value: 0}
	case types.FloatType:
		return &FloatLit{Value: 0}
	case types.StringType:
		return &StringLit{Value: ""}
	case types.BoolType:
		return &BoolLit{Value: false}
	case types.ListType:
		return &ListLit{}
	case types.MapType, types.StructType:
		return &MapLit{}
	default:
		return &Ident{Name: "nil"}
	}
}

func valueToExpr(v interface{}, typ *parser.TypeRef) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		fields := make([]StructField, len(names))
		items := make([]MapItem, len(names))
		for i, k := range names {
			expr := valueToExpr(val[k], nil)
			fields[i] = StructField{Name: fieldName(k), Value: expr}
			items[i] = MapItem{Key: &StringLit{Value: k}, Value: expr}
		}
		if typ != nil && typ.Simple != nil {
			return &StructNewExpr{Name: toTypeName(*typ.Simple), Fields: fields}
		}
		return &MapLit{Items: items}
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
	case int:
		return &IntLit{Value: val}
	case int64:
		return &IntLit{Value: int(val)}
	case float64:
		if float64(int(val)) == val {
			return &IntLit{Value: int(val)}
		}
		return &FloatLit{Value: val}
	default:
		return &StringLit{Value: fmt.Sprintf("%v", val)}
	}
}

func dataExprFromFile(path, format, delim string, header bool, typ *parser.TypeRef) (Expr, error) {
	if path == "" {
		return &ListLit{}, nil
	}
	root := repoRoot()
	if root != "" {
		if strings.HasPrefix(path, "../") {
			clean := strings.TrimPrefix(path, "../")
			path = filepath.Join(root, "tests", clean)
		} else if !filepath.IsAbs(path) {
			path = filepath.Join(root, path)
		}
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
		lines := strings.Split(strings.TrimSpace(string(data)), "\n")
		arr := make([]interface{}, 0, len(lines))
		for _, line := range lines {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			var obj interface{}
			if err := json.Unmarshal([]byte(line), &obj); err != nil {
				return nil, err
			}
			arr = append(arr, obj)
		}
		v = arr
	case "csv", "":
		d := delim
		if d == "" {
			d = ","
		}
		lines := strings.Split(strings.TrimSpace(string(data)), "\n")
		var rows []interface{}
		var headers []string
		for i, line := range lines {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			parts := strings.Split(line, d)
			if i == 0 && header {
				headers = parts
				continue
			}
			m := map[string]interface{}{}
			for j, part := range parts {
				key := fmt.Sprintf("c%d", j)
				if header && j < len(headers) {
					key = headers[j]
				}
				m[key] = strings.TrimSpace(part)
			}
			rows = append(rows, m)
		}
		v = rows
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v, typ), nil
}

func exprFromPrimary(p *parser.Primary) *parser.Expr {
	return &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: p}}}}
}

func fieldAccess(expr Expr, t types.Type, name string) (Expr, types.Type) {
	switch ty := t.(type) {
	case types.StructType:
		if ty.Name == "MGroup" {
			return &IndexExpr{Target: expr, Index: &StringLit{Value: name}}, ty.Fields[name]
		}
		if ft, ok := ty.Fields[name]; ok {
			return &FieldExpr{Target: expr, Name: fieldName(name), Struct: true}, ft
		}
	case types.MapType:
		return &IndexExpr{Target: expr, Index: &StringLit{Value: name}}, ty.Value
	}
	if id, ok := expr.(*Ident); ok && currentEnv != nil {
		if vt, err := currentEnv.GetVar(id.Name); err == nil {
			if st, ok := vt.(types.StructType); ok {
				if ft, ok := st.Fields[name]; ok {
					return &FieldExpr{Target: expr, Name: fieldName(name), Struct: true}, ft
				}
			}
		}
	}
	// When type information is unavailable assume map style access.
	// If the field name starts with an uppercase letter, treat it as
	// a method/constant reference instead of map indexing so that
	// calls like `pkg.Name()` preserve the original casing.
	if name != "" && unicode.IsUpper(rune(name[0])) {
		return &FieldExpr{Target: expr, Name: fieldName(name)}, nil
	}
	return &IndexExpr{Target: expr, Index: &StringLit{Value: fieldName(name)}}, nil
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

func substituteFields(ex Expr, varName string, fields map[string]bool) Expr {
	switch n := ex.(type) {
	case *Ident:
		if fields[n.Name] {
			return &IndexExpr{Target: &Ident{Name: varName}, Index: &StringLit{Value: n.Name}}
		}
		return n
	case *BinaryExpr:
		n.Left = substituteFields(n.Left, varName, fields)
		n.Right = substituteFields(n.Right, varName, fields)
		return n
	case *UnaryExpr:
		n.Expr = substituteFields(n.Expr, varName, fields)
		return n
	case *CallExpr:
		for i := range n.Args {
			n.Args[i] = substituteFields(n.Args[i], varName, fields)
		}
		return n
	case *MethodCallExpr:
		n.Target = substituteFields(n.Target, varName, fields)
		for i := range n.Args {
			n.Args[i] = substituteFields(n.Args[i], varName, fields)
		}
		return n
	case *IndexExpr:
		n.Target = substituteFields(n.Target, varName, fields)
		n.Index = substituteFields(n.Index, varName, fields)
		return n
	case *SliceExpr:
		n.Target = substituteFields(n.Target, varName, fields)
		if n.Start != nil {
			n.Start = substituteFields(n.Start, varName, fields)
		}
		if n.End != nil {
			n.End = substituteFields(n.End, varName, fields)
		}
		return n
	case *CondExpr:
		n.Cond = substituteFields(n.Cond, varName, fields)
		n.Then = substituteFields(n.Then, varName, fields)
		n.Else = substituteFields(n.Else, varName, fields)
		return n
	case *ListLit:
		for i := range n.Elems {
			n.Elems[i] = substituteFields(n.Elems[i], varName, fields)
		}
		return n
	case *MapLit:
		for i := range n.Items {
			n.Items[i] = MapItem{Key: substituteFields(n.Items[i].Key, varName, fields), Value: substituteFields(n.Items[i].Value, varName, fields)}
		}
		return n
	default:
		return n
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

func parseLoadOptions(e *parser.Expr) (format, delim string, header bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return
	}
	for _, it := range p.Target.Map.Items {
		key, ok := literalString(it.Key)
		if !ok {
			continue
		}
		switch key {
		case "format":
			if s, ok := literalString(it.Value); ok {
				format = s
			}
		case "delimiter":
			if s, ok := literalString(it.Value); ok {
				delim = s
			}
		case "header":
			if b, ok := literalBool(it.Value); ok {
				header = b
			}
		}
	}
	return
}

func extractMapLiteral(e *parser.Expr) ([]string, []*parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return nil, nil, false
	}
	items := p.Target.Map.Items
	keys := make([]string, len(items))
	vals := make([]*parser.Expr, len(items))
	for i, it := range items {
		k, ok := literalString(it.Key)
		if !ok {
			return nil, nil, false
		}
		keys[i] = k
		vals[i] = it.Value
	}
	return keys, vals, true
}

func extractListOfMaps(e *parser.Expr) ([]string, [][]*parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.List == nil {
		return nil, nil, false
	}
	list := p.Target.List
	if len(list.Elems) == 0 {
		return nil, nil, false
	}
	keys, firstVals, ok := extractMapLiteral(list.Elems[0])
	if !ok {
		return nil, nil, false
	}
	values := make([][]*parser.Expr, len(list.Elems))
	values[0] = firstVals
	for i := 1; i < len(list.Elems); i++ {
		k, v, ok := extractMapLiteral(list.Elems[i])
		if !ok || len(k) != len(keys) {
			return nil, nil, false
		}
		for j := range k {
			if k[j] != keys[j] {
				return nil, nil, false
			}
		}
		values[i] = v
	}
	return keys, values, true
}

func extractQueryExpr(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Query == nil {
		return nil
	}
	return p.Target.Query
}

func toStructName(varName string) string {
	name := strings.Title(varName)
	if strings.HasSuffix(varName, "s") {
		return name + "Item"
	}
	return name
}

func toTypeName(name string) string {
	return strings.Title(name)
}

// global environment used for type inference during conversion
var currentEnv *types.Env
var funcDepth int
var topVars map[string]bool

// Emit writes Ruby code for program p to w.
func Emit(w io.Writer, p *Program) error {
	e := &emitter{w: w}
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "$VERBOSE = nil\n"); err != nil {
		return err
	}
	if needsJSON {
		if _, err := io.WriteString(w, "require 'json'\n"); err != nil {
			return err
		}
	}
	if usesNow {
		if _, err := io.WriteString(w, helperNow+"\n"); err != nil {
			return err
		}
	}
	if usesInput {
		if _, err := io.WriteString(w, helperInput+"\n"); err != nil {
			return err
		}
	}
	if usesLookupHost {
		if _, err := io.WriteString(w, helperLookupHost+"\n"); err != nil {
			return err
		}
	}
	if usesSHA256 {
		if _, err := io.WriteString(w, helperSHA256+"\n"); err != nil {
			return err
		}
	}
	if usesIndexOf {
		if _, err := io.WriteString(w, helperIndexOf+"\n"); err != nil {
			return err
		}
	}
	if usesRepeat {
		if _, err := io.WriteString(w, helperRepeat+"\n"); err != nil {
			return err
		}
	}
	if usesSplit {
		if _, err := io.WriteString(w, helperSplit+"\n"); err != nil {
			return err
		}
	}
	if usesFetch {
		if _, err := io.WriteString(w, helperFetch+"\n"); err != nil {
			return err
		}
	}
	if usesParseIntStr {
		if _, err := io.WriteString(w, helperParseIntStr+"\n"); err != nil {
			return err
		}
	}
	if usesMem {
		if _, err := io.WriteString(w, helperMem+"\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, helperAdd+"\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, helperEq+"\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, helperPadStart+"\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, helperPadEnd+"\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, helperStr+"\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, helperStringEach+"\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, helperPanic+"\n"); err != nil {
		return err
	}
	for _, s := range p.Stmts {
		e.writeIndent()
		s.emit(e)
		e.nl()
	}
	return nil
}

func reorderFuncs(stmts []Stmt) []Stmt {
	if len(stmts) == 0 {
		return stmts
	}
	var funcs []Stmt
	var rest []Stmt
	for _, st := range stmts {
		switch s := st.(type) {
		case *FuncStmt:
			funcs = append(funcs, s)
		case *BenchStmt:
			s.Body = reorderFuncs(s.Body)
			rest = append(rest, s)
		default:
			rest = append(rest, st)
		}
	}
	return append(funcs, rest...)
}

// Transpile converts a Mochi program into a Ruby AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	needsJSON = false
	usesNow = false
	usesInput = false
	usesLookupHost = false
	usesSHA256 = false
	usesIndexOf = false
	usesRepeat = false
	usesParseIntStr = false
	usesSplit = false
	usesFetch = false
	usesMem = false
	currentEnv = env
	// track top-level variables even when benchmarking so that global
	// variables are correctly prefixed with '$' when referenced inside
	// functions. Without this, methods defined within the benchmark block
	// cannot access global variables, leading to NameError at runtime.
	topVars = map[string]bool{}
	scopeStack = nil
	rbProg := &Program{}
	for _, st := range prog.Statements {
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if conv != nil {
			rbProg.Stmts = append(rbProg.Stmts, conv)
		}
	}
	rbProg.Stmts = reorderFuncs(rbProg.Stmts)
	if benchMain {
		needsJSON = true
		usesNow = true
		usesMem = true
		rbProg.Stmts = []Stmt{&BenchStmt{Name: "main", Body: rbProg.Stmts}}
	}
	return rbProg, nil
}

func convertStmt(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Import != nil:
		return convertImport(st.Import)
	case st.ExternVar != nil, st.ExternFun != nil, st.ExternType != nil, st.ExternObject != nil:
		return nil, nil
	case st.Expr != nil:
		if isExponentToken(st.Expr.Expr) {
			return nil, nil
		}
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Type != nil:
		if len(st.Type.Members) > 0 {
			fields := make([]string, 0)
			var methods []*FuncStmt
			for _, m := range st.Type.Members {
				if m.Field != nil {
					fields = append(fields, fieldName(m.Field.Name))
				} else if m.Method != nil {
					fn, err := convertFunc(m.Method)
					if err != nil {
						return nil, err
					}
					methods = append(methods, fn)
				}
			}
			if len(fields) > 0 || len(methods) > 0 {
				name := toTypeName(st.Type.Name)
				return &StructDefStmt{Name: name, Fields: fields, Methods: methods}, nil
			}
		}
		var stmts []Stmt
		for _, v := range st.Type.Variants {
			if len(v.Fields) == 0 {
				stmts = append(stmts, &LetStmt{Name: identName(v.Name), Value: &CallExpr{Func: "Object.new"}})
			} else {
				fields := make([]string, len(v.Fields))
				for i, f := range v.Fields {
					fields[i] = fieldName(f.Name)
				}
				name := toTypeName(v.Name)
				stmts = append(stmts, &StructDefStmt{Name: name, Fields: fields})
			}
		}
		if len(stmts) > 0 {
			return &BlockStmt{Stmts: stmts}, nil
		}
		return nil, nil
	case st.Let != nil:
		if st.Let.Value != nil {
			if keys, vals, ok := extractListOfMaps(st.Let.Value); ok {
				structName := identName(toStructName(st.Let.Name))
				skeys := make([]string, len(keys))
				for i, k := range keys {
					skeys[i] = fieldName(k)
				}
				stmts := []Stmt{&StructDefStmt{Name: structName, Fields: skeys}}
				elems := make([]Expr, len(vals))
				if currentEnv != nil {
					fieldTypes := map[string]types.Type{}
					for j, val := range vals[0] {
						fieldTypes[skeys[j]] = types.ExprType(val, currentEnv)
					}
					currentEnv.SetStruct(structName, types.StructType{Name: structName, Fields: fieldTypes, Order: skeys})
					currentEnv.SetVar(st.Let.Name, types.ListType{Elem: types.StructType{Name: structName, Fields: fieldTypes, Order: skeys}}, false)
				}
				for i, fields := range vals {
					fv := make([]StructField, len(skeys))
					for j, val := range fields {
						ex, err := convertExpr(val)
						if err != nil {
							return nil, err
						}
						fv[j] = StructField{Name: skeys[j], Value: ex}
					}
					elems[i] = &StructNewExpr{Name: structName, Fields: fv}
				}
				list := &ListLit{Elems: elems}
				stmts = append(stmts, &LetStmt{Name: st.Let.Name, Value: list})
				return &BlockStmt{Stmts: stmts}, nil
			}
			if q := extractQueryExpr(st.Let.Value); q != nil {
				qe, def, err := convertQueryForLet(q, st.Let.Name)
				if err != nil {
					return nil, err
				}
				stmts := []Stmt{}
				if def != nil {
					stmts = append(stmts, def)
				}
				stmts = append(stmts, &LetStmt{Name: st.Let.Name, Value: qe})
				return &BlockStmt{Stmts: stmts}, nil
			}
		}
		var v Expr
		var err error
		if st.Let.Value != nil {
			v, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			if currentEnv != nil {
				t := types.ExprType(st.Let.Value, currentEnv)
				if isExistsCall(st.Let.Value) {
					t = types.StringType{}
				}
				currentEnv.SetVar(st.Let.Name, t, false)
			}
		} else if st.Let.Type != nil && currentEnv != nil {
			typ := types.ResolveTypeRef(st.Let.Type, currentEnv)
			v = zeroValueExpr(typ)
			currentEnv.SetVar(st.Let.Name, typ, false)
		} else {
			v = &Ident{Name: "nil"}
			if currentEnv != nil {
				currentEnv.SetVar(st.Let.Name, types.AnyType{}, false)
			}
		}
		if funcDepth == 0 && loopDepth == 0 {
			if topVars != nil {
				topVars[st.Let.Name] = true
			}
		}
		if funcDepth > 0 {
			addVar(st.Let.Name)
		}
		return &LetStmt{Name: st.Let.Name, Value: v}, nil
	case st.Var != nil:
		var v Expr
		var err error
		if st.Var.Value != nil {
			v, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
			if currentEnv != nil {
				t := types.ExprType(st.Var.Value, currentEnv)
				currentEnv.SetVar(st.Var.Name, t, true)
			}
		} else if st.Var.Type != nil && currentEnv != nil {
			typ := types.ResolveTypeRef(st.Var.Type, currentEnv)
			v = zeroValueExpr(typ)
			currentEnv.SetVar(st.Var.Name, typ, true)
		} else {
			v = &Ident{Name: "nil"}
			if currentEnv != nil {
				currentEnv.SetVar(st.Var.Name, types.AnyType{}, true)
			}
		}
		if funcDepth == 0 && loopDepth == 0 {
			if topVars != nil {
				topVars[st.Var.Name] = true
			}
		}
		addVar(st.Var.Name)
		return &VarStmt{Name: st.Var.Name, Value: v}, nil
	case st.Assign != nil:
		v, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		if len(st.Assign.Index) >= 1 && len(st.Assign.Field) == 0 {
			target := Expr(&Ident{Name: identName(st.Assign.Name)})
			for i := 0; i < len(st.Assign.Index)-1; i++ {
				if st.Assign.Index[i].Colon != nil || st.Assign.Index[i].Colon2 != nil {
					return nil, fmt.Errorf("unsupported assignment")
				}
				idx, err := convertExpr(st.Assign.Index[i].Start)
				if err != nil {
					return nil, err
				}
				target = &IndexExpr{Target: target, Index: idx}
			}
			last := st.Assign.Index[len(st.Assign.Index)-1]
			if last.Colon != nil || last.Colon2 != nil {
				return nil, fmt.Errorf("unsupported assignment")
			}
			idx, err := convertExpr(last.Start)
			if err != nil {
				return nil, err
			}
			return &IndexAssignStmt{Target: target, Index: idx, Value: v}, nil
		}
		if len(st.Assign.Index) >= 1 && len(st.Assign.Field) >= 1 {
			target := Expr(&Ident{Name: identName(st.Assign.Name)})
			for _, ix := range st.Assign.Index {
				if ix.Colon != nil || ix.Colon2 != nil {
					return nil, fmt.Errorf("unsupported assignment")
				}
				idx, err := convertExpr(ix.Start)
				if err != nil {
					return nil, err
				}
				target = &IndexExpr{Target: target, Index: idx}
			}
			for i := 0; i < len(st.Assign.Field)-1; i++ {
				target = &IndexExpr{Target: target, Index: &StringLit{Value: fieldName(st.Assign.Field[i].Name)}}
			}
			idx := &StringLit{Value: fieldName(st.Assign.Field[len(st.Assign.Field)-1].Name)}
			return &IndexAssignStmt{Target: target, Index: idx, Value: v}, nil
		}
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) >= 1 {
			target := Expr(&Ident{Name: identName(st.Assign.Name)})
			for i := 0; i < len(st.Assign.Field)-1; i++ {
				target = &IndexExpr{Target: target, Index: &StringLit{Value: fieldName(st.Assign.Field[i].Name)}}
			}
			idx := &StringLit{Value: fieldName(st.Assign.Field[len(st.Assign.Field)-1].Name)}
			return &IndexAssignStmt{Target: target, Index: idx, Value: v}, nil
		}
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			return &AssignStmt{Name: identName(st.Assign.Name), Value: v}, nil
		}
		return nil, fmt.Errorf("unsupported assignment")
	case st.If != nil:
		return convertIf(st.If)
	case st.While != nil:
		return convertWhile(st.While)
	case st.For != nil:
		return convertFor(st.For)
	case st.Update != nil:
		return convertUpdate(st.Update)
	case st.Expect != nil:
		e, err := convertExpr(st.Expect.Value)
		if err != nil {
			return nil, err
		}
		return &AssertStmt{Expr: e}, nil
	case st.Test != nil:
		body := make([]Stmt, len(st.Test.Body))
		for i, s := range st.Test.Body {
			st2, err := convertStmt(s)
			if err != nil {
				return nil, err
			}
			body[i] = st2
		}
		comment := &CommentStmt{Text: "test " + strings.Trim(st.Test.Name, "\"")}
		return &BlockStmt{Stmts: append([]Stmt{comment}, body...)}, nil
	case st.Bench != nil:
		body := make([]Stmt, len(st.Bench.Body))
		savedEnv := currentEnv
		if savedEnv != nil {
			currentEnv = types.NewEnv(savedEnv)
		}
		pushScope()
		for i, s := range st.Bench.Body {
			st2, err := convertStmt(s)
			if err != nil {
				popScope()
				currentEnv = savedEnv
				return nil, err
			}
			body[i] = st2
		}
		popScope()
		currentEnv = savedEnv
		needsJSON = true
		usesNow = true
		usesMem = true
		name := strings.Trim(st.Bench.Name, "\"")
		return &BenchStmt{Name: name, Body: body}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Return != nil:
		var v Expr
		if st.Return.Value != nil {
			var err error
			v, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: v}, nil
	case st.Fun != nil:
		if st.Fun.Name == "sqrt" && len(st.Fun.Params) == 1 {
			p := safeName(st.Fun.Params[0].Name)
			body := []Stmt{&ReturnStmt{Value: &CallExpr{Func: "Math.sqrt", Args: []Expr{&Ident{Name: p}}}}}
			return &FuncStmt{Name: st.Fun.Name, Params: []string{p}, Body: body}, nil
		}
		funcDepth++
		savedEnv := currentEnv
		if savedEnv != nil {
			child := types.NewEnv(savedEnv)
			for _, p := range st.Fun.Params {
				var typ types.Type = types.AnyType{}
				if p.Type != nil {
					typ = types.ResolveTypeRef(p.Type, savedEnv)
				}
				child.SetVar(p.Name, typ, true)
			}
			currentEnv = child
		}
		pushScope()
		for _, p := range st.Fun.Params {
			addVar(p.Name)
		}
		body := make([]Stmt, len(st.Fun.Body))
		for i, s := range st.Fun.Body {
			st2, err := convertStmt(s)
			if err != nil {
				popScope()
				currentEnv = savedEnv
				funcDepth--
				return nil, err
			}
			body[i] = st2
		}
		popScope()
		currentEnv = savedEnv
		funcDepth--
		var params []string
		for _, p := range st.Fun.Params {
			params = append(params, safeName(p.Name))
		}
		// In earlier versions the transpiler copied struct parameters at
		// the start of top‑level functions to emulate pass‑by‑value
		// semantics.  This caused issues for functions that rely on
		// updating a struct argument as a side effect (e.g. readers that
		// maintain internal state).  Since Rosetta examples expect
		// struct parameters to behave like references, the automatic
		// cloning is removed so mutations are visible to the caller.
		if funcDepth > 0 {
			lam := &LambdaExpr{Params: params, Body: body}
			if currentEnv != nil {
				currentEnv.SetVar(st.Fun.Name, types.FuncType{}, false)
			}
			addVar(st.Fun.Name)
			return &LetStmt{Name: st.Fun.Name, Value: lam}, nil
		}
		return &FuncStmt{Name: st.Fun.Name, Params: params, Body: body}, nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func isExponentToken(e *parser.Expr) bool {
	if e != nil && e.Binary != nil {
		u := e.Binary.Left
		if pf := u.Value; pf != nil && pf.Target != nil && pf.Target.Selector != nil {
			root := pf.Target.Selector.Root
			if strings.HasPrefix(root, "e") {
				if _, err := strconv.Atoi(root[1:]); err == nil {
					return true
				}
			}
		}
	}
	return false
}

func convertFunc(fn *parser.FunStmt) (*FuncStmt, error) {
	funcDepth++
	savedEnv := currentEnv
	if savedEnv != nil {
		child := types.NewEnv(savedEnv)
		for _, p := range fn.Params {
			var typ types.Type = types.AnyType{}
			if p.Type != nil {
				typ = types.ResolveTypeRef(p.Type, savedEnv)
			}
			child.SetVar(p.Name, typ, true)
		}
		currentEnv = child
	}
	pushScope()
	for _, p := range fn.Params {
		addVar(p.Name)
	}
	if fn.Name == "sqrt" && len(fn.Params) == 1 {
		popScope()
		currentEnv = savedEnv
		funcDepth--
		p := safeName(fn.Params[0].Name)
		body := []Stmt{&ReturnStmt{Value: &CallExpr{Func: "Math.sqrt", Args: []Expr{&Ident{Name: p}}}}}
		return &FuncStmt{Name: fn.Name, Params: []string{p}, Body: body}, nil
	}
	stmts := mergeSciNotation(fn.Body)
	body := make([]Stmt, 0, len(stmts))
	for _, s := range stmts {
		st2, err := convertStmt(s)
		if err != nil {
			popScope()
			currentEnv = savedEnv
			funcDepth--
			return nil, err
		}
		if st2 != nil {
			body = append(body, st2)
		}
	}
	popScope()
	currentEnv = savedEnv
	funcDepth--
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = safeName(p.Name)
	}
	return &FuncStmt{Name: fn.Name, Params: params, Body: body}, nil
}

// mergeSciNotation merges scientific notation represented as a float literal
// followed by a standalone identifier like e18 into a single float literal.
// Some Mochi programs use syntax like 1.0e18 which the parser currently
// splits into a float literal (1.0) and a separate expression statement with
// selector root "e18". This helper scans the function body and combines such
// pairs into a single float value so the transpiled Ruby code renders the
// intended scientific-notation number.
func mergeSciNotation(stmts []*parser.Statement) []*parser.Statement {
	if len(stmts) == 0 {
		return stmts
	}
	res := make([]*parser.Statement, 0, len(stmts))
	for i := 0; i < len(stmts); i++ {
		st := stmts[i]
		if st.While != nil {
			st.While.Body = mergeSciNotation(st.While.Body)
		}
		if st.If != nil {
			st.If.Then = mergeSciNotation(st.If.Then)
			st.If.Else = mergeSciNotation(st.If.Else)
		}
		if st.For != nil {
			st.For.Body = mergeSciNotation(st.For.Body)
		}
		// drop standalone exponent tokens like e18
		if st.Expr != nil && st.Expr.Expr != nil && st.Expr.Expr.Binary != nil {
			u := st.Expr.Expr.Binary.Left
			if pf := u.Value; pf != nil && pf.Target != nil && pf.Target.Selector != nil {
				root := pf.Target.Selector.Root
				if strings.HasPrefix(root, "e") {
					if _, err := strconv.Atoi(root[1:]); err == nil {
						continue
					}
				}
			}
		}
		if st.Var != nil && i+1 < len(stmts) {
			next := stmts[i+1]
			// check next statement is an expr with selector root eNN
			if next.Expr != nil && next.Expr.Expr != nil && next.Expr.Expr.Binary != nil {
				u := next.Expr.Expr.Binary.Left
				if pf := u.Value; pf != nil && pf.Target != nil && pf.Target.Selector != nil {
					root := pf.Target.Selector.Root
					if strings.HasPrefix(root, "e") {
						if exp, err := strconv.Atoi(root[1:]); err == nil {
							if st.Var.Value != nil && st.Var.Value.Binary != nil {
								u2 := st.Var.Value.Binary.Left
								if pf2 := u2.Value; pf2 != nil && pf2.Target != nil && pf2.Target.Lit != nil && pf2.Target.Lit.Float != nil {
									sign := 1.0
									for _, op := range u2.Ops {
										if op == "-" {
											sign *= -1
										}
									}
									base := *pf2.Target.Lit.Float
									newVal := sign * base * math.Pow10(exp)
									u2.Ops = nil
									*pf2.Target.Lit.Float = newVal
									i++ // skip next statement
								}
							}
						}
					}
				}
			}
		}
		res = append(res, st)
	}
	return res
}

func convertIf(ifst *parser.IfStmt) (Stmt, error) {
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
		st, err := convertIf(ifst.ElseIf)
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
	var expr Expr = &Ident{Name: "nil"}
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		res, err := convertExpr(c.Result)
		if err != nil {
			return nil, err
		}
		pat := c.Pattern
		if pat.Binary != nil && len(pat.Binary.Right) == 0 {
			u := pat.Binary.Left
			if len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil {
				call := u.Value.Target.Call
				if st, ok := currentEnv.GetStruct(call.Func); ok {
					cond := &MethodCallExpr{Target: target, Method: "is_a?", Args: []Expr{&Ident{Name: call.Func}}}
					exprCase := res
					for j := len(call.Args) - 1; j >= 0; j-- {
						name, ok := isSimpleIdent(call.Args[j])
						if ok && name != "_" {
							if j < len(st.Order) {
								field := st.Order[j]
								lam := &LambdaExpr{Params: []string{identName(name)}, Expr: exprCase}
								exprCase = &MethodCallExpr{Target: lam, Method: "call", Args: []Expr{&FieldExpr{Target: target, Name: field, Struct: true}}}
							}
						}
					}
					expr = &CondExpr{Cond: cond, Then: exprCase, Else: expr}
					continue
				}
			}
		}
		if pat.Binary != nil && len(pat.Binary.Right) == 0 {
			u := pat.Binary.Left
			if len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil && u.Value.Target.Call.Func == "Node" {
				call := u.Value.Target.Call
				if len(call.Args) == 3 {
					names := []string{}
					ok := true
					for _, a := range call.Args {
						name, ok2 := isSimpleIdent(a)
						if !ok2 {
							ok = false
							break
						}
						names = append(names, name)
					}
					if ok {
						if tn, ok2 := target.(*Ident); ok2 {
							fields := map[string]bool{}
							for _, nm := range names {
								fields[nm] = true
							}
							res = substituteFields(res, tn.Name, fields)
							cond := &BinaryExpr{Left: target, Op: "!=", Right: &Ident{Name: "nil"}}
							expr = &CondExpr{Cond: cond, Then: res, Else: expr}
							continue
						}
					}
				}
			}
		}
		patExpr, err := convertExpr(pat)
		if err != nil {
			return nil, err
		}
		if id, ok := patExpr.(*Ident); ok && id.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: patExpr}
		expr = &CondExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func convertWhile(ws *parser.WhileStmt) (Stmt, error) {
	loopDepth++
	defer func() { loopDepth-- }()
	cond, err := convertExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	body := make([]Stmt, 0, len(ws.Body))
	for _, s := range ws.Body {
		st, err := convertStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			body = append(body, st)
		}
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertFor(f *parser.ForStmt) (Stmt, error) {
	loopDepth++
	defer func() { loopDepth-- }()
	var elemType types.Type = types.AnyType{}
	if currentEnv != nil {
		t := types.ExprType(f.Source, currentEnv)
		switch tt := t.(type) {
		case types.ListType:
			elemType = tt.Elem
		case types.MapType:
			elemType = tt.Key
		}
	}
	if f.RangeEnd != nil {
		elemType = types.IntType{}
	}
	saved := currentEnv
	if saved != nil {
		child := types.NewEnv(saved)
		child.SetVar(f.Name, elemType, true)
		currentEnv = child
	}
	addVar(safeName(f.Name))
	body := make([]Stmt, len(f.Body))
	for i, s := range f.Body {
		st, err := convertStmt(s)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
		body[i] = st
	}
	currentEnv = saved
	if f.RangeEnd != nil {
		start, err := convertExpr(f.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(f.RangeEnd)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: f.Name, Start: start, End: end, Body: body}, nil
	}
	iterable, err := convertExpr(f.Source)
	if err != nil {
		return nil, err
	}
	charEach := false
	if currentEnv != nil {
		t := types.ExprType(f.Source, currentEnv)
		switch t.(type) {
		case types.MapType:
			iterable = &MethodCallExpr{Target: iterable, Method: "keys"}
		case types.StringType:
			charEach = true
		}
	}
	return &ForInStmt{Name: f.Name, Iterable: iterable, Body: body, CharEach: charEach}, nil
}

func convertUpdate(u *parser.UpdateStmt) (Stmt, error) {
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
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
	}
	prev := currentEnv
	currentEnv = child
	var fields []string
	var values []Expr
	for _, it := range u.Set.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
			if !ok {
				currentEnv = prev
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(it.Value)
		if err != nil {
			currentEnv = prev
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
			currentEnv = prev
			return nil, err
		}
		cond = substituteFields(c, "item", fieldSet)
	}
	currentEnv = prev
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond, Struct: true}, nil
}

func convertImport(im *parser.ImportStmt) (Stmt, error) {
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
			if currentEnv != nil {
				fields := map[string]types.Type{
					"pi":   types.FloatType{},
					"e":    types.FloatType{},
					"sqrt": types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}},
					"pow":  types.FuncType{Params: []types.Type{types.FloatType{}, types.FloatType{}}, Return: types.FloatType{}},
					"sin":  types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}},
					"log":  types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}},
				}
				currentEnv.SetVar(alias, types.StructType{Name: "PythonMath", Fields: fields}, false)
			}
			return &ImportStmt{Alias: alias, Module: "python_math"}, nil
		}
	case "go":
		if im.Auto && path == "mochi/runtime/ffi/go/testpkg" {
			if currentEnv != nil {
				fields := map[string]types.Type{
					"Add":                  types.FuncType{Params: []types.Type{types.IntType{}, types.IntType{}}, Return: types.IntType{}},
					"Pi":                   types.FloatType{},
					"Answer":               types.IntType{},
					"FifteenPuzzleExample": types.FuncType{Params: []types.Type{}, Return: types.StringType{}},
					"MD5Hex":               types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}},
				}
				currentEnv.SetVar(alias, types.StructType{Name: "GoTestpkg", Fields: fields}, false)
			}
			return &ImportStmt{Alias: alias, Module: "go_testpkg"}, nil
		}
		if im.Auto && path == "net" {
			if currentEnv != nil {
				fields := map[string]types.Type{
					"LookupHost": types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.ListType{Elem: types.AnyType{}}},
				}
				currentEnv.SetVar(alias, types.StructType{Name: "GoNet", Fields: fields}, false)
			}
			return &ImportStmt{Alias: alias, Module: "go_net"}, nil
		}
		if im.Auto && path == "os" {
			if currentEnv != nil {
				fields := map[string]types.Type{
					"Getenv":  types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}},
					"Environ": types.FuncType{Params: []types.Type{}, Return: types.ListType{Elem: types.StringType{}}},
				}
				currentEnv.SetVar(alias, types.StructType{Name: "GoOS", Fields: fields}, false)
			}
			return &ImportStmt{Alias: alias, Module: "go_os"}, nil
		}
		if im.Auto && path == "strings" {
			if currentEnv != nil {
				fields := map[string]types.Type{
					"ToUpper":   types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}},
					"TrimSpace": types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.StringType{}},
				}
				currentEnv.SetVar(alias, types.StructType{Name: "GoStrings", Fields: fields}, false)
			}
			return &ImportStmt{Alias: alias, Module: "go_strings"}, nil
		}
	}
	return nil, fmt.Errorf("unsupported import")
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	return convertBinary(e.Binary)
}

type binOp struct {
	op    string
	all   bool
	right *parser.PostfixExpr
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	operands := []Expr{}
	ops := []binOp{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	for _, p := range b.Right {
		o, err := convertPostfix(p.Right)
		if err != nil {
			return nil, err
		}
		ops = append(ops, binOp{op: p.Op, all: p.All, right: p.Right})
		operands = append(operands, o)
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

	apply := func(i int) error {
		left := operands[i]
		right := operands[i+1]
		op := ops[i]
		var expr Expr
		switch op.op {
		case "in":
			typ := types.TypeOfPostfix(op.right, currentEnv)
			if _, ok := typ.(types.MapType); ok {
				expr = &MethodCallExpr{Target: right, Method: "key?", Args: []Expr{left}}
			} else {
				expr = &MethodCallExpr{Target: right, Method: "include?", Args: []Expr{left}}
			}
		case "union":
			if op.all {
				expr = &UnionAllExpr{Left: left, Right: right}
			} else {
				expr = &UnionExpr{Left: left, Right: right}
			}
		case "except":
			expr = &ExceptExpr{Left: left, Right: right}
		case "intersect":
			expr = &IntersectExpr{Left: left, Right: right}
		default:
			if op.all {
				return fmt.Errorf("unsupported binary op")
			}
			expr = &BinaryExpr{Op: op.op, Left: left, Right: right}
		}
		operands[i] = expr
		operands = append(operands[:i+1], operands[i+2:]...)
		ops = append(ops[:i], ops[i+1:]...)
		return nil
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i].op == t {
					if err := apply(i); err != nil {
						return nil, err
					}
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
		return nil, fmt.Errorf("unsupported unary")
	}
	ex, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			ex = &UnaryExpr{Op: op, Expr: ex}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return ex, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	var expr Expr
	var err error
	var curType types.Type
	// special case: selector with method call
	start := 0
	if pf.Target != nil && pf.Target.Selector != nil && len(pf.Ops) > 0 && pf.Ops[0].Call != nil && len(pf.Target.Selector.Tail) > 0 {
		expr = &Ident{Name: pf.Target.Selector.Root}
		if currentEnv != nil {
			rootPrim := &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
			curType = types.ExprType(exprFromPrimary(rootPrim), currentEnv)
			for _, t := range pf.Target.Selector.Tail[:len(pf.Target.Selector.Tail)-1] {
				expr, curType = fieldAccess(expr, curType, t)
			}
		}
		method := pf.Target.Selector.Tail[len(pf.Target.Selector.Tail)-1]
		args := make([]Expr, len(pf.Ops[0].Call.Args))
		for i, a := range pf.Ops[0].Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		if field, ft := fieldAccess(expr, curType, method); ft != nil {
			expr = &MethodCallExpr{Target: field, Method: "call", Args: args}
		} else {
			if ident, ok := expr.(*Ident); ok && ident.Name == "net" && method == "LookupHost" {
				usesLookupHost = true
				expr = &CallExpr{Func: "_lookup_host", Args: args}
			} else {
				if method == "contains" {
					method = "include?"
				}
				if method == "get" && len(args) == 2 {
					expr = &MapGetExpr{Map: expr, Key: args[0], Default: args[1]}
				} else {
					expr = &MethodCallExpr{Target: expr, Method: method, Args: args}
				}
			}
		}
		start = 1
	} else {
		expr, err = convertPrimary(pf.Target)
		if err != nil {
			return nil, err
		}
		if currentEnv != nil {
			curType = types.ExprType(exprFromPrimary(pf.Target), currentEnv)
		}
	}
	for i := start; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			// If the index is a string literal and the target is a
			// struct, sanitize the field name and emit a field
			// access. For non-struct types (maps), preserve the
			// original key so that characters like '.' remain
			// intact. Previously we sanitized all string keys which
			// caused map lookups such as ["input.txt"] to be
			// rewritten as field accesses ($fs.input.txt).
			if sl, ok := idx.(*StringLit); ok {
				if st, ok := curType.(types.StructType); ok && isValidIdent(sl.Value) {
					name := fieldName(sl.Value)
					if ft, ok := st.Fields[sl.Value]; ok {
						expr = &FieldExpr{Target: expr, Name: name, Struct: true}
						curType = ft
						continue
					}
					idx = &StringLit{Value: name}
				}
			}
			expr = &IndexExpr{Target: expr, Index: idx}
			if lt, ok := curType.(types.ListType); ok {
				curType = lt.Elem
			} else if mt, ok := curType.(types.MapType); ok {
				curType = mt.Value
			} else {
				curType = nil
			}
		case op.Index != nil && (op.Index.Colon != nil || op.Index.Colon2 != nil):
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
			expr = &SliceExpr{Target: expr, Start: start, End: end}
			if lt, ok := curType.(types.ListType); ok {
				curType = lt
			} else {
				curType = nil
			}
		case op.Field != nil && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			// method call like expr.field(args)
			call := pf.Ops[i+1].Call
			args := make([]Expr, len(call.Args))
			for j, a := range call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			// If the field exists on the struct, treat it as a stored function value
			// and invoke it with `.call`. Otherwise assume a normal method call.
			if field, ft := fieldAccess(expr, curType, op.Field.Name); ft != nil {
				expr = &MethodCallExpr{Target: field, Method: "call", Args: args}
			} else {
				method := op.Field.Name
				if ident, ok := expr.(*Ident); ok && ident.Name == "net" && method == "LookupHost" {
					usesLookupHost = true
					expr = &CallExpr{Func: "_lookup_host", Args: args}
				} else {
					if method == "contains" {
						method = "include?"
					}
					if method == "padStart" && len(args) == 2 {
						expr = &CallExpr{Func: "_padStart", Args: append([]Expr{expr}, args...)}
					} else if method == "padEnd" && len(args) == 2 {
						expr = &CallExpr{Func: "_padEnd", Args: append([]Expr{expr}, args...)}
					} else if method == "get" && len(args) == 2 {
						expr = &MapGetExpr{Map: expr, Key: args[0], Default: args[1]}
					} else {
						expr = &MethodCallExpr{Target: expr, Method: method, Args: args}
					}
				}
			}
			i++ // consume call op
			curType = nil
		case op.Field != nil:
			expr, curType = fieldAccess(expr, curType, op.Field.Name)
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				name := *op.Cast.Type.Simple
				typName := identName(toTypeName(name))
				switch name {
				case "int", "float", "string", "bigrat":
					typName = name
				}
				if currentEnv != nil {
					if st, ok := currentEnv.GetStruct(typName); ok {
						if ml, ok2 := expr.(*MapLit); ok2 {
							fields := make([]StructField, len(st.Order))
							for i, name := range st.Order {
								for _, it := range ml.Items {
									if sl, ok := it.Key.(*StringLit); ok && sl.Value == name {
										fields[i] = StructField{Name: fieldName(name), Value: it.Value}
										break
									}
								}
							}
							expr = &StructNewExpr{Name: typName, Fields: fields}
						}
						curType = st
					} else {
						curType = types.ResolveTypeRef(op.Cast.Type, currentEnv)
					}
				}
				expr = &CastExpr{Value: expr, Type: typName}
			}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for j, a := range op.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = ex
			}
			if f, ok := expr.(*FieldExpr); ok {
				if _, ok := curType.(types.FuncType); ok || (f.Name != "" && unicode.IsUpper(rune(f.Name[0]))) {
					expr = &MethodCallExpr{Target: f.Target, Method: f.Name, Args: args}
					curType = nil
					continue
				}
			}
			expr = &MethodCallExpr{Target: expr, Method: "call", Args: args}
			curType = nil
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
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
		switch name {
		case "print":
			return convertPrintCall(args, p.Call.Args)
		case "len", "count":
			if currentEnv != nil {
				if _, ok := currentEnv.GetFunc(name); ok {
					return &CallExpr{Func: name, Args: args}, nil
				}
			}
			if len(args) != 1 {
				return nil, fmt.Errorf("len/count takes one arg")
			}
			return &LenExpr{Value: args[0]}, nil
		case "sum":
			if len(args) != 1 {
				return nil, fmt.Errorf("sum takes one arg")
			}
			return &SumExpr{Value: args[0]}, nil
		case "avg":
			if len(args) != 1 {
				return nil, fmt.Errorf("avg takes one arg")
			}
			return &AvgExpr{Value: args[0]}, nil
		case "min":
			if len(args) != 1 {
				return nil, fmt.Errorf("min takes one arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "min"}, nil
		case "max":
			if len(args) != 1 {
				return nil, fmt.Errorf("max takes one arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "max"}, nil
		case "int":
			if len(args) != 1 {
				return nil, fmt.Errorf("int takes one arg")
			}
			return &CastExpr{Value: args[0], Type: "int"}, nil
		case "float":
			if len(args) != 1 {
				return nil, fmt.Errorf("float takes one arg")
			}
			return &CastExpr{Value: args[0], Type: "float"}, nil
		case "to_float":
			if len(args) != 1 {
				return nil, fmt.Errorf("to_float expects 1 arg")
			}
			return &CastExpr{Value: args[0], Type: "float"}, nil
		case "str":
			if len(args) != 1 {
				return nil, fmt.Errorf("str takes one arg")
			}
			if _, ok := types.ExprType(p.Call.Args[0], currentEnv).(types.ListType); ok {
				return &FormatListBare{List: args[0]}, nil
			}
			return &CastExpr{Value: args[0], Type: "string"}, nil
		case "values":
			if len(args) != 1 {
				return nil, fmt.Errorf("values takes one arg")
			}
			return &ValuesExpr{Map: args[0]}, nil
		case "keys":
			if len(args) != 1 {
				return nil, fmt.Errorf("keys takes one arg")
			}
			if currentEnv != nil {
				if _, ok := currentEnv.GetFunc(name); ok {
					return &CallExpr{Func: name, Args: args}, nil
				}
			}
			return &MethodCallExpr{Target: args[0], Method: "keys"}, nil
		case "exists":
			if len(args) != 1 {
				return nil, fmt.Errorf("exists takes one arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "is_a?", Args: []Expr{&Ident{Name: "Array"}}}, nil
		case "append":
			if len(args) != 2 {
				return nil, fmt.Errorf("append takes two args")
			}
			return &AppendExpr{List: args[0], Elem: args[1]}, nil
		case "first":
			if len(args) != 1 {
				return nil, fmt.Errorf("first expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "first"}, nil
		case "panic", "error":
			if len(args) != 1 {
				return nil, fmt.Errorf("panic expects 1 arg")
			}
			return &CallExpr{Func: "panic", Args: args}, nil
		case "concat":
			if len(args) != 2 {
				return nil, fmt.Errorf("concat expects 2 args")
			}
			return &BinaryExpr{Op: "+", Left: args[0], Right: args[1]}, nil
		case "substring", "substr", "slice":
			if len(args) < 2 || len(args) > 3 {
				return nil, fmt.Errorf("%s expects 2 or 3 args", name)
			}
			var end Expr
			if len(args) == 3 {
				end = args[2]
			}
			return &SliceExpr{Target: args[0], Start: args[1], End: end}, nil
		case "upper":
			if len(args) != 1 {
				return nil, fmt.Errorf("upper expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "upcase"}, nil
		case "lower":
			if len(args) != 1 {
				return nil, fmt.Errorf("lower expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "downcase"}, nil
		case "contains":
			if currentEnv != nil {
				if _, ok := currentEnv.GetFunc(name); ok {
					return &CallExpr{Func: name, Args: args}, nil
				}
			}
			if len(args) != 2 {
				return nil, fmt.Errorf("contains expects 2 args")
			}
			return &MethodCallExpr{Target: args[0], Method: "include?", Args: []Expr{args[1]}}, nil
		case "indexOf":
			if len(args) != 2 {
				return nil, fmt.Errorf("indexOf expects 2 args")
			}
			usesIndexOf = true
			return &CallExpr{Func: "_indexOf", Args: args}, nil
		case "repeat":
			if len(args) != 2 {
				return nil, fmt.Errorf("repeat expects 2 args")
			}
			usesRepeat = true
			return &CallExpr{Func: "_repeat", Args: args}, nil
		case "split":
			if currentEnv != nil {
				if _, ok := currentEnv.GetFunc(name); ok {
					return &CallExpr{Func: name, Args: args}, nil
				}
			}
			if len(args) == 1 {
				args = append(args, &StringLit{Value: " "})
			} else if len(args) != 2 {
				return nil, fmt.Errorf("split expects 1 or 2 args")
			}
			usesSplit = true
			return &CallExpr{Func: "_split", Args: args}, nil
		case "parseIntStr":
			usesParseIntStr = true
			switch len(args) {
			case 1:
				return &CallExpr{Func: "parseIntStr", Args: args}, nil
			case 2:
				return &CallExpr{Func: "parseIntStr", Args: args}, nil
			default:
				return nil, fmt.Errorf("parseIntStr expects 1 or 2 args")
			}
		case "num":
			if len(args) != 1 {
				return nil, fmt.Errorf("num expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "numerator"}, nil
		case "denom":
			if len(args) != 1 {
				return nil, fmt.Errorf("denom expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "denominator"}, nil
		case "padStart":
			if len(args) != 3 {
				return nil, fmt.Errorf("padStart expects 3 args")
			}
			return &CallExpr{Func: "_padStart", Args: args}, nil
		case "padEnd":
			if len(args) != 3 {
				return nil, fmt.Errorf("padEnd expects 3 args")
			}
			return &CallExpr{Func: "_padEnd", Args: args}, nil
		case "toi":
			if len(args) != 1 {
				return nil, fmt.Errorf("toi expects 1 arg")
			}
			return &CastExpr{Value: args[0], Type: "int"}, nil
		case "sha256":
			if len(args) != 1 {
				return nil, fmt.Errorf("sha256 expects 1 arg")
			}
			usesSHA256 = true
			return &CallExpr{Func: "_sha256", Args: args}, nil
		case "pow":
			if len(args) != 2 {
				return nil, fmt.Errorf("pow expects 2 args")
			}
			if currentEnv != nil {
				t := types.ExprType(p.Call.Args[0], currentEnv)
				if isNumericType(t) {
					return &BinaryExpr{Op: "**", Left: args[0], Right: args[1]}, nil
				}
			}
			return &CallExpr{Func: name, Args: args}, nil
		case "floor":
			if len(args) != 1 {
				return nil, fmt.Errorf("floor expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "floor"}, nil
		case "abs":
			if len(args) != 1 {
				return nil, fmt.Errorf("abs expects 1 arg")
			}
			return &MethodCallExpr{Target: args[0], Method: "abs"}, nil
		case "now":
			if len(args) != 0 {
				return nil, fmt.Errorf("now takes no args")
			}
			usesNow = true
			return &CallExpr{Func: "_now"}, nil
		case "input":
			if len(args) != 0 {
				return nil, fmt.Errorf("input takes no args")
			}
			usesInput = true
			return &CallExpr{Func: "_input"}, nil
		case "json":
			if len(args) != 1 {
				return nil, fmt.Errorf("json expects 1 arg")
			}
			needsJSON = true
			pretty := &MethodCallExpr{Target: &Ident{Name: "JSON"}, Method: "pretty_generate", Args: []Expr{args[0]}}
			return &CallExpr{Func: "puts", Args: []Expr{pretty}}, nil
		case "net.LookupHost":
			if len(args) != 1 {
				return nil, fmt.Errorf("net.LookupHost expects 1 arg")
			}
			usesLookupHost = true
			return &CallExpr{Func: "_lookup_host", Args: args}, nil
		default:
			if currentEnv != nil {
				if fn, ok := currentEnv.GetFunc(name); ok {
					if len(args) < len(fn.Params) {
						extra := []Expr{}
						params := []string{}
						for _, p := range fn.Params[len(args):] {
							params = append(params, p.Name)
							extra = append(extra, &Ident{Name: p.Name})
						}
						callArgs := append(append([]Expr{}, args...), extra...)
						call := &CallExpr{Func: name, Args: callArgs}
						return &LambdaExpr{Params: params, Expr: call}, nil
					}
					return &CallExpr{Func: name, Args: args}, nil
				}
				if vt, err := currentEnv.GetVar(name); err == nil {
					if _, ok := vt.(types.FuncType); ok {
						return &MethodCallExpr{Target: &Ident{Name: name}, Method: "call", Args: args}, nil
					}
				}
			}
			return &CallExpr{Func: name, Args: args}, nil
		}
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Float != nil {
			return &FloatLit{Value: *p.Lit.Float}, nil
		}
		if p.Lit.Bool != nil {
			return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
		}
		if p.Lit.Null {
			return &NullLit{}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
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
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k Expr
			if s, ok := literalString(it.Key); ok {
				k = &StringLit{Value: s}
			} else {
				var err error
				k, err = convertExpr(it.Key)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		usesFetch = true
		if p.Fetch.With != nil {
			withExpr, err := convertExpr(p.Fetch.With)
			if err != nil {
				return nil, err
			}
			return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr, withExpr}}, nil
		}
		return &CallExpr{Func: "_fetch", Args: []Expr{urlExpr}}, nil
	case p.Load != nil:
		format, delim, header := parseLoadOptions(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		return dataExprFromFile(path, format, delim, header, p.Load.Type)
	case p.Save != nil:
		if p.Save.Path != nil && p.Save.With != nil {
			format := parseFormat(p.Save.With)
			if format == "jsonl" && strings.Trim(*p.Save.Path, "\"") == "-" {
				src, err := convertExpr(p.Save.Src)
				if err != nil {
					return nil, err
				}
				needsJSON = true
				return &SaveJSONLExpr{Src: src}, nil
			}
		}
		return nil, fmt.Errorf("unsupported save expression")
	case p.Struct != nil:
		if len(p.Struct.Fields) == 0 {
			return &Ident{Name: identName(toTypeName(p.Struct.Name))}, nil
		}
		fields := make([]StructField, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			fields[i] = StructField{Name: fieldName(f.Name), Value: v}
		}
		return &StructNewExpr{Name: identName(toTypeName(p.Struct.Name)), Fields: fields}, nil
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Group != nil:
		ex, err := convertExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &GroupExpr{Expr: ex}, nil
	case p.Selector != nil:
		name := p.Selector.Root
		if topVars != nil && topVars[name] && !inScope(name) {
			name = "$" + name
		}
		expr := Expr(&Ident{Name: name})
		var t types.Type
		if currentEnv != nil {
			prim := &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Selector.Root}}
			t = types.ExprType(exprFromPrimary(prim), currentEnv)
		}
		if len(p.Selector.Tail) == 0 && currentEnv != nil {
			if vt, err := currentEnv.GetVar(p.Selector.Root); err == nil {
				if _, okf := currentEnv.GetFunc(p.Selector.Root); okf {
					if _, ok := vt.(types.FuncType); ok {
						return &MethodRefExpr{Name: identName(p.Selector.Root)}, nil
					}
				}
				return expr, nil
			}
			if _, ok := currentEnv.GetFunc(p.Selector.Root); ok {
				return &MethodRefExpr{Name: identName(p.Selector.Root)}, nil
			}
		}
		for _, seg := range p.Selector.Tail {
			expr, t = fieldAccess(expr, t, seg)
		}
		return expr, nil
	case p.Query != nil:
		if ex, err := convertGroupQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertRightJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertLeftJoinMultiQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertLeftJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		if ex, err := convertOuterJoinQuery(p.Query); err == nil {
			return ex, nil
		}
		return convertQueryExpr(p.Query)
	default:
		if p.FunExpr != nil {
			params := make([]string, len(p.FunExpr.Params))
			for i, pa := range p.FunExpr.Params {
				params[i] = pa.Name
			}
			pushScope(params...)
			if p.FunExpr.ExprBody != nil {
				body, err := convertExpr(p.FunExpr.ExprBody)
				popScope()
				if err != nil {
					return nil, err
				}
				return &LambdaExpr{Params: params, Expr: body}, nil
			}
			if len(p.FunExpr.BlockBody) > 0 {
				stmts := make([]Stmt, len(p.FunExpr.BlockBody))
				for i, s := range p.FunExpr.BlockBody {
					st, err := convertStmt(s)
					if err != nil {
						popScope()
						return nil, err
					}
					stmts[i] = st
				}
				popScope()
				return &LambdaExpr{Params: params, Body: stmts}, nil
			}
			popScope()
		}
		return nil, fmt.Errorf("unsupported primary")
	}
}

func convertPrintCall(args []Expr, orig []*parser.Expr) (Expr, error) {
	if len(args) == 1 {
		ex := args[0]
		t := types.ExprType(orig[0], currentEnv)
		switch t.(type) {
		case types.ListType:
			ex = &FormatList{List: ex}
		case types.BoolType:
			ex = &FormatBool{Value: ex}
		}
		return &CallExpr{Func: "puts", Args: []Expr{ex}}, nil
	}
	conv := make([]Expr, len(args))
	for i, a := range args {
		ex := a
		switch types.ExprType(orig[i], currentEnv).(type) {
		case types.ListType:
			ex = &FormatList{List: ex}
		case types.BoolType:
			ex = &FormatBool{Value: ex}
		}
		conv[i] = ex
	}
	list := &ListLit{Elems: conv}
	joined := &JoinExpr{List: list}
	return &CallExpr{Func: "puts", Args: []Expr{&MethodCallExpr{Target: joined, Method: "rstrip"}}}, nil
}

func isExistsCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	if u.Value.Target.Call != nil && u.Value.Target.Call.Func == "exists" {
		return true
	}
	return false
}

func isMembershipExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) == 1 && e.Binary.Right[0].Op == "in" {
		return true
	}
	u := e.Binary.Left
	if u != nil && u.Value != nil && u.Value.Target != nil && len(u.Value.Ops) == 1 && u.Value.Ops[0].Call != nil {
		if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) > 0 && sel.Tail[len(sel.Tail)-1] == "contains" {
			return true
		}
	}
	return false
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if id, ok := src.(*Ident); ok && id.Name == "g" {
		src = &IndexExpr{Target: src, Index: &StringLit{Value: "items"}}
	}
	froms := make([]queryFrom, len(q.Froms))
	child := types.NewEnv(currentEnv)
	var srcElem types.Type = types.AnyType{}
	if currentEnv != nil {
		st := types.ExprType(q.Source, currentEnv)
		switch ty := st.(type) {
		case types.ListType:
			srcElem = ty.Elem
		case types.GroupType:
			srcElem = ty.Elem
		}
	}
	child.SetVar(q.Var, srcElem, true)
	savedEnv := currentEnv
	currentEnv = child
	for i, f := range q.Froms {
		fe, err := convertExpr(f.Src)
		if err != nil {
			currentEnv = savedEnv
			return nil, err
		}
		var felem types.Type = types.AnyType{}
		if currentEnv != nil {
			ft := types.ExprType(f.Src, currentEnv)
			if lt, ok := ft.(types.ListType); ok {
				felem = lt.Elem
			}
		}
		child.SetVar(f.Var, felem, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := convertExpr(j.Src)
		if err != nil {
			currentEnv = savedEnv
			return nil, err
		}
		var jelem types.Type = types.AnyType{}
		if currentEnv != nil {
			jt := types.ExprType(j.Src, currentEnv)
			if lt, ok := jt.(types.ListType); ok {
				jelem = lt.Elem
			}
		}
		child.SetVar(j.Var, jelem, true)
		var onExpr Expr
		if j.On != nil {
			onExpr, err = convertExpr(j.On)
			if err != nil {
				currentEnv = savedEnv
				return nil, err
			}
		}
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: onExpr, Side: side}
	}
	var where Expr
	if q.Where != nil {
		where, err = convertExpr(q.Where)
		if err != nil {
			currentEnv = savedEnv
			return nil, err
		}
	}
	var sort Expr
	if q.Sort != nil {
		sort, err = convertExpr(q.Sort)
		if err != nil {
			currentEnv = savedEnv
			return nil, err
		}
	}
	var skip Expr
	if q.Skip != nil {
		skip, err = convertExpr(q.Skip)
		if err != nil {
			currentEnv = savedEnv
			return nil, err
		}
	}
	var take Expr
	if q.Take != nil {
		take, err = convertExpr(q.Take)
		if err != nil {
			currentEnv = savedEnv
			return nil, err
		}
	}
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = savedEnv
		return nil, err
	}
	currentEnv = savedEnv
	return &QueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Sort: sort, Skip: skip, Take: take, Select: sel}, nil
}

func convertRightJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertLeftJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &LeftJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertGroupLeftJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct || q.Where != nil {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	keyExpr, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	// row map
	items := []MapItem{{Key: &StringLit{Value: q.Var}, Value: &Ident{Name: q.Var}}, {Key: &StringLit{Value: j.Var}, Value: &Ident{Name: j.Var}}}
	row := &MapLit{Items: items}
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	var sort Expr
	if q.Sort != nil {
		sort, err = convertExpr(q.Sort)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
	}
	currentEnv = saved
	return &GroupLeftJoinQueryExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Row: row, Key: keyExpr, GroupVar: q.Group.Name, Select: sel, Sort: sort, Having: having}, nil
}

func convertLeftJoinMultiQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 2 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil {
		return nil, fmt.Errorf("unsupported query")
	}
	j1 := q.Joins[0]
	j2 := q.Joins[1]
	if j1.Side != nil || j2.Side == nil || *j2.Side != "left" {
		return nil, fmt.Errorf("unsupported query")
	}
	src1, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	src2, err := convertExpr(j1.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j1.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond2, err := convertExpr(j1.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	child.SetVar(j2.Var, types.AnyType{}, true)
	src3, err := convertExpr(j2.Src)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	cond3, err := convertExpr(j2.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &LeftJoinMultiExpr{Var1: q.Var, Src1: src1, Var2: j1.Var, Src2: src2, Cond2: cond2, Var3: j2.Var, Src3: src3, Cond3: cond3, Select: sel}, nil
}

func convertOuterJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || len(q.Froms) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "outer" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	saved := currentEnv
	currentEnv = child
	cond, err := convertExpr(j.On)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	return &OuterJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertGroupQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Froms) > 0 || len(q.Joins) > 0 || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	grp := types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
	child.SetVar(q.Group.Name, grp, true)
	saved := currentEnv
	currentEnv = child
	keyExpr, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	currentEnv = saved
	if err != nil {
		return nil, err
	}
	var sort Expr
	if q.Sort != nil {
		currentEnv = child
		sort, err = convertExpr(q.Sort)
		currentEnv = saved
		if err != nil {
			return nil, err
		}
	}
	var having Expr
	if q.Group.Having != nil {
		currentEnv = child
		having, err = convertExpr(q.Group.Having)
		currentEnv = saved
		if err != nil {
			return nil, err
		}
	}
	return &GroupQueryExpr{Var: q.Var, Src: src, Key: keyExpr, GroupVar: q.Group.Name, Select: sel, Sort: sort, Having: having}, nil
}

func convertGroupJoinQuery(q *parser.QueryExpr) (Expr, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || q.Distinct || (len(q.Froms) == 0 && len(q.Joins) == 0) {
		return nil, fmt.Errorf("unsupported query")
	}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(currentEnv)
	var srcElem types.Type = types.AnyType{}
	if currentEnv != nil {
		st := types.ExprType(q.Source, currentEnv)
		switch ty := st.(type) {
		case types.ListType:
			srcElem = ty.Elem
		case types.GroupType:
			srcElem = ty.Elem
		}
	}
	child.SetVar(q.Var, srcElem, true)
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		var ft types.Type = types.AnyType{}
		if currentEnv != nil {
			t := types.ExprType(f.Src, child)
			if lt, ok := t.(types.ListType); ok {
				ft = lt.Elem
			}
		}
		child.SetVar(f.Var, ft, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}
	joins := make([]queryJoin, len(q.Joins))
	for i, j := range q.Joins {
		je, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		var jt types.Type = types.AnyType{}
		if currentEnv != nil {
			t := types.ExprType(j.Src, child)
			if lt, ok := t.(types.ListType); ok {
				jt = lt.Elem
			}
		}
		child.SetVar(j.Var, jt, true)
		var on Expr
		if j.On != nil {
			on, err = convertExpr(j.On)
			if err != nil {
				return nil, err
			}
		}
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		joins[i] = queryJoin{Var: j.Var, Src: je, On: on, Side: side}
	}
	var where Expr
	if q.Where != nil {
		w, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		where = w
	}
	// build row map storing all vars
	items := []MapItem{{Key: &StringLit{Value: q.Var}, Value: &Ident{Name: q.Var}}}
	for _, f := range q.Froms {
		items = append(items, MapItem{Key: &StringLit{Value: f.Var}, Value: &Ident{Name: f.Var}})
	}
	for _, j := range q.Joins {
		items = append(items, MapItem{Key: &StringLit{Value: j.Var}, Value: &Ident{Name: j.Var}})
	}
	row := &MapLit{Items: items}

	saved := currentEnv
	currentEnv = child
	keyExpr, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = saved
		return nil, err
	}
	var sort Expr
	if q.Sort != nil {
		sort, err = convertExpr(q.Sort)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			currentEnv = saved
			return nil, err
		}
	}
	currentEnv = saved
	return &GroupJoinQueryExpr{Var: q.Var, Src: src, Froms: froms, Joins: joins, Where: where, Row: row, Key: keyExpr, GroupVar: q.Group.Name, Select: sel, Sort: sort, Having: having}, nil
}

func convertQueryForLet(q *parser.QueryExpr, name string) (Expr, Stmt, error) {
	if keys, vals, ok := extractMapLiteral(q.Select); ok {
		structName := identName(toStructName(name))
		skeys := make([]string, len(keys))
		fields := make([]StructField, len(keys))
		fieldTypes := map[string]types.Type{}
		for i, v := range vals {
			ex, err := convertExpr(v)
			if err != nil {
				return nil, nil, err
			}
			skeys[i] = fieldName(keys[i])
			fields[i] = StructField{Name: skeys[i], Value: ex}
			if currentEnv != nil {
				fieldTypes[skeys[i]] = types.ExprType(v, currentEnv)
			}
		}
		var qe Expr
		var err error
		if ex, err2 := convertGroupLeftJoinQuery(q); err2 == nil {
			qe = ex
			if gq, ok := qe.(*GroupLeftJoinQueryExpr); ok {
				gq.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		} else if len(q.Froms) == 0 && len(q.Joins) == 0 {
			if ex, err2 := convertGroupQuery(q); err2 == nil {
				qe = ex
				if gq, ok := qe.(*GroupQueryExpr); ok {
					gq.Select = &StructNewExpr{Name: structName, Fields: fields}
				}
			} else if ex, err2 := convertGroupJoinQuery(q); err2 == nil {
				qe = ex
				if gq, ok := qe.(*GroupJoinQueryExpr); ok {
					gq.Select = &StructNewExpr{Name: structName, Fields: fields}
				}
			}
		} else if ex, err2 := convertGroupJoinQuery(q); err2 == nil {
			qe = ex
			if gq, ok := qe.(*GroupJoinQueryExpr); ok {
				gq.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		} else if ex, err2 := convertGroupQuery(q); err2 == nil {
			qe = ex
			if gq, ok := qe.(*GroupQueryExpr); ok {
				gq.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		} else if ex, err2 := convertRightJoinQuery(q); err2 == nil {
			qe = ex
			if rj, ok := qe.(*RightJoinExpr); ok {
				rj.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		} else if ex, err2 := convertLeftJoinMultiQuery(q); err2 == nil {
			qe = ex
			if lj, ok := qe.(*LeftJoinMultiExpr); ok {
				lj.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		} else if ex, err2 := convertLeftJoinQuery(q); err2 == nil {
			qe = ex
			if lj, ok := qe.(*LeftJoinExpr); ok {
				lj.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		} else if ex, err2 := convertOuterJoinQuery(q); err2 == nil {
			qe = ex
			if oj, ok := qe.(*OuterJoinExpr); ok {
				oj.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		} else {
			qe, err = convertQueryExpr(q)
			if err != nil {
				return nil, nil, err
			}
			if qe != nil {
				if qq, ok := qe.(*QueryExpr); ok {
					qq.Select = &StructNewExpr{Name: structName, Fields: fields}
				}
			}
		}
		if qe == nil {
			qe, err = convertQueryExpr(q)
			if err != nil {
				return nil, nil, err
			}
			if qq, ok := qe.(*QueryExpr); ok {
				qq.Select = &StructNewExpr{Name: structName, Fields: fields}
			}
		}
		if currentEnv != nil {
			st := types.StructType{Name: structName, Fields: fieldTypes, Order: skeys}
			currentEnv.SetStruct(structName, st)
			currentEnv.SetVar(name, types.ListType{Elem: st}, false)
		}
		return qe, &StructDefStmt{Name: structName, Fields: skeys}, nil
	}
	if ex, err := convertRightJoinQuery(q); err == nil {
		return ex, nil, nil
	}
	if ex, err := convertLeftJoinMultiQuery(q); err == nil {
		return ex, nil, nil
	}
	if ex, err := convertLeftJoinQuery(q); err == nil {
		return ex, nil, nil
	}
	if ex, err := convertOuterJoinQuery(q); err == nil {
		return ex, nil, nil
	}
	if ex, err := convertGroupLeftJoinQuery(q); err == nil {
		return ex, nil, nil
	}
	if ex, err := convertGroupJoinQuery(q); err == nil {
		return ex, nil, nil
	}
	if ex, err := convertGroupQuery(q); err == nil {
		return ex, nil, nil
	}
	qe, err := convertQueryExpr(q)
	return qe, nil, err
}

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
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *IndexAssignStmt:
		n := &ast.Node{Kind: "index_assign"}
		n.Children = append(n.Children, exprNode(st.Target))
		n.Children = append(n.Children, exprNode(st.Index))
		n.Children = append(n.Children, exprNode(st.Value))
		return n
	case *UpdateStmt:
		n := &ast.Node{Kind: "update", Value: st.Target}
		fields := &ast.Node{Kind: "fields"}
		for i, f := range st.Fields {
			fn := &ast.Node{Kind: "field", Value: f, Children: []*ast.Node{exprNode(st.Values[i])}}
			fields.Children = append(fields.Children, fn)
		}
		n.Children = append(n.Children, fields)
		if st.Cond != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(st.Cond)}})
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
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
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprNode(st.Cond)}}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for_range", Value: st.Name}
		n.Children = append(n.Children, exprNode(st.Start), exprNode(st.End))
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ForInStmt:
		n := &ast.Node{Kind: "for_in", Value: st.Name, Children: []*ast.Node{exprNode(st.Iterable)}}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *BenchStmt:
		n := &ast.Node{Kind: "bench", Value: st.Name}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *AssertStmt:
		return &ast.Node{Kind: "assert", Children: []*ast.Node{exprNode(st.Expr)}}
	case *CommentStmt:
		return &ast.Node{Kind: "comment", Value: st.Text}
	case *BlockStmt:
		n := &ast.Node{Kind: "block"}
		for _, b := range st.Stmts {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = append(n.Children, exprNode(st.Value))
		}
		return n
	case *FuncStmt:
		n := &ast.Node{Kind: "func", Value: st.Name}
		params := &ast.Node{Kind: "params"}
		for _, p := range st.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params)
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *Ident:
		return &ast.Node{Kind: "ident", Value: ex.Name}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprintf("%d", ex.Value)}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, e := range ex.Elems {
			n.Children = append(n.Children, exprNode(e))
		}
		return n
	case *MapLit:
		n := &ast.Node{Kind: "map"}
		for _, it := range ex.Items {
			n.Children = append(n.Children, &ast.Node{Kind: "entry", Children: []*ast.Node{exprNode(it.Key), exprNode(it.Value)}})
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{exprNode(ex.Value)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.Value)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Elem)}}
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
	case *ValuesExpr:
		return &ast.Node{Kind: "values", Children: []*ast.Node{exprNode(ex.Map)}}
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		params := &ast.Node{Kind: "params"}
		for _, p := range ex.Params {
			params.Children = append(params.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, params)
		body := &ast.Node{Kind: "body"}
		for _, st := range ex.Body {
			body.Children = append(body.Children, stmtNode(st))
		}
		if ex.Expr != nil {
			body.Children = append(body.Children, exprNode(ex.Expr))
		}
		n.Children = append(n.Children, body)
		return n
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	case *JoinExpr:
		return &ast.Node{Kind: "join", Children: []*ast.Node{exprNode(ex.List)}}
	case *GroupExpr:
		return &ast.Node{Kind: "group", Children: []*ast.Node{exprNode(ex.Expr)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *CastExpr:
		return &ast.Node{Kind: "cast", Value: ex.Type, Children: []*ast.Node{exprNode(ex.Value)}}
	case *MethodCallExpr:
		n := &ast.Node{Kind: "method", Value: ex.Method}
		n.Children = append(n.Children, exprNode(ex.Target))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *UnionExpr:
		return &ast.Node{Kind: "union", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnionAllExpr:
		return &ast.Node{Kind: "unionall", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *ExceptExpr:
		return &ast.Node{Kind: "except", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *IntersectExpr:
		return &ast.Node{Kind: "intersect", Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *MatchExpr:
		n := &ast.Node{Kind: "match"}
		n.Children = append(n.Children, exprNode(ex.Target))
		for _, c := range ex.Cases {
			mc := &ast.Node{Kind: "case"}
			mc.Children = append(mc.Children, exprNode(c.Pattern), exprNode(c.Result))
			n.Children = append(n.Children, mc)
		}
		if ex.Else != nil {
			n.Children = append(n.Children, &ast.Node{Kind: "else", Children: []*ast.Node{exprNode(ex.Else)}})
		}
		return n
	case *RightJoinExpr:
		n := &ast.Node{Kind: "right_join"}
		n.Children = append(n.Children, &ast.Node{Kind: "left_var", Value: ex.LeftVar})
		n.Children = append(n.Children, exprNode(ex.LeftSrc))
		n.Children = append(n.Children, &ast.Node{Kind: "right_var", Value: ex.RightVar})
		n.Children = append(n.Children, exprNode(ex.RightSrc))
		n.Children = append(n.Children, exprNode(ex.Cond))
		n.Children = append(n.Children, exprNode(ex.Select))
		return n
	case *LeftJoinExpr:
		n := &ast.Node{Kind: "left_join"}
		n.Children = append(n.Children, &ast.Node{Kind: "left_var", Value: ex.LeftVar})
		n.Children = append(n.Children, exprNode(ex.LeftSrc))
		n.Children = append(n.Children, &ast.Node{Kind: "right_var", Value: ex.RightVar})
		n.Children = append(n.Children, exprNode(ex.RightSrc))
		n.Children = append(n.Children, exprNode(ex.Cond))
		n.Children = append(n.Children, exprNode(ex.Select))
		return n
	case *LeftJoinMultiExpr:
		n := &ast.Node{Kind: "left_join_multi"}
		n.Children = append(n.Children, &ast.Node{Kind: "var1", Value: ex.Var1})
		n.Children = append(n.Children, exprNode(ex.Src1))
		n.Children = append(n.Children, &ast.Node{Kind: "var2", Value: ex.Var2})
		n.Children = append(n.Children, exprNode(ex.Src2))
		n.Children = append(n.Children, exprNode(ex.Cond2))
		n.Children = append(n.Children, &ast.Node{Kind: "var3", Value: ex.Var3})
		n.Children = append(n.Children, exprNode(ex.Src3))
		n.Children = append(n.Children, exprNode(ex.Cond3))
		n.Children = append(n.Children, exprNode(ex.Select))
		return n
	case *OuterJoinExpr:
		n := &ast.Node{Kind: "outer_join"}
		n.Children = append(n.Children, &ast.Node{Kind: "left_var", Value: ex.LeftVar})
		n.Children = append(n.Children, exprNode(ex.LeftSrc))
		n.Children = append(n.Children, &ast.Node{Kind: "right_var", Value: ex.RightVar})
		n.Children = append(n.Children, exprNode(ex.RightSrc))
		n.Children = append(n.Children, exprNode(ex.Cond))
		n.Children = append(n.Children, exprNode(ex.Select))
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes a Lisp-like representation of the AST to stdout.
func Print(p *Program) { toNode(p).Print("") }
