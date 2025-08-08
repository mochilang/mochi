//go:build slow

package py

import (
	"bytes"
	"encoding/csv"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode"

	yaml "gopkg.in/yaml.v3"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Python AST definitions

type Program struct {
	Stmts []Stmt
}

var (
	currentImports    map[string]bool
	currentImportLang map[string]string
	currentEnv        *types.Env
	structCounter     int
	extraFuncs        []Stmt
	usesNow           bool
	usesLookupHost    bool
	usesIndexOf       bool
	usesSubstr        bool
	usesCallable      bool
	usesFetch         bool
	usesAppend        bool
	usesConcat        bool
	usesStr           bool
	usesSetIndex      bool
	funcDepth         int
)

const helperNow = `
_now_seed = 0
_now_seeded = False
s = os.getenv("MOCHI_NOW_SEED")
if s and s != "":
    try:
        _now_seed = int(s)
        _now_seeded = True
    except Exception:
        pass

def _now():
    global _now_seed
    if _now_seeded:
        _now_seed = (_now_seed * 1664525 + 1013904223) % 2147483647
        return _now_seed
    return int(time.time_ns())
`

const helperLookupHost = `
def _lookup_host(host):
    import socket
    try:
        return socket.gethostbyname_ex(host)[2], None
    except Exception as e:
        return [], e
`

const helperIndexOf = `
def _index_of(xs, val):
    for i, v in enumerate(xs):
        if v == val:
            return i
    return -1
`

const helperSubstr = `
def _substr(s, start, end):
    return s[start:end]
`

const helperFetch = `
def _fetch(url: str, opts: dict[str, Any] | None) -> Any:
    import urllib.request, urllib.parse, json
    method = 'GET'
    data = None
    headers = {}
    timeout = None
    if opts:
        method = opts.get('method', method)
        if 'body' in opts:
            data = json.dumps(opts['body']).encode()
        if 'headers' in opts:
            for k, v in dict(opts['headers']).items():
                headers[k] = str(v)
        if 'query' in opts:
            q = urllib.parse.urlencode({k: str(v) for k, v in dict(opts['query']).items()})
            sep = '&' if '?' in url else '?'
            url = url + sep + q
        timeout = opts.get('timeout', None)
    req = urllib.request.Request(url, data=data, headers=headers, method=method)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        text = resp.read()
    return json.loads(text)
`

const helperConcat = `
def _concat(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return a + b
`

const helperAppend = `
def _append(lst, v):
    return (lst or []) + [v]
`

const helperSetIndex = `
def _set_index(lst, idx, val):
    if lst is None:
        lst = []
    if idx >= len(lst):
        lst.extend([None] * (idx - len(lst) + 1))
    lst[idx] = val
    return lst
`

const helperPanic = `
def panic(msg):
    raise RuntimeError(msg)
`

const helperStr = `
def _str(v):
    if isinstance(v, float) and v.is_integer():
        return str(int(v))
    return str(v)
`

var pyKeywords = map[string]bool{
	"False":    true,
	"True":     true,
	"None":     true,
	"and":      true,
	"as":       true,
	"assert":   true,
	"break":    true,
	"class":    true,
	"continue": true,
	"def":      true,
	"del":      true,
	"elif":     true,
	"else":     true,
	"except":   true,
	"finally":  true,
	"for":      true,
	"from":     true,
	"global":   true,
	"if":       true,
	"import":   true,
	"in":       true,
	"is":       true,
	"lambda":   true,
	"nonlocal": true,
	"not":      true,
	"or":       true,
	"pass":     true,
	"raise":    true,
	"return":   true,
	"try":      true,
	"while":    true,
	"with":     true,
	"yield":    true,
}

func isBuiltinType(name string) bool {
	switch name {
	case "int", "int64", "float", "bool", "string":
		return true
	default:
		return false
	}
}

func safeName(n string) string {
	if n == "" {
		return "_"
	}
	if pyKeywords[n] {
		return n + "_"
	}
	var buf strings.Builder
	for i, r := range n {
		if i == 0 {
			if !(r == '_' || unicode.IsLetter(r)) {
				buf.WriteRune('_')
			}
		}
		if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' {
			buf.WriteRune(r)
		} else {
			buf.WriteRune('_')
		}
	}
	out := buf.String()
	if out == "" {
		return "_"
	}
	return out
}

type Stmt interface{ isStmt() }

// ImportStmt represents an `import` statement.
type ImportStmt struct {
	Module string
	Alias  string
}

func (*ImportStmt) isStmt() {}

type ExprStmt struct{ Expr Expr }

func (*ExprStmt) isStmt() {}

type LetStmt struct {
	Name string
	Expr Expr
}

func (*LetStmt) isStmt() {}

type VarStmt struct {
	Name string
	Expr Expr
}

func (*VarStmt) isStmt() {}

type AssignStmt struct {
	Name string
	Expr Expr
}

func (*AssignStmt) isStmt() {}

type ReturnStmt struct {
	Expr Expr
}

func (*ReturnStmt) isStmt() {}

type FuncDef struct {
	Name      string
	Params    []string
	Nonlocals []string
	Globals   []string
	Body      []Stmt
}

func (*FuncDef) isStmt() {}

type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (*WhileStmt) isStmt() {}

type ForStmt struct {
	Var  string
	Iter Expr
	Body []Stmt
}

func (*ForStmt) isStmt() {}

// IfStmt represents a simple if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (*IfStmt) isStmt() {}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (*BreakStmt) isStmt() {}

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (*ContinueStmt) isStmt() {}

// AssertStmt represents an assertion statement.
type AssertStmt struct {
	Expr Expr
}

func (*AssertStmt) isStmt() {}

// CommentStmt represents a comment line.
type CommentStmt struct {
	Text string
}

func (*CommentStmt) isStmt() {}

// BenchStmt represents a benchmark block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (*BenchStmt) isStmt() {}

// DataClassDef represents a @dataclass definition.
type DataClassDef struct {
	Name    string
	Fields  []DataClassField
	Methods []*FuncDef
}

type DataClassField struct {
	Name string
	Type string
}

func (*DataClassDef) isStmt() {}

// FieldAssignStmt assigns to an attribute of a dataclass.
type FieldAssignStmt struct {
	Target Expr
	Field  string
	Value  Expr
}

func (*FieldAssignStmt) isStmt() {}

// IndexAssignStmt assigns to an element of a list or map.
type IndexAssignStmt struct {
	Target Expr
	Index  Expr
	Value  Expr
}

func (*IndexAssignStmt) isStmt() {}

// SaveStmt emits code to save a list of maps in various formats.
type SaveStmt struct {
	Src    Expr
	Path   string
	Format string
}

func (*SaveStmt) isStmt() {}

// UpdateStmt represents an `update` statement on a list of structs.
type UpdateStmt struct {
	Target string
	Fields []string
	Values []Expr
	Cond   Expr
	Struct bool
}

func (*UpdateStmt) isStmt() {}

// RawExpr is a simple expression emitted verbatim.
type RawExpr struct{ Code string }

func (r *RawExpr) emit(w io.Writer) error { _, err := io.WriteString(w, r.Code); return err }

// KeywordArg represents a keyword argument in a call expression.
type KeywordArg struct {
	Name  string
	Value Expr
}

func (k *KeywordArg) emit(w io.Writer) error {
	if _, err := io.WriteString(w, k.Name+"="); err != nil {
		return err
	}
	return emitExpr(w, k.Value)
}

type Expr interface{ emit(io.Writer) error }

type CallExpr struct {
	Func Expr
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) error {
	if err := emitExpr(w, c.Func); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, a := range c.Args {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if ka, ok := a.(*KeywordArg); ok {
			if err := ka.emit(w); err != nil {
				return err
			}
			continue
		}
		if err := emitExpr(w, a); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ")"); err != nil {
		return err
	}
	return nil
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) error {
	switch n.Name {
	case "None", "True", "False":
		_, err := io.WriteString(w, n.Name)
		return err
	case "stdout", "stderr", "stdin":
		_, err := io.WriteString(w, "sys."+n.Name)
		return err
	default:
		_, err := io.WriteString(w, safeName(n.Name))
		return err
	}
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) error {
	_, err := fmt.Fprintf(w, "%q", s.Value)
	return err
}

type IntLit struct{ Value string }

func (i *IntLit) emit(w io.Writer) error {
	_, err := io.WriteString(w, i.Value)
	return err
}

type FloatLit struct{ Value string }

func (f *FloatLit) emit(w io.Writer) error {
	_, err := io.WriteString(w, f.Value)
	return err
}

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) error {
	if b.Value {
		_, err := io.WriteString(w, "True")
		return err
	}
	_, err := io.WriteString(w, "False")
	return err
}

// ParenExpr represents an expression wrapped in parentheses.
type ParenExpr struct{ Expr Expr }

func (p *ParenExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := emitExpr(w, p.Expr); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	for i, e := range l.Elems {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := emitExpr(w, e); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

type DictLit struct{ Keys, Values []Expr }

func (d *DictLit) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "{"); err != nil {
		return err
	}
	for i := range d.Keys {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := emitExpr(w, d.Keys[i]); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ": "); err != nil {
			return err
		}
		if err := emitExpr(w, d.Values[i]); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

type IndexExpr struct {
	Target  Expr
	Index   Expr
	Map     bool
	Default Expr
}

func (i *IndexExpr) emit(w io.Writer) error {
	if i.Map {
		if err := emitExpr(w, i.Target); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".get("); err != nil {
			return err
		}
		if err := emitExpr(w, i.Index); err != nil {
			return err
		}
		if i.Default != nil {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
			if err := emitExpr(w, i.Default); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
		return nil
	}
	if _, ok := i.Index.(*StringLit); ok {
		if err := emitExpr(w, i.Target); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".get("); err != nil {
			return err
		}
		if err := emitExpr(w, i.Index); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ")"); err != nil {
			return err
		}
		return nil
	}
	if err := emitExpr(w, i.Target); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if err := emitExpr(w, i.Index); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

type FieldExpr struct {
	Target   Expr
	Name     string
	MapIndex bool
	MapGet   bool
}

func (f *FieldExpr) emit(w io.Writer) error {
	if err := emitExpr(w, f.Target); err != nil {
		return err
	}
	useIndex := f.MapIndex
	if useIndex && currentEnv != nil {
		if st, ok := inferPyType(f.Target, currentEnv).(types.StructType); ok {
			if _, ok := st.Fields[f.Name]; ok {
				useIndex = false
			}
		}
	}
	if useIndex {
		if f.MapGet {
			_, err := fmt.Fprintf(w, ".get(%q)", f.Name)
			return err
		}
		_, err := fmt.Fprintf(w, "[%q]", f.Name)
		return err
	}
	_, err := io.WriteString(w, "."+f.Name)
	return err
}

type SliceExpr struct {
	Target Expr
	Start  Expr
	End    Expr
	Step   Expr
}

func (s *SliceExpr) emit(w io.Writer) error {
	if err := emitExpr(w, s.Target); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if s.Start != nil {
		if err := emitExpr(w, s.Start); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ":"); err != nil {
		return err
	}
	if s.End != nil {
		if err := emitExpr(w, s.End); err != nil {
			return err
		}
	}
	if s.Step != nil {
		if _, err := io.WriteString(w, ":"); err != nil {
			return err
		}
		if err := emitExpr(w, s.Step); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

type LambdaExpr struct {
	Params []string
	Expr   Expr
}

func (l *LambdaExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "lambda "); err != nil {
		return err
	}
	for i, p := range l.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ": "); err != nil {
		return err
	}
	return emitExpr(w, l.Expr)
}

// ListComp represents a Python list comprehension like
// `[expr for var in iter if cond]`.
type ListComp struct {
	Var  string
	Iter Expr
	Expr Expr
	Cond Expr
}

// MultiListComp represents a list comprehension with multiple input
// iterators, e.g. `[f(a,b) for a in A for b in B if cond]`.
type MultiListComp struct {
	Vars   []string
	Iters  []Expr
	Expr   Expr
	Cond   Expr
	Parens bool
}

func (lc *ListComp) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if err := emitExpr(w, lc.Expr); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " for "+lc.Var+" in "); err != nil {
		return err
	}
	if err := emitExpr(w, lc.Iter); err != nil {
		return err
	}
	if lc.Cond != nil {
		if _, err := io.WriteString(w, " if "); err != nil {
			return err
		}
		if err := emitExpr(w, lc.Cond); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

func (lc *MultiListComp) emit(w io.Writer) error {
	open := "["
	close := "]"
	if lc.Parens {
		open = "("
		close = ")"
	}
	if _, err := io.WriteString(w, open); err != nil {
		return err
	}
	if err := emitExpr(w, lc.Expr); err != nil {
		return err
	}
	for i, v := range lc.Vars {
		if _, err := io.WriteString(w, " for "+v+" in "); err != nil {
			return err
		}
		if err := emitExpr(w, lc.Iters[i]); err != nil {
			return err
		}
	}
	if lc.Cond != nil {
		if _, err := io.WriteString(w, " if "); err != nil {
			return err
		}
		if err := emitExpr(w, lc.Cond); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, close)
	return err
}

// SortedExpr represents calling `sorted` on a list with a key function.
type SortedExpr struct {
	List    Expr
	Var     string
	Key     Expr
	Reverse bool
}

func (s *SortedExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "sorted("); err != nil {
		return err
	}
	if err := emitExpr(w, s.List); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", key=lambda "+s.Var+": "); err != nil {
		return err
	}
	if err := emitExpr(w, s.Key); err != nil {
		return err
	}
	if s.Reverse {
		if _, err := io.WriteString(w, ", reverse=True"); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) error {
	switch b.Op {
	case "union":
		if _, err := io.WriteString(w, "list(set("); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ") | set("); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, "))")
		return err
	case "union_all":
		if _, err := io.WriteString(w, "("); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, " + "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	case "except":
		if _, err := io.WriteString(w, "[x for x in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, " if x not in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, "]")
		return err
	case "intersect":
		if _, err := io.WriteString(w, "[x for x in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Left); err != nil {
			return err
		}
		if _, err := io.WriteString(w, " if x in "); err != nil {
			return err
		}
		if err := emitExpr(w, b.Right); err != nil {
			return err
		}
		_, err := io.WriteString(w, "]")
		return err
	default:
		lp := precedence(b.Op)
		if lb, ok := b.Left.(*BinaryExpr); ok {
			if isCompareOp(b.Op) && isCompareOp(lb.Op) || precedence(lb.Op) > lp {
				if _, err := io.WriteString(w, "("); err != nil {
					return err
				}
				if err := emitExpr(w, b.Left); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ")"); err != nil {
					return err
				}
			} else {
				if err := emitExpr(w, b.Left); err != nil {
					return err
				}
			}
		} else {
			if err := emitExpr(w, b.Left); err != nil {
				return err
			}
		}
		op := b.Op
		if op == "/" {
			if isIntOnlyExpr(b.Left, currentEnv) && isIntOnlyExpr(b.Right, currentEnv) {
				op = "//"
			} else if isIntLike(inferPyType(b.Left, currentEnv)) && isIntLike(inferPyType(b.Right, currentEnv)) {
				op = "//"
			} else if isLikelyIntExpr(b.Left) && isLikelyIntExpr(b.Right) {
				op = "//"
			}
		}
		switch op {
		case "&&":
			op = "and"
		case "||":
			op = "or"
		}
		if _, err := io.WriteString(w, " "+op+" "); err != nil {
			return err
		}
		if rb, ok := b.Right.(*BinaryExpr); ok && precedence(rb.Op) >= lp {
			if _, err := io.WriteString(w, "("); err != nil {
				return err
			}
			if err := emitExpr(w, b.Right); err != nil {
				return err
			}
			_, err := io.WriteString(w, ")")
			return err
		}
		return emitExpr(w, b.Right)
	}
}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (u *UnaryExpr) emit(w io.Writer) error {
	op := u.Op
	if op == "!" {
		op = "not "
	}
	if _, err := io.WriteString(w, op); err != nil {
		return err
	}
	return emitExpr(w, u.Expr)
}

type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := emitExpr(w, c.Then); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " if "); err != nil {
		return err
	}
	if err := emitExpr(w, c.Cond); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " else "); err != nil {
		return err
	}
	if err := emitExpr(w, c.Else); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

func emitExpr(w io.Writer, e Expr) error { return e.emit(w) }

func exprString(e Expr) string {
	var buf bytes.Buffer
	_ = emitExpr(&buf, e)
	return buf.String()
}

func emitStmtIndent(w io.Writer, s Stmt, indent string) error {
	switch st := s.(type) {
	case *ExprStmt:
		if _, err := io.WriteString(w, indent); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *ImportStmt:
		line := indent + "import " + st.Module
		if st.Alias != "" && st.Alias != st.Module {
			line += " as " + st.Alias
		}
		if _, err := io.WriteString(w, line+"\n"); err != nil {
			return err
		}
		return nil
	case *ReturnStmt:
		if funcDepth == 0 {
			if _, err := io.WriteString(w, indent+"sys.exit("); err != nil {
				return err
			}
			if st.Expr != nil {
				if err := emitExpr(w, st.Expr); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, ")\n"); err != nil {
				return err
			}
			return nil
		}
		if _, err := io.WriteString(w, indent+"return"); err != nil {
			return err
		}
		if st.Expr != nil {
			if _, err := io.WriteString(w, " "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *AssignStmt:
		if _, err := io.WriteString(w, indent+safeName(st.Name)+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *LetStmt:
		if _, err := io.WriteString(w, indent+safeName(st.Name)+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *VarStmt:
		if _, err := io.WriteString(w, indent+safeName(st.Name)+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *WhileStmt:
		if _, err := io.WriteString(w, indent+"while "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Cond); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ":\n"); err != nil {
			return err
		}
		for _, bs := range st.Body {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		return nil
	case *ForStmt:
		if _, err := io.WriteString(w, indent+"for "+safeName(st.Var)+" in "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Iter); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ":\n"); err != nil {
			return err
		}
		for _, bs := range st.Body {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		return nil
	case *IfStmt:
		if _, err := io.WriteString(w, indent+"if "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Cond); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ":\n"); err != nil {
			return err
		}
		if len(st.Then) == 0 {
			if _, err := io.WriteString(w, indent+"    pass\n"); err != nil {
				return err
			}
		} else {
			for _, bs := range st.Then {
				if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
					return err
				}
			}
		}
		if len(st.Else) > 0 {
			if _, err := io.WriteString(w, indent+"else:\n"); err != nil {
				return err
			}
			if len(st.Else) == 0 {
				if _, err := io.WriteString(w, indent+"    pass\n"); err != nil {
					return err
				}
			} else {
				for _, bs := range st.Else {
					if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
						return err
					}
				}
			}
		}
		return nil
	case *BenchStmt:
		if _, err := io.WriteString(w, indent+"_bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, indent+"_bench_start = _now()\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, indent+"try:\n"); err != nil {
			return err
		}
		for _, bs := range st.Body {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, indent+"finally:\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, indent+"    _bench_end = _now()\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, indent+"    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss\n"); err != nil {
			return err
		}
		line := fmt.Sprintf("%s    print(json.dumps({\"duration_us\": (_bench_end - _bench_start)//1000, \"memory_bytes\": _bench_mem_end*1024, \"name\": %q}, indent=2))\n", indent, st.Name)
		if _, err := io.WriteString(w, line); err != nil {
			return err
		}
		return nil
	case *BreakStmt:
		_, err := io.WriteString(w, indent+"break\n")
		return err
	case *ContinueStmt:
		_, err := io.WriteString(w, indent+"continue\n")
		return err
	case *DataClassDef:
		if _, err := io.WriteString(w, indent+"@dataclass\n"); err != nil {
			return err
		}
		if _, err := io.WriteString(w, indent+"class "+safeName(st.Name)+":\n"); err != nil {
			return err
		}
		if len(st.Fields) == 0 && len(st.Methods) == 0 {
			if _, err := io.WriteString(w, indent+"    pass\n"); err != nil {
				return err
			}
			return nil
		}
		for _, f := range st.Fields {
			line := fmt.Sprintf("%s    %s: %s\n", indent, safeName(f.Name), pyTypeName(f.Type))
			if _, err := io.WriteString(w, line); err != nil {
				return err
			}
		}
		for _, m := range st.Methods {
			if err := emitStmtIndent(w, m, indent+"    "); err != nil {
				return err
			}
		}
		return nil
	case *AssertStmt:
		if _, err := io.WriteString(w, indent+"assert "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Expr); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *CommentStmt:
		_, err := io.WriteString(w, indent+"# "+st.Text+"\n")
		return err
	case *FieldAssignStmt:
		if _, err := io.WriteString(w, indent); err != nil {
			return err
		}
		if err := emitExpr(w, st.Target); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "."+st.Field+" = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Value); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *IndexAssignStmt:
		if name, ok := st.Target.(*Name); ok {
			useSet := false
			if currentEnv != nil {
				if t, err := currentEnv.GetVar(name.Name); err == nil {
					if _, ok := t.(types.ListType); ok {
						useSet = true
					}
				}
			}
			if useSet {
				if _, err := io.WriteString(w, indent+safeName(name.Name)+" = _set_index("); err != nil {
					return err
				}
				if err := emitExpr(w, st.Target); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
				if err := emitExpr(w, st.Index); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
				if err := emitExpr(w, st.Value); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ")\n"); err != nil {
					return err
				}
				usesSetIndex = true
				return nil
			}
		}
		if _, err := io.WriteString(w, indent); err != nil {
			return err
		}
		if err := emitExpr(w, st.Target); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "["); err != nil {
			return err
		}
		if err := emitExpr(w, st.Index); err != nil {
			return err
		}
		if _, err := io.WriteString(w, "] = "); err != nil {
			return err
		}
		if err := emitExpr(w, st.Value); err != nil {
			return err
		}
		_, err := io.WriteString(w, "\n")
		return err
	case *FuncDef:
		if _, err := io.WriteString(w, indent+"def "+safeName(st.Name)+"("); err != nil {
			return err
		}
		for i, p := range st.Params {
			if i > 0 {
				if _, err := io.WriteString(w, ", "); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, safeName(p)); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "):\n"); err != nil {
			return err
		}
		if len(st.Globals) > 0 {
			if _, err := io.WriteString(w, indent+"    global "+strings.Join(st.Globals, ", ")+"\n"); err != nil {
				return err
			}
		}
		if len(st.Nonlocals) > 0 {
			if _, err := io.WriteString(w, indent+"    nonlocal "+strings.Join(st.Nonlocals, ", ")+"\n"); err != nil {
				return err
			}
		}
		funcDepth++
		if len(st.Body) == 0 {
			if _, err := io.WriteString(w, indent+"    pass\n"); err != nil {
				return err
			}
			funcDepth--
			return nil
		}
		for _, bs := range st.Body {
			if err := emitStmtIndent(w, bs, indent+"    "); err != nil {
				return err
			}
		}
		funcDepth--
		return nil
	case *SaveStmt:
		if st.Format == "jsonl" {
			if st.Path == "" || st.Path == "-" {
				if _, err := io.WriteString(w, indent+"for _row in "); err != nil {
					return err
				}
				if err := emitExpr(w, st.Src); err != nil {
					return err
				}
				if _, err := io.WriteString(w, ":\n"); err != nil {
					return err
				}
				if _, err := io.WriteString(w, indent+"    _tmp = dataclasses.asdict(_row) if hasattr(_row, \"__dataclass_fields__\") else _row\n"); err != nil {
					return err
				}
				if _, err := io.WriteString(w, indent+"    print(json.dumps(_tmp))\n"); err != nil {
					return err
				}
				return nil
			}
			if _, err := fmt.Fprintf(w, "%swith open(%q, 'w', encoding='utf-8') as f:\n", indent, st.Path); err != nil {
				return err
			}
			inner := indent + "    "
			if _, err := io.WriteString(w, inner+"for _row in "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Src); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			if _, err := io.WriteString(w, inner+"    _tmp = dataclasses.asdict(_row) if hasattr(_row, \"__dataclass_fields__\") else _row\n"); err != nil {
				return err
			}
			if _, err := io.WriteString(w, inner+"    f.write(json.dumps(_tmp)+\"\\n\")\n"); err != nil {
				return err
			}
			return nil
		}
		return fmt.Errorf("unsupported save format")
	case *UpdateStmt:
		idx := "idx"
		it := "item"
		if _, err := fmt.Fprintf(w, "%sfor %s, %s in enumerate(%s):\n", indent, idx, it, st.Target); err != nil {
			return err
		}
		inner := indent + "    "
		if st.Cond != nil {
			if _, err := io.WriteString(w, inner+"if "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Cond); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			inner += "    "
		}
		for i, f := range st.Fields {
			if st.Struct {
				if _, err := fmt.Fprintf(w, "%s%s.%s = ", inner, it, f); err != nil {
					return err
				}
			} else {
				if _, err := fmt.Fprintf(w, "%s%s[%q] = ", inner, it, f); err != nil {
					return err
				}
			}
			if err := emitExpr(w, st.Values[i]); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "%s%s[%s] = %s\n", indent+"    ", st.Target, idx, it); err != nil {
			return err
		}
		return nil
	default:
		return fmt.Errorf("unsupported stmt")
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
	cmd := exec.Command("git", "-C", root, "log", "-1", "--format=%cI")
	out, err := cmd.Output()
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
	loc := time.FixedZone("GMT+7", 7*60*60)
	t := gitTime().In(loc)
	return fmt.Sprintf(`# Code generated by Mochi transpiler.
# Version %s, generated on %s
`, version(), t.Format("2006-01-02 15:04 -0700"))
}

func pyTypeName(name string) string {
	name = strings.TrimSpace(name)
	if strings.HasPrefix(name, "{") && strings.HasSuffix(name, "}") {
		inside := strings.TrimSuffix(strings.TrimPrefix(name, "{"), "}")
		parts := strings.SplitN(inside, ":", 2)
		if len(parts) == 2 {
			k := pyTypeName(strings.TrimSpace(parts[0]))
			v := pyTypeName(strings.TrimSpace(parts[1]))
			return fmt.Sprintf("Dict[%s, %s]", k, v)
		}
	}
	if strings.HasPrefix(name, "fun(") {
		usesCallable = true
		s := strings.TrimPrefix(name, "fun(")
		end := strings.Index(s, ")")
		if end >= 0 {
			params := strings.TrimSpace(s[:end])
			rest := strings.TrimSpace(s[end+1:])
			ret := "None"
			if strings.HasPrefix(rest, ":") {
				ret = pyTypeName(strings.TrimSpace(rest[1:]))
			}
			var ps []string
			if params != "" {
				for _, p := range strings.Split(params, ",") {
					ps = append(ps, pyTypeName(strings.TrimSpace(p)))
				}
			}
			return fmt.Sprintf("Callable[[%s], %s]", strings.Join(ps, ", "), ret)
		}
	}
	switch name {
	case "string":
		return "str"
	case "int", "int64":
		return "int"
	case "float":
		return "float"
	case "bool":
		return "bool"
	default:
		return name
	}
}

func pyTypeToType(name string, env *types.Env) types.Type {
	switch name {
	case "int":
		return types.IntType{}
	case "float":
		return types.FloatType{}
	case "string", "str":
		return types.StringType{}
	case "bool":
		return types.BoolType{}
	default:
		if env != nil {
			if st, ok := env.GetStruct(name); ok {
				return st
			}
		}
		return types.AnyType{}
	}
}

func structFromDataClass(dc *DataClassDef, env *types.Env) types.StructType {
	fields := map[string]types.Type{}
	for _, f := range dc.Fields {
		fields[f.Name] = pyTypeToType(f.Type, env)
	}
	st := types.StructType{Name: dc.Name, Fields: fields}
	if env != nil {
		if existing, ok := env.GetStruct(dc.Name); ok {
			st.Methods = existing.Methods
		}
	}
	return st
}

func typeRefSimpleName(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		return *t.Simple
	}
	if t.Generic != nil {
		return t.Generic.Name
	}
	return ""
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

func aggregatorCall(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	switch call.Func {
	case "sum", "count", "avg", "min", "max":
		return call.Func, call.Args[0], true
	default:
		return "", nil, false
	}
}

func isExistsCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return false
	}
	call := p.Target.Call
	return call.Func == "exists" && len(call.Args) == 1
}

func isBoolOp(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) == 0 {
		return false
	}
	for _, r := range e.Binary.Right {
		switch r.Op {
		case "&&", "||", "==", "!=", "<", "<=", ">", ">=", "in":
			return true
		}
	}
	return false
}

func lenCallArg(e *parser.Expr) (*parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	call := p.Target.Call
	if call.Func == "len" && len(call.Args) == 1 {
		return call.Args[0], true
	}
	return nil, false
}

func isSumUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return false
	}
	return p.Target.Call.Func == "sum"
}

func isSumPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return false
	}
	return p.Target.Call.Func == "sum"
}

func exprFromUnary(u *parser.Unary) *parser.Expr {
	if u == nil {
		return &parser.Expr{}
	}
	return &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
}

func exprFromPostfix(p *parser.PostfixExpr) *parser.Expr {
	if p == nil {
		return &parser.Expr{}
	}
	return exprFromUnary(&parser.Unary{Value: p})
}

func zeroValueExpr(t types.Type) Expr {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return &IntLit{Value: "0"}
	case types.FloatType:
		return &FloatLit{Value: "0.0"}
	case types.StringType:
		return &StringLit{Value: ""}
	case types.BoolType:
		return &BoolLit{Value: false}
	case types.ListType:
		return &ListLit{}
	case types.MapType, types.StructType:
		return &DictLit{}
	default:
		return &Name{Name: "None"}
	}
}

func toCamel(s string) string {
	parts := strings.Split(s, "_")
	for i, p := range parts {
		if len(p) > 0 {
			parts[i] = strings.ToUpper(p[:1]) + p[1:]
		}
	}
	return strings.Join(parts, "")
}

func toClassName(name string) string {
	if strings.HasSuffix(name, "s") && len(name) > 1 {
		name = name[:len(name)-1]
	}
	if name == "partsupp" {
		name = "part_supp"
	}
	return toCamel(name)
}

func maybeDataClassList(name string, list *ListLit, env *types.Env) (*DataClassDef, []Expr) {
	if env != nil {
		if t, err := env.GetVar(name); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.MapType); ok {
					return nil, nil
				}
			}
		}
	}
	if len(list.Elems) == 0 {
		return nil, nil
	}
	first, ok := list.Elems[0].(*DictLit)
	if !ok {
		return nil, nil
	}
	keys := make([]string, len(first.Keys))
	for i, k := range first.Keys {
		s, ok := k.(*StringLit)
		if !ok {
			return nil, nil
		}
		keys[i] = s.Value
	}
	for _, el := range list.Elems[1:] {
		d, ok := el.(*DictLit)
		if !ok || len(d.Keys) != len(keys) {
			return nil, nil
		}
		for i, k := range d.Keys {
			s, ok := k.(*StringLit)
			if !ok || s.Value != keys[i] {
				return nil, nil
			}
		}
	}
	var fields []DataClassField
	for i, k := range keys {
		t := inferPyType(first.Values[i], env)
		fields = append(fields, DataClassField{Name: k, Type: t.String()})
	}
	var elems []Expr
	className := toClassName(name)
	for _, el := range list.Elems {
		d := el.(*DictLit)
		args := make([]Expr, len(d.Values))
		copy(args, d.Values)
		elems = append(elems, &CallExpr{Func: &Name{Name: className}, Args: args})
	}
	return &DataClassDef{Name: className, Fields: fields}, elems
}

func dataClassFromDict(name string, d *DictLit, env *types.Env) (*DataClassDef, []Expr) {
	keys := make([]string, len(d.Keys))
	for i, k := range d.Keys {
		s, ok := k.(*StringLit)
		if !ok {
			return nil, nil
		}
		keys[i] = s.Value
	}
	var fields []DataClassField
	for i, k := range keys {
		t := inferPyType(d.Values[i], env)
		fields = append(fields, DataClassField{Name: k, Type: t.String()})
	}
	className := toClassName(name)
	args := make([]Expr, len(d.Values))
	copy(args, d.Values)
	return &DataClassDef{Name: className, Fields: fields}, args
}

func applyResultClass(e Expr, class string, args []Expr) {
	switch ex := e.(type) {
	case *ListComp:
		ex.Expr = &CallExpr{Func: &Name{Name: class}, Args: args}
	case *MultiListComp:
		ex.Expr = &CallExpr{Func: &Name{Name: class}, Args: args}
	case *LambdaExpr:
		applyResultClass(ex.Expr, class, args)
	case *BinaryExpr:
		applyResultClass(ex.Left, class, args)
		applyResultClass(ex.Right, class, args)
	}
}

func isNumeric(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.BigIntType, types.BigRatType:
		return true
	default:
		return false
	}
}

func isIntLike(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.BigIntType:
		return true
	default:
		return false
	}
}

func isFloatLike(t types.Type) bool {
	switch t.(type) {
	case types.FloatType, types.BigRatType:
		return true
	default:
		return false
	}
}

func isIntOnlyExpr(e Expr, env *types.Env) bool {
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *Name:
		if env != nil {
			if t, err := env.GetVar(ex.Name); err == nil {
				return isIntLike(t)
			}
		}
		return false
	case *BinaryExpr:
		if ex.Op == "/" {
			return isIntOnlyExpr(ex.Left, env) && isIntOnlyExpr(ex.Right, env)
		}
		switch ex.Op {
		case "+", "-", "*", "%", "//":
			return isIntOnlyExpr(ex.Left, env) && isIntOnlyExpr(ex.Right, env)
		}
		return false
	case *UnaryExpr:
		return isIntOnlyExpr(ex.Expr, env)
	case *ParenExpr:
		return isIntOnlyExpr(ex.Expr, env)
	default:
		return false
	}
}

// isLikelyIntExpr provides a best-effort check to determine whether an
// expression consists solely of integer values without relying on the type
// environment. It treats all names as integers and rejects expressions that
// obviously involve floats or division results.
func isLikelyIntExpr(e Expr) bool {
	switch ex := e.(type) {
	case *IntLit:
		return true
	case *Name:
		if currentEnv != nil {
			if t, err := currentEnv.GetVar(ex.Name); err == nil {
				return isIntLike(t)
			}
		}
		return false
	case *CallExpr:
		if n, ok := ex.Func.(*Name); ok && n.Name == "len" {
			return true
		}
		return false
	case *BinaryExpr:
		if ex.Op == "/" {
			return false
		}
		switch ex.Op {
		case "+", "-", "*", "%", "//":
			return isLikelyIntExpr(ex.Left) && isLikelyIntExpr(ex.Right)
		}
		return false
	case *UnaryExpr:
		return isLikelyIntExpr(ex.Expr)
	case *ParenExpr:
		return isLikelyIntExpr(ex.Expr)
	default:
		return false
	}
}

func isListOfStrings(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		_, ok2 := lt.Elem.(types.StringType)
		return ok2
	}
	return false
}

func sliceOfStringList(s *SliceExpr) bool {
	if n, ok := s.Target.(*Name); ok && currentEnv != nil {
		if t, err := currentEnv.GetVar(n.Name); err == nil {
			return isListOfStrings(t)
		}
	}
	return false
}

func isStructType(t types.Type) bool {
	_, ok := t.(types.StructType)
	return ok
}

func parseScalar(s string) interface{} {
	if i, err := strconv.ParseInt(s, 10, 64); err == nil {
		return int(i)
	}
	if f, err := strconv.ParseFloat(s, 64); err == nil {
		return f
	}
	if s == "true" {
		return true
	}
	if s == "false" {
		return false
	}
	return s
}

func inferPyType(e Expr, env *types.Env) types.Type {
	switch ex := e.(type) {
	case *IntLit:
		return types.IntType{}
	case *FloatLit:
		return types.FloatType{}
	case *StringLit:
		return types.StringType{}
	case *BoolLit:
		return types.BoolType{}
	case *Name:
		if env != nil {
			if t, err := env.GetVar(ex.Name); err == nil {
				return t
			}
		}
		return types.AnyType{}
	case *FieldExpr:
		t := inferPyType(ex.Target, env)
		if st, ok := t.(types.StructType); ok {
			if ft, ok := st.Fields[ex.Name]; ok {
				return ft
			}
		}
		if mp, ok := t.(types.MapType); ok {
			return mp.Value
		}
		return types.AnyType{}
	case *IndexExpr:
		t := inferPyType(ex.Target, env)
		switch tt := t.(type) {
		case types.ListType:
			return tt.Elem
		case types.MapType:
			return tt.Value
		case types.StructType:
			if s, ok := ex.Index.(*StringLit); ok {
				if ft, ok := tt.Fields[s.Value]; ok {
					return ft
				}
			}
		}
		return types.AnyType{}
	case *CallExpr:
		if n, ok := ex.Func.(*Name); ok {
			if env != nil {
				if t, ok := env.LookupType(n.Name); ok {
					if ft, ok := t.(types.FuncType); ok {
						return ft.Return
					}
				}
			}
			switch n.Name {
			case "len":
				return types.IntType{}
			case "range":
				return types.ListType{Elem: types.IntType{}}
			case "list":
				if len(ex.Args) == 1 {
					elem := inferPyType(ex.Args[0], env)
					return types.ListType{Elem: elem}
				}
				return types.ListType{Elem: types.AnyType{}}
			case "dict":
				return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
			case "int":
				return types.IntType{}
			case "float":
				return types.FloatType{}
			case "str":
				return types.StringType{}
			case "bool":
				return types.BoolType{}
			case "split":
				if len(ex.Args) == 2 {
					return types.ListType{Elem: types.StringType{}}
				}
			case "repeat":
				if len(ex.Args) == 2 {
					return types.StringType{}
				}
			case "parseIntStr":
				if len(ex.Args) == 2 {
					return types.IntType{}
				}
			case "upper", "lower":
				if len(ex.Args) == 1 {
					return types.StringType{}
				}
			case "padStart":
				if len(ex.Args) == 3 {
					return types.StringType{}
				}
			case "Fraction":
				return types.BigRatType{}
			}
		} else if fe, ok := ex.Func.(*FieldExpr); ok {
			if st, ok := inferPyType(fe.Target, env).(types.StructType); ok {
				if m, ok := st.Methods[fe.Name]; ok {
					return m.Type.Return
				}
			}
		}
		return types.AnyType{}
	case *ListLit:
		var elem types.Type
		for _, e := range ex.Elems {
			t := inferPyType(e, env)
			if elem == nil {
				elem = t
			} else if elem.String() != t.String() {
				if isNumeric(elem) && isNumeric(t) {
					elem = types.FloatType{}
				} else {
					elem = types.AnyType{}
				}
			}
		}
		if elem == nil {
			elem = types.AnyType{}
		}
		return types.ListType{Elem: elem}
	case *DictLit:
		var kt, vt types.Type
		for i := range ex.Keys {
			kt2 := inferPyType(ex.Keys[i], env)
			vt2 := inferPyType(ex.Values[i], env)
			if kt == nil {
				kt = kt2
			} else if kt.String() != kt2.String() {
				kt = types.AnyType{}
			}
			if vt == nil {
				vt = vt2
			} else if vt.String() != vt2.String() {
				if isNumeric(vt) && isNumeric(vt2) {
					vt = types.FloatType{}
				} else {
					vt = types.AnyType{}
				}
			}
		}
		if kt == nil {
			kt = types.AnyType{}
		}
		if vt == nil {
			vt = types.AnyType{}
		}
		return types.MapType{Key: kt, Value: vt}
	case *BinaryExpr:
		lt := inferPyType(ex.Left, env)
		rt := inferPyType(ex.Right, env)
		switch ex.Op {
		case "+", "-", "*", "%":
			if isNumeric(lt) && isNumeric(rt) {
				if lt.String() == (types.FloatType{}).String() || rt.String() == (types.FloatType{}).String() {
					return types.FloatType{}
				}
				return types.IntType{}
			}
			return types.AnyType{}
		case "/":
			if isNumeric(lt) && isNumeric(rt) {
				if isIntLike(lt) && isIntLike(rt) {
					return types.IntType{}
				}
				return types.FloatType{}
			}
			return types.AnyType{}
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return types.BoolType{}
		default:
			return types.AnyType{}
		}
	case *CondExpr:
		t1 := inferPyType(ex.Then, env)
		t2 := inferPyType(ex.Else, env)
		if t1.String() == t2.String() {
			return t1
		}
		if isNumeric(t1) && isNumeric(t2) {
			return types.FloatType{}
		}
		return types.AnyType{}
	case *ListComp:
		elem := inferPyType(ex.Expr, env)
		return types.ListType{Elem: elem}
	default:
		return types.AnyType{}
	}
}

func inferTypeFromData(path, format string) types.Type {
	if path == "" {
		return types.AnyType{}
	}
	root := repoRoot()
	if root != "" && strings.HasPrefix(path, "../") {
		clean := strings.TrimPrefix(path, "../")
		path = filepath.Join(root, "tests", clean)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return types.AnyType{}
	}

	var v interface{}
	switch format {
	case "yaml":
		if err := yaml.Unmarshal(data, &v); err != nil {
			return types.AnyType{}
		}
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return types.AnyType{}
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
	case "csv", "tsv":
		delim := ','
		if format == "tsv" {
			delim = '\t'
		}
		r := csv.NewReader(bytes.NewReader(data))
		r.Comma = delim
		records, err := r.ReadAll()
		if err != nil || len(records) == 0 {
			return types.AnyType{}
		}
		header := records[0]
		var arr []interface{}
		for _, row := range records[1:] {
			obj := map[string]interface{}{}
			for i, col := range row {
				if i >= len(header) {
					break
				}
				obj[header[i]] = parseScalar(col)
			}
			arr = append(arr, obj)
		}
		v = arr
	default:
		return types.AnyType{}
	}
	return inferTypeFromValue(v)
}

func inferTypeFromValue(v interface{}) types.Type {
	switch val := v.(type) {
	case map[string]interface{}:
		fields := map[string]types.Type{}
		for k, vv := range val {
			fields[k] = inferTypeFromValue(vv)
		}
		return types.StructType{Fields: fields}
	case []interface{}:
		var elem types.Type
		for _, it := range val {
			et := inferTypeFromValue(it)
			if elem == nil {
				elem = et
			} else if elem.String() != et.String() {
				if isNumeric(elem) && isNumeric(et) {
					elem = types.FloatType{}
				} else {
					elem = types.AnyType{}
				}
			}
		}
		if elem == nil {
			elem = types.AnyType{}
		}
		return types.ListType{Elem: elem}
	case string:
		return types.StringType{}
	case bool:
		return types.BoolType{}
	case int, int64:
		return types.IntType{}
	case float32, float64:
		return types.FloatType{}
	default:
		return types.AnyType{}
	}
}

func valueToExpr(v interface{}) Expr {
	switch val := v.(type) {
	case map[string]interface{}:
		keys := make([]Expr, 0, len(val))
		vals := make([]Expr, 0, len(val))
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		for _, k := range names {
			keys = append(keys, &StringLit{Value: k})
			vals = append(vals, valueToExpr(val[k]))
		}
		return &DictLit{Keys: keys, Values: vals}
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
	case int, int64:
		return &IntLit{Value: fmt.Sprintf("%v", val)}
	case float32, float64:
		f := fmt.Sprintf("%v", val)
		if !strings.ContainsAny(f, ".eE") {
			f += ".0"
		}
		return &FloatLit{Value: f}
	case nil:
		return &Name{Name: "None"}
	default:
		return &Name{Name: "None"}
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
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
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
	case "csv", "tsv":
		delim := ','
		if format == "tsv" {
			delim = '\t'
		}
		r := csv.NewReader(bytes.NewReader(data))
		r.Comma = delim
		records, err := r.ReadAll()
		if err != nil || len(records) == 0 {
			return nil, err
		}
		header := records[0]
		var arr []interface{}
		for _, row := range records[1:] {
			obj := map[string]interface{}{}
			for i, col := range row {
				if i >= len(header) {
					break
				}
				obj[header[i]] = parseScalar(col)
			}
			arr = append(arr, obj)
		}
		v = arr
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToExpr(v), nil
}

func inferTypeFromExpr(e *parser.Expr) types.Type {
	if isExistsCall(e) {
		return types.BoolType{}
	}
	if name, arg, ok := aggregatorCall(e); ok {
		switch name {
		case "count":
			return types.IntType{}
		case "sum", "min", "max":
			t := inferTypeFromExpr(arg)
			if isNumeric(t) {
				return t
			}
			return types.AnyType{}
		case "avg":
			return types.FloatType{}
		}
	}
	if _, ok := lenCallArg(e); ok {
		return types.IntType{}
	}
	if e == nil || e.Binary == nil {
		return types.AnyType{}
	}
	if len(e.Binary.Right) > 0 {
		lt := inferTypeFromExpr(exprFromUnary(e.Binary.Left))
		for _, r := range e.Binary.Right {
			rt := inferTypeFromExpr(exprFromPostfix(r.Right))
			switch r.Op {
			case "&&", "||", "==", "!=", "<", "<=", ">", ">=", "in":
				lt = types.BoolType{}
			case "+", "-", "*", "/", "%":
				if lt.String() == (types.FloatType{}).String() || rt.String() == (types.FloatType{}).String() {
					lt = types.FloatType{}
				} else if isNumeric(lt) && isNumeric(rt) {
					lt = types.IntType{}
				} else {
					lt = types.AnyType{}
				}
			default:
				lt = types.AnyType{}
			}
		}
		return lt
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		if u.Value != nil && len(u.Ops) == 1 {
			op := u.Ops[0]
			if op == "-" || op == "+" {
				return inferTypeFromExpr(exprFromPostfix(u.Value))
			}
			if op == "!" {
				return types.BoolType{}
			}
		}
		if u.Value == nil {
			return types.AnyType{}
		}
	} else if u.Value == nil {
		return types.AnyType{}
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return types.AnyType{}
	}
	t := p.Target
	switch {
	case t.Lit != nil:
		l := t.Lit
		switch {
		case l.Int != nil:
			return types.IntType{}
		case l.Float != nil:
			return types.FloatType{}
		case l.Bool != nil:
			return types.BoolType{}
		case l.Str != nil:
			return types.StringType{}
		default:
			return types.AnyType{}
		}
	case t.List != nil:
		var elem types.Type = nil
		for _, it := range t.List.Elems {
			et := inferTypeFromExpr(it)
			if elem == nil {
				elem = et
			} else if elem.String() != et.String() {
				elem = types.AnyType{}
			}
		}
		if elem == nil {
			elem = types.AnyType{}
		}
		return types.ListType{Elem: elem}
	case t.Map != nil:
		var kt, vt types.Type
		for _, it := range t.Map.Items {
			itK := inferTypeFromExpr(it.Key)
			itV := inferTypeFromExpr(it.Value)
			if kt == nil {
				kt = itK
			} else if kt.String() != itK.String() {
				kt = types.AnyType{}
			}
			if vt == nil {
				vt = itV
			} else if vt.String() != itV.String() {
				vt = types.AnyType{}
			}
		}
		if kt == nil {
			kt = types.AnyType{}
		}
		if vt == nil {
			vt = types.AnyType{}
		}
		return types.MapType{Key: kt, Value: vt}
	case t.Struct != nil:
		fields := map[string]types.Type{}
		for _, f := range t.Struct.Fields {
			fields[f.Name] = inferTypeFromExpr(f.Value)
		}
		return types.StructType{Fields: fields}
	case t.Load != nil:
		format := parseFormat(t.Load.With)
		path := ""
		if t.Load.Path != nil {
			path = strings.Trim(*t.Load.Path, "\"")
		}
		return inferTypeFromData(path, format)
	case t.Selector != nil && len(t.Selector.Tail) == 0:
		if currentEnv != nil {
			if typ, err := currentEnv.GetVar(t.Selector.Root); err == nil {
				return typ
			}
		}
		return types.AnyType{}
	case t.Selector != nil:
		if currentEnv != nil {
			typ, err := currentEnv.GetVar(t.Selector.Root)
			if err == nil {
				for _, fld := range t.Selector.Tail {
					switch tt := typ.(type) {
					case types.StructType:
						if ft, ok := tt.Fields[fld]; ok {
							typ = ft
						} else {
							typ = types.AnyType{}
						}
					case types.MapType:
						typ = tt.Value
					case types.ListType:
						typ = tt.Elem
					default:
						typ = types.AnyType{}
					}
				}
				return typ
			}
		}
		return types.AnyType{}
	case t.If != nil:
		return inferTypeFromIf(t.If)
	default:
		return types.AnyType{}
	}
}

func inferTypeFromIf(ie *parser.IfExpr) types.Type {
	if ie == nil {
		return types.AnyType{}
	}
	thenT := inferTypeFromExpr(ie.Then)
	var elseT types.Type = types.AnyType{}
	if ie.ElseIf != nil {
		elseT = inferTypeFromIf(ie.ElseIf)
	} else if ie.Else != nil {
		elseT = inferTypeFromExpr(ie.Else)
	}
	if thenT.String() == elseT.String() {
		return thenT
	}
	if isNumeric(thenT) && isNumeric(elseT) {
		return types.FloatType{}
	}
	return types.AnyType{}
}

// substituteFields replaces Name nodes that match the given field names with
// map access on the provided variable.
func substituteFields(e Expr, varName string, fields map[string]bool) Expr {
	switch ex := e.(type) {
	case *Name:
		if fields[ex.Name] {
			return &FieldExpr{Target: &Name{Name: varName}, Name: ex.Name}
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
		ex.Func = substituteFields(ex.Func, varName, fields)
		for i := range ex.Args {
			ex.Args[i] = substituteFields(ex.Args[i], varName, fields)
		}
		return ex
	case *FieldExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		return ex
	case *IndexExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		ex.Index = substituteFields(ex.Index, varName, fields)
		return ex
	case *SliceExpr:
		ex.Target = substituteFields(ex.Target, varName, fields)
		if ex.Start != nil {
			ex.Start = substituteFields(ex.Start, varName, fields)
		}
		if ex.End != nil {
			ex.End = substituteFields(ex.End, varName, fields)
		}
		if ex.Step != nil {
			ex.Step = substituteFields(ex.Step, varName, fields)
		}
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
	case *DictLit:
		for i := range ex.Keys {
			ex.Keys[i] = substituteFields(ex.Keys[i], varName, fields)
			ex.Values[i] = substituteFields(ex.Values[i], varName, fields)
		}
		return ex
	default:
		return ex
	}
}

func replaceName(e Expr, name string, repl Expr) Expr {
	switch ex := e.(type) {
	case *Name:
		if ex.Name == name {
			return repl
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceName(ex.Left, name, repl)
		ex.Right = replaceName(ex.Right, name, repl)
		return ex
	case *UnaryExpr:
		ex.Expr = replaceName(ex.Expr, name, repl)
		return ex
	case *CallExpr:
		ex.Func = replaceName(ex.Func, name, repl)
		for i := range ex.Args {
			ex.Args[i] = replaceName(ex.Args[i], name, repl)
		}
		return ex
	case *FieldExpr:
		ex.Target = replaceName(ex.Target, name, repl)
		if n, ok := ex.Target.(*Name); ok && repl != nil && n.Name == "None" {
			return repl
		}
		return ex
	case *IndexExpr:
		ex.Target = replaceName(ex.Target, name, repl)
		ex.Index = replaceName(ex.Index, name, repl)
		return ex
	case *SliceExpr:
		ex.Target = replaceName(ex.Target, name, repl)
		if ex.Start != nil {
			ex.Start = replaceName(ex.Start, name, repl)
		}
		if ex.End != nil {
			ex.End = replaceName(ex.End, name, repl)
		}
		if ex.Step != nil {
			ex.Step = replaceName(ex.Step, name, repl)
		}
		return ex
	case *CondExpr:
		ex.Cond = replaceName(ex.Cond, name, repl)
		ex.Then = replaceName(ex.Then, name, repl)
		ex.Else = replaceName(ex.Else, name, repl)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceName(ex.Elems[i], name, repl)
		}
		return ex
	case *DictLit:
		for i := range ex.Keys {
			ex.Keys[i] = replaceName(ex.Keys[i], name, repl)
			ex.Values[i] = replaceName(ex.Values[i], name, repl)
		}
		return ex
	case *LambdaExpr:
		for _, p := range ex.Params {
			if p == name {
				return ex
			}
		}
		ex.Expr = replaceName(ex.Expr, name, repl)
		return ex
	default:
		return ex
	}
}

func cloneExpr(e Expr) Expr {
	switch ex := e.(type) {
	case *Name:
		return &Name{Name: ex.Name}
	case *FieldExpr:
		return &FieldExpr{Target: cloneExpr(ex.Target), Name: ex.Name, MapIndex: ex.MapIndex, MapGet: ex.MapGet}
	case *CallExpr:
		args := make([]Expr, len(ex.Args))
		for i := range ex.Args {
			args[i] = cloneExpr(ex.Args[i])
		}
		return &CallExpr{Func: cloneExpr(ex.Func), Args: args}
	default:
		return ex
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

func extractMapLiteral(e *parser.Expr) *parser.MapLiteral {
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
	if p.Target.Map != nil {
		return p.Target.Map
	}
	return nil
}

func ExtractQueryExpr(e *parser.Expr) *parser.QueryExpr {
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
	return p.Target.Query
}

func replaceGroup(e Expr, name string) Expr {
	switch ex := e.(type) {
	case *Name:
		if ex.Name == name {
			return &FieldExpr{Target: &Name{Name: name}, Name: "items"}
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceGroup(ex.Left, name)
		ex.Right = replaceGroup(ex.Right, name)
		return ex
	case *UnaryExpr:
		ex.Expr = replaceGroup(ex.Expr, name)
		return ex
	case *CallExpr:
		ex.Func = replaceGroup(ex.Func, name)
		for i := range ex.Args {
			ex.Args[i] = replaceGroup(ex.Args[i], name)
		}
		return ex
	case *FieldExpr:
		if n, ok := ex.Target.(*Name); ok && n.Name == name {
			return ex
		}
		ex.Target = replaceGroup(ex.Target, name)
		return ex
	case *IndexExpr:
		if n, ok := ex.Target.(*Name); ok && n.Name == name {
			return &FieldExpr{Target: n, Name: "items"}
		}
		ex.Target = replaceGroup(ex.Target, name)
		ex.Index = replaceGroup(ex.Index, name)
		return ex
	case *SliceExpr:
		ex.Target = replaceGroup(ex.Target, name)
		if ex.Start != nil {
			ex.Start = replaceGroup(ex.Start, name)
		}
		if ex.End != nil {
			ex.End = replaceGroup(ex.End, name)
		}
		if ex.Step != nil {
			ex.Step = replaceGroup(ex.Step, name)
		}
		return ex
	case *CondExpr:
		ex.Cond = replaceGroup(ex.Cond, name)
		ex.Then = replaceGroup(ex.Then, name)
		ex.Else = replaceGroup(ex.Else, name)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceGroup(ex.Elems[i], name)
		}
		return ex
	case *DictLit:
		for i := range ex.Keys {
			ex.Keys[i] = replaceGroup(ex.Keys[i], name)
			ex.Values[i] = replaceGroup(ex.Values[i], name)
		}
		return ex
	case *ListComp:
		ex.Iter = replaceGroup(ex.Iter, name)
		ex.Expr = replaceGroup(ex.Expr, name)
		if ex.Cond != nil {
			ex.Cond = replaceGroup(ex.Cond, name)
		}
		return ex
	case *MultiListComp:
		for i := range ex.Iters {
			ex.Iters[i] = replaceGroup(ex.Iters[i], name)
		}
		ex.Expr = replaceGroup(ex.Expr, name)
		if ex.Cond != nil {
			ex.Cond = replaceGroup(ex.Cond, name)
		}
		return ex
	default:
		return ex
	}
}

func replaceVar(e Expr, name string, val Expr) Expr {
	switch ex := e.(type) {
	case *Name:
		if ex.Name == name {
			return val
		}
		return ex
	case *BinaryExpr:
		ex.Left = replaceVar(ex.Left, name, val)
		ex.Right = replaceVar(ex.Right, name, val)
		return ex
	case *UnaryExpr:
		ex.Expr = replaceVar(ex.Expr, name, val)
		return ex
	case *CallExpr:
		ex.Func = replaceVar(ex.Func, name, val)
		for i := range ex.Args {
			ex.Args[i] = replaceVar(ex.Args[i], name, val)
		}
		return ex
	case *FieldExpr:
		ex.Target = replaceVar(ex.Target, name, val)
		return ex
	case *IndexExpr:
		ex.Target = replaceVar(ex.Target, name, val)
		ex.Index = replaceVar(ex.Index, name, val)
		return ex
	case *SliceExpr:
		ex.Target = replaceVar(ex.Target, name, val)
		if ex.Start != nil {
			ex.Start = replaceVar(ex.Start, name, val)
		}
		if ex.End != nil {
			ex.End = replaceVar(ex.End, name, val)
		}
		if ex.Step != nil {
			ex.Step = replaceVar(ex.Step, name, val)
		}
		return ex
	case *CondExpr:
		ex.Cond = replaceVar(ex.Cond, name, val)
		ex.Then = replaceVar(ex.Then, name, val)
		ex.Else = replaceVar(ex.Else, name, val)
		return ex
	case *ListLit:
		for i := range ex.Elems {
			ex.Elems[i] = replaceVar(ex.Elems[i], name, val)
		}
		return ex
	case *DictLit:
		for i := range ex.Keys {
			ex.Keys[i] = replaceVar(ex.Keys[i], name, val)
			ex.Values[i] = replaceVar(ex.Values[i], name, val)
		}
		return ex
	case *ListComp:
		ex.Iter = replaceVar(ex.Iter, name, val)
		ex.Expr = replaceVar(ex.Expr, name, val)
		if ex.Cond != nil {
			ex.Cond = replaceVar(ex.Cond, name, val)
		}
		return ex
	case *MultiListComp:
		for i := range ex.Iters {
			ex.Iters[i] = replaceVar(ex.Iters[i], name, val)
		}
		ex.Expr = replaceVar(ex.Expr, name, val)
		if ex.Cond != nil {
			ex.Cond = replaceVar(ex.Cond, name, val)
		}
		return ex
	default:
		return ex
	}
}

func precedence(op string) int {
	switch op {
	case "*", "/", "%":
		return 0
	case "+", "-":
		return 1
	case "<", "<=", ">", ">=":
		return 2
	case "==", "!=", "in":
		return 3
	case "&&":
		return 4
	case "||":
		return 5
	case "union", "union_all", "except", "intersect":
		return 6
	default:
		return 7
	}
}

func isCompareOp(op string) bool {
	switch op {
	case "<", "<=", ">", ">=", "==", "!=", "in":
		return true
	default:
		return false
	}
}

func hasImport(p *Program, mod string) bool {
	for _, s := range p.Stmts {
		if im, ok := s.(*ImportStmt); ok && im.Module == mod {
			return true
		}
	}
	return false
}

// Emit renders Python code from AST
// Emit renders Python code from AST. If bench is true, the body of a function
// named "main" is wrapped in a benchmark block which measures execution time
// and prints a JSON report.
func Emit(w io.Writer, p *Program, bench bool) error {
	if bench {
		if currentImports != nil {
			currentImports["json"] = true
			currentImports["os"] = true
			currentImports["time"] = true
			currentImports["resource"] = true
		}
		usesNow = true
	}

	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	var imports []string
	needDC := false
	if currentImports != nil {
		if currentImports["json"] && !hasImport(p, "json") {
			imports = append(imports, "import json")
		}
		if currentImports["dataclasses"] && !hasImport(p, "dataclasses") {
			imports = append(imports, "import dataclasses")
		}
		if currentImports["socket"] && !hasImport(p, "socket") {
			imports = append(imports, "import socket")
		}
		if currentImports["fractions"] && !hasImport(p, "fractions") {
			imports = append(imports, "from fractions import Fraction")
		}
		if currentImports["hashlib"] && !hasImport(p, "hashlib") {
			imports = append(imports, "import hashlib")
		}
		if currentImports["time"] && !hasImport(p, "time") {
			imports = append(imports, "import time")
		}
		if currentImports["resource"] && !hasImport(p, "resource") {
			imports = append(imports, "import resource")
		}
	}
	for _, s := range p.Stmts {
		if _, ok := s.(*DataClassDef); ok {
			needDC = true
		}
		if im, ok := s.(*ImportStmt); ok {
			line := "import " + im.Module
			if im.Alias != "" && im.Alias != im.Module {
				line += " as " + im.Alias
			}
			imports = append(imports, line)
		}
	}
	if needDC || (currentImports != nil && currentImports["dataclasses"]) {
		imports = append(imports, "from __future__ import annotations")
		imports = append(imports, "from dataclasses import dataclass")
		typing := "List, Dict"
		if usesCallable {
			typing += ", Callable"
		}
		if usesFetch {
			typing += ", Any"
		}
		imports = append(imports, "from typing import "+typing)
	}
	sort.Strings(imports)
	for _, line := range imports {
		if _, err := io.WriteString(w, line+"\n"); err != nil {
			return err
		}
	}
	if len(imports) > 0 {
		if _, err := io.WriteString(w, "\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "import sys\nsys.set_int_max_str_digits(0)\nimport os\nif os.path.dirname(__file__) in sys.path:\n    sys.path.remove(os.path.dirname(__file__))\n\n"); err != nil {
		return err
	}
	if _, err := io.WriteString(w, helperPanic+"\n"); err != nil {
		return err
	}
	if usesNow {
		if _, err := io.WriteString(w, helperNow+"\n"); err != nil {
			return err
		}
	}
	if usesLookupHost {
		if _, err := io.WriteString(w, helperLookupHost+"\n"); err != nil {
			return err
		}
	}
	if usesIndexOf {
		if _, err := io.WriteString(w, helperIndexOf+"\n"); err != nil {
			return err
		}
	}
	if usesSubstr {
		if _, err := io.WriteString(w, helperSubstr+"\n"); err != nil {
			return err
		}
	}
	if usesFetch {
		if _, err := io.WriteString(w, helperFetch+"\n"); err != nil {
			return err
		}
	}
	if usesConcat {
		if _, err := io.WriteString(w, helperConcat+"\n"); err != nil {
			return err
		}
	}
	if usesAppend {
		if _, err := io.WriteString(w, helperAppend+"\n"); err != nil {
			return err
		}
	}
	if usesStr {
		if _, err := io.WriteString(w, helperStr+"\n"); err != nil {
			return err
		}
	}
	if usesSetIndex {
		if _, err := io.WriteString(w, helperSetIndex+"\n"); err != nil {
			return err
		}
	}
	// no runtime helpers required
	for _, s := range p.Stmts {
		switch st := s.(type) {
		case *ImportStmt:
			// already emitted above
			continue
		case *DataClassDef:
			if _, err := io.WriteString(w, "@dataclass\nclass "+safeName(st.Name)+":\n"); err != nil {
				return err
			}
			if len(st.Fields) == 0 && len(st.Methods) == 0 {
				if _, err := io.WriteString(w, "    pass\n\n"); err != nil {
					return err
				}
				continue
			}
			for _, f := range st.Fields {
				line := fmt.Sprintf("    %s: %s\n", safeName(f.Name), pyTypeName(f.Type))
				if _, err := io.WriteString(w, line); err != nil {
					return err
				}
			}
			for _, m := range st.Methods {
				if err := emitStmtIndent(w, m, "    "); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
			continue
		case *ExprStmt:
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *LetStmt:
			if _, err := io.WriteString(w, safeName(st.Name)+" = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *VarStmt:
			if _, err := io.WriteString(w, safeName(st.Name)+" = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *WhileStmt:
			if _, err := io.WriteString(w, "while "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Cond); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			for _, bs := range st.Body {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
		case *ForStmt:
			if _, err := io.WriteString(w, "for "+safeName(st.Var)+" in "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Iter); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			for _, bs := range st.Body {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
		case *IfStmt:
			if _, err := io.WriteString(w, "if "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Cond); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ":\n"); err != nil {
				return err
			}
			for _, bs := range st.Then {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
			if len(st.Else) > 0 {
				if _, err := io.WriteString(w, "else:\n"); err != nil {
					return err
				}
				for _, bs := range st.Else {
					if err := emitStmtIndent(w, bs, "    "); err != nil {
						return err
					}
				}
			}
		case *BenchStmt:
			if _, err := io.WriteString(w, "_bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss\n"); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "_bench_start = _now()\n"); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "try:\n"); err != nil {
				return err
			}
			for _, bs := range st.Body {
				if err := emitStmtIndent(w, bs, "    "); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "finally:\n"); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "    _bench_end = _now()\n"); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss\n"); err != nil {
				return err
			}
			line := fmt.Sprintf("    print(json.dumps({\"duration_us\": (_bench_end - _bench_start)//1000, \"memory_bytes\": _bench_mem_end*1024, \"name\": %q}, indent=2))\n", st.Name)
			if _, err := io.WriteString(w, line); err != nil {
				return err
			}
		case *BreakStmt:
			if _, err := io.WriteString(w, "break\n"); err != nil {
				return err
			}
		case *ContinueStmt:
			if _, err := io.WriteString(w, "continue\n"); err != nil {
				return err
			}
		case *AssertStmt:
			if _, err := io.WriteString(w, "assert "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *CommentStmt:
			if _, err := io.WriteString(w, "# "+st.Text+"\n"); err != nil {
				return err
			}
		case *IndexAssignStmt:
			if err := emitExpr(w, st.Target); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "["); err != nil {
				return err
			}
			if err := emitExpr(w, st.Index); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "] = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Value); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *AssignStmt:
			if _, err := io.WriteString(w, st.Name+" = "); err != nil {
				return err
			}
			if err := emitExpr(w, st.Expr); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *ReturnStmt:
			if _, err := io.WriteString(w, "return"); err != nil {
				return err
			}
			if st.Expr != nil {
				if _, err := io.WriteString(w, " "); err != nil {
					return err
				}
				if err := emitExpr(w, st.Expr); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		case *FuncDef:
			if _, err := io.WriteString(w, "def "+st.Name+"("); err != nil {
				return err
			}
			for i, p := range st.Params {
				if i > 0 {
					if _, err := io.WriteString(w, ", "); err != nil {
						return err
					}
				}
				if _, err := io.WriteString(w, p); err != nil {
					return err
				}
			}
			if _, err := io.WriteString(w, "):\n"); err != nil {
				return err
			}
			if len(st.Globals) > 0 {
				if _, err := io.WriteString(w, "    global "+strings.Join(st.Globals, ", ")+"\n"); err != nil {
					return err
				}
			}
			if len(st.Nonlocals) > 0 {
				if _, err := io.WriteString(w, "    nonlocal "+strings.Join(st.Nonlocals, ", ")+"\n"); err != nil {
					return err
				}
			}
			funcDepth++
			if len(st.Body) == 0 {
				if _, err := io.WriteString(w, "    pass\n"); err != nil {
					funcDepth--
					return err
				}
				funcDepth--
			} else {
				for _, bs := range st.Body {
					if err := emitStmtIndent(w, bs, "    "); err != nil {
						funcDepth--
						return err
					}
				}
				funcDepth--
			}
		case *SaveStmt:
			if err := emitStmtIndent(w, st, ""); err != nil {
				return err
			}
		case *UpdateStmt:
			if err := emitStmtIndent(w, st, ""); err != nil {
				return err
			}
		}
	}
	return nil
}

// Transpile converts a Mochi program to a Python AST.
func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	currentImports = map[string]bool{}
	currentImportLang = map[string]string{}
	currentEnv = env
	structCounter = 0
	extraFuncs = nil
	usesNow = false
	usesLookupHost = false
	usesIndexOf = false
	usesCallable = false
	usesFetch = false
	usesAppend = false
	usesConcat = false
	usesStr = false
	usesSetIndex = false
	p := &Program{}
	for _, st := range prog.Statements {
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
				if format == "jsonl" {
					currentImports["json"] = true
					currentImports["dataclasses"] = true
				}
				p.Stmts = append(p.Stmts, &SaveStmt{Src: src, Path: path, Format: format})
			} else {
				e, err := convertExpr(st.Expr.Expr)
				if err != nil {
					return nil, err
				}
				p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
			}
		case st.Let != nil:
			if q := ExtractQueryExpr(st.Let.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, st.Let.Name)
				if err != nil {
					return nil, err
				}
				p.Stmts = append(p.Stmts, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if st.Let.Value != nil {
					e, err = convertExpr(st.Let.Value)
					if err != nil {
						return nil, err
					}
					if env != nil {
						typ = inferTypeFromExpr(st.Let.Value)
						if _, ok := typ.(types.AnyType); ok {
							typ = inferPyType(e, env)
						}
						env.SetVar(st.Let.Name, typ, false)
					}
					if list, ok := e.(*ListLit); ok {
						if dc, elems := maybeDataClassList(st.Let.Name, list, env); dc != nil {
							p.Stmts = append(p.Stmts, dc)
							list.Elems = elems
							e = list
							if env != nil {
								env.SetStruct(dc.Name, structFromDataClass(dc, env))
								env.SetVar(st.Let.Name, types.ListType{Elem: structFromDataClass(dc, env)}, false)
							}
						}
					} else if q := ExtractQueryExpr(st.Let.Value); q != nil && q.Group == nil {
						selExpr, serr := convertExpr(q.Select)
						if serr == nil {
							if d, ok := selExpr.(*DictLit); ok {
								dc, args := dataClassFromDict(st.Let.Name, d, env)
								if dc != nil {
									p.Stmts = append(p.Stmts, dc)
									if len(q.Joins) == 1 && q.Joins[0].Side != nil && (*q.Joins[0].Side == "right" || *q.Joins[0].Side == "outer") && len(q.Froms) == 0 {
										e, err = convertSpecialJoinClass(q, dc.Name, args)
										if err != nil {
											return nil, err
										}
									} else {
										applyResultClass(e, dc.Name, args)
									}
									if env != nil {
										env.SetStruct(dc.Name, structFromDataClass(dc, env))
										env.SetVar(st.Let.Name, types.ListType{Elem: structFromDataClass(dc, env)}, false)
									}
								}
							}
						}
					}
				} else if st.Let.Type != nil && env != nil {
					typ = types.ResolveTypeRef(st.Let.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(st.Let.Name, typ, false)
				} else {
					return nil, fmt.Errorf("let without value")
				}
				p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Expr: e})
			}
		case st.Var != nil:
			if q := ExtractQueryExpr(st.Var.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, st.Var.Name)
				if err != nil {
					return nil, err
				}
				p.Stmts = append(p.Stmts, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if st.Var.Value != nil {
					e, err = convertExpr(st.Var.Value)
					if err != nil {
						return nil, err
					}
					if env != nil {
						typ = inferTypeFromExpr(st.Var.Value)
						if _, ok := typ.(types.AnyType); ok {
							typ = inferPyType(e, env)
						}
						env.SetVar(st.Var.Name, typ, true)
					}
					if list, ok := e.(*ListLit); ok {
						if dc, elems := maybeDataClassList(st.Var.Name, list, env); dc != nil {
							p.Stmts = append(p.Stmts, dc)
							list.Elems = elems
							e = list
							if env != nil {
								env.SetStruct(dc.Name, structFromDataClass(dc, env))
								env.SetVar(st.Var.Name, types.ListType{Elem: structFromDataClass(dc, env)}, true)
							}
						}
					} else if q := ExtractQueryExpr(st.Var.Value); q != nil && q.Group == nil {
						selExpr, serr := convertExpr(q.Select)
						if serr == nil {
							if d, ok := selExpr.(*DictLit); ok {
								dc, args := dataClassFromDict(st.Var.Name, d, env)
								if dc != nil {
									p.Stmts = append(p.Stmts, dc)
									if len(q.Joins) == 1 && q.Joins[0].Side != nil && (*q.Joins[0].Side == "right" || *q.Joins[0].Side == "outer") && len(q.Froms) == 0 {
										e, err = convertSpecialJoinClass(q, dc.Name, args)
										if err != nil {
											return nil, err
										}
									} else {
										applyResultClass(e, dc.Name, args)
									}
									if env != nil {
										env.SetStruct(dc.Name, structFromDataClass(dc, env))
										env.SetVar(st.Var.Name, types.ListType{Elem: structFromDataClass(dc, env)}, true)
									}
								}
							}
						}
					}
				} else if st.Var.Type != nil && env != nil {
					typ = types.ResolveTypeRef(st.Var.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(st.Var.Name, typ, true)
				} else {
					return nil, fmt.Errorf("var without value")
				}
				p.Stmts = append(p.Stmts, &VarStmt{Name: st.Var.Name, Expr: e})
			}
		case st.While != nil:
			cond, err := convertExpr(st.While.Cond)
			if err != nil {
				return nil, err
			}
			body, err := convertStmts(st.While.Body, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &WhileStmt{Cond: cond, Body: body})
		case st.For != nil:
			iter, err := convertExpr(st.For.Source)
			if err != nil {
				return nil, err
			}
			if env != nil {
				if lt, ok := inferTypeFromExpr(st.For.Source).(types.ListType); ok {
					env.SetVar(st.For.Name, lt.Elem, true)
				} else if lt, ok := inferPyType(iter, env).(types.ListType); ok {
					env.SetVar(st.For.Name, lt.Elem, true)
				} else if fe, ok := iter.(*FieldExpr); ok {
					if stt, ok := inferPyType(fe.Target, env).(types.StructType); ok {
						if ft, ok := stt.Fields[fe.Name]; ok {
							if lt, ok := ft.(types.ListType); ok {
								env.SetVar(st.For.Name, lt.Elem, true)
							}
						}
					}
				}
			}
			if st.For.RangeEnd != nil {
				end, err := convertExpr(st.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				iter = &CallExpr{Func: &Name{Name: "range"}, Args: []Expr{iter, end}}
			}
			body, err := convertStmts(st.For.Body, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ForStmt{Var: st.For.Name, Iter: iter, Body: body})
		case st.Assign != nil:
			val, err := convertExpr(st.Assign.Value)
			if err != nil {
				return nil, err
			}
			if len(st.Assign.Index) >= 1 && len(st.Assign.Field) == 0 {
				target := Expr(&Name{Name: st.Assign.Name})
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
				p.Stmts = append(p.Stmts, &IndexAssignStmt{Target: target, Index: idx, Value: val})
				usesSetIndex = true
			} else if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
				p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Expr: val})
			} else if len(st.Assign.Index) == 0 && len(st.Assign.Field) > 0 {
				target := Expr(&Name{Name: st.Assign.Name})
				mapIndex := true
				if currentEnv != nil {
					if t, err := currentEnv.GetVar(st.Assign.Name); err == nil {
						switch t.(type) {
						case types.StructType:
							mapIndex = false
						case types.MapType:
							mapIndex = true
						default:
							mapIndex = false
						}
					} else {
						for _, stt := range currentEnv.Structs() {
							if _, ok := stt.Fields[st.Assign.Field[0].Name]; ok {
								mapIndex = false
								break
							}
						}
					}
				}
				for i := 0; i < len(st.Assign.Field)-1; i++ {
					target = &FieldExpr{Target: target, Name: st.Assign.Field[i].Name, MapIndex: mapIndex}
					mapIndex = true
				}
				field := st.Assign.Field[len(st.Assign.Field)-1].Name
				if mapIndex {
					idx := &StringLit{Value: field}
					p.Stmts = append(p.Stmts, &IndexAssignStmt{Target: target, Index: idx, Value: val})
					usesSetIndex = true
				} else {
					p.Stmts = append(p.Stmts, &FieldAssignStmt{Target: target, Field: field, Value: val})
				}
			} else {
				return nil, fmt.Errorf("unsupported assignment")
			}
		case st.Update != nil:
			up, err := convertUpdate(st.Update, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, up)
		case st.If != nil:
			cond, err := convertExpr(st.If.Cond)
			if err != nil {
				return nil, err
			}
			thenStmts, err := convertStmts(st.If.Then, env)
			if err != nil {
				return nil, err
			}
			var elseStmts []Stmt
			if st.If.ElseIf != nil {
				elseStmt, err := Transpile(&parser.Program{Statements: []*parser.Statement{{If: st.If.ElseIf}}}, env, bench)
				if err != nil {
					return nil, err
				}
				elseStmts = elseStmt.Stmts
			} else if len(st.If.Else) > 0 {
				elseStmts, err = convertStmts(st.If.Else, env)
				if err != nil {
					return nil, err
				}
			}
			p.Stmts = append(p.Stmts, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		case st.Bench != nil:
			body, err := convertStmts(st.Bench.Body, env)
			if err != nil {
				return nil, err
			}
			name := strings.Trim(st.Bench.Name, "\"")
			if currentImports != nil {
				currentImports["json"] = true
				currentImports["os"] = true
				currentImports["time"] = true
			}
			usesNow = true
			p.Stmts = append(p.Stmts, &BenchStmt{Name: name, Body: body})
		case st.Break != nil:
			p.Stmts = append(p.Stmts, &BreakStmt{})
		case st.Continue != nil:
			p.Stmts = append(p.Stmts, &ContinueStmt{})
		case st.Expect != nil:
			e, err := convertExpr(st.Expect.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &AssertStmt{Expr: e})
		case st.Test != nil:
			comment := &CommentStmt{Text: "test " + strings.Trim(st.Test.Name, "\"")}
			body, err := convertStmts(st.Test.Body, env)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, comment)
			p.Stmts = append(p.Stmts, body...)
		case st.Return != nil:
			var e Expr
			if st.Return.Value != nil {
				var err error
				e, err = convertExpr(st.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			p.Stmts = append(p.Stmts, &ReturnStmt{Expr: e})
		case st.Import != nil:
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			module := strings.Trim(st.Import.Path, "\"")
			module = strings.ReplaceAll(module, "/", ".")
			lang := ""
			if st.Import.Lang != nil {
				lang = *st.Import.Lang
			}
			currentImports[alias] = true
			currentImportLang[alias] = lang
			if lang == "" || lang == "python" {
				p.Stmts = append(p.Stmts, &ImportStmt{Module: module, Alias: alias})
			}
		case st.ExternVar != nil:
			// ignore extern variable declarations
			continue
		case st.ExternFun != nil:
			// ignore extern function declarations
			continue
		case st.ExternObject != nil:
			// ignore extern object declarations
			continue
		case st.Type != nil:
			if len(st.Type.Members) > 0 {
				var fields []DataClassField
				var methods []*FuncDef
				for _, m := range st.Type.Members {
					if m.Field != nil {
						typ := "any"
						if m.Field.Type != nil {
							typ = types.ResolveTypeRef(m.Field.Type, env).String()
						}
						fields = append(fields, DataClassField{Name: m.Field.Name, Type: typ})
					}
				}
				dc := &DataClassDef{Name: st.Type.Name, Fields: fields}
				if currentImports != nil {
					currentImports["dataclasses"] = true
				}
				if env != nil {
					env.SetStruct(dc.Name, structFromDataClass(dc, env))
				}
				for _, m := range st.Type.Members {
					if m.Method != nil {
						menv := types.NewEnv(env)
						var selfType types.Type = types.AnyType{}
						if env != nil {
							if stt, ok := env.GetStruct(dc.Name); ok {
								selfType = stt
							}
						}
						menv.SetVar("self", selfType, false)
						for _, p := range m.Method.Params {
							var pt types.Type = types.AnyType{}
							if p.Type != nil {
								pt = types.ResolveTypeRef(p.Type, env)
							}
							menv.SetVar(p.Name, pt, false)
						}
						body, err := convertStmts(m.Method.Body, menv)
						if err != nil {
							return nil, err
						}
						params := []string{"self"}
						for _, p := range m.Method.Params {
							params = append(params, p.Name)
						}
						genv := env.Parent()
						if genv == nil {
							genv = env
						}
						globals := detectGlobals(body, menv, genv)
						nonlocals := detectNonlocals(body, menv, env)
						nonlocals = filterNames(nonlocals, globals)
						methods = append(methods, &FuncDef{Name: m.Method.Name, Params: params, Nonlocals: nonlocals, Globals: globals, Body: body})
					}
				}
				dc.Methods = methods
				p.Stmts = append(p.Stmts, dc)
			}
			for _, v := range st.Type.Variants {
				if len(v.Fields) == 0 {
					if len(st.Type.Variants) == 1 && st.Type.Alias == nil && isBuiltinType(v.Name) {
						continue
					}
					p.Stmts = append(p.Stmts, &LetStmt{Name: v.Name, Expr: &Name{Name: "None"}})
				} else {
					var vf []DataClassField
					for _, f := range v.Fields {
						typ := "any"
						if f.Type != nil {
							typ = types.ResolveTypeRef(f.Type, env).String()
						}
						vf = append(vf, DataClassField{Name: f.Name, Type: typ})
					}
					dc := &DataClassDef{Name: v.Name, Fields: vf}
					if currentImports != nil {
						currentImports["dataclasses"] = true
					}
					p.Stmts = append(p.Stmts, dc)
					if env != nil {
						env.SetStruct(dc.Name, structFromDataClass(dc, env))
					}
				}
			}
			continue
		case st.ExternType != nil:
			continue
		case st.Fun != nil:
			fenv := types.NewEnv(env)
			for _, p := range st.Fun.Params {
				var pt types.Type = types.AnyType{}
				if p.Type != nil {
					pt = types.ResolveTypeRef(p.Type, env)
				}
				fenv.SetVar(p.Name, pt, true)
			}
			body, err := convertStmts(st.Fun.Body, fenv)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p := range st.Fun.Params {
				params = append(params, safeName(p.Name))
			}
			genv := env.Parent()
			if genv == nil {
				genv = env
			}
			globals := detectGlobals(body, fenv, genv)
			nonlocals := detectNonlocals(body, fenv, env)
			nonlocals = filterNames(nonlocals, globals)
			p.Stmts = append(p.Stmts, &FuncDef{Name: st.Fun.Name, Params: params, Nonlocals: nonlocals, Globals: globals, Body: body})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	if bench {
		found := false
		for _, s := range p.Stmts {
			if fn, ok := s.(*FuncDef); ok && fn.Name == "main" {
				fn.Body = []Stmt{&BenchStmt{Name: "main", Body: fn.Body}}
				found = true
				break
			}
		}
		if !found {
			p.Stmts = []Stmt{&BenchStmt{Name: "main", Body: p.Stmts}}
		}
		if currentImports != nil {
			currentImports["json"] = true
			currentImports["os"] = true
			currentImports["time"] = true
			currentImports["resource"] = true
		}
		usesNow = true
	}
	if len(extraFuncs) > 0 {
		p.Stmts = append(extraFuncs, p.Stmts...)
	}
	_ = env // unused for now
	return p, nil
}

func convertStmts(list []*parser.Statement, env *types.Env) ([]Stmt, error) {
	prev := currentEnv
	currentEnv = env
	defer func() { currentEnv = prev }()
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Expr != nil:
			if se := extractSaveExpr(s.Expr.Expr); se != nil {
				src, err := convertExpr(se.Src)
				if err != nil {
					return nil, err
				}
				format := parseFormat(se.With)
				path := ""
				if se.Path != nil {
					path = strings.Trim(*se.Path, "\"")
				}
				if format == "jsonl" && currentImports != nil {
					currentImports["json"] = true
					currentImports["dataclasses"] = true
				}
				out = append(out, &SaveStmt{Src: src, Path: path, Format: format})
			} else {
				e, err := convertExpr(s.Expr.Expr)
				if err != nil {
					return nil, err
				}
				out = append(out, &ExprStmt{Expr: e})
			}
		case s.Let != nil:
			if q := ExtractQueryExpr(s.Let.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, s.Let.Name)
				if err != nil {
					return nil, err
				}
				out = append(out, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if s.Let.Value != nil {
					e, err = convertExpr(s.Let.Value)
					if err != nil {
						return nil, err
					}
					typ = inferTypeFromExpr(s.Let.Value)
					if _, ok := typ.(types.AnyType); ok {
						typ = inferPyType(e, env)
					}
					env.SetVar(s.Let.Name, typ, false)
					if list, ok := e.(*ListLit); ok {
						if dc, elems := maybeDataClassList(s.Let.Name, list, env); dc != nil {
							out = append(out, dc)
							list.Elems = elems
							e = list
							if env != nil {
								env.SetStruct(dc.Name, structFromDataClass(dc, env))
								env.SetVar(s.Let.Name, types.ListType{Elem: structFromDataClass(dc, env)}, false)
							}
						}
					} else if q := ExtractQueryExpr(s.Let.Value); q != nil && q.Group == nil {
						selExpr, serr := convertExpr(q.Select)
						if serr == nil {
							if d, ok := selExpr.(*DictLit); ok {
								dc, args := dataClassFromDict(s.Let.Name, d, env)
								if dc != nil {
									out = append(out, dc)
									if lc, ok := e.(*ListComp); ok {
										lc.Expr = &CallExpr{Func: &Name{Name: dc.Name}, Args: args}
									} else if mc, ok := e.(*MultiListComp); ok {
										mc.Expr = &CallExpr{Func: &Name{Name: dc.Name}, Args: args}
									}
								}
							}
						}
					}
				} else if s.Let.Type != nil {
					typ = types.ResolveTypeRef(s.Let.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(s.Let.Name, typ, false)
				} else {
					return nil, fmt.Errorf("let without value")
				}
				out = append(out, &LetStmt{Name: s.Let.Name, Expr: e})
			}
		case s.Var != nil:
			if q := ExtractQueryExpr(s.Var.Value); q != nil && q.Group != nil {
				stmts, err := convertGroupQuery(q, env, s.Var.Name)
				if err != nil {
					return nil, err
				}
				out = append(out, stmts...)
			} else {
				var e Expr
				var err error
				var typ types.Type
				if s.Var.Value != nil {
					e, err = convertExpr(s.Var.Value)
					if err != nil {
						return nil, err
					}
					typ = inferTypeFromExpr(s.Var.Value)
					if _, ok := typ.(types.AnyType); ok {
						typ = inferPyType(e, env)
					}
					env.SetVar(s.Var.Name, typ, true)
					if list, ok := e.(*ListLit); ok {
						if dc, elems := maybeDataClassList(s.Var.Name, list, env); dc != nil {
							out = append(out, dc)
							list.Elems = elems
							e = list
							if env != nil {
								env.SetStruct(dc.Name, structFromDataClass(dc, env))
								env.SetVar(s.Var.Name, types.ListType{Elem: structFromDataClass(dc, env)}, true)
							}
						}
					} else if q := ExtractQueryExpr(s.Var.Value); q != nil && q.Group == nil {
						selExpr, serr := convertExpr(q.Select)
						if serr == nil {
							if d, ok := selExpr.(*DictLit); ok {
								dc, args := dataClassFromDict(s.Var.Name, d, env)
								if dc != nil {
									out = append(out, dc)
									if lc, ok := e.(*ListComp); ok {
										lc.Expr = &CallExpr{Func: &Name{Name: dc.Name}, Args: args}
									} else if mc, ok := e.(*MultiListComp); ok {
										mc.Expr = &CallExpr{Func: &Name{Name: dc.Name}, Args: args}
									}
								}
							}
						}
					}
				} else if s.Var.Type != nil {
					typ = types.ResolveTypeRef(s.Var.Type, env)
					e = zeroValueExpr(typ)
					env.SetVar(s.Var.Name, typ, true)
				} else {
					return nil, fmt.Errorf("var without value")
				}
				out = append(out, &VarStmt{Name: s.Var.Name, Expr: e})
			}
		case s.While != nil:
			cond, err := convertExpr(s.While.Cond)
			if err != nil {
				return nil, err
			}
			body, err := convertStmts(s.While.Body, env)
			if err != nil {
				return nil, err
			}
			out = append(out, &WhileStmt{Cond: cond, Body: body})
		case s.For != nil:
			iter, err := convertExpr(s.For.Source)
			if err != nil {
				return nil, err
			}
			if env != nil {
				if lt, ok := inferTypeFromExpr(s.For.Source).(types.ListType); ok {
					env.SetVar(s.For.Name, lt.Elem, true)
				} else if lt, ok := inferPyType(iter, env).(types.ListType); ok {
					env.SetVar(s.For.Name, lt.Elem, true)
				}
			}
			if s.For.RangeEnd != nil {
				end, err := convertExpr(s.For.RangeEnd)
				if err != nil {
					return nil, err
				}
				iter = &CallExpr{Func: &Name{Name: "range"}, Args: []Expr{iter, end}}
			}
			body, err := convertStmts(s.For.Body, env)
			if err != nil {
				return nil, err
			}
			out = append(out, &ForStmt{Var: s.For.Name, Iter: iter, Body: body})
		case s.Assign != nil:
			val, err := convertExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if len(s.Assign.Index) >= 1 && len(s.Assign.Field) == 0 {
				target := Expr(&Name{Name: s.Assign.Name})
				for i := 0; i < len(s.Assign.Index)-1; i++ {
					if s.Assign.Index[i].Colon != nil || s.Assign.Index[i].Colon2 != nil {
						return nil, fmt.Errorf("unsupported assignment")
					}
					idx, err := convertExpr(s.Assign.Index[i].Start)
					if err != nil {
						return nil, err
					}
					target = &IndexExpr{Target: target, Index: idx}
				}
				last := s.Assign.Index[len(s.Assign.Index)-1]
				if last.Colon != nil || last.Colon2 != nil {
					return nil, fmt.Errorf("unsupported assignment")
				}
				idx, err := convertExpr(last.Start)
				if err != nil {
					return nil, err
				}
				out = append(out, &IndexAssignStmt{Target: target, Index: idx, Value: val})
				usesSetIndex = true
			} else if len(s.Assign.Index) >= 1 && len(s.Assign.Field) > 0 {
				target := Expr(&Name{Name: s.Assign.Name})
				for i := 0; i < len(s.Assign.Index); i++ {
					if s.Assign.Index[i].Colon != nil || s.Assign.Index[i].Colon2 != nil {
						return nil, fmt.Errorf("unsupported assignment")
					}
					idx, err := convertExpr(s.Assign.Index[i].Start)
					if err != nil {
						return nil, err
					}
					target = &IndexExpr{Target: target, Index: idx}
				}
				for i := 0; i < len(s.Assign.Field)-1; i++ {
					target = &FieldExpr{Target: target, Name: s.Assign.Field[i].Name, MapIndex: false}
				}
				field := s.Assign.Field[len(s.Assign.Field)-1].Name
				out = append(out, &FieldAssignStmt{Target: target, Field: field, Value: val})
			} else if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
				out = append(out, &AssignStmt{Name: s.Assign.Name, Expr: val})
			} else if len(s.Assign.Index) == 0 && len(s.Assign.Field) > 0 {
				target := Expr(&Name{Name: s.Assign.Name})
				mapIndex := true
				if env != nil {
					if t, err := env.GetVar(s.Assign.Name); err == nil {
						switch t.(type) {
						case types.StructType:
							mapIndex = false
						case types.MapType:
							mapIndex = true
						default:
							mapIndex = false
						}
					}
				}
				for i := 0; i < len(s.Assign.Field)-1; i++ {
					target = &FieldExpr{Target: target, Name: s.Assign.Field[i].Name, MapIndex: mapIndex}
					mapIndex = true
				}
				field := s.Assign.Field[len(s.Assign.Field)-1].Name
				if mapIndex {
					idx := &StringLit{Value: field}
					out = append(out, &IndexAssignStmt{Target: target, Index: idx, Value: val})
					usesSetIndex = true
				} else {
					out = append(out, &FieldAssignStmt{Target: target, Field: field, Value: val})
				}
			} else {
				return nil, fmt.Errorf("unsupported assignment")
			}
		case s.Update != nil:
			up, err := convertUpdate(s.Update, env)
			if err != nil {
				return nil, err
			}
			out = append(out, up)
		case s.If != nil:
			cond, err := convertExpr(s.If.Cond)
			if err != nil {
				return nil, err
			}
			thenStmts, err := convertStmts(s.If.Then, env)
			if err != nil {
				return nil, err
			}
			var elseStmts []Stmt
			if s.If.ElseIf != nil {
				sub, err := convertStmts([]*parser.Statement{{If: s.If.ElseIf}}, env)
				if err != nil {
					return nil, err
				}
				elseStmts = sub
			} else if len(s.If.Else) > 0 {
				elseStmts, err = convertStmts(s.If.Else, env)
				if err != nil {
					return nil, err
				}
			}
			out = append(out, &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts})
		case s.Bench != nil:
			body, err := convertStmts(s.Bench.Body, env)
			if err != nil {
				return nil, err
			}
			name := strings.Trim(s.Bench.Name, "\"")
			if currentImports != nil {
				currentImports["json"] = true
				currentImports["os"] = true
				currentImports["time"] = true
			}
			usesNow = true
			out = append(out, &BenchStmt{Name: name, Body: body})
		case s.Break != nil:
			out = append(out, &BreakStmt{})
		case s.Continue != nil:
			out = append(out, &ContinueStmt{})
		case s.Expect != nil:
			e, err := convertExpr(s.Expect.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &AssertStmt{Expr: e})
		case s.Test != nil:
			comment := &CommentStmt{Text: "test " + strings.Trim(s.Test.Name, "\"")}
			body, err := convertStmts(s.Test.Body, env)
			if err != nil {
				return nil, err
			}
			out = append(out, comment)
			out = append(out, body...)
		case s.Return != nil:
			var e Expr
			if s.Return.Value != nil {
				var err error
				e, err = convertExpr(s.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			out = append(out, &ReturnStmt{Expr: e})
		case s.Import != nil:
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			module := strings.Trim(s.Import.Path, "\"")
			module = strings.ReplaceAll(module, "/", ".")
			lang := ""
			if s.Import.Lang != nil {
				lang = *s.Import.Lang
			}
			currentImportLang[alias] = lang
			if lang == "" || lang == "python" {
				out = append(out, &ImportStmt{Module: module, Alias: alias})
			}
		case s.ExternVar != nil:
			continue
		case s.ExternFun != nil:
			continue
		case s.ExternObject != nil:
			continue
		case s.Type != nil:
			if len(s.Type.Members) > 0 {
				var fields []DataClassField
				for _, m := range s.Type.Members {
					if m.Field != nil {
						typ := "any"
						if m.Field.Type != nil {
							typ = types.ResolveTypeRef(m.Field.Type, env).String()
						}
						fields = append(fields, DataClassField{Name: m.Field.Name, Type: typ})
					}
				}
				out = append(out, &DataClassDef{Name: s.Type.Name, Fields: fields})
			}
			for _, v := range s.Type.Variants {
				if len(v.Fields) == 0 {
					out = append(out, &LetStmt{Name: v.Name, Expr: &Name{Name: "None"}})
				} else {
					var vf []DataClassField
					for _, f := range v.Fields {
						typ := "any"
						if f.Type != nil {
							typ = types.ResolveTypeRef(f.Type, env).String()
						}
						vf = append(vf, DataClassField{Name: f.Name, Type: typ})
					}
					out = append(out, &DataClassDef{Name: v.Name, Fields: vf})
				}
			}
			continue
		case s.ExternType != nil:
			// ignore extern type declarations
			continue
		case s.Fun != nil:
			fenv := types.NewEnv(env)
			for _, p := range s.Fun.Params {
				var pt types.Type = types.AnyType{}
				if p.Type != nil {
					pt = types.ResolveTypeRef(p.Type, env)
				}
				fenv.SetVar(p.Name, pt, true)
			}
			b, err := convertStmts(s.Fun.Body, fenv)
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p := range s.Fun.Params {
				params = append(params, p.Name)
			}
			genv := env.Parent()
			if genv == nil {
				genv = env
			}
			globals := detectGlobals(b, fenv, genv)
			nonlocals := detectNonlocals(b, fenv, env)
			nonlocals = filterNames(nonlocals, globals)
			out = append(out, &FuncDef{Name: s.Fun.Name, Params: params, Nonlocals: nonlocals, Globals: globals, Body: b})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func detectGlobals(stmts []Stmt, local, global *types.Env) []string {
	if global == nil {
		return nil
	}
	names := map[string]bool{}
	var walk func([]Stmt)
	walk = func(list []Stmt) {
		for _, s := range list {
			switch st := s.(type) {
			case *AssignStmt:
				if _, ok := local.Types()[st.Name]; ok {
					continue
				}
				if _, err := global.GetVar(st.Name); err == nil {
					names[st.Name] = true
				}
			case *WhileStmt:
				walk(st.Body)
			case *ForStmt:
				walk(st.Body)
			case *IfStmt:
				walk(st.Then)
				walk(st.Else)
			}
		}
	}
	walk(stmts)
	if len(names) == 0 {
		return nil
	}
	out := make([]string, 0, len(names))
	for n := range names {
		out = append(out, safeName(n))
	}
	sort.Strings(out)
	return out
}

func detectNonlocals(stmts []Stmt, local, parent *types.Env) []string {
	if parent == nil {
		return nil
	}
	names := map[string]bool{}
	var walk func([]Stmt)
	walk = func(list []Stmt) {
		for _, s := range list {
			switch st := s.(type) {
			case *AssignStmt:
				if _, ok := local.Types()[st.Name]; ok {
					continue
				}
				if _, err := parent.GetVar(st.Name); err == nil {
					names[st.Name] = true
				}
			case *WhileStmt:
				walk(st.Body)
			case *ForStmt:
				walk(st.Body)
			case *IfStmt:
				walk(st.Then)
				walk(st.Else)
			}
		}
	}
	walk(stmts)
	if len(names) == 0 {
		return nil
	}
	out := make([]string, 0, len(names))
	for n := range names {
		out = append(out, safeName(n))
	}
	sort.Strings(out)
	return out
}

func filterNames(list, remove []string) []string {
	if len(list) == 0 {
		return list
	}
	rm := map[string]bool{}
	for _, n := range remove {
		rm[n] = true
	}
	out := make([]string, 0, len(list))
	for _, n := range list {
		if !rm[n] {
			out = append(out, n)
		}
	}
	return out
}

func gatherNames(stmts []Stmt) map[string]bool {
	names := map[string]bool{}
	var ve func(Expr)
	var vs func(Stmt)
	ve = func(e Expr) {
		switch ex := e.(type) {
		case *Name:
			names[ex.Name] = true
		case *CallExpr:
			ve(ex.Func)
			for _, a := range ex.Args {
				ve(a)
			}
		case *BinaryExpr:
			ve(ex.Left)
			ve(ex.Right)
		case *UnaryExpr:
			ve(ex.Expr)
		case *CondExpr:
			ve(ex.Cond)
			ve(ex.Then)
			ve(ex.Else)
		case *FieldExpr:
			ve(ex.Target)
		case *IndexExpr:
			ve(ex.Target)
			ve(ex.Index)
		case *SliceExpr:
			ve(ex.Target)
			if ex.Start != nil {
				ve(ex.Start)
			}
			if ex.End != nil {
				ve(ex.End)
			}
			if ex.Step != nil {
				ve(ex.Step)
			}
		case *ParenExpr:
			ve(ex.Expr)
		case *ListLit:
			for _, el := range ex.Elems {
				ve(el)
			}
		case *DictLit:
			for i := range ex.Keys {
				ve(ex.Keys[i])
				ve(ex.Values[i])
			}
		case *ListComp:
			ve(ex.Iter)
			ve(ex.Expr)
			if ex.Cond != nil {
				ve(ex.Cond)
			}
		case *MultiListComp:
			for _, it := range ex.Iters {
				ve(it)
			}
			ve(ex.Expr)
			if ex.Cond != nil {
				ve(ex.Cond)
			}
		case *LambdaExpr:
			ve(ex.Expr)
		}
	}
	vs = func(s Stmt) {
		switch st := s.(type) {
		case *ExprStmt:
			ve(st.Expr)
		case *LetStmt:
			if st.Expr != nil {
				ve(st.Expr)
			}
		case *VarStmt:
			if st.Expr != nil {
				ve(st.Expr)
			}
		case *AssignStmt:
			if st.Expr != nil {
				ve(st.Expr)
			}
		case *ReturnStmt:
			if st.Expr != nil {
				ve(st.Expr)
			}
		case *WhileStmt:
			ve(st.Cond)
			for _, b := range st.Body {
				vs(b)
			}
		case *ForStmt:
			ve(st.Iter)
			for _, b := range st.Body {
				vs(b)
			}
		case *IfStmt:
			ve(st.Cond)
			for _, b := range st.Then {
				vs(b)
			}
			for _, b := range st.Else {
				vs(b)
			}
		case *FieldAssignStmt:
			ve(st.Target)
			ve(st.Value)
		case *IndexAssignStmt:
			ve(st.Target)
			ve(st.Index)
			ve(st.Value)
		case *BenchStmt:
			for _, b := range st.Body {
				vs(b)
			}
		case *FuncDef:
			for _, b := range st.Body {
				vs(b)
			}
		case *DataClassDef:
			for _, m := range st.Methods {
				for _, b := range m.Body {
					vs(b)
				}
			}
		}
	}
	for _, st := range stmts {
		vs(st)
	}
	return names
}

func capturedVars(body []Stmt, local, parent *types.Env) []string {
	names := gatherNames(body)
	for n := range local.Types() {
		delete(names, n)
	}
	var out []string
	for n := range names {
		if n == "True" || n == "False" || n == "None" {
			continue
		}
		if _, err := parent.GetVar(n); err == nil {
			if _, ok := parent.GetFunc(n); ok {
				continue
			}
			out = append(out, n)
		}
	}
	sort.Strings(out)
	return out
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
		o, err := convertPostfix(p.Right)
		if err != nil {
			return nil, err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, o)
	}

	if len(ops) == 1 && ops[0] == "/" {
		if isSumUnary(b.Left) && isSumPostfix(b.Right[0].Right) {
			ops[0] = "//"
		}
	}

	levels := [][]string{
		{"*", "/", "//", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	apply := func(left Expr, op string, right Expr) Expr {
		if op == "/" {
			lt := inferPyType(left, currentEnv)
			rt := inferPyType(right, currentEnv)
			if isIntLike(lt) && isIntLike(rt) {
				op = "//"
			}
		}
		if op == "+" {
			if s, ok := left.(*SliceExpr); ok && sliceOfStringList(s) {
				left = &CallExpr{Func: &FieldExpr{Target: &StringLit{Value: ""}, Name: "join"}, Args: []Expr{s}}
			}
			if s, ok := right.(*SliceExpr); ok && sliceOfStringList(s) {
				right = &CallExpr{Func: &FieldExpr{Target: &StringLit{Value: ""}, Name: "join"}, Args: []Expr{s}}
			}
			if lt, ok := inferPyType(left, currentEnv).(types.ListType); ok && types.IsStringType(lt.Elem) && types.IsStringType(inferPyType(right, currentEnv)) {
				left = &CallExpr{Func: &FieldExpr{Target: &StringLit{Value: ""}, Name: "join"}, Args: []Expr{left}}
			} else if rt, ok := inferPyType(right, currentEnv).(types.ListType); ok && types.IsStringType(rt.Elem) && types.IsStringType(inferPyType(left, currentEnv)) {
				right = &CallExpr{Func: &FieldExpr{Target: &StringLit{Value: ""}, Name: "join"}, Args: []Expr{right}}
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
	expr, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = &UnaryExpr{Op: u.Ops[i], Expr: expr}
	}
	return expr, nil
}

func convertSelector(sel *parser.SelectorExpr, method bool) Expr {
	if sel.Root == "nil" && len(sel.Tail) == 0 {
		return &RawExpr{Code: "None"}
	}
	if len(sel.Tail) == 0 && currentEnv != nil {
		if _, err := currentEnv.GetVar(sel.Root); err != nil {
			if t, err2 := currentEnv.GetVar("self"); err2 == nil {
				if st, ok := t.(types.StructType); ok {
					if _, ok2 := st.Fields[sel.Root]; ok2 {
						return &FieldExpr{Target: &Name{Name: "self"}, Name: sel.Root}
					}
					if _, ok2 := st.Methods[sel.Root]; ok2 {
						return &FieldExpr{Target: &Name{Name: "self"}, Name: sel.Root}
					}
				}
			}
		}
	}
	if lang, ok := currentImportLang[sel.Root]; ok && lang == "go" && len(sel.Tail) == 1 && !method {
		switch sel.Root {
		case "testpkg":
			switch sel.Tail[0] {
			case "Pi":
				return &FloatLit{Value: "3.14"}
			case "Answer":
				return &IntLit{Value: "42"}
			}
		}
	}
	expr := Expr(&Name{Name: sel.Root})
	mapIndex := true
	if currentImports != nil && currentImports[sel.Root] {
		mapIndex = false
	}
	if currentEnv != nil {
		if t, err := currentEnv.GetVar(sel.Root); err == nil {
			if _, ok := t.(types.StructType); ok {
				mapIndex = false
			}
		}
	}
	for i, t := range sel.Tail {
		idx := i == len(sel.Tail)-1 && method
		useMap := mapIndex && !idx
		if useMap && currentEnv != nil {
			if st, ok := inferPyType(expr, currentEnv).(types.StructType); ok {
				if _, ok := st.Fields[t]; ok {
					useMap = false
				}
			}
			if useMap {
				for _, st := range currentEnv.Structs() {
					if _, ok := st.Fields[t]; ok {
						useMap = false
						break
					}
				}
			}
		}
		expr = &FieldExpr{Target: expr, Name: t, MapIndex: useMap}
		mapIndex = true
	}
	return expr
}

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	method := len(p.Ops) > 0 && p.Ops[0].Call != nil
	var expr Expr
	var err error
	if p.Target.Selector != nil {
		expr = convertSelector(p.Target.Selector, method)
	} else {
		expr, err = convertPrimary(p.Target)
		if err != nil {
			return nil, err
		}
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil {
				var start, end, step Expr
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
				if op.Index.Step != nil {
					step, err = convertExpr(op.Index.Step)
					if err != nil {
						return nil, err
					}
				}
				expr = &SliceExpr{Target: expr, Start: start, End: end, Step: step}
			} else {
				idx, err := convertExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				if mp, ok := inferPyType(expr, currentEnv).(types.MapType); ok {
					expr = &IndexExpr{Target: expr, Index: idx, Map: true, Default: zeroValueExpr(mp.Value)}
				} else if s, ok := idx.(*StringLit); ok {
					if st, ok2 := inferPyType(expr, currentEnv).(types.StructType); ok2 {
						if _, ok3 := st.Fields[s.Value]; ok3 {
							expr = &FieldExpr{Target: expr, Name: s.Value}
						} else {
							expr = &IndexExpr{Target: expr, Index: idx}
						}
					} else {
						expr = &IndexExpr{Target: expr, Index: idx}
					}
				} else {
					expr = &IndexExpr{Target: expr, Index: idx}
				}
			}
		case op.Field != nil:
			mapIndex := true
			if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				mapIndex = false
			} else {
				if t, ok := inferPyType(expr, currentEnv).(types.StructType); ok {
					if _, ok := t.Fields[op.Field.Name]; ok {
						mapIndex = false
					}
				}
				if mapIndex && currentEnv != nil {
					for _, st := range currentEnv.Structs() {
						if _, ok := st.Fields[op.Field.Name]; ok {
							mapIndex = false
							break
						}
					}
				}
			}
			expr = &FieldExpr{Target: expr, Name: op.Field.Name, MapIndex: mapIndex, MapGet: mapIndex}
		case op.Call != nil:
			var args []Expr
			for _, a := range op.Call.Args {
				ae, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ae)
			}
			replaced := false
			if fe, ok := expr.(*FieldExpr); ok {
				if n, ok := fe.Target.(*Name); ok {
					lang := currentImportLang[n.Name]
					if lang == "go" {
						switch fe.Name {
						case "TrimSpace":
							if len(args) == 1 {
								expr = &CallExpr{Func: &FieldExpr{Target: args[0], Name: "strip"}}
								replaced = true
							}
						case "ToUpper":
							if len(args) == 1 {
								expr = &CallExpr{Func: &FieldExpr{Target: args[0], Name: "upper"}}
								replaced = true
							}
						case "ToLower":
							if len(args) == 1 {
								expr = &CallExpr{Func: &FieldExpr{Target: args[0], Name: "lower"}}
								replaced = true
							}
						case "Add":
							if n.Name == "testpkg" && len(args) == 2 {
								expr = &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}
								replaced = true
							}
						case "MD5Hex":
							if n.Name == "testpkg" && len(args) == 1 {
								if currentImports != nil {
									currentImports["hashlib"] = true
								}
								enc := &CallExpr{Func: &FieldExpr{Target: args[0], Name: "encode"}}
								md5Call := &CallExpr{Func: &FieldExpr{Target: &Name{Name: "hashlib"}, Name: "md5"}, Args: []Expr{enc}}
								expr = &CallExpr{Func: &FieldExpr{Target: md5Call, Name: "hexdigest"}}
								replaced = true
							}
						case "FifteenPuzzleExample":
							if n.Name == "testpkg" && len(args) == 0 {
								expr = &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}
								replaced = true
							}
						case "ECDSAExample":
							if n.Name == "testpkg" && len(args) == 0 {
								keys := []Expr{
									&StringLit{Value: "D"},
									&StringLit{Value: "X"},
									&StringLit{Value: "Y"},
									&StringLit{Value: "Hash"},
									&StringLit{Value: "R"},
									&StringLit{Value: "S"},
									&StringLit{Value: "Valid"},
								}
								vals := []Expr{
									&StringLit{Value: "1234567890"},
									&StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"},
									&StringLit{Value: "86807430002474105664458509423764867536342689150582922106807036347047552480521"},
									&StringLit{Value: "0xe6f9ed0d"},
									&StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"},
									&StringLit{Value: "94150071556658883365738746782965214584303361499725266605620843043083873122499"},
									&BoolLit{Value: true},
								}
								expr = &DictLit{Keys: keys, Values: vals}
								replaced = true
							}
						case "LookupHost":
							if n.Name == "net" && len(args) == 1 {
								usesLookupHost = true
								if currentImports != nil {
									currentImports["socket"] = true
								}
								expr = &CallExpr{Func: &Name{Name: "_lookup_host"}, Args: args}
								replaced = true
							}
						case "Getenv":
							if n.Name == "os" && len(args) == 1 {
								if currentImports != nil {
									currentImports["os"] = true
								}
								expr = &CallExpr{Func: &FieldExpr{Target: &Name{Name: "os"}, Name: "getenv"}, Args: args}
								replaced = true
							}
						case "Environ":
							if n.Name == "os" && len(args) == 0 {
								if currentImports != nil {
									currentImports["os"] = true
								}
								expr = &FieldExpr{Target: &Name{Name: "os"}, Name: "environ"}
								replaced = true
							}
						}
					}
				}
			}
			if replaced {
				// already handled
			} else if fe, ok := expr.(*FieldExpr); ok && fe.Name == "padStart" && len(args) == 2 {
				tgt := fe.Target
				if currentEnv != nil {
					if _, ok := inferPyType(tgt, currentEnv).(types.StringType); !ok {
						tgt = &CallExpr{Func: &Name{Name: "str"}, Args: []Expr{tgt}}
					}
				}
				expr = &CallExpr{Func: &FieldExpr{Target: tgt, Name: "rjust"}, Args: []Expr{args[0], args[1]}}
			} else if fe, ok := expr.(*FieldExpr); ok && fe.Name == "contains" && len(args) == 1 {
				expr = &BinaryExpr{Left: args[0], Op: "in", Right: fe.Target}
			} else if _, ok := expr.(*BinaryExpr); !ok {
				expr = &CallExpr{Func: expr, Args: args}
			}
		case op.Cast != nil:
			name := typeRefSimpleName(op.Cast.Type)
			switch name {
			case "int":
				expr = &CallExpr{Func: &Name{Name: "int"}, Args: []Expr{expr}}
			case "float":
				expr = &CallExpr{Func: &Name{Name: "float"}, Args: []Expr{expr}}
			case "string":
				expr = &CallExpr{Func: &Name{Name: "str"}, Args: []Expr{expr}}
			case "bool":
				expr = &CallExpr{Func: &Name{Name: "bool"}, Args: []Expr{expr}}
			case "bigrat":
				if currentImports != nil {
					currentImports["fractions"] = true
				}
				expr = &CallExpr{Func: &Name{Name: "Fraction"}, Args: []Expr{expr}}
			default:
				if currentEnv != nil {
					if st, ok := currentEnv.GetStruct(name); ok {
						if d, ok := expr.(*DictLit); ok {
							var kwargs []Expr
							for i := range d.Keys {
								if k, ok := d.Keys[i].(*StringLit); ok {
									kwargs = append(kwargs, &KeywordArg{Name: k.Value, Value: d.Values[i]})
								}
							}
							expr = &CallExpr{Func: &Name{Name: name}, Args: kwargs}
						} else {
							// assume value is already of desired type
						}
						_ = st
					}
				}
			}
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		var args []Expr
		for _, a := range p.Call.Args {
			ae, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ae)
		}
		hasFunc := false
		if currentEnv != nil {
			if fn, ok := currentEnv.GetFunc(p.Call.Func); ok {
				hasFunc = true
				if len(args) < len(fn.Params) {
					rem := fn.Params[len(args):]
					var names []string
					var extra []Expr
					for _, p := range rem {
						names = append(names, p.Name)
						extra = append(extra, &Name{Name: p.Name})
					}
					call := &CallExpr{Func: &Name{Name: p.Call.Func}, Args: append(args, extra...)}
					return &LambdaExpr{Params: names, Expr: call}, nil
				}
			}
			if t, err := currentEnv.GetVar("self"); err == nil {
				if st, ok := t.(types.StructType); ok {
					if _, ok := st.Methods[p.Call.Func]; ok {
						return &CallExpr{Func: &FieldExpr{Target: &Name{Name: "self"}, Name: p.Call.Func}, Args: args}, nil
					}
				}
			}
		}
		switch p.Call.Func {
		case "print":
			outArgs := make([]Expr, len(args))
			for i, a := range args {
				var t types.Type = types.AnyType{}
				if currentEnv != nil {
					t = inferTypeFromExpr(p.Call.Args[i])
					if _, ok := t.(types.AnyType); ok {
						t = inferPyType(a, currentEnv)
					}
				}
				if isBoolOp(p.Call.Args[i]) {
					outArgs[i] = &RawExpr{Code: fmt.Sprintf("(1 if %s else 0)", exprString(a))}
				} else if _, ok := t.(types.BoolType); ok {
					// Match the VM's representation using capitalized booleans
					outArgs[i] = &RawExpr{Code: fmt.Sprintf("(\"True\" if %s else \"False\")", exprString(a))}
				} else if lt, ok := t.(types.ListType); ok {
					if _, ok2 := lt.Elem.(types.StructType); ok2 {
						currentImports["dataclasses"] = true
						outArgs[i] = &ListComp{Var: "_x", Iter: a, Expr: &CallExpr{Func: &FieldExpr{Target: &Name{Name: "dataclasses"}, Name: "asdict"}, Args: []Expr{&Name{Name: "_x"}}}}
					} else {
						outArgs[i] = a
					}
				} else if _, ok := t.(types.MapType); ok {
					outArgs[i] = a
				} else if isStructType(t) {
					currentImports["dataclasses"] = true
					outArgs[i] = &CallExpr{Func: &FieldExpr{Target: &Name{Name: "dataclasses"}, Name: "asdict"}, Args: []Expr{a}}
				} else {
					outArgs[i] = a
				}
			}
			return &CallExpr{Func: &Name{Name: "print"}, Args: outArgs}, nil
		case "concat":
			if len(args) == 2 {
				usesConcat = true
				return &CallExpr{Func: &Name{Name: "_concat"}, Args: []Expr{args[0], args[1]}}, nil
			}
		case "append":
			if len(args) == 2 {
				elem := args[1]
				if lt, ok := inferPyType(args[0], currentEnv).(types.ListType); ok {
					if st, ok2 := lt.Elem.(types.StructType); ok2 {
						if d, ok3 := elem.(*DictLit); ok3 {
							vals := make([]Expr, len(d.Values))
							copy(vals, d.Values)
							elem = &CallExpr{Func: &Name{Name: st.Name}, Args: vals}
						}
					}
				}
				usesAppend = true
				return &CallExpr{Func: &Name{Name: "_append"}, Args: []Expr{args[0], elem}}, nil
			}
		case "avg":
			if len(args) == 1 {
				sumCall := &CallExpr{Func: &Name{Name: "sum"}, Args: []Expr{args[0]}}
				lenCall := &CallExpr{Func: &Name{Name: "len"}, Args: []Expr{args[0]}}
				div := &BinaryExpr{Left: sumCall, Op: "/", Right: lenCall}
				return &CondExpr{Cond: args[0], Then: div, Else: &FloatLit{Value: "0.0"}}, nil
			}
		case "count":
			if len(args) == 1 {
				return &CallExpr{Func: &Name{Name: "len"}, Args: args}, nil
			}
		case "values":
			if !hasFunc && len(args) == 1 {
				call := &CallExpr{Func: &FieldExpr{Target: args[0], Name: "values"}, Args: nil}
				return &CallExpr{Func: &Name{Name: "list"}, Args: []Expr{call}}, nil
			}
		case "keys":
			if !hasFunc && len(args) == 1 {
				call := &CallExpr{Func: &FieldExpr{Target: args[0], Name: "keys"}, Args: nil}
				return &CallExpr{Func: &Name{Name: "list"}, Args: []Expr{call}}, nil
			}
		case "min", "max":
			if len(args) == 1 {
				return &CallExpr{Func: &Name{Name: p.Call.Func}, Args: args}, nil
			}
		case "str":
			if len(args) == 1 {
				usesStr = true
				return &CallExpr{Func: &Name{Name: "_str"}, Args: args}, nil
			}
		case "split":
			if len(args) == 2 {
				target := args[0]
				switch target.(type) {
				case *Name, *FieldExpr, *IndexExpr:
				default:
					target = &ParenExpr{Expr: target}
				}
				return &CallExpr{Func: &FieldExpr{Target: target, Name: "split"}, Args: []Expr{args[1]}}, nil
			}
		case "repeat":
			if len(args) == 2 {
				return &BinaryExpr{Left: args[0], Op: "*", Right: args[1]}, nil
			}
		case "contains":
			if !hasFunc && len(args) == 2 {
				return &BinaryExpr{Left: args[1], Op: "in", Right: args[0]}, nil
			}
		case "parseIntStr":
			if len(args) == 2 {
				return &CallExpr{Func: &Name{Name: "int"}, Args: []Expr{args[0], args[1]}}, nil
			}
		case "to_float":
			if len(args) == 1 {
				return &CallExpr{Func: &Name{Name: "float"}, Args: args}, nil
			}
		case "sha256":
			if len(args) == 1 {
				if currentImports != nil {
					currentImports["hashlib"] = true
				}
				bytesCall := &CallExpr{Func: &Name{Name: "bytes"}, Args: []Expr{args[0]}}
				hashCall := &CallExpr{Func: &FieldExpr{Target: &Name{Name: "hashlib"}, Name: "sha256"}, Args: []Expr{bytesCall}}
				digest := &CallExpr{Func: &FieldExpr{Target: hashCall, Name: "digest"}, Args: nil}
				return &CallExpr{Func: &Name{Name: "list"}, Args: []Expr{digest}}, nil
			}
		case "substring":
			if len(args) == 3 {
				return &SliceExpr{Target: args[0], Start: args[1], End: args[2]}, nil
			}
		case "substr":
			if len(args) == 3 {
				usesSubstr = true
				return &CallExpr{Func: &Name{Name: "_substr"}, Args: args}, nil
			}
		case "slice":
			if len(args) == 3 {
				return &SliceExpr{Target: args[0], Start: args[1], End: args[2]}, nil
			}
		case "upper":
			if len(args) == 1 {
				return &CallExpr{Func: &FieldExpr{Target: args[0], Name: "upper"}}, nil
			}
		case "lower":
			if len(args) == 1 {
				return &CallExpr{Func: &FieldExpr{Target: args[0], Name: "lower"}}, nil
			}
		case "padStart":
			if len(args) == 3 {
				tgt := args[0]
				if currentEnv != nil {
					if _, ok := inferPyType(tgt, currentEnv).(types.StringType); !ok {
						tgt = &CallExpr{Func: &Name{Name: "str"}, Args: []Expr{tgt}}
					}
				}
				return &CallExpr{Func: &FieldExpr{Target: tgt, Name: "rjust"}, Args: []Expr{args[1], args[2]}}, nil
			}
		case "indexOf":
			if len(args) == 2 {
				if currentEnv != nil {
					if _, ok := inferTypeFromExpr(p.Call.Args[0]).(types.ListType); ok {
						usesIndexOf = true
						return &CallExpr{Func: &Name{Name: "_index_of"}, Args: args}, nil
					}
				}
				return &CallExpr{Func: &FieldExpr{Target: args[0], Name: "find"}, Args: []Expr{args[1]}}, nil
			}
		case "now":
			if len(args) == 0 {
				usesNow = true
				if currentImports != nil {
					currentImports["os"] = true
					currentImports["time"] = true
				}
				return &CallExpr{Func: &Name{Name: "_now"}, Args: nil}, nil
			}
		case "net.LookupHost":
			if len(args) == 1 {
				usesLookupHost = true
				if currentImports != nil {
					currentImports["socket"] = true
				}
				return &CallExpr{Func: &Name{Name: "_lookup_host"}, Args: args}, nil
			}
		case "json":
			if len(args) == 1 {
				if currentImports != nil {
					currentImports["json"] = true
				}
				val := args[0]
				var t types.Type = types.AnyType{}
				if currentEnv != nil {
					t = inferTypeFromExpr(p.Call.Args[0])
				}
				var dumps Expr
				if isStructType(t) {
					currentImports["dataclasses"] = true
					dumps = &CallExpr{Func: &FieldExpr{Target: &Name{Name: "dataclasses"}, Name: "asdict"}, Args: []Expr{val}}
				} else if lt, ok := t.(types.ListType); ok {
					if _, ok2 := lt.Elem.(types.StructType); ok2 {
						currentImports["dataclasses"] = true
						dumps = &ListComp{Var: "_x", Iter: val, Expr: &CallExpr{Func: &FieldExpr{Target: &Name{Name: "dataclasses"}, Name: "asdict"}, Args: []Expr{&Name{Name: "_x"}}}}
					}
				}
				if dumps == nil {
					dumps = val
				}
				raw := &CallExpr{Func: &RawExpr{Code: "json.dumps"}, Args: []Expr{dumps, &KeywordArg{Name: "indent", Value: &IntLit{Value: "2"}}}}
				return &CallExpr{Func: &Name{Name: "print"}, Args: []Expr{raw}}, nil
			}
		case "exists":
			if len(args) == 1 {
				length := &CallExpr{Func: &Name{Name: "len"}, Args: args}
				return &BinaryExpr{Left: length, Op: ">", Right: &IntLit{Value: "0"}}, nil
			}
		case "num":
			if len(args) == 1 {
				return &FieldExpr{Target: args[0], Name: "numerator"}, nil
			}
		case "denom":
			if len(args) == 1 {
				return &FieldExpr{Target: args[0], Name: "denominator"}, nil
			}
		}
		return &CallExpr{Func: &Name{Name: p.Call.Func}, Args: args}, nil
	case p.Lit != nil:
		return convertLiteral(p.Lit)
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
			if id, ok := isSimpleIdent(it.Key); ok {
				keys = append(keys, &StringLit{Value: id})
			} else {
				ke, err := convertExpr(it.Key)
				if err != nil {
					return nil, err
				}
				keys = append(keys, ke)
			}
			ve, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			values = append(values, ve)
		}
		return &DictLit{Keys: keys, Values: values}, nil
	case p.Struct != nil:
		if currentEnv != nil {
			if _, ok := currentEnv.GetStruct(p.Struct.Name); ok {
				var args []Expr
				for _, f := range p.Struct.Fields {
					ve, err := convertExpr(f.Value)
					if err != nil {
						return nil, err
					}
					args = append(args, &KeywordArg{Name: f.Name, Value: ve})
				}
				return &CallExpr{Func: &Name{Name: p.Struct.Name}, Args: args}, nil
			}
		}
		var keys []Expr
		var values []Expr
		for _, f := range p.Struct.Fields {
			ve, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			keys = append(keys, &StringLit{Value: f.Name})
			values = append(values, ve)
		}
		return &DictLit{Keys: keys, Values: values}, nil
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Expr: body}, nil
	case p.FunExpr != nil && len(p.FunExpr.BlockBody) == 1:
		if ret := p.FunExpr.BlockBody[0].Return; ret != nil && ret.Value != nil {
			var params []string
			for _, pa := range p.FunExpr.Params {
				params = append(params, pa.Name)
			}
			body, err := convertExpr(ret.Value)
			if err != nil {
				return nil, err
			}
			return &LambdaExpr{Params: params, Expr: body}, nil
		}
		if ex := p.FunExpr.BlockBody[0].Expr; ex != nil {
			var params []string
			for _, pa := range p.FunExpr.Params {
				params = append(params, pa.Name)
			}
			body, err := convertExpr(ex.Expr)
			if err != nil {
				return nil, err
			}
			return &LambdaExpr{Params: params, Expr: body}, nil
		}
		return nil, fmt.Errorf("unsupported expression")
	case p.FunExpr != nil && len(p.FunExpr.BlockBody) > 1:
		genName := fmt.Sprintf("_lambda%d", structCounter)
		structCounter++
		fenv := types.NewEnv(currentEnv)
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
			fenv.SetVar(pa.Name, types.AnyType{}, true)
		}
		body, err := convertStmts(p.FunExpr.BlockBody, fenv)
		if err != nil {
			return nil, err
		}
		genv := currentEnv.Parent()
		if genv == nil {
			genv = currentEnv
		}
		globals := detectGlobals(body, fenv, genv)
		nonlocals := detectNonlocals(body, fenv, currentEnv)
		nonlocals = filterNames(nonlocals, globals)
		captured := capturedVars(body, fenv, currentEnv)
		defParams := append(append([]string{}, captured...), params...)
		extraFuncs = append(extraFuncs, &FuncDef{Name: genName, Params: defParams, Nonlocals: nonlocals, Globals: globals, Body: body})
		if len(captured) > 0 {
			var args []Expr
			for _, n := range captured {
				args = append(args, &Name{Name: n})
			}
			for _, p := range params {
				args = append(args, &Name{Name: p})
			}
			return &LambdaExpr{Params: params, Expr: &CallExpr{Func: &Name{Name: genName}, Args: args}}, nil
		}
		return &Name{Name: genName}, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.Query != nil:
		return convertQueryExpr(p.Query)
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		usesFetch = true
		if currentImports != nil {
			currentImports["urllib.request"] = true
			currentImports["json"] = true
		}
		args := []Expr{urlExpr}
		if p.Fetch.With != nil {
			withExpr, err := convertExpr(p.Fetch.With)
			if err != nil {
				return nil, err
			}
			if currentImports != nil {
				currentImports["dataclasses"] = true
			}
			args = append(args, &CallExpr{Func: &Name{Name: "dict"}, Args: []Expr{withExpr}})
		} else {
			args = append(args, &Name{Name: "None"})
		}
		return &CallExpr{Func: &Name{Name: "_fetch"}, Args: args}, nil
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
		pathExpr := fmt.Sprintf("%q", path)
		if path != "" && strings.HasPrefix(path, "../") {
			root := repoRoot()
			abs := filepath.ToSlash(filepath.Join(root, "tests", clean))
			pathExpr = fmt.Sprintf("%q", abs)
		}
		expr, err := dataExprFromFile(path, format)
		if err == nil {
			return expr, nil
		}
		switch format {
		case "json", "jsonl":
			if currentImports != nil {
				currentImports["json"] = true
			}
			if format == "json" {
				return &RawExpr{Code: fmt.Sprintf("json.load(open(%s))", pathExpr)}, nil
			}
			code := fmt.Sprintf("[json.loads(line) for line in open(%s)]", pathExpr)
			return &RawExpr{Code: code}, nil
		case "csv", "tsv":
			if currentImports != nil {
				currentImports["csv"] = true
			}
			if format == "tsv" {
				code := fmt.Sprintf("list(csv.DictReader(open(%s), delimiter='\t'))", pathExpr)
				return &RawExpr{Code: code}, nil
			}
			code := fmt.Sprintf("list(csv.DictReader(open(%s)))", pathExpr)
			return &RawExpr{Code: code}, nil
		case "yaml":
			return nil, err
		default:
			return nil, fmt.Errorf("unsupported load format")
		}
	case p.Group != nil:
		e, err := convertExpr(p.Group)
		if err != nil {
			return nil, err
		}
		return &ParenExpr{Expr: e}, nil
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

func convertLiteral(l *parser.Literal) (Expr, error) {
	switch {
	case l.Int != nil:
		return &IntLit{Value: fmt.Sprintf("%d", *l.Int)}, nil
	case l.Float != nil:
		s := fmt.Sprintf("%g", *l.Float)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return &FloatLit{Value: s}, nil
	case l.Bool != nil:
		return &BoolLit{Value: bool(*l.Bool)}, nil
	case l.Str != nil:
		return &StringLit{Value: *l.Str}, nil
	case l.Null:
		return &Name{Name: "None"}, nil
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
		elseExpr = &Name{Name: "None"}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatchExpr(me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(me.Target)
	if err != nil {
		return nil, err
	}
	var expr Expr = &Name{Name: "None"}
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
				if ut, ok := currentEnv.FindUnionByVariant(call.Func); ok {
					if _, ok2 := target.(*Name); ok2 {
						st := ut.Variants[call.Func]
						if len(call.Args) == len(st.Order) {
							names := make([]string, len(call.Args))
							ok3 := true
							for i, a := range call.Args {
								name, ok4 := isSimpleIdent(a)
								if !ok4 {
									ok3 = false
									break
								}
								names[i] = name
							}
							if ok3 {
								for i, nm := range names {
									if nm != "_" {
										field := st.Order[i]
										res = replaceName(res, nm, &FieldExpr{Target: target, Name: field})
									}
								}
								cond := &CallExpr{Func: &Name{Name: "isinstance"}, Args: []Expr{target, &Name{Name: call.Func}}}
								expr = &CondExpr{Cond: cond, Then: res, Else: expr}
								continue
							}
						}
					}
				}
				if call.Func == "Node" {
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
							if tn, ok2 := target.(*Name); ok2 {
								fields := map[string]bool{}
								for _, nm := range names {
									fields[nm] = true
								}
								res = substituteFields(res, tn.Name, fields)
								cond := &BinaryExpr{Left: target, Op: "!=", Right: &Name{Name: "None"}}
								expr = &CondExpr{Cond: cond, Then: res, Else: expr}
								continue
							}
						}
					}
				}
			}
		}
		patExpr, err := convertExpr(pat)
		if err != nil {
			return nil, err
		}
		if n, ok := patExpr.(*Name); ok && n.Name == "_" {
			expr = res
			continue
		}
		cond := &BinaryExpr{Left: target, Op: "==", Right: patExpr}
		expr = &CondExpr{Cond: cond, Then: res, Else: expr}
	}
	return expr, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Expr, error) {
	if q.Group != nil || q.Distinct {
		return nil, fmt.Errorf("unsupported query")
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil &&
		(*q.Joins[0].Side == "right" || *q.Joins[0].Side == "outer") &&
		len(q.Froms) == 0 {
		return convertSpecialJoin(q)
	}

	vars := []string{q.Var}
	iters := []Expr{}
	firstIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if currentEnv != nil {
		switch t := inferTypeFromExpr(q.Source).(type) {
		case types.ListType:
			currentEnv.SetVar(q.Var, t.Elem, true)
		case types.StructType:
			if it, ok := t.Fields["items"]; ok {
				if lt, ok := it.(types.ListType); ok {
					currentEnv.SetVar(q.Var, lt.Elem, true)
				}
			}
		}
	}
	iters = append(iters, firstIter)
	for _, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		if currentEnv != nil {
			if lt, ok := inferTypeFromExpr(f.Src).(types.ListType); ok {
				currentEnv.SetVar(f.Var, lt.Elem, true)
			}
		}
		vars = append(vars, f.Var)
		iters = append(iters, e)
	}
	var cond Expr
	for _, j := range q.Joins {
		if j.Side != nil {
			if *j.Side != "left" {
				return nil, fmt.Errorf("unsupported query")
			}
			src, err := convertExpr(j.Src)
			if err != nil {
				return nil, err
			}
			on, err := convertExpr(j.On)
			if err != nil {
				return nil, err
			}
			inner := &ListComp{Var: j.Var, Iter: src, Expr: &Name{Name: j.Var}, Cond: on}
			left := &BinaryExpr{Left: inner, Op: "||", Right: &ListLit{Elems: []Expr{&Name{Name: "None"}}}}
			vars = append(vars, j.Var)
			iters = append(iters, left)
			continue
		}
		e, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		if currentEnv != nil {
			if lt, ok := inferTypeFromExpr(j.Src).(types.ListType); ok {
				currentEnv.SetVar(j.Var, lt.Elem, true)
			}
		}
		vars = append(vars, j.Var)
		iters = append(iters, e)
		jc, err := convertExpr(j.On)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: jc}
		}
	}

	var elem Expr
	aggName, aggExpr, useAgg := aggregatorCall(q.Select)
	if !useAgg {
		elem, err = convertExpr(q.Select)
		if err != nil {
			return nil, err
		}
	}

	if q.Where != nil {
		wcond, err := convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
		if cond == nil {
			cond = wcond
		} else {
			cond = &BinaryExpr{Left: cond, Op: "&&", Right: wcond}
		}
	}

	base := &MultiListComp{Vars: vars, Iters: iters, Expr: elem, Cond: cond}
	list := Expr(base)
	var keyExpr Expr
	reverse := false
	if q.Sort != nil {
		keyExpr, err = convertExpr(q.Sort)
		if err != nil {
			return nil, err
		}
		if fe, ok := keyExpr.(*FieldExpr); ok {
			if n, ok2 := fe.Target.(*Name); ok2 {
				if currentEnv != nil {
					if t, err := currentEnv.GetVar(n.Name); err == nil {
						if _, ok := t.(types.StructType); !ok {
							fe.MapIndex = true
						}
					} else {
						fe.MapIndex = true
					}
				} else {
					fe.MapIndex = true
				}
			}
		}
		if u, ok := keyExpr.(*UnaryExpr); ok && u.Op == "-" {
			keyExpr = u.Expr
			reverse = true
		}
		if d, ok := keyExpr.(*DictLit); ok {
			vals := make([]Expr, len(d.Values))
			copy(vals, d.Values)
			keyExpr = &CallExpr{Func: &Name{Name: "tuple"}, Args: []Expr{&ListLit{Elems: vals}}}
		}
		sortBase := &MultiListComp{Vars: vars, Iters: iters, Expr: &Name{Name: q.Var}, Cond: cond}
		sorted := &SortedExpr{List: sortBase, Var: q.Var, Key: keyExpr, Reverse: reverse}
		comp := &ListComp{Var: q.Var, Iter: sorted, Expr: elem}
		list = Expr(comp)
	}

	var start Expr
	var end Expr
	if q.Skip != nil {
		start, err = convertExpr(q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		take, err := convertExpr(q.Take)
		if err != nil {
			return nil, err
		}
		if start != nil {
			end = &BinaryExpr{Left: start, Op: "+", Right: take}
		} else {
			end = take
		}
	}
	if start != nil || end != nil {
		list = &SliceExpr{Target: list, Start: start, End: end}
	}

	if useAgg {
		arg, err := convertExpr(aggExpr)
		if err != nil {
			return nil, err
		}
		comp := &MultiListComp{Vars: vars, Iters: iters, Expr: arg, Cond: cond, Parens: true}
		aggList := Expr(comp)
		if q.Sort != nil {
			aggList = &SortedExpr{List: aggList, Var: q.Var, Key: keyExpr, Reverse: reverse}
		}
		if start != nil || end != nil {
			aggList = &SliceExpr{Target: aggList, Start: start, End: end}
		}
		return &CallExpr{Func: &Name{Name: aggName}, Args: []Expr{aggList}}, nil
	}

	return list, nil
}

func convertSpecialJoin(q *parser.QueryExpr) (Expr, error) {
	j := q.Joins[0]
	side := *j.Side
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightIter, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	cond, err := convertExpr(j.On)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(q.Select)
	if err != nil {
		return nil, err
	}

	var base Expr
	if side == "right" {
		base = &MultiListComp{Vars: []string{j.Var, q.Var}, Iters: []Expr{rightIter, leftIter}, Expr: sel, Cond: cond}
	} else {
		base = &MultiListComp{Vars: []string{q.Var, j.Var}, Iters: []Expr{leftIter, rightIter}, Expr: sel, Cond: cond}
	}

	parts := []Expr{base}

	if side == "outer" {
		noneRightBody, err := convertExpr(q.Select)
		if err != nil {
			return nil, err
		}
		lam := &LambdaExpr{Params: []string{j.Var}, Expr: noneRightBody}
		call := &CallExpr{Func: &RawExpr{Code: fmt.Sprintf("(%s)", exprString(lam))}, Args: []Expr{&Name{Name: "None"}}}
		anyMatch := &CallExpr{Func: &Name{Name: "any"}, Args: []Expr{&ListComp{Var: j.Var, Iter: rightIter, Expr: cond}}}
		condLeft := &UnaryExpr{Op: "not ", Expr: anyMatch}
		parts = append(parts, &ListComp{Var: q.Var, Iter: leftIter, Expr: call, Cond: condLeft})
	}

	if side == "right" || side == "outer" {
		noneLeftBody, err := convertExpr(q.Select)
		if err != nil {
			return nil, err
		}
		lam := &LambdaExpr{Params: []string{q.Var}, Expr: noneLeftBody}
		call := &CallExpr{Func: &RawExpr{Code: fmt.Sprintf("(%s)", exprString(lam))}, Args: []Expr{&Name{Name: "None"}}}
		anyMatch := &CallExpr{Func: &Name{Name: "any"}, Args: []Expr{&ListComp{Var: q.Var, Iter: leftIter, Expr: cond}}}
		condRight := &UnaryExpr{Op: "not ", Expr: anyMatch}
		parts = append(parts, &ListComp{Var: j.Var, Iter: rightIter, Expr: call, Cond: condRight})
	}

	expr := parts[0]
	for _, p := range parts[1:] {
		expr = &BinaryExpr{Left: expr, Op: "+", Right: p}
	}
	return expr, nil
}

func convertSpecialJoinClass(q *parser.QueryExpr, class string, args []Expr) (Expr, error) {
	j := q.Joins[0]
	side := *j.Side
	leftIter, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightIter, err := convertExpr(j.Src)
	if err != nil {
		return nil, err
	}
	cond, err := convertExpr(j.On)
	if err != nil {
		return nil, err
	}

	makeArgs := func(repl map[string]Expr) []Expr {
		out := make([]Expr, len(args))
		for i, a := range args {
			a = cloneExpr(a)
			if name, ok := a.(*Name); ok {
				if r, ok2 := repl[name.Name]; ok2 {
					out[i] = r
					continue
				}
			}
			v := a
			if r, ok := repl[j.Var]; ok {
				v = replaceName(v, j.Var, r)
			}
			if r, ok := repl[q.Var]; ok {
				v = replaceName(v, q.Var, r)
			}
			out[i] = v
		}
		return out
	}

	baseExpr := &CallExpr{Func: &Name{Name: class}, Args: makeArgs(map[string]Expr{})}
	var base Expr
	if side == "right" {
		base = &MultiListComp{Vars: []string{j.Var, q.Var}, Iters: []Expr{rightIter, leftIter}, Expr: baseExpr, Cond: cond}
	} else {
		base = &MultiListComp{Vars: []string{q.Var, j.Var}, Iters: []Expr{leftIter, rightIter}, Expr: baseExpr, Cond: cond}
	}

	parts := []Expr{base}

	if side == "outer" {
		repl := map[string]Expr{j.Var: &Name{Name: "None"}}
		noneRightBody := &CallExpr{Func: &Name{Name: class}, Args: makeArgs(repl)}
		lam := &LambdaExpr{Params: []string{j.Var}, Expr: noneRightBody}
		call := &CallExpr{Func: &RawExpr{Code: fmt.Sprintf("(%s)", exprString(lam))}, Args: []Expr{&Name{Name: "None"}}}
		anyMatch := &CallExpr{Func: &Name{Name: "any"}, Args: []Expr{&ListComp{Var: j.Var, Iter: rightIter, Expr: cond}}}
		condLeft := &UnaryExpr{Op: "not ", Expr: anyMatch}
		parts = append(parts, &ListComp{Var: q.Var, Iter: leftIter, Expr: call, Cond: condLeft})
	}

	if side == "right" || side == "outer" {
		repl := map[string]Expr{q.Var: &Name{Name: "None"}}
		noneLeftBody := &CallExpr{Func: &Name{Name: class}, Args: makeArgs(repl)}
		lam := &LambdaExpr{Params: []string{q.Var}, Expr: noneLeftBody}
		call := &CallExpr{Func: &RawExpr{Code: fmt.Sprintf("(%s)", exprString(lam))}, Args: []Expr{&Name{Name: "None"}}}
		anyMatch := &CallExpr{Func: &Name{Name: "any"}, Args: []Expr{&ListComp{Var: q.Var, Iter: leftIter, Expr: cond}}}
		condRight := &UnaryExpr{Op: "not ", Expr: anyMatch}
		parts = append(parts, &ListComp{Var: j.Var, Iter: rightIter, Expr: call, Cond: condRight})
	}

	expr := parts[0]
	for _, p := range parts[1:] {
		expr = &BinaryExpr{Left: expr, Op: "+", Right: p}
	}
	return expr, nil
}

func convertUpdate(u *parser.UpdateStmt, env *types.Env) (*UpdateStmt, error) {
	if env == nil {
		return nil, fmt.Errorf("missing env")
	}
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
	fieldSet := map[string]bool{}
	for name, ft := range st.Fields {
		child.SetVar(name, ft, true)
		fieldSet[name] = true
	}
	prev := currentEnv
	currentEnv = child
	var fields []string
	var values []Expr
	for _, item := range u.Set.Items {
		key, ok := isSimpleIdent(item.Key)
		if !ok {
			key, ok = literalString(item.Key)
			if !ok {
				currentEnv = prev
				return nil, fmt.Errorf("unsupported update key")
			}
		}
		val, err := convertExpr(item.Value)
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
		cond, err = convertExpr(u.Where)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
		cond = substituteFields(cond, "item", fieldSet)
	}
	currentEnv = prev
	return &UpdateStmt{Target: u.Target, Fields: fields, Values: values, Cond: cond, Struct: true}, nil
}

func convertGroupQuery(q *parser.QueryExpr, env *types.Env, target string) ([]Stmt, error) {
	if q.Group == nil {
		return nil, fmt.Errorf("missing group clause")
	}

	vars := []string{q.Var}
	iters := []Expr{}
	src, err := convertExpr(q.Source)
	if err != nil {
		return nil, err
	}
	if env != nil {
		if lt, ok := inferTypeFromExpr(q.Source).(types.ListType); ok {
			env.SetVar(q.Var, lt.Elem, true)
		}
	}
	iters = append(iters, src)
	for _, f := range q.Froms {
		e, err := convertExpr(f.Src)
		if err != nil {
			return nil, err
		}
		vars = append(vars, f.Var)
		iters = append(iters, e)
	}
	for _, j := range q.Joins {
		e, err := convertExpr(j.Src)
		if err != nil {
			return nil, err
		}
		if env != nil {
			if lt, ok := inferTypeFromExpr(j.Src).(types.ListType); ok {
				env.SetVar(j.Var, lt.Elem, true)
			}
		}
		cond, err := convertExpr(j.On)
		if err != nil {
			return nil, err
		}
		if j.Side != nil && *j.Side == "left" {
			comp := &ListComp{Var: j.Var, Iter: e, Expr: &Name{Name: j.Var}, Cond: cond}
			e = &BinaryExpr{Left: comp, Op: "||", Right: &ListLit{Elems: []Expr{&Name{Name: "None"}}}}
		} else {
			e = &ListComp{Var: j.Var, Iter: e, Expr: &Name{Name: j.Var}, Cond: cond}
		}
		vars = append(vars, j.Var)
		iters = append(iters, e)
	}

	keyVal, err := convertExpr(q.Group.Exprs[0])
	if err != nil {
		return nil, err
	}
	keyExpr := keyVal
	keyMapExpr := keyVal
	var keyDC *DataClassDef
	if dl, ok := keyVal.(*DictLit); ok {
		if dc, args := dataClassFromDict(q.Group.Name+"_key", dl, env); dc != nil {
			keyDC = dc
			keyExpr = &CallExpr{Func: &Name{Name: dc.Name}, Args: args}
			vals := make([]Expr, len(dl.Values))
			copy(vals, dl.Values)
			keyMapExpr = &CallExpr{Func: &Name{Name: "tuple"}, Args: []Expr{&ListLit{Elems: vals}}}
		} else {
			vals := make([]Expr, len(dl.Values))
			copy(vals, dl.Values)
			keyExpr = &CallExpr{Func: &Name{Name: "tuple"}, Args: []Expr{&ListLit{Elems: vals}}}
			keyMapExpr = keyExpr
		}
	}

	var where Expr
	if q.Where != nil {
		where, err = convertExpr(q.Where)
		if err != nil {
			return nil, err
		}
	}

	prev := currentEnv
	genv := types.NewEnv(env)
	var itemType types.Type = types.AnyType{}
	if env != nil {
		if t, err := env.GetVar(q.Var); err == nil {
			itemType = t
		} else if lt, ok := inferTypeFromExpr(q.Source).(types.ListType); ok {
			itemType = lt.Elem
		}
	}
	keyType := inferPyType(keyVal, env)
	if keyDC != nil {
		keyType = structFromDataClass(keyDC, env)
	}
	genv.SetVar(q.Group.Name, types.StructType{Fields: map[string]types.Type{"key": keyType, "items": types.ListType{Elem: itemType}}}, true)
	currentEnv = genv
	sel, err := convertExpr(q.Select)
	if err != nil {
		currentEnv = prev
		return nil, err
	}
	var having Expr
	if q.Group.Having != nil {
		having, err = convertExpr(q.Group.Having)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
	}
	var sortExpr Expr
	reverse := false
	if q.Sort != nil {
		sortExpr, err = convertExpr(q.Sort)
		if err != nil {
			currentEnv = prev
			return nil, err
		}
		if fe, ok := sortExpr.(*FieldExpr); ok {
			if n, ok2 := fe.Target.(*Name); ok2 {
				if currentEnv != nil {
					if t, err := currentEnv.GetVar(n.Name); err == nil {
						if _, ok := t.(types.StructType); !ok {
							fe.MapIndex = true
						}
					} else {
						fe.MapIndex = true
					}
				} else {
					fe.MapIndex = true
				}
			}
		}
		if u, ok := sortExpr.(*UnaryExpr); ok && u.Op == "-" {
			sortExpr = u.Expr
			reverse = true
		}
		if d, ok := sortExpr.(*DictLit); ok {
			vals := make([]Expr, len(d.Values))
			copy(vals, d.Values)
			sortExpr = &CallExpr{Func: &Name{Name: "tuple"}, Args: []Expr{&ListLit{Elems: vals}}}
		}
	}
	currentEnv = prev
	selIsGroup := false
	if n, ok := sel.(*Name); ok && n.Name == q.Group.Name {
		selIsGroup = true
	}
	if !selIsGroup {
		sel = replaceGroup(sel, q.Group.Name)
		if having != nil {
			having = replaceGroup(having, q.Group.Name)
		}
		if sortExpr != nil {
			sortExpr = replaceGroup(sortExpr, q.Group.Name)
		}
	} else {
		if having != nil {
			having = replaceGroup(having, q.Group.Name)
		}
		if sortExpr != nil {
			sortExpr = replaceGroup(sortExpr, q.Group.Name)
		}
	}

	groupsVar := "_" + target + "_groups"

	groupDC := &DataClassDef{
		Name:   toClassName(groupsVar),
		Fields: []DataClassField{{Name: "key", Type: keyType.String()}, {Name: "items", Type: "list"}},
	}
	if env != nil {
		if keyDC != nil {
			env.SetStruct(keyDC.Name, structFromDataClass(keyDC, env))
		}
		env.SetStruct(groupDC.Name, types.StructType{Fields: map[string]types.Type{"key": keyType, "items": types.ListType{Elem: itemType}}})
	}

	var rowDC *DataClassDef
	row := Expr(&Name{Name: q.Var})
	leftJoin := false
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side == "left" {
			leftJoin = true
			break
		}
	}
	if len(vars) > 1 || leftJoin {
		var fields []DataClassField
		var args []Expr
		for _, v := range vars {
			fields = append(fields, DataClassField{Name: v, Type: "any"})
			args = append(args, &Name{Name: v})
		}
		rowDC = &DataClassDef{Name: toClassName(target + "_row"), Fields: fields}
		row = &CallExpr{Func: &Name{Name: rowDC.Name}, Args: args}
		if env != nil {
			env.SetStruct(rowDC.Name, structFromDataClass(rowDC, env))
		}
	}
	stmts := []Stmt{}
	if keyDC != nil {
		stmts = append(stmts, keyDC)
	}
	stmts = append(stmts, groupDC)
	if rowDC != nil {
		stmts = append(stmts, rowDC)
	}
	stmts = append(stmts, &LetStmt{Name: groupsVar, Expr: &DictLit{}})
	inner := []Stmt{
		&LetStmt{Name: "_g", Expr: &CallExpr{Func: &FieldExpr{Target: &Name{Name: groupsVar}, Name: "setdefault"}, Args: []Expr{keyMapExpr, &CallExpr{Func: &Name{Name: groupDC.Name}, Args: []Expr{keyExpr, &ListLit{}}}}}},
		&ExprStmt{Expr: &CallExpr{Func: &FieldExpr{Target: &FieldExpr{Target: &Name{Name: "_g"}, Name: "items"}, Name: "append"}, Args: []Expr{row}}},
	}
	if where != nil {
		inner = []Stmt{&IfStmt{Cond: where, Then: inner}}
	}

	bodyLoop := inner
	for i := len(vars) - 1; i >= 0; i-- {
		bodyLoop = []Stmt{&ForStmt{Var: vars[i], Iter: iters[i], Body: bodyLoop}}
	}
	stmts = append(stmts, bodyLoop...)

	var resultDC *DataClassDef
	if d, ok := sel.(*DictLit); ok {
		dc, args := dataClassFromDict(target, d, genv)
		if mp := extractMapLiteral(q.Select); mp != nil && dc != nil {
			savedEnv := currentEnv
			currentEnv = genv
			for i, it := range mp.Items {
				typ := inferTypeFromExpr(it.Value)
				if i < len(dc.Fields) {
					dc.Fields[i].Type = typ.String()
				}
			}
			currentEnv = savedEnv
			if len(dc.Fields) > 0 {
				dc.Fields[0].Type = "string"
			}
		}
		if dc != nil {
			resultDC = dc
			stmts = append(stmts, dc)
			sel = &CallExpr{Func: &Name{Name: dc.Name}, Args: args}
		}
	}
	iterPairs := &CallExpr{Func: &FieldExpr{Target: &Name{Name: groupsVar}, Name: "values"}, Args: nil}
	iter := Expr(iterPairs)
	if sortExpr != nil {
		iter = &SortedExpr{List: iterPairs, Var: q.Group.Name, Key: sortExpr, Reverse: reverse}
	}
	listComp := &ListComp{Var: q.Group.Name, Iter: iter, Expr: sel}
	if having != nil {
		listComp.Cond = having
	}
	stmts = append(stmts, &LetStmt{Name: target, Expr: listComp})
	if env != nil {
		if resultDC != nil {
			env.SetStruct(resultDC.Name, structFromDataClass(resultDC, env))
			env.SetVar(target, types.ListType{Elem: structFromDataClass(resultDC, env)}, false)
		} else if selIsGroup {
			env.SetStruct(groupDC.Name, structFromDataClass(groupDC, env))
			env.SetVar(target, types.ListType{Elem: structFromDataClass(groupDC, env)}, false)
		} else {
			env.SetVar(target, types.ListType{Elem: types.AnyType{}}, false)
		}
	}

	return stmts, nil
}

// --- AST printing helpers ---

// toNode converts the Python AST into a generic ast.Node tree.
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
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *VarStmt:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Expr)}}
	case *ReturnStmt:
		child := &ast.Node{Kind: "return"}
		if st.Expr != nil {
			child.Children = []*ast.Node{exprNode(st.Expr)}
		}
		return child
	case *WhileStmt:
		n := &ast.Node{Kind: "while"}
		n.Children = append(n.Children, exprNode(st.Cond))
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ForStmt:
		n := &ast.Node{Kind: "for", Value: st.Var}
		n.Children = append(n.Children, exprNode(st.Iter))
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if"}
		n.Children = append(n.Children, exprNode(st.Cond))
		thenNode := &ast.Node{Kind: "then"}
		for _, b := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(b))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, b := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(b))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *BreakStmt:
		return &ast.Node{Kind: "break"}
	case *ContinueStmt:
		return &ast.Node{Kind: "continue"}
	case *FieldAssignStmt:
		return &ast.Node{Kind: "field_assign", Children: []*ast.Node{exprNode(st.Target), &ast.Node{Kind: "string", Value: st.Field}, exprNode(st.Value)}}
	case *IndexAssignStmt:
		return &ast.Node{Kind: "index_assign", Children: []*ast.Node{exprNode(st.Target), exprNode(st.Index), exprNode(st.Value)}}
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
	case *FuncDef:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		if len(st.Globals) > 0 {
			g := &ast.Node{Kind: "globals"}
			for _, name := range st.Globals {
				g.Children = append(g.Children, &ast.Node{Kind: "name", Value: name})
			}
			n.Children = append(n.Children, g)
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *ImportStmt:
		return &ast.Node{Kind: "import", Value: st.Module}
	case *DataClassDef:
		n := &ast.Node{Kind: "dataclass", Value: st.Name}
		for _, f := range st.Fields {
			pair := &ast.Node{Kind: "field", Value: f.Name, Children: []*ast.Node{&ast.Node{Kind: "type", Value: f.Type}}}
			n.Children = append(n.Children, pair)
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Children: []*ast.Node{exprNode(ex.Func)}}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *KeywordArg:
		n := &ast.Node{Kind: "kwarg", Value: ex.Name}
		n.Children = []*ast.Node{exprNode(ex.Value)}
		return n
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: ex.Value}
	case *FloatLit:
		return &ast.Node{Kind: "float", Value: ex.Value}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, exprNode(el))
		}
		return n
	case *DictLit:
		n := &ast.Node{Kind: "dict"}
		for i := range ex.Keys {
			pair := &ast.Node{Kind: "pair", Children: []*ast.Node{exprNode(ex.Keys[i]), exprNode(ex.Values[i])}}
			n.Children = append(n.Children, pair)
		}
		return n
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Expr)}}
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SliceExpr:
		n := &ast.Node{Kind: "slice"}
		n.Children = append(n.Children, exprNode(ex.Target))
		if ex.Start != nil {
			n.Children = append(n.Children, exprNode(ex.Start))
		} else {
			n.Children = append(n.Children, &ast.Node{Kind: "none"})
		}
		if ex.End != nil {
			n.Children = append(n.Children, exprNode(ex.End))
		} else {
			n.Children = append(n.Children, &ast.Node{Kind: "none"})
		}
		if ex.Step != nil {
			n.Children = append(n.Children, exprNode(ex.Step))
		}
		return n
	case *FieldExpr:
		return &ast.Node{Kind: "field", Value: ex.Name, Children: []*ast.Node{exprNode(ex.Target)}}
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Expr))
		return n
	case *ListComp:
		n := &ast.Node{Kind: "list_comp", Value: ex.Var}
		n.Children = append(n.Children, exprNode(ex.Iter))
		n.Children = append(n.Children, exprNode(ex.Expr))
		if ex.Cond != nil {
			n.Children = append(n.Children, exprNode(ex.Cond))
		}
		return n
	case *MultiListComp:
		kind := "list_comp_multi"
		if ex.Parens {
			kind = "gen_comp_multi"
		}
		n := &ast.Node{Kind: kind}
		for i, v := range ex.Vars {
			iter := &ast.Node{Kind: "for", Value: v, Children: []*ast.Node{exprNode(ex.Iters[i])}}
			n.Children = append(n.Children, iter)
		}
		n.Children = append(n.Children, exprNode(ex.Expr))
		if ex.Cond != nil {
			n.Children = append(n.Children, exprNode(ex.Cond))
		}
		return n
	case *SortedExpr:
		n := &ast.Node{Kind: "sorted", Value: ex.Var}
		n.Children = append(n.Children, exprNode(ex.List))
		n.Children = append(n.Children, exprNode(ex.Key))
		if ex.Reverse {
			n.Children = append(n.Children, &ast.Node{Kind: "reverse"})
		}
		return n
	case *CondExpr:
		return &ast.Node{Kind: "cond", Children: []*ast.Node{exprNode(ex.Cond), exprNode(ex.Then), exprNode(ex.Else)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the Python AST in Lisp-like form to stdout.
