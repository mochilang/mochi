//go:build slow

package php

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
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
	"values": {}, "keys": {}, "load": {}, "save": {}, "now": {}, "input": {},
	"upper": {}, "lower": {}, "num": {}, "denom": {}, "indexOf": {}, "repeat": {}, "parseIntStr": {}, "slice": {}, "split": {}, "contains": {}, "substr": {}, "pow": {}, "getoutput": {}, "intval": {}, "floatval": {}, "int": {}, "float": {}, "to_float": {}, "ord": {}, "ctype_digit": {},
	"concat": {}, "panic": {}, "error": {}, "ceil": {}, "floor": {},
}

const helperLookupHost = `function _lookup_host($host) {
    $res = dns_get_record($host, DNS_A + DNS_AAAA);
    if ($res === false) {
        $fallback = gethostbynamel($host);
        if ($fallback === false) {
            return [[], "lookup failed"];
        }
        return [$fallback, null];
    }
    $ips = [];
    foreach ($res as $r) {
        if (isset($r['ip'])) { $ips[] = $r['ip']; }
        if (isset($r['ipv6'])) { $ips[] = $r['ipv6']; }
    }
    return [$ips, null];
}`

const helperNow = `$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}`

const helperLen = `function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}`

const helperBigRat = `function _gcd($a, $b) {
    if (function_exists('bcadd')) {
        if (bccomp($a, '0') < 0) $a = bcsub('0', $a);
        if (bccomp($b, '0') < 0) $b = bcsub('0', $b);
        while (bccomp($b, '0') != 0) {
            $t = bcmod($a, $b);
            $a = $b;
            $b = $t;
        }
        return $a;
    }
    $a = abs($a);
    $b = abs($b);
    while ($b != 0) {
        $t = $a % $b;
        $a = $b;
        $b = $t;
    }
    return $a;
}
function _bigrat($n, $d = 1) {
    if (is_array($n) && isset($n['num']) && isset($n['den']) && $d === null) {
        return $n;
    }
    if ($d === null) { $d = 1; }
    if (function_exists('bcadd')) {
        $n = (string)$n; $d = (string)$d;
        if (bccomp($d, '0') < 0) { $n = bcsub('0', $n); $d = bcsub('0', $d); }
        $g = _gcd($n, $d);
        return ['num' => bcdiv($n, $g, 0), 'den' => bcdiv($d, $g, 0)];
    }
    if ($d < 0) { $n = -$n; $d = -$d; }
    $g = _gcd($n, $d);
    return ['num' => intdiv($n, $g), 'den' => intdiv($d, $g)];
}
function _add($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcadd(bcmul(num($a), denom($b)), bcmul(num($b), denom($a)));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b) + num($b) * denom($a), denom($a) * denom($b));
}
function _sub($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcsub(bcmul(num($a), denom($b)), bcmul(num($b), denom($a)));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b) - num($b) * denom($a), denom($a) * denom($b));
}
function _mul($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcmul(num($a), num($b));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * num($b), denom($a) * denom($b));
}
function _div($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcmul(num($a), denom($b));
        $d = bcmul(denom($a), num($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b), denom($a) * num($b));
}
function num($x) {
    if (is_array($x) && array_key_exists('num', $x)) return $x['num'];
    return $x;
}
function denom($x) {
    if (is_array($x) && array_key_exists('den', $x)) return $x['den'];
    return function_exists('bcadd') ? '1' : 1;
}`

const helperStr = `function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}`

const helperIndexOf = `function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}`

const helperEnviron = `function _environ() {
    $vars = getenv();
    $list = [];
    foreach ($vars as $k => $v) { $list[] = "$k=$v"; }
    return $list;
}`

const helperRepeat = `function repeat($s, $n) {
    return str_repeat($s, intval($n));
}`

const helperParseIntStr = `function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}`

const helperAppend = `function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}`

const helperGetOutput = `function _getoutput($cmd) {
    $out = shell_exec($cmd);
    if ($out === null) return '';
    return rtrim($out);
}`

const helperIntDiv = `function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}`

const helperBigInt = `function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}`

const helperSHA256 = `function _sha256($bs) {
    $bin = '';
    foreach ($bs as $b) { $bin .= chr($b); }
    $hash = hash('sha256', $bin, true);
    return array_values(unpack('C*', $hash));
}`

const helperFetch = `function _fetch($url, $opts = null) {
    $method = 'GET';
    $headers = [];
    $body = null;
    $query = null;
    $timeout = 0;
    if ($opts !== null) {
        if (isset($opts['method'])) $method = strtoupper($opts['method']);
        if (isset($opts['headers'])) {
            foreach ($opts['headers'] as $k => $v) { $headers[] = "$k: $v"; }
        }
        if (isset($opts['body'])) $body = json_encode($opts['body']);
        if (isset($opts['query'])) $query = http_build_query($opts['query']);
        if (isset($opts['timeout'])) $timeout = intval($opts['timeout']);
    }
    if ($query !== null) {
        $url .= (strpos($url, '?') !== false ? '&' : '?') . $query;
    }
    $context = ['http' => ['method' => $method, 'header' => implode("\r\n", $headers)]];
    if ($body !== null) $context['http']['content'] = $body;
    if ($timeout > 0) $context['http']['timeout'] = $timeout / 1000.0;
    $ctx = stream_context_create($context);
    $data = file_get_contents($url, false, $ctx);
    return json_decode($data, true);
}`

const helperPanic = `function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}`

var usesLookupHost bool
var usesNow bool
var usesLen bool
var usesBigRat bool
var usesStr bool
var usesIndexOf bool
var usesRepeat bool
var usesParseIntStr bool
var usesIntDiv bool
var usesSHA256 bool
var usesEnviron bool
var usesPanic bool
var usesBigIntOps bool
var usesGetOutput bool
var usesAppend bool
var usesFetch bool
var benchMain bool
var extraStmts []Stmt

// SetBenchMain configures whether the generated main function is wrapped in a
// benchmark block when emitting code. When enabled, the program will print a
// JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

// Some PHP built-in functions cannot be redefined. When a Mochi program
// defines a function with one of these names we rename the function and all
// call sites to avoid redeclaration errors.
var phpReserved = map[string]struct{}{
	"shuffle":       {},
	"join":          {},
	"list":          {},
	"pow":           {},
	"abs":           {},
	"ord":           {},
	"chr":           {},
	"key":           {},
	"exp":           {},
	"hypot":         {},
	"floor":         {},
	"ceil":          {},
	"round":         {},
	"fmod":          {},
	"parseIntStr":   {},
	"repeat":        {},
	"rand":          {},
	"random_int":    {},
	"sqrt":          {},
	"crc32":         {},
	"xor":           {},
	"base64_encode": {},
	"base64_decode": {},
	"trim":          {},
	"copy":          {},
	"serialize":     {},
	"unserialize":   {},
	"extract":       {},
	"new":           {},
	"New":           {},
	"echo":          {},
}

// phpReservedVar lists variable names that cannot be used directly in PHP
// programs. The transpiler renames these variables to avoid runtime errors.
var phpReservedVar = map[string]struct{}{
	"this": {},
}

func sanitizeVarName(name string) string {
	if _, ok := phpReservedVar[name]; ok {
		return "mochi_" + name
	}
	return name
}

// renameMap tracks function names that were renamed due to conflicts with
// PHP built-ins. The key is the original Mochi name and the value is the name
// used in the generated PHP code.
var renameMap map[string]string
var closureNames = map[string]bool{}
var funcRefParams = map[string][]bool{}
var groupStack []string
var structStack []string
var globalNames []string
var globalSet map[string]struct{}
var globalFuncs map[string]struct{}
var importedModules = map[string]struct{}{}

func addGlobal(name string) {
	if _, ok := globalSet[name]; ok {
		return
	}
	globalSet[name] = struct{}{}
	globalNames = append(globalNames, name)
}

func addGlobalFunc(name string) {
	addGlobal(name)
	globalFuncs[name] = struct{}{}
}

// --- Simple PHP AST ---

type Program struct {
	Env   *types.Env
	Stmts []Stmt
}

func exprString(e Expr) string {
	var buf bytes.Buffer
	e.emit(&buf)
	return buf.String()
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
	name := sanitizeVarName(fr.Name)
	fmt.Fprintf(w, "for ($%s = ", name)
	fr.Start.emit(w)
	fmt.Fprintf(w, "; $%s < ", name)
	fr.End.emit(w)
	fmt.Fprintf(w, "; $%s++) {\n", name)
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
	Name   string
	Expr   Expr
	Keys   bool
	String bool
	Body   []Stmt
}

func (fe *ForEachStmt) emit(w io.Writer) {
	io.WriteString(w, "foreach (")
	if fe.Keys {
		io.WriteString(w, "array_keys(")
		fe.Expr.emit(w)
		fmt.Fprintf(w, ") as $%s) {\n", sanitizeVarName(fe.Name))
	} else {
		if fe.String || isStringExpr(fe.Expr) {
			io.WriteString(w, "str_split(")
			fe.Expr.emit(w)
			fmt.Fprintf(w, ") as $%s) {\n", sanitizeVarName(fe.Name))
		} else {
			fe.Expr.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", sanitizeVarName(fe.Name))
		}
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
	if _, ok := s.Value.(*ClosureExpr); ok {
		fmt.Fprintf(w, "$%s = null;\n", sanitizeVarName(s.Name))
		fmt.Fprintf(w, "$%s = ", sanitizeVarName(s.Name))
	} else {
		fmt.Fprintf(w, "$%s = ", sanitizeVarName(s.Name))
	}
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
	fmt.Fprintf(w, "$%s = ", sanitizeVarName(s.Name))
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
	name := sanitizeVarName(s.Name)
	fmt.Fprintf(w, "$%s = [];%s", name, "\n")
	s.Query.emitInto(w, name, 0)
}

// FuncDecl represents a simple function declaration.
type FuncDecl struct {
	Name      string
	Params    []string
	RefParams []bool
	Body      []Stmt
}

func gatherLocals(stmts []Stmt, locals map[string]struct{}) {
	for _, st := range stmts {
		switch s := st.(type) {
		case *LetStmt:
			locals[s.Name] = struct{}{}
		case *VarStmt:
			locals[s.Name] = struct{}{}
		case *IfStmt:
			gatherLocals(s.Then, locals)
			gatherLocals(s.Else, locals)
		case *WhileStmt:
			gatherLocals(s.Body, locals)
		case *ForRangeStmt:
			gatherLocals(s.Body, locals)
		case *ForEachStmt:
			gatherLocals(s.Body, locals)
		case *QueryLetStmt:
			locals[s.Name] = struct{}{}
		}
	}
}

func freeVars(body []Stmt, params []string) []string {
	locals := map[string]struct{}{}
	gatherLocals(body, locals)
	for _, p := range params {
		locals[p] = struct{}{}
	}
	seen := map[string]struct{}{}

	var rootVar func(Expr) string
	rootVar = func(e Expr) string {
		switch t := e.(type) {
		case *Var:
			return t.Name
		case *IndexExpr:
			return rootVar(t.X)
		default:
			return ""
		}
	}

	var walkExpr func(Expr)
	walkExpr = func(e Expr) {
		switch ex := e.(type) {
		case *Var:
			if _, ok := locals[ex.Name]; !ok {
				seen[ex.Name] = struct{}{}
			}
		case *IndexExpr:
			if name := rootVar(ex.X); name != "" {
				if _, ok := locals[name]; !ok {
					seen[name] = struct{}{}
				}
			}
			walkExpr(ex.X)
			walkExpr(ex.Index)
		case *SliceExpr:
			walkExpr(ex.X)
			if ex.Start != nil {
				walkExpr(ex.Start)
			}
			if ex.End != nil {
				walkExpr(ex.End)
			}
		case *UnaryExpr:
			walkExpr(ex.X)
		case *BinaryExpr:
			walkExpr(ex.Left)
			walkExpr(ex.Right)
		case *CondExpr:
			walkExpr(ex.Cond)
			walkExpr(ex.Then)
			walkExpr(ex.Else)
		case *MatchExpr:
			walkExpr(ex.Target)
			for _, a := range ex.Arms {
				walkExpr(a.Pattern)
				walkExpr(a.Result)
			}
		case *UnionMatchExpr:
			walkExpr(ex.Target)
			for _, c := range ex.Cases {
				walkExpr(c.Result)
			}
		case *QueryExpr:
			if ex.Select != nil {
				walkExpr(ex.Select)
			}
			if ex.Where != nil {
				walkExpr(ex.Where)
			}
		case *GroupByExpr:
			if ex.Key != nil {
				walkExpr(ex.Key)
			}
			if ex.Where != nil {
				walkExpr(ex.Where)
			}
			if ex.Select != nil {
				walkExpr(ex.Select)
			}
			if ex.Having != nil {
				walkExpr(ex.Having)
			}
		case *RightJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *LeftJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *OuterJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *SumExpr:
			walkExpr(ex.List)
		case *SubstringExpr:
			walkExpr(ex.Str)
			walkExpr(ex.Start)
			if ex.End != nil {
				walkExpr(ex.End)
			}
		case *CallExpr:
			if _, builtin := builtinNames[ex.Func]; !builtin {
				name := ex.Func
				if strings.HasPrefix(name, "$") {
					name = name[1:]
					if idx := strings.Index(name, "["); idx >= 0 {
						name = name[:idx]
					}
				}
				if _, ok := locals[name]; !ok {
					seen[name] = struct{}{}
				}
			}
			for _, a := range ex.Args {
				walkExpr(a)
			}
		}
	}

	var walkStmt func(Stmt)
	walkStmt = func(s Stmt) {
		switch st := s.(type) {
		case *IndexAssignStmt:
			if name := rootVar(st.Target); name != "" {
				if _, ok := locals[name]; !ok {
					seen[name] = struct{}{}
				}
			}
			walkExpr(st.Value)
		case *ExprStmt:
			walkExpr(st.Expr)
		case *LetStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *VarStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *IfStmt:
			for _, t := range st.Then {
				walkStmt(t)
			}
			for _, e := range st.Else {
				walkStmt(e)
			}
		case *WhileStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForRangeStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForEachStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *QueryLetStmt:
			if st.Query != nil {
				walkExpr(st.Query.Select)
				walkExpr(st.Query.Where)
			}
		case *ReturnStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *SaveStmt:
			walkExpr(st.Src)
		case *UpdateStmt:
			for _, v := range st.Values {
				walkExpr(v)
			}
			if st.Cond != nil {
				walkExpr(st.Cond)
			}
		}
	}

	for _, st := range body {
		walkStmt(st)
	}

	vars := make([]string, 0, len(seen))
	for n := range seen {
		vars = append(vars, n)
	}
	sort.Strings(vars)
	return vars
}

func mutatedVars(body []Stmt) map[string]struct{} {
	mutated := map[string]struct{}{}

	var rootVar func(Expr) string
	rootVar = func(e Expr) string {
		switch t := e.(type) {
		case *Var:
			return t.Name
		case *IndexExpr:
			return rootVar(t.X)
		default:
			return ""
		}
	}

	var walkExpr func(Expr)
	walkExpr = func(e Expr) {
		switch ex := e.(type) {
		case *CallExpr:
			for _, a := range ex.Args {
				walkExpr(a)
			}
		case *IndexExpr:
			walkExpr(ex.X)
			walkExpr(ex.Index)
		case *SliceExpr:
			walkExpr(ex.X)
			if ex.Start != nil {
				walkExpr(ex.Start)
			}
			if ex.End != nil {
				walkExpr(ex.End)
			}
		case *UnaryExpr:
			walkExpr(ex.X)
		case *BinaryExpr:
			walkExpr(ex.Left)
			walkExpr(ex.Right)
		case *CondExpr:
			walkExpr(ex.Cond)
			walkExpr(ex.Then)
			walkExpr(ex.Else)
		case *MatchExpr:
			walkExpr(ex.Target)
			for _, a := range ex.Arms {
				walkExpr(a.Pattern)
				walkExpr(a.Result)
			}
		case *UnionMatchExpr:
			walkExpr(ex.Target)
			for _, c := range ex.Cases {
				walkExpr(c.Result)
			}
		case *QueryExpr:
			if ex.Select != nil {
				walkExpr(ex.Select)
			}
			if ex.Where != nil {
				walkExpr(ex.Where)
			}
		case *GroupByExpr:
			if ex.Key != nil {
				walkExpr(ex.Key)
			}
			if ex.Where != nil {
				walkExpr(ex.Where)
			}
			if ex.Select != nil {
				walkExpr(ex.Select)
			}
			if ex.Having != nil {
				walkExpr(ex.Having)
			}
		case *RightJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *LeftJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *OuterJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *SumExpr:
			walkExpr(ex.List)
		case *SubstringExpr:
			walkExpr(ex.Str)
			walkExpr(ex.Start)
			if ex.End != nil {
				walkExpr(ex.End)
			}
		}
	}

	var walkStmt func(Stmt)
	walkStmt = func(s Stmt) {
		switch st := s.(type) {
		case *AssignStmt:
			mutated[st.Name] = struct{}{}
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *IndexAssignStmt:
			if name := rootVar(st.Target); name != "" {
				mutated[name] = struct{}{}
			}
			walkExpr(st.Value)
		case *ExprStmt:
			walkExpr(st.Expr)
		case *LetStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *VarStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *IfStmt:
			for _, t := range st.Then {
				walkStmt(t)
			}
			for _, e := range st.Else {
				walkStmt(e)
			}
		case *WhileStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForRangeStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForEachStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *QueryLetStmt:
			if st.Query != nil {
				walkExpr(st.Query.Select)
				walkExpr(st.Query.Where)
			}
		case *ReturnStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *SaveStmt:
			walkExpr(st.Src)
		case *UpdateStmt:
			for _, v := range st.Values {
				walkExpr(v)
			}
			if st.Cond != nil {
				walkExpr(st.Cond)
			}
		}
	}

	for _, st := range body {
		walkStmt(st)
	}
	return mutated
}

func markRefParams(body []Stmt, params []string) []bool {
	idx := map[string]int{}
	for i, p := range params {
		idx[p] = i
	}
	ref := make([]bool, len(params))

	var rootVar func(Expr) string
	rootVar = func(e Expr) string {
		switch t := e.(type) {
		case *Var:
			return t.Name
		case *IndexExpr:
			return rootVar(t.X)
		default:
			return ""
		}
	}

	ret := map[string]struct{}{}

	var collect func(Stmt)
	collect = func(s Stmt) {
		switch st := s.(type) {
		case *ReturnStmt:
			if v, ok := st.Value.(*Var); ok {
				ret[v.Name] = struct{}{}
			}
		case *IfStmt:
			for _, t := range st.Then {
				collect(t)
			}
			for _, e := range st.Else {
				collect(e)
			}
		case *WhileStmt:
			for _, b := range st.Body {
				collect(b)
			}
		case *ForRangeStmt:
			for _, b := range st.Body {
				collect(b)
			}
		case *ForEachStmt:
			for _, b := range st.Body {
				collect(b)
			}
		}
	}
	for _, st := range body {
		collect(st)
	}

	var walkExpr func(Expr)
	walkExpr = func(e Expr) {
		switch ex := e.(type) {
		case *CallExpr:
			if flags, ok := funcRefParams[ex.Func]; ok {
				for iArg, a := range ex.Args {
					if iArg < len(flags) && flags[iArg] {
						if name := rootVar(a); name != "" {
							if idxPos, ok := idx[name]; ok {
								if _, ok := ret[name]; !ok {
									ref[idxPos] = true
								}
							}
						}
					}
					walkExpr(a)
				}
			} else {
				for _, a := range ex.Args {
					walkExpr(a)
				}
			}
		case *IndexExpr:
			walkExpr(ex.X)
			walkExpr(ex.Index)
		case *SliceExpr:
			walkExpr(ex.X)
			if ex.Start != nil {
				walkExpr(ex.Start)
			}
			if ex.End != nil {
				walkExpr(ex.End)
			}
		case *UnaryExpr:
			walkExpr(ex.X)
		case *BinaryExpr:
			walkExpr(ex.Left)
			walkExpr(ex.Right)
		case *CondExpr:
			walkExpr(ex.Cond)
			walkExpr(ex.Then)
			walkExpr(ex.Else)
		case *MatchExpr:
			walkExpr(ex.Target)
			for _, a := range ex.Arms {
				walkExpr(a.Pattern)
				walkExpr(a.Result)
			}
		case *UnionMatchExpr:
			walkExpr(ex.Target)
			for _, c := range ex.Cases {
				walkExpr(c.Result)
			}
		case *QueryExpr:
			if ex.Select != nil {
				walkExpr(ex.Select)
			}
			if ex.Where != nil {
				walkExpr(ex.Where)
			}
		case *GroupByExpr:
			if ex.Key != nil {
				walkExpr(ex.Key)
			}
			if ex.Where != nil {
				walkExpr(ex.Where)
			}
			if ex.Select != nil {
				walkExpr(ex.Select)
			}
			if ex.Having != nil {
				walkExpr(ex.Having)
			}
		case *RightJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *LeftJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *OuterJoinExpr:
			walkExpr(ex.Select)
			walkExpr(ex.Cond)
		case *SumExpr:
			walkExpr(ex.List)
		case *SubstringExpr:
			walkExpr(ex.Str)
			walkExpr(ex.Start)
			if ex.End != nil {
				walkExpr(ex.End)
			}
		case *IndexAssignStmt:
			// handled in walkStmt
		}
	}

	var walkStmt func(Stmt)
	walkStmt = func(s Stmt) {
		switch st := s.(type) {
		case *IndexAssignStmt:
			if name := rootVar(st.Target); name != "" {
				if i, ok := idx[name]; ok {
					if _, ok := ret[name]; !ok {
						ref[i] = true
					}
				}
			}
			walkExpr(st.Value)
		case *ExprStmt:
			walkExpr(st.Expr)
		case *LetStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *VarStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *IfStmt:
			for _, t := range st.Then {
				walkStmt(t)
			}
			for _, e := range st.Else {
				walkStmt(e)
			}
		case *WhileStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForRangeStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *ForEachStmt:
			for _, b := range st.Body {
				walkStmt(b)
			}
		case *QueryLetStmt:
			if st.Query != nil {
				walkExpr(st.Query.Select)
				walkExpr(st.Query.Where)
			}
		case *ReturnStmt:
			if st.Value != nil {
				walkExpr(st.Value)
			}
		case *SaveStmt:
			walkExpr(st.Src)
		case *UpdateStmt:
			for _, v := range st.Values {
				walkExpr(v)
			}
			if st.Cond != nil {
				walkExpr(st.Cond)
			}
		}
	}

	for _, st := range body {
		walkStmt(st)
	}

	return ref
}

func (f *FuncDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "function %s(", f.Name)
	for i, p := range f.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if i < len(f.RefParams) && f.RefParams[i] {
			fmt.Fprint(w, "&")
		}
		fmt.Fprintf(w, "$%s", sanitizeVarName(p))
	}
	fmt.Fprint(w, ") {\n")
	if len(globalNames) > 0 {
		locals := map[string]struct{}{}
		gatherLocals(f.Body, locals)
		var vars []string
		for _, g := range globalNames {
			if g == f.Name {
				continue
			}
			skip := false
			for _, p := range f.Params {
				if g == p {
					skip = true
					break
				}
			}
			if _, ok := locals[g]; ok {
				skip = true
			}
			if _, ok := globalFuncs[g]; ok {
				skip = true
			}
			if skip {
				continue
			}
			vars = append(vars, "$"+sanitizeVarName(g))
		}
		if len(vars) > 0 {
			fmt.Fprintf(w, "  global %s;\n", strings.Join(vars, ", "))
		}
	}
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
	Params    []string
	RefParams []bool
	Uses      []string
	Body      []Stmt
}

func (c *ClosureExpr) emit(w io.Writer) {
	fmt.Fprint(w, "function(")
	for i, p := range c.Params {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		if i < len(c.RefParams) && c.RefParams[i] {
			fmt.Fprint(w, "&")
		}
		fmt.Fprintf(w, "$%s", sanitizeVarName(p))
	}
	fmt.Fprint(w, ")")
	if len(c.Uses) > 0 {
		fmt.Fprint(w, " use (")
		for i, u := range c.Uses {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			if strings.HasPrefix(u, "&") {
				fmt.Fprintf(w, "&$%s", sanitizeVarName(u[1:]))
			} else {
				fmt.Fprintf(w, "$%s", sanitizeVarName(u))
			}
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
	fmt.Fprintf(w, "$%s = ", sanitizeVarName(s.Name))
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

// BenchStmt measures execution time and memory usage of a block.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (b *BenchStmt) emit(w io.Writer) {
	// Capture baseline and peak memory to account for allocations inside
	// the benchmarked block. `memory_get_peak_usage()` reports the highest
	// amount of memory used by the script, so subtracting the starting
	// usage approximates memory consumed by this block.
	io.WriteString(w, "$__start_mem = memory_get_usage();\n")
	io.WriteString(w, "$__start = _now();\n")
	for _, st := range b.Body {
		io.WriteString(w, "  ")
		st.emit(w)
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForEachStmt:
			io.WriteString(w, "\n")
		default:
			io.WriteString(w, ";\n")
		}
	}
	io.WriteString(w, "$__end = _now();\n")
	io.WriteString(w, "$__end_mem = memory_get_peak_usage();\n")
	io.WriteString(w, "$__duration = max(1, intdiv($__end - $__start, 1000));\n")
	io.WriteString(w, "$__mem_diff = max(0, $__end_mem - $__start_mem);\n")
	fmt.Fprintf(w, "$__bench = [\"duration_us\" => $__duration, \"memory_bytes\" => $__mem_diff, \"name\" => %q];\n", b.Name)
	io.WriteString(w, "$__j = json_encode($__bench, 128);\n")
	io.WriteString(w, "$__j = str_replace(\"    \", \"  \", $__j);\n")
	io.WriteString(w, "echo $__j, PHP_EOL;")
}

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
	fmt.Fprintf(w, "  $%s[$idx] = $item;\n", sanitizeVarName(u.Target))
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

// NullLit represents the `null` literal.
type NullLit struct{}

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
	End   Expr // nil for open-ended
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
	End   Expr // nil for open-ended
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

type UnionMatchCase struct {
	Tag    string
	Vars   []string
	Fields []string
	Result Expr
}

type UnionMatchExpr struct {
	Target  Expr
	Cases   []UnionMatchCase
	Default Expr
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
			fmt.Fprintf(w, "%s$%s[] = ", ind, sanitizeVarName(res))
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
			fmt.Fprintf(w, " as $%s) {\n", sanitizeVarName(lp.Name))
			emitLoops(idx+1, lvl+1)
			fmt.Fprintf(w, "%s}\n", ind)
		case LeftJoinLoop:
			fmt.Fprintf(w, "%s$matched = false;\n", ind)
			fmt.Fprintf(w, "%sforeach (", ind)
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", sanitizeVarName(lp.Name))
			fmt.Fprintf(w, "%s  if (!(", ind)
			lp.Cond.emit(w)
			fmt.Fprint(w, ")) continue;\n")
			fmt.Fprintf(w, "%s  $matched = true;\n", ind)
			emitLoops(idx+1, lvl+1)
			fmt.Fprintf(w, "%s}\n", ind)
			fmt.Fprintf(w, "%sif (!$matched) {\n", ind)
			fmt.Fprintf(w, "%s  $%s = null;\n", ind, sanitizeVarName(lp.Name))
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

func (u *UnionMatchExpr) emit(w io.Writer) {
	io.WriteString(w, "(function($__v) {\n")
	for i, c := range u.Cases {
		if i == 0 {
			fmt.Fprintf(w, "  if ($__v['__tag'] === %q) {\n", c.Tag)
		} else {
			fmt.Fprintf(w, "  } elseif ($__v['__tag'] === %q) {\n", c.Tag)
		}
		for j, vname := range c.Vars {
			if vname == "_" || c.Fields == nil || j >= len(c.Fields) {
				continue
			}
			fmt.Fprintf(w, "    $%s = $__v[%q];\n", sanitizeVarName(vname), c.Fields[j])
		}
		io.WriteString(w, "    return ")
		c.Result.emit(w)
		io.WriteString(w, ";\n")
	}
	if len(u.Cases) > 0 {
		if u.Default != nil {
			io.WriteString(w, "  } else {\n")
			io.WriteString(w, "    return ")
			u.Default.emit(w)
			io.WriteString(w, ";\n")
		}
		io.WriteString(w, "  }\n")
	} else if u.Default != nil {
		io.WriteString(w, "  return ")
		u.Default.emit(w)
		io.WriteString(w, ";\n")
	}
	io.WriteString(w, "})(")
	u.Target.emit(w)
	io.WriteString(w, ")")
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
			fmt.Fprintf(w, "$%s", sanitizeVarName(u))
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
			fmt.Fprintf(w, " as $%s) {\n", sanitizeVarName(lp.Name))
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
		case LeftJoinLoop:
			io.WriteString(w, indent)
			io.WriteString(w, "$matched = false;\n")
			io.WriteString(w, indent)
			io.WriteString(w, "foreach (")
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", sanitizeVarName(lp.Name))
			io.WriteString(w, indent+"  if (!(")
			lp.Cond.emit(w)
			io.WriteString(w, ")) continue;\n")
			io.WriteString(w, indent+"  $matched = true;\n")
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
			io.WriteString(w, indent)
			io.WriteString(w, "if (!$matched) {\n")
			fmt.Fprintf(w, indent+"  $%s = null;\n", sanitizeVarName(lp.Name))
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
			fmt.Fprintf(w, "$%s", sanitizeVarName(u))
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
					fmt.Fprintf(w, "$%s;\n", sanitizeVarName(ql.Name))
				} else if ll, ok := g.Loops[0].(LeftJoinLoop); ok {
					fmt.Fprintf(w, "$%s;\n", sanitizeVarName(ll.Name))
				}
			} else {
				io.WriteString(w, "[")
				for i, lp := range g.Loops {
					if i > 0 {
						io.WriteString(w, ", ")
					}
					switch l := lp.(type) {
					case QueryLoop:
						fmt.Fprintf(w, "'%s' => $%s", l.Name, sanitizeVarName(l.Name))
					case LeftJoinLoop:
						fmt.Fprintf(w, "'%s' => $%s", l.Name, sanitizeVarName(l.Name))
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
			fmt.Fprintf(w, " as $%s) {\n", sanitizeVarName(lp.Name))
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
		case LeftJoinLoop:
			io.WriteString(w, indent)
			io.WriteString(w, "$matched = false;\n")
			io.WriteString(w, indent)
			io.WriteString(w, "foreach (")
			lp.Source.emit(w)
			fmt.Fprintf(w, " as $%s) {\n", sanitizeVarName(lp.Name))
			io.WriteString(w, indent+"  if (!(")
			lp.Cond.emit(w)
			io.WriteString(w, ")) continue;\n")
			io.WriteString(w, indent+"  $matched = true;\n")
			emitLoops(idx+1, level+1)
			io.WriteString(w, indent)
			io.WriteString(w, "}\n")
			io.WriteString(w, indent)
			io.WriteString(w, "if (!$matched) {\n")
			fmt.Fprintf(w, indent+"  $%s = null;\n", sanitizeVarName(lp.Name))
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
			fmt.Fprintf(w, "$%s", sanitizeVarName(u))
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
			fmt.Fprintf(w, "$%s", sanitizeVarName(u))
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
			fmt.Fprintf(w, "$%s", sanitizeVarName(u))
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

func (s *StringLit) emit(w io.Writer) {
	// Use single quoted strings to avoid PHP variable interpolation.
	// Escape backslashes and single quotes.
	esc := strings.ReplaceAll(s.Value, "\\", "\\\\")
	esc = strings.ReplaceAll(esc, "'", "\\'")
	// Preserve actual tabs and newlines in strings so Rosetta outputs match.
	io.WriteString(w, "'"+esc+"'")
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "+" {
		lp := precedence(b.Op)
		if isStringExpr(b.Left) || isStringExpr(b.Right) {
			if lb, ok := b.Left.(*BinaryExpr); ok && precedence(lb.Op) > lp {
				io.WriteString(w, "(")
				lb.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			fmt.Fprint(w, " . ")
			if rb, ok := b.Right.(*BinaryExpr); ok && precedence(rb.Op) >= lp {
				io.WriteString(w, "(")
				rb.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
		} else {
			if lb, ok := b.Left.(*BinaryExpr); ok && precedence(lb.Op) > lp {
				io.WriteString(w, "(")
				lb.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			fmt.Fprint(w, " + ")
			if rb, ok := b.Right.(*BinaryExpr); ok && precedence(rb.Op) >= lp {
				io.WriteString(w, "(")
				rb.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
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
		// Fallback for unknown types: check array key existence
		fmt.Fprint(w, "isset(")
		b.Right.emit(w)
		fmt.Fprint(w, "[")
		b.Left.emit(w)
		fmt.Fprint(w, "])")
		return
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
	lp := precedence(b.Op)
	if lb, ok := b.Left.(*BinaryExpr); ok && precedence(lb.Op) > lp {
		io.WriteString(w, "(")
		lb.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Left.emit(w)
	}
	fmt.Fprintf(w, " %s ", b.Op)
	if rb, ok := b.Right.(*BinaryExpr); ok && precedence(rb.Op) >= lp {
		io.WriteString(w, "(")
		rb.emit(w)
		io.WriteString(w, ")")
	} else {
		b.Right.emit(w)
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

func (g *GroupExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	g.X.emit(w)
	io.WriteString(w, ")")
}

func (d *IntDivExpr) emit(w io.Writer) {
	usesIntDiv = true
	io.WriteString(w, "_intdiv(")
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
			fmt.Fprintf(w, "$%s", sanitizeVarName(u))
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
	if s.End != nil {
		fmt.Fprint(w, ", ")
		if lit, ok := s.Start.(*IntLit); ok && lit.Value == 0 {
			s.End.emit(w)
		} else {
			(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
		}
	}
	fmt.Fprint(w, ")")
}

func (i *IndexExpr) emit(w io.Writer) {
	i.X.emit(w)
	fmt.Fprint(w, "[")
	i.Index.emit(w)
	fmt.Fprint(w, "]")
}

func (s *SliceExpr) emit(w io.Writer) {
	if isStringExpr(s.X) {
		fmt.Fprint(w, "substr(")
		s.X.emit(w)
		fmt.Fprint(w, ", ")
		s.Start.emit(w)
		if s.End != nil {
			fmt.Fprint(w, ", ")
			(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
		}
		fmt.Fprint(w, ")")
		return
	} else if s.End != nil {
		if be, ok := s.End.(*BinaryExpr); ok && be.Op == "+" {
			if rv, ok2 := be.Right.(*IntLit); ok2 && rv.Value == 1 {
				if lv, ok3 := be.Left.(*Var); ok3 {
					if sv, ok4 := s.Start.(*Var); ok4 && sv.Name == lv.Name {
						s.X.emit(w)
						fmt.Fprint(w, "[")
						s.Start.emit(w)
						fmt.Fprint(w, "]")
						return
					}
				}
			}
		}
	}
	fmt.Fprint(w, "array_slice(")
	s.X.emit(w)
	fmt.Fprint(w, ", ")
	s.Start.emit(w)
	if s.End != nil {
		fmt.Fprint(w, ", ")
		if lit, ok := s.Start.(*IntLit); ok && lit.Value == 0 {
			s.End.emit(w)
		} else {
			(&BinaryExpr{Left: s.End, Op: "-", Right: s.Start}).emit(w)
		}
	}
	fmt.Fprint(w, ")")
}

func (v *Var) emit(w io.Writer) {
	if v.Name == "nil" {
		io.WriteString(w, "null")
		return
	}
	fmt.Fprintf(w, "$%s", sanitizeVarName(v.Name))
}

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

func (n *NullLit) emit(w io.Writer) { io.WriteString(w, "null") }

// Emit writes formatted PHP source to w.
func Emit(w io.Writer, p *Program) error {
	transpileEnv = p.Env
	defer func() { transpileEnv = nil }()
	if _, err := io.WriteString(w, "<?php\nini_set('memory_limit', '-1');\n"); err != nil {
		return err
	}
	if usesLookupHost {
		if _, err := io.WriteString(w, helperLookupHost+"\n"); err != nil {
			return err
		}
	}
	if usesNow {
		if _, err := io.WriteString(w, helperNow+"\n"); err != nil {
			return err
		}
	}
	if usesLen {
		if _, err := io.WriteString(w, helperLen+"\n"); err != nil {
			return err
		}
	}
	if usesBigRat {
		if _, err := io.WriteString(w, helperBigRat+"\n"); err != nil {
			return err
		}
	}
	if usesStr {
		if _, err := io.WriteString(w, helperStr+"\n"); err != nil {
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
	if usesParseIntStr {
		if _, err := io.WriteString(w, helperParseIntStr+"\n"); err != nil {
			return err
		}
	}
	if usesAppend {
		if _, err := io.WriteString(w, helperAppend+"\n"); err != nil {
			return err
		}
	}
	if usesIntDiv {
		if _, err := io.WriteString(w, helperIntDiv+"\n"); err != nil {
			return err
		}
	}
	if usesBigIntOps {
		if _, err := io.WriteString(w, helperBigInt+"\n"); err != nil {
			return err
		}
	}
	if usesSHA256 {
		if _, err := io.WriteString(w, helperSHA256+"\n"); err != nil {
			return err
		}
	}
	if usesEnviron {
		if _, err := io.WriteString(w, helperEnviron+"\n"); err != nil {
			return err
		}
	}
	if usesPanic {
		if _, err := io.WriteString(w, helperPanic+"\n"); err != nil {
			return err
		}
	}
	if usesGetOutput {
		if _, err := io.WriteString(w, helperGetOutput+"\n"); err != nil {
			return err
		}
	}
	if usesFetch {
		if _, err := io.WriteString(w, helperFetch+"\n"); err != nil {
			return err
		}
	}
	hasMain := false
	mainCalled := false
	stmts := p.Stmts
	var bench *BenchStmt
	if len(p.Stmts) == 1 {
		if b, ok := p.Stmts[0].(*BenchStmt); ok {
			bench = b
			stmts = b.Body
		}
	}
	for _, s := range stmts {
		if fd, ok := s.(*FuncDecl); ok && fd.Name == "main" {
			hasMain = true
		} else if es, ok := s.(*ExprStmt); ok {
			if call, ok := es.Expr.(*CallExpr); ok {
				if call.Func == "main" && len(call.Args) == 0 {
					mainCalled = true
				}
			}
		}
	}
	if bench != nil && hasMain && !mainCalled {
		bench.Body = append(bench.Body, &ExprStmt{Expr: &CallExpr{Func: "main"}})
		mainCalled = true
	}
	for _, s := range p.Stmts {
		s.emit(w)
		switch s.(type) {
		case *IfStmt, *FuncDecl, *WhileStmt, *ForRangeStmt, *ForEachStmt, *QueryLetStmt, *BenchStmt:
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	if bench == nil && hasMain && !mainCalled {
		if _, err := io.WriteString(w, "main();\n"); err != nil {
			return err
		}
	}
	return nil
}

// Transpile converts a Mochi program into our PHP AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	transpileEnv = env
	globalNames = nil
	globalSet = map[string]struct{}{}
	globalFuncs = map[string]struct{}{}
	closureNames = map[string]bool{}
	renameMap = map[string]string{}
	usesLookupHost = false
	usesNow = false
	usesLen = false
	usesBigRat = false
	usesStr = false
	usesIndexOf = false
	usesRepeat = false
	usesParseIntStr = false
	usesIntDiv = false
	usesSHA256 = false
	usesEnviron = false
	usesPanic = false
	usesBigIntOps = false
	usesGetOutput = false
	usesAppend = false
	usesFetch = false
	defer func() { transpileEnv = nil }()
	p := &Program{Env: env}
	extraStmts = nil
	for _, st := range prog.Statements {
		if st.Fun != nil {
			name := st.Fun.Name
			if _, reserved := phpReserved[name]; reserved {
				newName := "mochi_" + name
				renameMap[name] = newName
				name = newName
			}
			addGlobalFunc(name)
		}
	}
	for _, st := range prog.Statements {
		conv, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if conv != nil {
			p.Stmts = append(p.Stmts, conv)
		}
	}
	if len(extraStmts) > 0 {
		p.Stmts = append(extraStmts, p.Stmts...)
	}
	if benchMain {
		usesNow = true
		p.Stmts = []Stmt{&BenchStmt{Name: "main", Body: p.Stmts}}
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
	strFlags := []bool{}

	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, first)
	if transpileEnv != nil {
		t := types.TypeOfUnary(b.Left, transpileEnv)
		_, ok := t.(types.StringType)
		strFlags = append(strFlags, ok)
	} else {
		strFlags = append(strFlags, false)
	}
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
		if transpileEnv != nil {
			t := types.TypeOfPostfix(p.Right, transpileEnv)
			_, ok := t.(types.StringType)
			strFlags = append(strFlags, ok)
		} else {
			strFlags = append(strFlags, false)
		}
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

	apply := func(left Expr, ls bool, op string, right Expr, rs bool) (Expr, bool) {
		switch op {
		case "/":
			if isIntExpr(left) && isIntExpr(right) {
				usesIntDiv = true
				return &IntDivExpr{Left: left, Right: right}, false
			}
			if isBigRatExpr(left) || isBigRatExpr(right) {
				usesBigRat = true
				return &CallExpr{Func: "_div", Args: []Expr{left, right}}, false
			}
			if isBigIntExpr(left) || isBigIntExpr(right) {
				usesBigIntOps = true
				usesIntDiv = true
				return &CallExpr{Func: "_idiv", Args: []Expr{left, right}}, false
			}
			return &BinaryExpr{Left: left, Op: "/", Right: right}, false
		case "in":
			if isListExpr(right) {
				return &CallExpr{Func: "in_array", Args: []Expr{left, right}}, false
			}
			if isMapExpr(right) {
				return &CallExpr{Func: "array_key_exists", Args: []Expr{left, right}}, false
			}
			if isStringExpr(right) {
				cmp := &CallExpr{Func: "strpos", Args: []Expr{right, left}}
				return &BinaryExpr{Left: cmp, Op: "!==", Right: &BoolLit{Value: false}}, false
			}
		case "+":
			if ls || rs {
				return &BinaryExpr{Left: left, Op: ".", Right: right}, true
			}
			if isBigRatExpr(left) || isBigRatExpr(right) {
				usesBigRat = true
				return &CallExpr{Func: "_add", Args: []Expr{left, right}}, false
			}
			if isBigIntExpr(left) || isBigIntExpr(right) {
				usesBigIntOps = true
				return &CallExpr{Func: "_iadd", Args: []Expr{left, right}}, false
			}
			if isListExpr(left) && isListExpr(right) {
				return &CallExpr{Func: "array_merge", Args: []Expr{left, right}}, false
			}
			if isListExpr(left) && isStringExpr(right) {
				joined := &CallExpr{Func: "implode", Args: []Expr{&StringLit{Value: ""}, left}}
				return &BinaryExpr{Left: joined, Op: ".", Right: right}, true
			}
			if isStringExpr(left) && isListExpr(right) {
				joined := &CallExpr{Func: "implode", Args: []Expr{&StringLit{Value: ""}, right}}
				return &BinaryExpr{Left: left, Op: ".", Right: joined}, true
			}
			return &BinaryExpr{Left: left, Op: "+", Right: right}, false
		case "-":
			if isBigRatExpr(left) || isBigRatExpr(right) {
				usesBigRat = true
				return &CallExpr{Func: "_sub", Args: []Expr{left, right}}, false
			}
			if isBigIntExpr(left) || isBigIntExpr(right) {
				usesBigIntOps = true
				return &CallExpr{Func: "_isub", Args: []Expr{left, right}}, false
			}
			return &BinaryExpr{Left: left, Op: "-", Right: right}, false
		case "*":
			if isBigRatExpr(left) || isBigRatExpr(right) {
				usesBigRat = true
				return &CallExpr{Func: "_mul", Args: []Expr{left, right}}, false
			}
			if isBigIntExpr(left) || isBigIntExpr(right) {
				usesBigIntOps = true
				return &CallExpr{Func: "_imul", Args: []Expr{left, right}}, false
			}
			return &BinaryExpr{Left: left, Op: "*", Right: right}, false
		case "%":
			if isBigIntExpr(left) || isBigIntExpr(right) {
				usesBigIntOps = true
				return &CallExpr{Func: "_imod", Args: []Expr{left, right}}, false
			}
			if !isIntExpr(left) || !isIntExpr(right) {
				return &CallExpr{Func: "fmod", Args: []Expr{left, right}}, false
			}
			return &BinaryExpr{Left: left, Op: "%", Right: right}, false
		}
		return &BinaryExpr{Left: left, Op: op, Right: right}, false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					expr, sf := apply(operands[i], strFlags[i], ops[i], operands[i+1], strFlags[i+1])
					operands[i] = expr
					strFlags[i] = sf
					operands = append(operands[:i+1], operands[i+2:]...)
					strFlags = append(strFlags[:i+1], strFlags[i+2:]...)
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
		case "-":
			if isBigRatExpr(x) {
				usesBigRat = true
				x = &CallExpr{Func: "_mul", Args: []Expr{x, &IntLit{Value: -1}}}
			} else {
				x = &UnaryExpr{Op: op, X: x}
			}
		case "!":
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
		case "write":
			if len(args) != 1 {
				return nil, fmt.Errorf("write expects 1 arg")
			}
			target := e
			if v, ok := e.(*Var); ok && v.Name == "stdout" {
				target = &Name{Value: "STDOUT"}
			}
			return &CallExpr{Func: "fwrite", Args: []Expr{target, args[0]}}, nil
		case "contains":
			if isStringExpr(e) {
				return &CallExpr{Func: "str_contains", Args: append([]Expr{e}, args...)}, nil
			}
			if isListExpr(e) {
				return &CallExpr{Func: "in_array", Args: append(args, e)}, nil
			}
			return nil, fmt.Errorf("contains on unsupported type")
		case "padStart":
			if len(args) != 2 {
				return nil, fmt.Errorf("padStart expects 2 args")
			}
			return &CallExpr{Func: "str_pad", Args: []Expr{e, args[0], args[1], &Name{Value: "STR_PAD_LEFT"}}}, nil
		case "keys":
			if len(args) == 0 {
				return &CallExpr{Func: "array_keys", Args: []Expr{e}}, nil
			}
			if len(args) == 1 {
				if v, ok := e.(*Var); ok && v.Name == "Object" {
					return &CallExpr{Func: "array_keys", Args: args}, nil
				}
				return nil, fmt.Errorf("keys expects no args")
			}
			return nil, fmt.Errorf("keys expects at most 1 arg")
		case "get":
			if len(args) == 1 {
				return &CondExpr{Cond: &CallExpr{Func: "array_key_exists", Args: []Expr{args[0], e}}, Then: &IndexExpr{X: e, Index: args[0]}, Else: &NullLit{}}, nil
			}
			if len(args) == 2 {
				return &CondExpr{Cond: &CallExpr{Func: "array_key_exists", Args: []Expr{args[0], e}}, Then: &IndexExpr{X: e, Index: args[0]}, Else: args[1]}, nil
			}
			return nil, fmt.Errorf("get expects 1 or 2 args")
		default:
			if pf.Target != nil && pf.Target.Selector != nil {
				root := pf.Target.Selector.Root
				if _, ok := importedModules[root]; ok {
					call := fmt.Sprintf("$%s['%s']", root, method)
					return &CallExpr{Func: call, Args: args}, nil
				}
			}
			if transpileEnv != nil && pf.Target != nil {
				base := pf.Target
				if pf.Target.Selector != nil {
					base = &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
				}
				if t := types.TypeOfPrimary(base, transpileEnv); t != nil {
					if st, ok := t.(types.StructType); ok && st.Name != "" {
						if _, ok := st.Methods[method]; ok {
							call := st.Name + "_" + method
							args = append([]Expr{e}, args...)
							return &CallExpr{Func: call, Args: args}, nil
						}
					}
				}
				// Fallback: search all known structs for this method
				for name, st := range transpileEnv.Structs() {
					if _, ok := st.Methods[method]; ok {
						call := name + "_" + method
						args = append([]Expr{e}, args...)
						return &CallExpr{Func: call, Args: args}, nil
					}
				}
			}
			call := fmt.Sprintf("%s['%s']", exprString(e), method)
			return &CallExpr{Func: call, Args: args}, nil
		}
	}

	for _, f := range tail {
		e = &IndexExpr{X: e, Index: &StringLit{Value: f}}
	}

	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Cast != nil:
			if op.Cast.Type == nil {
				return nil, fmt.Errorf("unsupported cast")
			}
			if op.Cast.Type.Simple == nil {
				// ignore casts to complex types
				break
			}
			switch *op.Cast.Type.Simple {
			case "int":
				e = replaceStringNamesWithVars(e)
				if isCharExpr(e) {
					e = &CondExpr{
						Cond: &CallExpr{Func: "ctype_digit", Args: []Expr{e}},
						Then: &CallExpr{Func: "intval", Args: []Expr{e}},
						Else: &CallExpr{Func: "ord", Args: []Expr{e}},
					}
				} else {
					e = &CallExpr{Func: "intval", Args: []Expr{e}}
				}
			case "float":
				e = &CallExpr{Func: "floatval", Args: []Expr{e}}
			case "string":
				e = &CallExpr{Func: "strval", Args: []Expr{e}}
			case "bool":
				e = &CallExpr{Func: "boolval", Args: []Expr{e}}
			case "bigrat":
				usesBigRat = true
				e = &CallExpr{Func: "_bigrat", Args: []Expr{e, &NullLit{}}}
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
		case op.Field != nil && op.Field.Name == "padStart":
			if i+1 >= len(pf.Ops) || pf.Ops[i+1].Call == nil {
				return nil, fmt.Errorf("method padStart requires args")
			}
			if len(pf.Ops[i+1].Call.Args) != 2 {
				return nil, fmt.Errorf("padStart expects 2 args")
			}
			width, err := convertExpr(pf.Ops[i+1].Call.Args[0])
			if err != nil {
				return nil, err
			}
			pad, err := convertExpr(pf.Ops[i+1].Call.Args[1])
			if err != nil {
				return nil, err
			}
			e = &CallExpr{Func: "str_pad", Args: []Expr{e, width, pad, &Name{Value: "STR_PAD_LEFT"}}}
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
				if op.Index.Step != nil || op.Index.Colon2 != nil {
					return nil, fmt.Errorf("unsupported slice expression")
				}
				var end Expr
				if op.Index.End != nil {
					end, err = convertExpr(op.Index.End)
					if err != nil {
						return nil, err
					}
				}
				if isStringExpr(e) {
					e = &SubstringExpr{Str: e, Start: start, End: end}
				} else {
					e = &SliceExpr{X: e, Start: start, End: end}
				}
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
			e = &CallExpr{Func: "call_user_func", Args: append([]Expr{e}, args...)}
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
		isBuiltin := false
		if _, b := builtinNames[name]; b {
			isBuiltin = true
		}
		if transpileEnv != nil {
			if _, ok := transpileEnv.FindUnionByVariant(name); ok {
				isBuiltin = false
			} else if fn, defined := transpileEnv.GetFunc(name); defined && fn != nil {
				// user-defined function overrides builtin
				isBuiltin = false
				if closureNames[name] {
					callName = "$" + name
				}
			} else if !isBuiltin {
				if closureNames[name] {
					callName = "$" + name
				}
			}
		}
		if !isBuiltin && callName == name {
			if _, glob := globalSet[name]; !glob {
				callName = "$" + name
			}
		}
		if newName, ok := renameMap[name]; ok {
			callName = newName
		}
		if strings.HasPrefix(callName, "$") && len(structStack) > 0 && transpileEnv != nil {
			structName := structStack[len(structStack)-1]
			if st, ok := transpileEnv.GetStruct(structName); ok {
				if _, ok := st.Methods[name]; ok {
					callName = structName + "_" + name
					args = append([]Expr{&Var{Name: "self"}}, args...)
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
				// Trim trailing spaces only for known string expressions to match Mochi output
				if isStringExpr(args[i]) {
					args[i] = &CallExpr{Func: "rtrim", Args: []Expr{args[i]}}
				}
			}
		} else if name == "len" {
			if len(args) == 1 {
				if isListArg(args[0]) || isMapArg(args[0]) || isListExpr(args[0]) || isMapExpr(args[0]) {
					callName = "count"
				} else if isStringExpr(args[0]) {
					callName = "strlen"
				} else {
					usesLen = true
					return &CallExpr{Func: "_len", Args: args}, nil
				}
			}
		} else if name == "substring" {
			if len(args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		} else if name == "substr" {
			if len(args) != 3 {
				return nil, fmt.Errorf("substr expects 3 args")
			}
			return &SubstringExpr{Str: args[0], Start: args[1], End: args[2]}, nil
		} else if name == "slice" {
			if len(args) != 3 {
				return nil, fmt.Errorf("slice expects 3 args")
			}
			return &SliceExpr{X: args[0], Start: args[1], End: args[2]}, nil
		} else if name == "split" && isBuiltin {
			if len(args) != 2 {
				return nil, fmt.Errorf("split expects 2 args")
			}
			return &CallExpr{Func: "explode", Args: []Expr{args[1], args[0]}}, nil
		} else if name == "indexOf" {
			if len(args) != 2 {
				return nil, fmt.Errorf("indexOf expects 2 args")
			}
			if isStringExpr(args[0]) {
				usesIndexOf = true
				return &CallExpr{Func: "_indexof", Args: args}, nil
			}
		} else if name == "contains" && isBuiltin {
			if len(args) != 2 {
				return nil, fmt.Errorf("contains expects 2 args")
			}
			if isStringExpr(args[0]) {
				return &CallExpr{Func: "str_contains", Args: args}, nil
			}
			if isListExpr(args[0]) {
				return &CallExpr{Func: "in_array", Args: []Expr{args[1], args[0]}}, nil
			}
			if isMapExpr(args[0]) {
				return &CallExpr{Func: "array_key_exists", Args: []Expr{args[1], args[0]}}, nil
			}
			return nil, fmt.Errorf("contains on unsupported type")
		} else if name == "int" {
			if len(args) != 1 {
				return nil, fmt.Errorf("int expects 1 arg")
			}
			return &CallExpr{Func: "intval", Args: args}, nil
		} else if name == "float" {
			if len(args) != 1 {
				return nil, fmt.Errorf("float expects 1 arg")
			}
			return &CallExpr{Func: "floatval", Args: args}, nil
		} else if name == "to_float" {
			if len(args) != 1 {
				return nil, fmt.Errorf("to_float expects 1 arg")
			}
			return &CallExpr{Func: "floatval", Args: args}, nil
		} else if name == "parseIntStr" {
			if len(args) == 1 {
				usesParseIntStr = true
				return &CallExpr{Func: "parseIntStr", Args: []Expr{args[0], &IntLit{Value: 10}}}, nil
			} else if len(args) == 2 {
				usesParseIntStr = true
				return &CallExpr{Func: "parseIntStr", Args: []Expr{args[0], args[1]}}, nil
			}
			return nil, fmt.Errorf("parseIntStr expects 1 or 2 args")
		} else if name == "error" && isBuiltin {
			if len(args) != 1 {
				return nil, fmt.Errorf("error expects 1 arg")
			}
			usesPanic = true
			return &CallExpr{Func: "_panic", Args: args}, nil
		} else if name == "panic" && isBuiltin {
			if len(args) != 1 {
				return nil, fmt.Errorf("panic expects 1 arg")
			}
			usesPanic = true
			return &CallExpr{Func: "_panic", Args: args}, nil
		} else if name == "sha256" {
			if len(args) != 1 {
				return nil, fmt.Errorf("sha256 expects 1 arg")
			}
			usesSHA256 = true
			return &CallExpr{Func: "_sha256", Args: args}, nil
		} else if name == "getoutput" {
			if len(args) != 1 {
				return nil, fmt.Errorf("getoutput expects 1 arg")
			}
			usesGetOutput = true
			return &CallExpr{Func: "_getoutput", Args: args}, nil
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
		} else if name == "num" {
			if len(args) != 1 {
				return nil, fmt.Errorf("num expects 1 arg")
			}
			usesBigRat = true
			return &CallExpr{Func: "num", Args: args}, nil
		} else if name == "denom" {
			if len(args) != 1 {
				return nil, fmt.Errorf("denom expects 1 arg")
			}
			usesBigRat = true
			return &CallExpr{Func: "denom", Args: args}, nil
		} else if name == "str" {
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects 1 arg")
			}
			usesStr = true
			return &CallExpr{Func: "_str", Args: args}, nil
		} else if name == "upper" {
			if len(args) != 1 {
				return nil, fmt.Errorf("upper expects 1 arg")
			}
			return &CallExpr{Func: "strtoupper", Args: args}, nil
		} else if name == "lower" {
			if len(args) != 1 {
				return nil, fmt.Errorf("lower expects 1 arg")
			}
			return &CallExpr{Func: "strtolower", Args: args}, nil
		} else if name == "repeat" {
			if len(args) != 2 {
				return nil, fmt.Errorf("repeat expects 2 args")
			}
			usesRepeat = true
			return &CallExpr{Func: "repeat", Args: []Expr{args[0], args[1]}}, nil
		} else if name == "padStart" {
			if len(args) != 3 {
				return nil, fmt.Errorf("padStart expects 3 args")
			}
			return &CallExpr{Func: "str_pad", Args: []Expr{args[0], args[1], args[2], &Name{Value: "STR_PAD_LEFT"}}}, nil
		} else if name == "min" || name == "max" {
			if len(args) != 1 {
				return nil, fmt.Errorf("%s expects 1 arg", name)
			}
			return &CallExpr{Func: callName, Args: args}, nil
		} else if name == "append" {
			if len(args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			if transpileEnv != nil {
				switch v := args[0].(type) {
				case *Name:
					if _, err := transpileEnv.GetVar(v.Value); err == nil {
						args[0] = &Var{Name: v.Value}
					}
				case *StringLit:
					if _, err := transpileEnv.GetVar(v.Value); err == nil {
						args[0] = &Var{Name: v.Value}
					}
				}
			}
			args[1] = replaceStringNamesWithVars(args[1])
			usesAppend = true
			return &CallExpr{Func: "_append", Args: []Expr{args[0], args[1]}}, nil
		} else if name == "concat" {
			if len(args) != 2 {
				return nil, fmt.Errorf("concat expects 2 args")
			}
			args[0] = replaceStringNamesWithVars(args[0])
			args[1] = replaceStringNamesWithVars(args[1])
			return &CallExpr{Func: "array_merge", Args: []Expr{args[0], args[1]}}, nil
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
		} else if name == "keys" {
			if len(args) != 1 {
				return nil, fmt.Errorf("keys expects 1 arg")
			}
			var target Expr = args[0]
			if isGroupArg(args[0]) {
				target = &IndexExpr{X: args[0], Index: &StringLit{Value: "items"}}
			}
			return &CallExpr{Func: "array_keys", Args: []Expr{target}}, nil
		} else if name == "now" {
			if len(args) != 0 {
				return nil, fmt.Errorf("now expects no args")
			}
			usesNow = true
			return &CallExpr{Func: "_now"}, nil
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
	case p.Struct != nil:
		items := []MapEntry{}
		if transpileEnv != nil {
			if ut, ok := transpileEnv.FindUnionByVariant(p.Struct.Name); ok {
				items = append(items, MapEntry{Key: &StringLit{Value: "__tag"}, Value: &StringLit{Value: p.Struct.Name}})
				st := ut.Variants[p.Struct.Name]
				for idx, fname := range st.Order {
					if idx < len(p.Struct.Fields) {
						v, err := convertExpr(p.Struct.Fields[idx].Value)
						if err != nil {
							return nil, err
						}
						items = append(items, MapEntry{Key: &StringLit{Value: fname}, Value: v})
					}
				}
				return &MapLit{Items: items}, nil
			}
		}
		items = make([]MapEntry, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := convertExpr(f.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapEntry{Key: &StringLit{Value: f.Name}, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Map != nil:
		items := make([]MapEntry, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			if v, ok := k.(*Var); ok {
				k = &StringLit{Value: v.Name}
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapEntry{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		var refFlags []bool
		for i, p2 := range p.FunExpr.Params {
			params[i] = p2.Name
			if transpileEnv != nil {
				typ := simpleResolveType(p2.Type, transpileEnv)
				if isFuncType(typ) {
					closureNames[p2.Name] = true
				}
			}
		}
		funcStack = append(funcStack, params)
		var body []Stmt
		if p.FunExpr.ExprBody != nil {
			ex, err := convertExpr(p.FunExpr.ExprBody)
			if err != nil {
				funcStack = funcStack[:len(funcStack)-1]
				return nil, err
			}
			if call, ok := ex.(*CallExpr); ok && call.Func == "echo" {
				body = []Stmt{&ExprStmt{Expr: ex}}
			} else {
				body = []Stmt{&ReturnStmt{Value: ex}}
			}
		} else {
			var err error
			body, err = convertStmtList(p.FunExpr.BlockBody)
			if err != nil {
				funcStack = funcStack[:len(funcStack)-1]
				return nil, err
			}
		}
		funcStack = funcStack[:len(funcStack)-1]
		refFlags = markRefParams(body, params)
		mut := mutatedVars(body)
		outer := map[string]struct{}{}
		for _, frame := range funcStack {
			for _, v := range frame {
				outer[v] = struct{}{}
			}
		}
		fv := freeVars(body, params)
		uses := []string{}
		for _, n := range fv {
			if _, ok := outer[n]; !ok {
				continue
			}
			prefix := ""
			if _, ok := mut[n]; ok {
				prefix = "&"
			}
			if _, ok := globalSet[n]; ok {
				if _, fn := globalFuncs[n]; fn {
					continue
				}
				uses = append(uses, "&"+n)
			} else {
				uses = append(uses, prefix+n)
			}
		}
		for n := range outer {
			found := false
			for _, u := range uses {
				if u == n || u == "&"+n {
					found = true
					break
				}
			}
			if found {
				continue
			}
			prefix := ""
			if _, ok := mut[n]; ok {
				prefix = "&"
			}
			if _, ok := globalSet[n]; ok {
				if _, fn := globalFuncs[n]; fn {
					continue
				}
				uses = append(uses, "&"+n)
			} else {
				uses = append(uses, prefix+n)
			}
		}
		for _, n := range fv {
			present := false
			for _, u := range uses {
				if u == n || u == "&"+n {
					present = true
					break
				}
			}
			if present {
				continue
			}
			prefix := ""
			if _, ok := mut[n]; ok {
				prefix = "&"
			}
			if _, ok := globalSet[n]; ok {
				if _, fn := globalFuncs[n]; fn {
					continue
				}
				uses = append(uses, "&"+n)
			} else {
				uses = append(uses, prefix+n)
			}
		}
		// remove duplicates from captured variables
		if len(uses) > 1 {
			uniq := make([]string, 0, len(uses))
			seen := map[string]struct{}{}
			for _, u := range uses {
				if _, ok := seen[u]; ok {
					continue
				}
				seen[u] = struct{}{}
				uniq = append(uniq, u)
			}
			uses = uniq
		}
		return &ClosureExpr{Params: params, RefParams: refFlags, Uses: uses, Body: body}, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 {
			return nil, fmt.Errorf("selector tail not supported")
		}
		name := p.Selector.Root
		lookup := name
		if newName, ok := renameMap[name]; ok {
			lookup = newName
		}
		if _, ok := globalFuncs[lookup]; ok {
			if newName, ok := renameMap[name]; ok {
				name = newName
			}
			return &StringLit{Value: name}, nil
		}
		if newName, ok := renameMap[name]; ok {
			name = newName
		}
		return &Var{Name: name}, nil
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
	case p.Fetch != nil:
		urlExpr, err := convertExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		usesFetch = true
		args := []Expr{urlExpr}
		if p.Fetch.With != nil {
			withExpr, err := convertExpr(p.Fetch.With)
			if err != nil {
				return nil, err
			}
			args = append(args, withExpr)
		}
		return &CallExpr{Func: "_fetch", Args: args}, nil
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
	case l.Null:
		return &NullLit{}, nil
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
		var val Expr
		if st.Let.Value != nil {
			if len(funcStack) > 0 {
				funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], st.Let.Name)
				defer func() { funcStack[len(funcStack)-1] = funcStack[len(funcStack)-1][:len(funcStack[len(funcStack)-1])-1] }()
			}
			v, err := convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
			if q, ok := v.(*QueryExpr); ok {
				if len(funcStack) == 0 {
					addGlobal(st.Let.Name)
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
				if _, ok := typ.(types.FuncType); ok {
					closureNames[st.Let.Name] = true
				}
			}
		}
		if len(funcStack) == 0 {
			addGlobal(st.Let.Name)
		} else {
			funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], st.Let.Name)
		}
		return &LetStmt{Name: st.Let.Name, Value: val}, nil
	case st.Var != nil:
		var val Expr
		if st.Var.Value != nil {
			if len(funcStack) > 0 {
				funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], st.Var.Name)
				defer func() { funcStack[len(funcStack)-1] = funcStack[len(funcStack)-1][:len(funcStack[len(funcStack)-1])-1] }()
			}
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
				if _, ok := typ.(types.FuncType); ok {
					closureNames[st.Var.Name] = true
				}
			}
		}
		if len(funcStack) == 0 {
			addGlobal(st.Var.Name)
		} else {
			funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], st.Var.Name)
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
		var refFlags []bool
		savedEnv := transpileEnv
		childEnv := transpileEnv
		if transpileEnv != nil {
			childEnv = types.NewEnv(transpileEnv)
			for i, p := range st.Fun.Params {
				params[i] = p.Name
				typ := simpleResolveType(p.Type, transpileEnv)
				childEnv.SetVar(p.Name, typ, true)
				if isFuncType(typ) {
					closureNames[p.Name] = true
				}
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
		refFlags = markRefParams(body, params)
		mut := mutatedVars(body)
		name := st.Fun.Name
		if _, reserved := phpReserved[name]; reserved {
			newName := "mochi_" + name
			renameMap[name] = newName
			name = newName
		}
		funcRefParams[name] = refFlags
		uses := []string{"&" + st.Fun.Name}
		for _, frame := range funcStack {
			for _, v := range frame {
				if _, ok := mut[v]; ok {
					uses = append(uses, "&"+v)
				} else {
					uses = append(uses, v)
				}
			}
		}
		if len(globalNames) > 0 {
			for _, g := range globalNames {
				if _, ok := globalFuncs[g]; ok {
					continue
				}
				uses = append(uses, "&"+g)
			}
		}
		if len(uses) > 1 {
			uniq := make([]string, 0, len(uses))
			seen := map[string]struct{}{}
			for _, u := range uses {
				if _, ok := seen[u]; ok {
					continue
				}
				seen[u] = struct{}{}
				uniq = append(uniq, u)
			}
			uses = uniq
		}
		if wasTop {
			decl := &FuncDecl{Name: name, Params: params, RefParams: refFlags, Body: body}
			addGlobalFunc(name)
			return decl, nil
		}
		clo := &ClosureExpr{Params: params, RefParams: refFlags, Uses: uses, Body: body}
		if len(funcStack) == 0 {
			addGlobal(name)
		} else {
			funcStack[len(funcStack)-1] = append(funcStack[len(funcStack)-1], name)
		}
		return &LetStmt{Name: name, Value: clo}, nil
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
		str := false
		if transpileEnv != nil {
			t := types.ExprType(st.For.Source, savedEnv)
			if types.IsMapType(t) {
				keys = true
			}
			if types.IsStringType(t) {
				str = true
			}
		}
		return &ForEachStmt{Name: st.For.Name, Expr: expr, Keys: keys, String: str, Body: body}, nil
	case st.Import != nil:
		stmt, err := convertImport(st.Import)
		if err != nil {
			return nil, err
		}
		if ls, ok := stmt.(*LetStmt); ok && len(funcStack) == 0 {
			addGlobal(ls.Name)
		}
		return stmt, nil
	case st.ExternVar != nil, st.ExternFun != nil, st.ExternType != nil, st.ExternObject != nil:
		return nil, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Bench != nil:
		usesNow = true
		savedEnv := transpileEnv
		if transpileEnv != nil {
			transpileEnv = types.NewEnv(transpileEnv)
		}
		body, err := convertStmtList(st.Bench.Body)
		if transpileEnv != nil {
			transpileEnv = savedEnv
		}
		if err != nil {
			return nil, err
		}
		return &BenchStmt{Name: st.Bench.Name, Body: body}, nil
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.Type != nil:
		for _, mem := range st.Type.Members {
			if mem.Method == nil {
				continue
			}
			fun := *mem.Method
			fun.Name = st.Type.Name + "_" + fun.Name
			fun.Params = append([]*parser.Param{{Name: "self"}}, fun.Params...)
			// Inject local variables for struct fields so method body can
			// reference them directly.
			var prelude []*parser.Statement
			for _, fm := range st.Type.Members {
				if fm.Field != nil {
					fname := fm.Field.Name
					fieldExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: "self", Tail: []string{fname}}}}}}}
					prelude = append(prelude, &parser.Statement{Var: &parser.VarStmt{Name: fname, Value: fieldExpr}})
				}
			}
			fun.Body = append(prelude, fun.Body...)
			stmt := &parser.Statement{Fun: &fun}
			structStack = append(structStack, st.Type.Name)
			conv, err := convertStmt(stmt)
			structStack = structStack[:len(structStack)-1]
			if err != nil {
				return nil, err
			}
			if conv != nil {
				extraStmts = append(extraStmts, conv)
			}
		}
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
		if s.Type != nil {
			for _, mem := range s.Type.Members {
				if mem.Method == nil {
					continue
				}
				fun := *mem.Method
				fun.Name = s.Type.Name + "_" + fun.Name
				fun.Params = append([]*parser.Param{{Name: "self"}}, fun.Params...)
				var prelude []*parser.Statement
				for _, fm := range s.Type.Members {
					if fm.Field != nil {
						fname := fm.Field.Name
						fieldExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: "self", Tail: []string{fname}}}}}}}
						prelude = append(prelude, &parser.Statement{Var: &parser.VarStmt{Name: fname, Value: fieldExpr}})
					}
				}
				fun.Body = append(prelude, fun.Body...)
				stmt := &parser.Statement{Fun: &fun}
				structStack = append(structStack, s.Type.Name)
				st, err := convertStmt(stmt)
				structStack = structStack[:len(structStack)-1]
				if err != nil {
					return nil, err
				}
				if st != nil {
					out = append(out, st)
				}
			}
			continue
		}
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
	var unionCases []UnionMatchCase
	var defaultRes Expr
	allUnion := true
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
			if transpileEnv != nil {
				switch pe := pex.(type) {
				case *CallExpr:
					if ut, ok := transpileEnv.FindUnionByVariant(pe.Func); ok {
						st := ut.Variants[pe.Func]
						vars := make([]string, len(pe.Args))
						for j, a := range pe.Args {
							if v, ok := a.(*Var); ok {
								vars[j] = v.Name
							} else {
								vars[j] = "_"
							}
						}
						fields := make([]string, len(st.Order))
						copy(fields, st.Order)
						unionCases = append(unionCases, UnionMatchCase{Tag: pe.Func, Vars: vars, Fields: fields, Result: res})
						continue
					}
				case *Var:
					if ut, ok := transpileEnv.FindUnionByVariant(pe.Name); ok {
						st := ut.Variants[pe.Name]
						fields := make([]string, len(st.Order))
						copy(fields, st.Order)
						unionCases = append(unionCases, UnionMatchCase{Tag: pe.Name, Fields: fields, Result: res})
						continue
					}
				case nil:
					// wildcard pattern
					if len(unionCases) > 0 {
						defaultRes = res
						continue
					}
				}
			}
		} else {
			if len(unionCases) > 0 {
				defaultRes = res
				continue
			}
		}
		arms[i] = MatchArm{Pattern: pat, Result: res}
		allUnion = false
	}
	if len(unionCases) > 0 {
		if allUnion {
			return &UnionMatchExpr{Target: target, Cases: unionCases, Default: defaultRes}, nil
		}
		return &UnionMatchExpr{Target: target, Cases: unionCases, Default: defaultRes}, nil
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
			for _, g := range globalNames {
				if _, ok := globalFuncs[g]; ok {
					continue
				}
				uses = append(uses, "&"+g)
			}
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
			for _, g := range globalNames {
				if _, ok := globalFuncs[g]; ok {
					continue
				}
				uses = append(uses, "&"+g)
			}
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
			for _, g := range globalNames {
				if _, ok := globalFuncs[g]; ok {
					continue
				}
				uses = append(uses, "&"+g)
			}
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
		for _, g := range globalNames {
			if _, ok := globalFuncs[g]; ok {
				continue
			}
			uses = append(uses, "&"+g)
		}
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
		for _, g := range globalNames {
			if _, ok := globalFuncs[g]; ok {
				continue
			}
			uses = append(uses, "&"+g)
		}
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

// exprType attempts to determine the static type of expression e using
// transpileEnv. It only handles a subset of expressions needed by the
// transpiler helpers.
func exprType(e Expr) types.Type {
	if transpileEnv == nil {
		return types.AnyType{}
	}
	switch v := e.(type) {
	case *Var:
		if t, err := transpileEnv.GetVar(v.Name); err == nil {
			return t
		}
	case *IndexExpr:
		t := exprType(v.X)
		switch tt := t.(type) {
		case types.ListType:
			return tt.Elem
		case types.MapType:
			return tt.Value
		case types.StringType:
			return types.StringType{}
		}
	case *SubstringExpr:
		return types.StringType{}
	case *SliceExpr:
		t := exprType(v.X)
		if _, ok := t.(types.StringType); ok {
			return types.StringType{}
		}
		if lt, ok := t.(types.ListType); ok {
			return lt
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
		if t, err := transpileEnv.GetVar(v.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				return ft.Return
			}
		}
		switch v.Func {
		case "len":
			return types.IntType{}
		case "append":
			if len(v.Args) > 0 {
				return exprType(v.Args[0])
			}
		}
	}
	return types.AnyType{}
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

func isVarInScope(name string) bool {
	for i := len(funcStack) - 1; i >= 0; i-- {
		for _, v := range funcStack[i] {
			if v == name {
				return true
			}
		}
	}
	if transpileEnv != nil {
		if _, err := transpileEnv.GetVar(name); err == nil {
			return true
		}
	}
	return false
}

func replaceStringNamesWithVars(e Expr) Expr {
	switch v := e.(type) {
	case *StringLit:
		if isVarInScope(v.Value) {
			return &Var{Name: v.Value}
		}
	case *Name:
		if isVarInScope(v.Value) {
			return &Var{Name: v.Value}
		}
	case *IndexExpr:
		v.X = replaceStringNamesWithVars(v.X)
		if _, ok := v.Index.(*StringLit); !ok {
			v.Index = replaceStringNamesWithVars(v.Index)
		}
	case *SliceExpr:
		v.X = replaceStringNamesWithVars(v.X)
		v.Start = replaceStringNamesWithVars(v.Start)
		if v.End != nil {
			v.End = replaceStringNamesWithVars(v.End)
		}
	case *SubstringExpr:
		v.Str = replaceStringNamesWithVars(v.Str)
		v.Start = replaceStringNamesWithVars(v.Start)
		if v.End != nil {
			v.End = replaceStringNamesWithVars(v.End)
		}
		if _, ok := v.Str.(*Var); ok {
			return &IndexExpr{X: v.Str, Index: v.Start}
		}
	case *CallExpr:
		for i := range v.Args {
			v.Args[i] = replaceStringNamesWithVars(v.Args[i])
		}
	case *CondExpr:
		v.Cond = replaceStringNamesWithVars(v.Cond)
		v.Then = replaceStringNamesWithVars(v.Then)
		v.Else = replaceStringNamesWithVars(v.Else)
	case *BinaryExpr:
		v.Left = replaceStringNamesWithVars(v.Left)
		v.Right = replaceStringNamesWithVars(v.Right)
	case *UnaryExpr:
		v.X = replaceStringNamesWithVars(v.X)
	case *ListLit:
		for i := range v.Elems {
			v.Elems[i] = replaceStringNamesWithVars(v.Elems[i])
		}
	case *MapLit:
		for i := range v.Items {
			v.Items[i].Key = replaceStringNamesWithVars(v.Items[i].Key)
			v.Items[i].Value = replaceStringNamesWithVars(v.Items[i].Value)
		}
	}
	return e
}

func simpleResolveType(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = simpleResolveType(p, env)
		}
		var ret types.Type
		if t.Fun.Return != nil {
			ret = simpleResolveType(t.Fun.Return, env)
		}
		return types.FuncType{Params: params, Return: ret}
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
		case "bigrat":
			return types.BigRatType{}
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
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(c.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if _, ok := ft.Return.(types.ListType); ok {
						return true
					}
				}
			}
		}
		switch c.Func {
		case "array_merge", "_append", "array_slice", "array_values", "array_diff", "array_intersect", "array_unique":
			return true
		}
	} else if b, ok := e.(*BinaryExpr); ok {
		switch b.Op {
		case "union", "union_all", "except", "intersect":
			return true
		}
	} else if _, ok := e.(*SliceExpr); ok {
		return true
	} else if ie, ok := e.(*IndexExpr); ok {
		if _, ok := exprType(ie).(types.ListType); ok {
			return true
		}
	}
	return false
}

func isMapExpr(e Expr) bool {
	if isMapArg(e) {
		return true
	}
	switch v := e.(type) {
	case *CallExpr:
		c := v
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(c.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if types.IsMapType(ft.Return) {
						return true
					}
				}
			}
		}
	case *IndexExpr:
		if _, ok := v.Index.(*StringLit); ok {
			if _, ok := exprType(v.X).(types.MapType); ok {
				return true
			}
		}
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
	case *CallExpr:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if _, ok := ft.Return.(types.StringType); ok {
						return true
					}
				}
			}
		}
		switch v.Func {
		case "json_encode", "strval", "strtoupper", "strtolower", "trim", "fgets", "_str":
			return true
		}
	case *CondExpr:
		if isStringExpr(v.Then) && isStringExpr(v.Else) {
			return true
		}
	case *BinaryExpr:
		if v.Op == "." {
			return true
		}
		if v.Op == "+" {
			if isStringExpr(v.Left) || isStringExpr(v.Right) {
				return true
			}
		}
	case *IndexExpr:
		if _, ok := exprType(v).(types.StringType); ok {
			return true
		}
	case *SliceExpr:
		if isStringExpr(v.X) {
			return true
		}
	case *SubstringExpr:
		if isStringExpr(v.Str) {
			return true
		}
	}
	return false
}

// isCharExpr reports whether e represents a single character extracted from a
// string. This helps choose between ord() and intval() when casting to int.
func isCharExpr(e Expr) bool {
	switch v := e.(type) {
	case *SubstringExpr:
		if _, ok := exprType(v.Str).(types.StringType); ok {
			return true
		}
	case *IndexExpr:
		if _, ok := exprType(v.X).(types.StringType); ok {
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

func isIntExpr(e Expr) bool {
	switch v := e.(type) {
	case *IntLit:
		return true
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if _, ok := t.(types.IntType); ok {
					return true
				}
			}
		}
	case *BinaryExpr:
		switch v.Op {
		case "+", "-", "*", "/", "%":
			return isIntExpr(v.Left) && isIntExpr(v.Right)
		}
	case *GroupExpr:
		return isIntExpr(v.X)
	case *IntDivExpr:
		return isIntExpr(v.Left) && isIntExpr(v.Right)
	case *CondExpr:
		return isIntExpr(v.Then) && isIntExpr(v.Else)
	case *CallExpr:
		switch v.Func {
		case "intval", "_intdiv", "_iadd", "_isub", "_imul", "_idiv", "_imod":
			return true
		}
	}
	return false
}

func isBigIntExpr(e Expr) bool {
	switch v := e.(type) {
	case *IntLit:
		return true
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if types.IsIntType(t) || types.IsBigIntType(t) {
					return true
				}
			}
		}
	case *BinaryExpr:
		switch v.Op {
		case "+", "-", "*", "/", "%":
			return isBigIntExpr(v.Left) || isBigIntExpr(v.Right)
		}
	case *GroupExpr:
		return isBigIntExpr(v.X)
	case *CondExpr:
		return isBigIntExpr(v.Then) && isBigIntExpr(v.Else)
	case *IntDivExpr:
		return isBigIntExpr(v.Left) && isBigIntExpr(v.Right)
	case *CallExpr:
		switch v.Func {
		case "_iadd", "_isub", "_imul", "_idiv", "_imod", "_intdiv":
			return true
		}
	}
	if t := exprType(e); types.IsIntType(t) || types.IsBigIntType(t) {
		return true
	}
	return false
}

func isBigRatExpr(e Expr) bool {
	switch v := e.(type) {
	case *Var:
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Name); err == nil {
				if _, ok := t.(types.BigRatType); ok {
					return true
				}
			}
		}
	case *CallExpr:
		if v.Func == "_bigrat" {
			return true
		}
		if transpileEnv != nil {
			if t, err := transpileEnv.GetVar(v.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if _, ok := ft.Return.(types.BigRatType); ok {
						return true
					}
				}
			}
		}
	case *BinaryExpr:
		switch v.Op {
		case "+", "-", "*", "/":
			return isBigRatExpr(v.Left) || isBigRatExpr(v.Right)
		}
	case *GroupExpr:
		return isBigRatExpr(v.X)
	case *CondExpr:
		return isBigRatExpr(v.Then) && isBigRatExpr(v.Else)
	case *IndexExpr:
		if _, ok := exprType(v).(types.BigRatType); ok {
			return true
		}
	}
	if _, ok := exprType(e).(types.BigRatType); ok {
		return true
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
	if lang == "python" && path == "subprocess" {
		usesGetOutput = true
		items := []MapEntry{
			{Key: &StringLit{Value: "getoutput"}, Value: &StringLit{Value: "_getoutput"}},
		}
		return &LetStmt{Name: alias, Value: &MapLit{Items: items}}, nil
	}
	if lang == "go" && im.Auto && strings.Contains(path, "testpkg") {
		items := []MapEntry{
			{Key: &StringLit{Value: "Add"}, Value: &ClosureExpr{Params: []string{"a", "b"}, Body: []Stmt{&ReturnStmt{Value: &BinaryExpr{Left: &Var{Name: "a"}, Op: "+", Right: &Var{Name: "b"}}}}}},
			{Key: &StringLit{Value: "Pi"}, Value: &FloatLit{Value: 3.14}},
			{Key: &StringLit{Value: "Answer"}, Value: &IntLit{Value: 42}},
			{Key: &StringLit{Value: "FifteenPuzzleExample"}, Value: &ClosureExpr{Params: []string{}, Body: []Stmt{&ReturnStmt{Value: &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}}}}},
			{Key: &StringLit{Value: "MD5Hex"}, Value: &StringLit{Value: "md5"}},
			{Key: &StringLit{Value: "ECDSAExample"}, Value: &ClosureExpr{Params: []string{}, Body: []Stmt{&ReturnStmt{Value: &MapLit{Items: []MapEntry{
				{Key: &StringLit{Value: "D"}, Value: &StringLit{Value: "1234567890"}},
				{Key: &StringLit{Value: "X"}, Value: &StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"}},
				{Key: &StringLit{Value: "Y"}, Value: &StringLit{Value: "86807430002474105664458509423764867536342689150582922106807036347047552480521"}},
				{Key: &StringLit{Value: "Hash"}, Value: &StringLit{Value: "0xe6f9ed0d"}},
				{Key: &StringLit{Value: "R"}, Value: &StringLit{Value: "43162711582587979080031819627904423023685561091192625653251495188141318209988"}},
				{Key: &StringLit{Value: "S"}, Value: &StringLit{Value: "94150071556658883365738746782965214584303361499725266605620843043083873122499"}},
				{Key: &StringLit{Value: "Valid"}, Value: &BoolLit{Value: true}},
			}}}}}},
		}
		return &LetStmt{Name: alias, Value: &MapLit{Items: items}}, nil
	}
	if lang == "go" && im.Auto && path == "net" {
		usesLookupHost = true
		items := []MapEntry{
			{Key: &StringLit{Value: "LookupHost"}, Value: &StringLit{Value: "_lookup_host"}},
		}
		return &LetStmt{Name: alias, Value: &MapLit{Items: items}}, nil
	}
	if lang == "go" && im.Auto && path == "os" {
		usesEnviron = true
		items := []MapEntry{
			{Key: &StringLit{Value: "Getenv"}, Value: &StringLit{Value: "getenv"}},
			{Key: &StringLit{Value: "Environ"}, Value: &StringLit{Value: "_environ"}},
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
	case *BenchStmt:
		n := &ast.Node{Kind: "bench", Value: st.Name}
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
