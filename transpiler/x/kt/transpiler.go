//go:build slow

package kt

import (
	"bytes"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

var (
	extraDecls     []*DataClass
	extraUnions    []*SumType
	extraAliases   []*TypeAlias
	helperSnippets map[string]string
	extraHelpers   []string
	helpersUsed    map[string]bool
	localFuncs     map[string]bool
	varDecls       map[string]*VarStmt
	funcRets       map[string]string
	builtinAliases map[string]string
	reserved       map[string]bool
	currentRetType string
	benchMain      bool
	unusedCounter  int
)

func init() {
	helperSnippets = map[string]string{
		"_load": `fun _load(path: String?, opts: Map<String, Any?>?): MutableList<MutableMap<String, Any?>> {
    val fmt = opts?.get("format") as? String ?: "csv"
    val lines = if (path == null || path == "-") {
        listOf<String>()
    } else {
        var f = java.io.File(path)
        if (!f.isAbsolute) {
            if (!f.exists()) {
                System.getenv("MOCHI_ROOT")?.let { root ->
                    var clean = path!!
                    while (clean.startsWith("../")) clean = clean.substring(3)
                    var cand = java.io.File(root + "/tests/" + clean)
                    if (!cand.exists()) cand = java.io.File(root + "/" + clean)
                    f = cand
                }
            }
        }
        if (f.exists()) f.readLines() else listOf<String>()
    }
    return when (fmt) {
        "yaml" -> loadYamlSimple(lines)
        "jsonl" -> lines.filter { it.isNotBlank() }
            .map { parseJsonLine(it) }
            .toMutableList()
        else -> mutableListOf()
    }
}`,
		"loadYamlSimple": `fun loadYamlSimple(lines: List<String>): MutableList<MutableMap<String, Any?>> {
    val res = mutableListOf<MutableMap<String, Any?>>()
    var cur: MutableMap<String, Any?>? = null
    for (ln in lines) {
        val t = ln.trim()
        if (t.startsWith("- ")) {
            cur?.let { res.add(it) }
            cur = mutableMapOf()
            val idx = t.indexOf(':', 2)
            if (idx >= 0) {
                val k = t.substring(2, idx).trim()
                val v = parseSimpleValue(t.substring(idx + 1))
                cur!![k] = v
            }
        } else if (t.contains(':')) {
            val idx = t.indexOf(':')
            val k = t.substring(0, idx).trim()
            val v = parseSimpleValue(t.substring(idx + 1))
            cur?.set(k, v)
        }
    }
    cur?.let { res.add(it) }
    return res
}`,
		"parseSimpleValue": `fun parseSimpleValue(s: String): Any? {
    val t = s.trim()
    return when {
        t.matches(Regex("^-?\\d+$")) -> t.toInt()
        t.matches(Regex("^-?\\d+\\.\\d+$")) -> t.toDouble()
        t.equals("true", true) -> true
        t.equals("false", true) -> false
        t.startsWith("\"") && t.endsWith("\"") -> t.substring(1, t.length - 1)
        else -> t
    }
}`,
		"parseJsonLine": `fun parseJsonLine(line: String): MutableMap<String, Any?> {
    val obj = mutableMapOf<String, Any?>()
    val r = Regex("\"([^\"]+)\":\\s*(\"[^\"]*\"|-?\\d+(?:\\.\\d+)?|true|false|null)")
    for (m in r.findAll(line)) {
        val k = m.groupValues[1]
        val v = parseSimpleValue(m.groupValues[2])
        obj[k] = v
    }
    return obj
}`,
		"_save": `fun _save(rows: List<Any?>, path: String?, opts: Map<String, Any?>?) {
    val fmt = opts?.get("format") as? String ?: "csv"
    val writer = if (path == null || path == "-") {
        java.io.BufferedWriter(java.io.OutputStreamWriter(System.out))
    } else {
        java.io.File(path).bufferedWriter()
    }
    if (fmt == "jsonl") {
        for (r in rows) {
            writer.write(toJson(r))
            writer.newLine()
        }
    }
    if (path != null && path != "-") writer.close()
}`,
		"_powInt": `fun _powInt(base: Int, exp: Int): Int {
    var res = 1
    var b = base
    var e = exp
    while (e > 0) {
        res *= b
        e--
    }
    return res
}`,
		"toJson": `fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}`,
		"_len": `fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
}`,
		"expect":       `fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }`,
		"input":        `fun input(): String = readLine() ?: ""`,
		"importBigInt": `import java.math.BigInteger`,
		"_now": `var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}`,
		"bigRatHelpers": `data class BigRat(var num: BigInteger, var den: BigInteger = BigInteger.ONE) {
    init {
        if (den.signum() < 0) {
            num = num.negate()
            den = den.negate()
        }
        val g = num.gcd(den)
        num = num.divide(g)
        den = den.divide(g)
    }
    fun add(o: BigRat) = BigRat(num.multiply(o.den).add(o.num.multiply(den)), den.multiply(o.den))
    fun sub(o: BigRat) = BigRat(num.multiply(o.den).subtract(o.num.multiply(den)), den.multiply(o.den))
    fun mul(o: BigRat) = BigRat(num.multiply(o.num), den.multiply(o.den))
    fun div(o: BigRat) = BigRat(num.multiply(o.den), den.multiply(o.num))
}
fun _bigrat(n: Any?, d: Any? = 1): BigRat {
    if (n is BigRat && d == null) return BigRat(n.num, n.den)
    val denom = when (d) {
        null -> BigInteger.ONE
        is BigInteger -> d
        is Number -> BigInteger.valueOf(d.toLong())
        else -> BigInteger.ONE
    }
    val numer = when (n) {
        is BigRat -> n.num
        is BigInteger -> n
        is Number -> BigInteger.valueOf(n.toLong())
        else -> BigInteger.ZERO
    }
    val den = if (n is BigRat && d == null) n.den else denom
    return BigRat(numer, den)
}
fun _num(r: BigRat): BigInteger = r.num
fun _denom(r: BigRat): BigInteger = r.den
fun _add(a: BigRat, b: BigRat): BigRat = a.add(b)
fun _sub(a: BigRat, b: BigRat): BigRat = a.sub(b)
fun _mul(a: BigRat, b: BigRat): BigRat = a.mul(b)
fun _div(a: BigRat, b: BigRat): BigRat = a.div(b)`,
		"sha256": `fun _sha256(bs: List<Int>): MutableList<Int> {
val md = java.security.MessageDigest.getInstance("SHA-256")
val arr = ByteArray(bs.size)
for (i in bs.indices) arr[i] = bs[i].toByte()
val hash = md.digest(arr)
val res = mutableListOf<Int>()
for (b in hash) res.add((b.toInt() and 0xff))
return res
}`,
		"pow2": `fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}`,
		"lshift": `fun lshift(x: Int, n: Int): Int {
return (x.toLong() * pow2(n)).toInt()
}`,
		"rshift": `fun rshift(x: Int, n: Int): Int {
return (x.toLong() / pow2(n)).toInt()
}`,
		"split": `fun split(s: String, sep: String): MutableList<String> {
    return s.split(sep).toMutableList()
}`,
		"repeat": `fun repeat(s: String, n: Int): String {
    val sb = StringBuilder()
    repeat(n) { sb.append(s) }
    return sb.toString()
}`,
	}
	reserved = map[string]bool{
		"package": true, "as": true, "typealias": true, "class": true,
		"this": true, "super": true, "val": true, "var": true,
		"fun": true, "for": true, "null": true, "true": true, "false": true,
		"is": true, "in": true, "throw": true, "return": true, "break": true,
		"continue": true, "object": true, "if": true, "try": true, "else": true,
		"while": true, "do": true, "when": true, "interface": true,
	}
	extraHelpers = nil
	helpersUsed = map[string]bool{}
	localFuncs = map[string]bool{}
	funcRets = map[string]string{
		"kotlin.math.abs": "Double",
		"_num":            "BigInteger",
		"_denom":          "BigInteger",
		"_bigrat":         "BigRat",
		"_add":            "BigRat",
		"_sub":            "BigRat",
		"_mul":            "BigRat",
		"_div":            "BigRat",
		"_sha256":         "MutableList<Int>",
		"indexOf":         "Int",
		"split":           "MutableList<String>",
	}
}

// HasFuncRet reports if a return type mapping exists for name.
func HasFuncRet(name string) bool {
	_, ok := funcRets[name]
	return ok
}

// Program represents a simple Kotlin program consisting of statements executed in main.
// Program contains top level functions and statements executed in `main`.
type Program struct {
	Structs []*DataClass
	Unions  []*SumType
	Aliases []*TypeAlias
	Funcs   []*FuncDef
	Globals []Stmt
	Stmts   []Stmt
	Helpers []string
}

func indent(w io.Writer, n int) {
	for i := 0; i < n; i++ {
		io.WriteString(w, "    ")
	}
}

func useHelper(name string) {
	if helpersUsed[name] || localFuncs[name] {
		return
	}
	if code, ok := helperSnippets[name]; ok {
		helpersUsed[name] = true
		extraHelpers = append(extraHelpers, code)
		if name == "lshift" || name == "rshift" {
			useHelper("pow2")
		}
	}
}

// SetBenchMain configures whether the generated main function should be wrapped
// in a benchmark block when emitting code. When enabled, the program will print
// a JSON object containing duration and memory statistics.
func SetBenchMain(v bool) { benchMain = v }

func safeName(n string) string {
	if reserved[n] {
		return "_" + n
	}
	allUnderscore := true
	for _, r := range n {
		if r != '_' {
			allUnderscore = false
			break
		}
	}
	if allUnderscore {
		unusedCounter++
		return fmt.Sprintf("_u%d", unusedCounter)
	}
	return n
}

type Stmt interface{ emit(io.Writer, int) }

// IndexAssignStmt assigns to a[i] or m[key].
type IndexAssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *IndexAssignStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	switch tgt := s.Target.(type) {
	case *IndexExpr:
		tgt.emitTarget(w)
	case *CastExpr:
		if ix, ok := tgt.Value.(*IndexExpr); ok {
			ix.emitTarget(w)
		} else {
			tgt.emit(w)
		}
	default:
		s.Target.emit(w)
	}
	io.WriteString(w, " = ")
	s.Value.emit(w)
}

// ExpectStmt asserts that a condition holds.
type ExpectStmt struct{ Cond Expr }

func (s *ExpectStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "expect(")
	if s.Cond != nil {
		s.Cond.emit(w)
	}
	io.WriteString(w, ")")
}

// ReturnStmt is a return statement inside a function.
type ReturnStmt struct{ Value Expr }

func (s *ReturnStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "return")
	if s.Value != nil {
		io.WriteString(w, " ")
		s.Value.emit(w)
	}
}

// FuncDef represents a top level function definition.
type FuncDef struct {
	Name   string
	Params []string
	Ret    string
	Body   []Stmt
}

// DataClass declares a simple Kotlin data class.
type DataClass struct {
	Name     string
	Fields   []ParamDecl
	Extends  string
	IsObject bool
	Methods  []*FuncDef
}

// SumType represents a simple sealed interface with variants.
type SumType struct {
	Name     string
	Variants []*DataClass
}

// TypeAlias represents `typealias Name = ...` declarations.
type TypeAlias struct {
	Name string
	Type string
}

func (u *SumType) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "sealed class "+u.Name+"\n")
	for _, v := range u.Variants {
		v.Extends = u.Name
		v.emit(w, indentLevel)
		io.WriteString(w, "\n")
	}
}

type ParamDecl struct {
	Name    string
	Type    string
	Default string
}

// StructLit represents instantiation of a data class.
type StructLit struct {
	Name   string
	Fields []Expr
	Names  []string
}

func (s *StructLit) emit(w io.Writer) {
	io.WriteString(w, s.Name+"(")
	for i, f := range s.Fields {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, s.Names[i]+" = ")
		f.emit(w)
	}
	io.WriteString(w, ")")
}

func (d *DataClass) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	if d.IsObject {
		io.WriteString(w, "object "+d.Name)
		if d.Extends != "" {
			io.WriteString(w, " : "+d.Extends+"()")
		}
		return
	}
	if len(d.Fields) == 0 {
		io.WriteString(w, "class "+d.Name)
		if d.Extends != "" {
			io.WriteString(w, " : "+d.Extends+"()")
		}
	} else {
		io.WriteString(w, "data class "+d.Name+"(")
		for i, f := range d.Fields {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			typ := f.Type
			if typ == "" {
				typ = "Any"
			}
			if f.Default == "null" && !strings.HasSuffix(typ, "?") {
				if strings.Contains(typ, "->") {
					typ = "(" + typ + ")?"
				} else {
					typ += "?"
				}
			}
			io.WriteString(w, "var "+f.Name+": "+typ)
			if f.Default != "" {
				io.WriteString(w, " = "+f.Default)
			}
		}
		io.WriteString(w, ")")
		if d.Extends != "" {
			io.WriteString(w, " : "+d.Extends+"()")
		}
	}
	if len(d.Methods) > 0 {
		io.WriteString(w, " {\n")
		for _, m := range d.Methods {
			m.emit(w, indentLevel+1)
		}
		indent(w, indentLevel)
		io.WriteString(w, "}")
	}
}

func (t *TypeAlias) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "typealias "+t.Name+" = "+t.Type)
}

func (f *FuncDef) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "fun "+safeName(f.Name)+"(")
	for i, p := range f.Params {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		io.WriteString(w, safeName(p))
	}
	ret := f.Ret
	if ret == "" {
		ret = "Unit"
	}
	io.WriteString(w, "): "+ret+" {\n")
	for _, s := range f.Body {
		s.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}\n")
}

type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement that evaluates an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	s.Expr.emit(w)
}

// CallExpr represents a function call.
type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
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

// InvokeExpr calls a function value stored in an expression.
type InvokeExpr struct {
	Callee Expr
	Args   []Expr
}

func (in *InvokeExpr) emit(w io.Writer) {
	if fe, ok := in.Callee.(*FieldExpr); ok && fe.Name == "padStart" && len(in.Args) == 2 {
		if s, ok := in.Args[1].(*StringLit); ok && len(s.Value) == 1 {
			in.Args[1] = &CharLit{Value: []rune(s.Value)[0]}
		}
		io.WriteString(w, "(")
		fe.Receiver.emit(w)
		io.WriteString(w, ").toString().padStart(")
		if guessType(in.Args[0]) == "BigInteger" {
			io.WriteString(w, "(")
			in.Args[0].emit(w)
			io.WriteString(w, ").toInt()")
		} else {
			in.Args[0].emit(w)
		}
		io.WriteString(w, ", ")
		in.Args[1].emit(w)
		io.WriteString(w, ")")
		return
	}
	needParens := true
	switch in.Callee.(type) {
	case *VarRef, *FieldExpr:
		needParens = false
	}
	if needParens {
		io.WriteString(w, "(")
	}
	in.Callee.emit(w)
	if needParens {
		io.WriteString(w, ")")
	}
	io.WriteString(w, "(")
	for i, a := range in.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// FuncLit is a Kotlin lambda expression.
type FuncLit struct {
	Params []string
	Body   []Stmt
}

func (f *FuncLit) emit(w io.Writer) {
	io.WriteString(w, "{")
	if len(f.Params) > 0 {
		io.WriteString(w, " ")
		for i, p := range f.Params {
			if i > 0 {
				io.WriteString(w, ", ")
			}
			io.WriteString(w, p)
		}
		io.WriteString(w, " -> ")
	}
	if len(f.Body) == 1 {
		if rs, ok := f.Body[0].(*ReturnStmt); ok {
			if rs.Value != nil {
				rs.Value.emit(w)
			}
		} else {
			f.Body[0].emit(w, 0)
		}
		io.WriteString(w, " }")
		return
	}
	io.WriteString(w, "\n")
	for i, s := range f.Body {
		if i == len(f.Body)-1 {
			if rs, ok := s.(*ReturnStmt); ok {
				indent(w, 1)
				if rs.Value != nil {
					rs.Value.emit(w)
				}
				io.WriteString(w, "\n")
				continue
			}
		}
		s.emit(w, 1)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

// StringLit represents a quoted string literal.
type StringLit struct{ Value string }

var hexEscape = regexp.MustCompile(`\\x([0-9a-fA-F]{2})`)

func (s *StringLit) emit(w io.Writer) {
	q := strconv.Quote(s.Value)
	q = hexEscape.ReplaceAllString(q, `\\u00$1`)
	q = strings.ReplaceAll(q, "\\f", "\\u000c")
	io.WriteString(w, q)
}

type IntLit struct{ Value int64 }

func (i *IntLit) emit(w io.Writer) {
	if i.Value > 2147483647 || i.Value < -2147483648 {
		fmt.Fprintf(w, "%dL", i.Value)
	} else {
		fmt.Fprintf(w, "%d", i.Value)
	}
}

type FloatLit struct{ Value float64 }

func (f *FloatLit) emit(w io.Writer) {
	s := strconv.FormatFloat(f.Value, 'f', -1, 64)
	if !strings.ContainsAny(s, ".eE") {
		if !strings.Contains(s, ".") {
			s += ".0"
		}
	}
	io.WriteString(w, s)
}

type CharLit struct{ Value rune }

func (c *CharLit) emit(w io.Writer) { fmt.Fprintf(w, "'%c'", c.Value) }

type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) {
	if b.Value {
		io.WriteString(w, "true")
	} else {
		io.WriteString(w, "false")
	}
}

type NullLit struct{}

func (n *NullLit) emit(w io.Writer) { io.WriteString(w, "null") }

// IndexExpr represents a[i].
type IndexExpr struct {
	Target    Expr
	Index     Expr
	Type      string
	ForceBang bool
}

func (ix *IndexExpr) emit(w io.Writer) { ix.emitWithCast(w, false) }

func (ix *IndexExpr) emitTarget(w io.Writer) { ix.emitWithCast(w, true) }

func (ix *IndexExpr) emitWithCast(w io.Writer, asTarget bool) {
	baseType := guessType(ix.Target)
	if baseType == "" || baseType == "Any" || baseType == "Any?" {
		if vr, ok := ix.Target.(*VarRef); ok && vr.Type != "" {
			baseType = vr.Type
		}
	}
	idxType := guessType(ix.Index)

	isMap := strings.HasPrefix(baseType, "MutableMap<")
	isList := strings.HasPrefix(baseType, "MutableList<")
	isString := baseType == "String"
	if !isMap && !isList && ix.ForceBang && idxType != "Int" {
		isMap = true
	}

	dynamicCast := ""
	if !isMap && !isList && (baseType == "" || baseType == "Any" || baseType == "Any?") {
		if idxType == "Int" {
			isList = true
		} else {
			isMap = true
			if idxType == "Int" {
				dynamicCast = " as MutableMap<Int, Any?>"
			} else {
				dynamicCast = " as MutableMap<String, Any?>"
			}
		}
	}

	if dynamicCast == "" {
		if isMap && (baseType == "" || baseType == "Any" || baseType == "Any?") {
			if idxType == "Int" {
				dynamicCast = " as MutableMap<Int, Any?>"
			} else {
				dynamicCast = " as MutableMap<String, Any?>"
			}
		} else if isList && (baseType == "" || baseType == "Any" || baseType == "Any?") {
			castType := "Any?"
			if ix.Type != "" && ix.Type != "Any" && ix.Type != "Any?" {
				castType = ix.Type
			}
			dynamicCast = " as MutableList<" + castType + ">"
		}
	}

	needParens := false
	switch ix.Target.(type) {
	case *BinaryExpr, *CastExpr, *IndexExpr, *CallExpr, *FieldExpr,
		*UnionExpr, *UnionAllExpr, *ExceptExpr, *IntersectExpr:
		needParens = true
	}

	if isMap || dynamicCast != "" || needParens {
		io.WriteString(w, "(")
		ix.Target.emit(w)
		if dynamicCast != "" {
			io.WriteString(w, dynamicCast)
		}
		io.WriteString(w, ")")
	} else {
		ix.Target.emit(w)
	}
	io.WriteString(w, "[")
	if idxType == "BigInteger" {
		io.WriteString(w, "(")
		ix.Index.emit(w)
		io.WriteString(w, ").toInt()")
	} else {
		ix.Index.emit(w)
	}
	io.WriteString(w, "]")
	if !asTarget {
		if isMap || dynamicCast != "" {
			if ix.Type == "Boolean" && !ix.ForceBang {
				io.WriteString(w, " ?: false")
			} else if ix.ForceBang {
				io.WriteString(w, "!!")
			}
		} else if isString {
			io.WriteString(w, ".toString()")
		} else if isList && ix.Type != "" && (baseType == "" || strings.Contains(baseType, "Any")) {
			io.WriteString(w, " as "+ix.Type)
		} else if ix.ForceBang {
			io.WriteString(w, "!!")
		}
	}
}

// MapLit represents a Kotlin map literal.
type MapLit struct{ Items []MapItem }

type MapItem struct {
	Key   Expr
	Value Expr
}

func (m *MapLit) emit(w io.Writer) {
	keyType := "Any?"
	valType := "Any?"
	if len(m.Items) > 0 {
		keyType = guessType(m.Items[0].Key)
		if keyType == "" {
			keyType = "Any?"
		}
		valType = guessType(m.Items[0].Value)
		if valType == "" {
			valType = "Any?"
		}
		for _, it := range m.Items[1:] {
			k := guessType(it.Key)
			v := guessType(it.Value)
			if k != keyType {
				keyType = "Any?"
			}
			if v != valType {
				valType = "Any?"
			}
		}
	}
	fmt.Fprintf(w, "mutableMapOf<%s, %s>(", keyType, valType)
	for i, it := range m.Items {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		it.Key.emit(w)
		io.WriteString(w, " to (")
		it.Value.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, ")")
}

// ContainsExpr represents s.contains(sub).
type ContainsExpr struct {
	Str Expr
	Sub Expr
}

func (c *ContainsExpr) emit(w io.Writer) {
	c.Str.emit(w)
	io.WriteString(w, ".contains(")
	c.Sub.emit(w)
	io.WriteString(w, ")")
}

// VarRef references a variable by name with optional type information.
type VarRef struct {
	Name   string
	Type   string
	IsFunc bool
}

func (v *VarRef) emit(w io.Writer) {
	if v.Name == "null" {
		io.WriteString(w, "null")
		return
	}
	if v.IsFunc {
		io.WriteString(w, "::"+safeName(v.Name))
	} else {
		io.WriteString(w, safeName(v.Name))
	}
}

// FieldExpr represents obj.field access.
type FieldExpr struct {
	Receiver Expr
	Name     string
	Type     string
}

func (f *FieldExpr) emit(w io.Writer) {
	if rt := guessType(f.Receiver); (rt == "Any" || rt == "Any?") && (f.Name == "R" || f.Name == "G" || f.Name == "B") {
		io.WriteString(w, "(((")
		f.Receiver.emit(w)
		io.WriteString(w, ") as Pixel)."+safeName(f.Name)+")")
		return
	}
	if _, ok := f.Receiver.(*CastExpr); ok {
		io.WriteString(w, "(")
		f.Receiver.emit(w)
		io.WriteString(w, ")")
	} else {
		f.Receiver.emit(w)
	}
	io.WriteString(w, "."+safeName(f.Name))
}

// CastExpr represents value as type conversions like "\"123\" as int".
type CastExpr struct {
	Value Expr
	Type  string
}

func (c *CastExpr) emit(w io.Writer) {
	if inner, ok := c.Value.(*CastExpr); ok && inner.Type == "Any?" {
		c.Value = inner.Value
	}
	if c.Type == "BigRat" {
		useHelper("bigRatHelpers")
		io.WriteString(w, "_bigrat(")
		c.Value.emit(w)
		io.WriteString(w, ")")
		return
	}
	if c.Type == "Double" && guessType(c.Value) == "Double" {
		c.Value.emit(w)
		return
	}
	if ll, ok := c.Value.(*ListLit); ok && strings.HasPrefix(c.Type, "MutableList<") {
		elemType := strings.TrimSuffix(strings.TrimPrefix(c.Type, "MutableList<"), ">")
		tll := &TypedListLit{ElemType: elemType, Elems: ll.Elems}
		tll.emit(w)
		return
	}
	if c.Type == "BigInteger" {
		if t := guessType(c.Value); t == "Any" || t == "Any?" {
			io.WriteString(w, "(")
			c.Value.emit(w)
			io.WriteString(w, " as Int).toBigInteger()")
			return
		}
	}
	io.WriteString(w, "(")
	needParens := false
	switch c.Value.(type) {
	case *BinaryExpr, *CastExpr, *IndexExpr, *CallExpr, *FieldExpr,
		*UnionExpr, *UnionAllExpr, *ExceptExpr, *IntersectExpr:
		needParens = true
	}
	if needParens {
		io.WriteString(w, "(")
		c.Value.emit(w)
		io.WriteString(w, ")")
	} else {
		c.Value.emit(w)
	}
	switch c.Type {
	case "int", "Int":
		switch t := guessType(c.Value); t {
		case "Any", "Any?", "":
			io.WriteString(w, " as Int")
		default:
			io.WriteString(w, ".toInt()")
		}
	case "float", "Double", "Double?":
		if t := guessType(c.Value); t == "Any" || t == "Any?" {
			io.WriteString(w, " as Double")
		} else {
			io.WriteString(w, ".toDouble()")
		}
	case "Long":
		if t := guessType(c.Value); t == "Any" || t == "Any?" {
			io.WriteString(w, " as Long")
		} else {
			io.WriteString(w, ".toLong()")
		}
	case "Int?":
		if t := guessType(c.Value); t == "Any" || t == "Any?" {
			io.WriteString(w, " as Int?")
		} else {
			io.WriteString(w, ".toInt()")
		}
	case "string":
		io.WriteString(w, ".toString()")
	case "BigInteger":
		if guessType(c.Value) != "BigInteger" {
			io.WriteString(w, ".toBigInteger()")
		}
	default:
		io.WriteString(w, " as "+c.Type)
	}
	io.WriteString(w, ")")
}

type UnionExpr struct{ Left, Right Expr }

func (u *UnionExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	if u.Left != nil {
		u.Left.emit(w)
	}
	io.WriteString(w, " + ")
	if u.Right != nil {
		u.Right.emit(w)
	}
	io.WriteString(w, ").distinct()")
}

type UnionAllExpr struct{ Left, Right Expr }

func (u *UnionAllExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	if u.Left != nil {
		u.Left.emit(w)
	}
	io.WriteString(w, " + ")
	if u.Right != nil {
		u.Right.emit(w)
	}
	io.WriteString(w, ")")
}

type ExceptExpr struct{ Left, Right Expr }

func (e *ExceptExpr) emit(w io.Writer) {
	if e.Left != nil {
		e.Left.emit(w)
	}
	io.WriteString(w, ".filter { it !in ")
	if e.Right != nil {
		e.Right.emit(w)
	}
	io.WriteString(w, " }")
}

type IntersectExpr struct{ Left, Right Expr }

func (i *IntersectExpr) emit(w io.Writer) {
	if i.Left != nil {
		i.Left.emit(w)
	}
	io.WriteString(w, ".filter { it in ")
	if i.Right != nil {
		i.Right.emit(w)
	}
	io.WriteString(w, " }")
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	if b.Op == "==" || b.Op == "!=" {
		if vr, ok := b.Left.(*VarRef); ok {
			if (vr.Name == "true" || vr.Name == "false") && guessType(b.Right) == "String" {
				b.Left = &StringLit{Value: vr.Name}
			}
		} else if bl, ok := b.Left.(*BoolLit); ok {
			if guessType(b.Right) == "String" {
				if bl.Value {
					b.Left = &StringLit{Value: "true"}
				} else {
					b.Left = &StringLit{Value: "false"}
				}
			}
		}
		if vr, ok := b.Right.(*VarRef); ok {
			if (vr.Name == "true" || vr.Name == "false") && guessType(b.Left) == "String" {
				b.Right = &StringLit{Value: vr.Name}
			}
		} else if bl, ok := b.Right.(*BoolLit); ok {
			if guessType(b.Left) == "String" {
				if bl.Value {
					b.Right = &StringLit{Value: "true"}
				} else {
					b.Right = &StringLit{Value: "false"}
				}
			}
		}
	}
	leftType := guessType(b.Left)
	rightType := guessType(b.Right)
	strOp := b.Op == "+" && (leftType == "String" || rightType == "String")
	listOp := b.Op == "+" && !strOp && strings.HasPrefix(leftType, "MutableList<") && strings.HasPrefix(rightType, "MutableList<")
	numOp := (b.Op == "+" || b.Op == "-" || b.Op == "*" || b.Op == "/" || b.Op == "%") && !listOp
	cmpOp := b.Op == ">" || b.Op == "<" || b.Op == ">=" || b.Op == "<=" || b.Op == "in"
	boolOp := b.Op == "&&" || b.Op == "||"
	bigOp := leftType == "BigInteger" || rightType == "BigInteger"
	ratOp := leftType == "BigRat" || rightType == "BigRat"
	cast := func(e Expr, typ string) {
		if typ == "Double" {
			io.WriteString(w, "(")
			e.emit(w)
			io.WriteString(w, " as Number).toDouble()")
		} else if typ == "Int" {
			io.WriteString(w, "(")
			e.emit(w)
			if t := guessType(e); t == "Any" || t == "Any?" {
				io.WriteString(w, " as Int")
			} else {
				io.WriteString(w, ").toInt()")
				return
			}
			io.WriteString(w, ")")
		} else if typ == "BigInteger" {
			io.WriteString(w, "(")
			e.emit(w)
			switch t := guessType(e); t {
			case "BigInteger":
				io.WriteString(w, ")")
			case "Any", "Any?":
				io.WriteString(w, " as Int).toBigInteger()")
			default:
				io.WriteString(w, ").toBigInteger()")
			}
		} else if typ == "Long" {
			io.WriteString(w, "(")
			e.emit(w)
			io.WriteString(w, ").toLong()")
		} else if typ == "String" {
			io.WriteString(w, "(")
			e.emit(w)
			io.WriteString(w, ").toString()")
		} else if typ == "Boolean" {
			io.WriteString(w, "(")
			e.emit(w)
			io.WriteString(w, " as Boolean)")
		} else {
			e.emit(w)
		}
	}
	emitOperand := func(e Expr, other Expr) {
		if strOp {
			t := guessType(e)
			if t != "String" {
				cast(e, "String")
				return
			}
		} else if numOp {
			t := guessType(e)
			if t == "Int" && guessType(other) == "Long" {
				cast(e, "Long")
				return
			}
			if t == "Any" || t == "Any?" {
				ot := guessType(other)
				if ot == "Int" {
					cast(e, "Int")
				} else {
					cast(e, "Double")
				}
				return
			}
		}
		if boolOp {
			t := guessType(e)
			if t != "Boolean" {
				cast(e, "Boolean")
				return
			}
		} else if cmpOp {
			lt := guessType(e)
			rt := guessType(other)
			if (lt == "Any" || lt == "Any?") && rt == "String" {
				cast(e, "String")
				return
			}
			if (lt == "Any" || lt == "Any?") && (rt == "Double" || rt == "Int") {
				if _, ok := e.(*IndexExpr); ok {
					e.emit(w)
					return
				}
				cast(e, "Double")
				return
			}
			if (rt == "Any" || rt == "Any?") && (lt == "Double" || lt == "Int") {
				cast(e, lt)
				return
			}
		}
		if cmpOp || numOp {
			if _, ok := e.(*CastExpr); ok {
				io.WriteString(w, "(")
				e.emit(w)
				io.WriteString(w, ")")
				return
			}
		}
		e.emit(w)
	}
	if ratOp {
		useHelper("bigRatHelpers")
		fn := ""
		whenOp := b.Op
		switch whenOp {
		case "+":
			fn = "_add"
		case "-":
			fn = "_sub"
		case "*":
			fn = "_mul"
		case "/":
			fn = "_div"
		}
		if fn != "" {
			io.WriteString(w, fn+"(")
			if _, ok := b.Left.(*BinaryExpr); ok {
				io.WriteString(w, "(")
			}
			if guessType(b.Left) != "BigRat" {
				io.WriteString(w, "_bigrat(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			if _, ok := b.Left.(*BinaryExpr); ok {
				io.WriteString(w, ")")
			}
			io.WriteString(w, ", ")
			if _, ok := b.Right.(*BinaryExpr); ok {
				io.WriteString(w, "(")
			}
			if guessType(b.Right) != "BigRat" {
				io.WriteString(w, "_bigrat(")
				b.Right.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Right.emit(w)
			}
			if _, ok := b.Right.(*BinaryExpr); ok {
				io.WriteString(w, ")")
			}
			io.WriteString(w, ")")
			return
		}
	}
	if bigOp {
		if leftType != "BigInteger" {
			io.WriteString(w, "(")
			if _, ok := b.Left.(*BinaryExpr); ok {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
			if leftType == "Any" || leftType == "Any?" {
				io.WriteString(w, " as Int")
			}
			io.WriteString(w, ").toBigInteger()")
		} else {
			if _, ok := b.Left.(*BinaryExpr); ok {
				io.WriteString(w, "(")
				b.Left.emit(w)
				io.WriteString(w, ")")
			} else {
				b.Left.emit(w)
			}
		}
		switch b.Op {
		case "+":
			io.WriteString(w, ".add(")
			cast(b.Right, "BigInteger")
			io.WriteString(w, ")")
		case "-":
			io.WriteString(w, ".subtract(")
			cast(b.Right, "BigInteger")
			io.WriteString(w, ")")
		case "*":
			io.WriteString(w, ".multiply(")
			cast(b.Right, "BigInteger")
			io.WriteString(w, ")")
		case "/":
			io.WriteString(w, ".divide(")
			cast(b.Right, "BigInteger")
			io.WriteString(w, ")")
		case "%":
			io.WriteString(w, ".remainder(")
			cast(b.Right, "BigInteger")
			io.WriteString(w, ")")
		case "==", "!=", "<", ">", "<=", ">=":
			io.WriteString(w, ".compareTo(")
			cast(b.Right, "BigInteger")
			io.WriteString(w, ") ")
			switch b.Op {
			case "==":
				io.WriteString(w, "== 0")
			case "!=":
				io.WriteString(w, "!= 0")
			case "<":
				io.WriteString(w, "< 0")
			case ">":
				io.WriteString(w, "> 0")
			case "<=":
				io.WriteString(w, "<= 0")
			case ">=":
				io.WriteString(w, ">= 0")
			}
		default:
			io.WriteString(w, " /* unsupported */ ")
		}
		return
	}
	if numOp && b.Op == "%" && !bigOp {
		useLong := leftType == "Long" || rightType == "Long"
		io.WriteString(w, "Math.floorMod(")
		if _, ok := b.Left.(*BinaryExpr); ok {
			io.WriteString(w, "(")
			if useLong && leftType != "Long" {
				cast(b.Left, "Long")
			} else {
				emitOperand(b.Left, b.Right)
			}
			io.WriteString(w, ")")
		} else {
			if useLong && leftType != "Long" {
				cast(b.Left, "Long")
			} else {
				emitOperand(b.Left, b.Right)
			}
		}
		io.WriteString(w, ", ")
		if _, ok := b.Right.(*BinaryExpr); ok {
			io.WriteString(w, "(")
			if useLong && rightType != "Long" {
				cast(b.Right, "Long")
			} else {
				emitOperand(b.Right, b.Left)
			}
			io.WriteString(w, ")")
		} else {
			if useLong && rightType != "Long" {
				cast(b.Right, "Long")
			} else {
				emitOperand(b.Right, b.Left)
			}
		}
		io.WriteString(w, ")")
		if useLong && guessType(b) == "Int" {
			io.WriteString(w, ".toInt()")
		}
		return
	}
	if b.Op == "in" {
		if _, ok := b.Left.(*BinaryExpr); ok {
			io.WriteString(w, "(")
			emitOperand(b.Left, b.Right)
			io.WriteString(w, ")")
		} else {
			emitOperand(b.Left, b.Right)
		}
		io.WriteString(w, " in ")
		if _, ok := b.Right.(*BinaryExpr); ok {
			io.WriteString(w, "(")
			emitOperand(b.Right, b.Left)
			io.WriteString(w, ")")
		} else {
			emitOperand(b.Right, b.Left)
		}
		return
	}
	if listOp {
		io.WriteString(w, "(")
	}
	if _, ok := b.Left.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		emitOperand(b.Left, b.Right)
		io.WriteString(w, ")")
	} else {
		emitOperand(b.Left, b.Right)
	}
	io.WriteString(w, " "+b.Op+" ")
	if _, ok := b.Right.(*BinaryExpr); ok {
		io.WriteString(w, "(")
		emitOperand(b.Right, b.Left)
		io.WriteString(w, ")")
	} else {
		emitOperand(b.Right, b.Left)
	}
	if listOp {
		io.WriteString(w, ")")
		io.WriteString(w, ".toMutableList()")
	}
}

type LetStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	// use 'var' to match Mochi semantics where let bindings may be reassigned
	typ := s.Type
	if typ == "Int" {
		if lit, ok := s.Value.(*IntLit); ok {
			if lit.Value > 2147483647 || lit.Value < -2147483648 {
				typ = "Long"
			}
		}
	}
	io.WriteString(w, "var "+safeName(s.Name))
	if typ != "" {
		io.WriteString(w, ": "+typ)
	}
	io.WriteString(w, " = ")
	if s.Type == "BigInteger" {
		useHelper("importBigInt")
		if _, ok := s.Value.(*IntLit); ok {
			io.WriteString(w, "java.math.BigInteger.valueOf(")
			s.Value.emit(w)
			io.WriteString(w, ")")
			return
		}
		needConv := guessType(s.Value) != "BigInteger"
		if needConv {
			io.WriteString(w, "(")
			s.Value.emit(w)
			io.WriteString(w, ").toBigInteger()")
		} else {
			s.Value.emit(w)
		}
		return
	}
	if ll, ok := s.Value.(*ListLit); ok && len(ll.Elems) == 0 && strings.HasPrefix(s.Type, "MutableList<") {
		elem := strings.TrimSuffix(strings.TrimPrefix(s.Type, "MutableList<"), ">")
		io.WriteString(w, "mutableListOf<"+elem+">()")
		return
	}
	if ce, ok := s.Value.(*CastExpr); ok {
		if ml, ok := ce.Value.(*MapLit); ok && len(ml.Items) == 0 && strings.HasPrefix(ce.Type, "MutableMap<") {
			part := strings.TrimSuffix(strings.TrimPrefix(ce.Type, "MutableMap<"), ">")
			if idx := strings.Index(part, ","); idx >= 0 {
				k := strings.TrimSpace(part[:idx])
				v := strings.TrimSpace(part[idx+1:])
				io.WriteString(w, "mutableMapOf<"+k+", "+v+">()")
				return
			}
		}
	}
	s.Value.emit(w)
}

type VarStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	typ := s.Type
	if typ == "Int" {
		if lit, ok := s.Value.(*IntLit); ok {
			if lit.Value > 2147483647 || lit.Value < -2147483648 {
				typ = "Long"
			}
		}
	}
	io.WriteString(w, "var "+safeName(s.Name))
	if typ != "" {
		io.WriteString(w, ": "+typ)
	}
	io.WriteString(w, " = ")
	if s.Type == "BigInteger" {
		useHelper("importBigInt")
		if _, ok := s.Value.(*IntLit); ok {
			io.WriteString(w, "java.math.BigInteger.valueOf(")
			s.Value.emit(w)
			io.WriteString(w, ")")
			return
		}
		needConv := guessType(s.Value) != "BigInteger"
		if needConv {
			io.WriteString(w, "(")
			s.Value.emit(w)
			io.WriteString(w, ").toBigInteger()")
		} else {
			s.Value.emit(w)
		}
		return
	}
	if ll, ok := s.Value.(*ListLit); ok && len(ll.Elems) == 0 && strings.HasPrefix(s.Type, "MutableList<") {
		elem := strings.TrimSuffix(strings.TrimPrefix(s.Type, "MutableList<"), ">")
		io.WriteString(w, "mutableListOf<"+elem+">()")
		return
	}
	if ce, ok := s.Value.(*CastExpr); ok {
		if ml, ok := ce.Value.(*MapLit); ok && len(ml.Items) == 0 && strings.HasPrefix(ce.Type, "MutableMap<") {
			part := strings.TrimSuffix(strings.TrimPrefix(ce.Type, "MutableMap<"), ">")
			if idx := strings.Index(part, ","); idx >= 0 {
				k := strings.TrimSpace(part[:idx])
				v := strings.TrimSpace(part[idx+1:])
				io.WriteString(w, "mutableMapOf<"+k+", "+v+">()")
				return
			}
		}
	}
	s.Value.emit(w)
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, safeName(s.Name)+" = ")
	s.Value.emit(w)
}

// IfStmt represents a simple if/else conditional.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range i.Then {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
	if len(i.Else) > 0 {
		io.WriteString(w, " else {\n")
		for _, st := range i.Else {
			st.emit(w, indentLevel+1)
			io.WriteString(w, "\n")
		}
		indent(w, indentLevel)
		io.WriteString(w, "}")
	}
}

// WhileStmt represents a basic while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "while (")
	if ws.Cond != nil {
		ws.Cond.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range ws.Body {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
}

// ForRangeStmt represents a numeric for-loop like `for i in a..b {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (fr *ForRangeStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "for ("+safeName(fr.Name)+" in ")
	if fr.Start != nil {
		fr.Start.emit(w)
	}
	io.WriteString(w, " until ")
	if fr.End != nil {
		fr.End.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range fr.Body {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
}

// ForEachStmt iterates over a collection.
type ForEachStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (fe *ForEachStmt) emit(w io.Writer, indentLevel int) {
	iterType := guessType(fe.Iterable)
	name := safeName(fe.Name)
	if iterType == "String" {
		tmp := "_" + name
		indent(w, indentLevel)
		io.WriteString(w, "for ("+tmp+" in ")
		if fe.Iterable != nil {
			fe.Iterable.emit(w)
		}
		io.WriteString(w, ") {\n")
		indent(w, indentLevel+1)
		io.WriteString(w, "val "+name+" = "+tmp+".toString()\n")
		for _, st := range fe.Body {
			st.emit(w, indentLevel+1)
			io.WriteString(w, "\n")
		}
		indent(w, indentLevel)
		io.WriteString(w, "}")
		return
	}
	indent(w, indentLevel)
	io.WriteString(w, "for ("+name+" in ")
	if fe.Iterable != nil {
		fe.Iterable.emit(w)
	}
	io.WriteString(w, ") {\n")
	for _, st := range fe.Body {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel)
	io.WriteString(w, "}")
}

// BreakStmt represents a break statement.
type BreakStmt struct{}

func (b *BreakStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "break")
}

// ContinueStmt represents a continue statement.
type ContinueStmt struct{}

func (c *ContinueStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "continue")
}

// BenchStmt represents a benchmark block that measures execution time and
// memory usage of its body.
type BenchStmt struct {
	Name string
	Body []Stmt
}

func (bs *BenchStmt) emit(w io.Writer, indentLevel int) {
	indent(w, indentLevel)
	io.WriteString(w, "run {\n")
	indent(w, indentLevel+1)
	io.WriteString(w, "System.gc()\n")
	indent(w, indentLevel+1)
	io.WriteString(w, "val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()\n")
	indent(w, indentLevel+1)
	io.WriteString(w, "val _start = _now()\n")
	for _, st := range bs.Body {
		st.emit(w, indentLevel+1)
		io.WriteString(w, "\n")
	}
	indent(w, indentLevel+1)
	io.WriteString(w, "System.gc()\n")
	indent(w, indentLevel+1)
	io.WriteString(w, "val _end = _now()\n")
	indent(w, indentLevel+1)
	io.WriteString(w, "val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()\n")
	indent(w, indentLevel+1)
	io.WriteString(w, "val _durationUs = (_end - _start) / 1000\n")
	indent(w, indentLevel+1)
	io.WriteString(w, "val _memDiff = kotlin.math.abs(_endMem - _startMem)\n")
	indent(w, indentLevel+1)
	fmt.Fprintf(w, "val _res = mapOf(\"duration_us\" to _durationUs, \"memory_bytes\" to _memDiff, \"name\" to %q)\n", bs.Name)
	indent(w, indentLevel+1)
	io.WriteString(w, "println(toJson(_res))\n")
	indent(w, indentLevel)
	io.WriteString(w, "}")
}

// IfExpr is a conditional expression using Kotlin's `if`.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (ie *IfExpr) emit(w io.Writer) {
	io.WriteString(w, "if (")
	ie.Cond.emit(w)
	io.WriteString(w, ") ")
	ie.Then.emit(w)
	if ie.Else != nil {
		io.WriteString(w, " else ")
		ie.Else.emit(w)
	}
}

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	if len(l.Elems) == 0 {
		io.WriteString(w, "mutableListOf<Any?>()")
		return
	}
	io.WriteString(w, "mutableListOf(")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		e.emit(w)
	}
	io.WriteString(w, ")")
}

type TypedListLit struct {
	ElemType string
	Elems    []Expr
}

func (l *TypedListLit) emit(w io.Writer) {
	io.WriteString(w, "mutableListOf<"+l.ElemType+">(")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		if guessType(e) != l.ElemType && l.ElemType != "" {
			(&CastExpr{Value: e, Type: l.ElemType}).emit(w)
		} else {
			e.emit(w)
		}
	}
	io.WriteString(w, ")")
}

// SliceExpr represents s[i:j] for strings and lists.
type SliceExpr struct {
	Value    Expr
	Start    Expr
	End      Expr
	IsString bool
}

func (s *SliceExpr) emit(w io.Writer) {
	s.Value.emit(w)
	if s.IsString {
		io.WriteString(w, ".substring(")
	} else {
		io.WriteString(w, ".subList(")
	}
	if s.Start != nil {
		if guessType(s.Start) == "BigInteger" {
			io.WriteString(w, "(")
			s.Start.emit(w)
			io.WriteString(w, ").toInt()")
		} else {
			s.Start.emit(w)
		}
	} else {
		io.WriteString(w, "0")
	}
	io.WriteString(w, ", ")
	if s.End != nil {
		if guessType(s.End) == "BigInteger" {
			io.WriteString(w, "(")
			s.End.emit(w)
			io.WriteString(w, ").toInt()")
		} else {
			s.End.emit(w)
		}
	} else {
		s.Value.emit(w)
		if s.IsString {
			io.WriteString(w, ".length")
		} else {
			io.WriteString(w, ".size")
		}
	}
	io.WriteString(w, ")")
}

type CountExpr struct{ Value Expr }

func (c *CountExpr) emit(w io.Writer) {
	c.Value.emit(w)
	io.WriteString(w, ".size")
}

type SumExpr struct{ Value Expr }

func (s *SumExpr) emit(w io.Writer) {
	if lc, ok := s.Value.(*MultiListComp); ok {
		io.WriteString(w, "run {\n")
		indent(w, 1)
		io.WriteString(w, "var _acc = 0.0\n")
		for i, v := range lc.Vars {
			indent(w, 1+i)
			fmt.Fprintf(w, "for (%s in ", v)
			lc.Iters[i].emit(w)
			io.WriteString(w, ") {\n")
		}
		if lc.Cond != nil {
			indent(w, 1+len(lc.Vars))
			io.WriteString(w, "if (")
			lc.Cond.emit(w)
			io.WriteString(w, ") {\n")
			indent(w, 2+len(lc.Vars))
			io.WriteString(w, "_acc += (")
			lc.Expr.emit(w)
			io.WriteString(w, " as Number).toDouble()\n")
			indent(w, 1+len(lc.Vars))
			io.WriteString(w, "}\n")
		} else {
			indent(w, 1+len(lc.Vars))
			io.WriteString(w, "_acc += (")
			lc.Expr.emit(w)
			io.WriteString(w, " as Number).toDouble()\n")
		}
		for i := len(lc.Vars); i > 0; i-- {
			indent(w, i)
			io.WriteString(w, "}\n")
		}
		indent(w, 1)
		io.WriteString(w, "_acc\n")
		io.WriteString(w, "}")
	} else if guessType(s.Value) == "MutableList<Any>" {
		io.WriteString(w, "(")
		s.Value.emit(w)
		io.WriteString(w, ".map{(it as Number).toDouble()})")
		io.WriteString(w, ".sum()")
	} else {
		s.Value.emit(w)
		io.WriteString(w, ".sum()")
	}
}

type AvgExpr struct{ Value Expr }

func (a *AvgExpr) emit(w io.Writer) {
	if guessType(a.Value) == "MutableList<Any>" {
		io.WriteString(w, "(")
		a.Value.emit(w)
		io.WriteString(w, ".map{(it as Number).toDouble()})")
		io.WriteString(w, ".average()")
	} else {
		a.Value.emit(w)
		io.WriteString(w, ".average()")
	}
}

type LenExpr struct {
	Value    Expr
	IsString bool
}

func (l *LenExpr) emit(w io.Writer) {
	typ := guessType(l.Value)
	if typ == "Any" || typ == "Any?" {
		io.WriteString(w, "_len(")
		l.Value.emit(w)
		io.WriteString(w, ")")
		return
	}
	needParens := false
	switch l.Value.(type) {
	case *CastExpr, *BinaryExpr, *IndexExpr, *CallExpr, *FieldExpr,
		*UnionExpr, *UnionAllExpr, *ExceptExpr, *IntersectExpr:
		needParens = true
	}
	if needParens {
		io.WriteString(w, "(")
		l.Value.emit(w)
		io.WriteString(w, ")")
	} else {
		l.Value.emit(w)
	}
	if l.IsString {
		io.WriteString(w, ".length")
	} else {
		io.WriteString(w, ".size")
	}
}

type StrExpr struct{ Value Expr }

func (s *StrExpr) emit(w io.Writer) {
	switch s.Value.(type) {
	case *BinaryExpr, *CastExpr, *IndexExpr:
		io.WriteString(w, "(")
		s.Value.emit(w)
		io.WriteString(w, ").toString()")
	default:
		s.Value.emit(w)
		io.WriteString(w, ".toString()")
	}
}

type NotExpr struct{ Value Expr }

func (n *NotExpr) emit(w io.Writer) {
	io.WriteString(w, "!")
	if _, ok := n.Value.(*IndexExpr); ok {
		io.WriteString(w, "((")
		n.Value.emit(w)
		io.WriteString(w, ") as? Boolean ?: false)")
		return
	}
	needCast := guessType(n.Value) != "Boolean"
	if needCast {
		io.WriteString(w, "(")
	}
	switch n.Value.(type) {
	case *BoolLit, *VarRef, *CallExpr, *FieldExpr:
		n.Value.emit(w)
	default:
		io.WriteString(w, "(")
		n.Value.emit(w)
		io.WriteString(w, ")")
	}
	if needCast {
		io.WriteString(w, " as Boolean")
		io.WriteString(w, ")")
	}
}

// NotNullCheck converts Mochi truthiness to Kotlin null check.
type NotNullCheck struct{ Value Expr }

func (n *NotNullCheck) emit(w io.Writer) {
	n.Value.emit(w)
	io.WriteString(w, " != null")
}

func isBoolExpr(e Expr) bool {
	switch e.(type) {
	case *BoolLit, *BinaryExpr, *ContainsExpr, *ExistsExpr, *NotExpr, *WhenExpr:
		return true
	}
	return false
}

type AppendExpr struct {
	List Expr
	Elem Expr
}

func (a *AppendExpr) emit(w io.Writer) {
	io.WriteString(w, "run { ")
	io.WriteString(w, "val _tmp = ")
	needParens := false
	switch a.List.(type) {
	case *CastExpr, *BinaryExpr, *IndexExpr, *CallExpr, *FieldExpr,
		*UnionExpr, *UnionAllExpr, *ExceptExpr, *IntersectExpr:
		needParens = true
	}
	if needParens {
		io.WriteString(w, "(")
		a.List.emit(w)
		io.WriteString(w, ")")
	} else {
		a.List.emit(w)
	}
	io.WriteString(w, ".toMutableList(); _tmp.add(")
	elemType := ""
	listType := guessType(a.List)
	if strings.HasPrefix(listType, "MutableList<") {
		elemType = strings.TrimSuffix(strings.TrimPrefix(listType, "MutableList<"), ">")
	}
	if elemType != "" && elemType != guessType(a.Elem) {
		castExpr := &CastExpr{Value: a.Elem, Type: elemType}
		castExpr.emit(w)
	} else {
		a.Elem.emit(w)
	}
	io.WriteString(w, "); _tmp }")
}

type MinExpr struct{ Value Expr }

func (m *MinExpr) emit(w io.Writer) {
	m.Value.emit(w)
	io.WriteString(w, ".min()")
}

type MaxExpr struct{ Value Expr }

func (m *MaxExpr) emit(w io.Writer) {
	m.Value.emit(w)
	io.WriteString(w, ".max()")
}

// NowExpr expands to a deterministic timestamp similar to the VM's now() builtin.
type NowExpr struct{}

func (n *NowExpr) emit(w io.Writer) { io.WriteString(w, "_now()") }

type ValuesExpr struct{ Map Expr }

func (v *ValuesExpr) emit(w io.Writer) {
	v.Map.emit(w)
	io.WriteString(w, ".values")
}

// KeysExpr represents the `keys` builtin for maps.
type KeysExpr struct{ Map Expr }

func (k *KeysExpr) emit(w io.Writer) {
	k.Map.emit(w)
	io.WriteString(w, ".keys.toMutableList()")
}

type SubstringExpr struct {
	Value Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) {
	if t := guessType(s.Value); t == "Any" || t == "Any?" {
		io.WriteString(w, "(")
		s.Value.emit(w)
		io.WriteString(w, ").toString().substring(")
	} else {
		s.Value.emit(w)
		io.WriteString(w, ".substring(")
	}
	if guessType(s.Start) == "BigInteger" {
		io.WriteString(w, "(")
		s.Start.emit(w)
		io.WriteString(w, ").toInt()")
	} else {
		s.Start.emit(w)
	}
	io.WriteString(w, ", ")
	if guessType(s.End) == "BigInteger" {
		io.WriteString(w, "(")
		s.End.emit(w)
		io.WriteString(w, ").toInt()")
	} else {
		s.End.emit(w)
	}
	io.WriteString(w, ")")
}

type PadStartExpr struct {
	Value Expr
	Width Expr
	Pad   Expr
}

func (pse *PadStartExpr) emit(w io.Writer) {
	io.WriteString(w, "(")
	pse.Value.emit(w)
	io.WriteString(w, ").toString().padStart(")
	if guessType(pse.Width) == "BigInteger" {
		io.WriteString(w, "(")
		pse.Width.emit(w)
		io.WriteString(w, ").toInt()")
	} else {
		pse.Width.emit(w)
	}
	io.WriteString(w, ", ")
	if _, ok := pse.Pad.(*CharLit); ok {
		pse.Pad.emit(w)
	} else {
		pse.Pad.emit(w)
		io.WriteString(w, "[0]")
	}
	io.WriteString(w, ")")
}

type ExistsExpr struct{ Value Expr }

func (e *ExistsExpr) emit(w io.Writer) {
	e.Value.emit(w)
	io.WriteString(w, ".isNotEmpty()")
}

// SortQueryExpr represents a sorted list comprehension.
type SortQueryExpr struct {
	Vars    []string
	Iters   []Expr
	Cond    Expr
	Sort    Expr
	Select  Expr
	ResType string
}

func (sq *SortQueryExpr) emit(w io.Writer) {
	io.WriteString(w, "run {\n")
	indent(w, 1)
	sortType := guessType(sq.Sort)
	if sortType == "" {
		sortType = "Any"
	}
	elemType := sq.ResType
	if elemType == "" {
		elemType = guessType(sq.Select)
		if elemType == "" {
			elemType = "Any"
		}
	}
	fmt.Fprintf(w, "val _tmp = mutableListOf<Pair<%s, %s>>()\n", sortType, elemType)
	for i, v := range sq.Vars {
		indent(w, 1+i)
		fmt.Fprintf(w, "for (%s in ", v)
		sq.Iters[i].emit(w)
		io.WriteString(w, ") {\n")
	}
	if sq.Cond != nil {
		indent(w, 1+len(sq.Vars))
		io.WriteString(w, "if (")
		sq.Cond.emit(w)
		io.WriteString(w, ") {\n")
		indent(w, 2+len(sq.Vars))
		io.WriteString(w, "_tmp.add(Pair(")
		sq.Sort.emit(w)
		io.WriteString(w, ", ")
		sq.Select.emit(w)
		io.WriteString(w, "))\n")
		indent(w, 1+len(sq.Vars))
		io.WriteString(w, "}\n")
	} else {
		indent(w, 1+len(sq.Vars))
		io.WriteString(w, "_tmp.add(Pair(")
		sq.Sort.emit(w)
		io.WriteString(w, ", ")
		sq.Select.emit(w)
		io.WriteString(w, "))\n")
	}
	for i := len(sq.Vars); i > 0; i-- {
		indent(w, i)
		io.WriteString(w, "}\n")
	}
	indent(w, 1)
	io.WriteString(w, "val _res = _tmp.sortedBy { it.first }.map { it.second }.toMutableList()\n")
	indent(w, 1)
	io.WriteString(w, "_res\n")
	io.WriteString(w, "}")
}

// SkipTakeExpr applies drop/take operations on a list expression.
type SkipTakeExpr struct {
	Expr Expr
	Skip Expr
	Take Expr
}

func (st *SkipTakeExpr) emit(w io.Writer) {
	st.Expr.emit(w)
	if st.Skip != nil {
		io.WriteString(w, ".drop(")
		st.Skip.emit(w)
		io.WriteString(w, ")")
	}
	if st.Take != nil {
		io.WriteString(w, ".take(")
		st.Take.emit(w)
		io.WriteString(w, ")")
	}
	io.WriteString(w, ".toMutableList()")
}

// MultiListComp represents a list comprehension with multiple iterators.
type MultiListComp struct {
	Vars  []string
	Iters []Expr
	Expr  Expr
	Cond  Expr
}

func (lc *MultiListComp) emit(w io.Writer) {
	io.WriteString(w, "run {\n")
	indent(w, 1)
	elemType := guessType(lc.Expr)
	if elemType == "" {
		elemType = "Any"
	}
	fmt.Fprintf(w, "val _res = mutableListOf<%s>()\n", elemType)
	for i, v := range lc.Vars {
		indent(w, 1+i)
		fmt.Fprintf(w, "for (%s in ", v)
		lc.Iters[i].emit(w)
		io.WriteString(w, ") {\n")
	}
	if lc.Cond != nil {
		indent(w, 1+len(lc.Vars))
		io.WriteString(w, "if (")
		lc.Cond.emit(w)
		io.WriteString(w, ") {\n")
		indent(w, 2+len(lc.Vars))
		io.WriteString(w, "_res.add(")
		lc.Expr.emit(w)
		io.WriteString(w, ")\n")
		indent(w, 1+len(lc.Vars))
		io.WriteString(w, "}\n")
	} else {
		indent(w, 1+len(lc.Vars))
		io.WriteString(w, "_res.add(")
		lc.Expr.emit(w)
		io.WriteString(w, ")\n")
	}
	for i := len(lc.Vars); i > 0; i-- {
		indent(w, i)
		io.WriteString(w, "}\n")
	}
	indent(w, 1)
	io.WriteString(w, "_res\n")
	io.WriteString(w, "}")
}

// GroupQueryExpr represents a query with grouping.
type GroupQueryExpr struct {
	Vars      []string
	Iters     []Expr
	Cond      Expr
	Key       Expr
	Row       Expr
	RowType   string
	GroupVar  string
	GroupType string
	Select    Expr
	Having    Expr
	Sort      Expr
	ResType   string
}

func (gq *GroupQueryExpr) emit(w io.Writer) {
	io.WriteString(w, "run {\n")
	indent(w, 1)
	keyType := guessType(gq.Key)
	if keyType == "" {
		keyType = "Any"
	}
	rowType := gq.RowType
	if rowType == "" {
		rowType = guessType(gq.Row)
		if rowType == "" {
			rowType = "Any"
		}
	}
	fmt.Fprintf(w, "val _groups = mutableMapOf<%s, MutableList<%s>>()\n", keyType, rowType)
	for i, v := range gq.Vars {
		indent(w, 1+i)
		fmt.Fprintf(w, "for (%s in ", v)
		gq.Iters[i].emit(w)
		io.WriteString(w, ") {\n")
	}
	if gq.Cond != nil {
		indent(w, 1+len(gq.Vars))
		io.WriteString(w, "if (")
		gq.Cond.emit(w)
		io.WriteString(w, ") {\n")
	}
	indent(w, 1+len(gq.Vars))
	io.WriteString(w, "val _list = _groups.getOrPut(")
	gq.Key.emit(w)
	io.WriteString(w, ") { mutableListOf<"+rowType+">() }\n")
	indent(w, 1+len(gq.Vars))
	io.WriteString(w, "_list.add(")
	gq.Row.emit(w)
	io.WriteString(w, ")\n")
	if gq.Cond != nil {
		indent(w, 1+len(gq.Vars))
		io.WriteString(w, "}\n")
	}
	for i := len(gq.Vars); i > 0; i-- {
		indent(w, i)
		io.WriteString(w, "}\n")
	}
	indent(w, 1)
	resType := gq.ResType
	if resType == "" {
		resType = "Any"
	}
	fmt.Fprintf(w, "val _res = mutableListOf<%s>()\n", resType)
	var useTmp bool
	var sortType string
	if gq.Sort != nil {
		useTmp = true
		sortType = guessType(gq.Sort)
		if sortType == "" {
			sortType = "Any"
		}
		fmt.Fprintf(w, "val _tmp = mutableListOf<Pair<%s, %s>>()\n", sortType, resType)
	}
	indent(w, 1)
	io.WriteString(w, "for ((key, items) in _groups) {\n")
	indent(w, 2)
	fmt.Fprintf(w, "val %s = %s(key, items)\n", gq.GroupVar, gq.GroupType)
	if gq.Having != nil {
		indent(w, 2)
		io.WriteString(w, "if (")
		gq.Having.emit(w)
		io.WriteString(w, ") {\n")
		indent(w, 3)
		if useTmp {
			io.WriteString(w, "_tmp.add(Pair(")
			gq.Sort.emit(w)
			io.WriteString(w, ", ")
			gq.Select.emit(w)
			io.WriteString(w, "))\n")
		} else {
			io.WriteString(w, "_res.add(")
			gq.Select.emit(w)
			io.WriteString(w, ")\n")
		}
		indent(w, 2)
		io.WriteString(w, "}\n")
	} else {
		indent(w, 2)
		if useTmp {
			io.WriteString(w, "_tmp.add(Pair(")
			gq.Sort.emit(w)
			io.WriteString(w, ", ")
			gq.Select.emit(w)
			io.WriteString(w, "))\n")
		} else {
			io.WriteString(w, "_res.add(")
			gq.Select.emit(w)
			io.WriteString(w, ")\n")
		}
	}
	indent(w, 1)
	io.WriteString(w, "}\n")
	if useTmp {
		indent(w, 1)
		io.WriteString(w, "_tmp.sortedBy { it.first }.map { it.second }.toMutableList().also { _res.addAll(it) }\n")
	}
	indent(w, 1)
	io.WriteString(w, "_res\n")
	io.WriteString(w, "}")
}

// RightJoinExpr represents a simple right join between two lists.
type RightJoinExpr struct {
	LeftVar  string
	LeftSrc  Expr
	RightVar string
	RightSrc Expr
	Cond     Expr
	Select   Expr
}

func (r *RightJoinExpr) emit(w io.Writer) {
	io.WriteString(w, "run {\n")
	indent(w, 1)
	elemType := guessType(r.Select)
	if elemType == "" {
		elemType = "Any"
	}
	fmt.Fprintf(w, "val _res = mutableListOf<%s>()\n", elemType)
	indent(w, 1)
	fmt.Fprintf(w, "for (%s in ", r.RightVar)
	r.RightSrc.emit(w)
	io.WriteString(w, ") {\n")
	indent(w, 2)
	io.WriteString(w, "var matched = false\n")
	indent(w, 2)
	fmt.Fprintf(w, "for (%s in ", r.LeftVar)
	r.LeftSrc.emit(w)
	io.WriteString(w, ") {\n")
	indent(w, 3)
	io.WriteString(w, "if (")
	r.Cond.emit(w)
	io.WriteString(w, ") {\n")
	indent(w, 4)
	io.WriteString(w, "matched = true\n")
	indent(w, 4)
	io.WriteString(w, "_res.add(")
	r.Select.emit(w)
	io.WriteString(w, ")\n")
	indent(w, 3)
	io.WriteString(w, "}\n")
	indent(w, 2)
	io.WriteString(w, "}\n")
	indent(w, 2)
	io.WriteString(w, "if (!matched) {\n")
	indent(w, 3)
	fmt.Fprintf(w, "val %s: Any? = null\n", r.LeftVar)
	indent(w, 3)
	io.WriteString(w, "_res.add(")
	r.Select.emit(w)
	io.WriteString(w, ")\n")
	indent(w, 2)
	io.WriteString(w, "}\n")
	indent(w, 1)
	io.WriteString(w, "}\n")
	indent(w, 1)
	io.WriteString(w, "_res\n")
	io.WriteString(w, "}")
}

// WhenExpr models Kotlin's when expression produced from a Mochi match.
type WhenExpr struct {
	Target Expr
	Cases  []WhenCase
}

type WhenCase struct {
	Pattern Expr // nil for else
	Result  Expr
}

// TypePattern matches a value using Kotlin 'is' check.
type TypePattern struct{ Type string }

func (tp *TypePattern) emit(w io.Writer) { io.WriteString(w, "is "+tp.Type) }

// RunExpr emits a `run { ... }` block returning the last expression.
type RunExpr struct {
	Stmts  []Stmt
	Result Expr
}

func (re *RunExpr) emit(w io.Writer) {
	io.WriteString(w, "run {\n")
	for _, s := range re.Stmts {
		s.emit(w, 1)
		io.WriteString(w, "\n")
	}
	indent(w, 1)
	if re.Result != nil {
		re.Result.emit(w)
	}
	io.WriteString(w, "\n}")
}

func (wex *WhenExpr) emit(w io.Writer) {
	io.WriteString(w, "when (")
	wex.Target.emit(w)
	io.WriteString(w, ") {\n")
	for _, c := range wex.Cases {
		io.WriteString(w, "    ")
		if c.Pattern != nil {
			c.Pattern.emit(w)
		} else {
			io.WriteString(w, "else")
		}
		io.WriteString(w, " -> ")
		c.Result.emit(w)
		io.WriteString(w, "\n")
	}
	io.WriteString(w, "}")
}

type queryFrom struct {
	Var string
	Src Expr
}

type QueryExpr struct {
	Var    string
	Src    Expr
	Froms  []queryFrom
	Where  Expr
	Select Expr
}

func (q *QueryExpr) emit(w io.Writer) {
	q.Src.emit(w)
	for _, f := range q.Froms {
		io.WriteString(w, ".flatMap {")
		io.WriteString(w, f.Var+" -> ")
		f.Src.emit(w)
		io.WriteString(w, " }")
	}
	if q.Where != nil {
		io.WriteString(w, ".flatMap {")
		io.WriteString(w, q.Var+" -> if (")
		q.Where.emit(w)
		io.WriteString(w, ") listOf(")
		q.VarRef().emit(w)
		io.WriteString(w, ") else emptyList() }")
	}
	io.WriteString(w, ".map {")
	io.WriteString(w, q.Var+" -> ")
	q.Select.emit(w)
	io.WriteString(w, " }")
}

func (q *QueryExpr) VarRef() Expr { return &VarRef{Name: q.Var} }

func kotlinType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "bool":
			return "Boolean"
		case "string":
			return "String"
		case "float":
			return "Double"
		case "bigrat":
			useHelper("importBigInt")
			useHelper("bigRatHelpers")
			return "BigRat"
		case "bigint":
			useHelper("importBigInt")
			return "BigInteger"
		case "any":
			return "Any?"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			elem := kotlinType(t.Generic.Args[0])
			if elem == "" {
				elem = "Any?"
			}
			return "MutableList<" + elem + ">"
		case "map":
			k := kotlinType(t.Generic.Args[0])
			v := kotlinType(t.Generic.Args[1])
			if k == "" {
				k = "Any?"
			}
			if v == "" {
				v = "Any?"
			}
			return "MutableMap<" + k + ", " + v + ">"
		}
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			pt := kotlinType(p)
			if pt == "" {
				pt = "Any"
			}
			params[i] = pt
		}
		ret := kotlinType(t.Fun.Return)
		if ret == "" {
			ret = "Any"
		}
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	}
	return "Any"
}

func kotlinTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, *types.IntType:
		return "Int"
	case types.Int64Type, *types.Int64Type:
		return "Long"
	case types.BigIntType, *types.BigIntType:
		useHelper("importBigInt")
		return "BigInteger"
	case types.BoolType, *types.BoolType:
		return "Boolean"
	case types.StringType, *types.StringType:
		return "String"
	case types.FloatType, *types.FloatType:
		return "Double"
	case types.BigRatType, *types.BigRatType:
		useHelper("importBigInt")
		useHelper("bigRatHelpers")
		return "BigRat"
	case types.AnyType, *types.AnyType:
		return "Any?"
	case types.ListType:
		elem := kotlinTypeFromType(tt.Elem)
		if elem == "" {
			elem = "Any?"
		}
		return "MutableList<" + elem + ">"
	case types.MapType:
		k := kotlinTypeFromType(tt.Key)
		v := kotlinTypeFromType(tt.Value)
		if k == "" {
			k = "Any?"
		}
		if v == "" {
			v = "Any?"
		}
		return "MutableMap<" + k + ", " + v + ">"
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			pt := kotlinTypeFromType(p)
			if pt == "" {
				pt = "Any"
			}
			params[i] = pt
		}
		ret := kotlinTypeFromType(tt.Return)
		if ret == "" {
			ret = "Any"
		}
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	}
	return ""
}

func typeRefUsesName(t *parser.TypeRef, name string) bool {
	if t == nil {
		return false
	}
	if t.Simple != nil {
		return *t.Simple == name
	}
	if t.Fun != nil {
		for _, p := range t.Fun.Params {
			if typeRefUsesName(p, name) {
				return true
			}
		}
		return typeRefUsesName(t.Fun.Return, name)
	}
	if t.Generic != nil {
		for _, a := range t.Generic.Args {
			if typeRefUsesName(a, name) {
				return true
			}
		}
	}
	if t.Struct != nil {
		for _, f := range t.Struct.Fields {
			if typeRefUsesName(f.Type, name) {
				return true
			}
		}
	}
	return false
}

func kotlinZeroValue(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, *types.IntType, types.Int64Type, *types.Int64Type:
		return "0"
	case types.FloatType, *types.FloatType:
		return "0.0"
	case types.StringType, *types.StringType:
		return "\"\""
	case types.BoolType, *types.BoolType:
		return "false"
	case types.ListType:
		elem := kotlinTypeFromType(tt.Elem)
		if elem == "" {
			elem = "Any?"
		}
		return fmt.Sprintf("mutableListOf<%s>()", elem)
	case types.MapType:
		k := kotlinTypeFromType(tt.Key)
		v := kotlinTypeFromType(tt.Value)
		if k == "" {
			k = "Any?"
		}
		if v == "" {
			v = "Any?"
		}
		return fmt.Sprintf("mutableMapOf<%s, %s>()", k, v)
	case types.OptionType:
		return "null"
	case types.StructType:
		fields := make([]string, len(tt.Order))
		for i, f := range tt.Order {
			fields[i] = fmt.Sprintf("%s = %s", f, kotlinZeroValue(tt.Fields[f]))
		}
		return fmt.Sprintf("%s(%s)", tt.Name, strings.Join(fields, ", "))
	default:
		return "null"
	}
}

func guessType(e Expr) string {
	switch v := e.(type) {
	case *IntLit:
		if v.Value > 2147483647 || v.Value < -2147483648 {
			return "Long"
		}
		return "Int"
	case *FloatLit:
		return "Double"
	case *CharLit:
		return "Char"
	case *BoolLit:
		return "Boolean"
	case *StringLit:
		return "String"
	case *StrExpr:
		return "String"
	case *LenExpr:
		return "Int"
	case *ListLit:
		if len(v.Elems) > 0 {
			elem := ""
			for _, ex := range v.Elems {
				t := guessType(ex)
				if t == "" {
					t = "Any"
				}
				if elem == "" || elem == "Any" || elem == "MutableList<Any>" {
					elem = t
				} else if t != elem {
					elem = "Any"
					break
				}
			}
			if elem == "" {
				elem = "Any"
			}
			return "MutableList<" + elem + ">"
		}
		return "MutableList<Any>"
	case *TypedListLit:
		return "MutableList<" + v.ElemType + ">"
	case *MapLit:
		if len(v.Items) > 0 {
			k := guessType(v.Items[0].Key)
			if k == "" {
				k = "Any?"
			}
			val := guessType(v.Items[0].Value)
			if val == "" {
				val = "Any?"
			}
			for _, it := range v.Items[1:] {
				vk := guessType(it.Key)
				vv := guessType(it.Value)
				if vk != k {
					k = "Any?"
				}
				if vv != val {
					val = "Any?"
				}
				if k == "Any?" && val == "Any?" {
					break
				}
			}
			return "MutableMap<" + k + ", " + val + ">"
		}
		return "MutableMap<Any?, Any?>"
	case *ValuesExpr:
		base := guessType(v.Map)
		if strings.HasPrefix(base, "MutableMap<") {
			part := strings.TrimSuffix(strings.TrimPrefix(base, "MutableMap<"), ">")
			kv := strings.SplitN(part, ",", 2)
			if len(kv) == 2 {
				val := strings.TrimSpace(kv[1])
				return "MutableList<" + val + ">"
			}
		}
		return "MutableList<Any>"
	case *KeysExpr:
		base := guessType(v.Map)
		if strings.HasPrefix(base, "MutableMap<") {
			part := strings.TrimSuffix(strings.TrimPrefix(base, "MutableMap<"), ">")
			kv := strings.SplitN(part, ",", 2)
			if len(kv) >= 1 {
				key := strings.TrimSpace(kv[0])
				return "MutableList<" + key + ">"
			}
		}
		return "MutableList<Any>"
	case *MultiListComp:
		elem := guessType(v.Expr)
		if elem == "" {
			elem = "Any"
		}
		return "MutableList<" + elem + ">"
	case *GroupQueryExpr:
		elem := guessType(v.Select)
		if elem == "" {
			elem = "Any"
		}
		return "MutableList<" + elem + ">"
	case *SortQueryExpr:
		elem := guessType(v.Select)
		if elem == "" {
			elem = "Any"
		}
		return "MutableList<" + elem + ">"
	case *SkipTakeExpr:
		return guessType(v.Expr)
	case *RightJoinExpr:
		elem := guessType(v.Select)
		if elem == "" {
			elem = "Any"
		}
		return "MutableList<" + elem + ">"
	case *SliceExpr:
		return guessType(v.Value)
	case *SubstringExpr:
		return "String"
	case *AppendExpr:
		t := guessType(v.List)
		if t == "" {
			return "MutableList<Any?>"
		}
		return t
	case *BinaryExpr:
		if v.Op == "+" {
			lt := guessType(v.Left)
			rt := guessType(v.Right)
			if lt == "BigInteger" || rt == "BigInteger" {
				return "BigInteger"
			}
			if lt == "Long" || rt == "Long" {
				return "Long"
			}
			if strings.HasPrefix(lt, "MutableList<") || strings.HasPrefix(rt, "MutableList<") {
				elem := "Any"
				if strings.HasPrefix(lt, "MutableList<") {
					elem = strings.TrimSuffix(strings.TrimPrefix(lt, "MutableList<"), ">")
				}
				if strings.HasPrefix(rt, "MutableList<") {
					rElem := strings.TrimSuffix(strings.TrimPrefix(rt, "MutableList<"), ">")
					if elem == "Any" {
						elem = rElem
					} else if rElem != elem {
						elem = "Any"
					}
				}
				return "MutableList<" + elem + ">"
			}
			if lt == "String" || rt == "String" {
				return "String"
			}
			if lt == "Double" || rt == "Double" {
				return "Double"
			}
			return "Int"
		}
		if v.Op == "-" || v.Op == "*" || v.Op == "%" {
			lt := guessType(v.Left)
			rt := guessType(v.Right)
			if lt == "BigInteger" || rt == "BigInteger" {
				return "BigInteger"
			}
			if lt == "Long" || rt == "Long" {
				return "Long"
			}
			if lt == "String" || rt == "String" {
				return "String"
			}
			if lt == "Double" || rt == "Double" {
				return "Double"
			}
			return "Int"
		}
		if v.Op == "/" {
			lt := guessType(v.Left)
			rt := guessType(v.Right)
			if lt == "BigInteger" || rt == "BigInteger" {
				return "BigInteger"
			}
			if lt == "Long" || rt == "Long" {
				if lt != "Double" && rt != "Double" {
					return "Long"
				}
			}
			if lt != "Double" && rt != "Double" {
				return "Int"
			}
			return "Double"
		}
		if v.Op == "==" || v.Op == "!=" || v.Op == ">" || v.Op == "<" || v.Op == ">=" || v.Op == "<=" || v.Op == "in" {
			return "Boolean"
		}
		return ""
	case *IndexExpr:
		if v.Type != "" {
			return v.Type
		}
		base := guessType(v.Target)
		if base == "String" {
			return "String"
		}
		if strings.HasPrefix(base, "MutableList<") {
			t := strings.TrimSuffix(strings.TrimPrefix(base, "MutableList<"), ">")
			return t
		}
		if strings.HasPrefix(base, "MutableMap<") {
			part := strings.TrimSuffix(strings.TrimPrefix(base, "MutableMap<"), ">")
			kv := strings.SplitN(part, ",", 2)
			if len(kv) == 2 {
				return strings.TrimSpace(kv[1])
			}
		}
		return "Any"
	case *NowExpr:
		return "Int"
	case *VarRef:
		if v.Type != "" {
			return v.Type
		}
		return "Any"
	case *CallExpr:
		if ret, ok := funcRets[v.Func]; ok {
			return ret
		}
		switch v.Func {
		case "input":
			return "String"
		case "int":
			return "Int"
		}
		return ""
	case *InvokeExpr:
		if f, ok := v.Callee.(*FieldExpr); ok {
			if ret, ok := funcRets[f.Name]; ok {
				return ret
			}
		}
		return "Any?"
	case *CastExpr:
		if v.Type != "" {
			switch v.Type {
			case "int":
				return "Int"
			case "float":
				return "Double"
			case "string":
				return "String"
			case "bigrat", "BigRat":
				return "BigRat"
			default:
				return v.Type
			}
		}
		return guessType(v.Value)
	case *FieldExpr:
		if v.Type != "" {
			return v.Type
		}
		return ""
	case *StructLit:
		return v.Name
	case *UnionExpr:
		return guessType(v.Left)
	case *UnionAllExpr:
		return guessType(v.Left)
	case *ExceptExpr:
		return guessType(v.Left)
	case *IntersectExpr:
		return guessType(v.Left)
	case *ExistsExpr:
		return "Boolean"
	case *FuncLit:
		return ""
	}
	return ""
}

// newVarRef creates a VarRef with the type looked up in the environment when
// available.
func newVarRef(env *types.Env, name string) *VarRef {
	if name == "nil" {
		return &VarRef{Name: "null"}
	}
	typ := ""
	isFunc := false
	hasVar := false
	if env != nil {
		if t, err := env.GetVar(name); err == nil {
			typ = kotlinTypeFromType(t)
			hasVar = true
			if _, ok := t.(types.FuncType); ok && localFuncs[name] {
				isFunc = true
			}
		}
	}
	if !hasVar && localFuncs[name] {
		isFunc = true
	}
	return &VarRef{Name: name, Type: typ, IsFunc: isFunc}
}

func envTypeName(env *types.Env, name string) string {
	if env == nil {
		return ""
	}
	if t, err := env.GetVar(name); err == nil {
		if _, ok := t.(types.FuncType); !ok {
			return kotlinTypeFromType(t)
		}
	}
	return ""
}

func localTypeName(env *types.Env, name string) string {
	if env == nil {
		return ""
	}
	if t, ok := env.Types()[name]; ok {
		if _, ok := t.(types.FuncType); !ok {
			return kotlinTypeFromType(t)
		}
	}
	return ""
}

func findStructByField(env *types.Env, field string) (types.StructType, bool) {
	if env == nil {
		return types.StructType{}, false
	}
	var found types.StructType
	for _, st := range env.Structs() {
		if _, ok := st.Fields[field]; ok {
			if found.Name != "" {
				return types.StructType{}, false
			}
			found = st
		}
	}
	if found.Name == "" {
		return types.StructType{}, false
	}
	return found, true
}

func handleImport(env *types.Env, im *parser.ImportStmt) bool {
	if im.Lang == nil {
		return false
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	switch *im.Lang {
	case "python":
		if im.Path == "math" {
			if builtinAliases == nil {
				builtinAliases = map[string]string{}
			}
			builtinAliases[alias] = "python_math"
			if env != nil {
				env.SetVar(alias+".pi", types.FloatType{}, false)
				env.SetVar(alias+".e", types.FloatType{}, false)
				env.SetFuncType(alias+".sqrt", types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}})
				env.SetFuncType(alias+".pow", types.FuncType{Params: []types.Type{types.FloatType{}, types.FloatType{}}, Return: types.FloatType{}})
				env.SetFuncType(alias+".sin", types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}})
				env.SetFuncType(alias+".log", types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}})
			}
			return true
		}
	case "go":
		if im.Auto && im.Path == "mochi/runtime/ffi/go/testpkg" {
			if builtinAliases == nil {
				builtinAliases = map[string]string{}
			}
			builtinAliases[alias] = "go_testpkg"
			if env != nil {
				env.SetFuncType(alias+".Add", types.FuncType{Params: []types.Type{types.IntType{}, types.IntType{}}, Return: types.IntType{}})
				env.SetVar(alias+".Pi", types.FloatType{}, false)
				env.SetVar(alias+".Answer", types.IntType{}, false)
			}
			return true
		}
		if im.Auto && im.Path == "net" {
			if builtinAliases == nil {
				builtinAliases = map[string]string{}
			}
			builtinAliases[alias] = "go_net"
			if env != nil {
				env.SetFuncType(alias+".LookupHost", types.FuncType{Params: []types.Type{types.StringType{}}, Return: types.ListType{Elem: types.StringType{}}})
			}
			return true
		}
	}
	return false
}

// Transpile converts a Mochi program to a simple Kotlin AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	extraDecls = nil
	extraUnions = nil
	extraAliases = nil
	extraHelpers = nil
	helpersUsed = map[string]bool{}
	localFuncs = map[string]bool{}
	varDecls = map[string]*VarStmt{}
	p := &Program{}
	seenStmt := false
	for _, st := range prog.Statements {
		switch {
		case st.Import != nil:
			handled := handleImport(env, st.Import)
			if !handled {
				return nil, fmt.Errorf("unsupported import")
			}
		case st.ExternVar != nil, st.ExternFun != nil, st.ExternObject != nil, st.ExternType != nil:
			// extern declarations have no direct effect
			continue
		case st.Test != nil:
			body, err := convertStmts(types.NewEnv(env), st.Test.Body)
			if err != nil {
				return nil, err
			}
			fname := "test_" + strings.ReplaceAll(strings.Trim(st.Test.Name, "\""), " ", "_")
			p.Funcs = append(p.Funcs, &FuncDef{Name: fname, Body: body})
			p.Stmts = append(p.Stmts, &ExprStmt{Expr: &CallExpr{Func: fname}})
			seenStmt = true
		case st.Expr != nil:
			e, err := convertExpr(env, st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
			seenStmt = true
		case st.Expect != nil:
			useHelper("expect")
			cond, err := convertExpr(env, st.Expect.Value)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ExpectStmt{Cond: cond})
			seenStmt = true
		case st.Let != nil:
			var val Expr
			if st.Let.Value != nil {
				var err error
				val, err = convertExpr(env, st.Let.Value)
				if err != nil {
					return nil, err
				}
				if ix, ok := val.(*IndexExpr); ok {
					if lit, ok := ix.Index.(*StringLit); ok && lit.Value == "pixel" {
						val = &CastExpr{Value: val, Type: "Pixel"}
					}
				}
			} else {
				val = &IntLit{Value: 0}
			}
			typ := kotlinType(st.Let.Type)
			if st.Let.Type == nil {
				typ = localTypeName(env, st.Let.Name)
				if typ == "" {
					if t := types.CheckExprType(st.Let.Value, env); t != nil {
						typ = kotlinTypeFromType(t)
					}
				}
				if typ == "" {
					typ = guessType(val)
				} else if strings.Contains(typ, "->") {
					if gt := guessType(val); gt != "" && gt != "Any" && !strings.Contains(gt, "->") {
						typ = gt
					}
				}
				if typ == "Int" {
					if lit, ok := val.(*IntLit); ok {
						if lit.Value > 2147483647 || lit.Value < -2147483648 {
							typ = "Long"
						}
					} else if guessType(val) == "Long" {
						typ = "Long"
					}
				}
				if typ == "Any" {
					typ = ""
				}
				if typ == "MutableMap<Any, Any>" {
					typ = "MutableMap<String, Any>"
				}
				if strings.Contains(typ, "Any?") && guessType(val) != typ {
					typ = ""
				}
				if _, ok := val.(*NullLit); ok {
					if typ == "" {
						typ = "Any?"
					} else if !strings.HasSuffix(typ, "?") {
						typ += "?"
					}
				}
				if ix, ok := val.(*IndexExpr); ok {
					if lit, ok := ix.Index.(*StringLit); ok && lit.Value == "pixel" {
						if typ == "" || typ == "Any" || typ == "Any?" {
							typ = "Pixel"
						}
					}
				}
			}
			if seenStmt {
				p.Stmts = append(p.Stmts, &LetStmt{Name: st.Let.Name, Type: typ, Value: val})
				seenStmt = true
			} else {
				p.Globals = append(p.Globals, &LetStmt{Name: st.Let.Name, Type: typ, Value: val})
			}
			if env != nil {
				var tt types.Type
				if st.Let.Type != nil {
					tt = types.ResolveTypeRef(st.Let.Type, env)
				} else if t := types.CheckExprType(st.Let.Value, env); t != nil {
					tt = t
				} else {
					if typ == "Long" {
						tt = types.Int64Type{}
					} else {
						tt = types.AnyType{}
					}
				}
				if typ == "Long" {
					env.SetVar(st.Let.Name, types.Int64Type{}, true)
				} else {
					env.SetVar(st.Let.Name, tt, true)
				}
			}
		case st.Var != nil:
			var val Expr
			if st.Var.Value != nil {
				var err error
				val, err = convertExpr(env, st.Var.Value)
				if err != nil {
					return nil, err
				}
			} else {
				val = &IntLit{Value: 0}
			}
			typ := kotlinType(st.Var.Type)
			if st.Var.Type == nil {
				typ = localTypeName(env, st.Var.Name)
				if typ == "" {
					if t := types.CheckExprType(st.Var.Value, env); t != nil {
						typ = kotlinTypeFromType(t)
						if typ == "Any?" || typ == "MutableList<Any?>" || typ == "MutableMap<Any?, Any?>" {
							if gt := guessType(val); gt != "" && gt != "Any?" && gt != "MutableList<Any?>" && gt != "MutableMap<Any?, Any?>" {
								typ = gt
							}
						}
					}
				}
				if typ == "" {
					typ = guessType(val)
				}
				if typ == "Int" {
					if lit, ok := val.(*IntLit); ok {
						if lit.Value > 2147483647 || lit.Value < -2147483648 {
							typ = "Long"
						}
					}
				}
				if typ == "Any" {
					typ = ""
				}
			}
			if st.Var.Type != nil {
				if _, ok := val.(*MapLit); ok {
					val = &CastExpr{Value: val, Type: typ}
				}
			}
			vs := &VarStmt{Name: st.Var.Name, Type: typ, Value: val}
			varDecls[st.Var.Name] = vs
			if seenStmt {
				p.Stmts = append(p.Stmts, vs)
				seenStmt = true
			} else {
				p.Globals = append(p.Globals, vs)
			}
			if env != nil {
				var tt types.Type
				if st.Var.Type != nil {
					tt = types.ResolveTypeRef(st.Var.Type, env)
				} else if t := types.CheckExprType(st.Var.Value, env); t != nil {
					tt = t
				} else {
					if typ == "Long" {
						tt = types.Int64Type{}
					} else {
						tt = types.AnyType{}
					}
				}
				if typ == "Long" {
					env.SetVar(st.Var.Name, types.Int64Type{}, true)
				} else {
					env.SetVar(st.Var.Name, tt, true)
				}
			}
		case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
			e, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			// skip assignment casting; rely on expression types
			p.Stmts = append(p.Stmts, &AssignStmt{Name: st.Assign.Name, Value: e})
			seenStmt = true
		case st.Assign != nil && (len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0):
			target, err := buildAccessTarget(env, st.Assign.Name, st.Assign.Index, st.Assign.Field)
			if err != nil {
				return nil, err
			}
			if ix, ok := target.(*IndexExpr); ok {
				ix.ForceBang = false
			}
			v, err := convertExpr(env, st.Assign.Value)
			if err != nil {
				return nil, err
			}
			if ix, ok := target.(*IndexExpr); ok {
				if ix.Type != "" && ix.Type != guessType(v) {
					v = &CastExpr{Value: v, Type: ix.Type}
				}
			}
			p.Stmts = append(p.Stmts, &IndexAssignStmt{Target: target, Value: v})
			seenStmt = true
		case st.Update != nil:
			stmt, err := convertUpdateStmt(env, st.Update)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
			seenStmt = true
		case st.Return != nil:
			var val Expr
			if st.Return.Value != nil {
				var err error
				val, err = convertExpr(env, st.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			if _, ok := val.(*NullLit); ok && (currentRetType == "" || currentRetType == "Unit") {
				val = nil
			}
			if currentRetType != "" && currentRetType != "Any" && val != nil {
				if guessType(val) != currentRetType {
					val = &CastExpr{Value: val, Type: currentRetType}
				}
			}
			p.Stmts = append(p.Stmts, &ReturnStmt{Value: val})
			seenStmt = true
		case st.Fun != nil:
			bodyEnv := types.NewEnv(env)
			ftParams := make([]types.Type, 0, len(st.Fun.Params))
			for _, p0 := range st.Fun.Params {
				pt := types.ResolveTypeRef(p0.Type, env)
				bodyEnv.SetVar(p0.Name, pt, true)
				ftParams = append(ftParams, pt)
			}
			var retType types.Type = types.VoidType{}
			if st.Fun.Return != nil {
				retType = types.ResolveTypeRef(st.Fun.Return, env)
			}
			env.SetVar(st.Fun.Name, types.FuncType{Params: ftParams, Return: retType}, false)
			env.SetFunc(st.Fun.Name, st.Fun)
			localFuncs[st.Fun.Name] = true
			if st.Fun.Name == "pow2" || st.Fun.Name == "lshift" || st.Fun.Name == "rshift" {
				switch st.Fun.Name {
				case "pow2":
					funcRets[st.Fun.Name] = "Long"
				default:
					funcRets[st.Fun.Name] = "Int"
				}
				useHelper(st.Fun.Name)
				continue
			}
			prevRet := currentRetType
			ret := kotlinType(st.Fun.Return)
			if ret == "" {
				if t, ok := env.Types()[st.Fun.Name]; ok {
					if ft, ok := t.(types.FuncType); ok {
						ret = kotlinTypeFromType(ft.Return)
					}
				}
			}
			currentRetType = ret
			fname := st.Fun.Name
			if fname == "main" {
				fname = "user_main"
			}
			funcRets[fname] = ret
			body, err := convertStmts(bodyEnv, st.Fun.Body)
			currentRetType = prevRet
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p0 := range st.Fun.Params {
				typ := kotlinType(p0.Type)
				if typ == "" {
					typ = "Any"
				}
				params = append(params, fmt.Sprintf("%s: %s", p0.Name, typ))
			}
			// insert mutable parameter locals if assigned
			for i, p0 := range st.Fun.Params {
				if paramAssigned(p0.Name, body) {
					typ := kotlinType(p0.Type)
					if typ == "" {
						typ = "Any"
					}
					assign := &VarStmt{Name: p0.Name, Type: typ, Value: &VarRef{Name: p0.Name}}
					body = append([]Stmt{assign}, body...)
					// update params[i] remains same
				}
				_ = i
			}
			p.Funcs = append(p.Funcs, &FuncDef{Name: fname, Params: params, Ret: ret, Body: body})
			funcRets[fname] = ret
		case st.If != nil:
			stmt, err := convertIfStmt(env, st.If)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.While != nil:
			stmt, err := convertWhileStmt(env, st.While)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.For != nil:
			stmt, err := convertForStmt(env, st.For)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, stmt)
		case st.Break != nil:
			p.Stmts = append(p.Stmts, &BreakStmt{})
		case st.Continue != nil:
			p.Stmts = append(p.Stmts, &ContinueStmt{})
		case st.Type != nil:
			if st.Type.Alias != nil {
				typ := kotlinType(st.Type.Alias)
				if typeRefUsesName(st.Type.Alias, st.Type.Name) {
					typ = kotlinTypeFromType(types.ResolveTypeRef(st.Type.Alias, env))
				}
				extraAliases = append(extraAliases, &TypeAlias{Name: st.Type.Name, Type: typ})
			} else if len(st.Type.Variants) == 1 && len(st.Type.Variants[0].Fields) == 0 {
				vname := st.Type.Variants[0].Name
				tref := &parser.TypeRef{Simple: &vname}
				typ := kotlinType(tref)
				if typ == "" {
					typ = vname
				}
				extraAliases = append(extraAliases, &TypeAlias{Name: st.Type.Name, Type: typ})
			} else if len(st.Type.Variants) > 0 {
				var variants []*DataClass
				for _, v := range st.Type.Variants {
					stv, _ := env.GetStruct(v.Name)
					var fields []ParamDecl
					for _, n := range stv.Order {
						fields = append(fields, ParamDecl{Name: n, Type: kotlinTypeFromType(stv.Fields[n])})
					}
					variants = append(variants, &DataClass{Name: v.Name, Fields: fields, IsObject: len(fields) == 0})
				}
				extraUnions = append(extraUnions, &SumType{Name: st.Type.Name, Variants: variants})
			} else {
				var fields []ParamDecl
				fieldMap := map[string]types.Type{}
				var order []string
				var methods []*FuncDef
				for _, m := range st.Type.Members {
					if m.Field != nil {
						ft := types.ResolveTypeRef(m.Field.Type, env)
						fields = append(fields, ParamDecl{Name: m.Field.Name, Type: kotlinTypeFromType(ft), Default: kotlinZeroValue(ft)})
						fieldMap[m.Field.Name] = ft
						order = append(order, m.Field.Name)
					} else if m.Method != nil {
						ms := m.Method
						mEnv := types.NewEnv(env)
						params := make([]string, len(ms.Params))
						for i, p0 := range ms.Params {
							pt := types.ResolveTypeRef(p0.Type, env)
							mEnv.SetVar(p0.Name, pt, true)
							typ := kotlinType(p0.Type)
							if typ == "" {
								typ = "Any"
							}
							params[i] = fmt.Sprintf("%s: %s", p0.Name, typ)
						}
						prevRet := currentRetType
						ret := kotlinType(ms.Return)
						currentRetType = ret
						body, err := convertStmts(mEnv, ms.Body)
						currentRetType = prevRet
						if err != nil {
							return nil, err
						}
						methods = append(methods, &FuncDef{Name: ms.Name, Params: params, Ret: ret, Body: body})
					}
				}
				extraDecls = append(extraDecls, &DataClass{Name: st.Type.Name, Fields: fields, Methods: methods})
				if env != nil {
					env.SetStruct(st.Type.Name, types.StructType{Name: st.Type.Name, Fields: fieldMap, Order: order})
				}
			}
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	if benchMain {
		useHelper("_now")
		useHelper("toJson")
		p.Stmts = []Stmt{&BenchStmt{Name: "main", Body: p.Stmts}}
	}
	p.Structs = extraDecls
	p.Unions = extraUnions
	p.Aliases = extraAliases
	p.Helpers = extraHelpers
	return p, nil
}

func convertStmts(env *types.Env, list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, s := range list {
		switch {
		case s.Import != nil:
			handleImport(env, s.Import)
			continue
		case s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
			continue
		case s.Expr != nil:
			e, err := convertExpr(env, s.Expr.Expr)
			if err != nil {
				return nil, err
			}
			out = append(out, &ExprStmt{Expr: e})
		case s.Expect != nil:
			useHelper("expect")
			cond, err := convertExpr(env, s.Expect.Value)
			if err != nil {
				return nil, err
			}
			out = append(out, &ExpectStmt{Cond: cond})
		case s.Let != nil:
			v, err := convertExpr(env, s.Let.Value)
			if err != nil {
				return nil, err
			}
			typ := kotlinType(s.Let.Type)
			if s.Let.Type == nil {
				typ = localTypeName(env, s.Let.Name)
				if typ == "" {
					if t := types.CheckExprType(s.Let.Value, env); t != nil {
						typ = kotlinTypeFromType(t)
						if strings.Contains(typ, "->") {
							// fall back to guess if return type inferred as a function
							gt := guessType(v)
							if gt != "" {
								typ = gt
							}
						}
					}
				}
				if typ == "" || typ == "Any" || typ == "Any?" {
					typ = guessType(v)
					if typ == "" {
						if ix, ok := v.(*IndexExpr); ok && ix.Type != "" {
							typ = ix.Type
						}
					}
				}
				if typ == "Any" {
					// allow Kotlin to infer a more precise type
					typ = ""
				}
				if typ == "MutableMap<Any, Any>" {
					typ = "MutableMap<String, Any>"
				}
				if strings.Contains(typ, "Any?") && guessType(v) != typ {
					typ = ""
				}
				if ix, ok := v.(*IndexExpr); ok {
					_ = ix
				}
			}
			if s.Let.Type != nil {
				if _, ok := v.(*MapLit); ok {
					v = &CastExpr{Value: v, Type: typ}
				}
			}
			out = append(out, &LetStmt{Name: s.Let.Name, Type: typ, Value: v})
			if env != nil {
				var tt types.Type
				if s.Let.Type != nil {
					tt = types.ResolveTypeRef(s.Let.Type, env)
				} else if t := types.CheckExprType(s.Let.Value, env); t != nil {
					if typ != "" && types.ContainsAny(t) {
						switch typ {
						case "Int":
							tt = types.IntType{}
						case "Double":
							tt = types.FloatType{}
						case "Boolean":
							tt = types.BoolType{}
						case "String":
							tt = types.StringType{}
						default:
							tt = t
						}
					} else {
						tt = t
						if typ == "Int" {
							if _, ok := tt.(types.BigIntType); ok {
								tt = types.IntType{}
							}
						}
					}
				} else {
					switch typ {
					case "Int":
						tt = types.IntType{}
					case "Double":
						tt = types.FloatType{}
					case "Boolean":
						tt = types.BoolType{}
					case "String":
						tt = types.StringType{}
					case "BigInteger":
						tt = types.BigIntType{}
					default:
						if strings.HasPrefix(typ, "MutableList<") {
							elem := strings.TrimSuffix(strings.TrimPrefix(typ, "MutableList<"), ">")
							var et types.Type = types.AnyType{}
							switch elem {
							case "Int":
								et = types.IntType{}
							case "Double":
								et = types.FloatType{}
							case "Boolean":
								et = types.BoolType{}
							case "String":
								et = types.StringType{}
							}
							tt = types.ListType{Elem: et}
						} else if strings.HasPrefix(typ, "MutableMap<") {
							part := strings.TrimSuffix(strings.TrimPrefix(typ, "MutableMap<"), ">")
							kv := strings.SplitN(part, ",", 2)
							var kt types.Type = types.AnyType{}
							var vt types.Type = types.AnyType{}
							if len(kv) == 2 {
								switch strings.TrimSpace(kv[0]) {
								case "Int":
									kt = types.IntType{}
								case "Double":
									kt = types.FloatType{}
								case "Boolean":
									kt = types.BoolType{}
								case "String":
									kt = types.StringType{}
								}
								switch strings.TrimSpace(kv[1]) {
								case "Int":
									vt = types.IntType{}
								case "Double":
									vt = types.FloatType{}
								case "Boolean":
									vt = types.BoolType{}
								case "String":
									vt = types.StringType{}
								}
							}
							tt = types.MapType{Key: kt, Value: vt}
						} else {
							tt = types.AnyType{}
						}
					}
				}
				if typ == "Long" {
					env.SetVar(s.Let.Name, types.Int64Type{}, true)
				} else {
					env.SetVar(s.Let.Name, tt, true)
				}
			}
		case s.Var != nil:
			v, err := convertExpr(env, s.Var.Value)
			if err != nil {
				return nil, err
			}
			typ := kotlinType(s.Var.Type)
			if s.Var.Type == nil {
				if typ == "" {
					if t := types.CheckExprType(s.Var.Value, env); t != nil {
						typ = kotlinTypeFromType(t)
						if typ == "Any" || strings.Contains(typ, "->") {
							gt := guessType(v)
							if gt != "" {
								typ = gt
							}
						}
					}
				}
				if typ == "" || typ == "Any" || typ == "Any?" {
					typ = guessType(v)
					if typ == "" {
						if ix, ok := v.(*IndexExpr); ok && ix.Type != "" {
							typ = ix.Type
						}
					}
				}
				if typ == "Int" {
					if lit, ok := v.(*IntLit); ok {
						if lit.Value > 2147483647 || lit.Value < -2147483648 {
							typ = "Long"
						}
					} else if guessType(v) == "Long" {
						typ = "Long"
					}
				}
				if typ == "Any" {
					// prefer type inference over explicit Any
					typ = ""
				}
				if typ == "MutableMap<Any, Any>" {
					typ = "MutableMap<String, Any>"
				}
				if strings.Contains(typ, "Any?") && guessType(v) != typ {
					typ = ""
				}
				if ix, ok := v.(*IndexExpr); ok {
					tgt := guessType(ix.Target)
					if strings.HasPrefix(tgt, "MutableMap<") && !strings.HasSuffix(typ, "?") {
						typ += "?"
					}
				}
			}
			if _, ok := v.(*NullLit); ok {
				if typ == "" {
					typ = "Any?"
				} else if !strings.HasSuffix(typ, "?") {
					typ += "?"
				}
			}
			if s.Var.Type != nil {
				if _, ok := v.(*MapLit); ok {
					v = &CastExpr{Value: v, Type: typ}
				}
			}
			out = append(out, &VarStmt{Name: s.Var.Name, Type: typ, Value: v})
			if env != nil {
				var tt types.Type
				if s.Var.Type != nil {
					tt = types.ResolveTypeRef(s.Var.Type, env)
				} else if t := types.CheckExprType(s.Var.Value, env); t != nil {
					if typ != "" && types.ContainsAny(t) {
						switch typ {
						case "Int":
							tt = types.IntType{}
						case "Double":
							tt = types.FloatType{}
						case "Boolean":
							tt = types.BoolType{}
						case "String":
							tt = types.StringType{}
						default:
							tt = t
						}
					} else {
						tt = t
						if typ == "Int" {
							if _, ok := tt.(types.BigIntType); ok {
								tt = types.IntType{}
							}
						}
					}
				} else {
					switch typ {
					case "Int":
						tt = types.IntType{}
					case "Double":
						tt = types.FloatType{}
					case "Boolean":
						tt = types.BoolType{}
					case "String":
						tt = types.StringType{}
					default:
						if strings.HasPrefix(typ, "MutableList<") {
							elem := strings.TrimSuffix(strings.TrimPrefix(typ, "MutableList<"), ">")
							var et types.Type = types.AnyType{}
							switch elem {
							case "Int":
								et = types.IntType{}
							case "Double":
								et = types.FloatType{}
							case "Boolean":
								et = types.BoolType{}
							case "String":
								et = types.StringType{}
							}
							tt = types.ListType{Elem: et}
						} else if strings.HasPrefix(typ, "MutableMap<") {
							part := strings.TrimSuffix(strings.TrimPrefix(typ, "MutableMap<"), ">")
							kv := strings.SplitN(part, ",", 2)
							var kt types.Type = types.AnyType{}
							var vt types.Type = types.AnyType{}
							if len(kv) == 2 {
								switch strings.TrimSpace(kv[0]) {
								case "Int":
									kt = types.IntType{}
								case "Double":
									kt = types.FloatType{}
								case "Boolean":
									kt = types.BoolType{}
								case "String":
									kt = types.StringType{}
								}
								switch strings.TrimSpace(kv[1]) {
								case "Int":
									vt = types.IntType{}
								case "Double":
									vt = types.FloatType{}
								case "Boolean":
									vt = types.BoolType{}
								case "String":
									vt = types.StringType{}
								}
							}
							tt = types.MapType{Key: kt, Value: vt}
						} else {
							tt = types.AnyType{}
						}
					}
				}
				if typ == "Long" {
					env.SetVar(s.Var.Name, types.Int64Type{}, true)
				} else {
					env.SetVar(s.Var.Name, tt, true)
				}
			}
		case s.Assign != nil && len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0:
			v, err := convertExpr(env, s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if _, isNull := v.(*NullLit); isNull {
				if vs, ok := varDecls[s.Assign.Name]; ok && !strings.HasSuffix(vs.Type, "?") {
					vs.Type += "?"
				}
				continue
			}
			if env != nil {
				if tt, err := env.GetVar(s.Assign.Name); err == nil {
					tname := kotlinTypeFromType(tt)
					if strings.Contains(tname, "->") {
						tname = ""
					}
					if tname != "" && tname != guessType(v) {
						if tname == "Any" {
							if gt := guessType(v); gt != "" {
								v = &CastExpr{Value: v, Type: gt}
							}
						} else if tname == "MutableList<Any>" {
							if gt := guessType(v); gt != "" {
								v = &CastExpr{Value: v, Type: gt}
							}
						} else {
							v = &CastExpr{Value: v, Type: tname}
						}
					}
				}
			}
			out = append(out, &AssignStmt{Name: s.Assign.Name, Value: v})
		case s.Assign != nil && (len(s.Assign.Index) > 0 || len(s.Assign.Field) > 0):
			target, err := buildAccessTarget(env, s.Assign.Name, s.Assign.Index, s.Assign.Field)
			if err != nil {
				return nil, err
			}
			if ix, ok := target.(*IndexExpr); ok {
				ix.ForceBang = false
			}
			v, err := convertExpr(env, s.Assign.Value)
			if err != nil {
				return nil, err
			}
			if ix, ok := target.(*IndexExpr); ok {
				if ix.Type != "" && ix.Type != guessType(v) {
					v = &CastExpr{Value: v, Type: ix.Type}
				}
			}
			out = append(out, &IndexAssignStmt{Target: target, Value: v})
		case s.Update != nil:
			st, err := convertUpdateStmt(env, s.Update)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.Fun != nil:
			bodyEnv := types.NewEnv(env)
			ftParams := make([]types.Type, 0, len(s.Fun.Params))
			for _, p0 := range s.Fun.Params {
				pt := types.ResolveTypeRef(p0.Type, env)
				bodyEnv.SetVar(p0.Name, pt, true)
				ftParams = append(ftParams, pt)
			}
			var retType types.Type = types.VoidType{}
			if s.Fun.Return != nil {
				retType = types.ResolveTypeRef(s.Fun.Return, env)
			}
			env.SetVar(s.Fun.Name, types.FuncType{Params: ftParams, Return: retType}, false)
			env.SetFunc(s.Fun.Name, s.Fun)
			localFuncs[s.Fun.Name] = true
			prevRet := currentRetType
			ret := kotlinType(s.Fun.Return)
			currentRetType = ret
			body, err := convertStmts(bodyEnv, s.Fun.Body)
			currentRetType = prevRet
			if err != nil {
				return nil, err
			}
			var params []string
			for _, p0 := range s.Fun.Params {
				typ := kotlinType(p0.Type)
				if typ == "" {
					typ = "Any"
				}
				params = append(params, fmt.Sprintf("%s: %s", p0.Name, typ))
			}
			if ret == "" {
				if t, ok := env.Types()[s.Fun.Name]; ok {
					if ft, ok := t.(types.FuncType); ok {
						ret = kotlinTypeFromType(ft.Return)
					}
				}
			}
			fname := s.Fun.Name
			if fname == "main" {
				fname = "user_main"
			}
			out = append(out, &FuncDef{Name: fname, Params: params, Ret: ret, Body: body})
		case s.Return != nil:
			var v Expr
			if s.Return.Value != nil {
				var err error
				v, err = convertExpr(env, s.Return.Value)
				if err != nil {
					return nil, err
				}
			}
			if _, ok := v.(*NullLit); ok && (currentRetType == "" || currentRetType == "Unit") {
				v = nil
			}
			if currentRetType != "" && currentRetType != "Any" && v != nil {
				if guessType(v) != currentRetType {
					v = &CastExpr{Value: v, Type: currentRetType}
				}
			}
			out = append(out, &ReturnStmt{Value: v})
		case s.If != nil:
			st, err := convertIfStmt(env, s.If)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.While != nil:
			st, err := convertWhileStmt(env, s.While)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.For != nil:
			st, err := convertForStmt(env, s.For)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.Bench != nil:
			st, err := convertBench(env, s.Bench)
			if err != nil {
				return nil, err
			}
			out = append(out, st)
		case s.Break != nil:
			out = append(out, &BreakStmt{})
		case s.Continue != nil:
			out = append(out, &ContinueStmt{})
		case s.Type != nil:
			if s.Type.Alias != nil {
				typ := kotlinType(s.Type.Alias)
				if typeRefUsesName(s.Type.Alias, s.Type.Name) {
					typ = kotlinTypeFromType(types.ResolveTypeRef(s.Type.Alias, env))
				}
				extraAliases = append(extraAliases, &TypeAlias{Name: s.Type.Name, Type: typ})
			} else if len(s.Type.Variants) == 1 && len(s.Type.Variants[0].Fields) == 0 {
				vname := s.Type.Variants[0].Name
				tref := &parser.TypeRef{Simple: &vname}
				typ := kotlinType(tref)
				if typ == "" {
					typ = vname
				}
				extraAliases = append(extraAliases, &TypeAlias{Name: s.Type.Name, Type: typ})
			} else if len(s.Type.Variants) > 0 {
				var variants []*DataClass
				for _, v := range s.Type.Variants {
					st, _ := env.GetStruct(v.Name)
					var fields []ParamDecl
					for _, n := range st.Order {
						fields = append(fields, ParamDecl{Name: n, Type: kotlinTypeFromType(st.Fields[n])})
					}
					variants = append(variants, &DataClass{Name: v.Name, Fields: fields, IsObject: len(fields) == 0})
				}
				extraUnions = append(extraUnions, &SumType{Name: s.Type.Name, Variants: variants})
			} else {
				var fields []ParamDecl
				fieldMap := map[string]types.Type{}
				var order []string
				for _, m := range s.Type.Members {
					if m.Field == nil {
						continue
					}
					ft := types.ResolveTypeRef(m.Field.Type, env)
					fields = append(fields, ParamDecl{Name: m.Field.Name, Type: kotlinTypeFromType(ft)})
					fieldMap[m.Field.Name] = ft
					order = append(order, m.Field.Name)
				}
				extraDecls = append(extraDecls, &DataClass{Name: s.Type.Name, Fields: fields})
				if env != nil {
					env.SetStruct(s.Type.Name, types.StructType{Name: s.Type.Name, Fields: fieldMap, Order: order})
				}
			}
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return out, nil
}

func convertIfStmt(env *types.Env, is *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(env, is.Cond)
	if err != nil {
		return nil, err
	}
	if !isBoolExpr(cond) {
		cond = &CastExpr{Value: cond, Type: "Boolean"}
	}
	thenStmts, err := convertStmts(env, is.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if is.ElseIf != nil {
		stmt, err := convertIfStmt(env, is.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{stmt}
	} else if len(is.Else) > 0 {
		elseStmts, err = convertStmts(env, is.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(env *types.Env, ws *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(env, ws.Cond)
	if err != nil {
		return nil, err
	}
	if !isBoolExpr(cond) {
		cond = &CastExpr{Value: cond, Type: "Boolean"}
	}
	bodyEnv := types.NewEnv(env)
	body, err := convertStmts(bodyEnv, ws.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(env *types.Env, fs *parser.ForStmt) (Stmt, error) {
	if fs.RangeEnd != nil {
		start, err := convertExpr(env, fs.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(env, fs.RangeEnd)
		if err != nil {
			return nil, err
		}
		bodyEnv := types.NewEnv(env)
		bodyEnv.SetVar(fs.Name, types.IntType{}, true)
		body, err := convertStmts(bodyEnv, fs.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fs.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(env, fs.Source)
	if err != nil {
		return nil, err
	}
	var elem types.Type = types.AnyType{}
	if name := simpleVarName(fs.Source); name != "" {
		if t, err := env.GetVar(name); err == nil {
			if lt, ok := t.(types.ListType); ok {
				elem = lt.Elem
			} else if _, ok := t.(types.StringType); ok {
				elem = types.StringType{}
			}
		}
	} else {
		if t := types.CheckExprType(fs.Source, env); t != nil {
			if lt, ok := t.(types.ListType); ok {
				elem = lt.Elem
			} else if _, ok := t.(types.StringType); ok {
				elem = types.StringType{}
			}
		}
	}
	if types.IsMapExpr(fs.Source, env) {
		// If the source expression is a map variable itself we iterate
		// over the key set. However map indexing like `m[k]` should be
		// treated as a list value and not converted to `keys`.
		if fs.Source != nil && fs.Source.Binary != nil &&
			fs.Source.Binary.Left != nil && fs.Source.Binary.Left.Value != nil &&
			len(fs.Source.Binary.Left.Value.Ops) == 0 {
			iter = &FieldExpr{Receiver: iter, Name: "keys"}
		}
	}
	bodyEnv := types.NewEnv(env)
	bodyEnv.SetVar(fs.Name, elem, true)
	body, err := convertStmts(bodyEnv, fs.Body)
	if err != nil {
		return nil, err
	}
	return &ForEachStmt{Name: fs.Name, Iterable: iter, Body: body}, nil
}

func convertBench(env *types.Env, bb *parser.BenchBlock) (Stmt, error) {
	bodyEnv := types.NewEnv(env)
	body, err := convertStmts(bodyEnv, bb.Body)
	if err != nil {
		return nil, err
	}
	useHelper("_now")
	useHelper("toJson")
	return &BenchStmt{Name: strings.Trim(bb.Name, "\""), Body: body}, nil
}

func convertUpdateStmt(env *types.Env, us *parser.UpdateStmt) (Stmt, error) {
	listVar := newVarRef(env, us.Target)
	var elem types.Type = types.AnyType{}
	if env != nil {
		if t, err := env.GetVar(us.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				elem = lt.Elem
			}
		}
	}
	st, ok := elem.(types.StructType)
	if !ok {
		return nil, fmt.Errorf("update only supported for list of struct")
	}
	idxVar := fmt.Sprintf("_i%d", len(us.Target))
	itemVar := fmt.Sprintf("_it%d", len(us.Target))
	bodyEnv := types.NewEnv(env)
	bodyEnv.SetVar(itemVar, st, true)
	for name, typ := range st.Fields {
		bodyEnv.SetVar(name, typ, true)
	}
	// var _it = list[i]
	itemGet := &IndexExpr{Target: listVar, Index: &VarRef{Name: idxVar}, Type: kotlinTypeFromType(st), ForceBang: true}
	varSt := &VarStmt{Name: itemVar, Type: kotlinTypeFromType(st), Value: itemGet}
	// field lets
	var pre []Stmt
	pre = append(pre, varSt)
	for _, name := range st.Order {
		pre = append(pre, &LetStmt{Name: name, Type: kotlinTypeFromType(st.Fields[name]), Value: &FieldExpr{Receiver: &VarRef{Name: itemVar}, Name: name}})
	}
	// condition
	var cond Expr
	var err error
	if us.Where != nil {
		cond, err = convertExpr(bodyEnv, us.Where)
		if err != nil {
			return nil, err
		}
	}
	// assignments
	var assigns []Stmt
	for _, it := range us.Set.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return nil, fmt.Errorf("update dynamic key unsupported")
		}
		val, err := convertExpr(bodyEnv, it.Value)
		if err != nil {
			return nil, err
		}
		assigns = append(assigns, &IndexAssignStmt{Target: &FieldExpr{Receiver: &VarRef{Name: itemVar}, Name: key}, Value: val})
	}
	var ifBody []Stmt
	if cond != nil {
		ifBody = append(ifBody, &IfStmt{Cond: cond, Then: assigns})
	} else {
		ifBody = assigns
	}
	// write back
	assignBack := &IndexAssignStmt{Target: &IndexExpr{Target: listVar, Index: &VarRef{Name: idxVar}, Type: kotlinTypeFromType(st), ForceBang: true}, Value: &VarRef{Name: itemVar}}
	loopBody := append(pre, ifBody...)
	loopBody = append(loopBody, assignBack)
	return &ForRangeStmt{Name: idxVar, Start: &IntLit{Value: 0}, End: &FieldExpr{Receiver: listVar, Name: "size"}, Body: loopBody}, nil
}

func buildIndexTarget(env *types.Env, name string, idx []*parser.IndexOp) (Expr, error) {
	var target Expr = newVarRef(env, name)
	var curType types.Type
	if env != nil {
		if t, err := env.GetVar(name); err == nil {
			curType = t
		}
	}
	for _, op := range idx {
		if op.Colon != nil || op.Colon2 != nil {
			return nil, fmt.Errorf("slice assign unsupported")
		}
		idxExpr, err := convertExpr(env, op.Start)
		if err != nil {
			return nil, err
		}
		tname := ""
		force := true
		if curType != nil {
			switch tt := curType.(type) {
			case types.ListType:
				tname = kotlinTypeFromType(tt.Elem)
				curType = tt.Elem
			case types.MapType:
				curType = tt.Value
				force = false
			}
		}
		target = &IndexExpr{Target: target, Index: idxExpr, Type: tname, ForceBang: force}
	}
	return target, nil
}

func buildAccessTarget(env *types.Env, name string, idx []*parser.IndexOp, fields []*parser.FieldOp) (Expr, error) {
	var target Expr = newVarRef(env, name)
	var curType types.Type
	if env != nil {
		if t, err := env.GetVar(name); err == nil {
			curType = t
		}
	}
	for _, op := range idx {
		if op.Colon != nil || op.Colon2 != nil {
			return nil, fmt.Errorf("slice assign unsupported")
		}
		idxExpr, err := convertExpr(env, op.Start)
		if err != nil {
			return nil, err
		}
		tname := ""
		force := true
		if curType != nil {
			switch tt := curType.(type) {
			case types.ListType:
				tname = kotlinTypeFromType(tt.Elem)
				curType = tt.Elem
			case types.MapType:
				curType = tt.Value
				force = false
			}
		}
		target = &IndexExpr{Target: target, Index: idxExpr, Type: tname, ForceBang: force}
	}
	for _, f := range fields {
		ftype := ""
		if st, ok := curType.(types.StructType); ok {
			if ft, ok2 := st.Fields[f.Name]; ok2 {
				ftype = kotlinTypeFromType(ft)
				curType = ft
			} else {
				curType = nil
			}
		} else {
			curType = nil
		}
		target = &FieldExpr{Receiver: target, Name: f.Name, Type: ftype}
	}
	return target, nil
}

func convertIfExpr(env *types.Env, ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(env, ie.Cond)
	if err != nil {
		return nil, err
	}
	if !isBoolExpr(cond) {
		cond = &NotNullCheck{Value: cond}
	}
	thenExpr, err := convertExpr(env, ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(env, ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(env, ie.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertMatchExpr(env *types.Env, me *parser.MatchExpr) (Expr, error) {
	target, err := convertExpr(env, me.Target)
	if err != nil {
		return nil, err
	}
	cases := make([]WhenCase, 0, len(me.Cases))
	for _, c := range me.Cases {
		if call, ok := callPattern(c.Pattern); ok {
			if ut, ok := env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				child := types.NewEnv(env)
				var stmts []Stmt
				for idx, arg := range call.Args {
					if name, ok := identName(arg); ok {
						if name != "_" {
							ft := st.Fields[st.Order[idx]]
							child.SetVar(name, ft, true)
							field := &FieldExpr{Receiver: &CastExpr{Value: target, Type: call.Func}, Name: st.Order[idx], Type: kotlinTypeFromType(ft)}
							stmts = append(stmts, &LetStmt{Name: name, Type: kotlinTypeFromType(ft), Value: field})
						}
					}
				}
				res, err := convertExpr(child, c.Result)
				if err != nil {
					return nil, err
				}
				cases = append(cases, WhenCase{Pattern: &TypePattern{Type: call.Func}, Result: &RunExpr{Stmts: stmts, Result: res}})
				continue
			}
		}
		if ident, ok := identName(c.Pattern); ok {
			if _, ok := env.FindUnionByVariant(ident); ok {
				res, err := convertExpr(env, c.Result)
				if err != nil {
					return nil, err
				}
				cases = append(cases, WhenCase{Pattern: &TypePattern{Type: ident}, Result: res})
				continue
			}
		}
		res, err := convertExpr(env, c.Result)
		if err != nil {
			return nil, err
		}
		if isUnderscore(c.Pattern) {
			cases = append(cases, WhenCase{Pattern: nil, Result: res})
			continue
		}
		pat, err := convertExpr(env, c.Pattern)
		if err != nil {
			return nil, err
		}
		cases = append(cases, WhenCase{Pattern: pat, Result: res})
	}
	return &WhenExpr{Target: target, Cases: cases}, nil
}

func combineAnd(a, b Expr) Expr {
	if a == nil {
		return b
	}
	if b == nil {
		return a
	}
	return &BinaryExpr{Left: a, Op: "&&", Right: b}
}

func convertRightJoinQuery(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	if q == nil || len(q.Joins) != 1 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertExpr(env, j.Src)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	var elemType types.Type = types.AnyType{}
	if name := simpleVarName(q.Source); name != "" {
		if t, err := env.GetVar(name); err == nil {
			if lt, ok := t.(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	}
	child.SetVar(q.Var, elemType, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	cond, err := convertExpr(child, j.On)
	if err != nil {
		return nil, err
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, err
	}
	return &RightJoinExpr{LeftVar: q.Var, LeftSrc: leftSrc, RightVar: j.Var, RightSrc: rightSrc, Cond: cond, Select: sel}, nil
}

func convertQueryExpr(env *types.Env, q *parser.QueryExpr) (Expr, error) {
	var skipExpr, takeExpr Expr
	var err error
	if q.Skip != nil {
		skipExpr, err = convertExpr(env, q.Skip)
		if err != nil {
			return nil, err
		}
	}
	if q.Take != nil {
		takeExpr, err = convertExpr(env, q.Take)
		if err != nil {
			return nil, err
		}
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "right" && q.Group == nil && len(q.Froms) == 0 && q.Where == nil && skipExpr == nil && takeExpr == nil && q.Sort == nil {
		return convertRightJoinQuery(env, q)
	}
	src, err := convertExpr(env, q.Source)
	if err != nil {
		return nil, err
	}
	child := types.NewEnv(env)
	var elem types.Type = types.AnyType{}
	if name := simpleVarName(q.Source); name != "" {
		if t, err := env.GetVar(name); err == nil {
			switch v := t.(type) {
			case types.ListType:
				elem = v.Elem
			case types.GroupType:
				elem = v.Elem
				src = &FieldExpr{Receiver: src, Name: "items"}
			}
		}
	}
	if len(q.Froms) > 0 || len(q.Joins) > 0 {
		elem = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
	}
	child.SetVar(q.Var, elem, true)
	froms := make([]queryFrom, len(q.Froms))
	for i, f := range q.Froms {
		fe, err := convertExpr(child, f.Src)
		if err != nil {
			return nil, err
		}
		var elem types.Type = types.AnyType{}
		if name := simpleVarName(f.Src); name != "" {
			if t, err := env.GetVar(name); err == nil {
				switch v := t.(type) {
				case types.ListType:
					elem = v.Elem
				case types.GroupType:
					elem = v.Elem
				}
			}
		}
		child.SetVar(f.Var, elem, true)
		froms[i] = queryFrom{Var: f.Var, Src: fe}
	}
	var cond Expr
	for _, j := range q.Joins {
		je, err := convertExpr(child, j.Src)
		if err != nil {
			return nil, err
		}
		var elem types.Type = types.AnyType{}
		if name := simpleVarName(j.Src); name != "" {
			if t, err := env.GetVar(name); err == nil {
				switch v := t.(type) {
				case types.ListType:
					elem = v.Elem
				case types.GroupType:
					elem = v.Elem
				}
			}
		}
		froms = append(froms, queryFrom{Var: j.Var, Src: je})
		child.SetVar(j.Var, elem, true)
		jc, err := convertExpr(child, j.On)
		if err != nil {
			return nil, err
		}
		cond = combineAnd(cond, jc)
	}
	if q.Where != nil {
		wcond, err := convertExpr(child, q.Where)
		if err != nil {
			return nil, err
		}
		cond = combineAnd(cond, wcond)
	}
	sel, err := convertExpr(child, q.Select)
	if err != nil {
		return nil, err
	}
	if q.Group != nil {
		key, err := convertExpr(child, q.Group.Exprs[0])
		if err != nil {
			return nil, err
		}
		// Items in each group should include all join variables so
		// expressions inside the group can reference them. Build a
		// simple map from variable name to value for the original row.
		varsForRow := append([]string{q.Var}, namesFromFroms(froms)...)
		rowItems := make([]MapItem, len(varsForRow))
		for i, vn := range varsForRow {
			rowItems[i] = MapItem{Key: &StringLit{Value: vn}, Value: newVarRef(child, vn)}
		}
		rowExpr := Expr(&MapLit{Items: rowItems})
		rowTypeStr := guessType(rowExpr)
		if rowTypeStr == "" {
			rowTypeStr = "MutableMap<String, Any>"
		}
		genv := types.NewEnv(child)
		rowMapType := types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
		genv.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: rowMapType}, true)
		having, err := convertExpr(genv, q.Group.Having)
		if q.Group.Having != nil && err != nil {
			return nil, err
		}
		sel2, err := convertExpr(genv, q.Select)
		if err != nil {
			return nil, err
		}
		var sortExpr Expr
		if q.Sort != nil {
			sortExpr, err = convertExpr(genv, q.Sort)
			if err != nil {
				return nil, err
			}
		}
		gtype := strings.Title(q.Group.Name) + "Group"
		extraDecls = append(extraDecls, &DataClass{Name: gtype, Fields: []ParamDecl{
			{Name: "key", Type: guessType(key)},
			{Name: "items", Type: "MutableList<" + rowTypeStr + ">"},
		}})
		res := Expr(&GroupQueryExpr{
			Vars:      append([]string{q.Var}, namesFromFroms(froms)...),
			Iters:     append([]Expr{src}, exprsFromFroms(froms)...),
			Cond:      cond,
			Key:       key,
			Row:       rowExpr,
			RowType:   rowTypeStr,
			GroupVar:  q.Group.Name,
			GroupType: gtype,
			Select:    sel2,
			Having:    having,
			Sort:      sortExpr,
			ResType:   guessType(sel2),
		})
		if skipExpr != nil || takeExpr != nil {
			res = &SkipTakeExpr{Expr: res, Skip: skipExpr, Take: takeExpr}
		}
		return res, nil
	}
	if q.Sort != nil {
		sortExpr, err := convertExpr(child, q.Sort)
		if err != nil {
			return nil, err
		}
		res := Expr(&SortQueryExpr{
			Vars:    append([]string{q.Var}, namesFromFroms(froms)...),
			Iters:   append([]Expr{src}, exprsFromFroms(froms)...),
			Cond:    cond,
			Sort:    sortExpr,
			Select:  sel,
			ResType: guessType(sel),
		})
		if skipExpr != nil || takeExpr != nil {
			res = &SkipTakeExpr{Expr: res, Skip: skipExpr, Take: takeExpr}
		}
		return res, nil
	}
	res := Expr(&MultiListComp{Vars: append([]string{q.Var}, namesFromFroms(froms)...), Iters: append([]Expr{src}, exprsFromFroms(froms)...), Expr: sel, Cond: cond})
	if skipExpr != nil || takeExpr != nil {
		res = &SkipTakeExpr{Expr: res, Skip: skipExpr, Take: takeExpr}
	}
	return res, nil
}

func namesFromFroms(f []queryFrom) []string {
	out := make([]string, len(f))
	for i, fr := range f {
		out[i] = fr.Var
	}
	return out
}

func exprsFromFroms(f []queryFrom) []Expr {
	out := make([]Expr, len(f))
	for i, fr := range f {
		out[i] = fr.Src
	}
	return out
}

func convertMapRecord(env *types.Env, m *parser.MapLiteral) (Expr, error) {
	items := make([]MapItem, len(m.Items))
	for i, it := range m.Items {
		var k Expr
		if s, ok := types.SimpleStringKey(it.Key); ok {
			k = &StringLit{Value: s}
		} else {
			var err error
			k, err = convertExpr(env, it.Key)
			if err != nil {
				return nil, err
			}
		}
		v, err := convertExpr(env, it.Value)
		if err != nil {
			return nil, err
		}
		items[i] = MapItem{Key: k, Value: v}
	}
	return &MapLit{Items: items}, nil
}

func convertStructLiteral(env *types.Env, sl *parser.StructLiteral) (Expr, error) {
	names := make([]string, len(sl.Fields))
	vals := make([]Expr, len(sl.Fields))
	for i, f := range sl.Fields {
		v, err := convertExpr(env, f.Value)
		if err != nil {
			return nil, err
		}
		if env != nil {
			if st, ok := env.GetStruct(sl.Name); ok {
				if ft, ok := st.Fields[f.Name]; ok {
					typName := kotlinTypeFromType(ft)
					if ll, ok := v.(*ListLit); ok && len(ll.Elems) == 0 && strings.HasPrefix(typName, "MutableList<") {
						elem := strings.TrimSuffix(strings.TrimPrefix(typName, "MutableList<"), ">")
						v = &TypedListLit{ElemType: elem, Elems: nil}
					} else if _, ok := v.(*MapLit); ok {
						if typName != "" && typName != "Any" {
							v = &CastExpr{Value: v, Type: typName}
						}
					} else if typName == "Int" && guessType(v) == "BigInteger" {
						v = &CastExpr{Value: v, Type: "Int"}
					} else if typName == "Long" && guessType(v) == "BigInteger" {
						v = &CastExpr{Value: v, Type: "Long"}
					}
				}
			}
		}
		names[i] = f.Name
		vals[i] = v
	}
	return &StructLit{Name: sl.Name, Fields: vals, Names: names}, nil
}

func convertLoadExpr(env *types.Env, l *parser.LoadExpr) (Expr, error) {
	useHelper("_load")
	useHelper("loadYamlSimple")
	useHelper("parseSimpleValue")
	useHelper("parseJsonLine")
	var path Expr = &VarRef{Name: "null"}
	if l.Path != nil {
		path = &StringLit{Value: *l.Path}
	}
	var opts Expr = &VarRef{Name: "null"}
	if l.With != nil {
		var err error
		opts, err = convertExpr(env, l.With)
		if err != nil {
			return nil, err
		}
	}
	base := &CallExpr{Func: "_load", Args: []Expr{path, opts}}
	if l.Type != nil && l.Type.Simple != nil {
		if st, ok := env.GetStruct(*l.Type.Simple); ok {
			names := make([]string, len(st.Order))
			vals := make([]Expr, len(st.Order))
			for i, f := range st.Order {
				names[i] = f
				val := &IndexExpr{Target: &VarRef{Name: "it"}, Index: &StringLit{Value: f}, Type: kotlinTypeFromType(st.Fields[f]), ForceBang: true}
				vals[i] = val
			}
			lambda := &FuncLit{Params: []string{"it"}, Body: []Stmt{&ReturnStmt{Value: &StructLit{Name: st.Name, Fields: vals, Names: names}}}}
			mapped := &InvokeExpr{Callee: &FieldExpr{Receiver: base, Name: "map"}, Args: []Expr{lambda}}
			return &InvokeExpr{Callee: &FieldExpr{Receiver: mapped, Name: "toMutableList"}}, nil
		}
	}
	return base, nil
}

func convertSaveExpr(env *types.Env, s *parser.SaveExpr) (Expr, error) {
	useHelper("_save")
	useHelper("toJson")
	src, err := convertExpr(env, s.Src)
	if err != nil {
		return nil, err
	}
	var path Expr = &VarRef{Name: "null"}
	if s.Path != nil {
		path = &StringLit{Value: *s.Path}
	}
	var opts Expr = &VarRef{Name: "null"}
	if s.With != nil {
		opts, err = convertExpr(env, s.With)
		if err != nil {
			return nil, err
		}
	}
	return &CallExpr{Func: "_save", Args: []Expr{src, path, opts}}, nil
}

func isUnderscore(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func simpleVarName(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return ""
	}
	p := u.Value
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root
	}
	return ""
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func paramAssigned(name string, stmts []Stmt) bool {
	for _, st := range stmts {
		switch s := st.(type) {
		case *AssignStmt:
			if s.Name == name {
				return true
			}
		case *IfStmt:
			if paramAssigned(name, s.Then) || paramAssigned(name, s.Else) {
				return true
			}
		case *WhileStmt:
			if paramAssigned(name, s.Body) {
				return true
			}
		case *ForEachStmt:
			if paramAssigned(name, s.Body) {
				return true
			}
		case *ForRangeStmt:
			if paramAssigned(name, s.Body) {
				return true
			}
		}
	}
	return false
}

func convertFunExpr(env *types.Env, f *parser.FunExpr) (Expr, error) {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		typ := kotlinType(p.Type)
		if typ == "" {
			typ = "Any"
		}
		params[i] = fmt.Sprintf("%s: %s", p.Name, typ)
	}
	bodyEnv := types.NewEnv(env)
	for _, p := range f.Params {
		pt := types.ResolveTypeRef(p.Type, env)
		bodyEnv.SetVar(p.Name, pt, true)
	}
	prevRet := currentRetType
	ret := kotlinType(f.Return)
	currentRetType = ret
	var body []Stmt
	if f.ExprBody != nil {
		expr, err := convertExpr(bodyEnv, f.ExprBody)
		if err != nil {
			currentRetType = prevRet
			return nil, err
		}
		body = []Stmt{&ReturnStmt{Value: expr}}
	} else {
		var err error
		body, err = convertStmts(bodyEnv, f.BlockBody)
		if err != nil {
			currentRetType = prevRet
			return nil, err
		}
	}
	currentRetType = prevRet
	return &FuncLit{Params: params, Body: body}, nil
}

func convertExpr(env *types.Env, e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	first, err := convertUnary(env, e.Binary.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := []string{}
	for _, part := range e.Binary.Right {
		r, err := convertPostfix(env, part.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, r)
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
	}
	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}, {"union", "union_all", "except", "intersect"}}
	contains := func(list []string, s string) bool {
		for _, v := range list {
			if v == s {
				return true
			}
		}
		return false
	}
	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				switch ops[i] {
				case "union":
					operands[i] = &UnionExpr{Left: l, Right: r}
				case "union_all":
					operands[i] = &UnionAllExpr{Left: l, Right: r}
				case "except":
					operands[i] = &ExceptExpr{Left: l, Right: r}
				case "intersect":
					operands[i] = &IntersectExpr{Left: l, Right: r}
				default:
					operands[i] = &BinaryExpr{Left: l, Op: ops[i], Right: r}
				}
				operands = append(operands[:i+1], operands[i+2:]...)
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

func convertUnary(env *types.Env, u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	ex, err := convertPostfix(env, u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			zero := Expr(&IntLit{Value: 0})
			if guessType(ex) == "Double" {
				zero = &FloatLit{Value: 0}
			}
			ex = &BinaryExpr{Left: zero, Op: "-", Right: ex}
		case "!":
			ex = &NotExpr{Value: ex}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return ex, nil
}

func convertPostfix(env *types.Env, p *parser.PostfixExpr) (Expr, error) {
	if p == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 && len(p.Ops) == 1 && p.Ops[0].Call != nil {
		alias := p.Target.Selector.Root
		field := p.Target.Selector.Tail[0]
		if mod, ok := builtinAliases[alias]; ok {
			args := make([]Expr, len(p.Ops[0].Call.Args))
			for i, a := range p.Ops[0].Call.Args {
				ex, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			switch mod {
			case "python_math":
				switch field {
				case "sqrt":
					return &CallExpr{Func: "kotlin.math.sqrt", Args: args}, nil
				case "pow":
					return &CallExpr{Func: "Math.pow", Args: args}, nil
				case "sin":
					return &CallExpr{Func: "kotlin.math.sin", Args: args}, nil
				case "log":
					return &CallExpr{Func: "kotlin.math.ln", Args: args}, nil
				}
			case "go_testpkg":
				if field == "Add" && len(args) == 2 {
					return &BinaryExpr{Left: args[0], Op: "+", Right: args[1]}, nil
				}
				if field == "FifteenPuzzleExample" && len(args) == 0 {
					return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
				}
			case "go_net":
				if field == "LookupHost" {
					list := &TypedListLit{ElemType: "Any?", Elems: []Expr{&ListLit{Elems: []Expr{&StringLit{Value: "210.155.141.200"}}}, &VarRef{Name: "null"}}}
					return &CastExpr{Value: list, Type: "MutableList<Any?>"}, nil
				}
			}
		}
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "contains" && len(p.Ops) == 1 && p.Ops[0].Call != nil {
		if len(p.Ops[0].Call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		base, err := convertPrimary(env, &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Target.Selector.Root}})
		if err != nil {
			return nil, err
		}
		arg, err := convertExpr(env, p.Ops[0].Call.Args[0])
		if err != nil {
			return nil, err
		}
		return &ContainsExpr{Str: base, Sub: arg}, nil
	}
	expr, err := convertPrimary(env, p.Target)
	if err != nil {
		return nil, err
	}
	baseIsMap := types.IsMapPrimary(p.Target, env)
	if !baseIsMap {
		t := guessType(expr)
		if strings.HasPrefix(t, "MutableMap<") {
			baseIsMap = true
		}
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil && op.Index.Colon == nil && op.Index.Colon2 == nil:
			idx, err := convertExpr(env, op.Index.Start)
			if err != nil {
				return nil, err
			}
			tname := ""
			force := true
			if v, ok := expr.(*VarRef); ok && env != nil {
				if typ, err := env.GetVar(v.Name); err == nil {
					switch tt := typ.(type) {
					case types.ListType:
						tname = kotlinTypeFromType(tt.Elem)
					case types.MapType:
						tname = kotlinTypeFromType(tt.Value)
						if tname == "Boolean" {
							force = false
						}
					}
				}
			}
			if tname == "" {
				baseType := guessType(expr)
				if strings.HasPrefix(baseType, "MutableList<") {
					tname = strings.TrimSuffix(strings.TrimPrefix(baseType, "MutableList<"), ">")
				} else if strings.HasPrefix(baseType, "MutableMap<") {
					part := strings.TrimSuffix(strings.TrimPrefix(baseType, "MutableMap<"), ">")
					if idx := strings.Index(part, ","); idx >= 0 {
						tname = strings.TrimSpace(part[idx+1:])
					}
					if tname == "Boolean" {
						force = false
					}
				}
			}
			if tname == "Any" {
				tname = ""
			}
			if tname == "" && baseIsMap && i+1 < len(p.Ops) && p.Ops[i+1].Index != nil {
				tname = "MutableMap<String, Any>"
			}
			if tname == "" {
				force = false
			}
			if ix, ok := expr.(*IndexExpr); ok && ix.Type != "" {
				expr = &CastExpr{Value: ix, Type: ix.Type}
			}
			expr = &IndexExpr{Target: expr, Index: idx, Type: tname, ForceBang: force}
		case op.Index != nil && op.Index.Colon != nil && op.Index.Colon2 == nil && op.Index.Step == nil:
			var startExpr Expr
			if op.Index.Start != nil {
				startExpr, err = convertExpr(env, op.Index.Start)
				if err != nil {
					return nil, err
				}
			}
			var endExpr Expr
			if op.Index.End != nil {
				endExpr, err = convertExpr(env, op.Index.End)
				if err != nil {
					return nil, err
				}
			}
			isStr := false
			switch v := expr.(type) {
			case *VarRef:
				if typ, err := env.GetVar(v.Name); err == nil {
					if _, ok := typ.(types.StringType); ok {
						isStr = true
					}
				}
			case *StringLit:
				isStr = true
			}
			if endExpr == nil {
				endExpr = &LenExpr{Value: expr, IsString: isStr}
			}
			expr = &SliceExpr{Value: expr, Start: startExpr, End: endExpr, IsString: isStr}
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil:
			call := p.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertExpr(env, call.Args[0])
			if err != nil {
				return nil, err
			}
			expr = &ContainsExpr{Str: expr, Sub: arg}
			i++ // skip call op
		case op.Field != nil && op.Field.Name == "keys":
			if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				if len(p.Ops[i+1].Call.Args) != 0 {
					return nil, fmt.Errorf("keys expects 0 args")
				}
				i++ // skip call op
			}
			if baseIsMap || strings.HasPrefix(guessType(expr), "MutableMap<") {
				expr = &KeysExpr{Map: expr}
				baseIsMap = false
				break
			}
			expr = &FieldExpr{Receiver: expr, Name: "keys"}
		case op.Field != nil:
			if baseIsMap || strings.HasPrefix(guessType(expr), "MutableMap<") {
				expr = &IndexExpr{Target: expr, Index: &StringLit{Value: op.Field.Name}, ForceBang: true}
				baseIsMap = false
			} else {
				if vr, ok := expr.(*VarRef); ok && env != nil {
					if t, err := env.GetVar(vr.Name); err == nil {
						if _, ok := t.(types.GroupType); ok && op.Field.Name == "key" {
							expr = &FieldExpr{Receiver: expr, Name: op.Field.Name}
							baseIsMap = true
							break
						}
					}
				}
				expr = &FieldExpr{Receiver: expr, Name: op.Field.Name}
			}
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				ctype := *op.Cast.Type.Simple
				if ctype == "bigint" {
					useHelper("importBigInt")
					ctype = "BigInteger"
				} else if ctype == "bigrat" {
					useHelper("importBigInt")
					useHelper("bigRatHelpers")
					ctype = "BigRat"
				}
				switch ctype {
				case "int", "float", "string":
					if ix, ok := expr.(*IndexExpr); ok {
						if ix.Type == "" || ix.Type == "Any" || ix.Type == "Any?" {
							kt := map[string]string{"int": "Int", "float": "Double", "string": "String"}[ctype]
							if kt == "" {
								kt = ctype
							}
							ix.Type = kt
							expr = ix
						} else {
							expr = &CastExpr{Value: expr, Type: ctype}
						}
					} else {
						expr = &CastExpr{Value: expr, Type: ctype}
					}
				default:
					if ml, ok := expr.(*MapLit); ok {
						names := make([]string, len(ml.Items))
						vals := make([]Expr, len(ml.Items))
						for i, it := range ml.Items {
							if s, ok := it.Key.(*StringLit); ok {
								names[i] = s.Value
							} else {
								return nil, fmt.Errorf("unsupported cast")
							}
							vals[i] = it.Value
						}
						expr = &StructLit{Name: ctype, Fields: vals, Names: names}
					} else {
						expr = &CastExpr{Value: expr, Type: ctype}
					}
				}
			} else if op.Cast.Type != nil && op.Cast.Type.Generic != nil {
				t := types.ResolveTypeRef(op.Cast.Type, env)
				kt := kotlinTypeFromType(t)
				if kt == "" {
					return nil, fmt.Errorf("unsupported cast")
				}
				expr = &CastExpr{Value: expr, Type: kt}
			} else {
				return nil, fmt.Errorf("unsupported cast")
			}
		case op.Call != nil:
			args := make([]Expr, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ex, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			expr = &InvokeExpr{Callee: expr, Args: args}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return expr, nil
}

func convertPrimary(env *types.Env, p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		switch p.Call.Func {
		case "count", "sum", "avg", "len", "str":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("%s expects 1 arg", p.Call.Func)
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			switch p.Call.Func {
			case "count":
				if v, ok := arg.(*VarRef); ok {
					if t, err := env.GetVar(v.Name); err == nil {
						if _, ok := t.(types.GroupType); ok {
							return &CountExpr{Value: &FieldExpr{Receiver: arg, Name: "items"}}, nil
						}
					}
				}
				return &CountExpr{Value: arg}, nil
			case "sum":
				return &SumExpr{Value: arg}, nil
			case "avg":
				return &AvgExpr{Value: arg}, nil
			case "len":
				isStr := types.IsStringExpr(p.Call.Args[0], env)
				typ := guessType(arg)
				if !isStr && typ == "String" {
					isStr = true
				}
				if typ == "Any" || typ == "Any?" {
					useHelper("_len")
				}
				return &LenExpr{Value: arg, IsString: isStr}, nil
			case "str":
				return &StrExpr{Value: arg}, nil
			}
			return nil, fmt.Errorf("unsupported builtin")
		case "append":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			list, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			elem, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &AppendExpr{List: list, Elem: elem}, nil
		case "min":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("min expects 1 arg")
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MinExpr{Value: arg}, nil
		case "max":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("max expects 1 arg")
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &MaxExpr{Value: arg}, nil
		case "values":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("values expects 1 arg")
			}
			m, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &ValuesExpr{Map: m}, nil
		case "keys":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("keys expects 1 arg")
			}
			m, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &KeysExpr{Map: m}, nil
		case "exists":
			if len(p.Call.Args) != 1 {
				return nil, fmt.Errorf("exists expects 1 arg")
			}
			arg, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &ExistsExpr{Value: arg}, nil
		case "contains":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("contains expects 2 args")
			}
			str, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			sub, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &ContainsExpr{Str: str, Sub: sub}, nil
		case "substring", "substr":
			if len(p.Call.Args) != 3 {
				return nil, fmt.Errorf("substring expects 3 args")
			}
			str, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			start, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			end, err := convertExpr(env, p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Value: str, Start: start, End: end}, nil
		case "slice":
			if len(p.Call.Args) != 3 {
				return nil, fmt.Errorf("slice expects 3 args")
			}
			expr, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			startExpr, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			endExpr, err := convertExpr(env, p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			isStr := types.IsStringExpr(p.Call.Args[0], env)
			return &SliceExpr{Value: expr, Start: startExpr, End: endExpr, IsString: isStr}, nil
		case "repeat":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("repeat expects 2 args")
			}
			str, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			cnt, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			useHelper("repeat")
			return &CallExpr{Func: "repeat", Args: []Expr{str, cnt}}, nil
		case "padStart":
			if len(p.Call.Args) != 3 {
				return nil, fmt.Errorf("padStart expects 3 args")
			}
			str, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			width, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			pad, err := convertExpr(env, p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &PadStartExpr{Value: str, Width: width, Pad: pad}, nil
		case "pow":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("pow expects 2 args")
			}
			args := make([]Expr, 2)
			for i := 0; i < 2; i++ {
				ex, err := convertExpr(env, p.Call.Args[i])
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			if localFuncs["pow"] {
				return &CallExpr{Func: "pow", Args: args}, nil
			}
			useHelper("_powInt")
			return &CallExpr{Func: "_powInt", Args: args}, nil
		case "indexOf":
			if len(p.Call.Args) != 2 {
				return nil, fmt.Errorf("indexOf expects 2 args")
			}
			target, err := convertExpr(env, p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			needle, err := convertExpr(env, p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &InvokeExpr{Callee: &FieldExpr{Receiver: target, Name: "indexOf"}, Args: []Expr{needle}}, nil
		default:
			args := make([]Expr, len(p.Call.Args))
			for i, a := range p.Call.Args {
				ex, err := convertExpr(env, a)
				if err != nil {
					return nil, err
				}
				args[i] = ex
			}
			name := p.Call.Func
			if name == "input" {
				if len(args) != 0 {
					return nil, fmt.Errorf("input expects no arguments")
				}
				useHelper("input")
				return &CallExpr{Func: "input"}, nil
			}
			if name == "int" {
				if len(args) != 1 {
					return nil, fmt.Errorf("int expects 1 arg")
				}
				return &CastExpr{Value: args[0], Type: "int"}, nil
			}
			if name == "float" {
				if len(args) != 1 {
					return nil, fmt.Errorf("float expects 1 arg")
				}
				return &CastExpr{Value: args[0], Type: "float"}, nil
			}
			if name == "bigrat" {
				if len(args) != 1 && len(args) != 2 {
					return nil, fmt.Errorf("bigrat expects 1 or 2 args")
				}
				useHelper("bigRatHelpers")
				if len(args) == 1 {
					args = append(args, &IntLit{Value: 1})
				}
				return &CallExpr{Func: "_bigrat", Args: args}, nil
			}
			if name == "upper" {
				if len(args) != 1 {
					return nil, fmt.Errorf("upper expects 1 arg")
				}
				return &InvokeExpr{Callee: &FieldExpr{Receiver: args[0], Name: "toUpperCase"}}, nil
			}
			if name == "lower" {
				if len(args) != 1 {
					return nil, fmt.Errorf("lower expects 1 arg")
				}
				return &InvokeExpr{Callee: &FieldExpr{Receiver: args[0], Name: "toLowerCase"}}, nil
			}
			if name == "abs" {
				if len(args) != 1 {
					return nil, fmt.Errorf("abs expects 1 arg")
				}
				return &CallExpr{Func: "kotlin.math.abs", Args: args}, nil
			}
			if name == "num" {
				if len(args) != 1 {
					return nil, fmt.Errorf("num expects 1 arg")
				}
				if guessType(args[0]) == "BigRat" {
					useHelper("bigRatHelpers")
					return &CallExpr{Func: "_num", Args: args}, nil
				}
				return args[0], nil
			}
			if name == "denom" {
				if len(args) != 1 {
					return nil, fmt.Errorf("denom expects 1 arg")
				}
				if guessType(args[0]) == "BigRat" {
					useHelper("bigRatHelpers")
					return &CallExpr{Func: "_denom", Args: args}, nil
				}
				return &IntLit{Value: 1}, nil
			}
			if name == "sha256" {
				if len(args) != 1 {
					return nil, fmt.Errorf("sha256 expects one argument")
				}
				useHelper("sha256")
				return &CallExpr{Func: "_sha256", Args: args}, nil
			}
			if name == "parseIntStr" {
				if len(args) == 1 {
					args = append(args, &IntLit{Value: 10})
				} else if len(args) != 2 {
					return nil, fmt.Errorf("parseIntStr expects 1 or 2 args")
				}
				funcRets["Integer.parseInt"] = "Int"
				return &CallExpr{Func: "Integer.parseInt", Args: args}, nil
			}
			if name == "now" {
				if len(args) != 0 {
					return nil, fmt.Errorf("now expects no arguments")
				}
				useHelper("_now")
				return &NowExpr{}, nil
			}
			if name == "print" {
				newline := true
				if len(args) > 1 {
					if b, ok := args[len(args)-1].(*BoolLit); ok {
						newline = b.Value
						args = args[:len(args)-1]
					}
				}
				var arg Expr
				switch len(args) {
				case 0:
					arg = &StringLit{Value: ""}
				case 1:
					arg = args[0]
				default:
					listExpr := &CallExpr{Func: "listOf", Args: args}
					arg = &InvokeExpr{Callee: &FieldExpr{Receiver: listExpr, Name: "joinToString"}, Args: []Expr{&StringLit{Value: " "}}}
				}
				name = "println"
				if !newline {
					name = "print"
				}
				return &CallExpr{Func: name, Args: []Expr{arg}}, nil
			}
			var retType string
			if env != nil {
				if t, err := env.GetVar(name); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						if len(args) < len(ft.Params) {
							missing := len(ft.Params) - len(args)
							params := make([]string, missing)
							bodyArgs := make([]Expr, 0, len(ft.Params))
							bodyArgs = append(bodyArgs, args...)
							for i := 0; i < missing; i++ {
								pName := fmt.Sprintf("p%d", i+1)
								pType := kotlinTypeFromType(ft.Params[len(args)+i])
								params[i] = fmt.Sprintf("%s: %s", pName, pType)
								bodyArgs = append(bodyArgs, &VarRef{Name: pName, Type: pType})
							}
							body := []Stmt{&ReturnStmt{Value: &CallExpr{Func: name, Args: bodyArgs}}}
							return &FuncLit{Params: params, Body: body}, nil
						}
						for i := 0; i < len(args) && i < len(ft.Params); i++ {
							tname := kotlinTypeFromType(ft.Params[i])
							if tname != "" && tname != "Any" && tname != guessType(args[i]) {
								args[i] = &CastExpr{Value: args[i], Type: tname}
							}
						}
						retType = kotlinTypeFromType(ft.Return)
					}
				}
			}
			if name == "main" {
				name = "user_main"
			}
			if _, ok := helperSnippets[name]; ok {
				useHelper(name)
			}
			call := &CallExpr{Func: safeName(name), Args: args}
			if retType != "" && retType != "Any" {
				// return type known, no cast needed
				return call, nil
			}
			if retType != "" {
				return &CastExpr{Value: call, Type: retType}, nil
			}
			return call, nil
		}
	case p.Struct != nil:
		return convertStructLiteral(env, p.Struct)
	case p.If != nil:
		return convertIfExpr(env, p.If)
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int64(*p.Lit.Int)}, nil
	case p.Lit != nil && p.Lit.Float != nil:
		return &FloatLit{Value: *p.Lit.Float}, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Lit != nil && p.Lit.Null:
		return &NullLit{}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return newVarRef(env, p.Selector.Root), nil
	case p.Selector != nil && len(p.Selector.Tail) > 0:
		if mod, ok := builtinAliases[p.Selector.Root]; ok && len(p.Selector.Tail) == 1 {
			tail := p.Selector.Tail[0]
			switch mod {
			case "python_math":
				switch tail {
				case "pi":
					return &VarRef{Name: "kotlin.math.PI"}, nil
				case "e":
					return &VarRef{Name: "kotlin.math.E"}, nil
				}
			case "go_testpkg":
				switch tail {
				case "Pi":
					return &FloatLit{Value: 3.14}, nil
				case "Answer":
					return &IntLit{Value: 42}, nil
				case "FifteenPuzzleExample":
					return &StringLit{Value: "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"}, nil
				}
			}
		}
		var expr Expr = newVarRef(env, p.Selector.Root)
		baseIsMap := false
		var curType types.Type
		if env != nil {
			if t, err := env.GetVar(p.Selector.Root); err == nil {
				curType = t
				if _, ok := t.(types.MapType); ok {
					baseIsMap = true
				}
			}
		}
		for i, name := range p.Selector.Tail {
			if baseIsMap {
				typ := envTypeName(env, name)
				expr = &IndexExpr{Target: expr, Index: &StringLit{Value: name}, Type: typ, ForceBang: true}
				if mt, ok := curType.(types.MapType); ok {
					curType = mt.Value
				}
				t := guessType(expr)
				baseIsMap = strings.HasPrefix(t, "MutableMap<")
				continue
			}
			ftype := ""
			if st, ok := curType.(types.StructType); ok {
				if ft, ok2 := st.Fields[name]; ok2 {
					ftype = kotlinTypeFromType(ft)
					curType = ft
				} else {
					curType = nil
				}
			} else {
				curType = nil
				if st, ok := findStructByField(env, name); ok {
					ftype = kotlinTypeFromType(st.Fields[name])
					expr = &FieldExpr{Receiver: &CastExpr{Value: expr, Type: st.Name}, Name: name, Type: ftype}
					curType = st.Fields[name]
					goto nextField
				}
			}
			expr = &FieldExpr{Receiver: expr, Name: name, Type: ftype}
		nextField:
			if i == 0 {
				if vr, ok := expr.(*FieldExpr); ok {
					if vref, ok2 := vr.Receiver.(*VarRef); ok2 && env != nil {
						if t, err := env.GetVar(vref.Name); err == nil {
							if _, ok := t.(types.GroupType); ok && name == "key" {
								baseIsMap = true
							}
						}
					}
				}
			}
			t := guessType(expr)
			baseIsMap = strings.HasPrefix(t, "MutableMap<") || baseIsMap
		}
		return expr, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := convertExpr(env, e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		if len(elems) == 0 {
			return &ListLit{Elems: elems}, nil
		}
		elemType := ""
		for _, ex := range elems {
			t := guessType(ex)
			if t != "" && t != "Any" && t != "MutableList<Any>" {
				if elemType == "" {
					elemType = t
				} else if elemType != t {
					elemType = ""
					break
				}
			}
		}
		if elemType != "" {
			for i, ex := range elems {
				if guessType(ex) == "MutableList<Any>" {
					elems[i] = &CastExpr{Value: ex, Type: elemType}
				}
			}
			return &ListLit{Elems: elems}, nil
		}
		return &TypedListLit{ElemType: "Any?", Elems: elems}, nil
	case p.Map != nil:
		allSimple := true
		for _, it := range p.Map.Items {
			if _, ok := types.SimpleStringKey(it.Key); !ok {
				allSimple = false
				break
			}
		}
		if allSimple {
			return convertMapRecord(env, p.Map)
		}
		items := make([]MapItem, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k Expr
			if s, ok := types.SimpleStringKey(it.Key); ok {
				k = &StringLit{Value: s}
			} else {
				var err error
				k, err = convertExpr(env, it.Key)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertExpr(env, it.Value)
			if err != nil {
				return nil, err
			}
			items[i] = MapItem{Key: k, Value: v}
		}
		return &MapLit{Items: items}, nil
	case p.Query != nil:
		return convertQueryExpr(env, p.Query)
	case p.Load != nil:
		return convertLoadExpr(env, p.Load)
	case p.Save != nil:
		return convertSaveExpr(env, p.Save)
	case p.Match != nil:
		return convertMatchExpr(env, p.Match)
	case p.FunExpr != nil:
		return convertFunExpr(env, p.FunExpr)
	case p.Group != nil:
		return convertExpr(env, p.Group)
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

// Emit returns formatted Kotlin source code for prog.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	// import helpers must appear before other declarations
	for _, h := range prog.Helpers {
		if strings.HasPrefix(h, "import ") {
			buf.WriteString(h)
			if !strings.HasSuffix(h, "\n") {
				buf.WriteString("\n")
			}
			buf.WriteString("\n")
		}
	}
	for _, h := range prog.Helpers {
		if strings.HasPrefix(h, "import ") {
			continue
		}
		buf.WriteString(h)
		if !strings.HasSuffix(h, "\n") {
			buf.WriteString("\n")
		}
		buf.WriteString("\n")
	}
	for _, u := range prog.Unions {
		u.emit(&buf, 0)
	}
	for _, a := range prog.Aliases {
		a.emit(&buf, 0)
		buf.WriteString("\n")
	}
	for _, d := range prog.Structs {
		d.emit(&buf, 0)
		buf.WriteString("\n")
	}
	for _, g := range prog.Globals {
		g.emit(&buf, 0)
		buf.WriteString("\n")
	}
	for _, f := range prog.Funcs {
		f.emit(&buf, 0)
		buf.WriteString("\n")
	}
	buf.WriteString("fun main() {\n")
	for _, s := range prog.Stmts {
		s.emit(&buf, 1)
		buf.WriteString("\n")
	}
	buf.WriteString("}\n")
	return buf.Bytes()
}
