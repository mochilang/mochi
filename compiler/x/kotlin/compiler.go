//go:build slow

package kotlin

import (
	"bytes"
	"fmt"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"mochi/compiler/meta"
	"mochi/parser"
	"mochi/types"
)

var runtimePieces = map[string]string{
	"append": `fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}`,
	"avg": `fun avg(list: List<Any?>): Number {
    if (list.isEmpty()) return 0
    var s = 0.0
    for (n in list) s += toDouble(n)
    val r = s / list.size
    return if (r % 1.0 == 0.0) r.toInt() else r
}`,
	"div": `fun div(a: Any?, b: Any?): Double {
    val x = toDouble(a)
    val y = toDouble(b)
    return if (y == 0.0) 0.0 else x / y
}`,
	"count":  `fun count(list: Collection<Any?>): Int = list.size`,
	"exists": `fun exists(list: Collection<Any?>): Boolean = list.isNotEmpty()`,
	"values": `fun <T> values(m: Map<*, T>): MutableList<T> = m.values.toMutableList()`,
	"len": `fun len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> 0
}`,
	"max": `fun max(list: List<Any?>): Any? {
    if (list.isEmpty()) return 0
    var m = list[0]
    for (n in list) {
        if ((n as Comparable<Any?>) > (m as Comparable<Any?>)) m = n
    }
    return m
}`,
	"min": `fun min(list: List<Any?>): Any? {
    if (list.isEmpty()) return 0
    var m = list[0]
    for (n in list) {
        if ((n as Comparable<Any?>) < (m as Comparable<Any?>)) m = n
    }
    return m
}`,
	"sum": `fun sum(list: List<Any?>): Number {
    var s = 0.0
    var allInt = true
    for (n in list) {
        val d = toDouble(n)
        if (d % 1.0 != 0.0) allInt = false
        s += d
    }
    return if (allInt) s.toInt() else s
}`,
	"str":         `fun str(v: Any?): String = v.toString()`,
	"substring":   `fun substring(s: String, start: Int, end: Int): String = s.substring(start, end)`,
	"starts_with": `fun String.starts_with(prefix: String): Boolean = this.startsWith(prefix)`,
	"toInt": `fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}`,
	"toDouble": `fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}`,
	"toBool": `fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}`,
	"union": `fun <T> union(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = a.toMutableList()
    for (x in b) if (!res.contains(x)) res.add(x)
    return res
}`,
	"except": `fun <T> except(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (!b.contains(x)) res.add(x)
    return res
}`,
	"intersect": `fun <T> intersect(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (b.contains(x)) res.add(x)
    return res
}`,
	"_load": `fun _load(path: String?, opts: Map<String, Any?>?): MutableList<MutableMap<String, Any?>> {
    val fmt = opts?.get("format") as? String ?: "csv"
    val lines = if (path == null || path == "-") {
        listOf<String>()
    } else {
        var f = java.io.File(path)
        if (!f.isAbsolute) {
            if (!f.exists()) {
                System.getenv("MOCHI_ROOT")?.let { root ->
                    var clean = path
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
	"json": `fun json(v: Any?) {
    println(toJson(v))
}`,
	"toJson": `fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}`,
	"Group":  `class Group<K, T>(val key: K, val items: MutableList<T>) : MutableList<T> by items`,
	"_input": `fun _input(): String { return readLine() ?: "" }`,
}

var runtimeOrder = []string{"append", "avg", "div", "count", "exists", "values", "len", "max", "min", "sum", "str", "substring", "starts_with", "toInt", "toDouble", "toBool", "union", "except", "intersect", "_load", "loadYamlSimple", "parseSimpleValue", "_save", "json", "toJson", "Group", "_input"}

func buildRuntime(used map[string]bool) string {
	var parts []string
	for _, n := range runtimeOrder {
		if used[n] {
			parts = append(parts, runtimePieces[n])
		}
	}
	return strings.Join(parts, "\n\n")
}

// Compiler converts a subset of Mochi programs to Kotlin source code.
type Compiler struct {
	buf         bytes.Buffer
	indent      int
	env         *types.Env
	tmpCount    int
	structCount int
	used        map[string]bool

	// inferred struct types from list of maps
	inferred   map[string]types.StructType
	mapNodes   map[*parser.MapLiteral]string
	queryTypes map[*parser.QueryExpr]types.Type

	srcName string
}

// New creates a new Kotlin compiler.
func New(env *types.Env, srcName string) *Compiler {
	return &Compiler{
		env:         env,
		tmpCount:    0,
		structCount: 0,
		used:        make(map[string]bool),
		inferred:    make(map[string]types.StructType),
		mapNodes:    make(map[*parser.MapLiteral]string),
		queryTypes:  make(map[*parser.QueryExpr]types.Type),
		srcName:     srcName,
	}
}

// Compile generates Kotlin code from prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.used = make(map[string]bool)
	c.inferred = make(map[string]types.StructType)
	c.mapNodes = make(map[*parser.MapLiteral]string)

	if c.srcName != "" {
		c.writeln("// Code generated from " + c.srcName)
		c.writeln("")
	}

	// Structural inference infers struct types from map literals.
	c.discoverStructs(prog)

	// handle builtin imports
	for _, s := range prog.Statements {
		if s.Import != nil {
			if _, err := c.builtinImport(s.Import); err != nil {
				return nil, err
			}
		}
	}

	hasMain := false
	for _, s := range prog.Statements {
		if s.Fun != nil && s.Fun.Name == "main" {
			hasMain = true
		}
	}

	// emit type declarations first
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.typeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// emit global variable declarations before functions so they are
	// visible to all functions
	for _, s := range prog.Statements {
		if s.Type != nil || s.Fun != nil || s.Import != nil {
			continue
		}
		if s.Let != nil || s.Var != nil {
			if err := c.stmt(s); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	for _, s := range prog.Statements {
		if s.Import != nil {
			continue
		}
		if s.Fun != nil {
			if err := c.funDecl(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	if !hasMain {
		c.writeln("fun main() {")
		c.indent++
		for _, s := range prog.Statements {
			if s.Fun != nil || s.Type != nil || s.Import != nil {
				continue
			}
			if s.Let != nil || s.Var != nil {
				// already emitted as global variable
				continue
			}
			if err := c.stmt(s); err != nil {
				return nil, err
			}
		}
		c.indent--
		c.writeln("}")
	}

	body := c.buf.Bytes()

	// Emit any inferred structs discovered during compilation.
	names := make([]string, 0, len(c.inferred))
	for name := range c.inferred {
		names = append(names, name)
	}
	sort.Strings(names)
	var structsBuf bytes.Buffer
	for _, name := range names {
		st := c.inferred[name]
		fields := make([]string, len(st.Order))
		for i, f := range st.Order {
			fields[i] = fmt.Sprintf("var %s: %s", escapeIdent(f), kotlinTypeOf(st.Fields[f]))
		}
		structsBuf.WriteString(fmt.Sprintf("data class %s(%s)\n\n", st.Name, strings.Join(fields, ", ")))
	}

	rt := buildRuntime(c.used)
	var out bytes.Buffer
	out.Write(meta.Header("//"))
	if structsBuf.Len() > 0 {
		out.Write(structsBuf.Bytes())
	}
	if rt != "" {
		out.WriteString(rt)
		out.WriteByte('\n')
	}
	out.Write(body)
	code := escapeKeywords(out.String())
	return []byte(code), nil
}

func (c *Compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		var val, typ string
		if s.Let.Value != nil {
			// Preserve explicit type when assigning an empty list or map literal.
			if s.Let.Type != nil && (isEmptyListLiteral(s.Let.Value) || isEmptyMapLiteral(s.Let.Value)) {
				val = c.zeroValue(s.Let.Type)
			} else {
				v, err := c.expr(s.Let.Value)
				if err != nil {
					return err
				}
				val = v
			}
			if s.Let.Type != nil {
				typ = ": " + c.typeName(s.Let.Type)
			}
		} else {
			val = "null"
			if s.Let.Type != nil {
				typ = ": " + c.typeNameNullable(s.Let.Type)
				val = c.zeroValue(s.Let.Type)
			}
		}
		c.writeln(fmt.Sprintf("val %s%s = %s", escapeIdent(s.Let.Name), typ, val))
		var t types.Type
		if s.Let.Type != nil {
			t = types.ResolveTypeRef(s.Let.Type, c.env)
		} else if s.Let.Value != nil {
			t = c.inferExprType(s.Let.Value)
			if isAnyType(t) {
				t = c.inferListLike(s.Let.Value)
			} else if lt, ok := t.(types.ListType); ok {
				if isAnyType(lt.Elem) {
					t = c.inferListLike(s.Let.Value)
				}
			}
		}
		if t != nil {
			if _, ok := t.(types.AnyType); !ok {
				if cur, err := c.env.GetVar(s.Let.Name); err != nil || isAnyType(cur) {
					c.env.SetVar(s.Let.Name, t, false)
				}
			}
		}
	case s.Var != nil:
		var val, typ string
		if s.Var.Value != nil {
			if s.Var.Type != nil && (isEmptyListLiteral(s.Var.Value) || isEmptyMapLiteral(s.Var.Value)) {
				val = c.zeroValue(s.Var.Type)
			} else {
				v, err := c.expr(s.Var.Value)
				if err != nil {
					return err
				}
				val = v
			}
			if s.Var.Type != nil {
				typ = ": " + c.typeName(s.Var.Type)
			}
		} else {
			val = "null"
			if s.Var.Type != nil {
				typ = ": " + c.typeNameNullable(s.Var.Type)
				val = c.zeroValue(s.Var.Type)
			}
		}
		c.writeln(fmt.Sprintf("var %s%s = %s", escapeIdent(s.Var.Name), typ, val))
		var t types.Type
		if s.Var.Type != nil {
			t = types.ResolveTypeRef(s.Var.Type, c.env)
		} else if s.Var.Value != nil {
			t = c.inferExprType(s.Var.Value)
			if isAnyType(t) {
				t = c.inferListLike(s.Var.Value)
			} else if lt, ok := t.(types.ListType); ok {
				if isAnyType(lt.Elem) {
					t = c.inferListLike(s.Var.Value)
				}
			}
		}
		if t != nil {
			if _, ok := t.(types.AnyType); !ok {
				if cur, err := c.env.GetVar(s.Var.Name); err != nil || isAnyType(cur) {
					c.env.SetVar(s.Var.Name, t, true)
				}
			}
		}
	case s.Assign != nil:
		v, err := c.expr(s.Assign.Value)
		if err != nil {
			return err
		}
		target := escapeIdent(s.Assign.Name)
		for i, idx := range s.Assign.Index {
			idxVal, err := c.expr(idx.Start)
			if err != nil {
				return err
			}
			if i < len(s.Assign.Index)-1 || len(s.Assign.Field) > 0 {
				target += fmt.Sprintf("[%s]!!", idxVal)
			} else {
				target += fmt.Sprintf("[%s]", idxVal)
			}
		}
		for _, f := range s.Assign.Field {
			target += "." + f.Name
		}
		c.writeln(fmt.Sprintf("%s = %s", target, v))
	case s.Return != nil:
		v, err := c.expr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("return %s", v))
	case s.If != nil:
		return c.ifStmt(s.If)
	case s.While != nil:
		return c.whileStmt(s.While)
	case s.For != nil:
		return c.forStmt(s.For)
	case s.Break != nil:
		c.writeln("break")
	case s.Continue != nil:
		c.writeln("continue")
	case s.Fun != nil:
		return c.funDecl(s.Fun)
	case s.Expect != nil:
		cond, err := c.expr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.writeln("check(" + cond + ")")
	case s.Test != nil:
		// Skip test blocks when compiling dataset programs. The runtime
		// output will be validated separately by the Go tests.
		return nil
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Import != nil:
		handled, err := c.builtinImport(s.Import)
		if err != nil {
			return err
		}
		if !handled {
			return fmt.Errorf("unsupported import at line %d", s.Pos.Line)
		}
		return nil
	case s.Type != nil:
		// type declarations are emitted before main
		return nil
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
		// extern declarations have no effect
		return nil
	case s.Expr != nil:
		e, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(e)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) funDecl(f *parser.FunStmt) error {
	// emit basic KDoc with parameter and return type information
	c.writeln("/**")
	c.writeln(" * Auto-generated from Mochi")
	for _, p := range f.Params {
		typ := "Any"
		if p.Type != nil {
			typ = c.typeName(p.Type)
		}
		c.writeln(fmt.Sprintf(" * @param %s %s", p.Name, typ))
	}
	if f.Return != nil {
		c.writeln(fmt.Sprintf(" * @return %s", c.typeName(f.Return)))
	}
	c.writeln(" */")
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeName(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	ret := "Unit"
	if f.Return != nil {
		ret = c.typeName(f.Return)
	}
	prefix := ""
	if isTailRecursive(f) {
		prefix = "tailrec "
	}
	c.writeln(fmt.Sprintf("%sfun %s(%s): %s {", prefix, f.Name, strings.Join(params, ", "), ret))
	oldEnv := c.env
	c.env = types.NewEnv(c.env)
	for _, p := range f.Params {
		var pt types.Type = types.AnyType{}
		if p.Type != nil {
			pt = types.ResolveTypeRef(p.Type, oldEnv)
		}
		c.env.SetVar(p.Name, pt, false)
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			c.env = oldEnv
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.env = oldEnv
	return nil
}

func (c *Compiler) ifStmt(i *parser.IfStmt) error {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return err
	}
	if _, ok := c.inferExprType(i.Cond).(types.BoolType); !ok {
		c.use("toBool")
		cond = "toBool(" + cond + ")"
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range i.Then {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if i.ElseIf != nil {
		c.writeln("else")
		return c.ifStmt(i.ElseIf)
	}
	if len(i.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.stmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) whileStmt(w *parser.WhileStmt) error {
	cond, err := c.expr(w.Cond)
	if err != nil {
		return err
	}
	if _, ok := c.inferExprType(w.Cond).(types.BoolType); !ok {
		c.use("toBool")
		cond = "toBool(" + cond + ")"
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range w.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) forStmt(f *parser.ForStmt) error {
	var elem types.Type
	if f.RangeEnd != nil {
		start, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.expr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (%s in %s until %s) {", escapeIdent(f.Name), start, end))
		elem = types.IntType{}
	} else {
		src, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		t := types.TypeOfExprBasic(f.Source, c.env)
		if types.IsMapType(t) {
			c.writeln(fmt.Sprintf("for (%s in %s.keys) {", escapeIdent(f.Name), src))
			if mt, ok := t.(types.MapType); ok {
				elem = mt.Key
			}
		} else {
			c.writeln(fmt.Sprintf("for (%s in %s) {", escapeIdent(f.Name), src))
			if lt, ok := t.(types.ListType); ok {
				elem = lt.Elem
			}
		}
	}

	oldEnv := c.env
	if elem != nil {
		c.env = types.NewEnv(c.env)
		c.env.SetVar(f.Name, elem, true)
	}

	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			if elem != nil {
				c.env = oldEnv
			}
			return err
		}
	}
	c.indent--
	c.writeln("}")

	if elem != nil {
		c.env = oldEnv
	}
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := u.Target
	idx := fmt.Sprintf("_i%d", c.tmpCount)
	item := fmt.Sprintf("_it%d", c.tmpCount)
	c.tmpCount++

	c.writeln(fmt.Sprintf("for (%s in 0 until %s.size) {", idx, list))
	c.indent++
	c.writeln(fmt.Sprintf("var %s = %s[%s]", item, list, idx))

	origEnv := c.env
	c.env = types.NewEnv(c.env)

	if t, err := c.env.GetVar(u.Target); err == nil {
		if lt, ok := t.(types.ListType); ok {
			if st, ok := lt.Elem.(types.StructType); ok {
				for _, f := range st.Order {
					c.writeln(fmt.Sprintf("val %s = %s.%s", escapeIdent(f), item, escapeIdent(f)))
					c.env.SetVar(f, st.Fields[f], true)
				}
			}
		}
	}

	if u.Where != nil {
		cond, err := c.expr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent++
	}

	for _, it := range u.Set.Items {
		key, ok := identName(it.Key)
		valExpr, err := c.expr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		if ok {
			c.writeln(fmt.Sprintf("%s.%s = %s", item, escapeIdent(key), valExpr))
		} else {
			keyExpr, err := c.expr(it.Key)
			if err != nil {
				c.env = origEnv
				return err
			}
			c.writeln(fmt.Sprintf("(%s as MutableMap<Any?, Any?>)[%s] = %s", item, keyExpr, valExpr))
		}
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.writeln(fmt.Sprintf("%s[%s] = %s", list, idx, item))
	c.env = origEnv
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) typeDecl(t *parser.TypeDecl) error {
	if len(t.Members) == 0 && len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("sealed class %s", t.Name))
		c.writeln("")
		for _, v := range t.Variants {
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("object %s : %s()", v.Name, t.Name))
				continue
			}
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = fmt.Sprintf("val %s: %s", escapeIdent(f.Name), c.typeName(f.Type))
			}
			c.writeln(fmt.Sprintf("data class %s(%s) : %s()", v.Name, strings.Join(fields, ", "), t.Name))
		}
		return nil
	}
	if len(t.Members) == 0 {
		return nil
	}
	fields := make([]string, 0, len(t.Members))
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, fmt.Sprintf("var %s: %s", escapeIdent(m.Field.Name), c.typeName(m.Field.Type)))
		}
	}
	c.writeln(fmt.Sprintf("data class %s(%s)", t.Name, strings.Join(fields, ", ")))
	return nil
}

func (c *Compiler) typeName(t *parser.TypeRef) string {
	if t == nil {
		return "Any"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.typeName(p)
		}
		ret := "Unit"
		if t.Fun.Return != nil {
			ret = c.typeName(t.Fun.Return)
		}
		return fmt.Sprintf("(%s) -> %s", strings.Join(params, ", "), ret)
	}
	if t.Generic != nil {
		name := t.Generic.Name
		switch name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return fmt.Sprintf("MutableList<%s>", c.typeName(t.Generic.Args[0]))
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return fmt.Sprintf("MutableMap<%s, %s>", c.typeName(t.Generic.Args[0]), c.typeName(t.Generic.Args[1]))
			}
		}
		args := make([]string, len(t.Generic.Args))
		for i, a := range t.Generic.Args {
			args[i] = c.typeName(a)
		}
		return fmt.Sprintf("%s<%s>", strings.Title(name), strings.Join(args, ", "))
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "string":
			return "String"
		case "bool":
			return "Boolean"
		}
		return strings.Title(*t.Simple)
	}
	return "Any"
}

func (c *Compiler) typeNameNullable(t *parser.TypeRef) string {
	name := c.typeName(t)
	if name == "Any" {
		return "Any?"
	}
	return name + "?"
}

func (c *Compiler) zeroValue(t *parser.TypeRef) string {
	if t == nil {
		return "null"
	}
	tt := types.ResolveTypeRef(t, c.env)
	return kotlinZeroValue(tt)
}

func kotlinZeroValue(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "0"
	case types.FloatType:
		return "0.0"
	case types.StringType:
		return "\"\""
	case types.BoolType:
		return "false"
	case types.ListType:
		return fmt.Sprintf("mutableListOf<%s>()", kotlinTypeOf(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("mutableMapOf<%s, %s>()", kotlinTypeOf(tt.Key), kotlinTypeOf(tt.Value))
	case types.StructType:
		fields := make([]string, len(tt.Order))
		for i, f := range tt.Order {
			fields[i] = fmt.Sprintf("%s = %s", escapeIdent(f), kotlinZeroValue(tt.Fields[f]))
		}
		return fmt.Sprintf("%s(%s)", tt.Name, strings.Join(fields, ", "))
	default:
		return "null"
	}
}

func (c *Compiler) expr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.binary(e.Binary)
}

func (c *Compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	lType := types.TypeOfUnary(b.Left, c.env)

	operands := []string{left}
	typesList := []types.Type{lType}
	ops := []string{}

	for _, part := range b.Right {
		r, err := c.postfix(part.Right)
		if err != nil {
			return "", err
		}
		rType := types.TypeOfPostfix(part.Right, c.env)
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
		operands = append(operands, r)
		typesList = append(typesList, rType)
	}

	precLevels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	for _, level := range precLevels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				expr, typ, err := c.binaryOp(operands[i], typesList[i], ops[i], operands[i+1], typesList[i+1])
				if err != nil {
					return "", err
				}
				operands[i] = expr
				typesList[i] = typ
				operands = append(operands[:i+1], operands[i+2:]...)
				typesList = append(typesList[:i+1], typesList[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary compilation")
	}
	return operands[0], nil
}

func (c *Compiler) binaryOp(left string, lType types.Type, op string, right string, rType types.Type) (string, types.Type, error) {
	switch op {
	case "union_all":
		return fmt.Sprintf("%s.toMutableList().apply { addAll(%s) }", left, right), types.ListType{}, nil
	case "union":
		c.use("union")
		return fmt.Sprintf("union(%s.toMutableList(), %s.toMutableList())", left, right), types.ListType{}, nil
	case "except":
		c.use("except")
		return fmt.Sprintf("except(%s.toMutableList(), %s.toMutableList())", left, right), types.ListType{}, nil
	case "intersect":
		c.use("intersect")
		return fmt.Sprintf("intersect(%s.toMutableList(), %s.toMutableList())", left, right), types.ListType{}, nil
	case "+", "-", "*", "/", "%":
		if _, ok := lType.(types.IntType); ok {
			if _, rok := rType.(types.FloatType); rok {
				left = fmt.Sprintf("(%s).toDouble()", left)
				lType = types.FloatType{}
			}
		} else if _, ok := rType.(types.IntType); ok {
			if _, lok := lType.(types.FloatType); lok {
				right = fmt.Sprintf("(%s).toDouble()", right)
				rType = types.FloatType{}
			}
		}
		if _, lok := lType.(types.AnyType); lok {
			if _, rok := rType.(types.AnyType); rok {
				c.use("toDouble")
				left = fmt.Sprintf("toDouble(%s)", left)
				right = fmt.Sprintf("toDouble(%s)", right)
			} else {
				switch rType.(type) {
				case types.IntType:
					c.use("toInt")
					left = fmt.Sprintf("toInt(%s)", left)
				case types.FloatType:
					c.use("toDouble")
					left = fmt.Sprintf("toDouble(%s)", left)
				}
			}
		} else if _, rok := rType.(types.AnyType); rok {
			switch lType.(type) {
			case types.IntType:
				c.use("toInt")
				right = fmt.Sprintf("toInt(%s)", right)
			case types.FloatType:
				c.use("toDouble")
				right = fmt.Sprintf("toDouble(%s)", right)
			default:
				c.use("toDouble")
				right = fmt.Sprintf("toDouble(%s)", right)
			}
		}
		if op == "/" {
			if _, ok := lType.(types.IntType); ok {
				left = fmt.Sprintf("(%s).toDouble()", left)
				lType = types.FloatType{}
			}
			if _, ok := rType.(types.IntType); ok {
				right = fmt.Sprintf("(%s).toDouble()", right)
				rType = types.FloatType{}
			}
		}
		if op == "/" && (isAnyType(lType) || isAnyType(rType)) {
			c.use("div")
			return fmt.Sprintf("div(%s, %s)", left, right), types.FloatType{}, nil
		}
		t := rType
		if isComparisonOp(op) {
			t = types.BoolType{}
		}
		return fmt.Sprintf("%s %s %s", left, op, right), t, nil
	case "==", "!=", "<", "<=", ">", ">=":
		if isAnyType(lType) {
			c.use("toDouble")
			left = fmt.Sprintf("toDouble(%s)", left)
		}
		if isAnyType(rType) {
			c.use("toDouble")
			right = fmt.Sprintf("toDouble(%s)", right)
		}
		return fmt.Sprintf("%s %s %s", left, op, right), types.BoolType{}, nil
	case "in":
		return fmt.Sprintf("%s in %s", left, right), types.BoolType{}, nil
	case "&&", "||":
		return fmt.Sprintf("%s %s %s", left, op, right), types.BoolType{}, nil
	}
	return "", types.AnyType{}, fmt.Errorf("unsupported operator %s", op)
}

func (c *Compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	t := c.inferPostfixType(u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "!":
			if _, ok := t.(types.BoolType); !ok {
				c.use("toBool")
				val = "!toBool(" + val + ")"
			} else {
				val = "!" + val
			}
			t = types.BoolType{}
		default:
			val = op + val
		}
	}
	return val, nil
}

func (c *Compiler) postfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	lastField := ""
	for i, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if lastField == "starts_with" {
				c.use("starts_with")
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			lastField = ""
		case op.Index != nil:
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.expr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := ""
				if op.Index.End != nil {
					e, err := c.expr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				t := types.TypeOfPrimaryBasic(p.Target, c.env)
				if types.IsStringType(t) {
					if end == "" {
						end = fmt.Sprintf("%s.length", val)
					}
					val = fmt.Sprintf("%s.substring(%s, %s)", val, start, end)
				} else if types.IsListType(t) {
					if end == "" {
						end = fmt.Sprintf("%s.size", val)
					}
					val = fmt.Sprintf("%s.subList(%s, %s)", val, start, end)
				} else {
					return "", fmt.Errorf("unsupported slice type")
				}
			} else {
				idx, err := c.expr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if i < len(p.Ops)-1 {
					prefix := &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:i]}
					ct := c.inferPostfixType(prefix)
					if mt, ok := ct.(types.MapType); ok {
						val = fmt.Sprintf("(%s[%s] as %s)", val, idx, kotlinTypeOf(mt.Value))
					} else if types.IsMapType(ct) {
						val = fmt.Sprintf("(%s[%s] as MutableMap<*, *>)", val, idx)
					} else if lt, ok := ct.(types.ListType); ok {
						val = fmt.Sprintf("(%s[%s] as %s)", val, idx, kotlinTypeOf(lt.Elem))
					} else if types.IsListType(ct) {
						val = fmt.Sprintf("(%s[%s] as MutableList<Any?>)", val, idx)
					} else {
						val = fmt.Sprintf("%s[%s]!!", val, idx)
					}
				} else {
					val = fmt.Sprintf("%s[%s]", val, idx)
				}
			}
		case op.Field != nil:
			val = fmt.Sprintf("%s.%s", val, escapeIdent(op.Field.Name))
			lastField = op.Field.Name
		case op.Cast != nil:
			// special case: casting a map literal to a struct type
			if op.Cast.Type.Simple != nil {
				if _, ok := c.env.GetStruct(*op.Cast.Type.Simple); ok && p.Target.Map != nil && len(p.Ops) == 1 {
					fields := make([]string, len(p.Target.Map.Items))
					for i, it := range p.Target.Map.Items {
						k, err := c.expr(it.Key)
						if err != nil {
							return "", err
						}
						v, err := c.expr(it.Value)
						if err != nil {
							return "", err
						}
						fields[i] = fmt.Sprintf("%s = %s", strings.Trim(k, "\""), v)
					}
					val = fmt.Sprintf("%s(%s)", *op.Cast.Type.Simple, strings.Join(fields, ", "))
					break
				}
			}
			typ := c.typeName(op.Cast.Type)
			if typ == "Int" {
				val = fmt.Sprintf("(%s).toInt()", val)
			} else if typ == "String" {
				val = fmt.Sprintf("%s.toString()", val)
			} else {
				val = fmt.Sprintf("%s as %s", val, typ)
			}
		default:
			return "", fmt.Errorf("unsupported postfix")
		}
	}
	return val, nil
}

func (c *Compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.literal(p.Lit), nil
	case p.Selector != nil:
		name := escapeIdent(p.Selector.Root)
		t, _ := c.env.GetVar(p.Selector.Root)
		for i, part := range p.Selector.Tail {
			if i == 0 {
				if gt, ok := t.(types.GroupType); ok {
					if part == "key" {
						name += ".key"
						t = gt.Key
						continue
					}
					if part == "items" {
						name += ".items"
						t = types.ListType{Elem: gt.Elem}
						continue
					}
				}
			}
			if isStructType(t) {
				name += "." + escapeIdent(part)
				if st, ok := t.(types.StructType); ok {
					t = st.Fields[part]
				} else {
					t = types.AnyType{}
				}
				continue
			}
			if types.IsStringType(t) {
				name += "." + escapeIdent(part)
				t = types.AnyType{}
				continue
			}
			if mt, ok := t.(types.MapType); ok {
				name = fmt.Sprintf("(%s as MutableMap<%s, %s>)[%q]", name, kotlinTypeOf(mt.Key), kotlinTypeOf(mt.Value), part)
				t = mt.Value
			} else {
				name = fmt.Sprintf("(%s as MutableMap<*, *>)[%q]", name, part)
				t = types.AnyType{}
			}
		}
		return name, nil
	case p.Call != nil:
		return c.callExpr(p.Call)
	case p.Group != nil:
		s, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + s + ")", nil
	case p.List != nil:
		if len(p.List.Elems) == 0 {
			return "mutableListOf<Any?>()", nil
		}
		parts := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.expr(e)
			if err != nil {
				return "", err
			}
			parts[i] = s
		}
		return "mutableListOf(" + strings.Join(parts, ", ") + ")", nil
	case p.Map != nil:
		if name, ok := c.mapNodes[p.Map]; ok {
			fields := make([]string, len(p.Map.Items))
			for i, it := range p.Map.Items {
				v, err := c.expr(it.Value)
				if err != nil {
					return "", err
				}
				k, _ := identName(it.Key)
				fields[i] = fmt.Sprintf("%s = %s", escapeIdent(k), v)
			}
			return fmt.Sprintf("%s(%s)", name, strings.Join(fields, ", ")), nil
		}
		if name, ok := c.structForMap(p.Map); ok {
			fields := make([]string, len(p.Map.Items))
			for i, it := range p.Map.Items {
				v, err := c.expr(it.Value)
				if err != nil {
					return "", err
				}
				k, _ := identName(it.Key)
				fields[i] = fmt.Sprintf("%s = %s", escapeIdent(k), v)
			}
			return fmt.Sprintf("%s(%s)", name, strings.Join(fields, ", ")), nil
		}
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k string
			if name, ok := identName(it.Key); ok {
				k = fmt.Sprintf("%q", name)
			} else if it.Key.Binary.Left.Value.Target.Lit != nil && it.Key.Binary.Left.Value.Target.Lit.Str != nil {
				k = fmt.Sprintf("%q", *it.Key.Binary.Left.Value.Target.Lit.Str)
			} else {
				var err error
				k, err = c.expr(it.Key)
				if err != nil {
					return "", err
				}
			}
			v, err := c.expr(it.Value)
			if err != nil {
				return "", err
			}
			if !simpleExpr(v) {
				v = "(" + v + ")"
			}
			items[i] = fmt.Sprintf("%s to %s", k, v)
		}
		return "mutableMapOf(" + strings.Join(items, ", ") + ")", nil
	case p.Struct != nil:
		fields := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.expr(f.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s = %s", escapeIdent(f.Name), v)
		}
		return fmt.Sprintf("%s(%s)", p.Struct.Name, strings.Join(fields, ", ")), nil
	case p.FunExpr != nil:
		return c.funExpr(p.FunExpr)
	case p.Query != nil:
		return c.queryExpr(p.Query)
	case p.If != nil:
		return c.ifExpr(p.If)
	case p.Match != nil:
		return c.matchExpr(p.Match)
	case p.Load != nil:
		return c.loadExpr(p.Load)
	case p.Save != nil:
		return c.saveExpr(p.Save)
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) callExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	var paramTypes []types.Type
	if t, err := c.env.GetVar(call.Func); err == nil {
		if ft, ok := t.(types.FuncType); ok {
			paramTypes = ft.Params
		}
	}
	for i, a := range call.Args {
		if len(paramTypes) > i {
			v, err := c.expr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
			continue
		}
		s, err := c.expr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	if code, ok := c.builtinCall(call, args); ok {
		return code, nil
	}
	if _, ok := runtimePieces[call.Func]; ok {
		c.use(call.Func)
	}
	if call.Func == "print" {
		if len(args) == 1 {
			return fmt.Sprintf("println(%s)", args[0]), nil
		}
		return fmt.Sprintf("println(listOf(%s).joinToString(\" \"))", strings.Join(args, ", ")), nil
	}
	if call.Func == "json" {
		c.use("json")
		if len(args) == 1 {
			return fmt.Sprintf("json(%s)", args[0]), nil
		}
		return fmt.Sprintf("json(listOf(%s))", strings.Join(args, ", ")), nil
	}
	if len(paramTypes) > 0 && len(args) < len(paramTypes) {
		missing := paramTypes[len(args):]
		names := make([]string, len(missing))
		params := make([]string, len(missing))
		for i, pt := range missing {
			names[i] = fmt.Sprintf("p%d", i)
			params[i] = fmt.Sprintf("%s: %s", names[i], kotlinTypeOf(pt))
		}
		allArgs := append(append([]string{}, args...), names...)
		body := fmt.Sprintf("%s(%s)", call.Func, strings.Join(allArgs, ", "))
		return fmt.Sprintf("{ %s -> %s }", strings.Join(params, ", "), body), nil
	}
	return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
}

// builtinCall rewrites calls to certain helper functions using idiomatic Kotlin.
func (c *Compiler) builtinCall(call *parser.CallExpr, args []string) (string, bool) {
	switch call.Func {
	case "count":
		if len(args) == 1 {
			return fmt.Sprintf("%s.size", args[0]), true
		}
	case "exists":
		if len(args) == 1 {
			return fmt.Sprintf("%s.isNotEmpty()", args[0]), true
		}
	case "len":
		if len(args) == 1 {
			t := types.TypeOfExprBasic(call.Args[0], c.env)
			if types.IsStringType(t) {
				return fmt.Sprintf("%s.length", args[0]), true
			}
			return fmt.Sprintf("%s.size", args[0]), true
		}
	case "avg":
		if len(args) == 1 {
			c.use("toDouble")
			return fmt.Sprintf("run { val r = %s.map{ toDouble(it) }.average(); if (r %s 1.0 == 0.0) r.toInt() else r }", args[0], "%"), true
		}
	case "sum":
		if len(args) == 1 {
			c.use("sum")
			c.use("toDouble")
			return fmt.Sprintf("sum(%s)", args[0]), true
		}
	case "max":
		if len(args) == 1 {
			c.use("max")
			return fmt.Sprintf("max(%s)", args[0]), true
		}
	case "min":
		if len(args) == 1 {
			c.use("min")
			return fmt.Sprintf("min(%s)", args[0]), true
		}
	case "values":
		if len(args) == 1 {
			return fmt.Sprintf("%s.values.toMutableList()", args[0]), true
		}
	case "str":
		if len(args) == 1 {
			return fmt.Sprintf("%s.toString()", args[0]), true
		}
	case "substring":
		if len(args) == 3 {
			return fmt.Sprintf("%s.substring(%s, %s)", args[0], args[1], args[2]), true
		}
	case "starts_with":
		if len(args) == 2 {
			c.use("starts_with")
			return fmt.Sprintf("starts_with(%s, %s)", args[0], args[1]), true
		}
	case "int":
		if len(args) == 1 {
			c.use("toInt")
			return fmt.Sprintf("toInt(%s)", args[0]), true
		}
	case "input":
		if len(args) == 0 {
			c.use("_input")
			return "_input()", true
		}
	case "now":
		if len(args) == 0 {
			return "System.nanoTime().toInt()", true
		}
	case "append":
		if len(args) == 2 {
			c.use("append")
			return fmt.Sprintf("append(%s, %s)", args[0], args[1]), true
		}
	}
	return "", false
}

func (c *Compiler) literal(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return s
	case l.Bool != nil:
		return fmt.Sprintf("%t", bool(*l.Bool))
	case l.Null:
		return "null"
	}
	return "null"
}

func (c *Compiler) funExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeName(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	if fn.ExprBody != nil {
		body, err := c.expr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("{ %s -> %s }", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) ifExpr(ix *parser.IfExpr) (string, error) {
	cond, err := c.expr(ix.Cond)
	if err != nil {
		return "", err
	}
	if _, ok := c.inferExprType(ix.Cond).(types.BoolType); !ok {
		c.use("toBool")
		cond = "toBool(" + cond + ")"
	}
	thenExpr, err := c.expr(ix.Then)
	if err != nil {
		return "", err
	}
	if ix.ElseIf != nil {
		elseExpr, err := c.ifExpr(ix.ElseIf)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("if (%s) %s else %s", cond, thenExpr, elseExpr), nil
	}
	elseCode := "Unit"
	if ix.Else != nil {
		elseCode, err = c.expr(ix.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("if (%s) %s else %s", cond, thenExpr, elseCode), nil
}

func (c *Compiler) matchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.expr(m.Target)
	if err != nil {
		return "", err
	}
	var ut types.UnionType
	if t, ok := c.inferExprType(m.Target).(types.UnionType); ok {
		ut = t
	}
	var b strings.Builder
	b.WriteString("run {\n")
	b.WriteString("    val __t = " + target + "\n")
	b.WriteString("    when (__t) {\n")
	hasElse := false
	covered := make(map[string]bool)
	for _, cse := range m.Cases {
		// prepare environment for pattern variables if needed
		oldEnv := c.env
		c.env = types.NewEnv(c.env)

		var patCode string
		if name, ok := identName(cse.Pattern); ok && name == "_" {
			res, err := c.expr(cse.Result)
			if err != nil {
				c.env = oldEnv
				return "", err
			}
			b.WriteString("        else -> " + res + "\n")
			hasElse = true
			c.env = oldEnv
			continue
		} else if call := cse.Pattern.Binary.Left.Value.Target.Call; call != nil {
			if utv, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := utv.Variants[call.Func]
				patCode = "is " + call.Func
				b.WriteString("        " + patCode + " -> {")
				b.WriteString("\n")
				for i, arg := range call.Args {
					if argName, ok := identName(arg); ok {
						if i < len(st.Order) {
							field := escapeIdent(st.Order[i])
							c.env.SetVar(argName, st.Fields[st.Order[i]], true)
							b.WriteString("            val " + argName + " = __t." + field + "\n")
						} else {
							c.env.SetVar(argName, types.AnyType{}, true)
						}
					}
				}
				res, err := c.expr(cse.Result)
				if err != nil {
					c.env = oldEnv
					return "", err
				}
				b.WriteString("            " + res + "\n")
				b.WriteString("        }\n")
				if ut.Name != "" && ut.Name == utv.Name {
					covered[call.Func] = true
				}
				c.env = oldEnv
				continue
			}
		}

		// fallback: simple expression pattern
		pat, err := c.expr(cse.Pattern)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		patCode = pat
		if ut.Name != "" {
			if name, ok := identName(cse.Pattern); ok {
				if _, ok := ut.Variants[name]; ok {
					covered[name] = true
				}
			}
		}
		res, err := c.expr(cse.Result)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		b.WriteString("        " + patCode + " -> " + res + "\n")
		c.env = oldEnv
	}
	if !hasElse {
		if ut.Name == "" || len(covered) != len(ut.Variants) {
			b.WriteString("        else -> null\n")
		}
	}
	b.WriteString("    }\n")
	b.WriteString("}")
	return b.String(), nil
}

func (c *Compiler) queryExpr(q *parser.QueryExpr) (string, error) {
	var b strings.Builder
	indent := func(n int) string { return strings.Repeat("    ", n) }
	lvl := 1

	src, err := c.expr(q.Source)
	if err != nil {
		return "", err
	}

	child := types.NewEnv(c.env)
	if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, true)
	} else if gt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.GroupType); ok {
		child.SetVar(q.Var, gt.Elem, true)
	} else {
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	for _, f := range q.Froms {
		if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else if gt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.GroupType); ok {
			child.SetVar(f.Var, gt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	for _, j := range q.Joins {
		if lt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.ListType); ok {
			child.SetVar(j.Var, lt.Elem, true)
		} else if gt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.GroupType); ok {
			child.SetVar(j.Var, gt.Elem, true)
		} else {
			child.SetVar(j.Var, types.AnyType{}, true)
		}
	}
	var elem types.Type
	if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
		elem = lt.Elem
	} else if gt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.GroupType); ok {
		elem = gt.Elem
	}
	if len(q.Froms) > 0 || len(q.Joins) > 0 {
		if st, ok := c.queryRowStruct(q); ok {
			elem = st
			if _, exists := c.inferred[st.Name]; !exists {
				c.inferred[st.Name] = st
				c.env.SetStruct(st.Name, st)
			}
		} else {
			elem = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
		}
	}
	var keyType types.Type
	selEnv := child
	if q.Group != nil {
		// infer group key type using the child environment so join and
		// from variables are visible
		keyType = types.ExprType(q.Group.Exprs[0], child)
		if st, ok := keyType.(types.StructType); ok && st.Name == "" {
			keyType = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
		}
		if keyType == (types.AnyType{}) {
			if _, ok := mapLiteral(q.Group.Exprs[0]); ok {
				keyType = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
			}
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: elem}, true)
		selEnv = genv
	}
	selType := types.ExprType(q.Select, selEnv)
	if t := selectorType(q.Select, selEnv); t != nil {
		selType = t
	}

	if ml, ok := mapLiteral(q.Select); ok && q.Group == nil {
		if name, ok := c.mapNodes[ml]; ok {
			if st, ok := c.env.GetStruct(name); ok {
				selType = st
			}
		} else {
			if st, ok := c.structFromMapLiteral(ml, child); ok {
				c.inferred[st.Name] = st
				c.env.SetStruct(st.Name, st)
				c.mapNodes[ml] = st.Name
				selType = st
			}
		}
	}

	oldEnv := c.env
	// special case: simple join variants without extra clauses
	if q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct &&
		len(q.Froms) == 0 && len(q.Joins) == 1 && q.Where == nil {
		j := q.Joins[0]
		if j.Side != nil && (*j.Side == "left" || *j.Side == "outer") {
			// simple left or outer join
			c.env = child
			code, err := c.simpleRightOuterJoin(src, q, j, selType, *j.Side == "outer")
			c.env = oldEnv
			return code, err
		} else if j.Side != nil && *j.Side == "right" {
			c.env = child
			code, err := c.simpleLeftOuterJoin(src, q, j, selType, false)
			c.env = oldEnv
			return code, err
		}
	}
	c.env = child

	b.WriteString("run {\n")
	b.WriteString(indent(lvl))
	if q.Group != nil {
		c.use("Group")
		b.WriteString(fmt.Sprintf("val __groups = mutableMapOf<%s, Group<%s, %s>>()\n", kotlinTypeOf(keyType), kotlinTypeOf(keyType), kotlinTypeOf(elem)))
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("val __order = mutableListOf<%s>()\n", kotlinTypeOf(keyType)))
	} else {
		b.WriteString(fmt.Sprintf("val __res = mutableListOf<%s>()\n", kotlinTypeOf(selType)))
	}
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in %s) {\n", q.Var, src))
	lvl++
	for _, f := range q.Froms {
		s, err := c.expr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("for (%s in %s) {\n", f.Var, s))
		lvl++
	}
	for _, j := range q.Joins {
		js, err := c.expr(j.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("for (%s in %s) {\n", j.Var, js))
		lvl++
		cond, err := c.expr(j.On)
		if err != nil {
			return "", err
		}
		if _, ok := c.inferExprType(j.On).(types.BoolType); !ok {
			c.use("toBool")
			cond = "toBool(" + cond + ")"
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("if (%s) {\n", cond))
		lvl++
	}
	if q.Where != nil {
		cond, err := c.expr(q.Where)
		if err != nil {
			return "", err
		}
		if _, ok := c.inferExprType(q.Where).(types.BoolType); !ok {
			c.use("toBool")
			cond = "toBool(" + cond + ")"
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("if (%s) {\n", cond))
		lvl++
	}
	selEnv = c.env
	if q.Group != nil {
		genv := types.NewEnv(c.env)
		genv.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: elem}, true)
		selEnv = genv
		c.env = selEnv
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		if q.Group != nil {
			c.env = child
		}
		return "", err
	}
	var having string
	if q.Group != nil && q.Group.Having != nil {
		h, err := c.expr(q.Group.Having)
		if err != nil {
			c.env = child
			return "", err
		}
		if _, ok := c.inferExprType(q.Group.Having).(types.BoolType); !ok {
			c.use("toBool")
			h = "toBool(" + h + ")"
		}
		having = h
	}
	if q.Group != nil {
		c.env = child
	}
	if q.Group != nil {
		keyExpr, err := c.expr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		if mt, ok := keyType.(types.MapType); ok {
			keyExpr = fmt.Sprintf("(%s as MutableMap<%s, %s>)", keyExpr, kotlinTypeOf(mt.Key), kotlinTypeOf(mt.Value))
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("val __k = %s\n", keyExpr))
		b.WriteString(indent(lvl))
		b.WriteString("var __g = __groups[__k]\n")
		b.WriteString(indent(lvl))
		b.WriteString("if (__g == null) {\n")
		lvl++
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__g = Group(__k, mutableListOf<%s>())\n", kotlinTypeOf(elem)))
		b.WriteString(indent(lvl))
		b.WriteString("__groups[__k] = __g\n")
		b.WriteString(indent(lvl))
		b.WriteString("__order.add(__k)\n")
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
		b.WriteString(indent(lvl))
		var row string
		if st, ok := elem.(types.StructType); ok {
			fields := []string{fmt.Sprintf("%s = %s", escapeIdent(q.Var), q.Var)}
			for _, f := range q.Froms {
				fields = append(fields, fmt.Sprintf("%s = %s", escapeIdent(f.Var), f.Var))
			}
			for _, j := range q.Joins {
				fields = append(fields, fmt.Sprintf("%s = %s", escapeIdent(j.Var), j.Var))
			}
			if len(q.Froms) == 0 && len(q.Joins) == 0 && len(fields) == 1 {
				row = q.Var
			} else {
				row = fmt.Sprintf("%s(%s)", st.Name, strings.Join(fields, ", "))
			}
		} else {
			rowParts := []string{fmt.Sprintf("\"%s\" to %s", q.Var, q.Var)}
			for _, f := range q.Froms {
				rowParts = append(rowParts, fmt.Sprintf("\"%s\" to %s", f.Var, f.Var))
			}
			for _, j := range q.Joins {
				rowParts = append(rowParts, fmt.Sprintf("\"%s\" to %s", j.Var, j.Var))
			}
			rowType := kotlinTypeOf(elem)
			if len(rowParts) == 1 {
				row = q.Var
			} else {
				row = fmt.Sprintf("mutableMapOf(%s) as %s", strings.Join(rowParts, ", "), rowType)
			}
		}
		b.WriteString(fmt.Sprintf("__g.add(%s)\n", row))
	} else {
		selAdd := sel
		if _, ok := selType.(types.MapType); ok {
			selAdd = fmt.Sprintf("(%s as %s)", sel, kotlinTypeOf(selType))
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__res.add(%s)\n", selAdd))
	}
	if q.Where != nil {
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
	}
	for range q.Joins {
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
	}
	for range q.Froms {
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
	}
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	if q.Group != nil {
		if q.Sort != nil {
			sortEnv := types.NewEnv(c.env)
			sortEnv.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: elem}, true)
			oldSort := c.env
			c.env = sortEnv
			sortExpr, err := c.expr(q.Sort)
			c.env = oldSort
			if err != nil {
				return "", err
			}
			cast := ""
			switch types.TypeOfExprBasic(q.Sort, sortEnv).(type) {
			case types.IntType:
				cast = " as Int"
			case types.FloatType:
				cast = " as Double"
			case types.StringType:
				cast = " as String"
			default:
				cast = " as Comparable<Any>"
			}
			if strings.HasPrefix(sortExpr, "-") {
				sortExpr = strings.TrimPrefix(sortExpr, "-")
				b.WriteString(indent(lvl))
				b.WriteString("__order.sortByDescending { k ->\n")
			} else {
				b.WriteString(indent(lvl))
				b.WriteString("__order.sortBy { k ->\n")
			}
			lvl++
			b.WriteString(indent(lvl))
			b.WriteString(fmt.Sprintf("val %s = __groups[k]!!\n", q.Group.Name))
			b.WriteString(indent(lvl))
			b.WriteString(sortExpr + cast + "\n")
			lvl--
			b.WriteString(indent(lvl))
			b.WriteString("}\n")
		}

		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("val __res = mutableListOf<%s>()\n", kotlinTypeOf(selType)))
		b.WriteString(indent(lvl))
		b.WriteString("for (k in __order) {\n")
		lvl++
		b.WriteString(indent(lvl))
		b.WriteString("val g = __groups[k]!!\n")
		if q.Group.Name != "g" {
			b.WriteString(indent(lvl))
			b.WriteString(fmt.Sprintf("val %s = g\n", q.Group.Name))
		}
		if having != "" {
			b.WriteString(indent(lvl))
			b.WriteString(fmt.Sprintf("if (%s) {\n", having))
			lvl++
		}
		selAdd := sel
		if _, ok := selType.(types.MapType); ok {
			selAdd = fmt.Sprintf("(%s as %s)", sel, kotlinTypeOf(selType))
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__res.add(%s)\n", selAdd))
		if having != "" {
			lvl--
			b.WriteString(indent(lvl))
			b.WriteString("}\n")
		}
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
		b.WriteString(indent(lvl))
		b.WriteString("__res\n")
		b.WriteString("}")
	} else {
		b.WriteString(indent(lvl))
		b.WriteString("__res\n")
		b.WriteString("}")
	}

	res := b.String()
	if q.Sort != nil && q.Group == nil {
		sortEnv := c.env
		if q.Group != nil {
			sortEnv = types.NewEnv(c.env)
			sortEnv.SetVar(q.Group.Name, types.GroupType{Elem: elem}, true)
		}
		old := c.env
		c.env = sortEnv
		sortExpr, err := c.expr(q.Sort)
		c.env = old
		if err != nil {
			return "", err
		}
		vars := []string{q.Var}
		for _, f := range q.Froms {
			vars = append(vars, f.Var)
		}
		for _, j := range q.Joins {
			vars = append(vars, j.Var)
		}
		if q.Group != nil {
			vars = append(vars, q.Group.Name)
		}
		for _, v := range vars {
			sortExpr = replaceIdent(sortExpr, v, "it")
		}
		switch types.TypeOfExprBasic(q.Sort, sortEnv).(type) {
		case types.IntType:
			sortExpr += " as Int"
		case types.FloatType:
			sortExpr += " as Double"
		case types.StringType:
			sortExpr += " as String"
		default:
			sortExpr += " as Comparable<Any>"
		}
		if strings.HasPrefix(sortExpr, "-") {
			sortExpr = strings.TrimPrefix(sortExpr, "-")
			res += fmt.Sprintf(".sortedByDescending { %s }", sortExpr)
		} else {
			res += fmt.Sprintf(".sortedBy { %s }", sortExpr)
		}
	}
	if q.Skip != nil {
		skipEnv := c.env
		if q.Group != nil {
			skipEnv = types.NewEnv(c.env)
			skipEnv.SetVar(q.Group.Name, types.GroupType{Elem: elem}, true)
		}
		oldSkip := c.env
		c.env = skipEnv
		skip, err := c.expr(q.Skip)
		c.env = oldSkip
		if err != nil {
			return "", err
		}
		res += fmt.Sprintf(".drop(%s)", skip)
	}
	if q.Take != nil {
		takeEnv := c.env
		if q.Group != nil {
			takeEnv = types.NewEnv(c.env)
			takeEnv.SetVar(q.Group.Name, types.GroupType{Elem: elem}, true)
		}
		oldTake := c.env
		c.env = takeEnv
		take, err := c.expr(q.Take)
		c.env = oldTake
		if err != nil {
			return "", err
		}
		res += fmt.Sprintf(".take(%s)", take)
	}
	if q.Distinct {
		res += ".distinct()"
	}
	c.env = oldEnv

	var retType types.Type = types.ListType{Elem: selType}
	if name, arg, ok := aggregateCallName(q.Select); ok && q.Group == nil {
		switch name {
		case "sum", "avg":
			retType = types.FloatType{}
			c.use(name)
		case "count":
			retType = types.IntType{}
			c.use("count")
		case "min", "max":
			retType = types.ExprType(arg, child)
			c.use(name)
		}
		res = fmt.Sprintf("%s(%s)", name, res)
	}
	c.queryTypes[q] = retType
	return res, nil
}

func (c *Compiler) simpleRightOuterJoin(src string, q *parser.QueryExpr, j *parser.JoinClause, selType types.Type, outer bool) (string, error) {
	var b strings.Builder
	indent := func(n int) string { return strings.Repeat("    ", n) }
	lvl := 1

	js, err := c.expr(j.Src)
	if err != nil {
		return "", err
	}
	cond, err := c.expr(j.On)
	if err != nil {
		return "", err
	}
	if _, ok := c.inferExprType(j.On).(types.BoolType); !ok {
		c.use("toBool")
		cond = "toBool(" + cond + ")"
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		return "", err
	}

	b.WriteString("run {\n")
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("val __res = mutableListOf<%s>()\n", kotlinTypeOf(selType)))
	if outer {
		b.WriteString(indent(lvl))
		b.WriteString("val __matched = mutableSetOf<Any?>()\n")
	}
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in %s) {\n", q.Var, src))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString("val __tmp = mutableListOf<Any?>()\n")
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in %s) {\n", j.Var, js))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("if (%s) {\n", cond))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("__tmp.add(%s)\n", j.Var))
	if outer {
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__matched.add(%s)\n", j.Var))
	}
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	b.WriteString(indent(lvl))
	b.WriteString("if (__tmp.isEmpty()) __tmp.add(null)\n")
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in __tmp) {\n", j.Var))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("__res.add(%s)\n", sel))
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	if outer {
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("for (%s in %s) {\n", j.Var, js))
		lvl++
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("if (!__matched.contains(%s)) {\n", j.Var))
		lvl++
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("val %s = null\n", q.Var))
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__res.add(%s)\n", sel))
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
	}
	b.WriteString(indent(lvl))
	b.WriteString("__res\n")
	b.WriteString("}")
	c.queryTypes[q] = types.ListType{Elem: selType}
	return b.String(), nil
}

func (c *Compiler) simpleLeftOuterJoin(src string, q *parser.QueryExpr, j *parser.JoinClause, selType types.Type, outer bool) (string, error) {
	var b strings.Builder
	indent := func(n int) string { return strings.Repeat("    ", n) }
	lvl := 1

	js, err := c.expr(j.Src)
	if err != nil {
		return "", err
	}
	cond, err := c.expr(j.On)
	if err != nil {
		return "", err
	}
	if _, ok := c.inferExprType(j.On).(types.BoolType); !ok {
		c.use("toBool")
		cond = "toBool(" + cond + ")"
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		return "", err
	}

	b.WriteString("run {\n")
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("val __res = mutableListOf<%s>()\n", kotlinTypeOf(selType)))
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in %s) {\n", j.Var, js))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString("val __tmp = mutableListOf<Any?>()\n")
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in %s) {\n", q.Var, src))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("if (%s) {\n", cond))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("__tmp.add(%s)\n", q.Var))
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	b.WriteString(indent(lvl))
	b.WriteString("if (__tmp.isEmpty()) __tmp.add(null)\n")
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in __tmp) {\n", q.Var))
	lvl++
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("__res.add(%s)\n", sel))
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	b.WriteString(indent(lvl))
	b.WriteString("__res\n")
	b.WriteString("}")
	c.queryTypes[q] = types.ListType{Elem: selType}
	return b.String(), nil
}

func (c *Compiler) loadExpr(l *parser.LoadExpr) (string, error) {
	c.use("_load")
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "null"
	if l.With != nil {
		v, err := c.expr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	expr := fmt.Sprintf("_load(%s, %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		if st, ok := c.env.GetStruct(*l.Type.Simple); ok {
			fields := make([]string, len(st.Order))
			for i, f := range st.Order {
				v := fmt.Sprintf("it[%q]", f)
				if kt := kotlinCastType(st.Fields[f]); kt != "" {
					v += fmt.Sprintf(" as %s", kt)
				}
				fields[i] = fmt.Sprintf("%s = %s", f, v)
			}
			expr += fmt.Sprintf(".map { %s(%s) }.toMutableList()", st.Name, strings.Join(fields, ", "))
		}
	}
	return expr, nil
}

func (c *Compiler) saveExpr(s *parser.SaveExpr) (string, error) {
	c.use("_save")
	src, err := c.expr(s.Src)
	if err != nil {
		return "", err
	}
	path := "null"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "null"
	if s.With != nil {
		v, err := c.expr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
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

// selectorType returns the type of a simple selector expression if it can be
// derived from the environment. It returns nil if the expression is not a
// selector or the type cannot be determined.
func selectorType(e *parser.Expr, env *types.Env) types.Type {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Selector == nil {
		return nil
	}
	sel := p.Target.Selector
	t, err := env.GetVar(sel.Root)
	if err != nil {
		return nil
	}
	return fieldType(t, sel.Tail)
}

func structNameFromVar(name string) string {
	if strings.HasSuffix(name, "s") && len(name) > 1 {
		name = name[:len(name)-1]
	}
	if len(name) == 0 {
		return "Record"
	}
	return strings.ToUpper(name[:1]) + name[1:]
}

func mapLiteral(e *parser.Expr) (*parser.MapLiteral, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	pf := u.Value
	if len(pf.Ops) != 0 || pf.Target == nil {
		return nil, false
	}
	if pf.Target.Map != nil {
		return pf.Target.Map, true
	}
	return nil, false
}

func listLiteral(e *parser.Expr) (*parser.ListLiteral, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	pf := u.Value
	if len(pf.Ops) != 0 || pf.Target == nil {
		return nil, false
	}
	if pf.Target.List != nil {
		return pf.Target.List, true
	}
	return nil, false
}

// queryRowStruct attempts to infer a struct type for the implicit rows produced
// by a query with join or from clauses. It returns the struct type and true on
// success.
func (c *Compiler) queryRowStruct(q *parser.QueryExpr) (types.StructType, bool) {
	fields := map[string]types.Type{}
	order := []string{}

	addField := func(name string, src *parser.Expr) {
		t := c.inferExprType(src)
		if lt, ok := t.(types.ListType); ok {
			t = lt.Elem
		} else if gt, ok := t.(types.GroupType); ok {
			t = gt.Elem
		}
		fields[name] = t
		order = append(order, name)
	}

	if !identRE.MatchString(q.Var) {
		return types.StructType{}, false
	}
	addField(q.Var, q.Source)
	for _, f := range q.Froms {
		if !identRE.MatchString(f.Var) {
			return types.StructType{}, false
		}
		addField(f.Var, f.Src)
	}
	for _, j := range q.Joins {
		if !identRE.MatchString(j.Var) {
			return types.StructType{}, false
		}
		addField(j.Var, j.Src)
	}

	name := fmt.Sprintf("Row%d", c.structCount)
	c.structCount++
	st := types.StructType{Name: name, Fields: fields, Order: order}
	return st, true
}

// discoverStructs scans the program for list literals consisting of map
// literals with identical simple keys and promotes them to data classes.
func (c *Compiler) discoverStructs(prog *parser.Program) {
	for _, st := range prog.Statements {
		var list *parser.ListLiteral
		var q *parser.QueryExpr
		var name string
		var mutable bool
		switch {
		case st.Let != nil && st.Let.Value != nil:
			list, _ = listLiteral(st.Let.Value)
			if ql := st.Let.Value.Binary.Left.Value.Target.Query; ql != nil {
				q = ql
			}
			name = st.Let.Name
			mutable = false
		case st.Var != nil && st.Var.Value != nil:
			list, _ = listLiteral(st.Var.Value)
			if ql := st.Var.Value.Binary.Left.Value.Target.Query; ql != nil {
				q = ql
			}
			name = st.Var.Name
			mutable = true
		}
		if list != nil {
			if len(list.Elems) == 0 {
				continue
			}
			firstMap, ok := mapLiteral(list.Elems[0])
			if !ok {
				continue
			}
			keys := []string{}
			for _, it := range firstMap.Items {
				if k, ok := identName(it.Key); ok {
					keys = append(keys, k)
				} else {
					keys = nil
					break
				}
			}
			if keys == nil || len(keys) == 0 {
				continue
			}
			okAll := true
			for _, e := range list.Elems[1:] {
				m, ok := mapLiteral(e)
				if !ok || len(m.Items) != len(keys) {
					okAll = false
					break
				}
				for i, it := range m.Items {
					k, ok := identName(it.Key)
					if !ok || k != keys[i] {
						okAll = false
						break
					}
				}
				if !okAll {
					break
				}
			}
			if !okAll {
				continue
			}
			fields := map[string]types.Type{}
			for i, it := range firstMap.Items {
				fields[keys[i]] = c.inferExprType(it.Value)
			}
			structName := structNameFromVar(name)
			stype := types.StructType{Name: structName, Fields: fields, Order: keys}
			c.inferred[structName] = stype
			c.env.SetStruct(structName, stype)
			c.env.SetVar(name, types.ListType{Elem: stype}, mutable)
			for _, e := range list.Elems {
				if m, ok := mapLiteral(e); ok {
					c.mapNodes[m] = structName
				}
			}
		} else if q != nil {
			child := types.NewEnv(c.env)
			if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
				child.SetVar(q.Var, lt.Elem, true)
			} else if gt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.GroupType); ok {
				child.SetVar(q.Var, gt.Elem, true)
			} else {
				child.SetVar(q.Var, types.AnyType{}, true)
			}
			for _, f := range q.Froms {
				if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
					child.SetVar(f.Var, lt.Elem, true)
				} else if gt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.GroupType); ok {
					child.SetVar(f.Var, gt.Elem, true)
				} else {
					child.SetVar(f.Var, types.AnyType{}, true)
				}
			}
			for _, j := range q.Joins {
				if lt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.ListType); ok {
					child.SetVar(j.Var, lt.Elem, true)
				} else if gt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.GroupType); ok {
					child.SetVar(j.Var, gt.Elem, true)
				} else {
					child.SetVar(j.Var, types.AnyType{}, true)
				}
			}
			if ml, ok := mapLiteral(q.Select); ok && q.Group == nil {
				if st, ok := c.structFromMapLiteral(ml, child); ok {
					c.inferred[st.Name] = st
					c.env.SetStruct(st.Name, st)
					c.mapNodes[ml] = st.Name
					c.env.SetVar(name, types.ListType{Elem: st}, mutable)
				}
			} else if id, ok := identName(q.Select); ok && id == q.Var {
				if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
					c.env.SetVar(name, types.ListType{Elem: lt.Elem}, mutable)
				}
			} else {
				if t := selectorType(q.Select, child); t != nil {
					if st, ok := t.(types.StructType); ok {
						c.env.SetVar(name, types.ListType{Elem: st}, mutable)
					}
				}
			}
		}
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func replaceIdent(s, name, repl string) string {
	re := regexp.MustCompile(`\b` + regexp.QuoteMeta(name) + `\b`)
	return re.ReplaceAllString(s, repl)
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func isNumericOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%":
		return true
	default:
		return false
	}
}

func isComparisonOp(op string) bool {
	switch op {
	case "==", "!=", "<", "<=", ">", ">=":
		return true
	default:
		return false
	}
}

func (c *Compiler) use(name string) {
	if c.used == nil {
		c.used = make(map[string]bool)
	}
	if c.used[name] {
		return
	}
	c.used[name] = true
	switch name {
	case "_load":
		c.use("loadYamlSimple")
		c.use("parseSimpleValue")
	case "_save", "json":
		c.use("toJson")
	}
}

func fieldType(t types.Type, path []string) types.Type {
	for _, p := range path {
		switch tt := t.(type) {
		case types.StructType:
			if f, ok := tt.Fields[p]; ok {
				t = f
			} else {
				return types.AnyType{}
			}
		case types.MapType:
			t = tt.Value
		default:
			return types.AnyType{}
		}
	}
	return t
}

func (c *Compiler) structForMap(m *parser.MapLiteral) (string, bool) {
	keys := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, ok := identName(it.Key)
		if !ok {
			return "", false
		}
		keys[i] = k
	}
	for _, st := range c.inferred {
		if len(st.Order) != len(keys) {
			continue
		}
		match := true
		for i, k := range st.Order {
			if k != keys[i] {
				match = false
				break
			}
		}
		if match {
			return st.Name, true
		}
	}
	return "", false
}

func aggregateCallName(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	switch call.Func {
	case "sum", "avg", "min", "max", "count":
		return call.Func, call.Args[0], true
	}
	return "", nil, false
}

func (c *Compiler) structFromMapLiteral(m *parser.MapLiteral, env *types.Env) (types.StructType, bool) {
	keys := make([]string, len(m.Items))
	fields := make(map[string]types.Type)
	for i, it := range m.Items {
		k, ok := identName(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		keys[i] = k
		fields[k] = types.ExprType(it.Value, env)
	}
	name := fmt.Sprintf("Row%d", c.structCount)
	c.structCount++
	st := types.StructType{Name: name, Fields: fields, Order: keys}
	return st, true
}

func kotlinCastType(t types.Type) string {
	switch t.(type) {
	case types.IntType:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	default:
		return ""
	}
}

func kotlinElemType(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.FloatType:
		return "Number"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	default:
		return "Any"
	}
}

func kotlinTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	case types.ListType:
		return fmt.Sprintf("MutableList<%s>", kotlinTypeOf(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("MutableMap<%s, %s>", kotlinTypeOf(tt.Key), kotlinTypeOf(tt.Value))
	case types.StructType:
		if tt.Name == "" {
			return "Any?"
		}
		return tt.Name
	case types.GroupType:
		return fmt.Sprintf("Group<%s, %s>", kotlinTypeOf(tt.Key), kotlinTypeOf(tt.Elem))
	default:
		return "Any?"
	}
}

var kotlinKeywords = map[string]bool{
	"class":  true,
	"object": true,
	"val":    true,
	"var":    true,
	"when":   true,
	"this":   true,
}

var identRE = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)

func escapeIdent(name string) string {
	if kotlinKeywords[name] || !identRE.MatchString(name) {
		return "`" + name + "`"
	}
	return name
}

func escapeKeywords(src string) string {
	for kw := range kotlinKeywords {
		re := regexp.MustCompile(`\.` + regexp.QuoteMeta(kw) + `\b`)
		src = re.ReplaceAllString(src, ".`"+kw+"`")
	}
	return src
}

func isStructType(t types.Type) bool {
	switch t.(type) {
	case types.StructType, types.GroupType:
		return true
	default:
		return false
	}
}

func isStringType(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isAnyType(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

// isSelfCall reports whether e is a call expression invoking the function
// named name with no additional operations.
func isSelfCall(e *parser.Expr, name string) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return false
	}
	return p.Target.Call.Func == name
}

// isTailRecursive checks for the simple pattern where the last statement of
// the function body is `return f(...)` and marks the function eligible for
// Kotlin's `tailrec` modifier.
func isTailRecursive(f *parser.FunStmt) bool {
	if len(f.Body) == 0 {
		return false
	}
	last := f.Body[len(f.Body)-1]
	if last.Return == nil {
		return false
	}
	return isSelfCall(last.Return.Value, f.Name)
}

// builtinImport emits Kotlin equivalents for certain foreign imports.
func (c *Compiler) builtinImport(im *parser.ImportStmt) (bool, error) {
	if im.Lang == nil {
		return false, nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = escapeIdent(alias)
	switch *im.Lang {
	case "python":
		if im.Path == "math" {
			st := types.StructType{Name: strings.Title(alias), Fields: map[string]types.Type{
				"pi":   types.FloatType{},
				"e":    types.FloatType{},
				"sqrt": types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}},
				"pow":  types.FuncType{Params: []types.Type{types.FloatType{}, types.FloatType{}}, Return: types.FloatType{}},
				"sin":  types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}},
				"log":  types.FuncType{Params: []types.Type{types.FloatType{}}, Return: types.FloatType{}},
			}, Order: []string{"pi", "e", "sqrt", "pow", "sin", "log"}}
			c.env.SetStruct(st.Name, st)
			c.env.SetVar(alias, st, false)
			c.writeln(fmt.Sprintf("object %s {", alias))
			c.indent++
			c.writeln("const val pi: Double = kotlin.math.PI")
			c.writeln("const val e: Double = kotlin.math.E")
			c.writeln("fun sqrt(x: Double): Double = kotlin.math.sqrt(x)")
			// Kotlin 1.3 does not provide a top-level pow function.
			// Use java.lang.Math.pow for portability.
			c.writeln("fun pow(x: Double, y: Double): Double = Math.pow(x, y)")
			c.writeln("fun sin(x: Double): Double = kotlin.math.sin(x)")
			c.writeln("fun log(x: Double): Double = kotlin.math.ln(x)")
			c.indent--
			c.writeln("}")
			c.writeln("")
			return true, nil
		}
	case "go":
		if im.Auto && im.Path == "mochi/runtime/ffi/go/testpkg" {
			st := types.StructType{Name: strings.Title(alias), Fields: map[string]types.Type{
				"Add":    types.FuncType{Params: []types.Type{types.IntType{}, types.IntType{}}, Return: types.IntType{}},
				"Pi":     types.FloatType{},
				"Answer": types.IntType{},
			}, Order: []string{"Add", "Pi", "Answer"}}
			c.env.SetStruct(st.Name, st)
			c.env.SetVar(alias, st, false)
			c.writeln(fmt.Sprintf("object %s {", alias))
			c.indent++
			c.writeln("fun Add(a: Int, b: Int): Int = a + b")
			c.writeln("const val Pi: Double = 3.14")
			c.writeln("var Answer: Int = 42")
			c.indent--
			c.writeln("}")
			c.writeln("")
			return true, nil
		}
	}
	return false, nil
}

// inferListLike attempts to infer the element type of a query followed by
// simple list operations like sort, drop or take. It returns AnyType if it
// cannot determine a better type.
func (c *Compiler) inferListLike(e *parser.Expr) types.Type {
	if e == nil || e.Binary == nil {
		return types.AnyType{}
	}
	q := e.Binary.Left.Value.Target.Query
	if q == nil {
		return types.AnyType{}
	}
	srcType := types.TypeOfExprBasic(q.Source, c.env)
	var elem types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elem = lt.Elem
	}
	if sel := q.Select; sel != nil {
		if sel.Binary != nil && len(sel.Binary.Right) == 0 {
			if pf := sel.Binary.Left.Value; pf != nil && len(pf.Ops) == 0 {
				if pf.Target.Selector != nil && pf.Target.Selector.Root == q.Var && len(pf.Target.Selector.Tail) == 0 {
					return types.ListType{Elem: elem}
				}
			}
		}
	}
	return types.ListType{Elem: types.MapType{Key: types.StringType{}, Value: types.AnyType{}}}
}

// isEmptyListLiteral reports whether the expression is a direct list literal
// with no elements.
func isEmptyListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if ll := p.Target.List; ll != nil {
		return len(ll.Elems) == 0
	}
	return false
}

// isEmptyMapLiteral reports whether the expression is a direct map literal with
// no elements.
func isEmptyMapLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if ml := p.Target.Map; ml != nil {
		return len(ml.Items) == 0
	}
	return false
}

// simpleExpr reports whether the expression is a single identifier or literal
// value without operators.
func simpleExpr(s string) bool {
	if identRE.MatchString(s) {
		return true
	}
	if strings.HasPrefix(s, "\"") && strings.HasSuffix(s, "\"") {
		return true
	}
	if s == "true" || s == "false" || s == "null" {
		return true
	}
	if regexp.MustCompile(`^-?\d+(\.\d+)?$`).MatchString(s) {
		return true
	}
	return false
}
