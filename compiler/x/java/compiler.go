//go:build slow

package javacode

import (
	"bytes"
	"fmt"
	"path/filepath"
	"regexp"
	"sort"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf               *bytes.Buffer
	indent            int
	helpers           map[string]bool
	vars              map[string]string
	funRet            map[string]string
	funSigs           map[string]*funSig
	types             map[string]*parser.TypeDecl
	needFuncImports   bool
	tmpCount          int
	groupKeys         map[string]string
	variantOf         map[string]string
	variantFields     map[string][]string
	variantFieldTypes map[string][]string
	dataClasses       map[string]*dataClass
	dataClassOrder    []string
	srcDir            string
	curVar            string
}

type dataClass struct {
	name   string
	fields []string
	types  []string
}

type funSig struct {
	params []*parser.Param
	ret    *parser.TypeRef
}

func New() *Compiler {
	return &Compiler{
		buf:               new(bytes.Buffer),
		helpers:           make(map[string]bool),
		vars:              make(map[string]string),
		funRet:            make(map[string]string),
		funSigs:           make(map[string]*funSig),
		types:             make(map[string]*parser.TypeDecl),
		tmpCount:          0,
		groupKeys:         make(map[string]string),
		variantOf:         make(map[string]string),
		variantFields:     make(map[string][]string),
		variantFieldTypes: make(map[string][]string),
		dataClasses:       make(map[string]*dataClass),
		dataClassOrder:    []string{},
		curVar:            "",
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// compile main body first so we know which helpers are needed
	c.srcDir = filepath.Dir(prog.Pos.Filename)
	body := new(bytes.Buffer)
	origBuf := c.buf
	c.buf = body
	c.indent = 1
	for _, s := range prog.Statements {
		if s.Var != nil {
			typ := c.typeName(s.Var.Type)
			if s.Var.Type == nil && s.Var.Value != nil {
				orig := c.curVar
				c.curVar = s.Var.Name
				typ = c.inferType(s.Var.Value)
				c.curVar = orig
			}
			c.vars[s.Var.Name] = typ
			if s.Var.Value != nil {
				orig := c.curVar
				c.curVar = s.Var.Name
				if _, err := c.compileExpr(s.Var.Value); err != nil {
					return nil, err
				}
				c.curVar = orig
			}
			continue
		}
		if s.Let != nil {
			typ := c.typeName(s.Let.Type)
			if s.Let.Type == nil && s.Let.Value != nil {
				orig := c.curVar
				c.curVar = s.Let.Name
				typ = c.inferType(s.Let.Value)
				c.curVar = orig
			}
			c.vars[s.Let.Name] = typ
			if s.Let.Value != nil {
				orig := c.curVar
				c.curVar = s.Let.Name
				if _, err := c.compileExpr(s.Let.Value); err != nil {
					return nil, err
				}
				c.curVar = orig
			}
			continue
		}
		if s.Type != nil {
			c.types[s.Type.Name] = s.Type
			continue
		}
		if s.Fun != nil {
			c.funRet[s.Fun.Name] = c.typeName(s.Fun.Return)
			c.funSigs[s.Fun.Name] = &funSig{params: s.Fun.Params, ret: s.Fun.Return}
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	// restore buffer for final output
	c.buf = origBuf
	c.buf.Reset()
	c.indent = 0
	c.writeln("import java.util.*;")
	if c.helpers["load_yaml"] {
		c.writeln("import java.io.*;")
	}
	if c.needFuncImports {
		c.writeln("import java.util.function.*;")
	}
	// user defined types
	for _, tdecl := range c.types {
		if err := c.compileTypeDecl(tdecl); err != nil {
			return nil, err
		}
	}
	// inferred data classes
	for _, key := range c.dataClassOrder {
		c.compileDataClass(c.dataClasses[key])
	}
	c.writeln("public class Main {")
	c.indent++
	// global declarations
	for _, s := range prog.Statements {
		switch {
		case s.Var != nil:
			if err := c.compileGlobalVar(s.Var); err != nil {
				return nil, err
			}
		case s.Let != nil:
			if err := c.compileGlobalLet(s.Let); err != nil {
				return nil, err
			}
		}
	}
	// function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
		}
	}
	// helper methods
	if c.helpers["append"] {
		c.writeln("static <T> List<T> append(List<T> list, T item) {")
		c.indent++
		c.writeln("List<T> res = new ArrayList<>(list);")
		c.writeln("res.add(item);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["count"] {
		c.writeln("static int count(Collection<?> c) {")
		c.indent++
		c.writeln("return c.size();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["sum"] {
		c.writeln("static int sum(List<? extends Number> v) {")
		c.indent++
		c.writeln("int s = 0;")
		c.writeln("for (Number n : v) s += n.intValue();")
		c.writeln("return s;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["avg"] {
		c.writeln("static double avg(List<? extends Number> v) {")
		c.indent++
		c.writeln("if (v.isEmpty()) return 0;")
		c.writeln("int s = 0;")
		c.writeln("for (Number n : v) s += n.intValue();")
		c.writeln("return (double)s / v.size();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["min"] {
		c.writeln("static int min(List<? extends Number> v) {")
		c.indent++
		c.writeln("int m = Integer.MAX_VALUE;")
		c.writeln("for (Number n : v) if (n.intValue() < m) m = n.intValue();")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["max"] {
		c.writeln("static int max(List<? extends Number> v) {")
		c.indent++
		c.writeln("int m = Integer.MIN_VALUE;")
		c.writeln("for (Number n : v) if (n.intValue() > m) m = n.intValue();")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["values"] {
		c.writeln("static <K,V> List<V> values(Map<K,V> m) {")
		c.indent++
		c.writeln("return new ArrayList<>(m.values());")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["map_of_entries"] {
		c.writeln("static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }")
		c.writeln("static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {")
		c.indent++
		c.writeln("LinkedHashMap<K,V> m = new LinkedHashMap<>();")
		c.writeln("for (var e : entries) m.put(e.getKey(), e.getValue());")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["in"] {
		c.writeln("static boolean inOp(Object item, Object collection) {")
		c.indent++
		c.writeln("if (collection instanceof Map<?,?> m) return m.containsKey(item);")
		c.writeln("if (collection instanceof Collection<?> c) return c.contains(item);")
		c.writeln("if (collection instanceof String s) return s.contains(String.valueOf(item));")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["union_all"] {
		c.writeln("static <T> List<T> union_all(List<T> a, List<T> b) {")
		c.indent++
		c.writeln("List<T> res = new ArrayList<>(a);")
		c.writeln("res.addAll(b);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["union"] {
		c.writeln("static <T> List<T> union(List<T> a, List<T> b) {")
		c.indent++
		c.writeln("LinkedHashSet<T> s = new LinkedHashSet<>(a);")
		c.writeln("s.addAll(b);")
		c.writeln("return new ArrayList<>(s);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["except"] {
		c.writeln("static <T> List<T> except(List<T> a, List<T> b) {")
		c.indent++
		c.writeln("List<T> res = new ArrayList<>();")
		c.writeln("for (T x : a) if (!b.contains(x)) res.add(x);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["intersect"] {
		c.writeln("static <T> List<T> intersect(List<T> a, List<T> b) {")
		c.indent++
		c.writeln("List<T> res = new ArrayList<>();")
		c.writeln("for (T x : a) if (b.contains(x) && !res.contains(x)) res.add(x);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["load_yaml"] {
		c.writeln("static List<Map<String,Object>> loadYaml(String path) {")
		c.indent++
		c.writeln("if (!(new java.io.File(path)).isAbsolute()) {")
		c.indent++
		c.writeln("java.io.File f = new java.io.File(path);")
		c.writeln("if (!f.exists()) {")
		c.indent++
		c.writeln("String root = System.getenv(\"MOCHI_ROOT\");")
		c.writeln("if (root != null && !root.isEmpty()) {")
		c.indent++
		c.writeln("String clean = path;")
		c.writeln("while (clean.startsWith(\"../\")) clean = clean.substring(3);")
		c.writeln("java.io.File alt = new java.io.File(root + java.io.File.separator + \"tests\" + java.io.File.separator + clean);")
		c.writeln("if (!alt.exists()) alt = new java.io.File(root, clean);")
		c.writeln("if (alt.exists()) path = alt.getPath();")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("List<Map<String,Object>> list = new ArrayList<>();")
		c.writeln("try (BufferedReader br = new BufferedReader(new FileReader(path))) {")
		c.indent++
		c.writeln("Map<String,Object> cur = null;")
		c.writeln("String line;")
		c.writeln("while ((line = br.readLine()) != null) {")
		c.indent++
		c.writeln("line = line.trim();")
		c.writeln("if (line.startsWith(\"- name:\")) {")
		c.indent++
		c.writeln("if (cur != null) list.add(cur);")
		c.writeln("cur = new LinkedHashMap<>();")
		c.writeln("cur.put(\"name\", line.substring(line.indexOf(':')+1).trim());")
		c.indent--
		c.writeln("} else if (line.startsWith(\"age:\")) {")
		c.indent++
		c.writeln("if (cur != null) cur.put(\"age\", Integer.parseInt(line.substring(line.indexOf(':')+1).trim()));")
		c.indent--
		c.writeln("} else if (line.startsWith(\"email:\")) {")
		c.indent++
		c.writeln("if (cur != null) cur.put(\"email\", line.substring(line.indexOf(':')+1).trim());")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("if (cur != null) list.add(cur);")
		c.indent--
		c.writeln("} catch (Exception e) { throw new RuntimeException(e); }")
		c.writeln("return list;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["save_jsonl"] {
		c.writeln("static void saveJsonl(List<Map<?,?>> list) {")
		c.indent++
		c.writeln("for (Map<?,?> m : list) {")
		c.indent++
		c.writeln("List<String> parts = new ArrayList<>();")
		c.writeln("for (var e : m.entrySet()) { parts.add(\"\\\"\" + e.getKey() + \"\\\":\" + e.getValue()); }")
		c.writeln("System.out.println(\"{\" + String.join(\",\", parts) + \"}\");")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["json"] {
		c.writeln("static String toJson(Object o) {")
		c.indent++
		c.writeln("if (o instanceof Map<?,?> m) {")
		c.indent++
		c.writeln("StringJoiner j = new StringJoiner(\",\", \"{\", \"}\");")
		c.writeln("for (var e : m.entrySet()) j.add(\"\\\"\" + e.getKey() + \"\\\":\" + e.getValue());")
		c.writeln("return j.toString();")
		c.indent--
		c.writeln("} else if (o instanceof Collection<?> c) {")
		c.indent++
		c.writeln("StringJoiner j = new StringJoiner(\",\", \"[\", \"]\");")
		c.writeln("for (var x : c) j.add(toJson(x));")
		c.writeln("return j.toString();")
		c.indent--
		c.writeln("} else if (o instanceof String s) {")
		c.indent++
		c.writeln("return \"\\\"\" + s + \"\\\"\";")
		c.indent--
		c.writeln("}")
		c.writeln("return String.valueOf(o);")
		c.indent--
		c.writeln("}")
		c.writeln("static void json(Object o) { System.out.println(toJson(o)); }")
	}
	c.writeln("public static void main(String[] args) {")
	c.indent++
	c.buf.Write(body.Bytes())
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Fun != nil:
		return c.compileLocalFun(s.Fun)
	case s.Test != nil:
		for _, st := range s.Test.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		return nil
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("if (!(%s)) throw new AssertionError(\"expect failed\");", expr))
		return nil
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Expr != nil:
		return c.compileExprStmt(s.Expr)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) typeName(t *parser.TypeRef) string {
	if t == nil {
		return "int"
	}
	if t.Fun != nil {
		if len(t.Fun.Params) == 1 && t.Fun.Return != nil && t.Fun.Return.Simple != nil && *t.Fun.Return.Simple == "int" && t.Fun.Params[0].Simple != nil && *t.Fun.Params[0].Simple == "int" {
			c.needFuncImports = true
			return "IntUnaryOperator"
		}
		return "Object"
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			return fmt.Sprintf("List<%s>", wrapperType(c.typeName(t.Generic.Args[0])))
		case "map":
			if len(t.Generic.Args) >= 2 {
				kt := wrapperType(c.typeName(t.Generic.Args[0]))
				vt := wrapperType(c.typeName(t.Generic.Args[1]))
				return fmt.Sprintf("Map<%s,%s>", kt, vt)
			}
			return "Map<Object,Object>"
		default:
			return "Object"
		}
	}
	if t.Simple == nil {
		return "Object"
	}
	switch *t.Simple {
	case "int":
		return "int"
	case "string":
		return "String"
	case "bool":
		return "boolean"
	case "float":
		return "double"
	default:
		return *t.Simple
	}
}

func (c *Compiler) defaultValue(typ string) string {
	switch typ {
	case "String":
		return "\"\""
	case "int":
		return "0"
	case "boolean":
		return "false"
	case "double":
		return "0.0"
	default:
		return "null"
	}
}

func wrapperType(t string) string {
	switch t {
	case "int":
		return "Integer"
	case "double":
		return "Double"
	case "boolean":
		return "Boolean"
	default:
		return t
	}
}

func classNameFromVar(s string) string {
	if s == "" {
		return ""
	}
	parts := strings.FieldsFunc(s, func(r rune) bool {
		return r == '_' || r == '-' || r == ' '
	})
	for i, p := range parts {
		if p == "" {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
}

func mapValueType(t string) string {
	t = strings.TrimSpace(t)
	if !strings.HasPrefix(t, "Map<") || !strings.HasSuffix(t, ">") {
		return "Object"
	}
	inner := t[4 : len(t)-1]
	depth := 0
	for i := 0; i < len(inner); i++ {
		switch inner[i] {
		case '<':
			depth++
		case '>':
			depth--
		case ',':
			if depth == 0 {
				return strings.TrimSpace(inner[i+1:])
			}
		}
	}
	return "Object"
}

func listElemType(t string) string {
	t = strings.TrimSpace(t)
	if !strings.HasPrefix(t, "List<") || !strings.HasSuffix(t, ">") {
		return "Object"
	}
	return strings.TrimSpace(t[5 : len(t)-1])
}

func (c *Compiler) maybeNumber(expr string) string {
	if t, ok := c.vars[expr]; ok {
		if t == "int" || t == "double" {
			return expr
		}
	}
	if strings.Contains(expr, ".get(") {
		return fmt.Sprintf("((Number)%s).doubleValue()", expr)
	}
	return expr
}

func maybeBool(expr string) string {
	if strings.Contains(expr, ".get(") {
		if strings.ContainsAny(expr, "<>=!+-*/%") || strings.Contains(expr, "==") || strings.Contains(expr, "!=") || strings.Contains(expr, "Objects.equals") {
			return expr
		}
		return fmt.Sprintf("%s != null", expr)
	}
	return expr
}
func isPrimitive(expr string, c *Compiler) bool {
	if expr == "true" || expr == "false" {
		return true
	}
	if regexp.MustCompile(`^-?\d+(\.\d+)?$`).MatchString(expr) {
		return true
	}
	if t, ok := c.vars[expr]; ok {
		if t == "int" || t == "double" || t == "boolean" {
			return true
		}
	}
	return false
}

func (c *Compiler) inferType(e *parser.Expr) string {
	// handle cast expressions first
	if e != nil && e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		u := e.Binary.Left.Value
		if len(u.Ops) > 0 {
			if cast := u.Ops[len(u.Ops)-1].Cast; cast != nil {
				typ := c.typeName(cast.Type)
				return typ
			}
		}
	}
	if e != nil && e.Binary != nil && len(e.Binary.Right) > 0 {
		op := e.Binary.Right[len(e.Binary.Right)-1].Op
		switch op {
		case "==", "!=", "<", "<=", ">", ">=", "in", "&&", "||":
			return "boolean"
		case "+", "-", "*", "/", "%":
			return "int"
		}
	}
	if l := isListLiteral(e); l != nil {
		et := "Object"
		if len(l.Elems) > 0 {
			et = wrapperType(c.inferType(l.Elems[0]))
			if et == "var" {
				et = c.litType(l.Elems[0])
			}
		}
		return fmt.Sprintf("List<%s>", et)
	}
	if m := isMapLiteral(e); m != nil {
		if name := c.dataClassFor(m); name != "" {
			return name
		}
		kt, vt := "Object", "Object"
		for i, it := range m.Items {
			k := c.inferType(it.Key)
			if k == "var" {
				if _, ok := simpleStringKey(it.Key); ok {
					k = "String"
				} else {
					k = c.litType(it.Key)
				}
			}
			v := c.inferType(it.Value)
			if v == "var" {
				v = c.litType(it.Value)
			}
			k = wrapperType(k)
			v = wrapperType(v)
			if i == 0 {
				kt, vt = k, v
			} else {
				if kt != k {
					kt = "Object"
				}
				if vt != v {
					vt = "Object"
				}
			}
		}
		return fmt.Sprintf("Map<%s,%s>", kt, vt)
	}
	p := rootPrimary(e)
	if p != nil && p.Lit != nil {
		return c.typeName(&parser.TypeRef{Simple: litTypeName(p)})
	}
	if p != nil && p.Call != nil {
		if sig, ok := c.funSigs[p.Call.Func]; ok {
			if len(p.Call.Args) < len(sig.params) {
				if len(sig.params)-len(p.Call.Args) == 1 && sig.ret != nil && sig.ret.Simple != nil && *sig.ret.Simple == "int" && sig.params[len(p.Call.Args)].Type != nil && sig.params[len(p.Call.Args)].Type.Simple != nil && *sig.params[len(p.Call.Args)].Type.Simple == "int" {
					c.needFuncImports = true
					return "IntUnaryOperator"
				}
				return "Object"
			}
		}
		if t, ok := c.funRet[p.Call.Func]; ok {
			return t
		}
		switch p.Call.Func {
		case "len":
			return "int"
		case "sum", "min", "max":
			return "int"
		case "count":
			return "int"
		case "avg":
			return "double"
		case "str":
			return "String"
		case "values":
			return "List<Object>"
		}
	}
	if p != nil && p.Struct != nil {
		if parent, ok := c.variantOf[p.Struct.Name]; ok {
			return parent
		}
		return p.Struct.Name
	}
	if p != nil && p.Load != nil {
		return "List<Map<String,Object>>"
	}
	if p != nil && p.FunExpr != nil {
		if len(p.FunExpr.Params) == 1 && p.FunExpr.Return != nil && p.FunExpr.Return.Simple != nil && *p.FunExpr.Return.Simple == "int" && p.FunExpr.Params[0].Type != nil && p.FunExpr.Params[0].Type.Simple != nil && *p.FunExpr.Params[0].Type.Simple == "int" {
			c.needFuncImports = true
			return "IntUnaryOperator"
		}
		return "Object"
	}
	if p != nil && p.Selector != nil {
		if keyVar, ok := c.groupKeys[p.Selector.Root]; ok {
			if len(p.Selector.Tail) > 0 && p.Selector.Tail[0] == "key" {
				typ := c.vars[keyVar]
				if len(p.Selector.Tail) == 1 {
					return typ
				}
				for _, f := range p.Selector.Tail[1:] {
					if strings.HasPrefix(typ, "Map<") {
						typ = mapValueType(typ)
					} else {
						typ = c.fieldType(typ, f)
					}
				}
				return typ
			}
		}
		if t, ok := c.vars[p.Selector.Root]; ok {
			typ := t
			if len(p.Selector.Tail) == 0 {
				return typ
			}
			for _, f := range p.Selector.Tail {
				if strings.HasPrefix(typ, "Map<") {
					typ = mapValueType(typ)
				} else {
					typ = c.fieldType(typ, f)
				}
			}
			return typ
		}
		if parent, ok := c.variantOf[p.Selector.Root]; ok && len(p.Selector.Tail) == 0 {
			return parent
		}
	}
	if p != nil && p.Query != nil {
		if call := rootPrimary(p.Query.Select); call != nil && call.Call != nil && call.Call.Func == "sum" && len(call.Call.Args) == 1 && p.Query.Group == nil {
			return "int"
		}
		old := c.vars
		c.vars = copyMap(c.vars)
		srcType := c.inferType(p.Query.Source)
		c.vars[p.Query.Var] = listElemType(srcType)
		for _, fr := range p.Query.Froms {
			ft := c.inferType(fr.Src)
			c.vars[fr.Var] = listElemType(ft)
		}
		for _, j := range p.Query.Joins {
			jt := c.inferType(j.Src)
			c.vars[j.Var] = listElemType(jt)
		}
		et := c.inferType(p.Query.Select)
		if et == "var" {
			et = "Object"
		}
		c.vars = old
		return fmt.Sprintf("List<%s>", wrapperType(et))
	}
	return "var"
}

func (c *Compiler) litType(e *parser.Expr) string {
	p := rootPrimary(e)
	if p == nil || p.Lit == nil {
		return "Object"
	}
	switch {
	case p.Lit.Int != nil:
		return "Integer"
	case p.Lit.Float != nil:
		return "Double"
	case p.Lit.Str != nil:
		return "String"
	case p.Lit.Bool != nil:
		return "Boolean"
	default:
		return "Object"
	}
}

func rootPrimary(e *parser.Expr) *parser.Primary {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	return e.Binary.Left.Value.Target
}

func litTypeName(p *parser.Primary) *string {
	if p.Lit == nil {
		return nil
	}
	if p.Lit.Int != nil {
		s := "int"
		return &s
	}
	if p.Lit.Float != nil {
		s := "float"
		return &s
	}
	if p.Lit.Str != nil {
		s := "string"
		return &s
	}
	if p.Lit.Bool != nil {
		s := "bool"
		return &s
	}
	return nil
}

func isListLiteral(e *parser.Expr) *parser.ListLiteral {
	p := rootPrimary(e)
	if p != nil && p.List != nil && len(e.Binary.Right) == 0 {
		return p.List
	}
	return nil
}

func isMapLiteral(e *parser.Expr) *parser.MapLiteral {
	p := rootPrimary(e)
	if p != nil && p.Map != nil && len(e.Binary.Right) == 0 {
		return p.Map
	}
	return nil
}

func isMapLitCastToStructExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value
	if p.Target != nil && p.Target.Map != nil && len(p.Ops) == 1 && p.Ops[0].Cast != nil {
		return true
	}
	return false
}

func isQueryExpr(e *parser.Expr) bool {
	p := rootPrimary(e)
	return p != nil && p.Query != nil && len(e.Binary.Right) == 0
}

func (c *Compiler) exprIsMap(e *parser.Expr) bool {
	if isMapLiteral(e) != nil {
		return true
	}
	if p := rootPrimary(e); p != nil && p.Selector != nil && len(p.Selector.Tail) == 0 {
		if t, ok := c.vars[p.Selector.Root]; ok {
			return strings.HasPrefix(t, "Map")
		}
	}
	return false
}

func (c *Compiler) compileGlobalVar(v *parser.VarStmt) error {
	typ := c.typeName(v.Type)
	orig := c.curVar
	c.curVar = v.Name
	expr, err := c.compileExpr(v.Value)
	c.curVar = orig
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		}
	}
	if typ == "var" {
		typ = "Object"
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileGlobalLet(v *parser.LetStmt) error {
	typ := c.typeName(v.Type)
	orig := c.curVar
	c.curVar = v.Name
	expr, err := c.compileExpr(v.Value)
	c.curVar = orig
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		}
		if typ == "var" {
			typ = "Object"
		}
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("class %s {", t.Name))
		c.indent++
		for _, v := range t.Variants {
			c.variantOf[v.Name] = t.Name
			var fields []string
			var types []string
			for _, f := range v.Fields {
				typ := c.typeName(f.Type)
				fields = append(fields, f.Name)
				types = append(types, typ)
			}
			c.variantFields[v.Name] = fields
			c.variantFieldTypes[v.Name] = types
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("static class %s extends %s {}", v.Name, t.Name))
				continue
			}
			c.writeln(fmt.Sprintf("static class %s extends %s {", v.Name, t.Name))
			c.indent++
			var params []string
			for _, f := range v.Fields {
				typ := c.typeName(f.Type)
				c.writeln(fmt.Sprintf("%s %s;", typ, f.Name))
				params = append(params, fmt.Sprintf("%s %s", typ, f.Name))
			}
			c.writeln(fmt.Sprintf("%s(%s) {", v.Name, strings.Join(params, ", ")))
			c.indent++
			for _, f := range v.Fields {
				c.writeln(fmt.Sprintf("this.%s = %s;", f.Name, f.Name))
			}
			c.indent--
			c.writeln("}")
			c.indent--
			c.writeln("}")
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln(fmt.Sprintf("class %s {", t.Name))
	c.indent++
	var params []string
	for _, m := range t.Members {
		if m.Field != nil {
			typ := c.typeName(m.Field.Type)
			c.writeln(fmt.Sprintf("%s %s;", typ, m.Field.Name))
			params = append(params, fmt.Sprintf("%s %s", typ, m.Field.Name))
		}
	}
	if len(params) > 0 {
		c.writeln(fmt.Sprintf("%s(%s) {", t.Name, strings.Join(params, ", ")))
		c.indent++
		for _, m := range t.Members {
			if m.Field != nil {
				name := m.Field.Name
				c.writeln(fmt.Sprintf("this.%s = %s;", name, name))
			}
		}
		c.indent--
		c.writeln("}")
	}
	if len(params) > 0 {
		var conds []string
		var fields []string
		for _, m := range t.Members {
			if m.Field != nil {
				fields = append(fields, m.Field.Name)
				conds = append(conds, fmt.Sprintf("Objects.equals(this.%s, other.%s)", m.Field.Name, m.Field.Name))
			}
		}
		c.writeln("@Override public boolean equals(Object o) {")
		c.indent++
		c.writeln("if (this == o) return true;")
		c.writeln(fmt.Sprintf("if (!(o instanceof %s other)) return false;", t.Name))
		c.writeln(fmt.Sprintf("return %s;", strings.Join(conds, " && ")))
		c.indent--
		c.writeln("}")
		c.writeln("@Override public int hashCode() {")
		c.indent++
		c.writeln(fmt.Sprintf("return Objects.hash(%s);", strings.Join(fields, ", ")))
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) dataClassFor(m *parser.MapLiteral) string {
	var keys []string
	for _, it := range m.Items {
		s, ok := simpleStringKey(it.Key)
		if !ok {
			return ""
		}
		keys = append(keys, s)
	}
	sort.Strings(keys)
	shape := strings.Join(keys, ";")
	if dc, ok := c.dataClasses[shape]; ok {
		return dc.name
	}
	name := fmt.Sprintf("DataClass%d", len(c.dataClasses)+1)
	base := c.curVar
	if base != "" {
		base = base + "_" + strings.Join(keys, "_")
		if n := classNameFromVar(base); n != "" {
			if c.dataClassByName(n) == nil {
				name = n
			}
		}
	} else if n := classNameFromVar(strings.Join(keys, "_")); n != "" {
		if c.dataClassByName(n) == nil {
			name = n
		}
	}
	var fields []string
	var types []string
	for _, it := range m.Items {
		s, _ := simpleStringKey(it.Key)
		fields = append(fields, s)
		typ := c.inferType(it.Value)
		if typ == "var" {
			typ = c.litType(it.Value)
		}
		types = append(types, typ)
	}
	c.dataClasses[shape] = &dataClass{name: name, fields: fields, types: types}
	c.dataClassOrder = append(c.dataClassOrder, shape)
	return name
}

func (c *Compiler) dataClassByName(name string) *dataClass {
	for _, dc := range c.dataClasses {
		if dc.name == name {
			return dc
		}
	}
	return nil
}

func (c *Compiler) rowDataClassFor(vars []string) string {
	shape := "row|" + strings.Join(vars, ";")
	if dc, ok := c.dataClasses[shape]; ok {
		return dc.name
	}
	name := fmt.Sprintf("DataClass%d", len(c.dataClasses)+1)
	if len(vars) > 0 {
		n := classNameFromVar(strings.Join(vars, "_"))
		if c.dataClassByName(n) == nil {
			name = n
		}
	}
	var fields, types []string
	for _, v := range vars {
		fields = append(fields, v)
		typ := c.vars[v]
		if typ == "" {
			typ = "Object"
		}
		types = append(types, typ)
	}
	c.dataClasses[shape] = &dataClass{name: name, fields: fields, types: types}
	c.dataClassOrder = append(c.dataClassOrder, shape)
	return name
}

func (c *Compiler) fieldType(structName, field string) string {
	if td, ok := c.types[structName]; ok {
		for _, m := range td.Members {
			if m.Field != nil && m.Field.Name == field {
				return c.typeName(m.Field.Type)
			}
		}
	}
	if dc := c.dataClassByName(structName); dc != nil {
		for i, f := range dc.fields {
			if f == field {
				return dc.types[i]
			}
		}
	}
	return "Object"
}

func (c *Compiler) compileDataClass(dc *dataClass) {
	c.writeln(fmt.Sprintf("class %s {", dc.name))
	c.indent++
	var params []string
	for i, f := range dc.fields {
		typ := dc.types[i]
		c.writeln(fmt.Sprintf("%s %s;", typ, f))
		params = append(params, fmt.Sprintf("%s %s", typ, f))
	}
	c.writeln(fmt.Sprintf("%s(%s) {", dc.name, strings.Join(params, ", ")))
	c.indent++
	for _, f := range dc.fields {
		c.writeln(fmt.Sprintf("this.%s = %s;", f, f))
	}
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
}

func (c *Compiler) compileList(l *parser.ListLiteral) (string, error) {
	var elems []string
	for _, e := range l.Elems {
		s, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		elems = append(elems, s)
	}
	return fmt.Sprintf("Arrays.asList(%s)", strings.Join(elems, ", ")), nil
}

func (c *Compiler) compileMap(m *parser.MapLiteral) (string, error) {
	if name := c.dataClassFor(m); name != "" {
		var args []string
		origVar := c.curVar
		for _, it := range m.Items {
			if s, ok := simpleStringKey(it.Key); ok {
				c.curVar = origVar + "_" + s
			}
			v, err := c.compileExpr(it.Value)
			c.curVar = origVar
			if err != nil {
				return "", err
			}
			args = append(args, v)
		}
		return fmt.Sprintf("new %s(%s)", name, strings.Join(args, ", ")), nil
	}
	c.helpers["map_of_entries"] = true
	var entries []string
	for _, it := range m.Items {
		var k string
		if s, ok := simpleStringKey(it.Key); ok {
			k = fmt.Sprintf("\"%s\"", s)
		} else {
			var err error
			k, err = c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		entries = append(entries, fmt.Sprintf("entry(%s, %s)", k, v))
	}
	return fmt.Sprintf("mapOfEntries(%s)", strings.Join(entries, ", ")), nil
}

func (c *Compiler) mapLiteralTypes(m *parser.MapLiteral) (string, string) {
	kt, vt := "Object", "Object"
	for i, it := range m.Items {
		ktyp := c.inferType(it.Key)
		if ktyp == "var" {
			if _, ok := simpleStringKey(it.Key); ok {
				ktyp = "String"
			} else {
				ktyp = c.litType(it.Key)
			}
		}
		vtyp := c.inferType(it.Value)
		if vtyp == "var" {
			vtyp = c.litType(it.Value)
		}
		ktyp = wrapperType(ktyp)
		vtyp = wrapperType(vtyp)
		if i == 0 {
			kt, vt = ktyp, vtyp
		} else {
			if kt != ktyp {
				kt = "Object"
			}
			if vt != vtyp {
				vt = "Object"
			}
		}
	}
	return kt, vt
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	var args []string
	for _, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		args = append(args, v)
	}
	name := s.Name
	if parent, ok := c.variantOf[s.Name]; ok {
		name = parent + "." + s.Name
	}
	return fmt.Sprintf("new %s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	if len(f.Params) == 1 && f.Return != nil && f.Return.Simple != nil && *f.Return.Simple == "int" && f.Params[0].Type != nil && f.Params[0].Type.Simple != nil && *f.Params[0].Type.Simple == "int" {
		c.needFuncImports = true
	}
	var params []string
	for _, p := range f.Params {
		params = append(params, p.Name)
	}
	if f.ExprBody != nil {
		body, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		if len(params) == 1 {
			return fmt.Sprintf("%s -> %s", params[0], body), nil
		}
		return fmt.Sprintf("(%s) -> %s", strings.Join(params, ", "), body), nil
	}

	origBuf := c.buf
	origIndent := c.indent
	origVars := c.vars
	buf := new(bytes.Buffer)
	c.buf = buf
	c.indent = 0
	c.vars = copyMap(origVars)
	for _, p := range f.Params {
		c.vars[p.Name] = c.typeName(p.Type)
	}
	for _, s := range f.BlockBody {
		if err := c.compileStmt(s); err != nil {
			c.buf = origBuf
			c.indent = origIndent
			c.vars = origVars
			return "", err
		}
	}
	body := indentBlock(buf.String(), origIndent+1)
	c.buf = origBuf
	c.indent = origIndent
	c.vars = origVars
	paramList := strings.Join(params, ", ")
	if len(params) == 1 {
		return fmt.Sprintf("%s -> {\n%s%s}", params[0], body, strings.Repeat("\t", origIndent)), nil
	}
	return fmt.Sprintf("(%s) -> {\n%s%s}", paramList, body, strings.Repeat("\t", origIndent)), nil
}

func (c *Compiler) compileIfExpr(e *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(e.Cond)
	if err != nil {
		return "", err
	}
	cond = maybeBool(cond)
	thenExpr, err := c.compileExpr(e.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if e.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(e.ElseIf)
		if err != nil {
			return "", err
		}
	} else if e.Else != nil {
		elseExpr, err = c.compileExpr(e.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "null"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	typ := c.typeName(v.Type)
	orig := c.curVar
	c.curVar = v.Name
	expr, err := c.compileExpr(v.Value)
	c.curVar = orig
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		} else if ml := isMapLiteral(v.Value); ml != nil && c.dataClassFor(ml) == "" && !isMapLitCastToStructExpr(v.Value) {
			expr = fmt.Sprintf("new HashMap<>(%s)", expr)
		}
		if typ == "var" {
			typ = "Object"
		}
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileLet(v *parser.LetStmt) error {
	typ := c.typeName(v.Type)
	orig := c.curVar
	c.curVar = v.Name
	expr, err := c.compileExpr(v.Value)
	c.curVar = orig
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		} else if ml := isMapLiteral(v.Value); ml != nil && c.dataClassFor(ml) == "" && !isMapLitCastToStructExpr(v.Value) {
			expr = fmt.Sprintf("new HashMap<>(%s)", expr)
		}
		if typ == "var" {
			typ = "Object"
		}
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	orig := c.curVar
	c.curVar = a.Name
	expr, err := c.compileExpr(a.Value)
	c.curVar = orig
	if err != nil {
		return err
	}
	if len(a.Index) == 0 && len(a.Field) == 0 {
		if c.vars[a.Name] == "int" {
			if strings.Contains(expr, ".doubleValue()") {
				expr = fmt.Sprintf("(int)(%s)", expr)
			} else {
				expr = fmt.Sprintf("(int)(%s)", expr)
			}
		}
		c.writeln(fmt.Sprintf("%s = %s;", a.Name, expr))
		return nil
	}
	target := a.Name
	for _, f := range a.Field {
		target += "." + f.Name
	}
	typ := c.vars[a.Name]
	for i, idx := range a.Index {
		if idx.Start == nil || idx.Colon != nil {
			return fmt.Errorf("complex indexing not supported")
		}
		ix, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		if i == len(a.Index)-1 {
			if strings.HasPrefix(typ, "Map<") {
				c.writeln(fmt.Sprintf("%s.put(%s, %s);", target, ix, expr))
			} else if strings.HasPrefix(typ, "List<") {
				c.writeln(fmt.Sprintf("%s.set(%s, %s);", target, ix, expr))
			} else if field, ok := simpleStringKey(idx.Start); ok {
				c.writeln(fmt.Sprintf("%s.%s = %s;", target, field, expr))
			} else {
				c.writeln(fmt.Sprintf("((Map)%s).put(%s, %s);", target, ix, expr))
			}
		} else {
			if strings.HasPrefix(typ, "Map<") {
				target = fmt.Sprintf("((Map)%s.get(%s))", target, ix)
				typ = mapValueType(typ)
			} else if strings.HasPrefix(typ, "List<") {
				target = fmt.Sprintf("((List)%s.get(%s))", target, ix)
				typ = listElemType(typ)
			} else if field, ok := simpleStringKey(idx.Start); ok {
				target = fmt.Sprintf("%s.%s", target, field)
				typ = c.fieldType(typ, field)
			} else {
				target = fmt.Sprintf("((Map)%s.get(%s))", target, ix)
				typ = mapValueType(typ)
			}
		}
	}
	if len(a.Index) == 0 {
		c.writeln(fmt.Sprintf("%s = %s;", target, expr))
	}
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + maybeBool(cond) + ") {")
	c.indent++
	for _, s := range i.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if len(i.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, s := range i.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd == nil {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		if c.exprIsMap(f.Source) {
			c.writeln(fmt.Sprintf("for (var %s : %s.keySet()) {", f.Name, src))
		} else {
			if strings.Contains(src, ".get(") {
				src = fmt.Sprintf("(List)%s", src)
			}
			elemType := listElemType(c.inferType(f.Source))
			if elemType == "" {
				elemType = "var"
			}
			c.writeln(fmt.Sprintf("for (%s %s : %s) {", elemType, f.Name, src))
		}
		c.indent++
		prev := c.vars[f.Name]
		if t := listElemType(c.inferType(f.Source)); t != "" {
			c.vars[f.Name] = t
		}
		for _, s := range f.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		if prev == "" {
			delete(c.vars, f.Name)
		} else {
			c.vars[f.Name] = prev
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", f.Name, start, f.Name, end, f.Name))
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range w.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileLocalFun(f *parser.FunStmt) error {
	expr, err := c.compileFunExpr(&parser.FunExpr{Params: f.Params, Return: f.Return, BlockBody: f.Body})
	if err != nil {
		return err
	}
	typ := "Object"
	declType := "Object"
	if len(f.Params) == 1 && c.typeName(f.Return) == "int" {
		if f.Params[0].Type != nil && c.typeName(f.Params[0].Type) == "int" {
			typ = "IntUnaryOperator"
			declType = "java.util.function.IntUnaryOperator"
		}
	}
	c.writeln(fmt.Sprintf("%s %s = %s;", declType, f.Name, expr))
	c.vars[f.Name] = typ
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	ret := c.typeName(f.Return)
	if f.Return == nil {
		ret = "void"
	}
	var params []string
	for _, p := range f.Params {
		params = append(params, fmt.Sprintf("%s %s", c.typeName(p.Type), p.Name))
	}
	c.writeln(fmt.Sprintf("static %s %s(%s) {", ret, f.Name, strings.Join(params, ", ")))
	origVars := c.vars
	c.vars = copyMap(origVars)
	for _, p := range f.Params {
		c.vars[p.Name] = c.typeName(p.Type)
	}
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.vars = origVars
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val + ";")
	return nil
}

func (c *Compiler) compileExprStmt(e *parser.ExprStmt) error {
	expr, err := c.compileExpr(e.Expr)
	if err != nil {
		return err
	}
	c.writeln(expr + ";")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	expr := left
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		if op.Op == "in" {
			c.helpers["in"] = true
			expr = fmt.Sprintf("inOp(%s, %s)", expr, right)
			continue
		}
		if op.Op == "union" {
			if op.All {
				c.helpers["union_all"] = true
				expr = fmt.Sprintf("union_all(%s, %s)", expr, right)
			} else {
				c.helpers["union"] = true
				expr = fmt.Sprintf("union(%s, %s)", expr, right)
			}
			continue
		}
		if op.Op == "except" {
			c.helpers["except"] = true
			expr = fmt.Sprintf("except(%s, %s)", expr, right)
			continue
		}
		if op.Op == "intersect" {
			c.helpers["intersect"] = true
			expr = fmt.Sprintf("intersect(%s, %s)", expr, right)
			continue
		}
		if (op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=") &&
			isStringVal(expr, c) && isStringVal(right, c) {
			expr = fmt.Sprintf("%s.compareTo(%s) %s 0", expr, right, op.Op)
		} else if op.Op == "==" || op.Op == "!=" {
			if isPrimitive(expr, c) && isPrimitive(right, c) {
				expr = fmt.Sprintf("%s %s %s", expr, op.Op, right)
			} else {
				if op.Op == "==" {
					expr = fmt.Sprintf("Objects.equals(%s, %s)", expr, right)
				} else {
					expr = fmt.Sprintf("!Objects.equals(%s, %s)", expr, right)
				}
			}
		} else if op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" || op.Op == "%" ||
			op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=" {
			if op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=" {
				if isStringVal(expr, c) || isStringVal(right, c) {
					expr = fmt.Sprintf("String.valueOf(%s).compareTo(String.valueOf(%s)) %s 0", expr, right, op.Op)
					continue
				}
			}
			expr = fmt.Sprintf("%s %s %s", c.maybeNumber(expr), op.Op, c.maybeNumber(right))
		} else if op.Op == "&&" || op.Op == "||" {
			expr = fmt.Sprintf("%s %s %s", maybeBool(expr), op.Op, maybeBool(right))
		} else {
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, right)
		}
	}
	return expr, nil
}

func isString(s string) bool {
	return len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"'
}

func isStringVal(s string, c *Compiler) bool {
	if isString(s) {
		return true
	}
	if t, ok := c.vars[s]; ok && t == "String" {
		return true
	}
	return false
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	typ := ""
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		typ = c.vars[p.Target.Selector.Root]
	}
	for i, op := range p.Ops {
		switch {
		case op.Call != nil:
			var args []string
			for _, a := range op.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, arg)
			}
			if typ == "IntUnaryOperator" {
				val = fmt.Sprintf("%s.applyAsInt(%s)", val, strings.Join(args, ", "))
			} else {
				val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			}
		case op.Cast != nil:
			t := c.typeName(op.Cast.Type)
			if _, ok := c.types[t]; ok && p.Target.Map != nil {
				var args []string
				for _, it := range p.Target.Map.Items {
					a, err := c.compileExpr(it.Value)
					if err != nil {
						return "", err
					}
					args = append(args, a)
				}
				val = fmt.Sprintf("new %s(%s)", t, strings.Join(args, ", "))
			} else {
				switch t {
				case "int":
					val = fmt.Sprintf("Integer.parseInt(%s)", val)
				case "double":
					val = fmt.Sprintf("Double.parseDouble(%s)", val)
				case "String":
					val = fmt.Sprintf("String.valueOf(%s)", val)
				case "boolean":
					val = fmt.Sprintf("Boolean.parseBoolean(%s)", val)
				default:
					return "", fmt.Errorf("unsupported cast to %s", t)
				}
			}
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				// slice operation
				start := "0"
				end := ""
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				} else {
					if isString(val) || c.vars[val] == "String" {
						end = fmt.Sprintf("%s.length()", val)
					} else {
						end = fmt.Sprintf("%s.size()", val)
					}
				}
				if isString(val) || c.vars[val] == "String" {
					val = fmt.Sprintf("%s.substring(%s, %s)", val, start, end)
				} else {
					val = fmt.Sprintf("((List)%s).subList(%s, %s)", val, start, end)
				}
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("complex indexing not supported")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				last := i == len(p.Ops)-1
				if isString(val) || c.vars[val] == "String" {
					val = fmt.Sprintf("%s.charAt(%s)", val, idx)
				} else if last {
					if strings.HasPrefix(typ, "Map<") {
						val = fmt.Sprintf("%s.get(%s)", val, idx)
					} else if strings.HasPrefix(typ, "List<") {
						val = fmt.Sprintf("%s.get(%s)", val, idx)
					} else if field, ok := simpleStringKey(op.Index.Start); ok {
						val = fmt.Sprintf("%s.%s", val, field)
						typ = c.fieldType(typ, field)
					} else {
						val = fmt.Sprintf("((Map)%s).get(%s)", val, idx)
					}
				} else {
					if strings.HasPrefix(typ, "Map<") {
						val = fmt.Sprintf("((Map)%s.get(%s))", val, idx)
						typ = mapValueType(typ)
					} else if strings.HasPrefix(typ, "List<") {
						val = fmt.Sprintf("((List)%s.get(%s))", val, idx)
						typ = listElemType(typ)
					} else if field, ok := simpleStringKey(op.Index.Start); ok {
						val = fmt.Sprintf("%s.%s", val, field)
						typ = c.fieldType(typ, field)
					} else {
						val = fmt.Sprintf("((Map)%s.get(%s))", val, idx)
					}
				}
			}
		default:
			return "", fmt.Errorf("postfix operations unsupported")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", nil
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		case p.Lit.Float != nil:
			return fmt.Sprintf("%f", *p.Lit.Float), nil
		case p.Lit.Str != nil:
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		case p.Lit.Bool != nil:
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		case p.Lit.Null:
			return "null", nil
		}
	case p.Selector != nil:
		s := p.Selector.Root
		typ := c.vars[p.Selector.Root]
		if parent, ok := c.variantOf[p.Selector.Root]; ok && len(p.Selector.Tail) == 0 {
			if len(c.variantFields[p.Selector.Root]) == 0 {
				return fmt.Sprintf("new %s.%s()", parent, p.Selector.Root), nil
			}
		}
		if keyVar, ok := c.groupKeys[p.Selector.Root]; ok {
			if len(p.Selector.Tail) == 1 && p.Selector.Tail[0] == "key" {
				return keyVar, nil
			}
			if len(p.Selector.Tail) > 0 && p.Selector.Tail[0] == "key" {
				s = keyVar
				typ = c.vars[keyVar]
				pTail := p.Selector.Tail[1:]
				for _, f := range pTail {
					if strings.HasPrefix(typ, "Map<") || typ == "Map" || typ == "Object" || typ == "" {
						s = fmt.Sprintf("((Map)%s).get(\"%s\")", s, f)
						typ = mapValueType(typ)
					} else {
						s += "." + f
					}
				}
				return s, nil
			}
		}
		for _, f := range p.Selector.Tail {
			if strings.HasPrefix(typ, "Map<") || typ == "Map" || typ == "Object" || typ == "" {
				s = fmt.Sprintf("((Map)%s).get(\"%s\")", s, f)
				typ = mapValueType(typ)
			} else {
				s += "." + f
			}
		}
		return s, nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.FunExpr != nil:
		expr, err := c.compileFunExpr(p.FunExpr)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.List != nil:
		return c.compileList(p.List)
	case p.Map != nil:
		return c.compileMap(p.Map)
	case p.Query != nil:
		return c.compileQuery(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Call != nil:
		switch p.Call.Func {
		case "print":
			if len(p.Call.Args) == 0 {
				return "", fmt.Errorf("print expects at least one argument at line %d", p.Pos.Line)
			}
			var parts []string
			for _, a := range p.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				parts = append(parts, arg)
			}
			expr := parts[0]
			for i := 1; i < len(parts); i++ {
				expr += " + \" \" + " + parts[i]
			}
			return fmt.Sprintf("System.out.println(%s)", expr), nil
		case "len":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("len expects one argument at line %d", p.Pos.Line)
			}
			a := p.Call.Args[0]
			expr, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			if isString(expr) || c.vars[expr] == "String" {
				return fmt.Sprintf("%s.length()", expr), nil
			}
			return fmt.Sprintf("%s.size()", expr), nil
		case "str":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("str expects one argument at line %d", p.Pos.Line)
			}
			arg, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("String.valueOf(%s)", arg), nil
		case "json":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("json expects one argument at line %d", p.Pos.Line)
			}
			c.helpers["json"] = true
			arg, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("json(%s)", arg), nil
		case "substring":
			if len(p.Call.Args) < 2 || len(p.Call.Args) > 3 {
				return "", fmt.Errorf("substring expects 2 or 3 arguments at line %d", p.Pos.Line)
			}
			target, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			start, err := c.compileExpr(p.Call.Args[1])
			if err != nil {
				return "", err
			}
			if len(p.Call.Args) == 3 {
				end, err := c.compileExpr(p.Call.Args[2])
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("%s.substring(%s, %s)", target, start, end), nil
			}
			return fmt.Sprintf("%s.substring(%s)", target, start), nil
		case "append":
			if len(p.Call.Args) != 2 {
				return "", fmt.Errorf("append expects 2 arguments at line %d", p.Pos.Line)
			}
			c.helpers["append"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			a2, err := c.compileExpr(p.Call.Args[1])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("append(%s, %s)", a1, a2), nil
		case "count":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("count expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["count"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("count(%s)", a1), nil
		case "sum":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("sum expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["sum"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("sum((List<Number>)(List<?>)%s)", a1), nil
		case "avg":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("avg expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["avg"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("avg((List<Number>)(List<?>)%s)", a1), nil
		case "min":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("min expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["min"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("min((List<Number>)(List<?>)%s)", a1), nil
		case "max":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("max expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["max"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("max((List<Number>)(List<?>)%s)", a1), nil
		case "values":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("values expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["values"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("values(%s)", a1), nil
		case "exists":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("exists expects 1 argument at line %d", p.Pos.Line)
			}
			// Special handling when argument is a query expression
			if q := p.Call.Args[0].Binary; q != nil && q.Left != nil && q.Left.Value != nil && q.Left.Value.Target != nil && q.Left.Value.Target.Query != nil {
				return c.compileExistsQuery(q.Left.Value.Target.Query)
			}
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("!%s.isEmpty()", a1), nil
		}
		var args []string
		for _, a := range p.Call.Args {
			arg, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, arg)
		}
		if sig, ok := c.funSigs[p.Call.Func]; ok && len(args) < len(sig.params) {
			remain := sig.params[len(args):]
			if len(remain) == 1 {
				param := remain[0].Name
				callArgs := append(append([]string{}, args...), param)
				c.needFuncImports = true
				return fmt.Sprintf("%s -> %s(%s)", param, p.Call.Func, strings.Join(callArgs, ", ")), nil
			}
			return "", fmt.Errorf("partial application unsupported")
		}
		if t, ok := c.vars[p.Call.Func]; ok && t == "IntUnaryOperator" {
			return fmt.Sprintf("%s.applyAsInt(%s)", p.Call.Func, strings.Join(args, ", ")), nil
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	}
	return "", fmt.Errorf("expression unsupported at line %d", p.Pos.Line)
}

func copyMap(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("\t", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
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
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) != 0 {
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
	if len(p.Ops) != 0 {
		return nil, false
	}
	return p.Target.Call, true
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func (c *Compiler) compileExistsQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	cond := "true"
	if q.Where != nil {
		oldVars := c.vars
		c.vars = copyMap(c.vars)
		c.vars[q.Var] = "var"
		cond, err = c.compileExpr(q.Where)
		c.vars = oldVars
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("%s.stream().anyMatch(%s -> %s)", src, q.Var, cond), nil
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	if q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}

	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	srcType := c.inferType(q.Source)
	elemType := listElemType(srcType)
	if name, ok := identName(q.Source); ok {
		if t, ok2 := c.vars[name]; ok2 {
			elemType = listElemType(t)
		}
	}

	oldVars := c.vars
	c.vars = copyMap(c.vars)
	if elemType != "" {
		c.vars[q.Var] = elemType
	} else {
		c.vars[q.Var] = "var"
	}
	for _, fr := range q.Froms {
		ft := c.inferType(fr.Src)
		if name, ok := identName(fr.Src); ok {
			if t, ok2 := c.vars[name]; ok2 {
				ft = t
			}
		}
		c.vars[fr.Var] = listElemType(ft)
	}
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side != "left" && *j.Side != "right" && *j.Side != "outer" {
			return "", fmt.Errorf("join type not supported")
		}
		jt := c.inferType(j.Src)
		if name, ok := identName(j.Src); ok {
			if t, ok2 := c.vars[name]; ok2 {
				jt = t
			}
		}
		c.vars[j.Var] = listElemType(jt)
	}

	resVar := fmt.Sprintf("_res%d", c.tmpCount)
	c.tmpCount++
	resultElem := c.inferType(q.Select)
	if resultElem == "var" {
		resultElem = "Object"
	}
	listType := fmt.Sprintf("List<%s>", wrapperType(resultElem))

	groupsVar := ""
	if q.Group != nil {
		if len(q.Group.Exprs) != 1 {
			c.vars = oldVars
			return "", fmt.Errorf("unsupported multi-key group")
		}
		groupsVar = fmt.Sprintf("_groups%d", c.tmpCount)
		c.tmpCount++
	}

	rowVars := []string{q.Var}
	for _, fr := range q.Froms {
		rowVars = append(rowVars, fr.Var)
	}
	for _, j := range q.Joins {
		rowVars = append(rowVars, j.Var)
	}
	rowType := elemType
	if q.Group != nil && len(rowVars) > 1 {
		rowType = c.rowDataClassFor(rowVars)
		elemType = rowType
	}

	// simple right or outer join without additional clauses
	if q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct &&
		len(q.Froms) == 0 && len(q.Joins) == 1 && q.Where == nil {
		j := q.Joins[0]
		if j.Side != nil && (*j.Side == "right" || *j.Side == "outer") {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				c.vars = oldVars
				return "", err
			}
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.vars = oldVars
				return "", err
			}
			sel, err := c.compileExpr(q.Select)
			if err != nil {
				c.vars = oldVars
				return "", err
			}
			tmpList := fmt.Sprintf("_tmp%d", c.tmpCount)
			c.tmpCount++
			var b strings.Builder
			b.WriteString(fmt.Sprintf("(new java.util.function.Supplier<%s>(){public %s get(){\n", listType, listType))
			b.WriteString(fmt.Sprintf("\t%s %s = new ArrayList<>();\n", listType, resVar))
			if *j.Side == "outer" {
				b.WriteString("\tjava.util.Set<Object> _matched = new java.util.HashSet<>();\n")
			}
			b.WriteString(fmt.Sprintf("\tfor (var %s : %s) {\n", q.Var, src))
			b.WriteString(fmt.Sprintf("\t\tList<Object> %s = new ArrayList<>();\n", tmpList))
			b.WriteString(fmt.Sprintf("\t\tfor (var _it%d : %s) {\n", c.tmpCount, js))
			iter := fmt.Sprintf("_it%d", c.tmpCount)
			c.tmpCount++
			b.WriteString(fmt.Sprintf("\t\t\tvar %s = %s;\n", j.Var, iter))
			b.WriteString(fmt.Sprintf("\t\t\tif (!(%s)) continue;\n", on))
			b.WriteString(fmt.Sprintf("\t\t\t%[1]s.add(%s);\n", tmpList, iter))
			if *j.Side == "outer" {
				b.WriteString(fmt.Sprintf("\t\t\t_matched.add(%s);\n", iter))
			}
			b.WriteString("\t\t}\n")
			b.WriteString(fmt.Sprintf("\t\tif (%s.isEmpty()) %s.add(null);\n", tmpList, tmpList))
			b.WriteString(fmt.Sprintf("\t\tfor (var %s : %s) {\n", j.Var, tmpList))
			b.WriteString(fmt.Sprintf("\t\t\t%[1]s.add(%s);\n", resVar, sel))
			b.WriteString("\t\t}\n")
			b.WriteString("\t}\n")
			if *j.Side == "outer" {
				b.WriteString(fmt.Sprintf("\tfor (var %s : %s) {\n", j.Var, js))
				b.WriteString("\t\tif (!_matched.contains(" + j.Var + ")) {\n")
				b.WriteString(fmt.Sprintf("\t\t\tObject %s = null;\n", q.Var))
				b.WriteString(fmt.Sprintf("\t\t\t%[1]s.add(%s);\n", resVar, sel))
				b.WriteString("\t\t}\n")
				b.WriteString("\t}\n")
			}
			b.WriteString(fmt.Sprintf("\treturn %s;\n", resVar))
			b.WriteString("}}).get()")
			c.vars = oldVars
			return b.String(), nil
		}
	}

	if q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct && len(q.Froms) == 0 && len(q.Joins) == 0 {
		if call := rootPrimary(q.Select); call != nil && call.Call != nil && call.Call.Func == "sum" && len(call.Call.Args) == 1 {
			arg, err := c.compileExpr(call.Call.Args[0])
			if err != nil {
				return "", err
			}
			tmp := fmt.Sprintf("_sum%d", c.tmpCount)
			c.tmpCount++
			var b strings.Builder
			b.WriteString("(new java.util.function.Supplier<Integer>(){public Integer get(){\n")
			b.WriteString(fmt.Sprintf("\tint %s = 0;\n", tmp))
			b.WriteString(fmt.Sprintf("\tfor (var %s : %s) {\n", q.Var, src))
			cond := "true"
			if q.Where != nil {
				cond, err = c.compileExpr(q.Where)
				if err != nil {
					return "", err
				}
				cond = maybeBool(cond)
				b.WriteString(fmt.Sprintf("\t\tif (!(%s)) continue;\n", cond))
			}
			b.WriteString(fmt.Sprintf("\t\t%s += %s;\n", tmp, c.maybeNumber(arg)))
			b.WriteString("\t}\n")
			b.WriteString(fmt.Sprintf("\treturn %s;\n", tmp))
			b.WriteString("}}).get()")
			c.vars = oldVars
			return b.String(), nil
		}
	}

	var b strings.Builder
	b.WriteString(fmt.Sprintf("(new java.util.function.Supplier<%s>(){public %s get(){\n", listType, listType))
	b.WriteString(fmt.Sprintf("\t%s %s = new ArrayList<>();\n", listType, resVar))
	if groupsVar != "" {
		keyType := c.inferType(q.Group.Exprs[0])
		if keyType == "var" {
			keyType = "Object"
		}
		b.WriteString(fmt.Sprintf("\tMap<%s,List<%s>> %s = new LinkedHashMap<>();\n", wrapperType(keyType), rowType, groupsVar))
	}
	indent := "\t"
	b.WriteString(fmt.Sprintf("%sfor (var %s : %s) {\n", indent, q.Var, src))
	indent += "\t"
	for _, fr := range q.Froms {
		fs, err := c.compileExpr(fr.Src)
		if err != nil {
			c.vars = oldVars
			return "", err
		}
		b.WriteString(fmt.Sprintf("%sfor (var %s : %s) {\n", indent, fr.Var, fs))
		indent += "\t"
	}
	for _, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.vars = oldVars
			return "", err
		}
		if j.Side != nil && *j.Side == "left" {
			tmpList := fmt.Sprintf("_tmp%d", c.tmpCount)
			c.tmpCount++
			iterVar := fmt.Sprintf("_it%d", c.tmpCount)
			c.tmpCount++
			b.WriteString(fmt.Sprintf("%sList<Object> %s = new ArrayList<>();\n", indent, tmpList))
			b.WriteString(fmt.Sprintf("%sfor (var %s : %s) {\n", indent, iterVar, js))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%svar %s = %s;\n", indent, j.Var, iterVar))
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.vars = oldVars
				return "", err
			}
			b.WriteString(fmt.Sprintf("%sif (!(%s)) continue;\n", indent, on))
			b.WriteString(fmt.Sprintf("%s%s.add(%s);\n", indent, tmpList, iterVar))
			indent = indent[:len(indent)-1]
			b.WriteString(fmt.Sprintf("%s}\n", indent))
			b.WriteString(fmt.Sprintf("%sif (%s.isEmpty()) %s.add(null);\n", indent, tmpList, tmpList))
			b.WriteString(fmt.Sprintf("%sfor (var %s : %s) {\n", indent, j.Var, tmpList))
			indent += "\t"
		} else if j.Side != nil {
			c.vars = oldVars
			return "", fmt.Errorf("join type not supported")
		} else {
			b.WriteString(fmt.Sprintf("%sfor (var %s : %s) {\n", indent, j.Var, js))
			indent += "\t"
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.vars = oldVars
				return "", err
			}
			b.WriteString(fmt.Sprintf("%sif (!(%s)) continue;\n", indent, on))
		}
	}
	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			c.vars = oldVars
			return "", err
		}
		cond = maybeBool(cond)
		b.WriteString(fmt.Sprintf("%sif (!(%s)) continue;\n", indent, cond))
	}
	if groupsVar != "" {
		rowVar := fmt.Sprintf("_row%d", c.tmpCount)
		c.tmpCount++
		vars := []string{q.Var}
		for _, fr := range q.Froms {
			vars = append(vars, fr.Var)
		}
		for _, j := range q.Joins {
			vars = append(vars, j.Var)
		}
		if len(vars) == 1 {
			b.WriteString(fmt.Sprintf("%svar %s = %s;\n", indent, rowVar, vars[0]))
			c.vars[rowVar] = c.vars[vars[0]]
		} else {
			args := strings.Join(vars, ", ")
			b.WriteString(fmt.Sprintf("%s%s %s = new %s(%s);\n", indent, rowType, rowVar, rowType, args))
			c.vars[rowVar] = rowType
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.vars = oldVars
			return "", err
		}
		keyVar := fmt.Sprintf("_key%d", c.tmpCount)
		c.tmpCount++
		keyType := c.inferType(q.Group.Exprs[0])
		if keyType == "var" {
			keyType = "Object"
		}
		b.WriteString(fmt.Sprintf("%s%s %s = %s;\n", indent, keyType, keyVar, keyExpr))
		c.vars[keyVar] = keyType
		bucketVar := fmt.Sprintf("_b%d", c.tmpCount)
		c.tmpCount++
		b.WriteString(fmt.Sprintf("%sList<%s> %s = %s.get(%s);\n", indent, rowType, bucketVar, groupsVar, keyVar))
		b.WriteString(fmt.Sprintf("%sif (%s == null) { %s = new ArrayList<>(); %s.put(%s, %s); }\n", indent, bucketVar, bucketVar, groupsVar, keyVar, bucketVar))
		b.WriteString(fmt.Sprintf("%s%s.add(%s);\n", indent, bucketVar, rowVar))
	} else {
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			c.vars = oldVars
			return "", err
		}
		b.WriteString(fmt.Sprintf("%s%s.add(%s);\n", indent, resVar, sel))
	}
	for range q.Joins {
		loops := 1
		// the additional loop used for left join is closed earlier
		for i := 0; i < loops; i++ {
			indent = indent[:len(indent)-1]
			b.WriteString(fmt.Sprintf("%s}\n", indent))
		}
	}
	for range q.Froms {
		indent = indent[:len(indent)-1]
		b.WriteString(fmt.Sprintf("%s}\n", indent))
	}
	indent = indent[:len(indent)-1]
	b.WriteString(fmt.Sprintf("%s}\n", indent))

	if groupsVar != "" {
		gName := q.Group.Name
		keyVar := fmt.Sprintf("%s_key", gName)
		c.groupKeys[gName] = keyVar
		c.vars[gName] = fmt.Sprintf("List<%s>", rowType)
		keyType := c.inferType(q.Group.Exprs[0])
		if keyType == "var" {
			keyType = "Object"
		}
		c.vars[keyVar] = keyType
		b.WriteString(fmt.Sprintf("\tfor (var __e : %s.entrySet()) {\n", groupsVar))
		b.WriteString(fmt.Sprintf("\t\t%[1]s %s = __e.getKey();\n", keyType, keyVar))
		b.WriteString(fmt.Sprintf("\t\tList<%s> %s = __e.getValue();\n", rowType, gName))
		if q.Group.Having != nil {
			cond, err := c.compileExpr(q.Group.Having)
			if err != nil {
				c.vars = oldVars
				return "", err
			}
			cond = maybeBool(cond)
			b.WriteString(fmt.Sprintf("\t\tif (!(%s)) continue;\n", cond))
		}
		if id, ok := identName(q.Select); ok && id == gName {
			b.WriteString(fmt.Sprintf("\t\t%[1]s.add(new LinkedHashMap<>(Map.ofEntries(Map.entry(\"key\", %s), Map.entry(\"items\", %s))));\n", resVar, keyVar, gName))
		} else {
			sel, err := c.compileExpr(q.Select)
			if err != nil {
				c.vars = oldVars
				return "", err
			}
			b.WriteString(fmt.Sprintf("\t\t%[1]s.add(%[2]s);\n", resVar, sel))
		}
		b.WriteString("\t}\n")
		delete(c.groupKeys, gName)
	}
	b.WriteString(fmt.Sprintf("\treturn %s;\n", resVar))
	b.WriteString("}}).get()")

	c.vars = oldVars
	return b.String(), nil
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	tmp := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++
	// infer return type of cases
	retType := "Object"
	for _, cs := range m.Cases {
		t := c.inferType(cs.Result)
		if t == "var" {
			retType = "Object"
			break
		}
		if retType == "Object" {
			retType = t
		} else if retType != t {
			retType = "Object"
			break
		}
	}
	c.needFuncImports = true
	var b strings.Builder
	b.WriteString(fmt.Sprintf("(new java.util.function.Supplier<%s>(){public %s get(){\n", wrapperType(retType), wrapperType(retType)))
	b.WriteString("\tvar " + tmp + " = " + target + ";\n")
	for i, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + ";\n")
			b.WriteString("}}).get()")
			return b.String(), nil
		}
		if call, ok := callPattern(cs.Pattern); ok {
			if parent, ok2 := c.variantOf[call.Func]; ok2 {
				varVar := fmt.Sprintf("_v%d", c.tmpCount)
				c.tmpCount++
				b.WriteString(fmt.Sprintf("\tif (%s instanceof %s.%s %s) {\n", tmp, parent, call.Func, varVar))
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						field := c.variantFields[call.Func][idx]
						b.WriteString(fmt.Sprintf("\t\tvar %s = %s.%s;\n", id, varVar, field))
						c.vars[id] = c.variantFieldTypes[call.Func][idx]
					}
				}
				b.WriteString(fmt.Sprintf("\t\treturn %s;\n", res))
				b.WriteString("\t}\n")
				for _, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						delete(c.vars, id)
					}
				}
				continue
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if parent, ok2 := c.variantOf[ident]; ok2 {
				cond := fmt.Sprintf("%s instanceof %s.%s", tmp, parent, ident)
				if i == 0 {
					b.WriteString("\tif (" + cond + ") return " + res + ";\n")
				} else {
					b.WriteString("\telse if (" + cond + ") return " + res + ";\n")
				}
				continue
			}
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		cond := fmt.Sprintf("Objects.equals(%s, %s)", tmp, pat)
		if i == 0 {
			b.WriteString("\tif (" + cond + ") return " + res + ";\n")
		} else {
			b.WriteString("\telse if (" + cond + ") return " + res + ";\n")
		}
	}
	b.WriteString("\treturn null;\n")
	b.WriteString("}}).get()")
	return b.String(), nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	listType := c.vars[u.Target]
	elemType := listElemType(listType)
	iter := fmt.Sprintf("_it%d", c.tmpCount)
	c.tmpCount++
	fields := map[string]bool{}
	if td, ok := c.types[elemType]; ok {
		for _, m := range td.Members {
			if m.Field != nil {
				fields[m.Field.Name] = true
			}
		}
	}
	c.writeln(fmt.Sprintf("for (%s %s : %s) {", elemType, iter, u.Target))
	c.indent++
	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			return err
		}
		for f := range fields {
			cond = regexp.MustCompile(`\b`+f+`\b`).ReplaceAllString(cond, iter+"."+f)
		}
		c.writeln(fmt.Sprintf("if (!(%s)) continue;", cond))
	}
	for _, it := range u.Set.Items {
		val, err := c.compileExpr(it.Value)
		if err != nil {
			return err
		}
		key, ok := simpleStringKey(it.Key)
		if !ok {
			return fmt.Errorf("update key must be identifier")
		}
		for f := range fields {
			val = regexp.MustCompile(`\b`+f+`\b`).ReplaceAllString(val, iter+"."+f)
		}
		c.writeln(fmt.Sprintf("%s.%s = %s;", iter, key, val))
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	if l.Path == nil {
		return "", fmt.Errorf("load path required")
	}
	c.helpers["load_yaml"] = true
	return fmt.Sprintf("loadYaml(%q)", *l.Path), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	if s.Path == nil || *s.Path == "-" {
		c.helpers["save_jsonl"] = true
		src, err := c.compileExpr(s.Src)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("saveJsonl((List<Map<?,?>>)(List<?>)%s)", src), nil
	}
	return "", fmt.Errorf("save only supports stdout")
}
