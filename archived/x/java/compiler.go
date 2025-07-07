//go:build archived

package javacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Java source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	mainStmts    []*parser.Statement
	earlyStmts   []*parser.Statement
	tests        []*parser.TestBlock
	helpers      map[string]bool
	structs      map[string]bool
	returnType   types.Type
	tempVarCount int
}

// New creates a new Java compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:          env,
		helpers:      make(map[string]bool),
		structs:      make(map[string]bool),
		tests:        []*parser.TestBlock{},
		earlyStmts:   []*parser.Statement{},
		tempVarCount: 0,
	}
}

// Compile generates Java code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	if prog.Package != "" {
		c.writeln("package " + sanitizeName(prog.Package) + ";")
		c.writeln("")
	}
	c.writeln("public class Main {")
	c.indent++

	// type declarations
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// collect function and test declarations, and main statements
	for i, s := range prog.Statements {
		switch {
		case s.Fun != nil:
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		case s.Test != nil:
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.tests = append(c.tests, s.Test)
			c.writeln("")
		case (s.Let != nil || s.Var != nil) && hasLaterTest(prog, i):
			c.earlyStmts = append(c.earlyStmts, s)
		default:
			c.mainStmts = append(c.mainStmts, s)
		}
	}

	for _, s := range c.earlyStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
		c.writeln("")
	}

	if len(c.mainStmts) > 0 || len(c.tests) > 0 {
		c.writeln("public static void main(String[] args) {")
		c.indent++
		for _, t := range c.tests {
			name := "test_" + sanitizeName(t.Name)
			c.writeln(name + "();")
		}
		for _, s := range c.mainStmts {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
		c.indent--
		c.writeln("}")
	}

	if len(c.helpers) > 0 {
		c.emitRuntime()
	}

	c.indent--
	c.writeln("}")
	return FormatJava(c.buf.Bytes()), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return nil // unions not supported
	}

	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	fields := []struct{ name, typ string }{}
	if c.env != nil {
		st := types.StructType{Name: t.Name, Fields: map[string]types.Type{}, Order: []string{}}
		for _, m := range t.Members {
			if m.Field != nil {
				ft := c.resolveTypeRef(m.Field.Type)
				st.Fields[m.Field.Name] = ft
				st.Order = append(st.Order, m.Field.Name)
				fields = append(fields, struct{ name, typ string }{sanitizeName(m.Field.Name), c.javaType(ft)})
			}
		}
		c.env.SetStruct(t.Name, st)
	} else {
		for _, m := range t.Members {
			if m.Field != nil {
				fields = append(fields, struct{ name, typ string }{sanitizeName(m.Field.Name), c.javaType(c.resolveTypeRef(m.Field.Type))})
			}
		}
	}

	c.writeln(fmt.Sprintf("static class %s {", name))
	c.indent++
	for _, f := range fields {
		c.writeln(fmt.Sprintf("%s %s;", f.typ, f.name))
	}
	if len(fields) > 0 {
		params := make([]string, len(fields))
		for i, f := range fields {
			params[i] = f.typ + " " + f.name
		}
		c.writeln("")
		c.writeln(fmt.Sprintf("%s(%s) {", name, strings.Join(params, ", ")))
		c.indent++
		for _, f := range fields {
			c.writeln(fmt.Sprintf("this.%s = %s;", f.name, f.name))
		}
		c.indent--
		c.writeln("}")
		c.writeln("")
		c.writeln(fmt.Sprintf("%s() {}", name))
	}
	c.indent--
	c.writeln("}")

	// compile nested struct field types
	for _, m := range t.Members {
		if m.Field != nil {
			if st, ok := c.resolveTypeRef(m.Field.Type).(types.StructType); ok {
				c.compileStructType(st)
			}
		}
	}
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("static class %s {", name))
	c.indent++
	for _, fn := range st.Order {
		ft := st.Fields[fn]
		c.writeln(fmt.Sprintf("%s %s;", c.javaType(ft), sanitizeName(fn)))
	}
	if len(st.Order) > 0 {
		params := make([]string, len(st.Order))
		for i, fn := range st.Order {
			typ := c.javaType(st.Fields[fn])
			params[i] = typ + " " + sanitizeName(fn)
		}
		c.writeln("")
		c.writeln(fmt.Sprintf("%s(%s) {", name, strings.Join(params, ", ")))
		c.indent++
		for _, fn := range st.Order {
			n := sanitizeName(fn)
			c.writeln(fmt.Sprintf("this.%s = %s;", n, n))
		}
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func hasLaterTest(prog *parser.Program, idx int) bool {
	for _, s := range prog.Statements[idx+1:] {
		if s.Test != nil {
			return true
		}
	}
	return false
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	if c.env != nil {
		c.env.SetFunc(fun.Name, fun)
	}

	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	if ft.Params == nil {
		ft.Params = make([]types.Type, len(fun.Params))
		for i, p := range fun.Params {
			if p.Type != nil {
				ft.Params[i] = c.resolveTypeRef(p.Type)
			} else {
				ft.Params[i] = types.AnyType{}
			}
		}
	}
	if ft.Return == nil && fun.Return != nil {
		ft.Return = c.resolveTypeRef(fun.Return)
	}
	if ft.Return == nil {
		ft.Return = types.VoidType{}
	}
	if c.env != nil {
		c.env.SetVar(fun.Name, ft, false)
	}

	c.writeIndent()
	ret := c.javaType(ft.Return)
	if ret == "" {
		ret = "void"
	}
	c.buf.WriteString("static " + ret + " " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		ptype := "var"
		if i < len(ft.Params) {
			ptype = c.javaType(ft.Params[i])
		} else if p.Type != nil {
			ptype = c.javaType(c.resolveTypeRef(p.Type))
		}
		if ptype == "" {
			ptype = "var"
		}
		c.buf.WriteString(ptype + " " + sanitizeName(p.Name))
	}
	c.buf.WriteString(") {")
	c.buf.WriteByte('\n')
	c.indent++

	prevEnv := c.env
	if prevEnv != nil {
		child := types.NewEnv(prevEnv)
		for i, p := range fun.Params {
			var t types.Type = types.AnyType{}
			if i < len(ft.Params) {
				t = ft.Params[i]
			} else if p.Type != nil {
				t = c.resolveTypeRef(p.Type)
			}
			child.SetVar(p.Name, t, true)
		}
		c.env = child
	}

	prevRet := c.returnType
	c.returnType = ft.Return
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			if prevEnv != nil {
				c.env = prevEnv
			}
			c.returnType = prevRet
			return err
		}
	}
	c.returnType = prevRet
	if prevEnv != nil {
		c.env = prevEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) emitRuntime() {
	if c.helpers["_input"] {
		c.writeln("")
		c.writeln("static java.util.Scanner _scanner = new java.util.Scanner(System.in);")
		c.writeln("static String _input() {")
		c.indent++
		c.writeln("return _scanner.nextLine();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_count"] {
		c.writeln("")
		c.writeln("static int _count(Object v) {")
		c.indent++
		c.writeln("if (v instanceof _Group) return ((_Group)v).length();")
		c.writeln("java.util.List<Object> items = _toList(v);")
		c.writeln("return items.size();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_exists"] {
		c.writeln("")
		c.writeln("static boolean _exists(Object v) {")
		c.indent++
		c.writeln("if (v instanceof _Group) return ((_Group)v).length() > 0;")
		c.writeln("if (v instanceof java.util.Map<?,?>) return !((java.util.Map<?,?>)v).isEmpty();")
		c.writeln("if (v instanceof String) return !((String)v).isEmpty();")
		c.writeln("java.util.List<Object> items = _toList(v);")
		c.writeln("return !items.isEmpty();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_sum"] {
		c.writeln("")
		c.writeln("static double _sum(Object v) {")
		c.indent++
		c.writeln("java.util.List<Object> items = (v instanceof _Group) ? ((_Group)v).Items : _toList(v);")
		c.writeln("double sum = 0;")
		c.writeln("for (Object it : items) {")
		c.indent++
		c.writeln("if (it instanceof Number) sum += ((Number)it).doubleValue(); else throw new RuntimeException(\"sum() expects numbers\");")
		c.indent--
		c.writeln("}")
		c.writeln("return sum;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_avg"] {
		c.writeln("")
		c.writeln("static double _avg(Object v) {")
		c.indent++
		c.writeln("java.util.List<Object> items = (v instanceof _Group) ? ((_Group)v).Items : _toList(v);")
		c.writeln("if (items.isEmpty()) return 0;")
		c.writeln("double sum = 0;")
		c.writeln("for (Object it : items) {")
		c.indent++
		c.writeln("if (it instanceof Number) sum += ((Number)it).doubleValue(); else throw new RuntimeException(\"avg() expects numbers\");")
		c.indent--
		c.writeln("}")
		c.writeln("return sum / items.size();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_concat"] {
		c.writeln("")
		c.writeln("static int[] _concat(int[] a, int[] b) {")
		c.indent++
		c.writeln("int[] res = new int[a.length + b.length];")
		c.writeln("System.arraycopy(a, 0, res, 0, a.length);")
		c.writeln("System.arraycopy(b, 0, res, a.length, b.length);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static double[] _concat(double[] a, double[] b) {")
		c.indent++
		c.writeln("double[] res = new double[a.length + b.length];")
		c.writeln("System.arraycopy(a, 0, res, 0, a.length);")
		c.writeln("System.arraycopy(b, 0, res, a.length, b.length);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static boolean[] _concat(boolean[] a, boolean[] b) {")
		c.indent++
		c.writeln("boolean[] res = new boolean[a.length + b.length];")
		c.writeln("System.arraycopy(a, 0, res, 0, a.length);")
		c.writeln("System.arraycopy(b, 0, res, a.length, b.length);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static <T> T[] _concat(T[] a, T[] b) {")
		c.indent++
		c.writeln("T[] res = java.util.Arrays.copyOf(a, a.length + b.length);")
		c.writeln("System.arraycopy(b, 0, res, a.length, b.length);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_union"] {
		c.writeln("")
		c.writeln("static int[] _union(int[] a, int[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Integer> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (int v : a) set.add(v);")
		c.writeln("for (int v : b) set.add(v);")
		c.writeln("int[] res = new int[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (int v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static double[] _union(double[] a, double[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Double> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (double v : a) set.add(v);")
		c.writeln("for (double v : b) set.add(v);")
		c.writeln("double[] res = new double[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (double v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static boolean[] _union(boolean[] a, boolean[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Boolean> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (boolean v : a) set.add(v);")
		c.writeln("for (boolean v : b) set.add(v);")
		c.writeln("boolean[] res = new boolean[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (boolean v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static <T> T[] _union(T[] a, T[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<T> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (T v : a) set.add(v);")
		c.writeln("for (T v : b) set.add(v);")
		c.writeln("@SuppressWarnings(\"unchecked\") T[] res = (T[]) java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), set.size());")
		c.writeln("int i = 0;")
		c.writeln("for (T v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_except"] {
		c.writeln("")
		c.writeln("static int[] _except(int[] a, int[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Integer> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (int v : a) set.add(v);")
		c.writeln("for (int v : b) set.remove(v);")
		c.writeln("int[] res = new int[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (int v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static double[] _except(double[] a, double[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Double> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (double v : a) set.add(v);")
		c.writeln("for (double v : b) set.remove(v);")
		c.writeln("double[] res = new double[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (double v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static boolean[] _except(boolean[] a, boolean[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Boolean> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (boolean v : a) set.add(v);")
		c.writeln("for (boolean v : b) set.remove(v);")
		c.writeln("boolean[] res = new boolean[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (boolean v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static <T> T[] _except(T[] a, T[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<T> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (T v : a) set.add(v);")
		c.writeln("for (T v : b) set.remove(v);")
		c.writeln("@SuppressWarnings(\"unchecked\") T[] res = (T[]) java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), set.size());")
		c.writeln("int i = 0;")
		c.writeln("for (T v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_intersect"] {
		c.writeln("")
		c.writeln("static int[] _intersect(int[] a, int[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Integer> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (int v : a) if (java.util.Arrays.binarySearch(b, v) >= 0) set.add(v);")
		c.writeln("int[] res = new int[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (int v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static double[] _intersect(double[] a, double[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Double> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (double v : a) if (java.util.Arrays.binarySearch(b, v) >= 0) set.add(v);")
		c.writeln("double[] res = new double[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (double v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static boolean[] _intersect(boolean[] a, boolean[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<Boolean> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (boolean v : a) {")
		c.indent++
		c.writeln("for (boolean u : b) { if (u == v) { set.add(v); break; } }")
		c.indent--
		c.writeln("}")
		c.writeln("boolean[] res = new boolean[set.size()];")
		c.writeln("int i = 0;")
		c.writeln("for (boolean v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static <T> T[] _intersect(T[] a, T[] b) {")
		c.indent++
		c.writeln("java.util.LinkedHashSet<T> set = new java.util.LinkedHashSet<>();")
		c.writeln("for (T v : a) { for (T u : b) { if (java.util.Objects.equals(u, v)) { set.add(v); break; } } }")
		c.writeln("@SuppressWarnings(\"unchecked\") T[] res = (T[]) java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), set.size());")
		c.writeln("int i = 0;")
		c.writeln("for (T v : set) res[i++] = v;")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_in"] {
		c.writeln("")
		c.writeln("static boolean _in(Object item, Object col) {")
		c.indent++
		c.writeln("if (col instanceof String s && item instanceof String sub) return s.contains(sub);")
		c.writeln("if (col instanceof java.util.Map<?,?> m) return m.containsKey(item);")
		c.writeln("if (col != null && col.getClass().isArray()) {")
		c.indent++
		c.writeln("int n = java.lang.reflect.Array.getLength(col);")
		c.writeln("for (int i = 0; i < n; i++) {")
		c.indent++
		c.writeln("if (java.util.Objects.equals(java.lang.reflect.Array.get(col, i), item)) return true;")
		c.indent--
		c.writeln("}")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("if (col instanceof Iterable<?> it) {")
		c.indent++
		c.writeln("for (Object v : it) {")
		c.indent++
		c.writeln("if (java.util.Objects.equals(v, item)) return true;")
		c.indent--
		c.writeln("}")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_slice"] {
		c.writeln("")
		c.writeln("static int[] _slice(int[] arr, int i, int j) {")
		c.indent++
		c.writeln("if (i < 0) i += arr.length;")
		c.writeln("if (j < 0) j += arr.length;")
		c.writeln("if (i < 0) i = 0;")
		c.writeln("if (j > arr.length) j = arr.length;")
		c.writeln("if (j < i) j = i;")
		c.writeln("int[] res = new int[j - i];")
		c.writeln("System.arraycopy(arr, i, res, 0, j - i);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_slice"] {
		c.writeln("")
		c.writeln("static double[] _slice(double[] arr, int i, int j) {")
		c.indent++
		c.writeln("if (i < 0) i += arr.length;")
		c.writeln("if (j < 0) j += arr.length;")
		c.writeln("if (i < 0) i = 0;")
		c.writeln("if (j > arr.length) j = arr.length;")
		c.writeln("if (j < i) j = i;")
		c.writeln("double[] res = new double[j - i];")
		c.writeln("System.arraycopy(arr, i, res, 0, j - i);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_slice"] {
		c.writeln("")
		c.writeln("static boolean[] _slice(boolean[] arr, int i, int j) {")
		c.indent++
		c.writeln("if (i < 0) i += arr.length;")
		c.writeln("if (j < 0) j += arr.length;")
		c.writeln("if (i < 0) i = 0;")
		c.writeln("if (j > arr.length) j = arr.length;")
		c.writeln("if (j < i) j = i;")
		c.writeln("boolean[] res = new boolean[j - i];")
		c.writeln("System.arraycopy(arr, i, res, 0, j - i);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_slice"] {
		c.writeln("")
		c.writeln("static <T> T[] _slice(T[] arr, int i, int j) {")
		c.indent++
		c.writeln("if (i < 0) i += arr.length;")
		c.writeln("if (j < 0) j += arr.length;")
		c.writeln("if (i < 0) i = 0;")
		c.writeln("if (j > arr.length) j = arr.length;")
		c.writeln("if (j < i) j = i;")
		c.writeln("return java.util.Arrays.copyOfRange(arr, i, j);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_indexString"] {
		c.writeln("")
		c.writeln("static String _indexString(String s, int i) {")
		c.indent++
		c.writeln("char[] runes = s.toCharArray();")
		c.writeln("if (i < 0) i += runes.length;")
		c.writeln("if (i < 0 || i >= runes.length) throw new RuntimeException(\"index out of range\");")
		c.writeln("return String.valueOf(runes[i]);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_indexList"] {
		c.writeln("")
		c.writeln("static int _indexList(int[] arr, int i) {")
		c.indent++
		c.writeln("int idx = i;")
		c.writeln("int n = arr.length;")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 || idx >= n) throw new RuntimeException(\"index out of range\");")
		c.writeln("return arr[idx];")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static double _indexList(double[] arr, int i) {")
		c.indent++
		c.writeln("int idx = i;")
		c.writeln("int n = arr.length;")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 || idx >= n) throw new RuntimeException(\"index out of range\");")
		c.writeln("return arr[idx];")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static boolean _indexList(boolean[] arr, int i) {")
		c.indent++
		c.writeln("int idx = i;")
		c.writeln("int n = arr.length;")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 || idx >= n) throw new RuntimeException(\"index out of range\");")
		c.writeln("return arr[idx];")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static <T> T _indexList(T[] arr, int i) {")
		c.indent++
		c.writeln("int idx = i;")
		c.writeln("int n = arr.length;")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 || idx >= n) throw new RuntimeException(\"index out of range\");")
		c.writeln("return arr[idx];")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_sliceString"] {
		c.writeln("")
		c.writeln("static String _sliceString(String s, int i, int j) {")
		c.indent++
		c.writeln("int start = i;")
		c.writeln("int end = j;")
		c.writeln("int n = s.length();")
		c.writeln("if (start < 0) start += n;")
		c.writeln("if (end < 0) end += n;")
		c.writeln("if (start < 0) start = 0;")
		c.writeln("if (end > n) end = n;")
		c.writeln("if (end < start) end = start;")
		c.writeln("return s.substring(start, end);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_reverseString"] {
		c.writeln("")
		c.writeln("static String _reverseString(String s) {")
		c.indent++
		c.writeln("char[] r = s.toCharArray();")
		c.writeln("for (int i=0, j=r.length-1; i<j; i++, j--) {")
		c.indent++
		c.writeln("char tmp = r[i]; r[i] = r[j]; r[j] = tmp;")
		c.indent--
		c.writeln("}")
		c.writeln("return new String(r);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_reverseList"] {
		c.writeln("")
		c.writeln("static java.util.List<Object> _reverseList(java.util.List<Object> src) {")
		c.indent++
		c.writeln("java.util.List<Object> out = new java.util.ArrayList<>(src);")
		c.writeln("java.util.Collections.reverse(out);")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_expect"] {
		c.writeln("")
		c.writeln("static void expect(boolean cond) {")
		c.indent++
		c.writeln("if (!cond) throw new RuntimeException(\"expect failed\");")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_eval"] {
		c.writeln("")
		c.writeln("static Object _eval(String code) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("javax.script.ScriptEngine eng = new javax.script.ScriptEngineManager().getEngineByName(\"javascript\");")
		c.writeln("return eng.eval(code);")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_genText"] {
		c.writeln("")
		c.writeln("static String _genText(String prompt, String model, java.util.Map<String,Object> params) {")
		c.indent++
		c.writeln("// TODO: integrate with an LLM")
		c.writeln("return prompt;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_genEmbed"] {
		c.writeln("")
		c.writeln("static double[] _genEmbed(String text, String model, java.util.Map<String,Object> params) {")
		c.indent++
		c.writeln("double[] vec = new double[text.length()];")
		c.writeln("for (int i = 0; i < text.length(); i++) {")
		c.indent++
		c.writeln("vec[i] = text.charAt(i);")
		c.indent--
		c.writeln("}")
		c.writeln("return vec;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_genStruct"] {
		c.writeln("")
		c.writeln("static <T> T _genStruct(Class<T> cls, String prompt, String model, java.util.Map<String,Object> params) {")
		c.indent++
		c.writeln("// TODO: integrate with an LLM and parse JSON")
		c.writeln("try {")
		c.indent++
		c.writeln("return cls.getDeclaredConstructor().newInstance();")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_cast"] {
		c.writeln("")
		c.writeln("static <T> T _cast(Class<T> cls, Object v) {")
		c.indent++
		c.writeln("if (cls.isInstance(v)) return cls.cast(v);")
		c.writeln("if (cls == Integer.class) {")
		c.indent++
		c.writeln("if (v instanceof Number n) return cls.cast(n.intValue());")
		c.writeln("if (v instanceof String s) return cls.cast(Integer.parseInt(s));")
		c.writeln("return cls.cast(0);")
		c.indent--
		c.writeln("}")
		c.writeln("if (cls == Double.class) {")
		c.indent++
		c.writeln("if (v instanceof Number n) return cls.cast(n.doubleValue());")
		c.writeln("if (v instanceof String s) return cls.cast(Double.parseDouble(s));")
		c.writeln("return cls.cast(0.0);")
		c.indent--
		c.writeln("}")
		c.writeln("if (cls == Boolean.class) {")
		c.indent++
		c.writeln("if (v instanceof Boolean b) return cls.cast(b);")
		c.writeln("if (v instanceof String s) return cls.cast(Boolean.parseBoolean(s));")
		c.writeln("return cls.cast(false);")
		c.indent--
		c.writeln("}")
		c.writeln("if (v instanceof java.util.Map<?,?> m) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("T out = cls.getDeclaredConstructor().newInstance();")
		c.writeln("for (java.lang.reflect.Field f : cls.getDeclaredFields()) {")
		c.indent++
		c.writeln("Object val = m.get(f.getName());")
		c.writeln("if (val != null) {")
		c.indent++
		c.writeln("f.setAccessible(true);")
		c.writeln("Class<?> ft = f.getType();")
		c.writeln("if (ft == int.class) {")
		c.indent++
		c.writeln("if (val instanceof Number n) f.setInt(out, n.intValue()); else if (val instanceof String s) f.setInt(out, Integer.parseInt(s));")
		c.indent--
		c.writeln("} else if (ft == double.class) {")
		c.indent++
		c.writeln("if (val instanceof Number n) f.setDouble(out, n.doubleValue()); else if (val instanceof String s) f.setDouble(out, Double.parseDouble(s));")
		c.indent--
		c.writeln("} else if (ft == boolean.class) {")
		c.indent++
		c.writeln("if (val instanceof Boolean b) f.setBoolean(out, b); else if (val instanceof String s) f.setBoolean(out, Boolean.parseBoolean(s));")
		c.indent--
		c.writeln("} else { f.set(out, val); }")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return out;")
		c.indent--
		c.writeln("} catch (Exception e) { throw new RuntimeException(e); }")
		c.indent--
		c.writeln("}")
		c.writeln("try { return cls.getDeclaredConstructor().newInstance(); } catch (Exception e) { throw new RuntimeException(e); }")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_fetch"] {
		c.writeln("")
		c.writeln("static Object _fetch(String url, java.util.Map<String,Object> opts) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("java.net.URI uri = java.net.URI.create(url);")
		c.writeln("if (\"file\".equals(uri.getScheme())) {")
		c.indent++
		c.writeln("String text = java.nio.file.Files.readString(java.nio.file.Paths.get(uri));")
		c.writeln("return _parseJson(text);")
		c.indent--
		c.writeln("}")
		c.writeln("String method = \"GET\";")
		c.writeln("if (opts != null && opts.get(\"method\") != null) method = opts.get(\"method\").toString();")
		c.writeln("if (opts != null && opts.get(\"query\") != null) {")
		c.indent++
		c.writeln("@SuppressWarnings(\"unchecked\") java.util.Map<String,Object> q = (java.util.Map<String,Object>) opts.get(\"query\");")
		c.writeln("java.lang.StringBuilder qs = new java.lang.StringBuilder();")
		c.writeln("for (var e : q.entrySet()) { if (qs.length() > 0) qs.append('&'); qs.append(java.net.URLEncoder.encode(e.getKey(), java.nio.charset.StandardCharsets.UTF_8)); qs.append('='); qs.append(java.net.URLEncoder.encode(String.valueOf(e.getValue()), java.nio.charset.StandardCharsets.UTF_8)); }")
		c.writeln("String sep = url.contains(\"?\") ? \"&\" : \"?\";")
		c.writeln("url = url + sep + qs.toString();")
		c.writeln("uri = java.net.URI.create(url);")
		c.indent--
		c.writeln("}")
		c.writeln("java.net.http.HttpRequest.Builder builder = java.net.http.HttpRequest.newBuilder(uri);")
		c.writeln("if (opts != null && opts.get(\"headers\") != null) {")
		c.indent++
		c.writeln("@SuppressWarnings(\"unchecked\") java.util.Map<String,Object> hs = (java.util.Map<String,Object>) opts.get(\"headers\");")
		c.writeln("for (var e : hs.entrySet()) builder.header(e.getKey(), String.valueOf(e.getValue()));")
		c.indent--
		c.writeln("}")
		c.writeln("boolean hasBody = opts != null && opts.containsKey(\"body\");")
		c.writeln("if (hasBody) {")
		c.indent++
		c.writeln("String data = _toJson(opts.get(\"body\"));")
		c.writeln("builder.method(method, java.net.http.HttpRequest.BodyPublishers.ofString(data));")
		c.writeln("boolean hasCT = false;")
		c.writeln("if (opts != null && opts.get(\"headers\") != null) {")
		c.indent++
		c.writeln("for (var e : ((java.util.Map<String,Object>)opts.get(\"headers\")).entrySet()) if (e.getKey().equalsIgnoreCase(\"Content-Type\")) { hasCT = true; break; }")
		c.indent--
		c.writeln("}")
		c.writeln("if (!hasCT) builder.header(\"Content-Type\", \"application/json\");")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("builder.method(method, java.net.http.HttpRequest.BodyPublishers.noBody());")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts != null && opts.get(\"timeout\") != null) {")
		c.indent++
		c.writeln("double secs = Double.parseDouble(opts.get(\"timeout\").toString());")
		c.writeln("builder.timeout(java.time.Duration.ofMillis((long)(secs*1000)));")
		c.indent--
		c.writeln("}")
		c.writeln("java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();")
		c.writeln("java.net.http.HttpResponse<String> resp = client.send(builder.build(), java.net.http.HttpResponse.BodyHandlers.ofString());")
		c.writeln("return _parseJson(resp.body());")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_dataset"] {
		c.writeln("")
		c.writeln("static java.util.List<java.util.Map<String,Object>> _load(String path, java.util.Map<String,Object> opts) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("String format = opts != null && opts.get(\"format\") != null ? opts.get(\"format\").toString() : \"csv\";")
		c.writeln("boolean header = opts == null || !opts.containsKey(\"header\") ? true : Boolean.parseBoolean(opts.get(\"header\").toString());")
		c.writeln("char delim = ',';")
		c.writeln("if (opts != null && opts.get(\"delimiter\") != null) { String d = opts.get(\"delimiter\").toString(); if (!d.isEmpty()) delim = d.charAt(0); }")
		c.writeln("if (\"tsv\".equals(format)) { delim='\t'; format=\"csv\"; }")
		c.writeln("java.io.BufferedReader r;")
		c.writeln("if (path == null || path.isEmpty() || path.equals(\"-\")) {")
		c.indent++
		c.writeln("r = new java.io.BufferedReader(new java.io.InputStreamReader(System.in));")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("r = java.nio.file.Files.newBufferedReader(java.nio.file.Path.of(path));")
		c.indent--
		c.writeln("}")
		c.writeln("java.util.List<java.util.Map<String,Object>> out = new java.util.ArrayList<>();")
		c.writeln("if (\"json\".equals(format)) {")
		c.indent++
		c.writeln("StringBuilder sb = new StringBuilder();")
		c.writeln("for (String line; (line = r.readLine()) != null;) { sb.append(line); }")
		c.writeln("r.close();")
		c.writeln("Object data = _parseJson(sb.toString());")
		c.writeln("if (data instanceof java.util.Map<?,?> m) {")
		c.indent++
		c.writeln("out.add(new java.util.HashMap<>( (java.util.Map<String,Object>) m));")
		c.indent--
		c.writeln("} else if (data instanceof java.util.List<?> l) {")
		c.indent++
		c.writeln("for (Object it : l) { if (it instanceof java.util.Map<?,?>) out.add(new java.util.HashMap<>( (java.util.Map<String,Object>) it)); }")
		c.indent--
		c.writeln("}")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
		c.writeln("java.util.List<String> lines = new java.util.ArrayList<>();")
		c.writeln("for (String line; (line = r.readLine()) != null;) { if (!line.isEmpty()) lines.add(line); }")
		c.writeln("r.close();")
		c.writeln("if (lines.isEmpty()) return out;")
		c.writeln("String[] headers = lines.get(0).split(Character.toString(delim));")
		c.writeln("int start = 0;")
		c.writeln("if (header) { start = 1; } else { for (int i=0;i<headers.length;i++) headers[i]=\"c\"+i; }")
		c.writeln("for (int i=start;i<lines.size();i++) {")
		c.indent++
		c.writeln("String[] parts = lines.get(i).split(Character.toString(delim));")
		c.writeln("java.util.Map<String,Object> row = new java.util.HashMap<>();")
		c.writeln("for (int j=0;j<headers.length;j++) {")
		c.indent++
		c.writeln("String val = j < parts.length ? parts[j] : \"\";")
		c.writeln("try { row.put(headers[j], Integer.parseInt(val)); } catch(NumberFormatException _e1) {")
		c.indent++
		c.writeln("try { row.put(headers[j], Double.parseDouble(val)); } catch(NumberFormatException _e2) { row.put(headers[j], val); }")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("out.add(row);")
		c.indent--
		c.writeln("}")
		c.writeln("return out;")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static void _save(java.util.List<java.util.Map<String,Object>> rows, String path, java.util.Map<String,Object> opts) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("boolean header = opts != null && opts.get(\"header\") != null ? Boolean.parseBoolean(opts.get(\"header\").toString()) : false;")
		c.writeln("char delim = ',';")
		c.writeln("if (opts != null && opts.get(\"delimiter\") != null) { String d = opts.get(\"delimiter\").toString(); if (!d.isEmpty()) delim = d.charAt(0); }")
		c.writeln("String format = opts != null && opts.get(\"format\") != null ? opts.get(\"format\").toString() : \"csv\";")
		c.writeln("if (\"tsv\".equals(format)) { delim='\t'; format=\"csv\"; }")
		c.writeln("java.io.BufferedWriter w;")
		c.writeln("if (path == null || path.isEmpty() || path.equals(\"-\")) {")
		c.indent++
		c.writeln("w = new java.io.BufferedWriter(new java.io.OutputStreamWriter(System.out));")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("w = java.nio.file.Files.newBufferedWriter(java.nio.file.Path.of(path));")
		c.indent--
		c.writeln("}")
		c.writeln("if (\"json\".equals(format)) {")
		c.indent++
		c.writeln("w.write(_toJson(rows.size() == 1 ? rows.get(0) : rows));")
		c.writeln("w.newLine();")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("java.util.List<String> headers = rows.isEmpty() ? java.util.List.of() : new java.util.ArrayList<>(rows.get(0).keySet());")
		c.writeln("java.util.Collections.sort(headers);")
		c.writeln("if (header && !headers.isEmpty()) { w.write(String.join(Character.toString(delim), headers)); w.newLine(); }")
		c.writeln("for (java.util.Map<String,Object> row : rows) {")
		c.indent++
		c.writeln("java.util.List<String> rec = new java.util.ArrayList<>();")
		c.writeln("for (String h : headers) { Object v = row.get(h); rec.add(v==null?\"\":v.toString()); }")
		c.writeln("w.write(String.join(Character.toString(delim), rec));")
		c.writeln("w.newLine();")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("w.flush(); if (path != null && !path.isEmpty() && !path.equals(\"-\")) w.close();")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static String _toJson(Object v) {")
		c.indent++
		c.writeln("if (v == null) return \"null\";")
		c.writeln("if (v instanceof String) {")
		c.indent++
		c.writeln("String s = (String) v;")
		c.writeln(`return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";`)
		c.indent--
		c.writeln("}")
		c.writeln("if (v instanceof Number || v instanceof Boolean) return v.toString();")
		c.writeln("if (v.getClass().isArray()) {")
		c.indent++
		c.writeln("int n = java.lang.reflect.Array.getLength(v);")
		c.writeln("StringBuilder sb = new StringBuilder();")
		c.writeln("sb.append('[');")
		c.writeln("for (int i=0;i<n;i++) {")
		c.indent++
		c.writeln("if (i>0) sb.append(',');")
		c.writeln("sb.append(_toJson(java.lang.reflect.Array.get(v,i)));")
		c.indent--
		c.writeln("}")
		c.writeln("sb.append(']');")
		c.writeln("return sb.toString();")
		c.indent--
		c.writeln("}")
		c.writeln("if (v instanceof java.util.List<?>) {")
		c.indent++
		c.writeln("java.util.List<?> l=(java.util.List<?>)v;")
		c.writeln("StringBuilder sb=new StringBuilder();")
		c.writeln("sb.append('[');")
		c.writeln("for (int i=0;i<l.size();i++) {")
		c.indent++
		c.writeln("if(i>0) sb.append(',');")
		c.writeln("sb.append(_toJson(l.get(i)));")
		c.indent--
		c.writeln("}")
		c.writeln("sb.append(']');")
		c.writeln("return sb.toString();")
		c.indent--
		c.writeln("}")
		c.writeln("if (v instanceof java.util.Map<?,?>) {")
		c.indent++
		c.writeln("java.util.Map<?,?> m=(java.util.Map<?,?>)v;")
		c.writeln("StringBuilder sb=new StringBuilder();")
		c.writeln("sb.append('{');")
		c.writeln("boolean first=true;")
		c.writeln("for (var e : m.entrySet()) {")
		c.indent++
		c.writeln("if(!first) sb.append(',');")
		c.writeln("first=false;")
		c.writeln("sb.append(_toJson(String.valueOf(e.getKey())));")
		c.writeln("sb.append(':');")
		c.writeln("sb.append(_toJson(e.getValue()));")
		c.indent--
		c.writeln("}")
		c.writeln("sb.append('}');")
		c.writeln("return sb.toString();")
		c.indent--
		c.writeln("}")
		c.writeln("return _toJson(v.toString());")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static Object _parseJson(String s) {")
		c.indent++
		c.writeln("int[] i = new int[]{0};")
		c.writeln("return _parseJsonValue(s, i);")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static Object _parseJsonValue(String s, int[] i) {")
		c.indent++
		c.writeln("_skip(s, i);")
		c.writeln("char c = s.charAt(i[0]);")
		c.writeln("if (c == '{') return _parseJsonObject(s, i);")
		c.writeln("if (c == '[') return _parseJsonArray(s, i);")
		c.writeln("if (c == '\"') return _parseJsonString(s, i);")
		c.writeln("if (c == '-' || Character.isDigit(c)) return _parseJsonNumber(s, i);")
		c.writeln("if (s.startsWith(\"true\", i[0])) { i[0]+=4; return true; }")
		c.writeln("if (s.startsWith(\"false\", i[0])) { i[0]+=5; return false; }")
		c.writeln("if (s.startsWith(\"null\", i[0])) { i[0]+=4; return null; }")
		c.writeln("throw new RuntimeException(\"invalid json\");")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static void _skip(String s, int[] i) { while (i[0] < s.length() && Character.isWhitespace(s.charAt(i[0]))) i[0]++; }")

		c.writeln("")
		c.writeln("static String _parseJsonString(String s, int[] i) {")
		c.indent++
		c.writeln("StringBuilder sb = new StringBuilder();")
		c.writeln("i[0]++;")
		c.writeln("while (i[0] < s.length()) {")
		c.indent++
		c.writeln("char ch = s.charAt(i[0]++);")
		c.writeln("if (ch == '\"') break;")
		c.writeln("if (ch == '\\') {")
		c.indent++
		c.writeln("char e = s.charAt(i[0]++);")
		c.writeln("switch (e) {")
		c.indent++
		c.writeln("case '\"': sb.append('\"'); break;")
		c.writeln("case '\\': sb.append('\\'); break;")
		c.writeln("case '/': sb.append('/'); break;")
		c.writeln("case 'b': sb.append('\b'); break;")
		c.writeln("case 'f': sb.append('\f'); break;")
		c.writeln("case 'n': sb.append('\n'); break;")
		c.writeln("case 'r': sb.append('\r'); break;")
		c.writeln("case 't': sb.append('\t'); break;")
		c.writeln("case 'u': sb.append((char)Integer.parseInt(s.substring(i[0], i[0]+4), 16)); i[0]+=4; break;")
		c.writeln("default: sb.append(e); break;")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("sb.append(ch);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return sb.toString();")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static Object _parseJsonNumber(String s, int[] i) {")
		c.indent++
		c.writeln("int start = i[0];")
		c.writeln("if (s.charAt(i[0])=='-') i[0]++;")
		c.writeln("while (i[0] < s.length() && Character.isDigit(s.charAt(i[0]))) i[0]++;")
		c.writeln("boolean f = false;")
		c.writeln("if (i[0] < s.length() && s.charAt(i[0])=='.') { f = true; i[0]++; while (i[0] < s.length() && Character.isDigit(s.charAt(i[0]))) i[0]++; }")
		c.writeln("String num = s.substring(start, i[0]);")
		c.writeln("return f ? Double.parseDouble(num) : Integer.parseInt(num);")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static java.util.List<Object> _parseJsonArray(String s, int[] i) {")
		c.indent++
		c.writeln("java.util.List<Object> a = new java.util.ArrayList<>();")
		c.writeln("i[0]++; _skip(s,i); if (i[0] < s.length() && s.charAt(i[0])==']') { i[0]++; return a; }")
		c.writeln("while (true) {")
		c.indent++
		c.writeln("a.add(_parseJsonValue(s,i));")
		c.writeln("_skip(s,i);")
		c.writeln("if (i[0] < s.length() && s.charAt(i[0])==']') { i[0]++; break; }")
		c.writeln("if (i[0] < s.length() && s.charAt(i[0])==',') { i[0]++; continue; }")
		c.writeln("throw new RuntimeException(\"invalid json array\");")
		c.indent--
		c.writeln("}")
		c.writeln("return a;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static java.util.Map<String,Object> _parseJsonObject(String s, int[] i) {")
		c.indent++
		c.writeln("java.util.Map<String,Object> m = new java.util.HashMap<>();")
		c.writeln("i[0]++; _skip(s,i); if (i[0] < s.length() && s.charAt(i[0])=='}') { i[0]++; return m; }")
		c.writeln("while (true) {")
		c.indent++
		c.writeln("String k = _parseJsonString(s,i);")
		c.writeln("_skip(s,i);")
		c.writeln("if (i[0] >= s.length() || s.charAt(i[0]) != ':') throw new RuntimeException(\"expected :\");")
		c.writeln("i[0]++; Object v = _parseJsonValue(s,i); m.put(k,v); _skip(s,i);")
		c.writeln("if (i[0] < s.length() && s.charAt(i[0])=='}') { i[0]++; break; }")
		c.writeln("if (i[0] < s.length() && s.charAt(i[0])==',') { i[0]++; continue; }")
		c.writeln("throw new RuntimeException(\"invalid json object\");")
		c.indent--
		c.writeln("}")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_json"] {
		c.writeln("")
		c.writeln("static void _json(Object v) {")
		c.indent++
		c.writeln("System.out.println(_toJson(v));")
		c.indent--
		c.writeln("}")

	}
	if c.helpers["_query"] || c.helpers["_group_by"] {
		c.writeln("")
		c.writeln("static java.util.List<Object> _toList(Object v) {")
		c.indent++
		c.writeln("if (v instanceof java.util.List<?>) return new java.util.ArrayList<>((java.util.List<?>)v);")
		c.writeln("int n = java.lang.reflect.Array.getLength(v);")
		c.writeln("java.util.List<Object> out = new java.util.ArrayList<>(n);")
		c.writeln("for (int i=0;i<n;i++) out.add(java.lang.reflect.Array.get(v,i));")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_filter"] {
		c.writeln("")
		c.writeln("static java.util.List<Object> _filter(java.util.List<Object> src, java.util.function.Function<Object,Boolean> pred) {")
		c.indent++
		c.writeln("java.util.List<Object> out = new java.util.ArrayList<>();")
		c.writeln("for (Object it : src) { if (pred.apply(it)) out.add(it); }")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_query"] {
		c.writeln("")
		c.writeln("static class _JoinSpec {")
		c.indent++
		c.writeln("java.util.List<Object> items;")
		c.writeln("java.util.function.Function<Object[],Boolean> on;")
		c.writeln("boolean left;")
		c.writeln("boolean right;")
		c.writeln("_JoinSpec(java.util.List<Object> items, java.util.function.Function<Object[],Boolean> on, boolean left, boolean right) {")
		c.indent++
		c.writeln("this.items=items; this.on=on; this.left=left; this.right=right;")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static class _QueryOpts {")
		c.indent++
		c.writeln("java.util.function.Function<Object[],Object> selectFn;")
		c.writeln("java.util.function.Function<Object[],Boolean> where;")
		c.writeln("java.util.function.Function<Object[],Object> sortKey;")
		c.writeln("int skip; int take;")
		c.writeln("_QueryOpts(java.util.function.Function<Object[],Object> s, java.util.function.Function<Object[],Boolean> w, java.util.function.Function<Object[],Object> k, int skip, int take) {")
		c.indent++
		c.writeln("this.selectFn=s; this.where=w; this.sortKey=k; this.skip=skip; this.take=take;")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")

		c.writeln("static java.util.List<Object> _query(java.util.List<Object> src, java.util.List<_JoinSpec> joins, _QueryOpts opts) {")
		c.indent++
		c.writeln("java.util.List<java.util.List<Object>> items = new java.util.ArrayList<>();")
		c.writeln("for (Object v : src) { java.util.List<Object> r = new java.util.ArrayList<>(); r.add(v); items.add(r); }")
		c.writeln("for (_JoinSpec j : joins) {")
		c.indent++
		c.writeln("java.util.List<java.util.List<Object>> joined = new java.util.ArrayList<>();")
		c.writeln("java.util.List<Object> jitems = j.items;")
		c.writeln("if (j.right && j.left) {")
		c.indent++
		c.writeln("boolean[] matched = new boolean[jitems.size()];")
		c.writeln("for (java.util.List<Object> left : items) {")
		c.indent++
		c.writeln("boolean m = false;")
		c.writeln("for (int ri=0; ri<jitems.size(); ri++) {")
		c.indent++
		c.writeln("Object right = jitems.get(ri);")
		c.writeln("boolean keep = true;")
		c.writeln("if (j.on != null) {")
		c.indent++
		c.writeln("Object[] args = new Object[left.size()+1];")
		c.writeln("for (int i=0;i<left.size();i++) args[i]=left.get(i);")
		c.writeln("args[left.size()] = right;")
		c.writeln("keep = j.on.apply(args);")
		c.indent--
		c.writeln("}")
		c.writeln("if (!keep) continue;")
		c.writeln("m = true; matched[ri] = true;")
		c.writeln("java.util.List<Object> row = new java.util.ArrayList<>(left);")
		c.writeln("row.add(right); joined.add(row);")
		c.indent--
		c.writeln("}")
		c.writeln("if (!m) { java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(null); joined.add(row); }")
		c.indent--
		c.writeln("}")
		c.writeln("for (int ri=0; ri<jitems.size(); ri++) {")
		c.indent++
		c.writeln("if (!matched[ri]) { java.util.List<Object> undef = new java.util.ArrayList<>(items.isEmpty()?0:items.get(0).size()); for(int k=0;k<undef.size();k++) undef.set(k,null); undef.add(jitems.get(ri)); joined.add(undef); }")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("} else if (j.right) {")
		c.indent++
		c.writeln("for (Object right : jitems) {")
		c.indent++
		c.writeln("boolean m = false;")
		c.writeln("for (java.util.List<Object> left : items) {")
		c.indent++
		c.writeln("boolean keep = true;")
		c.writeln("if (j.on != null) {")
		c.indent++
		c.writeln("Object[] args = new Object[left.size()+1];")
		c.writeln("for (int i=0;i<left.size();i++) args[i]=left.get(i);")
		c.writeln("args[left.size()] = right;")
		c.writeln("keep = j.on.apply(args);")
		c.indent--
		c.writeln("}")
		c.writeln("if (!keep) continue;")
		c.writeln("m = true; java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(right); joined.add(row);")
		c.indent--
		c.writeln("}")
		c.writeln("if (!m) { java.util.List<Object> undef = new java.util.ArrayList<>(items.isEmpty()?0:items.get(0).size()); for(int k=0;k<undef.size();k++) undef.set(k,null); undef.add(right); joined.add(undef); }")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("for (java.util.List<Object> left : items) {")
		c.indent++
		c.writeln("boolean m = false;")
		c.writeln("for (Object right : jitems) {")
		c.indent++
		c.writeln("boolean keep = true;")
		c.writeln("if (j.on != null) {")
		c.indent++
		c.writeln("Object[] args = new Object[left.size()+1];")
		c.writeln("for (int i=0;i<left.size();i++) args[i]=left.get(i);")
		c.writeln("args[left.size()] = right;")
		c.writeln("keep = j.on.apply(args);")
		c.indent--
		c.writeln("}")
		c.writeln("if (!keep) continue;")
		c.writeln("m = true; java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(right); joined.add(row);")
		c.indent--
		c.writeln("}")
		c.writeln("if (j.left && !m) { java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(null); joined.add(row); }")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("items = joined;")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts.where != null) {")
		c.indent++
		c.writeln("java.util.List<java.util.List<Object>> filtered = new java.util.ArrayList<>();")
		c.writeln("for (java.util.List<Object> r : items) if (opts.where.apply(r.toArray(new Object[0]))) filtered.add(r);")
		c.writeln("items = filtered;")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts.sortKey != null) {")
		c.indent++
		c.writeln("class Pair { java.util.List<Object> item; Object key; Pair(java.util.List<Object> i,Object k){item=i;key=k;} }")
		c.writeln("java.util.List<Pair> pairs = new java.util.ArrayList<>();")
		c.writeln("for (java.util.List<Object> it : items) pairs.add(new Pair(it, opts.sortKey.apply(it.toArray(new Object[0]))));")
		c.writeln("pairs.sort((a,b) -> {")
		c.indent++
		c.writeln("Object ak=a.key, bk=b.key;")
		c.writeln("if (ak instanceof Number && bk instanceof Number) return Double.compare(((Number)ak).doubleValue(), ((Number)bk).doubleValue());")
		c.writeln("if (ak instanceof String && bk instanceof String) return ((String)ak).compareTo((String)bk);")
		c.writeln("return ak.toString().compareTo(bk.toString());")
		c.indent--
		c.writeln("});")
		c.writeln("for (int i=0;i<pairs.size();i++) items.set(i, pairs.get(i).item);")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts.skip >= 0) { if (opts.skip < items.size()) items = new java.util.ArrayList<>(items.subList(opts.skip, items.size())); else items = new java.util.ArrayList<>(); }")
		c.writeln("if (opts.take >= 0) { if (opts.take < items.size()) items = new java.util.ArrayList<>(items.subList(0, opts.take)); }")
		c.writeln("java.util.List<Object> res = new java.util.ArrayList<>();")
		c.writeln("for (java.util.List<Object> r : items) res.add(opts.selectFn.apply(r.toArray(new Object[0])));")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_group_by"] {
		c.writeln("")
		c.writeln("static class _Group {")
		c.indent++
		c.writeln("Object key;")
		c.writeln("java.util.List<Object> Items = new java.util.ArrayList<>();")
		c.writeln("_Group(Object k) { key = k; }")
		c.writeln("int length() { return Items.size(); }")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static java.util.List<_Group> _group_by(java.util.List<Object> src, java.util.function.Function<Object,Object> keyfn) {")
		c.indent++
		c.writeln("java.util.Map<String,_Group> groups = new java.util.LinkedHashMap<>();")
		c.writeln("for (Object it : src) {")
		c.indent++
		c.writeln("Object key = keyfn.apply(it);")
		c.writeln("String ks = String.valueOf(key);")
		c.writeln("_Group g = groups.get(ks);")
		c.writeln("if (g == null) { g = new _Group(key); groups.put(ks, g); }")
		c.writeln("g.Items.add(it);")
		c.indent--
		c.writeln("}")
		c.writeln("return new java.util.ArrayList<>(groups.values());")
		c.indent--
		c.writeln("}")
	}
}
