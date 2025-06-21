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
	tests        []*parser.TestBlock
	helpers      map[string]bool
	returnType   types.Type
	tempVarCount int
}

// New creates a new Java compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), tests: []*parser.TestBlock{}, tempVarCount: 0}
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
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.tests = append(c.tests, s.Test)
			c.writeln("")
			continue
		}
		c.mainStmts = append(c.mainStmts, s)
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
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return nil // unions not supported
	}

	name := sanitizeName(t.Name)
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
	}
	c.indent--
	c.writeln("}")
	return nil
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
		c.writeln("static int _count(int[] arr) {")
		c.indent++
		c.writeln("return arr.length;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_avg"] {
		c.writeln("")
		c.writeln("static int _avg(int[] arr) {")
		c.indent++
		c.writeln("if (arr.length == 0) return 0;")
		c.writeln("int sum = 0;")
		c.writeln("for (int v : arr) {")
		c.indent++
		c.writeln("sum += v;")
		c.indent--
		c.writeln("}")
		c.writeln("return sum / arr.length;")
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
	if c.helpers["_fetch"] {
		c.writeln("")
		c.writeln("static java.util.Map<String,Object> _fetch(String url, java.util.Map<String,Object> opts) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();")
		c.writeln("java.net.http.HttpRequest req = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url)).build();")
		c.writeln("java.net.http.HttpResponse<String> resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString());")
		c.writeln("java.util.Map<String,Object> out = new java.util.HashMap<>();")
		c.writeln("out.put(\"status\", resp.statusCode());")
		c.writeln("out.put(\"body\", resp.body());")
		c.writeln("return out;")
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
		c.writeln("java.util.List<String> lines = new java.util.ArrayList<>();")
		c.writeln("for (String line; (line = r.readLine()) != null;) { if (!line.isEmpty()) lines.add(line); }")
		c.writeln("r.close();")
		c.writeln("java.util.List<java.util.Map<String,Object>> out = new java.util.ArrayList<>();")
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
		c.writeln("w.flush(); if (path != null && !path.isEmpty() && !path.equals(\"-\")) w.close();")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
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
	}
}
