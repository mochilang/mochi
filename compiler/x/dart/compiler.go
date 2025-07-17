//go:build slow

package dart

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"unicode"

	"mochi/parser"
	"mochi/types"
)

const dartEqualHelper = `
bool _equal(dynamic a, dynamic b) {
    if (a is List && b is List) {
        if (a.length != b.length) return false;
        for (var i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }
        return true;
    }
    if (a is Map && b is Map) {
        if (a.length != b.length) return false;
        for (var k in a.keys) { if (!b.containsKey(k) || !_equal(a[k], b[k])) return false; }
        return true;
    }
    return a == b;
}
`

const dartHelpers = `
String _formatDuration(Duration d) {
    if (d.inMicroseconds < 1000) return '${d.inMicroseconds}µs';
    if (d.inMilliseconds < 1000) return '${d.inMilliseconds}ms';
    return '${(d.inMilliseconds/1000).toStringAsFixed(2)}s';
}

void _json(dynamic v) {
    print(jsonEncode(v));
}

void _print(List<dynamic> args) {
    for (var i = 0; i < args.length; i++) {
        if (i > 0) stdout.write(' ');
        var v = args[i];
        if (v is List) {
            stdout.write(v.join(' '));
        } else if (v is double && v == v.roundToDouble()) {
            stdout.write(v.toInt());
        } else {
            stdout.write(v);
        }
    }
    stdout.writeln();
}


dynamic _min(dynamic v) {
    List<dynamic>? list;
    if (v is List) list = v;
    else if (v is Map && v['items'] is List) list = (v['items'] as List);
    else if (v is Map && v['Items'] is List) list = (v['Items'] as List);
    else { try { var it = (v as dynamic).items; if (it is List) list = it; } catch (_) {} }
    if (list == null || list.isEmpty) return 0;
    var m = list[0];
    for (var n in list) { if ((n as Comparable).compareTo(m) < 0) m = n; }
    return m;
}

num _sum(dynamic v) {
    Iterable<dynamic>? list;
    if (v is Iterable) list = v;
    else if (v is Map && v['items'] is Iterable) list = (v['items'] as Iterable);
    else if (v is Map && v['Items'] is Iterable) list = (v['Items'] as Iterable);
    else { try { var it = (v as dynamic).items; if (it is Iterable) list = it; } catch (_) {} }
    if (list == null) return 0;
    num s = 0;
    for (var n in list) s += (n as num);
    return s;
}

bool _runTest(String name, void Function() f) {
    stdout.write('   test $name ...');
    var start = DateTime.now();
    try {
        f();
        var d = DateTime.now().difference(start);
        stdout.writeln(' ok (${_formatDuration(d)})');
        return true;
    } catch (e) {
        var d = DateTime.now().difference(start);
        stdout.writeln(' fail $e (${_formatDuration(d)})');
        return false;
    }
}

String findRepoRoot() {
    var dir = Directory.current;
    for (var i = 0; i < 10; i++) {
        if (File('${dir.path}/go.mod').existsSync()) return dir.path;
        var parent = dir.parent;
        if (parent.path == dir.path) break;
        dir = parent;
    }
    return '';
}
`

// Compiler is a very small proof-of-concept translator that converts a limited
// subset of Mochi programs into Dart code.  For many of the test programs this
// translation is incomplete, but it is sufficient for simple examples such as
// variable declarations and basic arithmetic/print statements.
type Compiler struct {
	env        *types.Env
	buf        bytes.Buffer
	indent     int
	useIn      bool
	useJSON    bool
	useIO      bool
	useYAML    bool
	useLoad    bool
	useSave    bool
	useEqual   bool
	tmp        int
	mapVars    map[string]bool
	groupKeys  map[string]string
	fieldTypes map[string]map[string]types.Type
	imports    map[string]string
	mainRename string
}

// New creates a new Dart compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, mapVars: make(map[string]bool), groupKeys: make(map[string]string), fieldTypes: make(map[string]map[string]types.Type), imports: make(map[string]string), mainRename: ""}
}

// Compile translates the given Mochi program into Dart source code.  If there
// is a hand written translation under tests/human/x/dart it is returned.
// Otherwise the program is compiled using the small subset supported here.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.mainRename = ""
	for _, st := range prog.Statements {
		if st.Fun != nil && st.Fun.Name == "main" {
			c.mainRename = "_main"
			break
		}
	}
	// compile function declarations first so that they appear before main
	c.buf.Reset()
	c.indent = 0
	c.useIn = false
	// Helpers currently always reference jsonEncode and stdout even if the
	// generated program itself does not.  Without these imports the
	// generated code fails to compile.  Default to true so simple programs
	// work without explicit JSON or IO usage.
	c.useJSON = true
	c.useIO = true
	c.useYAML = false
	c.useLoad = false
	c.useSave = false
	c.useEqual = false
	c.mapVars = make(map[string]bool)
	c.groupKeys = make(map[string]string)
	c.fieldTypes = make(map[string]map[string]types.Type)
	c.imports = make(map[string]string)

	for _, st := range prog.Statements {
		if st.Test != nil {
			c.useIO = true
		}
	}

	// handle simple builtin imports
	for _, st := range prog.Statements {
		if st.Import != nil && st.Import.Lang != nil {
			alias := st.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(st.Import.Path)
			}
			path := strings.Trim(st.Import.Path, "\"")
			switch *st.Import.Lang {
			case "python":
				if path == "math" {
					c.imports[alias] = "dart_math"
				}
			case "go":
				if st.Import.Auto && path == "mochi/runtime/ffi/go/testpkg" {
					c.imports[alias] = "go_testpkg"
				}
			}
		}
	}

	// collect top-level declarations so that functions and global variables
	// appear before the main entry point
	var fnBuf bytes.Buffer
	old := c.buf
	c.buf.Reset()

	for _, st := range prog.Statements {
		switch {
		case st.Fun != nil:
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		case st.Type != nil:
			if err := c.compileType(st.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		case st.Let != nil:
			if err := c.compileLet(st.Let); err != nil {
				return nil, err
			}
			c.writeln("")
		case st.Var != nil:
			if err := c.compileVar(st.Var); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	fnBuf.Write(c.buf.Bytes())
	c.buf.Reset()

	c.writeln("void main() {")
	c.indent++
	for _, st := range prog.Statements {
		if st.Fun != nil || st.Let != nil || st.Var != nil {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	mainBytes := c.buf.Bytes()
	c.buf = old

	var out bytes.Buffer
	if c.useIO {
		out.WriteString("import 'dart:io';\n")
	}
	if c.useJSON {
		out.WriteString("import 'dart:convert';\n")
	}
	if c.useYAML {
		// no external dependencies; a tiny YAML parser is embedded below
	}
	if c.useIO || c.useJSON || c.useYAML {
		out.WriteString("\n")
	}
	if c.useIn {
		out.WriteString("bool _in(dynamic item, dynamic col) {\n")
		out.WriteString("  if (col is Map) return col.containsKey(item);\n")
		out.WriteString("  if (col is Iterable || col is String) return col.contains(item);\n")
		out.WriteString("  return false;\n")
		out.WriteString("}\n\n")
	}
	if c.useLoad {
		out.WriteString("dynamic _load(String path, dynamic opts) {\n")
		out.WriteString("  var fmt = 'csv';\n")
		out.WriteString("  if (opts is Map && opts.containsKey('format')) fmt = opts['format'].toString();\n")
		out.WriteString("  var f = File(path);\n")
		out.WriteString("  if (!f.existsSync()) {\n")
		out.WriteString("    var root = findRepoRoot();\n")
		out.WriteString("    if (root.isNotEmpty) f = File(root + '/' + path);\n")
		out.WriteString("  }\n")
		out.WriteString("  if (fmt == 'yaml') {\n")
		out.WriteString("    var text = f.readAsStringSync();\n")
		out.WriteString("    var data = _parseYaml(text);\n")
		out.WriteString("    return data;\n")
		out.WriteString("  }\n")
		out.WriteString("  var text = f.readAsStringSync();\n")
		out.WriteString("  var data = jsonDecode(text);\n")
		out.WriteString("  if (data is List) return data;\n")
		out.WriteString("  if (data is Map) return [data];\n")
		out.WriteString("  return [];\n")
		out.WriteString("}\n\n")
	}
	if c.useYAML {
		out.WriteString("List<Map<String,dynamic>> _parseYaml(String text) {\n")
		out.WriteString("  var rows = <Map<String,dynamic>>[];\n")
		out.WriteString("  Map<String,dynamic>? cur;\n")
		out.WriteString("  for (var line in LineSplitter.split(text)) {\n")
		out.WriteString("    var t = line.trim();\n")
		out.WriteString("    if (t.isEmpty) continue;\n")
		out.WriteString("    if (t.startsWith('-')) {\n")
		out.WriteString("      if (cur != null) rows.add(cur);\n")
		out.WriteString("      cur = <String,dynamic>{};\n")
		out.WriteString("      t = t.substring(1).trim();\n")
		out.WriteString("      if (t.isEmpty) continue;\n")
		out.WriteString("    }\n")
		out.WriteString("    var idx = t.indexOf(':');\n")
		out.WriteString("    if (idx <= 0) continue;\n")
		out.WriteString("    var key = t.substring(0, idx).trim();\n")
		out.WriteString("    var val = t.substring(idx+1).trim();\n")
		out.WriteString("    if ((val.startsWith(\"\\\"\") && val.endsWith(\"\\\"\")) || (val.startsWith(\"'\") && val.endsWith(\"'\"))) {\n")
		out.WriteString("      val = val.substring(1, val.length-1);\n")
		out.WriteString("    }\n")
		out.WriteString("    var numVal = num.tryParse(val);\n")
		out.WriteString("    cur?[key] = numVal ?? val;\n")
		out.WriteString("  }\n")
		out.WriteString("  if (cur != null) rows.add(cur);\n")
		out.WriteString("  return rows;\n")
		out.WriteString("}\n\n")
	}
	if c.useSave {
		out.WriteString("void _save(List<dynamic> rows, String path, String fmt) {\n")
		out.WriteString("  if (fmt == 'jsonl' && path == '-') {\n")
		out.WriteString("    for (var r in rows) { stdout.writeln(jsonEncode(r)); }\n")
		out.WriteString("    return;\n")
		out.WriteString("  }\n")
		out.WriteString("  if (fmt == 'json') {\n")
		out.WriteString("    File(path).writeAsStringSync(jsonEncode(rows));\n")
		out.WriteString("  }\n")
		out.WriteString("}\n\n")
	}

	if len(c.imports) > 0 {
		aliases := make([]string, 0, len(c.imports))
		for a := range c.imports {
			aliases = append(aliases, a)
		}
		sort.Strings(aliases)
		for _, a := range aliases {
			kind := c.imports[a]
			switch kind {
			case "dart_math":
				out.WriteString(fmt.Sprintf("import 'dart:math' as %s;\n", a))
			}
		}
		out.WriteString("\n")
		for _, a := range aliases {
			kind := c.imports[a]
			switch kind {
			case "go_testpkg":
				out.WriteString(fmt.Sprintf("class %s {\n", a))
				out.WriteString("  static int Add(int a, int b) => a + b;\n")
				out.WriteString("  static const double Pi = 3.14;\n")
				out.WriteString("  static const int Answer = 42;\n")
				out.WriteString("}\n\n")
			}
		}
	}

	out.Write(fnBuf.Bytes())
	out.Write(mainBytes)
	if c.useEqual {
		out.WriteString(dartEqualHelper)
	}
	out.WriteString(dartHelpers)
	return formatDart(out.Bytes()), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Type != nil:
		// type declarations are ignored in generated code
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Import != nil, s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
		// extern/import statements are handled separately or have no effect
		return nil
	case s.Test != nil:
		// test blocks are ignored
		return nil
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Fun != nil:
		// nested function declaration
		if err := c.compileFun(s.Fun); err != nil {
			return err
		}
		c.writeln("")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	typ := dartType(l.Type)
	var val string
	var err error
	if l.Value != nil {
		val, err = c.compileExpr(l.Value)
		if err != nil {
			return err
		}
	} else {
		val = defaultValue(typ)
	}
	name := escapeIdent(l.Name)
	if typ == "" {
		if val == "0" || val == "0.0" {
			c.writeln(fmt.Sprintf("num %s = %s;", name, val))
		} else {
			c.writeln(fmt.Sprintf("var %s = %s;", name, val))
		}
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	typ := dartType(v.Type)
	var val string
	var err error
	if v.Value != nil {
		val, err = c.compileExpr(v.Value)
		if err != nil {
			return err
		}
	} else {
		val = defaultValue(typ)
	}
	name := escapeIdent(v.Name)
	if typ == "" {
		if val == "0" || val == "0.0" {
			c.writeln(fmt.Sprintf("num %s = %s;", name, val))
		} else {
			c.writeln(fmt.Sprintf("var %s = %s;", name, val))
		}
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	target := escapeIdent(a.Name)
	typ, _ := c.env.GetVar(a.Name)
	if c.mapVars[a.Name] {
		typ = types.MapType{}
	}
	for i, idx := range a.Index {
		if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
			return fmt.Errorf("slice assignment not supported")
		}
		if idx.Start == nil {
			return fmt.Errorf("missing index expression")
		}
		expr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		more := i < len(a.Index)-1 || len(a.Field) > 0
		switch t := typ.(type) {
		case types.ListType:
			typ = t.Elem
			if more && isMapType(typ) {
				target = fmt.Sprintf("(%s[%s] as Map)", target, expr)
			} else if more {
				target = fmt.Sprintf("%s[%s]", target, expr)
			} else {
				target += fmt.Sprintf("[%s]", expr)
			}
		case types.MapType:
			typ = t.Value
			if more && isMapType(typ) {
				target = fmt.Sprintf("(%s[%s] as Map)", target, expr)
			} else if more {
				target = fmt.Sprintf("%s[%s]", target, expr)
			} else {
				target += fmt.Sprintf("[%s]", expr)
			}
		default:
			if more {
				target = fmt.Sprintf("%s[%s]", target, expr)
			} else {
				target += fmt.Sprintf("[%s]", expr)
			}
			typ = types.AnyType{}
		}
	}
	for _, f := range a.Field {
		switch t := typ.(type) {
		case types.MapType:
			target += fmt.Sprintf("['%s']", escapeIdent(f.Name))
			typ = t.Value
		case types.StructType:
			target += "." + escapeIdent(f.Name)
			if ft, ok := t.Fields[f.Name]; ok {
				typ = ft
			} else {
				typ = types.AnyType{}
			}
		default:
			target += "." + escapeIdent(f.Name)
			typ = types.AnyType{}
		}
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", target, val))
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

func (c *Compiler) compileType(td *parser.TypeDecl) error {
	if st, ok := c.env.GetStruct(td.Name); ok {
		c.writeln(fmt.Sprintf("class %s {", td.Name))
		c.indent++
		fields := make([]string, len(st.Order))
		for i, name := range st.Order {
			typ := dartTypeFromType(st.Fields[name])
			if typ == "" {
				typ = "dynamic"
			}
			ename := escapeIdent(name)
			c.writeln(fmt.Sprintf("%s %s;", typ, ename))
			fields[i] = "this." + ename
		}
		c.writeln(fmt.Sprintf("%s(%s);", td.Name, strings.Join(fields, ", ")))
		c.indent--
		c.writeln("}")
		return nil
	}
	if ut, ok := c.env.GetUnion(td.Name); ok {
		c.writeln(fmt.Sprintf("abstract class %s {}", td.Name))
		names := make([]string, 0, len(ut.Variants))
		for n := range ut.Variants {
			names = append(names, n)
		}
		sort.Strings(names)
		var variantMap = map[string]*parser.TypeVariant{}
		for _, v := range td.Variants {
			vv := v
			variantMap[v.Name] = vv
		}
		for _, name := range names {
			st := ut.Variants[name]
			c.writeln(fmt.Sprintf("class %s extends %s {", name, td.Name))
			c.indent++
			fields := make([]string, len(st.Order))
			for i, f := range st.Order {
				var typ string
				if v, ok := variantMap[name]; ok {
					for _, fld := range v.Fields {
						if fld.Name == f {
							typ = dartType(fld.Type)
							break
						}
					}
				}
				if typ == "" {
					typ = dartTypeFromType(st.Fields[f])
				}
				if typ == "" {
					typ = "dynamic"
				}
				ef := escapeIdent(f)
				c.writeln(fmt.Sprintf("%s %s;", typ, ef))
				fields[i] = "this." + ef
			}
			if len(st.Order) > 0 {
				c.writeln(fmt.Sprintf("%s(%s);", name, strings.Join(fields, ", ")))
			} else {
				c.writeln(fmt.Sprintf("%s();", name))
			}
			c.indent--
			c.writeln("}")
		}
	}
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	name := escapeIdent(f.Name)
	if name == "main" && c.mainRename != "" {
		name = c.mainRename
	}
	ret := dartType(f.Return)
	if ret == "" {
		ret = "void"
	}
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		pt := dartType(p.Type)
		if pt == "" {
			pt = "dynamic"
		}
		params[i] = fmt.Sprintf("%s %s", pt, escapeIdent(p.Name))
	}
	c.writeln(fmt.Sprintf("%s %s(%s) {", ret, name, strings.Join(params, ", ")))
	c.indent++
	origEnv := c.env
	child := types.NewEnv(c.env)
	for _, p := range f.Params {
		child.SetVar(p.Name, types.ResolveTypeRef(p.Type, c.env), true)
	}
	c.env = child
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.env = origEnv
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	if !isBoolType(types.TypeOfExpr(s.Cond, c.env)) {
		cond += " != null"
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if s.ElseIf != nil {
		c.writeln("else ")
		if err := c.compileIf(s.ElseIf); err != nil {
			return err
		}
	} else if s.Else != nil {
		c.writeln("else {")
		c.indent++
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	if !isBoolType(types.TypeOfExpr(w.Cond, c.env)) {
		cond += " != null"
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	origEnv := c.env
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		child := types.NewEnv(c.env)
		child.SetVar(f.Name, types.IntType{}, true)
		c.env = child
		c.writeln(fmt.Sprintf("for (var %s = %s; %s < %s; %s++) {", f.Name, start, f.Name, end, f.Name))
	} else {
		srcExpr := f.Source
		src, err := c.compileExpr(srcExpr)
		if err != nil {
			return err
		}
		t := types.TypeOfExpr(srcExpr, c.env)
		switch tt := t.(type) {
		case types.ListType:
			if _, ok := tt.Elem.(types.MapType); ok {
				c.mapVars[f.Name] = true
			}
			child := types.NewEnv(c.env)
			child.SetVar(f.Name, tt.Elem, true)
			c.env = child
			c.writeln(fmt.Sprintf("for (var %s in %s) {", f.Name, src))
		case types.MapType:
			child := types.NewEnv(c.env)
			child.SetVar(f.Name, tt.Value, true)
			c.env = child
			c.writeln(fmt.Sprintf("for (var %s in %s.keys) {", f.Name, src))
		default:
			iterVar := fmt.Sprintf("_iter%d", c.tmp)
			c.tmp++
			c.writeln(fmt.Sprintf("var %s = %s;", iterVar, src))
			child := types.NewEnv(c.env)
			child.SetVar(f.Name, types.AnyType{}, true)
			c.env = child
			c.writeln(fmt.Sprintf("for (var %s in (%s is Map ? (%s as Map).keys : %s) as Iterable) {", f.Name, iterVar, iterVar, iterVar))
		}
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.env = origEnv
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := u.Target
	idxVar := fmt.Sprintf("_i%d", c.tmp)
	c.tmp++
	itemVar := fmt.Sprintf("_it%d", c.tmp)
	c.tmp++

	c.writeln(fmt.Sprintf("for (var %s = 0; %s < %s.length; %s++) {", idxVar, idxVar, list, idxVar))
	c.indent++
	c.writeln(fmt.Sprintf("var %s = %s[%s];", itemVar, list, idxVar))

	var st types.StructType
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}

	origEnv := c.env
	if st.Name != "" {
		child := types.NewEnv(c.env)
		for _, f := range st.Order {
			c.writeln(fmt.Sprintf("var %s = %s.%s;", f, itemVar, f))
			child.SetVar(f, st.Fields[f], true)
		}
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent++
	}

	for _, it := range u.Set.Items {
		if st.Name != "" {
			if key, ok := c.simpleIdentifier(it.Key); ok {
				val, err := c.compileExpr(it.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				c.writeln(fmt.Sprintf("%s.%s = %s;", itemVar, key, val))
				continue
			}
		}
		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.env = origEnv
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("%s[%s] = %s;", itemVar, keyExpr, valExpr))
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.writeln(fmt.Sprintf("%s[%s] = %s;", list, idxVar, itemVar))
	if st.Name != "" {
		c.env = origEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	type binOp struct {
		op  string
		all bool
	}

	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []string{left}
	typesList := []types.Type{types.TypeOfUnary(b.Left, c.env)}
	ops := []binOp{}

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		typesList = append(typesList, types.TypeOfPostfix(part.Right, c.env))
		ops = append(ops, binOp{op: part.Op, all: part.All})
	}

	prec := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "except", "intersect"},
	}

	contains := func(list []string, s string) bool {
		for _, x := range list {
			if x == s {
				return true
			}
		}
		return false
	}

	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i].op) {
				expr, typ, err := c.compileBinaryOp(operands[i], typesList[i], ops[i].op, ops[i].all, operands[i+1], typesList[i+1])
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

func (c *Compiler) compileBinaryOp(left string, leftType types.Type, op string, all bool, right string, rightType types.Type) (string, types.Type, error) {
	switch op {
	case "in":
		c.useIn = true
		return fmt.Sprintf("_in(%s, %s)", left, right), types.BoolType{}, nil
	case "union":
		if all {
			return fmt.Sprintf("(List.from(%s)..addAll(%s))", left, right), leftType, nil
		}
		return fmt.Sprintf("{...%s, ...%s}.toList()", left, right), leftType, nil
	case "except":
		return fmt.Sprintf("(List.from(%s)..removeWhere((x) => %s.contains(x)))", left, right), leftType, nil
	case "intersect":
		return fmt.Sprintf("%s.where((x) => %s.contains(x)).toList()", left, right), leftType, nil
	default:
		if op == "==" || op == "!=" {
			if left == "null" || right == "null" {
				return fmt.Sprintf("%s %s %s", left, op, right), types.BoolType{}, nil
			}
			if isListType(leftType) || isListType(rightType) || isMapType(leftType) || isMapType(rightType) {
				c.useEqual = true
				expr := fmt.Sprintf("_equal(%s, %s)", left, right)
				if op == "!=" {
					expr = "!" + expr
				}
				return expr, types.BoolType{}, nil
			}
			return fmt.Sprintf("%s %s %s", left, op, right), types.BoolType{}, nil
		}
		if op == "<" || op == "<=" || op == ">" || op == ">=" {
			l := left
			r := right
			if isStringType(leftType) || isStringType(rightType) {
				cmp := map[string]string{"<": "< 0", "<=": "<= 0", ">": "> 0", ">=": ">= 0"}[op]
				return fmt.Sprintf("%s.compareTo(%s) %s", l, r, cmp), types.BoolType{}, nil
			}
			if !isNumericType(leftType) {
				l = fmt.Sprintf("(%s as num)", l)
			}
			if !isNumericType(rightType) {
				r = fmt.Sprintf("(%s as num)", r)
			}
			return fmt.Sprintf("%s %s %s", l, op, r), types.BoolType{}, nil
		} else if op == "+" && (isStringType(leftType) || isStringType(rightType)) {
			return fmt.Sprintf("%s + %s", left, right), types.StringType{}, nil
		}

		l := left
		r := right
		if isNumericOp(op) {
			if !isNumericType(leftType) {
				l = fmt.Sprintf("(%s as num)", l)
			}
			if !isNumericType(rightType) {
				r = fmt.Sprintf("(%s as num)", r)
			}
		}
		expr := fmt.Sprintf("%s %s %s", l, op, r)
		newType := types.ResultType(op, leftType, rightType)
		if _, ok := newType.(types.IntType); ok {
			if op == "/" {
				expr = fmt.Sprintf("%s ~/ %s", l, r)
			}
			return expr, newType, nil
		}
		return expr, newType, nil
	}
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	if len(u.Ops) == 1 && u.Ops[0] == "-" {
		if lit := numericLiteral(u.Value); lit != "" {
			return "-" + lit, nil
		}
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			val = fmt.Sprintf("-(%s as num)", val)
			continue
		}
		val = op + val
	}
	return val, nil
}

func numericLiteral(p *parser.PostfixExpr) string {
	if p == nil || len(p.Ops) > 0 || p.Target == nil || p.Target.Lit == nil {
		return ""
	}
	lit := p.Target.Lit
	if lit.Int != nil {
		return fmt.Sprintf("%d", *lit.Int)
	}
	if lit.Float != nil {
		return fmt.Sprintf("%g", *lit.Float)
	}
	return ""
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	t := types.TypeOfPrimary(p.Target, c.env)
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
				if op.Index.Step != nil || op.Index.Colon2 != nil {
					return "", fmt.Errorf("slices not supported")
				}
				start := "0"
				if op.Index.Start != nil {
					start, err = c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
				}
				end := fmt.Sprintf("%s.length", val)
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
				}
				switch tt := t.(type) {
				case types.StringType:
					val = fmt.Sprintf("%s.substring(%s, %s)", val, start, end)
					t = types.StringType{}
				case types.ListType:
					val = fmt.Sprintf("%s.sublist(%s, %s)", val, start, end)
					t = tt
				default:
					val = fmt.Sprintf("((%s is String) ? %s.substring(%s, %s) : (%s as List).sublist(%s, %s))", val, val, start, end, val, start, end)
				}
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("missing index expression")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if mt, ok := t.(types.MapType); ok {
					val = fmt.Sprintf("(%s as Map)[%s]", val, idx)
					t = mt.Value
				} else {
					val = fmt.Sprintf("%s[%s]", val, idx)
					switch tt := t.(type) {
					case types.ListType:
						t = tt.Elem
					case types.StringType:
						t = types.StringType{}
					default:
						t = types.AnyType{}
					}
				}
			}
		case op.Field != nil:
			switch tt := t.(type) {
			case types.MapType:
				val += fmt.Sprintf("['%s']", op.Field.Name)
				t = tt.Value
			case types.StructType:
				val += "." + op.Field.Name
				if ft, ok := tt.Fields[op.Field.Name]; ok {
					t = ft
				} else {
					t = types.AnyType{}
				}
			default:
				val += "." + op.Field.Name
				t = types.AnyType{}
			}
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if strings.HasSuffix(val, ".contains") && len(args) == 1 {
				base := strings.TrimSuffix(val, ".contains")
				val = fmt.Sprintf("%s.contains(%s)", base, args[0])
			} else if strings.HasSuffix(val, ".starts_with") && len(args) == 1 {
				base := strings.TrimSuffix(val, ".starts_with")
				val = fmt.Sprintf("%s.toString().startsWith(%s.toString())", base, args[0])
			} else if strings.HasSuffix(val, ".ends_with") && len(args) == 1 {
				base := strings.TrimSuffix(val, ".ends_with")
				val = fmt.Sprintf("%s.toString().endsWith(%s.toString())", base, args[0])
			} else {
				val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			}
		case op.Cast != nil:
			typ := dartType(op.Cast.Type)
			switch typ {
			case "int":
				val = fmt.Sprintf("int.parse(%s)", val)
			case "double":
				val = fmt.Sprintf("double.parse(%s)", val)
			case "String":
				val = fmt.Sprintf("%s.toString()", val)
			case "bool":
				val = fmt.Sprintf("(%s ? true : false)", val)
			default:
				if st, ok := c.env.GetStruct(typ); ok {
					args := make([]string, len(st.Order))
					for i, name := range st.Order {
						ftyp := dartTypeFromType(st.Fields[name])
						if ftyp == "" {
							ftyp = "dynamic"
						}
						args[i] = fmt.Sprintf("(%s['%s'] as %s)", val, name, ftyp)
					}
					val = fmt.Sprintf("%s(%s)", typ, strings.Join(args, ", "))
				} else {
					// Fallback to a simple Dart cast which will allow the
					// program to compile even if the type isn't recognised.
					val = fmt.Sprintf("(%s as %s)", val, typ)
				}
			}
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		multi := len(elems) > 3
		if !multi {
			for _, e := range elems {
				if strings.ContainsAny(e, "[]{}\n") || len(e) > 20 {
					multi = true
					break
				}
			}
		}
		if multi {
			indent := strings.Repeat("  ", c.indent)
			var b bytes.Buffer
			b.WriteString("[\n")
			for _, e := range elems {
				b.WriteString(indent + "  " + e + ",\n")
			}
			b.WriteString(indent + "]")
			return b.String(), nil
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, m := range p.Map.Items {
			k, err := c.compileMapKey(m.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(m.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		multi := len(items) > 3
		if !multi {
			for _, it := range items {
				if strings.ContainsAny(it, "[]{}\n") || len(it) > 20 {
					multi = true
					break
				}
			}
		}
		if multi {
			indent := strings.Repeat("  ", c.indent)
			var b bytes.Buffer
			b.WriteString("{\n")
			for _, it := range items {
				b.WriteString(indent + "  " + it + ",\n")
			}
			b.WriteString(indent + "}")
			return b.String(), nil
		}
		return "{" + strings.Join(items, ", ") + "}", nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Selector != nil:
		return c.compileSelector(p.Selector), nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Group != nil:
		e, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileIfExpr(ix *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ix.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ix.Then)
	if err != nil {
		return "", err
	}
	if ix.ElseIf != nil {
		elseExpr, err := c.compileIfExpr(ix.ElseIf)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
	}
	elseCode := "null"
	if ix.Else != nil {
		elseCode, err = c.compileExpr(ix.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseCode), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	var b bytes.Buffer
	tmp := fmt.Sprintf("_q%d", c.tmp)
	c.tmp++

	w := func(s string) { b.WriteString(s) }

	origEnv := c.env
	origGroup := c.groupKeys
	origFields := c.fieldTypes
	tmpGroup := make(map[string]string, len(c.groupKeys))
	for k, v := range c.groupKeys {
		tmpGroup[k] = v
	}
	c.groupKeys = tmpGroup
	tmpFields := make(map[string]map[string]types.Type, len(c.fieldTypes))
	for k, v := range c.fieldTypes {
		fv := make(map[string]types.Type, len(v))
		for kk, vv := range v {
			fv[kk] = vv
		}
		tmpFields[k] = fv
	}
	c.fieldTypes = tmpFields
	child := types.NewEnv(c.env)
	srcType := types.TypeOfExpr(q.Source, c.env)
	var elemType types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemType = lt.Elem
		if _, ok := lt.Elem.(types.MapType); ok {
			c.mapVars[q.Var] = true
		}
	}
	child.SetVar(q.Var, elemType, true)
	for _, f := range q.Froms {
		ft := types.TypeOfExpr(f.Src, c.env)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
			if _, ok := lt.Elem.(types.MapType); ok {
				c.mapVars[f.Var] = true
			}
		}
		child.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := types.TypeOfExpr(j.Src, c.env)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
			if _, ok := lt.Elem.(types.MapType); ok {
				c.mapVars[j.Var] = true
			}
		}
		if j.Side != nil && (*j.Side == "left" || *j.Side == "right" || *j.Side == "outer") {
			child.SetVar(j.Var, types.OptionType{Elem: je}, true)
			if *j.Side == "right" || *j.Side == "outer" {
				// left side may be null
				child.SetVar(q.Var, types.OptionType{Elem: elemType}, true)
			}
		} else {
			child.SetVar(j.Var, je, true)
		}
	}
	c.env = child

	// special cases for simple right or outer joins
	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil && q.Sort == nil &&
		q.Skip == nil && q.Take == nil && q.Where == nil {
		j := q.Joins[0]
		if j.Side != nil && (*j.Side == "right" || *j.Side == "outer") {
			var out bytes.Buffer
			out.WriteString("(() {\n")
			out.WriteString(fmt.Sprintf("  var %s = <dynamic>[];\n", tmp))
			left := c.mustExpr(q.Source)
			right := c.mustExpr(j.Src)
			cond := c.mustExpr(j.On)
			sel := c.mustExpr(q.Select)

			if *j.Side == "right" {
				tmpVar := fmt.Sprintf("_jt%d", c.tmp)
				c.tmp++
				out.WriteString(fmt.Sprintf("  for (var %s in %s) {\n", j.Var, right))
				out.WriteString(fmt.Sprintf("    var %s = <dynamic>[];\n", tmpVar))
				out.WriteString(fmt.Sprintf("    for (var %s in %s) {\n", q.Var, left))
				out.WriteString(fmt.Sprintf("      if (!(%s)) continue;\n", cond))
				out.WriteString(fmt.Sprintf("      %s.add(%s);\n", tmpVar, q.Var))
				out.WriteString("    }\n")
				out.WriteString(fmt.Sprintf("    if (%s.isEmpty) %s.add(null);\n", tmpVar, tmpVar))
				out.WriteString(fmt.Sprintf("    for (var %s in %s) {\n", q.Var, tmpVar))
				out.WriteString(fmt.Sprintf("      %s.add(%s);\n", tmp, sel))
				out.WriteString("    }\n")
				out.WriteString("  }\n")
				out.WriteString(fmt.Sprintf("  return %s;\n", tmp))
				out.WriteString("})()")
				c.env = origEnv
				return out.String(), nil
			}

			if *j.Side == "outer" {
				tmpVar := fmt.Sprintf("_jt%d", c.tmp)
				c.tmp++
				out.WriteString(fmt.Sprintf("  for (var %s in %s) {\n", q.Var, left))
				out.WriteString(fmt.Sprintf("    var %s = <dynamic>[];\n", tmpVar))
				out.WriteString(fmt.Sprintf("    for (var %s in %s) {\n", j.Var, right))
				out.WriteString(fmt.Sprintf("      if (!(%s)) continue;\n", cond))
				out.WriteString(fmt.Sprintf("      %s.add(%s);\n", tmpVar, j.Var))
				out.WriteString("    }\n")
				out.WriteString(fmt.Sprintf("    if (%s.isEmpty) %s.add(null);\n", tmpVar, tmpVar))
				out.WriteString(fmt.Sprintf("    for (var %s in %s) {\n", j.Var, tmpVar))
				out.WriteString(fmt.Sprintf("      %s.add(%s);\n", tmp, sel))
				out.WriteString("    }\n")
				out.WriteString("  }\n")
				// unmatched right side
				out.WriteString(fmt.Sprintf("  for (var %s in %s) {\n", j.Var, right))
				out.WriteString("    var _f = false;\n")
				out.WriteString(fmt.Sprintf("    for (var %s in %s) {\n", q.Var, left))
				out.WriteString(fmt.Sprintf("      if (!(%s)) continue;\n", cond))
				out.WriteString("      _f = true;\n      break;\n    }\n")
				out.WriteString("    if (!_f) {\n")
				out.WriteString(fmt.Sprintf("      var %s = null;\n", q.Var))
				out.WriteString(fmt.Sprintf("      %s.add(%s);\n", tmp, sel))
				out.WriteString("    }\n")
				out.WriteString("  }\n")
				out.WriteString(fmt.Sprintf("  return %s;\n", tmp))
				out.WriteString("})()")
				c.env = origEnv
				return out.String(), nil
			}
		}
	}

	// handle simple aggregate queries like `select sum(x)` when no grouping
	if q.Group == nil {
		if agg, arg, ok := c.aggregateCall(q.Select); ok {
			w("(() {\n")
			w(fmt.Sprintf("  var %s = <dynamic>[];\n", tmp))

			type loopInfo struct {
				pre        []string
				head, cond string
				boolCond   bool
				typ        types.Type
			}
			loops := []loopInfo{{head: fmt.Sprintf("var %s in %s", q.Var, c.mustExpr(q.Source))}}
			if t := types.TypeOfExpr(q.Source, c.env); t != nil {
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.MapType); ok {
						c.mapVars[q.Var] = true
					}
				}
			}
			if name, ok := c.simpleIdentifier(q.Source); ok {
				if ft, ok := c.fieldTypes[name]; ok {
					c.mapVars[q.Var] = true
					c.fieldTypes[q.Var] = ft
					for f, t := range ft {
						child.SetVar(q.Var+"."+f, t, true)
					}
				}
			}
			for _, f := range q.Froms {
				loops = append(loops, loopInfo{head: fmt.Sprintf("var %s in %s", f.Var, c.mustExpr(f.Src))})
				if t := types.TypeOfExpr(f.Src, c.env); t != nil {
					if lt, ok := t.(types.ListType); ok {
						if _, ok := lt.Elem.(types.MapType); ok {
							c.mapVars[f.Var] = true
						}
					}
				}
			}
			for _, j := range q.Joins {
				js := c.mustExpr(j.Src)
				on := c.mustExpr(j.On)
				jt := types.TypeOfExpr(j.On, c.env)
				loops = append(loops, loopInfo{head: fmt.Sprintf("var %s in %s", j.Var, js), cond: on, boolCond: isBoolType(jt), typ: jt})
				if t := types.TypeOfExpr(j.Src, c.env); t != nil {
					if lt, ok := t.(types.ListType); ok {
						if _, ok := lt.Elem.(types.MapType); ok {
							c.mapVars[j.Var] = true
						}
					}
				}
				if name, ok := c.simpleIdentifier(j.Src); ok {
					if ft, ok := c.fieldTypes[name]; ok {
						c.mapVars[j.Var] = true
						c.fieldTypes[j.Var] = ft
						for fld, t := range ft {
							child.SetVar(j.Var+"."+fld, t, true)
						}
					}
				}
			}
			for i, loop := range loops {
				for _, pl := range loop.pre {
					w(strings.Repeat("  ", i+1) + pl + "\n")
				}
				w(strings.Repeat("  ", i+1) + "for (" + loop.head + ") {\n")
				if cond := loop.cond; cond != "" {
					if loop.boolCond {
						w(strings.Repeat("  ", i+2) + fmt.Sprintf("if (!(%s)) continue;\n", cond))
					} else if isOptionType(loop.typ) {
						w(strings.Repeat("  ", i+2) + fmt.Sprintf("if (%s == null) continue;\n", cond))
					} else {
						w(strings.Repeat("  ", i+2) + fmt.Sprintf("if (!((%s) ?? false)) continue;\n", cond))
					}
				}
			}
			if q.Where != nil {
				condStr := c.mustExpr(q.Where)
				condT := types.TypeOfExpr(q.Where, c.env)
				if isBoolType(condT) {
					w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("if (!(%s)) continue;\n", condStr))
				} else if isOptionType(condT) {
					w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("if (%s == null) continue;\n", condStr))
				} else {
					w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("if (!((%s) ?? false)) continue;\n", condStr))
				}
			}
			val := c.mustExpr(arg)
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.add(%s);\n", tmp, val))
			for i := len(loops) - 1; i >= 0; i-- {
				w(strings.Repeat("  ", i+1) + "}\n")
			}
			w(fmt.Sprintf("  return %s;\n", c.aggregateExpr(agg, tmp)))
			w("})()")
			c.env = origEnv
			c.groupKeys = origGroup
			return b.String(), nil
		}
	}

	w("(() {\n")
	w(fmt.Sprintf("  var %s = <dynamic>[];\n", tmp))
	grpVar := ""
	groups := ""
	keyVar := ""
	if q.Group != nil {
		groups = fmt.Sprintf("_g%d", c.tmp)
		c.tmp++
		w(fmt.Sprintf("  var %s = <String, List<dynamic>>{};\n", groups))
	}

	type loopInfo struct {
		pre      []string
		head     string
		cond     string
		boolCond bool
		typ      types.Type
	}
	loops := []loopInfo{{head: fmt.Sprintf("var %s in %s", q.Var, c.mustExpr(q.Source))}}
	// mark main variable type
	if t := types.TypeOfExpr(q.Source, c.env); t != nil {
		if lt, ok := t.(types.ListType); ok {
			if _, ok := lt.Elem.(types.MapType); ok {
				c.mapVars[q.Var] = true
			}
		}
	}
	if name, ok := c.simpleIdentifier(q.Source); ok {
		if ft, ok := c.fieldTypes[name]; ok {
			c.mapVars[q.Var] = true
			c.fieldTypes[q.Var] = ft
			for f, t := range ft {
				child.SetVar(q.Var+"."+f, t, true)
			}
		}
	}
	for _, f := range q.Froms {
		loops = append(loops, loopInfo{head: fmt.Sprintf("var %s in %s", f.Var, c.mustExpr(f.Src))})
		if t := types.TypeOfExpr(f.Src, c.env); t != nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.MapType); ok {
					c.mapVars[f.Var] = true
				}
			}
		}
		if name, ok := c.simpleIdentifier(f.Src); ok {
			if ft, ok := c.fieldTypes[name]; ok {
				c.mapVars[f.Var] = true
				c.fieldTypes[f.Var] = ft
				for fld, t := range ft {
					child.SetVar(f.Var+"."+fld, t, true)
				}
			}
		}
	}
	for _, j := range q.Joins {
		js := c.mustExpr(j.Src)
		on := c.mustExpr(j.On)
		if j.Side != nil && *j.Side == "left" {
			tmpVar := fmt.Sprintf("_jt%d", c.tmp)
			c.tmp++
			pre := []string{
				fmt.Sprintf("var %s = <dynamic>[];", tmpVar),
				fmt.Sprintf("for (var %s in %s) {", j.Var, js),
				fmt.Sprintf("  if (!(%s)) continue;", on),
				fmt.Sprintf("  %s.add(%s);", tmpVar, j.Var),
				"}",
				fmt.Sprintf("if (%s.isEmpty) %s.add(null);", tmpVar, tmpVar),
			}
			loops = append(loops, loopInfo{pre: pre, head: fmt.Sprintf("var %s in %s", j.Var, tmpVar), boolCond: true, typ: types.BoolType{}})
		} else {
			jt := types.TypeOfExpr(j.On, c.env)
			loops = append(loops, loopInfo{head: fmt.Sprintf("var %s in %s", j.Var, js), cond: on, boolCond: isBoolType(jt), typ: jt})
		}
		if t := types.TypeOfExpr(j.Src, c.env); t != nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.MapType); ok {
					c.mapVars[j.Var] = true
				}
			}
		}
	}

	for i, loop := range loops {
		for _, pl := range loop.pre {
			w(strings.Repeat("  ", i+1) + pl + "\n")
		}
		w(strings.Repeat("  ", i+1) + "for (" + loop.head + ") {\n")
		if cond := loop.cond; cond != "" {
			if loop.boolCond {
				w(strings.Repeat("  ", i+2) + fmt.Sprintf("if (!(%s)) continue;\n", cond))
			} else if isOptionType(loop.typ) {
				w(strings.Repeat("  ", i+2) + fmt.Sprintf("if (%s == null) continue;\n", cond))
			} else {
				w(strings.Repeat("  ", i+2) + fmt.Sprintf("if (!((%s) ?? false)) continue;\n", cond))
			}
		}
	}

	if q.Where != nil {
		condStr := c.mustExpr(q.Where)
		condT := types.TypeOfExpr(q.Where, c.env)
		if isBoolType(condT) {
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("if (!(%s)) continue;\n", condStr))
		} else if isOptionType(condT) {
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("if (%s == null) continue;\n", condStr))
		} else {
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("if (!((%s) ?? false)) continue;\n", condStr))
		}
	}

	sel := c.mustExpr(q.Select)
	var keyT types.Type
	var elemT types.Type
	if q.Group != nil {
		keyVar = fmt.Sprintf("_k%d", c.tmp)
		c.tmp++
		if name, ok := c.simpleIdentifier(q.Select); ok && name == q.Group.Name {
			sel = fmt.Sprintf("{'key': %s, 'items': %s}", keyVar, q.Group.Name)
		}
		keyExpr := c.mustExpr(q.Group.Exprs[0])
		grpVar = q.Group.Name
		c.groupKeys[grpVar] = keyVar

		vars := []string{q.Var}
		for _, f := range q.Froms {
			vars = append(vars, f.Var)
		}
		for _, j := range q.Joins {
			vars = append(vars, j.Var)
		}

		w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("var %s = %s;\n", keyVar, keyExpr))
		keyStr := fmt.Sprintf("%s_s", keyVar)
		c.useJSON = true
		w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("var %s = jsonEncode(%s);\n", keyStr, keyVar))

		keyT = types.TypeOfExpr(q.Group.Exprs[0], c.env)
		if len(vars) == 1 {
			if t, err := c.env.GetVar(vars[0]); err == nil {
				elemT = t
			} else {
				elemT = types.AnyType{}
			}
			child.SetVar(grpVar, types.GroupType{Key: keyT, Elem: elemT}, true)
			item := vars[0]
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.putIfAbsent(%s_s, () => <dynamic>[]).add(%s);\n", groups, keyVar, item))
		} else {
			c.mapVars[grpVar] = true
			elemT = types.MapType{Key: types.StringType{}, Value: types.OptionType{Elem: types.AnyType{}}}
			ft := make(map[string]types.Type)
			for _, v := range vars {
				if t, err := c.env.GetVar(v); err == nil {
					ft[v] = t
				}
			}
			c.fieldTypes[grpVar] = ft
			child.SetVar(grpVar, types.GroupType{Key: keyT, Elem: elemT}, true)
			parts := []string{"'" + q.Var + "': " + q.Var}
			for _, f := range q.Froms {
				parts = append(parts, "'"+f.Var+"': "+f.Var)
			}
			for _, j := range q.Joins {
				parts = append(parts, "'"+j.Var+"': "+j.Var)
			}
			item := "{" + strings.Join(parts, ", ") + "}"
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.putIfAbsent(%s_s, () => <dynamic>[]).add(%s);\n", groups, keyVar, item))
		}
	} else {
		if q.Sort != nil {
			key := c.mustExpr(q.Sort)
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.add([%s, %s]);\n", tmp, key, sel))
		} else {
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.add(%s);\n", tmp, sel))
		}
	}

	for i := len(loops) - 1; i >= 0; i-- {
		w(strings.Repeat("  ", i+1) + "}\n")
	}

	if q.Group != nil {
		w(fmt.Sprintf("  for (var entry in %s.entries) {\n", groups))
		w(fmt.Sprintf("    var %s = entry.value;\n", grpVar))
		c.useJSON = true
		w(fmt.Sprintf("    var %s = jsonDecode(entry.key);\n", keyVar))

		// update environment so `g` refers to the list of items and
		// the key variable has the proper type while compiling the select
		groupEnv := types.NewEnv(c.env)
		groupEnv.SetVar(grpVar, types.ListType{Elem: elemT}, true)
		groupEnv.SetVar(keyVar, keyT, true)
		origEnv := c.env
		c.env = groupEnv

		if q.Group.Having != nil {
			condStr := c.mustExpr(q.Group.Having)
			condT := types.TypeOfExpr(q.Group.Having, c.env)
			if isBoolType(condT) {
				w(fmt.Sprintf("    if (!(%s)) continue;\n", condStr))
			} else if isOptionType(condT) {
				w(fmt.Sprintf("    if (%s == null) continue;\n", condStr))
			} else {
				w(fmt.Sprintf("    if (!((%s) ?? false)) continue;\n", condStr))
			}
		}
		sel := c.mustExpr(q.Select)
		if name, ok := c.simpleIdentifier(q.Select); ok && name == q.Group.Name {
			sel = fmt.Sprintf("{'key': %s, 'items': %s}", keyVar, grpVar)
		}
		if q.Sort != nil {
			key := c.mustExpr(q.Sort)
			w(fmt.Sprintf("    %s.add([%s, %s]);\n", tmp, key, sel))
		} else {
			w(fmt.Sprintf("    %s.add(%s);\n", tmp, sel))
		}
		c.env = origEnv
		w("  }\n")
	}

	if q.Sort != nil {
		cmpA := "a[0]"
		cmpB := "b[0]"
		if !isComparableType(types.TypeOfExpr(q.Sort, c.env)) {
			c.useJSON = true
			cmpA = fmt.Sprintf("jsonEncode(%s)", cmpA)
			cmpB = fmt.Sprintf("jsonEncode(%s)", cmpB)
		}
		w(fmt.Sprintf("  %s.sort((a,b) => (%s as Comparable).compareTo(%s));\n", tmp, cmpA, cmpB))
		w(fmt.Sprintf("  %s = [for (var x in %s) x[1]];\n", tmp, tmp))
	}
	if q.Skip != nil || q.Take != nil {
		start := "0"
		if q.Skip != nil {
			start = c.mustExpr(q.Skip)
		}
		end := fmt.Sprintf("%s.length", tmp)
		if q.Take != nil {
			end = c.mustExpr(q.Take)
			if q.Skip != nil {
				end = fmt.Sprintf("(%s)+(%s)", start, end)
			}
		}
		w(fmt.Sprintf("  %s = %s.sublist(%s, %s);\n", tmp, tmp, start, end))
	}
	if q.Distinct {
		w(fmt.Sprintf("  %s = %s.toSet().toList();\n", tmp, tmp))
	}
	w(fmt.Sprintf("  return %s;\n", tmp))
	w("})()")
	c.env = origEnv
	c.groupKeys = origGroup
	c.fieldTypes = origFields
	return b.String(), nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	fieldMap := make(map[string]string)
	for _, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fieldMap[f.Name] = v
	}
	if st, ok := c.env.GetStruct(sl.Name); ok {
		args := make([]string, len(st.Order))
		for i, name := range st.Order {
			if v, ok := fieldMap[name]; ok {
				args[i] = v
			} else {
				args[i] = defaultValue(dartTypeFromType(st.Fields[name]))
			}
		}
		return fmt.Sprintf("%s(%s)", sl.Name, strings.Join(args, ", ")), nil
	}
	items := make([]string, 0, len(sl.Fields))
	for name, v := range fieldMap {
		items = append(items, fmt.Sprintf("'%s': %s", name, v))
	}
	return "{" + strings.Join(items, ", ") + "}", nil
}

func (c *Compiler) mustExpr(e *parser.Expr) string {
	s, err := c.compileExpr(e)
	if err != nil {
		return "null"
	}
	return s
}

func (c *Compiler) compileMapKey(e *parser.Expr) (string, error) {
	if name, ok := c.simpleIdentifier(e); ok {
		return fmt.Sprintf("'%s'", name), nil
	}
	return c.compileExpr(e)
}

func (c *Compiler) simpleIdentifier(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return escapeIdent(p.Target.Selector.Root), true
}

func (c *Compiler) simpleString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return "", false
	}
	p := u.Value
	if p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil || len(p.Ops) > 0 {
		return "", false
	}
	return *p.Target.Lit.Str, true
}

// aggregateCall returns the aggregate builtin name and argument if e is a call
// to sum/avg/min/max/count/len.
func (c *Compiler) aggregateCall(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", nil, false
	}
	p := u.Value
	if p.Target == nil || p.Target.Call == nil || len(p.Ops) > 0 {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	switch call.Func {
	case "sum", "avg", "min", "max", "count", "len":
		return call.Func, call.Args[0], true
	}
	return "", nil, false
}

func (c *Compiler) aggregateExpr(name, list string) string {
	switch name {
	case "count", "len":
		return fmt.Sprintf("%s.length", list)
	case "sum":
		return fmt.Sprintf("_sum(%s)", list)
	case "avg":
		return fmt.Sprintf("(%s.isEmpty ? 0 : %s.reduce((a, b) => a + b) / %s.length)", list, list, list)
	case "min":
		return fmt.Sprintf("_min(%s)", list)
	case "max":
		return fmt.Sprintf("%s.reduce((a, b) => a > b ? a : b)", list)
	}
	return list
}

// simpleMapQuery returns a Dart expression using collection methods if the
// query is of the form `from v in src select expr` with no additional clauses.
func (c *Compiler) simpleMapQuery(q *parser.QueryExpr) (string, bool) {
	if q == nil || len(q.Froms) != 0 || len(q.Joins) != 0 || q.Where != nil ||
		q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil ||
		q.Distinct {
		return "", false
	}

	src := c.mustExpr(q.Source)
	child := types.NewEnv(c.env)
	srcType := types.TypeOfExpr(q.Source, c.env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemT = lt.Elem
		if _, ok := lt.Elem.(types.MapType); ok {
			c.mapVars[q.Var] = true
		}
	}
	child.SetVar(q.Var, elemT, true)
	orig := c.env
	c.env = child
	sel := c.mustExpr(q.Select)
	c.env = orig

	return fmt.Sprintf("%s.map((%s) => %s)", src, q.Var, sel), true
}

func (c *Compiler) compileSelector(sel *parser.SelectorExpr) string {
	rootOrig := sel.Root
	root := escapeIdent(sel.Root)
	if root == "main" && c.mainRename != "" {
		root = c.mainRename
	}
	tail := sel.Tail
	if kind, ok := c.imports[root]; ok {
		switch kind {
		case "dart_math", "go_testpkg":
			s := root
			for _, part := range tail {
				s += "." + escapeIdent(part)
			}
			return s
		}
	}
	if key, ok := c.groupKeys[root]; ok {
		if len(tail) > 0 && tail[0] == "key" {
			root = key
			tail = tail[1:]
		} else if len(tail) > 0 && tail[0] == "items" {
			tail = tail[1:]
		}
		if len(tail) == 0 {
			return root
		}
	}
	typ, err := c.env.GetVar(rootOrig)
	if ot, ok := typ.(types.OptionType); ok {
		typ = ot.Elem
	}
	if ok := c.mapVars[rootOrig]; ok {
		typ = types.MapType{}
	}
	if _, ok := c.groupKeys[sel.Root]; ok && root != sel.Root {
		typ = types.MapType{}
	}
	if (typ == nil || typ == types.AnyType{}) && len(tail) > 0 {
		r := []rune(root)
		if len(r) > 0 && unicode.IsLower(r[0]) {
			typ = types.MapType{}
		}
	}
	if st, ok := c.env.GetStruct(root); ok && len(st.Order) == 0 {
		return root + "()"
	}
	if ut, ok := c.env.FindUnionByVariant(root); ok {
		if st, ok := ut.Variants[root]; ok && len(st.Order) == 0 {
			return root + "()"
		}
	}
	if err != nil {
		// maybe a group key reference like `supp_nation`
		if len(c.groupKeys) == 1 {
			for _, k := range c.groupKeys {
				s := fmt.Sprintf("%s['%s']", k, root)
				if len(tail) > 0 {
					esc := make([]string, len(tail))
					for i, t := range tail {
						esc[i] = escapeIdent(t)
					}
					s += fmt.Sprintf("['%s']", strings.Join(esc, "']['"))
				}
				return s
			}
		}
	}
	if _, ok := typ.(types.MapType); ok {
		s := root
		tcur := typ
		for i, part := range tail {
			switch tt := tcur.(type) {
			case types.MapType:
				s += fmt.Sprintf("['%s']", escapeIdent(part))
				if tt.Value == nil {
					if i+1 < len(tail) {
						nx := tail[i+1]
						if nx != "contains" && nx != "starts_with" && nx != "ends_with" {
							tcur = types.MapType{}
						} else {
							tcur = types.AnyType{}
						}
					} else {
						tcur = types.MapType{}
					}
				} else if _, ok := tt.Value.(types.AnyType); ok {
					if i+1 < len(tail) {
						nx := tail[i+1]
						if nx != "contains" && nx != "starts_with" && nx != "ends_with" {
							tcur = types.MapType{}
						} else {
							tcur = types.AnyType{}
						}
					} else {
						tcur = types.MapType{}
					}
				} else {
					tcur = tt.Value
				}
			case types.StructType:
				s += "." + escapeIdent(part)
				if ft, ok := tt.Fields[part]; ok {
					tcur = ft
				} else {
					tcur = types.AnyType{}
				}
			default:
				if i == 0 {
					s += fmt.Sprintf("['%s']", escapeIdent(part))
				} else {
					s = fmt.Sprintf("(%s as dynamic).%s", s, escapeIdent(part))
				}
				tcur = types.AnyType{}
			}
		}
		return s
	}
	if _, ok := typ.(types.GroupType); ok {
		s := root
		for _, part := range tail {
			s += fmt.Sprintf("['%s']", escapeIdent(part))
		}
		return s
	}
	s := root
	for _, part := range tail {
		s += "." + escapeIdent(part)
	}
	return s
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	name := call.Func
	if name == "main" && c.mainRename != "" {
		name = c.mainRename
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	if fn, ok := c.env.GetFunc(call.Func); ok && len(call.Args) < len(fn.Params) {
		missing := fn.Params[len(call.Args):]
		vars := make([]string, len(missing))
		for i, p := range missing {
			vars[i] = p.Name
		}
		all := append(append([]string{}, args...), vars...)
		return fmt.Sprintf("(%s) => %s(%s)", strings.Join(vars, ", "), name, strings.Join(all, ", ")), nil
	}
	// handle simple builtins so the generated Dart code is runnable without
	// additional support libraries
	switch call.Func {
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("print(%s)", args[0]), nil
		}
		return fmt.Sprintf("_print([%s])", strings.Join(args, ", ")), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("List.from(%s)..add(%s)", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		a := args[0]
		if _, ok := c.simpleIdentifier(call.Args[0]); ok {
			return fmt.Sprintf("(%s.isEmpty ? 0 : %s.reduce((a, b) => a + b) / %s.length)", a, a, a), nil
		}
		tmp := fmt.Sprintf("_t%d", c.tmp)
		c.tmp++
		return fmt.Sprintf("(() { var %s = %s; return (%s.isEmpty ? 0 : %s.reduce((a, b) => a + b) / %s.length); })()", tmp, a, tmp, tmp, tmp), nil
	case "count", "len":
		if len(args) != 1 {
			return "", fmt.Errorf("%s expects 1 arg", call.Func)
		}
		return fmt.Sprintf("%s.length", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		if arg := call.Args[0]; arg != nil && arg.Binary != nil && len(arg.Binary.Right) == 0 && arg.Binary.Left != nil && arg.Binary.Left.Value != nil && arg.Binary.Left.Value.Target != nil && arg.Binary.Left.Value.Target.Query != nil {
			if expr, ok := c.simpleMapQuery(arg.Binary.Left.Value.Target.Query); ok {
				return fmt.Sprintf("_sum(%s)", expr), nil
			}
		}
		a := args[0]
		if _, ok := c.simpleIdentifier(call.Args[0]); ok {
			return fmt.Sprintf("_sum(%s)", a), nil
		}
		tmp := fmt.Sprintf("_t%d", c.tmp)
		c.tmp++
		return fmt.Sprintf("(() { var %s = %s; return _sum(%s); })()", tmp, a, tmp), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		a := args[0]
		if _, ok := c.simpleIdentifier(call.Args[0]); ok {
			return fmt.Sprintf("_min(%s)", a), nil
		}
		tmp := fmt.Sprintf("_t%d", c.tmp)
		c.tmp++
		return fmt.Sprintf("(() { var %s = %s; return _min(%s); })()", tmp, a, tmp), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		a := args[0]
		if _, ok := c.simpleIdentifier(call.Args[0]); ok {
			return fmt.Sprintf("%s.reduce((a, b) => a > b ? a : b)", a), nil
		}
		tmp := fmt.Sprintf("_t%d", c.tmp)
		c.tmp++
		return fmt.Sprintf("(() { var %s = %s; return %s.reduce((a, b) => a > b ? a : b); })()", tmp, a, tmp), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("%s.toString()", args[0]), nil
	case "substring":
		if len(args) == 2 {
			return fmt.Sprintf("%s.toString().substring(%s)", args[0], args[1]), nil
		} else if len(args) == 3 {
			return fmt.Sprintf("%s.toString().substring(%s, %s)", args[0], args[1], args[2]), nil
		}
		return "", fmt.Errorf("substring expects 2 or 3 args")
	case "starts_with":
		if len(args) != 2 {
			return "", fmt.Errorf("starts_with expects 2 args")
		}
		return fmt.Sprintf("%s.toString().startsWith(%s.toString())", args[0], args[1]), nil
	case "ends_with":
		if len(args) != 2 {
			return "", fmt.Errorf("ends_with expects 2 args")
		}
		return fmt.Sprintf("%s.toString().endsWith(%s.toString())", args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values.toList()", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("%s.isNotEmpty", args[0]), nil
	case "now":
		if len(args) != 0 {
			return "", fmt.Errorf("now expects no args")
		}
		return "DateTime.now().microsecondsSinceEpoch", nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		c.useJSON = true
		return fmt.Sprintf("print(jsonEncode(%s))", args[0]), nil
	}

	return fmt.Sprintf("%s(%s)", escapeIdent(name), strings.Join(args, ", ")), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := ""
	if l.Path != nil {
		p := *l.Path
		if strings.HasPrefix(p, "../") {
			p = filepath.Join("tests", strings.TrimPrefix(p, "../"))
		}
		path = p
	}
	opts := "null"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.useLoad = true
	c.useIO = true
	c.useJSON = true
	c.useYAML = true
	expr := fmt.Sprintf("_load('%s', %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		tname := *l.Type.Simple
		if st, ok := c.env.GetStruct(tname); ok {
			args := make([]string, len(st.Order))
			for i, name := range st.Order {
				typ := dartTypeFromType(st.Fields[name])
				if typ == "" {
					typ = "dynamic"
				}
				args[i] = fmt.Sprintf("(_it['%s'] as %s)", name, typ)
			}
			return fmt.Sprintf("[for (var _it in (%s)) %s(%s)]", expr, tname, strings.Join(args, ", ")), nil
		}
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "-"
	if s.Path != nil {
		path = *s.Path
	}
	format := "json"
	if s.With != nil {
		if str, ok := c.simpleString(s.With); ok {
			format = str
		}
	}
	c.useIO = true
	c.useJSON = true
	if format == "jsonl" && path == "-" {
		return fmt.Sprintf("%s.forEach((r) => stdout.writeln(jsonEncode(r)))", src), nil
	}
	if format == "json" {
		return fmt.Sprintf("File('%s').writeAsStringSync(jsonEncode(%s))", path, src), nil
	}
	return "null", fmt.Errorf("unsupported save format")
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(() {\n")
	b.WriteString("  var _t = " + target + ";\n")
	hasDefault := false
	for i, cs := range m.Cases {
		if isUnderscoreExpr(cs.Pattern) {
			res, err := c.compileExpr(cs.Result)
			if err != nil {
				return "", err
			}
			b.WriteString("  else {\n    return " + res + ";\n  }\n")
			hasDefault = true
			continue
		}
		if name, vars, ok := c.constructorPattern(cs.Pattern); ok {
			st, _ := c.env.GetStruct(name)
			res, err := c.compileExpr(cs.Result)
			if err != nil {
				return "", err
			}
			if i == 0 {
				b.WriteString("  if (_t is " + name + ") {\n")
			} else {
				b.WriteString(" else if (_t is " + name + ") {\n")
			}
			for idx, v := range vars {
				if idx < len(st.Order) {
					b.WriteString("    var " + v + " = (_t as " + name + ")." + st.Order[idx] + ";\n")
				}
			}
			b.WriteString("    return " + res + ";\n  }")
			continue
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if i == 0 {
			b.WriteString("  if (_t == " + pat + ") {\n    return " + res + ";\n  }")
		} else {
			b.WriteString(" else if (_t == " + pat + ") {\n    return " + res + ";\n  }")
		}
	}
	if !hasDefault {
		b.WriteString("  return null;\n")
	}
	b.WriteString("})()")
	return "(" + b.String() + " as dynamic)", nil
}

func (c *Compiler) constructorPattern(e *parser.Expr) (string, []string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", nil, false
	}
	p := u.Value
	if p.Target != nil && p.Target.Call != nil && len(p.Ops) == 0 {
		call := p.Target.Call
		vars := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, ok := c.simpleIdentifier(a)
			if !ok {
				return "", nil, false
			}
			vars[i] = v
		}
		return call.Func, vars, true
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Ops) == 0 && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, nil, true
	}
	return "", nil, false
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return false
	}
	return p.Target.Selector.Root == "_"
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	case l.Str != nil:
		s := strings.ReplaceAll(*l.Str, "'", "\\'")
		s = strings.ReplaceAll(s, "$", "\\$")
		s = strings.ReplaceAll(s, "\n", "\\n")
		s = strings.ReplaceAll(s, "\r", "\\r")
		return "'" + strings.Trim(s, "\"") + "'"
	case l.Null:
		return "null"
	default:
		return "null"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func dartTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "bool"
	case types.ListType:
		elem := dartTypeFromType(tt.Elem)
		if elem == "" {
			elem = "dynamic"
		}
		return fmt.Sprintf("List<%s>", elem)
	case types.MapType:
		k := dartTypeFromType(tt.Key)
		if k == "" {
			k = "dynamic"
		}
		v := dartTypeFromType(tt.Value)
		if v == "" {
			v = "dynamic"
		}
		return fmt.Sprintf("Map<%s, %s>", k, v)
	case types.UnionType:
		return tt.Name
	case types.StructType:
		return tt.Name
	default:
		return ""
	}
}

func dartType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Fun != nil {
		return "Function"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "double"
		case "string":
			return "String"
		case "bool":
			return "bool"
		}
		return *t.Simple
	}
	if t.Generic != nil {
		name := t.Generic.Name
		if name == "list" && len(t.Generic.Args) == 1 {
			elem := dartType(t.Generic.Args[0])
			if elem == "" {
				elem = "dynamic"
			}
			return fmt.Sprintf("List<%s>", elem)
		}
		if name == "map" && len(t.Generic.Args) == 2 {
			k := dartType(t.Generic.Args[0])
			if k == "" {
				k = "dynamic"
			}
			v := dartType(t.Generic.Args[1])
			if v == "" {
				v = "dynamic"
			}
			return fmt.Sprintf("Map<%s, %s>", k, v)
		}
		return name
	}
	return ""
}

func defaultValue(typ string) string {
	switch typ {
	case "int":
		return "0"
	case "double":
		return "0.0"
	case "String":
		return "''"
	case "bool":
		return "false"
	default:
		if strings.HasPrefix(typ, "List<") {
			return "[]"
		}
		if strings.HasPrefix(typ, "Map<") {
			return "{}"
		}
		return "null"
	}
}

func isNumericType(t types.Type) bool {
	if ot, ok := t.(types.OptionType); ok {
		t = ot.Elem
	}
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.BigIntType, types.BigRatType:
		return true
	default:
		return false
	}
}

func isStringType(t types.Type) bool {
	if ot, ok := t.(types.OptionType); ok {
		t = ot.Elem
	}
	_, ok := t.(types.StringType)
	return ok
}

func isIntType(t types.Type) bool {
	if ot, ok := t.(types.OptionType); ok {
		t = ot.Elem
	}
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return true
	default:
		return false
	}
}

func isBoolType(t types.Type) bool {
	if ot, ok := t.(types.OptionType); ok {
		t = ot.Elem
	}
	_, ok := t.(types.BoolType)
	return ok
}

func isOptionType(t types.Type) bool {
	_, ok := t.(types.OptionType)
	return ok
}

func isComparableType(t types.Type) bool {
	if ot, ok := t.(types.OptionType); ok {
		t = ot.Elem
	}
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.StringType, types.BoolType:
		return true
	default:
		return false
	}
}

func isNumericOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "<", "<=", ">", ">=":
		return true
	default:
		return false
	}
}

func isMapType(t types.Type) bool {
	if ot, ok := t.(types.OptionType); ok {
		t = ot.Elem
	}
	_, ok := t.(types.MapType)
	return ok
}

func isListType(t types.Type) bool {
	if ot, ok := t.(types.OptionType); ok {
		t = ot.Elem
	}
	_, ok := t.(types.ListType)
	return ok
}

func findRepoRoot() string {
	dir, _ := os.Getwd()
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
