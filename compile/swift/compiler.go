package swiftcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Swift source code (very limited subset).
type Compiler struct {
	buf         bytes.Buffer
	indent      int
	env         *types.Env
	locals      map[string]types.Type
	useAvg      bool
	useIndex    bool
	useIndexStr bool
	useSlice    bool
	useSliceStr bool
	useLoad     bool
	useSave     bool
	useFetch    bool
	useJSON     bool
	funcRet     types.Type
}

func New(env *types.Env) *Compiler { return &Compiler{env: env, locals: map[string]types.Type{}} }

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.useAvg = false
	c.useIndex = false
	c.useIndexStr = false
	c.useSlice = false
	c.useSliceStr = false
	c.useLoad = false
	c.useSave = false
	c.useFetch = false
	c.useJSON = false

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent = 0

	// type declarations first
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// test blocks
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main body
	c.writeln("func main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("main()")

	bodyBytes := c.buf.Bytes()

	c.buf = oldBuf
	c.indent = 0

	c.writeln("import Foundation")
	c.writeln("")
	c.writeExpectFunc(prog)
	if c.useAvg {
		c.writeln("func _avg<T: BinaryInteger>(_ arr: [T]) -> Double {")
		c.indent++
		c.writeln("if arr.isEmpty { return 0 }")
		c.writeln("var sum = 0.0")
		c.writeln("for v in arr { sum += Double(v) }")
		c.writeln("return sum / Double(arr.count)")
		c.indent--
		c.writeln("}")
		c.writeln("func _avg<T: BinaryFloatingPoint>(_ arr: [T]) -> Double {")
		c.indent++
		c.writeln("if arr.isEmpty { return 0 }")
		c.writeln("var sum = 0.0")
		c.writeln("for v in arr { sum += Double(v) }")
		c.writeln("return sum / Double(arr.count)")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useIndexStr {
		c.writeln("func _indexString(_ s: String, _ i: Int) -> String {")
		c.indent++
		c.writeln("var idx = i")
		c.writeln("let chars = Array(s)")
		c.writeln("if idx < 0 { idx += chars.count }")
		c.writeln("if idx < 0 || idx >= chars.count { fatalError(\"index out of range\") }")
		c.writeln("return String(chars[idx])")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useIndex {
		c.writeln("func _index<T>(_ arr: [T], _ i: Int) -> T {")
		c.indent++
		c.writeln("var idx = i")
		c.writeln("let n = arr.count")
		c.writeln("if idx < 0 { idx += n }")
		c.writeln("if idx < 0 || idx >= n { fatalError(\"index out of range\") }")
		c.writeln("return arr[idx]")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useSlice {
		c.writeln("func _slice<T>(_ arr: [T], _ i: Int, _ j: Int) -> [T] {")
		c.indent++
		c.writeln("var start = i")
		c.writeln("var end = j")
		c.writeln("let n = arr.count")
		c.writeln("if start < 0 { start += n }")
		c.writeln("if end < 0 { end += n }")
		c.writeln("if start < 0 { start = 0 }")
		c.writeln("if end > n { end = n }")
		c.writeln("if end < start { end = start }")
		c.writeln("return Array(arr[start..<end])")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useSliceStr {
		c.writeln("func _sliceString(_ s: String, _ i: Int, _ j: Int) -> String {")
		c.indent++
		c.writeln("var start = i")
		c.writeln("var end = j")
		c.writeln("let chars = Array(s)")
		c.writeln("let n = chars.count")
		c.writeln("if start < 0 { start += n }")
		c.writeln("if end < 0 { end += n }")
		c.writeln("if start < 0 { start = 0 }")
		c.writeln("if end > n { end = n }")
		c.writeln("if end < start { end = start }")
		c.writeln("return String(chars[start..<end])")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useLoad {
		c.writeln("func _readInput(_ path: String?) -> String {")
		c.indent++
		c.writeln("if let p = path, !p.isEmpty && p != \"-\" {")
		c.indent++
		c.writeln("return (try? String(contentsOfFile: p)) ?? \"\"")
		c.indent--
		c.writeln("}")
		c.writeln("let data = FileHandle.standardInput.readDataToEndOfFile()")
		c.writeln("return String(data: data, encoding: .utf8) ?? \"\"")
		c.indent--
		c.writeln("}")
		c.writeln("func _parseCSV(_ text: String, _ header: Bool, _ delim: Character) -> [[String: Any]] {")
		c.indent++
		c.writeln("let lines = text.split(whereSeparator: { $0 == \"\n\" || $0 == \"\r\" })")
		c.writeln("if lines.isEmpty { return [] }")
		c.writeln("var headers: [String] = []")
		c.writeln("var start = 0")
		c.writeln("if header {")
		c.indent++
		c.writeln("headers = lines[0].split(separator: delim).map { String($0) }")
		c.writeln("start = 1")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("headers = lines[0].split(separator: delim).enumerated().map { \"c\" + String($0.offset) }")
		c.indent--
		c.writeln("}")
		c.writeln("var out: [[String: Any]] = []")
		c.writeln("for i in start..<lines.count {")
		c.indent++
		c.writeln("let parts = lines[i].split(separator: delim)")
		c.writeln("var row: [String: Any] = [:]")
		c.writeln("for j in 0..<headers.count {")
		c.indent++
		c.writeln("let val = j < parts.count ? String(parts[j]) : \"\"")
		c.writeln("if let iv = Int(val) { row[headers[j]] = iv } else if let dv = Double(val) { row[headers[j]] = dv } else { row[headers[j]] = val }")
		c.indent--
		c.writeln("}")
		c.writeln("out.append(row)")
		c.indent--
		c.writeln("}")
		c.writeln("return out")
		c.indent--
		c.writeln("}")
		c.writeln("func _load(_ path: String?, _ opts: [String: Any]?) -> [[String: Any]] {")
		c.indent++
		c.writeln("let format = (opts?[\"format\"] as? String) ?? \"csv\"")
		c.writeln("let header = (opts?[\"header\"] as? Bool) ?? true")
		c.writeln("var delim: Character = ','")
		c.writeln("if let d = opts?[\"delimiter\"] as? String, !d.isEmpty { delim = d.first! }")
		c.writeln("let text = _readInput(path)")
		c.writeln("switch format {")
		c.writeln("case \"jsonl\":")
		c.indent++
		c.writeln("return text.split(separator: \"\n\").filter { !$0.isEmpty }.compactMap { line in")
		c.indent++
		c.writeln("if let data = line.data(using: .utf8), let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any] { return obj }")
		c.writeln("return nil")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("case \"json\":")
		c.indent++
		c.writeln("if let data = text.data(using: .utf8) {")
		c.indent++
		c.writeln("if let arr = try? JSONSerialization.jsonObject(with: data) as? [[String: Any]] { return arr }")
		c.writeln("if let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any] { return [obj] }")
		c.indent--
		c.writeln("}")
		c.writeln("return []")
		c.indent--
		c.writeln("case \"tsv\":")
		c.indent++
		c.writeln("delim = '\t'")
		c.writeln("fallthrough")
		c.indent--
		c.writeln("default:")
		c.indent++
		c.writeln("return _parseCSV(text, header, delim)")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useSave {
		c.writeln("func _writeOutput(_ path: String?, _ text: String) {")
		c.indent++
		c.writeln("if let p = path, !p.isEmpty && p != \"-\" {")
		c.indent++
		c.writeln("try? text.write(toFile: p, atomically: true, encoding: .utf8)")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("if let data = text.data(using: .utf8) { FileHandle.standardOutput.write(data) }")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("func _save(_ rows: [[String: Any]], _ path: String?, _ opts: [String: Any]?) {")
		c.indent++
		c.writeln("let format = (opts?[\"format\"] as? String) ?? \"csv\"")
		c.writeln("let header = (opts?[\"header\"] as? Bool) ?? false")
		c.writeln("var delim: Character = ','")
		c.writeln("if let d = opts?[\"delimiter\"] as? String, !d.isEmpty { delim = d.first! }")
		c.writeln("var text = \"\"")
		c.writeln("switch format {")
		c.writeln("case \"jsonl\":")
		c.indent++
		c.writeln("for r in rows { if let d = try? JSONSerialization.data(withJSONObject: r), let s = String(data: d, encoding: .utf8) { text += s + \"\\n\" } }")
		c.indent--
		c.writeln("case \"json\":")
		c.indent++
		c.writeln("let obj: Any = rows.count == 1 ? rows[0] : rows")
		c.writeln("if let d = try? JSONSerialization.data(withJSONObject: obj), let s = String(data: d, encoding: .utf8) { text = s }")
		c.indent--
		c.writeln("case \"tsv\":")
		c.indent++
		c.writeln("delim = '\t'")
		c.writeln("fallthrough")
		c.indent--
		c.writeln("default:")
		c.indent++
		c.writeln("let headers = rows.isEmpty ? [] : rows[0].keys.sorted()")
		c.writeln("var lines: [String] = []")
		c.writeln("if header && !headers.isEmpty { lines.append(headers.joined(separator: String(delim))) }")
		c.writeln("for r in rows {")
		c.indent++
		c.writeln("let rec = headers.map { h in String(describing: r[h] ?? \"\") }")
		c.writeln("lines.append(rec.joined(separator: String(delim)))")
		c.indent--
		c.writeln("}")
		c.writeln("text = lines.joined(separator: \"\\n\") + \"\\n\"")
		c.indent--
		c.writeln("}")
		c.writeln("_writeOutput(path, text)")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useFetch {
		c.writeln("func _fetch(_ urlStr: String, _ opts: [String: Any]?) -> Any {")
		c.indent++
		c.writeln("guard let url = URL(string: urlStr) else { return [:] }")
		c.writeln("if let data = try? Data(contentsOf: url) {")
		c.indent++
		c.writeln("if let obj = try? JSONSerialization.jsonObject(with: data) { return obj }")
		c.writeln("return String(data: data, encoding: .utf8) ?? \"\"")
		c.indent--
		c.writeln("}")
		c.writeln("return [:]")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useJSON {
		c.writeln("func _json(_ v: Any) {")
		c.indent++
		c.writeln("if let d = try? JSONSerialization.data(withJSONObject: v, options: []), let s = String(data: d, encoding: .utf8) {")
		c.indent++
		c.writeln("print(s)")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	c.buf.Write(bodyBytes)
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("_ %s: %s", p.Name, c.compileType(p.Type))
	}
	ret := c.compileType(fn.Return)
	c.writeln(fmt.Sprintf("func %s(%s) -> %s {", fn.Name, strings.Join(params, ", "), ret))
	c.indent++
	oldEnv := c.env
	oldRet := c.funcRet
	oldLocals := c.locals
	c.locals = map[string]types.Type{}
	c.funcRet = resolveTypeRef(fn.Return, oldEnv)
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		for _, p := range fn.Params {
			if p.Type != nil {
				t := resolveTypeRef(p.Type, oldEnv)
				c.env.SetVar(p.Name, t, true)
				c.locals[p.Name] = t
			}
		}
	}
	for _, p := range fn.Params {
		decl := "var"
		if !paramAssigned(fn.Body, p.Name) {
			decl = "let"
		}
		c.writeln(fmt.Sprintf("%s %s = %s", decl, p.Name, p.Name))
	}
	if len(fn.Params) > 0 {
		c.writeln("")
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = oldEnv
	}
	c.locals = oldLocals
	c.funcRet = oldRet
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMethod(structName string, fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("_ %s: %s", p.Name, c.compileType(p.Type))
	}
	ret := c.compileType(fn.Return)
	c.writeln(fmt.Sprintf("func %s(%s) -> %s {", fn.Name, strings.Join(params, ", "), ret))
	c.indent++

	oldEnv := c.env
	oldRet := c.funcRet
	oldLocals := c.locals

	c.locals = map[string]types.Type{}
	if c.env != nil {
		methodEnv := types.NewEnv(c.env)
		if st, ok := c.env.GetStruct(structName); ok {
			for name, t := range st.Fields {
				methodEnv.SetVar(name, t, true)
				c.locals[name] = t
			}
		}
		c.env = methodEnv
		for _, p := range fn.Params {
			if p.Type != nil {
				t := resolveTypeRef(p.Type, oldEnv)
				c.env.SetVar(p.Name, t, true)
				c.locals[p.Name] = t
			}
		}
	}

	c.funcRet = resolveTypeRef(fn.Return, oldEnv)

	for _, p := range fn.Params {
		decl := "var"
		if !paramAssigned(fn.Body, p.Name) {
			decl = "let"
		}
		c.writeln(fmt.Sprintf("%s %s = %s", decl, p.Name, p.Name))
	}
	if len(fn.Params) > 0 {
		c.writeln("")
	}

	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}

	if c.env != nil {
		c.env = oldEnv
	}
	c.locals = oldLocals
	c.funcRet = oldRet

	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileType(t *parser.TypeRef) string {
	if t == nil {
		return "Void"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.compileType(p)
		}
		ret := c.compileType(t.Fun.Return)
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Bool"
		case "string":
			return "String"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return "[" + c.compileType(t.Generic.Args[0]) + "]"
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return "[" + c.compileType(t.Generic.Args[0]) + ": " + c.compileType(t.Generic.Args[1]) + "]"
		}
	}
	return "Any"
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("protocol %s {}", t.Name))
		for _, v := range t.Variants {
			c.writeln(fmt.Sprintf("struct %s: %s {", v.Name, t.Name))
			c.indent++
			for _, f := range v.Fields {
				typ := c.compileType(f.Type)
				c.writeln(fmt.Sprintf("var %s: %s", f.Name, typ))
			}
			c.indent--
			c.writeln("}")
		}
		return nil
	}
	c.writeln(fmt.Sprintf("struct %s {", t.Name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			typ := c.compileType(m.Field.Type)
			c.writeln(fmt.Sprintf("var %s: %s", m.Field.Name, typ))
		} else if m.Method != nil {
			if err := c.compileMethod(t.Name, m.Method); err != nil {
				return err
			}
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		typ := ""
		var t types.Type = types.AnyType{}
		if s.Let.Type != nil {
			typ = ": " + c.compileType(s.Let.Type)
			t = resolveTypeRef(s.Let.Type, c.env)
		}
		if typ == "" {
			t = c.inferExprType(s.Let.Value)
			if lt, ok := t.(types.ListType); ok && !containsAny(lt) {
				typ = ": [" + swiftType(lt.Elem) + "]"
			} else if mt, ok := t.(types.MapType); ok && !containsAny(mt) {
				typ = ": [" + swiftType(mt.Key) + ": " + swiftType(mt.Value) + "]"
			}
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("let %s%s", s.Let.Name, typ))
		} else {
			c.writeln(fmt.Sprintf("let %s%s = %s", s.Let.Name, typ, expr))
		}
		if c.env != nil {
			c.env.SetVar(s.Let.Name, t, true)
		}
		c.locals[s.Let.Name] = t
	case s.Var != nil:
		expr, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		typ := ""
		var t types.Type = types.AnyType{}
		if s.Var.Type != nil {
			typ = ": " + c.compileType(s.Var.Type)
			t = resolveTypeRef(s.Var.Type, c.env)
		}
		if typ == "" {
			t = c.inferExprType(s.Var.Value)
			if lt, ok := t.(types.ListType); ok && !containsAny(lt) {
				typ = ": [" + swiftType(lt.Elem) + "]"
			} else if mt, ok := t.(types.MapType); ok && !containsAny(mt) {
				typ = ": [" + swiftType(mt.Key) + ": " + swiftType(mt.Value) + "]"
			}
		}
		if typ == "" && expr == "[]" {
			if lt, ok := c.funcRet.(types.ListType); ok {
				typ = ": [" + swiftType(lt.Elem) + "]"
				t = types.ListType{Elem: lt.Elem}
			} else {
				typ = ": [Any]"
				t = types.ListType{Elem: types.AnyType{}}
			}
		}
		if typ == "" && expr == "[:]" {
			if mt, ok := c.funcRet.(types.MapType); ok {
				typ = ": [" + swiftType(mt.Key) + ": " + swiftType(mt.Value) + "]"
				t = types.MapType{Key: mt.Key, Value: mt.Value}
			} else {
				typ = ": [AnyHashable: Any]"
				t = types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
			}
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("var %s%s", s.Var.Name, typ))
		} else {
			c.writeln(fmt.Sprintf("var %s%s = %s", s.Var.Name, typ, expr))
		}
		if c.env != nil {
			c.env.SetVar(s.Var.Name, t, true)
		}
		c.locals[s.Var.Name] = t
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("return %s", expr))
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	default:
		// ignore unsupported statements
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s..<%s {", f.Name, start, end))
		if c.env != nil {
			c.env.SetVar(f.Name, types.IntType{}, true)
		}
		c.locals[f.Name] = types.IntType{}
		c.indent++
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		srcType := c.inferExprType(f.Source)
		if _, ok := srcType.(types.StringType); ok {
			if f.Name == "_" {
				c.writeln(fmt.Sprintf("for _ in %s {", src))
				c.indent++
				for _, st := range f.Body {
					if err := c.compileStmt(st); err != nil {
						return err
					}
				}
				c.indent--
				c.writeln("}")
				return nil
			}
			tmp := f.Name + "_ch"
			c.writeln(fmt.Sprintf("for %s in %s {", tmp, src))
			c.indent++
			c.writeln(fmt.Sprintf("let %s = String(%s)", f.Name, tmp))
			if c.env != nil {
				c.env.SetVar(f.Name, types.StringType{}, true)
			}
			c.locals[f.Name] = types.StringType{}
			for _, st := range f.Body {
				if err := c.compileStmt(st); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("}")
			return nil
		} else {
			c.writeln(fmt.Sprintf("for %s in %s {", f.Name, src))
			if lt, ok := srcType.(types.ListType); ok {
				if c.env != nil {
					c.env.SetVar(f.Name, lt.Elem, true)
				}
				c.locals[f.Name] = lt.Elem
			} else if mt, ok := srcType.(types.MapType); ok {
				if c.env != nil {
					c.env.SetVar(f.Name, mt.Key, true)
				}
				c.locals[f.Name] = mt.Key
			} else {
				if c.env != nil {
					c.env.SetVar(f.Name, types.AnyType{}, true)
				}
				c.locals[f.Name] = types.AnyType{}
			}
			c.indent++
			for _, st := range f.Body {
				if err := c.compileStmt(st); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("}")
			return nil
		}
	}
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if ifst.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(ifst.ElseIf)
	}
	if len(ifst.Else) > 0 {
		c.buf.WriteString(" else {")
		c.buf.WriteByte('\n')
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while %s {", cond))
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

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	name := a.Name
	for _, idx := range a.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		name = fmt.Sprintf("%s[%s]", name, iexpr)
	}
	value, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		if op.Op == "in" {
			if c.isMapPostfix(op.Right) {
				expr = fmt.Sprintf("%s[%s] != nil", rhs, expr)
			} else {
				expr = fmt.Sprintf("%s.contains(%s)", rhs, expr)
			}
		} else {
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, rhs)
		}
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = u.Ops[i] + expr
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
		} else if op.Index != nil {
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("%s.count", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if c.isStringPrimary(p.Target) {
					c.useSliceStr = true
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				} else {
					c.useSlice = true
					expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isStringPrimary(p.Target) {
					c.useIndexStr = true
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					if c.isMapPrimary(p.Target) {
						expr = fmt.Sprintf("%s[%s]!", expr, idx)
					} else if c.isListPrimary(p.Target) {
						c.useIndex = true
						expr = fmt.Sprintf("_index(%s, %s)", expr, idx)
					} else {
						expr = fmt.Sprintf("%s[%s]", expr, idx)
					}
				}
			}
		} else if op.Cast != nil {
			expr = fmt.Sprintf("%s as! %s", expr, c.compileType(op.Cast.Type))
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, item := range p.Map.Items {
			k, err := c.compileExpr(item.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(item.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		if len(items) == 0 {
			return "[:]", nil
		}
		return "[" + strings.Join(items, ", ") + "]", nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", f.Name, v)
		}
		return fmt.Sprintf("%s(%s)", p.Struct.Name, strings.Join(parts, ", ")), nil
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		switch p.Call.Func {
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			return fmt.Sprintf("%s.count", args[0]), nil
		case "count":
			if len(args) != 1 {
				return "", fmt.Errorf("count expects 1 arg")
			}
			return fmt.Sprintf("%s.count", args[0]), nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects 1 arg")
			}
			return fmt.Sprintf("String(%s)", args[0]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			c.useAvg = true
			return fmt.Sprintf("_avg(%s.map { Double($0) })", args[0]), nil
		case "now":
			if len(args) != 0 {
				return "", fmt.Errorf("now expects 0 args")
			}
			return "Int64(Date().timeIntervalSince1970 * 1_000_000_000)", nil
		case "json":
			if len(args) != 1 {
				return "", fmt.Errorf("json expects 1 arg")
			}
			c.useJSON = true
			return fmt.Sprintf("_json(%s)", args[0]), nil
		case "input":
			if len(args) != 0 {
				return "", fmt.Errorf("input expects 0 args")
			}
			return "readLine() ?? \"\"", nil
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
		}
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		paramType := "Any"
		if p.Type != nil {
			paramType = c.compileType(p.Type)
		}
		params[i] = fmt.Sprintf("%s: %s", p.Name, paramType)
	}
	ret := "Void"
	if fn.Return != nil {
		ret = c.compileType(fn.Return)
	}
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 1
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.buf = oldBuf
			c.indent = oldIndent
			return "", err
		}
		c.writeln("return " + expr)
	} else {
		for _, st := range fn.BlockBody {
			if err := c.compileStmt(st); err != nil {
				c.buf = oldBuf
				c.indent = oldIndent
				return "", err
			}
		}
	}
	bodyStr := c.buf.String()
	c.buf = oldBuf
	c.indent = oldIndent
	result := fmt.Sprintf("{ (%s) -> %s in\n%s}", strings.Join(params, ", "), ret, indentBlock(bodyStr, 1))
	return result, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "nil"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.useLoad = true
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "nil"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.useSave = true
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.useFetch = true
	return fmt.Sprintf("_fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 || q.Group != nil || q.Skip != nil || q.Take != nil {
		return "", fmt.Errorf("advanced query clauses not supported")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	// special case: simple sort and select by query variable
	if len(q.Froms) == 0 && q.Sort != nil {
		orig := c.env
		child := types.NewEnv(c.env)
		elem := c.inferExprType(q.Source)
		if lt, ok := elem.(types.ListType); ok {
			elem = lt.Elem
		}
		child.SetVar(q.Var, elem, true)
		c.env = child
		sortExpr, err := c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
		selExpr, err := c.compileExpr(q.Select)
		c.env = orig
		if err != nil {
			return "", err
		}
		if sortExpr == q.Var && selExpr == q.Var {
			return fmt.Sprintf("%s.sorted()", src), nil
		}
		return "", fmt.Errorf("advanced query clauses not supported")
	}

	// simple map with optional cross joins
	orig := c.env
	child := types.NewEnv(c.env)
	elem := c.inferExprType(q.Source)
	if lt, ok := elem.(types.ListType); ok {
		elem = lt.Elem
	}
	child.SetVar(q.Var, elem, true)
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		ft := c.inferExprType(f.Src)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
		fromSrcs[i] = fs
	}
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	cond := ""
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	elemType := c.inferExprType(q.Select)
	c.env = orig
	resType := swiftType(elemType)
	if resType == "" {
		resType = "Any"
	}

	var b strings.Builder
	b.WriteString("({\n")
	b.WriteString(fmt.Sprintf("\tvar _res: [%s] = []\n", resType))
	b.WriteString(fmt.Sprintf("\tfor %s in %s {\n", q.Var, src))
	indent := "\t\t"
	for i, f := range q.Froms {
		b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, f.Var, fromSrcs[i]))
		indent += "\t"
	}
	if cond != "" {
		b.WriteString(fmt.Sprintf("%sif %s {\n", indent, cond))
		indent += "\t"
	}
	b.WriteString(fmt.Sprintf("%s_res.append(%s)\n", indent, sel))
	if cond != "" {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for range q.Froms {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("\t}\n")
	b.WriteString("\treturn _res\n")
	b.WriteString("}())")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("({\n")
	b.WriteString("\tlet _t = " + target + "\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + "\n")
			b.WriteString("}())")
			return b.String(), nil
		}
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				b.WriteString("\tif let v = _t as? " + sanitizeName(call.Func) + " {\n")
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						field := sanitizeName(st.Order[idx])
						b.WriteString(fmt.Sprintf("\t\tlet %s = v.%s\n", sanitizeName(id), field))
					}
				}
				b.WriteString("\t\treturn " + res + "\n")
				b.WriteString("\t}\n")
				continue
			}
		}
		if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				b.WriteString("\tif _t is " + sanitizeName(ident) + " {\n")
				b.WriteString("\t\treturn " + res + "\n")
				b.WriteString("\t}\n")
				continue
			}
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		b.WriteString("\tif _t == " + pat + " {\n")
		b.WriteString("\t\treturn " + res + "\n")
		b.WriteString("\t}\n")
	}
	b.WriteString("\treturn nil\n")
	b.WriteString("}())")
	return b.String(), nil
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		if t, ok := c.locals[name]; ok {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
		if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isStringExpr(p.Group)
	}
	return false
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	return c.isStringUnary(e.Binary.Left)
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	return c.isStringPrimary(p.Target)
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Map != nil:
		return true
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		if t, ok := c.locals[name]; ok {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
		if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isMapExpr(p.Group)
	}
	return false
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	return c.isMapUnary(e.Binary.Left)
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isMapPostfix(u.Value)
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	return c.isMapPrimary(p.Target)
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.List != nil:
		return true
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		if t, ok := c.locals[name]; ok {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
		if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isListExpr(p.Group)
	}
	return false
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	return c.isListUnary(e.Binary.Left)
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isListPostfix(u.Value)
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	return c.isListPrimary(p.Target)
}

// paramAssigned reports whether a function parameter is assigned within the
// given body. Only direct assignments are considered.
func paramAssigned(body []*parser.Statement, name string) bool {
	for _, st := range body {
		if stmtAssignsVar(st, name) {
			return true
		}
	}
	return false
}

func stmtAssignsVar(s *parser.Statement, name string) bool {
	switch {
	case s.Assign != nil:
		// treat assignments to a parameter via indexing as modifying
		// the parameter so it should be mutable
		return s.Assign.Name == name
	case s.For != nil:
		return paramAssigned(s.For.Body, name)
	case s.While != nil:
		return paramAssigned(s.While.Body, name)
	case s.If != nil:
		if paramAssigned(s.If.Then, name) {
			return true
		}
		if s.If.ElseIf != nil && stmtAssignsVar(&parser.Statement{If: s.If.ElseIf}, name) {
			return true
		}
		if len(s.If.Else) > 0 {
			return paramAssigned(s.If.Else, name)
		}
	}
	return false
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln("expect(" + expr + ")")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln("func " + name + "() {")
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) writeExpectFunc(prog *parser.Program) {
	if !hasExpect(prog) {
		return
	}
	c.writeln("func expect(_ cond: Bool) {")
	c.indent++
	c.writeln("if !cond { fatalError(\"expect failed\") }")
	c.indent--
	c.writeln("}")
	c.writeln("")
}

func hasTest(p *parser.Program) bool {
	for _, s := range p.Statements {
		if s.Test != nil {
			return true
		}
	}
	return false
}

func hasExpect(p *parser.Program) bool {
	for _, s := range p.Statements {
		if containsExpect(s) {
			return true
		}
	}
	return false
}

func containsExpect(s *parser.Statement) bool {
	switch {
	case s.Expect != nil:
		return true
	case s.If != nil:
		for _, t := range s.If.Then {
			if containsExpect(t) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if containsExpect(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if containsExpect(t) {
				return true
			}
		}
	case s.For != nil:
		for _, t := range s.For.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.While != nil:
		for _, t := range s.While.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if containsExpect(t) {
				return true
			}
		}
	}
	return false
}
