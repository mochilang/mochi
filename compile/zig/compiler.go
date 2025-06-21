package zigcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Zig source code (very small subset).
type Compiler struct {
	buf               bytes.Buffer
	indent            int
	env               *types.Env
	tmpCount          int
	imports           map[string]string
	needsAvgInt       bool
	needsAvgFloat     bool
	needsInListInt    bool
	needsInListString bool
	needsSetOps       bool
	needsJSON         bool
	needsIndex        bool
	needsIndexString  bool
	needsSlice        bool
	needsSliceString  bool
	needsReduce       bool
	needsEqual        bool
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, imports: map[string]string{}}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

// Compile converts a Mochi program to Zig.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// compile functions and test blocks first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Test != nil {
			if err := c.compileTest(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main body
	c.writeln("pub fn main() void {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s, false); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	// prepend import
	body := c.buf.String()
	c.buf.Reset()
	c.writeln("const std = @import(\"std\");")
	for alias, path := range c.imports {
		c.writeln(fmt.Sprintf("const %s = @import(\"%s\");", alias, path))
	}
	c.writeln("")
	if c.needsAvgInt {
		c.writeln("fn _avg_int(v: []const i32) f64 {")
		c.indent++
		c.writeln("if (v.len == 0) return 0;")
		c.writeln("var sum: f64 = 0;")
		c.writeln("for (v) |it| { sum += @floatFromInt(it); }")
		c.writeln("return sum / @as(f64, @floatFromInt(v.len));")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsAvgFloat {
		c.writeln("fn _avg_float(v: []const f64) f64 {")
		c.indent++
		c.writeln("if (v.len == 0) return 0;")
		c.writeln("var sum: f64 = 0;")
		c.writeln("for (v) |it| { sum += it; }")
		c.writeln("return sum / @as(f64, @floatFromInt(v.len));")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsInListInt {
		c.writeln("fn _contains_list_int(v: []const i32, item: i32) bool {")
		c.indent++
		c.writeln("for (v) |it| { if (it == item) return true; }")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsInListString {
		c.writeln("fn _contains_list_string(v: []const []const u8, item: []const u8) bool {")
		c.indent++
		c.writeln("for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSetOps {
		c.writeln("fn _contains(comptime T: type, v: []const T, item: T) bool {")
		c.indent++
		c.writeln("for (v) |it| { if (std.meta.eql(it, item)) return true; }")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _union_all(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { res.append(it) catch unreachable; }")
		c.writeln("for (b) |it| { res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _union(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { res.append(it) catch unreachable; }")
		c.writeln("for (b) |it| { if (!_contains(T, res.items, it)) res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _except(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { if (!_contains(T, b, it)) res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")

		c.writeln("fn _intersect(comptime T: type, a: []const T, b: []const T) []T {")
		c.indent++
		c.writeln("var res = std.ArrayList(T).init(std.heap.page_allocator);")
		c.writeln("defer res.deinit();")
		c.writeln("for (a) |it| { if (_contains(T, b, it) and !_contains(T, res.items, it)) res.append(it) catch unreachable; }")
		c.writeln("return res.toOwnedSlice() catch unreachable;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsJSON {
		c.writeln("fn _json(v: anytype) void {")
		c.indent++
		c.writeln("var buf = std.ArrayList(u8).init(std.heap.page_allocator);")
		c.writeln("defer buf.deinit();")
		c.writeln("std.json.stringify(v, .{}, buf.writer()) catch unreachable;")
		c.writeln("std.debug.print(\"{s}\\n\", .{buf.items});")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsIndex {
		c.writeln("fn _index_list(comptime T: type, v: []const T, i: i32) T {")
		c.indent++
		c.writeln("var idx = i;")
		c.writeln("const n: i32 = @as(i32, @intCast(v.len));")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 or idx >= n) @panic(\"index out of range\");")
		c.writeln("return v[@as(usize, @intCast(idx))];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsIndexString {
		c.writeln("fn _index_string(s: []const u8, i: i32) []const u8 {")
		c.indent++
		c.writeln("var idx = i;")
		c.writeln("const n: i32 = @as(i32, @intCast(s.len));")
		c.writeln("if (idx < 0) idx += n;")
		c.writeln("if (idx < 0 or idx >= n) @panic(\"index out of range\");")
		c.writeln("const u = @as(usize, @intCast(idx));")
		c.writeln("return s[u..u+1];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSlice {
		c.writeln("fn _slice_list(comptime T: type, v: []const T, start: i32, end: i32) []const T {")
		c.indent++
		c.writeln("var s = start;")
		c.writeln("var e = end;")
		c.writeln("const n: i32 = @as(i32, @intCast(v.len));")
		c.writeln("if (s < 0) s += n;")
		c.writeln("if (e < 0) e += n;")
		c.writeln("if (s < 0) s = 0;")
		c.writeln("if (e > n) e = n;")
		c.writeln("if (e < s) e = s;")
		c.writeln("return v[@as(usize, @intCast(s))..@as(usize, @intCast(e))];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSliceString {
		c.writeln("fn _slice_string(s: []const u8, start: i32, end: i32) []const u8 {")
		c.indent++
		c.writeln("var sidx = start;")
		c.writeln("var eidx = end;")
		c.writeln("const n: i32 = @as(i32, @intCast(s.len));")
		c.writeln("if (sidx < 0) sidx += n;")
		c.writeln("if (eidx < 0) eidx += n;")
		c.writeln("if (sidx < 0) sidx = 0;")
		c.writeln("if (eidx > n) eidx = n;")
		c.writeln("if (eidx < sidx) eidx = sidx;")
		c.writeln("return s[@as(usize, @intCast(sidx))..@as(usize, @intCast(eidx))];")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsReduce {
		c.writeln("fn _reduce(comptime T: type, v: []const T, init: T, f: fn (T, T) T) T {")
		c.indent++
		c.writeln("var acc: T = init;")
		c.writeln("for (v) |it| {")
		c.indent++
		c.writeln("acc = f(acc, it);")
		c.indent--
		c.writeln("}")
		c.writeln("return acc;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsEqual {
		c.writeln("fn _equal(a: anytype, b: anytype) bool {")
		c.indent++
		c.writeln("if (@TypeOf(a) != @TypeOf(b)) return false;")
		c.writeln("return switch (@typeInfo(@TypeOf(a))) {")
		c.indent++
		c.writeln(".Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),")
		c.writeln("else => a == b,")
		c.indent--
		c.writeln("};")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	c.buf.WriteString(body)
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := sanitizeName(fn.Name)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := c.zigType(p.Type)
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ)
	}
	ret := "void"
	if fn.Return != nil {
		ret = c.zigType(fn.Return)
	}
	c.writeln(fmt.Sprintf("fn %s(%s) %s {", name, strings.Join(params, ", "), ret))
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileTest(tb *parser.TestBlock) error {
	c.writeln(fmt.Sprintf("test \"%s\" {", tb.Name))
	c.indent++
	for _, st := range tb.Body {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) zigType(t *parser.TypeRef) string {
	if t == nil {
		return "i32"
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return "[]const " + c.zigType(t.Generic.Args[0])
	}
	if t.Generic != nil && t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
		return fmt.Sprintf("std.AutoHashMap(%s, %s)", c.zigType(t.Generic.Args[0]), c.zigType(t.Generic.Args[1]))
	}
	if t.Simple == nil {
		return "i32"
	}
	switch *t.Simple {
	case "int":
		return "i32"
	case "float":
		return "f64"
	case "bool":
		return "bool"
	case "string":
		return "[]const u8"
	}
	return "i32"
}

func (c *Compiler) compileStmt(s *parser.Statement, inFun bool) error {
	switch {
	case s.Let != nil:
		name := sanitizeName(s.Let.Name)
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if s.Let.Type != nil {
				typ = c.resolveTypeRef(s.Let.Type)
			} else if s.Let.Value != nil {
				typ = c.inferExprType(s.Let.Value)
			} else if old, err := c.env.GetVar(s.Let.Name); err == nil {
				typ = old
			}
			c.env.SetVar(s.Let.Name, typ, false)
		}
		if s.Let.Value != nil && isEmptyMapExpr(s.Let.Value) {
			keyT := "i32"
			valT := "i32"
			if mt, ok := typ.(types.MapType); ok {
				keyT = zigTypeOf(mt.Key)
				valT = zigTypeOf(mt.Value)
			}
			c.writeln(fmt.Sprintf("var %s = std.AutoHashMap(%s, %s).init(std.heap.page_allocator);", name, keyT, valT))
			return nil
		}
		val := "0"
		if s.Let.Value != nil {
			v, err := c.compileExpr(s.Let.Value, false)
			if err != nil {
				return err
			}
			val = v
		}
		c.writeln(fmt.Sprintf("const %s: %s = %s;", name, zigTypeOf(typ), val))
		return nil
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Import != nil:
		return c.addImport(s.Import)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		if isListLiteralExpr(s.Return.Value) {
			ll := s.Return.Value.Binary.Left.Value.Target.List
			v, err := c.compileListLiteral(ll, false)
			if err != nil {
				return err
			}
			c.writeln("return " + v + ";")
		} else {
			v, err := c.compileExpr(s.Return.Value, true)
			if err != nil {
				return err
			}
			c.writeln("return " + v + ";")
		}
	case s.For != nil:
		start, err := c.compileExpr(s.For.Source, false)
		if err != nil {
			return err
		}
		end := ""
		if s.For.RangeEnd != nil {
			end, err = c.compileExpr(s.For.RangeEnd, false)
			if err != nil {
				return err
			}
		}
		name := sanitizeName(s.For.Name)
		if s.For.RangeEnd != nil {
			c.writeln(fmt.Sprintf("for (%s .. %s) |%s| {", start, end, name))
		} else {
			c.writeln(fmt.Sprintf("for (%s) |%s| {", start, name))
		}
		c.indent++
		for _, st := range s.For.Body {
			if err := c.compileStmt(st, inFun); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Expr != nil:
		v, err := c.compileExpr(s.Expr.Expr, false)
		if err != nil {
			return err
		}
		c.writeln(v + ";")
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Test != nil:
		return c.compileTest(s.Test)
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value, false)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("try std.testing.expect(%s);", expr))
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond, false)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileStmt(st, true); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond, false)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent++
	for _, st := range stmt.Body {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target, false)
	if err != nil {
		return "", err
	}
	expr := "0"
	for i := len(m.Cases) - 1; i >= 0; i-- {
		cs := m.Cases[i]
		res, err := c.compileExpr(cs.Result, false)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			expr = res
			continue
		}
		pat, err := c.compileExpr(cs.Pattern, false)
		if err != nil {
			return "", err
		}
		c.needsEqual = true
		expr = fmt.Sprintf("if (_equal(%s, %s)) %s else (%s)", target, pat, res, expr)
	}
	return expr, nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond, false)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExpr(ie.Then, false)
	if err != nil {
		return "", err
	}
	elseVal := "void"
	if ie.ElseIf != nil {
		v, err := c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
		elseVal = v
	} else if ie.Else != nil {
		v, err := c.compileExpr(ie.Else, false)
		if err != nil {
			return "", err
		}
		elseVal = v
	}
	return fmt.Sprintf("if (%s) (%s) else (%s)", cond, thenVal, elseVal), nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	name := sanitizeName(st.Name)
	var typ types.Type = types.AnyType{}
	if c.env != nil {
		if st.Type != nil {
			typ = c.resolveTypeRef(st.Type)
		} else if st.Value != nil {
			typ = c.inferExprType(st.Value)
		} else if old, err := c.env.GetVar(st.Name); err == nil {
			typ = old
		}
		c.env.SetVar(st.Name, typ, true)
	}
	if st.Value != nil && isEmptyListExpr(st.Value) {
		elem := "i32"
		if lt, ok := typ.(types.ListType); ok {
			elem = strings.TrimPrefix(zigTypeOf(lt.Elem), "[]const ")
		}
		c.writeln(fmt.Sprintf("var %s = std.ArrayList(%s).init(std.heap.page_allocator);", name, elem))
		return nil
	}
	if st.Value != nil && isEmptyMapExpr(st.Value) {
		keyT := "i32"
		valT := "i32"
		if mt, ok := typ.(types.MapType); ok {
			keyT = zigTypeOf(mt.Key)
			valT = zigTypeOf(mt.Value)
		}
		c.writeln(fmt.Sprintf("var %s = std.AutoHashMap(%s, %s).init(std.heap.page_allocator);", name, keyT, valT))
		return nil
	}
	val := "0"
	if st.Value != nil {
		v, err := c.compileExpr(st.Value, false)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln(fmt.Sprintf("var %s: %s = %s;", name, zigTypeOf(typ), val))
	return nil
}

func (c *Compiler) addImport(im *parser.ImportStmt) error {
	if im.Lang != nil && *im.Lang != "zig" {
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	path := strings.Trim(im.Path, "\"")
	if c.imports == nil {
		c.imports = map[string]string{}
	}
	c.imports[alias] = path
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	name := sanitizeName(st.Name)
	// check for append pattern: x = x + [val]
	if elem, ok := isSelfAppend(st); ok {
		v, err := c.compileExpr(elem, false)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("try %s.append(@as(i32,@intCast(%s)));", name, v))
		return nil
	}
	lhs := name
	if len(st.Index) == 1 && st.Index[0].Colon == nil && c.isMapVar(st.Name) {
		key, err := c.compileExpr(st.Index[0].Start, false)
		if err != nil {
			return err
		}
		val, err := c.compileExpr(st.Value, false)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("_ = %s.put(%s, %s) catch unreachable;", lhs, key, val))
		return nil
	}
	for i, idx := range st.Index {
		ie, err := c.compileExpr(idx.Start, false)
		if err != nil {
			return err
		}
		if i == 0 && idx.Colon == nil && c.isListVar(st.Name) {
			lhs = fmt.Sprintf("%s.items[%s]", lhs, ie)
		} else {
			lhs = fmt.Sprintf("%s[%s]", lhs, ie)
		}
	}
	rhs, err := c.compileExpr(st.Value, false)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr, asReturn bool) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary, asReturn)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr, asReturn bool) (string, error) {
	left, err := c.compileUnary(b.Left, asReturn)
	if err != nil {
		return "", err
	}
	expr := left
	leftIsStr := c.isStringUnary(b.Left)
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right, asReturn)
		if err != nil {
			return "", err
		}
		opStr := op.Op
		rightIsStr := c.isStringPostfix(op.Right)
		if (opStr == "==" || opStr == "!=") && (leftIsStr || rightIsStr) {
			cmp := fmt.Sprintf("std.mem.eql(u8, %s, %s)", expr, right)
			if opStr == "!=" {
				cmp = "!" + cmp
			}
			expr = cmp
			leftIsStr = false
			continue
		}
		if opStr == "in" {
			if c.isStringListPostfix(op.Right) {
				c.needsInListString = true
				expr = fmt.Sprintf("_contains_list_string(%s, %s)", right, expr)
			} else {
				c.needsInListInt = true
				expr = fmt.Sprintf("_contains_list_int(%s, %s)", right, expr)
			}
			leftIsStr = false
			continue
		}
		if opStr == "union" && op.All {
			opStr = "union_all"
		}
		if opStr == "union" || opStr == "union_all" || opStr == "except" || opStr == "intersect" {
			elem := c.listElemTypeUnary(b.Left)
			c.needsSetOps = true
			expr = fmt.Sprintf("_%s(%s, %s, %s)", opStr, elem, expr, right)
			leftIsStr = false
			continue
		}
		switch opStr {
		case "&&":
			opStr = "and"
		case "||":
			opStr = "or"
		}
		if opStr == "%" {
			expr = fmt.Sprintf("@mod(%s, %s)", expr, right)
		} else {
			expr = fmt.Sprintf("(%s %s %s)", expr, opStr, right)
		}
		leftIsStr = false
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary, asReturn bool) (string, error) {
	val, err := c.compilePostfix(u.Value, asReturn)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("%s%s", u.Ops[i], val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr, asReturn bool) (string, error) {
	expr, err := c.compilePrimary(p.Target, asReturn)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := c.compileExpr(op.Index.Start, false)
				if err != nil {
					return "", err
				}
				if c.isMapPostfix(p) {
					expr = fmt.Sprintf("(%s.get(%s) orelse unreachable)", expr, idx)
				} else if c.isStringPostfix(p) {
					c.needsIndexString = true
					expr = fmt.Sprintf("_index_string(%s, %s)", expr, idx)
				} else if c.isListPostfix(p) {
					elem := c.listElemTypePostfix(p)
					c.needsIndex = true
					expr = fmt.Sprintf("_index_list(%s, %s, %s)", elem, expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
			} else {
				start := "0"
				end := ""
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start, false)
					if err != nil {
						return "", err
					}
					start = s
				}
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End, false)
					if err != nil {
						return "", err
					}
					end = e
				} else {
					end = fmt.Sprintf("%s.len", expr)
				}
				if c.isStringPostfix(p) {
					c.needsSliceString = true
					expr = fmt.Sprintf("_slice_string(%s, %s, %s)", expr, start, end)
				} else if c.isListPostfix(p) {
					elem := c.listElemTypePostfix(p)
					c.needsSlice = true
					expr = fmt.Sprintf("_slice_list(%s, %s, %s, %s)", elem, expr, start, end)
				} else {
					expr = fmt.Sprintf("%s[%s..%s]", expr, start, end)
				}
			}
		} else if op.Call != nil {
			expr, err = c.compileCallOp(expr, op.Call)
			if err != nil {
				return "", err
			}
		} else if op.Cast != nil {
			typ := c.zigType(op.Cast.Type)
			expr = fmt.Sprintf("@as(%s, %s)", typ, expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compileCallOp(receiver string, call *parser.CallOp) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a, false)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", receiver, strings.Join(args, ", ")), nil
}

func (c *Compiler) compilePrimary(p *parser.Primary, asReturn bool) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		} else if asReturn && c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if m, _ := c.env.IsMutable(p.Selector.Root); m {
					if _, ok := t.(types.ListType); ok {
						name += ".items"
					}
				}
			}
		}
		return name, nil
	case p.List != nil:
		return c.compileListLiteral(p.List, !asReturn)
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group, asReturn)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "0", nil
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	if name == "len" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s).len", arg), nil
	}
	if name == "count" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s).len", arg), nil
	}
	if name == "avg" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if c.isFloatListExpr(call.Args[0]) {
			c.needsAvgFloat = true
			return fmt.Sprintf("_avg_float(%s)", arg), nil
		}
		c.needsAvgInt = true
		return fmt.Sprintf("_avg_int(%s)", arg), nil
	}
	if name == "reduce" && len(call.Args) == 3 {
		listArg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		fnArg, err := c.compileExpr(call.Args[1], false)
		if err != nil {
			return "", err
		}
		initArg, err := c.compileExpr(call.Args[2], false)
		if err != nil {
			return "", err
		}
		elem := c.listElemTypeUnary(call.Args[0].Binary.Left)
		c.needsReduce = true
		return fmt.Sprintf("_reduce(%s, %s, %s, %s)", elem, listArg, initArg, fnArg), nil
	}
	if name == "str" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("std.fmt.allocPrint(std.heap.page_allocator, \"{d}\", .{%s}) catch unreachable", arg), nil
	}
	if name == "now" && len(call.Args) == 0 {
		return "std.time.nanoTimestamp()", nil
	}
	if name == "json" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		c.needsJSON = true
		return fmt.Sprintf("_json(%s)", arg), nil
	}
	if name == "print" {
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, err := c.compileExpr(a, false)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		fmtParts := make([]string, len(args))
		for i := range args {
			if c.isStringExpr(call.Args[i]) {
				fmtParts[i] = "{s}"
			} else {
				fmtParts[i] = "{any}"
			}
		}
		format := strings.Join(fmtParts, " ") + "\\n"
		return fmt.Sprintf("std.debug.print(\"%s\", .{%s})", format, strings.Join(args, ", ")), nil
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a, false)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("@as(i32,@intCast(%d))", *l.Int), nil
	case l.Float != nil:
		return strconv.FormatFloat(*l.Float, 'f', -1, 64), nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	}
	return "0", nil
}

func (c *Compiler) compileListLiteral(list *parser.ListLiteral, asRef bool) (string, error) {
	elems := make([]string, len(list.Elems))
	elemType := "i32"
	if len(list.Elems) > 0 {
		if c.isStringExpr(list.Elems[0]) {
			elemType = "[]const u8"
		} else if c.isFloatExpr(list.Elems[0]) {
			elemType = "f64"
		} else if c.isBoolExpr(list.Elems[0]) {
			elemType = "bool"
		}
	}
	for i, e := range list.Elems {
		v, err := c.compileExpr(e, false)
		if err != nil {
			return "", err
		}
		elems[i] = v
	}
	if asRef {
		return fmt.Sprintf("&[_]%s{%s}", elemType, strings.Join(elems, ", ")), nil
	}
	return fmt.Sprintf("[_]%s{%s}", elemType, strings.Join(elems, ", ")), nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	keyType := "i32"
	valType := "i32"
	if len(m.Items) > 0 {
		if c.isStringExpr(m.Items[0].Key) {
			keyType = "[]const u8"
		} else if c.isFloatExpr(m.Items[0].Key) {
			keyType = "f64"
		} else if c.isBoolExpr(m.Items[0].Key) {
			keyType = "bool"
		}
		if c.isStringExpr(m.Items[0].Value) {
			valType = "[]const u8"
		} else if c.isFloatExpr(m.Items[0].Value) {
			valType = "f64"
		} else if c.isBoolExpr(m.Items[0].Value) {
			valType = "bool"
		}
	}
	var b strings.Builder
	b.WriteString("blk: { var m = std.AutoHashMap(" + keyType + ", " + valType + ").init(std.heap.page_allocator); ")
	for _, it := range m.Items {
		k, err := c.compileExpr(it.Key, false)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value, false)
		if err != nil {
			return "", err
		}
		b.WriteString("m.put(" + k + ", " + v + ") catch unreachable; ")
	}
	b.WriteString("break :blk m; }")
	return b.String(), nil
}

func isListLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return isListLiteralUnary(e.Binary.Left)
}

func isListLiteralUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isListLiteralPostfix(u.Value)
}

func isListLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListLiteralPrimary(p.Target)
}

func isListLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.List != nil
}

func isEmptyListExpr(e *parser.Expr) bool {
	if !isListLiteralExpr(e) {
		return false
	}
	ll := e.Binary.Left.Value.Target.List
	return len(ll.Elems) == 0
}

func isMapLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return isMapLiteralUnary(e.Binary.Left)
}

func isMapLiteralUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isMapLiteralPostfix(u.Value)
}

func isMapLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isMapLiteralPrimary(p.Target)
}

func isMapLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.Map != nil
}

func isEmptyMapExpr(e *parser.Expr) bool {
	if !isMapLiteralExpr(e) {
		return false
	}
	ml := e.Binary.Left.Value.Target.Map
	return len(ml.Items) == 0
}

func isSelfAppend(st *parser.AssignStmt) (*parser.Expr, bool) {
	if st == nil || st.Value == nil || st.Value.Binary == nil {
		return nil, false
	}
	b := st.Value.Binary
	if len(b.Right) != 1 || b.Right[0].Op != "+" {
		return nil, false
	}
	left := b.Left
	if left == nil || left.Value == nil || left.Value.Target == nil {
		return nil, false
	}
	if left.Value.Target.Selector == nil || left.Value.Target.Selector.Root != st.Name {
		return nil, false
	}
	if len(left.Value.Ops) > 0 {
		return nil, false
	}
	r := b.Right[0].Right
	if r == nil || r.Target == nil || r.Target.List == nil || len(r.Ops) > 0 {
		return nil, false
	}
	if len(r.Target.List.Elems) != 1 {
		return nil, false
	}
	return r.Target.List.Elems[0], true
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isStringUnary(e.Binary.Left)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isStringPrimary(p.Target)
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
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
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isMapPrimary(p.Target)
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isFloatExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isFloatUnary(e.Binary.Left)
}

func (c *Compiler) isFloatUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isFloatPostfix(u.Value)
}

func (c *Compiler) isFloatPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isFloatPrimary(p.Target)
}

func (c *Compiler) isFloatPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Float != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isFloatListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	p := e.Binary.Left.Value
	if p == nil {
		return false
	}
	if p.Target != nil && p.Target.List != nil && len(p.Target.List.Elems) > 0 {
		return c.isFloatExpr(p.Target.List.Elems[0])
	}
	if p.Target != nil && p.Target.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.FloatType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isBoolExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isBoolUnary(e.Binary.Left)
}

func (c *Compiler) isBoolUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isBoolPostfix(u.Value)
}

func (c *Compiler) isBoolPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isBoolPrimary(p.Target)
}

func (c *Compiler) isBoolPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Bool != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.BoolType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isStringListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isStringListPrimary(p.Target)
}

func (c *Compiler) isStringListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.List != nil && len(p.List.Elems) > 0 {
		return c.isStringExpr(p.List.Elems[0])
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
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
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isListPrimary(p.Target)
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) listElemTypeUnary(u *parser.Unary) string {
	if u == nil {
		return "i32"
	}
	return c.listElemTypePostfix(u.Value)
}

func (c *Compiler) listElemTypePostfix(p *parser.PostfixExpr) string {
	if p == nil {
		return "i32"
	}
	if p.Target != nil {
		if p.Target.List != nil && len(p.Target.List.Elems) > 0 {
			first := p.Target.List.Elems[0]
			switch {
			case c.isStringExpr(first):
				return "[]const u8"
			case c.isFloatExpr(first):
				return "f64"
			case c.isBoolExpr(first):
				return "bool"
			default:
				return "i32"
			}
		}
		if p.Target.Selector != nil && c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if lt, ok := t.(types.ListType); ok {
					switch lt.Elem.(type) {
					case types.StringType:
						return "[]const u8"
					case types.FloatType:
						return "f64"
					case types.BoolType:
						return "bool"
					default:
						return "i32"
					}
				}
			}
		}
	}
	return "i32"
}

var zigReserved = map[string]bool{
	"fn": true, "var": true, "const": true, "pub": true, "return": true,
	"for": true, "while": true, "if": true, "else": true,
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	if zigReserved[s] || isZigTypeName(s) {
		s = "_" + s
	}
	return s
}

func isZigTypeName(name string) bool {
	if len(name) < 2 {
		return false
	}
	switch name[0] {
	case 'i', 'u', 'f':
		for i := 1; i < len(name); i++ {
			if name[i] < '0' || name[i] > '9' {
				return false
			}
		}
		return true
	}
	return false
}

func (c *Compiler) isMapVar(name string) bool {
	if c.env == nil {
		return false
	}
	if t, err := c.env.GetVar(name); err == nil {
		if _, ok := t.(types.MapType); ok {
			return true
		}
	}
	return false
}

func (c *Compiler) isListVar(name string) bool {
	if c.env == nil {
		return false
	}
	if t, err := c.env.GetVar(name); err == nil {
		if _, ok := t.(types.ListType); ok {
			return true
		}
	}
	return false
}
