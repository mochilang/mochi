package cppcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// ordered helper names ensures deterministic output
var helperOrder = []string{"indexString", "sliceVec", "sliceStr", "fmtVec", "groupBy", "reduce"}

// helperCode contains the C++ source for each optional runtime helper
var helperCode = map[string][]string{
	"indexString": {
		"string _indexString(const string& s, int i) {",
		"\tint n = s.size();",
		"\tif (i < 0) i += n;",
		"\tif (i < 0 || i >= n) throw std::out_of_range(\"index out of range\");",
		"\treturn string(1, s[i]);",
		"}",
	},
	"sliceVec": {
		"template<typename T> vector<T> _slice(const vector<T>& v, int start, int end) {",
		"\tint n = v.size();",
		"\tif (start < 0) start += n;",
		"\tif (end < 0) end += n;",
		"\tif (start < 0) start = 0;",
		"\tif (end > n) end = n;",
		"\tif (end < start) end = start;",
		"\treturn vector<T>(v.begin() + start, v.begin() + end);",
		"}",
	},
	"sliceStr": {
		"string _sliceString(const string& s, int start, int end) {",
		"\tint n = s.size();",
		"\tif (start < 0) start += n;",
		"\tif (end < 0) end += n;",
		"\tif (start < 0) start = 0;",
		"\tif (end > n) end = n;",
		"\tif (end < start) end = start;",
		"\treturn s.substr(start, end - start);",
		"}",
	},
	"fmtVec": {
		"template<typename T> string _fmtVec(const vector<T>& v) {",
		"\tstringstream ss;",
		"\tss << '[';",
		"\tfor (size_t i = 0; i < v.size(); i++) {",
		"\t\tif (i > 0) ss << ' ';",
		"\t\tss << v[i];",
		"\t}",
		"\tss << ']';",
		"\treturn ss.str();",
		"}",
	},
	"groupBy": {
		"template<typename Src, typename KeyFunc> auto _group_by(const Src& src, KeyFunc keyfn) {",
		"\tusing ElemT = typename std::decay<decltype(*std::begin(src))>::type;",
		"\tusing KeyT = decltype(keyfn(*std::begin(src)));",
		"\tstruct _Group { KeyT Key; vector<ElemT> Items; };",
		"\tunordered_map<KeyT, _Group> groups;",
		"\tvector<KeyT> order;",
		"\tfor (const auto& it : src) {",
		"\t\tKeyT k = keyfn(it);",
		"\t\tif (!groups.count(k)) { groups[k] = _Group{k, {}}; order.push_back(k); }",
		"\t\tgroups[k].Items.push_back(it);",
		"\t}",
		"\tvector<_Group> res;",
		"\tfor (const auto& k : order) res.push_back(groups[k]);",
		"\treturn res;",
		"}",
	},
	"reduce": {
		"template<typename Src, typename Fn, typename Acc> Acc _reduce(const Src& src, Fn fn, Acc acc) {",
		"\tfor (const auto& it : src) {",
		"\t\tacc = fn(acc, it);",
		"\t}",
		"\treturn acc;",
		"}",
	},
}

// Compiler translates a Mochi AST into minimal C++ source code.
type Compiler struct {
	buf      bytes.Buffer
	indent   int
	env      *types.Env
	helpers  map[string]bool
	vars     []map[string]string
	tmpCount int
}

func New(env *types.Env) *Compiler {
	return &Compiler{
		env:      env,
		helpers:  map[string]bool{},
		vars:     []map[string]string{{}},
		tmpCount: 0,
	}
}

func (c *Compiler) pushScope() { c.vars = append(c.vars, map[string]string{}) }
func (c *Compiler) popScope()  { c.vars = c.vars[:len(c.vars)-1] }
func (c *Compiler) setVar(name, typ string) {
	if len(c.vars) == 0 {
		c.vars = append(c.vars, map[string]string{})
	}
	c.vars[len(c.vars)-1][name] = typ
}
func (c *Compiler) getVar(name string) (string, bool) {
	for i := len(c.vars) - 1; i >= 0; i-- {
		if t, ok := c.vars[i][name]; ok {
			if t != "auto" {
				return t, true
			}
			break
		}
	}
	if typ, err := c.env.GetVar(name); err == nil {
		return c.cppTypeRef(typ), true
	}
	return "", false
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

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// reset state to avoid leaking helpers between compilations
	c.helpers = map[string]bool{}

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent = 0

	// type declarations
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("int main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")

	bodyBytes := c.buf.Bytes()
	c.buf = oldBuf
	c.indent = 0
	c.writeln("#include <bits/stdc++.h>")
	c.writeln("using namespace std;")
	c.writeln("")
	c.writeHelpers()
	c.buf.Write(bodyBytes)
	if !bytes.HasSuffix(c.buf.Bytes(), []byte("\n")) {
		c.writeln("")
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) cppType(t *parser.TypeRef) string {
	if t == nil {
		return "void"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "double"
		case "bool":
			return "bool"
		case "string":
			return "string"
		}
		return *t.Simple
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return "vector<" + c.cppType(t.Generic.Args[0]) + ">"
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return "unordered_map<" + c.cppType(t.Generic.Args[0]) + ", " + c.cppType(t.Generic.Args[1]) + ">"
		}
	}
	return "auto"
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	ret := c.cppType(fn.Return)
	c.writeIndent()
	c.buf.WriteString(ret + " ")
	c.buf.WriteString(fn.Name)
	c.buf.WriteByte('(')
	c.pushScope()
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(c.cppType(p.Type))
		c.buf.WriteByte(' ')
		c.buf.WriteString(p.Name)
		c.setVar(p.Name, c.cppType(p.Type))
	}
	c.buf.WriteString("){\n")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.popScope()
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		names := make([]string, len(t.Variants))
		for i, v := range t.Variants {
			cname := v.Name
			names[i] = cname
			c.writeln(fmt.Sprintf("struct %s {", cname))
			c.indent++
			for _, f := range v.Fields {
				typ := c.cppType(f.Type)
				c.writeln(fmt.Sprintf("%s %s;", typ, f.Name))
			}
			c.indent--
			c.writeln("};")
		}
		c.writeln(fmt.Sprintf("using %s = std::variant<%s>;", t.Name, strings.Join(names, ", ")))
		return nil
	}
	c.writeln(fmt.Sprintf("struct %s {", t.Name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			typ := c.cppType(m.Field.Type)
			c.writeln(fmt.Sprintf("%s %s;", typ, m.Field.Name))
		}
	}
	c.indent--
	c.writeln("};")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr := c.compileExpr(s.Let.Value)
		if lit := getEmptyListLiteral(s.Let.Value); lit != nil {
			var elem string
			if s.Let.Type != nil && s.Let.Type.Generic != nil && len(s.Let.Type.Generic.Args) == 1 {
				elem = c.cppType(s.Let.Type.Generic.Args[0])
			} else if typ, err := c.env.GetVar(s.Let.Name); err == nil {
				if lt, ok := typ.(types.ListType); ok {
					elem = c.cppTypeRef(lt.Elem)
				}
			}
			if elem != "" {
				expr = fmt.Sprintf("vector<%s>{}", elem)
			}
		} else if lit := getEmptyMapLiteral(s.Let.Value); lit != nil {
			var keyT, valT string
			if s.Let.Type != nil && s.Let.Type.Generic != nil && len(s.Let.Type.Generic.Args) == 2 {
				keyT = c.cppType(s.Let.Type.Generic.Args[0])
				valT = c.cppType(s.Let.Type.Generic.Args[1])
			} else if typ, err := c.env.GetVar(s.Let.Name); err == nil {
				if mt, ok := typ.(types.MapType); ok {
					keyT = c.cppTypeRef(mt.Key)
					valT = c.cppTypeRef(mt.Value)
				}
			}
			if keyT != "" && valT != "" {
				expr = fmt.Sprintf("unordered_map<%s, %s>{}", keyT, valT)
			}
		}
		typ := "auto"
		if s.Let.Type != nil {
			typ = c.cppType(s.Let.Type)
		} else if t := c.guessExprType(s.Let.Value); t != "" {
			typ = t
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("%s %s;", typ, s.Let.Name))
		} else {
			c.writeln(fmt.Sprintf("%s %s = %s;", typ, s.Let.Name, expr))
		}
		c.setVar(s.Let.Name, typ)
	case s.Var != nil:
		expr := c.compileExpr(s.Var.Value)
		if lit := getEmptyListLiteral(s.Var.Value); lit != nil {
			var elem string
			if s.Var.Type != nil && s.Var.Type.Generic != nil && len(s.Var.Type.Generic.Args) == 1 {
				elem = c.cppType(s.Var.Type.Generic.Args[0])
			} else if typ, err := c.env.GetVar(s.Var.Name); err == nil {
				if lt, ok := typ.(types.ListType); ok {
					elem = c.cppTypeRef(lt.Elem)
				}
			}
			if elem != "" {
				expr = fmt.Sprintf("vector<%s>{}", elem)
			}
		} else if lit := getEmptyMapLiteral(s.Var.Value); lit != nil {
			var keyT, valT string
			if s.Var.Type != nil && s.Var.Type.Generic != nil && len(s.Var.Type.Generic.Args) == 2 {
				keyT = c.cppType(s.Var.Type.Generic.Args[0])
				valT = c.cppType(s.Var.Type.Generic.Args[1])
			} else if typ, err := c.env.GetVar(s.Var.Name); err == nil {
				if mt, ok := typ.(types.MapType); ok {
					keyT = c.cppTypeRef(mt.Key)
					valT = c.cppTypeRef(mt.Value)
				}
			}
			if keyT != "" && valT != "" {
				expr = fmt.Sprintf("unordered_map<%s, %s>{}", keyT, valT)
			}
		}
		typ := "auto"
		if s.Var.Type != nil {
			typ = c.cppType(s.Var.Type)
		} else if t := c.guessExprType(s.Var.Value); t != "" {
			typ = t
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("%s %s;", typ, s.Var.Name))
		} else {
			c.writeln(fmt.Sprintf("%s %s = %s;", typ, s.Var.Name, expr))
		}
		c.setVar(s.Var.Name, typ)
	case s.Assign != nil:
		name := s.Assign.Name
		for _, idx := range s.Assign.Index {
			iexpr := c.compileExpr(idx.Start)
			name = fmt.Sprintf("%s[%s]", name, iexpr)
		}
		value := c.compileExpr(s.Assign.Value)
		c.writeln(fmt.Sprintf("%s = %s;", name, value))
	case s.Return != nil:
		expr := c.compileExpr(s.Return.Value)
		c.writeln(fmt.Sprintf("return %s;", expr))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Expr != nil:
		if call := getPrintCall(s.Expr.Expr); call != nil {
			return c.compilePrint(call)
		}
		expr := c.compileExpr(s.Expr.Expr)
		if expr != "" {
			c.writeln(fmt.Sprintf("%s;", expr))
		}
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd != nil {
		start := c.compileExpr(f.Source)
		end := c.compileExpr(f.RangeEnd)
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", f.Name, start, f.Name, end, f.Name))
		c.setVar(f.Name, "int")
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
	src := c.compileExpr(f.Source)
	elemType := "auto"
	if isListLiteral(f.Source) {
		if t := c.guessExprType(f.Source); strings.HasPrefix(t, "vector<") {
			elemType = strings.TrimSuffix(strings.TrimPrefix(t, "vector<"), ">")
			if !isPrimitive(elemType) {
				elemType = "const " + elemType + "&"
			}
		}
	} else if isStringLiteral(f.Source) {
		elemType = "char"
	}
	name := f.Name
	if name == "_" {
		name = "_"
	}
	c.writeln(fmt.Sprintf("for (%s %s : %s) {", elemType, name, src))
	c.setVar(f.Name, strings.TrimPrefix(elemType, "const "))
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

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond := c.compileExpr(w.Cond)
	c.writeln(fmt.Sprintf("while (%s) {", cond))
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

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	cond := c.compileExpr(ifst.Cond)
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if ifst.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("} else ")
		return c.compileIf(ifst.ElseIf)
	}
	if len(ifst.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
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

func (c *Compiler) compileExpr(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) string {
	expr := c.compileUnary(b.Left)
	typ := c.guessUnaryType(b.Left)
	for _, op := range b.Right {
		rhs := c.compilePostfix(op.Right)
		rtyp := c.guessPostfixType(op.Right)
		if op.Op == "+" && (strings.HasPrefix(typ, "vector<") || strings.HasPrefix(rtyp, "vector<")) {
			elem := "int"
			if strings.HasPrefix(typ, "vector<") {
				elem = strings.TrimSuffix(strings.TrimPrefix(typ, "vector<"), ">")
			} else if strings.HasPrefix(rtyp, "vector<") {
				elem = strings.TrimSuffix(strings.TrimPrefix(rtyp, "vector<"), ">")
			}
			expr = fmt.Sprintf("([&](vector<%s> a, vector<%s> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(%s, %s)", elem, elem, expr, rhs)
			typ = "vector<" + elem + ">"
			continue
		}
		if op.Op == "+" && (typ == "string" || rtyp == "string") {
			if typ != "string" {
				if typ == "char" {
					expr = fmt.Sprintf("string(1, %s)", expr)
				} else {
					expr = fmt.Sprintf("string(%s)", expr)
				}
			}
			if rtyp != "string" {
				if rtyp == "char" {
					rhs = fmt.Sprintf("string(1, %s)", rhs)
				} else {
					rhs = fmt.Sprintf("string(%s)", rhs)
				}
			}
			expr = fmt.Sprintf("%s + %s", expr, rhs)
			typ = "string"
			continue
		}
		expr = fmt.Sprintf("%s %s %s", expr, op.Op, rhs)
		typ = guessBinaryResultType(typ, op.Op, rtyp)
	}
	return expr
}

func (c *Compiler) compileUnary(u *parser.Unary) string {
	expr := c.compilePostfix(u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = u.Ops[i] + expr
	}
	return expr
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) string {
	expr := c.compilePrimary(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil {
			// Handle simple index or slice
			if op.Index.Colon == nil {
				idx := c.compileExpr(op.Index.Start)
				typ := c.guessPrimaryType(p.Target)
				if typ == "string" {
					c.helpers["indexString"] = true
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
			} else {
				start := "0"
				if op.Index.Start != nil {
					start = c.compileExpr(op.Index.Start)
				}
				end := fmt.Sprintf("%s.size()", expr)
				if op.Index.End != nil {
					end = c.compileExpr(op.Index.End)
				}
				typ := c.guessPrimaryType(p.Target)
				if strings.HasPrefix(typ, "vector<") {
					c.helpers["sliceVec"] = true
					expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
				} else if typ == "string" {
					c.helpers["sliceStr"] = true
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				} else {
					expr = fmt.Sprintf("%s.substr(%s, %s - %s)", expr, start, end, start)
				}
			}
		}
	}
	return expr
}

func (c *Compiler) compilePrimary(p *parser.Primary) string {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int)
		}
		if p.Lit.Float != nil {
			v := strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
			if !strings.ContainsAny(v, ".eE") {
				if !strings.Contains(v, ".") {
					v += ".0"
				}
			}
			return v
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true"
			}
			return "false"
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("string(%s)", strconv.Quote(*p.Lit.Str))
		}
	case p.Struct != nil:
		fields := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			fields[i] = c.compileExpr(f.Value)
		}
		return fmt.Sprintf("%s{%s}", p.Struct.Name, strings.Join(fields, ", "))
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			elems[i] = c.compileExpr(e)
		}
		elemType := "int"
		if len(p.List.Elems) > 0 {
			if t := c.guessExprType(p.List.Elems[0]); t != "" {
				elemType = t
			}
		}
		return fmt.Sprintf("vector<%s>{%s}", elemType, strings.Join(elems, ", "))
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Query != nil:
		q, _ := c.compileQuery(p.Query)
		return q
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			args[i] = c.compileExpr(a)
		}
		switch p.Call.Func {
		case "len":
			return fmt.Sprintf("%s.size()", args[0])
		case "str":
			return fmt.Sprintf("to_string(%s)", args[0])
		case "reduce":
			if len(args) == 3 {
				c.helpers["reduce"] = true
				return fmt.Sprintf("_reduce(%s, %s, %s)", args[0], args[1], args[2])
			}
			return fmt.Sprintf("reduce(%s)", strings.Join(args, ", "))
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", "))
		}
	case p.Group != nil:
		return "(" + c.compileExpr(p.Group) + ")"
	}
	return ""
}

func getPrintCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	if post.Target.Call != nil && post.Target.Call.Func == "print" {
		return post.Target.Call
	}
	return nil
}

func (c *Compiler) compilePrint(call *parser.CallExpr) error {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		expr := c.compileExpr(a)
		typ := c.guessExprType(a)
		if strings.HasPrefix(typ, "vector<") {
			c.helpers["fmtVec"] = true
			expr = "_fmtVec(" + expr + ")"
		}
		args[i] = expr
	}
	c.writeIndent()
	c.buf.WriteString("std::cout")
	for i, a := range args {
		if i == 0 {
			c.buf.WriteString(" << ")
		} else {
			c.buf.WriteString(" << \" \" << ")
		}
		c.buf.WriteString("(" + a + ")")
	}
	c.buf.WriteString(" << std::endl;\n")
	return nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) string {
	items := make([]string, len(m.Items))
	keyType := "string"
	valType := "int"
	if len(m.Items) > 0 {
		if t := c.guessExprType(m.Items[0].Key); t != "" {
			keyType = t
		}
		if t := c.guessExprType(m.Items[0].Value); t != "" {
			valType = t
		}
	}
	for i, it := range m.Items {
		k := c.compileExpr(it.Key)
		v := c.compileExpr(it.Value)
		items[i] = fmt.Sprintf("{%s, %s}", k, v)
	}
	return fmt.Sprintf("unordered_map<%s, %s>{%s}", keyType, valType, strings.Join(items, ", "))
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) string {
	target := c.compileExpr(m.Target)
	tmp := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++
	var b strings.Builder
	b.WriteString("([&]() { auto " + tmp + " = " + target + "; ")
	var def string
	for _, cs := range m.Cases {
		res := c.compileExpr(cs.Result)
		if isUnderscoreExpr(cs.Pattern) {
			def = res
			continue
		}
		if st := getStructLiteral(cs.Pattern); st != nil {
			b.WriteString(fmt.Sprintf("if (std::holds_alternative<%s>(%s)) { ", st.Name, tmp))
			b.WriteString(fmt.Sprintf("auto _v = std::get<%s>(%s); ", st.Name, tmp))
			for _, f := range st.Fields {
				if name, ok := selectorName(f.Value); ok {
					b.WriteString(fmt.Sprintf("auto %s = _v.%s; ", name, f.Name))
				}
			}
			b.WriteString("return " + res + "; }")
			continue
		}
		if name, ok := selectorName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(name); ok {
				b.WriteString(fmt.Sprintf("if (std::holds_alternative<%s>(%s)) return %s; ", name, tmp, res))
				continue
			}
		}
		if call := getCallExpr(cs.Pattern); call != nil {
			b.WriteString(fmt.Sprintf("if (std::holds_alternative<%s>(%s)) { ", call.Func, tmp))
			b.WriteString(fmt.Sprintf("auto _v = std::get<%s>(%s); ", call.Func, tmp))
			for i, a := range call.Args {
				if name, ok := selectorName(a); ok {
					st, _ := c.env.FindUnionByVariant(call.Func)
					fieldName := st.Variants[call.Func].Order[i]
					b.WriteString(fmt.Sprintf("auto %s = _v.%s; ", name, fieldName))
				}
			}
			b.WriteString("return " + res + "; }")
			continue
		}
		pat := c.compileExpr(cs.Pattern)
		b.WriteString(fmt.Sprintf("if (%s == %s) return %s; ", tmp, pat, res))
	}
	if def != "" {
		b.WriteString("return " + def + ";")
	} else {
		b.WriteString("return {};")
	}
	b.WriteString(" })()")
	return b.String()
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) string {
	var params []string
	for _, p := range fn.Params {
		params = append(params, c.cppType(p.Type)+" "+p.Name)
	}
	var body bytes.Buffer
	if fn.ExprBody != nil {
		body.WriteString("return " + c.compileExpr(fn.ExprBody) + ";")
	} else {
		oldBuf := c.buf
		c.buf = body
		for _, st := range fn.BlockBody {
			c.compileStmt(st)
		}
		c.buf = oldBuf
	}
	return "[=](" + strings.Join(params, ", ") + ") { " + body.String() + " }"
}

func (c *Compiler) writeHelpers() {
	for _, name := range helperOrder {
		if !c.helpers[name] {
			continue
		}
		for _, line := range helperCode[name] {
			c.writeln(line)
		}
		c.writeln("")
	}
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		c.helpers["groupBy"] = true
		src := c.compileExpr(q.Source)
		key := c.compileExpr(q.Group.Expr)
		sel := c.compileExpr(q.Select)
		resType := c.guessExprType(q.Select)
		if resType == "" {
			resType = "auto"
		}
		var buf bytes.Buffer
		buf.WriteString("([&]() -> vector<" + resType + "> {\n")
		buf.WriteString("\tauto _src = " + src + ";\n")
		buf.WriteString("\tauto _groups = _group_by(_src, [&](auto& " + q.Var + "){ return " + key + "; });\n")
		buf.WriteString("\tvector<" + resType + "> _res;\n")
		buf.WriteString("\tfor (auto& " + q.Group.Name + " : _groups) {\n")
		buf.WriteString("\t\t_res.push_back(" + sel + ");\n")
		buf.WriteString("\t}\n")
		buf.WriteString("\treturn _res;\n")
		buf.WriteString("})()")
		return buf.String(), nil
	}
	if q.Group != nil {
		return "", fmt.Errorf("query not supported")
	}
	src := c.compileExpr(q.Source)
	vars := []string{q.Var}
	srcs := []string{src}
	for _, f := range q.Froms {
		vars = append(vars, f.Var)
		srcs = append(srcs, c.compileExpr(f.Src))
	}
	joinVars := make([]string, len(q.Joins))
	joinSrcs := make([]string, len(q.Joins))
	joinConds := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		joinVars[i] = j.Var
		joinSrcs[i] = c.compileExpr(j.Src)
		joinConds[i] = c.compileExpr(j.On)
	}
	sel := c.compileExpr(q.Select)
	resType := c.guessExprType(q.Select)
	if resType == "" {
		resType = "auto"
	}
	var sortExpr string
	if q.Sort != nil {
		sortExpr = c.compileExpr(q.Sort)
	}
	var skipExpr, takeExpr string
	if q.Skip != nil {
		skipExpr = c.compileExpr(q.Skip)
	}
	if q.Take != nil {
		takeExpr = c.compileExpr(q.Take)
	}
	var buf bytes.Buffer
	buf.WriteString("([&]() -> vector<" + resType + "> {\n")
	if q.Sort != nil {
		buf.WriteString("\tvector<pair<" + resType + ", " + resType + ">> _tmp;\n")
	} else {
		buf.WriteString("\tvector<" + resType + "> _res;\n")
	}
	indent := "\t"
	for i, v := range vars {
		buf.WriteString(indent + "for (auto& " + v + " : " + srcs[i] + ") {\n")
		indent += "\t"
	}
	for i, v := range joinVars {
		buf.WriteString(indent + "for (auto& " + v + " : " + joinSrcs[i] + ") {\n")
		indent += "\t"
		buf.WriteString(indent + "if (!(" + joinConds[i] + ")) continue;\n")
	}
	if q.Where != nil {
		cond := c.compileExpr(q.Where)
		buf.WriteString(indent + "if (" + cond + ") {\n")
		indent += "\t"
	}
	if q.Sort != nil {
		buf.WriteString(indent + "_tmp.push_back({" + sortExpr + ", " + sel + "});\n")
	} else {
		buf.WriteString(indent + "_res.push_back(" + sel + ");\n")
	}
	if q.Where != nil {
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	}
	for range joinVars {
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	}
	for range vars {
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	}
	if q.Sort != nil {
		buf.WriteString("\tstd::sort(_tmp.begin(), _tmp.end(), [](const auto& a, const auto& b){ return a.first < b.first; });\n")
		buf.WriteString("\tvector<" + resType + "> _res;\n")
		buf.WriteString("\t_res.reserve(_tmp.size());\n")
		buf.WriteString("\tfor (auto& _it : _tmp) _res.push_back(_it.second);\n")
	}
	if skipExpr != "" {
		buf.WriteString("\tint _skip = " + skipExpr + ";\n")
		buf.WriteString("\tif (_skip < (int)_res.size()) _res.erase(_res.begin(), _res.begin() + _skip); else _res.clear();\n")
	}
	if takeExpr != "" {
		buf.WriteString("\tint _take = " + takeExpr + ";\n")
		buf.WriteString("\tif (_take < (int)_res.size()) _res.resize(_take);\n")
	}
	buf.WriteString("\treturn _res;\n")
	buf.WriteString("})()")
	return buf.String(), nil
}
