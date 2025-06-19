package cppcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into minimal C++ source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	helpers map[string]bool
}

func New(env *types.Env) *Compiler { return &Compiler{env: env, helpers: map[string]bool{}} }

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
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return "vector<" + c.cppType(t.Generic.Args[0]) + ">"
	}
	return "auto"
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	ret := c.cppType(fn.Return)
	c.writeIndent()
	c.buf.WriteString(ret + " ")
	c.buf.WriteString(fn.Name)
	c.buf.WriteByte('(')
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(c.cppType(p.Type))
		c.buf.WriteByte(' ')
		c.buf.WriteString(p.Name)
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
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return nil // unions not supported
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
	case s.Var != nil:
		expr := c.compileExpr(s.Var.Value)
		if s.Var.Type != nil && s.Var.Value != nil {
			if lit := getEmptyListLiteral(s.Var.Value); lit != nil {
				if s.Var.Type.Generic != nil && len(s.Var.Type.Generic.Args) == 1 {
					elem := c.cppType(s.Var.Type.Generic.Args[0])
					expr = fmt.Sprintf("vector<%s>{}", elem)
				}
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
	if t := c.guessExprType(f.Source); strings.HasPrefix(t, "vector<") {
		elemType = strings.TrimSuffix(strings.TrimPrefix(t, "vector<"), ">")
		if !isPrimitive(elemType) {
			elemType = "const " + elemType + "&"
		}
	} else if t == "string" {
		elemType = "char"
	}
	name := f.Name
	if name == "_" {
		name = "_"
	}
	c.writeln(fmt.Sprintf("for (%s %s : %s) {", elemType, name, src))
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
		expr = fmt.Sprintf("%s %s %s", expr, op.Op, rhs)
		typ = ""
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
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true"
			}
			return "false"
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str)
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
		args[i] = c.compileExpr(a)
	}
	c.writeIndent()
	c.buf.WriteString("std::cout")
	for i, a := range args {
		if i == 0 {
			c.buf.WriteString(" << ")
		} else {
			c.buf.WriteString(" << \" \" << ")
		}
		c.buf.WriteString(a)
	}
	c.buf.WriteString(" << std::endl;\n")
	return nil
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
	if c.helpers["indexString"] {
		c.writeln("char _indexString(const string& s, int i) {")
		c.writeln("\tint n = s.size();")
		c.writeln("\tif (i < 0) i += n;")
		c.writeln("\tif (i < 0 || i >= n) throw std::out_of_range(\"index out of range\");")
		c.writeln("\treturn s[i];")
		c.writeln("}")
		c.writeln("")
	}
	if c.helpers["sliceVec"] {
		c.writeln("template<typename T> vector<T> _slice(const vector<T>& v, int start, int end) {")
		c.writeln("\tint n = v.size();")
		c.writeln("\tif (start < 0) start += n;")
		c.writeln("\tif (end < 0) end += n;")
		c.writeln("\tif (start < 0) start = 0;")
		c.writeln("\tif (end > n) end = n;")
		c.writeln("\tif (end < start) end = start;")
		c.writeln("\treturn vector<T>(v.begin() + start, v.begin() + end);")
		c.writeln("}")
		c.writeln("")
	}
	if c.helpers["sliceStr"] {
		c.writeln("string _sliceString(const string& s, int start, int end) {")
		c.writeln("\tint n = s.size();")
		c.writeln("\tif (start < 0) start += n;")
		c.writeln("\tif (end < 0) end += n;")
		c.writeln("\tif (start < 0) start = 0;")
		c.writeln("\tif (end > n) end = n;")
		c.writeln("\tif (end < start) end = start;")
		c.writeln("\treturn s.substr(start, end - start);")
		c.writeln("}")
		c.writeln("")
	}
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	if q.Where != nil || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || len(q.Joins) != 0 {
		return "", fmt.Errorf("query not supported")
	}
	src := c.compileExpr(q.Source)
	vars := []string{q.Var}
	srcs := []string{src}
	for _, f := range q.Froms {
		vars = append(vars, f.Var)
		srcs = append(srcs, c.compileExpr(f.Src))
	}
	sel := c.compileExpr(q.Select)
	resType := c.guessExprType(q.Select)
	if resType == "" {
		resType = "auto"
	}
	var buf bytes.Buffer
	buf.WriteString("([&]() -> vector<" + resType + "> {\n")
	buf.WriteString("\tvector<" + resType + "> _res;\n")
	indent := "\t"
	for i, v := range vars {
		buf.WriteString(indent + "for (auto& " + v + " : " + srcs[i] + ") {\n")
		indent += "\t"
	}
	buf.WriteString(indent + "_res.push_back(" + sel + ");\n")
	for range vars {
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	}
	buf.WriteString("\treturn _res;\n")
	buf.WriteString("})()")
	return buf.String(), nil
}

func (c *Compiler) guessExprType(e *parser.Expr) string {
	t := c.inferExprType(e)
	if _, ok := t.(types.AnyType); ok {
		return ""
	}
	return c.cppTypeRef(t)
}

func (c *Compiler) guessUnaryType(u *parser.Unary) string {
	t := c.inferUnaryType(u)
	if _, ok := t.(types.AnyType); ok {
		return ""
	}
	return c.cppTypeRef(t)
}

func (c *Compiler) guessPostfixType(p *parser.PostfixExpr) string {
	t := c.inferPostfixType(p)
	if _, ok := t.(types.AnyType); ok {
		return ""
	}
	return c.cppTypeRef(t)
}

func (c *Compiler) guessPrimaryType(p *parser.Primary) string {
	t := c.inferPrimaryType(p)
	if _, ok := t.(types.AnyType); ok {
		return ""
	}
	return c.cppTypeRef(t)
}

func (c *Compiler) cppTypeRef(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "string"
	case types.ListType:
		return "vector<" + c.cppTypeRef(tt.Elem) + ">"
	case types.StructType:
		return tt.Name
	}
	return "auto"
}

func isPrimitive(t string) bool {
	switch t {
	case "int", "double", "bool", "char":
		return true
	}
	return false
}

func getEmptyListLiteral(e *parser.Expr) *parser.ListLiteral {
	if e == nil || e.Binary == nil {
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
	if post.Target.List != nil && len(post.Target.List.Elems) == 0 {
		return post.Target.List
	}
	return nil
}
