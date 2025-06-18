package ccode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

var cReserved = map[string]bool{
	"auto": true, "break": true, "case": true, "char": true, "const": true,
	"continue": true, "default": true, "do": true, "double": true, "else": true,
	"enum": true, "extern": true, "float": true, "for": true, "goto": true,
	"if": true, "int": true, "long": true, "register": true, "return": true,
	"short": true, "signed": true, "sizeof": true, "static": true, "struct": true,
	"switch": true, "typedef": true, "union": true, "unsigned": true, "void": true,
	"volatile": true, "while": true,
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
	if cReserved[s] {
		s = "_" + s
	}
	return s
}

type Compiler struct {
	buf                    bytes.Buffer
	indent                 int
	tmp                    int
	env                    *types.Env
	needsStr               bool
	needsInput             bool
	needsIndexString       bool
	needsStrLen            bool
	needsSliceListInt      bool
	needsSliceString       bool
	needsListListInt       bool
	needsConcatListListInt bool
	needsConcatListInt     bool
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
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

func (c *Compiler) newTemp() string {
	c.tmp++
	return fmt.Sprintf("_t%d", c.tmp)
}

func (c *Compiler) cType(t *parser.TypeRef) string {
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
			return "int"
		case "string":
			return "char*"
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" {
		if len(t.Generic.Args) == 1 {
			elem := c.cType(t.Generic.Args[0])
			if elem == "int" {
				return "list_int"
			}
			if elem == "list_int" {
				return "list_list_int"
			}
		}
	}
	return "int"
}

func (c *Compiler) compileProgram(prog *parser.Program) ([]byte, error) {
	// compile body first to know which helpers are needed
	oldBuf := c.buf
	c.buf = bytes.Buffer{}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main function
	c.writeln("int main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun == nil {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
	}
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")
	body := c.buf.String()
	c.buf = oldBuf

	c.writeln("#include <stdio.h>")
	c.writeln("#include <stdlib.h>")
	if c.needsInput || c.needsIndexString || c.needsStrLen || c.needsSliceString {
		c.writeln("#include <string.h>")
	}
	c.writeln("")
	c.writeln("typedef struct { int len; int *data; } list_int;")
	if c.needsListListInt {
		c.writeln("typedef struct { int len; list_int *data; } list_list_int;")
	}
	c.writeln("")
	c.writeln("static list_int list_int_create(int len) {")
	c.indent++
	c.writeln("list_int l;")
	c.writeln("l.len = len;")
	c.writeln("l.data = (int*)malloc(sizeof(int)*len);")
	c.writeln("return l;")
	c.indent--
	c.writeln("}")
	if c.needsConcatListInt {
		c.writeln("")
		c.writeln("static list_int concat_list_int(list_int a, list_int b) {")
		c.indent++
		c.writeln("list_int r = list_int_create(a.len + b.len);")
		c.writeln("for (int i = 0; i < a.len; i++) {")
		c.indent++
		c.writeln("r.data[i] = a.data[i];")
		c.indent--
		c.writeln("}")
		c.writeln("for (int i = 0; i < b.len; i++) {")
		c.indent++
		c.writeln("r.data[a.len + i] = b.data[i];")
		c.indent--
		c.writeln("}")
		c.writeln("return r;")
		c.indent--
		c.writeln("}")
	}
	if c.needsListListInt {
		c.writeln("")
		c.writeln("static list_list_int list_list_int_create(int len) {")
		c.indent++
		c.writeln("list_list_int l;")
		c.writeln("l.len = len;")
		c.writeln("l.data = (list_int*)malloc(sizeof(list_int)*len);")
		c.writeln("return l;")
		c.indent--
		c.writeln("}")
		if c.needsConcatListListInt {
			c.writeln("")
			c.writeln("static list_list_int concat_list_list_int(list_list_int a, list_list_int b) {")
			c.indent++
			c.writeln("list_list_int r = list_list_int_create(a.len + b.len);")
			c.writeln("for (int i = 0; i < a.len; i++) {")
			c.indent++
			c.writeln("r.data[i] = a.data[i];")
			c.indent--
			c.writeln("}")
			c.writeln("for (int i = 0; i < b.len; i++) {")
			c.indent++
			c.writeln("r.data[a.len + i] = b.data[i];")
			c.indent--
			c.writeln("}")
			c.writeln("return r;")
			c.indent--
			c.writeln("}")
		}
	}
	c.writeln("")
	// helper functions for builtins
	c.writeln("static int _count(list_int v) {")
	c.indent++
	c.writeln("return v.len;")
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.writeln("static double _avg(list_int v) {")
	c.indent++
	c.writeln("if (v.len == 0) return 0;")
	c.writeln("double sum = 0;")
	c.writeln("for (int i = 0; i < v.len; i++) {")
	c.indent++
	c.writeln("sum += v.data[i];")
	c.indent--
	c.writeln("}")
	c.writeln("return sum / v.len;")
	c.indent--
	c.writeln("}")
	if c.needsInput {
		c.writeln("")
		c.writeln("static char* _input() {")
		c.indent++
		c.writeln("char buf[1024];")
		c.writeln("if (!fgets(buf, sizeof(buf), stdin)) return strdup(\"\");")
		c.writeln("size_t len = strlen(buf);")
		c.writeln("if (len > 0 && buf[len-1] == '\\n') buf[len-1] = '\\0';")
		c.writeln("return strdup(buf);")
		c.indent--
		c.writeln("}")
	}
	if c.needsStr {
		c.writeln("")
		c.writeln("static char* _str(int v) {")
		c.indent++
		c.writeln("char* buf = (char*)malloc(32);")
		c.writeln("sprintf(buf, \"%d\", v);")
		c.writeln("return buf;")
		c.indent--
		c.writeln("}")
	}
	if c.needsIndexString {
		c.writeln("")
		c.writeln("static char* _index_string(char* s, int i) {")
		c.indent++
		c.writeln("int len = strlen(s);")
		c.writeln("if (i < 0) i += len;")
		c.writeln("if (i < 0 || i >= len) { fprintf(stderr, \"index out of range\\n\"); exit(1); }")
		c.writeln("char* buf = (char*)malloc(2);")
		c.writeln("buf[0] = s[i];")
		c.writeln("buf[1] = '\\0';")
		c.writeln("return buf;")
		c.indent--
		c.writeln("}")
	}
	if c.needsSliceString {
		c.writeln("")
		c.writeln("static char* slice_string(char* s, int start, int end) {")
		c.indent++
		c.writeln("int len = strlen(s);")
		c.writeln("if (start < 0) start += len;")
		c.writeln("if (end < 0) end += len;")
		c.writeln("if (start < 0) start = 0;")
		c.writeln("if (end > len) end = len;")
		c.writeln("if (start > end) start = end;")
		c.writeln("char* buf = (char*)malloc(end - start + 1);")
		c.writeln("memcpy(buf, s + start, end - start);")
		c.writeln("buf[end - start] = '\\0';")
		c.writeln("return buf;")
		c.indent--
		c.writeln("}")
	}
	if c.needsSliceListInt {
		c.writeln("")
		c.writeln("static list_int slice_list_int(list_int v, int start, int end) {")
		c.indent++
		c.writeln("if (start < 0) start += v.len;")
		c.writeln("if (end < 0) end += v.len;")
		c.writeln("if (start < 0) start = 0;")
		c.writeln("if (end > v.len) end = v.len;")
		c.writeln("if (start > end) start = end;")
		c.writeln("list_int r = list_int_create(end - start);")
		c.writeln("for (int i = 0; i < r.len; i++) {")
		c.indent++
		c.writeln("r.data[i] = v.data[start + i];")
		c.indent--
		c.writeln("}")
		c.writeln("return r;")
		c.indent--
		c.writeln("}")
	}
	if c.needsListListInt {
		c.writeln("")
		c.writeln("static void _print_list_int(list_int v) {")
		c.indent++
		c.writeln("printf(\"[\");")
		c.writeln("for (int i = 0; i < v.len; i++) {")
		c.indent++
		c.writeln("if (i > 0) printf(\" \");")
		c.writeln("printf(\"%d\", v.data[i]);")
		c.indent--
		c.writeln("}")
		c.writeln("printf(\"]\");")
		c.indent--
		c.writeln("}")
		c.writeln("")
		c.writeln("static void _print_list_list_int(list_list_int v) {")
		c.indent++
		c.writeln("printf(\"[\");")
		c.writeln("for (int i = 0; i < v.len; i++) {")
		c.indent++
		c.writeln("if (i > 0) printf(\" \");")
		c.writeln("_print_list_int(v.data[i]);")
		c.indent--
		c.writeln("}")
		c.writeln("printf(\"]\");")
		c.indent--
		c.writeln("}")
	}
	c.writeln("")

	c.buf.WriteString(body)
	return c.buf.Bytes(), nil
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	return c.compileProgram(prog)
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	ret := c.cType(fun.Return)
	c.buf.WriteString(ret + " ")
	c.buf.WriteString(sanitizeName(fun.Name))
	c.buf.WriteByte('(')
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(c.cType(p.Type))
		c.buf.WriteByte(' ')
		c.buf.WriteString(sanitizeName(p.Name))
	}
	c.buf.WriteString("){\n")
	c.indent++
	oldEnv := c.env
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		for _, p := range fun.Params {
			if p.Type != nil {
				c.env.SetVar(p.Name, resolveTypeRef(p.Type, oldEnv), true)
			}
		}
	}
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = oldEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := sanitizeName(s.Let.Name)
		typ := "int"
		if s.Let.Type != nil {
			typ = c.cType(s.Let.Type)
		} else if c.env != nil {
			if t, err := c.env.GetVar(s.Let.Name); err == nil {
				switch tt := t.(type) {
				case types.ListType:
					if _, ok := tt.Elem.(types.IntType); ok {
						typ = "list_int"
					} else if lt, ok2 := tt.Elem.(types.ListType); ok2 {
						if _, ok3 := lt.Elem.(types.IntType); ok3 {
							typ = "list_list_int"
						}
					}
				case types.StringType:
					typ = "char*"
				case types.FloatType:
					typ = "double"
				}
			}
		}
		if s.Let.Value != nil {
			val := c.compileExpr(s.Let.Value)
			c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
		} else {
			c.writeln(fmt.Sprintf("%s %s;", typ, name))
		}
		if c.env != nil {
			var t types.Type
			if s.Let.Type != nil {
				t = resolveTypeRef(s.Let.Type, c.env)
			} else if isListListExpr(s.Let.Value, c.env) {
				t = types.ListType{Elem: types.ListType{Elem: types.IntType{}}}
			} else if isListIntExpr(s.Let.Value, c.env) {
				t = types.ListType{Elem: types.IntType{}}
			} else if isStringExpr(s.Let.Value, c.env) {
				t = types.StringType{}
			} else if isFloatArg(s.Let.Value, c.env) {
				t = types.FloatType{}
			} else {
				t = types.IntType{}
			}
			c.env.SetVar(s.Let.Name, t, false)
		}
	case s.Var != nil:
		name := sanitizeName(s.Var.Name)
		typ := "int"
		if s.Var.Type != nil {
			typ = c.cType(s.Var.Type)
		} else if c.env != nil {
			if t, err := c.env.GetVar(s.Var.Name); err == nil {
				switch tt := t.(type) {
				case types.ListType:
					if _, ok := tt.Elem.(types.IntType); ok {
						typ = "list_int"
					} else if lt, ok2 := tt.Elem.(types.ListType); ok2 {
						if _, ok3 := lt.Elem.(types.IntType); ok3 {
							typ = "list_list_int"
						}
					}
				case types.StringType:
					typ = "char*"
				case types.FloatType:
					typ = "double"
				}
			} else {
				if isListListExpr(s.Var.Value, c.env) {
					typ = "list_list_int"
				} else if isListIntExpr(s.Var.Value, c.env) {
					typ = "list_int"
				} else if isStringExpr(s.Var.Value, c.env) {
					typ = "char*"
				} else if isFloatArg(s.Var.Value, c.env) {
					typ = "double"
				}
			}
		} else {
			if isListListExpr(s.Var.Value, nil) {
				typ = "list_list_int"
			} else if isListIntExpr(s.Var.Value, nil) {
				typ = "list_int"
			} else if isStringExpr(s.Var.Value, nil) {
				typ = "char*"
			} else if isFloatArg(s.Var.Value, nil) {
				typ = "double"
			}
		}
		if s.Var.Value != nil {
			val := c.compileExpr(s.Var.Value)
			c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
		} else {
			c.writeln(fmt.Sprintf("%s %s;", typ, name))
		}
		if c.env != nil {
			var t types.Type
			if s.Var.Type != nil {
				t = resolveTypeRef(s.Var.Type, c.env)
			} else if isListListExpr(s.Var.Value, c.env) {
				t = types.ListType{Elem: types.ListType{Elem: types.IntType{}}}
			} else if isListIntExpr(s.Var.Value, c.env) {
				t = types.ListType{Elem: types.IntType{}}
			} else if isStringExpr(s.Var.Value, c.env) {
				t = types.StringType{}
			} else if isFloatArg(s.Var.Value, c.env) {
				t = types.FloatType{}
			} else {
				t = types.IntType{}
			}
			c.env.SetVar(s.Var.Name, t, true)
		}
	case s.Assign != nil:
		lhs := sanitizeName(s.Assign.Name)
		if len(s.Assign.Index) > 0 {
			idx := c.compileExpr(s.Assign.Index[0].Start)
			val := c.compileExpr(s.Assign.Value)
			c.writeln(fmt.Sprintf("%s.data[%s] = %s;", lhs, idx, val))
		} else {
			val := c.compileExpr(s.Assign.Value)
			c.writeln(fmt.Sprintf("%s = %s;", lhs, val))
		}
	case s.Return != nil:
		val := c.compileExpr(s.Return.Value)
		c.writeln(fmt.Sprintf("return %s;", val))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr := c.compileExpr(s.Expr.Expr)
		if expr != "" {
			c.writeln(fmt.Sprintf("%s;", expr))
		}
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	default:
		// unsupported
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	useVar := f.Name != "_"
	if f.RangeEnd != nil {
		start := c.compileExpr(f.Source)
		end := c.compileExpr(f.RangeEnd)
		loopVar := name
		if !useVar {
			loopVar = c.newTemp()
		}
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", loopVar, start, loopVar, end, loopVar))
		if useVar && loopVar != name {
			c.indent++
			c.writeln(fmt.Sprintf("int %s = %s;", name, loopVar))
		} else {
			c.indent++
		}
	} else {
		src := c.compileExpr(f.Source)
		idx := c.newTemp()
		isStr := isStringExpr(f.Source, c.env)
		if isStr {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s[%s] != '\\0'; %s++) {", idx, src, idx, idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("char %s[2];", name))
				c.writeln(fmt.Sprintf("%s[0] = %s[%s];", name, src, idx))
				c.writeln(fmt.Sprintf("%s[1] = '\\0';", name))
			}
		} else {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s.len; %s++) {", idx, idx, src, idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("int %s = %s.data[%s];", name, src, idx))
			}
		}
		oldEnv := c.env
		if c.env != nil {
			c.env = types.NewEnv(c.env)
			if useVar {
				if isStr {
					c.env.SetVar(f.Name, types.StringType{}, true)
				} else {
					c.env.SetVar(f.Name, types.IntType{}, true)
				}
			}
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		if c.env != nil {
			c.env = oldEnv
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	oldEnv := c.env
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		if useVar {
			c.env.SetVar(f.Name, types.IntType{}, true)
		}
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = oldEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond := c.compileExpr(stmt.Cond)
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
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

func (c *Compiler) compileExpr(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	res := c.compileBinary(e.Binary)
	return res
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) string {
	left := c.compileUnary(b.Left)
	leftList := isListListUnary(b.Left, c.env)
	leftListInt := isListIntUnary(b.Left, c.env)
	for _, op := range b.Right {
		right := c.compilePostfix(op.Right)
		if op.Op == "+" && leftList && isListListPostfix(op.Right, c.env) {
			c.needsConcatListListInt = true
			c.needsListListInt = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_list_int %s = concat_list_list_int(%s, %s);", name, left, right))
			left = name
			leftList = true
			leftListInt = false
			continue
		}
		if op.Op == "+" && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.needsConcatListInt = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = concat_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			continue
		}
		left = fmt.Sprintf("(%s %s %s)", left, op.Op, right)
		leftList = false
		leftListInt = false
	}
	return left
}

func (c *Compiler) compileUnary(u *parser.Unary) string {
	expr := c.compilePostfix(u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			expr = fmt.Sprintf("(-%s)", expr)
		} else if op == "!" {
			expr = fmt.Sprintf("(!%s)", expr)
		}
	}
	return expr
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) string {
	expr := c.compilePrimary(p.Target)
	isStr := isStringPrimary(p.Target, c.env)
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx := c.compileExpr(op.Index.Start)
				if isStr && op.Index.End == nil {
					name := c.newTemp()
					c.needsIndexString = true
					c.writeln(fmt.Sprintf("char* %s = _index_string(%s, %s);", name, expr, idx))
					if c.env != nil {
						c.env.SetVar(name, types.StringType{}, true)
					}
					expr = name
					isStr = true
				} else {
					expr = fmt.Sprintf("%s.data[%s]", expr, idx)
					isStr = false
				}
			} else {
				start := "0"
				if op.Index.Start != nil {
					start = c.compileExpr(op.Index.Start)
				}
				end := fmt.Sprintf("%s.len", expr)
				if op.Index.End != nil {
					end = c.compileExpr(op.Index.End)
				}
				name := c.newTemp()
				if isStr {
					c.needsSliceString = true
					c.writeln(fmt.Sprintf("char* %s = slice_string(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.StringType{}, true)
					}
					expr = name
					isStr = true
				} else {
					c.needsSliceListInt = true
					c.writeln(fmt.Sprintf("list_int %s = slice_list_int(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.IntType{}}, true)
					}
					expr = name
					isStr = false
				}
			}
		}
	}
	return expr
}

func (c *Compiler) compilePrimary(p *parser.Primary) string {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		return c.compileSelector(p.Selector)
	case p.List != nil:
		name := c.newTemp()
		nested := false
		if len(p.List.Elems) > 0 {
			if isListLiteral(p.List.Elems[0]) || isListIntExpr(p.List.Elems[0], c.env) {
				nested = true
			}
		}
		if nested {
			c.needsListListInt = true
			c.writeln(fmt.Sprintf("list_list_int %s = list_list_int_create(%d);", name, len(p.List.Elems)))
			for i, el := range p.List.Elems {
				v := c.compileExpr(el)
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		} else {
			c.writeln(fmt.Sprintf("list_int %s = list_int_create(%d);", name, len(p.List.Elems)))
			for i, el := range p.List.Elems {
				v := c.compileExpr(el)
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		}
		return name
	case p.Call != nil:
		if p.Call.Func == "len" {
			isStr := isStringArg(p.Call.Args[0], c.env)
			arg := c.compileExpr(p.Call.Args[0])
			if isStr {
				c.needsStrLen = true
				return fmt.Sprintf("strlen(%s)", arg)
			}
			return fmt.Sprintf("%s.len", arg)
		} else if p.Call.Func == "print" {
			for i, a := range p.Call.Args {
				argExpr := c.compileExpr(a)
				if isListListExpr(a, c.env) {
					c.needsListListInt = true
					c.writeln(fmt.Sprintf("_print_list_list_int(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isListIntExpr(a, c.env) {
					c.needsListListInt = true
					c.writeln(fmt.Sprintf("_print_list_int(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else {
					fmtStr := "%d"
					if isStringArg(a, c.env) {
						fmtStr = "%s"
					} else if isFloatArg(a, c.env) {
						fmtStr = "%g"
					}
					end := " "
					if i == len(p.Call.Args)-1 {
						end = "\\n"
					}
					c.writeln(fmt.Sprintf("printf(\"%s%s\", %s);", fmtStr, end, argExpr))
				}
			}
			return ""
		} else if p.Call.Func == "count" {
			arg := c.compileExpr(p.Call.Args[0])
			return fmt.Sprintf("_count(%s)", arg)
		} else if p.Call.Func == "avg" {
			arg := c.compileExpr(p.Call.Args[0])
			return fmt.Sprintf("_avg(%s)", arg)
		} else if p.Call.Func == "str" {
			arg := c.compileExpr(p.Call.Args[0])
			name := c.newTemp()
			c.needsStr = true
			c.writeln(fmt.Sprintf("char* %s = _str(%s);", name, arg))
			return name
		} else if p.Call.Func == "input" {
			c.needsInput = true
			return "_input()"
		}
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			args[i] = c.compileExpr(a)
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", "))
	case p.Group != nil:
		return fmt.Sprintf("(%s)", c.compileExpr(p.Group))
	default:
		return "0"
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return strconv.FormatFloat(*l.Float, 'f', -1, 64)
	case l.Bool != nil:
		if *l.Bool {
			return "1"
		} else {
			return "0"
		}
	case l.Str != nil:
		return strconv.Quote(*l.Str)
	}
	return "0"
}

func (c *Compiler) compileSelector(s *parser.SelectorExpr) string {
	return sanitizeName(s.Root)
}

func isStringArg(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnaryOrIndex(e.Binary.Left, env)
}

func isStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnary(e.Binary.Left, env)
}

func isStringUnaryOrIndex(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfixOrIndex(u.Value, env)
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfix(u.Value, env)
}

func isStringPostfixOrIndex(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if p.Ops[0].Index != nil && p.Ops[0].Index.Colon == nil && isStringPrimary(p.Target, env) {
			return true
		}
		return false
	}
	return isStringPrimary(p.Target, env)
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isStringPrimary(p.Target, env)
}

func isStringPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	case p.Call != nil && p.Call.Func == "str":
		return true
	case p.Selector != nil && env != nil:
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func isFloatArg(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isFloatUnary(e.Binary.Left, env)
}

func isFloatUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isFloatPostfix(u.Value, env)
}

func isFloatPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isFloatPrimary(p.Target, env)
}

func isFloatPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Float != nil:
		return true
	case p.Selector != nil && env != nil:
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	case p.Call != nil && env != nil:
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if _, ok2 := ft.Return.(types.FloatType); ok2 {
					return true
				}
			}
		}
	}
	return false
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
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

func isListListIntType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if inner, ok2 := lt.Elem.(types.ListType); ok2 {
			if _, ok3 := inner.Elem.(types.IntType); ok3 {
				return true
			}
		}
	}
	return false
}

func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.IntType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" {
		if len(t.Generic.Args) == 1 {
			return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
		}
	}
	return types.AnyType{}
}

func isListListExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListListUnary(e.Binary.Left, env)
}

func isListListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListListPostfix(u.Value, env)
}

func isListListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListListPrimary(p.Target, env)
}

func isListListPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) > 0 {
			if isListLiteral(p.List.Elems[0]) || isListIntExpr(p.List.Elems[0], env) {
				return true
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if isListListIntType(t) {
				return true
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if isListListIntType(ft.Return) {
					return true
				}
			}
		}
	}
	return false
}

func isListIntExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListIntUnary(e.Binary.Left, env)
}

func isListIntUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListIntPostfix(u.Value, env)
}

func isListIntPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListIntPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		return true
	}
	return false
}

func isListIntPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if el.Binary.Left.Value.Target.Lit != nil && el.Binary.Left.Value.Target.Lit.Int != nil {
				return true
			}
			if el.Binary.Left.Value.Target.Selector != nil && env != nil {
				name := el.Binary.Left.Value.Target.Selector.Root
				if t, err := env.GetVar(name); err == nil {
					if lt, ok := t.(types.ListType); ok {
						if _, ok := lt.Elem.(types.IntType); ok {
							return true
						}
					}
					if _, ok := t.(types.IntType); ok {
						return true
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.IntType); ok {
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					if _, ok3 := lt.Elem.(types.IntType); ok3 {
						return true
					}
				}
			}
		}
	}
	return false
}
