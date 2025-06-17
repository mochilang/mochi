package ccode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf              bytes.Buffer
	indent           int
	tmp              int
	env              *types.Env
	needsStr         bool
	needsInput       bool
	needsListListInt bool
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
	if c.needsInput {
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
	}
	c.writeln("")
	// helper functions for builtins
	c.writeln("static int _count(list_int v) {")
	c.indent++
	c.writeln("return v.len;")
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.writeln("static int _avg(list_int v) {")
	c.indent++
	c.writeln("if (v.len == 0) return 0;")
	c.writeln("int sum = 0;")
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
	c.buf.WriteString(fun.Name)
	c.buf.WriteByte('(')
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(c.cType(p.Type))
		c.buf.WriteByte(' ')
		c.buf.WriteString(p.Name)
	}
	c.buf.WriteString("){\n")
	c.indent++
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := s.Let.Name
		typ := "int"
		if s.Let.Type != nil {
			typ = c.cType(s.Let.Type)
		} else if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
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
				}
			}
		}
		if s.Let.Value != nil {
			val := c.compileExpr(s.Let.Value)
			c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
		} else {
			c.writeln(fmt.Sprintf("%s %s;", typ, name))
		}
	case s.Var != nil:
		name := s.Var.Name
		typ := "int"
		if s.Var.Type != nil {
			typ = c.cType(s.Var.Type)
		} else if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
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
				}
			}
		}
		if s.Var.Value != nil {
			val := c.compileExpr(s.Var.Value)
			c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
		} else {
			c.writeln(fmt.Sprintf("%s %s;", typ, name))
		}
	case s.Assign != nil:
		lhs := s.Assign.Name
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
	name := f.Name
	if f.RangeEnd != nil {
		start := c.compileExpr(f.Source)
		end := c.compileExpr(f.RangeEnd)
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", name, start, name, end, name))
		c.indent++
	} else {
		src := c.compileExpr(f.Source)
		idx := c.newTemp()
		isStr := isStringExpr(f.Source, c.env)
		if isStr {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s[%s] != '\\0'; %s++) {", idx, src, idx, idx))
			c.indent++
			c.writeln(fmt.Sprintf("char %s[2];", name))
			c.writeln(fmt.Sprintf("%s[0] = %s[%s];", name, src, idx))
			c.writeln(fmt.Sprintf("%s[1] = '\\0';", name))
		} else {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s.len; %s++) {", idx, idx, src, idx))
			c.indent++
			c.writeln(fmt.Sprintf("int %s = %s.data[%s];", name, src, idx))
		}
		oldEnv := c.env
		if c.env != nil {
			c.env = types.NewEnv(c.env)
			if isStr {
				c.env.SetVar(name, types.StringType{}, true)
			} else {
				c.env.SetVar(name, types.IntType{}, true)
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
		c.env.SetVar(name, types.IntType{}, true)
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
	for _, op := range b.Right {
		right := c.compilePostfix(op.Right)
		left = fmt.Sprintf("(%s %s %s)", left, op.Op, right)
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
	for _, op := range p.Ops {
		if op.Index != nil {
			idx := c.compileExpr(op.Index.Start)
			expr = fmt.Sprintf("%s.data[%s]", expr, idx)
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
		if len(p.List.Elems) > 0 && isListLiteral(p.List.Elems[0]) {
			nested = true
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
			arg := c.compileExpr(p.Call.Args[0])
			return fmt.Sprintf("%s.len", arg)
		} else if p.Call.Func == "print" {
			for i, a := range p.Call.Args {
				argExpr := c.compileExpr(a)
				fmtStr := "%d"
				if isStringArg(a, c.env) {
					fmtStr = "%s"
				}
				end := " "
				if i == len(p.Call.Args)-1 {
					end = "\\n"
				}
				c.writeln(fmt.Sprintf("printf(\"%s%s\", %s);", fmtStr, end, argExpr))
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
	return s.Root
}

func isStringArg(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnary(e.Binary.Left, env)
}

func isStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnary(e.Binary.Left, env)
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfix(u.Value, env)
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		// conservatively assume non-string if operations present
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
