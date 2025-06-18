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
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

func New(env *types.Env) *Compiler { return &Compiler{env: env} }

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

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr := c.compileExpr(s.Let.Value)
		typ := "auto"
		if s.Let.Type != nil {
			typ = c.cppType(s.Let.Type)
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("%s %s;", typ, s.Let.Name))
		} else {
			c.writeln(fmt.Sprintf("%s %s = %s;", typ, s.Let.Name, expr))
		}
	case s.Var != nil:
		expr := c.compileExpr(s.Var.Value)
		typ := "auto"
		if s.Var.Type != nil {
			typ = c.cppType(s.Var.Type)
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
	// unsupported
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
	for _, op := range b.Right {
		rhs := c.compilePostfix(op.Right)
		expr = fmt.Sprintf("%s %s %s", expr, op.Op, rhs)
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
			idx := c.compileExpr(op.Index.Start)
			expr = fmt.Sprintf("%s[%s]", expr, idx)
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
		return "vector<int>{" + strings.Join(elems, ", ") + "}"
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
	for i, a := range args {
		if i > 0 {
			c.buf.WriteString("std::cout << \" \" << ")
			c.buf.WriteString(a)
		} else {
			c.buf.WriteString("std::cout << ")
			c.buf.WriteString(a)
		}
	}
	c.buf.WriteString(" << std::endl;\n")
	return nil
}
