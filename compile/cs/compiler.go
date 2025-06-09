package cscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("using System;")
	c.writeln("using System.Collections.Generic;")
	c.writeln("")
	c.writeln("public class Program {")
	c.indent++

	// emit function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("public static void Main(string[] args) {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")

	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("public static dynamic " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString("dynamic " + sanitizeName(p.Name))
	}
	c.buf.WriteString(") {")
	c.buf.WriteByte('\n')
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	value := "null"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("var %s = %s;", sanitizeName(s.Name), value))
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	value := "null"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("dynamic %s = %s;", sanitizeName(s.Name), value))
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	v, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(s.Name), v))
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if len(stmt.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		name := sanitizeName(stmt.Name)
		c.writeln(fmt.Sprintf("for (var %s = %s; %s <= %s; %s++) {", name, start, name, end, name))
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	name := sanitizeName(stmt.Name)
	c.writeln(fmt.Sprintf("foreach (var %s in %s) {", name, src))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, r)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("(%s%s)", u.Ops[i], val)
	}
	return val, nil
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
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s[%s]", expr, idx)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			expr += "." + sanitizeName(t)
		}
		return expr, nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("new List<dynamic>{%s}", strings.Join(elems, ", ")), nil
	case p.Group != nil:
		v, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	name := sanitizeName(call.Func)
	if name == "print" {
		return fmt.Sprintf("Console.WriteLine(%s)", strings.Join(args, ", ")), nil
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func compileLiteral(lit *parser.Literal) string {
	switch {
	case lit.Int != nil:
		return fmt.Sprintf("%d", *lit.Int)
	case lit.Float != nil:
		return fmt.Sprintf("%g", *lit.Float)
	case lit.Bool != nil:
		if bool(*lit.Bool) {
			return "true"
		}
		return "false"
	case lit.Str != nil:
		return fmt.Sprintf("%q", *lit.Str)
	}
	return "null"
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	return b.String()
}
