//go:build !slow

package cscode

import (
	"bytes"
	"fmt"
	"strconv"

	"mochi/parser"
	"mochi/types"
)

// Compiler is a very small C# code generator used by the tests. It
// only understands a tiny subset of Mochi syntax sufficient for the
// simplest example programs.  The intent is to expand support
// incrementally.
type Compiler struct {
	buf    bytes.Buffer
	indent int
}

// New returns a new minimal C# compiler.
func New(_ *types.Env) *Compiler { return &Compiler{} }

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile translates prog into C# source code. Only a handful of
// constructs like variable declarations, arithmetic expressions, basic
// loops and function calls are currently supported.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("using System;")
	c.writeln("using System.Collections.Generic;")
	c.writeln("")
	c.writeln("class Program {")
	c.indent++

	// emit function declarations first
	for _, st := range prog.Statements {
		if st.Fun != nil {
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
		}
	}

	c.writeln("static void Main() {")
	c.indent++
	for _, st := range prog.Statements {
		if st.Fun != nil {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	ret := "void"
	if fn.Return != nil {
		ret = "int" // only int return supported for now
	}
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("int %s", sanitize(p.Name))
	}
	c.writeln(fmt.Sprintf("static %s %s(%s) {", ret, sanitize(fn.Name), join(params, ", ")))
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

func join(list []string, sep string) string {
	if len(list) == 0 {
		return ""
	}
	res := list[0]
	for _, s := range list[1:] {
		res += sep + s
	}
	return res
}

func sanitize(s string) string { return s }

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("var %s = %s;", sanitize(s.Let.Name), val))
	case s.Var != nil:
		val, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("var %s = %s;", sanitize(s.Var.Name), val))
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s;", sanitize(s.Assign.Name), val))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
	case s.While != nil:
		cond, err := c.compileExpr(s.While.Cond)
		if err != nil {
			return err
		}
		c.writeln("while (" + cond + ") {")
		c.indent++
		for _, st := range s.While.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.For != nil:
		start, err := c.compileExpr(s.For.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(s.For.RangeEnd)
		if err != nil {
			return err
		}
		loopVar := sanitize(s.For.Name)
		c.writeln(fmt.Sprintf("for (var %s = %s; %s < %s; %s++) {", loopVar, start, loopVar, end, loopVar))
		c.indent++
		for _, st := range s.For.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("%s %s %s", res, op.Op, r)
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if val == "print" {
				c.writeln(fmt.Sprintf("Console.WriteLine(%s);", join(args, " + \" \" + ")))
				return "", nil
			}
			val = fmt.Sprintf("%s(%s)", val, join(args, ", "))
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", fmt.Errorf("nil primary")
	case p.Lit != nil:
		return compileLiteral(p.Lit), nil
	case p.Selector != nil:
		s := sanitize(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			s += "." + sanitize(t)
		}
		return s, nil
	case p.Call != nil:
		// handle simple call without postfix
		val := sanitize(p.Call.Func)
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		if val == "print" {
			return fmt.Sprintf("Console.WriteLine(%s)", join(args, " + \" \" + ")), nil
		}
		return fmt.Sprintf("%s(%s)", val, join(args, ", ")), nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int)
	case l.Str != nil:
		return strconv.Quote(*l.Str)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	default:
		return "0"
	}
}
