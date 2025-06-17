package ccode

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
	tmp    int
	env    *types.Env
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
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" {
		if len(t.Generic.Args) == 1 {
			elem := c.cType(t.Generic.Args[0])
			if elem == "int" {
				return "list_int"
			}
		}
	}
	return "int"
}

func (c *Compiler) compileProgram(prog *parser.Program) ([]byte, error) {
	c.writeln("#include <stdio.h>")
	c.writeln("#include <stdlib.h>")
	c.writeln("")
	c.writeln("typedef struct { int len; int *data; } list_int;")
	c.writeln("")
	c.writeln("static list_int list_int_create(int len) {")
	c.indent++
	c.writeln("list_int l;")
	c.writeln("l.len = len;")
	c.writeln("l.data = (int*)malloc(sizeof(int)*len);")
	c.writeln("return l;")
	c.indent--
	c.writeln("}")
	c.writeln("")
	// functions first
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
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.IntType); ok {
						typ = "list_int"
					}
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
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.IntType); ok {
						typ = "list_int"
					}
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
		cond := c.compileExpr(s.If.Cond)
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent++
		for _, st := range s.If.Then {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
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
	start := c.compileExpr(f.Source)
	end := c.compileExpr(f.RangeEnd)
	name := f.Name
	c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", name, start, name, end, name))
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
		c.writeln(fmt.Sprintf("list_int %s = list_int_create(%d);", name, len(p.List.Elems)))
		for i, el := range p.List.Elems {
			v := c.compileExpr(el)
			c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
		}
		return name
	case p.Call != nil:
		if p.Call.Func == "len" {
			arg := c.compileExpr(p.Call.Args[0])
			return fmt.Sprintf("%s.len", arg)
		} else if p.Call.Func == "print" {
			arg := c.compileExpr(p.Call.Args[0])
			c.writeln(fmt.Sprintf("printf(\"%s\\n\", %s);", "%d", arg))
			return ""
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
	}
	return "0"
}

func (c *Compiler) compileSelector(s *parser.SelectorExpr) string {
	return s.Root
}
