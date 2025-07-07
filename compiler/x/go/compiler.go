package gocode

import (
	"bytes"
	"fmt"

	"mochi/parser"
)

// Compiler translates a subset of Mochi to Go source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
}

// New creates a new Go compiler.
func New() *Compiler {
	return &Compiler{}
}

// Compile translates the given program to Go.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.writeln("//go:build ignore")
	c.writeln("")
	c.writeln("package main")
	c.writeln("")
	c.writeln("import \"fmt\"")
	c.writeln("")
	// first pass: function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
		}
	}
	c.writeln("")
	c.writeln("func main() {")
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
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	if l.Value == nil {
		return fmt.Errorf("let without value at line %d", l.Pos.Line)
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s", l.Name, val))
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		typ := "interface{}"
		if p.Type != nil {
			typ = c.typeString(p.Type)
		}
		params[i] = fmt.Sprintf("%s %s", p.Name, typ)
	}
	ret := ""
	if f.Return != nil {
		ret = " " + c.typeString(f.Return)
	}
	c.writeln(fmt.Sprintf("func %s(%s)%s {", f.Name, join(params, ", "), ret))
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val)
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond + " {")
	c.indent++
	for _, s := range i.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.write("}")
	if i.ElseIf != nil {
		nextCond, err := c.compileExpr(i.ElseIf.Cond)
		if err != nil {
			return err
		}
		c.buf.WriteString(" else if " + nextCond + " {")
		c.buf.WriteByte('\n')
		c.indent++
		for _, s := range i.ElseIf.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.write("}")
		if len(i.ElseIf.Else) > 0 {
			c.buf.WriteString(" else {\n")
			c.indent++
			for _, s := range i.ElseIf.Else {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("}")
		} else {
			c.writeln("")
		}
		return nil
	}
	if len(i.Else) > 0 {
		c.buf.WriteString(" else {\n")
		c.indent++
		for _, s := range i.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("")
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
		c.writeln(fmt.Sprintf("for %s := %s; %s < %s; %s++ {", f.Name, start, f.Name, end, f.Name))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for _, %s := range %s {", f.Name, src))
	}
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) typeString(t *parser.TypeRef) string {
	if t == nil {
		return "interface{}"
	}
	if t.Simple != nil {
		return *t.Simple
	}
	return "interface{}"
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
	if len(p.Ops) > 0 {
		return "", fmt.Errorf("postfix operations not supported")
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[]int{" + join(elems, ", ") + "}", nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + join(p.Selector.Tail, ".")
		}
		return name, nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	argStr := join(args, ", ")
	switch call.Func {
	case "print":
		return fmt.Sprintf("fmt.Println(%s)", argStr), nil
	case "append":
		return fmt.Sprintf("append(%s)", argStr), nil
	case "len":
		return fmt.Sprintf("len(%s)", argStr), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, argStr), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		return fmt.Sprintf("%t", bool(*l.Bool))
	default:
		return "nil"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) write(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
}

func join(parts []string, sep string) string {
	if len(parts) == 0 {
		return ""
	}
	out := parts[0]
	for _, p := range parts[1:] {
		out += sep + p
	}
	return out
}
