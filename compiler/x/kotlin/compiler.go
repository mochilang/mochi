//go:build slow

package kotlin

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler converts a subset of Mochi programs to Kotlin source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Kotlin compiler.
func New(env *types.Env, _ string) *Compiler { return &Compiler{env: env} }

// Compile generates Kotlin code from prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0

	// emit function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.funDecl(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("fun main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.stmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		v, err := c.expr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("val %s = %s", s.Let.Name, v))
	case s.Var != nil:
		v, err := c.expr(s.Var.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("var %s = %s", s.Var.Name, v))
	case s.Assign != nil:
		v, err := c.expr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", s.Assign.Name, v))
	case s.Expr != nil:
		e, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(e)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) funDecl(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("fun %s(%s) {", f.Name, strings.Join(params, ", ")))
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) expr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.binary(e.Binary)
}

func (c *Compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for _, op := range b.Right {
		r, err := c.postfix(op.Right)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("%s %s %s", res, op.Op, r)
	}
	return res, nil
}

func (c *Compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) postfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
		case op.Index != nil:
			idx, err := c.expr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s[%s]", val, idx)
		case op.Field != nil:
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
		default:
			return "", fmt.Errorf("unsupported postfix")
		}
	}
	return val, nil
}

func (c *Compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.literal(p.Lit), nil
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.Call != nil:
		return c.callExpr(p.Call)
	case p.Group != nil:
		s, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + s + ")", nil
	case p.List != nil:
		parts := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.expr(e)
			if err != nil {
				return "", err
			}
			parts[i] = s
		}
		return "mutableListOf(" + strings.Join(parts, ", ") + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) callExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.expr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	if call.Func == "print" {
		return fmt.Sprintf("println(%s)", strings.Join(args, ", ")), nil
	}
	return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
}

func (c *Compiler) literal(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		return fmt.Sprintf("%t", bool(*l.Bool))
	case l.Null:
		return "null"
	}
	return "null"
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
