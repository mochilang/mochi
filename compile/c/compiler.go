package ccode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into minimal C source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new C compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

// Compile returns C source code implementing prog. Only a very small
// subset of Mochi is supported.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("#include <stdio.h>")
	c.writeln("")
	c.writeln("int main() {")
	c.indent++
	for _, stmt := range prog.Statements {
		if err := c.compileStmt(stmt); err != nil {
			return nil, err
		}
	}
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	typ := "int"
	if s.Type != nil && s.Type.Simple != nil {
		switch *s.Type.Simple {
		case "int":
			typ = "int"
		case "float":
			typ = "double"
		case "string":
			typ = "const char*"
		default:
			return fmt.Errorf("unsupported type %s", *s.Type.Simple)
		}
	}
	val := "0"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln(fmt.Sprintf("%s %s = %s;", typ, s.Name, val))
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
			callExpr, err := c.compileCallExpr(expr, op.Call)
			if err != nil {
				return "", err
			}
			expr = callExpr
			continue
		}
		return "", fmt.Errorf("unsupported postfix expression")
	}
	return expr, nil
}

func (c *Compiler) compileCallExpr(name string, call *parser.CallOp) (string, error) {
	if name == "print" {
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		if len(args) == 1 {
			return fmt.Sprintf("printf(\"%s\\n\", %s)", "%s", args[0]), nil
		}
		format := strings.TrimSpace(strings.Repeat("%s ", len(args)))
		return fmt.Sprintf("printf(\"%s\\n\", %s)", format, strings.Join(args, ", ")), nil
	}
	return "", fmt.Errorf("unsupported call to %s", name)
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Call != nil:
		return c.compileNamedCall(p.Call)
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

func (c *Compiler) compileNamedCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch call.Func {
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("printf(\"%s\\n\", %s)", "%s", args[0]), nil
		}
		format := strings.TrimSpace(strings.Repeat("%s ", len(args)))
		return fmt.Sprintf("printf(\"%s\\n\", %s)", format, strings.Join(args, ", ")), nil
	default:
		return "", fmt.Errorf("unsupported function %s", call.Func)
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str), nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "1", nil
		}
		return "0", nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
