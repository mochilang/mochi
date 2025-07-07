package dart

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
)

// Compiler generates Dart code from a Mochi AST.
type Compiler struct {
	buf    bytes.Buffer
	indent int
}

// New returns a new Compiler instance.
func New() *Compiler { return &Compiler{} }

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile translates prog into Dart source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("void main() {")
	c.indent++
	for _, s := range prog.Statements {
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
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr + ";")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	expr, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	if l.Type != nil && l.Type.Simple != nil {
		c.writeln(fmt.Sprintf("%s %s = %s;", *l.Type.Simple, l.Name, expr))
	} else {
		c.writeln(fmt.Sprintf("var %s = %s;", l.Name, expr))
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	expr, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type != nil && v.Type.Simple != nil {
		c.writeln(fmt.Sprintf("%s %s = %s;", *v.Type.Simple, v.Name, expr))
	} else {
		c.writeln(fmt.Sprintf("var %s = %s;", v.Name, expr))
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("%s %s %s", expr, op.Op, rhs)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = u.Ops[i] + expr
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
		case op.Field != nil:
			expr = fmt.Sprintf("%s.%s", expr, op.Field.Name)
		case op.Index != nil:
			if op.Index.Start != nil {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s[%s]", expr, idx)
			}
		case op.Cast != nil:
			// ignore casts for now
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		return strings.Join(append([]string{p.Selector.Root}, p.Selector.Tail...), "."), nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		return fmt.Sprint(*l.Float), nil
	case l.Str != nil:
		return fmt.Sprintf("'%s'", *l.Str), nil
	case l.Bool != nil:
		v := bool(*l.Bool)
		if v {
			return "true", nil
		}
		return "false", nil
	case l.Null:
		return "null", nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}
