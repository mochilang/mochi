package racket

import (
	"bytes"
	"fmt"

	"mochi/parser"
)

type Compiler struct {
	buf bytes.Buffer
}

func New() *Compiler { return &Compiler{} }

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("#lang racket")
	for _, st := range prog.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	if c.buf.Len() == 0 {
		c.writeln("")
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) write(s string)   { c.buf.WriteString(s) }
func (c *Compiler) writeln(s string) { c.buf.WriteString(s); c.buf.WriteByte('\n') }

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := s.Let.Name
		expr := "0"
		if s.Let.Value != nil {
			v, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
	case s.Var != nil:
		name := s.Var.Name
		expr := "0"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
	case s.Expr != nil:
		e, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(e)
	case s.For != nil:
		return c.compileFor(s.For)
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	val, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	for _, op := range e.Binary.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		operator := op.Op
		if operator == "==" {
			operator = "="
		}
		val = fmt.Sprintf("(%s %s %s)", operator, val, rhs)
	}
	return val, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = fmt.Sprintf("(- %s)", val)
		case "!":
			val = fmt.Sprintf("(not %s)", val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	// ignore postfix operations for now
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Call != nil:
		if p.Call.Func == "print" {
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("print expects 1 arg")
			}
			arg, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(displayln %s)", arg), nil
		}
		return "", fmt.Errorf("unsupported call")
	case p.If != nil:
		cond, err := c.compileExpr(p.If.Cond)
		if err != nil {
			return "", err
		}
		thn, err := c.compileExpr(p.If.Then)
		if err != nil {
			return "", err
		}
		els, err := c.compileExpr(p.If.Else)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(if %s %s %s)", cond, thn, els), nil
	case p.Selector != nil:
		// simple variable access only
		return p.Selector.Root, nil
	default:
		return "", fmt.Errorf("unsupported primary")
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%v", *l.Float), nil
	case l.Bool != nil:
		if *l.Bool {
			return "#t", nil
		}
		return "#f", nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Null:
		return "'()", nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := f.Name
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end := ""
	if f.RangeEnd != nil {
		end, err = c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
	}
	if end != "" {
		c.writeln(fmt.Sprintf("(for ([%s (in-range %s %s)])", name, start, end))
	} else {
		c.writeln(fmt.Sprintf("(for ([%s %s])", name, start))
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln(")")
	return nil
}
