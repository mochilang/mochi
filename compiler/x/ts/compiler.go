//go:build slow

package tscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler converts a subset of Mochi into TypeScript source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile translates the program into TypeScript code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	for _, st := range p.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := sanitizeName(s.Let.Name)
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		if val == "" {
			c.writeln(fmt.Sprintf("let %s;", name))
		} else {
			c.writeln(fmt.Sprintf("let %s = %s;", name, val))
		}
	case s.Var != nil:
		name := sanitizeName(s.Var.Name)
		val, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		if val == "" {
			c.writeln(fmt.Sprintf("let %s;", name))
		} else {
			c.writeln(fmt.Sprintf("let %s = %s;", name, val))
		}
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(s.Assign.Name), val))
	case s.Expr != nil:
		val, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(val + ";")
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		if val == "" {
			c.writeln("return;")
		} else {
			c.writeln("return " + val + ";")
		}
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		cond, err := c.compileExpr(s.While.Cond)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("while (%s) {", cond))
		c.indent++
		for _, st := range s.While.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if len(ifst.Else) == 0 && ifst.ElseIf == nil {
		c.writeln("}")
		return nil
	}
	c.writeln("} else {")
	c.indent++
	if ifst.ElseIf != nil {
		// else if chain: treat as nested if within else
		if err := c.compileIf(ifst.ElseIf); err != nil {
			return err
		}
	} else {
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(fr *parser.ForStmt) error {
	if fr.RangeEnd != nil {
		start, err := c.compileExpr(fr.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fr.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (let %s = %s; %s < %s; %s++) {", fr.Name, start, fr.Name, end, fr.Name))
		c.indent++
		for _, st := range fr.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	src, err := c.compileExpr(fr.Source)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("for (const %s of %s) {", fr.Name, src))
	c.indent++
	for _, st := range fr.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := sanitizeName(fn.Name)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("function %s(%s) {", name, strings.Join(params, ", ")))
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

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	if e.Binary != nil {
		left, err := c.compileUnary(e.Binary.Left)
		if err != nil {
			return "", err
		}
		expr := left
		for _, op := range e.Binary.Right {
			rhs, err := c.compilePostfix(op.Right)
			if err != nil {
				return "", err
			}
			if op.Op == "in" {
				expr = fmt.Sprintf("(%s.includes(%s))", rhs, expr)
			} else {
				expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, rhs)
			}
		}
		return expr, nil
	}
	return "", fmt.Errorf("unsupported expression at line %d", e.Pos.Line)
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
		switch {
		case op.Index != nil:
			if op.Index.Start == nil {
				return "", fmt.Errorf("index without start")
			}
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon != nil {
				end, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s.slice(%s, %s)", expr, idx, end)
			} else {
				expr = fmt.Sprintf("%s[%s]", expr, idx)
			}
		case op.Field != nil:
			expr = fmt.Sprintf("%s.%s", expr, sanitizeName(op.Field.Name))
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
		case op.Cast != nil:
			// ignore casts
		default:
			return "", fmt.Errorf("unsupported postfix op")
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return compileLiteral(p.Lit), nil
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return sanitizeName(name), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Group != nil:
		s, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + s + ")", nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	default:
		return "", fmt.Errorf("unsupported primary at line %d", p.Pos.Line)
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
	switch call.Func {
	case "print":
		return fmt.Sprintf("console.log(%s)", strings.Join(args, ", ")), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("(%s).length", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("(%s.reduce((a,b)=>a+b,0))", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(%s.reduce((a,b)=>a+b,0)/%s.length)", args[0], args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("(%s.length)", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("String(%s)", args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
	}
	var buf bytes.Buffer
	buf.WriteString("(")
	buf.WriteString(strings.Join(params, ", "))
	buf.WriteString(") => {\n")
	c.indent++
	old := c.buf
	c.buf = buf
	for _, st := range fn.BlockBody {
		if err := c.compileStmt(st); err != nil {
			return "", err
		}
	}
	c.buf = old
	c.indent--
	buf.WriteString("}")
	return buf.String(), nil
}

func compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if *l.Bool {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Null:
		return "null"
	default:
		return ""
	}
}
