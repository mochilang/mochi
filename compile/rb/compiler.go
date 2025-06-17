package rbcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Ruby source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Ruby compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile generates Ruby code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

// --- Statements ---
func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val)
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("next")
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.If != nil:
		return c.compileIf(s.If)
	default:
		return nil
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	val := "nil"
	if l.Value != nil {
		v, err := c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln(fmt.Sprintf("%s = %s", sanitizeName(l.Name), val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	val := "nil"
	if v.Value != nil {
		e, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = e
	}
	c.writeln(fmt.Sprintf("%s = %s", sanitizeName(v.Name), val))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	lhs := sanitizeName(a.Name)
	for _, idx := range a.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, val))
	return nil
}

func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("def %s(%s)", sanitizeName(fn.Name), strings.Join(params, ", ")))
	c.indent++
	for _, s := range fn.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond)
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		cond2, _ := c.compileExpr(stmt.ElseIf.Cond)
		c.writeln("elsif " + cond2)
		c.indent++
		for _, s := range stmt.ElseIf.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	if len(stmt.Else) > 0 {
		c.writeln("else")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s...%s", name, start, end))
	} else {
		src, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s", name, src))
	}
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond)
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

// --- Expressions ---
func (c *Compiler) compileExpr(e *parser.Expr) (string, error) { return c.compileBinaryExpr(e.Binary) }

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", fmt.Errorf("nil binary expr")
	}
	res, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			res = fmt.Sprintf("(%s %s %s)", res, op.Op, right)
		default:
			return "", fmt.Errorf("unsupported operator %s", op.Op)
		}
	}
	return res, nil
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
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s[%s]", expr, idx)
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			argStr := strings.Join(args, ", ")
			switch expr {
			case "print":
				expr = fmt.Sprintf("puts([%s].join(' '))", argStr)
			case "len":
				if len(args) != 1 {
					return "", fmt.Errorf("len expects 1 arg")
				}
				expr = fmt.Sprintf("(%s).length", args[0])
			default:
				expr = fmt.Sprintf("%s(%s)", expr, argStr)
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			name += "." + sanitizeName(t)
		}
		return name, nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		argStr := strings.Join(args, ", ")
		switch p.Call.Func {
		case "print":
			return fmt.Sprintf("puts([%s].join(' '))", argStr), nil
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			return fmt.Sprintf("(%s).length", args[0]), nil
		default:
			return fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), argStr), nil
		}
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

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		return strconv.FormatFloat(*l.Float, 'f', -1, 64), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}
