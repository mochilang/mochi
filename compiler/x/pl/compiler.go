package pl

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf      bytes.Buffer
	indent   int
	tmpCount int
}

func New() *Compiler { return &Compiler{} }

func (c *Compiler) newTmp() string {
	name := fmt.Sprintf("_V%d", c.tmpCount)
	c.tmpCount++
	return name
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func sanitizeVar(name string) string {
	name = strings.ReplaceAll(name, "-", "_")
	if name == "" {
		return "_"
	}
	if name[0] >= 'a' && name[0] <= 'z' {
		name = strings.ToUpper(name[:1]) + name[1:]
	}
	return name
}

func sanitizeAtom(name string) string {
	name = strings.ReplaceAll(strings.ToLower(name), "-", "_")
	return name
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln(":- style_check(-singleton).")
	c.writeln("main :-")
	c.indent++
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			c.writeln("% unsupported: " + err.Error())
		}
	}
	c.writeln("true.")
	c.indent--
	c.writeln(":- initialization(main, main).")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s,", sanitizeVar(s.Let.Name), val))
	case s.Var != nil:
		if s.Var.Value == nil {
			return fmt.Errorf("var without init")
		}
		val, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", sanitizeAtom(s.Var.Name), val))
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", sanitizeAtom(s.Assign.Name), val))
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Expr != nil:
		if call := getPrintCall(s.Expr.Expr); call != nil {
			if len(call.Args) != 1 {
				return fmt.Errorf("print with one arg supported")
			}
			arg, err := c.compileExpr(call.Args[0])
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("write(%s),", arg))
			c.writeln("nl,")
		} else {
			return fmt.Errorf("unsupported expression statement")
		}
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(%s ->", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if len(ifst.Else) > 0 {
		c.writeln(";")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("),")
	return nil
}

func (c *Compiler) compileWhile(ws *parser.WhileStmt) error {
	cond, err := c.compileExpr(ws.Cond)
	if err != nil {
		return err
	}
	c.writeln("repeat,")
	c.writeln(fmt.Sprintf("(%s ->", cond))
	c.indent++
	for _, st := range ws.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("fail")
	c.indent--
	c.writeln("; !),")
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	if fs.RangeEnd == nil {
		return fmt.Errorf("for without range not supported")
	}
	start, err := c.compileExpr(fs.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(fs.RangeEnd)
	if err != nil {
		return err
	}
	tmp := c.newTmp()
	c.writeln(fmt.Sprintf("%s is %s - 1,", tmp, end))
	c.writeln(fmt.Sprintf("(between(%s, %s, %s),", start, tmp, sanitizeVar(fs.Name)))
	c.indent++
	for _, st := range fs.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("fail")
	c.indent--
	c.writeln("; true),")
	return nil
}

func getPrintCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	if p.Target != nil && p.Target.Call != nil && p.Target.Call.Func == "print" {
		return p.Target.Call
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "0", nil
	}
	if len(e.Binary.Right) == 0 {
		return c.compileUnary(e.Binary.Left)
	}
	if len(e.Binary.Right) == 1 {
		left, err := c.compileUnary(e.Binary.Left)
		if err != nil {
			return "", err
		}
		op := e.Binary.Right[0]
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/":
			return fmt.Sprintf("(%s %s %s)", left, op.Op, right), nil
		case "%":
			return fmt.Sprintf("(%s mod %s)", left, right), nil
		case "==":
			return fmt.Sprintf("(%s =:= %s)", left, right), nil
		case "!=":
			return fmt.Sprintf("(%s =\\= %s)", left, right), nil
		case "<":
			return fmt.Sprintf("(%s < %s)", left, right), nil
		case "<=":
			return fmt.Sprintf("(%s =< %s)", left, right), nil
		case ">":
			return fmt.Sprintf("(%s > %s)", left, right), nil
		case ">=":
			return fmt.Sprintf("(%s >= %s)", left, right), nil
		case "&&":
			return fmt.Sprintf("(%s, %s)", left, right), nil
		case "||":
			return fmt.Sprintf("(%s ; %s)", left, right), nil
		}
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if len(u.Ops) > 1 {
		return "", fmt.Errorf("multiple unary ops")
	}
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	if len(u.Ops) == 1 {
		if u.Ops[0] == "-" {
			return fmt.Sprintf("(-%s)", val), nil
		}
		return "", fmt.Errorf("unsupported unary op")
	}
	return val, nil
}

func (c *Compiler) compilePostfix(pf *parser.PostfixExpr) (string, error) {
	if len(pf.Ops) > 0 {
		return "", fmt.Errorf("postfix ops not supported")
	}
	return c.compilePrimary(pf.Target)
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return sanitizeVar(p.Selector.Root), nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
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
	}
	return "", fmt.Errorf("unsupported primary")
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float), nil
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str), nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true", nil
		}
		return "false", nil
	default:
		return "", fmt.Errorf("unsupported literal")
	}
}
