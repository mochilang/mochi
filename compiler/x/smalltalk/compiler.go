//go:build slow

package smalltalk

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
)

// Compiler translates a limited subset of Mochi into GNU Smalltalk code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
}

// New returns a new Smalltalk compiler.
func New() *Compiler { return &Compiler{} }

// Compile converts the given Mochi program into Smalltalk source code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	vars := collectVars(p.Statements)
	if len(vars) > 0 {
		c.writeln("| " + strings.Join(vars, " ") + " |")
	}
	for _, st := range p.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func collectVars(st []*parser.Statement) []string {
	m := map[string]bool{}
	var walk func([]*parser.Statement)
	walk = func(ss []*parser.Statement) {
		for _, s := range ss {
			switch {
			case s.Let != nil:
				m[s.Let.Name] = true
			case s.Var != nil:
				m[s.Var.Name] = true
			case s.Fun != nil:
				m[s.Fun.Name] = true
				walk(s.Fun.Body)
			case s.If != nil:
				walk(s.If.Then)
				if s.If.Else != nil {
					walk(s.If.Else)
				}
				if s.If.ElseIf != nil {
					walk(s.If.ElseIf.Then)
					if s.If.ElseIf.Else != nil {
						walk(s.If.ElseIf.Else)
					}
				}
			case s.While != nil:
				walk(s.While.Body)
			case s.For != nil:
				walk(s.For.Body)
			}
		}
	}
	walk(st)
	names := make([]string, 0, len(m))
	for k := range m {
		names = append(names, k)
	}
	sort.Strings(names)
	return names
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ".")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
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
		return nil
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", l.Name, val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	if v.Value == nil {
		return nil
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", v.Name, val))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	if len(a.Index) > 0 || len(a.Field) > 0 {
		return fmt.Errorf("complex assignment not supported")
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", a.Name, val))
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = ":" + p.Name
	}
	body, err := c.blockString(f.Body)
	if err != nil {
		return err
	}
	block := fmt.Sprintf("[%s | %s ]", strings.Join(params, " "), body)
	c.writeln(fmt.Sprintf("%s := %s.", f.Name, block))
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(%s) ifTrue: [", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else == nil {
		c.writeln("] .")
		return nil
	}
	c.writeln("] ifFalse: [")
	c.indent++
	for _, st := range i.Else {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("].")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("[%s] whileTrue: [", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("].")
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
		c.writeln(fmt.Sprintf("%s to: %s do: [:%s |", start, end, f.Name))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s do: [:%s |", src, f.Name))
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("].")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
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
		switch op.Op {
		case "&&":
			res = fmt.Sprintf("(%s and: [%s])", res, r)
		case "||":
			res = fmt.Sprintf("(%s or: [%s])", res, r)
		case "==":
			res = fmt.Sprintf("%s = %s", res, r)
		case "!=":
			res = fmt.Sprintf("%s ~= %s", res, r)
		default:
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
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
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = val + " not"
		}
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
				if len(args) != 1 {
					return "", fmt.Errorf("print expects 1 arg")
				}
				val = fmt.Sprintf("Transcript show: (%s) printString; cr", args[0])
			} else {
				call := val
				for _, a := range args {
					call += " value: " + a
				}
				val = call
			}
		} else {
			return "", fmt.Errorf("unsupported postfix expression")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Selector != nil:
		name := p.Selector.Root
		for _, s := range p.Selector.Tail {
			name += "." + s
		}
		return name, nil
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
		return "{" + strings.Join(elems, ". ") + "}", nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		if p.Call.Func == "print" {
			if len(args) != 1 {
				return "", fmt.Errorf("print expects 1 arg")
			}
			return fmt.Sprintf("Transcript show: (%s) printString; cr", args[0]), nil
		}
		call := p.Call.Func
		for _, a := range args {
			call += " value: " + a
		}
		return call, nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
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
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	default:
		return "nil"
	}
}

func (c *Compiler) blockString(stmts []*parser.Statement) (string, error) {
	sub := &Compiler{}
	for _, st := range stmts {
		if err := sub.compileStmt(st); err != nil {
			return "", err
		}
	}
	return strings.TrimSpace(sub.buf.String()), nil
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
