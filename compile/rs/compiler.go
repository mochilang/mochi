package rscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Rust source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Rust compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

// Compile generates Rust source code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("fn main() {")
	c.indent++
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let, false)
	case s.Var != nil:
		return c.compileLet(&parser.LetStmt{Name: s.Var.Name, Value: s.Var.Value}, true)
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(s.Assign.Name), val))
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s;", expr))
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
		c.writeln("return " + expr + ";")
		return nil
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt, mutable bool) error {
	name := sanitizeName(s.Name)
	val := "Default::default()"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = v
	}
	mut := ""
	if mutable {
		mut = "mut "
	}
	c.writeln(fmt.Sprintf("let %s%s = %s;", mut, name, val))
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
	if i.Else != nil || i.ElseIf != nil {
		c.writeln("} else {")
		c.indent++
		if i.ElseIf != nil {
			if err := c.compileIf(i.ElseIf); err != nil {
				return err
			}
		} else {
			for _, s := range i.Else {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
		}
		c.indent--
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " {")
	c.indent++
	for _, s := range w.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	iter, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	name := sanitizeName(f.Name)
	c.writeln(fmt.Sprintf("for %s in %s {", name, iter))
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
		val = fmt.Sprintf("%s%s", u.Ops[i], val)
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
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			if val == "print" {
				val = fmt.Sprintf("println!(\"{}\", %s)", strings.Join(args, ", "))
			} else if val == "len" && len(args) == 0 {
				val = fmt.Sprintf("%s.len()", args[0])
			} else {
				val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			}
			continue
		}
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s[%s]", val, idx)
			continue
		}
	}
	return val, nil
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
		return "vec![" + strings.Join(elems, ", ") + "]", nil
	case p.Call != nil:
		return sanitizeName(p.Call.Func), nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			expr += "." + sanitizeName(t)
		}
		return expr, nil
	case p.Group != nil:
		e, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	default:
		return sanitizeName(p.Selector.Root), nil
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
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	default:
		return "()", nil
	}
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || r == '_' || (i > 0 && r >= '0' && r <= '9') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 {
		return "_"
	}
	return b.String()
}
