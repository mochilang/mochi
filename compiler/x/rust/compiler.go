//go:build slow

package rustcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

// Compiler translates a subset of Mochi to Rust source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	helpers map[string]bool
	tmp     int
}

// New returns a new Compiler instance.
func New() *Compiler {
	return &Compiler{helpers: make(map[string]bool)}
}

// Compile converts a parsed Mochi program into Rust source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.helpers = map[string]bool{}
	c.writeln("fn main() {")
	c.indent++
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	// prepend helpers
	var out bytes.Buffer
	if c.helpers["append"] {
		out.WriteString("fn append<T: Clone>(mut v: Vec<T>, item: T) -> Vec<T> {\n")
		out.WriteString("    v.push(item);\n    v\n}\n\n")
	}
	if c.helpers["avg"] {
		out.WriteString("fn avg(v: Vec<i32>) -> f64 {\n    let sum: i32 = v.iter().sum();\n    sum as f64 / v.len() as f64\n}\n\n")
	}
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Fun != nil:
		return c.compileFun(s.Fun)
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
	c.writeln(fmt.Sprintf("let mut %s = %s;", l.Name, val))
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
	if i.Else != nil {
		c.writeln("} else {")
		c.indent++
		for _, s := range i.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	} else {
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	if f.RangeEnd != nil {
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s..%s {", f.Name, src, end))
	} else {
		c.writeln(fmt.Sprintf("for %s in %s {", f.Name, src))
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

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = fmt.Sprintf("%s: %s", p.Name, rustType(p.Type))
	}
	retTy := rustType(f.Return)
	c.writeln(fmt.Sprintf("fn %s(%s) -> %s {", f.Name, strings.Join(params, ", "), retTy))
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
	for _, op := range p.Ops {
		switch {
		case op.Cast != nil:
			rustTy := rustType(op.Cast.Type)
			switch rustTy {
			case "i32":
				val = fmt.Sprintf("%s.parse::<i32>().unwrap()", val)
			default:
				return "", fmt.Errorf("unsupported cast to %s", rustTy)
			}
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
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
		return "vec![" + strings.Join(elems, ", ") + "]", nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return p.Selector.Root, nil
		}
		return p.Selector.Root + "." + strings.Join(p.Selector.Tail, "."), nil
	case p.FunExpr != nil:
		return "0", fmt.Errorf("fun expr not supported")
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
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
	switch call.Func {
	case "print":
		fmtStr := strings.TrimSpace(strings.Repeat("{:?} ", len(args)))
		return fmt.Sprintf("println!(\"%s\", %s)", fmtStr, strings.Join(args, ", ")), nil
	case "append":
		c.helpers["append"] = true
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("append(%s, %s)", args[0], args[1]), nil
	case "avg":
		c.helpers["avg"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("avg(%s)", args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		} else {
			return "false"
		}
	default:
		return "()"
	}
}

func rustType(t *parser.TypeRef) string {
	if t == nil {
		return "i32"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i32"
		case "bool":
			return "bool"
		case "float":
			return "f64"
		case "string":
			return "String"
		}
	}
	return "i32"
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
