package swiftcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Swift source code (very limited subset).
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
	useAvg bool
}

func New(env *types.Env) *Compiler { return &Compiler{env: env} }

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.useAvg = false

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent = 0

	// function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main body
	c.writeln("func main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("main()")

	bodyBytes := c.buf.Bytes()

	c.buf = oldBuf
	c.indent = 0

	c.writeln("import Foundation")
	c.writeln("")
	if c.useAvg {
		c.writeln("func _avg<T: BinaryInteger>(_ arr: [T]) -> Double {")
		c.indent++
		c.writeln("if arr.isEmpty { return 0 }")
		c.writeln("var sum = 0.0")
		c.writeln("for v in arr { sum += Double(v) }")
		c.writeln("return sum / Double(arr.count)")
		c.indent--
		c.writeln("}")
		c.writeln("func _avg<T: BinaryFloatingPoint>(_ arr: [T]) -> Double {")
		c.indent++
		c.writeln("if arr.isEmpty { return 0 }")
		c.writeln("var sum = 0.0")
		c.writeln("for v in arr { sum += Double(v) }")
		c.writeln("return sum / Double(arr.count)")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	c.buf.Write(bodyBytes)
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("_ %s: %s", p.Name, c.compileType(p.Type))
	}
	ret := c.compileType(fn.Return)
	c.writeln(fmt.Sprintf("func %s(%s) -> %s {", fn.Name, strings.Join(params, ", "), ret))
	c.indent++
	for _, p := range fn.Params {
		c.writeln(fmt.Sprintf("var %s = %s", p.Name, p.Name))
	}
	if len(fn.Params) > 0 {
		c.writeln("")
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileType(t *parser.TypeRef) string {
	if t == nil {
		return "Void"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Bool"
		case "string":
			return "String"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return "[" + c.compileType(t.Generic.Args[0]) + "]"
		}
	}
	return "Any"
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		typ := ""
		if s.Let.Type != nil {
			typ = ": " + c.compileType(s.Let.Type)
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("let %s%s", s.Let.Name, typ))
		} else {
			c.writeln(fmt.Sprintf("let %s%s = %s", s.Let.Name, typ, expr))
		}
	case s.Var != nil:
		expr, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		typ := ""
		if s.Var.Type != nil {
			typ = ": " + c.compileType(s.Var.Type)
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("var %s%s", s.Var.Name, typ))
		} else {
			c.writeln(fmt.Sprintf("var %s%s = %s", s.Var.Name, typ, expr))
		}
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("return %s", expr))
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	default:
		// ignore unsupported statements
	}
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
		c.writeln(fmt.Sprintf("for %s in %s..<%s {", f.Name, start, end))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s {", f.Name, src))
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if ifst.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(ifst.ElseIf)
	}
	if len(ifst.Else) > 0 {
		c.buf.WriteString(" else {")
		c.buf.WriteByte('\n')
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while %s {", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	name := a.Name
	for _, idx := range a.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		name = fmt.Sprintf("%s[%s]", name, iexpr)
	}
	value, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
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
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s[%s]", expr, idx)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
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
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		switch p.Call.Func {
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			return fmt.Sprintf("%s.count", args[0]), nil
		case "count":
			if len(args) != 1 {
				return "", fmt.Errorf("count expects 1 arg")
			}
			return fmt.Sprintf("%s.count", args[0]), nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects 1 arg")
			}
			return fmt.Sprintf("String(%s)", args[0]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			c.useAvg = true
			return fmt.Sprintf("_avg(%s.map { Double($0) })", args[0]), nil
		case "input":
			if len(args) != 0 {
				return "", fmt.Errorf("input expects 0 args")
			}
			return "readLine() ?? \"\"", nil
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
		}
	}
	return "", fmt.Errorf("unsupported expression")
}
