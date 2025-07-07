package scalacode

import (
	"bytes"
	"fmt"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

const indentStep = 2

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte(' ')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	name := strings.TrimSuffix(filepath.Base(prog.Pos.Filename), filepath.Ext(prog.Pos.Filename))
	if name == "" {
		name = "Main"
	}
	c.writeln(fmt.Sprintf("object %s {", name))
	c.indent += indentStep
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("def main(args: Array[String]): Unit = {")
	c.indent += indentStep
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	c.indent -= indentStep
	c.writeln("}")
	out := c.buf.Bytes()
	if len(out) == 0 || out[len(out)-1] != '\n' {
		out = append(out, '\n')
	}
	return out, nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Expr != nil:
		return c.compileExprStmt(s.Expr)
	default:
		return fmt.Errorf("line %d: unsupported statement", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	rhs := "0"
	if s.Value != nil {
		var err error
		rhs, err = c.compileExpr(s.Value)
		if err != nil {
			return err
		}
	}
	if s.Type != nil {
		typ := c.typeString(s.Type)
		c.writeln(fmt.Sprintf("val %s: %s = %s", s.Name, typ, rhs))
	} else {
		c.writeln(fmt.Sprintf("val %s = %s", s.Name, rhs))
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	rhs := "0"
	if s.Value != nil {
		var err error
		rhs, err = c.compileExpr(s.Value)
		if err != nil {
			return err
		}
	}
	if s.Type != nil {
		typ := c.typeString(s.Type)
		c.writeln(fmt.Sprintf("var %s: %s = %s", s.Name, typ, rhs))
	} else {
		c.writeln(fmt.Sprintf("var %s = %s", s.Name, rhs))
	}
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeString(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	ret := ""
	if fn.Return != nil {
		ret = ": " + c.typeString(fn.Return)
	}
	c.writeln(fmt.Sprintf("def %s(%s)%s = {", fn.Name, strings.Join(params, ", "), ret))
	c.indent += indentStep
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	expr, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent += indentStep
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	if s.ElseIf != nil {
		c.writeln("} else {")
		c.indent += indentStep
		if err := c.compileIf(s.ElseIf); err != nil {
			return err
		}
		c.indent -= indentStep
		c.writeln("}")
		return nil
	}
	if s.Else != nil {
		c.writeln("} else {")
		c.indent += indentStep
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent -= indentStep
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(s *parser.WhileStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent += indentStep
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(s *parser.ForStmt) error {
	start, err := c.compileExpr(s.Source)
	if err != nil {
		return err
	}
	loop := start
	if s.RangeEnd != nil {
		end, err := c.compileExpr(s.RangeEnd)
		if err != nil {
			return err
		}
		loop = fmt.Sprintf("%s to %s", start, end)
	}
	c.writeln(fmt.Sprintf("for(%s <- %s) {", s.Name, loop))
	c.indent += indentStep
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	expr, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	if len(s.Index) != 0 || len(s.Field) != 0 {
		return fmt.Errorf("line %d: complex assignment unsupported", s.Pos.Line)
	}
	c.writeln(fmt.Sprintf("%s = %s", s.Name, expr))
	return nil
}

func (c *Compiler) compileExprStmt(s *parser.ExprStmt) error {
	call, ok := callPattern(s.Expr)
	if ok && call.Func == "print" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("println(%s)", arg))
		return nil
	}
	expr, err := c.compileExpr(s.Expr)
	if err != nil {
		return err
	}
	c.writeln(expr)
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	s, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	for _, op := range e.Binary.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			s = fmt.Sprintf("%s %s %s", s, op.Op, r)
		default:
			return "", fmt.Errorf("line %d: unsupported operator %s", op.Pos.Line, op.Op)
		}
	}
	return s, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	s, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			s = fmt.Sprintf("%s%s", op, s)
		default:
			return "", fmt.Errorf("line %d: unsupported unary op %s", u.Pos.Line, op)
		}
	}
	return s, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	s, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				val, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = val
			}
			s = fmt.Sprintf("%s(%s)", s, strings.Join(args, ", "))
		} else {
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return s, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", fmt.Errorf("nil primary")
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.List != nil:
		return c.compileList(p.List)
	case p.Selector != nil:
		return c.compileSelector(p.Selector), nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", expr), nil
	default:
		return "", fmt.Errorf("line %d: unsupported expression", p.Pos.Line)
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
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
	default:
		return "null"
	}
}

func (c *Compiler) compileSelector(s *parser.SelectorExpr) string {
	if len(s.Tail) == 0 {
		return s.Root
	}
	parts := append([]string{s.Root}, s.Tail...)
	return strings.Join(parts, ".")
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch call.Func {
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("%s :+ %s", args[0], args[1]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("%s.size", args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileList(l *parser.ListLiteral) (string, error) {
	elems := make([]string, len(l.Elems))
	for i, e := range l.Elems {
		s, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		elems[i] = s
	}
	return fmt.Sprintf("List(%s)", strings.Join(elems, ", ")), nil
}

func (c *Compiler) typeString(t *parser.TypeRef) string {
	if t == nil {
		return "Any"
	}
	if t.Simple != nil {
		id := *t.Simple
		switch id {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Boolean"
		case "string":
			return "String"
		default:
			return id
		}
	}
	return "Any"
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}
