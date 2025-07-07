package scalacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("object Main {")
	c.indent++
	c.writeln("def main(args: Array[String]): Unit = {")
	c.indent++
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
	out := c.buf.Bytes()
	if len(out) == 0 || out[len(out)-1] != '\n' {
		out = append(out, '\n')
	}
	return out, nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
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
	if len(p.Ops) > 0 {
		return "", fmt.Errorf("postfix operations not supported")
	}
	return s, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", fmt.Errorf("nil primary")
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
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
