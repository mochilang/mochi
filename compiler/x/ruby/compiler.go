//go:build slow

package rubycode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a subset of Mochi to Ruby code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Ruby compiler.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile converts a parsed Mochi program into Ruby source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileVarStmt(s.Let.Name, s.Let.Value)
	case s.Var != nil:
		return c.compileVarStmt(s.Var.Name, s.Var.Value)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val)
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("next")
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileVarStmt(name string, val *parser.Expr) error {
	value := "nil"
	if val != nil {
		v, err := c.compileExpr(val)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("def %s(%s)", fn.Name, strings.Join(params, ", ")))
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(fn.Body) == 0 {
		c.writeln("nil")
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	return c.compileIfChain("if", ifst)
}

func (c *Compiler) compileIfChain(keyword string, ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(keyword + " " + cond)
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(ifst.Then) == 0 {
		c.writeln("nil")
	}
	c.indent--
	if ifst.ElseIf != nil {
		return c.compileIfChain("elsif", ifst.ElseIf)
	}
	if ifst.Else != nil {
		c.writeln("else")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		if len(ifst.Else) == 0 {
			c.writeln("nil")
		}
		c.indent--
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileWhile(ws *parser.WhileStmt) error {
	cond, err := c.compileExpr(ws.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond)
	c.indent++
	for _, st := range ws.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(ws.Body) == 0 {
		c.writeln("nil")
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	if fs.RangeEnd != nil {
		start, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fs.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s..%s", fs.Name, start, end))
	} else {
		src, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s", fs.Name, src))
	}
	c.indent++
	for _, st := range fs.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(fs.Body) == 0 {
		c.writeln("nil")
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	if len(a.Index) > 0 {
		target := a.Name
		for _, idx := range a.Index {
			val, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			target = fmt.Sprintf("%s[%s]", target, val)
		}
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", target, val))
		return nil
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", a.Name, val))
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
		opStr := op.Op
		if opStr == "&&" {
			opStr = "&&"
		} else if opStr == "||" {
			opStr = "||"
		}
		res = fmt.Sprintf("%s %s %s", res, opStr, r)
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
			val = "!" + val
		default:
			return "", fmt.Errorf("unsupported unary op %s", u.Ops[i])
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
		switch {
		case op.Cast != nil:
			typ, _ := c.compileType(op.Cast.Type)
			switch typ {
			case "int":
				val = fmt.Sprintf("(%s).to_i", val)
			case "float":
				val = fmt.Sprintf("(%s).to_f", val)
			case "string":
				val = fmt.Sprintf("(%s).to_s", val)
			default:
				return "", fmt.Errorf("unsupported cast to %s", typ)
			}
		case op.Index != nil:
			if op.Index.Start == nil || op.Index.Colon != nil {
				return "", fmt.Errorf("complex indexing not supported")
			}
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s[%s]", val, idx)
		default:
			return "", fmt.Errorf("unsupported postfix at line %d", p.Target.Pos.Line)
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
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
	case p.Map != nil:
		parts := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s => %s", k, v)
		}
		return "{" + strings.Join(parts, ", ") + "}", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
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
		joined := strings.Join(args, ", ")
		if len(args) > 1 {
			return fmt.Sprintf("puts([%s].join(' '))", joined), nil
		}
		return fmt.Sprintf("puts(%s)", joined), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("%s + [%s]", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(%s.sum.to_f / %s.length)", args[0], args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("%s.length", args[0]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("%s.sum", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("%s[%s,%s-%s]", args[0], args[1], args[2], args[1]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("->(%s){ %s }", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileIfExpr(ix *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ix.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ix.Then)
	if err != nil {
		return "", err
	}
	if ix.ElseIf != nil {
		elseExpr, err := c.compileIfExpr(ix.ElseIf)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
	}
	elseCode := "nil"
	if ix.Else != nil {
		elseCode, err = c.compileExpr(ix.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseCode), nil
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
	case l.Null:
		return "nil"
	default:
		return "nil"
	}
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil || t.Simple == nil {
		return "", fmt.Errorf("unsupported type")
	}
	return *t.Simple, nil
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
