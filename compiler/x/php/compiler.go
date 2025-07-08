//go:build slow

package phpcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler converts Mochi AST to PHP source code.
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	needsJSON bool
	env       *types.Env
	helpers   map[string]bool
}

// New creates a new Compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile translates the program to PHP code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsJSON = false
	c.writeln("<?php")
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.emitRuntime()
	return c.buf.Bytes(), nil
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
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr + ";")
		}
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error { return c.compileVarStmt(l.Name, l.Value) }

func (c *Compiler) compileVar(v *parser.VarStmt) error { return c.compileVarStmt(v.Name, v.Value) }

func (c *Compiler) compileVarStmt(name string, val *parser.Expr) error {
	var value string
	if val != nil {
		var err error
		value, err = c.compileExpr(val)
		if err != nil {
			return err
		}
	} else {
		value = "null"
	}
	c.writeln(fmt.Sprintf("$%s = %s;", sanitizeName(name), value))
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
	c.writeln(fmt.Sprintf("$%s = %s;", sanitizeName(a.Name), val))
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = "$" + sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("function %s(%s) {", sanitizeName(fn.Name), strings.Join(params, ", ")))
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(fn.Body) == 0 {
		c.writeln("return;")
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val := "null"
	if r.Value != nil {
		v, err := c.compileExpr(r.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln("return " + val + ";")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(st.Then) == 0 {
		c.writeln("//pass")
	}
	c.indent--
	if st.ElseIf != nil {
		c.writeln("} elseif (" + cond + ") {/*unsupported*/}")
		return fmt.Errorf("elseif not supported")
	}
	if len(st.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(ws *parser.WhileStmt) error {
	cond, err := c.compileExpr(ws.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range ws.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	name := "$" + sanitizeName(fs.Name)
	if fs.RangeEnd != nil {
		start, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fs.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (%s = %s; %s <= %s; %s++) {", name, start, name, end, name))
	} else {
		src, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		if c.isMapExpr(fs.Source) {
			c.writeln(fmt.Sprintf("foreach (%s as %s => $_v) {", src, name))
		} else {
			c.writeln(fmt.Sprintf("foreach (%s as %s) {", src, name))
		}
	}
	c.indent++
	for _, st := range fs.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
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
		switch opStr {
		case "&&":
			opStr = "&&"
		case "||":
			opStr = "||"
		case "in":
			res = fmt.Sprintf("in_array(%s, %s)", left, r)
			left = res
			continue
		}
		res = fmt.Sprintf("%s %s %s", res, opStr, r)
		left = res
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
		case op.Index != nil:
			if op.Index.Start == nil || op.Index.Colon != nil {
				return "", fmt.Errorf("complex indexing not supported")
			}
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s[%s]", val, idx)
		case op.Field != nil:
			val = fmt.Sprintf("%s->%s", val, sanitizeName(op.Field.Name))
		case op.Cast != nil:
			if op.Cast.Type.Simple == nil {
				return "", fmt.Errorf("unsupported cast")
			}
			typ := *op.Cast.Type.Simple
			switch typ {
			case "int":
				val = fmt.Sprintf("(int)(%s)", val)
			case "float":
				val = fmt.Sprintf("(float)(%s)", val)
			case "string":
				val = fmt.Sprintf("(string)(%s)", val)
			default:
				val = fmt.Sprintf("(%s)(%s)", typ, val)
			}
		default:
			return "", fmt.Errorf("unsupported postfix")
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
		return "[" + strings.Join(parts, ", ") + "]", nil
	case p.Selector != nil:
		name := "$" + sanitizeName(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			name += "->" + sanitizeName(t)
		}
		return name, nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
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
		c.use("_print")
		return fmt.Sprintf("_print(%s)", strings.Join(args, ", ")), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("array_merge(%s, [%s])", args[0], args[1]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("count(%s)", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("count(%s)", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(count(%[1]s) ? array_sum(%[1]s)/count(%[1]s) : 0)", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("substr(%s, %s, %s)", args[0], args[1], args[2]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = "$" + sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("function(%s) { return %s; }", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
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
		return "null"
	default:
		return "null"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
