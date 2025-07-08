//go:build slow

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
	vars     map[string]string
}

func New() *Compiler { return &Compiler{vars: make(map[string]string)} }

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
	c.vars = make(map[string]string)
	c.writeln(":- style_check(-singleton).")

	// compile function definitions first
	for _, st := range prog.Statements {
		if st.Fun != nil {
			if err := c.compileFun(st.Fun); err != nil {
				c.writeln("% unsupported fun: " + err.Error())
			}
			c.writeln("")
		}
	}

	// main body
	c.writeln("main :-")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
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
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Let != nil:
		if call := getSimpleCall(s.Let.Value); call != nil && call.Func == "exists" && len(call.Args) == 1 {
			if q := getQuery(call.Args[0]); q != nil {
				if err := c.compileExists(s.Let.Name, q); err != nil {
					return err
				}
				return nil
			}
		}
		if call := getSimpleCall(s.Let.Value); call != nil && call.Func == "append" && len(call.Args) == 2 {
			listExpr, err := c.compileExpr(call.Args[0])
			if err != nil {
				return err
			}
			elemExpr, err := c.compileExpr(call.Args[1])
			if err != nil {
				return err
			}
			name := sanitizeVar(s.Let.Name)
			c.writeln(fmt.Sprintf("append(%s, [%s], %s),", listExpr, elemExpr, name))
			c.vars[s.Let.Name] = name
			return nil
		}
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeVar(s.Let.Name)
		c.writeln(fmt.Sprintf("%s is %s,", name, val))
		c.vars[s.Let.Name] = name
	case s.Var != nil:
		if s.Var.Value == nil {
			return fmt.Errorf("var without init")
		}
		if call := getSimpleCall(s.Var.Value); call != nil && call.Func == "append" && len(call.Args) == 2 {
			listExpr, err := c.compileExpr(call.Args[0])
			if err != nil {
				return err
			}
			elemExpr, err := c.compileExpr(call.Args[1])
			if err != nil {
				return err
			}
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("append(%s, [%s], %s),", listExpr, elemExpr, tmp))
			c.vars[s.Var.Name] = tmp
			return nil
		}
		val, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("%s is %s,", tmp, val))
		c.vars[s.Var.Name] = tmp
	case s.Assign != nil:
		if call := getSimpleCall(s.Assign.Value); call != nil && call.Func == "append" && len(call.Args) == 2 {
			listExpr, err := c.compileExpr(call.Args[0])
			if err != nil {
				return err
			}
			elemExpr, err := c.compileExpr(call.Args[1])
			if err != nil {
				return err
			}
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("append(%s, [%s], %s),", listExpr, elemExpr, tmp))
			c.vars[s.Assign.Name] = tmp
			return nil
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("%s is %s,", tmp, val))
		c.vars[s.Assign.Name] = tmp
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
			if acall := getSimpleCall(call.Args[0]); acall != nil && acall.Func == "append" && len(acall.Args) == 2 {
				listExpr, err := c.compileExpr(acall.Args[0])
				if err != nil {
					return err
				}
				elemExpr, err := c.compileExpr(acall.Args[1])
				if err != nil {
					return err
				}
				tmp := c.newTmp()
				c.writeln(fmt.Sprintf("append(%s, [%s], %s),", listExpr, elemExpr, tmp))
				c.writeln(fmt.Sprintf("write(%s),", tmp))
				c.writeln("nl,")
				return nil
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
		src, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(member(%s, %s),", sanitizeVar(fs.Name), src))
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

func getSimpleCall(e *parser.Expr) *parser.CallExpr {
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
	if p.Target != nil && p.Target.Call != nil {
		return p.Target.Call
	}
	return nil
}

func getQuery(e *parser.Expr) *parser.QueryExpr {
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
	return p.Target.Query
}

func (c *Compiler) compileExists(name string, q *parser.QueryExpr) error {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return err
	}
	varName := sanitizeVar(q.Var)
	old := c.vars
	c.vars = make(map[string]string)
	for k, v := range old {
		c.vars[k] = v
	}
	c.vars[q.Var] = varName
	cond := "true"
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.vars = old
			return err
		}
	}
	c.vars = old
	target := sanitizeVar(name)
	c.writeln(fmt.Sprintf("(once((member(%s, %s), %s)) -> %s = true ; %s = false),", varName, src, cond, target, target))
	c.vars[name] = target
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "0", nil
	}
	res, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	for _, op := range e.Binary.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/":
			res = fmt.Sprintf("(%s %s %s)", res, op.Op, rhs)
		case "%":
			res = fmt.Sprintf("(%s mod %s)", res, rhs)
		case "==":
			res = fmt.Sprintf("(%s =:= %s)", res, rhs)
		case "!=":
			res = fmt.Sprintf("(%s =\\= %s)", res, rhs)
		case "<":
			res = fmt.Sprintf("(%s < %s)", res, rhs)
		case "<=":
			res = fmt.Sprintf("(%s =< %s)", res, rhs)
		case ">":
			res = fmt.Sprintf("(%s > %s)", res, rhs)
		case ">=":
			res = fmt.Sprintf("(%s >= %s)", res, rhs)
		case "&&":
			res = fmt.Sprintf("(%s, %s)", res, rhs)
		case "||":
			res = fmt.Sprintf("(%s ; %s)", res, rhs)
		default:
			return "", fmt.Errorf("unsupported op")
		}
	}
	return res, nil
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
		if v, ok := c.vars[p.Selector.Root]; ok {
			return v, nil
		}
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

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeVar(p.Name)
	}
	ret := c.newTmp()
	oldVars := c.vars
	c.vars = make(map[string]string)
	for i, p := range fn.Params {
		c.vars[p.Name] = params[i]
	}
	c.writeln(fmt.Sprintf("%s(%s, %s) :-", sanitizeAtom(fn.Name), strings.Join(params, ", "), ret))
	c.indent++
	for _, st := range fn.Body {
		if st.Return != nil {
			val, err := c.compileExpr(st.Return.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s = %s.", ret, val))
			c.indent--
			c.vars = oldVars
			return nil
		}
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("true.")
	c.indent--
	c.vars = oldVars
	return nil
}
