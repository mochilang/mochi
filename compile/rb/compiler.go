package rbcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Ruby source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Ruby compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile generates Ruby code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

// --- Statements ---
func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val)
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("next")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	val := "nil"
	if l.Value != nil {
		v, err := c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln(fmt.Sprintf("%s = %s", sanitizeName(l.Name), val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	val := "nil"
	if v.Value != nil {
		e, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = e
	}
	c.writeln(fmt.Sprintf("%s = %s", sanitizeName(v.Name), val))
	return nil
}

func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
	if c.env != nil {
		c.env.SetFunc(fn.Name, fn)
	}
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("def %s(%s)", sanitizeName(fn.Name), strings.Join(params, ", ")))
	c.indent++
	for _, s := range fn.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond)
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		cond2, _ := c.compileExpr(stmt.ElseIf.Cond)
		c.writeln("elsif " + cond2)
		c.indent++
		for _, s := range stmt.ElseIf.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	if len(stmt.Else) > 0 {
		c.writeln("else")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s...%s", name, start, end))
	} else {
		src, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s", name, src))
	}
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond)
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)
	for _, idx := range s.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	val, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, val))
	return nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("->(%s){ %s }", strings.Join(params, ", "), expr), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

// --- Expressions ---
func (c *Compiler) compileExpr(e *parser.Expr) (string, error) { return c.compileBinaryExpr(e.Binary) }

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", fmt.Errorf("nil binary expr")
	}
	res, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			res = fmt.Sprintf("(%s %s %s)", res, op.Op, right)
		default:
			return "", fmt.Errorf("unsupported operator %s", op.Op)
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
		val = fmt.Sprintf("(%s%s)", u.Ops[i], val)
	}
	return val, nil
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
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			argStr := strings.Join(args, ", ")
			switch expr {
			case "print":
				expr = fmt.Sprintf("puts([%s].join(\" \"))", argStr)
			case "len", "count":
				if len(args) != 1 {
					return "", fmt.Errorf("%s expects 1 arg", expr)
				}
				expr = fmt.Sprintf("(%s).length", args[0])
			case "str":
				if len(args) != 1 {
					return "", fmt.Errorf("str expects 1 arg")
				}
				expr = fmt.Sprintf("(%s).to_s", args[0])
			case "avg":
				if len(args) != 1 {
					return "", fmt.Errorf("avg expects 1 arg")
				}
				expr = fmt.Sprintf("((%[1]s).length > 0 ? (%[1]s).sum(0.0) / (%[1]s).length : 0)", args[0])
			case "input":
				if len(args) != 0 {
					return "", fmt.Errorf("input expects no args")
				}
				expr = "STDIN.gets.to_s.strip"
			default:
				if _, ok := c.env.GetFunc(expr); ok {
					expr = fmt.Sprintf("%s(%s)", expr, argStr)
				} else {
					expr = fmt.Sprintf("%s.call(%s)", expr, argStr)
				}
			}
		}
	}
	return expr, nil
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
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			name += "." + sanitizeName(t)
		}
		return name, nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		argStr := strings.Join(args, ", ")
		name := sanitizeName(p.Call.Func)
		switch name {
		case "print":
			return fmt.Sprintf("puts([%s].join(\" \"))", argStr), nil
		case "len", "count":
			if len(args) != 1 {
				return "", fmt.Errorf("%s expects 1 arg", name)
			}
			return fmt.Sprintf("(%s).length", args[0]), nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects 1 arg")
			}
			return fmt.Sprintf("(%s).to_s", args[0]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			return fmt.Sprintf("((%[1]s).length > 0 ? (%[1]s).sum(0.0) / (%[1]s).length : 0)", args[0]), nil
		case "input":
			if len(args) != 0 {
				return "", fmt.Errorf("input expects no args")
			}
			return "STDIN.gets.to_s.strip", nil
		default:
			if _, ok := c.env.GetFunc(name); ok {
				return fmt.Sprintf("%s(%s)", name, argStr), nil
			}
			return fmt.Sprintf("%s.call(%s)", name, argStr), nil
		}
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s.new(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Group != nil:
		v, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil {
		return "", fmt.Errorf("unsupported query expression")
	}

	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	orig := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)
	c.env = child

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	varName := sanitizeName(q.Var)
	c.env = orig

	simple := sortExpr == "" && skipExpr == "" && takeExpr == ""

	var b strings.Builder
	b.WriteString("(begin\n")
	b.WriteString("\t_src = " + src + "\n")
	if simple {
		b.WriteString("\t_res = []\n")
	} else {
		b.WriteString("\t_items = []\n")
	}
	b.WriteString("\t_src.each do |" + varName + "|\n")
	if cond != "" {
		b.WriteString("\t\tnext unless (" + cond + ")\n")
	}
	if simple {
		b.WriteString("\t\t_res << (" + sel + ")\n")
	} else {
		b.WriteString("\t\t_items << " + varName + "\n")
	}
	b.WriteString("\tend\n")
	if !simple {
		if sortExpr != "" {
			b.WriteString("\t_items = _items.sort_by { |" + varName + "| " + sortExpr + " }\n")
		}
		if skipExpr != "" {
			b.WriteString("\t_items = _items.drop(" + skipExpr + ")\n")
		}
		if takeExpr != "" {
			b.WriteString("\t_items = _items.take(" + takeExpr + ")\n")
		}
		b.WriteString("\t_res = []\n")
		b.WriteString("\t_items.each do |" + varName + "|\n")
		b.WriteString("\t\t_res << (" + sel + ")\n")
		b.WriteString("\tend\n")
	} else {
		if sortExpr != "" {
			b.WriteString("\t_res = _res.sort_by { |" + varName + "| " + sortExpr + " }\n")
		}
		if skipExpr != "" {
			b.WriteString("\t_res = _res.drop(" + skipExpr + ")\n")
		}
		if takeExpr != "" {
			b.WriteString("\t_res = _res.take(" + takeExpr + ")\n")
		}
	}
	b.WriteString("\t_res\n")
	b.WriteString("end)")
	return b.String(), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return fmt.Errorf("union types not supported")
	}
	fields := make([]string, 0, len(t.Members))
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, ":"+sanitizeName(m.Field.Name))
		}
	}
	c.writeln(fmt.Sprintf("%s = Struct.new(%s, keyword_init: true)", sanitizeName(t.Name), strings.Join(fields, ", ")))
	return nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		return strconv.FormatFloat(*l.Float, 'f', -1, 64), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}
