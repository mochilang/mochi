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
	buf      bytes.Buffer
	indent   int
	env      *types.Env
	tmpCount int
}

// New creates a new Ruby compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

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
		if s.Fun != nil || s.Type != nil {
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
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
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

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("module %s; end", name))
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			fields := []string{}
			for _, f := range v.Fields {
				fields = append(fields, ":"+sanitizeName(f.Name))
			}
			fieldList := strings.Join(fields, ", ")
			if fieldList != "" {
				fieldList += ", "
			}
			c.writeln(fmt.Sprintf("%s = Struct.new(%skeyword_init: true) do", vname, fieldList))
			c.indent++
			c.writeln(fmt.Sprintf("include %s", name))
			c.indent--
			c.writeln("end")
		}
		return nil
	}
	fields := []string{}
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, ":"+sanitizeName(m.Field.Name))
		}
	}
	fieldList := strings.Join(fields, ", ")
	if fieldList != "" {
		fieldList += ", "
	}
	c.writeln(fmt.Sprintf("%s = Struct.new(%skeyword_init: true)", name, fieldList))
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

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil {
		return "", fmt.Errorf("advanced query clauses not supported")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	iter := sanitizeName(q.Var)
	expr := fmt.Sprintf("(%s)", src)
	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s).select { |%s| %s }", expr, iter, cond)
	}
	if q.Sort != nil {
		val, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s).sort_by { |%s| %s }", expr, iter, val)
	}
	if q.Skip != nil {
		val, err := c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s).drop(%s)", expr, val)
	}
	if q.Take != nil {
		val, err := c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s).take(%s)", expr, val)
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	expr = fmt.Sprintf("(%s).map { |%s| %s }", expr, iter, sel)
	return expr, nil
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
               case "in":
                       res = fmt.Sprintf("(%s.include?(%s))", right, res)
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
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
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
		q, err := c.compileQueryExpr(p.Query)
		if err != nil {
			return "", err
		}
		return q, nil
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
       case p.Map != nil:
               items := make([]string, len(p.Map.Items))
               for i, it := range p.Map.Items {
                       k, err := c.compileExpr(it.Key)
                       if err != nil {
                               return "", err
                       }
                       v, err := c.compileExpr(it.Value)
                       if err != nil {
                               return "", err
                       }
                       items[i] = fmt.Sprintf("%s => %s", k, v)
               }
               return "{" + strings.Join(items, ", ") + "}", nil
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

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	tmp := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++
	var b strings.Builder
	b.WriteString("(begin\n")
	b.WriteString(fmt.Sprintf("\t%s = %s\n", tmp, target))
	for i, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			if i == 0 {
				b.WriteString("\t" + res + "\n")
				b.WriteString("end)")
				return b.String(), nil
			}
			b.WriteString("\telse\n")
			b.WriteString("\t\t" + res + "\n")
			b.WriteString("\tend\nend)")
			return b.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("%s.is_a?(%s)", tmp, sanitizeName(call.Func))
				names := []string{}
				values := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						names = append(names, sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						values = append(values, fmt.Sprintf("%s.%s", tmp, field))
					}
				}
				if len(names) > 0 {
					res = fmt.Sprintf("(->(%s){ %s }).call(%s)", strings.Join(names, ", "), res, strings.Join(values, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("%s.is_a?(%s)", tmp, sanitizeName(ident))
			}
		}
		if cond == "" {
			pat, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cond = fmt.Sprintf("%s == %s", tmp, pat)
		}
		if i == 0 {
			b.WriteString("\tif " + cond + "\n")
		} else {
			b.WriteString("\telsif " + cond + "\n")
		}
		b.WriteString("\t\t" + res + "\n")
	}
	b.WriteString("\telse\n\t\tnil\n\tend\nend)")
	return b.String(), nil
}
