package luacode

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
)

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}

	operands := []string{left}
	ops := []string{}
	for _, part := range b.Right {
		right, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
		operands = append(operands, right)
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}, {"union", "union_all", "except", "intersect"}}
	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				opstr := ops[i]
				var expr string
				switch opstr {
				case "&&":
					expr = fmt.Sprintf("(%s and %s)", l, r)
				case "||":
					expr = fmt.Sprintf("(%s or %s)", l, r)
				case "in":
					c.helpers["contains"] = true
					expr = fmt.Sprintf("__contains(%s, %s)", r, l)
				case "/":
					c.helpers["div"] = true
					expr = fmt.Sprintf("__div(%s, %s)", l, r)
				case "+":
					c.helpers["add"] = true
					expr = fmt.Sprintf("__add(%s, %s)", l, r)
				case "==":
					c.helpers["eq"] = true
					expr = fmt.Sprintf("__eq(%s, %s)", l, r)
				case "!=":
					c.helpers["eq"] = true
					expr = fmt.Sprintf("not __eq(%s, %s)", l, r)
				case "union_all":
					c.helpers["union_all"] = true
					expr = fmt.Sprintf("__union_all(%s, %s)", l, r)
				case "union":
					c.helpers["union"] = true
					expr = fmt.Sprintf("__union(%s, %s)", l, r)
				case "except":
					c.helpers["except"] = true
					expr = fmt.Sprintf("__except(%s, %s)", l, r)
				case "intersect":
					c.helpers["intersect"] = true
					expr = fmt.Sprintf("__intersect(%s, %s)", l, r)
				default:
					expr = fmt.Sprintf("(%s %s %s)", l, opstr, r)
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return "", fmt.Errorf("invalid expression")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			expr = "-" + expr
		case "!":
			expr = "not " + expr
		}
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
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := "nil"
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				c.helpers["slice"] = true
				expr = fmt.Sprintf("__slice(%s, %s, %s)", expr, start, end)
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if isStringLiteral(op.Index.Start) {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				} else {
					c.helpers["index"] = true
					c.helpers["indexString"] = true
					expr = fmt.Sprintf("__index(%s, %s)", expr, idx)
				}
			}
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
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
		return "{" + strings.Join(elems, ", ") + "}", nil
	case p.Map != nil:
		pairs := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs[i] = fmt.Sprintf("[%s]=%s", k, v)
		}
		return "{" + strings.Join(pairs, ", ") + "}", nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.Struct != nil:
		if c.env != nil {
			if _, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
				parts := make([]string, 0, len(p.Struct.Fields)+1)
				parts = append(parts, fmt.Sprintf("__name=%q", p.Struct.Name))
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					parts = append(parts, fmt.Sprintf("%s=%s", sanitizeName(f.Name), v))
				}
				return "{" + strings.Join(parts, ", ") + "}", nil
			}
		}
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s=%s", sanitizeName(f.Name), v)
		}
		return "{" + strings.Join(parts, ", ") + "}", nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	if name == "len" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("#%s", arg), nil
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")
	switch name {
	case "print":
		c.helpers["print"] = true
		return fmt.Sprintf("__print(%s)", argStr), nil
	case "str":
		if len(args) == 1 {
			return fmt.Sprintf("tostring(%s)", args[0]), nil
		}
		return fmt.Sprintf("tostring(%s)", argStr), nil
	case "input":
		c.helpers["input"] = true
		return "__input()", nil
	case "count":
		c.helpers["count"] = true
		return fmt.Sprintf("__count(%s)", argStr), nil
	case "avg":
		c.helpers["avg"] = true
		return fmt.Sprintf("__avg(%s)", argStr), nil
	case "now":
		return "os.time()*1000000000", nil
	case "json":
		c.helpers["json"] = true
		return fmt.Sprintf("__json(%s)", argStr), nil
	case "eval":
		c.helpers["eval"] = true
		if len(args) == 1 {
			return fmt.Sprintf("__eval(%s)", args[0]), nil
		}
		return fmt.Sprintf("__eval(%s)", argStr), nil
	default:
		return fmt.Sprintf("%s(%s)", name, argStr), nil
	}
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	group := q.Group != nil
	for _, j := range q.Joins {
		if j.Side != nil {
			return "", fmt.Errorf("join sides not supported")
		}
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	iter := sanitizeName(q.Var)
	var whereCond, sortExpr, skipExpr, takeExpr string
	if group {
		if _, err = c.compileExpr(q.Group.Expr); err != nil {
			return "", err
		}
	}
	if q.Where != nil {
		whereCond, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}

	var b strings.Builder
	b.WriteString("(function()\n")

	// handle simple joins/cross joins without sort/skip/take
	if !group && (len(q.Froms) > 0 || len(q.Joins) > 0) && sortExpr == "" && skipExpr == "" && takeExpr == "" {
		b.WriteString("\tlocal _res = {}\n")
		b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(%s) do\n", iter, src))
		indent := "\t\t"
		for _, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(f.Var), fs))
			indent += "\t"
		}
		for _, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(j.Var), js))
			indent += "\t"
		}
		if whereCond != "" {
			b.WriteString(fmt.Sprintf(indent+"if %s then\n", whereCond))
			indent += "\t"
		}
		b.WriteString(indent + "table.insert(_res, " + sel + ")\n")
		for i := 0; i < len(q.Froms)+len(q.Joins); i++ {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		}
		if whereCond != "" {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		}
		b.WriteString("\treturn _res\n")
		b.WriteString("end)()")
		return b.String(), nil
	}

	b.WriteString("\tlocal _res = {}\n")
	b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(%s) do\n", iter, src))
	b.WriteString("\t\tlocal _row = {}\n")
	b.WriteString(fmt.Sprintf("\t\t_row.v = %s\n", iter))
	b.WriteString("\t\t_res[#_res+1] = _row\n")
	b.WriteString("\tend\n")
	b.WriteString("\treturn _res\n")
	b.WriteString("end)()")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	tmp := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++
	var b strings.Builder
	b.WriteString("(function()\n")
	b.WriteString("\tlocal " + tmp + " = " + target + "\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + "\n")
			b.WriteString("end)()")
			return b.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("%s.__name == \"%s\"", tmp, call.Func)
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
					res = fmt.Sprintf("(function(%s) return %s end)(%s)", strings.Join(names, ", "), res, strings.Join(values, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("%s.__name == \"%s\"", tmp, ident)
			}
		}
		if cond == "" {
			pat, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cond = fmt.Sprintf("%s == %s", tmp, pat)
		}
		b.WriteString(fmt.Sprintf("\tif %s then return %s end\n", cond, res))
	}
	b.WriteString("\treturn nil\n")
	b.WriteString("end)()")
	return b.String(), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var (
		prompt string
		text   string
		model  string
		params []string
	)
	for _, f := range g.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		switch f.Name {
		case "prompt":
			prompt = v
		case "text":
			text = v
		case "model":
			model = v
		default:
			params = append(params, fmt.Sprintf("%q=%s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "nil"
	if len(params) > 0 {
		paramStr = "{" + strings.Join(params, ", ") + "}"
	}
	if model == "" {
		model = "nil"
	}
	if g.Target == "embedding" {
		c.helpers["gen_embed"] = true
		return fmt.Sprintf("__gen_embed(%s, %s, %s)", text, model, paramStr), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.helpers["gen_struct"] = true
			return fmt.Sprintf("__gen_struct(%s, %s, %s)", prompt, model, paramStr), nil
		}
	}
	c.helpers["gen_text"] = true
	return fmt.Sprintf("__gen_text(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	sub := &Compiler{env: c.env, helpers: c.helpers}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		for _, st := range fn.BlockBody {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return "function(" + strings.Join(params, ", ") + ")\n" + body + "end", nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "nil"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.helpers["load"] = true
	return fmt.Sprintf("__load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "nil"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.helpers["save"] = true
	return fmt.Sprintf("__save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.helpers["fetch"] = true
	return fmt.Sprintf("__fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileLiteral(lit *parser.Literal) (string, error) {
	switch {
	case lit.Int != nil:
		return strconv.Itoa(*lit.Int), nil
	case lit.Float != nil:
		s := strconv.FormatFloat(*lit.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") && !strings.Contains(s, ".") {
			s += ".0"
		}
		return s, nil
	case lit.Str != nil:
		return strconv.Quote(*lit.Str), nil
	case lit.Bool != nil:
		if *lit.Bool {
			return "true", nil
		}
		return "false", nil
	}
	return "nil", fmt.Errorf("unknown literal")
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil || p.Lit == nil || p.Lit.Str == nil {
		return false
	}
	return true
}
