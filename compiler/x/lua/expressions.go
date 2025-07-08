//go:build slow

package luacode

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
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
	strs := []bool{c.isStringUnary(b.Left)}
	maps := []bool{c.isMapUnary(b.Left)}
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
		strs = append(strs, c.isStringPostfix(part.Right))
		maps = append(maps, c.isMapPostfix(part.Right))
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}, {"union", "union_all", "except", "intersect"}}
	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				opstr := ops[i]
				var expr string
				resStr := false
				switch opstr {
				case "&&":
					expr = fmt.Sprintf("(%s and %s)", l, r)
				case "||":
					expr = fmt.Sprintf("(%s or %s)", l, r)
				case "in":
					if maps[i+1] {
						expr = fmt.Sprintf("(%s[%s] ~= nil)", r, l)
					} else {
						c.helpers["contains"] = true
						expr = fmt.Sprintf("__contains(%s, %s)", r, l)
					}
					resStr = false
				case "/":
					c.helpers["div"] = true
					expr = fmt.Sprintf("__div(%s, %s)", l, r)
					resStr = false
				case "+":
					if strs[i] && strs[i+1] {
						expr = fmt.Sprintf("(%s .. %s)", l, r)
						resStr = true
					} else {
						c.helpers["add"] = true
						expr = fmt.Sprintf("__add(%s, %s)", l, r)
						resStr = strs[i] || strs[i+1]
					}
				case "==":
					c.helpers["eq"] = true
					expr = fmt.Sprintf("__eq(%s, %s)", l, r)
					resStr = false
				case "!=":
					c.helpers["eq"] = true
					expr = fmt.Sprintf("not __eq(%s, %s)", l, r)
					resStr = false
				case "union_all":
					c.helpers["union_all"] = true
					c.helpers["eq"] = true
					expr = fmt.Sprintf("__union_all(%s, %s)", l, r)
					resStr = false
				case "union":
					c.helpers["union"] = true
					c.helpers["eq"] = true
					expr = fmt.Sprintf("__union(%s, %s)", l, r)
					resStr = false
				case "except":
					c.helpers["except"] = true
					c.helpers["eq"] = true
					expr = fmt.Sprintf("__except(%s, %s)", l, r)
					resStr = false
				case "intersect":
					c.helpers["intersect"] = true
					c.helpers["eq"] = true
					expr = fmt.Sprintf("__intersect(%s, %s)", l, r)
					resStr = false
				default:
					expr = fmt.Sprintf("(%s %s %s)", l, opstr, r)
					resStr = false
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
				strs[i] = resStr
				strs = append(strs[:i+1], strs[i+2:]...)
				maps[i] = false
				maps = append(maps[:i+1], maps[i+2:]...)
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
			if strings.HasSuffix(expr, ".contains") && len(args) == 1 {
				c.helpers["contains"] = true
				base := strings.TrimSuffix(expr, ".contains")
				expr = fmt.Sprintf("__contains(%s, %s)", base, args[0])
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
			}
		} else if op.Cast != nil {
			if op.Cast.Type.Simple != nil && c.env != nil {
				if st, ok := c.resolveTypeRef(op.Cast.Type).(types.StructType); ok {
					expr = fmt.Sprintf("%s.new(%s)", sanitizeName(st.Name), expr)
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
		return "{" + strings.Join(elems, ", ") + "}", nil
	case p.Map != nil:
		pairs := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k string
			if s, ok := simpleStringKey(it.Key); ok {
				k = fmt.Sprintf("\"%s\"", s)
			} else {
				var err error
				k, err = c.compileExpr(it.Key)
				if err != nil {
					return "", err
				}
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
		if c.methodFields != nil && c.methodFields[p.Selector.Root] {
			name = "self." + name
		}
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
		if c.env != nil {
			if st, ok := c.env.GetStruct(p.Struct.Name); ok && len(st.Methods) > 0 {
				return fmt.Sprintf("%s.new({%s})", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
			}
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
	case p.If != nil:
		return c.compileIfExpr(p.If)
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
		at := c.inferExprType(call.Args[0])
		if isMap(at) || isString(at) {
			c.helpers["count"] = true
			return fmt.Sprintf("__count(%s)", arg), nil
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
		c.helpers["write_val"] = true
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
	case "contains":
		c.helpers["contains"] = true
		if len(args) != 2 {
			return "", fmt.Errorf("contains expects 2 args")
		}
		return fmt.Sprintf("__contains(%s, %s)", args[0], args[1]), nil
	case "exists":
		c.helpers["exists"] = true
		return fmt.Sprintf("__exists(%s)", argStr), nil
	case "avg":
		c.helpers["avg"] = true
		return fmt.Sprintf("__avg(%s)", argStr), nil
	case "sum":
		c.helpers["sum"] = true
		return fmt.Sprintf("__sum(%s)", argStr), nil
	case "min":
		c.helpers["min"] = true
		return fmt.Sprintf("__min(%s)", argStr), nil
	case "max":
		c.helpers["max"] = true
		return fmt.Sprintf("__max(%s)", argStr), nil
	case "abs":
		if len(args) != 1 {
			return "", fmt.Errorf("abs expects 1 arg")
		}
		return fmt.Sprintf("math.abs(%s)", args[0]), nil
	case "first":
		if len(args) != 1 {
			return "", fmt.Errorf("first expects 1 arg")
		}
		c.helpers["first"] = true
		return fmt.Sprintf("__first(%s)", args[0]), nil
	case "lower":
		if len(args) != 1 {
			return "", fmt.Errorf("lower expects 1 arg")
		}
		return fmt.Sprintf("string.lower(%s)", args[0]), nil
	case "substr":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		c.helpers["slice"] = true
		return fmt.Sprintf("__slice(%s, %s, %s)", args[0], args[1], args[2]), nil
	case "reverse":
		if len(args) != 1 {
			return "", fmt.Errorf("reverse expects 1 arg")
		}
		at := c.inferExprType(call.Args[0])
		if isString(at) {
			c.helpers["reverse_string"] = true
			return fmt.Sprintf("__reverse_string(%s)", args[0]), nil
		}
		if _, ok := at.(types.ListType); ok {
			c.helpers["reverse_list"] = true
			return fmt.Sprintf("__reverse_list(%s)", args[0]), nil
		}
		return "", fmt.Errorf("reverse expects string or list")
	case "concat":
		if len(args) < 2 {
			return "", fmt.Errorf("concat expects at least 2 args")
		}
		c.helpers["concat"] = true
		expr := fmt.Sprintf("__concat(%s, %s)", args[0], args[1])
		for i := 2; i < len(args); i++ {
			expr = fmt.Sprintf("__concat(%s, %s)", expr, args[i])
		}
		return expr, nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		c.helpers["append"] = true
		return fmt.Sprintf("__append(%s, %s)", args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		c.helpers["values"] = true
		return fmt.Sprintf("__values(%s)", args[0]), nil
	case "reduce":
		if len(args) != 3 {
			return "", fmt.Errorf("reduce expects 3 args")
		}
		c.helpers["reduce"] = true
		return fmt.Sprintf("__reduce(%s, %s, %s)", args[0], args[1], args[2]), nil
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
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		c.env = child
		var whereExpr string
		if q.Where != nil {
			whereExpr, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig
		c.helpers["_Group"] = true
		c.helpers["_group_by"] = true
		var b strings.Builder
		b.WriteString("(function()\n")
		if whereExpr != "" {
			b.WriteString("\tlocal _items = {}\n")
			b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(%s) do\n", sanitizeName(q.Var), src))
			b.WriteString(fmt.Sprintf("\t\tif %s then _items[#_items+1] = %s end\n", whereExpr, sanitizeName(q.Var)))
			b.WriteString("\tend\n")
			b.WriteString(fmt.Sprintf("\tlocal _groups = __group_by(_items, function(%s) return %s end)\n", sanitizeName(q.Var), keyExpr))
		} else {
			b.WriteString(fmt.Sprintf("\tlocal _groups = __group_by(%s, function(%s) return %s end)\n", src, sanitizeName(q.Var), keyExpr))
		}
		b.WriteString("\tlocal _res = {}\n")
		b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(_groups) do\n", sanitizeName(q.Group.Name)))
		b.WriteString(fmt.Sprintf("\t\t_res[#_res+1] = %s\n", valExpr))
		b.WriteString("\tend\n")
		b.WriteString("\treturn _res\n")
		b.WriteString("end)()")
		return b.String(), nil
	}

	if q.Group != nil {
		// Queries with joins and grouping are compiled using the helper
		// query and group_by functions. Only the main iter variable is
		// selected from the join results which matches the needs of the
		// TPC-DS queries.
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		c.env = child

		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			fromSrcs[i] = fs
			child.SetVar(f.Var, types.AnyType{}, true)
		}

		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		joinSides := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinSrcs[i] = js
			joinOns[i] = on
			if j.Side != nil {
				joinSides[i] = *j.Side
			}
			child.SetVar(j.Var, types.AnyType{}, true)
		}

		var whereExpr string
		if q.Where != nil {
			whereExpr, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}

		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}

		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig

		params := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			params = append(params, sanitizeName(f.Var))
		}
		paramCopy := append([]string(nil), params...)

		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{ items = %s }", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
			spec := fmt.Sprintf("{ items = %s, on = function(%s) return %s end", js, strings.Join(onParams, ", "), joinOns[i])
			side := joinSides[i]
			if side == "left" || side == "outer" {
				spec += ", left = true"
			}
			if side == "right" || side == "outer" {
				spec += ", right = true"
			}
			spec += " }"
			joins = append(joins, spec)
			paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
		}

		allParams := strings.Join(paramCopy, ", ")
		selectFn := fmt.Sprintf("function(%s) return {%s} end", allParams, strings.Join(paramCopy, ", "))
		var whereFn string
		if whereExpr != "" {
			whereFn = fmt.Sprintf("function(%s) return (%s) end", allParams, whereExpr)
		}
		keyFn := fmt.Sprintf("function(%s) return %s end", allParams, keyExpr)
		var valBuilder strings.Builder
		valBuilder.WriteString(fmt.Sprintf("function(%s) ", allParams))
		valBuilder.WriteString(fmt.Sprintf("local _row = __merge(%s)", strings.Join(paramCopy, ", ")))
		for _, v := range paramCopy {
			valBuilder.WriteString(fmt.Sprintf("; _row.%s = %s", v, v))
		}
		valBuilder.WriteString("; return _row end")
		valFn := valBuilder.String()

		var b strings.Builder
		b.WriteString("(function()\n")
		b.WriteString("\tlocal _src = " + src + "\n")
		b.WriteString("\tlocal _rows = __query(_src, {\n")
		for i, j := range joins {
			b.WriteString("\t\t" + j)
			if i != len(joins)-1 {
				b.WriteString(",")
			}
			b.WriteString("\n")
		}
		b.WriteString("\t}, { selectFn = " + selectFn)
		if whereFn != "" {
			b.WriteString(", where = " + whereFn)
		}
		b.WriteString(" })\n")
		b.WriteString(fmt.Sprintf("\tlocal _groups = __group_by_rows(_rows, %s, %s)\n", keyFn, valFn))
		b.WriteString("\tlocal _res = {}\n")
		b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(_groups) do\n", sanitizeName(q.Group.Name)))
		b.WriteString(fmt.Sprintf("\t\t_res[#_res+1] = %s\n", valExpr))
		b.WriteString("\tend\n")
		b.WriteString("\treturn _res\n")
		b.WriteString("end)()")
		c.helpers["query"] = true
		c.helpers["_Group"] = true
		c.helpers["_group_by_rows"] = true
		c.helpers["merge"] = true
		return b.String(), nil
	}

	if len(q.Joins) > 0 {
		return c.compileQueryWithHelper(q)
	}

	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	iter := sanitizeName(q.Var)

	var whereCond, sortExpr, skipExpr, takeExpr string
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
	b.WriteString("\tlocal _res = {}\n")
	if _, ok := c.inferExprType(q.Source).(types.GroupType); ok {
		b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(%s.items) do\n", iter, src))
	} else {
		b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(%s) do\n", iter, src))
	}
	indent := "\t\t"
	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(f.Var), fs))
		indent += "\t"
	}
	if whereCond != "" {
		b.WriteString(fmt.Sprintf(indent+"if %s then\n", whereCond))
		indent += "\t"
	}
	if sortExpr != "" {
		b.WriteString(fmt.Sprintf(indent+"_res[#_res+1] = {__key = %s, __val = %s}\n", sortExpr, sel))
	} else {
		b.WriteString(fmt.Sprintf(indent+"_res[#_res+1] = %s\n", sel))
	}
	if whereCond != "" {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
	}
	for _ = range q.Froms {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
	}
	b.WriteString("\tend\n")

	if sortExpr != "" || skipExpr != "" || takeExpr != "" {
		b.WriteString("\tlocal items = _res\n")
		if sortExpr != "" {
			b.WriteString("\ttable.sort(items, function(a,b) return a.__key < b.__key end)\n")
			b.WriteString("\tlocal tmp = {}\n")
			b.WriteString("\tfor _, it in ipairs(items) do tmp[#tmp+1] = it.__val end\n")
			b.WriteString("\titems = tmp\n")
		}
		if skipExpr != "" {
			b.WriteString(fmt.Sprintf("\tlocal skip = %s\n", skipExpr))
			b.WriteString("\tif skip < #items then\n")
			b.WriteString("\t\tfor i=1,skip do table.remove(items,1) end\n")
			b.WriteString("\telse\n")
			b.WriteString("\t\titems = {}\n")
			b.WriteString("\tend\n")
		}
		if takeExpr != "" {
			b.WriteString(fmt.Sprintf("\tlocal take = %s\n", takeExpr))
			b.WriteString("\tif take < #items then\n")
			b.WriteString("\t\tfor i=#items, take+1, -1 do table.remove(items) end\n")
			b.WriteString("\tend\n")
		}
		b.WriteString("\t_res = items\n")
	}

	b.WriteString("\treturn _res\n")
	b.WriteString("end)()")
	return b.String(), nil
}

func (c *Compiler) compileQueryWithHelper(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}

	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		joinOns[i] = on
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var whereExpr, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
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

	params := []string{sanitizeName(q.Var)}
	for _, f := range q.Froms {
		params = append(params, sanitizeName(f.Var))
	}
	paramCopy := append([]string(nil), params...)

	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	for _, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("{ items = %s }", fs))
	}
	for i, js := range joinSrcs {
		onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
		spec := fmt.Sprintf("{ items = %s, on = function(%s) return %s end", js, strings.Join(onParams, ", "), joinOns[i])
		side := joinSides[i]
		if side == "left" || side == "outer" {
			spec += ", left = true"
		}
		if side == "right" || side == "outer" {
			spec += ", right = true"
		}
		spec += " }"
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}

	allParams := strings.Join(paramCopy, ", ")
	selectFn := fmt.Sprintf("function(%s) return %s end", allParams, sel)
	var whereFn, sortFn string
	if whereExpr != "" {
		whereFn = fmt.Sprintf("function(%s) return (%s) end", allParams, whereExpr)
	}
	if sortExpr != "" {
		sortFn = fmt.Sprintf("function(%s) return (%s) end", allParams, sortExpr)
	}

	var b strings.Builder
	b.WriteString("(function()\n")
	b.WriteString("\tlocal _src = " + src + "\n")
	b.WriteString("\treturn __query(_src, {\n")
	for i, j := range joins {
		b.WriteString("\t\t" + j)
		if i != len(joins)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString("\t}, { selectFn = " + selectFn)
	if whereFn != "" {
		b.WriteString(", where = " + whereFn)
	}
	if sortFn != "" {
		b.WriteString(", sortKey = " + sortFn)
	}
	if skipExpr != "" {
		b.WriteString(", skip = " + skipExpr)
	}
	if takeExpr != "" {
		b.WriteString(", take = " + takeExpr)
	}
	b.WriteString(" })\n")
	b.WriteString("end)()")
	c.helpers["query"] = true
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

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
	}
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(function()\n")
	b.WriteString("\tif " + cond + " then\n")
	b.WriteString("\t\treturn " + thenExpr + "\n")
	if elseExpr != "" {
		b.WriteString("\telse\n")
		b.WriteString("\t\treturn " + elseExpr + "\n")
		b.WriteString("\tend\n")
	} else {
		b.WriteString("\tend\n")
		b.WriteString("\treturn nil\n")
	}
	b.WriteString("end)()")
	return b.String(), nil
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
	case lit.Null:
		return "nil", nil
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
