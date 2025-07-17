//go:build slow

package excode

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Expression compilation helpers extracted from compiler.go for readability.
func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	type operand struct {
		expr     string
		isList   bool
		isString bool
	}

	if b == nil {
		return "", fmt.Errorf("nil binary expression")
	}

	ops := []string{}
	operands := []operand{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, operand{expr: first, isList: isListUnary(b.Left, c.env), isString: isStringUnary(b.Left, c.env)})

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, operand{expr: r, isList: isListPostfix(part.Right, c.env), isString: isStringPostfix(part.Right, c.env)})
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(sl []string, s string) bool {
		for _, v := range sl {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if !contains(level, ops[i]) {
				i++
				continue
			}

			op := ops[i]
			l := operands[i]
			r := operands[i+1]

			var expr string
			var isList bool
			var isString bool

			switch op {
			case "+":
				if l.isList || r.isList {
					expr = fmt.Sprintf("%s ++ %s", l.expr, r.expr)
					isList = true
				} else if l.isString || r.isString {
					expr = fmt.Sprintf("(%s <> %s)", l.expr, r.expr)
					isString = true
				} else {
					expr = fmt.Sprintf("(%s + %s)", l.expr, r.expr)
				}
			case "-", "*", "/", "<", "<=", ">", ">=", "&&", "||":
				expr = fmt.Sprintf("(%s %s %s)", l.expr, op, r.expr)
			case "%":
				expr = fmt.Sprintf("rem(%s, %s)", l.expr, r.expr)
			case "==", "!=":
				expr = fmt.Sprintf("(%s %s %s)", l.expr, op, r.expr)
			case "in":
				if r.isString && l.isString {
					expr = fmt.Sprintf("String.contains?(%s, %s)", r.expr, l.expr)
				} else {
					expr = fmt.Sprintf("(if is_map(%s), do: Map.has_key?(%s, %s), else: Enum.member?(%s, %s))", r.expr, r.expr, l.expr, r.expr, l.expr)
				}
			case "union_all":
				if l.isList && r.isList {
					expr = fmt.Sprintf("(%s ++ %s)", l.expr, r.expr)
				} else {
					c.use("_union_all")
					expr = fmt.Sprintf("_union_all((%s), (%s))", l.expr, r.expr)
				}
				isList = true
			case "union":
				if l.isList && r.isList {
					expr = fmt.Sprintf("Enum.uniq(%s ++ %s)", l.expr, r.expr)
				} else {
					c.use("_union")
					expr = fmt.Sprintf("_union((%s), (%s))", l.expr, r.expr)
				}
				isList = true
			case "except":
				if l.isList && r.isList {
					expr = fmt.Sprintf("Enum.reject(%s, fn x -> Enum.member?(%s, x) end)", l.expr, r.expr)
				} else {
					c.use("_except")
					expr = fmt.Sprintf("_except((%s), (%s))", l.expr, r.expr)
				}
				isList = true
			case "intersect":
				if l.isList && r.isList {
					expr = fmt.Sprintf("Enum.filter(%s, fn x -> Enum.member?(%s, x) end) |> Enum.uniq()", l.expr, r.expr)
				} else {
					c.use("_intersect")
					expr = fmt.Sprintf("_intersect((%s), (%s))", l.expr, r.expr)
				}
				isList = true
			default:
				return "", fmt.Errorf("unsupported operator %s", op)
			}

			operands[i] = operand{expr: expr, isList: isList, isString: isString}
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state in binary expression")
	}

	return operands[0].expr, nil
}

func isListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	if p.Target != nil {
		if p.Target.List != nil {
			return true
		}
		if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
			if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	}
	return false
}

func isListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isListPostfix(u.Value, env)
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	if p.Target != nil {
		if p.Target.Lit != nil && p.Target.Lit.Str != nil {
			return true
		}
		if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
			if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isStringPostfix(u.Value, env)
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
			expr = "!" + expr
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	res, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	var typ types.Type
	if t, ok := c.staticTypeOfPostfix(p); ok {
		typ = t
	} else {
		typ = c.inferPrimaryType(p.Target)
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon == nil {
				switch tt := typ.(type) {
				case types.MapType:
					res = fmt.Sprintf("Map.get(%s, %s)", res, idx)
					typ = tt.Value
				case types.StringType:
					c.use("_index_string")
					res = fmt.Sprintf("_index_string(%s, %s)", res, idx)
				default:
					res = fmt.Sprintf("Enum.at((%s), %s)", res, idx)
					if lt, ok := tt.(types.ListType); ok {
						typ = lt.Elem
					}
				}
			} else {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("length(String.graphemes(%s))", res)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				switch typ.(type) {
				case types.ListType, types.GroupType:
					length := fmt.Sprintf("(%s) - %s", end, start)
					res = fmt.Sprintf("Enum.slice((%s), %s, %s)", res, start, length)
				default:
					c.use("_slice_string")
					res = fmt.Sprintf("_slice_string(%s, %s, %s)", res, start, end)
				}
			}
		} else if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
			argStr := strings.Join(args, ", ")
			if idx := strings.Index(res, "."); idx > 0 {
				alias := res[:idx]
				method := res[idx+1:]
				if kind, ok := c.builtinAliases[alias]; ok {
					switch kind {
					case "go_testpkg":
						switch method {
						case "Add":
							if len(args) == 2 {
								res = fmt.Sprintf("(%s + %s)", args[0], args[1])
								continue
							}
						}
					case "python_math":
						switch method {
						case "sqrt", "sin", "log":
							if len(args) == 1 {
								res = fmt.Sprintf(":math.%s(%s)", strings.ToLower(method), args[0])
								continue
							}
						case "pow":
							if len(args) == 2 {
								res = fmt.Sprintf(":math.pow(%s, %s)", args[0], args[1])
								continue
							}
						}
					}
				}
			}
			if strings.HasSuffix(res, ".contains") && len(args) == 1 {
				target := strings.TrimSuffix(res, ".contains")
				res = fmt.Sprintf("String.contains?(%s, %s)", target, argStr)
				continue
			}
			if strings.HasSuffix(res, ".starts_with") && len(args) == 1 {
				target := strings.TrimSuffix(res, ".starts_with")
				res = fmt.Sprintf("String.starts_with?(%s, %s)", target, argStr)
				continue
			}
			if strings.HasSuffix(res, ".keys") && len(args) == 0 {
				target := strings.TrimSuffix(res, ".keys")
				res = fmt.Sprintf("Map.keys(%s)", target)
				typ = types.ListType{Elem: types.AnyType{}}
			} else if strings.HasSuffix(res, ".values") && len(args) == 0 {
				target := strings.TrimSuffix(res, ".values")
				res = fmt.Sprintf("Map.values(%s)", target)
				typ = types.ListType{Elem: types.AnyType{}}
			} else {
				switch res {
				case "print":
					if len(args) == 1 {
						t := c.inferExprType(op.Call.Args[0])
						switch t.(type) {
						case types.StringType:
							res = fmt.Sprintf("IO.puts(%s)", args[0])
						case types.ListType, types.GroupType:
							res = fmt.Sprintf("IO.inspect(%s)", args[0])
						default:
							res = fmt.Sprintf("IO.inspect(%s)", args[0])
						}
					} else {
						res = fmt.Sprintf("IO.puts(Enum.join(Enum.map([%s], &inspect(&1)), \" \"))", argStr)
					}
				case "len":
					if len(args) != 1 {
						return "", fmt.Errorf("len expects 1 arg")
					}
					t := c.inferExprType(op.Call.Args[0])
					switch t.(type) {
					case types.StringType:
						res = fmt.Sprintf("String.length(%s)", args[0])
					case types.MapType:
						res = fmt.Sprintf("map_size(%s)", args[0])
					case types.GroupType:
						res = fmt.Sprintf("length(%s.items)", args[0])
					default:
						c.use("_length")
						res = fmt.Sprintf("_length(%s)", args[0])
					}
				case "count":
					if len(args) != 1 {
						return "", fmt.Errorf("count expects 1 arg")
					}
					t := c.inferExprType(op.Call.Args[0])
					switch t.(type) {
					case types.ListType:
						res = fmt.Sprintf("length(%s)", args[0])
					case types.GroupType:
						res = fmt.Sprintf("length(%s.items)", args[0])
					default:
						c.use("_count")
						res = fmt.Sprintf("_count(%s)", argStr)
					}
				case "exists":
					if len(args) != 1 {
						return "", fmt.Errorf("exists expects 1 arg")
					}
					t := c.inferExprType(op.Call.Args[0])
					switch t.(type) {
					case types.ListType:
						res = fmt.Sprintf("length(%s) > 0", args[0])
					case types.MapType:
						res = fmt.Sprintf("map_size(%s) > 0", args[0])
					case types.GroupType:
						res = fmt.Sprintf("length(%s.items) > 0", args[0])
					case types.StringType:
						res = fmt.Sprintf("String.length(%s) > 0", args[0])
					case types.OptionType:
						res = fmt.Sprintf("%s != nil", args[0])
					default:
						c.use("_exists")
						res = fmt.Sprintf("_exists(%s)", argStr)
					}
				case "avg":
					if len(args) != 1 {
						return "", fmt.Errorf("avg expects 1 arg")
					}
					t := c.inferExprType(op.Call.Args[0])
					switch tt := t.(type) {
					case types.ListType:
						if _, ok := tt.Elem.(types.IntType); ok {
							res = fmt.Sprintf("div(Enum.sum(%s), Enum.count(%s))", args[0], args[0])
						} else if _, ok := tt.Elem.(types.Int64Type); ok {
							res = fmt.Sprintf("div(Enum.sum(%s), Enum.count(%s))", args[0], args[0])
						} else {
							res = fmt.Sprintf("(Enum.sum(%s) / Enum.count(%s))", args[0], args[0])
						}
					case types.GroupType:
						if _, ok := tt.Elem.(types.IntType); ok {
							res = fmt.Sprintf("div(Enum.sum(%s.items), Enum.count(%s.items))", args[0], args[0])
						} else if _, ok := tt.Elem.(types.Int64Type); ok {
							res = fmt.Sprintf("div(Enum.sum(%s.items), Enum.count(%s.items))", args[0], args[0])
						} else {
							res = fmt.Sprintf("(Enum.sum(%s.items) / Enum.count(%s.items))", args[0], args[0])
						}
					default:
						c.use("_avg")
						res = fmt.Sprintf("_avg(%s)", argStr)
					}
				case "min":
					if len(args) == 1 {
						t := c.inferExprType(op.Call.Args[0])
						switch t.(type) {
						case types.ListType:
							res = fmt.Sprintf("Enum.min(%s)", args[0])
						case types.GroupType:
							res = fmt.Sprintf("Enum.min(%s.items)", args[0])
						default:
							c.use("_min")
							res = fmt.Sprintf("_min(%s)", argStr)
						}
					} else {
						c.use("_min")
						res = fmt.Sprintf("_min(%s)", argStr)
					}
				case "str":
					res = fmt.Sprintf("to_string(%s)", argStr)
				case "input":
					c.use("_input")
					res = "_input()"
				case "keys":
					res = fmt.Sprintf("Map.keys(%s)", argStr)
				case "values":
					res = fmt.Sprintf("Map.values(%s)", argStr)
				default:
					if _, ok := typ.(types.FuncType); ok {
						res = fmt.Sprintf("%s.(%s)", res, argStr)
						typ = typ.(types.FuncType).Return
					} else {
						res = fmt.Sprintf("%s(%s)", res, argStr)
					}
				}
			}
		} else if op.Cast != nil {
			if op.Cast.Type != nil {
				t := c.resolveTypeRef(op.Cast.Type)
				if st, ok := t.(types.StructType); ok {
					c.ensureStruct(st)
					c.use("_structify")
					res = fmt.Sprintf("_structify(%s, %s)", sanitizeName(st.Name), res)
				} else if _, ok := t.(types.IntType); ok {
					res = fmt.Sprintf("String.to_integer(%s)", res)
				} else if _, ok := t.(types.FloatType); ok {
					res = fmt.Sprintf("String.to_float(%s)", res)
				}
				typ = t
			}
		}
	}
	return res, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Null {
			return "nil", nil
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
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			if s, ok := simpleAtomKey(it.Key); ok {
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				items[i] = fmt.Sprintf("%s: %s", s, v)
				continue
			}
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
		return "%{" + strings.Join(items, ", ") + "}", nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if kind, ok := c.builtinAliases[p.Selector.Root]; ok && len(p.Selector.Tail) == 1 {
			switch kind {
			case "go_testpkg":
				switch p.Selector.Tail[0] {
				case "Pi":
					return "3.14", nil
				case "Answer":
					return "42", nil
				}
			case "python_math":
				switch p.Selector.Tail[0] {
				case "pi":
					return ":math.pi()", nil
				case "e":
					return ":math.exp(1)", nil
				}
			}
		}
		if _, ok := c.attrs[p.Selector.Root]; ok {
			name = "@" + name
		}
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
			return name, nil
		}
		if c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(p.Selector.Root); ok {
				st := ut.Variants[p.Selector.Root]
				c.ensureStruct(st)
				if len(st.Order) == 0 {
					return fmt.Sprintf("%%%s{}", sanitizeName(st.Name)), nil
				}
			}
			if st, ok := c.env.GetStruct(p.Selector.Root); ok && len(st.Order) == 0 {
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{}", sanitizeName(st.Name)), nil
			}
			if _, err := c.env.GetVar(p.Selector.Root); err == nil {
				return name, nil
			}
		}
		if c.currentGroup != "" && c.groupFields[name] {
			return fmt.Sprintf("%s.key.%s", c.currentGroup, name), nil
		}
		return name, nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", f.Name, v)
		}
		if c.env != nil {
			if st, ok := c.env.GetStruct(p.Struct.Name); ok {
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(parts, ", ")), nil
			}
		}
		return "%{" + strings.Join(parts, ", ") + "}", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, v)
		}
		argStr := strings.Join(args, ", ")
		if strings.HasSuffix(p.Call.Func, ".contains") && len(args) == 1 {
			base := strings.TrimSuffix(p.Call.Func, ".contains")
			return fmt.Sprintf("String.contains?(%s, %s)", base, argStr), nil
		}
		if c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(p.Call.Func); ok {
				st := ut.Variants[p.Call.Func]
				if len(args) != len(st.Order) {
					return "", fmt.Errorf("variant %s expects %d args", p.Call.Func, len(st.Order))
				}
				c.ensureStruct(st)
				parts := make([]string, len(st.Order))
				for i, f := range st.Order {
					parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f), args[i])
				}
				return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(parts, ", ")), nil
			}
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 1 {
				t := c.inferExprType(p.Call.Args[0])
				switch t.(type) {
				case types.StringType:
					return fmt.Sprintf("IO.puts(%s)", args[0]), nil
				case types.ListType, types.GroupType:
					return fmt.Sprintf("IO.inspect(%s)", args[0]), nil
				default:
					return fmt.Sprintf("IO.inspect(%s)", args[0]), nil
				}
			}
			return fmt.Sprintf("IO.puts(Enum.join(Enum.map([%s], &inspect(&1)), \" \"))", argStr), nil
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.StringType:
				return fmt.Sprintf("String.length(%s)", args[0]), nil
			case types.MapType:
				return fmt.Sprintf("map_size(%s)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("length(%s.items)", args[0]), nil
			case types.ListType:
				return fmt.Sprintf("length(%s)", args[0]), nil
			default:
				c.use("_length")
				return fmt.Sprintf("_length(%s)", args[0]), nil
			}
		case "count":
			if len(args) != 1 {
				return "", fmt.Errorf("count expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("length(%s)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("length(%s.items)", args[0]), nil
			default:
				c.use("_count")
				return fmt.Sprintf("_count(%s)", argStr), nil
			}
		case "exists":
			if len(args) != 1 {
				return "", fmt.Errorf("exists expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("length(%s) > 0", args[0]), nil
			case types.MapType:
				return fmt.Sprintf("map_size(%s) > 0", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("length(%s.items) > 0", args[0]), nil
			case types.StringType:
				return fmt.Sprintf("String.length(%s) > 0", args[0]), nil
			case types.OptionType:
				return fmt.Sprintf("%s != nil", args[0]), nil
			default:
				c.use("_exists")
				return fmt.Sprintf("_exists(%s)", argStr), nil
			}
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("Enum.sum(%s)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("Enum.sum(%s.items)", args[0]), nil
			default:
				c.use("_sum")
				return fmt.Sprintf("_sum(%s)", argStr), nil
			}
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch tt := t.(type) {
			case types.ListType:
				if _, ok := tt.Elem.(types.IntType); ok {
					return fmt.Sprintf("div(Enum.sum(%s), Enum.count(%s))", args[0], args[0]), nil
				} else if _, ok := tt.Elem.(types.Int64Type); ok {
					return fmt.Sprintf("div(Enum.sum(%s), Enum.count(%s))", args[0], args[0]), nil
				}
				return fmt.Sprintf("(Enum.sum(%s) / Enum.count(%s))", args[0], args[0]), nil
			case types.GroupType:
				if _, ok := tt.Elem.(types.IntType); ok {
					return fmt.Sprintf("div(Enum.sum(%s.items), Enum.count(%s.items))", args[0], args[0]), nil
				} else if _, ok := tt.Elem.(types.Int64Type); ok {
					return fmt.Sprintf("div(Enum.sum(%s.items), Enum.count(%s.items))", args[0], args[0]), nil
				}
				return fmt.Sprintf("(Enum.sum(%s.items) / Enum.count(%s.items))", args[0], args[0]), nil
			default:
				c.use("_avg")
				return fmt.Sprintf("_avg(%s)", argStr), nil
			}
		case "min":
			if len(args) != 1 {
				return "", fmt.Errorf("min expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("Enum.min(%s)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("Enum.min(%s.items)", args[0]), nil
			default:
				c.use("_min")
				return fmt.Sprintf("_min(%s)", argStr), nil
			}
		case "max":
			if len(args) != 1 {
				return "", fmt.Errorf("max expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("Enum.max(%s)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("Enum.max(%s.items)", args[0]), nil
			default:
				c.use("_max")
				return fmt.Sprintf("_max(%s)", argStr), nil
			}
		case "first":
			if len(args) != 1 {
				return "", fmt.Errorf("first expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("Enum.at(%s, 0)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("Enum.at(%s.items, 0)", args[0]), nil
			default:
				c.use("_first")
				return fmt.Sprintf("_first(%s)", argStr), nil
			}
		case "reverse":
			if len(args) != 1 {
				return "", fmt.Errorf("reverse expects 1 arg")
			}
			t := c.inferExprType(p.Call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("Enum.reverse(%s)", args[0]), nil
			case types.StringType:
				return fmt.Sprintf("String.reverse(%s)", args[0]), nil
			default:
				c.use("_reverse")
				return fmt.Sprintf("_reverse(%s)", argStr), nil
			}
		case "concat":
			if len(args) < 2 {
				return "", fmt.Errorf("concat expects at least 2 args")
			}
			allList := true
			allString := true
			for _, a := range p.Call.Args {
				t := c.inferExprType(a)
				_, isList := t.(types.ListType)
				_, isString := t.(types.StringType)
				if !isList {
					allList = false
				}
				if !isString {
					allString = false
				}
			}
			expr := args[0]
			if allList {
				for i := 1; i < len(args); i++ {
					expr = fmt.Sprintf("%s ++ %s", expr, args[i])
				}
				return expr, nil
			}
			if allString {
				for i := 1; i < len(args); i++ {
					expr = fmt.Sprintf("%s <> %s", expr, args[i])
				}
				return expr, nil
			}
			c.use("_concat")
			for i := 1; i < len(args); i++ {
				expr = fmt.Sprintf("_concat(%s, %s)", expr, args[i])
			}
			return expr, nil
		case "append":
			if len(args) != 2 {
				return "", fmt.Errorf("append expects 2 args")
			}
			return fmt.Sprintf("%s ++ [%s]", args[0], args[1]), nil
		case "starts_with":
			if len(args) != 2 {
				return "", fmt.Errorf("starts_with expects 2 args")
			}
			return fmt.Sprintf("String.starts_with?(%s, %s)", args[0], args[1]), nil
		case "substr", "substring":
			if len(args) != 3 {
				return "", fmt.Errorf("substr expects 3 args")
			}
			c.use("_slice_string")
			return fmt.Sprintf("_slice_string(%s, %s, %s)", args[0], args[1], args[2]), nil
		case "lower":
			if len(args) != 1 {
				return "", fmt.Errorf("lower expects 1 arg")
			}
			return fmt.Sprintf("String.downcase(to_string(%s))", args[0]), nil
		case "str":
			return fmt.Sprintf("to_string(%s)", argStr), nil
		case "input":
			c.use("_input")
			return "_input()", nil
		case "now":
			c.use("_now")
			return "_now()", nil
		case "json":
			c.use("_json")
			return fmt.Sprintf("_json(%s)", argStr), nil
		case "keys":
			return fmt.Sprintf("Map.keys(%s)", argStr), nil
		case "values":
			return fmt.Sprintf("Map.values(%s)", argStr), nil
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(p.Call.Func); ok {
					if len(args) != len(st.Order) {
						return "", fmt.Errorf("struct %s expects %d args", p.Call.Func, len(st.Order))
					}
					c.ensureStruct(st)
					parts := make([]string, len(st.Order))
					for i, f := range st.Order {
						parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f), args[i])
					}
					return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(parts, ", ")), nil
				}
			}
			if c.funcs[p.Call.Func] {
				if c.env != nil {
					if t, err := c.env.GetVar(p.Call.Func); err == nil {
						if ft, ok := t.(types.FuncType); ok && len(args) < len(ft.Params) {
							params := make([]string, len(ft.Params)-len(args))
							for i := range params {
								params[i] = fmt.Sprintf("p%d", i)
							}
							allArgs := append(append([]string{}, args...), params...)
							return fmt.Sprintf("fn %s -> %s(%s) end", strings.Join(params, ", "), sanitizeName(p.Call.Func), strings.Join(allArgs, ", ")), nil
						}
					}
				}
				return fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), argStr), nil
			}
			if c.env != nil {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						if len(args) < len(ft.Params) {
							params := make([]string, len(ft.Params)-len(args))
							for i := range params {
								params[i] = fmt.Sprintf("p%d", i)
							}
							allArgs := append(append([]string{}, args...), params...)
							return fmt.Sprintf("fn %s -> %s.(%s) end", strings.Join(params, ", "), sanitizeName(p.Call.Func), strings.Join(allArgs, ", ")), nil
						}
						return fmt.Sprintf("%s.(%s)", sanitizeName(p.Call.Func), argStr), nil
					}
				}
			}
			return fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), argStr), nil
		}
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("fn %s -> %s end", strings.Join(params, ", "), expr), nil
	}
	if len(fn.BlockBody) > 0 {
		sub := &Compiler{env: c.env, indent: c.indent + 1}
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
		body := sub.buf.String()
		var b strings.Builder
		b.WriteString("fn " + strings.Join(params, ", ") + " ->\n")
		b.WriteString(body)
		for i := 0; i < c.indent; i++ {
			b.WriteByte('\t')
		}
		b.WriteString("end")
		return b.String(), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

// simpleAtomKey checks if e is a bare identifier or string literal and returns
// its value for use as a map key. This mirrors simpleStringKey in the Go
// backend so that expressions like `{n: 1}` produce `%{n: 1}` rather than using
// the variable value as the key.
func simpleAtomKey(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		// string literals should remain strings, not atoms
		return "", false
	}
	return "", false
}

func isValidAtom(s string) bool {
	return atomIdent.MatchString(s)
}

func (c *Compiler) staticTypeOfPostfix(p *parser.PostfixExpr) (types.Type, bool) {
	if p == nil || len(p.Ops) > 0 {
		return nil, false
	}
	return c.staticTypeOfPrimary(p.Target)
}

func (c *Compiler) staticTypeOfPrimary(p *parser.Primary) (types.Type, bool) {
	if p == nil {
		return nil, false
	}
	switch {
	case p.List != nil:
		return types.ListType{Elem: types.AnyType{}}, true
	case p.Map != nil:
		return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}, true
	case p.Selector != nil && len(p.Selector.Tail) == 0 && c.env != nil:
		t, err := c.env.GetVar(p.Selector.Root)
		if err == nil {
			return t, true
		}
	}
	return nil, false
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "bool":
			return types.BoolType{}
		case "string":
			return types.StringType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: c.resolveTypeRef(t.Generic.Args[0])}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: c.resolveTypeRef(t.Generic.Args[0]), Value: c.resolveTypeRef(t.Generic.Args[1])}
		}
	}
	return types.AnyType{}
}
