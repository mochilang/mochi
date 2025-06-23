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
		ops = append(ops, part.Op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
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
				expr = fmt.Sprintf("(if is_map(%s), do: Map.has_key?(%s, %s), else: Enum.member?(%s, %s))", r.expr, r.expr, l.expr, r.expr, l.expr)
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
					res = fmt.Sprintf("Enum.at(String.graphemes(%s), %s)", res, idx)
				default:
					res = fmt.Sprintf("Enum.at(%s, %s)", res, idx)
					if lt, ok := tt.(types.ListType); ok {
						typ = lt.Elem
					}
				}
			} else {
				start := idx
				end := "nil"
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				switch typ.(type) {
				case types.StringType:
					slice := fmt.Sprintf("Enum.slice(String.graphemes(%s), %s, %s == nil and length(String.graphemes(%s)) - %s || (%s - %s))", res, start, end, res, start, end, start)
					res = fmt.Sprintf("Enum.join(%s)", slice)
				default:
					length := fmt.Sprintf("(%s) - %s", end, start)
					if end == "nil" {
						length = fmt.Sprintf("length(%s) - %s", res, start)
					}
					res = fmt.Sprintf("Enum.slice(%s, %s, %s)", res, start, length)
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
			switch res {
			case "print":
				if len(args) == 1 {
					res = fmt.Sprintf("IO.puts(%s)", argStr)
				} else {
					res = fmt.Sprintf("IO.puts(Enum.join(Enum.map([%s], &to_string(&1)), \" \"))", argStr)
				}
			case "len":
				res = fmt.Sprintf("length(%s)", argStr)
			case "count":
				c.use("_count")
				res = fmt.Sprintf("_count(%s)", argStr)
			case "avg":
				c.use("_avg")
				res = fmt.Sprintf("_avg(%s)", argStr)
			case "str":
				res = fmt.Sprintf("to_string(%s)", argStr)
			case "input":
				c.use("_input")
				res = "_input()"
			default:
				if _, ok := typ.(types.FuncType); ok {
					res = fmt.Sprintf("%s.(%s)", res, argStr)
					typ = typ.(types.FuncType).Return
				} else {
					res = fmt.Sprintf("%s(%s)", res, argStr)
				}
			}
		} else if op.Cast != nil {
			if op.Cast.Type != nil {
				t := c.resolveTypeRef(op.Cast.Type)
				if _, ok := t.(types.StructType); ok {
					res = fmt.Sprintf("Map.new(%s, fn {k, v} -> {String.to_atom(to_string(k)), v} end)", res)
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
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
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
		switch p.Call.Func {
		case "print":
			if len(args) == 1 {
				return fmt.Sprintf("IO.puts(%s)", argStr), nil
			}
			return fmt.Sprintf("IO.puts(Enum.join(Enum.map([%s], &to_string(&1)), \" \"))", argStr), nil
		case "len":
			return fmt.Sprintf("length(%s)", argStr), nil
		case "count":
			c.use("_count")
			return fmt.Sprintf("_count(%s)", argStr), nil
		case "avg":
			c.use("_avg")
			return fmt.Sprintf("_avg(%s)", argStr), nil
		case "str":
			return fmt.Sprintf("to_string(%s)", argStr), nil
		case "input":
			c.use("_input")
			return "_input()", nil
		default:
			if c.funcs[p.Call.Func] {
				return fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), argStr), nil
			}
			if c.env != nil {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if _, ok := t.(types.FuncType); ok {
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
		s := *p.Target.Lit.Str
		if isValidAtom(s) {
			return s, true
		}
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
