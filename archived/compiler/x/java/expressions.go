//go:build archived

package javacode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []string{left}
	lists := []bool{c.isListExpr(b.Left.Value)}
	ops := []string{}
	alls := []bool{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		lists = append(lists, c.isListExpr(part.Right))
		ops = append(ops, part.Op)
		alls = append(alls, part.All)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in", "union", "except", "intersect"},
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
			llist := lists[i]
			rlist := lists[i+1]

			var expr string
			var isList bool
			switch op {
			case "+":
				if llist || rlist {
					c.helpers["_concat"] = true
					expr = fmt.Sprintf("_concat(%s, %s)", l, r)
					isList = true
				} else {
					expr = fmt.Sprintf("(%s + %s)", l, r)
				}
			case "union":
				if alls[i] {
					c.helpers["_concat"] = true
					expr = fmt.Sprintf("_concat(%s, %s)", l, r)
				} else {
					c.helpers["_union"] = true
					expr = fmt.Sprintf("_union(%s, %s)", l, r)
				}
				isList = true
			case "except":
				c.helpers["_except"] = true
				expr = fmt.Sprintf("_except(%s, %s)", l, r)
				isList = true
			case "intersect":
				c.helpers["_intersect"] = true
				expr = fmt.Sprintf("_intersect(%s, %s)", l, r)
				isList = true
			case "in":
				c.helpers["_in"] = true
				expr = fmt.Sprintf("_in(%s, %s)", l, r)
			default:
				expr = fmt.Sprintf("(%s %s %s)", l, op, r)
			}

			operands[i] = expr
			lists[i] = isList
			operands = append(operands[:i+1], operands[i+2:]...)
			lists = append(lists[:i+1], lists[i+2:]...)
			alls = append(alls[:i], alls[i+1:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected binary expression state")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if u == nil {
		return "", nil
	}
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" || op == "!" {
			expr = fmt.Sprintf("(%s%s)", op, expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if p == nil {
		return "", nil
	}
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isMapExpr(p) {
					expr = fmt.Sprintf("%s.get(%s)", expr, idx)
				} else if c.isStringExpr(p) {
					c.helpers["_indexString"] = true
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else if c.isListExpr(p) {
					c.helpers["_indexList"] = true
					expr = fmt.Sprintf("_indexList(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
				continue
			}
			start := "0"
			if op.Index.Start != nil {
				s, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				start = s
			}
			end := fmt.Sprintf("%s.length", expr)
			if op.Index.End != nil {
				e, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				end = e
			}
			if c.isStringExpr(p) {
				c.helpers["_sliceString"] = true
				expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
			} else {
				c.helpers["_slice"] = true
				expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
			}
			continue
		}
		if op.Cast != nil {
			t := c.resolveTypeRef(op.Cast.Type)
			typ := c.javaType(t)
			box := boxedType(typ)
			if _, ok := t.(types.StructType); ok || box != typ {
				c.helpers["_cast"] = true
				expr = fmt.Sprintf("_cast(%s.class, %s)", box, expr)
				if box != typ {
					expr = fmt.Sprintf("((%s)%s)", typ, expr)
				}
			} else {
				expr = fmt.Sprintf("(%s)(%s)", typ, expr)
			}
			continue
		}
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, arg)
			}
			if expr == "print" {
				expr = fmt.Sprintf("System.out.println(%s)", joinArgs(args))
			} else if expr == "len" {
				if c.isMapExprByExpr(op.Call.Args[0]) {
					expr = fmt.Sprintf("%s.size()", args[0])
				} else if c.isStringExprByExpr(op.Call.Args[0]) {
					expr = fmt.Sprintf("%s.length()", args[0])
				} else {
					expr = fmt.Sprintf("%s.length", args[0])
				}
			} else if expr == "str" {
				expr = fmt.Sprintf("String.valueOf(%s)", joinArgs(args))
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, joinArgs(args))
			}
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) isStringExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isMapExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isMapExprByExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	if e.Binary.Left.Value == nil {
		return false
	}
	return c.isMapExpr(e.Binary.Left.Value)
}

func (c *Compiler) isStringExprByExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	if e.Binary.Left.Value == nil {
		return false
	}
	return c.isStringExpr(e.Binary.Left.Value)
}

func (c *Compiler) isListExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
			return false
		}
	}
	if p.Target.List != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("\"%s\"", *p.Lit.Str), nil
		}
		if p.Lit.Null {
			return "null", nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ce, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = ce
		}
		var elemType types.Type = types.AnyType{}
		if len(p.List.Elems) > 0 {
			elemType = c.exprType(p.List.Elems[0])
			for _, el := range p.List.Elems[1:] {
				t := c.exprType(el)
				if !equalTypes(elemType, t) {
					elemType = types.AnyType{}
					break
				}
			}
		}
		typ := c.javaType(elemType)
		if strings.Contains(typ, "<") {
			typ = "Object"
		}
		return fmt.Sprintf("new %s[]{%s}", typ, joinArgs(elems)), nil
	case p.Map != nil:
		items := []string{}
		for _, it := range p.Map.Items {
			var k string
			if ks, ok := simpleStringKey(it.Key); ok {
				k = fmt.Sprintf("\"%s\"", ks)
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
			items = append(items, k+", "+v)
		}
		if len(items) == 0 {
			return "new java.util.HashMap<>()", nil
		}
		return "new java.util.HashMap<>(java.util.Map.of(" + joinArgs(items) + "))", nil
	case p.Struct != nil:
		args := []string{}
		if c.env != nil {
			if st, ok := c.env.GetStruct(p.Struct.Name); ok {
				m := map[string]string{}
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					m[f.Name] = v
				}
				for _, fn := range st.Order {
					args = append(args, m[fn])
				}
			}
		}
		if len(args) == 0 {
			for _, f := range p.Struct.Fields {
				v, err := c.compileExpr(f.Value)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
		}
		return fmt.Sprintf("new %s(%s)", sanitizeName(p.Struct.Name), joinArgs(args)), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		var typ types.Type
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		for i, f := range p.Selector.Tail {
			isLast := i == len(p.Selector.Tail)-1
			switch tt := typ.(type) {
			case types.MapType:
				if isLast && len(p.Selector.Tail) > 1 {
					expr += "." + sanitizeName(f)
				} else {
					expr += fmt.Sprintf(".get(\"%s\")", f)
					typ = tt.Value
				}
			case types.StructType:
				expr += "." + sanitizeName(f)
				if ft, ok := tt.Fields[f]; ok {
					typ = ft
				} else {
					typ = nil
				}
			default:
				if isLast && len(p.Selector.Tail) > 1 {
					expr += "." + sanitizeName(f)
				} else {
					expr += fmt.Sprintf(".get(\"%s\")", f)
				}
				typ = nil
			}
		}
		return expr, nil
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			ce, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, ce)
		}
		name := sanitizeName(p.Call.Func)
		if name == "print" {
			if len(args) == 1 {
				return "System.out.println(" + args[0] + ")", nil
			}
			expr := args[0]
			for i := 1; i < len(args); i++ {
				expr += " + \" \" + " + args[i]
			}
			return "System.out.println(" + expr + ")", nil
		}
		if name == "len" && len(args) == 1 {
			if c.isMapExprByExpr(p.Call.Args[0]) {
				return args[0] + ".size()", nil
			}
			if c.isStringExprByExpr(p.Call.Args[0]) {
				return args[0] + ".length()", nil
			}
			return args[0] + ".length", nil
		}
		if name == "str" && len(args) == 1 {
			return "String.valueOf(" + args[0] + ")", nil
		}
		if name == "substr" {
			if len(args) == 2 {
				c.helpers["_sliceString"] = true
				return fmt.Sprintf("_sliceString(%s, %s, %s.length())", args[0], args[1], args[0]), nil
			}
			if len(args) == 3 {
				c.helpers["_sliceString"] = true
				return fmt.Sprintf("_sliceString(%s, %s, %s + %s)", args[0], args[1], args[1], args[2]), nil
			}
		}
		if name == "reverse" && len(args) == 1 {
			if c.isStringExprByExpr(p.Call.Args[0]) {
				c.helpers["_reverseString"] = true
				return fmt.Sprintf("_reverseString(%s)", args[0]), nil
			}
			c.helpers["_reverseList"] = true
			c.helpers["_toList"] = true
			return fmt.Sprintf("_reverseList(_toList(%s))", args[0]), nil
		}
		if name == "input" && len(args) == 0 {
			c.helpers["_input"] = true
			return "_input()", nil
		}
		if name == "count" && len(args) == 1 {
			c.helpers["_count"] = true
			return "_count(" + args[0] + ")", nil
		}
		if name == "exists" && len(args) == 1 {
			c.helpers["_exists"] = true
			return "_exists(" + args[0] + ")", nil
		}
		if name == "sum" && len(args) == 1 {
			c.helpers["_sum"] = true
			return "_sum(" + args[0] + ")", nil
		}
		if name == "avg" && len(args) == 1 {
			c.helpers["_avg"] = true
			return "_avg(" + args[0] + ")", nil
		}
		if name == "now" && len(args) == 0 {
			return "System.nanoTime()", nil
		}
		if name == "json" && len(args) == 1 {
			c.helpers["_json"] = true
			return "_json(" + args[0] + ")", nil
		}
		if name == "eval" && len(args) == 1 {
			c.helpers["_eval"] = true
			return "_eval(" + args[0] + ")", nil
		}
		if c.env != nil {
			if _, ok := c.env.GetFunc(p.Call.Func); !ok {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						if len(ft.Params) == 0 {
							return name + ".get()", nil
						}
						return name + ".apply(" + joinArgs(args) + ")", nil
					}
				}
			}
		}
		return name + "(" + joinArgs(args) + ")", nil
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Group != nil:
		return c.compileExpr(p.Group)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		} else {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
		params[i] = sanitizeName(p.Name)
	}
	sub := &Compiler{helpers: c.helpers, env: child}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr + ";")
	} else {
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return "(" + strings.Join(params, ", ") + ") -> {\n" + body + "}", nil
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

func (c *Compiler) javaType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return "Object[]"
	case types.MapType:
		key := c.javaType(tt.Key)
		val := c.javaType(tt.Value)
		key = boxedType(key)
		val = boxedType(val)
		return "java.util.Map<" + key + ", " + val + ">"
	case types.FuncType:
		ret := boxedType(c.javaType(tt.Return))
		if len(tt.Params) == 0 {
			return "java.util.function.Supplier<" + ret + ">"
		}
		if len(tt.Params) == 1 {
			arg := boxedType(c.javaType(tt.Params[0]))
			return "java.util.function.Function<" + arg + ", " + ret + ">"
		}
		if len(tt.Params) == 2 {
			a := boxedType(c.javaType(tt.Params[0]))
			b := boxedType(c.javaType(tt.Params[1]))
			return "java.util.function.BiFunction<" + a + ", " + b + ", " + ret + ">"
		}
		return "java.util.function.Function"
	case types.StructType:
		return sanitizeName(tt.Name)
	default:
		return "Object"
	}
}

func boxedType(typ string) string {
	switch typ {
	case "int":
		return "Integer"
	case "double":
		return "Double"
	case "boolean":
		return "Boolean"
	default:
		return typ
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "null"
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.helpers["_fetch"] = true
	c.helpers["_dataset"] = true
	c.helpers["_parseJson"] = true
	c.helpers["_toJson"] = true
	return fmt.Sprintf("_fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "null"
	if l.With != nil {
		o, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.helpers["_dataset"] = true
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "null"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "null"
	if s.With != nil {
		o, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.helpers["_dataset"] = true
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	orig := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)

	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 {
		c.env = child
		var whereExpr string
		if q.Where != nil {
			w, err := c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
			whereExpr = w
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
		var b bytes.Buffer
		b.WriteString("(new java.util.function.Supplier<java.util.List<Object>>() {\n")
		b.WriteString("\tpublic java.util.List<Object> get() {\n")
		b.WriteString("\t\tjava.util.List<Object> _src = _toList(" + src + ");\n")
		if whereExpr != "" {
			b.WriteString("\t\t_src = _filter(_src, (Object " + sanitizeName(q.Var) + ") -> { return " + whereExpr + "; });\n")
			c.helpers["_filter"] = true
		}
		b.WriteString("\t\tjava.util.List<_Group> _grps = _group_by(_src, " + sanitizeName(q.Var) + " -> " + keyExpr + ");\n")
		b.WriteString("\t\tjava.util.List<Object> _res = new java.util.ArrayList<>();\n")
		b.WriteString("\t\tfor (_Group " + sanitizeName(q.Group.Name) + " : _grps) {\n")
		b.WriteString("\t\t\t_res.add(" + valExpr + ");\n")
		b.WriteString("\t\t}\n")
		b.WriteString("\t\treturn _res;\n")
		b.WriteString("\t}\n")
		b.WriteString("}).get()")
		c.helpers["_group_by"] = true
		c.helpers["_group"] = true
		c.helpers["_toList"] = true
		return b.String(), nil
	}

	varNames := []string{sanitizeName(q.Var)}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
		child.SetVar(f.Var, types.AnyType{}, true)
		varNames = append(varNames, sanitizeName(f.Var))
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinLeft := make([]bool, len(q.Joins))
	joinRight := make([]bool, len(q.Joins))
	paramCopy := append([]string(nil), varNames...)
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		child.SetVar(j.Var, types.AnyType{}, true)
		c.env = child
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinOns[i] = on
		if j.Side != nil {
			switch *j.Side {
			case "left":
				joinLeft[i] = true
			case "right":
				joinRight[i] = true
			case "outer":
				joinLeft[i] = true
				joinRight[i] = true
			}
		}
		paramCopy = append(paramCopy, sanitizeName(j.Var))
	}
	c.env = child
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
	c.env = orig

	lambda := func(params []string, body string) string {
		var b bytes.Buffer
		b.WriteString("(Object[] a) -> { ")
		for i, p := range params {
			b.WriteString(fmt.Sprintf("Object %s = a[%d]; ", sanitizeName(p), i))
		}
		b.WriteString("return " + body + "; }")
		return b.String()
	}

	lambda1 := func(param, body string) string {
		var b bytes.Buffer
		b.WriteString("(Object " + sanitizeName(param) + ") -> { ")
		b.WriteString("return " + body + "; }")
		return b.String()
	}

	pushdown := false
	if q.Where != nil {
		vars := exprVars(q.Where)
		if len(vars) == 1 && vars[q.Var] {
			pushdown = true
		}
	}

	allParams := append([]string(nil), paramCopy...)
	selectFn := lambda(allParams, sel)
	whereFn := "null"
	if whereExpr != "" {
		whereFn = lambda(allParams, whereExpr)
	}
	sortFn := "null"
	if sortExpr != "" {
		sortFn = lambda(allParams, sortExpr)
	}
	if skipExpr == "" {
		skipExpr = "-1"
	}
	if takeExpr == "" {
		takeExpr = "-1"
	}

	joins := make([]string, 0, len(fromSrcs)+len(joinSrcs))
	for _, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("new _JoinSpec(_toList(%s), null, false, false)", fs))
	}
	paramCopy = append([]string(nil), varNames...)
	for i, js := range joinSrcs {
		onFn := lambda(append(paramCopy, sanitizeName(q.Joins[i].Var)), joinOns[i])
		spec := fmt.Sprintf("new _JoinSpec(_toList(%s), %s, %t, %t)", js, onFn, joinLeft[i], joinRight[i])
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}

	var b bytes.Buffer
	b.WriteString("(new java.util.function.Supplier<java.util.List<Object>>() {\n")
	b.WriteString("\tpublic java.util.List<Object> get() {\n")
	b.WriteString("\t\tjava.util.List<Object> _src = _toList(" + src + ");\n")
	if pushdown {
		filterFn := lambda1(q.Var, whereExpr)
		b.WriteString("\t\t_src = _filter(_src, " + filterFn + ");\n")
		c.helpers["_filter"] = true
		whereFn = "null"
	}
	b.WriteString("\t\tjava.util.List<_JoinSpec> _joins = java.util.List.of(\n")
	for i, j := range joins {
		b.WriteString("\t\t\t" + j)
		if i != len(joins)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString("\t\t);\n")
	b.WriteString("\t\treturn _query(_src, _joins, new _QueryOpts(" + selectFn + ", " + whereFn + ", " + sortFn + ", " + skipExpr + ", " + takeExpr + "));\n")
	b.WriteString("\t}\n")
	b.WriteString("}).get()")
	c.helpers["_query"] = true
	return b.String(), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var prompt, text, model string
	params := []string{}
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
			params = append(params, fmt.Sprintf("%q, %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramMap := "null"
	if len(params) > 0 {
		paramMap = "java.util.Map.of(" + joinArgs(params) + ")"
	}
	if model == "" {
		model = "null"
	}
	if g.Target == "embedding" {
		c.helpers["_genEmbed"] = true
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramMap), nil
	}

	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.helpers["_genStruct"] = true
			return fmt.Sprintf("_genStruct(%s.class, %s, %s, %s)", sanitizeName(g.Target), prompt, model, paramMap), nil
		}
	}

	c.helpers["_genText"] = true
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramMap), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Match: m}}}}}
	retT := c.exprType(expr)
	retType := c.javaType(retT)
	if retType == "" {
		retType = "Object"
	}
	retGen := boxedType(retType)
	var buf bytes.Buffer
	buf.WriteString("(new java.util.function.Supplier<" + retGen + ">() {\n")
	buf.WriteString("\tpublic " + retGen + " get() {\n")
	buf.WriteString("\t\tObject _t = " + target + ";\n")
	for _, cse := range m.Cases {
		if isUnderscoreExpr(cse.Pattern) {
			res, err := c.compileExpr(cse.Result)
			if err != nil {
				return "", err
			}
			buf.WriteString("\t\treturn " + res + ";\n")
			buf.WriteString("\t}\n")
			buf.WriteString("}).get()")
			return buf.String(), nil
		}
		pat, err := c.compileExpr(cse.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cse.Result)
		if err != nil {
			return "", err
		}
		buf.WriteString("\t\tif (java.util.Objects.equals(_t, " + pat + ")) return " + res + ";\n")
	}
	buf.WriteString("\t\treturn " + zeroValue(retType) + ";\n")
	buf.WriteString("\t}\n")
	buf.WriteString("}).get()")
	return buf.String(), nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseVal string
	if ie.ElseIf != nil {
		elseVal, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseVal, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseVal = "null"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenVal, elseVal), nil
}

// compileExprHint compiles an expression using a type hint. The hint is used
// for list literals that would otherwise default to any.
func (c *Compiler) compileExprHint(e *parser.Expr, hint types.Type) (string, error) {
	if lt, ok := hint.(types.ListType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ll := e.Binary.Left.Value.Target.List; ll != nil {
				elems := make([]string, len(ll.Elems))
				for i, el := range ll.Elems {
					ev, err := c.compileExprHint(el, lt.Elem)
					if err != nil {
						return "", err
					}
					elems[i] = ev
				}
				typ := c.javaType(lt.Elem)
				if strings.Contains(typ, "<") {
					typ = "Object"
				}
				return fmt.Sprintf("new %s[]{%s}", typ, joinArgs(elems)), nil
			}
		}
	}
	expr, err := c.compileExpr(e)
	if err != nil {
		return "", err
	}
	exprT := c.exprType(e)
	hintJava := c.javaType(hint)
	exprJava := c.javaType(exprT)
	if hintJava != "" && hintJava != exprJava && (isAny(exprT) || !equalTypes(hint, exprT)) {
		box := boxedType(hintJava)
		c.helpers["_cast"] = true
		expr = fmt.Sprintf("_cast(%s.class, %s)", box, expr)
		if box != hintJava {
			expr = fmt.Sprintf("((%s)%s)", hintJava, expr)
		}
	}
	return expr, nil
}

func joinArgs(args []string) string {
	if len(args) == 0 {
		return ""
	}
	res := args[0]
	for i := 1; i < len(args); i++ {
		res += ", " + args[i]
	}
	return res
}

func exprVars(e *parser.Expr) map[string]bool {
	vars := map[string]bool{}
	var walkExpr func(*parser.Expr)
	var walkUnary func(*parser.Unary)
	var walkPostfix func(*parser.PostfixExpr)
	var walkPrimary func(*parser.Primary)

	walkExpr = func(e *parser.Expr) {
		if e == nil {
			return
		}
		walkUnary(e.Binary.Left)
		for _, op := range e.Binary.Right {
			walkPostfix(op.Right)
		}
	}

	walkUnary = func(u *parser.Unary) {
		if u != nil {
			walkPostfix(u.Value)
		}
	}

	walkPostfix = func(p *parser.PostfixExpr) {
		if p == nil {
			return
		}
		walkPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					walkExpr(a)
				}
			}
			if op.Index != nil {
				walkExpr(op.Index.Start)
				walkExpr(op.Index.End)
				walkExpr(op.Index.Step)
			}
		}
	}

	walkPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Selector != nil:
			vars[p.Selector.Root] = true
		case p.List != nil:
			for _, e := range p.List.Elems {
				walkExpr(e)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				walkExpr(it.Key)
				walkExpr(it.Value)
			}
		case p.Struct != nil:
			for _, f := range p.Struct.Fields {
				walkExpr(f.Value)
			}
		case p.Call != nil:
			for _, a := range p.Call.Args {
				walkExpr(a)
			}
		case p.If != nil:
			walkExpr(p.If.Cond)
			walkExpr(p.If.Then)
			if p.If.Else != nil {
				walkExpr(p.If.Else)
			}
			if p.If.ElseIf != nil {
				walkExpr(p.If.ElseIf.Cond)
				walkExpr(p.If.ElseIf.Then)
				if p.If.ElseIf.Else != nil {
					walkExpr(p.If.ElseIf.Else)
				}
			}
		case p.Match != nil:
			walkExpr(p.Match.Target)
			for _, c := range p.Match.Cases {
				walkExpr(c.Pattern)
				walkExpr(c.Result)
			}
		case p.Generate != nil:
			for _, f := range p.Generate.Fields {
				walkExpr(f.Value)
			}
		case p.Fetch != nil:
			walkExpr(p.Fetch.URL)
			walkExpr(p.Fetch.With)
		case p.Load != nil:
			walkExpr(p.Load.With)
		case p.Save != nil:
			walkExpr(p.Save.Src)
			walkExpr(p.Save.With)
		case p.Query != nil:
			walkExpr(p.Query.Source)
			for _, f := range p.Query.Froms {
				walkExpr(f.Src)
			}
			for _, j := range p.Query.Joins {
				walkExpr(j.Src)
				walkExpr(j.On)
			}
			walkExpr(p.Query.Where)
			if p.Query.Group != nil {
				for _, ge := range p.Query.Group.Exprs {
					walkExpr(ge)
				}
			}
			walkExpr(p.Query.Sort)
			walkExpr(p.Query.Skip)
			walkExpr(p.Query.Take)
			walkExpr(p.Query.Select)
		}
	}

	walkExpr(e)
	return vars
}
