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
				expr = fmt.Sprintf("%s.containsKey(%s)", r, l)
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
				expr = fmt.Sprintf("%s.substring(%s, %s)", expr, start, end)
			} else {
				c.helpers["_slice"] = true
				expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
			}
			continue
		}
		if op.Cast != nil {
			t := c.resolveTypeRef(op.Cast.Type)
			expr = fmt.Sprintf("(%s)(%s)", c.javaType(t), expr)
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
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.IntType); ok {
						return true
					}
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
			elemType = c.inferExprType(p.List.Elems[0])
			for _, el := range p.List.Elems[1:] {
				t := c.inferExprType(el)
				if !equalTypes(elemType, t) {
					elemType = types.AnyType{}
					break
				}
			}
		}
		return fmt.Sprintf("new %s[]{%s}", c.javaType(elemType), joinArgs(elems)), nil
	case p.Map != nil:
		items := []string{}
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
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
		for _, f := range p.Selector.Tail {
			expr += "." + sanitizeName(f)
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
		if name == "input" && len(args) == 0 {
			c.helpers["_input"] = true
			return "_input()", nil
		}
		if name == "count" && len(args) == 1 {
			if c.isMapExprByExpr(p.Call.Args[0]) {
				return args[0] + ".size()", nil
			}
			if c.isStringExprByExpr(p.Call.Args[0]) {
				return args[0] + ".length()", nil
			}
			return args[0] + ".length", nil
		}
		if name == "avg" && len(args) == 1 {
			return fmt.Sprintf("(int) java.util.Arrays.stream(%s).average().orElse(0)", args[0]), nil
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
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
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
		// only support list<int> -> int[] for now
		return c.javaType(tt.Elem) + "[]"
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
	retT := c.inferExprType(expr)
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
				return fmt.Sprintf("new %s[]{%s}", c.javaType(lt.Elem), joinArgs(elems)), nil
			}
		}
	}
	return c.compileExpr(e)
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
