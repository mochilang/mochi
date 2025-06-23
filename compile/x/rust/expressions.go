package rscode

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	leftList := c.isListExpr(b.Left.Value)
	leftString := c.isStringExpr(b.Left.Value)
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightList := c.isListExpr(op.Right)
		rightString := c.isStringExpr(op.Right)
		switch op.Op {
		case "+":
			if leftList || rightList {
				expr = fmt.Sprintf("{ let a = &%s; let b = &%s; let mut res = Vec::with_capacity(a.len() + b.len()); res.extend_from_slice(a); res.extend_from_slice(b); res }", expr, r)
				leftList = true
				leftString = false
			} else if leftString || rightString {
				expr = fmt.Sprintf("format!(\"{}{}\", %s, %s)", expr, r)
				leftString = true
				leftList = false
			} else {
				expr = fmt.Sprintf("%s + %s", expr, r)
				leftList = false
				leftString = false
			}
		case "in":
			expr = fmt.Sprintf("%s.contains_key(&%s)", r, expr)
			leftList = false
		case "union_all":
			c.use("_union_all")
			expr = fmt.Sprintf("_union_all(&%s, &%s)", expr, r)
			leftList = true
			leftString = false
		case "union":
			c.use("_union")
			expr = fmt.Sprintf("_union(&%s, &%s)", expr, r)
			leftList = true
			leftString = false
		case "except":
			c.use("_except")
			expr = fmt.Sprintf("_except(&%s, &%s)", expr, r)
			leftList = true
			leftString = false
		case "intersect":
			c.use("_intersect")
			expr = fmt.Sprintf("_intersect(&%s, &%s)", expr, r)
			leftList = true
			leftString = false
		default:
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, r)
			leftList = false
		}
	}
	return expr, nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	// Handle simple grouping without joins or filters
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Expr)
		if err != nil {
			return "", err
		}
		selExpr, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		var elemType types.Type = types.AnyType{}
		srcType := c.inferExprType(q.Source)
		if lt, ok := srcType.(types.ListType); ok {
			elemType = lt.Elem
		}
		keyType := c.inferExprType(q.Group.Expr)
		retType := c.inferExprType(q.Select)
		alias := sanitizeName(q.Group.Name)
		var b strings.Builder
		b.WriteString("{\n")
		b.WriteString("    #[derive(Clone, Debug)]\n")
		b.WriteString(fmt.Sprintf("    struct Group { key: %s, items: Vec<%s> }\n", rustTypeFrom(keyType), rustTypeFrom(elemType)))
		b.WriteString("    let mut groups: std::collections::HashMap<String, Group> = std::collections::HashMap::new();\n")
		b.WriteString("    let mut order: Vec<String> = Vec::new();\n")
		b.WriteString(fmt.Sprintf("    for %s in %s.clone() {\n", sanitizeName(q.Var), src))
		b.WriteString(fmt.Sprintf("        let key: %s = %s;\n", rustTypeFrom(keyType), keyExpr))
		b.WriteString("        let ks = format!(\"{:?}\", key.clone());\n")
		b.WriteString("        if !groups.contains_key(&ks) {\n")
		b.WriteString("            groups.insert(ks.clone(), Group{ key: key.clone(), items: Vec::new() });\n")
		b.WriteString("            order.push(ks.clone());\n")
		b.WriteString("        }\n")
		b.WriteString(fmt.Sprintf("        groups.get_mut(&ks).unwrap().items.push(%s.clone());\n", sanitizeName(q.Var)))
		b.WriteString("    }\n")
		b.WriteString(fmt.Sprintf("    let mut _res: Vec<%s> = Vec::new();\n", rustTypeFrom(retType)))
		b.WriteString("    for ks in order {\n")
		b.WriteString("        let g = groups.get(&ks).unwrap().clone();\n")
		if alias != "g" {
			b.WriteString(fmt.Sprintf("        let %s = g.clone();\n", alias))
		}
		b.WriteString(fmt.Sprintf("        _res.push(%s);\n", selExpr))
		b.WriteString("    }\n")
		b.WriteString("    _res\n")
		b.WriteString("}\n")
		return b.String(), nil
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
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinOns[i] = on
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	condParts := make([]string, 0, len(joinOns)+1)
	for _, on := range joinOns {
		condParts = append(condParts, on)
	}
	if q.Where != nil {
		w, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		condParts = append(condParts, w)
	}
	cond := strings.Join(condParts, " && ")
	var b strings.Builder
	b.WriteString("{\n")
	if q.Sort != nil {
		b.WriteString("    let mut _pairs = Vec::new();\n")
	} else {
		b.WriteString("    let mut _res = Vec::new();\n")
	}
	loopSrc := src
	if len(fromSrcs) > 0 || len(joinSrcs) > 0 {
		loopSrc += ".clone()"
	}
	b.WriteString(fmt.Sprintf("    for %s in %s {\n", sanitizeName(q.Var), loopSrc))
	indent := "        "
	for i, fs := range fromSrcs {
		fvar := sanitizeName(q.Froms[i].Var)
		srcPart := fs + ".clone()"
		b.WriteString(indent + fmt.Sprintf("for %s in %s {\n", fvar, srcPart))
		indent += "    "
	}
	for i, js := range joinSrcs {
		jvar := sanitizeName(q.Joins[i].Var)
		srcPart := js + ".clone()"
		b.WriteString(indent + fmt.Sprintf("for %s in %s {\n", jvar, srcPart))
		indent += "    "
	}
	if cond != "" {
		b.WriteString(indent + fmt.Sprintf("if %s {\n", cond))
		indent += "    "
		if q.Sort != nil {
			sortKey, err := c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			b.WriteString(indent + fmt.Sprintf("_pairs.push((%s, %s));\n", sortKey, sel))
		} else {
			b.WriteString(indent + fmt.Sprintf("_res.push(%s);\n", sel))
		}
		indent = indent[:len(indent)-4]
		b.WriteString(indent + "}\n")
	} else {
		if q.Sort != nil {
			sortKey, err := c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			b.WriteString(indent + fmt.Sprintf("_pairs.push((%s, %s));\n", sortKey, sel))
		} else {
			b.WriteString(indent + fmt.Sprintf("_res.push(%s);\n", sel))
		}
	}
	for range joinSrcs {
		indent = indent[:len(indent)-4]
		b.WriteString(indent + "}\n")
	}
	for range fromSrcs {
		indent = indent[:len(indent)-4]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("    }\n")
	if q.Sort != nil {
		b.WriteString("    _pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());\n")
		b.WriteString("    let mut _res = Vec::new();\n")
		b.WriteString("    for p in _pairs { _res.push(p.1); }\n")
	}
	if q.Skip != nil {
		skip, err := c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf("    let mut skip = %s as usize;\n", skip))
		b.WriteString("    if skip < _res.len() { _res = _res[skip..].to_vec(); } else { _res = Vec::new(); }\n")
	}
	if q.Take != nil {
		take, err := c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf("    let take = %s as usize;\n", take))
		b.WriteString("    if take < _res.len() { _res.truncate(take); }\n")
	}
	b.WriteString("    _res\n")
	b.WriteString("}")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(|| {\n")
	b.WriteString("    match " + target + " {\n")
	for _, cs := range m.Cases {
		pat, err := c.compileMatchPattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		caseEnv := types.NewEnv(c.env)
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						caseEnv.SetVar(id, st.Fields[st.Order[idx]], true)
					}
				}
			}
		}
		orig := c.env
		c.env = caseEnv
		res, err := c.compileExpr(cs.Result)
		c.env = orig
		if err != nil {
			return "", err
		}
		b.WriteString("        " + pat + " => { " + res + " },\n")
	}
	b.WriteString("    }\n")
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: c.resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: c.resolveTypeRef(args[0]), Value: c.resolveTypeRef(args[1])}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func (c *Compiler) compileMatchPattern(pat *parser.Expr) (string, error) {
	if isUnderscoreExpr(pat) {
		return "_", nil
	}
	if call, ok := callPattern(pat); ok {
		if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
			st := ut.Variants[call.Func]
			parts := make([]string, 0, len(call.Args))
			for idx, a := range call.Args {
				if name, ok := identName(a); ok {
					if name == "_" {
						continue
					}
					field := sanitizeName(st.Order[idx])
					parts = append(parts, fmt.Sprintf("%s: %s", field, sanitizeName(name)))
				} else {
					return "", fmt.Errorf("invalid pattern")
				}
			}
			if len(parts) > 0 {
				return fmt.Sprintf("%s::%s { %s }", sanitizeName(ut.Name), sanitizeName(call.Func), strings.Join(parts, ", ")), nil
			}
			if len(st.Fields) == 0 {
				return fmt.Sprintf("%s::%s", sanitizeName(ut.Name), sanitizeName(call.Func)), nil
			}
			return fmt.Sprintf("%s::%s { .. }", sanitizeName(ut.Name), sanitizeName(call.Func)), nil
		}
	}
	if id, ok := identName(pat); ok {
		if ut, ok := c.env.FindUnionByVariant(id); ok {
			return fmt.Sprintf("%s::%s", sanitizeName(ut.Name), sanitizeName(id)), nil
		}
		return sanitizeName(id), nil
	}
	return c.compileExpr(pat)
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		val = fmt.Sprintf("%s%s", op, val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
		} else if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil {
				iexpr, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				if c.isStringBase(p) {
					expr = fmt.Sprintf("{ let s = &%s; let mut idx = %s; let chars: Vec<char> = s.chars().collect(); if idx < 0 { idx += chars.len() as i64; } if idx < 0 || idx >= chars.len() as i64 { panic!(\"index out of range\"); } chars[idx as usize].to_string() }", expr, iexpr)
				} else if c.isMapExpr(p) {
					expr = fmt.Sprintf("%s.get(&%s).unwrap().clone()", expr, iexpr)
				} else if isStringLiteral(idx.Start) {
					expr = fmt.Sprintf("%s[%s]", expr, iexpr)
				} else if id, ok := identName(idx.Start); ok {
					expr = fmt.Sprintf("%s[%s as usize]", expr, id)
				} else {
					expr = fmt.Sprintf("%s[(%s) as usize]", expr, iexpr)
				}
			} else {
				start := "0"
				if idx.Start != nil {
					s, err := c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := ""
				if idx.End != nil {
					e, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					end = e
				} else {
					end = fmt.Sprintf("%s.len()", expr)
				}
				simpleStart := idx.Start != nil
				if simpleStart {
					_, simpleStart = identName(idx.Start)
				}
				simpleEnd := idx.End != nil
				if simpleEnd {
					_, simpleEnd = identName(idx.End)
				}
				if c.isStringBase(p) {
					if simpleStart && simpleEnd {
						expr = fmt.Sprintf("%s[%s as usize..%s as usize].to_string()", expr, start, end)
					} else if simpleStart {
						expr = fmt.Sprintf("%s[%s as usize..(%s) as usize].to_string()", expr, start, end)
					} else if simpleEnd {
						expr = fmt.Sprintf("%s[(%s) as usize..%s as usize].to_string()", expr, start, end)
					} else {
						expr = fmt.Sprintf("%s[(%s) as usize..(%s) as usize].to_string()", expr, start, end)
					}
				} else {
					if simpleStart && simpleEnd {
						expr = fmt.Sprintf("%s[%s as usize..%s as usize].to_vec()", expr, start, end)
					} else if simpleStart {
						expr = fmt.Sprintf("%s[%s as usize..(%s) as usize].to_vec()", expr, start, end)
					} else if simpleEnd {
						expr = fmt.Sprintf("%s[(%s) as usize..%s as usize].to_vec()", expr, start, end)
					} else {
						expr = fmt.Sprintf("%s[(%s) as usize..(%s) as usize].to_vec()", expr, start, end)
					}
				}
			}
		} else if op.Cast != nil {
			typ := rustType(op.Cast.Type)
			expr = fmt.Sprintf("(%s as %s)", expr, typ)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		var st types.StructType
		var ok bool
		if c.env != nil {
			st, ok = c.env.GetStruct(p.Struct.Name)
		}
		var unionName string
		var isVariant bool
		if ut, ok2 := c.env.FindUnionByVariant(p.Struct.Name); ok2 {
			unionName = ut.Name
			isVariant = true
		}
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			if ok {
				if ft, ok2 := st.Fields[f.Name]; ok2 {
					if _, isString := ft.(types.StringType); isString && isStringLiteral(f.Value) {
						v = fmt.Sprintf("%s.to_string()", v)
					}
					if ut, ok3 := ft.(types.UnionType); ok3 {
						if isVariant && ut.Name == unionName {
							v = fmt.Sprintf("Box::new(%s)", v)
						}
					}
				}
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		if isVariant {
			return fmt.Sprintf("%s::%s { %s }", sanitizeName(unionName), sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
		}
		return fmt.Sprintf("%s { %s }", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			if isStringLiteral(e) {
				v = fmt.Sprintf("%s.to_string()", v)
			}
			elems[i] = v
		}
		return fmt.Sprintf("vec![%s]", strings.Join(elems, ", ")), nil
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
			items[i] = fmt.Sprintf("(%s.to_string(), %s)", k, v)
		}
		return fmt.Sprintf("std::collections::HashMap::from([%s])", strings.Join(items, ", ")), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			s := strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") && !strings.Contains(s, ".") {
				s += ".0"
			}
			return s, nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		return "0", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			name := sanitizeName(p.Selector.Root)
			if c.methodFields != nil && c.methodFields[p.Selector.Root] {
				return fmt.Sprintf("self.%s", name), nil
			}
			return name, nil
		}
		parts := []string{sanitizeName(p.Selector.Root)}
		for _, t := range p.Selector.Tail {
			parts = append(parts, sanitizeName(t))
		}
		return strings.Join(parts, "."), nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", inner), nil
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	var paramTypes []types.Type
	if c.env != nil {
		if t, err := c.env.GetVar(call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				paramTypes = ft.Params
			}
		}
	}
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		if len(paramTypes) > i {
			if _, ok := paramTypes[i].(types.StringType); ok {
				if !isStringLiteral(a) {
					v = fmt.Sprintf("&%s", v)
				}
			}
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")
	switch call.Func {
	case "print":
		if len(args) == 1 && c.env != nil {
			if _, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
				return fmt.Sprintf("println!(\"[{}]\", %s.iter().map(|v| format!(\"{}\", v)).collect::<Vec<_>>().join(\" \"))", args[0]), nil
			}
		}
		fmtParts := make([]string, len(args))
		for i := range args {
			fmtParts[i] = "{}"
		}
		fmtStr := strings.Join(fmtParts, " ")
		return fmt.Sprintf("println!(\"%s\", %s)", fmtStr, argStr), nil
	case "str":
		if len(args) == 1 {
			return fmt.Sprintf("format!(\"{}\", %s)", argStr), nil
		}
		fmtParts := make([]string, len(args))
		for i := range args {
			fmtParts[i] = "{}"
		}
		fmtStr := strings.Join(fmtParts, " ")
		return fmt.Sprintf("format!(\"%s\", %s)", fmtStr, argStr), nil
	case "len":
		if len(args) == 1 {
			return fmt.Sprintf("%s.len() as i64", args[0]), nil
		}
	case "count":
		if len(args) == 1 {
			return fmt.Sprintf("%s.len() as i32", args[0]), nil
		}
	case "avg":
		if len(args) == 1 {
			return fmt.Sprintf("{ let v = &%s; if v.is_empty() { 0.0 } else { let mut sum = 0.0; for &it in v { sum += Into::<f64>::into(it); } sum / v.len() as f64 } }", args[0]), nil
		}
	case "input":
		if len(args) == 0 {
			return "{ use std::io::Read; let mut s = String::new(); std::io::stdin().read_line(&mut s).unwrap(); s.trim().to_string() }", nil
		}
	}
	return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	prompt := "\"\""
	text := "\"\""
	model := "\"\""
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
		}
	}
	if g.Target == "embedding" {
		c.use("_gen_embed")
		return fmt.Sprintf("_gen_embed(%s, %s)", text, model), nil
	}
	c.use("_gen_text")
	return fmt.Sprintf("_gen_text(%s, %s)", prompt, model), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s)", url), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	typ := "std::collections::HashMap<String, String>"
	if l.Type != nil {
		typ = rustType(l.Type)
	}
	c.use("_load")
	return fmt.Sprintf("_load::<%s>(%s)", typ, path), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s)", src, path), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := rustType(p.Type)
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ)
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("Box::new(move |%s| %s)", strings.Join(params, ", "), expr), nil
	}
	sub := &Compiler{env: c.env, helpers: c.helpers, structs: c.structs}
	sub.indent = 1
	for _, s := range fn.BlockBody {
		if err := sub.compileStmt(s); err != nil {
			return "", err
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return fmt.Sprintf("Box::new(move |%s| {\n%s})", strings.Join(params, ", "), body), nil
}
