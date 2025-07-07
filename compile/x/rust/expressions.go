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
			if rightList {
				expr = fmt.Sprintf("%s.contains(&%s)", r, expr)
			} else if rightString {
				c.use("_in_string")
				expr = fmt.Sprintf("_in_string(%s, %s)", r, expr)
			} else if c.isMapExpr(op.Right) {
				expr = fmt.Sprintf("%s.contains_key(&%s)", r, expr)
			} else {
				expr = fmt.Sprintf("%s.contains(&%s)", r, expr)
			}
			leftList = false
			leftString = false
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
	src, err := c.compileIterExpr(q.Source)
	if err != nil {
		return "", err
	}

	// Handle simple grouping without joins. Allow optional filtering
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		selExpr, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		var whereExpr string
		if q.Where != nil {
			if w, err := c.compileExpr(q.Where); err == nil {
				whereExpr = w
			} else {
				return "", err
			}
		}
		var elemType types.Type = types.AnyType{}
		srcType := c.inferExprType(q.Source)
		if lt, ok := srcType.(types.ListType); ok {
			elemType = lt.Elem
		}
		keyType := c.inferExprType(q.Group.Exprs[0])
		retType := c.inferExprType(q.Select)
		alias := sanitizeName(q.Group.Name)
		var b strings.Builder
		b.WriteString("{\n")
		b.WriteString("    #[derive(Clone, Debug)]\n")
		b.WriteString(fmt.Sprintf("    struct Group { key: %s, items: Vec<%s> }\n", rustTypeFrom(keyType), rustTypeFrom(elemType)))
		b.WriteString("    let mut groups: std::collections::HashMap<String, Group> = std::collections::HashMap::new();\n")
		b.WriteString("    let mut order: Vec<String> = Vec::new();\n")
		b.WriteString(fmt.Sprintf("    for %s in %s.clone() {\n", sanitizeName(q.Var), src))
		if whereExpr != "" {
			b.WriteString(fmt.Sprintf("        if !(%s) { continue; }\n", whereExpr))
		}
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

	// Handle grouping with join or from sources without sorting/pagination
	if q.Group != nil && (len(q.Froms) > 0 || len(q.Joins) > 0) && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		selExpr, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		var whereExpr string
		if q.Where != nil {
			if w, err := c.compileExpr(q.Where); err == nil {
				whereExpr = w
			} else {
				return "", err
			}
		}
		var elemType types.Type = types.AnyType{}
		srcType := c.inferExprType(q.Source)
		if lt, ok := srcType.(types.ListType); ok {
			elemType = lt.Elem
		}
		keyType := c.inferExprType(q.Group.Exprs[0])
		retType := c.inferExprType(q.Select)
		alias := sanitizeName(q.Group.Name)

		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileIterExpr(f.Src)
			if err != nil {
				return "", err
			}
			fromSrcs[i] = fs
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileIterExpr(j.Src)
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

		var b strings.Builder
		b.WriteString("{\n")
		b.WriteString("    #[derive(Clone, Debug)]\n")
		b.WriteString(fmt.Sprintf("    struct Group { key: %s, items: Vec<%s> }\n", rustTypeFrom(keyType), rustTypeFrom(elemType)))
		b.WriteString("    let mut groups: std::collections::HashMap<String, Group> = std::collections::HashMap::new();\n")
		b.WriteString("    let mut order: Vec<String> = Vec::new();\n")
		b.WriteString(fmt.Sprintf("    for %s in %s.clone() {\n", sanitizeName(q.Var), src))
		indent := "        "
		for i, fs := range fromSrcs {
			b.WriteString(indent + fmt.Sprintf("for %s in %s.clone() {\n", sanitizeName(q.Froms[i].Var), fs))
			indent += "    "
		}
		for i, js := range joinSrcs {
			b.WriteString(indent + fmt.Sprintf("for %s in %s.clone() {\n", sanitizeName(q.Joins[i].Var), js))
			indent += "    "
			b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", joinOns[i]))
		}
		if whereExpr != "" {
			b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", whereExpr))
		}
		b.WriteString(indent + fmt.Sprintf("let key: %s = %s;\n", rustTypeFrom(keyType), keyExpr))
		b.WriteString(indent + "let ks = format!(\"{:?}\", key.clone());\n")
		b.WriteString(indent + "if !groups.contains_key(&ks) {\n")
		b.WriteString(indent + "    groups.insert(ks.clone(), Group{ key: key.clone(), items: Vec::new() });\n")
		b.WriteString(indent + "    order.push(ks.clone());\n")
		b.WriteString(indent + "}\n")
		b.WriteString(indent + fmt.Sprintf("groups.get_mut(&ks).unwrap().items.push(%s.clone());\n", sanitizeName(q.Var)))
		for range joinSrcs {
			indent = indent[:len(indent)-4]
			b.WriteString(indent + "}\n")
		}
		for range fromSrcs {
			indent = indent[:len(indent)-4]
			b.WriteString(indent + "}\n")
		}
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
		fs, err := c.compileIterExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	joinTypes := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileIterExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinOns[i] = on
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
		jt := c.inferExprType(j.Src)
		if lt, ok := jt.(types.ListType); ok {
			joinTypes[i] = rustTypeFrom(lt.Elem)
		} else {
			joinTypes[i] = "()"
		}
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}

	var whereExpr string
	pushAlias := ""
	if q.Where != nil {
		if aliases := usedAliases(q.Where); len(aliases) == 1 {
			for a := range aliases {
				pushAlias = a
			}
		}
		var err error
		whereExpr, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}

	pushDown := false
	if pushAlias != "" {
		if pushAlias == q.Var {
			pushDown = true
		}
		for _, f := range q.Froms {
			if pushAlias == f.Var {
				pushDown = true
			}
		}
		for i, j := range q.Joins {
			if pushAlias == j.Var && joinSides[i] == "" {
				pushDown = true
			}
		}
	}

	condParts := make([]string, 0, len(joinOns)+1)
	for _, on := range joinOns {
		condParts = append(condParts, on)
	}
	if q.Where != nil && !pushDown {
		condParts = append(condParts, whereExpr)
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
	if pushDown && pushAlias == q.Var {
		b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", whereExpr))
	}
	specialJoin := len(q.Joins) == 1 && joinSides[0] == "left"

	for i, fs := range fromSrcs {
		fvar := sanitizeName(q.Froms[i].Var)
		srcPart := fs + ".clone()"
		b.WriteString(indent + fmt.Sprintf("for %s in %s {\n", fvar, srcPart))
		indent += "    "
		if pushDown && pushAlias == q.Froms[i].Var {
			b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", whereExpr))
		}
	}
	if specialJoin {
		jvar := sanitizeName(q.Joins[0].Var)
		jsrc := joinSrcs[0]
		b.WriteString(indent + "let mut matched = false;\n")
		b.WriteString(indent + fmt.Sprintf("for %s in %s.clone() {\n", jvar, jsrc))
		indent += "    "
		b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", joinOns[0]))
		if pushDown && pushAlias == q.Joins[0].Var {
			b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", whereExpr))
		}
		b.WriteString(indent + "matched = true;\n")
	} else {
		for i, js := range joinSrcs {
			jvar := sanitizeName(q.Joins[i].Var)
			srcPart := js + ".clone()"
			b.WriteString(indent + fmt.Sprintf("for %s in %s {\n", jvar, srcPart))
			indent += "    "
			b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", joinOns[i]))
			if pushDown && pushAlias == q.Joins[i].Var {
				b.WriteString(indent + fmt.Sprintf("if !(%s) { continue; }\n", whereExpr))
			}
		}
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
	if specialJoin {
		indent = indent[:len(indent)-4]
		b.WriteString(indent + "}\n")
		b.WriteString(indent + "if !matched {\n")
		indent += "    "
		b.WriteString(indent + fmt.Sprintf("let %s: %s = Default::default();\n", sanitizeName(q.Joins[0].Var), joinTypes[0]))
		if cond != "" {
			b.WriteString(indent + fmt.Sprintf("if %s {\n", cond))
			indent += "    "
		}
		if cond != "" && q.Sort != nil {
			sortKey, err := c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			b.WriteString(indent + fmt.Sprintf("_pairs.push((%s, %s));\n", sortKey, sel))
		} else if q.Sort != nil {
			sortKey, err := c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			b.WriteString(indent + fmt.Sprintf("_pairs.push((%s, %s));\n", sortKey, sel))
		} else {
			b.WriteString(indent + fmt.Sprintf("_res.push(%s);\n", sel))
		}
		if cond != "" {
			indent = indent[:len(indent)-4]
			b.WriteString(indent + "}\n")
		}
		indent = indent[:len(indent)-4]
		b.WriteString(indent + "}\n")
	} else {
		for range joinSrcs {
			indent = indent[:len(indent)-4]
			b.WriteString(indent + "}\n")
		}
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
	return types.ResolveTypeRef(t, c.env)
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
				if c.isStringBase(p) {
					c.use("_slice_string")
					expr = fmt.Sprintf("_slice_string(%s, %s, %s)", expr, start, end)
				} else {
					simpleStart := idx.Start != nil
					if simpleStart {
						_, simpleStart = identName(idx.Start)
					}
					simpleEnd := idx.End != nil
					if simpleEnd {
						_, simpleEnd = identName(idx.End)
					}
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
		var valType types.Type = types.AnyType{}
		if mt, ok := c.inferPrimaryType(&parser.Primary{Map: p.Map}).(types.MapType); ok {
			valType = mt.Value
		}
		for i, it := range p.Map.Items {
			kexpr, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if s, ok := simpleStringKey(it.Key); ok {
				kexpr = fmt.Sprintf("%q.to_string()", s)
			} else if isStringLiteral(it.Key) {
				kexpr = fmt.Sprintf("%s.to_string()", kexpr)
			} else {
				kexpr = fmt.Sprintf("%s.to_string()", kexpr)
			}
			vexpr, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			if _, ok := valType.(types.AnyType); ok {
				vexpr = fmt.Sprintf("std::rc::Rc::new(%s)", vexpr)
			}
			items[i] = fmt.Sprintf("(%s, %s)", kexpr, vexpr)
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
		if p.Lit.Null {
			return "Default::default()", nil
		}
		return "0", nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			name := sanitizeName(p.Selector.Root)
			if c.methodFields != nil && c.methodFields[p.Selector.Root] {
				return fmt.Sprintf("self.%s", name), nil
			}
			return name, nil
		}
		expr := sanitizeName(p.Selector.Root)
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		for _, tname := range p.Selector.Tail {
			if _, ok := typ.(types.MapType); ok || typ == (types.AnyType{}) {
				c.use("_map_get")
				expr = fmt.Sprintf("_map_get(&%s, &\"%s\".to_string())", expr, tname)
				if mt, ok := typ.(types.MapType); ok {
					typ = mt.Value
				} else {
					typ = types.AnyType{}
				}
				switch typ.(type) {
				case types.IntType, types.Int64Type:
					c.use("_cast_int")
					expr = fmt.Sprintf("_cast_int(%s)", expr)
				case types.FloatType:
					c.use("_cast_float")
					expr = fmt.Sprintf("_cast_float(%s)", expr)
				case types.StringType:
					c.use("_cast_string")
					expr = fmt.Sprintf("_cast_string(%s)", expr)
				case types.BoolType:
					c.use("_cast_bool")
					expr = fmt.Sprintf("_cast_bool(%s)", expr)
				}
			} else {
				expr = fmt.Sprintf("%s.%s", expr, sanitizeName(tname))
				if st, ok := typ.(types.StructType); ok {
					if ft, ok2 := st.Fields[tname]; ok2 {
						typ = ft
					} else {
						typ = types.AnyType{}
					}
				} else {
					typ = types.AnyType{}
				}
			}
		}
		return expr, nil
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
		return "", fmt.Errorf("unsupported expression at %d:%d", p.Pos.Line, p.Pos.Column)
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
			c.use("_count")
			src := fmt.Sprintf("&%s", args[0])
			if _, ok := c.inferExprType(call.Args[0]).(types.GroupType); ok {
				src = fmt.Sprintf("&%s.items", args[0])
			}
			return fmt.Sprintf("_count(%s)", src), nil
		}
	case "avg":
		if len(args) == 1 {
			c.use("_avg")
			src := fmt.Sprintf("&%s", args[0])
			if _, ok := c.inferExprType(call.Args[0]).(types.GroupType); ok {
				src = fmt.Sprintf("&%s.items", args[0])
			}
			return fmt.Sprintf("_avg(%s)", src), nil
		}
	case "sum":
		if len(args) == 1 {
			c.use("_sum")
			src := fmt.Sprintf("&%s", args[0])
			if _, ok := c.inferExprType(call.Args[0]).(types.GroupType); ok {
				src = fmt.Sprintf("&%s.items", args[0])
			}
			return fmt.Sprintf("_sum(%s)", src), nil
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
	return c.compileFetchExprWithType(f, "std::rc::Rc<dyn std::any::Any>")
}

func (c *Compiler) compileFetchExprWithType(f *parser.FetchExpr, typ string) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "std::collections::HashMap::new()"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch::<%s>(%s, %s)", typ, url, opts), nil
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
	opts := "std::collections::HashMap::new()"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_load")
	return fmt.Sprintf("_load::<%s>(%s, %s)", typ, path, opts), nil
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
	opts := "std::collections::HashMap::new()"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
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
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "Default::default()"
	}
	return fmt.Sprintf("if %s { %s } else { %s }", cond, thenExpr, elseExpr), nil
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
	sub := &Compiler{env: c.env, helpers: c.helpers, structs: c.structs, locals: map[string]types.Type{}}
	sub.indent = 1
	for _, s := range fn.BlockBody {
		if err := sub.compileStmt(s); err != nil {
			return "", err
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return fmt.Sprintf("Box::new(move |%s| {\n%s})", strings.Join(params, ", "), body), nil
}
