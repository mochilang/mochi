package rscode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Rust source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	helpers map[string]bool
	structs map[string]bool
	retType string
}

// New creates a new Rust compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), structs: make(map[string]bool)}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile returns Rust source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, stmt := range prog.Statements {
		if stmt.Type != nil {
			if err := c.compileTypeDecl(stmt.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, stmt := range prog.Statements {
		if stmt.Fun != nil {
			if err := c.compileFun(stmt.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("fn main() {")
	c.indent++
	for _, stmt := range prog.Statements {
		if stmt.Fun == nil {
			if err := c.compileStmt(stmt); err != nil {
				return nil, err
			}
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.emitRuntime()
	return c.buf.Bytes(), nil
}

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
		if c.retType == "String" && c.isStringExpr(s.Return.Value.Binary.Left.Value) {
			val = fmt.Sprintf("%s.to_string()", val)
		}
		c.writeln(fmt.Sprintf("return %s;", val))
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s;", expr))
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Type != nil:
		return nil
	case s.Test != nil:
		// Test blocks are ignored when compiling to Rust.
		return nil
	case s.Expect != nil:
		// Expect statements are used only inside tests, so skip them.
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	val := "Default::default()"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		if (stmt.Type != nil && stmt.Type.Simple != nil && *stmt.Type.Simple == "string") ||
			isStringLiteral(stmt.Value) || c.isStringExpr(stmt.Value.Binary.Left.Value) {
			v = fmt.Sprintf("%s.to_string()", v)
		}
		val = v
	} else if stmt.Type != nil && stmt.Type.Simple != nil && *stmt.Type.Simple == "string" {
		val = "String::new()"
	}
	name := sanitizeName(stmt.Name)
	c.writeln(fmt.Sprintf("let mut %s = %s;", name, val))
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	val := "Default::default()"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		if (stmt.Type != nil && stmt.Type.Simple != nil && *stmt.Type.Simple == "string") ||
			isStringLiteral(stmt.Value) || c.isStringExpr(stmt.Value.Binary.Left.Value) {
			v = fmt.Sprintf("%s.to_string()", v)
		}
		val = v
	} else if stmt.Type != nil && stmt.Type.Simple != nil && *stmt.Type.Simple == "string" {
		val = "String::new()"
	}
	name := sanitizeName(stmt.Name)
	c.writeln(fmt.Sprintf("let mut %s = %s;", name, val))
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[(%s) as usize]", lhs, iexpr)
	}
	val, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, val))
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		c.writeln("#[derive(Clone, Debug)]")
		c.writeln(fmt.Sprintf("enum %s {", name))
		c.indent++
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("%s,", vname))
			} else {
				c.writeln(fmt.Sprintf("%s {", vname))
				c.indent++
				for _, f := range v.Fields {
					typ := rustType(f.Type)
					if f.Type != nil && f.Type.Simple != nil && *f.Type.Simple == t.Name {
						typ = fmt.Sprintf("Box<%s>", typ)
					}
					c.writeln(fmt.Sprintf("%s: %s,", sanitizeName(f.Name), typ))
					if c.env != nil {
						st, ok := c.env.GetStruct(v.Name)
						if !ok {
							st = types.StructType{Name: v.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
						}
						st.Fields[f.Name] = c.resolveTypeRef(f.Type)
						st.Order = append(st.Order, f.Name)
						c.env.SetStruct(v.Name, st)

						ut, ok := c.env.GetUnion(t.Name)
						if !ok {
							ut = types.UnionType{Name: t.Name, Variants: map[string]types.StructType{}}
						}
						ut.Variants[v.Name] = st
						c.env.SetUnion(t.Name, ut)
					}
				}
				c.indent--
				c.writeln("},")
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("#[derive(Clone, Debug)]")
	c.writeln(fmt.Sprintf("struct %s {", name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			field := sanitizeName(m.Field.Name)
			typ := rustType(m.Field.Type)
			c.writeln(fmt.Sprintf("%s: %s,", field, typ))
			if c.env != nil {
				st, ok := c.env.GetStruct(t.Name)
				if !ok {
					st = types.StructType{Name: t.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
				}
				st.Fields[m.Field.Name] = c.resolveTypeRef(m.Field.Type)
				st.Order = append(st.Order, m.Field.Name)
				c.env.SetStruct(t.Name, st)
			}
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	start, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	end := ""
	if stmt.RangeEnd != nil {
		end, err = c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
	}
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		c.writeln(fmt.Sprintf("for %s in %s..%s {", name, start, end))
	} else {
		if isStringLiteral(stmt.Source) {
			c.writeln(fmt.Sprintf("for %s in %s.chars() {", name, start))
		} else {
			c.writeln(fmt.Sprintf("for %s in %s {", name, start))
		}
	}
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while %s {", cond))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeln("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fn " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		typ := rustType(p.Type)
		if p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "string" {
			typ = "&str"
		}
		c.buf.WriteString(fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ))
	}
	c.buf.WriteString(")")
	ret := ""
	if fun.Return != nil {
		ret = rustType(fun.Return)
		c.buf.WriteString(" -> " + ret)
	}
	c.buf.WriteString(" {\n")
	origEnv := c.env
	origRet := c.retType
	c.retType = ret
	if c.env != nil {
		child := types.NewEnv(c.env)
		for _, p := range fun.Params {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		}
		c.env = child
	}
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.indent--
	c.env = origEnv
	c.retType = origRet
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	leftList := c.isListExpr(b.Left.Value)
	leftStr := c.isStringExpr(b.Left.Value)
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightList := c.isListExpr(op.Right)
		rightStr := c.isStringExpr(op.Right)
		switch op.Op {
		case "+":
			if leftList || rightList {
				c.use("_concat")
				expr = fmt.Sprintf("_concat(&%s, &%s)", expr, r)
				leftList = true
				leftStr = false
			} else if leftStr || rightStr {
				expr = fmt.Sprintf("format!(\"{}{}\", %s, %s)", expr, r)
				leftStr = true
				leftList = false
			} else {
				expr = fmt.Sprintf("%s + %s", expr, r)
				leftList = false
				leftStr = false
			}
		case "in":
			c.use("_in_map")
			expr = fmt.Sprintf("_in_map(&%s, &%s)", r, expr)
			leftList = false
			leftStr = false
		default:
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, r)
			leftList = false
			leftStr = false
		}
	}
	return expr, nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
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
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var cond string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	var b strings.Builder
	b.WriteString("{\n")
	b.WriteString("    let mut _res = Vec::new();\n")
	loopSrc := src
	if len(fromSrcs) > 0 {
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
	if cond != "" {
		b.WriteString(indent + fmt.Sprintf("if %s {\n", cond))
		indent += "    "
		b.WriteString(indent + fmt.Sprintf("_res.push(%s);\n", sel))
		indent = indent[:len(indent)-4]
		b.WriteString(indent + "}\n")
	} else {
		b.WriteString(indent + fmt.Sprintf("_res.push(%s);\n", sel))
	}
	for range fromSrcs {
		indent = indent[:len(indent)-4]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("    }\n")
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
				if c.isStringExpr(p) {
					expr = fmt.Sprintf("%s.chars().nth((%s) as usize).unwrap()", expr, iexpr)
				} else if isStringLiteral(idx.Start) {
					expr = fmt.Sprintf("%s[%s]", expr, iexpr)
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
				if c.isStringExpr(p) {
					expr = fmt.Sprintf("%s[((%s) as usize)..((%s) as usize)].to_string()", expr, start, end)
				} else {
					expr = fmt.Sprintf("%s[((%s) as usize)..((%s) as usize)].to_vec()", expr, start, end)
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
			return sanitizeName(p.Selector.Root), nil
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
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")
	switch call.Func {
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("println!(\"{}\", %s)", argStr), nil
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
			return fmt.Sprintf("%s.len() as i32", args[0]), nil
		}
	case "count":
		if len(args) == 1 {
			c.use("_count")
			return fmt.Sprintf("_count(&%s)", args[0]), nil
		}
	case "avg":
		if len(args) == 1 {
			c.use("_avg")
			return fmt.Sprintf("_avg(&%s)", args[0]), nil
		}
	case "input":
		if len(args) == 0 {
			c.use("_input")
			return "_input()", nil
		}
	}
	return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
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

func rustType(t *parser.TypeRef) string {
	if t == nil {
		return "()"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i32"
		case "float":
			return "f64"
		case "string":
			return "String"
		case "bool":
			return "bool"
		default:
			return sanitizeName(*t.Simple)
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return fmt.Sprintf("Vec<%s>", rustType(t.Generic.Args[0]))
		}
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = rustType(p)
		}
		ret := "()"
		if t.Fun.Return != nil {
			ret = rustType(t.Fun.Return)
		}
		return fmt.Sprintf("Box<dyn Fn(%s) -> %s>", strings.Join(params, ", "), ret)
	}
	return "()"
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 {
		return false
	}
	if p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil {
		return false
	}
	return true
}

func (c *Compiler) isListExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil || len(p.Ops) > 0 {
		return false
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

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	res := b.String()
	if res == "" || !((res[0] >= 'A' && res[0] <= 'Z') || (res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
		res = "_" + res
	}
	return res
}

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("    ", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}
