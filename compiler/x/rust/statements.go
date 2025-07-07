package rscode

import (
	"fmt"

	"mochi/parser"
	"mochi/types"
)

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
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Type != nil:
		return nil
	case s.Test != nil:
		// Test blocks are compiled separately before main.
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	default:
		return fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	val := "Default::default()"
	if stmt.Value != nil {
		if f, ok := fetchExpr(stmt.Value); ok && stmt.Type != nil {
			v, err := c.compileFetchExprWithType(f, rustType(stmt.Type))
			if err != nil {
				return err
			}
			val = v
		} else {
			v, err := c.compileExpr(stmt.Value)
			if err != nil {
				return err
			}
			if isStringLiteral(stmt.Value) {
				v = fmt.Sprintf("%s.to_string()", v)
			}
			val = v
		}
	}
	name := sanitizeName(stmt.Name)
	if stmt.Type != nil {
		c.writeln(fmt.Sprintf("let mut %s: %s = %s;", name, rustType(stmt.Type), val))
	} else {
		c.writeln(fmt.Sprintf("let mut %s = %s;", name, val))
	}
	if c.env != nil {
		var typ types.Type
		if stmt.Type != nil {
			typ = c.resolveTypeRef(stmt.Type)
		} else {
			typ = c.inferExprType(stmt.Value)
		}
		c.env.SetVar(stmt.Name, typ, false)
		c.locals[stmt.Name] = typ
	}
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	val := "Default::default()"
	if stmt.Value != nil {
		if f, ok := fetchExpr(stmt.Value); ok && stmt.Type != nil {
			v, err := c.compileFetchExprWithType(f, rustType(stmt.Type))
			if err != nil {
				return err
			}
			val = v
		} else {
			v, err := c.compileExpr(stmt.Value)
			if err != nil {
				return err
			}
			if isStringLiteral(stmt.Value) {
				v = fmt.Sprintf("%s.to_string()", v)
			}
			val = v
		}
	}
	name := sanitizeName(stmt.Name)
	if stmt.Type != nil {
		c.writeln(fmt.Sprintf("let mut %s: %s = %s;", name, rustType(stmt.Type), val))
	} else {
		c.writeln(fmt.Sprintf("let mut %s = %s;", name, val))
	}
	if c.env != nil {
		var typ types.Type
		if stmt.Type != nil {
			typ = c.resolveTypeRef(stmt.Type)
		} else {
			typ = c.inferExprType(stmt.Value)
		}
		c.env.SetVar(stmt.Name, typ, true)
		c.locals[stmt.Name] = typ
	}
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	if c.methodFields != nil && c.methodFields[stmt.Name] {
		lhs = fmt.Sprintf("self.%s", lhs)
	}

	// Handle simple map assignments like m[k] = v
	if len(stmt.Index) == 1 && (c.isMapVar(stmt.Name) || isStringLiteral(stmt.Index[0].Start)) {
		keyExpr, err := c.compileExpr(stmt.Index[0].Start)
		if err != nil {
			return err
		}
		if isStringLiteral(stmt.Index[0].Start) {
			keyExpr = fmt.Sprintf("%s.to_string()", keyExpr)
		}
		val, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s.insert(%s, %s);", lhs, keyExpr, val))
		return nil
	}

	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		if id, ok := identName(idx.Start); ok {
			lhs = fmt.Sprintf("%s[%s as usize]", lhs, id)
		} else {
			lhs = fmt.Sprintf("%s[(%s) as usize]", lhs, iexpr)
		}
	}
	var val string
	var err error
	if f, ok := fetchExpr(stmt.Value); ok {
		if t, errVar := c.env.GetVar(stmt.Name); errVar == nil {
			val, err = c.compileFetchExprWithType(f, rustTypeFrom(t))
		} else {
			val, err = c.compileExpr(stmt.Value)
		}
	} else {
		val, err = c.compileExpr(stmt.Value)
	}
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, val))
	if c.env != nil && len(stmt.Index) == 0 {
		typ := c.inferExprType(stmt.Value)
		mut, _ := c.env.IsMutable(stmt.Name)
		c.env.SetVar(stmt.Name, typ, mut)
		c.locals[stmt.Name] = typ
	}
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln("fn " + name + "() {")
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.use("expect")
	c.writeln(fmt.Sprintf("expect(%s);", expr))
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
	c.writeln("#[derive(Clone, Debug, Default)]")
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

	// Compile methods defined within the type
	for _, m := range t.Members {
		if m.Method != nil {
			if c.env != nil {
				st, ok := c.env.GetStruct(t.Name)
				if !ok {
					st = types.StructType{Name: t.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
				}
				params := make([]types.Type, len(m.Method.Params))
				for i, p := range m.Method.Params {
					params[i] = c.resolveTypeRef(p.Type)
				}
				var ret types.Type = types.VoidType{}
				if m.Method.Return != nil {
					ret = c.resolveTypeRef(m.Method.Return)
				}
				st.Methods[m.Method.Name] = types.Method{Decl: m.Method, Type: types.FuncType{Params: params, Return: ret}}
				c.env.SetStruct(t.Name, st)
			}
			c.writeln(fmt.Sprintf("impl %s {", name))
			c.indent++
			if err := c.compileMethod(name, m.Method); err != nil {
				return err
			}
			c.indent--
			c.writeln("}")
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	start, err := c.compileIterExpr(stmt.Source)
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
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.IntType{}, true)
			c.locals[stmt.Name] = types.IntType{}
		}
		c.indent++
	} else {
		if isStringLiteral(stmt.Source) || c.isStringExpr(stmt.Source.Binary.Left.Value) {
			tmp := name + "_ch"
			c.writeln(fmt.Sprintf("for %s in %s.chars() {", tmp, start))
			if c.env != nil {
				c.env.SetVar(stmt.Name, types.StringType{}, true)
				c.locals[stmt.Name] = types.StringType{}
			}
			c.indent++
			c.writeln(fmt.Sprintf("let %s = %s.to_string();", name, tmp))
		} else if c.isMapExpr(stmt.Source.Binary.Left.Value) {
			keyTmp := name + "_key"
			c.writeln(fmt.Sprintf("for %s in %s.keys() {", keyTmp, start))
			if c.env != nil {
				srcType := c.inferExprType(stmt.Source)
				if mt, ok := srcType.(types.MapType); ok {
					c.env.SetVar(stmt.Name, mt.Key, true)
					c.locals[stmt.Name] = mt.Key
				} else {
					c.env.SetVar(stmt.Name, types.AnyType{}, true)
					c.locals[stmt.Name] = types.AnyType{}
				}
			}
			c.indent++
			c.writeln(fmt.Sprintf("let %s = %s.clone();", name, keyTmp))
		} else {
			c.writeln(fmt.Sprintf("for %s in %s {", name, start))
			if c.env != nil {
				srcType := c.inferExprType(stmt.Source)
				if lt, ok := srcType.(types.ListType); ok {
					c.env.SetVar(stmt.Name, lt.Elem, true)
					c.locals[stmt.Name] = lt.Elem
				} else if gt, ok := srcType.(types.GroupType); ok {
					c.env.SetVar(stmt.Name, gt.Elem, true)
					c.locals[stmt.Name] = gt.Elem
				} else {
					c.env.SetVar(stmt.Name, types.AnyType{}, true)
					c.locals[stmt.Name] = types.AnyType{}
				}
			}
			c.indent++
		}
	}
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
		name := sanitizeName(p.Name)
		if paramMutated(fun.Body, p.Name) {
			c.buf.WriteString(fmt.Sprintf("mut %s: %s", name, typ))
		} else {
			c.buf.WriteString(fmt.Sprintf("%s: %s", name, typ))
		}
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
	origLocals := c.locals
	c.locals = map[string]types.Type{}
	c.retType = ret
	if c.env != nil {
		child := types.NewEnv(c.env)
		for _, p := range fun.Params {
			t := c.resolveTypeRef(p.Type)
			child.SetVar(p.Name, t, true)
			c.locals[p.Name] = t
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
	c.locals = origLocals
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fn " + name + "(&mut self")
	for _, p := range fun.Params {
		typ := rustType(p.Type)
		c.buf.WriteString(fmt.Sprintf(", %s: %s", sanitizeName(p.Name), typ))
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
	origLocals := c.locals
	c.locals = map[string]types.Type{}
	c.retType = ret
	if c.env != nil {
		child := types.NewEnv(c.env)
		if st, ok := c.env.GetStruct(structName); ok {
			c.methodFields = make(map[string]bool, len(st.Fields))
			for fname, ft := range st.Fields {
				child.SetVar(fname, ft, true)
				c.methodFields[fname] = true
				c.locals[fname] = ft
			}
		}
		for _, p := range fun.Params {
			t := c.resolveTypeRef(p.Type)
			child.SetVar(p.Name, t, true)
			c.locals[p.Name] = t
		}
		c.env = child
	}
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = origEnv
			c.methodFields = nil
			return err
		}
	}
	c.indent--
	c.env = origEnv
	c.retType = origRet
	c.locals = origLocals
	c.methodFields = nil
	c.writeln("}")
	return nil
}
