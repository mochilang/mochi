package pycode

import (
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// --- Statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr)
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.If != nil:
		return c.compileIf(s.If, "if")
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Fetch != nil:
		return c.compileFetchStmt(s.Fetch)
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.Model != nil:
		return c.compileModelDecl(s.Model)
	case s.Fun != nil:
		return c.compileFunStmt(s.Fun)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Agent != nil:
		return c.compileAgentDecl(s.Agent)
	case s.Import != nil:
		if s.Import.Lang == nil {
			return c.compilePackageImport(s.Import)
		}
		if *s.Import.Lang == "go" && strings.Trim(s.Import.Path, "\"") == "strings" {
			// no-op, handled by special cases in call compilation
			return nil
		}
		if *s.Import.Lang != "python" {
			return fmt.Errorf("unsupported import language: %v", s.Import.Lang)
		}
		return nil
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
		// extern declarations have no runtime effect when compiling to Python
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	value := "None"
	var typ types.Type
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if typ == nil && s.Type != nil {
		typ = c.resolveTypeRef(s.Type)
	}
	if s.Value != nil {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := typ.(types.MapType); ok {
				c.imports["typing"] = "typing"
				value = fmt.Sprintf("typing.cast(dict[%s, %s], {})", pyType(mt.Key), pyType(mt.Value))
			}
		}
		if value == "None" {
			v, err := c.compileExpr(s.Value)
			if err != nil {
				return err
			}
			value = v
		}
	}
	if c.env != nil {
		t, err := c.env.GetVar(s.Name)
		if err != nil {
			if typ != nil {
				t = typ
			} else if s.Value != nil {
				t = c.inferExprType(s.Value)
			} else {
				t = types.AnyType{}
			}
			c.env.SetVar(s.Name, t, false)
		}
	}
	useAnn := typ != nil && !isAny(typ)
	if useAnn {
		c.imports["typing"] = "typing"
		c.writeln(fmt.Sprintf("%s: %s = %s", name, pyType(typ), value))
	} else {
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	if c.methodFields != nil && c.methodFields[s.Name] {
		name = fmt.Sprintf("self.%s", name)
	}
	value := "None"
	if s.Value != nil {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if c.env != nil {
				if t, err := c.env.GetVar(s.Name); err == nil {
					if mt, ok := t.(types.MapType); ok {
						c.imports["typing"] = "typing"
						value = fmt.Sprintf("typing.cast(dict[%s, %s], {})", pyType(mt.Key), pyType(mt.Value))
					}
				}
			}
		}
		if value == "None" {
			v, err := c.compileExpr(s.Value)
			if err != nil {
				return err
			}
			value = v
		}
	}
	if c.env != nil {
		t, err := c.env.GetVar(s.Name)
		if err != nil {
			if s.Type != nil {
				t = c.resolveTypeRef(s.Type)
			} else if s.Value != nil {
				t = c.inferExprType(s.Value)
			} else {
				t = types.AnyType{}
			}
			c.env.SetVar(s.Name, t, true)
		}
	}
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil && !isAny(t) {
			c.imports["typing"] = "typing"
			c.writeln(fmt.Sprintf("%s: %s = %s", name, pyType(t), value))
			return nil
		}
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)
	if c.methodFields != nil && c.methodFields[s.Name] {
		lhs = fmt.Sprintf("self.%s", lhs)
	}
	var typ types.Type
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	for _, idx := range s.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
		if mt, ok := typ.(types.MapType); ok {
			typ = mt.Value
		}
	}
	value := ""
	if len(s.Index) == 0 {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := typ.(types.MapType); ok {
				c.imports["typing"] = "typing"
				value = fmt.Sprintf("typing.cast(dict[%s, %s], {})", pyType(mt.Key), pyType(mt.Value))
			}
		}
	}
	if value == "" {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, value))
	return nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	if err := c.compileStructType(st); err != nil {
		return err
	}
	varName := unexportName(sanitizeName(s.Name)) + "Stream"
	c.use("_stream")
	c.writeln(fmt.Sprintf("%s = Stream(%q)", varName, s.Name))
	return nil
}

func (c *Compiler) compileModelDecl(m *parser.ModelDecl) error {
	c.models = true
	parts := make([]string, len(m.Fields))
	for i, f := range m.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%q: %s", f.Name, v)
	}
	c.writeln(fmt.Sprintf("_models[%q] = {%s}", m.Name, strings.Join(parts, ", ")))
	return nil
}

func (c *Compiler) compileOnHandler(h *parser.OnHandler) error {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", h.Stream)
	}
	if err := c.compileStructType(st); err != nil {
		return err
	}
	streamVar := unexportName(sanitizeName(h.Stream)) + "Stream"
	handlerName := fmt.Sprintf("_handler_%d", c.handlerCount)
	c.handlerCount++
	c.writeln(fmt.Sprintf("def %s(ev):", handlerName))
	c.indent++
	alias := sanitizeName(h.Alias)
	c.writeln(fmt.Sprintf("%s = ev", alias))
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.indent--
			return err
		}
	}
	c.indent--
	c.writeln(fmt.Sprintf("%s.register(%s)", streamVar, handlerName))
	c.use("_stream")
	return nil
}

func (c *Compiler) compileEmit(e *parser.EmitStmt) error {
	st, ok := c.env.GetStream(e.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", e.Stream)
	}
	if err := c.compileStructType(st); err != nil {
		return err
	}
	parts := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%s=%s", sanitizeName(f.Name), v)
	}
	lit := fmt.Sprintf("%s(%s)", sanitizeName(st.Name), strings.Join(parts, ", "))
	streamVar := unexportName(sanitizeName(e.Stream)) + "Stream"
	c.writeln(fmt.Sprintf("%s.append(%s)", streamVar, lit))
	c.use("_stream")
	return nil
}

func (c *Compiler) compileFetchStmt(f *parser.FetchStmt) error {
	expr, err := c.compileFetchExpr(&parser.FetchExpr{Pos: f.Pos, URL: f.URL, With: f.With})
	if err != nil {
		return err
	}
	name := sanitizeName(f.Target)
	if c.env != nil {
		c.env.SetVar(f.Target, types.AnyType{}, false)
	}
	c.writeln(fmt.Sprintf("%s = %s", name, expr))
	return nil
}

func (c *Compiler) compileAgentDecl(a *parser.AgentDecl) error {
	st, ok := c.env.GetStruct(a.Name)
	if !ok {
		return fmt.Errorf("unknown agent: %s", a.Name)
	}
	name := sanitizeName(a.Name)
	if c.agents[name] {
		return nil
	}
	c.agents[name] = true

	baseEnv := types.NewEnv(c.env)
	for _, fn := range st.Order {
		baseEnv.SetVar(fn, st.Fields[fn], true)
	}

	c.writeln(fmt.Sprintf("class %s:", name))
	c.indent++
	c.writeln("def __init__(self):")
	c.indent++
	c.use("_agent")
	c.writeln(fmt.Sprintf("self.Agent = Agent(%q)", a.Name))

	origEnv := c.env
	c.env = baseEnv
	for _, blk := range a.Body {
		switch {
		case blk.Let != nil:
			val := "None"
			if blk.Let.Value != nil {
				v, err := c.compileExpr(blk.Let.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("self.%s = %s", sanitizeName(blk.Let.Name), val))
		case blk.Var != nil:
			val := "None"
			if blk.Var.Value != nil {
				v, err := c.compileExpr(blk.Var.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("self.%s = %s", sanitizeName(blk.Var.Name), val))
		}
	}
	c.env = origEnv

	handlerID := 0
	for _, blk := range a.Body {
		if blk.On != nil {
			streamVar := unexportName(sanitizeName(blk.On.Stream)) + "Stream"
			c.writeln(fmt.Sprintf("self.Agent.on(%s, self._on%d)", streamVar, handlerID))
			handlerID++
		}
	}
	for _, blk := range a.Body {
		if blk.Intent != nil {
			mname := sanitizeName(blk.Intent.Name)
			c.writeln(fmt.Sprintf("self.Agent.register_intent(%q, self.%s)", blk.Intent.Name, mname))
		}
	}
	c.writeln("self.Agent.start()")
	c.indent--

	handlerID = 0
	for _, blk := range a.Body {
		switch {
		case blk.Intent != nil:
			if err := c.compileAgentIntent(name, baseEnv, blk.Intent); err != nil {
				return err
			}
		case blk.On != nil:
			if _, err := c.compileAgentOn(name, baseEnv, blk.On, handlerID); err != nil {
				return err
			}
			handlerID++
		}
	}
	c.indent--
	c.writeln("")
	c.writeln(fmt.Sprintf("def New%s():", name))
	c.indent++
	c.writeln(fmt.Sprintf("return %s()", name))
	c.indent--
	c.writeln("")
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) error {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	c.imports["dataclasses"] = "dataclasses"
	c.imports["typing"] = "typing"
	c.writeln("@dataclasses.dataclass")
	c.writeln(fmt.Sprintf("class %s:", name))
	c.indent++
	hasField := len(st.Order) > 0
	if !hasField && len(st.Methods) == 0 {
		c.writeln("pass")
	} else {
		for _, fn := range st.Order {
			typStr := pyType(st.Fields[fn])
			c.writeln(fmt.Sprintf("%s: %s", sanitizeName(fn), typStr))
		}
		if len(st.Methods) > 0 {
			base := types.NewEnv(c.env)
			for _, fn := range st.Order {
				base.SetVar(fn, st.Fields[fn], true)
			}
			for _, m := range st.Methods {
				c.writeln("")
				if err := c.compileMethod(name, base, m.Decl); err != nil {
					return err
				}
			}
		}
	}
	c.indent--
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			if err := c.compileStructType(sub); err != nil {
				return err
			}
		}
	}
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	c.imports["dataclasses"] = "dataclasses"
	c.imports["typing"] = "typing"
	name := sanitizeName(t.Name)
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("class %s:", name))
		c.indent++
		c.writeln("pass")
		c.indent--
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			c.writeln("@dataclasses.dataclass")
			c.writeln(fmt.Sprintf("class %s(%s):", vname, name))
			c.indent++
			if len(v.Fields) == 0 {
				c.writeln("pass")
			} else {
				for _, f := range v.Fields {
					typStr := pyType(c.resolveTypeRef(f.Type))
					c.writeln(fmt.Sprintf("%s: %s", sanitizeName(f.Name), typStr))
				}
			}
			c.indent--
		}
	} else {
		c.writeln("@dataclasses.dataclass")
		c.writeln(fmt.Sprintf("class %s:", name))
		c.indent++
		hasField := false
		for _, m := range t.Members {
			if m.Field != nil {
				hasField = true
				break
			}
		}
		base := types.NewEnv(c.env)
		for _, m := range t.Members {
			if m.Field != nil {
				ft := c.resolveTypeRef(m.Field.Type)
				base.SetVar(m.Field.Name, ft, true)
			}
		}
		if !hasField && len(t.Members) == 0 {
			c.writeln("pass")
		} else {
			for _, m := range t.Members {
				if m.Field != nil {
					typStr := pyType(c.resolveTypeRef(m.Field.Type))
					c.writeln(fmt.Sprintf("%s: %s", sanitizeName(m.Field.Name), typStr))
				}
			}
			for _, m := range t.Members {
				if m.Method != nil {
					c.writeln("")
					if err := c.compileMethod(name, base, m.Method); err != nil {
						return err
					}
				}
			}
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt, kw string) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("%s %s:\n", kw, cond))
	c.indent++
	if len(stmt.Then) == 0 {
		c.writeln("pass")
	} else {
		for _, s := range stmt.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		return c.compileIf(stmt.ElseIf, "elif")
	}
	if stmt.Else != nil {
		c.writeIndent()
		c.buf.WriteString("else:\n")
		c.indent++
		if len(stmt.Else) == 0 {
			c.writeln("pass")
		} else {
			for _, s := range stmt.Else {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("while %s:\n", cond))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for %s in range(%s, %s):\n", name, start, end))
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		return nil
	}
	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	t := c.inferExprType(stmt.Source)
	c.writeIndent()
	iter := ""
	switch tt := t.(type) {
	case types.ListType:
		iter = src
		if c.env != nil {
			c.env.SetVar(stmt.Name, tt.Elem, true)
		}
	case types.StringType:
		iter = src
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.StringType{}, true)
		}
	case types.MapType:
		iter = src
		if c.env != nil {
			c.env.SetVar(stmt.Name, tt.Key, true)
		}
	default:
		iter = src
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.AnyType{}, true)
		}
	}
	c.buf.WriteString(fmt.Sprintf("for %s in %s:\n", name, iter))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	idxVar := fmt.Sprintf("_i%d", c.tmpCount)
	c.tmpCount++
	itemVar := fmt.Sprintf("_it%d", c.tmpCount)
	c.tmpCount++

	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("for %s, %s in enumerate(%s):\n", idxVar, itemVar, list))
	c.indent++

	var st types.StructType
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}

	var orig *types.Env
	if st.Name != "" {
		child := types.NewEnv(c.env)
		for _, f := range st.Order {
			c.writeln(fmt.Sprintf("%s = %s.%s", sanitizeName(f), itemVar, sanitizeName(f)))
			child.SetVar(f, st.Fields[f], true)
		}
		orig = c.env
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			if orig != nil {
				c.env = orig
			}
			return err
		}
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("if %s:\n", cond))
		c.indent++
	}

	for _, it := range u.Set.Items {
		field, _ := identName(it.Key)
		val, err := c.compileExpr(it.Value)
		if err != nil {
			if orig != nil {
				c.env = orig
			}
			return err
		}
		c.writeln(fmt.Sprintf("setattr(%s, %q, %s)", itemVar, sanitizeName(field), val))
	}

	if u.Where != nil {
		c.indent--
	}

	c.writeln(fmt.Sprintf("%s[%s] = %s", list, idxVar, itemVar))

	if orig != nil {
		c.env = orig
	}

	c.indent--
	return nil
}
func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.imports["typing"] = "typing"
	c.writeIndent()
	c.buf.WriteString("def " + name + "(")
	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	paramTypes := make([]types.Type, len(fun.Params))
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
		var typ types.Type
		if i < len(ft.Params) {
			typ = ft.Params[i]
		} else if p.Type != nil {
			typ = c.resolveTypeRef(p.Type)
		}
		if typ != nil {
			c.buf.WriteString(": " + pyType(typ))
		} else {
			typ = types.AnyType{}
		}
		paramTypes[i] = typ
	}
	retType := "None"
	if ft.Return != nil {
		retType = pyType(ft.Return)
	} else if fun.Return != nil {
		retType = pyType(c.resolveTypeRef(fun.Return))
	}
	c.buf.WriteString(") -> " + retType + ":\n")
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		if i < len(paramTypes) {
			child.SetVar(p.Name, paramTypes[i], true)
		}
	}
	locals := map[string]bool{}
	for _, p := range fun.Params {
		locals[p.Name] = true
	}
	assigns := map[string]bool{}
	collectScopeInfo(fun.Body, locals, assigns)
	var nonlocals []string
	if c.env != nil {
		for name := range assigns {
			if !locals[name] {
				if _, err := c.env.GetVar(name); err == nil {
					nonlocals = append(nonlocals, sanitizeName(name))
				}
			}
		}
	}
	origEnv := c.env
	c.env = child
	c.indent++
	for _, n := range nonlocals {
		c.writeln("nonlocal " + n)
	}
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.indent--
	c.env = origEnv
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("def " + name + "():\n")
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileAgentIntent(agentName string, env *types.Env, in *parser.IntentDecl) error {
	name := sanitizeName(in.Name)
	c.writeIndent()
	c.buf.WriteString("def " + name + "(self")
	for _, p := range in.Params {
		c.buf.WriteString(", " + sanitizeName(p.Name))
	}
	c.buf.WriteString("):\n")
	child := types.NewEnv(env)
	orig := c.env
	c.env = child
	c.indent++
	for _, s := range in.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = orig
			return err
		}
	}
	c.indent--
	c.env = orig
	return nil
}

func (c *Compiler) compileAgentOn(agentName string, env *types.Env, h *parser.OnHandler, id int) (string, error) {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return "", fmt.Errorf("unknown stream: %s", h.Stream)
	}
	fname := fmt.Sprintf("_on%d", id)
	c.writeIndent()
	c.buf.WriteString("def " + fname + "(self, ev):\n")
	alias := sanitizeName(h.Alias)
	child := types.NewEnv(env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	c.indent++
	c.writeln(fmt.Sprintf("%s = ev", alias))
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return "", err
		}
	}
	c.indent--
	c.env = orig
	return fname, nil
}

func (c *Compiler) compileMethod(structName string, env *types.Env, fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.imports["typing"] = "typing"
	c.writeIndent()
	c.buf.WriteString("def " + name + "(self")
	var ft types.FuncType
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			if m, ok := st.Methods[fun.Name]; ok {
				ft = m.Type
			}
		}
	}
	paramTypes := make([]types.Type, len(fun.Params))
	for i, p := range fun.Params {
		c.buf.WriteString(", " + sanitizeName(p.Name))
		var typ types.Type
		if i < len(ft.Params) {
			typ = ft.Params[i]
		} else if p.Type != nil {
			typ = c.resolveTypeRef(p.Type)
		}
		if typ != nil {
			c.buf.WriteString(": " + pyType(typ))
		} else {
			typ = types.AnyType{}
		}
		paramTypes[i] = typ
	}
	retType := "None"
	if ft.Return != nil {
		retType = pyType(ft.Return)
	} else if fun.Return != nil {
		retType = pyType(c.resolveTypeRef(fun.Return))
	}
	c.buf.WriteString(") -> " + retType + ":\n")
	child := types.NewEnv(env)
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			c.methodFields = make(map[string]bool, len(st.Fields))
			for fname := range st.Fields {
				c.methodFields[fname] = true
			}
		}
	}
	for i, p := range fun.Params {
		if i < len(paramTypes) {
			child.SetVar(p.Name, paramTypes[i], true)
		}
	}
	orig := c.env
	c.env = child
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = orig
			c.methodFields = nil
			return err
		}
	}
	c.indent--
	c.env = orig
	c.methodFields = nil
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("assert %s", expr))
	return nil
}
