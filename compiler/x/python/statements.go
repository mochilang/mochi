//go:build slow

package pycode

import (
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func toCamelCase(name string) string {
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return r == '_' || r == '-' || r == ' '
	})
	for i, p := range parts {
		if p == "" {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + strings.ToLower(p[1:])
	}
	return strings.Join(parts, "")
}

func dataclassNameFromVar(name string) string {
	if name == "people" {
		name = "person"
	}
	if strings.HasSuffix(name, "ies") {
		name = name[:len(name)-3] + "y"
	} else if strings.HasSuffix(name, "s") {
		name = name[:len(name)-1]
	}
	return toCamelCase(name)
}

func (c *Compiler) registerAutoStruct(name string, st types.StructType) types.StructType {
	key := structKey(st)
	if n, ok := c.structKeys[key]; ok {
		st.Name = n
		return st
	}
	c.imports["dataclasses"] = "dataclasses"
	st.Name = name
	c.autoStructs[name] = st
	c.structKeys[key] = name
	if c.env != nil {
		c.env.SetStruct(name, st)
	}
	return st
}

func (c *Compiler) listAsStruct(ll *parser.ListLiteral, varName string) (string, types.StructType, bool, error) {
	if ll == nil || len(ll.Elems) == 0 {
		return "", types.StructType{}, false, nil
	}
	firstMap := ll.Elems[0].Binary.Left.Value.Target.Map
	if firstMap == nil {
		return "", types.StructType{}, false, nil
	}
	keys := make([]string, len(firstMap.Items))
	fields := make(map[string]types.Type, len(firstMap.Items))
	for i, it := range firstMap.Items {
		name, ok := identName(it.Key)
		if !ok {
			return "", types.StructType{}, false, nil
		}
		keys[i] = name
		fields[name] = c.inferExprType(it.Value)
	}
	for _, el := range ll.Elems[1:] {
		m := el.Binary.Left.Value.Target.Map
		if m == nil || len(m.Items) != len(keys) {
			return "", types.StructType{}, false, nil
		}
		for i, it := range m.Items {
			name, ok := identName(it.Key)
			if !ok || name != keys[i] {
				return "", types.StructType{}, false, nil
			}
			t := c.inferExprType(it.Value)
			if ft, ok := fields[name]; ok {
				if !equalTypes(ft, t) {
					fields[name] = types.AnyType{}
				}
			}
		}
	}
	name := dataclassNameFromVar(varName)
	st := types.StructType{Fields: fields, Order: keys}
	st = c.registerAutoStruct(name, st)
	elems := make([]string, len(ll.Elems))
	for i, el := range ll.Elems {
		m := el.Binary.Left.Value.Target.Map
		parts := make([]string, len(m.Items))
		for j, it := range m.Items {
			name, _ := identName(it.Key)
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", types.StructType{}, false, err
			}
			parts[j] = fmt.Sprintf("%s=%s", sanitizeName(name), v)
		}
		elems[i] = fmt.Sprintf("%s(%s)", sanitizeName(st.Name), strings.Join(parts, ", "))
	}
	return "[" + strings.Join(elems, ", ") + "]", st, true, nil
}

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
		if s.Return.Value == nil {
			c.writeln("return")
			return nil
		}
		if fn := simpleFunExpr(s.Return.Value); fn != nil && fn.ExprBody != nil {
			name, err := c.compileFunExprDef(fn)
			if err != nil {
				return err
			}
			c.writeln("return " + name)
			return nil
		}
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
		if *s.Import.Lang == "go" {
			p := strings.Trim(s.Import.Path, "\"")
			if p == "strings" {
				// no-op, handled by special cases in call compilation
				return nil
			}
			if s.Import.Auto && p == "mochi/runtime/ffi/go/testpkg" {
				// simple built-in Go package used by tests
				return nil
			}
			if p == "net" {
				c.imports["socket"] = "socket"
				if s.Import.As != "" {
					alias := sanitizeName(s.Import.As)
					c.writeln(fmt.Sprintf("%s = socket", alias))
				} else {
					c.writeln("net = socket")
				}
				return nil
			}
			return fmt.Errorf("unsupported import language: %v", s.Import.Lang)
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
	value := "UNDEFINED"
	var typ types.Type
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if typ == nil && s.Type != nil {
		typ = c.resolveTypeRef(s.Type)
	}
	if typ == nil && s.Value != nil {
		typ = c.inferExprType(s.Value)
	}
	if s.Value != nil {
		if c.autoStructsEnabled {
			if ll := s.Value.Binary.Left.Value.Target.List; ll != nil {
				if val, st, ok, err := c.listAsStruct(ll, s.Name); ok && err == nil {
					value = val
					typ = types.ListType{Elem: st}
				} else if err != nil {
					return err
				}
			}
		}
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := typ.(types.MapType); ok {
				typStr := fmt.Sprintf("typing.cast(dict[%s, %s], {})", pyType(c.namedType(mt.Key)), pyType(c.namedType(mt.Value)))
				if c.typeHints {
					if c.typeHints {
						if needsTyping(typStr) {
							c.imports["typing"] = "typing"
						}
						value = typStr
					} else {
						value = "{}"
					}
				} else {
					value = "{}"
				}
			}
		}
		if value == "UNDEFINED" {
			var err error
			if typ != nil && !isAny(typ) {
				value, err = c.compileExprHint(s.Value, typ)
			} else {
				value, err = c.compileExpr(s.Value)
			}
			if err != nil {
				return err
			}
		}
	}
	if s.Value == nil && typ != nil {
		value = zeroValue(typ)
	}
	if c.env != nil {
		if s.Value != nil {
			if q := s.Value.Binary.Left.Value.Target.Query; q != nil {
				if st, ok := c.queryStructs[q]; ok {
					typ = types.ListType{Elem: st}
				}
			}
		}
		t := typ
		if t == nil {
			if s.Value != nil {
				t = c.inferExprType(s.Value)
			} else {
				t = types.AnyType{}
			}
		}
		t = c.namedType(t)
		c.env.SetVar(s.Name, t, false)
		typ = t
	}
	explicit := s.Type != nil
	useAnn := c.typeHints && typ != nil && !isAny(typ)
	if explicit {
		typStr := pyType(c.namedType(c.resolveTypeRef(s.Type)))
		if needsTyping(typStr) {
			c.imports["typing"] = "typing"
		}
		c.writeln(fmt.Sprintf("%s: %s = %s", name, typStr, value))
	} else if useAnn {
		typStr := pyType(c.namedType(typ))
		if needsTyping(typStr) {
			c.imports["typing"] = "typing"
		}
		c.writeln(fmt.Sprintf("%s: %s = %s", name, typStr, value))
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
	value := "UNDEFINED"
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
		if c.autoStructsEnabled {
			if ll := s.Value.Binary.Left.Value.Target.List; ll != nil {
				if val, st, ok, err := c.listAsStruct(ll, s.Name); ok && err == nil {
					value = val
					typ = types.ListType{Elem: st}
				} else if err != nil {
					return err
				}
			}
		}
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := typ.(types.MapType); ok {
				typStr := fmt.Sprintf("typing.cast(dict[%s, %s], {})", pyType(c.namedType(mt.Key)), pyType(c.namedType(mt.Value)))
				if c.typeHints {
					if needsTyping(typStr) {
						c.imports["typing"] = "typing"
					}
					value = typStr
				} else {
					value = "{}"
				}
			}
		}
		if value == "UNDEFINED" {
			var err error
			if typ != nil && !isAny(typ) {
				value, err = c.compileExprHint(s.Value, typ)
			} else {
				value, err = c.compileExpr(s.Value)
			}
			if err != nil {
				return err
			}
		}
	} else if typ != nil {
		value = zeroValue(typ)
	}
	if c.env != nil {
		if s.Value != nil {
			if q := s.Value.Binary.Left.Value.Target.Query; q != nil {
				if st, ok := c.queryStructs[q]; ok {
					typ = types.ListType{Elem: st}
				}
			}
		}
		t := typ
		if t == nil {
			if s.Value != nil {
				t = c.inferExprType(s.Value)
			} else {
				t = types.AnyType{}
			}
		}
		t = c.namedType(t)
		c.env.SetVar(s.Name, t, true)
		typ = t
	}
	if s.Type != nil {
		typStr := pyType(c.namedType(c.resolveTypeRef(s.Type)))
		if c.typeHints && needsTyping(typStr) {
			c.imports["typing"] = "typing"
		}
		if c.typeHints {
			c.writeln(fmt.Sprintf("%s: %s = %s", name, typStr, value))
		} else {
			c.writeln(fmt.Sprintf("%s = %s", name, value))
		}
	} else if c.typeHints && typ != nil && !isAny(typ) {
		typStr := pyType(c.namedType(typ))
		if needsTyping(typStr) {
			c.imports["typing"] = "typing"
		}
		c.writeln(fmt.Sprintf("%s: %s = %s", name, typStr, value))
	} else {
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	}
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
	for i, fld := range s.Field {
		rest := make([]string, len(s.Field[i:]))
		for j, f := range s.Field[i:] {
			rest[j] = f.Name
		}
		switch t := typ.(type) {
		case types.StructType:
			if ft, ok := t.Fields[fld.Name]; ok {
				lhs = fmt.Sprintf("%s.%s", lhs, sanitizeName(fld.Name))
				typ = ft
				continue
			}
		case types.UnionType:
			if ft, ok := unionFieldPathType(t, rest); ok {
				lhs = fmt.Sprintf("%s.%s", lhs, sanitizeName(fld.Name))
				typ = ft
				continue
			}
		}
		lhs = fmt.Sprintf("getattr(%s, %q)", lhs, sanitizeName(fld.Name))
		typ = types.AnyType{}
	}
	value := ""
	if len(s.Index) == 0 {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := typ.(types.MapType); ok {
				typStr := fmt.Sprintf("typing.cast(dict[%s, %s], {})", pyType(c.namedType(mt.Key)), pyType(c.namedType(mt.Value)))
				if c.typeHints {
					if needsTyping(typStr) {
						c.imports["typing"] = "typing"
					}
					value = typStr
				} else {
					value = "{}"
				}
			}
		}
	}
	if value == "" {
		var err error
		if typ != nil && !isAny(typ) {
			value, err = c.compileExprHint(s.Value, typ)
		} else {
			value, err = c.compileExpr(s.Value)
		}
		if err != nil {
			return err
		}
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
	needTyping := false
	c.imports["dataclasses"] = "dataclasses"
	c.writeln("@dataclasses.dataclass")
	c.writeln(fmt.Sprintf("class %s:", name))
	c.indent++
	hasField := len(st.Order) > 0
	if !hasField && len(st.Methods) == 0 {
		c.writeln("pass")
	} else {
		for _, fn := range st.Order {
			typStr := pyType(c.namedType(st.Fields[fn]))
			if needsTyping(typStr) {
				needTyping = true
			}
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
		// no extra magic methods needed for plain structs
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
	if needTyping {
		c.imports["typing"] = "typing"
	}
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	c.imports["dataclasses"] = "dataclasses"
	needTyping := false
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
					typStr := pyType(c.namedType(c.resolveTypeRef(f.Type)))
					if needsTyping(typStr) {
						needTyping = true
					}
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
					typStr := pyType(c.namedType(c.resolveTypeRef(m.Field.Type)))
					if needsTyping(typStr) {
						needTyping = true
					}
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
			if hasField {
				c.writeln("")
				c.writeln("def __contains__(self, key):")
				c.indent++
				c.writeln("return hasattr(self, key)")
				c.indent--
			}
		}
		c.indent--
	}
	if needTyping {
		c.imports["typing"] = "typing"
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
	needTyping := false
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
			if c.typeHints {
				typStr := pyType(c.namedType(typ))
				if needsTyping(typStr) {
					needTyping = true
				}
				c.buf.WriteString(": " + typStr)
			}
		} else {
			typ = types.AnyType{}
		}
		paramTypes[i] = typ
	}
	var retT types.Type = types.VoidType{}
	if ft.Return != nil {
		retT = ft.Return
	} else if fun.Return != nil {
		retT = c.resolveTypeRef(fun.Return)
	} else if c.env != nil {
		retT = c.inferFunReturnType(fun.Body)
	}
	retType := pyType(c.namedType(retT))
	if c.typeHints && needsTyping(retType) {
		needTyping = true
	}
	if c.typeHints {
		c.buf.WriteString(") -> " + retType + ":\n")
	} else {
		c.buf.WriteString("):\n")
	}
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
	var globals []string
	var nonlocals []string
	if c.env != nil {
		root := c.env == c.rootEnv
		for name := range assigns {
			if !locals[name] {
				if _, err := c.env.GetVar(name); err == nil {
					if root {
						globals = append(globals, sanitizeName(name))
					} else if _, ok := c.env.Types()[name]; ok {
						nonlocals = append(nonlocals, sanitizeName(name))
					} else {
						globals = append(globals, sanitizeName(name))
					}
				}
			}
		}
	}
	origEnv := c.env
	c.env = child
	c.indent++
	if c.typeHints {
		paramDocs := make([]string, len(fun.Params))
		for i, p := range fun.Params {
			desc := sanitizeName(p.Name)
			if paramTypes[i] != nil {
				typStr := pyType(c.namedType(paramTypes[i]))
				desc += ": " + typStr
			}
			paramDocs[i] = desc
		}
		sigDoc := fmt.Sprintf("%s(%s)", name, strings.Join(paramDocs, ", "))
		if retType != "None" {
			sigDoc += " -> " + retType
		}
		c.writeln(fmt.Sprintf("\"\"\"%s\"\"\"", sigDoc))
	}
	for _, n := range globals {
		c.writeln("global " + n)
	}
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
	if needTyping && c.typeHints {
		c.imports["typing"] = "typing"
	}
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
	needTyping := false
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
			if c.typeHints {
				typStr := pyType(c.namedType(typ))
				if needsTyping(typStr) {
					needTyping = true
				}
				c.buf.WriteString(": " + typStr)
			}
		} else {
			typ = types.AnyType{}
		}
		paramTypes[i] = typ
	}
	var retT types.Type = types.VoidType{}
	if ft.Return != nil {
		retT = ft.Return
	} else if fun.Return != nil {
		retT = c.resolveTypeRef(fun.Return)
	} else if c.env != nil {
		retT = c.inferFunReturnType(fun.Body)
	}
	retType := pyType(c.namedType(retT))
	if c.typeHints && needsTyping(retType) {
		needTyping = true
	}
	if c.typeHints {
		c.buf.WriteString(") -> " + retType + ":\n")
	} else {
		c.buf.WriteString("):\n")
	}
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
	c.writeln(fmt.Sprintf("\"\"\"%s.%s\"\"\"", structName, name))
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
	if needTyping && c.typeHints {
		c.imports["typing"] = "typing"
	}
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
