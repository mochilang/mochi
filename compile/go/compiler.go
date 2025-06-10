package gocode

import (
	"bytes"
	"fmt"
	"reflect"
	"sort"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Go source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	imports      map[string]bool
	env          *types.Env
	helpers      map[string]bool
	structs      map[string]bool
	handlerCount int
	handlerDones []string
	streams      []string
	usesHandlers bool
	memo         map[string]*parser.Literal
}

// New creates a new Go compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		imports:      make(map[string]bool),
		env:          env,
		helpers:      make(map[string]bool),
		structs:      make(map[string]bool),
		handlerDones: []string{},
		memo:         map[string]*parser.Literal{},
	}
}

// Compile returns Go source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("package main")
	c.writeln("")

	// Top level statements may require imports.
	for _, stmt := range prog.Statements {
		c.scanImports(stmt)
	}

	if len(c.imports) > 0 {
		c.writeln("import (")
		c.indent++
		keys := make([]string, 0, len(c.imports))
		for k := range c.imports {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, imp := range keys {
			if strings.Contains(imp, " ") {
				c.writeln(imp)
			} else {
				c.writeln(fmt.Sprintf("\"%s\"", imp))
			}
		}
		c.indent--
		c.writeln(")")
		c.writeln("")
	}

	if hasExpect(prog) {
		c.writeln("func expect(cond bool) {")
		c.indent++
		c.writeln("if !cond { panic(\"expect failed\") }")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	// Emit type declarations.
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit function declarations first.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit test functions.
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("func main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s()", name))
		}
	}
	if c.usesHandlers {
		for _, dn := range c.handlerDones {
			c.writeln(fmt.Sprintf("<-%s", dn))
		}
		for _, sn := range c.streams {
			c.writeln(fmt.Sprintf("%s.Close()", sn))
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.emitRuntime()

	return c.buf.Bytes(), nil
}

// --- Statement Compilation ---

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
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Agent != nil:
		return c.compileAgentDecl(s.Agent)
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.writeln("expect(" + expr + ")")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	value := "nil"
	var lit *parser.Literal
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
		if l, ok := c.evalConstExpr(s.Value); ok {
			lit = l
		}
	}
	typStr := "any"
	if c.env != nil {
		t, err := c.env.GetVar(s.Name)
		if err != nil {
			if s.Value != nil {
				t = c.inferExprType(s.Value)
			} else {
				t = types.AnyType{}
			}
			c.env.SetVar(s.Name, t, false)
		}
		typStr = goType(t)
	}
	c.writeln(fmt.Sprintf("var %s %s = %s", name, typStr, value))
	if lit != nil && c.env != nil {
		c.env.SetValue(s.Name, literalValue(lit), false)
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	value := "nil"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	typStr := "any"
	if c.env != nil {
		t, err := c.env.GetVar(s.Name)
		if err != nil {
			if s.Value != nil {
				t = c.inferExprType(s.Value)
			} else {
				t = types.AnyType{}
			}
			c.env.SetVar(s.Name, t, true)
		}
		typStr = goType(t)
	}
	c.writeln(fmt.Sprintf("var %s %s = %s", name, typStr, value))
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	name := sanitizeName(s.Name)
	value, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	return nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	c.compileStructType(st)
	varName := unexportName(sanitizeName(s.Name)) + "Stream"
	c.imports["mochi/runtime/stream"] = true
	c.writeln(fmt.Sprintf("var %s = stream.New(%q, 64)", varName, s.Name))
	c.streams = append(c.streams, varName)
	return nil
}

func (c *Compiler) compileOnHandler(h *parser.OnHandler) error {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", h.Stream)
	}
	c.usesHandlers = true
	streamVar := unexportName(sanitizeName(h.Stream)) + "Stream"
	handlerID := c.handlerCount
	c.handlerCount++
	doneVar := fmt.Sprintf("_done%d", handlerID)
	c.writeln(fmt.Sprintf("%s := make(chan struct{})", doneVar))
	c.writeln(fmt.Sprintf("%s.Subscribe(\"handler-%d\", func(ev *stream.Event) error {", streamVar, handlerID))
	c.indent++
	alias := sanitizeName(h.Alias)
	c.writeln(fmt.Sprintf("%s := ev.Data.(%s)", alias, sanitizeName(st.Name)))
	child := types.NewEnv(c.env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return err
		}
	}
	c.env = orig
	c.writeln(fmt.Sprintf("close(%s)", doneVar))
	c.writeln("return nil")
	c.indent--
	c.writeln("})")
	c.handlerDones = append(c.handlerDones, doneVar)
	return nil
}

func (c *Compiler) compileEmit(e *parser.EmitStmt) error {
	st, ok := c.env.GetStream(e.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", e.Stream)
	}
	parts := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%s: %s", exportName(sanitizeName(f.Name)), v)
	}
	lit := fmt.Sprintf("%s{%s}", sanitizeName(st.Name), strings.Join(parts, ", "))
	streamVar := unexportName(sanitizeName(e.Stream)) + "Stream"
	c.writeln(fmt.Sprintf("_, _ = %s.Emit(context.Background(), %s)", streamVar, lit))
	return nil
}

func (c *Compiler) compileAgentDecl(a *parser.AgentDecl) error {
	st, ok := c.env.GetStruct(a.Name)
	if !ok {
		return fmt.Errorf("unknown agent: %s", a.Name)
	}

	name := sanitizeName(a.Name)
	if !c.structs[name] {
		c.structs[name] = true
		c.writeln(fmt.Sprintf("type %s struct {", name))
		c.indent++
		c.writeln("*agent.Agent")
		for _, fn := range st.Order {
			ft := st.Fields[fn]
			c.writeln(fmt.Sprintf("%s %s", exportName(sanitizeName(fn)), goType(ft)))
		}
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	baseEnv := types.NewEnv(c.env)
	for _, fn := range st.Order {
		baseEnv.SetVar(fn, st.Fields[fn], true)
	}

	onHandlers := []struct{ name, stream string }{}
	handlerID := 0

	for _, blk := range a.Body {
		switch {
		case blk.Intent != nil:
			if _, err := c.compileAgentIntent(name, baseEnv, blk.Intent); err != nil {
				return err
			}
		case blk.On != nil:
			hname, err := c.compileAgentOn(name, baseEnv, blk.On, handlerID)
			if err != nil {
				return err
			}
			onHandlers = append(onHandlers, struct{ name, stream string }{hname, blk.On.Stream})
			handlerID++
		case blk.Let != nil:
			baseEnv.SetVar(blk.Let.Name, st.Fields[blk.Let.Name], false)
		case blk.Var != nil:
			baseEnv.SetVar(blk.Var.Name, st.Fields[blk.Var.Name], true)
		}
	}

	// constructor
	c.writeln(fmt.Sprintf("func New%s() *%s {", name, name))
	c.indent++
	c.writeln(fmt.Sprintf("inst := &%s{Agent: agent.New(agent.Config{Name: %q, BufSize: 16})}", name, a.Name))

	origEnv := c.env
	c.env = baseEnv
	for _, blk := range a.Body {
		switch {
		case blk.Let != nil:
			val := "nil"
			if blk.Let.Value != nil {
				v, err := c.compileExpr(blk.Let.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("inst.%s = %s", exportName(sanitizeName(blk.Let.Name)), val))
		case blk.Var != nil:
			val := "nil"
			if blk.Var.Value != nil {
				v, err := c.compileExpr(blk.Var.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("inst.%s = %s", exportName(sanitizeName(blk.Var.Name)), val))
		}
	}
	c.env = origEnv

	for _, h := range onHandlers {
		streamVar := unexportName(sanitizeName(h.stream)) + "Stream"
		c.writeln(fmt.Sprintf("inst.Agent.On(%s, inst.%s)", streamVar, h.name))
	}
	for _, blk := range a.Body {
		if blk.Intent != nil {
			c.writeln(fmt.Sprintf("inst.Agent.RegisterIntent(%q, func(ctx context.Context, args ...agent.Value) (agent.Value, error) {", blk.Intent.Name))
			c.indent++
			paramNames := make([]string, len(blk.Intent.Params))
			for i, p := range blk.Intent.Params {
				typ := goType(st.Methods[blk.Intent.Name].Type.Params[i])
				pname := sanitizeName(p.Name)
				c.writeln(fmt.Sprintf("var %s %s", pname, typ))
				c.writeln(fmt.Sprintf("if len(args) > %d { %s = args[%d].(%s) }", i, pname, i, typ))
				paramNames[i] = pname
			}
			call := fmt.Sprintf("inst.%s(%s)", sanitizeName(blk.Intent.Name), strings.Join(paramNames, ", "))
			if rt := goType(st.Methods[blk.Intent.Name].Type.Return); rt != "" {
				c.writeln(fmt.Sprintf("res := %s", call))
				c.writeln("return res, nil")
			} else {
				c.writeln(call)
				c.writeln("return nil, nil")
			}
			c.indent--
			c.writeln("})")
		}
	}

	c.writeln("inst.Agent.Start(context.Background())")
	c.writeln("return inst")
	c.indent--
	c.writeln("}")
	c.writeln("")

	return nil
}

func (c *Compiler) compileAgentIntent(agentName string, env *types.Env, in *parser.IntentDecl) (string, error) {
	st, _ := c.env.GetStruct(agentName)
	ft := st.Methods[in.Name].Type
	name := sanitizeName(in.Name)
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("func (a *%s) %s(", agentName, name))
	for i, p := range in.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		paramType := "any"
		if i < len(ft.Params) {
			paramType = goType(ft.Params[i])
		}
		c.buf.WriteString(sanitizeName(p.Name) + " " + paramType)
	}
	retType := goType(ft.Return)
	if retType == "" {
		c.buf.WriteString(") {\n")
	} else {
		c.buf.WriteString(") " + retType + " {\n")
	}
	child := types.NewEnv(env)
	for i, p := range in.Params {
		if i < len(ft.Params) {
			child.SetVar(p.Name, ft.Params[i], true)
		}
	}
	orig := c.env
	c.env = child
	c.indent++
	for _, s := range in.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = orig
			return "", err
		}
	}
	c.indent--
	c.env = orig
	c.writeIndent()
	c.buf.WriteString("}\n\n")
	return name, nil
}

func (c *Compiler) compileAgentOn(agentName string, env *types.Env, h *parser.OnHandler, id int) (string, error) {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return "", fmt.Errorf("unknown stream: %s", h.Stream)
	}
	fname := fmt.Sprintf("_on%d", id)
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("func (a *%s) %s(ctx context.Context, ev *stream.Event) {\n", agentName, fname))
	alias := sanitizeName(h.Alias)
	c.indent++
	c.writeln(fmt.Sprintf("%s := ev.Data.(%s)", alias, sanitizeName(st.Name)))
	child := types.NewEnv(env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig
	c.indent--
	c.writeln("}")
	c.writeln("")
	return fname, nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("type %s struct {", name))
	c.indent++
	for _, fn := range st.Order {
		ft := st.Fields[fn]
		c.writeln(fmt.Sprintf("%s %s `json:\"%s\"`", exportName(sanitizeName(fn)), goType(ft), fn))
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func unexportName(name string) string {
	if name == "" {
		return ""
	}
	runes := []rune(name)
	if runes[0] >= 'A' && runes[0] <= 'Z' {
		runes[0] = runes[0] - 'A' + 'a'
	}
	return string(runes)
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		iface := fmt.Sprintf("type %s interface { is%s() }", name, name)
		c.writeln(iface)
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			c.writeln(fmt.Sprintf("type %s struct {", vname))
			c.indent++
			for _, f := range v.Fields {
				fieldName := exportName(sanitizeName(f.Name))
				typ := goType(resolveTypeRef(f.Type))
				c.writeln(fmt.Sprintf("%s %s `json:\"%s\"`", fieldName, typ, f.Name))
			}
			c.indent--
			c.writeln("}")
			c.writeln(fmt.Sprintf("func (%s) is%s() {}", vname, name))
		}
		return nil
	}
	c.writeln(fmt.Sprintf("type %s struct {", name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			fieldName := exportName(sanitizeName(m.Field.Name))
			typ := goType(resolveTypeRef(m.Field.Type))
			c.writeln(fmt.Sprintf("%s %s `json:\"%s\"`", fieldName, typ, m.Field.Name))
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
	c.writeIndent()
	c.buf.WriteString("if " + cond + " {")
	c.buf.WriteByte('\n')
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if stmt.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.buf.WriteString(" else {")
		c.buf.WriteByte('\n')
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString("for " + cond + " {\n")
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
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
		c.buf.WriteString(fmt.Sprintf("for %s := %s; %s < %s; %s++ {\n", name, start, name, end, name))
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.IntType{}, true)
		}
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}\n")
		return nil
	}

	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	t := c.inferExprType(stmt.Source)
	preBody := ""
	switch tt := t.(type) {
	case types.ListType:
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for _, %s := range %s {\n", name, src))
		if c.env != nil {
			c.env.SetVar(stmt.Name, tt.Elem, true)
		}
	case types.StringType:
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for _, r := range []rune(%s) {\n", src))
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.StringType{}, true)
		}
		preBody = fmt.Sprintf("%s := string(r)\n", name)
	default:
		c.use("_iter")
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for _, %s := range _iter(%s) {\n", name, src))
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.AnyType{}, true)
		}
	}
	c.indent++
	if preBody != "" {
		c.writeIndent()
		c.buf.WriteString(preBody)
	}
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	if c.env != nil {
		c.env.SetFunc(fun.Name, fun)
	}
	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	c.writeIndent()
	c.buf.WriteString("func " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		paramType := "any"
		if i < len(ft.Params) {
			paramType = goType(ft.Params[i])
		}
		c.buf.WriteString(sanitizeName(p.Name) + " " + paramType)
	}
	retType := goType(ft.Return)
	if retType == "" {
		c.buf.WriteString(") {\n")
	} else {
		c.buf.WriteString(") " + retType + " {\n")
	}
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		if i < len(ft.Params) {
			child.SetVar(p.Name, ft.Params[i], true)
		}
	}
	originalEnv := c.env
	c.env = child
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = originalEnv
			return err
		}
	}
	c.indent--
	c.env = originalEnv
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("func " + name + "() {\n")
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

// --- Expression Compilation ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	leftType := c.inferUnaryType(b.Left)
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightType := c.inferPostfixType(op.Right)
		var next types.Type
		switch op.Op {
		case "+", "-", "*", "/", "%":
			if _, ok := leftType.(types.AnyType); ok || isAny(rightType) {
				return "", fmt.Errorf("operator %q cannot be used on types %s and %s", op.Op, leftType, rightType)
			}
			switch {
			case (isInt64(leftType) && (isInt64(rightType) || isInt(rightType))) ||
				(isInt64(rightType) && (isInt64(leftType) || isInt(leftType))):
				expr = fmt.Sprintf("(int64(%s) %s int64(%s))", expr, op.Op, right)
				next = types.Int64Type{}
			case isInt(leftType) && isInt(rightType):
				expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, right)
				next = types.IntType{}
			case isFloat(leftType) && isFloat(rightType):
				expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, right)
				next = types.FloatType{}
			case op.Op == "+" && isString(leftType) && isString(rightType):
				expr = fmt.Sprintf("%s + %s", expr, right)
				next = types.StringType{}
			default:
				return "", fmt.Errorf("operator %q cannot be used on types %s and %s", op.Op, leftType, rightType)
			}
		case "==", "!=", "<", "<=", ">", ">=":
			if _, ok := leftType.(types.AnyType); ok || isAny(rightType) {
				return "", fmt.Errorf("incompatible types in comparison: %s and %s", leftType, rightType)
			}
			expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, right)
			next = types.BoolType{}
		default:
			return "", fmt.Errorf("unsupported operator: %s", op.Op)
		}
		leftType = next
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" || op == "!" {
			val = fmt.Sprintf("%s%s", op, val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	typ := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			typ = c.inferPostfixType(&parser.PostfixExpr{Target: &parser.Primary{Call: nil}})
		case op.Index != nil:
			idx := op.Index
			if idx.Colon == nil {
				key, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				switch tt := typ.(type) {
				case types.ListType:
					val = fmt.Sprintf("%s[%s]", val, key)
					typ = tt.Elem
				case types.MapType:
					val = fmt.Sprintf("%s[%s]", val, key)
					typ = tt.Value
				case types.StringType:
					c.use("_indexString")
					val = fmt.Sprintf("_indexString(%s, %s)", val, key)
					typ = types.StringType{}
				default:
					c.use("_index")
					val = fmt.Sprintf("_index(%s, %s)", val, key)
					typ = types.AnyType{}
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
				end := fmt.Sprintf("len(%s)", val)
				if idx.End != nil {
					e, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				switch typ.(type) {
				case types.ListType:
					if idx.End == nil {
						end = fmt.Sprintf("len(%s)", val)
					}
					val = fmt.Sprintf("%s[%s:%s]", val, start, end)
				case types.StringType:
					if idx.End == nil {
						end = fmt.Sprintf("len([]rune(%s))", val)
					}
					val = fmt.Sprintf("string([]rune(%s)[%s:%s])", val, start, end)
				default:
					if idx.End == nil {
						end = fmt.Sprintf("len(%s)", val)
					}
					val = fmt.Sprintf("%s[%s:%s]", val, start, end)
				}
			}
		case op.Cast != nil:
			t := resolveTypeRef(op.Cast.Type)
			c.use("_cast")
			c.imports["encoding/json"] = true
			val = fmt.Sprintf("_cast[%s](%s)", goType(t), val)
			typ = t
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Selector != nil:
		base := sanitizeName(p.Selector.Root)
		for _, field := range p.Selector.Tail {
			base += "." + exportName(sanitizeName(field))
		}
		return base, nil
	case p.Struct != nil:
		if _, ok := c.env.GetAgent(p.Struct.Name); ok {
			if len(p.Struct.Fields) > 0 {
				return "", fmt.Errorf("agent initialization with fields not supported")
			}
			return fmt.Sprintf("New%s()", sanitizeName(p.Struct.Name)), nil
		}
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", exportName(sanitizeName(f.Name)), v)
		}
		return fmt.Sprintf("%s{%s}", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		typ := c.inferPrimaryType(p)
		elemType := "any"
		if lt, ok := typ.(types.ListType); ok {
			elemType = goType(lt.Elem)
		}
		return "[]" + elemType + "{" + strings.Join(elems, ", ") + "}", nil
	case p.Map != nil:
		typ := c.inferPrimaryType(p)
		keyType := "string"
		valType := "any"
		if mt, ok := typ.(types.MapType); ok {
			keyType = goType(mt.Key)
			valType = goType(mt.Value)
		}

		parts := make([]string, len(p.Map.Items))
		for i, item := range p.Map.Items {
			k, err := c.compileExpr(item.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(item.Value)
			if err != nil {
				return "", err
			}
			// If the map value type is int64 but the expression is int,
			// cast to int64 to satisfy the Go compiler.
			if valType == "int64" && goType(c.inferExprType(item.Value)) == "int" {
				v = fmt.Sprintf("int64(%s)", v)
			}
			parts[i] = fmt.Sprintf("%s: %s", k, v)
		}

		return fmt.Sprintf("map[%s]%s{%s}", keyType, valType, strings.Join(parts, ", ")), nil

	case p.Query != nil:
		return c.compileQueryExpr(p.Query)

	case p.Match != nil:
		return c.compileMatchExpr(p.Match)

	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)

	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	default:
		return "nil", fmt.Errorf("unsupported primary expression")
	}
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}

	var withStr string
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		withStr = fmt.Sprintf("_toAnyMap(%s)", w)
	} else {
		withStr = "nil"
	}

	c.imports["net/http"] = true
	c.imports["io"] = true
	c.imports["encoding/json"] = true
	c.imports["bytes"] = true
	c.imports["net/url"] = true
	c.imports["time"] = true
	c.imports["fmt"] = true
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, withStr), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	switch g.Target {
	case "embedding":
		var text string
		for _, f := range g.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			if f.Name == "text" {
				text = v
			}
		}
		if text == "" {
			text = "\"\""
		}
		c.use("_genEmbed")
		c.imports["context"] = true
		c.imports["mochi/runtime/llm"] = true
		return fmt.Sprintf("_genEmbed(%s)", text), nil
	default:
		var prompt string
		for _, f := range g.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			if f.Name == "prompt" {
				prompt = v
			}
		}
		if prompt == "" {
			prompt = "\"\""
		}
		c.imports["context"] = true
		c.imports["mochi/runtime/llm"] = true
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_genStruct")
			c.imports["encoding/json"] = true
			return fmt.Sprintf("_genStruct[%s](%s)", sanitizeName(g.Target), prompt), nil
		}
		c.use("_genText")
		return fmt.Sprintf("_genText(%s)", prompt), nil
	}
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	needsHelper := false
	for _, j := range q.Joins {
		if j.Side != nil {
			needsHelper = true
			break
		}
	}

	// Prepare environment for the query variable
	srcType := c.inferExprType(q.Source)
	var elemType types.Type = types.AnyType{}
	directRange := false
	if lt, ok := srcType.(types.ListType); ok {
		elemType = lt.Elem
		directRange = true
	}
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, elemType, true)
	// Add cross join variables to environment
	for _, f := range q.Froms {
		ft := c.inferExprType(f.Src)
		var felem types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			felem = lt.Elem
		}
		child.SetVar(f.Var, felem, true)
	}
	// Add join variables to environment
	for _, j := range q.Joins {
		jt := c.inferExprType(j.Src)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
	}
	original := c.env
	c.env = child

	// compile cross join sources
	fromSrcs := make([]string, len(q.Froms))
	fromDirect := make([]bool, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = original
			return "", err
		}
		fromSrcs[i] = fs
		if _, ok := c.inferExprType(f.Src).(types.ListType); ok {
			fromDirect[i] = true
		}
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = original
		return "", err
	}
	var cond string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = original
			return "", err
		}
	}
	var sortExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = original
			return "", err
		}
	}
	var skipExpr string
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = original
			return "", err
		}
	}
	var takeExpr string
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = original
			return "", err
		}
	}

	// compile join sources and conditions while join vars are in scope
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinDirect := make([]bool, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	joinTypes := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = original
			return "", err
		}
		joinSrcs[i] = js
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			joinDirect[i] = true
			je = lt.Elem
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = original
			return "", err
		}
		joinOns[i] = on
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
		t := goType(je)
		if t == "" {
			t = "any"
		}
		joinTypes[i] = t
	}
	retElem := goType(c.inferExprType(q.Select))
	if retElem == "" {
		retElem = "any"
	}

	c.env = original
	if needsHelper {
		varNames := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			varNames = append(varNames, sanitizeName(f.Var))
		}
		params := append([]string(nil), varNames...)
		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{items: _iter(%s)}", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(params, sanitizeName(q.Joins[i].Var))
			onFn := fmt.Sprintf("func(%s) bool { return %s }", strings.Join(onParams, ", "), joinOns[i])
			spec := fmt.Sprintf("{items: _iter(%s), on: %s", js, onFn)
			if joinSides[i] == "left" || joinSides[i] == "outer" {
				spec += ", left: true"
			}
			if joinSides[i] == "right" || joinSides[i] == "outer" {
				spec += ", right: true"
			}
			spec += "}"
			joins = append(joins, spec)
			params = append(params, sanitizeName(q.Joins[i].Var))
		}
		allParams := strings.Join(params, ", ")
		selectFn := fmt.Sprintf("func(%s) any { return %s }", allParams, sel)
		var whereFn, sortFn string
		if cond != "" {
			whereFn = fmt.Sprintf("func(%s) bool { return %s }", allParams, cond)
		}
		if sortExpr != "" {
			sortFn = fmt.Sprintf("func(%s) any { return %s }", allParams, sortExpr)
		}

		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("func() []%s {\n", retElem))
		c.use("_iter")
		c.use("_query")
		buf.WriteString(fmt.Sprintf("\tsrc := _iter(%s)\n", src))
		buf.WriteString("\tresAny := _query(src, []_joinSpec{\n")
		for _, j := range joins {
			buf.WriteString("\t\t" + j + ",\n")
		}
		buf.WriteString("\t}, _queryOpts{selectFn: " + selectFn)
		if whereFn != "" {
			buf.WriteString(", where: " + whereFn)
		}
		if sortFn != "" {
			buf.WriteString(", sortKey: " + sortFn)
		}
		if skipExpr != "" {
			buf.WriteString(", skip: " + skipExpr)
		} else {
			buf.WriteString(", skip: -1")
		}
		if takeExpr != "" {
			buf.WriteString(", take: " + takeExpr)
		} else {
			buf.WriteString(", take: -1")
		}
		buf.WriteString("})\n")
		buf.WriteString(fmt.Sprintf("\tout := make([]%s, len(resAny))\n", retElem))
		buf.WriteString("\tfor i, v := range resAny {\n")
		if retElem == "any" {
			buf.WriteString("\t\tout[i] = v\n")
		} else {
			c.use("_cast")
			buf.WriteString(fmt.Sprintf("\t\tout[i] = _cast[%s](v)\n", retElem))
		}
		buf.WriteString("\t}\n")
		buf.WriteString("\treturn out\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}
	if !directRange {
		c.use("_iter")
	}

	simple := q.Sort == nil && q.Skip == nil && q.Take == nil

	elemGo := goType(elemType)
	if elemGo == "" {
		elemGo = "any"
	}

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("func() []%s {\n", retElem))
	if simple {
		buf.WriteString(fmt.Sprintf("\tres := []%s{}\n", retElem))
	} else {
		buf.WriteString(fmt.Sprintf("\titems := []%s{}\n", elemGo))
	}
	indent := "\t"
	if directRange {
		buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", sanitizeName(q.Var), src))
	} else {
		buf.WriteString(fmt.Sprintf(indent+"for _, %s := range _iter(%s) {\n", sanitizeName(q.Var), src))
	}
	indent += "\t"

	for i := range q.Froms {
		fvar := sanitizeName(q.Froms[i].Var)
		fsrc := fromSrcs[i]
		if fromDirect[i] {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", fvar, fsrc))
		} else {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range _iter(%s) {\n", fvar, fsrc))
		}
		indent += "\t"
	}

	specialJoin := len(q.Joins) == 1 && (joinSides[0] == "left")
	if specialJoin {
		jvar := sanitizeName(q.Joins[0].Var)
		jsrc := joinSrcs[0]
		buf.WriteString(indent + "matched := false\n")
		if joinDirect[0] {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", jvar, jsrc))
		} else {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range _iter(%s) {\n", jvar, jsrc))
		}
		indent += "\t"
		buf.WriteString(fmt.Sprintf(indent+"if !(%s) { continue }\n", joinOns[0]))
		buf.WriteString(indent + "matched = true\n")
	} else {
		for i := range q.Joins {
			jvar := sanitizeName(q.Joins[i].Var)
			jsrc := joinSrcs[i]
			if joinDirect[i] {
				buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", jvar, jsrc))
			} else {
				buf.WriteString(fmt.Sprintf(indent+"for _, %s := range _iter(%s) {\n", jvar, jsrc))
			}
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"if !(%s) { continue }\n", joinOns[i]))
		}
	}

	if specialJoin {
		if cond != "" {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\tres = append(res, %s)\n", sel))
				buf.WriteString(indent + "}\n")
			} else {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\titems = append(items, %s)\n", sanitizeName(q.Var)))
				buf.WriteString(indent + "}\n")
			}
		} else {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"res = append(res, %s)\n", sel))
			} else {
				buf.WriteString(fmt.Sprintf(indent+"items = append(items, %s)\n", sanitizeName(q.Var)))
			}
		}
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
		buf.WriteString(indent + "if !matched {\n")
		indent += "\t"
		buf.WriteString(fmt.Sprintf(indent+"var %s %s\n", sanitizeName(q.Joins[0].Var), joinTypes[0]))
		if cond != "" {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\tres = append(res, %s)\n", sel))
				buf.WriteString(indent + "}\n")
			} else {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\titems = append(items, %s)\n", sanitizeName(q.Var)))
				buf.WriteString(indent + "}\n")
			}
		} else {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"res = append(res, %s)\n", sel))
			} else {
				buf.WriteString(fmt.Sprintf(indent+"items = append(items, %s)\n", sanitizeName(q.Var)))
			}
		}
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	} else {
		if cond != "" {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\tres = append(res, %s)\n", sel))
				buf.WriteString(indent + "}\n")
			} else {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\titems = append(items, %s)\n", sanitizeName(q.Var)))
				buf.WriteString(indent + "}\n")
			}
		} else {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"res = append(res, %s)\n", sel))
			} else {
				buf.WriteString(fmt.Sprintf(indent+"items = append(items, %s)\n", sanitizeName(q.Var)))
			}
		}

		for i := len(q.Joins) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
	}
	for i := len(q.Froms) - 1; i >= 0; i-- {
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	}
	indent = indent[:len(indent)-1]
	buf.WriteString(indent + "}\n")

	if simple {
		buf.WriteString("\treturn res\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}

	if sortExpr != "" {
		buf.WriteString("\ttype pair struct { item " + elemGo + "; key any }\n")
		buf.WriteString("\tpairs := make([]pair, len(items))\n")
		buf.WriteString("\tfor idx, it := range items {\n")
		buf.WriteString(fmt.Sprintf("\t\t%s := it\n", sanitizeName(q.Var)))
		buf.WriteString(fmt.Sprintf("\t\tpairs[idx] = pair{item: it, key: %s}\n", sortExpr))
		buf.WriteString("\t}\n")
		buf.WriteString("\tsort.Slice(pairs, func(i, j int) bool {\n")
		buf.WriteString("\t\ta, b := pairs[i].key, pairs[j].key\n")
		buf.WriteString("\t\tswitch av := a.(type) {\n")
		buf.WriteString("\t\tcase int:\n")
		buf.WriteString("\t\t\tswitch bv := b.(type) {\n")
		buf.WriteString("\t\t\tcase int:\n")
		buf.WriteString("\t\t\t\treturn av < bv\n")
		buf.WriteString("\t\t\tcase float64:\n")
		buf.WriteString("\t\t\t\treturn float64(av) < bv\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\tcase float64:\n")
		buf.WriteString("\t\t\tswitch bv := b.(type) {\n")
		buf.WriteString("\t\t\tcase int:\n")
		buf.WriteString("\t\t\t\treturn av < float64(bv)\n")
		buf.WriteString("\t\t\tcase float64:\n")
		buf.WriteString("\t\t\t\treturn av < bv\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\tcase string:\n")
		buf.WriteString("\t\t\tbs, _ := b.(string)\n")
		buf.WriteString("\t\t\treturn av < bs\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\treturn fmt.Sprint(a) < fmt.Sprint(b)\n")
		buf.WriteString("\t})\n")
		buf.WriteString("\tfor idx, p := range pairs {\n")
		buf.WriteString("\t\titems[idx] = p.item\n")
		buf.WriteString("\t}\n")
	}

	if skipExpr != "" {
		buf.WriteString(fmt.Sprintf("\tskip := %s\n", skipExpr))
		buf.WriteString("\tif skip < len(items) {\n")
		buf.WriteString("\t\titems = items[skip:]\n")
		buf.WriteString("\t} else {\n")
		buf.WriteString(fmt.Sprintf("\t\titems = []%s{}\n", elemGo))
		buf.WriteString("\t}\n")
	}

	if takeExpr != "" {
		buf.WriteString(fmt.Sprintf("\ttake := %s\n", takeExpr))
		buf.WriteString("\tif take < len(items) {\n")
		buf.WriteString("\t\titems = items[:take]\n")
		buf.WriteString("\t}\n")
	}

	buf.WriteString(fmt.Sprintf("\tres := []%s{}\n", retElem))
	buf.WriteString(fmt.Sprintf("\tfor _, %s := range items {\n", sanitizeName(q.Var)))
	buf.WriteString(fmt.Sprintf("\t\tres = append(res, %s)\n", sel))
	buf.WriteString("\t}\n")
	buf.WriteString("\treturn res\n")
	buf.WriteString("}()")
	return buf.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Match: m}}}}}
	retType := goType(c.inferExprType(expr))
	if retType == "" {
		retType = "any"
	}
	var buf bytes.Buffer
	buf.WriteString("func() " + retType + " {\n")
	buf.WriteString("\t_t := " + target + "\n")
	buf.WriteString("\tswitch _t {\n")
	for _, cse := range m.Cases {
		if isUnderscoreExpr(cse.Pattern) {
			buf.WriteString("\tdefault:\n")
		} else {
			pat, err := c.compileExpr(cse.Pattern)
			if err != nil {
				return "", err
			}
			buf.WriteString("\tcase " + pat + ":\n")
		}
		res, err := c.compileExpr(cse.Result)
		if err != nil {
			return "", err
		}
		buf.WriteString("\t\treturn " + res + "\n")
	}
	buf.WriteString("\t}\n")
	if retType == "any" {
		buf.WriteString("\treturn nil\n")
	}
	buf.WriteString("}()")
	return buf.String(), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	default:
		return "nil", fmt.Errorf("invalid literal")
	}
}

func literalKey(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("i%v", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("f%v", *l.Float)
	case l.Str != nil:
		return fmt.Sprintf("s%q", *l.Str)
	case l.Bool != nil:
		if *l.Bool {
			return "btrue"
		}
		return "bfalse"
	default:
		return "nil"
	}
}

func extractLiteral(e *parser.Expr) *parser.Literal {
	if e == nil {
		return nil
	}
	if len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	if p.Target != nil {
		return p.Target.Lit
	}
	return nil
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
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
	return "", false
}

func (c *Compiler) evalConstExpr(e *parser.Expr) (*parser.Literal, bool) {
	if lit := extractLiteral(e); lit != nil {
		return lit, true
	}
	if call, ok := callPattern(e); ok {
		return interpreter.EvalPureCall(call, c.env)
	}
	return nil, false
}

func literalValue(l *parser.Literal) any {
	switch {
	case l.Int != nil:
		return *l.Int
	case l.Float != nil:
		return *l.Float
	case l.Str != nil:
		return *l.Str
	case l.Bool != nil:
		return *l.Bool
	default:
		return nil
	}
}

func (c *Compiler) callKey(call *parser.CallExpr) string {
	parts := make([]string, len(call.Args)+1)
	parts[0] = call.Func
	for i, arg := range call.Args {
		if lit := extractLiteral(arg); lit != nil {
			parts[i+1] = literalKey(lit)
		} else if name, ok := identName(arg); ok {
			if val, err := c.env.GetValue(name); err == nil {
				if l := types.AnyToLiteral(val); l != nil {
					parts[i+1] = literalKey(l)
				}
			}
		}
	}
	return strings.Join(parts, ":")
}

func (c *Compiler) foldCall(call *parser.CallExpr) (*parser.Literal, bool) {
	key := c.callKey(call)
	if lit, ok := c.memo[key]; ok {
		return lit, true
	}
	args := make([]*parser.Expr, len(call.Args))
	for i, a := range call.Args {
		if lit := extractLiteral(a); lit != nil {
			args[i] = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: lit}}}}}
			continue
		}
		if name, ok := identName(a); ok {
			if val, err := c.env.GetValue(name); err == nil {
				if l := types.AnyToLiteral(val); l != nil {
					args[i] = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: l}}}}}
					continue
				}
			}
		}
		return nil, false
	}
	lit, ok := interpreter.EvalPureCall(&parser.CallExpr{Func: call.Func, Args: args}, c.env)
	if ok {
		c.memo[key] = lit
	}
	return lit, ok
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	if lit, ok := c.foldCall(call); ok {
		return c.compileLiteral(lit)
	}
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
		c.imports["fmt"] = true
		return fmt.Sprintf("fmt.Println(%s)", argStr), nil
	case "len":
		return fmt.Sprintf("len(%s)", argStr), nil
	case "now":
		c.imports["time"] = true
		// time.Now().UnixNano() already returns an int64. Use it directly
		// so `now()` provides nanosecond precision consistent with the
		// interpreter.
		return "time.Now().UnixNano()", nil
	case "json":
		c.imports["encoding/json"] = true
		c.imports["fmt"] = true
		return fmt.Sprintf("func(){b,_:=json.Marshal(%s);fmt.Println(string(b))}()", argStr), nil
	default:
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := "any"
		if p.Type != nil {
			typ = goType(resolveTypeRef(p.Type))
		}
		params[i] = sanitizeName(p.Name) + " " + typ
	}
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, resolveTypeRef(p.Type), true)
		}
	}
	sub := &Compiler{imports: c.imports, helpers: c.helpers, env: child}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	retType := "any"
	if fn.Return != nil {
		retType = goType(resolveTypeRef(fn.Return))
	}
	if retType == "" {
		return "func(" + strings.Join(params, ", ") + ") {\n" + body + "}", nil
	}
	return "func(" + strings.Join(params, ", ") + ") " + retType + " {\n" + body + "}", nil
}

// --- Helpers ---

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

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("\t", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else if r > 127 {
			b.WriteRune('_')
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	return b.String()
}

func exportName(name string) string {
	if name == "" {
		return ""
	}
	runes := []rune(name)
	if runes[0] >= 'a' && runes[0] <= 'z' {
		runes[0] = runes[0] - 'a' + 'A'
	}
	return string(runes)
}

func goType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "int"
	case types.Int64Type:
		return "int64"
	case types.FloatType:
		return "float64"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "[]" + goType(tt.Elem)
	case types.MapType:
		return fmt.Sprintf("map[%s]%s", goType(tt.Key), goType(tt.Value))
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = goType(p)
		}
		ret := goType(tt.Return)
		if ret == "" || ret == "void" {
			return fmt.Sprintf("func(%s)", strings.Join(params, ", "))
		}
		return fmt.Sprintf("func(%s) %s", strings.Join(params, ", "), ret)
	case types.VoidType:
		return ""
	case types.AnyType:
		return "any"
	default:
		return "any"
	}
}

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
	}
	if isInt64(a) && (isInt64(b) || isInt(b)) {
		return true
	}
	if isInt64(b) && (isInt64(a) || isInt(a)) {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	return reflect.DeepEqual(a, b)
}

func isInt64(t types.Type) bool {
	_, ok := t.(types.Int64Type)
	return ok
}

func isInt(t types.Type) bool {
	_, ok := t.(types.IntType)
	return ok
}

func isFloat(t types.Type) bool {
	_, ok := t.(types.FloatType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: resolveTypeRef(args[0]), Value: resolveTypeRef(args[1])}
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
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	return c.inferBinaryType(e.Binary)
}

func (c *Compiler) inferBinaryType(b *parser.BinaryExpr) types.Type {
	if b == nil {
		return types.AnyType{}
	}
	t := c.inferUnaryType(b.Left)
	for _, op := range b.Right {
		rt := c.inferPostfixType(op.Right)
		switch op.Op {
		case "+", "-", "*", "/", "%":
			if isInt64(t) {
				if isInt64(rt) || isInt(rt) {
					t = types.Int64Type{}
					continue
				}
			}
			if _, ok := t.(types.IntType); ok {
				if _, ok := rt.(types.IntType); ok {
					t = types.IntType{}
					continue
				}
			}
			if _, ok := t.(types.FloatType); ok {
				if _, ok := rt.(types.FloatType); ok {
					t = types.FloatType{}
					continue
				}
			}
			if op.Op == "+" {
				if _, ok := t.(types.StringType); ok {
					if _, ok := rt.(types.StringType); ok {
						t = types.StringType{}
						continue
					}
				}
			}
			t = types.AnyType{}
		case "==", "!=", "<", "<=", ">", ">=":
			t = types.BoolType{}
		default:
			t = types.AnyType{}
		}
	}
	return t
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	return c.inferPostfixType(u.Value)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	t := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
			switch tt := t.(type) {
			case types.ListType:
				t = tt.Elem
			case types.MapType:
				t = tt.Value
			case types.StringType:
				t = types.StringType{}
			default:
				t = types.AnyType{}
			}
		} else if op.Index != nil {
			switch tt := t.(type) {
			case types.ListType:
				t = tt
			case types.StringType:
				t = types.StringType{}
			default:
				t = types.AnyType{}
			}
		} else if op.Call != nil {
			if ft, ok := t.(types.FuncType); ok {
				t = ft.Return
			} else {
				t = types.AnyType{}
			}
		} else if op.Cast != nil {
			t = resolveTypeRef(op.Cast.Type)
		}
	}
	return t
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return types.IntType{}
		case p.Lit.Float != nil:
			return types.FloatType{}
		case p.Lit.Str != nil:
			return types.StringType{}
		case p.Lit.Bool != nil:
			return types.BoolType{}
		}
	case p.Selector != nil:
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if len(p.Selector.Tail) == 0 {
					return t
				}
				st, ok := t.(types.StructType)
				if ok {
					cur := st
					for idx, field := range p.Selector.Tail {
						ft, ok := cur.Fields[field]
						if !ok {
							return types.AnyType{}
						}
						if idx == len(p.Selector.Tail)-1 {
							return ft
						}
						if next, ok := ft.(types.StructType); ok {
							cur = next
						} else {
							return types.AnyType{}
						}
					}
				}
			}
		}
		return types.AnyType{}
	case p.Struct != nil:
		if c.env != nil {
			if st, ok := c.env.GetStruct(p.Struct.Name); ok {
				return st
			}
		}
		return types.AnyType{}
	case p.Call != nil:
		switch p.Call.Func {
		case "len":
			return types.IntType{}
		case "now":
			return types.Int64Type{}
		default:
			if c.env != nil {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						return ft.Return
					}
				}
			}
			return types.AnyType{}
		}
	case p.Group != nil:
		return c.inferExprType(p.Group)
	case p.List != nil:
		var elemType types.Type = types.AnyType{}
		if len(p.List.Elems) > 0 {
			elemType = c.inferExprType(p.List.Elems[0])
			for _, e := range p.List.Elems[1:] {
				t := c.inferExprType(e)
				if !equalTypes(elemType, t) {
					elemType = types.AnyType{}
					break
				}
			}
		}
		return types.ListType{Elem: elemType}
	case p.Query != nil:
		elem := c.inferExprType(p.Query.Select)
		return types.ListType{Elem: elem}
	case p.Map != nil:
		var keyType types.Type = types.AnyType{}
		var valType types.Type = types.AnyType{}
		if len(p.Map.Items) > 0 {
			keyType = c.inferExprType(p.Map.Items[0].Key)
			valType = c.inferExprType(p.Map.Items[0].Value)
			for _, it := range p.Map.Items[1:] {
				kt := c.inferExprType(it.Key)
				vt := c.inferExprType(it.Value)
				if !equalTypes(keyType, kt) {
					keyType = types.AnyType{}
				}
				if !equalTypes(valType, vt) {
					valType = types.AnyType{}
				}
			}
		}
		return types.MapType{Key: keyType, Value: valType}
	case p.Match != nil:
		var rType types.Type
		for _, cs := range p.Match.Cases {
			t := c.inferExprType(cs.Result)
			if rType == nil {
				rType = t
			} else if !equalTypes(rType, t) {
				rType = types.AnyType{}
			}
		}
		if rType == nil {
			rType = types.AnyType{}
		}
		return rType
	}
	return types.AnyType{}
}

func resultType(op string, left, right types.Type) types.Type {
	switch op {
	case "+", "-", "*", "/", "%":
		if _, ok := left.(types.IntType); ok {
			if _, ok := right.(types.IntType); ok {
				return types.IntType{}
			}
		}
		if _, ok := left.(types.FloatType); ok {
			if _, ok := right.(types.FloatType); ok {
				return types.FloatType{}
			}
		}
		if op == "+" {
			if _, ok := left.(types.StringType); ok {
				if _, ok := right.(types.StringType); ok {
					return types.StringType{}
				}
			}
		}
		return types.AnyType{}
	case "==", "!=", "<", "<=", ">", ">=":
		return types.BoolType{}
	default:
		return types.AnyType{}
	}
}

func hasExpect(p *parser.Program) bool {
	for _, s := range p.Statements {
		if containsExpect(s) {
			return true
		}
	}
	return false
}

func containsExpect(s *parser.Statement) bool {
	switch {
	case s.Expect != nil:
		return true
	case s.If != nil:
		for _, t := range s.If.Then {
			if containsExpect(t) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if containsExpect(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if containsExpect(t) {
				return true
			}
		}
	case s.For != nil:
		for _, t := range s.For.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.While != nil:
		for _, t := range s.While.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if containsExpect(t) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) scanImports(s *parser.Statement) {
	if s.Expr != nil {
		c.scanExprImports(s.Expr.Expr)
	}
	if s.Expect != nil {
		c.scanExprImports(s.Expect.Value)
	}
	if s.Let != nil {
		if s.Let.Value != nil {
			c.scanExprImports(s.Let.Value)
		}
	}
	if s.Var != nil {
		if s.Var.Value != nil {
			c.scanExprImports(s.Var.Value)
		}
	}
	if s.Assign != nil {
		c.scanExprImports(s.Assign.Value)
	}
	if s.If != nil {
		c.scanExprImports(s.If.Cond)
		for _, t := range s.If.Then {
			c.scanImports(t)
		}
		if s.If.ElseIf != nil {
			c.scanImports(&parser.Statement{If: s.If.ElseIf})
		}
		for _, t := range s.If.Else {
			c.scanImports(t)
		}
	}
	if s.For != nil {
		c.scanExprImports(s.For.Source)
		if s.For.RangeEnd != nil {
			c.scanExprImports(s.For.RangeEnd)
		}
		for _, t := range s.For.Body {
			c.scanImports(t)
		}
	}
	if s.While != nil {
		c.scanExprImports(s.While.Cond)
		for _, t := range s.While.Body {
			c.scanImports(t)
		}
	}
	if s.Stream != nil {
		c.imports["mochi/runtime/stream"] = true
	}
	if s.On != nil {
		c.imports["context"] = true
		c.imports["mochi/runtime/stream"] = true
		c.usesHandlers = true
		for _, t := range s.On.Body {
			c.scanImports(t)
		}
	}
	if s.Agent != nil {
		c.imports["context"] = true
		c.imports["mochi/runtime/agent"] = true
		for _, blk := range s.Agent.Body {
			switch {
			case blk.Let != nil && blk.Let.Value != nil:
				c.scanExprImports(blk.Let.Value)
			case blk.Var != nil && blk.Var.Value != nil:
				c.scanExprImports(blk.Var.Value)
			case blk.On != nil:
				c.imports["mochi/runtime/stream"] = true
				for _, t := range blk.On.Body {
					c.scanImports(t)
				}
			case blk.Intent != nil:
				for _, t := range blk.Intent.Body {
					c.scanImports(t)
				}
			}
		}
	}
	if s.Emit != nil {
		c.imports["context"] = true
		c.imports["mochi/runtime/stream"] = true
		for _, f := range s.Emit.Fields {
			c.scanExprImports(f.Value)
		}
	}
	if s.Fun != nil {
		for _, t := range s.Fun.Body {
			c.scanImports(t)
		}
	}
	if s.Test != nil {
		for _, t := range s.Test.Body {
			c.scanImports(t)
		}
	}
}

func (c *Compiler) scanExprImports(e *parser.Expr) {
	if e == nil {
		return
	}
	c.scanBinaryImports(e.Binary)
}

func (c *Compiler) scanBinaryImports(b *parser.BinaryExpr) {
	if b == nil {
		return
	}
	c.scanUnaryImports(b.Left)
	for _, op := range b.Right {
		c.scanPostfixImports(op.Right)
		if op.Op == "==" || op.Op == "!=" || op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=" || op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" || op.Op == "%" {
			// no import needed
		}
	}
}

func (c *Compiler) scanUnaryImports(u *parser.Unary) {
	if u == nil {
		return
	}
	c.scanPostfixImports(u.Value)
}

func (c *Compiler) scanPostfixImports(p *parser.PostfixExpr) {
	if p == nil {
		return
	}
	c.scanPrimaryImports(p.Target)
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			for _, a := range op.Call.Args {
				c.scanExprImports(a)
			}
		case op.Index != nil:
			idx := op.Index
			c.scanExprImports(idx.Start)
			c.scanExprImports(idx.End)
		case op.Cast != nil:
			c.imports["encoding/json"] = true
			c.use("_cast")
		}
	}
}

func (c *Compiler) scanPrimaryImports(p *parser.Primary) {
	if p == nil {
		return
	}
	switch {
	case p.Call != nil:
		if p.Call.Func == "print" {
			c.imports["fmt"] = true
		}
		if p.Call.Func == "now" {
			c.imports["time"] = true
		}
		if p.Call.Func == "json" {
			c.imports["encoding/json"] = true
			c.imports["fmt"] = true
		}
		for _, a := range p.Call.Args {
			c.scanExprImports(a)
		}
	case p.Group != nil:
		c.scanExprImports(p.Group)
	case p.FunExpr != nil:
		if p.FunExpr.ExprBody != nil {
			c.scanExprImports(p.FunExpr.ExprBody)
		}
		for _, s := range p.FunExpr.BlockBody {
			c.scanImports(s)
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			c.scanExprImports(e)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			c.scanExprImports(it.Key)
			c.scanExprImports(it.Value)
		}
	case p.Query != nil:
		c.scanExprImports(p.Query.Source)
		if p.Query.Where != nil {
			c.scanExprImports(p.Query.Where)
		}
		if p.Query.Sort != nil {
			c.scanExprImports(p.Query.Sort)
			c.imports["sort"] = true
			c.imports["fmt"] = true
		}
		if p.Query.Skip != nil {
			c.scanExprImports(p.Query.Skip)
		}
		if p.Query.Take != nil {
			c.scanExprImports(p.Query.Take)
		}
		c.scanExprImports(p.Query.Select)
	case p.Match != nil:
		c.scanExprImports(p.Match.Target)
		for _, cs := range p.Match.Cases {
			c.scanExprImports(cs.Pattern)
			c.scanExprImports(cs.Result)
		}
	case p.Generate != nil:
		c.imports["fmt"] = true
		c.imports["context"] = true
		c.imports["mochi/runtime/llm"] = true
		c.imports["_ \"mochi/runtime/llm/provider/echo\""] = true
		if _, ok := c.env.GetStruct(p.Generate.Target); ok {
			c.imports["encoding/json"] = true
		}
		for _, f := range p.Generate.Fields {
			c.scanExprImports(f.Value)
		}
	case p.Selector != nil:
		// no imports
	case p.Lit != nil:
		// none
	}
}

// Runtime helper functions injected into generated programs.
const (
	helperIndex = "func _index(v any, k any) any {\n" +
		"    switch s := v.(type) {\n" +
		"    case []any:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []int:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []float64:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []string:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []bool:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case string:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid string index\")\n" +
		"        }\n" +
		"        runes := []rune(s)\n" +
		"        if i < 0 {\n" +
		"            i += len(runes)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(runes) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return string(runes[i])\n" +
		"    case map[string]any:\n" +
		"        ks, ok := k.(string)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid map key\")\n" +
		"        }\n" +
		"        return s[ks]\n" +
		"    default:\n" +
		"        panic(\"invalid index target\")\n" +
		"    }\n" +
		"}\n"

	helperIndexString = "func _indexString(s string, i int) string {\n" +
		"    runes := []rune(s)\n" +
		"    if i < 0 {\n" +
		"        i += len(runes)\n" +
		"    }\n" +
		"    if i < 0 || i >= len(runes) {\n" +
		"        panic(\"index out of range\")\n" +
		"    }\n" +
		"    return string(runes[i])\n" +
		"}\n"

	helperIter = "func _iter(v any) []any {\n" +
		"    switch s := v.(type) {\n" +
		"    case []any:\n" +
		"        return s\n" +
		"    case []int:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case []float64:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case []string:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case []bool:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case map[string]any:\n" +
		"        out := make([]any, 0, len(s))\n" +
		"        for k := range s {\n" +
		"            out = append(out, k)\n" +
		"        }\n" +
		"        return out\n" +
		"    case string:\n" +
		"        runes := []rune(s)\n" +
		"        out := make([]any, len(runes))\n" +
		"        for i, r := range runes {\n" +
		"            out[i] = string(r)\n" +
		"        }\n" +
		"        return out\n" +
		"    default:\n" +
		"        return nil\n" +
		"    }\n" +
		"}\n"

	helperGenText = "func _genText(prompt string) string {\n" +
		"    resp, err := llm.Chat(context.Background(), []llm.Message{{Role: \"user\", Content: prompt}})\n" +
		"    if err != nil { panic(err) }\n" +
		"    return resp.Message.Content\n" +
		"}\n"

	helperGenEmbed = "func _genEmbed(text string) []float64 {\n" +
		"    resp, err := llm.Embed(context.Background(), text)\n" +
		"    if err != nil { panic(err) }\n" +
		"    return resp.Vector\n" +
		"}\n"

	helperGenStruct = "func _genStruct[T any](prompt string) T {\n" +
		"    resp, err := llm.Chat(context.Background(), []llm.Message{{Role: \"user\", Content: prompt}})\n" +
		"    if err != nil { panic(err) }\n" +
		"    var out T\n" +
		"    if err := json.Unmarshal([]byte(resp.Message.Content), &out); err != nil { panic(err) }\n" +
		"    return out\n" +
		"}\n"

	helperFetch = "func _fetch(url string, opts map[string]any) any {\n" +
		"    method := \"GET\"\n" +
		"    if opts != nil {\n" +
		"        if m, ok := opts[\"method\"].(string); ok {\n" +
		"            method = m\n" +
		"        }\n" +
		"    }\n" +
		"    var body io.Reader\n" +
		"    if opts != nil {\n" +
		"        if b, ok := opts[\"body\"]; ok {\n" +
		"            data, err := json.Marshal(b)\n" +
		"            if err != nil { panic(err) }\n" +
		"            body = bytes.NewReader(data)\n" +
		"        }\n" +
		"    }\n" +
		"    u, err := url.Parse(url)\n" +
		"    if err != nil { panic(err) }\n" +
		"    if opts != nil {\n" +
		"        if q, ok := opts[\"query\"]; ok {\n" +
		"            vals := u.Query()\n" +
		"            for k, v := range _toAnyMap(q) {\n" +
		"                vals.Set(k, fmt.Sprint(v))\n" +
		"            }\n" +
		"            u.RawQuery = vals.Encode()\n" +
		"        }\n" +
		"    }\n" +
		"    req, err := http.NewRequest(method, u.String(), body)\n" +
		"    if err != nil { panic(err) }\n" +
		"    if opts != nil {\n" +
		"        if hs, ok := opts[\"headers\"]; ok {\n" +
		"            for k, v := range _toAnyMap(hs) {\n" +
		"                if s, ok := v.(string); ok {\n" +
		"                    req.Header.Set(k, s)\n" +
		"                }\n" +
		"            }\n" +
		"        }\n" +
		"    }\n" +
		"    client := http.DefaultClient\n" +
		"    if opts != nil {\n" +
		"        if t, ok := opts[\"timeout\"]; ok {\n" +
		"            switch v := t.(type) {\n" +
		"            case int:\n" +
		"                client = &http.Client{Timeout: time.Duration(v) * time.Second}\n" +
		"            case int64:\n" +
		"                client = &http.Client{Timeout: time.Duration(v) * time.Second}\n" +
		"            case float64:\n" +
		"                client = &http.Client{Timeout: time.Duration(v * float64(time.Second))}\n" +
		"            case float32:\n" +
		"                client = &http.Client{Timeout: time.Duration(float64(v) * float64(time.Second))}\n" +
		"            }\n" +
		"        }\n" +
		"    }\n" +
		"    resp, err := client.Do(req)\n" +
		"    if err != nil { panic(err) }\n" +
		"    defer resp.Body.Close()\n" +
		"    data, err := io.ReadAll(resp.Body)\n" +
		"    if err != nil { panic(err) }\n" +
		"    var out any\n" +
		"    if err := json.Unmarshal(data, &out); err != nil { panic(err) }\n" +
		"    return out\n" +
		"}\n"

	helperToAnyMap = "func _toAnyMap(m any) map[string]any {\n" +
		"    switch v := m.(type) {\n" +
		"    case map[string]any:\n" +
		"        return v\n" +
		"    case map[string]string:\n" +
		"        out := make(map[string]any, len(v))\n" +
		"        for k, vv := range v {\n" +
		"            out[k] = vv\n" +
		"        }\n" +
		"        return out\n" +
		"    default:\n" +
		"        return nil\n" +
		"    }\n" +
		"}\n"

	helperCast = "func _cast[T any](v any) T {\n" +
		"    data, err := json.Marshal(v)\n" +
		"    if err != nil { panic(err) }\n" +
		"    var out T\n" +
		"    if err := json.Unmarshal(data, &out); err != nil { panic(err) }\n" +
		"    return out\n" +
		"}\n"

	helperQuery = "type _joinSpec struct { items []any; on func(...any) bool; left bool; right bool }\n" +
		"type _queryOpts struct { selectFn func(...any) any; where func(...any) bool; sortKey func(...any) any; skip int; take int }\n" +
		"func _query(src []any, joins []_joinSpec, opts _queryOpts) []any {\n" +
		"    items := make([][]any, len(src))\n" +
		"    for i, v := range src { items[i] = []any{v} }\n" +
		"    for _, j := range joins {\n" +
		"        joined := [][]any{}\n" +
		"        if j.right && j.left {\n" +
		"            matched := make([]bool, len(j.items))\n" +
		"            for _, left := range items {\n" +
		"                m := false\n" +
		"                for ri, right := range j.items {\n" +
		"                    keep := true\n" +
		"                    if j.on != nil { args := append(append([]any(nil), left...), right); keep = j.on(args...) }\n" +
		"                    if !keep { continue }\n" +
		"                    m = true; matched[ri] = true\n" +
		"                    joined = append(joined, append(append([]any(nil), left...), right))\n" +
		"                }\n" +
		"                if !m { joined = append(joined, append(append([]any(nil), left...), nil)) }\n" +
		"            }\n" +
		"            for ri, right := range j.items {\n" +
		"                if !matched[ri] { undef := make([]any, len(items[0])); joined = append(joined, append(undef, right)) }\n" +
		"            }\n" +
		"        } else if j.right {\n" +
		"            for _, right := range j.items {\n" +
		"                m := false\n" +
		"                for _, left := range items {\n" +
		"                    keep := true\n" +
		"                    if j.on != nil { args := append(append([]any(nil), left...), right); keep = j.on(args...) }\n" +
		"                    if !keep { continue }\n" +
		"                    m = true; joined = append(joined, append(append([]any(nil), left...), right))\n" +
		"                }\n" +
		"                if !m { undef := make([]any, len(items[0])); joined = append(joined, append(undef, right)) }\n" +
		"            }\n" +
		"        } else {\n" +
		"            for _, left := range items {\n" +
		"                m := false\n" +
		"                for _, right := range j.items {\n" +
		"                    keep := true\n" +
		"                    if j.on != nil { args := append(append([]any(nil), left...), right); keep = j.on(args...) }\n" +
		"                    if !keep { continue }\n" +
		"                    m = true; joined = append(joined, append(append([]any(nil), left...), right))\n" +
		"                }\n" +
		"                if j.left && !m { joined = append(joined, append(append([]any(nil), left...), nil)) }\n" +
		"            }\n" +
		"        }\n" +
		"        items = joined\n" +
		"    }\n" +
		"    if opts.where != nil {\n" +
		"        filtered := [][]any{}\n" +
		"        for _, r := range items { if opts.where(r...) { filtered = append(filtered, r) } }\n" +
		"        items = filtered\n" +
		"    }\n" +
		"    if opts.sortKey != nil {\n" +
		"        type pair struct { item []any; key any }\n" +
		"        pairs := make([]pair, len(items))\n" +
		"        for i, it := range items { pairs[i] = pair{it, opts.sortKey(it...)} }\n" +
		"        sort.Slice(pairs, func(i, j int) bool {\n" +
		"            a, b := pairs[i].key, pairs[j].key\n" +
		"            switch av := a.(type) {\n" +
		"            case int:\n" +
		"                switch bv := b.(type) { case int: return av < bv; case float64: return float64(av) < bv }\n" +
		"            case float64:\n" +
		"                switch bv := b.(type) { case int: return av < float64(bv); case float64: return av < bv }\n" +
		"            case string:\n" +
		"                bs, _ := b.(string); return av < bs\n" +
		"            }\n" +
		"            return fmt.Sprint(a) < fmt.Sprint(b)\n" +
		"        })\n" +
		"        for i, p := range pairs { items[i] = p.item }\n" +
		"    }\n" +
		"    if opts.skip >= 0 { if opts.skip < len(items) { items = items[opts.skip:] } else { items = [][]any{} } }\n" +
		"    if opts.take >= 0 { if opts.take < len(items) { items = items[:opts.take] } }\n" +
		"    res := make([]any, len(items))\n" +
		"    for i, r := range items { res[i] = opts.selectFn(r...) }\n" +
		"    return res\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_index":       helperIndex,
	"_indexString": helperIndexString,
	"_iter":        helperIter,
	"_genText":     helperGenText,
	"_genEmbed":    helperGenEmbed,
	"_genStruct":   helperGenStruct,
	"_fetch":       helperFetch,
	"_toAnyMap":    helperToAnyMap,
	"_cast":        helperCast,
	"_query":       helperQuery,
}

func (c *Compiler) use(name string) {
	c.helpers[name] = true
}

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
		c.buf.WriteByte('\n')
	}
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}
