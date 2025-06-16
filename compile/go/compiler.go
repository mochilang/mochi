package gocode

import (
	"bytes"
	"fmt"
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

	pyModules     map[string]string
	goModules     map[string]string
	tsModules     map[string]string
	externObjects map[string]bool

	tempVarCount int

	returnType types.Type
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

		pyModules:     map[string]string{},
		goModules:     map[string]string{},
		tsModules:     map[string]string{},
		externObjects: map[string]bool{},
		tempVarCount:  0,
	}
}

// Compile returns Go source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// Compile the program body first so we know all helper imports.
	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent = 0

	for _, stmt := range prog.Statements {
		c.scanImports(stmt)
	}

	// Generate program body.
	c.writeExpectFunc(prog)

	if err := c.compileTypeDecls(prog); err != nil {
		return nil, err
	}
	if err := c.compileFunDecls(prog); err != nil {
		return nil, err
	}
	if err := c.compileTestBlocks(prog); err != nil {
		return nil, err
	}
	if err := c.compileMainFunc(prog); err != nil {
		return nil, err
	}

	c.writeln("")
	c.emitRuntime()

	bodyBytes := c.buf.Bytes()

	// Restore buffer and write final output with updated imports.
	c.buf = oldBuf
	c.indent = 0
	c.writeln("package main")
	c.writeln("")
	c.writeImports()
	c.buf.Write(bodyBytes)

	return c.buf.Bytes(), nil
}

func (c *Compiler) writeImports() {
	if len(c.imports) == 0 {
		return
	}
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

func (c *Compiler) writeExpectFunc(prog *parser.Program) {
	if !hasExpect(prog) {
		return
	}
	c.writeln("func expect(cond bool) {")
	c.indent++
	c.writeln("if !cond { panic(\"expect failed\") }")
	c.indent--
	c.writeln("}")
	c.writeln("")
}

func (c *Compiler) compileTypeDecls(prog *parser.Program) error {
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileFunDecls(prog *parser.Program) error {
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileTestBlocks(prog *parser.Program) error {
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileMainFunc(prog *parser.Program) error {
	// Emit global variables for statements that appear before any test block.
	for i, s := range prog.Statements {
		if s.Let == nil && s.Var == nil {
			continue
		}
		if hasLaterTest(prog, i) {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
	}

	c.writeln("func main() {")
	c.indent++
	if len(c.externObjects) > 0 {
		c.imports["mochi/runtime/ffi/extern"] = true
		names := make([]string, 0, len(c.externObjects))
		for name := range c.externObjects {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			c.writeln(fmt.Sprintf("if _, ok := extern.Get(%q); !ok { panic(\"extern object not registered: %s\") }", name, name))
		}
	}
	for i, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if (s.Let != nil || s.Var != nil) && hasLaterTest(prog, i) {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return err
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
	return nil
}

func hasLaterTest(prog *parser.Program, idx int) bool {
	for _, s := range prog.Statements[idx+1:] {
		if s.Test != nil {
			return true
		}
	}
	return false
}

func exprUsesVarFun(fn *parser.FunExpr, name string) bool {
	for _, s := range fn.BlockBody {
		if stmtUsesVar(s, name) {
			return true
		}
	}
	if fn.ExprBody != nil && exprUsesVar(fn.ExprBody, name) {
		return true
	}
	return false
}

func stmtUsesVar(s *parser.Statement, name string) bool {
	switch {
	case s.Let != nil:
		if s.Let.Name == name {
			return true
		}
		return exprUsesVar(s.Let.Value, name)
	case s.Var != nil:
		if s.Var.Name == name {
			return true
		}
		return exprUsesVar(s.Var.Value, name)
	case s.Assign != nil:
		if s.Assign.Name == name {
			return true
		}
		return exprUsesVar(s.Assign.Value, name)
	case s.Expr != nil:
		return exprUsesVar(s.Expr.Expr, name)
	case s.Return != nil:
		return exprUsesVar(s.Return.Value, name)
	case s.If != nil:
		if exprUsesVar(s.If.Cond, name) {
			return true
		}
		for _, t := range s.If.Then {
			if stmtUsesVar(t, name) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if stmtUsesVar(&parser.Statement{If: s.If.ElseIf}, name) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.For != nil:
		if exprUsesVar(s.For.Source, name) || exprUsesVar(s.For.RangeEnd, name) {
			return true
		}
		for _, t := range s.For.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.While != nil:
		if exprUsesVar(s.While.Cond, name) {
			return true
		}
		for _, t := range s.While.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	}
	return false
}

func exprUsesVar(e *parser.Expr, name string) bool {
	if e == nil {
		return false
	}
	if unaryUsesVar(e.Binary.Left, name) {
		return true
	}
	for _, op := range e.Binary.Right {
		if postfixUsesVar(op.Right, name) {
			return true
		}
	}
	return false
}

func unaryUsesVar(u *parser.Unary, name string) bool {
	if u == nil {
		return false
	}
	return postfixUsesVar(u.Value, name)
}

func postfixUsesVar(p *parser.PostfixExpr, name string) bool {
	if p == nil {
		return false
	}
	if primaryUsesVar(p.Target, name) {
		return true
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if exprUsesVar(op.Index.Start, name) || exprUsesVar(op.Index.End, name) {
				return true
			}
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				if exprUsesVar(a, name) {
					return true
				}
			}
		}
	}
	return false
}

func primaryUsesVar(p *parser.Primary, name string) bool {
	if p == nil {
		return false
	}
	if p.Selector != nil {
		if p.Selector.Root == name && len(p.Selector.Tail) == 0 {
			return true
		}
	}
	if p.Group != nil {
		return exprUsesVar(p.Group, name)
	}
	if p.FunExpr != nil {
		return exprUsesVarFun(p.FunExpr, name)
	}
	if p.Call != nil {
		if p.Call.Func == name {
			return true
		}
		for _, a := range p.Call.Args {
			if exprUsesVar(a, name) {
				return true
			}
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			if exprUsesVar(e, name) {
				return true
			}
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			if exprUsesVar(it.Key, name) || exprUsesVar(it.Value, name) {
				return true
			}
		}
	}
	if p.Match != nil {
		if exprUsesVar(p.Match.Target, name) {
			return true
		}
		for _, cs := range p.Match.Cases {
			if exprUsesVar(cs.Pattern, name) || exprUsesVar(cs.Result, name) {
				return true
			}
		}
	}
	if p.Query != nil {
		if exprUsesVar(p.Query.Source, name) {
			return true
		}
		if p.Query.Where != nil && exprUsesVar(p.Query.Where, name) {
			return true
		}
		if p.Query.Sort != nil && exprUsesVar(p.Query.Sort, name) {
			return true
		}
		if p.Query.Skip != nil && exprUsesVar(p.Query.Skip, name) {
			return true
		}
		if p.Query.Take != nil && exprUsesVar(p.Query.Take, name) {
			return true
		}
		if exprUsesVar(p.Query.Select, name) {
			return true
		}
	}
	if p.Load != nil {
		// path is a string literal; no variable usage
	}
	if p.Save != nil {
		if exprUsesVar(p.Save.Src, name) {
			return true
		}
	}
	return false
}

func pureFunExpr(e *parser.Expr) *parser.FunExpr {
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
	return p.Target.FunExpr
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
		if c.returnType != nil && !equalTypes(c.returnType, c.inferExprType(s.Return.Value)) {
			c.use("_cast")
			expr = fmt.Sprintf("_cast[%s](%s)", goType(c.returnType), expr)
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
	case s.Import != nil:
		return c.addImport(s.Import)
	case s.ExternObject != nil:
		c.externObjects[s.ExternObject.Name] = true
		return nil
	case s.Fun != nil:
		return c.compileFunStmt(s.Fun)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil:
		return nil
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
	if fn := pureFunExpr(s.Value); fn != nil && exprUsesVarFun(fn, s.Name) {
		c.writeln(fmt.Sprintf("var %s %s", name, typStr))
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	} else {
		c.writeln(fmt.Sprintf("var %s %s = %s", name, typStr, value))
	}
	if lit != nil && c.env != nil {
		c.env.SetValue(s.Name, literalValue(lit), false)
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)

	var typ types.Type = types.AnyType{}
	if c.env != nil {
		if s.Type != nil {
			typ = resolveTypeRef(s.Type)
		} else if s.Value != nil {
			typ = c.inferExprType(s.Value)
		} else if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
		c.env.SetVar(s.Name, typ, true)
	}
	typStr := goType(typ)

	value := "nil"
	if s.Value != nil {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := typ.(types.MapType); ok {
				value = fmt.Sprintf("map[%s]%s{}", goType(mt.Key), goType(mt.Value))
			}
		} else if ll := s.Value.Binary.Left.Value.Target.List; ll != nil && len(ll.Elems) == 0 {
			if lt, ok := typ.(types.ListType); ok {
				value = fmt.Sprintf("[]%s{}", goType(lt.Elem))
			}
		}
		if value == "nil" {
			v, err := c.compileExpr(s.Value)
			if err != nil {
				return err
			}
			value = v
		}
	}

	if fn := pureFunExpr(s.Value); fn != nil && exprUsesVarFun(fn, s.Name) {
		c.writeln(fmt.Sprintf("var %s %s", name, typStr))
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	} else {
		c.writeln(fmt.Sprintf("var %s %s = %s", name, typStr, value))
	}
	c.writeln(fmt.Sprintf("_ = %s", name))
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)
	for _, idx := range s.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	value, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, value))
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
	c.writeln(fmt.Sprintf("var %s = stream.New(%q, 64, nil)", varName, s.Name))
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
	c.writeln(fmt.Sprintf("inst := &%s{Agent: agent.New(agent.Config{Name: %q, BufSize: 16, WG: nil})}", name, a.Name))

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
	if lit, ok := c.evalConstExpr(stmt.Cond); ok && lit.Bool != nil && bool(*lit.Bool) {
		c.buf.WriteString("for {\n")
	} else {
		c.buf.WriteString("for " + cond + " {\n")
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
	case types.MapType:
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for %s := range %s {\n", name, src))
		if c.env != nil {
			c.env.SetVar(stmt.Name, tt.Key, true)
		}
	default:
		return fmt.Errorf("cannot iterate over type %s", t)
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
	// Fallback to declared parameter and return types when type checker
	// hasn't populated the env (e.g. for nested functions).
	if ft.Params == nil {
		ft.Params = make([]types.Type, len(fun.Params))
		for i, p := range fun.Params {
			if p.Type != nil {
				ft.Params[i] = resolveTypeRef(p.Type)
			}
		}
	}
	if ft.Return == nil && fun.Return != nil {
		ft.Return = resolveTypeRef(fun.Return)
	}
	if ft.Return == nil {
		ft.Return = types.VoidType{}
	}
	if c.env != nil {
		c.env.SetVar(fun.Name, ft, false)
	}

	// Nested functions are compiled as function literals assigned to a variable.
	if c.indent > 0 {
		expr, err := c.compileFunExpr(&parser.FunExpr{Params: fun.Params, Return: fun.Return, BlockBody: fun.Body})
		if err != nil {
			return err
		}
		if exprUsesVarFun(&parser.FunExpr{Params: fun.Params, Return: fun.Return, BlockBody: fun.Body}, fun.Name) {
			c.writeln(fmt.Sprintf("var %s %s", name, goType(ft)))
			c.writeln(fmt.Sprintf("%s = %s", name, expr))
		} else {
			c.writeln("var " + name + " = " + expr)
		}
		return nil
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
	origRet := c.returnType
	c.returnType = ft.Return
	c.env = child
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = originalEnv
			c.returnType = origRet
			return err
		}
	}
	c.indent--
	c.env = originalEnv
	c.returnType = origRet
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
	if b == nil {
		return "", nil
	}

	leftExpr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	leftType := c.inferUnaryType(b.Left)

	operands := []string{leftExpr}
	typesList := []types.Type{leftType}
	ops := []string{}

	for _, part := range b.Right {
		right, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		rightType := c.inferPostfixType(part.Right)
		ops = append(ops, part.Op)
		operands = append(operands, right)
		typesList = append(typesList, rightType)
	}

	precLevels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	for _, level := range precLevels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				expr, typ, err := c.compileBinaryOp(operands[i], typesList[i], ops[i], operands[i+1], typesList[i+1])
				if err != nil {
					return "", err
				}
				operands[i] = expr
				typesList[i] = typ
				operands = append(operands[:i+1], operands[i+2:]...)
				typesList = append(typesList[:i+1], typesList[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary compilation")
	}
	return operands[0], nil
}

func (c *Compiler) compileBinaryOp(left string, leftType types.Type, op string, right string, rightType types.Type) (string, types.Type, error) {
	var expr string
	var next types.Type
	switch op {
	case "+", "-", "*", "/", "%":
		if op != "+" && (isAny(leftType) || isAny(rightType)) {
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
		switch {
		case (isInt64(leftType) && (isInt64(rightType) || isInt(rightType))) ||
			(isInt64(rightType) && (isInt64(leftType) || isInt(leftType))):
			expr = fmt.Sprintf("(int64(%s) %s int64(%s))", left, op, right)
			next = types.Int64Type{}
		case isInt(leftType) && isInt(rightType):
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			next = types.IntType{}
		case isFloat(leftType) && isFloat(rightType):
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			next = types.FloatType{}
		case op == "+" && isAny(leftType) && isList(rightType):
			rt := rightType.(types.ListType)
			c.use("_cast")
			left = fmt.Sprintf("_cast[%s](%s)", goType(rightType), left)
			expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(rt.Elem), left, right)
			next = rightType
		case op == "+" && isList(leftType) && isAny(rightType):
			lt := leftType.(types.ListType)
			c.use("_cast")
			right = fmt.Sprintf("_cast[%s](%s)", goType(leftType), right)
			expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
			next = leftType
		case op == "+" && isAny(leftType) && isString(rightType):
			expr = fmt.Sprintf("fmt.Sprint(%s) + %s", left, right)
			c.imports["fmt"] = true
			next = types.StringType{}
		case op == "+" && isString(leftType) && isAny(rightType):
			expr = fmt.Sprintf("%s + fmt.Sprint(%s)", left, right)
			c.imports["fmt"] = true
			next = types.StringType{}
		case op == "+" && isList(leftType) && isList(rightType):
			lt := leftType.(types.ListType)
			rt := rightType.(types.ListType)
			if _, ok := lt.Elem.(types.AnyType); ok && !isAny(rt.Elem) {
				c.use("_toAnySlice")
				right = fmt.Sprintf("_toAnySlice(%s)", right)
				expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
				next = leftType
			} else if !isAny(lt.Elem) && isAny(rt.Elem) {
				c.use("_toAnySlice")
				left = fmt.Sprintf("_toAnySlice(%s)", left)
				expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(rt.Elem), left, right)
				next = types.ListType{Elem: types.AnyType{}}
			} else if equalTypes(lt.Elem, rt.Elem) {
				expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
				next = leftType
			} else {
				c.use("_toAnySlice")
				if !isAny(lt.Elem) {
					left = fmt.Sprintf("_toAnySlice(%s)", left)
				}
				if !isAny(rt.Elem) {
					right = fmt.Sprintf("_toAnySlice(%s)", right)
				}
				expr = fmt.Sprintf("append(append([]any{}, %s...), %s...)", left, right)
				next = types.ListType{Elem: types.AnyType{}}
			}
		case op == "+" && isString(leftType) && isString(rightType):
			expr = fmt.Sprintf("%s + %s", left, right)
			next = types.StringType{}
		case op == "+" && isAny(leftType) && isAny(rightType):
			c.use("_cast")
			left = fmt.Sprintf("_cast[[]any](%s)", left)
			right = fmt.Sprintf("_cast[[]any](%s)", right)
			expr = fmt.Sprintf("append(append([]any{}, %s...), %s...)", left, right)
			next = types.ListType{Elem: types.AnyType{}}
		default:
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
	case "==", "!=", "<", "<=", ">", ">=":
		if _, ok := leftType.(types.AnyType); ok || isAny(rightType) {
			return "", types.AnyType{}, fmt.Errorf("incompatible types in comparison: %s and %s", leftType, rightType)
		}
		if isList(leftType) || isList(rightType) || isMap(leftType) || isMap(rightType) || isStruct(leftType) || isStruct(rightType) {
			c.use("_equal")
			c.imports["reflect"] = true
			if op == "==" {
				expr = fmt.Sprintf("_equal(%s, %s)", left, right)
			} else if op == "!=" {
				expr = fmt.Sprintf("!_equal(%s, %s)", left, right)
			} else {
				return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
			}
		} else {
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
		}
		next = types.BoolType{}
	case "&&", "||":
		if !(isBool(leftType) && isBool(rightType)) {
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
		expr = fmt.Sprintf("(%s %s %s)", left, op, right)
		next = types.BoolType{}
	case "in":
		switch rightType.(type) {
		case types.MapType:
			keyTemp := c.newVar()
			mapTemp := c.newVar()
			c.writeln(fmt.Sprintf("%s := %s", keyTemp, left))
			c.writeln(fmt.Sprintf("%s := %s", mapTemp, right))
			okVar := c.newVar()
			c.writeln(fmt.Sprintf("_, %s := %s[%s]", okVar, mapTemp, keyTemp))
			expr = okVar
			next = types.BoolType{}
		case types.ListType:
			itemVar := c.newVar()
			listVar := c.newVar()
			c.writeln(fmt.Sprintf("%s := %s", itemVar, left))
			c.writeln(fmt.Sprintf("%s := %s", listVar, right))
			resultVar := c.newVar()
			c.writeln(fmt.Sprintf("%s := false", resultVar))
			iterVar := c.newVar()
			c.writeln(fmt.Sprintf("for _, %s := range %s {", iterVar, listVar))
			c.indent++
			c.writeln(fmt.Sprintf("if %s == %s { %s = true; break }", iterVar, itemVar, resultVar))
			c.indent--
			c.writeln("}")
			expr = resultVar
			next = types.BoolType{}
		case types.StringType:
			c.imports["strings"] = true
			expr = fmt.Sprintf("strings.Contains(%s, %s)", right, left)
			next = types.BoolType{}
		default:
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
	default:
		return "", types.AnyType{}, fmt.Errorf("unsupported operator: %s", op)
	}
	return expr, next, nil
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
	// Special handling for FFI selectors
	if sel := p.Target.Selector; sel != nil {
		if mod, ok := c.pyModules[sel.Root]; ok {
			attr := strings.Join(sel.Tail, ".")
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				c.imports["mochi/runtime/ffi/python"] = true
				var rtype types.Type = types.AnyType{}
				if t, err := c.env.GetVar(sel.Root + "." + attr); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						rtype = ft.Return
					}
				}
				val := fmt.Sprintf("func() %s { v, _ := python.Attr(%q, %q, %s); return v.(%s) }()", goType(rtype), mod, attr, strings.Join(args, ", "), goType(rtype))
				if goType(rtype) == "any" {
					val = fmt.Sprintf("func() any { v, _ := python.Attr(%q, %q, %s); return v }()", mod, attr, strings.Join(args, ", "))
				}
				return val, nil
			}
			c.imports["mochi/runtime/ffi/python"] = true
			var rtype types.Type = types.AnyType{}
			if t, err := c.env.GetVar(sel.Root + "." + attr); err == nil {
				rtype = t
			}
			if goType(rtype) == "any" {
				val := fmt.Sprintf("func() any { v, _ := python.Attr(%q, %q); return v }()", mod, attr)
				return val, nil
			}
			val := fmt.Sprintf("func() %s { v, _ := python.Attr(%q, %q); return v.(%s) }()", goType(rtype), mod, attr, goType(rtype))
			return val, nil
		}
		if mod, ok := c.goModules[sel.Root]; ok {
			name := mod
			if len(sel.Tail) > 0 {
				name += "." + strings.Join(sel.Tail, ".")
			}
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				c.imports["mochi/runtime/ffi/go"] = true
				val := fmt.Sprintf("func() any { v, _ := goffi.Call(%q, %s); return v }()", name, strings.Join(args, ", "))
				return val, nil
			}
			c.imports["mochi/runtime/ffi/go"] = true
			val := fmt.Sprintf("func() any { v, _ := goffi.Call(%q); return v }()", name)
			return val, nil
		}
		if mod, ok := c.tsModules[sel.Root]; ok {
			attr := strings.Join(sel.Tail, ":")
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				c.imports["mochi/runtime/ffi/deno"] = true
				val := fmt.Sprintf("func() any { v, _ := denoffi.Attr(%q, %q, %s); return v }()", mod, attr, strings.Join(args, ", "))
				return val, nil
			}
			c.imports["mochi/runtime/ffi/deno"] = true
			val := fmt.Sprintf("func() any { v, _ := denoffi.Attr(%q, %q); return v }()", mod, attr)
			return val, nil
		}
	}

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
			if ft, ok := typ.(types.FuncType); ok {
				typ = ft.Return
			} else {
				typ = types.AnyType{}
			}
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
					return "", fmt.Errorf("cannot index into type %s", typ)
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
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		if _, ok := typ.(types.MapType); ok && len(p.Selector.Tail) > 0 {
			key := p.Selector.Tail[0]
			base = fmt.Sprintf("%s[%q]", base, key)
			for _, field := range p.Selector.Tail[1:] {
				base += ".[" + fmt.Sprintf("%q", field) + "]"
			}
			return base, nil
		}
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
			var k string
			if s, ok := simpleStringKey(item.Key); ok {
				k = fmt.Sprintf("\"%s\"", s)
			} else {
				kk, err := c.compileExpr(item.Key)
				if err != nil {
					return "", err
				}
				k = fmt.Sprintf("%s", kk)
				if !(strings.HasPrefix(k, "\"") && strings.HasSuffix(k, "\"")) {
					k = fmt.Sprintf("%s", kk)
				}
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
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
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
	var (
		prompt string
		text   string
		model  string
		params []string
	)
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
			params = append(params, fmt.Sprintf("%q: %s", f.Name, v))
		}
	}

	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}

	paramStr := "nil"
	if len(params) > 0 {
		paramStr = fmt.Sprintf("map[string]any{%s}", strings.Join(params, ", "))
	}

	if model == "" {
		model = "\"\""
	}

	c.imports["context"] = true
	c.imports["mochi/runtime/llm"] = true

	if g.Target == "embedding" {
		c.use("_genEmbed")
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramStr), nil
	}

	if _, ok := c.env.GetStruct(g.Target); ok {
		c.use("_genStruct")
		c.imports["encoding/json"] = true
		return fmt.Sprintf("_genStruct[%s](%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
	}

	c.use("_genText")
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		opts = fmt.Sprintf("_toAnyMap(%s)", v)
	}
	c.imports["mochi/runtime/data"] = true
	c.imports["os"] = true
	c.use("_load")
	if l.Type != nil {
		t := resolveTypeRef(l.Type)
		if st, ok := c.env.GetStruct(*l.Type.Simple); t == (types.AnyType{}) && ok {
			t = st
		}
		goT := goType(t)
		if goT == "" {
			goT = "any"
		}
		c.use("_cast")
		if goT != "any" {
			c.imports["encoding/json"] = true
		}
		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("func() []%s {\n", goT))
		buf.WriteString(fmt.Sprintf("\trows := _load(%s, %s)\n", path, opts))
		buf.WriteString(fmt.Sprintf("\tout := make([]%s, len(rows))\n", goT))
		buf.WriteString("\tfor i, r := range rows {\n")
		if goT == "any" {
			buf.WriteString("\t\tout[i] = r\n")
		} else {
			buf.WriteString(fmt.Sprintf("\t\tout[i] = _cast[%s](r)\n", goT))
		}
		buf.WriteString("\t}\n")
		buf.WriteString("\treturn out\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
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
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		opts = fmt.Sprintf("_toAnyMap(%s)", v)
	}
	c.imports["mochi/runtime/data"] = true
	c.imports["os"] = true
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
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
			joins = append(joins, fmt.Sprintf("{items: %s}", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(params, sanitizeName(q.Joins[i].Var))
			onFn := fmt.Sprintf("func(%s) bool { return %s }", strings.Join(onParams, ", "), joinOns[i])
			spec := fmt.Sprintf("{items: %s, on: %s", js, onFn)
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
		c.use("_query")
		buf.WriteString(fmt.Sprintf("\tsrc := %s\n", src))
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
		return "", fmt.Errorf("query source must be list")
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
	buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", sanitizeName(q.Var), src))
	indent += "\t"

	for i := range q.Froms {
		fvar := sanitizeName(q.Froms[i].Var)
		fsrc := fromSrcs[i]
		if fromDirect[i] {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", fvar, fsrc))
		} else {
			return "", fmt.Errorf("query from source must be list")
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
			return "", fmt.Errorf("join source must be list")
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
				return "", fmt.Errorf("join source must be list")
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
	var paramTypes []types.Type
	if t, err := c.env.GetVar(call.Func); err == nil {
		if ft, ok := t.(types.FuncType); ok {
			paramTypes = ft.Params
		}
	}
	for i, a := range call.Args {
		if len(paramTypes) > i {
			if ll := a.Binary.Left.Value.Target.List; ll != nil && len(ll.Elems) == 0 {
				if lt, ok := paramTypes[i].(types.ListType); ok {
					args[i] = fmt.Sprintf("[]%s{}", goType(lt.Elem))
					continue
				}
			}
		}
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
	case "str":
		c.imports["fmt"] = true
		return fmt.Sprintf("fmt.Sprint(%s)", argStr), nil
	case "count":
		c.imports["mochi/runtime/data"] = true
		c.use("_count")
		return fmt.Sprintf("_count(%s)", argStr), nil
	case "avg":
		c.imports["mochi/runtime/data"] = true
		c.use("_avg")
		return fmt.Sprintf("_avg(%s)", argStr), nil
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
	sub := &Compiler{imports: c.imports, helpers: c.helpers, env: child, memo: map[string]*parser.Literal{}}
	sub.indent = 1
	if fn.Return != nil {
		sub.returnType = resolveTypeRef(fn.Return)
	}
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
	retType := ""
	if fn.Return != nil {
		retType = goType(resolveTypeRef(fn.Return))
	}
	if retType == "" {
		return "func(" + strings.Join(params, ", ") + ") {\n" + body + "}", nil
	}
	return "func(" + strings.Join(params, ", ") + ") " + retType + " {\n" + body + "}", nil
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
	if s.Import != nil {
		c.addImport(s.Import)
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
	leftType := c.inferUnaryType(b.Left)
	c.scanUnaryImports(b.Left)
	for _, op := range b.Right {
		c.scanPostfixImports(op.Right)
		rightType := c.inferPostfixType(op.Right)
		if (op.Op == "==" || op.Op == "!=") && (isList(leftType) || isList(rightType) || isMap(leftType) || isMap(rightType) || isStruct(leftType) || isStruct(rightType)) {
			c.imports["reflect"] = true
			c.use("_equal")
		}
		leftType = resultType(op.Op, leftType, rightType)
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
		if p.Call.Func == "str" {
			c.imports["fmt"] = true
		}
		if p.Call.Func == "count" || p.Call.Func == "avg" {
			c.imports["mochi/runtime/data"] = true
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
	case p.Load != nil:
		if p.Load.With != nil {
			c.scanExprImports(p.Load.With)
		}
		if p.Load.Type != nil {
			if st, ok := c.env.GetStruct(*p.Load.Type.Simple); ok {
				c.imports["encoding/json"] = true
				_ = st
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.imports["os"] = true
		c.use("_load")
	case p.Save != nil:
		c.scanExprImports(p.Save.Src)
		if p.Save.With != nil {
			c.scanExprImports(p.Save.With)
		}
		c.imports["mochi/runtime/data"] = true
		c.imports["os"] = true
		c.use("_save")
	case p.Selector != nil:
		// no imports
	case p.Lit != nil:
		// none
	}
}

func (c *Compiler) addImport(im *parser.ImportStmt) error {
	mod := strings.Trim(im.Path, "\"")
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	if im.Lang == nil {
		return fmt.Errorf("unsupported import language: <nil>")
	}
	switch *im.Lang {
	case "python":
		c.pyModules[alias] = mod
		c.imports["mochi/runtime/ffi/python"] = true
	case "go":
		c.goModules[alias] = mod
		c.imports["mochi/runtime/ffi/go"] = true
	case "typescript":
		c.tsModules[alias] = mod
		c.imports["mochi/runtime/ffi/deno"] = true
	default:
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
	return nil
}
