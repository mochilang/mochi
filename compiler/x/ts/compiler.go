//go:build slow

package tscode

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into TypeScript source code that can be run with Deno.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	helpers      map[string]bool
	imports      map[string]string
	env          *types.Env
	structs      map[string]bool
	agents       map[string]bool
	handlerCount int
	packages     map[string]bool
	root         string
	globals      map[string]string
	moduleScope  bool
}

// New creates a new TypeScript compiler instance.
func New(env *types.Env, root string) *Compiler {
	return &Compiler{
		helpers:     make(map[string]bool),
		imports:     make(map[string]string),
		env:         env,
		structs:     make(map[string]bool),
		agents:      make(map[string]bool),
		packages:    make(map[string]bool),
		root:        root,
		globals:     make(map[string]string),
		moduleScope: false,
	}
}

func containsStreamCode(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if stmtHasStream(s) {
			return true
		}
	}
	return false
}

var (
	fetchExprType = reflect.TypeOf(&parser.FetchExpr{})
	fetchStmtType = reflect.TypeOf(&parser.FetchStmt{})
)

func containsFetch(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if hasFetch(reflect.ValueOf(s)) {
			return true
		}
	}
	return false
}

func hasFetch(v reflect.Value) bool {
	if !v.IsValid() {
		return false
	}
	if v.Type() == fetchExprType || v.Type() == fetchStmtType {
		return !v.IsNil()
	}
	switch v.Kind() {
	case reflect.Ptr, reflect.Interface:
		if v.IsNil() {
			return false
		}
		return hasFetch(v.Elem())
	case reflect.Slice, reflect.Array:
		for i := 0; i < v.Len(); i++ {
			if hasFetch(v.Index(i)) {
				return true
			}
		}
	case reflect.Struct:
		for i := 0; i < v.NumField(); i++ {
			if hasFetch(v.Field(i)) {
				return true
			}
		}
	}
	return false
}

// collectImports scans statements for TypeScript imports.
func (c *Compiler) collectImports(stmts []*parser.Statement) {
	for _, s := range stmts {
		if s.Import != nil && s.Import.Lang != nil && *s.Import.Lang == "typescript" {
			path := strings.Trim(s.Import.Path, "\"")
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			c.imports[alias] = path
		}
	}
}

// collectGlobals records global variable declarations for top-level statements.
func (c *Compiler) collectGlobals(stmts []*parser.Statement) {
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			name := sanitizeName(s.Let.Name)
			var typ types.Type = types.AnyType{}
			if c.env != nil {
				if s.Let.Type != nil {
					typ = c.resolveTypeRef(s.Let.Type)
				} else if s.Let.Value != nil {
					typ = c.inferExprType(s.Let.Value)
				}
			}
			ts := tsType(typ)
			if ts != "" {
				c.globals[name] = fmt.Sprintf("let %s: %s", name, ts)
			} else {
				c.globals[name] = fmt.Sprintf("let %s", name)
			}
		case s.Var != nil:
			name := sanitizeName(s.Var.Name)
			var typ types.Type = types.AnyType{}
			if c.env != nil {
				if s.Var.Type != nil {
					typ = c.resolveTypeRef(s.Var.Type)
				} else if s.Var.Value != nil {
					typ = c.inferExprType(s.Var.Value)
				}
			}
			ts := tsType(typ)
			if ts != "" {
				c.globals[name] = fmt.Sprintf("var %s: %s", name, ts)
			} else {
				c.globals[name] = fmt.Sprintf("var %s", name)
			}
		case s.Fetch != nil:
			name := sanitizeName(s.Fetch.Target)
			c.globals[name] = fmt.Sprintf("let %s", name)
		}
	}
}

func (c *Compiler) compilePackageImport(alias, path, filename string) error {
	if c.packages[alias] {
		return nil
	}
	c.packages[alias] = true
	p := strings.Trim(path, "\"")
	base := c.root
	if strings.HasPrefix(p, "./") || strings.HasPrefix(p, "../") {
		base = filepath.Dir(filename)
	}
	target := filepath.Join(base, p)

	info, err := os.Stat(target)
	if err != nil {
		if os.IsNotExist(err) && !strings.HasSuffix(target, ".mochi") {
			if fi, err2 := os.Stat(target + ".mochi"); err2 == nil {
				info = fi
				target += ".mochi"
			} else {
				return fmt.Errorf("import package: %w", err)
			}
		} else {
			return fmt.Errorf("import package: %w", err)
		}
	}

	var files []string
	if info.IsDir() {
		entries, err := os.ReadDir(target)
		if err != nil {
			return fmt.Errorf("import package: %w", err)
		}
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
				files = append(files, filepath.Join(target, e.Name()))
			}
		}
		sort.Strings(files)
	} else {
		files = []string{target}
	}
	var stmts []*parser.Statement
	pkgName := alias
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			return err
		}
		if prog.Package != "" {
			pkgName = prog.Package
		}
		stmts = append(stmts, prog.Statements...)
	}
	c.writeln(fmt.Sprintf("const %s = (() => {", sanitizeName(alias)))
	c.indent++
	pkgEnv := types.NewEnv(c.env)
	origEnv := c.env
	c.env = pkgEnv
	exports := []string{}
	for _, s := range stmts {
		if s.Fun != nil {
			if s.Fun.Export {
				exports = append(exports, s.Fun.Name)
			}
			if err := c.compileFunStmt(s.Fun); err != nil {
				c.env = origEnv
				return err
			}
			c.writeln("")
			continue
		}
		if s.Import != nil {
			// imports within a package are ignored for now
			continue
		}
		if err := c.compileStmt(s); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.env = origEnv
	c.writeln("return {")
	c.indent++
	c.writeln(fmt.Sprintf("__name: \"%s\",", pkgName))
	for _, name := range exports {
		c.writeln(fmt.Sprintf("%s: %s,", sanitizeName(name), sanitizeName(name)))
	}
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("})()")
	c.writeln("")
	return nil
}

func stmtHasStream(s *parser.Statement) bool {
	switch {
	case s.Stream != nil, s.Emit != nil, s.On != nil, s.Agent != nil:
		return true
	case s.Fun != nil:
		return containsStreamCode(s.Fun.Body)
	case s.Test != nil:
		return containsStreamCode(s.Test.Body)
	case s.If != nil:
		if containsStreamCode(s.If.Then) {
			return true
		}
		if s.If.ElseIf != nil {
			if stmtHasStream(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		return containsStreamCode(s.If.Else)
	case s.While != nil:
		return containsStreamCode(s.While.Body)
	case s.For != nil:
		return containsStreamCode(s.For.Body)
	case s.On != nil:
		return containsStreamCode(s.On.Body)
	}
	return false
}

// Compile generates TypeScript source code for the given program.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	needsAsync := containsStreamCode(prog.Statements) || containsFetch(prog.Statements)

	// Pre-collect global variable declarations
	c.collectGlobals(prog.Statements)

	c.writeln("// Generated by Mochi TypeScript compiler")
	c.writeln("")

	// Collect TypeScript imports and emit them.
	c.collectImports(prog.Statements)
	for alias, path := range c.imports {
		c.writeln(fmt.Sprintf("import * as %s from \"%s\"", alias, path))
	}
	if len(c.imports) > 0 {
		c.writeln("")
	}

	// Handle simple built-in imports from other languages.
	for _, s := range prog.Statements {
		if s.Import != nil && s.Import.Lang != nil && *s.Import.Lang == "python" {
			p := strings.Trim(s.Import.Path, "\"")
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			switch p {
			case "math":
				c.writeln(fmt.Sprintf("const %s = { sqrt: Math.sqrt }", alias))
				c.writeln("")
			}
		}
	}

	// Compile Mochi package imports.
	for _, s := range prog.Statements {
		if s.Import != nil && s.Import.Lang == nil {
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			if err := c.compilePackageImport(alias, s.Import.Path, s.Pos.Filename); err != nil {
				return nil, err
			}
		}
	}

	// Emit type declarations first so they're visible globally.
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

	// Emit collected global variable declarations.
	if len(c.globals) > 0 {
		keys := make([]string, 0, len(c.globals))
		for k := range c.globals {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			c.writeln(c.globals[k])
		}
		c.writeln("")
	}

	// Emit test block declarations.
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	if needsAsync {
		c.writeln("async function main(): Promise<void> {")
	} else {
		c.writeln("function main(): void {")
	}
	c.moduleScope = true
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil || s.Type != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s()", name))
		}
	}
	c.indent--
	c.writeln("}")
	c.moduleScope = false
	if needsAsync {
		c.use("_waitAll")
	}
	c.emitRuntime()
	if needsAsync {
		c.writeln("await main()")
		c.writeln("await _waitAll()")
	} else {
		c.writeln("main()")
	}
	c.writeln("")
	code := c.buf.Bytes()
	return formatTS(code), nil
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
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Agent != nil:
		return c.compileAgentDecl(s.Agent)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Import != nil:
		// imports are emitted at the top in Compile()
		return nil
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
		// extern declarations have no runtime effect when compiling to TypeScript
		return nil
	case s.Fun != nil:
		return c.compileFunStmt(s.Fun)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if strings.HasPrefix(expr, "(") {
			c.writeln(";" + expr)
		} else {
			c.writeln(expr)
		}
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
	case s.Fetch != nil:
		return c.compileFetchStmt(s.Fetch)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	value := "undefined"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	var typ types.Type = types.AnyType{}
	if c.env != nil {
		if s.Type != nil {
			typ = c.resolveTypeRef(s.Type)
		} else if s.Value != nil {
			typ = c.inferExprType(s.Value)
		}
		c.env.SetVar(s.Name, typ, false)
	}
	typStr := tsType(typ)
	needType := s.Type != nil || s.Value == nil || (c.moduleScope && c.indent == 1)
	if c.moduleScope && c.indent == 1 {
		// declare at module scope
		if needType && typStr != "" {
			c.globals[name] = fmt.Sprintf("let %s: %s", name, typStr)
		} else {
			c.globals[name] = fmt.Sprintf("let %s", name)
		}
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	} else {
		if needType && typStr != "" {
			c.writeln(fmt.Sprintf("let %s: %s = %s", name, typStr, value))
		} else {
			c.writeln(fmt.Sprintf("let %s = %s", name, value))
		}
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	value := "undefined"
	if s.Value != nil {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if c.env != nil {
				if t, err := c.env.GetVar(s.Name); err == nil {
					if mt, ok := t.(types.MapType); ok {
						value = fmt.Sprintf("{} as Record<%s, %s>", tsType(mt.Key), tsType(mt.Value))
					}
				}
			}
		}
		if value == "undefined" {
			v, err := c.compileExpr(s.Value)
			if err != nil {
				return err
			}
			value = v
		}
	}
	var typ types.Type = types.AnyType{}
	if c.env != nil {
		if s.Type != nil {
			typ = c.resolveTypeRef(s.Type)
		} else if s.Value != nil {
			typ = c.inferExprType(s.Value)
		}
		c.env.SetVar(s.Name, typ, true)
	}
	typStr := tsType(typ)
	needType := s.Type != nil || s.Value == nil || (c.moduleScope && c.indent == 1)
	if c.moduleScope && c.indent == 1 {
		if needType && typStr != "" {
			c.globals[name] = fmt.Sprintf("var %s: %s", name, typStr)
		} else {
			c.globals[name] = fmt.Sprintf("var %s", name)
		}
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	} else {
		if needType && typStr != "" {
			c.writeln(fmt.Sprintf("var %s: %s = %s", name, typStr, value))
		} else {
			c.writeln(fmt.Sprintf("var %s = %s", name, value))
		}
	}
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

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (!(%s)) { throw new Error('expect failed') }", expr))
	return nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	c.compileStructType(st)
	varName := unexportName(sanitizeName(s.Name)) + "Stream"
	c.use("_stream")
	c.writeln(fmt.Sprintf("const %s = new Stream(%q)", varName, s.Name))
	return nil
}

func (c *Compiler) compileOnHandler(h *parser.OnHandler) error {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", h.Stream)
	}
	c.compileStructType(st)
	streamVar := unexportName(sanitizeName(h.Stream)) + "Stream"
	handlerName := fmt.Sprintf("_handler_%d", c.handlerCount)
	c.handlerCount++
	c.writeln(fmt.Sprintf("function %s(ev: %s): void {", handlerName, sanitizeName(st.Name)))
	c.indent++
	alias := sanitizeName(h.Alias)
	c.writeln(fmt.Sprintf("const %s = ev", alias))
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
	c.indent--
	c.writeln("}")
	c.writeln(fmt.Sprintf("%s.register(%s)", streamVar, handlerName))
	c.use("_stream")
	return nil
}

func (c *Compiler) compileEmit(e *parser.EmitStmt) error {
	st, ok := c.env.GetStream(e.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", e.Stream)
	}
	c.compileStructType(st)
	parts := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
	}
	lit := "{" + strings.Join(parts, ", ") + "}"
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
	if c.moduleScope && c.indent == 1 {
		c.globals[name] = fmt.Sprintf("let %s", name)
		c.writeln(fmt.Sprintf("%s = %s", name, expr))
	} else {
		c.writeln(fmt.Sprintf("const %s = %s", name, expr))
	}
	if c.env != nil {
		c.env.SetVar(f.Target, types.AnyType{}, false)
	}
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("for (let _i = 0; _i < %s.length; _i++) {\n", list))
	c.indent++
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("let _item = %s[_i];\n", list))

	var st types.StructType
	if c.env != nil {
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}
	child := types.NewEnv(c.env)
	if st.Name != "" {
		for _, fn := range st.Order {
			child.SetVar(fn, st.Fields[fn], true)
			c.writeIndent()
			c.buf.WriteString(fmt.Sprintf("let %s = _item.%s;\n", sanitizeName(fn), sanitizeName(fn)))
		}
	}
	orig := c.env
	c.env = child
	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = orig
			return err
		}
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("if (%s) {\n", cond))
		c.indent++
	}
	for _, it := range u.Set.Items {
		if key, ok := identName(it.Key); ok {
			val, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeIndent()
			c.buf.WriteString(fmt.Sprintf("_item.%s = %s;\n", sanitizeName(key), val))
			continue
		}

		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.env = orig
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = orig
			return err
		}
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("_item[%s] = %s;\n", keyExpr, valExpr))
	}
	if u.Where != nil {
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}\n")
	}
	c.env = orig
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("%s[_i] = _item;\n", list))
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
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

	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	c.writeln("Agent: Agent")
	for _, fn := range st.Order {
		if typ, ok := st.Fields[fn]; ok {
			ts := tsType(typ)
			if ts != "" {
				c.writeln(fmt.Sprintf("%s: %s", sanitizeName(fn), ts))
				continue
			}
		}
		c.writeln(fmt.Sprintf("%s: any", sanitizeName(fn)))
	}
	c.writeln("constructor() {")
	c.indent++
	c.use("_agent")
	c.writeln(fmt.Sprintf("this.Agent = new Agent(%q)", a.Name))
	orig := c.env
	c.env = baseEnv
	for _, blk := range a.Body {
		switch {
		case blk.Let != nil:
			val := "undefined"
			if blk.Let.Value != nil {
				v, err := c.compileExpr(blk.Let.Value)
				if err != nil {
					c.env = orig
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("this.%s = %s", sanitizeName(blk.Let.Name), val))
		case blk.Var != nil:
			val := "undefined"
			if blk.Var.Value != nil {
				v, err := c.compileExpr(blk.Var.Value)
				if err != nil {
					c.env = orig
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("this.%s = %s", sanitizeName(blk.Var.Name), val))
		}
	}
	c.env = orig
	handlerID := 0
	for _, blk := range a.Body {
		if blk.On != nil {
			streamVar := unexportName(sanitizeName(blk.On.Stream)) + "Stream"
			c.writeln(fmt.Sprintf("this.Agent.on(%s, this._on%d.bind(this))", streamVar, handlerID))
			handlerID++
		}
	}
	for _, blk := range a.Body {
		if blk.Intent != nil {
			mname := sanitizeName(blk.Intent.Name)
			c.writeln(fmt.Sprintf("this.Agent.registerIntent(%q, this.%s.bind(this))", blk.Intent.Name, mname))
		}
	}
	c.writeln("this.Agent.start()")
	c.indent--
	c.writeln("}")

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
	c.writeln("}")
	c.writeln("")
	c.writeln(fmt.Sprintf("function New%s(): %s {", name, name))
	c.indent++
	c.writeln(fmt.Sprintf("return new %s()", name))
	c.indent--
	c.writeln("}")
	c.writeln("")
	return nil
}

func (c *Compiler) compileAgentIntent(agentName string, env *types.Env, in *parser.IntentDecl) error {
	name := sanitizeName(in.Name)
	c.writeIndent()
	c.buf.WriteString(name + "(")
	for i, p := range in.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
	}
	c.buf.WriteString(") {\n")
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
	c.writeIndent()
	c.buf.WriteString("}\n\n")
	return nil
}

func (c *Compiler) compileAgentOn(agentName string, env *types.Env, h *parser.OnHandler, id int) (string, error) {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return "", fmt.Errorf("unknown stream: %s", h.Stream)
	}
	fname := fmt.Sprintf("_on%d", id)
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("%s(ev: %s): void {\n", fname, sanitizeName(st.Name)))
	alias := sanitizeName(h.Alias)
	child := types.NewEnv(env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	c.indent++
	c.writeln(fmt.Sprintf("const %s = ev", alias))
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return "", err
		}
	}
	c.indent--
	c.env = orig
	c.writeIndent()
	c.buf.WriteString("}\n\n")
	return fname, nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)

	if len(t.Variants) > 0 {
		var variants []string
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			variants = append(variants, vname)
			c.writeln(fmt.Sprintf("type %s = {", vname))
			c.indent++
			c.writeln(fmt.Sprintf("__name: \"%s\";", v.Name))
			for _, f := range v.Fields {
				ts := tsType(c.resolveTypeRef(f.Type))
				if ts != "" {
					c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(f.Name), ts))
				} else {
					c.writeln(fmt.Sprintf("%s: any;", sanitizeName(f.Name)))
				}
			}
			c.indent--
			c.writeln("}")
			c.writeln("")
		}
		c.writeln(fmt.Sprintf("type %s = %s", name, strings.Join(variants, " | ")))
		return nil
	}

	c.writeln(fmt.Sprintf("type %s = {", name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			ts := tsType(c.resolveTypeRef(m.Field.Type))
			if ts != "" {
				c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(m.Field.Name), ts))
			} else {
				c.writeln(fmt.Sprintf("%s: any;", sanitizeName(m.Field.Name)))
			}
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
	c.buf.WriteString("if (" + cond + ") {")
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

func ifStmtToExpr(s *parser.IfStmt) *parser.IfExpr {
	if s == nil {
		return nil
	}
	if len(s.Then) != 1 || s.Then[0].Expr == nil {
		return nil
	}
	ie := &parser.IfExpr{Pos: s.Pos, Cond: s.Cond, Then: s.Then[0].Expr.Expr}
	if s.ElseIf != nil {
		ie.ElseIf = ifStmtToExpr(s.ElseIf)
		if ie.ElseIf == nil {
			return nil
		}
	}
	if len(s.Else) == 1 && s.Else[0].Expr != nil {
		ie.Else = s.Else[0].Expr.Expr
	} else if len(s.Else) > 0 {
		return nil
	}
	return ie
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString("while (" + cond + ") {\n")
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
		elseVal = "undefined"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenVal, elseVal), nil
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
		var t types.Type = types.IntType{}
		if c.env != nil {
			c.env.SetVar(stmt.Name, t, true)
		}
		ts := tsType(t)
		c.buf.WriteString(fmt.Sprintf("for (let %s: %s = %s; %s < %s; %s++) {\n", name, ts, start, name, end, name))
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
	var elem types.Type = types.AnyType{}
	convert := ""
	keyVar := name
	switch tt := t.(type) {
	case types.ListType:
		elem = tt.Elem
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for (const %s of %s) {\n", name, src))
	case types.StringType:
		elem = types.StringType{}
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for (const %s of %s) {\n", name, src))
	case types.MapType:
		elem = tt.Key
		c.writeIndent()
		if isInt(tt.Key) || isInt64(tt.Key) || isFloat(tt.Key) {
			keyVar = name + "Key"
			convert = fmt.Sprintf("const %s: number = Number(%s)\n", name, keyVar)
		}
		c.buf.WriteString(fmt.Sprintf("for (const %s of Object.keys(%s)) {\n", keyVar, src))
	default:
		c.writeIndent()
		c.use("_iter")
		c.buf.WriteString(fmt.Sprintf("for (const %s of _iter(%s)) {\n", name, src))
	}
	if c.env != nil {
		c.env.SetVar(stmt.Name, elem, true)
	}
	c.indent++
	if convert != "" {
		c.writeIndent()
		c.buf.WriteString(convert)
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
	c.writeIndent()
	c.buf.WriteString("function " + name + "(")
	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		} else {
			// Function not in env (e.g. nested function). Build type from declaration.
			params := make([]types.Type, len(fun.Params))
			for i, p := range fun.Params {
				if p.Type != nil {
					params[i] = c.resolveTypeRef(p.Type)
				} else {
					params[i] = types.AnyType{}
				}
			}
			var ret types.Type = types.VoidType{}
			if fun.Return != nil {
				ret = c.resolveTypeRef(fun.Return)
			}
			ft = types.FuncType{Params: params, Return: ret}
			c.env.SetVar(fun.Name, ft, false)
		}
	}
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
			ts := tsType(typ)
			if ts != "" {
				c.buf.WriteString(": " + ts)
			}
		}
	}
	retType := "void"
	if ft.Return != nil {
		if ts := tsType(ft.Return); ts != "" {
			retType = ts
		}
	} else if fun.Return != nil {
		if ts := tsType(c.resolveTypeRef(fun.Return)); ts != "" {
			retType = ts
		}
	}
	c.buf.WriteString(") : " + retType + " {")
	c.buf.WriteByte('\n')
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		if i < len(ft.Params) {
			child.SetVar(p.Name, ft.Params[i], true)
		} else if p.Type != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		} else {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
	}
	origEnv := c.env
	c.env = child
	c.indent++
	for i, s := range fun.Body {
		if i == len(fun.Body)-1 {
			if s.Expr != nil {
				expr, err := c.compileExpr(s.Expr.Expr)
				if err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("return " + expr)
				continue
			}
			if s.If != nil {
				ifExpr := ifStmtToExpr(s.If)
				if ifExpr != nil {
					val, err := c.compileIfExpr(ifExpr)
					if err != nil {
						c.env = origEnv
						return err
					}
					c.writeln("return " + val)
					continue
				}
			}
		}
		if err := c.compileStmt(s); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.indent--
	c.env = origEnv
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("function " + name + "(): void {\n")
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
		return "", fmt.Errorf("nil binary expression")
	}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []string{first}
	typesList := []types.Type{c.inferUnaryType(b.Left)}
	ops := []string{}

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		typesList = append(typesList, c.inferPostfixType(part.Right))
		op := part.Op
		if part.All {
			op = op + "_all"
		}
		ops = append(ops, op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
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
	switch op {
	case "+", "-", "*", "/", "%":
		if op == "+" {
			if (isList(leftType) && isList(rightType)) ||
				(isList(leftType) && isAny(rightType)) ||
				(isAny(leftType) && isList(rightType)) {
				return fmt.Sprintf("%s.concat(%s)", left, right), leftType, nil
			}
		}
		if op == "+" && isString(leftType) && isString(rightType) {
			return fmt.Sprintf("%s + %s", left, right), types.StringType{}, nil
		}
		if op == "/" && ((isInt(leftType) || isInt64(leftType)) || (isInt(rightType) || isInt64(rightType))) {
			return fmt.Sprintf("Math.trunc(%s / %s)", left, right), types.IntType{}, nil
		}
		return fmt.Sprintf("(%s %s %s)", left, op, right), leftType, nil
	case "==", "!=":
		if isList(leftType) || isList(rightType) || isMap(leftType) || isMap(rightType) || isStruct(leftType) || isStruct(rightType) || isAny(leftType) || isAny(rightType) {
			c.use("_equal")
			if op == "==" {
				return fmt.Sprintf("_equal(%s, %s)", left, right), types.BoolType{}, nil
			}
			return fmt.Sprintf("!_equal(%s, %s)", left, right), types.BoolType{}, nil
		}
		return fmt.Sprintf("(%s %s %s)", left, op, right), types.BoolType{}, nil
	case "<", "<=", ">", ">=":
		return fmt.Sprintf("(%s %s %s)", left, op, right), types.BoolType{}, nil
	case "&&", "||":
		return fmt.Sprintf("(%s %s %s)", left, op, right), types.BoolType{}, nil
	case "in":
		switch rightType.(type) {
		case types.MapType:
			return fmt.Sprintf("Object.prototype.hasOwnProperty.call(%s, String(%s))", right, left), types.BoolType{}, nil
		case types.ListType, types.StringType:
			return fmt.Sprintf("%s.includes(%s)", right, left), types.BoolType{}, nil
		default:
			c.use("_contains")
			return fmt.Sprintf("_contains(%s, %s)", right, left), types.BoolType{}, nil
		}
	case "union", "union_all", "except", "intersect":
		c.use("_" + op)
		return fmt.Sprintf("_%s(%s, %s)", op, left, right), types.ListType{Elem: types.AnyType{}}, nil
	default:
		return fmt.Sprintf("(%s %s %s)", left, op, right), types.AnyType{}, nil
	}
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		val = fmt.Sprintf("(%s%s)", op, val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	typ := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		if op.Index == nil {
			if op.Call != nil {
				// check for method call on a selector expression
				if sel := p.Target.Selector; sel != nil && len(sel.Tail) >= 1 {
					method := sel.Tail[len(sel.Tail)-1]
					recvSel := &parser.SelectorExpr{Root: sel.Root, Tail: sel.Tail[:len(sel.Tail)-1]}
					recvExpr := &parser.Primary{Selector: recvSel}
					recvTyp := c.inferPrimaryType(recvExpr)
					recvCode, err2 := c.compilePrimary(recvExpr)
					if err2 != nil {
						return "", err2
					}
					rootExpr := &parser.Primary{Selector: &parser.SelectorExpr{Root: sel.Root}}
					rootTyp := c.inferPrimaryType(rootExpr)
					root := sanitizeName(sel.Root)
					switch method {
					case "keys":
						if len(op.Call.Args) == 0 {
							if mt, ok := rootTyp.(types.MapType); ok {
								if isInt(mt.Key) || isInt64(mt.Key) || isFloat(mt.Key) {
									expr = fmt.Sprintf("Object.keys(%s).map(k => Number(k))", root)
								} else {
									expr = fmt.Sprintf("Object.keys(%s)", root)
								}
								typ = types.ListType{Elem: types.AnyType{}}
								continue
							}
						}
					case "contains":
						if len(op.Call.Args) == 1 {
							arg, err := c.compileExpr(op.Call.Args[0])
							if err != nil {
								return "", err
							}
							if _, ok := recvTyp.(types.StringType); ok {
								expr = fmt.Sprintf("%s.includes(%s)", recvCode, arg)
								typ = types.BoolType{}
								continue
							}
						}
					case "ToUpper":
						if len(op.Call.Args) == 1 {
							arg, err := c.compileExpr(op.Call.Args[0])
							if err != nil {
								return "", err
							}
							expr = fmt.Sprintf("String(%s).toUpperCase()", arg)
							typ = types.StringType{}
							continue
						}
					}
				}
				args := make([]string, len(op.Call.Args))
				for i, a := range op.Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				if strings.HasSuffix(expr, ".contains") && len(args) == 1 {
					recv := strings.TrimSuffix(expr, ".contains")
					expr = fmt.Sprintf("%s.includes(%s)", recv, args[0])
					typ = types.BoolType{}
				} else {
					expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
					typ = c.inferPostfixType(&parser.PostfixExpr{Target: &parser.Primary{Call: nil}})
				}
			}
			continue
		}
		idx := op.Index
		if idx.Colon != nil {
			start := "0"
			end := "0"
			if idx.Start != nil {
				start, err = c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
			}
			if idx.End != nil {
				end, err = c.compileExpr(idx.End)
				if err != nil {
					return "", err
				}
			}
			switch tt := typ.(type) {
			case types.ListType:
				if idx.Start == nil {
					start = "0"
				}
				if idx.End == nil {
					end = fmt.Sprintf("%s.length", expr)
				}
				expr = fmt.Sprintf("%s.slice(%s, %s)", expr, start, end)
				typ = tt
			case types.StringType:
				if idx.Start == nil {
					start = "0"
				}
				if idx.End == nil {
					end = fmt.Sprintf("%s.length", expr)
				}
				c.use("_sliceString")
				expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				typ = types.StringType{}
			default:
				c.use("_slice")
				expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
				typ = types.AnyType{}
			}
		} else {
			idxExpr, err := c.compileExpr(idx.Start)
			if err != nil {
				return "", err
			}
			switch tt := typ.(type) {
			case types.ListType:
				expr = fmt.Sprintf("%s[%s]", expr, idxExpr)
				typ = tt.Elem
			case types.MapType:
				expr = fmt.Sprintf("%s[%s]", expr, idxExpr)
				typ = tt.Value
			case types.StringType:
				c.use("_indexString")
				expr = fmt.Sprintf("_indexString(%s, %s)", expr, idxExpr)
				typ = types.StringType{}
			default:
				expr = fmt.Sprintf("(%s as any)[%s]", expr, idxExpr)
				typ = types.AnyType{}
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.List != nil:
		return c.compileListLiteral(p.List)
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
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
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Selector != nil:
		if c.env != nil && len(p.Selector.Tail) == 0 {
			if _, ok := c.env.FindUnionByVariant(p.Selector.Root); ok {
				return fmt.Sprintf("{ __name: \"%s\" }", p.Selector.Root), nil
			}
		}
		expr := sanitizeName(p.Selector.Root)
		for _, s := range p.Selector.Tail {
			expr += "." + sanitizeName(s)
		}
		return expr, nil
	case p.Struct != nil:
		if c.env != nil {
			if _, ok := c.env.GetAgent(p.Struct.Name); ok {
				if len(p.Struct.Fields) > 0 {
					return "", fmt.Errorf("agent initialization with fields not supported")
				}
				return fmt.Sprintf("New%s()", sanitizeName(p.Struct.Name)), nil
			}
			if _, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
				parts := make([]string, 0, len(p.Struct.Fields)+1)
				parts = append(parts, fmt.Sprintf("__name: \"%s\"", p.Struct.Name))
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					parts = append(parts, fmt.Sprintf("%s: %s", sanitizeName(f.Name), v))
				}
				inner := strings.Join(parts, ", ")
				return "{" + inner + "}", nil
			}
		}
		parts := make([]string, len(p.Struct.Fields))
		multiline := len(p.Struct.Fields) > 1
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			if strings.Contains(v, "\n") {
				multiline = true
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		if !multiline {
			return "{" + strings.Join(parts, ", ") + "}", nil
		}
		inner := indentBlock(strings.Join(parts, ",\n"), c.indent+1)
		return "{\n" + inner + strings.Repeat(indentStr, c.indent) + "}", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	default:
		return "", fmt.Errorf("invalid primary expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	if lit, ok := interpreter.EvalPureCall(call, c.env); ok {
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
	case "strings.ToUpper":
		if len(args) != 1 {
			return "", fmt.Errorf("ToUpper expects 1 arg")
		}
		return fmt.Sprintf("String(%s).toUpperCase()", args[0]), nil
	case "strings.ToLower":
		if len(args) != 1 {
			return "", fmt.Errorf("ToLower expects 1 arg")
		}
		return fmt.Sprintf("String(%s).toLowerCase()", args[0]), nil
	case "lower":
		if len(args) != 1 {
			return "", fmt.Errorf("lower expects 1 arg")
		}
		return fmt.Sprintf("String(%s).toLowerCase()", args[0]), nil
	case "upper":
		if len(args) != 1 {
			return "", fmt.Errorf("upper expects 1 arg")
		}
		return fmt.Sprintf("String(%s).toUpperCase()", args[0]), nil
	}
	switch call.Func {
	case "print":
		c.use("_fmt")
		return fmt.Sprintf("console.log(_fmt(%s))", argStr), nil
	case "keys":
		if len(call.Args) == 1 {
			t := c.inferExprType(call.Args[0])
			if mt, ok := t.(types.MapType); ok {
				if isInt(mt.Key) || isInt64(mt.Key) || isFloat(mt.Key) {
					return fmt.Sprintf("Object.keys(%s).map(k => Number(k))", args[0]), nil
				}
			}
			return fmt.Sprintf("Object.keys(%s)", args[0]), nil
		}
		return "Object.keys()", nil
	case "len":
		if len(call.Args) == 1 {
			t := c.inferExprType(call.Args[0])
			switch t.(type) {
			case types.ListType, types.StringType:
				return fmt.Sprintf("%s.length", args[0]), nil
			case types.MapType, types.StructType, types.UnionType:
				return fmt.Sprintf("Object.keys(%s).length", args[0]), nil
			default:
				a := args[0]
				return fmt.Sprintf("(Array.isArray(%[1]s) || typeof %[1]s === 'string' ? (%[1]s as any).length : (%[1]s && typeof %[1]s === 'object' ? Object.keys(%[1]s).length : 0))", a), nil
			}
		}
		return fmt.Sprintf("(Array.isArray(%[1]s) || typeof %[1]s === 'string' ? (%[1]s as any).length : (%[1]s && typeof %[1]s === 'object' ? Object.keys(%[1]s).length : 0))", argStr), nil
	case "str":
		return fmt.Sprintf("String(%s)", argStr), nil
	case "input":
		c.use("_input")
		return "_input()", nil
	case "count":
		c.use("_count")
		return fmt.Sprintf("_count(%s)", argStr), nil
	case "append":
		c.use("_append")
		if len(args) == 2 {
			return fmt.Sprintf("_append(%s, %s)", args[0], args[1]), nil
		}
		return fmt.Sprintf("_append(%s)", argStr), nil
	case "contains":
		if len(args) != 2 {
			return "", fmt.Errorf("contains expects 2 args")
		}
		c.use("_contains")
		return fmt.Sprintf("_contains(%s, %s)", args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		c.use("_values")
		return fmt.Sprintf("_values(%s)", args[0]), nil
	case "exists":
		c.use("_exists")
		return fmt.Sprintf("_exists(%s)", argStr), nil
	case "avg":
		c.use("_avg")
		c.use("_count")
		c.use("_sum")
		return fmt.Sprintf("_avg(%s)", argStr), nil
	case "reduce":
		if len(args) != 3 {
			return "", fmt.Errorf("reduce expects 3 args")
		}
		c.use("_reduce")
		return fmt.Sprintf("_reduce(%s, %s, %s)", args[0], args[1], args[2]), nil
	case "sum":
		c.use("_sum")
		return fmt.Sprintf("_sum(%s)", argStr), nil
	case "min":
		c.use("_min")
		return fmt.Sprintf("_min(%s)", argStr), nil
	case "max":
		c.use("_max")
		return fmt.Sprintf("_max(%s)", argStr), nil
	case "floor":
		if len(args) == 1 {
			return fmt.Sprintf("Math.floor(%s)", args[0]), nil
		}
		return "", fmt.Errorf("floor expects 1 arg")
	case "ceil":
		if len(args) == 1 {
			return fmt.Sprintf("Math.ceil(%s)", args[0]), nil
		}
		return "", fmt.Errorf("ceil expects 1 arg")
	case "concat":
		if len(args) == 0 {
			return "[]", nil
		}
		return fmt.Sprintf("[].concat(%s)", strings.Join(args, ", ")), nil
	case "substr", "substring":
		if len(args) == 3 {
			return fmt.Sprintf("(%s).substring(%s, (%s)+(%s))", args[0], args[1], args[1], args[2]), nil
		}
		return fmt.Sprintf("(%s).substring(%s)", args[0], args[1]), nil
	case "reverse":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			if _, ok := t.(types.StringType); ok {
				return fmt.Sprintf("%s.split('') .reverse().join('')", args[0]), nil
			}
			return fmt.Sprintf("%s.slice().reverse()", args[0]), nil
		}
		return fmt.Sprintf("[].concat(%s).reverse()", strings.Join(args, ", ")), nil
	case "first":
		if len(args) == 1 {
			return fmt.Sprintf("(%s)[0]", args[0]), nil
		}
		return fmt.Sprintf("(%s)[0]", argStr), nil
	case "now":
		// performance.now() returns milliseconds as a float. Multiply
		// by 1e6 so that `now()` is consistent with Go's UnixNano()
		// and the interpreter which return nanoseconds.
		return "performance.now() * 1000000", nil
	case "json":
		c.use("_json")
		return fmt.Sprintf("console.log(_json(%s))", argStr), nil
	default:
		if fn, ok := c.env.GetFunc(call.Func); ok {
			if len(call.Args) < len(fn.Params) {
				missing := fn.Params[len(call.Args):]
				vars := make([]string, len(missing))
				for i, p := range missing {
					vars[i] = sanitizeName(p.Name)
				}
				allArgs := append(args, vars...)
				return fmt.Sprintf("(%s) => %s(%s)", strings.Join(vars, ", "), sanitizeName(call.Func), strings.Join(allArgs, ", ")), nil
			}
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		part := sanitizeName(p.Name)
		if p.Type != nil {
			part += ": " + tsType(c.resolveTypeRef(p.Type))
		}
		params[i] = part
	}
	retType := "any"
	if fn.Return != nil {
		if ts := tsType(c.resolveTypeRef(fn.Return)); ts != "" {
			retType = ts
		}
	} else if fn.ExprBody != nil {
		t := c.inferExprType(fn.ExprBody)
		if ts := tsType(t); ts != "" {
			retType = ts
		}
	} else if n := len(fn.BlockBody); n > 0 {
		last := fn.BlockBody[n-1]
		if last.Return != nil {
			t := c.inferExprType(last.Return.Value)
			if ts := tsType(t); ts != "" {
				retType = ts
			}
		} else if last.Expr != nil {
			t := c.inferExprType(last.Expr.Expr)
			if ts := tsType(t); ts != "" {
				retType = ts
			}
		}
	}
	sub := &Compiler{
		helpers:     c.helpers,
		imports:     c.imports,
		env:         c.env,
		globals:     make(map[string]string),
		moduleScope: false,
	}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		for i, s := range fn.BlockBody {
			if i == len(fn.BlockBody)-1 {
				if s.Expr != nil {
					expr, err := sub.compileExpr(s.Expr.Expr)
					if err != nil {
						return "", err
					}
					sub.writeln("return " + expr)
					continue
				}
				if s.If != nil {
					ifExpr := ifStmtToExpr(s.If)
					if ifExpr != nil {
						expr, err := sub.compileIfExpr(ifExpr)
						if err != nil {
							return "", err
						}
						sub.writeln("return " + expr)
						continue
					}
				}
			}
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	code := "function(" + strings.Join(params, ", ") + "): " + retType + " {\n" + body + "}"
	// Block expression with no params behaves like an IIFE
	if len(fn.Params) == 0 && fn.ExprBody == nil {
		return "(() => {\n" + body + "})()", nil
	}
	return code, nil
}

func (c *Compiler) compileListLiteral(l *parser.ListLiteral) (string, error) {
	elems := make([]string, len(l.Elems))
	multiline := len(l.Elems) > 1
	for i, e := range l.Elems {
		v, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		if strings.Contains(v, "\n") {
			multiline = true
		}
		elems[i] = v
	}
	if !multiline {
		return "[" + strings.Join(elems, ", ") + "]", nil
	}
	inner := indentBlock(strings.Join(elems, ",\n"), c.indent+1)
	return "[\n" + inner + strings.Repeat(indentStr, c.indent) + "]", nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	items := make([]string, len(m.Items))
	multiline := len(m.Items) > 1
	for i, it := range m.Items {
		var k string
		if s, ok := simpleStringKey(it.Key); ok {
			k = fmt.Sprintf("\"%s\"", sanitizeName(s))
		} else {
			var err error
			k, err = c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			k = fmt.Sprintf("[%s]", k)
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		if strings.Contains(v, "\n") {
			multiline = true
		}
		items[i] = fmt.Sprintf("%s: %s", k, v)
	}
	if !multiline {
		return "{" + strings.Join(items, ", ") + "}", nil
	}
	inner := indentBlock(strings.Join(items, ",\n"), c.indent+1)
	return "{\n" + inner + strings.Repeat(indentStr, c.indent) + "}", nil
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
		withStr = "undefined"
	}
	c.use("_fetch")
	return fmt.Sprintf("await _fetch(%s, %s)", urlStr, withStr), nil
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

	paramStr := "null"
	if len(params) > 0 {
		paramStr = fmt.Sprintf("{ %s }", strings.Join(params, ", "))
	}
	if model == "" {
		model = "null"
	}

	if g.Target == "embedding" {
		c.use("_gen_embed")
		return fmt.Sprintf("_gen_embed(%s, %s, %s)", text, model, paramStr), nil
	}

	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_gen_struct")
			return fmt.Sprintf("_gen_struct<%s>(%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
		}
	}
	c.use("_gen_text")
	return fmt.Sprintf("_gen_text(%s, %s, %s)", prompt, model, paramStr), nil

}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "undefined"
	if l.With != nil {
		w, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		opts = fmt.Sprintf("_toAnyMap(%s)", w)
	}
	c.use("_dataset")
	expr := fmt.Sprintf("_load(%s, %s)", path, opts)
	if l.Type != nil {
		t := c.resolveTypeRef(l.Type)
		ts := tsType(t)
		if ts != "" {
			expr = fmt.Sprintf("%s as Array<%s>", expr, ts)
		}
	}
	return expr, nil
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
	opts := "undefined"
	if s.With != nil {
		w, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		opts = fmt.Sprintf("_toAnyMap(%s)", w)
	}
	c.use("_dataset")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

// compileIterExpr compiles an expression expected to be iterated over. If the
// expression evaluates to a group value, its underlying item slice is used.
func (c *Compiler) compileIterExpr(e *parser.Expr) (string, error) {
	expr, err := c.compileExpr(e)
	if err != nil {
		return "", err
	}
	if _, ok := c.inferExprType(e).(types.GroupType); ok {
		expr += ".items"
	}
	return expr, nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileIterExpr(q.Source)
	if err != nil {
		return "", err
	}

	origWhere, origSkip, origTake := q.Where, q.Skip, q.Take
	defer func() { q.Where, q.Skip, q.Take = origWhere, origSkip, origTake }()

	if l, ok := loadExpr(q.Source); ok && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Group == nil {
		opts := []string{}
		if l.With != nil {
			w, err := c.compileExpr(l.With)
			if err != nil {
				return "", err
			}
			c.use("_toAnyMap")
			opts = append(opts, "..._toAnyMap("+w+")")
		}
		pushed := false
		if q.Where != nil {
			if f, v, ok := c.eqFilter(q.Where, q.Var); ok {
				opts = append(opts, fmt.Sprintf("filter: { %s: %s }", f, v))
				q.Where = nil
				pushed = true
			}
		}
		if q.Skip != nil {
			sk, err := c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
			opts = append(opts, "skip: "+sk)
			q.Skip = nil
			pushed = true
		}
		if q.Take != nil {
			tk, err := c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			opts = append(opts, "take: "+tk)
			q.Take = nil
			pushed = true
		}
		if pushed {
			c.use("_toAnyMap")
			path := "null"
			if l.Path != nil {
				path = fmt.Sprintf("%q", *l.Path)
			}
			src = fmt.Sprintf("_load(%s, _toAnyMap({%s}))", path, strings.Join(opts, ", "))
		}
	}

	needsHelper := false
	for _, j := range q.Joins {
		if j.Side != nil {
			needsHelper = true
			break
		}
	}
	if !needsHelper {
		simple := q.Sort == nil && q.Skip == nil && q.Take == nil
		group := q.Group != nil

		if simple && !group && len(q.Froms) == 0 && len(q.Joins) == 0 {
			child := types.NewEnv(c.env)
			var elemType types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
				elemType = lt.Elem
			}
			child.SetVar(q.Var, elemType, true)
			orig := c.env
			c.env = child
			val, err := c.compileExpr(q.Select)
			if err != nil {
				c.env = orig
				return "", err
			}
			var cond string
			if q.Where != nil {
				cond, err = c.compileExpr(q.Where)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			c.env = orig
			var b strings.Builder
			b.WriteString(src)
			if cond != "" {
				b.WriteString(fmt.Sprintf(".filter(%s => (%s))", sanitizeName(q.Var), cond))
			}
			mapped := val
			trimmed := strings.TrimSpace(val)
			if strings.HasPrefix(trimmed, "{") || strings.HasPrefix(trimmed, "[") || strings.Contains(trimmed, "\n") {
				mapped = "(" + val + ")"
			}
			b.WriteString(fmt.Sprintf(".map(%s => %s)", sanitizeName(q.Var), mapped))
			return b.String(), nil
		}

		var b strings.Builder
		b.WriteString("(() => {\n")
		b.WriteString("\tconst _src = " + src + ";\n")
		if group {
			b.WriteString("\tconst _map = new Map<string, any>();\n")
			b.WriteString("\tconst _order: string[] = [];\n")
		}
		if simple && !group {
			b.WriteString("\tconst _res = [];\n")
		} else {
			b.WriteString("\tvar _items = [];")
			b.WriteString("\n")
		}
		b.WriteString("\tfor (const " + sanitizeName(q.Var) + " of _src) {\n")
		child := types.NewEnv(c.env)
		var elemType types.Type = types.AnyType{}
		if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
			elemType = lt.Elem
		}
		child.SetVar(q.Var, elemType, true)
		for _, f := range q.Froms {
			ft := c.inferExprType(f.Src)
			var fe types.Type = types.AnyType{}
			if lt, ok := ft.(types.ListType); ok {
				fe = lt.Elem
			}
			child.SetVar(f.Var, fe, true)
		}
		for _, j := range q.Joins {
			jt := c.inferExprType(j.Src)
			var je types.Type = types.AnyType{}
			if lt, ok := jt.(types.ListType); ok {
				je = lt.Elem
			}
			child.SetVar(j.Var, je, true)
		}
		orig := c.env
		c.env = child
		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileIterExpr(f.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			fromSrcs[i] = fs
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileIterExpr(j.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinOns[i] = on
		}
		var keyExpr string
		var val string
		if group {
			keyExpr, err = c.compileExpr(q.Group.Exprs[0])
			if err != nil {
				c.env = orig
				return "", err
			}
			genv := types.NewEnv(child)
			genv.SetVar(q.Group.Name, types.GroupType{Elem: elemType}, true)
			c.env = genv
			val, err = c.compileExpr(q.Select)
			if err != nil {
				c.env = orig
				return "", err
			}
			c.env = child
		} else {
			val, err = c.compileExpr(q.Select)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		indent := indentStr + indentStr
		for i := range q.Froms {
			fvar := sanitizeName(q.Froms[i].Var)
			b.WriteString(indent + "for (const " + fvar + " of " + fromSrcs[i] + ") {\n")
			indent += indentStr
		}
		for i := range q.Joins {
			jvar := sanitizeName(q.Joins[i].Var)
			b.WriteString(indent + "for (const " + jvar + " of " + joinSrcs[i] + ") {\n")
			indent += indentStr
			b.WriteString(indent + "if (!(" + joinOns[i] + ")) { continue }\n")
		}
		if q.Where != nil {
			cond, err := c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
			b.WriteString(indent + "if (!(" + cond + ")) { continue }\n")
		}
		if group {
			b.WriteString(indent + "const _key = " + keyExpr + ";\n")
			b.WriteString(indent + "const _ks = JSON.stringify(_key);\n")
			b.WriteString(indent + "let _g = _map.get(_ks);\n")
			b.WriteString(indent + "if (!_g) { _g = { key: _key, items: [] }; _map.set(_ks, _g); _order.push(_ks); }\n")
			b.WriteString(indent + "_g.items.push({")
			parts := []string{}
			vars := []string{sanitizeName(q.Var)}
			for _, f := range q.Froms {
				vars = append(vars, sanitizeName(f.Var))
			}
			for _, j := range q.Joins {
				vars = append(vars, sanitizeName(j.Var))
			}
			for _, v := range vars {
				parts = append(parts, "..."+v)
			}
			for _, v := range vars {
				parts = append(parts, v+": "+v)
			}
			b.WriteString(strings.Join(parts, ", "))
			b.WriteString("});\n")
		} else if simple {
			b.WriteString(indent + "_res.push(" + val + ")\n")
		} else {
			b.WriteString(indent + "_items.push(" + sanitizeName(q.Var) + ");\n")
		}
		for i := len(q.Joins) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-len(indentStr)]
			b.WriteString(indent + "}\n")
		}
		for i := len(q.Froms) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-len(indentStr)]
			b.WriteString(indent + "}\n")
		}
		b.WriteString(indentStr + "}\n")

		if group {
			b.WriteString("\tlet _groups = _order.map(k => _map.get(k)!);\n")
			if simple {
				b.WriteString("\tconst _res = [];\n")
				b.WriteString("\tfor (const " + sanitizeName(q.Group.Name) + " of _groups) {\n")
				b.WriteString("\t\t_res.push(" + val + ")\n")
				b.WriteString("\t}\n")
				b.WriteString("\treturn _res;\n")
				b.WriteString("})()")
				c.env = orig
				return b.String(), nil
			}
			b.WriteString("\tvar _items = _groups;\n")
		} else if simple {
			b.WriteString("\treturn _res;\n")
			b.WriteString("})()")
			c.env = orig
			return b.String(), nil
		}

		var sortExpr string
		if q.Sort != nil {
			if group {
				genv := types.NewEnv(child)
				genv.SetVar(q.Group.Name, types.GroupType{Elem: elemType}, true)
				c.env = genv
			}
			sortExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				c.env = orig
				return "", err
			}
			var sv string
			if group {
				sv = sanitizeName(q.Group.Name)
			} else {
				sv = sanitizeName(q.Var)
			}
			b.WriteString("\tlet _pairs = _items.map(it => { const " + sv + " = it; return {item: it, key: " + sortExpr + "}; });\n")
			b.WriteString("\t_pairs.sort((a, b) => {\n")
			b.WriteString("\t\tconst ak = a.key; const bk = b.key;\n")
			b.WriteString("\t\tif (typeof ak === 'number' && typeof bk === 'number') return ak - bk;\n")
			b.WriteString("\t\tif (typeof ak === 'string' && typeof bk === 'string') return ak < bk ? -1 : (ak > bk ? 1 : 0);\n")
			b.WriteString("\t\treturn String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);\n")
			b.WriteString("\t});\n")
			b.WriteString("\t_items = _pairs.map(p => p.item);\n")
		}

		if q.Skip != nil {
			sk, err := c.compileExpr(q.Skip)
			if err != nil {
				c.env = orig
				return "", err
			}
			b.WriteString("\t{ const _n = " + sk + "; _items = _n < _items.length ? _items.slice(_n) : []; }\n")
		}

		if q.Take != nil {
			tk, err := c.compileExpr(q.Take)
			if err != nil {
				c.env = orig
				return "", err
			}
			b.WriteString("\t{ const _n = " + tk + "; if (_n < _items.length) _items = _items.slice(0, _n); }\n")
		}

		c.env = orig
		b.WriteString("\tconst _res = [];\n")
		var rv string
		if group {
			rv = sanitizeName(q.Group.Name)
		} else {
			rv = sanitizeName(q.Var)
		}
		b.WriteString("\tfor (const " + rv + " of _items) {\n")
		b.WriteString("\t\t_res.push(" + val + ")\n")
		b.WriteString("\t}\n")
		b.WriteString("\treturn _res;\n")
		b.WriteString("})()")
		return b.String(), nil
	}

	// specialized loop implementation for single joins with a side
	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil &&
		q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil {
		j := q.Joins[0]
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		child := types.NewEnv(c.env)
		var elemType types.Type = types.AnyType{}
		if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
			elemType = lt.Elem
		}
		child.SetVar(q.Var, elemType, true)
		var je types.Type = types.AnyType{}
		if lt, ok := c.inferExprType(j.Src).(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
		orig := c.env
		c.env = child
		joinSrc, err := c.compileIterExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		onExpr, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		val, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig

		qv := sanitizeName(q.Var)
		jv := sanitizeName(j.Var)
		var b strings.Builder
		b.WriteString("(() => {\n")
		b.WriteString("\tconst _src = " + src + ";\n")
		b.WriteString("\tconst _join = " + joinSrc + ";\n")
		if side == "outer" {
			b.WriteString("\tconst _matched = new Array(_join.length).fill(false);\n")
		}
		b.WriteString("\tconst _res = [];\n")
		if side == "right" {
			b.WriteString("\tfor (const " + jv + " of _join) {\n")
			b.WriteString("\t\tlet _m = false;\n")
			b.WriteString("\t\tfor (const " + qv + " of _src) {\n")
			b.WriteString("\t\t\tif (!(" + onExpr + ")) continue;\n")
			b.WriteString("\t\t\t_m = true;\n")
			b.WriteString("\t\t\t_res.push(" + val + ");\n")
			b.WriteString("\t\t}\n")
			b.WriteString("\t\tif (!_m) {\n")
			b.WriteString("\t\t\tconst " + qv + " = null;\n")
			b.WriteString("\t\t\t_res.push(" + val + ");\n")
			b.WriteString("\t\t}\n")
			b.WriteString("\t}\n")
		} else {
			b.WriteString("\tfor (const " + qv + " of _src) {\n")
			if side != "" {
				b.WriteString("\t\tlet _m = false;\n")
			}
			if side == "outer" {
				b.WriteString("\t\tfor (let _ri = 0; _ri < _join.length; _ri++) {\n")
				b.WriteString("\t\t\tconst " + jv + " = _join[_ri];\n")
			} else {
				b.WriteString("\t\tfor (const " + jv + " of _join) {\n")
			}
			b.WriteString("\t\t\tif (!(" + onExpr + ")) continue;\n")
			if side == "outer" {
				b.WriteString("\t\t\t_matched[_ri] = true;\n")
			}
			if side != "" {
				b.WriteString("\t\t\t_m = true;\n")
			}
			b.WriteString("\t\t\t_res.push(" + val + ");\n")
			b.WriteString("\t\t}\n")
			if side == "outer" {
				b.WriteString("\t\tif (!_m) {\n")
				b.WriteString("\t\t\tconst " + jv + " = null;\n")
				b.WriteString("\t\t\t_res.push(" + val + ");\n")
				b.WriteString("\t\t}\n")
				b.WriteString("\t}\n")
				b.WriteString("\tfor (let _ri = 0; _ri < _join.length; _ri++) {\n")
				b.WriteString("\t\tif (!_matched[_ri]) {\n")
				b.WriteString("\t\t\tconst " + qv + " = null;\n")
				b.WriteString("\t\t\tconst " + jv + " = _join[_ri];\n")
				b.WriteString("\t\t\t_res.push(" + val + ");\n")
				b.WriteString("\t\t}\n")
				b.WriteString("\t}\n")
			} else if side == "left" {
				b.WriteString("\t\tif (!_m) {\n")
				b.WriteString("\t\t\tconst " + jv + " = null;\n")
				b.WriteString("\t\t\t_res.push(" + val + ");\n")
				b.WriteString("\t\t}\n")
				b.WriteString("\t}\n")
			} else {
				// inner join fallback if side is empty
				b.WriteString("\t}\n")
			}
		}
		b.WriteString("\treturn _res;\n")
		b.WriteString("})()")
		return b.String(), nil
	}

	// helper-based implementation for joins with sides
	child := types.NewEnv(c.env)
	varNames := []string{sanitizeName(q.Var)}
	var elemType types.Type = types.AnyType{}
	if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
		elemType = lt.Elem
	}
	child.SetVar(q.Var, elemType, true)
	for _, f := range q.Froms {
		ft := c.inferExprType(f.Src)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
	}
	orig := c.env
	c.env = child
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileIterExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		fromSrcs[i] = fs
		varNames = append(varNames, sanitizeName(f.Var))
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	paramCopy := append([]string(nil), varNames...)
	for i, j := range q.Joins {
		js, err := c.compileIterExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinSrcs[i] = js
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinOns[i] = on
		varNames = append(varNames, sanitizeName(j.Var))
	}
	var keyExpr string
	var val string
	if q.Group != nil {
		keyExpr, err = c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: elemType}, true)
		c.env = genv
		val, err = c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = child
	} else {
		val, err = c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	var whereExpr, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		if q.Group != nil {
			genv := types.NewEnv(child)
			genv.SetVar(q.Group.Name, types.GroupType{Elem: elemType}, true)
			c.env = genv
		}
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig

	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	params := []string{sanitizeName(q.Var)}
	for i, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("{ items: %s }", fs))
		params = append(params, sanitizeName(q.Froms[i].Var))
	}
	paramCopy = append([]string(nil), params...)
	for i, js := range joinSrcs {
		onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
		spec := fmt.Sprintf("{ items: %s, on: (%s) => (%s)", js, strings.Join(onParams, ", "), joinOns[i])
		if q.Joins[i].Side != nil {
			side := *q.Joins[i].Side
			if side == "left" || side == "outer" {
				spec += ", left: true"
			}
			if side == "right" || side == "outer" {
				spec += ", right: true"
			}
		}
		spec += " }"
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}

	allParams := strings.Join(paramCopy, ", ")
	if q.Group != nil {
		selectFn := fmt.Sprintf("(%s) => [%s]", allParams, allParams)
		var whereFn string
		if whereExpr != "" {
			whereFn = fmt.Sprintf("(%s) => (%s)", allParams, whereExpr)
		}
		var b strings.Builder
		b.WriteString("(() => {\n")
		b.WriteString("\tconst _src = " + src + ";\n")
		b.WriteString("\tlet _items = _query(_src, [\n")
		for i, j := range joins {
			b.WriteString("\t\t" + j)
			if i != len(joins)-1 {
				b.WriteString(",")
			}
			b.WriteString("\n")
		}
		b.WriteString("\t], { select: " + selectFn)
		if whereFn != "" {
			b.WriteString(", where: " + whereFn)
		}
		b.WriteString(" });\n")
		b.WriteString("\tconst _map = new Map<string, any>();\n")
		b.WriteString("\tconst _order: string[] = [];\n")
		b.WriteString("\tfor (const _r of _items) {\n")
		b.WriteString("\t\tconst [" + allParams + "] = _r;\n")
		b.WriteString("\t\tconst _key = " + keyExpr + ";\n")
		b.WriteString("\t\tconst _ks = JSON.stringify(_key);\n")
		b.WriteString("\t\tlet _g = _map.get(_ks);\n")
		b.WriteString("\t\tif (!_g) { _g = { key: _key, items: [] }; _map.set(_ks, _g); _order.push(_ks); }\n")
		b.WriteString("\t\t_g.items.push({")
		parts := []string{}
		vars := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			vars = append(vars, sanitizeName(f.Var))
		}
		for _, j := range q.Joins {
			vars = append(vars, sanitizeName(j.Var))
		}
		for _, v := range vars {
			parts = append(parts, "..."+v)
		}
		for _, v := range vars {
			parts = append(parts, v+": "+v)
		}
		b.WriteString(strings.Join(parts, ", "))
		b.WriteString("});\n")
		b.WriteString("\t}\n")
		b.WriteString("\tlet _itemsG = _order.map(k => _map.get(k)!);\n")
		if sortExpr != "" {
			b.WriteString("\tlet _pairs = _itemsG.map(it => { const " + sanitizeName(q.Group.Name) + " = it; return {item: it, key: " + sortExpr + "}; });\n")
			b.WriteString("\t_pairs.sort((a, b) => {\n")
			b.WriteString("\t\tconst ak = a.key; const bk = b.key;\n")
			b.WriteString("\t\tif (typeof ak === 'number' && typeof bk === 'number') return ak - bk;\n")
			b.WriteString("\t\tif (typeof ak === 'string' && typeof bk === 'string') return ak < bk ? -1 : (ak > bk ? 1 : 0);\n")
			b.WriteString("\t\treturn String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);\n")
			b.WriteString("\t});\n")
			b.WriteString("\t_itemsG = _pairs.map(p => p.item);\n")
		}
		if skipExpr != "" {
			b.WriteString("\t{ const _n = " + skipExpr + "; _itemsG = _n < _itemsG.length ? _itemsG.slice(_n) : []; }\n")
		}
		if takeExpr != "" {
			b.WriteString("\t{ const _n = " + takeExpr + "; if (_n < _itemsG.length) _itemsG = _itemsG.slice(0, _n); }\n")
		}
		b.WriteString("\tconst _res = [];\n")
		b.WriteString("\tfor (const " + sanitizeName(q.Group.Name) + " of _itemsG) {\n")
		b.WriteString("\t\t_res.push(" + val + ")\n")
		b.WriteString("\t}\n")
		b.WriteString("\treturn _res;\n")
		b.WriteString("})()")
		c.use("_query")
		return b.String(), nil
	}

	mappedVal := val
	trimmedVal := strings.TrimSpace(val)
	if strings.HasPrefix(trimmedVal, "{") || strings.HasPrefix(trimmedVal, "[") || strings.Contains(trimmedVal, "\n") {
		mappedVal = "(" + val + ")"
	}
	selectFn := fmt.Sprintf("(%s) => %s", allParams, mappedVal)
	var whereFn, sortFn string
	if whereExpr != "" {
		whereFn = fmt.Sprintf("(%s) => (%s)", allParams, whereExpr)
	}
	if sortExpr != "" {
		sortFn = fmt.Sprintf("(%s) => (%s)", allParams, sortExpr)
	}

	var b strings.Builder
	b.WriteString("(() => {\n")
	b.WriteString("\tconst _src = " + src + ";\n")
	b.WriteString("\treturn _query(_src, [\n")
	for i, j := range joins {
		b.WriteString("\t\t" + j)
		if i != len(joins)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString("\t], { select: " + selectFn)
	if whereFn != "" {
		b.WriteString(", where: " + whereFn)
	}
	if sortFn != "" {
		b.WriteString(", sortKey: " + sortFn)
	}
	if skipExpr != "" {
		b.WriteString(", skip: " + skipExpr)
	}
	if takeExpr != "" {
		b.WriteString(", take: " + takeExpr)
	}
	b.WriteString(" });\n")
	b.WriteString("})()")
	c.use("_query")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(() => {\n")
	b.WriteString("\tconst _t = " + target + ";\n")
	for _, cs := range m.Cases {
		r, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + r + "\n")
			b.WriteString("})()")
			return b.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("_t.__name === \"%s\"", call.Func)
				names := []string{}
				values := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok {
						if id == "_" {
							continue
						}
						names = append(names, sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						values = append(values, fmt.Sprintf("_t.%s", field))
					}
				}
				if len(names) > 0 {
					r = fmt.Sprintf("((%s) => %s)(%s)", strings.Join(names, ", "), r, strings.Join(values, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("_t.__name === \"%s\"", ident)
			}
		}
		if cond == "" {
			p, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			c.use("_equal")
			cond = fmt.Sprintf("_equal(_t, %s)", p)
		}
		b.WriteString(fmt.Sprintf("\tif (%s) { return %s }\n", cond, r))
	}
	b.WriteString("\treturn undefined\n")
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return strconv.FormatFloat(*l.Float, 'g', -1, 64), nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Null:
		return "null", nil
	default:
		return "null", fmt.Errorf("invalid literal")
	}
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

func loadExpr(e *parser.Expr) (*parser.LoadExpr, bool) {
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
	if len(p.Ops) != 0 || p.Target.Load == nil {
		return nil, false
	}
	return p.Target.Load, true
}

func (c *Compiler) eqFilter(e *parser.Expr, varName string) (string, string, bool) {
	if e == nil || len(e.Binary.Right) != 1 {
		return "", "", false
	}
	b := e.Binary.Right[0]
	if b.Op != "==" {
		return "", "", false
	}
	if f, ok := fieldFromUnary(e.Binary.Left, varName); ok {
		v, err := c.compilePostfix(b.Right)
		if err == nil {
			return f, v, true
		}
	}
	if f, ok := fieldFromPostfix(b.Right, varName); ok {
		v, err := c.compileUnary(e.Binary.Left)
		if err == nil {
			return f, v, true
		}
	}
	return "", "", false
}

func fieldFromUnary(u *parser.Unary, varName string) (string, bool) {
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if p.Target.Selector == nil || p.Target.Selector.Root != varName {
		return "", false
	}
	if len(p.Ops) == 1 && p.Ops[0].Field != nil {
		return p.Ops[0].Field.Name, true
	}
	if len(p.Ops) == 0 && len(p.Target.Selector.Tail) == 1 {
		return p.Target.Selector.Tail[0], true
	}
	return "", false
}

func fieldFromPostfix(p *parser.PostfixExpr, varName string) (string, bool) {
	if p.Target.Selector == nil || p.Target.Selector.Root != varName {
		return "", false
	}
	if len(p.Ops) == 1 && p.Ops[0].Field != nil {
		return p.Ops[0].Field.Name, true
	}
	if len(p.Ops) == 0 && len(p.Target.Selector.Tail) == 1 {
		return p.Target.Selector.Tail[0], true
	}
	return "", false
}

func formatTS(src []byte) []byte {
	// Prefer official formatters when available
	if err := EnsureFormatter(); err == nil {
		if path, err := exec.LookPath("deno"); err == nil {
			cmd := exec.Command(path, "fmt", "-q", "--ext", "ts", "-")
			cmd.Stdin = bytes.NewReader(src)
			var out bytes.Buffer
			cmd.Stdout = &out
			if err := cmd.Run(); err == nil {
				return out.Bytes()
			}
		}
		if path, err := exec.LookPath("npx"); err == nil {
			cmd := exec.Command(path, "--yes", "prettier", "--parser", "typescript")
			cmd.Stdin = bytes.NewReader(src)
			var out bytes.Buffer
			cmd.Stdout = &out
			if err := cmd.Run(); err == nil {
				return out.Bytes()
			}
		}
	}

	// Fallback: replace tabs and trim whitespace to keep code readable
	s := strings.ReplaceAll(string(src), "\t", "  ")
	var buf bytes.Buffer
	scanner := bufio.NewScanner(strings.NewReader(s))
	for scanner.Scan() {
		line := strings.TrimRight(scanner.Text(), " \t")
		buf.WriteString(line)
		buf.WriteByte('\n')
	}
	res := buf.Bytes()
	if len(res) == 0 || res[len(res)-1] != '\n' {
		res = append(res, '\n')
	}
	return res
}
