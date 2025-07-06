package fscode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into F# source code (subset used for LeetCode examples).
type Compiler struct {
	buf         bytes.Buffer
	preamble    bytes.Buffer
	indent      int
	env         *types.Env
	tmp         int
	loopTmp     int
	funTmp      int
	currentFunc string
	loops       []loopCtx
	locals      map[string]bool
	localsStack []map[string]bool
	helpers     map[string]bool
	tests       []testInfo
	packages    map[string]bool
	fields      map[string]bool
	seenTypes   map[string]bool
}

type loopCtx struct {
	brk  string
	cont string
}

type testInfo struct {
	Func string
	Name string
}

func hasLoopCtrl(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		switch {
		case s.Break != nil || s.Continue != nil:
			return true
		case s.For != nil:
			if hasLoopCtrl(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasLoopCtrl(s.While.Body) {
				return true
			}
		case s.If != nil:
			if hasLoopCtrl(s.If.Then) || hasLoopCtrlIf(s.If.ElseIf) || hasLoopCtrl(s.If.Else) {
				return true
			}
		}
	}
	return false
}

func programHasLoopCtrl(stmts []*parser.Statement) bool {
	if hasLoopCtrl(stmts) {
		return true
	}
	for _, s := range stmts {
		if s.Fun != nil {
			if hasLoopCtrl(s.Fun.Body) {
				return true
			}
		}
	}
	return false
}

func hasLoopCtrlIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasLoopCtrl(ifst.Then) || hasLoopCtrl(ifst.Else) {
		return true
	}
	return hasLoopCtrlIf(ifst.ElseIf)
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, locals: make(map[string]bool), helpers: make(map[string]bool), packages: make(map[string]bool), fields: make(map[string]bool), seenTypes: make(map[string]bool)}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newTmp() string {
	name := fmt.Sprintf("__tmp%d", c.tmp)
	c.tmp++
	return name
}

func (c *Compiler) newLoopID() string {
	id := fmt.Sprintf("%d", c.loopTmp)
	c.loopTmp++
	return id
}

func (c *Compiler) newFunName() string {
	name := fmt.Sprintf("__anon%d", c.funTmp)
	c.funTmp++
	return name
}

// Compile converts prog into F# source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.preamble.Reset()
	c.tests = nil
	var header bytes.Buffer
	header.WriteString("open System\n")
	if programHasLoopCtrl(prog.Statements) {
		header.WriteString("exception BreakException of int\n")
		header.WriteString("exception ContinueException of int\n")
	}
	header.WriteByte('\n')
	for _, s := range prog.Statements {
		if s.Import != nil && s.Import.Lang == nil {
			if err := c.compilePackageImport(s.Import); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	if len(c.tests) > 0 {
		c.use("_run_test")
		c.writeln("let mutable failures = 0")
		for _, tinfo := range c.tests {
			c.writeln(fmt.Sprintf("if not (_run_test \"%s\" %s) then failures <- failures + 1", tinfo.Name, tinfo.Func))
		}
		c.writeln("if failures > 0 then")
		c.indent++
		c.writeln("printfn \"\\n[FAIL] %d test(s) failed.\" failures")
		c.indent--
	}
	c.emitRuntime()
	out := header.Bytes()
	if c.preamble.Len() > 0 {
		out = append(out, c.preamble.Bytes()...)
		out = append(out, '\n')
	}
	out = append(out, c.buf.Bytes()...)
	return FormatFS(out), nil
}

func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
	c.env.SetFunc(fn.Name, fn)
	nested := len(funcStack) > 0
	c.pushFunc(fn.Name)
	defer c.popFunc()
	exc := fmt.Sprintf("Return_%s", sanitizeName(fn.Name))
	ret := fsType(fn.Return)
	excType := ret
	if strings.ContainsAny(ret, " ->") {
		excType = "(" + ret + ")"
	}
	line := fmt.Sprintf("exception %s of %s", exc, excType)
	if nested {
		c.preamble.WriteString(line + "\n")
	} else {
		c.writeln(line)
	}
	params := make([]string, len(fn.Params))
	paramTypes := make([]types.Type, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("(%s: %s)", sanitizeName(p.Name), fsType(p.Type))
		paramTypes[i] = types.ResolveTypeRef(p.Type, c.env)
	}
	kw := "let rec"
	c.writeln(fmt.Sprintf("%s %s %s : %s =", kw, sanitizeName(fn.Name), strings.Join(params, " "), ret))

	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		child.SetVar(p.Name, paramTypes[i], true)
	}
	origEnv := c.env
	c.env = child

	c.indent++
	c.writeln("try")
	c.indent++
	for _, p := range fn.Params {
		name := sanitizeName(p.Name)
		c.writeln(fmt.Sprintf("let mutable %s = %s", name, name))
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.writeln("failwith \"unreachable\"")
	c.indent--
	c.writeln(fmt.Sprintf("with %s v -> v", exc))
	c.indent--
	c.env = origEnv
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("let %s() =", name))
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.tests = append(c.tests, testInfo{Func: name, Name: t.Name})
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	name := sanitizeName(td.Name)
	if c.seenTypes[name] {
		return nil
	}
	if len(td.Variants) > 0 {
		c.writeln(fmt.Sprintf("type %s =", name))
		c.indent++
		for _, v := range td.Variants {
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), fsType(f.Type))
			}
			if len(fields) == 0 {
				c.writeln("| " + sanitizeName(v.Name))
			} else {
				c.writeln("| " + sanitizeName(v.Name) + " of " + strings.Join(fields, " * "))
			}
		}
		c.indent--
		c.seenTypes[name] = true
		return nil
	}
	fields := []*parser.TypeField{}
	methods := []*parser.FunStmt{}
	for _, m := range td.Members {
		if m.Field != nil {
			fields = append(fields, m.Field)
		} else if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	c.writeln(fmt.Sprintf("type %s =", name))
	c.indent++
	c.writeln("{")
	c.indent++
	for i, f := range fields {
		sep := ";"
		if i == len(fields)-1 {
			sep = ""
		}
		c.writeln(fmt.Sprintf("%s: %s%s", sanitizeName(f.Name), fsType(f.Type), sep))
	}
	c.indent--
	c.writeln("}")
	for _, m := range methods {
		if err := c.compileMethod(name, m, fields); err != nil {
			return err
		}
		c.writeln("")
	}
	c.indent--
	c.seenTypes[name] = true
	if c.env != nil {
		st := types.StructType{Name: td.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
		for _, f := range fields {
			st.Fields[f.Name] = types.ResolveTypeRef(f.Type, c.env)
			st.Order = append(st.Order, f.Name)
		}
		for _, m := range methods {
			params := make([]types.Type, len(m.Params))
			for i, p := range m.Params {
				params[i] = types.ResolveTypeRef(p.Type, c.env)
			}
			var ret types.Type = types.VoidType{}
			if m.Return != nil {
				ret = types.ResolveTypeRef(m.Return, c.env)
			}
			st.Methods[m.Name] = types.Method{Decl: m, Type: types.FuncType{Params: params, Return: ret}}
		}
		c.env.SetStruct(td.Name, st)
	}
	return nil
}

func (c *Compiler) compileMethod(structName string, fn *parser.FunStmt, fields []*parser.TypeField) error {
	nested := len(funcStack) > 0
	name := sanitizeName(fn.Name)
	c.pushFunc(name)
	defer c.popFunc()
	exc := fmt.Sprintf("Return_%s_%s", sanitizeName(structName), name)
	ret := fsType(fn.Return)
	excType := ret
	if strings.ContainsAny(ret, " ->") {
		excType = "(" + ret + ")"
	}
	line := fmt.Sprintf("exception %s of %s", exc, excType)
	if nested {
		c.preamble.WriteString(line + "\n")
	} else {
		c.writeln(line)
	}
	params := make([]string, len(fn.Params))
	paramTypes := make([]types.Type, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("(%s: %s)", sanitizeName(p.Name), fsType(p.Type))
		paramTypes[i] = types.ResolveTypeRef(p.Type, c.env)
	}
	c.writeln(fmt.Sprintf("member this.%s %s : %s =", name, strings.Join(params, " "), ret))

	child := types.NewEnv(c.env)
	for _, f := range fields {
		child.SetVar(f.Name, types.ResolveTypeRef(f.Type, c.env), true)
	}
	for i, p := range fn.Params {
		child.SetVar(p.Name, paramTypes[i], true)
	}
	origEnv := c.env
	c.env = child

	c.indent++
	c.writeln("try")
	c.indent++
	for _, p := range fn.Params {
		name := sanitizeName(p.Name)
		c.writeln(fmt.Sprintf("let mutable %s = %s", name, name))
	}
	for _, f := range fields {
		n := sanitizeName(f.Name)
		c.writeln(fmt.Sprintf("let %s = this.%s", n, n))
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.writeln("failwith \"unreachable\"")
	c.indent--
	c.writeln(fmt.Sprintf("with %s v -> v", exc))
	c.indent--
	c.env = origEnv
	return nil
}

func (c *Compiler) compilePackageImport(im *parser.ImportStmt) error {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	if c.packages[alias] {
		return nil
	}
	c.packages[alias] = true

	path := strings.Trim(im.Path, "\"")
	base := ""
	if strings.HasPrefix(path, "./") || strings.HasPrefix(path, "../") {
		base = filepath.Dir(im.Pos.Filename)
	}
	target := filepath.Join(base, path)
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

	pkgEnv := types.NewEnv(c.env)
	origEnv := c.env
	c.env = pkgEnv
	c.writeln(fmt.Sprintf("module %s =", alias))
	c.indent++
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			c.env = origEnv
			return err
		}
		if errs := types.Check(prog, pkgEnv); len(errs) > 0 {
			c.env = origEnv
			return errs[0]
		}
		for _, s := range prog.Statements {
			if s.Type != nil {
				if err := c.compileTypeDecl(s.Type); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
		for _, s := range prog.Statements {
			if s.Fun != nil && s.Fun.Export {
				if err := c.compileFunStmt(s.Fun); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
	}
	c.indent--
	c.writeln("")
	c.env = origEnv
	return nil
}

func (c *Compiler) compileExternVar(ev *parser.ExternVarDecl) error {
	name := sanitizeName(ev.Name())
	typ := fsType(ev.Type)
	c.writeln(fmt.Sprintf("let mutable %s: %s = Unchecked.defaultof<%s>", name, typ, typ))
	if c.env != nil {
		c.env.SetVar(ev.Name(), types.ResolveTypeRef(ev.Type, c.env), true)
	}
	return nil
}

func (c *Compiler) compileExternFun(ef *parser.ExternFunDecl) error {
	params := make([]string, len(ef.Params))
	paramTypes := make([]types.Type, len(ef.Params))
	for i, p := range ef.Params {
		params[i] = fmt.Sprintf("(%s: %s)", sanitizeName(p.Name), fsType(p.Type))
		if c.env != nil {
			paramTypes[i] = types.ResolveTypeRef(p.Type, c.env)
		}
	}
	ret := fsType(ef.Return)
	c.writeln(fmt.Sprintf("let %s %s : %s = failwith \"extern\"", sanitizeName(ef.Name()), strings.Join(params, " "), ret))
	if c.env != nil {
		ft := types.FuncType{Params: paramTypes, Return: types.ResolveTypeRef(ef.Return, c.env)}
		c.env.SetVar(ef.Name(), ft, false)
	}
	return nil
}

func (c *Compiler) compileExternType(et *parser.ExternTypeDecl) error {
	c.writeln(fmt.Sprintf("type %s = obj", sanitizeName(et.Name)))
	return nil
}

func (c *Compiler) compileExternObject(eo *parser.ExternObjectDecl) error {
	name := sanitizeName(eo.Name)
	c.use("_extern")
	c.writeln(fmt.Sprintf("let %s = _extern_get \"%s\"", name, eo.Name))
	if c.env != nil {
		c.env.SetVar(eo.Name, types.AnyType{}, true)
	}
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.ExternVar != nil:
		return c.compileExternVar(s.ExternVar)
	case s.ExternFun != nil:
		return c.compileExternFun(s.ExternFun)
	case s.ExternType != nil:
		return c.compileExternType(s.ExternType)
	case s.ExternObject != nil:
		return c.compileExternObject(s.ExternObject)
	case s.Let != nil:
		expr, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Let.Name)
		if s.Let.Type != nil {
			typ := fsType(s.Let.Type)
			c.writeln(fmt.Sprintf("let %s: %s = %s", name, typ, expr))
		} else {
			t := types.TypeOfExpr(s.Let.Value, c.env)
			if !types.ContainsAny(t) && (isEmptyListLiteral(s.Let.Value) || isEmptyMapLiteral(s.Let.Value)) {
				c.writeln(fmt.Sprintf("let %s: %s = %s", name, fsTypeOf(t), expr))
			} else {
				c.writeln(fmt.Sprintf("let %s = %s", name, expr))
			}
		}
		if c.isMapExpr(s.Let.Value) {
			c.locals[name] = true
		}
		if c.env != nil {
			var typ types.Type
			if s.Let.Type != nil {
				typ = types.ResolveTypeRef(s.Let.Type, c.env)
			} else {
				typ = types.TypeOfExpr(s.Let.Value, c.env)
			}
			c.env.SetVar(s.Let.Name, typ, false)
		}
	case s.Var != nil:
		expr, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Var.Name)
		if s.Var.Type != nil {
			typ := fsType(s.Var.Type)
			c.writeln(fmt.Sprintf("let mutable %s: %s = %s", name, typ, expr))
		} else {
			t := types.TypeOfExpr(s.Var.Value, c.env)
			if !types.ContainsAny(t) && (isEmptyListLiteral(s.Var.Value) || isEmptyMapLiteral(s.Var.Value)) {
				c.writeln(fmt.Sprintf("let mutable %s: %s = %s", name, fsTypeOf(t), expr))
			} else {
				c.writeln(fmt.Sprintf("let mutable %s = %s", name, expr))
			}
		}
		if c.isMapExpr(s.Var.Value) {
			c.locals[name] = true
		}
		if c.env != nil {
			var typ types.Type
			if s.Var.Type != nil {
				typ = types.ResolveTypeRef(s.Var.Type, c.env)
			} else {
				typ = types.TypeOfExpr(s.Var.Value, c.env)
			}
			c.env.SetVar(s.Var.Name, typ, true)
		}
	case s.Import != nil:
		// package imports are handled in Compile
		return nil
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Fun != nil:
		return c.compileFunStmt(s.Fun)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("raise (Return_%s (%s))", sanitizeName(c.currentFunc), expr))
	case s.Assign != nil:
		base := sanitizeName(s.Assign.Name)
		idxParts := []string{}
		for _, idx := range s.Assign.Index {
			iexpr, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			idxParts = append(idxParts, iexpr)
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		if c.locals[base] && len(idxParts) == 1 {
			c.writeln(fmt.Sprintf("%s <- Map.add %s %s %s", base, idxParts[0], val, base))
		} else {
			lhs := base
			for _, p := range idxParts {
				lhs = fmt.Sprintf("%s.[%s]", lhs, p)
			}
			c.writeln(fmt.Sprintf("%s <- %s", lhs, val))
		}
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("ignore (%s)", expr))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		if len(c.loops) == 0 {
			return fmt.Errorf("break not in loop")
		}
		c.writeln(fmt.Sprintf("raise (%s)", c.loops[len(c.loops)-1].brk))
	case s.Continue != nil:
		if len(c.loops) == 0 {
			return fmt.Errorf("continue not in loop")
		}
		c.writeln(fmt.Sprintf("raise (%s)", c.loops[len(c.loops)-1].cont))
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	default:
		// ignore
	}
	return nil
}

var funcStack []string

func (c *Compiler) pushFunc(name string) {
	funcStack = append(funcStack, name)
	c.currentFunc = name
	c.localsStack = append(c.localsStack, c.locals)
	newLocals := make(map[string]bool)
	for k, v := range c.locals {
		newLocals[k] = v
	}
	c.locals = newLocals
}

func (c *Compiler) pushLoop(brk, cont string) {
	c.loops = append(c.loops, loopCtx{brk: brk, cont: cont})
}

func (c *Compiler) popLoop() {
	if len(c.loops) > 0 {
		c.loops = c.loops[:len(c.loops)-1]
	}
}

func (c *Compiler) popFunc() {
	if len(funcStack) > 0 {
		funcStack = funcStack[:len(funcStack)-1]
		if len(c.localsStack) > 0 {
			c.locals = c.localsStack[len(c.localsStack)-1]
			c.localsStack = c.localsStack[:len(c.localsStack)-1]
		}
		if len(funcStack) > 0 {
			c.currentFunc = funcStack[len(funcStack)-1]
		} else {
			c.currentFunc = ""
		}
	}
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	useVar := name != "_"
	if useVar && c.env != nil {
		var typ types.Type = types.AnyType{}
		if f.RangeEnd != nil {
			typ = types.IntType{}
		} else if c.isStringExpr(f.Source) {
			typ = types.StringType{}
		} else if mt, ok := types.TypeOfExpr(f.Source, c.env).(types.MapType); ok {
			typ = mt.Key
		} else if lt, ok := types.TypeOfExpr(f.Source, c.env).(types.ListType); ok {
			typ = lt.Elem
		}
		c.env.SetVar(f.Name, typ, true)
	}
	ctrl := hasLoopCtrl(f.Body)
	var brk, cont string
	var id string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		loopVar := name
		if !useVar {
			loopVar = c.newTmp()
		}
		c.writeln(fmt.Sprintf("for %s = %s to %s - 1 do", loopVar, start, end))
		c.indent++
		if ctrl {
			c.pushLoop(brk, cont)
			c.writeln("try")
			c.indent++
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		if ctrl {
			c.indent--
			c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
			c.popLoop()
		}
		c.indent--
		if ctrl {
			c.indent--
			c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
		}
		return nil
	}
	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	loopVar := name
	if !useVar {
		loopVar = c.newTmp()
	}
	srcExpr := src
	if c.isMapExpr(f.Source) {
		srcExpr = fmt.Sprintf("Map.keys %s", src)
	} else if c.isGroupExpr(f.Source) {
		srcExpr = fmt.Sprintf("%s.Items", src)
	}
	c.writeln(fmt.Sprintf("for %s in %s do", loopVar, srcExpr))
	c.indent++
	if c.isStringExpr(f.Source) && useVar {
		c.writeln(fmt.Sprintf("let %s = string %s", loopVar, loopVar))
	}
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	ctrl := hasLoopCtrl(w.Body)
	var brk, cont string
	var id string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
	c.writeln("while " + cond + " do")
	c.indent++
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
	}
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	name := sanitizeName(u.Target)
	var st *types.StructType
	if c.env != nil {
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = &s
				}
			}
		}
	}
	if st == nil {
		return fmt.Errorf("update: unsupported element type")
	}

	c.writeln(fmt.Sprintf("for i = 0 to %s.Length - 1 do", name))
	c.indent++
	c.writeln(fmt.Sprintf("let mutable item = %s.[i]", name))

	origEnv := c.env
	if st != nil {
		child := types.NewEnv(c.env)
		for _, f := range st.Order {
			c.writeln(fmt.Sprintf("let %s = item.%s", sanitizeName(f), sanitizeName(f)))
			child.SetVar(f, st.Fields[f], true)
		}
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("if %s then", cond))
		c.indent++
	}

	parts := make([]string, len(u.Set.Items))
	for i, it := range u.Set.Items {
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		key, ok := identOfExpr(it.Key)
		if !ok {
			c.env = origEnv
			return fmt.Errorf("unsupported update key")
		}
		parts[i] = fmt.Sprintf("%s = %s", sanitizeName(key), val)
	}
	c.writeln(fmt.Sprintf("item <- { item with %s }", strings.Join(parts, "; ")))

	if u.Where != nil {
		c.indent--
	}

	if st != nil {
		c.env = origEnv
	}

	c.writeln(fmt.Sprintf("%s.[i] <- item", name))
	c.indent--
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	return c.compileIfChain(ifst, true)
}

func (c *Compiler) compileIfChain(ifst *parser.IfStmt, first bool) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	kw := "if"
	if !first {
		kw = "elif"
	}
	c.writeln(fmt.Sprintf("%s %s then", kw, cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if ifst.ElseIf != nil {
		return c.compileIfChain(ifst.ElseIf, false)
	}
	if len(ifst.Else) > 0 {
		c.writeln("else")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf strings.Builder
	buf.WriteString("(match " + target + " with")
	for _, cs := range m.Cases {
		pat, err := c.compilePattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		buf.WriteString(" | " + pat + " -> " + res)
	}
	buf.WriteString(")")
	return buf.String(), nil
}

func (c *Compiler) compileIfExpr(ifst *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ifst.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ifst.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ifst.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ifst.Else != nil {
		elseExpr, err = c.compileExpr(ifst.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "()"
	}
	return fmt.Sprintf("(if %s then %s else %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	if c.isGroupExpr(q.Source) {
		src += ".Items"
	}

	// simple grouping without joins or filters
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		orig := c.env
		child := types.NewEnv(c.env)
		if lt, ok := types.TypeOfExpr(q.Source, c.env).(types.ListType); ok {
			child.SetVar(q.Var, lt.Elem, true)
		} else {
			child.SetVar(q.Var, types.AnyType{}, true)
		}
		c.env = child
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.AnyType{}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		havingExpr := ""
		if q.Group.Having != nil {
			havingExpr, err = c.compileExpr(q.Group.Having)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		c.env = orig
		c.use("_group_by")
		c.use("_Group")
		expr := fmt.Sprintf("_group_by %s (fun %s -> %s)", src, sanitizeName(q.Var), keyExpr)
		if havingExpr != "" {
			expr += fmt.Sprintf(" |> List.filter (fun %s -> %s)", sanitizeName(q.Group.Name), havingExpr)
		}
		expr += fmt.Sprintf(" |> List.map (fun %s -> %s)", sanitizeName(q.Group.Name), valExpr)
		return expr, nil
	}
	orig := c.env
	child := types.NewEnv(c.env)
	if lt, ok := types.TypeOfExpr(q.Source, c.env).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, true)
	} else {
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	fromSrc := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		if c.isGroupExpr(f.Src) {
			fs += ".Items"
		}
		fromSrc[i] = fs
		if lt, ok := types.TypeOfExpr(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else if gt, ok := types.TypeOfExpr(f.Src, c.env).(types.GroupType); ok {
			child.SetVar(f.Var, gt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	joinSrc := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		if c.isGroupExpr(j.Src) {
			js += ".Items"
		}
		joinSrc[i] = js
		if lt, ok := types.TypeOfExpr(j.Src, c.env).(types.ListType); ok {
			child.SetVar(j.Var, lt.Elem, true)
		} else if gt, ok := types.TypeOfExpr(j.Src, c.env).(types.GroupType); ok {
			child.SetVar(j.Var, gt.Elem, true)
		} else {
			child.SetVar(j.Var, types.AnyType{}, true)
		}
	}
	c.env = child
	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinOns[i] = on
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
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

	if q.Group != nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		tupleParts := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			tupleParts = append(tupleParts, sanitizeName(f.Var))
		}
		for _, j := range q.Joins {
			tupleParts = append(tupleParts, sanitizeName(j.Var))
		}
		tuple := strings.Join(tupleParts, ", ")
		if len(tupleParts) > 1 {
			tuple = "(" + tuple + ")"
		}
		var rows strings.Builder
		indent := "    "
		rows.WriteString("[|\n")
		rows.WriteString(fmt.Sprintf("%sfor %s in %s do\n", indent, sanitizeName(q.Var), src))
		ind := indent + "    "
		for i, f := range q.Froms {
			rows.WriteString(fmt.Sprintf("%sfor %s in %s do\n", ind, sanitizeName(f.Var), fromSrc[i]))
			ind += "    "
		}
		for i := range joinSrc {
			rows.WriteString(fmt.Sprintf("%sfor %s in %s do\n", ind, sanitizeName(q.Joins[i].Var), joinSrc[i]))
			ind += "    "
			if joinOns[i] != "" {
				rows.WriteString(fmt.Sprintf("%sif %s then\n", ind, joinOns[i]))
				ind += "    "
			}
		}
		if whereExpr != "" {
			rows.WriteString(fmt.Sprintf("%sif %s then\n", ind, whereExpr))
			ind += "    "
		}
		rows.WriteString(fmt.Sprintf("%syield %s\n", ind, tuple))
		rows.WriteString("|]")
		groupRows := rows.String()

		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.AnyType{}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		sortExpr2 := ""
		if q.Sort != nil {
			sortExpr2, err = c.compileExpr(q.Sort)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		havingExpr := ""
		if q.Group.Having != nil {
			havingExpr, err = c.compileExpr(q.Group.Having)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		c.env = orig
		c.use("_group_by")
		c.use("_Group")
		groups := fmt.Sprintf("_group_by %s (fun %s -> %s)", groupRows, tuple, keyExpr)
		expr := fmt.Sprintf("[| for g in %s do let %s = g", groups, sanitizeName(q.Group.Name))
		if havingExpr != "" {
			expr += fmt.Sprintf(" if %s then", havingExpr)
		}
		if q.Sort != nil {
			expr += fmt.Sprintf(" yield (%s, %s) |] |> Array.sortBy fst |> Array.map snd", sortExpr2, valExpr)
		} else {
			expr += fmt.Sprintf(" yield %s |]", valExpr)
		}
		if skipExpr != "" {
			expr += fmt.Sprintf(" |> Array.skip %s", skipExpr)
		}
		if takeExpr != "" {
			expr += fmt.Sprintf(" |> Array.take %s", takeExpr)
		}
		return expr, nil
	}

	var buf strings.Builder
	indent := "    "
	buf.WriteString("\n    [|\n")
	buf.WriteString(fmt.Sprintf("%sfor %s in %s do\n", indent, sanitizeName(q.Var), src))
	ind := indent + "    "
	for i, f := range q.Froms {
		buf.WriteString(fmt.Sprintf("%sfor %s in %s do\n", ind, sanitizeName(f.Var), fromSrc[i]))
		ind += "    "
	}
	for i := range joinSrc {
		buf.WriteString(fmt.Sprintf("%sfor %s in %s do\n", ind, sanitizeName(q.Joins[i].Var), joinSrc[i]))
		ind += "    "
		if joinOns[i] != "" {
			buf.WriteString(fmt.Sprintf("%sif %s then\n", ind, joinOns[i]))
			ind += "    "
		}
	}
	if whereExpr != "" {
		buf.WriteString(fmt.Sprintf("%sif %s then\n", ind, whereExpr))
		ind += "    "
	}
	if q.Sort != nil {
		buf.WriteString(fmt.Sprintf("%syield (%s, %s)\n", ind, sortExpr, sel))
		buf.WriteString("    |]\n")
		buf.WriteString("    |> Array.sortBy fst\n")
		buf.WriteString("    |> Array.map snd")
	} else {
		buf.WriteString(fmt.Sprintf("%syield %s\n", ind, sel))
		buf.WriteString("    |]")
	}
	if skipExpr != "" {
		if q.Sort != nil {
			buf.WriteString(fmt.Sprintf("\n    |> Array.skip %s", skipExpr))
		} else {
			buf.WriteString(fmt.Sprintf("\n|> Array.skip %s", skipExpr))
		}
	}
	if takeExpr != "" {
		if q.Sort != nil || skipExpr != "" {
			buf.WriteString(fmt.Sprintf("\n    |> Array.take %s", takeExpr))
		} else {
			buf.WriteString(fmt.Sprintf("\n|> Array.take %s", takeExpr))
		}
	}
	return buf.String(), nil
}

func (c *Compiler) compilePattern(e *parser.Expr) (string, error) {
	if isUnderscoreExpr(e) {
		return "_", nil
	}
	if call, ok := callPattern(e); ok {
		parts := make([]string, len(call.Args))
		for i, a := range call.Args {
			if id, ok := identName(a); ok {
				parts[i] = sanitizeName(id)
			} else {
				return "", fmt.Errorf("unsupported pattern")
			}
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), strings.Join(parts, ", ")), nil
	}
	if id, ok := identName(e); ok {
		return sanitizeName(id), nil
	}
	if lit := extractLiteral(e); lit != nil {
		return c.compileLiteral(lit)
	}
	return "", fmt.Errorf("unsupported pattern")
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", fmt.Errorf("nil binary expr")
	}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}

	operands := []string{first}
	lists := []bool{c.isListUnary(b.Left)}
	maps := []bool{c.isMapUnary(b.Left)}
	strs := []bool{c.isStringUnary(b.Left)}
	ops := []string{}

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		lists = append(lists, c.isListPostfix(part.Right))
		maps = append(maps, c.isMapPostfix(part.Right))
		strs = append(strs, c.isStringPostfix(part.Right))
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
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
				left := operands[i]
				right := operands[i+1]
				op := ops[i]

				symbol := op
				switch op {
				case "==":
					symbol = "="
				case "!=":
					symbol = "<>"
				}

				if op == "+" && (lists[i] || lists[i+1]) {
					operands[i] = fmt.Sprintf("Array.append %s %s", left, right)
					lists[i] = true
					maps[i] = false
					strs[i] = false
				} else if op == "+" && (strs[i] || strs[i+1]) {
					if !strs[i] {
						left = fmt.Sprintf("(string %s)", left)
					}
					if !strs[i+1] {
						right = fmt.Sprintf("(string %s)", right)
					}
					operands[i] = fmt.Sprintf("(%s + %s)", left, right)
					strs[i] = true
					lists[i] = false
					maps[i] = false
				} else if op == "in" {
					if maps[i+1] {
						operands[i] = fmt.Sprintf("Map.containsKey %s %s", left, right)
					} else if strs[i+1] {
						operands[i] = fmt.Sprintf("%s.Contains(%s)", right, left)
					} else {
						operands[i] = fmt.Sprintf("Array.contains %s %s", left, right)
					}
					lists[i] = false
					maps[i] = false
					strs[i] = false
				} else if op == "union_all" {
					operands[i] = fmt.Sprintf("Array.append %s %s", left, right)
					lists[i] = true
					maps[i] = false
					strs[i] = false
				} else if op == "union" {
					operands[i] = fmt.Sprintf("Array.append %s %s |> Array.distinct", left, right)
					lists[i] = true
					maps[i] = false
					strs[i] = false
				} else if op == "except" {
					c.use("_except")
					operands[i] = fmt.Sprintf("_except %s %s", left, right)
					lists[i] = true
					maps[i] = false
					strs[i] = false
				} else if op == "intersect" {
					c.use("_intersect")
					operands[i] = fmt.Sprintf("_intersect %s %s", left, right)
					lists[i] = true
					maps[i] = false
					strs[i] = false
				} else {
					operands[i] = fmt.Sprintf("(%s %s %s)", left, symbol, right)
					lists[i] = false
					maps[i] = false
					strs[i] = false
				}

				operands = append(operands[:i+1], operands[i+2:]...)
				lists = append(lists[:i+1], lists[i+2:]...)
				maps = append(maps[:i+1], maps[i+2:]...)
				strs = append(strs[:i+1], strs[i+2:]...)
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

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "!" {
			val = fmt.Sprintf("(not %s)", val)
		} else {
			val = fmt.Sprintf("(%s%s)", op, val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if len(p.Ops) == 1 && p.Ops[0].Cast != nil && p.Target.Fetch != nil {
		return c.compileFetchCast(p.Target.Fetch, p.Ops[0].Cast.Type)
	}

	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	isStr := c.isStringPrimary(p.Target)
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
			argStr := strings.Join(args, " ")
			if expr == "print" {
				if len(args) == 1 {
					return fmt.Sprintf("printfn \"%%A\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%A\" (%s)", argStr), nil
			}
			if expr == "len" {
				if len(args) != 1 {
					return "", fmt.Errorf("len expects 1 arg")
				}
				if c.isMapExpr(op.Call.Args[0]) {
					return fmt.Sprintf("Map.count (%s)", args[0]), nil
				}
				return fmt.Sprintf("%s.Length", args[0]), nil
			}
			expr = fmt.Sprintf("%s %s", expr, argStr)
			isStr = false
			continue
		}
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon != nil {
				start := "0"
				startExpr := start
				if op.Index.Start != nil {
					start, err = c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					startExpr = start
					if isStr {
						if v, ok := intLiteral(op.Index.Start); !ok || v < 0 {
							startExpr = fmt.Sprintf("(if %s < 0 then %s.Length + %s else %s)", start, expr, start, start)
						}
					}
				}
				end := fmt.Sprintf("%s.Length", expr)
				endExpr := end
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					endExpr = end
					if isStr {
						if v, ok := intLiteral(op.Index.End); !ok || v < 0 {
							endExpr = fmt.Sprintf("(if %s < 0 then %s.Length + %s else %s)", end, expr, end, end)
						}
					}
				}
				expr = fmt.Sprintf("%s.[%s .. (%s - 1)]", expr, startExpr, endExpr)
			} else {
				if isStr {
					if v, ok := intLiteral(op.Index.Start); ok && v >= 0 {
						expr = fmt.Sprintf("(string %s.[%d])", expr, v)
					} else {
						idxExpr := fmt.Sprintf("(if %s < 0 then %s.Length + %s else %s)", idx, expr, idx, idx)
						expr = fmt.Sprintf("(string %s.[%s])", expr, idxExpr)
					}
					isStr = true
				} else {
					expr = fmt.Sprintf("%s.[%s]", expr, idx)
					isStr = false
				}
			}
			continue
		}
		if op.Cast != nil {
			typ := op.Cast.Type
			if typ.Simple != nil {
				switch *typ.Simple {
				case "int":
					expr = fmt.Sprintf("(int %s)", expr)
				case "float":
					expr = fmt.Sprintf("(float %s)", expr)
				case "bool":
					expr = fmt.Sprintf("(bool %s)", expr)
				case "string":
					expr = fmt.Sprintf("(string %s)", expr)
				default:
					c.use("_cast")
					expr = fmt.Sprintf("_cast<%s>(%s)", fsType(typ), expr)
				}
			} else {
				c.use("_cast")
				expr = fmt.Sprintf("_cast<%s>(%s)", fsType(typ), expr)
			}
			isStr = typ.Simple != nil && *typ.Simple == "string"
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[|" + strings.Join(elems, "; ") + "|]", nil
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, s := range p.Selector.Tail {
			expr += "." + sanitizeName(s)
		}
		return expr, nil
	case p.Struct != nil:
		// struct literal or union variant
		if ut, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
			st := ut.Variants[p.Struct.Name]
			vals := make(map[string]string)
			for _, f := range p.Struct.Fields {
				v, err := c.compileExpr(f.Value)
				if err != nil {
					return "", err
				}
				vals[f.Name] = v
			}
			if len(st.Order) == 0 {
				return sanitizeName(p.Struct.Name), nil
			}
			parts := make([]string, len(st.Order))
			for i, n := range st.Order {
				parts[i] = vals[n]
			}
			return fmt.Sprintf("%s(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
		}
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
		}
		return "{ " + strings.Join(parts, "; ") + " }", nil
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Group != nil:
		e, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	}
	return "", fmt.Errorf("unsupported primary expression")
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	if _, ok := c.env.GetFunc(call.Func); ok {
		for i, a := range args {
			if !isSimpleIdent(a) {
				args[i] = "(" + a + ")"
			}
		}
		return fmt.Sprintf("%s %s", sanitizeName(call.Func), strings.Join(args, " ")), nil
	}

	switch call.Func {
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("(string %s)", args[0]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		if c.isMapExpr(call.Args[0]) {
			return fmt.Sprintf("Map.count (%s)", args[0]), nil
		}
		return fmt.Sprintf("%s.Length", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		if c.isGroupExpr(call.Args[0]) {
			return fmt.Sprintf("%s.size", args[0]), nil
		}
		return fmt.Sprintf("Seq.length %s", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("Seq.sum %s", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("Seq.average %s", args[0]), nil
	case "substr":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		c.use("_slice_string")
		return fmt.Sprintf("_slice_string %s %s %s", args[0], args[1], args[2]), nil
	case "reverse":
		if len(args) != 1 {
			return "", fmt.Errorf("reverse expects 1 arg")
		}
		if c.isStringExpr(call.Args[0]) {
			return fmt.Sprintf("(%s.ToCharArray() |> Array.rev |> System.String)", args[0]), nil
		}
		if c.isListExpr(call.Args[0]) {
			return fmt.Sprintf("Array.rev %s", args[0]), nil
		}
		return "", fmt.Errorf("reverse expects string or list")
	case "concat":
		if len(args) < 2 {
			return "", fmt.Errorf("concat expects at least 2 args")
		}
		expr := fmt.Sprintf("Array.append %s %s", args[0], args[1])
		for i := 2; i < len(args); i++ {
			expr = fmt.Sprintf("Array.append %s %s", expr, args[i])
		}
		return expr, nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		return fmt.Sprintf("Seq.min %s", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("Seq.max %s", args[0]), nil
	case "pow":
		if len(args) != 2 {
			return "", fmt.Errorf("pow expects 2 args")
		}
		return fmt.Sprintf("Math.Pow(float %s, float %s)", args[0], args[1]), nil
	case "ceil":
		if len(args) != 1 {
			return "", fmt.Errorf("ceil expects 1 arg")
		}
		return fmt.Sprintf("Math.Ceiling(float %s)", args[0]), nil
	case "floor":
		if len(args) != 1 {
			return "", fmt.Errorf("floor expects 1 arg")
		}
		return fmt.Sprintf("Math.Floor(float %s)", args[0]), nil
	case "now":
		if len(args) != 0 {
			return "", fmt.Errorf("now expects no args")
		}
		return "System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() * 1000000L", nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		c.use("_json_helpers")
		return fmt.Sprintf("_json %s", args[0]), nil
	case "to_json":
		if len(args) != 1 {
			return "", fmt.Errorf("to_json expects 1 arg")
		}
		c.use("_json_helpers")
		return fmt.Sprintf("_to_json %s", args[0]), nil
	case "eval":
		if len(args) != 1 {
			return "", fmt.Errorf("eval expects 1 arg")
		}
		c.use("_eval")
		return fmt.Sprintf("_eval %s", args[0]), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input expects no args")
		}
		c.use("_input")
		return "_input()", nil
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("printfn \"%%A\" (%s)", args[0]), nil
		}
		return fmt.Sprintf("printfn \"%%A\" (%s)", strings.Join(args, ", ")), nil
	default:
		for i, a := range args {
			if !isSimpleIdent(a) {
				args[i] = "(" + a + ")"
			}
		}
		return fmt.Sprintf("%s %s", sanitizeName(call.Func), strings.Join(args, " ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("(%s: %s)", sanitizeName(p.Name), fsType(p.Type))
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(fun %s -> %s)", strings.Join(params, " "), expr), nil
	}
	name := c.newFunName()
	sub := New(c.env)
	sub.tmp = c.tmp
	sub.loopTmp = c.loopTmp
	sub.funTmp = c.funTmp
	fs := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: fn.BlockBody}
	if err := sub.compileFunStmt(fs); err != nil {
		return "", err
	}
	for h := range sub.helpers {
		c.helpers[h] = true
	}
	c.tmp = sub.tmp
	c.loopTmp = sub.loopTmp
	c.funTmp = sub.funTmp
	if sub.preamble.Len() > 0 {
		c.preamble.Write(sub.preamble.Bytes())
	}
	c.preamble.Write(sub.buf.Bytes())
	c.preamble.WriteByte('\n')
	return name, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "None"
	if l.Path != nil {
		path = fmt.Sprintf("Some %q", *l.Path)
	}
	opts := "None"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Some (%s)", v)
	}
	c.use("_load")
	expr := fmt.Sprintf("_load %s %s", path, opts)
	if l.Type != nil {
		c.use("_cast")
		typ := fsType(l.Type)
		expr = fmt.Sprintf("%s |> List.map (fun row -> _cast<%s>(row))", expr, typ)
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "None"
	if s.Path != nil {
		path = fmt.Sprintf("Some %q", *s.Path)
	}
	opts := "None"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Some (%s)", v)
	}
	c.use("_save")
	return fmt.Sprintf("_save %s %s %s", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "None"
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Some (%s)", w)
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch %s %s", url, opts), nil
}

func (c *Compiler) compileFetchCast(f *parser.FetchExpr, t *parser.TypeRef) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "None"
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Some (%s)", w)
	}
	typ := fsType(t)
	c.use("_fetch_json")
	return fmt.Sprintf("_fetch_json<%s> %s %s", typ, url, opts), nil
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
			params = append(params, fmt.Sprintf("(%q, %s)", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "None"
	if len(params) > 0 {
		paramStr = "Some (Map.ofList [" + strings.Join(params, "; ") + "])"
	}
	if model == "" {
		model = "\"\""
	}
	if g.Target == "embedding" {
		c.use("_genEmbed")
		return fmt.Sprintf("_genEmbed %s %s %s", text, model, paramStr), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_genStruct")
			return fmt.Sprintf("_genStruct<%s> %s %s %s", sanitizeName(g.Target), prompt, model, paramStr), nil
		}
	}
	c.use("_genText")
	return fmt.Sprintf("_genText %s %s %s", prompt, model, paramStr), nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if not (%s) then failwith \"expect failed\"", expr))
	return nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	if len(m.Items) == 0 {
		return "Map.empty", nil
	}
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		var k string
		if name, ok := identOfExpr(it.Key); ok {
			c.ensureFieldConst(name)
			k = sanitizeName(name)
		} else {
			var err error
			k, err = c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("(%s, %s)", k, v)
	}
	return "Map.ofList [" + strings.Join(items, "; ") + "]", nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			if !strings.Contains(s, ".") {
				s += ".0"
			}
		}
		return s, nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	case l.Null:
		return "null", nil
	}
	return "", fmt.Errorf("unknown literal")
}
