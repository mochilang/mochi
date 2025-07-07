//go:build slow

package pascode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Pascal source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	tempVarCount int
	tempVars     map[string]string
	expected     types.Type
	varTypes     map[string]string
	packages     map[string]bool
	helpers      map[string]bool
	lambdas      []string
	replacements map[string]string
}

// New creates a new Pascal compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:          env,
		tempVars:     make(map[string]string),
		packages:     make(map[string]bool),
		helpers:      make(map[string]bool),
		lambdas:      []string{},
		replacements: make(map[string]string),
	}
}

// Compile returns Pascal source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.tempVars = make(map[string]string)
	c.lambdas = nil
	c.replacements = make(map[string]string)
	name := "main"
	if prog.Package != "" {
		name = sanitizeName(prog.Package)
	}
	c.writeln(fmt.Sprintf("program %s;", name))
	c.writeln("{$mode objfpc}")
	c.writeln("uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;")
	c.writeln("")
	c.writeln("type")
	c.indent++
	c.writeln("generic TArray<T> = array of T;")
	c.indent--

	// Compile package imports first
	for _, s := range prog.Statements {
		if s.Import != nil {
			if s.Import.Lang == nil {
				if err := c.compilePackageImport(s.Import); err != nil {
					return nil, err
				}
			} else {
				// Allow builtin foreign packages like strings and math
				p := strings.Trim(s.Import.Path, "\"")
				if p != "strings" && p != "math" {
					return nil, fmt.Errorf("foreign imports not supported")
				}
			}
		}
	}

	// Emit user-defined types
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
		}
	}
	c.writeln("")

	// Emit function declarations first.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit generated lambda functions
	for _, code := range c.lambdas {
		c.buf.WriteString(code)
		if !strings.HasSuffix(code, "\n") {
			c.writeln("")
		}
		c.writeln("")
	}

	// Compile main body first to gather temporaries
	var body bytes.Buffer
	prevBuf := c.buf
	prevIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 1
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			c.buf = prevBuf
			c.indent = prevIndent
			return nil, err
		}
	}
	body = c.buf
	c.buf = prevBuf
	c.indent = prevIndent

	c.emitHelpers()

	// Collect global vars and compiler-generated temporaries.
	vars := map[string]string{}
	collectVars(prog.Statements, c.env, vars)
	for n, t := range c.tempVars {
		if t == "" {
			t = "integer"
		}
		vars[n] = t
	}

	if len(vars) > 0 {
		c.writeln("var")
		c.indent++
		names := make([]string, 0, len(vars))
		for n := range vars {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(n), vars[n]))
		}
		c.indent--
		c.writeln("")
	}

	// Emit test blocks as procedures after variables.
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("begin")
	c.indent++
	c.buf.Write(body.Bytes())
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s;", name))
		}
	}
	c.indent--
	c.writeln("end.")
	out := c.buf.Bytes()
	return FormatPas(out), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Let != nil:
		var t types.Type
		if c.env != nil {
			if tt, err := c.env.GetVar(s.Let.Name); err == nil {
				t = tt
			}
		}
		if t == nil {
			if c.varTypes != nil {
				if sType, ok := c.varTypes[s.Let.Name]; ok {
					t = parsePasType(sType)
				}
			}
		}
		if t == nil {
			t = resolveSimpleTypeRef(s.Let.Type)
		}
		val, err := c.compileExprWith(t, s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s;", sanitizeName(s.Let.Name), val))
	case s.Var != nil:
		if s.Var.Value != nil {
			var t types.Type
			if c.env != nil {
				if tt, err := c.env.GetVar(s.Var.Name); err == nil {
					t = tt
				}
			}
			if t == nil {
				if c.varTypes != nil {
					if sType, ok := c.varTypes[s.Var.Name]; ok {
						t = parsePasType(sType)
					}
				}
			}
			if t == nil {
				t = resolveSimpleTypeRef(s.Var.Type)
			}
			val, err := c.compileExprWith(t, s.Var.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s := %s;", sanitizeName(s.Var.Name), val))
		}
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("result := %s;", val))
		c.writeln("exit;")
	case s.Assign != nil:
		name := sanitizeName(s.Assign.Name)
		if len(s.Assign.Index) == 1 {
			iv, err := c.compileExpr(s.Assign.Index[0].Start)
			if err != nil {
				return err
			}
			if c.env != nil {
				if t, err := c.env.GetVar(s.Assign.Name); err == nil {
					if _, ok := t.(types.MapType); ok {
						name = fmt.Sprintf("%s.KeyData[%s]", name, iv)
					} else {
						name = fmt.Sprintf("%s[%s]", name, iv)
					}
				} else {
					name = fmt.Sprintf("%s[%s]", name, iv)
				}
			} else {
				name = fmt.Sprintf("%s[%s]", name, iv)
			}
		} else {
			for _, idx := range s.Assign.Index {
				iv, err := c.compileExpr(idx.Start)
				if err != nil {
					return err
				}
				name = fmt.Sprintf("%s[%s]", name, iv)
			}
		}
		var t types.Type
		if c.env != nil {
			if tt, err := c.env.GetVar(s.Assign.Name); err == nil {
				t = tt
			}
		}
		if t == nil {
			if c.varTypes != nil {
				if sType, ok := c.varTypes[s.Assign.Name]; ok {
					t = parsePasType(sType)
				}
			}
		}
		if t == nil {
			t = resolveSimpleTypeRef(nil)
		}
		val, err := c.compileExprWith(t, s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s;", name, val))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Agent != nil, s.Stream != nil, s.Model != nil, s.On != nil, s.Emit != nil:
		return fmt.Errorf("agents and streams not supported")
	case s.ExternType != nil, s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil:
		// Ignore foreign declarations like extern functions
		return nil
	case s.Fact != nil, s.Rule != nil:
		return fmt.Errorf("logic programming constructs not supported")
	case s.Import != nil:
		if s.Import.Lang == nil {
			return c.compilePackageImport(s.Import)
		}
		p := strings.Trim(s.Import.Path, "\"")
		if p == "strings" || p == "math" {
			return nil
		}
		return fmt.Errorf("foreign imports not supported")
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
	default:
		return fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s := %s to %s - 1 do", name, start, end))
		c.writeln("begin")
		c.indent++
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end;")
		return nil
	}

	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	if c.isStringExpr(f.Source) {
		idx := c.newVar()
		c.writeln(fmt.Sprintf("for %s := 1 to Length(%s) do", idx, src))
		c.writeln("begin")
		c.indent++
		if f.Name != "_" {
			c.writeln(fmt.Sprintf("%s := %s[%s];", name, src, idx))
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end;")
		return nil
	}
	loopVar := name
	if f.Name == "_" {
		loopVar = c.newVar()
	}
	c.writeln(fmt.Sprintf("for %s in %s do", loopVar, src))
	c.writeln("begin")
	c.indent++
	// value is assigned automatically in for..in loop
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end;")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " do")
	c.writeln("begin")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end;")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	if len(stmt.Then) == 1 && stmt.ElseIf == nil && len(stmt.Else) == 0 {
		var buf bytes.Buffer
		prevBuf := c.buf
		c.buf = buf
		prevIndent := c.indent
		c.indent = 0
		if err := c.compileStmt(stmt.Then[0]); err != nil {
			c.buf = prevBuf
			c.indent = prevIndent
			return err
		}
		line := strings.TrimSpace(buf.String())
		c.buf = prevBuf
		c.indent = prevIndent
		if strings.HasSuffix(line, ";") {
			line = strings.TrimSuffix(line, ";")
		}
		c.writeln("if " + cond + " then " + line + ";")
		return nil
	}

	c.writeln("if " + cond + " then")
	c.writeln("begin")
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	cur := stmt
	for cur.ElseIf != nil {
		c.writeln("end else if " + c.mustExpr(cur.ElseIf.Cond) + " then")
		c.writeln("begin")
		c.indent++
		for _, st := range cur.ElseIf.Then {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		cur = cur.ElseIf
	}
	if len(cur.Else) > 0 {
		c.writeln("end else")
		c.writeln("begin")
		c.indent++
		for _, st := range cur.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("end;")
	return nil
}

func (c *Compiler) compileIfExpr(expr *parser.IfExpr) (string, error) {
	typ := "integer"
	if c.expected != nil {
		typ = typeString(c.expected)
	}
	tmp := c.newTypedVar(typ)

	cond, err := c.compileExpr(expr.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExprWith(parsePasType(typ), expr.Then)
	if err != nil {
		return "", err
	}

	c.writeln("if " + cond + " then")
	c.writeln("begin")
	c.indent++
	c.writeln(fmt.Sprintf("%s := %s;", tmp, thenVal))
	c.indent--
	cur := expr
	for cur.ElseIf != nil {
		c.writeln("end else if " + c.mustExpr(cur.ElseIf.Cond) + " then")
		c.writeln("begin")
		c.indent++
		val, err := c.compileExprWith(parsePasType(typ), cur.ElseIf.Then)
		if err != nil {
			return "", err
		}
		c.writeln(fmt.Sprintf("%s := %s;", tmp, val))
		c.indent--
		cur = cur.ElseIf
	}
	if cur.Else != nil {
		c.writeln("end else")
		c.writeln("begin")
		c.indent++
		val, err := c.compileExprWith(parsePasType(typ), cur.Else)
		if err != nil {
			return "", err
		}
		c.writeln(fmt.Sprintf("%s := %s;", tmp, val))
		c.indent--
	}
	c.writeln("end;")
	return tmp, nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	typ := "integer"
	if c.expected != nil {
		typ = typeString(c.expected)
	}
	res := c.newTypedVar(typ)

	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	ttype := typeString(types.TypeOfExpr(m.Target, c.env))
	if name, ok := selectorName(m.Target); ok {
		ttype = typeString(c.varType(name))
	}
	tv := c.newTypedVar(ttype)
	c.writeln(fmt.Sprintf("%s := %s;", tv, target))

	first := true
	var defaultExpr *parser.Expr
	for _, cs := range m.Cases {
		if isUnderscoreExpr(cs.Pattern) {
			defaultExpr = cs.Result
			continue
		}
		condVal, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		val, err := c.compileExprWith(parsePasType(typ), cs.Result)
		if err != nil {
			return "", err
		}
		if first {
			c.writeln(fmt.Sprintf("if %s = %s then", tv, condVal))
			first = false
		} else {
			c.writeln(fmt.Sprintf("else if %s = %s then", tv, condVal))
		}
		c.writeln("begin")
		c.indent++
		c.writeln(fmt.Sprintf("%s := %s;", res, val))
		c.indent--
	}
	if defaultExpr != nil {
		c.writeln("else")
		c.writeln("begin")
		c.indent++
		val, err := c.compileExprWith(parsePasType(typ), defaultExpr)
		if err != nil {
			return "", err
		}
		c.writeln(fmt.Sprintf("%s := %s;", res, val))
		c.indent--
	}
	c.writeln("end;")
	return res, nil
}

func (c *Compiler) mustExpr(e *parser.Expr) string {
	s, _ := c.compileExpr(e)
	return s
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		hasFields := false
		for _, v := range t.Variants {
			if len(v.Fields) > 0 {
				hasFields = true
				break
			}
		}
		if hasFields {
			return fmt.Errorf("union types not supported")
		}
		name := sanitizeName(t.Name)
		c.writeln(fmt.Sprintf("type %s = (", name))
		for i, v := range t.Variants {
			if i > 0 {
				c.buf.WriteString(", ")
			}
			c.buf.WriteString(sanitizeName(v.Name))
		}
		c.writeln(");")
		if c.env != nil {
			variants := map[string]types.StructType{}
			for _, v := range t.Variants {
				st := types.StructType{Name: v.Name, Fields: map[string]types.Type{}, Order: []string{}}
				variants[v.Name] = st
				c.env.SetStruct(v.Name, st)
				c.env.SetFuncType(v.Name, types.FuncType{Params: nil, Return: types.UnionType{Name: t.Name}})
			}
			c.env.SetUnion(t.Name, types.UnionType{Name: t.Name, Variants: variants})
		}
		return nil
	}
	name := sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("type %s = record", name))
	c.indent++
	fields := map[string]types.Type{}
	order := []string{}
	for _, m := range t.Members {
		if m.Field != nil {
			fname := sanitizeName(m.Field.Name)
			ft := resolveSimpleTypeRef(m.Field.Type)
			c.writeln(fmt.Sprintf("%s: %s;", fname, c.typeRef(m.Field.Type)))
			fields[m.Field.Name] = ft
			order = append(order, m.Field.Name)
		}
	}
	c.indent--
	c.writeln("end;")
	if c.env != nil {
		st := types.StructType{Name: t.Name, Fields: fields, Order: order}
		c.env.SetStruct(t.Name, st)
	}
	return nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	// Use a fresh temp var set for each function
	prevTemps := c.tempVars
	c.tempVars = make(map[string]string)
	prevRepl := c.replacements
	c.replacements = make(map[string]string)

	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	c.varTypes = map[string]string{}
	for i, p := range fun.Params {
		pt := c.typeRef(p.Type)
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), pt)
		c.varTypes[p.Name] = pt
	}
	retType := c.typeRef(fun.Return)

	// Compile body first so that temporaries are collected
	var body bytes.Buffer
	prevBuf := c.buf
	prevIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = prevIndent + 1
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			c.buf = prevBuf
			c.indent = prevIndent
			return err
		}
	}
	body = c.buf
	c.buf = prevBuf
	c.indent = prevIndent

	vars := map[string]string{}
	paramMap := map[string]string{}
	for k, v := range c.varTypes {
		paramMap[k] = v
	}
	collectVars(fun.Body, c.env, paramMap)
	for k, v := range paramMap {
		if _, ok := c.varTypes[k]; !ok { // exclude parameters
			vars[k] = v
		}
		c.varTypes[k] = v
	}
	// include generated temporaries
	for n, t := range c.tempVars {
		if t == "" {
			t = "integer"
		}
		vars[n] = t
		c.varTypes[n] = t
	}

	c.writeln(fmt.Sprintf("function %s(%s): %s;", name, strings.Join(params, "; "), retType))
	if len(vars) > 0 {
		c.writeln("var")
		c.indent++
		names := make([]string, 0, len(vars))
		for n := range vars {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(n), vars[n]))
		}
		c.indent--
	}
	c.writeln("begin")
	c.indent++
	c.buf.Write(body.Bytes())
	c.indent--
	c.writeln("end;")

	// restore temp vars for outer scope
	c.tempVars = prevTemps
	c.varTypes = nil
	c.replacements = prevRepl
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
			if s.Fun != nil && s.Fun.Export {
				name := s.Fun.Name
				s.Fun.Name = alias + "_" + name
				if err := c.compileFun(s.Fun); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
	}
	c.env = origEnv
	return nil
}

func (c *Compiler) typeRef(t *parser.TypeRef) string {
	if t == nil {
		return "integer"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "integer"
		case "float":
			return "double"
		case "string":
			return "string"
		case "bool":
			return "boolean"
		}
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.typeRef(p)
		}
		ret := c.typeRef(t.Fun.Return)
		if ret == "" || ret == "void" {
			return fmt.Sprintf("procedure(%s)", strings.Join(params, "; "))
		}
		return fmt.Sprintf("function(%s): %s", strings.Join(params, "; "), ret)
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			elem := c.typeRef(t.Generic.Args[0])
			return fmt.Sprintf("specialize TArray<%s>", elem)
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			keyT := c.typeRef(t.Generic.Args[0])
			valT := c.typeRef(t.Generic.Args[1])
			return fmt.Sprintf("specialize TFPGMap<%s, %s>", keyT, valT)
		}
	}
	if t.Simple != nil {
		return sanitizeName(*t.Simple)
	}
	return "integer"
}

func typeString(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		_ = tt
		return "integer"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "boolean"
	case types.ListType:
		lt := tt
		return fmt.Sprintf("specialize TArray<%s>", typeString(lt.Elem))
	case types.MapType:
		mt := tt
		return fmt.Sprintf("specialize TFPGMap<%s, %s>", typeString(mt.Key), typeString(mt.Value))
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.AnyType:
		return "Variant"
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = typeString(p)
		}
		if tt.Return == nil || tt.Return == (types.VoidType{}) {
			return fmt.Sprintf("procedure(%s)", strings.Join(params, "; "))
		}
		return fmt.Sprintf("function(%s): %s", strings.Join(params, "; "), typeString(tt.Return))
	default:
		return "integer"
	}
}

func collectVars(stmts []*parser.Statement, env *types.Env, vars map[string]string) {
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			typ := "integer"
			if env != nil {
				if t, err := env.GetVar(s.Let.Name); err == nil {
					typ = typeString(t)
				}
			}
			if typ == "integer" && s.Let.Type != nil {
				typ = typeString(resolveSimpleTypeRef(s.Let.Type))
			}
			if typ == "integer" && s.Let.Value != nil {
				typT := types.TypeOfExpr(s.Let.Value, env)
				typ = typeString(typT)
				if typ == "integer" && isStringSliceExpr(s.Let.Value, env, vars) {
					typ = "string"
				} else if typ == "integer" && isStringLiteral(s.Let.Value) {
					typ = "string"
				} else if typ == "integer" && isBoolLiteral(s.Let.Value) {
					typ = "boolean"
				} else if typ == "integer" && isListLiteral(s.Let.Value) {
					typ = "specialize TArray<integer>"
				}
			}
			vars[s.Let.Name] = typ
			collectQueryVars(s.Let.Value, env, vars)
		case s.Var != nil:
			typ := "integer"
			if env != nil {
				if t, err := env.GetVar(s.Var.Name); err == nil {
					typ = typeString(t)
				}
			}
			if typ == "integer" && s.Var.Type != nil {
				typ = typeString(resolveSimpleTypeRef(s.Var.Type))
			}
			if typ == "integer" && s.Var.Value != nil {
				typT := types.TypeOfExpr(s.Var.Value, env)
				typ = typeString(typT)
				if typ == "integer" && isStringSliceExpr(s.Var.Value, env, vars) {
					typ = "string"
				}
			}
			if typ == "integer" && isListLiteral(s.Var.Value) {
				typ = "specialize TArray<integer>"
			} else if typ == "integer" && isStringLiteral(s.Var.Value) {
				typ = "string"
			} else if typ == "integer" && isBoolLiteral(s.Var.Value) {
				typ = "boolean"
			}
			vars[s.Var.Name] = typ
			collectQueryVars(s.Var.Value, env, vars)
		case s.For != nil:
			if s.For.Name != "_" {
				typ := "integer"
				if env != nil {
					if t, err := env.GetVar(s.For.Name); err == nil {
						typ = typeString(t)
					}
				}
				if typ == "integer" {
					typT := types.TypeOfExpr(s.For.Source, env)
					typ = typeString(typT)
				}
				if strings.HasPrefix(typ, "specialize TArray<") {
					inner := strings.TrimSuffix(strings.TrimPrefix(typ, "specialize TArray<"), ">")
					typ = inner
				} else if typ == "string" {
					typ = "char"
				}
				vars[s.For.Name] = typ
			}
			collectVars(s.For.Body, env, vars)
		case s.While != nil:
			collectVars(s.While.Body, env, vars)
		case s.If != nil:
			collectVars(s.If.Then, env, vars)
			if s.If.ElseIf != nil {
				collectVars([]*parser.Statement{{If: s.If.ElseIf}}, env, vars)
			}
			collectVars(s.If.Else, env, vars)
		case s.Fun != nil:
			// ignore nested functions
		}
	}
}

func collectQueryVars(e *parser.Expr, env *types.Env, vars map[string]string) {
	if e == nil || e.Binary == nil {
		return
	}
	var scanUnary func(u *parser.Unary)
	var scanPrimary func(p *parser.Primary)
	var scanPostfix func(p *parser.PostfixExpr)

	scanUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		scanPostfix(u.Value)
	}
	scanPostfix = func(p *parser.PostfixExpr) {
		if p == nil {
			return
		}
		scanPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Index != nil {
				collectQueryVars(op.Index.Start, env, vars)
				collectQueryVars(op.Index.End, env, vars)
			} else if op.Call != nil {
				for _, a := range op.Call.Args {
					collectQueryVars(a, env, vars)
				}
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Query != nil:
			qt := types.TypeOfExpr(p.Query.Source, env)
			elem := "integer"
			if lt, ok := qt.(types.ListType); ok {
				elem = typeString(lt.Elem)
			}
			vars[p.Query.Var] = elem
			for _, f := range p.Query.Froms {
				ft := types.TypeOfExpr(f.Src, env)
				fe := "integer"
				if lt, ok := ft.(types.ListType); ok {
					fe = typeString(lt.Elem)
				}
				vars[f.Var] = fe
			}
			collectQueryVars(p.Query.Source, env, vars)
			for _, f := range p.Query.Froms {
				collectQueryVars(f.Src, env, vars)
			}
			collectQueryVars(p.Query.Select, env, vars)
			collectQueryVars(p.Query.Where, env, vars)
			collectQueryVars(p.Query.Skip, env, vars)
			collectQueryVars(p.Query.Take, env, vars)
		case p.FunExpr != nil:
			for _, st := range p.FunExpr.BlockBody {
				collectVars([]*parser.Statement{st}, env, vars)
			}
			collectQueryVars(p.FunExpr.ExprBody, env, vars)
		case p.Group != nil:
			collectQueryVars(p.Group, env, vars)
		case p.List != nil:
			for _, el := range p.List.Elems {
				collectQueryVars(el, env, vars)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				collectQueryVars(it.Key, env, vars)
				collectQueryVars(it.Value, env, vars)
			}
		case p.Selector != nil:
			// no-op
		case p.Call != nil:
			for _, a := range p.Call.Args {
				collectQueryVars(a, env, vars)
			}
		}
	}

	scanUnary(e.Binary.Left)
	for _, part := range e.Binary.Right {
		scanPostfix(part.Right)
	}
}

func (c *Compiler) compileExprWith(expected types.Type, e *parser.Expr) (string, error) {
	old := c.expected
	c.expected = expected
	res, err := c.compileExpr(e)
	c.expected = old
	return res, err
}

func resolveSimpleTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.IntType{}
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
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: resolveSimpleTypeRef(t.Generic.Args[0])}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{
				Key:   resolveSimpleTypeRef(t.Generic.Args[0]),
				Value: resolveSimpleTypeRef(t.Generic.Args[1]),
			}
		}
	}
	return types.IntType{}
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	operands := []string{}
	lists := []bool{}
	maps := []bool{}

	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, left)
	lists = append(lists, c.isListUnary(b.Left))
	maps = append(maps, c.isMapUnary(b.Left))
	operators := []string{}

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		lists = append(lists, c.isListPostfix(part.Right))
		maps = append(maps, c.isMapPostfix(part.Right))
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		operators = append(operators, op)
	}

	levels := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}, {"union_all"}, {"intersect"}}

	for _, level := range levels {
		for i := 0; i < len(operators); {
			op := operators[i]
			if contains(level, op) {
				l := operands[i]
				r := operands[i+1]
				llist := lists[i]
				rlist := lists[i+1]
				var res string
				var isList bool
				switch op {
				case "+":
					if llist || rlist {
						res = fmt.Sprintf("Concat(%s, %s)", l, r)
						isList = true
					} else {
						res = fmt.Sprintf("%s + %s", l, r)
					}
				case "-":
					res = fmt.Sprintf("%s - %s", l, r)
				case "*":
					res = fmt.Sprintf("%s * %s", l, r)
				case "/":
					if strings.Contains(l, ".") || strings.Contains(r, ".") || strings.Contains(l, "Double(") || strings.Contains(r, "Double(") {
						res = fmt.Sprintf("%s / %s", l, r)
					} else {
						res = fmt.Sprintf("%s div %s", l, r)
					}
				case "%":
					res = fmt.Sprintf("%s mod %s", l, r)
				case "<", "<=", ">", ">=":
					res = fmt.Sprintf("(%s %s %s)", l, op, r)
				case "==":
					res = fmt.Sprintf("(%s = %s)", l, r)
				case "!=":
					res = fmt.Sprintf("(%s <> %s)", l, r)
				case "in":
					if maps[i+1] {
						res = fmt.Sprintf("(%s.IndexOf(%s) >= 0)", r, l)
					} else {
						res = fmt.Sprintf("(%s in %s)", l, r)
					}
				case "&&":
					res = fmt.Sprintf("(%s and %s)", l, r)
				case "||":
					res = fmt.Sprintf("(%s or %s)", l, r)
				case "union_all":
					res = fmt.Sprintf("Concat(%s, %s)", l, r)
					isList = true
				case "intersect":
					c.use("_intersect")
					res = fmt.Sprintf("specialize _intersect<Variant>(%s, %s)", l, r)
					isList = true
				}
				operands[i] = res
				lists[i] = isList
				operands = append(operands[:i+1], operands[i+2:]...)
				lists = append(lists[:i+1], lists[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected expression state")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			expr = "-" + expr
		case "!":
			expr = "not " + expr
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.End != nil || op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					sidx, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = sidx
				}
				end := fmt.Sprintf("Length(%s)", expr)
				if op.Index.End != nil {
					eidx, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = eidx
				}
				if c.isStringPostfix(&parser.PostfixExpr{Target: p.Target}) {
					c.use("_sliceString")
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				} else if c.isListPostfix(&parser.PostfixExpr{Target: p.Target}) {
					elem := c.listElemType(p.Target)
					c.use("_sliceList")
					expr = fmt.Sprintf("specialize _sliceList<%s>(%s, %s, %s)", elem, expr, start, end)
				} else {
					begin := start
					if begin != "0" {
						begin = fmt.Sprintf("%s + 1", begin)
					} else {
						begin = "1"
					}
					length := fmt.Sprintf("%s - %s", end, start)
					expr = fmt.Sprintf("Copy(%s, %s, %s)", expr, begin, length)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isMapPostfix(&parser.PostfixExpr{Target: p.Target}) {
					expr = fmt.Sprintf("%s.KeyData[%s]", expr, idx)
				} else if c.isStringPostfix(&parser.PostfixExpr{Target: p.Target}) {
					c.use("_indexString")
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else if c.isListPostfix(&parser.PostfixExpr{Target: p.Target}) {
					elem := c.listElemType(p.Target)
					c.use("_indexList")
					expr = fmt.Sprintf("specialize _indexList<%s>(%s, %s)", elem, expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
			}
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
		} else if op.Cast != nil {
			typ := c.typeRef(op.Cast.Type)
			if typ == "double" {
				expr = fmt.Sprintf("Double(%s)", expr)
			} else {
				expr = fmt.Sprintf("Trunc(%s)", expr)
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return fmt.Sprintf("%g", *p.Lit.Float), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "True", nil
			}
			return "False", nil
		}
		if p.Lit.Str != nil {
			s := strings.ReplaceAll(*p.Lit.Str, "'", "''")
			return fmt.Sprintf("'%s'", s), nil
		}
		if p.Lit.Null {
			return "Null", nil
		}
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if rep, ok := c.replacements[name]; ok && len(p.Selector.Tail) == 0 {
			return rep, nil
		}
		if c.packages != nil {
			if c.packages[name] {
				parts := make([]string, len(p.Selector.Tail))
				for i, part := range p.Selector.Tail {
					parts[i] = sanitizeName(part)
				}
				return name + "_" + strings.Join(parts, "_"), nil
			}
		}
		for _, part := range p.Selector.Tail {
			name += "." + sanitizeName(part)
		}
		return name, nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		argStr := strings.Join(args, ", ")
		switch p.Call.Func {
		case "count":
			if len(args) != 1 {
				return "", fmt.Errorf("count expects 1 argument")
			}
			t := types.TypeOfExpr(p.Call.Args[0], c.env)
			switch tt := t.(type) {
			case types.GroupType:
				elem := typeString(tt.Elem)
				c.use("_countGroup")
				return fmt.Sprintf("specialize _countGroup<%s>(%s)", elem, args[0]), nil
			case types.ListType:
				elem := typeString(tt.Elem)
				c.use("_countList")
				return fmt.Sprintf("specialize _countList<%s>(%s)", elem, args[0]), nil
			case types.StringType:
				return fmt.Sprintf("Length(%s)", args[0]), nil
			default:
				return fmt.Sprintf("Length(%s)", args[0]), nil
			}
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects 1 argument")
			}
			t := types.TypeOfExpr(p.Call.Args[0], c.env)
			switch tt := t.(type) {
			case types.GroupType:
				elem := typeString(tt.Elem)
				c.use("_sumList")
				return fmt.Sprintf("specialize _sumList<%s>(%s.Items)", elem, args[0]), nil
			case types.ListType:
				elem := typeString(tt.Elem)
				c.use("_sumList")
				return fmt.Sprintf("specialize _sumList<%s>(%s)", elem, args[0]), nil
			default:
				return "0", nil
			}
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 argument")
			}
			t := types.TypeOfExpr(p.Call.Args[0], c.env)
			switch tt := t.(type) {
			case types.GroupType:
				elem := typeString(tt.Elem)
				c.use("_avgList")
				return fmt.Sprintf("specialize _avgList<%s>(%s.Items)", elem, args[0]), nil
			case types.ListType:
				elem := typeString(tt.Elem)
				c.use("_avgList")
				return fmt.Sprintf("specialize _avgList<%s>(%s)", elem, args[0]), nil
			default:
				return "0", nil
			}
		case "len":
			return fmt.Sprintf("Length(%s)", argStr), nil
		case "print":
			if len(args) == 0 {
				return "writeln()", nil
			}
			withSpaces := make([]string, 0, len(args)*2-1)
			withSpaces = append(withSpaces, args[0])
			for _, a := range args[1:] {
				withSpaces = append(withSpaces, "' '", a)
			}
			return fmt.Sprintf("writeln(%s)", strings.Join(withSpaces, ", ")), nil
		case "abs":
			if len(args) != 1 {
				return "", fmt.Errorf("abs expects 1 argument")
			}
			return fmt.Sprintf("Abs(%s)", args[0]), nil
		case "str":
			if len(args) == 1 {
				t := typeString(types.TypeOfExpr(p.Call.Args[0], c.env))
				if t == "double" {
					return fmt.Sprintf("FloatToStr(%s)", args[0]), nil
				}
				return fmt.Sprintf("IntToStr(%s)", args[0]), nil
			}
			return fmt.Sprintf("IntToStr(%s)", argStr), nil
		case "upper":
			if len(args) != 1 {
				return "", fmt.Errorf("upper expects 1 argument")
			}
			return fmt.Sprintf("UpperCase(%s)", args[0]), nil
		case "lower":
			if len(args) != 1 {
				return "", fmt.Errorf("lower expects 1 argument")
			}
			return fmt.Sprintf("LowerCase(%s)", args[0]), nil
		case "trim":
			if len(args) != 1 {
				return "", fmt.Errorf("trim expects 1 argument")
			}
			return fmt.Sprintf("Trim(%s)", args[0]), nil
		case "concat":
			if len(args) < 2 {
				return "", fmt.Errorf("concat expects at least 2 args")
			}
			expr := fmt.Sprintf("Concat(%s, %s)", args[0], args[1])
			for i := 2; i < len(args); i++ {
				expr = fmt.Sprintf("Concat(%s, %s)", expr, args[i])
			}
			return expr, nil
		case "first":
			if len(args) != 1 {
				return "", fmt.Errorf("first expects 1 argument")
			}
			t := types.TypeOfExpr(p.Call.Args[0], c.env)
			switch tt := t.(type) {
			case types.ListType:
				elem := typeString(tt.Elem)
				c.use("_first")
				return fmt.Sprintf("specialize _first<%s>(%s)", elem, args[0]), nil
			case types.GroupType:
				elem := typeString(tt.Elem)
				c.use("_first")
				return fmt.Sprintf("specialize _first<%s>(%s.Items)", elem, args[0]), nil
			default:
				c.use("_first")
				return fmt.Sprintf("specialize _first<Variant>(%s)", args[0]), nil
			}
		case "contains":
			if len(args) != 2 {
				return "", fmt.Errorf("contains expects 2 arguments")
			}
			return fmt.Sprintf("(Pos(%s, %s) > 0)", args[1], args[0]), nil
		case "substr", "substring":
			if len(args) != 3 {
				return "", fmt.Errorf("substr expects 3 args")
			}
			c.use("_sliceString")
			end := fmt.Sprintf("%s + %s", args[1], args[2])
			return fmt.Sprintf("_sliceString(%s, %s, %s)", args[0], args[1], end), nil
		case "reverse":
			if len(args) != 1 {
				return "", fmt.Errorf("reverse expects 1 arg")
			}
			if c.isStringExpr(p.Call.Args[0]) {
				c.use("_reverseString")
				return fmt.Sprintf("_reverseString(%s)", args[0]), nil
			}
			if c.isListExpr(p.Call.Args[0]) {
				if lt, ok := types.TypeOfExpr(p.Call.Args[0], c.env).(types.ListType); ok {
					elem := typeString(lt.Elem)
					c.use("_reverseList")
					return fmt.Sprintf("specialize _reverseList<%s>(%s)", elem, args[0]), nil
				}
			}
			return args[0], nil
		case "split":
			if len(args) != 2 {
				return "", fmt.Errorf("split expects 2 arguments")
			}
			c.use("_splitString")
			return fmt.Sprintf("_splitString(%s, %s)", args[0], args[1]), nil
		case "join":
			if len(args) != 2 {
				return "", fmt.Errorf("join expects 2 arguments")
			}
			c.use("_joinStrings")
			return fmt.Sprintf("_joinStrings(%s, %s)", args[0], args[1]), nil
		case "strings.ToUpper":
			if len(args) != 1 {
				return "", fmt.Errorf("strings.ToUpper expects 1 arg")
			}
			return fmt.Sprintf("UpperCase(%s)", args[0]), nil
		case "math.sqrt":
			if len(args) != 1 {
				return "", fmt.Errorf("math.sqrt expects 1 argument")
			}
			return fmt.Sprintf("Sqrt(%s)", args[0]), nil
		case "json":
			if len(args) != 1 {
				return "", fmt.Errorf("json expects 1 argument")
			}
			typ := typeString(types.TypeOfExpr(p.Call.Args[0], c.env))
			c.use("_json")
			return fmt.Sprintf("specialize _json<%s>(%s)", typ, args[0]), nil
		default:
			return fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), argStr), nil
		}
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Struct != nil:
		if len(p.Struct.Fields) == 0 {
			if ut, ok := c.expected.(types.UnionType); ok {
				if _, ok := ut.Variants[p.Struct.Name]; ok {
					return sanitizeName(p.Struct.Name), nil
				}
			}
			if _, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
				return sanitizeName(p.Struct.Name), nil
			}
		}
		typ := sanitizeName(p.Struct.Name)
		tmp := c.newTypedVar(typ)
		for _, f := range p.Struct.Fields {
			val, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			c.writeln(fmt.Sprintf("%s.%s := %s;", tmp, sanitizeName(f.Name), val))
		}
		return tmp, nil
	case p.List != nil:
		elemType := "integer"
		var elemT types.Type
		if lt, ok := c.expected.(types.ListType); ok {
			elemType = typeString(lt.Elem)
			elemT = lt.Elem
		} else if len(p.List.Elems) > 0 {
			elemType = typeString(types.TypeOfExpr(p.List.Elems[0], c.env))
		}
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExprWith(elemT, e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("specialize TArray<%s>([%s])", elemType, strings.Join(elems, ", ")), nil
	case p.Map != nil:
		// infer key/value types from first element if available
		keyType := "string"
		valType := "integer"
		if len(p.Map.Items) > 0 {
			keyType = typeString(types.TypeOfExpr(p.Map.Items[0].Key, c.env))
			valType = typeString(types.TypeOfExpr(p.Map.Items[0].Value, c.env))
			if valType != "integer" {
				valType = "Variant"
			} else {
				for _, it := range p.Map.Items[1:] {
					vt := typeString(types.TypeOfExpr(it.Value, c.env))
					if vt != "integer" {
						valType = "Variant"
						break
					}
				}
			}
		}
		tmp := c.newTypedVar(fmt.Sprintf("specialize TFPGMap<%s, %s>", keyType, valType))
		c.writeln(fmt.Sprintf("%s := specialize TFPGMap<%s, %s>.Create;", tmp, keyType, valType))
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if name, ok := selectorName(it.Key); ok {
				k = fmt.Sprintf("'%s'", name)
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			c.writeln(fmt.Sprintf("%s.AddOrSetData(%s, %s);", tmp, k, v))
		}
		return tmp, nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.LogicQuery != nil:
		return "", fmt.Errorf("logic queries not supported")
	case p.Generate != nil:
		return "", fmt.Errorf("generative model expressions not supported")
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	// simple grouping without joins
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		orig := c.env
		child := types.NewEnv(c.env)
		srcType := types.TypeOfExpr(q.Source, c.env)
		var elem types.Type = types.AnyType{}
		if lt, ok := srcType.(types.ListType); ok {
			elem = lt.Elem
		}
		child.SetVar(q.Var, elem, true)
		c.env = child
		var condStr string
		if q.Where != nil {
			cs, err := c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
			condStr = cs
		}
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
		c.env = orig
		elemType := typeString(types.TypeOfExpr(q.Select, genv))
		if elemType == "" {
			elemType = "integer"
		}
		groupType := typeString(elem)
		filtered := c.newTypedVar(fmt.Sprintf("specialize TArray<%s>", groupType))
		c.writeln(fmt.Sprintf("SetLength(%s, 0);", filtered))
		c.writeln(fmt.Sprintf("for %s in %s do", sanitizeName(q.Var), src))
		c.writeln("begin")
		c.indent++
		if condStr != "" {
			c.writeln(fmt.Sprintf("if not (%s) then continue;", condStr))
		}
		c.writeln(fmt.Sprintf("%s := Concat(%s, [%s]);", filtered, filtered, sanitizeName(q.Var)))
		c.indent--
		c.writeln("end;")

		tmpGrp := c.newTypedVar(fmt.Sprintf("specialize TArray<specialize _Group<%s>>", groupType))
		c.use("_group_by")
		c.use("_Group")
		c.writeln(fmt.Sprintf("%s := specialize _group_by<%s>(%s, function(%s: %s): Variant begin Result := %s end);", tmpGrp, groupType, filtered, sanitizeName(q.Var), groupType, keyExpr))
		tmpRes := c.newTypedVar(fmt.Sprintf("specialize TArray<%s>", elemType))
		c.writeln(fmt.Sprintf("SetLength(%s, 0);", tmpRes))
		c.writeln(fmt.Sprintf("for %s in %s do", sanitizeName(q.Group.Name), tmpGrp))
		c.writeln("begin")
		c.indent++
		c.writeln(fmt.Sprintf("%s := Concat(%s, [%s]);", tmpRes, tmpRes, valExpr))
		c.indent--
		c.writeln("end;")
		return tmpRes, nil
	}

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
	joinSrcs := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
	}

	orig := c.env
	child := types.NewEnv(c.env)
	srcType := types.TypeOfExpr(q.Source, c.env)
	var elem types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elem = lt.Elem
	}
	child.SetVar(q.Var, elem, true)
	for _, f := range q.Froms {
		ft := types.TypeOfExpr(f.Src, child)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := types.TypeOfExpr(j.Src, child)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
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
	var condParts []string
	var sortStr, skipStr, takeStr string
	if q.Where != nil {
		cs, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
		if cs != "" {
			condParts = append(condParts, cs)
		}
	}
	if q.Sort != nil {
		sortStr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipStr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeStr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	condStr := strings.Join(condParts, " and ")
	elemType := typeString(types.TypeOfExpr(q.Select, child))
	if elemType == "" {
		elemType = "integer"
	}
	c.env = orig

	tmp := c.newTypedVar(fmt.Sprintf("specialize TArray<%s>", elemType))
	c.writeln(fmt.Sprintf("SetLength(%s, 0);", tmp))
	var keyVar string
	if sortStr != "" {
		keyVar = c.newTypedVar("specialize TArray<Variant>")
		c.writeln(fmt.Sprintf("SetLength(%s, 0);", keyVar))
	}
	c.writeln(fmt.Sprintf("for %s in %s do", sanitizeName(q.Var), src))
	c.writeln("begin")
	c.indent++
	for i, fs := range fromSrcs {
		c.writeln(fmt.Sprintf("for %s in %s do", sanitizeName(q.Froms[i].Var), fs))
		c.writeln("begin")
		c.indent++
	}
	for i, js := range joinSrcs {
		c.writeln(fmt.Sprintf("for %s in %s do", sanitizeName(q.Joins[i].Var), js))
		c.writeln("begin")
		c.indent++
		if joinOns[i] != "" {
			c.writeln(fmt.Sprintf("if not (%s) then continue;", joinOns[i]))
		}
	}
	if condStr != "" {
		c.writeln(fmt.Sprintf("if not (%s) then continue;", condStr))
	}
	c.writeln(fmt.Sprintf("%s := Concat(%s, [%s]);", tmp, tmp, sel))
	if sortStr != "" {
		c.writeln(fmt.Sprintf("%s := Concat(%s, [%s]);", keyVar, keyVar, sortStr))
	}
	// condStr does not open a begin/end block anymore
	for range joinSrcs {
		c.indent--
		c.writeln("end;")
	}
	for range fromSrcs {
		c.indent--
		c.writeln("end;")
	}
	c.indent--
	c.writeln("end;")

	result := tmp
	if sortStr != "" {
		c.use("_sortBy")
		c.writeln(fmt.Sprintf("specialize _sortBy<%s>(%s, %s);", elemType, result, keyVar))
	}
	if skipStr != "" {
		c.use("_sliceList")
		out := c.newTypedVar(fmt.Sprintf("specialize TArray<%s>", elemType))
		c.writeln(fmt.Sprintf("%s := specialize _sliceList<%s>(%s, %s, Length(%s));", out, elemType, result, skipStr, result))
		result = out
	}
	if takeStr != "" {
		c.use("_sliceList")
		out := c.newTypedVar(fmt.Sprintf("specialize TArray<%s>", elemType))
		c.writeln(fmt.Sprintf("%s := specialize _sliceList<%s>(%s, 0, %s);", out, elemType, result, takeStr))
		result = out
	}
	return result, nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	optsStr := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		optsStr = v
	}
	if st, ok := c.expected.(types.StructType); ok {
		c.use("_fetchJSON")
		typ := sanitizeName(st.Name)
		return fmt.Sprintf("specialize _fetchJSON<%s>(%s, %s)", typ, urlStr, optsStr), nil
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, optsStr), nil
}

func formatOption(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return ""
	}
	for _, it := range u.Value.Target.Map.Items {
		if name, ok := selectorName(it.Key); ok && name == "format" {
			if it.Value != nil && it.Value.Binary != nil && len(it.Value.Binary.Right) == 0 {
				if lit := it.Value.Binary.Left.Value.Target.Lit; lit != nil && lit.Str != nil {
					return *lit.Str
				}
			}
		}
	}
	return ""
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	typ := "integer"
	if l.Type != nil {
		if l.Type.Simple != nil {
			if _, ok := c.env.GetStruct(*l.Type.Simple); ok {
				typ = sanitizeName(*l.Type.Simple)
			} else {
				typ = c.typeRef(l.Type)
			}
		} else {
			typ = c.typeRef(l.Type)
		}
	}
	fmtOpt := formatOption(l.With)
	switch fmtOpt {
	case "json":
		c.use("_loadJSON")
		return fmt.Sprintf("specialize _loadJSON<%s>(%s)", typ, path), nil
	case "jsonl":
		c.use("_loadJSONL")
		return fmt.Sprintf("specialize _loadJSONL<%s>(%s)", typ, path), nil
	default:
		c.use("_load")
		return fmt.Sprintf("_load(%s)", path), nil
	}
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
	fmtOpt := formatOption(s.With)
	switch fmtOpt {
	case "json":
		c.use("_saveJSON")
		return fmt.Sprintf("_saveJSON(%s, %s)", src, path), nil
	case "jsonl":
		c.use("_saveJSONL")
		return fmt.Sprintf("_saveJSONL(%s, %s)", src, path), nil
	default:
		c.use("_save")
		return fmt.Sprintf("_save(%s, %s)", src, path), nil
	}
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	idx := c.newVar()
	var elem types.Type = types.AnyType{}
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				elem = lt.Elem
			}
		}
	}
	elemType := typeString(elem)
	item := c.newTypedVar(elemType)

	c.writeln(fmt.Sprintf("for %s := 0 to High(%s) do", idx, list))
	c.writeln("begin")
	c.indent++
	c.writeln(fmt.Sprintf("%s := %s[%s];", item, list, idx))

	orig := c.env
	prevRepl := c.replacements
	c.replacements = make(map[string]string)
	if st, ok := elem.(types.StructType); ok {
		child := types.NewEnv(c.env)
		for n, t := range st.Fields {
			child.SetVar(n, t, true)
			c.replacements[n] = fmt.Sprintf("%s.%s", item, sanitizeName(n))
		}
		c.env = child
	}
	cond := "True"
	if u.Where != nil {
		cs, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = orig
			return err
		}
		cond = cs
	}
	c.writeln("if " + cond + " then")
	c.writeln("begin")
	c.indent++
	for _, it := range u.Set.Items {
		if key, ok := stringKey(it.Key); ok {
			val, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln(fmt.Sprintf("%s.%s := %s;", item, sanitizeName(key), val))
		}
	}
	c.indent--
	c.writeln("end;")
	c.env = orig
	c.replacements = prevRepl

	c.writeln(fmt.Sprintf("%s[%s] := %s;", list, idx, item))
	c.indent--
	c.writeln("end;")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if not (%s) then raise Exception.Create('expect failed');", expr))
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	prevTemps := c.tempVars
	c.tempVars = make(map[string]string)
	prevRepl := c.replacements
	c.replacements = make(map[string]string)

	name := "test_" + sanitizeName(t.Name)

	var body bytes.Buffer
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = oldIndent + 1
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			c.buf = oldBuf
			c.indent = oldIndent
			c.tempVars = prevTemps
			return err
		}
	}
	body = c.buf
	c.buf = oldBuf
	c.indent = oldIndent

	vars := map[string]string{}
	collectVars(t.Body, c.env, vars)
	for n, typ := range c.tempVars {
		if typ == "" {
			typ = "integer"
		}
		vars[n] = typ
	}

	c.writeln(fmt.Sprintf("procedure %s;", name))
	if len(vars) > 0 {
		c.writeln("var")
		c.indent++
		names := make([]string, 0, len(vars))
		for n := range vars {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(n), vars[n]))
		}
		c.indent--
	}
	c.writeln("begin")
	c.indent++
	c.buf.Write(body.Bytes())
	c.indent--
	c.writeln("end;")

	c.tempVars = prevTemps
	c.replacements = prevRepl
	return nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	name := fmt.Sprintf("_lambda%d", len(c.lambdas))
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 0
	var body []*parser.Statement
	if fn.ExprBody != nil {
		body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fn.ExprBody}}}
	} else {
		body = fn.BlockBody
	}
	fs := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: body}
	if err := c.compileFun(fs); err != nil {
		c.buf = oldBuf
		c.indent = oldIndent
		return "", err
	}
	code := c.buf.String()
	c.lambdas = append(c.lambdas, code)
	c.buf = oldBuf
	c.indent = oldIndent
	if _, ok := c.expected.(types.FuncType); ok {
		return "@" + name, nil
	}
	return name, nil
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitHelpers() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		switch n {
		case "_fetch":
			c.writeln("function _fetch(url: string; opts: specialize TFPGMap<string, Variant>): string;")
			c.writeln("var client: TFPHTTPClient; sl: TStringList;")
			c.writeln("begin")
			c.indent++
			c.writeln("if Pos('file://', url) = 1 then")
			c.indent++
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("try")
			c.indent++
			c.writeln("sl.LoadFromFile(Copy(url, 8, Length(url)));")
			c.writeln("Result := sl.Text;")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end")
			c.indent--
			c.writeln("else")
			c.indent++
			c.writeln("begin")
			c.indent++
			c.writeln("client := TFPHTTPClient.Create(nil);")
			c.writeln("try")
			c.indent++
			c.writeln("Result := client.Get(url);")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("client.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_fetchJSON":
			c.writeln("generic function _fetchJSON<T>(url: string; opts: specialize TFPGMap<string, Variant>): T;")
			c.writeln("var data: string; client: TFPHTTPClient; ds: TJSONDeStreamer;")
			c.writeln("begin")
			c.indent++
			c.writeln("data := _fetch(url, opts);")
			c.writeln("ds := TJSONDeStreamer.Create(nil);")
			c.writeln("try")
			c.indent++
			c.writeln("ds.JSONToObject(GetJSON(data), @Result, TypeInfo(T));")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("ds.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_indexString":
			c.writeln("function _indexString(s: string; i: integer): string;")
			c.writeln("begin")
			c.indent++
			c.writeln("if i < 0 then i := Length(s) + i;")
			c.writeln("if (i < 0) or (i >= Length(s)) then")
			c.indent++
			c.writeln("raise Exception.Create('index out of range');")
			c.indent--
			c.writeln("Result := s[i + 1];")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_sliceString":
			c.writeln("function _sliceString(s: string; i, j: integer): string;")
			c.writeln("var start_, end_, n: integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("start_ := i;")
			c.writeln("end_ := j;")
			c.writeln("n := Length(s);")
			c.writeln("if start_ < 0 then start_ := n + start_;")
			c.writeln("if end_ < 0 then end_ := n + end_;")
			c.writeln("if start_ < 0 then start_ := 0;")
			c.writeln("if end_ > n then end_ := n;")
			c.writeln("if end_ < start_ then end_ := start_;")
			c.writeln("Result := Copy(s, start_ + 1, end_ - start_);")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_load":
			c.writeln("function _load(path: string): specialize TArray<string>;")
			c.writeln("var sl: TStringList; i: Integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("try")
			c.indent++
			c.writeln("sl.LoadFromFile(path);")
			c.writeln("SetLength(Result, sl.Count);")
			c.writeln("for i := 0 to sl.Count - 1 do")
			c.indent++
			c.writeln("Result[i] := sl[i];")
			c.indent--
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_save":
			c.writeln("procedure _save(data: specialize TArray<string>; path: string);")
			c.writeln("var sl: TStringList; i: Integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("try")
			c.indent++
			c.writeln("for i := 0 to High(data) do")
			c.indent++
			c.writeln("sl.Add(data[i]);")
			c.indent--
			c.writeln("sl.SaveToFile(path);")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_loadJSON":
			c.writeln("generic function _loadJSON<T>(path: string): specialize TArray<T>;")
			c.writeln("var sl: TStringList; data: TJSONData; arr: TJSONArray; i: Integer; ds: TJSONDeStreamer;")
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("try")
			c.indent++
			c.writeln("sl.LoadFromFile(path);")
			c.writeln("data := GetJSON(sl.Text);")
			c.writeln("if data.JSONType = jtArray then")
			c.indent++
			c.writeln("arr := TJSONArray(data)")
			c.indent--
			c.writeln("else")
			c.indent++
			c.writeln("arr := TJSONArray.Create([data]);")
			c.indent--
			c.writeln("SetLength(Result, arr.Count);")
			c.writeln("ds := TJSONDeStreamer.Create(nil);")
			c.writeln("try")
			c.indent++
			c.writeln("for i := 0 to arr.Count - 1 do")
			c.indent++
			c.writeln("ds.JSONToObject(arr.Objects[i], @Result[i], TypeInfo(T));")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("ds.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_loadJSONL":
			c.writeln("generic function _loadJSONL<T>(path: string): specialize TArray<T>;")
			c.writeln("var sl: TStringList; i: Integer; ds: TJSONDeStreamer;")
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("try")
			c.indent++
			c.writeln("sl.LoadFromFile(path);")
			c.writeln("SetLength(Result, sl.Count);")
			c.writeln("ds := TJSONDeStreamer.Create(nil);")
			c.writeln("try")
			c.indent++
			c.writeln("for i := 0 to sl.Count - 1 do")
			c.indent++
			c.writeln("ds.JSONToObject(GetJSON(sl[i]), @Result[i], TypeInfo(T));")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("ds.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_saveJSON":
			c.writeln("generic procedure _saveJSON<T>(data: specialize TArray<T>; path: string);")
			c.writeln("var sl: TStringList; i: Integer; ds: TJSONStreamer;")
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("ds := TJSONStreamer.Create(nil);")
			c.writeln("try")
			c.indent++
			c.writeln("sl.Add('[');")
			c.writeln("for i := 0 to High(data) do")
			c.writeln("begin")
			c.indent++
			c.writeln("sl.Add(ds.ObjectToJSONString(@data[i], TypeInfo(T)));")
			c.writeln("if i < High(data) then sl[sl.Count-1] := sl[sl.Count-1] + ',';")
			c.indent--
			c.writeln("end;")
			c.writeln("sl.Add(']');")
			c.writeln("sl.SaveToFile(path);")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("ds.Free;")
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_saveJSONL":
			c.writeln("generic procedure _saveJSONL<T>(data: specialize TArray<T>; path: string);")
			c.writeln("var sl: TStringList; i: Integer; ds: TJSONStreamer;")
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("ds := TJSONStreamer.Create(nil);")
			c.writeln("try")
			c.indent++
			c.writeln("for i := 0 to High(data) do")
			c.writeln("begin")
			c.indent++
			c.writeln("sl.Add(ds.ObjectToJSONString(@data[i], TypeInfo(T)));")
			c.indent--
			c.writeln("end;")
			c.writeln("sl.SaveToFile(path);")
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("ds.Free;")
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_json":
			c.writeln("generic procedure _json<T>(v: T);")
			c.writeln("begin")
			c.indent++
			c.writeln("writeln('[]');")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_splitString":
			c.writeln("function _splitString(s, sep: string): specialize TArray<string>;")
			c.writeln("var sl: TStringList; i: Integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("sl := TStringList.Create;")
			c.writeln("try")
			c.indent++
			c.writeln("sl.Delimiter := sep[1];")
			c.writeln("sl.StrictDelimiter := True;")
			c.writeln("sl.DelimitedText := s;")
			c.writeln("SetLength(Result, sl.Count);")
			c.writeln("for i := 0 to sl.Count - 1 do")
			c.indent++
			c.writeln("Result[i] := sl[i];")
			c.indent--
			c.indent--
			c.writeln("finally")
			c.indent++
			c.writeln("sl.Free;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_joinStrings":
			c.writeln("function _joinStrings(parts: specialize TArray<string>; sep: string): string;")
			c.writeln("var i: Integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("Result := '';")
			c.writeln("for i := 0 to High(parts) do")
			c.writeln("begin")
			c.indent++
			c.writeln("if i > 0 then Result := Result + sep;")
			c.writeln("Result := Result + parts[i];")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_reverseString":
			c.writeln("function _reverseString(s: string): string;")
			c.writeln("var i: Integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("SetLength(Result, Length(s));")
			c.writeln("for i := 1 to Length(s) do")
			c.writeln("  Result[i] := s[Length(s) - i + 1];")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_reverseList":
			c.writeln("generic function _reverseList<T>(arr: specialize TArray<T>): specialize TArray<T>;")
			c.writeln("var i: Integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("SetLength(Result, Length(arr));")
			c.writeln("for i := 0 to High(arr) do")
			c.writeln("  Result[i] := arr[High(arr) - i];")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_concat":
			c.writeln("generic function _concat<T>(a, b: specialize TArray<T>): specialize TArray<T>;")
			c.writeln("var i,n: Integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("SetLength(Result, Length(a) + Length(b));")
			c.writeln("for i := 0 to High(a) do")
			c.writeln("  Result[i] := a[i];")
			c.writeln("n := Length(a);")
			c.writeln("for i := 0 to High(b) do")
			c.writeln("  Result[n + i] := b[i];")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_intersect":
			c.writeln("generic function _intersect<T>(a, b: specialize TArray<T>): specialize TArray<T>;")
			c.writeln("var i,j,k: Integer; inB, exists: Boolean;")
			c.writeln("begin")
			c.indent++
			c.writeln("SetLength(Result, 0);")
			c.writeln("for i := 0 to High(a) do")
			c.writeln("begin")
			c.indent++
			c.writeln("inB := False;")
			c.writeln("for j := 0 to High(b) do")
			c.writeln("  if a[i] = b[j] then begin inB := True; Break; end;")
			c.writeln("if inB then")
			c.writeln("begin")
			c.indent++
			c.writeln("exists := False;")
			c.writeln("for k := 0 to High(Result) do")
			c.writeln("  if a[i] = Result[k] then begin exists := True; Break; end;")
			c.writeln("if not exists then")
			c.writeln("begin")
			c.indent++
			c.writeln("SetLength(Result, Length(Result)+1);")
			c.writeln("Result[High(Result)] := a[i];")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_countList":
			c.writeln("generic function _countList<T>(arr: specialize TArray<T>): integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("Result := Length(arr);")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_countGroup":
			c.writeln("generic function _countGroup<T>(g: specialize _Group<T>): integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("Result := Length(g.Items);")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_sumList":
			c.writeln("generic function _sumList<T>(arr: specialize TArray<T>): double;")
			c.writeln("var i: integer; s: double;")
			c.writeln("begin")
			c.indent++
			c.writeln("s := 0;")
			c.writeln("for i := 0 to High(arr) do")
			c.indent++
			c.writeln("s := s + arr[i];")
			c.indent--
			c.writeln("Result := s;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_avgList":
			c.writeln("generic function _avgList<T>(arr: specialize TArray<T>): double;")
			c.writeln("begin")
			c.indent++
			c.writeln("if Length(arr) = 0 then exit(0);")
			c.writeln("Result := specialize _sumList<T>(arr) / Length(arr);")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_first":
			c.writeln("generic function _first<T>(arr: specialize TArray<T>): T;")
			c.writeln("begin")
			c.indent++
			c.writeln("if Length(arr) = 0 then exit(Default(T));")
			c.writeln("Result := arr[0];")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_Group":
			c.writeln("generic _Group<T> = record")
			c.indent++
			c.writeln("Key: Variant;")
			c.writeln("Items: specialize TArray<T>;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_group_by":
			c.writeln("generic function _group_by<T>(src: specialize TArray<T>; keyfn: function(it: T): Variant): specialize TArray<specialize _Group<T>>;")
			c.writeln("var i,j,idx: Integer; key: Variant; ks: string;")
			c.writeln("begin")
			c.indent++
			c.writeln("SetLength(Result, 0);")
			c.writeln("for i := 0 to High(src) do")
			c.writeln("begin")
			c.indent++
			c.writeln("key := keyfn(src[i]);")
			c.writeln("ks := VarToStr(key);")
			c.writeln("idx := -1;")
			c.writeln("for j := 0 to High(Result) do")
			c.indent++
			c.writeln("if VarToStr(Result[j].Key) = ks then begin idx := j; Break; end;")
			c.indent--
			c.writeln("if idx = -1 then")
			c.writeln("begin")
			c.indent++
			c.writeln("idx := Length(Result);")
			c.writeln("SetLength(Result, idx + 1);")
			c.writeln("Result[idx].Key := key;")
			c.writeln("SetLength(Result[idx].Items, 0);")
			c.indent--
			c.writeln("end;")
			c.writeln("SetLength(Result[idx].Items, Length(Result[idx].Items)+1);")
			c.writeln("Result[idx].Items[High(Result[idx].Items)] := src[i];")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_indexList":
			c.writeln("generic function _indexList<T>(arr: specialize TArray<T>; i: integer): T;")
			c.writeln("begin")
			c.indent++
			c.writeln("if i < 0 then i := Length(arr) + i;")
			c.writeln("if (i < 0) or (i >= Length(arr)) then")
			c.indent++
			c.writeln("raise Exception.Create('index out of range');")
			c.indent--
			c.writeln("Result := arr[i];")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_sliceList":
			c.writeln("generic function _sliceList<T>(arr: specialize TArray<T>; i, j: integer): specialize TArray<T>;")
			c.writeln("var start_, end_, n: integer;")
			c.writeln("begin")
			c.indent++
			c.writeln("start_ := i;")
			c.writeln("end_ := j;")
			c.writeln("n := Length(arr);")
			c.writeln("if start_ < 0 then start_ := n + start_;")
			c.writeln("if end_ < 0 then end_ := n + end_;")
			c.writeln("if start_ < 0 then start_ := 0;")
			c.writeln("if end_ > n then end_ := n;")
			c.writeln("if end_ < start_ then end_ := start_;")
			c.writeln("Result := Copy(arr, start_ + 1, end_ - start_);")
			c.indent--
			c.writeln("end;")
			c.writeln("")
		case "_sortBy":
			c.writeln("generic procedure _sortBy<T>(var arr: specialize TArray<T>; keys: specialize TArray<Variant>);")
			c.writeln("var i,j: integer; tmp: T; k: Variant;")
			c.writeln("begin")
			c.indent++
			c.writeln("for i := 0 to High(arr) - 1 do")
			c.writeln("for j := i + 1 to High(arr) do")
			c.indent++
			c.writeln("if keys[i] > keys[j] then")
			c.writeln("begin")
			c.indent++
			c.writeln("tmp := arr[i]; arr[i] := arr[j]; arr[j] := tmp;")
			c.writeln("k := keys[i]; keys[i] := keys[j]; keys[j] := k;")
			c.indent--
			c.writeln("end;")
			c.indent--
			c.indent--
			c.writeln("end;")
			c.writeln("")
		}
	}
}
