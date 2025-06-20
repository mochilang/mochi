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
}

// New creates a new Pascal compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, tempVars: make(map[string]string), packages: make(map[string]bool)}
}

// Compile returns Pascal source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.tempVars = make(map[string]string)
	c.writeln("program main;")
	c.writeln("{$mode objfpc}")
	c.writeln("uses SysUtils, fgl;")
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
				return nil, fmt.Errorf("foreign imports not supported")
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

	// Collect global vars.
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

	c.writeln("begin")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("end.")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Test != nil, s.Expect != nil:
		// test blocks are ignored when compiling to Pascal
		return nil
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
	case s.Import != nil:
		if s.Import.Lang == nil {
			return c.compilePackageImport(s.Import)
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
		return fmt.Errorf("unsupported statement")
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

func (c *Compiler) mustExpr(e *parser.Expr) string {
	s, _ := c.compileExpr(e)
	return s
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return fmt.Errorf("union types not supported")
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

	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	c.varTypes = map[string]string{}
	for i, p := range fun.Params {
		pt := c.typeRef(p.Type)
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), pt)
		c.varTypes[p.Name] = pt
	}
	retType := c.typeRef(fun.Return)
	c.writeln(fmt.Sprintf("function %s(%s): %s;", name, strings.Join(params, "; "), retType))
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
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end;")

	// restore temp vars for outer scope
	c.tempVars = prevTemps
	c.varTypes = nil
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
				typ = inferTypeFromExpr(s.Let.Value, env, vars)
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
				typ = inferTypeFromExpr(s.Var.Value, env, vars)
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
		case s.For != nil:
			if s.For.Name != "_" {
				typ := "integer"
				if env != nil {
					if t, err := env.GetVar(s.For.Name); err == nil {
						typ = typeString(t)
					}
				}
				if typ == "integer" {
					typ = inferTypeFromExpr(s.For.Source, env, vars)
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

func inferTypeFromExpr(e *parser.Expr, env *types.Env, vars map[string]string) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "integer"
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "integer"
	}
	p := u.Value
	if p == nil || len(p.Ops) > 0 {
		return "integer"
	}
	return inferTypeFromPrimary(p.Target, env, vars)
}

func inferTypeFromPrimary(p *parser.Primary, env *types.Env, vars map[string]string) string {
	switch {
	case p == nil:
		return "integer"
	case p.Lit != nil:
		switch {
		case p.Lit.Str != nil:
			return "string"
		case p.Lit.Bool != nil:
			return "boolean"
		case p.Lit.Float != nil:
			return "double"
		default:
			return "integer"
		}
	case p.Selector != nil:
		if env != nil {
			if t, err := env.GetVar(p.Selector.Root); err == nil {
				return typeString(t)
			}
		}
		if vars != nil {
			if v, ok := vars[p.Selector.Root]; ok {
				return v
			}
		}
	case p.Struct != nil:
		return p.Struct.Name
	case p.List != nil:
		elem := "integer"
		if len(p.List.Elems) > 0 {
			elem = inferTypeFromExpr(p.List.Elems[0], env, vars)
		}
		return fmt.Sprintf("specialize TArray<%s>", elem)
	}
	return "integer"
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
		operators = append(operators, part.Op)
	}

	levels := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}}

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
	for i, op := range p.Ops {
		if op.Index != nil {
			if i == 0 && p.Target != nil && p.Target.Lit != nil && p.Target.Lit.Str != nil {
				if op.Index.Start != nil && op.Index.Start.Binary != nil && op.Index.Start.Binary.Left != nil && op.Index.Start.Binary.Left.Value != nil && op.Index.Start.Binary.Left.Value.Target != nil && op.Index.Start.Binary.Left.Value.Target.Lit != nil && op.Index.Start.Binary.Left.Value.Target.Lit.Int != nil && *op.Index.Start.Binary.Left.Value.Target.Lit.Int == 0 {
					s := strings.ReplaceAll(*p.Target.Lit.Str, "'", "''")
					expr = fmt.Sprintf("'%s'", s)
					continue
				}
			}
			if op.Index.End != nil || op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					sidx, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = sidx
				}
				if c.isStringPostfix(&parser.PostfixExpr{Target: p.Target}) {
					begin := start
					if begin != "0" {
						begin = fmt.Sprintf("%s + 1", begin)
					} else {
						begin = "1"
					}
					length := fmt.Sprintf("Length(%s)", expr)
					if op.Index.End != nil {
						eidx, err := c.compileExpr(op.Index.End)
						if err != nil {
							return "", err
						}
						if op.Index.Start != nil {
							length = fmt.Sprintf("(%s - %s)", eidx, start)
						} else {
							length = eidx
						}
					} else if op.Index.Start != nil {
						length = fmt.Sprintf("Length(%s) - %s", expr, start)
					}
					expr = fmt.Sprintf("Copy(%s, %s, %s)", expr, begin, length)
				} else {
					// fallback: simple index at start
					idx, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					if c.isStringPostfix(&parser.PostfixExpr{Target: p.Target}) {
						idx = fmt.Sprintf("%s + 1", idx)
					}
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isMapPostfix(&parser.PostfixExpr{Target: p.Target}) {
					expr = fmt.Sprintf("%s.KeyData[%s]", expr, idx)
				} else {
					if c.isStringPostfix(&parser.PostfixExpr{Target: p.Target}) {
						idx = fmt.Sprintf("%s + 1", idx)
					}
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
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
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
		case "len":
			return fmt.Sprintf("Length(%s)", argStr), nil
		case "print":
			return fmt.Sprintf("writeln(%s)", argStr), nil
		case "str":
			if len(args) == 1 {
				t := inferTypeFromExpr(p.Call.Args[0], c.env, c.varTypes)
				if t == "double" {
					return fmt.Sprintf("FloatToStr(%s)", args[0]), nil
				}
				return fmt.Sprintf("IntToStr(%s)", args[0]), nil
			}
			return fmt.Sprintf("IntToStr(%s)", argStr), nil
		default:
			return fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), argStr), nil
		}
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Struct != nil:
		typ := sanitizeName(p.Struct.Name)
		tmp := c.newTypedVar(typ)
		c.writeln(fmt.Sprintf("var %s: %s;", tmp, typ))
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
			elemType = inferTypeFromExpr(p.List.Elems[0], c.env, c.varTypes)
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
			if c.env != nil {
				// no direct info; use defaults
			}
		}
		pairs := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs[i] = fmt.Sprintf("m.AddOrSetData(%s, %s);", k, v)
		}
		tmp := c.newTypedVar(fmt.Sprintf("specialize TFPGMap<%s, %s>", keyType, valType))
		c.writeln(fmt.Sprintf("var %s: specialize TFPGMap<%s, %s>;", tmp, keyType, valType))
		c.writeln(fmt.Sprintf("%s := specialize TFPGMap<%s, %s>.Create;", tmp, keyType, valType))
		for _, p := range pairs {
			c.writeln(strings.ReplaceAll(p, "m", tmp))
		}
		return tmp, nil
	}
	return "", fmt.Errorf("unsupported expression")
}
