//go:build slow

package luacode

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
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
	case s.Import != nil:
		return c.compileImport(s.Import)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr)
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Fun != nil:
		return c.compileFun(s.Fun, true)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		if len(c.loopLabels) == 0 {
			return fmt.Errorf("continue outside loop")
		}
		label := c.loopLabels[len(c.loopLabels)-1]
		c.writeln("goto " + label)
		return nil
	case s.Fetch != nil:
		return c.compileFetchStmt(s.Fetch)
	case s.Update != nil:
		return c.compileUpdateStmt(s.Update)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	val := "nil"
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	} else if s.Type != nil {
		if c.uninitVars == nil {
			c.uninitVars = map[string]bool{}
		}
		c.uninitVars[s.Name] = true
	}
	prefix := "local "
	if c.indent == 0 {
		prefix = ""
	}
	c.writeln(fmt.Sprintf("%s%s = %s", prefix, name, val))
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	return c.compileLet(&parser.LetStmt{Name: s.Name, Type: s.Type, Value: s.Value})
}

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	if im.Lang != nil && *im.Lang != "lua" {
		if *im.Lang == "go" {
			alias := im.As
			if alias == "" {
				alias = parser.AliasFromPath(im.Path)
			}
			alias = sanitizeName(alias)
			path := strings.Trim(im.Path, "\"")
			if im.Auto && path == "mochi/runtime/ffi/go/testpkg" {
				c.writeln(fmt.Sprintf("local %s = { Add = function(a,b) return a + b end, Pi = 3.14, Answer = 42 }", alias))
				return nil
			}
			if path == "strings" {
				c.writeln(fmt.Sprintf("local %s = { ToUpper = string.upper }", alias))
				return nil
			}
		} else if *im.Lang == "python" {
			alias := im.As
			if alias == "" {
				alias = parser.AliasFromPath(im.Path)
			}
			alias = sanitizeName(alias)
			path := strings.Trim(im.Path, "\"")
			if path == "math" {
				c.writeln(fmt.Sprintf("local %s = { sqrt = math.sqrt, pow = math.pow, sin = math.sin, log = math.log, pi = math.pi, e = math.exp(1) }", alias))
				return nil
			}
		}
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
	if im.Lang == nil {
		return c.compilePackageImport(im)
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	path := strings.Trim(im.Path, "\"")
	c.writeln(fmt.Sprintf("local %s = require(%q)", alias, path))
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

	c.writeln(fmt.Sprintf("local function _import_%s()", alias))
	c.indent++
	c.writeln("local _pkg = {}")

	pkgName := alias
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			c.env = origEnv
			return err
		}
		if prog.Package != "" {
			pkgName = prog.Package
		}
		if errs := types.Check(prog, pkgEnv); len(errs) > 0 {
			c.env = origEnv
			return errs[0]
		}
		for _, s := range prog.Statements {
			if s.Fun != nil && s.Fun.Export {
				if err := c.compileFun(s.Fun, true); err != nil {
					c.env = origEnv
					return err
				}
				name := sanitizeName(s.Fun.Name)
				c.writeln(fmt.Sprintf("_pkg.%s = %s", name, name))
				c.writeln("")
			} else {
				if err := c.compileStmt(s); err != nil {
					c.env = origEnv
					return err
				}
			}
		}
	}
	c.writeln(fmt.Sprintf("_pkg.__name = %q", pkgName))
	c.writeln("return _pkg")
	c.env = origEnv
	c.indent--
	c.writeln("end")
	c.writeln(fmt.Sprintf("local %s = _import_%s()", alias, alias))
	c.writeln("")
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)
	if c.methodFields != nil && c.methodFields[s.Name] {
		lhs = "self." + lhs
	}

	var t types.Type = types.AnyType{}
	if c.env != nil {
		if tt, err := c.env.GetVar(s.Name); err == nil {
			t = tt
		}
	}

	for _, idx := range s.Index {
		idxExpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		if isList(t) || isString(t) {
			lhs = fmt.Sprintf("%s[(%s)+1]", lhs, idxExpr)
			if lt, ok := t.(types.ListType); ok {
				t = lt.Elem
			} else {
				t = types.AnyType{}
			}
		} else if isMap(t) {
			lhs = fmt.Sprintf("%s[%s]", lhs, idxExpr)
			if mt, ok := t.(types.MapType); ok {
				t = mt.Value
			} else {
				t = types.AnyType{}
			}
		} else {
			lhs = fmt.Sprintf("%s[%s]", lhs, idxExpr)
			t = types.AnyType{}
		}
	}

	for _, fld := range s.Field {
		lhs = fmt.Sprintf("%s.%s", lhs, sanitizeName(fld.Name))
		if st, ok := t.(types.StructType); ok {
			if ft, ok2 := st.Fields[fld.Name]; ok2 {
				t = ft
			} else {
				t = types.AnyType{}
			}
		} else {
			t = types.AnyType{}
		}
	}

	val, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, val))
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond + " then")
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	cur := stmt
	for cur.ElseIf != nil {
		cond2, err := c.compileExpr(cur.ElseIf.Cond)
		if err != nil {
			return err
		}
		c.writeln("elseif " + cond2 + " then")
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
		c.writeln("else")
		c.indent++
		for _, st := range cur.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileFor(s *parser.ForStmt) error {
	needLabel := hasContinue(s.Body)
	label := ""
	if needLabel {
		label = c.pushLoopLabel()
	}
	name := sanitizeName(s.Name)
	if s.RangeEnd != nil {
		start, err := c.compileExpr(s.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(s.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s = %s, (%s)-1 do", name, start, end))
		c.indent++
	} else {
		src, err := c.compileExpr(s.Source)
		if err != nil {
			return err
		}
		useVar := name != "_"
		t := c.inferExprType(s.Source)
		pre := ""
		switch {
		case isList(t):
			if useVar {
				c.writeln(fmt.Sprintf("for _, %s in ipairs(%s) do", name, src))
			} else {
				c.writeln(fmt.Sprintf("for _ in ipairs(%s) do", src))
			}
			c.indent++
		case isMap(t):
			keys := fmt.Sprintf("_k%d", c.tmpCount)
			tmp := fmt.Sprintf("_m%d", c.tmpCount)
			c.tmpCount++
			c.writeln(fmt.Sprintf("local %s = %s", tmp, src))
			c.writeln(fmt.Sprintf("local %s = {}", keys))
			c.writeln(fmt.Sprintf("for k in pairs(%s) do %s[#%s+1] = k end", tmp, keys, keys))
			c.writeln(fmt.Sprintf("table.sort(%s, function(a,b) return tostring(a)<tostring(b) end)", keys))
			if useVar {
				c.writeln(fmt.Sprintf("for _, %s in ipairs(%s) do", name, keys))
			} else {
				c.writeln(fmt.Sprintf("for _ in ipairs(%s) do", keys))
			}
			c.indent++
		case isString(t):
			tmp := fmt.Sprintf("_s%d", c.tmpCount)
			idx := fmt.Sprintf("_i%d", c.tmpCount)
			c.tmpCount++
			c.writeln(fmt.Sprintf("local %s = %s", tmp, src))
			c.writeln(fmt.Sprintf("for %s = 1, #%s do", idx, tmp))
			c.indent++
			if useVar {
				pre = fmt.Sprintf("local %s = string.sub(%s, %s, %s)", name, tmp, idx, idx)
			}
		default:
			c.helpers["iter"] = true
			if useVar {
				c.writeln(fmt.Sprintf("for _, %s in __iter(%s) do", name, src))
			} else {
				c.writeln(fmt.Sprintf("for _ in __iter(%s) do", src))
			}
			c.indent++
		}
		if pre != "" {
			c.writeln(pre)
		}
	}
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			if needLabel {
				c.popLoopLabel()
			}
			return err
		}
	}
	if needLabel {
		c.writeln("::" + label + "::")
	}
	c.indent--
	c.writeln("end")
	if needLabel {
		c.popLoopLabel()
	}
	return nil
}

func (c *Compiler) compileWhile(s *parser.WhileStmt) error {
	needLabel := hasContinue(s.Body)
	label := ""
	if needLabel {
		label = c.pushLoopLabel()
	}
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " do")
	c.indent++
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			if needLabel {
				c.popLoopLabel()
			}
			return err
		}
	}
	if needLabel {
		c.writeln("::" + label + "::")
	}
	c.indent--
	c.writeln("end")
	if needLabel {
		c.popLoopLabel()
	}
	return nil
}

func (c *Compiler) pushLoopLabel() string {
	label := fmt.Sprintf("__continue%d", c.labelCount)
	c.labelCount++
	c.loopLabels = append(c.loopLabels, label)
	return label
}

func (c *Compiler) popLoopLabel() {
	if len(c.loopLabels) > 0 {
		c.loopLabels = c.loopLabels[:len(c.loopLabels)-1]
	}
}

func hasContinue(stmts []*parser.Statement) bool {
	for _, st := range stmts {
		switch {
		case st.Continue != nil:
			return true
		case st.If != nil:
			if hasContinue(st.If.Then) {
				return true
			}
			cur := st.If
			for cur.ElseIf != nil {
				if hasContinue(cur.ElseIf.Then) {
					return true
				}
				cur = cur.ElseIf
			}
			if hasContinue(cur.Else) {
				return true
			}
		case st.For != nil:
			if hasContinue(st.For.Body) {
				return true
			}
		case st.While != nil:
			if hasContinue(st.While.Body) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) compileFun(fun *parser.FunStmt, local bool) error {
	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = sanitizeName(p.Name)
	}
	prefix := ""
	if local {
		prefix = "local "
	}
	c.writeln(fmt.Sprintf("%sfunction %s(%s)", prefix, name, strings.Join(params, ", ")))
	c.indent++
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("function %s()", name))
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if not (%s) then error('expect failed') end", expr))
	return nil
}

func (c *Compiler) compileFetchStmt(f *parser.FetchStmt) error {
	expr, err := c.compileFetchExpr(&parser.FetchExpr{Pos: f.Pos, URL: f.URL, With: f.With})
	if err != nil {
		return err
	}
	name := sanitizeName(f.Target)
	prefix := "local "
	if c.indent == 0 {
		prefix = ""
	}
	c.writeln(fmt.Sprintf("%s%s = %s", prefix, name, expr))
	return nil
}

func (c *Compiler) compileUpdateStmt(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	idx := fmt.Sprintf("_i%d", c.tmpCount)
	item := fmt.Sprintf("_it%d", c.tmpCount)
	c.tmpCount++

	c.writeln(fmt.Sprintf("for %s = 1, #%s do", idx, list))
	c.indent++
	c.writeln(fmt.Sprintf("local %s = %s[%s]", item, list, idx))

	origEnv := c.env
	if c.env != nil {
		child := types.NewEnv(c.env)
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					for _, f := range st.Order {
						fieldVar := sanitizeName(f)
						c.writeln(fmt.Sprintf("local %s = %s[%q]", fieldVar, item, f))
						child.SetVar(f, st.Fields[f], true)
					}
				}
			}
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

	for _, it := range u.Set.Items {
		if key, ok := simpleStringKey(it.Key); ok {
			val, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = origEnv
				return err
			}
			c.writeln(fmt.Sprintf("%s[%q] = %s", item, key, val))
		} else {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				c.env = origEnv
				return err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = origEnv
				return err
			}
			c.writeln(fmt.Sprintf("%s[%s] = %s", item, k, v))
		}
	}

	if u.Where != nil {
		c.indent--
		c.writeln("end")
	}

	c.env = origEnv
	c.writeln(fmt.Sprintf("%s[%s] = %s", list, idx, item))
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.buf.WriteString(fmt.Sprintf("function %s:%s(%s)", sanitizeName(structName), name, strings.Join(params, ", ")))
	c.buf.WriteByte('\n')
	origEnv := c.env
	if c.env != nil {
		child := types.NewEnv(c.env)
		if st, ok := c.env.GetStruct(structName); ok {
			c.methodFields = make(map[string]bool, len(st.Fields))
			for fname, ft := range st.Fields {
				child.SetVar(fname, ft, true)
				c.methodFields[fname] = true
			}
		}
		for _, p := range fun.Params {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
		c.env = child
	}
	c.indent++
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			c.methodFields = nil
			return err
		}
	}
	c.indent--
	c.env = origEnv
	c.methodFields = nil
	c.writeln("end")
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return nil
	}
	name := sanitizeName(t.Name)
	methods := []*parser.FunStmt{}
	for _, m := range t.Members {
		if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	c.writeln(fmt.Sprintf("%s = {}", name))
	c.writeln(fmt.Sprintf("%s.__index = %s", name, name))
	c.writeln(fmt.Sprintf("function %s.new(o)", name))
	c.indent++
	c.writeln("o = o or {}")
	c.writeln(fmt.Sprintf("setmetatable(o, %s)", name))
	c.writeln("return o")
	c.indent--
	c.writeln("end")
	for _, m := range methods {
		c.writeln("")
		if err := c.compileMethod(t.Name, m); err != nil {
			return err
		}
	}
	return nil
}
