//go:build archived

package erlcode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Erlang source code.
type Compiler struct {
	buf           bytes.Buffer
	indent        int
	env           *types.Env
	needPrint     bool
	needFormat    bool
	needCount     bool
	needInput     bool
	needAvg       bool
	needSum       bool
	needMin       bool
	needMax       bool
	needForeach   bool
	needWhile     bool
	needExpect    bool
	needTest      bool
	needGet       bool
	needSlice     bool
	needIO        bool
	needFetch     bool
	needJSON      bool
	needGenText   bool
	needGenEmbed  bool
	needGenStruct bool
	castStructs   map[string]types.StructType
	needSetOps    bool
	needGroup     bool
	needGroupBy   bool
	needLeftJoin  bool
	needRightJoin bool
	needOuterJoin bool
	vars          map[string]string
	counts        map[string]int
	tmpCount      int
	packages      map[string]bool
	tests         []testInfo
}

func (c *Compiler) compileTypeDecls(prog *parser.Program) error {
	found := false
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return err
			}
			found = true
		}
	}
	if found {
		c.writeln("")
	}
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		for _, v := range t.Variants {
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = sanitizeName(f.Name)
			}
			c.writeln(fmt.Sprintf("-record(%s, {%s}).", sanitizeName(v.Name), strings.Join(fields, ", ")))
		}
		return nil
	}
	fields := []string{}
	methods := []*parser.FunStmt{}
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, sanitizeName(m.Field.Name))
		}
		if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	c.writeln(fmt.Sprintf("-record(%s, {%s}).", sanitizeName(t.Name), strings.Join(fields, ", ")))
	for _, m := range methods {
		if err := c.compileMethod(t.Name, m); err != nil {
			return err
		}
		c.writeln("")
	}
	return nil
}

type testInfo struct {
	label string
	body  []*parser.Statement
}

func copyMap[K comparable, V any](m map[K]V) map[K]V {
	if m == nil {
		return nil
	}
	cp := make(map[K]V, len(m))
	for k, v := range m {
		cp[k] = v
	}
	return cp
}

func gatherAssigned(stmts []*parser.Statement, scope map[string]string, out map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Assign != nil:
			if _, ok := scope[s.Assign.Name]; ok {
				out[s.Assign.Name] = true
			}
		case s.For != nil:
			nscope := copyMap(scope)
			nscope[s.For.Name] = s.For.Name
			gatherAssigned(s.For.Body, nscope, out)
		case s.While != nil:
			gatherAssigned(s.While.Body, scope, out)
		case s.If != nil:
			gatherAssigned(s.If.Then, scope, out)
			if s.If.ElseIf != nil {
				gatherAssignedIf(s.If.ElseIf, scope, out)
			}
			gatherAssigned(s.If.Else, scope, out)
		}
	}
}

func gatherAssignedIf(ifst *parser.IfStmt, scope map[string]string, out map[string]bool) {
	if ifst == nil {
		return
	}
	gatherAssigned(ifst.Then, scope, out)
	if ifst.ElseIf != nil {
		gatherAssignedIf(ifst.ElseIf, scope, out)
	}
	gatherAssigned(ifst.Else, scope, out)
}

func capitalize(name string) string {
	if name == "_" {
		return name
	}
	if len(name) > 0 {
		b := []byte(name)
		if b[0] >= 'a' && b[0] <= 'z' {
			b[0] = b[0] - 'a' + 'A'
		}
		return string(b)
	}
	return name
}

func (c *Compiler) current(name string) string {
	if v, ok := c.vars[name]; ok {
		return v
	}
	v := capitalize(name)
	c.vars[name] = v
	return v
}

func (c *Compiler) newName(name string) string {
	n := c.counts[name]
	varName := capitalize(name)
	if n > 0 {
		varName = fmt.Sprintf("%s_%d", varName, n)
	}
	c.counts[name] = n + 1
	c.vars[name] = varName
	return varName
}

func gatherIdentsExpr(e *parser.Expr, out map[string]bool) {
	if e == nil {
		return
	}
	gatherIdentsUnary(e.Binary.Left, out)
	for _, op := range e.Binary.Right {
		gatherIdentsPostfix(op.Right, out)
	}
}

func gatherIdentsUnary(u *parser.Unary, out map[string]bool) {
	if u == nil {
		return
	}
	gatherIdentsPostfix(u.Value, out)
}

func gatherIdentsPostfix(p *parser.PostfixExpr, out map[string]bool) {
	if p == nil {
		return
	}
	gatherIdentsPrimary(p.Target, out)
	for _, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				gatherIdentsExpr(a, out)
			}
		}
		if op.Index != nil {
			gatherIdentsExpr(op.Index.Start, out)
			gatherIdentsExpr(op.Index.End, out)
			gatherIdentsExpr(op.Index.Step, out)
		}
	}
}

func gatherIdentsPrimary(p *parser.Primary, out map[string]bool) {
	if p == nil {
		return
	}
	switch {
	case p.Selector != nil:
		out[p.Selector.Root] = true
	case p.Call != nil:
		for _, a := range p.Call.Args {
			gatherIdentsExpr(a, out)
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			gatherIdentsExpr(f.Value, out)
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			gatherIdentsExpr(e, out)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			gatherIdentsExpr(it.Key, out)
			gatherIdentsExpr(it.Value, out)
		}
	case p.Group != nil:
		gatherIdentsExpr(p.Group, out)
	case p.Query != nil:
		// ignore nested query vars for simple check
	}
}

// New returns a new Compiler.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: map[string]string{}, counts: map[string]int{}, packages: map[string]bool{}, tmpCount: 0, tests: []testInfo{}, castStructs: map[string]types.StructType{}}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile generates Erlang source for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("#!/usr/bin/env escript")
	mod := "main"
	if prog.Package != "" {
		mod = prog.Package
	}
	c.writeln(fmt.Sprintf("-module(%s).", mod))

	// collect exported functions
	exports := []string{"main/1"}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			exports = append(exports, fmt.Sprintf("%s/%d", atomName(s.Fun.Name), len(s.Fun.Params)))
		}
	}
	c.writeln("-export([" + strings.Join(exports, ", ") + "]).")
	c.writeln("")

	if err := c.compileTypeDecls(prog); err != nil {
		return nil, err
	}

	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("main(_) ->")
	c.indent++
	body := []*parser.Statement{}
	for _, s := range prog.Statements {
		if s.Fun == nil && s.Test == nil && s.Type == nil {
			body = append(body, s)
		}
	}
	if err := c.compileBlock(body, len(c.tests) == 0, nil); err != nil {
		return nil, err
	}
	if len(c.tests) > 0 {
		if len(body) > 0 {
			c.buf.WriteString(",\n")
		}
		for i, t := range c.tests {
			c.writeIndent()
			c.buf.WriteString(fmt.Sprintf("mochi_run_test(\"%s\", fun() ->\n", t.label))
			c.indent++
			if err := c.compileBlock(t.body, false, nil); err != nil {
				return nil, err
			}
			c.indent--
			c.writeIndent()
			c.buf.WriteString("end")
			c.buf.WriteByte(')')
			if i == len(c.tests)-1 {
				c.buf.WriteString(".\n")
			} else {
				c.buf.WriteString(",\n")
			}
		}
	}
	c.indent--

	c.writeln("")
	c.emitRuntime()

	code := c.buf.Bytes()
	return Format(code), nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	c.writeln(fmt.Sprintf("%% line %d", fun.Pos.Line))
	params := []string{}
	savedVars := c.vars
	savedCounts := c.counts
	c.vars = map[string]string{}
	// preserve counts across functions
	if c.counts == nil {
		c.counts = map[string]int{}
	}
	for _, p := range fun.Params {
		params = append(params, c.newName(p.Name))
	}
	c.writeln(fmt.Sprintf("%s(%s) ->", atomName(fun.Name), strings.Join(params, ", ")))
	child := types.NewEnv(c.env)
	for _, p := range fun.Params {
		var typ types.Type = types.AnyType{}
		if p.Type != nil {
			typ = c.resolveTypeRef(p.Type)
		}
		child.SetVar(p.Name, typ, true)
	}
	origEnv := c.env
	c.env = child
	c.indent++
	c.writeln("try")
	c.indent++
	if err := c.compileBlock(fun.Body, false, nil); err != nil {
		c.env = origEnv
		return err
	}
	c.indent--
	c.writeln("catch")
	c.indent++
	c.writeln("throw:{return, V} -> V")
	c.indent--
	c.writeln("end.")
	c.indent--
	c.env = origEnv
	c.vars = savedVars
	c.counts = savedCounts
	return nil
}

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	c.writeln(fmt.Sprintf("%% line %d", fun.Pos.Line))
	params := []string{"Self"}
	savedVars := c.vars
	savedCounts := c.counts
	c.vars = map[string]string{}
	if c.counts == nil {
		c.counts = map[string]int{}
	}
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			for fname := range st.Fields {
				c.vars[fname] = fmt.Sprintf("Self#%s.%s", sanitizeName(structName), sanitizeName(fname))
			}
		}
	}
	for _, p := range fun.Params {
		params = append(params, c.newName(p.Name))
	}
	name := sanitizeName(structName) + "_" + sanitizeName(fun.Name)
	c.writeln(fmt.Sprintf("%s(%s) ->", atomName(name), strings.Join(params, ", ")))
	child := types.NewEnv(c.env)
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			for fname, t := range st.Fields {
				child.SetVar(fname, t, true)
			}
		}
	}
	for _, p := range fun.Params {
		var typ types.Type = types.AnyType{}
		if p.Type != nil {
			typ = c.resolveTypeRef(p.Type)
		}
		child.SetVar(p.Name, typ, true)
	}
	origEnv := c.env
	c.env = child
	c.indent++
	c.writeln("try")
	c.indent++
	if err := c.compileBlock(fun.Body, false, nil); err != nil {
		c.env = origEnv
		return err
	}
	c.indent--
	c.writeln("catch")
	c.indent++
	c.writeln("throw:{return, V} -> V")
	c.indent--
	c.writeln("end.")
	c.indent--
	c.env = origEnv
	c.vars = savedVars
	c.counts = savedCounts
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	c.tests = append(c.tests, testInfo{label: t.Name, body: t.Body})
	c.needTest = true
	return nil
}

// compileBlock writes a sequence of statements separated by commas. If filter is
// provided, only statements for which filter(stmt) is true are compiled. If
// lastPeriod is true, the final statement ends with a period.
func (c *Compiler) compileBlock(stmts []*parser.Statement, lastPeriod bool, filter func(*parser.Statement) bool) error {
	filtered := []*parser.Statement{}
	for _, s := range stmts {
		if filter == nil || filter(s) {
			filtered = append(filtered, s)
		}
	}
	for i, s := range filtered {
		c.writeIndent()
		if err := c.compileStmt(s); err != nil {
			return err
		}
		if i == len(filtered)-1 {
			if lastPeriod {
				c.buf.WriteString(".\n")
			} else {
				c.buf.WriteByte('\n')
			}
		} else {
			c.buf.WriteString(",\n")
		}
	}
	if len(filtered) == 0 && lastPeriod {
		c.writeln("ok.")
	}
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val := "undefined"
		if s.Let.Value != nil {
			v, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			val = v
		}
		name := c.newName(s.Let.Name)
		c.buf.WriteString(fmt.Sprintf("%s = %s", name, val))
		if c.env != nil {
			var t types.Type = types.AnyType{}
			if s.Let.Type != nil {
				t = c.resolveTypeRef(s.Let.Type)
			} else if s.Let.Value != nil {
				t = c.exprType(s.Let.Value)
			}
			c.env.SetVar(s.Let.Name, t, false)
		}
	case s.Var != nil:
		val := "undefined"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			val = v
		}
		name := c.newName(s.Var.Name)
		c.buf.WriteString(fmt.Sprintf("%s = %s", name, val))
		if c.env != nil {
			var t types.Type = types.AnyType{}
			if s.Var.Type != nil {
				t = c.resolveTypeRef(s.Var.Type)
			} else if s.Var.Value != nil {
				t = c.exprType(s.Var.Value)
			}
			c.env.SetVar(s.Var.Name, t, true)
		}
	case s.Assign != nil:
		if err := c.compileAssign(s.Assign); err != nil {
			return err
		}
	case s.Return != nil:
		v, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.buf.WriteString(fmt.Sprintf("throw({return, %s})", v))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.buf.WriteString(expr)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.buf.WriteString("throw(mochi_break)")
		return nil
	case s.Continue != nil:
		c.buf.WriteString("throw(mochi_continue)")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Type != nil:
		// type declarations are emitted globally
		return nil
	case s.Import != nil:
		if s.Import.Lang == nil || *s.Import.Lang == "erlang" {
			return c.compilePackageImport(s.Import)
		}
		// other languages not supported
		return nil
	case s.Test != nil:
		// handled separately
		return nil
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.needExpect = true
		c.buf.WriteString("mochi_expect(" + expr + ")")
		return nil
	case s.ExternVar != nil:
		return c.compileExternVar(s.ExternVar)
	case s.ExternFun != nil:
		return c.compileExternFun(s.ExternFun)
	case s.ExternType != nil:
		return c.compileExternType(s.ExternType)
	case s.ExternObject != nil:
		return c.compileExternObject(s.ExternObject)
	default:
		c.buf.WriteString("ok")
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("case " + cond + " of")
	c.indent++
	c.writeln("true ->")
	c.indent++
	start := c.buf.Len()
	if err := c.compileBlock(stmt.Then, false, nil); err != nil {
		return err
	}
	b := c.buf.Bytes()
	if n := c.buf.Len(); n > start && b[n-1] == '\n' {
		c.buf.Truncate(n - 1)
	}
	c.buf.WriteString(";\n")
	c.indent--
	if stmt.ElseIf != nil {
		c.writeIndent()
		c.writeln("_ ->")
		c.indent++
		if err := c.compileIf(stmt.ElseIf); err != nil {
			return err
		}
		c.indent--
	} else if len(stmt.Else) > 0 {
		c.writeIndent()
		c.writeln("_ ->")
		c.indent++
		if err := c.compileBlock(stmt.Else, false, nil); err != nil {
			return err
		}
		c.indent--
	} else {
		c.writeIndent()
		c.writeln("_ -> ok")
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("end")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	var list string
	var elem types.Type = types.AnyType{}
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		list = fmt.Sprintf("lists:seq(%s, (%s)-1)", start, end)
		elem = types.IntType{}
	} else {
		src, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		list = src
		t := c.exprType(stmt.Source)
		switch tt := t.(type) {
		case types.ListType:
			elem = tt.Elem
		case types.StringType:
			elem = types.StringType{}
		case types.MapType:
			list = fmt.Sprintf("maps:keys(%s)", src)
			elem = tt.Key
		default:
			elem = types.AnyType{}
		}
	}
	prevName, hasPrev := c.vars[stmt.Name]
	iter := c.newName(stmt.Name)
	c.needForeach = true
	c.buf.WriteString(fmt.Sprintf("mochi_foreach(fun(%s) ->\n", iter))
	child := types.NewEnv(c.env)
	child.SetVar(stmt.Name, elem, true)
	orig := c.env
	c.env = child
	c.indent++
	if err := c.compileBlock(stmt.Body, false, nil); err != nil {
		c.env = orig
		return err
	}
	c.indent--
	c.env = orig
	if hasPrev {
		c.vars[stmt.Name] = prevName
	} else {
		delete(c.vars, stmt.Name)
	}
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("end, %s)", list))
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}

	// Determine which variables are assigned inside the loop
	assigned := map[string]bool{}
	gatherAssigned(stmt.Body, c.vars, assigned)
	if len(assigned) == 0 {
		// fall back to simple helper-based loop
		c.needWhile = true
		c.buf.WriteString("mochi_while(fun() -> " + cond + " end, fun() ->\n")
		c.indent++
		if err := c.compileBlock(stmt.Body, false, nil); err != nil {
			return err
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("end)")
		return nil
	}

	names := make([]string, 0, len(assigned))
	for n := range assigned {
		names = append(names, n)
	}
	slices.Sort(names)

	params := make([]string, len(names))
	for i, n := range names {
		params[i] = c.current(n)
	}

	loopVar := c.newName("loop")
	c.buf.WriteString(loopVar + " = fun " + capitalize("loop") + "(" + strings.Join(params, ", ") + ") ->\n")
	c.indent++
	c.writeln("case " + cond + " of")
	c.indent++
	c.writeln("true ->")
	c.indent++
	c.writeln("try")
	c.indent++

	// save env before compiling body
	savedVars := copyMap(c.vars)
	savedCounts := copyMap(c.counts)

	if err := c.compileBlock(stmt.Body, false, nil); err != nil {
		return err
	}

	newNames := make([]string, len(names))
	for i, n := range names {
		newNames[i] = c.current(n)
	}

	c.writeln(capitalize("loop") + "(" + strings.Join(newNames, ", ") + ")")
	c.indent--
	c.writeln("catch")
	c.indent++
	c.writeln("throw:mochi_continue -> " + capitalize("loop") + "(" + strings.Join(newNames, ", ") + ");")
	c.writeln("throw:mochi_break -> {" + strings.Join(newNames, ", ") + "}")
	c.indent--
	c.writeln("end;")
	c.indent--
	c.writeln("_ -> {" + strings.Join(params, ", ") + "}")
	c.indent--
	c.writeln("end")
	c.indent--
	c.writeln("end,")

	// restore environment
	c.vars = savedVars
	c.counts = savedCounts

	resultNames := make([]string, len(names))
	for i, n := range names {
		resultNames[i] = c.newName(n)
	}

	c.writeln("{" + strings.Join(resultNames, ", ") + "} = " + loopVar + "(" + strings.Join(params, ", ") + ")")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := c.current(u.Target)
	result := c.newName(u.Target)
	iter := c.newName("item")

	origEnv := c.env
	savedVars := copyMap(c.vars)

	record := ""
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					record = sanitizeName(st.Name)
					child := types.NewEnv(c.env)
					for name, ft := range st.Fields {
						child.SetVar(name, ft, true)
						c.vars[name] = fmt.Sprintf("%s#%s.%s", iter, record, sanitizeName(name))
					}
					c.env = child
				}
			}
		}
	}

	var where string
	if u.Where != nil {
		w, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			c.vars = savedVars
			return err
		}
		where = w
	}

	var updated string
	if record != "" {
		parts := make([]string, len(u.Set.Items))
		for i, it := range u.Set.Items {
			key, _ := identName(it.Key)
			val, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = origEnv
				c.vars = savedVars
				return err
			}
			parts[i] = fmt.Sprintf("%s=%s", sanitizeName(key), val)
		}
		if len(parts) > 0 {
			updated = fmt.Sprintf("%s#%s{%s}", iter, record, strings.Join(parts, ", "))
		} else {
			updated = iter
		}
	} else {
		expr := iter
		for _, it := range u.Set.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				c.env = origEnv
				c.vars = savedVars
				return err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = origEnv
				c.vars = savedVars
				return err
			}
			expr = fmt.Sprintf("maps:put(%s, %s, %s)", k, v, expr)
		}
		updated = expr
	}

	if where != "" {
		updated = fmt.Sprintf("(case %s of true -> %s; _ -> %s end)", where, updated, iter)
	}

	c.env = origEnv
	c.vars = savedVars

	comp := fmt.Sprintf("[ %s || %s <- %s ]", updated, iter, list)
	c.buf.WriteString(fmt.Sprintf("%s = %s", result, comp))

	if c.env != nil {
		typ, err := c.env.GetVar(u.Target)
		if err != nil {
			typ = types.AnyType{}
		}
		mutable, _ := c.env.IsMutable(u.Target)
		c.env.SetVar(u.Target, typ, mutable)
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	name := c.newName(a.Name)
	c.buf.WriteString(fmt.Sprintf("%s = %s", name, val))
	if c.env != nil {
		t := c.exprType(a.Value)
		mutable, _ := c.env.IsMutable(a.Name)
		c.env.SetVar(a.Name, t, mutable)
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	type operand struct {
		expr string
		typ  types.Type
	}

	if b == nil {
		return "", fmt.Errorf("nil binary expression")
	}

	ops := []string{}
	operands := []operand{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, operand{expr: first, typ: c.unaryType(b.Left)})

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		op := part.Op
		if part.All {
			op = op + "_all"
		}
		operands = append(operands, operand{expr: r, typ: c.postfixType(part.Right)})
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

	contains := func(sl []string, s string) bool {
		for _, v := range sl {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if !contains(level, ops[i]) {
				i++
				continue
			}

			op := ops[i]
			l := operands[i]
			r := operands[i+1]

			var expr string
			var typ types.Type

			switch op {
			case "+":
				if _, ok := l.typ.(types.ListType); ok || isList(r.typ) {
					expr = fmt.Sprintf("lists:append(%s, %s)", l.expr, r.expr)
					if lt, ok := l.typ.(types.ListType); ok {
						typ = lt
					} else if rt, ok := r.typ.(types.ListType); ok {
						typ = rt
					} else {
						typ = types.ListType{Elem: types.AnyType{}}
					}
				} else {
					expr = fmt.Sprintf("(%s + %s)", l.expr, r.expr)
					typ = resultType(op, l.typ, r.typ)
				}
			case "-", "*", "/", "%", "<", "<=", ">", ">=":
				erlOp := op
				if op == "<=" {
					erlOp = "=<"
				}
				expr = fmt.Sprintf("(%s %s %s)", l.expr, erlOp, r.expr)
				typ = resultType(op, l.typ, r.typ)
			case "==", "!=":
				erlOp := op
				if op == "!=" {
					erlOp = "/="
				}
				expr = fmt.Sprintf("(%s %s %s)", l.expr, erlOp, r.expr)
				typ = resultType(op, l.typ, r.typ)
			case "&&":
				expr = fmt.Sprintf("(%s and %s)", l.expr, r.expr)
				typ = types.BoolType{}
			case "||":
				expr = fmt.Sprintf("(%s or %s)", l.expr, r.expr)
				typ = types.BoolType{}
			case "in":
				switch r.typ.(type) {
				case types.MapType:
					expr = fmt.Sprintf("maps:is_key(%s, %s)", l.expr, r.expr)
				case types.ListType:
					expr = fmt.Sprintf("lists:member(%s, %s)", l.expr, r.expr)
				case types.StringType:
					expr = fmt.Sprintf("(string:str(%s, %s) =/= 0)", r.expr, l.expr)
				default:
					expr = fmt.Sprintf("lists:member(%s, %s)", l.expr, r.expr)
				}
				typ = types.BoolType{}
			case "union_all":
				expr = fmt.Sprintf("lists:append(%s, %s)", l.expr, r.expr)
				typ = types.ListType{Elem: types.AnyType{}}
			case "union":
				c.needSetOps = true
				expr = fmt.Sprintf("mochi_union(%s, %s)", l.expr, r.expr)
				typ = types.ListType{Elem: types.AnyType{}}
			case "except":
				c.needSetOps = true
				expr = fmt.Sprintf("mochi_except(%s, %s)", l.expr, r.expr)
				typ = types.ListType{Elem: types.AnyType{}}
			case "intersect":
				c.needSetOps = true
				expr = fmt.Sprintf("mochi_intersect(%s, %s)", l.expr, r.expr)
				typ = types.ListType{Elem: types.AnyType{}}
			default:
				return "", fmt.Errorf("unsupported operator %s", op)
			}

			operands[i] = operand{expr: expr, typ: typ}
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state in binary expression")
	}

	return operands[0].expr, nil
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
		case "+":
			// unary plus has no effect
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	target := p.Target
	if len(p.Ops) > 0 && p.Ops[0].Call != nil && target.Selector != nil {
		tail := target.Selector.Tail
		if len(tail) > 0 && tail[len(tail)-1] == "contains" {
			sel := &parser.SelectorExpr{Root: target.Selector.Root, Tail: tail[:len(tail)-1]}
			target = &parser.Primary{Selector: sel}
		}
	}

	res, err := c.compilePrimary(target)
	if err != nil {
		return "", err
	}

	typ := c.primaryType(target)

	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("length(%s)", res)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				c.needSlice = true
				res = fmt.Sprintf("mochi_slice(%s, %s, %s)", res, start, end)
				switch tt := typ.(type) {
				case types.ListType:
					typ = tt
				case types.StringType:
					typ = types.StringType{}
				default:
					typ = types.AnyType{}
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}

				switch tt := typ.(type) {
				case types.MapType:
					res = fmt.Sprintf("maps:get(%s, %s)", idx, res)
					typ = tt.Value
				case types.ListType:
					c.needGet = true
					res = fmt.Sprintf("mochi_get(%s, %s)", res, idx)
					typ = tt.Elem
				case types.StringType:
					c.needGet = true
					res = fmt.Sprintf("mochi_get(%s, %s)", res, idx)
					typ = types.StringType{}
				default:
					c.needGet = true
					res = fmt.Sprintf("mochi_get(%s, %s)", res, idx)
					typ = types.AnyType{}
				}
			}
		} else if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
			argStr := strings.Join(args, ", ")
			// method call like obj.foo()
			if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 {
				root := p.Target.Selector.Root
				method := p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1]
				if method == "contains" && len(args) == 1 {
					switch typ.(type) {
					case types.MapType:
						res = fmt.Sprintf("maps:is_key(%s, %s)", args[0], res)
					case types.ListType:
						res = fmt.Sprintf("lists:member(%s, %s)", args[0], res)
					case types.StringType:
						res = fmt.Sprintf("(string:str(%s, %s) =/= 0)", res, args[0])
					default:
						res = fmt.Sprintf("lists:member(%s, %s)", args[0], res)
					}
					typ = types.BoolType{}
					continue
				}
				if t, err := c.env.GetVar(root); err == nil {
					if st, ok := t.(types.StructType); ok {
						if m, ok := st.Methods[method]; ok {
							callArgs := append([]string{c.current(root)}, args...)
							res = fmt.Sprintf("%s_%s(%s)", sanitizeName(st.Name), sanitizeName(method), strings.Join(callArgs, ", "))
							typ = m.Type.Return
							continue
						}
					}
				}
			}
			switch res {
			case "print":
				c.needPrint = true
				c.needFormat = true
				res = fmt.Sprintf("mochi_print([%s])", argStr)
			case "len":
				res = fmt.Sprintf("length(%s)", argStr)
			case "str":
				c.needFormat = true
				res = fmt.Sprintf("mochi_format(%s)", argStr)
			case "count":
				c.needCount = true
				res = fmt.Sprintf("mochi_count(%s)", argStr)
			case "avg":
				c.needAvg = true
				res = fmt.Sprintf("mochi_avg(%s)", argStr)
			case "sum":
				c.needSum = true
				res = fmt.Sprintf("mochi_sum(%s)", argStr)
			case "min":
				c.needMin = true
				res = fmt.Sprintf("mochi_min(%s)", argStr)
			case "max":
				c.needMax = true
				res = fmt.Sprintf("mochi_max(%s)", argStr)
			case "input":
				c.needInput = true
				res = "mochi_input()"
			default:
				res = fmt.Sprintf("%s(%s)", res, argStr)
			}
			if ft, ok := typ.(types.FuncType); ok {
				typ = ft.Return
			} else {
				typ = types.AnyType{}
			}
		} else if op.Cast != nil {
			typ = c.resolveTypeRef(op.Cast.Type)
			switch t := typ.(type) {
			case types.StructType:
				fn := "mochi_cast_" + sanitizeName(t.Name)
				if c.castStructs != nil {
					c.castStructs[t.Name] = t
				}
				res = fmt.Sprintf("%s(%s)", fn, res)
			case types.ListType:
				if st, ok := t.Elem.(types.StructType); ok {
					fn := "mochi_cast_" + sanitizeName(st.Name)
					if c.castStructs != nil {
						c.castStructs[st.Name] = st
					}
					res = fmt.Sprintf("[ %s(X) || X <- %s ]", fn, res)
				}
			default:
				// other casts ignored
			}
		}
	}
	return res, nil
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
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("\"%s\"", strings.ReplaceAll(*p.Lit.Str, "\"", "\\\"")), nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			// If the key is a simple selector like `n`, treat it as
			// an atom instead of a variable name.
			if sel := it.Key.Binary; sel != nil && sel.Left != nil && len(sel.Right) == 0 {
				u := sel.Left
				if len(u.Ops) == 0 && u.Value != nil {
					p := u.Value
					if len(p.Ops) == 0 && p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
						k = p.Target.Selector.Root
					}
				}
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s => %s", k, v)
		}
		return "#{" + strings.Join(items, ", ") + "}", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Struct != nil:
		if c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
				st := ut.Variants[p.Struct.Name]
				vals := map[string]string{}
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					vals[f.Name] = v
				}
				parts := make([]string, 0, len(st.Order))
				for _, n := range st.Order {
					v := "undefined"
					if val, ok := vals[n]; ok {
						v = val
					}
					parts = append(parts, fmt.Sprintf("%s=%s", sanitizeName(n), v))
				}
				return fmt.Sprintf("#%s{%s}", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
			}
			if st, ok := c.env.GetStruct(p.Struct.Name); ok {
				vals := map[string]string{}
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					vals[f.Name] = v
				}
				parts := make([]string, 0, len(st.Order))
				for _, n := range st.Order {
					v := "undefined"
					if val, ok := vals[n]; ok {
						v = val
					}
					parts = append(parts, fmt.Sprintf("%s=%s", sanitizeName(n), v))
				}
				return fmt.Sprintf("#%s{%s}", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
			}
		}
		fields := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s => %s", f.Name, v)
		}
		return "#{" + strings.Join(fields, ", ") + "}", nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Selector != nil:
		name := c.current(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			if len(p.Selector.Tail) == 1 && c.env != nil {
				if t, err := c.env.GetVar(p.Selector.Root); err == nil {
					if st, ok := t.(types.StructType); ok {
						field := sanitizeName(p.Selector.Tail[0])
						return fmt.Sprintf("%s#%s.%s", name, sanitizeName(st.Name), field), nil
					}
				}
			}
			for _, t := range p.Selector.Tail {
				name = fmt.Sprintf("maps:get(%s, %s)", t, name)
			}
			return name, nil
		}
		return name, nil
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, v)
		}
		argStr := strings.Join(args, ", ")
		switch p.Call.Func {
		case "print":
			c.needPrint = true
			c.needFormat = true
			return fmt.Sprintf("mochi_print([%s])", argStr), nil
		case "len":
			return fmt.Sprintf("length(%s)", argStr), nil
		case "str":
			c.needFormat = true
			return fmt.Sprintf("mochi_format(%s)", argStr), nil
		case "count":
			c.needCount = true
			return fmt.Sprintf("mochi_count(%s)", argStr), nil
		case "avg":
			c.needAvg = true
			return fmt.Sprintf("mochi_avg(%s)", argStr), nil
		case "sum":
			c.needSum = true
			return fmt.Sprintf("mochi_sum(%s)", argStr), nil
		case "min":
			c.needMin = true
			return fmt.Sprintf("mochi_min(%s)", argStr), nil
		case "max":
			c.needMax = true
			return fmt.Sprintf("mochi_max(%s)", argStr), nil
		case "input":
			c.needInput = true
			return "mochi_input()", nil
		case "json":
			c.needJSON = true
			return fmt.Sprintf("mochi_json(%s)", argStr), nil
		default:
			if v, ok := c.vars[p.Call.Func]; ok {
				return fmt.Sprintf("%s(%s)", v, argStr), nil
			}
			return fmt.Sprintf("%s(%s)", atomName(p.Call.Func), argStr), nil
		}
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 1 {
		for _, j := range q.Joins {
			if j.Side != nil {
				return "", fmt.Errorf("unsupported join side")
			}
		}
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	orig := c.env
	child := types.NewEnv(c.env)
	var elem types.Type = types.AnyType{}
	st := c.exprType(q.Source)
	if lt, ok := st.(types.ListType); ok {
		elem = lt.Elem
	} else if gt, ok := st.(types.GroupType); ok {
		elem = gt.Elem
	}
	child.SetVar(q.Var, elem, true)
	for _, f := range q.Froms {
		ft := c.exprType(f.Src)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		} else if gt, ok := ft.(types.GroupType); ok {
			fe = gt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := c.exprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		} else if gt, ok := jt.(types.GroupType); ok {
			je = gt.Elem
		}
		child.SetVar(j.Var, je, true)
	}
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		c.env = child
		var whereExpr string
		if q.Where != nil {
			w, err := c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
			whereExpr = w
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		val, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig
		srcExpr := src
		if whereExpr != "" {
			srcExpr = fmt.Sprintf("[%s || %s <- %s, %s]", capitalize(q.Var), capitalize(q.Var), src, whereExpr)
		}
		c.needGroup = true
		c.needGroupBy = true
		expr := fmt.Sprintf("[%s || %s <- mochi_group_by(%s, fun(%s) -> %s end)]",
			val, capitalize(q.Group.Name), srcExpr, capitalize(q.Var), keyExpr)
		return expr, nil
	}
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	var whereExpr string
	pushdown := false
	if q.Where != nil {
		w, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
		whereExpr = w
		ids := map[string]bool{}
		gatherIdentsExpr(q.Where, ids)
		pushdown = true
		for id := range ids {
			if id != q.Var {
				pushdown = false
				break
			}
		}
	}
	condParts := []string{}
	joinOns := []string{}
	joinSrc := []string{}
	if len(q.Joins) > 0 {
		joinOns = make([]string, len(q.Joins))
		joinSrc = make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinSrc[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinOns[i] = on
			if j.Side == nil {
				condParts = append(condParts, on)
			}
		}
	}
	if pushdown && whereExpr != "" {
		src = fmt.Sprintf("[%s || %s <- %s, %s]", capitalize(q.Var), capitalize(q.Var), src, whereExpr)
	} else if whereExpr != "" {
		condParts = append(condParts, whereExpr)
	}
	if len(condParts) > 0 {
		cond = strings.Join(condParts, ", ")
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

	specialJoin := len(q.Joins) == 1 && q.Joins[0].Side != nil &&
		(*q.Joins[0].Side == "right" || *q.Joins[0].Side == "outer")

	if sortExpr == "" && skipExpr == "" && takeExpr == "" {
		var b strings.Builder
		b.WriteString("[")
		b.WriteString(sel)
		b.WriteString(" || ")
		if specialJoin {
			side := *q.Joins[0].Side
			joinExpr := ""
			if side == "right" {
				joinExpr = fmt.Sprintf("mochi_right_join(%s, %s, fun(%s, %s) -> %s end)",
					src, joinSrc[0], capitalize(q.Var), capitalize(q.Joins[0].Var), joinOns[0])
				c.needRightJoin = true
			} else {
				joinExpr = fmt.Sprintf("mochi_outer_join(%s, %s, fun(%s, %s) -> %s end)",
					src, joinSrc[0], capitalize(q.Var), capitalize(q.Joins[0].Var), joinOns[0])
				c.needLeftJoin = true
				c.needRightJoin = true
				c.needOuterJoin = true
			}
			b.WriteString(fmt.Sprintf("{%s, %s} <- %s",
				capitalize(q.Var), capitalize(q.Joins[0].Var), joinExpr))
		} else {
			b.WriteString(capitalize(q.Var))
			b.WriteString(" <- ")
			b.WriteString(src)
		}
		for _, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			b.WriteString(", ")
			b.WriteString(capitalize(f.Var))
			b.WriteString(" <- ")
			b.WriteString(fs)
		}
		for i, j := range q.Joins {
			if specialJoin && i == 0 {
				continue
			}
			js := joinSrc[i]
			b.WriteString(", ")
			if j.Side != nil && *j.Side == "left" {
				on := joinOns[i]
				b.WriteString("{" + capitalize(q.Var) + ", " + capitalize(j.Var) + "} <- mochi_left_join_item(" + capitalize(q.Var) + ", " + js + ", fun(" + capitalize(q.Var) + ", " + capitalize(j.Var) + ") -> " + on + " end)")
				c.needLeftJoin = true
			} else {
				b.WriteString(capitalize(j.Var))
				b.WriteString(" <- ")
				b.WriteString(js)
			}
		}
		if cond != "" {
			b.WriteString(", ")
			b.WriteString(cond)
		}
		b.WriteString("]")
		return b.String(), nil
	}

	var b strings.Builder
	b.WriteString("(fun() ->\n")
	b.WriteString("\tItems = [")
	if sortExpr != "" {
		b.WriteString("{" + sortExpr + ", " + sel + "}")
	} else {
		b.WriteString(sel)
	}
	if specialJoin {
		side := *q.Joins[0].Side
		joinExpr := ""
		if side == "right" {
			joinExpr = fmt.Sprintf("mochi_right_join(%s, %s, fun(%s, %s) -> %s end)",
				src, joinSrc[0], capitalize(q.Var), capitalize(q.Joins[0].Var), joinOns[0])
			c.needRightJoin = true
		} else {
			joinExpr = fmt.Sprintf("mochi_outer_join(%s, %s, fun(%s, %s) -> %s end)",
				src, joinSrc[0], capitalize(q.Var), capitalize(q.Joins[0].Var), joinOns[0])
			c.needLeftJoin = true
			c.needRightJoin = true
			c.needOuterJoin = true
		}
		b.WriteString(" || {" + capitalize(q.Var) + ", " + capitalize(q.Joins[0].Var) + "} <- " + joinExpr)
	} else {
		b.WriteString(" || " + capitalize(q.Var) + " <- " + src)
	}
	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(", " + capitalize(f.Var) + " <- " + fs)
	}
	for i, j := range q.Joins {
		if specialJoin && i == 0 {
			continue
		}
		js := joinSrc[i]
		b.WriteString(", ")
		if j.Side != nil && *j.Side == "left" {
			on := joinOns[i]
			b.WriteString("{" + capitalize(q.Var) + ", " + capitalize(j.Var) + "} <- mochi_left_join_item(" + capitalize(q.Var) + ", " + js + ", fun(" + capitalize(q.Var) + ", " + capitalize(j.Var) + ") -> " + on + " end)")
			c.needLeftJoin = true
		} else {
			b.WriteString(capitalize(j.Var) + " <- " + js)
		}
	}
	if cond != "" {
		b.WriteString(", " + cond)
	}
	b.WriteString("],\n")
	b.WriteString("\tSorted = ")
	if sortExpr != "" {
		b.WriteString("begin\n")
		b.WriteString("\t\tSPairs = lists:sort(fun({A,_},{B,_}) -> A =< B end, Items),\n")
		b.WriteString("\t\t[ V || {_, V} <- SPairs ]\n")
		b.WriteString("\tend")
	} else {
		b.WriteString("Items")
	}
	b.WriteString(",\n")
	b.WriteString("\tSkipped = ")
	if skipExpr != "" {
		b.WriteString("(case " + skipExpr + " of\n")
		b.WriteString("\t\tN when N > 0 -> lists:nthtail(N, Sorted);\n")
		b.WriteString("\t\t_ -> Sorted\n")
		b.WriteString("\tend)")
	} else {
		b.WriteString("Sorted")
	}
	b.WriteString(",\n")
	b.WriteString("\tTaken = ")
	if takeExpr != "" {
		b.WriteString("lists:sublist(Skipped, " + takeExpr + ")")
	} else {
		b.WriteString("Skipped")
	}
	b.WriteString(",\n")
	b.WriteString("\tTaken\n")
	b.WriteString("end)()")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	tmp := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++
	var b strings.Builder
	b.WriteString("(fun() ->\n")
	b.WriteString("\t" + tmp + " = " + target + ",\n")
	b.WriteString("\tcase " + tmp + " of\n")
	for i, cs := range m.Cases {
		pat, err := c.compileMatchPattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		b.WriteString("\t\t" + pat + " -> " + res)
		if i != len(m.Cases)-1 {
			b.WriteString(";\n")
		} else {
			b.WriteString("\n")
		}
	}
	b.WriteString("\tend\nend)()")
	return b.String(), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	savedVars := c.vars
	savedCounts := c.counts
	c.vars = map[string]string{}
	if c.counts == nil {
		c.counts = map[string]int{}
	}
	for i, p := range fn.Params {
		params[i] = c.newName(p.Name)
	}

	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		child.SetVar(p.Name, types.AnyType{}, true)
	}
	origEnv := c.env
	c.env = child

	savedBuf := c.buf
	c.buf = bytes.Buffer{}
	savedIndent := c.indent
	c.indent = 1
	c.writeln("try")
	c.indent++
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.env = origEnv
			c.buf = savedBuf
			c.indent = savedIndent
			return "", err
		}
		c.writeln("throw({return, " + expr + "})")
	} else {
		if err := c.compileBlock(fn.BlockBody, false, nil); err != nil {
			c.env = origEnv
			c.buf = savedBuf
			c.indent = savedIndent
			return "", err
		}
	}
	c.indent--
	c.writeln("catch")
	c.indent++
	c.writeln("throw:{return, V} -> V")
	c.indent--
	c.writeln("end")
	body := indentBlock(c.buf.String(), 1)
	c.buf = savedBuf
	c.indent = savedIndent
	c.env = origEnv
	c.vars = savedVars
	c.counts = savedCounts

	return fmt.Sprintf("fun(%s) ->\n%send", strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileMatchPattern(e *parser.Expr) (string, error) {
	if isUnderscoreExpr(e) {
		return "_", nil
	}
	if call, ok := callPattern(e); ok {
		if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
			st := ut.Variants[call.Func]
			parts := make([]string, len(st.Order))
			for idx, field := range st.Order {
				if idx < len(call.Args) {
					if id, ok := identName(call.Args[idx]); ok {
						if id == "_" {
							parts[idx] = fmt.Sprintf("%s=_", sanitizeName(field))
						} else {
							parts[idx] = fmt.Sprintf("%s=%s", sanitizeName(field), capitalize(id))
						}
					} else {
						return "", fmt.Errorf("unsupported pattern")
					}
				} else {
					parts[idx] = fmt.Sprintf("%s=_", sanitizeName(field))
				}
			}
			return fmt.Sprintf("#%s{%s}", sanitizeName(call.Func), strings.Join(parts, ", ")), nil
		}
	}
	if ident, ok := identName(e); ok {
		if _, ok := c.env.FindUnionByVariant(ident); ok {
			return fmt.Sprintf("#%s{}", sanitizeName(ident)), nil
		}
		return capitalize(ident), nil
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	c.needIO = true
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "undefined"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	return fmt.Sprintf("mochi_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	c.needIO = true
	c.needJSON = true
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "undefined"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	return fmt.Sprintf("mochi_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "undefined"
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.needFetch = true
	c.needJSON = true
	return fmt.Sprintf("mochi_fetch(%s, %s)", url, opts), nil
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
			params = append(params, fmt.Sprintf("%s := %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "undefined"
	if len(params) > 0 {
		paramStr = "#{" + strings.Join(params, ", ") + "}"
	}
	if model == "" {
		model = "\"\""
	}
	switch g.Target {
	case "embedding":
		c.needGenEmbed = true
		return fmt.Sprintf("mochi_gen_embed(%s, %s, %s)", text, model, paramStr), nil
	default:
		if c.env != nil {
			if _, ok := c.env.GetStruct(g.Target); ok {
				c.needGenStruct = true
				return fmt.Sprintf("mochi_gen_struct(%s, %s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
			}
		}
		c.needGenText = true
		return fmt.Sprintf("mochi_gen_text(%s, %s, %s)", prompt, model, paramStr), nil
	}
}

func isListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isListPostfix(u.Value, env)
}

func isListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	if p.Target != nil {
		if p.Target.List != nil {
			return true
		}
		if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
			if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	}
	return false
}

func isStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) staticTypeOfPostfix(p *parser.PostfixExpr) (types.Type, bool) {
	if p == nil || len(p.Ops) > 0 {
		return nil, false
	}
	return c.staticTypeOfPrimary(p.Target)
}

func (c *Compiler) staticTypeOfPrimary(p *parser.Primary) (types.Type, bool) {
	if p == nil {
		return nil, false
	}
	switch {
	case p.List != nil:
		return types.ListType{Elem: types.AnyType{}}, true
	case p.Map != nil:
		return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}, true
	case p.Selector != nil && len(p.Selector.Tail) == 0 && c.env != nil:
		t, err := c.env.GetVar(p.Selector.Root)
		if err == nil {
			return t, true
		}
	}
	return nil, false
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

func (c *Compiler) compileExternVar(ev *parser.ExternVarDecl) error {
	name := c.newName(ev.Name())
	c.buf.WriteString(fmt.Sprintf("%s = undefined", name))
	if c.env != nil {
		c.env.SetVar(ev.Name(), c.resolveTypeRef(ev.Type), true)
	}
	return nil
}

func (c *Compiler) compileExternFun(ef *parser.ExternFunDecl) error {
	params := make([]string, len(ef.Params))
	paramTypes := make([]types.Type, len(ef.Params))
	for i, p := range ef.Params {
		params[i] = c.newName(p.Name)
		if c.env != nil {
			paramTypes[i] = c.resolveTypeRef(p.Type)
		}
	}
	c.writeln(fmt.Sprintf("%s(%s) -> erlang:error({extern, '%s'}).", atomName(ef.Name()), strings.Join(params, ", "), ef.Name()))
	if c.env != nil {
		ft := types.FuncType{Params: paramTypes, Return: c.resolveTypeRef(ef.Return)}
		c.env.SetVar(ef.Name(), ft, false)
	}
	return nil
}

func (c *Compiler) compileExternType(et *parser.ExternTypeDecl) error {
	c.writeln(fmt.Sprintf("-type %s() :: any().", sanitizeName(et.Name)))
	return nil
}

func (c *Compiler) compileExternObject(eo *parser.ExternObjectDecl) error {
	name := c.newName(eo.Name)
	c.buf.WriteString(fmt.Sprintf("%s = undefined", name))
	if c.env != nil {
		c.env.SetVar(eo.Name, types.AnyType{}, true)
	}
	return nil
}
