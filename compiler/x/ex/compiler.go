//go:build slow

// Package excode translates Mochi ASTs into Elixir source code.
package excode

import (
	"bytes"
	"fmt"
	"regexp"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Elixir source code.
type Compiler struct {
	buf        bytes.Buffer
	indent     int
	env        *types.Env
	tmp        int
	helpers    map[string]bool
	funcs      map[string]bool
	structs    map[string]types.StructType
	structDefs []string
	Module     string
	attrs      map[string]string
}

var atomIdent = regexp.MustCompile(`^[a-z_][a-zA-Z0-9_]*$`)

var exReserved = map[string]bool{
	"end": true,
}

func sanitizeName(name string) string {
	if name == "_" {
		return "v"
	}
	if exReserved[name] {
		return name + "_"
	}
	return name
}

func assignedVars(stmts []*parser.Statement) []string {
	set := map[string]struct{}{}
	decl := map[string]struct{}{}

	var walkIf func(*parser.IfStmt)
	var walk func([]*parser.Statement)

	walkIf = func(ifst *parser.IfStmt) {
		if ifst == nil {
			return
		}
		walk(ifst.Then)
		walk(ifst.Else)
		walkIf(ifst.ElseIf)
	}

	walk = func(st []*parser.Statement) {
		for _, s := range st {
			if s.Assign != nil {
				set[s.Assign.Name] = struct{}{}
			}
			if s.Var != nil {
				decl[s.Var.Name] = struct{}{}
			}
			if s.Let != nil {
				decl[s.Let.Name] = struct{}{}
			}
			if s.For != nil {
				walk(s.For.Body)
			}
			if s.While != nil {
				walk(s.While.Body)
			}
			if s.If != nil {
				walkIf(s.If)
			}
		}
	}

	walk(stmts)

	for v := range decl {
		delete(set, v)
	}
	vars := make([]string, 0, len(set))
	for v := range set {
		vars = append(vars, v)
	}
	sort.Strings(vars)
	return vars
}

func usesLoopControl(stmts []*parser.Statement) bool {
	var walkIf func(*parser.IfStmt) bool
	var walk func([]*parser.Statement) bool

	walkIf = func(ifst *parser.IfStmt) bool {
		if ifst == nil {
			return false
		}
		if walk(ifst.Then) {
			return true
		}
		if walk(ifst.Else) {
			return true
		}
		return walkIf(ifst.ElseIf)
	}

	walk = func(st []*parser.Statement) bool {
		for _, s := range st {
			if s.Break != nil || s.Continue != nil {
				return true
			}
			if s.For != nil {
				if walk(s.For.Body) {
					return true
				}
			}
			if s.While != nil {
				if walk(s.While.Body) {
					return true
				}
			}
			if s.If != nil {
				if walkIf(s.If) {
					return true
				}
			}
		}
		return false
	}

	return walk(stmts)
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), funcs: make(map[string]bool), structs: make(map[string]types.StructType), Module: "Main", structDefs: []string{}, attrs: make(map[string]string)}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) newTemp() string {
	c.tmp++
	return fmt.Sprintf("t%d", c.tmp)
}

// typeSpec converts a Mochi type into an Elixir type spec string.
func (c *Compiler) typeSpec(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "integer()"
	case types.FloatType:
		return "float()"
	case types.StringType:
		return "String.t()"
	case types.BoolType:
		return "boolean()"
	case types.ListType:
		return "list(" + c.typeSpec(tt.Elem) + ")"
	case types.MapType:
		return "map()"
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = c.typeSpec(p)
		}
		return "(" + strings.Join(params, ", ") + " -> " + c.typeSpec(tt.Return) + ")"
	case types.VoidType:
		return "nil"
	default:
		return "any()"
	}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	mod := c.Module
	if mod == "" {
		mod = "Main"
	}
	c.writeln(fmt.Sprintf("defmodule %s do", mod))
	c.indent++
	// collect top-level constants as module attributes
	helperRe := regexp.MustCompile(`_[a-zA-Z0-9]+\(`)
	letNames := []string{}
	for _, s := range prog.Statements {
		if s.Let != nil {
			letNames = append(letNames, sanitizeName(s.Let.Name))
		}
	}
	containsVar := func(expr, self string) bool {
		for _, n := range letNames {
			if n == self {
				continue
			}
			if strings.Contains(expr, n) {
				return true
			}
		}
		return false
	}
	for _, s := range prog.Statements {
		if s.Let != nil {
			val := "nil"
			if s.Let.Value != nil {
				v, err := c.compileExpr(s.Let.Value)
				if err != nil {
					return nil, err
				}
				val = v
			}
			name := sanitizeName(s.Let.Name)
			// avoid module attributes that invoke helper functions or reference other vars
			if !helperRe.MatchString(val) && !containsVar(val, name) {
				c.attrs[name] = val
				c.writeln(fmt.Sprintf("@%s %s", name, val))
			}
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("def main do")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun == nil {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
	}
	c.indent--
	c.writeln("end")
	c.emitRuntime()
	c.writeln("end")
	c.indent--
	mod = c.Module
	if mod == "" {
		mod = "Main"
	}
	c.writeln(fmt.Sprintf("%s.main()", mod))
	code := c.buf.Bytes()
	if formatted, err := Format(code); err == nil {
		code = formatted
	}
	var out bytes.Buffer
	out.WriteString("# Generated by Mochi Elixir compiler\n")
	for _, def := range c.structDefs {
		out.WriteString(def)
	}
	out.Write(code)
	return out.Bytes(), nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	if c.indent > 1 {
		if c.env != nil {
			c.env.SetVar(fun.Name, types.FuncType{}, false)
		}
		expr, err := c.compileFunExpr(&parser.FunExpr{Params: fun.Params, BlockBody: fun.Body})
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", sanitizeName(fun.Name), expr))
		return nil
	}

	// record top-level function name for call handling
	c.funcs[fun.Name] = true

	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				params := make([]string, len(ft.Params))
				for i, p := range ft.Params {
					params[i] = c.typeSpec(p)
				}
				ret := c.typeSpec(ft.Return)
				c.writeln(fmt.Sprintf("@spec %s(%s) :: %s", sanitizeName(fun.Name), strings.Join(params, ", "), ret))
			}
		}
	}

	c.writeIndent()
	c.buf.WriteString("def " + sanitizeName(fun.Name) + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
	}
	c.buf.WriteString(") do\n")
	c.indent++
	c.writeln("try do")
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("catch {:return, v} -> v end")
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		if _, ok := c.attrs[sanitizeName(s.Let.Name)]; !ok {
			var val string
			var err error
			if s.Let.Value != nil {
				val, err = c.compileExpr(s.Let.Value)
				if err != nil {
					return err
				}
			} else {
				val = "nil"
			}
			name := sanitizeName(s.Let.Name)
			if c.env != nil {
				if t, err := c.env.GetVar(s.Let.Name); err == nil {
					c.writeln(fmt.Sprintf("# %s :: %s", name, c.typeSpec(t)))
				}
			}
			c.writeln(fmt.Sprintf("%s = %s", name, val))
		}
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("throw {:return, %s}", val))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("throw :break")
	case s.Continue != nil:
		c.writeln("throw :continue")
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Fun != nil:
		return c.compileFunStmt(s.Fun)
	default:
		// ignore
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s do", cond))
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil || len(stmt.Else) > 0 {
		c.writeln("else")
		c.indent++
		if stmt.ElseIf != nil {
			if err := c.compileIf(stmt.ElseIf); err != nil {
				return err
			}
		} else {
			for _, s := range stmt.Else {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
		}
		c.indent--
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}

	vars := assignedVars(stmt.Body)
	for i, v := range vars {
		vars[i] = sanitizeName(v)
	}
	loop := c.newTemp()

	if len(vars) == 0 {
		c.writeln(fmt.Sprintf("%s = fn %s ->", loop, loop))
	} else {
		params := append([]string{loop}, vars...)
		c.writeln(fmt.Sprintf("%s = fn %s ->", loop, strings.Join(params, ", ")))
	}
	c.indent++
	c.writeln("try do")
	c.indent++
	c.writeln(fmt.Sprintf("if %s do", cond))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	callArgs := append([]string{loop}, vars...)
	c.writeln(fmt.Sprintf("%s.(%s)", loop, strings.Join(callArgs, ", ")))
	c.indent--
	c.writeln("else")
	c.indent++
	if len(vars) == 0 {
		c.writeln(":ok")
	} else {
		c.writeln(fmt.Sprintf("{:ok, %s}", strings.Join(vars, ", ")))
	}
	c.indent--
	c.writeln("end")
	c.indent--
	c.writeln("catch :break ->")
	c.indent++
	if len(vars) == 0 {
		c.writeln(":ok")
	} else {
		c.writeln(fmt.Sprintf("{:ok, %s}", strings.Join(vars, ", ")))
	}
	c.indent--
	c.writeln("end")
	c.indent--
	c.writeln("end")
	if len(vars) == 0 {
		c.writeln(fmt.Sprintf("%s.(%s)", loop, loop))
	} else {
		callArgs := append([]string{loop}, vars...)
		c.writeln(fmt.Sprintf("{_, %s} = %s.(%s)", strings.Join(vars, ", "), loop, strings.Join(callArgs, ", ")))
		for _, v := range vars {
			c.writeln(fmt.Sprintf("_ = %s", v))
		}
	}
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	mutated := assignedVars(stmt.Body)
	for i, v := range mutated {
		mutated[i] = sanitizeName(v)
	}

	srcExpr, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	t := c.inferExprType(stmt.Source)
	if stmt.RangeEnd != nil {
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		srcExpr = fmt.Sprintf("%s..(%s - 1)", srcExpr, end)
	} else if strings.HasPrefix(srcExpr, "\"") && strings.HasSuffix(srcExpr, "\"") {
		srcExpr = fmt.Sprintf("String.graphemes(%s)", srcExpr)
	} else {
		if _, ok := t.(types.MapType); ok {
			srcExpr = fmt.Sprintf("Map.keys(%s)", srcExpr)
		} else if _, ok := t.(types.AnyType); ok {
			c.use("_iter")
			srcExpr = fmt.Sprintf("_iter(%s)", srcExpr)
		}
	}

	needCtrl := usesLoopControl(stmt.Body)

	if len(mutated) == 0 && !needCtrl {
		c.writeln(fmt.Sprintf("for %s <- %s do", sanitizeName(name), srcExpr))
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end")
		return nil
	}

	tuple := strings.Join(mutated, ", ")
	accInit := ":ok"
	accPat := "_acc"
	if len(mutated) > 0 {
		accInit = fmt.Sprintf("{%s}", tuple)
		accPat = fmt.Sprintf("{%s}", tuple)
	}
	reduce := "Enum.reduce"
	if needCtrl {
		reduce = "Enum.reduce_while"
	}
	if len(mutated) > 0 {
		c.writeln(fmt.Sprintf("%s = %s(%s, %s, fn %s, %s ->", accPat, reduce, srcExpr, accInit, sanitizeName(name), accPat))
	} else {
		c.writeln(fmt.Sprintf("_ = %s(%s, %s, fn %s, %s ->", reduce, srcExpr, accInit, sanitizeName(name), accPat))
	}
	c.indent++
	if needCtrl {
		c.writeln("try do")
		c.indent++
	}
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(mutated) > 0 {
		c.writeln(fmt.Sprintf("{:cont, {%s}}", tuple))
	} else {
		c.writeln("{:cont, :ok}")
	}
	if needCtrl {
		c.indent--
		c.writeln("catch")
		c.indent++
		if len(mutated) > 0 {
			c.writeln(fmt.Sprintf(":break -> {:halt, {%s}}", tuple))
			c.writeln(fmt.Sprintf(":continue -> {:cont, {%s}}", tuple))
		} else {
			c.writeln(":break -> {:halt, :ok}")
			c.writeln(":continue -> {:cont, :ok}")
		}
		c.indent--
		c.writeln("end")
	}
	c.indent--
	c.writeln("end)")
	for _, v := range mutated {
		c.writeln(fmt.Sprintf("_ = %s", v))
	}
	return nil
}

func (c *Compiler) compileUpdate(stmt *parser.UpdateStmt) error {
	name := sanitizeName(stmt.Target)

	fields := []string{}
	orig := c.env
	child := types.NewEnv(c.env)
	if c.env != nil {
		if t, err := c.env.GetVar(stmt.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					for _, f := range st.Order {
						fields = append(fields, f)
						child.SetVar(f, st.Fields[f], true)
					}
				}
			}
		}
	}
	c.env = child
	var cond string
	var err error
	if stmt.Where != nil {
		cond, err = c.compileExpr(stmt.Where)
		if err != nil {
			c.env = orig
			return err
		}
	}
	pairs := make([]string, len(stmt.Set.Items))
	for i, it := range stmt.Set.Items {
		key, ok := identName(it.Key)
		if !ok {
			c.env = orig
			return fmt.Errorf("update key must be identifier")
		}
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = orig
			return err
		}
		pairs[i] = fmt.Sprintf("%s: %s", sanitizeName(key), val)
	}
	c.env = orig

	c.writeln(fmt.Sprintf("%s = Enum.map(%s, fn it ->", name, name))
	c.indent++
	if len(fields) > 0 {
		parts := make([]string, len(fields))
		for i, f := range fields {
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f), sanitizeName(f))
		}
		c.writeln(fmt.Sprintf("%%{%s} = it", strings.Join(parts, ", ")))
	}
	if cond != "" {
		c.writeln(fmt.Sprintf("if %s do", cond))
		c.indent++
	}
	c.writeln(fmt.Sprintf("%%{it | %s}", strings.Join(pairs, ", ")))
	if cond != "" {
		c.indent--
		c.writeln("else")
		c.indent++
		c.writeln("it")
		c.indent--
		c.writeln("end")
	}
	c.indent--
	c.writeln("end)")
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	name := sanitizeName(stmt.Name)
	value := "nil"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		value = v
	}
	if c.env != nil {
		if t, err := c.env.GetVar(stmt.Name); err == nil {
			c.writeln(fmt.Sprintf("# %s :: %s", name, c.typeSpec(t)))
			if st, ok := t.(types.StructType); ok {
				c.ensureStruct(st)
				c.use("_structify")
				value = fmt.Sprintf("_structify(%s, %s)", sanitizeName(st.Name), value)
			}
		}
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	c.writeln(fmt.Sprintf("_ = %s", name))
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	name := sanitizeName(stmt.Name)
	value, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	if c.env != nil {
		if t, err2 := c.env.GetVar(stmt.Name); err2 == nil {
			if st, ok := t.(types.StructType); ok {
				c.ensureStruct(st)
				c.use("_structify")
				value = fmt.Sprintf("_structify(%s, %s)", sanitizeName(st.Name), value)
			}
		}
	}
	if len(stmt.Index) > 0 {
		if t, err2 := c.env.GetVar(stmt.Name); err2 == nil {
			expr, err := c.compileIndexAssign(name, stmt.Index, value, t)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s = %s", name, expr))
			return nil
		}
		idx, err := c.compileExpr(stmt.Index[0].Start)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = Map.put(%s, %s, %s)", name, name, idx, value))
		return nil
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	return nil
}

func (c *Compiler) compileIndexAssign(base string, idxs []*parser.IndexOp, value string, t types.Type) (string, error) {
	if len(idxs) == 0 {
		return value, nil
	}
	idx, err := c.compileExpr(idxs[0].Start)
	if err != nil {
		return "", err
	}
	var inner types.Type
	switch tt := t.(type) {
	case types.ListType:
		inner = tt.Elem
		rest, err := c.compileIndexAssign("it", idxs[1:], value, inner)
		if err != nil {
			return "", err
		}
		if len(idxs) == 1 {
			return fmt.Sprintf("List.replace_at(%s, %s, %s)", base, idx, rest), nil
		}
		return fmt.Sprintf("List.update_at(%s, %s, fn it -> %s end)", base, idx, rest), nil
	case types.MapType:
		inner = tt.Value
		rest, err := c.compileIndexAssign("it", idxs[1:], value, inner)
		if err != nil {
			return "", err
		}
		if len(idxs) == 1 {
			return fmt.Sprintf("Map.put(%s, %s, %s)", base, idx, rest), nil
		}
		return fmt.Sprintf("Map.update!(%s, %s, fn it -> %s end)", base, idx, rest), nil
	default:
		rest, err := c.compileIndexAssign("it", idxs[1:], value, types.AnyType{})
		if err != nil {
			return "", err
		}
		if len(idxs) == 1 {
			return fmt.Sprintf("Map.put(%s, %s, %s)", base, idx, rest), nil
		}
		return fmt.Sprintf("Map.update!(%s, %s, fn it -> %s end)", base, idx, rest), nil
	}
}

// aggregatorInfo checks if the expression is a simple aggregator call like
// `sum(x)` and returns the helper name and the argument expression if so.
func aggregatorInfo(e *parser.Expr) (string, *parser.Expr) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", nil
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return "", nil
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil
	}
	switch call.Func {
	case "sum", "avg", "count", "min", "max", "first":
		return "_" + call.Func, call.Args[0]
	}
	return "", nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	if t := c.inferExprType(q.Source); t != nil {
		if _, ok := t.(types.GroupType); ok {
			src += ".items"
		}
	}
	orig := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, f := range q.Froms {
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	for _, j := range q.Joins {
		child.SetVar(j.Var, types.AnyType{}, true)
	}
	hasSide := false
	for _, j := range q.Joins {
		if j.Side != nil {
			hasSide = true
			break
		}
	}
	c.env = child

	// simple group-by without joins or filters
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
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
		c.use("_group_by")
		c.use("_group")
		expr := fmt.Sprintf("Enum.map(_group_by(%s, fn %s -> %s end), fn %s -> %s end)", src, q.Var, keyExpr, q.Group.Name, val)
		return expr, nil
	}

	agg, arg := aggregatorInfo(q.Select)
	if agg != "" {
		c.use(agg)
	}
	targetExpr := q.Select
	if arg != nil {
		targetExpr = arg
	}
	sel, err := c.compileExpr(targetExpr)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
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
		fromSrcs := make([]string, len(q.Froms))
		varNames := []string{sanitizeName(q.Var)}
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			fromSrcs[i] = fs
			varNames = append(varNames, sanitizeName(f.Var))
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		joinLeft := make([]bool, len(q.Joins))
		joinRight := make([]bool, len(q.Joins))
		paramCopy := append([]string(nil), varNames...)
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				return "", err
			}
			joinOns[i] = on
			if j.Side != nil {
				switch *j.Side {
				case "left":
					joinLeft[i] = true
				case "right":
					joinRight[i] = true
				case "outer":
					joinLeft[i] = true
					joinRight[i] = true
				}
			}
			paramCopy = append(paramCopy, sanitizeName(j.Var))
		}
		allParams := strings.Join(paramCopy, ", ")
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
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
		selectFn := fmt.Sprintf("fn %s -> [%s] end", allParams, allParams)
		whereFn := ""
		if cond != "" {
			whereFn = fmt.Sprintf("fn %s -> %s end", allParams, cond)
		}
		var b strings.Builder
		b.WriteString("(fn ->\n")
		b.WriteString("\tsrc = " + src + "\n")
		b.WriteString("\trows = _query(src, [\n")
		specs := make([]string, 0, len(fromSrcs)+len(joinSrcs))
		params := []string{sanitizeName(q.Var)}
		for i, fs := range fromSrcs {
			specs = append(specs, fmt.Sprintf("%%{items: %s}", fs))
			params = append(params, sanitizeName(q.Froms[i].Var))
		}
		paramCopy = append([]string(nil), params...)
		for i, js := range joinSrcs {
			onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
			spec := fmt.Sprintf("%%{items: %s, on: fn %s -> %s end", js, strings.Join(onParams, ", "), joinOns[i])
			if joinLeft[i] {
				spec += ", left: true"
			}
			if joinRight[i] {
				spec += ", right: true"
			}
			spec += "}"
			specs = append(specs, spec)
			paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
		}
		for i, j := range specs {
			b.WriteString("\t\t" + j)
			if i != len(specs)-1 {
				b.WriteString(",")
			}
			b.WriteString("\n")
		}
		b.WriteString("\t], %{select: " + selectFn)
		if whereFn != "" {
			b.WriteString(", where: " + whereFn)
		}
		if skipExpr != "" {
			b.WriteString(", skip: " + skipExpr)
		}
		if takeExpr != "" {
			b.WriteString(", take: " + takeExpr)
		}
		b.WriteString(" })\n")
		b.WriteString(fmt.Sprintf("\tgroups = _group_by(rows, fn [%s] -> %s end)\n", allParams, keyExpr))
		// Keep full rows in group items to support nested queries
		b.WriteString("\titems = groups\n")
		if sortExpr != "" {
			b.WriteString(fmt.Sprintf("\titems = Enum.sort_by(items, fn %s -> %s end)\n", sanitizeName(q.Group.Name), sortExpr))
		}
		if skipExpr != "" {
			b.WriteString("\titems = Enum.drop(items, " + skipExpr + ")\n")
		}
		if takeExpr != "" {
			b.WriteString("\titems = Enum.take(items, " + takeExpr + ")\n")
		}
		b.WriteString(fmt.Sprintf("\tEnum.map(items, fn %s -> %s end)\n", sanitizeName(q.Group.Name), val))
		b.WriteString("end).()")
		c.use("_query")
		c.use("_group_by")
		c.use("_group")
		return b.String(), nil
	}

	if hasSide {
		sideIdx := -1
		outer := false
		for i, j := range q.Joins {
			if j.Side != nil {
				if sideIdx != -1 {
					outer = true
					break
				}
				sideIdx = i
				if *j.Side == "outer" {
					outer = true
				}
			}
		}

		if sideIdx != -1 && !outer {
			loops := []string{}
			conds := []string{}
			varNames := []string{sanitizeName(q.Var)}
			for _, f := range q.Froms {
				fs, err := c.compileExpr(f.Src)
				if err != nil {
					return "", err
				}
				loops = append(loops, fmt.Sprintf("%s <- %s", sanitizeName(f.Var), fs))
				varNames = append(varNames, sanitizeName(f.Var))
			}
			sideSrc := ""
			sideOn := ""
			sideVar := ""
			for i, j := range q.Joins {
				js, err := c.compileExpr(j.Src)
				if err != nil {
					return "", err
				}
				on, err := c.compileExpr(j.On)
				if err != nil {
					return "", err
				}
				if i == sideIdx {
					sideSrc = js
					sideOn = on
					sideVar = sanitizeName(j.Var)
				} else {
					loops = append(loops, fmt.Sprintf("%s <- %s", sanitizeName(j.Var), js))
					varNames = append(varNames, sanitizeName(j.Var))
					conds = append(conds, on)
				}
			}
			if len(conds) > 0 {
				jc := strings.Join(conds, " && ")
				if cond != "" {
					cond = fmt.Sprintf("(%s) && (%s)", jc, cond)
				} else {
					cond = jc
				}
			}

			var b strings.Builder
			b.WriteString("for ")
			b.WriteString(sanitizeName(q.Var))
			b.WriteString(" <- ")
			b.WriteString(src)
			for _, p := range loops {
				b.WriteString(", ")
				b.WriteString(p)
			}
			if cond != "" {
				b.WriteString(", ")
				b.WriteString(cond)
			}
			b.WriteString(" do\n")
			b.WriteString("\t" + sideVar + " = Enum.find(" + sideSrc + ", fn " + sideVar + " -> " + sideOn + " end)\n")
			b.WriteString("\t" + sel + "\n")
			b.WriteString("end")

			expr := b.String()
			if sortExpr != "" {
				expr = fmt.Sprintf("Enum.sort_by((%s), fn {_, k} -> k end)", expr)
			}
			if skipExpr != "" {
				expr = fmt.Sprintf("Enum.drop(%s, %s)", expr, skipExpr)
			}
			if takeExpr != "" {
				expr = fmt.Sprintf("Enum.take(%s, %s)", expr, takeExpr)
			}
			if sortExpr != "" {
				expr = fmt.Sprintf("Enum.map(%s, fn {v, _} -> v end)", expr)
			}

			return expr, nil
		}

		fromSrcs := make([]string, len(q.Froms))
		varNames := []string{sanitizeName(q.Var)}
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			fromSrcs[i] = fs
			varNames = append(varNames, sanitizeName(f.Var))
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		joinLeft := make([]bool, len(q.Joins))
		joinRight := make([]bool, len(q.Joins))
		paramCopy := append([]string(nil), varNames...)
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				return "", err
			}
			joinOns[i] = on
			if j.Side != nil {
				switch *j.Side {
				case "left":
					joinLeft[i] = true
				case "right":
					joinRight[i] = true
				case "outer":
					joinLeft[i] = true
					joinRight[i] = true
				}
			}
			paramCopy = append(paramCopy, sanitizeName(j.Var))
		}

		joins := make([]string, 0, len(fromSrcs)+len(joinSrcs))
		params := []string{sanitizeName(q.Var)}
		for i, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("%%{items: %s}", fs))
			params = append(params, sanitizeName(q.Froms[i].Var))
		}
		paramCopy = append([]string(nil), params...)
		for i, js := range joinSrcs {
			onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
			spec := fmt.Sprintf("%%{items: %s, on: fn %s -> %s end", js, strings.Join(onParams, ", "), joinOns[i])
			if joinLeft[i] {
				spec += ", left: true"
			}
			if joinRight[i] {
				spec += ", right: true"
			}
			spec += "}"
			joins = append(joins, spec)
			paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
		}
		allParams := strings.Join(paramCopy, ", ")
		selectFn := fmt.Sprintf("fn %s -> %s end", allParams, sel)
		whereFn := ""
		if cond != "" {
			whereFn = fmt.Sprintf("fn %s -> %s end", allParams, cond)
		}
		sortFn := ""
		if sortExpr != "" {
			sortFn = fmt.Sprintf("fn %s -> %s end", allParams, sortExpr)
		}
		var b strings.Builder
		b.WriteString("(fn ->\n")
		b.WriteString("\tsrc = " + src + "\n")
		b.WriteString("\t_query(src, [\n")
		for i, j := range joins {
			b.WriteString("\t\t" + j)
			if i != len(joins)-1 {
				b.WriteString(",")
			}
			b.WriteString("\n")
		}
		b.WriteString("\t], %{select: " + selectFn)
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
		b.WriteString(" })\n")
		b.WriteString("end).()")
		c.use("_query")
		return b.String(), nil
	} else if len(q.Froms) > 0 || len(q.Joins) > 0 {
		parts := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%s <- %s", sanitizeName(f.Var), fs))
		}
		joinConds := []string{}
		for _, j := range q.Joins {
			if j.Side != nil {
				return "", fmt.Errorf("unsupported join type")
			}
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%s <- %s", sanitizeName(j.Var), js))
			onExpr, err := c.compileExpr(j.On)
			if err != nil {
				return "", err
			}
			joinConds = append(joinConds, onExpr)
		}

		if len(joinConds) > 0 {
			jc := strings.Join(joinConds, " && ")
			if cond != "" {
				cond = fmt.Sprintf("(%s) && (%s)", jc, cond)
			} else {
				cond = jc
			}
		}

		item := sel
		if sortExpr != "" {
			item = fmt.Sprintf("{%s, %s}", sel, sortExpr)
		}

		var b strings.Builder
		b.WriteString("for ")
		b.WriteString(sanitizeName(q.Var))
		b.WriteString(" <- ")
		b.WriteString(src)
		for _, p := range parts {
			b.WriteString(", ")
			b.WriteString(p)
		}
		if cond != "" {
			b.WriteString(", ")
			b.WriteString(cond)
		}
		b.WriteString(", do: ")
		b.WriteString(item)

		expr := b.String()

		if sortExpr != "" {
			expr = fmt.Sprintf("Enum.sort_by((%s), fn {_, k} -> k end)", expr)
		}
		if skipExpr != "" {
			expr = fmt.Sprintf("Enum.drop(%s, %s)", expr, skipExpr)
		}
		if takeExpr != "" {
			expr = fmt.Sprintf("Enum.take(%s, %s)", expr, takeExpr)
		}
		if sortExpr != "" {
			expr = fmt.Sprintf("Enum.map(%s, fn {v, _} -> v end)", expr)
		}

		return expr, nil
	}

	items := src
	if sortExpr != "" {
		items = fmt.Sprintf("Enum.sort_by(%s, fn %s -> %s end)", items, q.Var, sortExpr)
	}
	if skipExpr != "" {
		items = fmt.Sprintf("Enum.drop(%s, %s)", items, skipExpr)
	}
	if takeExpr != "" {
		items = fmt.Sprintf("Enum.take(%s, %s)", items, takeExpr)
	}

	var b strings.Builder
	b.WriteString("for ")
	b.WriteString(sanitizeName(q.Var))
	b.WriteString(" <- ")
	b.WriteString(items)
	if cond != "" {
		b.WriteString(", ")
		b.WriteString(cond)
	}
	b.WriteString(", do: ")
	b.WriteString(sel)
	expr := b.String()
	if agg != "" {
		expr = fmt.Sprintf("%s(%s)", agg, expr)
	}
	return expr, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "nil"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_load")
	c.use("_parse_csv")
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "nil"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_save")
	c.use("_to_map_list")
	c.use("_to_csv")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	withStr := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		withStr = v
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, withStr), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	tmp := c.newTemp()
	var b strings.Builder
	b.WriteString("(fn ->\n")
	b.WriteString("\t" + tmp + " = " + target + "\n")
	b.WriteString("\tcase " + tmp + " do\n")
	hasDefault := false
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\t\t_ -> " + res + "\n")
			hasDefault = true
			break
		}
		pat, err := c.compilePattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf("\t\t%s -> %s\n", pat, res))
	}
	if !hasDefault {
		b.WriteString("\t\t_ -> nil\n")
	}
	b.WriteString("\tend\nend).()")
	return b.String(), nil
}

func (c *Compiler) compilePattern(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return c.compileExpr(e)
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return c.compileExpr(e)
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return c.compileExpr(e)
	}
	t := p.Target
	switch {
	case t.Struct != nil:
		parts := make([]string, len(t.Struct.Fields))
		for i, f := range t.Struct.Fields {
			v, err := c.compilePattern(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		if c.env != nil {
			name := t.Struct.Name
			if st, ok := c.env.GetStruct(name); ok {
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(parts, ", ")), nil
			}
			if st, ok := c.env.GetStruct(sanitizeName(name)); ok {
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(parts, ", ")), nil
			}
		}
		return "%{" + strings.Join(parts, ", ") + "}", nil
	case t.Call != nil:
		if c.env != nil {
			name := t.Call.Func
			if ut, ok := c.env.FindUnionByVariant(name); ok {
				st := ut.Variants[t.Call.Func]
				if len(t.Call.Args) != len(st.Order) {
					return "", fmt.Errorf("variant %s expects %d args", t.Call.Func, len(st.Order))
				}
				args := make([]string, len(st.Order))
				for i, a := range t.Call.Args {
					v, err := c.compilePattern(a)
					if err != nil {
						return "", err
					}
					args[i] = fmt.Sprintf("%s: %s", sanitizeName(st.Order[i]), v)
				}
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(args, ", ")), nil
			}
			if st, ok := c.env.GetStruct(name); ok {
				if len(t.Call.Args) != len(st.Order) {
					return "", fmt.Errorf("struct %s expects %d args", t.Call.Func, len(st.Order))
				}
				args := make([]string, len(st.Order))
				for i, a := range t.Call.Args {
					v, err := c.compilePattern(a)
					if err != nil {
						return "", err
					}
					args[i] = fmt.Sprintf("%s: %s", sanitizeName(st.Order[i]), v)
				}
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(args, ", ")), nil
			}
			if st, ok := c.env.GetStruct(sanitizeName(name)); ok {
				if len(t.Call.Args) != len(st.Order) {
					return "", fmt.Errorf("struct %s expects %d args", name, len(st.Order))
				}
				args := make([]string, len(st.Order))
				for i, a := range t.Call.Args {
					v, err := c.compilePattern(a)
					if err != nil {
						return "", err
					}
					args[i] = fmt.Sprintf("%s: %s", sanitizeName(st.Order[i]), v)
				}
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{%s}", sanitizeName(st.Name), strings.Join(args, ", ")), nil
			}
		}
		parts := make([]string, len(t.Call.Args))
		for i, a := range t.Call.Args {
			v, err := c.compilePattern(a)
			if err != nil {
				return "", err
			}
			parts[i] = v
		}
		return fmt.Sprintf("%s(%s)", t.Call.Func, strings.Join(parts, ", ")), nil
	case t.Selector != nil && len(t.Selector.Tail) == 0:
		name := sanitizeName(t.Selector.Root)
		if c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(t.Selector.Root); ok {
				st := ut.Variants[t.Selector.Root]
				c.ensureStruct(st)
				if len(st.Order) == 0 {
					return fmt.Sprintf("%%%s{}", sanitizeName(st.Name)), nil
				}
			}
			if st, ok := c.env.GetStruct(t.Selector.Root); ok && len(st.Order) == 0 {
				c.ensureStruct(st)
				return fmt.Sprintf("%%%s{}", sanitizeName(st.Name)), nil
			}
			if _, ok := c.attrs[t.Selector.Root]; ok {
				return "@" + name, nil
			}
		}
		return name, nil
	case t.Lit != nil:
		return c.compileExpr(e)
	case t.Group != nil:
		return c.compilePattern(t.Group)
	default:
		return c.compileExpr(e)
	}
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "nil"
	}
	var b strings.Builder
	b.WriteString("(fn ->\n")
	b.WriteString("\tif " + cond + " do\n")
	b.WriteString("\t\t" + thenExpr + "\n")
	if elseExpr != "" {
		b.WriteString("\telse\n")
		b.WriteString("\t\t" + elseExpr + "\n")
	}
	b.WriteString("\tend\nend).()")
	return b.String(), nil
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
			params = append(params, fmt.Sprintf("%s: %s", f.Name, v))
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
		paramStr = "%{" + strings.Join(params, ", ") + "}"
	}
	if model == "" {
		model = "\"\""
	}
	switch g.Target {
	case "embedding":
		c.use("_gen_embed")
		return fmt.Sprintf("_gen_embed(%s, %s, %s)", text, model, paramStr), nil
	default:
		if c.env != nil {
			if _, ok := c.env.GetStruct(g.Target); ok {
				c.use("_gen_struct")
				return fmt.Sprintf("_gen_struct(%s, %s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
			}
		}
		c.use("_gen_text")
		return fmt.Sprintf("_gen_text(%s, %s, %s)", prompt, model, paramStr), nil
	}
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) ensureStruct(st types.StructType) {
	if _, ok := c.structs[st.Name]; ok {
		return
	}
	c.structs[st.Name] = st
	var b strings.Builder
	b.WriteString(fmt.Sprintf("defmodule %s do\n", sanitizeName(st.Name)))
	if len(st.Order) == 0 {
		b.WriteString("  defstruct []\n")
	} else {
		fields := make([]string, len(st.Order))
		for i, f := range st.Order {
			fields[i] = fmt.Sprintf("%s: nil", sanitizeName(f))
		}
		b.WriteString("  defstruct " + strings.Join(fields, ", ") + "\n")
	}
	b.WriteString("end\n\n")
	c.structDefs = append(c.structDefs, b.String())
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
		c.writeln(helperMap[n])
	}
}
