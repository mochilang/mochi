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
	needGet       bool
	needIO        bool
	needFetch     bool
	needGenText   bool
	needGenEmbed  bool
	needGenStruct bool
	needSetOps    bool
	vars          map[string]string
	counts        map[string]int
	tmpCount      int
	packages      map[string]bool
	tests         []testInfo
}

type testInfo struct {
	name  string
	label string
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

// New returns a new Compiler.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: map[string]string{}, counts: map[string]int{}, packages: map[string]bool{}, tmpCount: 0, tests: []testInfo{}}
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
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			exports = append(exports, fmt.Sprintf("%s/0", atomName(name)))
		}
	}
	c.writeln("-export([" + strings.Join(exports, ", ") + "]).")
	c.writeln("")

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
		if s.Fun == nil && s.Test == nil {
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
			c.buf.WriteString(fmt.Sprintf("mochi_run_test(\"%s\", fun %s/0)", t.label, atomName(t.name)))
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

	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
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
		child.SetVar(p.Name, types.AnyType{}, true)
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
	name := "test_" + sanitizeName(t.Name)
	c.tests = append(c.tests, testInfo{name: name, label: t.Name})
	c.writeln(fmt.Sprintf("%s() ->", name))
	c.indent++
	if err := c.compileBlock(t.Body, true, nil); err != nil {
		return err
	}
	c.indent--
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
			c.env.SetVar(s.Let.Name, types.AnyType{}, false)
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
			c.env.SetVar(s.Var.Name, types.AnyType{}, true)
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
	case s.Break != nil:
		c.buf.WriteString("throw(mochi_break)")
		return nil
	case s.Continue != nil:
		c.buf.WriteString("throw(mochi_continue)")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
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
		c.buf.WriteString("mochi_expect(" + expr + ")")
		return nil
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		// extern declarations are ignored
		return nil
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
	} else {
		src, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		list = src
	}
	prevName, hasPrev := c.vars[stmt.Name]
	iter := c.newName(stmt.Name)
	c.buf.WriteString(fmt.Sprintf("mochi_foreach(fun(%s) ->\n", iter))
	child := types.NewEnv(c.env)
	child.SetVar(stmt.Name, types.AnyType{}, true)
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

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	name := c.newName(a.Name)
	c.buf.WriteString(fmt.Sprintf("%s = %s", name, val))
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
	operands = append(operands, operand{expr: first, typ: c.inferUnaryType(b.Left)})

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		op := part.Op
		if part.All {
			op = op + "_all"
		}
		operands = append(operands, operand{expr: r, typ: c.inferPostfixType(part.Right)})
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
				expr = fmt.Sprintf("(%s %s %s)", l.expr, op, r.expr)
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
				expr = fmt.Sprintf("maps:is_key(%s, %s)", l.expr, r.expr)
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
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	res, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}

	typ := c.inferPrimaryType(p.Target)

	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}

			switch tt := typ.(type) {
			case types.MapType:
				res = fmt.Sprintf("maps:get(%s, %s)", idx, res)
				typ = tt.Value
			case types.ListType:
				res = fmt.Sprintf("lists:nth(%s + 1, %s)", idx, res)
				typ = tt.Elem
			case types.StringType:
				res = fmt.Sprintf("lists:nth(%s + 1, %s)", idx, res)
				typ = types.StringType{}
			default:
				if isStringExpr(op.Index.Start, c.env) {
					c.needGet = true
					res = fmt.Sprintf("mochi_get(%s, %s)", res, idx)
				} else {
					res = fmt.Sprintf("lists:nth(%s + 1, %s)", idx, res)
				}
				typ = types.AnyType{}
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
			switch res {
			case "print":
				res = fmt.Sprintf("mochi_print([%s])", argStr)
			case "len":
				res = fmt.Sprintf("length(%s)", argStr)
			case "str":
				res = fmt.Sprintf("mochi_format(%s)", argStr)
			case "count":
				res = fmt.Sprintf("mochi_count(%s)", argStr)
			case "avg":
				res = fmt.Sprintf("mochi_avg(%s)", argStr)
			case "input":
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
			// ignore type casts
			typ = c.resolveTypeRef(op.Cast.Type)
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
				parts := make([]string, 0, len(p.Struct.Fields)+1)
				parts = append(parts, fmt.Sprintf("'__name' => %s", atomName(p.Struct.Name)))
				vals := map[string]string{}
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					vals[f.Name] = v
				}
				for _, n := range st.Order {
					if v, ok := vals[n]; ok {
						parts = append(parts, fmt.Sprintf("%s => %s", n, v))
					}
				}
				return "#{" + strings.Join(parts, ", ") + "}", nil
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
			return fmt.Sprintf("mochi_print([%s])", argStr), nil
		case "len":
			return fmt.Sprintf("length(%s)", argStr), nil
		case "str":
			return fmt.Sprintf("mochi_format(%s)", argStr), nil
		case "count":
			return fmt.Sprintf("mochi_count(%s)", argStr), nil
		case "avg":
			return fmt.Sprintf("mochi_avg(%s)", argStr), nil
		case "input":
			return "mochi_input()", nil
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
	if len(q.Joins) > 0 || q.Group != nil {
		return "", fmt.Errorf("unsupported query expression")
	}
	if len(q.Froms) > 0 && (q.Sort != nil || q.Skip != nil || q.Take != nil) {
		return "", fmt.Errorf("unsupported query expression")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	orig := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, f := range q.Froms {
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	c.env = child
	sel, err := c.compileExpr(q.Select)
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

	if sortExpr == "" && skipExpr == "" && takeExpr == "" {
		var b strings.Builder
		b.WriteString("[")
		b.WriteString(sel)
		b.WriteString(" || ")
		b.WriteString(capitalize(q.Var))
		b.WriteString(" <- ")
		b.WriteString(src)
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
		if cond != "" {
			b.WriteString(", ")
			b.WriteString(cond)
		}
		b.WriteString("]")
		return b.String(), nil
	}

	var b strings.Builder
	b.WriteString("(fun() ->\n")
	b.WriteString("\tItems = [" + capitalize(q.Var) + " || " + capitalize(q.Var) + " <- " + src)
	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(", " + capitalize(f.Var) + " <- " + fs)
	}
	if cond != "" {
		b.WriteString(", " + cond)
	}
	b.WriteString("],\n")
	b.WriteString("\tSorted = ")
	if sortExpr != "" {
		b.WriteString("begin\n")
		b.WriteString("\t\tPairs = [{" + sortExpr + ", " + capitalize(q.Var) + "} || " + capitalize(q.Var) + " <- Items],\n")
		b.WriteString("\t\tSPairs = lists:sort(fun({A,_},{B,_}) -> A =< B end, Pairs),\n")
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
	b.WriteString("\t[" + sel + " || " + capitalize(q.Var) + " <- Taken]\n")
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
			parts := []string{fmt.Sprintf("'__name' := %s", atomName(call.Func))}
			for idx, arg := range call.Args {
				if id, ok := identName(arg); ok {
					if id == "_" {
						parts = append(parts, fmt.Sprintf("%s := _", st.Order[idx]))
					} else {
						parts = append(parts, fmt.Sprintf("%s := %s", st.Order[idx], capitalize(id)))
					}
				} else {
					return "", fmt.Errorf("unsupported pattern")
				}
			}
			return "#{" + strings.Join(parts, ", ") + "}", nil
		}
	}
	if ident, ok := identName(e); ok {
		if _, ok := c.env.FindUnionByVariant(ident); ok {
			return fmt.Sprintf("#{'__name' := %s}", atomName(ident)), nil
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

func (c *Compiler) emitRuntime() {
	c.writeln("mochi_print(Args) ->")
	c.indent++
	c.writeln("Strs = [ mochi_format(A) || A <- Args ],")
	c.writeln("io:format(\"~s~n\", [lists:flatten(Strs)]).")
	c.indent--
	c.writeln("")

	c.writeln("mochi_format(X) when is_integer(X) -> integer_to_list(X);")
	c.writeln("mochi_format(X) when is_float(X) -> float_to_list(X);")
	c.writeln("mochi_format(X) when is_list(X) -> X;")
	c.writeln("mochi_format(X) -> lists:flatten(io_lib:format(\"~p\", [X])).")

	c.writeln("")
	c.writeln("mochi_count(X) when is_list(X) -> length(X);")
	c.writeln("mochi_count(X) when is_map(X) -> maps:size(X);")
	c.writeln("mochi_count(X) when is_binary(X) -> byte_size(X);")
	c.writeln("mochi_count(_) -> erlang:error(badarg).")

	c.writeln("")
	c.writeln("mochi_input() ->")
	c.indent++
	c.writeln("case io:get_line(\"\") of")
	c.indent++
	c.writeln("eof -> \"\";")
	c.writeln("Line -> string:trim(Line)")
	c.indent--
	c.writeln("end.")
	c.indent--

	c.writeln("")
	c.writeln("mochi_avg([]) -> 0;")
	c.writeln("mochi_avg(L) when is_list(L) ->")
	c.indent++
	c.writeln("Sum = lists:foldl(fun(X, Acc) ->")
	c.indent++
	c.writeln("case X of")
	c.indent++
	c.writeln("I when is_integer(I) -> Acc + I;")
	c.writeln("F when is_float(F) -> Acc + F;")
	c.writeln("_ -> erlang:error(badarg) end")
	c.indent--
	c.writeln("end, 0, L),")
	c.writeln("Sum / length(L);")
	c.indent--
	c.writeln("mochi_avg(_) -> erlang:error(badarg).")

	c.writeln("")
	c.writeln("mochi_foreach(F, L) ->")
	c.indent++
	c.writeln("try mochi_foreach_loop(F, L) catch throw:mochi_break -> ok end.")
	c.indent--

	c.writeln("")
	c.writeln("mochi_foreach_loop(_, []) -> ok;")
	c.writeln("mochi_foreach_loop(F, [H|T]) ->")
	c.indent++
	c.writeln("try F(H) catch")
	c.indent++
	c.writeln("throw:mochi_continue -> ok;")
	c.writeln("throw:mochi_break -> throw(mochi_break)")
	c.indent--
	c.writeln("end,")
	c.writeln("mochi_foreach_loop(F, T).")
	c.indent--

	if c.needGet {
		c.writeln("")
		c.writeln("mochi_get(M, K) when is_list(M), is_integer(K) -> lists:nth(K + 1, M);")
		c.writeln("mochi_get(M, K) when is_map(M) -> maps:get(K, M);")
		c.writeln("mochi_get(_, _) -> erlang:error(badarg).")
	}

	if c.needIO {
		c.writeln("")
		c.writeln("mochi_load(Path, _Opts) ->")
		c.indent++
		c.writeln("case file:read_file(Path) of")
		c.indent++
		c.writeln("{ok, Bin} -> binary_to_term(Bin);")
		c.writeln("_ -> []")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_save(Data, Path, _Opts) ->")
		c.indent++
		c.writeln("ok = file:write_file(Path, term_to_binary(Data)).")
		c.indent--
	}

	if c.needFetch {
		c.writeln("")
		c.writeln("mochi_fetch(_Url, _Opts) -> [].")
	}

	if c.needGenText {
		c.writeln("")
		c.writeln("mochi_gen_text(Prompt, _Model, _Params) -> Prompt.")
	}

	if c.needGenEmbed {
		c.writeln("")
		c.writeln("mochi_gen_embed(Text, _Model, _Params) -> [ float(C) || <<C>> <= Text ].")
	}

	if c.needGenStruct {
		c.writeln("")
		c.writeln("mochi_gen_struct(_Mod, _Prompt, _Model, _Params) -> #{}.")
	}

	if c.needSetOps {
		c.writeln("")
		c.writeln("mochi_union(A, B) -> sets:to_list(sets:union(sets:from_list(A), sets:from_list(B))).")
		c.writeln("")
		c.writeln("mochi_except(A, B) -> sets:to_list(sets:subtract(sets:from_list(A), sets:from_list(B))).")
		c.writeln("")
		c.writeln("mochi_intersect(A, B) -> sets:to_list(sets:intersection(sets:from_list(A), sets:from_list(B))).")
	}

	c.writeln("")
	c.writeln("mochi_while(Cond, Body) ->")
	c.indent++
	c.writeln("case Cond() of")
	c.indent++
	c.writeln("true ->")
	c.indent++
	c.writeln("try Body() catch")
	c.indent++
	c.writeln("throw:mochi_continue -> ok;")
	c.writeln("throw:mochi_break -> ok")
	c.indent--
	c.writeln("end,")
	c.writeln("mochi_while(Cond, Body);")
	c.indent--
	c.writeln("_ -> ok")
	c.indent--
	c.writeln("end.")
	c.indent--

	c.writeln("")
	c.writeln("mochi_expect(true) -> ok;")
	c.writeln("mochi_expect(_) -> erlang:error(expect_failed).")

	c.writeln("")
	c.writeln("mochi_test_start(Name) -> io:format(\"   test ~s ...\", [Name]).")
	c.writeln("mochi_test_pass(Dur) -> io:format(\" ok (~p)~n\", [Dur]).")
	c.writeln("mochi_test_fail(Err, Dur) -> io:format(\" fail ~p (~p)~n\", [Err, Dur]).")

	c.writeln("")
	c.writeln("mochi_run_test(Name, Fun) ->")
	c.indent++
	c.writeln("mochi_test_start(Name),")
	c.writeln("Start = erlang:monotonic_time(millisecond),")
	c.writeln("try Fun() of _ ->")
	c.indent++
	c.writeln("Duration = erlang:monotonic_time(millisecond) - Start,")
	c.writeln("mochi_test_pass(Duration)")
	c.indent--
	c.writeln("catch C:R ->")
	c.indent++
	c.writeln("Duration = erlang:monotonic_time(millisecond) - Start,")
	c.writeln("mochi_test_fail({C,R}, Duration)")
	c.indent--
	c.writeln("end.")
	c.indent--
}
