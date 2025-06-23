package phpcode

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

const funcPrefix = "mochi_"

// Compiler translates a Mochi AST into PHP source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
	locals map[string]bool
	funcs  map[string]bool
}

// New creates a new PHP compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, locals: map[string]bool{}, funcs: map[string]bool{}}
}

// Compile generates PHP code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("<?php")

	c.funcs = map[string]bool{}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			c.funcs[s.Fun.Name] = true
		}
	}

	// functions first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// test blocks
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// main body
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}

	// run tests
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := funcPrefix + "test_" + sanitizeName(s.Test.Name)
			c.writeln(name + "();")
		}
	}
	return c.buf.Bytes(), nil
}

// --- Statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Fun != nil:
		fn := s.Fun
		params := make([]string, len(fn.Params))
		for i, p := range fn.Params {
			params[i] = "$" + sanitizeName(p.Name)
		}
		fe := &parser.FunExpr{Params: fn.Params, BlockBody: fn.Body}
		captured := freeVars(fe, params)
		captured = append(captured, "$"+sanitizeName(fn.Name))
		oldLocals := c.locals
		c.locals = map[string]bool{}
		for _, p := range fn.Params {
			c.locals[p.Name] = true
		}
		// allow recursive calls
		c.locals[fn.Name] = true
		name := sanitizeName(fn.Name)
		c.writeln(fmt.Sprintf("$%s = null;", name))
		use := ""
		if len(captured) > 0 {
			for i, v := range captured {
				captured[i] = "&" + v
			}
			use = " use (" + strings.Join(captured, ", ") + ")"
		}
		c.writeln(fmt.Sprintf("$%s = function (%s)%s {", name, strings.Join(params, ", "), use))
		c.indent++
		for _, st := range fn.Body {
			if err := c.compileStmt(st); err != nil {
				c.locals = oldLocals
				return err
			}
		}
		c.indent--
		c.writeln("};")
		c.locals = oldLocals
		c.locals[fn.Name] = true
		return nil
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr + ";")
		}
		return nil
	default:
		return nil
	}
}
func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	muts := mutatedVars(fn.Body)
	for i, p := range fn.Params {
		name := "$" + sanitizeName(p.Name)
		if muts[p.Name] {
			if isCompositeParam(p) {
				name = "&" + name
			}
		}
		params[i] = name
	}
	c.writeln(fmt.Sprintf("function %s%s(%s) {", funcPrefix, sanitizeName(fn.Name), strings.Join(params, ", ")))
	oldLocals := c.locals
	c.locals = map[string]bool{}
	for _, p := range fn.Params {
		c.locals[p.Name] = true
	}
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.locals = oldLocals
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.locals = oldLocals
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := funcPrefix + "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("function %s() {", name))
	oldLocals := c.locals
	c.locals = map[string]bool{}
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			c.locals = oldLocals
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.locals = oldLocals
	return nil
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	name := "$" + sanitizeName(l.Name)
	val := "null"
	if l.Value != nil {
		v, err := c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln(fmt.Sprintf("%s = %s;", name, val))
	c.locals[l.Name] = true
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (!(%s)) { throw new Exception('expect failed'); }", expr))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	name := "$" + sanitizeName(v.Name)
	val := "null"
	if v.Value != nil {
		valExpr, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = valExpr
	}
	c.writeln(fmt.Sprintf("%s = %s;", name, val))
	c.locals[v.Name] = true
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	lhs := "$" + sanitizeName(a.Name)
	for _, idx := range a.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := "$" + sanitizeName(f.Name)
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (%s = %s; %s < %s; %s++) {", name, start, name, end, name))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("foreach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as %s) {", src, name))
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if s.ElseIf != nil {
		c.writeln("} else ")
		return c.compileIf(s.ElseIf)
	}
	if len(s.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	} else {
		c.writeln("}")
	}
	return nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", nil
	}
	operands := []string{}
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, left)
	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
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
			if op == "in" {
				expr = fmt.Sprintf("(is_array(%[2]s) ? (array_key_exists(%[1]s, %[2]s) || in_array(%[1]s, %[2]s, true)) : (is_string(%[2]s) ? strpos(%[2]s, strval(%[1]s)) !== false : false))", l, r)
			} else if op == "+" {
				expr = fmt.Sprintf("((is_array(%[1]s) && is_array(%[2]s)) ? array_merge(%[1]s, %[2]s) : ((is_string(%[1]s) || is_string(%[2]s)) ? (%[1]s . %[2]s) : (%[1]s + %[2]s)))", l, r)
			} else if op == "/" {
				expr = fmt.Sprintf("((is_int(%[1]s) && is_int(%[2]s)) ? intdiv(%[1]s, %[2]s) : (%[1]s / %[2]s))", l, r)
			} else if op == "%" {
				expr = fmt.Sprintf("((is_int(%[1]s) && is_int(%[2]s)) ? (%[1]s %% %[2]s) : fmod(%[1]s, %[2]s))", l, r)
			} else if op == "union_all" {
				expr = fmt.Sprintf("array_merge(%s, %s)", l, r)
			} else if op == "union" {
				expr = fmt.Sprintf("array_values(array_unique(array_merge(%s, %s), SORT_REGULAR))", l, r)
			} else if op == "except" {
				expr = fmt.Sprintf("array_values(array_diff(%s, %s))", l, r)
			} else if op == "intersect" {
				expr = fmt.Sprintf("array_values(array_intersect(%s, %s))", l, r)
			} else {
				expr = fmt.Sprintf("(%s %s %s)", l, op, r)
			}
			operands[i] = expr
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state in binary expr")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if u == nil {
		return "", nil
	}
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("%s%s", u.Ops[i], val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil {
				key, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s[%s]", expr, key)
			} else {
				start := "0"
				if idx.Start != nil {
					s, err := c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("(is_array(%s) ? count(%s) : strlen(%s))", expr, expr, expr)
				if idx.End != nil {
					e, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				length := fmt.Sprintf("(%s) - (%s)", end, start)
				expr = fmt.Sprintf("(is_array(%[1]s) ? array_slice(%[1]s, %[2]s, %[3]s) : substr(%[1]s, %[2]s, %[3]s))", expr, start, length)
			}
		} else if op.Call != nil {
			call, err := c.compileCallOp(expr, op.Call)
			if err != nil {
				return "", err
			}
			expr = call
		}
	}
	return expr, nil
}

func (c *Compiler) compileCallOp(receiver string, call *parser.CallOp) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", receiver, strings.Join(args, ", ")), nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		name := "$" + sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			name += "->" + strings.Join(p.Selector.Tail, "->")
		}
		return name, nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
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
		return c.compileMapLiteral(p.Map)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := call.Func
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch name {
	case "print":
		if len(args) == 0 {
			return "", fmt.Errorf("print expects at least 1 arg")
		}
		joined := strings.Join(args, " . \" \" . ")
		return fmt.Sprintf("echo %s, PHP_EOL", joined), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("(is_array(%[1]s) ? count(%[1]s) : strlen(%[1]s))", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("strval(%s)", args[0]), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input expects no args")
		}
		return "trim(fgets(STDIN))", nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("(is_array(%[1]s) ? count(%[1]s) : strlen(%[1]s))", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(count(%[1]s) ? array_sum(%[1]s) / count(%[1]s) : 0)", args[0]), nil
	default:
		if c.locals[name] {
			return fmt.Sprintf("$%s(%s)", sanitizeName(name), strings.Join(args, ", ")), nil
		}
		if c.funcs[name] {
			return fmt.Sprintf("%s%s(%s)", funcPrefix, sanitizeName(name), strings.Join(args, ", ")), nil
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		var k string
		if s, ok := simpleStringKey(it.Key); ok {
			k = fmt.Sprintf("%q", s)
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
		items[i] = fmt.Sprintf("%s => %s", k, v)
	}
	return "[" + strings.Join(items, ", ") + "]", nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 || q.Group != nil {
		return "", fmt.Errorf("unsupported query expression")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	// Special-case simple sort by variable with no from/where and select same variable
	if len(q.Froms) == 0 && q.Where == nil && q.Sort != nil {
		if sname, ok := identName(q.Sort); ok && sname == q.Var {
			if selname, ok2 := identName(q.Select); ok2 && selname == q.Var {
				return fmt.Sprintf("(function($tmp){ sort($tmp); return $tmp; })(%s)", src), nil
			}
		}
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
	var cond string
	var skipExpr string
	var takeExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
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
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		fromSrcs[i] = fs
	}
	capture := queryFreeVars(q)
	c.env = orig

	var b strings.Builder
	use := ""
	if len(capture) > 0 {
		use = " use (" + strings.Join(capture, ", ") + ")"
	}
	b.WriteString("(function()" + use + " {\n")
	b.WriteString("\t$res = [];\n")
	b.WriteString(fmt.Sprintf("\tforeach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as $%s) {\n", src, sanitizeName(q.Var)))
	indent := "\t\t"
	for i, fs := range fromSrcs {
		b.WriteString(fmt.Sprintf(indent+"foreach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as $%s) {\n", fs, sanitizeName(q.Froms[i].Var)))
		indent += "\t"
	}
	if cond != "" {
		b.WriteString(indent + "if (" + cond + ") {\n")
		indent += "\t"
	}
	b.WriteString(indent + "$res[] = " + sel + ";\n")
	if cond != "" {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for range fromSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("\t}\n")
	if skipExpr != "" {
		b.WriteString(fmt.Sprintf("\t$res = array_slice($res, %s);\n", skipExpr))
	}
	if takeExpr != "" {
		b.WriteString(fmt.Sprintf("\t$res = array_slice($res, 0, %s);\n", takeExpr))
	}
	b.WriteString("\treturn $res;\n")
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		f := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(f, ".eE") {
			f += ".0"
		}
		return f, nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	}
	return "", fmt.Errorf("unknown literal")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = "$" + sanitizeName(p.Name)
	}
	sub := &Compiler{env: types.NewEnv(c.env)}
	var body bytes.Buffer
	sub.buf = body
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr + ";")
	} else {
		for _, st := range fn.BlockBody {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}
	captured := freeVars(fn, params)
	use := ""
	if len(captured) > 0 {
		use = " use (" + strings.Join(captured, ", ") + ")"
	}
	bodyStr := indentBlock(sub.buf.String(), 1)
	return fmt.Sprintf("function (%s)%s {\n%s}", strings.Join(params, ", "), use, bodyStr), nil
}

func indentBlock(s string, indent int) string {
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	pref := strings.Repeat("\t", indent)
	for i, l := range lines {
		lines[i] = pref + l
	}
	return strings.Join(lines, "\n") + "\n"
}

func freeVars(fn *parser.FunExpr, params []string) []string {
	vars := map[string]struct{}{}
	scanExpr(fn.ExprBody, vars)
	for _, st := range fn.BlockBody {
		scanStmt(st, vars)
	}
	m := make(map[string]struct{})
	for v := range vars {
		skip := false
		for _, p := range params {
			if p == "$"+sanitizeName(v) {
				skip = true
				break
			}
		}
		if !skip {
			m["$"+sanitizeName(v)] = struct{}{}
		}
	}
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func scanStmt(s *parser.Statement, vars map[string]struct{}) {
	switch {
	case s.Let != nil:
		scanExpr(s.Let.Value, vars)
	case s.Var != nil:
		scanExpr(s.Var.Value, vars)
	case s.Assign != nil:
		scanExpr(s.Assign.Value, vars)
	case s.Return != nil:
		scanExpr(s.Return.Value, vars)
	case s.Expr != nil:
		scanExpr(s.Expr.Expr, vars)
	case s.For != nil:
		scanExpr(s.For.Source, vars)
		scanExpr(s.For.RangeEnd, vars)
		for _, st := range s.For.Body {
			scanStmt(st, vars)
		}
	case s.While != nil:
		scanExpr(s.While.Cond, vars)
		for _, st := range s.While.Body {
			scanStmt(st, vars)
		}
	case s.If != nil:
		scanExpr(s.If.Cond, vars)
		for _, st := range s.If.Then {
			scanStmt(st, vars)
		}
		if s.If.ElseIf != nil {
			scanStmt(&parser.Statement{If: s.If.ElseIf}, vars)
		}
		for _, st := range s.If.Else {
			scanStmt(st, vars)
		}
	}
}

func scanExpr(e *parser.Expr, vars map[string]struct{}) {
	if e == nil {
		return
	}
	scanUnary(e.Binary.Left, vars)
	for _, op := range e.Binary.Right {
		scanPostfix(op.Right, vars)
	}
}

func scanUnary(u *parser.Unary, vars map[string]struct{}) {
	if u == nil {
		return
	}
	scanPostfix(u.Value, vars)
}

func scanPostfix(p *parser.PostfixExpr, vars map[string]struct{}) {
	if p == nil {
		return
	}
	scanPrimary(p.Target, vars)
	for _, op := range p.Ops {
		if op.Index != nil {
			scanExpr(op.Index.Start, vars)
			scanExpr(op.Index.End, vars)
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				scanExpr(a, vars)
			}
		}
	}
}

func queryFreeVars(q *parser.QueryExpr) []string {
	vars := map[string]struct{}{}
	scanExpr(q.Source, vars)
	for _, f := range q.Froms {
		scanExpr(f.Src, vars)
	}
	for _, j := range q.Joins {
		scanExpr(j.Src, vars)
		scanExpr(j.On, vars)
	}
	scanExpr(q.Select, vars)
	scanExpr(q.Where, vars)
	scanExpr(q.Sort, vars)
	scanExpr(q.Skip, vars)
	scanExpr(q.Take, vars)
	delete(vars, q.Var)
	for _, f := range q.Froms {
		delete(vars, f.Var)
	}
	outMap := map[string]struct{}{}
	for k := range vars {
		outMap["$"+sanitizeName(k)] = struct{}{}
	}
	out := make([]string, 0, len(outMap))
	for k := range outMap {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func scanPrimary(p *parser.Primary, vars map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		vars[p.Selector.Root] = struct{}{}
	}
	if p.Group != nil {
		scanExpr(p.Group, vars)
	}
	if p.FunExpr != nil {
		scanExpr(p.FunExpr.ExprBody, vars)
		for _, st := range p.FunExpr.BlockBody {
			scanStmt(st, vars)
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			scanExpr(e, vars)
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			scanExpr(it.Key, vars)
			scanExpr(it.Value, vars)
		}
	}
	if p.Call != nil {
		for _, a := range p.Call.Args {
			scanExpr(a, vars)
		}
	}
}

func mutatedVars(stmts []*parser.Statement) map[string]bool {
	vars := map[string]bool{}
	var walk func(*parser.Statement)
	walk = func(s *parser.Statement) {
		switch {
		case s.Assign != nil:
			vars[s.Assign.Name] = true
		case s.For != nil:
			for _, st := range s.For.Body {
				walk(st)
			}
		case s.While != nil:
			for _, st := range s.While.Body {
				walk(st)
			}
		case s.If != nil:
			for _, st := range s.If.Then {
				walk(st)
			}
			if s.If.ElseIf != nil {
				walk(&parser.Statement{If: s.If.ElseIf})
			}
			for _, st := range s.If.Else {
				walk(st)
			}
		}
	}
	for _, st := range stmts {
		walk(st)
	}
	return vars
}

func isComposite(t types.Type) bool {
	switch t.(type) {
	case types.ListType, types.MapType, types.StructType, types.UnionType:
		return true
	default:
		return false
	}
}

func isCompositeParam(p *parser.Param) bool {
	if p == nil || p.Type == nil {
		return false
	}
	if p.Type.Generic != nil {
		switch p.Type.Generic.Name {
		case "list", "map":
			return true
		}
	}
	return false
}
