package luacode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Lua source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env

	loopLabels []string
	labelCount int

	tmpCount int

	helpers map[string]bool
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), tmpCount: 0}
}

// Compile returns Lua source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// reset state so the compiler can be reused
	c.buf.Reset()
	c.indent = 0
	c.loopLabels = nil
	c.labelCount = 0
	c.tmpCount = 0
	c.helpers = make(map[string]bool)

	// Emit function declarations first.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun, false); err != nil {
				return nil, err
			}
			c.writeln("")
		}
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

	// Emit main body.
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}

	// Invoke test blocks.
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(name + "()")
		}
	}

	bodyBytes := append([]byte(nil), c.buf.Bytes()...)
	c.buf.Reset()
	c.emitRuntime()
	c.buf.Write(bodyBytes)

	code := c.buf.Bytes()
	if err := checkLuaSyntax(code); err != nil {
		return nil, err
	}

	return code, nil
}

// --- Statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
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
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
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
	}
	c.writeln(fmt.Sprintf("local %s = %s", name, val))
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	// treat same as let
	return c.compileLet(&parser.LetStmt{Name: s.Name, Value: s.Value})
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)

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
	label := c.pushLoopLabel()
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
			if useVar {
				c.writeln(fmt.Sprintf("for %s in pairs(%s) do", name, src))
			} else {
				c.writeln(fmt.Sprintf("for _ in pairs(%s) do", src))
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
			c.use("iter")
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
			return err
		}
	}
	c.writeln("::" + label + "::")
	c.indent--
	c.writeln("end")
	c.popLoopLabel()
	return nil
}

func (c *Compiler) compileWhile(s *parser.WhileStmt) error {
	label := c.pushLoopLabel()
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " do")
	c.indent++
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("::" + label + "::")
	c.indent--
	c.writeln("end")
	c.popLoopLabel()
	return nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}

	operands := []string{left}
	ops := []string{}
	for _, part := range b.Right {
		right, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		ops = append(ops, part.Op)
		operands = append(operands, right)
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}}
	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				opstr := ops[i]
				var expr string
				switch opstr {
				case "&&":
					expr = fmt.Sprintf("(%s and %s)", l, r)
				case "||":
					expr = fmt.Sprintf("(%s or %s)", l, r)
				case "in":
					c.use("contains")
					expr = fmt.Sprintf("__contains(%s, %s)", r, l)
				case "/":
					c.use("div")
					expr = fmt.Sprintf("__div(%s, %s)", l, r)
				case "+":
					c.use("add")
					expr = fmt.Sprintf("__add(%s, %s)", l, r)
				case "==":
					c.use("eq")
					expr = fmt.Sprintf("__eq(%s, %s)", l, r)
				case "!=":
					c.use("eq")
					expr = fmt.Sprintf("not __eq(%s, %s)", l, r)
				default:
					expr = fmt.Sprintf("(%s %s %s)", l, opstr, r)
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return "", fmt.Errorf("invalid expression")
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
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := "nil"
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				c.use("slice")
				expr = fmt.Sprintf("__slice(%s, %s, %s)", expr, start, end)
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if isStringLiteral(op.Index.Start) {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				} else {
					c.use("index")
					c.use("indexString")
					expr = fmt.Sprintf("__index(%s, %s)", expr, idx)
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
		return "{" + strings.Join(elems, ", ") + "}", nil
	case p.Map != nil:
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
			pairs[i] = fmt.Sprintf("[%s]=%s", k, v)
		}
		return "{" + strings.Join(pairs, ", ") + "}", nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.Struct != nil:
		if c.env != nil {
			if _, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
				parts := make([]string, 0, len(p.Struct.Fields)+1)
				parts = append(parts, fmt.Sprintf("__name=%q", p.Struct.Name))
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					parts = append(parts, fmt.Sprintf("%s=%s", sanitizeName(f.Name), v))
				}
				return "{" + strings.Join(parts, ", ") + "}", nil
			}
		}
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s=%s", sanitizeName(f.Name), v)
		}
		return "{" + strings.Join(parts, ", ") + "}", nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	if name == "len" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("#%s", arg), nil
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
	switch name {
	case "print":
		c.use("print")
		return fmt.Sprintf("__print(%s)", argStr), nil
	case "str":
		if len(args) == 1 {
			return fmt.Sprintf("tostring(%s)", args[0]), nil
		}
		return fmt.Sprintf("tostring(%s)", argStr), nil
	case "input":
		c.use("input")
		return "__input()", nil
	case "count":
		c.use("count")
		return fmt.Sprintf("__count(%s)", argStr), nil
	case "avg":
		c.use("avg")
		return fmt.Sprintf("__avg(%s)", argStr), nil
	default:
		return fmt.Sprintf("%s(%s)", name, argStr), nil
	}
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil {
		return "", fmt.Errorf("group clause not supported")
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return "", fmt.Errorf("join sides not supported")
		}
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	iter := sanitizeName(q.Var)
	var whereCond, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		whereCond, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}

	var b strings.Builder
	b.WriteString("(function()\n")

	// handle simple joins/cross joins without sort/skip/take
	if (len(q.Froms) > 0 || len(q.Joins) > 0) && sortExpr == "" && skipExpr == "" && takeExpr == "" {
		b.WriteString("\tlocal _res = {}\n")
		b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(%s) do\n", iter, src))
		indent := "\t\t"
		for _, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(f.Var), fs))
			indent += "\t"
		}
		for _, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(j.Var), js))
			indent += "\t"
			on, err := c.compileExpr(j.On)
			if err != nil {
				return "", err
			}
			b.WriteString(fmt.Sprintf(indent+"if %s then\n", on))
			indent += "\t"
		}
		if whereCond != "" {
			b.WriteString(fmt.Sprintf(indent+"if %s then\n", whereCond))
			indent += "\t"
		}
		b.WriteString(fmt.Sprintf(indent+"table.insert(_res, %s)\n", sel))
		if whereCond != "" {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		}
		for i := len(q.Joins) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		}
		for range q.Froms {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		}
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
		b.WriteString("\treturn _res\n")
		b.WriteString("end)()")
		return b.String(), nil
	}

	b.WriteString("\tlocal items = {}\n")
	b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(%s) do\n", iter, src))
	indent := "\t\t"
	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(f.Var), fs))
		indent += "\t"
	}
	for _, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(j.Var), js))
		indent += "\t"
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf(indent+"if %s then\n", on))
		indent += "\t"
	}
	if whereCond != "" {
		b.WriteString(fmt.Sprintf(indent+"if %s then\n", whereCond))
		indent += "\t"
	}
	b.WriteString(fmt.Sprintf(indent+"table.insert(items, %s)\n", iter))
	if whereCond != "" {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
	}
	for i := len(q.Joins) - 1; i >= 0; i-- {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
	}
	for i := len(q.Froms) - 1; i >= 0; i-- {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
	}
	indent = indent[:len(indent)-1]
	b.WriteString(indent + "end\n")
	if sortExpr != "" {
		b.WriteString("\ttable.sort(items, function(a, b)\n")
		b.WriteString(fmt.Sprintf("\t\tlocal %s = a\n", iter))
		b.WriteString(fmt.Sprintf("\t\tlocal _ka = %s\n", sortExpr))
		b.WriteString(fmt.Sprintf("\t\t%s = b\n", iter))
		b.WriteString(fmt.Sprintf("\t\tlocal _kb = %s\n", sortExpr))
		b.WriteString("\t\treturn (_ka) < (_kb)\n")
		b.WriteString("\tend)\n")
	}
	if skipExpr != "" {
		b.WriteString(fmt.Sprintf("\tif %s < #items then\n", skipExpr))
		b.WriteString(fmt.Sprintf("\t\titems = {table.unpack(items, (%s)+1)}\n", skipExpr))
		b.WriteString("\telse\n")
		b.WriteString("\t\titems = {}\n")
		b.WriteString("\tend\n")
	}
	if takeExpr != "" {
		b.WriteString(fmt.Sprintf("\tif %s < #items then\n", takeExpr))
		b.WriteString(fmt.Sprintf("\t\titems = {table.unpack(items, 1, %s)}\n", takeExpr))
		b.WriteString("\tend\n")
	}
	b.WriteString("\tlocal _res = {}\n")
	b.WriteString(fmt.Sprintf("\tfor _, %s in ipairs(items) do\n", iter))
	b.WriteString(fmt.Sprintf("\t\ttable.insert(_res, %s)\n", sel))
	b.WriteString("\tend\n")
	b.WriteString("\treturn _res\n")
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
	b.WriteString("(function()\n")
	b.WriteString("\tlocal " + tmp + " = " + target + "\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + "\n")
			b.WriteString("end)()")
			return b.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("%s.__name == \"%s\"", tmp, call.Func)
				names := []string{}
				values := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						names = append(names, sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						values = append(values, fmt.Sprintf("%s.%s", tmp, field))
					}
				}
				if len(names) > 0 {
					res = fmt.Sprintf("(function(%s) return %s end)(%s)", strings.Join(names, ", "), res, strings.Join(values, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("%s.__name == \"%s\"", tmp, ident)
			}
		}
		if cond == "" {
			pat, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cond = fmt.Sprintf("%s == %s", tmp, pat)
		}
		b.WriteString(fmt.Sprintf("\tif %s then return %s end\n", cond, res))
	}
	b.WriteString("\treturn nil\n")
	b.WriteString("end)()")
	return b.String(), nil
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
			params = append(params, fmt.Sprintf("%q=%s", f.Name, v))
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
		paramStr = "{" + strings.Join(params, ", ") + "}"
	}
	if model == "" {
		model = "nil"
	}
	if g.Target == "embedding" {
		c.use("gen_embed")
		return fmt.Sprintf("__gen_embed(%s, %s, %s)", text, model, paramStr), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("gen_struct")
			return fmt.Sprintf("__gen_struct(%s, %s, %s)", prompt, model, paramStr), nil
		}
	}
	c.use("gen_text")
	return fmt.Sprintf("__gen_text(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	sub := &Compiler{env: c.env, helpers: c.helpers}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		for _, st := range fn.BlockBody {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return "function(" + strings.Join(params, ", ") + ")\n" + body + "end", nil
}

func (c *Compiler) compileLiteral(lit *parser.Literal) (string, error) {
	switch {
	case lit.Int != nil:
		return strconv.Itoa(*lit.Int), nil
	case lit.Float != nil:
		s := strconv.FormatFloat(*lit.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") && !strings.Contains(s, ".") {
			s += ".0"
		}
		return s, nil
	case lit.Str != nil:
		return strconv.Quote(*lit.Str), nil
	case lit.Bool != nil:
		if *lit.Bool {
			return "true", nil
		}
		return "false", nil
	}
	return "nil", fmt.Errorf("unknown literal")
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil || p.Lit == nil || p.Lit.Str == nil {
		return false
	}
	return true
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
	c.use("load")
	return fmt.Sprintf("__load(%s, %s)", path, opts), nil
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
	c.use("save")
	return fmt.Sprintf("__save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("fetch")
	return fmt.Sprintf("__fetch(%s, %s)", url, opts), nil
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
func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func checkLuaSyntax(code []byte) error {
	if _, err := exec.LookPath("luac"); err != nil {
		return nil
	}
	tmp, err := os.CreateTemp("", "mochi_*.lua")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.Write(code); err != nil {
		tmp.Close()
		return err
	}
	tmp.Close()
	cmd := exec.Command("luac", "-p", tmp.Name())
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("luac error: %v\n%s", err, out)
	}
	return nil
}
