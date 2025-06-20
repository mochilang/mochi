package ktcode

import (
	"bytes"
	"fmt"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Kotlin source code.
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	mainStmts []*parser.Statement
}

// New creates a new Kotlin compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

// Compile generates Kotlin code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		c.mainStmts = append(c.mainStmts, s)
	}
	c.writeln("fun main() {")
	c.indent++
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Return != nil:
		return c.compileReturn(s.Return)
	default:
		return nil
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	expr, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	if stmt.Type != nil {
		t := c.resolveTypeRef(stmt.Type)
		typ := ktType(t)
		c.writeln(fmt.Sprintf("val %s: %s = %s", sanitizeName(stmt.Name), typ, expr))
		c.env.SetVar(stmt.Name, t, false)
	} else {
		c.writeln(fmt.Sprintf("val %s = %s", sanitizeName(stmt.Name), expr))
	}
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	value := "null"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		value = v
	}
	if stmt.Type != nil {
		t := c.resolveTypeRef(stmt.Type)
		typ := ktType(t)
		c.writeln(fmt.Sprintf("var %s: %s = %s", sanitizeName(stmt.Name), typ, value))
		c.env.SetVar(stmt.Name, t, true)
	} else {
		c.writeln(fmt.Sprintf("var %s = %s", sanitizeName(stmt.Name), value))
	}
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	idxStrs := make([]string, 0, len(stmt.Index))
	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		idxStrs = append(idxStrs, iexpr)
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	if len(stmt.Index) > 0 {
		if t, err := c.env.GetVar(stmt.Name); err == nil {
			if _, ok := t.(types.ListType); ok {
				idx := idxStrs[len(idxStrs)-1]
				c.writeln("run {")
				c.indent++
				c.writeln(fmt.Sprintf("val _tmp = %s.toMutableList()", sanitizeName(stmt.Name)))
				c.writeln(fmt.Sprintf("_tmp[%s] = %s", idx, rhs))
				c.writeln(fmt.Sprintf("%s = _tmp", sanitizeName(stmt.Name)))
				c.indent--
				c.writeln("}")
				return nil
			}
			if _, ok := t.(types.MapType); ok {
				idx := idxStrs[len(idxStrs)-1]
				c.writeln("run {")
				c.indent++
				c.writeln(fmt.Sprintf("val _tmp = %s.toMutableMap()", sanitizeName(stmt.Name)))
				c.writeln(fmt.Sprintf("_tmp[%s] = %s", idx, rhs))
				c.writeln(fmt.Sprintf("%s = _tmp", sanitizeName(stmt.Name)))
				c.indent--
				c.writeln("}")
				return nil
			}
		}
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, rhs))
	return nil
}

func (c *Compiler) compileReturn(stmt *parser.ReturnStmt) error {
	expr, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("sealed interface %s", name))
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			fields := []string{}
			for _, f := range v.Fields {
				typ := ktType(c.resolveTypeRef(f.Type))
				fields = append(fields, fmt.Sprintf("val %s: %s", sanitizeName(f.Name), typ))
			}
			if len(fields) == 0 {
				c.writeln(fmt.Sprintf("data class %s() : %s", vname, name))
			} else {
				c.writeln(fmt.Sprintf("data class %s(%s) : %s", vname, joinArgs(fields), name))
			}
		}
		return nil
	}
	fields := []string{}
	for _, m := range t.Members {
		if m.Field == nil {
			continue
		}
		typ := ktType(c.resolveTypeRef(m.Field.Type))
		fields = append(fields, fmt.Sprintf("val %s: %s", sanitizeName(m.Field.Name), typ))
	}
	c.writeln(fmt.Sprintf("data class %s(%s)", name, joinArgs(fields)))
	return nil
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
		c.writeln(fmt.Sprintf("for (%s in %s until %s) {", name, start, end))
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("for (%s in %s) {", name, src))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
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
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	cur := stmt
	for cur.ElseIf != nil {
		next := cur.ElseIf
		cond, err := c.compileExpr(next.Cond)
		if err != nil {
			return err
		}
		c.writeIndent()
		c.buf.WriteString("} else if (" + cond + ") {\n")
		c.indent++
		for _, s := range next.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		cur = next
	}
	if len(cur.Else) == 0 {
		c.writeln("}")
		return nil
	}
	c.writeIndent()
	c.buf.WriteString("} else {\n")
	c.indent++
	for _, s := range cur.Else {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	// simple cross join without sort/skip/take/group/join
	if len(q.Froms) > 0 && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Group == nil && len(q.Joins) == 0 {
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
		var where string
		if q.Where != nil {
			where, err = c.compileExpr(q.Where)
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
		c.env = orig

		var buf bytes.Buffer
		buf.WriteString("run {\n")
		buf.WriteString("                val _src = " + src + "\n")
		buf.WriteString("                val _res = mutableListOf<Any>()\n")
		buf.WriteString(fmt.Sprintf("                for (%s in _src) {\n", sanitizeName(q.Var)))
		indent := "                        "
		for i, fs := range fromSrcs {
			buf.WriteString(indent + fmt.Sprintf("for (%s in %s) {\n", sanitizeName(q.Froms[i].Var), fs))
			indent += "        "
		}
		if where != "" {
			buf.WriteString(indent + "if (" + where + ") {\n")
			indent += "        "
		}
		buf.WriteString(indent + "_res.add(" + sel + ")\n")
		if where != "" {
			indent = indent[:len(indent)-8]
			buf.WriteString(indent + "}\n")
		}
		for range fromSrcs {
			indent = indent[:len(indent)-8]
			buf.WriteString(indent + "}\n")
		}
		indent = indent[:len(indent)-8]
		buf.WriteString(indent + "}\n")
		buf.WriteString("                _res\n")
		buf.WriteString("        }")
		return buf.String(), nil
	}

	varName := sanitizeName(q.Var)
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var where, sortKey, skip, take string
	if q.Where != nil {
		where, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		sortKey, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skip, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		take, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}
	var buf bytes.Buffer
	buf.WriteString("run {\n")
	buf.WriteString("                var res = " + src + "\n")
	if where != "" {
		buf.WriteString(fmt.Sprintf("                res = res.filter { %s -> %s }\n", varName, where))
	}
	if sortKey != "" {
		buf.WriteString(fmt.Sprintf("                res = res.sortedBy { %s -> %s }\n", varName, sortKey))
	}
	if skip != "" {
		buf.WriteString("                res = res.drop(" + skip + ")\n")
	}
	if take != "" {
		buf.WriteString("                res = res.take(" + take + ")\n")
	}
	buf.WriteString(fmt.Sprintf("                res = res.map { %s -> %s }\n", varName, sel))
	buf.WriteString("                res\n")
	buf.WriteString("        }")
	return buf.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	buf.WriteString("run {\n")
	buf.WriteString("                val _t = " + target + "\n")
	buf.WriteString("                when {\n")
	for i, cse := range m.Cases {
		res, err := c.compileExpr(cse.Result)
		if err != nil {
			return "", err
		}
		if call, ok := callPattern(cse.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				buf.WriteString("                        _t is " + sanitizeName(call.Func) + " -> {\n")
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						field := sanitizeName(st.Order[idx])
						buf.WriteString(fmt.Sprintf("                                val %s = _t.%s\n", sanitizeName(id), field))
					}
				}
				buf.WriteString("                                " + res + "\n")
				buf.WriteString("                        }\n")
				continue
			}
		}
		if ident, ok := identName(cse.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				buf.WriteString("                        _t is " + sanitizeName(ident) + " -> " + res + "\n")
				continue
			}
		}
		if isUnderscoreExpr(cse.Pattern) {
			buf.WriteString("                        else -> " + res + "\n")
			continue
		}
		pat, err := c.compileExpr(cse.Pattern)
		if err != nil {
			return "", err
		}
		buf.WriteString("                        _t == " + pat + " -> " + res + "\n")
		if i == len(m.Cases)-1 {
			buf.WriteString("                        else -> null\n")
		}
	}
	buf.WriteString("                }\n")
	buf.WriteString("        }")
	return buf.String(), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		param := sanitizeName(p.Name)
		if p.Type != nil {
			typ := ktType(c.resolveTypeRef(p.Type))
			param += ": " + typ
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		}
		params[i] = param
	}
	sub := &Compiler{env: child}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	ret := "Unit"
	if fn.Return != nil {
		ret = ktType(c.resolveTypeRef(fn.Return))
	}
	return "fun(" + joinArgs(params) + ") : " + ret + " {\n" + body + "}", nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fun " + name + "(")
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
		if p.Type != nil {
			typ := c.resolveTypeRef(p.Type)
			c.buf.WriteString(": " + ktType(typ))
			child.SetVar(p.Name, typ, true)
		}
	}
	ret := "Unit"
	if fun.Return != nil {
		ret = ktType(c.resolveTypeRef(fun.Return))
	}
	c.buf.WriteString(") : " + ret + " {")
	c.buf.WriteByte('\n')
	sub := &Compiler{env: child, indent: c.indent + 1}
	for _, s := range fun.Body {
		if err := sub.compileStmt(s); err != nil {
			return err
		}
	}
	c.buf.Write(sub.buf.Bytes())
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
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
		rhs, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		ops = append(ops, part.Op)
		operands = append(operands, rhs)
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}}

	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				op := ops[i]
				var expr string
				if op == "in" {
					expr = fmt.Sprintf("%s.contains(%s)", r, l)
				} else {
					expr = fmt.Sprintf("(%s %s %s)", l, op, r)
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
	if u == nil {
		return "", nil
	}
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" || op == "!" {
			expr = fmt.Sprintf("%s%s", op, expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if p == nil {
		return "", nil
	}
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s[%s]", expr, idx)
				continue
			}
			start := "0"
			if op.Index.Start != nil {
				s, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				start = s
			}
			end := fmt.Sprintf("%s.length", expr)
			if op.Index.End != nil {
				e, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				end = e
			}
			if isStringPrimary(p.Target, c.env) {
				expr = fmt.Sprintf("%s.substring(%s, %s)", expr, start, end)
			} else {
				expr = fmt.Sprintf("%s.subList(%s, %s)", expr, start, end)
			}
			continue
		}
		if op.Cast != nil {
			t := c.resolveTypeRef(op.Cast.Type)
			switch t.(type) {
			case types.FloatType:
				expr = fmt.Sprintf("(%s).toDouble()", expr)
			case types.IntType:
				expr = fmt.Sprintf("(%s).toInt()", expr)
			case types.StringType:
				expr = fmt.Sprintf("(%s).toString()", expr)
			default:
				expr = fmt.Sprintf("(%s) as %s", expr, ktType(t))
			}
			continue
		}
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, arg)
			}
			if expr == "print" {
				expr = fmt.Sprintf("println(%s)", joinArgs(args))
			} else if expr == "len" {
				if len(op.Call.Args) == 1 && isStringExpr(op.Call.Args[0], c.env) {
					expr = fmt.Sprintf("%s.length", args[0])
				} else {
					expr = fmt.Sprintf("%s.size", args[0])
				}
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, joinArgs(args))
			}
			continue
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
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("\"%s\"", *p.Lit.Str), nil
		}
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.List != nil:
		elems := []string{}
		for _, e := range p.List.Elems {
			ce, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems = append(elems, ce)
		}
		if len(elems) == 0 {
			return "listOf()", nil
		}
		return "listOf(" + joinArgs(elems) + ")", nil
	case p.Map != nil:
		items := []string{}
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items = append(items, fmt.Sprintf("%s to %s", k, v))
		}
		if len(items) == 0 {
			return "mutableMapOf<Any, Any>()", nil
		}
		return "mutableMapOf(" + joinArgs(items) + ")", nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) == 0 {
			return expr, nil
		}
		for _, f := range p.Selector.Tail {
			expr += "." + sanitizeName(f)
		}
		return expr, nil
	case p.Struct != nil:
		args := []string{}
		for _, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			args = append(args, fmt.Sprintf("%s = %s", sanitizeName(f.Name), v))
		}
		return sanitizeName(p.Struct.Name) + "(" + joinArgs(args) + ")", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			ce, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, ce)
		}
		name := sanitizeName(p.Call.Func)
		if name == "print" {
			return "println(" + joinArgs(args) + ")", nil
		}
		if name == "len" && len(args) == 1 {
			if isStringExpr(p.Call.Args[0], c.env) {
				return args[0] + ".length", nil
			}
			return args[0] + ".size", nil
		}
		if name == "count" && len(args) == 1 {
			return args[0] + ".size", nil
		}
		if name == "avg" && len(args) == 1 {
			return args[0] + ".average()", nil
		}
		if name == "str" && len(args) == 1 {
			return args[0] + ".toString()", nil
		}
		if name == "input" && len(args) == 0 {
			return "readln()", nil
		}
		return name + "(" + joinArgs(args) + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "bool":
			return types.BoolType{}
		case "string":
			return types.StringType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: c.resolveTypeRef(t.Generic.Args[0])}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: c.resolveTypeRef(t.Generic.Args[0]), Value: c.resolveTypeRef(t.Generic.Args[1])}
		}
	}
	return types.AnyType{}
}

func ktType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return "List<" + ktType(tt.Elem) + ">"
	case types.MapType:
		return "MutableMap<" + ktType(tt.Key) + ", " + ktType(tt.Value) + ">"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	default:
		return "Any"
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("        ")
	}
}

func joinArgs(args []string) string {
	if len(args) == 0 {
		return ""
	}
	res := args[0]
	for i := 1; i < len(args); i++ {
		res += ", " + args[i]
	}
	return res
}
