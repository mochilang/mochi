//go:build ignore && slow

package luacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a small subset of Mochi into Lua.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	used   map[string]bool
}

// New creates a new Compiler.
func New(_ *types.Env) *Compiler { return &Compiler{used: make(map[string]bool)} }

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile converts the parsed program into Lua code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.used = make(map[string]bool)
	for _, st := range p.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	var out bytes.Buffer
	if c.used["append"] {
		out.WriteString("local function append(lst, v)\n")
		out.WriteString("\tlocal out = {}\n")
		out.WriteString("\tfor i=1,#lst do out[#out+1]=lst[i] end\n")
		out.WriteString("\tout[#out+1]=v\n")
		out.WriteString("\treturn out\nend\n\n")
	}
	if c.used["avg"] {
		out.WriteString("local function avg(lst)\n")
		out.WriteString("\tlocal sum = 0\n")
		out.WriteString("\tfor _, v in ipairs(lst) do sum = sum + v end\n")
		out.WriteString("\treturn sum / #lst\nend\n\n")
	}
	if c.used["print_value"] {
		out.WriteString("local function print_value(v)\n")
		out.WriteString("\tif v == nil then\n")
		out.WriteString("\t\tprint('<nil>')\n")
		out.WriteString("\t\treturn\n")
		out.WriteString("\tend\n")
		out.WriteString("\tif type(v)=='number' and v == math.floor(v) then\n")
		out.WriteString("\t\tprint(math.floor(v))\n")
		out.WriteString("\t\treturn\n")
		out.WriteString("\tend\n")
		out.WriteString("\tif type(v)=='table' then\n")
		out.WriteString("\t\tfor i,x in ipairs(v) do\n")
		out.WriteString("\t\t\tio.write(x)\n")
		out.WriteString("\t\t\tif i < #v then io.write(' ') end\n")
		out.WriteString("\t\tend\n")
		out.WriteString("\t\tio.write('\\n')\n")
		out.WriteString("\telse\n")
		out.WriteString("\t\tprint(v)\n")
		out.WriteString("\tend\nend\n\n")
	}
	if c.used["index"] {
		out.WriteString("local function index(obj, i)\n")
		out.WriteString("\tif type(obj)=='string' then\n")
		out.WriteString("\t\tlocal len=#obj\n")
		out.WriteString("\t\tif i<0 then i=len+i+1 else i=i+1 end\n")
		out.WriteString("\t\treturn string.sub(obj,i,i)\n")
		out.WriteString("\telseif type(obj)=='table' then\n")
		out.WriteString("\t\treturn obj[i+1]\n")
		out.WriteString("\telse\n")
		out.WriteString("\t\treturn nil\n")
		out.WriteString("\tend\nend\n\n")
	}
	if c.used["query"] {
		out.WriteString(helperQuery)
		out.WriteString("\n")
	}
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("local %s = %s", s.Let.Name, val))
	case s.Var != nil:
		val := "nil"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			val = v
		}
		c.writeln(fmt.Sprintf("local %s = %s", s.Var.Name, val))
	case s.Fun != nil:
		params := make([]string, len(s.Fun.Params))
		for i, p := range s.Fun.Params {
			params[i] = p.Name
		}
		c.writeln(fmt.Sprintf("local function %s(%s)", s.Fun.Name, strings.Join(params, ", ")))
		c.indent++
		for _, st := range s.Fun.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end")
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break")
	case s.Expr != nil:
		val, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(val)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "nil", nil
	}

	left, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	leftIsStr := isStringUnary(e.Binary.Left)

	operands := []string{left}
	ops := []string{}
	isStr := []bool{leftIsStr}
	for _, part := range e.Binary.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		ops = append(ops, part.Op)
		isStr = append(isStr, isStringPostfix(part.Right))
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!="}, {"&&"}, {"||"}}
	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				op := ops[i]
				var expr string
				switch op {
				case "&&":
					expr = fmt.Sprintf("(%s and %s)", l, r)
				case "||":
					expr = fmt.Sprintf("(%s or %s)", l, r)
				case "+":
					if isStr[i] || isStr[i+1] {
						expr = fmt.Sprintf("(%s .. %s)", l, r)
					} else {
						expr = fmt.Sprintf("(%s + %s)", l, r)
					}
				default:
					expr = fmt.Sprintf("(%s %s %s)", l, op, r)
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
				val := isStr[i] || isStr[i+1]
				isStr[i] = val
				isStr = append(isStr[:i+1], isStr[i+2:]...)
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
	v, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			v = fmt.Sprintf("(-%s)", v)
		} else if op == "!" {
			v = fmt.Sprintf("not %s", v)
		}
	}
	return v, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil {
				return "", fmt.Errorf("slices not supported")
			}
			if op.Index.Start == nil {
				return "", fmt.Errorf("empty index")
			}
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			c.used["index"] = true
			expr = fmt.Sprintf("index(%s, %s)", expr, idx)
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
		case op.Field != nil:
			expr = fmt.Sprintf("%s.%s", expr, op.Field.Name)
		default:
			return "", fmt.Errorf("unsupported postfix operation")
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
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		}
		if p.Lit.Null {
			return "nil", nil
		}
		return "nil", nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "{" + strings.Join(elems, ", ") + "}", nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		name := p.Call.Func
		argStr := strings.Join(args, ", ")
		switch name {
		case "print":
			c.used["print_value"] = true
			return fmt.Sprintf("print_value(%s)", argStr), nil
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			return fmt.Sprintf("#%s", args[0]), nil
		case "append":
			c.used["append"] = true
			return fmt.Sprintf("append(%s)", argStr), nil
		case "avg":
			c.used["avg"] = true
			return fmt.Sprintf("avg(%s)", argStr), nil
		default:
			return fmt.Sprintf("%s(%s)", name, argStr), nil
		}
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return p.Selector.Root, nil
		}
		return "", fmt.Errorf("field access not supported")
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
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
		cond, err := c.compileExpr(cur.ElseIf.Cond)
		if err != nil {
			return err
		}
		c.writeln("elseif " + cond + " then")
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

func (c *Compiler) compileIfExpr(e *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(e.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(e.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if e.Else != nil {
		elseExpr, err = c.compileExpr(e.Else)
		if err != nil {
			return "", err
		}
	} else if e.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(e.ElseIf)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "nil"
	}
	return fmt.Sprintf("(%s and %s or %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	target := a.Name
	for _, idx := range a.Index {
		if idx.Colon != nil {
			return fmt.Errorf("slices not supported")
		}
		if idx.Start == nil {
			return fmt.Errorf("empty index")
		}
		v, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		if isStringLiteral(idx.Start) {
			target += fmt.Sprintf("[%s]", v)
		} else {
			target += fmt.Sprintf("[%s+1]", v)
		}
	}
	for _, f := range a.Field {
		target += "." + f.Name
	}
	c.writeln(fmt.Sprintf("%s = %s", target, val))
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " do")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := f.Name
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s=%s,(%s)-1 do", name, start, end))
	} else if isListLiteralExpr(f.Source) {
		// inline list literal iteration
		lst := f.Source.Binary.Left.Value.Target.List
		elems := make([]string, len(lst.Elems))
		for i, e := range lst.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return err
			}
			elems[i] = v
		}
		tmp := "{" + strings.Join(elems, ", ") + "}"
		if name == "_" {
			c.writeln(fmt.Sprintf("for _ in ipairs(%s) do", tmp))
		} else {
			c.writeln(fmt.Sprintf("for _, %s in ipairs(%s) do", name, tmp))
		}
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		if name == "_" {
			c.writeln(fmt.Sprintf("for _ in pairs(%s) do", src))
		} else {
			c.writeln(fmt.Sprintf("for _, %s in pairs(%s) do", name, src))
		}
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	for _, j := range q.Joins {
		if j.Side != nil && (*j.Side == "right" || *j.Side == "outer") {
			return c.compileQueryWithHelper(q)
		}
	}

	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	var b strings.Builder
	b.WriteString("(function()\n")
	b.WriteString("\tlocal _res = {}\n")
	indent := "\t"
	b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(q.Var), src))
	indent += "\t"

	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf(indent+"for _, %s in ipairs(%s) do\n", sanitizeName(f.Var), fs))
		indent += "\t"
	}

	var emitJoin func(int, string) error
	emitJoin = func(idx int, ind string) error {
		if idx >= len(q.Joins) {
			if q.Where != nil {
				cond, err := c.compileExpr(q.Where)
				if err != nil {
					return err
				}
				b.WriteString(ind + "if " + cond + " then\n")
				ind += "\t"
				sel, err := c.compileExpr(q.Select)
				if err != nil {
					return err
				}
				b.WriteString(fmt.Sprintf(ind+"_res[#_res+1] = %s\n", sel))
				ind = ind[:len(ind)-1]
				b.WriteString(ind + "end\n")
			} else {
				sel, err := c.compileExpr(q.Select)
				if err != nil {
					return err
				}
				b.WriteString(fmt.Sprintf(ind+"_res[#_res+1] = %s\n", sel))
			}
			return nil
		}
		j := q.Joins[idx]
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return err
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			return err
		}
		side := ""
		if j.Side != nil {
			side = *j.Side
		}
		if side == "left" {
			flag := fmt.Sprintf("_m%d", idx)
			iter := fmt.Sprintf("_j%d", idx)
			b.WriteString(ind + fmt.Sprintf("local %s\n", sanitizeName(j.Var)))
			b.WriteString(ind + fmt.Sprintf("local %s = false\n", flag))
			b.WriteString(fmt.Sprintf(ind+"for _, %s in ipairs(%s) do\n", iter, js))
			b.WriteString(ind + "\t" + sanitizeName(j.Var) + " = " + iter + "\n")
			b.WriteString(ind + "\tif " + on + " then\n")
			b.WriteString(ind + "\t\t" + flag + " = true\n")
			if err := emitJoin(idx+1, ind+"\t\t"); err != nil {
				return err
			}
			b.WriteString(ind + "\tend\n")
			b.WriteString(ind + "end\n")
			b.WriteString(fmt.Sprintf(ind+"if not %s then\n", flag))
			b.WriteString(ind + "\t" + sanitizeName(j.Var) + " = nil\n")
			if err := emitJoin(idx+1, ind+"\t"); err != nil {
				return err
			}
			b.WriteString(ind + "end\n")
			return nil
		}
		b.WriteString(fmt.Sprintf(ind+"for _, %s in ipairs(%s) do\n", sanitizeName(j.Var), js))
		b.WriteString(ind + "\tif " + on + " then\n")
		if err := emitJoin(idx+1, ind+"\t\t"); err != nil {
			return err
		}
		b.WriteString(ind + "\tend\n")
		b.WriteString(ind + "end\n")
		return nil
	}
	if err := emitJoin(0, indent); err != nil {
		return "", err
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

func (c *Compiler) compileQueryWithHelper(q *parser.QueryExpr) (string, error) {
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
	joinOns := make([]string, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		joinOns[i] = on
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var whereExpr, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
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

	params := []string{sanitizeName(q.Var)}
	for _, f := range q.Froms {
		params = append(params, sanitizeName(f.Var))
	}
	paramCopy := append([]string(nil), params...)

	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	for _, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("{ items = %s }", fs))
	}
	for i, js := range joinSrcs {
		onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
		spec := fmt.Sprintf("{ items = %s, on = function(%s) return %s end", js, strings.Join(onParams, ", "), joinOns[i])
		side := joinSides[i]
		if side == "left" || side == "outer" {
			spec += ", left = true"
		}
		if side == "right" || side == "outer" {
			spec += ", right = true"
		}
		spec += " }"
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}

	allParams := strings.Join(paramCopy, ", ")
	selectFn := fmt.Sprintf("function(%s) return %s end", allParams, sel)
	var whereFn, sortFn string
	if whereExpr != "" {
		whereFn = fmt.Sprintf("function(%s) return (%s) end", allParams, whereExpr)
	}
	if sortExpr != "" {
		sortFn = fmt.Sprintf("function(%s) return (%s) end", allParams, sortExpr)
	}

	var b strings.Builder
	b.WriteString("(function()\n")
	b.WriteString("\tlocal _src = " + src + "\n")
	b.WriteString("\treturn __query(_src, {\n")
	for i, j := range joins {
		b.WriteString("\t\t" + j)
		if i != len(joins)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString("\t}, { selectFn = " + selectFn)
	if whereFn != "" {
		b.WriteString(", where = " + whereFn)
	}
	if sortFn != "" {
		b.WriteString(", sortKey = " + sortFn)
	}
	if skipExpr != "" {
		b.WriteString(", skip = " + skipExpr)
	}
	if takeExpr != "" {
		b.WriteString(", take = " + takeExpr)
	}
	b.WriteString(" })\n")
	b.WriteString("end)()")
	c.used["query"] = true
	return b.String(), nil
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func isStringUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isStringPostfix(u.Value)
}

func isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	if p.Target == nil || p.Target.Lit == nil {
		return false
	}
	return p.Target.Lit.Str != nil
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	p := u.Value
	if p == nil || p.Target == nil || p.Target.Lit == nil {
		return false
	}
	return p.Target.Lit.Str != nil
}

func isListLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	p := u.Value
	return p != nil && p.Target != nil && p.Target.List != nil
}

func init() {}

const helperQuery = "function __query(src, joins, opts)\n" +
	"    local whereFn = opts.where\n" +
	"    local items = {}\n" +
	"    if #joins == 0 and whereFn then\n" +
	"        for _, v in ipairs(src) do if whereFn(v) then items[#items+1] = {v} end end\n" +
	"    else\n" +
	"        for _, v in ipairs(src) do items[#items+1] = {v} end\n" +
	"    end\n" +
	"    for ji, j in ipairs(joins) do\n" +
	"        local joined = {}\n" +
	"        local jitems = j.items or {}\n" +
	"        if j.right and j.left then\n" +
	"            local matched = {}\n" +
	"            for _, left in ipairs(items) do\n" +
	"                local m = false\n" +
	"                for ri, right in ipairs(jitems) do\n" +
	"                    local keep = true\n" +
	"                    if j.on then\n" +
	"                        local args = {table.unpack(left)}\n" +
	"                        args[#args+1] = right\n" +
	"                        keep = j.on(table.unpack(args))\n" +
	"                    end\n" +
	"                    if keep then\n" +
	"                        m = true; matched[ri] = true\n" +
	"                        local row = {table.unpack(left)}\n" +
	"                        row[#row+1] = right\n" +
	"                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
	"                        else\n" +
	"                            joined[#joined+1] = row\n" +
	"                        end\n" +
	"                    end\n" +
	"                end\n" +
	"                if not m then\n" +
	"                    local row = {table.unpack(left)}\n" +
	"                    row[#row+1] = nil\n" +
	"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
	"                    else\n" +
	"                        joined[#joined+1] = row\n" +
	"                    end\n" +
	"                end\n" +
	"            end\n" +
	"            for ri, right in ipairs(jitems) do\n" +
	"                if not matched[ri] then\n" +
	"                    local undef = {}\n" +
	"                    if #items > 0 then for _=1,#items[1] do undef[#undef+1]=nil end end\n" +
	"                    local row = {table.unpack(undef)}\n" +
	"                    row[#row+1] = right\n" +
	"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
	"                    else\n" +
	"                        joined[#joined+1] = row\n" +
	"                    end\n" +
	"                end\n" +
	"            end\n" +
	"        elseif j.right then\n" +
	"            for _, right in ipairs(jitems) do\n" +
	"                local m = false\n" +
	"                for _, left in ipairs(items) do\n" +
	"                    local keep = true\n" +
	"                    if j.on then\n" +
	"                        local args = {table.unpack(left)}\n" +
	"                        args[#args+1] = right\n" +
	"                        keep = j.on(table.unpack(args))\n" +
	"                    end\n" +
	"                    if keep then\n" +
	"                        m = true\n" +
	"                        local row = {table.unpack(left)}\n" +
	"                        row[#row+1] = right\n" +
	"                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
	"                        else\n" +
	"                            joined[#joined+1] = row\n" +
	"                        end\n" +
	"                    end\n" +
	"                end\n" +
	"                if not m then\n" +
	"                    local undef = {}\n" +
	"                    if #items > 0 then for _=1,#items[1] do undef[#undef+1]=nil end end\n" +
	"                    local row = {table.unpack(undef)}\n" +
	"                    row[#row+1] = right\n" +
	"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
	"                    else\n" +
	"                        joined[#joined+1] = row\n" +
	"                    end\n" +
	"                end\n" +
	"            end\n" +
	"        else\n" +
	"            for _, left in ipairs(items) do\n" +
	"                local m = false\n" +
	"                for _, right in ipairs(jitems) do\n" +
	"                    local keep = true\n" +
	"                    if j.on then\n" +
	"                        local args = {table.unpack(left)}\n" +
	"                        args[#args+1] = right\n" +
	"                        keep = j.on(table.unpack(args))\n" +
	"                    end\n" +
	"                    if keep then\n" +
	"                        m = true\n" +
	"                        local row = {table.unpack(left)}\n" +
	"                        row[#row+1] = right\n" +
	"                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
	"                        else\n" +
	"                            joined[#joined+1] = row\n" +
	"                        end\n" +
	"                    end\n" +
	"                end\n" +
	"                if j.left and not m then\n" +
	"                    local row = {table.unpack(left)}\n" +
	"                    row[#row+1] = nil\n" +
	"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
	"                    else\n" +
	"                        joined[#joined+1] = row\n" +
	"                    end\n" +
	"                end\n" +
	"            end\n" +
	"        end\n" +
	"        items = joined\n" +
	"    end\n" +
	"    if opts.sortKey then\n" +
	"        local pairs = {}\n" +
	"        for _, it in ipairs(items) do pairs[#pairs+1] = {item=it, key=opts.sortKey(table.unpack(it))} end\n" +
	"        table.sort(pairs, function(a,b)\n" +
	"            local ak, bk = a.key, b.key\n" +
	"            if type(ak)=='number' and type(bk)=='number' then return ak < bk end\n" +
	"            if type(ak)=='string' and type(bk)=='string' then return ak < bk end\n" +
	"            return tostring(ak) < tostring(bk)\n" +
	"        end)\n" +
	"        items = {}\n" +
	"        for i,p in ipairs(pairs) do items[i] = p.item end\n" +
	"    end\n" +
	"    if opts.skip ~= nil then\n" +
	"        local n = opts.skip\n" +
	"        if n < #items then\n" +
	"            for i=1,n do table.remove(items,1) end\n" +
	"        else\n" +
	"            items = {}\n" +
	"        end\n" +
	"    end\n" +
	"    if opts.take ~= nil then\n" +
	"        local n = opts.take\n" +
	"        if n < #items then\n" +
	"            for i=#items, n+1, -1 do table.remove(items) end\n" +
	"        end\n" +
	"    end\n" +
	"    local res = {}\n" +
	"    for _, r in ipairs(items) do res[#res+1] = opts.selectFn(table.unpack(r)) end\n" +
	"    return res\n" +
	"end\n"
