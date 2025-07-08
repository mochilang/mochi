//go:build slow

package luacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a subset of Mochi into Lua.
type Compiler struct {
	buf        bytes.Buffer
	indent     int
	used       map[string]bool
	loopLabels []string
	labelCount int
}

// New creates a new Compiler.
func New(_ *types.Env) *Compiler {
	return &Compiler{used: make(map[string]bool)}
}

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
	c.loopLabels = nil
	c.labelCount = 0
	for _, st := range p.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	var out bytes.Buffer
	if c.used["iter"] {
		out.WriteString("local function __iter(v)\n")
		out.WriteString("\tif type(v)=='table' then\n")
		out.WriteString("\t\tif v[1]~=nil or #v>0 then\n")
		out.WriteString("\t\t\tlocal i=0\n")
		out.WriteString("\t\t\tlocal n=#v\n")
		out.WriteString("\t\t\treturn function()\n")
		out.WriteString("\t\t\t\ti=i+1\n")
		out.WriteString("\t\t\t\tif i<=n then return v[i] end\n")
		out.WriteString("\t\t\tend\n")
		out.WriteString("\t\telse\n")
		out.WriteString("\t\t\tlocal keys={}\n")
		out.WriteString("\t\t\tfor k in pairs(v) do table.insert(keys,k) end\n")
		out.WriteString("\t\t\ttable.sort(keys)\n")
		out.WriteString("\t\t\tlocal i=0\n")
		out.WriteString("\t\t\tlocal n=#keys\n")
		out.WriteString("\t\t\treturn function()\n")
		out.WriteString("\t\t\t\ti=i+1\n")
		out.WriteString("\t\t\t\tif i<=n then return keys[i] end\n")
		out.WriteString("\t\t\tend\n")
		out.WriteString("\t\tend\n")
		out.WriteString("\telse\n")
		out.WriteString("\t\treturn function() return nil end\n")
		out.WriteString("\tend\nend\n\n")
	}
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
	if c.used["count"] {
		out.WriteString("local function count(v)\n")
		out.WriteString("\tif type(v)=='table' then\n")
		out.WriteString("\t\tif v[1]~=nil or #v>0 then return #v end\n")
		out.WriteString("\t\tlocal n=0 for _ in pairs(v) do n=n+1 end return n\n")
		out.WriteString("\telseif type(v)=='string' then\n")
		out.WriteString("\t\treturn #v\n")
		out.WriteString("\telse return 0 end\nend\n\n")
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
		out.WriteString("\t\tio.write('\n')\n")
		out.WriteString("\telse\n")
		out.WriteString("\t\tprint(v)\n")
		out.WriteString("\tend\nend\n\n")
	}
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) newLabel() string {
	c.labelCount++
	return fmt.Sprintf("__continue%d", c.labelCount)
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
			var err error
			val, err = c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
		}
		c.writeln(fmt.Sprintf("%s = %s", s.Var.Name, val))
	case s.Assign != nil:
		if len(s.Assign.Index) > 0 || len(s.Assign.Field) > 0 {
			return fmt.Errorf("assignment with index or field not supported")
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", s.Assign.Name, val))
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
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val)
	case s.If != nil:
		return c.compileIf("if", s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break")
	case s.Continue != nil:
		if len(c.loopLabels) == 0 {
			return fmt.Errorf("continue outside loop at line %d", s.Pos.Line)
		}
		label := c.loopLabels[len(c.loopLabels)-1]
		c.writeln("goto " + label)
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

func (c *Compiler) compileIf(keyword string, ifs *parser.IfStmt) error {
	cond, err := c.compileExpr(ifs.Cond)
	if err != nil {
		return err
	}
	c.writeln(keyword + " " + cond + " then")
	c.indent++
	for _, st := range ifs.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if ifs.ElseIf != nil {
		return c.compileIf("elseif", ifs.ElseIf)
	}
	if ifs.Else != nil {
		c.writeln("else")
		c.indent++
		for _, st := range ifs.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileWhile(ws *parser.WhileStmt) error {
	label := c.newLabel()
	cond, err := c.compileExpr(ws.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " do")
	c.indent++
	c.loopLabels = append(c.loopLabels, label)
	for _, st := range ws.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("::" + label + "::")
	c.loopLabels = c.loopLabels[:len(c.loopLabels)-1]
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	label := c.newLabel()
	if fs.RangeEnd != nil {
		start, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fs.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s=%s,%s do", fs.Name, start, end))
	} else {
		src, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		c.used["iter"] = true
		c.writeln(fmt.Sprintf("for %s in __iter(%s) do", fs.Name, src))
	}
	c.indent++
	c.loopLabels = append(c.loopLabels, label)
	for _, st := range fs.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("::" + label + "::")
	c.loopLabels = c.loopLabels[:len(c.loopLabels)-1]
	c.indent--
	c.writeln("end")
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
	operands := []string{left}
	ops := []string{}
	for _, part := range e.Binary.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		ops = append(ops, part.Op)
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
				case "==":
					expr = fmt.Sprintf("(%s == %s)", l, r)
				case "!=":
					expr = fmt.Sprintf("(%s ~= %s)", l, r)
				default:
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
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int" {
				val = fmt.Sprintf("tonumber(%s)", val)
			}
		case op.Index != nil:
			if op.Index.Start == nil || op.Index.Colon != nil {
				return "", fmt.Errorf("complex indexing not supported")
			}
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s[%s]", val, idx)
		default:
			return "", fmt.Errorf("unsupported postfix at line %d", p.Target.Pos.Line)
		}
	}
	return val, nil
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
	case p.Map != nil:
		parts := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("[%s]=%s", k, v)
		}
		return "{" + strings.Join(parts, ", ") + "}", nil
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
		case "append":
			c.used["append"] = true
			return fmt.Sprintf("append(%s)", argStr), nil
		case "avg":
			c.used["avg"] = true
			return fmt.Sprintf("avg(%s)", argStr), nil
		case "count":
			c.used["count"] = true
			return fmt.Sprintf("count(%s)", argStr), nil
		default:
			return fmt.Sprintf("%s(%s)", name, argStr), nil
		}
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, p := range p.FunExpr.Params {
			params[i] = p.Name
		}
		body, err := c.compileExpr(p.FunExpr.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("function(%s) return %s end", strings.Join(params, ", "), body), nil
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func init() {}
