//go:build slow

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
	if len(p.Ops) > 0 {
		return "", fmt.Errorf("postfix operations not supported")
	}
	return c.compilePrimary(p.Target)
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

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func init() {}
