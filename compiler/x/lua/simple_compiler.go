//go:build ignore

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
