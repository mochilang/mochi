package pl

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf    bytes.Buffer
	indent int
	tmp    int
	vars   map[string]string
}

func New() *Compiler {
	return &Compiler{vars: make(map[string]string)}
}

func (c *Compiler) newVar(base string) string {
	name := sanitizeVar(base)
	if _, ok := c.vars[base]; !ok {
		c.vars[base] = name
		return name
	}
	tmp := fmt.Sprintf("%s_%d", name, c.tmp)
	c.tmp++
	c.vars[base] = tmp
	return tmp
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.tmp = 0
	c.vars = make(map[string]string)

	c.writeln(":- initialization(main, main).")
	c.writeln("main :-")
	c.indent++
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.writeln("true.")
	c.indent--
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, arith, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeVar(s.Let.Name)
		if arith {
			c.writeln(fmt.Sprintf("%s is %s,", name, val))
		} else {
			c.writeln(fmt.Sprintf("%s = %s,", name, val))
		}
		c.vars[s.Let.Name] = name
	case s.Var != nil:
		name := c.newVar(s.Var.Name)
		if s.Var.Value == nil {
			c.writeln(fmt.Sprintf("%s = _,", name))
			return nil
		}
		val, arith, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		if arith {
			c.writeln(fmt.Sprintf("%s is %s,", name, val))
		} else {
			c.writeln(fmt.Sprintf("%s = %s,", name, val))
		}
	case s.Assign != nil:
		name := c.newVar(s.Assign.Name)
		val, arith, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		if arith {
			c.writeln(fmt.Sprintf("%s is %s,", name, val))
		} else {
			c.writeln(fmt.Sprintf("%s = %s,", name, val))
		}
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Expr != nil:
		if call := getPrintCall(s.Expr.Expr); call != nil {
			arg, arith, err := c.compileExpr(call.Args[0])
			if err != nil {
				return err
			}
			if arith {
				tmp := c.newTmp()
				c.writeln(fmt.Sprintf("%s is %s,", tmp, arg))
				c.writeln(fmt.Sprintf("writeln(%s),", tmp))
			} else if isBoolExpr(arg) {
				tmp := c.newTmp()
				c.writeln(fmt.Sprintf("(%s -> %s = true ; %s = false),", arg, tmp, tmp))
				c.writeln(fmt.Sprintf("writeln(%s),", tmp))
			} else {
				c.writeln(fmt.Sprintf("writeln(%s),", arg))
			}
			return nil
		}
		return fmt.Errorf("unsupported expression statement")
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	v := sanitizeVar(fs.Name)
	if fs.RangeEnd != nil {
		start, _, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		end, _, err := c.compileExpr(fs.RangeEnd)
		if err != nil {
			return err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("%s is %s - 1,", tmp, end))
		c.writeln(fmt.Sprintf("(between(%s, %s, %s),", start, tmp, v))
		c.indent++
		for _, st := range fs.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.writeln("fail")
		c.indent--
		c.writeln("; true),")
		return nil
	}
	src, _, err := c.compileExpr(fs.Source)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(member(%s, %s),", v, src))
	c.indent++
	for _, st := range fs.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("fail")
	c.indent--
	c.writeln("; true),")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, bool, error) {
	if e == nil {
		return "0", true, nil
	}
	res, arith, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", false, err
	}
	for _, op := range e.Binary.Right {
		rhs, ar, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", false, err
		}
		arith = true
		if op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" {
			res = fmt.Sprintf("(%s %s %s)", res, op.Op, rhs)
		} else if op.Op == "%" {
			res = fmt.Sprintf("(%s mod %s)", res, rhs)
		} else if op.Op == "==" {
			res = fmt.Sprintf("(%s =:= %s)", res, rhs)
			arith = false
		} else if op.Op == "!=" {
			res = fmt.Sprintf("(%s =\\= %s)", res, rhs)
			arith = false
		} else if op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=" {
			res = fmt.Sprintf("(%s %s %s)", res, op.Op, rhs)
			arith = false
		} else if op.Op == "&&" {
			res = fmt.Sprintf("(%s, %s)", res, rhs)
			arith = false
		} else if op.Op == "||" {
			res = fmt.Sprintf("(%s ; %s)", res, rhs)
			arith = false
		} else {
			return "", false, fmt.Errorf("unsupported op")
		}
		if ar == false && (op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" || op.Op == "%") {
			arith = true
		}
	}
	return res, arith, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, bool, error) {
	val, arith, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", false, err
	}
	for _, op := range u.Ops {
		if op == "-" {
			val = fmt.Sprintf("(-%s)", val)
			arith = true
		} else {
			return "", false, fmt.Errorf("unsupported unary op")
		}
	}
	return val, arith, nil
}

func (c *Compiler) compilePostfix(pf *parser.PostfixExpr) (string, bool, error) {
	if len(pf.Ops) != 0 {
		return "", false, fmt.Errorf("postfix not supported")
	}
	return c.compilePrimary(pf.Target)
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, bool, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if v, ok := c.vars[p.Selector.Root]; ok {
			return v, false, nil
		}
		return sanitizeVar(p.Selector.Root), false, nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.List != nil:
		elems := []string{}
		for _, e := range p.List.Elems {
			s, _, err := c.compileExpr(e)
			if err != nil {
				return "", false, err
			}
			elems = append(elems, s)
		}
		return "[" + strings.Join(elems, ", ") + "]", false, nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	}
	return "", false, fmt.Errorf("unsupported primary")
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, bool, error) {
	switch call.Func {
	case "append":
		if len(call.Args) != 2 {
			return "", false, fmt.Errorf("append expects 2 args")
		}
		list, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		elem, _, err := c.compileExpr(call.Args[1])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("append(%s, [%s], %s),", list, elem, tmp))
		return tmp, false, nil
	case "avg":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("avg expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		s := c.newTmp()
		n := c.newTmp()
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("sum_list(%s, %s),", arg, s))
		c.writeln(fmt.Sprintf("length(%s, %s),", arg, n))
		c.writeln(fmt.Sprintf("%s > 0,", n))
		c.writeln(fmt.Sprintf("%s is %s / %s,", tmp, s, n))
		return tmp, true, nil
	case "sum":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("sum expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("sum_list(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "min":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("min expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("min_list(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "max":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("max expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("max_list(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "len":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("len expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		if strings.HasPrefix(arg, "\"") {
			c.writeln(fmt.Sprintf("string_length(%s, %s),", arg, tmp))
		} else {
			c.writeln(fmt.Sprintf("length(%s, %s),", arg, tmp))
		}
		return tmp, true, nil
	default:
		return "", false, fmt.Errorf("unsupported call")
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, bool, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), true, nil
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float), true, nil
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str), false, nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true", false, nil
		}
		return "false", false, nil
	default:
		return "", false, fmt.Errorf("unsupported literal")
	}
}

func (c *Compiler) lookupVar(name string) string {
	if v, ok := c.vars[name]; ok {
		return v
	}
	return sanitizeVar(name)
}

func (c *Compiler) newTmp() string {
	s := fmt.Sprintf("_V%d", c.tmp)
	c.tmp++
	return s
}

func sanitizeVar(s string) string {
	s = strings.ReplaceAll(s, "-", "_")
	if s == "" {
		return "_"
	}
	if s[0] >= 'a' && s[0] <= 'z' {
		s = strings.ToUpper(s[:1]) + s[1:]
	}
	return s
}

func getPrintCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	if p.Target != nil && p.Target.Call != nil && p.Target.Call.Func == "print" {
		return p.Target.Call
	}
	return nil
}

func isBoolExpr(s string) bool {
	ops := []string{"=:=", "=\\=", "<", "<=", ">", ">="}
	for _, op := range ops {
		if strings.Contains(s, op) {
			return true
		}
	}
	return false
}
