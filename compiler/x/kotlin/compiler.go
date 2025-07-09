package ktcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a limited subset of Mochi into Kotlin source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile converts the parsed program into Kotlin code. Only a subset of
// Mochi constructs are supported. Unsupported features result in an error.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0

	// Emit function declarations first.
	for _, st := range p.Statements {
		if st.Fun != nil {
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
		}
	}

	c.writeln("fun main() {")
	c.indent++
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("val %s = %s", s.Let.Name, val))
	case s.Var != nil:
		val := "null"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			val = v
		}
		c.writeln(fmt.Sprintf("var %s = %s", s.Var.Name, val))
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		target := s.Assign.Name
		for _, idx := range s.Assign.Index {
			if idx.Colon != nil {
				return fmt.Errorf("slices not supported")
			}
			if idx.Start == nil {
				return fmt.Errorf("empty index")
			}
			i, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			target += fmt.Sprintf("[%s]", i)
		}
		for _, f := range s.Assign.Field {
			target += "." + f.Name
		}
		c.writeln(fmt.Sprintf("%s = %s", target, val))
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
		return fmt.Errorf("statement not supported at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = fmt.Sprintf("%s: %s", p.Name, c.compileType(p.Type))
	}
	ret := ""
	if f.Return != nil {
		ret = ": " + c.compileType(f.Return)
	}
	c.writeln(fmt.Sprintf("fun %s(%s)%s {", f.Name, strings.Join(params, ", "), ret))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	return nil
}

func (c *Compiler) compileType(t *parser.TypeRef) string {
	if t == nil {
		return "Any"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "string":
			return "String"
		case "bool":
			return "Boolean"
		default:
			return strings.Title(*t.Simple)
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return "List<" + c.compileType(t.Generic.Args[0]) + ">"
	}
	return "Any"
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "null", nil
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
				expr := fmt.Sprintf("(%s %s %s)", l, op, r)
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

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	v, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			v = fmt.Sprintf("(-%s)", v)
		case "!":
			v = fmt.Sprintf("(!%s)", v)
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
			expr = fmt.Sprintf("%s[%s]", expr, idx)
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
		case op.Cast != nil:
			expr = fmt.Sprintf("(%s as %s)", expr, c.compileType(op.Cast.Type))
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
			return "null", nil
		}
		return "null", nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "listOf(" + strings.Join(elems, ", ") + ")", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, m := range p.Map.Items {
			k, err := c.compileExpr(m.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(m.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s to %s", k, v)
		}
		return "mapOf(" + strings.Join(items, ", ") + ")", nil
	case p.Selector != nil:
		parts := append([]string{p.Selector.Root}, p.Selector.Tail...)
		return strings.Join(parts, "."), nil
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
			if len(args) != 1 {
				return "", fmt.Errorf("print expects 1 arg")
			}
			return fmt.Sprintf("println(%s)", argStr), nil
		case "append":
			if len(args) != 2 {
				return "", fmt.Errorf("append expects 2 args")
			}
			return fmt.Sprintf("%s + %s", args[0], args[1]), nil
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects 1 arg")
			}
			return fmt.Sprintf("%s.sum()", args[0]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			return fmt.Sprintf("%s.average()", args[0]), nil
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			return fmt.Sprintf("%s.size", args[0]), nil
		default:
			return fmt.Sprintf("%s(%s)", name, argStr), nil
		}
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}
