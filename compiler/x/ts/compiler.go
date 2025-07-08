//go:build slow

package tscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

// Compiler translates a limited subset of Mochi into TypeScript.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	tmp    int
}

// New returns a new Compiler.
func New() *Compiler {
	return &Compiler{}
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("_tmp%d", c.tmp)
}

// Compile converts the parsed Mochi program into TypeScript source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	for _, st := range prog.Statements {
		if err := c.stmt(st); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.expr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let %s = %s;", s.Let.Name, val))
	case s.Var != nil:
		val, err := c.expr(s.Var.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let %s = %s;", s.Var.Name, val))
	case s.Return != nil:
		val, err := c.expr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
	case s.Expr != nil:
		val, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(val + ";")
	case s.If != nil:
		return c.ifStmt(s.If)
	case s.While != nil:
		cond, err := c.expr(s.While.Cond)
		if err != nil {
			return err
		}
		c.writeln("while (" + cond + ") {")
		c.indent++
		for _, st := range s.While.Body {
			if err := c.stmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.For != nil:
		return c.forStmt(s.For)
	case s.Fun != nil:
		return c.funStmt(s.Fun)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Type != nil:
		return c.typeDecl(s.Type)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) ifStmt(i *parser.IfStmt) error {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range i.Then {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else != nil {
		c.writeln("} else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.stmt(st); err != nil {
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

func (c *Compiler) forStmt(f *parser.ForStmt) error {
	src, err := c.expr(f.Source)
	if err != nil {
		return err
	}
	if f.RangeEnd != nil {
		end, err := c.expr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (let %s = %s; %s < %s; %s++) {", f.Name, src, f.Name, end, f.Name))
	} else {
		c.writeln(fmt.Sprintf("for (const %s of %s) {", f.Name, src))
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) funStmt(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("function %s(%s) {", f.Name, strings.Join(params, ", ")))
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) typeDecl(td *parser.TypeDecl) error {
	if len(td.Members) > 0 && len(td.Variants) == 0 {
		fields := make([]string, 0, len(td.Members))
		for _, m := range td.Members {
			if m.Field != nil {
				fields = append(fields, fmt.Sprintf("%s: any;", m.Field.Name))
			}
		}
		c.writeln(fmt.Sprintf("type %s = { %s };", td.Name, strings.Join(fields, " ")))
		return nil
	}
	return fmt.Errorf("unsupported type declaration")
}

func (c *Compiler) funExpr(fe *parser.FunExpr) (string, error) {
	params := make([]string, len(fe.Params))
	for i, p := range fe.Params {
		params[i] = p.Name
	}
	if fe.ExprBody != nil {
		body, err := c.expr(fe.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("unsupported function literal")
}

func (c *Compiler) queryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil || len(q.Joins) > 0 {
		return "", fmt.Errorf("query features not supported")
	}
	src, err := c.expr(q.Source)
	if err != nil {
		return "", err
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.expr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		return "", err
	}
	var whereStr, sortStr, skipStr, takeStr string
	if q.Where != nil {
		whereStr, err = c.expr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		sortStr, err = c.expr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skipStr, err = c.expr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeStr, err = c.expr(q.Take)
		if err != nil {
			return "", err
		}
	}

	var b bytes.Buffer
	indent := 0
	writeln := func(s string) {
		for i := 0; i < indent; i++ {
			b.WriteString("  ")
		}
		b.WriteString(s)
		b.WriteByte('\n')
	}
	res := c.newTmp()
	writeln("(() => {")
	indent++
	writeln("const " + res + " = [];")
	writeln(fmt.Sprintf("for (const %s of %s) {", q.Var, src))
	indent++
	for i, fs := range fromSrcs {
		writeln(fmt.Sprintf("for (const %s of %s) {", q.Froms[i].Var, fs))
		indent++
	}
	if whereStr != "" {
		writeln("if (!(" + whereStr + ")) continue;")
	}
	if sortStr != "" {
		writeln(res + ".push({item: " + sel + ", key: " + sortStr + "});")
	} else {
		writeln(res + ".push(" + sel + ");")
	}
	for range q.Froms {
		indent--
		writeln("}")
	}
	indent--
	writeln("}")
	writeln("let res = " + res + ";")
	if sortStr != "" {
		writeln("res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);")
	}
	if skipStr != "" || takeStr != "" {
		start := "0"
		if skipStr != "" {
			start = skipStr
		}
		end := "res.length"
		if takeStr != "" {
			if skipStr != "" {
				end = "(" + skipStr + " + " + takeStr + ")"
			} else {
				end = takeStr
			}
		}
		writeln(fmt.Sprintf("res = res.slice(%s, %s);", start, end))
	}
	writeln("return res;")
	indent--
	writeln("})()")
	return b.String(), nil
}

func (c *Compiler) ifExpr(i *parser.IfExpr) (string, error) {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.expr(i.Then)
	if err != nil {
		return "", err
	}
	var elseVal string
	if i.ElseIf != nil {
		elseVal, err = c.ifExpr(i.ElseIf)
		if err != nil {
			return "", err
		}
	} else if i.Else != nil {
		elseVal, err = c.expr(i.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseVal = "undefined"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenVal, elseVal), nil
}

func (c *Compiler) expr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.binary(e.Binary)
}

func (c *Compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	result := left
	for _, op := range b.Right {
		r, err := c.postfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "union":
			result = fmt.Sprintf("%s.concat(%s)", result, r)
		case "except":
			return "", fmt.Errorf("except not supported")
		case "intersect":
			return "", fmt.Errorf("intersect not supported")
		case "in":
			result = fmt.Sprintf("(%s.includes(%s))", r, result)
		default:
			result = fmt.Sprintf("(%s %s %s)", result, op.Op, r)
		}
	}
	return result, nil
}

func (c *Compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			val = fmt.Sprintf("(%s%s)", op, val)
		default:
			return "", fmt.Errorf("unary %s unsupported", op)
		}
	}
	return val, nil
}

func (c *Compiler) postfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
		} else if op.Index != nil {
			idx, err := c.expr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s[%s]", val, idx)
		} else if op.Field != nil {
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
		} else if op.Cast != nil {
			// ignore types
		}
	}
	return val, nil
}

func (c *Compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprint(*p.Lit.Int), nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.expr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, m := range p.Map.Items {
			k, err := c.expr(m.Key)
			if err != nil {
				return "", err
			}
			v, err := c.expr(m.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", strings.Trim(k, "\""), v)
		}
		return "{" + strings.Join(items, ", ") + "}", nil
	case p.Group != nil:
		v, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	case p.FunExpr != nil:
		return c.funExpr(p.FunExpr)
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.expr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 1 {
				return fmt.Sprintf("console.log(%s)", args[0]), nil
			}
			return fmt.Sprintf("console.log(%s)", strings.Join(args, ", ")), nil
		case "append":
			if len(args) == 2 {
				return fmt.Sprintf("[...%s, %s]", args[0], args[1]), nil
			}
			return "", fmt.Errorf("append expects 2 args")
		case "avg":
			if len(args) == 1 {
				return fmt.Sprintf("(%s.reduce((a,b)=>a+b,0)/%s.length)", args[0], args[0]), nil
			}
			return "", fmt.Errorf("avg expects 1 arg")
		case "sum":
			if len(args) == 1 {
				return fmt.Sprintf("(%s.reduce((a,b)=>a+b,0))", args[0]), nil
			}
			return "", fmt.Errorf("sum expects 1 arg")
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
		}
	case p.Query != nil:
		return c.queryExpr(p.Query)
	case p.If != nil:
		return c.ifExpr(p.If)
	case p.Selector != nil:
		return strings.Join(append([]string{p.Selector.Root}, p.Selector.Tail...), "."), nil
	}
	return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
}
