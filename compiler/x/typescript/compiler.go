//go:build slow

package typescriptcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

// Compiler translates a limited subset of Mochi into TypeScript.
type Compiler struct {
	buf                bytes.Buffer
	indent             int
	tmp                int
	needContainsHelper bool
	funParams          map[string]int
}

// New returns a new Compiler.
func New() *Compiler {
	return &Compiler{funParams: make(map[string]int)}
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("_tmp%d", c.tmp)
}

// Compile converts the parsed Mochi program into TypeScript source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needContainsHelper = false
	c.funParams = make(map[string]int)
	for _, st := range prog.Statements {
		if st.Fun != nil {
			c.funParams[st.Fun.Name] = len(st.Fun.Params)
		}
	}
	for _, st := range prog.Statements {
		if err := c.stmt(st); err != nil {
			return nil, err
		}
	}
	code := c.buf.Bytes()
	if c.needContainsHelper {
		helper := "function contains(a: any, b: any) {\n" +
			"  if (Array.isArray(a) || typeof a === \"string\") return a.includes(b);\n" +
			"  return Object.prototype.hasOwnProperty.call(a, b);\n" +
			"}\n"
		code = append([]byte(helper), code...)
	}
	return code, nil
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
		if s.Let.Value == nil {
			c.writeln(fmt.Sprintf("let %s = null;", s.Let.Name))
		} else {
			val, err := c.expr(s.Let.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let %s = %s;", s.Let.Name, val))
		}
	case s.Var != nil:
		if s.Var.Value == nil {
			c.writeln(fmt.Sprintf("let %s = null;", s.Var.Name))
		} else {
			val, err := c.expr(s.Var.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let %s = %s;", s.Var.Name, val))
		}
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
	case s.Assign != nil:
		target, err := c.assignTarget(s.Assign)
		if err != nil {
			return err
		}
		val, err := c.expr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s;", target, val))
	case s.Test != nil:
		for _, st := range s.Test.Body {
			if st.Expect != nil {
				exp, err := c.expr(st.Expect.Value)
				if err != nil {
					return err
				}
				msg := fmt.Sprintf("%s failed", s.Test.Name)
				c.writeln(fmt.Sprintf("if (!(%s)) { throw new Error(%q); }", exp, msg))
			} else {
				if err := c.stmt(st); err != nil {
					return err
				}
			}
		}
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
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("const %s = %s;", tmp, src))
		c.writeln(fmt.Sprintf("for (const %s of (Array.isArray(%s) ? %s : Object.keys(%s))) {", f.Name, tmp, tmp, tmp))
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

func (c *Compiler) assignTarget(a *parser.AssignStmt) (string, error) {
	target := a.Name
	for _, idx := range a.Index {
		if idx.Colon != nil || idx.End != nil || idx.Colon2 != nil || idx.Step != nil {
			return "", fmt.Errorf("slice assignment not supported")
		}
		if idx.Start == nil {
			return "", fmt.Errorf("missing index expression")
		}
		v, err := c.expr(idx.Start)
		if err != nil {
			return "", err
		}
		target = fmt.Sprintf("%s[%s]", target, v)
	}
	for _, f := range a.Field {
		target = fmt.Sprintf("%s.%s", target, f.Name)
	}
	return target, nil
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

func (c *Compiler) matchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.expr(m.Target)
	if err != nil {
		return "", err
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
	tmp := c.newTmp()
	writeln("(() => {")
	indent++
	writeln(fmt.Sprintf("const %s = %s;", tmp, target))
	writeln("let _res;")
	writeln(fmt.Sprintf("switch (%s) {", tmp))
	indent++
	hasDefault := false
	for _, cs := range m.Cases {
		if isUnderscoreExpr(cs.Pattern) {
			res, err := c.expr(cs.Result)
			if err != nil {
				return "", err
			}
			writeln("default:")
			indent++
			writeln(fmt.Sprintf("_res = %s;", res))
			writeln("break;")
			indent--
			hasDefault = true
			continue
		}
		pat, err := c.expr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.expr(cs.Result)
		if err != nil {
			return "", err
		}
		writeln(fmt.Sprintf("case %s:", pat))
		indent++
		writeln(fmt.Sprintf("_res = %s;", res))
		writeln("break;")
		indent--
	}
	if !hasDefault {
		writeln("default:")
		indent++
		writeln("_res = undefined;")
		writeln("break;")
		indent--
	}
	indent--
	writeln("}")
	writeln("return _res;")
	indent--
	writeln("})()")
	return b.String(), nil
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
			if op.All {
				result = fmt.Sprintf("%s.concat(%s)", result, r)
			} else {
				result = fmt.Sprintf("Array.from(new Set([...%s, ...%s]))", result, r)
			}
		case "except":
			result = fmt.Sprintf("%s.filter(x => !%s.includes(x))", result, r)
		case "intersect":
			result = fmt.Sprintf("%s.filter(x => %s.includes(x))", result, r)
		case "in":
			c.needContainsHelper = true
			result = fmt.Sprintf("contains(%s, %s)", r, result)
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
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		if op.Field != nil && op.Field.Name == "contains" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
			val = fmt.Sprintf("%s.includes", val)
			continue
		}
		if op.Call != nil {
			if strings.HasSuffix(val, ".contains") {
				val = strings.TrimSuffix(val, ".contains") + ".includes"
			}
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if arity, ok := c.funParams[val]; ok && len(args) < arity {
				val = fmt.Sprintf("(...args) => %s(%s, ...args)", val, strings.Join(args, ", "))
			} else {
				val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			}
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
	case p.Struct != nil:
		fields := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.expr(f.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s: %s", f.Name, v)
		}
		return "{" + strings.Join(fields, ", ") + "}", nil
	case p.Group != nil:
		v, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	case p.FunExpr != nil:
		return c.funExpr(p.FunExpr)
	case p.Match != nil:
		return c.matchExpr(p.Match)
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
		case "count":
			if len(args) == 1 {
				return fmt.Sprintf("%s.length", args[0]), nil
			}
			return "", fmt.Errorf("count expects 1 arg")
		case "exists":
			if len(args) == 1 {
				return fmt.Sprintf("(%s.length > 0)", args[0]), nil
			}
			return "", fmt.Errorf("exists expects 1 arg")
		case "len":
			if len(args) == 1 {
				return fmt.Sprintf("%s.length", args[0]), nil
			}
			return "", fmt.Errorf("len expects 1 arg")
		case "values":
			if len(args) == 1 {
				return fmt.Sprintf("Object.values(%s)", args[0]), nil
			}
			return "", fmt.Errorf("values expects 1 arg")
		case "contains":
			if len(args) == 2 {
				c.needContainsHelper = true
				return fmt.Sprintf("contains(%s, %s)", args[0], args[1]), nil
			}
			return "", fmt.Errorf("contains expects 2 args")
		case "str":
			if len(args) == 1 {
				return fmt.Sprintf("String(%s)", args[0]), nil
			}
			return "", fmt.Errorf("str expects 1 arg")
		case "substring":
			if len(args) == 3 {
				return fmt.Sprintf("%s.substring(%s, %s)", args[0], args[1], args[2]), nil
			}
			return "", fmt.Errorf("substring expects 3 args")
		case "min":
			if len(args) == 1 {
				return fmt.Sprintf("Math.min(...%s)", args[0]), nil
			}
			return "", fmt.Errorf("min expects 1 arg")
		case "max":
			if len(args) == 1 {
				return fmt.Sprintf("Math.max(...%s)", args[0]), nil
			}
			return "", fmt.Errorf("max expects 1 arg")
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

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}
