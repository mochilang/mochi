//go:build slow

package gocode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

// Compiler translates a subset of Mochi to Go source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	needsAvg     bool
	needsStrconv bool
	varTypes     map[string]string
}

// New creates a new Go compiler.
func New() *Compiler {
	return &Compiler{varTypes: make(map[string]string)}
}

// Compile translates the given program to Go.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsAvg = false
	c.needsStrconv = false
	c.varTypes = make(map[string]string)

	var body bytes.Buffer
	c.indent = 1
	// compile statements into body buffer
	for _, s := range prog.Statements {
		if err := c.compileStmtTo(&body, s); err != nil {
			return nil, err
		}
	}
	c.indent = 0

	// write header and imports
	c.writeln("//go:build ignore")
	c.writeln("")
	c.writeln("package main")
	c.writeln("")
	c.writeln("import (")
	c.indent++
	c.writeln("\"fmt\"")
	if c.needsStrconv {
		c.writeln("\"strconv\"")
	}
	c.indent--
	c.writeln(")")
	c.writeln("")

	if c.needsAvg {
		c.writeln("func avg(nums []int) int {")
		c.indent++
		c.writeln("if len(nums) == 0 {")
		c.indent++
		c.writeln("return 0")
		c.indent--
		c.writeln("}")
		c.writeln("var sum int")
		c.writeln("for _, n := range nums {")
		c.indent++
		c.writeln("sum += n")
		c.indent--
		c.writeln("}")
		c.writeln("return sum / len(nums)")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	c.writeln("func main() {")
	c.indent++
	c.buf.Write(body.Bytes())
	c.indent--
	c.writeln("}")

	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	return c.compileStmtTo(&c.buf, s)
}

func (c *Compiler) compileStmtTo(buf *bytes.Buffer, s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(buf, s.Let)
	case s.Var != nil:
		return c.compileVar(buf, s.Var)
	case s.Assign != nil:
		return c.compileAssign(buf, s.Assign)
	case s.Fun != nil:
		return c.compileFun(buf, s.Fun)
	case s.Return != nil:
		return c.compileReturn(buf, s.Return)
	case s.If != nil:
		return c.compileIf(buf, s.If)
	case s.While != nil:
		return c.compileWhile(buf, s.While)
	case s.For != nil:
		return c.compileFor(buf, s.For)
	case s.Break != nil:
		c.writeLine(buf, "break")
		return nil
	case s.Continue != nil:
		c.writeLine(buf, "continue")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeLine(buf, expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(buf *bytes.Buffer, l *parser.LetStmt) error {
	if l.Value == nil {
		if l.Type == nil {
			return fmt.Errorf("let without value at line %d", l.Pos.Line)
		}
		typ, err := c.compileType(l.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s", l.Name, typ))
		return nil
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	if l.Type != nil {
		typ, err := c.compileType(l.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s = %s", l.Name, typ, val))
		c.recordVarType(l.Name, l.Type, l.Value)
	} else {
		c.writeLine(buf, fmt.Sprintf("%s := %s", l.Name, val))
		c.recordVarType(l.Name, nil, l.Value)
	}
	return nil
}

func (c *Compiler) compileVar(buf *bytes.Buffer, v *parser.VarStmt) error {
	if v.Value == nil {
		if v.Type == nil {
			return fmt.Errorf("var without value at line %d", v.Pos.Line)
		}
		typ, err := c.compileType(v.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s", v.Name, typ))
		c.recordVarType(v.Name, v.Type, nil)
		return nil
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type != nil {
		typ, err := c.compileType(v.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s = %s", v.Name, typ, val))
		c.recordVarType(v.Name, v.Type, v.Value)
	} else {
		c.writeLine(buf, fmt.Sprintf("%s := %s", v.Name, val))
		c.recordVarType(v.Name, nil, v.Value)
	}
	return nil
}

func (c *Compiler) compileAssign(buf *bytes.Buffer, a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	target := a.Name
	for _, idx := range a.Index {
		islice, err := c.compileIndex(idx)
		if err != nil {
			return err
		}
		target += islice
	}
	for _, f := range a.Field {
		target += "." + f.Name
	}
	c.writeLine(buf, fmt.Sprintf("%s = %s", target, val))
	return nil
}

func (c *Compiler) compileFun(buf *bytes.Buffer, f *parser.FunStmt) error {
	header := "func " + f.Name + "("
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		typ := "interface{}"
		if p.Type != nil {
			t, err := c.compileType(p.Type)
			if err != nil {
				return err
			}
			typ = t
		}
		params[i] = fmt.Sprintf("%s %s", p.Name, typ)
	}
	header += join(params, ", ") + ")"
	if f.Return != nil {
		rt, err := c.compileType(f.Return)
		if err != nil {
			return err
		}
		header += " " + rt
	}
	c.writeLine(buf, header+" {")
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmtTo(buf, s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	return nil
}

func (c *Compiler) compileReturn(buf *bytes.Buffer, r *parser.ReturnStmt) error {
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeLine(buf, "return "+val)
	return nil
}

func (c *Compiler) compileIf(buf *bytes.Buffer, i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeLine(buf, "if "+cond+" {")
	c.indent++
	for _, s := range i.Then {
		if err := c.compileStmtTo(buf, s); err != nil {
			return err
		}
	}
	c.indent--
	if i.ElseIf != nil {
		cond2, err := c.compileExpr(i.ElseIf.Cond)
		if err != nil {
			return err
		}
		c.writeLine(buf, "} else if "+cond2+" {")
		c.indent++
		for _, s := range i.ElseIf.Then {
			if err := c.compileStmtTo(buf, s); err != nil {
				return err
			}
		}
		c.indent--
	}
	if len(i.Else) > 0 {
		c.writeLine(buf, "} else {")
		c.indent++
		for _, s := range i.Else {
			if err := c.compileStmtTo(buf, s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeLine(buf, "}")
	return nil
}

func (c *Compiler) compileWhile(buf *bytes.Buffer, w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeLine(buf, "for "+cond+" {")
	c.indent++
	for _, s := range w.Body {
		if err := c.compileStmtTo(buf, s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	return nil
}

func (c *Compiler) compileFor(buf *bytes.Buffer, f *parser.ForStmt) error {
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		loopHeader := fmt.Sprintf("for %s := %s; %s < %s; %s++", f.Name, start, f.Name, end, f.Name)
		c.writeLine(buf, loopHeader+" {")
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		kind := c.varTypes[src]
		if kind == "map" {
			c.writeLine(buf, fmt.Sprintf("for %s := range %s {", f.Name, src))
		} else {
			c.writeLine(buf, fmt.Sprintf("for _, %s := range %s {", f.Name, src))
		}
	}
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmtTo(buf, s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	return nil
}

func (c *Compiler) compileIndex(idx *parser.IndexOp) (string, error) {
	if idx.Colon != nil { // slice start:end
		start := ""
		if idx.Start != nil {
			s, err := c.compileExpr(idx.Start)
			if err != nil {
				return "", err
			}
			start = s
		}
		end := ""
		if idx.End != nil {
			s, err := c.compileExpr(idx.End)
			if err != nil {
				return "", err
			}
			end = s
		}
		return fmt.Sprintf("[%s:%s]", start, end), nil
	}
	if idx.Start == nil {
		return "[]", nil
	}
	val, err := c.compileExpr(idx.Start)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("[%s]", val), nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
		default:
			return "", fmt.Errorf("unsupported operator %s", op.Op)
		}
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Cast != nil:
			typ, err := c.compileType(op.Cast.Type)
			if err != nil {
				return "", err
			}
			if typ == "int" && strings.HasPrefix(val, "\"") {
				c.needsStrconv = true
				val = fmt.Sprintf("func() int { v, _ := strconv.Atoi(%s); return v }()", val)
			} else {
				val = fmt.Sprintf("(%s)(%s)", typ, val)
			}
		case op.Index != nil:
			idx, err := c.compileIndex(op.Index)
			if err != nil {
				return "", err
			}
			val += idx
		case op.Field != nil:
			val += "." + op.Field.Name
		case op.Call != nil:
			savedIndent := c.indent
			c.indent = 0
			callExpr, err := c.compileCallExpr(val, op.Call)
			c.indent = savedIndent
			if err != nil {
				return "", err
			}
			val = callExpr
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[]int{" + join(elems, ", ") + "}", nil
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
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		return "map[string]int{" + join(items, ", ") + "}", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Group != nil:
		s, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", s), nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	argStr := join(args, ", ")
	switch call.Func {
	case "print":
		return fmt.Sprintf("fmt.Println(%s)", argStr), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("append(%s, %s)", args[0], args[1]), nil
	case "avg":
		c.needsAvg = true
		return fmt.Sprintf("avg(%s)", argStr), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("len(%s)", args[0]), nil
	default:
		return "", fmt.Errorf("unsupported function %s", call.Func)
	}
}

func (c *Compiler) compileCallExpr(fn string, call *parser.CallOp) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	return fmt.Sprintf("%s(%s)", fn, join(args, ", ")), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		return fmt.Sprintf("%t", bool(*l.Bool))
	default:
		return "nil"
	}
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	var buf bytes.Buffer
	buf.WriteString("func(")
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		typ := "interface{}"
		if p.Type != nil {
			t, err := c.compileType(p.Type)
			if err != nil {
				return "", err
			}
			typ = t
		}
		params[i] = fmt.Sprintf("%s %s", p.Name, typ)
	}
	buf.WriteString(join(params, ", "))
	buf.WriteString(")")
	if f.Return != nil {
		rt, err := c.compileType(f.Return)
		if err != nil {
			return "", err
		}
		buf.WriteString(" " + rt)
	}
	buf.WriteString(" {")
	if f.ExprBody != nil {
		body, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		buf.WriteString("return " + body)
	} else {
		buf.WriteString("\n")
		c.indent++
		var inner bytes.Buffer
		for _, s := range f.BlockBody {
			if err := c.compileStmtTo(&inner, s); err != nil {
				return "", err
			}
		}
		buf.Write(inner.Bytes())
		c.indent--
	}
	buf.WriteString("}")
	return buf.String(), nil
}

func (c *Compiler) compileIfExpr(i *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(i.Then)
	if err != nil {
		return "", err
	}
	var elsePart string
	if i.ElseIf != nil {
		elseExpr, err := c.compileIfExpr(i.ElseIf)
		if err != nil {
			return "", err
		}
		elsePart = elseExpr
	} else if i.Else != nil {
		elseExpr, err := c.compileExpr(i.Else)
		if err != nil {
			return "", err
		}
		elsePart = elseExpr
	} else {
		elsePart = ""
	}
	buf := fmt.Sprintf("func() interface{} { if %s { return %s }", cond, thenExpr)
	if elsePart != "" {
		buf += fmt.Sprintf(" else { return %s }", elsePart)
	}
	buf += " }()"
	return buf, nil
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeLine(buf *bytes.Buffer, s string) {
	for i := 0; i < c.indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString(s)
	buf.WriteByte('\n')
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil || t.Simple == nil {
		return "", fmt.Errorf("unsupported type")
	}
	return *t.Simple, nil
}

func join(parts []string, sep string) string {
	if len(parts) == 0 {
		return ""
	}
	out := parts[0]
	for _, p := range parts[1:] {
		out += sep + p
	}
	return out
}

func (c *Compiler) recordVarType(name string, typ *parser.TypeRef, val *parser.Expr) {
	if typ != nil && typ.Simple != nil {
		if strings.HasPrefix(*typ.Simple, "map") {
			c.varTypes[name] = "map"
			return
		}
		if strings.HasPrefix(*typ.Simple, "list") {
			c.varTypes[name] = "list"
			return
		}
	}
	if val != nil {
		switch exprKind(val) {
		case "list", "map":
			c.varTypes[name] = exprKind(val)
		}
	}
}

func exprKind(e *parser.Expr) string {
	p := primaryFromExpr(e)
	if p == nil {
		return ""
	}
	if p.List != nil {
		return "list"
	}
	if p.Map != nil {
		return "map"
	}
	return ""
}

func primaryFromExpr(e *parser.Expr) *parser.Primary {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return nil
	}
	pexpr := u.Value
	if pexpr == nil || len(pexpr.Ops) > 0 {
		return nil
	}
	return pexpr.Target
}
