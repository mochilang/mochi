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
	buf           bytes.Buffer
	indent        int
	needsAvg      bool
	needsStrconv  bool
	needsStrings  bool
	needsContains bool
	types         map[string]string
}

// New creates a new Go compiler.
func New() *Compiler {
	return &Compiler{types: make(map[string]string)}
}

// Compile translates the given program to Go.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsAvg = false
	c.needsStrconv = false
	c.needsStrings = false
	c.needsContains = false
	c.types = make(map[string]string)

	var funcs bytes.Buffer
	var body bytes.Buffer
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunTo(&funcs, s.Fun); err != nil {
				return nil, err
			}
			funcs.WriteByte('\n')
		} else {
			c.indent = 1
			if err := c.compileStmtTo(&body, s); err != nil {
				return nil, err
			}
			c.indent = 0
		}
	}

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
	if c.needsStrings {
		c.writeln("\"strings\"")
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
	if c.needsContains {
		c.writeln("func contains(slice []int, v int) bool {")
		c.indent++
		c.writeln("for _, n := range slice {")
		c.indent++
		c.writeln("if n == v {")
		c.indent++
		c.writeln("return true")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return false")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	c.buf.Write(funcs.Bytes())
	if funcs.Len() > 0 {
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
		return c.compileFun(s.Fun)
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
	if t := c.exprType(l.Value); t != "" {
		c.types[l.Name] = t
	}
	if l.Type != nil {
		typ, err := c.compileType(l.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s = %s", l.Name, typ, val))
	} else {
		c.writeLine(buf, fmt.Sprintf("%s := %s", l.Name, val))
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
		return nil
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if t := c.exprType(v.Value); t != "" {
		c.types[v.Name] = t
	}
	if v.Type != nil {
		typ, err := c.compileType(v.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s = %s", v.Name, typ, val))
	} else {
		c.writeLine(buf, fmt.Sprintf("%s := %s", v.Name, val))
	}
	return nil
}

func (c *Compiler) compileAssign(buf *bytes.Buffer, a *parser.AssignStmt) error {
	target := a.Name
	for _, idx := range a.Index {
		s, err := c.compileIndexOp(idx)
		if err != nil {
			return err
		}
		target += s
	}
	for _, f := range a.Field {
		target += "." + f.Name
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("%s = %s", target, val))
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	return c.compileFunTo(&c.buf, f)
}

func (c *Compiler) compileFunTo(buf *bytes.Buffer, f *parser.FunStmt) error {
	var params []string
	for _, p := range f.Params {
		typ, err := c.compileType(p.Type)
		if err != nil {
			return err
		}
		params = append(params, fmt.Sprintf("%s %s", p.Name, typ))
	}
	ret := ""
	if f.Return != nil {
		t, err := c.compileType(f.Return)
		if err != nil {
			return err
		}
		ret = " " + t
	}
	c.writeto(buf, fmt.Sprintf("func %s(%s)%s {", f.Name, strings.Join(params, ", "), ret))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeto(buf, "}")
	return nil
}

func (c *Compiler) compileReturn(buf *bytes.Buffer, r *parser.ReturnStmt) error {
	if r.Value != nil {
		val, err := c.compileExpr(r.Value)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("return %s", val))
	} else {
		c.writeLine(buf, "return")
	}
	return nil
}

func (c *Compiler) compileIf(buf *bytes.Buffer, i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	if i.ElseIf != nil {
		c.writeLine(buf, "} else {")
		c.indent++
		if err := c.compileIf(buf, i.ElseIf); err != nil {
			return err
		}
		c.indent--
		c.writeLine(buf, "}")
	} else if len(i.Else) > 0 {
		c.writeLine(buf, "} else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.compileStmtTo(buf, st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeLine(buf, "}")
	} else {
		c.writeLine(buf, "}")
	}
	return nil
}

func (c *Compiler) compileWhile(buf *bytes.Buffer, w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("for %s {", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	return nil
}

func (c *Compiler) compileFor(buf *bytes.Buffer, fr *parser.ForStmt) error {
	if fr.RangeEnd != nil {
		start, err := c.compileExpr(fr.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fr.RangeEnd)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("for %s := %s; %s < %s; %s++ {", fr.Name, start, fr.Name, end, fr.Name))
		c.indent++
		for _, st := range fr.Body {
			if err := c.compileStmtTo(buf, st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeLine(buf, "}")
		return nil
	}
	src, err := c.compileExpr(fr.Source)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("for _, %s := range %s {", fr.Name, src))
	c.indent++
	for _, st := range fr.Body {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	leftVal, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := leftVal
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		if op.Op == "in" {
			// determine container type if possible
			var rightName string
			if op.Right.Target.Selector != nil && len(op.Right.Ops) == 0 {
				rightName = op.Right.Target.Selector.Root
			}
			contType := c.types[rightName]
			if contType == "string" || strings.HasPrefix(contType, "string") {
				c.needsStrings = true
				res = fmt.Sprintf("strings.Contains(%s, %s)", r, res)
			} else if strings.HasPrefix(contType, "map[") {
				res = fmt.Sprintf("func() bool { _, ok := %s[%s]; return ok }()", r, res)
			} else {
				c.needsContains = true
				res = fmt.Sprintf("contains(%s, %s)", r, res)
			}
		} else {
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
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
			idx, err := c.compileIndexOp(op.Index)
			if err != nil {
				return "", err
			}
			val += idx
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, string) {
	keyT, valT := mapLitTypes(m)
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, err := c.compileExpr(it.Key)
		if err != nil {
			return "", ""
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", ""
		}
		items[i] = fmt.Sprintf("%s: %s", k, v)
	}
	expr := fmt.Sprintf("map[%s]%s{%s}", keyT, valT, strings.Join(items, ", "))
	return expr, fmt.Sprintf("map[%s]%s", keyT, valT)
}

func (c *Compiler) compileIndexOp(i *parser.IndexOp) (string, error) {
	if i == nil {
		return "", fmt.Errorf("nil index")
	}
	if i.Colon != nil {
		start := ""
		if i.Start != nil {
			s, err := c.compileExpr(i.Start)
			if err != nil {
				return "", err
			}
			start = s
		}
		end := ""
		if i.End != nil {
			s, err := c.compileExpr(i.End)
			if err != nil {
				return "", err
			}
			end = s
		}
		return fmt.Sprintf("[%s:%s]", start, end), nil
	}
	if i.Start == nil {
		return "[]", nil
	}
	s, err := c.compileExpr(i.Start)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("[%s]", s), nil
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
		expr, typ := c.compileMapLiteral(p.Map)
		_ = typ
		return expr, nil
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
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("len(%s)", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		c.needsStrconv = true
		return fmt.Sprintf("strconv.Itoa(%s)", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("%s[%s:%s]", args[0], args[1], args[2]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("func() int { s := 0; for _, n := range %s { s += n }; return s }()", args[0]), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		return fmt.Sprintf("func() int { m := %s[0]; for _, v := range %s[1:] { if v < m { m = v } }; return m }()", args[0], args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("func() int { m := %s[0]; for _, v := range %s[1:] { if v > m { m = v } }; return m }()", args[0], args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, argStr), nil
	}
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

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeto(buf *bytes.Buffer, s string) {
	for i := 0; i < c.indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString(s)
	buf.WriteByte('\n')
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

func (c *Compiler) exprType(e *parser.Expr) string {
	if e == nil || e.Binary == nil {
		return ""
	}
	return c.unaryType(e.Binary.Left)
}

func (c *Compiler) unaryType(u *parser.Unary) string {
	return c.postfixType(u.Value)
}

func (c *Compiler) postfixType(p *parser.PostfixExpr) string {
	if len(p.Ops) > 0 {
		return ""
	}
	return c.primaryType(p.Target)
}

func (c *Compiler) primaryType(p *parser.Primary) string {
	switch {
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return "string"
		}
		if p.Lit.Int != nil {
			return "int"
		}
	case p.List != nil:
		return "[]int"
	case p.Map != nil:
		keyT, valT := mapLitTypes(p.Map)
		return fmt.Sprintf("map[%s]%s", keyT, valT)
	}
	return ""
}

func mapLitTypes(m *parser.MapLiteral) (string, string) {
	if len(m.Items) == 0 {
		return "string", "int"
	}
	kp := m.Items[0].Key.Binary.Left.Value.Target
	vp := m.Items[0].Value.Binary.Left.Value.Target

	keyT := primaryPrimitiveType(kp)
	if keyT == "" {
		keyT = "string"
	}
	var valT string
	if vp.Map != nil {
		k2, v2 := mapLitTypes(vp.Map)
		valT = fmt.Sprintf("map[%s]%s", k2, v2)
	} else if vp.List != nil {
		valT = "[]int"
	} else {
		valT = primaryPrimitiveType(vp)
		if valT == "" {
			valT = "int"
		}
	}
	return keyT, valT
}

func primaryPrimitiveType(p *parser.Primary) string {
	if p.Lit != nil {
		if p.Lit.Str != nil {
			return "string"
		}
		if p.Lit.Int != nil {
			return "int"
		}
	}
	return ""
}
