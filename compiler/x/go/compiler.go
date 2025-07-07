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
	mapVars      map[string]bool
}

// New creates a new Go compiler.
func New() *Compiler {
	return &Compiler{mapVars: map[string]bool{}}
}

// Compile translates the given program to Go.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsAvg = false
	c.needsStrconv = false

	var body bytes.Buffer
	var typeBuf bytes.Buffer
	// compile statements into body buffer, collect type declarations separately
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(&typeBuf, s.Type); err != nil {
				return nil, err
			}
		} else {
			if err := c.compileStmtTo(&body, s); err != nil {
				return nil, err
			}
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
	c.indent--
	c.writeln(")")
	c.writeln("")

	if typeBuf.Len() > 0 {
		c.buf.Write(typeBuf.Bytes())
		c.writeln("")
	}

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

	code := c.buf.Bytes()
	code = bytes.TrimLeft(code, "\n")
	return code, nil
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
	case s.For != nil:
		return c.compileFor(buf, s.For)
	case s.While != nil:
		return c.compileWhile(buf, s.While)
	case s.Type != nil:
		return c.compileTypeDecl(buf, s.Type)
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
	if c.isMapExpr(l.Value) {
		if c.mapVars == nil {
			c.mapVars = map[string]bool{}
		}
		c.mapVars[l.Name] = true
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
	if c.isMapExpr(v.Value) {
		if c.mapVars == nil {
			c.mapVars = map[string]bool{}
		}
		c.mapVars[v.Name] = true
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
	if len(a.Index) > 0 || len(a.Field) > 0 {
		return fmt.Errorf("assignment with index/field not supported")
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	if c.isMapExpr(a.Value) {
		if c.mapVars == nil {
			c.mapVars = map[string]bool{}
		}
		c.mapVars[a.Name] = true
	}
	c.writeLine(buf, fmt.Sprintf("%s = %s", a.Name, val))
	return nil
}

func (c *Compiler) compileFun(buf *bytes.Buffer, f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		typ := "interface{}"
		if p.Type != nil {
			if t, err := c.compileType(p.Type); err == nil {
				typ = t
			}
		}
		params[i] = fmt.Sprintf("%s %s", p.Name, typ)
	}
	ret := ""
	if f.Return != nil {
		if t, err := c.compileType(f.Return); err == nil && t != "void" {
			ret = " " + t
		}
	}
	c.writeLine(buf, fmt.Sprintf("func %s(%s)%s {", f.Name, join(params, ", "), ret))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	c.writeln("")
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

func (c *Compiler) compileIf(buf *bytes.Buffer, st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeLine(buf, "if "+cond+" {")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmtTo(buf, s); err != nil {
			return err
		}
	}
	c.indent--
	if st.ElseIf != nil {
		c.writeLine(buf, "} else ")
		return c.compileIf(buf, st.ElseIf)
	} else if st.Else != nil {
		c.writeLine(buf, "} else {")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmtTo(buf, s); err != nil {
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
		c.writeLine(buf, fmt.Sprintf("for %s := %s; %s < %s; %s++ {", name, start, name, end, name))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		if c.isMapExpr(f.Source) || c.mapVars[src] {
			c.writeLine(buf, fmt.Sprintf("for %s := range %s {", name, src))
		} else {
			c.writeLine(buf, fmt.Sprintf("for _, %s := range %s {", name, src))
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
		res = fmt.Sprintf("%s %s %s", res, op.Op, r)
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
	prev := p.Target
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
			} else if prev != nil && prev.Map != nil {
				fields := make([]string, len(prev.Map.Items))
				for i, it := range prev.Map.Items {
					v, err := c.compileExpr(it.Value)
					if err != nil {
						return "", err
					}
					k, err := c.compileExpr(it.Key)
					if err != nil {
						return "", err
					}
					k = strings.Trim(k, "\"")
					fields[i] = fmt.Sprintf("%s: %s", capitalize(k), v)
				}
				val = fmt.Sprintf("%s{%s}", typ, join(fields, ", "))
			} else {
				val = fmt.Sprintf("(%s)(%s)", typ, val)
			}
			prev = nil
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
			name += "." + capitalize(t)
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
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
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

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	fields := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", capitalize(f.Name), v)
	}
	return fmt.Sprintf("%s{%s}", s.Name, join(fields, ", ")), nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, err := c.compileExpr(it.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("%s: %s", k, v)
	}
	return fmt.Sprintf("map[interface{}]interface{}{%s}", join(items, ", ")), nil
}

func (c *Compiler) compileTypeDecl(buf *bytes.Buffer, t *parser.TypeDecl) error {
	if len(t.Members) == 0 {
		return fmt.Errorf("unsupported type decl")
	}
	c.writeLine(buf, fmt.Sprintf("type %s struct {", t.Name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			typ, err := c.compileType(m.Field.Type)
			if err != nil {
				return err
			}
			c.writeLine(buf, fmt.Sprintf("%s %s", capitalize(m.Field.Name), typ))
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	c.writeln("")
	return nil
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		typ := "interface{}"
		if p.Type != nil {
			if t, err := c.compileType(p.Type); err == nil {
				typ = t
			}
		}
		params[i] = fmt.Sprintf("%s %s", p.Name, typ)
	}
	ret := ""
	if f.Return != nil {
		if t, err := c.compileType(f.Return); err == nil && t != "void" {
			ret = " " + t
		}
	}
	var body bytes.Buffer
	if f.ExprBody != nil {
		expr, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		body.WriteString("return " + expr + "\n")
	} else {
		for _, st := range f.BlockBody {
			if err := c.compileStmtTo(&body, st); err != nil {
				return "", err
			}
		}
	}
	b := body.String()
	b = indentLines(b, c.indent+1)
	return fmt.Sprintf("func(%s)%s {\n%s%send }", join(params, ", "), ret, b, strings.Repeat("    ", c.indent)), nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	elseVal := "nil"
	if ie.ElseIf != nil {
		elseVal, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseVal, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(func() interface{} { if %s { return %s } else { return %s } })()", cond, thenVal, elseVal), nil
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return c.mapVars[p.Target.Selector.Root]
	}
	return false
}

func capitalize(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}

func indentLines(s string, indent int) string {
	if s == "" {
		return s
	}
	pad := strings.Repeat("    ", indent)
	lines := strings.Split(s, "\n")
	for i, line := range lines {
		if line != "" {
			lines[i] = pad + line
		}
	}
	return strings.Join(lines, "\n")
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
