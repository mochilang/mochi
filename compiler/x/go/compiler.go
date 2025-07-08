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
	needsSum      bool
	needsMin      bool
	needsMax      bool
	needsContains bool
	needsStrconv  bool
	needsStrings  bool
}

// New creates a new Go compiler.
func New() *Compiler {
	return &Compiler{}
}

// Compile translates the given program to Go.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsAvg = false
	c.needsSum = false
	c.needsMin = false
	c.needsMax = false
	c.needsContains = false
	c.needsStrconv = false
	c.needsStrings = false

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

	if c.needsSum {
		c.writeln("func sum(nums []int) int {")
		c.indent++
		c.writeln("var s int")
		c.writeln("for _, n := range nums {")
		c.indent++
		c.writeln("s += n")
		c.indent--
		c.writeln("}")
		c.writeln("return s")
		c.indent--
		c.writeln("}")
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

	if c.needsMin {
		c.writeln("func min(nums []int) int {")
		c.indent++
		c.writeln("if len(nums) == 0 {")
		c.indent++
		c.writeln("panic(\"empty slice\")")
		c.indent--
		c.writeln("}")
		c.writeln("m := nums[0]")
		c.writeln("for _, n := range nums[1:] {")
		c.indent++
		c.writeln("if n < m {")
		c.indent++
		c.writeln("m = n")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return m")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	if c.needsMax {
		c.writeln("func max(nums []int) int {")
		c.indent++
		c.writeln("if len(nums) == 0 {")
		c.indent++
		c.writeln("panic(\"empty slice\")")
		c.indent--
		c.writeln("}")
		c.writeln("m := nums[0]")
		c.writeln("for _, n := range nums[1:] {")
		c.indent++
		c.writeln("if n > m {")
		c.indent++
		c.writeln("m = n")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return m")
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
		if idx.Colon != nil {
			start, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			end, err := c.compileExpr(idx.End)
			if err != nil {
				return err
			}
			target = fmt.Sprintf("%s[%s:%s]", target, start, end)
		} else {
			i, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			target = fmt.Sprintf("%s[%s]", target, i)
		}
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
		if op.Op == "in" {
			c.needsContains = true
			res = fmt.Sprintf("contains(%s, %s)", r, res)
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
		case op.Index != nil:
			if op.Index.Colon != nil {
				start, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				end, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				if strings.HasPrefix(val, "\"") {
					val = fmt.Sprintf("string([]rune(%s)[%s:%s])", val, start, end)
				} else {
					val = fmt.Sprintf("%s[%s:%s]", val, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if strings.HasPrefix(val, "\"") {
					val = fmt.Sprintf("string([]rune(%s)[%s])", val, idx)
				} else {
					val = fmt.Sprintf("%s[%s]", val, idx)
				}
			}
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			switch val {
			case "len", "count":
				if len(args) != 1 {
					return "", fmt.Errorf("%s expects 1 arg", val)
				}
				val = fmt.Sprintf("len(%s)", args[0])
			case "append":
				if len(args) != 2 {
					return "", fmt.Errorf("append expects 2 args")
				}
				val = fmt.Sprintf("append(%s, %s)", args[0], args[1])
			case "avg":
				c.needsAvg = true
				val = fmt.Sprintf("avg(%s)", join(args, ", "))
			case "sum":
				c.needsSum = true
				val = fmt.Sprintf("sum(%s)", join(args, ", "))
			case "min":
				c.needsMin = true
				val = fmt.Sprintf("min(%s)", join(args, ", "))
			case "max":
				c.needsMax = true
				val = fmt.Sprintf("max(%s)", join(args, ", "))
			case "substring":
				if len(args) != 3 {
					return "", fmt.Errorf("substring expects 3 args")
				}
				val = fmt.Sprintf("string([]rune(%s)[%s:%s])", args[0], args[1], args[2])
			case "str":
				if len(args) != 1 {
					return "", fmt.Errorf("str expects 1 arg")
				}
				c.needsStrconv = true
				val = fmt.Sprintf("strconv.Itoa(%s)", args[0])
			case "contains":
				if len(args) != 2 {
					return "", fmt.Errorf("contains expects 2 args")
				}
				c.needsContains = true
				val = fmt.Sprintf("contains(%s, %s)", args[0], args[1])
			default:
				if strings.HasSuffix(val, ".contains") && len(args) == 1 {
					c.needsStrings = true
					base := strings.TrimSuffix(val, ".contains")
					val = fmt.Sprintf("strings.Contains(%s, %s)", base, args[0])
				} else {
					val = fmt.Sprintf("%s(%s)", val, join(args, ", "))
				}
			}
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
		case op.Field != nil:
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
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
		typ := "[]int"
		if len(p.List.Elems) > 0 && isListLiteral(p.List.Elems[0]) {
			typ = "[][]int"
		}
		return typ + "{" + join(elems, ", ") + "}", nil
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
	case "sum":
		c.needsSum = true
		return fmt.Sprintf("sum(%s)", argStr), nil
	case "min":
		c.needsMin = true
		return fmt.Sprintf("min(%s)", argStr), nil
	case "max":
		c.needsMax = true
		return fmt.Sprintf("max(%s)", argStr), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("string([]rune(%s)[%s:%s])", args[0], args[1], args[2]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		c.needsStrconv = true
		return fmt.Sprintf("strconv.Itoa(%s)", args[0]), nil
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

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	if e.Binary.Left.Value.Target != nil && e.Binary.Left.Value.Target.List != nil {
		return true
	}
	return false
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
