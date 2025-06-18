package zigcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Zig source code (very small subset).
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

func New(env *types.Env) *Compiler { return &Compiler{env: env} }

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

// Compile converts a Mochi program to Zig.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// compile functions first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main body
	c.writeln("pub fn main() void {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s, false); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	// prepend import
	body := c.buf.String()
	c.buf.Reset()
	c.writeln("const std = @import(\"std\");")
	c.writeln("")
	c.buf.WriteString(body)
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := sanitizeName(fn.Name)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := c.zigType(p.Type)
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ)
	}
	c.writeln(fmt.Sprintf("fn %s(%s) [2]i32 {", name, strings.Join(params, ", ")))
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) zigType(t *parser.TypeRef) string {
	if t == nil {
		return "i32"
	}
	if t.Generic != nil && t.Generic.Name == "list" {
		return "[]const i32"
	}
	if t.Simple == nil {
		return "i32"
	}
	switch *t.Simple {
	case "int":
		return "i32"
	case "float":
		return "f64"
	case "bool":
		return "bool"
	case "string":
		return "[]const u8"
	}
	if t.Generic != nil && t.Generic.Name == "list" {
		return "[]const i32"
	}
	return "i32"
}

func (c *Compiler) compileStmt(s *parser.Statement, inFun bool) error {
	switch {
	case s.Let != nil:
		val := "0"
		if s.Let.Value != nil {
			v, err := c.compileExpr(s.Let.Value, false)
			if err != nil {
				return err
			}
			val = v
		}
		c.writeln(fmt.Sprintf("const %s = %s;", sanitizeName(s.Let.Name), val))
	case s.Return != nil:
		if isListLiteralExpr(s.Return.Value) {
			ll := s.Return.Value.Binary.Left.Value.Target.List
			v, err := c.compileListLiteral(ll, false)
			if err != nil {
				return err
			}
			c.writeln("return " + v + ";")
		} else {
			v, err := c.compileExpr(s.Return.Value, true)
			if err != nil {
				return err
			}
			c.writeln("return " + v + ";")
		}
	case s.For != nil:
		start, err := c.compileExpr(s.For.Source, false)
		if err != nil {
			return err
		}
		end := ""
		if s.For.RangeEnd != nil {
			end, err = c.compileExpr(s.For.RangeEnd, false)
			if err != nil {
				return err
			}
		}
		name := sanitizeName(s.For.Name)
		if s.For.RangeEnd != nil {
			c.writeln(fmt.Sprintf("for (%s .. %s) |%s| {", start, end, name))
		} else {
			c.writeln(fmt.Sprintf("for (%s) |%s| {", start, name))
		}
		c.indent++
		for _, st := range s.For.Body {
			if err := c.compileStmt(st, inFun); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.Expr != nil:
		v, err := c.compileExpr(s.Expr.Expr, false)
		if err != nil {
			return err
		}
		c.writeln(v + ";")
	case s.If != nil:
		return c.compileIf(s.If)
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond, false)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileStmt(st, true); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr, asReturn bool) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary, asReturn)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr, asReturn bool) (string, error) {
	left, err := c.compileUnary(b.Left, asReturn)
	if err != nil {
		return "", err
	}
	expr := left
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right, asReturn)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, right)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary, asReturn bool) (string, error) {
	val, err := c.compilePostfix(u.Value, asReturn)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("%s%s", u.Ops[i], val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr, asReturn bool) (string, error) {
	expr, err := c.compilePrimary(p.Target, asReturn)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start, false)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s[%s]", expr, idx)
		} else if op.Call != nil {
			expr, err = c.compileCallOp(expr, op.Call)
			if err != nil {
				return "", err
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compileCallOp(receiver string, call *parser.CallOp) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a, false)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", receiver, strings.Join(args, ", ")), nil
}

func (c *Compiler) compilePrimary(p *parser.Primary, asReturn bool) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.List != nil:
		return c.compileListLiteral(p.List, !asReturn)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group, asReturn)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "0", nil
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	if name == "len" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return arg + ".len", nil
	}
	if name == "print" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("std.debug.print(\"{}\\n\", .{%s})", arg), nil
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a, false)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		return strconv.FormatFloat(*l.Float, 'f', -1, 64), nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	}
	return "0", nil
}

func (c *Compiler) compileListLiteral(list *parser.ListLiteral, asRef bool) (string, error) {
	elems := make([]string, len(list.Elems))
	for i, e := range list.Elems {
		v, err := c.compileExpr(e, false)
		if err != nil {
			return "", err
		}
		elems[i] = fmt.Sprintf("@as(i32,@intCast(%s))", v)
	}
	if asRef {
		return fmt.Sprintf("&[_]i32{%s}", strings.Join(elems, ", ")), nil
	}
	return fmt.Sprintf("[_]i32{%s}", strings.Join(elems, ", ")), nil
}

func isListLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return isListLiteralUnary(e.Binary.Left)
}

func isListLiteralUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isListLiteralPostfix(u.Value)
}

func isListLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListLiteralPrimary(p.Target)
}

func isListLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.List != nil
}

var zigReserved = map[string]bool{
	"fn": true, "var": true, "const": true, "pub": true, "return": true,
	"for": true, "while": true, "if": true, "else": true,
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	if zigReserved[s] {
		s = "_" + s
	}
	return s
}
