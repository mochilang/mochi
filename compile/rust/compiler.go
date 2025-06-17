package rscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Rust source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Rust compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile returns Rust source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, stmt := range prog.Statements {
		if stmt.Fun != nil {
			if err := c.compileFun(stmt.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("fn main() {")
	c.indent++
	for _, stmt := range prog.Statements {
		if stmt.Fun == nil {
			if err := c.compileStmt(stmt); err != nil {
				return nil, err
			}
		}
	}
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("return %s;", val))
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s;", expr))
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	val := "Default::default()"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		val = v
	}
	name := sanitizeName(stmt.Name)
	c.writeln(fmt.Sprintf("let mut %s = %s;", name, val))
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	start, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	end := ""
	if stmt.RangeEnd != nil {
		end, err = c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
	}
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		c.writeln(fmt.Sprintf("for %s in %s..%s {", name, start, end))
	} else {
		if isStringLiteral(stmt.Source) {
			c.writeln(fmt.Sprintf("for %s in %s.chars() {", name, start))
		} else {
			c.writeln(fmt.Sprintf("for %s in %s {", name, start))
		}
	}
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fn " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(fmt.Sprintf("%s: %s", sanitizeName(p.Name), rustType(p.Type)))
	}
	c.buf.WriteString(")")
	if fun.Return != nil {
		c.buf.WriteString(" -> " + rustType(fun.Return))
	}
	c.buf.WriteString(" {\n")
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("%s %s %s", expr, op.Op, r)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		val = fmt.Sprintf("%s%s", op, val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
		} else if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s[%s as usize]", expr, idx)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("vec![%s]", strings.Join(elems, ", ")), nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		return "0", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return sanitizeName(p.Selector.Root), nil
		}
		parts := []string{sanitizeName(p.Selector.Root)}
		for _, t := range p.Selector.Tail {
			parts = append(parts, sanitizeName(t))
		}
		return strings.Join(parts, "."), nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", inner), nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")
       switch call.Func {
       case "print":
               if len(args) == 1 {
                       return fmt.Sprintf("println!(\"{}\", %s)", argStr), nil
               }
               fmtParts := make([]string, len(args))
               for i := range args {
                       fmtParts[i] = "{}"
               }
               fmtStr := strings.Join(fmtParts, " ")
               return fmt.Sprintf("println!(\"%s\", %s)", fmtStr, argStr), nil
       case "str":
               if len(args) == 1 {
                       return fmt.Sprintf("format!(\"{}\", %s)", argStr), nil
               }
               fmtParts := make([]string, len(args))
               for i := range args {
                       fmtParts[i] = "{}"
               }
               fmtStr := strings.Join(fmtParts, " ")
               return fmt.Sprintf("format!(\"%s\", %s)", fmtStr, argStr), nil
       case "len":
		if len(args) == 1 {
			return fmt.Sprintf("%s.len() as i32", args[0]), nil
		}
	}
	return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
}

func rustType(t *parser.TypeRef) string {
	if t == nil {
		return "()"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i32"
		case "float":
			return "f64"
		case "string":
			return "String"
		case "bool":
			return "bool"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return fmt.Sprintf("Vec<%s>", rustType(t.Generic.Args[0]))
		}
	}
	if t.Fun != nil {
		return "()" // not handled
	}
	return "()"
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 {
		return false
	}
	if p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil {
		return false
	}
	return true
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	res := b.String()
	if res == "" || !((res[0] >= 'A' && res[0] <= 'Z') || (res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
		res = "_" + res
	}
	return res
}
