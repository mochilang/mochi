//go:build slow

package fscode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
)

// Compiler is a very small F# code generator that supports only a
// subset of Mochi. It is intentionally minimal and only handles the
// constructs required by a few simple programs.
type Compiler struct {
	buf    bytes.Buffer
	indent int
}

func defaultValue(typ string) string {
	switch typ {
	case "int":
		return "0"
	case "float":
		return "0.0"
	case "string":
		return "\"\""
	case "bool":
		return "false"
	default:
		return "()"
	}
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "", fmt.Errorf("unsupported type")
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int", "float", "string", "bool":
			return *t.Simple, nil
		case "void":
			return "unit", nil
		default:
			return *t.Simple, nil
		}
	}
	return "", fmt.Errorf("unsupported type")
}

// New creates a new F# compiler instance.
func New() *Compiler { return &Compiler{} }

// Compile translates the given Mochi program into F# source code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.writeln("open System")
	c.writeln("")
	for _, s := range p.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.Expr != nil:
		// special-case print calls which already emit printfn
		if s.Expr.Expr != nil && s.Expr.Expr.Binary != nil &&
			s.Expr.Expr.Binary.Left != nil && s.Expr.Expr.Binary.Left.Value != nil &&
			s.Expr.Expr.Binary.Left.Value.Target != nil && s.Expr.Expr.Binary.Left.Value.Target.Call != nil &&
			s.Expr.Expr.Binary.Left.Value.Target.Call.Func == "print" {
			expr, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return err
			}
			c.writeln(expr)
			return nil
		}
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("printfn \"%%A\" (%s)", expr))
		return nil
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIfStmt(s.If)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	var typ string
	var err error
	if l.Type != nil {
		typ, err = c.compileType(l.Type)
		if err != nil {
			return err
		}
	}
	var val string
	if l.Value != nil {
		val, err = c.compileExpr(l.Value)
		if err != nil {
			return err
		}
	} else {
		if typ == "" {
			return fmt.Errorf("let without value at line %d", l.Pos.Line)
		}
		val = defaultValue(typ)
	}
	if typ != "" {
		c.writeln(fmt.Sprintf("let %s: %s = %s", l.Name, typ, val))
	} else {
		c.writeln(fmt.Sprintf("let %s = %s", l.Name, val))
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	var typ string
	var err error
	if v.Type != nil {
		typ, err = c.compileType(v.Type)
		if err != nil {
			return err
		}
	}
	var val string = "0"
	if v.Value != nil {
		val, err = c.compileExpr(v.Value)
		if err != nil {
			return err
		}
	} else if typ != "" {
		val = defaultValue(typ)
	}
	if typ != "" {
		c.writeln(fmt.Sprintf("let mutable %s: %s = %s", v.Name, typ, val))
	} else {
		c.writeln(fmt.Sprintf("let mutable %s = %s", v.Name, val))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	if len(a.Index) == 0 && len(a.Field) == 0 {
		c.writeln(fmt.Sprintf("%s <- %s", a.Name, val))
		return nil
	}
	return fmt.Errorf("assignment with indexing not supported")
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while %s do", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	if f.RangeEnd != nil {
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s .. %s do", f.Name, start, end))
	} else {
		c.writeln(fmt.Sprintf("for %s in %s do", f.Name, start))
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = fmt.Sprintf("(%s)", p.Name)
	}
	header := fmt.Sprintf("let %s %s =", f.Name, strings.Join(params, " "))
	c.writeln(header)
	c.indent++
	for i, st := range f.Body {
		if i == len(f.Body)-1 {
			if st.Return != nil {
				val, err := c.compileExpr(st.Return.Value)
				if err != nil {
					return err
				}
				c.writeln(val)
				continue
			}
		}
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln(val)
	return nil
}

func (c *Compiler) compileIfStmt(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else != nil {
		c.writeln("else")
		c.indent++
		for _, st := range i.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
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
		oper := op.Op
		switch op.Op {
		case "==":
			oper = "="
		case "!=":
			oper = "<>"
		}
		res = fmt.Sprintf("%s %s %s", res, oper, r)
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "!" {
			val = "not " + val
		} else {
			val = op + val
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	if len(p.Ops) == 0 {
		return val, nil
	}
	// Only support single index op
	if len(p.Ops) == 1 && p.Ops[0].Index != nil && p.Ops[0].Index.Start != nil {
		idx, err := c.compileExpr(p.Ops[0].Index.Start)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s.[%s]", val, idx), nil
	}
	return "", fmt.Errorf("unsupported postfix expression")
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + join(elems, "; ") + "]", nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
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
	switch call.Func {
	case "print":
		fmtArgs := make([]string, len(args))
		for i, a := range args {
			fmtArgs[i] = fmt.Sprintf("(string %s)", a)
		}
		format := strings.Repeat("%s ", len(args))
		format = strings.TrimSpace(format)
		return fmt.Sprintf("printfn \"%s\" %s", format, join(fmtArgs, " ")), nil
	default:
		argStr := strings.Join(args, " ")
		return fmt.Sprintf("%s %s", call.Func, argStr), nil
	}
}

func (c *Compiler) compileIfExpr(i *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return "", err
	}
	thn, err := c.compileExpr(i.Then)
	if err != nil {
		return "", err
	}
	if i.Else == nil && i.ElseIf == nil {
		return fmt.Sprintf("(if %s then %s else ())", cond, thn), nil
	}
	var els string
	if i.Else != nil {
		e, err := c.compileExpr(i.Else)
		if err != nil {
			return "", err
		}
		els = e
	} else if i.ElseIf != nil {
		e, err := c.compileIfExpr(i.ElseIf)
		if err != nil {
			return "", err
		}
		els = e
	}
	return fmt.Sprintf("(if %s then %s else %s)", cond, thn, els), nil
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.Name
	}
	if f.ExprBody != nil {
		body, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("fun %s -> %s", strings.Join(params, " "), body), nil
	}
	return "fun _ -> ()", nil
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
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	default:
		return "()"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
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

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

// CompileFile is a helper that parses src and compiles it to F#.
func CompileFile(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, err
	}
	return New().Compile(prog)
}
