//go:build slow

package ftncode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a very small subset of Mochi into Fortran 90.
// Only basic constructs used by the simpler test programs are supported.
// Unsupported syntax results in a compilation error.
type Compiler struct {
	buf         bytes.Buffer
	indent      int
	functions   []*parser.FunStmt
	currentFunc string
}

// New creates a new compiler instance.
func New(_ *types.Env) *Compiler { return &Compiler{} }

// Compile converts a parsed Mochi program into Fortran source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.functions = nil
	name := "main"
	if prog.Package != "" {
		name = sanitize(prog.Package)
	}
	c.writeln(fmt.Sprintf("program %s", name))
	c.indent++
	c.writeln("implicit none")

	for _, st := range prog.Statements {
		if st.Fun != nil {
			c.functions = append(c.functions, st.Fun)
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}

	if len(c.functions) > 0 {
		c.writeln("contains")
		for _, fn := range c.functions {
			if err := c.compileFun(fn); err != nil {
				return nil, err
			}
		}
	}

	c.indent--
	c.writeln(fmt.Sprintf("end program %s", name))
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
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("exit")
		return nil
	case s.Continue != nil:
		c.writeln("cycle")
		return nil
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.Expr != nil:
		if call := callExpr(s.Expr.Expr); call != nil && call.Func == "print" {
			args := make([]string, len(call.Args))
			for i, a := range call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return err
				}
				args[i] = v
			}
			c.writeln("print *, " + strings.Join(args, ", "))
			return nil
		}
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	if l.Value == nil && l.Type != nil {
		c.writeln(fmt.Sprintf("integer :: %s", l.Name))
		return nil
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", l.Name, val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	return c.compileLet(&parser.LetStmt{Name: v.Name, Value: v.Value, Type: v.Type})
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	if len(a.Index) > 0 || len(a.Field) > 0 {
		return fmt.Errorf("assignment with index or field not supported")
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", a.Name, val))
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd == nil {
		return fmt.Errorf("only numeric ranges supported")
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("do %s = %s, %s", f.Name, start, end))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end do")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("do while (%s)", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end do")
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	return c.compileIfChain("if", ifst)
}

func (c *Compiler) compileIfChain(kw string, ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s (%s) then", kw, cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if ifst.ElseIf != nil {
		if err := c.compileIfChain("else if", ifst.ElseIf); err != nil {
			return err
		}
	} else if ifst.Else != nil {
		c.writeln("else")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	if kw == "if" {
		c.writeln("end if")
	}
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	if c.currentFunc == "" {
		return fmt.Errorf("return outside function")
	}
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", c.currentFunc, val))
	c.writeln("return")
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("integer function %s(%s)", fn.Name, strings.Join(params, ",")))
	c.indent++
	for _, p := range fn.Params {
		c.writeln(fmt.Sprintf("integer, intent(in) :: %s", p.Name))
	}
	prev := c.currentFunc
	c.currentFunc = fn.Name
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.currentFunc = prev
	c.indent--
	c.writeln(fmt.Sprintf("end function %s", fn.Name))
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("invalid expression")
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
		opStr := op.Op
		switch opStr {
		case "&&":
			opStr = ".and."
		case "||":
			opStr = ".or."
		case "!=":
			opStr = "/="
		case "%":
			res = fmt.Sprintf("mod(%s,%s)", res, r)
			continue
		}
		res = fmt.Sprintf("(%s %s %s)", res, opStr, r)
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = ".not. " + val
		default:
			return "", fmt.Errorf("unsupported unary op %s", u.Ops[i])
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if len(p.Ops) > 0 {
		return "", fmt.Errorf("postfix operations not supported")
	}
	return c.compilePrimary(p.Target)
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Selector != nil:
		if len(p.Selector.Tail) != 0 {
			return "", fmt.Errorf("selector not supported at line %d", p.Pos.Line)
		}
		return p.Selector.Root, nil
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

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ",")), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return ".true."
		}
		return ".false."
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str)
	default:
		return "0"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func sanitize(name string) string {
	name = strings.ReplaceAll(name, "-", "_")
	return name
}

func callExpr(e *parser.Expr) *parser.CallExpr {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil
	}
	v := u.Value
	if len(v.Ops) != 0 || v.Target == nil {
		return nil
	}
	if v.Target.Call != nil {
		return v.Target.Call
	}
	return nil
}
