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
	env         *types.Env
	declared    map[string]bool
	tmpIndex    int
}

// New creates a new compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env, declared: make(map[string]bool)} }

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
	if !c.declared[l.Name] {
		typ := typeName(l.Type)
		if lst := listLiteral(l.Value); lst != nil {
			elems := make([]string, len(lst.Elems))
			for i, e := range lst.Elems {
				v, err := c.compileExpr(e)
				if err != nil {
					return err
				}
				elems[i] = v
			}
			c.writeln(fmt.Sprintf("%s, dimension(%d) :: %s = (/%s/)", typ, len(lst.Elems), l.Name, strings.Join(elems, ",")))
			c.declared[l.Name] = true
			return nil
		}
		c.writeln(fmt.Sprintf("%s :: %s", typ, l.Name))
		c.declared[l.Name] = true
	}
	if l.Value != nil {
		val, err := c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", l.Name, val))
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	return c.compileLet(&parser.LetStmt{Name: v.Name, Value: v.Value, Type: v.Type})
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	if len(a.Field) > 0 {
		return fmt.Errorf("assignment with field not supported")
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	target := a.Name
	for _, idx := range a.Index {
		if idx.Colon != nil || idx.Colon2 != nil {
			return fmt.Errorf("slices not supported")
		}
		v, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		target += fmt.Sprintf("(%s)", v)
	}
	c.writeln(fmt.Sprintf("%s = %s", target, val))
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if !c.declared[f.Name] {
		c.writeln(fmt.Sprintf("integer :: %s", f.Name))
		c.declared[f.Name] = true
	}
	if f.RangeEnd == nil {
		lst := listLiteral(f.Source)
		if lst == nil {
			return fmt.Errorf("only numeric ranges or list literals supported")
		}
		arr := fmt.Sprintf("_arr_%d", c.tmpIndex)
		idx := fmt.Sprintf("_i%d", c.tmpIndex)
		c.tmpIndex++
		elems := make([]string, len(lst.Elems))
		for i, e := range lst.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return err
			}
			elems[i] = v
		}
		c.writeln(fmt.Sprintf("integer, dimension(%d) :: %s = (/%s/)", len(lst.Elems), arr, strings.Join(elems, ",")))
		c.writeln(fmt.Sprintf("integer :: %s", idx))
		c.writeln(fmt.Sprintf("do %s = 1, %d", idx, len(lst.Elems)))
		c.indent++
		c.writeln(fmt.Sprintf("%s = %s(%s)", f.Name, arr, idx))
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end do")
		return nil
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
	retType := typeName(fn.Return)
	c.writeln(fmt.Sprintf("%s function %s(%s)", retType, fn.Name, strings.Join(params, ",")))
	c.indent++
	for _, p := range fn.Params {
		c.writeln(fmt.Sprintf("%s, intent(in) :: %s", typeName(p.Type), p.Name))
		c.declared[p.Name] = true
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
	base, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	res := base
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil {
				return "", fmt.Errorf("slices not supported")
			}
			v, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			res = fmt.Sprintf("%s(%s)", res, v)
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return res, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
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
		return fmt.Sprintf("(/%s/)", strings.Join(elems, ",")), nil
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

func listLiteral(e *parser.Expr) *parser.ListLiteral {
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
	return v.Target.List
}

func typeName(t *parser.TypeRef) string {
	if t == nil {
		return "integer"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "integer"
		case "bool":
			return "logical"
		case "string":
			return "character(len=100)"
		case "float":
			return "real"
		}
	}
	return "integer"
}
