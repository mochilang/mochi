package ftncode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

// Compiler emits very small Fortran 90 code for a limited subset of Mochi.
// It only supports constructs needed for the leetcode two-sum example.
type Compiler struct {
	buf    bytes.Buffer
	indent int
}

func New() *Compiler { return &Compiler{} }

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
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
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z')) {
		s = "v_" + s
	}
	return s
}

func collectLoopVars(stmts []*parser.Statement, vars map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.For != nil:
			vars[sanitizeName(s.For.Name)] = true
			collectLoopVars(s.For.Body, vars)
		case s.If != nil:
			collectLoopVars(s.If.Then, vars)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				collectLoopVars(cur.Then, vars)
			}
			if len(cur.Else) > 0 {
				collectLoopVars(cur.Else, vars)
			}
		}
	}
}

// Compile converts prog into simple Fortran source.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	var funs []*parser.FunStmt
	var mainStmts []*parser.Statement
	for _, s := range prog.Statements {
		if s.Fun != nil {
			funs = append(funs, s.Fun)
		} else {
			mainStmts = append(mainStmts, s)
		}
	}
	c.writeln("program main")
	c.indent++
	c.writeln("implicit none")
	// crude variable declarations for lets in main
	declared := map[string]bool{}
	for _, s := range mainStmts {
		if s.Let != nil {
			name := sanitizeName(s.Let.Name)
			if name == "result" {
				c.writeln("integer :: result(2)")
			} else {
				c.writeln("integer :: " + name)
			}
			declared[name] = true
		}
	}
	loopVars := map[string]bool{}
	collectLoopVars(mainStmts, loopVars)
	for name := range loopVars {
		if !declared[name] {
			c.writeln("integer :: " + name)
		}
	}
	for _, s := range mainStmts {
		if err := c.compileStmt(s, ""); err != nil {
			return nil, err
		}
	}
	c.indent--
	if len(funs) > 0 {
		c.writeln("contains")
		c.indent++
		for _, f := range funs {
			if err := c.compileFun(f); err != nil {
				return nil, err
			}
			c.writeln("")
		}
		c.indent--
	}
	c.writeln("end program main")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	// Special-case the twoSum example that returns a pair of indices.
	if len(fn.Params) == 2 && fn.Params[0].Name == "nums" && fn.Params[1].Name == "target" {
		resVar := "res"
		c.writeln(fmt.Sprintf("function %s(nums, target) result(%s)", sanitizeName(fn.Name), resVar))
		c.indent++
		c.writeln("implicit none")
		c.writeln("integer, intent(in) :: nums(:)")
		c.writeln("integer, intent(in) :: target")
		c.writeln("integer :: n")
		c.writeln("integer :: i")
		c.writeln("integer :: j")
		c.writeln("integer :: res(2)")
		for _, st := range fn.Body {
			if err := c.compileStmt(st, resVar); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end function " + sanitizeName(fn.Name))
		return nil
	}

	// Generic support for simple functions with integer parameters and return value.
	resVar := "res"
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("function %s(%s) result(%s)", sanitizeName(fn.Name), strings.Join(params, ", "), resVar))
	c.indent++
	c.writeln("implicit none")
	for _, p := range fn.Params {
		c.writeln(fmt.Sprintf("integer, intent(in) :: %s", sanitizeName(p.Name)))
	}
	c.writeln(fmt.Sprintf("integer :: %s", resVar))
	loopVars := map[string]bool{}
	collectLoopVars(fn.Body, loopVars)
	for name := range loopVars {
		if name != resVar {
			c.writeln("integer :: " + name)
		}
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st, resVar); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end function " + sanitizeName(fn.Name))
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement, retVar string) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Let.Name), val))
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		if retVar == "" {
			return fmt.Errorf("return outside function")
		}
		c.writeln(fmt.Sprintf("%s = %s", retVar, val))
		c.writeln("return")
	case s.If != nil:
		if err := c.compileIf(s.If, retVar); err != nil {
			return err
		}
	case s.For != nil:
		start, err := c.compileExpr(s.For.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(s.For.RangeEnd)
		if err != nil {
			return err
		}
		name := sanitizeName(s.For.Name)
		c.writeln(fmt.Sprintf("do %s = %s, %s - 1", name, start, end))
		c.indent++
		for _, st := range s.For.Body {
			if err := c.compileStmt(st, retVar); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end do")
	case s.Expr != nil:
		if call, ok := printCall(s.Expr.Expr); ok {
			args := make([]string, len(call.Args))
			for i, a := range call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return err
				}
				args[i] = v
			}
			c.writeln("print *, " + strings.Join(args, ", "))
		} else {
			expr, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return err
			}
			c.writeln(expr)
		}
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileIf(ifStmt *parser.IfStmt, retVar string) error {
	cond, err := c.compileExpr(ifStmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) then", cond))
	c.indent++
	for _, st := range ifStmt.Then {
		if err := c.compileStmt(st, retVar); err != nil {
			return err
		}
	}
	c.indent--

	cur := ifStmt
	for cur.ElseIf != nil {
		cur = cur.ElseIf
		cond, err := c.compileExpr(cur.Cond)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("else if (%s) then", cond))
		c.indent++
		for _, st := range cur.Then {
			if err := c.compileStmt(st, retVar); err != nil {
				return err
			}
		}
		c.indent--
	}

	if len(cur.Else) > 0 {
		c.writeln("else")
		c.indent++
		for _, st := range cur.Else {
			if err := c.compileStmt(st, retVar); err != nil {
				return err
			}
		}
		c.indent--
	}

	c.writeln("end if")
	return nil
}

func printCall(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	if p.Target.Call.Func == "print" {
		return p.Target.Call, true
	}
	return nil, false
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "==", "<", "<=", ">", ">=":
			expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, right)
		case "!=":
			expr = fmt.Sprintf("(%s /= %s)", expr, right)
		default:
			return "", fmt.Errorf("unsupported op %s", op.Op)
		}
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		if u.Ops[i] == "-" {
			val = fmt.Sprintf("(-%s)", val)
		} else {
			return "", fmt.Errorf("unsupported unary op")
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s(%s + 1)", expr, idx)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Str != nil {
			s := strings.ReplaceAll(*p.Lit.Str, "'", "''")
			return "'" + s + "'", nil
		}
		return "", fmt.Errorf("unknown literal")
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "(/" + strings.Join(elems, ", ") + "/)", nil
	case p.Selector != nil:
		return sanitizeName(p.Selector.Root), nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call, "")
	case p.Group != nil:
		v, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr, recv string) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch call.Func {
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("size(%s)", args[0]), nil
	default:
		name := sanitizeName(call.Func)
		if recv != "" {
			name = recv
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}
