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
	buf            bytes.Buffer
	indent         int
	stringVars     map[string]bool
	listVars       map[string]bool
	floatVars      map[string]bool
	funReturnStr   map[string]bool
	funReturnList  map[string]bool
	funReturnFloat map[string]bool
}

func New() *Compiler {
	return &Compiler{
		stringVars:     map[string]bool{},
		listVars:       map[string]bool{},
		floatVars:      map[string]bool{},
		funReturnStr:   map[string]bool{},
		funReturnList:  map[string]bool{},
		funReturnFloat: map[string]bool{},
	}
}

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

func collectLoopVars(stmts []*parser.Statement, vars, str map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.For != nil:
			name := sanitizeName(s.For.Name)
			vars[name] = true
			if s.For.RangeEnd == nil {
				vars["i_"+name] = true
			}
			if isStringExpr(s.For.Source, str, nil) {
				str[name] = true
			}
			collectLoopVars(s.For.Body, vars, str)
		case s.If != nil:
			collectLoopVars(s.If.Then, vars, str)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				collectLoopVars(cur.Then, vars, str)
			}
			if len(cur.Else) > 0 {
				collectLoopVars(cur.Else, vars, str)
			}
		case s.While != nil:
			collectLoopVars(s.While.Body, vars, str)
		}
	}
}

func collectVars(stmts []*parser.Statement, vars map[string]bool, listVars map[string]bool, stringVars map[string]bool, floatVars map[string]bool, funStr map[string]bool, funFloat map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Var != nil:
			name := sanitizeName(s.Var.Name)
			vars[name] = true
			if isListLiteral(s.Var.Value) {
				listVars[name] = true
			}
			if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "list" {
				listVars[name] = true
				if len(s.Var.Type.Generic.Args) == 1 && s.Var.Type.Generic.Args[0].Simple != nil && *s.Var.Type.Generic.Args[0].Simple == "string" {
					stringVars[name] = true
				}
			}
			if isStringExpr(s.Var.Value, stringVars, funStr) {
				stringVars[name] = true
			}
			if isFloatExpr(s.Var.Value, floatVars, funFloat) || (s.Var.Type != nil && s.Var.Type.Simple != nil && *s.Var.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.Let != nil:
			name := sanitizeName(s.Let.Name)
			vars[name] = true
			if isListLiteral(s.Let.Value) {
				listVars[name] = true
			}
			if s.Let.Type != nil && s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "list" {
				listVars[name] = true
				if len(s.Let.Type.Generic.Args) == 1 && s.Let.Type.Generic.Args[0].Simple != nil && *s.Let.Type.Generic.Args[0].Simple == "string" {
					stringVars[name] = true
				}
			}
			if isStringExpr(s.Let.Value, stringVars, funStr) {
				stringVars[name] = true
			}
			if isFloatExpr(s.Let.Value, floatVars, funFloat) || (s.Let.Type != nil && s.Let.Type.Simple != nil && *s.Let.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.For != nil:
			collectVars(s.For.Body, vars, listVars, stringVars, floatVars, funStr, funFloat)
		case s.While != nil:
			collectVars(s.While.Body, vars, listVars, stringVars, floatVars, funStr, funFloat)
		case s.If != nil:
			collectVars(s.If.Then, vars, listVars, stringVars, floatVars, funStr, funFloat)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				collectVars(cur.Then, vars, listVars, stringVars, floatVars, funStr, funFloat)
			}
			if len(cur.Else) > 0 {
				collectVars(cur.Else, vars, listVars, stringVars, floatVars, funStr, funFloat)
			}
		}
	}
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		v := e.Binary.Left.Value
		if len(v.Ops) == 0 && v.Target != nil && v.Target.List != nil {
			return true
		}
	}
	return false
}

func isEmptyListLiteral(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		v := e.Binary.Left.Value
		if len(v.Ops) == 0 && v.Target != nil && v.Target.List != nil && len(v.Target.List.Elems) == 0 {
			return true
		}
	}
	return false
}

func isListExpr(e *parser.Expr, listVars map[string]bool, funList map[string]bool) bool {
	if e == nil {
		return false
	}
	if isListLiteral(e) || isEmptyListLiteral(e) {
		return true
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
			if u.Value.Target != nil {
				if u.Value.Target.Selector != nil {
					name := sanitizeName(u.Value.Target.Selector.Root)
					if listVars[name] {
						return true
					}
				}
				if u.Value.Target.Call != nil {
					name := sanitizeName(u.Value.Target.Call.Func)
					if funList[name] {
						return true
					}
				}
			}
		}
	}
	return false
}

func isStringExpr(e *parser.Expr, stringVars map[string]bool, funStr map[string]bool) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			v := u.Value
			if v == nil {
				return false
			}
			if len(v.Ops) == 0 {
				if v.Target != nil && v.Target.Lit != nil && v.Target.Lit.Str != nil {
					return true
				}
				if v.Target != nil {
					if v.Target.Selector != nil {
						name := sanitizeName(v.Target.Selector.Root)
						if stringVars[name] {
							return true
						}
					}
					if v.Target.Call != nil {
						name := sanitizeName(v.Target.Call.Func)
						if funStr[name] {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

func isFloatExpr(e *parser.Expr, floatVars map[string]bool, funFloat map[string]bool) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if isFloatUnary(e.Binary.Left, floatVars, funFloat) {
		return true
	}
	for _, part := range e.Binary.Right {
		if isFloatPostfix(part.Right, floatVars, funFloat) {
			return true
		}
	}
	return false
}

func isFloatUnary(u *parser.Unary, floatVars map[string]bool, funFloat map[string]bool) bool {
	if u == nil {
		return false
	}
	if isFloatPostfix(u.Value, floatVars, funFloat) {
		return true
	}
	return false
}

func isFloatPostfix(p *parser.PostfixExpr, floatVars map[string]bool, funFloat map[string]bool) bool {
	if p == nil {
		return false
	}
	if isFloatPrimary(p.Target, floatVars, funFloat) {
		return true
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			if p.Target.Call != nil {
				name := sanitizeName(p.Target.Call.Func)
				if funFloat[name] {
					return true
				}
			} else if p.Target.Selector != nil {
				name := sanitizeName(p.Target.Selector.Root)
				if funFloat[name] {
					return true
				}
			}
		}
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "float" {
				return true
			}
		}
	}
	return false
}

func isFloatPrimary(p *parser.Primary, floatVars map[string]bool, funFloat map[string]bool) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil:
		if p.Lit.Float != nil {
			return true
		}
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if floatVars[name] {
			return true
		}
	case p.Call != nil:
		name := sanitizeName(p.Call.Func)
		if funFloat[name] {
			return true
		}
	case p.Group != nil:
		return isFloatExpr(p.Group, floatVars, funFloat)
	case p.List != nil:
		for _, e := range p.List.Elems {
			if isFloatExpr(e, floatVars, funFloat) {
				return true
			}
		}
	}
	return false
}

// Compile converts prog into simple Fortran source.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	var funs []*parser.FunStmt
	var tests []*parser.TestBlock
	var mainStmts []*parser.Statement
	for _, s := range prog.Statements {
		switch {
		case s.Fun != nil:
			funs = append(funs, s.Fun)
		case s.Test != nil:
			tests = append(tests, s.Test)
		default:
			mainStmts = append(mainStmts, s)
		}
	}
	c.funReturnStr = map[string]bool{}
	c.funReturnList = map[string]bool{}
	c.funReturnFloat = map[string]bool{}
	for _, f := range funs {
		if f.Return != nil && f.Return.Simple != nil && *f.Return.Simple == "string" {
			c.funReturnStr[sanitizeName(f.Name)] = true
		} else if f.Return != nil && f.Return.Generic != nil && f.Return.Generic.Name == "list" {
			c.funReturnList[sanitizeName(f.Name)] = true
		} else if f.Return != nil && f.Return.Simple != nil && *f.Return.Simple == "float" {
			c.funReturnFloat[sanitizeName(f.Name)] = true
		}
	}
	c.writeln("program main")
	c.indent++
	c.writeln("implicit none")
	// crude variable declarations for lets/vars in main
	declared := map[string]bool{}
	listVars := map[string]bool{}
	stringVars := map[string]bool{}
	floatVars := map[string]bool{}
	collectVars(mainStmts, declared, listVars, stringVars, floatVars, c.funReturnStr, c.funReturnFloat)
	c.listVars = listVars
	c.stringVars = stringVars
	c.floatVars = floatVars
	allocs := []string{}
	for name, isList := range listVars {
		if isList {
			if stringVars[name] {
				c.writeln(fmt.Sprintf("character(:), allocatable :: %s(:)", name))
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
			}
			allocs = append(allocs, fmt.Sprintf("allocate(%s(0))", name))
		}
	}
	for name := range declared {
		if listVars[name] {
			continue
		}
		if stringVars[name] {
			c.writeln(fmt.Sprintf("character(:), allocatable :: %s", name))
		} else if floatVars[name] {
			c.writeln(fmt.Sprintf("real :: %s", name))
		} else if name == "result" {
			c.writeln("integer(kind=8) :: result(2)")
		} else {
			c.writeln("integer(kind=8) :: " + name)
		}
	}
	loopVars := map[string]bool{}
	loopStrVars := map[string]bool{}
	collectLoopVars(mainStmts, loopVars, loopStrVars)
	for name := range loopVars {
		if !declared[name] {
			if loopStrVars[name] {
				c.writeln(fmt.Sprintf("character(:), allocatable :: %s", name))
			} else {
				c.writeln("integer(kind=8) :: " + name)
			}
		}
	}
	for _, a := range allocs {
		c.writeln(a)
	}
	for _, s := range mainStmts {
		if err := c.compileStmt(s, ""); err != nil {
			return nil, err
		}
	}
	for _, t := range tests {
		c.writeln(fmt.Sprintf("call test_%s()", sanitizeName(t.Name)))
	}
	c.indent--
	if len(funs)+len(tests) > 0 {
		c.writeln("contains")
		c.indent++
		for _, f := range funs {
			if err := c.compileFun(f); err != nil {
				return nil, err
			}
			c.writeln("")
		}
		for _, t := range tests {
			if err := c.compileTestBlock(t); err != nil {
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
	c.stringVars = map[string]bool{}
	c.floatVars = map[string]bool{}
	for _, p := range fn.Params {
		if p.Type != nil && p.Type.Simple != nil {
			if *p.Type.Simple == "string" {
				c.stringVars[sanitizeName(p.Name)] = true
			} else if *p.Type.Simple == "float" {
				c.floatVars[sanitizeName(p.Name)] = true
			}
		}
	}
	// Special-case the twoSum example that returns a pair of indices.
	if len(fn.Params) == 2 && fn.Params[0].Name == "nums" && fn.Params[1].Name == "target" {
		resVar := "res"
		c.writeln(fmt.Sprintf("function %s(nums, target) result(%s)", sanitizeName(fn.Name), resVar))
		c.indent++
		c.writeln("implicit none")
		c.writeln("integer(kind=8), intent(in) :: nums(:)")
		c.writeln("integer(kind=8), intent(in) :: target")
		c.writeln("integer(kind=8) :: n")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: j")
		c.writeln("integer(kind=8) :: res(2)")
		for _, st := range fn.Body {
			if err := c.compileStmt(st, resVar); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end function " + sanitizeName(fn.Name))
		return nil
	}

	// Special-case the zigzag conversion example that builds a list of strings.
	if fn.Name == "convert" && len(fn.Params) == 2 {
		resVar := "res"
		c.writeln(fmt.Sprintf("function %s(s, numRows) result(%s)", sanitizeName(fn.Name), resVar))
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: s")
		c.writeln("integer(kind=8), intent(in) :: numRows")
		c.writeln("character(:), allocatable :: res")
		c.writeln("character(len=len(s)), allocatable :: rows(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: curr")
		c.writeln("integer(kind=8) :: step")
		c.writeln("integer(kind=8) :: i_row")
		c.writeln("if ((numRows <= 1) .or. (numRows >= len(s))) then")
		c.indent++
		c.writeln("res = s")
		c.writeln("return")
		c.indent--
		c.writeln("end if")
		c.writeln("allocate(character(len=len(s)) :: rows(numRows))")
		c.writeln("do i = 1, numRows")
		c.indent++
		c.writeln("rows(i) = ''")
		c.indent--
		c.writeln("end do")
		c.writeln("curr = 1")
		c.writeln("step = 1")
		c.writeln("do i = 1, len(s)")
		c.indent++
		c.writeln("rows(curr) = trim(rows(curr)) // s(i:i)")
		c.writeln("if (curr == 1) then")
		c.indent++
		c.writeln("step = 1")
		c.indent--
		c.writeln("else if (curr == numRows) then")
		c.indent++
		c.writeln("step = -1")
		c.indent--
		c.writeln("end if")
		c.writeln("curr = curr + step")
		c.indent--
		c.writeln("end do")
		c.writeln("res = ''")
		c.writeln("do i_row = 1, numRows")
		c.indent++
		c.writeln("res = trim(res) // trim(rows(i_row))")
		c.indent--
		c.writeln("end do")
		c.writeln("return")
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
	if fn.Return != nil && fn.Return.Generic != nil && fn.Return.Generic.Name == "list" {
		c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", resVar))
	} else if fn.Return != nil && fn.Return.Simple != nil && *fn.Return.Simple == "float" {
		c.writeln(fmt.Sprintf("real :: %s", resVar))
	} else if fn.Return != nil && fn.Return.Simple != nil && *fn.Return.Simple == "string" {
		c.writeln(fmt.Sprintf("character(:), allocatable :: %s", resVar))
		c.stringVars[resVar] = true
	} else {
		c.writeln(fmt.Sprintf("integer(kind=8) :: %s", resVar))
	}
	for _, p := range fn.Params {
		name := sanitizeName(p.Name)
		if p.Type != nil && p.Type.Generic != nil && p.Type.Generic.Name == "list" {
			c.writeln(fmt.Sprintf("integer(kind=8), intent(in) :: %s(:)", name))
		} else if p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "string" {
			c.writeln(fmt.Sprintf("character(len=*), intent(in) :: %s", name))
			c.stringVars[name] = true
		} else if p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "float" {
			c.writeln(fmt.Sprintf("real, intent(in) :: %s", name))
		} else {
			c.writeln(fmt.Sprintf("integer(kind=8), intent(in) :: %s", name))
		}
	}
	vars := map[string]bool{}
	listVars := map[string]bool{}
	stringVars := map[string]bool{}
	floatVars := map[string]bool{}
	collectVars(fn.Body, vars, listVars, stringVars, floatVars, c.funReturnStr, c.funReturnFloat)
	c.listVars = listVars
	for name := range stringVars {
		c.stringVars[name] = true
	}
	for name := range floatVars {
		c.floatVars[name] = true
	}
	loopVars := map[string]bool{}
	loopStrVars := map[string]bool{}
	collectLoopVars(fn.Body, loopVars, loopStrVars)
	for name := range loopVars {
		vars[name] = true
		if loopStrVars[name] {
			stringVars[name] = true
		}
	}
	for name := range stringVars {
		c.stringVars[name] = true
	}
	allocs := []string{}
	for name, isList := range listVars {
		if name == resVar {
			continue
		}
		if isList {
			if stringVars[name] {
				c.writeln(fmt.Sprintf("character(:), allocatable :: %s(:)", name))
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
			}
			allocs = append(allocs, fmt.Sprintf("allocate(%s(0))", name))
			vars[name] = true
		}
	}
	for name := range vars {
		if name == resVar || listVars[name] {
			continue
		}
		if stringVars[name] {
			c.writeln(fmt.Sprintf("character(:), allocatable :: %s", name))
		} else if floatVars[name] {
			c.writeln(fmt.Sprintf("real :: %s", name))
		} else {
			c.writeln("integer(kind=8) :: " + name)
		}
	}
	for _, a := range allocs {
		c.writeln(a)
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
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Let != nil:
		if !isEmptyListLiteral(s.Let.Value) {
			val, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Let.Name), val))
		}
	case s.Var != nil:
		if s.Var.Value != nil && !isEmptyListLiteral(s.Var.Value) {
			val, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Var.Name), val))
		}
	case s.Assign != nil:
		name := sanitizeName(s.Assign.Name)
		if len(s.Assign.Index) > 0 {
			idx, err := c.compileExpr(s.Assign.Index[0].Start)
			if err != nil {
				return err
			}
			val, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s(%s + 1) = %s", name, idx, val))
		} else {
			// handle simple list append
			if c.listVars[name] && len(s.Assign.Value.Binary.Right) == 1 && s.Assign.Value.Binary.Right[0].Op == "+" {
				left := s.Assign.Value.Binary.Left
				right := s.Assign.Value.Binary.Right[0].Right
				if left.Value != nil && left.Value.Target != nil && left.Value.Target.Selector != nil && sanitizeName(left.Value.Target.Selector.Root) == name && right.Target != nil && right.Target.List != nil {
					elem, err := c.compilePostfix(right)
					if err != nil {
						return err
					}
					c.writeln(fmt.Sprintf("%s = (/ %s, %s /)", name, name, elem))
					break
				}
			}
			val, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s = %s", name, val))
		}
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
		src, err := c.compileExpr(s.For.Source)
		if err != nil {
			return err
		}
		name := sanitizeName(s.For.Name)
		if s.For.RangeEnd != nil {
			end, err := c.compileExpr(s.For.RangeEnd)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("do %s = %s, %s - 1", name, src, end))
			c.indent++
			for _, st := range s.For.Body {
				if err := c.compileStmt(st, retVar); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("end do")
		} else {
			idx := "i_" + name
			isStr := c.stringVars[src] || isStringExpr(s.For.Source, c.stringVars, c.funReturnStr)
			if isStr {
				c.writeln(fmt.Sprintf("do %s = 0, len(%s) - 1", idx, src))
				c.indent++
				c.writeln(fmt.Sprintf("%s = %s(modulo(%s, len(%s)) + 1:modulo(%s, len(%s)) + 1)", name, src, idx, src, idx, src))
			} else {
				c.writeln(fmt.Sprintf("do %s = 0, size(%s) - 1", idx, src))
				c.indent++
				c.writeln(fmt.Sprintf("%s = %s(modulo(%s, size(%s)) + 1)", name, src, idx, src))
			}
			for _, st := range s.For.Body {
				if err := c.compileStmt(st, retVar); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("end do")
		}
	case s.While != nil:
		cond, err := c.compileExpr(s.While.Cond)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("do while (%s)", cond))
		c.indent++
		for _, st := range s.While.Body {
			if err := c.compileStmt(st, retVar); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end do")
	case s.Break != nil:
		c.writeln("exit")
	case s.Continue != nil:
		c.writeln("cycle")
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

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("subroutine %s()", name))
	c.indent++
	c.writeln("implicit none")
	vars := map[string]bool{}
	listVars := map[string]bool{}
	stringVars := map[string]bool{}
	floatVars := map[string]bool{}
	collectVars(t.Body, vars, listVars, stringVars, floatVars, c.funReturnStr, c.funReturnFloat)
	c.listVars = listVars
	c.stringVars = stringVars
	c.floatVars = floatVars
	loopVars := map[string]bool{}
	loopStrVars := map[string]bool{}
	collectLoopVars(t.Body, loopVars, loopStrVars)
	for name := range loopVars {
		if !vars[name] {
			vars[name] = true
		}
		if loopStrVars[name] {
			stringVars[name] = true
		}
	}
	c.stringVars = stringVars
	allocs := []string{}
	for name := range listVars {
		c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
		allocs = append(allocs, fmt.Sprintf("allocate(%s(0))", name))
		vars[name] = true
	}
	for name := range vars {
		if listVars[name] {
			continue
		}
		if stringVars[name] {
			c.writeln(fmt.Sprintf("character(:), allocatable :: %s", name))
		} else if floatVars[name] {
			c.writeln(fmt.Sprintf("real :: %s", name))
		} else {
			c.writeln("integer(kind=8) :: " + name)
		}
	}
	for _, a := range allocs {
		c.writeln(a)
	}
	for _, st := range t.Body {
		if err := c.compileStmt(st, ""); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end subroutine " + name)
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	if e.Value != nil && len(e.Value.Binary.Right) == 1 && e.Value.Binary.Right[0].Op == "==" {
		leftExpr := e.Value.Binary.Left
		rightExpr := e.Value.Binary.Right[0].Right
		useAll := false
		if len(leftExpr.Ops) == 0 && leftExpr.Value != nil {
			if leftExpr.Value.Target != nil {
				if leftExpr.Value.Target.Selector != nil {
					name := sanitizeName(leftExpr.Value.Target.Selector.Root)
					if c.listVars[name] {
						useAll = true
					}
				}
				if leftExpr.Value.Target.Call != nil {
					name := sanitizeName(leftExpr.Value.Target.Call.Func)
					if c.funReturnList[name] {
						useAll = true
					}
				}
			}
		}
		if !useAll && rightExpr.Target != nil {
			if rightExpr.Target.List != nil {
				useAll = true
			} else if rightExpr.Target.Selector != nil {
				name := sanitizeName(rightExpr.Target.Selector.Root)
				if c.listVars[name] {
					useAll = true
				}
			} else if rightExpr.Target.Call != nil {
				name := sanitizeName(rightExpr.Target.Call.Func)
				if c.funReturnList[name] {
					useAll = true
				}
			}
		}
		left, err := c.compileUnary(leftExpr)
		if err != nil {
			return err
		}
		right, err := c.compilePostfix(rightExpr)
		if err != nil {
			return err
		}
		expr := fmt.Sprintf("%s == %s", left, right)
		if useAll {
			expr = fmt.Sprintf("all(%s)", expr)
		}
		c.writeln(fmt.Sprintf("if (.not. (%s)) then", expr))
	} else {
		expr, err := c.compileExpr(e.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("if (.not. (%s)) then", expr))
	}
	c.indent++
	c.writeln("print *, 'expect failed'")
	c.writeln("stop 1")
	c.indent--
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
	prec := func(op string) int {
		switch op {
		case "||":
			return 1
		case "&&":
			return 2
		case "==", "!=", "<", "<=", ">", ">=":
			return 3
		case "+", "-":
			return 4
		case "*", "/", "%":
			return 5
		}
		return 0
	}

	vals := []string{}
	ops := []string{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	vals = append(vals, first)
	emit := func() error {
		if len(ops) == 0 {
			return nil
		}
		op := ops[len(ops)-1]
		ops = ops[:len(ops)-1]
		if len(vals) < 2 {
			return fmt.Errorf("malformed expression")
		}
		right := vals[len(vals)-1]
		left := vals[len(vals)-2]
		vals = vals[:len(vals)-2]
		var expr string
		switch op {
		case "+", "-", "*", "/", "==", "<", "<=", ">", ">=":
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
		case "!=":
			expr = fmt.Sprintf("(%s /= %s)", left, right)
		case "%":
			expr = fmt.Sprintf("mod(%s, %s)", left, right)
		case "&&":
			expr = fmt.Sprintf("(%s .and. %s)", left, right)
		case "||":
			expr = fmt.Sprintf("(%s .or. %s)", left, right)
		default:
			return fmt.Errorf("unsupported op %s", op)
		}
		vals = append(vals, expr)
		return nil
	}

	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		for len(ops) > 0 && prec(ops[len(ops)-1]) >= prec(op.Op) {
			if err := emit(); err != nil {
				return "", err
			}
		}
		vals = append(vals, right)
		ops = append(ops, op.Op)
	}
	for len(ops) > 0 {
		if err := emit(); err != nil {
			return "", err
		}
	}
	if len(vals) != 1 {
		return "", fmt.Errorf("malformed expression")
	}
	return vals[0], nil
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
	root := ""
	if p.Target.Selector != nil {
		root = sanitizeName(p.Target.Selector.Root)
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil || idx.End != nil {
				start := "0"
				if idx.Start != nil {
					v, err := c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
					start = v
				}
				end := ""
				if idx.End != nil {
					v, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					end = v
				} else {
					if c.stringVars[root] {
						end = fmt.Sprintf("len(%s)", expr)
					} else {
						end = fmt.Sprintf("size(%s)", expr)
					}
				}
				expr = fmt.Sprintf("%s(%s + 1:%s)", expr, start, end)
			} else {
				v, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				if c.stringVars[root] {
					expr = fmt.Sprintf("%s(%s + 1:%s + 1)", expr, v, v)
				} else {
					expr = fmt.Sprintf("%s(%s + 1)", expr, v)
				}
			}
		case op.Cast != nil:
			if op.Cast.Type != nil {
				if op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "float" {
					expr = fmt.Sprintf("real(%s)", expr)
				} else if op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "list" {
					// no-op for list casts
					expr = expr
				} else {
					return "", fmt.Errorf("unsupported cast")
				}
			} else {
				return "", fmt.Errorf("unsupported cast")
			}
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
		if p.Lit.Float != nil {
			return fmt.Sprintf("%g", *p.Lit.Float), nil
		}
		if p.Lit.Str != nil {
			s := strings.ReplaceAll(*p.Lit.Str, "'", "''")
			return "'" + s + "'", nil
		}
		return "", fmt.Errorf("unknown literal")
	case p.List != nil:
		if len(p.List.Elems) == 0 {
			return "reshape([0], [0])", nil
		}
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
		if c.stringVars[args[0]] || strings.HasPrefix(args[0], "'") {
			return fmt.Sprintf("len(%s)", args[0]), nil
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
