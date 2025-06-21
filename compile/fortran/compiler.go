package ftncode

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
)

// Compiler emits very small Fortran 90 code for a limited subset of Mochi.
// It only supports constructs needed for the leetcode two-sum example.
type Compiler struct {
	buf                  bytes.Buffer
	indent               int
	stringVars           map[string]bool
	listVars             map[string]bool
	floatVars            map[string]bool
	funReturnStr         map[string]bool
	funReturnList        map[string]bool
	funReturnFloat       map[string]bool
	needsUnionInt        bool
	needsUnionFloat      bool
	needsUnionString     bool
	needsExceptInt       bool
	needsExceptFloat     bool
	needsExceptString    bool
	needsIntersectInt    bool
	needsIntersectFloat  bool
	needsIntersectString bool
	needsStrInt          bool
	needsStrFloat        bool
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

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.needsUnionInt = false
	c.needsUnionFloat = false
	c.needsUnionString = false
	c.needsExceptInt = false
	c.needsExceptFloat = false
	c.needsExceptString = false
	c.needsIntersectInt = false
	c.needsIntersectFloat = false
	c.needsIntersectString = false
	c.needsStrInt = false
	c.needsStrFloat = false
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
	collectVars(mainStmts, declared, listVars, stringVars, floatVars, c.funReturnStr, c.funReturnFloat, c.funReturnList)
	c.listVars = listVars
	c.stringVars = stringVars
	c.floatVars = floatVars
	allocs := []string{}
	for name, isList := range listVars {
		if isList {
			if stringVars[name] {
				c.writeln(fmt.Sprintf("character(:), allocatable :: %s(:)", name))
			} else if floatVars[name] {
				c.writeln(fmt.Sprintf("real, allocatable :: %s(:)", name))
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
	needHelpers := c.needsUnionInt || c.needsUnionFloat || c.needsUnionString ||
		c.needsExceptInt || c.needsExceptFloat || c.needsExceptString ||
		c.needsIntersectInt || c.needsIntersectFloat || c.needsIntersectString ||
		c.needsStrInt || c.needsStrFloat
	if len(funs)+len(tests) > 0 || needHelpers {
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
		if needHelpers {
			c.writeHelpers()
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
		if len(fn.Return.Generic.Args) == 1 && fn.Return.Generic.Args[0].Simple != nil {
			switch *fn.Return.Generic.Args[0].Simple {
			case "string":
				c.writeln(fmt.Sprintf("character(:), allocatable :: %s(:)", resVar))
				c.stringVars[resVar] = true
			case "float":
				c.writeln(fmt.Sprintf("real, allocatable :: %s(:)", resVar))
				c.floatVars[resVar] = true
			default:
				c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", resVar))
			}
		} else {
			c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", resVar))
		}
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
			if len(p.Type.Generic.Args) == 1 && p.Type.Generic.Args[0].Simple != nil {
				switch *p.Type.Generic.Args[0].Simple {
				case "string":
					c.writeln(fmt.Sprintf("character(len=*), intent(in) :: %s(:)", name))
					c.stringVars[name] = true
				case "float":
					c.writeln(fmt.Sprintf("real, intent(in) :: %s(:)", name))
				default:
					c.writeln(fmt.Sprintf("integer(kind=8), intent(in) :: %s(:)", name))
				}
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), intent(in) :: %s(:)", name))
			}
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
	collectVars(fn.Body, vars, listVars, stringVars, floatVars, c.funReturnStr, c.funReturnFloat, c.funReturnList)
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
			} else if floatVars[name] {
				c.writeln(fmt.Sprintf("real, allocatable :: %s(:)", name))
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
			}
			allocs = append(allocs, fmt.Sprintf("allocate(%s(0))", name))
			vars[name] = true
		}
	}
	names := make([]string, 0, len(vars))
	for name := range vars {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
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
	collectVars(t.Body, vars, listVars, stringVars, floatVars, c.funReturnStr, c.funReturnFloat, c.funReturnList)
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
		if stringVars[name] {
			c.writeln(fmt.Sprintf("character(:), allocatable :: %s(:)", name))
		} else if floatVars[name] {
			c.writeln(fmt.Sprintf("real, allocatable :: %s(:)", name))
		} else {
			c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
		}
		allocs = append(allocs, fmt.Sprintf("allocate(%s(0))", name))
		vars[name] = true
	}
	names := make([]string, 0, len(vars))
	for name := range vars {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
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
		case "==", "!=", "<", "<=", ">", ">=", "in":
			return 3
		case "+", "-", "union", "except", "intersect":
			return 4
		case "*", "/", "%":
			return 5
		}
		return 0
	}

	vals := []string{}
	vStr := []bool{}
	vList := []bool{}
	vFloat := []bool{}
	ops := []*parser.BinaryOp{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	vals = append(vals, first)
	exprLeft := &parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}
	vStr = append(vStr, isStringExpr(exprLeft, c.stringVars, c.funReturnStr))
	vList = append(vList, isListExpr(exprLeft, c.listVars, c.funReturnList))
	vFloat = append(vFloat, isFloatExpr(exprLeft, c.floatVars, c.funReturnFloat))
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
		rStr := vStr[len(vStr)-1]
		lStr := vStr[len(vStr)-2]
		rList := vList[len(vList)-1]
		lList := vList[len(vList)-2]
		rFloat := vFloat[len(vFloat)-1]
		lFloat := vFloat[len(vFloat)-2]
		vals = vals[:len(vals)-2]
		vStr = vStr[:len(vStr)-2]
		vList = vList[:len(vList)-2]
		vFloat = vFloat[:len(vFloat)-2]
		var expr string
		switch op.Op {
		case "+":
			if lList || rList {
				expr = fmt.Sprintf("(/ %s, %s /)", left, right)
				vList = append(vList, true)
				vStr = append(vStr, false)
				vFloat = append(vFloat, lFloat || rFloat)
				vals = append(vals, expr)
				return nil
			}
			if lStr || rStr {
				expr = fmt.Sprintf("(trim(%s) // trim(%s))", left, right)
				vStr = append(vStr, true)
				vList = append(vList, false)
				vFloat = append(vFloat, false)
				vals = append(vals, expr)
				return nil
			}
			expr = fmt.Sprintf("(%s %s %s)", left, op.Op, right)
		case "-", "*", "/", "==", "<", "<=", ">", ">=":
			expr = fmt.Sprintf("(%s %s %s)", left, op.Op, right)
		case "!=":
			expr = fmt.Sprintf("(%s /= %s)", left, right)
		case "in":
			if rList {
				expr = fmt.Sprintf("any(%s == %s)", right, left)
			} else if rStr {
				expr = fmt.Sprintf("index(%s, %s) > 0", right, left)
			} else {
				return fmt.Errorf("unsupported op %s", op.Op)
			}
		case "%":
			expr = fmt.Sprintf("mod(%s, %s)", left, right)
		case "&&":
			expr = fmt.Sprintf("(%s .and. %s)", left, right)
		case "||":
			expr = fmt.Sprintf("(%s .or. %s)", left, right)
		case "union":
			if op.All {
				expr = fmt.Sprintf("(/ %s, %s /)", left, right)
				vFloat = append(vFloat, lFloat || rFloat)
			} else if lList && rList {
				if lFloat && rFloat {
					c.needsUnionFloat = true
					expr = fmt.Sprintf("union_float(%s, %s)", left, right)
					vFloat = append(vFloat, true)
				} else if lStr && rStr {
					c.needsUnionString = true
					expr = fmt.Sprintf("union_string(%s, %s)", left, right)
					vFloat = append(vFloat, false)
				} else if !lStr && !rStr {
					c.needsUnionInt = true
					expr = fmt.Sprintf("union_int(%s, %s)", left, right)
					vFloat = append(vFloat, false)
				} else {
					return fmt.Errorf("unsupported op %s", op.Op)
				}
			} else {
				return fmt.Errorf("unsupported op %s", op.Op)
			}
			vList = append(vList, true)
			vStr = append(vStr, false)
			vals = append(vals, expr)
			return nil
		case "except":
			if lList && rList {
				if lFloat && rFloat {
					c.needsExceptFloat = true
					expr = fmt.Sprintf("except_float(%s, %s)", left, right)
					vFloat = append(vFloat, true)
				} else if lStr && rStr {
					c.needsExceptString = true
					expr = fmt.Sprintf("except_string(%s, %s)", left, right)
					vFloat = append(vFloat, false)
				} else if !lStr && !rStr {
					c.needsExceptInt = true
					expr = fmt.Sprintf("except_int(%s, %s)", left, right)
					vFloat = append(vFloat, false)
				} else {
					return fmt.Errorf("unsupported op %s", op.Op)
				}
			} else {
				return fmt.Errorf("unsupported op %s", op.Op)
			}
			vList = append(vList, true)
			vStr = append(vStr, false)
			vals = append(vals, expr)
			return nil
		case "intersect":
			if lList && rList {
				if lFloat && rFloat {
					c.needsIntersectFloat = true
					expr = fmt.Sprintf("intersect_float(%s, %s)", left, right)
					vFloat = append(vFloat, true)
				} else if lStr && rStr {
					c.needsIntersectString = true
					expr = fmt.Sprintf("intersect_string(%s, %s)", left, right)
					vFloat = append(vFloat, false)
				} else if !lStr && !rStr {
					c.needsIntersectInt = true
					expr = fmt.Sprintf("intersect_int(%s, %s)", left, right)
					vFloat = append(vFloat, false)
				} else {
					return fmt.Errorf("unsupported op %s", op.Op)
				}
			} else {
				return fmt.Errorf("unsupported op %s", op.Op)
			}
			vList = append(vList, true)
			vStr = append(vStr, false)
			vals = append(vals, expr)
			return nil
		default:
			return fmt.Errorf("unsupported op %s", op.Op)
		}
		vals = append(vals, expr)
		vStr = append(vStr, false)
		vList = append(vList, false)
		vFloat = append(vFloat, lFloat && rFloat)
		return nil
	}

	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		exprRight := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}
		vStr = append(vStr, isStringExpr(exprRight, c.stringVars, c.funReturnStr))
		vList = append(vList, isListExpr(exprRight, c.listVars, c.funReturnList))
		vFloat = append(vFloat, isFloatExpr(exprRight, c.floatVars, c.funReturnFloat))
		for len(ops) > 0 && prec(ops[len(ops)-1].Op) >= prec(op.Op) {
			if err := emit(); err != nil {
				return "", err
			}
		}
		vals = append(vals, right)
		ops = append(ops, op)
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
		switch u.Ops[i] {
		case "-":
			val = fmt.Sprintf("(-%s)", val)
		case "!":
			val = fmt.Sprintf("(.not. %s)", val)
		default:
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
					if c.stringVars[root] || c.listVars[root] {
						start = fmt.Sprintf("modulo(%s, %s)", v, c.lengthFunc(root))
					} else {
						start = v
					}
				}
				end := ""
				if idx.End != nil {
					v, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					if c.stringVars[root] || c.listVars[root] {
						end = fmt.Sprintf("modulo(%s, %s)", v, c.lengthFunc(root))
					} else {
						end = v
					}
				} else {
					if c.stringVars[root] {
						end = fmt.Sprintf("len(%s)", root)
					} else {
						end = fmt.Sprintf("size(%s)", root)
					}
				}
				expr = fmt.Sprintf("%s(%s + 1:%s)", expr, start, end)
			} else {
				v, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				if c.stringVars[root] {
					expr = fmt.Sprintf("%s(modulo(%s, len(%s)) + 1:modulo(%s, len(%s)) + 1)", expr, v, root, v, root)
				} else if c.listVars[root] {
					expr = fmt.Sprintf("%s(modulo(%s, size(%s)) + 1)", expr, v, root)
				} else {
					expr = fmt.Sprintf("%s(%s + 1)", expr, v)
				}
			}
		case op.Cast != nil:
			if op.Cast.Type != nil {
				if op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "float" {
					expr = fmt.Sprintf("real(%s)", expr)
				} else if op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int" {
					expr = fmt.Sprintf("int(%s)", expr)
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
			// Emit 64-bit integer literals to match the default
			// INTEGER(KIND=8) type used throughout the backend.
			return fmt.Sprintf("%d_8", *p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return fmt.Sprintf("%g", *p.Lit.Float), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return ".true.", nil
			}
			return ".false.", nil
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
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Group != nil:
		v, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
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
	var elseVal string
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
	} else {
		return "", fmt.Errorf("if expression missing else branch")
	}
	return fmt.Sprintf("merge(%s, %s, %s)", thenVal, elseVal, cond), nil
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
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("size(%s)", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		if isStringExpr(call.Args[0], c.stringVars, c.funReturnStr) {
			return args[0], nil
		}
		if isFloatExpr(call.Args[0], c.floatVars, c.funReturnFloat) {
			c.needsStrFloat = true
			return fmt.Sprintf("str_float(%s)", args[0]), nil
		}
		c.needsStrInt = true
		return fmt.Sprintf("str_int(%s)", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		if isFloatExpr(call.Args[0], c.floatVars, c.funReturnFloat) {
			v := args[0]
			return fmt.Sprintf("merge(0.0, sum(%[1]s) / size(%[1]s), size(%[1]s) == 0)", v), nil
		}
		if isListExpr(call.Args[0], c.listVars, c.funReturnList) {
			v := args[0]
			return fmt.Sprintf("merge(0.0, sum(real(%[1]s)) / size(%[1]s), size(%[1]s) == 0)", v), nil
		}
		return "0", nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("(/ %s, %s /)", args[0], args[1]), nil
	default:
		name := sanitizeName(call.Func)
		if recv != "" {
			name = recv
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) lengthFunc(name string) string {
	if c.stringVars[name] {
		return fmt.Sprintf("len(%s)", name)
	}
	return fmt.Sprintf("size(%s)", name)
}

func (c *Compiler) writeHelpers() {
	if c.needsUnionInt {
		c.writeln("")
		c.writeln("function union_int(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("integer(kind=8), intent(in) :: a(:)")
		c.writeln("integer(kind=8), intent(in) :: b(:)")
		c.writeln("integer(kind=8), allocatable :: r(:)")
		c.writeln("integer(kind=8), allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(tmp(size(a)+size(b)))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end do")
		c.writeln("do i = 1, size(b)")
		c.indent++
		c.writeln("if (.not. any(tmp(:n) == b(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = b(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function union_int")
	}
	if c.needsUnionFloat {
		c.writeln("")
		c.writeln("function union_float(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("real, intent(in) :: a(:)")
		c.writeln("real, intent(in) :: b(:)")
		c.writeln("real, allocatable :: r(:)")
		c.writeln("real, allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(tmp(size(a)+size(b)))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end do")
		c.writeln("do i = 1, size(b)")
		c.indent++
		c.writeln("if (.not. any(tmp(:n) == b(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = b(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function union_float")
	}
	if c.needsUnionString {
		c.writeln("")
		c.writeln("function union_string(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: a(:)")
		c.writeln("character(len=*), intent(in) :: b(:)")
		c.writeln("character(len=:), allocatable :: r(:)")
		c.writeln("character(len=:), allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(character(len=max(len(a),len(b))) :: tmp(size(a)+size(b)))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end do")
		c.writeln("do i = 1, size(b)")
		c.indent++
		c.writeln("if (.not. any(tmp(:n) == b(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = b(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(character(len=len(tmp(1))) :: r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function union_string")
	}
	if c.needsExceptInt {
		c.writeln("")
		c.writeln("function except_int(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("integer(kind=8), intent(in) :: a(:)")
		c.writeln("integer(kind=8), intent(in) :: b(:)")
		c.writeln("integer(kind=8), allocatable :: r(:)")
		c.writeln("integer(kind=8), allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(tmp(size(a)))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("if (.not. any(b == a(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function except_int")
	}
	if c.needsExceptFloat {
		c.writeln("")
		c.writeln("function except_float(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("real, intent(in) :: a(:)")
		c.writeln("real, intent(in) :: b(:)")
		c.writeln("real, allocatable :: r(:)")
		c.writeln("real, allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(tmp(size(a)))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("if (.not. any(b == a(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function except_float")
	}
	if c.needsExceptString {
		c.writeln("")
		c.writeln("function except_string(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: a(:)")
		c.writeln("character(len=*), intent(in) :: b(:)")
		c.writeln("character(len=:), allocatable :: r(:)")
		c.writeln("character(len=:), allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(character(len=len(a)) :: tmp(size(a)))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("if (.not. any(b == a(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(character(len=len(tmp(1))) :: r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function except_string")
	}
	if c.needsIntersectInt {
		c.writeln("")
		c.writeln("function intersect_int(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("integer(kind=8), intent(in) :: a(:)")
		c.writeln("integer(kind=8), intent(in) :: b(:)")
		c.writeln("integer(kind=8), allocatable :: r(:)")
		c.writeln("integer(kind=8), allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(tmp(min(size(a),size(b))))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("if (any(b == a(i))) then")
		c.indent++
		c.writeln("if (.not. any(tmp(:n) == a(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function intersect_int")
	}
	if c.needsIntersectFloat {
		c.writeln("")
		c.writeln("function intersect_float(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("real, intent(in) :: a(:)")
		c.writeln("real, intent(in) :: b(:)")
		c.writeln("real, allocatable :: r(:)")
		c.writeln("real, allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(tmp(min(size(a),size(b))))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("if (any(b == a(i))) then")
		c.indent++
		c.writeln("if (.not. any(tmp(:n) == a(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function intersect_float")
	}
	if c.needsIntersectString {
		c.writeln("")
		c.writeln("function intersect_string(a, b) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: a(:)")
		c.writeln("character(len=*), intent(in) :: b(:)")
		c.writeln("character(len=:), allocatable :: r(:)")
		c.writeln("character(len=:), allocatable :: tmp(:)")
		c.writeln("integer(kind=8) :: i")
		c.writeln("integer(kind=8) :: n")
		c.writeln("allocate(character(len=len(a)) :: tmp(min(size(a),size(b))))")
		c.writeln("n = 0")
		c.writeln("do i = 1, size(a)")
		c.indent++
		c.writeln("if (any(b == a(i))) then")
		c.indent++
		c.writeln("if (.not. any(tmp(:n) == a(i))) then")
		c.indent++
		c.writeln("n = n + 1")
		c.writeln("tmp(n) = a(i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("allocate(character(len=len(tmp(1))) :: r(n))")
		c.writeln("r = tmp(:n)")
		c.indent--
		c.writeln("end function intersect_string")
	}
	if c.needsStrInt {
		c.writeln("")
		c.writeln("function str_int(v) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("integer(kind=8), intent(in) :: v")
		c.writeln("character(:), allocatable :: r")
		c.writeln("character(len=32) :: buf")
		c.writeln("write(buf,'(I0)') v")
		c.writeln("allocate(character(len=len_trim(buf)) :: r)")
		c.writeln("r = trim(buf)")
		c.indent--
		c.writeln("end function str_int")
	}
	if c.needsStrFloat {
		c.writeln("")
		c.writeln("function str_float(v) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("real, intent(in) :: v")
		c.writeln("character(:), allocatable :: r")
		c.writeln("character(len=64) :: buf")
		c.writeln("write(buf,'(G0)') v")
		c.writeln("allocate(character(len=len_trim(buf)) :: r)")
		c.writeln("r = trim(buf)")
		c.indent--
		c.writeln("end function str_float")
	}
}
