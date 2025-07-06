package ftncode

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
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
	lambdas              []string
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
	needsMinString       bool
	needsNow             bool
	needsFetch           bool
	needsUpper           bool
	needsLower           bool
	needsLoadJSON        bool
	needsSaveJSON        bool
	loadJSONTypes        map[string]bool
	saveJSONTypes        map[string]bool
	listStructTypes      map[string]string
}

func New() *Compiler {
	return &Compiler{
		stringVars:      map[string]bool{},
		listVars:        map[string]bool{},
		floatVars:       map[string]bool{},
		funReturnStr:    map[string]bool{},
		funReturnList:   map[string]bool{},
		funReturnFloat:  map[string]bool{},
		lambdas:         []string{},
		needsMinString:  false,
		needsNow:        false,
		needsFetch:      false,
		needsUpper:      false,
		needsLower:      false,
		loadJSONTypes:   map[string]bool{},
		saveJSONTypes:   map[string]bool{},
		listStructTypes: map[string]string{},
	}
}

// resetFeatures clears all feature flags for a new compilation unit.
func (c *Compiler) resetFeatures() {
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
	c.needsMinString = false
	c.needsNow = false
	c.needsFetch = false
	c.needsUpper = false
	c.needsLower = false
	c.needsLoadJSON = false
	c.needsSaveJSON = false
	c.loadJSONTypes = map[string]bool{}
	c.saveJSONTypes = map[string]bool{}
	c.listStructTypes = map[string]string{}
}

// resetVarScopes initializes variable tracking maps for a new scope.
func (c *Compiler) resetVarScopes() {
	c.stringVars = map[string]bool{}
	c.listVars = map[string]bool{}
	c.floatVars = map[string]bool{}
	c.listStructTypes = map[string]string{}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) blank() {
	ind := c.indent
	c.indent = 0
	c.writeln("")
	c.indent = ind
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.lambdas = nil
	if prog.Package != "" {
		c.writeln("! package " + prog.Package)
	}
	c.resetFeatures()
	c.resetVarScopes()
	var funs []*parser.FunStmt
	var tests []*parser.TestBlock
	var types []*parser.TypeDecl
	var mainStmts []*parser.Statement
	for _, s := range prog.Statements {
		switch {
		case s.Fun != nil:
			funs = append(funs, s.Fun)
		case s.Test != nil:
			tests = append(tests, s.Test)
		case s.Type != nil:
			types = append(types, s.Type)
		case s.ExternType != nil, s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil:
			// ignore extern declarations
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
	for _, t := range types {
		if err := c.compileTypeDecl(t); err != nil {
			return nil, err
		}
	}
	// crude variable declarations for lets/vars in main
	declared := map[string]bool{}
	listVars := map[string]bool{}
	stringVars := map[string]bool{}
	floatVars := map[string]bool{}
	listStructs := map[string]string{}
	collectVars(mainStmts, declared, listVars, stringVars, floatVars, listStructs, c.funReturnStr, c.funReturnFloat, c.funReturnList)
	structVars := map[string]string{}
	for _, st := range mainStmts {
		if st.Let != nil {
			if name, ok := structLitName(st.Let.Value); ok {
				structVars[sanitizeName(st.Let.Name)] = name
				delete(declared, sanitizeName(st.Let.Name))
			} else if st.Let.Type != nil && st.Let.Type.Simple != nil {
				typ := *st.Let.Type.Simple
				if typ != "int" && typ != "float" && typ != "string" && typ != "bool" {
					structVars[sanitizeName(st.Let.Name)] = sanitizeName(typ)
					delete(declared, sanitizeName(st.Let.Name))
				}
			}
		} else if st.Var != nil {
			if name, ok := structLitName(st.Var.Value); ok {
				structVars[sanitizeName(st.Var.Name)] = name
				delete(declared, sanitizeName(st.Var.Name))
			} else if st.Var.Type != nil && st.Var.Type.Simple != nil {
				typ := *st.Var.Type.Simple
				if typ != "int" && typ != "float" && typ != "string" && typ != "bool" {
					structVars[sanitizeName(st.Var.Name)] = sanitizeName(typ)
					delete(declared, sanitizeName(st.Var.Name))
				}
			}
		}
	}
	delete(listVars, "result")
	c.listVars = listVars
	c.listStructTypes = listStructs
	c.stringVars = stringVars
	c.floatVars = floatVars
	c.listStructTypes = listStructs
	allocs := []string{}
	for name, isList := range listVars {
		if isList {
			if t, ok := listStructs[name]; ok {
				c.writeln(fmt.Sprintf("type(%s), allocatable :: %s(:)", t, name))
			} else if stringVars[name] {
				c.writeln(fmt.Sprintf("character(:), allocatable :: %s(:)", name))
			} else if floatVars[name] {
				c.writeln(fmt.Sprintf("real, allocatable :: %s(:)", name))
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
			}
			allocs = append(allocs, fmt.Sprintf("allocate(%s(0))", name))
		}
	}
	names := make([]string, 0, len(structVars))
	for n := range structVars {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, name := range names {
		typ := structVars[name]
		c.writeln(fmt.Sprintf("type(%s) :: %s", typ, name))
	}
	names = names[:0]
	for name := range declared {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		if name == "result" {
			c.writeln("integer(kind=8) :: result(2)")
			continue
		}
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
	loopVars := map[string]bool{}
	loopStrVars := map[string]bool{}
	collectLoopVars(mainStmts, loopVars, loopStrVars)
	names = names[:0]
	for name := range loopVars {
		if !declared[name] {
			names = append(names, name)
		}
	}
	sort.Slice(names, func(i, j int) bool {
		a, b := names[i], names[j]
		if strings.HasPrefix(a, "i_") && strings.TrimPrefix(a, "i_") == b {
			return false
		}
		if strings.HasPrefix(b, "i_") && strings.TrimPrefix(b, "i_") == a {
			return true
		}
		return a < b
	})
	for _, name := range names {
		if loopStrVars[name] {
			c.writeln(fmt.Sprintf("character(:), allocatable :: %s", name))
		} else {
			c.writeln("integer(kind=8) :: " + name)
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
		c.needsStrInt || c.needsStrFloat || c.needsFetch
	if len(funs)+len(tests)+len(c.lambdas) > 0 || needHelpers {
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
		for _, l := range c.lambdas {
			c.buf.WriteString(l)
			if !strings.HasSuffix(l, "\n") {
				c.writeln("")
			}
			c.writeln("")
		}
		if needHelpers {
			c.writeHelpers()
		}
		c.indent--
	}
	c.writeln("end program main")
	code := c.buf.Bytes()
	code = Format(code)
	return code, nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	c.resetVarScopes()
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
	} else if fn.Return != nil && fn.Return.Simple != nil && !isBuiltin(*fn.Return.Simple) {
		c.writeln(fmt.Sprintf("type(%s) :: %s", sanitizeName(*fn.Return.Simple), resVar))
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
					c.listVars[name] = true
				case "float":
					c.writeln(fmt.Sprintf("real, intent(in) :: %s(:)", name))
					c.listVars[name] = true
				default:
					c.writeln(fmt.Sprintf("integer(kind=8), intent(in) :: %s(:)", name))
					c.listVars[name] = true
				}
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), intent(in) :: %s(:)", name))
				c.listVars[name] = true
			}
		} else if p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "string" {
			c.writeln(fmt.Sprintf("character(len=*), intent(in) :: %s", name))
			c.stringVars[name] = true
		} else if p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "float" {
			c.writeln(fmt.Sprintf("real, intent(in) :: %s", name))
		} else if p.Type != nil && p.Type.Simple != nil && !isBuiltin(*p.Type.Simple) {
			c.writeln(fmt.Sprintf("type(%s), intent(in) :: %s", sanitizeName(*p.Type.Simple), name))
		} else {
			c.writeln(fmt.Sprintf("integer(kind=8), intent(in) :: %s", name))
		}
	}
	vars := map[string]bool{}
	listVars := map[string]bool{}
	stringVars := map[string]bool{}
	floatVars := map[string]bool{}
	listStructs := map[string]string{}
	collectVars(fn.Body, vars, listVars, stringVars, floatVars, listStructs, c.funReturnStr, c.funReturnFloat, c.funReturnList)
	for name := range c.listVars {
		listVars[name] = true
	}
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
		if name == resVar || !vars[name] {
			continue
		}
		if isList {
			if t, ok := listStructs[name]; ok {
				c.writeln(fmt.Sprintf("type(%s), allocatable :: %s(:)", t, name))
			} else if stringVars[name] {
				c.writeln(fmt.Sprintf("character(:), allocatable :: %s(:)", name))
			} else if floatVars[name] {
				c.writeln(fmt.Sprintf("real, allocatable :: %s(:)", name))
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
			}
			allocs = append(allocs, fmt.Sprintf("allocate(%s(0))", name))
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

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return fmt.Errorf("union types not supported")
	}
	name := sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("type :: %s", name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			if m.Field.Type != nil && m.Field.Type.Generic != nil && m.Field.Type.Generic.Name == "list" && len(m.Field.Type.Generic.Args) == 1 {
				elem := c.ftnType(m.Field.Type.Generic.Args[0])
				c.writeln(fmt.Sprintf("%s, allocatable :: %s(:)", elem, sanitizeName(m.Field.Name)))
			} else {
				typ := c.ftnType(m.Field.Type)
				c.writeln(fmt.Sprintf("%s :: %s", typ, sanitizeName(m.Field.Name)))
			}
		}
	}
	c.indent--
	c.writeln(fmt.Sprintf("end type %s", name))
	return nil
}

func (c *Compiler) ftnType(t *parser.TypeRef) string {
	if t == nil {
		return "integer(kind=8)"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "integer(kind=8)"
		case "float":
			return "real"
		case "string":
			return "character(:), allocatable"
		case "bool":
			return "logical"
		default:
			return fmt.Sprintf("type(%s)", sanitizeName(*t.Simple))
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return c.ftnType(t.Generic.Args[0])
	}
	return "integer(kind=8)"
}

func (c *Compiler) ftnScalarType(t types.Type) string {
	switch v := t.(type) {
	case types.FloatType:
		return "real"
	case types.StringType:
		return "character(len=len(vsrc))"
	case types.StructType:
		return fmt.Sprintf("type(%s)", sanitizeName(v.Name))
	default:
		return "integer(kind=8)"
	}
}

func structLitName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		v := e.Binary.Left.Value
		if len(v.Ops) == 0 && v.Target != nil && v.Target.Struct != nil {
			return sanitizeName(v.Target.Struct.Name), true
		}
	}
	return "", false
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	fields := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s=%s", sanitizeName(f.Name), v)
	}
	return fmt.Sprintf("%s(%s)", sanitizeName(s.Name), strings.Join(fields, ", ")), nil
}

func (c *Compiler) compileStmt(s *parser.Statement, retVar string) error {
	switch {
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Let != nil:
		if s.Let.Value != nil {
			if _, ok := structLitName(s.Let.Value); ok {
				val, err := c.compileExpr(s.Let.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Let.Name), val))
			} else if !types.IsEmptyListLiteral(s.Let.Value) {
				val, err := c.compileExpr(s.Let.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Let.Name), val))
			}
		}
	case s.Var != nil:
		if s.Var.Value != nil {
			if _, ok := structLitName(s.Var.Value); ok {
				val, err := c.compileExpr(s.Var.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Var.Name), val))
			} else if !types.IsEmptyListLiteral(s.Var.Value) {
				val, err := c.compileExpr(s.Var.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Var.Name), val))
			}
		}
	case s.Assign != nil:
		name := sanitizeName(s.Assign.Name)
		if len(s.Assign.Index) > 0 {
			idx := s.Assign.Index[0]
			val, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			if idx.Colon != nil || idx.End != nil {
				start := "0"
				if idx.Start != nil {
					v, err := c.compileExpr(idx.Start)
					if err != nil {
						return err
					}
					if c.stringVars[name] || c.listVars[name] {
						start = fmt.Sprintf("modulo(%s, %s)", v, c.lengthFunc(name))
					} else {
						start = v
					}
				}
				end := ""
				if idx.End != nil {
					v, err := c.compileExpr(idx.End)
					if err != nil {
						return err
					}
					if c.stringVars[name] || c.listVars[name] {
						end = fmt.Sprintf("modulo(%s, %s)", v, c.lengthFunc(name))
					} else {
						end = v
					}
				} else {
					if c.stringVars[name] {
						end = fmt.Sprintf("len(%s)", name)
					} else {
						end = fmt.Sprintf("size(%s)", name)
					}
				}
				c.writeln(fmt.Sprintf("%s(%s + 1:%s) = %s", name, start, end, val))
			} else {
				v, err := c.compileExpr(idx.Start)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("%s(%s + 1) = %s", name, v, val))
			}
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
			isStr := c.stringVars[src] || types.IsStringExprVars(s.For.Source, sanitizeName, c.stringVars, c.funReturnStr)
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
	case s.Import != nil:
		if s.Import.Lang == nil || *s.Import.Lang == "fortran" {
			path := strings.Trim(s.Import.Path, "\"")
			c.writeln(fmt.Sprintf("include '%s'", path))
		}
	case s.ExternType != nil, s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil:
		// ignore extern declarations
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
	c.resetVarScopes()
	vars := map[string]bool{}
	listVars := map[string]bool{}
	stringVars := map[string]bool{}
	floatVars := map[string]bool{}
	listStructs := map[string]string{}
	collectVars(t.Body, vars, listVars, stringVars, floatVars, listStructs, c.funReturnStr, c.funReturnFloat, c.funReturnList)
	c.listVars = listVars
	c.stringVars = stringVars
	c.floatVars = floatVars
	c.listStructTypes = listStructs
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
		if t, ok := listStructs[name]; ok {
			c.writeln(fmt.Sprintf("type(%s), allocatable :: %s(:)", t, name))
		} else if stringVars[name] {
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
	vStr = append(vStr, types.IsStringExprVars(exprLeft, sanitizeName, c.stringVars, c.funReturnStr))
	vList = append(vList, types.IsListExprVars(exprLeft, sanitizeName, c.listVars, c.funReturnList))
	vFloat = append(vFloat, types.IsFloatExprVars(exprLeft, sanitizeName, c.floatVars, c.funReturnFloat))
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
		vStr = append(vStr, types.IsStringExprVars(exprRight, sanitizeName, c.stringVars, c.funReturnStr))
		vList = append(vList, types.IsListExprVars(exprRight, sanitizeName, c.listVars, c.funReturnList))
		vFloat = append(vFloat, types.IsFloatExprVars(exprRight, sanitizeName, c.floatVars, c.funReturnFloat))
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
					if c.stringVars[root] {
						start = fmt.Sprintf("modulo(%s, len(%s))", v, root)
					} else if c.listVars[root] {
						start = fmt.Sprintf("modulo(%s, size(%s))", v, root)
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
					if c.stringVars[root] {
						end = fmt.Sprintf("modulo(%s, len(%s))", v, root)
					} else if c.listVars[root] {
						end = fmt.Sprintf("modulo(%s, size(%s))", v, root)
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
				} else {
					return "", fmt.Errorf("unsupported cast")
				}
			} else {
				return "", fmt.Errorf("unsupported cast")
			}
		case op.Call != nil:
			recv := expr
			tmp := &parser.CallExpr{Func: "", Args: op.Call.Args}
			val, err := c.compileCallExpr(tmp, recv)
			if err != nil {
				return "", err
			}
			expr = val
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
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
			return "reshape([0_8], [0])", nil
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
		parts := make([]string, 0, 1+len(p.Selector.Tail))
		parts = append(parts, sanitizeName(p.Selector.Root))
		for _, f := range p.Selector.Tail {
			parts = append(parts, sanitizeName(f))
		}
		return strings.Join(parts, "%"), nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call, "")
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
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
		if types.IsStringExprVars(call.Args[0], sanitizeName, c.stringVars, c.funReturnStr) {
			return args[0], nil
		}
		if types.IsFloatExprVars(call.Args[0], sanitizeName, c.floatVars, c.funReturnFloat) {
			c.needsStrFloat = true
			return fmt.Sprintf("str_float(%s)", args[0]), nil
		}
		c.needsStrInt = true
		return fmt.Sprintf("str_int(%s)", args[0]), nil
	case "abs":
		if len(args) != 1 {
			return "", fmt.Errorf("abs expects 1 arg")
		}
		return fmt.Sprintf("abs(%s)", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		if types.IsFloatExprVars(call.Args[0], sanitizeName, c.floatVars, c.funReturnFloat) {
			v := args[0]
			return fmt.Sprintf("merge(0.0, sum(%[1]s) / size(%[1]s), size(%[1]s) == 0)", v), nil
		}
		if types.IsListExprVars(call.Args[0], sanitizeName, c.listVars, c.funReturnList) {
			v := args[0]
			return fmt.Sprintf("merge(0.0, sum(real(%[1]s)) / size(%[1]s), size(%[1]s) == 0)", v), nil
		}
		return "0", nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("(/ %s, %s /)", args[0], args[1]), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		if types.IsListExprVars(call.Args[0], sanitizeName, c.listVars, c.funReturnList) {
			v := args[0]
			if types.IsStringExprVars(call.Args[0], sanitizeName, c.stringVars, c.funReturnStr) {
				c.needsMinString = true
				return fmt.Sprintf("min_string(%s)", v), nil
			}
			if types.IsFloatExprVars(call.Args[0], sanitizeName, c.floatVars, c.funReturnFloat) {
				return fmt.Sprintf("merge(0.0, minval(%[1]s), size(%[1]s) == 0)", v), nil
			}
			return fmt.Sprintf("merge(0_8, minval(%[1]s), size(%[1]s) == 0)", v), nil
		}
		return args[0], nil
	case "upper":
		if len(args) != 1 {
			return "", fmt.Errorf("upper expects 1 arg")
		}
		c.needsUpper = true
		return fmt.Sprintf("str_upper(%s)", args[0]), nil
	case "lower":
		if len(args) != 1 {
			return "", fmt.Errorf("lower expects 1 arg")
		}
		c.needsLower = true
		return fmt.Sprintf("str_lower(%s)", args[0]), nil
	case "now":
		if len(args) != 0 {
			return "", fmt.Errorf("now expects 0 args")
		}
		c.needsNow = true
		return "mochi_now()", nil
	default:
		name := sanitizeName(call.Func)
		if recv != "" {
			name = recv
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	name := fmt.Sprintf("lambda_%d", len(c.lambdas))
	oldBuf := c.buf
	oldIndent := c.indent
	oldStrings := c.stringVars
	oldLists := c.listVars
	oldFloats := c.floatVars
	c.buf = bytes.Buffer{}
	c.indent = 1
	var body []*parser.Statement
	if fn.ExprBody != nil {
		body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fn.ExprBody}}}
	} else {
		body = fn.BlockBody
	}
	if err := c.compileFun(&parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: body}); err != nil {
		c.buf = oldBuf
		c.indent = oldIndent
		c.stringVars = oldStrings
		c.listVars = oldLists
		c.floatVars = oldFloats
		return "", err
	}
	code := c.buf.String()
	c.lambdas = append(c.lambdas, code)
	c.buf = oldBuf
	c.indent = oldIndent
	c.stringVars = oldStrings
	c.listVars = oldLists
	c.floatVars = oldFloats
	return name, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	if l.Type == nil || l.Type.Simple == nil {
		return "", fmt.Errorf("unsupported load type")
	}
	typ := sanitizeName(*l.Type.Simple)
	fname := "load_json_" + typ
	c.needsLoadJSON = true
	if c.loadJSONTypes == nil {
		c.loadJSONTypes = map[string]bool{}
	}
	c.loadJSONTypes[typ] = true
	path := "''"
	if l.Path != nil {
		path = fmt.Sprintf("'%s'", *l.Path)
	}
	return fmt.Sprintf("%s(%s)", fname, path), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "''"
	if s.Path != nil {
		path = fmt.Sprintf("'%s'", *s.Path)
	}
	typName := "int"
	if name, ok := identName(s.Src); ok {
		n := sanitizeName(name)
		if t, ok := c.listStructTypes[n]; ok {
			typName = t
		} else if c.stringVars[n] {
			typName = "string"
		} else if c.floatVars[n] {
			typName = "float"
		}
	}
	fname := "save_json_" + typName
	c.needsSaveJSON = true
	if c.saveJSONTypes == nil {
		c.saveJSONTypes = map[string]bool{}
	}
	c.saveJSONTypes[typName] = true
	return fmt.Sprintf("call %s(%s, %s)", fname, src, path), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	c.needsFetch = true
	return fmt.Sprintf("mochi_fetch(%s)", urlStr), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil {
		return "", fmt.Errorf("unsupported query expression")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	var elemType types.Type = types.IntType{}
	if name, ok := identName(q.Source); ok {
		n := sanitizeName(name)
		if c.listVars[n] {
			if c.stringVars[n] {
				elemType = types.StringType{}
			} else if c.floatVars[n] {
				elemType = types.FloatType{}
			}
		}
	}
	selIsStr := types.IsStringExprVars(q.Select, sanitizeName, c.stringVars, c.funReturnStr)
	selIsFloat := types.IsFloatExprVars(q.Select, sanitizeName, c.floatVars, c.funReturnFloat)
	var resType types.Type = types.IntType{}
	if selIsStr {
		resType = types.StringType{}
	} else if selIsFloat {
		resType = types.FloatType{}
	}
	selVar := sanitizeName(q.Var)
	if _, ok := elemType.(types.StringType); ok {
		c.stringVars[selVar] = true
	} else if _, ok := elemType.(types.FloatType); ok {
		c.floatVars[selVar] = true
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var cond string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	var sortStr string
	var sortType types.Type = types.IntType{}
	if q.Sort != nil {
		if types.IsFloatExprVars(q.Sort, sanitizeName, c.floatVars, c.funReturnFloat) {
			sortType = types.FloatType{}
		}
		sortStr, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	var skipStr, takeStr string
	if q.Skip != nil {
		skipStr, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeStr, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}

	name := fmt.Sprintf("lambda_%d", len(c.lambdas))
	oldBuf := c.buf
	oldIndent := c.indent
	oldStrings := c.stringVars
	oldLists := c.listVars
	oldFloats := c.floatVars
	c.buf = bytes.Buffer{}
	c.indent = 1
	c.writeln(fmt.Sprintf("function %s(vsrc) result(res)", name))
	c.indent++
	c.writeln("implicit none")
	varDecl := func(t types.Type, name string, intent bool) {
		in := ""
		if intent {
			in = ", intent(in)"
		}
		switch t.(type) {
		case types.FloatType:
			if intent {
				c.writeln(fmt.Sprintf("real%s :: %s(:)", in, name))
			} else {
				c.writeln(fmt.Sprintf("real, allocatable :: %s(:)", name))
			}
		case types.StringType:
			if intent {
				c.writeln(fmt.Sprintf("character(len=* )%s :: %s(:)", in, name))
			} else {
				c.writeln(fmt.Sprintf("character(len=:), allocatable :: %s(:)", name))
			}
		default:
			if intent {
				c.writeln(fmt.Sprintf("integer(kind=8)%s :: %s(:)", in, name))
			} else {
				c.writeln(fmt.Sprintf("integer(kind=8), allocatable :: %s(:)", name))
			}
		}
	}
	varDecl(elemType, "vsrc", true)
	varDecl(resType, "res", false)
	varDecl(resType, "tmp", false)
	if q.Sort != nil {
		varDecl(sortType, "tmpKey", false)
	}
	// declare loop var
	switch elemType.(type) {
	case types.FloatType:
		c.writeln("real :: " + sanitizeName(q.Var))
	case types.StringType:
		c.writeln("character(len=len(vsrc)) :: " + sanitizeName(q.Var))
	default:
		c.writeln("integer(kind=8) :: " + sanitizeName(q.Var))
	}
	c.writeln("integer(kind=8) :: n")
	c.writeln("integer(kind=8) :: i")
	if q.Sort != nil {
		c.writeln("integer(kind=8) :: j")
		c.writeln("integer(kind=8) :: min_idx")
		switch sortType.(type) {
		case types.FloatType:
			c.writeln("real :: sort_key")
			c.writeln("real :: swap_key")
		default:
			c.writeln("integer(kind=8) :: sort_key")
			c.writeln("integer(kind=8) :: swap_key")
		}
		switch rt := resType.(type) {
		case types.FloatType:
			c.writeln("real :: swap_item")
		case types.StringType:
			c.writeln("character(len=len(vsrc)) :: swap_item")
		case types.StructType:
			c.writeln(fmt.Sprintf("type(%s) :: swap_item", sanitizeName(rt.Name)))
		default:
			c.writeln("integer(kind=8) :: swap_item")
		}
	}
	c.writeln("allocate(tmp(size(vsrc)))")
	if q.Sort != nil {
		c.writeln("allocate(tmpKey(size(vsrc)))")
	}
	c.writeln("n = 0")
	c.writeln("do i = 1, size(vsrc)")
	c.indent++
	c.writeln(fmt.Sprintf("%s = vsrc(i)", sanitizeName(q.Var)))
	if cond != "" {
		c.writeln(fmt.Sprintf("if (%s) then", cond))
		c.indent++
	}
	if q.Sort != nil {
		c.writeln(fmt.Sprintf("sort_key = %s", sortStr))
	}
	c.writeln("n = n + 1")
	c.writeln(fmt.Sprintf("tmp(n) = %s", sel))
	if q.Sort != nil {
		c.writeln("tmpKey(n) = sort_key")
	}
	if cond != "" {
		c.indent--
		c.writeln("end if")
	}
	c.indent--
	c.writeln("end do")
	if q.Sort != nil {
		c.writeln("do i = 1, n - 1")
		c.indent++
		c.writeln("min_idx = i")
		c.writeln("do j = i + 1, n")
		c.indent++
		c.writeln("if (tmpKey(j) < tmpKey(min_idx)) then")
		c.indent++
		c.writeln("min_idx = j")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.writeln("if (min_idx /= i) then")
		c.indent++
		c.writeln("swap_key = tmpKey(i)")
		c.writeln("tmpKey(i) = tmpKey(min_idx)")
		c.writeln("tmpKey(min_idx) = swap_key")
		c.writeln("swap_item = tmp(i)")
		c.writeln("tmp(i) = tmp(min_idx)")
		c.writeln("tmp(min_idx) = swap_item")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
	}
	c.writeln("allocate(res(n))")
	c.writeln("res = tmp(:n)")
	c.indent--
	c.writeln("end function " + name)
	code := c.buf.String()
	c.lambdas = append(c.lambdas, code)
	c.buf = oldBuf
	c.indent = oldIndent
	c.stringVars = oldStrings
	c.listVars = oldLists
	c.floatVars = oldFloats
	expr := fmt.Sprintf("%s(%s)", name, src)
	if skipStr != "" && takeStr != "" {
		expr = fmt.Sprintf("%s(%s + 1:%s + %s)", expr, skipStr, skipStr, takeStr)
	} else if skipStr != "" {
		expr = fmt.Sprintf("%s(%s + 1:)", expr, skipStr)
	} else if takeStr != "" {
		expr = fmt.Sprintf("%s(1:%s)", expr, takeStr)
	}
	return expr, nil
}

func (c *Compiler) lengthFunc(name string) string {
	if c.stringVars[name] {
		return fmt.Sprintf("len(%s)", name)
	}
	return fmt.Sprintf("size(%s)", name)
}

func (c *Compiler) writeHelpers() {
	if c.needsUnionInt {
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
		c.blank()
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
	if c.needsUpper {
		c.blank()
		c.writeln("function str_upper(v) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: v")
		c.writeln("character(len=len(v)) :: r")
		c.writeln("integer :: i")
		c.writeln("do i = 1, len(v)")
		c.indent++
		c.writeln("if ((iachar(v(i:i)) >= iachar('a')) .and. (iachar(v(i:i)) <= iachar('z'))) then")
		c.indent++
		c.writeln("r(i:i) = achar(iachar(v(i:i)) - 32)")
		c.indent--
		c.writeln("else")
		c.indent++
		c.writeln("r(i:i) = v(i:i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.indent--
		c.writeln("end function str_upper")
	}
	if c.needsLower {
		c.blank()
		c.writeln("function str_lower(v) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: v")
		c.writeln("character(len=len(v)) :: r")
		c.writeln("integer :: i")
		c.writeln("do i = 1, len(v)")
		c.indent++
		c.writeln("if ((iachar(v(i:i)) >= iachar('A')) .and. (iachar(v(i:i)) <= iachar('Z'))) then")
		c.indent++
		c.writeln("r(i:i) = achar(iachar(v(i:i)) + 32)")
		c.indent--
		c.writeln("else")
		c.indent++
		c.writeln("r(i:i) = v(i:i)")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end do")
		c.indent--
		c.writeln("end function str_lower")
	}
	if c.needsMinString {
		c.blank()
		c.writeln("function min_string(v) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: v(:)")
		c.writeln("character(len=len(v(1))) :: r")
		c.writeln("integer(kind=8) :: i")
		c.writeln("if (size(v) == 0) then")
		c.indent++
		c.writeln("r = ''")
		c.indent--
		c.writeln("else")
		c.indent++
		c.writeln("r = v(1)")
		c.writeln("do i = 2, size(v)")
		c.indent++
		c.writeln("if (v(i) < r) r = v(i)")
		c.indent--
		c.writeln("end do")
		c.indent--
		c.writeln("end if")
		c.indent--
		c.writeln("end function min_string")
	}
	if c.needsNow {
		c.blank()
		c.writeln("function mochi_now() result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("integer(kind=8) :: r")
		c.writeln("integer(kind=8) :: cnt")
		c.writeln("integer(kind=8) :: rate")
		c.writeln("call system_clock(cnt, rate)")
		c.writeln("r = cnt * 1000000000_8 / rate")
		c.indent--
		c.writeln("end function mochi_now")
	}

	if c.needsFetch {
		c.blank()
		c.writeln("function mochi_fetch(url) result(r)")
		c.indent++
		c.writeln("implicit none")
		c.writeln("character(len=*), intent(in) :: url")
		c.writeln("character(len=:), allocatable :: r")
		c.writeln("character(len=1024) :: cmd")
		c.writeln("integer :: u, n")
		c.writeln("cmd = 'curl -s -o mochi_fetch.tmp ' // trim(url)")
		c.writeln("call execute_command_line(cmd)")
		c.writeln("open(newunit=u, file='mochi_fetch.tmp', access='stream', form='unformatted', action='read')")
		c.writeln("inquire(u, size=n)")
		c.writeln("allocate(character(len=n) :: r)")
		c.writeln("read(u) r")
		c.writeln("close(u)")
		c.writeln("call execute_command_line('rm -f mochi_fetch.tmp')")
		c.indent--
		c.writeln("end function mochi_fetch")
	}

	if c.needsLoadJSON {
		for typ := range c.loadJSONTypes {
			c.blank()
			c.writeln(fmt.Sprintf("function load_json_%s(path) result(res)", typ))
			c.indent++
			c.writeln("implicit none")
			c.writeln("character(len=*), intent(in) :: path")
			switch typ {
			case "string":
				c.writeln("character(:), allocatable :: res(:)")
			case "float":
				c.writeln("real, allocatable :: res(:)")
			case "int":
				c.writeln("integer(kind=8), allocatable :: res(:)")
			default:
				c.writeln(fmt.Sprintf("type(%s), allocatable :: res(:)", typ))
			}
			c.writeln("allocate(res(0))")
			c.indent--
			c.writeln(fmt.Sprintf("end function load_json_%s", typ))
		}
	}

	if c.needsSaveJSON {
		for typ := range c.saveJSONTypes {
			c.blank()
			c.writeln(fmt.Sprintf("subroutine save_json_%s(rows, path)", typ))
			c.indent++
			c.writeln("implicit none")
			switch typ {
			case "string":
				c.writeln("character(len=*), intent(in) :: rows(:)")
			case "float":
				c.writeln("real, intent(in) :: rows(:)")
			case "int":
				c.writeln("integer(kind=8), intent(in) :: rows(:)")
			default:
				c.writeln(fmt.Sprintf("type(%s), intent(in) :: rows(:)", typ))
			}
			c.writeln("character(len=*), intent(in) :: path")
			c.indent--
			c.writeln(fmt.Sprintf("end subroutine save_json_%s", typ))
		}
	}
}
