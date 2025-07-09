//go:build slow

package ftncode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a very small subset of Mochi into Fortran 90.
// Only basic constructs used by the simpler test programs are supported.
// Unsupported syntax results in a compilation error.
type Compiler struct {
	buf         bytes.Buffer
	decl        bytes.Buffer
	indent      int
	functions   []*parser.FunStmt
	currentFunc string
	env         *types.Env
	declared    map[string]bool
	tmpIndex    int
}

// New creates a new compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, declared: make(map[string]bool)}
}

// Compile converts a parsed Mochi program into Fortran source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.decl.Reset()
	c.functions = nil
	c.declared = make(map[string]bool)
	c.tmpIndex = 0
	name := "main"
	if prog.Package != "" {
		name = sanitize(prog.Package)
	}
	c.writeln(fmt.Sprintf("program %s", name))
	c.indent++
	c.writeln("implicit none")

	var globals []*parser.LetStmt
	var typesDecl []*parser.TypeDecl
	for _, st := range prog.Statements {
		switch {
		case st.Fun != nil:
			c.functions = append(c.functions, st.Fun)
		case st.Type != nil:
			typesDecl = append(typesDecl, st.Type)
		case st.Let != nil:
			globals = append(globals, st.Let)
		case st.Var != nil:
			globals = append(globals, &parser.LetStmt{Name: st.Var.Name, Value: st.Var.Value, Type: st.Var.Type})
		}
	}

	for _, td := range typesDecl {
		if err := c.compileTypeDecl(td); err != nil {
			return nil, err
		}
	}

	for _, g := range globals {
		if err := c.compileLetDecl(g); err != nil {
			return nil, err
		}
	}

	for _, st := range prog.Statements {
		if st.Fun != nil || st.Type != nil {
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

	body := c.buf.Bytes()
	var out bytes.Buffer
	first := bytes.IndexByte(body, '\n')
	if first < 0 {
		return body, nil
	}
	second := bytes.IndexByte(body[first+1:], '\n')
	if second >= 0 {
		second += first + 1
		out.Write(body[:second+1])
		out.Write(c.decl.Bytes())
		out.Write(body[second+1:])
	} else {
		out.Write(body)
	}
	return out.Bytes(), nil
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

func (c *Compiler) compileLetDecl(l *parser.LetStmt) error {
	if c.declared[l.Name] {
		return nil
	}
	typ := typeName(l.Type)
	if lst := listLiteral(l.Value); lst != nil {
		c.writelnDecl(fmt.Sprintf("%s, dimension(%d) :: %s", typ, len(lst.Elems), l.Name))
	} else {
		c.writelnDecl(fmt.Sprintf("%s :: %s", typ, l.Name))
	}
	c.declared[l.Name] = true
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	st, ok := c.env.GetStruct(t.Name)
	if !ok {
		return fmt.Errorf("unknown struct %s", t.Name)
	}
	c.writelnDecl(fmt.Sprintf("type :: %s", t.Name))
	c.indent++
	for _, f := range st.Order {
		c.writelnDecl(fmt.Sprintf("%s :: %s", typeNameFromTypes(st.Fields[f]), f))
	}
	c.indent--
	c.writelnDecl(fmt.Sprintf("end type %s", t.Name))
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
		c.writelnDecl(fmt.Sprintf("integer :: %s", f.Name))
		c.declared[f.Name] = true
	}
	if f.RangeEnd == nil {
		lst := listLiteral(f.Source)
		if lst != nil {
			arr := fmt.Sprintf("arr%d", c.tmpIndex)
			idx := fmt.Sprintf("i%d", c.tmpIndex)
			c.tmpIndex++
			elems := make([]string, len(lst.Elems))
			for i, e := range lst.Elems {
				v, err := c.compileExpr(e)
				if err != nil {
					return err
				}
				elems[i] = v
			}
			c.writelnDecl(fmt.Sprintf("integer, dimension(%d) :: %s = (/%s/)", len(lst.Elems), arr, strings.Join(elems, ",")))
			c.writelnDecl(fmt.Sprintf("integer :: %s", idx))
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
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		idx := fmt.Sprintf("i%d", c.tmpIndex)
		c.tmpIndex++
		c.writelnDecl(fmt.Sprintf("integer :: %s", idx))
		c.writeln(fmt.Sprintf("do %s = 1, size(%s)", idx, src))
		c.indent++
		c.writeln(fmt.Sprintf("%s = %s(%s)", f.Name, src, idx))
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
	if fn.Return == nil {
		if t, err := c.env.GetVar(fn.Name); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				retType = typeNameFromTypes(ft.Return)
			}
		}
	}
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
	leftIsStr := types.IsStringUnary(b.Left, c.env)
	leftIsLit := literalString(b.Left) != nil
	res := left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightIsStr := types.IsStringPostfix(op.Right, c.env)
		rightIsLit := literalStringPrimary(op.Right.Target) != nil
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
		case "as":
			if lit := literalString(b.Left); lit != nil && r == "int" {
				if iv, err := strconv.Atoi(*lit); err == nil {
					res = fmt.Sprintf("%d", iv)
					continue
				}
			}
			return "", fmt.Errorf("unsupported cast")
		case "in":
			res = fmt.Sprintf("any(%s == %s)", r, res)
			continue
		case "+":
			if leftIsStr || rightIsStr {
				l := res
				if leftIsStr && !leftIsLit {
					l = fmt.Sprintf("trim(%s)", l)
				}
				rr := r
				if rightIsStr && !rightIsLit {
					rr = fmt.Sprintf("trim(%s)", rr)
				}
				res = fmt.Sprintf("%s // %s", l, rr)
				leftIsStr = true
				leftIsLit = false
				continue
			}
		}
		res = fmt.Sprintf("(%s %s %s)", res, opStr, r)
		leftIsStr = false
		leftIsLit = false
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
		case op.Field != nil:
			res = fmt.Sprintf("%s%%%s", res, op.Field.Name)
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				switch *op.Cast.Type.Simple {
				case "int":
					if lit := literalStringPrimary(p.Target); lit != nil {
						if iv, err := strconv.Atoi(*lit); err == nil {
							res = fmt.Sprintf("%d", iv)
							continue
						}
					}
					res = fmt.Sprintf("int(%s)", res)
				case "float":
					res = fmt.Sprintf("real(%s)", res)
				default:
					if st, ok := c.env.GetStruct(*op.Cast.Type.Simple); ok {
						if ml := mapLiteralPrimary(p.Target); ml != nil {
							fields := make([]string, len(st.Order))
							for i, f := range st.Order {
								var val *parser.Expr
								for _, it := range ml.Items {
									if keyName(it.Key) == f {
										val = it.Value
										break
									}
								}
								if val == nil {
									return "", fmt.Errorf("missing field %s", f)
								}
								v, err := c.compileExpr(val)
								if err != nil {
									return "", err
								}
								fields[i] = v
							}
							res = fmt.Sprintf("%s(%s)", st.Name, strings.Join(fields, ","))
							continue
						}
					}
					return "", fmt.Errorf("unsupported cast")
				}
			} else {
				return "", fmt.Errorf("unsupported cast")
			}
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
		res := p.Selector.Root
		for _, t := range p.Selector.Tail {
			res += "%" + t
		}
		return res, nil
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
	switch call.Func {
	case "len":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		// heuristic: use len() for strings, size() otherwise
		if lit := call.Args[0].Binary.Left.Value.Target; lit != nil && lit.Lit != nil && lit.Lit.Str != nil {
			return fmt.Sprintf("len(%s)", args[0]), nil
		}
		return fmt.Sprintf("size(%s)", args[0]), nil
	case "sum":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("sum(%s)", args[0]), nil
	case "min":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		return fmt.Sprintf("minval(%s)", args[0]), nil
	case "max":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("maxval(%s)", args[0]), nil
	case "avg":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		arr := args[0]
		return fmt.Sprintf("(sum(%s)/real(size(%s)))", arr, arr), nil
	case "count":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("size(%s)", args[0]), nil
	case "str":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		tmp := fmt.Sprintf("s%d", c.tmpIndex)
		c.tmpIndex++
		c.writelnDecl(fmt.Sprintf("character(len=100) :: %s", tmp))
		c.writeln(fmt.Sprintf("write(%s,'(G0)') %s", tmp, args[0]))
		return tmp, nil
	case "substring":
		if len(call.Args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("%s(%s+1:%s)", args[0], args[1], args[2]), nil
	case "append":
		if len(call.Args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		arr := args[0]
		elem := args[1]
		tmp := fmt.Sprintf("app%d", c.tmpIndex)
		c.tmpIndex++
		c.writelnDecl(fmt.Sprintf("integer, allocatable, dimension(:) :: %s", tmp))
		c.writeln(fmt.Sprintf("allocate(%s(size(%s)+1))", tmp, arr))
		c.writeln(fmt.Sprintf("%s(1:size(%s)) = %s", tmp, arr, arr))
		c.writeln(fmt.Sprintf("%s(size(%s)+1) = %s", tmp, arr, elem))
		return tmp, nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ",")), nil
	}
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

func (c *Compiler) writelnDecl(s string) {
	for i := 0; i < c.indent; i++ {
		c.decl.WriteString("  ")
	}
	c.decl.WriteString(s)
	c.decl.WriteByte('\n')
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

func literalString(e *parser.Unary) *string {
	if e == nil || e.Value == nil || e.Value.Target == nil {
		return nil
	}
	if lit := e.Value.Target.Lit; lit != nil && lit.Str != nil {
		return lit.Str
	}
	return nil
}

func literalStringPrimary(p *parser.Primary) *string {
	if p == nil || p.Lit == nil || p.Lit.Str == nil {
		return nil
	}
	return p.Lit.Str
}

func mapLiteralPrimary(p *parser.Primary) *parser.MapLiteral {
	if p == nil {
		return nil
	}
	return p.Map
}

func keyName(e *parser.Expr) string {
	if e == nil || e.Binary == nil {
		return ""
	}
	if len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return ""
	}
	v := u.Value
	if len(v.Ops) != 0 || v.Target == nil {
		return ""
	}
	if v.Target.Lit != nil && v.Target.Lit.Str != nil {
		return *v.Target.Lit.Str
	}
	if v.Target.Selector != nil && len(v.Target.Selector.Tail) == 0 {
		return v.Target.Selector.Root
	}
	return ""
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
		default:
			return *t.Simple
		}
	}
	return "integer"
}

func typeNameFromTypes(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return "integer"
	case types.BoolType:
		return "logical"
	case types.StringType:
		return "character(len=100)"
	case types.FloatType:
		return "real"
	case types.StructType:
		st := t.(types.StructType)
		return st.Name
	default:
		return "integer"
	}
}
