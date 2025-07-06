package swiftcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Swift source code (very limited subset).
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	locals  map[string]types.Type
	helpers map[string]bool
	funcRet types.Type
}

func New(env *types.Env) *Compiler {
	return &Compiler{
		env:     env,
		locals:  map[string]types.Type{},
		helpers: map[string]bool{},
	}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.helpers = map[string]bool{}

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent = 0

	// type declarations first
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// test blocks
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// global variable declarations
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if s.Let != nil || s.Var != nil {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
	}
	// main body
	c.writeln("func main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if s.Let != nil || s.Var != nil {
			// already emitted as global
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(name + "()")
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("main()")

	bodyBytes := c.buf.Bytes()

	c.buf = oldBuf
	c.indent = 0

	c.writeln("import Foundation")
	c.writeln("")
	c.writeExpectFunc(prog)
	c.emitRuntime()

	c.buf.Write(bodyBytes)
	code := c.buf.Bytes()
	return Format(code), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("_ %s: %s", p.Name, c.compileType(p.Type))
	}
	ret := c.compileType(fn.Return)
	c.writeln(fmt.Sprintf("func %s(%s) -> %s {", fn.Name, strings.Join(params, ", "), ret))
	c.indent++
	oldEnv := c.env
	oldRet := c.funcRet
	oldLocals := c.locals
	c.locals = map[string]types.Type{}
	c.funcRet = resolveTypeRef(fn.Return, oldEnv)
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		for _, p := range fn.Params {
			if p.Type != nil {
				t := resolveTypeRef(p.Type, oldEnv)
				c.env.SetVar(p.Name, t, true)
				c.locals[p.Name] = t
			}
		}
	}
	for _, p := range fn.Params {
		decl := "var"
		if !paramAssigned(fn.Body, p.Name) {
			decl = "let"
		}
		c.writeln(fmt.Sprintf("%s %s = %s", decl, p.Name, p.Name))
	}
	if len(fn.Params) > 0 {
		c.writeln("")
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = oldEnv
	}
	c.locals = oldLocals
	c.funcRet = oldRet
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMethod(structName string, fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("_ %s: %s", p.Name, c.compileType(p.Type))
	}
	ret := c.compileType(fn.Return)

	mutating := false
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			for field := range st.Fields {
				if paramAssigned(fn.Body, field) {
					mutating = true
					break
				}
			}
		}
	}
	keyword := "func"
	if mutating {
		keyword = "mutating func"
	}

	c.writeln(fmt.Sprintf("%s %s(%s) -> %s {", keyword, fn.Name, strings.Join(params, ", "), ret))
	c.indent++

	oldEnv := c.env
	oldRet := c.funcRet
	oldLocals := c.locals

	c.locals = map[string]types.Type{}
	if c.env != nil {
		methodEnv := types.NewEnv(c.env)
		if st, ok := c.env.GetStruct(structName); ok {
			for name, t := range st.Fields {
				methodEnv.SetVar(name, t, true)
				c.locals[name] = t
			}
		}
		c.env = methodEnv
		for _, p := range fn.Params {
			if p.Type != nil {
				t := resolveTypeRef(p.Type, oldEnv)
				c.env.SetVar(p.Name, t, true)
				c.locals[p.Name] = t
			}
		}
	}

	c.funcRet = resolveTypeRef(fn.Return, oldEnv)

	for _, p := range fn.Params {
		decl := "var"
		if !paramAssigned(fn.Body, p.Name) {
			decl = "let"
		}
		c.writeln(fmt.Sprintf("%s %s = %s", decl, p.Name, p.Name))
	}
	if len(fn.Params) > 0 {
		c.writeln("")
	}

	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}

	if c.env != nil {
		c.env = oldEnv
	}
	c.locals = oldLocals
	c.funcRet = oldRet

	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileType(t *parser.TypeRef) string {
	if t == nil {
		return "Void"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.compileType(p)
		}
		ret := c.compileType(t.Fun.Return)
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Bool"
		case "string":
			return "String"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return "[" + c.compileType(t.Generic.Args[0]) + "]"
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return "[" + c.compileType(t.Generic.Args[0]) + ": " + c.compileType(t.Generic.Args[1]) + "]"
		}
	}
	return "Any"
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("protocol %s {}", t.Name))
		for _, v := range t.Variants {
			c.writeln(fmt.Sprintf("struct %s: %s {", v.Name, t.Name))
			c.indent++
			for _, f := range v.Fields {
				typ := c.compileType(f.Type)
				c.writeln(fmt.Sprintf("var %s: %s", f.Name, typ))
			}
			c.indent--
			c.writeln("}")
		}
		return nil
	}
	c.writeln(fmt.Sprintf("struct %s {", t.Name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			typ := c.compileType(m.Field.Type)
			c.writeln(fmt.Sprintf("var %s: %s", m.Field.Name, typ))
		} else if m.Method != nil {
			if err := c.compileMethod(t.Name, m.Method); err != nil {
				return err
			}
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		typ := ""
		var t types.Type = types.AnyType{}
		if s.Let.Type != nil {
			typ = ": " + c.compileType(s.Let.Type)
			t = resolveTypeRef(s.Let.Type, c.env)
		}
		if typ == "" {
			t = c.inferExprType(s.Let.Value)
			if lt, ok := t.(types.ListType); ok && !containsAny(lt) {
				typ = ": [" + swiftType(lt.Elem) + "]"
			} else if mt, ok := t.(types.MapType); ok && !containsAny(mt) {
				typ = ": [" + swiftType(mt.Key) + ": " + swiftType(mt.Value) + "]"
			}
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("let %s%s", s.Let.Name, typ))
		} else {
			c.writeln(fmt.Sprintf("let %s%s = %s", s.Let.Name, typ, expr))
		}
		if c.env != nil {
			c.env.SetVar(s.Let.Name, t, true)
		}
		c.locals[s.Let.Name] = t
	case s.Var != nil:
		expr, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		typ := ""
		var t types.Type = types.AnyType{}
		if s.Var.Type != nil {
			typ = ": " + c.compileType(s.Var.Type)
			t = resolveTypeRef(s.Var.Type, c.env)
		}
		if typ == "" {
			t = c.inferExprType(s.Var.Value)
			if lt, ok := t.(types.ListType); ok && !containsAny(lt) {
				typ = ": [" + swiftType(lt.Elem) + "]"
			} else if mt, ok := t.(types.MapType); ok && !containsAny(mt) {
				typ = ": [" + swiftType(mt.Key) + ": " + swiftType(mt.Value) + "]"
			}
		}
		if typ == "" && expr == "[]" {
			if lt, ok := c.funcRet.(types.ListType); ok {
				typ = ": [" + swiftType(lt.Elem) + "]"
				t = types.ListType{Elem: lt.Elem}
			} else {
				typ = ": [Any]"
				t = types.ListType{Elem: types.AnyType{}}
			}
		}
		if typ == "" && expr == "[:]" {
			if mt, ok := c.funcRet.(types.MapType); ok {
				typ = ": [" + swiftType(mt.Key) + ": " + swiftType(mt.Value) + "]"
				t = types.MapType{Key: mt.Key, Value: mt.Value}
			} else {
				typ = ": [AnyHashable: Any]"
				t = types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
			}
		}
		if expr == "" {
			c.writeln(fmt.Sprintf("var %s%s", s.Var.Name, typ))
		} else {
			c.writeln(fmt.Sprintf("var %s%s = %s", s.Var.Name, typ, expr))
		}
		if c.env != nil {
			c.env.SetVar(s.Var.Name, t, true)
		}
		c.locals[s.Var.Name] = t
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("return %s", expr))
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	default:
		// ignore unsupported statements
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s..<%s {", f.Name, start, end))
		if c.env != nil {
			c.env.SetVar(f.Name, types.IntType{}, true)
		}
		c.locals[f.Name] = types.IntType{}
		c.indent++
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		srcType := c.inferExprType(f.Source)
		if _, ok := srcType.(types.StringType); ok {
			if f.Name == "_" {
				c.writeln(fmt.Sprintf("for _ in %s {", src))
				c.indent++
				for _, st := range f.Body {
					if err := c.compileStmt(st); err != nil {
						return err
					}
				}
				c.indent--
				c.writeln("}")
				return nil
			}
			tmp := f.Name + "_ch"
			c.writeln(fmt.Sprintf("for %s in %s {", tmp, src))
			c.indent++
			c.writeln(fmt.Sprintf("let %s = String(%s)", f.Name, tmp))
			if c.env != nil {
				c.env.SetVar(f.Name, types.StringType{}, true)
			}
			c.locals[f.Name] = types.StringType{}
			for _, st := range f.Body {
				if err := c.compileStmt(st); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("}")
			return nil
		} else {
			loopSrc := src
			if mt, ok := srcType.(types.MapType); ok {
				loopSrc += ".keys"
				if c.env != nil {
					c.env.SetVar(f.Name, mt.Key, true)
				}
				c.locals[f.Name] = mt.Key
			} else if lt, ok := srcType.(types.ListType); ok {
				if c.env != nil {
					c.env.SetVar(f.Name, lt.Elem, true)
				}
				c.locals[f.Name] = lt.Elem
			} else {
				if c.env != nil {
					c.env.SetVar(f.Name, types.AnyType{}, true)
				}
				c.locals[f.Name] = types.AnyType{}
			}
			c.writeln(fmt.Sprintf("for %s in %s {", f.Name, loopSrc))
			c.indent++
			for _, st := range f.Body {
				if err := c.compileStmt(st); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("}")
			return nil
		}
	}
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if ifst.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(ifst.ElseIf)
	}
	if len(ifst.Else) > 0 {
		c.buf.WriteString(" else {")
		c.buf.WriteByte('\n')
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while %s {", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	name := a.Name
	for _, idx := range a.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		name = fmt.Sprintf("%s[%s]", name, iexpr)
	}
	value, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	target := u.Target
	c.writeln(fmt.Sprintf("for i in 0..<%s.count {", target))
	c.indent++
	c.writeln(fmt.Sprintf("var item = %s[i]", target))

	oldEnv := c.env
	oldLocals := c.locals
	var st types.StructType
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok2 := lt.Elem.(types.StructType); ok2 {
					st = s
					child := types.NewEnv(c.env)
					c.locals = map[string]types.Type{}
					for name, ft := range s.Fields {
						child.SetVar(name, ft, true)
						c.locals[name] = ft
						c.writeln(fmt.Sprintf("let %s = item.%s", name, name))
					}
					c.env = child
				}
			}
		}
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = oldEnv
			c.locals = oldLocals
			return err
		}
		c.writeln(fmt.Sprintf("if !(%s) {", cond))
		c.indent++
		c.writeln("continue")
		c.indent--
		c.writeln("}")
	}

	for _, it := range u.Set.Items {
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = oldEnv
			c.locals = oldLocals
			return err
		}
		if _, ok := st.Fields[stringKeyName(it.Key)]; ok {
			if key, ok2 := identName(it.Key); ok2 {
				c.writeln(fmt.Sprintf("item.%s = %s", key, val))
				continue
			}
		}
		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.env = oldEnv
			c.locals = oldLocals
			return err
		}
		if id, ok := identName(it.Key); ok {
			keyExpr = strconv.Quote(id)
		}
		c.writeln(fmt.Sprintf("item[%s] = %s", keyExpr, val))
	}

	c.writeln(fmt.Sprintf("%s[i] = item", target))
	if len(st.Fields) > 0 {
		c.env = oldEnv
		c.locals = oldLocals
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	exprIsString := c.isStringUnary(b.Left)
	for _, op := range b.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rhsIsString := c.isStringPostfix(op.Right)
		if op.Op == "+" && (exprIsString || rhsIsString) {
			if !exprIsString {
				expr = fmt.Sprintf("String(%s)", expr)
			}
			if !rhsIsString {
				rhs = fmt.Sprintf("String(%s)", rhs)
			}
			expr = fmt.Sprintf("%s + %s", expr, rhs)
			exprIsString = true
			continue
		}
		if op.Op == "in" {
			if c.isMapPostfix(op.Right) {
				expr = fmt.Sprintf("%s[%s] != nil", rhs, expr)
			} else {
				expr = fmt.Sprintf("%s.contains(%s)", rhs, expr)
			}
		} else if op.Op == "union" && op.All {
			expr = fmt.Sprintf("%s + %s", expr, rhs)
		} else if op.Op == "union" {
			c.use("_union")
			expr = fmt.Sprintf("_union(%s, %s)", expr, rhs)
		} else if op.Op == "except" {
			c.use("_except")
			expr = fmt.Sprintf("_except(%s, %s)", expr, rhs)
		} else if op.Op == "intersect" {
			c.use("_intersect")
			expr = fmt.Sprintf("_intersect(%s, %s)", expr, rhs)
		} else {
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, rhs)
		}
		exprIsString = false
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = u.Ops[i] + expr
	}
	return expr, nil
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
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("%s.count", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if c.isStringPrimary(p.Target) {
					c.use("_sliceString")
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				} else {
					c.use("_slice")
					expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isStringPrimary(p.Target) {
					c.use("_indexString")
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					if c.isMapPrimary(p.Target) {
						expr = fmt.Sprintf("%s[%s]!", expr, idx)
					} else if c.isListPrimary(p.Target) {
						c.use("_index")
						expr = fmt.Sprintf("_index(%s, %s)", expr, idx)
					} else {
						expr = fmt.Sprintf("%s[%s]", expr, idx)
					}
				}
			}
		} else if op.Cast != nil {
			c.use("_cast")
			expr = fmt.Sprintf("_cast(%s.self, %s)", c.compileType(op.Cast.Type), expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Selector != nil:
		expr := p.Selector.Root
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		isMap := false
		if _, ok := typ.(types.MapType); ok {
			isMap = true
		}
		for _, f := range p.Selector.Tail {
			if isMap {
				expr = fmt.Sprintf("%s[%q]!", expr, f)
			} else {
				expr += "." + f
			}
		}
		return expr, nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
		if p.Lit.Null {
			return "nil", nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, item := range p.Map.Items {
			k, err := c.compileExpr(item.Key)
			if err != nil {
				return "", err
			}
			// Treat bare identifiers as string keys.
			if id, ok := identName(item.Key); ok {
				k = strconv.Quote(id)
			} else if !c.isStringExpr(item.Key) {
				k = fmt.Sprintf("String(describing: %s)", k)
			}
			v, err := c.compileExpr(item.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		if len(items) == 0 {
			return "[:]", nil
		}
		return "[" + strings.Join(items, ", ") + "]", nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", f.Name, v)
		}
		return fmt.Sprintf("%s(%s)", p.Struct.Name, strings.Join(parts, ", ")), nil
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		switch p.Call.Func {
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			return fmt.Sprintf("%s.count", args[0]), nil
		case "count":
			if len(args) != 1 {
				return "", fmt.Errorf("count expects 1 arg")
			}
			return fmt.Sprintf("%s.count", args[0]), nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects 1 arg")
			}
			return fmt.Sprintf("String(describing: %s)", args[0]), nil
		case "upper":
			if len(args) != 1 {
				return "", fmt.Errorf("upper expects 1 arg")
			}
			return fmt.Sprintf("%s.uppercased()", args[0]), nil
		case "lower":
			if len(args) != 1 {
				return "", fmt.Errorf("lower expects 1 arg")
			}
			return fmt.Sprintf("%s.lowercased()", args[0]), nil
		case "abs":
			if len(args) != 1 {
				return "", fmt.Errorf("abs expects 1 arg")
			}
			return fmt.Sprintf("abs(%s)", args[0]), nil
		case "concat":
			if len(args) < 2 {
				return "", fmt.Errorf("concat expects at least 2 args")
			}
			c.use("_concat")
			expr := args[0]
			for i := 1; i < len(args); i++ {
				expr = fmt.Sprintf("_concat(%s, %s)", expr, args[i])
			}
			return expr, nil
		case "append":
			if len(args) != 2 {
				return "", fmt.Errorf("append expects 2 args")
			}
			c.use("_append")
			return fmt.Sprintf("_append(%s, %s)", args[0], args[1]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			c.use("_avg")
			return fmt.Sprintf("_avg(%s.map { Double($0) })", args[0]), nil
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects 1 arg")
			}
			c.use("_sum")
			return fmt.Sprintf("_sum(%s.map { Double($0) })", args[0]), nil
		case "min":
			if len(args) != 1 {
				return "", fmt.Errorf("min expects 1 arg")
			}
			c.use("_min")
			c.use("_Group")
			return fmt.Sprintf("_min(%s)", args[0]), nil
		case "max":
			if len(args) != 1 {
				return "", fmt.Errorf("max expects 1 arg")
			}
			c.use("_max")
			c.use("_Group")
			return fmt.Sprintf("_max(%s)", args[0]), nil
		case "values":
			if len(args) != 1 {
				return "", fmt.Errorf("values expects 1 arg")
			}
			c.use("_values")
			return fmt.Sprintf("_values(%s)", args[0]), nil
		case "exists":
			if len(args) != 1 {
				return "", fmt.Errorf("exists expects 1 arg")
			}
			c.use("_exists")
			c.use("_Group")
			return fmt.Sprintf("_exists(%s)", args[0]), nil
		case "substring":
			if len(args) != 3 {
				return "", fmt.Errorf("substring expects 3 args")
			}
			c.use("_sliceString")
			return fmt.Sprintf("_sliceString(%s, %s, %s)", args[0], args[1], args[2]), nil
		case "now":
			if len(args) != 0 {
				return "", fmt.Errorf("now expects 0 args")
			}
			return "Int64(Date().timeIntervalSince1970 * 1_000_000_000)", nil
		case "json":
			if len(args) != 1 {
				return "", fmt.Errorf("json expects 1 arg")
			}
			c.use("_json")
			return fmt.Sprintf("_json(%s)", args[0]), nil
		case "input":
			if len(args) != 0 {
				return "", fmt.Errorf("input expects 0 args")
			}
			return "readLine() ?? \"\"", nil
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
		}
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		paramType := "Any"
		if p.Type != nil {
			paramType = c.compileType(p.Type)
		}
		params[i] = fmt.Sprintf("%s: %s", p.Name, paramType)
	}
	ret := "Void"
	if fn.Return != nil {
		ret = c.compileType(fn.Return)
	}
	oldBuf := c.buf
	oldIndent := c.indent
	oldEnv := c.env
	oldRet := c.funcRet
	oldLocals := c.locals

	c.buf = bytes.Buffer{}
	c.indent = 1
	c.locals = map[string]types.Type{}
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		for _, p := range fn.Params {
			if p.Type != nil {
				t := resolveTypeRef(p.Type, oldEnv)
				c.env.SetVar(p.Name, t, true)
				c.locals[p.Name] = t
			}
		}
	}
	c.funcRet = resolveTypeRef(fn.Return, oldEnv)

	for _, p := range fn.Params {
		decl := "var"
		if fn.ExprBody != nil || !paramAssigned(fn.BlockBody, p.Name) {
			decl = "let"
		}
		c.writeln(fmt.Sprintf("%s %s = %s", decl, p.Name, p.Name))
	}
	if len(fn.Params) > 0 {
		c.writeln("")
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.buf = oldBuf
			c.indent = oldIndent
			if c.env != nil {
				c.env = oldEnv
			}
			c.locals = oldLocals
			c.funcRet = oldRet
			return "", err
		}
		c.writeln("return " + expr)
	} else {
		for _, st := range fn.BlockBody {
			if err := c.compileStmt(st); err != nil {
				c.buf = oldBuf
				c.indent = oldIndent
				if c.env != nil {
					c.env = oldEnv
				}
				c.locals = oldLocals
				c.funcRet = oldRet
				return "", err
			}
		}
	}
	bodyStr := c.buf.String()
	c.buf = oldBuf
	c.indent = oldIndent
	if c.env != nil {
		c.env = oldEnv
	}
	c.locals = oldLocals
	c.funcRet = oldRet
	result := fmt.Sprintf("{ (%s) -> %s in\n%s}", strings.Join(params, ", "), ret, indentBlock(bodyStr, 1))
	return result, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "nil"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_load")
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "nil"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_save")
	c.use("_toMapSlice")
	return fmt.Sprintf("_save(_toMapSlice(%s), %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	// special case: simple sort and select by query variable
	if len(q.Froms) == 0 && q.Sort != nil && q.Skip == nil && q.Take == nil {
		orig := c.env
		child := types.NewEnv(c.env)
		elem := c.inferExprType(q.Source)
		if lt, ok := elem.(types.ListType); ok {
			elem = lt.Elem
		}
		child.SetVar(q.Var, elem, true)
		c.env = child
		sortExpr, err := c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
		selExpr, err := c.compileExpr(q.Select)
		c.env = orig
		if err != nil {
			return "", err
		}
		if sortExpr == q.Var && selExpr == q.Var {
			return fmt.Sprintf("%s.sorted()", src), nil
		}
	}

	// grouping without joins/sorting/pagination. Optional where clause
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		orig := c.env
		child := types.NewEnv(c.env)
		elem := c.inferExprType(q.Source)
		if lt, ok := elem.(types.ListType); ok {
			elem = lt.Elem
		}
		child.SetVar(q.Var, elem, true)
		c.env = child
		cond := ""
		if q.Where != nil {
			var err error
			cond, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.AnyType{}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		c.env = orig
		if err != nil {
			return "", err
		}
		c.use("_group_by")
		c.use("_Group")
		filtered := src
		if cond != "" {
			filtered = fmt.Sprintf("%s.filter { %s in %s }", src, q.Var, cond)
		}
		expr := fmt.Sprintf("_group_by(%s.map { $0 as Any }, { %s in %s }).map { %s in %s }", filtered, q.Var, keyExpr, q.Group.Name, valExpr)
		return expr, nil
	}

	// simple map with optional cross joins
	orig := c.env
	child := types.NewEnv(c.env)
	elem := c.inferExprType(q.Source)
	if lt, ok := elem.(types.ListType); ok {
		elem = lt.Elem
	}
	child.SetVar(q.Var, elem, true)
	c.env = child
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		ft := c.inferExprType(f.Src)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
		fromSrcs[i] = fs
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinSrcs[i] = js
		joinOns[i] = on
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	cond := ""
	condVars := map[string]bool{}
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
		exprVars(q.Where, condVars)
	}
	sortExpr := ""
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	skipExpr := ""
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	takeExpr := ""
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	elemType := c.inferExprType(q.Select)
	c.env = orig
	resType := swiftType(elemType)
	if resType == "" {
		resType = "Any"
	}

	var b strings.Builder
	b.WriteString("({\n")
	if sortExpr != "" {
		b.WriteString(fmt.Sprintf("\tvar _pairs: [(item: %s, key: Any)] = []\n", resType))
	} else {
		b.WriteString(fmt.Sprintf("\tvar _res: [%s] = []\n", resType))
	}
	b.WriteString(fmt.Sprintf("\tfor %s in %s {\n", q.Var, src))
	indent := "\t\t"
	seen := map[string]bool{q.Var: true}
	pushed := false
	if cond != "" && subsetVars(seen, condVars) {
		b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, cond))
		pushed = true
	}
	for i, f := range q.Froms {
		b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, f.Var, fromSrcs[i]))
		indent += "\t"
		seen[f.Var] = true
		if cond != "" && !pushed && subsetVars(seen, condVars) {
			b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, cond))
			pushed = true
		}
	}
	for i := range q.Joins {
		b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, q.Joins[i].Var, joinSrcs[i]))
		indent += "\t"
		b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, joinOns[i]))
		seen[q.Joins[i].Var] = true
		if cond != "" && !pushed && subsetVars(seen, condVars) {
			b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, cond))
			pushed = true
		}
	}
	if cond != "" && !pushed {
		b.WriteString(fmt.Sprintf("%sif %s {\n", indent, cond))
		indent += "\t"
	}
	if sortExpr != "" {
		b.WriteString(fmt.Sprintf("%s_pairs.append((item: %s, key: %s))\n", indent, sel, sortExpr))
	} else {
		b.WriteString(fmt.Sprintf("%s_res.append(%s)\n", indent, sel))
	}
	if cond != "" && !pushed {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for range q.Joins {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for range q.Froms {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("\t}\n")
	if sortExpr != "" {
		b.WriteString("\t_pairs.sort { a, b in\n")
		b.WriteString("\t\tif let ai = a.key as? Int, let bi = b.key as? Int { return ai < bi }\n")
		b.WriteString("\t\tif let af = a.key as? Double, let bf = b.key as? Double { return af < bf }\n")
		b.WriteString("\t\tif let ai = a.key as? Int, let bf = b.key as? Double { return Double(ai) < bf }\n")
		b.WriteString("\t\tif let af = a.key as? Double, let bi = b.key as? Int { return af < Double(bi) }\n")
		b.WriteString("\t\tif let sa = a.key as? String, let sb = b.key as? String { return sa < sb }\n")
		b.WriteString("\t\treturn String(describing: a.key) < String(describing: b.key)\n")
		b.WriteString("\t}\n")
		b.WriteString("\tvar _items = _pairs.map { $0.item }\n")
	} else {
		b.WriteString("\tvar _items = _res\n")
	}
	if skipExpr != "" {
		b.WriteString("\t{ let _n = " + skipExpr + "; _items = _n < _items.count ? Array(_items[_n...]) : [] }\n")
	}
	if takeExpr != "" {
		b.WriteString("\t{ let _n = " + takeExpr + "; if _n < _items.count { _items = Array(_items[0..<_n]) } }\n")
	}
	b.WriteString("\treturn _items\n")
	b.WriteString("}())")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("({\n")
	b.WriteString("\tlet _t = " + target + "\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + "\n")
			b.WriteString("}())")
			return b.String(), nil
		}
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				b.WriteString("\tif let v = _t as? " + sanitizeName(call.Func) + " {\n")
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						field := sanitizeName(st.Order[idx])
						b.WriteString(fmt.Sprintf("\t\tlet %s = v.%s\n", sanitizeName(id), field))
					}
				}
				b.WriteString("\t\treturn " + res + "\n")
				b.WriteString("\t}\n")
				continue
			}
		}
		if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				b.WriteString("\tif _t is " + sanitizeName(ident) + " {\n")
				b.WriteString("\t\treturn " + res + "\n")
				b.WriteString("\t}\n")
				continue
			}
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		b.WriteString("\tif _t == " + pat + " {\n")
		b.WriteString("\t\treturn " + res + "\n")
		b.WriteString("\t}\n")
	}
	b.WriteString("\treturn nil\n")
	b.WriteString("}())")
	return b.String(), nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "()"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		if t, ok := c.locals[name]; ok {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
		if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isStringExpr(p.Group)
	}
	return false
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	return c.isStringUnary(e.Binary.Left)
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	return c.isStringPrimary(p.Target)
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Map != nil:
		return true
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		if t, ok := c.locals[name]; ok {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
		if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isMapExpr(p.Group)
	}
	return false
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	return c.isMapUnary(e.Binary.Left)
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isMapPostfix(u.Value)
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	return c.isMapPrimary(p.Target)
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.List != nil:
		return true
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		if t, ok := c.locals[name]; ok {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
		if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isListExpr(p.Group)
	}
	return false
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	return c.isListUnary(e.Binary.Left)
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isListPostfix(u.Value)
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	return c.isListPrimary(p.Target)
}

// paramAssigned reports whether a function parameter is assigned within the
// given body. Only direct assignments are considered.
func paramAssigned(body []*parser.Statement, name string) bool {
	for _, st := range body {
		if stmtAssignsVar(st, name) {
			return true
		}
	}
	return false
}

func stmtAssignsVar(s *parser.Statement, name string) bool {
	switch {
	case s.Assign != nil:
		// treat assignments to a parameter via indexing as modifying
		// the parameter so it should be mutable
		return s.Assign.Name == name
	case s.For != nil:
		return paramAssigned(s.For.Body, name)
	case s.While != nil:
		return paramAssigned(s.While.Body, name)
	case s.If != nil:
		if paramAssigned(s.If.Then, name) {
			return true
		}
		if s.If.ElseIf != nil && stmtAssignsVar(&parser.Statement{If: s.If.ElseIf}, name) {
			return true
		}
		if len(s.If.Else) > 0 {
			return paramAssigned(s.If.Else, name)
		}
	}
	return false
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln("expect(" + expr + ")")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln("func " + name + "() {")
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) writeExpectFunc(prog *parser.Program) {
	if !hasExpect(prog) {
		return
	}
	c.writeln("func expect(_ cond: Bool) {")
	c.indent++
	c.writeln("if !cond { fatalError(\"expect failed\") }")
	c.indent--
	c.writeln("}")
	c.writeln("")
}

func hasTest(p *parser.Program) bool {
	for _, s := range p.Statements {
		if s.Test != nil {
			return true
		}
	}
	return false
}

func hasExpect(p *parser.Program) bool {
	for _, s := range p.Statements {
		if containsExpect(s) {
			return true
		}
	}
	return false
}

func containsExpect(s *parser.Statement) bool {
	switch {
	case s.Expect != nil:
		return true
	case s.If != nil:
		for _, t := range s.If.Then {
			if containsExpect(t) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if containsExpect(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if containsExpect(t) {
				return true
			}
		}
	case s.For != nil:
		for _, t := range s.For.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.While != nil:
		for _, t := range s.While.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if containsExpect(t) {
				return true
			}
		}
	}
	return false
}
