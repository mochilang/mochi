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
	buf         bytes.Buffer
	indent      int
	env         *types.Env
	locals      map[string]types.Type
	useAvg      bool
	useIndexStr bool
	useSlice    bool
	useSliceStr bool
	funcRet     types.Type
}

func New(env *types.Env) *Compiler { return &Compiler{env: env, locals: map[string]types.Type{}} }

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.useAvg = false
	c.useIndexStr = false
	c.useSlice = false
	c.useSliceStr = false

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent = 0

	// function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main body
	c.writeln("func main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
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
	if c.useAvg {
		c.writeln("func _avg<T: BinaryInteger>(_ arr: [T]) -> Double {")
		c.indent++
		c.writeln("if arr.isEmpty { return 0 }")
		c.writeln("var sum = 0.0")
		c.writeln("for v in arr { sum += Double(v) }")
		c.writeln("return sum / Double(arr.count)")
		c.indent--
		c.writeln("}")
		c.writeln("func _avg<T: BinaryFloatingPoint>(_ arr: [T]) -> Double {")
		c.indent++
		c.writeln("if arr.isEmpty { return 0 }")
		c.writeln("var sum = 0.0")
		c.writeln("for v in arr { sum += Double(v) }")
		c.writeln("return sum / Double(arr.count)")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useIndexStr {
		c.writeln("func _indexString(_ s: String, _ i: Int) -> String {")
		c.indent++
		c.writeln("var idx = i")
		c.writeln("let chars = Array(s)")
		c.writeln("if idx < 0 { idx += chars.count }")
		c.writeln("if idx < 0 || idx >= chars.count { fatalError(\"index out of range\") }")
		c.writeln("return String(chars[idx])")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useSlice {
		c.writeln("func _slice<T>(_ arr: [T], _ i: Int, _ j: Int) -> [T] {")
		c.indent++
		c.writeln("var start = i")
		c.writeln("var end = j")
		c.writeln("let n = arr.count")
		c.writeln("if start < 0 { start += n }")
		c.writeln("if end < 0 { end += n }")
		c.writeln("if start < 0 { start = 0 }")
		c.writeln("if end > n { end = n }")
		c.writeln("if end < start { end = start }")
		c.writeln("return Array(arr[start..<end])")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.useSliceStr {
		c.writeln("func _sliceString(_ s: String, _ i: Int, _ j: Int) -> String {")
		c.indent++
		c.writeln("var start = i")
		c.writeln("var end = j")
		c.writeln("let chars = Array(s)")
		c.writeln("let n = chars.count")
		c.writeln("if start < 0 { start += n }")
		c.writeln("if end < 0 { end += n }")
		c.writeln("if start < 0 { start = 0 }")
		c.writeln("if end > n { end = n }")
		c.writeln("if end < start { end = start }")
		c.writeln("return String(chars[start..<end])")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	c.buf.Write(bodyBytes)
	return c.buf.Bytes(), nil
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

func swiftType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Bool"
	case types.StringType:
		return "String"
	case types.ListType:
		return "[" + swiftType(tt.Elem) + "]"
	case types.MapType:
		return "[" + swiftType(tt.Key) + ": " + swiftType(tt.Value) + "]"
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = swiftType(p)
		}
		ret := swiftType(tt.Return)
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	case types.VoidType:
		return "Void"
	default:
		return "Any"
	}
}

func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return types.MapType{Key: resolveTypeRef(t.Generic.Args[0], env), Value: resolveTypeRef(t.Generic.Args[1], env)}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if env != nil {
				if st, ok := env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
		}
	}
	return types.AnyType{}
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
		} else if c.env != nil {
			if t2, err := c.env.GetVar(s.Let.Name); err == nil {
				typ = ": " + swiftType(t2)
				t = t2
			}
		}
		if t == (types.AnyType{}) {
			t = c.inferExprType(s.Let.Value)
			if typ == "" && !containsAny(t) {
				typ = ": " + swiftType(t)
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
		} else if c.env != nil {
			if t2, err := c.env.GetVar(s.Var.Name); err == nil {
				typ = ": " + swiftType(t2)
				t = t2
			}
		}
		if t == (types.AnyType{}) {
			t = c.inferExprType(s.Var.Value)
			if typ == "" && !containsAny(t) {
				typ = ": " + swiftType(t)
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
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
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
			c.writeln(fmt.Sprintf("for %s in %s {", f.Name, src))
			if lt, ok := srcType.(types.ListType); ok {
				if c.env != nil {
					c.env.SetVar(f.Name, lt.Elem, true)
				}
				c.locals[f.Name] = lt.Elem
			} else if mt, ok := srcType.(types.MapType); ok {
				if c.env != nil {
					c.env.SetVar(f.Name, mt.Key, true)
				}
				c.locals[f.Name] = mt.Key
			} else {
				if c.env != nil {
					c.env.SetVar(f.Name, types.AnyType{}, true)
				}
				c.locals[f.Name] = types.AnyType{}
			}
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
	return nil
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
	for _, op := range b.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		if op.Op == "in" {
			if c.isMapPostfix(op.Right) {
				expr = fmt.Sprintf("%s[%s] != nil", rhs, expr)
			} else {
				expr = fmt.Sprintf("%s.contains(%s)", rhs, expr)
			}
		} else {
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, rhs)
		}
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
					c.useSliceStr = true
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				} else {
					c.useSlice = true
					expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isStringPrimary(p.Target) {
					c.useIndexStr = true
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
					if c.isMapPrimary(p.Target) {
						expr += "!"
					}
				}
			}
		} else if op.Cast != nil {
			expr = fmt.Sprintf("%s as! %s", expr, c.compileType(op.Cast.Type))
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
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
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
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
			return fmt.Sprintf("String(%s)", args[0]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			c.useAvg = true
			return fmt.Sprintf("_avg(%s.map { Double($0) })", args[0]), nil
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
	c.buf = bytes.Buffer{}
	c.indent = 1
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.buf = oldBuf
			c.indent = oldIndent
			return "", err
		}
		c.writeln("return " + expr)
	} else {
		for _, st := range fn.BlockBody {
			if err := c.compileStmt(st); err != nil {
				c.buf = oldBuf
				c.indent = oldIndent
				return "", err
			}
		}
	}
	bodyStr := c.buf.String()
	c.buf = oldBuf
	c.indent = oldIndent
	result := fmt.Sprintf("{ (%s) -> %s in\n%s}", strings.Join(params, ", "), ret, indentBlock(bodyStr, 1))
	return result, nil
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
		return s.Assign.Name == name && len(s.Assign.Index) == 0
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

func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	prog := &parser.Program{Statements: []*parser.Statement{{Let: &parser.LetStmt{Name: "_tmp", Value: e}}}}
	env := types.NewEnv(c.env)
	for name, t := range c.locals {
		env.SetVar(name, t, true)
	}
	if errs := types.Check(prog, env); len(errs) == 0 {
		if t, err := env.GetVar("_tmp"); err == nil {
			return t
		}
	}
	return types.AnyType{}
}

func containsAny(t types.Type) bool {
	switch tt := t.(type) {
	case types.AnyType:
		return true
	case types.ListType:
		return containsAny(tt.Elem)
	case types.MapType:
		return containsAny(tt.Key) || containsAny(tt.Value)
	}
	return false
}
