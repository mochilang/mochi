package javacode

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Java source code.
type Compiler struct {
	buf        bytes.Buffer
	indent     int
	imports    map[string]bool
	env        *types.Env
	mainStmts  []*parser.Statement
	tests      []*parser.TestBlock
	helpers    map[string]bool
	returnType types.Type
}

// New creates a new Java compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), tests: []*parser.TestBlock{}, imports: map[string]bool{}}
}

// Compile generates Java code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	if prog.Package != "" {
		c.writeln("package " + sanitizeName(prog.Package) + ";")
		c.writeln("")
	}

	// gather imports first
	for _, s := range prog.Statements {
		if s.Import != nil {
			if err := c.compileImport(s.Import); err != nil {
				return nil, err
			}
		}
	}
	if len(c.imports) > 0 {
		keys := make([]string, 0, len(c.imports))
		for p := range c.imports {
			keys = append(keys, p)
		}
		sort.Strings(keys)
		for _, imp := range keys {
			c.writeln("import " + imp + ";")
		}
		c.writeln("")
	}
	c.writeln("public class Main {")
	c.indent++

	// type declarations
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// collect function and test declarations, and main statements
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.tests = append(c.tests, s.Test)
			c.writeln("")
			continue
		}
		c.mainStmts = append(c.mainStmts, s)
	}

	if len(c.mainStmts) > 0 || len(c.tests) > 0 {
		c.writeln("public static void main(String[] args) {")
		c.indent++
		for _, t := range c.tests {
			name := "test_" + sanitizeName(t.Name)
			c.writeln(name + "();")
		}
		for _, s := range c.mainStmts {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
		c.indent--
		c.writeln("}")
	}

	c.emitRuntime()

	c.indent--
	c.writeln("}")
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
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr + ";")
		}
		return nil
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Import != nil:
		return c.compileImport(s.Import)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	var t types.Type = types.AnyType{}
	if stmt.Type != nil {
		t = c.resolveTypeRef(stmt.Type)
	} else if stmt.Value != nil {
		t = c.inferExprType(stmt.Value)
	} else if c.env != nil {
		if tv, err := c.env.GetVar(stmt.Name); err == nil {
			t = tv
		}
	}
	if c.env != nil {
		c.env.SetVar(stmt.Name, t, false)
	}

	typStr := c.javaType(t)
	if typStr == "" {
		typStr = "var"
	}
	expr := ""
	if stmt.Value != nil {
		v, err := c.compileExprHint(stmt.Value, t)
		if err != nil {
			return err
		}
		expr = " = " + v
	}
	c.writeln(fmt.Sprintf("%s %s%s;", typStr, sanitizeName(stmt.Name), expr))
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	var t types.Type = types.AnyType{}
	if stmt.Type != nil {
		t = c.resolveTypeRef(stmt.Type)
	} else if stmt.Value != nil {
		t = c.inferExprTypeHint(stmt.Value, t)
	} else if c.env != nil {
		if tv, err := c.env.GetVar(stmt.Name); err == nil {
			t = tv
		}
	}
	if c.env != nil {
		c.env.SetVar(stmt.Name, t, true)
	}

	typStr := c.javaType(t)
	if typStr == "" {
		typStr = "var"
	}
	expr := ""
	if stmt.Value != nil {
		v, err := c.compileExprHint(stmt.Value, t)
		if err != nil {
			return err
		}
		expr = " = " + v
	}
	c.writeln(fmt.Sprintf("%s %s%s;", typStr, sanitizeName(stmt.Name), expr))
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	var t types.Type
	if c.env != nil {
		if tv, err := c.env.GetVar(stmt.Name); err == nil {
			t = tv
			if mt, ok := tv.(types.MapType); ok && len(stmt.Index) == 1 {
				key, err := c.compileExpr(stmt.Index[0].Start)
				if err != nil {
					return err
				}
				val, err := c.compileExprHint(stmt.Value, mt.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("%s.put(%s, %s);", lhs, key, val))
				return nil
			}
		}
	}
	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExprHint(stmt.Value, t)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileReturn(stmt *parser.ReturnStmt) error {
	expr, err := c.compileExprHint(stmt.Value, c.returnType)
	if err != nil {
		return err
	}
	c.writeln("return " + expr + ";")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	outName := name
	if name == "_" {
		outName = "__"
	}
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", outName, start, outName, end, outName))
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.IntType{}, true)
		}
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	asString := false
	asMap := false
	if stmt.Source != nil && c.env != nil {
		if ident := stmt.Source.Binary.Left; ident != nil && len(ident.Ops) == 0 {
			p := ident.Value
			if p.Target != nil {
				if p.Target.Lit != nil && p.Target.Lit.Str != nil {
					asString = true
				}
				if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
					if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
						switch t.(type) {
						case types.StringType:
							asString = true
						case types.MapType:
							asMap = true
						}
					}
				}
			}
		}
	}
	if asString {
		src += ".toCharArray()"
	}
	if asMap {
		src += ".keySet()"
	}
	if c.env != nil {
		t := c.inferExprType(stmt.Source)
		switch tt := t.(type) {
		case types.ListType:
			c.env.SetVar(stmt.Name, tt.Elem, true)
		case types.MapType:
			c.env.SetVar(stmt.Name, tt.Key, true)
		case types.StringType:
			c.env.SetVar(stmt.Name, types.StringType{}, true)
		default:
			c.env.SetVar(stmt.Name, types.AnyType{}, true)
		}
	}
	c.writeln(fmt.Sprintf("for (var %s : %s) {", outName, src))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	if im.Lang != nil && *im.Lang != "java" {
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
	path := strings.Trim(im.Path, "\"")
	c.imports[path] = true
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if stmt.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.buf.WriteString(" else {")
		c.buf.WriteByte('\n')
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln("static void " + name + "() {")
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.helpers["_expect"] = true
	c.writeln(fmt.Sprintf("expect(%s);", expr))
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return nil // unions not supported
	}

	name := sanitizeName(t.Name)
	fields := []struct{ name, typ string }{}
	if c.env != nil {
		st := types.StructType{Name: t.Name, Fields: map[string]types.Type{}, Order: []string{}}
		for _, m := range t.Members {
			if m.Field != nil {
				ft := c.resolveTypeRef(m.Field.Type)
				st.Fields[m.Field.Name] = ft
				st.Order = append(st.Order, m.Field.Name)
				fields = append(fields, struct{ name, typ string }{sanitizeName(m.Field.Name), c.javaType(ft)})
			}
		}
		c.env.SetStruct(t.Name, st)
	} else {
		for _, m := range t.Members {
			if m.Field != nil {
				fields = append(fields, struct{ name, typ string }{sanitizeName(m.Field.Name), c.javaType(c.resolveTypeRef(m.Field.Type))})
			}
		}
	}

	c.writeln(fmt.Sprintf("static class %s {", name))
	c.indent++
	for _, f := range fields {
		c.writeln(fmt.Sprintf("%s %s;", f.typ, f.name))
	}
	if len(fields) > 0 {
		params := make([]string, len(fields))
		for i, f := range fields {
			params[i] = f.typ + " " + f.name
		}
		c.writeln("")
		c.writeln(fmt.Sprintf("%s(%s) {", name, strings.Join(params, ", ")))
		c.indent++
		for _, f := range fields {
			c.writeln(fmt.Sprintf("this.%s = %s;", f.name, f.name))
		}
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	if c.env != nil {
		c.env.SetFunc(fun.Name, fun)
	}

	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	if ft.Params == nil {
		ft.Params = make([]types.Type, len(fun.Params))
		for i, p := range fun.Params {
			if p.Type != nil {
				ft.Params[i] = c.resolveTypeRef(p.Type)
			} else {
				ft.Params[i] = types.AnyType{}
			}
		}
	}
	if ft.Return == nil && fun.Return != nil {
		ft.Return = c.resolveTypeRef(fun.Return)
	}
	if ft.Return == nil {
		ft.Return = types.VoidType{}
	}
	if c.env != nil {
		c.env.SetVar(fun.Name, ft, false)
	}

	c.writeIndent()
	ret := c.javaType(ft.Return)
	if ret == "" {
		ret = "void"
	}
	c.buf.WriteString("static " + ret + " " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		ptype := "var"
		if i < len(ft.Params) {
			ptype = c.javaType(ft.Params[i])
		} else if p.Type != nil {
			ptype = c.javaType(c.resolveTypeRef(p.Type))
		}
		if ptype == "" {
			ptype = "var"
		}
		c.buf.WriteString(ptype + " " + sanitizeName(p.Name))
	}
	c.buf.WriteString(") {")
	c.buf.WriteByte('\n')
	c.indent++

	prevEnv := c.env
	if prevEnv != nil {
		child := types.NewEnv(prevEnv)
		for i, p := range fun.Params {
			var t types.Type = types.AnyType{}
			if i < len(ft.Params) {
				t = ft.Params[i]
			} else if p.Type != nil {
				t = c.resolveTypeRef(p.Type)
			}
			child.SetVar(p.Name, t, true)
		}
		c.env = child
	}

	prevRet := c.returnType
	c.returnType = ft.Return
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			if prevEnv != nil {
				c.env = prevEnv
			}
			c.returnType = prevRet
			return err
		}
	}
	c.returnType = prevRet
	if prevEnv != nil {
		c.env = prevEnv
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
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []string{left}
	lists := []bool{c.isListExpr(b.Left.Value)}
	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		lists = append(lists, c.isListExpr(part.Right))
		ops = append(ops, part.Op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
	}

	contains := func(sl []string, s string) bool {
		for _, v := range sl {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if !contains(level, ops[i]) {
				i++
				continue
			}
			op := ops[i]
			l := operands[i]
			r := operands[i+1]
			llist := lists[i]
			rlist := lists[i+1]

			var expr string
			var isList bool
			switch op {
			case "+":
				if llist || rlist {
					c.helpers["_concat"] = true
					expr = fmt.Sprintf("_concat(%s, %s)", l, r)
					isList = true
				} else {
					expr = fmt.Sprintf("(%s + %s)", l, r)
				}
			case "in":
				expr = fmt.Sprintf("%s.containsKey(%s)", r, l)
			default:
				expr = fmt.Sprintf("(%s %s %s)", l, op, r)
			}

			operands[i] = expr
			lists[i] = isList
			operands = append(operands[:i+1], operands[i+2:]...)
			lists = append(lists[:i+1], lists[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected binary expression state")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if u == nil {
		return "", nil
	}
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" || op == "!" {
			expr = fmt.Sprintf("(%s%s)", op, expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if p == nil {
		return "", nil
	}
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isMapExpr(p) {
					expr = fmt.Sprintf("%s.get(%s)", expr, idx)
				} else if c.isStringExpr(p) {
					c.helpers["_indexString"] = true
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
				continue
			}
			start := "0"
			if op.Index.Start != nil {
				s, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				start = s
			}
			end := fmt.Sprintf("%s.length", expr)
			if op.Index.End != nil {
				e, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				end = e
			}
			if c.isStringExpr(p) {
				expr = fmt.Sprintf("%s.substring(%s, %s)", expr, start, end)
			} else {
				c.helpers["_slice"] = true
				expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
			}
			continue
		}
		if op.Cast != nil {
			t := c.resolveTypeRef(op.Cast.Type)
			expr = fmt.Sprintf("(%s)(%s)", c.javaType(t), expr)
			continue
		}
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, arg)
			}
			if expr == "print" {
				expr = fmt.Sprintf("System.out.println(%s)", joinArgs(args))
			} else if expr == "len" {
				if c.isMapExprByExpr(op.Call.Args[0]) {
					expr = fmt.Sprintf("%s.size()", args[0])
				} else if c.isStringExprByExpr(op.Call.Args[0]) {
					expr = fmt.Sprintf("%s.length()", args[0])
				} else {
					expr = fmt.Sprintf("%s.length", args[0])
				}
			} else if expr == "str" {
				expr = fmt.Sprintf("String.valueOf(%s)", joinArgs(args))
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, joinArgs(args))
			}
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) isStringExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isMapExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isMapExprByExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	if e.Binary.Left.Value == nil {
		return false
	}
	return c.isMapExpr(e.Binary.Left.Value)
}

func (c *Compiler) isStringExprByExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	if e.Binary.Left.Value == nil {
		return false
	}
	return c.isStringExpr(e.Binary.Left.Value)
}

func (c *Compiler) isListExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
			return false
		}
	}
	if p.Target.List != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.IntType); ok {
						return true
					}
				}
			}
		}
	}
	return false
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("\"%s\"", *p.Lit.Str), nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ce, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = ce
		}
		var elemType types.Type = types.AnyType{}
		if len(p.List.Elems) > 0 {
			elemType = c.inferExprType(p.List.Elems[0])
			for _, el := range p.List.Elems[1:] {
				t := c.inferExprType(el)
				if !equalTypes(elemType, t) {
					elemType = types.AnyType{}
					break
				}
			}
		}
		return fmt.Sprintf("new %s[]{%s}", c.javaType(elemType), joinArgs(elems)), nil
	case p.Map != nil:
		items := []string{}
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items = append(items, k+", "+v)
		}
		if len(items) == 0 {
			return "new java.util.HashMap<>()", nil
		}
		return "new java.util.HashMap<>(java.util.Map.of(" + joinArgs(items) + "))", nil
	case p.Struct != nil:
		args := []string{}
		if c.env != nil {
			if st, ok := c.env.GetStruct(p.Struct.Name); ok {
				m := map[string]string{}
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					m[f.Name] = v
				}
				for _, fn := range st.Order {
					args = append(args, m[fn])
				}
			}
		}
		if len(args) == 0 {
			for _, f := range p.Struct.Fields {
				v, err := c.compileExpr(f.Value)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
		}
		return fmt.Sprintf("new %s(%s)", sanitizeName(p.Struct.Name), joinArgs(args)), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, f := range p.Selector.Tail {
			expr += "." + sanitizeName(f)
		}
		return expr, nil
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			ce, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, ce)
		}
		name := sanitizeName(p.Call.Func)
		if name == "print" {
			if len(args) == 1 {
				return "System.out.println(" + args[0] + ")", nil
			}
			expr := args[0]
			for i := 1; i < len(args); i++ {
				expr += " + \" \" + " + args[i]
			}
			return "System.out.println(" + expr + ")", nil
		}
		if name == "len" && len(args) == 1 {
			if c.isMapExprByExpr(p.Call.Args[0]) {
				return args[0] + ".size()", nil
			}
			if c.isStringExprByExpr(p.Call.Args[0]) {
				return args[0] + ".length()", nil
			}
			return args[0] + ".length", nil
		}
		if name == "str" && len(args) == 1 {
			return "String.valueOf(" + args[0] + ")", nil
		}
		if name == "input" && len(args) == 0 {
			c.helpers["_input"] = true
			return "_input()", nil
		}
		if name == "count" && len(args) == 1 {
			c.helpers["_count"] = true
			return "_count(" + joinArgs(args) + ")", nil
		}
		if name == "avg" && len(args) == 1 {
			c.helpers["_avg"] = true
			return "_avg(" + joinArgs(args) + ")", nil
		}
		if name == "now" && len(args) == 0 {
			return "System.nanoTime()", nil
		}
		if name == "json" && len(args) == 1 {
			c.helpers["_json"] = true
			return "_json(" + args[0] + ")", nil
		}
		if c.env != nil {
			if _, ok := c.env.GetFunc(p.Call.Func); !ok {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						if len(ft.Params) == 0 {
							return name + ".get()", nil
						}
						return name + ".apply(" + joinArgs(args) + ")", nil
					}
				}
			}
		}
		return name + "(" + joinArgs(args) + ")", nil
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Group != nil:
		return c.compileExpr(p.Group)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		} else {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
		params[i] = sanitizeName(p.Name)
	}
	sub := &Compiler{helpers: c.helpers, env: child}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr + ";")
	} else {
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return "(" + strings.Join(params, ", ") + ") -> {\n" + body + "}", nil
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "bool":
			return types.BoolType{}
		case "string":
			return types.StringType{}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: c.resolveTypeRef(t.Generic.Args[0])}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: c.resolveTypeRef(t.Generic.Args[0]), Value: c.resolveTypeRef(t.Generic.Args[1])}
		}
	}
	return types.AnyType{}
}

func (c *Compiler) javaType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		// only support list<int> -> int[] for now
		return c.javaType(tt.Elem) + "[]"
	case types.MapType:
		key := c.javaType(tt.Key)
		val := c.javaType(tt.Value)
		key = boxedType(key)
		val = boxedType(val)
		return "java.util.Map<" + key + ", " + val + ">"
	case types.FuncType:
		ret := boxedType(c.javaType(tt.Return))
		if len(tt.Params) == 0 {
			return "java.util.function.Supplier<" + ret + ">"
		}
		if len(tt.Params) == 1 {
			arg := boxedType(c.javaType(tt.Params[0]))
			return "java.util.function.Function<" + arg + ", " + ret + ">"
		}
		if len(tt.Params) == 2 {
			a := boxedType(c.javaType(tt.Params[0]))
			b := boxedType(c.javaType(tt.Params[1]))
			return "java.util.function.BiFunction<" + a + ", " + b + ", " + ret + ">"
		}
		return "java.util.function.Function"
	case types.StructType:
		return sanitizeName(tt.Name)
	default:
		return "Object"
	}
}

func boxedType(typ string) string {
	switch typ {
	case "int":
		return "Integer"
	case "double":
		return "Double"
	case "boolean":
		return "Boolean"
	default:
		return typ
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "null"
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.helpers["_fetch"] = true
	return fmt.Sprintf("_fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "null"
	if l.With != nil {
		o, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.helpers["_dataset"] = true
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "null"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "null"
	if s.With != nil {
		o, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.helpers["_dataset"] = true
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var prompt, text, model string
	params := []string{}
	for _, f := range g.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		switch f.Name {
		case "prompt":
			prompt = v
		case "text":
			text = v
		case "model":
			model = v
		default:
			params = append(params, fmt.Sprintf("%q, %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramMap := "null"
	if len(params) > 0 {
		paramMap = "java.util.Map.of(" + joinArgs(params) + ")"
	}
	if model == "" {
		model = "null"
	}
	if g.Target == "embedding" {
		c.helpers["_genEmbed"] = true
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramMap), nil
	}

	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.helpers["_genStruct"] = true
			return fmt.Sprintf("_genStruct(%s.class, %s, %s, %s)", sanitizeName(g.Target), prompt, model, paramMap), nil
		}
	}

	c.helpers["_genText"] = true
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramMap), nil
}

func (c *Compiler) emitRuntime() {
	if c.helpers["_input"] {
		c.writeln("")
		c.writeln("static java.util.Scanner _scanner = new java.util.Scanner(System.in);")
		c.writeln("static String _input() {")
		c.indent++
		c.writeln("return _scanner.nextLine();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_count"] {
		c.writeln("")
		c.writeln("static int _count(int[] arr) {")
		c.indent++
		c.writeln("return arr.length;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_avg"] {
		c.writeln("")
		c.writeln("static int _avg(int[] arr) {")
		c.indent++
		c.writeln("if (arr.length == 0) return 0;")
		c.writeln("int sum = 0;")
		c.writeln("for (int v : arr) {")
		c.indent++
		c.writeln("sum += v;")
		c.indent--
		c.writeln("}")
		c.writeln("return sum / arr.length;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_concat"] {
		c.writeln("")
		c.writeln("static int[] _concat(int[] a, int[] b) {")
		c.indent++
		c.writeln("int[] res = new int[a.length + b.length];")
		c.writeln("System.arraycopy(a, 0, res, 0, a.length);")
		c.writeln("System.arraycopy(b, 0, res, a.length, b.length);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static boolean[] _concat(boolean[] a, boolean[] b) {")
		c.indent++
		c.writeln("boolean[] res = new boolean[a.length + b.length];")
		c.writeln("System.arraycopy(a, 0, res, 0, a.length);")
		c.writeln("System.arraycopy(b, 0, res, a.length, b.length);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static <T> T[] _concat(T[] a, T[] b) {")
		c.indent++
		c.writeln("T[] res = java.util.Arrays.copyOf(a, a.length + b.length);")
		c.writeln("System.arraycopy(b, 0, res, a.length, b.length);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_slice"] {
		c.writeln("")
		c.writeln("static int[] _slice(int[] arr, int i, int j) {")
		c.indent++
		c.writeln("if (i < 0) i += arr.length;")
		c.writeln("if (j < 0) j += arr.length;")
		c.writeln("if (i < 0) i = 0;")
		c.writeln("if (j > arr.length) j = arr.length;")
		c.writeln("if (j < i) j = i;")
		c.writeln("int[] res = new int[j - i];")
		c.writeln("System.arraycopy(arr, i, res, 0, j - i);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_indexString"] {
		c.writeln("")
		c.writeln("static String _indexString(String s, int i) {")
		c.indent++
		c.writeln("char[] runes = s.toCharArray();")
		c.writeln("if (i < 0) i += runes.length;")
		c.writeln("if (i < 0 || i >= runes.length) throw new RuntimeException(\"index out of range\");")
		c.writeln("return String.valueOf(runes[i]);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_expect"] {
		c.writeln("")
		c.writeln("static void expect(boolean cond) {")
		c.indent++
		c.writeln("if (!cond) throw new RuntimeException(\"expect failed\");")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_genText"] {
		c.writeln("")
		c.writeln("static String _genText(String prompt, String model, java.util.Map<String,Object> params) {")
		c.indent++
		c.writeln("// TODO: integrate with an LLM")
		c.writeln("return prompt;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_genEmbed"] {
		c.writeln("")
		c.writeln("static double[] _genEmbed(String text, String model, java.util.Map<String,Object> params) {")
		c.indent++
		c.writeln("double[] vec = new double[text.length()];")
		c.writeln("for (int i = 0; i < text.length(); i++) {")
		c.indent++
		c.writeln("vec[i] = text.charAt(i);")
		c.indent--
		c.writeln("}")
		c.writeln("return vec;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_genStruct"] {
		c.writeln("")
		c.writeln("static <T> T _genStruct(Class<T> cls, String prompt, String model, java.util.Map<String,Object> params) {")
		c.indent++
		c.writeln("// TODO: integrate with an LLM and parse JSON")
		c.writeln("try {")
		c.indent++
		c.writeln("return cls.getDeclaredConstructor().newInstance();")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_fetch"] {
		c.writeln("")
		c.writeln("static java.util.Map<String,Object> _fetch(String url, java.util.Map<String,Object> opts) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();")
		c.writeln("java.net.http.HttpRequest req = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url)).build();")
		c.writeln("java.net.http.HttpResponse<String> resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString());")
		c.writeln("java.util.Map<String,Object> out = new java.util.HashMap<>();")
		c.writeln("out.put(\"status\", resp.statusCode());")
		c.writeln("out.put(\"body\", resp.body());")
		c.writeln("return out;")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_dataset"] {
		c.writeln("")
		c.writeln("static java.util.List<java.util.Map<String,Object>> _load(String path, java.util.Map<String,Object> opts) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("String format = opts != null && opts.get(\"format\") != null ? opts.get(\"format\").toString() : \"csv\";")
		c.writeln("boolean header = opts == null || !opts.containsKey(\"header\") ? true : Boolean.parseBoolean(opts.get(\"header\").toString());")
		c.writeln("char delim = ',';")
		c.writeln("if (opts != null && opts.get(\"delimiter\") != null) { String d = opts.get(\"delimiter\").toString(); if (!d.isEmpty()) delim = d.charAt(0); }")
		c.writeln("if (\"tsv\".equals(format)) { delim='\t'; format=\"csv\"; }")
		c.writeln("java.io.BufferedReader r;")
		c.writeln("if (path == null || path.isEmpty() || path.equals(\"-\")) {")
		c.indent++
		c.writeln("r = new java.io.BufferedReader(new java.io.InputStreamReader(System.in));")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("r = java.nio.file.Files.newBufferedReader(java.nio.file.Path.of(path));")
		c.indent--
		c.writeln("}")
		c.writeln("java.util.List<String> lines = new java.util.ArrayList<>();")
		c.writeln("for (String line; (line = r.readLine()) != null;) { if (!line.isEmpty()) lines.add(line); }")
		c.writeln("r.close();")
		c.writeln("java.util.List<java.util.Map<String,Object>> out = new java.util.ArrayList<>();")
		c.writeln("if (lines.isEmpty()) return out;")
		c.writeln("String[] headers = lines.get(0).split(Character.toString(delim));")
		c.writeln("int start = 0;")
		c.writeln("if (header) { start = 1; } else { for (int i=0;i<headers.length;i++) headers[i]=\"c\"+i; }")
		c.writeln("for (int i=start;i<lines.size();i++) {")
		c.indent++
		c.writeln("String[] parts = lines.get(i).split(Character.toString(delim));")
		c.writeln("java.util.Map<String,Object> row = new java.util.HashMap<>();")
		c.writeln("for (int j=0;j<headers.length;j++) {")
		c.indent++
		c.writeln("String val = j < parts.length ? parts[j] : \"\";")
		c.writeln("try { row.put(headers[j], Integer.parseInt(val)); } catch(NumberFormatException _e1) {")
		c.indent++
		c.writeln("try { row.put(headers[j], Double.parseDouble(val)); } catch(NumberFormatException _e2) { row.put(headers[j], val); }")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("out.add(row);")
		c.indent--
		c.writeln("}")
		c.writeln("return out;")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static void _save(java.util.List<java.util.Map<String,Object>> rows, String path, java.util.Map<String,Object> opts) {")
		c.indent++
		c.writeln("try {")
		c.indent++
		c.writeln("boolean header = opts != null && opts.get(\"header\") != null ? Boolean.parseBoolean(opts.get(\"header\").toString()) : false;")
		c.writeln("char delim = ',';")
		c.writeln("if (opts != null && opts.get(\"delimiter\") != null) { String d = opts.get(\"delimiter\").toString(); if (!d.isEmpty()) delim = d.charAt(0); }")
		c.writeln("java.io.BufferedWriter w;")
		c.writeln("if (path == null || path.isEmpty() || path.equals(\"-\")) {")
		c.indent++
		c.writeln("w = new java.io.BufferedWriter(new java.io.OutputStreamWriter(System.out));")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("w = java.nio.file.Files.newBufferedWriter(java.nio.file.Path.of(path));")
		c.indent--
		c.writeln("}")
		c.writeln("java.util.List<String> headers = rows.isEmpty() ? java.util.List.of() : new java.util.ArrayList<>(rows.get(0).keySet());")
		c.writeln("java.util.Collections.sort(headers);")
		c.writeln("if (header && !headers.isEmpty()) { w.write(String.join(Character.toString(delim), headers)); w.newLine(); }")
		c.writeln("for (java.util.Map<String,Object> row : rows) {")
		c.indent++
		c.writeln("java.util.List<String> rec = new java.util.ArrayList<>();")
		c.writeln("for (String h : headers) { Object v = row.get(h); rec.add(v==null?\"\":v.toString()); }")
		c.writeln("w.write(String.join(Character.toString(delim), rec));")
		c.writeln("w.newLine();")
		c.indent--
		c.writeln("}")
		c.writeln("w.flush(); if (path != null && !path.isEmpty() && !path.equals(\"-\")) w.close();")
		c.indent--
		c.writeln("} catch (Exception e) {")
		c.indent++
		c.writeln("throw new RuntimeException(e);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["_json"] {
		c.writeln("")
		c.writeln("static void _json(Object v) {")
		c.indent++
		c.writeln("System.out.println(_toJson(v));")
		c.indent--
		c.writeln("}")

		c.writeln("")
		c.writeln("static String _toJson(Object v) {")
		c.indent++
		c.writeln("if (v == null) return \"null\";")
		c.writeln("if (v instanceof String) {")
		c.indent++
		c.writeln("String s = (String) v;")
		c.writeln(`return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";`)
		c.indent--
		c.writeln("}")
		c.writeln("if (v instanceof Number || v instanceof Boolean) return v.toString();")
		c.writeln("if (v.getClass().isArray()) {")
		c.indent++
		c.writeln("int n = java.lang.reflect.Array.getLength(v);")
		c.writeln("StringBuilder sb = new StringBuilder();")
		c.writeln("sb.append('[');")
		c.writeln("for (int i=0;i<n;i++) {")
		c.indent++
		c.writeln("if (i>0) sb.append(',');")
		c.writeln("sb.append(_toJson(java.lang.reflect.Array.get(v,i)));")
		c.indent--
		c.writeln("}")
		c.writeln("sb.append(']');")
		c.writeln("return sb.toString();")
		c.indent--
		c.writeln("}")
		c.writeln("if (v instanceof java.util.List<?>) {")
		c.indent++
		c.writeln("java.util.List<?> l=(java.util.List<?>)v;")
		c.writeln("StringBuilder sb=new StringBuilder();")
		c.writeln("sb.append('[');")
		c.writeln("for (int i=0;i<l.size();i++) {")
		c.indent++
		c.writeln("if(i>0) sb.append(',');")
		c.writeln("sb.append(_toJson(l.get(i)));")
		c.indent--
		c.writeln("}")
		c.writeln("sb.append(']');")
		c.writeln("return sb.toString();")
		c.indent--
		c.writeln("}")
		c.writeln("if (v instanceof java.util.Map<?,?>) {")
		c.indent++
		c.writeln("java.util.Map<?,?> m=(java.util.Map<?,?>)v;")
		c.writeln("StringBuilder sb=new StringBuilder();")
		c.writeln("sb.append('{');")
		c.writeln("boolean first=true;")
		c.writeln("for (var e : m.entrySet()) {")
		c.indent++
		c.writeln("if(!first) sb.append(',');")
		c.writeln("first=false;")
		c.writeln("sb.append(_toJson(String.valueOf(e.getKey())));")
		c.writeln("sb.append(':');")
		c.writeln("sb.append(_toJson(e.getValue()));")
		c.indent--
		c.writeln("}")
		c.writeln("sb.append('}');")
		c.writeln("return sb.toString();")
		c.indent--
		c.writeln("}")
		c.writeln("return _toJson(v.toString());")
		c.indent--
		c.writeln("}")
	}
}

// compileExprHint compiles an expression using a type hint. The hint is used
// for list literals that would otherwise default to any.
func (c *Compiler) compileExprHint(e *parser.Expr, hint types.Type) (string, error) {
	if lt, ok := hint.(types.ListType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ll := e.Binary.Left.Value.Target.List; ll != nil {
				elems := make([]string, len(ll.Elems))
				for i, el := range ll.Elems {
					ev, err := c.compileExprHint(el, lt.Elem)
					if err != nil {
						return "", err
					}
					elems[i] = ev
				}
				return fmt.Sprintf("new %s[]{%s}", c.javaType(lt.Elem), joinArgs(elems)), nil
			}
		}
	}
	return c.compileExpr(e)
}

func joinArgs(args []string) string {
	if len(args) == 0 {
		return ""
	}
	res := args[0]
	for i := 1; i < len(args); i++ {
		res += ", " + args[i]
	}
	return res
}
