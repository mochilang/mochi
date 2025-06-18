package rscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Rust source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	helpers map[string]bool
	structs map[string]bool
}

// New creates a new Rust compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), structs: make(map[string]bool)}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile returns Rust source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, stmt := range prog.Statements {
		if stmt.Type != nil {
			if err := c.compileTypeDecl(stmt.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, stmt := range prog.Statements {
		if stmt.Fun != nil {
			if err := c.compileFun(stmt.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("fn main() {")
	c.indent++
	for _, stmt := range prog.Statements {
		if stmt.Fun == nil {
			if err := c.compileStmt(stmt); err != nil {
				return nil, err
			}
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.emitRuntime()
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("return %s;", val))
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s;", expr))
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Type != nil:
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	val := "Default::default()"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		val = v
	}
	name := sanitizeName(stmt.Name)
	c.writeln(fmt.Sprintf("let mut %s = %s;", name, val))
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	val := "Default::default()"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		val = v
	}
	name := sanitizeName(stmt.Name)
	c.writeln(fmt.Sprintf("let mut %s = %s;", name, val))
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s as usize]", lhs, iexpr)
	}
	val, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, val))
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		return fmt.Errorf("union types not supported")
	}
	c.writeln("#[derive(Clone, Debug)]")
	c.writeln(fmt.Sprintf("struct %s {", name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			field := sanitizeName(m.Field.Name)
			typ := rustType(m.Field.Type)
			c.writeln(fmt.Sprintf("%s: %s,", field, typ))
			if c.env != nil {
				st, ok := c.env.GetStruct(t.Name)
				if !ok {
					st = types.StructType{Name: t.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
				}
				st.Fields[m.Field.Name] = c.resolveTypeRef(m.Field.Type)
				st.Order = append(st.Order, m.Field.Name)
				c.env.SetStruct(t.Name, st)
			}
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	start, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	end := ""
	if stmt.RangeEnd != nil {
		end, err = c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
	}
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		c.writeln(fmt.Sprintf("for %s in %s..%s {", name, start, end))
	} else {
		if isStringLiteral(stmt.Source) {
			c.writeln(fmt.Sprintf("for %s in %s.chars() {", name, start))
		} else {
			c.writeln(fmt.Sprintf("for %s in %s {", name, start))
		}
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

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while %s {", cond))
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

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeln("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fn " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(fmt.Sprintf("%s: %s", sanitizeName(p.Name), rustType(p.Type)))
	}
	c.buf.WriteString(")")
	if fun.Return != nil {
		c.buf.WriteString(" -> " + rustType(fun.Return))
	}
	c.buf.WriteString(" {\n")
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		if op.Op == "in" {
			c.use("_in_map")
			expr = fmt.Sprintf("_in_map(&%s, &%s)", r, expr)
		} else {
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, r)
		}
	}
	return expr, nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
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
	var b strings.Builder
	b.WriteString("{\n")
	b.WriteString("    let mut _res = Vec::new();\n")
	b.WriteString(fmt.Sprintf("    for %s in %s {\n", sanitizeName(q.Var), src))
	if cond != "" {
		b.WriteString(fmt.Sprintf("        if %s {\n", cond))
		b.WriteString(fmt.Sprintf("            _res.push(%s);\n", sel))
		b.WriteString("        }\n")
	} else {
		b.WriteString(fmt.Sprintf("        _res.push(%s);\n", sel))
	}
	b.WriteString("    }\n")
	b.WriteString("    _res\n")
	b.WriteString("}")
	return b.String(), nil
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: c.resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: c.resolveTypeRef(args[0]), Value: c.resolveTypeRef(args[1])}
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
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		val = fmt.Sprintf("%s%s", op, val)
	}
	return val, nil
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
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if isStringLiteral(op.Index.Start) {
				expr = fmt.Sprintf("%s[%s]", expr, idx)
			} else {
				expr = fmt.Sprintf("%s[%s as usize]", expr, idx)
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		var st types.StructType
		var ok bool
		if c.env != nil {
			st, ok = c.env.GetStruct(p.Struct.Name)
		}
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			if ok {
				if ft, ok2 := st.Fields[f.Name]; ok2 {
					if _, isString := ft.(types.StringType); isString && isStringLiteral(f.Value) {
						v = fmt.Sprintf("%s.to_string()", v)
					}
				}
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s { %s }", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("vec![%s]", strings.Join(elems, ", ")), nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("(%s.to_string(), %s)", k, v)
		}
		return fmt.Sprintf("std::collections::HashMap::from([%s])", strings.Join(items, ", ")), nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		return "0", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return sanitizeName(p.Selector.Root), nil
		}
		parts := []string{sanitizeName(p.Selector.Root)}
		for _, t := range p.Selector.Tail {
			parts = append(parts, sanitizeName(t))
		}
		return strings.Join(parts, "."), nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", inner), nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")
	switch call.Func {
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("println!(\"{}\", %s)", argStr), nil
		}
		fmtParts := make([]string, len(args))
		for i := range args {
			fmtParts[i] = "{}"
		}
		fmtStr := strings.Join(fmtParts, " ")
		return fmt.Sprintf("println!(\"%s\", %s)", fmtStr, argStr), nil
	case "str":
		if len(args) == 1 {
			return fmt.Sprintf("format!(\"{}\", %s)", argStr), nil
		}
		fmtParts := make([]string, len(args))
		for i := range args {
			fmtParts[i] = "{}"
		}
		fmtStr := strings.Join(fmtParts, " ")
		return fmt.Sprintf("format!(\"%s\", %s)", fmtStr, argStr), nil
	case "len":
		if len(args) == 1 {
			return fmt.Sprintf("%s.len() as i32", args[0]), nil
		}
	case "count":
		if len(args) == 1 {
			c.use("_count")
			return fmt.Sprintf("_count(&%s)", args[0]), nil
		}
	case "avg":
		if len(args) == 1 {
			c.use("_avg")
			return fmt.Sprintf("_avg(&%s)", args[0]), nil
		}
	case "input":
		if len(args) == 0 {
			c.use("_input")
			return "_input()", nil
		}
	}
	return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
}

func rustType(t *parser.TypeRef) string {
	if t == nil {
		return "()"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i32"
		case "float":
			return "f64"
		case "string":
			return "String"
		case "bool":
			return "bool"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return fmt.Sprintf("Vec<%s>", rustType(t.Generic.Args[0]))
		}
	}
	if t.Fun != nil {
		return "()" // not handled
	}
	return "()"
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 {
		return false
	}
	if p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil {
		return false
	}
	return true
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
	res := b.String()
	if res == "" || !((res[0] >= 'A' && res[0] <= 'Z') || (res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
		res = "_" + res
	}
	return res
}
