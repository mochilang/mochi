package gocode

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Go source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	imports map[string]bool
	env     *types.Env
	helpers map[string]bool
}

// New creates a new Go compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{imports: make(map[string]bool), env: env, helpers: make(map[string]bool)}
}

// Compile returns Go source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("package main")
	c.writeln("")

	// Top level statements may require imports.
	for _, stmt := range prog.Statements {
		c.scanImports(stmt)
	}

	if len(c.imports) > 0 {
		c.writeln("import (")
		c.indent++
		keys := make([]string, 0, len(c.imports))
		for k := range c.imports {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, imp := range keys {
			c.writeln(fmt.Sprintf("\"%s\"", imp))
		}
		c.indent--
		c.writeln(")")
		c.writeln("")
	}

	if hasExpect(prog) {
		c.writeln("func expect(cond bool) {")
		c.indent++
		c.writeln("if !cond { panic(\"expect failed\") }")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	// Emit function declarations first.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit test functions.
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("func main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s()", name))
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.writeRuntimeHelpers()

	return c.buf.Bytes(), nil
}

// --- Statement Compilation ---

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
		c.writeln(expr)
		return nil
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr)
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.writeln("expect(" + expr + ")")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	value := "nil"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("var %s = %s", name, value))
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	value := "nil"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("var %s = %s", name, value))
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	name := sanitizeName(s.Name)
	value, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", name, value))
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString("if " + cond + " {")
	c.buf.WriteByte('\n')
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
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for %s := %s; %s < %s; %s++ {\n", name, start, name, end, name))
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}\n")
		return nil
	}

	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	c.useHelper("_iter")
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("for _, %s := range _iter(%s) {\n", name, src))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("func " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name) + " any")
	}
	c.buf.WriteString(") any {\n")
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("func " + name + "() {\n")
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

// --- Expression Compilation ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	leftType := c.inferUnaryType(b.Left)
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightType := c.inferPostfixType(op.Right)
		switch op.Op {
		case "+":
			if _, ok := leftType.(types.IntType); ok {
				if _, ok := rightType.(types.IntType); ok {
					expr = fmt.Sprintf("(%s + %s)", expr, right)
				} else {
					c.useHelper("_add")
					expr = fmt.Sprintf("_add(%s, %s)", expr, right)
				}
			} else if _, ok := leftType.(types.FloatType); ok {
				if _, ok := rightType.(types.FloatType); ok {
					expr = fmt.Sprintf("(%s + %s)", expr, right)
				} else {
					c.useHelper("_add")
					expr = fmt.Sprintf("_add(%s, %s)", expr, right)
				}
			} else if _, ok := leftType.(types.StringType); ok {
				if _, ok := rightType.(types.StringType); ok {
					expr = fmt.Sprintf("%s + %s", expr, right)
				} else {
					c.useHelper("_add")
					expr = fmt.Sprintf("_add(%s, %s)", expr, right)
				}
			} else {
				c.useHelper("_add")
				expr = fmt.Sprintf("_add(%s, %s)", expr, right)
			}
		case "-":
			if _, ok := leftType.(types.IntType); ok {
				if _, ok := rightType.(types.IntType); ok {
					expr = fmt.Sprintf("(%s - %s)", expr, right)
				} else {
					c.useHelper("_sub")
					expr = fmt.Sprintf("_sub(%s, %s)", expr, right)
				}
			} else if _, ok := leftType.(types.FloatType); ok {
				if _, ok := rightType.(types.FloatType); ok {
					expr = fmt.Sprintf("(%s - %s)", expr, right)
				} else {
					c.useHelper("_sub")
					expr = fmt.Sprintf("_sub(%s, %s)", expr, right)
				}
			} else {
				c.useHelper("_sub")
				expr = fmt.Sprintf("_sub(%s, %s)", expr, right)
			}
		case "*":
			if _, ok := leftType.(types.IntType); ok {
				if _, ok := rightType.(types.IntType); ok {
					expr = fmt.Sprintf("(%s * %s)", expr, right)
				} else {
					c.useHelper("_mul")
					expr = fmt.Sprintf("_mul(%s, %s)", expr, right)
				}
			} else if _, ok := leftType.(types.FloatType); ok {
				if _, ok := rightType.(types.FloatType); ok {
					expr = fmt.Sprintf("(%s * %s)", expr, right)
				} else {
					c.useHelper("_mul")
					expr = fmt.Sprintf("_mul(%s, %s)", expr, right)
				}
			} else {
				c.useHelper("_mul")
				expr = fmt.Sprintf("_mul(%s, %s)", expr, right)
			}
		case "/":
			if _, ok := leftType.(types.IntType); ok {
				if _, ok := rightType.(types.IntType); ok {
					expr = fmt.Sprintf("(%s / %s)", expr, right)
				} else {
					c.useHelper("_div")
					expr = fmt.Sprintf("_div(%s, %s)", expr, right)
				}
			} else if _, ok := leftType.(types.FloatType); ok {
				if _, ok := rightType.(types.FloatType); ok {
					expr = fmt.Sprintf("(%s / %s)", expr, right)
				} else {
					c.useHelper("_div")
					expr = fmt.Sprintf("_div(%s, %s)", expr, right)
				}
			} else {
				c.useHelper("_div")
				expr = fmt.Sprintf("_div(%s, %s)", expr, right)
			}
		case "%":
			if _, ok := leftType.(types.IntType); ok {
				if _, ok := rightType.(types.IntType); ok {
					expr = fmt.Sprintf("(%s %% %s)", expr, right)
				} else {
					c.useHelper("_mod")
					expr = fmt.Sprintf("_mod(%s, %s)", expr, right)
				}
			} else {
				c.useHelper("_mod")
				expr = fmt.Sprintf("_mod(%s, %s)", expr, right)
			}
		case "==":
			expr = fmt.Sprintf("(%s == %s)", expr, right)
		case "!=":
			expr = fmt.Sprintf("(%s != %s)", expr, right)
		case "<":
			expr = fmt.Sprintf("(%s < %s)", expr, right)
		case "<=":
			expr = fmt.Sprintf("(%s <= %s)", expr, right)
		case ">":
			expr = fmt.Sprintf("(%s > %s)", expr, right)
		case ">=":
			expr = fmt.Sprintf("(%s >= %s)", expr, right)
		default:
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, right)
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
		op := u.Ops[i]
		if op == "-" || op == "!" {
			val = fmt.Sprintf("%s%s", op, val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Index {
		if op.Colon == nil {
			key, err := c.compileExpr(op.Start)
			if err != nil {
				return "", err
			}
			c.useHelper("_index")
			val = fmt.Sprintf("_index(%s, %s)", val, key)
		} else {
			c.useHelper("_slice")
			start := "0"
			if op.Start != nil {
				s, err := c.compileExpr(op.Start)
				if err != nil {
					return "", err
				}
				start = s
			}
			end := fmt.Sprintf("len(%s)", val)
			if op.End != nil {
				e, err := c.compileExpr(op.End)
				if err != nil {
					return "", err
				}
				end = e
			}
			val = fmt.Sprintf("_slice(%s, %s, %s)", val, start, end)
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Selector != nil:
		base := sanitizeName(p.Selector.Root)
		for _, field := range p.Selector.Tail {
			base += "." + sanitizeName(field)
		}
		return base, nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[]any{" + strings.Join(elems, ", ") + "}", nil
	case p.Map != nil:
		parts := make([]string, len(p.Map.Items))
		for i, item := range p.Map.Items {
			k, err := c.compileExpr(item.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(item.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", k, v)
		}
		return "map[string]any{" + strings.Join(parts, ", ") + "}", nil
	default:
		return "nil", fmt.Errorf("unsupported primary expression")
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	default:
		return "nil", fmt.Errorf("invalid literal")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
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
		c.imports["fmt"] = true
		return fmt.Sprintf("fmt.Println(%s)", argStr), nil
	case "len":
		return fmt.Sprintf("len(%s)", argStr), nil
	case "now":
		c.imports["time"] = true
		return "time.Now().UnixNano()", nil
	case "json":
		c.imports["encoding/json"] = true
		c.imports["fmt"] = true
		return fmt.Sprintf("func(){b,_:=json.Marshal(%s);fmt.Println(string(b))}()", argStr), nil
	default:
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name) + " any"
	}
	sub := &Compiler{imports: c.imports}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	code := "func(" + strings.Join(params, ", ") + ") any {\n" + body + "}"
	return code, nil
}

// --- Helpers ---

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

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("\t", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else if r > 127 {
			b.WriteRune('_')
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	return b.String()
}

func goType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "int"
	case types.FloatType:
		return "float64"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "[]" + goType(tt.Elem)
	case types.MapType:
		return fmt.Sprintf("map[%s]%s", goType(tt.Key), goType(tt.Value))
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = goType(p)
		}
		ret := goType(tt.Return)
		if ret == "" || ret == "void" {
			return fmt.Sprintf("func(%s)", strings.Join(params, ", "))
		}
		return fmt.Sprintf("func(%s) %s", strings.Join(params, ", "), ret)
	case types.AnyType:
		return "any"
	default:
		return "any"
	}
}

func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	return c.inferBinaryType(e.Binary)
}

func (c *Compiler) inferBinaryType(b *parser.BinaryExpr) types.Type {
	if b == nil {
		return types.AnyType{}
	}
	t := c.inferUnaryType(b.Left)
	for _, op := range b.Right {
		rt := c.inferPostfixType(op.Right)
		switch op.Op {
		case "+", "-", "*", "/", "%":
			if _, ok := t.(types.IntType); ok {
				if _, ok := rt.(types.IntType); ok {
					t = types.IntType{}
					continue
				}
			}
			if _, ok := t.(types.FloatType); ok {
				if _, ok := rt.(types.FloatType); ok {
					t = types.FloatType{}
					continue
				}
			}
			if op.Op == "+" {
				if _, ok := t.(types.StringType); ok {
					if _, ok := rt.(types.StringType); ok {
						t = types.StringType{}
						continue
					}
				}
			}
			t = types.AnyType{}
		case "==", "!=", "<", "<=", ">", ">=":
			t = types.BoolType{}
		default:
			t = types.AnyType{}
		}
	}
	return t
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	return c.inferPostfixType(u.Value)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	t := c.inferPrimaryType(p.Target)
	for range p.Index {
		switch t.(type) {
		case types.ListType:
			t = types.AnyType{}
		case types.MapType:
			t = types.AnyType{}
		default:
			t = types.AnyType{}
		}
	}
	return t
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return types.IntType{}
		case p.Lit.Float != nil:
			return types.FloatType{}
		case p.Lit.Str != nil:
			return types.StringType{}
		case p.Lit.Bool != nil:
			return types.BoolType{}
		}
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 && c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				return t
			}
		}
		return types.AnyType{}
	case p.Call != nil:
		switch p.Call.Func {
		case "len", "now":
			return types.IntType{}
		default:
			if c.env != nil {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						return ft.Return
					}
				}
			}
			return types.AnyType{}
		}
	case p.Group != nil:
		return c.inferExprType(p.Group)
	case p.List != nil:
		return types.ListType{Elem: types.AnyType{}}
	case p.Map != nil:
		return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	}
	return types.AnyType{}
}

func resultType(op string, left, right types.Type) types.Type {
	switch op {
	case "+", "-", "*", "/", "%":
		if _, ok := left.(types.IntType); ok {
			if _, ok := right.(types.IntType); ok {
				return types.IntType{}
			}
		}
		if _, ok := left.(types.FloatType); ok {
			if _, ok := right.(types.FloatType); ok {
				return types.FloatType{}
			}
		}
		if op == "+" {
			if _, ok := left.(types.StringType); ok {
				if _, ok := right.(types.StringType); ok {
					return types.StringType{}
				}
			}
		}
		return types.AnyType{}
	case "==", "!=", "<", "<=", ">", ">=":
		return types.BoolType{}
	default:
		return types.AnyType{}
	}
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

func (c *Compiler) scanImports(s *parser.Statement) {
	if s.Expr != nil {
		c.scanExprImports(s.Expr.Expr)
	}
	if s.Expect != nil {
		c.scanExprImports(s.Expect.Value)
	}
	if s.If != nil {
		c.scanExprImports(s.If.Cond)
		for _, t := range s.If.Then {
			c.scanImports(t)
		}
		if s.If.ElseIf != nil {
			c.scanImports(&parser.Statement{If: s.If.ElseIf})
		}
		for _, t := range s.If.Else {
			c.scanImports(t)
		}
	}
	if s.For != nil {
		c.scanExprImports(s.For.Source)
		if s.For.RangeEnd != nil {
			c.scanExprImports(s.For.RangeEnd)
		}
		for _, t := range s.For.Body {
			c.scanImports(t)
		}
	}
	if s.Fun != nil {
		for _, t := range s.Fun.Body {
			c.scanImports(t)
		}
	}
	if s.Test != nil {
		for _, t := range s.Test.Body {
			c.scanImports(t)
		}
	}
}

func (c *Compiler) scanExprImports(e *parser.Expr) {
	if e == nil {
		return
	}
	c.scanBinaryImports(e.Binary)
}

func (c *Compiler) scanBinaryImports(b *parser.BinaryExpr) {
	if b == nil {
		return
	}
	c.scanUnaryImports(b.Left)
	for _, op := range b.Right {
		c.scanPostfixImports(op.Right)
		if op.Op == "==" || op.Op == "!=" || op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=" || op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" || op.Op == "%" {
			// no import needed
		}
	}
}

func (c *Compiler) scanUnaryImports(u *parser.Unary) {
	if u == nil {
		return
	}
	c.scanPostfixImports(u.Value)
}

func (c *Compiler) scanPostfixImports(p *parser.PostfixExpr) {
	if p == nil {
		return
	}
	c.scanPrimaryImports(p.Target)
	for _, op := range p.Index {
		c.scanExprImports(op.Start)
		c.scanExprImports(op.End)
	}
}

func (c *Compiler) scanPrimaryImports(p *parser.Primary) {
	if p == nil {
		return
	}
	switch {
	case p.Call != nil:
		if p.Call.Func == "print" {
			c.imports["fmt"] = true
		}
		if p.Call.Func == "now" {
			c.imports["time"] = true
		}
		if p.Call.Func == "json" {
			c.imports["encoding/json"] = true
			c.imports["fmt"] = true
		}
		for _, a := range p.Call.Args {
			c.scanExprImports(a)
		}
	case p.Group != nil:
		c.scanExprImports(p.Group)
	case p.FunExpr != nil:
		if p.FunExpr.ExprBody != nil {
			c.scanExprImports(p.FunExpr.ExprBody)
		}
		for _, s := range p.FunExpr.BlockBody {
			c.scanImports(s)
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			c.scanExprImports(e)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			c.scanExprImports(it.Key)
			c.scanExprImports(it.Value)
		}
	case p.Selector != nil:
		// no imports
	case p.Lit != nil:
		// none
	}
}

// runtimeHelpers contains helper functions injected into generated programs.
const helperIndex = `func _index(v any, k any) any {
    switch s := v.(type) {
    case []any:
        i, ok := k.(int)
        if !ok {
            panic("invalid list index")
        }
        if i < 0 {
            i += len(s)
        }
        if i < 0 || i >= len(s) {
            panic("index out of range")
        }
        return s[i]
    case string:
        i, ok := k.(int)
        if !ok {
            panic("invalid string index")
        }
        runes := []rune(s)
        if i < 0 {
            i += len(runes)
        }
        if i < 0 || i >= len(runes) {
            panic("index out of range")
        }
        return string(runes[i])
    case map[string]any:
        ks, ok := k.(string)
        if !ok {
            panic("invalid map key")
        }
        return s[ks]
    default:
        panic("invalid index target")
    }
}
`

const helperSlice = `func _slice(v any, start, end int) any {
    switch s := v.(type) {
    case []any:
        l := len(s)
        if start < 0 {
            start += l
        }
        if end < 0 {
            end += l
        }
        if start < 0 || end > l || start > end {
            panic("slice out of range")
        }
        return s[start:end]
    case string:
        runes := []rune(s)
        l := len(runes)
        if start < 0 {
            start += l
        }
        if end < 0 {
            end += l
        }
        if start < 0 || end > l || start > end {
            panic("slice out of range")
        }
        return string(runes[start:end])
    default:
        panic("invalid slice target")
    }
}
`

const helperIter = `func _iter(v any) []any {
    switch s := v.(type) {
    case []any:
        return s
    case map[string]any:
        out := make([]any, 0, len(s))
        for k := range s {
            out = append(out, k)
        }
        return out
    case string:
        runes := []rune(s)
        out := make([]any, len(runes))
        for i, r := range runes {
            out[i] = string(r)
        }
        return out
    default:
        return nil
    }
}
`

const helperAdd = `func _add(a, b any) any { return a.(int) + b.(int) }`
const helperSub = `func _sub(a, b any) any { return a.(int) - b.(int) }`
const helperMul = `func _mul(a, b any) any { return a.(int) * b.(int) }`
const helperDiv = `func _div(a, b any) any { return a.(int) / b.(int) }`
const helperMod = `func _mod(a, b any) any { return a.(int) % b.(int) }`
const helperEq = `func _eq(a, b any) bool { return a == b }`
const helperNeq = `func _neq(a, b any) bool { return a != b }`
const helperLt = `func _lt(a, b any) bool { return a.(int) < b.(int) }`
const helperLe = `func _le(a, b any) bool { return a.(int) <= b.(int) }`
const helperGt = `func _gt(a, b any) bool { return a.(int) > b.(int) }`
const helperGe = `func _ge(a, b any) bool { return a.(int) >= b.(int) }`

var runtimeHelpers = map[string]string{
	"_index": helperIndex,
	"_slice": helperSlice,
	"_iter":  helperIter,
	"_add":   helperAdd,
	"_sub":   helperSub,
	"_mul":   helperMul,
	"_div":   helperDiv,
	"_mod":   helperMod,
	"_eq":    helperEq,
	"_neq":   helperNeq,
	"_lt":    helperLt,
	"_le":    helperLe,
	"_gt":    helperGt,
	"_ge":    helperGe,
}

func (c *Compiler) useHelper(name string) {
	if c.helpers != nil {
		c.helpers[name] = true
	}
}

func (c *Compiler) writeRuntimeHelpers() {
	if len(c.helpers) == 0 {
		return
	}
	keys := make([]string, 0, len(c.helpers))
	for k := range c.helpers {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		if code, ok := runtimeHelpers[k]; ok {
			c.writeln(code)
			c.writeln("")
		}
	}
}
