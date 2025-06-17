package ktcode

import (
	"bytes"
	"fmt"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Kotlin source code.
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	mainStmts []*parser.Statement
}

// New creates a new Kotlin compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

// Compile generates Kotlin code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		c.mainStmts = append(c.mainStmts, s)
	}
	if len(c.mainStmts) > 0 {
		c.writeln("fun main() {")
		c.indent++
		for _, s := range c.mainStmts {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
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
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	default:
		return nil
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	expr, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("val %s = %s", sanitizeName(stmt.Name), expr))
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	value := "null"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("var %s = %s", sanitizeName(stmt.Name), value))
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, rhs))
	return nil
}

func (c *Compiler) compileReturn(stmt *parser.ReturnStmt) error {
	expr, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return fmt.Errorf("union types not supported")
	}
	name := sanitizeName(t.Name)
	fields := []string{}
	for _, m := range t.Members {
		if m.Field == nil {
			continue
		}
		typ := ktType(c.resolveTypeRef(m.Field.Type))
		fields = append(fields, fmt.Sprintf("val %s: %s", sanitizeName(m.Field.Name), typ))
	}
	c.writeln(fmt.Sprintf("data class %s(%s)", name, joinArgs(fields)))
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
		c.writeln(fmt.Sprintf("for (%s in %s until %s) {", name, start, end))
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
	c.writeln(fmt.Sprintf("for (%s in %s) {", name, src))
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
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if len(stmt.Else) == 0 {
		c.writeln("}")
		return nil
	}
	c.writeln("} else {")
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

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	varName := sanitizeName(q.Var)
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var where, sortKey, skip, take string
	if q.Where != nil {
		where, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		sortKey, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skip, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		take, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}
	var buf bytes.Buffer
	buf.WriteString("run {\n")
	buf.WriteString("\tvar res = " + src + "\n")
	if where != "" {
		buf.WriteString(fmt.Sprintf("\tres = res.filter { %s -> %s }\n", varName, where))
	}
	if sortKey != "" {
		buf.WriteString(fmt.Sprintf("\tres = res.sortedBy { %s -> %s }\n", varName, sortKey))
	}
	if skip != "" {
		buf.WriteString("\tres = res.drop(" + skip + ")\n")
	}
	if take != "" {
		buf.WriteString("\tres = res.take(" + take + ")\n")
	}
	buf.WriteString(fmt.Sprintf("\tres = res.map { %s -> %s }\n", varName, sel))
	buf.WriteString("\tres\n")
	buf.WriteString("}")
	return buf.String(), nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fun " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
		if p.Type != nil {
			c.buf.WriteString(": " + ktType(c.resolveTypeRef(p.Type)))
		}
	}
	ret := "Unit"
	if fun.Return != nil {
		ret = ktType(c.resolveTypeRef(fun.Return))
	}
	c.buf.WriteString(") : " + ret + " {")
	c.buf.WriteByte('\n')
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
	if len(b.Right) == 0 {
		return left, nil
	}
	expr := left
	for _, op := range b.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=":
			expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, rhs)
		default:
			return "", fmt.Errorf("unsupported op %s", op.Op)
		}
	}
	return expr, nil
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
			expr = fmt.Sprintf("%s%s", op, expr)
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
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s[%s]", expr, idx)
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
				expr = fmt.Sprintf("println(%s)", joinArgs(args))
			} else if expr == "len" {
				expr = fmt.Sprintf("%s.size", args[0])
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, joinArgs(args))
			}
			continue
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
			return fmt.Sprintf("\"%s\"", *p.Lit.Str), nil
		}
	case p.List != nil:
		elems := []string{}
		for _, e := range p.List.Elems {
			ce, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems = append(elems, ce)
		}
		return "listOf(" + joinArgs(elems) + ")", nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) == 0 {
			return expr, nil
		}
		for _, f := range p.Selector.Tail {
			expr += "." + sanitizeName(f)
		}
		return expr, nil
	case p.Struct != nil:
		args := []string{}
		for _, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			args = append(args, fmt.Sprintf("%s = %s", sanitizeName(f.Name), v))
		}
		return sanitizeName(p.Struct.Name) + "(" + joinArgs(args) + ")", nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
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
			return "println(" + joinArgs(args) + ")", nil
		}
		if name == "len" && len(args) == 1 {
			return args[0] + ".size", nil
		}
		if name == "str" && len(args) == 1 {
			return args[0] + ".toString()", nil
		}
		return name + "(" + joinArgs(args) + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
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
	}
	return types.AnyType{}
}

func ktType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return "List<" + ktType(tt.Elem) + ">"
	default:
		return "Any"
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
