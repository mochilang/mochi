package javacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Java source code.
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	mainStmts []*parser.Statement
	helpers   map[string]bool
}

// New creates a new Java compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool)}
}

// Compile generates Java code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("public class Main {")
	c.indent++

	// collect function declarations and main statements
	for _, s := range prog.Statements {
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
		c.writeln("public static void main(String[] args) {")
		c.indent++
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
	case s.If != nil:
		return c.compileIf(s.If)
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
	typ := ""
	if stmt.Type != nil {
		typ = c.javaType(c.resolveTypeRef(stmt.Type))
	} else if c.env != nil {
		if t, err := c.env.GetVar(stmt.Name); err == nil {
			typ = c.javaType(t)
		}
	}
	if typ == "" {
		typ = "var"
	}
	expr := ""
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		expr = " = " + v
	}
	c.writeln(fmt.Sprintf("%s %s%s;", typ, sanitizeName(stmt.Name), expr))
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	typ := ""
	if stmt.Type != nil {
		typ = c.javaType(c.resolveTypeRef(stmt.Type))
	} else if c.env != nil {
		if t, err := c.env.GetVar(stmt.Name); err == nil {
			typ = c.javaType(t)
		}
	}
	if typ == "" {
		typ = "var"
	}
	expr := ""
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		expr = " = " + v
	}
	c.writeln(fmt.Sprintf("%s %s%s;", typ, sanitizeName(stmt.Name), expr))
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
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileReturn(stmt *parser.ReturnStmt) error {
	expr, err := c.compileExpr(stmt.Value)
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
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", name, start, name, end, name))
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
	if stmt.Source != nil && c.env != nil {
		if ident := stmt.Source.Binary.Left; ident != nil && len(ident.Ops) == 0 {
			p := ident.Value
			if p.Target != nil {
				if p.Target.Lit != nil && p.Target.Lit.Str != nil {
					asString = true
				}
				if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
					if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
						if _, ok := t.(types.StringType); ok {
							asString = true
						}
					}
				}
			}
		}
	}
	if asString {
		src += ".toCharArray()"
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

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	ret := "void"
	if fun.Return != nil {
		ret = c.javaType(c.resolveTypeRef(fun.Return))
	}
	c.buf.WriteString("static " + ret + " " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		ptype := c.javaType(c.resolveTypeRef(p.Type))
		if ptype == "" {
			ptype = "var"
		}
		c.buf.WriteString(ptype + " " + sanitizeName(p.Name))
	}
	c.buf.WriteString(") {")
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
				expr = fmt.Sprintf("System.out.println(%s)", joinArgs(args))
			} else if expr == "len" {
				expr = fmt.Sprintf("%s.length", args[0])
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

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
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
		elems := []string{}
		nested := false
		for _, e := range p.List.Elems {
			ce, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			if strings.HasPrefix(ce, "new int[") {
				nested = true
			}
			elems = append(elems, ce)
		}
		if nested {
			return "new int[][]{" + joinArgs(elems) + "}", nil
		}
		return "new int[]{" + joinArgs(elems) + "}", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return sanitizeName(p.Selector.Root), nil
		}
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
			return "System.out.println(" + joinArgs(args) + ")", nil
		}
		if name == "len" && len(args) == 1 {
			return args[0] + ".length", nil
		}
		if name == "str" && len(args) == 1 {
			return "String.valueOf(" + args[0] + ")", nil
		}
		if name == "count" && len(args) == 1 {
			c.helpers["_count"] = true
			return "_count(" + joinArgs(args) + ")", nil
		}
		if name == "avg" && len(args) == 1 {
			c.helpers["_avg"] = true
			return "_avg(" + joinArgs(args) + ")", nil
		}
		return name + "(" + joinArgs(args) + ")", nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
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
	default:
		return "Object"
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

func (c *Compiler) emitRuntime() {
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
