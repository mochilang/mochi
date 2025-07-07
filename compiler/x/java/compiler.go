//go:build slow

package javacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf         bytes.Buffer
	indent      int
	needsAppend bool
	needsSum    bool
	needsAvg    bool
}

func New() *Compiler {
	return &Compiler{}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsAppend = false
	c.needsSum = false
	c.needsAvg = false

	// dry run to determine required helper functions
	tmp := &Compiler{}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := tmp.compileFun(s.Fun); err != nil {
				return nil, err
			}
		} else {
			if err := tmp.compileStmt(s); err != nil {
				// ignore errors in dry run
			}
		}
	}
	c.needsAppend = tmp.needsAppend
	c.needsSum = tmp.needsSum
	c.needsAvg = tmp.needsAvg

	c.writeln("public class Main {")
	c.indent++
	// emit function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
		}
	}

	if c.needsAppend {
		c.writeln("static java.util.List<Integer> append(java.util.List<Integer> l, int v) {")
		c.indent++
		c.writeln("java.util.List<Integer> out = new java.util.ArrayList<>(l);")
		c.writeln("out.add(v);")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
	}
	if c.needsSum {
		c.writeln("static int sum(java.util.List<Integer> nums) {")
		c.indent++
		c.writeln("int s = 0;")
		c.writeln("for (int n : nums) s += n;")
		c.writeln("return s;")
		c.indent--
		c.writeln("}")
	}
	if c.needsAvg {
		c.writeln("static double avg(java.util.List<Integer> nums) {")
		c.indent++
		c.writeln("if (nums.isEmpty()) return 0;")
		c.writeln("return (double)sum(nums) / nums.size();")
		c.indent--
		c.writeln("}")
	}

	c.writeln("public static void main(String[] args) {")
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
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		return c.compileExprStmt(s.Expr)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) typeName(t *parser.TypeRef) string {
	if t == nil || t.Simple == nil {
		return "int"
	}
	switch *t.Simple {
	case "int":
		return "int"
	case "string":
		return "String"
	case "bool":
		return "boolean"
	case "float":
		return "double"
	default:
		return "Object"
	}
}

func (c *Compiler) defaultValue(typ string) string {
	switch typ {
	case "String":
		return "\"\""
	case "boolean":
		return "false"
	case "double":
		return "0.0"
	default:
		return "0"
	}
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	typ := c.typeName(v.Type)
	expr, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = "var"
	}
	if v.Value == nil {
		if typ == "var" {
			typ = "int"
		}
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, expr))
	}
	return nil
}

func (c *Compiler) compileLet(v *parser.LetStmt) error {
	typ := c.typeName(v.Type)
	expr, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = "var"
	}
	if v.Value == nil {
		if typ == "var" {
			typ = "int"
		}
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, expr))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	expr, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", a.Name, expr))
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range i.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if len(i.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, s := range i.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd == nil {
		return fmt.Errorf("for range over collections not supported at line %d", f.Pos.Line)
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", f.Name, start, f.Name, end, f.Name))
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range w.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	ret := c.typeName(f.Return)
	var params []string
	for _, p := range f.Params {
		params = append(params, fmt.Sprintf("%s %s", c.typeName(p.Type), p.Name))
	}
	c.writeln(fmt.Sprintf("static %s %s(%s) {", ret, f.Name, strings.Join(params, ", ")))
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val + ";")
	return nil
}

func (c *Compiler) compileExprStmt(e *parser.ExprStmt) error {
	expr, err := c.compileExpr(e.Expr)
	if err != nil {
		return err
	}
	c.writeln(expr + ";")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	expr := left
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		if (op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=") &&
			isString(expr) && isString(right) {
			expr = fmt.Sprintf("%s.compareTo(%s) %s 0", expr, right, op.Op)
		} else {
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, right)
		}
	}
	return expr, nil
}

func isString(s string) bool {
	return len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"'
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	s, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	if len(p.Ops) > 0 {
		return "", fmt.Errorf("postfix operations unsupported")
	}
	return s, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", nil
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		case p.Lit.Float != nil:
			return fmt.Sprintf("%f", *p.Lit.Float), nil
		case p.Lit.Str != nil:
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		case p.Lit.Bool != nil:
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		case p.Lit.Null:
			return "null", nil
		}
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 {
			return "", fmt.Errorf("selectors unsupported at line %d", p.Pos.Line)
		}
		return p.Selector.Root, nil
	case p.Call != nil:
		var args []string
		for _, a := range p.Call.Args {
			arg, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, arg)
		}
		switch p.Call.Func {
		case "print":
			if len(args) != 1 {
				return "", fmt.Errorf("print expects one argument at line %d", p.Pos.Line)
			}
			return fmt.Sprintf("System.out.println(%s)", args[0]), nil
		case "len", "count":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects one argument at line %d", p.Pos.Line)
			}
			return args[0] + ".size()", nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects one argument at line %d", p.Pos.Line)
			}
			return fmt.Sprintf("String.valueOf(%s)", args[0]), nil
		case "append":
			if len(args) != 2 {
				return "", fmt.Errorf("append expects two arguments at line %d", p.Pos.Line)
			}
			c.needsAppend = true
			return fmt.Sprintf("append(%s, %s)", args[0], args[1]), nil
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects one argument at line %d", p.Pos.Line)
			}
			c.needsSum = true
			return fmt.Sprintf("sum(%s)", args[0]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects one argument at line %d", p.Pos.Line)
			}
			c.needsSum = true
			c.needsAvg = true
			return fmt.Sprintf("avg(%s)", args[0]), nil
		case "min":
			if len(args) != 1 {
				return "", fmt.Errorf("min expects one argument at line %d", p.Pos.Line)
			}
			return fmt.Sprintf("java.util.Collections.min(%s)", args[0]), nil
		case "max":
			if len(args) != 1 {
				return "", fmt.Errorf("max expects one argument at line %d", p.Pos.Line)
			}
			return fmt.Sprintf("java.util.Collections.max(%s)", args[0]), nil
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ",")), nil
		}
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.List != nil:
		var elems []string
		for _, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems = append(elems, s)
		}
		return fmt.Sprintf("new java.util.ArrayList<>(java.util.Arrays.asList(%s))", strings.Join(elems, ",")), nil
	case p.Map != nil:
		var parts []string
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%s, %s", k, v))
		}
		return fmt.Sprintf("new java.util.LinkedHashMap<>(java.util.Map.of(%s))", strings.Join(parts, ",")), nil
	}
	return "", fmt.Errorf("expression unsupported at line %d", p.Pos.Line)
}
