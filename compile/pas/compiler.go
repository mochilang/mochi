package pascode

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Pascal source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	tempVarCount int
}

// New creates a new Pascal compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile returns Pascal source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("program main;")
	c.writeln("{$mode objfpc}")
	c.writeln("uses SysUtils;")
	c.writeln("")
	c.writeln("type TIntArray = array of integer;")
	c.writeln("")

	// Emit function declarations first.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Collect global vars.
	vars := map[string]string{}
	collectVars(prog.Statements, c.env, vars)
	if len(vars) > 0 {
		c.writeln("var")
		c.indent++
		names := make([]string, 0, len(vars))
		for n := range vars {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(n), vars[n]))
		}
		c.indent--
		c.writeln("")
	}

	c.writeln("begin")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("end.")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s;", sanitizeName(s.Let.Name), val))
	case s.Var != nil:
		if s.Var.Value != nil {
			val, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s := %s;", sanitizeName(s.Var.Name), val))
		}
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("result := %s;", val))
		c.writeln("exit;")
	case s.Assign != nil:
		name := sanitizeName(s.Assign.Name)
		for _, idx := range s.Assign.Index {
			iv, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			name = fmt.Sprintf("%s[%s]", name, iv)
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s;", name, val))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s := %s to %s - 1 do", name, start, end))
		c.writeln("begin")
		c.indent++
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("end;")
		return nil
	}

	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	idx := c.newVar()
	c.writeln(fmt.Sprintf("for %s := 0 to Length(%s) - 1 do", idx, src))
	c.writeln("begin")
	c.indent++
	if f.Name != "_" {
		c.writeln(fmt.Sprintf("%s := %s[%s];", name, src, idx))
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end;")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " do")
	c.writeln("begin")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end;")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond + " then")
	c.writeln("begin")
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	cur := stmt
	for cur.ElseIf != nil {
		c.writeln("end else if " + c.mustExpr(cur.ElseIf.Cond) + " then")
		c.writeln("begin")
		c.indent++
		for _, st := range cur.ElseIf.Then {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		cur = cur.ElseIf
	}
	if len(cur.Else) > 0 {
		c.writeln("end else")
		c.writeln("begin")
		c.indent++
		for _, st := range cur.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("end;")
	return nil
}

func (c *Compiler) mustExpr(e *parser.Expr) string {
	s, _ := c.compileExpr(e)
	return s
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		pt := c.typeRef(p.Type)
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), pt)
	}
	retType := c.typeRef(fun.Return)
	c.writeln(fmt.Sprintf("function %s(%s): %s;", name, strings.Join(params, "; "), retType))

	vars := map[string]string{}
	collectVars(fun.Body, c.env, vars)
	if len(vars) > 0 {
		c.writeln("var")
		c.indent++
		names := make([]string, 0, len(vars))
		for n := range vars {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(n), vars[n]))
		}
		c.indent--
	}
	c.writeln("begin")
	c.indent++
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end;")
	return nil
}

func (c *Compiler) typeRef(t *parser.TypeRef) string {
	if t == nil {
		return "integer"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "integer"
		case "string":
			return "string"
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" {
			return "TIntArray"
		}
	}
	return "integer"
}

func typeString(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		_ = tt
		return "integer"
	case types.StringType:
		return "string"
	case types.ListType:
		return "TIntArray"
	default:
		return "integer"
	}
}

func collectVars(stmts []*parser.Statement, env *types.Env, vars map[string]string) {
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			typ := "integer"
			if env != nil {
				if t, err := env.GetVar(s.Let.Name); err == nil {
					typ = typeString(t)
				}
			}
			vars[s.Let.Name] = typ
		case s.Var != nil:
			typ := "integer"
			if env != nil {
				if t, err := env.GetVar(s.Var.Name); err == nil {
					typ = typeString(t)
				}
			}
			vars[s.Var.Name] = typ
		case s.For != nil:
			if s.For.Name != "_" {
				vars[s.For.Name] = "integer"
			}
			collectVars(s.For.Body, env, vars)
		case s.If != nil:
			collectVars(s.If.Then, env, vars)
			if s.If.ElseIf != nil {
				collectVars([]*parser.Statement{{If: s.If.ElseIf}}, env, vars)
			}
			collectVars(s.If.Else, env, vars)
		case s.Fun != nil:
			// ignore nested functions
		}
	}
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "<", "<=", ">", ">=":
			expr = fmt.Sprintf("%s %s %s", expr, op.Op, r)
		case "%":
			expr = fmt.Sprintf("%s mod %s", expr, r)
		case "==":
			expr = fmt.Sprintf("%s = %s", expr, r)
		case "!=":
			expr = fmt.Sprintf("%s <> %s", expr, r)
		default:
			return "", fmt.Errorf("unsupported op %s", op.Op)
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
		switch u.Ops[i] {
		case "-":
			expr = "-" + expr
		case "!":
			expr = "not " + expr
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
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
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
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
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		}
	case p.Selector != nil:
		return sanitizeName(p.Selector.Root), nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		argStr := strings.Join(args, ", ")
		switch p.Call.Func {
		case "len":
			return fmt.Sprintf("Length(%s)", argStr), nil
		case "print":
			return fmt.Sprintf("writeln(%s)", argStr), nil
		default:
			return fmt.Sprintf("%s(%s)", sanitizeName(p.Call.Func), argStr), nil
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
	}
	return "", fmt.Errorf("unsupported expression")
}
