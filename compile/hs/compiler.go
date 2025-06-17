package hscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates Mochi AST to Haskell source code for a limited subset.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

// Compile generates Haskell code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("module Main where")
	c.writeln("")
	c.writeln("import Data.Maybe (fromMaybe)")
	c.writeln("")
	c.writeln(runtime)
	c.writeln("")

	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("main :: IO ()")
	c.writeln("main = do")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun == nil && s.Type == nil && s.Test == nil {
			if err := c.compileMainStmt(s); err != nil {
				return nil, err
			}
		}
	}
	c.indent--

	return c.buf.Bytes(), nil
}

func (c *Compiler) compileMainStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let %s = %s", sanitizeName(s.Let.Name), val))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	default:
		return fmt.Errorf("unsupported statement in main")
	}
	return nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(name + " " + strings.Join(params, " ") + " = fromMaybe (" + c.defaultReturn(fun.Body) + ") $")
	bodyStmts := fun.Body
	if len(bodyStmts) > 0 {
		if bodyStmts[len(bodyStmts)-1].Return != nil {
			bodyStmts = bodyStmts[:len(bodyStmts)-1]
		}
	}
	c.indent++
	expr, err := c.compileStmtExpr(bodyStmts)
	if err != nil {
		return err
	}
	c.writeln(expr)
	c.indent--
	// Collect let statements for where clause
	lets := c.collectLets(fun.Body)
	if len(lets) > 0 {
		c.writeln("  where")
		c.indent++
		for _, l := range lets {
			c.writeln(l)
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) defaultReturn(stmts []*parser.Statement) string {
	if len(stmts) == 0 {
		return "()"
	}
	if ret := stmts[len(stmts)-1].Return; ret != nil {
		v, _ := c.compileExpr(ret.Value)
		return v
	}
	return "()"
}

func (c *Compiler) collectLets(stmts []*parser.Statement) []string {
	var res []string
	for _, s := range stmts {
		if s.Let != nil {
			v, _ := c.compileExpr(s.Let.Value)
			res = append(res, fmt.Sprintf("%s = %s", sanitizeName(s.Let.Name), v))
		} else {
			break
		}
	}
	return res
}

// compileStmtExpr compiles statements to a Maybe-returning expression.
func (c *Compiler) compileStmtExpr(stmts []*parser.Statement) (string, error) {
	if len(stmts) == 0 {
		return "Nothing", nil
	}
	expr := "Nothing"
	for i := len(stmts) - 1; i >= 0; i-- {
		s := stmts[i]
		switch {
		case s.Return != nil:
			val, err := c.compileExpr(s.Return.Value)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("Just (%s)", val)
		case s.If != nil:
			thenExpr, err := c.compileStmtExpr(s.If.Then)
			if err != nil {
				return "", err
			}
			elseExpr, err := c.compileStmtExpr(s.If.Else)
			if err != nil {
				return "", err
			}
			cond, err := c.compileExpr(s.If.Cond)
			if err != nil {
				return "", err
			}
			e := fmt.Sprintf("if %s then %s else %s", cond, thenExpr, elseExpr)
			expr = chainMaybe(e, expr)
		case s.For != nil:
			bodyExpr, err := c.compileStmtExpr(s.For.Body)
			if err != nil {
				return "", err
			}
			start, err := c.compileExpr(s.For.Source)
			if err != nil {
				return "", err
			}
			end, err := c.compileExpr(s.For.RangeEnd)
			if err != nil {
				return "", err
			}
			loop := fmt.Sprintf("forLoop %s %s (\\%s -> %s)", start, end, sanitizeName(s.For.Name), bodyExpr)
			expr = chainMaybe(loop, expr)
		case s.Expr != nil:
			val, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return "", err
			}
			expr = chainMaybe(fmt.Sprintf("(let _ = %s in Nothing)", val), expr)
		}
	}
	return expr, nil
}

func chainMaybe(a, b string) string {
	if b == "Nothing" {
		return a
	}
	return fmt.Sprintf("case %s of Just v -> Just v; Nothing -> %s", a, b)
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
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, r)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			expr = fmt.Sprintf("(-%s)", expr)
		case "!":
			expr = fmt.Sprintf("not %s", expr)
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
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s %s", expr, strings.Join(args, " "))
		} else if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("(%s !! %s)", expr, idx)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("[%s]", strings.Join(elems, ", ")), nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		if p.Call.Func == "len" {
			return fmt.Sprintf("length %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "print" {
			joined := strings.Join(args, " ")
			if len(args) != 1 || strings.ContainsAny(joined, " ") {
				return fmt.Sprintf("print (%s)", joined), nil
			}
			return fmt.Sprintf("print %s", joined), nil
		}
		return fmt.Sprintf("%s %s", sanitizeName(p.Call.Func), strings.Join(args, " ")), nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			parts := []string{name}
			for _, t := range p.Selector.Tail {
				parts = append(parts, sanitizeName(t))
			}
			name = strings.Join(parts, ".")
		}
		return name, nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", inner), nil
	default:
		return "0", nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "True", nil
		}
		return "False", nil
	}
	return "0", nil
}
