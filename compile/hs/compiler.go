package hscode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates Mochi AST to Haskell source code for a limited subset.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	usesMap bool
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

// Compile generates Haskell code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.usesMap = false

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

	var header bytes.Buffer
	header.WriteString("module Main where\n\n")
	header.WriteString("import Data.Maybe (fromMaybe)\n")
	if c.usesMap {
		header.WriteString("import qualified Data.Map as Map\n")
	}
	header.WriteString("\n")
	header.WriteString(runtime)
	header.WriteString("\n\n")

	return append(header.Bytes(), c.buf.Bytes()...), nil
}

func (c *Compiler) compileMainStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		if isInputCall(s.Let.Value) {
			val, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s <- %s", sanitizeName(s.Let.Name), val))
		} else {
			val, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let %s = %s", sanitizeName(s.Let.Name), val))
		}
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.For != nil:
		body, err := c.simpleBodyExpr(s.For.Body)
		if err != nil {
			return err
		}
		name := sanitizeName(s.For.Name)
		src, err := c.compileExpr(s.For.Source)
		if err != nil {
			return err
		}
		if s.For.RangeEnd != nil {
			end, err := c.compileExpr(s.For.RangeEnd)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let _ = forLoop %s %s (\\%s -> Nothing <$ (%s)) in return ()", src, end, name, body))
		} else {
			c.writeln(fmt.Sprintf("mapM_ (\\%s -> %s) %s", name, body, src))
		}
	default:
		return fmt.Errorf("unsupported statement in main")
	}
	return nil
}

func (c *Compiler) simpleBodyExpr(stmts []*parser.Statement) (string, error) {
	if len(stmts) != 1 {
		return "", fmt.Errorf("unsupported loop body")
	}
	s := stmts[0]
	if s.Expr != nil {
		return c.compileExpr(s.Expr.Expr)
	}
	return "", fmt.Errorf("unsupported loop body")
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = sanitizeName(p.Name)
	}
	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	if ft.Return == nil && fun.Return != nil {
		ft.Return = types.AnyType{}
	}
	if ft.Return == nil {
		ft.Return = types.VoidType{}
	}
	c.writeln(name + " " + strings.Join(params, " ") + " = fromMaybe (" + c.defaultReturn(fun.Body, ft.Return) + ") $")
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

func zeroValue(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return "0"
	case types.FloatType:
		return "0.0"
	case types.StringType:
		return "\"\""
	case types.BoolType:
		return "False"
	case types.ListType:
		return "[]"
	default:
		return "()"
	}
}

func (c *Compiler) defaultReturn(stmts []*parser.Statement, retType types.Type) string {
	if len(stmts) == 0 {
		return zeroValue(retType)
	}
	if ret := stmts[len(stmts)-1].Return; ret != nil {
		v, _ := c.compileExpr(ret.Value)
		return v
	}
	return zeroValue(retType)
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
			ifExpr, err := c.compileIfExpr(s.If)
			if err != nil {
				return "", err
			}
			expr = chainMaybe(ifExpr, expr)
		case s.For != nil:
			bodyExpr, err := c.compileStmtExpr(s.For.Body)
			if err != nil {
				return "", err
			}
			name := sanitizeName(s.For.Name)
			src, err := c.compileExpr(s.For.Source)
			if err != nil {
				return "", err
			}
			if s.For.RangeEnd != nil {
				end, err := c.compileExpr(s.For.RangeEnd)
				if err != nil {
					return "", err
				}
				loop := fmt.Sprintf("forLoop %s %s (\\%s -> %s)", src, end, name, bodyExpr)
				expr = chainMaybe(loop, expr)
			} else {
				loop := fmt.Sprintf("foldr (\\%s acc -> case %s of Just v -> Just v; Nothing -> acc) Nothing %s", name, bodyExpr, src)
				expr = chainMaybe(loop, expr)
			}
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

func (c *Compiler) compileIfExpr(stmt *parser.IfStmt) (string, error) {
	thenExpr, err := c.compileStmtExpr(stmt.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if stmt.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(stmt.ElseIf)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr, err = c.compileStmtExpr(stmt.Else)
		if err != nil {
			return "", err
		}
	}
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("if %s then %s else %s", cond, thenExpr, elseExpr), nil
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
			if c.isMapExpr(p.Target) {
				c.usesMap = true
				expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %s %s)", idx, expr)
			} else if c.isStringExpr(p.Target) {
				expr = fmt.Sprintf("_indexString %s %s", expr, idx)
			} else {
				expr = fmt.Sprintf("(%s !! %s)", expr, idx)
			}
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
	case p.Map != nil:
		c.usesMap = true
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
			items[i] = fmt.Sprintf("(%s, %s)", k, v)
		}
		return fmt.Sprintf("Map.fromList [%s]", strings.Join(items, ", ")), nil
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
		if p.Call.Func == "count" {
			return fmt.Sprintf("length %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "avg" {
			return fmt.Sprintf("avg %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "str" {
			return fmt.Sprintf("show %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "input" {
			return "_input", nil
		}
		if p.Call.Func == "print" {
			return c.compilePrint(callArgs{args: args, exprs: p.Call.Args})
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
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return s, nil
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

type callArgs struct {
	args  []string
	exprs []*parser.Expr
}

func (c *Compiler) compilePrint(ca callArgs) (string, error) {
	if len(ca.args) == 1 {
		arg := ca.args[0]
		if strings.HasPrefix(arg, "\"") || strings.HasPrefix(arg, "show ") || strings.HasPrefix(arg, "show(") || strings.HasPrefix(arg, "_indexString") || c.exprIsString(ca.exprs[0]) {
			return fmt.Sprintf("putStrLn (%s)", arg), nil
		}
		return fmt.Sprintf("print (%s)", arg), nil
	}
	parts := make([]string, len(ca.args))
	for i, a := range ca.args {
		if strings.HasPrefix(a, "\"") || strings.HasPrefix(a, "_indexString") || c.exprIsString(ca.exprs[i]) {
			parts[i] = a
		} else {
			parts[i] = fmt.Sprintf("show %s", a)
		}
	}
	return fmt.Sprintf("putStrLn (unwords [%s])", strings.Join(parts, ", ")), nil
}

func (c *Compiler) exprIsString(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isMapExpr(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isStringExpr(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func isInputCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil || p.Call == nil {
		return false
	}
	return p.Call.Func == "input" && len(p.Call.Args) == 0
}
