package hscode

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Haskell source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	vars    map[string]bool
	imports map[string]bool
}

// New creates a new Haskell compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:     env,
		vars:    map[string]bool{},
		imports: map[string]bool{},
	}
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

// Compile generates Haskell source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	var body bytes.Buffer
	orig := c.buf
	c.buf = body

	// function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// main function
	c.writeln("main :: IO ()")
	c.writeln("main = do")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--

	bodyBytes := c.buf.Bytes()
	c.buf = orig

	// header and imports
	c.writeln("module Main where")
	c.writeln("")
	c.imports["Control.Monad"] = true
	c.imports["Data.IORef"] = true
	if len(c.imports) > 0 {
		imports := make([]string, 0, len(c.imports))
		for imp := range c.imports {
			imports = append(imports, imp)
		}
		sort.Strings(imports)
		for _, imp := range imports {
			c.writeln("import " + imp)
		}
		c.writeln("")
	}

	// whileM_ helper
	c.writeln("whileM_ :: IO Bool -> IO () -> IO ()")
	c.writeln("whileM_ cond body = do")
	c.indent++
	c.writeln("c <- cond")
	c.writeln("when c (body >> whileM_ cond body)")
	c.indent--
	c.writeln("")

	c.buf.Write(bodyBytes)
	return c.buf.Bytes(), nil
}

// --- Statements ---

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
		c.writeln("return (" + expr + ")")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	val := "undefined"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln(fmt.Sprintf("let %s = %s", name, val))
	c.vars[name] = false
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	val := "undefined"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln(fmt.Sprintf("%sRef <- newIORef %s", name, val))
	c.vars[name] = true
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	name := sanitizeName(s.Name)
	val, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	if c.vars[name] {
		c.writeln(fmt.Sprintf("writeIORef %sRef %s", name, val))
	} else {
		c.writeln(fmt.Sprintf("let %s = %s", name, val))
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then do", cond))
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeln("else do")
		c.indent++
		if err := c.compileIf(stmt.ElseIf); err != nil {
			return err
		}
		c.indent--
	} else if len(stmt.Else) > 0 {
		c.writeln("else do")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	} else {
		c.writeln("else return ()")
	}
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("whileM_ (return (%s)) $ do", cond))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
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
		c.writeln(fmt.Sprintf("forM_ [%s .. %s - 1] $ \\%s -> do", start, end, name))
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		return nil
	}
	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("forM_ %s $ \\%s -> do", src, name))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("when (not (%s)) (error \"expect failed\")", expr))
	return nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("%s %s = do\n", name, strings.Join(params, " ")))
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) { return c.compileBinaryExpr(e.Binary) }

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
		expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, r)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("(%s%s)", u.Ops[i], val)
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
			call := &parser.CallExpr{Func: expr, Args: op.Call.Args}
			expr, err = c.compileCallExpr(call)
			if err != nil {
				return "", err
			}
			continue
		}
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if strings.HasPrefix(idx, "\"") {
				c.imports["qualified Data.Map.Strict as M"] = true
				expr = fmt.Sprintf("(M.! %s) %s", idx, expr)
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
		name := sanitizeName(p.Selector.Root)
		for _, f := range p.Selector.Tail {
			name += "." + sanitizeName(f)
		}
		if c.vars[name] {
			return fmt.Sprintf("(readIORef %sRef)", name), nil
		}
		return name, nil
	case p.List != nil:
		return c.compileListLiteral(p.List)
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	default:
		return "", fmt.Errorf("invalid primary expression")
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
	switch call.Func {
	case "print":
		parts := make([]string, len(args))
		for i, a := range args {
			parts[i] = fmt.Sprintf("show %s", a)
		}
		return fmt.Sprintf("putStrLn (unwords [%s])", strings.Join(parts, ", ")), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("length %s", args[0]), nil
	default:
		return fmt.Sprintf("(%s %s)", sanitizeName(call.Func), strings.Join(args, " ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(\\%s -> %s)", strings.Join(params, " "), body), nil
	}
	return "undefined", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileListLiteral(l *parser.ListLiteral) (string, error) {
	elems := make([]string, len(l.Elems))
	for i, e := range l.Elems {
		v, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		elems[i] = v
	}
	return "[" + strings.Join(elems, ", ") + "]", nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
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
	c.imports["qualified Data.Map.Strict as M"] = true
	return "M.fromList [" + strings.Join(items, ", ") + "]", nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	cases := make([]string, len(m.Cases))
	for i, cs := range m.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		cases[i] = fmt.Sprintf("%s -> %s", pat, res)
	}
	return fmt.Sprintf("(case %s of { %s })", target, strings.Join(cases, "; ")), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	switch g.Target {
	case "text":
		return "\"\"", nil
	case "embedding":
		return "[0,0]", nil
	default:
		return "undefined", nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "True", nil
		}
		return "False", nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	default:
		return "", fmt.Errorf("invalid literal")
	}
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
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	return b.String()
}
