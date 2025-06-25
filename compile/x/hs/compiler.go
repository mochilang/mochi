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
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	structs      map[string]bool
	usesMap      bool
	usesList     bool
	usesTime     bool
	usesJSON     bool
	usesLoad     bool
	usesSave     bool
	usesSlice    bool
	usesSliceStr bool
	usesExpect   bool
}

func (c *Compiler) hsType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Bool"
	case types.ListType:
		return "[" + c.hsType(tt.Elem) + "]"
	case types.MapType:
		c.usesMap = true
		return "Map.Map " + c.hsType(tt.Key) + " " + c.hsType(tt.Value)
	case types.VoidType:
		return "()"
	case types.FuncType:
		parts := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			parts[i] = c.hsType(p)
		}
		ret := c.hsType(tt.Return)
		s := strings.Join(parts, " -> ")
		if s != "" {
			s += " -> " + ret
		} else {
			s = ret
		}
		return "(" + s + ")"
	case types.StructType:
		return tt.Name
	default:
		return "()"
	}
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, structs: make(map[string]bool)}
}

// Compile generates Haskell code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.structs = make(map[string]bool)
	c.usesMap = true
	c.usesList = false
	c.usesJSON = false
	c.usesLoad = false
	c.usesSave = false
	c.usesSlice = false
	c.usesSliceStr = false
	c.usesExpect = false

	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("main :: IO ()")
	c.writeln("main = do")
	c.indent++
	mainStmts := 0
	testCount := 0
	for _, s := range prog.Statements {
		if s.Fun == nil && s.Type == nil && s.Test == nil {
			if err := c.compileMainStmt(s); err != nil {
				return nil, err
			}
			mainStmts++
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(name)
			testCount++
		}
	}
	if mainStmts == 0 && testCount == 0 {
		c.writeln("return ()")
	}
	c.indent--

	var header bytes.Buffer
	header.WriteString("module Main where\n\n")
	header.WriteString("import Data.Maybe (fromMaybe)\n")
	header.WriteString("import Data.Time.Clock.POSIX (getPOSIXTime)\n")
	header.WriteString("import qualified Data.Map as Map\n")
	header.WriteString("import Data.List (intercalate)\n")
	header.WriteString("import qualified Data.List as List\n")
	header.WriteString("import qualified Data.Aeson as Aeson\n")
	header.WriteString("import qualified Data.ByteString.Lazy.Char8 as BSL\n")
	header.WriteString("\n")
	header.WriteString(runtime)
	if c.usesExpect {
		header.WriteString(expectHelper)
	}
	if c.usesSlice || c.usesSliceStr {
		header.WriteString(sliceHelpers)
	}
	header.WriteString("\n\n")

	code := append(header.Bytes(), c.buf.Bytes()...)
	// Ensure the generated file ends with a trailing newline so tools like
	// runhaskell do not complain about the last line.
	if len(code) == 0 || code[len(code)-1] != '\n' {
		code = append(code, '\n')
	}
	return code, nil
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
	case s.Var != nil:
		val := "()"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			val = v
		}
		c.writeln(fmt.Sprintf("let %s = %s", sanitizeName(s.Var.Name), val))
	case s.Fun != nil:
		expr, err := c.compileFunExpr(&parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body})
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let %s = %s", sanitizeName(s.Fun.Name), expr))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.For != nil:
		orig := c.env
		var vType types.Type = types.AnyType{}
		if s.For.RangeEnd != nil {
			vType = types.IntType{}
		} else {
			t := c.inferExprType(s.For.Source)
			switch tt := t.(type) {
			case types.ListType:
				vType = tt.Elem
			case types.MapType:
				vType = tt.Key
			}
		}
		child := types.NewEnv(c.env)
		child.SetVar(s.For.Name, vType, true)
		c.env = child
		body, err := c.simpleBodyExpr(s.For.Body)
		if err != nil {
			c.env = orig
			return err
		}
		name := sanitizeName(s.For.Name)
		src, err := c.compileExpr(s.For.Source)
		if err != nil {
			c.env = orig
			return err
		}
		if s.For.RangeEnd != nil {
			end, err := c.compileExpr(s.For.RangeEnd)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln(fmt.Sprintf("mapM_ (\\%s -> %s) [%s..%s - 1]", name, body, src, end))
		} else {
			iter := src
			if _, ok := c.inferExprType(s.For.Source).(types.MapType); ok {
				c.usesMap = true
				iter = fmt.Sprintf("(Map.keys %s)", src)
			}
			c.writeln(fmt.Sprintf("mapM_ (\\%s -> %s) %s", name, body, iter))
		}
		c.env = orig
	case s.While != nil:
		body, err := c.simpleBodyExpr(s.While.Body)
		if err != nil {
			return err
		}
		cond, err := c.compileExpr(s.While.Cond)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let _ = whileLoop (\\() -> %s) (\\() -> Nothing <$ (%s)) in return ()", cond, body))
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.usesExpect = true
		c.writeln(fmt.Sprintf("expect (%s)", expr))
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
	paramTypes := make([]string, len(fun.Params))
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
	if _, ok := ft.Return.(types.VoidType); ok {
		inf := c.inferFuncReturn(fun.Body)
		if _, ok := inf.(types.VoidType); !ok {
			ft.Return = inf
		}
	}
	if _, ok := ft.Return.(types.AnyType); ok {
		inf := c.inferFuncReturn(fun.Body)
		if _, ok := inf.(types.AnyType); !ok {
			ft.Return = inf
		}
	}
	if len(paramTypes) == len(ft.Params) {
		for i := range paramTypes {
			paramTypes[i] = c.hsType(ft.Params[i])
		}
	}
	retType := c.hsType(ft.Return)
	if len(paramTypes) > 0 {
		c.writeln(fmt.Sprintf("%s :: %s -> %s", name, strings.Join(paramTypes, " -> "), retType))
	} else {
		c.writeln(fmt.Sprintf("%s :: %s", name, retType))
	}

	if len(fun.Body) == 1 && fun.Body[0].Return != nil {
		val, err := c.compileExpr(fun.Body[0].Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s %s = %s", name, strings.Join(params, " "), val))
	} else {
		c.writeln(name + " " + strings.Join(params, " ") + " = fromMaybe (" + c.defaultReturn(fun.Body, ft.Return) + ") $")
		bodyStmts := fun.Body
		c.indent++
		expr, err := c.compileStmtExpr(bodyStmts, true)
		if err != nil {
			return err
		}
		c.writeln(expr)
		c.indent--
	}
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
	if len(stmts) == 1 && stmts[0].Return != nil {
		v, err := c.compileExpr(stmts[0].Return.Value)
		if err == nil {
			return v
		}
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

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("%s :: IO ()", name))
	c.writeln(name + " = do")
	c.indent++
	orig := c.env
	for _, st := range t.Body {
		if err := c.compileMainStmt(st); err != nil {
			c.env = orig
			return err
		}
	}
	c.env = orig
	c.indent--
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	v, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.usesExpect = true
	c.writeln(fmt.Sprintf("expect (%s)", v))
	return nil
}

func (c *Compiler) inferFuncReturn(body []*parser.Statement) types.Type {
	for _, s := range body {
		if s.Return != nil {
			return c.inferExprType(s.Return.Value)
		}
	}
	return types.VoidType{}
}

// compileStmtExpr compiles statements to a Maybe-returning expression.
func (c *Compiler) compileStmtExpr(stmts []*parser.Statement, top bool) (string, error) {
	if len(stmts) == 0 {
		return "Nothing", nil
	}
	if top && len(stmts) == 1 && stmts[0].Return != nil {
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
			bodyExpr, err := c.compileStmtExpr(s.For.Body, false)
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
				listExpr := src
				if _, ok := c.inferExprType(s.For.Source).(types.MapType); ok {
					c.usesMap = true
					listExpr = fmt.Sprintf("Map.keys %s", src)
				}
				loop := fmt.Sprintf("foldr (\\%s acc -> case %s of Just v -> Just v; Nothing -> acc) Nothing %s", name, bodyExpr, listExpr)
				expr = chainMaybe(loop, expr)
			}
		case s.Var != nil:
			val := "()"
			if s.Var.Value != nil {
				v, err := c.compileExpr(s.Var.Value)
				if err != nil {
					return "", err
				}
				val = v
			}
			expr = fmt.Sprintf("(let %s = %s in %s)", sanitizeName(s.Var.Name), val, expr)
		case s.Break != nil:
			expr = "Just ()"
			continue
		case s.Continue != nil:
			expr = "Nothing"
			continue
		case s.Let != nil:
			val := "()"
			if s.Let.Value != nil {
				v, err := c.compileExpr(s.Let.Value)
				if err != nil {
					return "", err
				}
				val = v
			}
			expr = fmt.Sprintf("(let %s = %s in %s)", sanitizeName(s.Let.Name), val, expr)
		case s.Fun != nil:
			val, err := c.compileFunExpr(&parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body})
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("(let %s = %s in %s)", sanitizeName(s.Fun.Name), val, expr)
		case s.Assign != nil:
			val, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("(let %s = %s in %s)", sanitizeName(s.Assign.Name), val, expr)
		case s.While != nil:
			bodyExpr, err := c.compileStmtExpr(s.While.Body, false)
			if err != nil {
				return "", err
			}
			cond, err := c.compileExpr(s.While.Cond)
			if err != nil {
				return "", err
			}
			loop := fmt.Sprintf("whileLoop (\\() -> %s) (\\() -> %s)", cond, bodyExpr)
			expr = chainMaybe(loop, expr)
			continue
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
	thenExpr, err := c.compileStmtExpr(stmt.Then, false)
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
		elseExpr, err = c.compileStmtExpr(stmt.Else, false)
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
	leftType := c.inferUnaryType(b.Left)
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightType := c.inferPostfixType(op.Right)
		if op.Op == "%" {
			expr = fmt.Sprintf("(%s `mod` %s)", expr, r)
			leftType = types.IntType{}
		} else if op.Op == "/" && c.isIntPostfix(op.Right) {
			expr = fmt.Sprintf("(div %s %s)", expr, r)
			leftType = types.IntType{}
		} else if op.Op == "in" {
			if c.isMapPostfix(op.Right) {
				c.usesMap = true
				expr = fmt.Sprintf("Map.member %s %s", expr, r)
			} else {
				expr = fmt.Sprintf("elem %s %s", expr, r)
			}
			leftType = types.BoolType{}
		} else if op.Op == "union" && op.All {
			c.usesList = true
			expr = fmt.Sprintf("(%s ++ %s)", expr, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		} else if op.Op == "union" {
			c.usesList = true
			expr = fmt.Sprintf("List.union %s %s", expr, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		} else if op.Op == "except" {
			c.usesList = true
			expr = fmt.Sprintf("(%s List.\\ %s)", expr, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		} else if op.Op == "intersect" {
			c.usesList = true
			expr = fmt.Sprintf("List.intersect %s %s", expr, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		} else if op.Op == "+" && isString(leftType) && isString(rightType) {
			expr = fmt.Sprintf("(%s ++ %s)", expr, r)
			leftType = types.StringType{}
		} else {
			opSym := op.Op
			if opSym == "!=" {
				opSym = "/="
			}
			expr = fmt.Sprintf("(%s %s %s)", expr, opSym, r)
			switch opSym {
			case "+", "-", "*", "/":
				if isInt(leftType) && isInt(rightType) {
					leftType = types.IntType{}
				} else if isFloat(leftType) && isFloat(rightType) {
					leftType = types.FloatType{}
				} else {
					leftType = types.AnyType{}
				}
			case "==", "!=", "<", "<=", ">", ">=":
				leftType = types.BoolType{}
			}
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
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("length %s", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if c.isStringPrimary(p.Target) {
					c.usesSliceStr = true
					expr = fmt.Sprintf("_sliceString %s %s %s", expr, start, end)
				} else {
					c.usesSlice = true
					expr = fmt.Sprintf("_slice %s %s %s", expr, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isMapPrimary(p.Target) {
					c.usesMap = true
					expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %s %s)", idx, expr)
				} else if c.isStringPrimary(p.Target) {
					expr = fmt.Sprintf("_indexString %s %s", expr, idx)
				} else {
					expr = fmt.Sprintf("(%s !! %s)", expr, idx)
				}
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
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
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
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
			if len(args) == 1 {
				if _, ok := c.inferExprType(p.Call.Args[0]).(types.GroupType); ok {
					return fmt.Sprintf("length (items %s)", args[0]), nil
				}
			}
			return fmt.Sprintf("length %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "avg" {
			return fmt.Sprintf("avg %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "str" {
			return fmt.Sprintf("show %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "push" && len(args) == 2 {
			return fmt.Sprintf("(%s ++ [%s])", args[0], args[1]), nil
		}
		if p.Call.Func == "keys" && len(args) == 1 {
			c.usesMap = true
			return fmt.Sprintf("(Map.keys %s)", args[0]), nil
		}
		if p.Call.Func == "now" {
			c.usesTime = true
			return "_now", nil
		}
		if p.Call.Func == "json" {
			c.usesJSON = true
			return fmt.Sprintf("_json %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "input" {
			return "_input", nil
		}
		if p.Call.Func == "print" {
			return c.compilePrint(callArgs{args: args, exprs: p.Call.Args})
		}
		return fmt.Sprintf("%s %s", sanitizeName(p.Call.Func), strings.Join(args, " ")), nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			expr = fmt.Sprintf("%s (%s)", sanitizeName(t), expr)
		}
		return expr, nil
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s { %s }", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
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
		if strings.HasPrefix(arg, "\"") || strings.HasPrefix(arg, "show ") || strings.HasPrefix(arg, "show(") || strings.HasPrefix(arg, "_indexString") || c.isStringExpr(ca.exprs[0]) {
			return fmt.Sprintf("putStrLn (%s)", arg), nil
		}
		return fmt.Sprintf("print (%s)", arg), nil
	}
	parts := make([]string, len(ca.args))
	for i, a := range ca.args {
		if strings.HasPrefix(a, "\"") || strings.HasPrefix(a, "_indexString") || c.isStringExpr(ca.exprs[i]) {
			parts[i] = a
		} else {
			parts[i] = fmt.Sprintf("show %s", a)
		}
	}
	return fmt.Sprintf("putStrLn (unwords [%s])", strings.Join(parts, ", ")), nil
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
	if len(fn.BlockBody) == 0 {
		return fmt.Sprintf("(\\%s -> ())", strings.Join(params, " ")), nil
	}
	expr, err := c.compileStmtExpr(fn.BlockBody, true)
	if err != nil {
		return "", err
	}
	ret := c.defaultReturn(fn.BlockBody, types.VoidType{})
	return fmt.Sprintf("(\\%s -> fromMaybe (%s) $ %s)", strings.Join(params, " "), ret, expr), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "Nothing"
	if l.Path != nil {
		path = fmt.Sprintf("Just %q", *l.Path)
	}
	opts := "Nothing"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Just (%s)", v)
	}
	c.usesLoad = true
	c.usesMap = true
	return fmt.Sprintf("_load %s %s", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "Nothing"
	if s.Path != nil {
		path = fmt.Sprintf("Just %q", *s.Path)
	}
	opts := "Nothing"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Just (%s)", v)
	}
	c.usesSave = true
	c.usesMap = true
	return fmt.Sprintf("_save %s %s %s", src, path, opts), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	orig := c.env
	child := types.NewEnv(c.env)
	var elemType types.Type = types.AnyType{}
	if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
		elemType = lt.Elem
	}
	child.SetVar(q.Var, elemType, true)

	loops := []string{}
	conds := []string{}

	pushDown := q.Where != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil
	var whereExpr string
	if pushDown {
		c.env = child
		w, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
		whereExpr = w
		c.env = orig
	}

	if pushDown {
		loops = append(loops, fmt.Sprintf("%s <- filter (\\%s -> %s) %s", sanitizeName(q.Var), sanitizeName(q.Var), whereExpr, src))
	} else {
		loops = append(loops, fmt.Sprintf("%s <- %s", sanitizeName(q.Var), src))
	}

	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		loops = append(loops, fmt.Sprintf("%s <- %s", sanitizeName(f.Var), fs))
		ft := c.inferExprType(f.Src)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}

	for _, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		loops = append(loops, fmt.Sprintf("%s <- %s", sanitizeName(j.Var), js))
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		conds = append(conds, on)
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
	}

	c.env = child
	if q.Where != nil && !pushDown {
		w, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
		conds = append(conds, w)
	}

	condStr := ""
	if len(conds) > 0 {
		condStr = ", " + strings.Join(conds, ", ")
	}

	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: elemType}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig
		c.usesMap = true
		c.usesList = true
		expr := fmt.Sprintf("[ %s | g <- _group_by %s (\\%s -> %s), let %s = g ]", valExpr, src, sanitizeName(q.Var), keyExpr, sanitizeName(q.Group.Name))
		return expr, nil
	}

	if q.Group != nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		tuple := fmt.Sprintf("(%s)", strings.Join(func() []string {
			names := []string{sanitizeName(q.Var)}
			for _, f := range q.Froms {
				names = append(names, sanitizeName(f.Var))
			}
			for _, j := range q.Joins {
				names = append(names, sanitizeName(j.Var))
			}
			return names
		}(), ", "))
		rows := fmt.Sprintf("[%s | %s%s]", tuple, strings.Join(loops, ", "), condStr)
		groups := fmt.Sprintf("_group_by %s (\\%s -> %s)", rows, tuple, keyExpr)
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		sortExpr := ""
		if q.Sort != nil {
			sortExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		c.env = orig
		res := fmt.Sprintf("[ %s | g <- %s, let %s = g ]", valExpr, groups, sanitizeName(q.Group.Name))
		if q.Sort != nil {
			res = fmt.Sprintf("map snd (List.sortOn fst [ ( %s, %s ) | g <- %s ])", sortExpr, valExpr, groups)
			c.usesList = true
		}
		if q.Skip != nil {
			sk, err := c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
			res = fmt.Sprintf("drop %s %s", sk, res)
			c.usesList = true
		}
		if q.Take != nil {
			tk, err := c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			res = fmt.Sprintf("take %s %s", tk, res)
			c.usesList = true
		}
		c.usesMap = true
		c.usesList = true
		return res, nil
	}

	val, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	c.env = orig
	comp := fmt.Sprintf("[%s | %s%s]", val, strings.Join(loops, ", "), condStr)
	res := comp

	if q.Sort != nil {
		sortExpr, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("map snd (List.sortOn fst [ ( %s, %s ) | %s%s ])", sortExpr, val, strings.Join(loops, ", "), condStr)
		c.usesList = true
	}
	if q.Skip != nil {
		sk, err := c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("drop %s %s", sk, res)
		c.usesList = true
	}
	if q.Take != nil {
		tk, err := c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("take %s %s", tk, res)
		c.usesList = true
	}
	return res, nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if c.env == nil {
		return nil
	}
	if len(t.Variants) > 0 {
		return nil
	}
	if st, ok := c.env.GetStruct(t.Name); ok {
		c.compileStructType(st)
	}
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("data %s = %s {", name, name))
	c.indent++
	for i, fn := range st.Order {
		ft := st.Fields[fn]
		line := fmt.Sprintf("%s :: %s", sanitizeName(fn), c.hsType(ft))
		if i < len(st.Order)-1 {
			line += ","
		}
		c.writeln(line)
	}
	c.indent--
	c.writeln("} deriving (Show)")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
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
