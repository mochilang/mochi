//go:build slow

package hscode

import (
	"bytes"
	"fmt"
	"sort"
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
	usesFetch    bool
	tmpCount     int
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
		c.usesMap = false
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
	return &Compiler{env: env, structs: make(map[string]bool), tmpCount: 0}
}

// Compile generates Haskell code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.structs = make(map[string]bool)
	c.usesMap = false
	c.usesList = false
	c.usesJSON = false
	c.usesLoad = false
	c.usesSave = false
	c.usesSlice = false
	c.usesSliceStr = false
	c.usesExpect = false
	c.usesFetch = false
	c.tmpCount = 0

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
		if s.Fun == nil && s.Type == nil && s.Test == nil {
			if s.Let != nil {
				if s.Let.Value == nil && s.Let.Type != nil {
					typ := c.hsType(c.resolveTypeRef(s.Let.Type))
					name := sanitizeName(s.Let.Name)
					c.writeln(fmt.Sprintf("%s :: Maybe %s", name, typ))
					c.writeln(fmt.Sprintf("%s = Nothing", name))
				} else {
					var val string
					if s.Let.Value != nil {
						v, err := c.compileExpr(s.Let.Value)
						if err != nil {
							return nil, err
						}
						val = v
					} else {
						val = "()"
					}
					c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Let.Name), val))
				}
				c.writeln("")
			} else if s.Var != nil {
				if s.Var.Value == nil && s.Var.Type != nil {
					typ := c.hsType(c.resolveTypeRef(s.Var.Type))
					name := sanitizeName(s.Var.Name)
					c.writeln(fmt.Sprintf("%s :: Maybe %s", name, typ))
					c.writeln(fmt.Sprintf("%s = Nothing", name))
				} else {
					val := "()"
					if s.Var.Value != nil {
						v, err := c.compileExpr(s.Var.Value)
						if err != nil {
							return nil, err
						}
						val = v
					}
					c.writeln(fmt.Sprintf("%s = %s", sanitizeName(s.Var.Name), val))
				}
				c.writeln("")
			}
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
		if s.Fun == nil && s.Type == nil && s.Test == nil && s.Let == nil && s.Var == nil {
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
	header.WriteString("{-# LANGUAGE DeriveGeneric #-}\n")
	header.WriteString("module Main where\n\n")
	header.WriteString("import Data.Maybe (fromMaybe)\n")
	header.WriteString("import Data.Time.Clock.POSIX (getPOSIXTime)\n")
	header.WriteString("import qualified Data.Map as Map\n")
	header.WriteString("import Data.List (intercalate, isPrefixOf)\n")
	header.WriteString("import qualified Data.List as List\n")
	if c.usesJSON || c.usesLoad || c.usesSave || c.usesFetch || c.usesMap {
		header.WriteString("import qualified Data.Aeson as Aeson\n")
	}
	if len(c.structs) > 0 {
		header.WriteString("import GHC.Generics (Generic)\n")
	}
	if c.usesLoad || c.usesSave || c.usesFetch || c.usesMap {
		header.WriteString("import qualified Data.Aeson.KeyMap as KeyMap\n")
		header.WriteString("import qualified Data.Aeson.Key as Key\n")
		header.WriteString("import qualified Data.Vector as V\n")
		header.WriteString("import qualified Data.Text as T\n")
	}
	if c.usesFetch {
		header.WriteString("import System.Process (readProcess)\n")
	}
	if c.usesJSON || c.usesLoad || c.usesSave || c.usesFetch || c.usesMap {
		header.WriteString("import qualified Data.ByteString.Lazy.Char8 as BSL\n")
	}
	header.WriteString("\n")
	header.WriteString(runtime)
	if c.usesJSON {
		header.WriteString(jsonHelper)
	}
	if c.usesLoad || c.usesSave || c.usesFetch || c.usesMap {
		header.WriteString(loadRuntime)
	}
	if c.usesExpect {
		header.WriteString(expectHelper)
	}
	if c.usesSlice || c.usesSliceStr {
		header.WriteString(sliceHelpers)
	}
	if c.usesFetch {
		header.WriteString(fetchHelper)
	}
	header.WriteString("\n\n")

	code := append(header.Bytes(), c.buf.Bytes()...)
	// Ensure the generated file ends with a trailing newline so tools like
	// runhaskell do not complain about the last line.
	if len(code) == 0 || code[len(code)-1] != '\n' {
		code = append(code, '\n')
	}
	return FormatHS(code), nil
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
		} else if isFetchCall(s.Let.Value) {
			val, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			if s.Let.Type != nil {
				typ := c.hsType(c.resolveTypeRef(s.Let.Type))
				val = fmt.Sprintf("(%s :: IO %s)", val, typ)
			}
			c.writeln(fmt.Sprintf("%s <- %s", sanitizeName(s.Let.Name), val))
		} else {
			if s.Let.Value == nil && s.Let.Type != nil {
				typ := c.hsType(c.resolveTypeRef(s.Let.Type))
				name := sanitizeName(s.Let.Name)
				c.writeln(fmt.Sprintf("let %s :: Maybe %s", name, typ))
				c.writeln(fmt.Sprintf("    %s = Nothing", name))
			} else {
				var val string
				if s.Let.Value != nil {
					v, err := c.compileExpr(s.Let.Value)
					if err != nil {
						return err
					}
					val = v
				} else {
					val = "()"
				}
				c.writeln(fmt.Sprintf("let %s = %s", sanitizeName(s.Let.Name), val))
			}
		}
	case s.Var != nil:
		if s.Var.Value == nil && s.Var.Type != nil {
			typ := c.hsType(c.resolveTypeRef(s.Var.Type))
			name := sanitizeName(s.Var.Name)
			c.writeln(fmt.Sprintf("let %s :: Maybe %s", name, typ))
			c.writeln(fmt.Sprintf("    %s = Nothing", name))
		} else {
			val := "()"
			if s.Var.Value != nil {
				v, err := c.compileExpr(s.Var.Value)
				if err != nil {
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("let %s = %s", sanitizeName(s.Var.Name), val))
		}
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let %s = %s", sanitizeName(s.Assign.Name), val))
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
		if hasBreakOrContinue(s.For.Body) {
			return c.compileForLoopBC(s.For)
		}
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
	case s.If != nil:
		if err := c.compileIfMain(s.If); err != nil {
			return err
		}
	case s.While != nil:
		// Special case simple counting loops where the body ends with an
		// assignment to a variable used in the condition. Translate
		// these to a recursive function so the variable value is
		// threaded through iterations.
		if n := len(s.While.Body); n > 0 && s.While.Body[n-1].Assign != nil {
			asn := s.While.Body[n-1].Assign
			orig := c.env
			if t, err := c.env.GetVar(asn.Name); err == nil {
				child := types.NewEnv(c.env)
				child.SetVar(asn.Name, t, true)
				c.env = child
			}
			cond, err := c.compileExpr(s.While.Cond)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln("let")
			c.indent++
			param := sanitizeName(asn.Name)
			c.writeln(fmt.Sprintf("loop %s = do", param))
			c.indent++
			c.writeln(fmt.Sprintf("if %s then do", cond))
			c.indent++
			for _, st := range s.While.Body[:n-1] {
				if err := c.compileMainStmt(st); err != nil {
					c.env = orig
					return err
				}
			}
			val, err := c.compileExpr(asn.Value)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln(fmt.Sprintf("loop (%s)", val))
			c.indent--
			c.writeln("else return ()")
			c.indent--
			c.indent--
			c.writeln(fmt.Sprintf("loop %s", param))
			c.env = orig
		} else {
			body, err := c.simpleBodyExpr(s.While.Body)
			if err != nil {
				return err
			}
			cond, err := c.compileExpr(s.While.Cond)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let _ = whileLoop (\\() -> %s) (\\() -> Nothing <$ (%s)) in return ()", cond, body))
		}
	case s.Update != nil:
		if err := c.compileUpdate(s.Update); err != nil {
			return err
		}
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

func (c *Compiler) compileIfMain(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then do", cond))
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileMainStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeln("else")
		c.indent++
		if err := c.compileIfMain(stmt.ElseIf); err != nil {
			return err
		}
		c.indent--
	} else if len(stmt.Else) > 0 {
		c.writeln("else do")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileMainStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	} else {
		c.writeln("else return ()")
	}
	return nil
}

func (c *Compiler) simpleBodyExpr(stmts []*parser.Statement) (string, error) {
	if len(stmts) == 1 && stmts[0].Expr != nil {
		return c.compileExpr(stmts[0].Expr.Expr)
	}
	expr, err := c.compileStmtExpr(stmts, false)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("fromMaybe () (%s)", expr), nil
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

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	item := fmt.Sprintf("_it%d", c.tmpCount)
	c.tmpCount++

	var st types.StructType
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}

	origEnv := c.env
	if st.Name != "" {
		child := types.NewEnv(c.env)
		for _, f := range st.Order {
			child.SetVar(f, st.Fields[f], true)
		}
		c.env = child
	}

	var bindings []string
	if st.Name != "" {
		for _, f := range st.Order {
			bindings = append(bindings, fmt.Sprintf("%s = Main.%s %s", sanitizeName(f), sanitizeName(f), item))
		}
	}

	cond := ""
	if u.Where != nil {
		v, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		cond = v
	}

	parts := make([]string, len(u.Set.Items))
	for i, it := range u.Set.Items {
		field, _ := identName(it.Key)
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		parts[i] = fmt.Sprintf("%s = %s", sanitizeName(field), val)
	}
	updated := fmt.Sprintf("%s { %s }", item, strings.Join(parts, ", "))

	expr := updated
	if cond != "" {
		expr = fmt.Sprintf("if %s then %s else %s", cond, updated, item)
	}
	if len(bindings) > 0 {
		expr = fmt.Sprintf("let %s in %s", strings.Join(bindings, "; "), expr)
	}

	c.writeln(fmt.Sprintf("let %s = map (\\%s -> %s) %s", list, item, expr, list))
	c.env = origEnv
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			c.env.SetVar(u.Target, t, true)
		}
	}
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
	for i, op := range p.Ops {
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
		} else if op.Cast != nil {
			typ := c.resolveTypeRef(op.Cast.Type)
			switch tt := typ.(type) {
			case types.IntType, types.Int64Type:
				expr = fmt.Sprintf("(read %s :: Int)", expr)
			case types.FloatType:
				expr = fmt.Sprintf("(read %s :: Double)", expr)
			case types.StringType:
				expr = fmt.Sprintf("show %s", expr)
			case types.BoolType:
				expr = fmt.Sprintf("(read %s :: Bool)", expr)
			case types.StructType:
				if i == 0 && p.Target != nil && p.Target.Map != nil {
					parts := make([]string, len(p.Target.Map.Items))
					for j, it := range p.Target.Map.Items {
						key, ok := simpleStringKey(it.Key)
						if !ok {
							return "", fmt.Errorf("unsupported struct cast key")
						}
						v, err := c.compileExpr(it.Value)
						if err != nil {
							return "", err
						}
						parts[j] = fmt.Sprintf("%s = %s", sanitizeName(key), v)
					}
					expr = fmt.Sprintf("%s { %s }", sanitizeName(tt.Name), strings.Join(parts, ", "))
				} else {
					return "", fmt.Errorf("unsupported cast to struct")
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
		mt, _ := c.inferPrimaryType(p).(types.MapType)
		anyVal := false
		if _, ok := mt.Value.(types.AnyType); ok {
			anyVal = true
		}
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			if s, ok := simpleStringKey(it.Key); ok {
				k := fmt.Sprintf("%q", s)
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				if anyVal {
					v = wrapAnyValue(c.inferExprType(it.Value), v)
				}
				items[i] = fmt.Sprintf("(%s, %s)", k, v)
				continue
			}
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			if anyVal {
				v = wrapAnyValue(c.inferExprType(it.Value), v)
			}
			items[i] = fmt.Sprintf("(%s, %s)", k, v)
		}
		return fmt.Sprintf("Map.fromList [%s]", strings.Join(items, ", ")), nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
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
		if p.Call.Func == "append" && len(args) == 2 {
			return fmt.Sprintf("_append %s %s", args[0], args[1]), nil
		}
		if p.Call.Func == "keys" && len(args) == 1 {
			c.usesMap = true
			return fmt.Sprintf("(Map.keys %s)", args[0]), nil
		}
		if p.Call.Func == "values" && len(args) == 1 {
			c.usesMap = true
			return fmt.Sprintf("(Map.elems %s)", args[0]), nil
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
		if p.Call.Func == "reverse" && len(args) == 1 {
			c.usesList = true
			return fmt.Sprintf("reverse %s", args[0]), nil
		}
		return fmt.Sprintf("%s %s", sanitizeName(p.Call.Func), strings.Join(args, " ")), nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		for i, field := range p.Selector.Tail {
			switch tt := typ.(type) {
			case types.GroupType:
				if field == "key" {
					expr = fmt.Sprintf("key (%s)", expr)
					typ = types.AnyType{}
					continue
				} else if field == "items" {
					expr = fmt.Sprintf("items (%s)", expr)
					typ = types.ListType{Elem: tt.Elem}
					continue
				}
				c.usesMap = true
				expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %q (key %s))", field, expr)
				typ = types.AnyType{}
			case types.MapType:
				c.usesMap = true
				expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %q %s)", field, expr)
				typ = tt.Value
				if i == len(p.Selector.Tail)-1 {
					if _, ok := tt.Value.(types.AnyType); ok {
						ft := c.inferPrimaryType(&parser.Primary{Selector: &parser.SelectorExpr{Root: p.Selector.Root, Tail: p.Selector.Tail}})
						switch ft.(type) {
						case types.IntType, types.Int64Type:
							expr = fmt.Sprintf("_asInt (%s)", expr)
						case types.FloatType:
							expr = fmt.Sprintf("_asDouble (%s)", expr)
						case types.StringType:
							expr = fmt.Sprintf("_asString (%s)", expr)
						case types.BoolType:
							expr = fmt.Sprintf("_asBool (%s)", expr)
						}
						typ = ft
					}
				}
			case types.StructType:
				expr = fmt.Sprintf("%s (%s)", sanitizeName(field), expr)
				if ft, ok := tt.Fields[field]; ok {
					typ = ft
				} else {
					typ = types.AnyType{}
				}
			default:
				c.usesMap = true
				expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %q (%s))", field, expr)
				typ = types.AnyType{}
			}
			if i == len(p.Selector.Tail)-1 {
				return expr, nil
			}
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
		if strings.HasPrefix(arg, "\"") || strings.HasPrefix(arg, "show(") || strings.HasPrefix(arg, "show ") || strings.HasPrefix(arg, "show") || strings.HasPrefix(arg, "_indexString") || c.isStringExpr(ca.exprs[0]) {
			return fmt.Sprintf("putStrLn (%s)", arg), nil
		}
		return fmt.Sprintf("print (%s)", arg), nil
	}
	parts := make([]string, len(ca.args))
	for i, a := range ca.args {
		if strings.HasPrefix(a, "\"") || strings.HasPrefix(a, "_indexString") || c.isStringExpr(ca.exprs[i]) {
			parts[i] = a
		} else {
			parts[i] = fmt.Sprintf("show (%s)", a)
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

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "Nothing"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Just (%s)", v)
	}
	c.usesFetch = true
	c.usesMap = true
	return fmt.Sprintf("_fetch %s %s", url, opts), nil
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

	// handle simple right join as swapped left join
	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "right" {
		j := q.Joins[0]
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
		c.env = child
		on, err := c.compileExpr(j.On)
		c.env = orig
		if err != nil {
			return "", err
		}
		tmp := fmt.Sprintf("_ms%d", c.tmpCount)
		c.tmpCount++
		loops := []string{
			fmt.Sprintf("%s <- %s", sanitizeName(j.Var), js),
			fmt.Sprintf("%s <- let %s = [ %s | %s <- %s, %s ] in if null %s then [Map.empty] else %s", sanitizeName(q.Var), tmp, sanitizeName(q.Var), sanitizeName(q.Var), src, on, tmp, tmp),
		}
		c.env = child
		val, err := c.compileExpr(q.Select)
		c.env = orig
		if err != nil {
			return "", err
		}
		res := fmt.Sprintf("[ %s | %s ]", val, strings.Join(loops, ", "))
		return res, nil
	}

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
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		if j.Side != nil && *j.Side == "left" {
			tmp := fmt.Sprintf("_ms%d", c.tmpCount)
			c.tmpCount++
			loops = append(loops, fmt.Sprintf("%s <- let %s = [ %s | %s <- %s, %s ] in if null %s then [Map.empty] else %s", sanitizeName(j.Var), tmp, sanitizeName(j.Var), sanitizeName(j.Var), js, on, tmp, tmp))
		} else {
			loops = append(loops, fmt.Sprintf("%s <- %s", sanitizeName(j.Var), js))
			conds = append(conds, on)
		}
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
			res = fmt.Sprintf("drop %s (%s)", sk, res)
			c.usesList = true
		}
		if q.Take != nil {
			tk, err := c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			res = fmt.Sprintf("take %s (%s)", tk, res)
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
		res = fmt.Sprintf("drop %s (%s)", sk, res)
		c.usesList = true
	}
	if q.Take != nil {
		tk, err := c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("take %s (%s)", tk, res)
		c.usesList = true
	}
	return res, nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if c.env == nil {
		return nil
	}
	if len(t.Variants) > 0 {
		if ut, ok := c.env.GetUnion(t.Name); ok {
			c.compileUnionType(ut)
		}
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
	c.writeln("} deriving (Eq, Show, Generic)")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) compileUnionType(ut types.UnionType) {
	name := sanitizeName(ut.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("data %s =", name))
	c.indent++
	keys := make([]string, 0, len(ut.Variants))
	for k := range ut.Variants {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for i, k := range keys {
		st := ut.Variants[k]
		line := sanitizeName(k)
		if len(st.Fields) > 0 {
			line += " {"
			for j, fn := range st.Order {
				ft := st.Fields[fn]
				line += fmt.Sprintf(" %s :: %s", sanitizeName(fn), c.hsType(ft))
				if j < len(st.Order)-1 {
					line += ","
				}
			}
			line += " }"
		}
		if i < len(keys)-1 {
			line += " |"
		}
		c.writeln(line)
	}
	c.indent--
	c.writeln("  deriving (Eq, Show, Generic)")
	c.writeln("")
	for _, st := range ut.Variants {
		for _, ft := range st.Fields {
			if sub, ok := ft.(types.StructType); ok {
				c.compileStructType(sub)
			}
		}
	}
}

func hasBreakOrContinue(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		switch {
		case s.Break != nil || s.Continue != nil:
			return true
		case s.If != nil:
			if hasBreakOrContinueIf(s.If) {
				return true
			}
		case s.For != nil:
			if hasBreakOrContinue(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasBreakOrContinue(s.While.Body) {
				return true
			}
		}
	}
	return false
}

func hasBreakOrContinueIf(st *parser.IfStmt) bool {
	if hasBreakOrContinue(st.Then) || hasBreakOrContinue(st.Else) {
		return true
	}
	if st.ElseIf != nil {
		return hasBreakOrContinueIf(st.ElseIf)
	}
	return false
}

func (c *Compiler) compileForLoopBC(loop *parser.ForStmt) error {
	orig := c.env
	var vType types.Type = types.AnyType{}
	if loop.RangeEnd != nil {
		vType = types.IntType{}
	} else {
		t := c.inferExprType(loop.Source)
		switch tt := t.(type) {
		case types.ListType:
			vType = tt.Elem
		case types.MapType:
			vType = tt.Key
		}
	}
	child := types.NewEnv(c.env)
	child.SetVar(loop.Name, vType, true)
	c.env = child
	name := sanitizeName(loop.Name)
	rest := name + "Rest"
	guards, body := c.extractGuards(loop.Body, name, rest)
	src, err := c.compileExpr(loop.Source)
	if err != nil {
		c.env = orig
		return err
	}
	c.writeln("let")
	c.indent++
	c.writeln("loop [] = return ()")
	c.writeln(fmt.Sprintf("loop (%s:%s)", name, rest))
	c.indent++
	for _, g := range guards {
		c.writeln(g)
	}
	if len(body) == 0 {
		c.writeln(fmt.Sprintf("| otherwise = loop %s", rest))
	} else {
		c.writeln("| otherwise = do")
		c.indent++
		for _, st := range body {
			if err := c.compileMainStmt(st); err != nil {
				c.env = orig
				return err
			}
		}
		c.writeln(fmt.Sprintf("loop %s", rest))
		c.indent--
	}
	c.indent--
	c.indent--
	c.writeln(fmt.Sprintf("loop %s", src))
	c.env = orig
	return nil
}

func (c *Compiler) extractGuards(stmts []*parser.Statement, name, rest string) ([]string, []*parser.Statement) {
	var guards []string
	i := 0
	for i < len(stmts) {
		s := stmts[i]
		if s.If != nil && len(s.If.Then) == 1 && s.If.ElseIf == nil && len(s.If.Else) == 0 {
			cond, err := c.compileExpr(s.If.Cond)
			if err != nil {
				break
			}
			if s.If.Then[0].Continue != nil {
				guards = append(guards, fmt.Sprintf("| %s = loop %s", cond, rest))
				i++
				continue
			}
			if s.If.Then[0].Break != nil {
				guards = append(guards, fmt.Sprintf("| %s = return ()", cond))
				i++
				continue
			}
		}
		break
	}
	return guards, stmts[i:]
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

func isFetchCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil || p.Fetch == nil {
		return false
	}
	return true
}
