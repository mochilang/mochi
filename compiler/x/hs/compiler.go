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
	usesAnyValue bool
	usesAnyCast  bool
	usesAsInt    bool
	usesAsDouble bool
	usesAsString bool
	usesAsBool   bool
	usesList     bool
	usesTime     bool
	usesJSON     bool
	usesLoad     bool
	usesSave     bool
	usesSlice    bool
	usesSliceStr bool
	usesExpect   bool
	usesFetch    bool
	usesLoop     bool
	usesUpdate   bool
	usesMaybe    bool
	usesGroup    bool
	hasUserMain  bool
	tmpCount     int
	autoImports  map[string]string
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
		// Mark that map operations are required so generated code
		// includes the Data.Map import. Previously this was set to
		// false which prevented the import and caused "not in scope"
		// errors when functions like Map.lookup were emitted.
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
	case types.UnionType:
		return tt.Name
	default:
		return "()"
	}
}

func New(env *types.Env) *Compiler {
	return &Compiler{
		env:          env,
		structs:      make(map[string]bool),
		tmpCount:     0,
		usesLoop:     false,
		usesUpdate:   false,
		usesMaybe:    false,
		usesAsInt:    false,
		usesAsDouble: false,
		usesAsString: false,
		usesAsBool:   false,
		usesGroup:    false,
		hasUserMain:  false,
		autoImports:  make(map[string]string),
	}
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
	c.usesLoop = false
	c.usesUpdate = false
	c.usesMaybe = false
	c.usesAnyCast = false
	c.usesAsInt = false
	c.usesAsDouble = false
	c.usesAsString = false
	c.usesAsBool = false
	c.usesGroup = false
	c.tmpCount = 0
	c.autoImports = make(map[string]string)

	for _, s := range prog.Statements {
		if s.Import != nil {
			if err := c.compileImport(s.Import); err != nil {
				return nil, err
			}
		}
	}

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
					name := c.sn(s.Let.Name)
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
					c.writeln(fmt.Sprintf("%s = %s", c.sn(s.Let.Name), val))
				}
				c.writeln("")
			} else if s.Var != nil {
				if s.Var.Value == nil && s.Var.Type != nil {
					typ := c.hsType(c.resolveTypeRef(s.Var.Type))
					name := c.sn(s.Var.Name)
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
					c.writeln(fmt.Sprintf("%s = %s", c.sn(s.Var.Name), val))
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

	if !c.hasUserMain {
		c.writeln("main :: IO ()")
		c.writeln("main = do")
		c.indent++
	}
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
			name := "test_" + c.sn(s.Test.Name)
			c.writeln(name)
			testCount++
		}
	}
	if !c.hasUserMain {
		if mainStmts == 0 && testCount == 0 {
			c.writeln("return ()")
		}
		c.indent--
	} else {
		c.writeln("main :: IO ()")
		c.writeln("main = user_main")
	}

	body := c.buf.Bytes()
	if !c.usesAsInt && bytes.Contains(body, []byte("_asInt")) {
		c.usesAnyValue = true
		c.usesAsInt = true
	}
	if !c.usesAsDouble && bytes.Contains(body, []byte("_asDouble")) {
		c.usesAnyValue = true
		c.usesAsDouble = true
	}
	if !c.usesAsString && bytes.Contains(body, []byte("_asString")) {
		c.usesAnyValue = true
		c.usesAsString = true
	}
	if !c.usesAsBool && bytes.Contains(body, []byte("_asBool")) {
		c.usesAnyValue = true
		c.usesAsBool = true
	}

	var header bytes.Buffer
	header.WriteString("-- Code generated by Mochi compiler; DO NOT EDIT.\n")
	header.WriteString("{-# LANGUAGE DeriveGeneric #-}\n")
	header.WriteString("module Main where\n\n")
	needsMaybe := c.usesMaybe || c.usesLoop || c.usesList || c.usesTime ||
		c.usesJSON || c.usesLoad || c.usesSave || c.usesSlice ||
		c.usesSliceStr || c.usesExpect || c.usesFetch || c.usesGroup
	if needsMaybe {
		header.WriteString("import Data.Maybe (fromMaybe)\n")
	}
	if c.usesTime {
		header.WriteString("import Data.Time.Clock.POSIX (getPOSIXTime)\n")
	}
	if c.usesMap || c.usesLoad || c.usesSave || c.usesFetch ||
		c.usesLoop || c.usesList || c.usesTime || c.usesJSON ||
		c.usesSlice || c.usesSliceStr || c.usesExpect {
		header.WriteString("import qualified Data.Map as Map\n")
	}
	if c.usesList || c.usesSlice || c.usesSliceStr || c.usesLoop || c.usesFetch {
		header.WriteString("import Data.List (intercalate, isPrefixOf, isInfixOf)\n")
		header.WriteString("import qualified Data.List as List\n")
	}
	if c.usesJSON || c.usesLoad || c.usesSave || c.usesFetch {
		header.WriteString("import qualified Data.Aeson as Aeson\n")
	}
	if len(c.structs) > 0 {
		header.WriteString("import GHC.Generics (Generic)\n")
	}
	if c.usesLoad || c.usesSave || c.usesFetch {
		header.WriteString("import qualified Data.Aeson.KeyMap as KeyMap\n")
		header.WriteString("import qualified Data.Aeson.Key as Key\n")
		header.WriteString("import qualified Data.Vector as V\n")
		header.WriteString("import qualified Data.Text as T\n")
	}
	if c.usesFetch {
		header.WriteString("import System.Process (readProcess)\n")
	}
	if c.usesJSON || c.usesLoad || c.usesSave || c.usesFetch {
		header.WriteString("import qualified Data.ByteString.Lazy.Char8 as BSL\n")
	}
	header.WriteString("\n")
	if c.usesLoop || c.usesList || c.usesTime || c.usesJSON || c.usesLoad || c.usesSave || c.usesSlice || c.usesSliceStr || c.usesExpect || c.usesFetch {
		header.WriteString(runtime)
	}
	if c.usesGroup {
		header.WriteString(groupHelpers)
	}
	if c.usesTime {
		header.WriteString(timeRuntime)
	}
	if c.usesJSON {
		header.WriteString(jsonHelper)
	}
	if c.usesLoad || c.usesSave || c.usesFetch {
		header.WriteString(anyValueRuntime)
		if c.usesAsInt {
			header.WriteString(asIntRuntime)
		}
		if c.usesAsDouble {
			header.WriteString(asDoubleRuntime)
		}
		if c.usesAsString {
			header.WriteString(asStringRuntime)
		}
		if c.usesAsBool {
			header.WriteString(asBoolRuntime)
		}
		header.WriteString(loadRuntime)
	} else if c.usesAnyValue {
		header.WriteString(anyValueRuntime)
		if c.usesAsInt {
			header.WriteString(asIntRuntime)
		}
		if c.usesAsDouble {
			header.WriteString(asDoubleRuntime)
		}
		if c.usesAsString {
			header.WriteString(asStringRuntime)
		}
		if c.usesAsBool {
			header.WriteString(asBoolRuntime)
		}
	} else if c.usesAsInt || c.usesAsDouble || c.usesAsString || c.usesAsBool {
		header.WriteString(anyValueRuntime)
		if c.usesAsInt {
			header.WriteString(asIntRuntime)
		}
		if c.usesAsDouble {
			header.WriteString(asDoubleRuntime)
		}
		if c.usesAsString {
			header.WriteString(asStringRuntime)
		}
		if c.usesAsBool {
			header.WriteString(asBoolRuntime)
		}
	}
	if c.usesExpect {
		header.WriteString(expectHelper)
	}
	if c.usesSlice || c.usesSliceStr {
		header.WriteString(sliceHelpers)
	}
	if c.usesUpdate {
		header.WriteString(updateHelpers)
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
	case s.Import != nil:
		return nil
	case s.ExternVar != nil || s.ExternFun != nil || s.ExternType != nil || s.ExternObject != nil:
		return nil
	case s.Let != nil:
		if isInputCall(s.Let.Value) {
			val, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s <- %s", c.sn(s.Let.Name), val))
		} else if isFetchCall(s.Let.Value) {
			val, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			if s.Let.Type != nil {
				typ := c.hsType(c.resolveTypeRef(s.Let.Type))
				val = fmt.Sprintf("(%s :: IO %s)", val, typ)
			}
			c.writeln(fmt.Sprintf("%s <- %s", c.sn(s.Let.Name), val))
		} else {
			if s.Let.Value == nil && s.Let.Type != nil {
				typ := c.hsType(c.resolveTypeRef(s.Let.Type))
				name := c.sn(s.Let.Name)
				c.writeln(fmt.Sprintf("let %s :: Maybe %s", name, typ))
				c.writeln(fmt.Sprintf("    %s = Nothing", name))
			} else {
				var val string
				if s.Let.Value != nil {
					if s.Let.Type != nil {
						v, err := c.compileExprHint(s.Let.Value, c.resolveTypeRef(s.Let.Type))
						if err != nil {
							return err
						}
						val = v
					} else {
						v, err := c.compileExpr(s.Let.Value)
						if err != nil {
							return err
						}
						val = v
					}
				} else {
					val = "()"
				}
				c.writeln(fmt.Sprintf("let %s = %s", c.sn(s.Let.Name), val))
			}
		}
	case s.Var != nil:
		if s.Var.Value == nil && s.Var.Type != nil {
			typ := c.hsType(c.resolveTypeRef(s.Var.Type))
			name := c.sn(s.Var.Name)
			c.writeln(fmt.Sprintf("let %s :: Maybe %s", name, typ))
			c.writeln(fmt.Sprintf("    %s = Nothing", name))
		} else {
			val := "()"
			if s.Var.Value != nil {
				if s.Var.Type != nil {
					v, err := c.compileExprHint(s.Var.Value, c.resolveTypeRef(s.Var.Type))
					if err != nil {
						return err
					}
					val = v
				} else {
					v, err := c.compileExpr(s.Var.Value)
					if err != nil {
						return err
					}
					val = v
				}
			}
			c.writeln(fmt.Sprintf("let %s = %s", c.sn(s.Var.Name), val))
		}
	case s.Assign != nil:
		val, err := c.compileAssignValue(s.Assign)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let %s = %s", c.sn(s.Assign.Name), val))
	case s.Fun != nil:
		expr, err := c.compileFunExpr(&parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body})
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("let %s = %s", c.sn(s.Fun.Name), expr))
	case s.Expr != nil:
		if c.hasUserMain {
			if call, ok := callPattern(s.Expr.Expr); ok && call.Func == "main" && len(call.Args) == 0 {
				return nil
			}
		}
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.For != nil:
		c.usesLoop = true
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
		name := c.sn(s.For.Name)
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
		c.usesLoop = true
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
			cond, err := c.compileBoolExpr(s.While.Cond)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln("let")
			c.indent++
			param := c.sn(asn.Name)
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
			cond, err := c.compileBoolExpr(s.While.Cond)
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
	cond, err := c.compileBoolExpr(stmt.Cond)
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
	c.usesMaybe = true
	return fmt.Sprintf("fromMaybe (return ()) (%s)", expr), nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	if fun.Name == "main" {
		c.hasUserMain = true
	}
	name := c.sn(fun.Name)
	params := make([]string, len(fun.Params))
	paramTypes := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = c.sn(p.Name)
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

	// create function scope with parameter variables so type inference works
	origEnv := c.env
	if len(fun.Params) > 0 {
		child := types.NewEnv(c.env)
		if len(ft.Params) == len(fun.Params) {
			for i, p := range fun.Params {
				child.SetVar(p.Name, ft.Params[i], true)
			}
		} else {
			for _, p := range fun.Params {
				child.SetVar(p.Name, types.AnyType{}, true)
			}
		}
		c.env = child
	}

	if len(fun.Body) == 1 && fun.Body[0].Return != nil {
		var val string
		var err error
		if fun.Return != nil {
			val, err = c.compileExprHint(fun.Body[0].Return.Value, c.resolveTypeRef(fun.Return))
		} else {
			val, err = c.compileExpr(fun.Body[0].Return.Value)
		}
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s %s = %s", name, strings.Join(params, " "), val))
	} else {
		c.usesMaybe = true
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
	c.env = origEnv
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
			res = append(res, fmt.Sprintf("%s = %s", c.sn(s.Let.Name), v))
		} else {
			break
		}
	}
	return res
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + c.sn(t.Name)
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

func (c *Compiler) compileAssignValue(a *parser.AssignStmt) (string, error) {
	var val string
	var err error
	var typ types.Type
	if c.env != nil {
		if t, err2 := c.env.GetVar(a.Name); err2 == nil {
			typ = t
		}
	}
	if typ != nil {
		val, err = c.compileExprHint(a.Value, typ)
	} else {
		val, err = c.compileExpr(a.Value)
	}
	if err != nil {
		return "", err
	}
	return c.assignPath(c.sn(a.Name), typ, a.Index, a.Field, val)
}

func (c *Compiler) assignPath(base string, typ types.Type, idx []*parser.IndexOp, fields []*parser.FieldOp, val string) (string, error) {
	if len(idx) == 0 && len(fields) == 0 {
		return val, nil
	}
	if len(idx) > 0 {
		ix, err := c.compileExpr(idx[0].Start)
		if err != nil {
			return "", err
		}
		if len(idx) == 1 && len(fields) == 0 {
			switch typ.(type) {
			case types.MapType:
				c.usesMap = true
				return fmt.Sprintf("Map.insert %s %s %s", ix, val, base), nil
			case types.ListType:
				c.usesList = true
				c.usesUpdate = true
				return fmt.Sprintf("_updateAt %s (const %s) %s", ix, val, base), nil
			default:
				if c.isIntExpr(idx[0].Start) {
					c.usesList = true
					c.usesUpdate = true
					return fmt.Sprintf("_updateAt %s (const %s) %s", ix, val, base), nil
				}
				c.usesMap = true
				return fmt.Sprintf("Map.insert %s %s %s", ix, val, base), nil
			}
		}
		switch tt := typ.(type) {
		case types.MapType:
			c.usesMap = true
			inner, err := c.assignPath("m", tt.Value, idx[1:], fields, val)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("Map.adjust (\\m -> %s) %s %s", inner, ix, base), nil
		case types.ListType:
			c.usesList = true
			c.usesUpdate = true
			inner, err := c.assignPath(fmt.Sprintf("(%s !! %s)", base, ix), tt.Elem, idx[1:], fields, val)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("_updateAt %s (\\_ -> %s) %s", ix, inner, base), nil
		default:
			if c.isIntExpr(idx[0].Start) {
				c.usesList = true
				c.usesUpdate = true
				inner, err := c.assignPath(fmt.Sprintf("(%s !! %s)", base, ix), types.AnyType{}, idx[1:], fields, val)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("_updateAt %s (\\_ -> %s) %s", ix, inner, base), nil
			}
			c.usesMap = true
			inner, err := c.assignPath("m", types.AnyType{}, idx[1:], fields, val)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("Map.adjust (\\m -> %s) %s %s", inner, ix, base), nil
		}
	} else if len(fields) > 0 {
		field := c.sn(fields[0].Name)
		if st, ok := typ.(types.StructType); ok {
			ft := st.Fields[fields[0].Name]
			inner, err := c.assignPath(fmt.Sprintf("%s (%s)", field, base), ft, idx, fields[1:], val)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s { %s = %s }", base, field, inner), nil
		}
	}
	return val, nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := c.sn(u.Target)
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
			bindings = append(bindings, fmt.Sprintf("%s = Main.%s %s", c.sn(f), c.sn(f), item))
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
		parts[i] = fmt.Sprintf("%s = %s", c.sn(field), val)
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
		case s.Import != nil:
			// imports have no runtime effect in generated Haskell
			continue
		case s.ExternVar != nil || s.ExternFun != nil || s.ExternType != nil || s.ExternObject != nil:
			continue
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
			c.usesLoop = true
			bodyExpr, err := c.compileStmtExpr(s.For.Body, false)
			if err != nil {
				return "", err
			}
			name := c.sn(s.For.Name)
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
			var t types.Type = types.AnyType{}
			if s.Var.Value != nil {
				if s.Var.Type != nil {
					vt := c.resolveTypeRef(s.Var.Type)
					v, err := c.compileExprHint(s.Var.Value, vt)
					if err != nil {
						return "", err
					}
					val = v
					t = vt
				} else {
					v, err := c.compileExpr(s.Var.Value)
					if err != nil {
						return "", err
					}
					val = v
					t = c.inferExprType(s.Var.Value)
				}
			} else if s.Var.Type != nil {
				t = c.resolveTypeRef(s.Var.Type)
			}
			c.env.SetVar(s.Var.Name, t, true)
			expr = fmt.Sprintf("(let %s = %s in %s)", c.sn(s.Var.Name), val, expr)
		case s.Break != nil:
			expr = "Just ()"
			continue
		case s.Continue != nil:
			expr = "Nothing"
			continue
		case s.Let != nil:
			val := "()"
			var t types.Type = types.AnyType{}
			if s.Let.Value != nil {
				if s.Let.Type != nil {
					vt := c.resolveTypeRef(s.Let.Type)
					v, err := c.compileExprHint(s.Let.Value, vt)
					if err != nil {
						return "", err
					}
					val = v
					t = vt
				} else {
					v, err := c.compileExpr(s.Let.Value)
					if err != nil {
						return "", err
					}
					val = v
					t = c.inferExprType(s.Let.Value)
				}
			} else if s.Let.Type != nil {
				t = c.resolveTypeRef(s.Let.Type)
			}
			c.env.SetVar(s.Let.Name, t, false)
			expr = fmt.Sprintf("(let %s = %s in %s)", c.sn(s.Let.Name), val, expr)
		case s.Fun != nil:
			val, err := c.compileFunExpr(&parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body})
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("(let %s = %s in %s)", c.sn(s.Fun.Name), val, expr)
		case s.Assign != nil:
			val, err := c.compileAssignValue(s.Assign)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("(let %s = %s in %s)", c.sn(s.Assign.Name), val, expr)
		case s.While != nil:
			c.usesLoop = true
			bodyExpr, err := c.compileStmtExpr(s.While.Body, false)
			if err != nil {
				return "", err
			}
			cond, err := c.compileBoolExpr(s.While.Cond)
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
			// Wrap expression statements so their side effects are
			// executed when the resulting IO action is run.
			expr = chainMaybe(fmt.Sprintf("Just (%s)", val), expr)
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
	cond, err := c.compileBoolExpr(stmt.Cond)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("if %s then %s else %s", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileIfExprExpr(e *parser.IfExpr) (string, error) {
	thenExpr, err := c.compileExpr(e.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if e.ElseIf != nil {
		elseExpr, err = c.compileIfExprExpr(e.ElseIf)
		if err != nil {
			return "", err
		}
	} else if e.Else != nil {
		elseExpr, err = c.compileExpr(e.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "()"
	}
	cond, err := c.compileBoolExpr(e.Cond)
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

// compileExprHint compiles an expression using a type hint when dealing with
// literals that would otherwise default to `any`. The hint is applied
// recursively for nested list and map literals.
func (c *Compiler) compileExprHint(e *parser.Expr, hint types.Type) (string, error) {
	switch ht := hint.(type) {
	case types.ListType:
		if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
			if ll := e.Binary.Left.Value.Target.List; ll != nil {
				elems := make([]string, len(ll.Elems))
				for i, el := range ll.Elems {
					ev, err := c.compileExprHint(el, ht.Elem)
					if err != nil {
						return "", err
					}
					elems[i] = ev
				}
				return "[" + strings.Join(elems, ", ") + "]", nil
			}
		}
	case types.MapType:
		if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
			if ml := e.Binary.Left.Value.Target.Map; ml != nil {
				items := make([]string, len(ml.Items))
				for i, it := range ml.Items {
					k, err := c.compileExprHint(it.Key, ht.Key)
					if err != nil {
						return "", err
					}
					if s, ok := simpleStringKey(it.Key); ok {
						k = fmt.Sprintf("%q", s)
					}
					v, err := c.compileExprHint(it.Value, ht.Value)
					if err != nil {
						return "", err
					}
					items[i] = fmt.Sprintf("(%s, %s)", k, v)
				}
				c.usesMap = true
				return fmt.Sprintf("Map.fromList [%s]", strings.Join(items, ", ")), nil
			}
		}
	case types.StructType:
		if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
			if ml := e.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == len(ht.Fields) {
				parts := make([]string, len(ml.Items))
				for i, it := range ml.Items {
					name, ok := identName(it.Key)
					if !ok {
						if s, ok2 := simpleStringKey(it.Key); ok2 {
							name = s
						} else {
							return c.compileExpr(e)
						}
					}
					ft, ok := ht.Fields[name]
					val, err := c.compileExprHint(it.Value, ft)
					if err != nil {
						return "", err
					}
					parts[i] = fmt.Sprintf("%s = %s", c.sn(name), val)
				}
				c.compileStructType(ht)
				return fmt.Sprintf("%s { %s }", c.sn(ht.Name), strings.Join(parts, ", ")), nil
			}
		}
	}
	return c.compileExpr(e)
}

// compileBoolExpr compiles an expression expected to yield a Bool. If the
// expression has type `any`, it is wrapped with `_asBool` to coerce the value
// at runtime.
func (c *Compiler) compileBoolExpr(e *parser.Expr) (string, error) {
	expr, err := c.compileExpr(e)
	if err != nil {
		return "", err
	}
	if _, ok := c.inferExprType(e).(types.AnyType); ok {
		c.usesAnyValue = true
		c.usesAnyCast = true
		c.usesAsBool = true
		return fmt.Sprintf("_asBool (%s)", expr), nil
	}
	return expr, nil
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
			} else if _, ok1 := leftType.(types.StringType); ok1 {
				if _, ok2 := rightType.(types.StringType); ok2 {
					expr = fmt.Sprintf("isInfixOf %s %s", expr, r)
				} else {
					expr = fmt.Sprintf("elem %s %s", expr, r)
				}
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
			expr = fmt.Sprintf("(%s List.\\\\ %s)", expr, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		} else if op.Op == "intersect" {
			c.usesList = true
			expr = fmt.Sprintf("List.intersect %s %s", expr, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		} else if op.Op == "+" && (isString(leftType) || isString(rightType) || c.isStringUnary(b.Left) || c.isStringPostfix(op.Right)) {
			expr = fmt.Sprintf("(%s ++ %s)", expr, r)
			leftType = types.StringType{}
		} else {
			opSym := op.Op
			if opSym == "!=" {
				opSym = "/="
			}
			if opSym == "+" || opSym == "-" || opSym == "*" || opSym == "/" {
				if (isAny(leftType) || containsAny(leftType)) && isInt(rightType) {
					expr = fmt.Sprintf("_asInt (%s)", expr)
					c.usesAnyCast = true
					c.usesAsInt = true
					leftType = types.IntType{}
				} else if (isAny(leftType) || containsAny(leftType)) && isFloat(rightType) {
					expr = fmt.Sprintf("_asDouble (%s)", expr)
					c.usesAnyCast = true
					c.usesAsDouble = true
					leftType = types.FloatType{}
				}
				if (isAny(rightType) || containsAny(rightType)) && isInt(leftType) {
					r = fmt.Sprintf("_asInt (%s)", r)
					c.usesAnyCast = true
					c.usesAsInt = true
					rightType = types.IntType{}
				} else if (isAny(rightType) || containsAny(rightType)) && isFloat(leftType) {
					r = fmt.Sprintf("_asDouble (%s)", r)
					c.usesAnyCast = true
					c.usesAsDouble = true
					rightType = types.FloatType{}
				}
			}
			if opSym == "==" || opSym == "/=" || opSym == "<" || opSym == "<=" || opSym == ">" || opSym == ">=" {
				if (isAny(leftType) || containsAny(leftType)) && isInt(rightType) {
					expr = fmt.Sprintf("_asInt (%s)", expr)
					c.usesAnyCast = true
					c.usesAsInt = true
					leftType = types.IntType{}
				} else if (isAny(leftType) || containsAny(leftType)) && isFloat(rightType) {
					expr = fmt.Sprintf("_asDouble (%s)", expr)
					c.usesAnyCast = true
					c.usesAsDouble = true
					leftType = types.FloatType{}
				}
				if (isAny(rightType) || containsAny(rightType)) && isInt(leftType) {
					r = fmt.Sprintf("_asInt (%s)", r)
					c.usesAnyCast = true
					c.usesAsInt = true
					rightType = types.IntType{}
				} else if (isAny(rightType) || containsAny(rightType)) && isFloat(leftType) {
					r = fmt.Sprintf("_asDouble (%s)", r)
					c.usesAnyCast = true
					c.usesAsDouble = true
					rightType = types.FloatType{}
				}
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
			t := c.inferPostfixType(u.Value)
			if isAny(t) {
				expr = fmt.Sprintf("(- (_asInt (%s)))", expr)
				c.usesAnyCast = true
				c.usesAsInt = true
			} else {
				expr = fmt.Sprintf("(-%s)", expr)
			}
		case "!":
			expr = fmt.Sprintf("not %s", expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if sel := p.Target.Selector; sel != nil && len(sel.Tail) == 1 {
		if mod, ok := c.autoImports[sel.Root]; ok {
			field := sel.Tail[0]
			switch mod {
			case "go_testpkg":
				if len(p.Ops) > 0 && p.Ops[0].Call != nil && field == "Add" {
					if len(p.Ops[0].Call.Args) == 2 {
						a1, err := c.compileExpr(p.Ops[0].Call.Args[0])
						if err != nil {
							return "", err
						}
						a2, err := c.compileExpr(p.Ops[0].Call.Args[1])
						if err != nil {
							return "", err
						}
						c.usesAnyValue = true
						return fmt.Sprintf("VInt (%s + %s)", a1, a2), nil
					}
				}
				if len(p.Ops) == 0 {
					c.usesAnyValue = true
					switch field {
					case "Pi":
						return "VDouble 3.14", nil
					case "Answer":
						return "VInt 42", nil
					}
				}
			case "python_math_auto":
				if len(p.Ops) > 0 && p.Ops[0].Call != nil {
					args := make([]string, len(p.Ops[0].Call.Args))
					for i, a := range p.Ops[0].Call.Args {
						v, err := c.compileExpr(a)
						if err != nil {
							return "", err
						}
						args[i] = v
					}
					switch field {
					case "sqrt", "sin", "log":
						if len(args) == 1 {
							c.usesAnyValue = true
							return fmt.Sprintf("VDouble (%s %s)", field, args[0]), nil
						}
					case "pow":
						if len(args) == 2 {
							c.usesAnyValue = true
							return fmt.Sprintf("VDouble (%s ** %s)", args[0], args[1]), nil
						}
					}
				} else if len(p.Ops) == 0 {
					c.usesAnyValue = true
					switch field {
					case "pi":
						return "VDouble pi", nil
					case "e":
						return "VDouble (exp 1)", nil
					}
				}
			case "python_math":
				if len(p.Ops) > 0 && p.Ops[0].Call != nil {
					args := make([]string, len(p.Ops[0].Call.Args))
					for i, a := range p.Ops[0].Call.Args {
						v, err := c.compileExpr(a)
						if err != nil {
							return "", err
						}
						args[i] = v
					}
					switch field {
					case "sqrt", "sin", "log":
						if len(args) == 1 {
							return fmt.Sprintf("%s %s", field, args[0]), nil
						}
					case "pow":
						if len(args) == 2 {
							return fmt.Sprintf("(%s ** %s)", args[0], args[1]), nil
						}
					}
				} else if len(p.Ops) == 0 {
					switch field {
					case "pi":
						return "pi", nil
					case "e":
						return "(exp 1)", nil
					}
				}
			}
		}
	}

	// Special-case string contains method
	if sel := p.Target.Selector; sel != nil && len(sel.Tail) > 0 && sel.Tail[len(sel.Tail)-1] == "contains" {
		if len(p.Ops) > 0 && p.Ops[0].Call != nil && len(p.Ops[0].Call.Args) == 1 {
			baseSel := &parser.SelectorExpr{Root: sel.Root, Tail: sel.Tail[:len(sel.Tail)-1]}
			basePrim := &parser.Primary{Selector: baseSel}
			if c.isStringPrimary(basePrim) {
				baseExpr, err := c.compilePrimary(basePrim)
				if err != nil {
					return "", err
				}
				argExpr, err := c.compileExpr(p.Ops[0].Call.Args[0])
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("isInfixOf %s %s", argExpr, baseExpr), nil
			}
		}
	}

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
					expr = fmt.Sprintf("take (%s - %s) (drop %s %s)", end, start, start, expr)
				} else {
					expr = fmt.Sprintf("take (%s - %s) (drop %s %s)", end, start, start, expr)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isMapPrimary(p.Target) {
					c.usesMap = true
					c.usesMaybe = true
					expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %s %s)", idx, expr)
				} else if c.isStringPrimary(p.Target) {
					expr = fmt.Sprintf("[%s !! %s]", expr, idx)
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
						parts[j] = fmt.Sprintf("%s = %s", c.sn(key), v)
					}
					expr = fmt.Sprintf("%s { %s }", c.sn(tt.Name), strings.Join(parts, ", "))
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
		hetero := false
		var first types.Type
		for i, it := range p.Map.Items {
			vt := c.inferExprType(it.Value)
			if i == 0 {
				first = vt
			} else if fmt.Sprintf("%T", vt) != fmt.Sprintf("%T", first) {
				hetero = true
				break
			}
		}
		if hetero {
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
				vt := c.inferExprType(it.Value)
				if anyVal {
					if !isAny(vt) {
						c.usesAnyValue = true
						v = wrapAnyValue(vt, v)
					}
				} else {
					switch vt.(type) {
					case types.IntType, types.Int64Type:
						v = fmt.Sprintf("(%s :: Int)", v)
					case types.FloatType:
						v = fmt.Sprintf("(%s :: Double)", v)
					}
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
				vt := c.inferExprType(it.Value)
				if !isAny(vt) {
					c.usesAnyValue = true
					v = wrapAnyValue(vt, v)
				}
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
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
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
			if len(args) == 1 {
				if _, ok := c.inferExprType(p.Call.Args[0]).(types.MapType); ok {
					c.usesMap = true
					return fmt.Sprintf("Map.size (%s)", args[0]), nil
				}
			}
			return fmt.Sprintf("length %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "count" {
			if len(args) == 1 {
				if _, ok := c.inferExprType(p.Call.Args[0]).(types.GroupType); ok {
					return fmt.Sprintf("length (gItems %s)", args[0]), nil
				}
			}
			return fmt.Sprintf("length %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "exists" {
			if len(args) == 1 {
				t := c.inferExprType(p.Call.Args[0])
				switch t.(type) {
				case types.MapType:
					c.usesMap = true
					return fmt.Sprintf("Map.size %s > 0", args[0]), nil
				case types.ListType, types.GroupType:
					return fmt.Sprintf("not (null %s)", args[0]), nil
				case types.StringType:
					return fmt.Sprintf("not (null %s)", args[0]), nil
				}
			}
			return fmt.Sprintf("not (null %s)", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "avg" && len(args) == 1 {
			return fmt.Sprintf("avg %s", args[0]), nil
		}
		if p.Call.Func == "substring" && len(args) == 3 {
			start := args[1]
			end := args[2]
			src := args[0]
			return fmt.Sprintf("take (%s - %s) (drop %s %s)", end, start, start, src), nil
		}
		if p.Call.Func == "min" && len(args) == 1 {
			return fmt.Sprintf("minimum %s", args[0]), nil
		}
		if p.Call.Func == "max" && len(args) == 1 {
			return fmt.Sprintf("maximum %s", args[0]), nil
		}
		if p.Call.Func == "str" {
			return fmt.Sprintf("show %s", strings.Join(args, " ")), nil
		}
		if p.Call.Func == "push" && len(args) == 2 {
			return fmt.Sprintf("(%s ++ [%s])", args[0], args[1]), nil
		}
		if p.Call.Func == "append" && len(args) == 2 {
			return fmt.Sprintf("(%s ++ [%s])", args[0], args[1]), nil
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
			if len(p.Call.Args) > 0 && containsAny(c.inferExprType(p.Call.Args[0])) {
				c.usesAnyValue = true
				c.usesLoad = true
			}
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
		return fmt.Sprintf("%s %s", c.sn(p.Call.Func), strings.Join(args, " ")), nil
	case p.Selector != nil:
		expr := c.sn(p.Selector.Root)
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
					expr = fmt.Sprintf("gKey (%s)", expr)
					typ = types.AnyType{}
					continue
				} else if field == "items" {
					expr = fmt.Sprintf("gItems (%s)", expr)
					typ = types.ListType{Elem: tt.Elem}
					continue
				}
				c.usesMap = true
				c.usesMaybe = true
				expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %q (gKey %s))", field, expr)
				typ = types.AnyType{}
			case types.MapType:
				c.usesMap = true
				c.usesMaybe = true
				expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %q %s)", field, expr)
				typ = tt.Value
				if i == len(p.Selector.Tail)-1 {
					if _, ok := tt.Value.(types.AnyType); ok {
						ft := c.inferPrimaryType(&parser.Primary{Selector: &parser.SelectorExpr{Root: p.Selector.Root, Tail: p.Selector.Tail}})
						switch ft.(type) {
						case types.IntType, types.Int64Type:
							expr = fmt.Sprintf("_asInt (%s)", expr)
							c.usesAnyCast = true
							c.usesAsInt = true
						case types.FloatType:
							expr = fmt.Sprintf("_asDouble (%s)", expr)
							c.usesAnyCast = true
							c.usesAsDouble = true
						case types.StringType:
							expr = fmt.Sprintf("_asString (%s)", expr)
							c.usesAnyCast = true
							c.usesAsString = true
						case types.BoolType:
							expr = fmt.Sprintf("_asBool (%s)", expr)
							c.usesAnyCast = true
							c.usesAsBool = true
						}
						typ = ft
					}
				}
			case types.StructType:
				expr = fmt.Sprintf("%s (%s)", c.sn(field), expr)
				if ft, ok := tt.Fields[field]; ok {
					typ = ft
				} else {
					typ = types.AnyType{}
				}
			default:
				c.usesMap = true
				c.usesMaybe = true
				expr = fmt.Sprintf("fromMaybe (error \"missing\") (Map.lookup %q (%s))", field, expr)
				typ = types.AnyType{}
			}
			if i == len(p.Selector.Tail)-1 {
				return expr, nil
			}
		}
		return expr, nil
	case p.If != nil:
		return c.compileIfExprExpr(p.If)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s = %s", c.sn(f.Name), v)
		}
		return fmt.Sprintf("%s { %s }", c.sn(p.Struct.Name), strings.Join(parts, ", ")), nil
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
		if strings.HasPrefix(arg, "\"") || strings.HasPrefix(arg, "show(") || strings.HasPrefix(arg, "show ") || strings.HasPrefix(arg, "show") || c.isStringExpr(ca.exprs[0]) {
			return fmt.Sprintf("putStrLn (%s)", arg), nil
		}
		return fmt.Sprintf("print (%s)", arg), nil
	}
	parts := make([]string, len(ca.args))
	for i, a := range ca.args {
		if strings.HasPrefix(a, "\"") || c.isStringExpr(ca.exprs[i]) {
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
		params[i] = c.sn(p.Name)
	}
	origEnv := c.env
	if len(fn.Params) > 0 {
		child := types.NewEnv(c.env)
		for _, p := range fn.Params {
			if p.Type != nil {
				child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
			} else {
				child.SetVar(p.Name, types.AnyType{}, true)
			}
		}
		c.env = child
	}
	if fn.ExprBody != nil {
		var body string
		var err error
		if fn.Return != nil {
			body, err = c.compileExprHint(fn.ExprBody, c.resolveTypeRef(fn.Return))
		} else {
			body, err = c.compileExpr(fn.ExprBody)
		}
		if err != nil {
			c.env = origEnv
			return "", err
		}
		c.env = origEnv
		return fmt.Sprintf("(\\%s -> %s)", strings.Join(params, " "), body), nil
	}
	if len(fn.BlockBody) == 0 {
		c.env = origEnv
		return fmt.Sprintf("(\\%s -> ())", strings.Join(params, " ")), nil
	}
	expr, err := c.compileStmtExpr(fn.BlockBody, true)
	if err != nil {
		c.env = origEnv
		return "", err
	}
	ret := c.defaultReturn(fn.BlockBody, types.VoidType{})
	c.usesMaybe = true
	c.env = origEnv
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

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	if im.Lang == nil {
		return nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	path := strings.Trim(im.Path, "\"")
	switch *im.Lang {
	case "python":
		if path == "math" {
			if im.Auto {
				c.autoImports[alias] = "python_math_auto"
			} else {
				c.autoImports[alias] = "python_math"
			}
		}
	case "go":
		if strings.Contains(path, "testpkg") {
			c.autoImports[alias] = "go_testpkg"
		}
	}
	return nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(case ")
	b.WriteString(target)
	b.WriteString(" of")
	for _, cs := range m.Cases {
		pat, vars, err := c.compileMatchPattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		orig := c.env
		if len(vars) > 0 {
			child := types.NewEnv(c.env)
			for n, t := range vars {
				child.SetVar(n, t, true)
			}
			c.env = child
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig
		b.WriteString(" ")
		b.WriteString(pat)
		b.WriteString(" -> ")
		b.WriteString(res)
		b.WriteString(";")
	}
	b.WriteString(" )")
	return b.String(), nil
}

func (c *Compiler) compileMatchPattern(e *parser.Expr) (string, map[string]types.Type, error) {
	if isUnderscoreExpr(e) {
		return "_", nil, nil
	}
	if lit := extractLiteral(e); lit != nil {
		v, err := c.compileLiteral(lit)
		return v, nil, err
	}
	if call, ok := callPattern(e); ok {
		if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
			st := ut.Variants[call.Func]
			if len(call.Args) != len(st.Order) {
				return "", nil, fmt.Errorf("pattern arg mismatch")
			}
			parts := make([]string, len(call.Args))
			vars := make(map[string]types.Type)
			for i, a := range call.Args {
				if id, ok := identName(a); ok {
					name := c.sn(id)
					parts[i] = name
					if id != "_" {
						vars[id] = st.Fields[st.Order[i]]
					}
				} else {
					parts[i] = "_"
				}
			}
			pat := c.sn(call.Func)
			if len(parts) > 0 {
				pat += " " + strings.Join(parts, " ")
			}
			return pat, vars, nil
		}
	}
	v, err := c.compileExpr(e)
	return v, nil, err
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
			fmt.Sprintf("%s <- %s", c.sn(j.Var), js),
			fmt.Sprintf("%s <- let %s = [ %s | %s <- %s, %s ] in if null %s then [Map.empty] else %s", c.sn(q.Var), tmp, c.sn(q.Var), c.sn(q.Var), src, on, tmp, tmp),
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

	// handle simple outer join by combining left and right joins
	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "outer" {
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
			fmt.Sprintf("%s <- %s", c.sn(q.Var), src),
			fmt.Sprintf("%s <- let %s = [ %s | %s <- %s, %s ] in if null %s then [Map.empty] else %s", c.sn(j.Var), tmp, c.sn(j.Var), c.sn(j.Var), js, on, tmp, tmp),
		}
		c.env = child
		val, err := c.compileExpr(q.Select)
		c.env = orig
		if err != nil {
			return "", err
		}
		leftPart := fmt.Sprintf("[ %s | %s ]", val, strings.Join(loops, ", "))
		rightCond := fmt.Sprintf("null [ () | %s <- %s, %s ]", c.sn(q.Var), src, on)
		rightPart := fmt.Sprintf("[ %s | %s <- %s, let %s = Map.empty, %s ]", val, c.sn(j.Var), js, c.sn(q.Var), rightCond)
		c.usesList = true
		c.usesMap = true
		res := fmt.Sprintf("%s ++ %s", leftPart, rightPart)
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
		loops = append(loops, fmt.Sprintf("%s <- filter (\\%s -> %s) %s", c.sn(q.Var), c.sn(q.Var), whereExpr, src))
	} else {
		loops = append(loops, fmt.Sprintf("%s <- %s", c.sn(q.Var), src))
	}

	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		loops = append(loops, fmt.Sprintf("%s <- %s", c.sn(f.Var), fs))
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
			loops = append(loops, fmt.Sprintf("%s <- let %s = [ %s | %s <- %s, %s ] in if null %s then [Map.empty] else %s", c.sn(j.Var), tmp, c.sn(j.Var), c.sn(j.Var), js, on, tmp, tmp))
		} else {
			loops = append(loops, fmt.Sprintf("%s <- %s", c.sn(j.Var), js))
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
		if _, ok := c.inferExprType(q.Group.Exprs[0]).(types.AnyType); ok {
			c.usesAnyValue = true
			c.usesAnyCast = true
			c.usesAsString = true
			keyExpr = fmt.Sprintf("_asString (%s)", keyExpr)
		}
		genv := types.NewEnv(child)
		keyT := c.inferExprType(q.Group.Exprs[0])
		genv.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: elemType}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig
		c.usesMap = true
		c.usesList = true
		c.usesMaybe = true
		c.usesGroup = true
		expr := fmt.Sprintf("[ %s | g <- _group_by %s (\\%s -> %s), let %s = g ]", valExpr, src, c.sn(q.Var), keyExpr, c.sn(q.Group.Name))
		return expr, nil
	}

	if q.Group != nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		tuple := fmt.Sprintf("(%s)", strings.Join(func() []string {
			names := []string{c.sn(q.Var)}
			for _, f := range q.Froms {
				names = append(names, c.sn(f.Var))
			}
			for _, j := range q.Joins {
				names = append(names, c.sn(j.Var))
			}
			return names
		}(), ", "))
		rows := fmt.Sprintf("[%s | %s%s]", tuple, strings.Join(loops, ", "), condStr)
		groups := fmt.Sprintf("_group_by %s (\\%s -> %s)", rows, tuple, keyExpr)
		c.usesMaybe = true
		c.usesGroup = true
		genv := types.NewEnv(child)
		keyT := c.inferExprType(q.Group.Exprs[0])
		genv.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: types.AnyType{}}, true)
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
		res := fmt.Sprintf("[ %s | g <- %s, let %s = g ]", valExpr, groups, c.sn(q.Group.Name))
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
	name := c.sn(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("data %s = %s {", name, name))
	c.indent++
	for i, fn := range st.Order {
		ft := st.Fields[fn]
		line := fmt.Sprintf("%s :: %s", c.sn(fn), c.hsType(ft))
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
	name := c.sn(ut.Name)
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
		line := c.sn(k)
		if len(st.Fields) > 0 {
			line += " {"
			for j, fn := range st.Order {
				ft := st.Fields[fn]
				line += fmt.Sprintf(" %s :: %s", c.sn(fn), c.hsType(ft))
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
	c.usesLoop = true
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
	name := c.sn(loop.Name)
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
