//go:build slow

package cscode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into C# source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	inFun        int
	env          *types.Env
	tempVarCount int
	helpers      map[string]bool
	useLinq      bool
	varTypes     map[string]string
	packages     map[string]bool
	structs      map[string]bool
	modules      map[string]bool
	handlerCount int
	useStream    bool
	models       bool
	structHint   string
	extraStructs []types.StructType
	structCount  int
	Namespace    string
	testArgs     map[string][]string
	// When true, compile anonymous struct literals as Dictionary values
	DictMode bool
	// internal flag to temporarily force struct literals to dictionaries
	structAsDict bool
}

// New creates a new C# compiler.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:          env,
		helpers:      make(map[string]bool),
		varTypes:     make(map[string]string),
		packages:     make(map[string]bool),
		structs:      make(map[string]bool),
		modules:      make(map[string]bool),
		inFun:        0,
		useStream:    false,
		models:       false,
		structHint:   "",
		extraStructs: nil,
		structCount:  0,
		Namespace:    "",
		testArgs:     make(map[string][]string),
		DictMode:     false,
		structAsDict: false,
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile converts prog into C# source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.helpers = make(map[string]bool)
	c.structs = make(map[string]bool)
	c.useStream = false
	c.handlerCount = 0
	c.scanProgram(prog)
	c.writeln("using System;")
	c.writeln("using System.Collections.Generic;")
	c.writeln("using System.IO;")
	c.writeln("using System.Net.Http;")
	c.writeln("using System.Text;")
	c.writeln("using System.Web;")
	if c.useLinq {
		c.writeln("using System.Linq;")
	}
	c.writeln("using System.Text.Json;")
	if c.helpers["_load"] || c.helpers["_save"] {
		c.writeln("using YamlDotNet.Serialization;")
	}
	if c.helpers["_eval"] {
		c.writeln("using System.Data;")
	}
	c.writeln("")
	ns := c.Namespace
	if ns == "" {
		ns = prog.Package
	}
	if ns != "" {
		c.writeln("namespace " + sanitizeName(ns) + " {")
		c.indent++
	}
	// Compile Mochi package imports
	for _, s := range prog.Statements {
		if s.Import != nil && s.Import.Lang == nil {
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			if err := c.compilePackageImport(alias, s.Import.Path, s.Pos.Filename); err != nil {
				return nil, err
			}
		}
	}

	// Detect model declarations
	for _, s := range prog.Statements {
		if s.Model != nil {
			c.models = true
			break
		}
	}

	// Type declarations
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Stream struct declarations
	for _, s := range prog.Statements {
		if s.Stream != nil {
			st, ok := c.env.GetStream(s.Stream.Name)
			if !ok {
				return nil, fmt.Errorf("unknown stream: %s", s.Stream.Name)
			}
			c.compileStructType(st)
			c.writeln("")
		}
	}

	// Foreign module stubs
	for _, s := range prog.Statements {
		if s.Import != nil && s.Import.Lang != nil {
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			lang := *s.Import.Lang
			p := strings.Trim(s.Import.Path, "\"")
			switch lang {
			case "go":
				if s.Import.Auto && p == "mochi/runtime/ffi/go/testpkg" {
					c.modules[alias] = true
					c.writeln(fmt.Sprintf("static class %s {", alias))
					c.indent++
					c.writeln("public static int Add(int a, int b) { return a + b; }")
					c.writeln("public const double Pi = 3.14;")
					c.writeln("public const int Answer = 42;")
					c.indent--
					c.writeln("}")
					c.writeln("")
				}
			case "python":
				if p == "math" {
					c.modules[alias] = true
					c.writeln(fmt.Sprintf("static class %s {", alias))
					c.indent++
					c.writeln("public const double pi = Math.PI;")
					c.writeln("public const double e = Math.E;")
					c.writeln("public static double sqrt(double x) { return Math.Sqrt(x); }")
					c.writeln("public static double pow(double x, double y) { return Math.Pow(x, y); }")
					c.writeln("public static double sin(double x) { return Math.Sin(x); }")
					c.writeln("public static double log(double x) { return Math.Log(x); }")
					c.indent--
					c.writeln("}")
					c.writeln("")
				}
			}
		}
	}

	c.writeln("class Program {")
	c.indent++
	if c.models {
		c.writeln("static Dictionary<string, Dictionary<string, object>> _models = new Dictionary<string, Dictionary<string, object>>();")
		c.writeln("")
	}

	// Function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Test block declarations
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Main method
	c.writeln("static void Main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil || s.Type != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			args := strings.Join(c.testArgs[name], ", ")
			c.writeln(fmt.Sprintf("%s(%s);", name, args))
		}
	}
	c.indent--
	c.writeln("}")
	ns = c.Namespace
	if ns == "" {
		ns = prog.Package
	}
	if ns != "" {
		c.indent--
		c.writeln("}")
	}
	for _, st := range c.extraStructs {
		c.compileClassType(st)
		c.writeln("")
	}
	c.emitRuntime()
	if c.useStream {
		c.writeln("class _Stream<T> {")
		c.indent++
		c.writeln("public string name;")
		c.writeln("public List<Action<T>> handlers = new List<Action<T>>();")
		c.writeln("public _Stream(string name) { this.name = name; }")
		c.writeln("public void Append(T data) {")
		c.indent++
		c.writeln("foreach (var h in new List<Action<T>>(handlers)) { h(data); }")
		c.indent--
		c.writeln("}")
		c.writeln("public void Register(Action<T> handler) { handlers.Add(handler); }")
		c.indent--
		c.writeln("}")
		c.writeln("static void _waitAll() {}")
	}
	c.indent--
	c.writeln("}")

	code := c.buf.Bytes()
	if c.useLinq && !bytes.Contains(code, []byte("using System.Linq;")) {
		code = bytes.Replace(code, []byte("using System.Collections.Generic;\n"), []byte("using System.Collections.Generic;\nusing System.Linq;\n"), 1)
	}
	src := string(code)
	if !strings.Contains(src, "JsonSerializer") {
		if _, ok := c.helpers["_cast"]; !ok {
			if _, ok := c.helpers["_genStruct"]; !ok {
				if _, ok := c.helpers["_fetch"]; !ok {
					code = bytes.Replace(code, []byte("using System.Text.Json;\n"), nil, 1)
				}
			}
		}
	}
	// Remove unused using statements for cleaner output
	src = string(code)
	if !strings.Contains(src, "List<") && !strings.Contains(src, "Dictionary<") {
		code = bytes.Replace(code, []byte("using System.Collections.Generic;\n"), nil, 1)
	}
	if !strings.Contains(src, "File.") && !strings.Contains(src, "Stream") {
		code = bytes.Replace(code, []byte("using System.IO;\n"), nil, 1)
	}
	if !strings.Contains(src, "HttpClient") {
		code = bytes.Replace(code, []byte("using System.Net.Http;\n"), nil, 1)
	}
	if !strings.Contains(src, "Encoding.") && !strings.Contains(src, "StringBuilder") {
		code = bytes.Replace(code, []byte("using System.Text;\n"), nil, 1)
	}
	if !strings.Contains(src, "HttpUtility") {
		code = bytes.Replace(code, []byte("using System.Web;\n"), nil, 1)
	}
	return FormatCS(code), nil
}

func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	origVars := c.varTypes
	c.varTypes = make(map[string]string)
	for k, v := range origVars {
		c.varTypes[k] = v
	}
	for i, p := range fn.Params {
		pType := csType(p.Type)
		name := sanitizeName(p.Name)
		c.varTypes[name] = pType
		params[i] = fmt.Sprintf("%s %s", pType, name)
	}
	ret := "void"
	if fn.Return != nil {
		ret = csType(fn.Return)
		if ret == "" {
			ret = "void"
		}
	}
	prefix := "static "
	if c.inFun > 0 {
		prefix = ""
	}
	c.writeln(fmt.Sprintf("%s%s %s(%s) {", prefix, ret, sanitizeName(fn.Name), strings.Join(params, ", ")))
	c.indent++
	c.inFun++
	for _, stmt := range fn.Body {
		if err := c.compileStmt(stmt); err != nil {
			return err
		}
	}
	c.inFun--
	c.indent--
	c.writeln("}")
	c.varTypes = origVars
	return nil
}

func (c *Compiler) compileTypeMethod(fn *parser.FunStmt) error {
	if fn.Doc != "" {
		for _, ln := range strings.Split(fn.Doc, "\n") {
			c.writeln("// " + ln)
		}
	}
	params := make([]string, len(fn.Params))
	origVars := c.varTypes
	c.varTypes = make(map[string]string)
	for k, v := range origVars {
		c.varTypes[k] = v
	}
	for i, p := range fn.Params {
		pType := csType(p.Type)
		name := sanitizeName(p.Name)
		c.varTypes[name] = pType
		params[i] = fmt.Sprintf("%s %s", pType, name)
	}
	ret := "void"
	if fn.Return != nil {
		ret = csType(fn.Return)
		if ret == "" {
			ret = "void"
		}
	}
	c.writeln(fmt.Sprintf("public %s %s(%s) {", ret, sanitizeName(fn.Name), strings.Join(params, ", ")))
	c.indent++
	c.inFun++
	for _, stmt := range fn.Body {
		if err := c.compileStmt(stmt); err != nil {
			return err
		}
	}
	c.inFun--
	c.indent--
	c.writeln("}")
	c.varTypes = origVars
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	aliasSet := map[string]struct{}{}
	for _, s := range t.Body {
		if s.Expect != nil {
			for v := range exprAliases(s.Expect.Value) {
				aliasSet[sanitizeName(v)] = struct{}{}
			}
		}
	}
	args := []string{}
	for v := range aliasSet {
		tname := c.varTypes[v]
		if tname == "" {
			tname = "dynamic"
		}
		args = append(args, fmt.Sprintf("%s %s", tname, v))
		c.testArgs[name] = append(c.testArgs[name], v)
	}
	c.writeln(fmt.Sprintf("static void %s(%s) {", name, strings.Join(args, ", ")))
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	c.helpers["_expect"] = true
	if e.Value != nil && e.Value.Binary != nil && len(e.Value.Binary.Right) == 1 {
		op := e.Value.Binary.Right[0].Op
		if op == "==" || op == "!=" {
			left, err := c.compileUnary(e.Value.Binary.Left)
			if err != nil {
				return err
			}
			right, err := c.compilePostfix(e.Value.Binary.Right[0].Right)
			if err != nil {
				return err
			}
			c.use("_equal")
			if op == "==" {
				c.writeln(fmt.Sprintf("expect(_equal(%s, %s));", left, right))
			} else {
				c.writeln(fmt.Sprintf("expect(!_equal(%s, %s));", left, right))
			}
			return nil
		}
	}
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("expect(%s);", expr))
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if t.Doc != "" {
		for _, ln := range strings.Split(t.Doc, "\n") {
			c.writeln("// " + ln)
		}
	}
	if len(t.Variants) > 0 {
		iface := fmt.Sprintf("public interface %s { void is%s(); }", name, name)
		c.writeln(iface)
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			c.writeln(fmt.Sprintf("public struct %s : %s {", vname, name))
			c.indent++
			for _, f := range v.Fields {
				typ := csType(f.Type)
				c.writeln(fmt.Sprintf("public %s %s;", typ, sanitizeName(f.Name)))
			}
			c.writeln(fmt.Sprintf("public void is%s() {}", name))
			c.indent--
			c.writeln("}")
		}
		return nil
	}
	kw := "struct"
	hasMethod := false
	for _, m := range t.Members {
		if m.Method != nil {
			hasMethod = true
			break
		}
	}
	if !hasMethod {
		kw = "record struct"
	}
	c.writeln(fmt.Sprintf("public %s %s {", kw, name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			typ := csType(m.Field.Type)
			c.writeln(fmt.Sprintf("public %s %s;", typ, sanitizeName(m.Field.Name)))
		}
	}
	for _, m := range t.Members {
		if m.Method != nil {
			if err := c.compileTypeMethod(m.Method); err != nil {
				return err
			}
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	kw := "struct"
	if len(st.Methods) == 0 {
		kw = "record struct"
	}
	c.writeln(fmt.Sprintf("public %s %s {", kw, name))
	c.indent++
	for _, fn := range st.Order {
		ft := st.Fields[fn]
		c.writeln(fmt.Sprintf("public %s %s;", csTypeOf(ft), sanitizeName(fn)))
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) compileClassType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("public class %s {", name))
	c.indent++
	for _, fn := range st.Order {
		ft := st.Fields[fn]
		c.writeln(fmt.Sprintf("public %s %s;", csTypeOf(ft), sanitizeName(fn)))
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileClassType(sub)
		}
	}
}

func (c *Compiler) scanProgram(prog *parser.Program) {
	var scanExpr func(e *parser.Expr)
	scanExpr = func(e *parser.Expr) {
		if e == nil || c.useLinq {
			return
		}
		u := e.Binary.Left
		if u != nil {
			p := u.Value
			if p.Target != nil && p.Target.Query != nil {
				c.useLinq = true
				q := p.Target.Query
				if q.Group != nil {
					for _, ge := range q.Group.Exprs {
						if isAnonStructExpr(ge) {
							c.DictMode = true
						}
					}
				}
				if isAnonStructExpr(q.Select) {
					c.DictMode = true
				}
				return
			}
			if p.Target != nil && p.Target.Call != nil && p.Target.Call.Func == "eval" {
				c.helpers["_eval"] = true
			}
		}
		for _, r := range e.Binary.Right {
			if r.Right != nil {
				if r.Right.Target != nil && r.Right.Target.Query != nil {
					c.useLinq = true
					q := r.Right.Target.Query
					if q.Group != nil {
						for _, ge := range q.Group.Exprs {
							if isAnonStructExpr(ge) {
								c.DictMode = true
							}
						}
					}
					if isAnonStructExpr(q.Select) {
						c.DictMode = true
					}
					return
				}
				if r.Right.Target != nil && r.Right.Target.Call != nil && r.Right.Target.Call.Func == "eval" {
					c.helpers["_eval"] = true
				}
			}
		}
	}

	for _, s := range prog.Statements {
		if s.Let != nil {
			scanExpr(s.Let.Value)
		}
		if s.Var != nil {
			scanExpr(s.Var.Value)
		}
		if s.Expr != nil {
			scanExpr(s.Expr.Expr)
		}
		if s.Return != nil {
			scanExpr(s.Return.Value)
		}
	}
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := sanitizeName(s.Let.Name)
		var typ string
		var expr string
		var static types.Type = types.AnyType{}
		if s.Let.Value != nil {
			var err error
			if s.Let.Type != nil {
				c.structHint = singular(name)
				expr, err = c.compileExpr(s.Let.Value)
				c.structHint = ""
				if err != nil {
					return err
				}
				typ = csType(s.Let.Type)
				static = c.resolveTypeRef(s.Let.Type)
				if isEmptyListLiteral(s.Let.Value) {
					if strings.HasPrefix(typ, "List<") {
						expr = fmt.Sprintf("new %s()", typ)
					} else {
						expr = fmt.Sprintf("new %s { }", typ)
					}
				}
				if isFetchExpr(s.Let.Value) && typ != "" {
					c.use("_cast")
					expr = fmt.Sprintf("_cast<%s>(%s)", typ, expr)
				}
			} else {
				inferredT := c.inferExprType(s.Let.Value)
				if !isGroupQuery(s.Let.Value) {
					inferredT = c.assignTypeNames(inferredT, singular(name))
					c.registerStructs(inferredT)
				}
				typ = csTypeOf(inferredT)
				static = inferredT
				c.structHint = singular(name)
				expr, err = c.compileExpr(s.Let.Value)
				c.structHint = ""
				if err != nil {
					return err
				}
				if isEmptyListLiteral(s.Let.Value) && (strings.HasSuffix(typ, "[]") || strings.HasPrefix(typ, "List<")) {
					if strings.HasPrefix(typ, "List<") {
						expr = fmt.Sprintf("new %s()", typ)
					} else {
						expr = fmt.Sprintf("new %s { }", typ)
					}
				}
				if isFetchExpr(s.Let.Value) && typ != "" {
					c.use("_cast")
					expr = fmt.Sprintf("_cast<%s>(%s)", typ, expr)
				}
			}
		} else {
			if s.Let.Type != nil {
				typ = csType(s.Let.Type)
				static = c.resolveTypeRef(s.Let.Type)
				expr = "default"
			} else {
				typ = "dynamic"
				expr = "null"
			}
		}
		if c.DictMode {
			switch st := static.(type) {
			case types.StructType:
				static = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
			case types.ListType:
				if _, ok := st.Elem.(types.StructType); ok {
					st.Elem = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
					static = st
				}
			}
		}
		c.env.SetVar(s.Let.Name, static, false)
		c.varTypes[name] = typ
		decl := "var"
		if typ != "" && !strings.Contains(typ, "dynamic") {
			decl = typ
		}
		c.writeln(fmt.Sprintf("%s %s = %s;", decl, name, expr))
	case s.Var != nil:
		name := sanitizeName(s.Var.Name)
		var typ string
		var expr string
		var static types.Type = types.AnyType{}
		if s.Var.Value != nil {
			var err error
			if s.Var.Type != nil {
				c.structHint = singular(name)
				expr, err = c.compileExpr(s.Var.Value)
				c.structHint = ""
				if err != nil {
					return err
				}
				typ = csType(s.Var.Type)
				static = c.resolveTypeRef(s.Var.Type)
				if isEmptyListLiteral(s.Var.Value) {
					if strings.HasPrefix(typ, "List<") {
						expr = fmt.Sprintf("new %s()", typ)
					} else {
						expr = fmt.Sprintf("new %s { }", typ)
					}
				}
				if isFetchExpr(s.Var.Value) && typ != "" {
					c.use("_cast")
					expr = fmt.Sprintf("_cast<%s>(%s)", typ, expr)
				}
			} else {
				inferredT := c.inferExprType(s.Var.Value)
				if !isGroupQuery(s.Var.Value) {
					inferredT = c.assignTypeNames(inferredT, singular(name))
					c.registerStructs(inferredT)
				}
				typ = csTypeOf(inferredT)
				static = inferredT
				c.structHint = singular(name)
				expr, err = c.compileExpr(s.Var.Value)
				c.structHint = ""
				if err != nil {
					return err
				}
				if isEmptyListLiteral(s.Var.Value) && (strings.HasSuffix(typ, "[]") || strings.HasPrefix(typ, "List<")) {
					if strings.HasPrefix(typ, "List<") {
						expr = fmt.Sprintf("new %s()", typ)
					} else {
						expr = fmt.Sprintf("new %s { }", typ)
					}
				}
				if isFetchExpr(s.Var.Value) && typ != "" {
					c.use("_cast")
					expr = fmt.Sprintf("_cast<%s>(%s)", typ, expr)
				}
			}
		} else {
			if s.Var.Type != nil {
				typ = csType(s.Var.Type)
				static = c.resolveTypeRef(s.Var.Type)
				expr = "default"
			} else {
				typ = "dynamic"
				expr = "null"
			}
		}
		if c.DictMode {
			switch st := static.(type) {
			case types.StructType:
				static = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
			case types.ListType:
				if _, ok := st.Elem.(types.StructType); ok {
					st.Elem = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
					static = st
				}
			}
		}
		c.env.SetVar(s.Var.Name, static, true)
		c.varTypes[name] = typ
		decl := "var"
		if typ != "" && !strings.Contains(typ, "dynamic") {
			decl = typ
		}
		c.writeln(fmt.Sprintf("%s %s = %s;", decl, name, expr))
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr + ";")
	case s.Assign != nil:
		if err := c.compileAssign(s.Assign); err != nil {
			return err
		}
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
	case s.Fun != nil:
		if err := c.compileFunStmt(s.Fun); err != nil {
			return err
		}
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.Agent != nil:
		return c.compileAgentDecl(s.Agent)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Model != nil:
		return c.compileModelDecl(s.Model)
	case s.Import != nil:
		if s.Import.Lang == nil {
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			return c.compilePackageImport(alias, s.Import.Path, s.Pos.Filename)
		}
		// foreign imports handled at top level
		return nil
	default:
		// ignore other statements in minimal compiler
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	useVar := name != "_"
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		loopVar := name
		if !useVar {
			loopVar = c.newVar()
		}
		c.writeln(fmt.Sprintf("for (var %s = %s; %s < %s; %s++) {", loopVar, start, loopVar, end, loopVar))
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
	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	loopVar := name
	if !useVar {
		loopVar = c.newVar()
	}
	t := c.inferExprType(f.Source)
	var elemType types.Type = types.AnyType{}
	switch tt := t.(type) {
	case types.ListType:
		elemType = tt.Elem
	case types.MapType:
		elemType = tt.Key
	case types.StringType:
		elemType = types.StringType{}
	}
	if _, ok := t.(types.MapType); ok {
		c.writeln(fmt.Sprintf("foreach (var %s in %s.Keys) {", loopVar, src))
	} else {
		c.writeln(fmt.Sprintf("foreach (var %s in %s) {", loopVar, src))
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	child.SetVar(f.Name, elemType, true)
	c.env = child
	origVars := c.varTypes
	c.varTypes = make(map[string]string)
	for k, v := range origVars {
		c.varTypes[k] = v
	}
	c.varTypes[name] = csTypeOf(elemType)
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = origEnv
			c.varTypes = origVars
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.env = origEnv
	c.varTypes = origVars
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	lhs := sanitizeName(a.Name)
	for _, idx := range a.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	for _, fld := range a.Field {
		lhs = fmt.Sprintf("%s.%s", lhs, sanitizeName(fld.Name))
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	inferred := csTypeOf(c.inferExprType(a.Value))
	cur, ok := c.varTypes[sanitizeName(a.Name)]
	if !ok || cur == "dynamic" || cur == "dynamic[]" {
		c.varTypes[sanitizeName(a.Name)] = inferred
	}
	if ok && cur != "" && cur != "dynamic" && isFetchExpr(a.Value) {
		c.use("_cast")
		val = fmt.Sprintf("_cast<%s>(%s)", cur, val)
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, val))
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	var rec func(s *parser.IfStmt, prefix string) error
	rec = func(s *parser.IfStmt, prefix string) error {
		cond, err := c.compileExpr(s.Cond)
		if err != nil {
			return err
		}
		cond = stripParens(cond)
		c.writeln(prefix + "if (" + cond + ") {")
		c.indent++
		for _, st := range s.Then {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		if s.ElseIf != nil {
			return rec(s.ElseIf, "} else ")
		}
		if len(s.Else) > 0 {
			c.writeln("} else {")
			c.indent++
			for _, st := range s.Else {
				if err := c.compileStmt(st); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln("}")
		} else {
			c.writeln("}")
		}
		return nil
	}
	return rec(stmt, "")
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	cond = stripParens(cond)
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

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	idx := c.newVar()
	c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s.Length; %s++) {", idx, idx, list, idx))
	c.indent++
	item := c.newVar()
	c.writeln(fmt.Sprintf("var %s = %s[%s];", item, list, idx))

	var st types.StructType
	origEnv := c.env
	if c.env != nil {
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}
	if st.Name != "" {
		child := types.NewEnv(c.env)
		for _, fn := range st.Order {
			child.SetVar(fn, st.Fields[fn], true)
			c.writeln(fmt.Sprintf("var %s = %s.%s;", sanitizeName(fn), item, sanitizeName(fn)))
		}
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln("if (" + cond + ") {")
		c.indent++
	}
	for _, it := range u.Set.Items {
		if st.Name != "" {
			if key, ok := identName(it.Key); ok {
				val, err := c.compileExpr(it.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				c.writeln(fmt.Sprintf("%s.%s = %s;", item, sanitizeName(key), val))
				continue
			}
		}
		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.env = origEnv
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("%s[%s] = %s;", item, keyExpr, valExpr))
	}
	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.env = origEnv
	c.writeln(fmt.Sprintf("%s[%s] = %s;", list, idx, item))
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIfExpr(e *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(e.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(e.Then)
	if err != nil {
		return "", err
	}
	elseExpr := "null"
	if e.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(e.ElseIf)
		if err != nil {
			return "", err
		}
	} else if e.Else != nil {
		elseExpr, err = c.compileExpr(e.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", fmt.Errorf("nil binary expr")
	}

	operands := []string{}
	lists := []bool{}
	strs := []bool{}
	typs := []types.Type{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, first)
	lists = append(lists, c.isListUnary(b.Left))
	strs = append(strs, c.isStringUnary(b.Left))
	typs = append(typs, c.inferUnaryType(b.Left))

	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		lists = append(lists, c.isListPostfix(part.Right))
		strs = append(strs, c.isStringPostfix(part.Right))
		typs = append(typs, c.inferPostfixType(part.Right))
		op := part.Op
		if part.All {
			op = op + "_all"
		}
		ops = append(ops, op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(sl []string, s string) bool {
		for _, v := range sl {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if !contains(lvl, ops[i]) {
				i++
				continue
			}
			op := ops[i]
			left := operands[i]
			right := operands[i+1]
			leftList := lists[i]
			rightList := lists[i+1]
			leftStr := strs[i]

			var expr string
			switch op {
			case "+":
				if leftList && rightList {
					c.useLinq = true
					expr = fmt.Sprintf("%s.Concat(%s).ToList()", left, right)
					leftList = true
					leftStr = false
				} else if strs[i] && strs[i+1] {
					expr = fmt.Sprintf("string.Concat(%s, %s)", left, right)
					leftList = false
					leftStr = true
				} else {
					expr = fmt.Sprintf("(%s + %s)", left, right)
					leftList = false
					leftStr = false
				}
				leftList = leftList && rightList
				strs[i] = leftStr
			case "==", "!=":
				leftStr = false
				complex := leftList && rightList || isMapType(typs[i]) || isStructType(typs[i]) || isUnionType(typs[i]) || isGroupType(typs[i]) || isMapType(typs[i+1]) || isStructType(typs[i+1]) || isUnionType(typs[i+1]) || isGroupType(typs[i+1])
				if complex {
					c.use("_equal")
					if op == "==" {
						expr = fmt.Sprintf("_equal(%s, %s)", left, right)
					} else {
						expr = fmt.Sprintf("!_equal(%s, %s)", left, right)
					}
				} else {
					expr = fmt.Sprintf("(%s %s %s)", left, op, right)
				}
			case "<", "<=", ">", ">=":
				leftStr = false
				if isStringType(typs[i]) && isStringType(typs[i+1]) {
					cmp := fmt.Sprintf("string.Compare(%s, %s)", left, right)
					switch op {
					case "<":
						expr = fmt.Sprintf("%s < 0", cmp)
					case "<=":
						expr = fmt.Sprintf("%s <= 0", cmp)
					case ">":
						expr = fmt.Sprintf("%s > 0", cmp)
					case ">=":
						expr = fmt.Sprintf("%s >= 0", cmp)
					}
				} else {
					expr = fmt.Sprintf("(%s %s %s)", left, op, right)
				}
			case "in":
				leftStr = false
				lt := typs[i]
				rt := typs[i+1]
				if isListType(rt) {
					c.useLinq = true
					expr = fmt.Sprintf("%s.Contains(%s)", right, left)
				} else if isMapType(rt) {
					expr = fmt.Sprintf("%s.ContainsKey(%s)", right, left)
				} else if isStringType(rt) && isStringType(lt) {
					expr = fmt.Sprintf("%s.Contains(%s)", right, left)
				} else {
					c.use("_in")
					expr = fmt.Sprintf("_in(%s, %s)", left, right)
				}
				lists[i] = false
				typs[i] = types.BoolType{}
			case "union_all":
				leftStr = false
				c.useLinq = true
				expr = fmt.Sprintf("Enumerable.Concat(%s, %s).ToList()", left, right)
				leftList = true
			case "union":
				leftStr = false
				c.useLinq = true
				expr = fmt.Sprintf("Enumerable.Union(%s, %s).ToList()", left, right)
				leftList = true
			case "except":
				leftStr = false
				c.useLinq = true
				expr = fmt.Sprintf("Enumerable.Except(%s, %s).ToList()", left, right)
				leftList = true
			case "intersect":
				leftStr = false
				c.useLinq = true
				expr = fmt.Sprintf("Enumerable.Intersect(%s, %s).ToList()", left, right)
				leftList = true
			default:
				leftStr = false
				expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			}

			operands[i] = expr
			lists[i] = leftList
			strs[i] = leftStr
			typs[i] = resultType(op, typs[i], typs[i+1])
			operands = append(operands[:i+1], operands[i+2:]...)
			lists = append(lists[:i+1], lists[i+2:]...)
			strs = append(strs[:i+1], strs[i+2:]...)
			typs = append(typs[:i+1], typs[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state in binary expr")
	}
	return operands[0], nil
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
	isStr := c.isStringPostfix(p)
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
			argStr := strings.Join(args, ", ")
			if strings.HasSuffix(expr, ".contains") {
				base := strings.TrimSuffix(expr, ".contains")
				if len(args) != 1 {
					return "", fmt.Errorf("contains expects 1 arg")
				}
				expr = fmt.Sprintf("Convert.ToString(%s).Contains(%s)", base, args[0])
			} else if expr == "print" {
				expr = fmt.Sprintf("Console.WriteLine(%s)", argStr)
			} else if expr == "len" {
				if len(args) != 1 {
					return "", fmt.Errorf("len() expects 1 arg")
				}
				expr = fmt.Sprintf("%s.Length", args[0])
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, argStr)
			}
			continue
		}
		if op.Index != nil {
			start := "0"
			if op.Index.Start != nil {
				v, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				start = v
			}
			if op.Index.Colon != nil {
				end := fmt.Sprintf("%s.Length", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if isStr {
					c.use("_sliceString")
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				} else {
					c.use("_sliceList")
					expr = fmt.Sprintf("_sliceList(%s, %s, %s)", expr, start, end)
				}
			} else {
				idx := start
				if isStr {
					c.use("_indexString")
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					var preType types.Type
					if c.env != nil {
						prefix := &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:i]}
						preType = c.inferPostfixType(prefix)
					}
					if _, ok := preType.(types.MapType); ok {
						expr = fmt.Sprintf("%s[%s]", expr, idx)
					} else {
						c.use("_indexList")
						expr = fmt.Sprintf("_indexList(%s, %s)", expr, idx)
					}
				}
			}
			isStr = false
			continue
		}
		if op.Cast != nil {
			typ := csType(op.Cast.Type)
			c.use("_cast")
			expr = fmt.Sprintf("_cast<%s>(%s)", typ, expr)
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	c.useLinq = true
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	stype := c.inferExprType(q.Source)
	if gt, ok := stype.(types.GroupType); ok {
		src = fmt.Sprintf("%s.Items", src)
		stype = types.ListType{Elem: gt.Elem}
	}
	resultT := c.inferExprType(q.Select)
	resultType := csTypeOf(resultT)
	if st, ok := resultT.(types.StructType); ok && st.Name == "" && q.Group == nil {
		base := c.structHint
		if base == "" {
			base = "Item"
		}
		st.Name = c.newStructName(base)
		resultType = st.Name
		resultT = st
		c.extraStructs = append(c.extraStructs, st)
	} else if lt, ok := resultT.(types.ListType); ok && q.Group == nil {
		if st, ok2 := lt.Elem.(types.StructType); ok2 && st.Name == "" {
			base := c.structHint
			if base == "" {
				base = "Item"
			}
			st.Name = c.newStructName(base)
			lt.Elem = st
			resultType = fmt.Sprintf("List<%s>", st.Name)
			resultT = types.ListType{Elem: st}
			c.extraStructs = append(c.extraStructs, st)
		}
	}
	orig := c.env
	child := types.NewEnv(c.env)
	var elemT types.Type = types.AnyType{}
	if lt, ok := stype.(types.ListType); ok {
		elemT = lt.Elem
	}
	elemType := csTypeOf(elemT)
	child.SetVar(q.Var, elemT, true)
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	aliasFilters := map[string][]string{}
	if q.Where != nil {
		parts := splitAnd(q.Where)
		other := []string{}
		for _, pexpr := range parts {
			aliases := exprAliases(pexpr)
			exprStr, err := c.compileExpr(pexpr)
			if err != nil {
				c.env = orig
				return "", err
			}
			if len(aliases) == 1 {
				for a := range aliases {
					aliasFilters[sanitizeName(a)] = append(aliasFilters[sanitizeName(a)], exprStr)
				}
			} else {
				other = append(other, exprStr)
			}
		}
		if len(other) > 0 {
			cond = strings.Join(other, " && ")
		} else if flt, ok := aliasFilters[sanitizeName(q.Var)]; ok {
			cond = strings.Join(flt, " && ")
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	v := sanitizeName(q.Var)

	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 {
		genv := types.NewEnv(child)
		var elemT types.Type = types.AnyType{}
		if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
			elemT = lt.Elem
		}
		keyT := c.assignTypeNames(c.inferExprType(q.Group.Exprs[0]), "Key")
		c.registerStructs(keyT)
		elemT = c.assignTypeNames(elemT, "Item").(types.Type)
		c.registerStructs(elemT)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: elemT}, true)
		c.env = genv
		origHint := c.structHint
		c.structHint = "Key"
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		c.structHint = origHint
		if err != nil {
			c.env = orig
			return "", err
		}
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		var sortGroup string
		var havingExpr string
		if q.Sort != nil {
			sortGroup, err = c.compileExpr(q.Sort)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		if q.Group.Having != nil {
			havingExpr, err = c.compileExpr(q.Group.Having)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		if q.Skip != nil {
			skipExpr, err = c.compileExpr(q.Skip)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		if q.Take != nil {
			takeExpr, err = c.compileExpr(q.Take)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyTypeStr := csTypeOf(keyT)
		elemTypeStr := csTypeOf(elemT)
		c.env = orig

		srcParts := []string{src}
		if cond != "" {
			srcParts = append(srcParts, fmt.Sprintf("Where(%s => %s)", v, cond))
		}
		filtered := strings.Join(srcParts, ".")
		c.use("_group")
		c.use("_group_by")
		expr := fmt.Sprintf("_group_by<%s, %s>(%s, %s => %s)", elemTypeStr, keyTypeStr, filtered, v, keyExpr)
		if havingExpr != "" {
			expr = fmt.Sprintf("%s.Where(%s => %s)", expr, sanitizeName(q.Group.Name), havingExpr)
		}
		if sortGroup != "" {
			expr = fmt.Sprintf("%s.OrderBy(%s => %s)", expr, sanitizeName(q.Group.Name), sortGroup)
		}
		expr = fmt.Sprintf("%s.Select(%s => %s)", expr, sanitizeName(q.Group.Name), valExpr)
		if skipExpr != "" {
			expr = fmt.Sprintf("%s.Skip(%s)", expr, skipExpr)
		}
		if takeExpr != "" {
			expr = fmt.Sprintf("%s.Take(%s)", expr, takeExpr)
		}
		return fmt.Sprintf("%s.ToList()", expr), nil
	}

	// handle cross/join using nested loops when q.Froms or q.Joins are present
	if len(q.Froms) > 0 || len(q.Joins) > 0 {
		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			fromSrcs[i] = fs
			var ft types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(f.Src).(types.ListType); ok {
				ft = lt.Elem
			}
			child.SetVar(f.Var, ft, true)
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		joinSides := make([]string, len(q.Joins))
		joinTypes := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinSrcs[i] = js
			if j.Side != nil {
				joinSides[i] = *j.Side
			}
			var jt types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(j.Src).(types.ListType); ok {
				jt = lt.Elem
			}
			joinTypes[i] = csTypeOf(jt)
			child.SetVar(j.Var, jt, true)
			c.env = child
			onExpr, err := c.compileExpr(j.On)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinOns[i] = onExpr
		}
		c.env = child

		if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil && joinSides[0] == "" {
			on := q.Joins[0].On
			if on != nil && on.Binary != nil && len(on.Binary.Right) == 1 && on.Binary.Right[0].Op == "==" {
				leftKey, err := c.compileUnary(on.Binary.Left)
				if err != nil {
					c.env = orig
					return "", err
				}
				rightKey, err := c.compilePostfix(on.Binary.Right[0].Right)
				if err != nil {
					c.env = orig
					return "", err
				}
				sel, err := c.compileExpr(q.Select)
				if err != nil {
					c.env = orig
					return "", err
				}
				c.env = orig
				expr := fmt.Sprintf("%s.Join(%s, %s => %s, %s => %s, (%s, %s) => %s)", src, joinSrcs[0], v, leftKey, sanitizeName(q.Joins[0].Var), rightKey, v, sanitizeName(q.Joins[0].Var), sel)
				if cond != "" {
					expr = fmt.Sprintf("%s.Where(%s => %s)", expr, v, cond)
				}
				if sortExpr != "" {
					expr = fmt.Sprintf("%s.OrderBy(%s => %s)", expr, v, sortExpr)
				}
				if skipExpr != "" {
					expr = fmt.Sprintf("%s.Skip(%s)", expr, skipExpr)
				}
				if takeExpr != "" {
					expr = fmt.Sprintf("%s.Take(%s)", expr, takeExpr)
				}
				return fmt.Sprintf("%s.ToList()", expr), nil
			}
		}

		// format query using LINQ comprehension when joins are simple
		if q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
			simple := true
			joinPairs := make([][2]string, len(q.Joins))
			for i, j := range q.Joins {
				if joinSides[i] != "" {
					simple = false
					break
				}
				on := j.On
				if on == nil || on.Binary == nil || len(on.Binary.Right) != 1 || on.Binary.Right[0].Op != "==" {
					simple = false
					break
				}
				lk, err := c.compileUnary(on.Binary.Left)
				if err != nil {
					c.env = orig
					return "", err
				}
				rk, err := c.compilePostfix(on.Binary.Right[0].Right)
				if err != nil {
					c.env = orig
					return "", err
				}
				joinPairs[i] = [2]string{lk, rk}
			}
			if simple {
				var buf strings.Builder
				buf.WriteString("(\n")
				buf.WriteString(fmt.Sprintf("\tfrom %s in %s\n", v, src))
				for i, f := range q.Froms {
					buf.WriteString(fmt.Sprintf("\tfrom %s in %s\n", sanitizeName(f.Var), fromSrcs[i]))
				}
				for i, j := range q.Joins {
					buf.WriteString(fmt.Sprintf("\tjoin %s in %s on %s equals %s\n", sanitizeName(j.Var), joinSrcs[i], joinPairs[i][0], joinPairs[i][1]))
				}
				if cond != "" {
					buf.WriteString(fmt.Sprintf("\twhere %s\n", cond))
				}
				buf.WriteString(fmt.Sprintf("\tselect %s\n)", sel))
				if _, ok := c.inferExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Query: q}}}}}).(types.ListType); ok {
					buf.WriteString(".ToList()")
				}
				c.env = orig
				return buf.String(), nil
			}
		}

		if q.Group != nil {
			keyT := c.assignTypeNames(c.inferExprType(q.Group.Exprs[0]), "Key")
			c.registerStructs(keyT)
			c.structHint = "Key"
			keyExpr, err := c.compileExpr(q.Group.Exprs[0])
			c.structHint = ""
			if err != nil {
				c.env = orig
				return "", err
			}
			genv := types.NewEnv(child)
			var groupT types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
				groupT = lt.Elem
			}
			groupT = c.assignTypeNames(groupT, "Item").(types.Type)
			c.registerStructs(groupT)
			genv.SetVar(q.Group.Name, groupT, true)
			c.env = genv
			valExpr, err := c.compileExpr(q.Select)
			if err != nil {
				c.env = orig
				return "", err
			}
			var sortGroup string
			if q.Sort != nil {
				sortGroup, err = c.compileExpr(q.Sort)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			if q.Skip != nil {
				skipExpr, err = c.compileExpr(q.Skip)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			if q.Take != nil {
				takeExpr, err = c.compileExpr(q.Take)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			keyTypeStr := csTypeOf(keyT)
			elemTypeStr := csTypeOf(groupT)
			c.env = orig

			var buf strings.Builder
			buf.WriteString(fmt.Sprintf("new Func<List<%s>>(() => {\n", resultType))
			buf.WriteString(fmt.Sprintf("\tvar groups = new Dictionary<string, _Group<%s, %s>>();\n", keyTypeStr, elemTypeStr))
			buf.WriteString("\tvar order = new List<string>();\n")
			buf.WriteString(fmt.Sprintf("\tforeach (var %s in %s) {\n", v, src))
			indent := "\t\t"
			if flt, ok := aliasFilters[v]; ok {
				for _, fcond := range flt {
					buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", fcond))
				}
			}
			for i, f := range q.Froms {
				buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", sanitizeName(f.Var), fromSrcs[i]))
				indent += "\t"
				if flt, ok := aliasFilters[sanitizeName(f.Var)]; ok {
					for _, fcond := range flt {
						buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", fcond))
					}
				}
			}
			for i := range q.Joins {
				buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", sanitizeName(q.Joins[i].Var), joinSrcs[i]))
				indent += "\t"
				buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", joinOns[i]))
				if flt, ok := aliasFilters[sanitizeName(q.Joins[i].Var)]; ok {
					for _, fcond := range flt {
						buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", fcond))
					}
				}
			}
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(fmt.Sprintf(indent+"var key = %s;\n", keyExpr))
			buf.WriteString(indent + "var ks = Convert.ToString(key);\n")
			buf.WriteString(indent + "if (!groups.TryGetValue(ks, out var g)) {\n")
			buf.WriteString(fmt.Sprintf(indent+"\tg = new _Group<%s, %s>(key);\n", keyTypeStr, elemTypeStr))
			buf.WriteString(indent + "\tgroups[ks] = g;\n")
			buf.WriteString(indent + "\torder.Add(ks);\n")
			buf.WriteString(indent + "}\n")
			buf.WriteString(fmt.Sprintf(indent+"g.Items.Add(%s);\n", v))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			for i := len(q.Joins) - 1; i >= 0; i-- {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			for i := len(q.Froms) - 1; i >= 0; i-- {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")

			buf.WriteString(fmt.Sprintf("\tvar items = new List<_Group<%s, %s>>();\n", keyTypeStr, elemTypeStr))
			buf.WriteString("\tforeach (var ks in order) items.Add(groups[ks]);\n")
			if sortGroup != "" {
				buf.WriteString(fmt.Sprintf("\titems = items.OrderBy(%s => %s).ToList();\n", sanitizeName(q.Group.Name), sortGroup))
			}
			if skipExpr != "" {
				buf.WriteString(fmt.Sprintf("\titems = items.Skip(%s).ToList();\n", skipExpr))
			}
			if takeExpr != "" {
				buf.WriteString(fmt.Sprintf("\titems = items.Take(%s).ToList();\n", takeExpr))
			}
			buf.WriteString(fmt.Sprintf("\tvar _res = new List<%s>();\n", resultType))
			buf.WriteString(fmt.Sprintf("\tforeach (var %s in items) {\n", sanitizeName(q.Group.Name)))
			buf.WriteString(fmt.Sprintf("\t\t_res.Add(%s);\n", valExpr))
			buf.WriteString("\t}\n")
			buf.WriteString("\treturn _res;\n")
			buf.WriteString("})()")
			c.use("_group")
			return buf.String(), nil
		}

		// existing join-only implementation
		sel, err = c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig

		var buf strings.Builder
		buf.WriteString(fmt.Sprintf("new Func<List<%s>>(() => {\n", resultType))
		buf.WriteString(fmt.Sprintf("\tvar _res = new List<%s>();\n", resultType))
		buf.WriteString(fmt.Sprintf("\tforeach (var %s in %s) {\n", v, src))
		indent := "\t\t"
		if flt, ok := aliasFilters[v]; ok {
			for _, fcond := range flt {
				buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", fcond))
			}
		}
		for i, f := range q.Froms {
			buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", sanitizeName(f.Var), fromSrcs[i]))
			indent += "\t"
			if flt, ok := aliasFilters[sanitizeName(f.Var)]; ok {
				for _, fcond := range flt {
					buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", fcond))
				}
			}
		}
		specialLeft := len(q.Joins) == 1 && joinSides[0] == "left"
		specialRight := len(q.Joins) == 1 && joinSides[0] == "right" && len(q.Froms) == 0
		specialOuter := len(q.Joins) == 1 && joinSides[0] == "outer" && len(q.Froms) == 0
		if specialLeft {
			buf.WriteString(indent + "bool _matched = false;\n")
			buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", sanitizeName(q.Joins[0].Var), joinSrcs[0]))
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", joinOns[0]))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(indent + "_matched = true;\n")
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			buf.WriteString(indent + "if (!_matched) {\n")
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"%s %s = default;\n", joinTypes[0], sanitizeName(q.Joins[0].Var)))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		} else if specialRight {
			buf.WriteString(indent + "bool _matched = false;\n")
			buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", sanitizeName(q.Joins[0].Var), joinSrcs[0]))
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", v, src))
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", joinOns[0]))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(indent + "_matched = true;\n")
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			for i := len(q.Froms) - 1; i >= 0; i-- {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			buf.WriteString(indent + "if (!_matched) {\n")
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"%s %s = default;\n", elemType, sanitizeName(v)))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		} else if specialOuter {
			buf.WriteString(fmt.Sprintf(indent+"var _joinItems = new List<%s>(%s);\n", joinTypes[0], joinSrcs[0]))
			buf.WriteString(indent + "var _matched = new bool[_joinItems.Count];\n")
			buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", v, src))
			indent += "\t"
			buf.WriteString(indent + "bool _m = false;\n")
			buf.WriteString(indent + "for (int i = 0; i < _joinItems.Count; i++) {\n")
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"var %s = _joinItems[i];\n", sanitizeName(q.Joins[0].Var)))
			buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", joinOns[0]))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(indent + "_m = true;\n")
			buf.WriteString(indent + "_matched[i] = true;\n")
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			buf.WriteString(indent + "if (!_m) {\n")
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"%s %s = default;\n", joinTypes[0], sanitizeName(q.Joins[0].Var)))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			buf.WriteString(indent + "for (int i = 0; i < _joinItems.Count; i++) {\n")
			indent += "\t"
			buf.WriteString(indent + "if (!_matched[i]) {\n")
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"%s %s = default;\n", elemType, sanitizeName(v)))
			buf.WriteString(fmt.Sprintf(indent+"var %s = _joinItems[i];\n", sanitizeName(q.Joins[0].Var)))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		} else {
			for i, j := range q.Joins {
				buf.WriteString(fmt.Sprintf(indent+"foreach (var %s in %s) {\n", sanitizeName(j.Var), joinSrcs[i]))
				indent += "\t"
				buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", joinOns[i]))
				if flt, ok := aliasFilters[sanitizeName(j.Var)]; ok {
					for _, fcond := range flt {
						buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) continue;\n", fcond))
					}
				}
			}
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
				indent += "\t"
			}
			buf.WriteString(fmt.Sprintf(indent+"_res.Add(%s);\n", sel))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			for i := len(q.Joins) - 1; i >= 0; i-- {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
		}
		for i := len(q.Froms) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
		buf.WriteString("\t}\n")
		if sortExpr != "" {
			buf.WriteString(fmt.Sprintf("\t_res = _res.OrderBy(%s => %s).ToList();\n", v, sortExpr))
		}
		if skipExpr != "" {
			buf.WriteString(fmt.Sprintf("\t_res = _res.Skip(%s).ToList();\n", skipExpr))
		}
		if takeExpr != "" {
			buf.WriteString(fmt.Sprintf("\t_res = _res.Take(%s).ToList();\n", takeExpr))
		}
		buf.WriteString("\treturn _res;\n")
		buf.WriteString("})()")
		return buf.String(), nil
	}

	c.env = orig

	parts := []string{src}
	if cond != "" {
		parts = append(parts, fmt.Sprintf("Where(%s => %s)", v, cond))
	}
	if sortExpr != "" {
		parts = append(parts, fmt.Sprintf("OrderBy(%s => %s)", v, sortExpr))
	}
	if skipExpr != "" {
		parts = append(parts, fmt.Sprintf("Skip(%s)", skipExpr))
	}
	if takeExpr != "" {
		parts = append(parts, fmt.Sprintf("Take(%s)", takeExpr))
	}
	parts = append(parts, fmt.Sprintf("Select(%s => %s)", v, sel))
	expr := strings.Join(parts, ".")
	if _, ok := c.inferExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Query: q}}}}}).(types.ListType); ok {
		expr += ".ToList()"
	}
	return expr, nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	retType := csTypeOf(c.inferPrimaryType(&parser.Primary{Match: m}))
	var buf strings.Builder
	buf.WriteString(fmt.Sprintf("new Func<%s>(() => {\n", retType))
	buf.WriteString("\t\tvar _t = " + target + ";\n")
	for _, cs := range m.Cases {
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				varName := c.newVar()
				buf.WriteString(fmt.Sprintf("\t\tif (_t is %s %s) {\n", sanitizeName(call.Func), varName))
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						field := sanitizeName(st.Order[idx])
						buf.WriteString(fmt.Sprintf("\t\t\tvar %s = %s.%s;\n", sanitizeName(id), varName, field))
					}
				}
				res, err := c.compileExpr(cs.Result)
				if err != nil {
					return "", err
				}
				buf.WriteString("\t\t\treturn " + res + ";\n")
				buf.WriteString("\t\t}\n")
				continue
			}
		}
		if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				res, err := c.compileExpr(cs.Result)
				if err != nil {
					return "", err
				}
				buf.WriteString(fmt.Sprintf("\t\tif (_t is %s) return %s;\n", sanitizeName(ident), res))
				continue
			}
		}
		if isUnderscoreExpr(cs.Pattern) {
			res, err := c.compileExpr(cs.Result)
			if err != nil {
				return "", err
			}
			buf.WriteString("\t\treturn " + res + ";\n")
			buf.WriteString("\t})()")
			return buf.String(), nil
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		c.use("_equal")
		buf.WriteString(fmt.Sprintf("\t\tif (_equal(_t, %s)) return %s;\n", pat, res))
	}
	if retType == "dynamic" {
		buf.WriteString("\t\treturn null;\n")
	} else {
		buf.WriteString("\t\treturn default;\n")
	}
	buf.WriteString("\t})()")
	return buf.String(), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var prompt, text, model string
	params := []string{}
	for _, f := range g.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		switch f.Name {
		case "prompt":
			prompt = v
		case "text":
			text = v
		case "model":
			model = v
		default:
			params = append(params, fmt.Sprintf("{ \"%s\", %s }", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "null"
	if len(params) > 0 {
		paramStr = "new Dictionary<string, object> { " + strings.Join(params, ", ") + " }"
	}
	if model == "" {
		model = "null"
	}
	if g.Target == "embedding" {
		c.use("_genEmbed")
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramStr), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_genStruct")
			return fmt.Sprintf("_genStruct<%s>(%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
		}
	}
	c.use("_genText")
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "null"
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		c.use("_cast")
		opts = fmt.Sprintf("_cast<Dictionary<string, object>>(%s)", w)
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, opts), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "null"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_load")
	c.useLinq = true
	expr := fmt.Sprintf("_load(%s, %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		typ := csType(l.Type)
		if typ != "" {
			c.use("_cast")
			expr = fmt.Sprintf("%s.Select(e => _cast<%s>(e)).ToList()", expr, typ)
		}
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "null"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_save")
	c.useLinq = true
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	varName := "_" + sanitizeName(s.Name) + "Stream"
	c.writeln(fmt.Sprintf("var %s = new _Stream<%s>(%q);", varName, sanitizeName(st.Name), s.Name))
	c.useStream = true
	return nil
}

func (c *Compiler) compileOnHandler(h *parser.OnHandler) error {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", h.Stream)
	}
	streamVar := "_" + sanitizeName(h.Stream) + "Stream"
	handlerName := fmt.Sprintf("_handler_%d", c.handlerCount)
	c.handlerCount++
	c.writeln(fmt.Sprintf("void %s(%s ev) {", handlerName, sanitizeName(st.Name)))
	c.indent++
	if h.Alias != "_" {
		alias := sanitizeName(h.Alias)
		c.writeln(fmt.Sprintf("var %s = ev;", alias))
	}
	child := types.NewEnv(c.env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return err
		}
	}
	c.env = orig
	c.indent--
	c.writeln("}")
	c.writeln(fmt.Sprintf("%s.Register(%s);", streamVar, handlerName))
	c.useStream = true
	return nil
}

func (c *Compiler) compileEmit(e *parser.EmitStmt) error {
	st, ok := c.env.GetStream(e.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", e.Stream)
	}
	parts := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
	}
	lit := fmt.Sprintf("new %s { %s }", sanitizeName(st.Name), strings.Join(parts, ", "))
	streamVar := "_" + sanitizeName(e.Stream) + "Stream"
	c.writeln(fmt.Sprintf("%s.Append(%s);", streamVar, lit))
	c.useStream = true
	return nil
}

func (c *Compiler) compileModelDecl(m *parser.ModelDecl) error {
	c.models = true
	parts := make([]string, len(m.Fields))
	for i, f := range m.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("{ \"%s\", %s }", f.Name, v)
	}
	c.writeln(fmt.Sprintf("_models[%q] = new Dictionary<string, object> { %s };", m.Name, strings.Join(parts, ", ")))
	return nil
}

func (c *Compiler) compileAgentDecl(a *parser.AgentDecl) error {
	st, ok := c.env.GetStruct(a.Name)
	if !ok {
		return fmt.Errorf("unknown agent: %s", a.Name)
	}
	name := sanitizeName(a.Name)
	baseEnv := types.NewEnv(c.env)
	for _, fn := range st.Order {
		baseEnv.SetVar(fn, st.Fields[fn], true)
	}

	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	c.use("_agent")
	c.writeln("public _Agent Agent;")
	for _, fn := range st.Order {
		typ := csTypeOf(st.Fields[fn])
		if typ == "" {
			typ = "dynamic"
		}
		c.writeln(fmt.Sprintf("public %s %s;", typ, sanitizeName(fn)))
	}
	c.writeln(fmt.Sprintf("public %s() {", name))
	c.indent++
	c.writeln(fmt.Sprintf("Agent = new _Agent(%q);", a.Name))
	orig := c.env
	c.env = baseEnv
	for _, blk := range a.Body {
		switch {
		case blk.Let != nil:
			val := "null"
			if blk.Let.Value != nil {
				v, err := c.compileExpr(blk.Let.Value)
				if err != nil {
					c.env = orig
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(blk.Let.Name), val))
		case blk.Var != nil:
			val := "null"
			if blk.Var.Value != nil {
				v, err := c.compileExpr(blk.Var.Value)
				if err != nil {
					c.env = orig
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(blk.Var.Name), val))
		}
	}
	c.env = orig
	handlerID := 0
	for _, blk := range a.Body {
		if blk.On != nil {
			streamVar := "_" + sanitizeName(blk.On.Stream) + "Stream"
			c.writeln(fmt.Sprintf("Agent.On(%s, _on%d);", streamVar, handlerID))
			handlerID++
		}
	}
	for _, blk := range a.Body {
		if blk.Intent != nil {
			mname := sanitizeName(blk.Intent.Name)
			c.writeln(fmt.Sprintf("Agent.RegisterIntent(%q, %s);", blk.Intent.Name, mname))
		}
	}
	c.writeln("Agent.Start();")
	c.indent--
	c.writeln("}")

	handlerID = 0
	for _, blk := range a.Body {
		switch {
		case blk.Intent != nil:
			if err := c.compileAgentIntent(name, baseEnv, blk.Intent); err != nil {
				return err
			}
		case blk.On != nil:
			if _, err := c.compileAgentOn(name, baseEnv, blk.On, handlerID); err != nil {
				return err
			}
			handlerID++
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.writeln(fmt.Sprintf("static %s New%s() {", name, name))
	c.indent++
	c.writeln(fmt.Sprintf("return new %s();", name))
	c.indent--
	c.writeln("}")
	c.writeln("")
	return nil
}

func (c *Compiler) compileAgentIntent(agentName string, env *types.Env, in *parser.IntentDecl) error {
	st, _ := c.env.GetStruct(agentName)
	ft := st.Methods[in.Name].Type
	name := sanitizeName(in.Name)
	params := make([]string, len(in.Params))
	child := types.NewEnv(env)
	for i, p := range in.Params {
		typ := "dynamic"
		if i < len(ft.Params) {
			typ = csTypeOf(ft.Params[i])
		}
		pname := sanitizeName(p.Name)
		params[i] = fmt.Sprintf("%s %s", typ, pname)
		child.SetVar(p.Name, ft.Params[i], true)
	}
	ret := "dynamic"
	if rt := csTypeOf(ft.Return); rt != "" {
		ret = rt
	}
	c.writeln(fmt.Sprintf("public %s %s(%s) {", ret, name, strings.Join(params, ", ")))
	orig := c.env
	c.env = child
	c.indent++
	for _, s := range in.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = orig
			return err
		}
	}
	c.indent--
	c.env = orig
	c.writeln("}")
	c.writeln("")
	return nil
}

func (c *Compiler) compileAgentOn(agentName string, env *types.Env, h *parser.OnHandler, id int) (string, error) {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return "", fmt.Errorf("unknown stream: %s", h.Stream)
	}
	fname := fmt.Sprintf("_on%d", id)
	c.writeln(fmt.Sprintf("void %s(%s ev) {", fname, sanitizeName(st.Name)))
	alias := sanitizeName(h.Alias)
	child := types.NewEnv(env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	c.indent++
	c.writeln(fmt.Sprintf("var %s = ev;", alias))
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return "", err
		}
	}
	c.indent--
	c.env = orig
	c.writeln("}")
	c.writeln("")
	return fname, nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	orig := c.varTypes
	c.varTypes = make(map[string]string)
	for k, v := range orig {
		c.varTypes[k] = v
	}

	paramDecls := make([]string, len(fn.Params))
	paramTypes := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := csType(p.Type)
		name := sanitizeName(p.Name)
		c.varTypes[name] = typ
		paramDecls[i] = fmt.Sprintf("%s %s", typ, name)
		paramTypes[i] = typ
	}
	ret := csType(fn.Return)
	if ret == "" {
		if fn.Return == nil && fn.ExprBody != nil {
			ret = csTypeOf(c.inferExprType(fn.ExprBody))
			if ret == "" {
				ret = "dynamic"
			}
		} else {
			ret = "dynamic"
		}
	}

	generics := append(append([]string{}, paramTypes...), ret)
	delegate := fmt.Sprintf("Func<%s>", strings.Join(generics, ", "))

	var body strings.Builder
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.varTypes = orig
			return "", err
		}
		body.WriteString("return " + expr + ";")
	} else {
		sub := &Compiler{env: c.env, helpers: c.helpers, varTypes: c.varTypes}
		sub.indent = 1
		sub.inFun = c.inFun + 1
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				c.varTypes = orig
				return "", err
			}
		}
		body.WriteString(strings.TrimSuffix(sub.buf.String(), "\n"))
	}

	lines := strings.Split(body.String(), "\n")
	for i, l := range lines {
		lines[i] = "\t" + l
	}
	formatted := strings.Join(lines, "\n")

	result := fmt.Sprintf("new %s((%s) => {\n%s\n})", delegate, strings.Join(paramDecls, ", "), formatted)
	c.varTypes = orig
	return result, nil
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			if c.DictMode || c.structAsDict {
				parts[i] = fmt.Sprintf("{ \"%s\", %s }", f.Name, v)
			} else {
				parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
			}
		}
		if c.DictMode || c.structAsDict {
			return fmt.Sprintf("new Dictionary<string, dynamic> { %s }", strings.Join(parts, ", ")), nil
		}
		return fmt.Sprintf("new %s { %s }", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.List != nil:
		t := c.inferPrimaryType(p)
		elemType := "dynamic"
		if lt, ok := t.(types.ListType); ok {
			if st, ok2 := lt.Elem.(types.StructType); ok2 && st.Name == "" && !c.DictMode {
				base := c.structHint
				if base == "" {
					base = "Item"
				}
				if def, ok := c.env.GetStruct(pascalCase(base)); ok {
					st.Name = def.Name
				} else if name, ok := c.findStructByFields(st); ok {
					st.Name = name
				} else {
					st.Name = c.newStructName(base)
					c.extraStructs = append(c.extraStructs, st)
				}
				lt.Elem = st
			}
			elemType = csTypeOf(lt.Elem)
		}
		if len(p.List.Elems) == 0 {
			return fmt.Sprintf("new List<%s>()", elemType), nil
		}
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("new List<%s> { %s }", elemType, strings.Join(elems, ", ")), nil
	case p.Map != nil:
		t := c.inferPrimaryType(p)
		if st, ok := t.(types.StructType); ok && !c.DictMode {
			name := st.Name
			if name == "" {
				base := c.structHint
				if base == "" {
					base = "Item"
				}
				if def, ok := c.env.GetStruct(pascalCase(base)); ok {
					name = def.Name
					st.Name = name
				} else if n, ok := c.findStructByFields(st); ok {
					name = n
					st.Name = n
				} else {
					name = c.newStructName(base)
					st.Name = name
					c.extraStructs = append(c.extraStructs, st)
				}
			}
			items := make([]string, len(p.Map.Items))
			for i, it := range p.Map.Items {
				field := ""
				if n, ok := selectorName(it.Key); ok {
					field = sanitizeName(n)
				} else {
					field = sanitizeName(fmt.Sprintf("f%d", i))
				}
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				items[i] = fmt.Sprintf("%s = %s", field, v)
			}
			return fmt.Sprintf("new %s { %s }", name, strings.Join(items, ", ")), nil
		}
		keyType := "dynamic"
		valType := "dynamic"
		if mt, ok := t.(types.MapType); ok {
			keyType = csTypeOf(mt.Key)
			valType = csTypeOf(mt.Value)
		}
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if name, ok := selectorName(it.Key); ok {
				k = fmt.Sprintf("%q", name)
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("{ %s, %s }", k, v)
		}
		return fmt.Sprintf("new Dictionary<%s, %s> { %s }", keyType, valType, strings.Join(items, ", ")), nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		if c.modules[p.Selector.Root] {
			if len(p.Selector.Tail) == 0 {
				return expr, nil
			}
			parts := make([]string, len(p.Selector.Tail))
			for i, s := range p.Selector.Tail {
				parts[i] = sanitizeName(s)
			}
			return expr + "." + strings.Join(parts, "."), nil
		}
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		if len(p.Selector.Tail) > 0 {
			if gt, ok := typ.(types.GroupType); ok {
				field := p.Selector.Tail[0]
				if field == "key" {
					expr = fmt.Sprintf("%s.key", expr)
					typ = types.AnyType{}
				} else if field == "items" {
					expr = fmt.Sprintf("%s", expr)
					typ = types.ListType{Elem: gt.Elem}
					expr += ".Items"
				} else {
					expr = fmt.Sprintf("%s[%q]", expr, field)
					typ = types.AnyType{}
				}
				for _, f := range p.Selector.Tail[1:] {
					expr += "." + sanitizeName(f)
					if st, ok := typ.(types.StructType); ok {
						if ft, ok := st.Fields[f]; ok {
							typ = ft
						} else {
							typ = types.AnyType{}
						}
					} else {
						typ = types.AnyType{}
					}
				}
				return expr, nil
			}
			if mt, ok := typ.(types.MapType); ok {
				key := p.Selector.Tail[0]
				expr = fmt.Sprintf("%s[%q]", expr, key)
				typ = mt.Value
				for _, f := range p.Selector.Tail[1:] {
					expr += "." + sanitizeName(f)
					if st, ok := typ.(types.StructType); ok {
						if ft, ok := st.Fields[f]; ok {
							typ = ft
						} else {
							typ = types.AnyType{}
						}
					} else {
						typ = types.AnyType{}
					}
				}
				return expr, nil
			}
			if _, ok := typ.(types.AnyType); ok {
				expr = fmt.Sprintf("%s[%q]", expr, p.Selector.Tail[0])
				for _, f := range p.Selector.Tail[1:] {
					expr += fmt.Sprintf("[%q]", f)
				}
				return expr, nil
			}
		}
		if ut, ok := typ.(types.UnionType); ok && len(p.Selector.Tail) > 0 {
			field := p.Selector.Tail[0]
			var variant string
			var ftyp types.Type
			for name, st := range ut.Variants {
				if ft, ok := st.Fields[field]; ok {
					if variant != "" {
						variant = ""
						break
					}
					variant = name
					ftyp = ft
				}
			}
			if variant != "" {
				expr = fmt.Sprintf("((%s)%s).%s", sanitizeName(variant), expr, sanitizeName(field))
				typ = ftyp
				for _, f := range p.Selector.Tail[1:] {
					expr += "." + sanitizeName(f)
					if st, ok := typ.(types.StructType); ok {
						if ft, ok := st.Fields[f]; ok {
							typ = ft
						} else {
							typ = types.AnyType{}
						}
					} else {
						typ = types.AnyType{}
					}
				}
				return expr, nil
			}
		}
		for _, s := range p.Selector.Tail {
			expr += "." + sanitizeName(s)
		}
		return expr, nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Group != nil:
		e, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	default:
		return "", fmt.Errorf("unsupported primary expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	var paramTypes []types.Type
	var retType types.Type = types.VoidType{}
	if fn, ok := c.env.GetFunc(call.Func); ok {
		paramTypes = make([]types.Type, len(fn.Params))
		for i, p := range fn.Params {
			if p.Type != nil {
				paramTypes[i] = c.resolveTypeRef(p.Type)
			} else {
				paramTypes[i] = types.AnyType{}
			}
		}
		if fn.Return != nil {
			retType = c.resolveTypeRef(fn.Return)
		}
	} else if t, err := c.env.GetVar(call.Func); err == nil {
		if ft, ok := t.(types.FuncType); ok {
			paramTypes = ft.Params
			retType = ft.Return
		}
	}
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")

	if len(paramTypes) > 0 && len(args) < len(paramTypes) {
		missing := paramTypes[len(args):]
		names := make([]string, len(missing))
		for i := range missing {
			names[i] = fmt.Sprintf("p%d", i)
		}
		callArgs := append(append([]string{}, args...), names...)
		ret := csTypeOf(retType)
		if ret == "" {
			ret = "void"
		}
		ptypes := make([]string, len(missing))
		pdecls := make([]string, len(missing))
		for i, pt := range missing {
			ptStr := csTypeOf(pt)
			ptypes[i] = ptStr
			pdecls[i] = fmt.Sprintf("%s %s", ptStr, names[i])
		}
		del := delegateType(ptypes, ret)
		body := fmt.Sprintf("%s(%s)", sanitizeName(call.Func), strings.Join(callArgs, ", "))
		if ret == "void" {
			body += ";"
		} else {
			body = "return " + body + ";"
		}
		expr := fmt.Sprintf("new %s((%s) => { %s })", del, strings.Join(pdecls, ", "), body)
		return expr, nil
	}
	switch call.Func {
	case "print":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			if isListType(t) {
				if lt, ok := t.(types.ListType); ok && isPrimitiveType(lt.Elem) {
					c.useLinq = true
					return fmt.Sprintf("Console.WriteLine(\"[\" + string.Join(\", \", %s) + \"]\")", args[0]), nil
				}
				return fmt.Sprintf("Console.WriteLine(JsonSerializer.Serialize(%s))", args[0]), nil
			}
			if isMapType(t) || isStructType(t) || isGroupType(t) || isUnionType(t) {
				return fmt.Sprintf("Console.WriteLine(JsonSerializer.Serialize(%s))", args[0]), nil
			}
			return fmt.Sprintf("Console.WriteLine(%s)", args[0]), nil
		}
		parts := make([]string, len(args))
		for i, a := range args {
			parts[i] = fmt.Sprintf("Convert.ToString(%s)", a)
		}
		joined := "string.Join(\" \", new [] { " + strings.Join(parts, ", ") + " })"
		return fmt.Sprintf("Console.WriteLine(%s)", joined), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count() expects 1 arg")
		}
		t := c.inferExprType(call.Args[0])
		if _, ok := t.(types.StringType); ok {
			return fmt.Sprintf("%s.Length", args[0]), nil
		}
		switch t.(type) {
		case types.ListType, types.MapType:
			c.useLinq = true
			return fmt.Sprintf("%s.Count()", args[0]), nil
		default:
			c.useLinq = true
			return fmt.Sprintf("Enumerable.Count(%s)", args[0]), nil
		}
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		elem := "dynamic"
		if lt, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
			elem = csTypeOf(lt.Elem)
		}
		return fmt.Sprintf("new List<%s>(%s){%s}", elem, args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		val := "dynamic"
		if mt, ok := c.inferExprType(call.Args[0]).(types.MapType); ok {
			val = csTypeOf(mt.Value)
			c.useLinq = true
			return fmt.Sprintf("%s.Values.ToList()", args[0]), nil
		}
		tmp := c.newVar()
		c.useLinq = true
		return fmt.Sprintf("(new Func<List<%s>>(() => {var %s=new List<%s>();foreach(System.Collections.DictionaryEntry kv in %s){%s.Add(kv.Value);}return %s;}))()", val, tmp, val, args[0], tmp, tmp), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists() expects 1 arg")
		}
		t := c.inferExprType(call.Args[0])
		switch t.(type) {
		case types.StringType:
			return fmt.Sprintf("%s.Length > 0", args[0]), nil
		case types.ListType, types.MapType:
			c.useLinq = true
			return fmt.Sprintf("Enumerable.Any(%s)", args[0]), nil
		default:
			return fmt.Sprintf("%s != null", args[0]), nil
		}
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg() expects 1 arg")
		}
		t := c.inferExprType(call.Args[0])
		if lt, ok := t.(types.ListType); ok && isNumeric(lt.Elem) {
			c.useLinq = true
			return fmt.Sprintf("Enumerable.Average(%s)", args[0]), nil
		}
		c.use("_avg")
		return fmt.Sprintf("_avg(%s)", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum() expects 1 arg")
		}
		t := c.inferExprType(call.Args[0])
		if lt, ok := t.(types.ListType); ok && isNumeric(lt.Elem) {
			c.useLinq = true
			return fmt.Sprintf("Enumerable.Sum(%s)", args[0]), nil
		}
		c.use("_sum")
		expr := fmt.Sprintf("_sum(%s)", args[0])
		if lt, ok := t.(types.ListType); ok {
			if isInt(lt.Elem) {
				expr = fmt.Sprintf("(int)%s", expr)
			} else if isInt64(lt.Elem) {
				expr = fmt.Sprintf("(long)%s", expr)
			}
		} else if gt, ok := t.(types.GroupType); ok {
			if isInt(gt.Elem) {
				expr = fmt.Sprintf("(int)%s", expr)
			} else if isInt64(gt.Elem) {
				expr = fmt.Sprintf("(long)%s", expr)
			}
		}
		return expr, nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min() expects 1 arg")
		}
		c.useLinq = true
		return fmt.Sprintf("Enumerable.Min(%s)", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max() expects 1 arg")
		}
		c.useLinq = true
		return fmt.Sprintf("Enumerable.Max(%s)", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str() expects 1 arg")
		}
		return fmt.Sprintf("Convert.ToString(%s)", args[0]), nil
	case "upper":
		if len(args) != 1 {
			return "", fmt.Errorf("upper() expects 1 arg")
		}
		return fmt.Sprintf("Convert.ToString(%s).ToUpper()", args[0]), nil
	case "lower":
		if len(args) != 1 {
			return "", fmt.Errorf("lower() expects 1 arg")
		}
		return fmt.Sprintf("Convert.ToString(%s).ToLower()", args[0]), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input() expects no args")
		}
		return "Console.ReadLine()", nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len() expects 1 arg")
		}
		t := c.inferExprType(call.Args[0])
		if _, ok := t.(types.MapType); ok {
			return fmt.Sprintf("%s.Count", args[0]), nil
		}
		return fmt.Sprintf("%s.Length", args[0]), nil
	case "now":
		if len(args) != 0 {
			return "", fmt.Errorf("now() expects no args")
		}
		return "DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() * 1000000", nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json() expects 1 arg")
		}
		return fmt.Sprintf("Console.WriteLine(JsonSerializer.Serialize(%s))", args[0]), nil
	case "substr":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		c.use("_sliceString")
		end := fmt.Sprintf("(%s)+(%s)", args[1], args[2])
		return fmt.Sprintf("_sliceString(%s, %s, %s)", args[0], args[1], end), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		c.use("_sliceString")
		return fmt.Sprintf("_sliceString(%s, %s, %s)", args[0], args[1], args[2]), nil
	case "eval":
		if len(args) != 1 {
			return "", fmt.Errorf("eval expects 1 arg")
		}
		c.use("_eval")
		return fmt.Sprintf("_eval(%s)", args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	case l.Null:
		return "null", nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}

func csType(t *parser.TypeRef) string {
	if t == nil {
		return "dynamic"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = csType(p)
		}
		ret := csType(t.Fun.Return)
		return delegateType(params, ret)
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "double"
		case "string":
			return "string"
		case "bool":
			return "bool"
		case "any":
			return "dynamic"
		case "void":
			return "void"
		default:
			return sanitizeName(*t.Simple)
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return fmt.Sprintf("List<%s>", csType(t.Generic.Args[0]))
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return fmt.Sprintf("Dictionary<%s, %s>", csType(t.Generic.Args[0]), csType(t.Generic.Args[1]))
		}
		args := make([]string, len(t.Generic.Args))
		for i, a := range t.Generic.Args {
			args[i] = csType(a)
		}
		return fmt.Sprintf("%s<%s>", sanitizeName(t.Generic.Name), strings.Join(args, ", "))
	}
	return "dynamic"
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	return s
}

func stripParens(s string) string {
	for strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		s = strings.TrimPrefix(s, "(")
		s = strings.TrimSuffix(s, ")")
	}
	return s
}

func (c *Compiler) varIsList(name string) bool {
	typ, ok := c.varTypes[name]
	if !ok {
		return false
	}
	return strings.HasSuffix(typ, "[]") || strings.HasPrefix(typ, "List<")
}

func (c *Compiler) varIsString(name string) bool {
	typ, ok := c.varTypes[name]
	return ok && typ == "string"
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return c.isListPostfix(u.Value)
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.List != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		name := sanitizeName(p.Target.Selector.Root)
		if c.varIsList(name) {
			return true
		}
	}
	if p.Target.Call != nil {
		if t, err := c.env.GetVar(p.Target.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if isListType(ft.Return) {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		name := sanitizeName(p.Target.Selector.Root)
		if c.varIsString(name) {
			return true
		}
		if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) newVar() string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	return name
}

func (c *Compiler) use(name string) {
	if c.helpers == nil {
		c.helpers = make(map[string]bool)
	}
	c.helpers[name] = true
}

// isListLiteral reports whether the expression is a direct list literal.
func isListLiteral(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	return p.Target != nil && p.Target.List != nil
}

func isEmptyListLiteral(e *parser.Expr) bool {
	if !isListLiteral(e) {
		return false
	}
	u := e.Binary.Left
	p := u.Value
	return len(p.Target.List.Elems) == 0
}

// isStringExpr reports whether the expression is a simple string literal or
// a call to the built-in str() conversion.
func isStringExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Call != nil && p.Target.Call.Func == "str" {
		return true
	}
	return false
}

// selectorName returns the root identifier if the expression is a simple selector.
func selectorName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if p == nil || p.Target == nil || p.Target.Selector == nil {
		return "", false
	}
	if len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func isGroupQuery(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	u := e.Binary.Left
	if u == nil {
		return false
	}
	p := u.Value
	if p == nil {
		return false
	}
	if p.Target != nil && p.Target.Query != nil && p.Target.Query.Group != nil {
		return true
	}
	return false
}

// listElemType attempts to infer the element type of a list literal.
// It returns an empty string if the type can't be determined.
func listElemType(e *parser.Expr) string {
	if e == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return ""
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.List == nil {
		return ""
	}
	list := p.Target.List
	if len(list.Elems) == 0 {
		return ""
	}
	first := list.Elems[0]
	if first == nil || len(first.Binary.Right) != 0 {
		return ""
	}
	pu := first.Binary.Left
	if pu == nil {
		return ""
	}
	if len(pu.Ops) > 1 {
		return ""
	}
	if len(pu.Ops) == 1 && pu.Ops[0] != "-" && pu.Ops[0] != "+" {
		return ""
	}
	pv := pu.Value
	if pv == nil || len(pv.Ops) != 0 || pv.Target == nil || pv.Target.Lit == nil {
		return ""
	}
	lit := pv.Target.Lit
	switch {
	case lit.Int != nil:
		return "int"
	case lit.Float != nil:
		return "double"
	case lit.Str != nil:
		return "string"
	case lit.Bool != nil:
		return "bool"
	default:
		return ""
	}
}

func isListType(t types.Type) bool {
	switch t.(type) {
	case types.ListType:
		return true
	}
	return false
}

func isMapType(t types.Type) bool {
	_, ok := t.(types.MapType)
	return ok
}

func isStringType(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isAnonStructExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil {
		return false
	}
	if p.Target.Struct != nil {
		return p.Target.Struct.Name == ""
	}
	if p.Target.Map != nil {
		return true
	}
	return false
}

func isStructType(t types.Type) bool { _, ok := t.(types.StructType); return ok }

func isUnionType(t types.Type) bool { _, ok := t.(types.UnionType); return ok }

func isGroupType(t types.Type) bool { _, ok := t.(types.GroupType); return ok }

func isPrimitiveType(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType, types.StringType, types.BoolType:
		return true
	}
	return false
}

func (c *Compiler) compilePackageImport(alias, path, filename string) error {
	if c.packages[alias] {
		return nil
	}
	c.packages[alias] = true
	p := strings.Trim(path, "\"")
	base := ""
	if strings.HasPrefix(p, "./") || strings.HasPrefix(p, "../") {
		base = filepath.Dir(filename)
	}
	target := filepath.Join(base, p)
	info, err := os.Stat(target)
	if err != nil {
		if os.IsNotExist(err) {
			root := repoRoot()
			if root != "" {
				if fi, err2 := os.Stat(filepath.Join(root, p)); err2 == nil {
					info = fi
					target = filepath.Join(root, p)
					err = nil
				}
			}
		}
		if err != nil {
			if os.IsNotExist(err) && !strings.HasSuffix(target, ".mochi") {
				if fi, err2 := os.Stat(target + ".mochi"); err2 == nil {
					info = fi
					target += ".mochi"
				} else {
					return fmt.Errorf("import package: %w", err)
				}
			} else {
				return fmt.Errorf("import package: %w", err)
			}
		}
	}

	var files []string
	if info.IsDir() {
		entries, err := os.ReadDir(target)
		if err != nil {
			return fmt.Errorf("import package: %w", err)
		}
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
				files = append(files, filepath.Join(target, e.Name()))
			}
		}
		sort.Strings(files)
	} else {
		files = []string{target}
	}

	pkgEnv := types.NewEnv(c.env)
	origEnv := c.env
	c.env = pkgEnv

	c.writeln(fmt.Sprintf("public static class %s {", sanitizeName(alias)))
	c.indent++
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			c.env = origEnv
			return err
		}
		if errs := types.Check(prog, pkgEnv); len(errs) > 0 {
			c.env = origEnv
			return errs[0]
		}
		for _, s := range prog.Statements {
			if s.Type != nil {
				if err := c.compileTypeDecl(s.Type); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
		for _, s := range prog.Statements {
			if s.Fun != nil && s.Fun.Export {
				if err := c.compileFunStmt(s.Fun); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.env = origEnv
	return nil
}
