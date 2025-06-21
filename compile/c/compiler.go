package ccode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

var cReserved = map[string]bool{
	"auto": true, "break": true, "case": true, "char": true, "const": true,
	"continue": true, "default": true, "do": true, "double": true, "else": true,
	"enum": true, "extern": true, "float": true, "for": true, "goto": true,
	"if": true, "int": true, "long": true, "register": true, "return": true,
	"short": true, "signed": true, "sizeof": true, "static": true, "struct": true,
	"switch": true, "typedef": true, "union": true, "unsigned": true, "void": true,
	"volatile": true, "while": true,
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
	if cReserved[s] {
		s = "_" + s
	}
	return s
}

type Compiler struct {
	buf                      bytes.Buffer
	indent                   int
	tmp                      int
	env                      *types.Env
	lambdas                  []string
	needsStr                 bool
	needsInput               bool
	needsIndexString         bool
	needsStrLen              bool
	needsSliceListInt        bool
	needsSliceString         bool
	needsListListInt         bool
	needsConcatListListInt   bool
	needsConcatListInt       bool
	needsListString          bool
	needsConcatListString    bool
	needsConcatString        bool
	needsCount               bool
	needsAvg                 bool
	needsInListInt           bool
	needsInListString        bool
	needsSliceListFloat      bool
	needsSliceListString     bool
	needsListFloat           bool
	needsConcatListFloat     bool
	needsUnionListFloat      bool
	needsExceptListFloat     bool
	needsIntersectListFloat  bool
	needsInListFloat         bool
	needsUnionListInt        bool
	needsUnionListString     bool
	needsExceptListInt       bool
	needsExceptListString    bool
	needsIntersectListInt    bool
	needsIntersectListString bool
	externs                  []string
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, lambdas: []string{}}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) newTemp() string {
	c.tmp++
	return fmt.Sprintf("_t%d", c.tmp)
}

func (c *Compiler) cType(t *parser.TypeRef) string {
	if t == nil {
		return "void"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "double"
		case "bool":
			return "int"
		case "string":
			return "char*"
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" {
		if len(t.Generic.Args) == 1 {
			elem := c.cType(t.Generic.Args[0])
			if elem == "int" {
				return "list_int"
			}
			if elem == "double" {
				return "list_float"
			}
			if elem == "char*" {
				return "list_string"
			}
			if elem == "list_int" {
				return "list_list_int"
			}
		}
	}
	return "int"
}

func (c *Compiler) compileProgram(prog *parser.Program) ([]byte, error) {
	// compile body first to know which helpers are needed
	oldBuf := c.buf
	c.buf = bytes.Buffer{}
	c.externs = nil
	c.lambdas = nil
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.ExternVar != nil {
			typ := c.cType(s.ExternVar.Type)
			name := sanitizeName(s.ExternVar.Name())
			c.externs = append(c.externs, fmt.Sprintf("extern %s %s;", typ, name))
			if c.env != nil {
				c.env.SetVar(s.ExternVar.Name(), resolveTypeRef(s.ExternVar.Type, c.env), true)
			}
		} else if s.ExternFun != nil {
			ret := c.cType(s.ExternFun.Return)
			params := make([]string, len(s.ExternFun.Params))
			var ptypes []types.Type
			for i, p := range s.ExternFun.Params {
				params[i] = fmt.Sprintf("%s %s", c.cType(p.Type), sanitizeName(p.Name))
				if c.env != nil {
					ptypes = append(ptypes, resolveTypeRef(p.Type, c.env))
				}
			}
			fname := sanitizeName(s.ExternFun.Name())
			c.externs = append(c.externs, fmt.Sprintf("extern %s %s(%s);", ret, fname, strings.Join(params, ", ")))
			if c.env != nil {
				ft := types.FuncType{Params: ptypes, Return: resolveTypeRef(s.ExternFun.Return, c.env)}
				c.env.SetVar(s.ExternFun.Name(), ft, true)
			}
		}
	}
	// main function
	c.writeln("int main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun == nil && s.Test == nil {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(name + "();")
		}
	}
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")
	body := c.buf.String()
	if len(c.lambdas) > 0 {
		body = strings.Join(c.lambdas, "\n") + "\n" + body
	}
	c.buf = oldBuf

	c.writeln("#include <stdio.h>")
	c.writeln("#include <stdlib.h>")
	c.writeln("#include <string.h>")
	c.writeln("")
	c.emitRuntime()
	if c.buf.Len() > 0 && c.buf.Bytes()[c.buf.Len()-1] != '\n' {
		c.writeln("")
	}
	for _, ex := range c.externs {
		c.writeln(ex)
	}
	if len(c.externs) > 0 {
		c.writeln("")
	}
	c.buf.WriteString(body)
	return c.buf.Bytes(), nil
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	return c.compileProgram(prog)
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	ret := c.cType(fun.Return)
	c.buf.WriteString(ret + " ")
	c.buf.WriteString(sanitizeName(fun.Name))
	c.buf.WriteByte('(')
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(c.cType(p.Type))
		c.buf.WriteByte(' ')
		c.buf.WriteString(sanitizeName(p.Name))
		if p.Type != nil {
			t := resolveTypeRef(p.Type, c.env)
			if isListStringType(t) {
				c.needsListString = true
			}
			if isListFloatType(t) {
				c.needsListFloat = true
			}
			if isListListIntType(t) {
				c.needsListListInt = true
			}
		}
	}
	c.buf.WriteString("){\n")
	c.indent++
	oldEnv := c.env
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		for _, p := range fun.Params {
			if p.Type != nil {
				c.env.SetVar(p.Name, resolveTypeRef(p.Type, oldEnv), true)
			}
		}
	}
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = oldEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := sanitizeName(s.Let.Name)
		var t types.Type
		if s.Let.Type != nil {
			t = resolveTypeRef(s.Let.Type, c.env)
		} else if s.Let.Value != nil {
			t = c.inferExprType(s.Let.Value)
		}
		if t == nil {
			t = types.IntType{}
		}
		typ := cTypeFromType(t)
		if s.Let.Value != nil {
			if isEmptyListLiteral(s.Let.Value) {
				if isListStringType(t) {
					c.needsListString = true
					val := c.newTemp()
					c.writeln(fmt.Sprintf("list_string %s = list_string_create(0);", val))
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				} else if isListFloatType(t) {
					c.needsListFloat = true
					val := c.newTemp()
					c.writeln(fmt.Sprintf("list_float %s = list_float_create(0);", val))
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				} else if isListListIntType(t) {
					c.needsListListInt = true
					val := c.newTemp()
					c.writeln(fmt.Sprintf("list_list_int %s = list_list_int_create(0);", val))
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				} else {
					val := c.compileExpr(s.Let.Value)
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				}
			} else {
				val := c.compileExpr(s.Let.Value)
				c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
			}
		} else {
			c.writeln(fmt.Sprintf("%s %s;", typ, name))
		}
		if c.env != nil {
			c.env.SetVar(s.Let.Name, t, false)
		}
	case s.Var != nil:
		name := sanitizeName(s.Var.Name)
		var t types.Type
		if s.Var.Type != nil {
			t = resolveTypeRef(s.Var.Type, c.env)
		} else if s.Var.Value != nil {
			t = c.inferExprType(s.Var.Value)
		}
		if t == nil {
			t = types.IntType{}
		}
		typ := cTypeFromType(t)
		if s.Var.Value != nil {
			if isEmptyListLiteral(s.Var.Value) {
				if isListStringType(t) {
					c.needsListString = true
					val := c.newTemp()
					c.writeln(fmt.Sprintf("list_string %s = list_string_create(0);", val))
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				} else if isListFloatType(t) {
					c.needsListFloat = true
					val := c.newTemp()
					c.writeln(fmt.Sprintf("list_float %s = list_float_create(0);", val))
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				} else if isListListIntType(t) {
					c.needsListListInt = true
					val := c.newTemp()
					c.writeln(fmt.Sprintf("list_list_int %s = list_list_int_create(0);", val))
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				} else {
					val := c.compileExpr(s.Var.Value)
					c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
				}
			} else {
				val := c.compileExpr(s.Var.Value)
				c.writeln(fmt.Sprintf("%s %s = %s;", typ, name, val))
			}
		} else {
			c.writeln(fmt.Sprintf("%s %s;", typ, name))
		}
		if c.env != nil {
			c.env.SetVar(s.Var.Name, t, true)
		}
	case s.Assign != nil:
		lhs := sanitizeName(s.Assign.Name)
		target := lhs
		for _, idx := range s.Assign.Index {
			ix := c.compileExpr(idx.Start)
			target = fmt.Sprintf("%s.data[%s]", target, ix)
		}
		val := c.compileExpr(s.Assign.Value)
		c.writeln(fmt.Sprintf("%s = %s;", target, val))
	case s.Return != nil:
		val := c.compileExpr(s.Return.Value)
		c.writeln(fmt.Sprintf("return %s;", val))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr := c.compileExpr(s.Expr.Expr)
		if expr != "" {
			c.writeln(fmt.Sprintf("%s;", expr))
		}
	case s.Expect != nil:
		if err := c.compileExpect(s.Expect); err != nil {
			return err
		}
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	default:
		// unsupported
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	useVar := f.Name != "_"
	if f.RangeEnd != nil {
		start := c.compileExpr(f.Source)
		end := c.compileExpr(f.RangeEnd)
		loopVar := name
		if !useVar {
			loopVar = c.newTemp()
		}
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", loopVar, start, loopVar, end, loopVar))
		if useVar && loopVar != name {
			c.indent++
			c.writeln(fmt.Sprintf("int %s = %s;", name, loopVar))
		} else {
			c.indent++
		}
	} else {
		src := c.compileExpr(f.Source)
		idx := c.newTemp()
		isStr := isStringExpr(f.Source, c.env)
		isListStr := isListStringExpr(f.Source, c.env)
		if isStr {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s[%s] != '\\0'; %s++) {", idx, src, idx, idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("char %s[2];", name))
				c.writeln(fmt.Sprintf("%s[0] = %s[%s];", name, src, idx))
				c.writeln(fmt.Sprintf("%s[1] = '\\0';", name))
			}
		} else if isListStr {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s.len; %s++) {", idx, idx, src, idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("char* %s = %s.data[%s];", name, src, idx))
			}
		} else {
			c.writeln(fmt.Sprintf("for (int %s = 0; %s < %s.len; %s++) {", idx, idx, src, idx))
			c.indent++
			if useVar {
				c.writeln(fmt.Sprintf("int %s = %s.data[%s];", name, src, idx))
			}
		}
		oldEnv := c.env
		if c.env != nil {
			c.env = types.NewEnv(c.env)
			if useVar {
				if isStr {
					c.env.SetVar(f.Name, types.StringType{}, true)
				} else if isListStr {
					c.env.SetVar(f.Name, types.StringType{}, true)
				} else {
					c.env.SetVar(f.Name, types.IntType{}, true)
				}
			}
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		if c.env != nil {
			c.env = oldEnv
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	oldEnv := c.env
	if c.env != nil {
		c.env = types.NewEnv(c.env)
		if useVar {
			c.env.SetVar(f.Name, types.IntType{}, true)
		}
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if c.env != nil {
		c.env = oldEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond := c.compileExpr(stmt.Cond)
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond := c.compileExpr(w.Cond)
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	cond := c.compileExpr(e.Value)
	c.writeln(fmt.Sprintf("if (!(%s)) { fprintf(stderr, \"expect failed\\n\"); exit(1); }", cond))
	return nil
}

func (c *Compiler) compileIfExpr(e *parser.IfExpr) string {
	cond := c.compileExpr(e.Cond)
	thenVal := c.compileExpr(e.Then)
	elseVal := "0"
	if e.ElseIf != nil {
		elseVal = c.compileIfExpr(e.ElseIf)
	} else if e.Else != nil {
		elseVal = c.compileExpr(e.Else)
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenVal, elseVal)
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) string {
	target := c.compileExpr(m.Target)
	expr := "0"
	for i := len(m.Cases) - 1; i >= 0; i-- {
		pat := c.compileExpr(m.Cases[i].Pattern)
		res := c.compileExpr(m.Cases[i].Result)
		if pat == "_" {
			expr = res
		} else {
			expr = fmt.Sprintf("(%s == %s ? %s : %s)", target, pat, res, expr)
		}
	}
	return expr
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) string {
	name := fmt.Sprintf("_lambda%d", len(c.lambdas))
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 0
	var body []*parser.Statement
	if fn.ExprBody != nil {
		body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fn.ExprBody}}}
	} else {
		body = fn.BlockBody
	}
	fs := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: body}
	if err := c.compileFun(fs); err != nil {
		c.buf = oldBuf
		c.indent = oldIndent
		return "0"
	}
	code := c.buf.String()
	c.lambdas = append(c.lambdas, code)
	c.buf = oldBuf
	c.indent = oldIndent
	return name
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln("static void " + name + "() {")
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	res := c.compileBinary(e.Binary)
	return res
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) string {
	left := c.compileUnary(b.Left)
	leftList := isListListUnary(b.Left, c.env)
	leftListInt := isListIntUnary(b.Left, c.env)
	leftListFloat := isListFloatUnary(b.Left, c.env)
	leftListString := isListStringUnary(b.Left, c.env)
	leftString := isStringUnary(b.Left, c.env)
	for _, op := range b.Right {
		right := c.compilePostfix(op.Right)
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftList && isListListPostfix(op.Right, c.env) {
			c.needsConcatListListInt = true
			c.needsListListInt = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_list_int %s = concat_list_list_int(%s, %s);", name, left, right))
			left = name
			leftList = true
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.needsConcatListInt = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = concat_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.needsConcatListFloat = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = concat_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftListString && isListStringPostfix(op.Right, c.env) {
			c.needsConcatListString = true
			c.needsListString = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = concat_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if (op.Op == "+" || (op.Op == "union" && op.All)) && leftString && isStringPostfixOrIndex(op.Right, c.env) {
			c.needsConcatString = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("char* %s = concat_string(%s, %s);", name, left, right))
			left = name
			leftString = true
			leftList = false
			leftListInt = false
			leftListString = false
			continue
		}
		if op.Op == "union" && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.needsUnionListInt = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = union_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "union" && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.needsUnionListFloat = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = union_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "union" && leftListString && isListStringPostfix(op.Right, c.env) {
			c.needsUnionListString = true
			c.needsListString = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = union_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if op.Op == "except" && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.needsExceptListInt = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = except_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "except" && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.needsExceptListFloat = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = except_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "except" && leftListString && isListStringPostfix(op.Right, c.env) {
			c.needsExceptListString = true
			c.needsListString = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = except_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if op.Op == "intersect" && leftListInt && isListIntPostfix(op.Right, c.env) {
			c.needsIntersectListInt = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_int %s = intersect_list_int(%s, %s);", name, left, right))
			left = name
			leftListInt = true
			leftList = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "intersect" && leftListString && isListStringPostfix(op.Right, c.env) {
			c.needsIntersectListString = true
			c.needsListString = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_string %s = intersect_list_string(%s, %s);", name, left, right))
			left = name
			leftListString = true
			leftList = false
			leftListInt = false
			leftString = false
			continue
		}
		if op.Op == "intersect" && leftListFloat && isListFloatPostfix(op.Right, c.env) {
			c.needsIntersectListFloat = true
			name := c.newTemp()
			c.writeln(fmt.Sprintf("list_float %s = intersect_list_float(%s, %s);", name, left, right))
			left = name
			leftListFloat = true
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "in" && isListIntPostfix(op.Right, c.env) {
			c.needsInListInt = true
			left = fmt.Sprintf("contains_list_int(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		if op.Op == "in" && isListStringPostfix(op.Right, c.env) {
			c.needsInListString = true
			c.needsListString = true
			left = fmt.Sprintf("contains_list_string(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftString = false
			continue
		}
		if op.Op == "in" && isListFloatPostfix(op.Right, c.env) {
			c.needsInListFloat = true
			left = fmt.Sprintf("contains_list_float(%s, %s)", right, left)
			leftList = false
			leftListInt = false
			leftListString = false
			leftListFloat = false
			leftString = false
			continue
		}
		left = fmt.Sprintf("(%s %s %s)", left, op.Op, right)
		leftList = false
		leftListInt = false
		leftListString = false
		leftListFloat = false
		leftString = false
	}
	return left
}

func (c *Compiler) compileUnary(u *parser.Unary) string {
	expr := c.compilePostfix(u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			expr = fmt.Sprintf("(-%s)", expr)
		} else if op == "!" {
			expr = fmt.Sprintf("(!%s)", expr)
		}
	}
	return expr
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) string {
	expr := c.compilePrimary(p.Target)
	isStr := isStringPrimary(p.Target, c.env)
	isFloatList := isListFloatPrimary(p.Target, c.env)
	isStringList := isListStringPrimary(p.Target, c.env)
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx := c.compileExpr(op.Index.Start)
				if isStr && op.Index.End == nil {
					name := c.newTemp()
					c.needsIndexString = true
					c.writeln(fmt.Sprintf("char* %s = _index_string(%s, %s);", name, expr, idx))
					if c.env != nil {
						c.env.SetVar(name, types.StringType{}, true)
					}
					expr = name
					isStr = true
				} else {
					expr = fmt.Sprintf("%s.data[%s]", expr, idx)
					if isStringList {
						isStr = true
					} else {
						isStr = false
					}
					isFloatList = false
					isStringList = false
				}
			} else {
				start := "0"
				if op.Index.Start != nil {
					start = c.compileExpr(op.Index.Start)
				}
				end := fmt.Sprintf("%s.len", expr)
				if op.Index.End != nil {
					end = c.compileExpr(op.Index.End)
				}
				name := c.newTemp()
				if isStr {
					c.needsSliceString = true
					c.writeln(fmt.Sprintf("char* %s = slice_string(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.StringType{}, true)
					}
					expr = name
					isStr = true
				} else if isFloatList {
					c.needsSliceListFloat = true
					c.writeln(fmt.Sprintf("list_float %s = slice_list_float(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.FloatType{}}, true)
					}
					expr = name
					isStr = false
					isFloatList = false
					isStringList = false
				} else if isStringList {
					c.needsSliceListString = true
					c.needsListString = true
					c.writeln(fmt.Sprintf("list_string %s = slice_list_string(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.StringType{}}, true)
					}
					expr = name
					isStr = false
					isFloatList = false
					isStringList = false
				} else {
					c.needsSliceListInt = true
					c.writeln(fmt.Sprintf("list_int %s = slice_list_int(%s, %s, %s);", name, expr, start, end))
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.IntType{}}, true)
					}
					expr = name
					isStr = false
					isFloatList = false
					isStringList = false
				}
			}
		}
	}
	return expr
}

func (c *Compiler) compilePrimary(p *parser.Primary) string {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		return c.compileSelector(p.Selector)
	case p.List != nil:
		name := c.newTemp()
		nested := false
		if len(p.List.Elems) > 0 {
			if isListLiteral(p.List.Elems[0]) || isListIntExpr(p.List.Elems[0], c.env) {
				nested = true
			}
		}
		if nested {
			c.needsListListInt = true
			c.writeln(fmt.Sprintf("list_list_int %s = list_list_int_create(%d);", name, len(p.List.Elems)))
			for i, el := range p.List.Elems {
				v := c.compileExpr(el)
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		} else if len(p.List.Elems) > 0 && isStringExpr(p.List.Elems[0], c.env) {
			c.needsListString = true
			c.writeln(fmt.Sprintf("list_string %s = list_string_create(%d);", name, len(p.List.Elems)))
			for i, el := range p.List.Elems {
				v := c.compileExpr(el)
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		} else if len(p.List.Elems) > 0 && isFloatExpr(p.List.Elems[0], c.env) {
			c.needsListFloat = true
			c.writeln(fmt.Sprintf("list_float %s = list_float_create(%d);", name, len(p.List.Elems)))
			for i, el := range p.List.Elems {
				v := c.compileExpr(el)
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		} else {
			c.writeln(fmt.Sprintf("list_int %s = list_int_create(%d);", name, len(p.List.Elems)))
			for i, el := range p.List.Elems {
				v := c.compileExpr(el)
				c.writeln(fmt.Sprintf("%s.data[%d] = %s;", name, i, v))
			}
		}
		return name
	case p.Call != nil:
		if p.Call.Func == "len" {
			isStr := isStringArg(p.Call.Args[0], c.env)
			arg := c.compileExpr(p.Call.Args[0])
			if isStr {
				c.needsStrLen = true
				return fmt.Sprintf("strlen(%s)", arg)
			}
			return fmt.Sprintf("%s.len", arg)
		} else if p.Call.Func == "print" {
			for i, a := range p.Call.Args {
				argExpr := c.compileExpr(a)
				if isListListExpr(a, c.env) {
					c.needsListListInt = true
					c.writeln(fmt.Sprintf("_print_list_list_int(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isListIntExpr(a, c.env) {
					c.needsListListInt = true
					c.writeln(fmt.Sprintf("_print_list_int(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isListFloatExpr(a, c.env) {
					c.needsListFloat = true
					c.writeln(fmt.Sprintf("_print_list_float(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else if isListStringExpr(a, c.env) {
					c.needsListString = true
					c.writeln(fmt.Sprintf("_print_list_string(%s);", argExpr))
					if i == len(p.Call.Args)-1 {
						c.writeln("printf(\"\\n\");")
					} else {
						c.writeln("printf(\" \");")
					}
				} else {
					fmtStr := "%d"
					if isStringArg(a, c.env) {
						fmtStr = "%s"
					} else if isFloatArg(a, c.env) {
						fmtStr = "%g"
					}
					end := " "
					if i == len(p.Call.Args)-1 {
						end = "\\n"
					}
					c.writeln(fmt.Sprintf("printf(\"%s%s\", %s);", fmtStr, end, argExpr))
				}
			}
			return ""
		} else if p.Call.Func == "count" {
			arg := c.compileExpr(p.Call.Args[0])
			c.needsCount = true
			return fmt.Sprintf("_count(%s)", arg)
		} else if p.Call.Func == "avg" {
			arg := c.compileExpr(p.Call.Args[0])
			c.needsAvg = true
			return fmt.Sprintf("_avg(%s)", arg)
		} else if p.Call.Func == "str" {
			arg := c.compileExpr(p.Call.Args[0])
			name := c.newTemp()
			c.needsStr = true
			c.writeln(fmt.Sprintf("char* %s = _str(%s);", name, arg))
			return name
		} else if p.Call.Func == "input" {
			c.needsInput = true
			return "_input()"
		}
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			args[i] = c.compileExpr(a)
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", "))
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Group != nil:
		return fmt.Sprintf("(%s)", c.compileExpr(p.Group))
	default:
		return "0"
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return s
	case l.Bool != nil:
		if *l.Bool {
			return "1"
		} else {
			return "0"
		}
	case l.Str != nil:
		return strconv.Quote(*l.Str)
	}
	return "0"
}

func (c *Compiler) compileSelector(s *parser.SelectorExpr) string {
	return sanitizeName(s.Root)
}

func isStringArg(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnaryOrIndex(e.Binary.Left, env)
}

func isStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnary(e.Binary.Left, env)
}

func isStringUnaryOrIndex(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfixOrIndex(u.Value, env)
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfix(u.Value, env)
}

func isStringPostfixOrIndex(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if p.Ops[0].Index != nil {
			if isStringPrimary(p.Target, env) || isListStringPrimary(p.Target, env) {
				return true
			}
			return false
		}
		return false
	}
	return isStringPrimary(p.Target, env)
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if p.Ops[0].Index != nil && isListStringPrimary(p.Target, env) {
			return true
		}
		return false
	}
	return isStringPrimary(p.Target, env)
}

func isStringPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	case p.Call != nil && p.Call.Func == "str":
		return true
	case p.Selector != nil && env != nil:
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func isFloatArg(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	c := New(env)
	_, ok := c.inferExprType(e).(types.FloatType)
	return ok
}

func isFloatExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isFloatUnary(e.Binary.Left, env)
}

func isFloatUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isFloatPostfix(u.Value, env)
}

func isFloatPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isFloatPrimary(p.Target, env)
}

func isFloatPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Float != nil:
		return true
	case p.Selector != nil && env != nil:
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	case p.Call != nil && env != nil:
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if _, ok2 := ft.Return.(types.FloatType); ok2 {
					return true
				}
			}
		}
	}
	return false
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListLiteralUnary(e.Binary.Left)
}

func isListLiteralUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isListLiteralPostfix(u.Value)
}

func isListLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListLiteralPrimary(p.Target)
}

func isListLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.List != nil
}

func isEmptyListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return false
	}
	if e.Binary.Left.Value.Target.List != nil {
		return len(e.Binary.Left.Value.Target.List.Elems) == 0
	}
	return false
}

func isListListExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListListUnary(e.Binary.Left, env)
}

func isListListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListListPostfix(u.Value, env)
}

func isListListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListListPrimary(p.Target, env)
}

func isListListPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) > 0 {
			if isListLiteral(p.List.Elems[0]) || isListIntExpr(p.List.Elems[0], env) {
				return true
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if isListListIntType(t) {
				return true
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if isListListIntType(ft.Return) {
					return true
				}
			}
		}
	}
	return false
}

func isListIntExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListIntUnary(e.Binary.Left, env)
}

func isListIntUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListIntPostfix(u.Value, env)
}

func isListIntPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListIntPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		if isStringPrimary(p.Target, env) {
			return false
		}
		return true
	}
	return false
}

func isListIntPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if el.Binary.Left.Value.Target.Lit != nil {
				if el.Binary.Left.Value.Target.Lit.Int != nil || el.Binary.Left.Value.Target.Lit.Bool != nil {
					return true
				}
			}
			if el.Binary.Left.Value.Target.Selector != nil && env != nil {
				name := el.Binary.Left.Value.Target.Selector.Root
				if t, err := env.GetVar(name); err == nil {
					if lt, ok := t.(types.ListType); ok {
						switch lt.Elem.(type) {
						case types.IntType, types.BoolType:
							return true
						}
					}
					switch t.(type) {
					case types.IntType, types.BoolType:
						return true
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				switch lt.Elem.(type) {
				case types.IntType, types.BoolType:
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					switch lt.Elem.(type) {
					case types.IntType, types.BoolType:
						return true
					}
				}
			}
		}
	}
	return false
}

func isListStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListStringUnary(e.Binary.Left, env)
}

func isListStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListStringPostfix(u.Value, env)
}

func isListStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListStringPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		if isStringPrimary(p.Target, env) {
			return false
		}
		return true
	}
	return false
}

func isListStringPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if lit := el.Binary.Left.Value.Target.Lit; lit != nil && lit.Str != nil {
				return true
			}
			if sel := el.Binary.Left.Value.Target.Selector; sel != nil && env != nil {
				if t, err := env.GetVar(sel.Root); err == nil {
					if _, ok := t.(types.StringType); ok {
						return true
					}
					if lt, ok := t.(types.ListType); ok {
						if _, ok2 := lt.Elem.(types.StringType); ok2 {
							return true
						}
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					if _, ok3 := lt.Elem.(types.StringType); ok3 {
						return true
					}
				}
			}
		}
	}
	return false
}

func isListFloatExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isListFloatUnary(e.Binary.Left, env)
}

func isListFloatUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListFloatPostfix(u.Value, env)
}

func isListFloatPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) == 0 {
		return isListFloatPrimary(p.Target, env)
	}
	if p.Ops[0].Index != nil && p.Ops[0].Index.Colon != nil {
		if isStringPrimary(p.Target, env) {
			return false
		}
		return true
	}
	return false
}

func isListFloatPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		if len(p.List.Elems) == 0 {
			return true
		}
		el := p.List.Elems[0]
		if el.Binary != nil && el.Binary.Left != nil && el.Binary.Left.Value != nil && el.Binary.Left.Value.Target != nil {
			if lit := el.Binary.Left.Value.Target.Lit; lit != nil && lit.Float != nil {
				return true
			}
			if sel := el.Binary.Left.Value.Target.Selector; sel != nil && env != nil {
				if t, err := env.GetVar(sel.Root); err == nil {
					if _, ok := t.(types.FloatType); ok {
						return true
					}
					if lt, ok := t.(types.ListType); ok {
						if _, ok2 := lt.Elem.(types.FloatType); ok2 {
							return true
						}
					}
				}
			}
		}
	}
	if p.Selector != nil && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.FloatType); ok {
					return true
				}
			}
		}
	}
	if p.Call != nil && env != nil {
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if lt, ok2 := ft.Return.(types.ListType); ok2 {
					if _, ok3 := lt.Elem.(types.FloatType); ok3 {
						return true
					}
				}
			}
		}
	}
	return false
}
