//go:build slow

package javacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf             *bytes.Buffer
	indent          int
	helpers         map[string]bool
	vars            map[string]string
	funRet          map[string]string
	types           map[string]*parser.TypeDecl
	needFuncImports bool
}

func toObjectType(t string) string {
	switch t {
	case "int":
		return "Integer"
	case "double":
		return "Double"
	case "boolean":
		return "Boolean"
	default:
		return t
	}
}

func New() *Compiler {
	return &Compiler{buf: new(bytes.Buffer), helpers: make(map[string]bool), vars: make(map[string]string), funRet: make(map[string]string), types: make(map[string]*parser.TypeDecl)}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// compile main body first so we know which helpers are needed
	body := new(bytes.Buffer)
	origBuf := c.buf
	c.buf = body
	c.indent = 1
	for _, s := range prog.Statements {
		if s.Var != nil {
			typ := c.typeName(s.Var.Type)
			if s.Var.Type == nil && s.Var.Value != nil {
				typ = c.inferType(s.Var.Value)
			}
			c.vars[s.Var.Name] = typ
			continue
		}
		if s.Let != nil {
			typ := c.typeName(s.Let.Type)
			if s.Let.Type == nil && s.Let.Value != nil {
				typ = c.inferType(s.Let.Value)
			}
			c.vars[s.Let.Name] = typ
			continue
		}
		if s.Type != nil {
			c.types[s.Type.Name] = s.Type
			continue
		}
		if s.Fun != nil {
			c.funRet[s.Fun.Name] = c.typeName(s.Fun.Return)
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	// restore buffer for final output
	c.buf = origBuf
	c.buf.Reset()
	c.indent = 0
	c.writeln("import java.util.*;")
	if c.needFuncImports {
		c.writeln("import java.util.function.*;")
	}
	// user defined types
	for _, tdecl := range c.types {
		if err := c.compileTypeDecl(tdecl); err != nil {
			return nil, err
		}
	}
	c.writeln("public class Main {")
	c.indent++
	// global declarations
	for _, s := range prog.Statements {
		switch {
		case s.Var != nil:
			if err := c.compileGlobalVar(s.Var); err != nil {
				return nil, err
			}
		case s.Let != nil:
			if err := c.compileGlobalLet(s.Let); err != nil {
				return nil, err
			}
		}
	}
	// function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
		}
	}
	// helper methods
	if c.helpers["append"] {
		c.writeln("static <T> List<T> append(List<T> list, T item) {")
		c.indent++
		c.writeln("List<T> res = new ArrayList<>(list);")
		c.writeln("res.add(item);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["count"] {
		c.writeln("static int count(Collection<?> c) {")
		c.indent++
		c.writeln("return c.size();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["sum"] {
		c.writeln("static int sum(List<Integer> v) {")
		c.indent++
		c.writeln("int s = 0;")
		c.writeln("for (int n : v) s += n;")
		c.writeln("return s;")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["avg"] {
		c.writeln("static double avg(List<Integer> v) {")
		c.indent++
		c.writeln("if (v.isEmpty()) return 0;")
		c.writeln("int s = 0;")
		c.writeln("for (int n : v) s += n;")
		c.writeln("return (double)s / v.size();")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["min"] {
		c.writeln("static int min(List<Integer> v) {")
		c.indent++
		c.writeln("return Collections.min(v);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["max"] {
		c.writeln("static int max(List<Integer> v) {")
		c.indent++
		c.writeln("return Collections.max(v);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["values"] {
		c.writeln("static <K,V> List<V> values(Map<K,V> m) {")
		c.indent++
		c.writeln("return new ArrayList<>(m.values());")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["slice"] {
		c.writeln("static <T> List<T> slice(List<T> obj, int i, int j) {")
		c.indent++
		c.writeln("int start = i;")
		c.writeln("int end = j;")
		c.writeln("int n = obj.size();")
		c.writeln("if (start < 0) start += n;")
		c.writeln("if (end < 0) end += n;")
		c.writeln("if (start < 0) start = 0;")
		c.writeln("if (end > n) end = n;")
		c.writeln("if (end < start) end = start;")
		c.writeln("return new ArrayList<>(obj.subList(start, end));")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["sliceString"] {
		c.writeln("static String sliceString(String s, int i, int j) {")
		c.indent++
		c.writeln("int start = i;")
		c.writeln("int end = j;")
		c.writeln("int n = s.length();")
		c.writeln("if (start < 0) start += n;")
		c.writeln("if (end < 0) end += n;")
		c.writeln("if (start < 0) start = 0;")
		c.writeln("if (end > n) end = n;")
		c.writeln("if (end < start) end = start;")
		c.writeln("return s.substring(start, end);")
		c.indent--
		c.writeln("}")
	}
	if c.helpers["in"] {
		c.writeln("static boolean inOp(Object item, Object collection) {")
		c.indent++
		c.writeln("if (collection instanceof Map<?,?> m) return m.containsKey(item);")
		c.writeln("if (collection instanceof Collection<?> c) return c.contains(item);")
		c.writeln("if (collection instanceof String s) return s.contains(String.valueOf(item));")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
	}
	c.writeln("public static void main(String[] args) {")
	c.indent++
	c.buf.Write(body.Bytes())
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
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Expr != nil:
		return c.compileExprStmt(s.Expr)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) typeName(t *parser.TypeRef) string {
	if t == nil {
		return "int"
	}
	if t.Fun != nil {
		if len(t.Fun.Params) == 1 && t.Fun.Return != nil && t.Fun.Return.Simple != nil && *t.Fun.Return.Simple == "int" && t.Fun.Params[0].Simple != nil && *t.Fun.Params[0].Simple == "int" {
			c.needFuncImports = true
			return "IntUnaryOperator"
		}
		return "Object"
	}
	if t.Generic != nil {
		g := t.Generic
		if g.Name == "list" && len(g.Args) == 1 {
			elem := toObjectType(c.typeName(g.Args[0]))
			return fmt.Sprintf("List<%s>", elem)
		}
		if g.Name == "map" && len(g.Args) == 2 {
			kt := toObjectType(c.typeName(g.Args[0]))
			vt := toObjectType(c.typeName(g.Args[1]))
			return fmt.Sprintf("Map<%s,%s>", kt, vt)
		}
		return "Object"
	}
	if t.Simple == nil {
		return "Object"
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
		return *t.Simple
	}
}

func (c *Compiler) defaultValue(typ string) string {
	switch typ {
	case "String":
		return "\"\""
	case "int":
		return "0"
	case "boolean":
		return "false"
	case "double":
		return "0.0"
	default:
		return "null"
	}
}

func (c *Compiler) inferType(e *parser.Expr) string {
	// handle cast expressions first
	if e != nil && e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		u := e.Binary.Left.Value
		if len(u.Ops) > 0 {
			if cast := u.Ops[len(u.Ops)-1].Cast; cast != nil {
				typ := c.typeName(cast.Type)
				return typ
			}
		}
	}
	if l := isListLiteral(e); l != nil {
		et := "Object"
		if len(l.Elems) > 0 {
			et = c.litType(l.Elems[0])
		}
		return fmt.Sprintf("List<%s>", et)
	}
	if m := isMapLiteral(e); m != nil {
		kt, vt := "Object", "Object"
		if len(m.Items) > 0 {
			kt = c.litType(m.Items[0].Key)
			vt = c.litType(m.Items[0].Value)
		}
		return fmt.Sprintf("Map<%s,%s>", kt, vt)
	}
	p := rootPrimary(e)
	if p != nil && p.Lit != nil {
		return c.typeName(&parser.TypeRef{Simple: litTypeName(p)})
	}
	if p != nil && p.Call != nil {
		if t, ok := c.funRet[p.Call.Func]; ok {
			return t
		}
	}
	if p != nil && p.Struct != nil {
		return p.Struct.Name
	}
	if p != nil && p.FunExpr != nil {
		if len(p.FunExpr.Params) == 1 && p.FunExpr.Return != nil && p.FunExpr.Return.Simple != nil && *p.FunExpr.Return.Simple == "int" && p.FunExpr.Params[0].Type != nil && p.FunExpr.Params[0].Type.Simple != nil && *p.FunExpr.Params[0].Type.Simple == "int" {
			c.needFuncImports = true
			return "IntUnaryOperator"
		}
		return "Object"
	}
	return "var"
}

func (c *Compiler) litType(e *parser.Expr) string {
	p := rootPrimary(e)
	if p == nil || p.Lit == nil {
		return "Object"
	}
	switch {
	case p.Lit.Int != nil:
		return "Integer"
	case p.Lit.Float != nil:
		return "Double"
	case p.Lit.Str != nil:
		return "String"
	case p.Lit.Bool != nil:
		return "Boolean"
	default:
		return "Object"
	}
}

func rootPrimary(e *parser.Expr) *parser.Primary {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	return e.Binary.Left.Value.Target
}

func litTypeName(p *parser.Primary) *string {
	if p.Lit == nil {
		return nil
	}
	if p.Lit.Int != nil {
		s := "int"
		return &s
	}
	if p.Lit.Float != nil {
		s := "float"
		return &s
	}
	if p.Lit.Str != nil {
		s := "string"
		return &s
	}
	if p.Lit.Bool != nil {
		s := "bool"
		return &s
	}
	return nil
}

func isListLiteral(e *parser.Expr) *parser.ListLiteral {
	p := rootPrimary(e)
	if p != nil && p.List != nil && len(e.Binary.Right) == 0 {
		return p.List
	}
	return nil
}

func isMapLiteral(e *parser.Expr) *parser.MapLiteral {
	p := rootPrimary(e)
	if p != nil && p.Map != nil && len(e.Binary.Right) == 0 {
		return p.Map
	}
	return nil
}

func isMapLitCastToStructExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value
	if p.Target != nil && p.Target.Map != nil && len(p.Ops) == 1 && p.Ops[0].Cast != nil {
		return true
	}
	return false
}

func (c *Compiler) exprIsMap(e *parser.Expr) bool {
	if isMapLiteral(e) != nil {
		return true
	}
	if p := rootPrimary(e); p != nil && p.Selector != nil && len(p.Selector.Tail) == 0 {
		if t, ok := c.vars[p.Selector.Root]; ok {
			return strings.HasPrefix(t, "Map")
		}
	}
	return false
}

func (c *Compiler) compileGlobalVar(v *parser.VarStmt) error {
	typ := c.typeName(v.Type)
	expr, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		} else if isMapLiteral(v.Value) != nil && !isMapLitCastToStructExpr(v.Value) {
			expr = fmt.Sprintf("new HashMap<>(%s)", expr)
		}
	}
	if typ == "var" {
		typ = "Object"
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileGlobalLet(v *parser.LetStmt) error {
	typ := c.typeName(v.Type)
	expr, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		} else if isMapLiteral(v.Value) != nil && !isMapLitCastToStructExpr(v.Value) {
			expr = fmt.Sprintf("new HashMap<>(%s)", expr)
		}
	}
	if typ == "var" {
		typ = "Object"
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("static %s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return fmt.Errorf("variants not supported")
	}
	c.writeln(fmt.Sprintf("class %s {", t.Name))
	c.indent++
	var params []string
	for _, m := range t.Members {
		if m.Field != nil {
			typ := c.typeName(m.Field.Type)
			c.writeln(fmt.Sprintf("%s %s;", typ, m.Field.Name))
			params = append(params, fmt.Sprintf("%s %s", typ, m.Field.Name))
		}
	}
	if len(params) > 0 {
		c.writeln(fmt.Sprintf("%s(%s) {", t.Name, strings.Join(params, ", ")))
		c.indent++
		for _, m := range t.Members {
			if m.Field != nil {
				name := m.Field.Name
				c.writeln(fmt.Sprintf("this.%s = %s;", name, name))
			}
		}
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileList(l *parser.ListLiteral) (string, error) {
	var elems []string
	for _, e := range l.Elems {
		s, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		elems = append(elems, s)
	}
	return fmt.Sprintf("java.util.Arrays.asList(%s)", strings.Join(elems, ", ")), nil
}

func (c *Compiler) compileMap(m *parser.MapLiteral) (string, error) {
	var items []string
	for _, it := range m.Items {
		k, err := c.compileExpr(it.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items = append(items, fmt.Sprintf("%s, %s", k, v))
	}
	return fmt.Sprintf("new HashMap<>(java.util.Map.of(%s))", strings.Join(items, ", ")), nil
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	var args []string
	for _, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		args = append(args, v)
	}
	return fmt.Sprintf("new %s(%s)", s.Name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	if len(f.Params) == 1 && f.Return != nil && f.Return.Simple != nil && *f.Return.Simple == "int" && f.Params[0].Type != nil && f.Params[0].Type.Simple != nil && *f.Params[0].Type.Simple == "int" {
		c.needFuncImports = true
	}
	var params []string
	for _, p := range f.Params {
		params = append(params, p.Name)
	}
	body, err := c.compileExpr(f.ExprBody)
	if err != nil {
		return "", err
	}
	if len(params) == 1 {
		return fmt.Sprintf("%s -> %s", params[0], body), nil
	}
	return fmt.Sprintf("(%s) -> %s", strings.Join(params, ", "), body), nil
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
	var elseExpr string
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
	} else {
		elseExpr = "null"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	typ := c.typeName(v.Type)
	expr, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		} else if isMapLiteral(v.Value) != nil && !isMapLitCastToStructExpr(v.Value) {
			expr = fmt.Sprintf("new HashMap<>(%s)", expr)
		}
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileLet(v *parser.LetStmt) error {
	typ := c.typeName(v.Type)
	expr, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type == nil && v.Value != nil {
		typ = c.inferType(v.Value)
		if isListLiteral(v.Value) != nil {
			expr = fmt.Sprintf("new ArrayList<>(%s)", expr)
		} else if isMapLiteral(v.Value) != nil && !isMapLitCastToStructExpr(v.Value) {
			expr = fmt.Sprintf("new HashMap<>(%s)", expr)
		}
	}
	if v.Value == nil {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, c.defaultValue(typ)))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, expr))
	}
	c.vars[v.Name] = typ
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	expr, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	if len(a.Index) == 0 && len(a.Field) == 0 {
		c.writeln(fmt.Sprintf("%s = %s;", a.Name, expr))
		return nil
	}
	target := a.Name
	for _, f := range a.Field {
		target += "." + f.Name
	}
	for i, idx := range a.Index {
		if idx.Start == nil || idx.Colon != nil {
			return fmt.Errorf("complex indexing not supported")
		}
		ix, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		if i == len(a.Index)-1 {
			if strings.HasPrefix(ix, "\"") {
				c.writeln(fmt.Sprintf("((Map)%s).put(%s, %s);", target, ix, expr))
			} else {
				c.writeln(fmt.Sprintf("((List)%s).set(%s, %s);", target, ix, expr))
			}
		} else {
			if strings.HasPrefix(ix, "\"") {
				target = fmt.Sprintf("((Map)%s).get(%s)", target, ix)
			} else {
				target = fmt.Sprintf("((List)%s).get(%s)", target, ix)
			}
		}
	}
	if len(a.Index) == 0 {
		c.writeln(fmt.Sprintf("%s = %s;", target, expr))
	}
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
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		if c.exprIsMap(f.Source) {
			c.writeln(fmt.Sprintf("for (var %s : %s.keySet()) {", f.Name, src))
		} else {
			c.writeln(fmt.Sprintf("for (var %s : %s) {", f.Name, src))
		}
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
	origVars := c.vars
	c.vars = copyMap(origVars)
	for _, p := range f.Params {
		c.vars[p.Name] = c.typeName(p.Type)
	}
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.vars = origVars
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
		if op.Op == "in" {
			c.helpers["in"] = true
			expr = fmt.Sprintf("inOp(%s, %s)", expr, right)
			continue
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
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			var args []string
			for _, a := range op.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, arg)
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
		case op.Cast != nil:
			t := c.typeName(op.Cast.Type)
			if _, ok := c.types[t]; ok && p.Target.Map != nil {
				var args []string
				for _, it := range p.Target.Map.Items {
					a, err := c.compileExpr(it.Value)
					if err != nil {
						return "", err
					}
					args = append(args, a)
				}
				val = fmt.Sprintf("new %s(%s)", t, strings.Join(args, ", "))
			} else {
				switch t {
				case "int":
					val = fmt.Sprintf("Integer.parseInt(%s)", val)
				case "double":
					val = fmt.Sprintf("Double.parseDouble(%s)", val)
				case "String":
					val = fmt.Sprintf("String.valueOf(%s)", val)
				case "boolean":
					val = fmt.Sprintf("Boolean.parseBoolean(%s)", val)
				default:
					return "", fmt.Errorf("unsupported cast to %s", t)
				}
			}
		case op.Index != nil:
			if op.Index.Colon != nil {
				start := "0"
				end := "0"
				var err error
				if op.Index.Start != nil {
					start, err = c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
				}
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
				}
				if isString(val) || c.vars[val] == "String" {
					c.helpers["sliceString"] = true
					val = fmt.Sprintf("sliceString(%s, %s, %s)", val, start, end)
				} else {
					c.helpers["slice"] = true
					val = fmt.Sprintf("slice(%s, %s, %s)", val, start, end)
				}
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("complex indexing not supported")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if isString(val) || c.vars[val] == "String" {
					val = fmt.Sprintf("%s.charAt(%s)", val, idx)
				} else {
					if strings.HasPrefix(idx, "\"") {
						val = fmt.Sprintf("((Map)%s).get(%s)", val, idx)
					} else {
						val = fmt.Sprintf("((List)%s).get(%s)", val, idx)
					}
				}
			}
		default:
			return "", fmt.Errorf("postfix operations unsupported")
		}
	}
	return val, nil
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
		s := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			s += "." + strings.Join(p.Selector.Tail, ".")
		}
		return s, nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.FunExpr != nil:
		expr, err := c.compileFunExpr(p.FunExpr)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.List != nil:
		return c.compileList(p.List)
	case p.Map != nil:
		return c.compileMap(p.Map)
	case p.Call != nil:
		switch p.Call.Func {
		case "print":
			if len(p.Call.Args) == 0 {
				return "", fmt.Errorf("print expects at least one argument at line %d", p.Pos.Line)
			}
			var parts []string
			for _, a := range p.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				parts = append(parts, arg)
			}
			expr := parts[0]
			for i := 1; i < len(parts); i++ {
				expr += " + \" \" + " + parts[i]
			}
			return fmt.Sprintf("System.out.println(%s)", expr), nil
		case "len":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("len expects one argument at line %d", p.Pos.Line)
			}
			a := p.Call.Args[0]
			expr, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			if isString(expr) || c.vars[expr] == "String" {
				return fmt.Sprintf("%s.length()", expr), nil
			}
			return fmt.Sprintf("%s.size()", expr), nil
		case "str":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("str expects one argument at line %d", p.Pos.Line)
			}
			arg, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("String.valueOf(%s)", arg), nil
		case "substring":
			if len(p.Call.Args) < 2 || len(p.Call.Args) > 3 {
				return "", fmt.Errorf("substring expects 2 or 3 arguments at line %d", p.Pos.Line)
			}
			target, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			start, err := c.compileExpr(p.Call.Args[1])
			if err != nil {
				return "", err
			}
			if len(p.Call.Args) == 3 {
				end, err := c.compileExpr(p.Call.Args[2])
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("%s.substring(%s, %s)", target, start, end), nil
			}
			return fmt.Sprintf("%s.substring(%s)", target, start), nil
		case "append":
			if len(p.Call.Args) != 2 {
				return "", fmt.Errorf("append expects 2 arguments at line %d", p.Pos.Line)
			}
			c.helpers["append"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			a2, err := c.compileExpr(p.Call.Args[1])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("append(%s, %s)", a1, a2), nil
		case "count":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("count expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["count"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("count(%s)", a1), nil
		case "sum":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("sum expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["sum"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("sum(%s)", a1), nil
		case "avg":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("avg expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["avg"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("avg(%s)", a1), nil
		case "min":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("min expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["min"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("min(%s)", a1), nil
		case "max":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("max expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["max"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("max(%s)", a1), nil
		case "values":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("values expects 1 argument at line %d", p.Pos.Line)
			}
			c.helpers["values"] = true
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("values(%s)", a1), nil
		case "exists":
			if len(p.Call.Args) != 1 {
				return "", fmt.Errorf("exists expects 1 argument at line %d", p.Pos.Line)
			}
			// Special handling when argument is a query expression
			if q := p.Call.Args[0].Binary; q != nil && q.Left != nil && q.Left.Value != nil && q.Left.Value.Target != nil && q.Left.Value.Target.Query != nil {
				return c.compileExistsQuery(q.Left.Value.Target.Query)
			}
			a1, err := c.compileExpr(p.Call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("!%s.isEmpty()", a1), nil
		}
		var args []string
		for _, a := range p.Call.Args {
			arg, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, arg)
		}
		if t, ok := c.vars[p.Call.Func]; ok && t == "IntUnaryOperator" {
			return fmt.Sprintf("%s.applyAsInt(%s)", p.Call.Func, strings.Join(args, ", ")), nil
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	}
	return "", fmt.Errorf("expression unsupported at line %d", p.Pos.Line)
}

func copyMap(src map[string]string) map[string]string {
	dst := make(map[string]string, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func (c *Compiler) compileExistsQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	cond := "true"
	if q.Where != nil {
		oldVars := c.vars
		c.vars = copyMap(c.vars)
		c.vars[q.Var] = "var"
		cond, err = c.compileExpr(q.Where)
		c.vars = oldVars
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("%s.stream().anyMatch(%s -> %s)", src, q.Var, cond), nil
}
