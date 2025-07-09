//go:build slow

package kotlin

import (
	"bytes"
	"fmt"
	"regexp"
	"strings"

	"mochi/parser"
	"mochi/types"
)

const runtime = `
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}

fun avg(list: List<Number>): Double {
    if (list.isEmpty()) return 0.0
    var s = 0.0
    for (n in list) s += n.toDouble()
    return s / list.size
}

fun count(list: Collection<Any?>): Int = list.size

fun exists(list: Collection<Any?>): Boolean = list.isNotEmpty()

fun len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> 0
}

fun max(list: List<Int>): Int {
    var m = Int.MIN_VALUE
    for (n in list) if (n > m) m = n
    return if (m == Int.MIN_VALUE) 0 else m
}

fun min(list: List<Int>): Int {
    var m = Int.MAX_VALUE
    for (n in list) if (n < m) m = n
    return if (m == Int.MAX_VALUE) 0 else m
}

fun sum(list: List<Int>): Int = list.sum()

fun str(v: Any?): String = v.toString()

fun substring(s: String, start: Int, end: Int): String = s.substring(start, end)

fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}

fun <T> union(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = a.toMutableList()
    for (x in b) if (!res.contains(x)) res.add(x)
    return res
}

fun <T> except(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (!b.contains(x)) res.add(x)
    return res
}

fun <T> intersect(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (b.contains(x)) res.add(x)
    return res
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
`

// Compiler converts a subset of Mochi programs to Kotlin source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

// New creates a new Kotlin compiler.
func New(env *types.Env, _ string) *Compiler { return &Compiler{env: env} }

// Compile generates Kotlin code from prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.writeln(runtime)
	c.writeln("")

	// emit type declarations first
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.typeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// emit global variable declarations before functions so they are
	// visible to all functions
	for _, s := range prog.Statements {
		if s.Type != nil || s.Fun != nil {
			continue
		}
		if s.Let != nil || s.Var != nil {
			if err := c.stmt(s); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.funDecl(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("fun main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil {
			continue
		}
		if s.Let != nil || s.Var != nil {
			// already emitted as global variable
			continue
		}
		if err := c.stmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val := "null"
		typ := ""
		if s.Let.Type != nil {
			typName := c.typeNameNullable(s.Let.Type)
			typ = ": " + typName
			val = c.zeroValue(s.Let.Type)
		}
		if s.Let.Value != nil {
			v, err := c.expr(s.Let.Value)
			if err != nil {
				return err
			}
			val = v
		}
		c.writeln(fmt.Sprintf("val %s%s = %s", s.Let.Name, typ, val))
	case s.Var != nil:
		val := "null"
		typ := ""
		if s.Var.Type != nil {
			typName := c.typeNameNullable(s.Var.Type)
			typ = ": " + typName
			val = c.zeroValue(s.Var.Type)
		}
		if s.Var.Value != nil {
			v, err := c.expr(s.Var.Value)
			if err != nil {
				return err
			}
			val = v
		}
		c.writeln(fmt.Sprintf("var %s%s = %s", s.Var.Name, typ, val))
	case s.Assign != nil:
		v, err := c.expr(s.Assign.Value)
		if err != nil {
			return err
		}
		target := s.Assign.Name
		for i, idx := range s.Assign.Index {
			idxVal, err := c.expr(idx.Start)
			if err != nil {
				return err
			}
			if i < len(s.Assign.Index)-1 || len(s.Assign.Field) > 0 {
				target += fmt.Sprintf("[%s]!!", idxVal)
			} else {
				target += fmt.Sprintf("[%s]", idxVal)
			}
		}
		for _, f := range s.Assign.Field {
			target += "." + f.Name
		}
		c.writeln(fmt.Sprintf("%s = %s", target, v))
	case s.Return != nil:
		v, err := c.expr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("return %s", v))
	case s.If != nil:
		return c.ifStmt(s.If)
	case s.While != nil:
		return c.whileStmt(s.While)
	case s.For != nil:
		return c.forStmt(s.For)
	case s.Break != nil:
		c.writeln("break")
	case s.Continue != nil:
		c.writeln("continue")
	case s.Type != nil:
		// type declarations are emitted before main
		return nil
	case s.Expr != nil:
		e, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(e)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) funDecl(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeName(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	ret := "Unit"
	if f.Return != nil {
		ret = c.typeName(f.Return)
	}
	c.writeln(fmt.Sprintf("fun %s(%s): %s {", f.Name, strings.Join(params, ", "), ret))
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) ifStmt(i *parser.IfStmt) error {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range i.Then {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if i.ElseIf != nil {
		c.writeln("else")
		return c.ifStmt(i.ElseIf)
	}
	if len(i.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.stmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) whileStmt(w *parser.WhileStmt) error {
	cond, err := c.expr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range w.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) forStmt(f *parser.ForStmt) error {
	var elem types.Type
	if f.RangeEnd != nil {
		start, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.expr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (%s in %s until %s) {", f.Name, start, end))
		elem = types.IntType{}
	} else {
		src, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		t := types.TypeOfExprBasic(f.Source, c.env)
		if types.IsMapType(t) {
			c.writeln(fmt.Sprintf("for (%s in %s.keys) {", f.Name, src))
			if mt, ok := t.(types.MapType); ok {
				elem = mt.Key
			}
		} else {
			c.writeln(fmt.Sprintf("for (%s in %s) {", f.Name, src))
			if lt, ok := t.(types.ListType); ok {
				elem = lt.Elem
			}
		}
	}

	oldEnv := c.env
	if elem != nil {
		c.env = types.NewEnv(c.env)
		c.env.SetVar(f.Name, elem, true)
	}

	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			if elem != nil {
				c.env = oldEnv
			}
			return err
		}
	}
	c.indent--
	c.writeln("}")

	if elem != nil {
		c.env = oldEnv
	}
	return nil
}

func (c *Compiler) typeDecl(t *parser.TypeDecl) error {
	if len(t.Members) == 0 {
		return nil
	}
	fields := make([]string, 0, len(t.Members))
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, fmt.Sprintf("var %s: %s", m.Field.Name, c.typeName(m.Field.Type)))
		}
	}
	c.writeln(fmt.Sprintf("data class %s(%s)", t.Name, strings.Join(fields, ", ")))
	return nil
}

func (c *Compiler) typeName(t *parser.TypeRef) string {
	if t == nil {
		return "Any"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.typeName(p)
		}
		ret := "Unit"
		if t.Fun.Return != nil {
			ret = c.typeName(t.Fun.Return)
		}
		return fmt.Sprintf("(%s) -> %s", strings.Join(params, ", "), ret)
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "string":
			return "String"
		case "bool":
			return "Boolean"
		}
		return strings.Title(*t.Simple)
	}
	return "Any"
}

func (c *Compiler) typeNameNullable(t *parser.TypeRef) string {
	name := c.typeName(t)
	if name == "Any" {
		return "Any?"
	}
	return name + "?"
}

func (c *Compiler) zeroValue(t *parser.TypeRef) string {
	if t == nil || t.Simple == nil {
		return "null"
	}
	switch *t.Simple {
	case "int":
		return "0"
	case "float":
		return "0.0"
	case "string":
		return "\"\""
	case "bool":
		return "false"
	default:
		return "null"
	}
}

func (c *Compiler) expr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.binary(e.Binary)
}

func (c *Compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	lType := types.TypeOfUnary(b.Left, c.env)
	res := left
	for _, op := range b.Right {
		r, err := c.postfix(op.Right)
		if err != nil {
			return "", err
		}
		rType := types.TypeOfPostfix(op.Right, c.env)
		if isNumericOp(op.Op) {
			if _, ok := lType.(types.AnyType); ok {
				switch rType.(type) {
				case types.IntType:
					res = fmt.Sprintf("toInt(%s)", res)
				case types.FloatType:
					res = fmt.Sprintf("toDouble(%s)", res)
				}
			}
			if _, ok := rType.(types.AnyType); ok {
				switch lType.(type) {
				case types.IntType:
					r = fmt.Sprintf("toInt(%s)", r)
				case types.FloatType:
					r = fmt.Sprintf("toDouble(%s)", r)
				}
			}
		}
		res = fmt.Sprintf("%s %s %s", res, op.Op, r)
		lType = rType
	}
	return res, nil
}

func (c *Compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) postfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	t := types.TypeOfPrimaryBasic(p.Target, c.env)
	for i, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
		case op.Index != nil:
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.expr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := ""
				if op.Index.End != nil {
					e, err := c.expr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if types.IsStringType(t) {
					if end == "" {
						end = fmt.Sprintf("%s.length", val)
					}
					val = fmt.Sprintf("%s.substring(%s, %s)", val, start, end)
				} else if types.IsListType(t) {
					if end == "" {
						end = fmt.Sprintf("%s.size", val)
					}
					val = fmt.Sprintf("%s.subList(%s, %s)", val, start, end)
				} else {
					return "", fmt.Errorf("unsupported slice type")
				}
			} else {
				idx, err := c.expr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if i < len(p.Ops)-1 {
					if types.IsMapType(t) {
						val = fmt.Sprintf("(%s[%s] as MutableMap<*, *>?)!!", val, idx)
						if mt, ok := t.(types.MapType); ok {
							t = mt.Value
						} else {
							t = types.AnyType{}
						}
					} else {
						val = fmt.Sprintf("%s[%s]!!", val, idx)
						if lt, ok := t.(types.ListType); ok {
							t = lt.Elem
						} else {
							t = types.AnyType{}
						}
					}
				} else {
					val = fmt.Sprintf("%s[%s]", val, idx)
				}
			}
		case op.Field != nil:
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
			switch tt := t.(type) {
			case types.StructType:
				if ft, ok := tt.Fields[op.Field.Name]; ok {
					t = ft
				} else {
					t = types.AnyType{}
				}
			case types.MapType:
				t = tt.Value
			default:
				t = types.AnyType{}
			}
		case op.Cast != nil:
			// special case: casting a map literal to a struct type
			if op.Cast.Type.Simple != nil {
				if _, ok := c.env.GetStruct(*op.Cast.Type.Simple); ok && p.Target.Map != nil && len(p.Ops) == 1 {
					fields := make([]string, len(p.Target.Map.Items))
					for i, it := range p.Target.Map.Items {
						k, err := c.expr(it.Key)
						if err != nil {
							return "", err
						}
						v, err := c.expr(it.Value)
						if err != nil {
							return "", err
						}
						fields[i] = fmt.Sprintf("%s = %s", strings.Trim(k, "\""), v)
					}
					val = fmt.Sprintf("%s(%s)", *op.Cast.Type.Simple, strings.Join(fields, ", "))
					break
				}
			}
			typ := c.typeName(op.Cast.Type)
			if typ == "Int" {
				val = fmt.Sprintf("(%s).toInt()", val)
			} else if typ == "String" {
				val = fmt.Sprintf("%s.toString()", val)
			} else {
				val = fmt.Sprintf("%s as %s", val, typ)
			}
		default:
			return "", fmt.Errorf("unsupported postfix")
		}
	}
	return val, nil
}

func (c *Compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.literal(p.Lit), nil
	case p.Selector != nil:
		name := p.Selector.Root
		t, _ := c.env.GetVar(p.Selector.Root)
		if types.IsMapType(t) || isStructType(t) {
			name = fmt.Sprintf("(%s as MutableMap<*, *>)", name)
			for i, part := range p.Selector.Tail {
				name += fmt.Sprintf("[%q]", part)
				if i == len(p.Selector.Tail)-1 {
					if kt := kotlinCastType(fieldType(t, p.Selector.Tail)); kt != "" {
						name += fmt.Sprintf(" as %s", kt)
					}
				} else {
					ft := fieldType(t, p.Selector.Tail[:i+1])
					if isStructType(ft) || types.IsMapType(ft) {
						name = fmt.Sprintf("(%s as MutableMap<*, *>)", name)
					}
				}
			}
		} else {
			if len(p.Selector.Tail) > 0 {
				name += "." + strings.Join(p.Selector.Tail, ".")
			}
		}
		return name, nil
	case p.Call != nil:
		return c.callExpr(p.Call)
	case p.Group != nil:
		s, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + s + ")", nil
	case p.List != nil:
		parts := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.expr(e)
			if err != nil {
				return "", err
			}
			parts[i] = s
		}
		return "mutableListOf(" + strings.Join(parts, ", ") + ")", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k string
			if name, ok := identName(it.Key); ok {
				k = fmt.Sprintf("%q", name)
			} else if it.Key.Binary.Left.Value.Target.Lit != nil && it.Key.Binary.Left.Value.Target.Lit.Str != nil {
				k = fmt.Sprintf("%q", *it.Key.Binary.Left.Value.Target.Lit.Str)
			} else {
				var err error
				k, err = c.expr(it.Key)
				if err != nil {
					return "", err
				}
			}
			v, err := c.expr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s to %s", k, v)
		}
		return "mutableMapOf(" + strings.Join(items, ", ") + ")", nil
	case p.Struct != nil:
		fields := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.expr(f.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s = %s", f.Name, v)
		}
		return fmt.Sprintf("%s(%s)", p.Struct.Name, strings.Join(fields, ", ")), nil
	case p.FunExpr != nil:
		return c.funExpr(p.FunExpr)
	case p.Query != nil:
		return c.queryExpr(p.Query)
	case p.If != nil:
		return c.ifExpr(p.If)
	case p.Match != nil:
		return c.matchExpr(p.Match)
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) callExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.expr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	if call.Func == "print" {
		if len(args) == 1 {
			return fmt.Sprintf("println(%s)", args[0]), nil
		}
		return fmt.Sprintf("println(listOf(%s).joinToString(\" \"))", strings.Join(args, ", ")), nil
	}
	return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
}

func (c *Compiler) literal(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		return fmt.Sprintf("%t", bool(*l.Bool))
	case l.Null:
		return "null"
	}
	return "null"
}

func (c *Compiler) funExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeName(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	if fn.ExprBody != nil {
		body, err := c.expr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("{ %s -> %s }", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) ifExpr(ix *parser.IfExpr) (string, error) {
	cond, err := c.expr(ix.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.expr(ix.Then)
	if err != nil {
		return "", err
	}
	if ix.ElseIf != nil {
		elseExpr, err := c.ifExpr(ix.ElseIf)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("if (%s) %s else %s", cond, thenExpr, elseExpr), nil
	}
	elseCode := "Unit"
	if ix.Else != nil {
		elseCode, err = c.expr(ix.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("if (%s) %s else %s", cond, thenExpr, elseCode), nil
}

func (c *Compiler) matchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.expr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("run {\n")
	b.WriteString("    val __t = " + target + "\n")
	b.WriteString("    when (__t) {\n")
	for _, cse := range m.Cases {
		res, err := c.expr(cse.Result)
		if err != nil {
			return "", err
		}
		if name, ok := identName(cse.Pattern); ok && name == "_" {
			b.WriteString("        else -> " + res + "\n")
			continue
		}
		pat, err := c.expr(cse.Pattern)
		if err != nil {
			return "", err
		}
		b.WriteString("        " + pat + " -> " + res + "\n")
	}
	b.WriteString("    }\n")
	b.WriteString("}")
	return b.String(), nil
}

func (c *Compiler) queryExpr(q *parser.QueryExpr) (string, error) {
	var b strings.Builder
	indent := func(n int) string { return strings.Repeat("    ", n) }
	lvl := 1

	src, err := c.expr(q.Source)
	if err != nil {
		return "", err
	}

	child := types.NewEnv(c.env)
	if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, true)
	} else {
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	for _, f := range q.Froms {
		if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	oldEnv := c.env
	c.env = child

	b.WriteString("run {\n")
	b.WriteString(indent(lvl))
	if q.Group != nil {
		b.WriteString("val __groups = mutableMapOf<Any?, Group>()\n")
		b.WriteString(indent(lvl))
		b.WriteString("val __order = mutableListOf<Any?>()\n")
	} else {
		b.WriteString("val __res = mutableListOf<Any>()\n")
	}
	b.WriteString(indent(lvl))
	b.WriteString(fmt.Sprintf("for (%s in %s) {\n", q.Var, src))
	lvl++
	for _, f := range q.Froms {
		s, err := c.expr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("for (%s in %s) {\n", f.Var, s))
		lvl++
	}
	if q.Where != nil {
		cond, err := c.expr(q.Where)
		if err != nil {
			return "", err
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("if (%s) {\n", cond))
		lvl++
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		return "", err
	}
	if q.Group != nil {
		keyExpr, err := c.expr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("val __k = %s\n", keyExpr))
		b.WriteString(indent(lvl))
		b.WriteString("var __g = __groups[__k]\n")
		b.WriteString(indent(lvl))
		b.WriteString("if (__g == null) {\n")
		lvl++
		b.WriteString(indent(lvl))
		b.WriteString("__g = Group(__k, mutableListOf())\n")
		b.WriteString(indent(lvl))
		b.WriteString("__groups[__k] = __g\n")
		b.WriteString(indent(lvl))
		b.WriteString("__order.add(__k)\n")
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__g.add(%s)\n", q.Var))
	} else {
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__res.add(%s)\n", sel))
	}
	if q.Where != nil {
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
	}
	for range q.Froms {
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
	}
	lvl--
	b.WriteString(indent(lvl))
	b.WriteString("}\n")
	if q.Group != nil {
		b.WriteString(indent(lvl))
		b.WriteString("val __res = mutableListOf<Any>()\n")
		b.WriteString(indent(lvl))
		b.WriteString("for (k in __order) {\n")
		lvl++
		b.WriteString(indent(lvl))
		b.WriteString("val g = __groups[k]!!\n")
		if q.Group.Name != "g" {
			b.WriteString(indent(lvl))
			b.WriteString(fmt.Sprintf("val %s = g\n", q.Group.Name))
		}
		b.WriteString(indent(lvl))
		b.WriteString(fmt.Sprintf("__res.add(%s)\n", sel))
		lvl--
		b.WriteString(indent(lvl))
		b.WriteString("}\n")
		b.WriteString(indent(lvl))
		b.WriteString("__res\n")
		b.WriteString("}")
	} else {
		b.WriteString(indent(lvl))
		b.WriteString("__res\n")
		b.WriteString("}")
	}

	res := b.String()
	if q.Sort != nil {
		sortExpr, err := c.expr(q.Sort)
		if err != nil {
			return "", err
		}
		vars := []string{q.Var}
		for _, f := range q.Froms {
			vars = append(vars, f.Var)
		}
		for _, v := range vars {
			sortExpr = replaceIdent(sortExpr, v, "it")
		}
		switch types.TypeOfExprBasic(q.Sort, c.env).(type) {
		case types.IntType:
			sortExpr += " as Int"
		case types.FloatType:
			sortExpr += " as Double"
		case types.StringType:
			sortExpr += " as String"
		default:
			sortExpr += " as Comparable<Any>"
		}
		if strings.HasPrefix(sortExpr, "-") {
			sortExpr = strings.TrimPrefix(sortExpr, "-")
			res += fmt.Sprintf(".sortedByDescending { %s }", sortExpr)
		} else {
			res += fmt.Sprintf(".sortedBy { %s }", sortExpr)
		}
	}
	if q.Skip != nil {
		skip, err := c.expr(q.Skip)
		if err != nil {
			return "", err
		}
		res += fmt.Sprintf(".drop(%s)", skip)
	}
	if q.Take != nil {
		take, err := c.expr(q.Take)
		if err != nil {
			return "", err
		}
		res += fmt.Sprintf(".take(%s)", take)
	}
	if q.Distinct {
		res += ".distinct()"
	}
	c.env = oldEnv
	return res, nil
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
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func replaceIdent(s, name, repl string) string {
	re := regexp.MustCompile(`\b` + regexp.QuoteMeta(name) + `\b`)
	return re.ReplaceAllString(s, repl)
}

func isNumericOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "<", "<=", ">", ">=":
		return true
	default:
		return false
	}
}

func fieldType(t types.Type, path []string) types.Type {
	for _, p := range path {
		switch tt := t.(type) {
		case types.StructType:
			if f, ok := tt.Fields[p]; ok {
				t = f
			} else {
				return types.AnyType{}
			}
		case types.MapType:
			t = tt.Value
		default:
			return types.AnyType{}
		}
	}
	return t
}

func kotlinCastType(t types.Type) string {
	switch t.(type) {
	case types.IntType:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "Boolean"
	default:
		return ""
	}
}

func isStructType(t types.Type) bool {
	_, ok := t.(types.StructType)
	return ok
}
