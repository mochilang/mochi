//go:build slow

package scalacode

import (
	"bytes"
	"fmt"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
}

const indentStep = 2

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte(' ')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	name := strings.TrimSuffix(filepath.Base(prog.Pos.Filename), filepath.Ext(prog.Pos.Filename))
	if name == "" {
		name = "Main"
	}
	// emit user-defined types before the main object
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln(fmt.Sprintf("object %s {", name))
	c.indent += indentStep
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("def main(args: Array[String]): Unit = {")
	c.indent += indentStep
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	c.indent -= indentStep
	c.writeln("}")
	out := c.buf.Bytes()
	if len(out) == 0 || out[len(out)-1] != '\n' {
		out = append(out, '\n')
	}
	return out, nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Break != nil:
		c.writeln("return")
		return nil
	case s.Continue != nil:
		c.writeln("// continue")
		return nil
	case s.Expr != nil:
		return c.compileExprStmt(s.Expr)
	default:
		return fmt.Errorf("line %d: unsupported statement", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	rhs := "0"
	if s.Value != nil {
		var err error
		rhs, err = c.compileExpr(s.Value)
		if err != nil {
			return err
		}
	}
	if s.Type != nil {
		typ := c.typeString(s.Type)
		c.writeln(fmt.Sprintf("val %s: %s = %s", s.Name, typ, rhs))
	} else {
		c.writeln(fmt.Sprintf("val %s = %s", s.Name, rhs))
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	rhs := "0"
	if s.Value != nil {
		var err error
		// use mutable collections when assigning list or map literals
		if lst := s.Value.Binary.Left.Value.Target.List; lst != nil {
			rhs, err = c.compileList(lst, true)
		} else if mp := s.Value.Binary.Left.Value.Target.Map; mp != nil {
			rhs, err = c.compileMap(mp, true)
		} else {
			rhs, err = c.compileExpr(s.Value)
		}
		if err != nil {
			return err
		}
	}
	if s.Type != nil {
		typ := c.typeString(s.Type)
		c.writeln(fmt.Sprintf("var %s: %s = %s", s.Name, typ, rhs))
	} else {
		c.writeln(fmt.Sprintf("var %s = %s", s.Name, rhs))
	}
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		c.writeln("sealed trait " + td.Name)
		for _, v := range td.Variants {
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("case object %s extends %s", v.Name, td.Name))
				continue
			}
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = fmt.Sprintf("%s: %s", f.Name, c.typeString(f.Type))
			}
			c.writeln(fmt.Sprintf("case class %s(%s) extends %s", v.Name, strings.Join(fields, ", "), td.Name))
		}
		return nil
	}
	fields := []string{}
	for _, m := range td.Members {
		if m.Field != nil {
			f := m.Field
			fields = append(fields, fmt.Sprintf("%s: %s", f.Name, c.typeString(f.Type)))
		}
	}
	if len(fields) == 0 {
		return fmt.Errorf("line %d: empty type", td.Pos.Line)
	}
	c.writeln(fmt.Sprintf("case class %s(%s)", td.Name, strings.Join(fields, ", ")))
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeString(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	ret := ""
	if fn.Return != nil {
		ret = ": " + c.typeString(fn.Return)
	}
	c.writeln(fmt.Sprintf("def %s(%s)%s = {", fn.Name, strings.Join(params, ", "), ret))
	c.indent += indentStep
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	expr, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent += indentStep
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	if s.ElseIf != nil {
		c.writeln("} else {")
		c.indent += indentStep
		if err := c.compileIf(s.ElseIf); err != nil {
			return err
		}
		c.indent -= indentStep
		c.writeln("}")
		return nil
	}
	if s.Else != nil {
		c.writeln("} else {")
		c.indent += indentStep
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent -= indentStep
	}
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
	elseExpr := "()"
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
	return fmt.Sprintf("if (%s) %s else %s", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeString(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	if fn.ExprBody == nil {
		return "", fmt.Errorf("line %d: block lambdas not supported", fn.Pos.Line)
	}
	body, err := c.compileExpr(fn.ExprBody)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileWhile(s *parser.WhileStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent += indentStep
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(s *parser.ForStmt) error {
	start, err := c.compileExpr(s.Source)
	if err != nil {
		return err
	}
	loop := start
	if s.RangeEnd != nil {
		end, err := c.compileExpr(s.RangeEnd)
		if err != nil {
			return err
		}
		loop = fmt.Sprintf("%s to %s", start, end)
	}
	var elemType types.Type = types.AnyType{}
	if t := types.ExprType(s.Source, c.env); t != nil {
		if mt, ok := t.(types.MapType); ok {
			c.writeln(fmt.Sprintf("for((%s, _) <- %s) {", s.Name, loop))
			elemType = mt.Key
		} else {
			c.writeln(fmt.Sprintf("for(%s <- %s) {", s.Name, loop))
			if lt, ok := t.(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	} else {
		c.writeln(fmt.Sprintf("for(%s <- %s) {", s.Name, loop))
	}
	child := types.NewEnv(c.env)
	child.SetVar(s.Name, elemType, false)
	oldEnv := c.env
	c.env = child
	c.indent += indentStep
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = oldEnv
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	c.env = oldEnv
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	expr, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	target := s.Name
	for _, idx := range s.Index {
		if idx.Colon != nil || idx.Colon2 != nil {
			return fmt.Errorf("line %d: slice assignment unsupported", s.Pos.Line)
		}
		idxExpr := "0"
		if idx.Start != nil {
			var err error
			idxExpr, err = c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
		}
		target = fmt.Sprintf("%s(%s)", target, idxExpr)
	}
	for _, f := range s.Field {
		target += "." + f.Name
	}
	c.writeln(fmt.Sprintf("%s = %s", target, expr))
	return nil
}

func (c *Compiler) compileExprStmt(s *parser.ExprStmt) error {
	call, ok := callPattern(s.Expr)
	if ok && call.Func == "print" {
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return err
			}
			args[i] = fmt.Sprintf("(%s)", v)
		}
		if len(args) == 1 {
			c.writeln(fmt.Sprintf("println(%s)", args[0]))
		} else {
			c.writeln(fmt.Sprintf("println(%s)", strings.Join(args, " + \" \" + ")))
		}
		return nil
	}
	expr, err := c.compileExpr(s.Expr)
	if err != nil {
		return err
	}
	c.writeln(expr)
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	s, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	leftType := types.TypeOfUnary(e.Binary.Left, c.env)
	for _, op := range e.Binary.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightType := types.TypeOfPostfix(op.Right, c.env)
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			ls, rs := s, r
			if _, ok := leftType.(types.AnyType); ok {
				ls = fmt.Sprintf("(%s).asInstanceOf[Int]", s)
			}
			if _, ok := rightType.(types.AnyType); ok {
				rs = fmt.Sprintf("(%s).asInstanceOf[Int]", r)
			}
			s = fmt.Sprintf("%s %s %s", ls, op.Op, rs)
			leftType = types.AnyType{}
		case "in":
			ct := types.TypeOfPostfix(op.Right, c.env)
			switch ct.(type) {
			case types.MapType, types.ListType, types.StringType:
				s = fmt.Sprintf("%s.contains(%s)", r, s)
			default:
				return "", fmt.Errorf("line %d: unsupported operator %s", op.Pos.Line, op.Op)
			}
		default:
			return "", fmt.Errorf("line %d: unsupported operator %s", op.Pos.Line, op.Op)
		}
	}
	return s, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	s, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	valType := types.TypeOfPostfix(u.Value, c.env)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			switch valType.(type) {
			case types.IntType, types.Int64Type, types.FloatType:
				s = fmt.Sprintf("-%s", s)
			default:
				s = fmt.Sprintf("-(%s).asInstanceOf[Int]", s)
			}
		case "!":
			s = fmt.Sprintf("!%s", s)
		default:
			return "", fmt.Errorf("line %d: unsupported unary op %s", u.Pos.Line, op)
		}
	}
	return s, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	s, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	typ := types.TypeOfPrimary(p.Target, c.env)
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				val, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = val
			}
			s = fmt.Sprintf("%s(%s)", s, strings.Join(args, ", "))
			if ft, ok := typ.(types.FuncType); ok {
				typ = ft.Return
			} else {
				typ = types.AnyType{}
			}
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil {
				start := "0"
				if idx.Start != nil {
					start, err = c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
				}
				end := fmt.Sprintf("%s.length", s)
				if idx.End != nil {
					end, err = c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
				}
				if _, ok := typ.(types.StringType); ok {
					s = fmt.Sprintf("%s.substring(%s, %s)", s, start, end)
					typ = types.StringType{}
				} else {
					s = fmt.Sprintf("%s.slice(%s, %s)", s, start, end)
				}
			} else {
				idxExpr, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				if _, ok := typ.(types.StringType); ok {
					s = fmt.Sprintf("%s.charAt(%s)", s, idxExpr)
					typ = types.StringType{}
				} else {
					s = fmt.Sprintf("%s(%s)", s, idxExpr)
					switch tt := typ.(type) {
					case types.ListType:
						typ = tt.Elem
					case types.MapType:
						typ = tt.Value
					default:
						typ = types.AnyType{}
					}
				}
			}
		case op.Cast != nil:
			tstr := c.typeString(op.Cast.Type)
			if st, ok := c.env.GetStruct(tstr); ok && p.Target.Map != nil {
				str, err := c.mapToStruct(tstr, st, p.Target.Map)
				if err != nil {
					return "", err
				}
				s = str
				typ = st
			} else {
				switch tstr {
				case "Int":
					s = fmt.Sprintf("%s.toInt", s)
				case "Double":
					s = fmt.Sprintf("%s.toDouble", s)
				case "String":
					s = fmt.Sprintf("%s.toString", s)
				default:
					s = fmt.Sprintf("%s.asInstanceOf[%s]", s, tstr)
				}
				typ = types.ResolveTypeRef(op.Cast.Type, c.env)
			}
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return s, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", fmt.Errorf("nil primary")
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.List != nil:
		return c.compileList(p.List, false)
	case p.Map != nil:
		return c.compileMap(p.Map, false)
	case p.Struct != nil:
		return c.compileStructLit(p.Struct)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Selector != nil:
		return c.compileSelector(p.Selector), nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", expr), nil
	default:
		return "", fmt.Errorf("line %d: unsupported expression", p.Pos.Line)
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if *l.Bool {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	default:
		return "null"
	}
}

func (c *Compiler) compileSelector(s *parser.SelectorExpr) string {
	if len(s.Tail) == 0 {
		return s.Root
	}
	if t, err := c.env.GetVar(s.Root); err == nil {
		if _, ok := t.(types.MapType); ok {
			base := s.Root
			for i, f := range s.Tail {
				base = fmt.Sprintf("%s(%q)", base, f)
				if i < len(s.Tail)-1 {
					// after indexing into map, assume value is map
				}
			}
			return base
		}
	}
	parts := append([]string{s.Root}, s.Tail...)
	return strings.Join(parts, ".")
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch call.Func {
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("%s :+ %s", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("%s.sum.toDouble / %s.size", args[0], args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("%s.size", args[0]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		t := types.ExprType(call.Args[0], c.env)
		switch t.(type) {
		case types.MapType:
			return fmt.Sprintf("%s.size", args[0]), nil
		default:
			return fmt.Sprintf("%s.length", args[0]), nil
		}
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		return fmt.Sprintf("%s.min", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("%s.max", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("%s.sum", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("%s.toString", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("%s.substring(%s, %s)", args[0], args[1], args[2]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values.toList", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("(%s).nonEmpty", args[0]), nil
	default:
		if t, err := c.env.GetVar(call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok && len(args) < len(ft.Params) {
				missing := len(ft.Params) - len(args)
				names := make([]string, missing)
				params := make([]string, missing)
				for i := 0; i < missing; i++ {
					pname := fmt.Sprintf("p%d", i)
					names[i] = pname
					params[i] = fmt.Sprintf("%s: %s", pname, c.typeOf(ft.Params[len(args)+i]))
				}
				callArgs := append(append([]string{}, args...), names...)
				return fmt.Sprintf("(%s) => %s(%s)", strings.Join(params, ", "), call.Func, strings.Join(callArgs, ", ")), nil
			}
		}
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileList(l *parser.ListLiteral, mutable bool) (string, error) {
	elems := make([]string, len(l.Elems))
	for i, e := range l.Elems {
		s, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		elems[i] = s
	}
	prefix := "List"
	if mutable {
		prefix = "scala.collection.mutable.ArrayBuffer"
	}
	return fmt.Sprintf("%s(%s)", prefix, strings.Join(elems, ", ")), nil
}

func (c *Compiler) compileMap(m *parser.MapLiteral, mutable bool) (string, error) {
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		var k string
		if name, ok := simpleIdent(it.Key); ok {
			k = fmt.Sprintf("%q", name)
		} else {
			var err error
			k, err = c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("%s -> %s", k, v)
	}
	prefix := "Map"
	if mutable {
		prefix = "scala.collection.mutable.Map"
	}
	return fmt.Sprintf("%s(%s)", prefix, strings.Join(items, ", ")), nil
}

func (c *Compiler) compileStructLit(st *parser.StructLiteral) (string, error) {
	args := make([]string, len(st.Fields))
	for i, f := range st.Fields {
		val, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		args[i] = fmt.Sprintf("%s = %s", f.Name, val)
	}
	return fmt.Sprintf("%s(%s)", st.Name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	buf.WriteString(target + " match {\n")
	c.indent += indentStep
	for _, cs := range m.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		for i := 0; i < c.indent; i++ {
			buf.WriteString(" ")
		}
		buf.WriteString(fmt.Sprintf("case %s => %s\n", pat, res))
	}
	c.indent -= indentStep
	for i := 0; i < c.indent; i++ {
		buf.WriteString(" ")
	}
	buf.WriteString("}")
	return buf.String(), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 || q.Group != nil || q.Distinct {
		return "", fmt.Errorf("line %d: query features not supported", q.Pos.Line)
	}
	parts := []string{}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	parts = append(parts, fmt.Sprintf("%s <- %s", q.Var, src))

	child := types.NewEnv(c.env)
	if lt, ok := types.ExprType(q.Source, c.env).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, false)
	}

	for _, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		parts = append(parts, fmt.Sprintf("%s <- %s", f.Var, s))
		if lt, ok := types.ExprType(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, false)
		}
	}

	oldEnv := c.env
	c.env = child
	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		parts = append(parts, fmt.Sprintf("if %s", cond))
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = oldEnv
		return "", err
	}
	expr := fmt.Sprintf("for { %s } yield %s", strings.Join(parts, "; "), sel)
	if q.Sort != nil {
		key, err := c.compileExpr(q.Sort)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		expr = fmt.Sprintf("(%s).sortBy(%s => %s)", expr, q.Var, key)
	}
	if q.Skip != nil {
		val, err := c.compileExpr(q.Skip)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		expr = fmt.Sprintf("%s.drop(%s)", expr, val)
	}
	if q.Take != nil {
		val, err := c.compileExpr(q.Take)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		expr = fmt.Sprintf("%s.take(%s)", expr, val)
	}
	c.env = oldEnv
	return expr, nil
}

func (c *Compiler) mapToStruct(name string, st types.StructType, m *parser.MapLiteral) (string, error) {
	args := make([]string, len(st.Order))
	for i, field := range st.Order {
		var expr *parser.Expr
		for _, it := range m.Items {
			if lit := it.Key.Binary.Left.Value.Target.Lit; lit != nil && lit.Str != nil && *lit.Str == field {
				expr = it.Value
				break
			}
		}
		if expr == nil {
			return "", fmt.Errorf("missing field %s", field)
		}
		val, err := c.compileExpr(expr)
		if err != nil {
			return "", err
		}
		args[i] = fmt.Sprintf("%s = %s", field, val)
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) typeString(t *parser.TypeRef) string {
	if t == nil {
		return "Any"
	}
	if t.Simple != nil {
		id := *t.Simple
		switch id {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Boolean"
		case "string":
			return "String"
		default:
			return id
		}
	}
	if t.Generic != nil {
		g := t.Generic
		args := make([]string, len(g.Args))
		for i, a := range g.Args {
			args[i] = c.typeString(a)
		}
		return fmt.Sprintf("%s[%s]", g.Name, strings.Join(args, ", "))
	}
	if t.Fun != nil {
		parts := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			parts[i] = c.typeString(p)
		}
		ret := c.typeString(t.Fun.Return)
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	}
	return "Any"
}

func (c *Compiler) typeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return fmt.Sprintf("List[%s]", c.typeOf(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Map[%s, %s]", c.typeOf(tt.Key), c.typeOf(tt.Value))
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = c.typeOf(p)
		}
		ret := c.typeOf(tt.Return)
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), ret)
	default:
		return "Any"
	}
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

func simpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}
