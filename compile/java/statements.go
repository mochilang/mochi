package javacode

import (
	"fmt"

	"mochi/parser"
	"mochi/types"
)

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
		if expr != "" {
			c.writeln(expr + ";")
		}
		return nil
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	var t types.Type = types.AnyType{}
	if stmt.Type != nil {
		t = c.resolveTypeRef(stmt.Type)
	} else if stmt.Value != nil {
		t = c.inferExprType(stmt.Value)
	} else if c.env != nil {
		if tv, err := c.env.GetVar(stmt.Name); err == nil {
			t = tv
		}
	}
	if c.env != nil {
		c.env.SetVar(stmt.Name, t, false)
	}

	typStr := c.javaType(t)
	if typStr == "" {
		typStr = "var"
	}
	expr := ""
	if stmt.Value != nil {
		v, err := c.compileExprHint(stmt.Value, t)
		if err != nil {
			return err
		}
		expr = " = " + v
	}
	c.writeln(fmt.Sprintf("%s %s%s;", typStr, sanitizeName(stmt.Name), expr))
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	var t types.Type = types.AnyType{}
	if stmt.Type != nil {
		t = c.resolveTypeRef(stmt.Type)
	} else if stmt.Value != nil {
		t = c.inferExprTypeHint(stmt.Value, t)
	} else if c.env != nil {
		if tv, err := c.env.GetVar(stmt.Name); err == nil {
			t = tv
		}
	}
	if c.env != nil {
		c.env.SetVar(stmt.Name, t, true)
	}

	typStr := c.javaType(t)
	if typStr == "" {
		typStr = "var"
	}
	expr := ""
	if stmt.Value != nil {
		v, err := c.compileExprHint(stmt.Value, t)
		if err != nil {
			return err
		}
		expr = " = " + v
	}
	c.writeln(fmt.Sprintf("%s %s%s;", typStr, sanitizeName(stmt.Name), expr))
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	var t types.Type
	if c.env != nil {
		if tv, err := c.env.GetVar(stmt.Name); err == nil {
			t = tv
			if mt, ok := tv.(types.MapType); ok && len(stmt.Index) == 1 {
				key, err := c.compileExpr(stmt.Index[0].Start)
				if err != nil {
					return err
				}
				val, err := c.compileExprHint(stmt.Value, mt.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("%s.put(%s, %s);", lhs, key, val))
				return nil
			}
		}
	}
	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExprHint(stmt.Value, t)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileReturn(stmt *parser.ReturnStmt) error {
	expr, err := c.compileExprHint(stmt.Value, c.returnType)
	if err != nil {
		return err
	}
	c.writeln("return " + expr + ";")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	outName := name
	if name == "_" {
		outName = "__"
	}
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; %s++) {", outName, start, outName, end, outName))
		if c.env != nil {
			c.env.SetVar(stmt.Name, types.IntType{}, true)
		}
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	asString := false
	asMap := false
	if stmt.Source != nil && c.env != nil {
		if ident := stmt.Source.Binary.Left; ident != nil && len(ident.Ops) == 0 {
			p := ident.Value
			if p.Target != nil {
				if p.Target.Lit != nil && p.Target.Lit.Str != nil {
					asString = true
				}
				if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
					if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
						switch t.(type) {
						case types.StringType:
							asString = true
						case types.MapType:
							asMap = true
						}
					}
				}
			}
		}
	}
	if asString {
		src += ".toCharArray()"
	}
	if asMap {
		src += ".keySet()"
	}
	if c.env != nil {
		t := c.inferExprType(stmt.Source)
		switch tt := t.(type) {
		case types.ListType:
			c.env.SetVar(stmt.Name, tt.Elem, true)
		case types.MapType:
			c.env.SetVar(stmt.Name, tt.Key, true)
		case types.StringType:
			c.env.SetVar(stmt.Name, types.StringType{}, true)
		default:
			c.env.SetVar(stmt.Name, types.AnyType{}, true)
		}
	}
	c.writeln(fmt.Sprintf("for (var %s : %s) {", outName, src))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if stmt.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.buf.WriteString(" else {")
		c.buf.WriteByte('\n')
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln("static void " + name + "() {")
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
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.helpers["_expect"] = true
	c.writeln(fmt.Sprintf("expect(%s);", expr))
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}
