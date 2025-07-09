//go:build slow

package schemecode

import (
	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) varType(name string) string {
	if t, ok := c.vars[name]; ok {
		return t
	}
	if c.env != nil {
		if tt, err := c.env.GetVar(name); err == nil {
			switch tt.(type) {
			case types.StringType:
				return "string"
			case types.MapType:
				return "map"
			case types.StructType:
				return "map"
			}
		}
	}
	return ""
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if types.IsStringExpr(e, c.env) {
		return true
	}
	root := rootNameExpr(e)
	return c.varType(root) == "string"
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if types.IsStringUnary(u, c.env) {
		return true
	}
	root := rootNameUnary(u)
	return c.varType(root) == "string"
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if types.IsStringPostfix(p, c.env) {
		return true
	}
	root := rootNamePostfix(p)
	return c.varType(root) == "string"
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if types.IsStringPrimary(p, c.env) {
		return true
	}
	if p != nil && p.Selector != nil {
		return c.varType(p.Selector.Root) == "string"
	}
	return false
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if types.IsMapExpr(e, c.env) {
		return true
	}
	root := rootNameExpr(e)
	return c.varType(root) == "map"
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	if types.IsMapUnary(u, c.env) {
		return true
	}
	root := rootNameUnary(u)
	return c.varType(root) == "map"
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if types.IsMapPostfix(p, c.env) {
		return true
	}
	root := rootNamePostfix(p)
	return c.varType(root) == "map"
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	if types.IsMapPrimary(p, c.env) {
		return true
	}
	if p != nil && p.Selector != nil {
		return c.varType(p.Selector.Root) == "map"
	}
	return false
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok && c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.List != nil {
				return true
			}
		}
	}
	return false
}
