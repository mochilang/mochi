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
			}
		}
	}
	return ""
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	return c.isStringUnary(e.Binary.Left)
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if c.isStringPrimary(p.Target) {
		return true
	}
	if p.Target != nil && p.Target.Selector != nil {
		return c.varType(p.Target.Selector.Root) == "string"
	}
	return false
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	return p != nil && p.Lit != nil && p.Lit.Str != nil
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	return c.isMapUnary(e.Binary.Left)
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isMapPostfix(u.Value)
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	if c.isMapPrimary(p.Target) {
		return true
	}
	if p.Target != nil && p.Target.Selector != nil {
		return c.varType(p.Target.Selector.Root) == "map"
	}
	return false
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	return p != nil && p.Map != nil
}
