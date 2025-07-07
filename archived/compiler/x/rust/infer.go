//go:build archived

package rscode

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.ExprType.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 {
				if p.Target.List != nil {
					var elem types.Type = types.AnyType{}
					for i, el := range p.Target.List.Elems {
						t := c.inferExprType(el)
						if i == 0 {
							elem = t
						} else if !equalTypes(elem, t) {
							elem = types.AnyType{}
							break
						}
					}
					return types.ListType{Elem: elem}
				}
				if p.Target.Map != nil {
					var key types.Type = types.AnyType{}
					var val types.Type = types.AnyType{}
					for i, it := range p.Target.Map.Items {
						kt := c.inferExprType(it.Key)
						if _, ok := identName(it.Key); ok {
							kt = types.StringType{}
						}
						vt := c.inferExprType(it.Value)
						if i == 0 {
							key = kt
							val = vt
						} else {
							if !equalTypes(key, kt) {
								key = types.AnyType{}
							}
							if !equalTypes(val, vt) {
								val = types.AnyType{}
							}
						}
					}
					return types.MapType{Key: key, Value: val}
				}
			}
		}
	}

	env := types.NewEnv(c.env)
	for name, tt := range c.locals {
		env.SetVar(name, tt, true)
	}
	return types.CheckExprType(e, env)
}

// inferExprTypeHint delegates to types.ExprTypeHint.
func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value.Target.List != nil && len(u.Value.Target.List.Elems) == 0 {
			if lt, ok := hint.(types.ListType); ok {
				return types.ListType{Elem: lt.Elem}
			}
		}
	}
	return c.inferExprType(e)
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
	return types.ExprType(expr, c.env)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	unary := &parser.Unary{Value: p}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return types.ExprType(expr, c.env)
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	postfix := &parser.PostfixExpr{Target: p}
	unary := &parser.Unary{Value: postfix}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return types.ExprType(expr, c.env)
}

func resultType(op string, left, right types.Type) types.Type {
	return types.ResultType(op, left, right)
}
