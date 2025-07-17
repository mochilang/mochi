//go:build slow

package gocode

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.ExprType.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	return types.ExprType(e, c.env)
}

// inferExprTypeHint delegates to types.ExprTypeHint.
func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		if ll := e.Binary.Left.Value.Target.List; ll != nil {
			if lt, ok := hint.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					if inf, ok := types.InferStructFromList(ll, c.env); ok && types.StructMatches(st, inf.Fields, inf.Order) {
						return lt
					}
				}
			}
		}
		if ml := e.Binary.Left.Value.Target.Map; ml != nil {
			if st, ok := hint.(types.StructType); ok {
				if inf, ok := types.InferStructFromMapEnv(ml, c.env); ok && types.StructMatches(st, inf.Fields, inf.Order) {
					return st
				}
			}
		}
	}
	return types.ExprTypeHint(e, hint, c.env)
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
