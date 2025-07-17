//go:build slow

package kotlin

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.ExprType.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e != nil && e.Binary != nil {
		u := e.Binary.Left
		if len(e.Binary.Right) == 0 && len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
			if q := u.Value.Target.Query; q != nil {
				if t, ok := c.queryTypes[q]; ok {
					return t
				}
			}
		}
	}
	return types.ExprType(e, c.env)
}

// inferExprTypeHint delegates to types.ExprTypeHint.
func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
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
