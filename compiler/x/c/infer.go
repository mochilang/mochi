//go:build slow

package ccode

import (
	"mochi/parser"
	"mochi/types"
)

// exprType delegates to types.ExprType.
func (c *Compiler) exprType(e *parser.Expr) types.Type {
	if c.env == nil {
		return types.ExprType(e, nil)
	}
	return types.CheckExprType(e, c.env)
}

func (c *Compiler) unaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
	if c.env == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, c.env)
}

func (c *Compiler) postfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	unary := &parser.Unary{Value: p}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	if c.env == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, c.env)
}

func (c *Compiler) primaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	postfix := &parser.PostfixExpr{Target: p}
	unary := &parser.Unary{Value: postfix}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	if c.env == nil {
		return types.ExprType(expr, nil)
	}
	return types.CheckExprType(expr, c.env)
}

func resultType(op string, left, right types.Type) types.Type {
	return types.ResultType(op, left, right)
}

func (c *Compiler) inferFunReturnType(body []*parser.Statement) types.Type {
	var ret types.Type
	for _, st := range body {
		if st.Return != nil {
			t := c.exprType(st.Return.Value)
			if ret == nil {
				ret = t
			} else if !equalTypes(ret, t) {
				return types.AnyType{}
			}
		}
	}
	if ret == nil {
		return types.VoidType{}
	}
	return ret
}
