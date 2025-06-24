package gocode

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.InferExprType.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	return types.InferExprType(e, c.env)
}

// inferExprTypeHint delegates to types.InferExprTypeHint.
func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
	return types.InferExprTypeHint(e, hint, c.env)
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
	return types.InferExprType(expr, c.env)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	unary := &parser.Unary{Value: p}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return types.InferExprType(expr, c.env)
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	postfix := &parser.PostfixExpr{Target: p}
	unary := &parser.Unary{Value: postfix}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return types.InferExprType(expr, c.env)
}

func resultType(op string, left, right types.Type) types.Type {
	return types.ResultType(op, left, right)
}
