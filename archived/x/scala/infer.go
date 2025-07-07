//go:build archived

package scalacode

import (
	"mochi/parser"
	"mochi/types"
)

// exprType returns the static type of expression e using the compiler's env.
func (c *Compiler) exprType(e *parser.Expr) types.Type {
	return types.ExprType(e, c.env)
}

// unaryType infers the type of a unary expression.
func (c *Compiler) unaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: u}}
	return types.ExprType(expr, c.env)
}

// postfixType infers the type of a postfix expression.
func (c *Compiler) postfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	unary := &parser.Unary{Value: p}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: unary}}
	return types.ExprType(expr, c.env)
}

// primaryType infers the type of a primary expression.
func (c *Compiler) primaryType(p *parser.Primary) types.Type {
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
