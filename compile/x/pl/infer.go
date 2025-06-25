package plcode

import (
	"mochi/parser"
	"mochi/types"
)

// exprType delegates to types.TypeOfExpr.
func (c *Compiler) exprType(e *parser.Expr) types.Type {
	return types.TypeOfExpr(e, c.env)
}

func (c *Compiler) postfixType(p *parser.PostfixExpr) types.Type {
	return types.TypeOfPostfix(p, c.env)
}

func (c *Compiler) unaryType(u *parser.Unary) types.Type {
	return types.TypeOfUnary(u, c.env)
}

func (c *Compiler) primaryType(p *parser.Primary) types.Type {
	return types.TypeOfPrimary(p, c.env)
}
