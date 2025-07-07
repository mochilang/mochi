//go:build archived

package rbcode

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.TypeOfExprBasic.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	return types.TypeOfExprBasic(e, c.env)
}

// inferPostfixType delegates to types.TypeOfPostfixBasic.
func (c *Compiler) inferPostfixType(u *parser.Unary) types.Type {
	return types.TypeOfPostfixBasic(u, c.env)
}

// inferPrimaryType delegates to types.TypeOfPrimaryBasic.
func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	return types.TypeOfPrimaryBasic(p, c.env)
}
