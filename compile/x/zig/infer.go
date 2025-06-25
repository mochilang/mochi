package zigcode

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
