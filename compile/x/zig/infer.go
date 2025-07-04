package zigcode

import (
	"mochi/parser"
	"mochi/types"
)

// inferExprType delegates to types.ExprType.
func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	t := types.ExprType(e, c.env)
	if types.ContainsAny(t) {
		env := types.NewEnv(c.env)
		for name, tt := range c.locals {
			env.SetVar(name, tt, true)
		}
		t = types.CheckExprType(e, env)
	}
	return t
}

// inferExprTypeHint delegates to types.ExprTypeHint.
func (c *Compiler) inferExprTypeHint(e *parser.Expr, hint types.Type) types.Type {
	t := types.ExprTypeHint(e, hint, c.env)
	if types.ContainsAny(t) {
		env := types.NewEnv(c.env)
		for name, tt := range c.locals {
			env.SetVar(name, tt, true)
		}
		t = types.CheckExprType(e, env)
	}
	return t
}
