package types

import "mochi/parser"

// TypeOfExpr returns the static type of expression e using env.
// It mirrors ExprType but follows Go naming conventions.
func TypeOfExpr(e *parser.Expr, env *Env) Type {
	return ExprType(e, env)
}

// TypeOfExprHint infers the type of e using a hint for list literals.
func TypeOfExprHint(e *parser.Expr, hint Type, env *Env) Type {
	return ExprTypeHint(e, hint, env)
}

// TypeOfBinary exposes inferBinaryType.
func TypeOfBinary(b *parser.BinaryExpr, env *Env) Type {
	return inferBinaryType(env, b)
}

// TypeOfUnary exposes inferUnaryType.
func TypeOfUnary(u *parser.Unary, env *Env) Type {
	return inferUnaryType(env, u)
}

// TypeOfPostfix exposes inferPostfixType.
func TypeOfPostfix(p *parser.PostfixExpr, env *Env) Type {
	return inferPostfixType(env, p)
}

// TypeOfPrimary exposes inferPrimaryType.
func TypeOfPrimary(p *parser.Primary, env *Env) Type {
	return inferPrimaryType(env, p)
}
