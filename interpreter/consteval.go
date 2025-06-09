package interpreter

import (
	"mochi/parser"
	"mochi/types"
)

// EvalPureCall evaluates a pure function call with literal arguments using a temporary interpreter.
func EvalPureCall(call *parser.CallExpr, env *types.Env) (*parser.Literal, bool) {
	if call == nil {
		return nil, false
	}
	t, err := env.GetVar(call.Func)
	if err != nil {
		return nil, false
	}
	ft, ok := t.(types.FuncType)
	if !ok || !ft.Pure {
		return nil, false
	}
	for _, arg := range call.Args {
		if !types.IsLiteralExpr(arg) {
			return nil, false
		}
	}
	interp := New(&parser.Program{}, env.Copy())
	// Avoid recursive const-evaluation when interpreting the call.
	interp.disablePureEval = true
	val, err := interp.EvalExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Call: call}}}}})
	if err != nil {
		return nil, false
	}
	lit := types.AnyToLiteral(val)
	if lit == nil {
		return nil, false
	}
	return lit, true
}
