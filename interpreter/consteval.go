package interpreter

import (
	"mochi/parser"
	"mochi/runtime/mod"
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
	modRoot, _ := mod.FindRoot(".")
	interp := New(&parser.Program{}, env.Copy(), modRoot)
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

// EvalConstExpr evaluates a constant, side-effect free expression using a
// temporary interpreter. The expression must only reference immutable values or
// pure functions.
func EvalConstExpr(expr *parser.Expr, env *types.Env) (*parser.Literal, bool) {
	if expr == nil {
		return nil, false
	}
	if !isConstExpr(expr, env) {
		return nil, false
	}
	modRoot, _ := mod.FindRoot(".")
	interp := New(&parser.Program{}, env.Copy(), modRoot)
	val, err := interp.EvalExpr(expr)
	if err != nil {
		return nil, false
	}
	lit := types.AnyToLiteral(val)
	if lit == nil {
		return nil, false
	}
	return lit, true
}

func isConstExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	if call, ok := callPattern(e); ok {
		return isConstCall(call, env)
	}
	if !isConstUnary(e.Binary.Left, env) {
		return false
	}
	for _, op := range e.Binary.Right {
		// BinaryOp.Right is a *parser.PostfixExpr, so check the postfix
		// expression directly for constant-ness.
		if !isConstPostfix(op.Right, env) {
			return false
		}
	}
	return true
}

func isConstUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	if !isConstPostfix(u.Value, env) {
		return false
	}
	return true
}

func isConstPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil {
		return false
	}
	if !isConstPrimary(p.Target, env) {
		return false
	}
	// Ignore postfix operations for now as they may have side effects
	return len(p.Ops) == 0
}

func isConstCall(call *parser.CallExpr, env *types.Env) bool {
	t, err := env.GetVar(call.Func)
	if err != nil {
		return false
	}
	ft, ok := t.(types.FuncType)
	if !ok || !ft.Pure {
		return false
	}
	for _, a := range call.Args {
		if !isConstExpr(a, env) {
			return false
		}
	}
	return true
}

func isConstPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p.Lit != nil:
		return true
	case p.Group != nil:
		return isConstExpr(p.Group, env)
	case p.List != nil:
		for _, e := range p.List.Elems {
			if !isConstExpr(e, env) {
				return false
			}
		}
		return true
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if !isConstExpr(it.Key, env) || !isConstExpr(it.Value, env) {
				return false
			}
		}
		return true
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			if !isConstExpr(f.Value, env) {
				return false
			}
		}
		return true
	case p.Call != nil:
		return isConstCall(p.Call, env)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			mutable, err := env.IsMutable(p.Selector.Root)
			if err == nil && !mutable {
				if val, err := env.GetValue(p.Selector.Root); err == nil {
					if types.AnyToLiteral(val) != nil {
						return true
					}
				}
			}
		}
		return false
	default:
		return false
	}
}
