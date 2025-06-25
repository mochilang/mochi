package types

import "mochi/parser"

// IdentName returns the identifier name of expression e if e is a simple
// identifier expression. The bool result reports whether the name was
// successfully extracted.
func IdentName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

// IsListUnary reports whether unary expression u has static list type.
func IsListUnary(u *parser.Unary, env *Env) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return IsListPostfix(u.Value, env)
}

// IsListPostfix reports whether postfix expression p has static list type.
func IsListPostfix(p *parser.PostfixExpr, env *Env) bool {
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.List != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(ListType); ok {
				return true
			}
		}
	}
	return false
}
