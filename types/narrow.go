package types

import "mochi/parser"

// optionNarrowing inspects a boolean expression for `x == null` or
// `x != null` patterns and reports which option-typed bindings can be
// tightened to their wrapped element type in the truthy and falsy
// branches. It implements MEP-16 N1 (if-condition narrowing).
//
// The function does not mutate env; the caller applies the result by
// constructing narrowed child envs through narrowedEnv.
func optionNarrowing(e *parser.Expr, env *Env) (truthy, falsy map[string]Type) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 1 {
		return nil, nil
	}
	op := e.Binary.Right[0].Op
	if op != "==" && op != "!=" {
		return nil, nil
	}

	leftName := identFromUnary(e.Binary.Left)
	rightName := identFromUnary(e.Binary.Right[0].Right)
	leftNull := isNullLiteralUnary(e.Binary.Left)
	rightNull := isNullLiteralUnary(e.Binary.Right[0].Right)

	var bind string
	switch {
	case leftName != "" && rightNull:
		bind = leftName
	case rightName != "" && leftNull:
		bind = rightName
	default:
		return nil, nil
	}

	t, err := env.GetVar(bind)
	if err != nil {
		return nil, nil
	}
	opt, ok := t.(OptionType)
	if !ok {
		return nil, nil
	}
	inner := map[string]Type{bind: opt.Elem}
	if op == "!=" {
		return inner, nil
	}
	return nil, inner
}

// identFromUnary returns the binding name if u is a bare identifier
// (no postfix ops, no selector tail). It returns "" for anything more
// structured: stage 1 only narrows on simple names.
func identFromUnary(u *parser.Unary) string {
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return ""
	}
	px := u.Value
	if len(px.Ops) != 0 || px.Target == nil {
		return ""
	}
	sel := px.Target.Selector
	if sel == nil || len(sel.Tail) != 0 {
		return ""
	}
	return sel.Root
}

// isNullLiteralUnary reports whether u is the bare `null` literal.
func isNullLiteralUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	px := u.Value
	if len(px.Ops) != 0 || px.Target == nil {
		return false
	}
	lit := px.Target.Lit
	return lit != nil && lit.Null
}

// narrowedEnv returns a child env where each (name, type) in narrowed
// shadows the parent binding. The child preserves the parent's
// mutability flag for the binding. When narrowed is empty the parent
// env is returned unchanged so the common path stays allocation-free.
func narrowedEnv(env *Env, narrowed map[string]Type) *Env {
	if len(narrowed) == 0 {
		return env
	}
	child := NewEnv(env)
	for name, t := range narrowed {
		mut, _ := env.isMutable(name)
		child.SetVar(name, t, mut)
	}
	return child
}
