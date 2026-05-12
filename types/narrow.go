package types

import "mochi/parser"

// optionNarrowing inspects a boolean expression and reports which
// option-typed bindings can be tightened to their wrapped element type
// in the truthy and falsy branches. It implements MEP-16 N1 (if-cond
// narrowing) and N2 (propagation through `&&` and `||`).
//
// The function does not mutate env; the caller applies the result by
// constructing narrowed child envs through narrowedEnv.
func optionNarrowing(e *parser.Expr, env *Env) (truthy, falsy map[string]Type) {
	if e == nil {
		return nil, nil
	}
	return optionNarrowingBinary(e.Binary, env)
}

// optionNarrowingBinary is the recursive worker that walks a binary
// expression tree. It splits the chain at the lowest-precedence boolean
// operator and combines the per-side narrowings using the truth-table
// rules from MEP-16 §N2.
func optionNarrowingBinary(b *parser.BinaryExpr, env *Env) (truthy, falsy map[string]Type) {
	if b == nil {
		return nil, nil
	}
	if idx := lastLogicalIndex(b, "||"); idx >= 0 {
		lhs, rhs := splitBinaryAt(b, idx)
		lt, lf := optionNarrowingBinary(lhs, env)
		// MEP-16 N2: when `||` is false, both sides are false. The RHS
		// is checked in the LHS falsy-narrowed env so chained patterns
		// like `x == none || y == none` accumulate narrowings.
		rt, rf := optionNarrowingBinary(rhs, narrowedEnv(env, lf))
		_, _ = lt, rt
		return nil, mergeNarrowings(lf, rf)
	}
	if idx := lastLogicalIndex(b, "&&"); idx >= 0 {
		lhs, rhs := splitBinaryAt(b, idx)
		lt, lf := optionNarrowingBinary(lhs, env)
		rt, rf := optionNarrowingBinary(rhs, narrowedEnv(env, lt))
		_, _ = lf, rf
		return mergeNarrowings(lt, rt), nil
	}
	if len(b.Right) != 1 {
		return nil, nil
	}
	op := b.Right[0].Op
	if op != "==" && op != "!=" {
		return nil, nil
	}

	leftName := identFromUnary(b.Left)
	rightName := identFromUnary(b.Right[0].Right)
	leftNone := isNoneLiteralUnary(b.Left)
	rightNone := isNoneLiteralUnary(b.Right[0].Right)

	var bind string
	switch {
	case leftName != "" && rightNone:
		bind = leftName
	case rightName != "" && leftNone:
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

// lastLogicalIndex returns the index of the rightmost occurrence of op
// in b.Right, or -1 when absent. Splitting at the rightmost match keeps
// the chain left-associative.
func lastLogicalIndex(b *parser.BinaryExpr, op string) int {
	for i := len(b.Right) - 1; i >= 0; i-- {
		if b.Right[i].Op == op {
			return i
		}
	}
	return -1
}

// splitBinaryAt slices b into the sub-binaries that flank the operator
// at idx. The returned binaries share AST nodes with b; callers must
// not mutate them.
func splitBinaryAt(b *parser.BinaryExpr, idx int) (lhs, rhs *parser.BinaryExpr) {
	lhs = &parser.BinaryExpr{Left: b.Left, Right: b.Right[:idx]}
	rhs = &parser.BinaryExpr{Left: b.Right[idx].Right, Right: b.Right[idx+1:]}
	return lhs, rhs
}

// mergeNarrowings unions two narrowing maps. When the same binding
// appears on both sides the right map wins; in practice both sides
// agree because they narrow Option<T> to the same T.
func mergeNarrowings(a, b map[string]Type) map[string]Type {
	if len(a) == 0 {
		return b
	}
	if len(b) == 0 {
		return a
	}
	out := make(map[string]Type, len(a)+len(b))
	for k, v := range a {
		out[k] = v
	}
	for k, v := range b {
		out[k] = v
	}
	return out
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

// isNoneLiteralUnary reports whether u is the bare `none` literal.
func isNoneLiteralUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	px := u.Value
	if len(px.Ops) != 0 || px.Target == nil {
		return false
	}
	lit := px.Target.Lit
	return lit != nil && lit.None
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
