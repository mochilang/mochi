//go:build slow

package schemecode

import "mochi/parser"

func rootNameExpr(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return ""
	}
	return rootNameUnary(e.Binary.Left)
}

func rootNameUnary(u *parser.Unary) string {
	if u == nil {
		return ""
	}
	return rootNamePostfix(u.Value)
}

func rootNamePostfix(p *parser.PostfixExpr) string {
	if p == nil {
		return ""
	}
	if p.Target != nil && p.Target.Selector != nil {
		return p.Target.Selector.Root
	}
	return ""
}

// simpleStringKey returns the string value of e if it is a simple identifier or
// string literal. This mirrors helpers used by other backends so that map keys
// like { key: val } are emitted with quoted symbols.
func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func identName(e *parser.Expr) (string, bool) {
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
