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
