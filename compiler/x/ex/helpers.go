//go:build slow

package excode

import (
	"mochi/parser"
)

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	return p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
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

// isConstExpr returns true if the expression is a compile-time constant.
func isConstExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	return isConstUnary(e.Binary.Left)
}

func isConstUnary(u *parser.Unary) bool {
	if len(u.Ops) != 0 {
		return false
	}
	return isConstPostfix(u.Value)
}

func isConstPostfix(p *parser.PostfixExpr) bool {
	if len(p.Ops) != 0 {
		return false
	}
	return isConstPrimary(p.Target)
}

func isConstPrimary(p *parser.Primary) bool {
	switch {
	case p.Lit != nil:
		return true
	case p.Group != nil:
		return isConstExpr(p.Group)
	case p.List != nil:
		for _, e := range p.List.Elems {
			if !isConstExpr(e) {
				return false
			}
		}
		return true
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if !isConstExpr(it.Key) || !isConstExpr(it.Value) {
				return false
			}
		}
		return true
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			if !isConstExpr(f.Value) {
				return false
			}
		}
		return true
	default:
		return false
	}
}
