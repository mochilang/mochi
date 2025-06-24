package stcode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	return b.String()
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
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

func isMapExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok && env != nil {
		if t, err := env.GetVar(name); err == nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.Map != nil {
				return true
			}
		}
	}
	return false
}
