//go:build archived

package ftncode

import "mochi/parser"

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
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func loadTypeName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	v := u.Value
	if len(v.Ops) != 0 || v.Target == nil || v.Target.Load == nil {
		return "", false
	}
	if v.Target.Load.Type != nil && v.Target.Load.Type.Simple != nil {
		return sanitizeName(*v.Target.Load.Type.Simple), true
	}
	return "", false
}

func isBuiltin(name string) bool {
	switch name {
	case "int", "float", "string", "bool":
		return true
	default:
		return false
	}
}
