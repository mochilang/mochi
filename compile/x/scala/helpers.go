package scalacode

import (
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
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

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
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

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("\t", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}

func emptyListExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	p := e.Binary.Left.Value
	return len(p.Ops) == 0 && p.Target.List != nil && len(p.Target.List.Elems) == 0
}

// isStringExpr reports whether e is a string literal or an identifier with
// a string type according to env. It provides a minimal heuristic for built-in
// functions like len.
func isStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok && env != nil {
		if t, err := env.GetVar(name); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.Lit != nil && p.Target.Lit.Str != nil {
				return true
			}
		}
	}
	return false
}

// isMapExpr reports whether e is a map literal or an identifier with
// a map type according to env. It is used to decide between `.length` and
// `.size` for builtin functions like len().
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

func isStringPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func isListPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	return false
}

// isListExpr reports whether e is a list literal or a variable of list type.
func isListExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok && env != nil {
		if t, err := env.GetVar(name); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.List != nil {
				return true
			}
		}
	}
	return false
}

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	if la, ok := a.(types.ListType); ok {
		if lb, ok := b.(types.ListType); ok {
			return equalTypes(la.Elem, lb.Elem)
		}
	}
	return reflect.DeepEqual(a, b)
}

func isInt(t types.Type) bool {
	_, ok := t.(types.IntType)
	return ok
}

func isFloat(t types.Type) bool {
	_, ok := t.(types.FloatType)
	return ok
}

func isNumber(t types.Type) bool { return isInt(t) || isFloat(t) }

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}
