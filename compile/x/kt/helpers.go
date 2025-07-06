package ktcode

import (
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || ('0' <= r && r <= '9' && i > 0) {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" {
		return "_"
	}
	if !(s[0] >= 'a' && s[0] <= 'z' || s[0] >= 'A' && s[0] <= 'Z' || s[0] == '_') {
		s = "_" + s
	}
	return s
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func isStringPrimary(p *parser.Primary, env *types.Env) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	case p.Call != nil && p.Call.Func == "str":
		return true
	case p.Selector != nil && env != nil:
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isStringPrimary(p.Target, env)
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isStringPostfix(u.Value, env)
}

func isString(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnary(e.Binary.Left, env)
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

func simpleStringKey(e *parser.Expr) (string, bool) {
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
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

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
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
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

func isListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListPrimary(p.Target, env)
}

func isListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isListPostfix(u.Value, env)
}

func isList(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	return isListUnary(e.Binary.Left, env)
}

func isMapPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func isMapPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isMapPrimary(p.Target, env)
}

func isMapUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil {
		return false
	}
	return isMapPostfix(u.Value, env)
}

func isMap(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	return isMapUnary(e.Binary.Left, env)
}

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("        ", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}

func isFetchExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
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
	return p.Target.Fetch != nil
}

func isEmptyListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	if ll := e.Binary.Left.Value.Target.List; ll != nil {
		return len(ll.Elems) == 0
	}
	return false
}

func isEmptyMapExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	if ml := e.Binary.Left.Value.Target.Map; ml != nil {
		return len(ml.Items) == 0
	}
	return false
}

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func containsAny(t types.Type) bool {
	switch tt := t.(type) {
	case types.AnyType:
		return true
	case types.ListType:
		return containsAny(tt.Elem)
	case types.MapType:
		return containsAny(tt.Key) || containsAny(tt.Value)
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
	if la, ok := a.(types.ListType); ok {
		if lb, ok := b.(types.ListType); ok {
			return equalTypes(la.Elem, lb.Elem)
		}
	}
	if ma, ok := a.(types.MapType); ok {
		if mb, ok := b.(types.MapType); ok {
			return equalTypes(ma.Key, mb.Key) && equalTypes(ma.Value, mb.Value)
		}
	}
	if ua, ok := a.(types.UnionType); ok {
		if sb, ok := b.(types.StructType); ok {
			if _, ok := ua.Variants[sb.Name]; ok {
				return true
			}
		}
	}
	if ub, ok := b.(types.UnionType); ok {
		if sa, ok := a.(types.StructType); ok {
			if _, ok := ub.Variants[sa.Name]; ok {
				return true
			}
		}
	}
	if isInt64(a) && (isInt64(b) || isInt(b)) {
		return true
	}
	if isInt64(b) && (isInt64(a) || isInt(a)) {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	return reflect.DeepEqual(a, b)
}

func isInt64(t types.Type) bool { _, ok := t.(types.Int64Type); return ok }
func isInt(t types.Type) bool   { _, ok := t.(types.IntType); return ok }
