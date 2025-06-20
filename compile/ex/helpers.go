package excode

import (
	"mochi/parser"
	"mochi/types"
	"reflect"
)

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
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

func isInt64(t types.Type) bool {
	_, ok := t.(types.Int64Type)
	return ok
}

func isInt(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return true
	default:
		return false
	}
}

func isFloat(t types.Type) bool  { _, ok := t.(types.FloatType); return ok }
func isBool(t types.Type) bool   { _, ok := t.(types.BoolType); return ok }
func isString(t types.Type) bool { _, ok := t.(types.StringType); return ok }
func isAny(t types.Type) bool {
	if t == nil {
		return true
	}
	_, ok := t.(types.AnyType)
	return ok
}

// isUnderscoreExpr reports whether e is the standalone '_' identifier.
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

// identName returns the identifier string if e is a bare identifier.
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
