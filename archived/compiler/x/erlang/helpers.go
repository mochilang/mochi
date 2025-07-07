//go:build archived

package erlcode

import (
	"fmt"
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

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

func isFloat(t types.Type) bool {
	_, ok := t.(types.FloatType)
	return ok
}

func isBool(t types.Type) bool {
	_, ok := t.(types.BoolType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isList(t types.Type) bool {
	_, ok := t.(types.ListType)
	return ok
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

func isAny(t types.Type) bool {
	if t == nil {
		return true
	}
	_, ok := t.(types.AnyType)
	return ok
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
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

// atomName returns a valid Erlang atom representing name. If name contains
// uppercase letters or other characters that are not allowed in unquoted atoms,
// it is returned quoted.
func atomName(name string) string {
	if name == "" {
		return name
	}
	for i, r := range name {
		if i == 0 {
			if r < 'a' || r > 'z' {
				return fmt.Sprintf("'%s'", name)
			}
		} else {
			if (r < 'a' || r > 'z') && (r < '0' || r > '9') && r != '_' {
				return fmt.Sprintf("'%s'", name)
			}
		}
	}
	return name
}

// sanitizeName converts name to a valid Erlang atom identifier.
func sanitizeName(name string) string {
	if name == "" {
		return "_"
	}
	b := strings.Builder{}
	for i, r := range name {
		if r >= 'A' && r <= 'Z' {
			r = r - 'A' + 'a'
		}
		if (r >= 'a' && r <= 'z') || (r >= '0' && r <= '9' && i > 0) || r == '_' {
			b.WriteRune(r)
		} else {
			b.WriteByte('_')
		}
	}
	res := b.String()
	if res == "" || !((res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
		res = "_" + res
	}
	return res
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: c.resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: c.resolveTypeRef(args[0]), Value: c.resolveTypeRef(args[1])}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func unionFieldPathType(ut types.UnionType, tail []string) (types.Type, bool) {
	var result types.Type
	for _, variant := range ut.Variants {
		cur := types.Type(variant)
		for _, field := range tail {
			st, ok := cur.(types.StructType)
			if !ok {
				return nil, false
			}
			ft, ok := st.Fields[field]
			if !ok {
				return nil, false
			}
			cur = ft
		}
		if result == nil {
			result = cur
		} else if !equalTypes(result, cur) {
			return nil, false
		}
	}
	if result == nil {
		return nil, false
	}
	return result, true
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
