package zigcode

import (
	"fmt"
	"reflect"

	"mochi/parser"
	"mochi/types"
)

func zigTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "i32"
	case types.Int64Type:
		return "i64"
	case types.FloatType:
		return "f64"
	case types.StringType:
		return "[]const u8"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "[]const " + zigTypeOf(tt.Elem)
	case types.MapType:
		return fmt.Sprintf("std.AutoHashMap(%s, %s)", zigTypeOf(tt.Key), zigTypeOf(tt.Value))
	case types.VoidType:
		return "void"
	default:
		return "i32"
	}
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
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

func isInt64(t types.Type) bool  { _, ok := t.(types.Int64Type); return ok }
func isInt(t types.Type) bool    { _, ok := t.(types.IntType); return ok }
func isFloat(t types.Type) bool  { _, ok := t.(types.FloatType); return ok }
func isBool(t types.Type) bool   { _, ok := t.(types.BoolType); return ok }
func isString(t types.Type) bool { _, ok := t.(types.StringType); return ok }
func isList(t types.Type) bool   { _, ok := t.(types.ListType); return ok }
func isMap(t types.Type) bool    { _, ok := t.(types.MapType); return ok }
