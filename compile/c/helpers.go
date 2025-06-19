package ccode

import (
	"reflect"

	"mochi/types"
)

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

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func cTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.BoolType:
		return "int"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "char*"
	case types.ListType:
		elem := cTypeFromType(tt.Elem)
		if elem == "int" {
			return "list_int"
		}
		if elem == "list_int" {
			return "list_list_int"
		}
	}
	return "int"
}
