package ccode

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

func isNumber(t types.Type) bool {
	return isInt(t) || isFloat(t)
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
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.ListType:
		elem := cTypeFromType(tt.Elem)
		if elem == "int" {
			return "list_int"
		}
		if elem == "double" {
			return "list_float"
		}
		if elem == "char*" {
			return "list_string"
		}
		if elem == "list_int" {
			return "list_list_int"
		}
	case types.MapType:
		if _, ok := tt.Key.(types.IntType); ok {
			if _, ok2 := tt.Value.(types.BoolType); ok2 {
				return "map_int_bool"
			}
		}
	case types.FuncType:
		ret := cTypeFromType(tt.Return)
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = cTypeFromType(p)
		}
		return fmt.Sprintf("%s (*)(%s)", ret, strings.Join(params, ", "))
	}
	return "int"
}

func isListListIntType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if inner, ok2 := lt.Elem.(types.ListType); ok2 {
			switch inner.Elem.(type) {
			case types.IntType, types.BoolType:
				return true
			}
		}
	}
	return false
}

func isListStringType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok2 := lt.Elem.(types.StringType); ok2 {
			return true
		}
	}
	return false
}

func isListFloatType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok2 := lt.Elem.(types.FloatType); ok2 {
			return true
		}
	}
	return false
}

func isMapIntBoolType(t types.Type) bool {
	if mt, ok := t.(types.MapType); ok {
		if _, ok := mt.Key.(types.IntType); ok {
			if _, ok2 := mt.Value.(types.BoolType); ok2 {
				return true
			}
		}
	}
	return false
}

func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.IntType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if env != nil {
				if st, ok := env.GetStruct(*t.Simple); ok {
					return st
				}
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: resolveTypeRef(t.Generic.Args[0], env), Value: resolveTypeRef(t.Generic.Args[1], env)}
		}
	}
	return types.AnyType{}
}
