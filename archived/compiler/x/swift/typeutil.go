//go:build archived

package swiftcode

import (
	"mochi/parser"
	"mochi/types"
	"strings"
)

// swiftType converts a Mochi static type to the corresponding Swift type.
func swiftType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Bool"
	case types.StringType:
		return "String"
	case types.ListType:
		return "[" + swiftType(tt.Elem) + "]"
	case types.MapType:
		key := swiftType(tt.Key)
		if key == "Any" {
			key = "AnyHashable"
		}
		return "[" + key + ": " + swiftType(tt.Value) + "]"
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = swiftType(p)
		}
		ret := swiftType(tt.Return)
		return "(" + strings.Join(params, ", ") + ") -> " + ret
	case types.VoidType:
		return "Void"
	default:
		return "Any"
	}
}

// resolveTypeRef converts a parsed type reference to a concrete static type.
func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.AnyType{}
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
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return types.MapType{Key: resolveTypeRef(t.Generic.Args[0], env), Value: resolveTypeRef(t.Generic.Args[1], env)}
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
			if env != nil {
				if st, ok := env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
		}
	}
	return types.AnyType{}
}

func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	// Handle simple list or map literals directly for better inference.
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 {
				if p.Target.List != nil {
					var elem types.Type = types.AnyType{}
					for i, el := range p.Target.List.Elems {
						t := c.inferExprType(el)
						if i == 0 {
							elem = t
						} else if t.String() != elem.String() {
							elem = types.AnyType{}
							break
						}
					}
					return types.ListType{Elem: elem}
				}
				if p.Target.Map != nil {
					var key types.Type = types.AnyType{}
					var val types.Type = types.AnyType{}
					for i, it := range p.Target.Map.Items {
						kt := c.inferExprType(it.Key)
						if _, ok := identName(it.Key); ok {
							// Treat bare identifiers used as map keys as string literals
							// even if a variable with the same name exists.
							kt = types.StringType{}
						}
						vt := c.inferExprType(it.Value)
						if i == 0 {
							key = kt
							val = vt
						} else {
							if kt.String() != key.String() {
								key = types.AnyType{}
							}
							if vt.String() != val.String() {
								val = types.AnyType{}
							}
						}
					}
					return types.MapType{Key: key, Value: val}
				}
			}
		}
	}
	env := types.NewEnv(c.env)
	for name, t := range c.locals {
		env.SetVar(name, t, true)
	}
	return types.CheckExprType(e, env)
}

func containsAny(t types.Type) bool {
	return types.ContainsAny(t)
}
