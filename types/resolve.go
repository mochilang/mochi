package types

import "mochi/parser"

func resolveTypeRef(t *parser.TypeRef, env *Env) Type {
	typ := resolveTypeRefInner(t, env)
	if t.Optional {
		// MEP-10 C1: `T?` desugars to `option[T]`.
		typ = OptionType{Elem: typ}
	}
	return typ
}

func resolveTypeRefInner(t *parser.TypeRef, env *Env) Type {
	if t.Fun != nil {
		params := make([]Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret Type = UnitType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return FuncType{Params: params, Return: ret}
	}

	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return ListType{Elem: resolveTypeRef(args[0], env)}
			}
		case "map":
			if len(args) == 2 {
				return MapType{
					Key:   resolveTypeRef(args[0], env),
					Value: resolveTypeRef(args[1], env),
				}
			}
		}
		// Fallback: unknown generic type
		return AnyType{}
	}

	if t.Struct != nil {
		var fields []StructField
		for _, f := range t.Struct.Fields {
			fields = append(fields, StructField{Name: f.Name, Type: resolveTypeRef(f.Type, env)})
		}
		return StructType{Name: "", Fields: fields}
	}

	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return IntType{}
		case "int64":
			return Int64Type{}
		case "float":
			return FloatType{}
		case "bigint":
			return BigIntType{}
		case "bigrat":
			return BigRatType{}
		case "string":
			return StringType{}
		case "bool":
			return BoolType{}
		case "unit":
			return UnitType{}
		case "any":
			return AnyType{}
		default:
			if tv, ok := env.LookupTypeParam(*t.Simple); ok {
				return tv
			}
			if ut, ok := env.GetUnion(*t.Simple); ok {
				return ut
			}
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
			if typ, ok := env.LookupType(*t.Simple); ok {
				return typ
			}
			if ut, ok := env.FindUnionByVariant(*t.Simple); ok {
				return ut
			}
			if isTypeParamName(*t.Simple) {
				return AnyType{}
			}
			env.RecordDiagnostic(errUnknownType(t.Pos, *t.Simple))
			return AnyType{}
		}
	}

	return AnyType{}
}

// isTypeParamName recognises single uppercase letters used as generic
// type-parameter names (`T`, `K`, `V`). MEP 12 will plumb a real type-
// parameter scope through resolveTypeRef; until then this heuristic
// keeps the unknown-type-name diagnostic from firing on names that are
// already valid in generic positions.
func isTypeParamName(name string) bool {
	if len(name) != 1 {
		return false
	}
	c := name[0]
	return c >= 'A' && c <= 'Z'
}

func resolveTypeName(name string, env *Env) Type {
	return resolveTypeRef(&parser.TypeRef{Simple: &name}, env)
}

