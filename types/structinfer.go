package types

import (
	"fmt"
	"mochi/parser"
)

// InferStructFromList infers a struct type from a list literal using env for expression types.
// The returned StructType has no Name set.
func InferStructFromList(ll *parser.ListLiteral, env *Env) (StructType, bool) {
	if ll == nil || len(ll.Elems) == 0 {
		return StructType{}, false
	}
	first := ll.Elems[0]
	if first.Binary == nil || len(first.Binary.Right) != 0 {
		return StructType{}, false
	}
	fm := first.Binary.Left.Value.Target.Map
	if fm == nil {
		return StructType{}, false
	}
	fields := map[string]Type{}
	order := make([]string, len(fm.Items))
	for i, it := range fm.Items {
		key, ok := SimpleStringKey(it.Key)
		if !ok {
			return StructType{}, false
		}
		order[i] = key
		fields[key] = ExprType(it.Value, env)
	}
	for _, el := range ll.Elems[1:] {
		if el.Binary == nil || len(el.Binary.Right) != 0 {
			return StructType{}, false
		}
		ml := el.Binary.Left.Value.Target.Map
		if ml == nil || len(ml.Items) != len(order) {
			return StructType{}, false
		}
		for i, it := range ml.Items {
			key, ok := SimpleStringKey(it.Key)
			if !ok || key != order[i] {
				return StructType{}, false
			}
			t := ExprType(it.Value, env)
			if !EqualTypes(fields[key], t) {
				return StructType{}, false
			}
		}
	}
	st := StructType{Fields: fields, Order: order}
	return st, true
}

// InferStructFromMap infers a struct type from a map literal using env for expression types.
// The returned StructType has no Name set.
func InferStructFromMap(ml *parser.MapLiteral, env *Env) (StructType, bool) {
	return InferStructFromMapEnv(ml, env)
}

// InferStructFromMapEnv is like InferStructFromMap but allows a custom env.
func InferStructFromMapEnv(ml *parser.MapLiteral, env *Env) (StructType, bool) {
	if ml == nil || len(ml.Items) == 0 {
		return StructType{}, false
	}
	fields := map[string]Type{}
	order := make([]string, len(ml.Items))
	for i, it := range ml.Items {
		key, ok := SimpleStringKey(it.Key)
		if !ok {
			return StructType{}, false
		}
		order[i] = key
		fields[key] = ExprType(it.Value, env)
	}
	st := StructType{Fields: fields, Order: order}
	return st, true
}

// InferSimpleMap attempts to infer a typed map from a map literal with homogeneous
// string keys and consistent value types. It returns false if the literal is empty
// or contains mixed key/value types.
func InferSimpleMap(ml *parser.MapLiteral, env *Env) (MapType, bool) {
	if ml == nil || len(ml.Items) == 0 {
		return MapType{}, false
	}
	var valType Type
	for i, it := range ml.Items {
		if _, ok := SimpleStringKey(it.Key); !ok {
			return MapType{}, false
		}
		t := ExprType(it.Value, env)
		if i == 0 {
			valType = t
		} else if !EqualTypes(valType, t) {
			return MapType{}, false
		}
	}
	return MapType{Key: StringType{}, Value: valType}, true
}

// UniqueStructName generates a struct name based on base that does not collide with
// existing names in env or reserved. If base is empty, "AnonStruct" is used.
func UniqueStructName(base string, env *Env, reserved map[string]bool) string {
	if base == "" {
		base = "AnonStruct"
	}
	name := base
	idx := 1
	for {
		if reserved != nil && reserved[name] {
			name = fmt.Sprintf("%s%d", base, idx)
			idx++
			continue
		}
		if env != nil {
			if _, ok := env.GetStruct(name); ok {
				name = fmt.Sprintf("%s%d", base, idx)
				idx++
				continue
			}
		}
		break
	}
	return name
}
