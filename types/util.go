package types

import "reflect"

// EqualTypes reports whether types a and b are considered equal.
// AnyType is treated as a wildcard that matches any other type.
func EqualTypes(a, b Type) bool {
	if _, ok := a.(AnyType); ok {
		_, ok2 := b.(AnyType)
		return ok2
	}
	if _, ok := b.(AnyType); ok {
		_, ok2 := a.(AnyType)
		return ok2
	}
	if la, ok := a.(ListType); ok {
		if lb, ok := b.(ListType); ok {
			return EqualTypes(la.Elem, lb.Elem)
		}
	}
	if ma, ok := a.(MapType); ok {
		if mb, ok := b.(MapType); ok {
			return EqualTypes(ma.Key, mb.Key) && EqualTypes(ma.Value, mb.Value)
		}
	}
	if oa, ok := a.(OptionType); ok {
		if ob, ok := b.(OptionType); ok {
			return EqualTypes(oa.Elem, ob.Elem)
		}
	}
	if ua, ok := a.(UnionType); ok {
		if sb, ok := b.(StructType); ok {
			if _, ok := ua.Variants[sb.Name]; ok {
				return true
			}
		}
	}
	if ub, ok := b.(UnionType); ok {
		if sa, ok := a.(StructType); ok {
			if _, ok := ub.Variants[sa.Name]; ok {
				return true
			}
		}
	}
	if IsInt64Type(a) && (IsInt64Type(b) || IsIntType(b)) {
		return true
	}
	if IsInt64Type(b) && (IsInt64Type(a) || IsIntType(a)) {
		return true
	}
	if IsIntType(a) && IsIntType(b) {
		return true
	}
	return reflect.DeepEqual(a, b)
}

// IsInt64Type reports whether t is an int64 type.
func IsInt64Type(t Type) bool { _, ok := t.(Int64Type); return ok }

// IsIntType reports whether t is an int type.
func IsIntType(t Type) bool { _, ok := t.(IntType); return ok }

// IsFloatType reports whether t is a float type.
func IsFloatType(t Type) bool {
	if _, ok := t.(FloatType); ok {
		return true
	}
	_, ok := t.(BigRatType)
	return ok
}

// IsBoolType reports whether t is a bool type.
func IsBoolType(t Type) bool { _, ok := t.(BoolType); return ok }

// IsNumericType reports whether t is any numeric type.
func IsNumericType(t Type) bool { return IsIntType(t) || IsInt64Type(t) || IsFloatType(t) }

// IsStructType reports whether t is a struct type.
func IsStructType(t Type) bool { _, ok := t.(StructType); return ok }

// IsUnionType reports whether t is a union type.
func IsUnionType(t Type) bool { _, ok := t.(UnionType); return ok }

// IsAnyType reports whether t is the special any type.
func IsAnyType(t Type) bool { _, ok := t.(AnyType); return ok }

// IsStringMap reports whether t is a map with string keys regardless of value type.
func IsStringMap(t Type) bool {
	if mt, ok := t.(MapType); ok {
		return IsStringType(mt.Key)
	}
	return false
}

// IsStringMapLike reports whether t resolves to a map with string keys.
// Union types are considered string map-like if all variants are string-keyed maps.
func IsStringMapLike(t Type) bool {
	if IsStringMap(t) {
		return true
	}
	if ut, ok := t.(UnionType); ok {
		for _, v := range ut.Variants {
			if !IsStringMap(v) {
				return false
			}
		}
		return true
	}
	return false
}

// IsStringAnyMap reports whether t is a map[string]any.
func IsStringAnyMap(t Type) bool {
	if mt, ok := t.(MapType); ok {
		if IsStringType(mt.Key) {
			if _, ok := mt.Value.(AnyType); ok {
				return true
			}
		}
	}
	return false
}

// IsStringAnyMapLike reports whether t resolves to map[string]any or a union of such maps.
func IsStringAnyMapLike(t Type) bool {
	if IsStringAnyMap(t) {
		return true
	}
	if ut, ok := t.(UnionType); ok {
		for _, v := range ut.Variants {
			if !IsStringAnyMap(v) {
				return false
			}
		}
		return true
	}
	return false
}

// ContainsAny reports whether t contains the Any type anywhere within its structure.

// StructMatches reports whether the provided field map and order slice match the layout of existing.
func StructMatches(existing StructType, fields map[string]Type, order []string) bool {
	if len(existing.Fields) != len(fields) || len(existing.Order) != len(order) {
		return false
	}
	for i, name := range existing.Order {
		if order[i] != name {
			return false
		}
		if !EqualTypes(existing.Fields[name], fields[name]) {
			return false
		}
	}
	return true
}

// StructTypesMatch reports whether two struct types have the same set of fields and ordering, ignoring their names.
func StructTypesMatch(a, b StructType) bool {
	if len(a.Fields) != len(b.Fields) || len(a.Order) != len(b.Order) {
		return false
	}
	for i, name := range a.Order {
		if b.Order[i] != name {
			return false
		}
		if !EqualTypes(a.Fields[name], b.Fields[name]) {
			return false
		}
	}
	return true
}
