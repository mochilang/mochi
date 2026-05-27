package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerType maps a Mochi aotir.Type to a Java TypeRef (raw, unparameterised).
// For generic collection types (list, map, set), use lowerListType /
// lowerMapType / lowerSetType which carry the element type arguments.
func lowerType(t aotir.Type) (javasrc.TypeRef, error) {
	switch t {
	case aotir.TypeInt:
		return javasrc.TypeLong, nil
	case aotir.TypeFloat:
		return javasrc.TypeDouble, nil
	case aotir.TypeBool:
		return javasrc.TypeBoolean, nil
	case aotir.TypeString:
		return javasrc.TypeString, nil
	case aotir.TypeUnit:
		return javasrc.TypeVoid, nil
	// Raw (erased) collection references -- caller must use the typed variants for declarations.
	case aotir.TypeList:
		return javasrc.TypeRef{Name: "java.util.List"}, nil
	case aotir.TypeMap:
		return javasrc.TypeRef{Name: "java.util.LinkedHashMap"}, nil
	case aotir.TypeSet:
		return javasrc.TypeRef{Name: "java.util.LinkedHashSet"}, nil
	default:
		return javasrc.TypeRef{}, fmt.Errorf("jvm/lower: unsupported type %v", t)
	}
}

// boxedType returns the boxed Java type for a Mochi scalar -- needed because
// Java generics cannot use primitive types.
func boxedType(t aotir.Type) javasrc.TypeRef {
	switch t {
	case aotir.TypeInt:
		return javasrc.TypeRef{Name: "Long"}
	case aotir.TypeFloat:
		return javasrc.TypeRef{Name: "Double"}
	case aotir.TypeBool:
		return javasrc.TypeRef{Name: "Boolean"}
	case aotir.TypeString:
		return javasrc.TypeRef{Name: "String"}
	default:
		return javasrc.TypeRef{Name: "Object"}
	}
}

// lowerListType returns java.util.List<BoxedElem>.
func lowerListType(elemType aotir.Type) javasrc.TypeRef {
	return javasrc.TypeRef{
		Name:     "java.util.List",
		TypeArgs: []javasrc.TypeRef{boxedType(elemType)},
	}
}

// lowerMutableListType returns java.util.ArrayList<BoxedElem>.
func lowerMutableListType(elemType aotir.Type) javasrc.TypeRef {
	return javasrc.TypeRef{
		Name:     "java.util.ArrayList",
		TypeArgs: []javasrc.TypeRef{boxedType(elemType)},
	}
}

// lowerMapType returns java.util.LinkedHashMap<BoxedKey, BoxedVal>.
func lowerMapType(keyType, valueType aotir.Type) javasrc.TypeRef {
	return javasrc.TypeRef{
		Name:     "java.util.LinkedHashMap",
		TypeArgs: []javasrc.TypeRef{boxedType(keyType), boxedType(valueType)},
	}
}

// lowerSetType returns java.util.LinkedHashSet<BoxedElem>.
func lowerSetType(elemType aotir.Type) javasrc.TypeRef {
	return javasrc.TypeRef{
		Name:     "java.util.LinkedHashSet",
		TypeArgs: []javasrc.TypeRef{boxedType(elemType)},
	}
}
