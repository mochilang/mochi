package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerType maps a Mochi aotir.Type to a Java TypeRef.
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
	default:
		return javasrc.TypeRef{}, fmt.Errorf("jvm/lower: unsupported type %v", t)
	}
}
