// Package typemap translates Kotlin types to Mochi types.
package typemap

import (
	"strings"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
)

// MochiType represents a Mochi type produced by the translation.
type MochiType struct {
	Name     string       // e.g. "int", "string", "List", "Option", "Map"
	TypeArgs []MochiType
	IsVoid   bool
	IsExtern bool         // extern type (opaque handle)
}

func (m MochiType) String() string {
	if m.IsVoid {
		return "(void)"
	}
	if len(m.TypeArgs) == 0 {
		return m.Name
	}
	var b strings.Builder
	b.WriteString(m.Name)
	b.WriteByte('<')
	for i, a := range m.TypeArgs {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(a.String())
	}
	b.WriteByte('>')
	return b.String()
}

// RefusalReason explains why a Kotlin type cannot be bridged.
type RefusalReason int

const (
	NotRefused RefusalReason = iota
	RefusalUnresolvedTypeParam
	RefusalInlineReifiedNoMonomorphise
	RefusalDynamicType
	RefusalThrowableReturn
	RefusalRawContinuation
	RefusalRawLambda
	RefusalKClassReflection
	RefusalUnsignedIntJVM17
	RefusalJavaNonPrimitiveArray
)

func (r RefusalReason) String() string {
	switch r {
	case NotRefused:
		return ""
	case RefusalUnresolvedTypeParam:
		return "unresolved type parameter (add to monomorphise)"
	case RefusalInlineReifiedNoMonomorphise:
		return "inline reified function (add instantiation to monomorphise)"
	case RefusalDynamicType:
		return "dynamic type (Kotlin/JS only)"
	case RefusalThrowableReturn:
		return "Throwable return type (use sealed Result instead)"
	case RefusalRawContinuation:
		return "raw Continuation type (use suspend bridge)"
	case RefusalRawLambda:
		return "raw FunctionN lambda type"
	case RefusalKClassReflection:
		return "KClass/KFunction reflection type"
	case RefusalUnsignedIntJVM17:
		return "unsigned integer type (UInt/ULong) on JVM < 21"
	case RefusalJavaNonPrimitiveArray:
		return "non-primitive Java array type"
	}
	return "unknown refusal"
}

// scalarTable maps fully qualified Kotlin class names to Mochi scalar types.
var scalarTable = map[string]string{
	"kotlin.Int":     "int",
	"kotlin.Long":    "long",
	"kotlin.Short":   "int",
	"kotlin.Byte":    "int",
	"kotlin.Double":  "double",
	"kotlin.Float":   "float",
	"kotlin.Boolean": "bool",
	"kotlin.Char":    "int",
	"kotlin.String":  "string",
	"kotlin.Unit":    "",    // void
	"kotlin.Nothing": "",    // void
	"kotlin.Any":     "any",
}

// collectionTable maps fully qualified Kotlin collection class names to Mochi types.
var collectionTable = map[string]string{
	"kotlin.collections.List":        "List",
	"kotlin.collections.MutableList": "List",
	"kotlin.collections.Set":         "Set",
	"kotlin.collections.MutableSet":  "Set",
	"kotlin.collections.Map":         "Map",
	"kotlin.collections.MutableMap":  "Map",
	"kotlin.Array":                   "List",
}

// refusalTable identifies types that cannot be bridged.
var refusalTable = map[string]RefusalReason{
	"kotlin.coroutines.Continuation": RefusalRawContinuation,
	"kotlin.reflect.KClass":          RefusalKClassReflection,
	"kotlin.reflect.KFunction":       RefusalKClassReflection,
	"kotlin.reflect.KProperty":       RefusalKClassReflection,
	"kotlin.UInt":                    RefusalUnsignedIntJVM17,
	"kotlin.ULong":                   RefusalUnsignedIntJVM17,
	"kotlin.UShort":                  RefusalUnsignedIntJVM17,
	"kotlin.UByte":                   RefusalUnsignedIntJVM17,
}

var throwablePrefixes = []string{
	"java.lang.Throwable",
	"java.lang.Exception",
	"java.lang.Error",
	"java.lang.RuntimeException",
	"kotlin.Exception",
	"kotlin.Error",
}

// IsRefused returns the refusal reason for a Kotlin type, or NotRefused.
func IsRefused(kt metadata.KotlinType) RefusalReason {
	if kt.IsTypeParam {
		return RefusalUnresolvedTypeParam
	}
	if r, ok := refusalTable[kt.ClassName]; ok {
		return r
	}
	for _, prefix := range throwablePrefixes {
		if kt.ClassName == prefix || len(kt.ClassName) > len(prefix) && kt.ClassName[:len(prefix)] == prefix {
			return RefusalThrowableReturn
		}
	}
	// FunctionN lambda types
	if len(kt.ClassName) > 9 && kt.ClassName[:21] == "kotlin.jvm.functions." {
		return RefusalRawLambda
	}
	return NotRefused
}

// Translate converts a Kotlin type to a Mochi type.
// Returns (MochiType, NotRefused) on success, or (zero, reason) if the type cannot be bridged.
func Translate(kt metadata.KotlinType) (MochiType, RefusalReason) {
	if reason := IsRefused(kt); reason != NotRefused {
		return MochiType{}, reason
	}

	// Scalar types
	if name, ok := scalarTable[kt.ClassName]; ok {
		if name == "" {
			return MochiType{IsVoid: true}, NotRefused
		}
		mt := MochiType{Name: name}
		if kt.Nullable {
			return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
		}
		return mt, NotRefused
	}

	// Nullable wrapping
	translateInner := func(inner metadata.KotlinType) (MochiType, RefusalReason) {
		inner.Nullable = false
		return Translate(inner)
	}

	// Collection types
	if mochiName, ok := collectionTable[kt.ClassName]; ok {
		mt := MochiType{Name: mochiName}
		for _, arg := range kt.TypeArgs {
			if arg.IsStarProjection {
				mt.TypeArgs = append(mt.TypeArgs, MochiType{Name: "any"})
				continue
			}
			argMochi, reason := translateInner(arg)
			if reason != NotRefused {
				return MochiType{}, reason
			}
			mt.TypeArgs = append(mt.TypeArgs, argMochi)
		}
		if kt.Nullable {
			return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
		}
		return mt, NotRefused
	}

	// Special: kotlin.Pair<A,B>
	if kt.ClassName == "kotlin.Pair" && len(kt.TypeArgs) == 2 {
		a, ra := translateInner(kt.TypeArgs[0])
		if ra != NotRefused {
			return MochiType{}, ra
		}
		b, rb := translateInner(kt.TypeArgs[1])
		if rb != NotRefused {
			return MochiType{}, rb
		}
		mt := MochiType{Name: "Pair", TypeArgs: []MochiType{a, b}}
		if kt.Nullable {
			return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
		}
		return mt, NotRefused
	}

	// Default: opaque extern type
	// Extract the simple class name for the extern type name.
	className := kt.ClassName
	if i := len(className) - 1; i >= 0 {
		for j := len(className) - 1; j >= 0; j-- {
			if className[j] == '.' {
				className = className[j+1:]
				break
			}
		}
	}
	mt := MochiType{Name: className, IsExtern: true}
	if kt.Nullable {
		return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
	}
	return mt, NotRefused
}
