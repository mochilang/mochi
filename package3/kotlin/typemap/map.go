// Package typemap translates Kotlin types to Mochi types.
package typemap

import (
	"strings"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
)

// MochiType represents a Mochi type produced by the translation.
type MochiType struct {
	Name     string // e.g. "int", "string", "List", "Option", "Map"
	TypeArgs []MochiType
	IsVoid   bool
	IsExtern bool // extern type (opaque handle)
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

// MochiParam is a single function parameter in a Mochi shim.
type MochiParam struct {
	Name string
	Type MochiType
}

// MochiFunction describes a shim function in Mochi.
type MochiFunction struct {
	Name       string     // Mochi shim function name (snake_case)
	Params     []MochiParam
	ReturnType MochiType  // IsVoid=true for Unit/Nothing
	IsSuspend  bool
	OrigName   string     // original Kotlin function name
	JVMName    string     // JVM method name (may differ from OrigName)
	Receiver   *MochiType // non-nil for extension functions
}

// MochiClass describes a Kotlin class translated for the Mochi bridge.
type MochiClass struct {
	ExternTypeName string // CamelCase name for extern type
	Kind           metadata.ClassKind
	Functions      []MochiFunction
	Properties     []MochiFunction // exposed as getter extern fns
	Constructors   []MochiFunction
	EnumVariants   []string
	SealedVariants []string
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
	"kotlin.Unit":    "", // void
	"kotlin.Nothing": "", // void
	"kotlin.Any":     "any",
	// Java string
	"java.lang.String": "string",
}

// primitiveArrayTable maps Kotlin primitive array types to their Mochi element type.
var primitiveArrayTable = map[string]string{
	"kotlin.IntArray":     "int",
	"kotlin.LongArray":    "long",
	"kotlin.ShortArray":   "int",
	"kotlin.FloatArray":   "float",
	"kotlin.DoubleArray":  "double",
	"kotlin.BooleanArray": "bool",
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
	// Additional collection/iterable types
	"kotlin.collections.Collection":        "List",
	"kotlin.collections.MutableCollection": "List",
	"kotlin.collections.Iterable":          "List",
	"kotlin.collections.MutableIterable":   "List",
	"kotlin.sequences.Sequence":            "List",
	// Java collection types
	"java.util.List":       "List",
	"java.util.ArrayList":  "List",
	"java.util.Map":        "Map",
	"java.util.HashMap":    "Map",
	"java.util.LinkedHashMap": "Map",
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
	if strings.HasPrefix(kt.ClassName, "kotlin.jvm.functions.") {
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

	// Scalar types (including java.lang.String)
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

	// kotlin.ByteArray -> bytes
	if kt.ClassName == "kotlin.ByteArray" {
		mt := MochiType{Name: "bytes"}
		if kt.Nullable {
			return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
		}
		return mt, NotRefused
	}

	// Primitive array types -> List<elementType>
	if elemName, ok := primitiveArrayTable[kt.ClassName]; ok {
		mt := MochiType{Name: "List", TypeArgs: []MochiType{{Name: elemName}}}
		if kt.Nullable {
			return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
		}
		return mt, NotRefused
	}

	// Helper: translate inner (strips nullable)
	translateInner := func(inner metadata.KotlinType) (MochiType, RefusalReason) {
		inner.Nullable = false
		return Translate(inner)
	}

	// kotlin.Result<T> -> KotlinResult (extern)
	if kt.ClassName == "kotlin.Result" {
		mt := MochiType{Name: "KotlinResult", IsExtern: true}
		if kt.Nullable {
			return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
		}
		return mt, NotRefused
	}

	// kotlin.Triple<A,B,C>
	if kt.ClassName == "kotlin.Triple" && len(kt.TypeArgs) == 3 {
		a, ra := translateInner(kt.TypeArgs[0])
		if ra != NotRefused {
			return MochiType{}, ra
		}
		b, rb := translateInner(kt.TypeArgs[1])
		if rb != NotRefused {
			return MochiType{}, rb
		}
		c, rc := translateInner(kt.TypeArgs[2])
		if rc != NotRefused {
			return MochiType{}, rc
		}
		mt := MochiType{Name: "Triple", TypeArgs: []MochiType{a, b, c}}
		if kt.Nullable {
			return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
		}
		return mt, NotRefused
	}

	// kotlin.Pair<A,B>
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

	// Collection types (List, Map, Set, Sequence, Iterable, java.util.List, java.util.Map, ...)
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

	// Default: opaque extern type
	// Extract the simple class name for the extern type name.
	className := kt.ClassName
	for j := len(className) - 1; j >= 0; j-- {
		if className[j] == '.' {
			className = className[j+1:]
			break
		}
	}
	mt := MochiType{Name: className, IsExtern: true}
	if kt.Nullable {
		return MochiType{Name: "Option", TypeArgs: []MochiType{mt}}, NotRefused
	}
	return mt, NotRefused
}

// TranslateFunction translates a metadata.Function to a MochiFunction.
// Returns RefusalUnresolvedTypeParam if any param or return type cannot be bridged.
func TranslateFunction(fn metadata.Function) (MochiFunction, RefusalReason) {
	mf := MochiFunction{
		OrigName:  fn.Name,
		JVMName:   fn.JVMName,
		IsSuspend: fn.Flags.IsSuspend,
	}

	// Translate return type
	ret, reason := Translate(fn.ReturnType)
	if reason != NotRefused {
		return MochiFunction{}, reason
	}
	mf.ReturnType = ret

	// Translate receiver (extension function)
	if fn.Receiver != nil {
		recv, reason := Translate(*fn.Receiver)
		if reason != NotRefused {
			return MochiFunction{}, reason
		}
		mf.Receiver = &recv
	}

	// Translate params
	for _, p := range fn.Params {
		pt, reason := Translate(p.Type)
		if reason != NotRefused {
			return MochiFunction{}, reason
		}
		mf.Params = append(mf.Params, MochiParam{
			Name: p.Name,
			Type: pt,
		})
	}

	mf.Name = KotlinToMochiName(fn.Name)
	return mf, NotRefused
}

// TranslateClass translates a metadata.APIObject into a MochiClass.
// Functions or properties that cannot be bridged are silently skipped.
func TranslateClass(obj *metadata.APIObject, nameRegistry *NameRegistry) (*MochiClass, RefusalReason) {
	mc := &MochiClass{
		ExternTypeName: ClassToExternName(obj.ClassName),
		Kind:           obj.Kind,
	}

	// Value class unwrapping: if exactly one property, use the underlying type.
	if obj.Kind == metadata.ClassKindValueClass && len(obj.Properties) == 1 {
		// The class itself is represented as its underlying property type; no extern needed.
		// Still register the class, but mark Kind.
	}

	// Enum variants
	mc.EnumVariants = append(mc.EnumVariants, obj.EnumEntries...)

	// Sealed variants
	for _, sub := range obj.SealedSubs {
		mc.SealedVariants = append(mc.SealedVariants, ClassToExternName(sub.ClassName))
	}

	// Functions
	for _, fn := range obj.Functions {
		mf, reason := TranslateFunction(fn)
		if reason != NotRefused {
			continue // skip unbridgeable functions
		}
		if nameRegistry != nil {
			mf.Name = nameRegistry.Allocate(mc.ExternTypeName, mf.Name)
		}
		mc.Functions = append(mc.Functions, mf)
	}

	// Properties (as getter fns)
	for _, prop := range obj.Properties {
		pt, reason := Translate(prop.Type)
		if reason != NotRefused {
			continue
		}
		getterName := "get_" + KotlinToMochiName(prop.Name)
		if nameRegistry != nil {
			getterName = nameRegistry.Allocate(mc.ExternTypeName, getterName)
		}
		mf := MochiFunction{
			Name:       getterName,
			OrigName:   prop.Name,
			JVMName:    prop.Name,
			ReturnType: pt,
		}
		mc.Properties = append(mc.Properties, mf)
	}

	// Constructors
	for i, ctor := range obj.Constructors {
		mf := MochiFunction{
			OrigName: "constructor",
			JVMName:  "<init>",
			ReturnType: MochiType{Name: mc.ExternTypeName, IsExtern: true},
		}
		ok := true
		for _, p := range ctor.Params {
			pt, reason := Translate(p.Type)
			if reason != NotRefused {
				ok = false
				break
			}
			mf.Params = append(mf.Params, MochiParam{Name: p.Name, Type: pt})
		}
		if !ok {
			continue
		}
		ctorFnName := KotlinToMochiName(mc.ExternTypeName) + "_new"
		if i > 0 {
			ctorFnName = KotlinToMochiName(mc.ExternTypeName) + "_new_" + strings.Repeat("_", i)
		}
		if nameRegistry != nil {
			ctorFnName = nameRegistry.Allocate(mc.ExternTypeName, ctorFnName)
		}
		mf.Name = ctorFnName
		mc.Constructors = append(mc.Constructors, mf)
	}

	return mc, NotRefused
}
