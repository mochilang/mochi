// Package typemap implements the closed PHP-to-Mochi type translation table
// for the MEP-75 PHP bridge.
//
// The table is closed: every PHP type produces either a Mochi type or a
// SkipReason. No open-ended inference is performed. The closed set matches
// the design in [website/docs/research/0075/05-type-mapping.md].
//
// Usage:
//
//	m, skip, detail := typemap.Map("string", false)
//	if skip != 0 { /* item should be SKIPPED */ }
//	// m.MochiType is the Mochi extern-type string
package typemap

import (
	"fmt"
	"strings"

	bridgeerrors "mochi/package3/php/errors"
)

// Direction indicates the translation direction for polymorphic types.
type Direction int

const (
	// DirectionIn translates a PHP parameter type into a Mochi parameter type.
	DirectionIn Direction = iota
	// DirectionOut translates a PHP return type into a Mochi return type.
	DirectionOut
)

// Mapping is the result of a successful PHP-to-Mochi type translation.
type Mapping struct {
	// PHPType is the original PHP type string as returned by ReflectionType.
	PHPType string
	// MochiType is the Mochi extern type expression, e.g. "string", "int",
	// "bool", "float", "list[T]", "map[K]V", "T|nil", or an extern-type name.
	MochiType string
	// Nullable is true when the PHP type was nullable (?T or T|null) and the
	// MochiType wraps the base type in T|nil.
	Nullable bool
	// IsPrimitive is true for scalar types (int, float, string, bool).
	IsPrimitive bool
}

// Map translates a PHP type string (as returned by ReflectionNamedType.getName()
// etc.) into a Mochi type Mapping, or returns a SkipReason when the type
// cannot be safely mapped.
//
// nullable is true when the PHP type is declared as ?T or when allowsNull()
// returns true for the ReflectionType. When nullable is true, the returned
// MochiType is wrapped as "T|nil".
//
// dir is DirectionIn for parameter types and DirectionOut for return types.
// It is currently only consulted for "void" (which is valid as DirectionOut
// but not as DirectionIn).
func Map(phpType string, nullable bool, dir Direction) (*Mapping, bridgeerrors.SkipReason, string) {
	// Normalise: strip leading backslashes, trim spaces.
	t := strings.TrimSpace(strings.TrimPrefix(phpType, "\\"))

	// Union types ("A|B|null") — decompose and map each component.
	if strings.Contains(t, "|") {
		return mapUnion(t, dir)
	}

	// Intersection types ("A&B") — always skip in v1.
	if strings.Contains(t, "&") {
		return nil, bridgeerrors.SkipIntersection, fmt.Sprintf("intersection type %q not supported in v1", t)
	}

	skip, detail := skipPrimitive(t, dir)
	if skip != 0 {
		return nil, skip, detail
	}

	mochiType := primitiveMochiType(t, dir)
	if mochiType == "" {
		// Class/interface/enum name: map to a handle type.
		mochiType = classHandleType(t)
	}

	if nullable {
		mochiType = mochiType + "|nil"
	}

	m := &Mapping{
		PHPType:     phpType,
		MochiType:   mochiType,
		Nullable:    nullable,
		IsPrimitive: isPrimitive(t),
	}
	return m, 0, ""
}

// mapUnion handles PHP union types like "int|string", "int|null", "A|B|null".
func mapUnion(t string, dir Direction) (*Mapping, bridgeerrors.SkipReason, string) {
	parts := strings.Split(t, "|")
	var nonNull []string
	hasNull := false
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "null" || p == "NULL" {
			hasNull = true
		} else {
			nonNull = append(nonNull, p)
		}
	}
	if len(nonNull) == 0 {
		return nil, bridgeerrors.SkipMixed, "union reduces to null only"
	}
	if len(nonNull) == 1 {
		return Map(nonNull[0], hasNull, dir)
	}
	var mochiParts []string
	for _, p := range nonNull {
		m, skip, detail := Map(p, false, dir)
		if skip != 0 {
			return nil, skip, fmt.Sprintf("union component %q: %s", p, detail)
		}
		mochiParts = append(mochiParts, m.MochiType)
	}
	mochiType := strings.Join(mochiParts, "|")
	if hasNull {
		mochiType = mochiType + "|nil"
	}
	return &Mapping{
		PHPType:     t,
		MochiType:   mochiType,
		Nullable:    hasNull,
		IsPrimitive: false,
	}, 0, ""
}

// skipPrimitive returns a SkipReason for PHP types that cannot be translated
// in v1. Returns 0 if the type is mappable.
func skipPrimitive(t string, _ Direction) (bridgeerrors.SkipReason, string) {
	switch strings.ToLower(t) {
	case "mixed":
		return bridgeerrors.SkipMixed, "mixed type has no Mochi equivalent"
	case "object":
		return bridgeerrors.SkipObject, "bare object type has no Mochi equivalent"
	case "callable":
		return bridgeerrors.SkipCallable, "callable pseudo-type has no stable ABI"
	case "resource":
		return bridgeerrors.SkipResource, "PHP resource handle is pre-8.0 legacy"
	case "never":
		return bridgeerrors.SkipNever, "never return type represents unreachable boundary"
	case "self", "static", "parent":
		return bridgeerrors.SkipSelfStatic, fmt.Sprintf("%q type requires runtime dispatch", t)
	case "array":
		return bridgeerrors.SkipUntypedArray, "untyped array requires docblock hint for list/map inference"
	}
	return 0, ""
}

// primitiveMochiType maps a PHP scalar or well-known type to a Mochi type string.
// Returns the empty string for class/interface/enum names (handled by classHandleType).
func primitiveMochiType(t string, dir Direction) string {
	switch strings.ToLower(t) {
	case "int", "integer":
		return "int"
	case "float", "double":
		return "float"
	case "string":
		return "string"
	case "bool", "boolean":
		return "bool"
	case "void":
		if dir == DirectionOut {
			return "unit"
		}
		return ""
	case "null":
		return "nil"
	case "true", "false":
		return "bool"
	case "iterable":
		return "list[any]"
	}
	return ""
}

// classHandleType maps a PHP class/interface/enum FQCN to its Mochi extern
// handle name. Namespace parts are joined in PascalCase.
//
// Example: "GuzzleHttp\\Client" -> "GuzzleHttpClient".
func classHandleType(fqcn string) string {
	fqcn = strings.TrimPrefix(fqcn, "\\")
	parts := strings.Split(fqcn, "\\")
	var sb strings.Builder
	for _, p := range parts {
		if p == "" {
			continue
		}
		sb.WriteString(strings.ToUpper(p[:1]))
		sb.WriteString(p[1:])
	}
	name := sb.String()
	if name == "" {
		return "OpaqueHandle"
	}
	return name
}

// isPrimitive returns true for PHP scalar types (int, float, string, bool).
func isPrimitive(t string) bool {
	switch strings.ToLower(t) {
	case "int", "integer", "float", "double", "string", "bool", "boolean":
		return true
	}
	return false
}

// MapTypedArray maps a PHP array type with explicit key and value types (from
// a PSR-compliant docblock like "array<string, int>") to a Mochi map or list.
// mapKeyType is "" for lists (array<T>), non-empty for maps (array<K, V>).
func MapTypedArray(mapKeyType, valueType string) (*Mapping, bridgeerrors.SkipReason, string) {
	if valueType == "" {
		return nil, bridgeerrors.SkipUntypedArray, "empty value type for typed array"
	}
	valueM, skip, detail := Map(valueType, false, DirectionIn)
	if skip != 0 {
		return nil, skip, fmt.Sprintf("array value type: %s", detail)
	}
	if mapKeyType == "" {
		return &Mapping{
			PHPType:   fmt.Sprintf("array<%s>", valueType),
			MochiType: fmt.Sprintf("list[%s]", valueM.MochiType),
		}, 0, ""
	}
	keyM, skip, detail := Map(mapKeyType, false, DirectionIn)
	if skip != 0 {
		return nil, skip, fmt.Sprintf("array key type: %s", detail)
	}
	return &Mapping{
		PHPType:   fmt.Sprintf("array<%s, %s>", mapKeyType, valueType),
		MochiType: fmt.Sprintf("map[%s]%s", keyM.MochiType, valueM.MochiType),
	}, 0, ""
}
