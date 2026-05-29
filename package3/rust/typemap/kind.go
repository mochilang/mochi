// Package typemap is the closed type-mapping table for MEP-73. It
// consumes rustdoc.Type values (the output of phase 2's walker) and
// either returns a structured Mapping describing the Mochi-side type
// and FFI representation, or an errors.SkipReason explaining why no
// mapping exists.
//
// The table is "closed": every rustdoc-types Type variant either has
// a documented mapping rule or a documented refusal class. There is
// no fallthrough that silently produces approximated types. See
// research note 05 ("Type mapping") for the design rationale.
package typemap

// Kind is the high-level Mochi type a Rust type lowers to. Each Kind
// corresponds to a section of the closed table.
type Kind int

const (
	// KindInvalid is the zero value; a Mapping with KindInvalid is a
	// programming error and must never escape the package.
	KindInvalid Kind = iota
	KindUnit
	KindBool
	KindInt
	KindInt64
	KindUInt
	KindUInt64
	KindFloat
	KindFloat64
	KindByte
	KindChar
	KindString
	KindBytes
	KindList
	KindMap
	KindOption
	KindResult
	KindTuple
	KindStruct
	KindEnum
	KindHandle
)

// String renders a Kind as the lower-case Mochi token used by golden
// fixtures and SKIPPED diagnostics.
func (k Kind) String() string {
	switch k {
	case KindUnit:
		return "unit"
	case KindBool:
		return "bool"
	case KindInt:
		return "int"
	case KindInt64:
		return "int64"
	case KindUInt:
		return "uint"
	case KindUInt64:
		return "uint64"
	case KindFloat:
		return "float"
	case KindFloat64:
		return "float64"
	case KindByte:
		return "byte"
	case KindChar:
		return "char"
	case KindString:
		return "string"
	case KindBytes:
		return "bytes"
	case KindList:
		return "list"
	case KindMap:
		return "map"
	case KindOption:
		return "option"
	case KindResult:
		return "result"
	case KindTuple:
		return "tuple"
	case KindStruct:
		return "struct"
	case KindEnum:
		return "enum"
	case KindHandle:
		return "handle"
	default:
		return "invalid"
	}
}

// Direction is the position of a type in a function signature. Some
// rules differ between input and output positions, principally for
// borrowed references and unit returns.
type Direction int

const (
	DirectionIn Direction = iota
	DirectionOut
)

// String renders a Direction as the lower-case token used by tests
// and skip diagnostics.
func (d Direction) String() string {
	if d == DirectionOut {
		return "out"
	}
	return "in"
}
