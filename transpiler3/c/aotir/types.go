package aotir

// Type is the monomorphic type of an aotir Value or function
// signature element. The set grows by phase. Phase 1 ships only
// TypeUnit (no value) and TypeString (read-only NUL-terminated
// C string). Phase 2.0 adds the three scalar primitives.
type Type int

const (
	TypeInvalid Type = iota
	TypeUnit
	TypeString
	TypeInt   // int64_t (signed, two's complement)
	TypeFloat // double (IEEE 754 binary64)
	TypeBool  // int storing 0 or 1 (matches C runtime ABI)
)

// String returns a stable identifier for the type, used in
// emit-time mangling and verifier diagnostics. The names must
// be deterministic; sort order on this name is the emit order
// for type-keyed lookup tables.
func (t Type) String() string {
	switch t {
	case TypeUnit:
		return "unit"
	case TypeString:
		return "string"
	case TypeInt:
		return "int"
	case TypeFloat:
		return "float"
	case TypeBool:
		return "bool"
	default:
		return "invalid"
	}
}
