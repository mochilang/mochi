package aotir

// Type is the monomorphic type of an aotir Value or function
// signature element. The set grows by phase. Phase 1 ships only
// TypeUnit (no value) and TypeString (read-only NUL-terminated
// C string). Phase 2.0 adds the three scalar primitives. Phase
// 3.0 adds TypeRecord; the record's identity (its source name)
// rides on a parallel RecordName field on the carrying IR node
// rather than inflating Type into a struct (avoids touching
// every Type compare site in the verifier / emit / lower).
type Type int

const (
	TypeInvalid Type = iota
	TypeUnit
	TypeString
	TypeInt    // int64_t (signed, two's complement)
	TypeFloat  // double (IEEE 754 binary64)
	TypeBool   // int storing 0 or 1 (matches C runtime ABI)
	TypeRecord // struct mochi_<Name>; identity carried as RecordName beside the Type
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
	case TypeRecord:
		return "record"
	default:
		return "invalid"
	}
}
