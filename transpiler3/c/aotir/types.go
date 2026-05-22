package aotir

// Type is the monomorphic type of an aotir Value or function
// signature element. The set grows by phase. Phase 1 ships only
// TypeUnit (no value) and TypeString (read-only NUL-terminated
// C string).
type Type int

const (
	TypeInvalid Type = iota
	TypeUnit
	TypeString
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
	default:
		return "invalid"
	}
}
