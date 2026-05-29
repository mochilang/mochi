// Package typemap - MochiKind enum and Mapping struct (phase 5 of MEP-76).
package typemap

// MochiKind enumerates every Mochi type shape that an RBS type can map to.
// It is analogous to the kind enum described in the MEP-73 typemap design.
type MochiKind int

const (
	// KindInvalid is the zero value; it must never appear in a successful Mapping.
	KindInvalid MochiKind = iota
	// KindUnit corresponds to the Mochi `unit` type (RBS void).
	KindUnit
	// KindBool corresponds to the Mochi `bool` type.
	KindBool
	// KindInt corresponds to the Mochi `int` type.
	KindInt
	// KindFloat corresponds to the Mochi `float` type.
	KindFloat
	// KindString corresponds to the Mochi `string` type (also used for Symbol).
	KindString
	// KindNil corresponds to the Mochi `nil` type.
	KindNil
	// KindList corresponds to a Mochi `list<T>` type.
	KindList
	// KindMap corresponds to a Mochi `map<K, V>` type.
	KindMap
	// KindOptional corresponds to a Mochi `T?` type.
	KindOptional
	// KindTuple corresponds to a Mochi `tuple<A, B, ...>` type.
	KindTuple
	// KindProc corresponds to a Mochi `fun(A, ...): R` type.
	KindProc
	// KindHandle corresponds to a Mochi `handle<Name>` opaque type for named
	// Ruby classes that cannot be structurally mapped.
	KindHandle
)

// Mapping is the result of translating one RBS Type into its Mochi equivalent.
// It carries both the MochiKind (for downstream code that needs to branch on
// shape) and the rendered MochiType string (for extern fn generation).
type Mapping struct {
	// Kind classifies the shape of the translated type.
	Kind MochiKind
	// MochiType is the fully rendered Mochi type string, e.g. "list<string>".
	MochiType string
	// Elem holds the element mapping for KindList and KindOptional.
	Elem *Mapping
	// Key holds the key mapping for KindMap.
	Key *Mapping
	// Value holds the value mapping for KindMap.
	Value *Mapping
	// Members holds the per-element mappings for KindTuple and KindProc
	// (params in order for KindProc, plus the return type as the last element).
	Members []*Mapping
}
