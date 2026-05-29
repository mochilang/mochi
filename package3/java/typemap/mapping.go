package typemap

// Mapping holds the result of successfully mapping a Java type to Mochi.
type Mapping struct {
	// Kind is the Mochi type category.
	Kind Kind
	// JavaName is the erased Java type name that was mapped.
	JavaName string
	// MochiType is the corresponding Mochi type expression.
	MochiType string
	// FFIRepr is the JNI-level representation used in wrapper method signatures.
	FFIRepr string
	// Elem is the element mapping for List and Optional kinds. May be nil.
	Elem *Mapping
	// Key is the key mapping for Map kinds. May be nil.
	Key *Mapping
	// Value is the value mapping for Map kinds. May be nil.
	Value *Mapping
}

// IsScalar returns true if the mapping is a JNI primitive (no boxing needed
// at the Java boundary).
func (m *Mapping) IsScalar() bool {
	switch m.Kind {
	case KindBool, KindInt, KindLong, KindFloat, KindDouble, KindVoid:
		return true
	default:
		return false
	}
}
