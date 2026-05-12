package types

// IsAnyType reports whether t is the AnyType.
func IsAnyType(t Type) bool { _, ok := t.(AnyType); return ok }
