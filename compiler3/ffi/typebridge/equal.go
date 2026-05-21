package typebridge

// Equal reports whether two Types describe the same Go shape. Used
// by the Phase 2 cache-hit check and by round-trip tests.
//
// Equality is a deep structural comparison; KindOpaque values are
// equal iff their OpaqueReason and GoType strings match. The
// Methods slices on KindNamed / KindIface are compared after
// sorting; GoToMochi already returns them sorted by Name.
func (a Type) Equal(b Type) bool {
	if a.Kind != b.Kind {
		return false
	}
	if a.Width != b.Width {
		return false
	}
	if a.ArrayLen != b.ArrayLen {
		return false
	}
	if a.Variadic != b.Variadic {
		return false
	}
	if a.Name != b.Name {
		return false
	}
	if a.PkgPath != b.PkgPath {
		return false
	}
	if a.ChanDir != b.ChanDir {
		return false
	}
	if a.OpaqueReason != b.OpaqueReason {
		return false
	}
	if a.GoType != b.GoType {
		return false
	}
	if !equalPtrType(a.Elem, b.Elem) {
		return false
	}
	if !equalPtrType(a.Key, b.Key) {
		return false
	}
	if !equalFields(a.Fields, b.Fields) {
		return false
	}
	if !equalTypeSlice(a.Params, b.Params) {
		return false
	}
	if !equalTypeSlice(a.Results, b.Results) {
		return false
	}
	if !equalTypeSlice(a.TypeArgs, b.TypeArgs) {
		return false
	}
	if !equalMethods(a.Methods, b.Methods) {
		return false
	}
	return true
}

func equalPtrType(a, b *Type) bool {
	if a == nil || b == nil {
		return a == b
	}
	return a.Equal(*b)
}

func equalTypeSlice(a, b []Type) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if !a[i].Equal(b[i]) {
			return false
		}
	}
	return true
}

func equalFields(a, b []Field) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i].Name != b[i].Name || a[i].Tag != b[i].Tag || a[i].Embedded != b[i].Embedded || a[i].Exported != b[i].Exported {
			return false
		}
		if !a[i].Type.Equal(b[i].Type) {
			return false
		}
	}
	return true
}

func equalMethods(a, b []Method) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i].Name != b[i].Name || a[i].Exported != b[i].Exported {
			return false
		}
		if !a[i].Signature.Equal(b[i].Signature) {
			return false
		}
	}
	return true
}
