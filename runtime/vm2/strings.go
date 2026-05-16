package vm2

// vmString is the heap representation of a string. Strings are
// immutable and held in the Objects table; a string-valued Cell is a
// tagPtr whose payload is the Objects index of a *vmString.
//
// The header carries a memoized FNV-1a hash. Map keys (subsystem §4)
// will read this; until then the field is just paying for itself on
// OpEqualStr fast-paths where a hash mismatch short-circuits the byte
// compare.
type vmString struct {
	bytes []byte
	hash  uint64 // 0 means "not computed yet"; 0 collisions recompute
}

// newString allocates a *vmString and registers it in the VM's
// Objects table. The returned Cell carries the table index.
func (vm *VM) newString(b []byte) Cell {
	return CPtr(vm.AddObject(&vmString{bytes: b}))
}

// stringAt fetches the *vmString backing the cell. Caller must have
// already checked the cell is a string (the typed opcodes guarantee
// this statically; this helper is for the verifier and tests).
func (vm *VM) stringAt(c Cell) *vmString {
	return vm.Objects[c.Ptr()].(*vmString)
}

// strHash returns the FNV-1a hash of s.bytes, memoizing it on s. A
// computed hash of 0 is mapped to 1 so the sentinel "not computed
// yet" stays unambiguous; 1 collisions are accepted.
func strHash(s *vmString) uint64 {
	if s.hash != 0 {
		return s.hash
	}
	const (
		offset64 uint64 = 14695981039346656037
		prime64  uint64 = 1099511628211
	)
	h := offset64
	for _, b := range s.bytes {
		h ^= uint64(b)
		h *= prime64
	}
	if h == 0 {
		h = 1
	}
	s.hash = h
	return h
}

// strEqual compares two *vmStrings for byte equality. Identity match
// and length mismatch are checked first; the memoized hash is used to
// short-circuit byte compare when both sides have one cached.
func strEqual(a, b *vmString) bool {
	if a == b {
		return true
	}
	if len(a.bytes) != len(b.bytes) {
		return false
	}
	if a.hash != 0 && b.hash != 0 && a.hash != b.hash {
		return false
	}
	for i := range a.bytes {
		if a.bytes[i] != b.bytes[i] {
			return false
		}
	}
	return true
}
