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
//
// Prefer makeStr for runtime construction: it returns an inline tagSStr
// Cell when len(b) <= MaxInlineStr, avoiding allocation entirely.
func (vm *VM) newString(b []byte) Cell {
	return CPtr(vm.AddObject(&vmString{bytes: b}))
}

// makeStr returns a string Cell for b. Length <= MaxInlineStr is
// packed inline (no allocation); longer strings allocate a *vmString.
func (vm *VM) makeStr(b []byte) Cell {
	if len(b) <= MaxInlineStr {
		return CSStr(b)
	}
	return vm.newString(b)
}

// stringAt fetches the *vmString backing the cell. Caller must have
// already checked the cell is a heap (tagPtr) string; inline strings
// have no *vmString.
func (vm *VM) stringAt(c Cell) *vmString {
	return vm.Objects[c.Ptr()].(*vmString)
}

// strLen returns the byte length of a string Cell (inline or heap).
func (vm *VM) strLen(c Cell) int {
	if c.IsSStr() {
		return c.SStrLen()
	}
	return len(vm.stringAt(c).bytes)
}

// strBytes returns a []byte view of the string. For inline strings the
// bytes are copied into buf; the returned slice aliases buf. For heap
// strings the underlying bytes slice is returned directly. Callers
// must not mutate the returned slice.
func (vm *VM) strBytes(c Cell, buf *[MaxInlineStr]byte) []byte {
	if c.IsSStr() {
		return c.SStrBytes(buf)
	}
	return vm.stringAt(c).bytes
}

// strEqualCell compares two string Cells (inline or heap) for byte
// equality. Inline/inline takes the fast Cell == Cell path; mixed and
// heap/heap fall back to byte compare via strEqual.
func (vm *VM) strEqualCell(a, b Cell) bool {
	if a == b {
		return true
	}
	if a.IsSStr() && b.IsSStr() {
		return false // distinct inline Cells encode distinct bytes
	}
	var abuf, bbuf [MaxInlineStr]byte
	ab := vm.strBytes(a, &abuf)
	bb := vm.strBytes(b, &bbuf)
	if len(ab) != len(bb) {
		return false
	}
	for i := range ab {
		if ab[i] != bb[i] {
			return false
		}
	}
	return true
}

// strHashCell returns the FNV-1a hash of c's bytes. For heap strings
// the result is memoized on the underlying *vmString.
func (vm *VM) strHashCell(c Cell) uint64 {
	if c.IsSStr() {
		var buf [MaxInlineStr]byte
		return hashBytes(c.SStrBytes(&buf))
	}
	return strHash(vm.stringAt(c))
}

func hashBytes(b []byte) uint64 {
	const (
		offset64 uint64 = 14695981039346656037
		prime64  uint64 = 1099511628211
	)
	h := offset64
	for _, x := range b {
		h ^= uint64(x)
		h *= prime64
	}
	if h == 0 {
		h = 1
	}
	return h
}

// strHash returns the FNV-1a hash of s.bytes, memoizing it on s. A
// computed hash of 0 is mapped to 1 so the sentinel "not computed
// yet" stays unambiguous; 1 collisions are accepted.
func strHash(s *vmString) uint64 {
	if s.hash != 0 {
		return s.hash
	}
	h := hashBytes(s.bytes)
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
