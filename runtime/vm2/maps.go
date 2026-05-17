package vm2

import (
	"unsafe"
)

// vmMap is the heap representation of a map (MEP-24 §4). The MVP uses
// Go's built-in map keyed on a normalized `any` so equality follows
// Go's comparable semantics: ints/bools/strings compare by value, and
// inline + heap string Cells with the same bytes alias the same entry.
//
// A follow-on MEP gated on profile evidence can replace this with an
// open-addressed table over the mapKey struct described in MEP-24 §4.
type vmMap struct {
	entries map[any]Cell
}

func (vm *VM) newMap() Cell {
	m := &vmMap{entries: map[any]Cell{}}
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(m)}
}

// mapAt fetches the *vmMap backing a tagPtr cell. Phase 2: the typed
// pointer is the only reference path; newMap no longer populates
// vm.Objects.
func (vm *VM) mapAt(c Cell) *vmMap {
	return (*vmMap)(c.PtrTo())
}

// mapKeyOf normalizes a Cell into a Go map key. Strings collapse to
// their byte content so a fresh-but-equal string Cell hits the same
// entry; ints/bools/null collapse to their decoded scalar value; other
// pointer cells use the Cell bit pattern as identity.
func (vm *VM) mapKeyOf(c Cell) any {
	switch {
	case c.IsSStr():
		var buf [MaxInlineStr]byte
		return string(c.SStrBytes(&buf))
	case c.IsHeapStr():
		// Heap strings collapse to byte-content keys so a fresh-but-equal
		// *vmString hits the same map entry. The tagPtrStrFlag bit set
		// by newString is what discriminates this from other pointer
		// cells now that we no longer route through Objects[].
		s := (*vmString)(c.PtrTo())
		return string(s.bytes)
	case c.IsPtr():
		// Other pointer kinds (lists, maps, structs, closures) use
		// identity (the tagPtr Cell bits) as the map key, matching
		// Mochi semantics.
		return c.Bits
	case c.IsInt():
		return c.Int()
	case c.IsBool():
		return c.Bool()
	case c.IsNull():
		return nil
	}
	return c.Bits
}
