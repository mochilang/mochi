package vm2

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
	return CPtr(vm.AddObject(&vmMap{entries: map[any]Cell{}}))
}

func (vm *VM) mapAt(c Cell) *vmMap {
	return vm.Objects[c.Ptr()].(*vmMap)
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
	case c.IsPtr():
		// Only *vmString gets byte-equal keys; other pointer kinds use
		// identity (the tagPtr Cell). Maps over lists/structs/closures
		// therefore key on object identity, matching Mochi semantics.
		if s, ok := vm.Objects[c.Ptr()].(*vmString); ok {
			return string(s.bytes)
		}
		return uint64(c)
	case c.IsInt():
		return c.Int()
	case c.IsBool():
		return c.Bool()
	case c.IsNull():
		return nil
	}
	return uint64(c)
}
