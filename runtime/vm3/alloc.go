package vm3

// AllocString reserves a string slot, copies src into the slot's
// backing buffer, and returns a tagHandle Cell pointing at it.
// Free-list slots are reused first; the slot's generation counter
// bumps on every reuse so any escaped stale handle fails the debug
// check.
func (a *Arenas) AllocString(src []byte) Cell {
	idx, gen := a.takeStringSlot()
	s := &a.Strings[idx]
	if cap(s.data) < len(src) {
		s.data = make([]byte, len(src))
	} else {
		s.data = s.data[:len(src)]
	}
	copy(s.data, src)
	s.len = uint32(len(src))
	s.flags = flagAlive
	return MakeHandle(ArenaString, gen, idx)
}

func (a *Arenas) takeStringSlot() (idx uint32, gen uint16) {
	if n := len(a.freeStrings); n > 0 {
		idx = a.freeStrings[n-1]
		a.freeStrings = a.freeStrings[:n-1]
		a.Strings[idx].gen++
		gen = a.Strings[idx].gen
		return
	}
	idx = uint32(len(a.Strings))
	a.Strings = append(a.Strings, vmString{flags: flagAlive})
	return idx, 0
}

// AllocList reserves a list slot with the given element type and
// capacity hint, and returns a tagHandle Cell.
func (a *Arenas) AllocList(elemType uint8, capHint int) Cell {
	idx, gen := a.takeListSlot(capHint)
	l := &a.Lists[idx]
	l.elemType = elemType
	l.flags = flagAlive
	l.len = 0
	return MakeHandle(ArenaList, gen, idx)
}

func (a *Arenas) takeListSlot(capHint int) (idx uint32, gen uint16) {
	if n := len(a.freeLists); n > 0 {
		idx = a.freeLists[n-1]
		a.freeLists = a.freeLists[:n-1]
		a.Lists[idx].gen++
		gen = a.Lists[idx].gen
		if cap(a.Lists[idx].cells) < capHint {
			a.Lists[idx].cells = make([]Cell, 0, capHint)
		} else {
			a.Lists[idx].cells = a.Lists[idx].cells[:0]
		}
		return
	}
	idx = uint32(len(a.Lists))
	a.Lists = append(a.Lists, vmList{
		flags: flagAlive,
		cells: make([]Cell, 0, capHint),
	})
	return idx, 0
}

// AllocMap reserves an empty open-addressed map slot.
func (a *Arenas) AllocMap(capHint int) Cell {
	idx, gen := a.takeMapSlot(capHint)
	return MakeHandle(ArenaMap, gen, idx)
}

func (a *Arenas) takeMapSlot(capHint int) (idx uint32, gen uint16) {
	if n := len(a.freeMaps); n > 0 {
		idx = a.freeMaps[n-1]
		a.freeMaps = a.freeMaps[:n-1]
		a.Maps[idx].gen++
		gen = a.Maps[idx].gen
		a.Maps[idx].nLive = 0
		if cap(a.Maps[idx].table) < capHint {
			a.Maps[idx].table = make([]mapEntry, 0, capHint)
		} else {
			a.Maps[idx].table = a.Maps[idx].table[:0]
		}
		a.Maps[idx].flags = flagAlive
		return
	}
	idx = uint32(len(a.Maps))
	a.Maps = append(a.Maps, vmMap{
		flags: flagAlive,
		table: make([]mapEntry, 0, capHint),
	})
	return idx, 0
}

// AllocSet reserves an empty open-addressed set slot.
func (a *Arenas) AllocSet(capHint int) Cell {
	idx, gen := a.takeSetSlot(capHint)
	return MakeHandle(ArenaSet, gen, idx)
}

func (a *Arenas) takeSetSlot(capHint int) (idx uint32, gen uint16) {
	if n := len(a.freeSets); n > 0 {
		idx = a.freeSets[n-1]
		a.freeSets = a.freeSets[:n-1]
		a.Sets[idx].gen++
		gen = a.Sets[idx].gen
		a.Sets[idx].nLive = 0
		if cap(a.Sets[idx].table) < capHint {
			a.Sets[idx].table = make([]mapEntry, 0, capHint)
		} else {
			a.Sets[idx].table = a.Sets[idx].table[:0]
		}
		a.Sets[idx].flags = flagAlive
		return
	}
	idx = uint32(len(a.Sets))
	a.Sets = append(a.Sets, vmSet{
		flags: flagAlive,
		table: make([]mapEntry, 0, capHint),
	})
	return idx, 0
}

// AllocStruct reserves a struct slot with the given shape and field
// count. Fields are initialized to CNull().
func (a *Arenas) AllocStruct(shapeID uint32, nFields int) Cell {
	idx, gen := a.takeStructSlot(shapeID, nFields)
	return MakeHandle(ArenaStruct, gen, idx)
}

func (a *Arenas) takeStructSlot(shapeID uint32, nFields int) (idx uint32, gen uint16) {
	if n := len(a.freeStructs); n > 0 {
		idx = a.freeStructs[n-1]
		a.freeStructs = a.freeStructs[:n-1]
		s := &a.Structs[idx]
		s.gen++
		gen = s.gen
		s.shapeID = shapeID
		if cap(s.fields) < nFields {
			s.fields = make([]Cell, nFields)
		} else {
			s.fields = s.fields[:nFields]
			clear(s.fields)
		}
		s.flags = flagAlive
		return
	}
	idx = uint32(len(a.Structs))
	a.Structs = append(a.Structs, vmStruct{
		flags:   flagAlive,
		shapeID: shapeID,
		fields:  make([]Cell, nFields),
	})
	return idx, 0
}

// AllocClosure reserves a closure slot.
func (a *Arenas) AllocClosure(fnID uint32, nUpvalues int) Cell {
	idx, gen := a.takeClosureSlot(fnID, nUpvalues)
	return MakeHandle(ArenaClosure, gen, idx)
}

func (a *Arenas) takeClosureSlot(fnID uint32, nUpvalues int) (idx uint32, gen uint16) {
	if n := len(a.freeClosures); n > 0 {
		idx = a.freeClosures[n-1]
		a.freeClosures = a.freeClosures[:n-1]
		c := &a.Closures[idx]
		c.gen++
		gen = c.gen
		c.fnID = fnID
		if cap(c.upvalues) < nUpvalues {
			c.upvalues = make([]Cell, nUpvalues)
		} else {
			c.upvalues = c.upvalues[:nUpvalues]
			clear(c.upvalues)
		}
		c.flags = flagAlive
		return
	}
	idx = uint32(len(a.Closures))
	a.Closures = append(a.Closures, vmClosure{
		flags:    flagAlive,
		fnID:     fnID,
		upvalues: make([]Cell, nUpvalues),
	})
	return idx, 0
}

// AllocBignum reserves a bignum slot with the given sign and word
// count. Words are zero-initialized.
func (a *Arenas) AllocBignum(sign int8, nWords int) Cell {
	idx, gen := a.takeBignumSlot(sign, nWords)
	return MakeHandle(ArenaBignum, gen, idx)
}

func (a *Arenas) takeBignumSlot(sign int8, nWords int) (idx uint32, gen uint16) {
	if n := len(a.freeBignums); n > 0 {
		idx = a.freeBignums[n-1]
		a.freeBignums = a.freeBignums[:n-1]
		b := &a.Bignums[idx]
		b.gen++
		gen = b.gen
		b.sign = sign
		if cap(b.words) < nWords {
			b.words = make([]uint64, nWords)
		} else {
			b.words = b.words[:nWords]
			clear(b.words)
		}
		b.flags = flagAlive
		return
	}
	idx = uint32(len(a.Bignums))
	a.Bignums = append(a.Bignums, vmBignum{
		flags: flagAlive,
		sign:  sign,
		words: make([]uint64, nWords),
	})
	return idx, 0
}

// AllocBytes reserves a bytes slot and copies src into it.
func (a *Arenas) AllocBytes(src []byte) Cell {
	idx, gen := a.takeBytesSlot()
	b := &a.Bytes[idx]
	if cap(b.data) < len(src) {
		b.data = make([]byte, len(src))
	} else {
		b.data = b.data[:len(src)]
	}
	copy(b.data, src)
	b.len = uint32(len(src))
	b.flags = flagAlive
	return MakeHandle(ArenaBytes, gen, idx)
}

func (a *Arenas) takeBytesSlot() (idx uint32, gen uint16) {
	if n := len(a.freeBytes); n > 0 {
		idx = a.freeBytes[n-1]
		a.freeBytes = a.freeBytes[:n-1]
		a.Bytes[idx].gen++
		gen = a.Bytes[idx].gen
		return
	}
	idx = uint32(len(a.Bytes))
	a.Bytes = append(a.Bytes, vmBytes{flags: flagAlive})
	return idx, 0
}

// AllocPair reserves a (first, second) pair slot.
func (a *Arenas) AllocPair(fst, snd Cell) Cell {
	idx, gen := a.takePairSlot()
	a.Pairs[idx].fst = fst
	a.Pairs[idx].snd = snd
	return MakeHandle(ArenaPair, gen, idx)
}

func (a *Arenas) takePairSlot() (idx uint32, gen uint16) {
	if n := len(a.freePairs); n > 0 {
		idx = a.freePairs[n-1]
		a.freePairs = a.freePairs[:n-1]
		a.Pairs[idx].gen++
		gen = a.Pairs[idx].gen
		a.Pairs[idx].flags = flagAlive
		return
	}
	idx = uint32(len(a.Pairs))
	a.Pairs = append(a.Pairs, vmPair{flags: flagAlive})
	return idx, 0
}

// AllocF64Arr reserves an f64-array slot of the given length.
func (a *Arenas) AllocF64Arr(n int) Cell {
	idx, gen := a.takeF64ArrSlot(n)
	return MakeHandle(ArenaF64Arr, gen, idx)
}

func (a *Arenas) takeF64ArrSlot(n int) (idx uint32, gen uint16) {
	if k := len(a.freeF64Arrs); k > 0 {
		idx = a.freeF64Arrs[k-1]
		a.freeF64Arrs = a.freeF64Arrs[:k-1]
		arr := &a.F64Arrs[idx]
		arr.gen++
		gen = arr.gen
		if cap(arr.data) < n {
			arr.data = make([]float64, n)
		} else {
			arr.data = arr.data[:n]
			clear(arr.data)
		}
		arr.len = uint32(n)
		arr.flags = flagAlive
		return
	}
	idx = uint32(len(a.F64Arrs))
	a.F64Arrs = append(a.F64Arrs, vmF64Array{
		flags: flagAlive,
		len:   uint32(n),
		data:  make([]float64, n),
	})
	return idx, 0
}

// AllocI64Arr reserves an i64-array slot of the given length.
func (a *Arenas) AllocI64Arr(n int) Cell {
	idx, gen := a.takeI64ArrSlot(n)
	return MakeHandle(ArenaI64Arr, gen, idx)
}

func (a *Arenas) takeI64ArrSlot(n int) (idx uint32, gen uint16) {
	if k := len(a.freeI64Arrs); k > 0 {
		idx = a.freeI64Arrs[k-1]
		a.freeI64Arrs = a.freeI64Arrs[:k-1]
		arr := &a.I64Arrs[idx]
		arr.gen++
		gen = arr.gen
		if cap(arr.data) < n {
			arr.data = make([]int64, n)
		} else {
			arr.data = arr.data[:n]
			clear(arr.data)
		}
		arr.len = uint32(n)
		arr.flags = flagAlive
		return
	}
	idx = uint32(len(a.I64Arrs))
	a.I64Arrs = append(a.I64Arrs, vmI64Array{
		flags: flagAlive,
		len:   uint32(n),
		data:  make([]int64, n),
	})
	return idx, 0
}

// AllocU8Arr reserves a u8-array slot of the given length.
func (a *Arenas) AllocU8Arr(n int) Cell {
	idx, gen := a.takeU8ArrSlot(n)
	return MakeHandle(ArenaU8Arr, gen, idx)
}

func (a *Arenas) takeU8ArrSlot(n int) (idx uint32, gen uint16) {
	if k := len(a.freeU8Arrs); k > 0 {
		idx = a.freeU8Arrs[k-1]
		a.freeU8Arrs = a.freeU8Arrs[:k-1]
		arr := &a.U8Arrs[idx]
		arr.gen++
		gen = arr.gen
		if cap(arr.data) < n {
			arr.data = make([]byte, n)
		} else {
			arr.data = arr.data[:n]
			clear(arr.data)
		}
		arr.len = uint32(n)
		arr.flags = flagAlive
		return
	}
	idx = uint32(len(a.U8Arrs))
	a.U8Arrs = append(a.U8Arrs, vmU8Array{
		flags: flagAlive,
		len:   uint32(n),
		data:  make([]byte, n),
	})
	return idx, 0
}
