package vm3

// Open-addressed i64-keyed map helpers backing OpMapSetI64I64 and
// OpMapGetI64I64. The table uses linear probing with a power-of-two
// capacity; an entry is empty when entry.hash == 0. Live hashes are
// forced odd (|1) so the zero-value of mapEntry is unambiguously empty.
//
// Both keys and values are stored as Cells (CInt) so the same arena
// slab will later carry mixed-type maps. The i64 fast path only reads
// .Int() on retrieval; Phase 4 may add a typed-key map for further
// box elision.

const mapInitCap = 8

// mapCapForEntries returns the smallest power-of-two table size that
// holds nEntries inserts without crossing the load-factor 0.5 grow
// trigger (2*(nLive+1) > cap). Floor is mapInitCap.
func mapCapForEntries(nEntries int) int {
	if nEntries <= 0 {
		return mapInitCap
	}
	need := uint32(2*nEntries) + 2
	c := uint32(mapInitCap)
	for c < need {
		c <<= 1
	}
	return int(c)
}

func hashI64(k int64) uint64 {
	x := uint64(k)
	x ^= x >> 30
	x *= 0xbf58476d1ce4e5b9
	x ^= x >> 27
	x *= 0x94d049bb133111eb
	x ^= x >> 31
	return x | 1
}

func (a *Arenas) growMap(m *vmMap, newCap int) {
	old := m.table
	if newCap < mapInitCap {
		newCap = mapInitCap
	}
	m.table = make([]mapEntry, newCap)
	m.nLive = 0
	mask := uint32(newCap - 1)
	for i := range old {
		e := &old[i]
		if e.hash == 0 {
			continue
		}
		pos := uint32(e.hash) & mask
		for {
			if m.table[pos].hash == 0 {
				m.table[pos] = *e
				m.nLive++
				break
			}
			pos = (pos + 1) & mask
		}
	}
}

// MapSetI64 puts (k, v) into the map handle c, growing the table when
// load factor exceeds 0.5. Caller must have verified c is a map handle.
func (a *Arenas) MapSetI64(c Cell, k, v int64) {
	_, _, idx := c.DecodeHandle()
	m := &a.Maps[idx]
	if len(m.table) == 0 || uint32(2)*(m.nLive+1) > uint32(len(m.table)) {
		a.growMap(m, max(2*len(m.table), mapInitCap))
	}
	h := hashI64(k)
	mask := uint32(len(m.table) - 1)
	pos := uint32(h) & mask
	for {
		e := &m.table[pos]
		if e.hash == 0 {
			e.hash = h
			e.key = CInt(k)
			e.value = CInt(v)
			m.nLive++
			return
		}
		if e.hash == h && e.key.Int() == k {
			e.value = CInt(v)
			return
		}
		pos = (pos + 1) & mask
	}
}

// MapGetI64 returns the i64 value associated with key k in map handle c.
// If k is absent, returns 0 (the kernel only reads keys it has written).
func (a *Arenas) MapGetI64(c Cell, k int64) int64 {
	_, _, idx := c.DecodeHandle()
	m := &a.Maps[idx]
	if len(m.table) == 0 {
		return 0
	}
	h := hashI64(k)
	mask := uint32(len(m.table) - 1)
	pos := uint32(h) & mask
	for {
		e := &m.table[pos]
		if e.hash == 0 {
			return 0
		}
		if e.hash == h && e.key.Int() == k {
			return e.value.Int()
		}
		pos = (pos + 1) & mask
	}
}
