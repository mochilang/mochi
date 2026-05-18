package vm3

// Arenas holds the typed slabs that back every handle Cell. Each slab
// is a plain Go slice, so Go's GC reclaims slab backing memory through
// normal field traversal: the slab slice header lives on a VM field,
// each entry's internal storage (bytes, cells, table) is a slice the
// GC also sees.
//
// Phase 1 grows slabs monotonically (no free-list reuse). Phase 6 wires
// a mark-sweep collector through the free-list fields below.
type Arenas struct {
	Strings  []vmString
	Lists    []vmList
	Maps     []vmMap
	Sets     []vmSet
	Structs  []vmStruct
	Closures []vmClosure
	Bignums  []vmBignum
	Bytes    []vmBytes
	Pairs    []vmPair
	F64Arrs  []vmF64Array
	I64Arrs  []vmI64Array
	U8Arrs   []vmU8Array

	freeStrings  []uint32
	freeLists    []uint32
	freeMaps     []uint32
	freeSets     []uint32
	freeStructs  []uint32
	freeClosures []uint32
	freeBignums  []uint32
	freeBytes    []uint32
	freePairs    []uint32
	freeF64Arrs  []uint32
	freeI64Arrs  []uint32
	freeU8Arrs   []uint32
}

const (
	flagAlive  uint8 = 1 << 0
	flagShared uint8 = 1 << 1
	// flagMarked is set by Phase 5 mark-sweep during the mark phase and
	// cleared during the sweep phase. A slot with flagAlive but not
	// flagMarked at sweep time is freed; an alive+marked slot retains
	// its flagAlive and has flagMarked cleared.
	flagMarked uint8 = 1 << 2
)

type vmString struct {
	gen   uint16
	flags uint8
	_     uint8
	len   uint32
	data  []byte
}

type vmList struct {
	gen      uint16
	flags    uint8
	_        uint8
	len      uint32
	cells    []Cell
	elemType uint8
}

type mapEntry struct {
	hash  uint64
	key   Cell
	value Cell
}

type vmMap struct {
	gen   uint16
	flags uint8
	_     uint8
	nLive uint32
	table []mapEntry
}

type vmSet struct {
	gen   uint16
	flags uint8
	_     uint8
	nLive uint32
	table []mapEntry
}

type vmStruct struct {
	gen     uint16
	flags   uint8
	_       uint8
	shapeID uint32
	fields  []Cell
}

type vmClosure struct {
	gen      uint16
	flags    uint8
	_        uint8
	fnID     uint32
	upvalues []Cell
}

type vmBignum struct {
	gen   uint16
	flags uint8
	sign  int8
	_     uint32
	words []uint64
}

type vmBytes struct {
	gen   uint16
	flags uint8
	_     uint8
	len   uint32
	data  []byte
}

type vmPair struct {
	gen   uint16
	flags uint8
	_     uint8
	_     uint32
	fst   Cell
	snd   Cell
}

type vmF64Array struct {
	gen   uint16
	flags uint8
	_     uint8
	len   uint32
	data  []float64
}

type vmI64Array struct {
	gen   uint16
	flags uint8
	_     uint8
	len   uint32
	data  []int64
}

type vmU8Array struct {
	gen   uint16
	flags uint8
	_     uint8
	len   uint32
	data  []byte
}
