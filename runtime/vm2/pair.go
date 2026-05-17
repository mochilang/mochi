package vm2

import "unsafe"

// vmPair is the packed two-element tuple value type (MEP-37 §3.4).
// Storage is a fixed 32 bytes (two Cells) reached via Cell.Obj. The
// canonical workload is BG binary_trees: ~2M nested two-element nodes
// per run. *vmList would charge one Go heap allocation per node plus a
// []Cell tail; *vmPair lives in a per-VM arena that batches 256 pairs
// per Go heap allocation.
//
// Pairs are immutable post-construction. Mochi has no use case for
// in-place pair mutation; the arena's "monotonic per-Run" contract
// depends on that immutability.
type vmPair struct {
	a, b Cell
}

// pairChunkSize is the batch size for the pair arena. 256 pairs per
// chunk = 8 KiB, fits in L1, and keeps the per-Run chunk count low
// (binary_trees at depth=10 builds ~2K pairs total → ~8 chunks).
const pairChunkSize = 256

// pairChunk is a fixed-size, never-resliced batch of pair storage.
// Once allocated, the address of each slot is stable for the lifetime
// of the chunk, so vmPair pointers carried in Cell.Obj never dangle.
type pairChunk struct {
	data [pairChunkSize]vmPair
}

// newPair allocates a fresh vmPair through the per-VM arena. The
// returned Cell is tagPtr with Obj = *vmPair. Chunks are appended on
// demand; existing chunks stay live until the VM is dropped.
func (vm *VM) newPair(a, b Cell) Cell {
	chunkIdx := vm.pairNext / pairChunkSize
	slot := vm.pairNext % pairChunkSize
	if chunkIdx >= len(vm.pairChunks) {
		vm.pairChunks = append(vm.pairChunks, &pairChunk{})
	}
	p := &vm.pairChunks[chunkIdx].data[slot]
	p.a = a
	p.b = b
	vm.pairNext++
	return Cell{Bits: tagPtr, Obj: unsafe.Pointer(p)}
}

func (vm *VM) pairAt(c Cell) *vmPair { return (*vmPair)(c.PtrTo()) }
