// Package vm3 is the from-scratch successor to runtime/vm2.
//
// Design contract: MEP-40 (vm3 + compiler3: 8-byte handle Cell, typed
// arenas, static-type-driven dispatch). See website/docs/mep/mep-0040.md.
//
// vm3 is built around three load-bearing ideas:
//
//  1. 8-byte Cell with handle-based NaN-boxing. The single uint64 carries
//     inline ints (48-bit signed), floats (full NaN range), bools, null,
//     inline short strings (up to 5 bytes), deopt sentinels, and arena
//     handles. Half the register-file cache footprint of vm2's 16-byte
//     {Bits, Obj} Cell.
//
//  2. Typed arenas with Go-GC-friendly slabs. Each container type lives
//     in its own Go-allocated slice. Slabs are reachable through normal
//     Go field traversal, so Go's GC reclaims slab backing without ever
//     inspecting handle bits.
//
//  3. Typed register banks per frame. Each Frame carries regsI64
//     []int64, regsF64 []float64, regsCell []Cell. compiler3 picks the
//     bank at emit time based on each SSA value's static type. Typed ops
//     read and write native machine words; the Cell envelope only
//     appears at boundaries (polymorphic call arguments, generic list
//     elements, return values to dyn-typed callers).
//
// vm3 ships side-by-side with runtime/vm2 until Phase 7 cut-over.
package vm3
