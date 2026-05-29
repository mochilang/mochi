// Package collections provides typed-collection helpers backing
// Mochi list, map, and set operations. Each helper is a thin
// wrapper around a Go slice or map plus invariants the Mochi
// type-checker assumes (e.g. set-element uniqueness).
//
// Phase 3.1 fleshes out List, Phase 3.2 OrderedMap, Phase 3.3
// Set. Phase 0 ships an empty package so the emitter can
// import the path.
package collections
