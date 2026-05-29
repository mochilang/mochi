// Package stream provides iter.Seq helpers used to back Mochi
// stream pipelines (map / filter / take / drop / fold) without
// materialising intermediate slices.
//
// Phase 10 introduces the full implementation. Phase 0 ships
// an empty package so the emitter can import the path.
package stream
