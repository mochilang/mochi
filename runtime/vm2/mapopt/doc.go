// Package mapopt is the third head-to-head dispatch-strategy
// comparison (after listopt and stropt), this time on the map
// container. It is the same recipe as before:
//
//   - baseline/ , today's MEP-24 §4 generic map shape, plus a raw
//     map[int64]int64 floor.
//   - mig/      , MEP-26 Migration: per-op switch over the
//     Empty / I64 / Generic map shapes.
//   - ic/       , MEP-27 Inline Cache: per-call-site K=1 shape cache.
//   - aot/      , MEP-28 AOT codegen: specialized handlers per
//     (shape, keyType) pair.
//
// Workload is `maps/fill_sum`: build a map of N int→int entries via
// repeated set, then sum all the values. This is the canonical map
// benchmark on MEP-23 and the one with the largest gap to a real-Go
// floor (4.86x on the most recent sweep).
//
// Each subpackage is self-contained, no shared kernel, so options
// can be enabled/disabled independently and the suite is a clean
// template for the next container (sets).
package mapopt
