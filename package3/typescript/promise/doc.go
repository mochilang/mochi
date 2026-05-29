// Package promise translates TS Promise<T> ↔ Mochi async fun and TS
// AsyncIterable<T> ↔ Mochi stream<T>. No runtime singleton is constructed: the
// host JS event loop is intrinsic to every target. Lands in MEP-72 Phase 14.
//
// See website/docs/implementation/0072/phase-14-promise-async.md.
package promise
