// Package frontend lowers parsed Mochi programs into compiler3 IR. It
// is the missing link between the Mochi parser/AST and
// compiler3/emit/go, closing out the deferred wiring noted in MEP-43
// Phase 6/7/8/9/10.
//
// Phase-6 close-out scope: a minimum-viable lowering that covers the
// i64 surface (let / var / return / if / while / binary arith /
// comparisons / function calls / print). It is sufficient to drive the
// A/B harness end-to-end on the numeric fixtures in tests/vm/valid and
// to remove `ErrNewPathPending` from RunNew. Wider type coverage (str,
// list, map, struct) re-uses the same lowering shape and lands as the
// existing IR ops admit them.
package frontend
