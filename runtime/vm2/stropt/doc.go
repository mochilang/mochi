// Package stropt hosts three independent prototype implementations of
// the vm2 string subsystem plus a no-rope baseline so the MEP-26/27/28
// dispatch strategies can be measured head-to-head on the canonical
// strings/concat_loop workload.
//
// Each sub-package implements three string shapes (Inline ≤5 bytes,
// Flat *vmString, Rope *vmStringRope) and the same concat decision
// tree from MEP-25 §1 (inline-pack / flat-copy / rope-link by length
// thresholds). They differ only in how OpConcatStr dispatches on the
// shape of its operands:
//
//   - baseline (no rope, today's MEP-24 §2 shape). Always Flat;
//     buffer-doubling. The reference point that ropes must beat.
//
//   - mig (MEP-26). Multi-arm Go type switch on both operands; the
//     result shape is chosen by a length/threshold decision tree.
//
//   - ic (MEP-27). Per-call-site K=4 cache of the observed
//     (leftShape, rightShape) tuple; cached arm jumps directly to
//     the specialized handler; miss updates the IC and falls
//     through.
//
//   - aot (MEP-28). One specialized handler per
//     (leftShape × rightShape) tuple, dispatched via an explicit
//     2D table indexed by the operand shapes; no IC, no type
//     switch.
//
// Unlike the list workload (where the shape is stable for the whole
// loop), strings/concat_loop is genuinely polymorphic: the left
// operand starts Inline, becomes Flat after the inline limit is
// crossed, then becomes Rope past the flat-concat threshold. The
// strategies are therefore stressed differently than lists were:
//
//   - Mig pays a 3x3 switch per iteration regardless.
//   - IC stabilizes after each shape transition, paying full miss
//     cost once per transition (≤3 misses total).
//   - AOT pays one table indirection per iteration but skips the
//     switch entirely on every iteration.
//
// Results are written up in MEP-29 alongside the list numbers.
package stropt
