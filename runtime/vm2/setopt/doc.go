// Package setopt is the fourth head-to-head dispatch-strategy
// comparison (after listopt, stropt, mapopt). Same recipe:
//
//   - baseline/  , today's MEP-24 §4 generic set shape (map[Cell]struct{})
//     plus a raw map[int64]struct{} floor.
//   - mig/       , MEP-26 Migration over SetI64 / Generic shapes.
//   - ic/        , MEP-27 K=1 IC slot per call site.
//   - aot/       , MEP-28 specialized I64 handlers.
//
// Workload is sets/fill_probe: insert N ints, then probe N members
// and count hits. This is the canonical set benchmark on MEP-23.
//
// Prediction (from the maps result): sets should pattern-match maps,
// since the underlying Go op is map-class. Baseline likely wins;
// AOT in the noise; IC and Migration regress.
package setopt
