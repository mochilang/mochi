// Package typebridge maps go/types.Type values to a structural,
// Mochi-side Type value (GoToMochi) and back to a Go source-level
// type string (MochiToGo).
//
// The bridge is the only hand-maintained mapping in the MEP-43 zero-
// glue Go target pipeline (MEP-43 §2, MEP-44 §1). Every later phase
// of MEP-43 consumes its output: the binding resolver (MEP-43 Phase
// 2) builds a *ir.PackageBinding tree whose every type is a Type
// produced here; the Go-source emitter (MEP-43 Phase 4) renders
// every type via MochiToGo; the export direction (MEP-43 Phase 8)
// renders Mochi types as Go signatures via MochiToGo in reverse.
//
// The bridge has no per-package code, no per-symbol code, and no
// reflection. It dispatches purely on go/types structural shape.
// Unsupported shapes degrade to Type{Kind: KindOpaque, OpaqueReason:
// ...} with a documented reason code; downstream consumers may pass
// the value through but may not destructure it. See OpaqueReason for
// the enumerated reasons.
//
// Wire format. Type implements gob.GobEncoder / gob.GobDecoder with
// a leading format-version byte (currently 0x01). A Mochi upgrade
// that adds a new Kind or OpaqueReason bumps the version and the
// Phase 2 cache invalidator discards stale entries. See gob.go.
//
// MEP references.
//   - MEP-43 §2: high-level architecture; this package is the
//     "type bridge" box in the diagram.
//   - MEP-43 §11.8: opaque-density risk; enforced by stdlib_test.go.
//   - MEP-44: deep-dive specification of this package; every public
//     decision (type model, constraint table, gob format) is pinned
//     there.
//
// Concurrency. All exported functions are safe for concurrent use.
// Type values are deep-copied on encode/decode; mutating a returned
// Type does not affect any other caller's view.
package typebridge
