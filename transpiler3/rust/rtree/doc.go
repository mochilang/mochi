// Package rtree is the Rust shadow AST used by the MEP-53 Rust transpiler.
//
// Each node corresponds to a Rust 2024 edition source construct. The
// renderer (File.RustSource) emits idiomatic, rustfmt-clean source so
// the build driver can hand the output straight to cargo without
// running rustfmt as a post-pass.
//
// Determinism: emit order is exactly the slice order on every node.
// The lower pass must therefore decide ordering before populating
// these nodes (alphabetical for declarations, source order for
// statements).
package rtree
