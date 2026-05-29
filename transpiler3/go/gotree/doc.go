// Package gotree is the Go shadow AST emitted by the MEP-54
// Mochi-to-Go transpiler.
//
// Design contract: MEP-54 §Phase 0 "Skeleton". See
// website/docs/mep/mep-0054.md and the implementation tracking
// pages under website/docs/implementation/0054.
//
// gotree mirrors the stdlib go/ast node set but is decoupled
// from the Go parser: we synthesize Go source, we never parse
// it. Each node is a plain Go struct with the structural
// fields that survive a round-trip through go/printer plus
// fields that the lowerer needs at construction time (e.g.
// CommentGroup attached to a declaration).
//
// Entry point:
//
//	file := &gotree.File{PackageName: "main", ...}
//	src, err := file.Render() // gofmt-clean []byte
//
// Render walks the tree into a bytes.Buffer of raw Go source
// then passes the result through go/format.Source. The lowerer
// is allowed to be sloppy about whitespace and import order;
// format.Source canonicalises both, so the rendered output is
// byte-identical to what `gofmt` would produce on the same
// program.
//
// Phase 0 ships the node types, the Writer helper, and File's
// Render() entry point. Subsequent phases extend Stmt, Expr,
// and Decl to cover whatever IR shapes the gate requires.
package gotree
