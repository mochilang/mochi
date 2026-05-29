// Package transpiler3/go is the Mochi-to-Go transpiler (MEP-54).
//
// The pipeline is parse + type-check (shared with vm3) -> aotir
// (shared with MEP-45/46/47/48/49/50/51/52/53) -> gotree (Go
// shadow AST) -> Go source via go/format.Source -> go build.
//
// Layout:
//
//   gotree/   Go shadow AST node types with Render methods.
//   lower/    Lowers aotir.Program to gotree.FileSpec.
//   emit/     Renders gotree.FileSpec to Go source (gofmt-clean).
//   build/    Driver: parse -> type-check -> lower -> emit ->
//             write runtime + go.mod + main.go -> go build ->
//             exec the binary and capture stdout.
//   runtime/  Per-program runtime helpers (print formatters,
//             collection helpers, agent supervisor, datalog
//             engine, etc.). Vendored into each build's work
//             directory so the produced binary has no
//             dependency outside the Go stdlib.
//
// Phase 0 ships package stubs only. Phase 1 lights up
// `print("hello, mochi!")`. Subsequent phases follow the
// per-phase tracking pages under
// website/docs/implementation/0054.
package transpiler3go
