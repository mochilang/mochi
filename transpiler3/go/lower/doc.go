// Package lower turns a type-checked Mochi program (via the
// shared aotir IR) into a gotree.File ready for emit.
//
// Design contract: MEP-54 §Phase 0+. See
// website/docs/mep/mep-0054.md.
//
// Public entry point (introduced incrementally by phase):
//
//	func Lower(prog *aotir.Program) (*gotree.File, error)
//
// Pipeline (each step lights up in its own phase):
//
//  1. Walk Program.Records / Unions: emit Go struct + tagged-
//     union shapes.
//  2. Walk Program.Functions: lower each function to a
//     gotree.FuncDecl with body lowered statement-by-statement.
//  3. Walk Program.Datalog / Agents / Streams: emit calls into
//     dev.mochilang/runtime/go.
//  4. Add imports collected during walk; defer to gotree's
//     import sorter so the output is gofmt-clean.
//
// Phase 0 ships only the package skeleton and the lowerer type.
package lower
