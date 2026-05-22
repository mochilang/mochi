// Package lower turns a type-checked Mochi program into aotir.
//
// Design contract: MEP-45 §1 "Pipeline and IR". See
// website/docs/mep/mep-0045.md.
//
// Public entry point (introduced incrementally by phase):
//
//	func Lower(prog *tast.Program) (*aotir.Program, error)
//
// The pass tree, in order:
//
//  1. Monomorphise. Walk every call site, instantiate every
//     generic function and type for each concrete type tuple
//     used. Mochi forbids polymorphic recursion, so the
//     instantiation set is finite (see MEP-45 §4).
//  2. Match-to-decision-tree. Replace match expressions with
//     the Maranget decision tree, sharing tests across arms
//     where possible.
//  3. Closure-convert. Lift every nested function; rewrite
//     references to captured variables as field accesses on
//     an explicit env struct; replace function values with
//     (code, env) fat pointers.
//  4. Effect rewrite. Lower try/catch to setjmp/longjmp
//     scaffolding; lower spawn / send / recv to scheduler
//     intrinsics (Phase 9).
//  5. Emit aotir.Program.
//
// Phase 0 ships the package skeleton. Each later phase plugs
// in the passes its gate requires. Tests live alongside each
// pass (e.g. lower/match_test.go for the Maranget pass).
package lower
