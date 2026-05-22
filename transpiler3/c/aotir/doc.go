// Package aotir is the typed lowering IR consumed by the
// transpiler3/c emit pass.
//
// Design contract: MEP-45 §1 "Pipeline and IR". See
// website/docs/mep/mep-0045.md and the phase tracking pages
// under website/docs/implementation/0045/.
//
// aotir sits between the type-checked AST and the C source
// emitter. It exists so that lowering decisions (closure
// representation, match decision tree, monomorphic instances,
// effect rewrites) are recorded as first-class structure rather
// than buried in printf templates. Properties:
//
//   - Typed. Every Value and every Function signature carries
//     a concrete monomorphic type. Generics have been resolved
//     upstream.
//   - Closure-converted. Functions take no free variables; a
//     closure is a struct of (code pointer, env pointer).
//   - Match-lowered. Pattern match expressions have been
//     lowered to switch / if-chain decision trees via the
//     Maranget construction.
//   - Effect-rewritten. try/catch has been rewritten to
//     setjmp/longjmp scaffolding; spawn / send / recv to
//     scheduler intrinsics.
//
// Phase 0 ships this package as an empty skeleton. Phase 1
// (hello world) introduces the minimum types and instructions
// needed for a single function that calls one string print.
// Later phases extend the type set and instruction set as their
// gates require.
//
// Independence from compiler3/ir: aotir is a separate IR with
// its own invariants. It does not import compiler3/ir and is
// not used by vm3. The shared upstream is only the parser and
// type checker. See MEP-45 Abstract.
package aotir
