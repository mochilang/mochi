// Package cgen lowers compiler3 IR to portable C99 source. It is the
// emitter half of the MEP-42 AOT track (the build/c driver invokes
// the system cc on the emitted .c file to produce a single native
// binary). Phase 4.0 ships the load-bearing skeleton: scalar i64 /
// f64 / bool ops, immediate-form variants, the full six-comparison
// set, and the three terminator kinds (TermReturn, TermJump,
// TermBranch). Strings, lists, maps, query algebra, and OpCallGo are
// out of scope for Phase 4.0 and report ErrUnsupportedOp so the
// driver can refuse rather than emit code that would mis-link.
//
// # Reading order
//
//  1. doc.go (this file) for scope and the Phase 4.0 gate.
//  2. emit.go for Program / Emit / emitFunc / emitValue / emitTerminator.
//  3. emit_test.go for the byte-level shape contract.
//
// # Phase 4.0 scope
//
// Supported IR types:
//
//   - TypeI64  -> int64_t
//   - TypeF64  -> double
//   - TypeBool -> int (0/1 canonical form)
//   - TypeUnit -> void (function-result only)
//
// Supported ops: OpParam, OpConst, OpPhi, the i64 arithmetic family
// (Op{Add,Sub,Mul,Div,Mod,Neg}I64 plus their *Imm variants), the f64
// arithmetic family (OpAdd/Sub/Mul/Div/NegF64), the six i64
// comparisons (Op{Eq,Ne,Lt,Le,Gt,Ge}I64 plus *Imm variants).
//
// Supported terminators: TermReturn, TermJump, TermBranch.
//
// # SSA-to-three-address lowering
//
// Every SSA value is declared as a C local at the function head with
// zero initializer; assignments inside blocks use plain "=". C99
// allows goto across declarations only when no variable-length array
// or VLA pointer is jumped over, so hoisting all decls to the
// function head makes label-driven control flow unconstrained.
//
// Phi values are lowered the same way: declared at the function
// head, the predecessor block assigns the appropriate source value
// before its terminator.
//
// # MEP-42 §13 identity rule
//
// The emitter writes C99 and nothing else. Compiler-specific
// extensions (GCC statement-expressions, Clang __builtin_*, MSVC
// __declspec) are forbidden. The driver invokes the system cc with
// -std=c99 to enforce this at build time.
//
// Phase 4.0 is x86_64 Linux only (gate alignment with §Phased-plan
// row 4). Phase 5.0 widens to the four phase-1 native targets via
// the same emitter (the output is target-independent; only the
// driver's cc invocation changes).
package cgen
