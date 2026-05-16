// Package tieredjit is the minimal prototype for MEP-32, the
// tiered method JIT alternative to MEP-30's whole-program template
// JIT and MEP-31's tracing JIT.
//
// MEP-32's full specification is a two-tier pipeline (interpreter
// + baseline tier 1 + optimizing tier 2) with type-feedback-driven
// inlining, escape analysis, and bidirectional on-stack
// replacement. This prototype implements the smallest slice that
// is actually measurable on the tmpljit bytecode:
//
//   - Tier 0: the tmpljit switch interpreter (reused as-is).
//   - Tier 1: the MEP-30 template / copy-and-patch JIT (reused
//     as-is via tmpljit.Compile).
//   - Tier 2: a peephole-optimized compiler that recognises the
//     three patterns the baseline tier deliberately leaves on the
//     floor and emits AArch64 immediate-form instructions:
//
//       MovImm r,c; Mul d,x,r  (r dead) -> ShlImm d,x,log2(c)    if c is a power of two
//                                       -> MulImm d,x,c          otherwise (still 1 instr via mul)
//       MovImm r,c; Add d,x,r  (r dead) -> AddImm d,x,c          if 0 <= c <= 4095
//       MovImm r,1; Add d,x,r  (r dead) -> AddImm d,x,1
//
// The resulting native code drops from 13 to 7 instructions in the
// fillsum loop body. The relevant tier-2 optimizations from
// MEP-32's spec - profile-guided inlining, scalar replacement,
// escape analysis, linear-scan register allocation, deopt to tier
// 1 on guard miss - are out of scope; this prototype exists to
// demonstrate that a tier-2 stack can extract additional perf on
// the same workload MEP-30 already handles, and to put real
// numbers in the MEP-33 appendix.
package tieredjit
