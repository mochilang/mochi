// Package vm2 is the from-scratch redesign of the Mochi bytecode VM.
//
// Design contract: MEP-21 v2 (Type-Directed Bytecode and Compiler/VM
// Co-Design) and MEP-20 (Value Representation and Allocation Discipline).
// See website/docs/mep/mep-0021.md.
//
// vm2 is built around an 8-byte NaN-boxed Cell, typed monomorphic
// opcodes emitted directly by compiler2, no Quick-byte rewriting and no
// inline caches on the typed surface. The runtime/vm package continues
// to ship as the observation-driven safety net.
package vm2
