// Package vm2jit is the production baseline JIT for the vm2 interpreter,
// specified in MEP-34.
//
// It lowers every vm2 opcode (runtime/vm2/ops.go) to native AArch64 and
// AMD64 machine code, one Function at a time, using the copy-and-patch
// template strategy validated by MEP-30 and MEP-33.
//
// # Architecture
//
// The JIT is single-tier baseline (no IR, no register allocator beyond the
// existing vm2 register file). Each compiled Function is entered via a
// pure-Go .s trampoline (see trampoline/); cgo is forbidden on the hot path.
//
// Slow-path callouts (NewList, ListPush, MapSet, ConcatStr, etc.) are thin
// Go shims in runtime/; they share a fixed calling convention so one
// trampoline shape covers every callout.
//
// # Integration with vm2
//
// vm2.JITCallFn (set in init()) routes OpCall to the JIT when
// callee.JITCode != nil. Frame layout is shared between interpreter and JIT:
// regs live in vm.Stack[frame.RegsBase:], Cells are NaN-boxed identically on
// both sides. A JIT'd function can call an interpreted callee and vice versa.
//
// # Engineering phases (MEP-34)
//
//   - Phase 1: arithmetic + control flow + list opcodes + calls.
//     Benchmark gate: ≥1.5x over vm2 on lists/fill_sum and arith/fib_iter.
//   - Phase 2: string + map opcodes.
//     Benchmark gate: ≥1.5x on strings/concat_loop and maps/fill_probe.
//   - Phase 3: set + struct opcodes.
//     Benchmark gate: full MEP-23 corpus, no regression, cross-language table.
package vm2jit
