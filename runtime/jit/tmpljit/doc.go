// Package tmpljit is the MEP-30 reference implementation: a
// copy-and-patch baseline JIT for a tiny register-based bytecode.
//
// Scope. This is a proof of the dispatch architecture, not a full
// vm2 JIT. The bytecode is a 6-opcode register VM sufficient to
// express the canonical fill_sum-shaped workload defined in
// workload.go. Three execution backends are provided so the
// dispatch-strategy delta can be measured cleanly:
//
//   - FloorGo: hand-written Go, the theoretical floor.
//   - Interp:  classic switch-dispatched interpreter over the bytecode.
//   - JIT:     copy-and-patch JIT that stitches one pre-defined
//     ARM64 instruction sequence per opcode into an mmap'd executable
//     buffer, with two-pass relocation for backward branches.
//
// Target. darwin/arm64 only. The choice mirrors MEP-30's tier 1
// surface; a linux/amd64 backend is a separate engineering
// commitment specified but not implemented here.
//
// Trampoline. The JIT entry point is reached via a cgo function-
// pointer call (see exec_arm64.go). MEP-30 specifies that the
// production trampoline must be pure Go (a hand-written .s file
// using Go's ABI) so that Mochi distributes as a single static
// binary. The prototype uses cgo because it is 5 lines and
// demonstrably correct; replacing it with the pure-Go trampoline is
// a localized change and is tracked separately.
package tmpljit
