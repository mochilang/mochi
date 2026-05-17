// Package trampoline provides the Go/native boundary for calling JIT'd vm2
// functions. See MEP-34 §Trampoline for the design contract.
//
// The JIT ABI (arm64): x0 = *Cell register file (in/out), returns result Cell
// in x0. The trampoline sets x0 = regs, calls the entry pointer, and returns
// the uint64 result.
package trampoline
