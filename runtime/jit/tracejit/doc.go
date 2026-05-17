// Package tracejit is the minimal prototype for MEP-31, the
// tracing JIT alternative to MEP-30's whole-program template JIT.
//
// It runs the same 6-opcode bytecode as tmpljit (lifted directly,
// not re-implemented) so the two prototypes can be benchmarked
// head-to-head on the same fillsum workload. The differences from
// MEP-30:
//
//   - The compiled unit is a *loop iteration*, not a whole program.
//     Recording starts when a back-edge target accumulates more
//     than TraceThreshold hits, captures one full iteration from
//     header to back-edge, then closes the trace.
//   - The loop branch is rewritten as an explicit guard: while the
//     guard holds, native code keeps iterating; on guard miss it
//     side-exits, writes the live VM registers back into the
//     interpreter's register file, and returns the PC where the
//     interpreter should resume (the instruction after the
//     original back-edge).
//   - There is no support for trace trees, allocation removal,
//     inlining, blacklisting, or any of the production features in
//     MEP-31's full specification. Those are deferred; this
//     prototype exists to put real numbers in MEP-33.
package tracejit
