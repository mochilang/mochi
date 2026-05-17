package runtime

// TODO(Phase 1): CallVariadic — handles OpCall to a function with more than
// 16 arguments (beyond the stack-snapshot fast path). Also the cross-tier
// call shim when a JIT'd function calls an interpreter-only callee.
