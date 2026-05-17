// Package runtime contains the slow-path Go shims called by JIT'd code for
// opcodes that touch the GC heap or the Objects table. Every shim is reached
// via the arch-specific trampoline; the JIT's calling convention saves all
// live registers before the call and restores them after.
//
// The shims in this package are stubs pending Phase 1 implementation. They
// will be wired up once vm2 exports the relevant primitives (AllocList,
// ListAppend, etc.) and the trampoline calling convention is finalised.
package runtime

// TODO(Phase 1): implement NewListSlow, ListPushSlow, ListGetSlowOOB,
// ListSetSlowOOB once vm2 exports the required heap-allocation primitives.
// See MEP-34 §List opcodes for the expected signatures.
