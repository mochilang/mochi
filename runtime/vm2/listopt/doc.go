// Package listopt hosts three independent prototype implementations of
// the vm2 list subsystem so the dispatch strategies specified in
// MEP-26, MEP-27 and MEP-28 can be measured head-to-head.
//
// Each sub-package (mig, ic, aot) is a self-contained mini-VM that
// runs the same fill+sum workload using a different dispatch strategy:
//
//   - mig (MEP-26, Migration). Generic opcodes; the handler runtime-
//     switches on the backing-store concrete type and migrates
//     *vmListI64 to *vmList on the first non-int operation.
//
//   - ic (MEP-27, Inline Cache). Generic opcodes; each instruction
//     site carries a per-ip cache of the last-seen shape. Hit path
//     is one shape-tag compare; miss path updates the IC and falls
//     through to the polymorphic handler.
//
//   - aot (MEP-28, AOT). Specialized opcode per (op, shape). The
//     handler knows its shape statically; no runtime check.
//
// The three packages share no code on the hot path so the bench
// numbers reflect the dispatch strategy alone, not the kernel. The
// non-list opcodes (load const, add, branch, return) are identical
// byte-for-byte across packages so the only measured delta is the
// list dispatch cost.
//
// Adding the next container (maps) replicates this directory:
// listopt/maps/{mig,ic,aot}/, each with the same skeleton.
//
// Results are written up in MEP-29.
package listopt
