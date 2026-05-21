// Package copypatch is the MEP-42 Phase 1 copy-and-patch JIT backend
// for compiler3 IR.
//
// # Strategy
//
// One hand-written C stencil per compiler3 IR opcode, compiled by
// Clang -O2 -fno-asynchronous-unwind-tables -fno-stack-protector
// -mno-red-zone at Mochi build time. The stencilgen tool
// (tools/stencilgen) extracts the resulting machine bytes and the
// relocation table from the .text section and emits a generated Go
// file per ISA: stencils_amd64.go, stencils_arm64.go. At runtime, the
// emitter walks the IR, picks the stencil for each op, memcpy's the
// bytes into an mmap'd executable region, and applies the relocations
// in place. The patched code is then jumped to via the runtime/jit
// trampoline.
//
// The technique is from Xu + Kjolstad, "Copy-and-Patch Compilation"
// (PLDI 2021) and is production-validated by CPython 3.13 (PEP 744,
// October 2024). The Mochi adaptation differs from CPython in three
// load-bearing ways:
//
//  1. Mochi IR is typed end-to-end (MEP-40). Every operand carries a
//     proven static type, so stencils never need to dispatch on tag.
//     The CPython JIT's "type observation" tier is unnecessary.
//
//  2. Mochi reserves three callee-save registers across stencil calls
//     (R12 = Frame, R13 = arena base table, R14 = per-VM context) so
//     stencils do not need to reload them per op. CPython 3.13 reloads
//     equivalents per stencil.
//
//  3. The MEP-41 verifier (compiler3/verify) has already rejected any
//     program with a memory-safety violation by the time copypatch
//     sees the IR. The JIT does not re-check rule classes A-E; it
//     trusts the verifier the same way every other Mochi backend
//     does.
//
// # Phase 1 scope (this iteration)
//
// Phase 1 lands the package skeleton: stencil table format, runtime
// patcher, IR-walking emitter, code cache, and the W^X mmap manager
// for linux/amd64. The amd64 stencil table ships as a hand-written
// placeholder set covering OpConst, OpAddI64, OpReturnI64; the full
// stencil set extracted by stencilgen lands in Phase 1.1. The patcher
// machinery, the emit walker, and the cache are exercised by tests
// against synthetic stencils so the load-bearing logic is covered
// independently of real machine code.
//
// # Deferred sub-phases
//
//   - 1.1 Real Clang stencil extraction via tools/stencilgen (build-time).
//   - 1.2 Full stencil set covering all non-allocating vm3 ops (i64
//     arithmetic, comparison, conditional jump, register move, frame
//     load/store, typed-array element load/store).
//   - 1.3 Inline allocation stencils for short-lived Cells (small int,
//     short string, bool).
//   - 1.4 Slow-path call stubs (handle deref miss, arena exhaustion,
//     deopt sentinel).
//   - 1.5 aarch64 stencil set + Apple Silicon JIT entitlement.
//   - 1.6 BG kernel cross-tier parity gate (interpreter vs JIT
//     byte-for-byte equal output on hello-world, sum_loop, fib_iter).
//
// # Reading order
//
//  1. doc.go (this file): package overview.
//  2. stencil.go: Stencil, RelocKind, RelocSite, SymbolID types.
//  3. patch.go: applyRelocs and per-RelocKind patching.
//  4. emit.go: IR walker, per-op stencil lookup, output buffer.
//  5. cache.go: code cache + bump allocator.
//  6. mmap_linux_amd64.go: dual-mapping W^X manager.
//
// # Cross-references
//
//   - MEP-42 §2 (copy-and-patch JIT).
//   - MEP-40 (vm3 + compiler3 typed IR).
//   - MEP-41 §5 (W^X hardening; rule class C generation opacity).
//   - tools/stencilgen (build-time Clang driver).
package copypatch
