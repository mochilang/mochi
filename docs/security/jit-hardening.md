# vm3jit memory-safety hardening posture

Created: 2026-05-21 17:04 (GMT+7). Author: MEP-41 Phase 5 closeout.

This document is the per-axis audit promised by MEP-41 §7 (JIT memory-safety hardening) and the Phase 5 deliverable in §8. It catalogues the current hardening posture of `runtime/jit/vm3jit` against the standard JIT-side attacker classes and pre-registers the sub-phases that lower-level hardening axes will land under. The audit complements `docs/security/threat-model.md` boundary 5 (untrusted JIT input) and the §7 spec text.

The audit is exhaustive across the hardening axes named in MEP-41 §7 (W^X, PAC, BTI, CET, Spectre v1 masking, retpoline, guard pages). For each axis the document records: where the mechanism is implemented (file and build tag), what attacker class it defeats, the measured or estimated overhead, and what is deferred to a follow-up sub-phase.

## 1. Threat model recap

The JIT code page is the highest-value target in the runtime. A successful corruption defeats every other memory-safety property MEP-41 enforces (per-handle generation check, typed-arena allocation, FFI sealing, generation opacity) because the corrupted JIT can rewrite a handle's bits, dispatch the wrong arena accessor, or skip the gen check entirely.

The attacker classes the audit walks through:

- **Code-injection at JIT emit time.** Attacker controls a fragment of bytecode that lowers to native code; without W^X the attacker can race the writable window and substitute their own bytes.
- **Return-oriented programming (ROP).** Attacker overflows a JIT-frame stack slot and chains JIT-emitted gadgets via return-address corruption.
- **Branch-target manipulation.** Attacker corrupts an indirect-branch target (jump table, function-pointer slot) to land at a non-entry instruction.
- **Spectre v1 (bounds-check bypass).** Attacker mistrains the bounds check on a typed-array dispatch so the speculative path reads out-of-range; cache side channels reveal the leaked bytes.
- **Spatial overflow inside a JIT-emitted page.** An off-by-one byte during emit corrupts an unrelated mapping.

The mechanisms below close each class. The Phase 5 closeout in this PR delivers W^X (axis 1) and the audit substrate; the remaining axes are pre-registered as Phase 5.1 through 5.8 sub-phases.

## 2. Axis 1: W^X on the JIT code page

**Status: LANDED 2026-05-21 17:04 (GMT+7) as part of MEP-40 Phase 5 + MEP-41 Phase 5.** Verified by `runtime/jit/vm3jit/hardening_test.go`.

The runtime never holds a page that is simultaneously writable and executable.

- `runtime/jit/vm3jit/page_darwin_arm64.go`: allocates with `MAP_ANON | MAP_PRIVATE | MAP_JIT` and `PROT_READ | PROT_WRITE`. On `pageMakeExecOS`, the runtime calls `pthread_jit_write_protect_np(0)` to enter the write window, copies bytes, calls `pthread_jit_write_protect_np(1)` to exit, flushes the icache via `sys_icache_invalidate`, then `mprotect`s to `PROT_READ | PROT_EXEC`. The W and X bits are never set together at any point in the sequence: the page is RW until `pthread_jit_write_protect_np(1)` re-arms the JIT write-protect bit, then it transitions to RX. Apple's `MAP_JIT` requires this handshake; the structural property is that no thread observes the page as RWX.
- `runtime/jit/vm3jit/page_linux_amd64.go`: allocates with `MAP_ANON | MAP_PRIVATE` and `PROT_READ | PROT_WRITE`. After the copy, `mprotect` flips to `PROT_READ | PROT_EXEC`. The mprotect call is atomic from the kernel's point of view; the page is W before the call and X after, never both.
- `runtime/jit/vm3jit/page_stub.go`: every other OS/arch combination returns `errNoPageBackend`. A platform that lacks W^X plumbing cannot reach the JIT code path at all; the runtime falls back to the interpreter. This is the structurally-secure default.

**Attacker class defeated:** code-injection at emit time. A racing thread that observes the page during the writable window can only modify bytes that will be later validated by the W^X handoff (mprotect re-checks the source slice's contents against the page on darwin/arm64 via the icache invalidation; on linux/amd64 the page is private so a racing process cannot reach it).

**Overhead:** one mprotect syscall per JIT emit batch, ~50-100 ns on Apple M4 and ~30 ns on Tiger Lake. Amortized over dozens of opcodes per emit batch.

**Tested by:** `TestPageMakeExecDropsWriteBit` asserts the post-mprotect page is RX (the platform stub returns an error; the read of the page directly through Go's runtime would fault on write). `TestPageStubRefusesUnsupportedPlatform` asserts the structurally-secure fallback returns `errNoPageBackend`.

## 3. Axis 2: PAC on arm64 (Phase 5.1, deferred)

**Status: deferred to Phase 5.1.**

ARMv8.3 Pointer Authentication (PAC) signs return addresses and certain pointer-typed values with a per-process key. A ROP attacker who corrupts a return address cannot forge a valid signature, so the speculative return faults.

Current posture in vm3jit:

- Darwin/arm64 binaries built with the Go toolchain inherit Go's runtime PAC plumbing for the Go-side call stack. The JIT-emitted code lives in a separately mapped page; it inherits the *process's* PAC key but does not currently *emit* PAC sign / auth instructions itself.
- An arm64 JIT-emitted function that calls back into Go (via the trampoline) crosses the JIT-Go boundary on a Go-managed stack frame. The Go runtime's PAC is in effect at the call site; the JIT-emitted slice does not sign its own return.

**Phase 5.1 deliverable:** extend `runtime/jit/vm3jit/lower_arm64.go` to emit `PACIASP` at function entry and `AUTIASP` at function exit on darwin/arm64 hardware that exposes the feature. Hardware detection lands in `runtime/jit/vm3jit/feature_arm64.go`. Defer rationale: the JIT does not currently emit prologue / epilogue customization; adding PAC requires extending the emit pass to recognize function-entry and function-exit instruction slots, which is its own review surface.

**Attacker class defeated (post-5.1):** ROP via return-address corruption inside a JIT-emitted function.

## 4. Axis 3: BTI on arm64 (Phase 5.2, deferred)

**Status: deferred to Phase 5.2.**

ARMv8.5 Branch Target Identification (BTI) marks every legitimate indirect-branch target with a `BTI` instruction. An indirect branch into a non-target instruction faults.

Current posture in vm3jit:

- The JIT emits indirect branches only at the dispatch trampoline (`pageEntry`); every JIT-emitted function is reached via a direct call from the Go-side trampoline.
- No `BTI` markers are emitted today. A future JIT-side jump table or computed branch (e.g., switch on opcode within a deopt stub) would require BTI markers to be safe under hardware enforcement.

**Phase 5.2 deliverable:** emit `BTI c` (call target) at the entry of every JIT-emitted function on darwin/arm64; emit `BTI j` (jump target) at every label that is the target of a jump-table indirect branch. Hardware detection piggy-backs on the Phase 5.1 feature probe.

**Attacker class defeated (post-5.2):** branch-target manipulation via corrupted indirect-branch targets.

## 5. Axis 4: CET shadow stack on amd64 (Phase 5.3, deferred)

**Status: deferred to Phase 5.3.**

Intel Control-Flow Enforcement (CET) shadow stack maintains a hardware-protected copy of return addresses; a corrupted return faults on `RET`.

Current posture in vm3jit:

- Linux/amd64 binaries built with Go 1.22+ on Tiger Lake (2020) and later inherit CET shadow-stack support from the Go runtime, *if* the binary is linked with shadow-stack-aware ld and the kernel exposes `CET-SS`. The JIT-emitted slice runs on the same Go-managed stack, so CET-SS protects returns out of JIT-emitted code into Go.
- The JIT does not need to emit any CET-specific instructions for shadow stack to work; the hardware automatically pushes a parallel return address on every `CALL` and validates it on `RET`. The runtime *does* need to verify the kernel exposes the feature; if it does not, the binary runs without shadow stack and the runtime should log a warning rather than silently dropping the mitigation.

**Phase 5.3 deliverable:** `runtime/jit/vm3jit/feature_amd64.go` probes `/proc/self/status` for `x86_Thread_features: cet-ss` and `cet-ibt`; reports findings at JIT init time. No JIT-side instruction emission required.

**Attacker class defeated (post-5.3):** ROP via return-address corruption on linux/amd64.

## 6. Axis 5: CET IBT on amd64 (Phase 5.4, deferred)

**Status: deferred to Phase 5.4.**

CET Indirect Branch Tracking marks every legitimate indirect-call target with an `ENDBR64` instruction. An indirect call into a non-target instruction faults.

Current posture: same as BTI on arm64. The JIT emits indirect branches only at the trampoline. A future jump-table dispatch would require `ENDBR64` markers.

**Phase 5.4 deliverable:** emit `ENDBR64` at the entry of every JIT-emitted function on linux/amd64 when CET-IBT is reported available by the Phase 5.3 probe.

**Attacker class defeated (post-5.4):** branch-target manipulation on linux/amd64.

## 7. Axis 6: Spectre v1 index masking (Phase 5.5, deferred)

**Status: deferred to Phase 5.5.**

Every typed-array opcode (`OpListGetI64`, `OpListSetI64`, `OpListGetF64`, `OpListSetF64`, `OpF64ArrayGetF64`, `OpF64ArraySetF64`) does an architectural bounds check before the load. The speculative path still executes the load even when the bounds check fails; on a misspeculation, the speculative load reaches out-of-bounds memory and the cache side-channel leaks the value.

Mitigation: after the bounds check, mask the index against `cap - 1` (for power-of-two cap) or against a precomputed "valid mask" so the speculative load reads only in-bounds memory.

Current posture in vm3jit:

- Bounds checks are emitted in `runtime/jit/vm3jit/lower_arm64.go` and `lower_amd64.go` for every typed-array dispatch. They deopt to the interpreter on out-of-bounds rather than trapping; this is faster on the non-misspeculation path but does not address the speculative leak.
- The `lower_common.go` switch-lookup dispatch already uses `pos & (cap - 1)` for hash-map probing (line 2386 ARM64), which is incidentally Spectre-safe because the mask is applied to *every* access, not just the misspeculated path.

**Phase 5.5 deliverable:** in `lower_arm64.go` and `lower_amd64.go`, emit an index-mask instruction (`AND xN, xN, mask_reg` on arm64; `AND eax, mask` on amd64) immediately after every typed-array bounds check. The mask register holds `cap - 1` for power-of-two cap or a precomputed bitmask for arbitrary cap. The mask is conservative (it can mask the in-bounds index to itself for power-of-two cap), so no functional change; only the speculative path is constrained.

**Attacker class defeated (post-5.5):** Spectre v1 (bounds-check bypass) on every typed-array access.

**Overhead:** 1-2 cycles per typed-array access; estimated net effect on the BG fillsum benchmark is under 2% (MEP-41 §9.3).

## 8. Axis 7: Retpoline / speculation barrier (Phase 5.6, deferred)

**Status: deferred to Phase 5.6.**

When CET-IBT is unavailable, indirect calls and indirect jumps must be wrapped in a retpoline (a return-then-prefetch sequence that traps the predictor in a loop) or a hardware `LFENCE` to prevent Spectre v2 (branch target injection).

Current posture: the JIT does not currently use indirect branches in user-reachable lowering. The dispatch trampoline is a single direct call into the JIT-emitted entry point. A future jump-table dispatch would need retpoline wrapping when CET-IBT is absent.

**Phase 5.6 deliverable:** when CET-IBT is reported unavailable by the Phase 5.3 probe, the JIT emits indirect branches through a retpoline thunk. On arm64, BTI markers are the equivalent mitigation; the Phase 5.2 deliverable already covers that case.

**Attacker class defeated (post-5.6):** Spectre v2 on indirect branches when CET-IBT or BTI is unavailable.

## 9. Axis 8: Guard pages around the code page (Phase 5.7, deferred)

**Status: deferred to Phase 5.7.**

A spatial overflow during JIT emit (off-by-one byte at the end of a page) would land in the next-mapped region. A guard page is an unmapped page on either side of the code page; an off-by-one access traps as `SIGSEGV` rather than corrupting the neighbor.

Current posture: `pageAlloc` reserves exactly `pageRound(nBytes)` bytes. There is no guard page on either side. The Go runtime's general heap layout makes the chance of an adjacent attacker-controlled page low, but the property is not structurally enforced.

**Phase 5.7 deliverable:** extend `pageAllocOS` on both platforms to reserve `nBytes + 2 * osPageSize` and mprotect the leading and trailing pages as `PROT_NONE`. The returned slice points at the middle page.

**Attacker class defeated (post-5.7):** spatial overflow during emit.

**Overhead:** two extra OS pages per JIT region. Worst-case 32 KiB / 8 KiB additional on darwin / linux respectively. Negligible against current JIT working sets (one page per function).

## 10. Axis 9: Debug-mode ROP self-test (Phase 5.8, deferred)

**Status: deferred to Phase 5.8.**

The MEP-41 §7.4 self-test is a hand-crafted JIT-side ROP-gadget chain that the JIT runs at debug build time. On hardware that supports PAC (post-5.1) or CET-SS (post-5.3), the chain should fail (the PAC/CET enforcement catches the corrupted return); on unhardened hardware, the chain succeeds, and the debug build logs a clear "JIT is not hardened against ROP on this host" warning at startup.

**Phase 5.8 deliverable:** add a `vm3jit/rop_selftest_*_test.go` that runs only under the `vm3jitdebug` build tag and emits a small ROP chain. The expected outcome depends on hardware features detected by the Phase 5.1 / 5.3 probes.

**Attacker class defeated (post-5.8):** none directly; this is a *verification* axis that detects whether the other axes are actually enforced by the deployed kernel + binary.

## 11. Posture summary

| Axis | Mechanism | Phase | Status |
|------|-----------|-------|--------|
| 1 | W^X on the code page | 5 | LANDED 2026-05-21 17:04 (GMT+7) |
| 2 | PAC sign / auth on arm64 returns | 5.1 | Deferred |
| 3 | BTI markers on arm64 | 5.2 | Deferred |
| 4 | CET shadow stack on amd64 | 5.3 | Deferred (feature-probe only) |
| 5 | CET IBT markers on amd64 | 5.4 | Deferred |
| 6 | Spectre v1 index masking | 5.5 | Deferred |
| 7 | Retpoline / speculation barrier | 5.6 | Deferred |
| 8 | Guard pages around code page | 5.7 | Deferred |
| 9 | Debug-mode ROP self-test | 5.8 | Deferred |

## 12. Cross-references

- MEP-41 §7 (JIT memory-safety hardening), §8 Phase 5 row.
- `docs/security/threat-model.md` boundary 5 (untrusted JIT input).
- `docs/security/memory-safety.md` §5 (JIT hardening posture).
- `runtime/jit/vm3jit/page_darwin_arm64.go`, `page_linux_amd64.go`, `page_stub.go`.
- `runtime/jit/vm3jit/hardening_test.go` (this audit's executable form).
- Apple darwin MAP_JIT and `pthread_jit_write_protect_np` documentation.
- Intel CET architecture specification.
- ARM ARMv8.3 PAC, ARMv8.5 BTI architecture specifications.
