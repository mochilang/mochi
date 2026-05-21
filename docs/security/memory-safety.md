# Mochi memory safety

This page is the public memory-safety statement for Mochi. It is a *skeleton* during MEP-41 Phases 0 through 6: section headings are stable, prose is provisional, and measured numbers are placeholders. The final wording is published in Phase 7 alongside the blog post on mochi-lang.org and the `SECURITY.md` at the repo root.

Created: 2026-05-21 (GMT+7) as part of MEP-41 Phase 0 closeout.

Status: **draft skeleton**. Do not cite this document in external roadmap submissions until Phase 7 (week 22) replaces the placeholder wording. The threat model at `docs/security/threat-model.md` is normative and stable; this document depends on it.

## TL;DR

Mochi is designed to enable signatories of the CISA Secure-by-Design Pledge to use it as part of their memory-safety roadmap, equivalent to selecting any other named memory-safe language (Rust, Go, C#, Java, Swift, Python, JavaScript). The vm3 runtime delivers per-handle generation checks (no use-after-free), typed-arena allocation (no cross-type confusion), null-safe option types (no null dereference, MEP-16), and JIT-side W^X + PAC/BTI/CET hardening (no code injection). The runtime is implemented in Go, a CISA-named memory-safe language; the safety chain bottoms out at the Go runtime on the same footing as Python bottoms out at CPython.

[*Replace the TL;DR above with the final-wording draft once Phase 7 reviewers sign off. See MEP-41 §10.8.*]

## 1. The guarantees

Each row below is a runtime invariant. The right-hand column points at the mechanism in `runtime/vm3` that enforces it and at the verifier rule (MEP-41 §6.2) that makes the mechanism load-bearing.

| Property | Mechanism | Enforced by |
|----------|-----------|-------------|
| No use-after-free | Per-deref generation check (Vale-style) | `runtime/vm3/accessors.go` + rule class A |
| No cross-type confusion | 12-arena partition with arena-tag dispatch | `runtime/vm3/arenas.go` + rule class D |
| No null dereference | `Option<T>` with no force-unwrap operator | MEP-16 type-checker discipline |
| No out-of-bounds container access | Length-checked accessor + arena bounds | `runtime/vm3/accessors.go` + rule class D |
| No code injection in JIT | W^X + PAC/BTI/CET + Spectre v1 masking | `runtime/vm3jit` + MEP-41 §7 hardening |
| No FFI escape | Sealed handles at the FFI boundary | MEP-43 §10 + `runtime/mochi/ffi/seal.go` |
| Generation opacity (no TCE-style leak) | Verifier rule class C | `runtime/vm3/verify.go` (Phase 1) |

The structural property: these invariants hold by construction for any program that the verifier admits. A program that the verifier rejects does not execute. The verifier is the single point of policy (see `docs/security/threat-model.md`).

## 2. What Mochi does not claim

Mochi is not Rust. The following are explicitly *not* claimed by this document:

- **Compile-time aliasing-XOR-mutation.** Borrow modes (`borrow` / `consume` / `inout` / `weak`, MEP-41 §6.9) are *advisory*: violation is a runtime panic, not a compile-time error.
- **Data-race freedom.** Mochi is single-VM (MEP-40 §3). Concurrent Mochi is a separate MEP.
- **Mechanized formal proof.** Iris-MSWasm (OOPSLA 2024) proves robust capability safety for MSWasm in Coq. vm3 is structurally analogous, but the mechanization is deferred to MEP-60+; see MEP-41 §10.3.
- **Hardware CHERI / MIE deployment.** The handle Cell is structurally compatible with a CHERI fat pointer and structurally analogous to an MIE-tagged pointer, but Mochi runs on a software soft-capability machine today. Hardware deployment is deferred to a future MEP.

## 3. Provenance chain

The standard CISA-named memory-safe languages (Rust, Go, C#, Java, Swift, Python, JavaScript) all rest on a chain of trust. Mochi's chain is the same shape:

```
Mochi source ─► statically type-checked (MEP-4, MEP-5, MEP-6, MEP-16)
              ─► compiler3 emits verified bytecode (MEP-40 §7, MEP-41 §6.2)
              ─► runtime/vm3 executes via typed-arena handles with
                 per-deref generation check (MEP-40, MEP-41 §6)
              ─► runtime is Go (CISA-named memory-safe)
              ─► Go runtime is taken on the same footing as
                 JVM for Java or CPython for Python
```

Adopting Mochi inside a CISA Secure-by-Design Pledge organization is equivalent to selecting any other named memory-safe language: the language layer is safe by construction, and the runtime bottoms out at a CISA-named memory-safe host.

## 4. Threat model summary

The full enumeration is at `docs/security/threat-model.md`. The short version:

- **Boundary 0 (verifier).** The single point of memory-safety policy. Trusted code in `runtime/vm3/verify.go`.
- **Boundary 1 (source).** Parsed and type-checked. Cannot produce unsafe bytecode because the verifier re-checks every instruction.
- **Boundary 2 (bytecode).** Must pass the verifier before any opcode executes. Verifier rule classes A-E close handle-forgery, tag-swap, gen-leak, wrong-arena, and reference-mode-violation attacks.
- **Boundary 3 (external data).** Constructed through verifier-checked constructors only. External bytes never synthesize a Cell.
- **Boundary 4 (FFI).** Handles cross the FFI boundary sealed. `OpUnseal` gated on MEP-15 `meta` effect.
- **Boundary 5 (JIT input).** Lowered from verified bytecode only. Code page hardened per MEP-41 §7.

## 5. JIT hardening posture

[*Placeholder. Measured numbers land in Phase 5 (week 18) and are filled in here in Phase 6 (week 21).*]

The vm3jit code page is hardened against the standard JIT-side attacker classes:

- **W^X.** Never simultaneously writable and executable. darwin/arm64 uses `MAP_JIT` + `pthread_jit_write_protect_np`; linux/amd64 uses dual mapping via `memfd_create` + two `mmap` calls.
- **PAC + BTI on arm64.** Pointer Authentication on return addresses; Branch Target Identification on every indirect-branch target.
- **Intel CET on amd64.** Shadow stack on returns; Indirect Branch Tracking on indirect calls. Tiger Lake (2020) and later.
- **Spectre v1 index masking.** Every typed-array opcode lowering masks the index against the array length before bounds check.
- **Retpoline / SB on indirect branches.** Where CET IBT and BTI are unavailable, indirect branches are wrapped.
- **Guard pages around code page.** Off-by-one writes during emit fault immediately rather than corrupting neighbours.

Measured overhead [*Phase 6 placeholder*]: under 2% on the BG benchmark suite (target gate; MEP-41 Phase 5).

## 6. Performance model

[*Placeholder. Measured numbers land in MEP-41 Phase 1 (verifier overhead), Phase 3 (quarantine), Phase 4 (reference-mode elision), and Phase 5 (JIT hardening). Filled in here in Phase 6.*]

| Cost | Default | With reference modes (MEP-41 §6.9) |
|------|---------|------------------------------------|
| Generation check per deref | 2-3 ns on Apple M4 at 4.4 GHz | Elided inside `borrow` / `inout` scopes |
| Per-arena allocator path | one CAS + slab pointer write | unchanged |
| JIT-hardening total | < 2% on BG suite (target) | unchanged |
| Quarantine bookkeeping | O(N) per arena | unchanged |

Vale's published baseline for generational references is 2-10.84% on BenchmarkRL (`verdagon.dev/blog/generational-references`). vm3 inherits the same overhead curve. Reference-mode elision aligns with Vale's measured region-borrow speedup on iteration-heavy code.

## 7. The CISA Secure-by-Design Pledge

This document exists in its present form because the **CISA Secure-by-Design Pledge** memory-safety roadmap obligation has a January 1, 2026 deadline (`https://www.cisa.gov/securebydesign/pledge`). Mochi itself is not a pledge signer (Mochi is a language, not a vendor), but downstream organizations that adopt Mochi *are* signers. This page is a reference template those organizations can attach to their roadmaps.

Suggested text for downstream roadmaps:

> "We selected Mochi for [scope] because it is designed to align with the CISA Secure-by-Design Pledge memory-safety roadmap obligation. Mochi's vm3 runtime delivers per-handle generation checks (no use-after-free), typed-arena allocation (no cross-type confusion), null-safe option types, and JIT-side W^X + PAC/BTI/CET hardening. The runtime is implemented in Go, a CISA-named memory-safe language. See `docs/security/memory-safety.md` in the Mochi tree for the language's threat model and current memory-safety status."

[*Replace with the canonical paste-template once Phase 7 reviewers sign off.*]

## 8. Related policy frameworks

The pledge is not the only relevant framework. Downstream organizations citing this page may also want to point at:

- **NSA CSI "Software Memory Safety"** (November 2022, updated June 2025). Recommends C#, Go, Java, Ruby, Rust, Swift. Mochi's positioning piggy-backs on this list via the Go-runtime provenance chain.
- **ONCD "Back to the Building Blocks"** (February 2024). White House technical report on memory safety.
- **DoD SWFT (Software Fast Track)** (2025). Faster authorization for Secure-by-Design-aligned products on DoD networks.
- **EU Cyber Resilience Act** (reporting begins September 2026). Manufacturer obligations for in-scope products in the EU market.
- **Apple Memory Integrity Enforcement** (September 9, 2025). The hardware-assisted memory-safety target that informed MEP-41's generation-as-secret design (rule class C; analogous to Apple TCE).

## 9. Status by phase

| Phase | Window | Status | Artifact |
|-------|--------|--------|----------|
| Phase 0 | Week 1 | LANDED 2026-05-21 (GMT+7) | This skeleton + `docs/security/threat-model.md` |
| Phase 1 | Weeks 2-4 | Pending | `runtime/vm3/verify.go` rule classes A-D |
| Phase 2 | Weeks 5-6 | Pending | Rule class C (gen opacity) + opcode audit |
| Phase 3 | Weeks 7-10 | Pending | Quarantine, guard slabs, sealed handles |
| Phase 4 | Weeks 11-14 | Pending | Reference modes (consume/borrow/inout/weak) |
| Phase 5 | Weeks 15-18 | Pending | vm3jit hardening (W^X, PAC, BTI, CET, masking) |
| Phase 6 | Weeks 19-21 | Pending | Audit + 24h fuzz + measured numbers in §5, §6 |
| Phase 7 | Week 22 | Pending | Final wording + blog post + `SECURITY.md` |

Phase status updates land in this table in the same PR that closes each phase (MEP-spec-in-sync rule, MEP-41 §13).

## 10. How to extend this document

Every MEP that touches the runtime updates this page in the same PR as the code change. Concretely:

- A new memory-safety property added to §1 must point at a verifier rule (or document why one is not needed) and at the threat-model boundary where it lands.
- A new attack class addressed must extend §4 (and the corresponding section of `docs/security/threat-model.md`).
- A new performance number replaces the placeholder in §6 with the measurement and a link to the bench harness.
- The phase status table in §9 is updated when a phase lands.

If a runtime change does not fit any of the above, ask whether the change is correct. A runtime change that does not move any of these rows is either a pure performance change (no entry needed) or a memory-safety regression (which is rejected by review).

## 11. Cross-references

- MEP-41 (this MEP).
- `docs/security/threat-model.md` (the boundaries this document summarizes).
- MEP-40 (vm3 + compiler3 substrate).
- MEP-15 (effects), MEP-16 (null safety), MEP-43 (Go FFI).
- CISA Secure-by-Design Pledge: `https://www.cisa.gov/securebydesign/pledge`.
- NSA CSI: `https://www.nsa.gov/Press-Room/News-Highlights/Article/Article/3215760/`.
- ONCD memo: `https://www.whitehouse.gov/wp-content/uploads/2024/02/Final-ONCD-Technical-Report.pdf`.
- Apple MIE: `https://security.apple.com/blog/memory-integrity-enforcement/`.
- Vale generational references: `https://verdagon.dev/blog/generational-references`.
