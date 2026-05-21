# Mochi threat model

This document enumerates the trust boundaries in a running Mochi program and names what each boundary is protecting against. It is the operational ground-truth that the verifier rules in MEP-41 §6.2 mechanize and that the public memory-safety statement in MEP-41 §10.8 summarizes. The page is referenced from MEP-41 §6.1, and any rewording in MEP-41 should round-trip into this document in the same PR (MEP-spec-in-sync rule, MEP-41 §13).

Created: 2026-05-21 14:00 (GMT+7) as part of MEP-41 Phase 0 closeout.

## 1. Scope and out-of-scope

In scope:

- A single-VM Mochi process executing verified bytecode against the vm3 runtime defined in MEP-40.
- The vm3jit code page when JIT is enabled (MEP-40 Phase 5; full hardening lands in MEP-41 Phase 5).
- The FFI boundary into the Go runtime when an `import go "path"` resolves to a real Go symbol (MEP-43).
- Untrusted external data entering the VM through builtin parsers, `load`, file I/O, network I/O, or FFI return values.
- The verifier itself (`runtime/vm3/verify.go`, landing in MEP-41 Phase 1).

Out of scope:

- Concurrent / multi-actor Mochi. MEP-40 §3 specifies single-VM execution; multi-VM concurrency will need its own threat model (Pony-style reference capabilities or Verona-style regions); see MEP-41 §10.4.
- Hardware CHERI / Morello targets. The handle Cell is structurally compatible with a CHERI fat pointer, but the present model targets the soft-capability shape implemented in vm3 today; MEP-41 §10 defers a CHERI back end.
- Side-channel attacks beyond the Spectre v1 / v2 mitigations called out in MEP-41 §7. Timing-channel attacks that observe gen-check latency externally (e.g., a sibling process on a shared cache) are documented as a residual risk in MEP-41 §11.7.
- Supply-chain attacks on the Go toolchain or on Mochi's own build artifacts. Those are handled by the build pipeline (signed releases, reproducible builds) and not by language-level memory safety.

## 2. Trust boundaries

Five boundaries separate trusted from untrusted state. The verifier (boundary 0) is the single point of policy; every other boundary is enforced by code that the verifier itself authorizes.

```
                  ┌──────────────────────────────────────────┐
                  │                Boundary 0:                │
                  │   verifier (runtime/vm3/verify.go)        │
                  │   single point of memory-safety policy    │
                  └──────────────────────────────────────────┘
                                  ▲     ▲     ▲     ▲     ▲
                                  │     │     │     │     │
  Boundary 1  ┌── untrusted ──────┘     │     │     │     └────── untrusted ──┐  Boundary 5
              │ source program          │     │     │                          │ JIT input
              │ (parser + types)        │     │     │                          │ (lowered from
              └───────────────────────  │     │     │                          │  verified
                                        │     │     │                          │  bytecode)
                       Boundary 2 ──────┘     │     └──── Boundary 4           │
                       untrusted bytecode     │      untrusted FFI             │
                       (from disk, network)   │      (sealed handles only)     │
                                              │                                │
                       Boundary 3 ────────────┘                                │
                       untrusted external data                                 │
                       (parsers, I/O, FFI returns) ─────────────────────────────
```

Every arrow into boundary 0 is a place where untrusted input is normalized to a verifier-checked invariant before any vm3 opcode sees it. Every arrow out of boundary 0 is a place where the runtime relies on a verifier obligation; if that obligation is wrong, the safety chain breaks at that arrow.

### Boundary 1. Untrusted source program

**What is untrusted.** Mochi source text read by `parser` and `types`. The parser is responsible for tokenization and AST construction; the type checker is responsible for static typing under MEP-4, MEP-5, MEP-6, and MEP-16. A well-typed program is *syntactically well-formed Mochi*; it is not yet trusted for execution.

**Threat.** A crafted source program that exercises pathological cases in the parser or type checker. Examples: deeply nested generics that explode the unifier; cyclic imports that wedge the resolver; over-long identifiers that overflow a fixed buffer; UTF-8 sequences that desynchronize the lexer.

**Mitigation.** Parser and type-checker are written in Go and inherit Go's memory safety (no out-of-bounds reads, no UAF). Pathological inputs at most produce a slow compile or a typecheck error; they do not produce miscompiled bytecode, because the verifier (boundary 0) re-checks every emitted instruction independent of the path it came from. Even a buggy compiler cannot emit unsafe bytecode if the verifier rejects it.

**What still gets through.** Logic bugs in source: division by zero, integer overflow at i64 limits, out-of-bounds index. These are program-level bugs, not memory-safety bugs. vm3 traps deterministically (panic) rather than corrupting memory; the trap path is in trusted Go code.

### Boundary 2. Untrusted bytecode

**What is untrusted.** A `*runtime/vm3.Program` value loaded from disk, from a network stream, from a `vm3.Decode` call against arbitrary bytes, or from any compiler other than the in-tree compiler3. The verifier treats this exactly the same as compiler3-emitted bytecode: until verification passes, execution refuses to start.

**Threat.** A crafted bytecode stream that synthesizes a handle from raw bits (rule class A), aliases two arenas (rule class B), exposes a generation (rule class C), or dereferences a wrong-tag Cell (rule class D). Any of these would let user code escape the typed-arena partition and read or write outside its allocations.

**Mitigation.** Five verifier rules in MEP-41 §6.2:

- Class A (handle origin): every Cell produced by any opcode is either copied, alloc-constructed, or an inline value. `MakeHandle(tag, gen, idx)` is package-internal and unreachable from any compiler3-emitted opcode.
- Class B (tag stability): no opcode rewrites a Cell's arena tag in place; sealing produces a fresh Cell rather than flipping the tag.
- Class C (generation opacity): no opcode exposes the generation field; no arithmetic on Cell values; no `gen_of(handle)` operator.
- Class D (arena tag dispatch): every dereferencing opcode dispatches on the arena tag *before* touching the slab; wrong-tag dereferences trap before any index arithmetic.
- Class E (reference-mode discipline): the optional `consume`/`borrow`/`inout`/`weak` annotations carry verifier obligations that the elision pass relies on.

**What still gets through.** A bug *in the verifier itself* would propagate as miscompiled-bytecode acceptance. This is mitigated by (a) Phase 1 fuzz harness in MEP-41 (1M random opcode sequences without false-accept), (b) cross-compilation A/B: every program in the BG corpus runs through both the legacy compiler and compiler3+verifier, and (c) by keeping the verifier in scope for the OOPSLA-style Iris mechanization that MEP-41 §10.3 leaves open.

### Boundary 3. Untrusted external data

**What is untrusted.** Bytes that enter the VM after verification has completed: stdin, file contents, network responses, parsed JSON / CSV / YAML, builtin numeric parsing (`int.parse(s)`, `float.parse(s)`), and the result of FFI-returned `string` / `bytes`.

**Threat.** Data that exploits a parser bug to corrupt VM state. Classic example from C: a JSON parser that mis-counts nesting depth, smashes the stack, and writes a pointer at a controlled offset. In a memory-unsafe runtime that becomes arbitrary code execution.

**Mitigation.** Every parser that ingests untrusted bytes is implemented in Go and runs inside the trusted Go runtime. Parser output is *always* a fresh handle constructed by `runtime/vm3/alloc.go` (verifier rule class A). External data never synthesizes a Cell from raw bits; it can only request that the runtime allocate a Cell for it. Numeric parsing returns `Option<T>` (MEP-16); allocation failure is surfaced as `none`, not as a wild pointer.

**What still gets through.** A logic bug in a Go-side parser (e.g., an integer overflow in a header field's length calculation) can still produce a wrong-but-typed result. That is a program-level bug, not a memory-safety bug; the resulting handle is structurally valid (right arena, right gen, in-bounds index), and any wrong-content propagation is caught by the program's higher-level invariants, not by the VM.

### Boundary 4. Untrusted FFI

**What is untrusted.** Anything inside a Go function that the Mochi program reaches through `import go "path"`. Every imported symbol's body is outside the verifier's reach. The Go function can call into arbitrary other Go code, including unsafe pointer manipulation in unrelated packages.

**Threat.** A malicious or buggy FFI function dereferences a Mochi handle in ways the VM did not authorize. Examples: storing the handle, returning it after a GC cycle, casting it to a Go `uintptr`, dereferencing the arena slab directly by reflection.

**Mitigation.** MEP-41 §6.7 (and MEP-43 Phase 10) seal every Mochi handle that crosses the FFI boundary. Sealing wraps the handle in `ffi.Seal[T]` at the Go target; at the vm3 target it sets the arena tag to a reserved `arenaSealed` value. A sealed handle cannot be dereferenced by any opcode and cannot be reflected on by any Go API because its Go-side static type is a generic identity wrapper with no exported field. The receiving Go function can store, pass, and return the wrapped handle, but cannot read its contents. Only the matching `OpUnseal` (or `ffi.Unseal[T]`) call from Mochi code reopens the wrapper. `OpUnseal` is gated on MEP-15's `meta` effect, so the call site is statically visible.

**What still gets through.** A Go function that takes a non-handle Mochi value (e.g., an `i64` argument) sees it as an ordinary Go `int64`. There is nothing to seal at the value level. The Go function can mis-use the value in ways that propagate as wrong-result bugs back to Mochi; that is a program-level bug, not a memory-safety bug. A Go function that takes a sealed handle and *forgets* to return it produces a Mochi-side panic at the next `OpUnseal`; that is the desired failure mode (loud, deterministic) rather than silent memory corruption.

### Boundary 5. Untrusted JIT input

**What is untrusted.** The IR fed to the vm3jit lowering pass (MEP-40 Phase 5). The IR is itself derived from verified bytecode, so by construction it has already passed boundary 0. But the lowering pass introduces a second opportunity for a bug: an incorrect lowering rule that emits machine code that violates a verifier invariant the bytecode did not.

**Threat.** A bug in JIT lowering that synthesizes a wrong-tag handle, skips a gen check, or emits a load at a controlled offset past the arena's slab bound. Because the JIT code page is, by definition, *executable trusted code*, a corrupted lowering becomes a direct path to arbitrary code execution.

**Mitigation.** Two layers:

1. *Structural correctness.* The JIT lowering pass refuses any IR node not derived from a verifier-accepted opcode. Every JIT-emitted dereference inherits the same arena-tag dispatch and gen-check sequence as the interpreter (MEP-40 Phase 5 lowering). Lowering-pass tests cross-check IR -> machine-code outputs against the interpreter's outputs on the BG corpus.
2. *Code-page integrity.* MEP-41 §7 mandates W^X on the JIT code page (MAP_JIT + pthread_jit_write_protect_np on darwin/arm64, dual-mapping on linux/amd64), PAC + BTI on arm64, Intel CET SHSTK + IBT on amd64, Spectre v1 index masking on every typed-array opcode lowering, retpoline or BTI on indirect branches, and guard pages around the code page. A buggy lowering that emits write-then-execute is rejected at runtime by W^X; a JOP gadget chain that would corrupt a return address fails at the next `retaa` (PAC) or `ret` (shadow stack) check.

**What still gets through.** A perfectly-formed JIT-emitted instruction sequence that mis-implements the IR's intent (e.g., emits an addition instead of a subtraction). That is a correctness bug, not a memory-safety bug; differential testing against the interpreter catches it.

### Boundary 0. The verifier (single point of policy)

The verifier is *trusted code*. It is the single point at which the memory-safety policy is decided. Every other boundary's mitigation reduces to "the verifier enforces an invariant that closes this attack." If the verifier is buggy, the entire chain falls.

The verifier's threat model is therefore narrow:

- The verifier must not depend on runtime state. Every rule is a static check over the compiler3 IR or the emitted bytecode.
- The verifier must reject any opcode sequence that violates rule classes A through E (MEP-41 §6.2). False-accepts are memory-safety bugs.
- False-rejects are *not* memory-safety bugs; they are usability bugs and are tracked at the same severity as a type-checker bug.

The verifier ships with three load-bearing test surfaces:

1. *Positive corpus.* Every program in `tests/vm/valid/` and the BG kernels must pass the verifier. Any false-reject blocks the gate at Phase 1.
2. *Negative corpus.* A hand-built set of synthetic malformed-bytecode fixtures (one per rule class A-E) must be rejected. New rules are added by adding a fixture first and then changing the verifier until the fixture is rejected.
3. *Fuzz harness.* `go-fuzz` over the verifier with random opcode sequences. The gate at Phase 1 is 1M sequences with zero false-accepts (uncaught violations) and a documented rate of false-rejects (which is the legitimate output for malformed input).

## 3. Invariants that the boundaries jointly enforce

The five user-visible memory-safety guarantees of MEP-41 §6 reduce to the following invariants. Each invariant cites the boundary that enforces it.

| Invariant | Boundary | Enforced by |
|-----------|----------|-------------|
| No use-after-free | Boundary 0, rule class A + alloc.go gen-bump | Verifier rejects any handle synthesized from raw bits; alloc-constructor bumps `gen` on slot reuse; accessor traps on `curr != remembered`. |
| No cross-type confusion | Boundary 0, rule class B + arena dispatch | Tag is stable for lifetime of value; every dereferencing opcode dispatches on arena tag before touching slab. |
| No generation leak | Boundary 0, rule class C | Verifier refuses any opcode that exposes `gen` to user bytecode; JIT lowering refuses to spill `gen` to a named SSA slot. |
| No null dereference | MEP-16 `Option<T>` discipline | Force-unwrap is not an operator; `?.` and `??` flow through `try_deref`. |
| No out-of-bounds container access | Boundary 0, rule class D + accessor bounds checks | Arena dispatch precedes any index arithmetic; accessors length-check the container's payload before load. |
| No code injection in JIT | Boundary 5 + MEP-41 §7 hardening | W^X discipline on code page; PAC/BTI/CET; index masking; retpoline/BTI on indirect branches; guard pages. |
| No FFI escape | Boundary 4 + MEP-41 §6.7 sealing | All handles crossing FFI are sealed; `OpUnseal` gated on MEP-15 `meta` effect; receiving Go function cannot reflect on sealed wrapper. |

If every boundary correctly enforces its row, the seven invariants hold jointly. The verifier (boundary 0) is the load-bearing artifact: it appears in four of the seven rows.

## 4. Adversary model

The threats above presume the following adversary capabilities.

**Local untrusted user.** Can execute arbitrary verified Mochi bytecode in the same process. Cannot modify the binary, the verifier, the Go runtime, or the JIT code page. This is the standard "execute a script in a sandbox" shape.

**Network untrusted client.** Can send arbitrary bytes that the program parses and stores. Cannot execute bytecode directly; can at most cause the host program to evaluate strings through `string.parse` or `json.parse`. The boundaries reduce to "any path from network bytes to a Cell must go through a verifier-checked constructor." Network adversary cannot reach the JIT code page except through whatever the host program voluntarily exposes.

**Compromised dependency.** A Go package that the host pulls in transitively. Boundary 4 (untrusted FFI) handles this case by default: every handle reaching the dependency is sealed. A dependency that calls only into the Go stdlib (the common case for stdlib FFI bridges like `compiler3/ffi/typebridge/stdlib`) reduces to "trust the Go stdlib," which is the same footing as Java trusting the JVM stdlib.

**Compromised toolchain.** Out of scope (build-pipeline concern). MEP-41's memory-safety claim is conditional on the host actually running the verifier; if a compromised toolchain ships a binary that skips the verifier, no language-level invariant can save it. Production builds enable the verifier as a non-optional pass and reject any bytecode that did not pass it.

**Side-channel observer.** A process with cache-line observability on the same physical core. MEP-41 §7.4 (Spectre v1 masking) closes the in-process speculative-OOB read channel. The out-of-process gen-churn timing channel that MEP-41 §11.7 names is a documented residual risk; mitigation is deferred to a future MEP that ports vm3 to MIE-class hardware.

## 5. Failure modes (loud and silent)

A memory-safety system is judged by what it does when its assumptions are wrong. Mochi's design prefers loud failure.

**Loud failures (preferred).** Each is a `panic` or `runtime.Throw` with a non-recoverable error code. The runtime is single-VM, so a panic in trusted code is a process-exit, not a thread-local catch.

- Stale handle dereference (gen mismatch): `vm3: stale handle (arena=X gen=Y curr=Z)`.
- Wrong-arena dereference (rule class D miss): `vm3: handle arena mismatch (got=X want=Y)`.
- Sealed handle dereference (rule class B miss after seal): `vm3: cannot deref sealed handle`.
- Out-of-bounds container access (accessor length check): `vm3: index out of range`.
- W^X violation on JIT code page (Phase 5 enforcement): `vm3jit: write to executable page`.
- PAC mismatch / shadow-stack mismatch / IBT trap: signaled by hardware via `SIGSEGV` / `SIGILL` with a `si_code` that the runtime translates into a fatal-error message.

**Silent failures (residual).** Documented; not addressed by MEP-41.

- Side-channel observation of gen value through out-of-process cache timing (MEP-41 §11.7).
- A perfectly-formed program with a logic bug whose output is wrong but whose runtime invariants all hold. Out of scope: this is a correctness concern, not a memory-safety concern.
- A bug in the Go runtime itself (the safety chain bottoms out at "Go runtime is correct," exactly as Python's bottoms out at "CPython is correct").

## 6. How to extend this model

When a new MEP adds a runtime feature, the corresponding boundary's "what still gets through" must be revisited. Concretely:

- **MEP that adds concurrency** -> Boundary 4 expands to include race-window attacks; new boundary 6 for cross-actor message-passing.
- **MEP that adds a CHERI back end** -> Boundary 5 changes shape (CHERI hardware enforces what verifier rule class A does today); document the shift.
- **MEP that adds capability-passing effects** -> Boundary 0's rule class E grows new obligations; document them here.
- **MEP that adds a new builtin parser for an external format** -> Boundary 3's "what still gets through" must be re-checked for the format's edge cases.

The standing rule is: every new MEP touching the runtime updates this document in the same PR as the code change (MEP-41 §13 spec-in-sync discipline). A code change to the runtime without a corresponding line in this table is rejected by review.

## 7. Cross-references

- MEP-41 §6.1 (informal threat-model summary), §6.2 (verifier rules), §6.7 (sealed handles), §7 (JIT hardening), §11 (residual risks).
- MEP-40 §3 (single-VM execution), §6.2 (typed arenas), §6.3 (handle Cell layout), §9.2 (free-list reuse and quarantine).
- MEP-43 §10 (FFI seal/unseal landing at the Go target).
- MEP-15 (`meta` effect gating).
- MEP-16 (`Option<T>` and no-force-unwrap discipline).
- `runtime/vm3/cell.go`, `runtime/vm3/accessors.go`, `runtime/vm3/alloc.go` (current handle, accessor, allocator implementations).
- `runtime/vm3/verify.go` (Phase 1 deliverable; lands later in MEP-41).
- `runtime/mochi/ffi/seal.go` (Go-target sealing markers; landed in MEP-43 Phase 10).
- `docs/security/memory-safety.md` (companion public statement).
