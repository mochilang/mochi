# Security policy

## Memory safety

Mochi is a memory-safe language. The full statement is at
[`docs/security/memory-safety.md`](docs/security/memory-safety.md).

Short version: the vm3 runtime enforces per-handle generation checks
(no use-after-free), typed-arena allocation (no cross-type confusion),
null-safe option types (no null dereference, MEP-16), generation
opacity (no TCE-style leak), and JIT-side W^X hardening on the code
page. The verifier in `compiler3/verify` is the single point of
memory-safety policy; programs the verifier rejects do not execute.
The runtime is implemented in Go, a CISA-named memory-safe language,
so the safety chain bottoms out at a memory-safe host.

Mochi is designed to enable signatories of the
[CISA Secure-by-Design Pledge](https://www.cisa.gov/securebydesign/pledge)
to cite it on their memory-safety roadmap, equivalent to selecting any
other named memory-safe language (Rust, Go, C#, Java, Swift, Python,
JavaScript).

### Citation

Downstream organizations may cite Mochi using the canonical paste
template in
[`docs/security/memory-safety.md` §7](docs/security/memory-safety.md).
A shorter footnote-shape variant is provided in the same section.

### Threat model

The normative threat model is at
[`docs/security/threat-model.md`](docs/security/threat-model.md).
It enumerates six trust boundaries (B0 verifier, B1 source, B2
bytecode, B3 external data, B4 FFI, B5 JIT input) and the verifier
rule classes (A handle origin, B tag stability, C generation opacity,
D arena-tag dispatch, E reference modes) that close each boundary.

The per-file internal audit at
[`docs/security/internal-audit.md`](docs/security/internal-audit.md)
walks every memory-safety-relevant source file in `runtime/vm3` and
`compiler3/verify`, mapping each to its boundary, rule class,
invariant, and any audit-flagged gap.

## Reporting a vulnerability

If you believe you have found a memory-safety regression or other
security issue in Mochi, please open a GitHub security advisory at
https://github.com/mochilang/mochi/security/advisories/new rather
than filing a public issue. Include:

- The Mochi version (commit SHA from `mochi --version` or the
  release tag).
- A minimal reproduction (Mochi source plus the expected vs.
  observed behavior).
- The trust boundary you believe is breached (see §threat-model.md
  for the six boundaries).

If the issue is a Go-runtime, kernel, or hardware-level concern
rather than a Mochi-specific one, please also report upstream
through the Go security team's process at
https://go.dev/security.

## Supported versions

The security statement covers the most recent tagged release plus
`main`. Pre-release branches and feature branches are out of scope.

## Related documents

- [`docs/security/memory-safety.md`](docs/security/memory-safety.md):
  the public memory-safety statement.
- [`docs/security/threat-model.md`](docs/security/threat-model.md):
  the normative threat model.
- [`docs/security/internal-audit.md`](docs/security/internal-audit.md):
  per-file audit of memory-safety-relevant source files.
- [`docs/security/jit-hardening.md`](docs/security/jit-hardening.md):
  vm3jit per-axis hardening posture.
- [`docs/security/gen-opacity-audit.md`](docs/security/gen-opacity-audit.md):
  rule class C (generation opacity) audit.
- [`docs/security/quarantine-design.md`](docs/security/quarantine-design.md):
  Phase 3 quarantine and sealing design.
- [`website/docs/mep/mep-0041.md`](website/docs/mep/mep-0041.md):
  the MEP-41 spec, which is normative for the verifier rule classes
  and the runtime invariants.
