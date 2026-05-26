// Package build is the top-level build driver for the BEAM transpiler.
//
// Design contract: MEP-46 §1 "Pipeline and IR reuse". See
// website/docs/mep/mep-0046.md.
//
// Public entry point:
//
//	func (d *Driver) Build(src, out string, target Target) error
//
// Target selects the output artifact:
//
//	TargetEscript  - single-file escript executable (default)
//	TargetRelease  - OTP release tarball (ERTS bundled)
//	TargetAtomVM   - .avm bundle for embedded targets (Phase 15)
//
// The driver glues the pipeline stages:
//
//  1. Parse + type-check (reuses compiler3 frontend).
//  2. Monomorphise + lower to aotir (reuses transpiler3/c passes).
//  3. beam/lower: aotir -> cerl.Module.
//  4. beam/emit: cerl.Module -> .beam files via compile:forms/2.
//  5. Pack: escript packing (Phase 1) or relx release (Phase 15).
//
// Caching: Driver.CacheDir (defaults to .mochi/beam-cache/) stores
// .beam files keyed by BLAKE3 hash of the source + compiler version.
// A cache hit skips steps 1-4 and goes straight to packing.
//
// OTP discovery: the driver finds the erl binary on $PATH or via
// the ERL env variable. It checks the OTP release >= 27 and fails
// fast with a clear error if the version is below the minimum.
package build
