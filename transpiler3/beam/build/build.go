package build

// Target selects the output artifact produced by Driver.Build.
type Target int

const (
	// TargetEscript produces a single-file escript executable.
	// Requires erl on PATH at runtime. Cold-start ~50ms. Size ~2-10MB.
	TargetEscript Target = iota

	// TargetRelease produces a self-contained OTP release tarball
	// with ERTS bundled. No erl required at runtime. Cold-start ~300ms.
	// Size ~30-80MB. Supports hot reload and supervision.
	TargetRelease

	// TargetAtomVM produces a .avm bundle for embedded targets
	// (ESP32, STM32). Only Phases 1-5 are supported; pg, gun, and
	// crypto are not available on AtomVM.
	TargetAtomVM
)

// Driver is the top-level build orchestrator for the BEAM transpiler.
// Call Build to compile a Mochi source file to the target artifact.
type Driver struct {
	// CacheDir is the directory for .beam file caches keyed by
	// BLAKE3 hash of the source + compiler version.
	// Defaults to .mochi/beam-cache/ in the source file's directory.
	CacheDir string
}

// Build compiles the Mochi source at src, writes the output artifact
// to out, and returns any error. The pipeline is:
//
//  1. Parse + type-check (compiler3 frontend, shared with MEP-45).
//  2. Monomorphise + lower to aotir (transpiler3/c passes, reused).
//  3. beam/lower: aotir -> cerl.Module.
//  4. beam/emit: cerl.Module -> .beam files via compile:forms/2.
//  5. Pack as escript (TargetEscript) or release (TargetRelease).
//
// Phase 0 ships the stub. Each later phase implements the stages
// required by its gate test.
func (d *Driver) Build(src, out string, target Target) error {
	// Stub: implemented in Phase 1.
	return errNotImplemented("Driver.Build not yet implemented (Phase 1)")
}

type notImplementedError string

func errNotImplemented(msg string) error { return notImplementedError(msg) }
func (e notImplementedError) Error() string { return string(e) }
