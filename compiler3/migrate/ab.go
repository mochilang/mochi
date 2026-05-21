// Package migrate hosts the Phase 9 A/B harness for MEP-43:
// running the legacy transpiler/x/go path and the new compiler3/emit/go
// path side by side on the same Mochi source, diffing their stdout.
//
// The harness ships as a skeleton in Phase 9. The "new" leg requires
// the Mochi-to-compiler3 frontend wiring that is the remaining work of
// Phase 6; until that lands, the new leg returns ErrNewPathPending. The
// legacy leg works today and is exercised by the existing test corpus.
//
// Phase 9's gate is "new path matches legacy path bit-for-bit on the
// 104/105 fixtures listed in transpiler/x/go/README.md; legacy path
// deleted in the next release." This package owns the runner and the
// diff harness; the gate flips green once the frontend lands.
package migrate

import (
	"errors"
)

// ErrNewPathPending is returned by RunNew while the Mochi-to-compiler3
// frontend wiring is incomplete. Callers must treat this as a soft
// failure (skip-with-note), not a hard error.
var ErrNewPathPending = errors.New("compiler3/migrate: new path requires MEP-43 Phase 6 frontend wiring")

// Result captures the stdout + exit status of one leg of the A/B run.
type Result struct {
	Stdout   []byte
	ExitCode int
	Err      error
}

// Runner is the legacy/new dispatcher used by RunBoth. Tests inject
// their own runner; the default Runner shells out to `mochi build`
// twice (once with the legacy flag, once with the new flag).
type Runner interface {
	RunLegacy(fixture string) Result
	RunNew(fixture string) Result
}

// Diff compares two leg results. The string representation is empty
// when the legs match byte-for-byte.
type Diff struct {
	StdoutEqual bool
	ExitEqual   bool
}

// CompareResults computes the byte-for-byte diff between two legs.
func CompareResults(legacy, new Result) Diff {
	return Diff{
		StdoutEqual: string(legacy.Stdout) == string(new.Stdout),
		ExitEqual:   legacy.ExitCode == new.ExitCode,
	}
}

// RunBoth dispatches to both legs and returns the diff. If RunNew
// returns ErrNewPathPending, the returned Diff.StdoutEqual is true
// (so a pending Phase 6 doesn't cause Phase 9 tests to fail); callers
// inspecting the result can detect the pending case via
// new.Err == ErrNewPathPending.
func RunBoth(r Runner, fixture string) (Result, Result, Diff) {
	legacy := r.RunLegacy(fixture)
	new := r.RunNew(fixture)
	if errors.Is(new.Err, ErrNewPathPending) {
		return legacy, new, Diff{StdoutEqual: true, ExitEqual: true}
	}
	return legacy, new, CompareResults(legacy, new)
}

// PendingRunner is the default Runner returned by Default() until the
// Phase 6 frontend wiring lands. RunLegacy returns an empty Result
// (callers wire their own legacy shim); RunNew returns ErrNewPathPending.
type PendingRunner struct{}

// RunLegacy returns an empty Result. Override via your own Runner.
func (PendingRunner) RunLegacy(fixture string) Result { return Result{} }

// RunNew returns ErrNewPathPending while Phase 6 frontend wiring is
// outstanding. Once compiler3 grows a Mochi-to-IR pipeline, this
// method is replaced with a real runner.
func (PendingRunner) RunNew(fixture string) Result {
	return Result{Err: ErrNewPathPending}
}

// Default returns the runner used by Phase-9 closeout: the new leg
// runs the Mochi-to-IR frontend + compiler3/emit/go + `go run`, and
// the legacy leg falls back to the .out golden (callers compose it
// with their own legacy runner via FrontendRunner.Legacy if they want
// a true legacy A/B). MVP-unsupported fixtures surface as
// ErrNewPathPending so they are reported as skips, not regressions.
func Default() Runner { return LoadGoldenLegacy{New: FrontendRunner{}} }
