package migrate

import (
	"bytes"
	"os"
	"os/exec"
	"strings"

	gobuild "mochi/compiler3/build/go"
)

// FrontendRunner is the Phase-9 runner whose RunNew drives the
// Mochi-to-IR frontend, the compiler3/emit/go emitter, and `go run`
// end-to-end. Fixtures the MVP frontend cannot lower yet surface as
// ErrNewPathPending so the A/B harness treats them as skipped rather
// than mismatched.
//
// This is the runner that flips Phase 9 from PARTIAL to LANDED: the
// soft-pass returned by PendingRunner.RunNew is replaced with an
// actual end-to-end execution result.
type FrontendRunner struct {
	// Legacy, if non-nil, supplies the legacy leg. When nil RunLegacy
	// returns an empty Result so callers can compose this runner with
	// LoadGoldenLegacy via the outer Runner.
	Legacy Runner
}

// RunLegacy delegates to the embedded Legacy runner. When unset, an
// empty Result is returned so the caller can compose this runner with
// LoadGoldenLegacy (which supplies the golden leg).
func (f FrontendRunner) RunLegacy(fixture string) Result {
	if f.Legacy != nil {
		return f.Legacy.RunLegacy(fixture)
	}
	return Result{}
}

// RunNew lowers the Mochi source at `fixture`, emits Go, and runs it.
// Returns ErrNewPathPending if the source uses a surface the MVP
// frontend doesn't yet cover, so the A/B harness records a skip rather
// than a regression.
func (f FrontendRunner) RunNew(fixture string) Result {
	tmp, err := os.MkdirTemp("", "mochi-frontend-*")
	if err != nil {
		return Result{Err: err}
	}
	defer os.RemoveAll(tmp)

	r, err := gobuild.BuildSource(fixture, gobuild.Options{
		Mode:   gobuild.ModeExecutable,
		OutDir: tmp,
	})
	if err != nil {
		// Surface MVP-unsupported fixtures as pending so the gate
		// treats them as skips, not regressions. The frontend uses
		// "unsupported in MVP" as the invariant marker for surfaces it
		// has not yet widened to.
		if strings.Contains(err.Error(), "unsupported in MVP") {
			return Result{Err: ErrNewPathPending}
		}
		return Result{Err: err}
	}
	cmd := exec.Command("go", "run", r.EntryPoint)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	runErr := cmd.Run()
	res := Result{Stdout: stdout.Bytes()}
	if runErr != nil {
		if ee, ok := runErr.(*exec.ExitError); ok {
			res.ExitCode = ee.ExitCode()
			res.Err = ee
		} else {
			res.Err = runErr
		}
	}
	return res
}
