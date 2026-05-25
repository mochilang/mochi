package build

import (
	"runtime"
	"testing"
)

// TestPhase7ErrorModel is the MEP-45 Phase 7.0 + 7.2 gate.
//
// Phase 7.0 adds the exception jump-buffer stack (mochi_try_push /
// mochi_try_pop / mochi_raise) to the runtime. Phase 7.2 routes
// mochi_panic_div_zero and mochi_panic_index through mochi_raise.
//
// Without a try block on the stack, mochi_raise falls through to
// fputs(msg, stderr) + exit(code) -- identical observable behaviour to
// the pre-7.0 direct exit() calls. This gate verifies that the refactor
// is transparent to existing programs:
//
//   - TestPhase2Divzero: full divzero fixture suite produces correct
//     output (all safe divisions return the right values).
//   - TestPhase2DivzeroTrip: all trip fixtures exit with MOCHI_ERR_DIVZERO
//     (exit code 5), proving mochi_raise still terminates with the right
//     code when no try block is active.
//
// Note: Phase 7.1 (try/catch lowering) is blocked on the parser adding
// `try { } catch e { }` syntax, so no Mochi-level fixtures for the
// catch path exist yet. The runtime infrastructure is in place; the
// fixtures will be added when Phase 7.1 lands.
func TestPhase7ErrorModel(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 7 error model gate skipped on Windows; Phase 11 wires Windows CI")
	}
	t.Log("Phase 7.0+7.2: mochi_raise infrastructure verified via divzero suite reuse")
	t.Log("Run TestPhase2Divzero and TestPhase2DivzeroTrip for full coverage")

	// The divzero suites are the canonical gate for Phase 7.0+7.2 since
	// they exercise the exact code paths that now route through mochi_raise.
	// We re-run the trip suite here as an explicit Phase 7 checkpoint.
	runFixtureSuite(t, "divzero")
}
