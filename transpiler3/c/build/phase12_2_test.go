package build

import (
	"os"
	"os/exec"
	"testing"
)

// TestPhase12WasmStreams is the MEP-45 Phase 12.2 gate. It verifies that
// chan<T>, stream<T>, and agent fixtures compile and run correctly under
// wasm32-wasi after the Phase 12.2 sched.c WASM stubs land.
//
// The narrowed surface: chan/stream blocking paths (full send, empty recv)
// abort rather than yield, since there is no peer fibre to schedule under
// the single-fibre synchronous WASM stub. All Phase 9.1-9.3 fixtures are
// designed to use buffered channels and pre-emitted streams, so they
// exercise only the non-blocking paths and run correctly.
//
// shutdown_sched is excluded because it uses extern fun run_scheduler()
// (a neighbour .c FFI call); FFI neighbour files are compiled without the
// wasm32-wasi target and are excluded from the WASM corpus in general.
//
// Gated on MOCHI_TEST_ZIG_DOWNLOAD=1; silently skipped if wasmtime is not
// on PATH (CI installs wasmtime; dev hosts may run compile-only).
func TestPhase12WasmStreams(t *testing.T) {
	if os.Getenv("MOCHI_TEST_ZIG_DOWNLOAD") != "1" {
		t.Skip("set MOCHI_TEST_ZIG_DOWNLOAD=1 to enable WASM streams gate")
	}

	wasmtime, wasmtimeErr := exec.LookPath("wasmtime")
	if wasmtimeErr != nil {
		t.Skip("wasmtime not found on PATH; set up wasmtime to run WASM gate")
	}

	// Chan, stream, and agent suites run without blocking under the WASM stub.
	for _, suite := range []string{"chan", "stream", "agent"} {
		suite := suite
		t.Run(suite, func(t *testing.T) {
			runFixtureSuiteWasm(t, suite, wasmtime)
		})
	}

	// Shutdown suite: exclude shutdown_sched (uses extern fun run_scheduler() FFI).
	// The other four fixtures (shutdown_basic, shutdown_agent, shutdown_chan,
	// shutdown_stream) don't use FFI and work correctly under the WASM stub.
	t.Run("shutdown", func(t *testing.T) {
		runFixtureSuiteWasmExclude(t, "shutdown", wasmtime, []string{"shutdown_sched"})
	})
}
