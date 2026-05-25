package build

import (
	"runtime"
	"testing"
)

// TestPhase9Scheduler is the MEP-45 Phase 9.0 gate for the cooperative
// fiber scheduler foundation. It walks every fixture under
// tests/transpiler3/c/fixtures/scheduler and runs the same end-to-end
// pipeline as all other phase gates.
//
// Phase 9.0 adds:
//   - transpiler3/c/runtime/include/mochi/sched.h: public scheduler API
//     (mochi_fiber_create/destroy/resume/yield/current, mochi_sched_spawn/run).
//   - transpiler3/c/runtime/src/sched.c: ucontext_t implementation of the
//     above, providing minicoro-compatible cooperative fiber scheduling.
//
// The fixtures use `extern fun` FFI to call a C harness that exercises the
// scheduler API directly; no language-level stream/agent syntax is required
// at this phase.
//
// Phase 9.0 fixtures:
//   - sched_basic: two fibers ping-pong via yield; verifies interleaving order.
//   - sched_yield: single fiber yields multiple times; verifies resume count.
//   - sched_nested: three fibers interleaved round-robin; verifies FIFO queue.
func TestPhase9Scheduler(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 9 scheduler gate skipped on Windows; ucontext ships POSIX only")
	}
	runFixtureSuite(t, "scheduler")
}
