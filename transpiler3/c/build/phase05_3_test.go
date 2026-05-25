package build

import (
	"testing"
)

// TestPhase5MethodShim is the MEP-45 Phase 5.3 gate for agent method-as-closure shims.
// It walks every fixture under tests/transpiler3/c/fixtures/method_shim and runs the
// same end-to-end pipeline as all other phase gates.
//
// Phase 5.3 adds:
//   - Bare agent method reference (c.intent with no following call) lowers to a FunLit.
//   - A __methodshim_AGENT_INTENT(void *__mochi_env, params...) shim is emitted once
//     per TU; it casts __mochi_env to mochi_agent_AGENT_t * and forwards to the intent.
//   - The FunLit carries &receiver as the env pointer so method calls through the
//     closure dispatch to the correct agent instance.
//   - Shims are deduplicated via the shimFuncs map (same mechanism as Phase 5.2).
//
// Phase 5.3 fixtures:
//   - method_shim_basic: Counter agent, store c.value as closure, call it.
//   - method_shim_arg: Mul agent, store m.multiply (takes int) as closure, call with args.
//   - method_shim_bool: Flag agent, store g.read (bool) as closure, call it.
//   - method_shim_float: Box agent, store b.get (float) as closure, call it.
//   - method_shim_pass: pass c.value as fun():int arg to a regular function.
func TestPhase5MethodShim(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping C compilation test in short mode")
	}
	runFixtureSuite(t, "method_shim")
}
