package build

import (
	"testing"
)

// TestPhase9Agent is the MEP-45 Phase 9.3 gate for synchronous agent dispatch.
// It walks every fixture under tests/transpiler3/c/fixtures/agent and runs the
// same end-to-end pipeline as all other phase gates.
//
// Phase 9.3 adds:
//   - agent as a first-class declaration in the type system
//   - TypeAgent IR type with AgentName parallel field
//   - AgentDecl / AgentIntentDecl / AgentLit / AgentMethodRef /
//     AgentIntentCallExpr / AgentIntentCallStmt IR nodes (aotir/program.go)
//   - Full lowering and emission for agent struct typedef + static intent fns
//   - Synchronous intent dispatch: mochi_agent_NAME__INTENT(&recv, args...)
//   - Intent bodies access agent fields via __self->field (pointer receiver)
//
// Agents are emitted as a C typedef struct + one static function per intent.
// There is no mailbox or fiber in Phase 9.3; dispatch is a direct function
// call on a stack-allocated struct pointer.
//
// Phase 9.3 fixtures:
//   - agent_basic: Counter agent with increment/value intents; int field.
//   - agent_bool: Switch agent with toggle/is_on intents; bool field.
//   - agent_float: Balance agent with deposit/withdraw/get intents; float field.
//   - agent_multi_intent: Accumulator with add/reset/get intents and an int param.
//   - agent_string: Greeter agent with set_name/get_name intents; string field.
func TestPhase9Agent(t *testing.T) {
	runFixtureSuite(t, "agent")
}
