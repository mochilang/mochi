// Phase 9 lands Mochi's agent sub-language for the TypeScript target.
//
// Mochi exposes a small synchronous-actor surface:
//
//     agent Counter {
//         var count: int = 0
//         intent increment() { count = count + 1 }
//         intent value(): int { return count }
//     }
//
//     let c = Counter { count: 0 }
//     c.increment()
//     print(c.value())
//
// MEP-52 §Phase 9's original budget proposed an
// `@mochi/runtime/agent` AsyncIterableQueue + AbortController
// runtime (~8 KB gzipped) plus a MochiAgent / MochiSupervisor pair
// that would let any intent call asynchronously. The audit found:
//
//   - Every fixture in the Phase 9 corpus is a single-threaded
//     synchronous state machine. No spawn-and-message pattern, no
//     mailbox draining, no Abort surface, no supervision tree.
//   - The C and Rust transpilers both compile agents to plain
//     struct + free function `mochi_agent_NAME__INTENT(__self, ...)`
//     with synchronous dispatch. The Rust path is at
//     transpiler3/rust/lower/lower.go:246 (`lowerAgentDecl`).
//   - The `c.intent()` call site is observationally identical to a
//     regular method call. Wrapping it in an AsyncIterableQueue
//     would force the async colour onto every intent (Phase 11)
//     even though no fixture's stdout depends on async ordering.
//
// The TS path therefore mirrors the Rust path: emit a class with
// mutable fields, a private constructor + static of() factory
// (parallel to the existing record class shape), and one method
// per intent. The intent body lowers as-is, with `__self->X`
// rewritten to `this.X` on both read and write sites (paralleling
// the Rust path's `__self.X` rewrite at lower.go:735 / 863). The
// runtime cost drops to zero bytes. If a future phase introduces
// `spawn`, mailbox-style messaging, or cross-agent supervision,
// the runtime engine can be added without disturbing the
// synchronous path for closed programs.
//
// Why not the existing ClassDecl: the record class is readonly
// (records are immutable in Mochi); agents are mutable by
// definition (the only reason intents exist is to mutate state).
// A separate AgentClassDecl keeps the readonly/mutable contract
// crisp at the tstree layer and avoids leaking the readonly
// modifier into the agent emit (a TS class with readonly fields
// would refuse the `this.x = ...` writes that every intent body
// produces).

package lower

import (
	"fmt"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// selfFieldPrefix is the C lowerer's intent-body prefix for field
// references. VarRef.Name and AssignStmt.Name carry it on every
// access to a field from within an intent. The TS lowerer rewrites
// it to `this.X` because TS classes provide `this` lexically
// inside method bodies.
const selfFieldPrefix = "__self->"

// stripSelfPrefix returns (field, true) when name has the
// `__self->` prefix; otherwise (name, false). Used by VarRef and
// AssignStmt lowering to detect intent-body field references.
func stripSelfPrefix(name string) (string, bool) {
	rest, ok := strings.CutPrefix(name, selfFieldPrefix)
	return rest, ok
}

// agentDecls walks prog.Agents in source order and emits one
// AgentClassDecl per agent. Emit order is the source order set by
// the C lowerer; two builds of the same program produce a byte-
// stable prelude (Phase 16 reproducibility gate).
//
// Each intent body is lowered through the regular lowerBlock path.
// Inside an intent body the C lowerer stamps every field reference
// as `__self->FieldName`; the VarRef and AssignStmt cases in
// lowerExpr / lowerAssignStmt detect that prefix and emit `this.X`
// member access / assignment. There is no special intent-body
// pass; the prefix rewrite is uniform.
func (l *lowerer) agentDecls() ([]tstree.Decl, error) {
	if len(l.prog.Agents) == 0 {
		return nil, nil
	}
	out := make([]tstree.Decl, 0, len(l.prog.Agents))
	for _, ad := range l.prog.Agents {
		if ad == nil {
			continue
		}
		fields := make([]tstree.ClassField, 0, len(ad.Fields))
		for _, f := range ad.Fields {
			ty, err := tsTypeFor(f.Type)
			if err != nil {
				return nil, fmt.Errorf("ts lower: agent %q field %q: %w", ad.Name, f.Name, err)
			}
			fields = append(fields, tstree.ClassField{Name: f.Name, Type: ty})
		}
		methods := make([]*tstree.MethodDecl, 0, len(ad.Intents))
		for _, intent := range ad.Intents {
			params := make([]tstree.FuncParam, 0, len(intent.Params))
			for _, p := range intent.Params {
				pt, err := tsTypeFor(p.Type)
				if err != nil {
					return nil, fmt.Errorf("ts lower: agent %q intent %q param %q: %w", ad.Name, intent.Name, p.Name, err)
				}
				params = append(params, tstree.FuncParam{Name: p.Name, Type: pt})
			}
			ret, err := tsTypeFor(intent.ReturnType)
			if err != nil {
				return nil, fmt.Errorf("ts lower: agent %q intent %q return: %w", ad.Name, intent.Name, err)
			}
			if intent.Body == nil {
				return nil, fmt.Errorf("ts lower: agent %q intent %q has nil Body", ad.Name, intent.Name)
			}
			body, err := l.lowerBlock(intent.Body.Statements)
			if err != nil {
				return nil, fmt.Errorf("ts lower: agent %q intent %q body: %w", ad.Name, intent.Name, err)
			}
			methods = append(methods, &tstree.MethodDecl{
				Name:       intent.Name,
				Params:     params,
				ReturnType: ret,
				Body:       body,
			})
		}
		out = append(out, &tstree.AgentClassDecl{
			Doc: []string{
				"Generated agent class for Mochi `agent " + ad.Name + "`.",
				"Fields are mutable; intents become method declarations",
				"dispatched synchronously (no runtime engine).",
			},
			Name:    ad.Name,
			Fields:  fields,
			Methods: methods,
		})
	}
	return out, nil
}

// lowerAgentLit translates `Counter { count: 0 }` to
// `Counter.of({ count: 0 })`. Identical shape to RecordLit; only
// the receiver class name differs. The aotir lowerer has already
// reordered the user's literal args into agent-decl source order.
func (l *lowerer) lowerAgentLit(e *aotir.AgentLit) (tstree.Expr, error) {
	if e.AgentName == "" {
		return nil, fmt.Errorf("ts lower: agent literal missing AgentName")
	}
	fields := make([]tstree.ObjectLitField, 0, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent %q field %d (%q): %w", e.AgentName, i, f.Name, err)
		}
		fields = append(fields, tstree.ObjectLitField{Name: f.Name, Value: v})
	}
	return &tstree.RecordOfCallExpr{
		RecordName: e.AgentName,
		Fields:     &tstree.ObjectLit{Fields: fields},
	}, nil
}

// lowerAgentIntentCallExpr translates the value-returning form
// `c.value()` to `c.value()`. The aotir captures (AgentName,
// IntentName, Receiver, Args); the TS emit reads `Receiver.IntentName(Args)`
// which is one canonical method-call expression. The receiver is
// any aotir Expr (typically a VarRef of the agent binding).
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (tstree.Expr, error) {
	return l.lowerAgentIntentCall(e.IntentName, e.Receiver, e.Args)
}

// lowerAgentIntentCallStmt translates the void-or-discarded form
// `c.increment()` to `c.increment();`. Identical lowering to the
// expression form, wrapped in an ExprStmt.
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt) ([]tstree.Stmt, error) {
	call, err := l.lowerAgentIntentCall(s.IntentName, s.Receiver, s.Args)
	if err != nil {
		return nil, err
	}
	return []tstree.Stmt{&tstree.ExprStmt{Expr: call}}, nil
}

// lowerAgentIntentCall is the shared call-shape builder for the
// stmt and expr forms. The receiver lowers through the regular
// lowerExpr path (so a VarRef becomes an IdentExpr); the
// MemberCallExpr renders `receiver.intent(args)`.
func (l *lowerer) lowerAgentIntentCall(intentName string, receiver aotir.Expr, callArgs []aotir.Expr) (tstree.Expr, error) {
	if intentName == "" {
		return nil, fmt.Errorf("ts lower: agent intent call missing IntentName")
	}
	recv, err := l.lowerExpr(receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: agent intent %q receiver: %w", intentName, err)
	}
	args := make([]tstree.Expr, 0, len(callArgs))
	for i, a := range callArgs {
		la, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent intent %q arg %d: %w", intentName, i, err)
		}
		args = append(args, la)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   intentName,
		Args:     args,
	}, nil
}
