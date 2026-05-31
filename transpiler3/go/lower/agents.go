package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// agentSelfName is the receiver name used for agent intent methods.
// varRefExpr strips this prefix when translating self-field accesses.
const agentSelfName = "self"

// lowerChanMakeExpr emits make(chan <elemType>, cap).
func (l *lowerer) lowerChanMakeExpr(e *aotir.ChanMakeExpr) (gotree.Expr, error) {
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, fmt.Errorf("chan make cap: %w", err)
	}
	et, err := l.lowerType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("chan make elem type: %w", err)
	}
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "make"},
		Args: []gotree.Expr{&gotree.Ident{Name: "chan " + et}, cap},
	}, nil
}

// lowerChanRecvExpr emits <-ch.
func (l *lowerer) lowerChanRecvExpr(e *aotir.ChanRecvExpr) (gotree.Expr, error) {
	ch, err := l.lowerExpr(e.Chan)
	if err != nil {
		return nil, fmt.Errorf("chan recv: %w", err)
	}
	return &gotree.UnaryExpr{Op: "<-", X: ch}, nil
}

// lowerStreamMakeExpr stubs stream creation for the Go target.
func (l *lowerer) lowerStreamMakeExpr(e *aotir.StreamMakeExpr) (gotree.Expr, error) {
	return nil, fmt.Errorf("transpiler3/go/lower: StreamMakeExpr not yet implemented in Go target")
}

// lowerSubMakeExpr stubs subscriber creation for the Go target.
func (l *lowerer) lowerSubMakeExpr(e *aotir.SubMakeExpr) (gotree.Expr, error) {
	return nil, fmt.Errorf("transpiler3/go/lower: SubMakeExpr not yet implemented in Go target")
}

// lowerSubMakeLimitExpr stubs bounded-subscriber creation for the Go target.
func (l *lowerer) lowerSubMakeLimitExpr(e *aotir.SubMakeLimitExpr) (gotree.Expr, error) {
	return nil, fmt.Errorf("transpiler3/go/lower: SubMakeLimitExpr not yet implemented in Go target")
}

// lowerSubRecvExpr stubs subscriber receive for the Go target.
func (l *lowerer) lowerSubRecvExpr(e *aotir.SubRecvExpr) (gotree.Expr, error) {
	return nil, fmt.Errorf("transpiler3/go/lower: SubRecvExpr not yet implemented in Go target")
}

// lowerAgentLit emits &AgentName{Field: val, ...}.
func (l *lowerer) lowerAgentLit(e *aotir.AgentLit) (gotree.Expr, error) {
	elts := make([]gotree.Expr, 0, len(e.Fields))
	for _, f := range e.Fields {
		val, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("agent lit field %s: %w", f.Name, err)
		}
		elts = append(elts, &gotree.KeyValueExpr{
			Key:   &gotree.Ident{Name: exportIdent(f.Name)},
			Value: val,
		})
	}
	return &gotree.UnaryExpr{
		Op: "&",
		X:  &gotree.CompositeLit{Type: &gotree.Ident{Name: e.AgentName}, Elts: elts},
	}, nil
}

// lowerAgentSpawnExpr emits &AgentName{} (field initialisation via separate calls in a later phase).
func (l *lowerer) lowerAgentSpawnExpr(e *aotir.AgentSpawnExpr) (gotree.Expr, error) {
	return &gotree.UnaryExpr{
		Op: "&",
		X:  &gotree.CompositeLit{Type: &gotree.Ident{Name: e.AgentName}},
	}, nil
}

// lowerAgentIntentCallExpr emits receiver.IntentName(args...).
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (gotree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("agent intent call recv: %w", err)
	}
	args := make([]gotree.Expr, 0, len(e.Args))
	for i, a := range e.Args {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("agent intent call arg %d: %w", i, err)
		}
		args = append(args, v)
	}
	return &gotree.CallExpr{
		Fun: &gotree.SelectorExpr{
			X:   recv,
			Sel: exportIdent(e.IntentName),
		},
		Args: args,
	}, nil
}

// lowerAsyncExpr emits the body expression synchronously for the Go target.
// Full goroutine-backed futures land in a later phase.
func (l *lowerer) lowerAsyncExpr(e *aotir.AsyncExpr) (gotree.Expr, error) {
	return l.lowerExpr(e.Body)
}

// lowerAwaitExpr unwraps a future. Paired with lowerAsyncExpr's synchronous
// emit, this is a no-op pass-through.
func (l *lowerer) lowerAwaitExpr(e *aotir.AwaitExpr) (gotree.Expr, error) {
	return l.lowerExpr(e.Future)
}
