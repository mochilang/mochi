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

// lowerStreamMakeExpr emits mochiStreamMake[T](cap).
func (l *lowerer) lowerStreamMakeExpr(e *aotir.StreamMakeExpr) (gotree.Expr, error) {
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, fmt.Errorf("stream make cap: %w", err)
	}
	et, err := l.lowerType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("stream make elem type: %w", err)
	}
	l.addHelper("mochiStream")
	l.addHelper("mochiSub")
	l.addHelper("mochiStreamMake")
	l.addImport("sync")
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiStreamMake[" + et + "]"},
		Args: []gotree.Expr{&gotree.CallExpr{Fun: &gotree.Ident{Name: "int"}, Args: []gotree.Expr{cap}}},
	}, nil
}

// lowerSubMakeExpr emits mochiStreamSubscribe[T](stream).
func (l *lowerer) lowerSubMakeExpr(e *aotir.SubMakeExpr) (gotree.Expr, error) {
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, fmt.Errorf("sub make stream: %w", err)
	}
	et, err := l.lowerType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("sub make elem type: %w", err)
	}
	l.addHelper("mochiStream")
	l.addHelper("mochiSub")
	l.addHelper("mochiStreamSubscribe")
	l.addImport("sync")
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiStreamSubscribe[" + et + "]"},
		Args: []gotree.Expr{stream},
	}, nil
}

// lowerSubMakeLimitExpr emits mochiStreamSubscribeLimit[T](stream, int(limit)).
func (l *lowerer) lowerSubMakeLimitExpr(e *aotir.SubMakeLimitExpr) (gotree.Expr, error) {
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, fmt.Errorf("sub make limit stream: %w", err)
	}
	limit, err := l.lowerExpr(e.Limit)
	if err != nil {
		return nil, fmt.Errorf("sub make limit: %w", err)
	}
	et, err := l.lowerType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("sub make limit elem type: %w", err)
	}
	l.addHelper("mochiStream")
	l.addHelper("mochiSub")
	l.addHelper("mochiStreamSubscribeLimit")
	l.addImport("sync")
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiStreamSubscribeLimit[" + et + "]"},
		Args: []gotree.Expr{stream, &gotree.CallExpr{Fun: &gotree.Ident{Name: "int"}, Args: []gotree.Expr{limit}}},
	}, nil
}

// lowerSubRecvExpr emits mochiSubRecv(sub). Go infers the element type
// from the *mochiSub[T] argument, so no explicit type parameter is needed.
func (l *lowerer) lowerSubRecvExpr(e *aotir.SubRecvExpr) (gotree.Expr, error) {
	sub, err := l.lowerExpr(e.Sub)
	if err != nil {
		return nil, fmt.Errorf("sub recv: %w", err)
	}
	l.addHelper("mochiSub")
	l.addHelper("mochiSubRecv")
	return &gotree.CallExpr{
		Fun:  &gotree.Ident{Name: "mochiSubRecv"},
		Args: []gotree.Expr{sub},
	}, nil
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

// lowerAsyncExpr emits `async expr` as a goroutine-backed buffered channel:
//
//	func() chan T { ch := make(chan T, 1); go func() { ch <- expr }(); return ch }()
func (l *lowerer) lowerAsyncExpr(e *aotir.AsyncExpr) (gotree.Expr, error) {
	body, err := l.lowerExpr(e.Body)
	if err != nil {
		return nil, fmt.Errorf("async body: %w", err)
	}
	et, err := l.lowerType(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("async elem type: %w", err)
	}
	chanType := "chan " + et

	// __fut := make(chan T, 1)
	makeCh := &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: "__fut"}},
		Tok: ":=",
		Rhs: []gotree.Expr{&gotree.CallExpr{
			Fun: &gotree.Ident{Name: "make"},
			Args: []gotree.Expr{
				&gotree.RawExpr{Src: chanType},
				&gotree.BasicLit{Kind: gotree.IntLit, Value: "1"},
			},
		}},
	}

	// go func() { __fut <- body }()
	goStmt := &gotree.GoStmt{
		Call: &gotree.CallExpr{
			Fun: &gotree.FuncLit{
				Type: &gotree.FuncType{},
				Body: &gotree.BlockStmt{
					List: []gotree.Stmt{
						&gotree.SendStmt{
							Chan:  &gotree.Ident{Name: "__fut"},
							Value: body,
						},
					},
				},
			},
		},
	}

	// return __fut
	retCh := &gotree.ReturnStmt{
		Results: []gotree.Expr{&gotree.Ident{Name: "__fut"}},
	}

	return &gotree.CallExpr{
		Fun: &gotree.FuncLit{
			Type: &gotree.FuncType{
				Results: []gotree.Field{{Type: &gotree.RawExpr{Src: chanType}}},
			},
			Body: &gotree.BlockStmt{
				List: []gotree.Stmt{makeCh, goStmt, retCh},
			},
		},
	}, nil
}

// lowerAwaitExpr unwraps a future. Paired with lowerAsyncExpr's goroutine
// emit, this receives from the channel: <-future.
func (l *lowerer) lowerAwaitExpr(e *aotir.AwaitExpr) (gotree.Expr, error) {
	ch, err := l.lowerExpr(e.Future)
	if err != nil {
		return nil, fmt.Errorf("await: %w", err)
	}
	return &gotree.UnaryExpr{Op: "<-", X: ch}, nil
}

// lowerLLMGenerateExpr emits mochiLLMGenerate(provider, model, prompt).
// In cassette mode (MOCHI_LLM_CASSETTE_DIR set) the helper replays a
// recorded reply; live providers are deferred to Phase 13.1+.
func (l *lowerer) lowerLLMGenerateExpr(e *aotir.LLMGenerateExpr) (gotree.Expr, error) {
	model, err := l.lowerExpr(e.Model)
	if err != nil {
		return nil, fmt.Errorf("llm generate model: %w", err)
	}
	prompt, err := l.lowerExpr(e.Prompt)
	if err != nil {
		return nil, fmt.Errorf("llm generate prompt: %w", err)
	}
	l.addHelper("mochiLLMGenerate")
	l.addHelper("mochiDJB2Key")
	l.addImport("fmt")
	l.addImport("os")
	l.addImport("strings")
	return &gotree.CallExpr{
		Fun: &gotree.Ident{Name: "mochiLLMGenerate"},
		Args: []gotree.Expr{
			&gotree.BasicLit{Kind: gotree.StringLit, Value: e.Provider},
			model,
			prompt,
		},
	}, nil
}

