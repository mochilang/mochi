package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/python/pysrc"
)

// lowerAgentDecl emits a Mochi agent declaration as a plain Python class.
//
// Agents differ from records in two ways: their fields are mutable (no
// `frozen=True` decorator) and they carry intent methods. We render the
// class as:
//
//	class Name:
//	    field1: T1
//	    field2: T2
//
//	    def __init__(self, field1: T1, field2: T2) -> None:
//	        self.field1 = field1
//	        self.field2 = field2
//
//	    def intent_name(self, p: T) -> R:
//	        <body>
//
// We use an explicit __init__ instead of @dataclass because the intent
// bodies inside the class reference `self.field` directly, while the C
// lower emits them as VarRef("field"). Until the c lower surfaces
// "self." prefix in IR, we rewrite VarRef of agent-field names to
// `self.X` here.
//
// Synchronous-only: spawn / async messaging is Phase 10+. Phase 9 only
// covers in-process AgentLit + AgentIntentCall* over a non-spawned ref.
func (l *lowerer) lowerAgentDecl(a *aotir.AgentDecl) (pysrc.Stmt, error) {
	fields := make([]pysrc.ClassField, 0, len(a.Fields))
	for _, f := range a.Fields {
		fields = append(fields, pysrc.ClassField{
			Name: f.Name,
			Type: pyTypeForRecord(f.Type, aotir.TypeInvalid, f.RecordName, "", aotir.TypeInvalid, aotir.TypeInvalid),
		})
	}

	initParams := []pysrc.Param{{Name: "self"}}
	initBody := make([]pysrc.Stmt, 0, len(a.Fields))
	for _, f := range a.Fields {
		annot := pyTypeForRecord(f.Type, aotir.TypeInvalid, f.RecordName, "", aotir.TypeInvalid, aotir.TypeInvalid)
		initParams = append(initParams, pysrc.Param{Name: f.Name, Type: annot})
		initBody = append(initBody, &pysrc.AttrAssignStmt{
			Target: &pysrc.Name{Id: "self"},
			Attr:   f.Name,
			Value:  &pysrc.Name{Id: f.Name},
		})
	}
	if len(initBody) == 0 {
		initBody = append(initBody, &pysrc.PassStmt{})
	}
	init := &pysrc.FunctionDef{
		Name:       "__init__",
		Params:     initParams,
		ReturnType: pysrc.TypeNone,
		Body:       initBody,
	}

	// Intent bodies reference agent fields via VarRef.Name = "__self->X".
	// The Python lower's VarRef/AssignStmt cases rewrite those to `self.X`.
	methods := make([]*pysrc.FunctionDef, 0, len(a.Intents))
	for _, intent := range a.Intents {
		body, err := l.lowerBlock(intent.Body)
		if err != nil {
			return nil, fmt.Errorf("agent %s intent %s: %w", a.Name, intent.Name, err)
		}
		params := []pysrc.Param{{Name: "self"}}
		for _, p := range intent.Params {
			params = append(params, pysrc.Param{
				Name: p.Name,
				Type: pyTypeFor(p.Type),
			})
		}
		ret := pyTypeFor(intent.ReturnType)
		if intent.ReturnType == aotir.TypeUnit {
			ret = pysrc.TypeNone
		}
		methods = append(methods, &pysrc.FunctionDef{
			Name:       intent.Name,
			Params:     params,
			ReturnType: ret,
			Body:       body,
		})
	}

	return &pysrc.ClassDef{
		Name:    a.Name,
		Fields:  fields,
		Init:    init,
		Methods: methods,
	}, nil
}

// lowerAgentLit emits `AgentName(field1=v1, field2=v2)`. Field order
// matches AgentDecl declaration order.
func (l *lowerer) lowerAgentLit(e *aotir.AgentLit) (pysrc.Expr, error) {
	kwargs := make([]pysrc.KeywordArg, 0, len(e.Fields))
	for _, f := range e.Fields {
		val, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		kwargs = append(kwargs, pysrc.KeywordArg{Name: f.Name, Value: val})
	}
	return &pysrc.Call{
		Func:   &pysrc.Name{Id: e.AgentName},
		Kwargs: kwargs,
	}, nil
}

// lowerAgentIntentCallExpr emits `receiver.intent(args...)`. Synchronous
// dispatch only (Phase 9.0); spawned agents are deferred to Phase 10.
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (pysrc.Expr, error) {
	if e.SpawnedRef {
		return nil, fmt.Errorf("python/lower: spawned-agent calls not supported (Phase 9.1 deferred to async surface)")
	}
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	args, err := l.lowerExprs(e.Args)
	if err != nil {
		return nil, err
	}
	return &pysrc.Call{
		Func: &pysrc.Attribute{Value: recv, Attr: e.IntentName},
		Args: args,
	}, nil
}

// lowerChanMakeExpr emits `deque[T](maxlen=N)`. Phase 9 channels are
// single-threaded synchronous FIFO queues. The fixture corpus
// `tests/transpiler3/c/fixtures/chan/*` exercises send-then-recv on a
// pre-bounded channel; collections.deque models that exactly.
//
// Cross-coroutine send/recv with backpressure is Phase 10's async
// surface (asyncio.Queue).
func (l *lowerer) lowerChanMakeExpr(e *aotir.ChanMakeExpr) (pysrc.Expr, error) {
	l.needsDeque = true
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, err
	}
	return &pysrc.Call{
		Func:   &pysrc.Name{Id: "deque"},
		Kwargs: []pysrc.KeywordArg{{Name: "maxlen", Value: cap}},
	}, nil
}

// lowerChanSendStmt emits `chan.append(value)`. deque.append on a
// bounded deque silently drops from the left when at capacity, which
// matches the v1 fixture corpus (every send has a paired recv).
func (l *lowerer) lowerChanSendStmt(s *aotir.ChanSendStmt) (pysrc.Stmt, error) {
	ch, err := l.lowerExpr(s.Chan)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, err
	}
	return &pysrc.ExprStmt{X: &pysrc.Call{
		Func: &pysrc.Attribute{Value: ch, Attr: "append"},
		Args: []pysrc.Expr{val},
	}}, nil
}

// lowerChanRecvExpr emits `chan.popleft()`. FIFO semantics.
func (l *lowerer) lowerChanRecvExpr(e *aotir.ChanRecvExpr) (pysrc.Expr, error) {
	ch, err := l.lowerExpr(e.Chan)
	if err != nil {
		return nil, err
	}
	return &pysrc.Call{
		Func: &pysrc.Attribute{Value: ch, Attr: "popleft"},
	}, nil
}

// lowerAgentIntentCallStmt is the statement form (void return or
// discarded result).
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt) (pysrc.Stmt, error) {
	if s.SpawnedRef {
		return nil, fmt.Errorf("python/lower: spawned-agent calls not supported (Phase 9.1 deferred to async surface)")
	}
	recv, err := l.lowerExpr(s.Receiver)
	if err != nil {
		return nil, err
	}
	args, err := l.lowerExprs(s.Args)
	if err != nil {
		return nil, err
	}
	return &pysrc.ExprStmt{X: &pysrc.Call{
		Func: &pysrc.Attribute{Value: recv, Attr: s.IntentName},
		Args: args,
	}}, nil
}
