package lower

import (
	"fmt"
	"sort"
	"strings"

	"mochi/transpiler3/beam/cerl"
	"mochi/transpiler3/c/aotir"
)

// Lower converts an aotir.Program to a cerl.Module ready for
// compile:forms/2 [from_core].
func Lower(prog *aotir.Program, modName string) (*cerl.Module, error) {
	if prog == nil {
		return nil, fmt.Errorf("beam/lower: nil program")
	}
	if modName == "" {
		return nil, fmt.Errorf("beam/lower: empty module name")
	}

	mod := &cerl.Module{
		Name:    modName,
		Exports: []cerl.FuncRef{{Name: "main", Arity: 1}},
	}

	if prog.Main < 0 || prog.Main >= len(prog.Functions) {
		return nil, fmt.Errorf("beam/lower: invalid main index %d (len=%d)", prog.Main, len(prog.Functions))
	}

	records := make(map[string]*aotir.RecordDecl, len(prog.Records))
	for _, r := range prog.Records {
		records[r.Name] = r
	}

	// Build a map of lifted functions so FunLit nodes can inline them as c_fun.
	liftedFuncs := make(map[string]*aotir.Function)
	for _, fn := range prog.Functions {
		if fn.IsLifted {
			liftedFuncs[fn.Name] = fn
		}
	}

	agents := make(map[string]*aotir.AgentDecl, len(prog.Agents))
	for _, ag := range prog.Agents {
		agents[ag.Name] = ag
	}

	l := &lowerer{mod: mod, records: records, liftedFuncs: liftedFuncs, agents: agents}

	// Phase 9.0: emit helper functions for each agent intent before user functions.
	for _, ag := range prog.Agents {
		if err := l.lowerAgentIntentFunctions(ag); err != nil {
			return nil, fmt.Errorf("beam/lower: agent %s: %w", ag.Name, err)
		}
	}

	for i, fn := range prog.Functions {
		if i == prog.Main || fn.IsLifted {
			continue
		}
		if err := l.lowerFunction(fn); err != nil {
			return nil, fmt.Errorf("beam/lower: lower %s: %w", fn.Name, err)
		}
	}

	mainFn := prog.Functions[prog.Main]
	body, err := l.lowerFunctionBody(mainFn.Body.Statements, nil)
	if err != nil {
		return nil, fmt.Errorf("beam/lower: lower main: %w", err)
	}

	mod.Defs = append(mod.Defs, cerl.FuncDef{
		Name:  "main",
		Arity: 1,
		Vars:  []string{"V__args"},
		Body:  body,
	})

	// Phase 17.0: emit -spec attributes for all functions + main.
	addSpecAttrs(mod, prog.Functions)

	// Phase 18.1: sort Defs by (name, arity) for canonical, reproducible output.
	sort.Slice(mod.Defs, func(i, j int) bool {
		if mod.Defs[i].Name != mod.Defs[j].Name {
			return mod.Defs[i].Name < mod.Defs[j].Name
		}
		return mod.Defs[i].Arity < mod.Defs[j].Arity
	})

	return mod, nil
}

// lowerer holds mutable state for one Lower() call.
type lowerer struct {
	mod          *cerl.Module
	loopNum      int             // monotonic counter for while/for helpers
	tryNum       int             // monotonic counter for unique try/catch variable names
	loopStack    []loopCtx       // stack of active loop contexts (innermost last)
	scope        map[string]bool // outer variables currently in scope
	records      map[string]*aotir.RecordDecl   // record name -> declaration
	liftedFuncs  map[string]*aotir.Function     // lifted closure bodies (skipped as top-level)
	agents       map[string]*aotir.AgentDecl    // agent name -> declaration (Phase 9.0)
}

// nextTryNum returns a unique suffix for CTry exception variable names.
func (l *lowerer) nextTryNum() int {
	n := l.tryNum
	l.tryNum++
	return n
}

// loopCtx holds context about one active loop.
type loopCtx struct {
	num    int
	params []string // outer vars threaded through this loop
}

func (l *lowerer) nextLoopNum() int {
	l.loopNum++
	return l.loopNum
}

func (l *lowerer) currentLoop() *loopCtx {
	if len(l.loopStack) == 0 {
		return nil
	}
	return &l.loopStack[len(l.loopStack)-1]
}

// lowerFunction lowers a non-main user function and appends it to mod.Defs.
func (l *lowerer) lowerFunction(fn *aotir.Function) error {
	vars := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		vars[i] = "V_" + p.Name
	}

	// Seed scope with parameters.
	outer := l.scope
	l.scope = make(map[string]bool)
	for _, p := range fn.Params {
		l.scope[p.Name] = true
	}

	body, err := l.lowerFunctionBody(fn.Body.Statements, nil)
	l.scope = outer
	if err != nil {
		return err
	}

	l.mod.Defs = append(l.mod.Defs, cerl.FuncDef{
		Name:  fn.Name,
		Arity: len(fn.Params),
		Vars:  vars,
		Body:  body,
	})
	return nil
}

// lowerFunctionBody lowers a function body, wrapping it in a c_try
// that catches {mochi_return, V} thrown by return statements.
func (l *lowerer) lowerFunctionBody(stmts []aotir.Stmt, cont cerl.Expr) (cerl.Expr, error) {
	body, err := l.lowerBlock(stmts, cont)
	if err != nil {
		return nil, err
	}
	// Use unique variable names so nested CTry nodes (from wrapArithErr or
	// TryCatchStmt) do not conflict with the function-body return wrapper.
	n := l.nextTryNum()
	cls := fmt.Sprintf("V___cls%d", n)
	rsn := fmt.Sprintf("V___rsn%d", n)
	stk := fmt.Sprintf("V___stk%d", n)
	ret := fmt.Sprintf("V___ret%d", n)
	retval := fmt.Sprintf("V___retval%d", n)
	return cerl.CTry(
		body,
		[]cerl.Expr{cerl.CVar(ret)},
		cerl.CVar(ret),
		[]cerl.Expr{cerl.CVar(cls), cerl.CVar(rsn), cerl.CVar(stk)},
		cerl.CCase(cerl.CVar(rsn), []cerl.Expr{
			cerl.CClause(
				[]cerl.Expr{cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_return"), cerl.CVar(retval)})},
				cerl.CAtom("true"),
				cerl.CVar(retval),
			),
			cerl.CClause(
				[]cerl.Expr{cerl.CVar("V___")},
				cerl.CAtom("true"),
				cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("raise"), []cerl.Expr{
					cerl.CVar(cls), cerl.CVar(rsn), cerl.CVar(stk),
				}),
			),
		}),
	), nil
}

// lowerBlock lowers a slice of statements to a cerl expression.
// cont is the continuation expression used when the block is empty
// (nil means c_atom("ok")).
func (l *lowerer) lowerBlock(stmts []aotir.Stmt, cont cerl.Expr) (cerl.Expr, error) {
	if len(stmts) == 0 {
		if cont != nil {
			return cont, nil
		}
		return cerl.CAtom("ok"), nil
	}

	head := stmts[0]
	tail := stmts[1:]

	switch s := head.(type) {
	case *aotir.LetStmt:
		// Track variable in scope.
		if l.scope == nil {
			l.scope = make(map[string]bool)
		}
		l.scope[s.Name] = true

		// LetStmt with nil Init is a declaration-only statement emitted for
		// match-as-expression temp vars. The binding is established by the
		// subsequent MatchStmt, so skip the CLet here.
		if s.Init == nil {
			return l.lowerBlock(tail, cont)
		}

		init, err := lowerExpr(l, s.Init)
		if err != nil {
			return nil, err
		}
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + s.Name)}, init, rest), nil

	case *aotir.AssignStmt:
		// Phase 9.0: agent field mutation uses "__self->" prefix.
		// Use maps:put/3 (a runtime call) instead of Core Erlang map-update
		// syntax to avoid BEAM validator "bad_type: actual=any" errors when the
		// validator cannot statically prove V___self is a map.
		if strings.HasPrefix(s.Name, "__self->") {
			field := strings.TrimPrefix(s.Name, "__self->")
			val, err := lowerExpr(l, s.Value)
			if err != nil {
				return nil, err
			}
			mapUpdate := cerl.CCall(cerl.CAtom("maps"), cerl.CAtom("put"),
				[]cerl.Expr{cerl.CAtom(field), val, cerl.CVar("V___self")})
			rest, err := l.lowerBlock(tail, cont)
			if err != nil {
				return nil, err
			}
			return cerl.CLet([]cerl.Expr{cerl.CVar("V___self")}, mapUpdate, rest), nil
		}
		val, err := lowerExpr(l, s.Value)
		if err != nil {
			return nil, err
		}
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + s.Name)}, val, rest), nil

	case *aotir.ReturnStmt:
		// Use erlang:throw/1 for new exceptions (primop 'raise' is only for re-raising).
		if s.Value == nil {
			return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("throw"),
				[]cerl.Expr{cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_return"), cerl.CAtom("ok")})}), nil
		}
		val, err := lowerExpr(l, s.Value)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("throw"),
			[]cerl.Expr{cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_return"), val})}), nil

	case *aotir.BreakStmt:
		lc := l.currentLoop()
		if lc == nil {
			return nil, fmt.Errorf("beam/lower: break outside loop")
		}
		state := l.loopStateExpr(lc.params)
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("throw"),
			[]cerl.Expr{cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_break"), cerl.CInt(int64(lc.num)), state})}), nil

	case *aotir.ContinueStmt:
		lc := l.currentLoop()
		if lc == nil {
			return nil, fmt.Errorf("beam/lower: continue outside loop")
		}
		state := l.loopStateExpr(lc.params)
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("throw"),
			[]cerl.Expr{cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_continue"), cerl.CInt(int64(lc.num)), state})}), nil

	case *aotir.IfStmt:
		// Thread the continuation into each if-branch so that variable updates
		// inside the branch are in scope for subsequent statements (e.g. count++
		// inside a for-each body must be visible to the recursion call that follows).
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return l.lowerIfStmtWithCont(s, rest)

	case *aotir.WhileStmt:
		// Compute rest first so loop var updates scope into subsequent code.
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return l.lowerWhileStmt(s, rest)

	case *aotir.ForRangeStmt:
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return l.lowerForRangeStmt(s, rest)

	case *aotir.CallStmt:
		expr, err := lowerCallStmt(l, s)
		if err != nil {
			return nil, err
		}
		if len(tail) == 0 && cont == nil {
			return expr, nil
		}
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CSeq(expr, rest), nil

	case *aotir.ListSetStmt:
		// xs[i] = v  →  let [V_xs] = mochi_list:set(V_xs, I, V) in ...
		idxExpr, err := lowerExpr(l, s.Index)
		if err != nil {
			return nil, err
		}
		valExpr, err := lowerExpr(l, s.Value)
		if err != nil {
			return nil, err
		}
		setCall := cerl.CCall(cerl.CAtom("mochi_list"), cerl.CAtom("set"),
			[]cerl.Expr{cerl.CVar("V_" + s.Name), idxExpr, valExpr})
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + s.Name)}, setCall, rest), nil

	case *aotir.MapPutStmt:
		// m[k] = v  →  let [V_m] = V_m#{K => V} in ...
		keyExpr, err := lowerExpr(l, s.Key)
		if err != nil {
			return nil, err
		}
		valExpr, err := lowerExpr(l, s.Value)
		if err != nil {
			return nil, err
		}
		updateMap := cerl.CMap(cerl.CVar("V_"+s.Name),
			[]cerl.Expr{cerl.CMapPairAssoc(keyExpr, valExpr)}, false)
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + s.Name)}, updateMap, rest), nil

	case *aotir.ForEachStmt:
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return l.lowerForEachStmt(s, rest)

	case *aotir.MatchStmt:
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return l.lowerMatchStmt(s, rest)

	case *aotir.ClosureEnvStmt:
		// No-op for BEAM: env structs are a C-specific concern.
		// Captured variables are handled natively by Core Erlang c_fun.
		return l.lowerBlock(tail, cont)

	case *aotir.QueryScopeStmt:
		// Arena scoping is a C-specific concern. For BEAM, just lower the body
		// inline and thread the tail continuation through.
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return l.lowerBlock(s.Body.Statements, rest)

	case *aotir.AgentIntentCallStmt:
		// Phase 9.0: unit intent call → call helper function, rebind receiver with new state.
		return l.lowerAgentIntentCallStmt(s, tail, cont)

	case *aotir.WriteFileStmt:
		// Phase 12.0: writeFile(path, content) → mochi_file:write_file(Path, Content).
		path, err := lowerExpr(l, s.Path)
		if err != nil {
			return nil, err
		}
		content, err := lowerExpr(l, s.Content)
		if err != nil {
			return nil, err
		}
		call := cerl.CCall(cerl.CAtom("mochi_file"), cerl.CAtom("write_file"),
			[]cerl.Expr{path, content})
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V___")}, call, rest), nil

	case *aotir.AppendFileStmt:
		// Phase 12.0: appendFile(path, content) → mochi_file:append_file(Path, Content).
		path, err := lowerExpr(l, s.Path)
		if err != nil {
			return nil, err
		}
		content, err := lowerExpr(l, s.Content)
		if err != nil {
			return nil, err
		}
		call := cerl.CCall(cerl.CAtom("mochi_file"), cerl.CAtom("append_file"),
			[]cerl.Expr{path, content})
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V___")}, call, rest), nil

	case *aotir.ChanSendStmt:
		// Phase 10.1: send(chan, val) → mochi_chan:send(Chan, Val) then continue.
		ch, err := lowerExpr(l, s.Chan)
		if err != nil {
			return nil, err
		}
		val, err := lowerExpr(l, s.Val)
		if err != nil {
			return nil, err
		}
		sendCall := cerl.CCall(cerl.CAtom("mochi_chan"), cerl.CAtom("send"),
			[]cerl.Expr{ch, val})
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V___")}, sendCall, rest), nil

	case *aotir.StreamEmitStmt:
		// Phase 10.0: emit(stream, val) → mochi_stream:emit(Stream, Val) then continue.
		stream, err := lowerExpr(l, s.Stream)
		if err != nil {
			return nil, err
		}
		val, err := lowerExpr(l, s.Val)
		if err != nil {
			return nil, err
		}
		emitCall := cerl.CCall(cerl.CAtom("mochi_stream"), cerl.CAtom("emit"),
			[]cerl.Expr{stream, val})
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V___")}, emitCall, rest), nil

	case *aotir.PanicStmt:
		// Phase 13.1: panic(code, msg) → erlang:error({mochi_panic, Code, Msg})
		code, err := lowerExpr(l, s.Code)
		if err != nil {
			return nil, err
		}
		msg, err := lowerExpr(l, s.Msg)
		if err != nil {
			return nil, err
		}
		panicTuple := cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_panic"), code, msg})
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("error"), []cerl.Expr{panicTuple}), nil

	case *aotir.TryCatchStmt:
		// Phase 13.1: try { tryBody } catch e { catchBody }
		// Continuation (statements after this try-catch) feeds into both branches.
		rest, err := l.lowerBlock(tail, cont)
		if err != nil {
			return nil, err
		}
		tryExpr, err := l.lowerBlock(s.TryBody.Statements, rest)
		if err != nil {
			return nil, fmt.Errorf("beam/lower: TryCatchStmt try body: %w", err)
		}
		// Bind the catch variable, then lower the catch body.
		if l.scope == nil {
			l.scope = make(map[string]bool)
		}
		l.scope[s.CatchVar] = true
		catchExpr, err := l.lowerBlock(s.CatchBody.Statements, rest)
		delete(l.scope, s.CatchVar)
		if err != nil {
			return nil, fmt.Errorf("beam/lower: TryCatchStmt catch body: %w", err)
		}
		// Catch handler: match {mochi_panic, Code, _Msg} or re-raise.
		// Use unique names to avoid duplicate-evar errors in nested c_try nodes.
		tn := l.nextTryNum()
		tcls := fmt.Sprintf("V___cls%d", tn)
		texc := fmt.Sprintf("V___exc%d", tn)
		tstk := fmt.Sprintf("V___stk%d", tn)
		ttryval := fmt.Sprintf("V___tryval%d", tn)
		tother := fmt.Sprintf("V___other%d", tn)
		catchVar := "V_" + s.CatchVar
		handler := cerl.CCase(cerl.CVar(texc), []cerl.Expr{
			cerl.CClause(
				[]cerl.Expr{cerl.CTuple([]cerl.Expr{
					cerl.CAtom("mochi_panic"), cerl.CVar(catchVar), cerl.CVar("V___"),
				})},
				cerl.CAtom("true"),
				catchExpr,
			),
			cerl.CClause(
				[]cerl.Expr{cerl.CVar(tother)},
				cerl.CAtom("true"),
				cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("throw"), []cerl.Expr{
					cerl.CVar(tother),
				}),
			),
		})
		return cerl.CTry(
			tryExpr,
			[]cerl.Expr{cerl.CVar(ttryval)},
			cerl.CVar(ttryval),
			[]cerl.Expr{cerl.CVar(tcls), cerl.CVar(texc), cerl.CVar(tstk)},
			handler,
		), nil

	case *aotir.RawCStmt:
		// Phase 8.0: Datalog C setup code — the BEAM backend evaluates at compile time via
		// DatalogQueryExpr; skip the raw C statement.
		return l.lowerBlock(tail, cont)

	default:
		return nil, fmt.Errorf("beam/lower: unsupported statement %T", head)
	}
}

// loopStateExpr builds a c_tuple of the current values of loop params.
func (l *lowerer) loopStateExpr(params []string) cerl.Expr {
	elems := make([]cerl.Expr, len(params))
	for i, p := range params {
		elems[i] = cerl.CVar("V_" + p)
	}
	return cerl.CTuple(elems)
}

// loopStateVars builds pattern variables for destructuring the state tuple.
func loopStateVars(params []string, suffix string) []cerl.Expr {
	elems := make([]cerl.Expr, len(params))
	for i, p := range params {
		elems[i] = cerl.CVar("V_" + p + suffix)
	}
	return elems
}

// lowerIfStmt lowers an IfStmt with no continuation (result is the branch value).
func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (cerl.Expr, error) {
	return l.lowerIfStmtWithCont(s, nil)
}

// lowerIfStmtWithCont lowers an IfStmt threading cont into each branch so that
// variable updates inside a branch are in scope for cont.
func (l *lowerer) lowerIfStmtWithCont(s *aotir.IfStmt, cont cerl.Expr) (cerl.Expr, error) {
	cond, err := lowerExpr(l, s.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := l.lowerBlock(s.Then.Statements, cont)
	if err != nil {
		return nil, err
	}
	var elseExpr cerl.Expr
	if s.Else != nil {
		elseExpr, err = l.lowerBlock(s.Else.Statements, cont)
		if err != nil {
			return nil, err
		}
	} else {
		if cont != nil {
			elseExpr = cont
		} else {
			elseExpr = cerl.CAtom("ok")
		}
	}
	return cerl.CCase(cond, []cerl.Expr{
		cerl.CClause([]cerl.Expr{cerl.CAtom("true")}, cerl.CAtom("true"), thenExpr),
		cerl.CClause([]cerl.Expr{cerl.CAtom("false")}, cerl.CAtom("true"), elseExpr),
	}), nil
}

// lowerWhileStmt emits a tail-recursive helper '__while_N/k' into the module
// and returns a call to it. Updated loop variable values are scoped into cont.
func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt, cont cerl.Expr) (cerl.Expr, error) {
	n := l.nextLoopNum()

	// Compute loop params: outer vars referenced or assigned in the loop.
	params := l.loopParams(s.Cond, s.Body.Statements)
	helperName := fmt.Sprintf("__while_%d", n)
	helperArity := len(params)

	// Push loop context.
	l.loopStack = append(l.loopStack, loopCtx{num: n, params: params})

	cond, err := lowerExpr(l, s.Cond)
	if err != nil {
		l.loopStack = l.loopStack[:len(l.loopStack)-1]
		return nil, err
	}

	// The body's continuation is a recursive call to the helper with current param values.
	recurseCall := cerl.CApply(cerl.CVarFunc(helperName, helperArity), l.loopParamVarExprs(params))
	bodyExpr, err := l.lowerBlock(s.Body.Statements, recurseCall)
	l.loopStack = l.loopStack[:len(l.loopStack)-1]
	if err != nil {
		return nil, err
	}

	// Wrap body with continue handler.
	contPatVars := loopStateVars(params, "__c")
	contPat := cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_continue"), cerl.CInt(int64(n)), cerl.CTuple(contPatVars)})
	contRecurse := cerl.CApply(cerl.CVarFunc(helperName, helperArity), contPatVarExprs(contPatVars))

	breakPatVars := loopStateVars(params, "__b")
	breakPat := cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_break"), cerl.CInt(int64(n)), cerl.CTuple(breakPatVars)})
	breakResult := l.loopParamTupleOrOk(breakPatVars)

	excHandler := cerl.CCase(cerl.CVar("V___rsn"), []cerl.Expr{
		cerl.CClause([]cerl.Expr{contPat}, cerl.CAtom("true"), contRecurse),
		cerl.CClause([]cerl.Expr{breakPat}, cerl.CAtom("true"), breakResult),
		cerl.CClause([]cerl.Expr{cerl.CVar("V___")}, cerl.CAtom("true"),
			cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("raise"), []cerl.Expr{
				cerl.CVar("V___cls"), cerl.CVar("V___rsn"), cerl.CVar("V___stk"),
			})),
	})

	bodyWithHandlers := cerl.CTry(
		bodyExpr,
		[]cerl.Expr{cerl.CVar("V___r")}, cerl.CVar("V___r"),
		[]cerl.Expr{cerl.CVar("V___cls"), cerl.CVar("V___rsn"), cerl.CVar("V___stk")},
		excHandler,
	)

	// The 'false' branch returns the final values of loop params.
	falseResult := l.loopParamTupleOrOk(l.loopParamVarExprs(params))

	helperBody := cerl.CCase(cond, []cerl.Expr{
		cerl.CClause([]cerl.Expr{cerl.CAtom("true")}, cerl.CAtom("true"), bodyWithHandlers),
		cerl.CClause([]cerl.Expr{cerl.CAtom("false")}, cerl.CAtom("true"), falseResult),
	})

	helperVars := make([]string, len(params))
	for i, p := range params {
		helperVars[i] = "V_" + p
	}
	l.mod.Defs = append(l.mod.Defs, cerl.FuncDef{
		Name:  helperName,
		Arity: helperArity,
		Vars:  helperVars,
		Body:  helperBody,
	})

	// Call site: call helper and scope updated loop var values into cont.
	initCall := cerl.CApply(cerl.CVarFunc(helperName, helperArity), l.loopParamVarExprs(params))
	return l.bindLoopResultWithCont(params, initCall, cont), nil
}

// lowerForRangeStmt emits '__for_range_N/k+2' and returns a call to it.
// k = number of loop params; the +2 are V_x (induction var) and V_end.
// Updated loop variable values are scoped into cont.
func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt, cont cerl.Expr) (cerl.Expr, error) {
	n := l.nextLoopNum()

	// The for-range induction variable is the loop var plus outer mutated vars.
	params := l.loopParams(nil, s.Body.Statements)
	// Remove the induction variable from params (it's a separate parameter).
	params = removeFrom(params, s.Var)

	helperName := fmt.Sprintf("__for_range_%d", n)
	varX := "V_" + s.Var
	varEnd := fmt.Sprintf("V___end_%d", n)
	// Helper arity: induction var + end var + outer params
	helperArity := 2 + len(params)

	l.loopStack = append(l.loopStack, loopCtx{num: n, params: append([]string{s.Var}, params...)})

	// Add induction var to scope for the body.
	if l.scope == nil {
		l.scope = make(map[string]bool)
	}
	l.scope[s.Var] = true

	// The body's continuation: increment V_x and recurse.
	nextX := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("+"),
		[]cerl.Expr{cerl.CVar(varX), cerl.CInt(1)})
	allParamExprs := append([]cerl.Expr{nextX, cerl.CVar(varEnd)}, l.loopParamVarExprs(params)...)
	recurseCall := cerl.CApply(cerl.CVarFunc(helperName, helperArity), allParamExprs)

	bodyExpr, err := l.lowerBlock(s.Body.Statements, recurseCall)

	delete(l.scope, s.Var)
	l.loopStack = l.loopStack[:len(l.loopStack)-1]
	if err != nil {
		return nil, err
	}

	// All loop params for this loop (induction var + outer params).
	allParams := append([]string{s.Var}, params...)

	// Continue handler: increment induction var and recurse.
	contPatVars := loopStateVars(allParams, "__c")
	contPat := cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_continue"), cerl.CInt(int64(n)), cerl.CTuple(contPatVars)})
	// On continue: V_x_cont is the NEXT value (the one that was being processed).
	// We increment it before recursing.
	nextXCont := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("+"),
		[]cerl.Expr{contPatVars[0], cerl.CInt(1)})
	contAllArgs := append([]cerl.Expr{nextXCont, cerl.CVar(varEnd)}, contPatVarExprs(contPatVars[1:])...)
	contRecurse := cerl.CApply(cerl.CVarFunc(helperName, helperArity), contAllArgs)

	// Break handler: extract state and return outer params.
	breakPatVars := loopStateVars(allParams, "__b")
	breakPat := cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_break"), cerl.CInt(int64(n)), cerl.CTuple(breakPatVars)})
	breakResult := l.loopParamTupleOrOk(breakPatVars[1:]) // skip induction var

	excHandler := cerl.CCase(cerl.CVar("V___rsn"), []cerl.Expr{
		cerl.CClause([]cerl.Expr{contPat}, cerl.CAtom("true"), contRecurse),
		cerl.CClause([]cerl.Expr{breakPat}, cerl.CAtom("true"), breakResult),
		cerl.CClause([]cerl.Expr{cerl.CVar("V___")}, cerl.CAtom("true"),
			cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("raise"), []cerl.Expr{
				cerl.CVar("V___cls"), cerl.CVar("V___rsn"), cerl.CVar("V___stk"),
			})),
	})

	bodyWithHandlers := cerl.CTry(
		bodyExpr,
		[]cerl.Expr{cerl.CVar("V___r")}, cerl.CVar("V___r"),
		[]cerl.Expr{cerl.CVar("V___cls"), cerl.CVar("V___rsn"), cerl.CVar("V___stk")},
		excHandler,
	)

	// cond: V_x >= V_end
	geExpr := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom(">="),
		[]cerl.Expr{cerl.CVar(varX), cerl.CVar(varEnd)})

	// false branch returns outer params (not induction var).
	falseResult := l.loopParamTupleOrOk(l.loopParamVarExprs(params))

	helperBody := cerl.CCase(geExpr, []cerl.Expr{
		cerl.CClause([]cerl.Expr{cerl.CAtom("true")}, cerl.CAtom("true"), falseResult),
		cerl.CClause([]cerl.Expr{cerl.CAtom("false")}, cerl.CAtom("true"), bodyWithHandlers),
	})

	helperVars := make([]string, 2+len(params))
	helperVars[0] = varX
	helperVars[1] = varEnd
	for i, p := range params {
		helperVars[2+i] = "V_" + p
	}
	l.mod.Defs = append(l.mod.Defs, cerl.FuncDef{
		Name:  helperName,
		Arity: helperArity,
		Vars:  helperVars,
		Body:  helperBody,
	})

	startExpr, err := lowerExpr(l, s.Start)
	if err != nil {
		return nil, err
	}
	endExpr, err := lowerExpr(l, s.End)
	if err != nil {
		return nil, err
	}

	initArgs := append([]cerl.Expr{startExpr, endExpr}, l.loopParamVarExprs(params)...)
	initCall := cerl.CApply(cerl.CVarFunc(helperName, helperArity), initArgs)
	return l.bindLoopResultWithCont(params, initCall, cont), nil
}

// lowerForEachStmt emits a tail-recursive '__for_each_N/1+k' helper for
// `for x in xs { ... }` and returns a call to it, scoping updated loop
// variable values into cont.
//
// The helper matches on [] (base case) or [H|T] (recursive case).
func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt, cont cerl.Expr) (cerl.Expr, error) {
	n := l.nextLoopNum()

	// Outer mutable vars referenced/assigned in the body (exclude the induction var).
	params := l.loopParams(nil, s.Body.Statements)
	params = removeFrom(params, s.Var)

	helperName := fmt.Sprintf("__for_each_%d", n)
	varX := "V_" + s.Var
	varRest := fmt.Sprintf("V___rest_%d", n)
	varList := fmt.Sprintf("V___list_%d", n)
	helperArity := 1 + len(params) // V_list + outer params

	// Push loop context so break/continue work.
	allLoopParams := append([]string{s.Var}, params...)
	l.loopStack = append(l.loopStack, loopCtx{num: n, params: allLoopParams})

	// Add induction var to scope for the body.
	if l.scope == nil {
		l.scope = make(map[string]bool)
	}
	l.scope[s.Var] = true

	// Body continuation: recurse with the rest of the list and updated params.
	recurseArgs := append([]cerl.Expr{cerl.CVar(varRest)}, l.loopParamVarExprs(params)...)
	recurseCall := cerl.CApply(cerl.CVarFunc(helperName, helperArity), recurseArgs)

	bodyExpr, err := l.lowerBlock(s.Body.Statements, recurseCall)

	delete(l.scope, s.Var)
	l.loopStack = l.loopStack[:len(l.loopStack)-1]
	if err != nil {
		return nil, err
	}

	// Continue handler: advance to next element (V_rest) with state from exception.
	contPatVars := loopStateVars(allLoopParams, "__c")
	contPat := cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_continue"), cerl.CInt(int64(n)), cerl.CTuple(contPatVars)})
	// On continue: use params from state, rest from outer scope.
	contArgs := append([]cerl.Expr{cerl.CVar(varRest)}, contPatVarExprs(contPatVars[1:])...)
	contRecurse := cerl.CApply(cerl.CVarFunc(helperName, helperArity), contArgs)

	// Break handler: return outer params (skip induction var).
	breakPatVars := loopStateVars(allLoopParams, "__b")
	breakPat := cerl.CTuple([]cerl.Expr{cerl.CAtom("mochi_break"), cerl.CInt(int64(n)), cerl.CTuple(breakPatVars)})
	breakResult := l.loopParamTupleOrOk(breakPatVars[1:]) // skip induction var

	excHandler := cerl.CCase(cerl.CVar("V___rsn"), []cerl.Expr{
		cerl.CClause([]cerl.Expr{contPat}, cerl.CAtom("true"), contRecurse),
		cerl.CClause([]cerl.Expr{breakPat}, cerl.CAtom("true"), breakResult),
		cerl.CClause([]cerl.Expr{cerl.CVar("V___")}, cerl.CAtom("true"),
			cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("raise"), []cerl.Expr{
				cerl.CVar("V___cls"), cerl.CVar("V___rsn"), cerl.CVar("V___stk"),
			})),
	})

	bodyWithHandlers := cerl.CTry(
		bodyExpr,
		[]cerl.Expr{cerl.CVar("V___r")}, cerl.CVar("V___r"),
		[]cerl.Expr{cerl.CVar("V___cls"), cerl.CVar("V___rsn"), cerl.CVar("V___stk")},
		excHandler,
	)

	// Base case: empty list → return final state of outer params.
	emptyResult := l.loopParamTupleOrOk(l.loopParamVarExprs(params))

	// Non-empty case: bind head to V_var, rest to varRest.
	nonEmptyPat := cerl.CCons(cerl.CVar(varX), cerl.CVar(varRest))

	helperBody := cerl.CCase(cerl.CVar(varList), []cerl.Expr{
		cerl.CClause([]cerl.Expr{cerl.CNil()}, cerl.CAtom("true"), emptyResult),
		cerl.CClause([]cerl.Expr{nonEmptyPat}, cerl.CAtom("true"), bodyWithHandlers),
	})

	helperVars := make([]string, 1+len(params))
	helperVars[0] = varList
	for i, p := range params {
		helperVars[1+i] = "V_" + p
	}
	l.mod.Defs = append(l.mod.Defs, cerl.FuncDef{
		Name:  helperName,
		Arity: helperArity,
		Vars:  helperVars,
		Body:  helperBody,
	})

	// Evaluate the list expression once, then call the helper.
	listExpr, err := lowerExpr(l, s.List)
	if err != nil {
		return nil, err
	}
	initArgs := append([]cerl.Expr{listExpr}, l.loopParamVarExprs(params)...)
	initCall := cerl.CApply(cerl.CVarFunc(helperName, helperArity), initArgs)
	return l.bindLoopResultWithCont(params, initCall, cont), nil
}

// loopParams computes the set of outer-scope variables referenced or
// assigned in the loop cond (may be nil) and body.
func (l *lowerer) loopParams(cond aotir.Expr, body []aotir.Stmt) []string {
	if l.scope == nil {
		return nil
	}
	seen := make(map[string]bool)

	if cond != nil {
		for _, v := range collectExprVarRefs(cond) {
			if l.scope[v] {
				seen[v] = true
			}
		}
	}
	for _, v := range collectStmtVarRefs(body) {
		if l.scope[v] {
			seen[v] = true
		}
	}
	for _, v := range collectAssignedVars(body) {
		if l.scope[v] {
			seen[v] = true
		}
	}

	params := make([]string, 0, len(seen))
	for v := range seen {
		params = append(params, v)
	}
	sort.Strings(params)
	return params
}

// loopParamVarExprs returns c_var expressions for each loop param.
func (l *lowerer) loopParamVarExprs(params []string) []cerl.Expr {
	exprs := make([]cerl.Expr, len(params))
	for i, p := range params {
		exprs[i] = cerl.CVar("V_" + p)
	}
	return exprs
}

// loopParamTupleOrOk returns a c_tuple of exprs if len>0, else c_atom("ok").
func (l *lowerer) loopParamTupleOrOk(exprs []cerl.Expr) cerl.Expr {
	if len(exprs) == 0 {
		return cerl.CAtom("ok")
	}
	if len(exprs) == 1 {
		return exprs[0]
	}
	return cerl.CTuple(exprs)
}

// bindLoopResultWithCont binds the returned loop state into cont so that
// updated loop variable values are in scope for subsequent code.
//
// For 0 params: seq(call, cont) if cont is non-trivial, else just call.
// For 1 param: let [V_p] = call in cont.
// For N params: helper returns {p1,...,pN} tuple; destructure via case.
func (l *lowerer) bindLoopResultWithCont(params []string, call cerl.Expr, cont cerl.Expr) cerl.Expr {
	if cont == nil {
		cont = cerl.CAtom("ok")
	}
	if len(params) == 0 {
		return cerl.CSeq(call, cont)
	}
	if len(params) == 1 {
		return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + params[0])}, call, cont)
	}
	// For N>1 params: the helper returns a tuple {p1,...,pN}.
	// c_let with multiple vars expects c_values, not a tuple,
	// so destructure with c_case instead.
	patVars := make([]cerl.Expr, len(params))
	for i, p := range params {
		patVars[i] = cerl.CVar("V_" + p)
	}
	return cerl.CLet(
		[]cerl.Expr{cerl.CVar("V___loopres")},
		call,
		cerl.CCase(cerl.CVar("V___loopres"), []cerl.Expr{
			cerl.CClause([]cerl.Expr{cerl.CTuple(patVars)}, cerl.CAtom("true"), cont),
		}),
	)
}

// contPatVarExprs returns just the expr form of pattern vars.
func contPatVarExprs(pvs []cerl.Expr) []cerl.Expr {
	return pvs
}

// collectExprVarRefs returns all variable names referenced in an expression.
func collectExprVarRefs(expr aotir.Expr) []string {
	if expr == nil {
		return nil
	}
	var names []string
	switch e := expr.(type) {
	case *aotir.VarRef:
		names = append(names, e.Name)
	case *aotir.BinaryExpr:
		names = append(names, collectExprVarRefs(e.Left)...)
		names = append(names, collectExprVarRefs(e.Right)...)
	case *aotir.UnaryExpr:
		names = append(names, collectExprVarRefs(e.Operand)...)
	case *aotir.CallExpr:
		for _, a := range e.Args {
			names = append(names, collectExprVarRefs(a)...)
		}
	case *aotir.AppendExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
		names = append(names, collectExprVarRefs(e.Value)...)
	case *aotir.IndexExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
		names = append(names, collectExprVarRefs(e.Index)...)
	case *aotir.LenExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	case *aotir.MapGetExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
		names = append(names, collectExprVarRefs(e.Key)...)
	case *aotir.MapHasExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
		names = append(names, collectExprVarRefs(e.Key)...)
	case *aotir.MapLenExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	case *aotir.MapKeysExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	case *aotir.MapValuesExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	case *aotir.ListSumExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	case *aotir.ListMinExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	case *aotir.ListMaxExpr:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	case *aotir.ListContainsExpr:
		names = append(names, collectExprVarRefs(e.List)...)
		names = append(names, collectExprVarRefs(e.Value)...)
	case *aotir.FieldAccess:
		names = append(names, collectExprVarRefs(e.Receiver)...)
	}
	return names
}

// collectStmtVarRefs returns all variable names READ in statements.
func collectStmtVarRefs(stmts []aotir.Stmt) []string {
	var names []string
	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *aotir.LetStmt:
			names = append(names, collectExprVarRefs(s.Init)...)
		case *aotir.AssignStmt:
			names = append(names, collectExprVarRefs(s.Value)...)
		case *aotir.CallStmt:
			for _, a := range s.Args {
				names = append(names, collectExprVarRefs(a)...)
			}
		case *aotir.IfStmt:
			names = append(names, collectExprVarRefs(s.Cond)...)
			if s.Then != nil {
				names = append(names, collectStmtVarRefs(s.Then.Statements)...)
			}
			if s.Else != nil {
				names = append(names, collectStmtVarRefs(s.Else.Statements)...)
			}
		case *aotir.WhileStmt:
			names = append(names, collectExprVarRefs(s.Cond)...)
			names = append(names, collectStmtVarRefs(s.Body.Statements)...)
		case *aotir.ForRangeStmt:
			names = append(names, collectExprVarRefs(s.Start)...)
			names = append(names, collectExprVarRefs(s.End)...)
			names = append(names, collectStmtVarRefs(s.Body.Statements)...)
		case *aotir.ReturnStmt:
			if s.Value != nil {
				names = append(names, collectExprVarRefs(s.Value)...)
			}
		case *aotir.MapPutStmt:
			names = append(names, s.Name)
			names = append(names, collectExprVarRefs(s.Key)...)
			names = append(names, collectExprVarRefs(s.Value)...)
		case *aotir.ForEachStmt:
			names = append(names, collectExprVarRefs(s.List)...)
			names = append(names, collectStmtVarRefs(s.Body.Statements)...)
		}
	}
	return names
}

// collectAssignedVars returns all variable names that are assigned (AssignStmt)
// in the given statements (shallowly - does not recurse into nested loops).
func collectAssignedVars(stmts []aotir.Stmt) []string {
	var names []string
	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *aotir.AssignStmt:
			names = append(names, s.Name)
		case *aotir.MapPutStmt:
			names = append(names, s.Name)
		case *aotir.IfStmt:
			if s.Then != nil {
				names = append(names, collectAssignedVars(s.Then.Statements)...)
			}
			if s.Else != nil {
				names = append(names, collectAssignedVars(s.Else.Statements)...)
			}
		}
	}
	return names
}

func removeFrom(ss []string, s string) []string {
	result := ss[:0:0]
	for _, v := range ss {
		if v != s {
			result = append(result, v)
		}
	}
	return result
}

// lowerCallStmt lowers a CallStmt.
func lowerCallStmt(l *lowerer, s *aotir.CallStmt) (cerl.Expr, error) {
	switch s.Func {
	case "mochi_print_str":
		return lowerPrintStr(l, s.Args)
	case "mochi_print_i64":
		return lowerPrintInt(l, s.Args)
	case "mochi_print_f64":
		return lowerPrintFloat(l, s.Args)
	case "mochi_print_bool":
		return lowerPrintBool(l, s.Args)
	default:
		args := make([]cerl.Expr, len(s.Args))
		for i, a := range s.Args {
			e, err := lowerExpr(l, a)
			if err != nil {
				return nil, err
			}
			args[i] = e
		}
		return cerl.CApply(cerl.CVarFunc(s.Func, len(s.Args)), args), nil
	}
}

func lowerPrintStr(l *lowerer, args []aotir.Expr) (cerl.Expr, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("beam/lower: mochi_print_str wants 1 arg, got %d", len(args))
	}
	arg, err := lowerExpr(l, args[0])
	if err != nil {
		return nil, err
	}
	argWithNewline := cerl.CCons(arg, cerl.CCons(cerl.CInt(10), cerl.CNil()))
	return cerl.CCall(cerl.CAtom("io"), cerl.CAtom("put_chars"), []cerl.Expr{argWithNewline}), nil
}

func lowerPrintInt(l *lowerer, args []aotir.Expr) (cerl.Expr, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("beam/lower: mochi_print_i64 wants 1 arg, got %d", len(args))
	}
	arg, err := lowerExpr(l, args[0])
	if err != nil {
		return nil, err
	}
	bin := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("integer_to_binary"), []cerl.Expr{arg})
	argWithNewline := cerl.CCons(bin, cerl.CCons(cerl.CInt(10), cerl.CNil()))
	return cerl.CCall(cerl.CAtom("io"), cerl.CAtom("put_chars"), []cerl.Expr{argWithNewline}), nil
}

func lowerPrintFloat(l *lowerer, args []aotir.Expr) (cerl.Expr, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("beam/lower: mochi_print_f64 wants 1 arg, got %d", len(args))
	}
	arg, err := lowerExpr(l, args[0])
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("print_float"), []cerl.Expr{arg}), nil
}

func lowerPrintBool(l *lowerer, args []aotir.Expr) (cerl.Expr, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("beam/lower: mochi_print_bool wants 1 arg, got %d", len(args))
	}
	arg, err := lowerExpr(l, args[0])
	if err != nil {
		return nil, err
	}
	return cerl.CCase(arg, []cerl.Expr{
		cerl.CClause([]cerl.Expr{cerl.CAtom("true")}, cerl.CAtom("true"),
			cerl.CCall(cerl.CAtom("io"), cerl.CAtom("put_chars"), []cerl.Expr{cerl.CBin([]byte("true\n"))})),
		cerl.CClause([]cerl.Expr{cerl.CAtom("false")}, cerl.CAtom("true"),
			cerl.CCall(cerl.CAtom("io"), cerl.CAtom("put_chars"), []cerl.Expr{cerl.CBin([]byte("false\n"))})),
	}), nil
}

// lowerExpr lowers one aotir expression to a cerl expression.
func lowerExpr(l *lowerer, expr aotir.Expr) (cerl.Expr, error) {
	switch e := expr.(type) {
	case *aotir.StringLit:
		return cerl.CBin([]byte(e.Value)), nil
	case *aotir.IntLit:
		return cerl.CInt(e.Value), nil
	case *aotir.FloatLit:
		return cerl.CFloat(e.Value), nil
	case *aotir.BoolLit:
		return cerl.CBool(e.Value), nil
	case *aotir.VarRef:
		// Phase 9.0: agent field reads use "__self->field" emitName encoding.
		if strings.HasPrefix(e.Name, "__self->") {
			field := strings.TrimPrefix(e.Name, "__self->")
			return cerl.CCall(cerl.CAtom("maps"), cerl.CAtom("get"),
				[]cerl.Expr{cerl.CAtom(field), cerl.CVar("V___self")}), nil
		}
		// Captured closure variables use "__e->fieldname" as VarRef.Name
		// in the C backend (emitName encoding). Strip the env-struct prefix
		// for BEAM; captured vars are naturally in scope via c_fun.
		return cerl.CVar("V_" + strings.TrimPrefix(e.Name, "__e->")), nil
	case *aotir.BinaryExpr:
		return lowerBinaryExpr(l, e)
	case *aotir.UnaryExpr:
		return lowerUnaryExpr(l, e)
	case *aotir.CallExpr:
		return lowerCallExpr(l, e)

	// Phase 3.1: list expressions
	case *aotir.ListLit:
		return lowerListLit(l, e)
	case *aotir.IndexExpr:
		return lowerIndexExpr(l, e)
	case *aotir.LenExpr:
		return lowerLenExpr(l, e)
	case *aotir.AppendExpr:
		return lowerAppendExpr(l, e)
	case *aotir.ListContainsExpr:
		return lowerListContainsExpr(l, e)
	case *aotir.ListSumExpr:
		return lowerListSumExpr(l, e)
	case *aotir.ListMinExpr:
		return lowerListMinExpr(l, e)
	case *aotir.ListMaxExpr:
		return lowerListMaxExpr(l, e)

	// Phase 3.2: map expressions
	case *aotir.MapLit:
		return lowerMapLit(l, e)
	case *aotir.MapGetExpr:
		return lowerMapGetExpr(l, e)
	case *aotir.MapHasExpr:
		return lowerMapHasExpr(l, e)
	case *aotir.MapLenExpr:
		return lowerMapLenExpr(l, e)
	case *aotir.MapKeysExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("maps"), cerl.CAtom("keys"), []cerl.Expr{recv}), nil
	case *aotir.MapValuesExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("maps"), cerl.CAtom("values"), []cerl.Expr{recv}), nil

	// Phase 4.0: record construction and field access
	case *aotir.RecordLit:
		return lowerRecordLit(l, e)
	case *aotir.FieldAccess:
		return lowerFieldAccess(l, e)

	// Phase 5.0: sum type construction and field access
	case *aotir.VariantLit:
		return lowerVariantLit(l, e)
	case *aotir.VariantFieldAccess:
		return lowerVariantFieldAccess(l, e)
	case *aotir.UnionVarRef:
		return cerl.CVar("V_" + e.Name), nil

	// Phase 6.0: closures and higher-order function calls
	case *aotir.FunLit:
		return l.lowerFunLit(e)
	case *aotir.FunCallExpr:
		return l.lowerFunCallExpr(e)

	// Phase 7.0: query DSL helpers
	case *aotir.ListSortAscExpr:
		return lowerListSortAscExpr(l, e)
	case *aotir.ListSliceExpr:
		return lowerListSliceExpr(l, e)

	// Phase 9.0: agents as functional state-threaded maps
	case *aotir.AgentLit:
		return lowerAgentLit(l, e)
	case *aotir.AgentIntentCallExpr:
		return l.lowerAgentIntentCallExpr(e)

	// Phase 13.0: string operations
	case *aotir.StrLenExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		// string:length/1 counts Unicode codepoints (same as Mochi len).
		return cerl.CCall(cerl.CAtom("string"), cerl.CAtom("length"), []cerl.Expr{recv}), nil
	case *aotir.StrIndexExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := lowerExpr(l, e.Index)
		if err != nil {
			return nil, err
		}
		// mochi_str:index(S, I) — returns single-codepoint binary.
		return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("index"), []cerl.Expr{recv, idx}), nil
	case *aotir.StrContainsExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		sub, err := lowerExpr(l, e.Sub)
		if err != nil {
			return nil, err
		}
		// binary:match/2 returns nomatch or {Start,Len}; convert to bool.
		match := cerl.CCall(cerl.CAtom("binary"), cerl.CAtom("match"), []cerl.Expr{recv, sub})
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("=/="),
			[]cerl.Expr{match, cerl.CAtom("nomatch")}), nil
	case *aotir.StrSubstringExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		start, err := lowerExpr(l, e.Start)
		if err != nil {
			return nil, err
		}
		end, err := lowerExpr(l, e.End)
		if err != nil {
			return nil, err
		}
		// mochi_str:substring(S, Start, End) — codepoint-indexed slice.
		return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("substring"),
			[]cerl.Expr{recv, start, end}), nil
	case *aotir.StrReverseExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("reverse"), []cerl.Expr{recv}), nil
	case *aotir.StrConvertExpr:
		operand, err := lowerExpr(l, e.Operand)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("convert"), []cerl.Expr{operand}), nil
	case *aotir.StrUpperExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("string"), cerl.CAtom("uppercase"), []cerl.Expr{recv}), nil
	case *aotir.StrLowerExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("string"), cerl.CAtom("lowercase"), []cerl.Expr{recv}), nil
	case *aotir.StrSplitExpr:
		str, err := lowerExpr(l, e.Str)
		if err != nil {
			return nil, err
		}
		sep, err := lowerExpr(l, e.Sep)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("split"), []cerl.Expr{str, sep}), nil
	case *aotir.StrJoinExpr:
		list, err := lowerExpr(l, e.List)
		if err != nil {
			return nil, err
		}
		sep, err := lowerExpr(l, e.Sep)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("join"), []cerl.Expr{list, sep}), nil

	// Phase 13.0: math builtins
	case *aotir.MathCallExpr:
		arg, err := lowerExpr(l, e.Arg)
		if err != nil {
			return nil, err
		}
		switch e.Func {
		case "abs_i64":
			return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("abs"), []cerl.Expr{arg}), nil
		case "abs_f64":
			return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("abs"), []cerl.Expr{arg}), nil
		case "floor":
			return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("floor"), []cerl.Expr{arg}), nil
		case "ceil":
			return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("ceil"), []cerl.Expr{arg}), nil
		default:
			return nil, fmt.Errorf("beam/lower: unsupported MathCallExpr func %q", e.Func)
		}

	// Phase 13.0: numeric type cast
	case *aotir.NumCastExpr:
		operand, err := lowerExpr(l, e.Operand)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("trunc"), []cerl.Expr{operand}), nil

	// Phase 13.0: list aggregates
	case *aotir.ListMinExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("min"), []cerl.Expr{recv}), nil
	case *aotir.ListMaxExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("max"), []cerl.Expr{recv}), nil
	case *aotir.ListContainsExpr:
		list, err := lowerExpr(l, e.List)
		if err != nil {
			return nil, err
		}
		val, err := lowerExpr(l, e.Value)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("member"), []cerl.Expr{val, list}), nil
	case *aotir.ListSumExpr:
		recv, err := lowerExpr(l, e.Receiver)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("sum"), []cerl.Expr{recv}), nil

	// Phase 12.0: file I/O via mochi_file runtime
	case *aotir.ReadFileExpr:
		path, err := lowerExpr(l, e.Path)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_file"), cerl.CAtom("read_file"),
			[]cerl.Expr{path}), nil
	case *aotir.LinesExpr:
		path, err := lowerExpr(l, e.Path)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_file"), cerl.CAtom("lines"),
			[]cerl.Expr{path}), nil

	// Phase 10.1: channels via mochi_chan runtime
	case *aotir.ChanMakeExpr:
		cap, err := lowerExpr(l, e.Cap)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_chan"), cerl.CAtom("make_chan"),
			[]cerl.Expr{cap}), nil
	case *aotir.ChanRecvExpr:
		ch, err := lowerExpr(l, e.Chan)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_chan"), cerl.CAtom("recv"),
			[]cerl.Expr{ch}), nil

	// Phase 10.0: streams via mochi_stream runtime
	case *aotir.StreamMakeExpr:
		cap, err := lowerExpr(l, e.Cap)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_stream"), cerl.CAtom("make_stream"),
			[]cerl.Expr{cap}), nil
	case *aotir.SubMakeExpr:
		stream, err := lowerExpr(l, e.Stream)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_stream"), cerl.CAtom("subscribe"),
			[]cerl.Expr{stream}), nil
	case *aotir.SubRecvExpr:
		sub, err := lowerExpr(l, e.Sub)
		if err != nil {
			return nil, err
		}
		return cerl.CCall(cerl.CAtom("mochi_stream"), cerl.CAtom("recv_sub"),
			[]cerl.Expr{sub}), nil

	// Phase 13.0: LLM generate → mochi_llm:generate/3
	case *aotir.LLMGenerateExpr:
		return lowerLLMGenerateExpr(l, e)

	// Phase 8.0: Datalog compile-time evaluation.
	case *aotir.DatalogQueryExpr:
		return lowerDatalogQueryExpr(e)

	// str(x) builtin: convert scalar to binary string.
	case *aotir.StrConvertExpr:
		return lowerStrConvertExpr(l, e)

	default:
		return nil, fmt.Errorf("beam/lower: unsupported expression %T", expr)
	}
}

// lowerStrConvertExpr lowers str(x) to erlang:integer_to_binary/1,
// erlang:float_to_binary/2, or identity for bool/string.
func lowerStrConvertExpr(l *lowerer, e *aotir.StrConvertExpr) (cerl.Expr, error) {
	operand, err := lowerExpr(l, e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Operand.Type() {
	case aotir.TypeInt:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("integer_to_binary"),
			[]cerl.Expr{operand}), nil
	case aotir.TypeFloat:
		// float_to_binary(X, [{decimals, 10}, compact]) mirrors mochi_str_from_f64 behaviour.
		opts := cerl.CCons(
			cerl.CTuple([]cerl.Expr{cerl.CAtom("decimals"), cerl.CInt(10)}),
			cerl.CCons(cerl.CAtom("compact"), cerl.CNil()),
		)
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("float_to_binary"),
			[]cerl.Expr{operand, opts}), nil
	case aotir.TypeBool:
		// true → <<"true">>, false → <<"false">>
		return cerl.CCase(operand, []cerl.Expr{
			cerl.CClause([]cerl.Expr{cerl.CAtom("true")}, cerl.CAtom("true"), cerl.CBin([]byte("true"))),
			cerl.CClause([]cerl.Expr{cerl.CAtom("false")}, cerl.CAtom("true"), cerl.CBin([]byte("false"))),
		}), nil
	default: // TypeString: identity
		return operand, nil
	}
}

// lowerListLit lowers [e1, e2, ...] to a CCons chain.
func lowerListLit(l *lowerer, e *aotir.ListLit) (cerl.Expr, error) {
	result := cerl.Expr(cerl.CNil())
	for i := len(e.Elems) - 1; i >= 0; i-- {
		elem, err := lowerExpr(l, e.Elems[i])
		if err != nil {
			return nil, err
		}
		result = cerl.CCons(elem, result)
	}
	return result, nil
}

// lowerIndexExpr lowers xs[i] to lists:nth(I+1, L) (0-indexed Mochi to 1-indexed Erlang).
func lowerIndexExpr(l *lowerer, e *aotir.IndexExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := lowerExpr(l, e.Index)
	if err != nil {
		return nil, err
	}
	oneIdx := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("+"), []cerl.Expr{idx, cerl.CInt(1)})
	return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("nth"), []cerl.Expr{oneIdx, recv}), nil
}

// lowerLenExpr lowers len(xs) to erlang:length(L).
func lowerLenExpr(l *lowerer, e *aotir.LenExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("length"), []cerl.Expr{recv}), nil
}

// lowerAppendExpr lowers append(xs, v) to erlang:'++'(L, [V]).
func lowerAppendExpr(l *lowerer, e *aotir.AppendExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	val, err := lowerExpr(l, e.Value)
	if err != nil {
		return nil, err
	}
	singleton := cerl.CCons(val, cerl.CNil())
	return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("++"), []cerl.Expr{recv, singleton}), nil
}

// lowerMapLit lowers {k1: v1, k2: v2} to a Core Erlang map literal.
func lowerMapLit(l *lowerer, e *aotir.MapLit) (cerl.Expr, error) {
	pairs := make([]cerl.Expr, len(e.Keys))
	for i, k := range e.Keys {
		keyExpr, err := lowerExpr(l, k)
		if err != nil {
			return nil, err
		}
		valExpr, err := lowerExpr(l, e.Values[i])
		if err != nil {
			return nil, err
		}
		pairs[i] = cerl.CMapPairAssoc(keyExpr, valExpr)
	}
	return cerl.CMap(cerl.CEmptyMap(), pairs, false), nil
}

// lowerMapGetExpr lowers m[k] to erlang:map_get(K, M).
func lowerMapGetExpr(l *lowerer, e *aotir.MapGetExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := lowerExpr(l, e.Key)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("map_get"), []cerl.Expr{key, recv}), nil
}

// lowerMapHasExpr lowers has(m, k) to maps:is_key(K, M).
func lowerMapHasExpr(l *lowerer, e *aotir.MapHasExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := lowerExpr(l, e.Key)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("maps"), cerl.CAtom("is_key"), []cerl.Expr{key, recv}), nil
}

// lowerMapLenExpr lowers len(m) for maps to erlang:map_size(M).
func lowerMapLenExpr(l *lowerer, e *aotir.MapLenExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("map_size"), []cerl.Expr{recv}), nil
}

// lowerListContainsExpr lowers `val in xs` to lists:member(Val, Xs).
func lowerListContainsExpr(l *lowerer, e *aotir.ListContainsExpr) (cerl.Expr, error) {
	xs, err := lowerExpr(l, e.List)
	if err != nil {
		return nil, err
	}
	val, err := lowerExpr(l, e.Value)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("member"), []cerl.Expr{val, xs}), nil
}

// lowerListSumExpr lowers `sum(xs)` to lists:foldl(fun(X,A)->X+A end, 0, Xs).
func lowerListSumExpr(l *lowerer, e *aotir.ListSumExpr) (cerl.Expr, error) {
	xs, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	var zero cerl.Expr
	if e.ElemType == aotir.TypeFloat {
		zero = cerl.CFloat(0.0)
	} else {
		zero = cerl.CInt(0)
	}
	addFun := cerl.CFun(
		[]cerl.Expr{cerl.CVar("V___x"), cerl.CVar("V___acc")},
		cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("+"),
			[]cerl.Expr{cerl.CVar("V___x"), cerl.CVar("V___acc")}))
	return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("foldl"),
		[]cerl.Expr{addFun, zero, xs}), nil
}

// lowerListMinExpr lowers `min(xs)` to lists:min(Xs).
func lowerListMinExpr(l *lowerer, e *aotir.ListMinExpr) (cerl.Expr, error) {
	xs, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("min"), []cerl.Expr{xs}), nil
}

// lowerListMaxExpr lowers `max(xs)` to lists:max(Xs).
func lowerListMaxExpr(l *lowerer, e *aotir.ListMaxExpr) (cerl.Expr, error) {
	xs, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("max"), []cerl.Expr{xs}), nil
}

// lowerRecordLit lowers Person{name: "alice", age: 30} to a tagged BEAM map:
// #{mochi_record_tag => person, name => <<"alice">>, age => 30}
// Fields are already in record-decl source order (aotir enforces this).
func lowerRecordLit(l *lowerer, e *aotir.RecordLit) (cerl.Expr, error) {
	pairs := make([]cerl.Expr, 0, 1+len(e.Fields))
	// First pair: mochi_record_tag => <lowercased record name atom>
	tagAtom := cerl.CAtom(strings.ToLower(e.TypeName))
	pairs = append(pairs, cerl.CMapPairAssoc(cerl.CAtom("mochi_record_tag"), tagAtom))
	// Remaining pairs: field name atom => lowered value
	for _, f := range e.Fields {
		val, err := lowerExpr(l, f.Value)
		if err != nil {
			return nil, fmt.Errorf("beam/lower: record field %s: %w", f.Name, err)
		}
		pairs = append(pairs, cerl.CMapPairAssoc(cerl.CAtom(f.Name), val))
	}
	return cerl.CMap(cerl.CEmptyMap(), pairs, false), nil
}

// lowerFieldAccess lowers p.name to maps:get(name, V_p).
func lowerFieldAccess(l *lowerer, e *aotir.FieldAccess) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("maps"), cerl.CAtom("get"),
		[]cerl.Expr{cerl.CAtom(e.FieldName), recv}), nil
}

// lowerVariantLit lowers a variant constructor to a tagged atom or tuple.
// Unit variants (no fields) → atom; variants with fields → {tag, f1, f2, ...}.
func lowerVariantLit(l *lowerer, e *aotir.VariantLit) (cerl.Expr, error) {
	tag := cerl.CAtom(strings.ToLower(e.VariantName))
	if len(e.Fields) == 0 {
		return tag, nil
	}
	elems := make([]cerl.Expr, 1+len(e.Fields))
	elems[0] = tag
	for i, f := range e.Fields {
		val, err := lowerExpr(l, f.Value)
		if err != nil {
			return nil, fmt.Errorf("beam/lower: variant field %s: %w", f.Name, err)
		}
		elems[1+i] = val
	}
	return cerl.CTuple(elems), nil
}

// lowerVariantFieldAccess lowers a field access on a known variant.
// After pattern matching, the field is bound to a variable by the match arm,
// so we just reference the variable V_<VarName> (set up by the bindings).
// If the receiver is a VarRef, the match arm body already has the binding in scope.
func lowerVariantFieldAccess(l *lowerer, e *aotir.VariantFieldAccess) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	// Receiver is a bound variable holding the tuple; extract by index.
	// FieldName is the field name; the variant has fields in declaration order.
	// We use element/2 (1-indexed, +1 for the tag element).
	// For a single-field variant: element(2, V_x).
	// For multi-field: element(fieldIndex+2, V_x).
	// Since MatchArm.Bindings already sets up V_fieldname = element(i, tuple),
	// this is called only when the variant is not destructured via match.
	// Fall back to a tuple element extraction; but for BEAM we need the field index.
	// Because we don't have the decl here, use a generic approach:
	// field access outside match is not typical in Phase 5.0 fixtures.
	// For now, return an error noting this should be accessed via match.
	_ = recv
	return nil, fmt.Errorf("beam/lower: VariantFieldAccess outside match not yet supported for field %s.%s", e.VariantName, e.FieldName)
}

// lowerMatchStmt lowers a MatchStmt to a Core Erlang c_case expression.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt, cont cerl.Expr) (cerl.Expr, error) {
	target, err := lowerExpr(l, s.Target)
	if err != nil {
		return nil, fmt.Errorf("beam/lower: match target: %w", err)
	}

	var clauses []cerl.Expr

	// Process each arm.
	for i := range s.Arms {
		arm := &s.Arms[i]
		clause, err := l.lowerMatchArm(arm, s, cont)
		if err != nil {
			return nil, fmt.Errorf("beam/lower: match arm %d: %w", i, err)
		}
		clauses = append(clauses, clause)
	}

	// Wildcard/default arm.
	if s.Default != nil {
		clause, err := l.lowerMatchArm(s.Default, s, cont)
		if err != nil {
			return nil, fmt.Errorf("beam/lower: match default arm: %w", err)
		}
		clauses = append(clauses, clause)
	}

	matchExpr := cerl.CCase(target, clauses)

	// If the match has a ResultVar, bind it and thread cont.
	if s.ResultVar != "" {
		if cont == nil {
			return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + s.ResultVar)}, matchExpr, cerl.CAtom("ok")), nil
		}
		return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + s.ResultVar)}, matchExpr, cont), nil
	}

	// No result var: cont was already threaded into each arm's body by lowerMatchArm.
	// Returning matchExpr alone is correct — don't CSeq cont again.
	return matchExpr, nil
}

// lowerMatchArm lowers one match arm to a c_clause.
func (l *lowerer) lowerMatchArm(arm *aotir.MatchArm, s *aotir.MatchStmt, cont cerl.Expr) (cerl.Expr, error) {
	var pat cerl.Expr
	if arm.VariantName == "" {
		// Wildcard arm: fresh variable.
		pat = cerl.CVar("V___wild")
	} else {
		tag := cerl.CAtom(strings.ToLower(arm.VariantName))
		if len(arm.Bindings) == 0 {
			// Unit variant.
			pat = tag
		} else {
			// Tuple variant: {tag, V_field1, V_field2, ...}.
			elems := make([]cerl.Expr, 1+len(arm.Bindings))
			elems[0] = tag
			for i, b := range arm.Bindings {
				if b.VarName == "_" {
					elems[1+i] = cerl.CVar(fmt.Sprintf("V___w%d", i))
				} else {
					elems[1+i] = cerl.CVar("V_" + b.VarName)
				}
			}
			pat = cerl.CTuple(elems)
		}
	}

	// Add bound variables to scope for the body.
	if l.scope == nil {
		l.scope = make(map[string]bool)
	}
	for _, b := range arm.Bindings {
		if b.VarName != "_" {
			l.scope[b.VarName] = true
		}
	}

	// Lower body.
	var bodyExpr cerl.Expr
	var err error
	if s.ResultVar != "" {
		// Match used as expression: the arm's body value becomes the match result.
		bodyExpr, err = l.lowerMatchArmAsExpr(arm)
	} else {
		bodyExpr, err = l.lowerBlock(arm.Body.Statements, cont)
	}

	for _, b := range arm.Bindings {
		if b.VarName != "_" {
			delete(l.scope, b.VarName)
		}
	}
	if err != nil {
		return nil, err
	}

	return cerl.CClause([]cerl.Expr{pat}, cerl.CAtom("true"), bodyExpr), nil
}

// lowerMatchArmAsExpr lowers a match arm body used as an expression value.
// Expression-style arms have a body of [AssignStmt{ResultVar, value}]; extract
// just the value so the enclosing CLet in lowerMatchStmt binds it correctly.
func (l *lowerer) lowerMatchArmAsExpr(arm *aotir.MatchArm) (cerl.Expr, error) {
	stmts := arm.Body.Statements
	if len(stmts) == 0 {
		return cerl.CAtom("ok"), nil
	}
	last := stmts[len(stmts)-1]
	if assign, ok := last.(*aotir.AssignStmt); ok {
		val, err := lowerExpr(l, assign.Value)
		if err != nil {
			return nil, err
		}
		if len(stmts) == 1 {
			return val, nil
		}
		// Multi-stmt body: lower preceding stmts with the value as continuation.
		return l.lowerBlock(stmts[:len(stmts)-1], val)
	}
	return l.lowerBlock(stmts, nil)
}

func lowerBinaryExpr(l *lowerer, e *aotir.BinaryExpr) (cerl.Expr, error) {
	left, err := lowerExpr(l, e.Left)
	if err != nil {
		return nil, err
	}
	right, err := lowerExpr(l, e.Right)
	if err != nil {
		return nil, err
	}

	switch e.Op {
	case aotir.BinAddI64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("+"), []cerl.Expr{left, right}), nil
	case aotir.BinSubI64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("-"), []cerl.Expr{left, right}), nil
	case aotir.BinMulI64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("*"), []cerl.Expr{left, right}), nil
	case aotir.BinDivI64:
		return lowerIntDiv(l, left, right)
	case aotir.BinModI64:
		return lowerIntMod(l, left, right)
	case aotir.BinAddF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("+"), []cerl.Expr{left, right}), nil
	case aotir.BinSubF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("-"), []cerl.Expr{left, right}), nil
	case aotir.BinMulF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("*"), []cerl.Expr{left, right}), nil
	case aotir.BinDivF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("/"), []cerl.Expr{left, right}), nil
	case aotir.BinEqI64, aotir.BinEqBool, aotir.BinEqStr, aotir.BinEqRec, aotir.BinEqList, aotir.BinEqMap:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("=:="), []cerl.Expr{left, right}), nil
	case aotir.BinNeI64, aotir.BinNeBool, aotir.BinNeStr, aotir.BinNeRec, aotir.BinNeList, aotir.BinNeMap:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("=/="), []cerl.Expr{left, right}), nil
	case aotir.BinLtI64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("<"), []cerl.Expr{left, right}), nil
	case aotir.BinLeI64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("=<"), []cerl.Expr{left, right}), nil
	case aotir.BinGtI64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom(">"), []cerl.Expr{left, right}), nil
	case aotir.BinGeI64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom(">="), []cerl.Expr{left, right}), nil
	case aotir.BinEqF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("=:="), []cerl.Expr{left, right}), nil
	case aotir.BinNeF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("=/="), []cerl.Expr{left, right}), nil
	case aotir.BinLtF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("<"), []cerl.Expr{left, right}), nil
	case aotir.BinLeF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("=<"), []cerl.Expr{left, right}), nil
	case aotir.BinGtF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom(">"), []cerl.Expr{left, right}), nil
	case aotir.BinGeF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom(">="), []cerl.Expr{left, right}), nil
	case aotir.BinAndBool:
		return cerl.CCase(left, []cerl.Expr{
			cerl.CClause([]cerl.Expr{cerl.CAtom("false")}, cerl.CAtom("true"), cerl.CAtom("false")),
			cerl.CClause([]cerl.Expr{cerl.CVar("V___")}, cerl.CAtom("true"), right),
		}), nil
	case aotir.BinOrBool:
		return cerl.CCase(left, []cerl.Expr{
			cerl.CClause([]cerl.Expr{cerl.CAtom("true")}, cerl.CAtom("true"), cerl.CAtom("true")),
			cerl.CClause([]cerl.Expr{cerl.CVar("V___")}, cerl.CAtom("true"), right),
		}), nil
	case aotir.BinStrCat:
		return cerl.CCall(cerl.CAtom("mochi_str"), cerl.CAtom("concat"), []cerl.Expr{left, right}), nil
	default:
		return nil, fmt.Errorf("beam/lower: unsupported binary op %v", e.Op)
	}
}

func lowerIntDiv(l *lowerer, left, right cerl.Expr) (cerl.Expr, error) {
	divExpr := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("div"), []cerl.Expr{left, right})
	return wrapArithErr(l, divExpr, "V___divres"), nil
}

func lowerIntMod(l *lowerer, left, right cerl.Expr) (cerl.Expr, error) {
	modExpr := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("rem"), []cerl.Expr{left, right})
	return wrapArithErr(l, modExpr, "V___modres"), nil
}

func wrapArithErr(l *lowerer, op cerl.Expr, resVar string) cerl.Expr {
	// Re-throw badarith as {mochi_panic, 5, Msg} (MOCHI_ERR_DIVZERO = 5) so
	// that TryCatchStmt's catch handler can intercept it uniformly.
	// Use unique variable names to avoid conflicts with nested CTry nodes.
	n := l.nextTryNum()
	cls := fmt.Sprintf("V___cls%d", n)
	rsn := fmt.Sprintf("V___rsn%d", n)
	stk := fmt.Sprintf("V___stk%d", n)
	res := fmt.Sprintf("%s%d", resVar, n)
	errHandler := cerl.CCase(cerl.CVar(rsn), []cerl.Expr{
		cerl.CClause(
			[]cerl.Expr{cerl.CAtom("badarith")},
			cerl.CAtom("true"),
			cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("error"),
				[]cerl.Expr{cerl.CTuple([]cerl.Expr{
					cerl.CAtom("mochi_panic"),
					cerl.CInt(5), // MOCHI_ERR_DIVZERO
					cerl.CBin([]byte("integer divide by zero")),
				})}),
		),
		cerl.CClause(
			[]cerl.Expr{cerl.CVar("V___")},
			cerl.CAtom("true"),
			cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("raise"), []cerl.Expr{
				cerl.CVar(cls), cerl.CVar(rsn), cerl.CVar(stk),
			}),
		),
	})
	return cerl.CTry(op,
		[]cerl.Expr{cerl.CVar(res)}, cerl.CVar(res),
		[]cerl.Expr{cerl.CVar(cls), cerl.CVar(rsn), cerl.CVar(stk)},
		errHandler,
	)
}

func lowerUnaryExpr(l *lowerer, e *aotir.UnaryExpr) (cerl.Expr, error) {
	operand, err := lowerExpr(l, e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("-"), []cerl.Expr{operand}), nil
	case aotir.UnNotBool:
		return cerl.CCase(operand, []cerl.Expr{
			cerl.CClause([]cerl.Expr{cerl.CAtom("true")}, cerl.CAtom("true"), cerl.CAtom("false")),
			cerl.CClause([]cerl.Expr{cerl.CAtom("false")}, cerl.CAtom("true"), cerl.CAtom("true")),
		}), nil
	default:
		return nil, fmt.Errorf("beam/lower: unsupported unary op %v", e.Op)
	}
}

func lowerCallExpr(l *lowerer, e *aotir.CallExpr) (cerl.Expr, error) {
	args := make([]cerl.Expr, len(e.Args))
	for i, a := range e.Args {
		arg, err := lowerExpr(l, a)
		if err != nil {
			return nil, err
		}
		args[i] = arg
	}
	return cerl.CApply(cerl.CVarFunc(e.Func, len(e.Args)), args), nil
}

// lowerFunLit lowers a closure literal to a Core Erlang c_fun.
// The lifted function body is inlined as a c_fun value so that the BEAM
// lambda is a first-class value.
func (l *lowerer) lowerFunLit(e *aotir.FunLit) (cerl.Expr, error) {
	fn, ok := l.liftedFuncs[e.FuncName]
	if !ok {
		return nil, fmt.Errorf("beam/lower: FunLit references unknown lifted function %q", e.FuncName)
	}

	// Build parameter variable expressions.
	vars := make([]cerl.Expr, len(fn.Params))
	for i, p := range fn.Params {
		vars[i] = cerl.CVar("V_" + p.Name)
	}

	// Save and reset scope for the closure body.
	outer := l.scope
	l.scope = make(map[string]bool)
	for _, p := range fn.Params {
		l.scope[p.Name] = true
	}

	body, err := l.lowerFunctionBody(fn.Body.Statements, nil)
	l.scope = outer
	if err != nil {
		return nil, fmt.Errorf("beam/lower: FunLit %s: %w", e.FuncName, err)
	}

	return cerl.CFun(vars, body), nil
}

// lowerFunCallExpr lowers a higher-order function call (callee is a fun value).
func (l *lowerer) lowerFunCallExpr(e *aotir.FunCallExpr) (cerl.Expr, error) {
	callee, err := lowerExpr(l, e.Callee)
	if err != nil {
		return nil, err
	}
	args := make([]cerl.Expr, len(e.Args))
	for i, a := range e.Args {
		arg, err := lowerExpr(l, a)
		if err != nil {
			return nil, err
		}
		args[i] = arg
	}
	return cerl.CApply(callee, args), nil
}

// lowerListSortAscExpr lowers a ListSortAscExpr to lists:sort/1.
func lowerListSortAscExpr(l *lowerer, e *aotir.ListSortAscExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("sort"), []cerl.Expr{recv}), nil
}

// lowerListSliceExpr lowers a ListSliceExpr to lists:sublist/2 + lists:nthtail/2.
// Mochi semantics: slice(xs, start, end) returns xs[start..end).
func lowerListSliceExpr(l *lowerer, e *aotir.ListSliceExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	start, err := lowerExpr(l, e.Start)
	if err != nil {
		return nil, err
	}
	end, err := lowerExpr(l, e.End)
	if err != nil {
		return nil, err
	}
	// lists:sublist(lists:nthtail(Start, Xs), End - Start)
	tail := cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("nthtail"), []cerl.Expr{start, recv})
	length := cerl.CCall(cerl.CAtom("erlang"), cerl.CAtom("-"), []cerl.Expr{end, start})
	return cerl.CCall(cerl.CAtom("lists"), cerl.CAtom("sublist"), []cerl.Expr{tail, length}), nil
}

// ---- Phase 9.0: agents as functional state-threaded maps ----

// agentIntentFuncName returns the helper function name for an agent intent.
// Format: mochi_agent_<lowercase_agentname>_<intentname>
func agentIntentFuncName(agentName, intentName string) string {
	return "mochi_agent_" + strings.ToLower(agentName) + "_" + intentName
}

// lowerAgentIntentFunctions generates helper functions for all intents of one agent.
func (l *lowerer) lowerAgentIntentFunctions(ag *aotir.AgentDecl) error {
	for i := range ag.Intents {
		if err := l.lowerAgentIntentFunc(ag, &ag.Intents[i]); err != nil {
			return fmt.Errorf("intent %s: %w", ag.Intents[i].Name, err)
		}
	}
	return nil
}

// lowerAgentIntentFunc emits one helper function for an agent intent.
//
// Unit intents take (State, params...) and return NewState.
// Value intents take (State, params...) and return Value (state is read-only).
func (l *lowerer) lowerAgentIntentFunc(ag *aotir.AgentDecl, intent *aotir.AgentIntentDecl) error {
	fnName := agentIntentFuncName(ag.Name, intent.Name)

	// Build parameter list: V___self is always first.
	vars := []string{"V___self"}
	for _, p := range intent.Params {
		vars = append(vars, "V_"+p.Name)
	}

	// Seed scope with parameters.
	outer := l.scope
	l.scope = make(map[string]bool)
	for _, p := range intent.Params {
		l.scope[p.Name] = true
	}

	var body cerl.Expr
	var err error
	if intent.ReturnType == aotir.TypeUnit {
		// Unit intent: return the (possibly updated) state at the end.
		body, err = l.lowerFunctionBody(intent.Body.Statements, cerl.CVar("V___self"))
	} else {
		// Value intent: return value via mochi_return throw.
		body, err = l.lowerFunctionBody(intent.Body.Statements, nil)
	}
	l.scope = outer
	if err != nil {
		return err
	}

	l.mod.Defs = append(l.mod.Defs, cerl.FuncDef{
		Name:  fnName,
		Arity: len(vars),
		Vars:  vars,
		Body:  body,
	})
	return nil
}

// lowerAgentLit lowers Counter{count: 0} to a BEAM map #{count => 0}.
func lowerAgentLit(l *lowerer, e *aotir.AgentLit) (cerl.Expr, error) {
	pairs := make([]cerl.Expr, len(e.Fields))
	for i, f := range e.Fields {
		val, err := lowerExpr(l, f.Value)
		if err != nil {
			return nil, fmt.Errorf("agent %s field %s: %w", e.AgentName, f.Name, err)
		}
		pairs[i] = cerl.CMapPairAssoc(cerl.CAtom(f.Name), val)
	}
	return cerl.CMap(cerl.CEmptyMap(), pairs, false), nil
}

// lowerAgentIntentCallExpr lowers a value-returning intent call c.get() to
// mochi_agent_<name>_<intent>(V_c).
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (cerl.Expr, error) {
	recv, err := lowerExpr(l, e.Receiver)
	if err != nil {
		return nil, err
	}
	args := []cerl.Expr{recv}
	for _, a := range e.Args {
		ae, err := lowerExpr(l, a)
		if err != nil {
			return nil, err
		}
		args = append(args, ae)
	}
	fnName := agentIntentFuncName(e.AgentName, e.IntentName)
	return cerl.CApply(cerl.CVarFunc(fnName, len(args)), args), nil
}

// lowerAgentIntentCallStmt lowers a unit intent call c.increment() to
// let V_c = mochi_agent_<name>_<intent>(V_c) in rest.
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt, tail []aotir.Stmt, cont cerl.Expr) (cerl.Expr, error) {
	// Receiver must be a VarRef so we can rebind it with the new state.
	receiverVar, ok := s.Receiver.(*aotir.VarRef)
	if !ok {
		return nil, fmt.Errorf("beam/lower: AgentIntentCallStmt: receiver must be a variable, got %T", s.Receiver)
	}
	varName := receiverVar.Name

	args := []cerl.Expr{cerl.CVar("V_" + varName)}
	for _, a := range s.Args {
		ae, err := lowerExpr(l, a)
		if err != nil {
			return nil, err
		}
		args = append(args, ae)
	}
	fnName := agentIntentFuncName(s.AgentName, s.IntentName)
	callExpr := cerl.CApply(cerl.CVarFunc(fnName, len(args)), args)

	rest, err := l.lowerBlock(tail, cont)
	if err != nil {
		return nil, err
	}
	return cerl.CLet([]cerl.Expr{cerl.CVar("V_" + varName)}, callExpr, rest), nil
}

// lowerLLMGenerateExpr lowers LLMGenerateExpr to mochi_llm:generate/3.
// Phase 13.0: in cassette mode (MOCHI_LLM_CASSETTE_DIR env set) mochi_llm
// replays pre-recorded responses; in live mode it calls the provider HTTP API.
func lowerLLMGenerateExpr(l *lowerer, e *aotir.LLMGenerateExpr) (cerl.Expr, error) {
	provider := cerl.CBin([]byte(e.Provider))
	model, err := lowerExpr(l, e.Model)
	if err != nil {
		return nil, err
	}
	prompt, err := lowerExpr(l, e.Prompt)
	if err != nil {
		return nil, err
	}
	return cerl.CCall(cerl.CAtom("mochi_llm"), cerl.CAtom("generate"),
		[]cerl.Expr{provider, model, prompt}), nil
}

// lowerDatalogQueryExpr runs a compile-time semi-naive bottom-up Datalog
// evaluator and returns a static Erlang list literal of binary strings.
// The result is a flat list: for each matching tuple the free-variable values
// are appended in order (same layout as the C backend's mochi_list_str).
func lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (cerl.Expr, error) {
	if e.Prog == nil {
		return cerl.CNil(), nil
	}
	results := datalogEval(e)
	// Build Erlang list from results (right-to-left CCons chain).
	list := cerl.Expr(cerl.CNil())
	for i := len(results) - 1; i >= 0; i-- {
		list = cerl.CCons(cerl.CBin([]byte(results[i])), list)
	}
	return list, nil
}

// datalogEval performs semi-naive bottom-up evaluation of e.Prog and
// returns the flat list of free-variable values from matching tuples.
func datalogEval(e *aotir.DatalogQueryExpr) []string {
	// Relation name -> set of tuples (each tuple is []string).
	state := map[string][][]string{}

	// Seed with base facts.
	for _, f := range e.Prog.Facts {
		args := make([]string, len(f.Args))
		copy(args, f.Args)
		state[f.Name] = append(state[f.Name], args)
	}

	// Semi-naive fixpoint: iterate until no new tuples are derived.
	for {
		changed := false
		for _, rule := range e.Prog.Rules {
			newTuples := deriveRule(rule, state)
			for _, t := range newTuples {
				if !tupleInRelation(state[rule.HeadName], t) {
					state[rule.HeadName] = append(state[rule.HeadName], t)
					changed = true
				}
			}
		}
		if !changed {
			break
		}
	}

	// Collect matching tuples for the query.
	rel := state[e.QueryName]
	var out []string
	for _, tuple := range rel {
		if len(tuple) != len(e.QueryArgs) {
			continue
		}
		match := true
		for i, qa := range e.QueryArgs {
			if qa != "" {
				// Bound argument: qa is "\"value\"" -- strip quotes.
				expected := qa
				if len(expected) >= 2 && expected[0] == '"' && expected[len(expected)-1] == '"' {
					expected = expected[1 : len(expected)-1]
				}
				if tuple[i] != expected {
					match = false
					break
				}
			}
		}
		if match {
			for i, qa := range e.QueryArgs {
				if qa == "" {
					out = append(out, tuple[i])
				}
			}
		}
	}
	return out
}

// deriveRule computes new head tuples by evaluating one rule body against state.
func deriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
	// Simple nested-loop join over body literals.
	// env maps variable names to bound values.
	results := []map[string]string{{}}
	for _, lit := range rule.Body {
		if lit.IsNeq {
			// Filter: env[NeqA] != env[NeqB].
			var next []map[string]string
			for _, env := range results {
				a, aok := env[lit.NeqA]
				b, bok := env[lit.NeqB]
				if !aok || !bok || a != b {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		if lit.IsNot {
			// Negation-as-failure: keep env only if no tuple in lit.Name matches.
			var next []map[string]string
			for _, env := range results {
				matched := false
				for _, t := range state[lit.Name] {
					if len(t) != len(lit.Args) {
						continue
					}
					ok := true
					for i, arg := range lit.Args {
						val := resolveArg(arg, env)
						if val != t[i] {
							ok = false
							break
						}
					}
					if ok {
						matched = true
						break
					}
				}
				if !matched {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		// Positive literal: join with relation tuples.
		var next []map[string]string
		for _, env := range results {
			for _, t := range state[lit.Name] {
				if len(t) != len(lit.Args) {
					continue
				}
				newEnv := copyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if isVariable(arg) {
						if existing, bound := newEnv[arg]; bound {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						// Constant: arg is "\"value\"" -- strip quotes.
						expected := unquoteStr(arg)
						if t[i] != expected {
							ok = false
							break
						}
					}
				}
				if ok {
					next = append(next, newEnv)
				}
			}
		}
		results = next
	}

	// Build head tuples from the final environments.
	var out [][]string
	for _, env := range results {
		head := make([]string, len(rule.HeadArgs))
		for i, ha := range rule.HeadArgs {
			if isVariable(ha) {
				head[i] = env[ha]
			} else {
				head[i] = unquoteStr(ha)
			}
		}
		out = append(out, head)
	}
	return out
}

func tupleInRelation(rel [][]string, t []string) bool {
	for _, r := range rel {
		if len(r) != len(t) {
			continue
		}
		eq := true
		for i := range r {
			if r[i] != t[i] {
				eq = false
				break
			}
		}
		if eq {
			return true
		}
	}
	return false
}

func resolveArg(arg string, env map[string]string) string {
	if isVariable(arg) {
		return env[arg]
	}
	return unquoteStr(arg)
}

func isVariable(s string) bool {
	// Variables are uppercase letters (standard Datalog convention) or lowercase
	// identifiers that don't start with a quote.
	return len(s) > 0 && s[0] != '"'
}

func unquoteStr(s string) string {
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		return s[1 : len(s)-1]
	}
	return s
}

func copyEnv(env map[string]string) map[string]string {
	out := make(map[string]string, len(env))
	for k, v := range env {
		out[k] = v
	}
	return out
}
