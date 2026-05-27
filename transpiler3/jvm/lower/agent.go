package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerAgentDecl lowers an aotir.AgentDecl to a CompilationUnit containing
// a single MochiAgent_<Name> class. The class uses the virtual-thread actor
// model described in MEP-47 Phase 9:
//
//   - A private static State inner class holds mutable fields.
//   - A sealed Message interface with one record per intent (fire-and-forget
//     intents use a plain record; value-returning intents carry a CompletableFuture).
//   - A public static Handle inner class with a LinkedBlockingQueue<Message>
//     and one method per intent.
//   - A static start() factory that creates the State and Handle, then launches
//     a virtual thread running the dispatch loop.
//   - A private static loop(Handle, State) method that takes() from the mailbox
//     and switches on the message type.
func (l *lowerer) lowerAgentDecl(ad *aotir.AgentDecl) (*javasrc.CompilationUnit, error) {
	agentClassName := "MochiAgent_" + ad.Name

	// ---- 1. Build State inner class ----
	stateFields := make([]javasrc.Member, len(ad.Fields))
	for i, f := range ad.Fields {
		ft, err := lowerAgentFieldType(f)
		if err != nil {
			return nil, fmt.Errorf("agent %q field %q: %w", ad.Name, f.Name, err)
		}
		stateFields[i] = &javasrc.FieldDecl{
			Type: ft,
			Name: f.Name,
		}
	}
	stateClass := &javasrc.ClassDecl{
		Modifiers: []string{"private", "static", "final"},
		Name:      "State",
		Members:   stateFields,
	}

	// ---- 2. Build Message sealed interface ----
	// One record per intent. For void intents: record Inc() implements Message {}
	// For value-returning intents: record Value(CompletableFuture<BoxedT> reply) implements Message {}
	// Plus a Stop record for agent shutdown.
	permits := make([]string, 0, len(ad.Intents)+1)
	messageMembers := []javasrc.Member{}

	for _, intent := range ad.Intents {
		// Permits clause uses "Message.IntentName" because the records are nested inside Message.
		permits = append(permits, "Message."+intent.Name)

		var components []javasrc.RecordComponent
		// Add the intent's parameters as record components.
		for _, p := range intent.Params {
			pt, err := lowerType(p.Type)
			if err != nil {
				return nil, fmt.Errorf("agent %q intent %q param %q: %w", ad.Name, intent.Name, p.Name, err)
			}
			components = append(components, javasrc.RecordComponent{Type: pt, Name: p.Name})
		}
		// For value-returning intents, add a reply CompletableFuture.
		if intent.ReturnType != aotir.TypeUnit {
			replyType, err := futureType(intent.ReturnType)
			if err != nil {
				return nil, fmt.Errorf("agent %q intent %q return: %w", ad.Name, intent.Name, err)
			}
			components = append(components, javasrc.RecordComponent{
				Type: replyType,
				Name: "$$reply",
			})
		}

		rec := &javasrc.RecordDecl{
			Modifiers:  nil, // "record" keyword is emitted by RecordDecl.javaString; no additional modifiers needed
			Name:       intent.Name,
			Components: components,
			Implements: []javasrc.TypeRef{{Name: "Message"}},
		}
		messageMembers = append(messageMembers, &javasrc.InnerTypeDecl{Decl: rec})
	}

	// Stop message.
	permits = append(permits, "Message.Stop")
	stopRec := &javasrc.RecordDecl{
		Modifiers:  nil,
		Name:       "Stop",
		Components: nil,
		Implements: []javasrc.TypeRef{{Name: "Message"}},
	}
	messageMembers = append(messageMembers, &javasrc.InnerTypeDecl{Decl: stopRec})

	// Note: "sealed" is already emitted by SealedInterfaceDecl.javaString as "sealed interface".
	// Do not put "sealed" in Modifiers or it will be emitted twice.
	sealedMsg := &javasrc.SealedInterfaceDecl{
		Modifiers: nil,
		Name:      "Message",
		Permits:   permits,
		Members:   messageMembers,
	}

	// ---- 3. Build Handle inner class ----
	// Fields: BlockingQueue<Message> $$mailbox, Thread $$thread.
	// Methods: one per intent + stop().
	handleMembers := []javasrc.Member{
		&javasrc.FieldDecl{
			Modifiers: []string{"final"},
			Type: javasrc.TypeRef{
				Name:     "java.util.concurrent.LinkedBlockingQueue",
				TypeArgs: []javasrc.TypeRef{{Name: "Message"}},
			},
			Name: "$$mailbox",
			// Initialized by constructor; no inline init.
		},
		&javasrc.FieldDecl{
			Modifiers: []string{"final"},
			Type:      javasrc.TypeRef{Name: "Thread"},
			Name:      "$$thread",
		},
	}

	// Constructor takes the pre-created mailbox and thread to avoid h[0] race.
	handleCtor := &javasrc.ConstructorDecl{
		Modifiers: []string{},
		Name:      "Handle",
		Params: []javasrc.Param{
			{Type: &javasrc.TypeRef{Name: "java.util.concurrent.LinkedBlockingQueue", TypeArgs: []javasrc.TypeRef{{Name: "Message"}}}, Name: "mbox"},
			{Type: &javasrc.TypeRef{Name: "Thread"}, Name: "t"},
		},
		Body: javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.AssignStmt{
				Target: &javasrc.NameExpr{Name: "this.$$mailbox"},
				Value:  &javasrc.NameExpr{Name: "mbox"},
			},
			&javasrc.AssignStmt{
				Target: &javasrc.NameExpr{Name: "this.$$thread"},
				Value:  &javasrc.NameExpr{Name: "t"},
			},
		}},
	}
	handleMembers = append(handleMembers, handleCtor)

	// Intent methods on Handle.
	for _, intent := range ad.Intents {
		method, err := buildHandleMethod(intent)
		if err != nil {
			return nil, fmt.Errorf("agent %q intent %q handle method: %w", ad.Name, intent.Name, err)
		}
		handleMembers = append(handleMembers, method)
	}

	// stop() method.
	stopMethod := buildHandleStopMethod()
	handleMembers = append(handleMembers, stopMethod)

	handleClass := &javasrc.ClassDecl{
		Modifiers: []string{"public", "static", "final"},
		Name:      "Handle",
		Members:   handleMembers,
	}

	// ---- 4. Build start() factory ----
	startMethod, err := buildStartMethod(ad)
	if err != nil {
		return nil, fmt.Errorf("agent %q start: %w", ad.Name, err)
	}

	// ---- 5. Build loop() dispatch method ----
	loopMethod, err := l.buildLoopMethod(ad)
	if err != nil {
		return nil, fmt.Errorf("agent %q loop: %w", ad.Name, err)
	}

	// ---- 6. Assemble the top-level MochiAgent_X class ----
	agentMembers := []javasrc.Member{
		&javasrc.InnerTypeDecl{Decl: stateClass},
		&javasrc.InnerTypeDecl{Decl: sealedMsg},
		&javasrc.InnerTypeDecl{Decl: handleClass},
		startMethod,
		loopMethod,
	}

	agentClass := &javasrc.ClassDecl{
		Modifiers: []string{"public", "final"},
		Name:      agentClassName,
		Members:   agentMembers,
	}

	return &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Types:   []javasrc.TypeDecl{agentClass},
	}, nil
}

// buildHandleMethod builds one method on the Handle class for a given intent.
// Fire-and-forget (void) intents: offer the message, return void.
// Value-returning intents: offer a message with CompletableFuture, block on future.get().
func buildHandleMethod(intent aotir.AgentIntentDecl) (*javasrc.MethodDecl, error) {
	params := make([]javasrc.Param, len(intent.Params))
	for i, p := range intent.Params {
		pt, err := lowerType(p.Type)
		if err != nil {
			return nil, err
		}
		params[i] = javasrc.Param{Type: &pt, Name: p.Name}
	}

	var body javasrc.Block
	if intent.ReturnType == aotir.TypeUnit {
		// Fire-and-forget: $$mailbox.offer(new Intent(params...))
		newMsgArgs := make([]javasrc.Expr, len(intent.Params))
		for i, p := range intent.Params {
			newMsgArgs[i] = &javasrc.NameExpr{Name: p.Name}
		}
		body = javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.ExprStmt{X: &javasrc.CallExpr{
				Receiver: &javasrc.NameExpr{Name: "$$mailbox"},
				Method:   "offer",
				Args: []javasrc.Expr{
					&javasrc.NewExpr{
						Type: javasrc.TypeRef{Name: "Message." + intent.Name},
						Args: newMsgArgs,
					},
				},
			}},
		}}

		retType := javasrc.TypeVoid
		return &javasrc.MethodDecl{
			Modifiers:  []string{"public"},
			ReturnType: retType,
			Name:       intent.Name,
			Params:     params,
			Body:       &body,
		}, nil
	}

	// Value-returning: create future, offer message with future, block on future.get().
	retType, err := lowerType(intent.ReturnType)
	if err != nil {
		return nil, err
	}

	futureRef, err := futureType(intent.ReturnType)
	if err != nil {
		return nil, err
	}

	// Build: var $$f = new CompletableFuture<BoxedT>()
	newMsgArgs := make([]javasrc.Expr, len(intent.Params)+1)
	for i, p := range intent.Params {
		newMsgArgs[i] = &javasrc.NameExpr{Name: p.Name}
	}
	newMsgArgs[len(intent.Params)] = &javasrc.NameExpr{Name: "$$f"}

	body = javasrc.Block{Stmts: []javasrc.Stmt{
		// final <FutureType> $$f = new CompletableFuture<>();
		&javasrc.VarDeclStmt{
			Final: true,
			Type:  &futureRef,
			Name:  "$$f",
			Init:  &javasrc.NewExpr{Type: javasrc.TypeRef{Name: "java.util.concurrent.CompletableFuture"}},
		},
		// $$mailbox.offer(new Message.Intent(params..., $$f));
		&javasrc.ExprStmt{X: &javasrc.CallExpr{
			Receiver: &javasrc.NameExpr{Name: "$$mailbox"},
			Method:   "offer",
			Args: []javasrc.Expr{
				&javasrc.NewExpr{
					Type: javasrc.TypeRef{Name: "Message." + intent.Name},
					Args: newMsgArgs,
				},
			},
		}},
		// try { return (RetType) $$f.get(); } catch (Exception $$e) { throw new RuntimeException($$e); }
		&javasrc.TryCatchStmt{
			Body: javasrc.Block{Stmts: []javasrc.Stmt{
				&javasrc.ReturnStmt{Value: &javasrc.CastExpr{
					Type: retType,
					X:    &javasrc.CallExpr{Receiver: &javasrc.NameExpr{Name: "$$f"}, Method: "get"},
				}},
			}},
			CatchVar:  "$$e",
			CatchTyp:  javasrc.TypeRef{Name: "Exception"},
			CatchBody: javasrc.Block{Stmts: []javasrc.Stmt{
				&javasrc.ThrowStmt{Value: &javasrc.NewExpr{
					Type: javasrc.TypeRef{Name: "RuntimeException"},
					Args: []javasrc.Expr{&javasrc.NameExpr{Name: "$$e"}},
				}},
			}},
		},
	}}

	return &javasrc.MethodDecl{
		Modifiers:  []string{"public"},
		ReturnType: retType,
		Name:       intent.Name,
		Params:     params,
		Body:       &body,
	}, nil
}

// buildHandleStopMethod builds the stop() method on the Handle class.
func buildHandleStopMethod() *javasrc.MethodDecl {
	return &javasrc.MethodDecl{
		Modifiers:  []string{"public"},
		ReturnType: javasrc.TypeVoid,
		Name:       "stop",
		Params:     nil,
		Body: &javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.ExprStmt{X: &javasrc.CallExpr{
				Receiver: &javasrc.NameExpr{Name: "$$mailbox"},
				Method:   "offer",
				Args:     []javasrc.Expr{&javasrc.NewExpr{Type: javasrc.TypeRef{Name: "Message.Stop"}}},
			}},
			&javasrc.TryCatchStmt{
				Body: javasrc.Block{Stmts: []javasrc.Stmt{
					&javasrc.ExprStmt{X: &javasrc.CallExpr{
						Receiver: &javasrc.NameExpr{Name: "$$thread"},
						Method:   "join",
					}},
				}},
				CatchVar:  "$$ie",
				CatchTyp:  javasrc.TypeRef{Name: "InterruptedException"},
				CatchBody: javasrc.Block{Stmts: []javasrc.Stmt{
					&javasrc.ExprStmt{X: &javasrc.StaticCallExpr{
						Class:  "Thread.currentThread()",
						Method: "interrupt",
					}},
				}},
			},
		}},
	}
}

// buildStartMethod builds the static start() factory method.
// Parameters: one per agent field (field name + type), in declaration order.
// Creates the mailbox first, initializes the state, then starts the virtual thread.
func buildStartMethod(ad *aotir.AgentDecl) (*javasrc.MethodDecl, error) {
	agentName := ad.Name
	mailboxType := javasrc.TypeRef{
		Name:     "java.util.concurrent.LinkedBlockingQueue",
		TypeArgs: []javasrc.TypeRef{{Name: "Message"}},
	}

	// Build params: one per field.
	params := make([]javasrc.Param, len(ad.Fields))
	for i, f := range ad.Fields {
		ft, err := lowerAgentFieldType(f)
		if err != nil {
			return nil, fmt.Errorf("start param %q: %w", f.Name, err)
		}
		params[i] = javasrc.Param{Type: &ft, Name: "$$init_" + f.Name}
	}

	// Statements:
	// 1. final State state = new State();
	// 2. state.<field> = $$init_<field>;  (for each field)
	// 3. final LinkedBlockingQueue<Message> $$mailbox = new LinkedBlockingQueue<>();
	// 4. final Thread $$t = Thread.ofVirtual()...start(() -> loop($$mailbox, state));
	// 5. return new Handle($$mailbox, $$t);
	stmts := []javasrc.Stmt{
		&javasrc.VarDeclStmt{
			Final: true,
			Type:  &javasrc.TypeRef{Name: "State"},
			Name:  "state",
			Init:  &javasrc.NewExpr{Type: javasrc.TypeRef{Name: "State"}},
		},
	}
	for _, f := range ad.Fields {
		stmts = append(stmts, &javasrc.AssignStmt{
			Target: &javasrc.NameExpr{Name: "state." + f.Name},
			Value:  &javasrc.NameExpr{Name: "$$init_" + f.Name},
		})
	}
	stmts = append(stmts,
		&javasrc.VarDeclStmt{
			Final: true,
			Type:  &mailboxType,
			Name:  "$$mailbox",
			Init:  &javasrc.NewExpr{Type: mailboxType},
		},
		&javasrc.VarDeclStmt{
			Final: true,
			Type:  &javasrc.TypeRef{Name: "Thread"},
			Name:  "$$t",
			Init:  &javasrc.LiteralExpr{Value: `Thread.ofVirtual().name("mochi-agent-` + agentName + `").start(() -> loop($$mailbox, state))`},
		},
		&javasrc.ReturnStmt{Value: &javasrc.NewExpr{
			Type: javasrc.TypeRef{Name: "Handle"},
			Args: []javasrc.Expr{
				&javasrc.NameExpr{Name: "$$mailbox"},
				&javasrc.NameExpr{Name: "$$t"},
			},
		}},
	)

	return &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: javasrc.TypeRef{Name: "Handle"},
		Name:       "start",
		Params:     params,
		Body:       &javasrc.Block{Stmts: stmts},
	}, nil
}

// buildLoopMethod builds the static loop(Handle h, State s) dispatch method.
// It takes from the mailbox in an infinite loop and pattern-matches on the message.
func (l *lowerer) buildLoopMethod(ad *aotir.AgentDecl) (*javasrc.MethodDecl, error) {
	// Build switch cases: one per intent + one for Stop.
	cases := make([]javasrc.SwitchCase, 0, len(ad.Intents)+1)

	stateFieldNames := make(map[string]bool, len(ad.Fields))
	for _, f := range ad.Fields {
		stateFieldNames[f.Name] = true
	}

	for _, intent := range ad.Intents {
		body, err := l.buildIntentCase(ad, intent, stateFieldNames)
		if err != nil {
			return nil, fmt.Errorf("intent %q: %w", intent.Name, err)
		}
		pattern := buildMessagePattern(intent)
		cases = append(cases, javasrc.SwitchCase{
			Pattern: pattern,
			Body:    body,
		})
	}

	// Stop case: use a block so "return" is valid inside a switch arm.
	// Java switch arrow arms require expression/block/throw; bare "return" is invalid.
	cases = append(cases, javasrc.SwitchCase{
		Pattern: "Message.Stop $$ignored",
		Body: []javasrc.Stmt{
			&javasrc.Block{Stmts: []javasrc.Stmt{&javasrc.ReturnStmt{}}},
		},
	})

	switchStmt := &javasrc.SwitchStmt{
		Tag:   &javasrc.NameExpr{Name: "$$m"},
		Cases: cases,
	}

	// Inner try-catch around mailbox.take().
	// The loop receives the mailbox directly to avoid h[0] race in start().
	takeTryCatch := &javasrc.TryCatchStmt{
		Body: javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.VarDeclStmt{
				Final: true,
				Type:  &javasrc.TypeRef{Name: "Message"},
				Name:  "$$m",
				Init: &javasrc.CallExpr{
					Receiver: &javasrc.NameExpr{Name: "$$mailbox"},
					Method:   "take",
				},
			},
			switchStmt,
		}},
		CatchVar: "$$ie",
		CatchTyp: javasrc.TypeRef{Name: "InterruptedException"},
		CatchBody: javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.ReturnStmt{},
		}},
	}

	whileBody := javasrc.Block{Stmts: []javasrc.Stmt{takeTryCatch}}

	mailboxParamType := javasrc.TypeRef{
		Name:     "java.util.concurrent.LinkedBlockingQueue",
		TypeArgs: []javasrc.TypeRef{{Name: "Message"}},
	}

	return &javasrc.MethodDecl{
		Modifiers:  []string{"private", "static"},
		ReturnType: javasrc.TypeVoid,
		Name:       "loop",
		Params: []javasrc.Param{
			{Type: &mailboxParamType, Name: "$$mailbox"},
			{Type: &javasrc.TypeRef{Name: "State"}, Name: "s"},
		},
		Body: &javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.WhileStmt{
				Cond: javasrc.Lit("true"),
				Body: whileBody,
			},
		}},
	}, nil
}

// buildMessagePattern builds the Java pattern for a switch case arm.
// For void intents with no params: "Message.Inc $$ignored"
// For intents with params or reply: "Message.Value(var p1, ..., var $$reply)"
func buildMessagePattern(intent aotir.AgentIntentDecl) string {
	totalFields := len(intent.Params)
	if intent.ReturnType != aotir.TypeUnit {
		totalFields++ // the reply future
	}
	if totalFields == 0 {
		return "Message." + intent.Name + " $$ignored"
	}
	// Destructure the record components.
	pattern := "Message." + intent.Name + "("
	for i, p := range intent.Params {
		if i > 0 {
			pattern += ", "
		}
		pattern += "var " + p.Name
	}
	if intent.ReturnType != aotir.TypeUnit {
		if len(intent.Params) > 0 {
			pattern += ", "
		}
		pattern += "var $$reply"
	}
	pattern += ")"
	return pattern
}

// buildIntentCase lowers the body of one intent case arm.
func (l *lowerer) buildIntentCase(ad *aotir.AgentDecl, intent aotir.AgentIntentDecl, stateFieldNames map[string]bool) ([]javasrc.Stmt, error) {
	body := rewriteAgentStateRefs(intent.Body, stateFieldNames)
	stmts, err := l.buildIntentStmts(body, intent.ReturnType != aotir.TypeUnit, stateFieldNames)
	if err != nil {
		return nil, err
	}
	return stmts, nil
}

// buildIntentStmts lowers an agent intent block.
// For value-returning intents, return statements become $$reply.complete(value).
func (l *lowerer) buildIntentStmts(b *aotir.Block, hasReply bool, stateFields map[string]bool) ([]javasrc.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	var out []javasrc.Stmt
	for _, s := range b.Statements {
		js, err := l.lowerIntentStmt(s, hasReply, stateFields)
		if err != nil {
			return nil, err
		}
		if js != nil {
			out = append(out, js)
		}
	}
	return out, nil
}

// lowerIntentStmt lowers one statement in an intent body.
func (l *lowerer) lowerIntentStmt(s aotir.Stmt, hasReply bool, stateFields map[string]bool) (javasrc.Stmt, error) {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if !hasReply || s.Value == nil {
			return &javasrc.ReturnStmt{}, nil
		}
		// Convert: return expr -> $$reply.complete(expr)
		val, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &javasrc.ExprStmt{X: &javasrc.CallExpr{
			Receiver: &javasrc.NameExpr{Name: "$$reply"},
			Method:   "complete",
			Args:     []javasrc.Expr{val},
		}}, nil

	case *aotir.AssignStmt:
		// The C lowerer may emit "__self->field" as the assignment target name.
		// Rewrite to "s.field" for JVM.
		val, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		target := s.Name
		if field := selfFieldName(s.Name); field != "" && stateFields[field] {
			target = "s." + field
		} else if stateFields[s.Name] {
			target = "s." + s.Name
		}
		return &javasrc.AssignStmt{
			Target: &javasrc.NameExpr{Name: target},
			Value:  val,
		}, nil

	case *aotir.IfStmt:
		cond, err := l.lowerExpr(s.Cond)
		if err != nil {
			return nil, err
		}
		thenStmts, err := l.buildIntentStmts(s.Then, hasReply, stateFields)
		if err != nil {
			return nil, err
		}
		var elseBlock *javasrc.Block
		if s.Else != nil {
			elseStmts, err := l.buildIntentStmts(s.Else, hasReply, stateFields)
			if err != nil {
				return nil, err
			}
			elseBlock = &javasrc.Block{Stmts: elseStmts}
		}
		return &javasrc.IfStmt{
			Cond: cond,
			Then: javasrc.Block{Stmts: thenStmts},
			Else: elseBlock,
		}, nil

	default:
		// Fall back to regular statement lowering.
		return l.lowerStmt(s)
	}
}

// rewriteAgentStateRefs rewrites a block so that references to agent state fields
// are prefixed with "s." for the dispatch loop.
func rewriteAgentStateRefs(b *aotir.Block, stateFields map[string]bool) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteAgentStateStmt(s, stateFields)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteAgentStateStmt(s aotir.Stmt, sf map[string]bool) aotir.Stmt {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return s
		}
		return &aotir.ReturnStmt{Value: rewriteAgentStateExpr(s.Value, sf)}
	case *aotir.AssignStmt:
		// Rewrite the value; the target rewriting is handled in lowerIntentStmt.
		return &aotir.AssignStmt{Name: s.Name, Value: rewriteAgentStateExpr(s.Value, sf)}
	case *aotir.LetStmt:
		cp := *s
		if s.Init != nil {
			cp.Init = rewriteAgentStateExpr(s.Init, sf)
		}
		return &cp
	case *aotir.CallStmt:
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteAgentStateExpr(a, sf)
		}
		return &aotir.CallStmt{Func: s.Func, Args: args}
	case *aotir.IfStmt:
		cp := *s
		cp.Cond = rewriteAgentStateExpr(s.Cond, sf)
		cp.Then = rewriteAgentStateBlock(s.Then, sf)
		if s.Else != nil {
			cp.Else = rewriteAgentStateBlock(s.Else, sf)
		}
		return &cp
	case *aotir.WhileStmt:
		return &aotir.WhileStmt{
			Cond: rewriteAgentStateExpr(s.Cond, sf),
			Body: rewriteAgentStateBlock(s.Body, sf),
		}
	case *aotir.ForEachStmt:
		cp := *s
		cp.List = rewriteAgentStateExpr(s.List, sf)
		cp.Body = rewriteAgentStateBlock(s.Body, sf)
		return &cp
	default:
		return s
	}
}

func rewriteAgentStateBlock(b *aotir.Block, sf map[string]bool) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteAgentStateStmt(s, sf)
	}
	return &aotir.Block{Statements: stmts}
}

// selfFieldName returns the bare field name if the VarRef name is "__self->fieldname",
// otherwise returns "".
func selfFieldName(name string) string {
	const prefix = "__self->"
	if len(name) > len(prefix) && name[:len(prefix)] == prefix {
		return name[len(prefix):]
	}
	return ""
}

func rewriteAgentStateExpr(e aotir.Expr, sf map[string]bool) aotir.Expr {
	if e == nil {
		return nil
	}
	switch e := e.(type) {
	case *aotir.VarRef:
		// The C lowerer represents agent state field accesses as "__self->fieldname".
		// Rewrite to "s.fieldname" for JVM emission.
		if field := selfFieldName(e.Name); field != "" && sf[field] {
			cp := *e
			cp.Name = "s." + field
			return &cp
		}
		return e
	case *aotir.BinaryExpr:
		return &aotir.BinaryExpr{
			Op:     e.Op,
			Left:   rewriteAgentStateExpr(e.Left, sf),
			Right:  rewriteAgentStateExpr(e.Right, sf),
			Result: e.Result,
		}
	case *aotir.UnaryExpr:
		return &aotir.UnaryExpr{
			Op:      e.Op,
			Operand: rewriteAgentStateExpr(e.Operand, sf),
			Result:  e.Result,
		}
	case *aotir.CallExpr:
		args := make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			args[i] = rewriteAgentStateExpr(a, sf)
		}
		cp := *e
		cp.Args = args
		return &cp
	default:
		return e
	}
}

// futureType returns the CompletableFuture<BoxedT> TypeRef for a return type.
func futureType(t aotir.Type) (javasrc.TypeRef, error) {
	boxed := boxedType(t)
	if boxed.Name == "Object" {
		return javasrc.TypeRef{}, fmt.Errorf("unsupported agent return type %v for CompletableFuture", t)
	}
	return javasrc.TypeRef{
		Name:     "java.util.concurrent.CompletableFuture",
		TypeArgs: []javasrc.TypeRef{boxed},
	}, nil
}

// lowerAgentFieldType maps a RecordField to a Java TypeRef for agent state.
// Only scalar primitives are supported in Phase 9.
func lowerAgentFieldType(f aotir.RecordField) (javasrc.TypeRef, error) {
	switch f.Type {
	case aotir.TypeInt:
		return javasrc.TypeLong, nil
	case aotir.TypeFloat:
		return javasrc.TypeDouble, nil
	case aotir.TypeBool:
		return javasrc.TypeBoolean, nil
	case aotir.TypeString:
		return javasrc.TypeString, nil
	default:
		return javasrc.TypeRef{}, fmt.Errorf("unsupported agent field type %v", f.Type)
	}
}

// lowerAgentSpawnExpr lowers AgentSpawnExpr to MochiAgent_X.start(initArgs...).
// When InitArgs is nil (the common case), we use zero values from the agent decl.
func (l *lowerer) lowerAgentSpawnExpr(e *aotir.AgentSpawnExpr) (javasrc.Expr, error) {
	var args []javasrc.Expr
	if len(e.InitArgs) > 0 {
		// Use provided initial args.
		lowered, err := l.lowerExprs(e.InitArgs)
		if err != nil {
			return nil, err
		}
		args = lowered
	} else {
		// No init args -- use zero values from the agent declaration.
		ad, ok := l.agents[e.AgentName]
		if !ok {
			return nil, fmt.Errorf("spawn: unknown agent %q", e.AgentName)
		}
		args = make([]javasrc.Expr, len(ad.Fields))
		for i, f := range ad.Fields {
			args[i] = zeroValueFor(f.Type)
		}
	}
	return &javasrc.StaticCallExpr{
		Class:  "MochiAgent_" + e.AgentName,
		Method: "start",
		Args:   args,
	}, nil
}

// lowerAgentLitExpr lowers AgentLit (synchronous non-spawn) to MochiAgent_X.start(fields...).
// The AgentLit.Fields contains the initial field values in declaration order.
func (l *lowerer) lowerAgentLitExpr(e *aotir.AgentLit) (javasrc.Expr, error) {
	args := make([]javasrc.Expr, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("agent lit %q field %q: %w", e.AgentName, f.Name, err)
		}
		args[i] = v
	}
	return &javasrc.StaticCallExpr{
		Class:  "MochiAgent_" + e.AgentName,
		Method: "start",
		Args:   args,
	}, nil
}

// zeroValueFor returns the Java zero/default value expression for an aotir type.
func zeroValueFor(t aotir.Type) javasrc.Expr {
	switch t {
	case aotir.TypeInt:
		return javasrc.Lit("0L")
	case aotir.TypeFloat:
		return javasrc.Lit("0.0")
	case aotir.TypeBool:
		return javasrc.Lit("false")
	case aotir.TypeString:
		return javasrc.Lit(`""`)
	default:
		return javasrc.Lit("null")
	}
}

// lowerAgentIntentCallExpr lowers AgentIntentCallExpr to handle.intentName(args...).
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	args, err := l.lowerExprs(e.Args)
	if err != nil {
		return nil, err
	}
	return &javasrc.CallExpr{
		Receiver: recv,
		Method:   e.IntentName,
		Args:     args,
	}, nil
}

// lowerAgentIntentCallStmt lowers AgentIntentCallStmt to handle.intentName(args...).
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt) (javasrc.Stmt, error) {
	recv, err := l.lowerExpr(s.Receiver)
	if err != nil {
		return nil, err
	}
	args, err := l.lowerExprs(s.Args)
	if err != nil {
		return nil, err
	}
	return &javasrc.ExprStmt{X: &javasrc.CallExpr{
		Receiver: recv,
		Method:   s.IntentName,
		Args:     args,
	}}, nil
}
