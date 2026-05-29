// Package lower - Phase 9: Agent lowering for Kotlin backend.
//
// Mochi agents are lowered to Kotlin classes with a LinkedBlockingQueue mailbox
// and a daemon Thread running the dispatch loop. No kotlin-coroutines dependency
// is required; the generated .kt file compiles with plain kotlinc.
//
// Pattern:
//
//	class <AgentName>Agent {
//	    sealed interface Message
//	    data object <IntentName> : Message  // for void intent
//	    data class <IntentName>(val reply: java.util.concurrent.CompletableFuture<Long>) : Message // for value-returning
//
//	    private val mailbox = java.util.concurrent.LinkedBlockingQueue<Message>()
//	    private var <field> = <zeroValue>
//
//	    private val thread = Thread {
//	        while (true) {
//	            try {
//	                when (val msg = mailbox.take()) {
//	                    is <IntentName> -> { ... }
//	                }
//	            } catch (e: InterruptedException) { break }
//	        }
//	    }.also { it.isDaemon = true; it.start() }
//
//	    fun <intent>(params): RetType { ... }
//	    fun shutdown() { thread.interrupt() }
//	}
package lower

import (
	"fmt"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

// lowerAgentDecl converts an aotir.AgentDecl to a Kotlin class declaration (RawKtDecl).
// We emit raw Kotlin because the ktree doesn't have a ClassDecl node yet.
func lowerAgentDecl(ad *aotir.AgentDecl) (ktree.Decl, error) {
	var sb strings.Builder
	name := ad.Name + "Agent"

	// Build constructor param list for initial field values.
	// Use "__init_<field>" to avoid shadowing the private var fields in lambdas.
	var ctorParams []string
	for _, f := range ad.Fields {
		typeName, err := ktScalarTypeErr(f.Type)
		if err != nil {
			return nil, fmt.Errorf("agent %q field %q: %w", ad.Name, f.Name, err)
		}
		zero, err := ktZeroValue(f.Type)
		if err != nil {
			return nil, fmt.Errorf("agent %q field %q: %w", ad.Name, f.Name, err)
		}
		ctorParams = append(ctorParams, fmt.Sprintf("__init_%s: %s = %s", f.Name, typeName, zero))
	}
	if len(ctorParams) > 0 {
		fmt.Fprintf(&sb, "class %s(%s) {\n", name, strings.Join(ctorParams, ", "))
	} else {
		fmt.Fprintf(&sb, "class %s {\n", name)
	}

	// -- sealed interface Message --
	// Message subclasses use the prefix "Msg_" to avoid name conflicts with intent methods.
	sb.WriteString("    sealed interface Message\n")
	for _, intent := range ad.Intents {
		msgName := "Msg_" + intent.Name
		if intent.ReturnType == aotir.TypeUnit && len(intent.Params) == 0 {
			// Void intents with no params: data object (singleton)
			fmt.Fprintf(&sb, "    data object %s : Message\n", msgName)
		} else {
			// Void intents with params or value-returning: data class
			var fields []string
			for _, p := range intent.Params {
				pt, err := ktScalarTypeErr(p.Type)
				if err != nil {
					return nil, fmt.Errorf("agent %q intent %q param %q: %w", ad.Name, intent.Name, p.Name, err)
				}
				fields = append(fields, fmt.Sprintf("val %s: %s", p.Name, pt))
			}
			if intent.ReturnType != aotir.TypeUnit {
				// Value-returning: also add reply CompletableFuture
				boxed, err := ktBoxedType(intent.ReturnType)
				if err != nil {
					return nil, fmt.Errorf("agent %q intent %q: %w", ad.Name, intent.Name, err)
				}
				fields = append(fields, fmt.Sprintf("val reply: java.util.concurrent.CompletableFuture<%s>", boxed))
			}
			fmt.Fprintf(&sb, "    data class %s(%s) : Message\n", msgName, strings.Join(fields, ", "))
		}
	}
	sb.WriteString("    data object Shutdown : Message\n\n")

	// -- mailbox --
	sb.WriteString("    private val mailbox = java.util.concurrent.LinkedBlockingQueue<Message>()\n")

	// -- mutable state fields (initialized from __init_<field> constructor params) --
	for _, f := range ad.Fields {
		typeName, err := ktScalarTypeErr(f.Type)
		if err != nil {
			return nil, fmt.Errorf("agent %q field %q: %w", ad.Name, f.Name, err)
		}
		fmt.Fprintf(&sb, "    private var %s: %s = __init_%s\n", f.Name, typeName, f.Name)
	}

	// -- dispatch thread --
	sb.WriteString("\n    private val thread = Thread {\n")
	sb.WriteString("        while (true) {\n")
	sb.WriteString("            try {\n")
	sb.WriteString("                when (val msg = mailbox.take()) {\n")
	for _, intent := range ad.Intents {
		msgName := "Msg_" + intent.Name
		fmt.Fprintf(&sb, "                    is %s -> {\n", msgName)
		// For intents with params, expose them as local vals from msg.paramName.
		for _, p := range intent.Params {
			fmt.Fprintf(&sb, "                        val %s = msg.%s\n", p.Name, p.Name)
		}
		// Emit intent body: references to state fields accessed as "this.<field>"
		// The aotir body uses "__self-><field>" pattern (C backend) or direct names.
		body, err := lowerAgentIntentBody(ad, intent)
		if err != nil {
			return nil, fmt.Errorf("agent %q intent %q body: %w", ad.Name, intent.Name, err)
		}
		for _, line := range body {
			fmt.Fprintf(&sb, "                        %s\n", line)
		}
		sb.WriteString("                    }\n")
	}
	sb.WriteString("                    is Shutdown -> break\n")
	sb.WriteString("                }\n")
	sb.WriteString("            } catch (e: InterruptedException) { break }\n")
	sb.WriteString("        }\n")
	sb.WriteString("    }.also { it.isDaemon = true; it.start() }\n\n")

	// -- public intent methods --
	for _, intent := range ad.Intents {
		// Build parameter list
		var params []string
		for _, p := range intent.Params {
			pt, err := ktScalarTypeErr(p.Type)
			if err != nil {
				return nil, fmt.Errorf("agent %q intent method %q param %q: %w", ad.Name, intent.Name, p.Name, err)
			}
			params = append(params, fmt.Sprintf("%s: %s", p.Name, pt))
		}
		paramStr := strings.Join(params, ", ")
		msgName := "Msg_" + intent.Name
		if intent.ReturnType == aotir.TypeUnit {
			if len(intent.Params) == 0 {
				fmt.Fprintf(&sb, "    fun %s(%s) { mailbox.put(%s) }\n", intent.Name, paramStr, msgName)
			} else {
				// Void intent with params: put a data class message
				var msgArgs []string
				for _, p := range intent.Params {
					msgArgs = append(msgArgs, p.Name)
				}
				fmt.Fprintf(&sb, "    fun %s(%s) { mailbox.put(%s(%s)) }\n", intent.Name, paramStr, msgName, strings.Join(msgArgs, ", "))
			}
		} else {
			boxed, err := ktBoxedType(intent.ReturnType)
			if err != nil {
				return nil, fmt.Errorf("agent %q intent method %q: %w", ad.Name, intent.Name, err)
			}
			retType, err := ktScalarTypeErr(intent.ReturnType)
			if err != nil {
				return nil, fmt.Errorf("agent %q intent method %q: %w", ad.Name, intent.Name, err)
			}
			var msgArgs []string
			for _, p := range intent.Params {
				msgArgs = append(msgArgs, p.Name)
			}
			fmt.Fprintf(&sb, "    fun %s(%s): %s {\n", intent.Name, paramStr, retType)
			fmt.Fprintf(&sb, "        val fut = java.util.concurrent.CompletableFuture<%s>()\n", boxed)
			if len(msgArgs) > 0 {
				fmt.Fprintf(&sb, "        mailbox.put(%s(%s, fut))\n", msgName, strings.Join(msgArgs, ", "))
			} else {
				fmt.Fprintf(&sb, "        mailbox.put(%s(fut))\n", msgName)
			}
			fmt.Fprintf(&sb, "        return fut.get()\n")
			fmt.Fprintf(&sb, "    }\n")
		}
	}

	// -- field setters for agent-literal initialisation --
	// Fields are private so they cannot be set from outside the class.
	// We expose internal setters so that the run{} block in
	// lowerAgentLitExpr can initialise non-zero field values.
	for _, f := range ad.Fields {
		typeName, err := ktScalarTypeErr(f.Type)
		if err != nil {
			return nil, fmt.Errorf("agent %q field setter %q: %w", ad.Name, f.Name, err)
		}
		fmt.Fprintf(&sb, "    internal fun setField_%s(v: %s) { %s = v }\n", f.Name, typeName, f.Name)
	}

	// -- shutdown --
	sb.WriteString("    fun shutdown() { mailbox.put(Shutdown) }\n")
	sb.WriteString("}")

	return &ktree.RawKtDecl{Code: sb.String()}, nil
}

// lowerAgentIntentBody lowers one agent intent's body to a list of Kotlin statement strings.
// The body stmts reference state fields (agent fields). In the C lowerer, field accesses
// are emitted as "__self-><fieldname>". We rewrite them to plain field names.
func lowerAgentIntentBody(ad *aotir.AgentDecl, intent aotir.AgentIntentDecl) ([]string, error) {
	if intent.Body == nil {
		return nil, nil
	}
	stateFieldNames := make(map[string]bool, len(ad.Fields))
	for _, f := range ad.Fields {
		stateFieldNames[f.Name] = true
	}

	l := &lowerer{}
	// Rewrite __self->fieldname to fieldname in the body block.
	body := rewriteAgentBodyBlock(intent.Body, stateFieldNames)
	stmts, err := l.lowerBlock(body)
	if err != nil {
		return nil, err
	}

	// For value-returning intents, convert return to msg.reply.complete(val).
	if intent.ReturnType != aotir.TypeUnit {
		stmts = rewriteAgentReturnStmts(stmts)
	}

	lines := make([]string, len(stmts))
	for i, s := range stmts {
		// KtString(0) gives no indentation; we add the dispatch body indentation above.
		lines[i] = s.KtString(0)
	}
	return lines, nil
}

// rewriteAgentReturnStmts converts ReturnStmt in an agent intent to msg.reply.complete(val).
func rewriteAgentReturnStmts(stmts []ktree.Stmt) []ktree.Stmt {
	result := make([]ktree.Stmt, len(stmts))
	for i, s := range stmts {
		if rs, ok := s.(*ktree.ReturnStmt); ok && rs.Value != nil {
			result[i] = &ktree.ExprStmt{
				X: &ktree.RawKtExpr{Code: fmt.Sprintf("msg.reply.complete(%s)", rs.Value.KtExprString())},
			}
		} else {
			result[i] = s
		}
	}
	return result
}

// rewriteAgentBodyBlock rewrites the body of an agent intent to replace
// "__self->fieldname" variable references with plain "fieldname".
func rewriteAgentBodyBlock(b *aotir.Block, stateFields map[string]bool) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteAgentBodyStmt(s, stateFields)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteAgentBodyStmt(s aotir.Stmt, sf map[string]bool) aotir.Stmt {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return s
		}
		return &aotir.ReturnStmt{Value: rewriteAgentBodyExpr(s.Value, sf)}
	case *aotir.AssignStmt:
		name := s.Name
		if field := agentSelfField(name); field != "" && sf[field] {
			name = field
		}
		return &aotir.AssignStmt{Name: name, Value: rewriteAgentBodyExpr(s.Value, sf)}
	case *aotir.LetStmt:
		cp := *s
		if s.Init != nil {
			cp.Init = rewriteAgentBodyExpr(s.Init, sf)
		}
		return &cp
	case *aotir.CallStmt:
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteAgentBodyExpr(a, sf)
		}
		return &aotir.CallStmt{Func: s.Func, Args: args}
	case *aotir.IfStmt:
		cp := *s
		cp.Cond = rewriteAgentBodyExpr(s.Cond, sf)
		cp.Then = rewriteAgentBodyBlock(s.Then, sf)
		if s.Else != nil {
			cp.Else = rewriteAgentBodyBlock(s.Else, sf)
		}
		return &cp
	default:
		return s
	}
}

func rewriteAgentBodyExpr(e aotir.Expr, sf map[string]bool) aotir.Expr {
	if e == nil {
		return nil
	}
	switch e := e.(type) {
	case *aotir.VarRef:
		if field := agentSelfField(e.Name); field != "" && sf[field] {
			cp := *e
			cp.Name = field
			return &cp
		}
		return e
	case *aotir.BinaryExpr:
		return &aotir.BinaryExpr{
			Op:     e.Op,
			Left:   rewriteAgentBodyExpr(e.Left, sf),
			Right:  rewriteAgentBodyExpr(e.Right, sf),
			Result: e.Result,
		}
	case *aotir.UnaryExpr:
		return &aotir.UnaryExpr{
			Op:      e.Op,
			Operand: rewriteAgentBodyExpr(e.Operand, sf),
			Result:  e.Result,
		}
	default:
		return e
	}
}

// agentSelfField returns the bare field name if name is "__self->fieldname", else "".
func agentSelfField(name string) string {
	const prefix = "__self->"
	if len(name) > len(prefix) && name[:len(prefix)] == prefix {
		return name[len(prefix):]
	}
	return ""
}

// ktBoxedType returns the Kotlin boxed type name for a scalar aotir type.
func ktBoxedType(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeInt:
		return "Long", nil
	case aotir.TypeFloat:
		return "Double", nil
	case aotir.TypeBool:
		return "Boolean", nil
	case aotir.TypeString:
		return "String", nil
	default:
		return "", fmt.Errorf("unsupported agent return type %v for CompletableFuture", t)
	}
}

// ktScalarTypeErr returns the Kotlin type name for an aotir scalar type, or an error.
func ktScalarTypeErr(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeInt:
		return "Long", nil
	case aotir.TypeFloat:
		return "Double", nil
	case aotir.TypeBool:
		return "Boolean", nil
	case aotir.TypeString:
		return "String", nil
	default:
		return "", fmt.Errorf("unsupported agent field type %v", t)
	}
}

// ktZeroValue returns the Kotlin zero value literal for an aotir scalar type.
func ktZeroValue(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeInt:
		return "0L", nil
	case aotir.TypeFloat:
		return "0.0", nil
	case aotir.TypeBool:
		return "false", nil
	case aotir.TypeString:
		return `""`, nil
	default:
		return "", fmt.Errorf("unsupported agent field type %v", t)
	}
}

// lowerAgentLitExpr lowers AgentLit to <AgentName>Agent(__init_field = val, ...).
// Constructor params are prefixed with __init_ to avoid shadowing the private var fields.
func (l *lowerer) lowerAgentLitExpr(e *aotir.AgentLit) (ktree.Expr, error) {
	if len(e.Fields) == 0 {
		return &ktree.RawKtExpr{Code: fmt.Sprintf("%sAgent()", e.AgentName)}, nil
	}
	var args []string
	for _, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, fmt.Errorf("agent lit %q field %q: %w", e.AgentName, f.Name, err)
		}
		args = append(args, fmt.Sprintf("__init_%s = %s", f.Name, v.KtExprString()))
	}
	return &ktree.RawKtExpr{Code: fmt.Sprintf("%sAgent(%s)", e.AgentName, strings.Join(args, ", "))}, nil
}

// lowerAgentSpawnExpr lowers AgentSpawnExpr to a new agent instance.
func (l *lowerer) lowerAgentSpawnExpr(e *aotir.AgentSpawnExpr) (ktree.Expr, error) {
	return &ktree.RawKtExpr{Code: fmt.Sprintf("%sAgent()", e.AgentName)}, nil
}

// lowerAgentIntentCallExpr lowers AgentIntentCallExpr to receiver.intentName(args...).
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	args := make([]ktree.Expr, len(e.Args))
	for i, a := range e.Args {
		ae, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = ae
	}
	argStrs := make([]string, len(args))
	for i, a := range args {
		argStrs[i] = a.KtExprString()
	}
	code := fmt.Sprintf("%s.%s(%s)", recv.KtExprString(), e.IntentName, strings.Join(argStrs, ", "))
	return &ktree.RawKtExpr{Code: code}, nil
}

// lowerAgentIntentCallStmt lowers AgentIntentCallStmt to receiver.intentName(args...).
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt) (ktree.Stmt, error) {
	recv, err := l.lowerExpr(s.Receiver)
	if err != nil {
		return nil, err
	}
	args := make([]ktree.Expr, len(s.Args))
	for i, a := range s.Args {
		ae, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = ae
	}
	argStrs := make([]string, len(args))
	for i, a := range args {
		argStrs[i] = a.KtExprString()
	}
	code := fmt.Sprintf("%s.%s(%s)", recv.KtExprString(), s.IntentName, strings.Join(argStrs, ", "))
	return &ktree.ExprStmt{X: &ktree.RawKtExpr{Code: code}}, nil
}
