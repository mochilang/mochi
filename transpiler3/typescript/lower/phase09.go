// Phase 9: Agent declaration and intent call lowering.
//
// Mochi `agent` declarations lower to TypeScript `class` with mutable
// private fields (AgentClassDecl). Each intent becomes a method; the
// constructor accepts one argument per agent field. AgentSpawnExpr
// lowers to `new MochiAgentName(args...)`.
//
// Field access inside intent bodies uses `this.FIELD` because the
// aotir C lowerer stamps captured fields as `__self->FIELD`. The TS
// lowerer strips `__self->` and prefixes with `this.`.
package lower

import (
	"fmt"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// lowerAgentDecls lowers all agent declarations in the program to
// AgentClassDecl nodes. Called during the prelude assembly in Lower().
func (l *lowerer) lowerAgentDecls() ([]tstree.Decl, error) {
	var out []tstree.Decl
	for _, ag := range l.prog.Agents {
		decl, err := l.lowerAgentDecl(ag)
		if err != nil {
			return nil, err
		}
		out = append(out, decl)
	}
	return out, nil
}

func (l *lowerer) lowerAgentDecl(ag *aotir.AgentDecl) (*tstree.AgentClassDecl, error) {
	className := "Mochi" + ag.Name

	// Build private fields.
	fields := make([]tstree.AgentClassField, 0, len(ag.Fields))
	for _, f := range ag.Fields {
		tn, err := tsTypeForAotirType(f.Type, "")
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent %s field %s: %w", ag.Name, f.Name, err)
		}
		fields = append(fields, tstree.AgentClassField{Name: f.Name, Type: tn})
	}

	// Constructor: one param per field, assigns to this.field.
	ctorParams := make([]tstree.FuncParam, 0, len(ag.Fields))
	for _, f := range ag.Fields {
		tn, err := tsTypeForAotirType(f.Type, "")
		if err != nil {
			return nil, err
		}
		ctorParams = append(ctorParams, tstree.FuncParam{Name: f.Name, Type: tn})
	}
	ctorBody := make([]tstree.Stmt, 0, len(ag.Fields))
	for _, f := range ag.Fields {
		ctorBody = append(ctorBody, &tstree.RawStmt{
			Text: fmt.Sprintf("this.%s = %s;", f.Name, f.Name),
		})
	}
	ctor := &tstree.AgentMethod{
		Name:   "constructor",
		Params: ctorParams,
		Body:   ctorBody,
	}

	// Intent methods.
	methods := make([]*tstree.AgentMethod, 0, len(ag.Intents))
	for _, intent := range ag.Intents {
		m, err := l.lowerIntentDecl(ag.Name, intent)
		if err != nil {
			return nil, err
		}
		methods = append(methods, m)
	}

	return &tstree.AgentClassDecl{
		Doc:         []string{fmt.Sprintf("Agent %s lowered from Mochi.", ag.Name)},
		Name:        className,
		AgentFields: fields,
		Constructor: ctor,
		Methods:     methods,
	}, nil
}

func (l *lowerer) lowerIntentDecl(agentName string, intent aotir.AgentIntentDecl) (*tstree.AgentMethod, error) {
	params := make([]tstree.FuncParam, 0, len(intent.Params))
	for _, p := range intent.Params {
		tn, err := tsTypeForAotirType(p.Type, "")
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent %s intent %s param %s: %w", agentName, intent.Name, p.Name, err)
		}
		params = append(params, tstree.FuncParam{Name: p.Name, Type: tn})
	}

	retType, err := tsTypeForAotirType(intent.ReturnType, "")
	if err != nil {
		return nil, fmt.Errorf("ts lower: agent %s intent %s return: %w", agentName, intent.Name, err)
	}

	var body []tstree.Stmt
	if intent.Body != nil {
		l.insideAgent = true
		body, err = l.lowerBlock(intent.Body.Statements)
		l.insideAgent = false
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent %s intent %s body: %w", agentName, intent.Name, err)
		}
	}

	return &tstree.AgentMethod{
		Name:       intent.Name,
		Params:     params,
		ReturnType: retType,
		Body:       body,
	}, nil
}

// lowerAgentSpawnExpr lowers `spawn Counter(0)` to `new MochiCounter(0)`.
func (l *lowerer) lowerAgentSpawnExpr(e *aotir.AgentSpawnExpr) (tstree.Expr, error) {
	args := make([]tstree.Expr, 0, len(e.InitArgs))
	for i, a := range e.InitArgs {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent spawn arg %d: %w", i, err)
		}
		args = append(args, v)
	}
	return &tstree.RawExpr{
		Text: buildNewExpr("Mochi"+e.AgentName, args),
	}, nil
}

// lowerAgentIntentCallExpr lowers `counter.get()` to `counter.get()`.
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (tstree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: agent intent call recv: %w", err)
	}
	args := make([]tstree.Expr, 0, len(e.Args))
	for i, a := range e.Args {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent intent call arg %d: %w", i, err)
		}
		args = append(args, v)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   e.IntentName,
		Args:     args,
	}, nil
}

// lowerAgentIntentCallStmt lowers an intent call as a statement.
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt) ([]tstree.Stmt, error) {
	recv, err := l.lowerExpr(s.Receiver)
	if err != nil {
		return nil, fmt.Errorf("ts lower: agent intent stmt recv: %w", err)
	}
	args := make([]tstree.Expr, 0, len(s.Args))
	for i, a := range s.Args {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("ts lower: agent intent stmt arg %d: %w", i, err)
		}
		args = append(args, v)
	}
	return []tstree.Stmt{
		&tstree.ExprStmt{Expr: &tstree.MemberCallExpr{
			Receiver: recv,
			Method:   s.IntentName,
			Args:     args,
		}},
	}, nil
}

// buildNewExpr constructs `new ClassName(arg0, arg1, ...)` as a string.
func buildNewExpr(className string, args []tstree.Expr) string {
	var sb strings.Builder
	sb.WriteString("new ")
	sb.WriteString(className)
	sb.WriteByte('(')
	for i, a := range args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.TsString(0))
	}
	sb.WriteByte(')')
	return sb.String()
}

// tsTypeForAotirType maps an aotir.Type to a TypeScript type name.
// Used by agent field/param lowering. For complex types, falls back
// to "unknown" (agents in Phase 9 use scalar fields only).
func tsTypeForAotirType(t aotir.Type, elemName string) (string, error) {
	switch t {
	case aotir.TypeInt:
		return "number", nil
	case aotir.TypeFloat:
		return "number", nil
	case aotir.TypeBool:
		return "boolean", nil
	case aotir.TypeString:
		return "string", nil
	case aotir.TypeUnit:
		return "void", nil
	case aotir.TypeList:
		if elemName != "" {
			return elemName + "[]", nil
		}
		return "unknown[]", nil
	case aotir.TypeMap:
		return "Map<unknown, unknown>", nil
	default:
		return "unknown", nil
	}
}

// stripSelfPrefix removes the `__self->` prefix stamped by the C lowerer
// for agent field references inside intent bodies. The TS backend replaces
// these with `this.FIELD` accesses (handled in lowerAgentIntentCallExpr
// and the VarRef case of lowerExpr when insideAgent is set).
func stripSelfPrefix(name string) string {
	const prefix = "__self->"
	if strings.HasPrefix(name, prefix) {
		return "this." + name[len(prefix):]
	}
	return name
}
