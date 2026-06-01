// Phase 9 widens tstree with an agent class node.
//
// Mochi `agent` declarations lower to TypeScript `class` with mutable
// private fields. Each intent becomes a method on the class. This differs
// from Phase 3.4's ClassDecl (for immutable records) because agent classes
// have mutable state and synchronous mutator methods.
//
// AgentClassDecl renders:
//
//	class MochiCounter {
//	    private count: number;
//	    constructor(count: number) { this.count = count; }
//	    get(): number { return this.count; }
//	    increment(): void { this.count = this.count + 1; }
//	}
package tstree

import "strings"

// AgentClassDecl is a top-level class declaration for a Mochi agent.
// Fields are private and mutable; methods correspond to intent declarations.
type AgentClassDecl struct {
	// Doc is the leading comment block (one line per entry).
	Doc []string
	// Name is the class identifier (conventionally "Mochi" + agent name).
	Name string
	// AgentFields lists private mutable fields in source order.
	AgentFields []AgentClassField
	// Constructor is the constructor body.
	Constructor *AgentMethod
	// Methods are the intent method bodies in source order.
	Methods []*AgentMethod
}

// AgentClassField is one private typed field.
type AgentClassField struct {
	Name string
	Type string
}

// AgentMethod is one method (constructor or intent) inside an AgentClassDecl.
type AgentMethod struct {
	Name       string
	Params     []FuncParam
	ReturnType string
	IsAsync    bool
	Body       []Stmt
}

func (d *AgentClassDecl) declNode() {}
func (d *AgentClassDecl) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	inner := pad + "  "
	var b strings.Builder
	for _, line := range d.Doc {
		b.WriteString(pad)
		b.WriteString("// ")
		b.WriteString(line)
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteString("class ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	for _, f := range d.AgentFields {
		b.WriteString(inner)
		b.WriteString("private ")
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Type)
		b.WriteString(";\n")
	}
	if d.Constructor != nil {
		b.WriteString(agentMethodStr(d.Constructor, indent+1))
		b.WriteByte('\n')
	}
	for _, m := range d.Methods {
		b.WriteString(agentMethodStr(m, indent+1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

func agentMethodStr(m *AgentMethod, indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteString(pad)
	if m.IsAsync {
		b.WriteString("async ")
	}
	b.WriteString(m.Name)
	b.WriteByte('(')
	for i, p := range m.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p.Name)
		b.WriteString(": ")
		b.WriteString(p.Type)
	}
	b.WriteByte(')')
	if m.ReturnType != "" {
		b.WriteString(": ")
		b.WriteString(m.ReturnType)
	}
	b.WriteString(" {")
	if len(m.Body) == 0 {
		b.WriteByte('}')
		return b.String()
	}
	b.WriteByte('\n')
	for _, s := range m.Body {
		b.WriteString(s.TsString(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}
