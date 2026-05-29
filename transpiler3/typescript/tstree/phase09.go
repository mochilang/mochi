// Phase 9 widens tstree with the minimum agent surface needed for
// MEP-52's agent sub-language. The aotir-side `AgentDecl` (one
// `agent NAME { var f: T = init; intent N(...) {...} ...}` block)
// is observably identical to a synchronous state-holder: every
// fixture in the Phase 9 corpus is a single-threaded state machine
// with no cross-agent mailboxes, no AbortController surface, and no
// supervision tree. The C and Rust transpilers both compile agents
// to plain struct + method-style functions (a single `&mut self`
// reference threaded through every intent). The TS side picks the
// closer-shape equivalent: a class with mutable fields whose
// intents become method declarations dispatched via `c.intent()`.
//
// MEP-52 §Phase 9 budgeted ~8 KB gzipped for an AsyncIterableQueue
// + AbortController runtime under `@mochi/runtime/agent` so the TS
// emit could match the spec's async coloring story (Phase 11). The
// audit found: every Phase 9 fixture is colourless (Blue), the
// `c.intent()` call is observationally indistinguishable from a
// regular method call, and the runtime engine would not move any
// fixture's stdout. Shipping the runtime engine therefore costs
// 8 KB and a new public API for zero behaviour benefit on the
// Phase 9 corpus.
//
// Three nodes are introduced here:
//
//   1. AgentClassDecl. A class declaration with mutable fields, a
//      private constructor that splats an opts bag, a static of()
//      factory parallel to Mochi's `Counter { count: 0 }` literal,
//      and method declarations holding each intent's body. Fields
//      are NOT readonly (records are; agents are not).
//
//   2. MethodDecl. One method inside an AgentClassDecl: name,
//      params, return type, body. The shape mirrors FuncDecl
//      minus the `function` keyword.
//
//   3. MemberAssignStmt. `RECEIVER.MEMBER = VALUE;`. The aotir
//      intent body lowers `__self->X = ...` to a member assignment
//      on `this` (the receiver of the dispatching method). Reads
//      reuse the existing MemberAccessExpr.

package tstree

import "strings"

// AgentClassDecl is a top-level `class NAME { F: T; ... private
// constructor(opts: { F: T; ... }) { this.F = opts.F; ... } static
// of(opts: { F: T; ... }): NAME { return new NAME(opts); } method1
// (...) {...} ... }`.
//
// Field order is the order Fields holds. Method order is the order
// Methods holds. Both must be set by the lowerer in source order
// (agent-decl order for intents, agent-field order for fields). The
// emit pass relies on this for Phase 16 byte-equal reproducibility.
//
// Fields are deliberately mutable (no readonly): an agent's whole
// purpose is to mutate state in response to intent dispatch. The TS
// `--strict` flag still type-checks every assignment.
type AgentClassDecl struct {
	// Doc is the leading `//` comment block, one entry per line.
	Doc []string
	// Name is the emitted class identifier (agent name).
	Name string
	// Fields lists (Name, Type) pairs in agent-decl source order.
	Fields []ClassField
	// Methods lists (Name, Params, ReturnType, Body) in agent-decl
	// intent-source order.
	Methods []*MethodDecl
}

// MethodDecl is one method inside an AgentClassDecl. The shape
// mirrors FuncDecl except for the leading `function` keyword and
// the indentation level (a method is always indented one step from
// the enclosing class).
type MethodDecl struct {
	// Doc is the leading `//` comment block.
	Doc []string
	// Name is the method identifier.
	Name string
	// Params lists formal parameters.
	Params []FuncParam
	// ReturnType is the declared return type.
	ReturnType string
	// Body is the statement list inside the braces.
	Body []Stmt
}

func (d *AgentClassDecl) declNode() {}
func (d *AgentClassDecl) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	inner := pad + "  "
	innerInner := inner + "  "
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
	for _, f := range d.Fields {
		b.WriteString(inner)
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Type)
		b.WriteString(";\n")
	}
	b.WriteString(inner)
	b.WriteString("private constructor(opts: { ")
	for i, f := range d.Fields {
		if i > 0 {
			b.WriteString("; ")
		}
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Type)
	}
	b.WriteString("; }) {\n")
	for _, f := range d.Fields {
		b.WriteString(innerInner)
		b.WriteString("this.")
		b.WriteString(f.Name)
		b.WriteString(" = opts.")
		b.WriteString(f.Name)
		b.WriteString(";\n")
	}
	b.WriteString(inner)
	b.WriteString("}\n")
	b.WriteString(inner)
	b.WriteString("static of(opts: { ")
	for i, f := range d.Fields {
		if i > 0 {
			b.WriteString("; ")
		}
		b.WriteString(f.Name)
		b.WriteString(": ")
		b.WriteString(f.Type)
	}
	b.WriteString("; }): ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	b.WriteString(innerInner)
	b.WriteString("return new ")
	b.WriteString(d.Name)
	b.WriteString("(opts);\n")
	b.WriteString(inner)
	b.WriteString("}\n")
	for _, m := range d.Methods {
		b.WriteString(m.tsStringMethod(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// tsStringMethod renders one method at the given indent (used by
// AgentClassDecl; not exported because methods only live inside a
// class).
func (m *MethodDecl) tsStringMethod(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	for _, line := range m.Doc {
		b.WriteString(pad)
		b.WriteString("// ")
		b.WriteString(line)
		b.WriteByte('\n')
	}
	b.WriteString(pad)
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
	b.WriteString("): ")
	b.WriteString(m.ReturnType)
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

// MemberAssignStmt is `RECEIVER.MEMBER = VALUE;`. The aotir intent
// body lowers `__self->X = ...` to a member assignment on `this`:
//   MemberAssignStmt{
//     Receiver: IdentExpr{"this"},
//     Member:   "X",
//     Value:    ...,
//   }
// Reads on `__self->X` use the existing MemberAccessExpr; only the
// write side needs a dedicated statement node because AssignStmt
// takes a string Name field and cannot model a member target.
type MemberAssignStmt struct {
	Receiver Expr
	Member   string
	Value    Expr
}

func (s *MemberAssignStmt) stmtNode() {}
func (s *MemberAssignStmt) TsString(indent int) string {
	return strings.Repeat("  ", indent) + s.Receiver.TsString(0) + "." + s.Member + " = " + s.Value.TsString(0) + ";"
}
