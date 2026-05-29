// Phase 11 widens tstree with the minimum surface needed for
// Mochi's panic / try-catch sub-language.
//
// Two observations drive the shape:
//
//  1. Every fixture in the Phase 11 corpus is synchronous: panic
//     is `throw`, try/catch is JS try/catch, the caught value is
//     an integer code (Mochi assigns -1, -2, ... in spec terms,
//     positive 1-9 in the runtime's exit-code mapping). No fixture
//     exercises Promise rejection, AbortError, AggregateError, or
//     fan-in result-aggregation.
//
//  2. JavaScript's native `throw` / `try` / `catch` covers the
//     full surface at zero runtime cost: a `class MochiPanic
//     extends Error` carrying the integer code lets the catch
//     prologue narrow `instanceof MochiPanic` and pull `.code`.
//
// Three nodes are introduced here:
//
//  1. ThrowStmt. `throw NEW_EXPR;` for the panic call site. The
//     value is always a MochiPanic instance built via NewExpr, so
//     no other shape needs modelling.
//
//  2. TryCatchStmt. `try { BODY } catch (BINDING) { HANDLER }`.
//     The binding name is the synthesized error variable; the
//     handler prologue (the `const e: number = ...` line that
//     extracts the panic code) is emitted by the lowerer as the
//     first statements of HandlerBody, not modelled here.
//
//  3. InstanceOfExpr. `EXPR instanceof CLASS_NAME`. Used in the
//     catch prologue to narrow before reading `.code`.

package tstree

import "strings"

// ThrowStmt is `throw VALUE;`. The Value is the throwable
// expression (in practice a NewExpr building a MochiPanic instance,
// but no constraint is enforced at the tstree layer).
type ThrowStmt struct {
	Value Expr
}

func (s *ThrowStmt) stmtNode() {}
func (s *ThrowStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	return pad + "throw " + s.Value.TsString(0) + ";"
}

// TryCatchStmt is `try { BODY } catch (BINDING) { HANDLER }`. The
// binding is the raw caught-value name (e.g. "__mochi_err_0"); the
// handler prologue that extracts the panic code into the
// user-visible catch variable is the lowerer's responsibility and
// appears as the first statements of HandlerBody.
type TryCatchStmt struct {
	// TryBody is the protected block.
	TryBody []Stmt
	// Binding is the catch-clause's raw error variable name.
	Binding string
	// HandlerBody is the catch block (includes the lowerer-injected
	// prologue that pulls `.code` from a MochiPanic).
	HandlerBody []Stmt
}

func (s *TryCatchStmt) stmtNode() {}
func (s *TryCatchStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("try {")
	for _, stmt := range s.TryBody {
		b.WriteByte('\n')
		b.WriteString(stmt.TsString(indent + 1))
	}
	b.WriteByte('\n')
	b.WriteString(pad)
	b.WriteString("} catch (")
	b.WriteString(s.Binding)
	b.WriteString(") {")
	for _, stmt := range s.HandlerBody {
		b.WriteByte('\n')
		b.WriteString(stmt.TsString(indent + 1))
	}
	b.WriteByte('\n')
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// InstanceOfExpr is `OPERAND instanceof CLASS_NAME`. Class is a
// bare identifier string (no path support; Phase 11 only uses it
// to test against `MochiPanic`).
type InstanceOfExpr struct {
	Operand Expr
	Class   string
}

func (e *InstanceOfExpr) exprNode() {}
func (e *InstanceOfExpr) TsString(_ int) string {
	return e.Operand.TsString(0) + " instanceof " + e.Class
}

// CondExpr is the ternary `COND ? THEN : ELSE`. Used by the
// catch-clause prologue to coalesce a non-MochiPanic catch
// (`throw 42` at a JS layer) into `0` so the user's `catch e {...}`
// always sees a number.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (e *CondExpr) exprNode() {}
func (e *CondExpr) TsString(_ int) string {
	return "(" + e.Cond.TsString(0) + " ? " + e.Then.TsString(0) + " : " + e.Else.TsString(0) + ")"
}
