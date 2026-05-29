// Phase 6 widens tstree with the nodes needed for closures and
// higher-order functions:
//
//   - ArrowExpr: a TypeScript arrow-function expression
//     `(p0: T0, p1: T1): R => { body }` or the parenthesised form
//     `(p0: T0): R => expr` when the body is a single expression
//     returned with `return E;`.
//
// The arrow form is the natural lowering for Mochi `fun(x): R =>
// expr` anonymous closures because (a) arrow callables close over
// their lexical scope automatically (no env struct needed, JS does
// the right thing), (b) the surrounding `const f: (x: T) => R = ...`
// type annotation gives tsc enough information to type-check call
// sites without inferring through the function body, and (c) the
// `as` cast Phase 5 uses for variant constructors is unnecessary
// here because the slot's declared type is already the arrow form.
//
// Why arrow and not `function () { ... }`. A `function` expression
// has its own `this` binding and its `arguments` magic, neither of
// which Mochi observes. Arrow functions inherit `this` from the
// enclosing scope, which matches Mochi's lexical-only model. They
// also serialise more compactly (one `=>` token versus the
// `function () {}` triad) which keeps the emit easier to read.
package tstree

import (
	"strings"
)

// ArrowExpr is `(p0: T0, p1: T1): R => { body }`. When ExprBody is
// non-nil the printer uses the concise form `(...) => ExprBody`
// (no braces, no `return`); when ExprBody is nil it falls back to
// the block form. Phase 6 only emits the block form because the
// aotir IR always carries a Body block with an explicit ReturnStmt;
// the ExprBody hook is reserved for a future lowerer optimisation
// that recognises single-statement `return E;` bodies and inlines
// them, mirroring tsc's own output for arrow literals.
//
// ReturnType is the declared return type. The lowerer always sets
// it (even for `void`) so tsc never has to infer through the
// closure body; that keeps the strict-mode emit clean of
// `noImplicitReturns` and `noImplicitAny` warnings.
//
// Determinism: Params are emitted in source order; the order is the
// caller's responsibility.
type ArrowExpr struct {
	Params     []FuncParam
	ReturnType string
	Body       []Stmt // block form when ExprBody is nil
	ExprBody   Expr   // concise form; takes precedence over Body when non-nil
}

func (e *ArrowExpr) exprNode() {}
func (e *ArrowExpr) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteByte('(')
	for i, p := range e.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p.Name)
		b.WriteString(": ")
		b.WriteString(p.Type)
	}
	b.WriteString("): ")
	b.WriteString(e.ReturnType)
	b.WriteString(" => ")
	if e.ExprBody != nil {
		b.WriteString(e.ExprBody.TsString(0))
		return b.String()
	}
	b.WriteByte('{')
	if len(e.Body) == 0 {
		b.WriteByte('}')
		return b.String()
	}
	b.WriteByte('\n')
	for _, s := range e.Body {
		b.WriteString(s.TsString(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// ParenExpr wraps an Expr in `(...)`. The printer applies it
// verbatim, no precedence inference. Phase 6 uses it when the
// callee of a CallExpr is itself an ArrowExpr (a directly-invoked
// arrow literal: `((x) => x + 1)(3)`); without the parens TS
// reads the body's `{` as an object literal in some contexts.
type ParenExpr struct {
	Inner Expr
}

func (e *ParenExpr) exprNode() {}
func (e *ParenExpr) TsString(_ int) string {
	return "(" + e.Inner.TsString(0) + ")"
}
