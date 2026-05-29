// Phase 3 widens tstree with collection literals and index access.
// Lists lower to TypeScript array literals (`[1, 2, 3]`) typed as
// `number[]` / `string[]` / `boolean[]`; `xs[i]` reads through a
// `mochi_list_at` runtime helper so out-of-range reads raise rather
// than yield `undefined` (which `--noUncheckedIndexedAccess` would
// force the caller to handle).
//
// Maps and sets land as later sub-phases and add their own node
// kinds here.
package tstree

import "strings"

// ListLit is `[a, b, c]`. Elems are rendered comma-separated at
// indent 0 so the literal fits on one line; multi-line layout is a
// future sub-phase concern (prettier reflows long literals anyway).
type ListLit struct {
	Elems []Expr
}

func (e *ListLit) exprNode() {}
func (e *ListLit) TsString(_ int) string {
	var b strings.Builder
	b.WriteByte('[')
	for i, el := range e.Elems {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(el.TsString(0))
	}
	b.WriteByte(']')
	return b.String()
}

// ForEachStmt is `for (const VAR of LIST) { BODY }`. The induction
// variable is always declared with `const` because Mochi treats the
// iteration variable as immutable (re-binding inside the body is an
// aotir verifier error) and `const` matches that contract on the TS
// side too. TypeScript doesn't allow a type annotation on the
// for-of binding (the grammar reserves the colon for label syntax);
// `tsc` infers the type from the list's element type instead, which
// matches Mochi's structural inference.
type ForEachStmt struct {
	Var  string
	List Expr
	Body []Stmt
}

func (s *ForEachStmt) stmtNode() {}
func (s *ForEachStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("for (const ")
	b.WriteString(s.Var)
	b.WriteString(" of ")
	b.WriteString(s.List.TsString(0))
	b.WriteString(") {")
	if len(s.Body) == 0 {
		b.WriteByte('}')
		return b.String()
	}
	b.WriteByte('\n')
	for _, t := range s.Body {
		b.WriteString(t.TsString(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// IndexAssignStmt is `RECEIVER[INDEX] = VALUE;`. The index is
// runtime-checked inside the `mochi_list_set` helper rather than
// inlined, so out-of-range mutation raises rather than silently
// extending the array.
type IndexAssignStmt struct {
	Receiver Expr
	Index    Expr
	Value    Expr
}

func (s *IndexAssignStmt) stmtNode() {}
func (s *IndexAssignStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	return pad + s.Receiver.TsString(0) + "[" + s.Index.TsString(0) + "] = " + s.Value.TsString(0) + ";"
}

// MemberAccessExpr is `RECEIVER.MEMBER`. Used for property reads
// such as `xs.length`. Distinct from MemberCallExpr (which calls
// the property) so callers can choose between read and invoke.
type MemberAccessExpr struct {
	Receiver Expr
	Member   string
}

func (e *MemberAccessExpr) exprNode() {}
func (e *MemberAccessExpr) TsString(_ int) string {
	return e.Receiver.TsString(0) + "." + e.Member
}

// SpreadAppendExpr is `[...LIST, TAIL]`. The shape is specific to
// Mochi's functional `append(xs, v)`: spread the existing list and
// add the single tail element. It renders the literal at indent 0
// (callers don't pass indent).
type SpreadAppendExpr struct {
	List Expr
	Tail Expr
}

func (e *SpreadAppendExpr) exprNode() {}
func (e *SpreadAppendExpr) TsString(_ int) string {
	return "[..." + e.List.TsString(0) + ", " + e.Tail.TsString(0) + "]"
}

// NewSetExpr is `new Set<T>([e1, e2, ...])`. Mochi set literals
// (`set{e1, e2, ...}`) lower here; the element type argument is
// emitted explicitly because `tsc --strict` can't infer it for an
// empty literal and emitting it uniformly keeps the printed form
// consistent. An empty literal renders `new Set<T>()`.
type NewSetExpr struct {
	ElemType string
	Elems    []Expr
}

func (e *NewSetExpr) exprNode() {}
func (e *NewSetExpr) TsString(_ int) string {
	var b strings.Builder
	b.WriteString("new Set<")
	b.WriteString(e.ElemType)
	b.WriteString(">(")
	if len(e.Elems) == 0 {
		b.WriteByte(')')
		return b.String()
	}
	b.WriteByte('[')
	for i, el := range e.Elems {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(el.TsString(0))
	}
	b.WriteString("])")
	return b.String()
}

// SetAddNewExpr is `new Set<T>([...recv, elem])`. Mochi's `add(s, x)`
// is functional (returns a fresh set without mutating the input);
// rendering it as a Set constructor over a spread keeps the emit
// expression-shaped (Mochi may use it inside a re-assignment, a
// function argument, or a return statement).
type SetAddNewExpr struct {
	ElemType string
	Receiver Expr
	Elem     Expr
}

func (e *SetAddNewExpr) exprNode() {}
func (e *SetAddNewExpr) TsString(_ int) string {
	return "new Set<" + e.ElemType + ">([..." + e.Receiver.TsString(0) + ", " + e.Elem.TsString(0) + "])"
}

// NewMapExpr is `new Map<K, V>([[k1, v1], [k2, v2], ...])`. Mochi map
// literals lower here; the K/V type arguments are emitted explicitly
// because `tsc --strict` can't always infer them for an empty literal
// (and emitting them everywhere keeps the printed form uniform).
//
// Entries are rendered comma-separated inline. Empty maps emit
// `new Map<K, V>()` (TS allows the parameterless ctor).
type NewMapExpr struct {
	KeyType   string
	ValueType string
	Keys      []Expr
	Values    []Expr // len(Keys) == len(Values), checked at build time
}

func (e *NewMapExpr) exprNode() {}
func (e *NewMapExpr) TsString(_ int) string {
	var b strings.Builder
	b.WriteString("new Map<")
	b.WriteString(e.KeyType)
	b.WriteString(", ")
	b.WriteString(e.ValueType)
	b.WriteString(">(")
	if len(e.Keys) == 0 {
		b.WriteByte(')')
		return b.String()
	}
	b.WriteByte('[')
	for i := range e.Keys {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteByte('[')
		b.WriteString(e.Keys[i].TsString(0))
		b.WriteString(", ")
		b.WriteString(e.Values[i].TsString(0))
		b.WriteByte(']')
	}
	b.WriteString("])")
	return b.String()
}
