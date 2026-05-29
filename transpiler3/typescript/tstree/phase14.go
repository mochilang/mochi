// Phase 14 fetch surface. The only tstree addition is the
// `await E` expression form, used to mark the cross-runtime
// fetch helper call as async-coloured. The Mochi `fetch URL
// into body` statement desugars to `LetStmt{Init: HttpGetExpr}`
// in aotir; the TS lowerer wraps the HttpGetExpr emit in
// AwaitExpr so user code reads `let body = await mochi_http_get(url);`.
//
// Why a node rather than a RawExpr: an AST node lets the printer
// stay strict-mode clean (no embedded string template surface) and
// gives later phases (15 npm-package, 17 browser) a single place
// to inject re-typing when the helper signature widens (eg. when
// streaming bodies land in 14.2).

package tstree

// AwaitExpr is `await <inner>`. The printer never parenthesises;
// `await` has lower precedence than every unary form the rest of
// the tstree emits, and the consumers we have today are all bound
// to a LetStmt or a CallExpr arg slot where the precedence is
// already inside an unambiguous context.
type AwaitExpr struct {
	Inner Expr
}

func (e *AwaitExpr) exprNode() {}
func (e *AwaitExpr) TsString(_ int) string {
	return "await " + e.Inner.TsString(0)
}
