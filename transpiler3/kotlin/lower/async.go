// Package lower - Phase 11: Async/await lowering for Kotlin backend.
//
// Uses Java's CompletableFuture for basic async:
//   async { expr }  -> java.util.concurrent.CompletableFuture.supplyAsync { expr }
//   await fut       -> fut.get()
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

// lowerAsyncExpr lowers AsyncExpr to CompletableFuture.supplyAsync { body }.
func (l *lowerer) lowerAsyncExpr(e *aotir.AsyncExpr) (ktree.Expr, error) {
	body, err := l.lowerExpr(e.Body)
	if err != nil {
		return nil, err
	}
	// java.util.concurrent.CompletableFuture.supplyAsync { body }
	code := fmt.Sprintf("java.util.concurrent.CompletableFuture.supplyAsync { %s }", body.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

// lowerAwaitExpr lowers AwaitExpr to (future.get() as BoxedType).
func (l *lowerer) lowerAwaitExpr(e *aotir.AwaitExpr) (ktree.Expr, error) {
	fut, err := l.lowerExpr(e.Future)
	if err != nil {
		return nil, err
	}
	boxed := ktChanBoxedType(e.ElemType) // reuse chan helper — same boxing logic
	code := fmt.Sprintf("(%s.get() as %s)", fut.KtExprString(), boxed)
	return &ktree.RawKtExpr{Code: code}, nil
}

// lowerFutureLetTypeName returns the Kotlin type annotation for a future variable.
func lowerFutureLetTypeName(t aotir.Type) string {
	boxed := ktChanBoxedType(t)
	return fmt.Sprintf("java.util.concurrent.CompletableFuture<%s>", boxed)
}
