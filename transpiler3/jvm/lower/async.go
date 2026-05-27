package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerAsyncExpr lowers AsyncExpr to dev.mochi.runtime.async.Async.run(() -> body).
// The result is an Async<BoxedElem> that resolves when the virtual thread completes.
func (l *lowerer) lowerAsyncExpr(e *aotir.AsyncExpr) (javasrc.Expr, error) {
	body, err := l.lowerExpr(e.Body)
	if err != nil {
		return nil, err
	}
	// Async.run(() -> body)
	lambda := &javasrc.LambdaExpr{
		Params: nil, // no params: Supplier
		Body:   body,
	}
	return &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.async.Async",
		Method: "run",
		Args:   []javasrc.Expr{lambda},
	}, nil
}

// lowerAwaitExpr lowers AwaitExpr to (BoxedElem) future.get().
func (l *lowerer) lowerAwaitExpr(e *aotir.AwaitExpr) (javasrc.Expr, error) {
	fut, err := l.lowerExpr(e.Future)
	if err != nil {
		return nil, err
	}
	// future.await() blocks on the virtual thread and returns the result
	awaitCall := &javasrc.CallExpr{
		Receiver: fut,
		Method:   "await",
		Args:     nil,
	}
	castType := boxedType(e.ElemType)
	return &javasrc.CastExpr{
		Type: castType,
		X:    awaitCall,
	}, nil
}
