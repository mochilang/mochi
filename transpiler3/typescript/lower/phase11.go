// Phase 11: Async/await lowering.
//
// After the Phase 11 colour pass rewrite, functions containing async
// operations are marked Red. lowerFunction (phase02.go) is updated to
// check the colour map and emit `async function` headers.
//
// AsyncExpr{Body, ElemType} → (async (): Promise<T> => { return body; })()
// AwaitExpr{Future}         → await future
//
// For await_all: the aotir IR lowers `await_all(futures)` to a
// special CallExpr whose Func is "__mochi_await_all". We map that
// to `Promise.all(futures)` with an `await` at the call site.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/colour"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerAsyncExpr(e *aotir.AsyncExpr) (tstree.Expr, error) {
	body, err := l.lowerExpr(e.Body)
	if err != nil {
		return nil, fmt.Errorf("ts lower: async body: %w", err)
	}
	retType, err := tsTypeForAotirType(e.ElemType, "")
	if err != nil {
		retType = "unknown"
	}
	// Emit: (async (): Promise<T> => { return body; })()
	iife := fmt.Sprintf("(async (): Promise<%s> => { return %s; })()", retType, body.TsString(0))
	return &tstree.RawExpr{Text: iife}, nil
}

func (l *lowerer) lowerAwaitExpr(e *aotir.AwaitExpr) (tstree.Expr, error) {
	future, err := l.lowerExpr(e.Future)
	if err != nil {
		return nil, fmt.Errorf("ts lower: await: %w", err)
	}
	return &tstree.RawExpr{Text: "await " + future.TsString(0)}, nil
}

// isAsyncFunction returns true if the function with the given name is
// coloured Red (async) in the colour map.
func isAsyncFunction(name string, colours colour.ColourMap) bool {
	return colours[name] == colour.Red
}
