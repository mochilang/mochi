// Package colour assigns each aotir function a Blue (sync) or Red
// (async) colour. Phase 11 replaces the trivial all-Blue stub with
// a real call-graph fixed-point pass:
//
//  1. Seed: any function whose body contains AsyncExpr or AwaitExpr
//     is Red. The runtime builtins `mochi_http_get` and
//     `mochi_llm_generate` are also seeded Red (they invoke I/O).
//
//  2. Propagate: if function f is Red and calls user-defined function
//     g (by name in a CallExpr), mark g Red. Repeat to fixpoint.
//
//  3. Default Blue: any function not reached by step 1-2 stays Blue.
//
// The ColourMap type and Colour constants are unchanged from Phase 1
// so all call sites (lower.go) need zero modification.
package colour

import "mochi/transpiler3/c/aotir"

// Colour is the binary colour assigned to a function.
type Colour int

const (
	// Blue means the function body never awaits; it lowers to a
	// synchronous TS `function` (no `async` modifier).
	Blue Colour = iota
	// Red means the function body contains at least one await
	// site (transitively); it lowers to `async function` and the
	// return type wraps in `Promise<...>`.
	Red
)

// ColourMap maps a function (by aotir.Function.Name) to its
// computed colour.
type ColourMap map[string]Colour

// asyncBuiltins are runtime helpers that are always async (they do I/O).
var asyncBuiltins = map[string]bool{
	"mochi_http_get":     true,
	"mochi_llm_generate": true,
}

// Compute returns a ColourMap for prog using a call-graph fixed-point
// colour pass. Red = async (contains await or calls async); Blue = sync.
func Compute(prog *aotir.Program) ColourMap {
	if prog == nil {
		return ColourMap{}
	}
	out := make(ColourMap, len(prog.Functions))

	// Step 1: seed from async/await expression nodes.
	for _, fn := range prog.Functions {
		if fn.Body != nil && fnContainsAsync(fn.Body) {
			out[fn.Name] = Red
		}
	}

	// Step 2: propagate Red through call graph to fixpoint.
	changed := true
	for changed {
		changed = false
		for _, fn := range prog.Functions {
			if out[fn.Name] == Red {
				continue
			}
			if fn.Body != nil && fnCallsAnyRed(fn.Body, out) {
				out[fn.Name] = Red
				changed = true
			}
		}
	}

	// Step 3: default Blue for any function not marked Red.
	for _, fn := range prog.Functions {
		if _, ok := out[fn.Name]; !ok {
			out[fn.Name] = Blue
		}
	}
	return out
}

// fnContainsAsync returns true when the function body directly contains
// an AsyncExpr or AwaitExpr node (depth-first walk).
func fnContainsAsync(b *aotir.Block) bool {
	if b == nil {
		return false
	}
	for _, s := range b.Statements {
		if stmtContainsAsync(s) {
			return true
		}
	}
	return false
}

func stmtContainsAsync(s aotir.Stmt) bool {
	switch v := s.(type) {
	case *aotir.LetStmt:
		return v.Init != nil && exprContainsAsync(v.Init)
	case *aotir.AssignStmt:
		return exprContainsAsync(v.Value)
	case *aotir.CallStmt:
		for _, a := range v.Args {
			if exprContainsAsync(a) {
				return true
			}
		}
		return false
	case *aotir.ReturnStmt:
		return v.Value != nil && exprContainsAsync(v.Value)
	case *aotir.IfStmt:
		return exprContainsAsync(v.Cond) ||
			fnContainsAsync(v.Then) ||
			fnContainsAsync(v.Else)
	case *aotir.WhileStmt:
		return exprContainsAsync(v.Cond) || fnContainsAsync(v.Body)
	case *aotir.ForRangeStmt:
		return exprContainsAsync(v.Start) || exprContainsAsync(v.End) || fnContainsAsync(v.Body)
	case *aotir.ForEachStmt:
		return exprContainsAsync(v.List) || fnContainsAsync(v.Body)
	case *aotir.TryCatchStmt:
		return fnContainsAsync(v.TryBody) || fnContainsAsync(v.CatchBody)
	case *aotir.QueryScopeStmt:
		return fnContainsAsync(v.Body)
	default:
		return false
	}
}

func exprContainsAsync(e aotir.Expr) bool {
	if e == nil {
		return false
	}
	switch v := e.(type) {
	case *aotir.AsyncExpr:
		return true
	case *aotir.AwaitExpr:
		return true
	case *aotir.BinaryExpr:
		return exprContainsAsync(v.Left) || exprContainsAsync(v.Right)
	case *aotir.UnaryExpr:
		return exprContainsAsync(v.Operand)
	case *aotir.CallExpr:
		for _, a := range v.Args {
			if exprContainsAsync(a) {
				return true
			}
		}
		return false
	case *aotir.FunCallExpr:
		if exprContainsAsync(v.Callee) {
			return true
		}
		for _, a := range v.Args {
			if exprContainsAsync(a) {
				return true
			}
		}
		return false
	case *aotir.AgentIntentCallExpr:
		for _, a := range v.Args {
			if exprContainsAsync(a) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

// fnCallsAnyRed returns true when the function body contains a CallExpr
// targeting a Red function (or a known async builtin).
func fnCallsAnyRed(b *aotir.Block, colours ColourMap) bool {
	if b == nil {
		return false
	}
	for _, s := range b.Statements {
		if stmtCallsAnyRed(s, colours) {
			return true
		}
	}
	return false
}

func stmtCallsAnyRed(s aotir.Stmt, colours ColourMap) bool {
	switch v := s.(type) {
	case *aotir.LetStmt:
		return v.Init != nil && exprCallsAnyRed(v.Init, colours)
	case *aotir.AssignStmt:
		return exprCallsAnyRed(v.Value, colours)
	case *aotir.CallStmt:
		if colours[v.Func] == Red || asyncBuiltins[v.Func] {
			return true
		}
		for _, a := range v.Args {
			if exprCallsAnyRed(a, colours) {
				return true
			}
		}
		return false
	case *aotir.ReturnStmt:
		return v.Value != nil && exprCallsAnyRed(v.Value, colours)
	case *aotir.IfStmt:
		return exprCallsAnyRed(v.Cond, colours) ||
			fnCallsAnyRed(v.Then, colours) ||
			fnCallsAnyRed(v.Else, colours)
	case *aotir.WhileStmt:
		return exprCallsAnyRed(v.Cond, colours) || fnCallsAnyRed(v.Body, colours)
	case *aotir.ForRangeStmt:
		return exprCallsAnyRed(v.Start, colours) || exprCallsAnyRed(v.End, colours) || fnCallsAnyRed(v.Body, colours)
	case *aotir.ForEachStmt:
		return exprCallsAnyRed(v.List, colours) || fnCallsAnyRed(v.Body, colours)
	case *aotir.TryCatchStmt:
		return fnCallsAnyRed(v.TryBody, colours) || fnCallsAnyRed(v.CatchBody, colours)
	case *aotir.QueryScopeStmt:
		return fnCallsAnyRed(v.Body, colours)
	default:
		return false
	}
}

func exprCallsAnyRed(e aotir.Expr, colours ColourMap) bool {
	if e == nil {
		return false
	}
	switch v := e.(type) {
	case *aotir.CallExpr:
		if colours[v.Func] == Red || asyncBuiltins[v.Func] {
			return true
		}
		for _, a := range v.Args {
			if exprCallsAnyRed(a, colours) {
				return true
			}
		}
		return false
	case *aotir.FunCallExpr:
		if exprCallsAnyRed(v.Callee, colours) {
			return true
		}
		for _, a := range v.Args {
			if exprCallsAnyRed(a, colours) {
				return true
			}
		}
		return false
	case *aotir.BinaryExpr:
		return exprCallsAnyRed(v.Left, colours) || exprCallsAnyRed(v.Right, colours)
	case *aotir.UnaryExpr:
		return exprCallsAnyRed(v.Operand, colours)
	case *aotir.AwaitExpr:
		return exprCallsAnyRed(v.Future, colours)
	case *aotir.AsyncExpr:
		return exprCallsAnyRed(v.Body, colours)
	default:
		return false
	}
}
