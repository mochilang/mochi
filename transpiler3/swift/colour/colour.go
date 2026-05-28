// Package colour computes the async/sync colour of each function in the call graph.
package colour

import "mochi/transpiler3/c/aotir"

// Colour represents the async/sync colour of a function.
type Colour int

const (
	Blue Colour = iota // synchronous
	Red                // async
)

// ColourMap maps function names to their resolved Colour.
type ColourMap map[string]Colour

// Analyse colours functions based on async/await usage and call-graph propagation.
// A function is Red if it directly contains AsyncExpr or AwaitExpr nodes, or if it
// calls another Red function. Propagation continues until no new Red functions are found.
func Analyse(prog *aotir.Program) ColourMap {
	cm := make(ColourMap, len(prog.Functions))
	// Seed: functions containing AsyncExpr or AwaitExpr are Red.
	for _, fn := range prog.Functions {
		if containsAsyncOrAwait(fn.Body) {
			cm[fn.Name] = Red
		} else {
			cm[fn.Name] = Blue
		}
	}
	// Propagate: if A calls B and B is Red, A becomes Red.
	changed := true
	for changed {
		changed = false
		for _, fn := range prog.Functions {
			if cm[fn.Name] == Red {
				continue
			}
			if callsRed(fn.Body, cm) {
				cm[fn.Name] = Red
				changed = true
			}
		}
	}
	return cm
}

func containsAsyncOrAwait(b *aotir.Block) bool {
	if b == nil {
		return false
	}
	for _, s := range b.Statements {
		if stmtHasAsyncOrAwait(s) {
			return true
		}
	}
	return false
}

func stmtHasAsyncOrAwait(s aotir.Stmt) bool {
	switch s := s.(type) {
	case *aotir.LetStmt:
		return s.Init != nil && exprHasAsyncOrAwait(s.Init)
	case *aotir.ReturnStmt:
		return s.Value != nil && exprHasAsyncOrAwait(s.Value)
	case *aotir.AssignStmt:
		return exprHasAsyncOrAwait(s.Value)
	case *aotir.IfStmt:
		return exprHasAsyncOrAwait(s.Cond) || containsAsyncOrAwait(s.Then) || containsAsyncOrAwait(s.Else)
	case *aotir.WhileStmt:
		return exprHasAsyncOrAwait(s.Cond) || containsAsyncOrAwait(s.Body)
	case *aotir.CallStmt:
		for _, a := range s.Args {
			if exprHasAsyncOrAwait(a) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

func exprHasAsyncOrAwait(e aotir.Expr) bool {
	switch e := e.(type) {
	case *aotir.AsyncExpr:
		return true
	case *aotir.AwaitExpr:
		return true
	case *aotir.BinaryExpr:
		return exprHasAsyncOrAwait(e.Left) || exprHasAsyncOrAwait(e.Right)
	default:
		_ = e
		return false
	}
}

func callsRed(b *aotir.Block, cm ColourMap) bool {
	if b == nil {
		return false
	}
	for _, s := range b.Statements {
		if stmtCallsRed(s, cm) {
			return true
		}
	}
	return false
}

func stmtCallsRed(s aotir.Stmt, cm ColourMap) bool {
	switch s := s.(type) {
	case *aotir.LetStmt:
		return s.Init != nil && exprCallsRed(s.Init, cm)
	case *aotir.CallStmt:
		return cm[s.Func] == Red
	case *aotir.ReturnStmt:
		return s.Value != nil && exprCallsRed(s.Value, cm)
	default:
		return false
	}
}

func exprCallsRed(e aotir.Expr, cm ColourMap) bool {
	switch e := e.(type) {
	case *aotir.CallExpr:
		return cm[e.Func] == Red
	case *aotir.AsyncExpr:
		return true
	case *aotir.AwaitExpr:
		return true
	default:
		_ = e
		return false
	}
}
