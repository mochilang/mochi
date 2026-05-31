// Package colour assigns a sync/async colour to every aotir function.
// Phase 11 ships the genuine call-graph fixed-point analyser: any function
// containing AsyncExpr or AwaitExpr is Red; any function calling a Red
// function is also Red, propagated to fixpoint.
package colour

import (
	"mochi/transpiler3/c/aotir"
)

// Colour is the effect colour of a function.
type Colour int

const (
	// Green is a sync (non-async) function.
	Green Colour = iota
	// Red is an async function.
	Red
)

// ColourMap maps mangled function name to its Colour.
type ColourMap map[string]Colour

// asyncBuiltins are runtime helpers that are always async (I/O blocking).
var asyncBuiltins = map[string]bool{
	"mochi_http_get":     true,
	"mochi_llm_generate": true,
}

// Analyse returns a ColourMap for prog using a call-graph fixed-point:
// 1. Seed: any function whose body contains AsyncExpr or AwaitExpr is Red.
// 2. Propagate: if a Red function is called from another function, that
//    caller also becomes Red. Repeat until no new Red functions appear.
// 3. Default remaining functions to Green.
func Analyse(prog *aotir.Program) ColourMap {
	m := make(ColourMap, len(prog.Functions))
	// Seed from async/await nodes.
	for _, fn := range prog.Functions {
		if fnContainsAsync(fn.Body) {
			m[fn.Name] = Red
		}
	}
	// Propagate to fixpoint.
	changed := true
	for changed {
		changed = false
		for _, fn := range prog.Functions {
			if m[fn.Name] == Red {
				continue
			}
			if fnCallsAnyRed(fn.Body, m) {
				m[fn.Name] = Red
				changed = true
			}
		}
	}
	// Default to Green.
	for _, fn := range prog.Functions {
		if _, ok := m[fn.Name]; !ok {
			m[fn.Name] = Green
		}
	}
	return m
}

// fnContainsAsync reports whether any stmt/expr in b is AsyncExpr or AwaitExpr.
func fnContainsAsync(b *aotir.Block) bool {
	if b == nil {
		return false
	}
	for _, s := range b.Statements {
		if stmtHasAsync(s) {
			return true
		}
	}
	return false
}

func stmtHasAsync(s aotir.Stmt) bool {
	switch s := s.(type) {
	case *aotir.LetStmt:
		return s.Init != nil && exprHasAsync(s.Init)
	case *aotir.ReturnStmt:
		return s.Value != nil && exprHasAsync(s.Value)
	case *aotir.AssignStmt:
		return exprHasAsync(s.Value)
	case *aotir.IfStmt:
		return exprHasAsync(s.Cond) || fnContainsAsync(s.Then) || fnContainsAsync(s.Else)
	case *aotir.WhileStmt:
		return exprHasAsync(s.Cond) || fnContainsAsync(s.Body)
	case *aotir.ForEachStmt:
		return exprHasAsync(s.List) || fnContainsAsync(s.Body)
	case *aotir.CallStmt:
		for _, a := range s.Args {
			if exprHasAsync(a) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

func exprHasAsync(e aotir.Expr) bool {
	if e == nil {
		return false
	}
	switch e := e.(type) {
	case *aotir.AsyncExpr:
		return true
	case *aotir.AwaitExpr:
		return true
	case *aotir.BinaryExpr:
		return exprHasAsync(e.Left) || exprHasAsync(e.Right)
	case *aotir.UnaryExpr:
		return exprHasAsync(e.Operand)
	case *aotir.CallExpr:
		if asyncBuiltins[e.Func] {
			return true
		}
		for _, a := range e.Args {
			if exprHasAsync(a) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

// fnCallsAnyRed reports whether b contains a call to a Red function.
func fnCallsAnyRed(b *aotir.Block, m ColourMap) bool {
	if b == nil {
		return false
	}
	for _, s := range b.Statements {
		if stmtCallsRed(s, m) {
			return true
		}
	}
	return false
}

func stmtCallsRed(s aotir.Stmt, m ColourMap) bool {
	switch s := s.(type) {
	case *aotir.LetStmt:
		return s.Init != nil && exprCallsRed(s.Init, m)
	case *aotir.ReturnStmt:
		return s.Value != nil && exprCallsRed(s.Value, m)
	case *aotir.AssignStmt:
		return exprCallsRed(s.Value, m)
	case *aotir.CallStmt:
		if m[s.Func] == Red || asyncBuiltins[s.Func] {
			return true
		}
		for _, a := range s.Args {
			if exprCallsRed(a, m) {
				return true
			}
		}
		return false
	case *aotir.IfStmt:
		return exprCallsRed(s.Cond, m) || fnCallsAnyRed(s.Then, m) || fnCallsAnyRed(s.Else, m)
	case *aotir.WhileStmt:
		return exprCallsRed(s.Cond, m) || fnCallsAnyRed(s.Body, m)
	case *aotir.ForEachStmt:
		return exprCallsRed(s.List, m) || fnCallsAnyRed(s.Body, m)
	default:
		return false
	}
}

func exprCallsRed(e aotir.Expr, m ColourMap) bool {
	if e == nil {
		return false
	}
	switch e := e.(type) {
	case *aotir.CallExpr:
		if m[e.Func] == Red || asyncBuiltins[e.Func] {
			return true
		}
		for _, a := range e.Args {
			if exprCallsRed(a, m) {
				return true
			}
		}
		return false
	case *aotir.AsyncExpr:
		return true
	case *aotir.AwaitExpr:
		return true
	case *aotir.BinaryExpr:
		return exprCallsRed(e.Left, m) || exprCallsRed(e.Right, m)
	default:
		return false
	}
}
