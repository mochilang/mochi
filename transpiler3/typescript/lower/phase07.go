package lower

// Phase 7 lands Mochi's query DSL.
//
// The aotir Phase 8.3 closure-conversion pass desugars
//
//     let evens = from n in nums where n % 2 == 0 select n
//
// into a pre-allocated mutable result list plus an arena-scoped
// ForEachStmt+IfStmt+AssignStmt(AppendExpr) sequence wrapped in a
// QueryScopeStmt:
//
//     let __query0: int[] = []        // LetStmt (Mutable=true) into outer block
//     QueryScopeStmt {
//       Body: {
//         for n in nums {
//           if (n % 2 == 0) {
//             __query0 = append(__query0, n)
//           }
//         }
//       }
//     }
//
// The arena is purely a C concern: it lets the emitter rewrite
// append calls to bump-allocate from a stack-local mochi_arena_t and
// copy the final list to the heap exactly once, avoiding O(n) malloc
// calls inside the inner loop. JavaScript engines have a built-in GC
// and Array.prototype.push is already O(1) amortised, so the TS
// emitter just flattens QueryScopeStmt to its body statements. The
// surrounding LetStmt + ForEachStmt + AssignStmt(AppendExpr) all
// route through the Phase 2 / 3 lowerers untouched.
//
// AppendExpr currently lowers to [...xs, v] via SpreadAppendExpr
// (Phase 3.1). For the Phase 7 corpus this is correct but quadratic
// when the result list grows large. The execution-budget gate
// (TestPhase7QueryPerf) is deferred until Phase 7.4; if it comes due
// the AssignStmt+AppendExpr pattern can be peephole-lowered to an
// in-place .push() call here.

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// lowerQueryScopeStmt strips the arena wrapper and lowers the body
// statements into the surrounding block. The outer LetStmt that
// declared the result var is emitted separately by the aotir lowerer,
// so we do not need to materialise the scope at the TS surface.
func (l *lowerer) lowerQueryScopeStmt(s *aotir.QueryScopeStmt) ([]tstree.Stmt, error) {
	if s == nil || s.Body == nil {
		return nil, nil
	}
	out := make([]tstree.Stmt, 0, len(s.Body.Statements))
	for _, st := range s.Body.Statements {
		lowered, err := l.lowerStmt(st)
		if err != nil {
			return nil, fmt.Errorf("ts lower: query scope: %w", err)
		}
		out = append(out, lowered...)
	}
	return out, nil
}
