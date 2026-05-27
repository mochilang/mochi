package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerQueryScopeStmt lowers an aotir.QueryScopeStmt to Java.
//
// In C, QueryScopeStmt wraps the loop body in an arena scope for efficient
// temporary allocation. In JVM, no arena is needed: we simply lower the
// body block as a sequence of statements. The result variable (ResultVar)
// is declared by the LetStmt that precedes QueryScopeStmt in the outer block;
// the body just appends elements into it.
//
// C arena semantics: 1) alloc arena, 2) run body, 3) copy heap, 4) free.
// JVM semantics: run body directly (GC handles memory).
func (l *lowerer) lowerQueryScopeStmt(s *aotir.QueryScopeStmt) (javasrc.Stmt, error) {
	// Lower the body block and inline its statements into a single Block.
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, fmt.Errorf("jvm/lower: QueryScopeStmt body: %w", err)
	}
	// Return the block directly so its statements are emitted in place.
	return &body, nil
}

// lowerListSortAscExpr lowers aotir.ListSortAscExpr to a Java stream sorted().
//
// Mochi `sort by expr` desugars to ListSortAscExpr which sorts the result list
// ascending. The JVM lowering uses stream().sorted().collect(toList()) and wraps
// the result in a new ArrayList so further appends remain possible.
func (l *lowerer) lowerListSortAscExpr(e *aotir.ListSortAscExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("jvm/lower: ListSortAscExpr receiver: %w", err)
	}
	// xs.stream()
	stream := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "stream",
		Args:     nil,
	}
	// .sorted()  -- natural order for Comparable elements (Long, String, Double)
	sorted := &javasrc.CallExpr{
		Receiver: stream,
		Method:   "sorted",
		Args:     nil,
	}
	// .collect(java.util.stream.Collectors.toList())
	toList := &javasrc.StaticCallExpr{
		Class:  "java.util.stream.Collectors",
		Method: "toList",
		Args:   nil,
	}
	collected := &javasrc.CallExpr{
		Receiver: sorted,
		Method:   "collect",
		Args:     []javasrc.Expr{toList},
	}
	// Wrap in new ArrayList<>(...) so the result is mutable.
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: "java.util.ArrayList"},
		Args: []javasrc.Expr{collected},
	}, nil
}

// lowerListSliceExpr lowers aotir.ListSliceExpr to ListUtil.slice(xs, start, end).
//
// Mochi `skip N / take N` desugars to ListSliceExpr{Start, End}. The helper
// clamps indices and returns a new ArrayList.
func (l *lowerer) lowerListSliceExpr(e *aotir.ListSliceExpr) (javasrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, fmt.Errorf("jvm/lower: ListSliceExpr receiver: %w", err)
	}
	start, err := l.lowerExpr(e.Start)
	if err != nil {
		return nil, fmt.Errorf("jvm/lower: ListSliceExpr start: %w", err)
	}
	end, err := l.lowerExpr(e.End)
	if err != nil {
		return nil, fmt.Errorf("jvm/lower: ListSliceExpr end: %w", err)
	}
	// dev.mochi.runtime.coll.ListUtil.slice(xs, start, end)
	return &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.coll.ListUtil",
		Method: "slice",
		Args:   []javasrc.Expr{recv, start, end},
	}, nil
}
