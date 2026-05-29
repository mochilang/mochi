package lower

import (
	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/python/pysrc"
)

// lowerStreamMakeExpr emits `mochi_make_stream(cap)`. Phase 10.0 streams
// are synchronous broadcast buffers (no producer-side backpressure); the
// `cap` argument is preserved on the MochiStream instance for future use
// but does not bound buffer growth in v1.
func (l *lowerer) lowerStreamMakeExpr(e *aotir.StreamMakeExpr) (pysrc.Expr, error) {
	l.needsStream = true
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, err
	}
	return &pysrc.Call{
		Func: &pysrc.Name{Id: "mochi_make_stream"},
		Args: []pysrc.Expr{cap},
	}, nil
}

// lowerSubMakeExpr emits `mochi_subscribe(stream)`. The new subscriber's
// read cursor starts at the current write position; any value emitted
// before subscribe is invisible to that subscriber (broadcast semantics
// from the C fixture corpus).
func (l *lowerer) lowerSubMakeExpr(e *aotir.SubMakeExpr) (pysrc.Expr, error) {
	l.needsStream = true
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, err
	}
	return &pysrc.Call{
		Func: &pysrc.Name{Id: "mochi_subscribe"},
		Args: []pysrc.Expr{stream},
	}, nil
}

// lowerStreamEmitStmt emits `mochi_emit(stream, val)`. Synchronous append
// to the broadcast buffer; every live subscriber observes the value on
// their next recv_sub call.
func (l *lowerer) lowerStreamEmitStmt(s *aotir.StreamEmitStmt) (pysrc.Stmt, error) {
	l.needsStream = true
	stream, err := l.lowerExpr(s.Stream)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, err
	}
	return &pysrc.ExprStmt{X: &pysrc.Call{
		Func: &pysrc.Name{Id: "mochi_emit"},
		Args: []pysrc.Expr{stream, val},
	}}, nil
}

// lowerSubRecvExpr emits `mochi_recv_sub(sub)`. Advances the subscriber's
// read cursor by one and returns the value at the previous cursor.
// IndexError surfaces if the caller recvs past the latest emit; v1
// fixtures never do this (every recv is preceded by a paired emit).
func (l *lowerer) lowerSubRecvExpr(e *aotir.SubRecvExpr) (pysrc.Expr, error) {
	l.needsStream = true
	sub, err := l.lowerExpr(e.Sub)
	if err != nil {
		return nil, err
	}
	return &pysrc.Call{
		Func: &pysrc.Name{Id: "mochi_recv_sub"},
		Args: []pysrc.Expr{sub},
	}, nil
}
