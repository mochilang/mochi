package lower

// Phase 10: Stream and channel lowering for the JVM backend.
//
// Channel lowering (Phase 9.1 aotir nodes):
//   ChanMakeExpr  -> new java.util.concurrent.LinkedBlockingQueue<>(cap)
//   ChanSendStmt  -> ch.put(val)                   (blocks; virtual-thread friendly)
//   ChanRecvExpr  -> ch.take()                     (blocks; virtual-thread friendly)
//
// Stream / subscriber lowering (Phase 9.2 aotir nodes):
//   StreamMakeExpr     -> dev.mochi.runtime.stream.MochiStream.create(cap)
//   StreamEmitStmt     -> stream.emit(val)
//   SubMakeExpr        -> stream.subscribe()
//   SubMakeLimitExpr   -> stream.subscribeWithLimit(limit)
//   SubRecvExpr        -> sub.recv()
//
// The MochiStream runtime class (written to the runtime source tree by this
// phase) wraps SubmissionPublisher<Object> and a per-subscriber
// LinkedBlockingQueue so recv() can block the calling virtual thread.

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// ---- Channel lowering ----

// lowerChanMakeExpr lowers ChanMakeExpr to
// new java.util.concurrent.LinkedBlockingQueue<>((int) cap)
func (l *lowerer) lowerChanMakeExpr(e *aotir.ChanMakeExpr) (javasrc.Expr, error) {
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, err
	}
	intCap := &javasrc.CastExpr{Type: javasrc.TypeRef{Name: "int"}, X: cap}
	return &javasrc.NewExpr{
		Type: javasrc.TypeRef{Name: "java.util.concurrent.LinkedBlockingQueue"},
		Args: []javasrc.Expr{intCap},
	}, nil
}

// lowerChanSendStmt lowers ChanSendStmt to
// try { ch.put(val); } catch (InterruptedException $$ie) { Thread.currentThread().interrupt(); }
func (l *lowerer) lowerChanSendStmt(s *aotir.ChanSendStmt) (javasrc.Stmt, error) {
	ch, err := l.lowerExpr(s.Chan)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, err
	}
	putCall := &javasrc.ExprStmt{X: &javasrc.CallExpr{
		Receiver: ch,
		Method:   "put",
		Args:     []javasrc.Expr{val},
	}}
	return &javasrc.TryCatchStmt{
		Body:     javasrc.Block{Stmts: []javasrc.Stmt{putCall}},
		CatchVar: "$$ie",
		CatchTyp: javasrc.TypeRef{Name: "InterruptedException"},
		CatchBody: javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.ExprStmt{X: &javasrc.StaticCallExpr{
				Class:  "Thread.currentThread()",
				Method: "interrupt",
			}},
		}},
	}, nil
}

// lowerChanRecvExpr lowers ChanRecvExpr to
// (BoxedElemType) dev.mochi.runtime.stream.ChanUtil.take(ch)
// The explicit cast is necessary because the raw LinkedBlockingQueue returns Object,
// and Java cannot unbox Object to a primitive type without an explicit cast.
func (l *lowerer) lowerChanRecvExpr(e *aotir.ChanRecvExpr) (javasrc.Expr, error) {
	ch, err := l.lowerExpr(e.Chan)
	if err != nil {
		return nil, err
	}
	take := &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.stream.ChanUtil",
		Method: "take",
		Args:   []javasrc.Expr{ch},
	}
	// Wrap in a cast to the boxed element type so Java can unbox to primitive.
	castType := boxedType(e.ElemType)
	return &javasrc.CastExpr{Type: castType, X: take}, nil
}

// ---- Stream / subscriber lowering ----

// lowerStreamMakeExpr lowers StreamMakeExpr to
// dev.mochi.runtime.stream.MochiStream.create((int) cap)
func (l *lowerer) lowerStreamMakeExpr(e *aotir.StreamMakeExpr) (javasrc.Expr, error) {
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, err
	}
	intCap := &javasrc.CastExpr{Type: javasrc.TypeRef{Name: "int"}, X: cap}
	return &javasrc.StaticCallExpr{
		Class:  "dev.mochi.runtime.stream.MochiStream",
		Method: "create",
		Args:   []javasrc.Expr{intCap},
	}, nil
}

// lowerStreamEmitStmt lowers StreamEmitStmt to
// try { stream.emit(val); } catch (InterruptedException $$ie) { Thread.currentThread().interrupt(); }
func (l *lowerer) lowerStreamEmitStmt(s *aotir.StreamEmitStmt) (javasrc.Stmt, error) {
	stream, err := l.lowerExpr(s.Stream)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, err
	}
	emitCall := &javasrc.ExprStmt{X: &javasrc.CallExpr{
		Receiver: stream,
		Method:   "emit",
		Args:     []javasrc.Expr{val},
	}}
	return &javasrc.TryCatchStmt{
		Body:     javasrc.Block{Stmts: []javasrc.Stmt{emitCall}},
		CatchVar: "$$ie",
		CatchTyp: javasrc.TypeRef{Name: "InterruptedException"},
		CatchBody: javasrc.Block{Stmts: []javasrc.Stmt{
			&javasrc.ExprStmt{X: &javasrc.StaticCallExpr{
				Class:  "Thread.currentThread()",
				Method: "interrupt",
			}},
		}},
	}, nil
}

// lowerSubMakeExpr lowers SubMakeExpr to stream.subscribe()
func (l *lowerer) lowerSubMakeExpr(e *aotir.SubMakeExpr) (javasrc.Expr, error) {
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, err
	}
	return &javasrc.CallExpr{
		Receiver: stream,
		Method:   "subscribe",
		Args:     nil,
	}, nil
}

// lowerSubMakeLimitExpr lowers SubMakeLimitExpr to stream.subscribeWithLimit((int) limit)
func (l *lowerer) lowerSubMakeLimitExpr(e *aotir.SubMakeLimitExpr) (javasrc.Expr, error) {
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, err
	}
	limit, err := l.lowerExpr(e.Limit)
	if err != nil {
		return nil, err
	}
	intLimit := &javasrc.CastExpr{Type: javasrc.TypeRef{Name: "int"}, X: limit}
	return &javasrc.CallExpr{
		Receiver: stream,
		Method:   "subscribeWithLimit",
		Args:     []javasrc.Expr{intLimit},
	}, nil
}

// lowerSubRecvExpr lowers SubRecvExpr to (BoxedElemType) sub.recv()
// The explicit cast is necessary because MochiSub.recv() returns Object (type-erased),
// and Java cannot unbox Object to a primitive type without an explicit cast.
func (l *lowerer) lowerSubRecvExpr(e *aotir.SubRecvExpr) (javasrc.Expr, error) {
	sub, err := l.lowerExpr(e.Sub)
	if err != nil {
		return nil, err
	}
	recv := &javasrc.CallExpr{
		Receiver: sub,
		Method:   "recv",
		Args:     nil,
	}
	// Wrap in a cast to the boxed element type so Java can unbox to primitive.
	castType := boxedType(e.ElemType)
	return &javasrc.CastExpr{Type: castType, X: recv}, nil
}

// ---- Type helpers for chan / stream / sub ----

// lowerChanType returns java.util.concurrent.LinkedBlockingQueue (raw).
func lowerChanType() javasrc.TypeRef {
	return javasrc.TypeRef{Name: "java.util.concurrent.LinkedBlockingQueue"}
}

// lowerStreamType returns dev.mochi.runtime.stream.MochiStream (raw).
func lowerStreamType() javasrc.TypeRef {
	return javasrc.TypeRef{Name: "dev.mochi.runtime.stream.MochiStream"}
}

// lowerSubType returns dev.mochi.runtime.stream.MochiSub (raw).
func lowerSubType() javasrc.TypeRef {
	return javasrc.TypeRef{Name: "dev.mochi.runtime.stream.MochiSub"}
}

// errUnsupportedChanStreamType returns a descriptive error for an unsupported
// chan/stream/sub type in a context that needs an exact Java type.
func errUnsupportedChanStreamType(t aotir.Type) error {
	return fmt.Errorf("jvm/lower: unexpected type %v in chan/stream/sub context", t)
}
