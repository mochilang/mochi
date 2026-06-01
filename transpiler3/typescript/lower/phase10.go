// Phase 10: Stream and channel lowering.
//
// Mochi streams and channels use synchronous semantics in the TS backend
// (matching Go, Swift, and Ruby backends). No async/await is involved.
// A MochiStream<T> broadcasts to all subscribers; MochiSub<T> buffers
// incoming values and serves them via recv(). MochiChan<T> is a simple
// FIFO queue.
//
// Runtime helpers (MochiStream<T>, MochiSub<T>, MochiChan<T>) are emitted
// in the prelude when needed, controlled by runtimeFlags.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

func (l *lowerer) lowerStreamMakeExpr(e *aotir.StreamMakeExpr) (tstree.Expr, error) {
	l.runtime.needsMochiStream = true
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, fmt.Errorf("ts lower: stream make cap: %w", err)
	}
	return &tstree.RawExpr{Text: "new MochiStream(" + cap.TsString(0) + ")"}, nil
}

func (l *lowerer) lowerStreamEmitStmt(s *aotir.StreamEmitStmt) ([]tstree.Stmt, error) {
	l.runtime.needsMochiStream = true
	stream, err := l.lowerExpr(s.Stream)
	if err != nil {
		return nil, fmt.Errorf("ts lower: stream emit stream: %w", err)
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, fmt.Errorf("ts lower: stream emit val: %w", err)
	}
	return []tstree.Stmt{
		&tstree.ExprStmt{Expr: &tstree.MemberCallExpr{
			Receiver: stream,
			Method:   "emit",
			Args:     []tstree.Expr{val},
		}},
	}, nil
}

func (l *lowerer) lowerSubMakeExpr(e *aotir.SubMakeExpr) (tstree.Expr, error) {
	l.runtime.needsMochiStream = true
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, fmt.Errorf("ts lower: sub make: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: stream,
		Method:   "subscribe",
	}, nil
}

func (l *lowerer) lowerSubMakeLimitExpr(e *aotir.SubMakeLimitExpr) (tstree.Expr, error) {
	l.runtime.needsMochiStream = true
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, fmt.Errorf("ts lower: sub make limit stream: %w", err)
	}
	limit, err := l.lowerExpr(e.Limit)
	if err != nil {
		return nil, fmt.Errorf("ts lower: sub make limit: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: stream,
		Method:   "subscribeLimited",
		Args:     []tstree.Expr{limit},
	}, nil
}

func (l *lowerer) lowerSubRecvExpr(e *aotir.SubRecvExpr) (tstree.Expr, error) {
	l.runtime.needsMochiStream = true
	sub, err := l.lowerExpr(e.Sub)
	if err != nil {
		return nil, fmt.Errorf("ts lower: sub recv: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: sub,
		Method:   "recv",
	}, nil
}

func (l *lowerer) lowerChanMakeExpr(e *aotir.ChanMakeExpr) (tstree.Expr, error) {
	l.runtime.needsMochiChan = true
	return &tstree.RawExpr{Text: "new MochiChan()"}, nil
}

func (l *lowerer) lowerChanSendStmt(s *aotir.ChanSendStmt) ([]tstree.Stmt, error) {
	l.runtime.needsMochiChan = true
	ch, err := l.lowerExpr(s.Chan)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan send chan: %w", err)
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan send val: %w", err)
	}
	return []tstree.Stmt{
		&tstree.ExprStmt{Expr: &tstree.MemberCallExpr{
			Receiver: ch,
			Method:   "send",
			Args:     []tstree.Expr{val},
		}},
	}, nil
}

func (l *lowerer) lowerChanRecvExpr(e *aotir.ChanRecvExpr) (tstree.Expr, error) {
	l.runtime.needsMochiChan = true
	ch, err := l.lowerExpr(e.Chan)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan recv: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: ch,
		Method:   "recv",
	}, nil
}

// streamChanRuntimeDecls emits the MochiStream<T> and MochiChan<T> class
// helpers when needed.
func (l *lowerer) streamChanRuntimeDecls() []tstree.Decl {
	var out []tstree.Decl
	if l.runtime.needsMochiStream {
		out = append(out, &tstree.RawDecl{Text: mochiStreamClass})
	}
	if l.runtime.needsMochiChan {
		out = append(out, &tstree.RawDecl{Text: mochiChanClass})
	}
	return out
}

const mochiStreamClass = `class MochiStream {
  private subs: MochiSub[] = [];
  constructor(readonly capacity: number) {}
  emit(v: unknown): void { for (const s of this.subs) s._push(v); }
  subscribe(): MochiSub { const s = new MochiSub(-1); this.subs.push(s); return s; }
  subscribeLimited(n: number): MochiSub { const s = new MochiSub(n); this.subs.push(s); return s; }
}
class MochiSub {
  private buf: unknown[] = [];
  private idx = 0;
  constructor(private readonly cap: number) {}
  _push(v: unknown): void { if (this.cap < 0 || this.buf.length < this.cap) this.buf.push(v); }
  recv(): unknown { return this.buf[this.idx++]; }
}`

const mochiChanClass = `class MochiChan {
  private q: unknown[] = [];
  send(v: unknown): void { this.q.push(v); }
  recv(): unknown { return this.q.shift()!; }
}`
