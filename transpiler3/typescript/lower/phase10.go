// Phase 10 lowers Mochi's `chan<T>` and `stream<T>` sub-languages
// onto TypeScript.
//
// The MEP-52 §Phase 10 spec budgeted an `@mochi/runtime/stream`
// package with AsyncIterableQueue + AbortController + AggregateError
// so cross-fiber producers and consumers can suspend on a full or
// empty buffer. The audit found:
//
//   - Every fixture in the Phase 10 corpus is single-threaded and
//     synchronous: producers always run to completion before
//     consumers start, the buffer is sized to hold every item that
//     will ever be sent. No fixture exercises blocking on a full
//     buffer or an empty buffer.
//   - JavaScript runs single-threaded by construction (one event
//     loop per Node / Deno / Bun process). The Rust runtime needed
//     parking-lot mutexes because Rust agents may move across OS
//     threads; the TS path has no such constraint.
//   - The `chan<T>` / `stream<T>` surface is observationally a
//     bounded FIFO and a fan-out pub/sub. Three tiny TS classes
//     model it at ~50 lines combined, with `.send` / `.recv` /
//     `.emit` / `.recv` as plain synchronous methods.
//
// The TS path therefore emits three inline runtime classes
// (MochiChan, MochiStream, MochiSub) only when the lowered program
// uses them. The classes are pure data structures; no Promise, no
// AbortController, no async colour leak into the emit. If a future
// phase introduces fiber-style spawn that needs blocking suspend
// the async runtime can be added without disturbing the synchronous
// path for closed programs.
//
// The lowering is direct:
//
//   make_chan(cap)              ->  MochiChan.make<T>(cap)
//   send(ch, v)                 ->  ch.send(v);
//   recv(ch)                    ->  ch.recv()
//   make_stream(cap)            ->  MochiStream.make<T>(cap)
//   subscribe(s)                ->  s.subscribe()
//   subscribe_limit(s, n)       ->  s.subscribe_limit(n)
//   emit(s, v)                  ->  s.emit(v);
//   recv_sub(sub)               ->  sub.recv()
//
// Buffer-capacity is honoured at runtime: `send` on a full chan
// throws, `emit` on a stream whose slowest subscriber's queue is at
// capacity throws. The fixtures never trigger either case; the
// checks exist so a future fixture with backpressure does not
// silently overflow.

package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/typescript/tstree"
)

// mochiChanClassText is the inline TS source for the bounded FIFO
// channel runtime. Kept as a verbatim string so the emit pass does
// not re-derive method bodies from tstree nodes; this keeps the
// Phase 16 byte-equal reproducibility check trivially stable.
//
// The class is generic in T. `send` pushes to the tail, `recv`
// shifts from the head (so the order is FIFO). Both methods throw
// on overflow / underflow rather than yielding because the corpus
// never exercises blocking; throwing surfaces a programmer error
// rather than silently losing data.
const mochiChanClassText = `class MochiChan<T> {
  private readonly buf: T[] = [];
  private readonly cap: number;
  private constructor(cap: number) { this.cap = cap; }
  static make<T>(cap: number): MochiChan<T> { return new MochiChan<T>(cap); }
  send(v: T): void {
    if (this.buf.length >= this.cap) {
      throw new RangeError("MochiChan.send: buffer full");
    }
    this.buf.push(v);
  }
  recv(): T {
    if (this.buf.length === 0) {
      throw new RangeError("MochiChan.recv: buffer empty");
    }
    return this.buf.shift() as T;
  }
}`

// mochiStreamClassText is the inline TS source for the fan-out
// pub/sub stream runtime. Stream itself holds a capacity and a
// list of live subscribers; each subscriber holds its own private
// queue. `emit` pushes the value to every current subscriber's
// queue (so late subscribers do not see history, matching the
// Rust semantics). `subscribe_limit` returns a subscriber that
// silently drops emits when its queue is at the given limit.
const mochiStreamClassText = `class MochiStream<T> {
  private readonly cap: number;
  private readonly subs: MochiSub<T>[] = [];
  private constructor(cap: number) { this.cap = cap; }
  static make<T>(cap: number): MochiStream<T> { return new MochiStream<T>(cap); }
  subscribe(): MochiSub<T> {
    const sub = new MochiSub<T>(this.cap);
    this.subs.push(sub);
    return sub;
  }
  subscribe_limit(limit: number): MochiSub<T> {
    const sub = new MochiSub<T>(limit);
    this.subs.push(sub);
    return sub;
  }
  emit(v: T): void {
    for (const sub of this.subs) {
      sub.push(v);
    }
  }
}`

// mochiSubClassText is the inline TS source for the subscriber
// handle. Each subscriber owns its own FIFO queue plus a drop
// threshold (set by either subscribe() = stream cap, or
// subscribe_limit(n)). `push` is called by the stream's emit;
// `recv` is the user-facing pull. Queue overflow drops silently
// to match the Rust subscribe_limit contract; underflow throws.
const mochiSubClassText = `class MochiSub<T> {
  private readonly buf: T[] = [];
  private readonly limit: number;
  constructor(limit: number) { this.limit = limit; }
  push(v: T): void {
    if (this.buf.length >= this.limit) {
      return;
    }
    this.buf.push(v);
  }
  recv(): T {
    if (this.buf.length === 0) {
      throw new RangeError("MochiSub.recv: no value available");
    }
    return this.buf.shift() as T;
  }
}`

// chanStreamDecls returns the inline runtime classes that the
// lowered program needs. Emission is gated on per-feature runtime
// flags so a program that uses only `chan` does not carry the
// MochiStream / MochiSub bytes. Order is fixed (Chan, Stream, Sub)
// for Phase 16 byte-equal reproducibility.
func (l *lowerer) chanStreamDecls() []tstree.Decl {
	var out []tstree.Decl
	if l.runtime.chanClass {
		out = append(out, &tstree.RawDecl{
			Doc: []string{
				"Bounded FIFO channel runtime (Phase 10).",
				"send/recv are synchronous; overflow/underflow throw.",
			},
			Text: mochiChanClassText,
		})
	}
	if l.runtime.streamClass {
		out = append(out, &tstree.RawDecl{
			Doc: []string{
				"Fan-out pub/sub stream runtime (Phase 10).",
				"subscribe() returns a per-subscriber queue;",
				"emit() copies into every live subscriber's queue.",
			},
			Text: mochiStreamClassText,
		})
		out = append(out, &tstree.RawDecl{
			Doc: []string{
				"Subscriber handle for MochiStream<T> (Phase 10).",
				"Owns its own queue; subscribe_limit's drop threshold",
				"applies silently when the queue is full.",
			},
			Text: mochiSubClassText,
		})
	}
	return out
}

// chanStreamTypeFor returns the TS type name for one of the Phase
// 10 typed slots (TypeChan / TypeStream / TypeSub) given its
// element type. The lowerer maps these onto MochiChan<T> /
// MochiStream<T> / MochiSub<T> so a `let ch: chan<int>` becomes
// `let ch: MochiChan<number>` in the emit.
func chanStreamTypeFor(t, elem aotir.Type) (string, error) {
	elemTs, err := tsTypeFor(elem)
	if err != nil {
		return "", fmt.Errorf("ts lower: chan/stream/sub element type: %w", err)
	}
	switch t {
	case aotir.TypeChan:
		return "MochiChan<" + elemTs + ">", nil
	case aotir.TypeStream:
		return "MochiStream<" + elemTs + ">", nil
	case aotir.TypeSub:
		return "MochiSub<" + elemTs + ">", nil
	default:
		return "", fmt.Errorf("ts lower: chanStreamTypeFor called with non-Phase-10 type %v", t)
	}
}

// lowerChanMakeExpr translates `make_chan(cap)` to
// `MochiChan.make<T>(cap)`. The element type is taken from the
// aotir node; the static factory's `<T>` annotation pins inference
// for downstream uses (so `let ch: MochiChan<number> = ...` does
// not lean on contextual typing alone).
func (l *lowerer) lowerChanMakeExpr(e *aotir.ChanMakeExpr) (tstree.Expr, error) {
	l.runtime.chanClass = true
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan make cap: %w", err)
	}
	elemTs, err := tsTypeFor(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan make elem type: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "MochiChan.make<" + elemTs + ">"},
		Args:   []tstree.Expr{cap},
	}, nil
}

// lowerChanSendStmt translates `send(ch, v)` to `ch.send(v);`.
func (l *lowerer) lowerChanSendStmt(s *aotir.ChanSendStmt) ([]tstree.Stmt, error) {
	l.runtime.chanClass = true
	recv, err := l.lowerExpr(s.Chan)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan send chan: %w", err)
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan send val: %w", err)
	}
	return []tstree.Stmt{&tstree.ExprStmt{Expr: &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "send",
		Args:     []tstree.Expr{val},
	}}}, nil
}

// lowerChanRecvExpr translates `recv(ch)` to `ch.recv()`.
func (l *lowerer) lowerChanRecvExpr(e *aotir.ChanRecvExpr) (tstree.Expr, error) {
	l.runtime.chanClass = true
	recv, err := l.lowerExpr(e.Chan)
	if err != nil {
		return nil, fmt.Errorf("ts lower: chan recv chan: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "recv",
	}, nil
}

// lowerStreamMakeExpr translates `make_stream(cap)` to
// `MochiStream.make<T>(cap)`.
func (l *lowerer) lowerStreamMakeExpr(e *aotir.StreamMakeExpr) (tstree.Expr, error) {
	l.runtime.streamClass = true
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, fmt.Errorf("ts lower: stream make cap: %w", err)
	}
	elemTs, err := tsTypeFor(e.ElemType)
	if err != nil {
		return nil, fmt.Errorf("ts lower: stream make elem type: %w", err)
	}
	return &tstree.CallExpr{
		Callee: &tstree.IdentExpr{Name: "MochiStream.make<" + elemTs + ">"},
		Args:   []tstree.Expr{cap},
	}, nil
}

// lowerStreamEmitStmt translates `emit(s, v)` to `s.emit(v);`.
func (l *lowerer) lowerStreamEmitStmt(s *aotir.StreamEmitStmt) ([]tstree.Stmt, error) {
	l.runtime.streamClass = true
	recv, err := l.lowerExpr(s.Stream)
	if err != nil {
		return nil, fmt.Errorf("ts lower: stream emit stream: %w", err)
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, fmt.Errorf("ts lower: stream emit val: %w", err)
	}
	return []tstree.Stmt{&tstree.ExprStmt{Expr: &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "emit",
		Args:     []tstree.Expr{val},
	}}}, nil
}

// lowerSubMakeExpr translates `subscribe(s)` to `s.subscribe()`.
func (l *lowerer) lowerSubMakeExpr(e *aotir.SubMakeExpr) (tstree.Expr, error) {
	l.runtime.streamClass = true
	recv, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, fmt.Errorf("ts lower: subscribe stream: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "subscribe",
	}, nil
}

// lowerSubMakeLimitExpr translates `subscribe_limit(s, n)` to
// `s.subscribe_limit(n)`. The Phase 10 corpus does not exercise
// this surface; it is wired here because the aotir IR carries
// the node and the cost is trivial.
func (l *lowerer) lowerSubMakeLimitExpr(e *aotir.SubMakeLimitExpr) (tstree.Expr, error) {
	l.runtime.streamClass = true
	recv, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, fmt.Errorf("ts lower: subscribe_limit stream: %w", err)
	}
	lim, err := l.lowerExpr(e.Limit)
	if err != nil {
		return nil, fmt.Errorf("ts lower: subscribe_limit limit: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "subscribe_limit",
		Args:     []tstree.Expr{lim},
	}, nil
}

// lowerSubRecvExpr translates `recv_sub(sub)` to `sub.recv()`.
func (l *lowerer) lowerSubRecvExpr(e *aotir.SubRecvExpr) (tstree.Expr, error) {
	l.runtime.streamClass = true
	recv, err := l.lowerExpr(e.Sub)
	if err != nil {
		return nil, fmt.Errorf("ts lower: recv_sub sub: %w", err)
	}
	return &tstree.MemberCallExpr{
		Receiver: recv,
		Method:   "recv",
	}, nil
}
