---
title: "09. Agents and streams"
sidebar_position: 10
sidebar_label: "09. Agents & streams"
description: "Agents as plain structs (no threads), single-thread Rc<RefCell<VecDeque>> channels, single-thread broadcast streams, why Arc<Mutex> rejected, async colouring with no runtime effect."
---

# 09. Agents and streams

This note covers MEP-53's concurrency story: how Mochi agents, channels, streams, and async lower to single-thread Rust.

## The single-thread runtime decision

Mochi's source language has:

- `async expr` — colours expr as asynchronously evaluable.
- `await fut` — awaits a future.
- `chan <- v` / `<- chan` — channel send and receive.
- `make_stream(N)`, `emit(s, v)`, `subscribe(s)`, `recv_sub(sub)` — broadcast stream.
- `agent A { state ... on Msg ... }` / `spawn AgentType()` / `a.intent(arg)` — actor-style agents.

But Mochi does **not** have a thread-spawn primitive at user level. There is no `thread { ... }`, no `go fun()`, no `Promise.all`-style fan-out. Async is a typecheck-time colour pass that ensures effects (panics, side effects, blocking calls) are well-tracked through call chains, but it does not request concurrent evaluation.

Given that, MEP-53 lowers all concurrency primitives to single-thread Rust:

| Mochi | Rust |
|-------|------|
| `chan<T>` | `Rc<RefCell<VecDeque<T>>>` |
| `stream<T>` | `Rc<RefCell<Vec<Rc<RefCell<VecDeque<T>>>>>>` |
| `Sub<T>` | `Rc<RefCell<VecDeque<T>>>` |
| `agent A { ... }` | plain struct + impl block |
| `spawn AgentType()` | `AgentType::new()` (immediate construction) |
| `a.intent(arg)` | `a.intent(arg)` (method call) |
| `async expr` | `expr` (immediate evaluation) |
| `await fut` | `fut` (identity) |

Zero `std::sync`. Zero `std::thread`. Zero `Arc`. Zero `Mutex`.

## Why Arc Mutex is rejected

Three reasons:

1. **Synchronisation cost on every operation.** Every `Mutex::lock` is a syscall (futex on Linux, mutex_lock on macOS) — even uncontended. For a chan send / recv loop that iterates 1000 times, that's 1000 unnecessary syscalls.

2. **Send + 'static requirement on captured values.** Closures that capture state and run on a different thread must capture by `Send + 'static`. Mochi closures can capture anything that exists in scope. Forcing Send + 'static would require an extra typecheck pass that rejects perfectly valid Mochi closures.

3. **Embedded breakage.** `Arc` requires `portable-atomic` or `alloc::sync` (the latter requires atomic CAS, which some MCU targets lack). The `embedded` gate (phase 18) compiles cleanly with `Rc<RefCell>` and would fail with `Arc<Mutex>`.

The trade-off is real: Mochi programs that need genuine parallelism cannot get it from MEP-53's emitted Rust without going through FFI to `std::thread::spawn`. Document this as a limitation, not a deficiency: Mochi's source language doesn't promise threads.

## Channels: Rc<RefCell<VecDeque>>

```rust
pub struct Chan<T> {
    inner: Rc<RefCell<VecDeque<T>>>,
}

impl<T> Chan<T> {
    pub fn make(_cap: i64) -> Self {
        Self { inner: Rc::new(RefCell::new(VecDeque::new())) }
    }
    pub fn send(&self, v: T) {
        self.inner.borrow_mut().push_back(v);
    }
    pub fn recv(&self) -> T {
        self.inner.borrow_mut().pop_front().expect("recv on empty chan")
    }
}
```

`_cap` is ignored: the queue is unbounded. Mochi's source-level `make_chan(N)` requests capacity N, but for single-thread programs unbounded is fine (the producer can never outpace the consumer because they share a thread). A future sub-phase could enforce a soft cap.

Send / recv borrow `&self` (not `&mut self`) because Mochi programs frequently store a channel in two places (one for the producer side, one for the consumer side). `Rc::clone` is cheap, and `RefCell::borrow_mut` handles the interior mutability.

`recv` on empty panics. Mochi semantics require recv to block on empty when used in a concurrent setting; in single-thread mode, "block on empty" means "deadlock," and panic is the only useful behavior. Programs that need bounded recv should check via `is_empty` first.

## Streams: Rc<RefCell<Vec<Rc<RefCell<VecDeque>>>>>

```rust
pub struct Stream<T> {
    subs: Rc<RefCell<Vec<Rc<RefCell<VecDeque<T>>>>>>,
}

impl<T: Clone> Stream<T> {
    pub fn make(_cap: i64) -> Self {
        Self { subs: Rc::new(RefCell::new(Vec::new())) }
    }
    pub fn emit(&self, v: T) {
        for s in self.subs.borrow().iter() {
            s.borrow_mut().push_back(v.clone());
        }
    }
}
```

A stream is a Vec of per-subscriber queues. `subscribe(&s)` allocates a fresh queue, appends to the subscriber list, and returns a `Sub<T>` holding an Rc to its own queue. `emit(v)` clones v onto every subscriber's queue.

The `T: Clone` bound is required because emit clones; for non-Clone types, the emit would have to consume v, which contradicts the broadcast semantic. Mochi's source language ensures all stream payloads are Clone (typecheck enforces this).

`subscribe_limit(&s, _limit)` currently delegates to `subscribe(&s)` (limit ignored). The symbol is reserved for a future phase that wires a `LimitedQueue` similar to Ruby's drop-on-full semantics.

## Agents: plain structs

```mochi
agent Counter {
    state count: int = 0
    on inc(by: int) { count = count + by }
    on get(): int { return count }
}
```

```rust
#[derive(Clone, Default, Debug)]
struct Counter { count: i64 }

impl Counter {
    fn new() -> Self { Self::default() }
    fn inc(&mut self, by: i64) { self.count = self.count + by; }
    fn get(&self) -> i64 { self.count }
}
```

`spawn AgentType()` is immediate construction. There is no mailbox, no background thread, no message passing. The intent call (`a.inc(5)`) is a direct method call on the receiver.

The `&mut self` vs `&self` choice is per-intent: if the body mutates state, `&mut self`; otherwise `&self`. The colour pass propagates the borrow requirements through call sites so the user doesn't have to think about it.

Initial state values come from `state field: T = expr` declarations and feed `Default::default()`. `expr` must be const-foldable (a literal or const expression); non-const initial values are rejected at lower time. A future sub-phase could relax this by emitting a `Default` impl with non-const expressions in the bodies.

## Async colouring with no runtime effect

`async expr` and `await fut` both lower to identity:

```mochi
async fun fetch_user(id: int): User { ... }
let u = await fetch_user(42)
```

```rust
fn fetch_user(id: i64) -> User { ... }
let u = fetch_user(42);
```

The `async` keyword is consumed by the typecheck pass (which uses it to propagate the async-effect colour through call chains) and erased by the lower pass. The emitted Rust is plain blocking code.

This works because Mochi's `async` does not request concurrent evaluation; it only marks effects. The typecheck pass uses the colour to enforce that async-coloured functions can only be called from async-coloured contexts (or from a top-level `await`), which is a static guarantee that doesn't need runtime support.

## What's missing

- **Real concurrency.** Programs that need parallel agents, parallel stream emit, or parallel async fan-out cannot get them from MEP-53. Workaround: FFI to `std::thread::spawn`.
- **Bounded channels.** `make_chan(N)`'s N is ignored. Programs that depend on backpressure-by-blocking won't see it.
- **Stream subscriber limits.** `subscribe_limit(s, N)`'s N is ignored. Programs that depend on drop-on-full won't see it.

These gaps are documented as known limitations; the C / BEAM / JVM / .NET / Swift / Kotlin / Python / TypeScript / Ruby targets each handle some of these (notably BEAM has real OTP supervision, JVM uses Project Loom). The Rust target's niche is "single-binary native distribution with embedded as a stretch goal," not "concurrent-actor runtime."

## Cross-references

- [[design-philosophy]] for the single-thread rationale.
- [[runtime]] for the Rc / RefCell / VecDeque dep chain.
- [[type-lowering]] for the type targets.
- [MEP-53 §1 decision 2](/docs/mep/mep-0053#abstract) for the normative statement.
