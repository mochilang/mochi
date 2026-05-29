---
title: "09. Agents and streams"
sidebar_position: 10
sidebar_label: "09. Agents and streams"
description: "Mochi agents as a goroutine wrapping a chan Msg receive loop, streams as a struct of []chan T subscriber slots, async colouring as a typecheck pass with go statement boundaries."
---

# 09. Agents and streams

This note covers the concurrency-shaped lowering: agents, streams, channels, async, await.

## Channels

`chan<T>` lowers to Go's native `chan T`. Direct mapping; no runtime helper involved.

```mochi
let ch = make_chan(8)
send(ch, 42)
let v = recv(ch)
```

lowers to:

```go
ch := make(chan int64, 8)
ch <- 42
v := <-ch
```

Phase 9.1 ships this with three subtests: `chan_basic` (capacity 1 int), `chan_bool` (capacity 2 bool), `chan_buffered` (capacity 3 FIFO). All single-thread because Phase 9.1 predates the `go` keyword lowering; multi-thread channel use lands in Phase 9.2.

The capacity argument is lowered through `lowerExpr`, so `make_chan(n)` where `n` is a variable works, not just literal constants.

## Streams

Streams are 1-to-many: one producer, multiple subscribers, each subscriber with a bounded buffer (backpressure). The runtime struct:

```go
type Stream[T any] struct {
    mu   sync.Mutex
    subs []chan T
}

func StreamMake[T any](cap int) *Stream[T] {
    return &Stream[T]{}
}

func (s *Stream[T]) Subscribe() <-chan T {
    s.mu.Lock()
    defer s.mu.Unlock()
    ch := make(chan T, 64) // default subscriber buffer
    s.subs = append(s.subs, ch)
    return ch
}

func (s *Stream[T]) SubscribeLimit(cap int) <-chan T {
    s.mu.Lock()
    defer s.mu.Unlock()
    ch := make(chan T, cap)
    s.subs = append(s.subs, ch)
    return ch
}

func (s *Stream[T]) Emit(v T) {
    s.mu.Lock()
    defer s.mu.Unlock()
    for _, ch := range s.subs {
        select {
        case ch <- v:
        default: // backpressure: drop on full
        }
    }
}
```

The `select` with a `default` branch implements backpressure-as-drop: if a subscriber's channel is full, the emit silently drops for that subscriber. This matches the MEP-55 stream-backpressure semantics. Phase 9.2 wires this.

The mutex inside the stream is the only `sync.Mutex` use in the runtime. We considered alternatives:

- **`sync/atomic` with a copy-on-write slice.** Rejected: `Emit` is the hot path, `Subscribe` is the cold path; CoW makes the cold path cheap but the hot path expensive.
- **One `chan T` per subscriber held in a `sync.Map`.** Rejected: `sync.Map` is keyed; we want a slice of channels with no key.
- **Lock-free channel of channels.** Rejected: complexity not justified by the use cases we have.

## Agents

Mochi agents are stateful actors with message-passing semantics. The lowering produces:

```mochi
agent Counter {
    state count: int = 0
    on tick {
        count = count + 1
    }
    on get(reply: chan int) {
        send(reply, count)
    }
}

let c = spawn Counter()
c.tick()
c.tick()
let reply = make_chan(1)
c.get(reply)
print(recv(reply))   // 2
```

lowers to:

```go
type CounterAgent struct {
    in    chan counterMsg
    Count int64
}

type counterMsg interface{ isCounterMsg() }
type counterTickMsg struct{}
type counterGetMsg struct{ Reply chan int64 }

func (*counterTickMsg) isCounterMsg() {}
func (*counterGetMsg) isCounterMsg()  {}

func NewCounter() *CounterAgent {
    a := &CounterAgent{in: make(chan counterMsg, 64)}
    go a.run()
    return a
}

func (a *CounterAgent) run() {
    for m := range a.in {
        switch m := m.(type) {
        case *counterTickMsg:
            a.Count = a.Count + 1
            _ = m
        case *counterGetMsg:
            m.Reply <- a.Count
        }
    }
}

func (a *CounterAgent) Tick() { a.in <- &counterTickMsg{} }
func (a *CounterAgent) Get(reply chan int64) {
    a.in <- &counterGetMsg{Reply: reply}
}

c := NewCounter()
c.Tick()
c.Tick()
reply := make(chan int64, 1)
c.Get(reply)
fmt.Println(<-reply)
```

Key choices:

1. **Goroutine spawn inside `NewCounter`.** The user does not call `go` explicitly; `spawn` does it. This matches Mochi's actor model where every agent is alive after construction.
2. **Discriminated interface for messages.** One marker method, one struct per `on Foo` handler. Same pattern as sum types.
3. **`in` channel buffered at 64.** Configurable via `MOCHI_AGENT_BUF` env var; 64 is a default that matches Erlang's per-process mailbox sizing heuristic.
4. **No supervisor by default.** Phase 10 lands the supervisor as an opt-in `mochiruntime/agent.Supervise(NewCounter, agent.WithRestart(3))`. The plain `spawn Counter()` does not get restart-on-panic; that is a choice the source language makes per call-site.
5. **`for m := range a.in` exits when `in` is closed.** This is the agent's `stop` lowering: `c.stop()` closes `a.in` which exits the loop.

## Async / await

Mochi's `async` and `await` are colouring annotations: they mark which functions perform async work and where the boundaries are. The Go target lowers them as identity (no `go` statement, no `chan T` waiting):

```mochi
async fun fetch_data(): string {
    return httpGet("https://example.com")
}

let data = await fetch_data()
```

lowers to:

```go
func fetchData() string {
    return mochiruntime.HttpGet("https://example.com")
}

data := fetchData()
```

This is the "async colouring is a typecheck pass with no runtime effect" model. Phase 11 wires this. The Go runtime's M:N scheduler already gives us concurrency without per-await suspension points; we do not need to mirror the source-language async/await onto Go's `chan T`-based promise pattern.

Future sub-phases may extend the lowering to spawn `async` calls in their own goroutine when the colour pass detects they will block (e.g., `await Promise.all([...])`); for now the simple lowering suffices.

## Why this is simpler than every other target

The Go target has the easiest concurrency story by a wide margin:

| Target | Channel primitive | Agent primitive | Async story |
|--------|-------------------|-----------------|-------------|
| C (MEP-45) | hand-rolled bounded queue + mutex + condvar | hand-rolled thread + queue | not supported |
| BEAM (MEP-46) | erlang process mailbox | gen_server | erlang's spawn+receive |
| JVM (MEP-47) | `java.util.concurrent.ArrayBlockingQueue` | Loom virtual thread + queue | CompletableFuture |
| .NET (MEP-48) | `System.Threading.Channels.Channel<T>` | Task + ChannelReader | async/await |
| Swift (MEP-49) | AsyncChannel | actor + AsyncChannel | structured concurrency |
| Kotlin (MEP-50) | `Channel<T>` from kotlinx.coroutines | actor coroutine | suspend functions |
| Python (MEP-51) | `asyncio.Queue` | TaskGroup + Queue | async def + await |
| TypeScript (MEP-52) | AsyncIterableQueue | class + AsyncIterableQueue | async function + await |
| Rust (MEP-53) | `Rc<RefCell<VecDeque<T>>>` (single-thread) | plain struct + queue | identity (no runtime) |
| PHP (MEP-55) | userland Channel array | final class wrapping Channel | identity |
| Ruby (MEP-56) | `Thread::SizedQueue` | `Thread.new` + SizedQueue | identity |
| **Go (MEP-54)** | **native `chan T`** | **goroutine + `chan Msg`** | **identity** |

Only BEAM, JVM (Loom), and Go map the source-language model onto a native runtime primitive with no compromise. For everything else there is at least one of (single-thread restriction, cooperative-only scheduling, no preemption, manual locking, runtime dep). This is the structural argument for why a Go target is uniquely valuable even with eleven other transpilers already shipping.
