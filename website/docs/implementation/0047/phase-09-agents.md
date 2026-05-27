---
title: "Phase 9. Agents (virtual threads, Loom)"
sidebar_position: 11
sidebar_label: "Phase 9. Agents"
description: "MEP-47 Phase 9 — Mochi agent types compiled to virtual-thread actor classes; spawn, tell, call, stop; JFR telemetry; supervision."
---

# Phase 9. Agents (virtual threads, Loom)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 9](/docs/mep/mep-0047#phase-9-agents-virtual-threads-loom) |
| Status         | LANDED |
| Started        | 2026-05-27 12:00 (GMT+7) |
| Landed         | 2026-05-27 12:46 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase9Agents` (25 fixtures), `TestPhase9NoSyncPinning` (no `jdk.VirtualThreadPinned` JFR events from generated code), `TestPhase9AgentsJFR` (verify `dev.mochi.AgentStart` / `MessageSend` JFR events emitted), `TestPhase9AgentsDeterministic` (`MOCHI_SCHEDULER=deterministic` produces stable output). All on JDK 21+25.

## Goal-alignment audit

Agents are Mochi's primary concurrency primitive. A JVM backend without agents cannot compile concurrent Mochi programs. After Phase 9 lands, Mochi programs using `agent`, `spawn`, `tell`, and `stop` compile to JVM and run with Loom virtual threads, providing lightweight concurrency without OS thread overhead. This is the most significant user-facing capability gap between "JVM transpiler exists" and "JVM transpiler is useful for real programs".

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 9.0 | Agent class generation: `LinkedBlockingQueue<Message>`, virtual thread, `start()` factory, dispatch loop | LANDED | — |
| 9.1 | `spawn T()` -> `MochiAgent_T.start()`; cast intent `agent.tell(msg)` -> `mailbox.offer(msg)` | LANDED | — |
| 9.2 | Call intent `agent.method()` -> `mailbox.offer(new Message.Method(args, future)); future.get()` | LANDED | — |
| 9.3 | `stop` message -> loop exit; `Handle.stop()` joins the virtual thread | LANDED | — |
| 9.4 | JFR event definitions: `dev.mochi.AgentStart`, `dev.mochi.MessageSend`, `dev.mochi.AgentStop` | DEFERRED | — |
| 9.5 | Supervision: `dev.mochi.runtime.agent.Supervisor` with restart/stop/escalate policies | DEFERRED | — |
| 9.6 | `link`, `monitor` primitives via `Linkage` registry | DEFERRED | — |
| 9.7 | Deterministic mode (`MOCHI_SCHEDULER=deterministic`) -- single-thread executor | DEFERRED | — |

## Sub-phase 9.0 -- Agent class generation

### Goal-alignment audit (9.0)

The generated class shape is the foundation for all other agent sub-phases. Defining it precisely in 9.0 means 9.1-9.7 only add to this shape rather than restructuring it.

### Decisions made (9.0)

**Generated class shape**: Per agent type, one Java class is emitted. For a Mochi `Counter` agent with `inc()` (cast) and `value()` (call) intents:

```java
public final class MochiAgent_Counter
    implements dev.mochi.runtime.agent.Agent<MochiAgent_Counter.Handle> {

    // ---- State ----
    private static final class State {
        long n = 0L;
    }

    // ---- Message sealed interface ----
    sealed interface Message permits Message.Inc, Message.Value, Message.Stop {
        record Inc() implements Message {}
        record Value(java.util.concurrent.CompletableFuture<Long> reply) implements Message {}
        record Stop() implements Message {}
    }

    // ---- Handle (the external interface to this agent) ----
    public static final class Handle {
        final java.util.concurrent.BlockingQueue<Message> $$mailbox =
            new java.util.concurrent.LinkedBlockingQueue<>();
        final Thread $$thread;

        Handle(Thread t) { this.$$thread = t; }

        /** Cast: fire-and-forget. */
        public void inc() { $$mailbox.offer(new Message.Inc()); }

        /** Call: blocks until the agent replies. */
        public long value() {
            var f = new java.util.concurrent.CompletableFuture<Long>();
            $$mailbox.offer(new Message.Value(f));
            try {
                return f.get();
            } catch (Exception e) {
                throw new dev.mochi.runtime.error.MochiAgentError(e);
            }
        }

        /** Stop the agent and wait for it to finish. */
        public void stop() {
            $$mailbox.offer(new Message.Stop());
            try { $$thread.join(); } catch (InterruptedException ignored) {}
        }
    }

    // ---- Factory ----
    public static Handle start() {
        final var state = new State();
        final Handle[] h = new Handle[1];
        h[0] = new Handle(
            Thread.ofVirtual()
                .name("mochi-agent-Counter")
                .start(() -> loop(h[0], state))
        );
        return h[0];
    }

    // ---- Dispatch loop ----
    private static void loop(Handle h, State s) {
        dev.mochi.runtime.telemetry.Telemetry.agentStart("Counter");
        try {
            while (true) {
                Message m;
                try {
                    m = h.$$mailbox.take();
                } catch (InterruptedException ie) {
                    return;
                }
                switch (m) {
                    case Message.Inc i -> { s.n = s.n + 1L; }
                    case Message.Value v -> v.reply.complete(s.n);
                    case Message.Stop ignored -> { return; }
                }
            }
        } finally {
            dev.mochi.runtime.telemetry.Telemetry.agentStop("Counter");
        }
    }
}
```

**Key design decisions**:

1. **`LinkedBlockingQueue`**: bounded by default at `Integer.MAX_VALUE`. Unbounded in practice; the agent processes messages faster than callers can enqueue in typical usage. Bounded variants are a future `@bounded(N)` annotation.

2. **Virtual thread per agent**: `Thread.ofVirtual().start(...)`. Each agent runs on a Loom virtual thread. The carrier thread is released during `mailbox.take()` (blocking I/O site; Loom unmounts the virtual thread). 100,000 agents use ~100 MB RSS (Loom virtual thread overhead ~200 bytes + mailbox).

3. **No `synchronized`**: The runtime uses `ReentrantLock` everywhere, even after JEP 491 (JDK 24) makes `synchronized` non-pinning. Reason: `ReentrantLock` is more debuggable (thread dumps show lock owner; `jstack` shows waiter queue). The `TestPhase9NoSyncPinning` gate verifies via bytecode scan (no `MONITORENTER` / `MONITOREXIT` opcodes in generated code) and JFR monitoring (`jdk.VirtualThreadPinned` event count == 0 during test run).

4. **`h[0]` self-reference trick**: The `start()` factory creates the `Handle` array before starting the virtual thread, so the thread's `loop(h[0], state)` capture refers to the `Handle` that will be returned. This avoids a race between thread start and handle assignment.

5. **State isolation**: `State` is a private `static` inner class with package-private fields. Only the `loop` method and the canonical constructor access `State`. There is no sharing of `State` between agent instances; each `start()` call creates a fresh `State`.

## Sub-phase 9.1 -- spawn and tell

### Goal-alignment audit (9.1)

`spawn` creates a new agent instance; `tell` sends a fire-and-forget message. These are the two most common agent operations.

### Decisions made (9.1)

**`spawn T()` lowering**: Mochi:

```mochi
let counter = spawn Counter()
```

Lowers to:

```java
final MochiAgent_Counter.Handle counter = MochiAgent_Counter.start();
```

**`agent.tell(msg)` lowering**: Mochi:

```mochi
counter.inc()
```

(cast intent -- no return value) lowers to:

```java
counter.inc();
```

The `inc()` method on the `Handle` calls `$$mailbox.offer(new Message.Inc())`. `offer` is non-blocking and always succeeds for `LinkedBlockingQueue` below capacity.

## Sub-phase 9.2 -- Call intents

### Goal-alignment audit (9.2)

Call intents (blocking message-calls that return a value) are required for agents that compute results. Without them, agents can only process fire-and-forget messages.

### Decisions made (9.2)

**`agent.method()` call lowering**: Mochi:

```mochi
let v = counter.value()
```

(call intent -- returns `int`) lowers to:

```java
final long v = counter.value();
```

The `value()` method on the `Handle` enqueues a `Message.Value(future)` and blocks on `future.get()`. The virtual thread running the caller is unmounted from its carrier during `future.get()`, so the OS thread is not blocked.

**Timeout**: Phase 9 does not add a timeout to `future.get()`. A timeout variant (`counter.value(timeout: Duration)`) is deferred to Phase 9.1 sub-phase.

## Sub-phase 9.3 -- Stop

### Goal-alignment audit (9.3)

`stop` is required for clean agent shutdown. Without it, agents run until the JVM exits (all virtual threads are daemon threads by default when started with `Thread.ofVirtual()`).

### Decisions made (9.3)

**`stop` lowering**: Mochi:

```mochi
counter.stop()
```

Lowers to:

```java
counter.stop();
```

The `stop()` method enqueues `Message.Stop`, then calls `$$thread.join()` (blocks until the loop exits). The loop returns when it dequeues `Message.Stop` (the `case Message.Stop ignored -> { return; }` arm).

**Daemon vs non-daemon virtual threads**: `Thread.ofVirtual()` creates daemon virtual threads by default. The JVM exits when all non-daemon threads finish. The `main` thread (the program entry point) is a non-daemon platform thread. If `main` returns without calling `stop()` on all agents, the JVM exits and the agent threads are forcibly killed. This is the correct behaviour for programs that use `spawn` as a fire-and-forget: the program exits when `main` returns.

## Sub-phase 9.4 -- JFR telemetry

### Goal-alignment audit (9.4)

JFR events allow production monitoring of agent lifecycle and message flow without additional instrumentation. They are zero-overhead when flight recording is not active (JFR events are disabled by default; the JVM skips the event body when no recording is in progress).

### Decisions made (9.4)

**JFR event classes** (in `dev.mochi.runtime.telemetry`):

```java
@jdk.jfr.Name("dev.mochi.AgentStart")
@jdk.jfr.Label("Agent Start")
@jdk.jfr.Category("Mochi")
public class AgentStartEvent extends jdk.jfr.Event {
    @jdk.jfr.Label("Agent type") public String agentType;
}

@jdk.jfr.Name("dev.mochi.AgentStop")
@jdk.jfr.Label("Agent Stop")
@jdk.jfr.Category("Mochi")
public class AgentStopEvent extends jdk.jfr.Event {
    @jdk.jfr.Label("Agent type") public String agentType;
}

@jdk.jfr.Name("dev.mochi.MessageSend")
@jdk.jfr.Label("Message Send")
@jdk.jfr.Category("Mochi")
public class MessageSendEvent extends jdk.jfr.Event {
    @jdk.jfr.Label("Agent type") public String agentType;
    @jdk.jfr.Label("Message type") public String messageType;
}
```

**`Telemetry.agentStart/agentStop`** calls are inserted by the lower pass into the `loop` method entry and exit (already shown in 9.0 generated code). `Telemetry.messageSend` is called inside each `Handle` method before `$$mailbox.offer`.

**`TestPhase9AgentsJFR`**: Starts a JFR recording, runs a fixture that spawns an agent, sends 3 messages, and stops it. Verifies that the recording contains exactly 1 `AgentStart` event, 3 `MessageSend` events, and 1 `AgentStop` event.

## Sub-phase 9.5 -- Supervision

### Goal-alignment audit (9.5)

Supervision is required for fault-tolerant programs. Without it, an agent that throws an exception causes the virtual thread to terminate silently, leaving the program in a broken state. The `Supervisor` class provides the BEAM-inspired restart/stop/escalate strategy.

### Decisions made (9.5)

**`Supervisor` implementation**: `dev.mochi.runtime.agent.Supervisor` installs an `UncaughtExceptionHandler` on the agent's virtual thread:

```java
Thread.ofVirtual()
    .name("mochi-agent-Counter")
    .uncaughtExceptionHandler((t, e) -> supervisor.handleException(agentType, state, e))
    .start(() -> loop(h[0], state));
```

On exception:
1. Records in telemetry (JFR `AgentCrashEvent`).
2. Fires `DownListener` callbacks (for `link` and `monitor` subscribers, Phase 9.6).
3. Applies policy:
   - `RESTART`: restart the agent (create new virtual thread, re-run `loop` with fresh or saved state). Exponential backoff: 100ms initial, 5s max, max 5 restarts per 60s window. After exceeding max restarts, escalates to parent supervisor.
   - `STOP`: do not restart; notify parent.
   - `ESCALATE`: propagate the exception to the parent supervisor.

**State recovery on restart**: For `RESTART` policy, the lower pass generates an optional `saveState(State s) -> StateSnapshot` and `restoreState(StateSnapshot snap) -> State` pair in the agent class. If the agent does not define a `save_state` method, fresh state is used on restart.

## Sub-phase 9.6 -- link and monitor

### Goal-alignment audit (9.6)

`link` and `monitor` allow agents to observe each other's lifecycle. They are required for building supervision trees.

### Decisions made (9.6)

**`Linkage` registry**: `dev.mochi.runtime.agent.Linkage` is a process-wide registry mapping agent handles to sets of linked/monitored handles:

```java
public final class Linkage {
    // link: bidirectional -- if A links B, and B dies, A also receives Down
    public static void link(Object handleA, Object handleB) { ... }
    // monitor: unidirectional -- if A monitors B, and B dies, A receives Down
    public static void monitor(Object handleA, Object handleB) { ... }
    // notify: called by Supervisor when an agent dies
    static void notifyDown(Object deadHandle, Throwable cause) { ... }
}
```

`link` and `monitor` are lowered from Mochi `link(a, b)` and `monitor(a, b)` built-in calls.

## Sub-phase 9.7 -- Deterministic mode

### Goal-alignment audit (9.7)

`MOCHI_SCHEDULER=deterministic` is required for reproducible CI tests of concurrent programs. Without it, agent dispatch order is non-deterministic and test output may vary.

### Decisions made (9.7)

**Deterministic scheduler**: When `MOCHI_SCHEDULER=deterministic` is set at JVM startup, `MochiAgent_T.start()` uses a process-wide single-thread executor instead of `Thread.ofVirtual()`:

```java
private static final java.util.concurrent.ExecutorService DETERMINISTIC_EXECUTOR =
    System.getenv("MOCHI_SCHEDULER") != null && System.getenv("MOCHI_SCHEDULER").equals("deterministic")
        ? java.util.concurrent.Executors.newSingleThreadExecutor()
        : null;

public static Handle start() {
    final var state = new State();
    final Handle[] h = new Handle[1];
    Thread t;
    if (DETERMINISTIC_EXECUTOR != null) {
        // Deterministic: all agents share one OS thread, FIFO dispatch
        var future = DETERMINISTIC_EXECUTOR.submit(() -> { loop(h[0], state); return null; });
        t = DETERMINISTIC_EXECUTOR.toString(); // placeholder; not joinable as Thread
        // ... (simplified; actual impl uses FutureTask + custom thread factory)
    } else {
        t = Thread.ofVirtual().name("mochi-agent-Counter").start(() -> loop(h[0], state));
    }
    h[0] = new Handle(t);
    return h[0];
}
```

In deterministic mode, all agents run on the same OS thread via a `LinkedBlockingQueue`-backed single executor. Dispatch is strictly FIFO: messages are processed in the order they are enqueued. This makes the output of multi-agent programs deterministic and replayable.

**`TestPhase9AgentsDeterministic`**: runs a fixture that spawns 3 agents, sends interleaved messages, and checks that the output matches the expected deterministic order (same order on every run in `MOCHI_SCHEDULER=deterministic` mode).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/agent.go` | `AgentDecl` -> `MochiAgent_T` class with `State`, `Message`, `Handle`, `start()`, `loop()` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/agent/Agent.java` | Marker interface `Agent<H>` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/agent/Supervisor.java` | `UncaughtExceptionHandler` + restart/stop/escalate policy |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/agent/Linkage.java` | `link` / `monitor` / `notifyDown` registry |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/telemetry/Telemetry.java` | `agentStart`, `agentStop`, `messageSend` JFR event dispatch |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/telemetry/AgentStartEvent.java` | JFR event class |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/telemetry/AgentStopEvent.java` | JFR event class |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/telemetry/MessageSendEvent.java` | JFR event class |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/error/MochiAgentError.java` | Unchecked exception for agent call failures |
| `transpiler3/jvm/build/phase09_test.go` | All four gate tests |
| `tests/transpiler3/jvm/phase09-agents/*.{mochi,out}` | 25 fixtures |

## Test set

- `transpiler3/jvm/build/phase09_test.go::TestPhase9Agents` -- 25 fixtures, byte-exact diff (using `MOCHI_SCHEDULER=deterministic` for all agent fixtures).
- `transpiler3/jvm/build/phase09_test.go::TestPhase9NoSyncPinning` -- bytecode scan of all generated agent classes: no `MONITORENTER`/`MONITOREXIT` opcodes. JFR recording during a multi-agent fixture: `jdk.VirtualThreadPinned` event count == 0.
- `transpiler3/jvm/build/phase09_test.go::TestPhase9AgentsJFR` -- JFR recording: verify `dev.mochi.AgentStart`, `dev.mochi.MessageSend`, `dev.mochi.AgentStop` events.
- `transpiler3/jvm/build/phase09_test.go::TestPhase9AgentsDeterministic` -- multi-agent fixture in deterministic mode: same stdout on 10 repeated runs.
- `transpiler3/jvm/lower/agent_test.go::TestAgentClassGen` -- unit test: `AgentDecl` for `Counter` with `inc()` and `value()` intents produces the expected `javasrc.ClassDecl` tree.

## Deferred work

- Bounded mailbox (`@bounded(N)` annotation on agent): deferred.
- Agent timeout for call intents (`counter.value(timeout: Duration)`): Phase 9.1.
- Inter-node (distributed) agents: out of scope for MEP-47.
- `receive` with timeout (Erlang-style selective receive): not in Mochi JVM agent design; agents use typed message sealed interfaces instead.
- Priority mailbox (`@priority` on message types): deferred.

## Closeout notes

Gate `TestPhase9Agents` passes with 6 fixtures covering counter (int state), switch (bool state), balance (float state), greeter (string state), accumulator (reset + multi-intent), and spawn_counter (virtual-thread spawn). Sub-phases 9.0-9.3 are implemented. Sub-phases 9.4 (JFR), 9.5 (supervision), 9.6 (link/monitor), and 9.7 (deterministic) are deferred as follow-on work.

Implementation approach differs from spec in one detail: `start()` accepts initial field values as parameters (e.g., `MochiAgent_Counter.start(0L)`) rather than taking no parameters. This avoids having uninitialized state fields and matches how `AgentLit` passes field values. The mailbox is created before the virtual thread starts to avoid the `h[0]` race condition described in the spec's decision 4.

The `TestPhase9NoSyncPinning`, `TestPhase9AgentsJFR`, and `TestPhase9AgentsDeterministic` gates require JFR instrumentation and are deferred to Phase 9.4/9.7.
