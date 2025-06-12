# `mochi/runtime/agent`

The `agent` package defines the runtime model for agents in Mochi. An **agent** is a reactive entity that:

* Listens to real-time `stream.Event`s
* Mutates local state based on event handlers
* Exposes `intent` functions for querying or triggering behavior
* Persists `var` variables across events and intent calls

This package is designed to integrate cleanly with the `mochi` interpreter, stream engine, and generative agent system.

## Features

* Stream-backed inbox for event delivery
* Dynamic stream handler registration
* Named intent registration and execution
* Mutable state with `Set` and `Get`
* Safe `Start()` / `Stop()` lifecycle

## Usage

### 1. Create a Stream

```go
s := stream.New("SensorReading", 64)
```

### 2. Initialize an Agent

```go
a := agent.New(agent.Config{
	Name:    "Monitor",
	BufSize: 16,
})
```

### 3. Register Event Handler

```go
a.On(s, func(ctx context.Context, ev *stream.Event) {
	data := ev.Data.(map[string]any)
	temp := data["temperature"].(float64)

	count, _ := a.Get("count")
	if count == nil {
		a.Set("count", 1)
	} else {
		a.Set("count", count.(int)+1)
	}

	a.Set("lastTemp", temp)
})
```

### 4. Register Intents

```go
a.RegisterIntent("status", func(ctx context.Context, args ...agent.Value) (agent.Value, error) {
	count, _ := a.Get("count")
	last, _ := a.Get("lastTemp")
	return fmt.Sprintf("Seen %v readings, last = %v", count, last), nil
})
```

### 5. Run Agent

```go
ctx := context.Background()
a.Start(ctx)
defer a.Stop()
```

### 6. Emit Stream Event

```go
s.Emit(ctx, map[string]any{
	"id":          "sensor-1",
	"temperature": 30.5,
	"timestamp":   time.Now(),
})
```

## API

### `type Agent`

| Method                          | Description                             |
| ------------------------------- | --------------------------------------- |
| `Start(ctx)`                    | Starts the agent loop                   |
| `Stop()`                        | Stops the agent loop                    |
| `On(stream, handler)`           | Registers a handler for incoming events |
| `RegisterIntent(name, handler)` | Registers a named intent function       |
| `Call(ctx, name, args...)`      | Calls a named intent                    |
| `Set(name, value)`              | Updates internal state                  |
| `Get(name) (value, ok)`         | Reads internal state                    |
| `State() map[string]Value`      | Full access to agent state              |

### `type IntentHandler`

```go
type IntentHandler func(ctx context.Context, args ...Value) (Value, error)
```

### `type Value`

Aliased as `any`, used for flexible state and intent arguments.

## Integration Notes

This package is designed to be used by the **Mochi interpreter**, which will:

* Parse and compile `agent { ... }` blocks
* Call `agent.On(...)` and `RegisterIntent(...)` dynamically
* Use `Set` / `Get` to reflect interpreter variables into agent state
* Persist variables declared with `var` across events and intents
* Optionally inject LLM tooling or sandboxed control APIs in future

