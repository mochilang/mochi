# Mochi Language Roadmap

Mochi is a small, clean programming language built for clarity, composability, and agent-oriented design.
It is designed to evolve in small, focused steps—each version introduces one foundational concept, keeping the language
minimal but expressive.

This roadmap outlines the language development toward a stable 1.0, with an emphasis on first-class data types, control
flow, streaming logic, agents, and dataset support.

Current release: v0.3.0. Upcoming 0.3.x versions will focus on agents and datasets.

# v0.2.x – Compose data types

## v0.2.11 – Sets

- [ ]  Set literals
- [ ]  `in` operator
- [ ]  No duplicates
- [ ]  Basic set operations: insert, remove, union (future)

```
let seen = {"apple", "banana"}
if "apple" in seen {
  print("Already seen")
}
```

## v0.2.12 – Tuples

- [ ]  Tuple literals
- [ ]  Tuple indexing via `.0`, `.1`
- [ ]  Destructuring (planned)
- [ ]  Nesting

```
let point = (10, 20)
let nameAndAge = ("Ada", 35)

print(point.0)       // 10
print(nameAndAge.1)  // 35
```

## v0.2.13 – Struct Types

- [ ]  `type` declarations
- [ ]  Named field access via dot
- [ ]  Type annotation on variables
- [ ]  Nested structs

```
type User {
  name: string
  age: int
}

let u: User = {
  name: "Ana",
  age: 30
}

print(u.name)  // Ana
```

## v0.2.14 – Block Expressions

- [ ]  `{}` blocks as expressions
- [ ]  Evaluate to last value
- [ ]  Nesting inside `let`, `return`, conditionals

```
let result = {
  let x = 2
  let y = 3
  x + y
}

print(result)  // 5
```

## v0.2.15 – match Expressions

- [ ]  Pattern matching
- [ ]  Fallback case `_ =>`
- [ ]  Literal patterns
- [ ]  Destructuring (planned)

```
match value {
  0 => print("zero")
  1 => print("one")
  _ => print("other")
}

match user {
  { name: "Alice" } => print("Hi Alice")
  _ => print("Unknown")
}
```

## v0.2.16 – Naive error Type

- [ ]  Union return types
- [ ]  `error` literal
- [ ]  `try` expression
- [ ]  `or` fallback

```
fun parse(x: string): int | error {
  if x == "" {
    return error "empty input"
  }
  return 42
}

let n = try parse("") or 0
print(n)  // 0
```

## v0.3 – Streams

- [ ]  `stream` type declaration
- [ ]  `on` event handler syntax
- [ ]  Stream events with named fields
- [ ]  Type-safe field access
- [ ]  Runtime emit and dispatch
- [ ]  Support multiple handlers
- [ ]  Support multiple streams
- [ ]  Events queued and replayable
- [ ]  Declarative schema for events
- [ ]  Distinct stream name and type name
- [ ]  Streaming values from code
- [ ]  Optional timestamp field
- [ ]  Streaming into agents (in v0.4)

```
# Define a stream type named SensorReading
stream SensorReading {
  id: string
  temperature: float
  timestamp: time
}

# Listen to the stream using an `on` block
on SensorReading as r {
  print("Temp reading from", r.id)
  print("  →", r.temperature, "°C at", r.timestamp)
}

# You can also emit values into the stream at runtime (pseudo-syntax)
emit SensorReading {
  id: "sensor-1",
  temperature: 22.5,
  timestamp: now()
}

# Multiple on-handlers can be registered for the same stream
on SensorReading as r {
  if r.temperature > 30 {
    print("🔥 High temperature alert!")
  }
}

# Streams are independent from agents (but will be integrated in v0.4)
# Events are queued and processed in order
```

## v0.4 – Agents

- [ ]  `agent` block declaration
- [ ]  Internal `state` fields with initialization
- [ ]  `on` handlers for reacting to stream events
- [ ]  `intent` functions callable from outside
- [ ]  Local variable scoping inside agents
- [ ]  Mutable agent state with assignment
- [ ]  Deterministic execution (no shared memory)
- [ ]  Support for multiple agents
- [ ]  Agent lifecycle hooks (optional)
- [ ]  Support for `generate text` block using LLM
- [ ]  Automatic state snapshotting (future)
- [ ]  Integration with stream system (`on Stream as x` inside agent)

```
# Define a SensorReading stream as in v0.3
stream SensorReading {
  id: string
  temperature: float
  timestamp: time
}

# Define an agent with persistent state
agent Monitor {
  state {
    count: int = 0
    lastTemp: float = 0.0
  }

  # React to incoming stream events
  on SensorReading as r {
    count = count + 1
    lastTemp = r.temperature

    if r.temperature > 30 {
      print("🔥 Temp warning for", r.id, ":", r.temperature)
    }
  }

  # Define an intent function that can be called externally
  intent status(): string {
    return "Readings seen: " + count + ", last = " + lastTemp
  }

  # Generate a natural language summary using an LLM
  intent summary(): string {
    return generate text {
      prompt: "The sensor has recorded $count readings. The last temperature was $lastTemp °C.",
      args: {
        count: count,
        lastTemp: lastTemp
      }
    }
  }
}

# This defines a reusable, autonomous computation unit.
# It has memory (state), reacts to the environment (on), and exposes capability (intent).
```

## v0.5 – Datasets & Queries

- [ ]  `from "<file>"` syntax for loading datasets
- [ ]  Support for CSV, JSONL, Parquet (backend-dependent)
- [ ]  Row-oriented dataset model
- [ ]  SQL-like `select` / `from` / `where` / `join` syntax
- [ ]  Aliasing with `as`
- [ ]  Aggregation functions: `sum`, `avg`, `count`, etc.
- [ ]  `group by` and `having`
- [ ]  Expression support in `select` and `where`
- [ ]  Type inference from headers and values
- [ ]  Integration with agents and streams (read-only)
- [ ]  Deterministic, immutable datasets
- [ ]  Optional inline datasets (future)

```
# Load a dataset from CSV
let people = from "people.csv"

# Select a subset of rows
let adults = select name, age
from people
where age >= 18

# Join across multiple datasets
let purchases = from "purchases.parquet"
let customers = from "customers.jsonl"

let result = select p.id, c.name, p.amount
from purchases as p
join customers as c on p.customer_id == c.id
where p.amount > 100

# Aggregation and grouping
let summary = select city, count(*), avg(age)
from people
group by city
having count(*) > 10

# Expressions allowed in select
let short = select name, age * 2 as double_age
from people
where name != ""

# Datasets are immutable and queryable, like small in-memory tables.
# Future: support writing datasets with `write people to "people.jsonl"`
```

## v0.6 – Modules & Imports

- [ ]  `import` statement for loading external modules
- [ ]  Support importing specific names or full module
- [ ]  Standard library modules (`math`, `time`, etc.)
- [ ]  User-defined module files
- [ ]  Default module resolution via relative path or `std/` prefix
- [ ]  Aliasing with `import x as y`
- [ ]  Export control (future: `pub`)
- [ ]  Namespace access via `mod.name`
- [ ]  Integration with CLI and runtime loader
- [ ]  Module system supports agents, types, streams, datasets

```
# Import the entire math module
import math

let x = math.pow(2, 3)
print(x)  // 8

# Import only a single function
import time.now

let t = now()
print(t)

# Import with alias
import strings as str

let s = "hello"
print(str.upper(s))  // "HELLO"

# Import from a relative path
import "./utils/format"

print(format.debug(42))

# Module contents can include:
# - fun definitions
# - type declarations
# - agent declarations
# - streams
# - constants
# - datasets (via `from`)
```

## v1.0 – Production Agent Runtime & Language Maturity

- [ ]  Fully stable language grammar and core type system
- [ ]  Long-running agent runtime with scheduling and timers
- [ ]  Event replay and time-travel debugging
- [ ]  Stream processing engine with guaranteed ordering
- [ ]  Snapshotting agent state (disk or memory)
- [ ]  CLI toolchain: `mochi run`, `mochi test`, `mochi fmt`, `mochi repl`
- [ ]  Formatter (`mochi fmt`) with deterministic style
- [ ]  Language Server Protocol (LSP) support
- [ ]  Configurable module paths, stream sinks, and dataset backends
- [ ]  Built-in support for structured logging
- [ ]  Developer-first error messages and diagnostics
- [ ]  Full test runner with golden tests and simulation traces
- [ ]  LLM integration with reproducibility (prompt + args + result)
- [ ]  Safe-by-default execution: no uncontrolled side effects
- [ ]  Stable serialization for state, datasets, and streams
- [ ]  Release mode with static linking and cross-platform build

```
# Agent in production mode
agent BillingWorker {
  state {
    total: float = 0
  }

  on Invoice as i {
    total = total + i.amount
  }

  intent get_summary(): string {
    return generate text {
      prompt: "The total billed amount is $total USD.",
      args: { total: total }
    }
  }
}

# Run test simulation with golden output
test "billing logic" {
  emit Invoice { amount: 50 }
  emit Invoice { amount: 20 }

  expect BillingWorker.total == 70
  expect BillingWorker.get_summary() contains "70"
}

# Automatically formatted source file (mochi fmt)
# Integrated diagnostics and autocompletion (LSP)
# Stable snapshotting of state and deterministic replay
```
