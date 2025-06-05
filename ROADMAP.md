# Mochi Language Roadmap

Mochi is a small, clean programming language built for clarity, composability, and agent-oriented design.
It is designed to evolve in small, focused steps—each version introduces one foundational concept, keeping the language
minimal but expressive.

This roadmap outlines the language development toward a stable 1.0, with an emphasis on first-class data types, control
flow, streaming logic, agents, and dataset support.

Current release: v0.3.1. Upcoming 0.3.x versions will focus on generative AI.

# v0.3.x – Generative AI

The 0.3.x series focuses on expanding the `generate` block into a full-featured mechanism for AI-backed code generation,
completions, and embeddings. It introduces structured prompts, model configuration, postprocessing modes, and optional
streaming behavior. This lays the groundwork for agent reasoning in v0.4.

## v0.3.0 – Basic text generation (complete)

* [x] `generate text` block with `prompt` field
* [x] Result bound to variable as string
* [x] Runtime integration with default model
* [x] Print-compatible output
* [x] Static syntax validation

```
let poem = generate text {
  prompt: "Write a haiku about spring"
}
print(poem)
```

## v0.3.1 – Parameterized generation

* [ ] Support for optional fields: `temperature`, `top_p`, `max_tokens`, `stop`
* [ ] Static type checks for parameter types
* [ ] Full model request encoded at runtime
* [ ] Fallback to default values if omitted

```
let response = generate text {
  prompt: "Summarize the following:",
  temperature: 0.7,
  max_tokens: 100,
  stop: ["\n\n"]
}
```

## v0.3.2 – Model selection

* [ ] `model` field with values like `"openai:gpt-4"` or `"claude:opus"`
* [ ] Environment-based key loading per provider
* [ ] Custom alias models via `model` block

```
model quick {
  provider: "openai"
  name: "gpt-3.5-turbo"
  temperature: 0.2
}

let summary = generate text {
  model: "quick",
  prompt: "Explain photosynthesis"
}
```

## v0.3.3 – Output postprocessing

* [ ] `as` clause for structured result decoding
* [ ] Modes: `json`, `yaml`, `list`, `lines`, `markdown`
* [ ] Optional destructuring support
* [ ] Fallback on parse failure

```
let { title, body } = generate text {
  prompt: "Return a JSON with title and body fields"
} as json
```

## v0.3.4 – Embedding support

* [ ] `generate embedding` block with `text` field
* [ ] Returns a `list<float>` vector
* [ ] Optional `normalize: true`
* [ ] Supports different embedding models

```
let vec = generate embedding {
  text: "The quick brown fox jumps over the lazy dog",
  model: "openai:text-embedding-ada-002",
  normalize: true
}
```

## v0.3.5 – Streaming results

* [ ] Add `stream: true` field to `generate` block
* [ ] Streaming output to stdout during generation
* [ ] Works with both default and aliased models
* [ ] Buffered output fallback if not supported

```
let output = generate text {
  prompt: "Write a long poem",
  stream: true
}
```

## v0.3.6 – Async and future-style execution

* [ ] `async` keyword to defer execution
* [ ] `await` keyword for resolution
* [ ] Support for concurrent prompts and postprocessing
* [ ] Error propagation on await failure

```
let f = async generate text {
  prompt: "Describe the lifecycle of a butterfly"
}

let result = await f
print(result)
```

## v0.3.7 – Tool-assisted generation (preview)

* [ ] Define `fun` functions as callable tools
* [ ] Use `tools: [...]` in generate block
* [ ] Resolve structured tool\_call payloads
* [ ] Runtime loop to simulate tool invocation

```
fun search(q: string): string {
  return "No results for " + q
}

let reply = generate text {
  prompt: "What is the capital of Georgia?",
  tools: [search]
}
```

## v0.3.8 – Full integration test

* [ ] Realistic multi-model pipeline
* [ ] Mixed text, embedding, and postprocessing steps
* [ ] Validates all config and streaming modes

```
model fast {
  provider: "openai"
  name: "gpt-3.5-turbo"
  temperature: 0.3
}

model embed {
  provider: "openai"
  name: "text-embedding-ada-002"
}

fun summarize(text: string): string {
  return generate text {
    model: "fast"
    prompt: "Summarize this text:\n\n" + text,
    max_tokens: 80
  }
}

fun embed_text(text: string): list<float> {
  return generate embedding {
    model: "embed",
    text: text,
    normalize: true
  }
}

let input = """
Spring is the time of renewal and growth.
Cherry blossoms line the streets.
"""

let summary = summarize(input)
let vector = embed_text(input)

print(summary)
print(vector)
```

## v0.4 – Streams

Streams introduce time-ordered event types into the Mochi language. They support event declaration, event handling via
`on` blocks, and runtime emission of values. Streams are independent from agents but will be integrated in future
versions. Each stream defines a schema, and events are processed deterministically in order.

### Features

* [ ] `stream` type declaration with named fields
* [ ] Optional `time` field with implicit default (`now()` if omitted)
* [ ] `on Stream as x {}` blocks to register event handlers
* [ ] Multiple `on` blocks for the same stream allowed
* [ ] Runtime `emit Stream { ... }` support
* [ ] Type-checked access to stream fields
* [ ] Events are queued and replayable deterministically
* [ ] Support for multiple independent stream types
* [ ] Distinct stream name and event type
* [ ] Internal system clock for ordering
* [ ] Integration with `test` and `expect` simulation
* [ ] Future: stream-to-agent routing (`on Stream as x` inside agent)

### Example

```mochi
# Define a stream type for temperature sensor readings
stream SensorReading {
  id: string
  temperature: float
  timestamp: time
}

# Handler that prints every event
on SensorReading as r {
  print("Reading from", r.id)
  print(" →", r.temperature, "°C at", r.timestamp)
}

# Emit an event at runtime (pseudo-syntax in interpreter mode)
emit SensorReading {
  id: "sensor-1",
  temperature: 25.0,
  timestamp: now()
}

# Multiple handlers can coexist
on SensorReading as r {
  if r.temperature > 30 {
    print("⚠️ High temperature:", r.temperature)
  }
}

# Test simulation with stream emit and expectations
test "alert logic" {
  emit SensorReading { id: "a", temperature: 28, timestamp: "2025-06-01T12:00:00Z" }
  emit SensorReading { id: "a", temperature: 35, timestamp: "2025-06-01T12:01:00Z" }

  expect SensorReading.count() == 2
  expect SensorReading[1].temperature == 35
}
```

### Notes

* All stream declarations are globally scoped.
* Events must match the declared type exactly.
* The `timestamp` field is optional if the stream declares one; otherwise, it is auto-assigned.
* Emitted events are processed in order of their timestamp (or emit order if not provided).
* Handlers are registered once and run for every matching event.

## v0.5 – Agents

Agents define long-lived, reactive components that respond to events and expose callable functions. Each agent maintains
its own internal variables, reacts to incoming stream data via `on` blocks, and provides externally callable `intent`
functions. Agents execute deterministically and encapsulate state using `var` for mutable and `let` for constant
bindings.

### Features

* [ ] `agent` block declaration
* [ ] Internal `let` / `var` bindings scoped to the agent
* [ ] `on Stream as x {}` to handle stream events
* [ ] `intent` blocks for exposed callable logic
* [ ] Mutable internal state via `var`
* [ ] Supports `generate text` in `intent`
* [ ] Type-safe and deterministic execution
* [ ] Single instance per agent definition
* [ ] Access to all internal variables in all blocks
* [ ] Compatible with `test` blocks and stream simulation

### Example

```mochi
stream SensorReading {
  id: string
  temperature: float
  timestamp: time
}

agent Monitor {
  var count: int = 0
  var lastTemp: float = 0.0

  on SensorReading as r {
    count = count + 1
    lastTemp = r.temperature

    if r.temperature > 30 {
      print("High temperature from", r.id, ":", r.temperature)
    }
  }

  intent status(): string {
    return "Seen " + count + " readings, last = " + lastTemp
  }

  intent summary(): string {
    return generate text {
      prompt: "There have been $count readings. The last temperature was $lastTemp °C.",
      args: {
        count: count,
        lastTemp: lastTemp
      }
    }
  }
}
```

### Notes

* All variables declared with `var` persist across stream events and intent calls.
* `let` can be used for computed constants within the agent.
* Stream handlers (`on`) mutate state but return nothing.
* Intent functions may return values and are externally callable.
* All agent code runs in a sandboxed, serializable context.
* Multiple `on` blocks and multiple `intent` blocks are allowed.

### Test Example

```mochi
test "monitor alert" {
  emit SensorReading { id: "x", temperature: 25, timestamp: "2025-06-01T10:00:00Z" }
  emit SensorReading { id: "x", temperature: 36, timestamp: "2025-06-01T10:01:00Z" }

  expect Monitor.count == 2
  expect Monitor.lastTemp == 36

  let s = Monitor.status()
  expect s contains "Seen 2 readings"
}
```

## v0.6 – Datasets and Queries

This version introduces immutable, row-oriented datasets into the language. Datasets can be loaded from external files,
queried using a SQL-like syntax, and composed into analysis pipelines. Queries support filtering, projection, joining,
grouping, and aggregation. Datasets are read-only and deterministic, making them suitable for testable and reproducible
computation.

### Features

* [ ] `from "<file>"` syntax for loading datasets
* [ ] Support for CSV, JSONL, and Parquet formats
* [ ] Header-based type inference
* [ ] SQL-style `select`, `from`, `where`, `join`, `group by`, and `having`
* [ ] Aliasing via `as`
* [ ] Expression support in `select` and `where`
* [ ] Built-in aggregation functions: `sum`, `avg`, `count`, `min`, `max`
* [ ] Deterministic evaluation
* [ ] Integration with agents and streams (read-only access)
* [ ] Optional `let` bindings for intermediate datasets
* [ ] Testable query results via `expect`

### Example

```mochi
# Load a dataset from a CSV file
let people = from "people.csv"

# Filter rows where age >= 18
let adults = select name, age
from people
where age >= 18

# Join with another dataset
let purchases = from "purchases.jsonl"
let customers = from "customers.csv"

let result = select c.name, p.item, p.amount
from purchases as p
join customers as c on p.customer_id == c.id
where p.amount > 100
```

### Grouping and Aggregation

```mochi
# Group rows and compute aggregate values
let summary = select city, count(*), avg(age)
from people
group by city
having count(*) > 10
```

### Computed Fields and Aliasing

```mochi
# Compute new fields and rename them
let extended = select name, age * 2 as double_age
from people
where name != ""
```

### Test Example

```mochi
test "adults only" {
  let people = from "people.csv"

  let adults = select name, age
  from people
  where age >= 18

  expect adults.count() > 0
  expect adults[0].age >= 18
}
```

### Notes

* Datasets are loaded once and immutable.
* Type inference occurs at load time but can be overridden (future).
* Queries are declarative and do not mutate source datasets.
* All query clauses (`select`, `from`, `where`, etc.) must appear in order.
* Results are always materialized into in-memory tables.
* Expressions in `select` and `where` support full arithmetic and logical operations.

## v0.7 – Graphs

This version introduces native support for graph datasets and queries. Graphs consist of nodes and edges, with labeled
properties and types. Mochi provides a Cypher-like query syntax embedded in LINQ-style expressions for querying,
filtering, and transforming graph data. Graphs are immutable and may be used alongside tabular datasets and agents.

### Features

* [ ] `graph` binding for loading graph data
* [ ] Support for labeled nodes and typed edges
* [ ] Property maps on both nodes and edges
* [ ] Syntax for `from (n)-[e]->(m) in g` traversal
* [ ] Filtering via `where` and `select` clauses
* [ ] Type-safe access to node and edge properties
* [ ] Join compatibility between graph and table data
* [ ] Deterministic evaluation
* [ ] Testable results and introspection

### Example

```mochi
# Load a graph from file
let g = graph from "social.kuzu"

# Traverse friendships
let friends = select a.name, b.name
from (a)-[e]->(b) in g
where e.type == "FRIEND"
```

### Filtering and Labels

```mochi
# Filter by node labels and edge types
let followers = select a.name, b.name
from (a)-[f]->(b) in g
where f.type == "FOLLOWS" and a.label == "User" and b.label == "User"
```

### Join with Tables

```mochi
let users = from "users.csv"

let enriched = select u.name, b.name, p.amount
from (a)-[p]->(b) in g
join users as u on a.id == u.id
where p.type == "PURCHASE"
```

### Path Queries

```mochi
# Optional: future support for multi-hop paths
let paths = select a.name, c.name
from (a)-[e1]->(b)-[e2]->(c) in g
where e1.type == "FOLLOWS" and e2.type == "FOLLOWS"
```

### Test Example

```mochi
test "basic graph query" {
  let g = graph from "network.kuzu"

  let links = select a.id, b.id
  from (a)-[e]->(b) in g
  where e.type == "CONNECTED"

  expect links.count() > 0
  expect links[0].a.id != ""
}
```

### Notes

* Graphs are immutable and loaded as datasets.
* Each `(a)-[e]->(b)` clause produces one row per edge.
* Property access is type-safe: `a.name`, `e.type`, `b.label`, etc.
* Edge direction matters and is required in queries.
* Graph queries compose naturally with dataset pipelines.
* Future versions may add mutation, indexing, and visualization support.

## v0.8 – Packages

This version introduces a modular package system for organizing reusable code across directories. Each directory defines
a package. Files within the same directory belong to the same package and share access to all declarations. To make a
name visible to other packages, it must be explicitly marked with `export`.

Packages support imports of individual names, grouped names, or the entire package. The package system supports standard
libraries, local modules, aliasing, and integration with the CLI and language tooling.

### Features

* [ ] Each directory defines a package
* [ ] All `.mochi` files in a directory belong to the same package
* [ ] `export` keyword for exposing names outside the package
* [ ] `import` for loading packages or selected names
* [ ] Support for aliasing (`import x as y`)
* [ ] Logical and relative path resolution
* [ ] File extension `.mochi` inferred automatically
* [ ] Package contents: `export fun`, `export agent`, `export type`, etc.
* [ ] CLI-aware module loading via `mochi run`, `mochi test`, etc.
* [ ] Integration with language server and test runner

### Example: Exported API

```
# In math/arith.mochi (part of the math package)

export fun add(x: int, y: int): int {
  return x + y
}

export fun mul(x: int, y: int): int {
  return x * y
}

fun hidden(x: int): int {
  return x - 1  # not exported
}
```

```
# In main.mochi

import math.{add, mul}

print(add(2, 3))  # 5
print(mul(4, 5))  # 20
```

### Example: Full Package Import

```
# In util/format.mochi

export fun debug(x): string {
  return "debug: " + x
}
```

```
# In main.mochi

import util

print(util.debug("test"))
```

### Example: Import with Alias

```
import strings as str

let s = "hello"
print(str.upper(s))
```

### Example: Directory Layout

```
project/
├── main.mochi
├── math/
│   ├── arith.mochi      # defines math.add, math.mul
│   └── trig.mochi       # defines math.sin, math.cos
├── util/
│   └── format.mochi     # defines util.debug
```

### Notes

* `export` is required for visibility outside the package.
* All files in a directory form a single logical namespace.
* Import paths resolve relative to the entrypoint or via configured roots.
* Standard library packages may be imported using `import math`, `import time`, etc.
* Packages may contain any top-level declaration: `fun`, `type`, `agent`, `stream`, `let`, `const`, `dataset`.

## v0.9 – Standard Libraries

This version introduces the official standard library set for Mochi. These libraries provide core utilities—math
operations, string manipulation, time handling, structured collections, and common functional helpers. All standard
libraries are implemented as regular packages under the `std/` namespace and follow the package system introduced in
v0.8.

Standard library functions are explicitly marked with `export` and must be imported by name or via full package paths.
No global prelude is introduced; all access is explicit.

### Features

* [ ] `std/` namespace for all standard packages
* [ ] Libraries: `std/math`, `std/strings`, `std/time`, `std/list`, `std/set`, `std/uuid`, `std/fmt`, `std/test`
* [ ] Implemented entirely in Mochi or minimal host bindings
* [ ] Functions, types, and constants must use `export` to be visible
* [ ] CLI autoload support for `std/`
* [ ] Documentation embedded or extractable for each module
* [ ] Deterministic, pure-function implementations where possible

### Included Packages

#### `std/math`

* `export fun abs(x)`
* `export fun pow(x, y)`
* `export fun sqrt(x)`
* `export fun min(x, y)`
* `export fun max(x, y)`
* `export fun clamp(x, low, high)`

#### `std/strings`

* `export fun upper(s)`
* `export fun lower(s)`
* `export fun trim(s)`
* `export fun split(s, sep)`
* `export fun join(parts, sep)`
* `export fun contains(s, substr)`

#### `std/time`

* `export fun now(): time`
* `export fun parse_iso(s: string): time`
* `export fun format_iso(t: time): string`
* `export fun add(t: time, delta: duration): time`
* `export fun since(t: time): duration`

#### `std/list`

* `export fun map(list, f)`
* `export fun filter(list, pred)`
* `export fun reduce(list, f, init)`
* `export fun length(list)`
* `export fun reverse(list)`

#### `std/set`

* `export fun insert(s, v)`
* `export fun remove(s, v)`
* `export fun contains(s, v)`
* `export fun union(a, b)`
* `export fun intersect(a, b)`

#### `std/uuid`

* `export fun v4(): string`

#### `std/fmt`

* `export fun debug(x)`
* `export fun json(x)`
* `export fun pretty(x)`

#### `std/test`

* `export fun assert(cond)`
* `export fun assert_eq(a, b)`
* `export fun assert_ne(a, b)`

### Example Usage

```mochi
import std/math.{pow, sqrt}
import std/strings as str
import std/time.now

let x = pow(2, 5)
let root = sqrt(x)

let message = str.upper("hello world")
let t = now()

print(root)
print(message)
print(t)
```

### Notes

* All standard libraries live under the `std/` namespace
* Standard libraries are regular packages with `export`-scoped symbols
* There is no implicit prelude; all imports are explicit
* Libraries may depend on each other using standard import rules
* Future versions may provide CLI tools to install, inspect, or lint standard libraries
