# 🗺️ Mochi Language Roadmap

Mochi is a small, clean programming language built from the ground up for creating **AI agents** that work with **data**
and **real-time information**.

It’s not a general-purpose language trying to do everything — Mochi is focused. It gives you the tools to build
intelligent, reactive systems with minimal code.

Use Mochi if you want to:

- ✅ Define **composable logic** using `let`, `fun`, `if`, and `for`
- ✅ Write **tests inline** using `test` and `expect`
- ✅ Prepare for **agents, datasets, and event streams**
- ✅ Use a **toolchain-friendly language** with Lisp-style ASTs

| Version  | Feature                      | Status         |
|----------|------------------------------|----------------|
| `v0.2.0` | Tuples, Lists, Maps, Sets    | ✅ In Progress  |
| `v0.2.1` | Struct Types                 | ✅ In Progress  |
| `v0.2.2` | Block Expressions            | 📝 Planned     |
| `v0.2.3` | `match` Expressions          | 📝 Planned     |
| `v0.2.4` | Naive `error` Handling       | 📝 Planned     |
| `v0.3`   | Streams & Event Handlers     | 📝 Planned     |
| `v0.4`   | Agents & Generative Text     | 📝 Planned     |
| `v0.5`   | Datasets & SQL-like Queries  | 📝 Planned     |
| `v0.6`   | Modules & Import System      | 📝 Planned     |
| `v1.0`   | Agent Runtime + DX Polishing | 🚧 Coming Soon |

## ✅ v0.2.0 – Tuples, Lists, Maps, Sets

Support foundational compound data types:

- [ ] Tuples like `(1, "a")`
- [ ] Lists: `[1, 2, 3]`
- [ ] Maps: `{ "key": value }`
- [ ] Sets: `{"a", "b", "c"}`

```mochi
let point = (10, 20)
let items = [1, 2, 3]
let scores = { "math": 95, "science": 88 }
let seen = {"a", "b", "c"}

print(items[1])         // 2
print(scores["math"])   // 95
````

## ✅ v0.2.1 – Struct Types

Support for user-defined record types with dot access:

* [ ] `type Name { field: Type }` declarations
* [ ] Type-safe field access: `user.name`

```mochi
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

## 📝 v0.2.2 – Block Expressions

Allow `{ ... }` blocks to be used as values:

* [ ] Evaluate to the last statement
* [ ] Useful inside `let`, `return`, and expressions

```mochi
let result = {
  let a = 2
  let b = 3
  a * b
}

print(result)  // 6
```

## 📝 v0.2.3 – `match` Expressions

Add `match` support to destructure enums, optionals, and simple patterns:

* [ ] Match variants and values
* [ ] Destructure with bindings

```mochi
match value {
  1 => print("one")
  2 => print("two")
  _ => print("something else")
}

match user {
  { name: "Alice" } => print("Hi Alice")
  { age: 42 } => print("You're 42!")
  _ => print("Unrecognized")
}
```

## 📝 v0.2.4 – Naive `error` Type

Add minimal error-handling capabilities:

* [ ] Union return types: `int | error`
* [ ] `try` expression with fallback using `or`

```mochi
fun parseInt(s: string): int | error {
  if s == "bad" {
    return error "invalid number"
  }
  return 42
}

let value = try parseInt("bad") or -1
print(value)  // -1
```

## 📝 v0.3 – Streams & Event Handlers

Enable event-driven programming with typed streams:

* [ ] `stream` definitions with fields
* [ ] `on ... as ...` handlers

```mochi
stream SensorReading {
  id: string
  temperature: float
  time: time
}

on SensorReading as r {
  print("Temp:", r.temperature)
}
```

## 📝 v0.4 – Agents & Generative Text

### `v0.4.0` – Generate Text

* [ ] Native `generate text` block
* [ ] Use `prompt` + `args` to call language models

```mochi
let answer = generate text {
  prompt: "What is $x + $y?",
  args: { x: 2, y: 3 }
}
```

### `v0.4.1` – Agent DSL

* [ ] `agent` blocks with `state`, `on`, and `intent`
* [ ] Respond to streams or user actions

```mochi
agent Assistant {
  state {
    count: int = 0
  }

  on SensorReading as r {
    count = count + 1
  }

  intent greet(name: string): string {
    return "Hi, " + name
  }
}
```

## 📝 v0.5 – Datasets & Queries

Load datasets and run SQL-style queries inline:

* [ ] `from "data.csv"`
* [ ] `select`, `where`, `join`, `group by`

```mochi
let people = from "people.csv"

let adults = select name
from people
where age > 18

let result = select o.id, c.name
from orders as o
join customers as c on o.customer_id == c.id
```

## 📝 v0.6 – Modules & Imports

Add support for project structure and modularity:

* [ ] `import math`, `import "lib/util.mochi"`
* [ ] Per-file namespaces and clear boundaries

```mochi
import math
let result = math.pow(2, 3)
```

## 🚧 v1.0 – Production Agent Runtime

The 1.0 milestone will unify:

* Full agent runtime (long-running state, background tasks)
* LLM integration (OpenAI, open models)
* Datasets and streaming queries
* Project-level tooling: formatter, type checker, test runner
* Language server support (LSP)

Mochi is designed to grow step-by-step — cleanly, simply, and always with developer experience in mind.

PRs welcome 🙌

