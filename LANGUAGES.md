# Mochi Language Reference (v0.2.6)

Mochi is a small statically typed language focused on clarity and deterministic behavior. This document summarizes the core features available in version 0.2.6.

## 1. Basics

- Source files are UTF‑8 text.
- Bindings are introduced with `let` and are immutable.
- Primitive types include `int`, `float`, `bool`, `string`, and `time`.
- Composite types: `list<T>`, `map<K,V>`, and `set<T>`.

```mochi
let name = "Mochi"
let nums = [1, 2, 3]
let scores = {"a": 10, "b": 20}
```

## 2. Control Flow

Conditional and looping constructs keep syntax concise.

```mochi
if x > 0 {
  print("positive")
} else {
  print("non‑positive")
}

for i in 0..3 {
  print(i)      // range loop
}

for key in scores {
  print(key)   // collection loop (list or map)
}
```

## 3. Functions

Functions are first‑class and statically typed. Anonymous functions use `=>` syntax.

```mochi
fun add(a: int, b: int): int {
  return a + b
}

let square = fun(x: int): int => x * x
print(add(2, 3))
print(square(4))
```

Closures and higher‑order functions are fully supported.

## 4. Collections

Lists, maps, and sets are built in. Elements may be accessed with index syntax.

```mochi
let list = [1, 2, 3]
print(list[0])           // 1

let user = {"name": "Ana", "age": 22}
print(user["name"])     // "Ana"
```

## 5. Datasets and Queries

Datasets are loaded with `from` and queried using SQL‑like syntax.

```mochi
let people = from "people.csv"
let adults = select name, age
from people
where age > 18
```

Joins and aggregations are available for simple analytics.

## 6. Streams

Streams declare structured events. Handlers react to events with `on` blocks.

```mochi
stream SensorReading {
  id: string
  temperature: float
  timestamp: time
}

on SensorReading as r {
  print(r.id, r.temperature)
}
```

## 7. Agents

Agents combine state, event handlers, and callable intents.

```mochi
agent Assistant {
  state {
    count: int = 0
  }

  on SensorReading as e {
    count = count + 1
  }

  intent greet(name: string): string {
    return "Hello, " + name
  }
}
```

## 8. Generative Text

LLM prompts can be embedded with `generate text` blocks.

```mochi
generate text {
  prompt: "Write a haiku about $topic"
  args: { topic: "spring" }
  temperature: 0.7
}
```

## 9. Testing

Inline `test` blocks and `expect` statements make programs self‑verifying.

```mochi
test "math" {
  expect 2 + 2 == 4
}
```

---

Mochi continues to evolve toward v1.0. See `CHANGELOG.md` for detailed history.
