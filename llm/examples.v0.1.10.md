# Mochi Examples Index

> 📦 Generated with [Mochi Compiler](https://github.com/mochi-lang/mochi) `v0.1.10`

Each example includes source code, AST, and runtime output.

## 📚 Files

- [agent.mochi](#agent)
- [binary.mochi](#binary)
- [expr.mochi](#expr)
- [for.mochi](#for)
- [fun.mochi](#fun)
- [fun_anon.mochi](#fun-anon)
- [fun_closure.mochi](#fun-closure)
- [fun_curry.mochi](#fun-curry)
- [fun_high_order.mochi](#fun-high-order)
- [fun_infer.mochi](#fun-infer)
- [fun_return.mochi](#fun-return)
- [hello.mochi](#hello)
- [if.mochi](#if)
- [let.mochi](#let)
- [stream.mochi](#stream)
- [test.mochi](#test)
- [unary.mochi](#unary)

---
## ⚠️ Errors

The following files failed to compile or run:

- `agent.mochi`: runtime error: unsupported statement: examples/agent.mochi:1:1: agent Logger
- `stream.mochi`: runtime error: unsupported statement: examples/stream.mochi:1:1: stream sensor_data

---


## agent.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
agent Logger {
  let count: int = 0

  on sensor_data as e {
    count = count + 1
    print("Received event:", e, "Total:", count)
  }

  intent greet(name: string): string {
    return "Hello, " + name
  }
}
```

#### 🌲 AST
```lisp
(program
  (agent Logger
    (let count (type int) (int 0))
    (on sensor_data
      (assign count
        (binary "+" (selector count) (int 1))
      )
      (exprstmt
        (call print (string "Received event:") (selector e) (string "Total:") (selector count))
      )
    )
    (intent greet
      (param name (type string))
      (type string)
      (return
        (binary "+" (string "Hello, ") (selector name))
      )
    )
  )
)
```
#### ▶️ Output
```text
💥 Runtime Error

  → unsupported statement: examples/agent.mochi:1:1: agent Logger
```
</details>

## binary.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// Arithmetic operations
let add = 2 + 3
let sub = 7 - 4
let mul = 5 * 2
let div = 8 / 2

print("add:", add)
print("sub:", sub)
print("mul:", mul)
print("div:", div)

// Comparison operations (numbers)
let eq = 10 == 10
let neq = 10 != 5
let lt = 3 < 5
let lte = 5 <= 5
let gt = 7 > 3
let gte = 6 >= 6

print("eq:", eq)
print("neq:", neq)
print("lt:", lt)
print("lte:", lte)
print("gt:", gt)
print("gte:", gte)

// String comparisons
let sa = "hello"
let sb = "hello"
let sc = "world"

print("str_eq:", sa == sb)
print("str_neq:", sa != sc)
print("str_concat:", sa + " " + sc)

// Boolean comparisons
let ba = true
let bb = false

print("bool_eq:", ba == ba)
print("bool_neq:", ba != bb)
```

#### 🌲 AST
```lisp
(program
  (let add
    (binary "+" (int 2) (int 3))
  )
  (let sub
    (binary - (int 7) (int 4))
  )
  (let mul
    (binary "*" (int 5) (int 2))
  )
  (let div
    (binary "/" (int 8) (int 2))
  )
  (exprstmt
    (call print (string "add:") (selector add))
  )
  (exprstmt
    (call print (string "sub:") (selector sub))
  )
  (exprstmt
    (call print (string "mul:") (selector mul))
  )
  (exprstmt
    (call print (string "div:") (selector div))
  )
  (let eq
    (binary "==" (int 10) (int 10))
  )
  (let neq
    (binary "!=" (int 10) (int 5))
  )
  (let lt
    (binary "<" (int 3) (int 5))
  )
  (let lte
    (binary "<=" (int 5) (int 5))
  )
  (let gt
    (binary ">" (int 7) (int 3))
  )
  (let gte
    (binary ">=" (int 6) (int 6))
  )
  (exprstmt
    (call print (string "eq:") (selector eq))
  )
  (exprstmt
    (call print (string "neq:") (selector neq))
  )
  (exprstmt
    (call print (string "lt:") (selector lt))
  )
  (exprstmt
    (call print (string "lte:") (selector lte))
  )
  (exprstmt
    (call print (string "gt:") (selector gt))
  )
  (exprstmt
    (call print (string "gte:") (selector gte))
  )
  (let sa (string hello))
  (let sb (string hello))
  (let sc (string world))
  (exprstmt
    (call print
      (string "str_eq:")
      (binary "==" (selector sa) (selector sb))
    )
  )
  (exprstmt
    (call print
      (string "str_neq:")
      (binary "!=" (selector sa) (selector sc))
    )
  )
  (exprstmt
    (call print
      (string "str_concat:")
      (binary "+"
        (binary "+" (selector sa) (string " "))
        (selector sc)
      )
    )
  )
  (let ba (bool true))
  (let bb (bool false))
  (exprstmt
    (call print
      (string "bool_eq:")
      (binary "==" (selector ba) (selector ba))
    )
  )
  (exprstmt
    (call print
      (string "bool_neq:")
      (binary "!=" (selector ba) (selector bb))
    )
  )
)
```
#### ▶️ Output
```text
add: 5
sub: 3
mul: 10
div: 4
eq: true
neq: true
lt: true
lte: true
gt: true
gte: true
str_eq: true
str_neq: true
str_concat: hello world
bool_eq: true
bool_neq: true
```
</details>

## expr.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// Basic precedence: * before +
let a = 3 + 4 * 2        // 3 + (4 * 2) = 11
let b = (3 + 4) * 2      // (3 + 4) * 2 = 14

// Mix of +, -, *, /
let c = 10 - 2 * 3 + 4   // 10 - (2 * 3) + 4 = 10 - 6 + 4 = 8
let d = (10 - 2) * (3 + 4) // 8 * 7 = 56

// Pure float expression (int + float is not supported)
let e = 5.0 + 2.5 * 2.0      // 5.0 + 5.0 = 10.0

// Nested parentheses
let f = ((1 + 2) * (3 + 4)) / 7 // (3 * 7) / 7 = 3

print("a =", a)
print("b =", b)
print("c =", c)
print("d =", d)
print("e =", e)
print("f =", f)
```

#### 🌲 AST
```lisp
(program
  (let a
    (binary "+"
      (int 3)
      (binary "*" (int 4) (int 2))
    )
  )
  (let b
    (binary "*"
      (group
        (binary "+" (int 3) (int 4))
      )
      (int 2)
    )
  )
  (let c
    (binary "+"
      (binary -
        (int 10)
        (binary "*" (int 2) (int 3))
      )
      (int 4)
    )
  )
  (let d
    (binary "*"
      (group
        (binary - (int 10) (int 2))
      )
      (group
        (binary "+" (int 3) (int 4))
      )
    )
  )
  (let e
    (binary "+"
      (float 5.0)
      (binary "*" (float 2.5) (float 2.0))
    )
  )
  (let f
    (binary "/"
      (group
        (binary "*"
          (group
            (binary "+" (int 1) (int 2))
          )
          (group
            (binary "+" (int 3) (int 4))
          )
        )
      )
      (int 7)
    )
  )
  (exprstmt
    (call print (string "a =") (selector a))
  )
  (exprstmt
    (call print (string "b =") (selector b))
  )
  (exprstmt
    (call print (string "c =") (selector c))
  )
  (exprstmt
    (call print (string "d =") (selector d))
  )
  (exprstmt
    (call print (string "e =") (selector e))
  )
  (exprstmt
    (call print (string "f =") (selector f))
  )
)
```
#### ▶️ Output
```text
a = 11
b = 14
c = 8
d = 56
e = 10
f = 3
```
</details>

## for.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// Basic for loop from 0 to 4
for i in 0..5 {
  print("i =", i)
}

// Sum from 1 to 10
let sum = 0
for x in 1..11 {
  sum = sum + x
}
print("Sum =", sum)
```

#### 🌲 AST
```lisp
(program
  (for i
    (range (int 0) (int 5))
    (block
      (exprstmt
        (call print (string "i =") (selector i))
      )
    )
  )
  (let sum (int 0))
  (for x
    (range (int 1) (int 11))
    (block
      (assign sum
        (binary "+" (selector sum) (selector x))
      )
    )
  )
  (exprstmt
    (call print (string "Sum =") (selector sum))
  )
)
```
#### ▶️ Output
```text
i = 0
i = 1
i = 2
i = 3
i = 4
Sum = 55
```
</details>

## fun.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// --- Basic Setup ---
let name: string = "Mochi"

// Simple function with return value
fun greet(name: string): string {
  return "Hello, " + name
}

print(greet(name))  // Hello, Mochi

// --- Closure: makeAdder returns a function that captures `n` ---
fun makeAdder(n: int): fun(int): int {
  return fun(x: int): int => x + n
}

let add10: fun(int): int = makeAdder(10)
print(add10(5))   // 15

// --- Currying a multi-arg function ---
fun add(a: int, b: int, c: int): int {
  return a + b + c
}

let step1: fun(int): fun(int): int = add(1)
let step2: fun(int): int = step1(2)
let result: int = step2(3)

print(result)  // 1 + 2 + 3 = 6

// --- Higher-order function: accepts function and value ---
fun apply(f: fun(int): int, x: int): int {
  return f(x)
}

let square: fun(int): int = fun(x: int): int => x * x
print(apply(square, 6))  // 36

// --- Function expression as return value ---
fun always42(): fun(): int {
  return fun(): int => 42
}

let f: fun(): int = always42()
print(f())  // 42
```

#### 🌲 AST
```lisp
(program
  (let name (type string) (string Mochi))
  (fun greet
    (param name (type string))
    (type string)
    (return
      (binary "+" (string "Hello, ") (selector name))
    )
  )
  (exprstmt
    (call print
      (call greet (selector name))
    )
  )
  (fun makeAdder
    (param n (type int))
    (typefun (type int) (type int))
    (return
      (funexpr
        (param x (type int))
        (type int)
        (binary "+" (selector x) (selector n))
      )
    )
  )
  (let add10
    (typefun (type int) (type int))
    (call makeAdder (int 10))
  )
  (exprstmt
    (call print
      (call add10 (int 5))
    )
  )
  (fun add
    (param a (type int))
    (param b (type int))
    (param c (type int))
    (type int)
    (return
      (binary "+"
        (binary "+" (selector a) (selector b))
        (selector c)
      )
    )
  )
  (let step1
    (typefun
      (type int)
      (typefun (type int) (type int))
    )
    (call add (int 1))
  )
  (let step2
    (typefun (type int) (type int))
    (call step1 (int 2))
  )
  (let result
    (type int)
    (call step2 (int 3))
  )
  (exprstmt
    (call print (selector result))
  )
  (fun apply
    (param f
      (typefun (type int) (type int))
    )
    (param x (type int))
    (type int)
    (return
      (call f (selector x))
    )
  )
  (let square
    (typefun (type int) (type int))
    (funexpr
      (param x (type int))
      (type int)
      (binary "*" (selector x) (selector x))
    )
  )
  (exprstmt
    (call print
      (call apply (selector square) (int 6))
    )
  )
  (fun always42
    (typefun (type int))
    (return
      (funexpr (type int) (int 42))
    )
  )
  (let f
    (typefun (type int))
    (call always42)
  )
  (exprstmt
    (call print (call f))
  )
)
```
#### ▶️ Output
```text
Hello, Mochi
15
6
36
42
```
</details>

## fun_anon.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
let square = fun(x: int): int => x * x
print(square(6)) // 36
```

#### 🌲 AST
```lisp
(program
  (let square
    (funexpr
      (param x (type int))
      (type int)
      (binary "*" (selector x) (selector x))
    )
  )
  (exprstmt
    (call print
      (call square (int 6))
    )
  )
)
```
#### ▶️ Output
```text
36
```
</details>

## fun_closure.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// --- Closure: makeAdder returns a function that captures `n` ---
fun makeAdder(n: int): fun(int): int {
  return fun(x: int): int => x + n
}

let add10 = makeAdder(10)
print(add10(5))   // 15
```

#### 🌲 AST
```lisp
(program
  (fun makeAdder
    (param n (type int))
    (typefun (type int) (type int))
    (return
      (funexpr
        (param x (type int))
        (type int)
        (binary "+" (selector x) (selector n))
      )
    )
  )
  (let add10
    (call makeAdder (int 10))
  )
  (exprstmt
    (call print
      (call add10 (int 5))
    )
  )
)
```
#### ▶️ Output
```text
15
```
</details>

## fun_curry.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
fun add(a: int, b: int, c: int): int {
    return a + b + c
}

let step1 = add(1)
let step2 = step1(2)
let result = step2(3)

print(result)  // 1 + 2 + 3 = 6
```

#### 🌲 AST
```lisp
(program
  (fun add
    (param a (type int))
    (param b (type int))
    (param c (type int))
    (type int)
    (return
      (binary "+"
        (binary "+" (selector a) (selector b))
        (selector c)
      )
    )
  )
  (let step1
    (call add (int 1))
  )
  (let step2
    (call step1 (int 2))
  )
  (let result
    (call step2 (int 3))
  )
  (exprstmt
    (call print (selector result))
  )
)
```
#### ▶️ Output
```text
6
```
</details>

## fun_high_order.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// --- Higher-order function: accepts function and value ---
fun apply_twice(f: fun(int): int, x: int): int {
  return f(f(x))
}

let square = fun(x: int): int => x * x
print(apply_twice(square, 6)) // 36 * 36 = 1296
```

#### 🌲 AST
```lisp
(program
  (fun apply_twice
    (param f
      (typefun (type int) (type int))
    )
    (param x (type int))
    (type int)
    (return
      (call f
        (call f (selector x))
      )
    )
  )
  (let square
    (funexpr
      (param x (type int))
      (type int)
      (binary "*" (selector x) (selector x))
    )
  )
  (exprstmt
    (call print
      (call apply_twice (selector square) (int 6))
    )
  )
)
```
#### ▶️ Output
```text
1296
```
</details>

## fun_infer.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// --- Basic Setup ---
let name = "Mochi"

// Simple function
fun greet(name: string): string {
  return "Hello, " + name
}

print(greet(name))  // Hello, Mochi

// --- Closure that captures a variable ---
fun makeAdder(n: int): fun(int): int {
  return fun(x: int): int => x + n
}

let add10 = makeAdder(10)
print(add10(5))   // 15

// --- Currying a multi-arg function ---
fun add(a: int, b: int, c: int): int {
  return a + b + c
}

let step1 = add(1)
let step2 = step1(2)
let result = step2(3)

print(result)  // 1 + 2 + 3 = 6

// --- Higher-order function ---
fun apply(f: fun(int): int, x: int): int {
  return f(x)
}

let square = fun(x:int):int => x * x
print(apply(square, 6))  // 36

// --- Function returning a constant function ---
fun always42(): fun(): int {
  return fun() => 42
}

let f = always42()
print(f())  // 42
```

#### 🌲 AST
```lisp
(program
  (let name (string Mochi))
  (fun greet
    (param name (type string))
    (type string)
    (return
      (binary "+" (string "Hello, ") (selector name))
    )
  )
  (exprstmt
    (call print
      (call greet (selector name))
    )
  )
  (fun makeAdder
    (param n (type int))
    (typefun (type int) (type int))
    (return
      (funexpr
        (param x (type int))
        (type int)
        (binary "+" (selector x) (selector n))
      )
    )
  )
  (let add10
    (call makeAdder (int 10))
  )
  (exprstmt
    (call print
      (call add10 (int 5))
    )
  )
  (fun add
    (param a (type int))
    (param b (type int))
    (param c (type int))
    (type int)
    (return
      (binary "+"
        (binary "+" (selector a) (selector b))
        (selector c)
      )
    )
  )
  (let step1
    (call add (int 1))
  )
  (let step2
    (call step1 (int 2))
  )
  (let result
    (call step2 (int 3))
  )
  (exprstmt
    (call print (selector result))
  )
  (fun apply
    (param f
      (typefun (type int) (type int))
    )
    (param x (type int))
    (type int)
    (return
      (call f (selector x))
    )
  )
  (let square
    (funexpr
      (param x (type int))
      (type int)
      (binary "*" (selector x) (selector x))
    )
  )
  (exprstmt
    (call print
      (call apply (selector square) (int 6))
    )
  )
  (fun always42
    (typefun (type int))
    (return
      (funexpr (int 42))
    )
  )
  (let f (call always42))
  (exprstmt
    (call print (call f))
  )
)
```
#### ▶️ Output
```text
Hello, Mochi
15
6
36
42
```
</details>

## fun_return.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// --- Function expression as return value ---
fun always42(): fun(): int {
  return fun(): int => 42
}

let f = always42()
print(f())  // 42
```

#### 🌲 AST
```lisp
(program
  (fun always42
    (typefun (type int))
    (return
      (funexpr (type int) (int 42))
    )
  )
  (let f (call always42))
  (exprstmt
    (call print (call f))
  )
)
```
#### ▶️ Output
```text
42
```
</details>

## hello.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
print("Hello, world")
```

#### 🌲 AST
```lisp
(program
  (exprstmt
    (call print (string "Hello, world"))
  )
)
```
#### ▶️ Output
```text
Hello, world
```
</details>

## if.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
let age = 20

if age >= 18 {
  print("You are an adult.")
} else {
  print("You are a minor.")
}

// Nested if
let score = 85

if score >= 90 {
  print("Grade: A")
} else if score >= 80 {
  print("Grade: B")
} else {
  print("Grade: C or lower")
}
```

#### 🌲 AST
```lisp
(program
  (let age (int 20))
  (if
    (binary ">=" (selector age) (int 18))
    (block
      (exprstmt
        (call print (string "You are an adult."))
      )
    )
    (block
      (exprstmt
        (call print (string "You are a minor."))
      )
    )
  )
  (let score (int 85))
  (if
    (binary ">=" (selector score) (int 90))
    (block
      (exprstmt
        (call print (string "Grade: A"))
      )
    )
    (if
      (binary ">=" (selector score) (int 80))
      (block
        (exprstmt
          (call print (string "Grade: B"))
        )
      )
      (block
        (exprstmt
          (call print (string "Grade: C or lower"))
        )
      )
    )
  )
)
```
#### ▶️ Output
```text
You are an adult.
Grade: B
```
</details>

## let.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
let x = 42
let name: string = "Mochi"
print(x)
print(name)
```

#### 🌲 AST
```lisp
(program
  (let x (int 42))
  (let name (type string) (string Mochi))
  (exprstmt
    (call print (selector x))
  )
  (exprstmt
    (call print (selector name))
  )
)
```
#### ▶️ Output
```text
42
Mochi
```
</details>

## stream.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
stream sensor_data {
  device: string
  payload: {
    temperature: float
    humidity: float
  }
  timestamp: time
}

// on sensor_data as e {
//   print("Temp:", e.payload.temperature, "Humidity:", e.payload.humidity)
// }
```

#### 🌲 AST
```lisp
(program
  (stream sensor_data
    (field "device:string")
    (field "payload:" (field "temperature:float") (field "humidity:float"))
    (field "timestamp:time")
  )
)
```
#### ▶️ Output
```text
💥 Runtime Error

  → unsupported statement: examples/stream.mochi:1:1: stream sensor_data
```
</details>

## test.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// examples/test.mochi

test "basic arithmetic" {
  expect 1 + 2 == 3
  expect 10 - 4 == 6
  expect 2 * 3 == 6
  expect 8 / 2 == 4
}

test "equality and comparison" {
  expect 5 == 5
  expect 3 != 4
  expect 2 < 5
  expect 6 >= 6
  expect 9 > 3
}

test "let bindings" {
  let x = 42
  expect x == 42

  let y = x + 1
  expect y == 43
}

fun square(n: int): int {
  return n * n
}

test "function definitions and calls" {
  expect square(3) == 9
  expect square(5) == 25
}

fun make_adder(n: int): fun(int): int {
  return fun(x: int): int => x + n
}

test "closures and partial application" {
  let add10 = make_adder(10)
  expect add10(5) == 15
  expect add10(0) == 10
}

fun classify(n: int): string {
  if n < 0 {
    return "negative"
  } else if n == 0 {
    return "zero"
  } else {
    return "positive"
  }
}

test "if/else logic" {
  expect classify(-1) == "negative"
  expect classify(0) == "zero"
  expect classify(1) == "positive"
}

test "for loop and accumulation" {
  let sum = 0
  for i in 1..5 {
    sum = sum + i
  }

  // 1 + 2 + 3 + 4 = 10
  expect sum == 10
}
```

#### 🌲 AST
```lisp
(program
  (test "basic arithmetic"
    (expect
      (binary "=="
        (binary "+" (int 1) (int 2))
        (int 3)
      )
    )
    (expect
      (binary "=="
        (binary - (int 10) (int 4))
        (int 6)
      )
    )
    (expect
      (binary "=="
        (binary "*" (int 2) (int 3))
        (int 6)
      )
    )
    (expect
      (binary "=="
        (binary "/" (int 8) (int 2))
        (int 4)
      )
    )
  )
  (test "equality and comparison"
    (expect
      (binary "==" (int 5) (int 5))
    )
    (expect
      (binary "!=" (int 3) (int 4))
    )
    (expect
      (binary "<" (int 2) (int 5))
    )
    (expect
      (binary ">=" (int 6) (int 6))
    )
    (expect
      (binary ">" (int 9) (int 3))
    )
  )
  (test "let bindings"
    (let x (int 42))
    (expect
      (binary "==" (selector x) (int 42))
    )
    (let y
      (binary "+" (selector x) (int 1))
    )
    (expect
      (binary "==" (selector y) (int 43))
    )
  )
  (fun square
    (param n (type int))
    (type int)
    (return
      (binary "*" (selector n) (selector n))
    )
  )
  (test "function definitions and calls"
    (expect
      (binary "=="
        (call square (int 3))
        (int 9)
      )
    )
    (expect
      (binary "=="
        (call square (int 5))
        (int 25)
      )
    )
  )
  (fun make_adder
    (param n (type int))
    (typefun (type int) (type int))
    (return
      (funexpr
        (param x (type int))
        (type int)
        (binary "+" (selector x) (selector n))
      )
    )
  )
  (test "closures and partial application"
    (let add10
      (call make_adder (int 10))
    )
    (expect
      (binary "=="
        (call add10 (int 5))
        (int 15)
      )
    )
    (expect
      (binary "=="
        (call add10 (int 0))
        (int 10)
      )
    )
  )
  (fun classify
    (param n (type int))
    (type string)
    (if
      (binary "<" (selector n) (int 0))
      (block
        (return (string negative))
      )
      (if
        (binary "==" (selector n) (int 0))
        (block
          (return (string zero))
        )
        (block
          (return (string positive))
        )
      )
    )
  )
  (test "if/else logic"
    (expect
      (binary "=="
        (call classify
          (unary - (int 1))
        )
        (string negative)
      )
    )
    (expect
      (binary "=="
        (call classify (int 0))
        (string zero)
      )
    )
    (expect
      (binary "=="
        (call classify (int 1))
        (string positive)
      )
    )
  )
  (test "for loop and accumulation"
    (let sum (int 0))
    (for i
      (range (int 1) (int 5))
      (block
        (assign sum
          (binary "+" (selector sum) (selector i))
        )
      )
    )
    (expect
      (binary "==" (selector sum) (int 10))
    )
  )
)
```
#### ▶️ Output
```text
```
</details>

## unary.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// Basic unary negation and boolean not
let x = -5
let y = !true

// Double negation and double not
let a = -(-10)          // 10
let b = !false          // true
let c = !!true          // true

// Nested expressions with unary
let d = -(3 + 2)        // -5
let e = !((2 < 3) == true)  // !(true == true) => !true => false

print("x =", x)
print("y =", y)
print("a =", a)
print("b =", b)
print("c =", c)
print("d =", d)
print("e =", e)
```

#### 🌲 AST
```lisp
(program
  (let x
    (unary - (int 5))
  )
  (let y
    (unary "!" (bool true))
  )
  (let a
    (unary -
      (group
        (unary - (int 10))
      )
    )
  )
  (let b
    (unary "!" (bool false))
  )
  (let c
    (unary "!"
      (unary "!" (bool true))
    )
  )
  (let d
    (unary -
      (group
        (binary "+" (int 3) (int 2))
      )
    )
  )
  (let e
    (unary "!"
      (group
        (binary "=="
          (group
            (binary "<" (int 2) (int 3))
          )
          (bool true)
        )
      )
    )
  )
  (exprstmt
    (call print (string "x =") (selector x))
  )
  (exprstmt
    (call print (string "y =") (selector y))
  )
  (exprstmt
    (call print (string "a =") (selector a))
  )
  (exprstmt
    (call print (string "b =") (selector b))
  )
  (exprstmt
    (call print (string "c =") (selector c))
  )
  (exprstmt
    (call print (string "d =") (selector d))
  )
  (exprstmt
    (call print (string "e =") (selector e))
  )
)
```
#### ▶️ Output
```text
x = -5
y = false
a = 10
b = true
c = true
d = -5
e = false
```
</details>
