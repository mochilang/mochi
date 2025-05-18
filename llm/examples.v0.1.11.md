# Mochi Examples Index

> 📦 Generated with [Mochi Compiler](https://github.com/mochi-lang/mochi) `v0.1.11`

Each example includes source code, AST, and runtime output.

## 📚 Files

- [agent.mochi](#agent)
- [binary.mochi](#binary)
- [expr.mochi](#expr)
- [for.mochi](#for)
- [fun.mochi](#fun)
- [fun_anon.mochi](#fun_anon)
- [fun_closure.mochi](#fun_closure)
- [fun_curry.mochi](#fun_curry)
- [fun_high_order.mochi](#fun_high_order)
- [fun_infer.mochi](#fun_infer)
- [fun_return.mochi](#fun_return)
- [hello.mochi](#hello)
- [if.mochi](#if)
- [let.mochi](#let)
- [parser/e001.mochi](#parser-e001)
- [parser/e002.mochi](#parser-e002)
- [parser/e003.mochi](#parser-e003)
- [parser/e004.mochi](#parser-e004)
- [parser/e005.mochi](#parser-e005)
- [parser/e006.mochi](#parser-e006)
- [parser/e007.mochi](#parser-e007)
- [parser/e008.mochi](#parser-e008)
- [runtime/errors.mochi](#runtime-errors)
- [stream.mochi](#stream)
- [test.mochi](#test)
- [types/errors.mochi](#types-errors)
- [unary.mochi](#unary)

---
## ⚠️ Errors

The following files failed to compile or run:

- `examples/agent.mochi`: runtime error: unsupported statement: examples/agent.mochi:1:1: agent Logger
- `examples/parser/e001.mochi`: parse error: error[P001]: examples/parser/e001.mochi:3:26: unexpected token "<EOF>" (expected "}")
  --> examples/parser/e001.mochi:3:26

  3 |   return "Hello, " + name
    |                          ^

help:
  Check for a missing `{` or `}` to close the block.
- `examples/parser/e002.mochi`: type error (1 issue(s))
- `examples/parser/e003.mochi`: parse error: error[P999]: examples/parser/e003.mochi:2:5: unexpected token ":"
  --> examples/parser/e003.mochi:2:5

  2 | let : int = 42
    |     ^

help:
  Parse error occurred. Check syntax near this location.
- `examples/parser/e004.mochi`: parse error: error[P040]: examples/parser/e004.mochi:2:15: lexer: invalid input text "\"Unterminated st..."
  --> examples/parser/e004.mochi:2:15

  2 | let message = "Unterminated string
    |               ^

help:
  String literals must be properly closed with a `"`.
- `examples/parser/e005.mochi`: parse error: error[P001]: examples/parser/e005.mochi:2:25: unexpected token "=>" (expected "{" Statement* "}")
  --> examples/parser/e005.mochi:2:25

  2 | fun add(a: int, b: int) => a + b)  // extra closing paren
    |                         ^

help:
  Check for a missing `{` or `}` to close the block.
- `examples/parser/e006.mochi`: parse error: error[P020]: examples/parser/e006.mochi:2:9: unexpected token "*" (expected Primary)
  --> examples/parser/e006.mochi:2:9

  2 | let x = * 5
    |         ^

help:
  An expression was expected here. Check syntax.
- `examples/parser/e007.mochi`: type error (1 issue(s))
- `examples/parser/e008.mochi`: parse error: error[P999]: examples/parser/e008.mochi:2:11: unexpected token "}"
  --> examples/parser/e008.mochi:2:11

  2 | let y = 3 }}
    |           ^

help:
  Parse error occurred. Check syntax near this location.
- `examples/runtime/errors.mochi`: runtime error: 
💥 4 test(s) failed:
  - ❌ field access on non-object: error[I001]: cannot access field 'length' on non-object of type string
  --> examples/runtime/errors.mochi:7:15

  7 |   let value = name.length
    |               ^

help:
  Use a map-like object to access fields.
  - ❌ invalid for loop bounds: error[I010]: range bounds must be integers, got string and string
  --> examples/runtime/errors.mochi:15:3

 15 |   for i in start..end {
    |   ^

help:
  Ensure both `for x in a..b` bounds are integers.
  - ❌ divide by zero: error[I011]: division by zero
  --> examples/runtime/errors.mochi:25:18

 25 |   let result = a / b
    |                  ^

help:
  Make sure the denominator is not zero.
  - ❌ unary ! on string: error[I008]: invalid use of unary '!' on string
  --> examples/runtime/errors.mochi:32:16

 32 |   let result = !x
    |                ^

help:
  Use unary operators only with numeric or boolean values.

- `examples/stream.mochi`: runtime error: unsupported statement: examples/stream.mochi:1:1: stream sensor_data
- `examples/types/errors.mochi`: type error (13 issue(s))

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

## parser/e001.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 1. Missing closing brace in function (E0001)
fun greet(name: string) {
  return "Hello, " + name
```

#### ❌ Parse Error
```text
error[P001]: examples/parser/e001.mochi:3:26: unexpected token "<EOF>" (expected "}")
  --> examples/parser/e001.mochi:3:26

  3 |   return "Hello, " + name
    |                          ^

help:
  Check for a missing `{` or `}` to close the block.
```
</details>

## parser/e002.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 2. Else without matching if (E0002)
else {
  print("no if before else")
}
```

#### 🌲 AST
```lisp
(program
  (exprstmt (selector else))
  (exprstmt
    (funexpr
      (block
        (exprstmt
          (call print (string "no if before else"))
        )
      )
    )
  )
)
```
#### ▶️ Output
```text
🧠 Type Check Failed

   1. error[T002]: undefined variable: else
  --> examples/parser/e002.mochi:2:1

  2 | else {
    | ^

help:
  Check if the variable was declared in this scope.
```
</details>

## parser/e003.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 3. Missing variable name (E0003)
let : int = 42
```

#### ❌ Parse Error
```text
error[P999]: examples/parser/e003.mochi:2:5: unexpected token ":"
  --> examples/parser/e003.mochi:2:5

  2 | let : int = 42
    |     ^

help:
  Parse error occurred. Check syntax near this location.
```
</details>

## parser/e004.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 4. Unclosed string literal
let message = "Unterminated string
```

#### ❌ Parse Error
```text
error[P040]: examples/parser/e004.mochi:2:15: lexer: invalid input text "\"Unterminated st..."
  --> examples/parser/e004.mochi:2:15

  2 | let message = "Unterminated string
    |               ^

help:
  String literals must be properly closed with a `"`.
```
</details>

## parser/e005.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 5. Illegal token usage
fun add(a: int, b: int) => a + b)  // extra closing paren
```

#### ❌ Parse Error
```text
error[P001]: examples/parser/e005.mochi:2:25: unexpected token "=>" (expected "{" Statement* "}")
  --> examples/parser/e005.mochi:2:25

  2 | fun add(a: int, b: int) => a + b)  // extra closing paren
    |                         ^

help:
  Check for a missing `{` or `}` to close the block.
```
</details>

## parser/e006.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 6. Unexpected punctuation
let x = * 5
```

#### ❌ Parse Error
```text
error[P020]: examples/parser/e006.mochi:2:9: unexpected token "*" (expected Primary)
  --> examples/parser/e006.mochi:2:9

  2 | let x = * 5
    |         ^

help:
  An expression was expected here. Check syntax.
```
</details>

## parser/e007.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 7. Incomplete if statement
if 1 < 2
```

#### 🌲 AST
```lisp
(program
  (exprstmt (selector if))
  (exprstmt
    (binary "<" (int 1) (int 2))
  )
)
```
#### ▶️ Output
```text
🧠 Type Check Failed

   1. error[T002]: undefined variable: if
  --> examples/parser/e007.mochi:2:1

  2 | if 1 < 2
    | ^

help:
  Check if the variable was declared in this scope.
```
</details>

## parser/e008.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// 8. Extra closing brace
let y = 3 }}
```

#### ❌ Parse Error
```text
error[P999]: examples/parser/e008.mochi:2:11: unexpected token "}"
  --> examples/parser/e008.mochi:2:11

  2 | let y = 3 }}
    |           ^

help:
  Parse error occurred. Check syntax near this location.
```
</details>

## runtime/errors.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// examples/runtime/errors.mochi
// ✅ Runtime errors only (type checker must pass)

// --- R001: Field access on non-object ---
test "field access on non-object" {
  let name = "Mochi"
  let value = name.length
  expect false
}

// --- R002: Invalid for loop bounds ---
test "invalid for loop bounds" {
  let start = "a"
  let end = "z"
  for i in start..end {
    print(i)
  }
  expect false
}

// --- R003: Division by zero ---
test "divide by zero" {
  let a = 10
  let b = 0
  let result = a / b
  expect false
}

// --- R004: Unary ! on non-bool ---
test "unary ! on string" {
  let x = "hello"
  let result = !x
  expect false
}
```

#### 🌲 AST
```lisp
(program
  (test "field access on non-object"
    (let name (string Mochi))
    (let value
      (selector length (selector name))
    )
    (expect (bool false))
  )
  (test "invalid for loop bounds"
    (let start (string a))
    (let end (string z))
    (for i
      (range (selector start) (selector end))
      (block
        (exprstmt
          (call print (selector i))
        )
      )
    )
    (expect (bool false))
  )
  (test "divide by zero"
    (let a (int 10))
    (let b (int 0))
    (let result
      (binary "/" (selector a) (selector b))
    )
    (expect (bool false))
  )
  (test "unary ! on string"
    (let x (string hello))
    (let result
      (unary "!" (selector x))
    )
    (expect (bool false))
  )
)
```
#### ▶️ Output
```text
💥 Runtime Error

  → 
💥 4 test(s) failed:
  - ❌ field access on non-object: error[I001]: cannot access field 'length' on non-object of type string
  --> examples/runtime/errors.mochi:7:15

  7 |   let value = name.length
    |               ^

help:
  Use a map-like object to access fields.
  - ❌ invalid for loop bounds: error[I010]: range bounds must be integers, got string and string
  --> examples/runtime/errors.mochi:15:3

 15 |   for i in start..end {
    |   ^

help:
  Ensure both `for x in a..b` bounds are integers.
  - ❌ divide by zero: error[I011]: division by zero
  --> examples/runtime/errors.mochi:25:18

 25 |   let result = a / b
    |                  ^

help:
  Make sure the denominator is not zero.
  - ❌ unary ! on string: error[I008]: invalid use of unary '!' on string
  --> examples/runtime/errors.mochi:32:16

 32 |   let result = !x
    |                ^

help:
  Use unary operators only with numeric or boolean values.

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

## types/errors.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### 📄 Source
```mochi
// examples/types/errors.mochi
// ⚠️ Each test below triggers a specific type-checking error (T000 - T013)

test "T000 let requires type or value" {
  let x  // ❌ T000: missing type and value
  expect false
}

test "T001 assign to undeclared variable" {
  x = 42  // ❌ T001: x not declared
  expect false
}

test "T002 undefined variable in expression" {
  let y = z + 1  // ❌ T002: z not defined
  expect false
}

test "T003 unknown function" {
  unknownFn(123)  // ❌ T003: function not defined
  expect false
}

test "T004 type mismatch (explicit type)" {
  let n: int = "hello"  // ❌ T004: string assigned to int
  expect false
}

test "T005 cannot assign to incompatible type" {
  let a = 5
  a = "oops"  // ❌ T005: assigning string to int
  expect false
}

test "T006 function parameter missing type" {
  fun bad(x) { return x }  // ❌ T006: no type for parameter x
  expect false
}

test "T007 expect requires boolean" {
  expect 123  // ❌ T007: not a boolean
}

test "T008 too many arguments" {
  fun f(x: int) { return x }
  f(1, 2)  // ❌ T008: only 1 param expected
  expect false
}

test "T009 argument type mismatch" {
  fun f(x: int, y: string) {}
  f(1, 2)  // ❌ T009: y expects string, got int
  expect false
}

test "T010 not a function" {
  let g = 123
  g()  // ❌ T010: g is not a function
  expect false
}

test "T011 incompatible equality types" {
  let ok = "str" == 42  // ❌ T011: string vs int
  expect false
}

test "T012 incompatible comparison types" {
  let b = true < 3  // ❌ T012: bool vs int
  expect false
}
```

#### 🌲 AST
```lisp
(program
  (test "T000 let requires type or value"
    (let x)
    (expect (bool false))
  )
  (test "T001 assign to undeclared variable"
    (assign x (int 42))
    (expect (bool false))
  )
  (test "T002 undefined variable in expression"
    (let y
      (binary "+" (selector z) (int 1))
    )
    (expect (bool false))
  )
  (test "T003 unknown function"
    (exprstmt
      (call unknownFn (int 123))
    )
    (expect (bool false))
  )
  (test "T004 type mismatch (explicit type)"
    (let n (type int) (string hello))
    (expect (bool false))
  )
  (test "T005 cannot assign to incompatible type"
    (let a (int 5))
    (assign a (string oops))
    (expect (bool false))
  )
  (test "T006 function parameter missing type"
    (fun bad
      (param x)
      (return (selector x))
    )
    (expect (bool false))
  )
  (test "T007 expect requires boolean"
    (expect (int 123))
  )
  (test "T008 too many arguments"
    (fun f
      (param x (type int))
      (return (selector x))
    )
    (exprstmt
      (call f (int 1) (int 2))
    )
    (expect (bool false))
  )
  (test "T009 argument type mismatch"
    (fun f
      (param x (type int))
      (param y (type string))
    )
    (exprstmt
      (call f (int 1) (int 2))
    )
    (expect (bool false))
  )
  (test "T010 not a function"
    (let g (int 123))
    (exprstmt (call g))
    (expect (bool false))
  )
  (test "T011 incompatible equality types"
    (let ok
      (binary "==" (string str) (int 42))
    )
    (expect (bool false))
  )
  (test "T012 incompatible comparison types"
    (let b
      (binary "<" (bool true) (int 3))
    )
    (expect (bool false))
  )
)
```
#### ▶️ Output
```text
🧠 Type Check Failed

   1. error[T000]: `let` requires a type annotation or a value
  --> examples/types/errors.mochi:5:3

  5 |   let x  // ❌ T000: missing type and value
    |   ^

help:
  Write `let x = ...` or `let x: int`.
   2. error[T001]: assignment to undeclared variable: x
  --> examples/types/errors.mochi:10:3

 10 |   x = 42  // ❌ T001: x not declared
    |   ^

help:
  Declare the variable using `let` first.
   3. error[T002]: undefined variable: z
  --> examples/types/errors.mochi:15:11

 15 |   let y = z + 1  // ❌ T002: z not defined
    |           ^

help:
  Check if the variable was declared in this scope.
   4. error[T003]: unknown function: unknownFn
  --> examples/types/errors.mochi:20:3

 20 |   unknownFn(123)  // ❌ T003: function not defined
    |   ^

help:
  Ensure the function is defined before it's called.
   5. error[T004]: type mismatch: expected int but got string
  --> examples/types/errors.mochi:25:3

 25 |   let n: int = "hello"  // ❌ T004: string assigned to int
    |   ^

help:
  Change the value to match the expected type.
   6. error[T005]: cannot assign string to a (expected int)
  --> examples/types/errors.mochi:31:3

 31 |   a = "oops"  // ❌ T005: assigning string to int
    |   ^

help:
  Ensure the types on both sides of `=` are compatible.
   7. error[T006]: parameter `x` is missing a type
  --> examples/types/errors.mochi:36:3

 36 |   fun bad(x) { return x }  // ❌ T006: no type for parameter x
    |   ^

help:
  Add a type annotation like `x: int`.
   8. error[T007]: `expect` must be a boolean expression
  --> examples/types/errors.mochi:41:3

 41 |   expect 123  // ❌ T007: not a boolean
    |   ^

help:
  Return a `true` or `false` condition inside `expect`.
   9. error[T008]: too many arguments: expected 1, got 2
  --> examples/types/errors.mochi:46:3

 46 |   f(1, 2)  // ❌ T008: only 1 param expected
    |   ^

help:
  Remove extra arguments or update the function definition.
  10. error[T009]: argument 2 type mismatch: expected string, got int
  --> examples/types/errors.mochi:52:3

 52 |   f(1, 2)  // ❌ T009: y expects string, got int
    |   ^

help:
  Check the order and type of function arguments.
  11. error[T010]: g is not a function
  --> examples/types/errors.mochi:58:3

 58 |   g()  // ❌ T010: g is not a function
    |   ^

help:
  Use a function or closure in this position.
  12. error[T011]: incompatible types in equality expression
  --> examples/types/errors.mochi:63:18

 63 |   let ok = "str" == 42  // ❌ T011: string vs int
    |                  ^

help:
  Ensure both sides of `==` or `!=` are the same type.
  13. error[T012]: incompatible types in comparison expression
  --> examples/types/errors.mochi:68:16

 68 |   let b = true < 3  // ❌ T012: bool vs int
    |                ^

help:
  Use comparable types like numbers or strings.
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
