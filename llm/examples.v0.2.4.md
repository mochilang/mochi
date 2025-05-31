# Mochi Examples Index

> Generated with [Mochi](https://github.com/mochi-lang/mochi) `v0.2.4`

Each example includes source code, AST, and runtime output.

## Files

- [v0.1/agent.mochi](#v0.1-agent)
- [v0.1/binary.mochi](#v0.1-binary)
- [v0.1/expr.mochi](#v0.1-expr)
- [v0.1/for.mochi](#v0.1-for)
- [v0.1/fun.mochi](#v0.1-fun)
- [v0.1/fun_anon.mochi](#v0.1-fun_anon)
- [v0.1/fun_closure.mochi](#v0.1-fun_closure)
- [v0.1/fun_curry.mochi](#v0.1-fun_curry)
- [v0.1/fun_high_order.mochi](#v0.1-fun_high_order)
- [v0.1/fun_infer.mochi](#v0.1-fun_infer)
- [v0.1/fun_return.mochi](#v0.1-fun_return)
- [v0.1/hello.mochi](#v0.1-hello)
- [v0.1/if.mochi](#v0.1-if)
- [v0.1/let.mochi](#v0.1-let)
- [v0.1/stream.mochi](#v0.1-stream)
- [v0.1/test.mochi](#v0.1-test)
- [v0.1/unary.mochi](#v0.1-unary)
- [v0.2/list.mochi](#v0.2-list)
- [v0.2/map.mochi](#v0.2-map)
- [v0.2/matrix.mochi](#v0.2-matrix)
- [v0.2/π.mochi](#v0.2-π)

---

## v0.1/agent.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
/*
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
*/
```

#### AST
```lisp
(program)
```
#### Output
```text
```
</details>

## v0.1/binary.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (let add
    (binary + (int 2) (int 3))
  )
  (let sub
    (binary - (int 7) (int 4))
  )
  (let mul
    (binary * (int 5) (int 2))
  )
  (let div
    (binary / (int 8) (int 2))
  )
  (call print (string "add:") (selector add))
  (call print (string "sub:") (selector sub))
  (call print (string "mul:") (selector mul))
  (call print (string "div:") (selector div))
  (let eq
    (binary == (int 10) (int 10))
  )
  (let neq
    (binary != (int 10) (int 5))
  )
  (let lt
    (binary < (int 3) (int 5))
  )
  (let lte
    (binary <= (int 5) (int 5))
  )
  (let gt
    (binary > (int 7) (int 3))
  )
  (let gte
    (binary >= (int 6) (int 6))
  )
  (call print (string "eq:") (selector eq))
  (call print (string "neq:") (selector neq))
  (call print (string "lt:") (selector lt))
  (call print (string "lte:") (selector lte))
  (call print (string "gt:") (selector gt))
  (call print (string "gte:") (selector gte))
  (let sa (string hello))
  (let sb (string hello))
  (let sc (string world))
  (call print
    (string "str_eq:")
    (binary == (selector sa) (selector sb))
  )
  (call print
    (string "str_neq:")
    (binary != (selector sa) (selector sc))
  )
  (call print
    (string "str_concat:")
    (binary +
      (binary + (selector sa) (string " "))
      (selector sc)
    )
  )
  (let ba (bool true))
  (let bb (bool true))
  (call print
    (string "bool_eq:")
    (binary == (selector ba) (selector ba))
  )
  (call print
    (string "bool_neq:")
    (binary != (selector ba) (selector bb))
  )
)
```
#### Output
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
bool_neq: false
```
</details>

## v0.1/expr.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (let a
    (binary *
      (binary + (int 3) (int 4))
      (int 2)
    )
  )
  (let b
    (binary *
      (group
        (binary + (int 3) (int 4))
      )
      (int 2)
    )
  )
  (let c
    (binary +
      (binary *
        (binary - (int 10) (int 2))
        (int 3)
      )
      (int 4)
    )
  )
  (let d
    (binary *
      (group
        (binary - (int 10) (int 2))
      )
      (group
        (binary + (int 3) (int 4))
      )
    )
  )
  (let e
    (binary *
      (binary + (float 5) (float 2.5))
      (float 2)
    )
  )
  (let f
    (binary /
      (group
        (binary *
          (group
            (binary + (int 1) (int 2))
          )
          (group
            (binary + (int 3) (int 4))
          )
        )
      )
      (int 7)
    )
  )
  (call print (string "a =") (selector a))
  (call print (string "b =") (selector b))
  (call print (string "c =") (selector c))
  (call print (string "d =") (selector d))
  (call print (string "e =") (selector e))
  (call print (string "f =") (selector f))
)
```
#### Output
```text
a = 11
b = 14
c = 8
d = 56
e = 10
f = 3
```
</details>

## v0.1/for.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (for i
    (range (int 0) (int 5))
    (block
      (call print (string "i =") (selector i))
    )
  )
  (let sum (int 0))
  (for x
    (range (int 1) (int 11))
    (block
      (assign sum
        (binary + (selector sum) (selector x))
      )
    )
  )
  (call print (string "Sum =") (selector sum))
)
```
#### Output
```text
i = 0
i = 1
i = 2
i = 3
i = 4
Sum = 55
```
</details>

## v0.1/fun.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (let name (type string) (string Mochi))
  (fun greet
    (param name (type string))
    (type string)
    (return
      (binary + (string "Hello, ") (selector name))
    )
  )
  (call print
    (call greet (selector name))
  )
  (fun makeAdder
    (param n (type int))
    (typefun (type int) (type int))
    (return
      (funexpr
        (param x (type int))
        (type int)
        (binary + (selector x) (selector n))
      )
    )
  )
  (let add10
    (typefun (type int) (type int))
    (call makeAdder (int 10))
  )
  (call print
    (call add10 (int 5))
  )
  (fun add
    (param a (type int))
    (param b (type int))
    (param c (type int))
    (type int)
    (return
      (binary +
        (binary + (selector a) (selector b))
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
  (call print (selector result))
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
      (binary * (selector x) (selector x))
    )
  )
  (call print
    (call apply (selector square) (int 6))
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
  (call print (call f))
)
```
#### Output
```text
Hello, Mochi
15
6
36
42
```
</details>

## v0.1/fun_anon.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
let square = fun(x: int): int => x * x
print(square(6)) // 36
```

#### AST
```lisp
(program
  (let square
    (funexpr
      (param x (type int))
      (type int)
      (binary * (selector x) (selector x))
    )
  )
  (call print
    (call square (int 6))
  )
)
```
#### Output
```text
36
```
</details>

## v0.1/fun_closure.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
// --- Closure: makeAdder returns a function that captures `n` ---
fun makeAdder(n: int): fun(int): int {
  return fun(x: int): int => x + n
}

let add10 = makeAdder(10)
print(add10(5))   // 15
```

#### AST
```lisp
(program
  (fun makeAdder
    (param n (type int))
    (typefun (type int) (type int))
    (return
      (funexpr
        (param x (type int))
        (type int)
        (binary + (selector x) (selector n))
      )
    )
  )
  (let add10
    (call makeAdder (int 10))
  )
  (call print
    (call add10 (int 5))
  )
)
```
#### Output
```text
15
```
</details>

## v0.1/fun_curry.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
fun add(a: int, b: int, c: int): int {
    return a + b + c
}

let step1 = add(1)
let step2 = step1(2)
let result = step2(3)

print(result)  // 1 + 2 + 3 = 6
```

#### AST
```lisp
(program
  (fun add
    (param a (type int))
    (param b (type int))
    (param c (type int))
    (type int)
    (return
      (binary +
        (binary + (selector a) (selector b))
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
  (call print (selector result))
)
```
#### Output
```text
6
```
</details>

## v0.1/fun_high_order.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
// --- Higher-order function: accepts function and value ---
fun apply_twice(f: fun(int): int, x: int): int {
  return f(f(x))
}

let square = fun(x: int): int => x * x
print(apply_twice(square, 6)) // 36 * 36 = 1296
```

#### AST
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
      (binary * (selector x) (selector x))
    )
  )
  (call print
    (call apply_twice (selector square) (int 6))
  )
)
```
#### Output
```text
1296
```
</details>

## v0.1/fun_infer.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (let name (string Mochi))
  (fun greet
    (param name (type string))
    (type string)
    (return
      (binary + (string "Hello, ") (selector name))
    )
  )
  (call print
    (call greet (selector name))
  )
  (fun makeAdder
    (param n (type int))
    (typefun (type int) (type int))
    (return
      (funexpr
        (param x (type int))
        (type int)
        (binary + (selector x) (selector n))
      )
    )
  )
  (let add10
    (call makeAdder (int 10))
  )
  (call print
    (call add10 (int 5))
  )
  (fun add
    (param a (type int))
    (param b (type int))
    (param c (type int))
    (type int)
    (return
      (binary +
        (binary + (selector a) (selector b))
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
  (call print (selector result))
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
      (binary * (selector x) (selector x))
    )
  )
  (call print
    (call apply (selector square) (int 6))
  )
  (fun always42
    (typefun (type int))
    (return
      (funexpr (int 42))
    )
  )
  (let f (call always42))
  (call print (call f))
)
```
#### Output
```text
Hello, Mochi
15
6
36
42
```
</details>

## v0.1/fun_return.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
// --- Function expression as return value ---
fun always42(): fun(): int {
  return fun(): int => 42
}

let f = always42()
print(f())  // 42
```

#### AST
```lisp
(program
  (fun always42
    (typefun (type int))
    (return
      (funexpr (type int) (int 42))
    )
  )
  (let f (call always42))
  (call print (call f))
)
```
#### Output
```text
42
```
</details>

## v0.1/hello.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
print("Hello, world")
```

#### AST
```lisp
(program
  (call print (string "Hello, world"))
)
```
#### Output
```text
Hello, world
```
</details>

## v0.1/if.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (let age (int 20))
  (if
    (binary >= (selector age) (int 18))
    (block
      (call print (string "You are an adult."))
    )
    (block
      (call print (string "You are a minor."))
    )
  )
  (let score (int 85))
  (if
    (binary >= (selector score) (int 90))
    (block
      (call print (string "Grade: A"))
    )
    (if
      (binary >= (selector score) (int 80))
      (block
        (call print (string "Grade: B"))
      )
      (block
        (call print (string "Grade: C or lower"))
      )
    )
  )
)
```
#### Output
```text
You are an adult.
Grade: B
```
</details>

## v0.1/let.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
let x = 42
let name: string = "Mochi"
print(x)
print(name)
```

#### AST
```lisp
(program
  (let x (int 42))
  (let name (type string) (string Mochi))
  (call print (selector x))
  (call print (selector name))
)
```
#### Output
```text
42
Mochi
```
</details>

## v0.1/stream.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
/*
stream sensor_data {
  device: string
  payload: {
    temperature: float
    humidity: float
  }
  timestamp: time
}

on sensor_data as e {
  print("Temp:", e.payload.temperature, "Humidity:", e.payload.humidity)
}
*/
```

#### AST
```lisp
(program)
```
#### Output
```text
```
</details>

## v0.1/test.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (test "basic arithmetic"
    (expect
      (binary ==
        (binary + (int 1) (int 2))
        (int 3)
      )
    )
    (expect
      (binary ==
        (binary - (int 10) (int 4))
        (int 6)
      )
    )
    (expect
      (binary ==
        (binary * (int 2) (int 3))
        (int 6)
      )
    )
    (expect
      (binary ==
        (binary / (int 8) (int 2))
        (int 4)
      )
    )
  )
  (test "equality and comparison"
    (expect
      (binary == (int 5) (int 5))
    )
    (expect
      (binary != (int 3) (int 4))
    )
    (expect
      (binary < (int 2) (int 5))
    )
    (expect
      (binary >= (int 6) (int 6))
    )
    (expect
      (binary > (int 9) (int 3))
    )
  )
  (test "let bindings"
    (let x (int 42))
    (expect
      (binary == (selector x) (int 42))
    )
    (let y
      (binary + (selector x) (int 1))
    )
    (expect
      (binary == (selector y) (int 43))
    )
  )
  (fun square
    (param n (type int))
    (type int)
    (return
      (binary * (selector n) (selector n))
    )
  )
  (test "function definitions and calls"
    (expect
      (binary ==
        (call square (int 3))
        (int 9)
      )
    )
    (expect
      (binary ==
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
        (binary + (selector x) (selector n))
      )
    )
  )
  (test "closures and partial application"
    (let add10
      (call make_adder (int 10))
    )
    (expect
      (binary ==
        (call add10 (int 5))
        (int 15)
      )
    )
    (expect
      (binary ==
        (call add10 (int 0))
        (int 10)
      )
    )
  )
  (fun classify
    (param n (type int))
    (type string)
    (if
      (binary < (selector n) (int 0))
      (block
        (return (string negative))
      )
      (if
        (binary == (selector n) (int 0))
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
      (binary ==
        (call classify
          (unary - (int 1))
        )
        (string negative)
      )
    )
    (expect
      (binary ==
        (call classify (int 0))
        (string zero)
      )
    )
    (expect
      (binary ==
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
          (binary + (selector sum) (selector i))
        )
      )
    )
    (expect
      (binary == (selector sum) (int 10))
    )
  )
)
```
#### Output
```text
```
</details>

## v0.1/unary.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
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

#### AST
```lisp
(program
  (let x
    (unary - (int 5))
  )
  (let y
    (unary ! (bool true))
  )
  (let a
    (unary -
      (group
        (unary - (int 10))
      )
    )
  )
  (let b
    (unary ! (bool true))
  )
  (let c
    (unary !
      (unary ! (bool true))
    )
  )
  (let d
    (unary -
      (group
        (binary + (int 3) (int 2))
      )
    )
  )
  (let e
    (unary !
      (group
        (binary ==
          (group
            (binary < (int 2) (int 3))
          )
          (bool true)
        )
      )
    )
  )
  (call print (string "x =") (selector x))
  (call print (string "y =") (selector y))
  (call print (string "a =") (selector a))
  (call print (string "b =") (selector b))
  (call print (string "c =") (selector c))
  (call print (string "d =") (selector d))
  (call print (string "e =") (selector e))
)
```
#### Output
```text
x = -5
y = false
a = 10
b = false
c = true
d = -5
e = false
```
</details>

## v0.2/list.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
// examples/v0.2/list.mochi

// Define a list with mixed values
let values = [1, 2, 3, 4, 5]
print(values)

// Access elements by index
print("first: ", values[0])       //> first: 1
print("last: ", values[-1])       //> last: 5
print("second to last: ", values[-2]) //> second to last: 4

// Length of list
print("len: ", len(values))       //> len: 5

// Create a list of strings
let fruits = ["🍎", "🍌", "🍇", "🍓"]
print("fruits: ", fruits)

// Indexing
print("favorite: ", fruits[2])    //> favorite: 🍇

// Slicing: from index 1 to 3 (exclusive)
let some = fruits[1:3]
print("some fruits: ", some)      //> some fruits: ["🍌", "🍇"]

// Slice with open start or end
// print(fruits[:2]) //> ["🍎", "🍌"] - not supported for now
// print(fruits[2:]) //> ["🍇", "🍓"] - not supported for now

// Slice full copy
// let clone = fruits[:]
// print("clone: " + clone)

// Test: indexing and slicing
test "indexing and slicing" {
  expect values[0] == 1
  expect values[-1] == 5
  expect fruits[1] == "🍌"
  // expect fruits[:2] == ["🍎", "🍌"]
  // expect fruits[2:] == ["🍇", "🍓"]
  expect fruits[1:3] == ["🍌", "🍇"]
  // expect fruits[:] == fruits - not supported for now
}
```

#### AST
```lisp
(program
  (let values
    (list (int 1) (int 2) (int 3) (int 4) (int 5))
  )
  (call print (selector values))
  (call print
    (string "first: ")
    (index (selector values) (int 0))
  )
  (call print
    (string "last: ")
    (index
      (selector values)
      (unary - (int 1))
    )
  )
  (call print
    (string "second to last: ")
    (index
      (selector values)
      (unary - (int 2))
    )
  )
  (call print
    (string "len: ")
    (call len (selector values))
  )
  (let fruits
    (list (string "🍎") (string "🍌") (string "🍇") (string "🍓"))
  )
  (call print (string "fruits: ") (selector fruits))
  (call print
    (string "favorite: ")
    (index (selector fruits) (int 2))
  )
  (let some
    (index
      (selector fruits)
      (start (int 1))
      (end (int 3))
    )
  )
  (call print (string "some fruits: ") (selector some))
  (test "indexing and slicing"
    (expect
      (binary ==
        (index (selector values) (int 0))
        (int 1)
      )
    )
    (expect
      (binary ==
        (index
          (selector values)
          (unary - (int 1))
        )
        (int 5)
      )
    )
    (expect
      (binary ==
        (index (selector fruits) (int 1))
        (string "🍌")
      )
    )
    (expect
      (binary ==
        (index
          (selector fruits)
          (start (int 1))
          (end (int 3))
        )
        (list (string "🍌") (string "🍇"))
      )
    )
  )
)
```
#### Output
```text
[1 2 3 4 5]
first:  1
last:  5
second to last:  4
len:  5
fruits:  [🍎 🍌 🍇 🍓]
favorite:  🍇
some fruits:  [🍌 🍇]
```
</details>

## v0.2/map.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
// examples/v0.2/map.mochi

// Declare a map with string keys and int values
let scores: map<string, int> = {
  "Alice": 10,
  "Bob": 15
}

// Insert or update values
// scores["Charlie"] = 20 - won't support for now assign only name = expr
// scores["Alice"] = 12

// Access a value
let aliceScore = scores["Alice"]
print("Alice's score:", aliceScore)

// Safe access with default fallback
// let unknownScore = scores["Zoe"] or 0
// print("Zoe's score:", unknownScore)  // prints 0

// Remove a key using `del`
// del scores["Bob"] // Remove this, make it immutable

// Iterate over key-value pairs
/*
print("Final scores:")
for name, score in scores {
  print(name + " scored " + score)
}
*/

// Length of the map
print("Total players:", len(scores))

// Test block
test "map basic operations" {
  expect scores["Alice"] == 10
  expect len(scores) == 2
}
```

#### AST
```lisp
(program
  (let scores
    (type map (type string) (type int))
    (map
      (entry (string Alice) (int 10))
      (entry (string Bob) (int 15))
    )
  )
  (let aliceScore
    (index (selector scores) (string Alice))
  )
  (call print (string "Alice's score:") (selector aliceScore))
  (call print
    (string "Total players:")
    (call len (selector scores))
  )
  (test "map basic operations"
    (expect
      (binary ==
        (index (selector scores) (string Alice))
        (int 10)
      )
    )
    (expect
      (binary ==
        (call len (selector scores))
        (int 2)
      )
    )
  )
)
```
#### Output
```text
Alice's score: 10
Total players: 2
```
</details>

## v0.2/matrix.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
// examples/v0.2/matrix.mochi

// Define a 2D matrix (list of lists)
let matrix = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9],  // <- allow trailing comma
]

// Print the whole matrix
print("matrix: ", matrix)

// Access rows
print("first row: ", matrix[0])    //> [1, 2, 3]
print("last row: ", matrix[-1])    //> [7, 8, 9]

// Access individual elements
print("top-left: ", matrix[0][0])  //> 1
print("center: ", matrix[1][1])    //> 5
print("bottom-right: ", matrix[2][2]) //> 9

// Test row length
let row = matrix[1]
print("row 1 length: ", len(row))  //> 3

// Test nested indexing
test "matrix indexing" {
  expect matrix[0][0] == 1
  expect matrix[1][1] == 5
  expect matrix[2][2] == 9
  expect matrix[-1][0] == 7
  expect matrix[1][-1] == 60
}
```

#### AST
```lisp
(program
  (let matrix
    (list
      (list (int 1) (int 2) (int 3))
      (list (int 4) (int 5) (int 6))
      (list (int 7) (int 8) (int 9))
    )
  )
  (call print (string "matrix: ") (selector matrix))
  (call print
    (string "first row: ")
    (index (selector matrix) (int 0))
  )
  (call print
    (string "last row: ")
    (index
      (selector matrix)
      (unary - (int 1))
    )
  )
  (call print
    (string "top-left: ")
    (index
      (index (selector matrix) (int 0))
      (int 0)
    )
  )
  (call print
    (string "center: ")
    (index
      (index (selector matrix) (int 1))
      (int 1)
    )
  )
  (call print
    (string "bottom-right: ")
    (index
      (index (selector matrix) (int 2))
      (int 2)
    )
  )
  (let row
    (index (selector matrix) (int 1))
  )
  (call print
    (string "row 1 length: ")
    (call len (selector row))
  )
  (test "matrix indexing"
    (expect
      (binary ==
        (index
          (index (selector matrix) (int 0))
          (int 0)
        )
        (int 1)
      )
    )
    (expect
      (binary ==
        (index
          (index (selector matrix) (int 1))
          (int 1)
        )
        (int 5)
      )
    )
    (expect
      (binary ==
        (index
          (index (selector matrix) (int 2))
          (int 2)
        )
        (int 9)
      )
    )
    (expect
      (binary ==
        (index
          (index
            (selector matrix)
            (unary - (int 1))
          )
          (int 0)
        )
        (int 7)
      )
    )
    (expect
      (binary ==
        (index
          (index (selector matrix) (int 1))
          (unary - (int 1))
        )
        (int 60)
      )
    )
  )
)
```
#### Output
```text
matrix:  [[1 2 3] [4 5 6] [7 8 9]]
first row:  [1 2 3]
last row:  [7 8 9]
top-left:  1
center:  5
bottom-right:  9
row 1 length:  3
```
</details>

## v0.2/π.mochi

<details>
<summary>View Source, AST, and Output</summary>

#### Source
```mochi
let π = 3.14

fun area(r: float): float {
    return π * r * r
}
print(area(10.0))

test "π" {
    expect π == 3.14
    expect area(10.0) == 314.0
}

let 🍡="🍡૮₍ ˃ ⤙ ˂ ₎ა"
print(🍡)
```

#### AST
```lisp
(program
  (let "π" (float 3.14))
  (fun area
    (param r (type float))
    (type float)
    (return
      (binary *
        (binary * (selector "π") (selector r))
        (selector r)
      )
    )
  )
  (call print
    (call area (float 10))
  )
  (test "π"
    (expect
      (binary == (selector "π") (float 3.14))
    )
    (expect
      (binary ==
        (call area (float 10))
        (float 314)
      )
    )
  )
  (let "🍡" (string "🍡૮₍ ˃ ⤙ ˂ ₎ა"))
  (call print (selector "🍡"))
)
```
#### Output
```text
314
🍡૮₍ ˃ ⤙ ˂ ₎ა
```
</details>
