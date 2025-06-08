# Mochi Programming Language Specification (v0.3.5)

This document describes version 0.3.5 of the **Mochi programming language**. It is inspired by the structure of the [Go language specification](https://golang.org/ref/spec) and aims to formally define the syntax and semantics of Mochi.

## 0. Introduction

Mochi is a statically typed, expression-oriented language designed for simplicity, safety, and clarity. Programs are composed of statements executed in order. Mochi supports functions, first-class closures, immutable bindings, built-in testing, and agent-oriented constructs.

A Mochi program is stored in a UTF-8 encoded text file and executed top to bottom. There is no required `main` function; execution begins with the first statement. The reference implementation can compile Mochi sources to native binaries or generate Go, TypeScript, and Python code.

The following sections cover lexical structure, types, expressions, statements, functions, built-ins, and the runtime environment. An appendix summarizes the grammar in EBNF form.

## 1. Program Structure

A Mochi program consists of a sequence of statements. Statements may declare values, define functions, perform control flow, or respond to events. The general form of a program is:

```ebnf
Program = { Statement }.
```

Statements are executed in the order they appear. Top-level statements share a single global scope.

### Blocks

Blocks are sequences of statements enclosed in braces and introduce a new lexical scope:

```ebnf
Block = "{" { Statement } "}".
```

Blocks are used in function bodies, control flow constructs, tests, streams, and agents.

## 2. Lexical Elements

### Characters

Source files are UTF-8. Identifiers and string literals may contain any Unicode characters. The language treats ASCII letters, digits, and the underscore as special for identifiers.

### Tokens

The scanner breaks the source into the following token categories:

* identifiers
* keywords
* operators and delimiters
* literals (integers, floats, strings, booleans)
* comments

Whitespace is ignored except as a separator.

### Identifiers

Identifiers begin with a letter, underscore, or Unicode symbol. Subsequent characters may be letters, digits, underscores, or additional Unicode symbols. Identifiers are case sensitive.

Examples of valid identifiers:

```mochi
foo
π
🍣
result42
```

### Keywords

The following keywords are reserved:

```
let  var  fun  return
if   else
for  in
stream  emit  on  as
model  agent  test  expect
```

### Operators and Delimiters

Operators perform arithmetic and comparisons. Delimiters structure expressions and statements.

```
+  -  *  /            arithmetic
== != < <= > >=       comparison
! -                  unary
=                    assignment
..                   range
( ) { } , : => .      delimiters
```

### Literals

Mochi supports integer, floating point, string, and boolean literals. Strings use double quotes and support basic escape sequences (`\n`, `\t`, `\"`, `\\`).

### Comments

A comment begins with `//` and continues to the end of the line. Block comments using `/*` and `*/` are also supported.

## 3. Types

Mochi is statically typed. The built-in primitive types are `int`, `float`, `bool`, `string`, and `null`. Function values have type `fun` with a parameter and result signature.

Type annotations may appear in variable declarations and function signatures. When omitted in `let` declarations, the type is inferred from the initializer.

Examples:

```mochi
let x: int = 3
let y = 4.2        // inferred float
fun add(a: int, b: int): int { return a + b }
```

Function types use the syntax:

```mochi
fun(T1, T2): R
```

where `T1`, `T2`, and `R` are type references.

Generic container types use angle brackets. `list<T>` denotes a list of `T` and
`map<K, V>` denotes a mapping from keys of type `K` to values of type `V`.
For example:

```mochi
let scores: map<string, int> = {"alice": 1}
```

## 4. Expressions

Expressions compute values. The grammar defines expressions in precedence order.

```ebnf
Expression  = OrExpr
OrExpr      = AndExpr { "||" AndExpr }
AndExpr     = Equality { "&&" Equality }
Equality    = Comparison { ("==" | "!=") Comparison }
Comparison  = Term { ("<" | "<=" | ">" | ">=") Term }
Term        = Factor { ("+" | "-") Factor }
Factor      = Unary { ("*" | "/") Unary }
Unary       = { "-" | "!" } PostfixExpr
PostfixExpr = Primary { IndexOp }
Primary     = FunExpr | CallExpr | SelectorExpr | ListLiteral |
              MapLiteral | MatchExpr | GenerateExpr | FetchExpr |
              Literal | Identifier | "(" Expression ")"
```

#### Function Expressions

Functions may be expressed inline using `fun`:

```mochi
fun(x: int): int => x * x
fun(x: int): int {
  return x * x
}
```

#### Calls and Selectors

Function calls use parentheses. Selector expressions access fields using dot notation.

```mochi
print("hi")
event.payload.id
```

#### Lists and Maps

Lists use square brackets. Elements are accessed by index or slice.

```mochi
let nums = [1, 2, 3]
print(nums[0])
print(nums[1:3])
```

Maps use braces with `key: value` pairs and share the same indexing syntax.

```mochi
let scores = {"alice": 1, "bob": 2}
print(scores["alice"])
```

#### Match Expressions

`match` evaluates patterns in order and yields the corresponding result. `_` matches any value.

```mochi
let label = match x {
  1 => "one"
  2 => "two"
  _ => "other"
}
```

## 5. Statements

Statements control execution and may declare bindings, functions, or agents.

```ebnf
Statement   = LetStmt | VarStmt | AssignStmt | FunDecl | ReturnStmt |
              IfStmt | ForStmt | EmitStmt | ExprStmt | TestBlock |
              ExpectStmt | StreamDecl | OnHandler | AgentDecl .
```

### Let Statement

`let` introduces an immutable binding:

```mochi
let name = "Mochi"
```

### Var Statement

`var` introduces a mutable binding:

```mochi
var counter = 0
counter = counter + 1
```

### Assignment

Assignments update an existing binding or indexed element:

```mochi
x = x + 1
scores["alice"] = 10
```

### Function Declaration

Named functions bind an identifier to a function value:

```mochi
fun square(x: int): int {
  return x * x
}
```

### If Statement

Conditional execution based on a boolean expression:

```mochi
if x > 0 {
  print("positive")
} else {
  print("non-positive")
}
```

### For Statement

Two forms of `for` loops are supported:

```mochi
for i in 0..5 { print(i) }            // range loop
for item in items { print(item) }      // collection loop
```

### Emit Statement

`emit` sends an event to a named stream using struct literal syntax:

```mochi
emit Sensor { id: "s1", temperature: 21.3 }
```

### Test and Expect

`test` blocks group expectations. `expect` asserts a condition is true.

```mochi
test "math" {
  expect 1 + 2 == 3
}
```

### Streams and Agents

Streams declare named event types. Use `emit` to send events. Agents define event handlers with `on` blocks.

```mochi
stream Click { x: int, y: int }

agent Logger {
  on Click as e {
    print(e.x, e.y)
  }
}

emit Click { x: 10, y: 20 }
```

Agents may also declare persistent variables using `var` and expose `intent`
functions that can be called from other code or via an MCP server.

```mochi
agent Counter {
  var n: int = 0

  on Click as _ {
    n = n + 1
  }

  intent total(): int {
    return n
  }
}
```

### Model Declarations

`model` blocks define reusable language model aliases. Each block specifies a
`provider`, `name`, and optional parameters. Models can be referenced by name in
`generate` expressions.

```mochi
model quick {
  provider: "openai"
  name: "gpt-3.5-turbo"
}
```

### Generative Blocks

`generate` expressions invoke a language model. `generate text` returns a string,
`generate <Type>` returns a struct of the given type, and `generate embedding`
produces a `list<float>` vector. Embeddings may be normalized with the optional
`normalize` field.

```mochi
let vec = generate embedding {
  text: "hello world"
  normalize: true
}
```

### Fetch Expression

`fetch` performs an HTTP request and decodes the JSON response. If the
target variable has a known type or you cast with `as`, Mochi validates
that the JSON matches the expected structure. Additional options may be
provided with `with`.

```mochi
type Todo {
  userId: int
  id: int
  title: string
  completed: bool
}

let todo = fetch "https://example.com/todos/1" as Todo
```

```mochi
let created: Todo = fetch "https://example.com/todos" with {
  method: "POST",
  body: todo
}
```

## 6. Functions

Functions are first-class. Parameters are typed, and the return type may be omitted in block-bodied functions if `return` is used. Functions may capture variables from their enclosing scope, forming closures.

```mochi
fun makeCounter(): fun(): int {
  let count = 0
  return fun(): int {
    count = count + 1
    return count
  }
}
```

## 7. Built-in Functions

Mochi provides a small set of built-ins available in all scopes. The most common is `print`, which writes its arguments to standard output and returns `null`.

Other built-ins include `len` for obtaining the length of strings, lists, or maps; `now` which returns the current time as an integer; and `json` for printing values in JSON format.

## 8. Runtime Semantics

Mochi uses lexical scoping and a chain of environments for variable resolution. Blocks and functions create new environments that reference their parent scope. Functions capture the environment at the point of declaration.

Bindings are immutable; attempts to reassign produce a runtime error unless via explicit assignment to an indexed element.

## 9. Errors

Runtime errors occur for invalid operations, such as type mismatches, out-of-bounds indexing, or use of undeclared identifiers. Errors include a message and source location to aid debugging.

## Appendix A. Grammar Summary

The complete grammar for Mochi in EBNF notation:

```ebnf
Program       = { Statement }.
Statement     = LetStmt | VarStmt | AssignStmt | FunDecl | ReturnStmt |
                IfStmt | ForStmt | EmitStmt | ExprStmt | TestBlock |
                ExpectStmt | StreamDecl | OnHandler | ModelDecl | AgentDecl .
LetStmt       = "let" Identifier [ ":" TypeRef ] [ "=" Expression ] .
VarStmt       = "var" Identifier [ ":" TypeRef ] [ "=" Expression ] .
AssignStmt    = PostfixExpr "=" Expression .
FunDecl       = "fun" Identifier "(" [ ParamList ] ")" [ ":" TypeRef ] Block .
ReturnStmt    = "return" Expression .
IfStmt        = "if" Expression Block [ "else" (IfStmt | Block) ] .
ForStmt       = "for" Identifier "in" Expression [ ".." Expression ] Block .
EmitStmt      = "emit" Identifier MapLiteral .
ExprStmt      = Expression .
TestBlock     = "test" StringLiteral Block .
ExpectStmt    = "expect" Expression .
StreamDecl    = "stream" Identifier Block .
OnHandler     = "on" Identifier "as" Identifier Block .
AgentDecl     = "agent" Identifier "{" AgentField* "}" .
AgentField    = LetStmt | VarStmt | AssignStmt | OnHandler | IntentDecl .
IntentDecl    = "intent" Identifier "(" [ ParamList ] ")" [ ":" TypeRef ] Block .
ModelDecl     = "model" Identifier Block .

Expression    = OrExpr .
OrExpr       = AndExpr { "||" AndExpr } .
AndExpr      = Equality { "&&" Equality } .
Equality     = Comparison { ("==" | "!=") Comparison } .
Comparison   = Term { ("<" | "<=" | ">" | ">=") Term } .
Term         = Factor { ("+" | "-") Factor } .
Factor       = Unary { ("*" | "/") Unary } .
Unary        = { "-" | "!" } PostfixExpr .
PostfixExpr  = Primary { IndexOp } .
Primary      = FunExpr | CallExpr | SelectorExpr | ListLiteral | MapLiteral | MatchExpr | GenerateExpr | FetchExpr | Literal | Identifier | "(" Expression ")" .
FunExpr       = "fun" "(" [ ParamList ] ")" [ ":" TypeRef ] ("=>" Expression | Block) .
CallExpr      = Identifier "(" [ Expression { "," Expression } ] ")" .
SelectorExpr  = Identifier { "." Identifier } .
ListLiteral   = "[" [ Expression { "," Expression } [ "," ] ] "]" .
MapLiteral    = "{" [ MapEntry { "," MapEntry } ] [ "," ] "}" .
MapEntry      = Expression ":" Expression .
IndexOp       = "[" [ Expression ] [ ":" Expression ] "]" .
ParamList     = Param { "," Param } .
Param         = Identifier [ ":" TypeRef ] .
TypeRef       = FunType | GenericType | Identifier .
GenericType   = Identifier "<" TypeRef { "," TypeRef } ">" .
FunType       = "fun" "(" [ TypeRef { "," TypeRef } ] ")" [ ":" TypeRef ] .
```

This specification outlines the core language as of version 0.3.5. Future versions may introduce modules, user-defined types, pattern matching, and asynchronous operations while preserving backward compatibility.
