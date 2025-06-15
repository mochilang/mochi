# Mochi Programming Language Specification (v0.6.1)

This document describes the **Mochi programming language**. It is inspired by the structure of the [Go language specification](https://golang.org/ref/spec) and aims to formally define the syntax and semantics of Mochi.

## 0. Introduction

Mochi is a statically typed, expression-oriented language designed for simplicity, safety, and clarity. Programs are composed of statements executed in order. Mochi supports functions, first-class closures, immutable bindings, built-in testing, and agent-oriented constructs.

A Mochi program is stored in a UTF-8 encoded text file and executed top to bottom. There is no required `main` function; execution begins with the first statement. The reference implementation can compile Mochi sources to native binaries or generate Go, TypeScript, and Python code.

The following sections cover lexical structure, types, expressions, statements, functions, built-ins, and the runtime environment. An appendix summarizes the grammar in EBNF form.

## 1. Program Structure

A Mochi program consists of a sequence of statements. Statements may declare values, define functions, perform control flow, or respond to events. The general form of a program is:

```ebnf
Program = [ "package" Identifier ] { Statement }.
```

Statements are executed in the order they appear. Top-level statements share a single global scope.

If a file begins with `package name`, it belongs to that package along with any
other files in the same directory. Package declarations must precede all other
statements.

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
if   else  while
for  in   break  continue  package  export
import extern type model agent intent on stream emit as
test expect generate match fetch load save
```

### Operators and Delimiters

Operators perform arithmetic, comparison, logical, and membership operations. Delimiters structure expressions and statements.

```
+  -  *  /  %         arithmetic
== != < <= > >=       comparison
&& ||                 logical
! -                   unary
=                     assignment
in                    membership
union [all] except intersect  list operators
..                    range
( ) [ ] { } , : => .  delimiters
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

### Type Declarations

User-defined types are introduced with the `type` keyword. A type may define a set of
named fields forming a struct. An optional `=` may appear before the struct block.
Methods may also be declared inside the type block.

```mochi
type Person {
  name: string
  age: int

  fun greet(): string {
    return "hello " + name
  }
}
```

Types can alternatively declare variants separated by `|` to form sum types:

```mochi
type Result = Ok(value: int) | Err(message: string)
```

Field syntax for variants mirrors struct fields and may use either parentheses or braces.

Instances of a type are created using struct literal syntax:

```mochi
let user = Person { name: "Ada", age: 42 }
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
PostfixExpr = Primary { CallOp | IndexOp | CastOp }
Primary     = FunExpr | CallExpr | SelectorExpr | StructLiteral | ListLiteral |
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

The `in` operator tests membership in a collection:

```mochi
expect 2 in nums
```

Maps use braces with `key: value` pairs and share the same indexing syntax.

```mochi
let scores = {"alice": 1, "bob": 2}
print(scores["alice"])
```

List unions merge elements from two lists. The `union` operator removes
duplicates while `union all` preserves them:

```mochi
let merged = [1, 2, 3] union [3, 4]
let both   = [1, 2] union all [2, 3]
```

Strings are also indexable and iterable:

```mochi
let text = "hello"
print(text[1])

for ch in text {
  print(ch)
}
```

String comparison operators (`<`, `<=`, `>`, `>=`) compare values
lexicographically using Unicode code point order. For example:

```mochi
print("a" < "b")  // true
print("cat" >= "car")  // true
```

#### Struct Literals

Values of user-defined types are constructed with struct literal syntax. The field
order is irrelevant and missing fields default to `null` if allowed by the type.

```mochi
let p = Person { name: "Bob", age: 30 }
```

#### Type Casts

Use `as` to cast a value to a specific type. Casting validates that the runtime
value conforms to the target type and returns the converted value.

```mochi
let todo = fetch "https://example.com/todo" as Todo
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
              IfStmt | WhileStmt | ForStmt | BreakStmt | ContinueStmt |
              EmitStmt | ExprStmt | TestBlock | ExpectStmt |
              StreamDecl | OnHandler | ModelDecl | TypeDecl | AgentDecl .
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
Indexed assignments require the map to be declared with `var`.

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

### While Statement

Repeatedly execute a block while a condition is true:

```mochi
var j = 0
while j < 3 {
  print(j)
  j = j + 1
}
```

### Break and Continue

`break` exits the loop immediately, while `continue` skips to the next iteration.

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

### Load Expression

`load` reads a CSV or JSONL file and returns a list of records. Provide the
expected type with `as`. Additional options such as the file format may be
given with `with`.

```mochi
type Person {
  name: string
  age: int
}

let people = load "people.csv" as Person
```

### Dataset Queries

`from` expressions iterate over lists with optional filtering, joining,
grouping, sorting, skipping and limiting before projecting the result.

```mochi
let people = [
  { name: "Alice", age: 30 },
  { name: "Bob", age: 15 }
]

let stats = from p in people
            where p.age >= 18
            group by p.city into g
            sort by len(g)
            skip 1
            take 10
            select { city: g.key, count: len(g) }
```

Datasets can also be joined to combine records from multiple sources:

```mochi
let result = from o in orders
             join from c in customers on o.customerId == c.id
             select {
               orderId: o.id,
               customerName: c.name,
               total: o.total
             }
```

Query results may also be combined with `union` or `union all` to merge
lists produced by separate queries:

```mochi
let deduped  = (from x in listA select x)
               union
               (from y in listB select y)

let appended = (from x in listA select x)
               union all
               (from y in listB select y)
```

An expression may contain multiple `from` clauses. Each additional clause
performs a **cross join**, producing the Cartesian product of the sources
before any filters or joins are applied.

A plain `join` is an **inner join**, keeping only pairs of rows that satisfy
the `on` condition. Prefixing the clause with `left` or `right` yields a
**left join** or **right join** respectively. These joins preserve all rows
from the indicated side and use `null` when no matching partner exists.

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
Program       = [ "package" Identifier ] { Statement }.
Statement     = ImportStmt | LetStmt | VarStmt | AssignStmt | FunDecl | ReturnStmt |
                IfStmt | WhileStmt | ForStmt | BreakStmt | ContinueStmt |
                EmitStmt | ExprStmt | TestBlock | ExpectStmt |
                StreamDecl | OnHandler | ModelDecl | TypeDecl | AgentDecl .
LetStmt       = "let" Identifier [ ":" TypeRef ] [ "=" Expression ] .
VarStmt       = "var" Identifier [ ":" TypeRef ] [ "=" Expression ] .
AssignStmt    = PostfixExpr "=" Expression .
FunDecl       = "fun" Identifier "(" [ ParamList ] ")" [ ":" TypeRef ] Block .
ReturnStmt    = "return" Expression .
IfStmt        = "if" Expression Block [ "else" (IfStmt | Block) ] .
WhileStmt     = "while" Expression Block .
ForStmt       = "for" Identifier "in" Expression [ ".." Expression ] Block .
BreakStmt     = "break" .
ContinueStmt  = "continue" .
ImportStmt    = "import" [ Identifier ] StringLiteral [ "as" Identifier ] .
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
TypeDecl      = "type" Identifier [ [ "=" ] "{" TypeMember* "}" ] [ "=" TypeVariant { "|" TypeVariant } ] .
TypeMember    = TypeField | FunDecl .
TypeVariant   = Identifier [ "(" TypeField { "," TypeField } [ "," ]? ")" | "{" TypeField* "}" ] .
TypeField     = Identifier ":" TypeRef .

Expression    = OrExpr .
OrExpr       = AndExpr { "||" AndExpr } .
AndExpr      = Equality { "&&" Equality } .
Equality     = Comparison { ("==" | "!=") Comparison } .
Comparison   = Term { ("<" | "<=" | ">" | ">=") Term } .
Term         = Factor { ("+" | "-") Factor } .
Factor       = Unary { ("*" | "/") Unary } .
Unary        = { "-" | "!" } PostfixExpr .
PostfixExpr  = Primary { CallOp | IndexOp | CastOp } .
Primary      = FunExpr | CallExpr | SelectorExpr | StructLiteral | ListLiteral | MapLiteral | MatchExpr | GenerateExpr | FetchExpr | LoadExpr | QueryExpr | Literal | Identifier | "(" Expression ")" .
FunExpr       = "fun" "(" [ ParamList ] ")" [ ":" TypeRef ] ("=>" Expression | Block) .
CallExpr      = Identifier "(" [ Expression { "," Expression } ] ")" .
SelectorExpr  = Identifier { "." Identifier } .
ListLiteral   = "[" [ Expression { "," Expression } [ "," ] ] "]" .
MapLiteral    = "{" [ MapEntry { "," MapEntry } ] [ "," ] "}" .
MapEntry      = Expression ":" Expression .
IndexOp       = "[" [ Expression ] [ ":" Expression ] "]" .
CallOp        = "(" [ Expression { "," Expression } ] ")" .
CastOp        = "as" TypeRef .
LoadExpr      = "load" StringLiteral "as" TypeRef [ "with" Expression ] .
QueryExpr     = "from" Identifier "in" Expression
                { "from" Identifier "in" Expression }
                { [ "left" | "right" | "outer" ] "join" [ "from" ] Identifier "in" Expression "on" Expression }
                [ "where" Expression ]
                [ GroupByClause ]
                [ "sort" "by" Expression ]
                [ "skip" Expression ]
                [ "take" Expression ]
                "select" Expression .
GroupByClause = "group" "by" Expression "into" Identifier .
StructLiteral = Identifier "{" [ StructField { "," StructField } ] [ "," ] "}" .
StructField   = Identifier ":" Expression .
ParamList     = Param { "," Param } .
Param         = Identifier [ ":" TypeRef ] .
TypeRef       = FunType | GenericType | Identifier .
GenericType   = Identifier "<" TypeRef { "," TypeRef } ">" .
FunType       = "fun" "(" [ TypeRef { "," TypeRef } ] ")" [ ":" TypeRef ] .
```
