# Mochi Language Specification (v0.1.12)

# 0. Introduction

This document describes the syntax and semantics of the **Mochi programming language**, version 0.1.12.

Mochi is a statically typed, expression-oriented language designed for simplicity, safety, and expressiveness. It
supports imperative programming with lexical scoping and closures, as well as functional constructs such as first-class
functions, immutable bindings, and partial application.

A Mochi program consists of a sequence of **statements** executed from top to bottom. Functions are defined using the
`fun` keyword and may be declared at the top level or nested within other blocks. Mochi does not require a `main`
function or any predefined entry point.

The language emphasizes clarity and predictability. Every expression has a well-defined type and evaluates to a value.
Variables are declared using `let` and are immutable by default. Control flow is expressed using `if`, `for`, and
`return`.

Mochi provides built-in support for testing (`test`, `expect`), stream-based event handling (`stream`, `on`), and
agent-oriented programming (`agent`). These constructs are syntactically lightweight and integrated into the core
language.

The specification is organized into sections that cover the lexical structure, types, expressions, statements,
functions, and runtime behavior of Mochi. The formal syntax is written using Extended Backus–Naur Form (EBNF). Examples
are provided for illustration, but they are not part of the formal specification unless explicitly stated.

This specification describes version 0.1.12 of the language. Future versions may add support for modules, user-defined
types, pattern matching, and asynchronous operations. Additions will be made conservatively and without breaking
existing programs.

# 1. Program Structure

A Mochi program is a single source file encoded in UTF-8. The file contains a sequence of **statements**, which are
evaluated in the order they appear. There is no `main` function or special entry point — the program begins execution
from the top of the file and proceeds to the bottom.

## Declarations and Statements

Statements define the structure and behavior of a program. They include declarations of variables and functions,
expression evaluations, control flow, and specialized forms such as test blocks and event handlers.

Each statement is terminated implicitly by its syntactic form. Statements may appear at the top level of a program or
within blocks introduced by other statements.

## Blocks

A **block** is a sequence of statements enclosed in braces:

```
Block = "{" { Statement } "}" .
```

Blocks introduce a new lexical scope. Variables declared within a block are not visible outside the block.

Blocks are used in function bodies, control flow branches (`if`, `else`, `for`), test blocks, and agent handlers.

## Top-Level Form

At the top level, a Mochi program consists of zero or more statements:

```
Program = { Statement } .
```

Top-level statements are evaluated in order. There is no separate declaration or import phase.

## Grammar

The grammar for statements and program structure is as follows:
```ebnf
Program         = { Statement } .

Statement       = TestBlock
                | ExpectStmt
                | AgentDecl
                | StreamDecl
                | OnHandler
                | LetStmt
                | AssignStmt
                | FunStmt
                | ReturnStmt
                | IfStmt
                | ForStmt
                | ExprStmt .

TestBlock       = "test" StringLiteral "{" { Statement } "}" .
ExpectStmt      = "expect" Expr .
ExprStmt        = Expr .

Expr            = Equality .
Equality        = Comparison { EqualOp } .
Comparison      = Term { CompOp } .
Term            = Factor { TermOp } .
Factor          = Unary { FactorOp } .
Unary           = { "-" | "!" } PostfixExpr .

PostfixExpr     = Primary { IndexOp } .

Primary         = FunExpr
                | CallExpr
                | SelectorExpr
                | ListLiteral
                | Literal
                | "(" Expr ")" .

FunExpr         = "fun" "(" [ ParamList ] ")" [ ":" TypeRef ] "=>" Expr
                | "fun" "(" [ ParamList ] ")" [ ":" TypeRef ] "{" { Statement } "}" .

ParamList       = Param { "," Param } .
Param           = Identifier [ ":" TypeRef ] .

TypeRef         = FunType | Identifier .
FunType         = "fun" "(" [ TypeRef { "," TypeRef } ] ")" [ ":" TypeRef ] .

CallExpr        = Identifier "(" [ Expr { "," Expr } ] ")" .
SelectorExpr    = Identifier { "." Identifier } .

ListLiteral     = "[" [ Expr { "," Expr } [ "," ] ] "]" .

Literal         = FloatLiteral | IntLiteral | BooleanLiteral | StringLiteral .

IndexOp         = "[" [ Expr ] [ ":" Expr ] "]" .

EqualOp         = ("==" | "!=") Comparison .
CompOp          = ("<" | "<=" | ">" | ">=") Term .
TermOp          = ("+" | "-") Factor .
FactorOp        = ("*" | "/") Unary .
```

Each form of statement is defined in more detail in subsequent sections.

# 2. Lexical Elements

Mochi programs are written using Unicode characters and are encoded in UTF-8. The lexical structure of a program is
defined in terms of **tokens**: the smallest meaningful units of syntax.

Tokens include **identifiers**, **keywords**, **operators and delimiters**, **literals**, and **comments**. Whitespace
and line breaks are used to separate tokens but are otherwise ignored by the parser.

## 2.1 Characters

The source character set is Unicode. Mochi supports full Unicode in string literals and identifiers (including emojis). 

A **letter** is any of the ASCII letters `A–Z`, `a–z`, or the underscore `_`.

A **digit** is any of the ASCII digits `0–9`.

## 2.2 Tokens

Tokens are formed from characters without intervening whitespace. The scanner divides the input into the following
categories:

* **Identifiers**
* **Keywords**
* **Operators and delimiters**
* **Literals** (integers, floats, strings, booleans)
* **Comments**

Whitespace (spaces, tabs, carriage returns, and newlines) is ignored except as a separator between tokens.

### Identifiers

Identifiers are names used to refer to variables, parameters, functions, streams, agents, and other user-defined values.

```
Identifier = letter { letter | digit | "_" | UnicodeSymbol } .
```

An identifier must begin with a letter, underscore (`_`), or a Unicode character (e.g., emoji). Subsequent characters may include letters, digits, underscores, or additional Unicode symbols.

Identifiers are **case-sensitive** and must not conflict with reserved keywords.

These are all valid and distinct:

```
foo
Foo
FOO
```

#### ✅ Valid identifiers

```
x
result
_myVar
count42
π
🍣
名前
data_💡
```

#### ❌ Invalid identifiers

```
2fast      // starts with a digit
x-y        // contains invalid character '-'
fun        // reserved keyword
"hello"    // string literal, not an identifier
```

## 2.4 Keywords

The following keywords are reserved and may not be used as identifiers:

```
let      fun      return
if       else
for      in
test     expect
stream   on       as
agent
```

All keywords are lowercase. Attempting to use a keyword as an identifier results in a syntax error.

## 2.5 Operators and Delimiters

Operators combine values, perform comparisons, and express control. Delimiters structure program elements.

### Arithmetic operators

```
+    addition
-    subtraction
*    multiplication
/    division
```

### Comparison operators

```
==   equal
!=   not equal
<    less than
<=   less than or equal
>    greater than
>=   greater than or equal
```

### Unary operators

```
-    numeric negation
!    logical NOT
```

### Assignment operator

```
=    value assignment
```

### Range operator

```
..   half-open integer range: start..end (end excluded)
```

### Delimiters

```
( )    parentheses – grouping, function calls
{ }    braces – blocks
,      comma – separates parameters and arguments
:      colon – type annotations
=>     fat arrow – expression-bodied function
.      dot – selector expressions
```

## 2.6 Literals

Literals represent constant values written directly in the source code.

### Integer literals

A sequence of decimal digits.

```
IntLiteral = digit { digit } .
```

Examples:

```
0
123
999999
```

Negative integers are formed using the unary `-` operator.

### Floating-point literals

A decimal number with a decimal point and at least one digit on each side.

```
FloatLiteral = IntLiteral "." IntLiteral .
```

Examples:

```
3.14
0.0
42.0
```

Scientific notation (e.g., `1e6`) is not supported in this version.

### String literals

A string literal is a sequence of characters enclosed in double quotes.

```
StringLiteral = `"` { character | escape } `"` .
escape        = "\\" character .
```

Escape sequences include:

* `\\` – backslash
* `\"` – double quote
* `\n` – newline
* `\t` – tab

Examples:

```mochi
"hello"
"line\nbreak"
"quote: \"text\""
```

Multiline strings are not supported.

### Boolean literals

Boolean literals are:

```
true
false
```

They may be used in expressions and conditions.

### Null

Mochi has a `null` value, but no literal syntax for it. It is returned implicitly from functions with no return value.

## 2.7 Comments

A comment begins with `//` and continues to the end of the line. Comments are ignored by the parser.

```mochi
// This is a comment
let x = 42  // Inline comment
```

Block comments (`/* ... */`) are not supported in version 0.1.12.

# 3. Types

Every value in Mochi has a **type** that determines the operations permitted on it. The language uses **static typing**
with optional annotations and limited **type inference**. Type checks are performed both at parse time and at runtime.

There are four built-in primitive types and one function type. More complex types such as structs, enums, or generics
may be introduced in future versions.

## 3.1 Type Syntax

Type expressions appear in variable declarations, function parameters, and return types. They follow the grammar:

```
Type        = SimpleType | FuncType .
SimpleType  = Identifier .
FuncType    = "fun" "(" [ TypeList ] ")" [ ":" Type ] .
TypeList    = Type { "," Type } .
```

### Examples

```
int
float
bool
string
fun(int): int
fun(string, int): bool
```

## 3.2 Predeclared Types

The following simple types are predeclared and built into the language:

| Type     | Description                         | Example         |
|----------|-------------------------------------|-----------------|
| `int`    | 64-bit signed integer               | `42`, `-7`      |
| `float`  | 64-bit IEEE-754 floating-point      | `3.14`, `-0.25` |
| `bool`   | Boolean value                       | `true`, `false` |
| `string` | UTF-8 encoded immutable string      | `"hello"`       |
| `null`   | Special sentinel value (no literal) | *(implicit)*    |

The `null` value is not typed and is returned from functions without a `return` statement or from built-ins like
`print`.

## 3.3 Function Types

Function types describe callable values. A function type consists of zero or more input types and an optional output
type:

```
fun(<param types>): <result type>
```

### Examples

```mochi
fun(): int
fun(int): bool
fun(int, int): int
fun(string): string
```

Function types may be used:

* In type annotations for variables or parameters
* As the return type of a function that returns a function
* To describe closures

## 3.4 Type Annotations

Type annotations may be provided in variable declarations and function signatures.

```mochi
let name: string = "Mochi"
fun add(a: int, b: int): int {
  return a + b
}
```

* In `let` declarations, the type may be omitted if an initializer is present.
* In `fun` declarations, parameter types are required.
* Return types are optional in block-bodied functions but required in expression-bodied functions (`=>`).

## 3.5 Type Inference

Mochi supports type inference in `let` declarations when the type is obvious from the initializer.

```mochi
let x = 3         // inferred as int
let y = "hello"   // inferred as string
let z = true      // inferred as bool
```

Function parameters and expression-bodied functions do **not** support type inference. The return type of a block-bodied
function may be omitted if `return` is used explicitly.

## 3.6 Type Checking

Types are checked statically and dynamically:

* Static checks ensure function signatures and declarations are consistent.
* Runtime checks validate operations on values.

There are no implicit type conversions in Mochi. Operations on mismatched types result in runtime errors.

```mochi
1 + "x"     // ❌ error: incompatible types (int + string)
```

## 3.7 Equality and Comparison

Equality (`==`, `!=`) and ordering (`<`, `>`, `<=`, `>=`) are defined for specific types.

| Type     | `==`, `!=`         | `<`, `>` | Notes                        |
|----------|--------------------|----------|------------------------------|
| `int`    | ✅                  | ✅        | Standard numeric comparison  |
| `float`  | ✅                  | ✅        | IEEE 754 rules               |
| `bool`   | ✅                  | ❌        | Only equality allowed        |
| `string` | ✅                  | ❌        | Lexical ordering TBD         |
| `func`   | ❌                  | ❌        | Functions are not comparable |
| `null`   | ✅ (`null == null`) | ❌        | Identity only                |

Comparing values of incompatible types results in a runtime error.

# 4. Expressions

Expressions in Mochi compute values. They may refer to variables, perform arithmetic or logical operations, call
functions, construct closures, or evaluate conditionally. Every expression has a type and produces a value when
evaluated.

Expressions may appear in:

* Variable initializers
* Function return statements
* Function arguments
* Control flow conditions (`if`, `for`)
* Expression statements

## 4.1 Expression Grammar

The grammar defines expressions in terms of precedence and associativity:

```
Expression      = Equality .
Equality        = Comparison { ("==" | "!=") Comparison } .
Comparison      = Term { ("<" | "<=" | ">" | ">=") Term } .
Term            = Factor { ("+" | "-") Factor } .
Factor          = Unary { ("*" | "/") Unary } .
Unary           = { "-" | "!" } Primary .

Primary         = Literal
                | Identifier
                | FunctionExpr
                | CallExpr
                | SelectorExpr
                | "(" Expression ")" .

FunctionExpr    = "fun" "(" [ ParamList ] ")" [ ":" Type ] ( "=>" Expression | Block ) .
CallExpr        = Identifier "(" [ ArgList ] ")" .
SelectorExpr    = Identifier "." Identifier { "." Identifier } .
ArgList         = Expression { "," Expression } .
```

All binary operators are left-associative.

## 4.2 Literal Expressions

Literals evaluate directly to their corresponding values:

```mochi
42          // int
3.14        // float
"hello"     // string
true        // bool
```

Literals are discussed in more detail in Section 2.6.

## 4.3 Identifiers

An identifier refers to a variable or function previously declared in the current or enclosing scope:

```mochi
let x = 5
x + 3        // evaluates to 8
```

Using an undeclared identifier results in a runtime error.

## 4.4 Unary Operators

Unary expressions apply a single-operand prefix operator:

```
Unary = { "-" | "!" } Primary .
```

* `-x` negates a numeric value
* `!x` performs boolean negation

The operand must be of the appropriate type:

```mochi
let a = -3        // valid
let b = !true     // valid
let c = !"no"     // ❌ error: expected bool
```

## 4.5 Binary Operators

Binary expressions use infix notation:

```mochi
a + b
x * y
score == 100
```

Operands must be of compatible types. Errors are raised at runtime if types are incompatible.

## 4.6 Parenthesized Expressions

Parentheses can be used to group expressions and override precedence:

```mochi
(1 + 2) * 3   // evaluates to 9
```

Parentheses do not create new scopes.

## 4.7 Function Expressions

A function expression evaluates to a **function value**, also known as a **closure**. Function values are first-class
and may be assigned to variables, passed as arguments, or returned from other functions.

Mochi supports two forms:

### Expression-body form

```mochi
fun(x: int): int => x * x
```

* The return type is required.
* The body is a single expression.

### Block-body form

```mochi
fun(x: int): int {
  return x * x
}
```

* May include multiple statements.
* The return type may be omitted if `return` is used.

Function expressions may capture local variables from their enclosing environment.

## 4.8 Function Calls

A function call applies arguments to a callable value:

```mochi
add(1, 2)
square(5)
```

Arguments are evaluated from left to right. The number and type of arguments must match the function’s signature.

Function values may be called directly, or through identifiers that refer to closures:

```mochi
let f = fun(x: int): int => x + 1
f(3)   // evaluates to 4
```

## 4.9 Selector Expressions

A selector expression accesses a nested value or field:

```mochi
event.payload.temp
```

Each selector accesses a field of the previous value. The exact semantics depend on the runtime structure (e.g. maps,
structs, or event values).

## 4.10 Currying and Partial Application

Mochi supports manual currying using closures:

```mochi
fun add(a: int): fun(int): int {
  return fun(b: int): int => a + b
}
```

This can be called in steps:

```mochi
let add2 = add(2)
add2(3)     // evaluates to 5
```

Or immediately:

```mochi
add(1)(2)   // evaluates to 3
```

Each partially applied call returns a new function value that captures the previous arguments.

# 5. Statements

Statements define the control flow and structure of a Mochi program. They perform actions such as declaring variables,
executing expressions, defining functions, and controlling branching or iteration.

Unlike expressions, which evaluate to values, statements are evaluated for their **effects**. Statements are executed in
source order unless altered by control constructs such as `if`, `for`, or `return`.

Statements may appear at the top level of a program or within blocks.

## 5.1 Grammar

```
Statement       = LetStmt
                | AssignStmt
                | FunDecl
                | ReturnStmt
                | IfStmt
                | ForStmt
                | ExprStmt
                | TestBlock
                | ExpectStmt
                | StreamDecl
                | OnHandler
                | AgentDecl .

LetStmt         = "let" Identifier [ ":" Type ] [ "=" Expression ] .
AssignStmt      = Identifier "=" Expression .
FunDecl         = "fun" Identifier "(" [ ParamList ] ")" [ ":" Type ] Block .
ReturnStmt      = "return" Expression .
IfStmt          = "if" Expression Block [ "else" (IfStmt | Block) ] .
ForStmt         = "for" Identifier "in" Expression ".." Expression Block .
ExprStmt        = Expression .
TestBlock       = "test" StringLiteral Block .
ExpectStmt      = "expect" Expression .
StreamDecl      = "stream" Identifier Block .
OnHandler       = "on" Identifier "as" Identifier Block .
AgentDecl       = "agent" Identifier Block .
```

Each statement is described in detail below.

## 5.2 Let Statement

A `let` statement declares a new variable binding:

```mochi
let x = 42
let name: string = "Mochi"
```

* The type annotation is optional if an initializer is provided.
* The value is evaluated and stored in the current scope.
* Variables are **immutable** and may not be reassigned.

## 5.3 Assignment Statement

An assignment updates the value of an existing variable:

```mochi
x = x + 1
```

* The variable must already exist in the current or enclosing scope.
* Assignments do **not** create new bindings.
* Type compatibility is checked at runtime.

## 5.4 Function Declaration

A named function is defined with `fun`:

```mochi
fun add(a: int, b: int): int {
  return a + b
}
```

* Parameters must be typed.
* The return type is optional for block bodies.
* The body is a block that may contain any statements.
* Functions are hoisted: they may be called before they are defined.

Function declarations bind a name in the current scope.

## 5.5 Return Statement

A `return` statement exits a function with a value:

```mochi
return x * x
```

* May only appear inside function bodies.
* The return value is required.
* A function without an explicit `return` returns `null`.

## 5.6 If Statement

An `if` statement conditionally executes one of two blocks:

```mochi
if x > 0 {
  print("positive")
} else {
  print("non-positive")
}
```

* The condition must evaluate to a `bool`.
* The `else` branch is optional.
* `else if` is a syntactic shorthand for `else { if ... }`.

Both branches must be blocks.

## 5.7 For Statement

A `for` loop iterates over a half-open range:

```mochi
for i in 0..5 {
  print(i)
}
```

* The start and end expressions must evaluate to `int`.
* The loop variable is bound within the block scope.
* The range includes the start value and excludes the end.

Other loop forms (e.g., `while`, `loop`) are not currently supported.

## 5.8 Expression Statement

An expression may appear as a standalone statement:

```mochi
print("Done")
```

* The value of the expression is computed and discarded.
* Commonly used for function calls and side effects.

## 5.9 Block

A block is a sequence of statements enclosed in braces:

```mochi
{
  let a = 1
  print(a + 1)
}
```

* Blocks introduce a new lexical scope.
* Used in function bodies, `if`, `for`, `test`, `agent`, and `on` handlers.

## 5.10 Test Block

A `test` block defines a named unit test:

```mochi
test "basic math" {
  expect 1 + 2 == 3
}
```

* The string literal gives the test name.
* The block contains `expect` statements and other code.
* If any `expect` fails, the test is considered failed.
* Tests are executed during normal program evaluation.

## 5.11 Expect Statement

An `expect` statement asserts that an expression is `true`:

```mochi
expect x > 0
```

* If the expression evaluates to `false`, a test failure is raised.
* Only allowed inside `test` blocks.

## 5.12 Stream Declaration

A `stream` defines a named data stream:

```mochi
stream myStream {
  // configuration or events
}
```

* The body is a block that may be empty or include handlers.
* The stream is bound as a top-level value.

The runtime behavior of streams is defined in the event processing model.

## 5.13 Handler Declaration

An `on` handler binds a named event to a block of logic:

```mochi
on myStream as event {
  print(event.data)
}
```

* The event binding is an identifier.
* The block is executed whenever an event is received.

Only valid within agents.

## 5.14 Agent Declaration

An `agent` defines a stateful event processor:

```mochi
agent Counter {
  on myStream as e {
    print(e.value)
  }
}
```

* Contains one or more `on` handlers.
* May maintain internal state using `let` bindings.
* Executed in response to external or simulated events.

# 6. Functions

Functions are fundamental building blocks in Mochi. They define reusable computations, may accept parameters and return
results, and can be assigned to variables, passed as arguments, or returned from other functions.

Functions are **first-class values** and support lexical scoping and closures.

## 6.1 Function Declarations

A **function declaration** defines a named function using the `fun` keyword:

```mochi
fun square(x: int): int {
  return x * x
}
```

* The name is bound in the current scope.
* All parameters must be explicitly typed.
* The return type may be omitted if the function body is a block and contains a `return` statement.
* The body must be a block.

Declared functions are **hoisted**, meaning they can be called before they are defined in the source.

## 6.2 Function Expressions

Functions can also be created as expressions using the same `fun` keyword:

```mochi
let square = fun(x: int): int => x * x
```

There are two forms:

### Expression-body form

```mochi
fun(x: int): int => x * x
```

* Must include a return type.
* Body is a single expression.

### Block-body form

```mochi
fun(x: int): int {
  return x * x
}
```

* May omit the return type if a `return` statement is used.
* Body may include multiple statements.

Function expressions produce a **function value** and are not hoisted. They may capture variables from their enclosing
scope, forming **closures**.

## 6.3 Parameters

Function parameters follow the grammar:

```
ParamList = Param { "," Param } .
Param     = Identifier [ ":" Type ] .
```

* All parameters in **declarations** must include a type.
* In **function expressions**, the same rules apply.
* Parameter names must be unique within the function signature.

## 6.4 Return Values

Functions may return a value using the `return` keyword:

```mochi
fun double(x: int): int {
  return x * 2
}
```

If a function reaches the end of its block without a `return`, it returns `null`.

Expression-bodied functions always return the result of the expression.

## 6.5 Closures

Functions may reference and retain access to variables from their enclosing lexical scope. Such functions are called *
*closures**:

```mochi
fun makeCounter(): fun(): int {
  let count = 0
  return fun(): int {
    count = count + 1
    return count
  }
}
```

Each invocation of `makeCounter()` produces a new function with its own captured `count`.

Closures are essential for building partial application, callbacks, and agents with state.

## 6.6 Function Values

Function values may be assigned, passed, and returned:

```mochi
let add = fun(a: int, b: int): int => a + b
let result = add(1, 2)
```

They can also be curried manually by returning nested functions:

```mochi
fun curry(a: int): fun(int): int {
  return fun(b: int): int => a + b
}
```

Calling curried functions:

```mochi
curry(1)(2)   // 3
```

## 6.7 Function Types

The type of a function is written as:

```
fun(<param types>): <return type>
```

Examples:

```mochi
fun(int): int
fun(string, int): bool
```

Function types may be used in variable declarations:

```mochi
let f: fun(int): int = fun(x: int): int => x + 1
```

Mochi uses **structural typing**: two function values are compatible if their parameter and return types match.

# 7. Control Flow

Mochi supports structured control flow using conditional statements (`if`), loops (`for`), and early returns (`return`).
These constructs are evaluated in source order and define new lexical scopes where applicable.

## 7.1 If Statements

The `if` statement conditionally executes a block of code based on a boolean expression.

### Syntax

```
IfStmt = "if" Expression Block [ "else" (IfStmt | Block) ] .
```

### Example

```mochi
if score > 80 {
  print("Excellent")
} else {
  print("Keep going")
}
```

### Rules

* The condition must evaluate to a `bool`.
* The `else` clause is optional.
* Both `if` and `else` branches must be blocks.
* `else if` is syntactic sugar for `else { if ... }`.

Chained conditions:

```mochi
if x > 0 {
  print("positive")
} else if x == 0 {
  print("zero")
} else {
  print("negative")
}
```

## 7.2 For Loops

Mochi provides a single looping construct: the **for-in-range** loop.

### Syntax

```
ForStmt = "for" Identifier "in" Expression ".." Expression Block .
```

### Example

```mochi
for i in 0..3 {
  print(i)
}
```

This loop executes with `i = 0`, `1`, `2`. The upper bound is excluded.

### Rules

* The range must consist of two `int` expressions: a start and an exclusive end.
* The loop variable is scoped to the block.
* Start and end expressions are evaluated once at the beginning of the loop.

Other forms (e.g., `while`, infinite loops) are not yet supported.

## 7.3 Return Statement

A `return` statement exits the current function and optionally returns a value.

### Syntax

```
ReturnStmt = "return" Expression .
```

### Example

```mochi
fun square(x: int): int {
  return x * x
}
```

### Rules

* `return` may only appear within a function body.
* An expression is required.
* A function that reaches the end of its block without a `return` statement returns `null`.

## 7.4 Blocks and Scope

A **block** is a sequence of statements enclosed in `{ ... }`. Blocks introduce a new **lexical scope**.

### Example

```mochi
{
  let x = 1
  print(x)
}
print(x)  // ❌ Error: x not declared
```

### Rules

* Variables declared inside a block are not accessible outside.
* Shadowing is permitted:

```mochi
let x = 1
{
  let x = 2
  print(x)  // prints 2
}
print(x)    // prints 1
```

Blocks are used in `fun`, `if`, `for`, `test`, `stream`, `on`, and `agent` constructs.

# 8. Scoping and Environment

Mochi uses **lexical scoping**. The visibility of a name is determined by the location of its declaration in the source
code. The interpreter manages variable resolution and mutation using a chain of runtime environments.

## 8.1 Lexical Scope

Each block defines a new **lexical scope**. Names declared with `let` are visible from the point of declaration to the
end of the block.

### Example

```mochi
{
  let a = 1
  print(a)  // ✅
}
print(a)    // ❌ undefined identifier
```

Shadowing is permitted:

```mochi
let x = 5
{
  let x = 10
  print(x)  // 10
}
print(x)    // 5
```

Each declaration binds a new name, even if an outer name exists with the same identifier.

## 8.2 Runtime Environments

At runtime, Mochi uses **environments** to store and resolve variable values.

An environment is a mutable mapping of names to values. Each block, function, or agent creates a new environment that
points to its **parent**.

* **Global environment**: Holds top-level declarations.
* **Local environments**: Created for blocks and functions.

### Variable lookup

Variable resolution proceeds from the current environment outward:

1. Search the current block
2. Search the enclosing function or block
3. Repeat until the global environment is reached

If no binding is found, a runtime error is raised.

### Environment operations

The interpreter supports:

* `Set(name, value)` – introduce a new binding in the current scope
* `Update(name, value)` – update an existing binding in any enclosing scope
* `Get(name)` – retrieve the value of a binding

### Example

```mochi
let x = 1
{
  x = 2      // updates outer `x`
  let y = 3  // introduces local `y`
}
```

## 8.3 Closures and Captured Environments

Functions in Mochi capture their **defining environment**. This allows closures to retain access to local variables
after the outer function has returned.

### Example

```mochi
fun makeCounter(): fun(): int {
  let count = 0
  return fun(): int {
    count = count + 1
    return count
  }
}
```

Each call to `makeCounter()` returns a new closure with its own `count`.

```mochi
let next = makeCounter()
next()  // 1
next()  // 2
```

## 8.4 Parameters as Bindings

Function parameters are equivalent to local `let` bindings created when the function is called:

```mochi
fun greet(name: string) {
  print("Hi, " + name)
}
```

* Parameter bindings are scoped to the function body.
* Arguments are evaluated before binding.

## 8.5 Scope Errors

Using an undeclared name results in a runtime error:

```mochi
print(y)  // ❌ Error: undefined identifier "y"
```

Assigning to an undeclared name also raises an error:

```mochi
x = 3  // ❌ unless x was previously declared
```

All variables must be explicitly introduced with `let`.

# 9. Built-in Functions

Mochi includes a small set of **built-in functions** that are available in all scopes without declaration. These
functions are implemented directly in the runtime and support essential operations such as output and debugging.

Built-in functions behave like user-defined functions:

* They are **first-class values**.
* They may be called, passed, or ignored.
* They accept typed arguments and may return values.

## 9.1 `print`

The `print` function writes a space-separated list of arguments to the standard output, followed by a newline.

### Signature

```mochi
print(arg1, arg2, ...)
```

### Description

* Accepts **zero or more arguments** of any type.
* Each argument is stringified using a default runtime representation.
* Arguments are evaluated left to right.
* No formatting options are supported in v0.1.12.

### Return Value

* Always returns `null`.
* Intended for side effects, not value capture.

### Example

```mochi
print("Hello")              // Hello
print(1 + 2, true, "end")   // 3 true end
```

### Output

```
Hello
3 true end
```

## 9.2 Behavior and Restrictions

* `print` is globally available; it cannot be shadowed or redefined.
* It performs no static type checks on arguments.
* Unprintable or unsupported values may produce debug strings or raise errors.
* Future versions may support formatted printing (`println`, `format`).

## 9.3 Future Built-ins

Mochi aims to remain minimal by default. Additional built-ins will be added cautiously and organized into a **standard
library** in future versions.

Planned additions include:

* **I/O utilities**: `input`, `write`, `readFile`
* **String functions**: `len`, `split`, `substring`
* **Math functions**: `abs`, `min`, `max`, `floor`, `ceil`
* **Higher-order functions**: `map`, `filter`, `reduce`
* **Type introspection**: `typeof`, `toString`

Standard functions may be grouped into libraries and imported explicitly when the module system is introduced.

# 10. Types and Values

Mochi is a statically typed language. Every value has a **type**, either explicitly declared or inferred from context.
Types are used to describe the shape, behavior, and allowed operations of values during both static analysis and runtime
evaluation.

At runtime, values are dynamically checked against their expected types. Type mismatches produce runtime errors.

## 10.1 Primitive Types

Mochi defines the following primitive types:

| Type     | Description                           | Examples        |
|----------|---------------------------------------|-----------------|
| `int`    | 64-bit signed integer                 | `0`, `42`, `-7` |
| `float`  | 64-bit IEEE-754 floating-point number | `3.14`, `0.0`   |
| `bool`   | Boolean value                         | `true`, `false` |
| `string` | Immutable UTF-8 string                | `"hello"`       |
| `null`   | Special sentinel value                | *(no literal)*  |
| `func`   | Function or closure                   | `fun(x: int)`   |

## 10.2 Type Inference

If a type annotation is omitted, the type is inferred from the initializer expression:

```mochi
let a = 1         // int
let b = 3.14      // float
let s = "hi"      // string
let t = true      // bool
```

Types are also inferred within expressions and function bodies, where possible.

## 10.3 Function Values

Function values are created using function expressions (closures) or declarations. They may be:

* Assigned to variables
* Passed as arguments
* Returned from other functions

Example:

```mochi
let add = fun(a: int, b: int): int => a + b
let inc = add(1)
print(inc(4))    // 5
```

Functions may capture variables from their lexical environment and retain them across invocations.

## 10.4 `null`

The `null` value is used to represent:

* The default result of a function with no `return`
* The return value of `print` and other side-effecting functions
* Uninitialized or absent values in future extensions

There is no literal syntax for `null`. It is produced implicitly.

Comparison with `null` is allowed:

```mochi
if result == null {
  print("empty")
}
```

## 10.5 Type Checking and Errors

Mochi performs type checks at runtime to ensure operations are valid. Type errors are reported when incompatible
operations are attempted.

```mochi
1 + "a"    // ❌ runtime error: int + string
```

Operations are defined only for compatible types:

| Operation          | Valid Types               |
|--------------------|---------------------------|
| `+`, `-`, `*`, `/` | `int`, `float`            |
| `==`, `!=`         | all types (except `func`) |
| `<`, `>`, etc.     | `int`, `float` only       |
| `!` (not)          | `bool` only               |

Function values are not comparable.

## 10.6 Equality and Comparison

| Type     | Supports `==`, `!=`  | Supports `<`, `>` |
|----------|----------------------|-------------------|
| `int`    | ✅                    | ✅                 |
| `float`  | ✅                    | ✅                 |
| `bool`   | ✅                    | ❌                 |
| `string` | ✅                    | ❌                 |
| `func`   | ❌                    | ❌                 |
| `null`   | ✅ (only with `null`) | ❌                 |

Comparing values of different types is a runtime error.

```mochi
1 == "1"    // ❌ error
```

## 10.7 Runtime Representation

Internally, the interpreter stores values using Go-native types:

| Mochi Type | Go Type          |
|------------|------------------|
| `int`      | `int64`          |
| `float`    | `float64`        |
| `string`   | `string`         |
| `bool`     | `bool`           |
| `null`     | `nil`            |
| `func`     | Closure with env |

The interpreter dispatches operations dynamically based on type.

# Appendix A. Grammar Summary

This appendix defines the complete grammar of Mochi in **EBNF** (Extended Backus–Naur Form), accompanied by **clear examples and explanations** to help users understand how to write valid Mochi code.


## A.1 Program

A Mochi program consists of a sequence of statements. There is no `main` function — code runs from top to bottom.

```ebnf
Program = { Statement } .
```

```mochi
// A simple Mochi program
let name = "Mochi"
print("Hello", name)
```


## A.2 Statements

Mochi supports variable declarations, assignments, functions, conditionals, loops, tests, and event-based constructs.

```ebnf
Statement =
    LetStmt
  | AssignStmt
  | FunDecl
  | ReturnStmt
  | IfStmt
  | ForStmt
  | ExprStmt
  | TestBlock
  | ExpectStmt
  | StreamDecl
  | OnHandler
  | AgentDecl .
```

```mochi
let x = 1                // Declare a variable
x = x + 1                // Assign a new value
fun square(n: int): int { return n * n }  // Define a function

if x > 0 {
  print("positive")
} else {
  print("not positive")
}

for i in 0..3 {
  print(i)
}

test "math" {
  expect square(2) == 4
}
```


## A.3 Expressions

Expressions are combinations of values and operators that evaluate to a result.

```ebnf
Expression  = Equality .
Equality    = Comparison { ("==" | "!=") Comparison } .
Comparison  = Term { ("<" | "<=" | ">" | ">=") Term } .
Term        = Factor { ("+" | "-") Factor } .
Factor      = Unary { ("*" | "/") Unary } .
Unary       = { "-" | "!" } Postfix .
Postfix     = Primary { IndexOp } .
```

```mochi
let result = (1 + 2) * 3 - 1   // Arithmetic with grouping
let ok = !(result == 8)        // Logical NOT and equality
```


## A.4 Primary Expressions

The most basic forms of expressions: literals, variables, functions, calls, etc.

```ebnf
Primary =
    Literal
  | Identifier
  | ListLiteral
  | FunctionExpr
  | CallExpr
  | SelectorExpr
  | "(" Expression ")" .
```

```mochi
let nums = [1, 2, 3]             // List literal
let square = fun(x: int): int => x * x
let value = (1 + 2) * 3          // Parentheses
```


## A.5 Function Expressions

Anonymous functions (closures) can be written with either arrow or block bodies.

```ebnf
FunctionExpr =
  "fun" "(" [ ParamList ] ")" [ ":" Type ] ( "=>" Expression | Block ) .
```

```mochi
let add = fun(a: int, b: int): int => a + b      // Arrow body
let inc = fun(x: int): int { return x + 1 }      // Block body
```


## A.6 Function Calls

Apply a function to arguments.

```ebnf
CallExpr = Identifier "(" [ ArgList ] ")" .
ArgList  = Expression { "," Expression } .
```

```mochi
print("sum is", add(1, 2))  // Call with multiple arguments
```


## A.7 Selectors

Access a nested value or property, like `event.payload.id`.

```ebnf
SelectorExpr = Identifier "." Identifier { "." Identifier } .
```

```mochi
print(event.payload.value)  // Dot notation for nested fields
```


## A.8 Lists and Indexing

Lists can be indexed with integers or sliced with `start:end` syntax.

```ebnf
ListLiteral = "[" [ Expression { "," Expression } [ "," ] ] "]" .
IndexOp     = "[" [ Expression ] [ ":" Expression ] "]" .
```

```mochi
let nums = [10, 20, 30, 40]
print(nums[1])         // 20
print(nums[1:3])       // [20, 30]
```


## A.9 Types

Mochi supports simple and function types with optional annotations.

```ebnf
Type        = SimpleType | FuncType .
SimpleType  = Identifier .
FuncType    = "fun" "(" [ TypeList ] ")" [ ":" Type ] .
TypeList    = Type { "," Type } .
```

```mochi
let f: fun(int): int = fun(x: int): int => x * x
```


## A.10 Literals

Literals represent constant values — numbers, booleans, and strings.

```ebnf
Literal  = Integer | Float | Boolean | String .
Integer  = Digit { Digit } .
Float    = Digit { Digit } "." Digit { Digit } .
Boolean  = "true" | "false" .
String   = '"' { Character | Escape } '"' .
```

```mochi
let x = 42
let y = 3.14
let b = true
let s = "hello\nworld"
```


## A.11 Identifiers

Identifiers name variables, functions, and fields.

```ebnf
Identifier = Letter { Letter | Digit | "_" } .
Letter     = "a" .. "z" | "A" .. "Z" | "_" .
Digit      = "0" .. "9" .
```

```mochi
let my_var = 123
let Count42 = my_var
```


## A.12 Comments

Mochi supports both single-line and multi-line (block) comments.

```ebnf
LineComment  = "//" { any character except newline } .
BlockComment = "/*" { any character except "*/" } "*/" .
```

```mochi
// This is a comment
let x = 1

/*
  This is a block comment
  over multiple lines
*/
```


## A.13 Test Blocks

Tests help verify behavior during execution.

```ebnf
TestBlock  = "test" StringLiteral Block .
ExpectStmt = "expect" Expression .
```

```mochi
test "math basics" {
  expect 1 + 2 == 3
}
```


## A.14 Streams and Agents

Streams carry events; agents respond to them with `on` handlers.

```ebnf
StreamDecl  = "stream" Identifier Block .
OnHandler   = "on" Identifier "as" Identifier Block .
AgentDecl   = "agent" Identifier Block .
```

```mochi
stream clicks {
  x: int
  y: int
}

agent Logger {
  on clicks as e {
    print("Clicked at", e.x, e.y)
  }
}
```

