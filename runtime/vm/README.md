# `runtime/vm`

A lightweight bytecode virtual machine used for executing a small subset of Mochi programs.  It includes a compiler that converts parsed Mochi ASTs into a sequence of instructions which are then executed by the VM. The VM is included in the main `mochi` CLI and can be invoked with `mochi run`.

## Architecture

* **Compiler** – walks the parsed program and emits instructions.  Each top level function becomes a `Function` containing bytecode instructions and register count.  A `Program` is a collection of these functions.
* **VM** – executes a `Program` using a simple stack of call frames.  Each frame has its own register slice.  Instructions operate on registers and support calls between compiled functions.
* **Disassembler** – converts bytecode back into a human readable listing, used by tests.

## Supported features

The VM supports a small but useful subset of Mochi:

* Integer, float, boolean and string constants
* Arithmetic operations `+`, `-`, `*`, `/`, `%`
* String concatenation using `+`
* Comparison operators `==`, `!=`, `<`, `>`, `<=`, `>=`
* Membership tests using `in`
* Short circuit boolean operators `&&` and `||`
* Variable definitions using `let` (immutable) and `var` (mutable)
  with optional type annotations and reassignment
* `if`, `while` and `for` loops with `break` and `continue`
* Function definitions
* Function calls with any number of arguments
* Anonymous function expressions
* Built‑ins `len`, `print` (up to two arguments), `append`, `concat`, `first`, `str`, `json`, `now`, `input`, `count`, `exists`, `avg`, `min`, `max`, `substring`, `substr`, `upper`, `lower`, `b64encode` and `b64decode`
* Dataset loading with `load` (optionally casting rows to a type) and saving with `save`
* HTTP requests using the `fetch` expression
* Loops internally use specialised integer instructions for indexing to improve performance
* List, map and struct construction
* String indexing to access individual characters
* Field access using the `.` operator
* List and string slicing with `[start:end]` syntax (supports negative indices)
* Pattern matching
* Test blocks with `expect` statements
* Capable of running the full suite of simplified TPC‑DS benchmark queries
* Able to execute the JOB dataset benchmark queries used for compiler testing
* Golden tests cover the generated SQLLogicTest programs under `tests/dataset/slt`

## Unsupported features

Many language capabilities are intentionally left out of this
experimental VM.  Unsupported areas include:

* Error handling with `try`/`catch`
* Logic programming constructs (`fact`, `rule`, `query`)
* Set collections (`set<T>`) and related operations
* Generic type parameters for functions and user-defined types
* Reflection and macro facilities
* Concurrency primitives such as `spawn` and channels
* Package declarations and `export` statements
* Asynchronous functions (`async`/`await`)
* Agents, streams and intent blocks with persistent state
* Agent initialization with field values
* Generative AI blocks, model declarations and other LLM helpers
* Foreign imports and FFI calls
* The `eval` builtin function
* Generic methods inside `type` blocks
* Pattern matching on union variants or enums
* Advanced string or list slicing beyond `[start:end]`
* Functions with multiple return values or variadic parameters
* Methods declared inside `type` blocks
* Enum and additional union type declarations
* Extern type declarations
* Increment/decrement operators (`++`, `--`) and compound
  assignments like `+=`
* Python style `range()` loops

This VM is intentionally simple and primarily used for experimentation and testing.

## Running tests

Golden tests ensure the VM stays in sync with the main interpreter. Execute:

```
go test ./tests/vm -run .
```

Use `-update` to refresh the expected output files when modifying the VM.

## Instruction set

Below is a brief overview of the bytecode instructions emitted by the compiler.
Each instruction operates on registers `r0`, `r1`, ... as shown by the examples
from the disassembler:

| Opcode | Description | Example |
|-------|-------------|---------|
| `Const` | Load constant value into register | `Const r0, 1` |
| `Move` | Copy value from register B to A | `Move r1, r0` |
| `Add` | Add registers B and C (strings concatenate) | `Add r2, r0, r1` |
| `Sub` | Subtract register C from B | `Sub r2, r0, r1` |
| `Mul` | Multiply registers B and C | `Mul r2, r0, r1` |
| `Div` | Divide register B by C | `Div r2, r0, r1` |
| `Mod` | Remainder of B / C | `Mod r2, r0, r1` |
| `Equal` | Store B == C | `Equal r2, r0, r1` |
| `NotEqual` | Store B != C | `NotEqual r2, r0, r1` |
| `Less` | Store B < C | `Less r2, r0, r1` |
| `LessEq` | Store B <= C | `LessEq r2, r0, r1` |
| `In` | Check membership of B in C | `In r2, r0, r1` |
| `Jump` | Unconditional jump to label A | `Jump L1` |
| `JumpIfFalse` | Jump to label B if register A is false | `JumpIfFalse r0, L1` |
| `Len` | Length of list, map or string B | `Len r1, r0` |
| `Index` | Index B by C | `Index r2, r0, r1` |
| `Slice` | Slice B using [C:D] | `Slice r3, r0, r1, r2` |
| `SetIndex` | Set index/key in A using B to value C | `SetIndex r0, r1, r2` |
| `MakeList` | Create list of B values starting at C | `MakeList r0, 2, r1` |
| `MakeMap` | Create map of B key/value pairs starting at C | `MakeMap r0, 2, r1` |
| `Print` | Print value in A | `Print r0` |
| `Print2` | Print values A and B | `Print2 r0, r1` |
| `PrintN` | Print B registers starting at C | `PrintN r0, 2, r1` |
| `Call2` | Call function index B with registers C and D | `Call2 r0, fn, r1, r2` |
| `Call` | Call function index B with C args starting at D | `Call r0, fn, 2, r1` |
| `CallV` | Call function in register B with C args starting at D | `CallV r0, r1, 2, r2` |
| `Return` | Return register A | `Return r0` |
| `Not` | Logical NOT of B | `Not r0, r1` |
| `JumpIfTrue` | Jump to B if register A is true | `JumpIfTrue r0, L1` |
| `Now` | Current time in nanoseconds | `Now r0` |
| `JSON` | Print value A as JSON | `JSON r0` |
| `Append` | Append C to list B | `Append r0, r1, r2` |
| `Str` | Convert value B to string | `Str r0, r1` |
| `Upper` | Uppercase string B | `Upper r0, r1` |
| `Lower` | Lowercase string B | `Lower r0, r1` |
| `Input` | Read line from input | `Input r0` |
| `First` | First element of list B | `First r0, r1` |
| `Count` | Count elements in list/map B | `Count r0, r1` |
| `Exists` | True if list/map/string B is non-empty | `Exists r0, r1` |
| `Avg` | Average of numeric list B | `Avg r0, r1` |
| `Sum` | Sum of numeric list B | `Sum r0, r1` |
| `Min` | Minimum value of numeric list B | `Min r0, r1` |
| `Max` | Maximum value of numeric list B | `Max r0, r1` |
| `Values` | List of values in map B | `Values r0, r1` |
| `Cast` | Cast value B to type index C | `Cast r0, r1, 0` |
| `IterPrep` | Prepare iterable from B | `IterPrep r0, r1` |
| `Load` | Load dataset from path B with options C | `Load r0, r1, r2` |
| `Save` | Save dataset B to path C with options D | `Save r0, r1, r2, r3` |
| `Eval` | Evaluate Mochi source string in B | `Eval r0, r1` |
| `Fetch` | HTTP fetch from URL B with options C | `Fetch r0, r1, r2` |
| `MakeClosure` | Create closure from function index B capturing C regs from D | `MakeClosure r0, fn, 2, r1` |
| `AddInt` | Add two integers | `AddInt r0, r1, r2` |
| `AddFloat` | Add two floats | `AddFloat r0, r1, r2` |
| `SubInt` | Subtract two integers | `SubInt r0, r1, r2` |
| `SubFloat` | Subtract two floats | `SubFloat r0, r1, r2` |
| `MulInt` | Multiply two integers | `MulInt r0, r1, r2` |
| `MulFloat` | Multiply two floats | `MulFloat r0, r1, r2` |
| `DivInt` | Integer division | `DivInt r0, r1, r2` |
| `DivFloat` | Float division | `DivFloat r0, r1, r2` |
| `ModInt` | Integer modulo | `ModInt r0, r1, r2` |
| `ModFloat` | Float modulo | `ModFloat r0, r1, r2` |
| `EqualInt` | Compare two integers | `EqualInt r0, r1, r2` |
| `EqualFloat` | Compare two floats | `EqualFloat r0, r1, r2` |
| `LessInt` | Integer less-than | `LessInt r0, r1, r2` |
| `LessFloat` | Float less-than | `LessFloat r0, r1, r2` |
| `LessEqInt` | Integer <= | `LessEqInt r0, r1, r2` |
| `LessEqFloat` | Float <= | `LessEqFloat r0, r1, r2` |
| `Neg` | Negate number in B | `Neg r0, r1` |
| `NegInt` | Negate integer | `NegInt r0, r1` |
| `NegFloat` | Negate float | `NegFloat r0, r1` |
| `UnionAll` | Concatenate lists B and C | `UnionAll r0, r1, r2` |
| `Union` | Union of lists B and C | `Union r0, r1, r2` |
| `Except` | Difference of lists B and C | `Except r0, r1, r2` |
| `Intersect` | Intersection of lists B and C | `Intersect r0, r1, r2` |
| `Sort` | Sort pairs in list B by first element | `Sort r0, r1` |
| `First` | First element of list B | `First r0, r1` |

