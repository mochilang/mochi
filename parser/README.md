# Mochi parser

This package owns the lexer, the grammar, the AST node types, and the
documentation comment attachment pass. It produces a `*Program` that
the rest of the toolchain (type checker, interpreter, bytecode VM,
cross compilers) consumes.

The grammar specs live at `notes/Spec/1800/` (1801 for the lexer, 1802
for the grammar, 1803 for the AST). This file is the operational guide:
how to call the parser, how the code is laid out, and how to add a new
feature without breaking the goldens.

## Entry points

```go
// Read a file and parse it.
prog, err := parser.Parse(path)

// Parse a string in memory.
prog, err := parser.ParseString(src)
```

Both return `*parser.Program`. Errors are upgraded to a structured
`diagnostic.Error` value with a code, position, and help text. The
codes for parser errors are P prefixed and live in
`parser/errors.go:38-97` (see `suggestFix`).

## File layout

| File              | Owns                                              |
|-------------------|---------------------------------------------------|
| `parser.go`       | Lexer table, AST types, participle grammar.       |
| `errors.go`       | `Parse` entry point, error wrapping, P codes.     |
| `docs.go`         | Doc comment extraction and attachment.            |
| `parser_test.go`  | Golden tests for `tests/parser/{valid,errors}`.   |
| `mochi_parser_test.go` | Tests of the Mochi grammar against representative programs. |
| `docs_test.go`    | Tests for the doc attachment pass.                |

## Lexer

`parser.go:48-62`. A simple participle lexer with one regex per token
class. Order matters: `Bool` and `Keyword` come before `Ident` so
reserved words do not get classified as identifiers. The full set of
reserved keywords is on the `Keyword` line (line 51).

A few words that look like keywords are not reserved globally and only
become significant inside a specific production. Examples are query
clause names like `from`, `where`, `select`, `group`, `by`, and
modifier words like `as`, `to`, `with`. This keeps user identifier
names like `where` available outside query expressions.

A subtle design point. Numeric literals in the lexer do not include a
leading minus sign. Unary minus is handled by the parser as a prefix
operator. The reason is that `len(list)-1` should parse as
`len(list) - 1`, not as `len(list)`, `[`, `-1`, `]`. The cost is that
expressions like `x == -1` only parse because the grammar accepts a
unary expression on the right hand side of `==`. This is documented at
`parser.go:53-56`.

## Grammar

The grammar is encoded as participle struct tags on Go types. The
parser is built once at package init:

```go
var Parser = participle.MustBuild[Program](
    participle.Lexer(mochiLexer),
    participle.Elide("Whitespace", "Comment"),
    participle.Unquote("String"),
    participle.UseLookahead(999),
)
```

`UseLookahead(999)` lets the parser try arbitrarily many tokens of
lookahead when an alternation is ambiguous. The flat
`BinaryExpr -> Unary BinaryOp*` shape relies on this: the parser
collects a flat list of operators and the type checker rebalances them
by precedence.

Expression precedence is not in the grammar. The type checker resolves
it during inference at `types/infer.go:89-198`. Anyone reading the AST
directly must remember this and apply the precedence table or call
back into `types.ExprType`.

## AST

Every node in `parser.go` is a Go struct with two sets of tags:

- `parser:"..."` tags drive the participle grammar.
- `json:"..."` tags drive the parser golden tests, which serialise the
  AST as JSON.

The two views must stay in sync. Adding a field requires touching both
tag sets. The `omitempty` markers keep golden files small: an optional
field that is not used in a fixture does not appear in the golden.

Tagged unions are encoded as structs with one pointer field per
variant. Exactly one is non nil after a successful parse. The visitor
on the receiving side switches on the non nil pointer. Examples:

- `Statement` has 30 plus mutually exclusive fields.
- `TypeRef` has four (`Fun`, `Generic`, `Struct`, `Simple`).
- `Primary` has 17 (literals, queries, lambdas, control flow forms).

A pattern to know. `Primary.Struct` comes first in the alternation
because `Foo{...}` would otherwise be ambiguous with `Foo` followed by
a block start. The order of fields in the Go struct controls the order
of alternatives the parser tries.

## Doc comments

`docs.go`. After parsing, the source is re scanned for `///` doc
comments preceding declarations. The doc text is attached to the
nearest declaration node's `Doc` field. This pass is purely textual
and runs after participle has stripped comments from the token stream.

## Adding a new feature

A typical change touches several files. The checklist:

1. **Grammar**. Add the production to `parser.go`. If it is a new
   statement, add a field to `Statement`. If it is a new expression
   atom, add a field to `Primary`. Use participle tags consistently
   with the existing patterns.
2. **AST**. Define the Go struct for the new node and the JSON tags.
   Ensure `omitempty` is set on optional fields so old goldens stay
   minimal.
3. **Lexer (rare)**. Adding a new keyword means editing the `Keyword`
   regex at `parser.go:51`. Be careful: this reserves the word
   globally and may break user code that used it as an identifier.
   See 1810 C5.
4. **Tests**. Add a fixture under `tests/parser/valid/` for the
   accepted shape. Add a fixture under `tests/parser/errors/` for at
   least one malformed shape. Run:

   ```
   make update-golden STAGE=parser
   ```

   to seed the new goldens. Inspect the JSON to make sure the AST
   shape matches what you expected.
5. **Type checker**. Add the typing rule to `types/check.go` (and
   `types/infer.go` for expressions). Add fixtures under
   `tests/types/valid/` and `tests/types/errors/`.
6. **Specs**. Update the relevant 18xx spec under `notes/Spec/1800/`.
   At minimum 1802 (grammar) and 1803 (AST), plus 1805 if there is a
   typing rule.

## Test commands

```
# Run the parser tests.
go test ./parser

# Run only the valid fixtures.
go test ./parser -run TestParser_ValidPrograms

# Run only one fixture (the harness honours an environment variable).
MOCHI_ROSETTA_ONLY=if_stmt go test ./parser -run TestParser_ValidPrograms

# Refresh goldens after a deliberate AST change.
go test ./parser -update
```

The harness lives at `golden/golden.go`. It glob matches `*.mochi`
files in the directory and pairs them with `*.golden` (or `*.err`)
companions.

## Diagnostic codes

Parser diagnostics use P prefixed codes (`parser/errors.go:38-97`):

- `P001` block delimiter mismatch.
- `P002` unexpected EOF.
- `P010` function body must be braced.
- `P011` `=>` cannot replace a block in a function body.
- `P020` expected expression.
- `P030` expected identifier.
- `P031` `let` not followed by a name.
- `P040` unterminated string.
- `P050` `*` in primary position.
- `P051` missing comma.
- `P052` unbalanced parentheses.
- `P053` stray `}`.
- `P054` misused colon in parameter list.
- `P055` stray `.`.
- `P056` unexpected `let`.
- `P999` everything else.

Type checker diagnostics use T prefixed codes registered in
`types/errors.go`. Spec 1806 has the full table.

## Known parser limitations

These are tracked in 1810. Brief versions:

- `int | nil` does not parse as a type. Use either a struct or wait
  for option types.
- `x == -1` requires a space or parens because of how unary minus
  binds. The error message is misleading.
- Block comments `/* ... */` do not nest.
- The `Keyword` regex at `parser.go:51` reserves words globally. New
  keywords break user code that used them as identifiers.

## Performance notes

Parsing a typical 200 line file is sub millisecond. The parser uses
unbounded lookahead, so pathological inputs (deeply nested ambiguous
constructs) can be slow. The fixture set is the practical guard
against regressions; if a new feature pushes parse time up
noticeably, add a regression test under `tests/parser/valid/` that
exercises the worst case.

## CHECKLIST

- [ ] New grammar features add fixtures under `tests/parser/valid/`
      and `tests/parser/errors/`.
- [ ] New AST fields are JSON tagged with `omitempty`.
- [ ] New keywords are reviewed for collision with user identifiers.
- [ ] New diagnostic codes are added to `errors.go` and listed here.
- [ ] Spec docs (1801, 1802, 1803) are updated in the same change.
