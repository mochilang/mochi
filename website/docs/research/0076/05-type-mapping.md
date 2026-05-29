---
title: "05. Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed RBS-to-Mochi type translation table, refusal cases (untyped, complex unions, self/instance/class/top/bot, void in non-return, Proc beyond simple arities), Symbol handling, and the SkipReport format."
---

# 05. Type mapping table

This note enumerates the closed translation table the bridge uses to map RBS types to Mochi types. An item whose entire public signature falls inside the table is translated; any item with a single out-of-table type is skipped with a `SkipReport`.

## Scalar types

| RBS type | Mochi type | Notes |
|----------|-----------|-------|
| `Integer` | `int` | Maps to Mochi's 64-bit int. Ruby integers are arbitrary-precision; values outside `[-2^63, 2^63)` panic the shim at runtime. |
| `Float` | `float` | Maps to Mochi's 64-bit float (IEEE 754 double). |
| `String` | `string` | Owned UTF-8 copy across the bridge. |
| `Symbol` | `string` | See the Symbol special case section below. |
| `bool` | `bool` | The RBS literal types `true` and `false` are also both mapped to `bool`. |
| `true` | `bool` | Literal singleton; widened to `bool`. |
| `false` | `bool` | Literal singleton; widened to `bool`. |
| `nil` | `nil` | Maps to Mochi's nil type. Used in return position or union forms. |
| `void` (return only) | `unit` | `void` in non-return position is refused (see refusal section). |

## Composite types

| RBS type | Mochi type | Notes |
|----------|-----------|-------|
| `Array[T]` where T in table | `list<T>` | Owned copy across the bridge. |
| `Hash[String, V]` where V in table | `map<string, V>` | String keys. |
| `Hash[Symbol, V]` where V in table | `map<string, V>` | Symbol keys converted to strings; the shim strips the leading `:` (Ruby `:foo` → Mochi `"foo"`). |
| `T?` | `T?` | RBS shorthand for `T \| nil`; the bridge recognises both forms. |
| `T \| nil` | `T?` | Expanded optional form; normalised to `T?` in the emitted Mochi. |
| `[A, B]` | `tuple<A, B>` | RBS tuple type. Both elements must be in table. |
| `[A, B, C]` | `tuple<A, B, C>` | Triples and higher supported up to arity 12. |
| `Integer \| Float` | `float` | Numeric widening union; the only non-nil binary union the bridge accepts. |
| `String \| Symbol` | `string` | Symbol-string widening; accepted as a convenience for APIs that accept either. |

## Optional types

RBS has two syntactic forms for "nullable T":

```rbs
# Form 1: shorthand
def fetch: (String key) -> Integer?

# Form 2: explicit union
def fetch: (String key) -> (Integer | nil)
```

The bridge normalises both to Mochi's `int?`. The canonical emitted declaration:

```mochi
extern fun fetch(key: string): int? from ruby "MyClass#fetch"
```

## Proc and lambda types

RBS `^(A) -> B` translates to Mochi `fun(A): B` when both `A` and `B` are in table:

| RBS | Mochi |
|-----|-------|
| `^(Integer) -> String` | `fun(int): string` |
| `^(String, Integer) -> bool` | `fun(string, int): bool` |
| `^(Float, Float, Float) -> Float` | `fun(float, float, float): float` |

Procs are supported up to arity 5. Procs where any parameter or return type is `untyped` are refused. Procs returning `void` in non-callback position are refused (ambiguous semantics; use explicit `nil` return instead).

Higher-arity procs (arity > 5) are refused in v1. The SkipReport instructs the user to hand-author an `extern fun` override.

## Class-to-record translation

A Ruby class whose public interface is entirely `attr_reader` methods with types inside the table translates to a Mochi record. The RBS signature drives the translation, not the Ruby implementation:

```rbs
class Point
  attr_reader x: Integer
  attr_reader y: Integer
  def initialize: (Integer x, Integer y) -> void
end
```

Becomes:

```mochi
record Point {
    x: int,
    y: int,
}
```

The `initialize` constructor is exposed as:

```mochi
extern fun point_new(x: int, y: int): Point from ruby "Point.new"
```

A class that has any method beyond `attr_reader` / `attr_accessor` on in-table types, or any `attr_reader` with an out-of-table type, is refused as a record. The entire class is skipped with a SkipReport citing the first offending item.

`attr_writer`-only and `attr_accessor` fields: `attr_accessor :x` is accepted if `x`'s type is in table; the bridge emits a mutable record field.

## DataClass (Ruby 3.2+)

`Data.define(:x, :y)` with RBS annotations translates to a Mochi record identically to an all-attr_reader class:

```rbs
class Coord < Data
  attr_reader x: Float
  attr_reader y: Float
end
```

Becomes:

```mochi
record Coord {
    x: float,
    y: float,
}
```

DataClass instances are immutable in Ruby; the bridge emits the Mochi record as immutable (no `mut` modifier).

## Struct translation

`Struct.new(:x, :y)` with RBS annotations translates to a Mochi record if all fields are in table:

```rbs
class Vec2 < Struct
  attr_reader x: Float
  attr_reader y: Float
end
```

Becomes:

```mochi
record Vec2 {
    x: float,
    y: float,
}
```

A Struct with any field outside the table is refused entirely (the bridge does not emit partial records).

## Symbol special case

RBS `Symbol` becomes Mochi `string`. The generated shim includes a comment explaining the mapping:

```ruby
# Symbol bridge note:
# Ruby symbol equality :foo == :foo maps correctly to string equality "foo" == "foo".
# Ruby's Symbol#to_proc behaviour (:upcase.to_proc) is NOT bridged.
# If you need to_proc semantics, write an explicit lambda wrapper.
```

Specifically: `:upcase.to_proc` (which creates a proc that calls `.upcase` on its argument) has no Mochi analogue and is not generated. Users needing this pattern must hand-author a lambda shim on the Ruby side and declare an explicit `fun` binding.

Symbol hash keys (`:foo => value`) are converted to string keys by the bridge: `"foo" => value`. This conversion is transparent at the Mochi call site.

## Refusal cases

The following types cause a `SkipReport` entry for any item containing them:

| RBS type | Reason |
|----------|--------|
| `untyped` | No static type information; the bridge cannot emit a typed Mochi binding. |
| `top` | Ruby's top type has no Mochi analogue. |
| `bot` | Ruby's bottom type has no Mochi analogue. |
| `self` | Open-class metaprogramming; the concrete type is not statically knowable. |
| `instance` | Same as `self`. |
| `class` | Same as `self`. |
| `A \| B \| C` (3+ branch non-nil union) | Complex union; the bridge only accepts `T \| nil` and the two widening pairs above. |
| `void` (non-return position) | `void` as a parameter or field type is semantically unclear in RBS. |
| `IO` / `File` | Platform-dependent; not safe to bridge. |
| `BasicObject` | Too low-level; no RBS surface. |
| `Encoding` | Encoding metadata object; no Mochi analogue. |
| `Fiber` | Coroutine handle; no Mochi analogue in v1. |
| `Thread` | Concurrency primitive; no Mochi analogue in v1. |
| Proc with `untyped` param or return | Static type information missing. |
| Proc with arity > 5 | v1 arity limit. |

## SkipReport format

Each skipped item produces a `SkipReport` Go struct:

```go
type SkipReport struct {
    Gem     string // e.g., "nokogiri"
    Item    string // e.g., "Nokogiri::XML::Document#xpath"
    Reason  SkipReason
    RBSType string // the verbatim RBS type string that caused the skip
}

type SkipReason int
const (
    SkipUntyped         SkipReason = iota // untyped in signature
    SkipTopBot                            // top or bot type
    SkipSelfInstanceClass                 // self / instance / class
    SkipComplexUnion                      // 3+ branch union beyond T|nil
    SkipVoidNonReturn                     // void in non-return position
    SkipIOFile                            // IO or File type
    SkipBasicObject                       // BasicObject
    SkipEncoding                          // Encoding
    SkipFiber                             // Fiber
    SkipThread                            // Thread
    SkipProcUntyped                       // Proc with untyped A or B
    SkipProcHighArity                     // Proc with arity > 5
    SkipNoRBS                             // no RBS at all for this item
    SkipStructPartial                     // Struct with out-of-table field
    SkipClassPartial                      // class with out-of-table method
)
```

The `SkipReport` list is printed during `mochi pkg lock` and written to `<workdir>/ruby_wrap/<gem>/skip_report.txt`:

```
SKIPPED: nokogiri / Nokogiri::XML::Document#xpath
  Reason: SkipUntyped
  RBSType: untyped
  Override: hand-author an extern fun binding in mochi.toml [[ruby.extern]]

SKIPPED: mysql2 / Mysql2::Client#query
  Reason: SkipNoRBS
  RBSType: (none)
  Override: add RBS annotations or use [[ruby.extern]] to bind manually
```

## Cross-references

- [[02-design-philosophy]] for why the table is closed.
- [[04-rbs-ingest]] for how RBS files are parsed into the type surface.
- [[06-rubygems-publish-flow]] for the gemspec fields that carry type annotations.
- [[09-bundler-lockfile]] for lockfile entries recording which items were skipped.
- [MEP-76 §3](/docs/mep/mep-0076#3-lockfile-extension-ruby-package) for the normative type-table fields.
- [MEP-56](/docs/mep/mep-0056) for the Mochi-to-Ruby record bridge that the reverse direction reuses.
