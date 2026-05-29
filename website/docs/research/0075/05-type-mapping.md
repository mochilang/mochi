---
title: "05. Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed PHP-to-Mochi type translation table: every PHP 8.4 type with translation rationale, union types, intersection types, nullable, void, never, mixed, callable, object, self, static, parent, array shapes (list vs map heuristic), typed array PHPDoc annotations, enum (pure and backed), readonly class, fibers, and the SkipReport mechanism for out-of-table cases."
---

# 05. Type mapping table

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note documents the complete PHP-to-Mochi type translation table the bridge uses when synthesising Mochi `extern fn` declarations from a PHP package's reflection surface.

## 1. Guiding principles

The table is closed: every PHP type has an explicit entry (translate, SkipReport, or special-case). There are no implicit translations. The SkipReport mechanism ensures that untranslatable items are named and explained, not silently dropped.

PHP's type system is dynamic: a function declared as returning `string` at the PHP level is a hint that PHP enforces at runtime but that does not prevent a programmer from returning `int` in edge cases (strict_types=1 mode enforces it; MEP-55's output always uses `declare(strict_types=1)`). The bridge treats the declared type as authoritative for the purposes of the Mochi extern declaration, matching MEP-55's approach.

## 2. Scalar types

| PHP 8.4 type | Mochi type | Notes |
|---|---|---|
| `int` | `int` | PHP integers are 64-bit on all modern platforms. Direct mapping. |
| `float` | `float` | PHP `float` is IEEE 754 double. Direct mapping. |
| `string` | `string` | PHP strings are byte sequences (not UTF-8-validated). Same as Mochi strings. |
| `bool` | `bool` | Direct mapping. |
| `true` (literal type) | `bool` | PHP 8.0+ `true` as a standalone return type maps to `bool` with a SkipReport note that it always returns `true`. |
| `false` (literal type) | `bool` | Same as above; maps to `bool`. |
| `null` | `nil` | PHP `null` as a standalone type (not nullable) maps to Mochi `nil`. |

## 3. Nullable types

| PHP 8.4 type | Mochi type | Notes |
|---|---|---|
| `?T` where T is in-table | `T\|nil` | PHP's `?T` is shorthand for `T\|null`. Maps to Mochi's `T\|nil` union. |
| `T\|null` (explicit union) | `T\|nil` | Same as `?T`. |

## 4. Void and never

| PHP 8.4 type | Mochi type | Notes |
|---|---|---|
| `void` | `unit` | A function returning `void` maps to Mochi `unit`. |
| `never` | `panic` | PHP `never` means the function always throws or exits. Maps to Mochi's `panic` marker. The synthesised Mochi extern is declared with return type `unit` and the `from php "..." panic` annotation so the type checker knows it diverges. |

## 5. Object types

| PHP 8.4 type | Mochi type | Notes |
|---|---|---|
| Named class (concrete, all public properties typed) | `record` | If all public properties are typed PHP 8.4 (or PHP 8.1+ readonly), the bridge emits a Mochi `record` with matching fields. |
| Named class (concrete, some properties untyped or mixed) | opaque handle | The class is emitted as `extern type ClassName`. Methods are emitted as `extern fn`. |
| Named abstract class | `extern type AbstractName` | Abstract classes cannot be instantiated; emitted as an opaque handle. Concrete subclasses are emitted separately. See [[11-testing-gates]] §interfaces for the interface/abstract bridge. |
| Named interface | `extern type InterfaceName` | Interfaces map to opaque protocol handles. Methods are emitted as `extern fn`. |
| `object` (untyped) | SkipReport | The bare `object` type in PHP is equivalent to `mixed` for the bridge: it could be any class. Too ambiguous to translate. |
| `self` | SkipReport | `self` refers to the class of the current method's declaring class. The bridge does not resolve late static binding at reflection time. |
| `static` | SkipReport | `static` refers to the runtime class; cannot be resolved statically. |
| `parent` | SkipReport | Same as `self`. |
| `mixed` | SkipReport | `mixed` is the top type in PHP. No Mochi equivalent. Any item whose return type is `mixed` is skipped. |

## 6. Array types

PHP arrays are the most challenging type for the bridge. PHP's `array` is simultaneously a list (`[1, 2, 3]`) and a map (`["key" => "value"]`). PHP 8.x does not distinguish between them at the runtime type level.

The bridge applies a shape heuristic:

| PHP type / PHPDoc annotation | Mochi type | Heuristic |
|---|---|---|
| `array` (untyped) | SkipReport | Cannot determine list vs map shape. |
| PHPDoc `@param list<T>` where T in-table | `list<T>` | Explicit PHPDoc `list<T>` annotation. |
| PHPDoc `@param array<int, T>` where T in-table | `list<T>` | Integer-keyed array with consistent value type. |
| PHPDoc `@param array<string, T>` where T in-table | `map<string, T>` | String-keyed array with consistent value type. |
| PHPDoc `@param array<K, V>` where K is not string or int | SkipReport | Non-standard key type; bridge cannot translate. |
| PHPDoc `@return array{name: string, value: int}` (array shape) | `record` (anonymous) | Named-key array shapes map to anonymous Mochi records if all value types are in-table. |
| `array` (untyped, no PHPDoc) | SkipReport | No shape information available. |

The heuristic is conservative. An untyped `array` with no PHPDoc is always skipped. This is intentional: silently translating an unknown-shape array to `list<any>` would produce wrong Mochi types.

## 7. Enum types (PHP 8.1+)

PHP 8.1 introduced native enums. The bridge translates both variants:

| PHP enum form | Mochi type | Example |
|---|---|---|
| Pure enum (no backing type) | `type E = V1 \| V2 \| V3` (ADT) | `enum Color { Red, Green, Blue }` |
| Backed enum (`string` backing) | `type E = V1(string) \| V2(string)` with string value | `enum Status: string { Active = "active", Inactive = "inactive" }` |
| Backed enum (`int` backing) | `type E = V1(int) \| V2(int)` with int value | `enum Priority: int { Low = 1, High = 2 }` |
| Backed enum with methods | opaque handle + enum methods | If the backed enum declares methods, those are emitted as `extern fn`. |

## 8. Callable and closure types

| PHP 8.4 type | Mochi type | Notes |
|---|---|---|
| `callable` | SkipReport | The bare `callable` pseudo-type covers functions, closures, array callables (`[$obj, "method"]`), and string callables (`"strlen"`). Too polymorphic to translate. |
| `Closure` | `(A, B) -> C` (closure type) | Named `Closure` with explicit parameter and return types (via PHPDoc or PHP 8.0+ native return types) translates to a Mochi closure type. If the `Closure` type has no parameter list annotation, it is SkipReport. |
| PHP 8.1 first-class callable (`strlen(...)`) | closure | First-class callables (PHP 8.1) are `Closure` instances; translated the same way as `Closure`. |

## 9. Union and intersection types

| PHP 8.4 type | Mochi type | Notes |
|---|---|---|
| `T\|null` | `T\|nil` | Special-cased as nullable (see §3). |
| `T\|U` where both T and U are scalar in-table types | SkipReport | Mochi has no native union of scalars equivalent to `int\|string`. The bridge refuses to synthesise these; the user must hand-write an override. |
| `T\|U` where T or U is a class | SkipReport | PHP discriminated union types do not map cleanly to Mochi's ADT union. |
| `A&B` (intersection type, PHP 8.1) | SkipReport | Intersection types express "implements both A and B". Mochi has no intersection type. |
| `T\|false` | `T\|nil` special case | A common PHP pattern: return the value or `false` on failure. The bridge maps `T\|false` to `T\|nil` with a SkipReport note. This is a best-effort translation; the user should use `T?` or `Result<T>` semantics and hand-write the override if false-vs-nil matters. |

## 10. readonly class (PHP 8.2) and property promotion

PHP 8.2 introduced `readonly class`, which makes all promoted properties readonly. PHP 8.4's `abstract readonly class` (used by MEP-55 for sum types) is also handled.

| PHP construct | Mochi type | Notes |
|---|---|---|
| `readonly class Foo { public function __construct(public readonly string $bar) {} }` | `record Foo { bar: string }` | All-typed readonly class maps to a Mochi record. |
| `class Foo` with some `readonly` properties and some non-readonly | opaque handle | Mixed mutability; bridge cannot translate to an immutable record. Emitted as `extern type Foo`. |
| `abstract readonly class` (MEP-55 pattern for sum types) | `extern type FooBase` | The abstract base of a MEP-55 sum type is reflected as an opaque handle; the concrete variants are reflected as separate items. |

## 11. self, static, parent, mixed

These four types are SkipReport in all positions:

- `self`: resolved at class-declaration time by PHP, not at reflection time. The bridge would need to track the declaring class for every method, which complicates the reflection pass for marginal gain.
- `static`: resolved at runtime (late static binding). Cannot be known statically.
- `parent`: same as `self`.
- `mixed`: PHP's top type. No Mochi equivalent; any item with a `mixed` return or parameter is skipped.

## 12. PHP fibers (PHP 8.1)

PHP fibers are cooperative coroutines accessible via `new Fiber(callable)`, `Fiber::suspend()`, and `Fiber::resume()`. They do not appear in a package's public surface as a *type* (there is no `Fiber<T>` return type annotation in PHP 8.4's type system). Fibers appear as `Fiber` (the class) in return type positions when a method returns a fiber object.

| PHP type | Mochi type | Notes |
|---|---|---|
| `Fiber` (the built-in class, returned by a factory) | opaque handle `extern type Fiber` | Emitted as an opaque handle with `suspend`, `resume`, `getReturn`, `isTerminated` methods. The async semantics are not reflected in the Mochi type system; the user must manage fiber lifecycle manually via the opaque handle. |

## 13. SkipReport format

When the bridge refuses to translate an item, it emits a `SkipReport` entry in the lock-time diagnostic output:

```
SkipReport: guzzlehttp/guzzle::GuzzleHttp\Client::send
  reason: parameter $options has type `array` (untyped); no PHPDoc annotation found
  override: add `extern fn guzzle_client_send(c: Client, options: ???): Response from php "GuzzleHttp\Client::send" custom` to your source
```

The user can override any skipped item with a `from php "..." custom` annotation that bypasses the type table entirely.

## Cross-references

- [[04-packagist-ingest]] for how the PHP surface document is obtained.
- [[02-design-philosophy]] §3 for the closed-table rationale.
- [[11-testing-gates]] for how the fixture corpus tests cover the type table.
- [MEP-73 research/05](/docs/research/0073/05-type-mapping) for the analogous Rust type table.
- [MEP-55](/docs/mep/mep-0055) for the PHP lowering rules that inform what types MEP-55 can emit.
