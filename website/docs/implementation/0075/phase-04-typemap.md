# MEP-75 Phase 4: Closed PHP-to-Mochi Type Mapping Table

**Status**: LANDED 2026-05-30 00:00 (GMT+7)

## Goal

Implement the closed PHP-to-Mochi type translation table under `package3/php/typemap`. Every PHP type must produce either a Mochi extern-type string or a `SkipReason` -- no open-ended inference.

## Type Table

| PHP Type | Mochi Type | Notes |
|---|---|---|
| `int`, `integer` | `int` | primitive |
| `float`, `double` | `float` | primitive |
| `string` | `string` | primitive |
| `bool`, `boolean` | `bool` | primitive |
| `?T` / `T\|null` | `T\|nil` | nullable wrapping |
| `void` (return) | `unit` | DirectionOut only |
| `null` | `nil` | literal null type |
| `true`, `false` | `bool` | singleton bool types |
| `iterable` | `list[any]` | coarse mapping |
| `array<T>` | `list[T]` | via MapTypedArray |
| `array<K,V>` | `map[K]V` | via MapTypedArray |
| `FQCN` | PascalCase handle | GuzzleHttp\\Client -> GuzzleHttpClient |
| `A\|B` | `A\|B` | true union |
| `mixed` | -- | SkipMixed |
| `object` | -- | SkipObject |
| `callable` | -- | SkipCallable |
| `resource` | -- | SkipResource |
| `never` | -- | SkipNever |
| `self`/`static`/`parent` | -- | SkipSelfStatic |
| `array` (untyped) | -- | SkipUntypedArray |
| `A&B` | -- | SkipIntersection |

## Files Landed

- `package3/php/typemap/typemap.go` -- full implementation
- `package3/php/typemap/typemap_test.go` -- comprehensive test suite (15 test functions)

## Test Coverage

- Primitives (all PHP aliases)
- Nullable wrapping (?T and T|null)
- Void return type (DirectionOut)
- All 8 SkipReason variants triggered by Map()
- Class handle generation (FQCN -> PascalCase)
- Nullable class handles
- Union degenerate (int|null -> int|nil)
- Union two types (int|string)
- Union with null (int|string|null)
- Union containing skip component
- Intersection type skip
- MapTypedArray list and map forms
- MapTypedArray propagates skip from value type
- Case-insensitive matching (INT, STRING, BOOL, FLOAT)
- classHandleType edge cases (empty, leading backslash, multi-segment)
- Mapping struct field verification
