# MEP-75 Phase 5: Mochi Extern Emitter + SKIPPED.txt

**Status**: LANDED 2026-05-30 00:00 (GMT+7)

## Goal

Implement the Mochi extern fn / extern type emitter under `package3/php/externemit`. Given a PHP reflection surface, the emitter produces:

- `extern type` declarations for every public class, interface, and enum
- `extern fn` declarations for every translatable public method and top-level function
- A `SKIPPED.txt` report for items that cannot be translated

## Emitter Design

The emitter walks the `ReflectionSurface` and calls `typemap.Map` for each parameter and return type. On success it accumulates extern declarations; on failure it appends a `SkipReport`.

### Naming conventions

- Type handles: PascalCase from FQCN (GuzzleHttp\\Client -> GuzzleHttpClient)
- Method fns: `snake_case(handle) + "_" + snake_case(method)`, e.g. `guzzle_http_client_send`
- Static methods: no `self` receiver parameter
- Top-level functions: `snake_case(namespace_fn_name)`
- Backed enum constructor: `snake_case(handle) + "_from_value"`

### Skip rules

| Condition | Reason |
|---|---|
| Magic method (__construct, __get, etc.) | SkipMagicMethod |
| Variadic parameter | SkipVararg |
| Untyped parameter (no annotation) | SkipMixed |
| Return type mixed/object/callable/etc. | SkipMixed / SkipObject / etc. |
| array (untyped) parameter | SkipUntypedArray |
| Intersection type A&B | SkipIntersection |

## Files Landed

- `package3/php/externemit/externemit.go` -- Emit() + FormatSKIPPED()
- `package3/php/externemit/externemit_test.go` -- 16 test functions

## Test Coverage

- Empty surface produces no output and no skips
- Class extern type declaration
- Instance method with receiver and typed parameters
- Static method (no self receiver)
- Magic methods skip with SkipMagicMethod
- Variadic parameters skip with SkipVararg
- Untyped parameters skip with SkipMixed
- Mixed return type skips
- No return annotation maps to unit
- Interface extern type + method
- Pure enum extern type (no fromValue)
- Backed enum fromValue constructor
- Top-level function emission
- Nullable parameter (string|nil)
- FormatSKIPPED with entries and empty
- toSnakeCase and classHandle unit tests
