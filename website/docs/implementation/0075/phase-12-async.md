# MEP-75 Phase 12: Async PHP Bridge (ReactPHP / Revolt / Amp)

**Status**: LANDED 2026-05-30 00:36 (GMT+7)

## Goal

Implement async-aware extern emission for PHP methods that return promise/future types from the major PHP async runtimes: ReactPHP, RevoltPHP, Amp, and Guzzle. The emitter produces `async extern fn` declarations so Mochi callers can await PHP async operations.

## Supported Promise Types

| FQCN | Runtime |
|---|---|
| `React\Promise\PromiseInterface` | ReactPHP |
| `React\Promise\Promise` | ReactPHP |
| `Amp\Promise` | Amp v2 |
| `Amp\Future` | Amp v3 / Revolt |
| `GuzzleHttp\Promise\PromiseInterface` | Guzzle HTTP |
| `GuzzleHttp\Promise\Promise` | Guzzle HTTP |

Extra promise types can be injected via `Config.ExtraPromiseTypes`.

## Design

`IsPromiseType(phpType, extra)` detects async return types by matching against the known FQCN table plus any caller-supplied extra types. Leading backslash is stripped before comparison.

`Emit(surface, cfg)` scans all class and interface methods. Methods are emitted as `async extern fn` if their return type is a promise type. Magic methods (`__`-prefixed) are always skipped.

Return type is always `unit` because PHP's reflection surface does not expose generic type parameters.

`LoopDriver` enum (`LoopRevolt`, `LoopReactPHP`, `LoopAmp`) is reserved for future glue-code generation targeting a specific event loop backend.

`ToSnakeCase` was exported from `package3/php/externemit` to allow reuse.

## Files Landed

- `package3/php/asyncemit/asyncemit.go` -- IsPromiseType + Emit + emitter
- `package3/php/asyncemit/asyncemit_test.go` -- 10 test functions
- `package3/php/externemit/externemit.go` -- exported ToSnakeCase wrapper

## Test Coverage

- IsPromiseType for all known promise FQCN variants
- IsPromiseType with leading backslash normalisation
- IsPromiseType with extra custom types
- Emit counts async methods correctly
- Emit emits async extern fn syntax
- Emit skips magic methods
- Emit produces empty output for non-async surface
- Emit respects ExtraPromiseTypes config
- All async methods return unit
- Instance methods have self parameter
