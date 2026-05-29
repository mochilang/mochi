# MEP-75 Phase 6: PHP Glue Stub Emitter

**Status**: LANDED 2026-05-30 00:00 (GMT+7)

## Goal

Implement the PHP-side glue stub emitter under `package3/php/glue`. The glue stubs bridge the Mochi extern type bindings to the real Composer autoloaded classes, providing a stable PHP wrapper layer in the `MochiGlue\<Vendor>\<Package>` namespace.

## Design

The `Emit()` function takes a `ReflectionSurface` and vendor/package names, and generates PHP files under the `MochiGlue\<PascalVendor>\<PascalPackage>` namespace.

### Namespace convention

`"guzzlehttp"/"guzzle"` -> `MochiGlue\Guzzlehttp\Guzzle`

The `MochiGlue\` prefix is reserved -- no upstream Packagist package uses it.

### Per-class stubs

For each public class:
- A PHP wrapper class with `private $_inner` holding the real object
- Constructor `__construct(OrigClass $inner)` for injection
- Instance method forwarders: `public function method(...params): RetType { return $this->_inner->method(...); }`
- Static method forwarders: `public static function method(...): RetType { return OrigClass::method(...); }`
- Magic methods (__construct from original, __get, etc.) are NOT forwarded

### Per-interface stubs

- PHP use alias: `use FQCN as _HandleName` with documentation comment
- Concrete implementations are wrapped by their own class stubs

### Per-enum stubs

- `fromValue(mixed $value)` constructor for backed enums
- `caseXxx()` static accessors for each enum case
- Pure enums get only case accessors

## Files Landed

- `package3/php/glue/glue.go` -- Emit() + PHP code generators
- `package3/php/glue/glue_test.go` -- 14 test functions

## Test Coverage

- Empty surface produces no files
- Namespace generation (vendor/package -> MochiGlue namespace)
- Class file creation with correct name
- Instance method forwarding with typed params
- Static method forwarding (no _inner delegation)
- Magic method skip (__construct and __get not forwarded)
- Void return (no return keyword)
- Nullable parameter (?string)
- Variadic parameter (Type ...$args)
- Interface alias stub
- Pure enum case accessors
- Backed enum fromValue + case accessors
- pascalCase and classHandle unit tests
