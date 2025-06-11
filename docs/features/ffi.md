## Foreign Function Interface

Mochi can call into Go, Python and TypeScript libraries using the `import`
statement and `extern` declarations. Imported modules expose their values
through language specific FFI runtimes so Mochi code can invoke them
directly. When `as` is omitted, the last path component becomes the module
name.

```mochi
import python "math" as math

extern fun math.sqrt(x: float): float

print(math.sqrt(49.0))
```

Modules may be loaded from the host language's package system or from a
local file. Functions and variables must be declared with `extern` before
use so the compiler knows their types.

### Calling Go Code

```mochi
import go "math" as math

extern fun math.Sqrt(x: float): float

print(math.Sqrt(16.0))
```

### Calling Python Code

```mochi
import python "math" as math

extern fun math.sqrt(x: float): float

print(math.sqrt(49.0))
```

### Calling TypeScript Code

```mochi
import typescript "./runtime/ffi/deno/math.ts" as math

extern fun math.pow(x: float, y: float): float

print(math.pow(2.0, 8.0))
```

Each runtime has a small host-side library under `runtime/ffi` which
implements the calls made by Mochi programs.
