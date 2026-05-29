---
title: "05. Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping table"
description: "The complete closed .NET-to-Mochi translation table, refusal cases, generic monomorphization rule, string UTF-8 copy strategy, Task<T>/ValueTask<T> desugar, Nullable<T> handling, enum desugar, record structural desugar, and the discriminated-union sealed-class-hierarchy pattern."
---

# 05. Type mapping table

The bridge uses a **closed type-translation table**: only items whose types appear in the table are translated; everything else generates a `SkipReport` entry. This note defines the table in full.

## The translation table

### Primitive types

| .NET type | ECMA-335 element type | Mochi type | Notes |
|-----------|----------------------|------------|-------|
| `void` | `ELEMENT_TYPE_VOID` | `unit` | Return type only |
| `bool` | `ELEMENT_TYPE_BOOLEAN` | `bool` | 1-byte at ABI |
| `byte` / `System.Byte` | `ELEMENT_TYPE_U1` | `byte` | 0–255 |
| `sbyte` / `System.SByte` | `ELEMENT_TYPE_I1` | `int` | -128–127, widened |
| `char` / `System.Char` | `ELEMENT_TYPE_CHAR` | `int` | UTF-16 code unit as int; surrogate pairs are a SkipReport |
| `short` / `System.Int16` | `ELEMENT_TYPE_I2` | `int` | Widened |
| `ushort` / `System.UInt16` | `ELEMENT_TYPE_U2` | `int` | Widened |
| `int` / `System.Int32` | `ELEMENT_TYPE_I4` | `int` | Direct |
| `uint` / `System.UInt32` | `ELEMENT_TYPE_U4` | `long` | Widened to avoid unsigned overflow |
| `long` / `System.Int64` | `ELEMENT_TYPE_I8` | `long` | Direct |
| `ulong` / `System.UInt64` | `ELEMENT_TYPE_U8` | `long` | Truncation risk; SkipReport if > Int64.MaxValue possible |
| `float` / `System.Single` | `ELEMENT_TYPE_R4` | `float` | Direct |
| `double` / `System.Double` | `ELEMENT_TYPE_R8` | `float` | Direct (Mochi `float` is 64-bit) |
| `decimal` / `System.Decimal` | `VALUETYPE System.Decimal` | `decimal` | 128-bit opaque struct; arithmetic via wrapper symbols |
| `nint` / `System.IntPtr` | `ELEMENT_TYPE_I` | `long` | Native pointer-sized int, mapped to long |
| `nuint` / `System.UIntPtr` | `ELEMENT_TYPE_U` | `long` | Same; SkipReport if unsigned semantics matter |
| `string` / `System.String` | `ELEMENT_TYPE_STRING` | `string` | UTF-16→UTF-8 copy across boundary; see §String strategy |

### Nullable types

| .NET type | Translation | Notes |
|-----------|-------------|-------|
| `T?` (nullable value type, e.g. `int?`) | `T\|nil` | `Nullable<T>` unwrapped; `nil` means `HasValue=false` |
| `T?` (nullable reference type, e.g. `string?`) | `T\|nil` | Detected via `NullableAttribute`; default is non-null |
| `Nullable<T>` | `T\|nil` | Same as `T?` value type |

If a reference-type parameter lacks `NullableAttribute`, the bridge assumes non-null and generates a `SkipReport: no nullable annotation, assuming non-null`.

### Collection types

| .NET type | Mochi type | Strategy |
|-----------|------------|---------|
| `T[]` (single-dimension zero-based) | `list<T>` (when T in-table) | Marshalled as (ptr, len) pair; copy on entry and exit |
| `List<T>` | `list<T>` (when T in-table) | Copy via `ToArray()` at the ABI boundary |
| `IList<T>` | `list<T>` | Materialised via `.ToList().ToArray()` |
| `IEnumerable<T>` | `list<T>` | Materialised via `.ToArray()`; lazy sequences become eager |
| `IReadOnlyList<T>` | `list<T>` | Same as `IList<T>` |
| `IReadOnlyCollection<T>` | `list<T>` | Materialised |
| `Dictionary<K, V>` | `map<K, V>` (when K is `string` or integer, V is in-table) | Copy via KeyValuePair enumeration |
| `IDictionary<K, V>` | `map<K, V>` | Same |
| `SortedDictionary<K, V>` | `omap<K, V>` | Preserves insertion/sort order |
| `HashSet<T>` | `set<T>` | Copy via `ToArray()` |
| `SortedSet<T>` | `oset<T>` | Ordered set |
| `ReadOnlyDictionary<K, V>` | `map<K, V>` | Same as Dictionary |
| `KeyValuePair<K, V>` | `tuple<K, V>` | Blittable struct when both K, V are value types |

Collections with reference-type element T that is itself in the table (e.g., `List<MyRecord>`) are translated if `MyRecord` is in the table. Collections with element types outside the table generate `SkipReport`.

`IAsyncEnumerable<T>` is explicitly excluded from v1. `Span<T>`, `ReadOnlySpan<T>`, `Memory<T>`, `ReadOnlyMemory<T>` are excluded (stack-allocated / unsafe-pinned; no GC handle strategy applies).

### Task and async types

| .NET type | Mochi type | Strategy |
|-----------|------------|---------|
| `Task` | `async unit` | Awaited via ManualResetEventSlim + ThreadPool (see [[08-async-bridge]]) |
| `Task<T>` | `async T` (when T in-table) | Same |
| `ValueTask` | `async unit` | Same as Task; converted via `.AsTask()` |
| `ValueTask<T>` | `async T` | Same |

### Enum types

.NET enums are integer-backed nominal types. The bridge translates:

```csharp
public enum Direction { North = 0, South = 1, East = 2, West = 3 }
```

to:

```mochi
extern type Direction = North | South | East | West
```

backed by an `int` at the ABI. The underlying type (`byte`, `short`, `int`, `long`) is preserved as the C ABI integer width.

Bit-flag enums (`[Flags]` attribute) are translated to `int` (not a Mochi enum) with a `SkipReport: [Flags] enum translated to int; use bitwise operations`. This is a deliberate simplification for v1.

### Struct types (value types)

A `struct` is translated when all its public fields and properties are in-table and it is `[StructLayout(LayoutKind.Sequential)]` or `[StructLayout(LayoutKind.Explicit)]` or has a default sequential layout with all blittable fields.

```csharp
public struct Point { public float X; public float Y; }
```

→

```mochi
extern record Point { X: float, Y: float }
```

Passed by value at the ABI (no GC handle required). Non-blittable structs (structs containing reference-type fields) use the GC handle strategy (see [[10-gc-and-memory]]).

### Class and record types (reference types)

Concrete non-generic classes are translated as opaque GC handle types:

```csharp
public sealed class NpgsqlConnection { ... }
```

→

```mochi
extern type NpgsqlConnection   // opaque GCHandle-backed handle
```

Methods on the class are emitted as `extern fn` declarations taking the handle as the first argument (receiver):

```mochi
extern fn NpgsqlConnection_Open(conn: NpgsqlConnection): unit
extern fn NpgsqlConnection_Close(conn: NpgsqlConnection): unit
extern fn NpgsqlConnection_free(conn: NpgsqlConnection): unit
```

C# 9+ `record` types are translated analogously. Mutable `record class` types follow the class strategy; `record struct` types follow the blittable struct strategy when all fields are in-table.

### Interface types

An interface is translated only when the use site provides a concrete implementing type (the Mochi code passes a `NpgsqlConnection` where `IDbConnection` is expected). The bridge emits:

```mochi
extern type IDbConnection  // opaque handle; implemented by NpgsqlConnection, SqlConnection, etc.
```

Methods on the interface are emitted with the interface type as the receiver. At the call site, Mochi can pass any `extern type` that the ECMA-335 metadata records as implementing the interface.

### Sealed class hierarchy (discriminated union pattern)

The sealed-class-hierarchy pattern (common for AST nodes, discriminated union emulation) is:

```csharp
public abstract class Shape { }
public sealed class Circle : Shape { public float Radius; }
public sealed class Rectangle : Shape { public float Width, Height; }
```

When a type is abstract and all its immediate subtypes in the assembly are sealed, the bridge translates it to a Mochi ADT:

```mochi
extern type Shape = Circle(radius: float) | Rectangle(width: float, height: float)
```

This translation applies only when the closed set of subtypes is fully known from the assembly (all subtypes in the same assembly, all sealed). If external subtypes are possible (the base class is `public abstract` and not `sealed`), the bridge falls back to the opaque handle strategy and emits a `SkipReport: open hierarchy; cannot emit exhaustive ADT`.

### Delegate types

A `delegate` type is translated as a Mochi `fn` type when its signature is fully in-table:

```csharp
public delegate bool Predicate<T>(T item);  // with T monomorphised to string
```

→

```mochi
extern type Predicate_string = fn(item: string): bool
```

Multi-cast delegates (where the .NET runtime chains multiple subscribers) are excluded: `SkipReport: multi-cast delegate not representable in Mochi fn type`.

## Refusal cases (SkipReport)

The following generate a `SkipReport` entry and are excluded from the synthesised shim:

| Case | Reason |
|------|--------|
| `void*`, `T*` (pointer types) | Unsafe; no safe Mochi equivalent |
| `ref T`, `out T`, `in T` (by-ref reference types) | By-ref semantics require aliasing, not present in Mochi |
| `Span<T>`, `ReadOnlySpan<T>`, `Memory<T>` | Stack-allocated or pinned; not transferable across GC boundary |
| `dynamic` | Untyped; cannot generate typed Mochi extern fn |
| `object` (return type) | Untyped; cannot generate typed Mochi extern fn; if parameter type, treated as `any` with a warning |
| `IAsyncEnumerable<T>` | Async stream; deferred to phase 11 |
| Unbound generic type parameters (`T`, `U`) | Cannot emit without monomorphisation; add to `[dotnet.monomorphise]` |
| `[Flags]` enum | Translated to `int`, not `extern type`; see enum section |
| `unsafe` method (`MethodImplAttributes.Unmanaged`) | Unsafe; require `[dotnet.capabilities] unsafe = true` plus hand-authored `extern fn` |
| Open abstract class hierarchy | Cannot emit exhaustive ADT; use opaque handle instead |
| `event` declaration | Event subscription requires a delegate callback; deferred to phase 11 (callback bridge) |
| COM interop types | Windows-only; not NativeAOT-compatible |
| `ref struct` types | Stack-only; not transferable |
| `Cow<T>` analogues (`StringSegment`, `ArraySegment`) | Copy-or-slice semantics not representable |

## Generic monomorphization

Generic items (methods or types with unresolved type parameters at the assembly level) are excluded by default. The `[dotnet.monomorphise]` table in `mochi.toml` lets the user request specific instantiations:

```toml
[dotnet]
monomorphise = [
    { item = "Newtonsoft.Json.JsonConvert.DeserializeObject", T = "MyRecord" },
    { item = "System.Collections.Generic.List", T = "string" },
    { item = "System.Linq.Enumerable.Where", T = "MyRecord" },
]
```

For each entry, the bridge instantiates the generic with the specified Mochi type (which must be in the type table), emits the NativeAOT wrapper for that instantiation, and emits a typed `extern fn` for the Mochi shim.

The monomorphise list is explicit-only. The bridge does not infer which instantiations to emit from the user's source code (unlike Rust's generic monomorphization via `cargo build`); inference would require a full type-checking pass before the wrapper is synthesised. Users add monomorphise entries when they see a `SkipReport: unresolved generic type parameter; add to [dotnet.monomorphise]` for a method they need.

## String strategy

.NET strings are UTF-16 (two-byte per code unit). Mochi strings are UTF-8. The bridge converts at every crossing:

- **Mochi→.NET**: the wrapper receives a `byte*` (UTF-8, null-terminated), calls `System.Text.Encoding.UTF8.GetString(ptr, len)` to produce a .NET `string`, passes the managed string to the target method.
- **.NET→Mochi**: the wrapper receives the return `string`, calls `System.Text.Encoding.UTF8.GetBytes(s)`, allocates a `byte*` via `Marshal.AllocHGlobal(bytes.Length + 1)`, copies the bytes, null-terminates, returns the `byte*`. The Mochi side receives the pointer and calls the synthesised `mochi_dotnet_string_free` symbol to free it after use.

This copy strategy is correct and allocation-safe. It accepts the UTF-16↔UTF-8 transcode cost (~2ns per character, negligible for most strings). The alternative of passing a raw `char*` (UTF-16) would require Mochi to handle UTF-16 strings, which it does not support.

## decimal type

`System.Decimal` is a 128-bit fixed-point type stored as three `int` fields (`lo`, `mid`, `hi`) plus a sign/scale field. It has no native equivalent in Go, Rust, or most C FFI layers. The bridge maps it to a blittable four-int struct:

```csharp
[StructLayout(LayoutKind.Sequential)]
public struct MochiDecimal {
    public int Lo; public int Mid; public int Hi; public int Flags;
}
```

with synthesised arithmetic symbols:

```
mochi_dotnet_decimal_add(a: decimal, b: decimal): decimal
mochi_dotnet_decimal_sub(a: decimal, b: decimal): decimal
mochi_dotnet_decimal_mul(a: decimal, b: decimal): decimal
mochi_dotnet_decimal_div(a: decimal, b: decimal): decimal
mochi_dotnet_decimal_from_long(v: long): decimal
mochi_dotnet_decimal_to_string(v: decimal): string
```

Mochi programs that need decimal arithmetic call these symbols explicitly. This is verbose but correct; a Mochi `decimal` literal syntax (`1.50d`) is a future MEP.
