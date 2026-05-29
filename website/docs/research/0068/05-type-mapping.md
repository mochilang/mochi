---
title: "05. Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed CLR-to-Mochi type translation table, the refusal cases (pointer types, unsafe code, unconcretised generics, COM interop types, by-ref parameters), the `[dotnet.monomorphise]` explicit instantiation rule, the nullable T? and Task<T> handling, and the SkipReport format."
---

# 05. Type mapping table

This note enumerates the closed translation table the bridge uses to map CLR types to Mochi types. Items whose entire signature falls inside the table are translated; items with any out-of-table type are skipped with a `SkipReport`.

## Scalar types

| CLR type | Mochi type | Notes |
|----------|------------|-------|
| `System.Int32` (`int`) | `int` | Widened to int64 at the native boundary. |
| `System.Int64` (`long`) | `int` | Native fit. |
| `System.Int16` (`short`) | `int` | Widened to int64. |
| `System.Byte` (`byte`) | `int` | Widened to int64. |
| `System.SByte` (`sbyte`) | `int` | Widened to int64. |
| `System.UInt16` (`ushort`) | `int` | Widened to int64. |
| `System.UInt32` (`uint`) | `int` | Widened to int64; values >= 2^31 positive in Mochi. |
| `System.UInt64` (`ulong`) | `int` | Translated as int64; values >= 2^63 panic the shim. |
| `System.Single` (`float`) | `double` | Widened to float64 at the boundary. |
| `System.Double` (`double`) | `double` | Native fit. |
| `System.Boolean` (`bool`) | `bool` | Passed as `int32` (0/1) across the native boundary. |
| `System.Char` (`char`) | `string` | Translated as a 1-codepoint Mochi string (Mochi has no char type). |
| `void` | unit | Functions returning `void` emit Mochi `fun(...): unit`. |
| `System.Object` | `any` | The CLR base type. Passed as an opaque GCHandle. |

## String types

| CLR type | Mochi type | Notes |
|----------|------------|-------|
| `System.String` | `string` | Owned. Marshalled as UTF-8 `byte*` + `int` length via `MochiMarshal.StringToCoTaskMem`. |
| `System.Text.StringBuilder` | (refused) | Mutable string type; no Mochi analogue. |
| `System.ReadOnlySpan<char>` | (refused, v1) | Managed reference type; cannot cross the native boundary. |
| `System.Span<T>` | (refused) | Managed reference type. |
| `System.Memory<T>` | (refused) | Same. |

## Collection types

| CLR type | Mochi type | Notes |
|----------|------------|-------|
| `System.Collections.Generic.List<T>` where T in table | `list<T>` | Marshalled as a `MochiSlice` (pointer + length + capacity). |
| `T[]` (array) where T in table | `list<T>` | Fixed-size CLR array. Translated as a `MochiSlice` with capacity = length. |
| `System.Collections.Generic.IEnumerable<T>` where T in table | `list<T>` | Materialised into a `List<T>` before marshalling. |
| `System.Collections.Generic.Dictionary<K, V>` where K = `string` and V in table | `map<string, V>` | Marshalled as a `MochiMap` opaque handle. |
| `System.Collections.Generic.Dictionary<K, V>` where K is integer type and V in table | `map<int, V>` | Same. |
| `System.Collections.Generic.HashSet<T>` where T in table | `set<T>` | Marshalled as a `MochiSet` opaque handle. |
| `System.Collections.Generic.IList<T>` | `list<T>` (materialised) | Materialised into a `List<T>` before marshalling. |
| `System.Collections.Generic.IReadOnlyList<T>` | `list<T>` (materialised) | Same. |
| `System.Linq.IQueryable<T>` | (refused) | LINQ queryable; requires runtime query compilation. |

## Nullable, Task, and special types

| CLR type | Mochi type | Notes |
|----------|------------|-------|
| `T?` where T is a value type in table | `T\|nil` | `Nullable<T>` struct. Null becomes nil; non-null becomes the value. |
| `T?` where T is a reference type in table | `T\|nil` | Reference nullable. Null becomes nil. |
| `System.Threading.Tasks.Task<T>` where T in table | (synchronous dispatch) | See [[08-async-bridge]]. The return type becomes T in Mochi; the shim calls `.GetAwaiter().GetResult()`. |
| `System.Threading.Tasks.Task` | unit (async) | A `Task` without a result is treated as `fun(): unit`. |
| `System.Threading.Tasks.ValueTask<T>` | (same as Task<T>) | Treated identically to `Task<T>` at the shim boundary. |
| `System.Threading.CancellationToken` | (refused, v1) | No Mochi cancellation primitive in v1. |
| `System.Guid` | `string` | Marshalled as a UUID string (36-character `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` format). |
| `System.DateTime` | `int` | Marshalled as Unix timestamp (milliseconds since epoch) as int64. |
| `System.DateTimeOffset` | `int` | Same as DateTime. |
| `System.TimeSpan` | `int` | Marshalled as total milliseconds as int64. |
| `System.Uri` | `string` | Marshalled as the string form of the URI. |

## Struct and record types

A CLR `struct` with all-fields-in-table and default layout (no `[StructLayout(LayoutKind.Explicit)]`) translates to a Mochi `record`:

```csharp
public struct Point {
    public double X;
    public double Y;
}
```

becomes:

```mochi
record Point {
    X: double,
    Y: double,
}
```

A struct with any out-of-table field type is refused. A struct with `[StructLayout(LayoutKind.Explicit)]` or `unsafe` fields is refused.

A C# `record` type (the `record class` or `record struct` introduced in C# 9) is treated identically to a struct or class: if all property types are in-table, it translates; otherwise it is refused.

## Class and interface types

A `class` with no constructor parameters that has in-table public methods and no `abstract` required-override methods translates to an opaque handle:

```csharp
public class HttpClient {
    public async Task<string> GetStringAsync(string requestUri) { ... }
}
```

becomes:

```mochi
extern type HttpClient
extern fn http_client_new(): HttpClient from dotnet "System.Net.Http.HttpClient..ctor"
extern fn http_client_get_string_async(client: HttpClient, uri: string): string from dotnet "System.Net.Http.HttpClient.GetStringAsync"
```

The `HttpClient` is held as a `GCHandle.Alloc(instance, GCHandleType.Normal)` on the CLR side; the Mochi side receives the `GCHandle.ToIntPtr()` value and treats it as an opaque `nint` handle. The shim provides a `_free` method that calls `GCHandle.Free`.

An `interface` type translates as an opaque handle. The bridge generates shim methods for each interface method, dispatching via the interface method on the held object reference.

Abstract classes and abstract methods are refused: instantiation requires a concrete implementation that the bridge cannot synthesise.

## Enum types

| CLR pattern | Mochi translation | Notes |
|-------------|-------------------|-------|
| `enum E { A, B, C }` (no values) | `type E = A \| B \| C` | Compact integer encoding. |
| `enum E { A = 1, B = 2, C = 4 }` (explicit values) | `type E = A \| B \| C` | Values passed as int at the boundary. |
| `[Flags] enum E { A = 1, B = 2, C = 4 }` | `int` with named constants | Flags enums translate to int; the bridge emits named constants. |

## Generic types and `[dotnet.monomorphise]`

A generic method or class with unconcretised type parameters is refused at default ingest:

```
SKIPPED: Newtonsoft.Json.JsonConvert.DeserializeObject`1[T]
  Reason: SkipUnconcretisedGeneric
  Detail: generic parameter T has no explicit monomorphisation declared
  Override: add { item = "Newtonsoft.Json.JsonConvert.DeserializeObject", T = "MyType" } to [dotnet.monomorphise]
```

The user enables import by enumerating concretisations:

```toml
[dotnet]
monomorphise = [
    { item = "Newtonsoft.Json.JsonConvert.DeserializeObject", T = "MyType" },
    { item = "System.Collections.Generic.List`1.Add", T = "string" },
]
```

For each enumeration, the bridge generates a monomorphised shim method:

```mochi
extern fn json_convert_deserialize_my_type(json: string): MyType from dotnet "Newtonsoft.Json.JsonConvert.DeserializeObject`1[MyType]"
extern fn list_string_add(list: any, item: string) from dotnet "System.Collections.Generic.List`1[System.String].Add"
```

Unlike Rust's monomorphisation (which is a compile-time template expansion), CLR generic methods are JIT-compiled per closed instantiation at runtime. The `mochi-dotnet-meta` tool emits the open generic signature; the shim generates the closed instantiation signature using the CLR's type name format: `` Foo`1[FullTypeName] ``.

## Refusal reasons

A `SkipReport` entry records:

```go
type SkipReport struct {
    ItemPath  string      // e.g., "System.IO.StreamReader.ReadAsync"
    Reason    SkipReason
    Detail    string
}

type SkipReason int
const (
    SkipPointerType     SkipReason = iota // T* (unsafe pointer)
    SkipByRef                             // ref T or out T parameter
    SkipUnconcretisedGeneric              // unconcretised generic type parameter
    SkipSpanType                          // Span<T> or ReadOnlySpan<T>
    SkipMemoryType                        // Memory<T> or ReadOnlyMemory<T>
    SkipCancellationToken                 // System.Threading.CancellationToken
    SkipAbstractClass                     // abstract class or abstract method
    SkipDelegate                          // delegate type (no Mochi analogue)
    SkipEventInfo                         // event (add/remove handlers; no Mochi analogue)
    SkipComImport                         // [ComImport] attribute (COM interop type)
    SkipUnsafeMethod                      // method marked unsafe
    SkipInternalVisibility                // internal or private method
    SkipObsolete                          // [Obsolete(error: true)] method
    SkipMulticastDelegate                 // multicast delegate invocation list
    SkipDynamicType                       // dynamic CLR type (late-bound)
    SkipQueryable                         // IQueryable<T> requires LINQ provider
    SkipFunctionPointer                   // delegate* managed/unmanaged
)
```

The bridge emits the `SkipReport` list to `<workdir>/dotnet_shim/<pkg>/SKIPPED.txt`:

```
SKIPPED: System.IO.Stream.ReadAsync
  Reason: SkipCancellationToken
  Detail: parameter "cancellationToken" of type System.Threading.CancellationToken cannot be expressed in Mochi v1
  Override: write a custom extern fn that omits the cancellationToken parameter

SKIPPED: System.Collections.Generic.List`1.Sort
  Reason: SkipDelegate
  Detail: parameter "comparison" of type System.Comparison`1[T] is a delegate type
  Override: write a custom extern fn that uses a specific comparison function
```

## Cross-references

- [[02-design-philosophy]] §6 for why the table is closed.
- [[04-assembly-metadata-ingest]] for the parsed surface that feeds the table.
- [[09-abi-stability]] for the shim-side encoding of each translated type.
- [[10-generics-and-reification]] for the CLR generics model that requires explicit monomorphisation.
- [MEP-68 §3](/docs/mep/mep-0068#3-lockfile-extension-dotnet-package) for the type-table-related lockfile fields.
