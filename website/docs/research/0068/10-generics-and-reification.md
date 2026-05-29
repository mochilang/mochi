---
title: "10. Generics and reification"
sidebar_position: 11
sidebar_label: "10. Generics"
description: "How .NET reified generics (List<int> and List<string> as distinct CLR types) differ from Java's type-erased generics and Rust's monomorphised generics; the [dotnet.monomorphise] explicit instantiation model; combinatorial explosion risks; and the shim generation strategy for closed generic types."
---

# 10. Generics and reification

This note documents the .NET generic type system and how it drives MEP-68's explicit monomorphisation model.

## .NET generics are reified

Unlike Java (where `List<Integer>` and `List<String>` are the same `List` class at runtime, differing only in compile-time type-checking), .NET generics are fully reified: `List<int>` and `List<string>` are distinct CLR types at runtime, with distinct method tables, distinct JIT-compiled code, and distinct memory layouts.

This reification has concrete consequences:

```csharp
var intList = new List<int>();
var stringList = new List<string>();

// These are distinct types at runtime:
Console.WriteLine(intList.GetType());    // System.Collections.Generic.List`1[System.Int32]
Console.WriteLine(stringList.GetType()); // System.Collections.Generic.List`1[System.String]
Console.WriteLine(intList.GetType() == stringList.GetType()); // False
```

The CLR JIT compiles a separate specialised version of `List<T>` for each value-type instantiation (`List<int>`, `List<double>`, `List<DateTime>`) and shares a single reference-type instantiation for all reference-type instantiations (`List<string>`, `List<object>`, `List<HttpClient>` all use the same JIT-compiled code).

This behaviour is similar to Rust's monomorphisation (where `Vec<i64>` and `Vec<String>` are distinct types), but differs from Rust in that CLR generics are resolved at JIT time rather than at compile time.

## Open and closed generic types

An **open generic type** has one or more unbound type parameters: `List<T>` (where `T` is unbound). Open generic types cannot be instantiated directly.

A **closed generic type** has all type parameters bound to concrete types: `List<int>` (where `T = int`). Closed generic types can be instantiated.

The `mochi-dotnet-meta` tool emits open generic type signatures using the CLR's backtick notation: `System.Collections.Generic.List\`1` (the backtick-1 indicating one type parameter), `System.Collections.Generic.Dictionary\`2` (two type parameters).

For a method on an open generic type, the tool emits the open signature:

```json
{
  "name": "Add",
  "declaring-type": "System.Collections.Generic.List`1",
  "params": [{ "name": "item", "type": { "kind": "GenericParam", "name": "T", "index": 0 } }],
  "return-type": { "kind": "Void" }
}
```

The type-mapping pass encounters the `GenericParam` kind and emits a `SkipUnconcretisedGeneric` report.

## The `[dotnet.monomorphise]` model

The user enables import of a specific generic instantiation by declaring it in `mochi.toml`:

```toml
[dotnet]
monomorphise = [
    { item = "System.Collections.Generic.List`1.Add", T = "string" },
    { item = "System.Collections.Generic.List`1.Add", T = "int" },
    { item = "Newtonsoft.Json.JsonConvert.DeserializeObject`1", T = "MyApp.UserRecord" },
]
```

For each declaration, the bridge:

1. Constructs the closed CLR type name: `System.Collections.Generic.List\`1[System.String]` for `T = "string"`.
2. Generates a C# shim method that wraps the closed instantiation:
   ```csharp
   [UnmanagedCallersOnly(EntryPoint = "mochi_List_string_Add")]
   public static unsafe void List_string_Add(IntPtr list_handle, IntPtr item_ptr, int item_len)
   {
       var list = (List<string>)GCHandle.FromIntPtr(list_handle).Target!;
       var item = MochiMarshal.FromNativeString(item_ptr, item_len);
       list.Add(item);
   }
   ```
3. Emits the Mochi extern declaration:
   ```mochi
   extern fn list_string_add(list: any, item: string) from dotnet "System.Collections.Generic.List`1[string].Add"
   ```

The naming convention for the Mochi extern is `<type-snake>_<T-snake>_<method-snake>` where type-snake, T-snake, and method-snake are the lowercase, dot-to-underscore-converted forms of the respective names.

## Type name resolution for monomorphise entries

The `T = "..."` value in a `[dotnet.monomorphise]` entry is resolved against the packages in `[dotnet-dependencies]` plus the BCL (Base Class Library). Fully qualified CLR names (`T = "System.Int32"`) are also accepted:

```toml
monomorphise = [
    { item = "Newtonsoft.Json.JsonConvert.DeserializeObject`1", T = "System.Int64" },
    # Equivalent shorthand:
    { item = "Newtonsoft.Json.JsonConvert.DeserializeObject`1", T = "long" },
]
```

The bridge resolves common C# keyword aliases:

| C# alias | CLR type |
|----------|----------|
| `int` | `System.Int32` |
| `long` | `System.Int64` |
| `double` | `System.Double` |
| `float` | `System.Single` |
| `string` | `System.String` |
| `bool` | `System.Boolean` |
| `object` | `System.Object` |

User-defined types must be fully qualified with the namespace (`T = "MyApp.UserRecord"`) unless the namespace is unambiguous across all packages in `[dotnet-dependencies]`.

## Multiple type parameters

For types and methods with multiple generic parameters, all parameters must be specified:

```toml
monomorphise = [
    { item = "System.Collections.Generic.Dictionary`2", K = "string", V = "int" },
    { item = "System.Linq.Enumerable.ToDictionary`3", TSource = "MyRecord", TKey = "string", TElement = "int" },
]
```

The parameter name keys (`K`, `V`, `TSource`, `TKey`, `TElement`) must match the generic parameter names from the `mochi-dotnet-meta` JSON output. A parameter name mismatch is a lock error:

```
ERROR: monomorphise entry mismatch
  Item: System.Collections.Generic.Dictionary`2
  Expected keys: K, V
  Provided keys: T, V
  Resolution: rename T to K in [dotnet.monomorphise]
```

## Combinatorial explosion risk

A user who declares many monomorphisations generates many shim methods. The practical limits:

- The bridge warns when a single item has more than 10 monomorphisations.
- The bridge errors when the total `[dotnet.monomorphise]` count exceeds 200 (configurable via `[dotnet.limits] max-monomorphise = 200`).
- Each monomorphisation adds approximately 10-30 lines of C# to the shim and 1-2 lines of Mochi extern declaration.

For typical usage (deserialising 2-5 record types, adding to 2-3 list types), the monomorphisation table has fewer than 20 entries.

## Generic types in the 20-package fixture corpus

From the April 2026 analysis of the 20-package corpus:

| Package | Generic methods encountered | Monomorphisations needed for basic use |
|---------|----------------------------|---------------------------------------|
| Newtonsoft.Json | 8 (JsonConvert generic overloads) | 1-3 (the user's record types) |
| System.Text.Json | 6 (JsonSerializer generic overloads) | 1-3 |
| Dapper | 4 (Query<T>, QueryFirst<T>, etc.) | 1-3 |
| Entity Framework Core | 20+ (LINQ operator overloads) | 5-10 |
| FluentAssertions | 12 (generic assertion builders) | 2-5 |
| AutoMapper | 3 (Map<TSource, TDest>, etc.) | 2-4 |
| MediatR | 5 (Send<T>, Publish<T>, etc.) | 3-6 |

The Entity Framework Core count is the highest because the LINQ provider exposes many generic LINQ operators. Users doing simple EF Core queries (select all rows, insert by type) need approximately 5-10 monomorphisations.

## Relationship to Rust's monomorphisation

MEP-73 (Rust bridge) uses the same explicit `[rust.monomorphise]` model for Rust's compile-time-monomorphised generics. The analogy holds:

| .NET | Rust |
|------|------|
| CLR reified generics (JIT-time) | Rust compile-time monomorphisation |
| `List<T>` open type | `Vec<T>` generic function |
| `List<int>` closed instantiation | `Vec<i64>` concrete instantiation |
| `[dotnet.monomorphise]` table | `[rust.monomorphise]` table |
| CLR type name: `List\`1[System.Int32]` | Rust turbofish: `Vec::<i64>::new()` |

The key difference: Rust's monomorphisation is resolved at compile time and can be reasoned about statically; .NET's reification is resolved at JIT time and requires the closed CLR type name for `[UnmanagedCallersOnly]` shim generation.

## Cross-references

- [[05-type-mapping]] for the type table that triggers SkipUnconcretisedGeneric.
- [[04-assembly-metadata-ingest]] for how the `mochi-dotnet-meta` tool emits generic type information.
- [[09-abi-stability]] for the closed-instantiation shim method generation.
- [MEP-73 §05](/docs/research/0073/05-type-mapping) for the analogous Rust monomorphisation model.
- [MEP-68 §2](/docs/mep/mep-0068#2-manifest-extension-dotnet-dependencies-and-dotnet) for the normative `[dotnet.monomorphise]` manifest spec.
