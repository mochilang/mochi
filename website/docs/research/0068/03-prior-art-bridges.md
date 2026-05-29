---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "pythonnet, CppSharp, IKVM.NET, grpc-dotnet, NativeAOT interop experiments, swift-bridge analogues, uniffi for .NET, ClangSharp, and the Unity IL2CPP pipeline. What each gets right, what each requires the user to write, and what MEP-68 borrows."
---

# 03. Prior-art bridges

MEP-68 sits in a large landscape of existing .NET interop tools. This note surveys the most relevant prior art, documents what each tool provides, what it requires from the user, and where MEP-68 borrows or deliberately diverges.

## pythonnet (Python.NET)

**What it is.** A bidirectional Python / .NET interop runtime. Python scripts call `import clr; clr.AddReference("MyLib"); from MyLib import MyClass`. The CLR is hosted in the Python process via `coreclr_initialize`. Object references are proxied through Python wrapper objects backed by GCHandles.

**What it requires.** Zero boilerplate on the .NET side (all public types are available). Python must have the `pythonnet` package installed; .NET 8+ must be installed on the machine.

**What MEP-68 borrows.** The GCHandle proxy strategy: every .NET reference type returned across the boundary is tracked by a `GCHandle`, and the foreign-language side holds an opaque handle rather than a raw pointer. pythonnet's design proves this is the right strategy for object identity and lifetime across a GC boundary.

**Why MEP-68 diverges.** pythonnet uses CoreCLR hosting (JIT mode); MEP-68 uses NativeAOT. pythonnet requires the .NET runtime on the target machine; MEP-68's NativeAOT primary path requires nothing at runtime. pythonnet's proxy objects are dynamically typed (Python does not validate the method signature at import time); MEP-68's closed type table performs static validation at `mochi pkg lock` time and emits typed `extern fn` declarations.

## CppSharp

**What it is.** A binding generator that reads a C/C++ header via Clang and emits C# P/Invoke bindings. Used by the .NET community to call native C++ libraries from C#.

**What it requires.** A C/C++ header. The `CppSharp.Generator` NuGet tool is invoked with the header path and output directory.

**What MEP-68 borrows.** The idea of reading a source-level type description (header for C++; ECMA-335 for .NET) and generating typed bindings automatically. CppSharp is the "C++ → C#" direction; MEP-68 is the ".NET → Mochi" direction. Both avoid hand-written interop boilerplate.

**Why MEP-68 diverges.** CppSharp is single-direction (C++ to C#). MEP-68 is bidirectional. CppSharp reads C++ headers (text); MEP-68 reads ECMA-335 binary metadata. CppSharp does not synthesise NativeAOT wrappers; MEP-68 does.

## IKVM.NET

**What it is.** A .NET implementation of the Java Virtual Machine (JVM). IKVM compiles Java `.class` files to .NET assemblies (static compilation) or runs Java bytecode under the .NET CLR (dynamic mode).

**What it requires.** Java source or bytecode. IKVM is a .NET-side tool for consuming Java from .NET; it is not directly applicable to Mochi.

**What MEP-68 borrows.** IKVM's "compile to .NET assembly" approach demonstrates that ahead-of-time cross-language compilation (not hosting a foreign runtime) is a viable strategy for language bridge design. NativeAOT is MEP-68's equivalent: compile .NET ahead-of-time to native machine code, then link it into the Mochi binary without a hosted runtime.

## grpc-dotnet (gRPC for .NET)

**What it is.** The official .NET gRPC client and server libraries. Applications generate C# stubs from `.proto` files and communicate over HTTP/2.

**What it requires.** A `.proto` file describing the service contract. The `dotnet-grpc` tool generates C# client and server stubs.

**Why MEP-68 rejects it.** gRPC runs a separate process per service and communicates over a socket. Per-call overhead is ~100µs minimum (socket write + serialize + HTTP/2 + deserialize + socket read). For tight loops calling into a NuGet library, this is 100-1000x the overhead of a direct function call via NativeAOT. gRPC is the right tool for distributed service communication; it is not the right tool for in-process library calls.

**What MEP-68 borrows.** The `.proto` → strongly-typed stub generation pattern is analogous to ECMA-335 → Mochi extern fn generation. Both start from a structured type description and generate language-native bindings automatically.

## NativeAOT interop experiments (dotnet/runtime)

**What they are.** The dotnet/runtime repository has several experimental NativeAOT interop guides: `NativeLibrary.SetDllImportResolver`, `NativeAOT/samples/NativeLibrary`, and `NativeAOT/samples/HelloWorld`. These demonstrate `[UnmanagedCallersOnly]` entry points and `NativeLib=Static` builds.

**What they require.** A C# class library project with `[UnmanagedCallersOnly]` methods, `<NativeLib>Static</NativeLib>` in `.csproj`, and `dotnet publish -r <rid> /p:PublishAot=true`.

**What MEP-68 borrows.** Exactly this pattern, fully automated. The NativeAOT interop sample is the "hello world" of the wrapper approach; MEP-68 is the tool that generates the sample for every NuGet package.

## swift-bridge

**What it is.** A Rust library and code generator for Swift-Rust bidirectional FFI. The user writes a `#[swift_bridge::bridge]` module declaring the types and functions shared across the boundary, and `swift-bridge` generates Swift and Rust glue code.

**What it requires.** A hand-written bridge module (analogous to `cxx::bridge` for C++). The user must enumerate every function and type they want to share.

**What MEP-68 borrows.** The architectural pattern of generating bridge glue from a declaration, not from reflection. MEP-68 differs by generating the declaration automatically from ECMA-335 metadata, so the user writes nothing.

## uniffi-rs for .NET (mozWinRT / UniFFI .NET backend)

**What it is.** Mozilla's `uniffi-rs` tool generates bindings from a `.udl` interface description file for Swift, Kotlin, Python, and (experimentally) C#/.NET. The .NET backend generates C# bindings from the `.udl`.

**What it requires.** A `.udl` file describing the interface. The user must write the `.udl` for every library they want to bridge.

**Why MEP-68 rejects it.** The `.udl` file is the same kind of boilerplate MEP-68 is designed to eliminate. uniffi is the right tool when the bridge author wants fine-grained control; MEP-68 derives the interface automatically.

**What MEP-68 borrows.** The `SkipReport` concept: uniffi generates a "not representable" error for types it cannot express in its UDL. MEP-68 generates a `SkipReport` entry for types outside the closed table. Both communicate clearly what the bridge covers and what it skips.

## ClangSharp

**What it is.** Microsoft's `ClangSharp.PInvokeGenerator` tool, which reads C/C++ headers via libClang and generates C# P/Invoke declarations. Used to generate the Win32Metadata NuGet package (the Windows API as a NuGet package).

**What it requires.** A C header. The generator is invoked with the header, a config file specifying namespaces and excluded items, and an output directory.

**What MEP-68 borrows.** The generator-plus-config model: ClangSharp takes a "here are all the types you can skip" config; MEP-68 has a `SkipReport` for types outside the closed table. Both accept that not every item in the source surface can be represented in the target language, and communicate the gap explicitly.

## Unity IL2CPP

**What it is.** Unity's ahead-of-time C# compiler that converts managed CIL bytecode to C++ source code, which is then compiled to native machine code by the platform's C++ compiler. IL2CPP is Unity's production path for iOS, WebGL, and console builds.

**What it requires.** Nothing from the developer; IL2CPP is Unity's build-time transform. The developer writes C# against the Unity API and IL2CPP handles the rest.

**What MEP-68 borrows.** The "compile managed code to native ahead-of-time" principle. IL2CPP via C++ is to NativeAOT via LLVM as an older slower path to a newer faster one. Both prove that the full .NET BCL + user code + NuGet packages can be compiled to native without a JIT runtime. NativeAOT (shipping as part of the official .NET SDK since .NET 8) is the modern replacement for the IL2CPP approach.

**Why MEP-68 uses NativeAOT instead.** NativeAOT is an official Microsoft product with LTS support, ships in the .NET SDK without third-party tooling, and produces native binaries that link directly as static archives. IL2CPP is Unity-specific and requires the Unity Editor to orchestrate.

## Summary: what MEP-68 synthesises

| Prior art | Key idea borrowed | Key difference |
|-----------|-------------------|----------------|
| pythonnet | GCHandle proxy for reference types | MEP-68 uses NativeAOT, not CLR hosting; types are statically verified |
| CppSharp | Auto-generate bindings from structured type description | MEP-68 reads ECMA-335 binary, not C++ headers; direction is reversed |
| NativeAOT samples | `[UnmanagedCallersOnly]` + `NativeLib=Static` | MEP-68 generates the wrapper automatically |
| uniffi-rs | SkipReport for out-of-table items | MEP-68 derives the interface from metadata, not a UDL |
| ClangSharp | Generator-plus-config, explicit skip list | MEP-68 uses a closed type table instead of an exclude list |
| Unity IL2CPP | AOT compile of managed code to native | MEP-68 uses official .NET NativeAOT, not IL2CPP |
| grpc-dotnet | Auto-generate typed stubs from a schema | MEP-68 generates directly to Mochi extern fn, not to a protobuf schema |
